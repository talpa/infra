unit InfraPersistence;

interface
uses
  SysUtils,
  SyncObjs,
  {TStrings}Classes, Contnrs,
  {Zeos}ZDbcIntfs,
  {Infra}InfraCommon, 
  {InfraInf}InfraCommonIntf,
  InfraValueTypeIntf,
  InfraPersistenceIntf;

type
  TConfiguration = class(TBaseElement, IConfiguration)
    FProperties: TStrings;
    function GetProperties: TStrings;
    function GetPropertyItem(const pName: string): string;
    procedure SetPropertyItem(const pName: string; const Value: string);
  protected
    property Properties: TStrings read GetProperties;
    property PropertyItem[const pName: string]: string read GetPropertyItem write SetPropertyItem;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetAsInteger(const pName: string): Integer; overload;
    function GetAsDouble(const pName: string): Double; overload;
    function GetAsString(const pName: string): string; overload;

    function GetValue(const pName: string; const pDefaultValue: Integer): Integer; overload;
    function GetValue(const pName: string; const pDefaultValue: Double): Double; overload;
    function GetValue(const pName: string; const pDefaultValue: string): string; overload;

    procedure SetValue(const pName: string; const Value: Integer); overload;
    procedure SetValue(const pName: string; const Value: Double); overload;
    procedure SetValue(const pName: string; const Value: string); overload;
  end;

  /// Classe responsável por prover conexões com o SGDB
  TConnectionProvider = class(TBaseElement, IConnectionProvider)
  private
    FConfiguration: IConfiguration;
    FDriverManager: IZDriverManager; // O DriverManager que será usado pra criar as conexões
    FPool: array of IZConnection; // O pool
    FCriticalSection: TCriticalSection;
    function BuildConnectionString(pConfiguration: IConfiguration): string;
    procedure CloseConnections; // CriticalSection usado para evitar conflitos em aplicações multi-thread
  protected
    function GetFreeConnection: IZConnection; // Procura por uma conexao livre
    function CreateConnection: IZConnection; // Cria uma nova conexao
    function FindConnection(const pConnection: IZConnection): IZConnection; // Procura por uma conexao no pool
  public
    constructor Create(pDriverManager: IZDriverManager; pConfiguration: IConfiguration); reintroduce;
    destructor Destroy; override;
    function GetConnection: IZConnection; // Caso tenha conexoes disponíveis no Pool bloqueia uma e retorna-a
    procedure Close; // Fecha todas as conexões do pool
    procedure ReleaseConnection(const pConnection: IZConnection); // Devolve a conexao ao Pool
  end;

  TPersistentState = class(TBaseElement, IPersistentState)
  private
    FState: TPersistentStateKind;
    FIsPersistent: Boolean;
  protected
    function GetIsPersistent: Boolean;
    function GetState: TPersistentStateKind;
    procedure SetIsPersistent(Value: Boolean);
    procedure SetState(Value: TPersistentStateKind);
    property IsPersistent: Boolean read GetIsPersistent write SetIsPersistent;
    property State: TPersistentStateKind read GetState
      write SetState;
  end;

  TSQLCommand = class(TBaseElement, ISQLCommand)
  private
    FPersistenceEngine: IPersistenceEngine;
    FName: string;
    FClassID: TGUID;
    FListID: TGUID;
  protected
    function GetName: string;
    procedure SetName(const Value: string);
    function GetClassID: TGUID;
    procedure SetClassID(const Value: TGUID);
    function GetListID: TGUID;
    procedure SetListID(const Value: TGUID);
    function GetResult: IInfraType;
    procedure SetParam(const pParamName: string; const value: IInfraType); overload;
    procedure SetParam(const pObj: IInfraType); overload;
    procedure ClearParams;
    property Name: string read GetName write SetName;
    property ClassID: TGUID read GetClassID write SetClassID;
    property ListID: TGUID read GetListID write SetListID;
  public
    constructor Create(pPersistenceEngine: IPersistenceEngine); reintroduce;
  end;

  TSession = class(TBaseElement, ISession)
  private
    FPersistenceEngine: IPersistenceEngine;
    FListCommand: ISQLCommandList;
    procedure AddCommand(const pCommandName: string; const pObj: IInfraObject);
  protected
    function Load(const pCommandName: string; const pObj: IInfraObject = nil): ISQLCommand; overload;
    function Load(const pCommandName: string; const pClassID: TGUID): ISQLCommand; overload;
    function LoadList(const pCommandName: string): ISQLCommand; overload;
    function LoadList(const pCommandName: string; const pClassID: TGUID): ISQLCommand; overload;
    function LoadList(const pCommandName: string; const pClassID: TGUID; const pListID: TGUID): ISQLCommand; overload;
    function LoadList(const pCommandName: string; const pObj: IInfraObject; const pListID: TGUID): ISQLCommand; overload;
    function LoadList(const pCommandName: string; const pObj: IInfraObject; const pList: IInfraList = nil): ISQLCommand; overload;
    procedure Delete(const pCommandName: string; const pObj: IInfraObject);
    procedure Save(const pCommandName: string; const pObj: IInfraObject);
    function Flush: Integer;
  public
    constructor Create(const pPersistenceEngine: IPersistenceEngine); reintroduce;
  end;

  TPersistenceEngine = class(TBaseElement, IPersistenceEngine)
  private
    FConfiguration: IConfiguration;
  protected
    procedure SetConnection(const pConnection: IZConnection);
    procedure Load(const pSqlCommand: ISqlCommand; const List: IInfraList);
    function Execute(const pSqlCommand: ISqlCommand): IInfraInteger;
  public
    constructor Create(pConfiguration: IConfiguration); reintroduce;
  end;

  TInfraPersistenceService = class(TBaseElement, IInfraPersistenceService)
  private
    FConfiguration: IConfiguration;
    FPersistenceEngine: IPersistenceEngine;
    function GetPersistenceEngine: IPersistenceEngine;
  protected
    function GetConfiguration: IConfiguration;
    function OpenSession: ISession; overload;
    procedure SetConnection(const pConnection: IZConnection);
    property Configuration: IConfiguration read GetConfiguration;
  end;

implementation

uses
  InfraPersistenceConsts,
  List_SQLCommandList,
  InfraBasicList,
  InfraConsts;

{ TConfiguration }

constructor TConfiguration.Create;
begin
  inherited;
  FProperties := TStringList.Create;
end;

destructor TConfiguration.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
end;

function TConfiguration.GetAsDouble(const pName: string): Double;
begin
  Result := StrToFloat(PropertyItem[pName]);
end;

function TConfiguration.GetAsInteger(const pName: string): Integer;
begin
  Result := StrToInt(PropertyItem[pName]);
end;

function TConfiguration.GetAsString(const pName: string): string;
begin
  Result := PropertyItem[pName];
end;

function TConfiguration.GetProperties: TStrings;
begin
  Result := FProperties;
end;

function TConfiguration.GetPropertyItem(const pName: string): string;
begin
  Result := FProperties.Values[pName]
end;

function TConfiguration.GetValue(const pName: string;
  const pDefaultValue: Integer): Integer;
begin
  if FProperties.IndexOfName(pName) <> -1 then
    Result := StrToIntDef(PropertyItem[pName], pDefaultValue)
  else
    Result := pDefaultValue;
end;

function TConfiguration.GetValue(const pName: string;
  const pDefaultValue: Double): Double;
begin
  if FProperties.IndexOfName(pName) <> -1 then
    Result := StrToFloatDef(PropertyItem[pName], pDefaultValue)
  else
    Result := pDefaultValue;
end;

function TConfiguration.GetValue(const pName,
  pDefaultValue: string): string;
begin
  if FProperties.IndexOfName(pName) <> -1 then
    Result := PropertyItem[pName]
  else
    Result := pDefaultValue;
end;

procedure TConfiguration.SetPropertyItem(const pName, Value: string);
begin
  FProperties.Values[pName] := Value;
end;

procedure TConfiguration.SetValue(const pName: string;
  const Value: Integer);
begin
  PropertyItem[pName] := IntToStr(Value);
end;

procedure TConfiguration.SetValue(const pName: string;
  const Value: Double);
begin
  PropertyItem[pName] := FloatToStr(Value);
end;

procedure TConfiguration.SetValue(const pName, Value: string);
begin
  PropertyItem[pName] := Value;
end;

{ TInfraConnectionProvider }

{**
  Cria uma nova instância de TInfraConnectionProvider.
  @param MaxSize Tamanho máximo do Pool de conexões
  @param ADriverManager Um objeto do tipo IZDriverManager que criará as conexões
  @param AConfiguration Um objeto do tipo IConfiguration que contém todas as
    informações para criar uma nova conexão
}

constructor TConnectionProvider.Create(pDriverManager: IZDriverManager; pConfiguration: IConfiguration);
var
  iMax: Integer;
begin
  if not Assigned(pDriverManager) then
    raise EInfraArgumentError.Create('pDriverManager');
  if not Assigned(pConfiguration) then
    raise EInfraArgumentError.Create('pConfiguration');
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FDriverManager := pDriverManager;
  FConfiguration := pConfiguration;
  iMax := FConfiguration.GetValue(cCONFIGKEY_MAXCONNECTIONS, cGlobalMaxConnections);
  SetLength(FPool, iMax);
end;

destructor TConnectionProvider.Destroy;
begin
  CloseConnections;
  SetLength(FPool, 0);
  FCriticalSection.Free;
  inherited;
end;

procedure TConnectionProvider.CloseConnections;
var
  i: Integer;
begin
  for i := Low(FPool) to High(FPool) do
    if Assigned(FPool[i]) then
      FPool[i].Close;
end;

procedure TConnectionProvider.Close;
var
  i: Integer;
begin
  for i := Low(FPool) to High(FPool) do
    if Assigned(FPool[i]) then
      ReleaseConnection(FPool[i]);
end;

{**
  Localiza um objeto no Pool. Se este não for encontrado retorna nil
  @param pConnection Objeto a ser localizado
  @return Retorna o objeto encontrado ou nil caso não seja localizado
}

function TConnectionProvider.FindConnection(const pConnection: IZConnection): IZConnection;
var
  i: Integer;
begin
  Result := nil;
  for i := Low(FPool) to High(FPool) do
    if FPool[i] = pConnection then
    begin
      Result := FPool[i];
      Break;
    end;
end;

{**
  Libera uma conexão de volta ao pool para ser reutilizada
  @param pConnection Conexão a ser liberada
}

procedure TConnectionProvider.ReleaseConnection(const pConnection: IZConnection);
begin
  if FindConnection(pConnection) = nil then
    raise EInfraConnectionProviderError.Create(cErrorConnectionNotFoundOnPool);

  if pConnection.IsClosed then
    raise EInfraConnectionProviderError.Create(cErrorAlreadyClosedConnection);

  // Ao fechar a conexao, ela, automaticamente, fica disponível no pool
  pConnection.Close;

  // TODO: Criar Thread para verificar o tempo de expiração do objeto
  // ...
end;

{**
  Procura no Pool por uma conexão disponível (ou seja, uma conexao fechada).
  E, caso a encontre, retorna-a.
  @return Retorna um objeto do tipo IZConnection
}

function TConnectionProvider.GetFreeConnection: IZConnection;
var
  i: Integer;
begin
  Result := nil;
  for i := Low(FPool) to High(FPool) do
    if Assigned(FPool[i]) and FPool[i].IsClosed then
    begin
      Result := FPool[i];
      Break;
    end;
end;

function TConnectionProvider.BuildConnectionString(pConfiguration: IConfiguration): string;
begin
  Result := 'zdbc:' + pConfiguration.GetAsString(cCONFIGKEY_DRIVER) +
    '://' + pConfiguration.GetAsString(cCONFIGKEY_HOSTNAME) +
    '/' + pConfiguration.GetAsString(cCONFIGKEY_DATABASENAME) +
    '?username=' + pConfiguration.GetAsString(cCONFIGKEY_USERNAME) +
    ';password=' + pConfiguration.GetAsString(cCONFIGKEY_DATABASENAME);
end;

{**
  Cria uma nova conexao, caso haja algum slot vazio.
  Caso contrário, levanta uma exceção EInfraConnectionProviderError
  @return Retorna um objeto do tipo IZConnection
}

function TConnectionProvider.CreateConnection: IZConnection;
var
  i: Integer;
begin
  for i := Low(FPool) to High(FPool) do
    if not Assigned(FPool[i]) then
    begin
      FPool[i] := FDriverManager.GetConnection(BuildConnectionString(FConfiguration));
      Result := FPool[i];
      Exit;
    end;

  raise EInfraConnectionProviderError.Create(cErrorConnectionsLimitExceeded);
end;

{**
  Procura no Pool por uma conexão disponível e, caso a encontre, retorna-a.
  Caso contrário, tenta criar uma nova conexão. Se isto não for possível,
  levanta uma exceção EInfraConnectionProviderError
  @return Retorna um objeto do tipo IZConnection
}

function TConnectionProvider.GetConnection: IZConnection;
begin
  FCriticalSection.Acquire;
  try
    Result := GetFreeConnection;
    if not Assigned(Result) then
      Result := CreateConnection;
  finally
    FCriticalSection.Release;
  end;
end;

{ TPersistentState }

function TPersistentState.GetIsPersistent: Boolean;
begin
  Result := FIsPersistent;
end;

function TPersistentState.GetState: TPersistentStateKind;
begin
  Result := FState;
end;

procedure TPersistentState.SetIsPersistent(Value: Boolean);
begin
  FIsPersistent := Value;
end;

procedure TPersistentState.SetState(Value: TPersistentStateKind);
begin
  FState := Value;
end;

{ TSQLCommand }

constructor TSQLCommand.Create(pPersistenceEngine: IPersistenceEngine);
begin
  inherited Create;
  FPersistenceEngine := pPersistenceEngine;
end;

procedure TSQLCommand.ClearParams;
begin
  // TODO:
  // FParams.Clear?
end;

function TSQLCommand.GetClassID: TGUID;
begin
  // TODO:
  Result := FClassID;
end;

function TSQLCommand.GetListID: TGUID;
begin
  // TODO:
  Result := FListID;
end;

function TSQLCommand.GetName: string;
begin
  // TODO:
  Result := FName;
end;

function TSQLCommand.GetResult: IInfraType;
begin
  // TODO:
  // Result := FPersistenceEngine.???
end;

procedure TSQLCommand.SetClassID(const Value: TGUID);
begin
  // TODO:
  FClassID := Value;
end;

procedure TSQLCommand.SetListID(const Value: TGUID);
begin
  // TODO:
  FListID := Value;
end;

procedure TSQLCommand.SetName(const Value: string);
begin
  // TODO:
  FName := Value;
end;

procedure TSQLCommand.SetParam(const pParamName: string;
  const value: IInfraType);
begin
  // TODO:
  //FParams[pParamName] := Value?
end;

procedure TSQLCommand.SetParam(const pObj: IInfraType);
begin

end;

{ TSession }

constructor TSession.Create(const pPersistenceEngine: IPersistenceEngine);
begin
  if not Assigned(pPersistenceEngine) then
    raise EInfraArgumentError.Create('pPersistenceEngine');
  inherited Create;
  FPersistenceEngine := pPersistenceEngine;
  FListCommand := TSQLCommandList.Create;
end;

procedure TSession.Delete(const pCommandName: string; const pObj: IInfraObject);
begin
  AddCommand(pCommandName, pObj);
end;

function TSession.Flush: Integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FListCommand.Count - 1 do
  begin
//    Result := result + (FPersistenceEngine.Execute(FListCommand.Items[i])).AsInteger;
  end;

end;

procedure TSession.AddCommand(const pCommandName: string; const pObj: IInfraObject);
var
  ASqlCommand: ISQLCommand;
begin
  ASqlCommand := TSQLCommand.Create(FPersistenceEngine);
  ASqlCommand.Name := pCommandName;
  ASqlCommand.SetParam(pObj);
//  FListCommand.Add(ASqlCommand);
end;

function TSession.Load(const pCommandName: string; const pClassID: TGUID): ISQLCommand;
begin
  Result := TSQLCommand.Create(FPersistenceEngine);
  result.Name := pCommandName;
  Result.ClassID := pClassID
end;

function TSession.Load(const pCommandName: string; const pObj: IInfraObject): ISQLCommand;
begin
  Result := TSQLCommand.Create(FPersistenceEngine);
  result.Name := pCommandName;
  Result.SetParam(pObj);
end;

function TSession.LoadList(const pCommandName: string): ISQLCommand;
begin
  Result := TSQLCommand.Create(FPersistenceEngine);
  result.Name := pCommandName;
end;

function TSession.LoadList(const pCommandName: string; const pClassID: TGUID): ISQLCommand;
begin
  Result :=LoadList(pCommandName);
  Result.ClassID := pClassID;
end;

function TSession.LoadList(const pCommandName: string; const pClassID, pListID: TGUID): ISQLCommand;
begin
  Result :=LoadList(pCommandName,pClassID);
  Result.ListID := pListID;
end;

function TSession.LoadList(const pCommandName: string; const pObj: IInfraObject; const pListID: TGUID): ISQLCommand;
begin
  Result := TSQLCommand.Create(FPersistenceEngine);
  result.setParam(pObj);
  Result.ClassID := pObj.TypeInfo.TypeID
end;

function TSession.LoadList(const pCommandName: string; const pObj: IInfraObject; const pList: IInfraList): ISQLCommand;
begin
  //....
end;

procedure TSession.Save(const pCommandName: string; const pObj: IInfraObject);
begin
  AddCommand(pCommandName, pObj);
end;

{ TPersistenceEngine }

constructor TPersistenceEngine.Create(pConfiguration: IConfiguration);
begin
  if not Assigned(pConfiguration) then
    raise EInfraArgumentError.Create('pConfiguration');
  inherited Create;
  FConfiguration := pConfiguration;
end;

function TPersistenceEngine.Execute(
  const pSqlCommand: ISqlCommand): IInfraInteger;
begin
  // carregar a sql usando um reader com base no Name do pSqlCommand
  // preencher os params da sql com base nos Params do pSqlCommand
  // pegar o connection no connectionprovider
  // executa a sql e retornar  a quantidade de registros afetados
end;

procedure TPersistenceEngine.Load(const pSqlCommand: ISqlCommand;
  const List: IInfraList);
begin
  // carregar a sql usando um reader com base no Name do pSqlCommand
  // preencher os params da sql com base nos Params do pSqlCommand
  // executa a sql e pega um IZStatement
  // cria uma lista com base em ClassList do pSqlCommand, ou usa a lista passada como parâmetro
  // Faz um laço para pegar cada registro
  //    cria um objeto com base no ClassType do pSqlCommand,
  //    Seta o estado persistent e clean ao objeto criado
  //    faz a carga dos atributos com base no registro
  //    Adiciona o novo objeto a lista
  // retorna a lista
end;

procedure TPersistenceEngine.SetConnection(
  const pConnection: IZConnection);
begin
  // preencher o connection provider com o pConnection
end;

{ TInfraPersistenceService }

function TInfraPersistenceService.GetConfiguration: IConfiguration;
begin
  if not Assigned(FConfiguration) then
    FConfiguration := TConfiguration.Create;
  Result := FConfiguration;
end;

function TInfraPersistenceService.GetPersistenceEngine: IPersistenceEngine;
begin
  if not Assigned(FPersistenceEngine) then
    FPersistenceEngine := TPersistenceEngine.Create(FConfiguration);
  Result := FPersistenceEngine;
end;

function TInfraPersistenceService.OpenSession: ISession;
begin
  Result := TSession.Create(GetPersistenceEngine);
end;

procedure TInfraPersistenceService.SetConnection(
  const pConnection: IZConnection);
begin
  GetPersistenceEngine.SetConnection(pConnection);
end;

// Não entendi mas se por direto no Initialization acontece
// Access Violations.

// ATENÇÃO: Vc não deve atribuir PersistenceService para uma variável de
// instancia nem global sob pena de acontecer um AV no final da aplicação
procedure InjectPersistenceService;
begin
  (ApplicationContext as IBaseElement).Inject(
    IInfraPersistenceService, TInfraPersistenceService.Create);
end;

initialization
  InjectPersistenceService;

end.
