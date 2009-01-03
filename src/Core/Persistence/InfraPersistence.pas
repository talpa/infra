// xxx
unit InfraPersistence;

interface

uses
  SysUtils,
  SyncObjs,
  Classes,
  Contnrs,
  {Zeos}
  ZDbcIntfs,
  {Infra}
  InfraCommon,
  InfraCommonIntf,
  InfraValueTypeIntf,
  InfraPersistenceIntf,
  InfraPersistenceAnnotationIntf;
  
type  
  TConfiguration = class(TBaseElement, IConfiguration)
    FProperties: TStrings;
  protected
    function GetProperties: TStrings;
    function GetPropertyItem(const pName: string): string;
    function GetAsInteger(const pName: string): Integer; overload;
    function GetAsDouble(const pName: string): Double; overload;
    function GetAsString(const pName: string): string; overload;
    function GetValue(const pName: string; const pDefaultValue: Integer): Integer; overload;
    function GetValue(const pName: string; const pDefaultValue: Double): Double; overload;
    function GetValue(const pName: string; const pDefaultValue: string): string; overload;
    procedure SetValue(const pName: string; const Value: Integer); overload;
    procedure SetValue(const pName: string; const Value: Double); overload;
    procedure SetValue(const pName: string; const Value: string); overload;
    procedure SetPropertyItem(const pName: string; const Value: string);
    procedure Clear;
    property Properties: TStrings read GetProperties;
    property PropertyItem[const pName: string]: string read GetPropertyItem write SetPropertyItem;
  public
    constructor Create; override;
    destructor Destroy; override;
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
    property State: TPersistentStateKind read GetState write SetState;
  end;
  
  TSQLCommand = class(TBaseElement, ISQLCommand)
  private
    FName: string;
    FParams: ISQLCommandParams;    
  protected
    FPersistenceEngine: IPersistenceEngine;
    function GetName: string;
    function GetParams:ISQLCommandParams;
    procedure SetName(const Value: string);
    property Params: ISQLCommandParams read GetParams;
  public
    constructor Create(pPersistenceEngine: IPersistenceEngine); reintroduce;
  end;

  TSQLCommandQuery = class(TSQLCommand, ISQLCommandQuery)
  private
    FClassID: TGUID;
    FListID: TGUID;
    function CreateList: IInfraList;
  protected
    function GetResult: IInfraType;
    function GetList: IInfraList;
    function GetListID: TGUID;
    function GetClassID: TGUID;
    procedure SetListID(const Value: TGUID);
    procedure SetClassID(const Value: TGUID);
    property ClassID: TGUID read GetClassID write SetClassID;
    property ListID: TGUID read GetListID write SetListID;
  end;

  TSession = class(TBaseElement, ISession)
  private
    FPersistenceEngine: IPersistenceEngine;
    FCommandList: ISQLCommandList;
  protected
    function Load(const pCommandName: string; 
      const pObj: IInfraObject = nil): ISQLCommandQuery; overload;
    function Load(const pCommandName: string; 
      const pClassID: TGUID): ISQLCommandQuery; overload;
    function Load(const pCommandName: string; 
      const pClassID: TGUID; const pListID: TGUID): ISQLCommandQuery; overload;
    function Load(const pCommandName: string; 
      const pObj: IInfraObject; const pListID: TGUID): ISQLCommandQuery; overload;
    function Delete(const pCommandName: string; const pObj: IInfraObject): ISQLCommand;
    function Save(const pCommandName: string; const pObj: IInfraObject): ISQLCommand;
    function Flush: Integer;
  public
    constructor Create(const pPersistenceEngine: IPersistenceEngine); reintroduce;
  end;

  TPersistenceEngine = class(TBaseElement, IPersistenceEngine)
  private
    FConfiguration: IConfiguration;
    FConnection : IZConnection;
    function GetReader: ITemplateReader;
    procedure SetParameters(const pStatement: IZPreparedStatement; 
      const pSqlCommand: ISqlCommand);
    function GetRowFromResultSet(
      const pSqlCommand: ISQLCommand; 
      const pResultSet: IZResultSet): IInfraObject;
    procedure SetPropertyFromResultSet(const pAttribute: IInfraType; 
      const pResultSet: IZResultSet; pIndex: Integer);
  protected
    procedure SetConnection(const pConnection: IZConnection);
    procedure Load(const pSqlCommand: ISqlCommand; const pList: IInfraList);
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
  
  TTemplateReader = class(TElement, ITemplateReader)
  protected
    function Read(const pTemplateName: string): string;
  public
    constructor Create(pConfiguration: IConfiguration); reintroduce; virtual;
  end;

implementation

uses
  InfraPersistenceConsts,
  InfraBasicList,
  InfraConsts,
  List_SQLCommandList,
  List_SQLCommandParam,
  InfraValueType;

{ TConfiguration }

procedure TConfiguration.Clear;
begin
FProperties.clear;
end;

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
    raise EInfraArgumentError.Create('DriverManager in ConnectionProvider.Create');
  if not Assigned(pConfiguration) then
    raise EInfraArgumentError.Create('Configuration in ConnectionProvider.Create');
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
  i: integer;
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
    raise EPersistenceConnectionProviderError.Create(cErrorConnectionNotFoundOnPool);
  if pConnection.IsClosed then
    raise EPersistenceConnectionProviderError.Create(cErrorAlreadyClosedConnection);
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
  raise EPersistenceConnectionProviderError.Create(cErrorConnectionsLimitExceeded);
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
  FParams := TSQLCommandParams.Create;
end;

function TSQLCommand.GetName: string;
begin
  Result := FName;
end;

procedure TSQLCommand.SetName(const Value: string);
begin
  if not AnsiSameText(FName, Value) then
    FName := Value;
end;

function TSQLCommand.GetParams: ISQLCommandParams;
begin
  Result := FParams;
end;

{ TSQLCommandQuery }

function TSQLCommandQuery.GetClassID: TGUID;
begin
  Result := FClassID;
end;

function TSQLCommandQuery.GetListID: TGUID;
begin
  Result := FListID;
end;

function TSQLCommandQuery.CreateList: IInfraList;
begin
  Result := TypeService.CreateInstance(FListID) as IInfraList;
end;

function TSQLCommandQuery.GetResult: IInfraType;
var
  vList: IInfraList;
begin
  vList := CreateList;
  FPersistenceEngine.Load(Self, vList);
  // *** deveria gerar exceção caso o load acima retornar mais de um objeto na lista????
  Result := vList[0] as IInfratype;
end;

function TSQLCommandQuery.GetList: IInfraList;
begin
  Result := CreateList;
  FPersistenceEngine.Load(Self, Result);  
end;
    
procedure TSQLCommandQuery.SetClassID(const Value: TGUID);
begin
  FClassID := Value;
end;

procedure TSQLCommandQuery.SetListID(const Value: TGUID);
begin
  FListID := Value;
end;

{ TSession }

constructor TSession.Create(const pPersistenceEngine: IPersistenceEngine);
begin
  if not Assigned(pPersistenceEngine) then
    raise EInfraArgumentError.Create('PersistenceEngine in Session.Create');
  inherited Create;
  FPersistenceEngine := pPersistenceEngine;
  FCommandList := TSQLCommandList.Create;
end;

function TSession.Load(const pCommandName: string; const pClassID: TGUID): ISQLCommandQuery;
begin
  Result := TSQLCommandQuery.Create(FPersistenceEngine);
  Result.ListID := IInfraList;
  Result.ClassID := pClassID;
end;

function TSession.Load(const pCommandName: string; const pClassID, pListID: TGUID): ISQLCommandQuery;
begin
  Result := Load(pCommandName, pClassID);
  Result.ListID := pListID;
end;

function TSession.Load(const pCommandName: string; const pObj: IInfraObject = nil): ISQLCommandQuery;
begin
  Result := Load(pCommandName, pObj.TypeInfo.TypeID);
  if Assigned(pObj) then
    Result.Params.AddObject(pObj);
end;

function TSession.Load(const pCommandName: string; const pObj: IInfraObject; const pListID: TGUID): ISQLCommandQuery;
begin
  Result := Load(pCommandName, pObj.TypeInfo.TypeID, pListID);
  Result.Params.AddObject(pObj);
end;

function TSession.Save(const pCommandName: string; const pObj: IInfraObject): ISQLCommand;
begin
  Result := TSQLCommand.Create(FPersistenceEngine);
  with Result do
  begin
    Name := pCommandName;
    Params.AddObject(pObj);
  end;
  FCommandList.Add(Result);
end;

function TSession.Delete(const pCommandName: string; const pObj: IInfraObject): ISQLCommand;
var
  vState: IPersistentState;
begin
  if Supports(pObj, IPersistentState, vState) then
  begin
    vState.State := osDeleted;
    Save(pCommandName, pObj);
  end;
end;

function TSession.Flush: Integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FCommandList.Count - 1 do
    Result := Result + FPersistenceEngine.Execute(FCommandList[i]).AsInteger;
end;

{ TPersistenceEngine }

constructor TPersistenceEngine.Create(pConfiguration: IConfiguration);
begin
  if not Assigned(pConfiguration) then
    raise EInfraArgumentError.Create('Configuration in PersistenceEngine.Create');
  inherited Create;
  FConfiguration := pConfiguration;
end;

function TPersistenceEngine.GetReader: ITemplateReader;
var
  vReaderClassName: string;
  vReaderTypeInfo: IClassInfo;
begin
  vReaderClassName := FConfiguration.GetValue(cCONFIGKEY_TEMPLATETYPE, EmptyStr);
  if vReaderClassName = EmptyStr then
    raise EPersistenceTemplateError.Create(cErrorTemplateTypeInvalid);
  vReaderTypeInfo := TypeService.GetType(vReaderClassName, True);
  Result := TypeService.CreateInstance(vReaderTypeInfo) as ITemplateReader;
end;

{
  carregar a sql usando um reader com base no Name do pSqlCommand vReader.Read(pSqlCommand.Name)
  preencher os params da sql com base nos Params do pSqlCommand
  pegar o connection no connectionprovider
  executa a sql  e retornar  a quantidade de registros afetados
}
function TPersistenceEngine.Execute(const pSqlCommand: ISqlCommand): IInfraInteger;
var
  vReader: ITemplateReader;
  vSQL: string;
  vST: IZPreparedStatement;

begin
  vReader := GetReader;
  vSQL := vReader.Read(pSqlCommand.Name);
  vST := FConnection.PrepareStatement(vSQL);
  SetParameters(vST, pSqlCommand);
  Result := TInfraInteger.NewFrom(vST.ExecuteUpdatePrepared);
end;

{ carregar a sql usando um reader com base no Name do pSqlCommand
  preencher os params da sql com base nos Params do pSqlCommand
  executa a sql e pega um IZStatement
  Faz um laço para pegar cada registro
  cria um objeto com base no ClassType do pSqlCommand,
  Seta o estado persistent e clean ao objeto criado
  faz a carga dos atributos com base no registro
  Adiciona o novo objeto em pList retorna a lista }
procedure TPersistenceEngine.Load(const pSqlCommand: ISqlCommand; 
  const pList: IInfraList);
var
  vReader: ITemplateReader;
  vSQL: string;
  vST: IZPreparedStatement;
  vRS: IZResultSet;
  vObject: IInfraObject;
begin
  vReader := GetReader;
  vSQL := vReader.Read(pSqlCommand.Name);
  try
   vST := FConnection.PrepareStatement(vSQL);
   SetParameters(vST, pSqlCommand);
   vRS := vST.ExecuteQueryPrepared;
   while vRS.Next do
   begin
     vObject := GetRowFromResultSet(pSqlCommand, vRS);
     pList.Add(vObject);
   end;   
  finally
    vRs.Close;
    vST.Close;
  end;
end;

procedure TPersistenceEngine.SetConnection(const pConnection: IZConnection);
begin
  // preencher o connection provider com o pConnection
end;

function TPersistenceEngine.GetRowFromResultSet(const pSqlCommand: ISQLCommand; 
  const pResultSet: IZResultSet): IInfraObject;
var
  vIndex: integer;
  vAttribute: IInfraType;
begin
  Result := TypeService.CreateInstance((pSqlCommand as ISQLCommandQuery).GetClassID) as IInfraObject;
  if Assigned(Result) then
  begin
    for vIndex :=0 to pResultSet.GetMetadata.GetColumnCount-1 do 
    begin
      vAttribute := Result.TypeInfo.GetProperty(
        Result, pResultSet.GetMetadata.GetColumnName(vIndex)) as IInfraType;
      SetPropertyFromResultSet(vAttribute, pResultSet, vIndex);
    end;
  end;
end;

procedure TPersistenceEngine.SetPropertyFromResultSet(
  const pAttribute: IInfraType; const pResultSet: IZResultSet; pIndex: Integer);
begin
  // *** Teria de tratar o InfraType aqui se for um Object ou InfraList
  // *** por que provavelmente estará apontando para outro objeto e 
  // *** talvez tem que carregar com base em colunas de um join no template.
  (pAttribute.TypeInfo as IZTypeAnnotation).NullSafeGet(pResultSet, 
    pIndex, pAttribute)
end;

procedure TPersistenceEngine.SetParameters(const pStatement: IZPreparedStatement; 
  const pSqlCommand: ISqlCommand);
var
  vIndex: integer;
  vTypeInfo: IClassInfo;
  vParamValue: IInfraType;
  vParams: TStrings;
begin
  vParams := pStatement.GetParameters;
  for vIndex := 0 to vParams.Count-1 do
  begin
    vParamValue := pSqlCommand.Params[vParams[vIndex]];
    if Assigned(vParamValue) then
      (vTypeInfo as IZTypeAnnotation).NullSafeSet(pStatement, vIndex,  vParamValue)
    else
      Raise EPersistenceengineError.CreateFmt(cErrorPersistenceEngineParamNotFound, [vParams[vIndex]]);
  end;
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

{ TTemplateReader }

constructor TTemplateReader.Create(pConfiguration: IConfiguration);
begin
  raise EPersistenceTemplateError.Create(cErrorTemplateTryCreateClassBase);
end;

function TTemplateReader.Read(const pTemplateName: string): string;
begin
  Result := '';
end;

// Não entendi mas se por direto no Initialization acontece Access Violations.
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


