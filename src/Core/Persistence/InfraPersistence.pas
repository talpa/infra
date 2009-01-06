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
  InfraPersistenceIntf;
  
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
    FSlots: array of IZConnection; // O pool
    FCriticalSection: TCriticalSection;
    function BuildConnectionString(pConfiguration: IConfiguration): string;
    procedure CloseConnections; // CriticalSection usado para evitar conflitos em aplicações multi-thread
  protected
    function GetFreeConnection: IZConnection; // Procura por uma conexao livre
    function CreateConnection: IZConnection; // Cria uma nova conexao
    function FindConnection(const pConnection: IZConnection): IZConnection; // Procura por uma conexao no pool
  public
    constructor Create(pConfiguration: IConfiguration); reintroduce;
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
    FConnnectionProvider: IConnectionProvider;
    FParse: IParseParams;
    function GetReader: ITemplateReader;
    procedure SetParameters(const pStatement: IZPreparedStatement;
      const pSqlCommand: ISqlCommand);
    function GetRowFromResultSet(
      const pSqlCommand: ISQLCommandQuery;
      const pResultSet: IZResultSet): IInfraObject;
  protected
    procedure DoLoad(const pST: IZPreparedStatement; const pSqlCommand:
      ISQLCommandQuery; const pList: IInfraList);
    procedure SetConnection(const pConnection: IZConnection);
    procedure Load(const pSqlCommand: ISQLCommandQuery; const pList: IInfraList);
    function Execute(const pSqlCommand: ISqlCommand): Integer;
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
  private
    FConfiguration: IConfiguration;
  protected
    function Read(const pTemplateName: string): string;
    function GetConfiguration: IConfiguration;
    procedure SetConfiguration(const Value: IConfiguration);
    property Configuration: IConfiguration read GetConfiguration write SetConfiguration;
  public
    constructor Create; reintroduce; virtual;
  end;

  TParseParams = class(TBaseElement, IParseParams)
  private
    FParams: TStrings;
    FMacroParams: TStrings;
    FSQL: String;
    function IsLiteral(CurChar: Char): Boolean;
    function NameDelimiter(CurChar: Char): Boolean;
    procedure StripChar(TempBuf: PChar; Len: Word);
    function StripLiterals(Buffer: PChar): string;
    function IsParameter(CurPos: PChar; Literal: Boolean): Boolean;
    function IsFalseParameter(CurPos: PChar; Literal: Boolean): Boolean;
  protected
    procedure Parse(const pSQL: string);
    function GetParams: TStrings;
    function GetMacroParams: TStrings;
  public
    constructor Create; override;
    destructor Destroy; override;
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
  @param AConfiguration Um objeto do tipo IConfiguration que contém todas as
    informações para criar uma nova conexão
}
constructor TConnectionProvider.Create(pConfiguration: IConfiguration);
var
  iMax: Integer;
begin
  if not Assigned(pConfiguration) then
    raise EInfraArgumentError.Create('Configuration in ConnectionProvider.Create');
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FConfiguration := pConfiguration;
  iMax := FConfiguration.GetValue(cCONFIGKEY_MAXCONNECTIONS, cGlobalMaxConnections);
  SetLength(FSlots, iMax);
end;

destructor TConnectionProvider.Destroy;
begin
  CloseConnections;
  SetLength(FSlots, 0);
  FCriticalSection.Free;
  inherited;
end;

procedure TConnectionProvider.CloseConnections;
var
  i: Integer;
begin
  for i := Low(FSlots) to High(FSlots) do
    if Assigned(FSlots[i]) then
      FSlots[i].Close;
end;

procedure TConnectionProvider.Close;
var
  i: integer;
begin
  for i := Low(FSlots) to High(FSlots) do
    if Assigned(FSlots[i]) then
      ReleaseConnection(FSlots[i]);
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
  for i := Low(FSlots) to High(FSlots) do
    if FSlots[i] = pConnection then
    begin
      Result := FSlots[i];
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
  for i := Low(FSlots) to High(FSlots) do
    if not Assigned(FSlots[i]) or
      (Assigned(FSlots[i]) and FSlots[i].IsClosed) then
    begin
      Result := FSlots[i];
      Break;
    end;
end;

function TConnectionProvider.BuildConnectionString(pConfiguration: IConfiguration): string;
begin
  Result := 'zdbc:' + pConfiguration.GetAsString(cCONFIGKEY_DRIVER) +
    '://' + pConfiguration.GetAsString(cCONFIGKEY_HOSTNAME) +
    '/' + pConfiguration.GetAsString(cCONFIGKEY_DATABASENAME) +
    '?username=' + pConfiguration.GetAsString(cCONFIGKEY_USERNAME) +
    ';password=' + pConfiguration.GetAsString(cCONFIGKEY_PASSWORD);
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
  for i := Low(FSlots) to High(FSlots) do
    if not Assigned(FSlots[i]) then
    begin
      FSlots[i] := DriverManager.GetConnection(BuildConnectionString(FConfiguration));
      Result := FSlots[i];
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
    // *** alterado até que o pool funcione
    // Result := CreateConnection;
    Result := DriverManager.GetConnection(BuildConnectionString(FConfiguration));
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
  Result.Name := pCommandName;
  Result.ListID := IInfraList;
  if not IsEqualGUID(pClassID, NullGUID) then
    Result.ClassID := pClassID;
end;

function TSession.Load(const pCommandName: string; const pClassID, pListID: TGUID): ISQLCommandQuery;
begin
  Result := Load(pCommandName, pClassID);
  Result.ListID := pListID;
end;

function TSession.Load(const pCommandName: string; const pObj: IInfraObject = nil): ISQLCommandQuery;
begin
  if Assigned(pObj) then
    Result := Load(pCommandName, pObj.TypeInfo.TypeID)
  else
    Result := Load(pCommandName, NullGUID);
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
    Result := Result + FPersistenceEngine.Execute(FCommandList[i]);
end;

{ TPersistenceEngine }

constructor TPersistenceEngine.Create(pConfiguration: IConfiguration);
begin
  inherited Create;
  if not Assigned(pConfiguration) then
    raise EInfraArgumentError.Create('Configuration in PersistenceEngine.Create');
  FConfiguration := pConfiguration;
  FConnnectionProvider := TConnectionProvider.Create(pConfiguration);
  FParse := TParseParams.Create;
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
  Result.Configuration := FConfiguration;
end;

{
  carregar a sql usando um reader com base no Name do pSqlCommand vReader.Read(pSqlCommand.Name)
  preencher os params da sql com base nos Params do pSqlCommand
  pegar o connection no connectionprovider
  executa a sql  e retornar  a quantidade de registros afetados
}
function TPersistenceEngine.Execute(const pSqlCommand: ISqlCommand): Integer;
var
  vReader: ITemplateReader;
  vSQL: string;
  vStatement: IZPreparedStatement;
  vConnection: IZConnection;
begin
  vReader := GetReader;
  vSQL := vReader.Read(pSqlCommand.Name);
  FParse.Parse(vSQL);
  // *** 1) Acho que os parâmetros macros de FParse devem ser substituidos aqui antes de chamar o PrepareStatementWithParams
  // *** 2) Acho que poderia chamar o PrepareStatementWithParams passando o FParse.Params.GetParams
  vConnection := FConnnectionProvider.GetConnection;
  vStatement := vConnection.PrepareStatementWithParams(vSQL, FParse.GetParams);
  SetParameters(vStatement, pSqlCommand);
  // *** 3) Acho que pode retornar um simples Integer.
  Result := vStatement.ExecuteUpdatePrepared;
end;

procedure TPersistenceEngine.DoLoad(const pST: IZPreparedStatement;
  const pSqlCommand: ISQLCommandQuery; const pList: IInfraList);
var
  vResultSet: IZResultSet;
  vObject: IInfraObject;
begin
  vResultSet := pST.ExecuteQueryPrepared;
  try
    while vResultSet.Next do
    begin
      vObject :=
        GetRowFromResultSet(pSqlCommand, vResultSet);
      pList.Add(vObject);
    end;
  finally
    vResultSet.Close;
  end;
end;

{ carregar a sql usando um reader com base no Name do pSqlCommand
  preencher os params da sql com base nos Params do pSqlCommand
  executa a sql e pega um IZStatement
  Faz um laço para pegar cada registro
  cria um objeto com base no ClassType do pSqlCommand,
  Seta o estado persistent e clean ao objeto criado
  faz a carga dos atributos com base no registro
  Adiciona o novo objeto em pList retorna a lista }
procedure TPersistenceEngine.Load(const pSqlCommand: ISQLCommandQuery; 
  const pList: IInfraList);
var
  vReader: ITemplateReader;
  vSQL: string;
  vStatement: IZPreparedStatement;
  vConnection: IZConnection;
begin
  vReader := GetReader;
  vSQL := vReader.Read(pSqlCommand.Name);
  // *** 1) se a SQL está vazia aqui deveria gerar exceção ou deveria ser dentro
  // do vReader.Read????
  FParse.Parse(vSQL);
  // *** 2) Acho que os parâmetros macros de FParse devem ser substituidos aqui
  // antes de chamar o PrepareStatementWithParams
  try
    vConnection := FConnnectionProvider.GetConnection;
    vStatement := vConnection.PrepareStatementWithParams(vSQL, FParse.GetParams);
    SetParameters(vStatement, pSqlCommand);
    DoLoad(vStatement, pSqlCommand, pList);
  finally
    vStatement.Close;
  end;
end;

procedure TPersistenceEngine.SetConnection(const pConnection: IZConnection);
begin
  // preencher o connection provider com o pConnection
end;

// *** 1) Como poderiamos carregar Objetos/Listas relacionados ao objeto atual a
// ***    partir de colunas vindos no resultset frutos de um join?
function TPersistenceEngine.GetRowFromResultSet(
  const pSqlCommand: ISQLCommandQuery;
  const pResultSet: IZResultSet): IInfraObject;
var
  vIndex: integer;
  vAttribute: IInfraType;
  vZeosType: IZTypeAnnotation;
  vAliasName: string;
begin
  // *** Será que isso deveria estar aqui?????
  if IsEqualGUID(pSqlCommand.GetClassID, NullGUID) then
    Raise EPersistenceEngineError.Create(
      cErrorPersistenceEngineObjectIDUndefined);
  Result := TypeService.CreateInstance(pSqlCommand.GetClassID) as IInfraObject;
  if Assigned(Result) then
  begin
    // A lista de colunas do ResultSet.GetMetadata do Zeos começa do 1.
    for vIndex := 1 to pResultSet.GetMetadata.GetColumnCount do
    begin
      vAliasName := pResultSet.GetMetadata.GetColumnLabel(vIndex);
      vAttribute :=
        Result.TypeInfo.GetProperty(Result, vAliasName) as IInfraType;
      if not Assigned(vAttribute) then
        Raise EPersistenceEngineError.CreateFmt(
          cErrorPersistenceEngineAttributeNotFound,
          [vAliasName, pResultSet.GetMetadata.GetColumnName(vIndex)]);
      if Supports(vAttribute.TypeInfo, IZTypeAnnotation, vZeosType) then
        vZeosType.NullSafeGet(pResultSet, vIndex, vAttribute)
      else
        Raise EPersistenceEngineError.CreateFmt(
          cErrorPersistenceEngineCannotMapAttribute, [vAttribute.TypeInfo.Name]);
    end;
  end;
end;

// *** o que acontece caso tenhamos um template com nomes de parametros repetidos?
procedure TPersistenceEngine.SetParameters(
  const pStatement: IZPreparedStatement; const pSqlCommand: ISqlCommand);
var
  vIndex: integer;
  vParamValue: IInfraType;
  vParams: TStrings;
  vZeosType: IZTypeAnnotation;
begin
  vParams := pStatement.GetParameters;
  for vIndex := 0 to vParams.Count-1 do
  begin
    vParamValue := pSqlCommand.Params[vParams[vIndex]];
    if Assigned(vParamValue)
      and Supports(vParamValue.TypeInfo, IZTypeAnnotation, vZeosType) then
      // Aumenta o vIndex por que no Zeos as colunas começam de 1
      vZeosType.NullSafeSet(pStatement, vIndex+1, vParamValue)
    else
      Raise EPersistenceEngineError.CreateFmt(
        cErrorPersistenceEngineParamNotFound, [vParams[vIndex]]);
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

constructor TTemplateReader.Create;
begin
  raise EPersistenceTemplateError.Create(cErrorTemplateTryCreateClassBase);
end;

function TTemplateReader.GetConfiguration: IConfiguration;
begin
  Result := FConfiguration;
end;

procedure TTemplateReader.SetConfiguration(
  const Value: IConfiguration);
begin
  FConfiguration := Value;
end;

function TTemplateReader.Read(const pTemplateName: string): string;
begin
  Result := '';
end;

{ TParseParams }

const
  cLiterals = ['''', '"', '`'];

constructor TParseParams.Create;
begin
  inherited;
  FParams := TStringList.Create;
  FMacroParams := TStringList.Create;
end;

destructor TParseParams.Destroy;
begin
  FParams.Free;
  FMacroParams.Free;
  inherited;
end;

function TParseParams.NameDelimiter(CurChar: Char): Boolean;
begin
  Result := CurChar in [' ', ',', ';', ')', '*', #13, #10];
end;

function TParseParams.IsLiteral(CurChar: Char): Boolean;
begin
  Result := CurChar in cLiterals;
end;

procedure TParseParams.StripChar(TempBuf: PChar; Len: Word);
begin
  if TempBuf^ in cLiterals then
    StrMove(TempBuf, TempBuf + 1, Len - 1);
  if TempBuf[StrLen(TempBuf) - 1] in cLiterals then
    TempBuf[StrLen(TempBuf) - 1] := #0;
end;

function TParseParams.StripLiterals(Buffer: PChar): string;
var
  vLen: Word;
  vTempBuf: PChar;
begin
  vLen := StrLen(Buffer) + 1;
  vTempBuf := AllocMem(vLen);
  try
    StrCopy(vTempBuf, Buffer);
    StripChar(vTempBuf, vLen);
    Result := StrPas(vTempBuf);
  finally
    FreeMem(vTempBuf, vLen);
  end;
end;

function TParseParams.IsParameter(CurPos: PChar; Literal: Boolean): Boolean;
begin
  Result := not Literal and
    ( (CurPos^ in [':', '#']) and not ((CurPos + 1)^ in [':', '#']) )
end;

function TParseParams.IsFalseParameter(CurPos: PChar; Literal: Boolean): Boolean;
begin
  Result := not Literal and
    ( (CurPos^ in [':', '#']) and ((CurPos + 1)^ in [':', '#']) )
end;

procedure TParseParams.Parse(const pSQL: string);
var
  vValue, vCurPos, vStartPos: PChar;
  vCurChar: Char;
  vLiteral: Boolean;
  vEmbeddedLiteral, vIsMacro: Boolean;
  vName: string;
begin
  FSQL := pSQL;
  FParams.Clear;
  FMacroParams.Clear;
  vValue := PChar(FSQL);
  vCurPos := vValue;
  vLiteral := False;
  vEmbeddedLiteral := False;
  repeat
    while (vCurPos^ in LeadBytes) do Inc(vCurPos, 2);
    vCurChar := vCurPos^;
    if IsParameter(vCurPos, vLiteral) then
    begin
      vIsMacro := (vCurChar = '#');
      vStartPos := vCurPos;
      while (vCurChar <> #0) and (vLiteral or not NameDelimiter(vCurChar)) do
      begin
        Inc(vCurPos);
        while (vCurPos^ in LeadBytes) do Inc(vCurPos, 2);
        vCurChar := vCurPos^;
        if IsLiteral(vCurchar) then
        begin
          vLiteral := vLiteral xor True;
          if vCurPos = vStartPos + 1 then vEmbeddedLiteral := True;
        end;
      end;
      vCurPos^ := #0;
      if vEmbeddedLiteral then
      begin
        vName := StripLiterals(vStartPos + 1);
        vEmbeddedLiteral := False;
      end
      else vName := StrPas(vStartPos + 1);
      if vIsMacro then
        FMacroParams.Add(vName)
      else
        FParams.Add(vName);
      vCurPos^ := vCurChar;
      vStartPos^ := '?';
      Inc(vStartPos);
      StrMove(vStartPos, vCurPos, StrLen(vCurPos) + 1);
      vCurPos := vStartPos;
    end
    else if IsFalseParameter(vCurPos, vLiteral) then
      StrMove(vCurPos, vCurPos + 1, StrLen(vCurPos) + 1)
    else if IsLiteral(vCurChar) then
      vLiteral := vLiteral xor True;
    Inc(vCurPos);
  until vCurChar = #0;
end;

function TParseParams.GetMacroParams: TStrings;
begin
  Result := FMacroParams;
end;

function TParseParams.GetParams: TStrings;
begin
  Result := FParams;
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
