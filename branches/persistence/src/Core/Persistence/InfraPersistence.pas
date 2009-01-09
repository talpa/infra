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
  /// Classe para armazenar as configurações do Framework
  TConfiguration = class(TBaseElement, IConfiguration)
  private
    /// Aqui são armazenadas as configurações no formato <nome>=<valor>
    FProperties: TStrings;
    function GetProperties: TStrings;
  protected
    function GetAsInteger(const pName: string): Integer; overload;
    function GetAsDouble(const pName: string): Double; overload;
    function GetAsString(const pName: string): string; overload;
    function GetValue(const pName: string; const pDefaultValue: Integer): Integer; overload;
    function GetValue(const pName: string; const pDefaultValue: Double): Double; overload;
    function GetValue(const pName: string; const pDefaultValue: string): string; overload;
    procedure SetValue(const pName: string; const Value: Integer); overload;
    procedure SetValue(const pName: string; const Value: Double); overload;
    procedure SetValue(const pName: string; const Value: string); overload;
    procedure Clear;
    property Properties: TStrings read GetProperties;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  /// Descrição da classe
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

  /// Descrição da classe
  TSQLCommand = class(TBaseElement, ISQLCommand)
  private
    FName: string;
    FParams: ISQLCommandParams;
    FPersistenceEngine: IPersistenceEngine;
  protected
    function GetName: string;
    function GetParams:ISQLCommandParams;
    procedure SetName(const Value: string);
    /// PersistenceEngine ao qual o SQLCommand está vinculado
    property PersistenceEngine: IPersistenceEngine read FPersistenceEngine;
    property Params: ISQLCommandParams read GetParams;
  public
    constructor Create(pPersistenceEngine: IPersistenceEngine); reintroduce;
  end;

  /// Descrição da classe
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

  /// Descrição da classe
  TSession = class(TBaseElement, ISession)
  private
    FPersistenceEngine: IPersistenceEngine;
    FPendingCommands: ISQLCommandList;
  protected
    function CreateQuery(const pCommandName: string;
      const pObj: IInfraObject = nil): ISQLCommandQuery; overload;
    function CreateQuery(const pCommandName: string;
      const pClassID: TGUID): ISQLCommandQuery; overload;
    function CreateQuery(const pCommandName: string;
      const pClassID: TGUID; const pListID: TGUID): ISQLCommandQuery; overload;
    function CreateQuery(const pCommandName: string; const pObj: IInfraObject;
      const pListID: TGUID): ISQLCommandQuery; overload;
    function Delete(const pCommandName: string;
      const pObj: IInfraObject): ISQLCommand;
    function Save(const pCommandName: string;
      const pObj: IInfraObject): ISQLCommand;
    function Flush: Integer;
  public
    constructor Create(
      const pPersistenceEngine: IPersistenceEngine); reintroduce;
  end;

  /// Descrição da classe
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
    procedure DoLoad(const pStatement: IZPreparedStatement; const pSqlCommand:
        ISQLCommandQuery; const pList: IInfraList);
    function GetConnectionProvider: IConnectionProvider;
  protected
    procedure SetConnection(const pConnection: IZConnection);
    procedure Load(const pSqlCommand: ISQLCommandQuery;
      const pList: IInfraList);
    function Execute(const pConnection: IZConnection;
      const pSqlCommand: ISqlCommand): Integer;
  public
    constructor Create(pConfiguration: IConfiguration;
      pConnectionProvider: IConnectionProvider); reintroduce;
    property ConnectionProvider: IConnectionProvider read GetConnectionProvider;
  end;

  /// Descrição da classe
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

  /// Descrição da classe
  TTemplateReader = class(TElement, ITemplateReader)
  private
    FConfiguration: IConfiguration;
  protected
    function ReadFromStream(const pStream: TStream): string; 
    function Read(const pTemplateName: string): string; virtual; abstract;
    function GetConfiguration: IConfiguration;
    procedure SetConfiguration(const Value: IConfiguration);
    property Configuration: IConfiguration read GetConfiguration
      write SetConfiguration;
  public
    constructor Create; reintroduce; virtual;
  end;

  /// Classe utilitária para obter parâmetros e macros de uma instrução SQL
  TParseParams = class(TBaseElement, IParseParams)
  private
    FParams: TStrings;
    FMacroParams: TStrings;
  protected
    function Parse(const pSQL: string): string;
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
  InfraValueType,
  RegExpr,
  InfraPersistenceConnProvider;

{ TConfiguration }

/// Cria uma nova instância de TConfiguration
constructor TConfiguration.Create;
begin
  inherited;
  FProperties := TStringList.Create;
end;

/// Destrói a instância de TConfiguration
destructor TConfiguration.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
end;

///  Limpa todas as propriedades
procedure TConfiguration.Clear;
begin
  FProperties.Clear;
end;

{**
  Obtem o valor de uma propriedade como Double
  @param pName Nome da propriedade da qual se quer obter o valor
  @returns O valor da propriedade como Double
}
function TConfiguration.GetAsDouble(const pName: string): Double;
begin
  Result := StrToFloat(FProperties.Values[pName]);
end;

{**
  Obtem o valor de uma propriedade como Integer
  @param pName Nome da propriedade da qual se quer obter o valor
  @returns O valor da propriedade como Integer
}
function TConfiguration.GetAsInteger(const pName: string): Integer;
begin
  Result := StrToInt(FProperties.Values[pName]);
end;

{**
  Obtem o valor de uma propriedade como string
  @param pName Nome da propriedade da qual se quer obter o valor
  @returns O valor da propriedade como string
}
function TConfiguration.GetAsString(const pName: string): string;
begin
  Result := FProperties.Values[pName];
end;

{**
  Obtem uma referencia ao objeto que contém as propriedades
  @returns Um objeto do tipo TStrings
}
function TConfiguration.GetProperties: TStrings;
begin
  Result := FProperties;
end;

{**
  Obtem o valor de uma propriedade como Integer e, se não existir, o valor default
  @param pName Nome da propriedade da qual se quer obter o valor
  @returns O valor da propriedade como Integer ou o valor default
}
function TConfiguration.GetValue(const pName: string;
  const pDefaultValue: Integer): Integer;
begin
  if FProperties.IndexOfName(pName) <> -1 then
    Result := StrToIntDef(FProperties.Values[pName], pDefaultValue)
  else
    Result := pDefaultValue;
end;

{**
  Obtem o valor de uma propriedade como Double e, se não existir, o valor default
  @param pName Nome da propriedade da qual se quer obter o valor
  @returns O valor da propriedade como Double ou o valor default
}
function TConfiguration.GetValue(const pName: string;
  const pDefaultValue: Double): Double;
begin
  if FProperties.IndexOfName(pName) <> -1 then
    Result := StrToFloatDef(FProperties.Values[pName], pDefaultValue)
  else
    Result := pDefaultValue;
end;

{**
  Obtem o valor de uma propriedade como string e, se não existir, o valor default
  @param pName Nome da propriedade da qual se quer obter o valor
  @returns O valor da propriedade como string ou o valor default
}
function TConfiguration.GetValue(const pName, pDefaultValue: string): string;
begin
  if FProperties.IndexOfName(pName) <> -1 then
    Result := FProperties.Values[pName]
  else
    Result := pDefaultValue;
end;

{**
  Atribui o valor de uma propriedade como Integer
  @param pName Nome da propriedade à qual se quer atribuir o valor
}
procedure TConfiguration.SetValue(const pName: string; const Value: Integer);
begin
  FProperties.Values[pName] := IntToStr(Value);
end;

{**
  Atribui o valor de uma propriedade como Double
  @param pName Nome da propriedade à qual se quer atribuir o valor
}
procedure TConfiguration.SetValue(const pName: string; const Value: Double);
begin
  FProperties.Values[pName] := FloatToStr(Value);
end;

{**
  Atribui o valor de uma propriedade como string
  @param pName Nome da propriedade à qual se quer atribuir o valor
}
procedure TConfiguration.SetValue(const pName, Value: string);
begin
  FProperties.Values[pName] := Value;
end;

{ TPersistentState }

{*
  @return ResultDescription
}
function TPersistentState.GetIsPersistent: Boolean;
begin
  Result := FIsPersistent;
end;

{*
  @return ResultDescription
}
function TPersistentState.GetState: TPersistentStateKind;
begin
  Result := FState;
end;

{*
  @param Value   ParameterDescription
  @return ResultDescription
}
procedure TPersistentState.SetIsPersistent(Value: Boolean);
begin
  FIsPersistent := Value;
end;

{*

  @param Value   ParameterDescription
  @return ResultDescription
}
procedure TPersistentState.SetState(Value: TPersistentStateKind);
begin
  FState := Value;
end;

{ TSQLCommand }

{*
  Cria uma nova instância de TSQLCommand.
  @param pPersistenceEngine
}
constructor TSQLCommand.Create(pPersistenceEngine: IPersistenceEngine);
begin
  inherited Create;
  FPersistenceEngine := pPersistenceEngine;
  FParams := TSQLCommandParams.Create;
end;

{*

  @return Retorna o nome do template
}
function TSQLCommand.GetName: string;
begin
  Result := FName;
end;

{*

  @param Value O nome do template
}
procedure TSQLCommand.SetName(const Value: string);
begin
  FName := Value;
end;

{*

  @return ResultDescription
}
function TSQLCommand.GetParams: ISQLCommandParams;
begin
  Result := FParams;
end;

{ TSQLCommandQuery }
{*

  @return ResultDescription
}
function TSQLCommandQuery.GetClassID: TGUID;
begin
  Result := FClassID;
end;

{*

  @return ResultDescription
}
function TSQLCommandQuery.GetListID: TGUID;
begin
  Result := FListID;
end;

{*

  @return ResultDescription
}
function TSQLCommandQuery.CreateList: IInfraList;
begin
  Result := TypeService.CreateInstance(FListID) as IInfraList;
end;

{*

  @return ResultDescription
}
function TSQLCommandQuery.GetResult: IInfraType;
var
  vList: IInfraList;
begin
  vList := CreateList;
  PersistenceEngine.Load(Self, vList);
  // *** deveria gerar exceção caso o load acima retornar mais de um objeto na lista????
  Result := vList[0] as IInfratype;
end;

{*

  @return ResultDescription
}
function TSQLCommandQuery.GetList: IInfraList;
begin
  Result := CreateList;
  PersistenceEngine.Load(Self, Result);
end;

{*

  @param Value   ParameterDescription
}
procedure TSQLCommandQuery.SetClassID(const Value: TGUID);
begin
  FClassID := Value;
end;

{*

  @param Value   ParameterDescription
}
procedure TSQLCommandQuery.SetListID(const Value: TGUID);
begin
  FListID := Value;
end;

{ TSession }
{*

  @param pPersistenceEngine   ParameterDescription
}
constructor TSession.Create(const pPersistenceEngine: IPersistenceEngine);
begin
  if not Assigned(pPersistenceEngine) then
    raise EInfraArgumentError.Create('PersistenceEngine in Session.Create');
  inherited Create;
  FPersistenceEngine := pPersistenceEngine;
  FPendingCommands := TSQLCommandList.Create;
end;

{*

  @param pCommandName   ParameterDescription
  @param pClassID   ParameterDescription
  @return ResultDescription
}
function TSession.CreateQuery(const pCommandName: string; const pClassID: TGUID): ISQLCommandQuery;
begin
  Result := TSQLCommandQuery.Create(FPersistenceEngine);
  Result.Name := pCommandName;
  Result.ListID := IInfraList;
  if not IsEqualGUID(pClassID, NullGUID) then
    Result.ClassID := pClassID;
end;

{*

  @param pCommandName   ParameterDescription
  @param pClassID   ParameterDescription
  @param pListID   ParameterDescription
  @return ResultDescription
}
function TSession.CreateQuery(const pCommandName: string; const pClassID, pListID: TGUID): ISQLCommandQuery;
begin
  Result := CreateQuery(pCommandName, pClassID);
  Result.ListID := pListID;
end;

{*

  @param pCommandName   ParameterDescription
  @param pObj   ParameterDescription
  @param pListID   ParameterDescription
  @return ResultDescription
}
function TSession.CreateQuery(const pCommandName: string; const pObj: IInfraObject = nil): ISQLCommandQuery;
begin
  if Assigned(pObj) then
    Result := CreateQuery(pCommandName, pObj.TypeInfo.TypeID)
  else
    Result := CreateQuery(pCommandName, NullGUID);
  if Assigned(pObj) then
    Result.Params.AddObject(pObj);
end;

{*

  @param pCommandName   ParameterDescription
  @param pObj   ParameterDescription
  @return ResultDescription
}
function TSession.CreateQuery(const pCommandName: string; const pObj: IInfraObject; const pListID: TGUID): ISQLCommandQuery;
begin
  Result := CreateQuery(pCommandName, pObj.TypeInfo.TypeID, pListID);
  Result.Params.AddObject(pObj);
end;

{*

  @param pCommandName   ParameterDescription
  @param pObj   ParameterDescription
  @return ResultDescription
}
function TSession.Save(const pCommandName: string; const pObj: IInfraObject): ISQLCommand;
begin
  Result := TSQLCommand.Create(FPersistenceEngine);
  with Result do
  begin
    Name := pCommandName;
    Params.AddObject(pObj);
  end;
  FPendingCommands.Add(Result);
end;

{*

  @return ResultDescription
}
function TSession.Delete(const pCommandName: string; const pObj: IInfraObject): ISQLCommand;
var
  vState: IPersistentState;
begin
//  if Supports(pObj, IPersistentState, vState) then
//  begin
//    vState.State := osDeleted;
    Save(pCommandName, pObj);
//  end;
end;

{*

  @param pConfiguration   ParameterDescription
  @return ResultDescription
}
function TSession.Flush: Integer;
var
  i: integer;
  vConnection: IZConnection;
begin
  // TODO: Dar atenção a isto porque, pelo modelo atual, pra cada Execute ele vai
  // abrir uma nova conexão. Isto impediria que todas as operações fossem feitas
  // no contexto de UMA transação
  Result := 0;
  vConnection := FPersistenceEngine.ConnectionProvider.GetConnection;
  for i := 0 to FPendingCommands.Count - 1 do
    Result := Result + FPersistenceEngine.Execute(vConnection, FPendingCommands[i]);

  // Se deu tudo ok, limpa a lista de pendencias
  FPendingCommands.Clear;
end;

{ TPersistenceEngine }

{*
  Cria uma nova instância de TPersistenceEngine
  @param pConfiguration   ParameterDescription
}
constructor TPersistenceEngine.Create(pConfiguration: IConfiguration;
  pConnectionProvider: IConnectionProvider);
begin
  inherited Create;
  // O argumento pConfiguration sempre é requerido, visto que é dele que
  // o Framework obterá as suas configurações
  if not Assigned(pConfiguration) then
    raise EInfraArgumentError.Create('pConfiguration');

  FConfiguration := pConfiguration;
  FConnnectionProvider := pConnectionProvider;
  FParse := TParseParams.Create;
end;

{*

  @return ResultDescription
}
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

{*
  Executa uma instrução SQL contra o banco baseado nas informações contidas no
  parâmetro passado.
  @param pSqlCommand  Objeto com as informações sobre o que e como executar a instrução.
  @return Retornar a quantidade de registros afetados pela atualização.
}
function TPersistenceEngine.Execute(const pConnection: IZConnection;
  const pSqlCommand: ISqlCommand): Integer;
var
  vReader: ITemplateReader;
  vSQL: string;
  vStatement: IZPreparedStatement;
begin
  // Carrega a SQL e extrai os parâmetros
  if not Assigned(pSqlCommand) then
    raise EInfraArgumentError.Create('pSqlCommand');

  vReader := GetReader;
  vSQL := vReader.Read(pSqlCommand.Name);
  vSQL := FParse.Parse(vSQL);

  // *** 1) Acho que os parâmetros macros de FParse devem ser substituidos aqui
  //   antes de chamar o PrepareStatementWithParams

  // Solicita um connection e prepara a SQL
  vStatement := pConnection.PrepareStatementWithParams(vSQL, FParse.GetParams);
  // Seta os parametros e executa
  SetParameters(vStatement, pSqlCommand);
  Result := vStatement.ExecuteUpdatePrepared;
end;

{**

  @param pStatement   ParameterDescription
  @param pSqlCommand   ParameterDescription
  @param pList   ParameterDescription
  @return ResultDescription
}
procedure TPersistenceEngine.DoLoad(const pStatement: IZPreparedStatement;
    const pSqlCommand: ISQLCommandQuery; const pList: IInfraList);
var
  vResultSet: IZResultSet;
  vObject: IInfraObject;
begin
  vResultSet := pStatement.ExecuteQueryPrepared;
  try
    while vResultSet.Next do
    begin
      vObject := GetRowFromResultSet(pSqlCommand, vResultSet);
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
{**

  @param pSqlCommand   ParameterDescription
  @param pList   ParameterDescription
  @return ResultDescription
}
procedure TPersistenceEngine.Load(const pSqlCommand: ISQLCommandQuery;
  const pList: IInfraList);
var
  vReader: ITemplateReader;
  vSQL: string;
  vStatement: IZPreparedStatement;
  vConnection: IZConnection;
begin
  if not Assigned(pSqlCommand) then
    raise EInfraArgumentError.Create('pSqlCommand');

  if not Assigned(pList) then
    raise EInfraArgumentError.Create('pList');

  vReader := GetReader;
  vSQL := vReader.Read(pSqlCommand.Name);
  // *** 1) se a SQL está vazia aqui deveria gerar exceção ou deveria ser dentro
  // do vReader.Read????
  vSQL := FParse.Parse(vSQL);
  // *** 2) Acho que os parâmetros macros de FParse devem ser substituidos aqui
  // antes de chamar o PrepareStatementWithParams
  vConnection := FConnnectionProvider.GetConnection;
  vStatement := vConnection.PrepareStatementWithParams(vSQL, FParse.GetParams);
  try
    SetParameters(vStatement, pSqlCommand);
    DoLoad(vStatement, pSqlCommand, pList);
  finally
    vStatement.Close;
  end;
end;

{**

  @return Retorna uma referencia ao ConnectionProvider
}
function TPersistenceEngine.GetConnectionProvider: IConnectionProvider;
begin
  Result := FConnnectionProvider;
end;

{**

  @param pConnection   ParameterDescription
  @return ResultDescription
}
procedure TPersistenceEngine.SetConnection(const pConnection: IZConnection);
begin
  if not Assigned(pConnection) then
    raise EInfraArgumentError.Create('pConnection');
    
  // TODO: preencher o connection provider com o pConnection
end;

{**

  @param pSqlCommand   ParameterDescription
  @param pResultSet   ParameterDescription
  @return ResultDescription
}
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
      vAttribute := Result.TypeInfo.GetProperty(Result, vAliasName) as IInfraType;
      if not Assigned(vAttribute) then
        Raise EPersistenceEngineError.CreateFmt(
          cErrorPersistenceEngineAttributeNotFound,
          [vAliasName, pResultSet.GetMetadata.GetColumnName(vIndex)]);
      if Supports(vAttribute.TypeInfo, IZTypeAnnotation, vZeosType) then
        vZeosType.NullSafeGet(pResultSet, vIndex, vAttribute)
      else
        raise EPersistenceEngineError.CreateFmt(
          cErrorPersistenceEngineCannotMapAttribute, [vAttribute.TypeInfo.Name]);
    end;
  end;
end;

// *** o que acontece caso tenhamos um template com nomes de parametros repetidos?
{**

  @param pStatement   ParameterDescription
  @param pSqlCommand   ParameterDescription
  @return ResultDescription
}
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
      raise EPersistenceEngineError.CreateFmt(
        cErrorPersistenceEngineParamNotFound, [vParams[vIndex]]);
  end;
end;

{ TInfraPersistenceService }

{**
  Permite acesso as configurações do Framework
  Chame GetConfiguration para obter uma interface de acesso aos parâmetros de
  configuração do framework.
  Ele assegura que seja retornado sempre a mesma instância.
  
  @return Retorna uma interface do tipo IConfiguration
*}
function TInfraPersistenceService.GetConfiguration: IConfiguration;
begin
  if not Assigned(FConfiguration) then
    FConfiguration := TConfiguration.Create;
  Result := FConfiguration;
end;

{**
  Permite acesso ao PersistenceEngine.
  Chame GetPersistenceEngine para obter uma interface de acesso ao PersistenceEngine.
  Ele assegura que seja retornado sempre a mesma instância.
  
  @return Retorna uma interface do tipo IPersistenceEngine
}
function TInfraPersistenceService.GetPersistenceEngine: IPersistenceEngine;
begin
  if not Assigned(FPersistenceEngine) then
    FPersistenceEngine := TPersistenceEngine.Create(FConfiguration,
      ConnectionProviderFactory.CreateProvider(FConfiguration));
  Result := FPersistenceEngine;
end;

{**
  Cria uma nova Session
  Chame OpenSession para criar uma nova instancia de Session.

  @return Retorna uma interface do tipo ISession
}
function TInfraPersistenceService.OpenSession: ISession;
begin
  Result := TSession.Create(GetPersistenceEngine);
end;

{**
  Permite setar um Connection ao PersistenceEngine
  Chame SetConnection se quiser facilitar a migração para o Infra.

  @param pConnection Qualquer objeto que implemente a interface IZConnection.
                     É a conexao com o banco de dados
}
procedure TInfraPersistenceService.SetConnection(
  const pConnection: IZConnection);
begin
  GetPersistenceEngine.SetConnection(pConnection);
end;

{ TTemplateReader }

{**
  Contructor de TTemplateReader. Por ser uma classe abstrata, não pode ser instanciada
}
constructor TTemplateReader.Create;
begin
  raise EPersistenceTemplateError.Create(cErrorTemplateTryCreateClassBase);
end;

{**

  @return ResultDescription
}
function TTemplateReader.GetConfiguration: IConfiguration;
begin
  Result := FConfiguration;
end;

{**

  @param Value   ParameterDescription
}
procedure TTemplateReader.SetConfiguration(const Value: IConfiguration);
begin
  FConfiguration := Value;
end;

{**
  Lê o template de um Stream
  @param pStream Stream do qual o Reader efetuará a leitura do Template
  @return Retorna o conteúdo do template no formato de string 
}
function TTemplateReader.ReadFromStream(const pStream: TStream): string;
begin
  pStream.Position := 0;
  SetLength(Result, pStream.Size);
  pStream.ReadBuffer(PChar(Result)^, pStream.Size);
end;

{ TParseParams }

///  Cria uma nova instância de TParseParams
constructor TParseParams.Create;
begin
  inherited;
  FParams := TStringList.Create;
  FMacroParams := TStringList.Create;
end;

///  Destrói o objeto
destructor TParseParams.Destroy;
begin
  FParams.Free;
  FMacroParams.Free;
  inherited;
end;

{**
  Parse analisa a instrução SQL à procura de parâmetros e macros.
  Procura por parâmetros no formato :<nome_param>
  e macros no formato #<nome_da_macro>. Os parâmetros encontrados são colocados
  numa lista e podem ser recuperados através da função GetParams.
  As macros encontradas são colocadas numa lista e podem ser recuperados através
  da função GetMacroParams.

  @param pSql instrução SQL que será analisada
}
function TParseParams.Parse(const pSQL: string): string;
const
  cExpRegCommentsML = '(\/\*(.*?)\*\/)'; // comentarios no formato /* ... */
  cExpRegCommentsInLine = '--(.*?)$'; // comentarios no formato -- ...
  cExpRegInvalidMacros = '##\w+'; // macros no formato ##<nome> são inválidas
  cExpRegInvalidParams = '::\w+'; // params no formato ::<nome> são inválidos
var
  vSql: string;
  vRegEx: TRegExpr;
begin
  FParams.Clear;
  FMacroParams.Clear;
  vRegEx := TRegExpr.Create;
  try
    // Elimina do texto tudo que deve ser ignorado: comentários,
    // parametros e macros inválidas
    vRegEx.Expression := cExpRegCommentsML+'|'+cExpRegInvalidMacros+'|'+
      cExpRegInvalidParams+'|'+cExpRegCommentsInline;
    vRegEx.ModifierM := True;
    vSql := vRegEx.Replace(pSQL, '', False)+' ';

    // Verifica se existe algum(a) param/macro sem nome
    vRegEx.Expression := '[:#]$|\s[:#]\s';
    if vRegEx.Exec (vSql) then
      raise EInfraParserError.Create('Parâmetro inválido');

    // Depois de remover do texto as partes a serem ignoradas,
    // procuramos por parametros e macros válidos
    vRegEx.Expression := '[\s\(=]:(\w+)[^w]|[\s\(=]#(\w+)[^w]|^#(\w+)[^w]';
    if vRegEx.Exec (vSql) then
    repeat
      if vRegEx.MatchPos[1] > 0 then
        FParams.Add (System.Copy(vSql, vRegEx.MatchPos[1], vRegEx.MatchLen[1]));
      if vRegEx.MatchPos[2] > 0 then
        FMacroParams.Add (System.Copy(vSql, vRegEx.MatchPos[2], vRegEx.MatchLen[2]));
      if vRegEx.MatchPos[3] > 0 then
        FMacroParams.Add (System.Copy(vSql, vRegEx.MatchPos[3], vRegEx.MatchLen[3]));
    until not vRegEx.ExecNext;

    vRegEx.Expression := ':(\w+)';
    Result := vRegEx.Replace(pSQL, '?', False);
  finally
    vRegEx.Free;
  end;
end;

{**
  Retorna um TStrings com a lista de macros
  
  @return Retorna um TStrings com a lista de parametros encontrados na instrução
    SQL durante o Parse
}
function TParseParams.GetMacroParams: TStrings;
begin
  Result := FMacroParams;
end;

{**
  Retorna um TStrings com a lista de parametros

  @return Retorna um TStrings com a lista de parametros encontrados na instrução
    SQL durante o Parse
}
function TParseParams.GetParams: TStrings;
begin
  Result := FParams;
end;

// Não entendi, mas se pôr direto no Initialization acontece Access Violations.
// ATENÇÃO: Vc não deve atribuir PersistenceService para uma variável de
// instancia nem global sem que no final da aplicação atribuia nil a ela explicitamente,
// sob pena de acontecer um AV no final da aplicação
procedure InjectPersistenceService;
begin
  (ApplicationContext as IBaseElement).Inject(
    IInfraPersistenceService, TInfraPersistenceService.Create);
end;

initialization
  InjectPersistenceService;
end.
