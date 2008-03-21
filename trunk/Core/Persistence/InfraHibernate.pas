unit InfraHibernate;

interface

{$I InfraPersistence.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  {TStrings} Classes,
  {Zeos} ZDbcIntfs,
  {Infra} InfraBase, InfraCommon, InfraCommonIntf, InfraValueTypeIntf,
  InfraHibernateIntf;

type
  TPersistenceService = class(TInterfacedObject, IPersistenceService)
  private
    FConfiguration: IConfiguration;
    function GetConfiguration: IConfiguration;
    function GetSessionFactory: ISessionFactory;
  public
    property Configuration: IConfiguration read GetConfiguration;
    property SessionFactory: ISessionFactory read GetSessionFactory;
  end;

  TPersistentClass = class(TBaseElement, IPersistentClass)
  private
    FEntityTypeInfo: IClassInfo;
    function GetEntityName: string;
    function GetColumnName(const pPropertyInfo: IPropertyInfo): string;
    procedure CheckEntityTypeInfo;
    function GetEntityTypeInfo: IClassInfo;
    procedure SetEntityTypeInfo(const Value: IClassInfo);
    function IsIdentifierColumn(const pPropertyInfo: IPropertyInfo): boolean;
  public
    property EntityTypeInfo: IClassInfo read GetEntityTypeInfo
      write SetEntityTypeInfo;
  end;

  TConfiguration = class(TBaseElement, IConfiguration)
  private
    FProperties: TStrings;
    function GetProperties: TStrings;
    function GetPropertyItem(const pName: String): string;
    procedure SetPropertyItem(const pName: String; const Value: string);
    procedure BindClasses;
    function BuildSessionFactory: ISessionFactory;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Properties: TStrings read GetProperties;
    property PropertyItem[const pName: String]: string read GetPropertyItem
      write SetPropertyItem;
  end;

  TConnectionProvider = class(TBaseElement, IConnectionProvider)
  private
    FConfiguration: IConfiguration;
    procedure Close;
    procedure CloseConnection(const pConnection: IZConnection);
    procedure Configure;
    function GetConnection: IZConnection;
  public
    constructor Create(const pConfig: IConfiguration); reintroduce;
  end;

  TSessionFactory = class(TBaseElement, ISessionFactory)
  private
    FConfiguration: IConfiguration;
    FConnectionProvider: IConnectionProvider;
    FDialect: IDialect;
    FEntityPersisters: IEntityPersisters;
    function GetConnectionProvider: IConnectionProvider;
    function GetDialect: IDialect;
    function OpenSession: ISession; overload;
    function OpenSession(const pConnection: IZConnection): ISession; overload;
    function GetEntityPersister(const pEntityID: TGUID): IEntityPersister;
  public
    constructor Create(pConfig: IConfiguration); reintroduce;
    property ConnectionProvider: IConnectionProvider read GetConnectionProvider;
    property Dialect: IDialect read GetDialect;
  end;

  TSession = class(TBaseElement, ISession, IInternalSession)
  private
    FConnection: IZConnection;
    FSessionFactory: ISessionFactory;
    function FireLoad(const pEntityID: TGUID; const pOID: IInfraType;
      const pInstanceToLoad: IInfraType = nil): IInfraType;
  protected
    // ISession
    function GetConnection: IZConnection;
    function GetSessionFactory: ISessionFactory;
    function Load(const pEntityID: TGUID;
      const pOID: IInfraType): IInfraType; overload;
    function Load(const pInstanceToLoad: IInfraType;
      const pOID: IInfraType): IInfraType; overload;
    procedure SetConnection(const Value: IZConnection);
    function CreateCriteria(const pTypeID: TGUID): ICriteria; overload;
    function CreateCriteria(const pTypeInfo: IClassInfo): ICriteria; overload;
    property Connection: IZConnection read GetConnection write SetConnection;
    property SessionFactory: ISessionFactory read GetSessionFactory;
    // IInternalSession
    function List(const Criteria: ICriteria): IInfraList;
  public
    constructor Create(pSessionFactory: ISessionFactory); reintroduce;
  end;

  TEntityLoader = class(TBaseElement, IEntityLoader)
  private
    FEntityPersister: IEntityPersister;
    function DoQuery(const pSession: ISession; const pOID,
      pOptionalInstance: IInfraType): IInfraList;
    function GetRowFromResultSet(const pOID: IInfraType;
      const pOptionalInstance: IInfraType;
      const pResultSet: IZResultSet): IInfraType;
    function Load(const pOID: IInfraType; const pOptionalInstance: IInfraType;
      const pSession: ISession): IInfraType;
    procedure InitializeEntity(const pObject: IInfraType;
      const pResultSet: IZResultSet);
    procedure SetPropertyFromResultSet(const pAttribute: IInfraType;
      const pResultSet: IZResultSet; pIndex: Integer);
    procedure SetParameters(const pStatement: IZPreparedStatement;
      const pParamTypes: IMemberInfoList; const pParamValues: IInfraType);
  public
    constructor Create(const pPersister: IEntityPersister); Reintroduce;
  end;

  TEntityPersister = class(TBaseElement, IEntityPersister)
  private
    FPersistentClass: IPersistentClass;
    FIdentifierColumns: IMemberInfoList;
    FColumns: IMemberInfoList;
    FAllColumns: IMemberInfoList;
    FEntityLoader: IEntityLoader;
    FSessionFactory: ISessionFactory;
    FSQLSnapshotSelectString: string;
    function GetPersistentClass: IPersistentClass;
    function GetSQLSnapshotSelectString: string;
    function GetColumnName(const pPropertyInfo: IPropertyInfo): string;
    function ConcretePropertySelectFragment: string;
    procedure LoadColumns;
    function getIdentifierColumnNames: TStrings;
    function GenerateSnapshotSelectString: string;
    function GetAllColumns: IMemberInfoList;
    function GetIdentifierColumns: IMemberInfoList;
  protected
    function Load(const pOID: IInfraType; const pOptionalInstance: IInfraType;
      const pSession: ISession): IInfraType;
    function Instantiate(const pOID: IInfraType): IInfraType;
    property PersistentClass: IPersistentClass read GetPersistentClass;
    property SQLSnapshotSelectString: string read GetSQLSnapshotSelectString;
  public
    constructor Create(const pEntityInfo: IClassInfo;
      const pSessionFactory: ISessionFactory); reintroduce;
  end;

implementation

uses
  SysUtils, List_MemberInfo, InfraHibernateAnnotationIntf, List_EntityPersister, InfraConsts,
  List_Criterion, InfraValueType, InfraCriteria, InfraSQLBuilder;

// *** acho que nao precisamos desta classe
type
  TConnectionStringBuilder = class
    class function BuildConnectionString(
      const pConfiguration: IConfiguration): String;
  end;

{ TPersistenceService }

function TPersistenceService.GetConfiguration: IConfiguration;
begin
  if not Assigned(FConfiguration) then
    FConfiguration := TConfiguration.Create;
  Result := FConfiguration;
end;

function TPersistenceService.GetSessionFactory: ISessionFactory;
begin
  if not Assigned(FConfiguration) then
    raise Exception.Create('Configuration is empty');
  Result := FConfiguration.BuildSessionFactory;
end;

{ TConnectionStringBuilder }

class function TConnectionStringBuilder.BuildConnectionString(
  const pConfiguration: IConfiguration): String;
begin
  Result :=
    'zdbc:' + pConfiguration.PropertyItem['protocol'] +
    '://' + pConfiguration.PropertyItem['hostname'] +
    '/' + pConfiguration.PropertyItem['database'] +
    '?username=' + pConfiguration.PropertyItem['username'] +
    ';password=' + pConfiguration.PropertyItem['password'];
end;

{ TConfiguration }

constructor TConfiguration.Create;
begin
  inherited Create;
  FProperties := TStringList.Create;
end;

destructor TConfiguration.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
end;

function TConfiguration.GetProperties: TStrings;
begin
  Result := FProperties;
end;

function TConfiguration.GetPropertyItem(const pName: String): string;
begin
  Result := FProperties.Values[pName]
end;

procedure TConfiguration.SetPropertyItem(const pName: String; const Value:
  string);
begin
  FProperties.Values[pName] := Value;
end;

function TConfiguration.BuildSessionFactory: ISessionFactory;
begin
  BindClasses;
  Result := TSessionFactory.Create(Self);
end;

procedure TConfiguration.BindClasses;
begin
  // criar todos os PersistentClass aqui a partir da reflexão de classes
  // que estiverem anotados com IEntity
end;

{ TConnectionProvider }

constructor TConnectionProvider.Create(const pConfig: IConfiguration);
begin
  inherited Create;
  FConfiguration := pConfig;
  Configure;
  // quando tiver pool:
  // - cria o pool de conexões;
  // - guarda o classinfo de connection a ser instanciada quando chamar
  //   GetConnection. com base no ConnectionString e ConnectionStringName;
end;

procedure TConnectionProvider.Close;
begin
  // quando tiver pool:
  // - remove todas as conexões do pool
end;

procedure TConnectionProvider.CloseConnection(const pConnection: IZConnection);
begin
  // quando tiver pool:
  // - procura a conexão na lista e remove o mesmo
  pConnection.Close;
end;

procedure TConnectionProvider.Configure;
begin
  // quando tiver pool:
  // - Guarda em IDriver uma instância do driver a ser usado pelo Connection
  // - Este driver é criado com base na reflexão pela string definida na chave
  //   Environment.ConnectionDriver
end;

function TConnectionProvider.GetConnection: IZConnection;
begin
  // quando tiver pool:
  // - verifica se há alguma conexão livre na lista trava e retorna esta conexão
  //   senao cria uma e a retorna.
  Result := DriverManager.GetConnection(
    TConnectionStringBuilder.BuildConnectionString(FConfiguration));
end;

{ TSessionFactory }

constructor TSessionFactory.Create(pConfig: IConfiguration);
begin
  inherited Create;
  FConfiguration := pConfig;
  FConnectionProvider := TConnectionProvider.Create(pConfig);
  FEntityPersisters := TEntityPersisters.Create;
end;

function TSessionFactory.GetConnectionProvider: IConnectionProvider;
begin
  Result := FConnectionProvider;
end;

function TSessionFactory.GetDialect: IDialect;
{
var
  vEntityTypeInfo: IClassInfo;
}
begin
  Result := nil;
  {
  // *** Por enquanto vamos retornar nil no Dialect, depois teria de
  // *** descomentar o codigo abaixo.
  vEntityTypeInfo := nil;
  if not Assigned(FDialect) then
    with TypeService do
    begin
      vEntityTypeInfo := GetType(FConfiguration.PropertyItem['Dialect'], True);
      FDialect := CreateInstance(vEntityTypeInfo) as IDialect;
    end;
  Result := FDialect;
  }
end;

function TSessionFactory.OpenSession: ISession;
var
  vConnection: IZConnection;
begin
  vConnection := ConnectionProvider.GetConnection;
  Result := OpenSession(vConnection);
end;

// ### No hibernate:
// Todos os EntityPersister são criados e adicionados em uma matriz no
// construtor desta classe.
function TSessionFactory.GetEntityPersister(
  const pEntityID: TGUID): IEntityPersister;
begin
  Result := FEntityPersisters[pEntityID];
  if not Assigned(Result) then
  begin
    // *** Criar as subclasses de entitypersister e carregar o apropriado
    // *** com base no mapeamento. Será criado um dos Entitypersisters abaixo:
    // *** SingleTableEntityPersister (padrão), UnionSubclassEntityPersister,
    // *** ou JoinedSubclassEntityPersister.
    Result := TEntityPersister.Create(
      TypeService.GetType(pEntityID, True), Self);
    FEntityPersisters.Add(pEntityID, Result);
  end;
end;

function TSessionFactory.OpenSession(const pConnection: IZConnection): ISession;
begin
  Result := TSession.Create(Self);
  Result.Connection := pConnection;
end;

{ TSession }

constructor TSession.Create(pSessionFactory: ISessionFactory);
begin
  inherited Create;
  FSessionFactory := pSessionFactory;
end;

function TSession.GetConnection: IZConnection;
begin
  Result := FConnection;
end;

function TSession.GetSessionFactory: ISessionFactory;
begin
  Result := FSessionFactory;
end;

function TSession.Load(const pEntityID: TGUID;
  const pOID: IInfraType): IInfraType;
begin
  Result := FireLoad(pEntityID, pOID);
end;

function TSession.Load(const pInstanceToLoad: IInfraType;
  const pOID: IInfraType): IInfraType;
begin
  Result := FireLoad(NullGUID, pOid, pInstanceToLoad);
end;

function TSession.FireLoad(const pEntityID: TGUID; const pOID: IInfraType;
  const pInstanceToLoad: IInfraType = nil): IInfraType;
var
  vEntityPersister: IEntityPersister;
begin
  with FSessionFactory do
    if Assigned(pInstanceToLoad) then
      vEntityPersister := GetEntityPersister(pInstanceToLoad.TypeInfo.TypeID)
    else
      vEntityPersister := GetEntityPersister(pEntityID);
  if not Assigned(vEntityPersister) then
    Raise Exception.Create('EntityPersister to '+GUIDToString(pEntityID)+
      'not found!');
  // ### No hibernate:
  // - Verificar se o Oid passado é compatível com o tipo de OID do
  //   EntityPersister;
  // - Prepara um objeto EntityKey com informações sobre o OID e passa
  //   para DoLoad;
  // - Há uma tratamento de Proxy;
  // - Se pInstanceToLoad já está associado a este session gera exceção;
  // - Primeiro procura a instância nos caches e se nao achar em nenhum
  //   carrega do Datasource. Este chama o Load do EntityPersister;
  Result := vEntityPersister.Load(pOID, pInstanceToLoad, Self);
end;

procedure TSession.SetConnection(const Value: IZConnection);
begin
  FConnection := Value;
end;

function TSession.CreateCriteria(const pTypeID: TGUID): ICriteria;
begin
  Result := CreateCriteria(TypeService.GetType(pTypeID, True));
end;

function TSession.CreateCriteria(const pTypeInfo: IClassInfo): ICriteria;
begin
  Result := TCriteria.Create(pTypeInfo, Self);
end;

function TSession.List(const Criteria: ICriteria): IInfraList;
begin

end;

{ TEntityLoader }

constructor TEntityLoader.Create(const pPersister: IEntityPersister);
begin
  inherited Create;
  // O EntityPersister Aponta para este Loader logo este tem de manter uma
  // referencia fraca ao seu EntityPersister.
  SetReference(IInterface(FEntityPersister), pPersister);
end;

function TEntityLoader.DoQuery(const pSession: ISession; const pOID: IInfraType;
  const pOptionalInstance: IInfraType): IInfraList;
var
  vSQL: string;
  vST: IZPreparedStatement;
  vRS: IZResultSet;
  vObject: IInfraType;
begin
  // ### No hibernate:
  // Acontece um laço em uma matriz chamada EntityPersisters. Esta
  // matriz contem todos os persisters que precisam ser carregados
  // para casos como herança ou esta classe possuir relacionamentos.
  vSQL := FEntityPersister.SQLSnapshotSelectString;
  vST := pSession.Connection.PrepareStatement(vSQL);
  SetParameters(vST, FEntityPersister.GetIdentifierColumns, pOID);
  vRS := vST.ExecuteQueryPrepared;
  Result := TInfraList.Create;
  try
    while vRS.Next do
    begin
      vObject := GetRowFromResultSet(pOID, pOptionalInstance, vRS);
      Result.Add(vObject);
    end;
  finally
    vRs.Close;
    vST.Close;
  end;
end;

function TEntityLoader.GetRowFromResultSet(const pOID: IInfraType;
  const pOptionalInstance: IInfraType;
  const pResultSet: IZResultSet): IInfraType;
begin
  // ### No Hibernate:
  // Aqui acontece um laço que preenche uma matriz de EntityKeys (Keys)
  // ersta matriz é passada para uma função que cria os objetos caso
  // pOptionalInstance = nil e então já preenche o oid destes objetos
  // com o conteudo de Keys
  if not Assigned(pOptionalInstance) then
    Result := FEntityPersister.Instantiate(pOID)
  else
    Result := pOptionalInstance;
  InitializeEntity(Result, pResultSet);
end;

procedure TEntityLoader.InitializeEntity(const pObject: IInfraType;
  const pResultSet: IZResultSet);
var
  vPropertyInfo: IPropertyInfo;
  vI: integer;
  vObj: IInfraObject;
  vAttribute: IInfraType;
begin
  if Supports(pObject, IInfraObject, vObj) then
  begin
    for vI := 0 to FEntityPersister.GetAllColumns.Count-1 do
    begin
      vPropertyInfo := FEntityPersister.GetAllColumns[vI] as IPropertyInfo;
      vAttribute := vPropertyInfo.GetValue(pObject) as IInfraType;
      SetPropertyFromResultSet(vAttribute, pResultSet, vI);
    end;
  end;
end;

procedure TEntityLoader.SetParameters(const pStatement: IZPreparedStatement;
  const pParamTypes: IMemberInfoList; const pParamValues: IInfraType);
var
  vIndex: integer;
  vTypeInfo: IClassInfo;
  vParamValue: IInfraType;
begin
  for vIndex := 0 to pParamTypes.Count-1 do
  begin
    vTypeInfo := (pParamTypes[vIndex] as IPropertyInfo).TypeInfo;
    // *** tem de tratar o InfraType aqui se for um Object
    if Supports(pParamValues, IInfraList) then
      vParamValue := (pParamValues as IInfraList).Items[vIndex]
    else
      vParamValue := pParamValues;
    (vTypeInfo as IZTypeAnnotation).NullSafeSet(pStatement, vIndex,
      vParamValue);
  end;
end;

procedure TEntityLoader.SetPropertyFromResultSet(const pAttribute: IInfraType;
  const pResultSet: IZResultSet; pIndex: Integer);
begin
  // *** tem de tratar o InfraType aqui se for um Object ou InfraList
  (pAttribute.TypeInfo as IZTypeAnnotation).NullSafeGet(
    pResultSet, pIndex, pAttribute)
end;

function TEntityLoader.Load(const pOID: IInfraType;
  const pOptionalInstance: IInfraType; const pSession: ISession): IInfraType;
var
  vList: IInfraList;
begin
  vList := DoQuery(pSession, pOID, pOptionalInstance);
  case vList.Count of
    0: Result := nil;
    1: Result := vList[0];
  else
    Raise Exception.Create('Mais de uma linha foi encontrada'#13+
      ' para o mesmo identificador');
  end;
end;

{ TPersistentClass }

procedure TPersistentClass.CheckEntityTypeInfo;
begin
  if not Assigned(FEntityTypeInfo) then
    Raise Exception.Create('Classinfo should be filled.');
end;

function TPersistentClass.GetEntityTypeInfo: IClassInfo;
begin
  Result := FEntityTypeInfo;
end;

// Retorna o nome da coluna ou o nome da propriedade (se nao houve anotação)
function TPersistentClass.GetColumnName(
  const pPropertyInfo: IPropertyInfo): string;
var
  vColumn: IColumn;
begin
  if Supports(pPropertyInfo, IColumn, vColumn) then
    Result := vColumn.Name
  else
    Result := pPropertyInfo.Name;
end;

// Retorna o nome da tabela ou o nome da classe (se nao houve anotação)
function TPersistentClass.GetEntityName: string;
var
  vEntity: IEntity;
begin
  CheckEntityTypeInfo;
  if Supports(FEntityTypeInfo, IEntity, vEntity) then
    Result := vEntity.Name
  else
    Result := FEntityTypeInfo.Name;
end;

// Verifica se a propriedade faz parte da chave primaria.
function TPersistentClass.IsIdentifierColumn(
  const pPropertyInfo: IPropertyInfo): boolean;
begin
  Result := Supports(pPropertyInfo, IID);
end;

procedure TPersistentClass.SetEntityTypeInfo(const Value: IClassInfo);
begin
  FEntityTypeInfo := Value;
end;

{ TEntityPersister }

constructor TEntityPersister.Create(const pEntityInfo: IClassInfo;
  const pSessionFactory: ISessionFactory);
begin
  inherited Create;
  FPersistentClass := TPersistentClass.Create;
  FPersistentClass.EntityTypeInfo := pEntityInfo;
  FEntityLoader := TEntityLoader.Create(Self);
  // O SessionFactory mantem a lista de EntityPersisters logo este
  // EntityPersister tem de manter uma referencia fraca ao SessionFactory
  SetReference(IInterface(FSessionFactory), pSessionFactory);
  LoadColumns;
end;

procedure TEntityPersister.LoadColumns;
var
  vPropIterator: IPropertyInfoIterator;
begin
  FColumns := TMemberInfoList.Create;
  FIdentifierColumns := TMemberInfoList.Create;
  FAllColumns := TMemberInfoList.Create;
  vPropIterator := FPersistentClass.EntityTypeInfo.GetProperties;
  while not vPropIterator.IsDone do
  begin
    if FPersistentClass.IsIdentifierColumn(vPropIterator.CurrentItem) then
      FIdentifierColumns.Add(vPropIterator.CurrentItem)
    else
      FColumns.Add(vPropIterator.CurrentItem);
    FAllColumns.Add(vPropIterator.CurrentItem);
    vPropIterator.Next;
  end;
end;

function TEntityPersister.GetColumnName(
  const pPropertyInfo: IPropertyInfo): string;
begin
  Result := FPersistentClass.GetColumnName(pPropertyInfo);
end;

function TEntityPersister.GetPersistentClass: IPersistentClass;
begin
  Result := FPersistentClass;
end;

function TEntityPersister.GetSQLSnapshotSelectString: string;
begin
  if (FSQLSnapshotSelectString = EmptyStr) then
    FSQLSnapshotSelectString := GenerateSnapshotSelectString;
  Result := FSQLSnapshotSelectString;
end;

function TEntityPersister.GenerateSnapshotSelectString: string;
var
  vSelect: ISelectBuilder;
  vSelectClause, vFromClause, vWhereClause: string;
  vIdColumns: TStrings;
begin
  vSelect := TSelectBuilder.Create(FSessionFactory.Dialect);
  with vSelect do
  begin
    vIdColumns := getIdentifierColumnNames;
    try
      vSelectClause := TStringHelper.Join(',', vIdColumns) + ',' +
        ConcretePropertySelectFragment;
      vFromClause := FPersistentClass.GetEntityName;
      vWhereClause := TStringHelper.Join('=? and ', vIdColumns) + '=?';
    finally
      vIdColumns.Free;
    end;
    SetSelectClause(vSelectClause);
    SetFromClause(vFromClause);
    SetOuterJoins(EmptyStr, EmptyStr);
    SetWhereClause(vWhereClause);
    Result := ToStatementString;
  end;
end;

function TEntityPersister.getIdentifierColumnNames: TStrings;
var
  i: integer;
  vColumn: IPropertyInfo;
begin
  Result := TStringList.Create;
  for i := 0 to FIdentifierColumns.Count-1 do
  begin
    vColumn := FIdentifierColumns[i] as IPropertyInfo;
    Result.Add(FPersistentClass.GetColumnName(vColumn));
  end;
end;

function TEntityPersister.ConcretePropertySelectFragment: string;
var
  vColumnNames: TStrings;
  i: integer;
begin
  vColumnNames := TStringList.Create;
  try
    for i := 0 to FColumns.Count-1 do
      vColumnNames.Add(FPersistentClass.GetColumnName(
        FColumns[i] as IPropertyInfo));
    Result := TStringHelper.Join(',', vColumnNames);
  finally
    vColumnNames.Free;
  end;
end;

function TEntityPersister.Instantiate(const pOID: IInfraType): IInfraType;
begin
  Result := TypeService.CreateInstance(
    FPersistentClass.EntityTypeInfo) as IInfraType;
  // *** Tem de preencher o oid do objeto já aqui. no hibernate usar o setter
end;

function TEntityPersister.Load(const pOID: IInfraType;
  const pOptionalInstance: IInfraType; const pSession: ISession): IInfraType;
begin
  // ### No hibernate:
  // - Pega o EntityLoader apropriado com base no lockmode
  Result := FEntityLoader.Load(pOID, pOptionalInstance, pSession);
end;

// Injeta o mecanismo de persistencia no Applicationcontext.
procedure InjectPersitenceService;
begin
  (ApplicationContext as IBaseElement).Inject(
    IPersistenceService, TPersistenceService.Create);
end;

function TEntityPersister.GetAllColumns: IMemberInfoList;
begin
  Result := FAllColumns;
end;

function TEntityPersister.GetIdentifierColumns: IMemberInfoList;
begin
  Result := FIdentifierColumns;
end;

initialization
  InjectPersitenceService;

end.
