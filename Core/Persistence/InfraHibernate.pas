unit InfraHibernate;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  {TStrings}
  Classes,
  {Infra}
  InfraCommon, InfraCommonIntf, InfraValueTypeIntf,
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

  THibPersistentClass = class(TMemoryManagedObject, IHibPersistentClass)
  private
    FEntityTypeInfo: IClassInfo;
    function GetEntityName: string;
    // *** function GetEntityPersister: IEntityPersister;
    function GetOIDColumnName: string;
    function GetColumnName(const pPropertyInfo: IPropertyInfo): string;
    procedure CheckEntityTypeInfo;
    function GetEntityTypeInfo: IClassInfo;
    procedure SetEntityTypeInfo(const Value: IClassInfo);
  public
    property EntityTypeInfo: IClassInfo read GetEntityTypeInfo write SetEntityTypeInfo;
  end;

  TConfiguration = class(TElement, IConfiguration)
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

  TConnectionProvider = class(TElement, IConnectionProvider)
  private
    FConfiguration: IConfiguration;
    procedure Close;
    procedure CloseConnection(const pConnection: IConnection);
    procedure Configure;
    function GetConnection: IConnection;
  public
    constructor Create(const pConfig: IConfiguration); reintroduce;
  end;

  TSessionFactory = class(TElement, ISessionFactory)
  private
    FConfiguration: IConfiguration;
    FConnectionProvider: IConnectionProvider;
    FDialect: IDialect;
    function GetConnectionProvider: IConnectionProvider;
    function GetDialect: IDialect;
    function OpenSession: ISession; overload;
    function OpenSession(const pConnection: IConnection): ISession; overload;
  public
    constructor Create(pConfig: IConfiguration); reintroduce;
    property ConnectionProvider: IConnectionProvider read GetConnectionProvider;
    property Dialect: IDialect read GetDialect;
  end;

  TSession = class(TElement, ISession)
  private
    FConnection: IConnection;
    FSessionFactory: ISessionFactory;
    function GetConnection: IConnection;
    function GetSessionFactory: ISessionFactory;
    function Load(const pTypeID: TGUID; const pOID: IInfraType): IInfraType;
    procedure SetConnection(const Value: IConnection);
  public
    constructor Create(pSessionFactory: ISessionFactory); reintroduce;
    property Connection: IConnection read GetConnection write SetConnection;
    property SessionFactory: ISessionFactory read GetSessionFactory;
  end;

  TLoader = class(TElement, ILoader)
  private
    FPersistentClass: IHibPersistentClass;
    function FillObject(const pObject: IInfraType;
      const vResultSet: IResultSet): string;
    function GetSelectClause(const pEntityTypeInfo: IClassInfo;
      const pOID: IInfraType): string;
    function Load(const pTypeID: TGUID; const pSession: ISession;
      const pOID: IInfraType): IInfraType;
  public
    constructor Create; override;
  end;

implementation

uses
  SysUtils, MapperAnnotationIntf;

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
  // criar todos os HibPersistentClass aqui a partir da reflexão de classes
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

procedure TConnectionProvider.CloseConnection(const pConnection: IConnection);
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

function TConnectionProvider.GetConnection: IConnection;
var
  vClassInfo: IClassInfo;
begin
  // quando tiver pool:
  // - verifica se há alguma conexão livre na lista trava e retorna esta conexão
  //   senao cria uma e a retorna.
  with TypeService do
  begin
    vClassInfo := GetType(
      FConfiguration.PropertyItem['ConnectionClass'], True);
    Result := CreateInstance(vClassInfo) as IConnection;
  end;
end;

{ TSessionFactory }

constructor TSessionFactory.Create(pConfig: IConfiguration);
begin
  inherited Create;
  FConfiguration := pConfig;
  FConnectionProvider := TConnectionProvider.Create(pConfig);
end;

function TSessionFactory.GetConnectionProvider: IConnectionProvider;
begin
  Result := FConnectionProvider;
end;

function TSessionFactory.GetDialect: IDialect;
var
  vEntityTypeInfo: IClassInfo;
begin
  vEntityTypeInfo := nil;
  if not Assigned(FDialect) then
    with TypeService do
    begin
      vEntityTypeInfo := GetType(FConfiguration.PropertyItem['Dialect'], True);
      FDialect := CreateInstance(vEntityTypeInfo) as IDialect;
    end;
  Result := FDialect;
end;

function TSessionFactory.OpenSession: ISession;
var
  vConnection: IConnection;
begin
  vConnection := ConnectionProvider.GetConnection;
  Result := OpenSession(vConnection);
end;

function TSessionFactory.OpenSession(const pConnection: IConnection): ISession;
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

function TSession.GetConnection: IConnection;
begin
  Result := FConnection;
end;

function TSession.GetSessionFactory: ISessionFactory;
begin
  Result := FSessionFactory;
end;

function TSession.Load(const pTypeID: TGUID; const pOID: IInfraType): IInfraType;
var
  vLoader: ILoader;
begin
  vLoader := TLoader.Create;
  Result := vLoader.Load(pTypeID, Self, pOID);
end;

procedure TSession.SetConnection(const Value: IConnection);
begin
  FConnection := Value;
end;

{ TLoader }

function TLoader.Load(const pTypeID: TGUID; const pSession: ISession;
  const pOID: IInfraType): IInfraType;
var
  vEntityTypeInfo: IClassInfo;
  sSelectClause: string;
  vResultSet: IResultSet;
begin
  with TypeService do
  begin
    vEntityTypeInfo := GetType(pTypeID, True);
    Result := CreateInstance(vEntityTypeInfo) as IInfraType;
  end;
  FPersistentClass.EntityTypeInfo := vEntityTypeInfo;
  sSelectClause := GetSelectClause(vEntityTypeInfo, pOID);
  vResultSet := pSession.Connection.ExecuteQuery(sSelectClause);
  FillObject(Result, vResultSet);
end;

// gera uma instrução SQL de seleção simples
function TLoader.GetSelectClause(const pEntityTypeInfo: IClassInfo;
  const pOID: IInfraType): string;
begin
  Result :=
    ' SELECT *' +
    ' FROM ' + FPersistentClass.GetEntityName +
    ' WHERE ' +
    FPersistentClass.GetOIDColumnName + ' = ' +
    IntToStr((pOID as IInfraInteger).AsInteger);
end;

// Preenche as propriedades do objeto com base nos valores do registro atual
// do resultset.
function TLoader.FillObject(const pObject: IInfraType;
  const vResultSet: IResultSet): string;
var
  vPropIterator: IPropertyInfoIterator;
  vPropertyInfo: IPropertyInfo;
  vObj: IInfraObject;
  vAttribute: IInfraType;
begin
  if Supports(pObject, IInfraObject, vObj) then
  begin
    vPropIterator := vObj.TypeInfo.GetProperties;
    while not vPropIterator.IsDone do
    begin
      vPropertyInfo := vPropIterator.CurrentItem;
      vAttribute := vPropertyInfo.GetValue(pObject) as IInfraType;
      vAttribute.Assign(vResultSet.GetValue(
        FPersistentClass.GetColumnName(vPropertyInfo)));
      vPropIterator.Next;
    end;
  end;
end;

constructor TLoader.Create;
begin
  inherited Create;
  FPersistentClass := THibPersistentClass.Create;
end;

{ THibPersistentClass }

procedure THibPersistentClass.CheckEntityTypeInfo;
begin
  if not Assigned(FEntityTypeInfo) then
    Raise Exception.Create('Classinfo should be filled.');
end;

function THibPersistentClass.GetEntityTypeInfo: IClassInfo;
begin
  Result := FEntityTypeInfo;
end;

// Retorna o nome da coluna, se nao houve anotação retorna por padrão
// o nome da propriedade.
function THibPersistentClass.GetColumnName(
  const pPropertyInfo: IPropertyInfo): string;
var
  vColumn: IColumn;
begin
  if Supports(pPropertyInfo, IColumn, vColumn) then
    Result := vColumn.Name
  else
    Result := pPropertyInfo.Name;
end;

// Retorna o nome da tabela, se nao houve anotação retorna o nome da classe por
// padrão.
function THibPersistentClass.GetEntityName: string;
var
  vEntity: IEntity;
begin
  CheckEntityTypeInfo;
  if Supports(FEntityTypeInfo, IEntity, vEntity) then
    Result := vEntity.Name
  else
    Result := FEntityTypeInfo.Name;
end;

// Retorna o nome da coluna que é chave primaria com base na propriedade anotada
// como identificador.
function THibPersistentClass.GetOIDColumnName: string;
var
  vPropIterator: IPropertyInfoIterator;
  vPropertyInfo: IPropertyInfo;
begin
  CheckEntityTypeInfo;
  vPropIterator := FEntityTypeInfo.GetProperties;
  while not vPropIterator.IsDone do
  begin
    vPropertyInfo := vPropIterator.CurrentItem;
    if Supports(vPropertyInfo, IID) then
    begin
      Result := GetColumnName(vPropertyInfo);
      Break;
    end;
    vPropIterator.Next;
  end;
end;

procedure THibPersistentClass.SetEntityTypeInfo(const Value: IClassInfo);
begin
  FEntityTypeInfo := Value;
end;

// *** function THibPersistentClass.GetEntityPersister: IEntityPersister;
//begin
//
//end;

// Injeta o mecanismo de persistencia no Applicationcontext.
procedure InjectPersitenceService;
begin
  (ApplicationContext as IMemoryManagedObject).Inject(
    IPersistenceService, TPersistenceService.Create);
end;

initialization
  InjectPersitenceService;

end.
