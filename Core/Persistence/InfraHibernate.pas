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
    FSessionFactory: ISessionFactory;
    function GetConfiguration: IConfiguration;
    function GetSessionFactory: ISessionFactory;
  public
    property Configuration: IConfiguration read GetConfiguration;
    property SessionFactory: ISessionFactory read GetSessionFactory;
  end;

  TConfiguration = class(TElement, IConfiguration)
  private
    FProperties: TStrings;
    function GetProperties: TStrings;
    function GetPropertyItem(const pName: String): string;
    procedure SetPropertyItem(const pName: String; const Value: string);
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
    function FillObject(const pObject: IInfraType;
      const vResultSet: IResultSet): string;
    function GetEntityName(const pClassInfo: IClassInfo): string;
    function GetOIDColumnName(const pClassInfo: IClassInfo): string;
    function GetColumnName(const pPropertyInfo: IPropertyInfo): string;
    function GetSelectClause(const pClassInfo: IClassInfo;
      const pOID: IInfraType): string;
    function Load(const pTypeID: TGUID; const pSession: ISession;
      const pOID: IInfraType): IInfraType;
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
  if not Assigned(FSessionFactory) then
    FSessionFactory := TSessionFactory.Create(Configuration);
  Result := FSessionFactory;
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
  vTypeInfo: IClassInfo;
begin
  vTypeInfo := nil;
  if not Assigned(FDialect) then
    with TypeService do
    begin
      vTypeInfo := GetType(FConfiguration.PropertyItem['Dialect'], True);
      FDialect := CreateInstance(vTypeInfo) as IDialect;
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
  vClassInfo: IClassInfo;
  sSelectClause: string;
  vResultSet: IResultSet;
begin
  with TypeService do
  begin
    vClassInfo := GetType(pTypeID, True);
    Result := CreateInstance(vClassInfo) as IInfraType;
  end;
  sSelectClause := GetSelectClause(vClassInfo, pOID);
  vResultSet := pSession.Connection.ExecuteQuery(sSelectClause);
  FillObject(Result, vResultSet);
end;

function TLoader.GetEntityName(const pClassInfo: IClassInfo): string;
var
  vEntity: IEntity;
begin
  if Supports(pClassInfo, IEntity, vEntity) then
    Result := vEntity.Name
  else
    Result := pClassInfo.Name;
end;

function TLoader.GetColumnName(const pPropertyInfo: IPropertyInfo): string;
var
  vColumn: IColumn;
begin
  if Supports(pPropertyInfo, IColumn, vColumn) then
    Result := vColumn.Name
  else
    Result := pPropertyInfo.Name;
end;

function TLoader.GetOIDColumnName(const pClassInfo: IClassInfo): string;
var
  vPropIterator: IPropertyInfoIterator;
  vPropertyInfo: IPropertyInfo;
begin
  vPropIterator := pClassInfo.GetProperties;
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

function TLoader.GetSelectClause(const pClassInfo: IClassInfo;
  const pOID: IInfraType): string;
begin
  Result :=
    ' SELECT *' +
    ' FROM ' + GetEntityName(pClassInfo) +
    ' WHERE ' +
      GetOIDColumnName(pClassInfo) + ' = ' +
      IntToStr((pOID as IInfraInteger).AsInteger);
end;

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
      vAttribute.Assign(
        vResultSet.GetValue( GetColumnName(vPropertyInfo) ) );
      vPropIterator.Next;
    end;
  end;
end;

procedure InjectPersitenceService;
begin
  (ApplicationContext as IMemoryManagedObject).Inject(
    IPersistenceService, TPersistenceService.Create as IPersistenceService);
end;

initialization
  InjectPersitenceService;

end.
