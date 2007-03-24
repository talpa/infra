unit InfraHibernate;

interface

uses
  Classes, InfraCommon, InfraCommonIntf, InfraHibernateIntf,
  InfraValueTypeIntf;

type
  TConfiguration = class(TElement, IConfiguration)
  private
    FProperties: TStrings;
    function GetProperties: TStrings;
    function GetPropertyItem(const pName: String): string;
    procedure SetPropertyItem(const pName: String; const Value: string);
  public
    property Properties: TStrings read GetProperties;
    property PropertyItem[const pName: String]: string read GetPropertyItem
        write SetPropertyItem;
    destructor Destroy; override;
    constructor Create;
  end;

  TPersistenceService = class(TElement, IPersistenceService)
  private
    FConfiguration: IConfiguration;
    FSessionFactory: ISessionFactory;
    function GetConfiguration: IConfiguration;
    function GetSessionFactory: ISessionFactory;
  public
    property Configuration: IConfiguration read GetConfiguration;
    property SessionFactory: ISessionFactory read GetSessionFactory;
  end;

  TSessionFactory = class(TElement, ISessionFactory)
  private
    FDialect: IDialect;
    FConnectionProvider: IConnectionProvider;
    FConfiguration: IConfiguration;
    function GetConnectionProvider: IConnectionProvider;
    function GetDialect: IDialect;
    function OpenSession: ISession; overload;
    function OpenSession(const pConnection: IConnection): ISession; overload;
  public
    property ConnectionProvider: IConnectionProvider read GetConnectionProvider;
    property Dialect: IDialect read GetDialect;
    constructor Create(pConfig: IConfiguration);
  end;

  TSession = class(TElement, ISession)
  private
    FConnection: IConnection;
    FIsDirty: Boolean;
    FSessionFactory: ISessionFactory;
    function BeginTransaction: ITransaction;
    procedure Clear;
    procedure Close;
    procedure Delete(const pObject: IInfraType);
    function GetConnection: IConnection;
    function GetIsDirty: Boolean;
    function GetSessionFactory: ISessionFactory;
    procedure Load(const pObject, pOID: IInfraType);
    procedure Save(const pObject: IInfraType);
    procedure SetIsDirty(Value: Boolean);
    procedure SetConnection(const Value: IConnection);
  public
    property Connection: IConnection read GetConnection write SetConnection;
    property IsDirty: Boolean read GetIsDirty write SetIsDirty;
    property SessionFactory: ISessionFactory read GetSessionFactory;
  end;

  TConnectionProvider = class(TElement, IConnectionProvider)
  private
    // *** FConnectionList: IConnectionList;
    procedure Close;
    procedure CloseConnection(const pConnection: IConnection);
    function GetConnection: IConnection;
  public
    constructor Create(const pConfig: IConfiguration);
  end;

implementation

uses
  SysUtils;

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

{ TPersistenceService }

function TPersistenceService.GetConfiguration: IConfiguration;
begin
  if not Assigned(FConfiguration) then
    FConfiguration := TConfiguration.Create;
  Result := FConfiguration;
end;

function TPersistenceService.GetSessionFactory: ISessionFactory;
begin
  // *** I Thing that configuration cannot be empty here.
  // *** Acho que configuração nao deveria está limpo aqui.
  if not Assigned(FSessionFactory) then
    FSessionFactory := TSessionFactory.Create(Configuration);
  Result := FSessionFactory;
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
  begin
    vTypeInfo := TypeService.GetType(
      FConfiguration.PropertyItem['Dialect'], True);
    FDialect := TypeService.CreateInstance(vTypeInfo) as IDialect;
  end;
  Result := FDialect;
end;

function TSessionFactory.OpenSession(
  const pConnection: IConnection): ISession;
begin
  Result := TSession.Create;
  Result.Connection := pConnection;
end;

function TSessionFactory.OpenSession: ISession;
var
  vConnection: IConnection;
begin
  vConnection := ConnectionProvider.GetConnection;
  Result := OpenSession(vConnection);
end;

{ TSession }

function TSession.BeginTransaction: ITransaction;
begin
  // incia uma transação para este session 
end;

procedure TSession.Clear;
begin
  // ???? 
end;

procedure TSession.Close;
begin
  // fecha o session limpando tudo 
end;

procedure TSession.Delete(const pObject: IInfraType);
begin
  // remove um objeto do banco
end;

function TSession.GetConnection: IConnection;
begin
  Result := FConnection;
end;

function TSession.GetIsDirty: Boolean;
begin
  Result := FIsDirty;
end;

function TSession.GetSessionFactory: ISessionFactory;
begin
  Result := FSessionFactory;
end;

procedure TSession.Load(const pObject, pOID: IInfraType);
begin
  // carrega um objeto do banco
end;

procedure TSession.Save(const pObject: IInfraType);
begin
  // salva um objeto no banco
end;

procedure TSession.SetConnection(const Value: IConnection);
begin
  FConnection := Value;
end;

procedure TSession.SetIsDirty(Value: Boolean);
begin
  FIsDirty := Value;
end;

{ TConnectionProvider }

procedure TConnectionProvider.Close;
begin
  // remove todas as conexões do pool
end;

procedure TConnectionProvider.CloseConnection(
  const pConnection: IConnection);
begin
  // procura a conexão na lista e remove o mesmo
end;

constructor TConnectionProvider.Create(const pConfig: IConfiguration);
begin
  // cria o pool de conexões
end;

function TConnectionProvider.GetConnection: IConnection;
begin
  // verifica se há alguma conexão livre na lista trava e retorna esta conexão
  // senao cria uma e a retorna. 
end;

end.
