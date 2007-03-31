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
    constructor Create;
    destructor Destroy; override;
    property Properties: TStrings read GetProperties;
    property PropertyItem[const pName: String]: string read GetPropertyItem
        write SetPropertyItem;
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
    FConfiguration: IConfiguration;
    FConnectionProvider: IConnectionProvider;
    FDialect: IDialect;
    function GetConnectionProvider: IConnectionProvider;
    function GetDialect: IDialect;
    function OpenSession: ISession; overload;
    function OpenSession(const pConnection: IConnection): ISession; overload;
  public
    constructor Create(pConfig: IConfiguration);
    property ConnectionProvider: IConnectionProvider read GetConnectionProvider;
    property Dialect: IDialect read GetDialect;
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
    procedure SetConnection(const Value: IConnection);
    procedure SetIsDirty(Value: Boolean);
  public
    property Connection: IConnection read GetConnection write SetConnection;
    property IsDirty: Boolean read GetIsDirty write SetIsDirty;
    property SessionFactory: ISessionFactory read GetSessionFactory;
  end;

  TConnectionProvider = class(TElement, IConnectionProvider)
  private
    FDriver: IDriver;
    procedure Close;
    procedure CloseConnection(const pConnection: IConnection);
    procedure ConfigureDriver;
    function GetConnection: IConnection;
    function GetDriver: IDriver;
  public
    constructor Create(const pConfig: IConfiguration);
    property Driver: IDriver read GetDriver;
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
  // *** Acho que configura��o nao deveria est� limpo aqui.
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

function TSessionFactory.OpenSession: ISession;
var
  vConnection: IConnection;
begin
  vConnection := ConnectionProvider.GetConnection;
  Result := OpenSession(vConnection);
end;

function TSessionFactory.OpenSession(const pConnection: IConnection): ISession;
begin
  Result := TSession.Create;
  Result.Connection := pConnection;
end;

{ TSession }

function TSession.BeginTransaction: ITransaction;
begin
  // incia uma transa��o para este session
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

constructor TConnectionProvider.Create(const pConfig: IConfiguration);
begin
  // - cria o pool de conex�es;
  // - guarda o classinfo de connection a ser instanciada quando chamar
  //   GetConnection. com base no ConnectionString e ConnectionStringName;
  ConfigureDriver;
end;

procedure TConnectionProvider.Close;
begin
  // remove todas as conex�es do pool
end;

procedure TConnectionProvider.CloseConnection(const pConnection: IConnection);
begin
  // procura a conex�o na lista e remove o mesmo
end;

procedure TConnectionProvider.ConfigureDriver;
begin
  // Guarda em IDriver uma inst�ncia do driver a ser usado pelo Connection
  // Este driver � criado com base na reflex�o pela string definida na chave
  // Environment.ConnectionDriver
end;

function TConnectionProvider.GetConnection: IConnection;
begin
  // verifica se h� alguma conex�o livre na lista trava e retorna esta conex�o
  // senao cria uma e a retorna.
end;

function TConnectionProvider.GetDriver: IDriver;
begin
  Result := FDriver;
end;

end.
