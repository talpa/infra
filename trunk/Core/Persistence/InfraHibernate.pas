unit InfraHibernate;

interface

uses
  Classes, {TStrings}
  InfraCommon, InfraCommonIntf, InfraHibernateIntf, InfraValueTypeIntf, {Infra}
  SqlExpr, DBXpress; {DBX}

type
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
    constructor Create(pConfig: IConfiguration); reintroduce;
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
    procedure Close;
    procedure CloseConnection(const pConnection: IConnection);
    procedure Configure;
    function GetConnection: IConnection;
  public
    constructor Create(const pConfig: IConfiguration); reintroduce;
  end;

  TDBXConnection = class(TElement, IConnection)
  private
    FConnection: TSQLConnection;
    FTransactionIsolation: TTransactionIsolation;
    procedure Close;
    procedure Commit;
    function ExecuteQuery(const pSQL: String): IResultSet;
    function ExecuteUpdate(const pSQL: String): Integer;
    function GetIsClosed: Boolean;
    function GetTransactionIsolation: TTransactionIsolation;
    procedure Rollback;
    procedure SetTransactionIsolation(Value: TTransactionIsolation);
  public
    constructor Create; override;
    destructor Destroy; override;
    property IsClosed: Boolean read GetIsClosed;
    property TransactionIsolation: TTransactionIsolation read
        GetTransactionIsolation write SetTransactionIsolation;
  end;

implementation

uses
  SysUtils;

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
    vTypeInfo :=
      TypeService.GetType(FConfiguration.PropertyItem['Dialect'], True);
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

constructor TConnectionProvider.Create(const pConfig: IConfiguration);
begin
  inherited Create;
  Configure;
  // - cria o pool de conexões;
  // - guarda o classinfo de connection a ser instanciada quando chamar
  //   GetConnection. com base no ConnectionString e ConnectionStringName;
end;

procedure TConnectionProvider.Close;
begin
  // remove todas as conexões do pool
end;

procedure TConnectionProvider.CloseConnection(const pConnection: IConnection);
begin
  // procura a conexão na lista e remove o mesmo
end;

procedure TConnectionProvider.Configure;
begin
  // Guarda em IDriver uma instância do driver a ser usado pelo Connection
  // Este driver é criado com base na reflexão pela string definida na chave
  // Environment.ConnectionDriver
end;

function TConnectionProvider.GetConnection: IConnection;
begin
  // verifica se há alguma conexão livre na lista trava e retorna esta conexão
  // senao cria uma e a retorna.
end;

{ TDBXConnection }

constructor TDBXConnection.Create;
begin
  inherited Create;
  FConnection := TSQLConnection.Create(nil);
end;

destructor TDBXConnection.Destroy;
begin
  FreeAndNil(FConnection);
  inherited;
end;

procedure TDBXConnection.Close;
begin
  FConnection.Close;
end;

procedure TDBXConnection.Commit;
begin
  // *** nao mecher com transaçao agora
end;

function TDBXConnection.ExecuteQuery(const pSQL: String): IResultSet;
begin
{
  FConnection.Execute(pSQL, nil);
var
  SQLstmt: String;
  stmtParams: TParams;
begin
  stmtParams := TParams.Create;
  try
    SQLConnection1.Connected := True;
    stmtParams.CreateParam(ftString, 'StateParam', ptInput);
    stmtParams.ParamByName('StateParam').AsString := 'CA';
    SQLstmt := 'INSERT INTO "Custom" '+
      '(CustNo, Company, State) ' +
      'VALUES (7777, "Robin Dabank Consulting", :StateParam)';
    SQLConnection1.Execute(SQLstmt, stmtParams);
  finally
    stmtParams.Free;
  end;
}
end;

function TDBXConnection.ExecuteUpdate(const pSQL: String): Integer;
begin
  Result := 0;
end;

function TDBXConnection.GetIsClosed: Boolean;
begin
  Result := not FConnection.Connected;
end;

function TDBXConnection.GetTransactionIsolation: TTransactionIsolation;
begin
  // *** nao mecher com transaçao agora
  Result := tiNone;
end;

procedure TDBXConnection.Rollback;
begin
  // *** nao mecher com transaçao agora
end;

procedure TDBXConnection.SetTransactionIsolation(
  Value: TTransactionIsolation);
begin
  FTransactionIsolation := Value;
end;

end.
