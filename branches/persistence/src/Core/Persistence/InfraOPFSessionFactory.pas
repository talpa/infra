unit InfraOPFSessionFactory;

interface

uses
  InfraCommon,
  InfraOPFIntf,
  ZDbcIntfs;

type
  TSessionFactory = class(TBaseElement, ISessionFactory)
  private
    FClosed: Boolean;
    FConfiguration: IConfiguration;
    FConnectionProvider: IConnectionProvider;
    FConnectionString: string;
    function BuildConnectionString(pConfiguration: IConfiguration): string;
    function GetIsClosed: Boolean;
  protected
    function OpenSession: ISession; overload;
    function OpenSession(pConnection: IZConnection): ISession; overload;
    procedure Close;
    property isClosed: Boolean read GetIsClosed;
  public
    constructor Create(pConfiguration: IConfiguration); reintroduce;
  end;

implementation

uses
  InfraOPFConsts,
  InfraOPFSession,
  InfraOPFConnectionProvider,
  InfraCommonIntf;

{ TSessionFactory }

procedure TSessionFactory.Close;
begin
  FClosed := True;
end;

constructor TSessionFactory.Create(pConfiguration: IConfiguration);
begin
  inherited Create;
  // Clona a conexao para não guardar referencia a ela, assim,
  // modificar o Configuration após ser a SessionFactory, não terá efeito 
  FConfiguration := pConfiguration.Clone;
  FConnectionString := BuildConnectionString(FConfiguration);
  // TODO: depois precisamos passar mais informações para o ConnectionProvider,
  // mas acho desnecessário mandar o Configuration: O acoplamento geral está
  // aumentando muito
  FConnectionProvider := TConnectionProvider.Create(FConnectionString);
end;

{*
  Constrói a URL de conexão com o banco
  @param pConfiguration Objeto com as configurações de conexão com o banco de dados
  @return Retorna uma string no formato
    zdbc:<driver>://<hostname>/<databasename>?username=<username>;password=<password>
*}
function TSessionFactory.BuildConnectionString(pConfiguration: IConfiguration):
    string;
begin
  Result := 'zdbc:' + pConfiguration.GetAsString(cCONFIGKEY_DRIVER) +
    '://' + pConfiguration.GetAsString(cCONFIGKEY_HOSTNAME) +
    '/' + pConfiguration.GetAsString(cCONFIGKEY_DATABASENAME) +
    '?username=' + pConfiguration.GetAsString(cCONFIGKEY_USERNAME) +
    ';password=' + pConfiguration.GetAsString(cCONFIGKEY_PASSWORD);
end;

{**
  Retorna se a fábrica está fechada

  @return Retorna True se a fábrica estiver fechada
}
function TSessionFactory.GetIsClosed: Boolean;
begin
  Result := FClosed;
end;

{**
  Cria uma nova Session com a conexao informada
  Chame OpenSession para criar uma nova instancia de Session.

  @param pConnection Parameter Description
  @return Retorna um novo objeto Session
}
function TSessionFactory.OpenSession(pConnection: IZConnection): ISession;
begin
  Result := TSession.Create(pConnection, FConfiguration);
end;

{**
  Cria uma nova Session. Cria uma nova conexao
  Chame OpenSession para criar uma nova instancia de Session.

  @return Retorna um novo objeto Session
}
function TSessionFactory.OpenSession: ISession;
begin
  Result := TSession.Create(FConnectionProvider.GetConnection, FConfiguration);
end;

end.
