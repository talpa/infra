unit InfraOPFSessionFactory;

interface

uses
  InfraCommon,
  InfraOPFIntf;

type
  TSessionFactory = class(TBaseElement, ISessionFactory)
  private
    FClosed: Boolean;
    FConfiguration: IConfiguration;
    FConnectionProvider: IConnectionProvider;
    function GetIsClosed: Boolean;
  protected
    function OpenSession: ISession;
    procedure Close;
    property isClosed: Boolean read GetIsClosed;
  public
    constructor Create(pConfiguration: IConfiguration); reintroduce;
  end;

implementation

uses
  InfraOPFSession,
  InfraOPFConnectionProvider;

{ TSessionFactory }

constructor TSessionFactory.Create(pConfiguration: IConfiguration);
begin
  inherited Create;
  FConfiguration := pConfiguration;
  FConnectionProvider := TConnectionProvider.Create(FConfiguration);
end;

{**
  Define se a fábrica está fechada
  @return Retorna verdadeiro caso a fábrica de sessões esteja fechada
}
function TSessionFactory.GetIsClosed: Boolean;
begin
  Result := FClosed;
end;

{**
  Abre uma nova Session passando um PersistenceEngine.
  Chame OpenSession para criar uma nova instancia do Session.
  @return Retorna um novo objeto Session
}
function TSessionFactory.OpenSession: ISession;
begin
  Result := TSession.Create(FConfiguration, FConnectionProvider);
end;

procedure TSessionFactory.Close;
begin
  FClosed := True;
end;

end.
