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
    FPersistenceEngine: IPersistenceEngine;
    function GetIsClosed: Boolean;
  protected
    function OpenSession: ISession; overload;
    procedure Close;
    property isClosed: Boolean read GetIsClosed;
  public
    constructor Create(pConfiguration: IConfiguration); reintroduce;
  end;

implementation

uses
  InfraOPFConsts,
  InfraOPFSession,
  InfraOPFEngine,
  InfraCommonIntf;

{ TSessionFactory }

procedure TSessionFactory.Close;
begin
  FClosed := True;
end;

constructor TSessionFactory.Create(pConfiguration: IConfiguration);
begin
  inherited Create;
  FConfiguration := pConfiguration;
  FPersistenceEngine := TPersistenceEngine.Create(FConfiguration);
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
  Cria uma nova Session. Cria uma nova conexao
  Chame OpenSession para criar uma nova instancia de Session.

  @return Retorna um novo objeto Session
}
function TSessionFactory.OpenSession: ISession;
begin
  Result := TSession.Create(FConfiguration);
end;

end.
