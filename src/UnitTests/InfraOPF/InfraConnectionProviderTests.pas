unit InfraConnectionProviderTests;

interface

uses SysUtils, Classes, TestFramework, InfraPersistenceIntf;

type
  TTestConnectionProvider = class(TTestCase)
  private
    FConnProvider: IConnectionProvider;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateWithoutConfiguration;
    procedure TestGetConnection;
    procedure TestGetConnectionObjAvailableInPool;
    procedure TestGetConnectionObjUnavailableInPool;
    procedure TestGetConnectionBeyoundMaxSize;
    procedure TestCloseConnection;
    procedure TestCloseConnectionAlreadyClosed;
    procedure TestCloseConnectionNotFound;
    procedure TestClose;
  end;

implementation

uses
  InfraPersistence,
  InfraPersistenceConsts,
  InfraMocks,
  ZDbcIntfs,
  InfraCommonIntf,
  InfraTestsUtil;

{ TTestConnectionProvider }

procedure TTestConnectionProvider.SetUp;
begin
  inherited;
  FConnProvider := TConnectionProvider.Create(TTestsUtil.GetNewConfiguration);
end;

procedure TTestConnectionProvider.TearDown;
begin
  FConnProvider := nil;
  inherited;
end;

procedure TTestConnectionProvider.TestCreateWithoutConfiguration;
begin
  inherited;
  ExpectedException := EInfraArgumentError;
  TConnectionProvider.Create(nil);
  ExpectedException := nil;
end;

procedure TTestConnectionProvider.TestGetConnection;
var
  lConnection: IZConnection;
begin
  lConnection := FConnProvider.GetConnection;
  CheckNotNull(lConnection, 'Falha no retorno da conexão');
  CheckTrue(lConnection.IsClosed, 'Conexao está aberta');
end;

procedure TTestConnectionProvider.TestGetConnectionObjAvailableInPool;
begin

end;

procedure TTestConnectionProvider.TestGetConnectionObjUnavailableInPool;
begin

end;

procedure TTestConnectionProvider.TestGetConnectionBeyoundMaxSize;
begin

end;

procedure TTestConnectionProvider.TestCloseConnection;
begin

end;

procedure TTestConnectionProvider.TestCloseConnectionNotFound;
begin

end;

procedure TTestConnectionProvider.TestCloseConnectionAlreadyClosed;
begin

end;

procedure TTestConnectionProvider.TestClose;
begin

end;

initialization
  TestFramework.RegisterTest('Persistence Testes Caixa-Cinza',
    TTestConnectionProvider.Suite);
    
end.

