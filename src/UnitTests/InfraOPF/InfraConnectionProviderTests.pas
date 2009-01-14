unit InfraConnectionProviderTests;

interface

uses
  SysUtils,
  Classes,
  TestFramework,
  InfraOPFIntf;

type
  TTestConnectionProvider = class(TTestCase)
  private
    FConnProvider: IConnectionProvider;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateWithoutConnectionString;
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
  InfraOPFConnectionProvider,
  InfraOPFConsts,
  InfraMocks,
  ZDbcIntfs,
  InfraCommonIntf,
  InfraTestsUtil;

{ TTestConnectionProvider }

procedure TTestConnectionProvider.SetUp;
begin
  inherited;
  FConnProvider := TConnectionProvider.Create(TTestsUtil.GetConnectionString);
end;

procedure TTestConnectionProvider.TearDown;
begin
  FConnProvider := nil;
  inherited;
end;

procedure TTestConnectionProvider.TestCreateWithoutConnectionString;
begin
  inherited;
  ExpectedException := EInfraArgumentError;
  TConnectionProvider.Create('');
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
  // TODO: Implementar teste
end;

procedure TTestConnectionProvider.TestGetConnectionObjUnavailableInPool;
begin
  // TODO: Implementar teste
end;

procedure TTestConnectionProvider.TestGetConnectionBeyoundMaxSize;
begin
  // TODO: Implementar teste
end;

procedure TTestConnectionProvider.TestCloseConnection;
begin
  // TODO: Implementar teste
end;

procedure TTestConnectionProvider.TestCloseConnectionNotFound;
begin
  // TODO: Implementar teste
end;

procedure TTestConnectionProvider.TestCloseConnectionAlreadyClosed;
begin
  // TODO: Implementar teste
end;

procedure TTestConnectionProvider.TestClose;
begin
  // TODO: Implementar teste
end;

initialization
  TestFramework.RegisterTest('Persistence Testes Caixa-Cinza',
    TTestConnectionProvider.Suite);
    
end.


