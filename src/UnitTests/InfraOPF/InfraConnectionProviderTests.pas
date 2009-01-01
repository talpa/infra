unit InfraConnectionProviderTests;

interface

uses SysUtils, Classes, TestFramework, InfraPersistenceIntf;

type
  TTestConnectionProvider = class(TTestCase)
  private
    FConnProvider: IConnectionProvider;
    function CreateConfiguration: IConfiguration;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestCreateWithoutDriverManager;
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
  InfraCommonIntf;

function TTestConnectionProvider.CreateConfiguration: IConfiguration;
begin
  Result := TConfiguration.Create;
  Result.SetValue(cCONFIGKEY_CONNECTIONTIME, 10);
  Result.SetValue(cCONFIGKEY_MAXCONNECTIONS, 2);
end;

{ TTestConnectionProvider }

procedure TTestConnectionProvider.SetUp;
begin
  inherited;
  FConnProvider := TConnectionProvider.Create(TDriverManagerMock.Create, CreateConfiguration);
end;

procedure TTestConnectionProvider.TearDown;
begin
  FConnProvider := nil;
  inherited;
end;

procedure TTestConnectionProvider.TestCreate;
begin
  CheckNotNull(FConnProvider);
end;

procedure TTestConnectionProvider.TestCreateWithoutDriverManager;
begin
  ExpectedException := EInfraArgumentError;
  TConnectionProvider.Create(TDriverManagerMock.Create, nil);
  ExpectedException := nil;
end;

procedure TTestConnectionProvider.TestCreateWithoutConfiguration;
begin
  inherited;
  ExpectedException := EInfraArgumentError;
  TConnectionProvider.Create(nil, CreateConfiguration);
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
var
  lConnection1, lConnection2: IZConnection;
begin
  lConnection1 := FConnProvider.GetConnection;
  CheckNotNull(lConnection1, 'Falha no retorno da conexão 1');

  lConnection2 := FConnProvider.GetConnection;
  CheckNotNull(lConnection2, 'Falha no retorno da conexão 2');

  CheckTrue(lConnection1 = lConnection2, 'Deveria ter retornado a primeira conexao fechada disponível');
end;

procedure TTestConnectionProvider.TestGetConnectionObjUnavailableInPool;
var
  lConnection1, lConnection2: IZConnection;
begin
  lConnection1 := FConnProvider.GetConnection;
  lConnection1.Open;

  lConnection2 := FConnProvider.GetConnection;
  CheckNotNull(lConnection2, 'Falha no retorno da conexão 2');

  CheckTrue(lConnection1 <> lConnection2, 'Deveria ter criado uma nova conexao');
end;

procedure TTestConnectionProvider.TestGetConnectionBeyoundMaxSize;
var
  lConnection1, lConnection2, lConnection3: IZConnection;
begin
  lConnection1 := FConnProvider.GetConnection;
  lConnection1.Open;

  lConnection2 := FConnProvider.GetConnection;
  lConnection2.Open;

  ExpectedException := EInfraConnectionProviderError;
  lConnection3 := FConnProvider.GetConnection;
  ExpectedException := nil;
end;

procedure TTestConnectionProvider.TestCloseConnection;
var
  lConnection1: IZConnection;
begin
  lConnection1 := FConnProvider.GetConnection;
  lConnection1.Open;

  CheckFalse(lConnection1.IsClosed, 'A conexão está fechada');

  FConnProvider.ReleaseConnection(lConnection1);

  CheckTrue(lConnection1.IsClosed, 'A conexão continua aberta');
end;

procedure TTestConnectionProvider.TestCloseConnectionNotFound;
var
  lConnection1: IZConnection;
begin
  // Esta conexão não está no Pool
  lConnection1 := TDriverManagerMock.Create.GetConnection('');
  lConnection1.Open;

  ExpectedException := EInfraConnectionProviderError;
  FConnProvider.ReleaseConnection(lConnection1);
  ExpectedException := nil;
end;

procedure TTestConnectionProvider.TestCloseConnectionAlreadyClosed;
var
  lConnection1: IZConnection;
begin
  lConnection1 := FConnProvider.GetConnection;
  lConnection1.Close; // Só pra ficar claro

  ExpectedException := EInfraConnectionProviderError;
  FConnProvider.ReleaseConnection(lConnection1);
  ExpectedException := nil;
end;

procedure TTestConnectionProvider.TestClose;
var
  lConnection1, lConnection2: IZConnection;
begin
  lConnection1 := FConnProvider.GetConnection;
  lConnection1.Open;

  lConnection2 := FConnProvider.GetConnection;
  lConnection2.Open;

  FConnProvider.Close;

  CheckTrue(lConnection1.IsClosed and lConnection2.IsClosed, 'Close falhou');
end;

initialization
  TestFramework.RegisterTest(TTestConnectionProvider.Suite);
end.

