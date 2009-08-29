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
  FConnProvider := TConnectionProvider.Create(TTestsUtil.GetNewConfiguration);
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
  TConnectionProvider.Create(nil);
  ExpectedException := nil;
end;

procedure TTestConnectionProvider.TestGetConnection;
var
  vConnection1, vConnection2: IConnectionItem;
begin
  vConnection1 := FConnProvider.Acquire;
  CheckNotNull(vConnection1, 'Falha no retorno da conexão #1');
  CheckEquals(1, FConnProvider.ActiveConnections);

  vConnection2 := FConnProvider.Acquire;
  CheckNotNull(vConnection2, 'Falha no retorno da conexão #2');
  CheckEquals(2, FConnProvider.ActiveConnections);
end;

procedure TTestConnectionProvider.TestGetConnectionObjAvailableInPool;
var
  vConnection1, vConnection2: IConnectionItem;
begin
  vConnection1 := FConnProvider.Acquire;
  CheckNotNull(vConnection1, 'Falha no retorno da conexão #1');
  CheckEquals(1, FConnProvider.ActiveConnections);
  vConnection1 := nil; // A conexão foi liberada mas deve permanecer no Pool

  vConnection2 := FConnProvider.Acquire;
  CheckNotNull(vConnection2, 'Falha no retorno da conexão #2');
  CheckEquals(1, FConnProvider.ActiveConnections);
end;

procedure TTestConnectionProvider.TestGetConnectionObjUnavailableInPool;
var
  vConnection1, vConnection2: IConnectionItem;
begin
  vConnection1 := FConnProvider.Acquire;
  CheckNotNull(vConnection1, 'Falha no retorno da conexão #1');

  vConnection2 := FConnProvider.Acquire;
  CheckNotNull(vConnection2, 'Falha no retorno da conexão #2');

  CheckFalse(vConnection1 = vConnection2, 'O ConnectionProvider retornou a mesma conexão');
  CheckEquals(2, FConnProvider.ActiveConnections, 'Número de conexões errado');
end;

procedure TTestConnectionProvider.TestGetConnectionBeyoundMaxSize;
var
  vConnections: array of IConnectionItem;
  vAnotherConnection: IConnectionItem;
  i: Integer;
begin
  SetLength(vConnections, FConnProvider.PoolSize);
  for i := Low(vConnections) to High(vConnections) do
  begin
    vConnections[i] := FConnProvider.Acquire;
    CheckNotNull(vConnections[i], 'Falha no retorno da conexão #'+IntToStr(i+1));
  end;

  ExpectedException := EPersistenceConnectionProviderError;
  vAnotherConnection := FConnProvider.Acquire;
  ExpectedException := nil;
end;

initialization
  TestFramework.RegisterTest('Persistence Testes Caixa-Cinza',
    TTestConnectionProvider.Suite);
    
end.



