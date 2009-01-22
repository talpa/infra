unit InfraPersistenceEngineTests;

interface

uses
  SysUtils,
  InfraValueTypeIntf,
  InfraOPFIntf,
  TestFramework;

type
  TTestPersistenceEngine = class(TTestCase)
  private
    FPersistenceEngine: IPersistenceEngine;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestExecuteWithInvalidArgs;
    procedure TestExecuteAllWithInvalidArgs;
    procedure TestLoadWithInvalidArgs1;
    procedure TestLoadWithInvalidArgs2;
  end;

implementation

uses
  InfraOPFEngine,
  InfraOPFConnectionProvider,
  InfraCommonIntf,
  InfraTestsUtil,
  InfraOPFSqlCommands;

{ TTestPersistenceEngine }

procedure TTestPersistenceEngine.SetUp;
var
  vConfig: IConfiguration;
begin
  inherited;
  vConfig := TTestsUtil.GetNewConfiguration;
  FPersistenceEngine := TPersistenceEngine.Create(vConfig,
    TConnectionProvider.Create(vConfig));
end;

procedure TTestPersistenceEngine.TearDown;
begin
  FPersistenceEngine := nil;
  inherited;
end;

procedure TTestPersistenceEngine.TestCreate;
begin
  ExpectedException := EInfraArgumentError;
  TPersistenceEngine.Create(nil, nil);
  ExpectedException := nil;
end;

procedure TTestPersistenceEngine.TestExecuteWithInvalidArgs;
begin
  ExpectedException := EInfraArgumentError;
  FPersistenceEngine.Execute(nil);
  ExpectedException := nil;
end;

procedure TTestPersistenceEngine.TestExecuteAllWithInvalidArgs;
begin
  ExpectedException := EInfraArgumentError;
  FPersistenceEngine.ExecuteAll(nil);
  ExpectedException := nil;
end;

procedure TTestPersistenceEngine.TestLoadWithInvalidArgs1;
var
  List: IInfraList;
begin
  ExpectedException := EInfraArgumentError;
  FPersistenceEngine.Load(nil, List);
  ExpectedException := nil;
end;

procedure TTestPersistenceEngine.TestLoadWithInvalidArgs2;
begin
  ExpectedException := EInfraArgumentError;
  FPersistenceEngine.Load(TSQLCommandQuery.Create(FPersistenceEngine), nil);
  ExpectedException := nil;
end;

initialization
  TestFramework.RegisterTest('Persistence Testes Caixa-Cinza',
    TTestPersistenceEngine.Suite);

end.

