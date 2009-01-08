unit InfraPersistenceEngineTests;

interface

uses
  SysUtils,
  InfraValueTypeIntf,
  InfraPersistenceIntf,
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
    procedure TestLoadWithInvalidArgs1;
    procedure TestLoadWithInvalidArgs2;
    procedure TestSetConnectionWithInvalidArgs;
  end;

implementation

uses
  InfraPersistence,
  InfraPersistenceConnProvider,
  InfraCommonIntf,
  InfraTestsUtil;

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
  TPersistenceEngine.Create(nil,
    TConnectionProvider.Create(TTestsUtil.GetNewConfiguration));
  ExpectedException := nil;
end;

procedure TTestPersistenceEngine.TestExecuteWithInvalidArgs;
begin
  ExpectedException := EInfraArgumentError;
  FPersistenceEngine.Execute(nil);
  ExpectedException := nil;
end;

procedure TTestPersistenceEngine.TestLoadWithInvalidArgs1;
var
  List: IInfraList;
begin
//  List := ??
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

procedure TTestPersistenceEngine.TestSetConnectionWithInvalidArgs;
begin
  ExpectedException := EInfraArgumentError;
  FPersistenceEngine.SetConnection(nil);
  ExpectedException := nil;
end;

initialization
  TestFramework.RegisterTest('Persistence Testes Caixa-Cinza',
    TTestPersistenceEngine.Suite);

end.
