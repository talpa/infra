unit InfraPersistenceServiceTests;

interface

uses
  InfraPersistence,
  InfraPersistenceIntf,
  InfraCommonIntf,
  TestFrameWork;

type
  TInfraPersistenceServiceTests = class(TTestCase)
  private
    FPersistenceService: IInfraPersistenceService;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Test methods
    procedure TestConfigurationIsNotNull;
    procedure TestConfiguration;
    procedure TestOpenSessionWithNoConfig;
    procedure TestOpenSession;
    procedure TestSetConnection;
  end;

implementation

uses
  InfraConsts, InfraPersistenceConsts;

procedure TInfraPersistenceServiceTests.SetUp;
begin
  inherited;
  FPersistenceService := TInfraPersistenceService.Create;
end;

procedure TInfraPersistenceServiceTests.TearDown;
begin
  // Se esta linha nao existir, acontece um AV no final da aplicação
  FPersistenceService := nil;
  inherited;
end;

procedure TInfraPersistenceServiceTests.TestConfiguration;
var
  vInstancia1, vInstancia2: IConfiguration;
begin
  vInstancia1 := FPersistenceService.Configuration;
  vInstancia2 := FPersistenceService.Configuration;
  CheckTrue( vInstancia1 = vInstancia2, 'GetConfiguration retornou uma instância diferente de Configuration');
end;

procedure TInfraPersistenceServiceTests.TestConfigurationIsNotNull;
begin
  CheckNotNull(FPersistenceService.Configuration);
end;

procedure TInfraPersistenceServiceTests.TestOpenSessionWithNoConfig;
begin
  ExpectedException := EInfraError;
  FPersistenceService.OpenSession;
  ExpectedException := nil;
end;

procedure TInfraPersistenceServiceTests.TestOpenSession;
begin
  FPersistenceService.Configuration.PropertyItem[cCONFIGKEY_DRIVER] := 'firebird';
  FPersistenceService.Configuration.PropertyItem[cCONFIGKEY_HOSTNAME] := 'localhost';
  FPersistenceService.Configuration.PropertyItem[cCONFIGKEY_USERNAME] := 'sysdba';
  FPersistenceService.Configuration.PropertyItem[cCONFIGKEY_PASSWORD] := 'masterkey';
  FPersistenceService.OpenSession;
end;

procedure TInfraPersistenceServiceTests.TestSetConnection;
begin
  // TODO:
end;

initialization

  TestFramework.RegisterTest('InfraPersistenceServiceTests Suite',
    TInfraPersistenceServiceTests.Suite);

end.
 