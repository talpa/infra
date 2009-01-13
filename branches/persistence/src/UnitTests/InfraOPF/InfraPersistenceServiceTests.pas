unit InfraPersistenceServiceTests;

interface

uses
  InfraPersistence,
  InfraOPFIntf,
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
  end;

implementation

uses
  InfraConsts, InfraOPFConsts, InfraMocks, ZDbcIntfs;

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
  with FPersistenceService.Configuration do
  begin
    SetValue(cCONFIGKEY_DRIVER, 'firebird');
    SetValue(cCONFIGKEY_HOSTNAME, 'localhost');
    SetValue(cCONFIGKEY_USERNAME, 'sysdba');
    SetValue(cCONFIGKEY_PASSWORD, 'masterkey');
  end;
  FPersistenceService.OpenSession;
end;

initialization
  TestFramework.RegisterTest('Persistence Testes Caixa-Cinza',
    TInfraPersistenceServiceTests.Suite);
end.
 
