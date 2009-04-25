unit InfraPersistenceServiceTests;

interface

uses
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
  end;

implementation

uses
  InfraConsts,
  InfraOPFConsts,
  InfraOPFService,
  InfraMocks,
  ZDbcIntfs;

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
  vInstancia1 := FPersistenceService.GetConfiguration;
  vInstancia2 := FPersistenceService.GetConfiguration;
  CheckTrue( vInstancia1 <> vInstancia2, 'GetConfiguration retornou a mesma instância de Configuration');
end;

procedure TInfraPersistenceServiceTests.TestConfigurationIsNotNull;
begin
  CheckNotNull(FPersistenceService.GetConfiguration);
end;

initialization
  TestFramework.RegisterTest('Persistence Testes Caixa-Cinza',
    TInfraPersistenceServiceTests.Suite);
end.

