unit InfraOPFService;

interface

uses
  {Infra}
  InfraCommon,
  InfraCommonIntf,
  InfraOPFIntf,
  InfraOPFConfiguration;

type
  /// Descrição da classe
  TInfraPersistenceService = class(TBaseElement, IInfraPersistenceService)
  protected
    function GetConfiguration: IConfiguration;
  end;

implementation

{ TInfraPersistenceService }

{**
  Cria um novo objeto Configuration
  Chame GetConfiguration para obter um novo objeto configuration, com o qual
  poderá construir uma nova SessionFactory.
  
  @return Retorna um objeto que implementa IConfiguration
*}
function TInfraPersistenceService.GetConfiguration: IConfiguration;
begin
  Result := TConfiguration.Create;
end;

// Não entendi, mas se pôr direto no Initialization acontece Access Violations.
// ATENÇÃO: Vc não deve atribuir PersistenceService para uma variável de
// instancia nem global sem que no final da aplicação atribuia nil a ela explicitamente,
// sob pena de acontecer um AV no final da aplicação
procedure InjectPersistenceService;
begin
  (ApplicationContext as IBaseElement).Inject(
    IInfraPersistenceService, TInfraPersistenceService.Create);
end;

initialization
  InjectPersistenceService;
  
end.

