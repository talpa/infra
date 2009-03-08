unit InfraBindingService;

interface

uses
  InfraCommon, InfraBindingIntf;

type
  /// Serviço de Binding
  TInfraBindingService = class(TBaseElement, IInfraBindingService)
  protected
    function GetNewBindManager: IBindManager;
  end;

implementation

uses
  InfraCommonIntf,
  InfraBindingManager;

{ TInfraBindingService }

{**
  Cria um novo objeto BindManager
  Chame GetNewBindManager para obter um novo objeto BindManager, com o qual
  poderá fazer a ligação entre controles de tela, ou entre controles de tela
  com infratypes.

  @return Retorna um objeto que implementa IBindManager
*}
function TInfraBindingService.GetNewBindManager: IBindManager;
begin
  Result := TBindManager.Create;
end;

// Não entendi, mas se pôr direto no Initialization acontece Access Violations.
// ATENÇÃO: Vc não deve atribuir PersistenceService para uma variável de
// instancia nem global sem que no final da aplicação atribuia nil a ela explicitamente,
// sob pena de acontecer um AV no final da aplicação
procedure InjectBindingService;
begin
  (ApplicationContext as IBaseElement).Inject(
    IInfraBindingService, TInfraBindingService.Create);
end;

initialization
  InjectBindingService;
  
end.
