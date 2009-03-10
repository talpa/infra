unit InfraBindingService;

interface

uses
    Controls,InfraCommon, InfraBindingIntf;

type
  /// Serviço de Binding
  TInfraBindingService = class(TBaseElement, IInfraBindingService)
  private
     FMappingControl : IMappingControl;
  protected
    function GetNewBindManager: IBindManager;
    function RegisterControl(Control: TControl; BindableID: TGUID): IBindable;
  end;

implementation

uses
  InfraCommonIntf,
  InfraBindingManager,
  List_MappingControl;

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
  FMappingControl:= TMappingControl.Create;
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

function TInfraBindingService.RegisterControl(Control: TControl;
  BindableID: TGUID): IBindable;
begin
  FMappingControl.Add(Control.ClassType,BindableID)
end;

initialization
  InjectBindingService;
  
end.
