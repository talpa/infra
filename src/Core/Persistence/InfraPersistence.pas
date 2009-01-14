unit InfraPersistence;

interface

uses
  {Infra}
  InfraCommon,
  InfraCommonIntf,
  InfraOPFIntf,
  InfraOPFConfiguration;

type
  /// Descrição da classe
  TPersistentState = class(TBaseElement, IPersistentState)
  private
    FState: TPersistentStateKind;
    FIsPersistent: Boolean;
  protected
    function GetIsPersistent: Boolean;
    function GetState: TPersistentStateKind;
    procedure SetIsPersistent(Value: Boolean);
    procedure SetState(Value: TPersistentStateKind);
    property IsPersistent: Boolean read GetIsPersistent write SetIsPersistent;
    property State: TPersistentStateKind read GetState write SetState;
  end;

  /// Descrição da classe
  TInfraPersistenceService = class(TBaseElement, IInfraPersistenceService)
  protected
    function GetConfiguration: IConfiguration;
  end;

implementation

{ TPersistentState }

{*
  @return ResultDescription
}
function TPersistentState.GetIsPersistent: Boolean;
begin
  Result := FIsPersistent;
end;

{*
  @return ResultDescription
}
function TPersistentState.GetState: TPersistentStateKind;
begin
  Result := FState;
end;

{*
  @param Value   ParameterDescription
  @return ResultDescription
}
procedure TPersistentState.SetIsPersistent(Value: Boolean);
begin
  FIsPersistent := Value;
end;

{*

  @param Value   ParameterDescription
  @return ResultDescription
}
procedure TPersistentState.SetState(Value: TPersistentStateKind);
begin
  FState := Value;
end;

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

