unit ApplicationContext;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraCommonIntf;

type
  TApplicationContext = class(TInterfacedObject, IApplicationContext,
    IMemoryManagedObject, IInterface)
  private
    FElement: IMemoryManagedObject;
    FEventService: IInfraEventService;
    FReferenceService: IInfraReferenceService;
    FTypeService: ITypeService;
  protected
    function GetElement: IMemoryManagedObject;
    function GetEventService: IInfraEventService;
    function GetReferenceService: IInfraReferenceService;
    function GetTypeService: ITypeService;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    procedure ShutDown;
    property Element: IMemoryManagedObject read GetElement
      implements IMemoryManagedObject;
    property EventService: IInfraEventService read GetEventService;
    property ReferenceService: IInfraReferenceService read GetReferenceService;
    property TypeService: ITypeService 
      read GetTypeService;
  end;

implementation

// Ps: It is important put all services on use clause.
uses
  InfraReflect,
  InfraCommon,
  InfraNotify,
  InfraReference,
  InfraCommonRegister,
  InfraValueTypeRegister,
  InfraHibernateRegister,
  InfraGUIBuilderRegister;
  // *** InfraMVPRegister;

{ TApplicationContext }

function TApplicationContext.GetElement: IMemoryManagedObject;
begin
  if not Assigned(FElement) then 
    FElement := TMemoryManagedObject.Create;
  Result := FElement;
end;

function TApplicationContext.GetEventService: IInfraEventService;
begin
  if not Assigned(FEventService) then
    FEventService := TInfraEventService.Create;
  Result := FEventService;
end;

function TApplicationContext.GetReferenceService: IInfraReferenceService;
begin
  if not Assigned(FReferenceService) then
    FReferenceService := TInfraReferenceService.Create;
  Result := FReferenceService;
end;

function TApplicationContext.GetTypeService: ITypeService;
begin
  if not Assigned(FTypeService) then
    FTypeService := TTypeService.Create;
  Result := FTypeService;
end;

function TApplicationContext.QueryInterface(
  const IID: TGUID; out Obj): HResult;
begin
  Result := E_NOINTERFACE;
  if Assigned(FElement) then
    Result := FElement.QueryInterface(IID, Obj);
  if Result <> 0 then
    Result := inherited QueryInterface(IID, Obj);
end;

procedure TApplicationContext.ShutDown;
begin
  if Assigned(FElement) then
    FElement := nil;
  FTypeService := nil;
  FEventService := nil;
  FReferenceService := nil;
end;

procedure RegisterAllOnReflection;
begin
  InfraCommonRegister.RegisterOnReflection;
  InfraValueTypeRegister.RegisterOnReflection;
  InfraHibernateRegister.RegisterOnReflection;
  InfraGUIBuilderRegister.RegisterOnReflection;
  // *** Marcos: voltar quando encaixar o MVP
  // *** InfraMVPRegister.RegisterOnReflection;
end;

initialization
  RegisterApplicationContext(TApplicationContext.Create as IApplicationContext);
  RegisterAllOnReflection;

end.
