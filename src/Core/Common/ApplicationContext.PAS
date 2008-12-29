unit ApplicationContext;

interface

{$I InfraCommon.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraCommonIntf;

type
  TApplicationContext = class(TInterfacedObject, IApplicationContext,
    IBaseElement, IInterface)
  private
    FElement: IBaseElement;
    FEventService: IInfraEventService;
    FReferenceService: IInfraReferenceService;
    FTypeService: ITypeService;
  protected
    function GetElement: IBaseElement;
    function GetEventService: IInfraEventService;
    function GetReferenceService: IInfraReferenceService;
    function GetTypeService: ITypeService;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    procedure ShutDown;
    property Element: IBaseElement read GetElement
      implements IBaseElement;
    property EventService: IInfraEventService read GetEventService;
    property ReferenceService: IInfraReferenceService read GetReferenceService;
    property TypeService: ITypeService
      read GetTypeService;
  end;

implementation

uses
  InfraReflect,
  InfraCommon,
  InfraNotify,
  InfraReference,
  InfraCommonRegister;

{ TApplicationContext }

function TApplicationContext.GetElement: IBaseElement;
begin
  if not Assigned(FElement) then 
    FElement := TBaseElement.Create;
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

initialization
  RegisterApplicationContext(TApplicationContext.Create as IApplicationContext);
  InfraCommonRegister.RegisterOnReflection;

end.
