unit InfraValidator;

interface

uses
  InfraCommonIntf,
  InfraCommon,
  InfraValueTypeIntf,
  InfraValueType,
  InfraValidatorIntf;

type
  TValidationService = class(TMemoryManagedObject, IInfraValidationService)
  protected
    function GetValidatorFor(const InfraType: IInfraType): IInfraValidator;
  end;

  TValidator = class(TInfraBoolean, IInfraValidator)
  private
    FInnerValidators: IInfraValidatorList;
    FConstrainedObject: IInfraType;
    FOwner: IInfraValidatorList;
    function GetConstrainedObject: IInfraType;
    procedure SetConstrainedObject(const Value: IInfraType);
  protected
    function IInfraValidator.GetIsValid = GetAsBoolean;
    procedure IInfraValidator.SetIsValid = SetAsBoolean;
    procedure Calculate; override;
    function GetInnerValidators: IInfraValidatorList;
    procedure InvalidateCache; override;
    property InnerValidators: IInfraValidatorList read GetInnerValidators;
    property IsValid: Boolean read GetAsBoolean write SetAsBoolean;
  end;

implementation

uses
  List_Validator;
  
{ TValidationService }

function TValidationService.GetValidatorFor(
  const InfraType: IInfraType): IInfraValidator;
var
  InjectedList: IInfraInjectedInterfaceList;
  i: integer;
begin
  InjectedList := (InfraType as IInfraInjectedInterfaceList);
  i := InjectedList.IndexOf(IInfraValidator);
  if i < 0 then
  begin
    Result := TValidator(InfraType.TypeInfo.FindValidator).Create;
    InjectedList.Add(IInfraValidator, Result);
  end else
    Result := (InjectedList[i] as IInfraValidator);
end;

{ TValidator }

procedure TValidator.Calculate;
var
  i: integer;
begin
  if Assigned(FInnerValidators) then
  begin
    IsValid := True;
    for i := 0 to FInnerValidators.Count-1 do
    begin
      IsValid := IsValid and
        FInnerValidators[i].IsValid;
    end;
  end;
end;

function TValidator.GetConstrainedObject: IInfraType;
begin
  Result := FConstrainedObject;
end;

function TValidator.GetInnerValidators: IInfraValidatorList;
begin
  if not Assigned(FInnerValidators) then
    FInnerValidators := TValidatorList.Create(self);
  Result := FInnerValidators;
end;

procedure TValidator.InvalidateCache;
begin
  inherited;
  FOwner.InvalidateCache;
end;

procedure TValidator.SetConstrainedObject(const Value: IInfraType);
begin
  if Value <> FConstrainedObject then
    FConstrainedObject := Value;
end;

procedure InjectValidatorService;
begin
  with (ApplicationContext as IMemoryManagedObject).InjectedInterfaces do
    Add(IInfraValidationService,
      TValidationService.Create as IInfraValidationService);
end;

initialization
  InjectValidatorService;

end.
