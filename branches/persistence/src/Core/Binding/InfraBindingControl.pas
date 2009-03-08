unit InfraBindingControl;

interface

uses
  InfraCommon, InfraBindingIntf, TypInfo, InfraValueTypeIntf, Controls;

type
  TPropertyAccessMode = (paRTTI, paCustom);

  TBindableControl = class(TElement, IBindableControl)
  private
    FPropInfo: PPropInfo;
    FControl: TControl;
    FPropertyPath: String;
    FPropertyAccessMode: TPropertyAccessMode;
    function GetValueByRTTI: IInfraType;
    procedure SetValueByRTTI(const Value: IInfraType);
    function SupportPropertyByRTTI(const PropertyPath: String): Boolean;
    function GetValue: IInfraType;
    procedure SetValue(const Value: IInfraType);
    procedure Initialize(pControl: TControl;
      const pPropertyPath: String); overload;
  protected
    function SupportCustomProperty(const PropertyPath: String): Boolean; virtual;
    function GetCustomValue: IInfraType; virtual; abstract;
    procedure CustomSetValue(const Value: IInfraType); virtual; abstract;
  end;

  TBindableEdit = class(TBindableControl)
    // implementar para TEdit
  end;

implementation

uses
  InfraValueType, SysUtils, InfraBindingConsts;

{ TBindableControl }

procedure TBindableControl.Initialize(pControl: TControl;
  const pPropertyPath: String);
begin
  FControl := pControl;
  FPropertyPath := pPropertyPath;
  if SupportCustomProperty(pPropertyPath) then
    FPropertyAccessMode := paCustom
  else if SupportPropertyByRTTI(pPropertyPath) then
    FPropertyAccessMode := paRTTI
  else
    Raise EInfraBindingError.CreateFmt(
      cErrorBindingProprtyNotExists, [pPropertyPath]);
end;

function TBindableControl.SupportPropertyByRTTI(
  const PropertyPath: String): Boolean;
begin
  FPropInfo := GetPropInfo(FControl, FPropertyPath);
  Result := Assigned(FPropInfo);
end;

function TBindableControl.SupportCustomProperty(
  const PropertyPath: String): Boolean;
begin
  Result := False;
end;

function TBindableControl.GetValue: IInfraType;
begin
  case FPropertyAccessMode of
    paRTTI: Result := GetValueByRTTI;
    paCustom: Result := GetCustomValue;
  end;
end;

procedure TBindableControl.SetValue(const Value: IInfraType);
begin
  case FPropertyAccessMode of
    paRTTI: SetValueByRTTI(Value);
    paCustom: CustomSetValue(Value);
  end;
end;

function TBindableControl.GetValueByRTTI: IInfraType;
begin
  case FPropInfo^.PropType^.Kind of
    tkString, tkLString, tkWString:
      Result := TInfraString.NewFrom(GetStrProp(FControl, FPropInfo));
  end;
end;

procedure TBindableControl.SetValueByRTTI(const Value: IInfraType);
begin
  case FPropInfo^.PropType^.Kind of
    tkString, tkLString, tkWString:
      SetStrProp(FControl, FPropInfo, (Value as IInfraString).AsString);
  end;
end;

end.
