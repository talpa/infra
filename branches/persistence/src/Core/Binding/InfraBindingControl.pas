unit InfraBindingControl;

interface

uses
  InfraCommon, InfraBindingIntf, TypInfo, Controls, InfraValueTypeIntf;

type
  TPropertyAccessMode = (paRTTI, paCustom);

  TBindableControl = class(TBaseElement, IBindableControl)
  private
    FPropInfo: PPropInfo;
    FControl: TControl;
    FPropertyPath: String;
    FPropertyAccessMode: TPropertyAccessMode;
    function GetPropertyPath: String;
    function GetValue: IInfraType;
    procedure SetValue(const Value: IInfraType);
    function GetValueByRTTI: IInfraType;
    procedure SetValueByRTTI(const Value: IInfraType);
    function SupportPropertyByRTTI(const PropertyPath: String): Boolean;
  protected
    function SupportCustomProperty(const PropertyPath: String): Boolean; virtual;
    function GetCustomValue: IInfraType; virtual; abstract;
    procedure CustomSetValue(const Value: IInfraType); virtual; abstract;
  protected
    constructor Create(Control: TControl; const PropertyPath: String); reintroduce;
  end;

  TBindableEdit = class(TBindableControl)
    // implementar para TEdit
  end;

implementation

uses
  InfraValueType, SysUtils, InfraBindingConsts;

{ TBindableControl }

constructor TBindableControl.Create(Control: TControl;
  const PropertyPath: String);
begin
  inherited Create;
  FControl := Control;
  FPropertyPath := PropertyPath;
  if SupportCustomProperty(PropertyPath) then
    FPropertyAccessMode := paCustom
  else if SupportPropertyByRTTI(PropertyPath) then
    FPropertyAccessMode := paRTTI
  else
    Raise EInfraBindingError.CreateFmt(
      cErrorBindingProprtyNotExists, [FPropertyPath]);
end;

function TBindableControl.SupportCustomProperty(
  const PropertyPath: String): Boolean;
begin
  Result := False;
end;

function TBindableControl.GetPropertyPath: String;
begin
  Result := FPropertyPath;
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

function TBindableControl.SupportPropertyByRTTI(
  const PropertyPath: String): Boolean;
begin
  FPropInfo := GetPropInfo(FControl, FPropertyPath);
  Result := Assigned(FPropInfo);
end;

end.
