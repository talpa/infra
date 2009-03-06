unit InfraControlBinding;

interface

uses
  Controls, InfraBindingIntf, InfraCommon, InfraValueTypeIntf;

type
  TPropertyAccessMode = (paRTTI, paCustom);

  TBindableControl = class abstract(TBaseElement, IBindableControl)
  private
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
    function CustomSupportProperty(const PropertyPath: String): Boolean; virtual;
    function CustomGetValue: IInfraType; virtual; abstract;
    procedure CustomSetValue(const Value: IInfraType); virtual; abstract;
  protected
    constructor Create(Control: TControl; const PropertyPath: String); reintroduce;
  end;

implementation

uses
  SysUtils, TypInfo, InfraValueType;

{ TBindableControl }

constructor TBindableControl.Create(Control: TControl;
  const PropertyPath: String);
begin
  inherited Create;
  FControl := Control;
  FPropertyPath := PropertyPath;
  if SupportPropertyByRTTI(PropertyPath) then
    FPropertyAccessMode := paRTTI
  else if CustomSupportProperty(PropertyPath) then
    FPropertyAccessMode := paCustom
  else
  begin
  { TODO: Raise an exception if SelectBindablePropertyByPath returns False
    (Returning false means that this TBindableControl does not support the
    specified property path) }
  end;
end;

function TBindableControl.CustomSupportProperty(
  const PropertyPath: String): Boolean;
begin

end;

function TBindableControl.GetPropertyPath: String;
begin
  Result := FPropertyPath;
end;

function TBindableControl.GetValue: IInfraType;
begin
  case FPropertyAccessMode of
    paRTTI: Result := GetValueByRTTI;
    paCustom: Result := CustomGetValue;
  end;
end;

function TBindableControl.GetValueByRTTI: IInfraType;
var
  Info: PPropInfo;
begin
  Info := GetPropInfo(FControl, FPropertyPath);
  case Info^.PropType^.Kind of
    tkString: Result := TInfraString.NewFrom(GetStrProp(FControl, Info));
  end;
end;

procedure TBindableControl.SetValue(const Value: IInfraType);
begin
  case FPropertyAccessMode of
    paRTTI: SetValueByRTTI(Value);
    paCustom: CustomSetValue(Value);
  end;
end;

procedure TBindableControl.SetValueByRTTI(const Value: IInfraType);
var
  Info: PPropInfo;
begin
  Info := GetPropInfo(FControl, FPropertyPath);
  case Info^.PropType^.Kind of
    tkString: SetStrProp(FControl, Info, (Value as IInfraString).AsString);
  end;
end;

function TBindableControl.SupportPropertyByRTTI(
  const PropertyPath: String): Boolean;
begin

end;

end.
