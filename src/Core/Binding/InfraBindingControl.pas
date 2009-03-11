unit InfraBindingControl;

interface

uses
  Controls,
  TypInfo,
  InfraCommon,
  Messages,
  InfraBindingIntf,
  InfraBindingType,
  InfraValueTypeIntf;

type
  TPropertyAccessMode = (paRTTI, paCustom);

  TBindableControl = class(TBindable, IBindableControl)
  private
    FPropInfo: PPropInfo;
    FControl: TControl;
    FPropertyPath: String;
    FPropertyAccessMode: TPropertyAccessMode;
    FOldWndProc: TWndMethod;
    function GetValueByRTTI: IInfraType;
    procedure SetValueByRTTI(const Value: IInfraType);
    function SupportPropertyByRTTI(const PropertyPath: String): Boolean; virtual;
  protected
    procedure WindowProc(var Message: TMessage); virtual;    function GetValue: IInfraType; override;
    procedure SetValue(const Value: IInfraType); override;
    function SupportCustomProperty(const PropertyPath: String): Boolean; virtual;
    function GetCustomValue: IInfraType; virtual; abstract;
    procedure CustomSetValue(const Value: IInfraType); virtual; abstract;
  public
    class function GetBindable(pControl: TControl;
      const pPropertyPath: string): IBindable; virtual; abstract;
    constructor Create(pControl: TControl; const pPropertyPath: string); reintroduce;
  end;

  TBindableClass = class of TBindable;
  TBindableControlClass = class of TBindableControl;

  TBindableEdit = class(TBindableControl)
  protected
    function SupportPropertyByRTTI(const PropertyPath: String): Boolean; override;
    procedure WindowProc(var Message: TMessage); override;
  end;

implementation

uses
  SysUtils,
  InfraValueType,
  InfraBindingConsts,
  InfraCommonIntf, InfraBindingManager;

{ TBindableControl }

constructor TBindableControl.Create(pControl: TControl;
  const pPropertyPath: string);
begin
  inherited Create;
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

procedure TBindableControl.WindowProc(var Message: TMessage);
begin
  FOldWndProc(Message);
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

{ TBindableEdit }

function TBindableEdit.SupportPropertyByRTTI(
  const PropertyPath: String): Boolean;
begin
  Result := AnsiSameText(PropertyPath, 'Text');
end;

procedure TBindableEdit.WindowProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = WM_SETTEXT then
    Publisher.Publish(TBindableValueChanged.Create(Self) as IBindableValueChanged);
end;

end.
