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
    FDefaultWindowProc: TWndMethod;
    FTriggeringMessage: Cardinal;
    procedure VerifySupport(pPropertyPath:  string);
    function GetValueByRTTI: IInfraType;
    procedure SetValueByRTTI(const Value: IInfraType);
    function SupportPropertyByRTTI(const pPropertyPath: String): Boolean; virtual;
  protected
    procedure WindowProc(var Message: TMessage); virtual;
    function GetValue: IInfraType; override;
    procedure SetValue(const Value: IInfraType); override;
    function SupportCustomProperty(const PropertyPath: String): Boolean; virtual;
    function GetCustomValue: IInfraType; virtual; abstract;
    procedure CustomSetValue(const Value: IInfraType); virtual; abstract;
    function GetSupports2Way: Boolean; override;    
  public
    class function GetBindable(pControl: TControl;
      const pPropertyPath: string): IBindable; virtual; abstract;
    constructor Create(pControl: TControl; const pPropertyPath: string); reintroduce;
    destructor Destroy; override;
  end;

  TBindableControlClass = class of TBindableControl;

  TBindableEdit = class(TBindableControl)
  protected
    procedure WindowProc(var Message: TMessage); override;
    function GetSupports2Way: Boolean; override;
  end;

implementation

uses
  SysUtils,
  InfraValueType,
  InfraBindingConsts,
  InfraCommonIntf, InfraBindingManager;

{ TBindableControl }

procedure TBindableControl.VerifySupport(pPropertyPath: string);
begin
  if SupportCustomProperty(pPropertyPath)  then
    FPropertyAccessMode := paCustom
  else
  if SupportPropertyByRTTI(pPropertyPath)  then
    FPropertyAccessMode := paRTTI
  else
   Raise EInfraBindingError.CreateFmt(
     cErrorBindingProprtyNotExists, [pPropertyPath]);
end;

constructor TBindableControl.Create(pControl: TControl;
  const pPropertyPath: string);
begin
  inherited Create;
  FControl := pControl;
  FPropertyPath := pPropertyPath;
  VerifySupport(pPropertyPath);
  // captura mensagens do controle
  FDefaultWindowProc := FControl.WindowProc;
  FControl.WindowProc := WindowProc;
end;

destructor TBindableControl.Destroy;
begin
  FControl.WindowProc := FDefaultWindowProc;
  inherited;
end;

function TBindableControl.SupportPropertyByRTTI(
  const pPropertyPath: String): Boolean;
begin
  FPropInfo := GetPropInfo(FControl, pPropertyPath);
  Result := Assigned(FPropInfo);
end;

procedure TBindableControl.WindowProc(var Message: TMessage);
begin
  FDefaultWindowProc(Message);
  if Message.Msg <> FTriggeringMessage then
  begin
    FTriggeringMessage := Message.Msg;
    Dispatch(Message);
  end;
end;

function TBindableControl.SupportCustomProperty(
  const PropertyPath: String): Boolean;
begin
  Result := False;
end;

function TBindableControl.GetSupports2Way: Boolean;
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

function TBindableEdit.GetSupports2Way: Boolean;
begin
  inherited GetSupports2Way;
  if AnsiSameText(FPropertyPath, 'Text') then
    Result := True;  
end;

procedure TBindableEdit.WindowProc(var Message: TMessage);
begin
  inherited WindowProc(Message);
  // *** teria de testar o updatetriggermode aqui tambem
  if AnsiSameText(FPropertyPath, 'Text') and (Message.Msg = WM_KILLFOCUS) then
    Changed;     

end;

end.
