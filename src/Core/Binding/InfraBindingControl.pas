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

  TBindableControlProperty = class(TBindable, IBindableControlProperty)
  private
    FControl: TControl;
    FOldWndProc: TWndMethod;
  protected
    procedure WndProc(var Message: TMessage); virtual;
  public
    class function CreateIfSupports(Control: TControl; const PropertyPath: String): TBindableControlProperty; virtual; abstract;
    constructor Create(Control: TControl); reintroduce;
    destructor Destroy; override;
  end;

  TRttiBasedBindableControlProperty = class(TBindableControlProperty)
  private
    FPropInfo: PPropInfo;
  protected
    function GetValue: IInfraType; override;
    procedure SetValue(const Value: IInfraType); override;
    function Support2Way: Boolean; override;
  public
    class function CreateIfSupports(Control: TControl;
      const PropertyPath: string): TBindableControlProperty; override;
    constructor Create(Control: TControl; PropInfo: PPropInfo); reintroduce;
  end;

  TTwoWayRttiBasedBindableControlProperty = class(TBindableControlProperty)
  protected
    function Support2Way: Boolean; override;
  end;

  TBindableEditText = class(TTwoWayRttiBasedBindableControlProperty)
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    class function CreateIfSupports(Control: TControl;
      const PropertyPath: String): TBindableControlProperty; override;
  end;

  TBindableControlVisible = class(TTwoWayRttiBasedBindableControlProperty)
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    class function CreateIfSupports(Control: TControl;
      const PropertyPath: String): TBindableControlProperty; override;
  end;

  TBindableControlEnabled = class(TTwoWayRttiBasedBindableControlProperty)
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    class function CreateIfSupports(Control: TControl;
      const PropertyPath: String): TBindableControlProperty; override;
  end;

implementation

uses
  SysUtils,
  InfraValueType,
  InfraBindingConsts,
  InfraCommonIntf,
  InfraBindingManager,
  StdCtrls;


{ TRttiBasedBindableControlProperty }

constructor TRttiBasedBindableControlProperty.Create(Control: TControl;
  PropInfo: PPropInfo);
begin
  inherited Create(Control);
  FPropInfo := PropInfo;
end;

class function TRttiBasedBindableControlProperty.CreateIfSupports(
  Control: TControl; const PropertyPath: String): TBindableControlProperty;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(Control, PropertyPath);
  if Assigned(PropInfo) then
    Result := Self.Create(Control, PropInfo)
  else
    Result := Nil;
end;

function TRttiBasedBindableControlProperty.GetValue: IInfraType;
begin
  case FPropInfo^.PropType^.Kind of
    tkString: Result := TInfraString.NewFrom(GetStrProp(FControl, FPropInfo));    
  end;
end;

procedure TRttiBasedBindableControlProperty.SetValue(const Value: IInfraType);
begin
  case FPropInfo^.PropType^.Kind of
    tkString: SetStrProp(FControl, FPropInfo, (Value as IInfraString).AsString);    
  end;
end;

function TRttiBasedBindableControlProperty.Support2Way: Boolean;
begin
  Result := False;
end;

{ TBindableEditText }

class function TBindableEditText.CreateIfSupports(Control: TControl;
  const PropertyPath: String): TBindableControlProperty;
begin
  if (Control is TEdit) and AnsiSameText(PropertyPath, 'Text') then
    Result := inherited CreateIfSupports(Control, PropertyPath)
  else
    Result := Nil;
end;

procedure TBindableEditText.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = WM_KILLFOCUS then
    Changed;
end;

{ TBindableControlProperty }

constructor TBindableControlProperty.Create(Control: TControl);
begin
  inherited Create;
  FControl := Control;
  FOldWndProc := FControl.WindowProc;
  FControl.WindowProc := WndProc;  
end;

destructor TBindableControlProperty.Destroy;
begin
  FControl.WindowProc := FOldWndProc;
  inherited;
end;

procedure TBindableControlProperty.WndProc(var Message: TMessage);
begin
  FOldWndProc(Message);
end;

{ TTwoWayRttiBasedBindableControlProperty }

function TTwoWayRttiBasedBindableControlProperty.Support2Way: Boolean;
begin
  Result := True;
end;

{ TBindableControlVisible }

class function TBindableControlVisible.CreateIfSupports(Control: TControl;
  const PropertyPath: String): TBindableControlProperty;
begin
  if AnsiSameText(PropertyPath, 'Visible') then
    Result := inherited CreateIfSupports(Control, PropertyPath)
  else
    Result := Nil;
end;

procedure TBindableControlVisible.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = CM_VISIBLECHANGED then
    Changed;
end;

{ TBindableControlEnabled }

class function TBindableControlEnabled.CreateIfSupports(Control: TControl;
  const PropertyPath: String): TBindableControlProperty;
begin
  if AnsiSameText(PropertyPath, 'Enabled') then
    Result := inherited CreateIfSupports(Control, PropertyPath)
  else
    Result := Nil;
end;

procedure TBindableControlEnabled.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = CM_ENABLEDCHANGED then
    Changed;
end;

end.

