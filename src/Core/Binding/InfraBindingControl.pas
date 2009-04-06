unit InfraBindingControl;

interface

uses
  Controls,
  TypInfo,
  Messages,
  InfraBindingIntf,
  InfraBinding,
  InfraValueTypeIntf;

type
  TBindableVCLPropertyClass = class of TBindableVCLProperty;

  // classe base para bindables de propriedades da vcl
  TBindableVCLProperty = class(TBindable, IBindableVCLProperty)
  private
    FControl: TControl;
    FOldWndProc: TWndMethod;
  protected
    procedure WndProc(var Message: TMessage); virtual;
    function GetControl: TControl;
    property Control: TControl read GetControl;
  public
    class function CreateIfSupports(pControl: TControl;
      const pPropertyPath: string): IBindableVCLProperty; virtual; abstract;
    constructor Create(pControl: TControl); reintroduce;
    destructor Destroy; override;
  end;

  TBindableVCLPropertyTwoWay = class(TBindableVCLProperty)
  protected
    function Support2Way: Boolean; override;
  end;

  // classe base para bindables de propriedades da vcl
  TBindableRTTIBased = class(TBindableVCLProperty)
  private
    FPropInfo: PPropInfo;
  protected
    function GetValue: IInfraType; override;
    procedure SetValue(const Value: IInfraType); override;
  public
    class function CreateIfSupports(pControl: TControl;
      const pPropertyPath: string): IBindableVCLProperty; override;
    constructor Create(pControl: TControl; pPropInfo: PPropInfo); reintroduce;
  end;

  TBindableRTTIBasedTwoWay = class(TBindableRTTIBased)
  protected
    function Support2Way: Boolean; override;
  end;

  TBindableCaption = class(TBindableRTTIBased)
  public
    class function CreateIfSupports(pControl: TControl;
      const pPropertyPath: string): IBindableVCLProperty; override;
  end;

  TBindableText = class(TBindableRTTIBasedTwoWay)
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    class function CreateIfSupports(pControl: TControl;
      const pPropertyPath: string): IBindableVCLProperty; override;
  end;

  TBindableTextLines = class(TBindableRTTIBasedTwoWay)
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    class function CreateIfSupports(pControl: TControl;
      const pPropertyPath: string): IBindableVCLProperty; override;
  end;

  TBindableVisible = class(TBindableRTTIBasedTwoWay)
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    class function CreateIfSupports(pControl: TControl;
      const pPropertyPath: string): IBindableVCLProperty; override;
  end;

  TBindableEnabled = class(TBindableRTTIBasedTwoWay)
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    class function CreateIfSupports(pControl: TControl;
      const pPropertyPath: string): IBindableVCLProperty; override;
  end;

  TBindableChecked = class(TBindableRTTIBasedTwoWay)
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    class function CreateIfSupports(pControl: TControl;
      const pPropertyPath: string): IBindableVCLProperty; override;
  end;

  TBindableColor = class(TBindableRTTIBasedTwoWay)
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    class function CreateIfSupports(pControl: TControl;
      const pPropertyPath: string): IBindableVCLProperty; override;
  end;

  // classe base para bindables de propriedades da vcl
  TBindableCustomListItems = class(TBindableVCLPropertyTwoWay)
  private
    FListType: IVCLListType;
  protected
    procedure WndProc(var Message: TMessage); override;
    function GetValue: IInfraType; override;
    procedure SetValue(const Value: IInfraType); override;
  public
    class function CreateIfSupports(pControl: TControl;
      const pPropertyPath: string): IBindableVCLProperty; override;
    constructor Create(pControl: TControl); reintroduce;
  end;

  {
  TBindableItemIndex  = class(TBindableVCLPropertyTwoWay)
  protected
    procedure WndProc(var Message: TMessage); override;
    function GetValue: IInfraType; override;
    procedure SetValue(const Value: IInfraType); override;
  public
    class function CreateIfSupports(pControl: TControl;
      const pPropertyPath: string): IBindableVCLProperty; override;
  end;
  }
  
procedure RegisterBindableClass(pBindableClass: TBindableVCLPropertyClass);
function GetBindableVCL(pControl: TControl;
  const pPropertyPath: string): IBindable;

implementation

uses
  Classes,
  SysUtils,
  StdCtrls,
  InfraValueType,
  InfraBindingConsts, InfraBindingConverter;

var
  _BindableClasses: TList;

procedure RegisterBindableClass(pBindableClass: TBindableVCLPropertyClass);
begin
  if not Assigned(_BindableClasses) then
    _BindableClasses := TList.Create;
  _BindableClasses.Add(pBindableClass);
end;

function GetBindableVCL(pControl: TControl;
  const pPropertyPath: string): IBindable;
var
  vI: Integer;
begin
  for vI := 0 to _BindableClasses.Count - 1 do
  begin
    Result := TBindableVCLPropertyClass(_BindableClasses[vI]).CreateIfSupports(
      pControl, pPropertyPath) as IBindable;
    if Assigned(Result) then
      Break;
  end;
  if not Assigned(Result) then
    raise EInfraBindingError.CreateFmt(cErrorBindableNotDefined,
      [pControl.ClassName, pPropertyPath]);
end;

{ TBindableVCLProperty }

constructor TBindableVCLProperty.Create(pControl: TControl);
begin
  inherited Create;
  FControl := pControl;
  FOldWndProc := FControl.WindowProc;
  FControl.WindowProc := WndProc;
end;

destructor TBindableVCLProperty.Destroy;
begin
  FControl.WindowProc := FOldWndProc;
  inherited;
end;

function TBindableVCLProperty.GetControl: TControl;
begin
  Result := FControl;
end;

procedure TBindableVCLProperty.WndProc(var Message: TMessage);
begin
  FOldWndProc(Message);
end;

{ TBindableVCLPropertyTwoWay }

function TBindableVCLPropertyTwoWay.Support2Way: Boolean;
begin
  Result := True;
end;

{ TBindableRTTIBased }

class function TBindableRTTIBased.CreateIfSupports(
  pControl: TControl; const pPropertyPath: string): IBindableVCLProperty;
var
  vPropInfo: PPropInfo;
begin
  vPropInfo := GetPropInfo(pControl, pPropertyPath);
  if Assigned(vPropInfo) then
    Result := Self.Create(pControl, vPropInfo)
  else
    Result := nil;
end;

constructor TBindableRTTIBased.Create(pControl: TControl;
  pPropInfo: PPropInfo);
begin
  inherited Create(pControl);
  FPropInfo := pPropInfo;
end;

function TBindableRTTIBased.GetValue: IInfraType;
begin
  case FPropInfo^.PropType^.Kind of
    tkLString, tkString:
      Result := TInfraString.NewFrom(GetStrProp(FControl, FPropInfo));
    tkEnumeration:
      if GetTypeData(FPropInfo^.PropType^)^.BaseType^ = System.TypeInfo(Boolean) then
        Result := TInfraBoolean.NewFrom(Boolean(GetOrdProp(FControl, FPropInfo)))
      else
        Result := TInfraInteger.NewFrom(GetOrdProp(FControl, FPropInfo));
    tkInteger, tkChar, tkWChar:
      Result := TInfraInteger.NewFrom(GetOrdProp(FControl, FPropInfo));
//    tkClass:
//      if GetObjectProp(FControl, FPropInfo) is TStrings then
//        Result := TInfraString.NewFrom((GetObjectProp(FControl, FPropInfo) as TStrings).Text);
  end;
end;

procedure TBindableRTTIBased.SetValue(const Value: IInfraType);
begin
  case FPropInfo^.PropType^.Kind of
    tkLString, tkString:
      SetStrProp(FControl, FPropInfo, (Value as IInfraString).AsString);
    tkEnumeration:
      if Supports(Value, IInfraBoolean) then
        SetOrdProp(FControl, FPropInfo,
          Abs(Trunc(Integer((Value as IInfraBoolean).AsBoolean))))
      else
        SetOrdProp(FControl, FPropInfo,
          Trunc((Value as IInfraInteger).AsInteger));
    tkInteger, tkChar, tkWChar:
      SetOrdProp(FControl, FPropInfo,
        Trunc((Value as IInfraInteger).AsInteger));
//    tkClass:
//      if GetObjectProp(FControl, FPropInfo) is TStrings then
//        (GetObjectProp(FControl, FPropInfo) as TStrings).Text := (Value as IInfraString).AsString;
  end;
end;

{ TBindableRTTIBasedTwoWay }

function TBindableRTTIBasedTwoWay.Support2Way: Boolean;
begin
  Result := True;
end;

{ TBindableText }

class function TBindableText.CreateIfSupports(pControl: TControl;
  const pPropertyPath: string): IBindableVCLProperty;
begin
  if (pControl is TCustomEdit) and AnsiSameText(pPropertyPath, 'Text') then
    Result := inherited CreateIfSupports(pControl, pPropertyPath)
  else
    Result := nil;
end;

procedure TBindableText.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if Message.Msg in [WM_KILLFOCUS, WM_SETTEXT] then
    Changed;
end;

{ TBindableTextLines }

class function TBindableTextLines.CreateIfSupports(pControl: TControl;
  const pPropertyPath: string): IBindableVCLProperty;
begin
  if (pControl is TCustomMemo) and AnsiSameText(pPropertyPath, 'Lines') then
    Result := inherited CreateIfSupports(pControl, pPropertyPath)
  else
    Result := nil;
end;

procedure TBindableTextLines.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if Message.Msg in [WM_KILLFOCUS, WM_SETTEXT] then
    Changed;
end;

{ TBindableCaption }

class function TBindableCaption.CreateIfSupports(pControl: TControl;
  const pPropertyPath: string): IBindableVCLProperty;
begin
  if AnsiSameText(pPropertyPath, 'Caption') then
    Result := inherited CreateIfSupports(pControl, pPropertyPath)
  else
    Result := nil;
end;

{ TBindableVisible }

class function TBindableVisible.CreateIfSupports(pControl: TControl;
  const pPropertyPath: string): IBindableVCLProperty;
begin
  if AnsiSameText(pPropertyPath, 'Visible') then
    Result := inherited CreateIfSupports(pControl, pPropertyPath)
  else
    Result := nil;
end;

procedure TBindableVisible.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if Message.Msg = CM_VISIBLECHANGED then
    Changed;
end;

{ TBindableEnabled }

class function TBindableEnabled.CreateIfSupports(pControl: TControl;
  const pPropertyPath: string): IBindableVCLProperty;
begin
  if AnsiSameText(pPropertyPath, 'Enabled') then
    Result := inherited CreateIfSupports(pControl, pPropertyPath)
  else
    Result := nil;
end;

procedure TBindableEnabled.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if Message.Msg = CM_ENABLEDCHANGED then
    Changed;
end;

{ TBindableChecked }

class function TBindableChecked.CreateIfSupports(pControl: TControl;
  const pPropertyPath: string): IBindableVCLProperty;
begin
  if AnsiSameText(pPropertyPath, 'Checked') then
    Result := inherited CreateIfSupports(pControl, pPropertyPath)
  else
    Result := nil;
end;

procedure TBindableChecked.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if Message.Msg = BM_SETCHECK then
    Changed;
end;

{ TBindableColor }

class function TBindableColor.CreateIfSupports(pControl: TControl;
  const pPropertyPath: string): IBindableVCLProperty;
begin
  if AnsiSameText(pPropertyPath, 'Color') then
    Result := inherited CreateIfSupports(pControl, pPropertyPath)
  else
    Result := nil;
end;

procedure TBindableColor.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if Message.Msg = CM_COLORCHANGED then
    Changed;
end;

{ TBindableCustomListItems }

class function TBindableCustomListItems.CreateIfSupports(
  pControl: TControl; const pPropertyPath: string): IBindableVCLProperty;
begin
  if (pControl is TCustomListBox) and AnsiSameText(pPropertyPath, 'Items') then
    Result := Self.Create(pControl)
  else
    Result := nil;
end;

constructor TBindableCustomListItems.Create(pControl: TControl);
begin
  inherited Create(pControl);
  FListType := TVCLListType.Create;
  FListType.Control := pControl;
  if TCustomListBox(pControl).Items.Count <> 0 then
  begin
    FListType.Operation := loRefresh;
    FListType.ItemText := TCustomListBox(pControl).Items.Text;
  end else
  begin
    FListType.Operation := loNone;
    FListType.ItemText := '';
  end;
end;

procedure TBindableCustomListItems.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if (Message.Msg = LB_DELETESTRING)
    or (Message.Msg = LB_ADDSTRING)
    or (Message.Msg = LB_RESETCONTENT) then
  begin
    case Message.Msg of
      // Add -> SendMessage(ListBox.Handle, LB_ADDSTRING, 0, Longint(PChar(S)));
      LB_ADDSTRING:
      begin
        FListType.Operation := loAdd;
        FListType.ItemText := PChar(Message.lParam);
      end;
      // Delete -> SendMessage(Handle, LB_DELETESTRING, Index, 0);
      LB_DELETESTRING:
      begin
        FListType.Operation := loRemove;
        FListType.ItemIndex := Message.WParam;
      end;
      // Clear -> SendMessage(Handle, LB_RESETCONTENT, 0, 0);
      LB_RESETCONTENT: FListType.Operation := loClear;
      // PutObject -> SendMessage(Handle, LB_SETITEMDATA, Index, AData);
      // *** LB_SETITEMDATA: FListType.Operation := loPutObject;
    end;
    Changed;
  end;
end;

function TBindableCustomListItems.GetValue: IInfraType;
begin
  Result := FListType;
end;

procedure TBindableCustomListItems.SetValue(const Value: IInfraType);
var
  vListType: IVCLListType;
begin
  if Supports(Value, IVCLListType, vListType) then
  begin
    case vListType.Operation of
      loAdd: TCustomListBox(Control).AddItem(vListType.ItemText, nil);
      loRemove: TCustomListBox(Control).Items.Delete(vListType.ItemIndex);
      loRefresh: TCustomListBox(Control).Items.Assign(
        TCustomListBox(vListType.Control).Items);
      loClear: TCustomListBox(Control).Clear;
    end;
  end;
end;

{ TBindableItemindex }
{
class function TBindableItemindex.CreateIfSupports(pControl: TControl;
  const pPropertyPath: string): IBindableVCLProperty;
begin
  if (pControl is TCustomListBox) and
    AnsiSameText(pPropertyPath, 'ItemIndex') then
    Result := inherited Create(pControl)
  else
    Result := nil;
end;

function TBindableItemIndex.GetValue: IInfraType;
begin
  Result := TInfraInteger.NewFrom(TCustomListControl(FControl).ItemIndex);
end;

procedure TBindableItemIndex.SetValue(const Value: IInfraType);
var
  vInteger: IInfraInteger;
begin
  if not Supports(Value, IInfraInteger, vInteger) then
    raise EInfraBindingError.CreateFmt(cErrorBindableValueNotsupported,
      [FControl.ClassName, 'ItemIndex', 'IInfraInteger']);
  TCustomListControl(FControl).ItemIndex := vInteger.AsInteger;
end;

procedure TBindableItemindex.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if (TWMCommand(Message).NotifyCode = LBN_SELCHANGE)
    or (Message.Msg = LB_SETCURSEL) then
    Changed;
end;
}

procedure RegisterBindables;
begin
  RegisterBindableClass(TBindableText);
  RegisterBindableClass(TBindableTextLines);
  RegisterBindableClass(TBindableVisible);
  RegisterBindableClass(TBindableEnabled);
  RegisterBindableClass(TBindableCaption);
  RegisterBindableClass(TBindableChecked);
  RegisterBindableClass(TBindableColor);
  RegisterBindableClass(TBindableCustomListItems);
  // *** RegisterBindableClass(TBindableItemindex);
end;

initialization
  RegisterBindables;

finalization
  if Assigned(_BindableClasses) then
    FreeAndNil(_BindableClasses);

end.
