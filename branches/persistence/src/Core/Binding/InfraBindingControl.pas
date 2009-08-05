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

  TBindableCustomListItems = class(TBindableRTTIBasedTwoWay)
  private
    FListModel: IBindableListModel;
    function FindObjectNoListBox(const pObject: IInfraType): integer;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure AddItemToControl(const pValue: string; const pObject: IInfraType);
    procedure RemoveItemOfControl(const pObject: IInfraType);
    procedure FillControl(const pListModel: IBindableListModel);
    procedure SetValue(const Value: IInfraType); override;
  public
    class function CreateIfSupports(pControl: TControl;
      const pPropertyPath: string): IBindableVCLProperty; override;
  end;

  TBindableItemIndex = class(TBindableVCLPropertyTwoWay)
  private
    //FListModel: IBindableListModel;
  protected
    procedure WndProc(var Message: TMessage); override;
    function GetValue: IInfraType; override;
    procedure SetValue(const Value: IInfraType); override;
  public
    class function CreateIfSupports(pControl: TControl;
      const pPropertyPath: string): IBindableVCLProperty; override;
    //constructor Create(pControl: TControl); reintroduce;
  end;

procedure RegisterBindableClass(pBindableClass: TBindableVCLPropertyClass);
function GetBindableVCL(pControl: TControl;
  const pExpression: string): IBindable;

implementation

uses
  Classes,
  SysUtils,
  StdCtrls,
  InfraValueType,
  InfraBindingConsts,
  InfraBindingConverter;

var
  _BindableClasses: TList;

procedure RegisterBindableClass(pBindableClass: TBindableVCLPropertyClass);
begin
  if not Assigned(_BindableClasses) then
    _BindableClasses := TList.Create;
  _BindableClasses.Add(pBindableClass);
end;

function GetBindableVCL(pControl: TControl;
  const pExpression: string): IBindable;
var
  vI: Integer;
begin
  for vI := 0 to _BindableClasses.Count - 1 do
  begin
    Result := TBindableVCLPropertyClass(_BindableClasses[vI]).CreateIfSupports(
      pControl, pExpression) as IBindable;
    if Assigned(Result) then
      Break;
  end;
  if not Assigned(Result) then
    raise EInfraBindingError.CreateFmt(cErrorBindableNotDefined,
      [pControl.ClassName, pExpression]);
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

// *** ver uma forma de nao precisar ficar criando o type a cada getvalue

function TBindableRTTIBased.GetValue: IInfraType;
var
  vObject: TObject;
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
    tkClass:
      begin
        vObject := GetObjectProp(FControl, FPropInfo);
        if vObject is TStrings then
          Result := TInfraString.NewFrom(TStrings(vObject).Text);
      end;
  end;
end;

procedure TBindableRTTIBased.SetValue(const Value: IInfraType);
var
  vObject: TObject;
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
    tkClass:
      begin
        vObject := GetObjectProp(FControl, FPropInfo);
        if vObject is TStrings then
          TStrings(vObject).Text := (Value as IInfraString).AsString;
      end;
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
  if AnsiSameText(pPropertyPath, 'Text') then
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
    Result := inherited CreateIfSupports(pControl, pPropertyPath)
  else
    Result := nil;
end;

procedure TBindableCustomListItems.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if not Assigned(FListModel) then
    case Message.Msg of
      LB_ADDSTRING, LB_DELETESTRING, LB_RESETCONTENT: Changed;
    end;
end;

procedure TBindableCustomListItems.SetValue(const Value: IInfraType);
var
  vListModel: IBindableListModel;
begin
  if Assigned(FListModel)
    and Supports(Value, IBindableListModel, vListModel) then
  begin
    case vListModel.Operation of
      loAdd: AddItemToControl(
        vListModel.GetValueOfExpression(vListModel.ItemOperated),
        vListModel.ItemOperated);
      loRemove: RemoveItemOfControl(vListModel.ItemOperated);
      loRefresh: FillControl(vListModel);
      loClear: TCustomListControl(Control).Clear;
    end;
  end else
    inherited SetValue(Value);
end;

procedure TBindableCustomListItems.AddItemToControl(const pValue: string;
  const pObject: IInfraType);
begin
  TCustomListControl(Control).AddItem(pValue, TObject(Pointer(pObject)));
end;

procedure TBindableCustomListItems.RemoveItemOfControl(
  const pObject: IInfraType);
begin
  TCustomListBox(Control).Items.Delete(FindObjectNoListBox(pObject));
end;

procedure TBindableCustomListItems.FillControl(
  const pListModel: IBindableListModel);
var
  vI: Integer;
  vList: IInfraList;
begin
  if Supports(pListModel.List, IInfraList, vList) then
    for vI := 0 to vList.Count-1 do
      AddItemToControl(pListModel.GetValueOfExpression(vList.Items[vI]),
        vList.Items[vI]);
end;

function TBindableCustomListItems.FindObjectNoListBox(
  const pObject: IInfraType): integer;
begin
  Result := TCustomListBox(Control).Items.IndexOfObject(TObject(Pointer(pObject)));
end;

{ TBindableItemindex }

class function TBindableItemIndex.CreateIfSupports(pControl: TControl;
  const pPropertyPath: string): IBindableVCLProperty;
begin
  if (pControl is TCustomListControl)
    and AnsiSameText(pPropertyPath, 'ItemIndex') then
    Result := inherited Create(pControl)
  else
    Result := nil;
end;

//constructor TBindableItemIndex.Create(pControl: TControl);
//begin
//  inherited Create(pControl);
//  // *** rever isso
//  {
//  FListType := TVCLListType.Create;
//  FListType.Control := pControl;
//  FListType.ItemIndex := TCustomListBox(pControl).ItemIndex;
//  if FListType.ItemIndex <> -1 then
//  begin
//    FListType.Operation := loSelectionChange;
//    FListType.ItemText := TCustomListBox(pControl).Items[FListType.ItemIndex];
//  end else
//  begin
//    FListType.Operation := loNone;
//    FListType.ItemText := '';
//  end;
//  }
//end;

function TBindableItemIndex.GetValue: IInfraType;
begin
  Result := TInfraInteger.NewFrom(TCustomListControl(Control).ItemIndex);
end;

// *** rever isso

procedure TBindableItemIndex.SetValue(const Value: IInfraType);
var
  //  vListType: IVCLListType;
  vInteger: IInfraInteger;
begin
  if Supports(Value, IInfraInteger, vInteger) then
    TCustomListControl(Control).ItemIndex := vInteger.AsInteger;
  {
  if Supports(Value, IVCLListType, vListType)
    and (vListType.Operation = loSelectionChange) then
  begin
    if vListType.ItemIndex <> -1 then
      TCustomListBox(Control).ItemIndex := vListType.ItemIndex
    else if vListType.ItemText <> '' then
      TCustomListBox(Control).ItemIndex :=
        TCustomListBox(Control).Items.IndexOf(vListType.ItemText);
  end;
  }
end;

procedure TBindableItemIndex.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if (TWMCommand(Message).NotifyCode = LBN_SELCHANGE)
    or (Message.Msg = LB_SETCURSEL) then
  begin
    // *** rever isso
    {
    FListType.Operation := loSelectionChange;
    FListType.ItemIndex := TCustomListControl(FControl).ItemIndex;
    FListType.ItemText := TCustomListControl(FControl). Items[FListType.ItemIndex];
    }
    Changed;
  end;
end;

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
  RegisterBindableClass(TBindableItemIndex);
end;

initialization
  RegisterBindables;

finalization
  if Assigned(_BindableClasses) then
    FreeAndNil(_BindableClasses);

end.
