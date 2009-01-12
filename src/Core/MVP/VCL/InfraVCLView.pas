unit InfraVCLView;

interface

uses
  InfraCommon,
  Forms,
  Controls,
  StdCtrls,
  Classes, 
  Types,
  Messages,
  Contnrs,
  InfraView,
  InfraFriendClasses,
  InfraNotify,
  InfraCommonIntf,
  InfraMVPIntf,
  InfraRenderer,
  InfraValueTypeIntf,
  InfraMVPVCLIntf;

type
  TMVPMapperService = class(TElement, IMVPMapperService)
  private
    FItems: TBucketList;
    FKeyInterceptor: IElement;
    FMouseInterceptor: IElement;
  protected
    function GetKeyInterceptor: IElement;
    function GetModelFromObject(Obj: TObject): IModel;
    function GetMouseInterceptor: IElement;
    procedure SetKeyInterceptor(const Value: IElement);
    procedure RegisterMVPItem(const Model: IModel; Obj: TObject);
    procedure RemoveMVPItem(Obj: TObject);
    procedure SetMouseInterceptor(const Value: IElement);
    property KeyInterceptor: IElement read GetKeyInterceptor
      write SetKeyInterceptor;
    property MouseInterceptor: IElement read GetMouseInterceptor
      write SetMouseInterceptor;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TSubViewsIterator = class(TElement, IInfraIterator)
  private
    FCurrentIndex: Integer;
    FList: IPresenterList;
  protected
    function CurrentItem: IInterface;
    procedure First;
    function IsDone: Boolean;
    procedure Next;
  public
    constructor Create(const List: IPresenterList); reintroduce;
  end;

  TVCLView = class(TView, IVCLView)
  private
    FVCLObject: TObject;
  protected
    function GetSubViewsIterator: IInfraIterator; virtual;
    function FindViewFromObject(aVCLObject: TObject): IView;
    property SubViewsIterator: IInfraIterator read GetSubViewsIterator;
  public
    destructor Destroy; override;
    function GetObject: TObject;
    procedure SetObject(Value: TObject); virtual;
    property VCLObject: TObject read GetObject write SetObject;
  end;

  TVCLControlView = class(TVCLView, IVCLControlView)
  private
    FControl: TControlFriend;
    FDefaultWindowProc: TWndMethod;
    function ApplyFilter(const Event: IInfraEvent): Boolean;
    procedure VCLFormDestroyEvent(const Event: IInfraEvent);
  protected
    FTriggeringMessage: Cardinal;
    function GetSubViewsIterator: IInfraIterator; override;
    function GetVisible: Boolean;
    procedure AssignWindowProc; virtual;
    procedure RestoreWindowProc; virtual;
    procedure WndProc(var Message: TMessage); virtual;
    procedure SetVisible(Value: Boolean);
    procedure OnClickHandle(Sender: TObject); virtual;
    procedure OnContextPopupHandle(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure OnDblClickHandle(Sender: TObject);
    procedure OnDragDropHandle(Sender, Source: TObject; X, Y: Integer);
    procedure OnDragOverHandle(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure OnEndDragHandle(Sender, Target: TObject; X, Y: Integer);
    procedure OnMouseDownHandle(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnMouseMoveHandle(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OnMouseUpHandle(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnStartDragHandle(Sender: TObject; var DragObject: TDragObject);
    procedure Update; override;
    procedure UpdateModel; override;
    property Visible: Boolean read GetVisible write SetVisible;
  public
    procedure SetObject(Value: TObject); override;
    procedure InfraInitInstance; override;
  end;

  TVCLWinControlView = class(TVCLControlView, IVCLWinControlView)
  private
    FWinControl: TWinControlFriend;
  protected
    procedure OnEnterHandle(Sender: TObject);
    procedure OnExitHandle(Sender: TObject);
    procedure OnKeyDownHandle(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OnKeyPressHandle(Sender: TObject; var Key: Char);
    procedure OnKeyUpHandle(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  public
    procedure SetObject(Value: TObject); override;
  end;

  TVCLCustomFormView = class(TVCLWinControlView,
    IVCLCustomFormView)
  private
    FCustomForm: TCustomFormFriend;
  protected
    procedure Close;
    procedure Hide;
    procedure Release;
    procedure Show;
    procedure ShowAndWait;
    procedure OnDestroyHandle(Sender: TObject);
    procedure OnCloseHandle(Sender: TObject; var Action: TCloseAction);
  public
    destructor Destroy; override;
    procedure SetObject(Value: TObject); override;
  end;

  TVCLCustomEditView = class(TVCLWinControlView, IVCLCustomEditView,
    ITextInterface)
  private
    FCustomEdit: TCustomEditFriend;
    function GetText: string;
  protected
    procedure OnChangeHandle(Sender: TObject);
    procedure Update; override;
    procedure UpdateModel; override;
    procedure EMSetSel(var Message: TMessage); message EM_SETSEL;
    property Text: string read GetText;
  public
    procedure SetObject(Value: TObject); override;
  end;

  TVCLEditView = class(TVCLCustomEditView, IVCLEditView);

  TVCLCustomLabelView = class(TVCLControlView,
    IVCLCustomLabelView)
  private
    FCustomLabel: TCustomLabel;
  protected
    function GetLabelText: string;
    procedure Update; override;
    property Caption: string read GetLabelText;
  public
    procedure SetObject(Value: TObject); override;
  end;

  TVCLCustomDateTimeView = class(TVCLWinControlView,
    IVCLCustomDateTimeView)
  private
    FDateTimePicker: TDateTimePickerFriend;
  protected
    procedure OnChangeHandle(Sender: TObject);
    function GetDateTime: TDateTime;
    procedure Update; override;
    procedure UpdateModel; override;
    property DateTime: TDateTime read GetDateTime;
  public
    procedure SetObject(Value: TObject); override;
  end;

  TVCLButtonControlView = class(TVCLWinControlView,
    IVCLButtonControlView);

  TVCLRadioButtonView = class(TVCLButtonControlView,
    IVCLRadioButtonView)
  private
    FRadioButton: TRadioButtonFriend;
    FChecked: IInfraBoolean;
    procedure RadioButtonCheckChanged(const Event: IInfraEvent);
  protected
    function GetChecked: IInfraBoolean;
    procedure Update; override;
    procedure UpdateModel; override;
    property Checked: IInfraBoolean read GetChecked;
  public
    procedure InfraInitInstance; override;
    procedure SetObject(Value: TObject); override;
  end;

  TVCLCustomCheckBoxView = class(TVCLButtonControlView,
    IVCLCustomCheckBoxView)
  private
    FCustomCheckBox: TCustomCheckBoxFriend;
    FState: IInfraBoolean;
    procedure CustomCheckBoxViewStateChanged(const Event: IInfraEvent);
  protected
    procedure Update; override;
    procedure UpdateModel; override;
  public
    procedure InfraInitInstance; override;
    function GetState: IInfraBoolean;
    property State: IInfraBoolean read GetState;
    procedure SetObject(Value: TObject); override;
  end;

  TVCLButtonView = class(TVCLButtonControlView, IVCLButtonView)
  private
    FButton: TButtonFriend;
    FClickCommand: ICommand;
    procedure ClickCommandEvent(const Event: IInfraEvent);
  protected
    function GetCancel: Boolean;
    function GetDefault: Boolean;
    function GetModalResult: Integer;
    procedure OnClickHandle(Sender: TObject); override;
    procedure SetCancel(Value: Boolean);
    procedure SetDefault(Value: Boolean);
    procedure SetModalResult(Value: Integer);
    procedure Update; override;
    property Cancel: Boolean read GetCancel write SetCancel;
    property Default: Boolean read GetDefault write SetDefault;
    property ModalResult: Integer read GetModalResult write SetModalResult;
  public
    procedure SetObject(Value: TObject); override;
    procedure InfraInitInstance; override;
    function GetClickCommand: ICommand;
    procedure SetClickCommand(const Value: ICommand);
    property ClickCommand: ICommand read GetClickCommand write SetClickCommand;
  end;

  TVCLCustomListBoxView = class(TVCLWinControlView,
    IVCLCustomListBoxView)
  private
    FCustomListBox: TCustomListBoxFriend;
    FUpdatingSelection: boolean;
    FListItemView: IVCLCustomListBoxItemView;
    FOwnerDraw: boolean;
    procedure AddItemEvent(const Event: IInfraEvent);
    procedure ListChangeEvent(const Event: IInfraEvent);
    procedure RemoveItemEvent(const Event: IInfraEvent);
    procedure MultiSelectChangeEvent(const Event: IInfraEvent);
    procedure ClearListEvent(const Event: IInfraEvent);
    function ChangedFilter(const Event: IInfraEvent): Boolean;
  protected
    {
    function Apply(const Event: IInfraEvent): Boolean; override;
    procedure Inform(const Event: IInfraEvent); override;
    }
    function GetCount: Integer;
    function GetItemIndex: Integer;
    function GetListItemView: IVCLCustomListBoxItemView;
    function GetOwnerDraw: boolean;
    function OnDataFindHandle(Control: TWinControl;
      FindString: String): Integer;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure SetOwnerDraw(const Value: boolean);
    procedure OnDataHandle(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure OnDataObjectHandle(Control: TWinControl; Index: Integer;
      var DataObject: TObject);
    procedure OnDrawItemHandle(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure OnMeasureItemHandle(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure SetCount(Value: Integer);
    procedure SetListItemView(const Value: IVCLCustomListBoxItemView);
    procedure Update; override;
    procedure UpdateModel; override;
    property Count: Integer read GetCount write SetCount;
    property ItemIndex: Integer read GetItemIndex;
    property ListItemView: IVCLCustomListBoxItemView read GetListItemView
      write SetListItemView;
    property OwnerDraw: boolean read GetOwnerDraw write SetOwnerDraw;
  public
    procedure SetObject(Value: TObject); override;
    procedure InfraInitInstance; override;
  end;

  TVCLCustomListBoxItemView = class(TView,
    IVCLCustomListBoxItemView)
  protected
    function GetFeatureName: string;
    function GetValueForIndex(Index: Integer): IInfraType;
    property FeatureName: string read GetFeatureName;
  public
    function GetRenderer: IRenderer; override;
  end;

  TVCLMenuView = class(TVCLControlView, IVCLMenuView)
  private
    FOwnerDraw: boolean;
    FItems: IVCLMenuItemView;
  protected
    function GetItems: IVCLMenuItemView;
    function GetOwnerDraw: boolean;
    procedure SetOwnerDraw(Value: boolean);
    property Items: IVCLMenuItemView read GetItems;
    property OwnerDraw: boolean read GetOwnerDraw
      write SetOwnerDraw;
  end;

implementation

uses
  SysUtils,
  InfraVCLEvents,
  InfraValueType,
  InfraVCLRenderer,
  List_VCLMenuItem;

{ TMVPMapperService }

constructor TMVPMapperService.Create;
begin
  FItems := TBucketList.Create;
end;

destructor TMVPMapperService.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TMVPMapperService.GetKeyInterceptor: IElement;
begin
  Result := FKeyInterceptor;
end;

function TMVPMapperService.GetModelFromObject(
  Obj: TObject): IModel;
begin
  Result := IModel(FItems[Pointer(Obj)]);
end;

function TMVPMapperService.GetMouseInterceptor: IElement;
begin
  Result := FMouseInterceptor;
end;

procedure TMVPMapperService.RegisterMVPItem(const Model: IModel;
  Obj: TObject);
begin
  if FItems.Exists(Pointer(Obj)) then
    FItems[Pointer(Obj)] := Pointer(Model)
  else
    FItems.Add(Pointer(Obj), Pointer(Model));
end;

procedure TMVPMapperService.RemoveMVPItem(Obj: TObject);
begin
  if FItems.Exists(Pointer(Obj)) then
    FItems.Remove(Pointer(Obj));
end;

procedure TMVPMapperService.SetKeyInterceptor(
  const Value: IElement);
begin
  FKeyInterceptor := Value;
end;

procedure TMVPMapperService.SetMouseInterceptor(
  const Value: IElement);
begin
  FMouseInterceptor := Value;
end;

{ TVCLControlView }

procedure TVCLControlView.InfraInitInstance;
begin
  inherited;
  EventService.Subscribe(IVCLFormDestroyEvent, Self as ISubscriber,
    VCLFormDestroyEvent, EmptyStr, ApplyFilter);
end;

function TVCLControlView.ApplyFilter(const Event: IInfraEvent): Boolean;
var
  Src: IVCLControlView;
begin
  Result := Assigned(VCLObject) and
    Supports(Event.Source, IVCLControlView, Src) and
    (FControl.Owner = Src.VCLObject);
end;

procedure TVCLControlView.VCLFormDestroyEvent(const Event: IInfraEvent);
begin
  SetObject(nil);
end;

procedure TVCLControlView.SetObject(Value: TObject);
begin
  if Assigned(VCLObject) then
  begin
    with FControl do
    begin
      OnClick := nil;
      OnContextPopup := nil;
      OnDblClick := nil;
      OnDragDrop := nil;
      OnDragOver := nil;
      OnEndDrag := nil;
      OnMouseDown := nil;
      OnMouseMove := nil;
      OnMouseUp := nil;
      OnStartDrag := nil;
    end;
    RestoreWindowProc;
  end;
  inherited SetObject(Value);
  FControl := TControlFriend(VCLObject);
  if Assigned(VCLObject) then
  begin
    AssignWindowProc;
    with FControl do
    begin
      OnClick := OnClickHandle;
      OnContextPopup := OnContextPopupHandle;
      OnDblClick := OnDblClickHandle;
      OnDragDrop := OnDragDropHandle;
      OnDragOver := OnDragOverHandle;
      OnEndDrag := OnEndDragHandle;
      OnMouseDown := OnMouseDownHandle;
      OnMouseMove := OnMouseMoveHandle;
      OnMouseUp := OnMouseUpHandle;
      OnStartDrag := OnStartDragHandle;
    end;
  end;
end;

procedure TVCLControlView.OnClickHandle(Sender: TObject);
begin
  Publisher.Publish(
    TVCLNotifyEvent.Create(Self, IClickEvent) as IInfraEvent);
end;

procedure TVCLControlView.OnContextPopupHandle(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  Publisher.Publish(
   TContextPopupEvent.Create(Self, MousePos, Handled) as IInfraEvent);
end;

procedure TVCLControlView.OnDblClickHandle(Sender: TObject);
begin
  Publisher.Publish(TVCLNotifyEvent.Create(Self,
    IVCLDblClickEvent) as IInfraEvent);
end;

procedure TVCLControlView.OnDragDropHandle(Sender, Source: TObject; X,
  Y: Integer);
begin
  Publisher.Publish(
    TVCLDragDropEvent.Create(Self, Source, X, Y) as IInfraEvent);
end;

procedure TVCLControlView.OnEndDragHandle(Sender, Target: TObject; X,
  Y: Integer);
begin
  Publisher.Publish(
    TVCLEndDragEvent.Create(Self, Target, X, Y) as IInfraEvent);
end;

procedure TVCLControlView.OnDragOverHandle(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Publisher.Publish(
    TVCLDragOverEvent.Create(Self, Source, X, Y, State, Accept) as IInfraEvent);
end;

procedure TVCLControlView.OnMouseDownHandle(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Publisher.Publish(TVCLMouseEvent.Create(Self, IVCLMouseDownEvent,
    Button, Shift, X, Y) as IInfraEvent);
end;

procedure TVCLControlView.OnMouseMoveHandle(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Publisher.Publish(
    TVCLMouseMoveEvent.Create(Self, Shift, X, Y) as IInfraEvent);
end;

procedure TVCLControlView.OnMouseUpHandle(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Publisher.Publish(TVCLMouseEvent.Create(Self, IVCLMouseUpEvent,
    Button, Shift, X, Y) as IInfraEvent);
end;

procedure TVCLControlView.OnStartDragHandle(Sender: TObject;
  var DragObject: TDragObject);
begin
  Publisher.Publish(TVCLStartDragEvent.Create(Self, DragObject) as IInfraEvent);
end;

procedure TVCLControlView.AssignWindowProc;
begin
  if Assigned(VCLObject) then
  begin
    FDefaultWindowProc := FControl.WindowProc;
    FControl.WindowProc := WndProc;
  end;
end;

procedure TVCLControlView.RestoreWindowProc;
begin
  if Assigned(VCLObject) then
    FControl.WindowProc := FDefaultWindowProc;
end;

procedure TVCLControlView.WndProc(var Message: TMessage);
var
  MySelf: IInterface;
begin
  MySelf := Self as IInterface;
  FDefaultWindowProc(Message);
  if Message.Msg <> FTriggeringMessage then
  begin
    FTriggeringMessage := Message.Msg;
    Dispatch(Message);
  end;
end;

function TVCLControlView.GetVisible: Boolean;
begin
  Result := FControl.Visible;
end;

procedure TVCLControlView.SetVisible(Value: Boolean);
begin
  FControl.Visible := Value;
end;

function TVCLControlView.GetSubViewsIterator: IInfraIterator;
begin
  if Presenter.HasSubPresenters then
    Result := TSubViewsIterator.Create(Presenter.SubPresenters)
  else
    Result := inherited GetSubViewsIterator;
end;

procedure TVCLControlView.Update;
var
  Iterator: IInfraIterator;
begin
  inherited;
  if HasSubViews then
  begin
    Iterator := SubViews.NewIterator;
    while not Iterator.IsDone do
    begin
      (Iterator.CurrentItem as IView).Update;
      Iterator.Next;
    end;
  end;
end;

procedure TVCLControlView.UpdateModel;
var
  Iterator: IInfraIterator;
begin
  inherited;
  if HasSubViews then
  begin
    Iterator := SubViews.NewIterator;
    while not Iterator.IsDone do
    begin
      (Iterator.CurrentItem as IView).UpdateModel;
      Iterator.Next;
    end;
  end;
end;

{ TVCLWinControlView }

procedure TVCLWinControlView.SetObject(Value: TObject);
begin
  if Assigned(VCLObject) then
    with FWinControl do
    begin
      OnEnter := nil;
      OnExit := nil;
      OnKeyDown := nil;
      OnKeyPress := nil;
      OnKeyUp := nil;
    end;
  inherited SetObject(Value);
  FWinControl := TWinControlFriend(VCLObject);
  if Assigned(VCLObject) then
    with FWinControl do
    begin
      OnEnter := OnEnterHandle;
      OnExit := OnExitHandle;
      OnKeyDown := OnKeyDownHandle;
      OnKeyPress := OnKeyPressHandle;
      OnKeyUp := OnKeyUpHandle;
    end;
end;

procedure TVCLWinControlView.OnEnterHandle(Sender: TObject);
begin
  Publisher.Publish(
    TVCLNotifyEvent.Create(Self, IEnterEvent) as IInfraEvent);
end;

procedure TVCLWinControlView.OnExitHandle(Sender: TObject);
begin
  Publisher.Publish(
    TVCLNotifyEvent.Create(Self, IExitEvent) as IInfraEvent);
end;

procedure TVCLWinControlView.OnKeyDownHandle(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  Publisher.Publish(
    TKeyEvent.Create(Self, IKeyDownEvent, Key, Shift) as IInfraEvent);
end;

procedure TVCLWinControlView.OnKeyPressHandle(Sender: TObject;
  var Key: Char);
begin
  Publisher.Publish(TKeyPressEvent.Create(Self, Key) as IInfraEvent);
end;

procedure TVCLWinControlView.OnKeyUpHandle(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Publisher.Publish(
    TKeyEvent.Create(Self, IKeyUpEvent, Key, Shift) as IInfraEvent);
end;

{ TVCLCustomFormView }

procedure TVCLCustomFormView.SetObject(Value: TObject);
begin
  if Assigned(VCLObject) then
    with FCustomForm do
    begin
      OnDestroy := nil;
      OnClose := nil;
    end;
  inherited SetObject(Value);
  FCustomForm := TCustomFormFriend(VCLObject);
  if Assigned(VCLObject) then
    with FCustomForm do
    begin
      OnDestroy := OnDestroyHandle;
      OnClose := OnCloseHandle;
    end;
end;

destructor TVCLCustomFormView.Destroy;
begin
  Publisher.Publish(TInfraEvent.Create(
    Self as IElement, IMVPFormViewDestroyEvent) as IInfraEvent);
  inherited;
end;

procedure TVCLCustomFormView.Close;
begin
  FCustomForm.Close;
end;

procedure TVCLCustomFormView.Hide;
begin
  FCustomForm.Hide;
end;

procedure TVCLCustomFormView.OnDestroyHandle(Sender: TObject);
begin
  Publisher.Publish(TVCLNotifyEvent.Create(Self,
    IVCLFormDestroyEvent) as IInfraEvent);
  SetObject(nil);
end;

procedure TVCLCustomFormView.OnCloseHandle(Sender: TObject;
  var Action: TCloseAction);
begin
  Publisher.Publish(TVCLNotifyEvent.Create(Self,
    IVCLFormCloseEvent) as IInfraEvent);
end;

procedure TVCLCustomFormView.Release;
begin
  FCustomForm.Release;
end;

procedure TVCLCustomFormView.Show;
begin
  FCustomForm.Show;
end;

procedure TVCLCustomFormView.ShowAndWait;
begin
  Hide;
  FCustomForm.Showmodal;
end;

{ TSubViewsIterator }

constructor TSubViewsIterator.Create(const List: IPresenterList);
begin
  inherited Create;
  FList := List;
  First;
end;

function TSubViewsIterator.CurrentItem: IInterface;
begin
  if Assigned(FList) and (fCurrentIndex <> -1)
    and (fCurrentIndex < FList.Count) then
    Result := FList.Items[fCurrentIndex].View
  else
    Result := nil;
end;

procedure TSubViewsIterator.First;
begin
  if FList.Count > 0 then
    fCurrentIndex := 0
  else
    fCurrentIndex := -1;
end;

function TSubViewsIterator.IsDone: Boolean;
begin
  Result := (fCurrentIndex <> FList.Count);
end;

procedure TSubViewsIterator.Next;
begin
  if (FList.Count > 0) and (FCurrentIndex < FList.Count) then
    Inc(fCurrentIndex);
end;

{ TVCLView }

destructor TVCLView.Destroy;
begin
  SetObject(nil);
  inherited;
end;

function TVCLView.FindViewFromObject(aVCLObject: TObject): IView;
var
  Iterator: IInfraIterator;
  CurrentView: IVCLView;
begin
  if aVCLObject = VCLObject then
  begin
    Result := Self as IView;
    Exit;
  end;
  Iterator := GetSubViewsIterator;
  if not Assigned(Iterator) then
  begin
    Result := nil;
    Exit;
  end;
  while not Iterator.IsDone do
  begin
    CurrentView := (Iterator.CurrentItem as IVCLView);
    if CurrentView.VCLObject = aVCLObject then
    begin
      Result := CurrentView;
      Break;
    end
    else
      Result := CurrentView.FindViewFromObject(aVCLObject);
    Iterator.Next;
  end;
end;

function TVCLView.GetObject: TObject;
begin
  Result := FVCLObject;
end;

function TVCLView.GetSubViewsIterator: IInfraIterator;
begin
  // do nothing here, just on descendents!
  Result := nil;
end;

procedure TVCLView.SetObject(Value: TObject);
begin
  if Value <> FVCLObject then
  begin
    if Assigned(FVCLObject) then
      MVPMApperService.RemoveMVPItem(FVCLObject);
    FVCLObject := Value;
    if Assigned(FVCLObject) then
      MVPMApperService.RegisterMVPItem(Model, FVCLObject)
    else
     MVPMApperService.RemoveMVPItem(FVCLObject);
  end;
end;

{ TVCLCustomEditView }

// *** this method not happen when we select the text with mouse ou keyboard
procedure TVCLCustomEditView.EMSetSel(var Message: TMessage);
var
  SelectedText: string;
begin
  SelectedText := FCustomEdit.GetSelText;
  if Model.Selection.Count = 0 then
    Model.Selection.Add(TInfraString.NewFrom(SelectedText))
  else
    (Model.Selection.Items[0] as IInfraString).AsString :=
      (Renderer.TypeConverter.ConvertToLeft(
      TInfraString.NewFrom(SelectedText),
      Renderer.Format) as IInfraString).AsString;
end;

function TVCLCustomEditView.GetText: string;
begin
  Result := FCustomEdit.Text;
end;

procedure TVCLCustomEditView.OnChangeHandle(Sender: TObject);
begin
  Publisher.Publish(TVCLNotifyEvent.Create(Self,
    IVCLCutomEditChangeEvent) as IInfraEvent);
end;

procedure TVCLCustomEditView.SetObject(Value: TObject);
begin
  if Assigned(VCLObject) then
    with FCustomEdit do
      OnChange := nil;
  inherited SetObject(Value);
  FCustomEdit := TCustomEditFriend(VCLObject);
  if Assigned(VCLObject) then
    with FCustomEdit do
      OnChange := OnChangeHandle
end;

procedure TVCLCustomEditView.Update;
begin
  inherited;
  if Assigned(VCLObject) then
  begin
    if Assigned(Model) and Assigned(Model.Value) then
      FCustomEdit.Text :=
        (Renderer.TypeConverter.ConvertToRight(Model.Value,
          Renderer.Format) as IInfraString).AsString
    else
      FCustomEdit.Text := '';
  end;
end;

procedure TVCLCustomEditView.UpdateModel;
begin
  inherited;
  Model.Value.Assign(
    Renderer.TypeConverter.ConvertToLeft(
      TInfraString.NewFrom(FCustomEdit.Text), Renderer.Format));
end;

{ TVCLCustomLabelView }

procedure TVCLCustomLabelView.SetObject(Value: TObject);
begin
  inherited SetObject(Value);
  FCustomLabel := Value as TCustomLabel;
end;

function TVCLCustomLabelView.GetLabelText: string;
begin
  Result := FCustomLabel.Caption;
end;

procedure TVCLCustomLabelView.Update;
begin
  inherited;
  if Assigned(VCLObject) then
  begin
    if Assigned(Model) and Assigned(Model.Value) then
      FCustomLabel.Caption := (Renderer.TypeConverter.ConvertToRight(
        Model.Value, Renderer.Format) as IInfraString).AsString
    else
      FCustomLabel.Caption := '';
  end;
end;

// *** Verificar esta View with details
{ TVCLCustomDateTimeView }

procedure TVCLCustomDateTimeView.SetObject(Value: TObject);
begin
  if Assigned(VCLObject) then
    with FDateTimePicker do
      OnChange := nil;
  inherited SetObject(Value);
  FDateTimePicker := TDateTimePickerFriend(VCLObject);
  if Assigned(VCLObject) then
    with FDateTimePicker do
      OnChange := OnChangeHandle;
end;

function TVCLCustomDateTimeView.GetDateTime:TDateTime;
begin
  Result := FDateTimePicker.DateTime;
end;

procedure TVCLCustomDateTimeView.OnChangeHandle(Sender: TObject);
begin
  // *** publichando um evento do edit?
  Publisher.Publish(TVCLNotifyEvent.Create(Self,
    IVCLCutomEditChangeEvent) as IInfraEvent);
end;

procedure TVCLCustomDateTimeView.Update;
begin
  inherited;
  if Assigned(VCLObject) then
  begin
    if Supports(Model.Value, IInfraDateTime) then
      FDateTimePicker.DateTime :=
        (Renderer.TypeConverter.ConvertToRight(Model.Value,
          Renderer.Format) as IInfraDateTime).AsDateTime
    else if Supports(Model.Value, IInfraDate) then
      FDateTimePicker.Date :=
        (Renderer.TypeConverter.ConvertToRight(Model.Value,
          Renderer.Format) as IInfraDate).AsDate
    else if Supports(Model.Value, IInfraTime) then
      FDateTimePicker.Time :=
        (Renderer.TypeConverter.ConvertToRight(Model.Value,
          Renderer.Format) as IInfraTime).AsTime;
  end;
end;

procedure TVCLCustomDateTimeView.UpdateModel;
begin
  inherited;
    if Supports(Model.Value, IInfraDateTime) then
      Model.Value.Assign(
        Renderer.TypeConverter.ConvertToLeft(
        TInfraDateTime.NewFrom(FDateTimePicker.DateTime), Renderer.Format))
    else if Supports(Model.Value, IInfraDate) then
      Model.Value.Assign(
        Renderer.TypeConverter.ConvertToLeft(
        TInfraDate.NewFrom(FDateTimePicker.Date), Renderer.Format))
    else if Supports(Model.Value, IInfraTime) then
      Model.Value.Assign(
        Renderer.TypeConverter.ConvertToLeft(
        TInfraTime.NewFrom(FDateTimePicker.Time), Renderer.Format));
end;

{ TVCLRadioButtonView }

procedure TVCLRadioButtonView.SetObject(Value: TObject);
begin
  inherited SetObject(Value);
  FRadioButton := TRadioButtonFriend(VCLObject);
end;

procedure TVCLRadioButtonView.InfraInitInstance;
begin
  inherited;
  EventService.Subscribe(IInfraChangedEvent, Self as ISubscriber,
    RadioButtonCheckChanged, 'Checked');
end;

procedure TVCLRadioButtonView.RadioButtonCheckChanged(const Event: IInfraEvent);
begin
  FRadioButton.Checked := Checked.AsBoolean
end;

procedure TVCLRadioButtonView.UpdateModel;
begin
  inherited;
  Model.Value.Assign(
    Renderer.TypeConverter.ConvertToLeft(
      TInfraBoolean.NewFrom(FRadioButton.Checked), Renderer.Format));
end;

function TVCLRadioButtonView.GetChecked: IInfraBoolean;
begin
  if FChecked = nil then
    FChecked := TInfraBoolean.Create;
  Result := FChecked;
end;

procedure TVCLRadioButtonView.Update;
begin
  inherited;
  if Assigned(VCLObject) then
    FRadioButton.Checked := (Renderer.TypeConverter.ConvertToRight(
      Model.Value, Renderer.Format) as IInfraBoolean).AsBoolean;
end;

{ TVCLCustomCheckBoxView }

procedure TVCLCustomCheckBoxView.SetObject(Value: TObject);
begin
  inherited SetObject(Value);
  FCustomCheckBox := TCustomCheckBoxFriend(VCLObject);
end;

procedure TVCLCustomCheckBoxView.InfraInitInstance;
begin
  inherited;
  EventService.Subscribe(IInfraChangedEvent, Self as ISubscriber,
    CustomCheckBoxViewStateChanged, 'State');
end;

procedure TVCLCustomCheckBoxView.CustomCheckBoxViewStateChanged(
  const Event: IInfraEvent);
begin
  if Assigned(VCLObject) then
  begin
    if State.IsNull then
      FCustomCheckBox.State := cbGrayed
    else if State.AsBoolean then
       FCustomCheckBox.State := cbChecked
    else
       FCustomCheckBox.State := cbUnchecked;
  end;
end;

function TVCLCustomCheckBoxView.GetState: IInfraBoolean;
begin
  if FState = nil then
    FState := TInfraBoolean.Create;
  Result := FState;
end;

procedure TVCLCustomCheckBoxView.UpdateModel;
begin
  inherited;
  if Assigned(VCLObject) then
    Model.Value.Assign(Renderer.TypeConverter.ConvertToLeft(
      TInfraBoolean.NewFrom(FCustomCheckBox.Checked), Renderer.Format));
end;

procedure TVCLCustomCheckBoxView.Update;
begin
  inherited;
  if Assigned(VCLObject) then
    FCustomCheckBox.Checked := (Renderer.TypeConverter.ConvertToRight(
      Model.Value, Renderer.Format) as IInfraBoolean).AsBoolean;
end;

{ TVCLButtonView }

procedure TVCLButtonView.SetObject(Value: TObject);
begin
  inherited SetObject(Value);
  FButton := TButtonFriend(VCLObject);
end;

procedure TVCLButtonView.InfraInitInstance;
begin
  inherited;
  EventService.Subscribe(ICommandChangedEvent, Self as ISubscriber,
    ClickCommandEvent, 'ClickCommand');
end;

procedure TVCLButtonView.ClickCommandEvent(const Event: IInfraEvent);
begin
  if Assigned(VCLObject) then
    FButton.Enabled := ClickCommand.Enabled;
end;

function TVCLButtonView.GetCancel: Boolean;
begin
  if Assigned(VCLObject) then
    Result := FButton.Cancel
  else
    Result := False;
end;

function TVCLButtonView.GetClickCommand: ICommand;
begin
  Result := FClickCommand;
end;

function TVCLButtonView.GetDefault: Boolean;
begin
  if Assigned(VCLObject) then
    Result := FButton.Default
  else
    Result := False;
end;

function TVCLButtonView.GetModalResult: Integer;
begin
  if Assigned(VCLObject) then
    Result := FButton.ModalResult
  else
    Result := -1;
end;

procedure TVCLButtonView.OnClickHandle(Sender: TObject);
begin
  inherited OnClickHandle(Sender);
// ***
//  if Assigned(FClickCommand) then
//    FClickCommand.Execute;
end;

procedure TVCLButtonView.SetCancel(Value: Boolean);
begin
  with FButton do
    if Value <> Cancel then
      Cancel := Value;
end;

procedure TVCLButtonView.SetClickCommand(const Value: ICommand);
begin
  if Value <> FClickCommand then
  begin
    FClickCommand := Value;
    Update;
  end;
end;

procedure TVCLButtonView.SetDefault(Value: Boolean);
begin
  with FButton do
    if Value <> Default then
      Default := Value;
end;

procedure TVCLButtonView.SetModalResult(Value: Integer);
begin
  with FButton do
    if Value <> ModalResult then
      ModalResult := Value;
end;

procedure TVCLButtonView.Update;
begin
  inherited;
  if Assigned(VCLObject) and Assigned(FClickCommand) then
    with FButton do
      Enabled := FClickCommand.Enabled;
end;

{ TVCLCustomListBoxView }

procedure TVCLCustomListBoxView.SetObject(Value: TObject);
begin
  if Assigned(VCLObject) then
    with FCustomListBox do
    begin
      OnData := nil;
      OnDrawItem := nil;
      OnMeasureItem := nil;
      OnDataObject := nil;
      OnDataFind := nil;
    end;
  inherited SetObject(Value);
  FCustomListBox := TCustomListBoxFriend(VCLObject);
  if Assigned(VCLObject) then
    with FCustomListBox do
    begin
      Style := lbVirtual;
      OnData := OnDataHandle;
      if FOwnerDraw then
        OnDrawItem := OnDrawItemHandle;
      OnMeasureItem := OnMeasureItemHandle;
      OnDataObject := OnDataObjectHandle;
      OnDataFind := OnDataFindHandle;
    end;
end;

procedure TVCLCustomListBoxView.InfraInitInstance;
begin
  inherited;
  EventService.Subscribe(IInfraMultiSelectChanged, Self as ISubscriber,
    MultiSelectChangeEvent, 'Model.Selection');
  EventService.Subscribe(IInfraRemoveItemEvent, Self as ISubscriber,
    RemoveItemEvent, 'Model.Selection');
  EventService.Subscribe(IInfraAddItemEvent, Self as ISubscriber,
    AddItemEvent, 'Model.Selection');
  EventService.Subscribe(IInfraClearListEvent, Self as ISubscriber,
    ClearListEvent, 'Model.Selection');
  EventService.Subscribe(IInfraChangedEvent, Self as ISubscriber,
    ListChangeEvent, EmptyStr, ChangedFilter);
end;

function TVCLCustomListBoxView.ChangedFilter(const Event: IInfraEvent): Boolean;
var
  vSrc: IInfraList;
  vProperty: IProperty;
begin
  Result :=
    (Supports(Event.Source, IProperty, vProperty) and
      (ListItemView.FeatureName = vProperty.Name))
    or
    (Supports(Event.Source, IInfraList, vSrc) and
      ((Model.Value as IInfraList) = vSrc));
end;

procedure TVCLCustomListBoxView.RemoveItemEvent(const Event: IInfraEvent);
begin
  if FUpdatingSelection then
    Exit;
  with FCustomListBox do
  begin
    if MultiSelect then
      Selected[(Event as IInfraRemoveItemEvent).ItemIndex] := False
    else
      ItemIndex := -1;
  end;
end;

procedure TVCLCustomListBoxView.AddItemEvent(const Event: IInfraEvent);
var
  i: integer;
begin
  if FUpdatingSelection then
    Exit;
  with FCustomListBox do
  begin
    i := (Model.Value as IInfraList).IndexOf(
      (Event as IInfraAddItemEvent).NewItem);
    if MultiSelect then
      Selected[i] := True
    else
      ItemIndex := i;
  end;
end;

procedure TVCLCustomListBoxView.ClearListEvent(const Event: IInfraEvent);
var
  i: integer;
begin
  if FUpdatingSelection then
    Exit;
  with FCustomListBox do
  begin
    if MultiSelect then
      for i := Items.Count - 1 downto 0 do
        Selected[i] := False
    else
      ItemIndex := -1;
  end;
end;

procedure TVCLCustomListBoxView.MultiSelectChangeEvent(const Event: IInfraEvent);
begin
  if FUpdatingSelection then
    Exit;
  with FCustomListBox do
  begin
    MultiSelect := Model.Selection.MultiSelect;
    ItemIndex := -1;
  end;
end;

procedure TVCLCustomListBoxView.ListChangeEvent(const Event: IInfraEvent);
begin
  if FUpdatingSelection then
    Exit;
  Self.Update;
end;

procedure TVCLCustomListBoxView.CNCommand(var Message: TWMCommand);
var
  i: integer;
begin
  case Message.NotifyCode of
    LBN_SELCHANGE:
    begin
      FUpdatingSelection := True;
      try
        Model.Selection.Clear;
      finally
        FUpdatingSelection := False;
      end;
      if Assigned(VCLObject) then
      begin
        if Model.Selection.MultiSelect then
        begin
          for i := 0 to FCustomListBox.Count-1 do
            if FCustomListBox.Selected[i] then
              Model.Selection.Add((ListItemView as
                IVCLCustomListBoxItemView).GetValueForIndex(i));
        end else
          Model.Selection.Add((ListItemView as
            IVCLCustomListBoxItemView).GetValueForIndex(
              FCustomListBox.ItemIndex));
      end;
    end;
  end;
end;

function TVCLCustomListBoxView.GetCount: Integer;
begin
  Result := FCustomListBox.Items.Count;
end;

function TVCLCustomListBoxView.GetItemIndex: Integer;
begin
  Result := FCustomListBox.ItemIndex;
end;

function TVCLCustomListBoxView.GetListItemView: IVCLCustomListBoxItemView;
begin
  if not Assigned(FListItemView) and (Presenter.SubPresenters.Count = 1) then
    FListItemView :=
      Presenter.SubPresenters.Items[0].View as IVCLCustomListBoxItemView;
  Result := FListItemView;
end;

function TVCLCustomListBoxView.OnDataFindHandle(Control: TWinControl;
  FindString: String): Integer;
begin
  if Assigned(ListItemView.Renderer) then
    Result := (ListItemView.Renderer as
      IVCLCustomListBoxItemRenderer).DoDataFind(FindString)
  else
    Result := -1;
end;

procedure TVCLCustomListBoxView.OnDataHandle(Control: TWinControl;
  Index: Integer; var Data: string);
begin
  // this test is necessary why when removing a item the Index variable is
  // invalid and GetValueForIndex will fail.
  if Index >= (Presenter.Model.Value as IInfraList).Count then
    Exit;
  ListItemView.Presenter.Model.Value :=
    (ListItemView as IVCLCustomListBoxItemView).GetValueForIndex(Index);
  if Assigned(ListItemView.Renderer) then
    (ListItemView.Renderer as
      IVCLCustomListBoxItemRenderer).DoGetData(Index, Data);
end;

procedure TVCLCustomListBoxView.OnDataObjectHandle(Control: TWinControl;
  Index: Integer; var DataObject: TObject);
begin
  if Assigned(ListItemView.Renderer) then
    (ListItemView.Renderer as
      IVCLCustomListBoxItemRenderer).DoDataObject(Index, DataObject);
end;

procedure TVCLCustomListBoxView.OnDrawItemHandle(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if Assigned(ListItemView.Renderer) then
    (ListItemView.Renderer as
      IVCLCustomListBoxItemRenderer).DoDrawItem(Index, Rect, State);
end;

procedure TVCLCustomListBoxView.OnMeasureItemHandle(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  if Assigned(ListItemView.Renderer) then
    (ListItemView.Renderer as
      IVCLCustomListBoxItemRenderer).DoMeasureItem(Index, Height);
end;

procedure TVCLCustomListBoxView.SetCount(Value: Integer);
begin
  if Assigned(VCLObject) then
    FCustomListBox.Count := Value;
end;

procedure TVCLCustomListBoxView.Update;
begin
  inherited;
  if Assigned(Model) and Assigned(Model.Value) then
    Count := (Model.Value as IInfraList).Count
  else
    Count := 0;
end;

procedure TVCLCustomListBoxView.UpdateModel;
begin
  // do nothing here, just on descendents!
end;

procedure TVCLCustomListBoxView.SetListItemView(
  const Value: IVCLCustomListBoxItemView);
begin
  if Value <> FListItemView then
    FListItemView := Value;
end;

function TVCLCustomListBoxView.GetOwnerDraw: boolean;
begin
  Result := FOwnerDraw;
end;

procedure TVCLCustomListBoxView.SetOwnerDraw(const Value: boolean);
begin
  if Value <> FOwnerDraw then
  begin
    FOwnerDraw := Value;
    if Assigned(VCLObject) then
      with FCustomListBox do
        if FOwnerDraw then
          OnDrawItem := OnDrawItemHandle
        else
          OnDrawItem := nil;
  end;
end;

{ TVCLCustomListBoxItemView }

function TVCLCustomListBoxItemView.GetFeatureName: string;
begin
  // *** Result := FFeatureName;
  // *** solerman: verificar se existe a possibilidade do listview
  // *** apresentar algo que nao use o objecttofeature...
  Result := '';
  if Assigned(Renderer) and
    Assigned(Renderer.Format) then
    Result := (Renderer.Format as IFormatProperty).PropertyName
  else
    Raise Exception.Create('Some problem with ListviewItem.Renderer.Format');
end;

function TVCLCustomListBoxItemView.GetRenderer: IRenderer;
begin
  if not Assigned(FRenderer) then
    FRenderer := TVCLCustomListBoxItemRenderer.Create as IRenderer;
  Result := inherited GetRenderer;
end;

function TVCLCustomListBoxItemView.GetValueForIndex(
  Index: Integer): IInfraType;
var
  List: IInfraList;
begin
  List := Presenter.ParentPresenter.Model.Value as IInfraList;
  Result := List.Items[Index] as IInfraType;
end;

{ TVCLMenuView }

function TVCLMenuView.GetItems: IVCLMenuItemView;
begin
  if not Assigned(FItems) then
    FItems := TVCLMenuItemView.Create;
  Result := FItems;
end;

function TVCLMenuView.GetOwnerDraw: boolean;
begin
  Result := FOwnerDraw;
end;

procedure TVCLMenuView.SetOwnerDraw(Value: boolean);
begin
  if Value <> FOwnerDraw then
    FOwnerDraw := Value;
end;

procedure InjectMVPMapperService;
begin
  (ApplicationContext as IBaseElement).Inject(
    IMVPMapperService, TMVPMapperService.Create);
end;

initialization
  InjectMVPMapperService

end.


