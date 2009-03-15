unit InfraBindingManager;

interface

uses
  Controls,
  InfraCommon,
  InfraNotify,
  InfraCommonIntf,
  InfraBindingIntf,
  InfraValueTypeIntf;

type
  TBinding = class(TElement, IBinding)
  private
    FActive: Boolean;
    FLeft, FRight: IBindable;
    FValueConverter: ITypeConverter;
    FMode: TBindingMode;
    procedure UpdateRight;
    procedure PropertyChanged(const Event: IInfraEvent);
    function PropertyChangedFilter(const Event: IInfraEvent): Boolean;
  protected
    function GetLeft: IBindable;
    function GetMode: TBindingMode;
    function GetRight: IBindable;
    function GetValueConverter: ITypeConverter;
    procedure SetMode(Value: TBindingMode);
    procedure SetValueConverter(const Value: ITypeConverter);
    procedure UpdateLeft;
    function TwoWay: IBinding;
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
  public
    constructor Create(const Left, Right: IBindable); reintroduce;
  end;

  TBindManager = class(TElement, IBindManager)
  private
    FActive: Boolean;
    FBindingList: IBindingList;
    FDataContext: IInfraType;
  protected
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetDataContext: IInfraType;
    procedure SetDataContext(const Value: IInfraType);
    function Add(const pLeft, pRight: IBindable;
      const pConverter: ITypeConverter = nil): IBinding; overload;
    function Add(
      pLeftControl: TControl; const pLeftProperty: string;
      pRightControl: TControl; const pRightProperty: string;
      const pConverter: ITypeConverter = nil): IBinding; overload;
    function Add(
      const pLeftProperty: string;
      pRightControl: TControl; const pRightProperty: string = '';
      const pConverter: ITypeConverter = nil): IBinding; overload;
    procedure ClearBindings;
    property DataContext: IInfraType read GetDataContext write SetDataContext;
    property Active: boolean read GetActive write SetActive;
  public
    constructor Create; override;
  end;

  TNotifyValueChanged = class(TInfraEvent, INotifyValueChanged);

implementation

uses
  List_Binding,
  InfraBindingControl,
  InfraBindingType,
  InfraBindingConsts,
  SysUtils;

{ TBinding }

constructor TBinding.Create(const Left, Right: IBindable);
begin
  if not Assigned(Left) then
    Raise EInfraBindingError.Create(cErrorLeftBindableNotDefined);
  if not Assigned(Right) then
    Raise EInfraBindingError.Create(cErrorRightBindableNotDefined);
  FLeft := Left;
  FRight := Right;
  SetMode(bmLeftToRight);
  EventService.Subscribe(INotifyValueChanged, Self as ISubscriber,
    PropertyChanged, EmptyStr, PropertyChangedFilter);
end;

procedure TBinding.PropertyChanged(const Event: IInfraEvent);
var
  vBindable: IBindable;
begin
  vBindable := (Event.Source as IBindable);
  if vBindable = FLeft then
    UpdateRight
  else
    UpdateLeft;
end;

function TBinding.PropertyChangedFilter(const Event: IInfraEvent): Boolean;
var
  vSource: IBindable;
begin
  vSource := Event.Source As IBindable;
  Result := (vSource = FLeft) or (vSource = FRight);
end;

function TBinding.GetLeft: IBindable;
begin
  Result := FLeft;
end;

function TBinding.GetMode: TBindingMode;
begin
  Result := FMode;
end;

function TBinding.GetRight: IBindable;
begin
  Result := FRight;
end;

function TBinding.GetValueConverter: ITypeConverter;
begin
  Result := FValueConverter;
end;

procedure TBinding.SetMode(Value: TBindingMode);
begin
  if (Value = bmTwoWay)
    and not FRight.Support2Way then
    Raise EInfraBindingError.Create(cErrorBindable2WayNotSupported);
  FMode := Value;
end;

procedure TBinding.SetValueConverter(const Value: ITypeConverter);
begin
  FValueConverter := Value;
end;

function TBinding.TwoWay: IBinding;
begin
  SetMode(bmTwoWay);
  Result := Self;
end;

procedure TBinding.UpdateLeft;
begin
  FLeft.Value := FRight.Value;
end;

procedure TBinding.UpdateRight;
begin
  FRight.Value := FLeft.Value;
end;

function TBinding.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TBinding.SetActive(Value: Boolean);
begin
  UpdateRight;
end;

{ TBindManager }

constructor TBindManager.Create;
begin
  inherited Create;
  FBindingList := TBindingList.Create;
end;

function TBindManager.Add(
  pLeftControl: TControl; const pLeftProperty: string;
  pRightControl: TControl; const pRightProperty: string;
  const pConverter: ITypeConverter = nil): IBinding;
var
  vLeft, vRight: IBindable;
begin
  vLeft := GetBindableVCL(pLeftControl, pLeftProperty);
  vRight := GetBindableVCL(pRightControl, pRightProperty);
  Result := Add(vLeft, vRight, pConverter);
end;

function TBindManager.Add(const pLeftProperty: string;
  pRightControl: TControl; const pRightProperty: string = '';
  const pConverter: ITypeConverter = nil): IBinding;
var
  vLeft, vRight: IBindable;
begin
  vLeft := TBindableInfraType.GetBindable(FDataContext, pLeftProperty);
  vRight := GetBindableVCL(pRightControl, pRightProperty);
  Result := Add(vLeft, vRight, pConverter);
end;

function TBindManager.Add(const pLeft, pRight: IBindable;
  const pConverter: ITypeConverter = nil): IBinding;
var
  vBinding: IBinding;
begin
  vBinding := TBinding.Create(pLeft, pRight);
  vBinding.ValueConverter := pConverter;
  FBindingList.Add(vBinding)
end;

procedure TBindManager.ClearBindings;
begin
  FBindingList.Clear;
end;

function TBindManager.GetDataContext: IInfraType;
begin
  Result := FDataContext;
end;

procedure TBindManager.SetDataContext(const Value: IInfraType);
begin
  FDataContext := Value;
end;

function TBindManager.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TBindManager.SetActive(Value: Boolean);
var
  vIterator: IInfraIterator;
begin
  vIterator := nil;
  if FActive <> Value then
    FActive := Value;
  if FActive then
  begin
    vIterator := FBindingList.NewIterator;
    while not vIterator.IsDone do
    begin
      (vIterator.CurrentItem as IBinding).Active := True;
      vIterator.Next;
    end;
  end;
end;

end.
