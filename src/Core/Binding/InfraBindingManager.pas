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
  public
    constructor Create(const Left, Right: IBindable); reintroduce;
  end;

  TBindManager = class(TElement, IBindManager)
  private
    FListBind: IBindingList;
    FDataContext: IInfraType;
  protected
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
  public
    constructor Create; override;
  end;

  TNotifyValueChanged  = class(TInfraEvent, INotifyValueChanged )
  private
    FSource: IBindable;
    function GetSource: IElement;
    procedure SetSource(const Value: IElement);
  public
    constructor Create(const Source: IBindable); reintroduce;
  end;

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
  EventService.Subscribe(INotifyValueChanged , Self as ISubscriber,
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
begin
  Result := (Event.Source = FLeft) or (Event.Source = FRight);
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
  FLeft.Value.Assign(FRight.Value);
end;

procedure TBinding.UpdateRight;
begin
  FRight.Value.Assign(FLeft.Value);
end;

{ TBindManager }

constructor TBindManager.Create;
begin
  inherited Create;
  FListBind := TBindingList.Create;
end;

function TBindManager.Add(
  pLeftControl: TControl; const pLeftProperty: string;
  pRightControl: TControl; const pRightProperty: string;
  const pConverter: ITypeConverter = nil): IBinding;
//var
//  vLeftClass, vRightClass: TBindableControlClass;
//  vLeft, vRight: IBindable;
begin
//  with BindingService.MappingControls do
//  begin
//    vLeftClass := TBindableControlClass(Items[pLeftControl.ClassType]);
//    vRightClass := TBindableControlClass(Items[pRightControl.ClassType]);
//  end;
//  vLeft := vLeftClass.Create(pLeftControl, pLeftProperty);
//  vRight := vRightClass.Create(pRightControl, pRightProperty);
//  Result := Add(vLeft, vRight, pConverter);
end;

function TBindManager.Add(const pLeftProperty: string;
  pRightControl: TControl; const pRightProperty: string = '';
  const pConverter: ITypeConverter = nil): IBinding;
//var
//  vRightClass: TBindableControlClass;
//  vLeft, vRight: IBindable;
begin
//  vLeft := TBindableInfraType.GetBindable(FDataContext, pLeftProperty);
//  with BindingService.MappingControls do
//    vRightClass := TBindableControlClass(Items[pRightControl.ClassType]);
//  vRight := vRightClass.Create(pRightControl, pRightProperty);
//  Result := Add(vLeft, vRight, pConverter);
end;

function TBindManager.Add(const pLeft, pRight: IBindable;
  const pConverter: ITypeConverter = nil): IBinding;
var
  vBinding: IBinding;
begin
  vBinding := TBinding.Create(pLeft, pRight);
  vBinding.ValueConverter := pConverter;
  FListBind.Add(vBinding)
end;

procedure TBindManager.ClearBindings;
begin
  FListBind.Clear;
end;

function TBindManager.GetDataContext: IInfraType;
begin
  Result := FDataContext;
end;

procedure TBindManager.SetDataContext(const Value: IInfraType);
begin
  FDataContext := Value;
end;

{ TNotifyValueChanged  }

constructor TNotifyValueChanged .Create(const Source: IBindable);
begin
  inherited Create;
  FSource := Source;
end;

function TNotifyValueChanged .GetSource: IElement;
begin
  Result := FSource;
end;

procedure TNotifyValueChanged .SetSource(const Value: IElement);
begin
  FSource := Value as IBindable;
end;

end.