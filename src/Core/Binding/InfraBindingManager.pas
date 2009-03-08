unit InfraBindingManager;

interface

uses
  Controls,
  InfraBindingIntf,
  InfraValueTypeIntf,
  InfraCommon;

type
  TBinding = class(TElement, IBinding)
  private
    FLeft, FRight: IBindable;
    FValueConverter: ITypeConverter;
    FMode: TBindingMode;
  protected
    function GetLeft: IBindable;
    function GetMode: TBindingMode;
    function GetRight: IBindable;
    function GetValueConverter: ITypeConverter;
    procedure SetLeft(const Value: IBindable);
    procedure SetMode(Value: TBindingMode);
    procedure SetRight(const Value: IBindable);
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
    function Add(const pLeftProperty: string;
      pRightControl: TControl; const pRightProperty: string = '';
      const pConverter: ITypeConverter = nil): IBinding; overload;
    procedure ClearBindings;
    property DataContext: IInfraType read GetDataContext write SetDataContext;
  public
    constructor Create; override;
  end;

implementation

uses
  List_Binding;

{ TBinding }

constructor TBinding.Create(const Left, Right: IBindable);
begin
  SetLeft(Left);
  SetRight(Right);
  SetMode(bmLeftToRight);
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

procedure TBinding.SetLeft(const Value: IBindable);
begin
  FLeft := Value;
end;

procedure TBinding.SetMode(Value: TBindingMode);
begin
  FMode := Value;
end;

procedure TBinding.SetRight(const Value: IBindable);
begin
  FRight := Value;
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
var
  vLeft, vRight: IBindable;
begin
  {
  vLeft := BindableControlFactory.GetBindable(pLeftControl, pLeftProperty);
  vRight := BindableControlFactory.GetBindable(pRightControl, pRightProperty);
  }
  Result := Add(vLeft, vRight, pConverter);
end;

function TBindManager.Add(const pLeftProperty: string;
  pRightControl: TControl; const pRightProperty: string = '';
  const pConverter: ITypeConverter = nil): IBinding;
var
  vLeft, vRight: IBindable;
begin
  {
  vLeft := BindableInfraTypeFactory.GetBindable(FDataContext, pLeftProperty);
  vRight := BindableControlFactory.GetBindable(pRightControl, pRightProperty);
  }
  Result := Add(vLeft, vRight, pConverter);
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

end.

