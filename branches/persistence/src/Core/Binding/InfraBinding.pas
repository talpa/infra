unit InfraBinding;

interface

uses
  InfraBindingIntf, InfraCommon, InfraValueTypeIntf, InfraCommonIntf;

type
  TBinding = class(TElement, IBinding)
  private
    FLeft, FRight: IBindable;
    FValueConverter: ITypeConverter;
    FMode: TBindingMode;
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

  TBindableValueChanged = class(TElement, IBindableValueChanged)
  private
    FSource: IBindable;
    function GetSource: IElement;
    procedure SetSource(const Value: IElement);
  public
    constructor Create(const Source: IBindable); reintroduce;
  end;

implementation

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

{ TBindableValueChanged }

constructor TBindableValueChanged.Create(const Source: IBindable);
begin
  inherited Create;
  FSource := Source;
end;

function TBindableValueChanged.GetSource: IElement;
begin
  Result := FSource;
end;

procedure TBindableValueChanged.SetSource(const Value: IElement);
begin
  FSource := Value as IBindable;
end;

end.
