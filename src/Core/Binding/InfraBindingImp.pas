unit InfraBindingImp;

interface

uses
  InfraBindingIntf,
  InfraCommon;

type
  TBinding = class(TElement, IBinding)
  private
    FLeft, FRight: IBindable;
    FValueConverter: IValueConverter;
    FMode: TBindingMode;
  protected
    function GetLeft: IBindable;
    function GetMode: TBindingMode;
    function GetRight: IBindable;
    function GetValueConverter: IValueConverter;
    procedure SetLeft(Value: IBindable);
    procedure SetMode(Value: TBindingMode);
    procedure SetRight(Value: IBindable);
    procedure SetValueConverter(Value: IValueConverter);
    procedure UpdateLeft;
    function TwoWay: IBindable;
  public
    constructor Create(const Left, Right: IBindable); reintroduce;
  end;

  TBindManager = class(TElement, IBindManager)
  private
   FListBind : IBindingList;
  protected
    procedure Add(Binding: IBinding); overload;
    procedure ClearBindings ;
  public
    constructor Create;
  end;

implementation
uses List_Binding;
{ TBinding }

constructor TBinding.Create(const Left, Right: IBindable);
begin
  SetLeft(Left);
  SetRight(Right);
  SetMode(bmTwoWay);
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

function TBinding.GetValueConverter: IValueConverter;
begin
  Result := FValueConverter;
end;

procedure TBinding.SetLeft(Value: IBindable);
begin
  FLeft := Value;
end;

procedure TBinding.SetMode(Value: TBindingMode);
begin
  FMode := Value;
end;

procedure TBinding.SetRight(Value: IBindable);
begin
  FRight := Value;
end;

procedure TBinding.SetValueConverter(Value: IValueConverter);
begin
  FValueConverter := Value;
end;

function TBinding.TwoWay: IBindable;
begin
  SetMode(bmTwoWay);
//  Result := Self;
end;

procedure TBinding.UpdateLeft;
begin

end;

{ TBindManager }

procedure TBindManager.Add(Binding: IBinding);
begin
  FListBind.Add(Binding)
end;

procedure TBindManager.ClearBindings;
begin
 FListBind.Clear;
end;

constructor TBindManager.Create;
begin
  FListBind := TBindingList.Create;
end;

end.

