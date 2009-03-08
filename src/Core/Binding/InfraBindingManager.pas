unit InfraBindingManager;

interface

uses
  Controls,
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
    procedure SetLeft(const Value: IBindable);
    procedure SetMode(Value: TBindingMode);
    procedure SetRight(const Value: IBindable);
    procedure SetValueConverter(const Value: IValueConverter);
    procedure UpdateLeft;
    function TwoWay: IBinding;
  public
    constructor Create(const Left, Right: IBindable); reintroduce;
  end;

  TBindManager = class(TElement, IBindManager)
  private
    FListBind : IBindingList;
  protected
    procedure Add(const Binding: IBinding); overload;
    procedure Add(pSourceControl: TControl;
      const pSourcePath: string;
      pTargetControl: TControl;
      const TargetProperty: string = '';
      const ValueConverter: IValueConverter = nil); overload;
    procedure add(const pSourcePath: string; pTargetControl: TControl;
      const TargetProperty: string = '';
      const ValueConverter: IValueConverter = nil);   overload;
    procedure ClearBindings ;
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

procedure TBinding.SetValueConverter(const Value: IValueConverter);
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

procedure TBindManager.Add(const Binding: IBinding);
begin
  FListBind.Add(Binding)
end;

procedure TBindManager.Add(pSourceControl: TControl;
  const pSourcePath: string; pTargetControl: TControl;
  const TargetProperty: string; const ValueConverter: IValueConverter);
begin

end;

procedure TBindManager.Add(const pSourcePath: string;
  pTargetControl: TControl; const TargetProperty: string;
  const ValueConverter: IValueConverter);
begin

end;

procedure TBindManager.ClearBindings;
begin
  FListBind.Clear;
end;

constructor TBindManager.Create;
begin
  inherited Create;
  FListBind := TBindingList.Create;
end;

end.

