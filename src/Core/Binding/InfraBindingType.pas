unit InfraBindingType;

interface

uses
  InfraBindingIntf,
  InfraValueTypeIntf,
  InfraCommonIntf,
  InfraBinding;

type
  TBindableInfraType = class(TBindable, IBindableInfraType)
  private
    FInfraType: IInfraType;
  protected
    function Support2Way: Boolean; override;
    function GetValue: IInfraType; override;
    procedure SetValue(const Value: IInfraType); override;
    procedure ValueChanged(const Event: IInfraEvent);
    function ValueChangedFilter(const Event: IInfraEvent): Boolean;
  public
    constructor Create(const pProperty: IProperty); reintroduce;
    class function GetBindable(pValue: IInfraType;
      const pPropertyPath: string): IBindable;
  end;

  TBindableInfraList = class(TBindableInfraType, IBindableInfraList)
  private
    FListType: IVCLListType;
  protected
    function GetValue: IInfraType; override;
    procedure SetValue(const Value: IInfraType); override;
    procedure ItemAdded(const Event: IInfraEvent);
    procedure ItemRemove(const Event: IInfraEvent);    
    procedure ItemsClear(const Event: IInfraEvent);
  public
    constructor Create(const pProperty: IProperty); reintroduce;  
  end;

implementation

uses
  SysUtils,
  InfraBindingConverter;

{ TBindableInfraType }

constructor TBindableInfraType.Create(const pProperty: IProperty);
begin
  inherited Create;
  FInfraType := pProperty;
  EventService.Subscribe(IInfraChangedEvent, Self as ISubscriber,
    ValueChanged, EmptyStr, ValueChangedFilter);
end;

class function TBindableInfraType.GetBindable(pValue: IInfraType;
  const pPropertyPath: string): IBindable;
var
  vObject: IInfraObject;
  vProperty: IProperty;
begin
  // *** E se o Value for uma lista, ou outro tipo de infratype? vai se criar um
  // *** bindable para cada tipo?
  if Supports(pValue, IInfraObject, vObject) then
  begin
    vProperty := vObject.GetProperty(pPropertyPath);
    // *** teria de gerar exceção quando o infraobject nao possuir a propriedade?
    if Supports(vProperty, IInfraList) then
      Result := TBindableInfraList.Create(vProperty)
    else
      Result := TBindableInfraType.Create(vProperty);
  end;
end;

function TBindableInfraType.GetValue: IInfraType;
begin
  Result := FInfraType;
end;

procedure TBindableInfraType.SetValue(const Value: IInfraType);
begin
  FInfraType.Assign(Value);
end;

function TBindableInfraType.Support2Way: Boolean;
begin
  Result := True;
end;

procedure TBindableInfraType.ValueChanged(const Event: IInfraEvent);
begin
  Changed;
end;

function TBindableInfraType.ValueChangedFilter(const Event: IInfraEvent): Boolean;
var
  vSource: IInfraType;
begin
  vSource := Event.Source as IInfraType;
  Result := vSource = FInfraType;
end;

{ TBindableInfraList }

constructor TBindableInfraList.Create(const pProperty: IProperty);
begin
  inherited Create(pProperty);
  EventService.Subscribe(IInfraAddItemEvent, Self as ISubscriber,
    ItemAdded, EmptyStr, ValueChangedFilter);
  EventService.Subscribe(IInfraClearListEvent, Self as ISubscriber,
    ItemsClear, EmptyStr, ValueChangedFilter);
  EventService.Subscribe(IInfraRemoveItemEvent, Self as ISubscriber,
    ItemRemove, EmptyStr, ValueChangedFilter);

  FListType := TVCLListType.Create;
  FListType.Operation := loRefresh;
  FListType.InfraValue := FInfraType;
end;

function TBindableInfraList.GetValue: IInfraType;
begin
  Result := FListType;
end;

procedure TBindableInfraList.SetValue(const Value: IInfraType);
begin
  FListType := Value as IVCLListType;
end;

procedure TBindableInfraList.ItemAdded(const Event: IInfraEvent);
begin
  FListType.Operation := loAdd;
  FListType.InfraValue := (Event as IInfraAddItemEvent).NewItem as IInfraObject;
end;

procedure TBindableInfraList.ItemRemove(const Event: IInfraEvent);
begin
  FListType.Operation := loRemove;
  FListType.ItemIndex := (Event as IInfraRemoveItemEvent).ItemIndex;
end;

procedure TBindableInfraList.ItemsClear(const Event: IInfraEvent);
begin
  FListType.Operation := loClear;
  FListType.InfraValue := FInfraType;
end;

end.
