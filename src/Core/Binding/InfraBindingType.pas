unit InfraBindingType;

interface

uses
  InfraValueTypeIntf,
  InfraBindingIntf,
  InfraBinding;

function GetBindableType(const pValue: IInfraType;
  const pExpression: string): IBindable;

implementation

uses
  SysUtils,
  InfraCommonIntf,
  InfraBindingConsts,
  InfraBindingConverter;

type
  TBindableInfraType = class(TBindable, IBindableInfraType)
  private
    FModel: IInfraType;
    procedure ValueChanged(const Event: IInfraEvent);
    function ValueChangedFilter(const Event: IInfraEvent): Boolean;
  protected
    function Support2Way: Boolean; override;
    function GetValue: IInfraType; override;
    procedure SetValue(const Value: IInfraType); override;
  public
    constructor Create(const pProperty: IProperty); reintroduce;
  end;

  TBindableInfraList = class(TBindable, IBindableInfraList)
  private
    FListModel: IBindableListModel;
    function CurrentItemChangedFilter(const Event: IInfraEvent): Boolean;
    function ListChangedFilter(const Event: IInfraEvent): Boolean;
    procedure CurrentItemChanged(const Event: IInfraEvent);
    procedure ItemAdded(const Event: IInfraEvent);
    procedure ItemRemoved(const Event: IInfraEvent);
    procedure ItemsClear(const Event: IInfraEvent);
  protected
    function GetValue: IInfraType; override;
    procedure SetValue(const Value: IInfraType); override;
  public
    constructor Create(const pProperty: IProperty;
      const pExpression: string); reintroduce;
  end;

function GetFirstPropertyName(var pExpression: string): string;
var
  vCommaPosition: Integer;
begin
  Result := pExpression;
  vCommaPosition := Pos('.', pExpression);
  if vCommaPosition = 0 then
  begin
    pExpression := '';
    Exit;
  end;
  Result := Copy(pExpression, 0, vCommaPosition-1);
  pExpression := Copy(pExpression, vCommaPosition+1, Length(pExpression));
end;

function GetBindableType(const pValue: IInfraType;
  const pExpression: string): IBindable;
var
  vObject: IInfraObject;
  vProperty: IProperty;
  vNewExpression: string;
begin
  vNewExpression := pExpression;
  if Supports(pValue, IInfraObject, vObject) then
  begin
    vProperty := vObject.GetProperty(GetFirstPropertyName(vNewExpression));
    if Supports(vProperty, IInfraList) then
      Result := TBindableInfraList.Create(vProperty, vNewExpression)
    else if Supports(vProperty, IInfraType) then
    begin
      if vNewExpression <> EmptyStr then
        vProperty := (vProperty as IInfraObject).GetProperty(vNewExpression);
      Result := TBindableInfraType.Create(vProperty);
    end else
      raise EInfraBindingError.Create(cErrorBindingExpressionNotsupported);
  end else
    raise EInfraBindingError.Create(cErrorDataContextNotIsInfraObject);
end;

{ TBindableInfraType }

constructor TBindableInfraType.Create(const pProperty: IProperty);
begin
  inherited Create;
  FModel := pProperty as IInfraType;
  EventService.Subscribe(IInfraChangedEvent, Self as ISubscriber,
    ValueChanged, EmptyStr, ValueChangedFilter);
end;

function TBindableInfraType.GetValue: IInfraType;
begin
  Result := FModel;
end;

procedure TBindableInfraType.SetValue(const Value: IInfraType);
begin
  FModel.Assign(Value);
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
  Result := vSource = FModel;
end;

{ TBindableInfraList }

constructor TBindableInfraList.Create(const pProperty: IProperty;
  const pExpression: string);
begin
  inherited Create;
  FListModel := TBindableListModel.Create;
  with FListModel do
  begin
    Operation := loRefresh;
    List := pProperty as IInfraType;
    Expression := pExpression;
  end;
  EventService.Subscribe(IInfraAddItemEvent, Self as ISubscriber,
    ItemAdded, EmptyStr, ListChangedFilter);
  EventService.Subscribe(IInfraClearListEvent, Self as ISubscriber,
    ItemsClear, EmptyStr, ListChangedFilter);
  EventService.Subscribe(IInfraRemoveItemEvent, Self as ISubscriber,
    ItemRemoved, EmptyStr, ListChangedFilter);
  EventService.Subscribe(IInfraChangedEvent, Self as ISubscriber,
    CurrentItemChanged, EmptyStr, CurrentItemChangedFilter);
end;

function TBindableInfraList.GetValue: IInfraType;
begin
  Result := FListModel as IInfraType;
end;

procedure TBindableInfraList.SetValue(const Value: IInfraType);
begin
  Raise EInfraBindingError.CreateFmt(cErrorBindableisReadOnly,
    ['TBindableInfraList']);
end;

procedure TBindableInfraList.ItemAdded(const Event: IInfraEvent);
var
  vAddEvent: IInfraAddItemEvent;
begin
  with FListModel do
  begin
    Operation := loAdd;
    vAddEvent := Event as IInfraAddItemEvent;
    ItemOperated := vAddEvent.NewItem;
  end;
  Changed;
end;

procedure TBindableInfraList.ItemRemoved(const Event: IInfraEvent);
var
  vRemoveEvent: IInfraRemoveItemEvent;
begin
  with FListModel do
  begin
    Operation := loRemove;
    vRemoveEvent := Event as IInfraRemoveItemEvent;
    ItemOperated := vRemoveEvent.RemovedItem;
  end;
  Changed;
end;

procedure TBindableInfraList.ItemsClear(const Event: IInfraEvent);
begin
  with FListModel do
    Operation := loClear;
  Changed;
end;

procedure TBindableInfraList.CurrentItemChanged(const Event: IInfraEvent);
begin
  with FListModel do
  begin
    Operation := loSelectionChange;
    Current := Event.Source as IInfraType;
  end;
  Changed;
end;

function TBindableInfraList.ListChangedFilter(
  const Event: IInfraEvent): Boolean;
var
  vSource: IInfraType;
begin
  vSource := Event.Source as IInfraType;
  Result := vSource = FListModel.List;
end;

function TBindableInfraList.CurrentItemChangedFilter(
  const Event: IInfraEvent): Boolean;
var
  vSource: IInfraType;
begin
  vSource := Event.Source as IInfraType;
  Result := vSource = FListModel.Current;
end;

end.
