unit InfraBindingType;

interface

uses
  InfraValueTypeIntf,
  InfraBindingIntf;

function GetBindableType(
  const pListModel: IBindableListModel): IBindable; overload;

function GetBindableType(const pValue: IProperty;
  const pExpression: string): IBindable; overload;

implementation

uses
  SysUtils,
  InfraCommonIntf,
  InfraBindingConsts,
  InfraBinding,
  InfraBindingConverter,
  InfraBindingManager;

type
  TBindableType = class(TBindable)
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

  TBindableTypeList = class(TBindable, IBindableList)
  private
    FListModel: IBindableListModel;
    function CurrentItemChangedFilter(const Event: IInfraEvent): Boolean;
    function ListChangedFilter(const Event: IInfraEvent): Boolean;
    procedure CurrentItemChanged(const Event: IInfraEvent);
    procedure ItemAdded(const Event: IInfraEvent);
    procedure ItemRemoved(const Event: IInfraEvent);
    procedure ItemsClear(const Event: IInfraEvent);
  protected
    function GetListModel: IBindableListModel;
    function GetValue: IInfraType; override;
    procedure SetListModel(const Value: IBindableListModel);
    procedure SetValue(const Value: IInfraType); override;
    property ListModel: IBindableListModel read GetListModel write SetListModel;
  public
    constructor Create; override;
  end;

{ TBindableType }

constructor TBindableType.Create(const pProperty: IProperty);
begin
  inherited Create;
  FModel := pProperty as IInfraType;
  EventService.Subscribe(IInfraChangedEvent, Self as ISubscriber,
    ValueChanged, EmptyStr, ValueChangedFilter);
end;

function TBindableType.GetValue: IInfraType;
begin
  Result := FModel;
end;

procedure TBindableType.SetValue(const Value: IInfraType);
begin
  FModel.Assign(Value);
end;

function TBindableType.Support2Way: Boolean;
begin
  Result := True;
end;

procedure TBindableType.ValueChanged(const Event: IInfraEvent);
begin
  Changed;
end;

function TBindableType.ValueChangedFilter(const Event: IInfraEvent): Boolean;
var
  vSource: IInfraType;
begin
  vSource := Event.Source as IInfraType;
  Result := vSource = FModel;
end;

{ TBindableTypeList }

constructor TBindableTypeList.Create;
begin
  inherited Create;
  EventService.Subscribe(IInfraAddItemEvent, Self as ISubscriber,
    ItemAdded, EmptyStr, ListChangedFilter);
  EventService.Subscribe(IInfraClearListEvent, Self as ISubscriber,
    ItemsClear, EmptyStr, ListChangedFilter);
  EventService.Subscribe(IInfraRemoveItemEvent, Self as ISubscriber,
    ItemRemoved, EmptyStr, ListChangedFilter);
  EventService.Subscribe(IInfraChangedEvent, Self as ISubscriber,
    CurrentItemChanged, EmptyStr, CurrentItemChangedFilter);
end;

function TBindableTypeList.GetListModel: IBindableListModel;
begin
  Result := FListModel;
end;

function TBindableTypeList.GetValue: IInfraType;
begin
  Result := FListModel as IInfraType;
end;

procedure TBindableTypeList.SetListModel(const Value: IBindableListModel);
begin
  FListModel := Value;
end;

procedure TBindableTypeList.SetValue(const Value: IInfraType);
begin
  Raise EInfraBindingError.CreateFmt(cErrorBindableisReadOnly,
    ['TBindableTypeList']);
end;

procedure TBindableTypeList.ItemAdded(const Event: IInfraEvent);
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

procedure TBindableTypeList.ItemRemoved(const Event: IInfraEvent);
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

procedure TBindableTypeList.ItemsClear(const Event: IInfraEvent);
begin
  with FListModel do
    Operation := loClear;
  Changed;
end;

procedure TBindableTypeList.CurrentItemChanged(const Event: IInfraEvent);
begin
  with FListModel do
  begin
    Operation := loSelectionChange;
    Current := Event.Source as IInfraType;
  end;
  Changed;
end;

function TBindableTypeList.ListChangedFilter(
  const Event: IInfraEvent): Boolean;
var
  vSource: IInfraType;
begin
  vSource := Event.Source as IInfraType;
  Result := vSource = FListModel.List;
end;

function TBindableTypeList.CurrentItemChangedFilter(
  const Event: IInfraEvent): Boolean;
var
  vSource: IInfraType;
begin
  vSource := Event.Source as IInfraType;
  Result := vSource = FListModel.Current;
end;

// Funções genéricas para criação de bindables para types

function GetBindableType(const pValue: IProperty;
  const pExpression: string): IBindable; overload;
var
  vObject: IInfraObject;
  vProperty: IProperty;
begin
  if Supports(pValue, IInfraObject, vObject) then
  begin
    vProperty := vObject.GetProperty(pExpression);
    Result := TBindableType.Create(vProperty);
  end else
    raise EInfraBindingError.Create(cErrorTypeNotSupportedToBinding);
end;

function GetBindableType(
  const pListModel: IBindableListModel): IBindable; overload;
begin
  Result := TBindableTypeList.Create;
end;

end.




