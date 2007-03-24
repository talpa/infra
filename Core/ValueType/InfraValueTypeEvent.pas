unit InfraValueTypeEvent;

{$I Infra.Inc}

interface

uses
  InfraValueTypeIntf, InfraNotify;

type
  TInfraChangedEvent = class(TInfraEvent, IInfraChangedEvent);

  TInfraAddItemEvent = class(TInfraEvent, IInfraAddItemEvent)
  private
    FItemIndex: Integer;
    FNewItem: IInfraType;
  protected
    function GetItemIndex: Integer;
    function GetNewItem: IInfraType;
    procedure SetItemIndex(Value: Integer);
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property NewItem: IInfraType read GetNewItem;
  public
    constructor Create(const Src: IInfraType; const NewItem: IInfraType;
      ItemIndex: Integer);
  end;

  TInfraClearListEvent = class(TInfraEvent, IInfraClearListEvent);

  TInfraRemoveItemEvent = class(TInfraEvent, IInfraRemoveItemEvent)
  private
    FItemIndex: Integer;
    FRemovedItem: IInfraType;
  protected
    function GetItemIndex: Integer;
    function GetRemovedItem: IInfraType;
    property ItemIndex: Integer read GetItemIndex;
    property RemovedItem: IInfraType read GetRemovedItem;
  public
    constructor Create(const Src: IInfraType; const RemovedItem: IInfraType;
      ItemIndex: Integer);
  end;

implementation

{ TInfraAddItemEvent }

constructor TInfraAddItemEvent.Create(const Src, NewItem: IInfraType;
  ItemIndex: Integer);
begin
  inherited Create(Src);
  FNewItem := NewItem;
  FItemIndex := ItemIndex;
end;

function TInfraAddItemEvent.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

function TInfraAddItemEvent.GetNewItem: IInfraType;
begin
  Result := FNewItem;
end;

procedure TInfraAddItemEvent.SetItemIndex(Value: Integer);
begin
  if Value <> FItemIndex then
    FItemIndex := Value;
end;

{ TInfraRemoveItemEvent }

constructor TInfraRemoveItemEvent.Create(const Src, RemovedItem: IInfraType;
  ItemIndex: Integer);
begin
  inherited Create(Src);
  FRemovedItem := RemovedItem;
  FItemIndex := ItemIndex;
end;

function TInfraRemoveItemEvent.GetRemovedItem: IInfraType;
begin
  Result := FRemovedItem;
end;

function TInfraRemoveItemEvent.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

end.
