{
  To use this generic, you must set a few things before the include. Define:

  type
    _ITERABLELIST_BASE_ = TElement;            // List's Class Base
    _ITERABLELIST_INTF_ = IInfraTypeInfoList;       // List's Interface
    _ITEM_INTF_ = IInfraTypeInfo;                   // List's Item Interface
    _ITERATOR_INTF_ = IInfraIterator;               // Iterator's Interface
      // $I Define the include file here.
      // you can put specific methods and properties here
    end;
    // Now you are free to assign to other type
    TInfraTypeInfoList = _ITERABLELIST_;
  ...
  implementation
    // Define the include file again, now to get the implementation.
    // you can put specific methods implementatios here
  end;
}
{$IFNDEF SECOND_PASS}
  _ITERABLELIST_ = class;

  _ITERATOR_ = class(TInterfacedObject, _ITERATOR_INTF_)
  private
    FCurrentIndex: integer;
    FList: _ITERABLELIST_;
  protected
    function CurrentItem: IInterface;
    procedure First; virtual;
    function IsDone: Boolean; virtual;
    procedure Next; virtual;
  public
    constructor Create(List: _ITERABLELIST_);
  end;

  _ITERABLELIST_ = class(_ITERABLELIST_BASE_, _ITERABLELIST_INTF_)
  private
    FItems: TInfraCustomList;
    function GetItem(AIndex: Integer): _ITEM_INTF_;
    procedure SetItem(Index: Integer; const Value: _ITEM_INTF_);
  protected
    function Add(const Item: _ITEM_INTF_): Integer; virtual;
    function First: _ITEM_INTF_; virtual;
    function GetCount: Integer; virtual;
    function NewIterator: _ITERATOR_INTF_; virtual;
    function IndexOf(const Item: _ITEM_INTF_): Integer; virtual;
    function Last: _ITEM_INTF_; virtual;
    function Remove(const Item: _ITEM_INTF_): Integer; virtual;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer); virtual;
    procedure Insert(Index: Integer; const Item: _ITEM_INTF_); virtual;
    procedure Lock; virtual;
    procedure Unlock; virtual;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: _ITEM_INTF_ read GetItem write SetItem; default;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
{$DEFINE SECOND_PASS}
{$ELSE}
{ _ITERATOR_ }

constructor _ITERATOR_.Create(List: _ITERABLELIST_);
begin
  inherited Create;
  FList := List;
  First;
end;

function _ITERATOR_.CurrentItem: IInterface;
begin
  if Assigned(FList) and (fCurrentIndex <> -1)
    and (fCurrentIndex < FList.Count) then
    Result := FList[fCurrentIndex]
  else
    Result := nil;
end;

procedure _ITERATOR_.First;
begin
  if FList.Count > 0 then
    fCurrentIndex := 0
  else
    fCurrentIndex := -1;
end;

function _ITERATOR_.IsDone: Boolean;
begin
  Result := (fCurrentIndex < 0) or (fCurrentIndex >= FList.Count);
end;

procedure _ITERATOR_.Next;
begin
  if (FList.Count > 0) and (FCurrentIndex < FList.Count) then
    Inc(fCurrentIndex);
end;

{ _ITERABLELIST_ }

constructor _ITERABLELIST_.Create;
begin
  inherited;
  FItems := TInfraCustomList.Create;
end;

function _ITERABLELIST_.GetItem(AIndex: Integer): _ITEM_INTF_;
begin
  Result := (FItems[AIndex] as _ITEM_INTF_);
end;

procedure _ITERABLELIST_.SetItem(Index: Integer; const Value: _ITEM_INTF_);
begin
  FItems[Index] := Value;
end;

function _ITERABLELIST_.Add(const Item: _ITEM_INTF_): Integer;
begin
  Result := FItems.Add(Item);
end;

function _ITERABLELIST_.First: _ITEM_INTF_;
begin
  Result := (FItems.First as _ITEM_INTF_);
end;

function _ITERABLELIST_.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function _ITERABLELIST_.NewIterator: _ITERATOR_INTF_;
begin
  Result := _ITERATOR_.Create(Self) as _ITERATOR_INTF_;
end;

function _ITERABLELIST_.IndexOf(const Item: _ITEM_INTF_): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

function _ITERABLELIST_.Last: _ITEM_INTF_;
begin
  Result := (FItems.Last as _ITEM_INTF_);
end;

function _ITERABLELIST_.Remove(const Item: _ITEM_INTF_): Integer;
begin
  Result := FItems.Remove(Item);
end;

procedure _ITERABLELIST_.Clear;
begin
  FItems.Clear;
end;

procedure _ITERABLELIST_.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

procedure _ITERABLELIST_.Exchange(Index1, Index2: Integer);
begin
  FItems.Exchange(Index1, Index2);
end;

procedure _ITERABLELIST_.Insert(Index: Integer; const Item: _ITEM_INTF_);
begin
  FItems.Insert(Index, Item);
end;

procedure _ITERABLELIST_.Lock;
begin
  FItems.Lock;
end;

procedure _ITERABLELIST_.UnLock;
begin
  FItems.UnLock;
end;

// *** destroy aqui
{$ENDIF}
