{
  To use this generic, you must set a few things before the include. Define:

  type
    _OBJECTLIST_BASE_ = TObject;                        // List's Class Base
    _ITEM_              = TClassicSubscription;           // List's Item
      // $I Define the include file here.
      // you can put specific methods and properties here
    end;
    // Now you are free to assign to other type
    TSubscriptions = _OBJECTLIST_;
  ...
  implementation
    // Define the include file again, now to get the implementation.
    // you can put specific methods implementatios here
  end;
}
{$IFNDEF SECOND_PASS}
_OBJECTLIST_ = class(_OBJECTLIST_BASE_)
private
  FItems: TObjectList;
  function GetItem(AIndex: Integer): _ITEM_;
  procedure SetItem(Index: Integer; const Value: _ITEM_);
  function GetCount: Integer;
public
  function Add(const Item: _ITEM_): Integer; virtual;
  function First: _ITEM_; virtual;
  function IndexOf(const Item: _ITEM_): Integer; virtual;
  function Last: _ITEM_; virtual;
  function Remove(const Item: _ITEM_): Integer; virtual;
  procedure Clear; virtual;
  procedure Delete(Index: Integer); virtual;
  procedure Exchange(Index1, Index2: Integer); virtual;
  procedure Insert(Index: Integer; const Item: _ITEM_); virtual;
  property Count: Integer read GetCount;
  property Items[Index: Integer]: _ITEM_ read GetItem write SetItem; default;
  constructor Create;
  destructor Destroy; override;
{$DEFINE SECOND_PASS}
{$ELSE}
{ _OBJECTLIST_ }

function _OBJECTLIST_.GetItem(AIndex: Integer): _ITEM_;
begin
  Result := _ITEM_(FItems[AIndex]);
end;

procedure _OBJECTLIST_.SetItem(Index: Integer; const Value: _ITEM_);
begin
  FItems[Index] := Value;
end;

function _OBJECTLIST_.Add(const Item: _ITEM_): Integer;
begin
  Result := FItems.Add(Item);
end;

function _OBJECTLIST_.First: _ITEM_;
begin
  Result := _ITEM_(FItems.First);
end;

function _OBJECTLIST_.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function _OBJECTLIST_.IndexOf(const Item: _ITEM_): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

function _OBJECTLIST_.Last: _ITEM_;
begin
  Result := _ITEM_(FItems.Last);
end;

function _OBJECTLIST_.Remove(const Item: _ITEM_): Integer;
begin
  Result := FItems.Remove(Item);
end;

procedure _OBJECTLIST_.Clear;
begin
  FItems.Clear;
end;

procedure _OBJECTLIST_.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

procedure _OBJECTLIST_.Exchange(Index1, Index2: Integer);
begin
  FItems.Exchange(Index1, Index2);
end;

procedure _OBJECTLIST_.Insert(Index: Integer; const Item: _ITEM_);
begin
  FItems.Insert(Index, Item);
end;

constructor _OBJECTLIST_.Create;
begin
  inherited;
  FItems := TObjectList.Create;
end;

// ***retornar o destroy para cá
{$ENDIF}
