{
  To use this generic, you must set a few things before the include. Define:

  type
    _LIST_INTF_ = INewInfraList;                    // List's Interface
    _ITEM_INTF_ = INewInfraType;                    // List's Item Interface
    _ITERATOR_INTF_ = IInfraIterator;               // Iterator's Interface
      // $I Define the include file here.
      // you can put specific methods and properties here
    end;
    // Now you are free to assign to other type
    TNewTypeInheritedOfInfraList = _INFRALIST_;
  ...
  implementation
    // $I Define the include file again, now to get the implementation.
    // you can put specific methods implementatios here
  end;
}
{$IFNDEF SECOND_PASS}
_INFRALIST_ = class(TInfraList, _LIST_INTF_)
protected
  function Add(const Item: _ITEM_INTF_): Integer; virtual;
  function First: _ITEM_INTF_; virtual;
  function GetItem(AIndex: Integer): _ITEM_INTF_;
  function IndexOf(const Item: _ITEM_INTF_): Integer; virtual;
  function Last: _ITEM_INTF_; virtual;
  function NewIterator: _ITERATOR_INTF_; virtual;
  function Remove(const Item: _ITEM_INTF_): Integer; virtual;
  procedure Insert(Index: Integer; const Item: _ITEM_INTF_); virtual;
  procedure SetItem(Index: Integer; const Value: _ITEM_INTF_);
  property Items[Index: Integer]: _ITEM_INTF_ read GetItem write SetItem; default;
{$DEFINE SECOND_PASS}
{$ELSE}
{ _ITERABLELIST_ }

function _ITERABLELIST_.Add(const Item: _ITEM_INTF_): Integer;
begin
  inherited Add(Item);
end;

function _ITERABLELIST_.First: _ITEM_INTF_;
begin
  Result := (inherited First as _ITEM_INTF_);
end;

function _ITERABLELIST_.GetItem(AIndex: Integer): _ITEM_INTF_;
begin
  Result := (inherited GetItem(AIndex) as _ITEM_INTF_);
end;

function _ITERABLELIST_.IndexOf(const Item: _ITEM_INTF_): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function _ITERABLELIST_.Last: _ITEM_INTF_;
begin
  Result := (inherited Last as _ITEM_INTF_);
end;

function _ITERABLELIST_.NewIterator: _ITERATOR_INTF_;
begin
  Result := (inherited NewIterator as _ITERATOR_INTF_);
end;

function _ITERABLELIST_.Remove(const Item: _ITEM_INTF_): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure _ITERABLELIST_.Insert(Index: Integer; const Item: _ITEM_INTF_);
begin
  inherited Insert(Index, Item);
end;

procedure _ITERABLELIST_.SetItem(Index: Integer; const Value: _ITEM_INTF_);
begin
  inherited SetItem(Index, Value);
end;
{$ENDIF}
