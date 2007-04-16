{

  Generic to list with any element as Index. See List_Reference.pas to sample.

  To use this generic, you must set a few things before the include. Define:

  type
    _ITERABLELIST_BASE_ = TAnyThingClass;           // List's Class Base
    _ITERABLELIST_INTF_ = IAnyThingInterface;       // List's Interface Implementing
    _ITERATOR_INTF_ = IAnyThingInterface;           // Iterator's Interface
    _INDEX_ = IorTAnyThing;                         // List's Index
    _VALUE_ = IorTAnyThing;                         // List's Item
      // $I Define the include file here.
      // you can put specific methods and properties here
    end;
    // Now you are free to assign to other type
    TMySelfDynamicIndexList = _ITERABLELIST_;
  ...
  implementation
    // Define the include file again, now to get the implementation.
    // you can put specific methods implementatios here
  end;
}
{$IFNDEF SECOND_PASS}
  _ITEM_ = class(TObject)
  private
    FItemIndex: _INDEX_;
    FItemValue: _VALUE_;
  public
    property ItemIndex: _INDEX_ read FItemIndex write FItemIndex;
    property ItemValue: _VALUE_ read FItemValue write FItemValue;
    constructor Create(pIndex: _INDEX_; pValue: _VALUE_);
  end;

  _ITERABLELIST_ = class;

  _ITERATOR_ = class(TInterfacedObject, _ITERATOR_INTF_)
  private
    FCurrentIndex: integer;
    FList: _ITERABLELIST_;
  protected
    function CurrentItem: _VALUE_; virtual;
    procedure First; virtual;
    function IsDone: Boolean; virtual;
    procedure Next; virtual;
  public
    constructor Create(List: _ITERABLELIST_);
  end;

  _ITERABLELIST_ = class(_ITERABLELIST_BASE_, _ITERABLELIST_INTF_)
  private
    FItems: TObjectList;
    function GetItem(Index: _INDEX_): _VALUE_;
    procedure SetItem(Index: _INDEX_; Value: _VALUE_);
  protected
    function PositionOfIndex(Index: _INDEX_): Integer;
    function PositionOfValue(Value: _VALUE_): Integer;
    function First: _VALUE_; virtual;
    function GetCount: Integer; virtual;
    function IndexOf(Item: _VALUE_): _INDEX_; virtual;
    function Last: _VALUE_; virtual;
    function Remove(Item: _VALUE_): _INDEX_; virtual;
    procedure Clear; virtual;
    procedure DeleteAll(Index: _INDEX_); virtual;
    procedure Exchange(Index1, Index2: _INDEX_); virtual;
    procedure Insert(Index: _INDEX_; Item: _VALUE_); virtual;
  public
    {$IFDEF INVALID_INDEX_DEFAULT}
    function InvalidIndex: _INDEX_;{$ENDIF}
    {$IFDEF INVALID_VALUE_DEFAULT}
    function InvalidValue: _VALUE_;{$ENDIF}
    {$IFDEF EQUAL_INDEX_DEFAULT}
    function IsIndexEqual(Index1, Index2: _INDEX_): boolean;{$ENDIF}
    {$IFDEF EQUAL_VALUE_DEFAULT}
    function IsValueEqual(Value1, Value2: _VALUE_): boolean;{$ENDIF}
    function Add(Index: _INDEX_; Value: _VALUE_): _INDEX_; virtual;
    procedure Delete(Index: _INDEX_); virtual;
    procedure DeletePosition(Index: integer);
    function NewIterator: _ITERATOR_INTF_; virtual;
    function PositionOf(Index: _INDEX_; Value: _VALUE_): integer;
    function ValueOfPosition(Index: Integer): _VALUE_;
    function IndexOfPosition(Index: Integer): _INDEX_;
    constructor Create; overload; override; 
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: _INDEX_]: _VALUE_ read GetItem write SetItem; default;
{$DEFINE SECOND_PASS}
{$ELSE}
{ _ITEM_ }
constructor _ITEM_.Create(pIndex: _INDEX_; pValue: _VALUE_);
begin
  inherited Create;
  FItemIndex := pIndex;
  FItemValue := pValue;
end;

{ _ITERATOR_ }

constructor _ITERATOR_.Create(List: _ITERABLELIST_);
begin
  inherited Create;
  FList := List;
  First;
end;

function _ITERATOR_.CurrentItem: _VALUE_;
begin
  if Assigned(FList) and (fCurrentIndex <> -1)
    and (fCurrentIndex < FList.Count) then
    Result := _ITERABLELIST_(FList).ValueOfPosition(fCurrentIndex)
  else
    Result := FList.InvalidValue;
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

function _ITERABLELIST_.GetItem(Index: _INDEX_): _VALUE_;
var
  i: integer;
begin
  i := PositionOfIndex(Index);
  if i >= 0 then
    Result := _ITEM_(FItems[i]).ItemValue
  else
    Result := InvalidValue;
end;

procedure _ITERABLELIST_.SetItem(Index: _INDEX_; Value: _VALUE_);
var
  i: integer;
begin
  i := PositionOfIndex(Index);
  if i >= 0 then
    _ITEM_(FItems[i]).ItemValue := Value
  else
    Add(Index, Value);
end;

{$IFDEF EQUAL_INDEX_DEFAULT}
function _ITERABLELIST_.IsIndexEqual(Index1, Index2: _INDEX_): boolean;
begin
  Result := (Index1 = Index2);
end;
{$ENDIF}

{$IFDEF EQUAL_VALUE_DEFAULT}
function _ITERABLELIST_.IsValueEqual(Value1, Value2: _VALUE_): boolean;
begin
  Result := (Value1 = Value2);
end;
{$ENDIF}

{$IFDEF INVALID_INDEX_DEFAULT}
function _ITERABLELIST_.InvalidIndex: _INDEX_;
begin
  Result := nil;
end;
{$ENDIF}

{$IFDEF INVALID_VALUE_DEFAULT}
function _ITERABLELIST_.InvalidValue: _VALUE_;
begin
  Result := nil;
end;
{$ENDIF}

function _ITERABLELIST_.PositionOf(Index: _INDEX_; Value: _VALUE_): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FItems.Count-1 do
    if IsIndexEqual(_ITEM_(FItems[i]).ItemIndex, Index) and
      IsValueEqual(_ITEM_(FItems[i]).ItemValue, Value) then
    begin
      Result := i;
      Break;
    end;
end;

function _ITERABLELIST_.PositionOfIndex(Index: _INDEX_): Integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FItems.Count-1 do
    if IsIndexEqual(_ITEM_(FItems[i]).ItemIndex, Index) then
    begin
      Result := i;
      Break;
    end;
end;

function _ITERABLELIST_.PositionOfValue(Value: _VALUE_): Integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FItems.Count-1 do
    if IsValueEqual(_ITEM_(FItems[i]).ItemValue, Value) then
    begin
      Result := i;
      Break;
    end;
end;

function _ITERABLELIST_.ValueOfPosition(Index: Integer): _VALUE_;
begin
  if (Index >= 0) and (Index <= FItems.Count-1) then
    Result := _ITEM_(FItems[Index]).ItemValue
  else
    Result := InvalidValue;
end;

function _ITERABLELIST_.IndexOfPosition(Index: Integer): _INDEX_;
begin
  if (Index >= 0) and (Index <= FItems.Count-1) then
    Result := _ITEM_(FItems[Index]).ItemIndex
  else
    Result := InvalidIndex;
end;

function _ITERABLELIST_.Add(Index: _INDEX_; Value: _VALUE_): _INDEX_;
var
  TheItem: _ITEM_;
begin
  TheItem := _ITEM_.Create(Index, Value);
  FItems.Add(TheItem);
  Result := Index;
end;

function _ITERABLELIST_.First: _VALUE_;
begin
  Result := _ITEM_(FItems.First).ItemValue;
end;

function _ITERABLELIST_.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function _ITERABLELIST_.IndexOf(Item: _VALUE_): _INDEX_;
var
  i: integer;
begin
  i := PositionOfValue(Item);
  if i >= 0 then
    Result := _ITEM_(FItems[i]).ItemIndex
  else
    Result := InvalidIndex;
end;

function _ITERABLELIST_.Last: _VALUE_;
begin
  Result := _ITEM_(FItems.Last).ItemValue;
end;

function _ITERABLELIST_.Remove(Item: _VALUE_): _INDEX_;
var
  i: integer;
begin
  i := PositionOfValue(Item);
  if i >= 0 then
  begin
    Result := _ITEM_(FItems[i]).ItemIndex;
    FItems.Remove(FItems[i]);
  end else
    Result := InvalidIndex;
end;

procedure _ITERABLELIST_.Clear;
begin
  FItems.Clear;
end;

procedure _ITERABLELIST_.DeletePosition(Index: integer);
begin
  FItems.Delete(Index);
end;

procedure _ITERABLELIST_.Delete(Index: _INDEX_);
var
  i: integer;
begin
  i := PositionOfIndex(Index);
  if i >= 0 then
     FItems.Delete(i);
end;

procedure _ITERABLELIST_.DeleteAll(Index: _INDEX_);
var
  i: integer;
begin
  for i := FItems.Count-1 downto 0 do
    if IsIndexEqual(_ITEM_(FItems[i]).ItemIndex, Index) then
      FItems.Delete(i);
end;

procedure _ITERABLELIST_.Exchange(Index1, Index2: _INDEX_);
var
  i, j: integer;
begin
  i := PositionOfIndex(Index1);
  j := PositionOfIndex(Index2);
  if (i >= 0) and (j >= 0) then
    FItems.Exchange(i, j);
end;

procedure _ITERABLELIST_.Insert(Index: _INDEX_;  Item: _VALUE_);
var
  i: integer;
  TheItem: _ITEM_;
begin
  i := PositionOfIndex(Index);
  if i < 0 then
    i := 0;
  TheItem := _ITEM_.Create(Index, Item);
  FItems.Insert(i, TheItem);
end;

function _ITERABLELIST_.NewIterator: _ITERATOR_INTF_;
begin
  Result := _ITERATOR_.Create(Self) as _ITERATOR_INTF_;
end;

constructor _ITERABLELIST_.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
  FItems.OwnsObjects := True;
end;

// *** destroy aqui
{$ENDIF}
