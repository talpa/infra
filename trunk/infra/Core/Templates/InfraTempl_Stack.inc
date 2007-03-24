{
  To use this generic, you must set a few things before the include. Define:

  type
    _STACK_BASE_ = TInterfacedObject;                  // Stack's Class Base
    _STACK_INTF_ = IInfraTypeInfoList;                 // Stack's Interface
    _ITEM_INTF_ = IInfraTypeInfo;                      // List's Item Interface
    _ITERATOR_INTF_ = IInfraIterator;               // Iterator's Interface
      // $I Define the include file here.
      // you can put specific methods and properties here
    end;
    // Now you are free to assign to other type
    TSomeStack = _STACK_;
  ...
  implementation
    // Define the include file again, now to get the implementation.
    // you can put specific methods implementatios here
  end;
}
{$IFNDEF SECOND_PASS}
  _STACK_ = class;

  _ITERATOR_ = class(TInterfacedObject, _ITERATOR_INTF_)
  private
    FCurrentIndex: integer;
    FList: _STACK_;
  protected
    function CurrentItem: IInterface;
    procedure First; virtual;
    function IsDone: Boolean; virtual;
    procedure Next; virtual;
  public
    constructor Create(List: _STACK_);
  end;

  _STACK_ = class(_STACK_BASE_, _STACK_BASE_INTF_)
  private
    FItems: TInfraCustomList;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): _ITEM_INTF_;
    procedure SetItem(Index: Integer; const Value: _ITEM_INTF_);    
  protected
    function AtLeast(ACount: Integer): Boolean; virtual;
    function Peek: _ITEM_INTF_; virtual;
    function Pop: _ITEM_INTF_;
    function Push(AItem: _ITEM_INTF_): _ITEM_INTF_;
    function NewIterator: _ITERATOR_INTF_; virtual;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: _ITEM_INTF_ read GetItem write SetItem; default;
  public
    constructor Create;
    destructor Destroy; override;
{$DEFINE SECOND_PASS}
{$ELSE}
{ _ITERATOR_ }

constructor _ITERATOR_.Create(List: _STACK_);
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

{ _STACK_ }

constructor _STACK_.Create;
begin
  inherited;
  FItems := TInfraCustomList.Create;
end;

function _STACK_.GetItem(AIndex: Integer): _ITEM_INTF_;
begin
  Result := (FItems[AIndex] as _ITEM_INTF_);
end;

procedure _STACK_.SetItem(Index: Integer; const Value: _ITEM_INTF_);
begin
  FItems[Index] := Value;
end;

function _STACK_.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function _STACK_.AtLeast(ACount: Integer): Boolean;
begin
  Result := (FItems.Count >= ACount);
end;

function _STACK_.Peek: _ITEM_INTF_;
begin
  if FItems.Count = 0 then
    Result := nil
  else
    Result := _ITEM_INTF_(FItems[FItems.Count-1]);
end;

function _STACK_.Pop: _ITEM_INTF_;
begin
  if Count <> 0 then
  begin
    Result := _ITEM_INTF_(FItems[FItems.Count-1]);
    FItems.Remove(FItems[FItems.Count-1]);
  end else
    Result := nil;
end;

function _STACK_.NewIterator: _ITERATOR_INTF_;
begin
  Result := _ITERATOR_.Create(Self) as _ITERATOR_INTF_;
end;

function _STACK_.Push(AItem: _ITEM_INTF_): _ITEM_INTF_;
begin
  Result := AItem;
  FItems.Add(AItem);
end;

// ***retornar o destroy para cá
{$ENDIF}
