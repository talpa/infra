unit List_InjectedItem;

{$I Infra.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBasicList,
  InfraCommonIntf,
  InfraCommon;

type
  _ITERABLELIST_BASE_ = TMemoryManagedObject;
  _ITERABLELIST_INTF_ = IInjectedList;
  _ITEM_INTF_ = IInjectedItem;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
    function IndexByGUID(const Item: TGUID): Integer;
  end;

  TAnnotationsIterator = class(TInterfacedObject, IAnnotationsIterator)
  private
    FCurrentIndex: integer;
    FList: _ITERABLELIST_INTF_;
  protected
    function CurrentItem: _ITEM_INTF_;
    procedure First; virtual;
    function IsDone: Boolean; virtual;
    procedure Next; virtual;
  public
    constructor Create(const List: _ITERABLELIST_INTF_);
  end;

  TInjectedList = class(_ITERABLELIST_);

implementation

uses
  SysUtils;

{ _ITERABLELIST_ }

{$I ..\Templates\InfraTempl_IntfList.inc}

function _ITERABLELIST_.IndexByGUID(const Item: TGUID): Integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FItems.Count-1 do
    if IsEqualGUID((FItems[i] as _ITEM_INTF_).ID, Item) then
    begin
      Result := i;
      Break;
    end;
end;

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

{ TAnnotationsIterator }

constructor TAnnotationsIterator.Create(const List: _ITERABLELIST_INTF_);
begin
  inherited Create;
  FList := List;
  First;
end;

function TAnnotationsIterator.CurrentItem: _ITEM_INTF_;
begin
  if Assigned(FList) and (fCurrentIndex <> -1)
    and (fCurrentIndex < FList.Count) then
    Result := FList[fCurrentIndex] as _ITEM_INTF_
  else
    Result := nil;
end;

procedure TAnnotationsIterator.First;
var
  i: integer;
begin
  fCurrentIndex := -1;
  if FList.Count > 0 then
    for i := 0 to Pred(FList.Count) do
    begin
      if (FList[i] as _ITEM_INTF_).IsAnnotation then
      begin
        fCurrentIndex := i;
        Break;
      end;
    end;
end;

function TAnnotationsIterator.IsDone: Boolean;
begin
  Result := (fCurrentIndex < 0) or (fCurrentIndex >= FList.Count);
end;

procedure TAnnotationsIterator.Next;
begin
  if (FList.Count > 0) then
    while (FCurrentIndex < FList.Count) and
      not (FList[fCurrentIndex+1] as _ITEM_INTF_).IsAnnotation do
      Inc(fCurrentIndex);
end;

end.
