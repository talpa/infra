unit List_View;

{$I Infra.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraMVPIntf,
  InfraBasicList,
  InfraCommonIntf,
  InfraCommon;

type
  _ITERABLELIST_BASE_ = TMemoryManagedObject;
  _ITERABLELIST_INTF_ = IViewList;
  _ITEM_INTF_ = IView;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  private
    FOwner: _ITEM_INTF_;
  protected
    function GetOwner: _ITEM_INTF_;
    procedure SetOwner(const Value: _ITEM_INTF_);
    property Owner: _ITEM_INTF_ read GetOwner write SetOwner;
  end;

  TViewList = class(_ITERABLELIST_)
  protected
    function Add(const Item: _ITEM_INTF_): Integer; override;
    function Remove(const Item: _ITEM_INTF_): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const Item: _ITEM_INTF_); override;
  end;

implementation

uses
  SysUtils;

{ TViewListBase }

{$I ..\Templates\InfraTempl_IntfList.inc}

function _ITERABLELIST_.GetOwner: _ITEM_INTF_;
begin
  Result := FOwner;
end;

procedure _ITERABLELIST_.SetOwner(const Value: _ITEM_INTF_);
begin
  SetReference(IInterface(FOwner), Value);
end;

{ *** temp acho que pode jogar no template }
destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

{ TMVPViewList }

function TViewList.Add(const Item: _ITEM_INTF_): Integer;
begin
  Result := inherited Add(Item);
  if Result <> -1 then
    Item.ParentView := Owner;
end;

function TViewList.Remove(const Item: _ITEM_INTF_): Integer;
begin
  Item.ParentView := nil;
  Result := inherited Remove(Item);
end;

procedure TViewList.Clear;
var
  Iterator: _ITERATOR_INTF_;
begin
  Iterator := NewIterator;
  while not Iterator.IsDone do
  begin
    (Iterator.CurrentItem as _ITEM_INTF_).ParentView := nil;
    Iterator.Next;
  end;
  inherited;
end;

procedure TViewList.Delete(Index: Integer);
begin
  (Items[Index] as _ITEM_INTF_).ParentView := nil;
  inherited Delete(Index);
end;

procedure TViewList.Insert(Index: Integer; const Item: _ITEM_INTF_);
begin
  inherited Insert(Index, Item);
  (Items[Index] as _ITEM_INTF_).ParentView := Owner;
end;

end.
