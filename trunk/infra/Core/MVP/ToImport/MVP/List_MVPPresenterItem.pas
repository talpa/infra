unit List_MVPPresenterItem;

{$I Infra.Inc}

interface

uses
  {$IFDEF INFRA_GXDEBUG}DBugIntf,{$ENDIF}
  SysUtils, Contnrs,
  InfraBasicList, ReferenceImpl, InfraInterfaces, InfraMVPInterfaces;

type
  _ITERABLELIST_BASE_ = TElement;
  _ITERABLELIST_INTF_ = IInfraMVPPresenterItemList;
  _ITEM_INTF_ = IInfraMVPPresenterItem;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I InfraTempl_IntfList.inc}
    function Append(const Item: _ITEM_INTF_): _ITEM_INTF_;
  end;

  TInfraMVPPresenterItemList = _ITERABLELIST_;

implementation

uses Types;

{ TInfraPresenterListBase }

{$I InfraTempl_IntfList.inc}

function _ITERABLELIST_.Append(const Item: _ITEM_INTF_): _ITEM_INTF_;
var
  i: Integer;
begin
  i := Add(Item);
  if i <> -1 then
    Result := Item
  else
    Result := nil;
end;

destructor _ITERABLELIST_.Destroy;
begin
  {$IFDEF INFRA_GXDEBUG}
  SendDebug('MVPPresenterItems Destroyied');
  {$ENDIF}
  FItems.Free;
  FItems := nil;
  inherited;
end;

end.
