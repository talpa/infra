unit List_MVPItem;

{$I Infra.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Contnrs, ReferenceImpl, InfraMVPInterfaces, InfraConsts;

type
  {.$DEFINE EQUAL_INDEX_DEFAULT Implementing Here}
  {$DEFINE EQUAL_VALUE_DEFAULT}
  {.$DEFINE INVALID_INDEX_DEFAULT Implementing Here}
  {$DEFINE INVALID_VALUE_DEFAULT}
  _ITERABLELIST_BASE_ = TMemoryManagedObject;// List's Class Base
  _ITERABLELIST_INTF_ = IMVPItemList;        // List's Interface Implementing
  _ITERATOR_INTF_ = IMVPItemIterator;        // List's Interface Implementing
  _INDEX_ = TGUID;                                // List's Item Index ===>>> TGUID
  _VALUE_ = IMVPItem;                        // List's Item Valeu
  {$I Templates\InfraTempl_ListDynIndex.inc}
    function IsIndexEqual(Index1, Index2: _INDEX_): boolean;
    function InvalidIndex: _INDEX_;
    function Append(Value: _VALUE_): _VALUE_; virtual;
  end;

  TMVPItemList = class(_ITERABLELIST_);

implementation

uses SysUtils;

{ TMVPItems }

{$I Templates\InfraTempl_ListDynIndex.inc}

function _ITERABLELIST_.IsIndexEqual(Index1, Index2: _INDEX_): boolean;
begin
  Result := IsEqualGUID(Index1, Index2);
end;

function _ITERABLELIST_.InvalidIndex: _INDEX_;
begin
  Result := NullGuid;
end;

function _ITERABLELIST_.Append(Value: _VALUE_): _VALUE_;
begin
  Add(Value.ItemId, Value);
  Result := Value;
end;

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

end.
