unit List_EventServiceItem;

{$I Infra.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Contnrs,
  InfraCommonIntf,
  InfraCommon;

type
  {.$DEFINE EQUAL_INDEX_DEFAULT Implementing Here}
  {$DEFINE EQUAL_VALUE_DEFAULT}
  {.$DEFINE INVALID_INDEX_DEFAULT Implementing Here}
  {$DEFINE INVALID_VALUE_DEFAULT}
  _ITERABLELIST_BASE_ = TMemoryManagedObject;// List's Class Base
  _ITERABLELIST_INTF_ = IInfraEventServiceItems;  // List's Interface Implementing
  _ITERATOR_INTF_ = IInterface;                   // List's Interface Implementing
  _INDEX_ = TGUID;                                // List's Item Index ===>>> TGUID
  _VALUE_ = IInfraEventServiceItem;               // List's Item Valeu
  {$I ..\Templates\InfraTempl_ListDynIndex.inc}
    function IsIndexEqual(Index1, Index2: _INDEX_): boolean;
    function InvalidIndex: _INDEX_;
  end;

  TInfraEventServiceItems = class(_ITERABLELIST_);

implementation

uses
  SysUtils,
  InfraConsts;

{ TInfraEventServiceItems }

{$I ..\Templates\InfraTempl_ListDynIndex.inc}

function _ITERABLELIST_.IsIndexEqual(Index1, Index2: _INDEX_): boolean;
begin
  Result := IsEqualGUID(Index1, Index2);
end;

function _ITERABLELIST_.InvalidIndex: _INDEX_;
begin
  Result := NullGuid;
end;

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

end.
