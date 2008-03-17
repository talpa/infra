unit List_EntityPersister;

interface

{$I InfraPersistence.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Contnrs,
  InfraCommonIntf,
  InfraCommon,
  InfraHibernateIntf;

type
  {.$DEFINE EQUAL_INDEX_DEFAULT Implementing Here}
  {$DEFINE EQUAL_VALUE_DEFAULT}
  {.$DEFINE INVALID_INDEX_DEFAULT Implementing Here}
  {$DEFINE INVALID_VALUE_DEFAULT}
  _ITERABLELIST_BASE_ = TBaseElement;       // List's Class Base
  _ITERABLELIST_INTF_ = IEntityPersisters;  // List's Interface Implementing
  _ITERATOR_INTF_ = IInterface;             // List's Interface Implementing
  _INDEX_ = TGUID;                          // List's Item Index ===>>> TGUID
  _VALUE_ = IEntityPersister;               // List's Item Valeu
  {$I ..\Templates\InfraTempl_ListDynIndex.inc}
    function IsIndexEqual(Index1, Index2: _INDEX_): boolean;
    function InvalidIndex: _INDEX_;
  end;

  TEntityPersisters = class(_ITERABLELIST_);

implementation

uses
  SysUtils,
  InfraConsts;

{ TEntityPersisters }

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
