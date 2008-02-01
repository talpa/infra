unit List_EntityPersister;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Contnrs,
  InfraCommonIntf,
  InfraHibernateIntf,
  InfraCommon;

type
  {$DEFINE EQUAL_INDEX_DEFAULT Implementing Here}
  {$DEFINE EQUAL_VALUE_DEFAULT}
  {$DEFINE INVALID_INDEX_DEFAULT Implementing Here}
  {$DEFINE INVALID_VALUE_DEFAULT}
  _ITERABLELIST_BASE_ = TBaseElement;        // List's Class Base
  _ITERABLELIST_INTF_ = IEntityPersisters;   // List's Intf Implementing
  _ITERATOR_INTF_ = IInterface;              // Iterator's Intf Implementing
  _INDEX_ = IClassInfo;                      // List's Item Index => IClassInfo
  _VALUE_ = IEntityPersister;                // List's Item Valeu
  {$I ..\Templates\InfraTempl_ListDynIndex.inc}
  end;

  TEntityPersisters = class(_ITERABLELIST_);

implementation

uses
  SysUtils;

{ TEntityPersisters }

{$I ..\Templates\InfraTempl_ListDynIndex.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

end.

