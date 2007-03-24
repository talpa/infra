unit List_MVPItemInstance;

{$I Infra.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Contnrs, ReferenceImpl, InfraMVPInterfaces;

type
  {$DEFINE EQUAL_INDEX_DEFAULT Implementing Here}
  {$DEFINE EQUAL_VALUE_DEFAULT}
  {$DEFINE INVALID_INDEX_DEFAULT Implementing Here}
  {$DEFINE INVALID_VALUE_DEFAULT}
  _ITERABLELIST_BASE_ = TMemoryManagedObject;// List's Class Base
  _ITERABLELIST_INTF_ = IMVPItemsInstances;  // List's Interface Implementing
  _ITERATOR_INTF_ = IInterface;                   // List's Interface Implementing
  _INDEX_ = IMVPItem;                        // List's Item Index
  _VALUE_ = IElement;                        // List's Item Valeu
  {$I Templates\InfraTempl_ListDynIndex.inc}
  end;

  TMVPItemsInstances = class(_ITERABLELIST_);

implementation

uses SysUtils;

{ TMVPItemsInstances }

{$I Templates\InfraTempl_ListDynIndex.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

end.
