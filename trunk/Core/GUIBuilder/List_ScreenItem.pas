unit List_ScreenItem;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBasicList,
  InfraCommonIntf,
  GUIAnnotationIntf,
  InfraCommon;

type
  _ITERABLELIST_BASE_ = TMemoryManagedObject;
  _ITERABLELIST_INTF_ = IScreenItemList;
  _ITEM_INTF_ = IScreenItem;
  _ITERATOR_INTF_ = IScreenItemIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  end;

  TScreenItemList = class(_ITERABLELIST_);

implementation

uses
  SysUtils;

{$I ..\Templates\InfraTempl_IntfList.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

end.
