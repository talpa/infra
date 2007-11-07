unit List_GUIControl;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBasicList,
  InfraCommonIntf,
  InfraGUIBuilderIntf,
  InfraCommon;

type
  _ITERABLELIST_BASE_ = TMemoryManagedObject;
  _ITERABLELIST_INTF_ = IGUIControlList;
  _ITEM_INTF_ = IGUIControl;
  _ITERATOR_INTF_ = IGUIControlIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  end;

  TGUIControlList = class(_ITERABLELIST_);

implementation

uses
  SysUtils;

{ TTypeMappingList }

{$I ..\Templates\InfraTempl_IntfList.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

end.
