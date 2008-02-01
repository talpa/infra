unit List_GUIMapping;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBasicList,
  InfraCommonIntf,
  InfraGUIBuilderIntf,
  InfraCommon;

type
  _ITERABLELIST_BASE_ = TBaseElement;
  _ITERABLELIST_INTF_ = IGUIMappingList;
  _ITEM_INTF_ = IGUIMapping;
  _ITERATOR_INTF_ = IGUIMappingIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  end;

  TGUIMappingList = class(_ITERABLELIST_);

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
