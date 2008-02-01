unit List_Screen;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBasicList,
  InfraCommonIntf,
  GUIAnnotationIntf,
  InfraCommon;

type
  _ITERABLELIST_BASE_ = TBaseElement;
  _ITERABLELIST_INTF_ = IScreenList;
  _ITEM_INTF_ = IScreen;
  _ITERATOR_INTF_ = IScreenIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  end;

  TScreenList = class(_ITERABLELIST_);

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
