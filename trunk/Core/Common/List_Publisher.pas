unit List_Publisher;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraCommonIntf,
  InfraCommon,
  InfraBasicList,
  InfraNotify;

type
  _ITERABLELIST_BASE_ = TBaseElement;
  _ITERABLELIST_INTF_ = IInfraPublisherList;
  _ITEM_INTF_ = IInfraPublisher;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  end;

  TInfraPublisherList = class(_ITERABLELIST_);

implementation

uses
  SysUtils;

{ TInfraPublisherList }

{$I ..\Templates\InfraTempl_IntfList.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

end.
