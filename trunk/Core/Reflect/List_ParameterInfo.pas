unit List_ParameterInfo;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBasicList,
  InfraCommonIntf,
  InfraCommon;

type
  _ITERABLELIST_BASE_ = TBaseElement;
  _ITERABLELIST_INTF_ = IParameterInfoList;
  _ITEM_INTF_ = IParameterInfo;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  end;

  TParameterInfoList = class(_ITERABLELIST_);

implementation

uses
  SysUtils;

{ TParameterInfoList }

{$I ..\Templates\InfraTempl_IntfList.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

end.

