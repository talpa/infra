unit List_Binding;

interface

{$I InfraBinding.Inc} 

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBindingIntf,
  InfraCommonIntf,
  InfraCommon,
  InfraBasicList;

type
  _ITERABLELIST_BASE_ = TElement;
  _ITERABLELIST_INTF_ = IBindingList;
  _ITEM_INTF_ = IBinding;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  end;

  TBindingList = class(_ITERABLELIST_);

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
 
