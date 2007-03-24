unit List_Literal;

{$I Infra.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraValueTypeIntf,
  InfraCommonIntf,
  InfraCommon,
  InfraBasicList;

type
  // *** Estava _ITERABLELIST_BASE_ = TMemoryManagedObject;
  _ITERABLELIST_BASE_ = TElement;
  _ITERABLELIST_INTF_ = IInfraLiteralList;
  _ITEM_INTF_ = IInfraLiteral;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  end;

  TInfraLiteralList = class(_ITERABLELIST_);

implementation

uses
  SysUtils;

{ TInfraLiteralList }

{$I ..\Templates\InfraTempl_IntfList.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

end.
