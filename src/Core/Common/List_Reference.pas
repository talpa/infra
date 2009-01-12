unit List_Reference;

interface

{$I InfraCommon.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBase,
  Contnrs;

type
  {$DEFINE EQUAL_INDEX_DEFAULT}
  {$DEFINE EQUAL_VALUE_DEFAULT}
  {$DEFINE INVALID_INDEX_DEFAULT}
  {$DEFINE INVALID_VALUE_DEFAULT}
  _ITERABLELIST_BASE_ = TInfraBaseObject;         // List's Class Base
  _ITERABLELIST_INTF_ = IInterface;               // List's Interface Implementing
  _ITERATOR_INTF_ = IInterface;                   // List's Interface Implementing
  _INDEX_ = Pointer;                              // List's Item Index
  _VALUE_ = Pointer;                              // List's Item Valeu

  {$I ..\Templates\InfraTempl_ListDynIndex.inc}
  end;

  TInfraReferenceList = class(_ITERABLELIST_);

implementation

uses SysUtils;

{ TInfraReferenceList }

{$I ..\Templates\InfraTempl_ListDynIndex.inc}

destructor _ITERABLELIST_.Destroy;
begin
  {$IFDEF INFRA_CLASSNAMEDESTROYED}
  SendDebug('<<< '+Self.ClassName);
  {$ENDIF}
  FreeAndNil(FItems);
  inherited;
end;

end.
