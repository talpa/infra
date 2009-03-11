unit  List_MappingControl;

interface

{$I ..\Common\InfraCommon.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Contnrs,
  InfraCommonIntf,
  InfraCommon,
  InfraBindingIntf,
  InfraValueTypeIntf;

type
  {$DEFINE EQUAL_INDEX_DEFAULT implementing here}
  {$DEFINE EQUAL_VALUE_DEFAULT implementing here}
  {$DEFINE INVALID_INDEX_DEFAULT implementing here}
  {$DEFINE INVALID_VALUE_DEFAULT implementing here}
  _ITERABLELIST_BASE_ = TBaseElement;          // List's Class Base
  _ITERABLELIST_INTF_ = IMappingControlList;   // List's Interface Implementing
  _ITERATOR_INTF_ = IInterface;                // List's Interface Implementing
  _INDEX_ = TClass;                            // List's Item Index ===>>> string
  _VALUE_ = TClass;                            // List's Item Value
  {$I ..\Templates\InfraTempl_ListDynIndex.inc}
//    function InvalidIndex: _INDEX_;
//    function InvalidValue: _VALUE_;
//    function IsIndexEqual(const Index1, Index2: _INDEX_): boolean;
//    function IsValueEqual(Value1, Value2: _VALUE_): boolean;
  end;

  TMappingControlList = class(_ITERABLELIST_);

implementation

uses
  SysUtils,
  InfraConsts;

{ TInfraSQLCommandParams }

{$I ..\Templates\InfraTempl_ListDynIndex.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

{
function _ITERABLELIST_.InvalidIndex: _INDEX_;
begin
  Result := nil;
end;

function _ITERABLELIST_.InvalidValue: _VALUE_;
begin
  Result := NullGUID;
end;

function _ITERABLELIST_.IsIndexEqual(const Index1, Index2: _INDEX_): boolean;
begin
  Result := (Index1 <> Index2);
end;

function _ITERABLELIST_.IsValueEqual(Value1, Value2: _VALUE_): boolean;
begin
  Result := IsEqualGUID(Value1, Value2);
end;
}

end.




