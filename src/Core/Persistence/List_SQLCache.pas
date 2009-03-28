unit List_SQLCache;

interface

{$I ..\Common\InfraCommon.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Contnrs,
  InfraCommonIntf,
  InfraCommon,
  InfraOPFIntf,
  InfraValueTypeIntf;

 type
  {$DEFINE EQUAL_INDEX_DEFAULT}
  {$DEFINE EQUAL_VALUE_DEFAULT}
  {.$DEFINE INVALID_INDEX_DEFAULT implementing here}
  {.$DEFINE INVALID_VALUE_DEFAULT}
  _ITERABLELIST_BASE_ = TBaseElement;       // List's Class Base
  _ITERABLELIST_INTF_ = ISQLCacheList;      // List's Interface Implementing
  _ITERATOR_INTF_ = IInterface;             // List's Interface Implementing
  _INDEX_ = string;                         // List's Item Index ===>>> string
  _VALUE_ = String;                         // List's Item Value
  {$I ..\Templates\InfraTempl_ListDynIndex.inc}
    function InvalidIndex: _INDEX_;
    function InvalidValue: _VALUE_;
  end;
  TInfraSQLCache = class(_ITERABLELIST_);


implementation

uses
  SysUtils,
  InfraConsts;

{ TInfraSQLCache }

{$I ..\Templates\InfraTempl_ListDynIndex.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function _ITERABLELIST_.InvalidIndex: _INDEX_;
begin
  Result := EmptyStr;
end;

function _ITERABLELIST_.InvalidValue: _VALUE_;
begin
  Result := EmptyStr;
end;


end.

