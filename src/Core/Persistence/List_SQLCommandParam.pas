// xxx
unit List_SQLCommandParam;

interface

{$I ..\Common\InfraCommon.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Contnrs,
  InfraCommonIntf,
  InfraCommon,
  InfraPersistenceIntf,
  InfraValueTypeIntf;

type
  {.$DEFINE EQUAL_INDEX_DEFAULT}
  {$DEFINE EQUAL_VALUE_DEFAULT}
  {.$DEFINE INVALID_INDEX_DEFAULT implementing here}
  {$DEFINE INVALID_VALUE_DEFAULT}
  _ITERABLELIST_BASE_ = TBaseElement;       // List's Class Base
  _ITERABLELIST_INTF_ = ISQLCommandParams;  // List's Interface Implementing
  _ITERATOR_INTF_ = IInterface;             // List's Interface Implementing
  _INDEX_ = string;                         // List's Item Index ===>>> string
  _VALUE_ = IInfraType;                     // List's Item Value
  {$I ..\Templates\InfraTempl_ListDynIndex.inc}
    function InvalidIndex: _INDEX_;
    function IsIndexEqual(const Index1, Index2: _INDEX_): boolean;
    procedure AddObject(const Value: IInfraObject);
  end;

  TSQLCommandParams = class(_ITERABLELIST_);

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

function _ITERABLELIST_.InvalidIndex: _INDEX_;
begin
  Result := EmptyStr;
end;

function _ITERABLELIST_.IsIndexEqual(const Index1, Index2: _INDEX_): boolean;
begin
  Result := AnsiSameText(Index1, Index2);
end;

procedure _ITERABLELIST_.AddObject(const Value: IInfraObject);
var
  vIterator: IPropertyInfoIterator;
  vParamValue, vPropertyValue: IInfraType;  
begin
  if Assigned(Value) then
  begin
    vIterator := TypeService.GetType(Value.TypeInfo.TypeID).GetProperties;
    while not vIterator.IsDone do
    begin
      vPropertyValue := vIterator.CurrentItem.GetValue(Value) as IInfraType;
      if not vPropertyValue.IsNull then
      begin
        vParamValue := TypeService.CreateInstance(vIterator.CurrentItem.GetTypeInfo.TypeID) as IInfraType;
        vParamValue.Assign(vPropertyValue);
        Add(vIterator.CurrentItem.Name, vParamValue);
      end;
      vIterator.Next;
    end;
  end;
end;

end.