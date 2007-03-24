unit List_ClassInfo;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBasicList,
  InfraCommonIntf,
  InfraCommon;

type
  _ITERABLELIST_BASE_ = TMemoryManagedObject;
  _ITERABLELIST_INTF_ = IClassInfoList;
  _ITEM_INTF_ = IClassInfo;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
    function ByClass(pClass: TClass): IClassInfo;
    function ByGUID(DataType: TGUID): IClassInfo;
    function ByName(const pName: string): IClassInfo;
  end;

  TClassInfoList = class(_ITERABLELIST_);

implementation

uses
  SysUtils;

{ TClassInfoList }

{$I ..\Templates\InfraTempl_IntfList.inc}

function _ITERABLELIST_.ByClass(pClass: TClass): _ITEM_INTF_;
var
  i: Integer;
  vClassInfo: _ITEM_INTF_;
begin
  Result := nil;
  for i := 0 to FItems.Count-1 do
  begin
    vClassInfo := (FItems[i] as _ITEM_INTF_);
    if vClassInfo.ImplClass = pClass then
    begin
      Result := vClassInfo;
      Break;
    end;
  end;
end;

function _ITERABLELIST_.ByGUID(DataType: TGUID): _ITEM_INTF_;
var
  i: Integer;
  vClassInfo: _ITEM_INTF_;
begin
  Result := nil;
  for i := 0 to FItems.Count-1 do
  begin
    vClassInfo := (FItems[i] as _ITEM_INTF_);
    if IsEqualGUID(vClassInfo.TypeID, DataType) then
    begin
      Result := vClassInfo;
      Break;
    end;
  end;
end;

function _ITERABLELIST_.ByName(const pName: string): _ITEM_INTF_;
var
  i: Integer;
  vClassInfo: _ITEM_INTF_;
begin
  Result := nil;
  for i := 0 to FItems.Count-1 do
  begin
    vClassInfo := (FItems[i] as _ITEM_INTF_);
    if SameText(vClassInfo.Name, pName) then
    begin
      Result := vClassInfo;
      Break;
    end;
  end;
end;

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

end.
