unit List_ClassInfo;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBasicList,
  InfraCommonIntf,
  InfraBase,
  InfraCommon;

type
  _ITERABLELIST_BASE_ = TMemoryManagedObject;
  _ITERABLELIST_INTF_ = IClassInfoList;
  _ITEM_INTF_ = IClassInfo;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
    function ByClass(pClass: TInfraBaseObjectClass): IClassInfo;
    function ByGUID(DataType: TGUID): IClassInfo;
    function ByName(const pName: string): IClassInfo;
  end;

  TClassInfoIterator = class(TInterfacedObject, IClassInfoIterator)
  private
    FCurrentIndex: integer;
    FList: IClassInfoList;
  protected
    function CurrentItem: IClassInfo;
    procedure First; virtual;
    function IsDone: Boolean; virtual;
    procedure Next; virtual;
  public
    constructor Create(const List: IClassInfoList);
  end;

  TClassInfoList = class(_ITERABLELIST_);

implementation

uses
  SysUtils;

{ TClassInfoList }

{$I ..\Templates\InfraTempl_IntfList.inc}

function _ITERABLELIST_.ByClass(pClass: TInfraBaseObjectClass): _ITEM_INTF_;
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

{ TClassInfoIterator }

constructor TClassInfoIterator.Create(const List: IClassInfoList);
begin
  inherited Create;
  FList := List;
  First;
end;

function TClassInfoIterator.CurrentItem: IClassInfo;
begin
  if Assigned(FList) and (fCurrentIndex <> -1)
    and (fCurrentIndex < FList.Count) then
    Result := FList[fCurrentIndex]
  else
    Result := nil;
end;

procedure TClassInfoIterator.First;
begin
  if FList.Count > 0 then
    fCurrentIndex := 0
  else
    fCurrentIndex := -1;
end;

function TClassInfoIterator.IsDone: Boolean;
begin
  Result := (fCurrentIndex < 0) or (fCurrentIndex >= FList.Count);
end;

procedure TClassInfoIterator.Next;
begin
  if (FList.Count > 0) and (FCurrentIndex < FList.Count) then
    Inc(fCurrentIndex);
end;

end.
