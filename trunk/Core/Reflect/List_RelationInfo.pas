unit List_RelationInfo;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBasicList,
  InfraCommonIntf,
  InfraCommon;

type
  _ITERABLELIST_BASE_ = TBaseElement;
  _ITERABLELIST_INTF_ = IRelationInfoList;
  _ITEM_INTF_ = IRelationInfo;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  end;

  TRelationInfoList = class(_ITERABLELIST_);

  TRelationInfoIterator = class(TInterfacedObject, IRelationInfoIterator)
  private
    FCurrentIndex: integer;
    FClassInfo: IClassInfo;
    FPropertyInfo: IPropertyInfo;
    FList: IRelationInfoList;
    function ClassInfoOnThisRelation: boolean;
    function PropertyInfoOnThisRelation: boolean;
  protected
    function CurrentItem: IRelationInfo;
    procedure First; virtual;
    function IsDone: Boolean; virtual;
    procedure Next; virtual;
  public
    constructor Create(const List: IRelationInfoList;
      const pClassInfo: IClassInfo); overload;
    constructor Create(const List: IRelationInfoList;
      const pPropertyInfo: IPropertyInfo); overload;
  end;

implementation

uses
  SysUtils;

{ TRelationInfoList }

{$I ..\Templates\InfraTempl_IntfList.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

{ TRelationInfoIterator }

constructor TRelationInfoIterator.Create(const List: IRelationInfoList;
  const pClassInfo: IClassInfo);
begin
  FClassInfo := pClassInfo;
  FPropertyInfo := nil;
  FList := List;
  First;
end;

constructor TRelationInfoIterator.Create(const List: IRelationInfoList;
  const pPropertyInfo: IPropertyInfo);
begin
  FClassInfo := nil;
  FPropertyInfo := pPropertyInfo;
  FList := List;
  First;
end;

function TRelationInfoIterator.CurrentItem: IRelationInfo;
begin
  if Assigned(FList) and not IsDone then
    Result := FList[fCurrentIndex]
  else
    Result := nil;
end;

procedure TRelationInfoIterator.First;
begin
  fCurrentIndex := -1;
  Next;
end;

function TRelationInfoIterator.IsDone: Boolean;
begin
  Result := (fCurrentIndex = FList.Count);
end;

procedure TRelationInfoIterator.Next;
begin
  Inc(fCurrentIndex);
  if not IsDone then
  begin
    while (fCurrentIndex < FList.Count) do
    begin
      if ClassInfoOnThisRelation or PropertyInfoOnThisRelation then
        Break;
      Inc(fCurrentIndex);
    end;
  end;
end;

function TRelationInfoIterator.ClassInfoOnThisRelation: boolean;
begin
  Result := Assigned(FClassInfo) and
    ((FList[fCurrentIndex].Source.PropertyInfo.DeclaringType = FClassInfo)
    or
    (FList[fCurrentIndex].Destination.PropertyInfo.DeclaringType = FClassInfo));
end;

function TRelationInfoIterator.PropertyInfoOnThisRelation: boolean;
begin
  Result := Assigned(FPropertyInfo) and
    ((FList[fCurrentIndex].Source.PropertyInfo = FPropertyInfo)
    or
    (FList[fCurrentIndex].Destination.PropertyInfo = FPropertyInfo));
end;

end.
