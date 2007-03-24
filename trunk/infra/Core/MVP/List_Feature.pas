unit List_Feature;

{$I Infra.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraCommon,
  InfraCommonIntf,
  InfraBasicList,
  InfraValueTypeIntf;

type
  _ITERABLELIST_BASE_ = TMemoryManagedObject;
  _ITERABLELIST_INTF_ = IInfraFeatureList;
  _ITEM_INTF_ = IInfraFeature;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I Templates\InfraTempl_IntfList.inc}
  private
    FOwner: IElement;
  protected
    function GetFeatureByName(const Name: string): _ITEM_INTF_;
    function GetOwner: IElement;
    procedure CreateFeatures(const TypeInfo: IClassInfo); virtual;
    procedure SetOwner(const Value: IElement);
    property FeatureByName[const Name: string]: _ITEM_INTF_
      read GetFeatureByName;
    property Owner: IElement read GetOwner write SetOwner;
  public
    constructor Create(const AOwner: IElement); overload;
  end;

  TInfraFeatureList = class(_ITERABLELIST_)
  protected
    function Add(const Item: _ITEM_INTF_): Integer; override;
    function Remove(const Item: _ITEM_INTF_): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const Item: _ITEM_INTF_); override;
  end;

implementation

uses
  SysUtils;

{ TInfraFeatureList }

{$I Templates\InfraTempl_IntfList.inc}

function _ITERABLELIST_.GetFeatureByName(const Name: string): _ITEM_INTF_;
var
  i: Integer;
  Feature: _ITEM_INTF_;
begin
  Result := nil;
  for i := 0 to FItems.Count-1 do
  begin
    Feature := (FItems.Items[i] as _ITEM_INTF_);
    if Feature.Name = Name then
    begin
      Result := Feature;
      Break;
    end;
  end;
  if not Assigned(Result) then
    Raise Exception.Create('Feature ou Command not found!');
end;

function _ITERABLELIST_.GetOwner: IElement;
begin
  Result := FOwner;
end;

procedure _ITERABLELIST_.CreateFeatures(const TypeInfo: IClassInfo);
var
  i: integer;
  TheFeature: _ITEM_INTF_;
begin
  if Assigned(TypeInfo) then
    CreateFeatures(TypeInfo.SuperClass)
  else
    Exit;
  for i := 0 to TypeInfo.MemberInfos.Count-1 do
  begin
    Assert(Supports(TypeInfo.MemberInfos[i].ImplClass.Create, _ITEM_INTF_,
      TheFeature));
    TheFeature.Owner := Self.Owner;
  end;
end;

procedure _ITERABLELIST_.SetOwner(const Value: IElement);
begin
  SetReference(IInterface(FOwner), Value);
end;

constructor _ITERABLELIST_.Create(const AOwner: IElement);
begin
  Create;
  Owner := AOwner
end;

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

{ TInfraFeatureList }

function TInfraFeatureList.Add(const Item: _ITEM_INTF_): Integer;
begin
  Result := inherited Add(Item);
  if Result <> -1 then
    Item.Owner := Owner;
end;

function TInfraFeatureList.Remove(const Item: _ITEM_INTF_): Integer;
begin
  Item.Owner := nil;
  Result := inherited Remove(Item);
end;

procedure TInfraFeatureList.Clear;
var
  Iterator: _ITERATOR_INTF_;
begin
  Iterator := NewIterator;
  while not Iterator.IsDone do
  begin
    (Iterator.CurrentItem as _ITEM_INTF_).Owner := nil;
    Iterator.Next;
  end;
  inherited;
end;

procedure TInfraFeatureList.Delete(Index: Integer);
begin
  (Items[Index] as _ITEM_INTF_).Owner := nil;
  inherited Delete(Index);
end;

procedure TInfraFeatureList.Insert(Index: Integer; const Item: _ITEM_INTF_);
begin
  inherited Insert(Index, Item);
  (Items[Index] as _ITEM_INTF_).Owner := Owner;
end;

end.
