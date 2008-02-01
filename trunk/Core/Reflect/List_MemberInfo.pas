unit List_MemberInfo;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBasicList,
  InfraCommonIntf,
  InfraCommon;

type
  _ITERABLELIST_BASE_ = TBaseElement;
  _ITERABLELIST_INTF_ = IMemberInfoList;
  _ITEM_INTF_ = IMemberInfo;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
    function NewMemberInfoIterator(MemberTypes: TMemberTypes): IMemberInfoIterator;
    function NewMethodInfoIterator(MethodType: TMethodType): IMethodInfoIterator;
    function NewPropertyInfoIterator: IPropertyInfoIterator;
  end;

  TMemberInfoIterator = class(TInterfacedObject, IMemberInfoIterator)
  private
    FCurrentIndex: integer;
    FList: _ITERABLELIST_INTF_;
    FMemberTypes: TMemberTypes;
  protected
    function CurrentItem: _ITEM_INTF_;
    procedure First; virtual;
    function IsDone: Boolean; virtual;
    procedure Next; virtual;
  public
    constructor Create(const List: _ITERABLELIST_INTF_;
      MemberTypes: TMemberTypes);
  end;

  TMethodInfoIterator = class(TInterfacedObject, IMethodInfoIterator)
  private
    FCurrentIndex: integer;
    FMethodType: TMethodType;
    FList: _ITERABLELIST_INTF_;
  protected
    function CurrentItem: IMethodInfo;
    procedure First; virtual;
    function IsDone: Boolean; virtual;
    procedure Next; virtual;
  public
    constructor Create(const List: _ITERABLELIST_INTF_;
      MethodType: TMethodType);
  end;

  TPropertyInfoIterator = class(TInterfacedObject, IPropertyInfoIterator)
  private
    FCurrentIndex: integer;
    FList: _ITERABLELIST_INTF_;
  protected
    function CurrentItem: IPropertyInfo;
    procedure First; virtual;
    function IsDone: Boolean; virtual;
    procedure Next; virtual;
  public
    constructor Create(const List: _ITERABLELIST_INTF_);
  end;

  TMemberInfoList = class(_ITERABLELIST_);

implementation

uses
  SysUtils;

{ TMemberInfoList }

{$I ..\Templates\InfraTempl_IntfList.inc}

function _ITERABLELIST_.NewMemberInfoIterator(
  MemberTypes: TMemberTypes): IMemberInfoIterator;
begin
  Result := TMemberInfoIterator.Create(Self, MemberTypes);
end;

function _ITERABLELIST_.NewMethodInfoIterator(MethodType: TMethodType): IMethodInfoIterator;
begin
  Result := TMethodInfoIterator.Create(Self, MethodType);
end;

function _ITERABLELIST_.NewPropertyInfoIterator: IPropertyInfoIterator;
begin
  Result := TPropertyInfoIterator.Create(Self);
end;

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

{ TMemberInfoIterator }

constructor TMemberInfoIterator.Create(const List: _ITERABLELIST_INTF_;
  MemberTypes: TMemberTypes);
begin
  inherited Create;
  FMemberTypes := MemberTypes;
  FList := List;
  First;
end;

function TMemberInfoIterator.CurrentItem: _ITEM_INTF_;
begin
  if Assigned(FList) and not IsDone then
    Result := FList[fCurrentIndex]
  else
    Result := nil;
end;

procedure TMemberInfoIterator.First;
begin
  fCurrentIndex := -1;
  Next;
end;

function TMemberInfoIterator.IsDone: Boolean;
begin
  Result := (fCurrentIndex = FList.Count);
end;

procedure TMemberInfoIterator.Next;
begin
  Inc(fCurrentIndex);
  if not IsDone then
  begin
    while (fCurrentIndex < FList.Count) do
    begin
      if (FList[fCurrentIndex] as IMemberInfo).MemberType in FMemberTypes then
        Break;
      Inc(fCurrentIndex);
    end;
  end;
end;

{ TMethodInfoIterator }

constructor TMethodInfoIterator.Create(const List: _ITERABLELIST_INTF_;
  MethodType: TMethodType);
begin
  inherited Create;
  FMethodType := MethodType;
  FList := List;
  First;
end;

function TMethodInfoIterator.CurrentItem: IMethodInfo;
begin
  if Assigned(FList) and not IsDone then
    Result := FList[fCurrentIndex] as IMethodInfo
  else
    Result := nil;
end;

procedure TMethodInfoIterator.First;
begin
  fCurrentIndex := -1;
  Next;
end;

function TMethodInfoIterator.IsDone: Boolean;
begin
  Result := (fCurrentIndex = FList.Count);
end;

procedure TMethodInfoIterator.Next;
var
  MemberInfo: IMemberInfo;
begin
  Inc(fCurrentIndex);
  if not IsDone then
  begin
    while (fCurrentIndex < FList.Count) do
    begin
      MemberInfo := FList[fCurrentIndex];
      if (MemberInfo.MemberType = mtConstructor) and
        (FMethodType in [mtJustConstructors, mtBoth]) then
        Break
      else if (MemberInfo.MemberType = mtMethod) and
        (FMethodType in [mtJustMethods, mtBoth]) then
        Break;
      Inc(fCurrentIndex);
    end;
  end;
end;

{ TPropertyInfoIterator }

constructor TPropertyInfoIterator.Create(const List: _ITERABLELIST_INTF_);
begin
  inherited Create;
  FList := List;
  First;
end;

function TPropertyInfoIterator.CurrentItem: IPropertyInfo;
begin
  if Assigned(FList) and not IsDone then
    Result := FList[fCurrentIndex] as IPropertyInfo
  else
    Result := nil;
end;

procedure TPropertyInfoIterator.First;
begin
  fCurrentIndex := -1;
  Next;
end;

function TPropertyInfoIterator.IsDone: Boolean;
begin
  Result := (fCurrentIndex = FList.Count);
end;

procedure TPropertyInfoIterator.Next;
begin
  Inc(fCurrentIndex);
  if not IsDone then
  begin
    while (fCurrentIndex < FList.Count) do
    begin
      if (FList[fCurrentIndex] as IMemberInfo).MemberType = mtProperty then
        Break;
      Inc(fCurrentIndex);
    end;
  end;
end;

end.
