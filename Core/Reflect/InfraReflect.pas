unit InfraReflect;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Classes,
  SysUtils,
  Types,
  InfraConsts,
  InfraBase,
  InfraCommonIntf,
  InfraCommon;

type
  TParams = array[0..50] of dword;
  PParams = ^TParams;

  EInfraTypeRegisteredAlready = class(Exception);

  TMultiplicityInfo = class(TElement, IMultiplicityInfo)
  private
    FLower: TRelationLimit;
    FUpper: TRelationLimit;
  protected
    function GetLower: TRelationLimit;
    function GetUpper: TRelationLimit;
    procedure SetLower(Value: TRelationLimit);
    procedure SetUpper(Value: TRelationLimit);
    property Lower: TRelationLimit read GetLower write SetLower;
    property Upper: TRelationLimit read GetUpper write SetUpper;
  end;

  TRelationEndInfo = class(TElement, IRelationEndInfo)
  private
    FPropertyInfo: IPropertyInfo;
    FMultiplicity: IMultiplicityInfo;
    FOwner: IRelationInfo;
    function GetOwner: IRelationInfo;
    procedure SetOwner(const Value: IRelationInfo);
  protected
    function GetPropertyInfo: IPropertyInfo;
    function GetMultiplicity: IMultiplicityInfo;
    function GetOtherEnd: IRelationEndInfo;
    procedure SetPropertyInfo(const Value: IPropertyInfo);
    property PropertyInfo: IPropertyInfo read GetPropertyInfo
      write SetPropertyInfo;
    property Multiplicity: IMultiplicityInfo read GetMultiplicity;
    property OtherEnd: IRelationEndInfo read GetOtherEnd;
    property Owner: IRelationInfo read GetOwner write SetOwner;
  public
    constructor Create(const Relation: IRelationInfo); reintroduce;
  end;

  TRelationInfo = class(TElement, IRelationInfo)
  private
    FDestination: IRelationEndInfo;
    FContainerInfo: IClassInfo;
    FSource: IRelationEndInfo;
    function IsManyRelation(const Destination: IRelationEndInfo): boolean;
    function IsRequiredRelation(const Source: IRelationEndInfo): boolean;
  protected
    function GetDestination: IRelationEndInfo;
    function GetContainerInfo: IClassInfo;
    function GetSource: IRelationEndInfo;
    function GetRelationKind(const Source: IRelationEndInfo): TRelationKind;
    function OtherEnd(const RelationEnd: IRelationEndInfo): IRelationEndInfo;
    procedure SetContainerInfo(const Value: IClassInfo);
    property Destination: IRelationEndInfo read GetDestination;
    property ContainerInfo: IClassInfo read GetContainerInfo
      write SetContainerInfo;
    property Source: IRelationEndInfo read GetSource;
  public
    procedure InfraInitInstance; override;
  end;

  TTypeService = class(TInterfacedObject, ITypeService)
  private
    FRelations: IRelationInfoList;
    FTypes: IClassInfoList;
    function CreateInstance(ClassID: TGUID): IElement; overload;
    function CreateInstance(const ClassInfo: IClassInfo): IElement; overload;
    function AddType(const pTypeID: TGUID; const pTypeName: string;
      pClassImplementing: TInfraBaseObjectClass; const pFamilyID: TGUID;
      const pSuperClassInfo: IClassInfo = nil): IClassInfo;
    function GetRelations: IRelationInfoList;
    function NewRelationsIterator(
      const pTypeInfo: IClassInfo): IRelationInfoIterator; overload;
    function NewRelationsIterator(
      const pPropertyInfo: IPropertyInfo): IRelationInfoIterator; overload;
    function GetTypes: IClassInfoList;
    function GetType(TypeID: TGUID;
      ThrowException: Boolean = False): IClassInfo; overload;
    function GetType(pClass: TInfraBaseObjectClass;
      ThrowException: Boolean = False): IClassInfo; overload;
    function GetType(const TypeName: String;
      ThrowException: Boolean = False): IClassInfo; overload;
    procedure AddRelation(const pPropertyInfo: IPropertyInfo;
      pSourceLower, pSourceUpper, pTargetLower, pTargetUpper: TRelationLimit;
      const pTarget: IClassInfo;
      const pListInfo: IClassInfo = nil); overload;
  public
    property Relations: IRelationInfoList read GetRelations;
    property Types: IClassInfoList read GetTypes;
    constructor Create;
  end;

  TClassInfo = class(TElement, IClassInfo)
  private
    FClassFamily: TGUID;
    FTypeID: TGUID;
    FName: string;
    FImplClass: TInfraBaseObjectClass;
    FSuperClass: IClassInfo;
    FMembers: IMemberInfoList;
    FOwner: IClassInfo;
    function AddPropertyInfo(const pName: string; const pType: IClassInfo;
      pGetterMethod: Pointer; pSetterMethod: Pointer = nil): IPropertyInfo;
    function AddMethodInfo(const pName: string;
      const pParametersInfo: IParameterInfoList; pMethod: Pointer;
      const pReturnInfo: IClassInfo = nil; pIsConstructor: boolean = False;
      pCallConvention: TCallingConvention = ccRegister): IMethodInfo;
    function AddConstructorInfo(const pName: string;
      const pParametersInfo: IParameterInfoList; pMethod: Pointer;
      pCallConvention: TCallingConvention = ccRegister): IMethodInfo;
    function FindMembers(MemberTypes: TMemberTypes): IMemberInfoIterator;
    function GetClassFamily: TGUID;
    function GetConstructors: IMethodInfoIterator; overload;
    function GetFullName: string;
    function GetTypeID: TGUID;
    function GetImplClass: TInfraBaseObjectClass;
    function GetMemberInfo(const pName: String): IMemberInfo;
    function GetMembers: IMemberInfoIterator;
    function GetMethodInfo(const pName: String): IMethodInfo;
    function GetMethods: IMethodInfoIterator;
    function GetName: string;
    function GetOwner: IClassInfo;
    function GetProperties: IPropertyInfoIterator;
    function GetProperty(const Obj: IElement; const pName: String;
      const pClassInfo: IClassInfo = nil): IInterface;
    function GetPropertyInfo(const pName: String;
      ThrowException: Boolean = False): IPropertyInfo;
    function GetSuperClass: IClassInfo;
    function IsSubClassOf(const Value: IClassInfo): Boolean;
    procedure SetClassFamily(const Family: TGUID);
    procedure SetTypeID(const Value: TGUID);
    procedure SetImplClass(Value: TInfraBaseObjectClass);
    procedure SetName(const Value: string);
    procedure SetOwner(const Value: IClassInfo);
    procedure SetRelation(const pPropertyName: string;
      pSrcMultLower, pSrcMultUpper,
      pTgtMultLower, pTgtMultUpper: TRelationLimit;
      const pTargetInfo: IClassInfo;
      const pListInfo: IClassInfo = nil); overload;
    procedure SetRelation(const pPropertyInfo: IPropertyInfo;
      pSrcMultLower, pSrcMultUpper,
      pTgtMultLower, pTgtMultUpper: TRelationLimit;
      const pTargetInfo: IClassInfo;
      const pListInfo: IClassInfo = nil); overload;
    procedure SetSuperClass(const Value: IClassInfo);
    function GetAllMethods: IMethodInfoIterator;
    function FindPropertyInfo(const pName: string): IPropertyInfo;
  public
    constructor Create; override;
    property ClassFamily: TGUID read GetClassFamily write SetClassFamily;
    property FullName: string read GetFullName;
    property TypeID: TGUID read GetTypeID write SetTypeID;
    property ImplClass: TInfraBaseObjectClass read GetImplClass
      write SetImplClass;
    property Name: string read GetName write SetName;
    property Owner: IClassInfo read GetOwner write SetOwner;
    property SuperClass: IClassInfo read GetSuperClass write SetSuperClass;
  end;

  TMemberInfo = class(TElement, IMemberInfo)
  private
    FDeclaringType: IClassInfo;
    FMemberType: TMemberType;
    FName: string;
  protected
    function GetDeclaringType: IClassInfo;
    function GetMemberType: TMemberType;
    function GetName: string;
    procedure SetDeclaringType(const Value: IClassInfo);
    procedure SetMemberType(Value: TMemberType);
    procedure SetName(const Value: string);
  public
    property DeclaringType: IClassInfo read GetDeclaringType
      write SetDeclaringType;
    property MemberType: TMemberType read GetMemberType write SetMemberType;
    property Name: string read GetName write SetName;
  end;

  TPropertyInfo = class(TMemberInfo, IPropertyInfo)
  private
    FTypeInfo: IClassInfo;
    FGetterInfo: IMethodInfo;
    FSetterInfo: IMethodInfo;
  protected
    function GetGetterInfo: IMethodInfo;
    function GetSetterInfo: IMethodInfo;
    function GetTypeInfo: IClassInfo;
    function GetValue(const pObject: IInterface): IInterface;
    procedure SetValue(const pObject, pValue: IInterface);
  public
    constructor Create(const pName: string;
      const pType, pDeclaringType: IClassInfo;
      const pGetterInfo, pSetterInfo: IMethodInfo); reintroduce;
  end;

  TMethodInfo = class(TMemberInfo, IMethodInfo)
  private
    FMethod: Pointer;
    FParameters: IParameterInfoList;
    FReturnType: IClassInfo;
    FCallingConvention: TCallingConvention;
    FIsConstructor: boolean;
    function InvokeRealMethod(pInstance: TObject;
      pArrayParams: Pointer): IInterface;
    function GetParamPointer(const Index: integer;
      const Param: IInterface): Pointer;
    procedure ParamsToDword(const pParams: IInterfaceList;
      var pStackParams: TParams);
    function GetMethodPointer: Pointer;
  protected
    function GetCallingConvention: TCallingConvention;
    function GetIsConstructor: Boolean;
    function GetParameters: IParameterInfoList;
    function GetReturnType: IClassInfo;
    function Invoke(const pObj: IInfraInstance;
      const pParameters: IInterfaceList): IInterface;
    function AddParam(const pName: string;
      pParameterType: IClassInfo; pOptions: TParameterOptions = [];
      const pDefaultValue: IInterface = nil): IParameterInfo;
  public
    constructor Create(const pName: string;
      const pDeclaringType: IClassInfo;
      pMethod: Pointer;
      const pParameters: IParameterInfoList = nil;
      const pReturnType: IClassInfo = nil;
      pIsConstructor: boolean = False;
      pCallingConvention: TCallingConvention = ccRegister); reintroduce;
    property MethodPointer: Pointer read GetMethodPointer;
    property IsConstructor: Boolean read GetIsConstructor;
    property Parameters: IParameterInfoList read GetParameters;
    property ReturnType: IClassInfo read GetReturnType;
    property CallingConvention: TCallingConvention read GetCallingConvention;
  end;

  TParameterInfo = class(TElement, IParameterInfo)
  private
    FDefaultValue: IInterface;
    FOptions: TParameterOptions;
    FName: string;
    FParameterType: IClassInfo;
    FPosition: integer;
    function GetDefaultValue: IInterface;
    function GetIsOptional: boolean;
    function GetIsConst: boolean;
    function GetIsVar: boolean;
    function GetIsReturn: boolean;
    function GetName: string;
    function GetParameterType: IClassInfo;
    function GetPosition: integer;
  public
    constructor Create(const pName: string;
      pParameterType: IClassInfo; pPosition: Integer;
      pOptions: TParameterOptions;
      const pDefaultValue: IInterface); reintroduce;
    property DefaultValue: IInterface read GetDefaultValue;
    property IsConst: boolean read GetIsConst;
    property IsOptional: boolean read GetIsOptional;
    property IsReturn: boolean read GetIsReturn;
    property IsVar: boolean read GetIsVar;
    property Name: string read GetName;
    property ParameterType: IClassInfo read GetParameterType;
    property Position: integer read GetPosition;
  end;

implementation

uses
  List_ClassInfo, List_RelationInfo, List_ParameterInfo, List_MemberInfo,
  InfraValueTypeIntf;

{ TMultiplicityInfo }

function TMultiplicityInfo.GetLower: TRelationLimit;
begin
  Result := FLower;
end;

function TMultiplicityInfo.GetUpper: TRelationLimit;
begin
  Result := FUpper;
end;

procedure TMultiplicityInfo.SetLower(Value: TRelationLimit);
begin
  if Value <> FLower then
    FLower := Value;
end;

procedure TMultiplicityInfo.SetUpper(Value: TRelationLimit);
begin
  if Value <> FUpper then
    FUpper := Value;
end;

{ TRelationEndInfo }

constructor TRelationEndInfo.Create(const Relation: IRelationInfo);
begin
  inherited Create;
  Owner := Relation;
  FMultiplicity := TMultiplicityInfo.Create;
end;

function TRelationEndInfo.GetPropertyInfo: IPropertyInfo;
begin
  Result := FPropertyInfo;
end;

function TRelationEndInfo.GetMultiplicity: IMultiplicityInfo;
begin
  Result := FMultiplicity;
end;

function TRelationEndInfo.GetOtherEnd: IRelationEndInfo;
begin
  Result := Owner.OtherEnd(Self);
end;

function TRelationEndInfo.GetOwner: IRelationInfo;
begin
  Result := FOwner;
end;

procedure TRelationEndInfo.SetPropertyInfo(const Value: IPropertyInfo);
begin
  if Value <> FPropertyInfo then
    SetReference(IInterface(FPropertyInfo), Value);
end;

procedure TRelationEndInfo.SetOwner(const Value: IRelationInfo);
begin
  SetReference(IInterface(FOwner), Value);
end;

{ TRelationInfo }

function TRelationInfo.GetContainerInfo: IClassInfo;
begin
  Result := FContainerInfo;
end;

function TRelationInfo.GetDestination: IRelationEndInfo;
begin
  Result := FDestination;
end;

function TRelationInfo.GetRelationKind(
  const Source: IRelationEndInfo): TRelationKind;
begin
  if IsManyRelation(Source.OtherEnd) then
  begin
    if IsRequiredRelation(Source) then
      Result := rkComposition
    else
      Result := rkAgregation
  end else
    Result := rkAssociation;
end;

function TRelationInfo.GetSource: IRelationEndInfo;
begin
  Result := FSource;
end;

procedure TRelationInfo.InfraInitInstance;
begin
  inherited;
  FSource := TRelationEndInfo.Create(Self);
  FDestination := TRelationEndInfo.Create(Self);
  with FSource.Multiplicity do
  begin
    Lower := 0;
    Upper := 1;
  end;
  with FDestination.Multiplicity do
  begin
    Lower := 0;
    Upper := 1;
  end;
end;

function TRelationInfo.IsManyRelation(
  const Destination: IRelationEndInfo): boolean;
begin
  Result := (Destination.Multiplicity.Upper = N) or
    (Destination.Multiplicity.Upper > 0);
end;

function TRelationInfo.IsRequiredRelation(
  const Source: IRelationEndInfo): boolean;
begin
  Result := (Source.Multiplicity.Lower >= 1);
end;

function TRelationInfo.OtherEnd(
  const RelationEnd: IRelationEndInfo): IRelationEndInfo;
begin
  if RelationEnd = FSource then
    Result := FDestination
  else
    Result := FSource;
end;

procedure TRelationInfo.SetContainerInfo(const Value: IClassInfo);
begin
  if Value <> FContainerInfo then
    FContainerInfo := Value;
end;

{ TTypeService }

procedure TTypeService.AddRelation(const pPropertyInfo: IPropertyInfo;
  pSourceLower, pSourceUpper, pTargetLower, pTargetUpper: TRelationLimit;
  const pTarget, pListInfo: IClassInfo);
var
  NewRelation: IRelationInfo;
begin
  NewRelation := TRelationInfo.Create;
  with NewRelation do
  begin
    ContainerInfo := pListInfo;
    with Source do
    begin
      PropertyInfo := pPropertyInfo;
      Multiplicity.Lower := pSourceLower;
      Multiplicity.Upper := pSourceUpper;
    end;
    with Destination do
    begin
      PropertyInfo := nil;
      Multiplicity.Lower := pTargetLower;
      Multiplicity.Upper := pTargetUpper;
    end;
  end;
  FRelations.Add(NewRelation);
end;

function TTypeService.AddType(const pTypeID: TGUID; const pTypeName: string;
  pClassImplementing: TInfraBaseObjectClass; const pFamilyID: TGUID;
  const pSuperClassInfo: IClassInfo = nil): IClassInfo;
begin
  if Assigned(GetType(pTypeID)) then
    raise EInfraTypeRegisteredAlready.Create('Type registred already');
  Result := TClassInfo.Create;
  with Result do
  begin
    Name := pTypeName;
    ImplClass := pClassImplementing;
    TypeID := pTypeID;
    FamilyID := pFamilyID;
    SuperClass := pSuperClassInfo;
  end;
  GetTypes.Add(Result);
end;

function TTypeService.CreateInstance(ClassID: TGUID): IElement;
var
  Info: IClassInfo;
begin
  Info := GetType(ClassID);
  Result := CreateInstance(Info);
end;

constructor TTypeService.Create;
begin
  FTypes := TClassInfoList.Create;
  FRelations := TRelationInfoList.Create;
end;

function TTypeService.CreateInstance(
  const ClassInfo: IClassInfo): IElement;
begin
  if not Assigned(ClassInfo)
    or not Supports(ClassInfo.ImplClass.Create, IElement, Result) then
    raise Exception.Create('Cannot Instanciate. ClassInfo not registred!');
end;

function TTypeService.GetRelations: IRelationInfoList;
begin
  Result := FRelations;
end;

function TTypeService.GetType(TypeID: TGUID;
  ThrowException: Boolean): IClassInfo;
begin
  Result := FTypes.ByGUID(TypeID);
  if ThrowException and not Assigned(Result) then
    raise Exception.Create('ClassInfo not registred to ID: ' +
      GuidToString(TypeID));
end;

function TTypeService.GetType(const TypeName: String;
  ThrowException: Boolean): IClassInfo;
begin
  Result := FTypes.ByName(TypeName);
  if ThrowException and not Assigned(Result) then
    raise Exception.Create('ClassInfo not registred to '+ TypeName);
end;

function TTypeService.GetType(pClass: TInfraBaseObjectClass;
  ThrowException: Boolean): IClassInfo;
begin
  Result := FTypes.ByClass(pClass);
  if ThrowException and not Assigned(Result) then
    raise Exception.Create('ClassInfo not registred to class: '+
      pClass.ClassName);
end;

function TTypeService.GetTypes: IClassInfoList;
begin
  if not Assigned(FTypes) then
    FTypes := TClassInfoList.Create;
  Result := FTypes;
end;

function TTypeService.NewRelationsIterator(
  const pTypeInfo: IClassInfo): IRelationInfoIterator;
begin
  Result := TRelationInfoIterator.Create(Relations, pTypeInfo);
end;

function TTypeService.NewRelationsIterator(
  const pPropertyInfo: IPropertyInfo): IRelationInfoIterator;
begin
  Result := TRelationInfoIterator.Create(Relations, pPropertyInfo);
end;

{ TClassInfo }

constructor TClassInfo.create;
begin
  inherited Create;
  FMembers := TMemberInfoList.Create;
end;

function TClassInfo.AddConstructorInfo(const pName: string;
  const pParametersInfo: IParameterInfoList; pMethod: Pointer;
  pCallConvention: TCallingConvention = ccRegister): IMethodInfo;
begin
  Result := AddMethodInfo(pName, pParametersInfo, pMethod, Self.TypeInfo,
    True, pCallConvention);
end;

function TClassInfo.AddMethodInfo(const pName: string;
  const pParametersInfo: IParameterInfoList; pMethod: Pointer;
  const pReturnInfo: IClassInfo = nil; pIsConstructor: boolean = False;
  pCallConvention: TCallingConvention = ccRegister): IMethodInfo;
begin
  Result := TMethodInfo.Create(pName, Self, pMethod,
    pParametersInfo, pReturnInfo, pIsConstructor, pCallConvention);
  FMembers.Add(Result as IMemberInfo);
end;

function TClassInfo.AddPropertyInfo(const pName: string;
  const pType: IClassInfo; pGetterMethod: Pointer;
  pSetterMethod: Pointer = nil): IPropertyInfo;
var
  GetterInfo, SetterInfo: IMethodInfo;
begin
  if Assigned(pGetterMethod) then
  begin
    GetterInfo := TMethodInfo.Create('Get'+pName, Self, pGetterMethod, nil, pType);
    GetterInfo.AddParam('Return', pType, [poReturn]);
    FMembers.Add(GetterInfo as IMemberInfo);
  end;
  if Assigned(pSetterMethod) then
  begin
    SetterInfo := TMethodInfo.Create('Set'+pName, Self, pSetterMethod);
    SetterInfo.AddParam('Value', pType);
    FMembers.Add(SetterInfo as IMemberInfo);
  end;
  Result := TPropertyInfo.Create(pName, pType, Self, GetterInfo, SetterInfo);
  FMembers.Add(Result as IMemberInfo);
end;

function TClassInfo.FindMembers(
  MemberTypes: TMemberTypes): IMemberInfoIterator;
begin
  Result := FMembers.NewMemberInfoIterator(MemberTypes);
end;

function TClassInfo.GetClassFamily: TGUID;
begin
  Result := FClassFamily;
end;

function TClassInfo.GetConstructors: IMethodInfoIterator;
begin
  Result := FMembers.NewMethodInfoIterator(mtJustConstructors);
end;

function TClassInfo.GetFullName: string;
begin
  if Assigned(Owner) then
    Result := Owner.FullName + '.' + Name
  else
    Result := Name;
end;

function TClassInfo.GetImplClass: TInfraBaseObjectClass;
begin
  Result := FImplClass;
end;

function TClassInfo.GetMemberInfo(const pName: String): IMemberInfo;
var
  Iterator: IMemberInfoIterator;
begin
  Result := nil;
  Iterator := FMembers.NewMemberInfoIterator(mtAll);
  while not Iterator.IsDone do
    if SameText(Iterator.CurrentItem.Name, pName) then
    begin
      Result := Iterator.CurrentItem;
      Break
    end else
      Iterator.Next;
end;

function TClassInfo.GetMembers: IMemberInfoIterator;
begin
  Result := FMembers.NewMemberInfoIterator(mtAll);
end;

function TClassInfo.GetMethodInfo(const pName: String): IMethodInfo;
var
  Iterator: IMethodInfoIterator;
begin
  Result := nil;
  Iterator := FMembers.NewMethodInfoIterator(mtBoth);
  while not Iterator.IsDone do
    if SameText(Iterator.CurrentItem.Name, pName) then
    begin
      Result := Iterator.CurrentItem;
      Break
    end else
      Iterator.Next;
end;

function TClassInfo.GetMethods: IMethodInfoIterator;
begin
  Result := FMembers.NewMethodInfoIterator(mtJustMethods);
end;

function TClassInfo.GetAllMethods: IMethodInfoIterator;
begin
  Result := FMembers.NewMethodInfoIterator(mtBoth);
end;

function TClassInfo.GetName: string;
begin
  Result := FName;
end;

function TClassInfo.GetOwner: IClassInfo;
begin
  Result := FOwner;
end;

function TClassInfo.GetProperties: IPropertyInfoIterator;
begin
  Result := FMembers.NewPropertyInfoIterator;
end;

function TClassInfo.FindPropertyInfo(const pName: string): IPropertyInfo;
var
  Iterator: IPropertyInfoIterator;
begin
  Result := nil;
  Iterator := FMembers.NewPropertyInfoIterator;
  while not Iterator.IsDone do
  begin
    if SameText(Iterator.CurrentItem.Name, pName) then
    begin
      Result := Iterator.CurrentItem;
      Break;
    end else
      Iterator.Next;
  end;
end;

function TClassInfo.GetPropertyInfo(const pName: String;
  ThrowException: Boolean = False): IPropertyInfo;
var
  CommaPosition: Integer;
  PropertyToSearch: string;
begin
  Result := nil;
  CommaPosition := Pos('.', pName);
  if CommaPosition = 0 then
    PropertyToSearch := pName
  else
    PropertyToSearch := Copy(pName, 0, CommaPosition-1);
  Result := FindPropertyInfo(PropertyToSearch);
  if not Assigned(Result) and Assigned(SuperClass) then
    Result := SuperClass.GetPropertyInfo(PropertyToSearch, ThrowException);
  if Assigned(Result) and (Result.Name <> pName) then
    Result := Result.TypeInfo.GetPropertyInfo(
      Copy(pName, CommaPosition+1, Length(pName)), ThrowException);
  if ThrowException and not Assigned(Result) then
    raise Exception.Create(Format(
      'PropertyInfo to "%s.%s" not found', [Self.FullName, pName]));
end;

function TClassInfo.GetProperty(const Obj: IElement; const pName: String;
  const pClassInfo: IClassInfo = nil): IInterface;
var
  CommaPosition: Integer;
  PropertyToSearch: string;
  PropertyInfo: IPropertyInfo;
begin
  Assert(Assigned(Obj.TypeInfo),
    'Property.TypeInfo to '+QuotedStr(pName)+' not registred');
  Result := nil;
  CommaPosition := Pos('.', pName);
  if CommaPosition = 0 then
    PropertyToSearch := pName
  else
    PropertyToSearch := Copy(pName, 0, CommaPosition-1);

  if Assigned(pClassInfo) then
    PropertyInfo := pClassInfo.GetPropertyInfo(PropertyToSearch, True)
  else
    PropertyInfo := GetPropertyInfo(PropertyToSearch, True);

  Result := PropertyInfo.GetValue(Obj);
  if (PropertyInfo.Name <> pName) and Assigned(Result) then
    Result := GetProperty(Result as IElement,
      Copy(pName, CommaPosition+1, Length(pName)), PropertyInfo.TypeInfo);
end;

function TClassInfo.GetSuperClass: IClassInfo;
begin
  Result := FSuperClass;
end;

function TClassInfo.GetTypeID: TGUID;
begin
  Result := FTypeID;
end;

function TClassInfo.IsSubClassOf(const Value: IClassInfo): Boolean;
begin
  Result := Assigned(FSuperClass)
    and ( (FSuperClass = Value) or FSuperClass.IsSubClassOf(Value) );
end;

procedure TClassInfo.SetClassFamily(const Family: TGUID);
begin
  if not IsEqualGUID(Family, FClassFamily) then
    FClassFamily := Family;
end;

procedure TClassInfo.SetImplClass(Value: TInfraBaseObjectClass);
begin
  FImplClass := Value;
end;

procedure TClassInfo.SetName(const Value: string);
begin
  if Value <> FName then
    FName := Value;
end;

procedure TClassInfo.SetOwner(const Value: IClassInfo);
begin
  FOwner := Value;
end;

procedure TClassInfo.SetRelation(const pPropertyInfo: IPropertyInfo;
  pSrcMultLower, pSrcMultUpper, pTgtMultLower,
  pTgtMultUpper: TRelationLimit; const pTargetInfo,
  pListInfo: IClassInfo);
begin
  TypeService.AddRelation(pPropertyInfo,
    pSrcMultLower, pSrcMultUpper, pTgtMultLower, pTgtMultUpper,
    pTargetInfo, pListInfo);
end;

procedure TClassInfo.SetRelation(const pPropertyName: string;
  pSrcMultLower, pSrcMultUpper, pTgtMultLower,
  pTgtMultUpper: TRelationLimit; const pTargetInfo,
  pListInfo: IClassInfo);
var
  pPropertyInfo: IPropertyInfo;
begin
  pPropertyInfo := GetPropertyInfo(pPropertyName);
  SetRelation(pPropertyInfo,
    pSrcMultLower, pSrcMultUpper, pTgtMultLower, pTgtMultUpper,
    pTargetInfo, pListInfo);
end;

procedure TClassInfo.SetSuperClass(const Value: IClassInfo);
begin
  if Value <> FSuperClass then
    FSuperClass := Value;
end;

procedure TClassInfo.SetTypeID(const Value: TGUID);
begin
  if not IsEqualGUID(Value, FTypeID) then
    FTypeID := Value;
end;

{ TMemberInfo }

function TMemberInfo.GetDeclaringType: IClassInfo;
begin
  Result := FDeclaringType;
end;

procedure TMemberInfo.SetDeclaringType(const Value: IClassInfo);
begin
  FDeclaringType := Value;
end;

function TMemberInfo.GetMemberType: TMemberType;
begin
  Result := FMemberType;
end;

function TMemberInfo.GetName: string;
begin
  Result := FName;
end;

procedure TMemberInfo.SetMemberType(Value: TMemberType);
begin
  FMemberType := Value;
end;

procedure TMemberInfo.SetName(const Value: string);
begin
  FName := Value;
end;

{ TPropertyInfo }

constructor TPropertyInfo.Create(const pName: string;
  const pType, pDeclaringType: IClassInfo;
  const pGetterInfo, pSetterInfo: IMethodInfo);
begin
  inherited Create;
  FTypeInfo := pType;
  MemberType := mtProperty;
  Name := pName;
  SetReference(IInterface(FDeclaringType), pDeclaringType);
  SetReference(IInterface(FGetterInfo), pGetterInfo);
  SetReference(IInterface(FSetterInfo), pSetterInfo);
end;

function TPropertyInfo.GetGetterInfo: IMethodInfo;
begin
  Result := FGetterInfo;
end;

function TPropertyInfo.GetSetterInfo: IMethodInfo;
begin
  Result := FSetterInfo;
end;

function TPropertyInfo.GetTypeInfo: IClassInfo;
begin
  Result := FTypeInfo;
end;

function TPropertyInfo.GetValue(const pObject: IInterface): IInterface;
var
  Obj: IInfraInstance;
begin
  if Supports(pObject, IInfraInstance, Obj)
    and Assigned(FGetterInfo) then
    Result := FGetterInfo.Invoke(Obj, nil)
  else
    Result := nil;
end;

// *** Ver com solerman
procedure TPropertyInfo.SetValue(const pObject, pValue: IInterface);
var
  pParameters: IInterfaceList;
  Obj: IInfraInstance;
begin
  if Supports(pObject, IInfraInstance, Obj)
    and Assigned(FSetterInfo) then
  begin
    pParameters := TInterfaceList.Create;
    pParameters.Add(pValue);
    FSetterInfo.Invoke(Obj, pParameters);
  end;
end;

{ TMethodInfo }

function TMethodInfo.AddParam(const pName: string;
  pParameterType: IClassInfo; pOptions: TParameterOptions;
  const pDefaultValue: IInterface): IParameterInfo;
begin
  Result := TParameterInfo.Create(pName,
    pParameterType, Parameters.Count, pOptions, pDefaultValue);
  Parameters.Add(Result);
end;

constructor TMethodInfo.Create(const pName: string;
  const pDeclaringType: IClassInfo;
  pMethod: Pointer;
  const pParameters: IParameterInfoList = nil;
  const pReturnType: IClassInfo = nil;
  pIsConstructor: boolean = False;
  pCallingConvention: TCallingConvention = ccRegister);
begin
  inherited Create;
  FIsConstructor := pIsConstructor;
  if FIsConstructor then
    MemberType := mtConstructor
  else
    MemberType := mtMethod;
  Name := pName;
  SetReference(IInterface(FDeclaringType), pDeclaringType);
  FMethod := pMethod;
  FCallingConvention := pCallingConvention;
  FReturnType := pReturnType;
end;

function TMethodInfo.GetCallingConvention: TCallingConvention;
begin
  Result := FCallingConvention;
end;

function TMethodInfo.GetIsConstructor: Boolean;
begin
  Result := FIsConstructor;
end;

function TMethodInfo.GetMethodPointer: Pointer;
begin
  Result := FMethod;
end;

function TMethodInfo.GetParameters: IParameterInfoList;
begin
  if not Assigned(FParameters) then
    FParameters := TParameterInfoList.Create;
  Result := FParameters;
end;

function TMethodInfo.GetReturnType: IClassInfo;
begin
  Result := FReturnType;
end;

function TMethodInfo.Invoke(const pObj: IInfraInstance;
  const pParameters: IInterfaceList): IInterface;
var
  Obj: TObject;
  vStackParams: TParams;
begin
  Obj := pObj.GetInstance;
  if Assigned(Obj) then
  begin
    FillChar(vStackParams, SizeOf(TParams), 0);
    ParamsToDword(pParameters, vStackParams);
    Result := InvokeRealMethod(Obj, @vStackParams);
  end else
    Result := nil;
end;

procedure TMethodInfo.ParamsToDword(const pParams: IInterfaceList;
  var pStackParams: TParams);
var
  i, QtdParams: integer;
begin
  if Assigned(ReturnType) then
    QtdParams := Parameters.Count-1
  else
    QtdParams := Parameters.Count;
  if QtdParams <> 0 then
    for i := 0 to Pred(QtdParams) do
      pStackParams[i] := dword(GetParamPointer(i, IInterface(pParams[i])));
end;

function TMethodInfo.GetParamPointer(const Index: integer;
  const Param: IInterface): Pointer;
var
  pType: IInfraType;
begin
  if Parameters.Count <= Index then
    raise exception.Create('Parameter index out of bounds');
  if Supports(Param, Parameters[Index].ParameterType.TypeID, pType) then
    Result := Pointer(pType)
  else
    raise exception.Create('Cannot unpack param. Incompatible types');
end;

// Invoke the real method passing to it the parameters.
function TMethodInfo.InvokeRealMethod(pInstance: TObject;
  pArrayParams: Pointer): IInterface;
var
  pParamCount: integer;
  pMethod: Pointer;
begin
  pParamCount := Parameters.Count;
  pMethod := FMethod;
  // Is need extra param when returning interface, string or record type.
  // At moment, we just will return always a interface, then....
  if Assigned(ReturnType) then
    TParams(pArrayParams^)[pParamCount-1] := Dword(@Result);
  asm
    push esp                        // store ESP
    push ebx                        // store EBX
    mov ebx, pParamCount            // EBX := pParamCount
    cmp ebx, 0                      // if pParamCount = 0 then
    je @CallMethod                  //   CallMethod
    cmp ebx, 1                      // else if pParamCount <> 1 then
    jne @Two_Params                 //   Two_Params
                                    // else
    mov ebx, pArrayParams           //   EBX := pArrayParams
    mov edx, [ebx]                  //   put the 1st param into EDX
    jmp @CallMethod                 //   CallMethod
  @Two_Params:
    cmp ebx, 2                      // if pParamCount <> 2 then
    jne @GreaterThanTwoParams       //   GreaterThanTwoParams
                                    // else
    mov ebx, pArrayParams           //   EBX := pArrayParams
    mov edx, [ebx]                  //   put the 1st param into EDX
    mov ecx, [ebx+4]                //   put the 2nd param into ECX
    jmp @CallMethod                 //   CallMethod
  @GreaterThanTwoParams:
    mov ebx, pArrayParams           // EBX := pArrayParams
    add ebx, 4                      // position at 2nd parameter
    mov ecx, pParamCount            // put pParamCount into ECX
    sub ecx, 2                      // pParamCount := pParamCount - 2
  @StackUP:
    add ebx, 4                      // position at next parameter
    mov eax, [ebx]                  // move the value of parameter to EAX
    push eax                        // stack up EAX
    loop @StackUP                   // loop (Decrement ECX to 0)
    mov ebx, pArrayParams           // put pArrayParams into EBX
    mov edx, [ebx]                  // put the 1st array item into EDX (1nd par)
    mov ecx, [ebx+4]                // put the 2nd array item into ECX (2nd par)
  @CallMethod:
    mov eax, pInstance              // put pInstance into EAX
    call pMethod                    // Call real method
    pop ebx                         // remove stack item ebx
    pop esp                         // remove stack item esp
  end;
end;

{ TParameterInfo }

constructor TParameterInfo.Create(const pName: string;
  pParameterType: IClassInfo; pPosition: Integer;
  pOptions: TParameterOptions;
  const pDefaultValue: IInterface);
begin
  inherited Create;
  FName := pName;
  FParameterType := pParameterType;
  FPosition := pPosition;
  FOptions := pOptions;
  FDefaultValue := pDefaultValue;
end;

function TParameterInfo.GetDefaultValue: IInterface;
begin
  Result := FDefaultValue;
end;

function TParameterInfo.GetIsConst: boolean;
begin
  Result := poConst in FOptions;
end;

function TParameterInfo.GetIsOptional: boolean;
begin
  Result := Assigned(FDefaultValue);
end;

function TParameterInfo.GetIsReturn: boolean;
begin
  Result := poReturn in FOptions;
end;

function TParameterInfo.GetIsVar: boolean;
begin
  Result := poVar in FOptions;
end;

function TParameterInfo.GetName: string;
begin
  Result := FName;
end;

function TParameterInfo.GetParameterType: IClassInfo;
begin
  Result := FParameterType;
end;

function TParameterInfo.GetPosition: integer;
begin
  Result := FPosition;
end;

end.
