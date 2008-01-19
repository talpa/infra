unit InfraCommonIntf;

interface

{$I 'Infra.Inc'}

uses
  Classes,
  InfraBase,
  InfraConsts;

type
  TRetentionPolice = (rpNone, rpClass, rpInstance);

  IMemoryManagedObject = interface;
  IInfraEventService = interface;
  IInfraReferenceService = interface;
  IInfraPublisher = interface;
  IInfraEvent = interface;
  IInfraIterator = interface;
  ITypeService = interface;
  IClassInfo = interface;
  IClassInfoList = interface;
  IRelationInfo = interface;

  // Application Context

  IApplicationContext = interface(IInterface)
    ['{E0C9C9AC-1555-4A14-B576-AEDB09A170AE}']
    function GetElement: IMemoryManagedObject;
    function GetEventService: IInfraEventService;
    function GetReferenceService: IInfraReferenceService;
    function GetTypeService: ITypeService;
    procedure ShutDown;
    property Element: IMemoryManagedObject read GetElement;
    property EventService: IInfraEventService read GetEventService;
    property ReferenceService: IInfraReferenceService read GetReferenceService;
    property TypeService: ITypeService read GetTypeService;
  end;

  // Interface Injection

  IInjectedItem = interface(IInterface)
    ['{BABD476C-9F13-45E0-9246-8694A02E0DDA}']
    function GetID: TGUID;
    function GetInjectedInterface: IInterface;
    function GetIsAnnotation: boolean;
    procedure SetID(const Value: TGUID);
    procedure SetInjectedInterface(const Value: IInterface);
    procedure SetIsAnnotation(Value: boolean);
    property ID: TGUID read GetID write SetID;
    property InjectedInterface: IInterface read GetInjectedInterface
      write SetInjectedInterface;
    property IsAnnotation: boolean read GetIsAnnotation write SetIsAnnotation;
  end;

  IAnnotationsIterator = interface(IInterface)
    ['{927DF9B4-B76D-436C-8108-1C33DE0CC459}']
    function CurrentItem: IInjectedItem;
    procedure First;
    function IsDone: Boolean;
    procedure Next;
  end;

  IInjectedList = interface(IInterface)
    ['{489622C1-09B1-4138-BBCF-E7428604ADEC}']
    function Add(const ID: TGUID; const pObject: IInterface): integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): IInjectedItem;
    function IndexByGUID(const Item: TGUID): Integer;
    function NewAnnotationIterator: IAnnotationsIterator;
    procedure Clear;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: IInjectedItem read GetItem; default;
  end;

  IMemoryManagedObject = interface(IInterface)
    ['{B70BB062-6298-4EF7-9CCB-9BFF732EC0CD}']
    {$IFDEF SEE_REFCOUNT}
    function GetRefCount: integer;
    {$ENDIF}
    function Annotate(const pID: TGUID): IInterface; overload;
    function Annotate(const pClassInfo: IClassInfo): IInterface; overload;
    function isAnnotationPresent(const pID: TGUID): Boolean;
    function GetAnnotation(const pID: TGUID): IInterface;
    function GetAnnotations: IAnnotationsIterator;
    function Inject(const pID: TGUID; const pItem: IInterface;
      pIsAnnotation: boolean = False): IInjectedItem;
    function GetInjectedList: IInjectedList;
    property InjectedList: IInjectedList read GetInjectedList;
  end;

  IElement = interface(IMemoryManagedObject)
    ['{82045917-1413-4752-ADFB-16A8B26D93B8}']
    function GetPublisher: IInfraPublisher;
    function GetTypeInfo: IClassInfo;
    procedure SetTypeInfo(const Value: IClassInfo);
    property Publisher: IInfraPublisher read GetPublisher;
    property TypeInfo: IClassInfo read GetTypeInfo write SetTypeInfo;
  end;

  // Instance

  IInfraInstance = interface(IInterface)
    ['{95987A62-A416-438F-9A58-88F8BB6D5446}']
    function GetInstance: TObject;
  end;

  // Reference

  IInfraReferenceKeeper = interface(IInterface)
    ['{CC81AD6A-B14D-4379-97E4-EA945AB700B1}']
    procedure SetReference(var Ref: IInterface; const Value: IInterface);
  end;

  IInfraReferenceService = interface(IInterface)
    ['{54987987-573B-4379-B15F-031FCE125412}']
    procedure NotifyDestruction(const Sender: IInterface);
    procedure SetReference(const Sender: IInterface; var Ref: IInterface;
      const Value: IInterface);
  end;

  // Notify

  TSubscriberInform = procedure (const Event: IInfraEvent) of object;

  TSubscriberFilter = function (const Event: IInfraEvent): Boolean of object;

  IInfraPublisher = interface(IElement)
    ['{8AAAA64F-D54E-4ADC-83B2-F40C25A35605}']
    function HasSubscribers(const EventType: TGUID): Boolean;
    procedure Publish(const Event: IInfraEvent);
  end;

  IInfraPublisherList = interface(IMemoryManagedObject)
    ['{3744A0C5-DB23-400F-9861-A075BF49DF21}']
    function Add(const Item: IInfraPublisher): Integer;
    function First: IInfraPublisher;
    function GetCount: Integer;
    function GetItem(Index: Integer): IInfraPublisher;
    function Last: IInfraPublisher;
    procedure SetItem(Index: Integer; const Value: IInfraPublisher);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IInfraPublisher read GetItem
      write SetItem; default;
  end;

  ISubscriber = interface(IInterface)
    ['{ABC1C548-8467-470A-8E1E-9C6ACCCDBE81}']
    procedure Inform(const Event: IInfraEvent);
  end;

  IInfraFilter = interface(IInterface)
    ['{82CB0372-0BC8-4204-95B3-ED83E88E82F9}']
    function Apply(const Event: IInfraEvent): Boolean;
  end;

  ISubscription = interface(IMemoryManagedObject)
    ['{00154BC2-A64D-45B4-9C3E-4915C39B565B}']
    function GetSubscriber: ISubscriber;
    procedure Publish(const Event: IInfraEvent);
    property Subscriber: ISubscriber read GetSubscriber;
  end;

  ICallBackSubscription = interface(ISubscription)
    ['{C1111067-B417-407B-BA25-E01D6F7EC88E}']
    function GetFilter: TSubscriberFilter;
    function GetInform: TSubscriberInform;
    property Filter: TSubscriberFilter read GetFilter;
    property Inform: TSubscriberInform read GetInform;
  end;

  IClassicSubscription = interface(ISubscription)
    ['{02269AA0-A39B-4C96-B057-7482EE4F3443}']
    function GetFilter: IInfraFilter;
    function GetSubscriber: ISubscriber;
    property Filter: IInfraFilter read GetFilter;
    property Subscriber: ISubscriber read GetSubscriber;
  end;

  ISubscriptionList = interface(IMemoryManagedObject)
    ['{CAFB19B6-91E5-45D7-BCE9-709794C1840E}']
    function Add(const Item: ISubscription): Integer;
    function First: ISubscription;
    function GetItem(AIndex: Integer): ISubscription;
    function GetCount: Integer;
    function NewIterator: IInfraIterator;
    function IndexOf(const Item: ISubscription): Integer;
    function Last: ISubscription;
    function Remove(const Item: ISubscription): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure SetItem(Index: Integer; const Value: ISubscription);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: ISubscription read GetItem
      write SetItem; default;
  end;

  IInfraEventType = interface(IInterface)
    ['{C41D07B2-72D7-49CD-8F0F-1B4C52F30935}']
  end;

  IInfraEvent = interface(IInterface)
    ['{D2460D4E-8B93-499A-B106-BADCFBC83351}']
    function GetSource: IElement;
    procedure SetSource(const Value: IElement);
    property Source: IElement read GetSource write SetSource;
  end;

  IInfraEventServiceItem = interface(IMemoryManagedObject)
    ['{54D2EDC8-299F-4055-9438-4B32CBF51D20}']
    function GetEventType: TGUID;
    function GetPublishers: IInfraPublisherList;
    function GetSubscriptions: ISubscriptionList;
    property EventType: TGUID read GetEventType;
    property Publishers: IInfraPublisherList read GetPublishers;
    property Subscriptions: ISubscriptionList read GetSubscriptions;
  end;

  IInfraEventServiceItems = interface(IInterface)
    ['{CA4A565C-8ABC-4567-9281-1F6F7E5F93AC}']
    function Add(ID: TGUID; Item: IInfraEventServiceItem): TGUID;
    function First: IInfraEventServiceItem;
    function GetCount: Integer;
    function GetItem(Index: TGUID): IInfraEventServiceItem;
    function IndexOf(Item: IInfraEventServiceItem): TGUID;
    function IndexOfPosition(Index: Integer): TGUID;
    function Last: IInfraEventServiceItem;
    function Remove(Item: IInfraEventServiceItem): TGUID;
    function ValueOfPosition(Index: Integer): IInfraEventServiceItem;
    procedure Clear;
    procedure Delete(Index: TGUID);
    procedure SetItem(Index: TGUID; Item: IInfraEventServiceItem);
    property Count: Integer read GetCount;
    property Items[Index: TGUID]: IInfraEventServiceItem read GetItem
      write SetItem; default;
  end;

  IInfraEventService = interface(IInterface)
    ['{553AED4B-820F-4CA5-8732-D30AEFD96751}']
    function GetItems: IInfraEventServiceItems;
    function GetPublishersForSubscriberIterator(
      const Subscriber: ISubscriber): IInfraIterator;
    function HasSubscribers(const EventType: TGUID): Boolean;
    procedure Publish(const Event: IInfraEvent);
    procedure Subscribe(const EventType: TGUID; const Subscriber: ISubscriber;
      const Filter: IInfraFilter); overload;
    procedure Subscribe(const EventType: TGUID;
      const Subscriber: ISubscriber; const Inform: TSubscriberInform;
      const PropertyName: string = '';
      const Filter: TSubscriberFilter = nil); overload;
    procedure UnSubscribe(const EventType: TGUID;
      const Subscriber: ISubscriber);
    procedure UnSubscribeAll(const Subscriber: ISubscriber);
    property Items: IInfraEventServiceItems read GetItems;
  end;

  // Metadata

  TMemberType = (mtConstructor, mtEvent, mtMethod, mtProperty);
  TMethodType = (mtJustMethods, mtJustConstructors, mtBoth);
  TMemberTypes = Set of TMemberType;
  TRelationKind = (rkAssociation, rkAgregation, rkComposition);
  TParameterOption = (poConst, poVar, poReturn);
  TParameterOptions = set of TParameterOption;
  TCallingConvention = (ccRegister, ccPascal, ccCdecl, ccStdcall, ccSafecall);

  IMemberInfo = interface;
  IMethodInfo = interface;
  IPropertyInfo = interface;
  IParameterInfo = interface;
  IParameterInfoList = interface;

  IClassInfoList = interface(IMemoryManagedObject)
    ['{A9E10732-7BED-4FC1-BCD2-556721330485}']
    function Add(const Item: IClassInfo): Integer;
    function First: IClassInfo;
    function GetCount: Integer;
    function GetItem(Index: Integer): IClassInfo;
    function IndexOf(const Item: IClassInfo): Integer;
    function Last: IClassInfo;
    function ByGUID(DataType: TGUID): IClassInfo;
    function ByClass(ClassType: TInfraBaseObjectClass): IClassInfo;
    function ByName(const pName: string): IClassInfo;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const Item: IClassInfo);
    procedure SetItem(Index: Integer; const TypeInfo: IClassInfo);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IClassInfo read GetItem
      write SetItem; default;
  end;

  IMemberInfoIterator = interface(IInterface)
    ['{2365C35D-6226-4BF8-85C5-080F9C0DF377}']
    function CurrentItem: IMemberInfo;
    procedure First;
    function IsDone: Boolean;
    procedure Next;
  end;

  IPropertyInfoIterator = interface(IInterface)
    ['{5C012700-E499-49AB-98E1-D95E6C85C173}']
    function CurrentItem: IPropertyInfo;
    procedure First;
    function IsDone: Boolean;
    procedure Next;
  end;

  IMethodInfoIterator = interface(IInterface)
    ['{5C012700-E499-49AB-98E1-D95E6C85C173}']
    function CurrentItem: IMethodInfo;
    function IsDone: Boolean;
    procedure First;
    procedure Next;
  end;

  IMemberInfoList = interface(IMemoryManagedObject)
    ['{A8439100-8F79-4693-A30C-04823B557769}']
    function Add(const Item: IMemberInfo): Integer;
    function First: IMemberInfo;
    function GetCount: Integer;
    function GetItem(Index: Integer): IMemberInfo;
    function IndexOf(const Item: IMemberInfo): Integer;
    function Last: IMemberInfo;
    procedure Clear;
    procedure Delete(Index: Integer);
    function NewMemberInfoIterator(
      MemberTypes: TMemberTypes): IMemberInfoIterator;
    function NewMethodInfoIterator(MethodType: TMethodType): IMethodInfoIterator;
    function NewPropertyInfoIterator: IPropertyInfoIterator;
    procedure Insert(Index: Integer; const Item: IMemberInfo);
    procedure SetItem(Index: Integer; const TypeInfo: IMemberInfo);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IMemberInfo read GetItem
      write SetItem; default;
  end;

  IPropertyInfoList = interface(IMemoryManagedObject)
    ['{86FED90A-06AC-4CB5-8BB8-2ECB78FA8A25}']
    function Add(const Item: IPropertyInfo): Integer;
    function First: IPropertyInfo;
    function GetCount: Integer;
    function GetItem(Index: Integer): IPropertyInfo;
    function IndexOf(const Item: IPropertyInfo): Integer;
    function Last: IPropertyInfo;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const Item: IPropertyInfo);
    procedure SetItem(Index: Integer; const TypeInfo: IPropertyInfo);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IPropertyInfo read GetItem
      write SetItem; default;
  end;

  IMethodInfoList = interface(IMemoryManagedObject)
    ['{EA1C173B-0079-4F53-819E-851A24C3798E}']
    function Add(const Item: IMethodInfo): Integer;
    function First: IMethodInfo;
    function GetCount: Integer;
    function GetItem(Index: Integer): IMethodInfo;
    function IndexOf(const Item: IMethodInfo): Integer;
    function Last: IMethodInfo;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const Item: IMethodInfo);
    procedure SetItem(Index: Integer; const TypeInfo: IMethodInfo);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IMethodInfo read GetItem
      write SetItem; default;
  end;

  IParameterInfoList = interface(IMemoryManagedObject)
    ['{E587DA28-2D36-4C37-AAA3-7817B9F8095E}']
    function Add(const Item: IParameterInfo): Integer;
    function First: IParameterInfo;
    function GetCount: Integer;
    function GetItem(Index: Integer): IParameterInfo;
    function IndexOf(const Item: IParameterInfo): Integer;
    function Last: IParameterInfo;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const Item: IParameterInfo);
    procedure SetItem(Index: Integer; const TypeInfo: IParameterInfo);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IParameterInfo read GetItem
      write SetItem; default;
  end;

  IRelationInfoList = interface(IMemoryManagedObject)
    ['{4C0735B1-779C-4E57-8799-89FC9727ED78}']
    function Add(const Item: IRelationInfo): Integer;
    function First: IRelationInfo;
    function GetCount: Integer;
    function GetItem(Index: Integer): IRelationInfo;
    function IndexOf(const Item: IRelationInfo): Integer;
    function Last: IRelationInfo;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const Item: IRelationInfo);
    procedure SetItem(Index: Integer; const TypeInfo: IRelationInfo);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IRelationInfo read GetItem
      write SetItem; default;
  end;

  IClassInfo = interface(IElement)
    ['{FCD45266-7AE7-4EB3-9F51-4CD22F3C7B4B}']
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
    function GetAllMethods: IMethodInfoIterator;
    function GetName: string;
    function GetOwner: IClassInfo;
    function GetProperties: IPropertyInfoIterator;
    function GetProperty(const Obj: IElement; const pName: String;
      const pClassInfo: IClassInfo = nil): IInterface;
    function GetPropertyInfo(const pName: String;
      ThrowException: Boolean = False): IPropertyInfo;
    function GetSuperClass: IClassInfo;
    function GetRetentionPolice: TRetentionPolice;
    function IsSubClassOf(const Value: IClassInfo): Boolean;
    function GetIsAnnotation: Boolean;
    procedure SetRetentionPolice(const Value: TRetentionPolice);
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
    property FamilyID: TGUID read GetClassFamily write SetClassFamily;
    property FullName: string read GetFullName;
    property TypeID: TGUID read GetTypeID write SetTypeID;
    property ImplClass: TInfraBaseObjectClass read GetImplClass
      write SetImplClass;
    property Name: string read GetName write SetName;
    property Owner: IClassInfo read GetOwner write SetOwner;
    property SuperClass: IClassInfo read GetSuperClass write SetSuperClass;
    property RetentionPolice: TRetentionPolice read GetRetentionPolice
      write SetRetentionPolice;
    property IsAnnotation: Boolean read GetIsAnnotation;
  end;

  IMemberInfo = interface(IElement)
    ['{879C1FB0-9FBF-4CAB-A4AC-E3A769C50304}']
    function GetDeclaringType: IClassInfo;
    function GetFullName: string;
    function GetMemberType: TMemberType;
    function GetName: string;
    procedure SetDeclaringType(const Value: IClassInfo);
    procedure SetMemberType(Value: TMemberType);
    procedure SetName(const Value: string);
    property DeclaringType: IClassInfo read GetDeclaringType write
      SetDeclaringType;
    property FullName: string read GetFullName;
    property MemberType: TMemberType read GetMemberType write SetMemberType;
    property Name: string read GetName write SetName;
  end;

  IPropertyInfo = interface(IMemberInfo)
    ['{D5063A5A-978F-475B-8CC1-2177F41DB28D}']
    function GetGetterInfo: IMethodInfo;
    function GetSetterInfo: IMethodInfo;
    function GetValue(const pObject: IInterface): IInterface;
    procedure SetValue(const pObject, pValue: IInterface);
    function GetTypeInfo: IClassInfo;
  end;

  IMethodInfo = interface(IMemberInfo)
    ['{F91AA616-6E05-4A0B-AC02-EFE207B32243}']
    function AddParam(const pName: string;
      pParameterType: IClassInfo; pOptions: TParameterOptions = [poConst];
      const pDefaultValue: IInterface = nil): IParameterInfo;
    function GetCallingConvention: TCallingConvention;
    function GetIsConstructor: Boolean;
    function GetIsFunction: Boolean;
    function GetMethodPointer: Pointer;
    function GetParameters: IParameterInfoList;
    function GetReturnType: IClassInfo;
    function Invoke(const pObj: IInfraInstance;
      const pParameters: IInterfaceList): IInterface; overload;
    function Invoke(pObj: TObject;
      const pParameters: IInterfaceList): IInterface; overload;
    property IsConstructor: Boolean read GetIsConstructor;
    property IsFunction: Boolean read GetIsFunction;
    property MethodPointer: Pointer read GetMethodPointer;
    property Parameters: IParameterInfoList read GetParameters;
    property ReturnType: IClassInfo read GetReturnType;
    property CallingConvention: TCallingConvention read GetCallingConvention;
  end;

  IConstructorInfo = interface(IMethodInfo)
    ['{E3A8CED1-0138-4CAA-BD83-75266DE95C8A}']
  end;

  IParameterInfo = interface(IMemoryManagedObject)
    ['{13334830-727F-4BB4-85DA-54EFE7673508}']
    function GetDefaultValue: IInterface;
    function GetIsOptional: boolean;
    function GetIsConst: boolean;
    function GetIsVar: boolean;
    function GetIsReturn: boolean;
    function GetName: string;
    function GetParameterType: IClassInfo;
    function GetPosition: integer;
    property DefaultValue: IInterface read GetDefaultValue;
    property IsConst: boolean read GetIsConst;
    property IsOptional: boolean read GetIsOptional;
    property IsReturn: boolean read GetIsReturn;
    property IsVar: boolean read GetIsVar;
    property Name: string read GetName;
    property ParameterType: IClassInfo read GetParameterType;
    property Position: integer read GetPosition;
  end;

  IMultiplicityInfo = interface(IMemoryManagedObject)
    ['{8E7BD210-2649-4F16-B19A-BEFD4CF2BC9F}']
    function GetLower: TRelationLimit;
    function GetUpper: TRelationLimit;
    procedure SetLower(Value: TRelationLimit);
    procedure SetUpper(Value: TRelationLimit);
    property Lower: TRelationLimit read GetLower write SetLower;
    property Upper: TRelationLimit read GetUpper write SetUpper;
  end;

  IRelationEndInfo = interface(IMemoryManagedObject)
    ['{F10A3702-1303-4FB1-B9F4-AB504866C1C5}']
    function GetPropertyInfo: IPropertyInfo;
    function GetMultiplicity: IMultiplicityInfo;
    function GetOtherEnd: IRelationEndInfo;
    procedure SetPropertyInfo(const Value: IPropertyInfo);
    property PropertyInfo: IPropertyInfo read GetPropertyInfo
      write SetPropertyInfo;
    property Multiplicity: IMultiplicityInfo read GetMultiplicity;
    property OtherEnd: IRelationEndInfo read GetOtherEnd;
  end;

  IRelationInfo = interface(IMemoryManagedObject)
    ['{6249107B-0C3F-4246-87C0-C9D92E2106F6}']
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
  end;

  IClassInfoIterator = interface(IInterface)
    ['{F252D2F0-A40A-4F99-931F-3FD1262FAF67}']
    function CurrentItem: IClassInfo; overload;
    procedure First;
    function IsDone: Boolean;
    procedure Next;
  end;

  IRelationInfoIterator = interface(IInterface)
    ['{110E3E80-B9FB-479C-A68C-5A9AFA7E54FE}']
    function CurrentItem: IRelationInfo;
    procedure First;
    function IsDone: Boolean;
    procedure Next;
  end;

  ITypeService = interface(IInterface)
    ['{93190314-B32A-4C57-B28E-478C99DEE0BA}']
    function CreateInstance(ClassID: TGUID): IElement; overload;
    function CreateInstance(const ClassInfo: IClassInfo): IElement; overload;
    function AddAnnotation(const pTypeID: TGUID;
      const pTypeName: string; pClassImplementing: TInfraBaseObjectClass;
      const pFamilyID: TGUID; const pSuperClassInfo: IClassInfo = nil;
      pRetention: TRetentionPolice = rpClass): IClassInfo;
    function AddType(const pTypeID: TGUID; const pTypeName: string;
      pClassImplementing: TInfraBaseObjectClass; const pFamilyID: TGUID;
      const pSuperClassInfo: IClassInfo = nil): IClassInfo;
    function GetRelations: IRelationInfoList;
    function NewClassInfoIterator: IClassInfoIterator;
    function NewRelationsIterator(
      const TypeInfo: IClassInfo): IRelationInfoIterator; overload;
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
    property Relations: IRelationInfoList read GetRelations;
    property Types: IClassInfoList read GetTypes;
  end;

  // Iterator

  IInfraIterator = interface(IInterface)
    ['{6DC5C8D0-643A-4926-8AC8-0814D36D8566}']
    function CurrentItem: IInterface;
    procedure First;
    function IsDone: Boolean;
    procedure Next;
  end;

  // Basic List

  IInfraCustomList = interface(IInterfaceList)
    ['{C3677798-1424-465F-BC99-EA58AD9A6E72}']
    function NewIterator: IInfraIterator;
  end;

procedure RegisterApplicationContext(const Context: IApplicationContext);
function ApplicationContext: IApplicationContext;

function EventService: IInfraEventService;
function ReferenceService: IInfraReferenceService;
function TypeService: ITypeService;

const
  mtAll = [mtConstructor..mtProperty];

implementation

var
  _ApplicationContext: IApplicationContext = nil;

procedure RegisterApplicationContext(const Context: IApplicationContext);
begin
  _ApplicationContext := Context;
end;

function ApplicationContext: IApplicationContext;
begin
  result := _ApplicationContext;
end;

function EventService: IInfraEventService;
begin
  result := ApplicationContext.EventService;
end;

function ReferenceService: IInfraReferenceService;
begin
  result := ApplicationContext.ReferenceService;
end;

function TypeService: ITypeService;
begin
  result := ApplicationContext.TypeService;
end;

initialization

finalization
  if Assigned(_ApplicationContext) then
    _ApplicationContext.ShutDown;

end.





