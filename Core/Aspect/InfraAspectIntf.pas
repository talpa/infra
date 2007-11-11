unit InfraAspectIntf;

interface

uses
  Classes,
  InfraCommonIntf;

type
  IInfraAspect = interface(IElement)
    ['{F0B6338B-4CA5-4142-9C2E-571F976AFC59}']
    function Around(const Sender: IElement;
      const Params: IInterfaceList): IInterface;
    function Proceed: IInterface;
    procedure After(const Sender: IElement; const Params: IInterfaceList);
    procedure Before(const Sender: IElement; const Params: IInterfaceList);
  end;

  // A intercepted application's point
  IInfraJointPoint = interface(IInterface)
    ['{E4EF3225-8558-4E70-A1A2-31312AFB5651}']
    function GetAspectClass: TClass;
    function GetMethodInfo: IMethodInfo;
    function GetMethodIndex: Integer;
    function GetParamsCount: Integer;
    procedure SetAspectClass(Value: TClass);
    procedure SetMethodInfo(const Value: IMethodInfo);
    procedure SetMethodIndex(Value: integer);
    // Aspect to be executed when program come in JoinPoint
    property AspectClass: TClass read GetAspectClass write SetAspectClass;
    // TypeInfo of Intercepted Method
    property MethodInfo: IMethodInfo read GetMethodInfo write SetMethodInfo;
    // Index of Stub Method on array of stubs
    property MethodIndex: integer read GetMethodIndex write SetMethodIndex;
    // Method's parameters Quatity
    property ParamsCount: integer read GetParamsCount;
  end;

  // list of joint points
  IInfraJointPoints = interface(IInterface)
    ['{18FE6D1B-6C49-4AD4-AD83-11A98E2406EF}']
    function Add(const Item: IInfraJointPoint): Integer;
    function NewIterator: IInfraIterator;
    function First: IInfraJointPoint;
    function GetCount: Integer;
    function GetItem(Index: Integer): IInfraJointPoint;
    function Last: IInfraJointPoint;
    function IndexOf(const Item: IInfraJointPoint): Integer;
    procedure SetItem(Index: Integer; const Value: IInfraJointPoint);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IInfraJointPoint read GetItem
      write SetItem; default;
  end;

  IAspectPointcutEval = interface
    ['{072D8133-AC1D-422E-AD44-82C97F668542}']
    function Evaluate(const TextToMatch, Expression: string): Boolean;
  end;

  // A item of stack that hold context between call advices
  IInterceptedStackItem = interface
    ['{3227229C-0AE8-4CE6-B818-0C81C4CFFECE}']
    function GetAspect: IInfraAspect;
    function GetInstance: Pointer;
    function GetJointPoint: IInfraJointPoint;
    function GetRealMethodInfo: IMethodInfo;
    function GetParams: IInterfaceList;
    procedure SetAspect(const Value: IInfraAspect);
    procedure SetInstance(const Value: Pointer);
    procedure SetJointPoint(const Value: IInfraJointPoint);
    procedure SetParams(const Value: IInterfaceList);
    property Aspect: IInfraAspect read GetAspect write SetAspect;
    property Instance: Pointer read GetInstance write SetInstance;
    property JointPoint: IInfraJointPoint read GetJointPoint
      write SetJointPoint;
    property RealMethodInfo: IMethodInfo read GetRealMethodInfo;
    property Params: IInterfaceList read GetParams write SetParams;
  end;

  // A stack to hold aspect context between call advices
  IInterceptedStack = interface
    ['{AD375F4E-95C4-41B8-914A-DE3781D69A2E}']
    function AtLeast(ACount: Integer): Boolean;
    function GetCount: Integer;
    function Peek: IInterceptedStackItem;
    function Pop: IInterceptedStackItem;
    function Push(AItem: IInterceptedStackItem): IInterceptedStackItem;
    function NewIterator: IInfraIterator;
    function NewInverseIterator: IInfraIterator;
    property Count: Integer read GetCount;
  end;

  { Service to mantain the intercepted joint points and execute the
    aspects so that the program's flow reach some these joint points }
  IInfraAspectService = interface(IMemoryManagedObject)
    ['{F1C7615D-BE6A-4638-ADF4-34528CB0CE9E}']
    // Find and Register a Aspect to all Joint Points matching with Expression
    procedure AddPointCut(const Expression: string; pAspectClass: TClass);
    function GetJointPoints: IInfraJointPoints;
    function Proceed: IInterface;
    function CallAdvices(const pJointPoint: IInfraJointPoint;
      pInstance: Pointer; const pParams: IInterfaceList): IInterface;
    property JointPoints: IInfraJointPoints read GetJointPoints;
  end;

function AspectService: IInfraAspectService;

implementation

function AspectService: IInfraAspectService;
begin
  result := ApplicationContext as IInfraAspectService;
end;

end.
