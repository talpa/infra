{
   Exemplo da precedencia:
   - Considre que existe 3 aspectos A1, A2, A3
   - todos implementam todos os advices
   - foi registrado CLASSEA.METODO1 para os métodos A1 e A3
   Precedencia será
   A1.Around
   A1.Proceed? ---> A3.Around
   A3.Proceed? ---> A1.Before, A3.Before, Real, A3.After, A1.After

   Problemas:
   - Quando chama proceed de um advice before ou after entra em loop
}
unit InfraAspect;

{$I 'Infra.Inc'}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf,{$ENDIF}
  Types,
  InfraAspectIntf,
  InfraCommonIntf,
  InfraValueTypeIntf,
  InfraCommon,
  InfraAspectVMTUtil;

type
  TInfraAspect = class(TElement, IInfraAspect)
  protected
    function Proceed: IInfraType;
    procedure After(const Sender: IElement;
      const Params: IInfraList); virtual;
    function Around(const Sender: IElement;
      const Params: IInfraList): IInfraType; virtual;
    procedure Before(const Sender: IElement;
      const Params: IInfraList); virtual;
  end;

  TInfraJointPoint = class(TMemoryManagedObject, IInfraJointPoint)
  private
    FAspectClass: TClass;
    FMethodInfo: IClassInfo;
    FMethodIndex: integer;
    FParamsCount: integer;
    function GetAspectClass: TClass;
    function GetMethodInfo: IClassInfo;
    function GetMethodIndex: Integer;
    procedure SetAspectClass(Value: TClass);
    procedure SetMethodInfo(const Value: IClassInfo);
    procedure SetMethodIndex(Value: Integer);
    function GetParamsCount: integer;
    procedure SetParamsCount(Value: integer);
  public
    property AspectClass: TClass read GetAspectClass write SetAspectClass;
    property MethodInfo: IClassInfo read GetMethodInfo write SetMethodInfo;
    property MethodIndex: integer read GetMethodIndex write SetMethodIndex;
    property ParamsCount: integer read GetParamsCount write SetParamsCount;
    constructor Create;
  end;

  TInterceptedStackItem = class(TInterfacedObject, IInterceptedStackItem)
  private
    FAspect: IInfraAspect;
    FInstance: Pointer;
    FJointPoint: IInfraJointPoint;
    FParams: IInfraList;
    function GetAspect: IInfraAspect;
    function GetInstance: Pointer;
    function GetJointPoint: IInfraJointPoint;
    function GetRealMethodInfo: IClassInfo;
    procedure SetAspect(const Value: IInfraAspect);
    procedure SetInstance(const Value: Pointer);
    procedure SetJointPoint(const Value: IInfraJointPoint);
    function GetParams: IInfraList;
    procedure SetParams(const Value: IInfraList);
  public
    property Aspect: IInfraAspect read FAspect write FAspect;
    property Instance: Pointer read FInstance write FInstance;
    property JointPoint: IInfraJointPoint read FJointPoint write FJointPoint;
    property RealMethodInfo: IClassInfo read GetRealMethodInfo;
    property Params: IInfraList read GetParams write SetParams;
  end;

  TInfraAspectService = class(TMemoryManagedObject, IInfraAspectService)
  private
    FInterceptedStack: IInterceptedStack;
    FJointPoints: IInfraJointPoints;
    FStubs: TStubArray;
    FAspectPointcutEval: IAspectPointcutEval;
    procedure AddPointCut(const pExpression: string; pAspectClass: TClass);
    function Proceed: IInfraType;
    function GetJointPoints: IInfraJointPoints;
    function SetStub(const pMethodInfo: IClassInfo): integer;
    function CallAdvices(const pJointPoint: IInfraJointPoint;
      pInstance: Pointer; const pParams: IInfraList): IInfraType;
    function Match(const pMethodName, pExpression: string): boolean;
    function GetNextJointPoint(
      const pJointPoint: IInfraJointPoint): IInfraJointPoint;
    function CallMethod: IInfraType;
    procedure CallBeforeAdvices;
    procedure CallAfterAdvices;
    procedure CreateJointPoint(pAspectClass: TClass;
      const vTypeInfo: IClassInfo; pParamCount: Integer);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  List_InterceptMethod,
  List_JointPoint,
  InfraAspectEval;  

{ TInfraAspect }

procedure TInfraAspect.After(const Sender: IElement;
  const Params: IInfraList);
begin
  // implemented on descendants
end;

function TInfraAspect.Around(const Sender: IElement;
  const Params: IInfraList): IInfraType;
begin
  {$IFDEF USE_GXDEBUG}
  SendDebug(ClassName+'.Around('+Sender.TypeInfo.FullName+') Called');
  {$ENDIF}
  Result := Proceed;
  {$IFDEF USE_GXDEBUG}
  SendDebug(ClassName+'.Around end');
  {$ENDIF}
end;

procedure TInfraAspect.Before(const Sender: IElement;
  const Params: IInfraList);
begin
  // implemented on descendants
end;

function TInfraAspect.Proceed: IInfraType;
begin
  {$IFDEF USE_GXDEBUG}
  SendDebug(ClassName+' Proceed called');
  {$ENDIF}
  Result := AspectService.Proceed;
  {$IFDEF USE_GXDEBUG}
  SendDebug(ClassName+' Proceed end');
  {$ENDIF}
end;

{ TInfraJointPoint }

constructor TInfraJointPoint.Create;
begin
  inherited Create;
end;

function TInfraJointPoint.GetAspectClass: TClass;
begin
  Result := FAspectClass;
end;

function TInfraJointPoint.GetMethodInfo: IClassInfo;
begin
  Result := FMethodInfo;
end;

function TInfraJointPoint.GetMethodIndex: Integer;
begin
  Result := FMethodIndex;
end;

function TInfraJointPoint.GetParamsCount: integer;
begin
  Result := FParamsCount
end;

procedure TInfraJointPoint.SetAspectClass(Value: TClass);
begin
  if Value <> FAspectClass then
    FAspectClass := Value;
end;

procedure TInfraJointPoint.SetMethodInfo(const Value: IClassInfo);
begin
  if Value <> FMethodInfo then
    FMethodInfo := Value;
end;

procedure TInfraJointPoint.SetMethodIndex(Value: Integer);
begin
  if Value <> FMethodIndex then
    FMethodIndex := Value;
end;

procedure TInfraJointPoint.SetParamsCount(Value: integer);
begin
  if Value <> FParamsCount then
    FParamsCount := Value;
end;

{ TInterceptedStackItem }

function TInterceptedStackItem.GetAspect: IInfraAspect;
begin
  Result := FAspect
end;

function TInterceptedStackItem.GetInstance: Pointer;
begin
  Result := FInstance
end;

function TInterceptedStackItem.GetJointPoint: IInfraJointPoint;
begin
  Result := FJointPoint
end;

function TInterceptedStackItem.GetRealMethodInfo: IClassInfo;
begin
  if Assigned(FJointPoint) then
    Result := JointPoint.MethodInfo
  else
    Result := nil;
end;

function TInterceptedStackItem.GetParams: IInfraList;
begin
  Result := FParams;
end;

procedure TInterceptedStackItem.SetAspect(const Value: IInfraAspect);
begin
  FAspect := Value;
end;

procedure TInterceptedStackItem.SetInstance(const Value: Pointer);
begin
  FInstance := Value;
end;

procedure TInterceptedStackItem.SetJointPoint(
  const Value: IInfraJointPoint);
begin
  FJointPoint := Value;
end;

procedure TInterceptedStackItem.SetParams(const Value: IInfraList);
begin
  FParams := Value;
end;

{ TInfraAspectService }

constructor TInfraAspectService.Create;
begin
  inherited;
  FInterceptedStack := TInterceptedStack.Create;
  FJointPoints := TInfraJointPoints.Create;
  FAspectPointcutEval := TAspectPointcutEval.Create;
end;

destructor TInfraAspectService.Destroy;
var
 i: integer;
begin
  for i := 0 to Length(FStubs)-1 do
    FreeMem(FStubs[i], SIZE_OF_STUB);
  inherited;
end;

procedure TInfraAspectService.AddPointCut(const pExpression: string;
  pAspectClass: TClass);
var
  MethodsIterator: IInfraIterator;
  vTypeInfo: IClassInfo;
begin
  MethodsIterator := TypeService.GetMethodsIterator;
  while not MethodsIterator.IsDone do
  begin
    vTypeInfo := (MethodsIterator.CurrentItem as IClassInfo);
    if Match(vTypeInfo.FullName, pExpression) then
    begin
      {$IFDEF USE_GXDEBUG}
      SendDebug('    '+vTypeInfo.FullName);
      {$ENDIF}
      CreateJointPoint(pAspectClass, vTypeInfo, vTypeInfo.MemberInfos.Count);
    end;
    MethodsIterator.Next;
  end;
end;

// Intercept Method and Create a jointpoint to hold information
// about aspect and interception.
procedure TInfraAspectService.CreateJointPoint(pAspectClass: TClass;
  const vTypeInfo: IClassInfo; pParamCount: Integer);
var
  IndexJointPoint, Index: Integer;
begin
  Index := SetStub(vTypeInfo);
  IndexJointPoint := FJointPoints.Add(
    TInfraJointPoint.Create as IInfraJointPoint);
  with FJointPoints[IndexJointPoint] do
  begin
    AspectClass := pAspectClass;
    MethodInfo := vTypeInfo;
    MethodIndex := Index;
    ParamsCount := pParamCount;
  end;
end;

function TInfraAspectService.SetStub(const pMethodInfo: IClassInfo): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FJointPoints.Count-1 do
    if FJointPoints[i].MethodInfo.Method = pMethodInfo.Method then
    begin
      {$IFDEF INFRA_CLASSNAMEDESTROYED}
      SendDebug('Stub Reused!!!');
      {$ENDIF}
      Result := FJointPoints[i].MethodIndex;
      break;
    end;
  if Result = -1 then
    Result := CreateStub(FStubs, pMethodInfo);
end;

function TInfraAspectService.Match(const pMethodName,
  pExpression: string): boolean;
begin
  Result := FAspectPointcutEval.Evaluate(pExpression, pMethodName);
end;

// Get AspectService's JointPoints list.
function TInfraAspectService.GetJointPoints: IInfraJointPoints;
begin
  if not Assigned(FJointPoints) then
    FJointPoints := TInfraJointPoints.Create;
  Result := FJointPoints;
end;

// Create a AspectType, an InterceptedItem to hold the context between aspects
// and call all aspects advices recursively.
function TInfraAspectService.CallAdvices(
  const pJointPoint: IInfraJointPoint; pInstance: Pointer;
  const pParams: IInfraList): IInfraType;
var
  Intercepted: IInterceptedStackItem;
  pAspect: IInfraAspect;
  pSender: IElement;
begin
  Assert(Supports(pJointPoint.AspectClass.Create, IInfraAspect, pAspect),
    'Class not supports IInfraAspect');
  Intercepted := TInterceptedStackItem.Create;
  with Intercepted do
  begin
    JointPoint := pJointPoint;
    Aspect := pAspect as IInfraAspect;
    Instance := pInstance;
    Params := pParams;
  end;
  if not Supports(TObject(Intercepted.Instance), IElement, pSender) then
    Raise Exception.Create('Instance not supports Element');
  FInterceptedStack.Push(Intercepted);
  Result := Intercepted.Aspect.Around(pSender, Intercepted.Params);
  FInterceptedStack.Pop;
end;

// Get next JointPoint to be recursively processed.
function TInfraAspectService.GetNextJointPoint(
  const pJointPoint: IInfraJointPoint): IInfraJointPoint;
var
  i, j: integer;
begin
  Result := nil;
  i := FJointPoints.Indexof(pJointPoint)+1;
  if i <> FJointPoints.Count then
    for j := i to FJointPoints.Count-1 do
      if FJointPoints[j].MethodInfo = pJointPoint.MethodInfo then
      begin
        Result := FJointPoints[j];
        Break;
      end;
end;

// CallAdvices if there is a next JointPoint, or call Realmethod if is the
// last JointPoint
function TInfraAspectService.Proceed: IInfraType;
var
  NextJointPoint: IInfraJointPoint;
begin
  Result := nil;
  with FInterceptedStack do
  begin
    NextJointPoint := GetNextJointPoint(Peek.JointPoint);
    if Assigned(NextJointPoint) then
      Result := CallAdvices(NextJointPoint, Peek.Instance, Peek.Params)
    else
      Result := CallMethod;
  end;
end;

procedure TInfraAspectService.CallBeforeAdvices;
var
  vIterator: IInfraIterator;
  vStackItem: IInterceptedStackItem;
  vSender: IElement;
begin
  vIterator := FInterceptedStack.NewIterator;
  while not vIterator.IsDone do
  begin
    vStackItem := (vIterator.CurrentItem as IInterceptedStackItem);
    with vStackItem do
      if JointPoint.MethodInfo = RealMethodInfo then
      begin
        if not Supports(TObject(Instance), IElement, vSender) then
          Raise Exception.Create('Instance not supports Element');
        Aspect.Before(vSender, Params);
      end;
    vIterator.Next;
  end;
end;

procedure TInfraAspectService.CallAfterAdvices;
var
  vIterator: IInfraIterator;
  vStackItem: IInterceptedStackItem;
  vSender: IElement;
begin
  vIterator := FInterceptedStack.NewInverseIterator;
  while not vIterator.IsDone do
  begin
    vStackItem := (vIterator.CurrentItem as IInterceptedStackItem);
    with vStackItem do
      if JointPoint.MethodInfo = RealMethodInfo then
      begin
        if not Supports(TObject(Instance), IElement, vSender) then
          Raise Exception.Create('Instance not supports Element');
        Aspect.After(vSender, Params);
      end;
    vIterator.Next;
  end;
end;

function TInfraAspectService.CallMethod: IInfraType;
begin
  Result := nil;
  with FInterceptedStack do
  begin
    CallBeforeAdvices;
    Result := InvokeMethod(TObject(Peek.Instance),
      Peek.RealMethodInfo, Peek.Params);
    CallAfterAdvices;
  end;
end;

// we cannot put directly on initialization (will receive AV)
procedure InjectAspectService;
begin
  with (ApplicationContext as IMemoryManagedObject).InjectedInterfaces do
    Add(IInfraAspectService,
      TInfraAspectService.Create as IInfraAspectService);
end;

initialization
  InjectAspectService;

end.




