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
  Classes,
  InfraAspectIntf,
  InfraCommonIntf,
  InfraCommon,
  InfraAspectUtil;

type
  {
    Classe base para construção de um aspecto

    Qualquer aspecto de sua aplicação deve herdar desta classe.

    Esta Classe possui o que chamamos na POA de Advices, representados
    aqui por Around, Before e After.
  }
  TInfraAspect = class(TElement, IInfraAspect)
  protected
    // descendentes implementam este Advice quando precisa enclausurar a
    // chamada ao método real. Muito usado em situações como controle de
    // transações. Internamente este método pode ou nao chamar o método
    // Proceed.
    function Around(const Sender: IElement;
      const Params: IInterfaceList): IInterface; virtual;
    // descendentes implementam este Advice quando precisa de algo que seja
    // executado antes da chamada ao método real.
    procedure Before(const Sender: IElement;
      const Params: IInterfaceList); virtual;
    // descendentes implementam este Advice quando precisa de algo que seja
    // executado após a chamada ao método real.
    procedure After(const Sender: IElement;
      const Params: IInterfaceList); virtual;
    // chama Proceed no servico de aspectos
    function Proceed: IInterface;
  end;

  {
    Esta classe mapeia um método a um aspecto. Objetos deste tipo são criados
    usando o AspectService.AddPointCut.
  }
  TInfraJointPoint = class(TBaseElement, IInfraJointPoint)
  private
    FAspectClass: TClass;
    FMethodInfo: IMethodInfo;
    FMethodIndex: integer;
    function GetAspectClass: TClass;
    function GetMethodInfo: IMethodInfo;
    function GetMethodIndex: Integer;
    procedure SetAspectClass(Value: TClass);
    procedure SetMethodInfo(const Value: IMethodInfo);
    procedure SetMethodIndex(Value: Integer);
    function GetParamsCount: integer;
  public
    property AspectClass: TClass read GetAspectClass write SetAspectClass;
    property MethodInfo: IMethodInfo read GetMethodInfo write SetMethodInfo;
    property MethodIndex: integer read GetMethodIndex write SetMethodIndex;
    property ParamsCount: integer read GetParamsCount;
    constructor Create; override;
  end;

  // classe resposavel por manter o contexto entre a execução dos aspectos
  // e poder chamá-los recursivamente.
  TInterceptedStackItem = class(TInterfacedObject, IInterceptedStackItem)
  private
    FAspect: IInfraAspect;
    FInstance: Pointer;
    FJointPoint: IInfraJointPoint;
    FParams: IInterfaceList;
    function GetAspect: IInfraAspect;
    function GetInstance: Pointer;
    function GetJointPoint: IInfraJointPoint;
    function GetRealMethodInfo: IMethodInfo;
    procedure SetAspect(const Value: IInfraAspect);
    procedure SetInstance(const Value: Pointer);
    procedure SetJointPoint(const Value: IInfraJointPoint);
    function GetParams: IInterfaceList;
    procedure SetParams(const Value: IInterfaceList);
  public
    property Aspect: IInfraAspect read FAspect write FAspect;
    property Instance: Pointer read FInstance write FInstance;
    property JointPoint: IInfraJointPoint read FJointPoint write FJointPoint;
    property RealMethodInfo: IMethodInfo read GetRealMethodInfo;
    property Params: IInterfaceList read GetParams write SetParams;
  end;

  {
    Serviço para se programar orientada a aspectos no delphi.

    Este serviço armazena:
      - uma pilha de aspectos para manter o contexto de execução;
      - uma lista de stubs. Um stub é um bloco de memoria que chama o
        AspectService para chamar os Advices. Existe um Stub para cada
        Indice de método interceptado (economizando memoria)
        Os métodos reais tem seus endereços substituídos pelo endereço
        do stub corresponde ao indice do metodo na tabela de métodos virtuais
        da classe
      - uma lista de jointpoints
  }
  TInfraAspectService = class(TBaseElement, IInfraAspectService)
  private
    FInterceptedStack: IInterceptedStack;
    FJointPoints: IInfraJointPoints;
    FStubs: TStubArray;
    FAspectPointcutEval: IAspectPointcutEval;
    // Define um ou mais métodos de classes da aplicação onde um determinado
    // aspecto deve atuar. Os métodos sao escolhidos atraves de uma expressao.
    procedure AddPointCut(const pExpression: string; pAspectClass: TClass);
    // Chama todos os Advices recursivamente até o ultimo Joint Point
    // e então chama o método real
    function Proceed: IInterface;
    // Retorna a lista de JointPoints registrados no AspectService.
    function GetJointPoints: IInfraJointPoints;
    // Cria um novo Stub caso ainda nao exista um para o indice do método
    // passado como parametro.
    function SetStub(const pMethodInfo: IMethodInfo): integer;
    // Aramazena o JointPoint na pilha de execução, Cria o Aspecto definido
    // neste JointPoint e chama o Advice Around do aspecto criado.
    function CallAdvices(const pJointPoint: IInfraJointPoint;
      pInstance: Pointer; const pParams: IInterfaceList): IInterface;
    // Verifica se o nome de um método é compatível a uma expressao
    function Match(const pMethodName, pExpression: string): boolean;
    // Tenta retornar o próximo JointPoint que aponta para o mesmo método que
    // está sendo atualmente processado.
    function GetNextJointPoint(
      const pJointPoint: IInfraJointPoint): IInfraJointPoint;
    // Pega o JointPoint atual, chama todos os Advices Before, o método real e 
    // depois todos os Advices After.
    function CallMethod: IInterface;
    // Chama Advices do tipo before para todos os aspectos relacionados ao
    // metodo atual;
    procedure CallBeforeAdvices;
    // Chama Advices do tipo after para todos os aspectos relacionados ao
    // metodo atual;
    procedure CallAfterAdvices;
    // Cria um JointPoint, relacionando uma classe de aspecto a um metodo.
    // metodo atual. Caso necessário um stub é criado;
    procedure CreateJointPoint(pAspectClass: TClass;
      const vTypeInfo: IMethodInfo);
  public
    constructor Create; override;
    // detroi todos os stubs criados
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  List_InterceptMethod,
  List_JointPoint,
  InfraAspectEval;

{ TInfraAspect }

function TInfraAspect.Around(const Sender: IElement;
  const Params: IInterfaceList): IInterface;
begin
  Result := Proceed;
end;

procedure TInfraAspect.Before(const Sender: IElement;
  const Params: IInterfaceList);
begin

end;

procedure TInfraAspect.After(const Sender: IElement;
  const Params: IInterfaceList);
begin

end;

function TInfraAspect.Proceed: IInterface;
begin
  Result := AspectService.Proceed;
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

function TInfraJointPoint.GetMethodInfo: IMethodInfo;
begin
  Result := FMethodInfo;
end;

function TInfraJointPoint.GetMethodIndex: Integer;
begin
  Result := FMethodIndex;
end;

function TInfraJointPoint.GetParamsCount: integer;
begin
  Result := FMethodInfo.Parameters.Count;
end;

procedure TInfraJointPoint.SetAspectClass(Value: TClass);
begin
  if Value <> FAspectClass then
    FAspectClass := Value;
end;

procedure TInfraJointPoint.SetMethodInfo(const Value: IMethodInfo);
begin
  if Value <> FMethodInfo then
    FMethodInfo := Value;
end;

procedure TInfraJointPoint.SetMethodIndex(Value: Integer);
begin
  if Value <> FMethodIndex then
    FMethodIndex := Value;
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

function TInterceptedStackItem.GetRealMethodInfo: IMethodInfo;
begin
  if Assigned(FJointPoint) then
    Result := JointPoint.MethodInfo
  else
    Result := nil;
end;

function TInterceptedStackItem.GetParams: IInterfaceList;
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

procedure TInterceptedStackItem.SetParams(const Value: IInterfaceList);
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
  vClassInfoIterator: IClassInfoIterator;
  vMethodsIterator: IMethodInfoIterator;
  vMethodInfo: IMethodInfo;
begin
  vClassInfoIterator := TypeService.NewClassInfoIterator;
  while not vClassInfoIterator.IsDone do
  begin
    vMethodsIterator := vClassInfoIterator.CurrentItem.GetMethods;
    while not vMethodsIterator.IsDone do
    begin
      vMethodInfo := (vMethodsIterator.CurrentItem as IMethodInfo);
      if Match(vMethodInfo.FullName, pExpression) then
        CreateJointPoint(pAspectClass, vMethodInfo);
      vMethodsIterator.Next;
    end;
    vClassInfoIterator.Next;
  end;
end;

procedure TInfraAspectService.CreateJointPoint(pAspectClass: TClass;
  const vTypeInfo: IMethodInfo);
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
  end;
end;

function TInfraAspectService.SetStub(const pMethodInfo: IMethodInfo): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FJointPoints.Count-1 do
    if FJointPoints[i].MethodInfo.MethodPointer = pMethodInfo.MethodPointer then
    begin
      Result := FJointPoints[i].MethodIndex;
      break;
    end;
  if Result = -1 then
    Result := CreateStub(FStubs, pMethodInfo);
end;

function TInfraAspectService.Match(const pMethodName,
  pExpression: string): boolean;
begin
  Result := FAspectPointcutEval.Evaluate(pMethodName, pExpression);
end;

function TInfraAspectService.GetJointPoints: IInfraJointPoints;
begin
  if not Assigned(FJointPoints) then
    FJointPoints := TInfraJointPoints.Create;
  Result := FJointPoints;
end;

function TInfraAspectService.CallAdvices(
  const pJointPoint: IInfraJointPoint; pInstance: Pointer;
  const pParams: IInterfaceList): IInterface;
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

function TInfraAspectService.Proceed: IInterface;
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

function TInfraAspectService.CallMethod: IInterface;
begin
  Result := nil;
  with FInterceptedStack do
  begin
    CallBeforeAdvices;
    Result := Peek.RealMethodInfo.Invoke(
      TObject(Peek.Instance), Peek.Params);
    CallAfterAdvices;
  end;
end;

// Não entendi mas se por direto no Initialization acontece
// Access Violations.
procedure InjectAspectService;
begin
  (ApplicationContext as IBaseElement).Inject(
    IInfraAspectService, TInfraAspectService.Create);
end;

initialization
  InjectAspectService;

end.
