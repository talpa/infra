unit MethodInfoTests;

interface

uses
  TestFrameWork,
  InfraCommonIntf,
  ReflectModelIntf;

type
  TMethodInfoTests = class(TTestCase)
  private
    FMockMethod: IClassInfo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Test methods
    procedure TestAddParam;
    procedure TestGetCallingConvention;
    procedure TestGetIsConstructor;
    procedure TestGetMethodPointer;
    procedure TestGetParameters;
    procedure TestGetReturnType;
    procedure TestInvoke_Procedures;
    procedure TestInvoke_Functions;
  end;

implementation

uses
  SysUtils, InfraConsts,
  InfraValueTypeIntf,
  InfraValueType,
  Classes,
  ReflectModel,
  InfraReflect;

{ TMethodInfoTests }

procedure TMethodInfoTests.SetUp;
begin
  inherited;
  FMockMethod := TSetupModel.RegisterMockMethod;
end;

procedure TMethodInfoTests.TearDown;
begin
  TSetupModel.RemoveTypeInfo(IMockMethod);
  inherited;
end;

procedure TMethodInfoTests.TestAddParam;
begin
  CheckEquals(0, FMockMethod.GetMethodInfo('Constructor0').Parameters.Count,
    'Constructor0 don''''t should be paramaters');

  CheckEquals(1, FMockMethod.GetMethodInfo('Constructor1').Parameters.Count,
    'Constructor1 should have 1 parameter');

  // *** Funões tem 1 parametro a mais que é o result. tem que ver como
  // *** ficará isso. Se vamos retornar apenas os verdadeiros parâmetros ou
  // *** deixamos como está.

  CheckEquals(1, FMockMethod.GetMethodInfo('MethodFunc0').Parameters.Count,
    'MethodFunc0 don''''t should be paramaters');

  CheckEquals(2, FMockMethod.GetMethodInfo('MethodFunc1').Parameters.Count,
    'MethodFunc1 should have 1 parameter');

  CheckEquals(3, FMockMethod.GetMethodInfo('MethodFunc2').Parameters.Count,
    'MethodFunc2 should have 2 parameters');

  CheckEquals(4, FMockMethod.GetMethodInfo('MethodFunc3').Parameters.Count,
    'MethodFunc3 should have 3 parameters');

  CheckEquals(5, FMockMethod.GetMethodInfo('MethodFunc4').Parameters.Count,
    'MethodFunc4 should have 4 parameters');

  CheckEquals(6, FMockMethod.GetMethodInfo('MethodFunc5').Parameters.Count,
    'MethodFunc5 should have 5 parameters');

  CheckEquals(0, FMockMethod.GetMethodInfo('MethodProc0').Parameters.Count,
    'MethodProc0 don''''t should be paramaters');

  CheckEquals(1, FMockMethod.GetMethodInfo('MethodProc1').Parameters.Count,
    'MethodProc1 should have 1 parameter');

  CheckEquals(2, FMockMethod.GetMethodInfo('MethodProc2').Parameters.Count,
    'MethodProc2 should have 2 parameters');

  CheckEquals(3, FMockMethod.GetMethodInfo('MethodProc3').Parameters.Count,
    'MethodProc3 should have 3 parameters');

  CheckEquals(4, FMockMethod.GetMethodInfo('MethodProc4').Parameters.Count,
    'MethodProc4 should have 4 parameters');

  CheckEquals(5, FMockMethod.GetMethodInfo('MethodProc5').Parameters.Count,
    'MethodProc5 should have 5 parameters');
end;

procedure TMethodInfoTests.TestGetCallingConvention;
begin
  CheckTrue(FMockMethod.GetMethodInfo('Constructor0').CallingConvention = ccRegister,
    'Constructor0 must have ccRegister conventionn');

  CheckTrue(FMockMethod.GetMethodInfo('MethodFunc0').CallingConvention = ccRegister,
    'MethodFunc0 must have ccRegister convention');

  CheckTrue(FMockMethod.GetMethodInfo('MethodProc0').CallingConvention = ccRegister,
    'MethodProc0 must have ccRegister convention');
end;

procedure TMethodInfoTests.TestGetIsConstructor;
begin
  CheckTrue(FMockMethod.GetMethodInfo('Constructor0').IsConstructor,
    'Constructor0 should be a constructor');

  CheckTrue(FMockMethod.GetMethodInfo('Constructor1').IsConstructor,
    'Constructor1 should be a constructor');

  CheckFalse(FMockMethod.GetMethodInfo('MethodFunc0').IsConstructor,
    'MethodFunc0 cannot be a constructor');

  CheckFalse(FMockMethod.GetMethodInfo('MethodProc0').IsConstructor,
    'MethodProc0 cannot be a constructor');
end;

procedure TMethodInfoTests.TestGetMethodPointer;
begin
  CheckMethodIsNotEmpty(FMockMethod.GetMethodInfo('Constructor0').MethodPointer);

  CheckMethodIsNotEmpty(FMockMethod.GetMethodInfo('MethodProc0').MethodPointer);

  CheckMethodIsNotEmpty(FMockMethod.GetMethodInfo('MethodFunc0').MethodPointer);
end;

procedure TMethodInfoTests.TestGetParameters;
begin
  CheckNotNull(FMockMethod.GetMethodInfo('MethodFunc0').Parameters,
    'Error on getting MethodFunc0 parameters');
  CheckNotNull(FMockMethod.GetMethodInfo('MethodFunc1').Parameters,
    'Error on getting MethodFunc1 parameters');
  CheckNotNull(FMockMethod.GetMethodInfo('MethodFunc2').Parameters,
    'Error on getting MethodFunc2 parameters');
  CheckNotNull(FMockMethod.GetMethodInfo('MethodFunc3').Parameters,
    'Error on getting MethodFunc3 parameters');
  CheckNotNull(FMockMethod.GetMethodInfo('MethodFunc4').Parameters,
    'Error on getting MethodFunc4 parameters');
  CheckNotNull(FMockMethod.GetMethodInfo('MethodFunc5').Parameters,
    'Error on getting MethodFunc5 parameters');
end;

procedure TMethodInfoTests.TestGetReturnType;
begin
  CheckTrue(FMockMethod.GetMethodInfo('MethodFunc0').ReturnType =
    TypeService.GetType(IInfraString),
    'MethodFunc0 must returns IInfraString');

  CheckTrue(FMockMethod.GetMethodInfo('MethodFunc1').ReturnType =
    TypeService.GetType(IInfraString),
    'MethodFunc1 must returns IInfraString');

  CheckTrue(FMockMethod.GetMethodInfo('MethodFunc2').ReturnType =
    TypeService.GetType(IInfraInteger),
    'MethodFunc2 must returns IInfraInteger');

  CheckTrue(FMockMethod.GetMethodInfo('MethodFunc3').ReturnType =
    TypeService.GetType(IInfraDateTime),
    'MethodFunc3 must returns IInfraDateTime');

  CheckTrue(FMockMethod.GetMethodInfo('MethodFunc4').ReturnType =
    TypeService.GetType(IInfraBoolean),
    'MethodFunc4 must returns IInfraBoolean');

  CheckTrue(FMockMethod.GetMethodInfo('MethodFunc5').ReturnType =
    TypeService.GetType(IInfraDouble),
    'MethodFunc5 must returns IInfraDouble');

  CheckTrue(FMockMethod.GetMethodInfo('Constructor0').ReturnType = nil,
    'Constructor0 does not have return type');

  CheckTrue(FMockMethod.GetMethodInfo('MethodProc0').ReturnType = nil,
    'MethodProc0 does not have return type');
end;

procedure TMethodInfoTests.TestInvoke_Procedures;
var
  mm: IMockMethod;
  fMethodInfo: IMethodInfo;
  fParams: IInterfaceList;
begin
  mm := TMockMethod.Create;
  fMethodInfo := TypeService.GetType(IMockMethod).GetMethodInfo('MethodProc0');
  fMethodInfo.Invoke(mm as IInfraInstance, nil);
  CheckEquals(cMessageProc0, mm.Message.AsString, 'Mismatch in MethodProc0');

  fMethodInfo := TypeService.GetType(IMockMethod).GetMethodInfo('MethodProc1');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageProc1,
    ['ValorP1']),
    mm.Message.AsString, 'Mismatch in MethodProc1');

  fMethodInfo := TypeService.GetType(IMockMethod).GetMethodInfo('MethodProc2');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fParams.Add(TInfraInteger.NewFrom(55));
  fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageProc2,
    ['ValorP1', 55]),
    mm.Message.AsString, 'Mismatch in MethodProc2');

  fMethodInfo := TypeService.GetType(IMockMethod).GetMethodInfo('MethodProc3');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fParams.Add(TInfraInteger.NewFrom(55));
  fParams.Add(TInfraDateTime.NewFrom(StrToDateTime('30/3/2007 20:15:00')));
  fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageProc3,
    ['ValorP1', 55, '30/3/2007 20:15:00']),
    mm.Message.AsString, 'Mismatch in MethodProc3');

  fMethodInfo := TypeService.GetType(IMockMethod).GetMethodInfo('MethodProc4');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fParams.Add(TInfraInteger.NewFrom(55));
  fParams.Add(TInfraDateTime.NewFrom(StrToDateTime('30/3/2007 20:15:00')));
  fParams.Add(TInfraBoolean.NewFrom(True));
  fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageProc4,
    ['ValorP1', 55, '30/3/2007 20:15:00', SysUtils.BoolToStr(True, False)]),
    mm.Message.AsString, 'Mismatch in MethodProc4');

  fMethodInfo := TypeService.GetType(IMockMethod).GetMethodInfo('MethodProc5');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fParams.Add(TInfraInteger.NewFrom(55));
  fParams.Add(TInfraDateTime.NewFrom(StrToDateTime('30/3/2007 20:15:00')));
  fParams.Add(TInfraBoolean.NewFrom(True));
  fParams.Add(TInfraDouble.NewFrom(125.5));
  fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageProc5,
    ['ValorP1', 55, '30/3/2007 20:15:00',
    SysUtils.BoolToStr(True, False), 125.5]),
    mm.Message.AsString, 'Mismatch in MethodProc5');
end;

procedure TMethodInfoTests.TestInvoke_Functions;
var
  mm: IMockMethod;
  fMethodInfo: IMethodInfo;
  fParams: IInterfaceList;
  fResult: IInterface;
begin
  mm := TMockMethod.Create;
  fMethodInfo := TypeService.GetType(IMockMethod).GetMethodInfo('MethodFunc0');
  CheckNotNull(fMethodInfo, 'MethodInfo MethodFunc0 Not found!');
  fResult := fMethodInfo.Invoke(mm as IInfraInstance, nil);
  CheckEquals(Format(cMessageFunc0,
    [(fResult as IInfraString).AsString]),
    mm.Message.AsString, 'Erro invoking MethodFunc0');

  fMethodInfo := TypeService.GetType(IMockMethod).GetMethodInfo('MethodFunc1');
  CheckNotNull(fMethodInfo, 'MethodInfo MethodFunc1 Not found!');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fResult := fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageFunc1,
    ['ValorP1', (fResult as IInfraString).AsString]),
    mm.Message.AsString, 'Erro invoking MethodFunc1');

  fMethodInfo := TypeService.GetType(IMockMethod).GetMethodInfo('MethodFunc2');
  CheckNotNull(fMethodInfo, 'MethodInfo MethodFunc2 Not found!');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fParams.Add(TInfraInteger.NewFrom(55));
  fResult := fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageFunc2,
    ['ValorP1', 55, (fResult as IInfraInteger).AsInteger]),
    mm.Message.AsString, 'Erron invoking MethodFunc2');

  fMethodInfo := TypeService.GetType(IMockMethod).GetMethodInfo('MethodFunc3');
  CheckNotNull(fMethodInfo, 'MethodInfo MethodFunc3 Not found!');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fParams.Add(TInfraInteger.NewFrom(55));
  fParams.Add(TInfraDateTime.NewFrom(StrToDateTime('30/3/2007 20:15:00')));
  fResult := fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageFunc3,
    ['ValorP1', 55, '30/3/2007 20:15:00',
    DateTimeToStr((fResult as IInfraDateTime).AsDateTime)]),
    mm.Message.AsString, 'Erro invoking MethodFunc3');

  fMethodInfo := TypeService.GetType(IMockMethod).GetMethodInfo('MethodFunc4');
  CheckNotNull(fMethodInfo, 'MethodInfo MethodFunc4 Not found!');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fParams.Add(TInfraInteger.NewFrom(55));
  fParams.Add(TInfraDateTime.NewFrom(StrToDateTime('30/3/2007 20:15:00')));
  fParams.Add(TInfraBoolean.NewFrom(True));
  fResult := fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageFunc4,
    ['ValorP1', 55, '30/3/2007 20:15:00',
    SysUtils.BoolToStr(True, True),
    SysUtils.BoolToStr((fResult as IInfraBoolean).AsBoolean, True)]),
    mm.Message.AsString, 'Mismatch in MethodFunc4');

  fMethodInfo := TypeService.GetType(IMockMethod).GetMethodInfo('MethodFunc5');
  CheckNotNull(fMethodInfo, 'MethodInfo MethodFunc5 Not found!');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fParams.Add(TInfraInteger.NewFrom(55));
  fParams.Add(TInfraDateTime.NewFrom(StrToDateTime('30/3/2007 20:15:00')));
  fParams.Add(TInfraBoolean.NewFrom(True));
  fParams.Add(TInfraDouble.NewFrom(125.5));
  fResult := fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageFunc5,
    ['ValorP1', 55, '30/3/2007 20:15:00',
    SysUtils.BoolToStr(True, True), 125.5,
    (fResult as IInfraDouble).AsDouble]),
    mm.Message.AsString, 'Mismatch in MethodFunc5');
end;

initialization
  TestFramework.RegisterTest('InfraReflectTests Suite', TMethodInfoTests.Suite);

end.
