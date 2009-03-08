unit MethodInfoTests;

interface

uses
  TestFrameWork,
  InfraCommonIntf,
  ReflectModelIntf;

type
  TMethodInfoTests = class(TTestCase)
  private
    FMyMethodsClass: IClassInfo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Test methods
    procedure TestAddParam;
    procedure TestGetCallingConvention;
    procedure TestGetIsConstructor;
    procedure TestGetIsFunction;
    procedure TestGetMethodPointer;
    procedure TestGetMethodName;
    procedure TestGetParameters;
    procedure TestGetReturnType;
    procedure TestInvoke_Procedures;
    procedure TestInvoke_Functions;
    procedure TestInvoke_Constructor;
    procedure TestInvoke_ConstructorWithParams;
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
  FMyMethodsClass := TSetupModel.RegisterMyMethodsClass;
end;

procedure TMethodInfoTests.TearDown;
begin
  TSetupModel.RemoveTypeInfo(IMyMethodsClass);
  inherited;
end;

procedure TMethodInfoTests.TestAddParam;
begin
  CheckEquals(0, FMyMethodsClass.GetMethodInfo('Constructor0').Parameters.Count,
    'Constructor0 don''''t should be paramaters');

  CheckEquals(1, FMyMethodsClass.GetMethodInfo('Constructor1').Parameters.Count,
    'Constructor1 should have 1 parameter');

  // *** Funões tem 1 parametro a mais que é o result. tem que ver como
  // *** ficará isso. Se vamos retornar apenas os verdadeiros parâmetros ou
  // *** deixamos como está.

  CheckEquals(1, FMyMethodsClass.GetMethodInfo('MethodFunc0').Parameters.Count,
    'MethodFunc0 don''''t should be paramaters');

  CheckEquals(2, FMyMethodsClass.GetMethodInfo('MethodFunc1').Parameters.Count,
    'MethodFunc1 should have 1 parameter');

  CheckEquals(3, FMyMethodsClass.GetMethodInfo('MethodFunc2').Parameters.Count,
    'MethodFunc2 should have 2 parameters');

  CheckEquals(4, FMyMethodsClass.GetMethodInfo('MethodFunc3').Parameters.Count,
    'MethodFunc3 should have 3 parameters');

  CheckEquals(5, FMyMethodsClass.GetMethodInfo('MethodFunc4').Parameters.Count,
    'MethodFunc4 should have 4 parameters');

  CheckEquals(6, FMyMethodsClass.GetMethodInfo('MethodFunc5').Parameters.Count,
    'MethodFunc5 should have 5 parameters');

  CheckEquals(0, FMyMethodsClass.GetMethodInfo('MethodProc0').Parameters.Count,
    'MethodProc0 don''''t should be paramaters');

  CheckEquals(1, FMyMethodsClass.GetMethodInfo('MethodProc1').Parameters.Count,
    'MethodProc1 should have 1 parameter');

  CheckEquals(2, FMyMethodsClass.GetMethodInfo('MethodProc2').Parameters.Count,
    'MethodProc2 should have 2 parameters');

  CheckEquals(3, FMyMethodsClass.GetMethodInfo('MethodProc3').Parameters.Count,
    'MethodProc3 should have 3 parameters');

  CheckEquals(4, FMyMethodsClass.GetMethodInfo('MethodProc4').Parameters.Count,
    'MethodProc4 should have 4 parameters');

  CheckEquals(5, FMyMethodsClass.GetMethodInfo('MethodProc5').Parameters.Count,
    'MethodProc5 should have 5 parameters');
end;

procedure TMethodInfoTests.TestGetCallingConvention;
begin
  CheckTrue(FMyMethodsClass.GetMethodInfo('Constructor0').CallingConvention = ccRegister,
    'Constructor0 must have ccRegister conventionn');

  CheckTrue(FMyMethodsClass.GetMethodInfo('MethodFunc0').CallingConvention = ccRegister,
    'MethodFunc0 must have ccRegister convention');

  CheckTrue(FMyMethodsClass.GetMethodInfo('MethodProc0').CallingConvention = ccRegister,
    'MethodProc0 must have ccRegister convention');
end;

procedure TMethodInfoTests.TestGetIsConstructor;
begin
  CheckTrue(FMyMethodsClass.GetMethodInfo('Constructor0').IsConstructor,
    'Constructor0 should be a constructor');

  CheckTrue(FMyMethodsClass.GetMethodInfo('Constructor1').IsConstructor,
    'Constructor1 should be a constructor');

  CheckFalse(FMyMethodsClass.GetMethodInfo('MethodFunc0').IsConstructor,
    'MethodFunc0 cannot be a constructor');

  CheckFalse(FMyMethodsClass.GetMethodInfo('MethodProc0').IsConstructor,
    'MethodProc0 cannot be a constructor');
end;

procedure TMethodInfoTests.TestGetIsFunction;
begin
  CheckTrue(FMyMethodsClass.GetMethodInfo('Constructor0').IsConstructor,
    'Constructor0 should be a function');

  CheckTrue(FMyMethodsClass.GetMethodInfo('MethodFunc0').IsFunction,
    'MethodFunc0 should be a function');

  CheckFalse(FMyMethodsClass.GetMethodInfo('MethodProc0').IsFunction,
    'MethodProc0 should not be a function');
end;

procedure TMethodInfoTests.TestGetMethodName;
begin
  CheckEquals('TMyMethodsClass.MethodProc2',
    FMyMethodsClass.GetMethodInfo('MethodProc2').FullName,
    'Full name mismatch');

  CheckEquals('TMyMethodsClass.MethodFunc0',
    FMyMethodsClass.GetMethodInfo('MethodFunc0').FullName,
    'Full name mismatch');

  CheckEquals('TMyMethodsClass.Constructor0',
    FMyMethodsClass.GetMethodInfo('Constructor0').FullName,
    'Full name mismatch');
end;

procedure TMethodInfoTests.TestGetMethodPointer;
begin
  CheckMethodIsNotEmpty(
    FMyMethodsClass.GetMethodInfo('Constructor0').MethodPointer);
  CheckMethodIsNotEmpty(
    FMyMethodsClass.GetMethodInfo('MethodProc0').MethodPointer);
  CheckMethodIsNotEmpty(
    FMyMethodsClass.GetMethodInfo('MethodFunc0').MethodPointer);
end;

procedure TMethodInfoTests.TestGetParameters;
begin
  CheckNotNull(FMyMethodsClass.GetMethodInfo('MethodFunc0').Parameters,
    'Error on getting MethodFunc0 parameters');
  CheckNotNull(FMyMethodsClass.GetMethodInfo('MethodFunc1').Parameters,
    'Error on getting MethodFunc1 parameters');
  CheckNotNull(FMyMethodsClass.GetMethodInfo('MethodFunc2').Parameters,
    'Error on getting MethodFunc2 parameters');
  CheckNotNull(FMyMethodsClass.GetMethodInfo('MethodFunc3').Parameters,
    'Error on getting MethodFunc3 parameters');
  CheckNotNull(FMyMethodsClass.GetMethodInfo('MethodFunc4').Parameters,
    'Error on getting MethodFunc4 parameters');
  CheckNotNull(FMyMethodsClass.GetMethodInfo('MethodFunc5').Parameters,
    'Error on getting MethodFunc5 parameters');
end;

procedure TMethodInfoTests.TestGetReturnType;
begin
  CheckTrue(FMyMethodsClass.GetMethodInfo('MethodFunc0').ReturnType =
    TypeService.GetType(IInfraString),
    'MethodFunc0 must returns IInfraString');

  CheckTrue(FMyMethodsClass.GetMethodInfo('MethodFunc1').ReturnType =
    TypeService.GetType(IInfraString),
    'MethodFunc1 must returns IInfraString');

  CheckTrue(FMyMethodsClass.GetMethodInfo('MethodFunc2').ReturnType =
    TypeService.GetType(IInfraInteger),
    'MethodFunc2 must returns IInfraInteger');

  CheckTrue(FMyMethodsClass.GetMethodInfo('MethodFunc3').ReturnType =
    TypeService.GetType(IInfraDateTime),
    'MethodFunc3 must returns IInfraDateTime');

  CheckTrue(FMyMethodsClass.GetMethodInfo('MethodFunc4').ReturnType =
    TypeService.GetType(IInfraBoolean),
    'MethodFunc4 must returns IInfraBoolean');

  CheckTrue(FMyMethodsClass.GetMethodInfo('MethodFunc5').ReturnType =
    TypeService.GetType(IInfraDouble),
    'MethodFunc5 must returns IInfraDouble');

  CheckTrue(FMyMethodsClass.GetMethodInfo('Constructor0').ReturnType = nil,
    'Constructor0 does not have return type');

  CheckTrue(FMyMethodsClass.GetMethodInfo('MethodProc0').ReturnType = nil,
    'MethodProc0 does not have return type');
end;

procedure TMethodInfoTests.TestInvoke_Procedures;
var
  mm: IMyMethodsClass;
  fClassInfo: IClassInfo;
  fMethodInfo: IMethodInfo;
  fParams: IInterfaceList;
begin
  fClassInfo := TypeService.GetType(IMyMethodsClass);

  mm := TMyMethodsClass.Create;
  fMethodInfo := fClassInfo.GetMethodInfo('MethodProc0');
  fMethodInfo.Invoke(mm as IInfraInstance, nil);
  CheckEquals(cMessageProc0, mm.Message.AsString, 'Mismatch in MethodProc0');

  fMethodInfo := fClassInfo.GetMethodInfo('MethodProc1');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageProc1,
    ['ValorP1']),
    mm.Message.AsString, 'Mismatch in MethodProc1');

  fMethodInfo := fClassInfo.GetMethodInfo('MethodProc2');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fParams.Add(TInfraInteger.NewFrom(55));
  fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageProc2,
    ['ValorP1', 55]),
    mm.Message.AsString, 'Mismatch in MethodProc2');

  fMethodInfo := fClassInfo.GetMethodInfo('MethodProc3');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fParams.Add(TInfraInteger.NewFrom(55));
  fParams.Add(TInfraDateTime.NewFrom(StrToDateTime('30/3/2007 20:15:00')));
  fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageProc3,
    ['ValorP1', 55, '30/3/2007 20:15:00']),
    mm.Message.AsString, 'Mismatch in MethodProc3');

  fMethodInfo := fClassInfo.GetMethodInfo('MethodProc4');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fParams.Add(TInfraInteger.NewFrom(55));
  fParams.Add(TInfraDateTime.NewFrom(StrToDateTime('30/3/2007 20:15:00')));
  fParams.Add(TInfraBoolean.NewFrom(True));
  fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageProc4,
    ['ValorP1', 55, '30/3/2007 20:15:00', SysUtils.BoolToStr(True, False)]),
    mm.Message.AsString, 'Mismatch in MethodProc4');

  fMethodInfo := fClassInfo.GetMethodInfo('MethodProc5');
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
  mm: IMyMethodsClass;
  fClassInfo: IClassInfo;
  fMethodInfo: IMethodInfo;
  fParams: IInterfaceList;
  fResult: IInterface;
begin
  fClassInfo := TypeService.GetType(IMyMethodsClass);

  mm := TMyMethodsClass.Create;
  fMethodInfo := fClassInfo.GetMethodInfo('MethodFunc0');
  CheckNotNull(fMethodInfo, 'MethodInfo MethodFunc0 Not found!');
  fResult := fMethodInfo.Invoke(mm as IInfraInstance, nil);
  CheckEquals(Format(cMessageFunc0,
    [(fResult as IInfraString).AsString]),
    mm.Message.AsString, 'Erro invoking MethodFunc0');

  fMethodInfo := fClassInfo.GetMethodInfo('MethodFunc1');
  CheckNotNull(fMethodInfo, 'MethodInfo MethodFunc1 Not found!');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fResult := fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageFunc1,
    ['ValorP1', (fResult as IInfraString).AsString]),
    mm.Message.AsString, 'Erro invoking MethodFunc1');

  fMethodInfo := fClassInfo.GetMethodInfo('MethodFunc2');
  CheckNotNull(fMethodInfo, 'MethodInfo MethodFunc2 Not found!');
  fParams := TInterfaceList.Create;
  fParams.Add(TInfraString.NewFrom('ValorP1'));
  fParams.Add(TInfraInteger.NewFrom(55));
  fResult := fMethodInfo.Invoke(mm as IInfraInstance, fParams);
  CheckEquals(Format(cMessageFunc2,
    ['ValorP1', 55, (fResult as IInfraInteger).AsInteger]),
    mm.Message.AsString, 'Erron invoking MethodFunc2');

  fMethodInfo := fClassInfo.GetMethodInfo('MethodFunc3');
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

  fMethodInfo := fClassInfo.GetMethodInfo('MethodFunc4');
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

  fMethodInfo := fClassInfo.GetMethodInfo('MethodFunc5');
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

procedure TMethodInfoTests.TestInvoke_Constructor;
//var
//  vPerson: IPerson;
//  vClassInfo: IClassInfo;
//  vMethodInfo: IConstructorInfo;
//  vParams: IInterfaceList;
//  vResult: IInterface;
begin
  // Criar método GetConstructor podendo passar os parametros para descobrir
  // o Create correto, caso seja sobrecarregado.
//  vParams := TInterfaceList.Create;
//  vParams.Add(TInfraString.NewFrom('Marcos Barreto'));
//  vParams.Add(TInfraInteger.NewFrom('mrbar2000@gmail.com'));
//  vConstructorInfo := TypeService.GetType(IPerson, True).GetMethodInfo(
//    'Create', vParams);
//  CheckNotNull(vConstructorInfo, 'vConstructorInfo Not found!');
//  IConstructorInfo.
//  vResult := vConstructorInfo.Invoke(mm as IInfraInstance, nil);
//  CheckEquals(Format(cMessageFunc0,
//    [(fResult as IInfraString).AsString]),
//    mm.Message.AsString, 'Erro invoking MethodFunc0');
end;

procedure TMethodInfoTests.TestInvoke_ConstructorWithParams;
begin

end;

initialization
  TestFramework.RegisterTest('InfraReflectTests Suite', TMethodInfoTests.Suite);

end.

