{
  Here we will test the routine that call Methods at runtime

  function InvokeMethod(pInstance: TObject; pMethod: Pointer;
    pParamCount: integer; pArrayParams: Pointer;
    pMethodResultKind: TMethodResultKind): IInfraType;

  To use this method we need pass:

  - pInstance: Object's Instance that have the method to be execute.
  - pMethod: Method of Object that will be executed.
  - pParamCount: Count of parameters of Method. If method is function then
    pParamCount should be increment by 1.
  - pArrayParams: Array of TParams. This array have all parameters that
    InvokeMethod will pass to Method. If Method is function then the last item
    of array is the variable that will store the result of function.
  - (*) pMethodResultKind: Type of Method's result. When this parameter is
    mrkNone - this method is a procedure (no parameter),
    mrkInteger - this method result integer,
    mrkInterface - this method result interface,
    mrkString - this method result string;

  (*) This param was a try to join the routines InvokeMethodPrimitive and
      InvokeMethod. For while, to InvokeMethod we use it just to know if
      method is function or no.

}
unit u_TestCasesMethodType;

interface

uses
  Classes, TestFramework, InfraInterfaces, AspectVMTUtil;

type
  TClassA = class(TObject)
  public
    List: TStringList;
    procedure ProcWithoutPar; virtual;
    procedure Proc1Par(const x: IInfraInteger); virtual;
    procedure Proc2Par(const x, y: IInfraInteger); virtual;
    procedure Proc3Par(const x, y, z: IInfraInteger); virtual;
    procedure Proc4Par(const x, y: IInfraString; const z, w: IInfraInteger); virtual;
    procedure Proc5Par(const x, y, z, w, k: IInfraInteger); virtual;
    function FuncWithoutPar: IInfraInteger; virtual;
    function Func1Par(const x: IInfraInteger): IInfraInteger; virtual;
    function Func2Par(const x, y: IInfraInteger): IInfraInteger; virtual;
    function Func3Par(const x, y, z: IInfraInteger): IInfraInteger; virtual;
    function Func4Par(const x, y: IInfraString; const z, w: IInfraInteger): IInfraString; virtual;
    function Func5Par(const x, y, z, w, k: IInfraInteger): IInfraInteger; virtual;
  end;

  TInvokeMethodType_Test = class(TTestCase)
  private
    FParams: TParams;
    ClasseA: TClassA;
    List: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcWithoutParameters;
    procedure TestProcWith1Parameter;
    procedure TestProcWith2Parameter;
    procedure TestProcWith3Parameter;
    procedure TestProcWith4Parameter;
    procedure TestProcWith5Parameter;
    procedure TestFuncWithoutParameters;
    procedure TestFuncWith1Parameter;
    procedure TestFuncWith2Parameter;
    procedure TestFuncWith3Parameter;
    procedure TestFuncWith4Parameter;
    procedure TestFuncWith5Parameter;
  end;

implementation

uses Types, SysUtils, SimpleTypeImpl;

const
  cMsgProcedureCalled = '%s.%s(%s) called';
  cMsgFunctionCalled = '%s.%s(%s):%s called';

{ TClassA }

function TClassA.Func1Par(const x: IInfraInteger): IInfraInteger;
begin
  Result := TInfraInteger.NewFrom(x.AsInteger * 100);
  List.Add('TClassA.Func1Par(' +
    IntToStr(x.AsInteger) + '): ' + IntToStr(Result.AsInteger));
end;

function TClassA.Func2Par(const x, y: IInfraInteger): IInfraInteger;
begin
  Result := TInfraInteger.NewFrom(x.AsInteger * y.AsInteger);
  List.Add('TClassA.Func2Par(' + IntToStr(x.AsInteger) + ',' +
    IntToStr(y.AsInteger) + '): ' + IntToStr(Result.AsInteger));
end;

function TClassA.Func3Par(const x, y, z: IInfraInteger): IInfraInteger;
begin
  Result := TInfraInteger.NewFrom(x.AsInteger * y.AsInteger *
    z.AsInteger);
  List.Add('TClassA.Func3Par('+
    IntToStr(x.AsInteger) + ',' + IntToStr(y.AsInteger) + ',' +
    IntToStr(z.AsInteger) + '): ' + IntToStr(Result.AsInteger));
end;

function TClassA.Func4Par(const x, y: IInfraString;
  const z, w: IInfraInteger): IInfraString;
begin
  Result := TInfraString.NewFrom(IntToStr(z.AsInteger * w.AsInteger));
  List.Add('TClassA.Func4Par('+
    x.AsString + ',' + y.AsString + ',' + IntToStr(z.AsInteger) + ',' +
    IntToStr(w.AsInteger) + '): ' + Result.AsString);
end;

function TClassA.Func5Par(const x, y, z, w,
  k: IInfraInteger): IInfraInteger;
begin
  Result := TInfraInteger.NewFrom(x.AsInteger * y.AsInteger *
    z.AsInteger * w.AsInteger * k.AsInteger);
  List.Add('TClassA.Func5Par('+
    IntToStr(x.AsInteger) + ',' + IntToStr(y.AsInteger) + ',' +
    IntToStr(z.AsInteger) + ',' + IntToStr(w.AsInteger) + ',' +
    IntToStr(k.AsInteger) + '): ' + IntToStr(Result.AsInteger));
end;

function TClassA.FuncWithoutPar: IInfraInteger;
begin
  Result := TInfraInteger.NewFrom(55);
  List.Add('TClassA.FuncWithoutPar: '+IntToStr(Result.AsInteger));
end;

procedure TClassA.Proc1Par(const x: IInfraInteger);
begin
  List.Add('TClassA.Proc1Par('+IntToStr(x.AsInteger)+')');
end;

procedure TClassA.Proc2Par(const x, y: IInfraInteger);
begin
  List.Add('TClassA.Proc2Par('+
    IntToStr(x.AsInteger)+','+
    IntToStr(y.AsInteger)+')');
end;

procedure TClassA.Proc3Par(const x, y, z: IInfraInteger);
begin
  List.Add('TClassA.Proc3Par('+
    IntToStr(x.AsInteger)+','+
    IntToStr(y.AsInteger)+','+
    IntToStr(z.AsInteger)+')');
end;

procedure TClassA.Proc4Par(const x, y: IInfraString; const z, w: IInfraInteger);
begin
  List.Add('TClassA.Proc4Par('+
    x.AsString+','+
    y.AsString+','+
    IntToStr(z.AsInteger)+','+
    IntToStr(w.AsInteger)+')');
end;

procedure TClassA.Proc5Par(const x, y, z, w, k: IInfraInteger);
begin
  List.Add('TClassA.Proc5Par('+
    IntToStr(x.AsInteger)+','+
    IntToStr(y.AsInteger)+','+
    IntToStr(z.AsInteger)+','+
    IntToStr(w.AsInteger)+','+
    IntToStr(k.AsInteger)+')');
end;

procedure TClassA.ProcWithoutPar;
begin
  List.Add('TClassA.ProcWithoutPar');
end;

{ TInvokeMethodType_Test }

procedure TInvokeMethodType_Test.SetUp;
begin
  inherited;
  List := TstringList.Create;
  ClasseA := TClassA.Create;
  ClasseA.List := List;
end;

procedure TInvokeMethodType_Test.TearDown;
begin
  ClasseA.Free;
  List.Free;
  inherited;
end;

procedure TInvokeMethodType_Test.TestFuncWith1Parameter;
var
  iRes: IInfraType;
  p1: IInfraInteger;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := TInfraInteger.NewFrom(25);
  FParams[0] := Dword(p1);
  FParams[1] := Dword(iRes);
  iRes := InvokeMethod(ClasseA, @TClassA.Func1Par, 2, @FParams,
    mrkInterface);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Func1Par(25): 2500');
  CheckNotNull(iRes, 'Ops, Func1Par returning nil!');
  CheckEquals(2500, (iRes as IInfraInteger).AsInteger);
end;

procedure TInvokeMethodType_Test.TestFuncWith2Parameter;
var
  iRes: IInfraType;
  p1, p2: IInfraInteger;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := TInfraInteger.NewFrom(150);
  p2 := TInfraInteger.NewFrom(20);
  FParams[0] := Dword(p1);
  FParams[1] := Dword(p2);
  FParams[2] := Dword(iRes);
  iRes := InvokeMethod(ClasseA, @TClassA.Func2Par, 3, @FParams,
    mrkInterface);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Func2Par(150,20): 3000');
  CheckNotNull(iRes, 'Ops, Func2Par returning nil!');
  CheckEquals(3000, (iRes as IInfraInteger).AsInteger);
end;

procedure TInvokeMethodType_Test.TestFuncWith3Parameter;
var
  iRes: IInfraType;
  p1, p2, p3: IInfraInteger;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := TInfraInteger.NewFrom(80);
  p2 := TInfraInteger.NewFrom(30);
  p3 := TInfraInteger.NewFrom(10);
  FParams[0] := Dword(p1);
  FParams[1] := Dword(p2);
  FParams[2] := Dword(p3);
  FParams[3] := Dword(iRes);
  iRes := InvokeMethod(ClasseA, @TClassA.Func3Par, 4, @FParams,
    mrkInterface);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Func3Par(80,30,10): 24000');
  CheckNotNull(iRes, 'Ops, Func3Par returning nil!');
  CheckEquals(24000, (iRes as IInfraInteger).AsInteger);
end;

procedure TInvokeMethodType_Test.TestFuncWith4Parameter;
var
  iRes: IInfraType;
  p1, p2: IInfraString;
  p3, p4: IInfraInteger;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := TInfraString.NewFrom('Test');
  p2 := TInfraString.NewFrom('Params');
  p3 := TInfraInteger.NewFrom(2);
  p4 := TInfraInteger.NewFrom(25);
  FParams[0] := Dword(p1);
  FParams[1] := Dword(p2);
  FParams[2] := Dword(p3);
  FParams[3] := Dword(p4);
  FParams[4] := Dword(iRes);
  iRes := InvokeMethod(ClasseA, @TClassA.Func4Par, 5, @FParams,
    mrkInterface);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Func4Par(Test,Params,2,25): 50');
  CheckNotNull(iRes, 'Ops, Func4Par returning nil!');
  CheckEquals('50', (iRes as IInfraString).AsString);
end;

procedure TInvokeMethodType_Test.TestFuncWith5Parameter;
var
  iRes: IInfraType;
  p1, p2, p3, p4, p5: IInfraInteger;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := TInfraInteger.NewFrom(1);
  p2 := TInfraInteger.NewFrom(2);
  p3 := TInfraInteger.NewFrom(3);
  p4 := TInfraInteger.NewFrom(4);
  p5 := TInfraInteger.NewFrom(5);
  FParams[0] := Dword(p1);
  FParams[1] := Dword(p2);
  FParams[2] := Dword(p3);
  FParams[3] := Dword(p4);
  FParams[4] := Dword(p5);
  FParams[5] := Dword(iRes);
  iRes := InvokeMethod(ClasseA, @TClassA.Func5Par, 6, @FParams,
    mrkInterface);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Func5Par(1,2,3,4,5): 120');
  CheckNotNull(iRes, 'Ops, Func5Par returning nil!');
  CheckEquals(120, (iRes as IInfraInteger).AsInteger);
end;

procedure TInvokeMethodType_Test.TestFuncWithoutParameters;
var
  iRes: IInfraType;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  FParams[0] := Dword(iRes);
  iRes := InvokeMethod(ClasseA, @TClassA.FuncWithoutPar, 1, @FParams,
    mrkInterface);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.FuncWithoutPar: 55');
  CheckNotNull(iRes, 'Ops, FuncWithoutPar returning nil!');
  CheckEquals(55, (iRes as IInfraInteger).AsInteger);
end;

procedure TInvokeMethodType_Test.TestProcWith1Parameter;
var
  iRes: IInfraType;
  p1: IInfraInteger;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := TInfraInteger.NewFrom(150);
  FParams[0] := Dword(p1);
  iRes := InvokeMethod(ClasseA, @TClassA.Proc1Par, 1, @FParams, mrkNone);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Proc1Par(150)');
  CheckNull(iRes, 'Ops, Procedure Proc1Par returning something!');
end;

procedure TInvokeMethodType_Test.TestProcWith2Parameter;
var
  iRes: IInfraType;
  p1, p2: IInfraInteger;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := TInfraInteger.NewFrom(150);
  p2 := TInfraInteger.NewFrom(20);
  FParams[0] := Dword(p1);
  FParams[1] := Dword(p2);
  iRes := InvokeMethod(ClasseA, @TClassA.Proc2Par, 2, @FParams, mrkNone);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Proc2Par(150,20)');
  CheckNull(iRes, 'Ops, Procedure Proc2Par returning something!');
end;

procedure TInvokeMethodType_Test.TestProcWith3Parameter;
var
  iRes: IInfraType;
  p1, p2, p3: IInfraInteger;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := TInfraInteger.NewFrom(80);
  p2 := TInfraInteger.NewFrom(30);
  p3 := TInfraInteger.NewFrom(10);
  FParams[0] := Dword(p1);
  FParams[1] := Dword(p2);
  FParams[2] := Dword(p3);
  iRes := InvokeMethod(ClasseA, @TClassA.Proc3Par, 3, @FParams, mrkNone);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Proc3Par(80,30,10)');
  CheckNull(iRes, 'Ops, Procedure Proc3Par returning something!');
end;

procedure TInvokeMethodType_Test.TestProcWith4Parameter;
var
  iRes: IInfraType;
  p1, p2: IInfraString;
  p3, p4: IInfraInteger;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := TInfraString.NewFrom('Test');
  p2 := TInfraString.NewFrom('Params');
  p3 := TInfraInteger.NewFrom(2);
  p4 := TInfraInteger.NewFrom(25);
  FParams[0] := Dword(p1);
  FParams[1] := Dword(p2);
  FParams[2] := Dword(p3);
  FParams[3] := Dword(p4);
  iRes := InvokeMethod(ClasseA, @TClassA.Proc4Par, 4, @FParams, mrkNone);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Proc4Par(Test,Params,2,25)');
  CheckNull(iRes, 'Ops, Procedure Proc4Par returning something!');
end;

procedure TInvokeMethodType_Test.TestProcWith5Parameter;
var
  iRes: IInfraType;
  p1, p2, p3, p4, p5: IInfraInteger;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := TInfraInteger.NewFrom(21);
  p2 := TInfraInteger.NewFrom(22);
  p3 := TInfraInteger.NewFrom(23);
  p4 := TInfraInteger.NewFrom(24);
  p5 := TInfraInteger.NewFrom(25);
  FParams[0] := Dword(p1);
  FParams[1] := Dword(p2);
  FParams[2] := Dword(p3);
  FParams[3] := Dword(p4);
  FParams[4] := Dword(p5);
  iRes := InvokeMethod(ClasseA, @TClassA.Proc5Par, 5, @FParams, mrkNone);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Proc5Par(21,22,23,24,25)');
  CheckNull(iRes, 'Ops, Procedure Proc5Par returning something!');
end;

procedure TInvokeMethodType_Test.TestProcWithoutParameters;
var
  iRes: IInfraType;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  iRes := InvokeMethod(ClasseA, @TClassA.ProcWithoutPar, 0, @FParams, mrkNone);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.ProcWithoutPar');
  CheckNull(iRes, 'Ops, Procedure ProcWithoutPar returning something!');
end;

initialization
  TestFramework.RegisterTest('TInvokeMethodsType_Tests Suite',
    TInvokeMethodType_Test.Suite);

end.
