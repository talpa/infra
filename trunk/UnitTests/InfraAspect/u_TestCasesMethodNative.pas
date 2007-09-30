{
  Here we will test the routine that call Methods at runtime

  function InvokeMethod_P(pInstance: TObject; pMethod: Pointer;
    pParamCount: integer; pArrayParams: Pointer;
    pMethodResultKind: TMethodResultKind): dword;

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
unit u_TestCasesMethodNative;

interface

uses
  Classes, TestFramework, InfraInterfaces, AspectVMTUtil;

type
  IVariableInterface = interface(IInterface)
    ['{44BE7F15-C480-4838-AFE6-25DE79A6F5A8}']
    function Value: string;
  end;

  TClassA = class(TObject)
  public
    List: TStringList;
    procedure ProcWithoutPar; virtual;
    procedure Proc1Par(const x: Integer); virtual;
    procedure Proc2Par(const x, y: Integer); virtual;
    procedure Proc3Par(const x, y, z: Integer); virtual;
    procedure Proc4Par(const x, y: String; z, w: Integer); virtual;
    procedure Proc5Par(x: Pointer; y: string; z: TDateTime;
      w: Integer; const k: IInterface); virtual;
    function FuncWithoutPar: Integer; virtual;
    function Func1Par(const x: Integer): Integer; virtual;
    function Func2Par(const x, y: Integer): Integer; virtual;
    function Func3Par(const x, y, z: Integer): Integer; virtual;
    function Func4Par(const x, y: String; z, w: Integer): String; virtual;
    function Func5Par(const x, y, z, w, k: Integer): Integer; virtual;
  end;

  TClassB = class(TInterfacedObject, IVariableInterface)
  private
    function Value: string;
  end;

  TInvokeMethodNative_Test = class(TTestCase)
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
    {
    procedure TestFuncWithoutParameters;
    procedure TestFuncWith1Parameter;
    procedure TestFuncWith2Parameter;
    procedure TestFuncWith3Parameter;
    procedure TestFuncWith4Parameter;
    procedure TestFuncWith5Parameter;
    }
  end;

implementation

uses Types, SysUtils, SimpleTypeImpl;

const
  cMsgProcedureCalled = '%s.%s(%s) called';
  cMsgFunctionCalled = '%s.%s(%s):%s called';

{ TClassA }

function TClassA.Func1Par(const x: Integer): Integer;
begin
  Result := x * 100;
  List.Add('TClassA.Func1Par(' +
    IntToStr(x) + '): ' + IntToStr(Result));
end;

function TClassA.Func2Par(const x, y: Integer): Integer;
begin
  Result := x * y;
  List.Add('TClassA.Func2Par(' + IntToStr(x) + ',' +
    IntToStr(y) + '): ' + IntToStr(Result));
end;

function TClassA.Func3Par(const x, y, z: Integer): Integer;
begin
  Result := x * y * z;
  List.Add('TClassA.Func3Par('+
    IntToStr(x) + ',' + IntToStr(y) + ',' +
    IntToStr(z) + '): ' + IntToStr(Result));
end;

function TClassA.Func4Par(const x, y: String; z,
  w: Integer): String;
begin
  Result := IntToStr(z * w);
  List.Add('TClassA.Func4Par('+
    x + ',' + y + ',' + IntToStr(z) + ',' +
    IntToStr(w) + '): ' + Result);
end;

function TClassA.Func5Par(const x, y, z, w,
  k: Integer): Integer;
begin
  Result := x * y * z * w * k;
  List.Add('TClassA.Func5Par('+
    IntToStr(x) + ',' + IntToStr(y) + ',' +
    IntToStr(z) + ',' + IntToStr(w) + ',' +
    IntToStr(k) + '): ' + IntToStr(Result));
end;

function TClassA.FuncWithoutPar: Integer;
begin
  Result := 55;
  List.Add('TClassA.FuncWithoutPar: '+IntToStr(Result));
end;

procedure TClassA.Proc1Par(const x: Integer);
begin
  List.Add('TClassA.Proc1Par('+IntToStr(x)+')');
end;

procedure TClassA.Proc2Par(const x, y: Integer);
begin
  List.Add('TClassA.Proc2Par('+
    IntToStr(x)+','+
    IntToStr(y)+')');
end;

procedure TClassA.Proc3Par(const x, y, z: Integer);
begin
  List.Add('TClassA.Proc3Par('+
    IntToStr(x)+','+
    IntToStr(y)+','+
    IntToStr(z)+')');
end;

procedure TClassA.Proc4Par(const x, y: String; z, w: Integer);
begin
  List.Add('TClassA.Proc4Par('+
    x + ',' + y + ',' + IntToStr(z) + ',' + IntToStr(w) + ')');
end;

procedure TClassA.Proc5Par(x: Pointer; y: string; z: TDateTime;
  w: Integer; const k: IInterface);
var
  x1: ^Integer;
begin
  x1 := x;
  List.Add('TClassA.Proc5Par('+ IntToStr(x1^) + ',' + y + ',' +
    DateTimeToStr(z) + ',' + IntToStr(w) + ',' +
    (k as IVariableInterface).Value + ')');
end;

procedure TClassA.ProcWithoutPar;
begin
  List.Add('TClassA.ProcWithoutPar');
end;

{ TInvokeMethodNative_Test }

procedure TInvokeMethodNative_Test.SetUp;
begin
  inherited;
  List := TstringList.Create;
  ClasseA := TClassA.Create;
  ClasseA.List := List;
end;

procedure TInvokeMethodNative_Test.TearDown;
begin
  ClasseA.Free;
  List.Free;
  inherited;
end;

{ TClassB }

function TClassB.Value: string;
begin
  Result := 'IVariableResult';
end;

{
procedure TInvokeMethodNative_Test.TestFuncWith1Parameter;
var
  iRes: dword;
  p1: Integer;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := 25);
  FParams[0] := Dword(p1);
  FParams[1] := Dword(iRes);
  iRes := InvokeMethod(ClasseA, @TClassA.Func1Par, 2, @FParams,
    mrkInterface);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Func1Par(25): 2500');
  CheckNotNull(iRes, 'Ops, Func1Par returning nil!');
  CheckEquals(2500, (iRes as Integer));
end;

procedure TInvokeMethodNative_Test.TestFuncWith2Parameter;
var
  iRes: dword;
  p1, p2: Integer;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := 150);
  p2 := 20);
  FParams[0] := Dword(p1);
  FParams[1] := Dword(p2);
  FParams[2] := Dword(iRes);
  iRes := InvokeMethod(ClasseA, @TClassA.Func2Par, 3, @FParams,
    mrkInterface);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Func2Par(150,20): 3000');
  CheckNotNull(iRes, 'Ops, Func2Par returning nil!');
  CheckEquals(3000, (iRes as Integer));
end;

procedure TInvokeMethodNative_Test.TestFuncWith3Parameter;
var
  iRes: dword;
  p1, p2, p3: Integer;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := 80);
  p2 := 30);
  p3 := 10);
  FParams[0] := Dword(p1);
  FParams[1] := Dword(p2);
  FParams[2] := Dword(p3);
  FParams[3] := Dword(iRes);
  iRes := InvokeMethod(ClasseA, @TClassA.Func3Par, 4, @FParams,
    mrkInterface);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Func3Par(80,30,10): 24000');
  CheckNotNull(iRes, 'Ops, Func3Par returning nil!');
  CheckEquals(24000, (iRes as Integer));
end;

procedure TInvokeMethodNative_Test.TestFuncWith4Parameter;
var
  iRes: dword;
  p1, p2: String;
  p3, p4: Integer;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := TInfraString.NewFrom('Test');
  p2 := TInfraString.NewFrom('Params');
  p3 := 2);
  p4 := 25);
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
  CheckEquals('50', (iRes as String));
end;

procedure TInvokeMethodNative_Test.TestFuncWith5Parameter;
var
  iRes: dword;
  p1, p2, p3, p4, p5: Integer;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := 1);
  p2 := 2);
  p3 := 3);
  p4 := 4);
  p5 := 5);
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
  CheckEquals(120, (iRes as Integer));
end;

procedure TInvokeMethodNative_Test.TestFuncWithoutParameters;
var
  iRes: dword;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  FParams[0] := Dword(iRes);
  iRes := InvokeMethod(ClasseA, @TClassA.FuncWithoutPar, 1, @FParams,
    mrkInterface);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.FuncWithoutPar: 55');
  CheckNotNull(iRes, 'Ops, FuncWithoutPar returning nil!');
  CheckEquals(55, (iRes as Integer));
end;
}

procedure TInvokeMethodNative_Test.TestProcWith1Parameter;
var
  p1: Integer;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := 150;
  FParams[0] := Dword(p1);
  InvokeMethod_P(ClasseA, @TClassA.Proc1Par, 1, @FParams, mrkNone);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Proc1Par(150)');
end;

procedure TInvokeMethodNative_Test.TestProcWith2Parameter;
var
  p1, p2: Integer;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := 150;
  p2 := 20;
  FParams[0] := Dword(p1);
  FParams[1] := Dword(p2);
  InvokeMethod_P(ClasseA, @TClassA.Proc2Par, 2, @FParams, mrkNone);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Proc2Par(150,20)');
end;

procedure TInvokeMethodNative_Test.TestProcWith3Parameter;
var
  p1, p2, p3: Integer;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := 80;
  p2 := 30;
  p3 := 10;
  FParams[0] := Dword(p1);
  FParams[1] := Dword(p2);
  FParams[2] := Dword(p3);
  InvokeMethod_P(ClasseA, @TClassA.Proc3Par, 3, @FParams, mrkNone);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Proc3Par(80,30,10)');
end;

procedure TInvokeMethodNative_Test.TestProcWith4Parameter;
var
  p1, p2: String;
  p3, p4: Integer;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  p1 := 'Test';
  p2 := 'Params';
  p3 := 2;
  p4 := 25;
  FParams[0] := Dword(p1);
  FParams[1] := Dword(p2);
  FParams[2] := Dword(p3);
  FParams[3] := Dword(p4);
  InvokeMethod_P(ClasseA, @TClassA.Proc4Par, 4, @FParams, mrkNone);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.Proc4Par(Test,Params,2,25)');
end;

procedure TInvokeMethodNative_Test.TestProcWith5Parameter;
var
  i: integer;
  p1: pointer;
  p2: string;
  p3: double; // TDateTime;
  p4: integer;
  p5: IVariableInterface;
begin
//  CheckEquals(0, List.Count);
//  FillChar(FParams, SizeOf(TParams), 0);
//  i := 21;
//  p1 := @i;
//  p2 := 'Test';
//  p3 := Now;
//  p4 := 22;
//  p5 := TClassB.Create;;
//  FParams[0] := Dword(p1);
//  FParams[1] := Dword(p2);
//  FParams[2] := Dword(p3);
//  FParams[3] := Dword(p4);
//  FParams[4] := Dword(p5);
//  InvokeMethod_P(ClasseA, @TClassA.Proc5Par, 5, @FParams, mrkNone);
//  CheckEquals(1, List.Count);
//  CheckEquals(List[0], 'TClassA.Proc5Par(21,22,23,24,25)');
end;

procedure TInvokeMethodNative_Test.TestProcWithoutParameters;
begin
  CheckEquals(0, List.Count);
  FillChar(FParams, SizeOf(TParams), 0);
  InvokeMethod_P(ClasseA, @TClassA.ProcWithoutPar, 0, @FParams, mrkNone);
  CheckEquals(1, List.Count);
  CheckEquals(List[0], 'TClassA.ProcWithoutPar');
end;

initialization
  TestFramework.RegisterTest('TInvokeMethodsNative_Tests Suite',
    TInvokeMethodNative_Test.Suite);

end.
