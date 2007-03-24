// FixUses
unit u_TestCasesPrimitives;

interface

uses
  TestFramework, u_ClassesProcPrimitive;

type
  TAspectPrimitiveProcedure_Test = class(TTestCase)
  private
    ClasseC: IClassC;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMethodNotIntercepted;
    procedure TestProcWithoutParameters;
    procedure TestProcWith1Parameter;
    procedure TestProcWith2Parameter;
    procedure TestProcWith3Parameter;
    procedure TestProcWith4Parameter;
    procedure TestProcWith5Parameter;
  end;

  {
  TAspectTypeFunction_Test = class(TTestCase)
  private
    ClasseB: IClassB;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFuncWithoutParameters;
    procedure TestFuncWith1Parameter;
    procedure TestFuncWith2Parameter;
    procedure TestFuncWith3Parameter;
    procedure TestFuncWith4Parameter;
    procedure TestFuncWith5Parameter;
    procedure TestFuncWithoutProceed;
    procedure TestFuncWithProceed;
  end;
  }
implementation

uses
  SysUtils, u_GlobalTestList;

procedure TAspectPrimitiveProcedure_Test.SetUp;
begin
  inherited;
  ClasseC := TClassC.Create;
  GlobalTestList.Clear;
end;

procedure TAspectPrimitiveProcedure_Test.TearDown;
begin
  GlobalTestList.Clear;
  inherited;
end;

procedure TAspectPrimitiveProcedure_Test.TestMethodNotIntercepted;
begin
  ClasseC.NotIntercepted;
  CheckEquals(1, GlobalTestList.Count, 'NotIntercepted not was called');
  CheckEquals('TClassC.NotIntercepted() called', GlobalTestList.Strings[0]);
end;

procedure TAspectPrimitiveProcedure_Test.TestProcWithoutParameters;
begin
  CheckEquals(0, GlobalTestList.Count);
  ClasseC.ProcSemPar;
  Check(Self.Name = 'TestProcWithoutParameters',
    'Self was changed after method intercepted');
  CheckEquals(3, GlobalTestList.Count);
  CheckEquals('TPrimitiveAspect1.Around0(TClassC, []): Before Proceed called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect1.Around0']));
  CheckEquals('TClassC.ProcSemPar() called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassC.ProcSemPar']));
  CheckEquals('TPrimitiveAspect1.Around0(TClassC, []): After Proceed called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect1.Around0']));
end;

procedure TAspectPrimitiveProcedure_Test.TestProcWith1Parameter;
var
  x: Integer;
begin
  x := 5;
  CheckEquals(0, GlobalTestList.Count);
  ClasseC.Proc1Par(x);
  Check(Self.Name = 'TestProcWith1Parameter',
    'Self was changed after method intercepted');
  CheckEquals(6, GlobalTestList.Count);
  CheckEquals('TPrimitiveAspect1.Around1(TClassC, [5]): Before Proceed called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect1.Around1']));
  CheckEquals('TPrimitiveAspect2.Around1(TClassC, [5]): Before Proceed called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect2.Around1']));
  CheckEquals('TClassC.Proc1Par(5) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TClassC.Proc1Par']));
  CheckEquals('TPrimitiveAspect2.After1(TClassC, [5]) called',
    GlobalTestList.Strings[3], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect2.After1']));
  CheckEquals('TPrimitiveAspect2.Around1(TClassC, [5]): After Proceed called',
    GlobalTestList.Strings[4], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect2.Around1']));
  CheckEquals('TPrimitiveAspect1.Around1(TClassC, [5]): After Proceed called',
    GlobalTestList.Strings[5], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect1.Around1']));
end;

procedure TAspectPrimitiveProcedure_Test.TestProcWith2Parameter;
var
  x, y: Integer;
begin
  x := 5;
  y := 3;
  CheckEquals(0, GlobalTestList.Count);
  ClasseC.Proc2Par(x, y);
  Check(Self.Name = 'TestProcWith2Parameter',
    'Self was changed after method intercepted');
  CheckEquals(6, GlobalTestList.Count);
  CheckEquals('TPrimitiveAspect1.Around2(TClassC, [5, 3]): Before Proceed called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect1.Around2']));
  CheckEquals('TPrimitiveAspect1.Before2(TClassC, [5, 3]) called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect1.Before']));
  CheckEquals('TPrimitiveAspect2.Before2(TClassC, [5, 3]) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect2.Before']));
  CheckEquals('TClassC.Proc2Par(5, 3) called',
    GlobalTestList.Strings[3], Format(cMsgAdviceOutOfOrder,
      ['TClassC.Proc2Par']));
  CheckEquals('TPrimitiveAspect1.After2(TClassC, [5, 3]) called',
    GlobalTestList.Strings[4], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect1.After2']));
  CheckEquals('TPrimitiveAspect1.Around2(TClassC, [5, 3]): After Proceed called',
    GlobalTestList.Strings[5], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect1.Around2']));
end;

procedure TAspectPrimitiveProcedure_Test.TestProcWith3Parameter;
var
  x, y, z: Integer;
begin
  x := 5;
  y := 3;
  z := 4;
  CheckEquals(0, GlobalTestList.Count);
  ClasseC.Proc3Par(x, y, z);
  Check(Self.Name = 'TestProcWith3Parameter',
    'Self was changed after method intercepted');
  CheckEquals(3, GlobalTestList.Count);
  CheckEquals('TPrimitiveAspect1.Around3(TClassC, [5, 3, 4]): Before Proceed called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect1.Around3']));
  CheckEquals('TClassC.Proc3Par(5, 3, 4) called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassC.Proc3Par']));
  CheckEquals('TPrimitiveAspect1.Around3(TClassC, [5, 3, 4]): After Proceed called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect1.Around3']));
end;

procedure TAspectPrimitiveProcedure_Test.TestProcWith4Parameter;
var
  x, y, z, w: Integer;
begin
  x := 5;
  y := 3;
  z := 4;
  w := 8;
  CheckEquals(0, GlobalTestList.Count);
  ClasseC.Proc4Par(x, y, z, w);
  Check(Self.Name = 'TestProcWith4Parameter',
    'Self was changed after method intercepted');
  CheckEquals(3, GlobalTestList.Count);
  CheckEquals('TPrimitiveAspect2.Before4(TClassC, [5, 3, 4, 8]) called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect2.Before4']));
  CheckEquals('TClassC.Proc4Par(5, 3, 4, 8) called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassC.Proc4Par']));
  CheckEquals('TPrimitiveAspect2.After4(TClassC, [5, 3, 4, 8]) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect2.After4']));
end;

procedure TAspectPrimitiveProcedure_Test.TestProcWith5Parameter;
var
  x, y, z, w, k: Integer;
begin
  x := 5;
  y := 3;
  z := 4;
  w := 8;
  k := 7;
  CheckEquals(0, GlobalTestList.Count);
  ClasseC.Proc5Par(x, y, z, w, k);
  Check(Self.Name = 'TestProcWith5Parameter',
    'Self was changed after method intercepted');
  CheckEquals(5, GlobalTestList.Count);
  CheckEquals('TPrimitiveAspect1.Around5(TClassC, [5, 3, 4, 8, 7]): Before Proceed called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect1.Around5']));
  CheckEquals('TPrimitiveAspect2.Around5(TClassC, [5, 3, 4, 8, 7]): Before Proceed called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect2.Around5']));
  CheckEquals('TClassC.Proc5Par(5, 3, 4, 8, 7) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TClassC.Proc5Par']));
  CheckEquals('TPrimitiveAspect2.Around5(TClassC, [5, 3, 4, 8, 7]): After Proceed called',
    GlobalTestList.Strings[3], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect2.Around5']));
  CheckEquals('TPrimitiveAspect1.Around5(TClassC, [5, 3, 4, 8, 7]): After Proceed called',
    GlobalTestList.Strings[4], Format(cMsgAdviceOutOfOrder,
      ['TPrimitiveAspect1.Around5']));
end;

(*
{ TAspectTypeFunction_Test }

procedure TAspectTypeFunction_Test.SetUp;
begin
  inherited;
  ClasseB := TClassB.Create;
  GlobalTestList.Clear;
end;

procedure TAspectTypeFunction_Test.TearDown;
begin
  GlobalTestList.Clear;
  inherited;
end;

procedure TAspectTypeFunction_Test.TestFuncWithoutParameters;
var
  r: Integer;
begin
  CheckEquals(0, GlobalTestList.Count);
  r := ClasseB.FuncSemPar;
  CheckEquals(55, r.AsInteger);
  Check(Self.Name = 'TestFuncWithoutParameters',
    'Self was changed after method intercepted');
  CheckEquals(3, GlobalTestList.Count);
  CheckEquals('TAspect1.Before(TClassB, []) called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TAspect1.Before']));
  CheckEquals('TClassB.FuncSemPar():55 called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassB.FuncSemPar']));
  CheckEquals('TAspect1.After(TClassB, []) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TAspect1.After']));
end;

procedure TAspectTypeFunction_Test.TestFuncWithoutProceed;
var
  r: Integer;
begin
  CheckEquals(0, GlobalTestList.Count);
  r := ClasseB.FuncSemParWithoutProceed;
  Check(Self.Name = 'TestFuncWithoutProceed',
    'Self was changed after method intercepted');
  CheckEquals(1000, R.AsInteger);
  CheckEquals(1, GlobalTestList.Count);
  CheckEquals('TAspect3.Around(TClassB, []):1000 called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TAspect3.Around']));
end;

procedure TAspectTypeFunction_Test.TestFuncWithProceed;
var
  r: Integer;
begin
  CheckEquals(0, GlobalTestList.Count);
  r := ClasseB.FuncSemParWithProceed;
  Check(Self.Name = 'TestFuncWithProceed',
    'Self was changed after method intercepted');
  CheckEquals(5030, R.AsInteger);
  CheckEquals(4, GlobalTestList.Count);
  CheckEquals('TAspect4.Around(TClassB, []): Before Proceed called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TAspect3.Around']));
  CheckEquals('TClassB.FuncSemParWithProceed():5000 called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassB.FuncSemParWithProceed']));
  CheckEquals('TAspect4.After(TClassB, []) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TAspect4.After']));
  CheckEquals('TAspect4.Around(TClassB, []):5030 After Proceed called',
    GlobalTestList.Strings[3], Format(cMsgAdviceOutOfOrder,
      ['TAspect4.Around']));
end;

procedure TAspectTypeFunction_Test.TestFuncWith1Parameter;
var
  r, x: Integer;
begin
  x := 5;
  CheckEquals(0, GlobalTestList.Count);
  r := ClasseB.Func1Par(x);
  CheckEquals(625, r.AsInteger);
  Check(Self.Name = 'TestFuncWith1Parameter',
    'Self was changed after method intercepted');
  CheckEquals(3, GlobalTestList.Count);
  CheckEquals('TAspect2.Before(TClassB, [5]) called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TAspect2.Before']));
  CheckEquals('TClassB.Func1Par(5):625 called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassB.Func1Par']));
  CheckEquals('TAspect2.After(TClassB, [5]) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TAspect2.After']));
end;

procedure TAspectTypeFunction_Test.TestFuncWith2Parameter;
var
  r, x, y: Integer;
begin
  x := 5;
  y := 3;
  CheckEquals(0, GlobalTestList.Count);
  r := ClasseB.Func2Par(x, y);
  CheckEquals(8, r.AsInteger);
  Check(Self.Name = 'TestFuncWith2Parameter',
    'Self was changed after method intercepted');
  CheckEquals(3, GlobalTestList.Count);
  CheckEquals('TAspect2.Before(TClassB, [5, 3]) called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TAspect2.Before']));
  CheckEquals('TClassB.Func2Par(5, 3):8 called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassB.Func2Par']));
  CheckEquals('TAspect2.After(TClassB, [5, 3]) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TAspect2.After']));
end;

procedure TAspectTypeFunction_Test.TestFuncWith3Parameter;
var
  r, x, y, z: Integer;
begin
  x := 5;
  y := 3;
  z := 4;
  CheckEquals(0, GlobalTestList.Count);
  r := ClasseB.Func3Par(x, y, z);
  CheckEquals(12, r.AsInteger);
  Check(Self.Name = 'TestFuncWith3Parameter',
    'Self was changed after method intercepted');
  CheckEquals(3, GlobalTestList.Count);
  CheckEquals('TAspect1.Before(TClassB, [5, 3, 4]) called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TAspect1.Before']));
  CheckEquals('TClassB.Func3Par(5, 3, 4):12 called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassB.Func3Par']));
  CheckEquals('TAspect1.After(TClassB, [5, 3, 4]) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TAspect1.After']));
end;

procedure TAspectTypeFunction_Test.TestFuncWith4Parameter;
var
  r, x, y, z, w: Integer;
begin
  x := 5;
  y := 3;
  z := 4;
  w := 8;
  CheckEquals(0, GlobalTestList.Count);
  r := ClasseB.Func4Par(x, y, z, w);
  CheckEquals(480, r.AsInteger);
  Check(Self.Name = 'TestFuncWith4Parameter',
    'Self was changed after method intercepted');
  CheckEquals(3, GlobalTestList.Count);
  CheckEquals('TAspect1.Before(TClassB, [5, 3, 4, 8]) called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TAspect1.Before']));
  CheckEquals('TClassB.Func4Par(5, 3, 4, 8):480 called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassB.Func3Par']));
  CheckEquals('TAspect1.After(TClassB, [5, 3, 4, 8]) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TAspect1.After']));
end;

procedure TAspectTypeFunction_Test.TestFuncWith5Parameter;
var
  r, x, y, z, w, k: Integer;
begin
  x := 5;
  y := 3;
  z := 4;
  w := 8;
  k := 7;
  CheckEquals(0, GlobalTestList.Count);
  r := ClasseB.Func5Par(x, y, z, w, k);
  CheckEquals(3360, r.AsInteger);
  Check(Self.Name = 'TestFuncWith5Parameter',
    'Self was changed after method intercepted');
  CheckEquals(3, GlobalTestList.Count);
  CheckEquals('TAspect1.Before(TClassB, [5, 3, 4, 8, 7]) called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TAspect1.Before']));
  CheckEquals('TClassB.Func5Par(5, 3, 4, 8, 7):3360 called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassB.Func5Par']));
  CheckEquals('TAspect1.After(TClassB, [5, 3, 4, 8, 7]) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TAspect1.After']));
end;
*)

initialization
  TestFramework.RegisterTest('TAspectPrimitiveProcedure_Tests Suite',
    TAspectPrimitiveProcedure_Test.Suite);
//  TestFramework.RegisterTest('TAspectTypeFunction_Tests Suite',
//    TAspectTypeFunction_Test.Suite);

end.
