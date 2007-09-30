{
http://www.knowsky.com/336168.html
http://www.knowsky.com/336169.html
}
// FixUses
unit u_TestCasesTypes;

interface

uses
  TestFramework, u_ClassesFuncType, u_ClassesProcType;

type

  TAspectTypeProcedure_Test = class(TTestCase)
  private
    ClasseA: IClassA;
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

implementation

uses
  SysUtils, InfraInterfaces, u_GlobalTestList, SimpleTypeImpl;

procedure TAspectTypeProcedure_Test.SetUp;
begin
  inherited;
  ClasseA := TClassA.Create;
  GlobalTestList.Clear;
end;

procedure TAspectTypeProcedure_Test.TearDown;
begin
  GlobalTestList.Clear;
  inherited;
end;

procedure TAspectTypeProcedure_Test.TestMethodNotIntercepted;
begin
  ClasseA.NotIntercepted;
  CheckEquals(1, GlobalTestList.Count, 'NotIntercepted not was called');
  CheckEquals('TClassA.NotIntercepted() called', GlobalTestList.Strings[0]);
end;

procedure TAspectTypeProcedure_Test.TestProcWithoutParameters;
begin
  CheckEquals(0, GlobalTestList.Count);
  ClasseA.ProcSemPar;
  Check(Self.Name = 'TestProcWithoutParameters',
    'Self was changed after method intercepted');
  CheckEquals(5, GlobalTestList.Count);
  CheckEquals('TAspect1.Before(TClassA, []) called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TAspect1.Before']));
  CheckEquals('TAspect2.Before(TClassA, []) called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TAspect2.Before']));
  CheckEquals('TClassA.ProcSemPar() called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TClassA.ProcSemPar']));
  CheckEquals('TAspect2.After(TClassA, []) called',
    GlobalTestList.Strings[3], Format(cMsgAdviceOutOfOrder,
      ['TAspect2.After']));
  CheckEquals('TAspect1.After(TClassA, []) called',
    GlobalTestList.Strings[4], Format(cMsgAdviceOutOfOrder,
      ['TAspect1.After']));
end;

procedure TAspectTypeProcedure_Test.TestProcWith1Parameter;
var
  x: IInfraInteger;
begin
  x := TInfraInteger.NewFrom(5);
  CheckEquals(0, GlobalTestList.Count);
  ClasseA.Proc1Par(x);
  Check(Self.Name = 'TestProcWith1Parameter',
    'Self was changed after method intercepted');
  CheckEquals(3, GlobalTestList.Count);
  CheckEquals('TAspect2.Before(TClassA, [5]) called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TAspect2.Before']));
  CheckEquals('TClassA.Proc1Par(5) called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassA.Proc1Par']));
  CheckEquals('TAspect2.After(TClassA, [5]) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TAspect2.After']));
end;

procedure TAspectTypeProcedure_Test.TestProcWith2Parameter;
var
  x, y: IInfraInteger;
begin
  x := TInfraInteger.NewFrom(5);
  y := TInfraInteger.NewFrom(3);
  CheckEquals(0, GlobalTestList.Count);
  ClasseA.Proc2Par(x, y);
  Check(Self.Name = 'TestProcWith2Parameter',
    'Self was changed after method intercepted');
  CheckEquals(3, GlobalTestList.Count);
  CheckEquals('TAspect1.Before(TClassA, [5, 3]) called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TAspect1.Before']));
  CheckEquals('TClassA.Proc2Par(5, 3) called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassA.Proc2Par']));
  CheckEquals('TAspect1.After(TClassA, [5, 3]) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TAspect1.After']));
end;

procedure TAspectTypeProcedure_Test.TestProcWith3Parameter;
var
  x, y, z: IInfraInteger;
begin
  x := TInfraInteger.NewFrom(5);
  y := TInfraInteger.NewFrom(3);
  z := TInfraInteger.NewFrom(4);
  CheckEquals(0, GlobalTestList.Count);
  ClasseA.Proc3Par(x, y, z);
  Check(Self.Name = 'TestProcWith3Parameter',
    'Self was changed after method intercepted');
  CheckEquals(3, GlobalTestList.Count);
  CheckEquals('TAspect2.Before(TClassA, [5, 3, 4]) called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TAspect2.Before']));
  CheckEquals('TClassA.Proc3Par(5, 3, 4) called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassA.Proc3Par']));
  CheckEquals('TAspect2.After(TClassA, [5, 3, 4]) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TAspect2.After']));
end;

procedure TAspectTypeProcedure_Test.TestProcWith4Parameter;
var
  x, y, z, w: IInfraInteger;
begin
  x := TInfraInteger.NewFrom(5);
  y := TInfraInteger.NewFrom(3);
  z := TInfraInteger.NewFrom(4);
  w := TInfraInteger.NewFrom(8);
  CheckEquals(0, GlobalTestList.Count);
  ClasseA.Proc4Par(x, y, z, w);
  Check(Self.Name = 'TestProcWith4Parameter',
    'Self was changed after method intercepted');
  CheckEquals(3, GlobalTestList.Count);
  CheckEquals('TAspect2.Before(TClassA, [5, 3, 4, 8]) called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TAspect2.Before']));
  CheckEquals('TClassA.Proc4Par(5, 3, 4, 8) called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassA.Proc3Par']));
  CheckEquals('TAspect2.After(TClassA, [5, 3, 4, 8]) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TAspect2.After']));
end;

procedure TAspectTypeProcedure_Test.TestProcWith5Parameter;
var
  x, y, z, w, k: IInfraInteger;
begin
  x := TInfraInteger.NewFrom(5);
  y := TInfraInteger.NewFrom(3);
  z := TInfraInteger.NewFrom(4);
  w := TInfraInteger.NewFrom(8);
  k := TInfraInteger.NewFrom(7);
  CheckEquals(0, GlobalTestList.Count);
  ClasseA.Proc5Par(x, y, z, w, k);
  Check(Self.Name = 'TestProcWith5Parameter',
    'Self was changed after method intercepted');
  CheckEquals(3, GlobalTestList.Count);
  CheckEquals('TAspect2.Before(TClassA, [5, 3, 4, 8, 7]) called',
    GlobalTestList.Strings[0], Format(cMsgAdviceOutOfOrder,
      ['TAspect2.Before']));
  CheckEquals('TClassA.Proc5Par(5, 3, 4, 8, 7) called',
    GlobalTestList.Strings[1], Format(cMsgAdviceOutOfOrder,
      ['TClassA.Proc5Par']));
  CheckEquals('TAspect2.After(TClassA, [5, 3, 4, 8, 7]) called',
    GlobalTestList.Strings[2], Format(cMsgAdviceOutOfOrder,
      ['TAspect2.After']));
end;

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
  r: IInfraInteger;
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
  r: IInfraInteger;
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
  r: IInfraInteger;
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
  r, x: IInfraInteger;
begin
  x := TInfraInteger.NewFrom(5);
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
  r, x, y: IInfraInteger;
begin
  x := TInfraInteger.NewFrom(5);
  y := TInfraInteger.NewFrom(3);
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
  r, x, y, z: IInfraInteger;
begin
  x := TInfraInteger.NewFrom(5);
  y := TInfraInteger.NewFrom(3);
  z := TInfraInteger.NewFrom(4);
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
  r, x, y, z, w: IInfraInteger;
begin
  x := TInfraInteger.NewFrom(5);
  y := TInfraInteger.NewFrom(3);
  z := TInfraInteger.NewFrom(4);
  w := TInfraInteger.NewFrom(8);
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
  r, x, y, z, w, k: IInfraInteger;
begin
  x := TInfraInteger.NewFrom(5);
  y := TInfraInteger.NewFrom(3);
  z := TInfraInteger.NewFrom(4);
  w := TInfraInteger.NewFrom(8);
  k := TInfraInteger.NewFrom(7);
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

initialization
  TestFramework.RegisterTest('TAspectTypeProcedure_Tests Suite',
    TAspectTypeProcedure_Test.Suite);
  TestFramework.RegisterTest('TAspectTypeFunction_Tests Suite',
    TAspectTypeFunction_Test.Suite);

end.
