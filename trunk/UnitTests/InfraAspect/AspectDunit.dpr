program AspectDunit;

uses
  {$IFDEF USE_FASTMM}FastMM4, {$ENDIF}
  ApplicationContextImpl,
  TestFrameWork,
  GuiTestRunner,
  Infra_TypeInfoRegister,
  u_TestCasesPrimitives in 'u_TestCasesPrimitives.pas',
  u_TestCasesTypes in 'u_TestCasesTypes.pas',
  u_ClassesFuncPrimitive in 'u_ClassesFuncPrimitive.pas',
  u_ClassesProcPrimitive in 'u_ClassesProcPrimitive.pas',
  u_ClassesProcType in 'u_ClassesProcType.pas',
  u_GlobalTestList in 'u_GlobalTestList.pas',
  u_AspectsPrimitive in 'u_AspectsPrimitive.pas',
  u_AspectsType in 'u_AspectsType.pas',
  u_TestCasesMethodType in 'u_TestCasesMethodType.pas',
  u_ClassesFuncType in 'u_ClassesFuncType.pas',
  u_TestCasesMethodNative in 'u_TestCasesMethodNative.pas';

{$R *.res}

begin
  TGUITestRunner.RunRegisteredTests

end.

