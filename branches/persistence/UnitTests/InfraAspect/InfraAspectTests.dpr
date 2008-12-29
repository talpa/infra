// Uncomment the following directive to create a console application
// or leave commented to create a GUI application... 
// {$APPTYPE CONSOLE}
program InfraAspectTests;

{$I Test.Inc}

uses
  {$IFDEF USE_FASTMM}FastMM4,
  {$ENDIF} ApplicationContext,
  TestFramework,
  InfraAspect
  {$IFDEF LINUX}, QForms, QGUITestRunner
  {$ELSE}, Forms, GUITestRunner
  {$ENDIF},
  TextTestRunner,
  AspectsTests in 'AspectsTests.pas',
  AspectRegister in 'AspectRegister.pas',
  AspectsClasses in 'AspectsClasses.pas',
  AspectModelIntf in 'AspectModelIntf.pas',
  AspectModel in 'AspectModel.pas';

{$R *.res}

begin
  Application.Initialize;

{$IFDEF LINUX}
  QGUITestRunner.RunRegisteredTests;
{$ELSE}
  if System.IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
{$ENDIF}

end.
