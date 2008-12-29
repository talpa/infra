// Uncomment the following directive to create a console application
// or leave commented to create a GUI application... 
// {$APPTYPE CONSOLE}
program InfraReflectTests;

{$I Test.Inc}

uses
  {$IFDEF USE_FASTMM}FastMM4,
  {$ENDIF}ApplicationContext,
  TestFramework {$IFDEF LINUX},
  QForms,
  QGUITestRunner {$ELSE},
  Forms,
  GUITestRunner {$ENDIF},
  TextTestRunner,
  ClassInfoTests in 'ClassInfoTests.pas',
  ReflectModel in 'ReflectModel.pas',
  ReflectModelIntf in 'ReflectModelIntf.pas',
  TypeServiceTests in 'TypeServiceTests.pas',
  PropertyInfoTests in 'PropertyInfoTests.pas',
  MethodInfoTests in 'MethodInfoTests.pas';

{$R *.RES}

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
