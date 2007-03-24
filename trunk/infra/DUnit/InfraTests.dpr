// Uncomment the following directive to create a console application
// or leave commented to create a GUI application... 
// {$APPTYPE CONSOLE}

program InfraTests;

uses
  FastMM4,
  ApplicationContext,
  TestFramework {$IFDEF LINUX},
  QForms,
  QGUITestRunner {$ELSE},
  Forms,
  GUITestRunner {$ENDIF},
  TextTestRunner,
  ClassInfoTests in 'InfraReflect\ClassInfoTests.pas',
  Model in 'InfraReflect\Model.pas',
  ModelIntf in 'InfraReflect\ModelIntf.pas',
  TypeServiceTests in 'InfraReflect\TypeServiceTests.pas',
  PropertyInfoTests in 'InfraReflect\PropertyInfoTests.pas',
  MethodInfoTests in 'InfraReflect\MethodInfoTests.pas';

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
