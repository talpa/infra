// Uncomment the following directive to create a console application
// or leave commented to create a GUI application... 
// {$APPTYPE CONSOLE}
program InfraTests;

{$I Infra.Inc}

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
  Model in 'Model\Model.pas',
  ModelIntf in 'Model\ModelIntf.pas',
  TypeServiceTests in 'InfraReflect\TypeServiceTests.pas',
  PropertyInfoTests in 'InfraReflect\PropertyInfoTests.pas',
  MethodInfoTests in 'InfraReflect\MethodInfoTests.pas',
  AnnotationsTests in 'InfraAnnotation\AnnotationsTests.pas';

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
