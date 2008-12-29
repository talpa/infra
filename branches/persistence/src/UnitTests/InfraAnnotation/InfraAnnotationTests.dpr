// Uncomment the following directive to create a console application
// or leave commented to create a GUI application...
// {$APPTYPE CONSOLE}
program InfraAnnotationTests;

{$I Test.Inc}

uses
  {$IFDEF USE_FASTMM}FastMM4,
  {$ENDIF} ApplicationContext,
  TestFramework 
  {$IFDEF LINUX}, QForms, QGUITestRunner
  {$ELSE}, Forms, GUITestRunner 
  {$ENDIF},
  TextTestRunner,
  AnnotationsTests in 'AnnotationsTests.pas',
  AnnotationsModel in 'AnnotationsModel.pas',
  AnnotationsModelIntf in 'AnnotationsModelIntf.pas';

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
