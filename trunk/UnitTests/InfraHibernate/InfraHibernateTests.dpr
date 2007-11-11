// Uncomment the following directive to create a console application
// or leave commented to create a GUI application... 
// {$APPTYPE CONSOLE}
program InfraHibernateTests;

{$I Infra.Inc}

uses
  {$IFDEF USE_FASTMM}FastMM4, {$ENDIF}
  ApplicationContext,
  TestFramework,
  InfraHibernate
  {$IFDEF LINUX}, QForms, QGUITestRunner
  {$ELSE}, Forms, GUITestRunner
  {$ENDIF},
  TextTestRunner,
  HibernateModel in 'HibernateModel.pas',
  HibernateModelIntf in 'HibernateModelIntf.pas',
  HibernateTests in 'HibernateTests.pas';

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
