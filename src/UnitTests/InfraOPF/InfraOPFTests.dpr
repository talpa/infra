// Uncomment the following directive to create a console application
// or leave commented to create a GUI application... 
// {$APPTYPE CONSOLE}

program InfraOPFTests;

uses
  TestFramework {$IFDEF LINUX},
  QForms,
  QGUITestRunner {$ELSE},
  Forms,
  GUITestRunner {$ENDIF},
  TextTestRunner,
  ApplicationContext,
  InfraMocks in 'InfraMocks.pas',
  InfraConnectionProviderTests in 'InfraConnectionProviderTests.pas',
  InfraConfigurationTests in 'InfraConfigurationTests.pas',
  InfraPersistenceServiceTests in 'InfraPersistenceServiceTests.pas',
  InfraPersistenceEngineTests in 'InfraPersistenceEngineTests.pas',
  InfraTestsUtil in 'InfraTestsUtil.pas',
  InfraTemplateReader_IOTests in 'InfraTemplateReader_IOTests.pas';

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

