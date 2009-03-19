program TypeDemo;

uses
  ApplicationContext,
  Forms,
  uTypeDemo in 'uTypeDemo.pas' {Form1},
  ModelIntf in 'ModelIntf.pas',
  Model in 'Model.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
