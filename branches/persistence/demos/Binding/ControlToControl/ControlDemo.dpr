program ControlDemo;

uses
  ApplicationContext,
  Forms,
  uControlDemo in 'uControlDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
