unit uSimpleDemo;

interface

uses
  Forms, Classes, Controls, StdCtrls, ExtCtrls, InfraBindingIntf;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    Panel1: TPanel;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo2: TMemo;
    Label1: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    bm: IBindManager;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  InfraBindingManager;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  bm := BindingService.GetNewBindManager;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  bm.Add(Edit1, 'Text', Label1, 'Caption');
//  bm.Add(Edit1, 'Text', Label1, 'Color');
//  bm.Add(Checkbox1, 'Checked', Checkbox1, 'Caption');
//  bm.Add(Checkbox1, 'Checked', Panel1, 'Visible');
  bm.Active := True;
end;

end.
