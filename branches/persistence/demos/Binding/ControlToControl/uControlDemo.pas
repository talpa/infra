unit uControlDemo;

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
    Edit2: TEdit;
    CheckBox2: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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

{$R *.dfm}

uses
  InfraValueType,
  InfraValueTypeIntf,
  InfraValueTypeConvert,
  InfraBindingConverter,
  InfraBindingManager;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Edit1.Text := 'Funcionou';
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CheckBox1.Checked := False;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CheckBox2.Checked := not CheckBox2.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  bm := BindingService.GetNewBindManager;
end;

procedure TForm1.FormActivate(Sender: TObject);
var
  b: IBinding;
begin
  bm.Add(Edit1, 'Text', Label1, 'Caption');

  bm.Add(Checkbox1, 'Checked', Panel1, 'Visible');

  bm.Add(Edit2, 'Text', Edit2, 'Color', TTextToColorConverter.Create);

  bm.Add(Checkbox1, 'Checked', Checkbox1, 'Caption', TBooleanToText.Create);

  b := bm.Add(Checkbox2, 'Checked', Checkbox2, 'Caption', TBooleanToText.Create);
  b.ConverterParameter := TInfraString.NewFrom('Invisivel;Visivel');

  bm.Active := True;
end;

end.
