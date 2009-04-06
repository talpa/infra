unit uControlDemo;

interface

uses
  Forms, Classes, Controls, StdCtrls, ExtCtrls, InfraBindingIntf, ComCtrls,
  Buttons;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Memo2: TMemo;
    l1: TLabel;
    l2: TLabel;
    l3: TLabel;
    Memo3: TMemo;
    CheckBox3: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Edit3: TEdit;
    Label5: TLabel;
    Memo1: TMemo;
    Label7: TLabel;
    Label8: TLabel;
    Label6: TLabel;
    ListBox3: TListBox;
    ListBox4: TListBox;
    Label9: TLabel;
    Label10: TLabel;
    Bevel1: TBevel;
    Label11: TLabel;
    Label12: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    SpeedButton1: TButton;
    SpeedButton2: TButton;
    Button4: TButton;
    Button5: TButton;
    Label13: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Exit(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
    bm: IBindManager;
    FLastListBox: TListBox;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Dialogs,
  InfraValueType,
  InfraValueTypeIntf,
  InfraValueTypeConvert,
  InfraBindingConverter,
  InfraBindingManager;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  bm := BindingService.GetNewBindManager;
  FLastListBox := ListBox1;
end;

procedure TForm1.FormActivate(Sender: TObject);
var
  b: IBinding;
begin
  bm.Add(Edit1, 'Text', Label1, 'Caption');
  bm.Add(Edit2, 'Text', Edit2, 'Color', TTextToColor.Create);

  bm.Add(Checkbox1, 'Checked', Checkbox1, 'Caption', TBooleanToText.Create);
  b := bm.Add(Checkbox2, 'Checked', Checkbox2, 'Caption', TBooleanToText.Create);
  b.ConverterParameter := TInfraString.NewFrom('Invisivel;Visivel');
  bm.Add(Checkbox3, 'Checked', Panel1, 'Visible');

  bm.Add(ListBox1, 'Items', ListBox2, 'Items').TwoWay;
  bm.Add(ListBox3, 'Items', ListBox4, 'Items');

//  bm.Add(ListBox3, 'ItemIndex', ListBox4, 'ItemIndex');
//  b := bm.Add(ListBox3, 'ItemIndex', Label13, 'Caption', TItemIndexToIntegerText.Create);
//  b := bm.Add(ListBox3, 'ItemIndex', Label12, 'Caption', TItemIndexToText.Create);

  bm.Active := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Edit1.Text := 'InfraBinding Framework ROCKS!';
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CheckBox1.Checked := not CheckBox1.Checked;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CheckBox2.Checked := not CheckBox2.Checked;
end;

procedure TForm1.ListBox1Exit(Sender: TObject);
begin
  FLastListBox := TListBox(Sender);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if Assigned(FLastListBox) then
    FLastListBox.Items.Add(Edit3.Text);
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  if (FLastListBox.ItemIndex <> -1) then
    FLastListBox.Items.Delete(FLastListBox.ItemIndex);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FLastListBox.Clear;
  FLastListBox.Items.Objects[FLastListBox.ItemIndex]
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  ListBox3.ItemIndex := 2;
end;

end.
