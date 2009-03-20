unit uTypeDemo;

interface

uses
  Forms, Classes, Controls, StdCtrls, ExtCtrls, InfraBindingIntf;

type
  TForm1 = class(TForm)
    Memo2: TMemo;
    Edit3: TEdit;
    Label2: TLabel;
    Label1: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    CheckBox1: TCheckBox;
    Label5: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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

uses ModelIntf, Model,
  InfraValueType,
  InfraValueTypeIntf,
  InfraValueTypeConvert,
  InfraBindingConverter,
  InfraBindingManager;

var
  Person: IPerson;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  bm := BindingService.GetNewBindManager;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  { initialize and load sample Person data }
  Person := TPerson.Create;
  Person.LoadSampleData;

  { set DataContext of BindingManager}
  bm.DataContext := Person;

  { bind Person properties to Controls }
  bm.Add('Name', Edit3, 'Text').TwoWay;
  bm.Add('Name', Label3, 'Caption');
  bm.Add('Country', Edit1, 'Text');
  bm.Add('Country', Label4, 'Caption');
  bm.Add('Active', CheckBox1, 'Checked').TwoWay;
  bm.Add('Active', Label5, 'Caption', TBooleanToText.Create);

  bm.Active := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Person.Name.AsString := 'Mattrah John';
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Person.Country.AsString := 'England';
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Person.Active.AsBoolean := not Person.Active.AsBoolean;
end;

end.
