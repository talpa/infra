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
    ListBox1: TListBox;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
    PersonBindManager, CompanyBindManager: IBindManager;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  SysUtils,
  Model,
  ModelIntf,
  InfraValueType,
  InfraValueTypeIntf,
  InfraValueTypeConvert,
  InfraBindingConverter,
  InfraBindingManager;

var
  Person: IPerson;
  Company: ICompany;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  PersonBindManager := BindingService.GetNewBindManager;
  CompanyBindManager := BindingService.GetNewBindManager;
end;

procedure TForm1.FormActivate(Sender: TObject);
var
  Binding: IBinding;
begin
  { initialize and load sample Person data }
  Person := TPerson.Create;
  Person.LoadSampleData;

  Company := TCompany.Create;
  Company.LoadSampleData;

  { set DataContext of BindingManager}
  PersonBindManager.DataContext := Person;
  CompanyBindManager.DataContext := Company;

  { bind Person properties to Controls }
  PersonBindManager.Add('Name', Edit3, 'Text').TwoWay;
  PersonBindManager.Add('Name', Label3, 'Caption');
  PersonBindManager.Add('Country', Edit1, 'Text');
  PersonBindManager.Add('Country', Label4, 'Caption');
  PersonBindManager.Add('Active', CheckBox1, 'Checked').TwoWay;
  PersonBindManager.Add('Active', Label5, 'Caption', TBooleanToText.Create);

  Binding := CompanyBindManager.Add('Employees', ListBox1, 'Items', TInfraListToText.Create);
  Binding.ConverterParameter := TInfraString.NewFrom('Name');


  PersonBindManager.Active := True;
  CompanyBindManager.Active := True;
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

procedure TForm1.Button4Click(Sender: TObject);
var
  vEmployee: IPerson;
begin
  vEmployee := TPerson.Create;
  vEmployee.Name.AsString := 'Leszek Gojniczek ' + IntToStr(ListBox1.Items.Count);
  Company.Employees.Add(vEmployee);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Company.Employees.Clear;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    Company.Employees.Delete(ListBox1.ItemIndex);
end;

end.
