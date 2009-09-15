unit uTypeDemo;

interface

uses
  Forms,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  InfraBindingIntf,
  ModelIntf;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Memo1: TMemo;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Edit3: TEdit;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo3: TMemo;
    ListBox1: TListBox;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Label10: TLabel;
    Label11: TLabel;
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
    PersonBindManager,
    CompanyBindManager: IBindManager;
    Person: IPerson;
    Company: ICompany;
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
  InfraValueTypeConvert,
  InfraBindingConverter,
  InfraValueType,
  InfraBindingManager, uRandomData;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  PersonBindManager := BindingService.GetNewBindManager;
  CompanyBindManager := BindingService.GetNewBindManager;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  // initialize and load sample Person data
  Person := TPerson.Create;
  Person.LoadSampleData;
  // set DataContext of BindingManager and bind Person properties to Controls
  PersonBindManager.DataContext := Person;
  PersonBindManager.Add('PersonName', Edit3, 'Text').TwoWay;
  PersonBindManager.Add('PersonName', Label3, 'Caption');
  PersonBindManager.Add('Country', Edit1, 'Text').TwoWay;
  PersonBindManager.Add('Country', Label4, 'Caption');
  PersonBindManager.Add('Active', CheckBox1, 'Checked').TwoWay;
  PersonBindManager.Add('Active', Label5, 'Caption', TBooleanToText.Create);
  PersonBindManager.Active := True;

  // initialize and load sample Company data
  Company := TCompany.Create;
  Company.LoadSampleData;
  // set DataContext to Company e bind employers to list
  CompanyBindManager.DataContext := Company;
  CompanyBindManager.Add('CompanyName', Label10, 'Caption');
  CompanyBindManager.AddList('Employees.PersonName', ListBox1, 'Items').
    AddSelection('Current', 'ItemIndex');
  CompanyBindManager.Active := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Person.PersonName.AsString := RandomPersonName;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Person.Country.AsString := RandomCountry;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Person.Active.AsBoolean := not Person.Active.AsBoolean;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  vEmployee: IPerson;
  vI: Integer;
begin
  for vI := 0 to 3 do
  begin
    vEmployee := TPerson.Create;
    vEmployee.LoadSampleData;
    Company.Employees.Add(vEmployee);
  end;
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
