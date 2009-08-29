unit Model;

interface

uses
  InfraValueTypeIntf, InfraValueType, InfraCommonIntf, InfraCommon, ModelIntf,
  DateUtils;

type

  TUser = class(TInfraObject, IUser)
  private
    FLogin: IInfraString;
    FPassword: IInfraString;
    function GetLogin: IInfraString;
    function GetPassword: IInfraString;
    procedure SetLogin(const Value: IInfraString);
    procedure SetPassword(const Value: IInfraString);
  public
    procedure InfraInitInstance; override;
    procedure LoadSampleData;
    property Login: IInfraString read GetLogin write SetLogin;
    property Password: IInfraString read GetPassword write SetPassword;
  end;

  TCity = class(TInfraObject, ICity)
  private
    FCityName: IInfraString;
    FPopulation: IInfraInteger;
    function GetCityName: IInfraString;
    function GetPopulation: IInfraInteger;
    procedure SetCityName(const Value: IInfraString);
    procedure SetPopulation(const Value: IInfraInteger);
  public
    procedure InfraInitInstance; override;
    procedure LoadSampleData;
    property CityName: IInfraString read GetCityName write SetCityName;
    property Population: IInfraInteger read GetPopulation write SetPopulation;
  end;

  TPerson = class(TInfraObject, IPerson)
  private
    FID: IInfraInteger;
    FPersonName: IInfraString;
    FEmail: IInfraString;
    FAddress: IInfraString;
    FState: IInfraString;
    FCountry: IInfraString;
    FBirthday: IInfraDateTime;
    FActive: IInfraBoolean;
    FAmount: IInfraDouble;
    FDetails: IInfraString;
    FCity: ICity;
    function GetActive: IInfraBoolean;
    function GetAddress: IInfraString;
    function GetAmount: IInfraDouble;
    function GetBirthday: IInfraDateTime;
    function GetCountry: IInfraString;
    function GetEmail: IInfraString;
    function GetID: IInfraInteger;
    function GetPersonName: IInfraString;
    function GetState: IInfraString;
    function GetDetails: IInfraString;
    function GetCity: ICity;
    procedure SetActive(const Value: IInfraBoolean);
    procedure SetAddress(const Value: IInfraString);
    procedure SetAmount(const Value: IInfraDouble);
    procedure SetBirthday(const Value: IInfraDateTime);
    procedure SetCountry(const Value: IInfraString);
    procedure SetEmail(const Value: IInfraString);
    procedure SetID(const Value: IInfraInteger);
    procedure SetPersonName(const Value: IInfraString);
    procedure SetState(const Value: IInfraString);
    procedure SetDetails(const Value: IInfraString);
    procedure SetCity(const Value: ICity);
  public
    procedure InfraInitInstance; override;
    procedure LoadSampleData;
    property ID: IInfraInteger read GetID write SetID;
    property PersonName: IInfraString read GetPersonName write SetPersonName;
    property Email: IInfraString read GetEmail write SetEmail;
    property Address: IInfraString read GetAddress write SetAddress;
    property State: IInfraString read GetState write SetState;
    property Country: IInfraString read GetCountry write SetCountry;
    property Birthday: IInfraDateTime read GetBirthday write SetBirthday;
    property Active: IInfraBoolean read GetActive write SetActive;
    property Amount: IInfraDouble read GetAmount write SetAmount;
    property Details: IInfraString read GetDetails write SetDetails;
    property City: ICity read GetCity write SetCity;
  end;

  TCompany = class(TInfraObject, ICompany)
  private
    FCompanyName: IInfraString;
    FEmployees: IInfraList;
    function GetCompanyName: IInfraString;
    procedure SetCompanyName(const Value: IInfraString);
    function GetEmployees: IInfraList;
  public
    procedure InfraInitInstance; override;
    procedure LoadSampleData;
    property CompanyName: IInfraString read GetCompanyName write SetCompanyName;
    property Employees: IInfraList read GetEmployees;
  end;  

procedure RegisterUser;
procedure RegisterPerson;
procedure RegisterCity;
procedure RegisterCompany;

implementation

uses uRandomData, SysUtils, Math, StrUtils;

procedure RegisterUser;
var
  lUser: IClassInfo;
begin
  with TypeService do
  begin
    lUser := AddType(IUser, 'User', TUser, IInfraObject, GetType(IInfraObject));

    with lUser do
    begin
      AddConstructorInfo('Create', nil, @TUser.Create);
      AddPropertyInfo('Login', GetType(IInfraString),
        @TUser.GetLogin, @TUser.SetLogin);
      AddPropertyInfo('Password', GetType(IInfraString),
        @TUser.GetPassword, @TUser.SetPassword);
    end;
  end;
end;

procedure RegisterPerson;
var
  lPerson: IClassInfo;
begin
  with TypeService do
  begin
    lPerson := AddType(IPerson, 'Person', TPerson, IInfraObject,
      GetType(IInfraObject));

    with lPerson do
    begin
      AddConstructorInfo('Create', nil, @TPerson.Create);
      AddPropertyInfo('ID', GetType(IInfraInteger),
        @TPerson.GetID, @TPerson.SetID);
      AddPropertyInfo('PersonName', GetType(IInfraString),
        @TPerson.GetPersonName, @TPerson.SetPersonName);
      AddPropertyInfo('Email', GetType(IInfraString),
        @TPerson.GetEmail, @TPerson.SetEmail);
      AddPropertyInfo('Address', GetType(IInfraString),
        @TPerson.GetAddress, @TPerson.SetAddress);
      AddPropertyInfo('City', GetType(ICity),
        @TPerson.GetCity, @TPerson.SetCity);
      AddPropertyInfo('State', GetType(IInfraString),
        @TPerson.GetState, @TPerson.SetState);
      AddPropertyInfo('Country', GetType(IInfraString),
        @TPerson.GetCountry, @TPerson.SetCountry);
      AddPropertyInfo('Birthday', GetType(IInfraDateTime),
        @TPerson.GetBirthday, @TPerson.SetBirthday);
      AddPropertyInfo('Active', GetType(IInfraBoolean),
        @TPerson.GetActive, @TPerson.SetActive);
      AddPropertyInfo('Amount', GetType(IInfraDouble),
        @TPerson.GetAmount, @TPerson.SetAmount);
      AddPropertyInfo('Details', GetType(IInfraString),
        @TPerson.GetDetails, @TPerson.SetDetails);
    end;
  end;
end;

procedure RegisterCity;
var
  lCity: IClassInfo;
begin
  with TypeService do
  begin
    lCity := AddType(ICity, 'City', TCity, IInfraObject, GetType(IInfraObject));

    with lCity do
    begin
      AddConstructorInfo('Create', nil, @TCity.Create);
      AddPropertyInfo('CityName', GetType(IInfraString),
        @TCity.GetCityName, @TCity.SetCityName);
      AddPropertyInfo('Population', GetType(IInfraInteger),
        @TCity.GetPopulation, @TCity.SetPopulation);
    end;
  end;
end;

procedure RegisterCompany;
var
  lCompany: IClassInfo;
begin
  with TypeService do
  begin
    lCompany := AddType(ICompany, 'Company', TCompany, IInfraObject, GetType(IInfraObject));

    with lCompany do
    begin
      AddConstructorInfo('Create', nil, @TCompany.Create);
      AddPropertyInfo('CompanyName', GetType(IInfraString),
        @TCompany.GetCompanyName, @TCompany.SetCompanyName);
      AddPropertyInfo('Employees', GetType(IInfraList),
        @TCompany.GetEmployees);
    end;
  end;
end;

{ TUser }

function TUser.GetLogin: IInfraString;
begin
  Result := FLogin;
end;

function TUser.GetPassword: IInfraString;
begin
  Result := FPassword;
end;

procedure TUser.InfraInitInstance;
begin
  inherited;

  FLogin := AddProperty('Login') as IInfraString;
  FPassword := AddProperty('Password') as IInfraString;
end;

procedure TUser.LoadSampleData;
begin

end;

procedure TUser.SetLogin(const Value: IInfraString);
begin
  FLogin := Value;
end;

procedure TUser.SetPassword(const Value: IInfraString);
begin
  FPassword := Value;
end;

{ TCity }

procedure TCity.InfraInitInstance;
begin
  inherited;
  FCityName := AddProperty('CityName') as IInfraString;
  FPopulation := AddProperty('Population') as IInfraInteger;
end;

procedure TCity.LoadSampleData;
begin
  CityName.AsString := RandomCity;
  Population.AsInteger := Random(200000);
end;

function TCity.GetCityName: IInfraString;
begin
  Result := FCityName;
end;

function TCity.GetPopulation: IInfraInteger;
begin
  Result := FPopulation;
end;

procedure TCity.SetCityName(const Value: IInfraString);
begin
  FCityName := Value;
end;

procedure TCity.SetPopulation(const Value: IInfraInteger);
begin
  FPopulation := Value;
end;

{ TPerson }

procedure TPerson.InfraInitInstance;
begin
  inherited;
  FID := AddProperty('ID') as IInfraInteger;
  FPersonName := AddProperty('PersonName') as IInfraString;
  FEmail := AddProperty('Email') as IInfraString;
  FAddress := AddProperty('Address') as IInfraString;
  FState := AddProperty('State') as IInfraString;
  FCountry := AddProperty('Country') as IInfraString;
  FBirthday := AddProperty('Birthday') as IInfraDateTime;
  FActive := AddProperty('Active') as IInfraBoolean;
  FAmount := AddProperty('Amount') as IInfraDouble;
  FDetails := AddProperty('Details') as IInfraString;
  FCity := AddProperty('City') as ICity;
end;

procedure TPerson.LoadSampleData;
begin
  ID.AsInteger := Random(1500);
  PersonName.AsString := RandomPersonName;
  Email.AsString := RandomEmail(RandomPersonName, RandomCompanyName);
  Address.AsString := RandomStreet;
  State.AsString := RandomState;
  Country.AsString := RandomCountry;
  Birthday.AsDateTime := 30280+Random(1000);
  Active.AsBoolean := Random(1)=1;
  Amount.AsDouble := Random(100000);
  Details.AsString :=
    'Observações ' + RandomCompanySuffixes + #13 +
    'Observações ' + RandomCompanySuffixes + #13 +
    'Observações ' + RandomCompanySuffixes;
  City.LoadSampleData;  
end;

function TPerson.GetActive: IInfraBoolean;
begin
  Result := FActive;
end;

function TPerson.GetAddress: IInfraString;
begin
  Result := FAddress;
end;

function TPerson.GetAmount: IInfraDouble;
begin
  Result := FAmount;
end;

function TPerson.GetBirthday: IInfraDateTime;
begin
  Result := FBirthday;
end;

function TPerson.GetCity: ICity;
begin
  Result := FCity;
end;

function TPerson.GetCountry: IInfraString;
begin
  Result := FCountry;
end;

function TPerson.GetDetails: IInfraString;
begin
  Result := FDetails;
end;

function TPerson.GetEmail: IInfraString;
begin
  Result := FEmail;
end;

function TPerson.GetID: IInfraInteger;
begin
  Result := FID;
end;

function TPerson.GetPersonName: IInfraString;
begin
  Result := FPersonName;
end;

function TPerson.GetState: IInfraString;
begin
  Result := FState;
end;

procedure TPerson.SetActive(const Value: IInfraBoolean);
begin
  FActive := Value;
end;

procedure TPerson.SetAddress(const Value: IInfraString);
begin
  FAddress := Value;
end;

procedure TPerson.SetAmount(const Value: IInfraDouble);
begin
  FAmount := Value;
end;

procedure TPerson.SetBirthday(const Value: IInfraDateTime);
begin
  FBirthday := Value;
end;

procedure TPerson.SetCity(const Value: ICity);
begin
  FCity := Value;
end;

procedure TPerson.SetCountry(const Value: IInfraString);
begin
  FCountry := Value;
end;

procedure TPerson.SetDetails(const Value: IInfraString);
begin
  FDetails := Value;
end;

procedure TPerson.SetEmail(const Value: IInfraString);
begin
  FEmail := Value;
end;

procedure TPerson.SetID(const Value: IInfraInteger);
begin
  FID := Value;
end;

procedure TPerson.SetPersonName(const Value: IInfraString);
begin
  FPersonName := Value;
end;

procedure TPerson.SetState(const Value: IInfraString);
begin
  FState := Value;
end;

{ TCompany }

procedure TCompany.InfraInitInstance;
begin
  inherited;
  FCompanyName := AddProperty('CompanyName') as IInfraString;
  FEmployees := AddProperty('Employees') as IInfraList;
end;

procedure TCompany.LoadSampleData;
var
  vEmployee: IPerson;
begin
  CompanyName.AsString := RandomCompanyName;
  vEmployee := TPerson.Create;
  vEmployee.LoadSampleData;
  Employees.Add(vEmployee);
end;

function TCompany.GetCompanyName: IInfraString;
begin
  Result := FCompanyName;
end;

procedure TCompany.SetCompanyName(const Value: IInfraString);
begin
  FCompanyName := Value;
end;

function TCompany.GetEmployees: IInfraList;
begin
  Result := FEmployees;
end;

initialization
  RegisterUser;
  RegisterCity;
  RegisterPerson;
  RegisterCompany;

end.
