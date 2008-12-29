unit ModelIntf;

interface

uses
  InfraValueTypeIntf;

type

  IUser = interface(IInfraObject)
    ['{541D07F9-B7EA-4E9C-9241-AED9C0F97639}']
    function GetLogin: IInfraString;
    function GetPassword: IInfraString;
    procedure SetLogin(const Value: IInfraString);
    procedure SetPassword(const Value: IInfraString);
    property Login: IInfraString read GetLogin write SetLogin;
    property Password: IInfraString read GetPassword write SetPassword;
  end;

  ICity = interface(IInfraObject)
    ['{AAF2F0C1-034F-4A86-827C-6284A82860F4}']
    function GetName: IInfraString;
    function GetPopulation: IInfraInteger;
    procedure SetName(const Value: IInfraString);
    procedure SetPopulation(const Value: IInfraInteger);
    procedure LoadSampleData;
    property Name: IInfraString read GetName write SetName;
    property Population: IInfraInteger read GetPopulation write SetPopulation;
  end;

  IPerson = interface(IInfraObject)
    ['{9281A67B-128F-4AC3-9B95-A2C5F61D47BD}']
    function GetActive: IInfraBoolean;
    function GetAddress: IInfraString;
    function GetAmount: IInfraDouble;
    function GetBirthday: IInfraDateTime;
    function GetCountry: IInfraString;
    function GetEmail: IInfraString;
    function GetID: IInfraInteger;
    function GetName: IInfraString;
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
    procedure SetName(const Value: IInfraString);
    procedure SetState(const Value: IInfraString);
    procedure SetDetails(const Value: IInfraString);
    procedure SetCity(const Value: ICity);
    procedure LoadSampleData;    
    property ID: IInfraInteger read GetID write SetID;
    property Name: IInfraString read GetName write SetName;
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

implementation

end.

