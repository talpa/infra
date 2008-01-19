unit AnnotationsModelIntf;

interface

uses
  InfraValueTypeIntf;

type
  IAddress = interface;

  // Interface para uma classe pessoa
  IPerson = interface
    ['{52768039-C8B4-4E31-BA9B-E765951454A8}']
    procedure SetAddress(const Value: IAddress);
    procedure SetBirthday(const Value: IInfraDate);
    procedure SetEmail(const Value: IInfraString);
    procedure SetName(const Value: IInfraString);
    function GetAddress: IAddress;
    function GetBirthday: IInfraDate;
    function GetAge: IInfraInteger;
    function GetEmail: IInfraString;
    function GetName: IInfraString;
    property Address: IAddress read GetAddress write SetAddress;
    property Name: IInfraString read GetName write SetName;
    property Email: IInfraString read GetEmail write SetEmail;
    property Birthday: IInfraDate read GetBirthday
      write SetBirthday;
  end;

  // Interface para uma classe estudante
  IStudent = interface(IPerson)
    ['{996D9565-F487-4766-B2F9-F37BEB74AE2F}']
    function GetSchoolNumber: IInfraString;
    procedure SetSchoolNumber(const Value: IInfraString);
    property SchoolNumber: IInfraString read GetSchoolNumber
      write SetSchoolNumber;
  end;

  // Interface para uma classe endereço
  IAddress = interface
    ['{844C021B-8D73-4CC9-867A-9553BAA0F0A2}']
    function GetCity: IInfraString;
    function GetNumber: IInfraInteger;
    function GetQuarter: IInfraString;
    function GetStreet: IInfraString;
    procedure SetCity(const Value: IInfraString);
    procedure SetNumber(const Value: IInfraInteger);
    procedure SetQuarter(const Value: IInfraString);
    procedure SetStreet(const Value: IInfraString);
    property Street: IInfraString read GetStreet write SetStreet;
    property City: IInfraString read GetCity write SetCity;
    property Quarter: IInfraString read GetQuarter write SetQuarter;
    property Number: IInfraInteger read GetNumber write SetNumber;
  end;

  // Uma interface de anotação
  IVersion = Interface
    ['{E0853981-6E95-4D98-9AD8-F363BB345F90}']
    function GetVersionNumber: IInfraString;
    procedure SetVersionNumber(const Value: IInfraString);
    property VersionNumber: IInfraString read GetVersionNumber write SetVersionNumber;
  end;

implementation

end.



