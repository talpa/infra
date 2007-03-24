unit ModelIntf;

interface

uses
  InfraValueTypeIntf;

type
  IAddress = interface;
  
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

  IStudent = interface(IPerson)
    ['{996D9565-F487-4766-B2F9-F37BEB74AE2F}']
    function GetSchoolNumber: IInfraString;
    procedure SetSchoolNumber(const Value: IInfraString);
    property SchoolNumber: IInfraString read GetSchoolNumber
      write SetSchoolNumber;
  end;

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

  IMockMethod = Interface
    ['{32E1DD21-2537-4A27-A3DE-481E85A8E246}']
    function GetMessage: IInfraString;
    function MethodFunc0: IInfraString;
    function MethodFunc1(const p1: IInfraString): IInfraString;
    function MethodFunc2(const p1: IInfraString;
      const p2:IInfraInteger): IInfraInteger;
    function MethodFunc3(const p1: IInfraString; const p2:IInfraInteger;
      const p3: IInfraDateTime): IInfraDateTime;
    function MethodFunc4(const p1: IInfraString; const p2:IInfraInteger;
      const p3: IInfraDateTime; const p4: IInfraBoolean): IInfraBoolean;
    function MethodFunc5(const p1: IInfraString; const p2:IInfraInteger;
      const p3: IInfraDateTime; const p4: IInfraBoolean;
      const p5: IInfraDouble): IInfraDouble;
    procedure MethodProc0;
    procedure MethodProc1(const p1: IInfraString);
    procedure MethodProc2(const p1: IInfraString; const p2:IInfraInteger);
    procedure MethodProc3(const p1: IInfraString; const p2:IInfraInteger;
      const p3: IInfraDateTime);
    procedure MethodProc4(const p1: IInfraString; const p2:IInfraInteger;
      const p3: IInfraDateTime; const p4: IInfraBoolean);
    procedure MethodProc5(const p1: IInfraString; const p2:IInfraInteger;
      const p3: IInfraDateTime; const p4: IInfraBoolean; const p5: IInfraDouble);
    property Message: IInfraString read GetMessage;
  end;

implementation

end.
