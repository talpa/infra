unit HibernateModelIntf;

interface

uses
  InfraValueTypeIntf;

type
  IAccount = interface
    ['{52768039-C8B4-4E31-BA9B-E765951454A8}']
    function GetAccountNumber: IInfraString;
    function GetName: IInfraString;
    function GetInitialBalance: IInfraDouble;
    function GetCurrentBalance: IInfraDouble;
    procedure SetAccountNumber(const value: IInfraString);
    procedure SetName(const value: IInfraString);
    procedure SetInitialBalance(const value: IInfraDouble);
    procedure SetCurrentBalance(const value: IInfraDouble);
    property AccountNumber: IInfraString read GetAccountNumber write SetAccountNumber;
    property Name: IInfraString read GetName write SetName;
    property InitialBalance: IInfraDouble read GetInitialBalance write SetInitialBalance;
    property CurrentBalance: IInfraDouble read GetCurrentBalance write SetCurrentBalance;
  end;
  
implementation

end.
