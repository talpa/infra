unit PersistenceModel;

interface

uses
  InfraValueTypeIntf, 
  InfraValueType, 
  InfraCommonIntf,
  PersistenceModelIntf;

type
  TAccount = class(TInfraObject, IAccount)
  private
    FID: IInfraInteger;
    FAccountNumber: IInfraString;
    FName: IInfraString;
    FInitialBalance: IInfraDouble;
    FCurrentBalance: IInfraDouble;
  protected
    function GetAccountNumber: IInfraString;
    function GetName: IInfraString;
    function GetInitialBalance: IInfraDouble;
    function GetCurrentBalance: IInfraDouble;
    function GetID: IInfraInteger;
    procedure SetAccountNumber(const value: IInfraString);
    procedure SetName(const value: IInfraString);
    procedure SetInitialBalance(const value: IInfraDouble);
    procedure SetCurrentBalance(const value: IInfraDouble);
    procedure SetID(const Value: IInfraInteger);
    property ID: IInfraInteger read GetID write SetID;
    property AccountNumber: IInfraString read GetAccountNumber write SetAccountNumber;
    property Name: IInfraString read GetName write SetName;
    property InitialBalance: IInfraDouble read GetInitialBalance write SetInitialBalance;
    property CurrentBalance: IInfraDouble read GetCurrentBalance write SetCurrentBalance;
  public
    procedure InfraInitInstance; override;
  end;

implementation

{ TAccount }

procedure TAccount.InfraInitInstance;
begin
  inherited;
  FID := AddProperty('ID') as IInfraInteger;
  FAccountNumber := AddProperty('AccountNumber') as IInfraString;
  FName := AddProperty('Name') as IInfraString;
  FInitialBalance := AddProperty('InitialBalance') as IInfraDouble;
  FCurrentBalance := AddProperty('CurrentBalance') as IInfraDouble;
end;

function TAccount.GetID: IInfraInteger;
begin
  Result := FID;
end;

function TAccount.GetAccountNumber: IInfraString;
begin
  Result := FAccountNumber;
end;

function TAccount.GetName: IInfraString;
begin
  Result := FName;
end;

function TAccount.GetInitialBalance: IInfraDouble;
begin
  Result := FInitialBalance;
end;

function TAccount.GetCurrentBalance: IInfraDouble;
begin
  Result := FCurrentBalance;
end;

procedure TAccount.SetAccountNumber(const Value: IInfraString);
begin
  FAccountNumber := Value;
end;

procedure TAccount.SetName(const Value: IInfraString);
begin
  FName := Value;
end;

procedure TAccount.SetInitialBalance(const Value: IInfraDouble);
begin
  FInitialBalance := Value;
end;

procedure TAccount.SetCurrentBalance(const Value: IInfraDouble);
begin
  FCurrentBalance := Value;
end;

procedure TAccount.SetID(const Value: IInfraInteger);
begin
  FID :=Value;
end;
end.
