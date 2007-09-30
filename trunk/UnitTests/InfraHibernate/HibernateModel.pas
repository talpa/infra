unit HibernateModel;

interface

uses
  InfraValueTypeIntf, InfraValueType, InfraCommonIntf,
  HibernateModelIntf;

type
  TAccount = class(TInfraObject, IAccount)
  private
    FID: IInfraInteger;
    FAccountNumber: IInfraString;
    FName: IInfraString;
    FInitialBalance: IInfraDouble;
    FCurrentBalance: IInfraDouble;
    function GetAccountNumber: IInfraString;
    function GetName: IInfraString;
    function GetInitialBalance: IInfraDouble;
    function GetCurrentBalance: IInfraDouble;
    procedure SetAccountNumber(const value: IInfraString);
    procedure SetName(const value: IInfraString);
    procedure SetInitialBalance(const value: IInfraDouble);
    procedure SetCurrentBalance(const value: IInfraDouble);
    function GetID: IInfraInteger;
  protected
    property ID: IInfraInteger read GetID;
    property AccountNumber: IInfraString read GetAccountNumber
      write SetAccountNumber;
    property Name: IInfraString read GetName write SetName;
    property InitialBalance: IInfraDouble read GetInitialBalance
      write SetInitialBalance;
    property CurrentBalance: IInfraDouble read GetCurrentBalance
      write SetCurrentBalance;
  public
    procedure InfraInitInstance; override;
  end;

implementation

uses
  MapperAnnotationIntf;

function RegisterAccountOnReflection: IClassInfo;
var
  vPropInfo: IPropertyInfo;
  vColumn: IColumn;
begin
  with TypeService do
  begin
    with AddType(IAccount, 'Account', TAccount,
      IInfraObject, GetType(IInfraObject)) do
    begin

      vPropInfo := AddPropertyInfo('ID', GetType(IInfraInteger), @TAccount.GetID);
      // anotação para persistencia
      vPropInfo.Annotate(IID);


      vPropInfo := AddPropertyInfo('Name', GetType(IInfraString),
        @TAccount.GetName, @TAccount.SetName);
      // anotação para persistencia
      vColumn := vPropInfo.Annotate(IColumn) as IColumn;
      vColumn.Name := 'ACCOUNTNAME';

      AddPropertyInfo('AccountNumber', GetType(IInfraString),
        @TAccount.GetAccountNumber, @TAccount.SetAccountNumber);
      AddPropertyInfo('InitialBalance', GetType(IInfraDouble),
        @TAccount.GetInitialBalance, @TAccount.SetInitialBalance);
      AddPropertyInfo('CurrentBalance', GetType(IInfraDouble),
        @TAccount.GetCurrentBalance, @TAccount.SetCurrentBalance);
    end;
  end;
end;

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

initialization
  RegisterAccountOnReflection;

end.
