{
  Esta unit é usada única e exclusivamente para registrar
  nossas classes no InfraReflection.

  Cada classe a ser reflexionada precisa ter uma herança aqui
  por que os Getters e Setters da nossa classe devem permanecer protegidas
  para que um programador inadvertidamente tente usar a classe em vez da
  interface.
}
unit PersistenceModelReflex;

interface

uses
  PersistenceModel;

type
  TAccountReflex = class(TAccount);

implementation

uses
  PersistenceModelIntf, 
  InfraCommonIntf, 
  InfraValueTypeIntf;

{
function RegisterAccountOnReflection: IClassInfo;
var
  vPropInfo: IPropertyInfo;
begin
  with TypeService do
  begin
    with AddType(IAccount, 'Account', TAccount, IInfraObject, GetType(IInfraObject)) do
    begin
      AddConstructorInfo('Create', nil, @TAccount.Create);
      AddPropertyInfo('ID', GetType(IInfraInteger), @TAccountReflex.GetID, @TAccountReflex.SetID);
      AddPropertyInfo('Name', GetType(IInfraString), @TAccountReflex.GetName, @TAccountReflex.SetName);
      AddPropertyInfo('AccountNumber', GetType(IInfraString), @TAccountReflex.GetAccountNumber, @TAccountReflex.SetAccountNumber);
      AddPropertyInfo('InitialBalance', GetType(IInfraDouble), @TAccountReflex.GetInitialBalance, @TAccountReflex.SetInitialBalance);
      AddPropertyInfo('CurrentBalance', GetType(IInfraDouble), @TAccountReflex.GetCurrentBalance, @TAccountReflex.SetCurrentBalance);
    end;
  end;
end;

initialization
  RegisterAccountOnReflection;
}

end.
