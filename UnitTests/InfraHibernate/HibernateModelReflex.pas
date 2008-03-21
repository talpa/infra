{
  Esta unit é usada única e exclusivamente para registrar
  nossas classes no InfraReflection.

  Cada classe a ser reflexionada precisa ter uma herança aqui
  por que os Getters e Setters da nossa classe devem permanecer protegidas
  para que um programador inadvertidamente tente usar a classe em vez da
  interface.
}
unit HibernateModelReflex;

interface

uses
  HibernateModel;

type
  TAccountReflex = class(TAccount);

implementation

uses
  HibernateModelIntf, InfraCommonIntf, InfraValueTypeIntf,
  InfraHibernateAnnotationIntf;

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

      vPropInfo := AddPropertyInfo('ID', GetType(IInfraInteger),
        @TAccountReflex.GetID);
      // anotação para informar a persistencia qual o atributo que corresponde
      // à chave primária.
      vPropInfo.Annotate(IID);

      vPropInfo := AddPropertyInfo('Name', GetType(IInfraString),
        @TAccountReflex.GetName, @TAccountReflex.SetName);
      // anotação da coluna por que nome no banco difere do nome do atributo.
      vColumn := vPropInfo.Annotate(IColumn) as IColumn;
      vColumn.Name := 'ACCOUNTNAME';

      AddPropertyInfo('AccountNumber', GetType(IInfraString),
        @TAccountReflex.GetAccountNumber, @TAccountReflex.SetAccountNumber);
      AddPropertyInfo('InitialBalance', GetType(IInfraDouble),
        @TAccountReflex.GetInitialBalance, @TAccountReflex.SetInitialBalance);
      AddPropertyInfo('CurrentBalance', GetType(IInfraDouble),
        @TAccountReflex.GetCurrentBalance, @TAccountReflex.SetCurrentBalance);
    end;
  end;
end;

initialization
  RegisterAccountOnReflection;

end.
