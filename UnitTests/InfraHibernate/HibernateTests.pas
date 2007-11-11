unit HibernateTests;

interface

uses
  InfraHibernateIntf,
  TestFramework;

type
  THibernateTests = class(TTestCase)
  private
    FConfiguration: IConfiguration;
    FSessionFactory: ISessionFactory;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadObjectByOID;
  end;

implementation

uses
  SysUtils, InfraValueType, InfraValueTypeIntf,
  HibernateModel, HibernateModelIntf, Math;

{ THibernateTests }

procedure THibernateTests.SetUp;
begin
  inherited;
  FConfiguration := PersistenceService.Configuration;
  FSessionFactory := PersistenceService.SessionFactory;

  // Aqui é definido no configuration algumas propriedades, para que o
  // InfraHibernate saiba algumas coisas necessárias para configurar o 
  // connection e outras coisas internas. Caso haja propriedades especificas
  // que precisam ser definidas, podem ser naturalmente colocados ai tambem
  // que o tipo de Connection específico poderar ler sem problema.

  with FConfiguration do
  begin
    PropertyItem['ConnectionClass'] := 'DBXConnection';
    PropertyItem['Connection.DatabaseLogin'] := 'SYSDBA';
    PropertyItem['Connection.DatabasePassword'] := 'masterkey';
    PropertyItem['Connection.DatabasePath'] :=
      '..\InfraHibernate\DBDEMOS.FDB';
  end;
end;

procedure THibernateTests.TearDown;
begin
  inherited;
  FConfiguration.Properties.Clear;
end;

procedure THibernateTests.TestLoadObjectByOID;
var
  vSession: ISession;
  vObj: IAccount;
  vOID: IInfraType;
begin
  // Abre uma nova sessão a ser utilizada para carregar e popular o objeto
  vSession := FSessionFactory.OpenSession;

  // Crio um oid do tipo inteiro. Este código está desta forma para nao ser
  // necessário criar um objeto account e setar o oid do mesmo antes de passar
  // para o load.
  vOID := TInfraInteger.NewFrom(1);

  // carrega o objeto Account com base no oid fornecido.
  vObj := vSession.Load(IAccount, vOID) as IAccount;

  // verifica se o objeto realmente foi carregado.
  CheckNotNull(vObj, 'Cannot load object');
  CheckEquals('BB 1361', vObj.Name.AsString, 'Name mismatch');
  CheckEquals('1361-2', vObj.AccountNumber.AsString, 'AccountNumber mismatch');
  CheckTrue(SameValue(125.3, vObj.InitialBalance.AsDouble), 'InitialBalance mismatch');
  CheckTrue(SameValue(1524.25, vObj.CurrentBalance.AsDouble), 'CurrentBalance mismatch');
end;

initialization
  TestFramework.RegisterTest('Hibernate TestsSuite',
    THibernateTests.Suite);

end.
