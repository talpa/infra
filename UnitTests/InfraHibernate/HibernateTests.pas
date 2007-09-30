unit HibernateTests;

interface

uses
  InfraHibernateIntf,
  TestFramework;

type
  THibernateTests = class(TTestCase)
  private
    FCfg: IConfiguration;
    FSF: ISessionFactory;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadObjectByOID;
  end;

implementation

uses
  InfraValueType, InfraValueTypeIntf,
  HibernateModel, HibernateModelIntf, Math;

{ THibernateTests }

procedure THibernateTests.SetUp;
begin
  inherited;
  FCfg := PersistenceService.Configuration;
  FSF := PersistenceService.SessionFactory;
  FCfg.PropertyItem['ConnectionClass'] := 'DBXConnection';
  FCfg.PropertyItem['Connection.DatabasePath'] := 'C:\Infra\UnitTests\InfraHibernate\DBDEMOS.FDB';
  FCfg.PropertyItem['Connection.DatabaseLogin'] := 'SYSDBA';
  FCfg.PropertyItem['Connection.DatabasePassword'] := 'masterkey';
end;

procedure THibernateTests.TearDown;
begin
  inherited;
  FCfg.Properties.Clear;
end;

procedure THibernateTests.TestLoadObjectByOID;
var
  vSession: ISession;
  vObj: IAccount;
  vOID: IInfraType;
begin
  vSession := FSF.OpenSession;
  vOID := TInfraInteger.NewFrom(1);
  vObj := vSession.Load(IAccount, vOID) as IAccount;
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
