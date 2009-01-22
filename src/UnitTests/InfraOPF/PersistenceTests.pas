unit PersistenceTests;

interface

uses
  InfraOPFIntf,
  ZDbcIntfs,
  TestFramework,
  PersistenceModelIntf;

type
  TPersistenceTests = class(TTestCase)
  private
    FSessionFactory: ISessionFactory;
    procedure DoLoad(pSession: ISession);
    procedure PrepararBancoParaCarga;
    procedure PrepararBancoParaDeletar;
    procedure PrepararBancoParaInserir;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadWithObject;
    procedure TestLoadWithParams;
    procedure TestSaveInsertWithObject;
    procedure TestDeleteWithObject;
    procedure TestLoadWithObjectInTransaction;
  end;

implementation

uses
  Dialogs,
  SysUtils,
  Forms,
  Math,
  Classes,
  InfraValueType,
  InfraValueTypeIntf,
  PersistenceModel,
  InfraOPFConsts,
  InfraTestsUtil, TestExtensions;

// START resource string wizard section
resourcestring
  cObjetoNaoFoiCarregado = 'Objeto não foi carregado';
  cNomeContaIncompativel = 'Nome conta incompatível';
  cNumeroDaContaIncompativel = 'Número da conta incompatível';
  cSaldoInicialIncompativel = 'Saldo inicial incompatível';
  cSaldoAtualIncompativel = 'Saldo atual incompatível';
  cQuantidadeDeRegistrosAfetadosInv = 'Quantidade de registros afetados inválida';

// END resource string wizard section


const
  cSQLDeleteFromAccount = 'DELETE FROM ACCOUNT';
  cLoadAccountByIDTemplateName = 'LoadAccountById';
  cDeleteAccountByIDTemplateName = 'DeleteAccountByID';

const
  cEpsilon = 0.00001; /// Maior diferença entre dois números e ainda serem considerados iguais
  cNumVezesRepetirTeste = 10; /// Número de vezes que os testes serão repetidos

{ TPersistenceTests }

procedure TPersistenceTests.SetUp;
var
  cfg: IConfiguration;
begin
  inherited;
  // Aqui é definido no Configuration algumas propriedades, para que o
  // InfraPersistence saiba algumas coisas necessárias para configurar o
  // connection e outras coisas internas. Caso haja propriedades especificas
  // que precisam ser definidas, podem ser naturalmente colocados ai tambem
  // que o tipo de Connection específico poderar ler sem problema.
  cfg := PersistenceService.GetConfiguration;
  with cfg do
  begin
    SetValue(cCONFIGKEY_DRIVER, 'firebird-2.0');
    SetValue(cCONFIGKEY_USERNAME, 'SYSDBA');
    SetValue(cCONFIGKEY_PASSWORD, 'masterkey');
    SetValue(cCONFIGKEY_HOSTNAME, 'localhost');
    SetValue(cCONFIGKEY_DATABASENAME,
      ExtractFilePath(Application.ExeName) + 'data\dbdemos.fdb');
    SetValue(cCONFIGKEY_TEMPLATETYPE, 'TemplateReader_IO');
    SetValue(cCONFIGKEY_TEMPLATEPATH,
      ExtractFilePath(Application.ExeName) + 'Data');
  end;

  FSessionFactory := cfg.BuildSessionFactory;

  // Prepara o DBUnit para deixar o banco no estado adequado antes de testar.
  GetZeosExecutor.OpenConnection('zdbc:firebird-2.0://localhost/' +
    ExtractFilePath(Application.ExeName) + 'data\dbdemos.fdb' +
    '?username=SYSDBA;password=masterkey');
end;

procedure TPersistenceTests.PrepararBancoParaCarga;
begin
  GetZeosExecutor.Execute(cSQLDeleteFromAccount);
  GetZeosExecutor.Execute(
    'INSERT INTO ACCOUNT (ID, ACCOUNTNUMBER, ACCOUNTNAME, '+
    'INITIALBALANCE, CURRENTBALANCE) VALUES (1, ''1361-2'', ''BB 1361'', '+
    '125.3, 1524.25)');
end;

procedure TPersistenceTests.PrepararBancoParaInserir;
begin
  GetZeosExecutor.Execute(cSQLDeleteFromAccount);
end;

procedure TPersistenceTests.PrepararBancoParaDeletar;
begin
  GetZeosExecutor.Execute(cSQLDeleteFromAccount);
  GetZeosExecutor.Execute(
    'INSERT INTO ACCOUNT (ID, ACCOUNTNUMBER, ACCOUNTNAME, '+
    'INITIALBALANCE, CURRENTBALANCE) VALUES (3, ''1111-3'', ''CEF 1111'', '+
    'NULL, NULL)');
end;

procedure TPersistenceTests.TestLoadWithObject;
var
  vSession: ISession;
begin
  PrepararBancoParaCarga;
  // abre uma nova sessão e cria um objeto preenchendo apenas as propriedades
  // que irão servir de parâmetro para a busca
  vSession := FSessionFactory.OpenSession;

  DoLoad(vSession);
  // *** verificar estado do objeto
end;

procedure TPersistenceTests.TestLoadWithParams;
var
  vSession: ISession;
  vObj: IAccount;
  vSQLCommand: ISQLCommandQuery;
begin
  PrepararBancoParaCarga;

  // Abre a sessao e define o parametro e o tipo de classe a ser carregada.
  vSession := FSessionFactory.OpenSession;
  vSQLCommand := vSession.CreateNamedQuery(cLoadAccountByIDTemplateName,IAccount);
  vSQLCommand.Params['Id'] := TInfraInteger.NewFrom(1);
  // Executa a carga do objeto
  vObj := vSQLCommand.GetResult as IAccount;

  CheckNotNull(vObj, cObjetoNaoFoiCarregado);
  CheckEqualsString('BB 1361', vObj.Name.AsString, cNomeContaIncompativel);
  CheckEqualsString('1361-2', vObj.AccountNumber.AsString, cNumeroDaContaIncompativel);
  CheckEquals(125.3, vObj.InitialBalance.AsDouble, cEpsilon, cSaldoInicialIncompativel);
  CheckEquals(1524.25, vObj.CurrentBalance.AsDouble, cEpsilon, cSaldoAtualIncompativel);
  // *** verificar estado do objeto
end;

procedure TPersistenceTests.TestSaveInsertWithObject;
var
  vSession: ISession;
  vObj: IAccount;
  vCont: integer;
  vSQLCommand :ISQLCommandQuery;
begin
  PrepararBancoParaInserir;
  vSession := FSessionFactory.OpenSession;
  vObj := TAccount.Create;
  vObj.Id.AsInteger := 2;
  vObj.AccountNumber.AsString := '2812-3';
  vObj.InitialBalance.AsDouble := 789.3;
  vObj.CurrentBalance.AsDouble := 222.25;
  // *** Deveria testar aqui o estado do objeto deveria estar Clear e not Persistent
  vSession.Save('InsertAccount', vObj);
  vCont := vSession.Flush;
  // *** Deveria testar aqui o estado do objeto deveria estar Clear e Persistent
  CheckEquals(1, vCont, cQuantidadeDeRegistrosAfetadosInv);

  vSQLCommand := vSession.CreateNamedQuery(cLoadAccountByIDTemplateName, vObj);
  vObj := vSQLCommand.GetResult as IAccount;

  CheckEqualsString('2812-3', vObj.AccountNumber.AsString, cNumeroDaContaIncompativel);
  CheckEquals(789.3, vObj.InitialBalance.AsDouble, cEpsilon, cSaldoInicialIncompativel);
  CheckEquals(222.25, vObj.CurrentBalance.AsDouble, cEpsilon, cSaldoAtualIncompativel);
end;

procedure TPersistenceTests.TestDeleteWithObject;
var
  vSession: ISession;
  vObj: IAccount;
  vCont :integer;
begin
  PrepararBancoParaDeletar;

  // Abre a sessao, cria um objeto e define o id a ser deletado
  vSession := FSessionFactory.OpenSession;
  vObj := TAccount.Create;
  vObj.Id.AsInteger := 3;
  vSession.Delete(cDeleteAccountByIDTemplateName, vObj);
  vCont := vSession.Flush;

  // *** Deveria testar aqui o estado do objeto deveria estar Deleted e Persistent
  // *** pegar um resultset e verificar se o dado foi realmente apagado
  CheckEquals(1, vCont, cQuantidadeDeRegistrosAfetadosInv);
end;

procedure TPersistenceTests.TearDown;
begin
  inherited;
  FSessionFactory := nil;
  ReleaseZeosExecutor;
end;

function UnitTests: ITest;
begin
  Result := TRepeatedTest.Create(TPersistenceTests.Suite, cNumVezesRepetirTeste);
end;

procedure TPersistenceTests.DoLoad(pSession: ISession);
var
  vObj: IAccount;
  vSQLCommand: ISQLCommandQuery;
begin
  vObj := TAccount.Create;
  vObj.Id.AsInteger := 1;
  // *** verificar estado do objeto
  // Prepara a carga, definindo o objeto como parâmetro
  vSQLCommand := pSession.CreateNamedQuery(cLoadAccountByIDTemplateName, vObj);
  // Executa a carga do objeto
  vObj := vSQLCommand.GetResult as IAccount;

  CheckNotNull(vObj, cObjetoNaoFoiCarregado);
  CheckEqualsString('BB 1361', vObj.Name.AsString, cNomeContaIncompativel);
  CheckEqualsString('1361-2', vObj.AccountNumber.AsString, cNumeroDaContaIncompativel);
  CheckEquals(125.3, vObj.InitialBalance.AsDouble, cEpsilon, cSaldoInicialIncompativel);
  CheckEquals(1524.25, vObj.CurrentBalance.AsDouble, cEpsilon, cSaldoAtualIncompativel);
end;

procedure TPersistenceTests.TestLoadWithObjectInTransaction;
var
  vSession: ISession;
begin
  PrepararBancoParaCarga;
  // abre uma nova sessão e cria um objeto preenchendo apenas as propriedades
  // que irão servir de parâmetro para a busca
  vSession := FSessionFactory.OpenSession;

  vSession.BeginTransaction;
  try
    DoLoad(vSession);

    vSession.Commit;
  except
    vSession.Rollback;
    raise;
  end;
  // *** verificar estado do objeto
end;

initialization
  TestFramework.RegisterTest('Persistence Testes Caixa-Preta', UnitTests);

end.

