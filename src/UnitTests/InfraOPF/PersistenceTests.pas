unit PersistenceTests;

interface

uses
  InfraPersistenceIntf,
  ZDbcInterbase6,
  TestFramework,
  PersistenceModelIntf;

type
  TPersistenceTests = class(TTestCase)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadWithObject;
    procedure TestLoadWithParams;
    procedure TestSaveWithObject;
    procedure TestDeleteWithObject;
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
  InfraPersistence,
  InfraPersistenceConsts;

{ THibernateTests }

procedure TPersistenceTests.SetUp;
begin
  inherited;
  // Aqui é definido no Configuration algumas propriedades, para que o
  // InfraPersistence saiba algumas coisas necessárias para configurar o
  // connection e outras coisas internas. Caso haja propriedades especificas
  // que precisam ser definidas, podem ser naturalmente colocados ai tambem
  // que o tipo de Connection específico poderar ler sem problema.
  with PersistenceService.Configuration do
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
end;

procedure TPersistenceTests.TearDown;
begin
  inherited;
  
end;

procedure TPersistenceTests.TestDeleteWithObject;
var
  vSession: ISession;
  vObj: IAccount;
  vCont :integer;
begin
  vSession := PersistenceService.OpenSession;
  vObj := TAccount.Create;
  vObj.Id.AsInteger := 2 ;
  vSession.Delete('DeleteAccountByID', vObj);
  vCont:=vSession.Flush;
  CheckEquals(1, vCont, 'Não foi possivel apagar o registro');
end;

procedure TPersistenceTests.TestSaveWithObject;
var
  vSession: ISession;
  vObj: IAccount;
  vCont :integer;
begin
  vSession := PersistenceService.OpenSession;
  vObj := TAccount.Create;
  vObj.Id.AsInteger := 2;
  vObj.AccountNumber.AsString:='1361-2';
  vObj.InitialBalance.AsDouble:=125.3;
  vObj.CurrentBalance.AsDouble:=1524.25;
  vSession.Save('SaveAccount', vObj);
  vCont:=vSession.Flush;
  CheckEquals(1, vCont, 'Não foi possivel salvar o registro');
end;

procedure TPersistenceTests.TestLoadWithObject;
var
  vSession: ISession;
  vObj: IAccount;
  vSQLCommand: ISQLCommandQuery;
begin
  // *** Acho que aqui deveria fazer algo para deixar a aplicação num estado
  // *** apto a este teste.

  // abre uma nova sessão e cria um objeto preenchendo apenas as propriedades
  // que irão servir de parâmetro para a busca
  vSession := PersistenceService.OpenSession;
  vObj := TAccount.Create;
  vObj.Id.AsInteger := 1;

  // Prepara a carga, definindo um objeto como parâmetro
  vSQLCommand := vSession.Load('LoadAccountbyId', vObj);

  // Executa a carga do objeto
  vObj := vSQLCommand.GetResult as IAccount;

  // verifica se o objeto realmente foi carregado.
  CheckNotNull(vObj, 'Objecto não foi carregado');
  CheckEquals('BB 1361', vObj.Name.AsString, 'Nome conta incompatível');
  CheckEquals('1361-2', vObj.AccountNumber.AsString, 'Número da conta incompatível');
  CheckTrue(SameValue(125.3, vObj.InitialBalance.AsDouble), 'Saldo inicial incompatível');
  CheckTrue(SameValue(1524.25, vObj.CurrentBalance.AsDouble), 'Saldo atual incompatível');
end;

procedure TPersistenceTests.TestLoadWithParams;
var
  vSession: ISession;
  vObj: IAccount;
  vSQLCommand: ISQLCommandQuery;
begin
  // *** Acho que aqui deveria fazer algo para deixar a aplicação num estado
  // *** apto a este teste.

  // abre uma nova sessão e cria um objeto preenchendo apenas as propriedades
  // que irão servir de parâmetro para a busca
  vSession := PersistenceService.OpenSession;

  // Prepara a carga, definindo um parâmetro comum.
  vSQLCommand := vSession.Load('LoadAccountbyId');
  vSQLCommand.ClassID := IAccount;
  vSQLCommand.Params['Id'] := TInfraInteger.NewFrom(1);

  // Executa a carga do objeto
  vObj := vSQLCommand.GetResult as IAccount;

  // verifica se o objeto realmente foi carregado.
  CheckNotNull(vObj, 'Objecto não foi carregado');
  CheckEquals('BB 1361', vObj.Name.AsString, 'Nome conta incompatível');
  CheckEquals('1361-2', vObj.AccountNumber.AsString, 'Número da conta incompatível');
  CheckTrue(SameValue(125.3, vObj.InitialBalance.AsDouble), 'Saldo inicial incompatível');
  CheckTrue(SameValue(1524.25, vObj.CurrentBalance.AsDouble), 'Saldo atual incompatível');
end;

initialization
  TestFramework.RegisterTest('Persistence TestsSuite', TPersistenceTests.Suite);

end.
