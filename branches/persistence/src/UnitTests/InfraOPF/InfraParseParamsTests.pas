{
  1) Acho que nao precisa de um teste para cada instrução pois nao é um parser
     de sqls e sim de parametros existente em uma string.

  2) Deve testar a questão de parâmetros macro exemplo:
     SQL = 'Select #Macro1 from teste where x = :Param1'
     Isso deveria retornar 1 item em GetParams e 1 item em GetMacroParams

  3) Deve testar a questão de falsos parâmetros exemplo:
     SQL = 'Select ##Macro1 from teste'
     SQL = 'Select #:Macro1 from teste'
     SQL = 'Select ::Macro1 from teste'
     SQL = 'Select :#Macro1 from teste'
     Estes nao deveriam ser considerados parâmetros pelo que está implementado
}
unit InfraParseParamsTests;

interface

uses
  SysUtils,
  Classes,
  InfraPersistence,
  InfraPersistenceIntf,
  TestFramework;

type
  TTestParseParams = class(TTestCase)
  private
  published
    procedure TestParserSelect;
    procedure TestParserInsert;
    procedure TestParserUpdate;
    procedure TestParserDelete;
    procedure TestParserParamsRepeated;
    procedure TestParserParamNoName;
    procedure TestParserMacroNoName;
    procedure TestParserParamInvalid1;
    procedure TestParserParamInvalid2;
    procedure TestParserMacroInvalid1;
    procedure TestParserMacroInvalid2;
    procedure TestParserMacrosAndParams;
  end;

implementation

{ TTestParseParams }

procedure TTestParseParams.TestParserInsert;
const
  cSql = 'insert into tabela (codigo, nome) values (:codigo, :nome)';
var
  vParser: IParseParams;
  vParams: TStrings;
begin
  vParser := TParseParams.Create;
  vParser.Parse(cSql);
  vParams := vParser.GetParams;
  CheckEquals(2, vParams.Count);
  CheckEqualsString('codigo', vParams[0]);
  CheckEqualsString('nome', vParams[1]);
end;

procedure TTestParseParams.TestParserSelect;
const
  cSql = 'select * from tabela where codigo = :codigo and nome = :nome';
var
  vParser: IParseParams;
  vParams: TStrings;
begin
  vParser := TParseParams.Create;
  vParser.Parse(cSql);
  vParams := vParser.GetParams;
  CheckEquals(2, vParams.Count);
  CheckEqualsString('codigo', vParams[0]);
  CheckEqualsString('nome', vParams[1]);
end;

procedure TTestParseParams.TestParserUpdate;
const
  cSql = 'update tabela set codigo = :codigo, nome = :nome where id = :id';
var
  vParser: IParseParams;
  vParams: TStrings;
begin
  vParser := TParseParams.Create;
  vParser.Parse(cSql);
  vParams := vParser.GetParams;
  CheckEquals(3, vParams.Count);
  CheckEqualsString('codigo', vParams[0]);
  CheckEqualsString('nome', vParams[1]);
  CheckEqualsString('id', vParams[2]);
end;

procedure TTestParseParams.TestParserDelete;
const
  cSql = 'delete from tabela where codigo = :codigo and nome = :nome and id = :id';
var
  vParser: IParseParams;
  vParams: TStrings;
begin
  vParser := TParseParams.Create;
  vParser.Parse(cSql);
  vParams := vParser.GetParams;
  CheckEquals(3, vParams.Count);
  CheckEqualsString('codigo', vParams[0]);
  CheckEqualsString('nome', vParams[1]);
  CheckEqualsString('id', vParams[2]);
end;

procedure TTestParseParams.TestParserParamsRepeated;
const
  cSql = 'update tabela set id = :id, codigo = :codigo, nome = :nome where id = :id';
var
  vParser: IParseParams;
  vParams: TStrings;
begin
  vParser := TParseParams.Create;
  vParser.Parse(cSql);
  vParams := vParser.GetParams;
  CheckEquals(4, vParams.Count);
  CheckEqualsString('id', vParams[0]);
  CheckEqualsString('codigo', vParams[1]);
  CheckEqualsString('nome', vParams[2]);
  CheckEqualsString('id', vParams[3]);
end;

procedure TTestParseParams.TestParserParamNoName;
const
  cSql = 'update tabela set id = :id, codigo = :codigo, nome = :nome where id = :';
var
  vParser: IParseParams;
begin
  vParser := TParseParams.Create;
  ExpectedException := EInfraParserError;
  vParser.Parse(cSql);
  ExpectedException := nil;
end;

procedure TTestParseParams.TestParserMacrosAndParams;
const
  cSql = 'select #macro1 from #macro2 where id = :id or codigo = :codigo order by #macro3';
var
  vParser: IParseParams;
  vParams: TStrings;
  vMacros: TStrings;
begin
  vParser := TParseParams.Create;
  vParser.Parse(cSql);
  vParams := vParser.GetParams;
  CheckEquals(2, vParams.Count);
  CheckEqualsString('id', vParams[0]);
  CheckEqualsString('codigo', vParams[1]);

  vMacros := vParser.GetMacroParams;
  CheckEquals(3, vMacros.Count);
  CheckEqualsString('macro1', vMacros[0]);
  CheckEqualsString('macro2', vMacros[1]);
  CheckEqualsString('macro3', vMacros[2]);
end;

procedure TTestParseParams.TestParserMacroNoName;
const
  cSql = 'update # set codigo = :codigo, nome = :nome where id = :id';
var
  vParser: IParseParams;
begin
  vParser := TParseParams.Create;
  ExpectedException := EInfraParserError;
  vParser.Parse(cSql);
  ExpectedException := nil;
end;

procedure TTestParseParams.TestParserMacroInvalid1;
var
  vParser: IParseParams;
begin
  vParser := TParseParams.Create;
  ExpectedException := EInfraParserError;
  vParser.Parse('select * from tabela where ##');
  ExpectedException := nil;
end;

procedure TTestParseParams.TestParserMacroInvalid2;
var
  vParser: IParseParams;
begin
  vParser := TParseParams.Create;
  ExpectedException := EInfraParserError;
  vParser.Parse('select * from tabela where #.');
  ExpectedException := nil;
end;

procedure TTestParseParams.TestParserParamInvalid1;
var
  vParser: IParseParams;
begin
  vParser := TParseParams.Create;
  ExpectedException := EInfraParserError;
  vParser.Parse('select * from tabela where id = :.');
  ExpectedException := nil;
end;

procedure TTestParseParams.TestParserParamInvalid2;
var
  vParser: IParseParams;
begin
  vParser := TParseParams.Create;
  ExpectedException := EInfraParserError;
  vParser.Parse('select * from tabela where id = ::');
  ExpectedException := nil;
end;

initialization
  TestFramework.RegisterTest(TTestParseParams.Suite);

end.
