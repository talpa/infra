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
    FParser: IParseParams;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParserSelect;
    procedure TestParserInsert;
    procedure TestParserUpdate;
    procedure TestParserDelete;
    procedure TestParserParamsRepeated;
    procedure TestParserParamNoName;
    procedure TestParserMacroNoName;
    procedure TestParserMacrosAndParams;
  end;

implementation

{ TTestParseParams }

procedure TTestParseParams.TestParserInsert;
const
  cSql = 'insert into tabela (codigo, nome) values (:codigo, :nome)';
var
  vParams: TStrings;
begin
  FParser.Parse(cSql);
  vParams := FParser.GetParams;
  CheckEquals(2, vParams.Count);
  CheckEqualsString('codigo', vParams[0]);
  CheckEqualsString('nome', vParams[1]);
end;

procedure TTestParseParams.TestParserSelect;
const
  cSql = 'select * from tabela where codigo = :codigo and nome = :nome';
var
  vParams: TStrings;
begin
  FParser.Parse(cSql);
  vParams := FParser.GetParams;
  CheckEquals(2, vParams.Count);
  CheckEqualsString('codigo', vParams[0]);
  CheckEqualsString('nome', vParams[1]);
end;

procedure TTestParseParams.TestParserUpdate;
const
  cSql = 'update tabela set codigo = :codigo, nome = :nome where id = :id';
var
  vParams: TStrings;
begin
  FParser.Parse(cSql);
  vParams := FParser.GetParams;
  CheckEquals(3, vParams.Count);
  CheckEqualsString('codigo', vParams[0]);
  CheckEqualsString('nome', vParams[1]);
  CheckEqualsString('id', vParams[2]);
end;

procedure TTestParseParams.TestParserDelete;
const
  cSql = 'delete from tabela where codigo = :codigo and nome = :nome and id = :id';
var
  vParams: TStrings;
begin
  FParser.Parse(cSql);
  vParams := FParser.GetParams;
  CheckEquals(3, vParams.Count);
  CheckEqualsString('codigo', vParams[0]);
  CheckEqualsString('nome', vParams[1]);
  CheckEqualsString('id', vParams[2]);
end;

procedure TTestParseParams.TestParserParamsRepeated;
const
  cSql = 'update tabela set id = :id, codigo = :codigo, nome = :nome where id = :id';
var
  vParams: TStrings;
begin
  FParser.Parse(cSql);
  vParams := FParser.GetParams;
  CheckEquals(4, vParams.Count);
  CheckEqualsString('id', vParams[0]);
  CheckEqualsString('codigo', vParams[1]);
  CheckEqualsString('nome', vParams[2]);
  CheckEqualsString('id', vParams[3]);
end;

procedure TTestParseParams.TestParserParamNoName;
const
  cSql = 'update tabela set id = :id, codigo = :codigo, nome = :nome where id = :';
begin
  ExpectedException := EInfraParserError;
  FParser.Parse(cSql);
  ExpectedException := nil;
end;

procedure TTestParseParams.TestParserMacrosAndParams;
const
  cSql = 'select #macro1 from #macro2 where id = :id or codigo = :codigo or campo3 = ::campo3 order by #macro3 ##macro4';
var
  vParams: TStrings;
  vMacros: TStrings;
begin
  FParser.Parse(cSql);
  vParams := FParser.GetParams;
  CheckEquals(2, vParams.Count);
  CheckEqualsString('id', vParams[0]);
  CheckEqualsString('codigo', vParams[1]);

  vMacros := FParser.GetMacroParams;
  CheckEquals(3, vMacros.Count);
  CheckEqualsString('macro1', vMacros[0]);
  CheckEqualsString('macro2', vMacros[1]);
  CheckEqualsString('macro3', vMacros[2]);
end;

procedure TTestParseParams.TestParserMacroNoName;
const
  cSql = 'update # set codigo = :codigo, nome = :nome where id = :id';
begin
  ExpectedException := EInfraParserError;
  FParser.Parse(cSql);
  ExpectedException := nil;
end;

procedure TTestParseParams.SetUp;
begin
  inherited;
  FParser := TParseParams.Create;
end;

procedure TTestParseParams.TearDown;
begin
  FParser := nil;
  inherited;
end;

initialization
  TestFramework.RegisterTest(TTestParseParams.Suite);

end.
