unit InfraConfigurationTests;

interface

uses SysUtils, Classes, TestFramework, InfraOPFIntf;

type
  TTestConfiguration = class(TTestCase)
  private
    FConfiguration: IConfiguration;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestPropertyItem;
    procedure TestGetValueAsInteger;
    procedure TestGetValueAsIntegerAgain;
    procedure TestGetValueAsIntegerNoKey;
    procedure TestGetValueAsIntegerInvalid;
    procedure TestGetValueAsDouble;
    procedure TestGetValueAsDoubleAgain;
    procedure TestGetValueAsDoubleNoKey;
    procedure TestGetValueAsDoubleInvalid;
    procedure TestGetValueAsString;
    procedure TestGetValueAsStringAgain;
    procedure TestGetValueAsStringNoKey;
  end;

implementation

uses
  InfraOPFConfiguration;

{ TTestConnectionProvider }

procedure TTestConfiguration.Setup;
begin
  inherited;
  FConfiguration := TConfiguration.Create;
end;

procedure TTestConfiguration.TearDown;
begin
  FConfiguration := nil; // Necessário para evitar AV
  inherited;
end;

procedure TTestConfiguration.TestGetValueAsInteger;
const
  vExpected = 10;
var
  vActual: Integer;
begin
  FConfiguration.SetValue('teste', vExpected);
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected);
end;

procedure TTestConfiguration.TestGetValueAsIntegerAgain;
const
  vExpected = 20;
var
  vActual: Integer;
begin
  FConfiguration.SetValue('teste', vExpected);
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected);
end;

procedure TTestConfiguration.TestGetValueAsIntegerInvalid;
const
  vExpected = -1;
var
  vActual: Integer;
begin
  FConfiguration.SetValue('teste', 'x');
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected);
end;

procedure TTestConfiguration.TestGetValueAsIntegerNoKey;
const
  vExpected = 25;
var
  vActual: Integer;
begin
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected);
end;

procedure TTestConfiguration.TestPropertyItem;
const
  sExpected01 = 'Valor1';
  sExpected02 = 'Valor2';
begin
  FConfiguration.SetValue('teste1', sExpected01);
  FConfiguration.SetValue('teste2', sExpected02);

  CheckEqualsString(sExpected01, FConfiguration.GetAsString('teste1'));
  CheckEqualsString(sExpected02, FConfiguration.GetAsString('teste2'));
end;

procedure TTestConfiguration.TestGetValueAsDouble;
const
  vExpected = 1.73;
var
  vActual: Double;
begin
  FConfiguration.SetValue('teste', vExpected);
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected, 0.00001);
end;

procedure TTestConfiguration.TestGetValueAsDoubleAgain;
const
  vExpected = 21.47;
var
  vActual: Double;
begin
  FConfiguration.SetValue('teste', vExpected);
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected, 0.00001);
end;

procedure TTestConfiguration.TestGetValueAsDoubleNoKey;
const
  vExpected = -1200.1234;
var
  vActual: Double;
begin
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected, 0.00001);
end;

procedure TTestConfiguration.TestGetValueAsDoubleInvalid;
const
  vExpected = 21.47;
var
  vActual: Double;
begin
  FConfiguration.SetValue('teste', 'invalido');
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected, 0.00001);
end;

procedure TTestConfiguration.TestGetValueAsString;
const
  vExpected = 'valor_Esperado';
var
  vActual: string;
begin
  FConfiguration.SetValue('teste', vExpected);
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEqualsString(vActual, vExpected);
end;

procedure TTestConfiguration.TestGetValueAsStringAgain;
const
  vExpected = 'Valor_Esperado';
var
  vActual: string;
begin
  FConfiguration.SetValue('teste', vExpected);
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEqualsString(vActual, vExpected);
end;

procedure TTestConfiguration.TestGetValueAsStringNoKey;
const
  vExpected = 'Valor_Esperado';
var
  vActual: string;
begin
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEqualsString(vActual, vExpected);
end;

initialization
  TestFramework.RegisterTest('Persistence Testes Caixa-Cinza',
    TTestConfiguration.Suite);
end.


