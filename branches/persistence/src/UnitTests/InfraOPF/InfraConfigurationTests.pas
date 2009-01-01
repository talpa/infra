unit InfraConfigurationTests;

interface

uses SysUtils, Classes, TestFramework, InfraPersistenceIntf;

type
  TTestConnectionProvider = class(TTestCase)
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
  InfraPersistence;

{ TTestConnectionProvider }

procedure TTestConnectionProvider.Setup;
begin
  inherited;
  FConfiguration := TConfiguration.Create;
end;

procedure TTestConnectionProvider.TearDown;
begin
  FConfiguration := nil; // Necessário para evitar AV
  inherited;
end;

procedure TTestConnectionProvider.TestGetValueAsInteger;
const
  vExpected = 10;
var
  vActual: Integer;
begin
  FConfiguration.SetValue('teste', vExpected);
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected);
end;

procedure TTestConnectionProvider.TestGetValueAsIntegerAgain;
const
  vExpected = 20;
var
  vActual: Integer;
begin
  FConfiguration.SetValue('teste', vExpected);
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected);
end;

procedure TTestConnectionProvider.TestGetValueAsIntegerInvalid;
const
  vExpected = -1;
var
  vActual: Integer;
begin
  FConfiguration.SetValue('teste', 'x');
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected);
end;

procedure TTestConnectionProvider.TestGetValueAsIntegerNoKey;
const
  vExpected = 25;
var
  vActual: Integer;
begin
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected);
end;

procedure TTestConnectionProvider.TestPropertyItem;
const
  sExpected01 = 'Valor1';
  sExpected02 = 'Valor2';
begin
  FConfiguration.SetValue('teste1', sExpected01);
  FConfiguration.SetValue('teste2', sExpected02);

  CheckEqualsString(sExpected01, FConfiguration.GetAsString('teste1'));
  CheckEqualsString(sExpected02, FConfiguration.GetAsString('teste2'));
end;

procedure TTestConnectionProvider.TestGetValueAsDouble;
const
  vExpected = 1.73;
var
  vActual: Double;
begin
  FConfiguration.SetValue('teste', vExpected);
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected, 0.00001);
end;

procedure TTestConnectionProvider.TestGetValueAsDoubleAgain;
const
  vExpected = 21.47;
var
  vActual: Double;
begin
  FConfiguration.SetValue('teste', vExpected);
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected, 0.00001);
end;

procedure TTestConnectionProvider.TestGetValueAsDoubleNoKey;
const
  vExpected = -1200.1234;
var
  vActual: Double;
begin
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected, 0.00001);
end;

procedure TTestConnectionProvider.TestGetValueAsDoubleInvalid;
const
  vExpected = 21.47;
var
  vActual: Double;
begin
  FConfiguration.SetValue('teste', 'invalido');
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEquals(vActual, vExpected, 0.00001);
end;

procedure TTestConnectionProvider.TestGetValueAsString;
const
  vExpected = 'valor_Esperado';
var
  vActual: string;
begin
  FConfiguration.SetValue('teste', vExpected);
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEqualsString(vActual, vExpected);
end;

procedure TTestConnectionProvider.TestGetValueAsStringAgain;
const
  vExpected = 'Valor_Esperado';
var
  vActual: string;
begin
  FConfiguration.SetValue('teste', vExpected);
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEqualsString(vActual, vExpected);
end;

procedure TTestConnectionProvider.TestGetValueAsStringNoKey;
const
  vExpected = 'Valor_Esperado';
var
  vActual: string;
begin
  vActual := FConfiguration.GetValue('teste', vExpected);
  CheckEqualsString(vActual, vExpected);
end;

initialization
  TestFramework.RegisterTest(TTestConnectionProvider.Suite);
end.

