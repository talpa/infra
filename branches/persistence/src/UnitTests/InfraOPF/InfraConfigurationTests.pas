unit InfraConfigurationTests;

interface

uses SysUtils, Classes, TestFramework, InfraOPFIntf;

type
  TTestConfiguration = class(TTestCase)
  private
    FConfiguration: IConfiguration;
    procedure FillConfig;
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
    procedure TestLoadFromFile;
    procedure TestSaveToFile;
    procedure TestLoadFromStream;
    procedure TestSaveToStream;
    procedure TestClone;
    procedure TestWriteXml;
    procedure TestReadXml;
  end;

implementation

uses
  InfraOPFConfiguration,
  InfraOPFConsts, Forms, XMLIntf, XMLDoc, InfraCommonIntf;

procedure TTestConfiguration.FillConfig;
begin
  FConfiguration.SetValue(InfraOPFConsts.cCONFIGKEY_MAXCONNECTIONS, 10);
  FConfiguration.SetValue(InfraOPFConsts.cCONFIGKEY_CONNECTIONTIME, 5000);
  FConfiguration.SetValue(InfraOPFConsts.cCONFIGKEY_DRIVER, 'firebird-2.0');
  FConfiguration.SetValue(InfraOPFConsts.cCONFIGKEY_HOSTNAME, 'localhost');
  FConfiguration.SetValue(InfraOPFConsts.cCONFIGKEY_PASSWORD, 'masterkey');
  FConfiguration.SetValue(InfraOPFConsts.cCONFIGKEY_USERNAME, 'sysdba');
  FConfiguration.SetValue(InfraOPFConsts.cCONFIGKEY_DATABASENAME, 'dbdemos.gdb');
  FConfiguration.SetValue(InfraOPFConsts.cCONFIGKEY_TEMPLATETYPE, 'TemplateReader_IO');
  FConfiguration.SetValue(InfraOPFConsts.cCONFIGKEY_TEMPLATEPATH, ExtractFilePath(Application.ExeName) + 'data');
  FConfiguration.SetValue(InfraOPFConsts.cCONFIGKEY_TEMPLATEEXT, 'tsql');
end;

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

procedure TTestConfiguration.TestLoadFromFile;
const
  cExpected = 'Pool.MaxConnections=10'#13#10+
    'Pool.TimeExpirationConnection=5000'#13#10+
    'Connection.Driver=firebird-2.0'#13#10+
    'Connection.HostName=localhost'#13#10+
    'Connection.Password=masterkey'#13#10+
    'Connection.UserName=sysdba'#13#10+
    'Connection.DatabaseName=dbdemos.gdb'#13#10+
    'Template.ClassType=TemplateReader_IO'#13#10+
    'Template.Path=D:\_working\infra\src\UnitTests\bin\data'#13#10+
    'Template.Ext=tsql'#13#10;

  cFileName = 'infra.conf';
var
  vStm: TFileStream;
begin
  vStm := TFileStream.Create(cFileName, fmCreate or fmShareDenyWrite);
  try
    vStm.WriteBuffer(PChar(cExpected)^, Length(cExpected));
  finally
    vStm.Free;
  end;

  FConfiguration.LoadFromFile(cFileName);

  CheckEquals(FConfiguration.GetAsInteger(InfraOPFConsts.cCONFIGKEY_MAXCONNECTIONS), 10);
  CheckEquals(FConfiguration.GetAsInteger(InfraOPFConsts.cCONFIGKEY_CONNECTIONTIME), 5000);
  CheckEqualsString('firebird-2.0', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_DRIVER));
  CheckEqualsString('localhost', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_HOSTNAME));
  CheckEqualsString('masterkey', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_PASSWORD));
  CheckEqualsString('sysdba', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_USERNAME));
  CheckEqualsString('dbdemos.gdb', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_DATABASENAME));
  CheckEqualsString('TemplateReader_IO', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_TEMPLATETYPE));
  CheckEqualsString(ExtractFilePath(Application.ExeName) + 'data', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_TEMPLATEPATH));
  CheckEqualsString('tsql', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_TEMPLATEEXT));

  if FileExists(cFileName) then
    DeleteFile(cFileName);
end;

procedure TTestConfiguration.TestSaveToFile;
const
  cExpected = 'Pool.MaxConnections=10'#13#10+
    'Pool.TimeExpirationConnection=5000'#13#10+
    'Connection.Driver=firebird-2.0'#13#10+
    'Connection.HostName=localhost'#13#10+
    'Connection.Password=masterkey'#13#10+
    'Connection.UserName=sysdba'#13#10+
    'Connection.DatabaseName=dbdemos.gdb'#13#10+
    'Template.ClassType=TemplateReader_IO'#13#10+
    'Template.Path=D:\_working\infra\src\UnitTests\bin\data'#13#10+
    'Template.Ext=tsql'#13#10;

  cFileName = 'infra.conf';
var
  vStm: TFileStream;
  vActual: string;
begin
  FillConfig;
  FConfiguration.SaveToFile(cFileName);

  vStm := TFileStream.Create(cFileName, fmOpenRead);
  try
    SetLength(vActual, vStm.Size);
    vStm.ReadBuffer(PChar(vActual)^, vStm.Size);
  finally
    vStm.Free;
  end;

  CheckEqualsString(cExpected, vActual, 'A informação não foi gravada corretamente');

  if FileExists(cFileName) then
    DeleteFile(cFileName);
end;

procedure TTestConfiguration.TestWriteXml;
const
  cExpected = '<?xml version="1.0" encoding="utf-8"?>'+
    '<TConfiguration>'+
      '<Pool.MaxConnections>10</Pool.MaxConnections>'+
      '<Pool.TimeExpirationConnection>5000</Pool.TimeExpirationConnection>'+
      '<Connection.Driver>firebird-2.0</Connection.Driver>'+
      '<Connection.HostName>localhost</Connection.HostName>'+
      '<Connection.Password>masterkey</Connection.Password>'+
      '<Connection.UserName>sysdba</Connection.UserName>'+
      '<Connection.DatabaseName>dbdemos.gdb</Connection.DatabaseName>'+
      '<Template.ClassType>TemplateReader_IO</Template.ClassType>'+
      '<Template.Path>D:\_working\infra\src\UnitTests\bin\data</Template.Path>'+
      '<Template.Ext>tsql</Template.Ext>'+
    '</TConfiguration>'#13#10;

var
  xmlDoc: IXmlDocument;
begin
  FillConfig;

  xmlDoc := TXMLDocument.Create(nil);
  (FConfiguration as IXmlSerializable).WriteXml(xmlDoc);
  CheckEqualsString(cExpected, xmlDoc.XML.Text);
end;

procedure TTestConfiguration.TestReadXml;
const
  cExpected = '<?xml version="1.0" encoding="utf-8"?>'+
    '<TConfiguration>'+
      '<Pool.MaxConnections>10</Pool.MaxConnections>'+
      '<Pool.TimeExpirationConnection>5000</Pool.TimeExpirationConnection>'+
      '<Connection.Driver>firebird-2.0</Connection.Driver>'+
      '<Connection.HostName>localhost</Connection.HostName>'+
      '<Connection.Password>masterkey</Connection.Password>'+
      '<Connection.UserName>sysdba</Connection.UserName>'+
      '<Connection.DatabaseName>dbdemos.gdb</Connection.DatabaseName>'+
      '<Template.ClassType>TemplateReader_IO</Template.ClassType>'+
      '<Template.Path>D:\_working\infra\src\UnitTests\bin\data</Template.Path>'+
      '<Template.Ext>tsql</Template.Ext>'+
    '</TConfiguration>'#13#10;

var
  xmlDoc: IXmlDocument;
begin
  xmlDoc := TXMLDocument.Create(nil);
  xmlDoc.XML.Text := cExpected;
  xmlDoc.Active := True;
  (FConfiguration as IXmlSerializable).ReadXml(xmlDoc);

  CheckEquals(FConfiguration.GetAsInteger(InfraOPFConsts.cCONFIGKEY_MAXCONNECTIONS), 10);
  CheckEquals(FConfiguration.GetAsInteger(InfraOPFConsts.cCONFIGKEY_CONNECTIONTIME), 5000);
  CheckEqualsString('firebird-2.0', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_DRIVER));
  CheckEqualsString('localhost', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_HOSTNAME));
  CheckEqualsString('masterkey', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_PASSWORD));
  CheckEqualsString('sysdba', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_USERNAME));
  CheckEqualsString('dbdemos.gdb', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_DATABASENAME));
  CheckEqualsString('TemplateReader_IO', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_TEMPLATETYPE));
  CheckEqualsString(ExtractFilePath(Application.ExeName) + 'data', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_TEMPLATEPATH));
  CheckEqualsString('tsql', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_TEMPLATEEXT));
end;

procedure TTestConfiguration.TestLoadFromStream;
const
  cExpected = 'Pool.MaxConnections=10'#13#10+
    'Pool.TimeExpirationConnection=5000'#13#10+
    'Connection.Driver=firebird-2.0'#13#10+
    'Connection.HostName=localhost'#13#10+
    'Connection.Password=masterkey'#13#10+
    'Connection.UserName=sysdba'#13#10+
    'Connection.DatabaseName=dbdemos.gdb'#13#10+
    'Template.ClassType=TemplateReader_IO'#13#10+
    'Template.Path=D:\_working\infra\src\UnitTests\bin\data'#13#10+
    'Template.Ext=tsql'#13#10;
var
  vStm: TMemoryStream;
begin
  vStm := TMemoryStream.Create;
  try
    vStm.WriteBuffer(PChar(cExpected)^, Length(cExpected));
    vStm.Seek(0, 0);
    FConfiguration.LoadFromStream(vStm);

    CheckEquals(FConfiguration.GetAsInteger(InfraOPFConsts.cCONFIGKEY_MAXCONNECTIONS), 10);
    CheckEquals(FConfiguration.GetAsInteger(InfraOPFConsts.cCONFIGKEY_CONNECTIONTIME), 5000);
    CheckEqualsString('firebird-2.0', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_DRIVER));
    CheckEqualsString('localhost', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_HOSTNAME));
    CheckEqualsString('masterkey', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_PASSWORD));
    CheckEqualsString('sysdba', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_USERNAME));
    CheckEqualsString('dbdemos.gdb', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_DATABASENAME));
    CheckEqualsString('TemplateReader_IO', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_TEMPLATETYPE));
    CheckEqualsString(ExtractFilePath(Application.ExeName) + 'data', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_TEMPLATEPATH));
    CheckEqualsString('tsql', FConfiguration.GetAsString(InfraOPFConsts.cCONFIGKEY_TEMPLATEEXT));
  finally
    vStm.Free;
  end;

end;

procedure TTestConfiguration.TestSaveToStream;
const
  cExpected = 'Pool.MaxConnections=10'#13#10+
    'Pool.TimeExpirationConnection=5000'#13#10+
    'Connection.Driver=firebird-2.0'#13#10+
    'Connection.HostName=localhost'#13#10+
    'Connection.Password=masterkey'#13#10+
    'Connection.UserName=sysdba'#13#10+
    'Connection.DatabaseName=dbdemos.gdb'#13#10+
    'Template.ClassType=TemplateReader_IO'#13#10+
    'Template.Path=D:\_working\infra\src\UnitTests\bin\data'#13#10+
    'Template.Ext=tsql'#13#10;

  cFileName = 'infra.conf';
var
  vStm: TMemoryStream;
  vActual: string;
begin
  FillConfig;

  vStm := TMemoryStream.Create;
  try
    FConfiguration.SaveToStream(vStm);

    vStm.Seek(0, 0);
    SetLength(vActual, vStm.Size);
    vStm.ReadBuffer(PChar(vActual)^, vStm.Size);

    CheckEqualsString(cExpected, vActual, 'A informação não foi gravada corretamente');
  finally
    vStm.Free;
  end;
end;

procedure TTestConfiguration.TestClone;
var
  vConfig: IConfiguration;
begin
  FillConfig;

  vConfig := FConfiguration.Clone;
  CheckEquals(vConfig.GetAsInteger(InfraOPFConsts.cCONFIGKEY_MAXCONNECTIONS), 10);
  CheckEquals(vConfig.GetAsInteger(InfraOPFConsts.cCONFIGKEY_CONNECTIONTIME), 5000);
  CheckEqualsString('firebird-2.0', vConfig.GetAsString(InfraOPFConsts.cCONFIGKEY_DRIVER));
  CheckEqualsString('localhost', vConfig.GetAsString(InfraOPFConsts.cCONFIGKEY_HOSTNAME));
  CheckEqualsString('masterkey', vConfig.GetAsString(InfraOPFConsts.cCONFIGKEY_PASSWORD));
  CheckEqualsString('sysdba', vConfig.GetAsString(InfraOPFConsts.cCONFIGKEY_USERNAME));
  CheckEqualsString('dbdemos.gdb', vConfig.GetAsString(InfraOPFConsts.cCONFIGKEY_DATABASENAME));
  CheckEqualsString('TemplateReader_IO', vConfig.GetAsString(InfraOPFConsts.cCONFIGKEY_TEMPLATETYPE));
  CheckEqualsString(ExtractFilePath(Application.ExeName) + 'data', vConfig.GetAsString(InfraOPFConsts.cCONFIGKEY_TEMPLATEPATH));
  CheckEqualsString('tsql', vConfig.GetAsString(InfraOPFConsts.cCONFIGKEY_TEMPLATEEXT));
end;

initialization
  TestFramework.RegisterTest('Persistence Testes Caixa-Cinza',
    TTestConfiguration.Suite);
end.


