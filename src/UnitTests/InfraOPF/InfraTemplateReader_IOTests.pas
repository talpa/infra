unit InfraTemplateReader_IOTests;

interface

uses
  SysUtils,
  TestFramework,
  InfraCommonIntf,
  InfraPersistenceIntf,
  TemplateReader_IO;

type
  TTemplateReaderHacked = class(TTemplateReader_IO)
  end;

  TTestTemplateReader_IO = class(TTestCase)
  private
    FReader: TTemplateReaderHacked;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateWithInvalidArgs;
    procedure TestGetFileNameWithDefaults;
    procedure TestGetFileNameConfig;
    procedure TestReadFileNotExists;
    procedure TestRead;
  end;

implementation

uses
  InfraTestsUtil, InfraPersistenceConsts, Windows, Classes;

{ TTestTemplateReader_IO }

procedure TTestTemplateReader_IO.SetUp;
begin
  inherited;
  FReader := TTemplateReaderHacked.Create;
  FReader.Configuration := TTestsUtil.GetNewConfiguration;
end;

procedure TTestTemplateReader_IO.TearDown;
begin
  FReader := nil;
  inherited;
end;

procedure TTestTemplateReader_IO.TestCreateWithInvalidArgs;
begin
//  ExpectedException := EInfraArgumentError;
//  TTemplateReader_IO.Create;
//  ExpectedException := nil;
end;

procedure TTestTemplateReader_IO.TestGetFileNameWithDefaults;
var
  vConfiguration: IConfiguration;
  vReader: TTemplateReaderHacked;
  vExpected: string;
  vActual: string;
begin
  vConfiguration := TTestsUtil.GetNewConfiguration;
  vReader := TTemplateReaderHacked.Create;
  vReader.Configuration := vConfiguration;
  vExpected := ExtractFilePath(ParamStr(0))+'produtos.sql';
  vActual := vReader.GetFilename('produtos');
  CheckEqualsString(vExpected, vActual, 'GetFileName falhou');
end;

procedure TTestTemplateReader_IO.TestGetFileNameConfig;
var
  vConfiguration: IConfiguration;
  vReader: TTemplateReaderHacked;
  vExpected: string;
  vActual: string;
begin
  vConfiguration := TTestsUtil.GetNewConfiguration;
  vConfiguration.SetValue(cCONFIGKEY_TEMPLATEPATH, 'c:\');
  vConfiguration.SetValue(cCONFIGKEY_TEMPLATEEXT, 'tpl');
  vReader := TTemplateReaderHacked.Create;
  vReader.Configuration := vConfiguration;
  vExpected := 'c:\produtos.tpl';
  vActual := vReader.GetFilename('produtos');
  CheckEqualsString(vExpected, vActual, 'GetFileName falhou');
end;

procedure TTestTemplateReader_IO.TestReadFileNotExists;
var
  FileName: array [0..MAX_PATH] of Char;
begin
  GetTempFileName('.\','tst',0,FileName);
  try
    ExpectedException := EFOpenError;
    FReader.Read(FileName);
    ExpectedException := nil;
  finally
    if FileExists(FileName) then
      DeleteFile(FileName);
  end;
end;

procedure TTestTemplateReader_IO.TestRead;
var
  FileName: string;
  stm: TFileStream;
  vExpected: string;
  vActual: string;
  vFullFileName: string;
begin
  FileName := 'teste_template_reader';

  vExpected := 'select *'#13#10+
    'from produtos'+#13#10+
    'where codigo=:codigo'#13#10;

  vFullFileName := FReader.GetFilename(FileName);

  stm := TFileStream.Create(vFullFileName, fmCreate);
  try
    stm.Write(PChar(vExpected)^, Length(vExpected));
  finally
    stm.Free;
  end;

  try
    vActual := FReader.Read(FileName);
  finally
    DeleteFile(PChar(vFullFileName));
  end;

  CheckEqualsString(vExpected, vActual, 'Read falhou: O conteúdo do arquivo difere');
end;

initialization
  TestFramework.RegisterTest('Persistence Testes Caixa-Cinza',
    TTestTemplateReader_IO.Suite);
end.
