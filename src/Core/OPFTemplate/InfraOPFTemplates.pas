unit InfraOPFTemplates;

interface

uses
  SysUtils,
  Classes,
  InfraCommon,
  InfraCommonIntf,
  InfraOPFIntf;

type
  /// Descrição da classe
  TTemplateReader = class(TElement, ITemplateReader)
  private
    FConfiguration: IConfiguration;
  protected
    function ReadFromStream(pStream: TStream): string;
    function Read(const pSQLCommand: ISQLCommand): string; virtual; abstract;
    function GetConfiguration: IConfiguration;
    procedure SetConfiguration(const Value: IConfiguration);
    property Configuration: IConfiguration read GetConfiguration
      write SetConfiguration;
  public
    constructor Create; reintroduce;
  end;

  TTemplateReader_IO = class(TTemplateReader, ITemplateReader)
  protected
    function GetFilename(const pTemplateName: string): string;
    function Read(const pSQLCommand: ISQLCommand): string; override;
    function ReadFileName(const pTemplateName: string): string;
  end;

  TTemplateReader_Build = class(TTemplateReader, ITemplateReader_Build)
  protected
    function Read(const pSQLCommand: ISQLCommand): string; override;
  end;

procedure RegisterOnReflection;

implementation

uses
  InfraOPFConsts;

{ TTemplateReader }

{**
  Contructor de TTemplateReader. Por ser uma classe abstrata, não pode ser instanciada
}
constructor TTemplateReader.Create;
begin
  raise EPersistenceTemplateError.Create(cErrorTemplateTryCreateClassBase);
end;

{**

  @return ResultDescription
}
function TTemplateReader.GetConfiguration: IConfiguration;
begin
  Result := FConfiguration;
end;

{**

  @param Value   ParameterDescription
}
procedure TTemplateReader.SetConfiguration(const Value: IConfiguration);
begin
  FConfiguration := Value;
end;

{**
  Lê o template de um Stream
  @param pStream Stream do qual o Reader efetuará a leitura do Template
  @return Retorna o conteúdo do template no formato de string 
}
function TTemplateReader.ReadFromStream(pStream: TStream): string;
begin
  pStream.Position := 0;
  SetLength(Result, pStream.Size);
  pStream.ReadBuffer(PChar(Result)^, pStream.Size);
end;

{ TTemplateReader }

function TTemplateReader_IO.GetFilename(const pTemplateName: string): string;
begin
  with Configuration do
    Result := IncludeTrailingPathDelimiter(
      GetValue(cCONFIGKEY_TEMPLATEPATH, ExtractFilePath(ParamStr(0))))+
      pTemplateName+'.'+
      GetValue(cCONFIGKEY_TEMPLATEEXT, 'sql');
end;

function TTemplateReader_IO.ReadFileName(
  const pTemplateName: string): string;
var
  vStream: TFileStream;
begin
  vStream := TFileStream.Create(pTemplateName, fmOpenRead or fmShareDenyWrite);
  try
    Result := ReadFromStream(vStream);
  finally
    vStream.Free;
  end;
end;

function TTemplateReader_IO.Read(const pSQLCommand: ISQLCommand): string;
var
  vFileName: string;
begin
  // *** tem de gerar uma exceção caso o arquivo nao exista, acho que tem de
  // *** procurar o arquivo com findfirst ou tratar o Result = Emptystr.
  vFileName := GetFilename(pSQLCommand.Name);
  Result := ReadFileName(vFileName);
end;

procedure RegisterOnReflection;
begin
  with TypeService do
  begin
    AddType(ITemplateReader, 'TemplateReader', TTemplateReader, IElement);
    AddType(ITemplateReader_IO, 'TemplateReader_IO', TTemplateReader_IO, ITemplateReader);
  end;
end;

{ TTemplateReader_Build }

function TTemplateReader_Build.Read(const pSQLCommand: ISQLCommand): string;
begin

end;

end.

