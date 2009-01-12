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
    function ReadFromStream(const pStream: TStream): string; 
    function Read(const pTemplateName: string): string; virtual; abstract;
    function GetConfiguration: IConfiguration;
    procedure SetConfiguration(const Value: IConfiguration);
    property Configuration: IConfiguration read GetConfiguration
      write SetConfiguration;
  public
    constructor Create; reintroduce; virtual;
  end;

  TTemplateReader_IO = class(TTemplateReader, ITemplateReader)
  protected
    function GetFilename(const pTemplateName: string): string;
    function Read(const pTemplateName: string): string; override;
  public
    constructor Create; override;
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
function TTemplateReader.ReadFromStream(const pStream: TStream): string;
begin
  pStream.Position := 0;
  SetLength(Result, pStream.Size);
  pStream.ReadBuffer(PChar(Result)^, pStream.Size);
end;

{ TTemplateReader }

constructor TTemplateReader_IO.Create;
begin

end;

function TTemplateReader_IO.GetFilename(const pTemplateName: string): string;
begin
  with Configuration do
    Result := IncludeTrailingPathDelimiter(
      GetValue(cCONFIGKEY_TEMPLATEPATH, ExtractFilePath(ParamStr(0))))+
      pTemplateName+'.'+
      GetValue(cCONFIGKEY_TEMPLATEEXT, 'sql');
end;

function TTemplateReader_IO.Read(const pTemplateName: string): string;
var
  vFileName: string;
  vStream: TFileStream;
begin
  // *** tem de gerar uma exceção caso o arquivo nao exista, acho que tem de
  // *** procurar o arquivo com findfirst ou tratar o Result = Emptystr.
  vFileName := GetFilename(pTemplateName);
  vStream := TFileStream.Create(vFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := ReadFromStream(vStream);
  finally
    vStream.Free;
  end;
end;

procedure RegisterOnReflection;
begin
  with TypeService do
  begin
    AddType(ITemplateReader, 'TemplateReader', TTemplateReader, IElement);
    AddType(ITemplateReader_IO, 'TemplateReader_IO', TTemplateReader_IO, ITemplateReader);
  end;
end;

end.

