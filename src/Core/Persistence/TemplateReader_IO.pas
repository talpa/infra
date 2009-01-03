unit TemplateReader_IO;

interface

uses
  Classes,
  InfraCommon,
  InfraPersistenceIntf,
  InfraPersistence;

type
  TTemplateReader_IO = class(TTemplateReader, ITemplateReader)
  protected
    function GetFilename(const pTemplateName: string): string;
    function Read(const pTemplateName: string): string;
  end;

implementation

uses
  SysUtils,
  InfraPersistenceConsts,
  InfraCommonIntf;

{ TTemplateReader }

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
  vFileSize: Integer;
begin
  // *** tem de gerar uma exceção caso o arquivo nao exista, acho que tem de
  // *** procurar o arquivo com findfirst ou tratar o Result = Emptystr.
  vFileName := GetFilename(pTemplateName);
  vStream := TFileStream.Create(vFileName, fmOpenRead or fmShareDenyWrite);
  try
    vFileSize := vStream.Seek(0, soFromEnd);
    vStream.Seek(0, 0);
    SetLength(Result, vFileSize);
    vStream.Read(PChar(Result)^, vFileSize);
  finally
    vStream.Free;
  end;
end;

end.
