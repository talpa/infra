unit ReaderTemplate_IO;

interface

uses
  Classes,
  InfraCommon,
  InfraPersistenceIntf;

type
  TTemplateReader = class(TBaseElement, ITemplateReader)
  private
    FConfiguration: IConfiguration;
  protected
    function GetFilename(const pTemplateName: string): string;
    function Read(const pTemplateName: string): string;
  public
    constructor Create(pConfiguration: IConfiguration); reintroduce;
  end;

implementation

uses
  SysUtils,
  InfraPersistenceConsts,
  InfraCommonIntf;

{ TTemplateReader }

constructor TTemplateReader.Create(pConfiguration: IConfiguration);
begin
  if not Assigned(pConfiguration) then
    raise EInfraArgumentError.Create('pConfiguration');
  inherited Create;
  FConfiguration := pConfiguration;
end;

function TTemplateReader.GetFilename(const pTemplateName: string): string;
begin
  with FConfiguration do
    Result := IncludeTrailingPathDelimiter(
      GetValue(cCONFIGKEY_TEMPLATEPATH, ExtractFilePath(ParamStr(0))))+
      pTemplateName+'.'+
      GetValue(cCONFIGKEY_TEMPLATEEXT, 'sql');
end;

function TTemplateReader.Read(const pTemplateName: string): string;
var
  vFileName: string;
  stm: TFileStream;
  vFileSize: Integer;
begin
  vFileName := GetFilename(pTemplateName);

  stm := TFileStream.Create(vFileName, fmOpenRead or fmShareDenyWrite);
  try
    vFileSize := stm.Seek(0, soFromEnd);
    stm.Seek(0, 0);
    SetLength(Result, vFileSize);
    stm.Read(PChar(Result)^, vFileSize);
  finally
    stm.Free;
  end;
end;

end.
