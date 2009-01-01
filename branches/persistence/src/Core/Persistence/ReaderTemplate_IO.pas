unit ReaderTemplate_IO;

interface

uses
  Classes, InfraCommon, InfraPersistenceIntf;

type
  TTemplateReader = class(TBaseElement, ITemplateReader)
  private
    FTemplateText: TStringList;
    FConfiguration: IConfiguration;
  protected
    function Read(const pTemplateName: string): string;
  public
    constructor Create(pConfiguration: IConfiguration); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  InfraPersistenceConsts;

{ TTemplateReader }

constructor TTemplateReader.Create(pConfiguration: IConfiguration);
begin
  inherited Create;
  FConfiguration := pConfiguration;
  FTemplateText := TStringList.Create;
end;

destructor TTemplateReader.Destroy;
begin
  FTemplateText.Free;
  inherited;
end;

function TTemplateReader.Read(const pTemplateName: string): string;
var
  vFileName: string;
begin
  with FConfiguration do
  begin
    vFileName :=
      IncludeTrailingPathDelimiter(
        GetValue(cCONFIGKEY_TEMPLATEPATH, ExtractFilePath(ParamStr(0))))+
      pTemplateName+'.'+
      GetValue(cCONFIGKEY_TEMPLATEEXT, 'sql');
  end;
  with FTemplateText do
  begin
    Clear;
    LoadFromFile(vFileName);
    if Text = EmptyStr then
      Raise EInfraTemplateNotFound.Create(cErrorTemplateFileNotFound);
    Result := Text;
  end;
end;

end.
