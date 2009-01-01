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

uses SysUtils;

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
//var
//  vFileName: string;
begin
//  cCONFIGKEY_TEMPLATEPATH = 'Template.Path';
//  cCONFIGKEY_TEMPLATEEXT = 'Template.Ext';
//
//  vFileName :=
//    FConfiguration.GetValue(cCONFIGKEY_TEMPLATEPATH, EmptyStr)+
//  with FTemplateText do
//  begin
//    Clear;
//    LoadFromFile(Format());
//    if not FileExists  = EmptyStr then
//      Raise ETemplateReaderInvalidType.Create();
//
//    Result := Text;
//  end;  .
//  FTemplateText.Clear;
//  if not Assigned() then
//
//  cErrorTemplateFileNotFound = 'Template %s não encontrado';
end;

end.
