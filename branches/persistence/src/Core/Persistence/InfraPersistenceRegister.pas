unit InfraPersistenceRegister;

interface

procedure RegisterOnReflection;

implementation

uses
  InfraCommonIntf,
  InfraPersistenceIntf,
  InfraPersistence,
  TemplateReader_IO;

procedure RegisterOnReflection;
begin
  with TypeService do
  begin
    AddType(ITemplateReader, 'TemplateReader', TTemplateReader, IElement);
    AddType(ITemplateReader_IO, 'TemplateReader_IO', TTemplateReader_IO, ITemplateReader);
  end;
end;

end.
