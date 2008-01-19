unit InfraHibernateRegister;

interface

procedure RegisterOnReflection;

implementation

uses
  InfraCommonIntf,
  InfraHibernateIntf,
  InfraDBXConnection,
  MapperAnnotationIntf,
  MapperAnnotation;

procedure RegisterOnReflection;
begin
  with TypeService do
  begin
    AddType(IDBXConnection, 'DBXConnection', TDBXConnection, IElement);
    AddAnnotation(IEntity, 'Entity', TEntity, IElement);
    AddAnnotation(IColumn, 'Column', TColumn, IElement);
    AddAnnotation(IID, 'ID', TID, IElement);
  end;
end;

end.
