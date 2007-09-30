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
    AddType(IEntity, 'Entity', TEntity, IElement);
    AddType(IColumn, 'Column', TColumn, IElement);
    AddType(IID, 'ID', TID, IElement);
  end;
end;

end.
