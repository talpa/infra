unit InfraHibernateRegister;

interface

procedure RegisterOnReflection;

implementation

uses
  InfraCommonIntf,
  InfraHibernateIntf,
  MapperAnnotationIntf,
  MapperAnnotation;

procedure RegisterOnReflection;
begin
  with TypeService do
  begin
    AddAnnotation(IEntity, 'Entity', TEntity, IElement);
    AddAnnotation(IColumn, 'Column', TColumn, IElement);
    AddAnnotation(IID, 'ID', TID, IElement);
  end;
end;

end.
