unit InfraCommonRegister;

interface

procedure RegisterOnReflection;

implementation

uses
  InfraCommonIntf,
  InfraCommon;

procedure RegisterOnReflection;
begin
  with TypeService do
  begin
    AddType(IMemoryManagedObject, 'MemoryManagedObject', TMemoryManagedObject,
      IInterface);
    AddType(IElement, 'Element', TElement, IInterface);
  end;
end;

end.


