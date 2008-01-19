unit InfraValueTypeRegister;

interface

procedure RegisterOnReflection;

implementation

uses
  InfraCommonIntf,
  InfraValueTypeIntf,
  InfraValueType;

procedure RegisterOnReflection;
begin
  with TypeService do
  begin
    AddType(IInfraType, 'InfraType', TInfraType, IElement);
    AddType(IInfraBoolean, 'InfraBoolean', TInfraBoolean, IInfraType);
    AddType(IInfraDate, 'InfraDate', TInfraDate, IInfraType);
    AddType(IInfraDateTime, 'InfraDateTime', TInfraDateTime, IInfraType);
    AddType(IInfraDouble, 'InfraDouble', TInfraDouble, IInfraType);
    AddType(IInfraEnumeration, 'InfraEnumeration', TInfraEnumeration,
      IInfraType);
    AddType(IInfraInteger, 'InfraInteger', TInfraInteger, IInfraType);
    AddType(IInfraList, 'InfraList', TInfraList, IInfraType);
    AddType(IInfraMap, 'InfraMap', TInfraMap, IInfraType);
    AddType(IInfraNativeObject, 'InfraNativeObject', TInfraNativeObject,
      IInfraType);
    AddType(IInfraObject, 'InfraObject', TInfraObject, IInfraType);
    AddType(IInfraStream, 'InfraStream', TInfraStream, IInfraType);
    AddType(IInfraString, 'InfraString', TInfraString, IInfraType);
    AddType(IInfraTime, 'InfraTime', TInfraTime, IInfraType);
    AddType(IInfraVariant, 'InfraVariant', TInfraVariant, IInfraType);
  end;
end;

end.
