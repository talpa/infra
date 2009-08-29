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
    AddType(IInfraBoolean, 'InfraBoolean', TInfraBoolean, IInfraBoolean);
    AddType(IInfraDate, 'InfraDate', TInfraDate, IInfraDate);
    AddType(IInfraDateTime, 'InfraDateTime', TInfraDateTime, IInfraDateTime);
    AddType(IInfraDouble, 'InfraDouble', TInfraDouble, IInfraDouble);
    AddType(IInfraEnumeration, 'InfraEnumeration', TInfraEnumeration,
      IInfraEnumeration);
    AddType(IInfraInteger, 'InfraInteger', TInfraInteger, IInfraInteger);
    AddType(IInfraList, 'InfraList', TInfraList, IInfraList);
    AddType(IInfraMap, 'InfraMap', TInfraMap, IInfraMap);
    AddType(IInfraNativeObject, 'InfraNativeObject', TInfraNativeObject,
      IInfraNativeObject);
    AddType(IInfraObject, 'InfraObject', TInfraObject, IInfraObject);
    AddType(IInfraStream, 'InfraStream', TInfraStream, IInfraStream);
    AddType(IInfraString, 'InfraString', TInfraString, IInfraString);
    AddType(IInfraTime, 'InfraTime', TInfraTime, IInfraTime);
    AddType(IInfraVariant, 'InfraVariant', TInfraVariant, IInfraVariant);
  end;
end;

end.
