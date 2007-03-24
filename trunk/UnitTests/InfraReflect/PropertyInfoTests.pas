unit PropertyInfoTests;

interface

uses
  InfraCommonIntf,
  TestFrameWork,
  Model,
  ModelIntf;

type
  TPropertyInfoTests = class(TTestCase)
  private
    FPersonInfo: IClassInfo;
    FStudentInfo: IClassInfo;
    FPerson : IPerson;
    FStudend : IStudent;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetGetterInfo;
    procedure TestGetSetterInfo;
    procedure TestGetValue;
    procedure TestSetValue;
    procedure TestGetTypeInfo;
  end;

implementation

uses
  SysUtils, InfraConsts,
  InfraValueTypeIntf, StdVCL;

{ TPropertyInfoTests }

procedure TPropertyInfoTests.SetUp;
begin
  inherited;
  TSetupModel.RegisterAddress;
  FPersonInfo  := TSetupModel.RegisterPerson;
  FStudentInfo := TSetupModel.RegisterStudent;
  TSetupModel.RegisterPersonAddressRelation(FPersonInfo);

  FPerson  := TPerson.Create;
  with FPerson do
  begin
    Name.AsString := 'Filiph Stradius';
    Email.AsString := 'Filiph Address';
    Birthday.AsDate := StrToDate('10/02/1980');
    Address.Street.AsString := 'Filiph Street';
    Address.City.AsString := 'Filiph City';
    Address.Quarter.AsString := 'Filiph Quarter';
    Address.Number.AsInteger := 100;
  end;

  FStudend := TStudent.Create;
  with FStudend do
  begin
    Name.AsString := 'Michael Stradius';
    Email.AsString := 'Michael Address';
    SchoolNumber.AsString := 'Michael ShcoolNumber';
    Birthday.AsDate := StrToDate('10/02/1980');
    Address.Street.AsString := 'Michael Street';
    Address.City.AsString := 'Michael City';
    Address.Quarter.AsString := 'Michael Quarter';
    Address.Number.AsInteger := 100;
  end;
end;

procedure TPropertyInfoTests.TearDown;
begin
  TSetupModel.RemoveTypeInfo(IPerson);
  TSetupModel.RemoveTypeInfo(IAddress);
  TSetupModel.RemoveTypeInfo(IStudent);
  inherited;
end;

procedure TPropertyInfoTests.TestGetGetterInfo;
var
  FPropertyInfo : IPropertyInfo;
  FMethodInfo   : IMethodInfo;
begin
  FPropertyInfo := FPersonInfo.GetPropertyInfo('Email');
  CheckNotNull(FPropertyInfo, 'Email property should be in Person');
  FMethodInfo := FPropertyInfo.GetGetterInfo;
  CheckNotNull(FMethodInfo, 'Email Getter not assigned');

  FPropertyInfo := FPersonInfo.GetPropertyInfo('Address');
  CheckNotNull(FPropertyInfo, 'Address City property should be in Person');
  FMethodInfo := FPropertyInfo.GetGetterInfo;
  CheckNotNull(FMethodInfo, 'Address City Getter not assigned');

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Email');
  CheckNotNull(FPropertyInfo, 'Email property should be in Student');
  FMethodInfo := FPropertyInfo.GetGetterInfo;
  CheckNotNull( FMethodInfo, 'Email Getter not assigned');

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Address.City');
  CheckNotNull(FPropertyInfo, 'Address.City property should be in Student');
  CheckEquals('City', FPropertyInfo.Name, 'Property should be name City');
  FMethodInfo := FPropertyInfo.GetGetterInfo;
  CheckNotNull(FMethodInfo, 'Address City Getter not assigned');
end;

procedure TPropertyInfoTests.TestGetSetterInfo;
var
  FPropertyInfo : IPropertyInfo;
  FMethodInfo   : IMethodInfo;
begin
  FPropertyInfo := FPersonInfo.GetPropertyInfo('Email');
  CheckNotNull(FPropertyInfo, 'Email property should be in Person');
  FMethodInfo := FPropertyInfo.GetSetterInfo;
  CheckNotNull( FMethodInfo, 'Email Setter not assigned');

  FPropertyInfo := FPersonInfo.GetPropertyInfo('Address');
  CheckNotNull(FPropertyInfo, 'Address City property should be in Person');
  FMethodInfo := FPropertyInfo.GetSetterInfo;
  CheckNotNull( FMethodInfo, 'Address City Setter not assigned');

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Email');
  CheckNotNull(FPropertyInfo, 'Email property should be in Student');
  FMethodInfo := FPropertyInfo.GetSetterInfo;
  CheckNotNull( FMethodInfo, 'Email Setter not assigned');

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Address');
  CheckNotNull(FPropertyInfo, 'Address City property should be in Student');
  FMethodInfo := FPropertyInfo.GetSetterInfo;
  CheckNotNull( FMethodInfo, 'Address City Setter not assigned');  
end;

procedure TPropertyInfoTests.TestGetTypeInfo;
var
  FPropertyInfo : IPropertyInfo;
  FTypeInfo : IClassInfo;
begin
  FPropertyInfo := FPersonInfo.GetPropertyInfo('Email');
  CheckNotNull(FPropertyInfo, 'Email property should be in Person');
  FTypeInfo := FPropertyInfo.GetTypeInfo;
  CheckNotNull(FTypeInfo, 'Person not have TypeInfo');
  CheckTrue(IsEqualGUID(FTypeInfo.TypeID, IInfraString),
    'Property TypeInfo should be a InfraString');

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Email');
  CheckNotNull(FPropertyInfo, 'Email property should be in Student');
  FTypeInfo := FPropertyInfo.GetTypeInfo;
  CheckNotNull(FTypeInfo, 'Person not have TypeInfo');
  CheckTrue(IsEqualGUID(FTypeInfo.TypeID, IInfraString),
    'Property TypeInfo should be a InfraString');
end;

procedure TPropertyInfoTests.TestGetValue;
var
  FPropertyInfo : IPropertyInfo;
  FMethodInfo : IMethodInfo;
//  FInfraInstance : IInfraInstance;
begin
  FPropertyInfo := FPersonInfo.GetPropertyInfo('Email');
  CheckNotNull(FPropertyInfo, 'Email property should be in Person');
  FMethodInfo := FPropertyInfo.GetGetterInfo;
  CheckNotNull(FMethodInfo, 'Email Getter not assigned');
end;

procedure TPropertyInfoTests.TestSetValue;
var
  FPropertyInfo : IPropertyInfo;
  FMethodInfo : IMethodInfo;
begin
  FPropertyInfo := FPersonInfo.GetPropertyInfo('Email');
  CheckNotNull(FPropertyInfo, 'Email property should be in Person');
  FMethodInfo := FPropertyInfo.GetSetterInfo;
  CheckNotNull(FMethodInfo, 'Email Getter not assigned');
end;

initialization
  TestFramework.RegisterTest('InfraReflectTests Suite', TPropertyInfoTests.Suite);

end.