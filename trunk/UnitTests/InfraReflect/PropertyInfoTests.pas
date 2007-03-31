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
    FPerson: IPerson;
    FStudent: IStudent;
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
  InfraValueTypeIntf,
  InfraValueType, StdVCL;

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
    Email.AsString := 'Filiph@Mail';
    Birthday.AsDate := StrToDate('10/02/1980');
    Address.Street.AsString := 'Filiph Street';
    Address.City.AsString := 'Filiph City';
    Address.Quarter.AsString := 'Filiph Quarter';
    Address.Number.AsInteger := 100;
  end;

  FStudent := TStudent.Create;
  with FStudent do
  begin
    Name.AsString := 'Michael Stradius';
    Email.AsString := 'Michael@Mail';
    SchoolNumber.AsString := 'Michael ShcoolNumber';
    Birthday.AsDate := StrToDate('10/02/1980');
    Address.Street.AsString := 'Michael Street';
    Address.City.AsString := 'Michael City';
    Address.Quarter.AsString := 'Michael Quarter';
    Address.Number.AsInteger := 25;
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
  FPropertyInfo: IPropertyInfo;
  FMethodInfo: IMethodInfo;
begin
  FPropertyInfo := FPersonInfo.GetPropertyInfo('Email');
  FMethodInfo := FPropertyInfo.GetGetterInfo;
  CheckNotNull(FMethodInfo, 'Email Getter not assigned on Person');

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Email');
  FMethodInfo := FPropertyInfo.GetGetterInfo;
  CheckNotNull(FMethodInfo, 'Email Getter not assigned on Student');

  FPropertyInfo := FPersonInfo.GetPropertyInfo('Address.Street');
  FMethodInfo := FPropertyInfo.GetGetterInfo;
  CheckNotNull(FMethodInfo, 'Address.Street Getter not assigned on Person');

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Address.Street');
  FMethodInfo := FPropertyInfo.GetGetterInfo;
  CheckNotNull(FMethodInfo, 'Address.Street Getter not assigned on Student');
end;

procedure TPropertyInfoTests.TestGetSetterInfo;
var
  FPropertyInfo: IPropertyInfo;
  FMethodInfo: IMethodInfo;
begin
  FPropertyInfo := FPersonInfo.GetPropertyInfo('Email');
  FMethodInfo := FPropertyInfo.GetSetterInfo;
  CheckNotNull(FMethodInfo, 'Email Setter not assigned on Person');

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Email');
  FMethodInfo := FPropertyInfo.GetSetterInfo;
  CheckNotNull(FMethodInfo, 'Email Setter not assigned on Student');

  FPropertyInfo := FPersonInfo.GetPropertyInfo('Address.Quarter');
  FMethodInfo := FPropertyInfo.GetSetterInfo;
  CheckNotNull(FMethodInfo, 'Address.Quarter Setter not assigned on Person');

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Address.Quarter');
  FMethodInfo := FPropertyInfo.GetSetterInfo;
  CheckNotNull(FMethodInfo, 'Address.Quarter Setter not assigned on Student');
end;

procedure TPropertyInfoTests.TestGetTypeInfo;
var
  FPropertyInfo: IPropertyInfo;
  FTypeInfo: IClassInfo;
begin
  FPropertyInfo := FPersonInfo.GetPropertyInfo('Email');
  FTypeInfo := FPropertyInfo.GetTypeInfo;
  CheckNotNull(FTypeInfo, 'Person.Email do not have TypeInfo');
  CheckTrue(IsEqualGUID(FTypeInfo.TypeID, IInfraString),
    'Person.Email''''s TypeInfo should be an InfraString');

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Email');
  FTypeInfo := FPropertyInfo.GetTypeInfo;
  CheckNotNull(FTypeInfo, 'Student.Email do not have TypeInfo');
  CheckTrue(IsEqualGUID(FTypeInfo.TypeID, IInfraString),
    'Student.Email''''s TypeInfo should be an InfraString');

  FPropertyInfo := FPersonInfo.GetPropertyInfo('Address.City');
  FTypeInfo := FPropertyInfo.GetTypeInfo;
  CheckNotNull(FTypeInfo, 'Person.Address.City do not have TypeInfo');
  CheckTrue(IsEqualGUID(FTypeInfo.TypeID, IInfraString),
    'Person.Address.City''''s TypeInfo should be an InfraString');

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Address.City');
  FTypeInfo := FPropertyInfo.GetTypeInfo;
  CheckNotNull(FTypeInfo, 'Student.Address.City do not have TypeInfo');
  CheckTrue(IsEqualGUID(FTypeInfo.TypeID, IInfraString),
    'Student.Address.City''''s TypeInfo should be an InfraString');
end;

procedure TPropertyInfoTests.TestGetValue;
var
  FPropertyInfo: IPropertyInfo;
  FMethodInfo: IMethodInfo;
  Street: string;
begin
  FPropertyInfo := FPersonInfo.GetPropertyInfo('Email');
  CheckEquals('Filiph@Mail',
    (FPropertyInfo.GetValue(FPerson) as IInfraString).AsString,
    'Mismatch Person.Email value: '+
    (FPropertyInfo.GetValue(FPerson) as IInfraString).AsString);

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Email');
  CheckEquals('Michael@Mail',
    (FPropertyInfo.GetValue(FStudent) as IInfraString).AsString,
    'Mismatch Student.Email value: '+
    (FPropertyInfo.GetValue(FStudent) as IInfraString).AsString);

  FPropertyInfo := FPersonInfo.GetPropertyInfo('Address.Street');
  Street := (FPropertyInfo.GetValue(FPerson.Address) as IInfraString).AsString;
  CheckEquals('Filiph Street', Street,
    'Mismatch Person.Address.Street value: ' + Street);

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Address.Street');
  Street := (FPropertyInfo.GetValue(FStudent.Address) as IInfraString).AsString;
  CheckEquals('Michael Street', Street,
    'Mismatch Person.Address.Street value: ' + Street);
end;

procedure TPropertyInfoTests.TestSetValue;
var
  FPropertyInfo: IPropertyInfo;
  FMethodInfo: IMethodInfo;
begin
  FPropertyInfo := FPersonInfo.GetPropertyInfo('Email');
  FPropertyInfo.SetValue(FPerson, TInfraString.NewFrom('NewFiliph@Mail'));
  CheckEquals('NewFiliph@Mail', FPerson.Email.AsString,
    'Mismatch Person.Email value: ' + FPerson.Email.AsString);

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Email');
  FPropertyInfo.SetValue(FStudent, TInfraString.NewFrom('NewMichael@Mail'));
  CheckEquals('NewMichael@Mail', FStudent.Email.AsString,
    'Mismatch Student.Email value: ' + FStudent.Email.AsString);

  FPropertyInfo := FPersonInfo.GetPropertyInfo('Address.Street');
  FPropertyInfo.SetValue(FPerson.Address,
    TInfraString.NewFrom('New Filiph Street'));
  CheckEquals('New Filiph Street', FPerson.Address.Street.AsString,
    'Mismatch Person.Address.Street value: ' + FPerson.Address.Street.AsString);

  FPropertyInfo := FStudentInfo.GetPropertyInfo('Address.Street');
  FPropertyInfo.SetValue(FStudent.Address,
    TInfraString.NewFrom('New Michael Street'));
  CheckEquals('New Michael Street', FStudent.Address.Street.AsString,
    'Mismatch Student.Address.Street value: ' + FStudent.Address.Street.AsString);
end;

initialization
  TestFramework.RegisterTest('InfraReflectTests Suite', TPropertyInfoTests.Suite);

end.