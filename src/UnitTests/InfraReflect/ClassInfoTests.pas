unit ClassInfoTests;

interface

uses
  InfraCommonIntf,
  TestFrameWork;

type
  TClassInfoTests = class(TTestCase)
  private
    FPersonInfo: IClassInfo;
    FStudentInfo: IClassInfo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFamilyID;
    procedure TestFullName;
    procedure TestTypeID;
    procedure TestImplClass;
    procedure TestName;
    procedure TestOwner;
    procedure TestFindMembers;
    procedure TestConstructors;
    procedure TestMemberInfo;
    procedure TestMembers;
    procedure TestMethodInfo;
    procedure TestMethods;
    procedure TestProperties;
    procedure TestProperty;
    procedure TestPropertyInfo;
    procedure TestSuperClass;
    procedure TestIsSubClassOf;
    procedure TestSetRelation;
  end;

implementation

uses
  SysUtils, InfraConsts,
  InfraValueTypeIntf,
  ReflectModel, ReflectModelIntf;

{ TClassInfoTests }

procedure TClassInfoTests.SetUp;
begin
  inherited;
  TSetupModel.RegisterAddress;
  FPersonInfo := TSetupModel.RegisterPerson;
  FStudentInfo := TSetupModel.RegisterStudent;
  TSetupModel.RegisterPersonAddressRelation(FPersonInfo);
end;

procedure TClassInfoTests.TearDown;
begin
  inherited;
  TSetupModel.RemoveTypeInfo(IPerson);
  TSetupModel.RemoveTypeInfo(IAddress);
  TSetupModel.RemoveTypeInfo(IStudent);
end;

procedure TClassInfoTests.TestConstructors;
var
  i : integer;
  it : IMethodInfoIterator;
begin
  i := 0;
  it := FPersonInfo.GetConstructors;
  CheckNotNull(It, 'Constructors''s Iterator not created');
  while not it.IsDone do
  begin
    Inc(i);
    it.Next;
  end;
  CheckEquals(PERSON_CONSTRUCTOR_COUNT, i, 'Wrong constructors count');
end;

procedure TClassInfoTests.TestFamilyID;
begin
  CheckFalse(IsEqualGUID(FPersonInfo.FamilyID, NullGuid), 'Empty FamilyID');
  Check(IsEqualGUID(FPersonInfo.FamilyID, IInfraObject),
    'Wrong FamilyID, Should be IInfraObject');
end;

procedure TClassInfoTests.TestFindMembers;
var
  i: integer;
  It: IMemberInfoIterator;
begin
  i := 0;
  It := FPersonInfo.FindMembers(mtAll);
  CheckNotNull(It, '(mtAll) FindMembers''s Iterator not created');
  if Assigned(It) then
  begin
    while not It.IsDone do
    begin
      Inc(i);
      It.Next;
    end;
    CheckEquals(
      PERSON_PROPERTIES_COUNT+
      PERSON_METHODS_COUNT+
      PERSON_CONSTRUCTOR_COUNT, i, 'Wrong findmembers mtAll count');
  end;

  i := 0;
  It := FPersonInfo.FindMembers([mtProperty]);
  CheckNotNull(It, '(mtProperty) FindMembers''s Iterator to property not created');
  if Assigned(It) then
  begin
    while not It.IsDone do
    begin
      Inc(i);
      It.Next;
    end;
    CheckEquals(PERSON_PROPERTIES_COUNT, i,
      'Wrong FindMembers(Properties) count');
  end;

  i := 0;
  It := FPersonInfo.FindMembers([mtMethod]);
  CheckNotNull(It, '(mtMethod) FindMembers''s Iterator to Method not created');
  if Assigned(It) then
  begin
    while not It.IsDone do
    begin
      Inc(i);
      It.Next;
    end;
    CheckEquals(PERSON_METHODS_COUNT, i,
      'Wrong FindMembers(Methods) count');
  end;
end;

procedure TClassInfoTests.TestFullName;
begin
  CheckEquals(FPersonInfo.FullName, 'TPerson',
    'ClassInfo FullName incorrect to Person');
  CheckEquals(FStudentInfo.FullName, 'TStudent',
    'ClassInfo FullName incorrect to Student');
  // *** adicionar um CheckEquals para um objeto que tenha um fullname <> name
end;

procedure TClassInfoTests.TestImplClass;
begin
  CheckEquals(FPersonInfo.ImplClass, TPerson,
    'ImplClass diferent of Person');
end;

procedure TClassInfoTests.TestIsSubClassOf;
begin
  with TypeService do
  begin
    CheckTrue(FStudentInfo.IsSubClassOf(GetType(IPerson)),
      'Student should be subclass of Person');
    CheckTrue(FStudentInfo.IsSubClassOf(GetType(IInfraObject)),
      'Student should be subclass of InfraObject');
    CheckFalse(FStudentInfo.IsSubClassOf(GetType(IAddress)),
      'Student shouldn''''t be subclass of Address');
  end;
end;

procedure TClassInfoTests.TestMemberInfo;
var
  FMember : IMemberInfo;
begin
  FMember := FPersonInfo.GetMemberInfo('GetEmail');
  CheckNotNull(FMember, 'GetMail member should be in Person');
end;

procedure TClassInfoTests.TestMembers;
var
  i : integer;
  it : IMemberInfoIterator;
begin
  i := 0;
  it := FPersonInfo.GetMembers;
  CheckNotNull(It, 'Members''s Iterator not created');
  while not it.IsDone do
  begin
    Inc(i);
    it.Next;
  end;
  CheckEquals(
    PERSON_METHODS_COUNT+
    PERSON_PROPERTIES_COUNT+
    PERSON_CONSTRUCTOR_COUNT, i, 'Wrong members count');
end;

procedure TClassInfoTests.TestMethodInfo;
var
  FMethod : IMethodInfo;
begin
  FMethod := FPersonInfo.GetMethodInfo('SetName');
  CheckNotNull(FMethod, 'SetName Method should be in Person');
  FMethod := FPersonInfo.GetMethodInfo('Create');
  CheckNotNull(FMethod, 'Create constructor should be in Person');
end;

procedure TClassInfoTests.TestMethods;
var
  i: integer;
  It: IMethodInfoIterator;
begin
  i := 0;
  It := FPersonInfo.GetMethods;
  CheckNotNull(It, 'Methods''s Iterator not created');
  while not It.IsDone do
  begin
    Inc(i);
    It.Next;
  end;
  CheckEquals(PERSON_METHODS_COUNT, i, 'Wrong methods count');
end;

procedure TClassInfoTests.TestName;
begin
  CheckEquals(FPersonInfo.Name, 'TPerson', 'Incorrect Person´s Class Name');
  CheckEquals(FStudentInfo.Name, 'TStudent', 'Incorrect Student´s Class Name');
end;

procedure TClassInfoTests.TestOwner;
begin
   CheckNull(FPersonInfo.Owner, 'Persson´s Owner should be null');
   CheckNull(FStudentInfo.Owner, 'Student´s Owner should be null');
   // *** ver uma classe que teria o owner <> null
end;

procedure TClassInfoTests.TestProperties;
var
  i: integer;
  It: IPropertyInfoIterator;
  Value: IInfraType;
  p:  IPerson;
begin
  p := TPerson.Create;
  p.Name.AsString := 'Marcos';
  p.Email.AsString := 'mrbar2000@gmail.com';

  i := 0;
  It := FPersonInfo.GetProperties;
  CheckNotNull(It, 'PropertyInfo Iterator not created');
  if Assigned(It) then
    while not It.IsDone do
    begin

      //FPropertyInfo := ;
      Value := It.CurrentItem.GetValue(p) as IInfraType;

      Inc(i);
      It.Next;
    end;
  CheckEquals(PERSON_PROPERTIES_COUNT, i, 'Wrong properties count');
end;

procedure TClassInfoTests.TestProperty;
var
  Value: IInfraString;
  Pessoa:  IPerson;
begin
  Pessoa := TPerson.Create;
  Pessoa.Name.AsString := 'Marcos';
  Pessoa.Email.AsString := 'mrbar2000@gmail.com';

  Value := FPersoninfo.GetProperty(Pessoa as IElement, 'Name') as IInfraString;
  CheckEquals(Pessoa.Name.AsString, Value.AsString,
    'Wrong person´s name getting by reflection');

  Value := FPersoninfo.GetProperty(Pessoa as IElement, 'Email') as IInfraString;
  CheckEquals(Pessoa.Email.AsString, Value.AsString,
    'Wrong person´s mail getting by reflection');

  Value := FPersoninfo.GetProperty(Pessoa as IElement,
    'Address.Street') as IInfraString;
  CheckEquals(Pessoa.Address.Street.AsString, Value.AsString,
    'Wrong person''''s Address.Street getting by reflection');

  Value := FPersoninfo.GetProperty(Pessoa as IElement,
    'Address.Quarter') as IInfraString;
  CheckEquals(Pessoa.Address.Quarter.AsString, Value.AsString,
    'Wrong person''''s Address.Quarter getting by reflection');

  Value := FPersoninfo.GetProperty(Pessoa as IElement, 'NAME') as IInfraString;
  CheckNotNull(Value, 'Propriedade NAME não foi encontrado. GetProperty deveria ser CaseInsensitive');
end;

procedure TClassInfoTests.TestPropertyInfo;
var
  FProperty: IPropertyInfo;
begin
  FProperty := FPersonInfo.GetPropertyInfo('Email');
  CheckNotNull(FProperty, 'Email property should be in Person');

  FProperty := FPersonInfo.GetPropertyInfo('EMAIL');
  CheckNotNull(FProperty, 'EMAIL property should be in Person. GetPropertyInfo deveria ser CaseInsensitive');

  FProperty := FStudentInfo.GetPropertyInfo('Email');
  CheckNotNull(FProperty, 'Email property should be in Student');

  FProperty := FPersonInfo.GetPropertyInfo('Address.City');
  CheckNotNull(FProperty, 'Address.City property should be in Person');

  FProperty := FStudentInfo.GetPropertyInfo('Address.City');
  CheckNotNull(FProperty, 'Address.City property should be in Student');
end;

procedure TClassInfoTests.TestSetRelation;
begin
  // *** Testar aqui relacoes criadas a partir de:
  // - TClassInfo.SetRelation(SetRelation(const pPropertyName: string;...);
  // - TClassInfo.SetRelation(const pPropertyInfo: IPropertyInfo;...);
end;

procedure TClassInfoTests.TestSuperClass;
begin
  with TypeService do
  begin
    CheckTrue(FPersonInfo.SuperClass = GetType(IInfraObject),
      'Person''''s SuperClass diferent of IInfraObject');
    CheckTrue(FStudentInfo.SuperClass = GetType(IPerson),
      'Student''''s SuperClass diferent of IPerson');
  end;
end;

procedure TClassInfoTests.TestTypeID;
begin
  CheckTrue(IsEqualGUID(FPersonInfo.TypeID, IPerson),
    'TypeID diferent of Person');
end;

initialization
  TestFramework.RegisterTest('InfraReflectTests Suite',
    TClassInfoTests.Suite);

end.
