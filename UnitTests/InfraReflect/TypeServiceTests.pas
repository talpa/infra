unit TypeServiceTests;

interface

uses
  InfraCommonIntf,
  TestFramework;

type
  TTypeServiceTests = class(TTestCase)
  private
    FPersonInfo: IClassInfo;
    FTypesCountOnSetup: integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTypes;
    procedure TestRelations;
    procedure TestGetType;
    procedure TestAddType;
    procedure TestAddRelation;
    procedure TestRelationsIteratorByType;
    procedure TestRelationsIteratorByProperty;
    procedure TestCreateInstanceByGUID;
    procedure TestCreateInstanceByTypeInfo;
    procedure TestDuplicateType;
  end;

implementation

uses
  SysUtils,
  InfraReflect, InfraValueTypeIntf, InfraValueType,
  ReflectModel, ReflectModelIntf;

{ TTypeServiceTests }

procedure TTypeServiceTests.SetUp;
begin
  inherited;
  with Typeservice do
  begin
    FTypesCountOnSetup := Types.Count;
    TSetupModel.RegisterAddress;
    FPersonInfo := TSetupModel.RegisterPerson;
    TSetupModel.RegisterStudent;
    TSetupModel.RegisterPersonAddressRelation(FPersonInfo);
  end;
end;

procedure TTypeServiceTests.TearDown;
begin
  inherited;
  TSetupModel.RemoveTypeInfo(IPerson);
  TSetupModel.RemoveTypeInfo(IAddress);
  TSetupModel.RemoveTypeInfo(IStudent);
  CheckEquals(FTypesCountOnSetup, TypeService.GetTypes.Count,
    'Wrong types''''s count after remove typeinfo');
end;

procedure TTypeServiceTests.TestTypes;
begin
  with TypeService do
  begin
    CheckNotNull(Types, 'Types not Assigned');
    CheckEquals(FTypesCountOnSetup + CLASSES_COUNT,
      TypeService.Types.Count, 'Wrong types count');
  end;
end;

procedure TTypeServiceTests.TestRelations;
begin
  with TypeService do
  begin
    CheckNotNull(Relations, 'Relations not Assigned');
    CheckEquals(RELATIONS_COUNT, TypeService.Relations.Count,
      'Wrong Relations''''s count');
  end;
end;

procedure TTypeServiceTests.TestGetType;
var
  SomeType: IClassInfo;
begin
  with TypeService do
  begin
    SomeType := GetType(IMemoryManagedObject);
    CheckNotNull(SomeType, 'Cannot get type by GUID');

    SomeType := GetType('InfraObject');
    CheckNotNull(SomeType, 'Cannot get type by Name');

    SomeType := GetType(TInfraVariant);
    CheckNotNull(SomeType, 'Cannot get type by Class');

    SomeType := GetType('SomeClassInexistent');
    CheckNull(SomeType, 'Found a class not registred on reflection');

    SomeType := GetType(IPerson);
    CheckNotNull(SomeType, 'Cannot get type Person');
  end;
end;

procedure TTypeServiceTests.TestAddType;
begin
  CheckNotNull(FPersonInfo, 'Metadata not returned on AddType');
end;

procedure TTypeServiceTests.TestCreateInstanceByGUID;
var
  Person: IPerson;
  R: IInterface;
begin
  with Typeservice do
  begin
    Person := CreateInstance(IPerson) as IPerson;
    CheckNotNull(Person, 'CreateInstance By GUID Fail, instance not created');
    Person.Name.AsString := 'InfraTeam''''s someone';
    CheckEquals(Person.Name.AsString, 'InfraTeam''''s someone',
      'Cannot fill property of Instance created by GUID');
  end;
  with Typeservice do
  begin
    R := CreateInstance(IPerson) as IPerson;
    CheckNotNull(Person, 'CreateInstance By GUID Fail, instance not created');
    Person.Name.AsString := 'InfraTeam''''s someone';
    CheckEquals(Person.Name.AsString, 'InfraTeam''''s someone',
      'Cannot fill property of Instance created by GUID');
  end;
end;

procedure TTypeServiceTests.TestCreateInstanceByTypeInfo;
var
  Person: IPerson;
begin
  with Typeservice do
  begin
    Person := CreateInstance(GetType(IPerson, True)) as IPerson;
    CheckNotNull(Person, 'CreateInstance By ClassInfo Fail, instance not created');
    Person.Name.AsString := 'InfraTeam''''s someone';
    CheckEquals(Person.Name.AsString, 'InfraTeam''''s someone',
      'Cannot fill property of Instance created by ClassInfo');
  end;
end;

procedure TTypeServiceTests.TestDuplicateType;
begin
  ExpectedException := EInfraTypeRegisteredAlready;
  with Typeservice do
    AddType(IPerson, 'Person', TPerson, IInfraObject, GetType(IInfraObject));
end;

procedure TTypeServiceTests.TestAddRelation;
begin
  with TypeService.Relations do
  begin
    CheckNotNull(Items[0], 'Metadata not returned on AddRelation');
    CheckEquals(RELATIONS_COUNT, Count,
      'Wrong number of relations on Iterator ByProperty');
  end;
end;

procedure TTypeServiceTests.TestRelationsIteratorByProperty;
var
  Iterator: IRelationInfoIterator;
  PropInfo: IPropertyInfo;
  i: integer;
begin
  with TypeService do
  begin
    PropInfo := GetType(IPerson).GetPropertyInfo('Address', True);
    Iterator := NewRelationsIterator(PropInfo);
  end;
  CheckNotNull(Iterator, 'Relations Iterator ByProperty not created');
  i := 0;
  while not Iterator.IsDone do
  begin
    Inc(i);
    Iterator.Next;
  end;
  CheckEquals(RELATIONS_COUNT, i,
    'Wrong number of relations on Iterator ByProperty');
end;

procedure TTypeServiceTests.TestRelationsIteratorByType;
var
  Iterator: IRelationInfoIterator;
  i: integer;
begin
  with TypeService do
    Iterator := NewRelationsIterator(GetType(IPerson));
  CheckNotNull(Iterator, 'Relations Iterator ByType not created');
  i := 0;
  while not Iterator.IsDone do
  begin
    Inc(i);
    Iterator.Next;
  end;
  CheckEquals(RELATIONS_COUNT, i,
    'Wrong number of relations on Iterator ByType');
end;

initialization
  TestFramework.RegisterTest('InfraReflectTests Suite',
    TTypeServiceTests.Suite);

end.
