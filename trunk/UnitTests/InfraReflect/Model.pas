unit Model;

interface

uses
  InfraValueTypeIntf, InfraValueType, InfraCommonIntf,
  ModelIntf;

type
  // Esta classe serve apenas para propósito de teste. Nossas classes poderiam
  // ser registradas todas de uma só vez. Estes registros serão gerados
  // automaticamente por alguma ferramnenta ou wizard na IDE.
  TSetupModel = class
  public
    class function RegisterPerson: IClassInfo;
    class function RegisterAddress: IClassInfo;
    class function RegisterStudent: IClassInfo;
    class procedure RegisterPersonAddressRelation(
      const Source: IClassInfo);
    class function RegisterMockMethod: IClassInfo;
    class procedure RemoveTypeInfo(ID: TGUID);
  end;

  TPerson = class(TInfraObject, IPerson)
  private
    FBirthday: IInfraDate;
    FEmail: IInfraString;
    FName: IInfraString;
    FAddress: IAddress;
    procedure SetAddress(const Value: IAddress);
    procedure SetBirthday(const Value: IInfraDate);
    procedure SetEmail(const Value: IInfraString);
    procedure SetName(const Value: IInfraString);
    function GetAge: IInfraInteger;
    function GetAddress: IAddress;
    function GetBirthday: IInfraDate;
    function GetEmail: IInfraString;
    function GetName: IInfraString;
  public
    procedure InfraInitInstance; override;
    property Address: IAddress read GetAddress write SetAddress;
    property Name: IInfraString read GetName write SetName;
    property Email: IInfraString read GetEmail write SetEmail;
    property Birthday: IInfraDate read GetBirthday
      write SetBirthday;
  end;

  TAddress = class(TInfraObject, IAddress)
  private
    FCity: IInfraString;
    FNumber: IInfraInteger;
    FQuarter: IInfraString;
    FStreet: IInfraString;
    function GetCity: IInfraString;
    function GetNumber: IInfraInteger;
    function GetQuarter: IInfraString;
    function GetStreet: IInfraString;
    procedure SetCity(const Value: IInfraString);
    procedure SetNumber(const Value: IInfraInteger);
    procedure SetQuarter(const Value: IInfraString);
    procedure SetStreet(const Value: IInfraString);
  public
    procedure InfraInitInstance; override;
    property Street: IInfraString read GetStreet write SetStreet;
    property City: IInfraString read GetCity write SetCity;
    property Quarter: IInfraString read GetQuarter write SetQuarter;
    property Number: IInfraInteger read GetNumber write SetNumber;
  end;

  TStudent = class(TPerson, IStudent)
  private
    FSchoolNumber: IInfraString;
    function GetSchoolNumber: IInfraString;
    procedure SetSchoolNumber(const Value: IInfraString);
  public
    procedure InfraInitInstance; override;
    property SchoolNumber: IInfraString read GetSchoolNumber
      write SetSchoolNumber;
  end;

  {
  TTeacher = class(TPerson, ITeacher)
  public
    procedure InfraInitInstance; override;
    property Title: IInfraString read GetTitle
      write SetTitle;
  end;

  TGroup = class(TInfraObject, ITeacher)
  public
    procedure InfraInitInstance; override;
    property Name: IInfraString read GetName write SetName;
  end;

  TDiscipline = class(TInfraObject, ITeacher)
  public
    procedure InfraInitInstance; override;
    property Name: IInfraString read GetName write SetName;
    property Summary: IInfraString read GetSummary write SetSummary;
    property Summary: IInfraString read GetSummary write SetSummary;
  end;
  }

  TMockMethod = class(TInfraObject, IMockMethod)
  private
    FMessage: IInfraString;
    function GetMessage: IInfraString;
  public
    Constructor Create; overload;
    property Message: IInfraString read GetMessage;
    constructor Constructor0;
    constructor Constructor1(p1: IInfraString);
    function MethodFunc0: IInfraString;
    function MethodFunc1(const p1: IInfraString): IInfraString;
    function MethodFunc2(const p1: IInfraString; const p2:IInfraInteger): IInfraInteger;
    function MethodFunc3(const p1: IInfraString; const p2:IInfraInteger;
      const p3: IInfraDateTime): IInfraDateTime;
    function MethodFunc4(const p1: IInfraString; const p2:IInfraInteger;
      const p3: IInfraDateTime; const p4: IInfraBoolean): IInfraBoolean;
    function MethodFunc5(const p1: IInfraString; const p2:IInfraInteger;
      const p3: IInfraDateTime; const p4: IInfraBoolean;
      const p5: IInfraDouble): IInfraDouble;
    procedure MethodProc0;
    procedure MethodProc1(const p1: IInfraString);
    procedure MethodProc2(const p1: IInfraString; const p2:IInfraInteger);
    procedure MethodProc3(const p1: IInfraString; const p2:IInfraInteger;
      const p3: IInfraDateTime);
    procedure MethodProc4(const p1: IInfraString; const p2:IInfraInteger;
      const p3: IInfraDateTime; const p4: IInfraBoolean);
    procedure MethodProc5(const p1: IInfraString; const p2:IInfraInteger;
      const p3: IInfraDateTime; const p4: IInfraBoolean; const p5: IInfraDouble);
    procedure InfraInitInstance; override;
  end;

const
  CLASSES_COUNT = 3;
  RELATIONS_COUNT = 1;
  PERSON_METHODS_COUNT = 9;
  PERSON_PROPERTIES_COUNT = 4;
  PERSON_CONSTRUCTOR_COUNT = 1;
  cMessageProc0 = 'MethodProc0 called';
  cMessageProc1 = 'MethodProc1(%s) called';
  cMessageProc2 = 'MethodProc2(%s, %d) called';
  cMessageProc3 = 'MethodProc3(%s, %d, %s) called';
  cMessageProc4 = 'MethodProc4(%s, %d, %s, %s) called';
  cMessageProc5 = 'MethodProc5(%s, %d, %s, %s, %e) called';
  cMessageFunc0 = 'MethodFunc0: %s called';
  cMessageFunc1 = 'MethodFunc1(%s): %s called';
  cMessageFunc2 = 'MethodFunc2(%s, %d): %d called';
  cMessageFunc3 = 'MethodFunc3(%s, %d, %s): %s called';
  cMessageFunc4 = 'MethodFunc4(%s, %d, %s, %s): %s called';
  cMessageFunc5 = 'MethodFunc5(%s, %d, %s, %s, %e): %e called';

implementation

uses
  SysUtils, DateUtils, InfraConsts;

{ TSetupModel }

class function TSetupModel.RegisterPerson: IClassInfo;
begin
  with TypeService do
  begin
    Result := AddType(IPerson, 'Person', TPerson,
      IInfraObject, GetType(IInfraObject));
    with Result do
    begin
      AddConstructorInfo('Create', nil, @TPerson.Create);
      AddPropertyInfo('Name', GetType(IInfraString),
        @TPerson.GetName, @TPerson.SetName);
      AddPropertyInfo('Email', GetType(IInfraString),
        @TPerson.GetEmail, @TPerson.SetEmail);
      AddPropertyInfo('Birthday', GetType(IInfraDate),
        @TPerson.GetBirthday, @TPerson.SetBirthday);
      AddPropertyInfo('Address', GetType(IAddress),
          @TPerson.GetAddress, @TPerson.SetAddress);
      AddMethodInfo('GetAge', nil, @TPerson.GetAge);
    end;
  end;
end;

class function TSetupModel.RegisterAddress: IClassInfo;
begin
  with TypeService do
  begin
    Result := AddType(IAddress, 'Address', TAddress,
      IInfraObject, GetType(IInfraObject));
    with Result do
    begin
      AddPropertyInfo('Street', GetType(IInfraString),
        @TAddress.GetStreet, @TAddress.SetStreet);
      AddPropertyInfo('City', GetType(IInfraString),
        @TAddress.GetCity, @TAddress.SetCity);
      AddPropertyInfo('Quarter', GetType(IInfraString),
        @TAddress.GetQuarter, @TAddress.SetQuarter);
      AddPropertyInfo('Number', GetType(IInfraInteger),
        @TAddress.GetNumber, @TAddress.SetNumber);
    end;
  end;
end;

class function TSetupModel.RegisterStudent: IClassInfo;
begin
  with TypeService do
  begin
    Result := AddType(IStudent, 'Student', TStudent,
    IInfraObject, GetType(IPerson));
    with Result do
      AddPropertyInfo('SchoolNumber', GetType(IInfraString),
        @TStudent.GetSchoolNumber, @TStudent.SetSchoolNumber);
  end;
end;

class procedure TSetupModel.RemoveTypeInfo(ID: TGUID);
var
  c: IClassInfo;
  Iterator: IRelationInfoIterator;
begin
  with TypeService do
  begin
    c := GetType(ID, True);
    Iterator := NewRelationsIterator(c);
    while not Iterator.IsDone do
      with Relations do
        Delete(IndexOf(Iterator.CurrentItem));
    with Types do
      Delete(IndexOf(c));
  end;
end;

class procedure TSetupModel.RegisterPersonAddressRelation(
  const Source: IClassInfo);
var
  target: IClassInfo;
  prop: IPropertyInfo;
begin
  with TypeService do
  begin
    target := GetType(IAddress, True);
    prop := Source.GetPropertyInfo('Address', True);
    AddRelation(prop, 1, 1, 0, N, target);
  end;
end;

class function TSetupModel.RegisterMockMethod: IClassInfo;
begin
  with TypeService do
  begin
    Result := AddType(IMockMethod, 'MockMethod', TMockMethod,
      IInfraObject, GetType(IInfraObject));
    with Result do
    begin
      AddConstructorInfo('Create',nil, @TMockMethod.Create);

      AddConstructorInfo('Constructor0', nil, @TMockMethod.Constructor0);

      with AddConstructorInfo('Constructor1', nil, @TMockMethod.Constructor1) do
        AddParam('p1', GetType(IInfraString));

      AddPropertyInfo('Message', GetType(IInfraString),
        @TMockMethod.GetMessage);

      AddMethodInfo('MethodProc0', nil, @TMockMethod.MethodProc0);

      with AddMethodInfo('MethodProc1', nil, @TMockMethod.MethodProc1) do
        AddParam('p1', GetType(IInfraString));

      with AddMethodInfo('MethodProc2', nil, @TMockMethod.MethodProc2) do
      begin
        AddParam('p1', GetType(IInfraString));
        AddParam('p2', GetType(IInfraInteger));
      end;

      with AddMethodInfo('MethodProc3', nil, @TMockMethod.MethodProc3) do
      begin
        AddParam('p1', GetType(IInfraString));
        AddParam('p2', GetType(IInfraInteger));
        AddParam('p3', GetType(IInfraDateTime));
      end;

      with AddMethodInfo('MethodProc4', nil, @TMockMethod.MethodProc4) do
      begin
        AddParam('p1', GetType(IInfraString));
        AddParam('p2', GetType(IInfraInteger));
        AddParam('p3', GetType(IInfraDateTime));
        AddParam('p4', GetType(IInfraBoolean));
      end;

      with AddMethodInfo('MethodProc5', nil, @TMockMethod.MethodProc5) do
      begin
        AddParam('p1', GetType(IInfraString));
        AddParam('p2', GetType(IInfraInteger));
        AddParam('p3', GetType(IInfraDateTime));
        AddParam('p4', GetType(IInfraBoolean));
        AddParam('p5', GetType(IInfraDouble));
      end;

      with AddMethodInfo('MethodFunc0', nil,
        @TMockMethod.MethodFunc0, GetType(IInfraString)) do
        AddParam('Result', GetType(IInfraString));

      with AddMethodInfo('MethodFunc1', nil, @TMockMethod.MethodFunc1,
        GetType(IInfraString)) do
      begin
        AddParam('p1', GetType(IInfraString));
        AddParam('Result', GetType(IInfraString));
      end;

      with AddMethodInfo('MethodFunc2', nil, @TMockMethod.MethodFunc2,
        GetType(IInfraInteger)) do
      begin
        AddParam('p1', GetType(IInfraString));
        AddParam('p2', GetType(IInfraInteger));
        AddParam('Result', GetType(IInfraInteger));
      end;

      with AddMethodInfo('MethodFunc3', nil, @TMockMethod.MethodFunc3,
        GetType(IInfraDateTime)) do
      begin
        AddParam('p1', GetType(IInfraString));
        AddParam('p2', GetType(IInfraInteger));
        AddParam('p3', GetType(IInfraDateTime));
        AddParam('Result', GetType(IInfraDateTime));
      end;

      with AddMethodInfo('MethodFunc4', nil, @TMockMethod.MethodFunc4,
        GetType(IInfraBoolean)) do
      begin
        AddParam('p1', GetType(IInfraString));
        AddParam('p2', GetType(IInfraInteger));
        AddParam('p3', GetType(IInfraDateTime));
        AddParam('p4', GetType(IInfraBoolean));
        AddParam('Result', GetType(IInfraBoolean));
      end;

      with AddMethodInfo('MethodFunc5', nil, @TMockMethod.MethodFunc5,
        GetType(IInfraDouble)) do
      begin
        AddParam('p1', GetType(IInfraString));
        AddParam('p2', GetType(IInfraInteger));
        AddParam('p3', GetType(IInfraDateTime));
        AddParam('p4', GetType(IInfraBoolean));
        AddParam('p5', GetType(IInfraDouble));
        AddParam('Result', GetType(IInfraDouble));
      end;

    end;
  end;
end;

{ TPerson }

procedure TPerson.InfraInitInstance;
begin
  inherited;
  FName := AddProperty('Name') as IInfraString;
  FEmail := AddProperty('Email') as IInfraString;
  FBirthday := AddProperty('Birthday') as IInfraDate;
  FAddress := AddProperty('Address') as IAddress;
end;

function TPerson.GetName: IInfraString;
begin
  Result := FName;
end;

procedure TPerson.SetName(const Value: IInfraString);
begin
  FName := Value;
end;

function TPerson.GetBirthday: IInfraDate;
begin
  Result := FBirthday;
end;

function TPerson.GetEmail: IInfraString;
begin
  Result := FEmail;
end;

procedure TPerson.SetBirthday(const Value: IInfraDate);
begin
  FBirthday := Value;
end;

procedure TPerson.SetEmail(const Value: IInfraString);
begin
  FEmail := Value;
end;

function TPerson.GetAddress: IAddress;
begin
  Result := FAddress;
end;

procedure TPerson.SetAddress(const Value: IAddress);
begin
  FAddress := Value;
end;

function TPerson.GetAge: IInfraInteger;
begin
  if Assigned(FBirthday) then
    Result := TInfraInteger.NewFrom(YearOf(Now) - YearOf(FBirthday.AsDate)); 
end;

{ TAddress }

procedure TAddress.InfraInitInstance;
begin
  inherited;
  FStreet := AddProperty('Street') as IInfraString;
  FCity := AddProperty('City') as IInfraString;
  FQuarter := AddProperty('Quarter') as IInfraString;
  FNumber := AddProperty('Number') as IInfraInteger;
end;

function TAddress.GetCity: IInfraString;
begin
  Result := FCity;
end;

function TAddress.GetNumber: IInfraInteger;
begin
  Result := FNumber;
end;

function TAddress.GetQuarter: IInfraString;
begin
  Result := FQuarter;
end;

function TAddress.GetStreet: IInfraString;
begin
  Result := FStreet;
end;

procedure TAddress.SetCity(const Value: IInfraString);
begin
  FCity := Value;
end;

procedure TAddress.SetNumber(const Value: IInfraInteger);
begin
  FNumber := Value;
end;

procedure TAddress.SetQuarter(const Value: IInfraString);
begin
  FQuarter := Value;
end;

procedure TAddress.SetStreet(const Value: IInfraString);
begin
  FStreet := Value;
end;

{ TStudent }

procedure TStudent.InfraInitInstance;
begin
  inherited;
  FSchoolNumber := AddProperty('SchoolNumber') as IInfraString;
end;

function TStudent.GetSchoolNumber: IInfraString;
begin
  Result := FSchoolNumber;
end;

procedure TStudent.SetSchoolNumber(const Value: IInfraString);
begin
  FSchoolNumber := Value;
end;

{ TMockMethod }

constructor TMockMethod.Create;
begin
  inherited;
end;

constructor TMockMethod.Constructor0;
begin
  FMessage := TInfraString.NewFrom('C0');
end;

constructor TMockMethod.Constructor1(p1: IInfraString);
begin
  FMessage := TInfraString.NewFrom(Format('C1 - %p', [p1.AsString]));
end;

function TMockMethod.GetMessage: IInfraString;
begin
  Result := FMessage;
end;

procedure TMockMethod.InfraInitInstance;
begin
  inherited;
  FMessage := AddProperty('Message') as IInfraString;
end;

function TMockMethod.MethodFunc0: IInfraString;
begin
  Result := TInfraString.NewFrom('TestWithoutParams');
  FMessage.AsString := Format(cMessageFunc0, [Result.AsString]);
end;

function TMockMethod.MethodFunc1(const p1: IInfraString): IInfraString;
begin
  Result := TInfraString.NewFrom('Test1Params');
  FMessage.AsString := Format(cMessageFunc1, [p1.AsString, Result.AsString]);
end;

function TMockMethod.MethodFunc2(const p1: IInfraString;
  const p2: IInfraInteger): IInfraInteger;
begin
  Result := TInfraInteger.NewFrom(55);
  FMessage.AsString := Format(cMessageFunc2,
    [p1.AsString, p2.AsInteger, Result.AsInteger]);
end;

function TMockMethod.MethodFunc3(const p1: IInfraString;
  const p2: IInfraInteger; const p3: IInfraDateTime): IInfraDateTime;
begin
  Result := TInfraDateTime.NewFrom(StrToDateTime('30/3/2007 15:20:12'));
  FMessage.AsString := Format(cMessageFunc3,
    [p1.AsString, p2.AsInteger,
    DateTimeToStr(p3.AsDateTime), DateTimeToStr(Result.AsDateTime)]);
end;

function TMockMethod.MethodFunc4(const p1: IInfraString;
  const p2: IInfraInteger; const p3: IInfraDateTime;
  const p4: IInfraBoolean): IInfraBoolean;
begin
  Result := TInfraBoolean.NewFrom(False);
  FMessage.AsString := Format(cMessageFunc4,
    [p1.AsString, p2.AsInteger,
    DateTimeToStr(p3.AsDateTime),
    SysUtils.BoolToStr(p4.AsBoolean, True),
    SysUtils.BoolToStr(Result.AsBoolean, True)]);
end;

function TMockMethod.MethodFunc5(const p1: IInfraString;
  const p2: IInfraInteger; const p3: IInfraDateTime;
  const p4: IInfraBoolean; const p5: IInfraDouble): IInfraDouble;
begin
  Result := TInfraDouble.NewFrom(125.55);
  FMessage.AsString := Format(cMessageFunc5,
    [p1.AsString, p2.AsInteger, DateTimeToStr(p3.AsDateTime),
    SysUtils.BoolToStr(p4.AsBoolean, True), p5.AsDouble, Result.AsDouble]);
end;

procedure TMockMethod.MethodProc0;
begin
  FMessage := TInfraString.NewFrom(cMessageProc0);
end;

procedure TMockMethod.MethodProc1(const p1: IInfraString);
begin
  FMessage := TInfraString.NewFrom(
    Format(cMessageProc1, [p1.AsString]) );
end;

procedure TMockMethod.MethodProc2(const p1: IInfraString;
  const p2: IInfraInteger);
begin
  FMessage := TInfraString.NewFrom(
    Format(cMessageProc2, [p1.AsString, p2.AsInteger]) );
end;

procedure TMockMethod.MethodProc3(const p1: IInfraString;
  const p2: IInfraInteger; const p3: IInfraDateTime);
begin
  FMessage := TInfraString.NewFrom(
    Format(cMessageProc3, [p1.AsString, p2.AsInteger,
      DateTimeToStr(p3.AsDateTime)]));
end;

procedure TMockMethod.MethodProc4(const p1: IInfraString;
  const p2: IInfraInteger; const p3: IInfraDateTime;
  const p4: IInfraBoolean);
begin
  FMessage := TInfraString.NewFrom(
    Format(cMessageProc4, [p1.AsString, p2.AsInteger,
      DateTimeToStr(p3.AsDateTime), BoolToStr(p4.AsBoolean)]));
end;

procedure TMockMethod.MethodProc5(const p1: IInfraString;
  const p2: IInfraInteger; const p3: IInfraDateTime;
  const p4: IInfraBoolean; const p5: IInfraDouble);
begin
  FMessage := TInfraString.NewFrom(
    Format(cMessageProc5, [p1.AsString, p2.AsInteger,
      DateTimeToStr(p3.AsDateTime), BoolToStr(p4.AsBoolean), p5.AsDouble]));
end;

end.
