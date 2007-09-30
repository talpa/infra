unit AnnotationsModel;

interface

uses
  InfraCommon, InfraCommonIntf, InfraValueTypeIntf, InfraValueType,
  AnnotationsModelIntf;

type
  // Esta classe serve apenas para propósito de teste. Nossas classes poderiam
  // ser registradas todas de uma só vez. Estes registros serão gerados
  // automaticamente por alguma ferramnenta ou wizard na IDE.
  TSetupModel = class
  public
    class function RegisterAddress: IClassInfo;
    class function RegisterVersion: IClassInfo;
    class procedure RemoveTypeInfo(ID: TGUID);
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

  TVersion = class(TElement, IVersion)
  private
    FVersionNumber: IInfraString;
    function GetVersionNumber: IInfraString;
    procedure SetVersionNumber(const Value: IInfraString);
  public
    constructor Create; override;
    property VersionNumber: IInfraString read GetVersionNumber write SetVersionNumber;
  end;

implementation

{ TSetupModel }

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

class function TSetupModel.RegisterVersion: IClassInfo;
begin
  with TypeService do
    Result := AddType(IVersion, 'Version', TVersion, IElement,
      GetType(IElement));
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

{ TVersion }

constructor TVersion.Create;
begin
  inherited;
  FVersionNumber := TInfraString.Create;
  FVersionNumber.AsString := '1.0';
end;

function TVersion.GetVersionNumber: IInfraString;
begin
  Result := FVersionNumber;
end;

procedure TVersion.SetVersionNumber(const Value: IInfraString);
begin
  FVersionNumber := Value;
end;

end.
