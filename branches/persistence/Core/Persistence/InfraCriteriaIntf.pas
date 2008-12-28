unit InfraCriteriaIntf;

interface

uses
  InfraValueType, InfraValueTypeIntf;

type
  ICriteria = interface;
  ICriterion = interface;
  ICriterionList = interface;

  ICriterion = interface
    ['{7AA21539-12C6-4923-8369-40D9DE05F0CD}']
    function ToSQL: String;
  end;

  ICompareCriterion = interface(ICriterion)
    function GetPropertyName: String;
    procedure SetPropertyName(const Value: String);
    function GetValue: IInfraType;
    procedure SetValue(const Value: IInfraType);
    property PropertyName: String read GetPropertyName write SetPropertyName;
    property Value: IInfraType read GetValue write SetValue;
  end;

  ICriterionList = interface(ICriterion)
    ['{64872EC1-91E3-4A18-BB34-A114A6B9DC0E}']
    function AddAnd(const Criterion: ICriterion): ICriterionList;
    function AddOr(const Criterion: ICriterion): ICriterionList;
    function NewGroupAnd(Negate: Boolean = false): ICriterionList;
    function NewGroupOr(Negate: Boolean = false): ICriterionList;
  end;

  ICriteria = interface
    ['{F49EE12B-0EAE-415A-B9CF-A7EF8C403D03}']
    function AddAnd(const Criterion: ICriterion): ICriterionList;
    function AddOr(const Criterion: ICriterion): ICriterionList;
    function NewGroupAnd(Negate: Boolean = false): ICriterionList;
    function NewGroupOr(Negate: Boolean = false): ICriterionList;
  end;

  TRestrictions = class
  protected
    class function DecorateCriterion(const Criterion: ICriterion): ICriterion; virtual;
  public
    class function Equals(const PropertyName: String; const Value: Integer): ICriterion; overload;
    class function Equals(const PropertyName: String; const Value: String): ICriterion; overload;
    class function Equals(const PropertyName: String; const Value: IInfraType): ICriterion; overload;
    class function Negate(const Criterion: ICriterion): ICriterion;
    class function CriterionAnd(const CriterionList: array of ICriterion): ICriterionList;
    class function CriterionOr(const CriterionList: array of ICriterion): ICriterionList;
  end;

  TNotRestrictions = class(TRestrictions)
  protected
    class function DecorateCriterion(const Criterion: ICriterion): ICriterion; override;
  end;

implementation

uses InfraCriteria;

{ TRestrictions }

class function TRestrictions.CriterionAnd(
  const CriterionList: array of ICriterion): ICriterionList;
var
  i: Integer;
begin
  Result := TCriterionList.Create;
  for i := 0 to High(CriterionList) do
    Result.AddAnd(CriterionList[i]);
end;

class function TRestrictions.CriterionOr(
  const CriterionList: array of ICriterion): ICriterionList;
var
  i: Integer;
begin
  Result := TCriterionList.Create;
  for i := 0 to High(CriterionList) do
    Result.AddOr(CriterionList[i]);
end;

class function TRestrictions.DecorateCriterion(
  const Criterion: ICriterion): ICriterion;
begin
  Result := Criterion;
end;

class function TRestrictions.Equals(const PropertyName: String;
  const Value: Integer): ICriterion;
begin
  Result := Equals(PropertyName, TInfraInteger.NewFrom(Value));
end;

class function TRestrictions.Equals(const PropertyName: String;
  const Value: IInfraType): ICriterion;
begin
  Result := DecorateCriterion(TEqualsCriterion.Create(PropertyName, Value));
end;

class function TRestrictions.Equals(const PropertyName,
  Value: String): ICriterion;
begin
  Result := Equals(PropertyName, TInfraString.NewFrom(Value));
end;

class function TRestrictions.Negate(const Criterion: ICriterion): ICriterion;
begin
  Result := DecorateCriterion(TNotCriterion.Create(Criterion));
end;

{ TNotRestrictions }

class function TNotRestrictions.DecorateCriterion(
  const Criterion: ICriterion): ICriterion;
begin
  Result := TNotCriterion.Create(Criterion);
end;

end.
