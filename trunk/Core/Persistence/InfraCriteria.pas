unit InfraCriteria;

interface

uses
  InfraBase, InfraHibernateIntf, InfraValueTypeIntf, InfraCommonIntf;

type
  TLogicalConnector = (lcOr, lcAnd);

  TRestrictions = class
  protected
    class function DecorateCriterion(const Criterion: ICriterion): ICriterion; virtual;
  public
    class function Equals(const PropertyName: String; const Value: Integer): ICriterion; overload;
    class function Equals(const PropertyName: String; const Value: String): ICriterion; overload;
    class function Equals(const PropertyName: String; const Value: IInfraType): ICriterion; overload;
    class function GreaterThan(const PropertyName: String; const Value: Integer): ICriterion; overload;
    class function GreaterThan(const PropertyName: String; const Value: String): ICriterion; overload;
    class function GreaterThan(const PropertyName: String; const Value: IInfraType): ICriterion; overload;
    class function Between(const PropertyName: String; const FromValue, ToValue: Integer): ICriterion; overload;
    class function Between(const PropertyName: String; const FromValue, ToValue: IInfraType): ICriterion; overload;
    class function CriterionIn(const PropertyName: String; const Values: array of Integer): ICriterion; overload;
    class function CriterionIn(const PropertyName: String; const Values: array of String): ICriterion; overload;
    class function CriterionIn(const PropertyName: String; const Values: array of IInfraType): ICriterion; overload;
    class function CriterionNot(const pCriterion: ICriterion): ICriterion;
    class function CriterionAnd(const pLExp, pRExp: ICriterion): ICriterion;
    class function CriterionOr(const pLExp, pRExp: ICriterion): ICriterion;
  end;

  TNotRestrictions = class(TRestrictions)
  protected
    class function DecorateCriterion(
      const pCriterion: ICriterion): ICriterion; override;
  end;

  TCriterion = class(TInterfacedObject, ICriterion)
  private
    function ToSql(pCriteria: ICriteria;
      pCriteriaQuery: ICriteriaQuery): String;
  protected
    function InternalToSQL: String; virtual; abstract;
  end;

  TPropertyCriterion = class(TCriterion, IPropertyCriterion)
  private
    FPropertyName: String;
    function GetPropertyName: String;
    procedure SetPropertyName(const Value: String);
  public
    constructor Create(const PropertyName: String);
  end;

  TSimpleExpression = class(TPropertyCriterion, ISimpleExpression)
  private
    FValue: IInfraType;
    FCompareOperator: String;
    function GetValue: IInfraType;
    procedure SetValue(const Value: IInfraType);
    function GetCompareOperator: String;
  public
    constructor Create(const PropertyName: String;
      const Value: IInfraType; const CompareOperator: String); reintroduce;
  end;

  TLogicalExpression = class(TInfraBaseObject, ICriterion)
  private
    FLeftExpression: ICriterion;
    FRightExpression: ICriterion;
    FOp: string;
  protected
    function ToSql(pCriteria: ICriteria;
      pCriteriaQuery: ICriteriaQuery): String;
  public
    constructor Create(const pLExp, pRExp: ICriterion;
      const pOp: string); reintroduce;
  end;

  TNotExpression = class(TInfraBaseObject, ICriterion)
  private
    FCriterion: ICriterion;
  protected
    function ToSql(pCriteria: ICriteria;
      pCriteriaQuery: ICriteriaQuery): String;
  public
    constructor Create(const pCriterion: ICriterion); reintroduce;
  end;

  TBetweenCriterion = class(TPropertyCriterion, IBetweenCriterion)
  private
    FFromValue, FToValue: IInfraType;
    function GetFromValue: IInfraType;
    procedure SetFromValue(const Value: IInfraType);
    function GetToValue: IInfraType;
    procedure SetToValue(const Value: IInfraType);
  public
    constructor Create(const PropertyName: String; const FromValue, ToValue: IInfraType); reintroduce;
  end;

  TInCriterion = class(TPropertyCriterion, IInCriterion)
  private
    FItems: IInfraList;
    function GetCount: Integer;
    function GetItem(Index: integer): IInfraType;
  public
    constructor Create(const PropertyName: String; const Values: array of
      IInfraType); reintroduce;
  end;

  TCriteria = class(TInterfacedObject, ICriteria)
  private
    FSession: IInternalSession;
    FCriterionList: ICriterionList;
    FTypeInfo: IClassInfo;
    function GetClassInfo: IClassInfo;
    function Add(const pExpression: ICriterion): ICriteria;
    function AddOr(const pExpression: ICriterion): ICriteria;
    function List: IInfraList;
    function UniqueResult: IInfraType;
  public
    constructor Create(const pEntityInfo: IClassInfo;
      const pSession: IInternalSession);
  end;

  {
  TCriterion = class(TInterfacedObject, ICriterion)
  private
    function ToSQL: String;
  protected
    FSession: ISession;
    function InternalToSQL: String; virtual; abstract;
  end;

  TCompareCriterion = class(TCriterion, ICompareCriterion)
  private
    FPropertyName: String;
    FValue: IInfraType;
    function GetPropertyName: String;
    procedure SetPropertyName(const Value: String);
    function GetValue: IInfraType;
    procedure SetValue(const Value: IInfraType);
  public
    constructor Create(const PropertyName: String;
      const Value: IInfraType); reintroduce;
  end;


  TEqualsCriterion = class(TCompareCriterion)
  end;

  TNotCriterion = class(TCriterion)
  private
    FCriterion: ICriterion;
  public
    constructor Create(const Criterion: ICriterion); reintroduce;
  end;

  TCriterionList = class(TCriterion, ICriterionList)
  private
    FCriterionList: TObjectList;
    function Add(IsOr: Boolean; const Criterion: ICriterion): ICriterionList;
    function NewGroup(IsOr, Negate: Boolean): ICriterionList;
    function AddAnd(const Criterion: ICriterion): ICriterionList;
    function AddOr(const Criterion: ICriterion): ICriterionList;
    function NewGroupAnd(Negate: Boolean = false): ICriterionList;
    function NewGroupOr(Negate: Boolean = false): ICriterionList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TCriteria = class(TInterfacedObject, ICriteria)
  private
    FCriterionList: ICriterionList;
    function AddAnd(const Criterion: ICriterion): ICriterionList;
    function AddOr(const Criterion: ICriterion): ICriterionList;
    function NewGroupAnd(Negate: Boolean = false): ICriterionList;
    function NewGroupOr(Negate: Boolean = false): ICriterionList;    
  public
    constructor Create(const Session: ISession);
  end;
  }
  
implementation

uses List_Criterion, InfraValueType;

type
  TCriterionSubGroupItem = class
    Criterion: ICriterion;
    IsOr: Boolean;
  end;

{ TRestrictions }

class function TRestrictions.Between(const PropertyName: String;
  const FromValue, ToValue: Integer): ICriterion;
begin
  // *** Result := Between(PropertyName, TInfraInteger.NewFrom(FromValue), TInfraInteger.NewFrom(ToValue));
end;

class function TRestrictions.Between(const PropertyName: String;
  const FromValue, ToValue: IInfraType): ICriterion;
begin
  Result := DecorateCriterion(
    TBetweenCriterion.Create(PropertyName, FromValue, ToValue));
end;

class function TRestrictions.CriterionIn(const PropertyName: String;
  const Values: array of Integer): ICriterion;
var
  InfraValues: array of IInfraType;
  i: Integer;
begin
//  SetLength(InfraValues, Length(Values));
//  for i := 0 to High(Values) do
//    InfraValues[i] := TInfraInteger.NewFrom(Values[i]);
//  Result := CriterionIn(PropertyName, InfraValues);
end;

class function TRestrictions.CriterionIn(const PropertyName: String;
  const Values: array of String): ICriterion;
var
  InfraValues: array of IInfraType;
  i: Integer;
begin
//  SetLength(InfraValues, Length(Values));
//  for i := 0 to High(Values) do
//    InfraValues[i] := TInfraString.NewFrom(Values[i]);
//  Result := CriterionIn(PropertyName, InfraValues);
end;

class function TRestrictions.CriterionIn(const PropertyName: String;
  const Values: array of IInfraType): ICriterion;
begin
  Result := DecorateCriterion(TInCriterion.Create(PropertyName, Values));
end;

class function TRestrictions.CriterionAnd(
  const pLExp, pRExp: ICriterion): ICriterion;
begin
  Result := DecorateCriterion(TLogicalExpression.Create(pLExp, pRExp, 'and'));
end;

class function TRestrictions.CriterionOr(
  const pLExp, pRExp: ICriterion): ICriterion;
begin
  Result := DecorateCriterion(TLogicalExpression.Create(pLExp, pRExp, 'or'));
end;

class function TRestrictions.DecorateCriterion(
  const Criterion: ICriterion): ICriterion;
begin
  Result := Criterion;
end;

class function TRestrictions.Equals(const PropertyName: String;
  const Value: Integer): ICriterion;
begin
//  Result := Equals(PropertyName, TInfraInteger.NewFrom(Value));
end;

class function TRestrictions.Equals(const PropertyName: String;
  const Value: IInfraType): ICriterion;
begin
  Result := DecorateCriterion(TSimpleExpression.Create(PropertyName, Value, '='));
end;

class function TRestrictions.GreaterThan(const PropertyName: String;
  const Value: Integer): ICriterion;
begin
//  Result := GreaterThan(PropertyName, TInfraInteger.NewFrom(Value));
end;

class function TRestrictions.GreaterThan(const PropertyName,
  Value: String): ICriterion;
begin
//  Result := GreaterThan(PropertyName, TInfraString.NewFrom(Value));
end;

class function TRestrictions.GreaterThan(const PropertyName: String;
  const Value: IInfraType): ICriterion;
begin
  Result := DecorateCriterion(TSimpleExpression.Create(PropertyName, Value, '>'));
end;

class function TRestrictions.Equals(const PropertyName,
  Value: String): ICriterion;
begin
//  Result := Equals(PropertyName, TInfraString.NewFrom(Value));
end;

class function TRestrictions.CriterionNot(
  const pCriterion: ICriterion): ICriterion;
begin
  Result := DecorateCriterion(TNotExpression.Create(pCriterion));
end;

{ TNotRestrictions }

class function TNotRestrictions.DecorateCriterion(
  const pCriterion: ICriterion): ICriterion;
begin
  Result := TNotExpression.Create(pCriterion);
end;

{ TCompareCriterion }

constructor TSimpleExpression.Create(const PropertyName: String;
  const Value: IInfraType; const CompareOperator: String);
begin
  inherited Create(PropertyName);
  FPropertyName := PropertyName;
  FValue := Value;
  FCompareOperator := CompareOperator;
end;

function TSimpleExpression.GetCompareOperator: String;
begin
  Result := FCompareOperator;
end;

function TSimpleExpression.GetValue: IInfraType;
begin
  Result := FValue;
end;

procedure TSimpleExpression.SetValue(const Value: IInfraType);
begin
  FValue := Value;
end;

{ TLogicalExpression }

constructor TLogicalExpression.Create(const pLExp, pRExp: ICriterion;
  const pOp: string);
begin
  FLeftExpression := pLExp;
  FRightExpression := pRExp;
  FOp := pOp;
end;

function TLogicalExpression.ToSql(pCriteria: ICriteria;
  pCriteriaQuery: ICriteriaQuery): String;
begin
	Result := '(' +
		FLeftExpression.ToSql(pCriteria, pCriteriaQuery) + ' ' +
    FOp + ' ' +
    FRightExpression.ToSql(pcriteria, pCriteriaQuery) + ')';
end;

{ TCriterion }

function TCriterion.ToSql(pCriteria: ICriteria;
  pCriteriaQuery: ICriteriaQuery): String;
begin
  Result := '(' + InternalToSQL + ')';
end;

{ TCriteria }

constructor TCriteria.Create(const pEntityInfo: IClassInfo;
  const pSession: IInternalSession);
begin
  FCriterionList := TCriterionList.Create;
  FSession := pSession;
  FTypeInfo := pEntityInfo;
end;

function TCriteria.Add(const pExpression: ICriterion): ICriteria;
begin
  FCriterionList.Add(pExpression);
  Result := Self;
end;

function TCriteria.AddOr(const pExpression: ICriterion): ICriteria;
begin
  FCriterionList.Add(pExpression);
  Result := Self;
end;

function TCriteria.GetClassInfo: IClassInfo;
begin
  Result := FTypeInfo;
end;

function TCriteria.List: IInfraList;
begin
  Result := FSession.List(Self);
end;

function TCriteria.UniqueResult: IInfraType;
var
  Results: IInfraList;
begin
  Results := List;
  if Results.Count = 0 then
    Result := nil
  else if Results.Count = 1 then
    Result := Results[0];
  // TODO: Lançar uma exceção se mais de um resultado for retornado.
end;

{ TPropertyCriterion }

constructor TPropertyCriterion.Create(const PropertyName: String);
begin
  FPropertyName := PropertyName;
end;

function TPropertyCriterion.GetPropertyName: String;
begin
  Result := FPropertyName;
end;

procedure TPropertyCriterion.SetPropertyName(const Value: String);
begin
  FPropertyName := Value;
end;

{ TBetweenCriterion }

constructor TBetweenCriterion.Create(const PropertyName: String;
  const FromValue, ToValue: IInfraType);
begin
  inherited Create(PropertyName);
  FFromValue := FromValue;
  FToValue := ToValue;
end;

function TBetweenCriterion.GetFromValue: IInfraType;
begin
  Result := FFromValue;
end;

function TBetweenCriterion.GetToValue: IInfraType;
begin
  Result := FToValue;
end;

procedure TBetweenCriterion.SetFromValue(const Value: IInfraType);
begin
  FFromValue := Value;
end;

procedure TBetweenCriterion.SetToValue(const Value: IInfraType);
begin
  FToValue := Value;
end;

{ TInCriterion }

constructor TInCriterion.Create(const PropertyName: String;
  const Values: array of IInfraType);
begin
  inherited Create(PropertyName);
  FItems := TInfraList.Create;
end;

function TInCriterion.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TInCriterion.GetItem(Index: integer): IInfraType;
begin
  Result := FItems[Index] as IInfraType;
end;

{ TNotExpression }

constructor TNotExpression.Create(const pCriterion: ICriterion);
begin
  FCriterion := pCriterion;
end;

function TNotExpression.ToSql(pCriteria: ICriteria;
  pCriteriaQuery: ICriteriaQuery): String;
begin
  // *** aqui deveria pedir uma uma string do dialect. pode ser:
  // "not ( %s )", ou "not %s".
  // no hibernate para mysql tem os parenteses para o resto nao.
  // o parâmetro %s seria substituido por FCriterion.ToSql(...)
  Result := 'not (' + FCriterion.ToSql(pCriteria, pCriteriaQuery) + ')';
end;

(*
procedure InjectSession(const Criterion: ICriterion; const Session: ISession);
  function InterfaceToObject(const I: IInterface): TObject;
  const
    AddByte = $04244483;
    AddLong = $04244481;
  type
    PAdjustSelfThunk = ^TAdjustSelfThunk;
    TAdjustSelfThunk = packed record
      case AddInstruction: longint of
        AddByte : (AdjustmentByte: shortint);
        AddLong : (AdjustmentLong: longint);
      end;
    PInterfaceMT = ^TInterfaceMT;
    TInterfaceMT = packed record
      QueryInterfaceThunk: PAdjustSelfThunk;
    end;
    TInterfaceRef = ^PInterfaceMT;
  var
    QueryInterfaceThunk: PAdjustSelfThunk;
  begin
    Result := Pointer(I);
    if Assigned(Result) then
      try
        QueryInterfaceThunk := TInterfaceRef(I)^.QueryInterfaceThunk;
        case QueryInterfaceThunk.AddInstruction of
          AddByte: Inc(PChar(Result), QueryInterfaceThunk.AdjustmentByte);
          AddLong: Inc(PChar(Result), QueryInterfaceThunk.AdjustmentLong);
        else
          Result := nil;
        end;
      except
        Result := nil;
      end;
  end;
var
  CriterionObject: TCriterion;
begin
  CriterionObject := InterfaceToObject(Criterion) as TCriterion;
  CriterionObject.FSession := Session;
end;

{ TNotCriterion }

constructor TNotCriterion.Create(const Criterion: ICriterion);
begin
  InjectSession(Criterion, FSession);
  FCriterion := Criterion;
end;

{ TCompareCriterion }

constructor TCompareCriterion.Create(const PropertyName: String;
  const Value: IInfraType);
begin
  FPropertyName := PropertyName;
  FValue := Value;
end;

function TCompareCriterion.GetPropertyName: String;
begin
  Result := FPropertyName;
end;                           

function TCompareCriterion.GetValue: IInfraType;
begin
  Result := FValue;
end;

procedure TCompareCriterion.SetPropertyName(const Value: String);
begin
  FPropertyName := Value;
end;

procedure TCompareCriterion.SetValue(const Value: IInfraType);
begin
  FValue := Value;
end;

{ TCriterionSubGroup }

function TCriterionList.Add(IsOr: Boolean; const Criterion: ICriterion): ICriterionList;
var
  Item: TCriterionSubGroupItem;
begin
  InjectSession(Criterion, FSession);
  Item := TCriterionSubGroupItem.Create;
  Item.Criterion := Criterion;
  Item.IsOr := IsOr;
  FCriterionList.Add(Item);
  Result := Self;
end;

function TCriterionList.AddAnd(
  const Criterion: ICriterion): ICriterionList;
begin
  Result := Add(false, Criterion);
end;

function TCriterionList.AddOr(
  const Criterion: ICriterion): ICriterionList;
begin
  Result := Add(true, Criterion);
end;

constructor TCriterionList.Create;
begin
  FCriterionList := TObjectList.Create(True);
end;

destructor TCriterionList.Destroy;
begin
  FCriterionList.Free;
  inherited;
end;

function TCriterionList.NewGroup(IsOr, Negate: Boolean): ICriterionList;
begin
  Result := TCriterionList.Create;
  if Negate then
    Add(IsOr, TNotCriterion.Create(Result))
  else
    Add(IsOr, Result);
end;

function TCriterionList.NewGroupAnd(Negate: Boolean): ICriterionList;
begin
  Result := NewGroup(false, Negate);
end;

function TCriterionList.NewGroupOr(Negate: Boolean): ICriterionList;
begin
  Result := NewGroup(True, Negate);
end;

{ TCriterion }

function TCriterion.ToSQL: String;
begin
  Result := '(' + InternalToSQL + ')';
end;

{ TCriteria }

function TCriteria.AddAnd(const Criterion: ICriterion): ICriterionList;
begin
  Result := FCriterionList.AddAnd(Criterion);
end;

function TCriteria.AddOr(const Criterion: ICriterion): ICriterionList;
begin
  Result := FCriterionList.AddOr(Criterion);
end;

constructor TCriteria.Create(const Session: ISession);
begin
  FCriterionList := TCriterionList.Create;
  InjectSession(FCriterionList, Session);
end;

function TCriteria.NewGroupAnd(Negate: Boolean): ICriterionList;
begin
  Result := FCriterionList.NewGroupAnd(Negate);
end;

function TCriteria.NewGroupOr(Negate: Boolean): ICriterionList;
begin
  Result := FCriterionList.NewGroupOr(Negate);
end;
*)
end.
