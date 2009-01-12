unit InfraValueType;

interface

{$I InfraTypes.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Classes,
  Contnrs,
  InfraCommonIntf,
  InfraCommon,
  InfraBasicList,
  InfraNotify,
  InfraValueTypeIntf;

type
  {! Classe base para ValueTypes }
  TInfraType = class(TElement, IInfraType, IProperty)
  private
    FChanged: boolean;
    FIsDerived: Boolean;
    FIsNull: Boolean;
    FOwner: IElement;
  protected
    {! Metodo usado para calculos internos }
    procedure Calculate; virtual;
    {! Metodo usado para criar uma copia da instância do value type }
    function Clone: IInfraType; virtual;
    {! Metodo usado para verificar se os objetos são iguais }
    function Equals(const Obj: IInfraType): Boolean; virtual;
    function GetIsDerived: Boolean;
    function GetIsNull: Boolean; virtual;
    function GetName: string;
    function GetOwner: IElement; virtual;
    procedure Assign(const Source: IInfraType); virtual; abstract;
    procedure Clear; virtual;
    procedure Changed; virtual;
    procedure InvalidateCache; virtual;
    procedure SetIsDerived(const Value: Boolean);
    procedure SetIsNull(Value: Boolean);
    procedure SetOwner(const Value: IElement); virtual;
    procedure UpdateCache;
    procedure ValidateCache;
    property IsDerived: boolean read GetIsDerived write SetIsDerived;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property Name: string read GetName;
    property Owner: IElement read GetOwner write SetOwner;
  public
    procedure InfraInitInstance; override;
  end;

  {! Classe que representa cada elemento do InfraEnumeration }
  TInfraLiteral = class(TElement, IInfraLiteral)
  private
    FName: string;
  protected
    function GetName: string;
    procedure SetName(const Value: string);
    property Name: string read GetName write SetName;
  end;

  {! Classe ValueType para representação de uma string }
  TInfraString = class(TInfraType, IInfraString)
  private
    FValue: string;
  protected
    function Equals(const Obj: IInfraType): Boolean; override;
    function GetAsString: string;
    procedure Assign(const Source: IInfraType); override;
    procedure Clear; override;
    procedure SetAsString(const Value: string);
    property AsString: string read GetAsString write SetAsString;
  public
    {! Metodo que instancia um valuetype TInfraString a partir de uma string }
    class function NewFrom(const Value: String): IInfraString;
  end;

  {! Classe ValueType para representação de um double }
  TInfraDouble = class(TInfraType, IInfraDouble)
  private
    FValue: Double;
  protected
    function Equals(const Obj: IInfraType): Boolean; override;
    function GetAsDouble: Double;
    procedure Assign(const Source: IInfraType); override;
    procedure Clear; override;
    procedure SetAsDouble(const Value: Double);
    property AsDouble: Double read GetAsDouble write SetAsDouble;
  public
    {! Metodo que instancia um valuetype TInfraDouble a partir de uma double }
    class function NewFrom(const Value: Double): IInfraDouble;
  end;

  {! Classe ValueType para representação de um variant }
  TInfraVariant = class(TInfraType, IInfraVariant)
  private
    FValue: Variant;
  protected
    function Equals(const Obj: IInfraType): Boolean; override;
    function GetAsVariant: Variant;
    procedure Assign(const Source: IInfraType); override;
    procedure Clear; override;
    procedure SetAsVariant(const Value: Variant);
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  public
    {! Metodo que instancia um valuetype TInfraVariant a partir de uma variant }
    class function NewFrom(const Value: Variant): IInfraVariant;
  end;

  {! Classe ValueType para representação de um integer }
  TInfraInteger = class(TInfraType, IInfraInteger)
  private
    FValue: Integer;
  protected
    function Equals(const Obj: IInfraType): Boolean; override;
    function GetAsInteger: Integer;
    procedure Assign(const Source: IInfraType); override;
    procedure Clear; override;
    procedure SetAsInteger(const Value: Integer);
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
  public
    {! Metodo que instancia um valuetype TInfraInteger a partir de uma integer }
    class function NewFrom(const Value: Integer): IInfraInteger;
  end;

  {! Classe ValueType para representação de um boolean }
  TInfraBoolean = class(TInfraType, IInfraBoolean)
  private
    FValue: boolean;
  protected
    function GetAsBoolean: boolean;
    procedure Assign(const Source: IInfraType); override;
    procedure SetAsBoolean(Value: boolean);
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
  public
    {! Metodo que instancia um valuetype TInfraBoolean a partir de um boolean }
    class function NewFrom(const Value: Boolean): IInfraBoolean;
  end;

  {! Classe ValueType para representação de datetime }
  TInfraDateTime = class(TInfraType, IInfraDateTime, IInfraTime, IInfraDate)
  private
    FValue: TDateTime;
  protected
    function GetAsDate: TDateTime;
    function GetAsDateTime: TDateTime;
    function GetAsTime: TDateTime;
    procedure Assign(const Source: IInfraType); override;
    procedure SetAsDate(Value: TDateTime);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsTime(Value: TDateTime);
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
  public
    {! Metodo que instancia um valuetype TInfraDateTime a partir de uma datetime }
    class function NewFrom(const Value: TDateTime): IInfraDateTime;
  end;

  {! Classe ValueType para representação de time }
  TInfraTime = class(TInfraType, IInfraTime)
  private
    FValue: TDateTime;
  protected
    function GetAsTime: TDateTime;
    procedure Assign(const Source: IInfraType); override;
    procedure SetAsTime(Value: TDateTime);
    property AsTime: TDateTime read GetAsTime write SetAsTime;
  public
    {! Metodo que instancia um valuetype TInfraTime a partir de um datetime }
    class function NewFrom(const Value: TDateTime): IInfraTime;
  end;

  {! Classe ValueType para representação de date }
  TInfraDate = class(TInfraType, IInfraDate)
  private
    FValue: Double;
  protected
    function GetAsDate: TDateTime;
    procedure Assign(const Source: IInfraType); override;
    procedure SetAsDate(Value: TDateTime);
    property AsDate: TDateTime read GetAsDate write SetAsDate;
  public
    {! Metodo que instancia um valuetype TInfraDate a partir de um datetime }
    class function NewFrom(const Value: TDateTime): IInfraDate;
  end;

  {! Classe ValueType para manter uma lista de outros ValueTypes }
  TInfraList = class(TInfraType, IInfraList, IInterfaceList)
  private
    FItems: TInfraCustomList;
    {$HINTS OFF}
    property InternalItems: TInfraCustomList read FItems
      write FItems implements IInterfaceList;
    {$HINTS ON}
  protected
    function Add(const Item: IInfraType): Integer;
    function Append(const Item: IInfraType): IInfraType;
    function Clone: IInfraType; override;
    function Equals(const Obj: IInfraType): Boolean; override;
    function First: IInfraType;
    function GetCount: Integer;
    function GetIsNull: Boolean; override;
    function GetItem(Index: Integer): IInfraType;
    function IndexOf(const Item: IInfraType): Integer;
    function Last: IInfraType;
    function Remove(const Item: IInfraType): Integer;
    procedure Assign(const Source: IInfraType); override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; const Item: IInfraType);
    procedure Lock;
    procedure SetCount(NewCount: Integer);
    procedure SetItem(Index: Integer; const Item: IInfraType);
    procedure Unlock;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IInfraType read GetItem write SetItem;
  public
    procedure InfraInitInstance; override;
    destructor Destroy; override;
    class function NewFrom(const Value: array of IInfraType): IInfraList;
  end;

  TInfraStream = class(TInfraType, IInfraStream)
  private
    FStream: TMemoryStream;
  protected
    function GetAsStream: TMemoryStream;
    procedure SetAsStream(const Value: TMemoryStream);
    property AsStream: TMemoryStream read GetAsStream;
  public
    class function NewFrom(const Value: TMemoryStream): IInfraStream;
  end;

  TBucketProc = procedure(AInfo, AItem, AData: Pointer; out AContinue: Boolean);

  TInfraMap = class(TInfraType, IInfraMap)
  private
    FItems: TBucketList;
    FValueFound: Boolean;
    FValueCallBack: Pointer;
    FKeyFound: IInfraType;
  protected
    function ContainsKey(const Key: IInfraType): boolean;
    function ContainsValue(const Value: IInfraType): boolean;
    function GetItem(const Key: IInfraType): IInfraType;
    function GetKeyByValue(const Value: IInfraType): IInfraType;
    function Keys: IInfraList;
    function Values: IInfraList;
    procedure Assign(const Source: IInfraType); override;
    procedure SetItem(const Key: IInfratype; Value: IInfraType);
  public
    property Items[const Key: IInfraType]: IInfraType read GetItem
      write SetItem; default;
    procedure InfraInitInstance; override;
    destructor Destroy; override;
  end;

  {! Classe ValueType para criação de objetos complexos }
  TInfraObject = class(TInfraType, IInfraObject)
  protected
    function AddProperty(const PropertyName: string): IProperty;
    function Equals(const Obj: IInfraType): Boolean; override;
    function GetProperty(const PropertyName: string): IProperty;
    procedure Assign(const Source: IInfraType); override;
  public
    procedure InfraInitInstance; override;
  end;

  {! Classe ValueType para criação enumeration }
  TInfraEnumeration = class(TInfraType, IInfraEnumeration)
  private
    FLiterals: IInfraLiteralList;
    FValue: IInfraLiteral;
  protected
    function Equals(const Obj: IInfraType): Boolean; override;
    function GetAsLiteral: IInfraLiteral;
    function GetLiterals: IInfraLiteralList;
    procedure SetAsLiteral(Value: IInfraLiteral);
    property AsLiteral: IInfraLiteral read GetAsLiteral write SetAsLiteral;
    property Literals: IInfraLiteralList read GetLiterals;
  end;

  {! Classe ValueType wrapper de objetos nativos }
  TInfraNativeObject = class(TInfraType, IInfraNativeObject)
  private
    FValue: TObject;
  protected
    function Equals(const Obj: IInfraType): Boolean; override;
    function GetAsNativeObject: TObject;
    procedure Assign(const Source: IInfraType); override;
    procedure Clear; override;
    procedure SetAsNativeObject(const Value: TObject);
    property AsNativeObject: TObject read GetAsNativeObject
      write SetAsNativeObject;
  public
    class function NewFrom(
      const Value: TObject): IInfraNativeObject;
  end;

procedure AssignMap(AInfo, AItem, AData: Pointer; out AContinue: Boolean);
procedure ExistValue(AInfo, AItem, AData: Pointer; out AContinue: Boolean);
procedure GetMapKeys(AInfo, AItem, AData: Pointer; out AContinue: Boolean);
procedure GetMapValues(AInfo, AItem, AData: Pointer; out AContinue: Boolean);

implementation

uses
  SysUtils,
  Variants,
  InfraValueTypeEvent;

{ TInfraType }

procedure TInfraType.InfraInitInstance;
begin
  inherited;
  FIsNull := True;
end;

procedure TInfraType.Clear;
begin
  FIsNull := True;
end;

function TInfraType.Equals(const Obj: IInfraType): Boolean;
begin
  Result := (TypeInfo = Obj.TypeInfo) and (IsNull = Obj.IsNull);
end;

function TInfraType.GetIsDerived: Boolean;
begin
  Result := FIsDerived;
end;

function TInfraType.GetIsNull: Boolean;
begin
  Result := FIsNull;
end;

function TInfraType.GetName: string;
begin
  if Assigned(TypeInfo) then
    Result := TypeInfo.Name
  else
    Result := '';
end;

function TInfraType.GetOwner: IElement;
begin
  Result := FOwner;
end;

procedure TInfraType.InvalidateCache;
begin
  FChanged := True;
  if Publisher.HasSubscribers(IInfraChangedEvent) then
    Publisher.Publish(TInfraChangedEvent.Create(Self) as IInfraChangedEvent);
end;

procedure TInfraType.SetIsDerived(const Value: Boolean);
begin
  if FIsDerived <> Value then
    FIsDerived := Value;
end;

procedure TInfraType.SetIsNull(Value: Boolean);
begin
  if FIsNull <> Value then
    FIsNull := Value;
end;

procedure TInfraType.SetOwner(const Value: IElement);
begin
  SetReference(IInterface(FOwner), Value);
end;

procedure TInfraType.UpdateCache;
begin
  if FIsDerived and FChanged then
    ValidateCache;
end;

procedure TInfraType.ValidateCache;
begin
  Calculate;
  FChanged := False;
end;

function TInfraType.Clone: IInfraType;
begin
  Result := TInfraType(ClassType.Create);
  if not IsNull then
    Result.Assign(Self);
end;

procedure TInfraType.Calculate;
begin
  // nothing
end;

procedure TInfraType.Changed;
begin
  FIsNull := False;
  Publisher.Publish(TInfraChangedEvent.Create(
    Self as IElement) as IInfraChangedEvent);
end;

{ TInfraLiteral }

function TInfraLiteral.GetName: string;
begin
  Result := FName;
end;

procedure TInfraLiteral.SetName(const Value: string);
begin
  if Value <> FName then
    FName := Value;
end;

{ TInfraString }

class function TInfraString.NewFrom(const Value: String): IInfraString;
begin
  Result := Self.Create;
  Result.AsString := Value;
end;

procedure TInfraString.Assign(const Source: IInfraType);
begin
  if (Source <> nil) and Supports(Source, IInfraString) then
    SetAsString((Source as IInfraString).AsString)
  else
    inherited Assign(Source);
end;

procedure TInfraString.Clear;
begin
  FValue := '';
  inherited;
end;

function TInfraString.Equals(const Obj: IInfraType): Boolean;
begin
  Result := inherited Equals(Obj) and (AsString = IInfraString(Obj).AsString);
end;

function TInfraString.GetAsString: string;
begin
  Result := FValue;
end;

procedure TInfraString.SetAsString(const Value: string);
begin
  if (Value <> FValue) or IsNull then
  begin
    FValue := Value;
    Changed;
  end;
end;

{ TInfraDouble }

class function TInfraDouble.NewFrom(const Value: Double): IInfraDouble;
begin
  Result := Self.Create;
  Result.AsDouble := Value;
end;

procedure TInfraDouble.Assign(const Source: IInfraType);
begin
  if (Source <> nil) and Supports(Source, IInfraDouble) then
    SetAsDouble((Source as IInfraDouble).AsDouble)
  else
    inherited Assign(Source);
end;

procedure TInfraDouble.Clear;
begin
  FValue := 0;
  inherited;
end;

function TInfraDouble.Equals(const Obj: IInfraType): Boolean;
begin
  Result := inherited Equals(Obj) and (AsDouble = IInfraDouble(Obj).AsDouble);
end;

function TInfraDouble.GetAsDouble: Double;
begin
  Result := FValue;
end;

procedure TInfraDouble.SetAsDouble(const Value: Double);
begin
  if (Value <> FValue) or IsNull then
  begin
    FValue := Value;
    Changed;
  end;
end;

{ TInfraVariant }
   
class function TInfraVariant.NewFrom(
  const Value: Variant): IInfraVariant;
begin
  Result := Self.Create;      
  Result.AsVariant := Value;
end;

procedure TInfraVariant.Assign(const Source: IInfraType);
begin
  if (Source <> nil) and Supports(Source, IInfraVariant) then
    SetAsVariant((Source as IInfraVariant).AsVariant)
  else
    inherited Assign(Source);
end;

procedure TInfraVariant.Clear;
begin
  FValue := null;
  inherited;
end;

function TInfraVariant.Equals(const Obj: IInfraType): Boolean;
begin
  Result := inherited Equals(Obj) and
    (AsVariant = IInfraVariant(Obj).AsVariant);
end;

function TInfraVariant.GetAsVariant: Variant;
begin
  Result := FValue;
end;

procedure TInfraVariant.SetAsVariant(const Value: Variant);
begin
  if (Value <> FValue) or IsNull then
  begin
    FValue := Value;
    Changed;
  end;
end;

{ TInfraInteger }
   
class function TInfraInteger.NewFrom(
  const Value: Integer): IInfraInteger;
begin
  Result := Self.Create;
  Result.AsInteger := Value;
end;

procedure TInfraInteger.Assign(const Source: IInfraType);
begin
  if (Source <> nil) and Supports(Source, IInfraInteger) then
    SetAsInteger((Source as IInfraInteger).AsInteger)
  else
    inherited Assign(Source);
end;

procedure TInfraInteger.Clear;
begin
  FValue := 0;
  inherited;
end;

function TInfraInteger.Equals(const Obj: IInfraType): Boolean;
begin
  Result := inherited Equals(Obj) and (AsInteger = IInfraInteger(Obj).AsInteger);
end;

function TInfraInteger.GetAsInteger: Integer;
begin
  Result := FValue;
end;

procedure TInfraInteger.SetAsInteger(const Value: Integer);
begin
  if (Value <> FValue) or IsNull then
  begin
    FValue := Value;
    Changed;
  end;
end;

{ TInfraBoolean }

class function TInfraBoolean.NewFrom(
  const Value: Boolean): IInfraBoolean;
begin
  Result := Self.Create;    
  Result.AsBoolean := Value;
end;

procedure TInfraBoolean.Assign(const Source: IInfraType);
begin
  if (Source <> nil) and Supports(Source, IInfraBoolean) then
    SetAsBoolean((Source as IInfraBoolean).AsBoolean)
  else
    inherited Assign(Source);
end;

function TInfraBoolean.GetAsBoolean: boolean;
begin
  Result := FValue;
end;

procedure TInfraBoolean.SetAsBoolean(Value: boolean);
begin
  if (Value <> FValue) or IsNull then
  begin
    FValue := Value;
    Changed;
  end;
end;

{ TInfraDateTime }

function TInfraDateTime.GetAsDateTime: TDateTime;
begin
  Result := FValue;
end;

procedure TInfraDateTime.SetAsDateTime(Value: TDateTime);
begin
  if (Value <> FValue) or IsNull then
  begin
    FValue := Value;
    Changed;
  end;
end;

function TInfraDateTime.GetAsDate: TDateTime;
begin
  Result := FValue;
end;

procedure TInfraDateTime.SetAsDate(Value: TDateTime);
begin
  if (Value <> FValue) or IsNull then
  begin
    FValue := Value;
    Changed;
  end;
end;

function TInfraDateTime.GetAsTime: TDateTime;
begin
  Result := FValue;
end;

procedure TInfraDateTime.SetAsTime(Value: TDateTime);
begin
  if (Value <> FValue) or IsNull then
  begin
    FValue := Value;
    Changed;
  end;
end;

procedure TInfraDateTime.Assign(const Source: IInfraType);
begin
  if (Source <> nil) then
  begin
    if Supports(Source, IInfraDateTime) then
      SetAsDateTime((Source as IInfraDateTime).AsDateTime)
    else if Supports(Source, IInfraDate) then
      SetAsDateTime((Source as IInfraDate).AsDate)
    else if Supports(Source, IInfraTime) then
      SetAsDateTime((Source as IInfraTime).AsTime)
    else
      inherited Assign(Source);
  end else
    inherited Assign(Source);
end;

class function TInfraDateTime.NewFrom(
  const Value: TDateTime): IInfraDateTime;
begin
  Result := Self.Create;
  Result.AsDateTime := Value;
end;

{ TInfraDate }

class function TInfraDate.NewFrom(const Value: TDateTime): IInfraDate;
begin
  Result := Self.Create;   
  Result.AsDate := Value;
end;

procedure TInfraDate.Assign(const Source: IInfraType);
begin
  if (Source <> nil) and Supports(Source, IInfraDate) then
    SetAsDate((Source as IInfraDate).AsDate)
  else
    inherited Assign(Source);
end;

function TInfraDate.GetAsDate: TDateTime;
begin
  Result := FValue;
end;

procedure TInfraDate.SetAsDate(Value: TDateTime);
begin
  if (Value <> FValue) or IsNull then
  begin
    FValue := Value;
    Changed;
  end;
end;

{ TInfraTime }

class function TInfraTime.NewFrom(const Value: TDateTime): IInfraTime;
begin
  Result := Self.Create;
  Result.AsTime := Value;
end;

procedure TInfraTime.Assign(const Source: IInfraType);
begin
  if (Source <> nil) and Supports(Source, IInfraTime) then
    SetAsTime((Source as IInfraTime).AsTime)
  else
    inherited Assign(Source);
end;

function TInfraTime.GetAsTime: TDateTime;
begin
  Result := FValue;
end;

procedure TInfraTime.SetAsTime(Value: TDateTime);
begin
  if (Value <> FValue) or IsNull then
  begin
    FValue := Value;
    Changed;
  end;
end;

{ TInfraList }

procedure TInfraList.InfraInitInstance;
begin
  inherited;
  FItems := TInfraCustomList.Create;
end;

destructor TInfraList.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TInfraList.Add(const Item: IInfraType): Integer;
var
  AddEvent: IInfraAddItemEvent;
begin
  with Publisher do
  begin
    AddEvent := TInfraAddItemEvent.Create(Self, Item, -1);
    Result := FItems.Add(Item);
    AddEvent.ItemIndex := Result;
    Publish(AddEvent);
    Publish(TInfraChangedEvent.Create(Self) as IInfraChangedEvent);
  end;
end;

function TInfraList.Clone: IInfraType;
var
  i: Integer;
  NewList: IInfraList;
begin
  NewList := TInfraList(ClassType.Create);
  for i := 0 to Count -1 do
    NewList.Add(Items[i].Clone);
  Result := NewList;
end;

procedure TInfraList.Assign(const Source: IInfraType);
var
  i: Integer;
  Intf: IInfraList;
begin
  if (Source <> nil) and Supports(Source, IInfraList, Intf) then
  begin
    Clear;
    FItems.Count := Intf.Count;
    for i := 0 to Intf.Count -1 do
      FItems[i] := Intf[i];
  end else
    inherited Assign(Source);
end;

procedure TInfraList.Clear;
begin
  with Publisher do
  begin
    FItems.Clear;
    Inherited;
    Publish(TInfraClearListEvent.Create(Self) as IInfraClearListEvent);
    Publish(TInfraChangedEvent.Create(Self) as IInfraChangedEvent);
  end;
end;

procedure TInfraList.Delete(Index: Integer);
var
  RemoveEvent: IInfraRemoveItemEvent;
begin
  with Publisher do
  begin
    RemoveEvent := TInfraRemoveItemEvent.Create(Self, Items[Index], Index);
    FItems.Delete(Index);
    Publish(RemoveEvent);
    Publish(TInfraChangedEvent.Create(Self) as IInfraChangedEvent);
  end;
end;

function TInfraList.Equals(const Obj: IInfraType): Boolean;
var
  i: Integer;
  TheList: IInfraList;
  TheObj: IInfraObject;
begin
  Result := inherited Equals(Obj);
  TheList := Obj as IInfraList;
  Result := Result or (Count = TheList.Count);
  i := 0;
  while Result and (i < Count) do
  begin
    TheObj := (TheList.Items[i] as IInfraObject);
    Result := Result and
      Items[i].Equals(TheObj);
    Inc(i);
  end;
end;

procedure TInfraList.Exchange(Index1, Index2: Integer);
begin
  FItems.Exchange(Index1, Index2);
  Publisher.Publish(TInfraChangedEvent.Create(Self) as IInfraChangedEvent);
end;

function TInfraList.First: IInfraType;
begin
  Result := (FItems.First as IInfraType);
end;

function TInfraList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TInfraList.GetIsNull: Boolean;
begin
  Result := FItems.Count = 0;
end;

function TInfraList.GetItem(Index: Integer): IInfraType;
begin
  Result := (FItems[Index] as IInfraType);
end;

function TInfraList.IndexOf(const Item: IInfraType): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

procedure TInfraList.Insert(Index: Integer; const Item: IInfraType);
var
  AddEvent: IInfraAddItemEvent;
begin
  with Publisher do
  begin
    AddEvent := TInfraAddItemEvent.Create(Self, Item, Index);
    FItems.Insert(Index, Item);
    Publish(AddEvent);
    Publish(TInfraChangedEvent.Create(Self) as IInfraChangedEvent);
  end;
end;

function TInfraList.Last: IInfraType;
begin
  Result := (FItems.Last as IInfraType);
end;

procedure TInfraList.Lock;
begin
  FItems.Lock;
end;

function TInfraList.Remove(const Item: IInfraType): Integer;
var
  RemoveEvent: IInfraRemoveItemEvent;
begin
  with Publisher do
  begin
    RemoveEvent := TInfraRemoveItemEvent.Create(Self, Item, IndexOf(Item));
    Result := FItems.Remove(Item);
    Publish(RemoveEvent);
    Publish(TInfraChangedEvent.Create(Self) as IInfraChangedEvent);
  end;
end;

procedure TInfraList.SetCount(NewCount: Integer);
begin
  FItems.Count := NewCount;
  Publisher.Publish(TInfraChangedEvent.Create(Self) as IInfraChangedEvent);
end;

procedure TInfraList.SetItem(Index: Integer; const Item: IInfraType);
begin
  FItems[Index] := Item;
end;

procedure TInfraList.Unlock;
begin
  FItems.Unlock;
end;

function TInfraList.Append(const Item: IInfraType): IInfraType;
var
  i: Integer;
begin
  i := Add(Item);
  if i <> -1 then
    Result := Item
  else
    Result := nil;
end;

class function TInfraList.NewFrom(
  const Value: array of IInfraType): IInfraList;
var
  i: integer;
begin
  Result := Self.Create;
  for i := Low(Value) to High(Value) do
    Result.Add(Value[i]);
end;

{ TInfraObject }

function TInfraObject.AddProperty(const PropertyName: string): IProperty;
var
  PropertyInfo: IPropertyInfo;
  PropertyType: IClassInfo;
begin
  Result := GetProperty(PropertyName);
  if not Assigned(Result) then
  begin
    PropertyInfo := TypeInfo.GetPropertyInfo(PropertyName, True);
    PropertyType := PropertyInfo.GetTypeInfo;
    if Assigned(PropertyType) then
    begin
      Result := TypeService.CreateInstance(PropertyType) as IProperty;
      if Assigned(Result) then
        Result.Owner := Self as IElement;
    end else
      raise Exception.Create('Cannot Instanciate '+PropertyName+
        '. ClassInfo not registred!');
  end else
    raise Exception.Create('Property '+PropertyName+
      ' exist already!');
end;

procedure TInfraObject.Assign(const Source: IInfraType);
var
  Iterator: IPropertyInfoIterator;
  Obj: IInfraObject;
  PropertyName: string;
begin
  if (Source <> nil)
    and Supports(Source, IInfraObject, Obj) then
  begin
    Iterator := TypeInfo.GetProperties;
    while not Iterator.IsDone do
    begin
      PropertyName := Iterator.CurrentItem.Name;
      GetProperty(PropertyName).Assign(
        Obj.GetProperty(PropertyName) as IInfraType);
      Iterator.Next;
    end;
  end else
    inherited Assign(Source);
end;

function TInfraObject.Equals(const Obj: IInfraType): Boolean;
var
  Iterator: IPropertyInfoIterator;
  TheObj: IInfraObject;
  TheProperty: IProperty;
begin
  Result := inherited Equals(Obj);
  if Result and Supports(Obj, IInfraObject, TheObj) then
  begin
    Iterator := TypeInfo.GetProperties;
    while Result and not Iterator.IsDone do
    begin
      TheProperty := GetProperty(Iterator.CurrentItem.Name);
      Result := Result and
        TheProperty.Equals(TheObj.GetProperty(Iterator.CurrentItem.Name));
      Iterator.Next;
    end;
  end;
end;

function TInfraObject.GetProperty(const PropertyName: string): IProperty;
begin
  Supports(TypeInfo.GetProperty(Self as IElement, PropertyName), IProperty, Result);
end;

procedure TInfraObject.InfraInitInstance;
begin
  inherited;
  // do nothing here, just on descendents!
end;

{ TInfraEnumeration }

function TInfraEnumeration.Equals(const Obj: IInfraType): Boolean;
begin
  Result := inherited Equals(Obj) and
    (AsLiteral = IInfraEnumeration(Obj).AsLiteral);
end;

function TInfraEnumeration.GetAsLiteral: IInfraLiteral;
begin
  Result := FValue;
end;

function TInfraEnumeration.GetLiterals: IInfraLiteralList;
begin
  Result := FLiterals;
end;

procedure TInfraEnumeration.SetAsLiteral(Value: IInfraLiteral);
begin
  if Value <> FValue then
  begin
    FValue := Value;
    Changed;
  end;
end;

{ TInfraStream }

class function TInfraStream.NewFrom(
  const Value: TMemoryStream): IInfraStream;
begin
  Result := Self.Create;
  Result.AsStream := Value;
end;

function TInfraStream.GetAsStream: TMemoryStream;
begin
  if Assigned(FStream) then
    FStream.Position := 0;
  Result := FStream;
end;

procedure TInfraStream.SetAsStream(const Value: TMemoryStream);
begin
  FStream := Value;
end;

{ TInfraMap global Routines }

procedure AssignMap(AInfo, AItem, AData: Pointer; out AContinue: Boolean);
begin
  TInfraMap(AInfo).FItems[TObject(IInfraType(AItem))] :=
    TObject(IInfraType(AData));
end;

procedure ExistValue(AInfo, AItem, AData: Pointer; out AContinue: Boolean);
begin
  with TInfraMap(AInfo) do begin
    FKeyFound := nil;
    if AData = FValueCallBack then
    begin
      FValueFound := True;
      FKeyFound := IInfraType(AItem);
      AContinue := False;
    end;
  end;
end;

procedure GetMapKeys(AInfo, AItem, AData: Pointer; out AContinue: Boolean);
begin
  IInfraList(TInfraMap(AInfo).FValueCallBack).Add(IInfraType(AItem));
end;

procedure GetMapValues(AInfo, AItem, AData: Pointer;
  out AContinue: Boolean);
begin
  IInfraList(TInfraMap(AInfo).FValueCallBack).Add(IInfraType(AData));
end;

procedure ClearMapItems(AInfo, AItem, AData: Pointer; out AContinue: Boolean);
begin
  IInterface(AItem) := nil;
  IInterface(AData) := nil;
end;

{ TInfraMap }

procedure TInfraMap.InfraInitInstance;
begin
  inherited;
  FItems := TBucketList.Create;
end;

destructor TInfraMap.Destroy;
begin
  FItems.ForEach(ClearMapItems, Pointer(Self));
  FItems.Free;
  inherited;
end;

function TInfraMap.ContainsKey(const Key: IInfraType): boolean;
begin
  Result := FItems.Exists(Pointer(Key));
end;

function TInfraMap.ContainsValue(const Value: IInfraType): boolean;
begin
  FValueFound := False;
  FValueCallBack := Pointer(Value);
  FItems.ForEach(ExistValue, Pointer(Self));
  Result := FValueFound;
end;

function TInfraMap.GetItem(const Key: IInfraType): IInfraType;
begin
  Result := IInfraType(FItems[Pointer(Key as IInfraType)]);
end;

function TInfraMap.GetKeyByValue(const Value: IInfraType): IInfraType;
begin
  FValueCallBack := Pointer(Value);
  FItems.ForEach(ExistValue, Pointer(Self));
  Result := FKeyFound;
end;

function TInfraMap.Keys: IInfraList;
begin
  Result := TInfraList.Create;
  FValueCallBack := Pointer(Result);
  FItems.ForEach(GetMapKeys, Pointer(Self));
end;

procedure TInfraMap.Assign(const Source: IInfraType);
begin
  FValueCallBack := Pointer(Source as IInfraMap);
  FItems.ForEach(AssignMap, Pointer(Self));
end;

procedure TInfraMap.SetItem(const Key: IInfratype; Value: IInfraType);
var
  PK, PV: Pointer;
begin
  PK := GetRefCountedPointerFromInterface(Key);
  PV := GetRefCountedPointerFromInterface(Value);
  if FItems.Exists(PK) then
    FItems[PK] := PV
  else
    FItems.Add(PK, PV);
end;

function TInfraMap.Values: IInfraList;
begin
  Result := TInfraList.Create;
  FValueCallBack := Pointer(Result);
  FItems.ForEach(GetMapValues, Pointer(Self));
end;

{ TInfraNativeObject }

class function TInfraNativeObject.NewFrom(
  const Value: TObject): IInfraNativeObject;
begin
  Result := Self.Create;
  Result.AsNativeObject := Value;
end;

procedure TInfraNativeObject.Assign(const Source: IInfraType);
begin
  if (Source <> nil) and Supports(Source, IInfraNativeObject) then
    SetAsNativeObject((Source as IInfraNativeObject).AsNativeObject)
  else
    inherited Assign(Source);
end;

procedure TInfraNativeObject.Clear;
begin
  inherited;
  FValue := nil;
end;

function TInfraNativeObject.Equals(const Obj: IInfraType): Boolean;
begin
  Result := (Self.AsNativeObject = (Obj as IInfraNativeObject).AsNativeObject);
end;

function TInfraNativeObject.GetAsNativeObject: TObject;
begin
  Result := FValue;
end;

procedure TInfraNativeObject.SetAsNativeObject(const Value: TObject);
begin
  if Value <> FValue then
  begin
    FValue := Value;
    Changed;
  end;
end;

end.
