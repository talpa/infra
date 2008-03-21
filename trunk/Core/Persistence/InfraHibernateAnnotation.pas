unit InfraHibernateAnnotation;

interface

Uses
  InfraHibernateAnnotationIntf, InfraCommonIntf, InfraCommon;

Type
  TZTypeAnnotation = Class(TElement, IZTypeAnnotation)
  private
    FNullSafeGetter: TZTypeGetter;
    FNullSafeSetter: TZTypeSetter;
    function GetNullSafeGetter: TZTypeGetter;
    function GetNullSafeSetter: TZTypeSetter;
    procedure Init(pGetter: TZTypeGetter; pSetter: TZTypeSetter);
  public
    property NullSafeGet: TZTypeGetter read GetNullSafeGetter;
    property NullSafeSet: TZTypeSetter read GetNullSafeSetter;
  end;

  TEntity = Class(TElement, IEntity)
  private
    FName: String;
  public
    procedure SetName( const Value: String);
    function GetName: String;     
    property Name: String read GetName write SetName;    
  end;

  TColumn = Class(TElement, IColumn)
  private
    FColumnDefinition: String;
    FInsertable: Boolean;
    FLength: Integer;
    FName: String;
    FNullable: Boolean;
    FPrecision: Integer;
    FScale: Integer;
    FTable: String;
    FUnique: Boolean;
    FUpdatable: Boolean;
  public
    procedure SetColumnDefinition( const Value: String);
    function  GetColumnDefinition: String;
    procedure SetInsertable( const Value: Boolean);
    function  GetInsertable: Boolean;
    procedure SetLength( const Value: Integer);
    function  GetLength: Integer;
    procedure SetName(const Value: String);
    function  GetName: String;
    procedure SetNullable( const Value: Boolean);
    function  GetNullable: Boolean;
    procedure SetPrecision(const Value: Integer);
    function  GetPrecision: Integer;
    procedure SetScale(const Value: Integer);
    function  GetScale: Integer;
    procedure SetTable(const Value: String);
    function  GetTable: String;
    procedure SetUnique(const Value: Boolean);
    function  GetUnique: Boolean;
    procedure SetUpdatable(const Value: Boolean);
    function  GetUpdatable: Boolean;
    property ColumnDefinition: String read GetColumnDefinition write SetColumnDefinition;
    property Insertable: Boolean read GetInsertable write SetInsertable;
    property Length: Integer read GetLength write SetLength;
    property Name: String read GetName write SetName; 
    property Nullable: Boolean read GetNullable write SetNullable;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Scale: Integer read GetScale write SetScale;
    property Table: String read GetTable write SetTable;
    property Unique: Boolean read GetUnique write SetUnique;
    property Updatable: Boolean read GetUpdatable write SetUpdatable;  
  end;

  TId = class(TElement, IId);

{
  TColumns = interface(IElement)
    property columns: IColumn;
  end;


  TColumn = interface(IElement)
    property ColumnDefinition: string = "";
    property Insertable: boolean = true;
    property Length: integer = 255;
    property Name: string = "";
    property Nullable: boolean = true;
    property Precision: integer = 0;
    property Scale: integer = 0;
    property Table: string = "";
    property Unique: boolean = false;
    property Updatable: boolean = true;
  end;}

implementation

uses
  {Zeos} ZDbcIntfs,
  InfraValueTypeIntf;

{ TZTypeAnnotation }

function TZTypeAnnotation.GetNullSafeGetter: TZTypeGetter;
begin
  Result := FNullSafeGetter;
end;

function TZTypeAnnotation.GetNullSafeSetter: TZTypeSetter;
begin
  Result := FNullSafeSetter;
end;

procedure TZTypeAnnotation.Init(pGetter: TZTypeGetter;
  pSetter: TZTypeSetter);
begin
  FNullSafeGetter := pGetter;
  FNullSafeSetter := pSetter;
end;

procedure SetAsString(const pStatement: IZPreparedStatement; pIndex: Integer;
  const pParamValue: IInfraType);
begin
  if pParamValue.IsNull then
    pStatement.SetNull(pIndex+1, stString)
  else
    pStatement.SetString(pIndex+1, (pParamValue as IInfraString).AsString)
end;

procedure GetString(const pResultSet: IZResultSet; pIndex: Integer;
  const pPropertyValue: IInfraType);
begin
  if pResultSet.IsNull(pIndex+1) then
    pPropertyValue.Clear
  else
    (pPropertyValue as IInfraString).AsString := pResultSet.GetString(pIndex+1);
end;

procedure SetAsInteger(const pStatement: IZPreparedStatement; pIndex: Integer;
  const pParamValue: IInfraType);
begin
  if pParamValue.IsNull then
    pStatement.SetNull(pIndex+1, stInteger)
  else
    pStatement.SetInt(pIndex+1, (pParamValue as IInfraInteger).AsInteger)
end;

procedure GetInteger(const pResultSet: IZResultSet; pIndex: Integer;
  const pPropertyValue: IInfraType);
begin
  if pResultSet.IsNull(pIndex+1) then
    pPropertyValue.Clear
  else
    (pPropertyValue as IInfraInteger).AsInteger := pResultSet.GetInt(pIndex+1);
end;

procedure SetAsDouble(const pStatement: IZPreparedStatement; pIndex: Integer;
  const pParamValue: IInfraType);
begin
  if pParamValue.IsNull then
    pStatement.SetNull(pIndex+1, stDouble)
  else
    pStatement.SetDouble(pIndex+1, (pParamValue as IInfraDouble).AsDouble)
end;

procedure GetDouble(const pResultSet: IZResultSet; pIndex: Integer;
  const pPropertyValue: IInfraType);
begin
  if pResultSet.IsNull(pIndex+1) then
    pPropertyValue.Clear
  else
    (pPropertyValue as IInfraDouble).AsDouble := pResultSet.GetDouble(pIndex+1);
end;

procedure SetAsBoolean(const pStatement: IZPreparedStatement; pIndex: Integer;
  const pParamValue: IInfraType);
begin
  if pParamValue.IsNull then
    pStatement.SetNull(pIndex+1, stBoolean)
  else
    pStatement.SetBoolean(pIndex+1, (pParamValue as IInfraBoolean).AsBoolean)
end;

procedure GetBoolean(const pResultSet: IZResultSet; pIndex: Integer;
  const pPropertyValue: IInfraType);
begin
  if pResultSet.IsNull(pIndex+1) then
    pPropertyValue.Clear
  else
    (pPropertyValue as IInfraBoolean).AsBoolean :=
      pResultSet.GetBoolean(pIndex+1);
end;

procedure SetAsDate(const pStatement: IZPreparedStatement; pIndex: Integer;
  const pParamValue: IInfraType);
begin
  if pParamValue.IsNull then
    pStatement.SetNull(pIndex+1, stDate)
  else
    pStatement.SetDate(pIndex+1, (pParamValue as IInfraDate).AsDate)
end;

procedure GetDate(const pResultSet: IZResultSet; pIndex: Integer;
  const pPropertyValue: IInfraType);
begin
  if pResultSet.IsNull(pIndex+1) then
    pPropertyValue.Clear
  else
    (pPropertyValue as IInfraDate).AsDate := pResultSet.GetDate(pIndex+1);
end;

procedure SetAsTime(const pStatement: IZPreparedStatement; pIndex: Integer;
  const pParamValue: IInfraType);
begin
  if pParamValue.IsNull then
    pStatement.SetNull(pIndex+1, stTime)
  else
    pStatement.SetTime(pIndex+1, (pParamValue as IInfraTime).AsTime)
end;

procedure GetTime(const pResultSet: IZResultSet; pIndex: Integer;
  const pPropertyValue: IInfraType);
begin
  if pResultSet.IsNull(pIndex+1) then
    pPropertyValue.Clear
  else
    (pPropertyValue as IInfraTime).AsTime := pResultSet.GetTime(pIndex+1);
end;

procedure SetAsDateTime(const pStatement: IZPreparedStatement; pIndex: Integer;
  const pParamValue: IInfraType);
begin
  if pParamValue.IsNull then
    pStatement.SetNull(pIndex+1, stTimestamp)
  else
    pStatement.SetTimestamp(pIndex+1, (pParamValue as IInfraDateTime).AsDateTime);
end;

procedure GetDateTime(const pResultSet: IZResultSet; pIndex: Integer;
  const pPropertyValue: IInfraType);
begin
  if pResultSet.IsNull(pIndex+1) then
    pPropertyValue.Clear
  else
    (pPropertyValue as IInfraDateTime).AsDateTime :=
      pResultSet.GetTimeStamp(pIndex+1);
end;

procedure RegisterZeosTypeMapping;
var
  vTypeInfo: IClassInfo;
  vZType: IZTypeAnnotation;
begin
  with TypeService do
  begin
    AddAnnotation(IZTypeAnnotation, 'ZTypeAnnotation',
      TZTypeAnnotation, IElement);

    vTypeInfo := GetType(IInfraString, True);
    vZType := vTypeInfo.Annotate(IZTypeAnnotation) as IZTypeAnnotation;
    vzType.Init(GetString, SetAsString);

    vTypeInfo := GetType(IInfraInteger, True);
    vZType := vTypeInfo.Annotate(IZTypeAnnotation) as IZTypeAnnotation;
    vzType.Init(GetInteger, SetAsInteger);

    vTypeInfo := GetType(IInfraDouble, True);
    vZType := vTypeInfo.Annotate(IZTypeAnnotation) as IZTypeAnnotation;
    vzType.Init(GetDouble, SetAsDouble);

    vTypeInfo := GetType(IInfraBoolean, True);
    vZType := vTypeInfo.Annotate(IZTypeAnnotation) as IZTypeAnnotation;
    vzType.Init(GetBoolean, SetAsBoolean);

    vTypeInfo := GetType(IInfraDate, True);
    vZType := vTypeInfo.Annotate(IZTypeAnnotation) as IZTypeAnnotation;
    vzType.Init(GetDate, SetAsDate);

    vTypeInfo := GetType(IInfraTime, True);
    vZType := vTypeInfo.Annotate(IZTypeAnnotation) as IZTypeAnnotation;
    vzType.Init(GetTime, SetAsTime);

    vTypeInfo := GetType(IInfraDateTime, True);
    vZType := vTypeInfo.Annotate(IZTypeAnnotation) as IZTypeAnnotation;
    vzType.Init(GetDateTime, SetAsDateTime);
  end;
end;

{ TEntity }

function TEntity.GetName: String;
begin
  Result:= FName;
end;

procedure TEntity.SetName(const Value: String);
begin
  FName:= Value;
end;

{ TColumn }
function TColumn.GetColumnDefinition: String;
begin
  Result:= FColumnDefinition;
end;

function TColumn.GetInsertable: Boolean;
begin
  Result:= FInsertable;
end;

function TColumn.GetLength: Integer;
begin
  Result:= FLength;
end;

function TColumn.GetName: String;
begin
  Result:= FName;
end;

function TColumn.GetNullable: Boolean;
begin
  Result:= FNullable;
end;

function TColumn.GetPrecision: Integer;
begin
  Result:= FPrecision;
end;

function TColumn.GetScale: Integer;
begin
  Result:= FScale;
end;

function TColumn.GetTable: String;
begin
  Result:= FTable;
end;

function TColumn.GetUnique: Boolean;
begin
  Result:= FUnique;
end;

function TColumn.GetUpdatable: Boolean;
begin
  Result:= FUpdatable
end;

procedure TColumn.SetColumnDefinition(const Value: String);
begin
  ColumnDefinition:= Value;
end;

procedure TColumn.SetInsertable(const Value: Boolean);
begin
  FInsertable:= Value;
end;

procedure TColumn.SetLength(const Value: Integer);
begin
  FLength:= Value;
end;

procedure TColumn.SetName(const Value: String);
begin
  FName:= Value;
end;

procedure TColumn.SetNullable(const Value: Boolean);
begin
  FNullable:= Value;
end;

procedure TColumn.SetPrecision(const Value: Integer);
begin
  FPrecision:= Value;
end;

procedure TColumn.SetScale(const Value: Integer);
begin
  FScale:= Value;
end;

procedure TColumn.SetTable(const Value: String);
begin
  FTable:= Value;
end;

procedure TColumn.SetUnique(const Value: Boolean);
begin
  FUnique:= Value;
end;

procedure TColumn.SetUpdatable(const Value: Boolean);
begin
  FUpdatable:= Value;
end;

initialization
  RegisterZeosTypeMapping;
  
end.
