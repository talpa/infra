unit InfraPersistenceAnnotation;

interface

Uses
  InfraPersistenceAnnotationIntf,
  InfraCommonIntf,
  InfraCommon;

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

initialization
  RegisterZeosTypeMapping;
  
end.
