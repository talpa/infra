unit InfraOPFAnnotation;

interface

Uses
  InfraCommon,
  InfraOPFIntf;

Type
  TPersistentState = class(TBaseElement, IPersistentState)
  private
    FState: TPersistentStateKind;
    FIsPersistent: Boolean;
  protected
    function GetIsPersistent: Boolean;
    function GetState: TPersistentStateKind;
    procedure SetIsPersistent(Value: Boolean);
    procedure SetState(Value: TPersistentStateKind);
    property IsPersistent: Boolean read GetIsPersistent write SetIsPersistent;
    property State: TPersistentStateKind read GetState write SetState;
  end;

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

  procedure RegisterZeosTypeMapping;
  
implementation

uses
  {Infra}
  InfraCommonIntf,
  InfraValueTypeIntf,
  {Zeos}
  ZDbcIntfs;

{ TPersistentState }

{*
  @return ResultDescription
}
function TPersistentState.GetIsPersistent: Boolean;
begin
  Result := FIsPersistent;
end;

{*
  Obtem o objeto é persistente, ou seja se veio do banco de dados
  @return ResultDescription
}
function TPersistentState.GetState: TPersistentStateKind;
begin
  Result := FState;
end;

{*
  Define se o objeto é persistente, ou seja se veio do banco de dados
  @param Value   ParameterDescription
  @return ResultDescription
}
procedure TPersistentState.SetIsPersistent(Value: Boolean);
begin
  FIsPersistent := Value;
end;

{*
  Define o estado de um objeto persistente
  @param Value ParameterDescription
  @return ResultDescription
}
procedure TPersistentState.SetState(Value: TPersistentStateKind);
begin
  FState := Value;
end;

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
    pStatement.SetNull(pIndex, stString)
  else
    pStatement.SetString(pIndex, (pParamValue as IInfraString).AsString)
end;

procedure GetString(const pResultSet: IZResultSet; pIndex: Integer;
  const pPropertyValue: IInfraType);
begin
  if pResultSet.IsNull(pIndex) then
    pPropertyValue.Clear
  else
    (pPropertyValue as IInfraString).AsString := pResultSet.GetString(pIndex);
end;

procedure SetAsInteger(const pStatement: IZPreparedStatement; pIndex: Integer;
  const pParamValue: IInfraType);
begin
  if pParamValue.IsNull then
    pStatement.SetNull(pIndex, stInteger)
  else
    pStatement.SetInt(pIndex, (pParamValue as IInfraInteger).AsInteger)
end;

procedure GetInteger(const pResultSet: IZResultSet; pIndex: Integer;
  const pPropertyValue: IInfraType);
begin
  if pResultSet.IsNull(pIndex) then
    pPropertyValue.Clear
  else
    (pPropertyValue as IInfraInteger).AsInteger := pResultSet.GetInt(pIndex);
end;

procedure SetAsDouble(const pStatement: IZPreparedStatement; pIndex: Integer;
  const pParamValue: IInfraType);
begin
  if pParamValue.IsNull then
    pStatement.SetNull(pIndex, stDouble)
  else
    pStatement.SetDouble(pIndex, (pParamValue as IInfraDouble).AsDouble)
end;

procedure GetDouble(const pResultSet: IZResultSet; pIndex: Integer;
  const pPropertyValue: IInfraType);
begin
  if pResultSet.IsNull(pIndex) then
    pPropertyValue.Clear
  else
    (pPropertyValue as IInfraDouble).AsDouble := pResultSet.GetDouble(pIndex);
end;

procedure SetAsBoolean(const pStatement: IZPreparedStatement; pIndex: Integer;
  const pParamValue: IInfraType);
begin
  if pParamValue.IsNull then
    pStatement.SetNull(pIndex, stBoolean)
  else
    pStatement.SetBoolean(pIndex, (pParamValue as IInfraBoolean).AsBoolean)
end;

procedure GetBoolean(const pResultSet: IZResultSet; pIndex: Integer;
  const pPropertyValue: IInfraType);
begin
  if pResultSet.IsNull(pIndex) then
    pPropertyValue.Clear
  else
    (pPropertyValue as IInfraBoolean).AsBoolean :=
      pResultSet.GetBoolean(pIndex);
end;

procedure SetAsDate(const pStatement: IZPreparedStatement; pIndex: Integer;
  const pParamValue: IInfraType);
begin
  if pParamValue.IsNull then
    pStatement.SetNull(pIndex, stDate)
  else
    pStatement.SetDate(pIndex, (pParamValue as IInfraDate).AsDate)
end;

procedure GetDate(const pResultSet: IZResultSet; pIndex: Integer;
  const pPropertyValue: IInfraType);
begin
  if pResultSet.IsNull(pIndex) then
    pPropertyValue.Clear
  else
    (pPropertyValue as IInfraDate).AsDate := pResultSet.GetDate(pIndex);
end;

procedure SetAsTime(const pStatement: IZPreparedStatement; pIndex: Integer;
  const pParamValue: IInfraType);
begin
  if pParamValue.IsNull then
    pStatement.SetNull(pIndex, stTime)
  else
    pStatement.SetTime(pIndex, (pParamValue as IInfraTime).AsTime)
end;

procedure GetTime(const pResultSet: IZResultSet; pIndex: Integer;
  const pPropertyValue: IInfraType);
begin
  if pResultSet.IsNull(pIndex) then
    pPropertyValue.Clear
  else
    (pPropertyValue as IInfraTime).AsTime := pResultSet.GetTime(pIndex);
end;

procedure SetAsDateTime(const pStatement: IZPreparedStatement; pIndex: Integer;
  const pParamValue: IInfraType);
begin
  if pParamValue.IsNull then
    pStatement.SetNull(pIndex, stTimestamp)
  else
    pStatement.SetTimestamp(pIndex, (pParamValue as IInfraDateTime).AsDateTime);
end;

procedure GetDateTime(const pResultSet: IZResultSet; pIndex: Integer;
  const pPropertyValue: IInfraType);
begin
  if pResultSet.IsNull(pIndex) then
    pPropertyValue.Clear
  else
    (pPropertyValue as IInfraDateTime).AsDateTime :=
      pResultSet.GetTimeStamp(pIndex);
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

end.

