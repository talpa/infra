unit InfraValueTypeConvert;

interface

{$I InfraTypes.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Windows,
  InfraSingleton,
  InfraValueTypeIntf;

type                                      
  TTypeConverter = class(TInfraSingleton, ITypeConverter)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; virtual; abstract;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; virtual; abstract;
    function ConvertToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType;
    function ConvertToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType;
  end;

  TNullConverter = class(TTypeConverter, INullConverter)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TDateTimeToText = class(TTypeConverter, IDateTimeToText)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TBooleanToText = class(TTypeConverter, IBooleanToText)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TIntegerToText = class(TTypeConverter, IIntegerToText)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TDoubleToText = class(TTypeConverter, IDoubleToText)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TTextToInteger = class(TIntegerToText)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TTextToDateTime = class(TDateTimeToText)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TTextToBoolean = class(TBooleanToText)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TTextToDouble = class(TDoubleToText)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TStringToVariant = class(TTypeConverter, IStringToVariant)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TDoubleToVariant = class(TTypeConverter, IDoubleToVariant)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TIntegerToVariant = class(TTypeConverter, IIntegerToVariant)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TBooleanToVariant = class(TTypeConverter, IBooleanToVariant)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TDateTimeToVariant = class(TTypeConverter, IDateTimeToVariant)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

function GetConverterToVariant(const pProperty: IInfraType): ITypeConverter;
function GetVariantValue(const pProperty: IInfraType): IInfraVariant;
procedure SetVariantValue(const pProperty: IInfraType; pValue: Variant);

implementation

uses
  Variants, SysUtils, InfraValueType;

function GetConverterToVariant(const pProperty: IInfraType): ITypeConverter;
begin
  if Supports(pProperty, IInfraString) then
    Result := TStringToVariant.Create
  else if Supports(pProperty, IInfraBoolean) then
    Result := TBooleanToVariant.Create
  else if Supports(pProperty, IInfraDateTime) then
    Result := TDateTimeToVariant.Create
  else if Supports(pProperty, IInfraDate) then
    Result := TDateTimeToVariant.Create
  else if Supports(pProperty, IInfraTime) then
    Result := TDateTimeToVariant.Create
  else if Supports(pProperty, IInfraDouble) then
    Result := TDoubleToVariant.Create
  else if Supports(pProperty, IInfraInteger) then
    Result := TIntegerToVariant.Create;
end;

function GetVariantValue(const pProperty: IInfraType): IInfraVariant;
begin
  if Supports(pProperty, IInfraVariant) then
    Result := pProperty as IInfraVariant
  else
    Result := GetConverterToVariant(pProperty).ConvertToRight(pProperty) as IInfraVariant;
end;

procedure SetVariantValue(const pProperty: IInfraType; pValue: Variant);
var
  lInfraType: IInfraType;
begin
  if Supports(pProperty, IInfraVariant) then
    (pProperty as IInfraVariant).AsVariant := pValue
  else
  begin
    lInfraType := GetConverterToVariant(pProperty).ConvertToLeft(
      TInfraVariant.NewFrom(pValue));
    pProperty.Assign(lInfraType);
  end;
end;

{ TTypeConverter }

function TTypeConverter.ConvertToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := LeftToRight(Value, Parameter);
end;

function TTypeConverter.ConvertToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := RightToLeft(Value, Parameter);
end;

{ TNullConverter }

function TNullConverter.LeftToRight(
  const Value, Parameter: IInfraType): IInfraType;
begin
  Result := Value;
end;

function TNullConverter.RightToLeft(
  const Value, Parameter: IInfraType): IInfraType;
begin
  Result := Value;
end;

{ TDateTimeToText }

function TDateTimeToText.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
var
  vFormat: string;
  vResult: IInfraString;
begin
  if Assigned(Parameter) and Supports(Parameter, IInfraString) then
    vFormat := (Parameter as IInfraString).AsString
  else
    vFormat := 'dd/mm/yyyy hh:mm:ss';
  vResult := TInfraString.NewFrom('');
  try
    if Supports(Value, IInfraDateTime) then
      vResult.AsString := FormatDateTime(vFormat,
        (Value as IInfraDateTime).AsDateTime)
    else if Supports(Value, IInfraDate) then
      vResult.AsString := FormatDateTime(vFormat,
        (Value as IInfraDate).AsDate)
    else if Supports(Value, IInfraTime) then
      vResult.AsString := FormatDateTime(vFormat,
        (Value as IInfraTime).AsTime)
  except
  end;
  Result := vResult;
end;

function TDateTimeToText.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := TInfraDateTime.NewFrom(0);
  try
    (Result as IInfraDateTime).AsDateTime :=
      StrToDateTime((Value as IInfraString).AsString);
  except
  end;
end;

{ TBooleanToText }

function TBooleanToText.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
var
  vToken: string;
  vResult: IInfraString;
begin
  if Assigned(Parameter) and Supports(Parameter, IInfraString) then
    vToken := (Parameter as IInfraString).AsString
  else
    vToken := 'False;True';
  vResult := TInfraString.NewFrom('');
  try
    if not (Value as IInfraBoolean).AsBoolean then
      vToken := Copy(vToken, 1, Pos(';', vToken)-1)
    else
      vToken := Copy(vToken, Pos(';', vToken)+1, Length(vToken));
    vResult.AsString := vToken;
  except
  end;
  Result := vResult;
end;

function TBooleanToText.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
var
  vToken: string;
begin
  Result := TInfraBoolean.NewFrom(False);
  if Assigned(Parameter) and Supports(Parameter, IInfraString) then
  begin
    vToken := (Parameter as IInfraString).AsString;
    vToken := Copy(vToken, 1, Pos(';', vToken)-1);
  end else
    vToken := 'False';
  try
    (Result as IInfraBoolean).AsBoolean :=
      AnsiSameText((Value as IInfraString).AsString, vToken);
  except
  end;
end;

{ TIntegerToText }

function TIntegerToText.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := TInfraString.NewFrom('');
  try
    (Result as IInfraString).AsString :=
      IntToStr((Value as IInfraInteger).AsInteger);
  except
  end;
end;

function TIntegerToText.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := TInfraInteger.NewFrom(0);
  try
    (Result as IInfraInteger).AsInteger :=
      StrToInt((Value as IInfraString).AsString);
  except
  end;
end;

{ TDoubleToText }

function TDoubleToText.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := TInfraString.Create;
  if Assigned(Parameter) and Supports(Parameter, IInfraString) then
    (Result as IInfraString).AsString := FormatFloat(
      (Parameter as IInfraString).AsString,
      (Value as IInfraDouble).AsDouble)
  else
    (Result as IInfraString).AsString := FloatToStr((Value as IInfraDouble).AsDouble);
end;

function TDoubleToText.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
var
  ValueToConvert: string;
begin
  Result := TInfraDouble.NewFrom(0.0);
  ValueToConvert := (Value as IInfraString).AsString;
  if ValueToConvert <> EmptyStr then
  begin
    ValueToConvert := StringReplace(ValueToConvert, ThousandSeparator, '', []);
    (Result as IInfraDouble).AsDouble := StrToFloat(ValueToConvert);
  end;
end;

{ TStringToVariant }

function TStringToVariant.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
var
  v: Variant;
begin
  v := (Value as IInfraString).AsString;
  Result := TInfraVariant.NewFrom(v);
end;

function TStringToVariant.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
var
  s: string;
begin
  s := (Value as IInfraVariant).AsVariant;
  Result := TInfraString.NewFrom(s);
end;

{ TDoubleToVariant }

function TDoubleToVariant.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
var
  v: Variant;
begin
  v := (Value as IInfraDouble).AsDouble;
  Result := TInfraVariant.NewFrom(v);
end;

function TDoubleToVariant.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
var
  d: Double;
begin
  d := (Value as IInfraVariant).AsVariant;
  Result := TInfraDouble.NewFrom(d);
end;

{ TIntegerToVariant }

function TIntegerToVariant.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
var
  v: Variant;
begin
  v := (Value as IInfraInteger).AsInteger;
  Result := TInfraVariant.NewFrom(v);
end;

function TIntegerToVariant.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
var
  i: integer;
begin
  i := (Value as IInfraVariant).AsVariant;
  Result := TInfraInteger.NewFrom(i);
end;

{ TBooleanToVariant }

function TBooleanToVariant.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
var
  v: Variant;
begin
  v := (Value as IInfraBoolean).AsBoolean;
  Result := TInfraVariant.NewFrom(v);
end;

function TBooleanToVariant.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
var
  b: Boolean;
begin
  b := (Value as IInfraVariant).AsVariant;
  Result := TInfraBoolean.NewFrom(b);
end;

{ TDateTimeToVariant }

function TDateTimeToVariant.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
var
  v: Variant;
begin
  v := VarFromDateTime((Value as IInfraDateTime).AsDateTime);
  Result := TInfraVariant.NewFrom(v);
end;

function TDateTimeToVariant.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
var
  d: TDateTime;
begin
  d := VarToDateTime((Value as IInfraVariant).AsVariant);
  Result := TInfraDateTime.NewFrom(d);
end;

{ TTextToDateTime }

function TTextToDateTime.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := inherited RightToLeft(Value, Parameter);
end;

function TTextToDateTime.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := inherited LeftToRight(Value, Parameter);
end;

{ TTextToBoolean }

function TTextToBoolean.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := inherited RightToLeft(Value, Parameter);
end;

function TTextToBoolean.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := inherited LeftToRight(Value, Parameter);
end;

{ TTextToInteger }

function TTextToInteger.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := inherited RightToLeft(Value, Parameter);
end;

function TTextToInteger.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := inherited LeftToRight(Value, Parameter);
end;

{ TTextToDouble }

function TTextToDouble.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := inherited RightToLeft(Value, Parameter);
end;

function TTextToDouble.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := inherited LeftToRight(Value, Parameter);
end;

end.
