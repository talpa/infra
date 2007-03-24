unit InfraValueTypeConvert;

{$I Infra.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraSingleton,
  InfraValueTypeIntf;

type                                      
  TTypeConverter = class(TInfraSingleton, ITypeConverter)
  protected
    function LeftToRight(const Value: IInfraType;
      const Format: IInfraType = nil): IInfraType; virtual; abstract;
    function RightToLeft(const Value: IInfraType;
      const Format: IInfraType = nil): IInfraType; virtual; abstract;
    function ConvertToRight(const Value: IInfraType;
      const Format: IInfraType = nil): IInfraType;
    function ConvertToLeft(const Value: IInfraType;
      const Format: IInfraType = nil): IInfraType;
  end;

  TNullConverter = class(TTypeConverter, INullConverter)
  protected
    function LeftToRight(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
  end;

  TDateTimeToText = class(TTypeConverter, IDateTimeToText)
  protected
    function LeftToRight(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
  end;

  TBooleanToText = class(TTypeConverter, IBooleanToText)
  protected
    function LeftToRight(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
  end;

  TIntegerToText = class(TTypeConverter, IIntegerToText)
  protected
    function LeftToRight(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
  end;

  TDoubleToText = class(TTypeConverter, IDoubleToText)
  protected
    function LeftToRight(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
  end;

  TStringToVariant = class(TTypeConverter, IStringToVariant)
  protected
    function LeftToRight(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
  end;

  TDoubleToVariant = class(TTypeConverter, IDoubleToVariant)
  protected
    function LeftToRight(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
  end;

  TIntegerToVariant = class(TTypeConverter, IIntegerToVariant)
  protected
    function LeftToRight(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
  end;

  TBooleanToVariant = class(TTypeConverter, IBooleanToVariant)
  protected
    function LeftToRight(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
  end;

  TDateTimeToVariant = class(TTypeConverter, IDateTimeToVariant)
  protected
    function LeftToRight(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType; const Format:
      IInfraType = nil): IInfraType; override;
  end;

implementation

uses
  Variants, SysUtils, InfraValueType;

{ TTypeConverter }

function TTypeConverter.ConvertToRight(const Value:
  IInfraType; const Format: IInfraType = nil): IInfraType;
begin
  Result := LeftToRight(Value, Format);
end;

function TTypeConverter.ConvertToLeft(const Value:
  IInfraType; const Format: IInfraType = nil): IInfraType;
begin
  Result := RightToLeft(Value, Format);
end;

{ TNullConverter }

function TNullConverter.LeftToRight(const Value,
  Format: IInfraType): IInfraType;
begin
  Result := Value;
end;

function TNullConverter.RightToLeft(const Value,
  Format: IInfraType): IInfraType;
begin
  Result := Value;
end;

{ TDateTimeToText }

function TDateTimeToText.LeftToRight(const Value: IInfraType; const
  Format: IInfraType = nil): IInfraType;
begin
  Result := TInfraString.Create;
  try
    if Supports(Value, IInfraDateTime) then
      (Result as IInfraString).AsString :=
        FormatDateTime((Format as IInfraString).AsString,
        (Value as IInfraDateTime).AsDateTime)
    else if Supports(Value, IInfraDate) then
      (Result as IInfraString).AsString :=
        FormatDateTime((Format as IInfraString).AsString,
        (Value as IInfraDate).AsDate)
    else if Supports(Value, IInfraTime) then
      (Result as IInfraString).AsString :=
        FormatDateTime((Format as IInfraString).AsString,
        (Value as IInfraTime).AsTime)
    else
      (Result as IInfraString).AsString := '';
  except
    (Result as IInfraString).AsString := '';
  end;
end;

function TDateTimeToText.RightToLeft(const Value: IInfraType; const
  Format: IInfraType = nil): IInfraType;
begin
  Result := TInfraDateTime.Create;
  try
    (Result as IInfraDateTime).AsDateTime :=
      StrToDateTime((Value as IInfraString).AsString);
  except
    (Result as IInfraDateTime).AsDateTime := 0;
  end;
end;

{ TBooleanToText }

function TBooleanToText.LeftToRight(const Value: IInfraType; const
  Format: IInfraType = nil): IInfraType;
var
  Token: string;
begin
  Result := TInfraString.Create;
  try
    Token := (Format as IInfraString).AsString;
    if not (Value as IInfraBoolean).AsBoolean then
      Token := Copy(Token, 1, Pos(';', Token)-1)
    else
      Token := Copy(Token, Pos(';', Token)+1, Length(Token));
    (Result as IInfraString).AsString := Token;
  except
    (Result as IInfraString).AsString := '';
  end;
end;

function TBooleanToText.RightToLeft(const Value: IInfraType; const
  Format: IInfraType = nil): IInfraType;
var
  Token: string;
begin
  Result := TInfraBoolean.Create;
  Token := (Format as IInfraString).AsString;
  Token := Copy(Token, 1, Pos(';', Token)-1);
  try
    (Result as IInfraBoolean).AsBoolean :=
      AnsiSameText((Value as IInfraString).AsString, Token);
  except
    (Result as IInfraBoolean).AsBoolean := False;
  end;
end;

{ TIntegerToText }

function TIntegerToText.LeftToRight(const Value: IInfraType; const
  Format: IInfraType = nil): IInfraType;
begin
  Result := TInfraString.Create;
  try
    (Result as IInfraString).AsString :=
      IntToStr((Value as IInfraInteger).AsInteger);
  except
    (Result as IInfraString).AsString := '';
  end;
end;

function TIntegerToText.RightToLeft(const Value: IInfraType; const
  Format: IInfraType = nil): IInfraType;
begin
  Result := TInfraInteger.Create;
  try
    (Result as IInfraInteger).AsInteger :=
      StrToInt((Value as IInfraString).AsString);
  except
    (Result as IInfraInteger).AsInteger := 0;
  end;
end;

{ TDoubleToText }

function TDoubleToText.LeftToRight(const Value: IInfraType; const
  Format: IInfraType = nil): IInfraType;
begin
  Result := TInfraString.Create;
  if Assigned(Format) then
    (Result as IInfraString).AsString := FormatFloat(
      (Format as IInfraString).AsString,
      (Value as IInfraDouble).AsDouble)
  else
    (Result as IInfraString).AsString :=
      FloatToStr((Value as IInfraDouble).AsDouble);
end;

function TDoubleToText.RightToLeft(const Value: IInfraType; const
  Format: IInfraType = nil): IInfraType;
var
  ValueToConvert: string;
begin
  Result := TInfraDouble.Create;
  ValueToConvert := (Value as IInfraString).AsString;
  if ValueToConvert = '' then
    (Result as IInfraDouble).AsDouble := 0.0
  else
  begin
    ValueToConvert := StringReplace(ValueToConvert, ThousandSeparator, '', []);
    (Result as IInfraDouble).AsDouble := StrToFloat(ValueToConvert);
  end;
end;

{ TStringToVariant }

function TStringToVariant.LeftToRight(const Value: IInfraType;
  const Format: IInfraType = nil): IInfraType;
var
  v: Variant;
begin
  v := (Value as IInfraString).AsString;
  Result := TInfraVariant.NewFrom(v);
end;

function TStringToVariant.RightToLeft(const Value: IInfraType;
  const Format: IInfraType = nil): IInfraType;
var
  s: string;
begin
  s := (Value as IInfraVariant).AsVariant;
  Result := TInfraString.NewFrom(s);
end;

{ TDoubleToVariant }

function TDoubleToVariant.LeftToRight(const Value: IInfraType;
  const Format: IInfraType = nil): IInfraType;
var
  v: Variant;
begin
  v := (Value as IInfraDouble).AsDouble;
  Result := TInfraVariant.NewFrom(v);
end;

function TDoubleToVariant.RightToLeft(const Value: IInfraType;
  const Format: IInfraType = nil): IInfraType;
var
  d: Double;
begin
  d := (Value as IInfraVariant).AsVariant;
  Result := TInfraDouble.NewFrom(d);
end;

{ TIntegerToVariant }

function TIntegerToVariant.LeftToRight(const Value: IInfraType;
  const Format: IInfraType = nil): IInfraType;
var
  v: Variant;
begin
  v := (Value as IInfraInteger).AsInteger;
  Result := TInfraVariant.NewFrom(v);
end;

function TIntegerToVariant.RightToLeft(const Value: IInfraType;
  const Format: IInfraType = nil): IInfraType;
var
  i: integer;
begin
  i := (Value as IInfraVariant).AsVariant;
  Result := TInfraInteger.NewFrom(i);
end;

{ TBooleanToVariant }

function TBooleanToVariant.LeftToRight(const Value: IInfraType;
  const Format: IInfraType = nil): IInfraType;
var
  v: Variant;
begin
  v := (Value as IInfraBoolean).AsBoolean;
  Result := TInfraVariant.NewFrom(v);
end;

function TBooleanToVariant.RightToLeft(const Value: IInfraType;
  const Format: IInfraType = nil): IInfraType;
var
  b: Boolean;
begin
  b := (Value as IInfraVariant).AsVariant;
  Result := TInfraBoolean.NewFrom(b);
end;

{ TDateTimeToVariant }

function TDateTimeToVariant.LeftToRight(const Value: IInfraType;
  const Format: IInfraType = nil): IInfraType;
var
  v: Variant;
begin
  v := VarFromDateTime((Value as IInfraDateTime).AsDateTime);
  Result := TInfraVariant.NewFrom(v);
end;

function TDateTimeToVariant.RightToLeft(const Value: IInfraType;
  const Format: IInfraType = nil): IInfraType;
var
  d: TDateTime;
begin
  d := VarToDateTime((Value as IInfraVariant).AsVariant);
  Result := TInfraDateTime.NewFrom(d);
end;

end.
