unit InfraBindingConverter;

interface

uses
  Graphics,
  InfraValueType,
  InfraValueTypeIntf,
  InfraValueTypeConvert;

type

  TTextToColorConverter = class(TTypeConverter)
  protected
    function LeftToRight(const Value: IInfraType;
      const Format: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Format: IInfraType = nil): IInfraType; override;
  end;

implementation


{ TTextToColorConverter }

function TTextToColorConverter.LeftToRight(const Value,
  Format: IInfraType): IInfraType;
begin
  Result := TInfraInteger.NewFrom(StringToColor(
    (Value as IInfraString).AsString));
end;

function TTextToColorConverter.RightToLeft(const Value,
  Format: IInfraType): IInfraType;
begin
  Result := TInfraString.NewFrom(ColorToString(
    (Value as IInfraInteger).AsInteger));
end;
end.
