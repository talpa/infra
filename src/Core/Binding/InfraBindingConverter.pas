unit InfraBindingConverter;

interface

uses
  Graphics,
  InfraValueType,
  InfraValueTypeIntf,
  InfraValueTypeConvert;

type

  TTextToColor = class(TTypeConverter)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TColorToText = class(TTextToColor)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TItemIndexToText = class(TTypeConverter)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TTextToItemIndex = class(TTypeConverter)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TInfraListToVCL = class(TTypeConverter)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

implementation


{ TTextToColor }

function TTextToColor.LeftToRight(const Value,
  Parameter: IInfraType): IInfraType;
begin
  Result := TInfraInteger.NewFrom(StringToColor(
    (Value as IInfraString).AsString));
end;

function TTextToColor.RightToLeft(const Value,
  Parameter: IInfraType): IInfraType;
begin
  Result := TInfraString.NewFrom(ColorToString(
    (Value as IInfraInteger).AsInteger));
end;

{ TColorToText }

function TColorToText.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := inherited RightToLeft(Value, Parameter);
end;

function TColorToText.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin
  Result := inherited LeftToRight(Value, Parameter);
end;

{ TItemIndexToText }

function TItemIndexToText.LeftToRight(const Value,
  Parameter: IInfraType): IInfraType;
begin

end;

function TItemIndexToText.RightToLeft(const Value,
  Parameter: IInfraType): IInfraType;
begin

end;

{ TTextToItemIndex }

function TTextToItemIndex.LeftToRight(const Value,
  Parameter: IInfraType): IInfraType;
begin

end;

function TTextToItemIndex.RightToLeft(const Value,
  Parameter: IInfraType): IInfraType;
begin

end;

{ TInfraListToVCL }

function TInfraListToVCL.LeftToRight(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin

end;

function TInfraListToVCL.RightToLeft(const Value: IInfraType;
  const Parameter: IInfraType = nil): IInfraType;
begin

end;

end.
