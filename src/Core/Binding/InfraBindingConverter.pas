unit InfraBindingConverter;

interface

uses
  Graphics,
  Controls,
  InfraValueTypeIntf,
  InfraBindingIntf,
  InfraValueType,
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

(*
*** nao deveriamos mais precisar deste tipo de converter

  TItemIndexToText = class(TTypeConverter)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TItemIndexToIntegerText = class(TTypeConverter)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;

  TInfraListToText = class(TTypeConverter)
  protected
    function LeftToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
    function RightToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType; override;
  end;
*)

implementation

uses
  SysUtils,
  Classes,
  InfraCommonIntf;

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

(*
*** nao deveriamos mais precisar deste tipo de converter

{ TItemIndexToText }
function TItemIndexToText.LeftToRight(const Value,
  Parameter: IInfraType): IInfraType;
begin
  Result := TInfraString.NewFrom((Value as IBindableListModel).ItemText);
end;

function TItemIndexToText.RightToLeft(const Value,
  Parameter: IInfraType): IInfraType;
var
  vListType: IVCLListType;
begin
  vListType := TVCLListType.Create;
  vListType.Operation := loSelectionChange;
  vListType.ItemIndex := -1;
  vListType.ItemText := (Value as IInfraString).AsString;
  Result := vListType;
end;

{ TItemIndexToIntegerText }

function TItemIndexToIntegerText.LeftToRight(const Value,
  Parameter: IInfraType): IInfraType;
begin
  Result := TInfraString.NewFrom(IntToStr((Value as IVCLListType).ItemIndex));
end;

function TItemIndexToIntegerText.RightToLeft(const Value,
  Parameter: IInfraType): IInfraType;
var
  vListType: IVCLListType;
begin
  vListType := TVCLListType.Create;
  vListType.Operation := loSelectionChange;
  vListType.ItemIndex := StrToInt((Value as IInfraString).AsString);
  vListType.ItemText := '';
  Result := vListType;
end;

{ TInfraListToText }

function TInfraListToText.LeftToRight(const Value,
  Parameter: IInfraType): IInfraType;
var
  vIterator: Integer;
  vVCLListType: IVCLListType;
begin
  {
  vVCLListType := Value as IVCLListType;
  case vVCLListType.Operation of
    loRefresh: FillItemText(vVCLListType);
    loAdd: vVCLListType.ItemText := GetItemText(vVCLListType);

  for vIterator := 0 to (vVCLListType.InfraValue as IInfraList).Count - 1 do
        vVCLListType.ItemText := vVCLListType.ItemText +
        (((vVCLListType.InfraValue as IInfraList)[vIterator] as IInfraObject).GetProperty((Parameter as IInfraString).AsString) as IInfraString).AsString +
        #13 + #10;
    loAdd: vVCLListType.ItemText := ((vVCLListType.InfraValue as IInfraObject).GetProperty((Parameter as IInfraString).AsString) as IInfraString).AsString;
    loClear: ;
    loRemove: ;
  end;
  Result := Value;
  }
end;

function TInfraListToText.RightToLeft(const Value,
  Parameter: IInfraType): IInfraType;
begin
  Result := Value;
end;
*)

end.
