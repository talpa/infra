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
  TVCLListType = class(TInfraType, IVCLListType)
  private
    FControl: TControl;
    FInfraValue: IInfraType;
    FItemIndex: Integer;
    FItemText: String;
    FOperation: TListOperation;
  protected
    procedure Assign(const Source: IInfraType); override;
    function GetOperation: TListOperation;
    function GetControl: TControl;
    function GetInfraValue: IInfraType;
    function GetItemIndex: integer;
    function GetItemText: String;
    procedure SetControl(Value: TControl);
    procedure SetInfraValue(Value: IInfraType);
    procedure SetItemIndex(Value: integer);
    procedure SetItemText(const Value: String);
    procedure SetOperation(const Value: TListOperation);
    procedure Clear; override;
    property Control: TControl read GetControl write SetControl;
    property InfraValue: IInfraType read GetInfraValue write SetInfraValue;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property ItemText: String read GetItemText write SetItemText;
    property Operation: TListOperation read GetOperation write SetOperation;
  end;

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

{ TItemIndexToText }

function TItemIndexToText.LeftToRight(const Value,
  Parameter: IInfraType): IInfraType;
begin
  Result := TInfraString.NewFrom((Value as IVCLListType).ItemText);
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

{ TVCLListType }

procedure TVCLListType.Assign(const Source: IInfraType);
var
  vListType: IVCLListType;
begin
  if (Source <> nil) and Supports(Source, IVCLListType, vListType) then
  begin
    SetControl(vListType.Control);
    SetItemIndex(vListType.ItemIndex);
    SetOperation(vListType.Operation);
    SetItemText(vListType.ItemText);
    SetInfraValue(vListType.InfraValue);
  end else
    inherited Assign(Source);
end;

procedure TVCLListType.Clear;
begin
  inherited Clear;
  FControl := nil;
  FInfraValue := nil;
  FItemIndex := -1;
  FOperation := loNone;
  FItemText := '';
end;

function TVCLListType.GetControl: TControl;
begin
  Result := FControl;
end;

function TVCLListType.GetInfraValue: IInfraType;
begin
  Result := FInfraValue;
end;

function TVCLListType.GetItemIndex: integer;
begin
  Result := FItemIndex;
end;

function TVCLListType.GetItemText: String;
begin
  Result := FItemText;
end;

function TVCLListType.GetOperation: TListOperation;
begin
  Result := FOperation;
end;

procedure TVCLListType.SetControl(Value: TControl);
begin
  FControl := Value;
end;

procedure TVCLListType.SetInfraValue(Value: IInfraType);
begin
  FInfraValue := Value;
end;

procedure TVCLListType.SetItemIndex(Value: integer);
begin
  FItemIndex := Value;
end;

procedure TVCLListType.SetItemText(const Value: String);
begin
  FItemText := Value;
end;

procedure TVCLListType.SetOperation(const Value: TListOperation);
begin
  FOperation := Value;
end;

procedure RegisterOnReflection;
begin
  with TypeService do
    AddType(IVCLListType, 'VCLListType', TVCLListType, IInfraType);
end;

initialization
  RegisterOnReflection;

end.
