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
    FItemIndex: Integer;
    FItemText: String;
    FOperation: TListOperation;
  protected
    procedure Assign(const Source: IInfraType); override;
    function GetOperation: TListOperation;
    function GetControl: TControl;
    function GetItemIndex: integer;
    function GetItemText: String;
    procedure SetControl(Value: TControl);
    procedure SetItemIndex(Value: integer);
    procedure SetItemText(const Value: String);
    procedure SetOperation(const Value: TListOperation);
    procedure Clear; override;
    property Control: TControl read GetControl write SetControl;
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
  end else
    inherited Assign(Source);
end;

procedure TVCLListType.Clear;
begin
  inherited Clear;
  FControl := nil;
  FItemIndex := -1;
  FOperation := loNone;
  FItemText := '';
end;

function TVCLListType.GetControl: TControl;
begin
  Result := FControl;
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
