unit InfraControlBinding;

interface

uses
  Controls, InfraBindingIntf, InfraCommon, InfraValueTypeIntf;

type
  TBindablePropertyAccessStrategy = (asRTTI, asCustom);

  TBindableProperty = record
    AccessStrategy: TBindablePropertyAccessStrategy;
    DataType: TGUID;
    PropertyPath: String;
  end;

  TBindableControl = class abstract(TBaseElement, IBindableControl)
  private
    FControl: TControl;
    FBindableProperties: array of TBindableProperty;
    FBindableProperty: TBindableProperty;
    procedure RegisterRTTIAccessibleProperties;
    procedure RegisterCustomProperties; virtual;
    function GetPropertyPath: String;
    function GetValue: IInfraType;
    procedure SetValue(const Value: IInfraType);
    function SelectBindablePropertyByPath(const PropertyPath: String): Boolean;
    function GetValueByRTTI: IInfraType;
    procedure SetValueByRTTI(const Value: IInfraType);
  protected
    procedure RegisterProperty(AccessStrategy: TBindablePropertyAccessStrategy;
      const PropertyPath: String; DataType: TGUID);
    function CustomGetValue: IInfraType; virtual; abstract;
    procedure CustomSetValue(const Value: IInfraType); virtual; abstract;
  protected
    constructor Create(Control: TControl; const PropertyPath: String); reintroduce;
  end;

implementation

uses
  SysUtils, TypInfo, InfraValueType;

{ TBindableControl }

constructor TBindableControl.Create(Control: TControl;
  const PropertyPath: String);
begin
  inherited Create;
  FControl := Control;
  RegisterRTTIAccessibleProperties;
  RegisterCustomProperties;
  SelectBindablePropertyByPath(PropertyPath);
  { TODO: Raise an exception if SelectBindablePropertyByPath returns False
    (Returning false means that this TBindableControl does not support the
    specified property path) }
end;

function TBindableControl.GetPropertyPath: String;
begin
  Result := FBindableProperty.PropertyPath;
end;

function TBindableControl.GetValue: IInfraType;
begin
  case FBindableProperty.AccessStrategy of
    asRTTI: Result := GetValueByRTTI;
    asCustom: Result := CustomGetValue;
  end;
end;

function TBindableControl.GetValueByRTTI: IInfraType;
begin
  if FBindableProperty.DataType = IInfraString then
    Result := TInfraString.NewFrom(GetStrProp(FControl, FBindableProperty.PropertyPath));
end;

procedure TBindableControl.RegisterCustomProperties;
begin

end;

procedure TBindableControl.RegisterProperty(
  AccessStrategy: TBindablePropertyAccessStrategy; const PropertyPath: String;
  DataType: TGUID);
begin
  SetLength(FBindableProperties, Length(FBindableProperties) + 1);
  FBindableProperties[High(FBindableProperties)].AccessStrategy := AccessStrategy;
  FBindableProperties[High(FBindableProperties)].PropertyPath := PropertyPath;  
  FBindableProperties[High(FBindableProperties)].DataType := DataType;
end;

procedure TBindableControl.RegisterRTTIAccessibleProperties;
var
  PropCount: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
  i: Integer;
begin
  PropCount := GetPropList(PTypeInfo(FControl.ClassInfo), PropList);
  if PropCount > 0 then
  begin
    try
      for i := 0 to PropCount - 1 do
      begin
        PropInfo := PropList^[i];

        if PropInfo^.PropType^ = TypeInfo(String) then
          RegisterProperty(asRTTI, PropInfo^.Name, IInfraString);
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

function TBindableControl.SelectBindablePropertyByPath(
  const PropertyPath: String): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(FBindableProperties) do
  begin
    if AnsiSameText(PropertyPath, FBindableProperties[i].PropertyPath) then
    begin
      FBindableProperty := FBindableProperties[0];
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

procedure TBindableControl.SetValue(const Value: IInfraType);
begin
  case FBindableProperty.AccessStrategy of
    asRTTI: SetValueByRTTI(Value);
    asCustom: CustomSetValue(Value);
  end;
end;

procedure TBindableControl.SetValueByRTTI(const Value: IInfraType);
begin
  if FBindableProperty.DataType = IInfraString then
    SetStrProp(FControl, FBindableProperty.PropertyPath, (Value as IInfraString).AsString);
end;

end.
