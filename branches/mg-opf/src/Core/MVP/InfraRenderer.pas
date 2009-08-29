unit InfraRenderer;

interface

uses
  InfraCommon,
  InfraValueType,
  InfraSingleton,
  InfraMVPIntf,
  InfraValueTypeIntf;

type
  TObjectToPropertyConverter = class(TElement,
    ITypeConverter, IObjectToProperty)
  private
    FObj: IInfraObject;
  protected
    function ConvertToRight(const Value: IInfraType;
      const Format: IInfraType = nil): IInfraType;
    function ConvertToLeft(const Value: IInfraType;
      const Format: IInfraType = nil): IInfraType;
  end;

  TFormatProperty = class(TInfraType, IFormatProperty)
  private
    FPropertyName: string;
    FRenderer: IRenderer;
  protected
    function GetPropertyName: string;
    function GetRenderer: IRenderer;
    procedure SetPropertyName(const Value: string);
    procedure SetRenderer(const Value: IRenderer);
    property PropertyName: string read GetPropertyName write SetPropertyName;
    property Renderer: IRenderer read GetRenderer write SetRenderer;
  end;

  TRenderer = class(TElement, IRenderer)
  private
    FFormat: IInfraType;
    FConverter: ITypeConverter;
    FView: IView;
  public
    function GetFormat: IInfraType;
    function GetTypeConverter: ITypeConverter;
    function GetView: IView;
    procedure SetFormat(const Value: IInfraType);
    procedure SetTypeConverter(const Value: ITypeConverter);
    procedure SetView(const Value: IView);
    property Format: IInfraType read GetFormat write SetFormat;
    property View: IView read GetView write SetView;
    property TypeConverter: ITypeConverter read GetTypeConverter
      write SetTypeConverter;
  end;

implementation

uses
  SysUtils, InfraValueTypeConvert;

{ TObjectToPropertyConverter }

function TObjectToPropertyConverter.ConvertToRight(const Value,
  Format: IInfraType): IInfraType;
begin
  Result := nil;
  if Supports(Value, IInfraObject, FObj) then
  begin
    with Format as IFormatProperty do
      if Assigned(Renderer) then
        Result := Renderer.TypeConverter.ConvertToRight(
          FObj.GetProperty(PropertyName), Renderer.Format) as IInfraType
      else
        Result := FObj.GetProperty(PropertyName) as IInfraType;
  end;
end;

function TObjectToPropertyConverter.ConvertToLeft(const Value,
  Format: IInfraType): IInfraType;
var
  lProperty: IProperty;
begin
  if Supports(Value, IProperty, lProperty) then
  begin
    with Format as IFormatProperty do
      lProperty.Assign(Renderer.TypeConverter.ConvertToLeft(
        FObj.GetProperty(PropertyName), Renderer.Format));
  end;
  Result := lProperty as IInfraType;
end;

{ TFormatProperty }

function TFormatProperty.GetPropertyName: string;
begin
  Result := FPropertyName;
end;

function TFormatProperty.GetRenderer: IRenderer;
begin
  Result := FRenderer;
end;

procedure TFormatProperty.SetPropertyName(const Value: string);
begin
  FPropertyName := Value;
end;

procedure TFormatProperty.SetRenderer(const Value: IRenderer);
begin
  FRenderer := Value;
end;

{ TRenderer }

function TRenderer.GetFormat: IInfraType;
begin
  Result := FFormat;
end;

function TRenderer.GetTypeConverter: ITypeConverter;
begin
  if not Assigned(FConverter) then
    FConverter := TNullConverter.Create;
  Result := FConverter;
end;

function TRenderer.GetView: IView;
begin
  Result := FView;
end;

procedure TRenderer.SetFormat(const Value: IInfraType);
begin
  if FFormat <> Value then
    FFormat := Value;
end;

procedure TRenderer.SetTypeConverter(const Value: ITypeConverter);
begin
  if FConverter <> Value then
    FConverter := Value;
end;

procedure TRenderer.SetView(const Value: IView);
begin
  SetReference(IInterface(FView), Value);
end;

end.
