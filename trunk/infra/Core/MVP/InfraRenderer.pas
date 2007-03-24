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
    function ConvertFromLeftToRight(const Value: IInfraType;
      const Format: IInfraType = nil): IInfraType;
    function ConvertFromRightToLeft(const Value: IInfraType;
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

{ TObjectToPropertyConverter }

function TObjectToPropertyConverter.ConvertFromLeftToRight(const Value,
  Format: IInfraType): IInfraType;
begin
  Result := nil;
  if Supports(Value, IInfraObject, FObj) then
  begin
    with Format as IFormatFeature do
      if Assigned(Renderer) then
        Result := Renderer.TypeConverter.ConvertFromLeftToRight(
          FObj.Feature[FeatureName], Renderer.Format) as IInfraType
      else
        Result := FObj.Feature[FeatureName] as IInfraType;
  end;
end;

function TObjectToPropertyConverter.ConvertFromRightToLeft(const Value,
  Format: IInfraType): IInfraType;
var
  Feature: IInfraFeature;
begin
  if Supports(Value, IInfraFeature, Feature) then
  begin
    with Format as IFormatFeature do
      Feature.Assign(Renderer.TypeConverter.ConvertFromRightToLeft(
        FObj.Feature[FeatureName], Renderer.Format));
  end;
  Result := Feature as IInfraType;
end;

{ TFormatFeature }

function TFormatFeature.GetFeatureName: string;
begin
  Result := FFeatureName;
end;

function TFormatFeature.GetRenderer: IRenderer;
begin
  Result := FRenderer;
end;

procedure TFormatFeature.SetFeatureName(const Value: string);
begin
  FFeatureName := Value;
end;

procedure TFormatFeature.SetRenderer(const Value: IRenderer);
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
