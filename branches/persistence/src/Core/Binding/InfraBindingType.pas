unit InfraBindingType;

interface

uses
  InfraCommon, InfraBindingIntf, InfraValueTypeIntf;

type
  TBindable = class(TElement, IBindable)
  private
    FInfraType: IInfraType;
  protected
    procedure Changed;
    function GetSupports2Way: Boolean; virtual; abstract;
    function GetValue: IInfraType; virtual;
    procedure SetValue(const Value: IInfraType); virtual;
    property Value: IInfraType read GetValue write SetValue;
    property Supports2Way: Boolean read GetSupports2Way;
  end;

  TBindableInfraType = class(TBindable, IBindableInfraType)
  public
    class function GetBindable(pValue: IInfraType;
      const pPropertyPath: string): IBindable; virtual;
  end;

implementation

uses
  SysUtils, InfraBindingManager;

{ TBindable }

procedure TBindable.Changed;
begin
  Publisher.Publish(TBindableValueChanged.Create(Self));
end;

function TBindable.GetValue: IInfraType;
begin
  Result := FInfraType;
end;

procedure TBindable.SetValue(const Value: IInfraType);
begin
  FInfraType := Value;
end;

{ TBindableInfraType }

class function TBindableInfraType.GetBindable(pValue: IInfraType;
  const pPropertyPath: string): IBindable;
var
  vObject: IInfraObject;
  vProperty: IProperty;
begin
  if Supports(pValue, IInfraObject, vObject) then
  begin
    vProperty := vObject.GetProperty(pPropertyPath);
    // *** gerar exceção quando a propriedade nao estiver lá?
    Result := TBindableInfraType.Create;
    Result.Value := vProperty as IInfraType;
  end;
end;

end.
