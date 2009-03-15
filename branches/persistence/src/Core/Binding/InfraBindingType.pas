unit InfraBindingType;

interface

uses
  InfraCommon, InfraBindingIntf, InfraValueTypeIntf;

type
  TBindable = class(TElement, IBindable)
  protected
    procedure Changed;
    function Support2Way: Boolean; virtual;
    function GetValue: IInfraType; virtual; abstract;
    procedure SetValue(const Value: IInfraType); virtual; abstract;    
  end;

  TBindableInfraType = class(TBindable, IBindableInfraType)
  private
    FInfraType: IInfraType;
  protected
    function Support2Way: Boolean; override;  
    function GetValue: IInfraType; override;
    procedure SetValue(const Value: IInfraType); override;
  public
    class function GetBindable(pValue: IInfraType;
      const pPropertyPath: string): IBindable;
  end;

implementation

uses
  SysUtils, InfraBindingManager;

{ TBindable }

procedure TBindable.Changed;
begin
  Publisher.Publish(TNotifyValueChanged .Create(Self) as INotifyValueChanged);
end;

function TBindable.Support2Way: Boolean;
begin
  Result := False;
end;

{ TBindableInfraType }

class function TBindableInfraType.GetBindable(pValue: IInfraType;
  const pPropertyPath: string): IBindable;
var
  vObject: IInfraObject;
  vProperty: IProperty;
begin
  // *** E se o Value for uma lista, ou outro tipo de infratype? vai se criar um
  // *** bindable para cada tipo?
  if Supports(pValue, IInfraObject, vObject) then
  begin
    vProperty := vObject.GetProperty(pPropertyPath);
    // *** teria de gerar exceção quando o infraobject nao possuir a propriedade?
    Result := TBindableInfraType.Create;
    Result.Value := vProperty as IInfraType;
  end;
end;

function TBindableInfraType.GetValue: IInfraType;
begin
  Result := FInfraType;
end;

procedure TBindableInfraType.SetValue(const Value: IInfraType);
begin
  FInfraType.Assign(Value);
end;

function TBindableInfraType.Support2Way: Boolean;
begin
  Result := True;
end;

end.
