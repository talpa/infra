unit InfraBindingType;

interface

uses
  InfraCommon, InfraBindingIntf, InfraValueTypeIntf;

type
  TBindable = class(TElement, IBindable)
  protected
    procedure Changed;
    function GetSupports2Way: Boolean; virtual; abstract;
    function GetValue: IInfraType; virtual; abstract;
    procedure SetValue(const Value: IInfraType); virtual; abstract;    
  end;

  TBindableInfraType = class(TBindable, IBindableInfraType)
  private
    FInfraType: IInfraType;
  protected
    function GetSupports2Way: Boolean; override;  
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
  Publisher.Publish(TBindableValueChanged.Create(Self));
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

function TBindableInfraType.GetSupports2Way: Boolean;
begin
  Result := True;
end;

function TBindableInfraType.GetValue: IInfraType;
begin
  Result := FInfraType;
end;

procedure TBindableInfraType.SetValue(const Value: IInfraType);
begin
  FInfraType.Assign(Value);
end;

end.
