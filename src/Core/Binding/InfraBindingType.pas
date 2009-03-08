unit InfraBindingType;

interface

uses
  InfraCommon, InfraBindingIntf, InfraValueTypeIntf;

type
  TBindableInfraType = class(TBaseElement, IBindable)
  private
    FValue: IInfraType;
    function GetValue: IInfraType;
    procedure SetValue(const Value: IInfraType);
  protected
    constructor Create(const pValue: IInfraType); reintroduce;
    property Value: IInfraType read GetValue write SetValue;
  end;

implementation

{ TBindableInfraType }

constructor TBindableInfraType.Create(const pValue: IInfraType);
begin
  inherited Create;
  SetValue(pValue);
end;

function TBindableInfraType.GetValue: IInfraType;
begin
  Result := FValue;
end;

procedure TBindableInfraType.SetValue(const Value: IInfraType);
begin
  FValue := Value;
end;

end.
