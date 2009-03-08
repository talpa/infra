unit InfraBindingType;

interface

uses
  InfraCommon, InfraBindingIntf, InfraValueTypeIntf;

type
  TBindableInfraType = class(TElement, IBindable)
  private
    FValue: IInfraType;
    function GetValue: IInfraType;
    procedure SetValue(const Value: IInfraType);
  protected
    property Value: IInfraType read GetValue write SetValue;
  end;

implementation

{ TBindableInfraType }

function TBindableInfraType.GetValue: IInfraType;
begin
  Result := FValue;
end;

procedure TBindableInfraType.SetValue(const Value: IInfraType);
begin
  FValue := Value;
end;

end.
