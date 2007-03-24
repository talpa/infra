unit InfraSelection;

interface

uses
  InfraMVPIntf,
  InfraValueType;

type
  TSelection = class(TInfraList, ISelection)
  private
    FMultiSelect: boolean;
  protected
    function GetMultiSelect: boolean;
    procedure SetMultiSelect(const Value: boolean);
    property MultiSelect: boolean read GetMultiSelect write SetMultiSelect;
  end;

procedure RegisterOnReflection;

implementation

uses
  InfraNotify,
  InfraValueTypeIntf,
  InfraCommonIntf;

procedure RegisterOnReflection;
begin
  with TypeService do
    RegisterNewType(ISelection, 'Selection', TSelection, IInfraType,
      GetTypeInfo(IInfraList));
end;

{ TSelection }

function TSelection.GetMultiSelect: boolean;
begin
  Result := FMultiSelect;
end;

procedure TSelection.SetMultiSelect(const Value: boolean);
begin
  if Value <> FMultiSelect then
  begin
    FMultiSelect := Value;
    if not FMultiSelect then
      Clear;
    Publisher.Publish(TInfraEvent.Create(Self,
      IInfraMultiSelectChanged) as IInfraEvent);
  end;
end;

end.
