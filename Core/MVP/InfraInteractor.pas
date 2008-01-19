unit InfraInteractor;

interface

uses
  InfraCommon,
  InfraCommonIntf,
  InfraMVPIntf;

type
  TInteractor = class(TElement, IInteractor)
  private
    FView: IView;
  public
    function GetView: IView;
    procedure SetView(const Value: IView);
    property View: IView read GetView write SetView;
  end;

implementation

{ TInteractor }

function TInteractor.GetView: IView;
begin
  Result := FView;
end;

procedure TInteractor.SetView(const Value: IView);
begin
  if FView <> Value then
    FView := Value;
end;

end.
