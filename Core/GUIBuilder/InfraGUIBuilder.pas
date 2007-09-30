unit InfraGUIBuilder;

interface

uses
  InfraBase, InfraCommon;

type

  TInfraGUIBuilder = class(TInfraBaseObject)
  private

  public
    procedure Build(AObject: TElement);
  end;

implementation

{ TInfraGUIBuilder }

procedure TInfraGUIBuilder.Build(AObject: TElement);
begin

end;

end.
