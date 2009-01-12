unit InfraVCLPresenter;

interface

uses
  InfraPresenter,
  InfraMVPIntf,
  InfraMVPVCLIntf;

type
  TContainerPresenter = class(TPresenter, IContainerPresenter);

  TCustomFormPresenter = class(TContainerPresenter, ICustomFormPresenter)
  protected
    procedure Execute; virtual;
  end;

implementation

{ TCustomFormPresenter }

procedure TCustomFormPresenter.Execute;
begin
  (View as IVCLCustomFormView).Show;
end;

end.
