unit InfraView;

interface

uses
  InfraCommon,
  InfraMVPIntf;

type
  TView = class(TElement, IView)
  private
    FParentView: IView;
    FPresenter: IPresenter;
    FSubViews: IViewList;
  protected
    FRenderer: IRenderer;
    function GetParentView: IView;
    function GetSubViews: IViewList;
    function GetTopView: IView;
    procedure SetParentView(const Value: IView);
    procedure SetRenderer(const Value: IRenderer);
    function HasSubViews: boolean;
    procedure Update; virtual;
    procedure UpdateModel; virtual;
    property ParentView: IView read GetParentView;
    property SubViews: IViewList read GetSubViews;
    property TopView: IView read GetTopView;
  public
    function GetModel: IModel;
    function GetPresenter: IPresenter;
    function GetRenderer: IRenderer; virtual;
    procedure SetPresenter(const Value: IPresenter); virtual;
    property Model: IModel read GetModel;
    property Presenter: IPresenter read GetPresenter write SetPresenter;
    property Renderer: IRenderer read GetRenderer write SetRenderer;
  end;

implementation

uses
  SysUtils,
  InfraRenderer,
  List_View;

{ TView }

function TView.GetPresenter: IPresenter;
begin
  Result := FPresenter;
end;

function TView.GetModel: IModel;
begin
  if Assigned(Presenter) then
    Result := Presenter.Model
  else
    Result := nil;
end;

procedure TView.Update;
begin
  // do nothing here, just on descendents!
end;

procedure TView.UpdateModel;
begin
  // do nothing here, just on descendents!
end;

function TView.GetParentView: IView;
begin
  Result := FParentView;
end;

procedure TView.SetPresenter(const Value: IPresenter);
begin
  if Value <> FPresenter then
  begin
    SetReference(IInterface(FPresenter), Value);
    if Assigned(Presenter) then
      Presenter.View := Self as IView
    else
      Presenter.View := nil;
  end;
  Update;
end;

function TView.GetTopView: IView;
begin
  if Assigned(ParentView) then
    Result := ParentView.TopView
  else
    Result := Self as IView;
end;

function TView.GetRenderer: IRenderer;
begin
  if not Assigned(FRenderer) then
    FRenderer := TRenderer.Create;
  Result := FRenderer;
end;

procedure TView.SetRenderer(const Value: IRenderer);
begin
  if Value <> FRenderer then
  begin
    FRenderer := Value;
    FRenderer.View := Self as IView;
  end;
end;

procedure TView.SetParentView(const Value: IView);
begin
  SetReference(IInterface(FParentView), Value);
end;

function TView.HasSubViews: boolean;
begin
  Result := Assigned(FSubViews) and (FSubViews.Count <> 0);
end;

function TView.GetSubViews: IViewList;
begin
  if not Assigned(FSubViews) then
  begin
    FSubViews := TViewList.Create;
    FSubViews.Owner := Self as IView;
  end;
  Result := FSubViews;
end;

end.
