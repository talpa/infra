unit InfraPresenter;

interface

uses
  InfraCommon,
  InfraValueTypeIntf,
  InfraCommonIntf,
  InfraMVPIntf;

type
  TPresenter = class(TElement, IPresenter)
  private
    FModel: IModel;
    FView: IView;
    FName: string;
    FCommandSet: IInfraFeatureList;
    FParentPresenter: IPresenter;
    FSubPresenters: IPresenterList;
    procedure ModelValueChanged(const Event: IInfraEvent);
    procedure ModelValueExchanged(const Event: IInfraEvent);
  protected
    function GetCommandSet: IInfraFeatureList;
    function GetName: string;
    function HasSubPresenters: boolean;
    function GetSubPresenters: IPresenterList;
    procedure BindCommands; virtual;
    procedure SetName(const Value: string);
    procedure SetupSubPresenters; virtual;
    procedure UpdatePresenter; virtual;
  public
    function GetModel: IModel;
    function GetParentPresenter: IPresenter;
    function GetView: IView;
    procedure SetModel(const Value: IModel);
    procedure SetParentPresenter(const Value: IPresenter);
    procedure SetView(const Value: IView);
    property CommandSet: IInfraFeatureList read GetCommandSet;
    property Model: IModel read GetModel write SetModel;
    property Name: string read GetName write SetName;
    property ParentPresenter: IPresenter read GetParentPresenter;
    property SubPresenters: IPresenterList read GetSubPresenters;
    property View: IView read GetView write SetView;
    procedure InfraInitInstance; override;
  end;

  TValuePresenter = class(TPresenter, IValuePresenter);

  TTextPresenter = class(TValuePresenter, ITextPresenter);

  TNumberPresenter = class(TValuePresenter, INumberPresenter);

  TIntegerPresenter = class(TNumberPresenter, IIntegerPresenter);

  TDateTimePresenter = class(TValuePresenter, IDateTimePresenter);

  TBooleanPresenter = class(TValuePresenter, IBooleanPresenter);

  TListPresenter = class(TPresenter, IListPresenter)
  protected
    function GetListItem: IListItemPresenter;
  public
    property ListItem: IListItemPresenter read GetListItem;
  end;

  TListItemPresenter = class(TPresenter, IListItemPresenter);

implementation

uses
  List_Presenter,
  List_Feature;

{ TPresenter }

procedure TPresenter.InfraInitInstance;
begin
  inherited;
  EventService.Subscribe(IModelValueChanged, Self as ISubscriber,
    ModelValueChanged, 'Model');
  EventService.Subscribe(IModelValueExchanged, Self as ISubscriber,
    ModelValueExchanged, 'Model');
  BindCommands;
end;

procedure TPresenter.ModelValueChanged(const Event: IInfraEvent);
begin
  UpdatePresenter
end;

procedure TPresenter.ModelValueExchanged(const Event: IInfraEvent);
begin
  SetupSubPresenters;
end;

procedure TPresenter.BindCommands;
begin
  // inherited in descendents
end;

function TPresenter.HasSubPresenters: boolean;
begin
  Result := Assigned(FSubPresenters) and (FSubPresenters.Count <> 0);
end;

function TPresenter.GetModel: IModel;
begin
  Result := FModel;
end;

function TPresenter.GetName: string;
begin
  Result := FName;
end;

function TPresenter.GetParentPresenter: IPresenter;
begin
  Result := FParentPresenter;
end;

function TPresenter.GetSubPresenters: IPresenterList;
begin
  if not Assigned(FSubPresenters) then
  begin
    FSubPresenters := TPresenterList.Create;
    FSubPresenters.Owner := Self as IPresenter;
  end;
  Result := FSubPresenters;
end;

function TPresenter.GetView: IView;
begin
  Result := FView;
end;

procedure TPresenter.SetModel(const Value: IModel);
begin
  if Value <> FModel then
    FModel := Value;
end;

procedure TPresenter.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TPresenter.SetParentPresenter(const Value: IPresenter);
begin
  SetReference(IInterface(FParentPresenter), Value);
end;

procedure TPresenter.SetView(const Value: IView);
begin
  if Value <> FView then
  begin
    FView := Value;
    FView.Presenter := Self as IPresenter;
  end;
end;

procedure TPresenter.UpdatePresenter;
var
  Iterator: IInfraIterator;
begin
  If Assigned(View) then
    View.Update;
  if HasSubPresenters then begin
    Iterator := SubPresenters.NewIterator;
    while not Iterator.IsDone do
    begin
      (Iterator.CurrentItem as IPresenter).UpdatePresenter;
      Iterator.Next;
    end;
  end;
end;

procedure TPresenter.SetupSubPresenters;
begin
  // inherited in descendents
end;

function TPresenter.GetCommandSet: IInfraFeatureList;
begin
  if not Assigned(FCommandSet) then
    FCommandSet := TInfraFeatureList.Create(Self as IElement);
  Result := FCommandSet;
end;

{ TListPresenter }

function TListPresenter.GetListItem: IListItemPresenter;
begin
  Result := SubPresenters.Items[0] as IListItemPresenter;
end;

end.
