unit InfraModel;

interface

uses
  InfraMVPIntf,
  InfraValueTypeIntf,
  InfraCommonIntf,
  InfraCommon;

type
  TModel = class(TElement, IModel)
  private
    FCommandSet: IInfraFeatureList;
    FSelection: ISelection;
    FValue: IInfraType;
    procedure ValueChanged(const Event: IInfraEvent);
  protected
    function GetCommandSet: IInfraFeatureList;
    property CommandSet: IInfraFeatureList read GetCommandSet;
  public
    function GetValue: IInfraType;
    function GetSelection: ISelection;
    procedure InfraInitInstance; override;
    procedure SetValue(const Value: IInfraType);
    property Value: IInfraType read GetValue write SetValue;
    property Selection: ISelection read GetSelection;
  end;

implementation

uses
  InfraNotify,
  InfraSelection,
  List_Feature;

{ TModel }

procedure TModel.InfraInitInstance;
begin
  inherited;
  EventService.Subscribe(IInfraChangedEvent, Self as ISubscriber,
    ValueChanged, 'Value');
end;

procedure TModel.ValueChanged(const Event: IInfraEvent);
begin
  Publisher.Publish(TInfraEvent.Create(Self as IElement,
    IModelValueChanged) as IInfraEvent);
end;

function TModel.GetSelection: ISelection;
begin
  if not Assigned(FSelection) then
    FSelection := TSelection.Create;
  Result := FSelection;
end;

function TModel.GetValue: IInfraType;
begin
  Result := FValue;
end;

procedure TModel.SetValue(const Value: IInfraType);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    Publisher.Publish(TInfraEvent.Create(Self as IElement,
      IModelValueExchanged) as IInfraEvent);
    Publisher.Publish(TInfraEvent.Create(Self as IElement,
      IModelValueChanged) as IInfraEvent);
  end;
end;

function TModel.GetCommandSet: IInfraFeatureList;
begin
  if not Assigned(FCommandSet) then
    FCommandSet := TInfraFeatureList.Create(Self as IElement);
  Result := FCommandSet;
end;

end.
