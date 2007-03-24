unit InfraCommand;

interface

uses
  InfraMVPIntf,
  InfraCommonIntf,
  InfraValueTypeIntf,
  InfraValueType;

type
  TCommand = class(TInfraMethod, ICommand)
  private
    FChecked: IInfraBoolean;
    FEnabled: IInfraBoolean;
    FGroupIndex: IInfraInteger;
    FRadioItem: IInfraBoolean;
    // It is required mapper the method InternalExecute to again.
    // Delphi not was calling InternalExecute when anyone call
    // SomeCommand.Execute.
    procedure ICommand.Execute = InternalExecute;
  protected
    function CanUndo: Boolean; virtual;
    function GetChecked: boolean;
    function GetEnabled: boolean;
    function GetGroupIndex: Integer;
    function GetRadioItem: boolean;
    procedure SetChecked(Value: boolean);
    procedure SetEnabled(Value: boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetRadioItem(Value: boolean);
    procedure Undo; virtual;
    property Checked: boolean read GetChecked write SetChecked;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex;
    property RadioItem: boolean read GetRadioItem write SetRadioItem;
  public
    procedure InfraInitInstance; override;
  end;

  TAppendCommand = class(TCommand, IAppendCommand)
  protected
    procedure Execute(const Parameters: IInfraList); override;
  end;

  TDeleteCommand = class(TCommand, IDeleteCommand)
  protected
    procedure Execute(const Parameters: IInfraList); override;
  end;

  TSelectionCommand = class(TCommand, ISelectionCommand)
  protected
    procedure Update(const Event: IInfraEvent);
  public
    function GetSelection: IInfraList;
    procedure InfraInitInstance; override;
    property Selection: IInfraList read GetSelection;
  end;

procedure RegisterOnReflection;

implementation

uses
  SysUtils,
  InfraNotify;  // Só por causa do TInfraEvent que é necessário aqui.

procedure RegisterOnReflection;
begin
  with TypeService do
  begin
    RegisterNewType(ICommand, 'Command', TCommand,
      IInfraType, GetTypeInfo(IInfraMethod));

    RegisterNewType(IAppendCommand, 'AppendCommand', TAppendCommand,
      ICommand, GetTypeInfo(ICommand));

    RegisterNewType(IDeleteCommand, 'DeleteCommand', TDeleteCommand,
      ICommand, GetTypeInfo(ICommand));

    with RegisterNewType(ISelectionCommand, 'SelectionCommand',
      TSelectionCommand, ICommand, GetTypeInfo(ICommand)) do
      AddMember('Selection', ISelection, @TSelectionCommand.GetSelection);
  end;
end;

{ TCommand }

procedure TCommand.InfraInitInstance;
var
  This: IElement;
begin
  inherited;
  This := Self as IElement;

  FChecked := TInfraBoolean.create;
  (FChecked as IInfraFeature).Owner := This;

  FEnabled := TInfraBoolean.create;
  (FEnabled as IInfraFeature).Owner := This;

  FGroupIndex := TInfraInteger.create;
  (FGroupIndex as IInfraFeature).Owner := This;

  FRadioItem := TInfraBoolean.create;
  (FRadioItem as IInfraFeature).Owner := This;
end;

function TCommand.CanUndo: Boolean;
begin
  Result := False;
end;

function TCommand.GetChecked: boolean;
begin
  Result := FChecked.AsBoolean;
end;

function TCommand.GetEnabled: boolean;
begin
  Result := FEnabled.AsBoolean;
end;

function TCommand.GetGroupIndex: Integer;
begin
  Result := FGroupIndex.AsInteger;
end;

function TCommand.GetRadioItem: boolean;
begin
  Result := FRadioItem.AsBoolean;
end;

procedure TCommand.SetChecked(Value: boolean);
begin
  if Value <> FChecked.AsBoolean then
  begin
    FChecked.AsBoolean := Value;
    Publisher.Publish(TInfraEvent.Create(Self,
      ICommandChangedEvent) as IInfraEvent);
  end;
end;

procedure TCommand.SetEnabled(Value: boolean);
begin
  if Value <> FEnabled.AsBoolean then
  begin
    FEnabled.AsBoolean := Value;
    Publisher.Publish(TInfraEvent.Create(Self,
      ICommandChangedEvent) as IInfraEvent);
  end;
end;

procedure TCommand.SetGroupIndex(Value: Integer);
begin
  if Value <> FGroupIndex.AsInteger then
  begin
    FGroupIndex.AsInteger := Value;
    Publisher.Publish(TInfraEvent.Create(Self,
      ICommandChangedEvent) as IInfraEvent);
  end;
end;

procedure TCommand.SetRadioItem(Value: boolean);
begin
  if Value <> FRadioItem.AsBoolean then
  begin
    FRadioItem.AsBoolean := Value;
    Publisher.Publish(TInfraEvent.Create(Self,
      ICommandChangedEvent) as IInfraEvent);
  end;
end;

procedure TCommand.Undo;
begin
  Raise Exception.Create('Undo not available!');
end;

{ TAppendCommand }

procedure TAppendCommand.Execute(const Parameters: IInfraList);
var
  List: IInfraList;
begin
  List := (Owner as IModel).Value as IInfraList;
  FResult := List.Append(Parameters[0] as IInfraType);
end;

{ TDeleteCommand }

procedure TDeleteCommand.Execute(const Parameters: IInfraList);
var
  Model: IModel;
  i: Integer;
begin
  Model := Owner as IModel;
  with Model.Value as IInfraList do
    for i := 0 to Model.Selection.Count-1 do
      Remove(Model.Selection[i]);
end;

{ TSelectionCommand }

procedure TSelectionCommand.InfraInitInstance;
begin
  inherited;
  EventService.Subscribe(IInfraChangedEvent,
    Self as ISubscriber, Update, 'Selection');
end;

function TSelectionCommand.GetSelection: IInfraList;
begin
  Result := (Owner as IPresenter).Model.Selection as IInfraList;
end;

procedure TSelectionCommand.Update(const Event: IInfraEvent);
begin
  Enabled := (Event.Source as IInfraList).Count <> 0;
end;

end.
