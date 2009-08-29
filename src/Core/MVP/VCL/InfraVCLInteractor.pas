unit InfraVCLInteractor;

interface

uses
  InfraCommon,
  InfraInteractor,
  InfraCommonIntf,
  InfraMVPVCLIntf;

type
  TObjectSelectionUpdater = class(TInteractor, IObjectSelectionUpdater)
  private
    procedure ControlEnterEvent(const Event: IInfraEvent);
    procedure ControlExitEvent(const Event: IInfraEvent);
  protected
    function ApplyFilter(const Event: IInfraEvent): Boolean;
  public
    procedure InfraInitInstance; override;
  end;

  TExitEventModelUpdater = class(TInteractor, IExitEventModelUpdater)
  protected
    procedure ControlExitEvent(const Event: IInfraEvent);
  public
    procedure InfraInitInstance; override;
  end;

  TClickEventModelUpdater = class(TInteractor, IClickEventModelUpdater)
  protected
    procedure ClickEvent(const Event: IInfraEvent);
  public
    procedure InfraInitInstance; override;
  end;

  TKeyBoardInteractor = class(TInteractor, IKeyBoardInteractor)
  protected
    procedure KeyDownEvent(const Event: IInfraEvent); virtual;
    procedure KeyPressEvent(const Event: IInfraEvent); virtual;
    procedure KeyUPEvent(const Event: IInfraEvent); virtual;
  public
    procedure InfraInitInstance; override;
  end;

  TInputNumberInteractor = class(TKeyBoardInteractor,
    IInputNumberInteractor)
  private
    FAcceptDecimals: boolean;
    FAcceptSignal: boolean;
  protected
    procedure KeyPressEvent(const Event: IInfraEvent); override;
    function GetAcceptDecimals: boolean;
    function GetAcceptSignal: boolean;
    procedure SetAcceptDecimals(const Value: boolean);
    procedure SetAcceptSignal(const Value: boolean);
    property AcceptDecimals: boolean read GetAcceptDecimals write SetAcceptDecimals;
    property AcceptSignal: boolean read GetAcceptSignal write SetAcceptSignal;
  public
    procedure InfraInitInstance; override;
  end;

implementation

uses
  SysUtils,
  InfraMVPIntf,
  InfraValueTypeIntf;

{ TObjectSelectionUpdater }

procedure TObjectSelectionUpdater.InfraInitInstance;
begin
  inherited;
  EventService.Subscribe(IExitEvent, Self as ISubscriber,
    ControlExitEvent, EmptyStr, ApplyFilter);
  EventService.Subscribe(IEnterEvent, Self as ISubscriber,
    ControlEnterEvent, EmptyStr, ApplyFilter);
end;

function TObjectSelectionUpdater.ApplyFilter(const Event: IInfraEvent): Boolean;
begin
  Result := View.HasSubViews and ((Event.Source as IView).ParentView = View);
end;

procedure TObjectSelectionUpdater.ControlExitEvent(const Event: IInfraEvent);
begin
  inherited Inform(Event);
  View.Model.Selection.Clear;
end;

procedure TObjectSelectionUpdater.ControlEnterEvent(const Event: IInfraEvent);
begin
  inherited Inform(Event);
  with View.Model do
    Selection.Add(Value as IInfraType);
end;

{ TExitEventModelUpdater }

procedure TExitEventModelUpdater.InfraInitInstance;
begin
  inherited;
  EventService.Subscribe(IExitEvent, Self as ISubscriber,
    ControlExitEvent, 'View');
end;

procedure TExitEventModelUpdater.ControlExitEvent(const Event: IInfraEvent);
begin
  inherited Inform(Event);
  View.UpdateModel;
end;

{ TClickEventModelUpdater }

procedure TClickEventModelUpdater.InfraInitInstance;
begin
  inherited;
  EventService.Subscribe(IClickEvent, Self as ISubscriber,
    ClickEvent, 'View');
end;

procedure TClickEventModelUpdater.ClickEvent(const Event: IInfraEvent);
begin
  View.UpdateModel;
end;

{ TKeyBoardInteractor }

procedure TKeyBoardInteractor.InfraInitInstance;
begin
  inherited;
  EventService.Subscribe(IKeyPressEvent, Self as ISubscriber,
    KeyPressEvent, 'View');
  EventService.Subscribe(IKeyDownEvent, Self as ISubscriber,
    KeyDownEvent, 'View');
  EventService.Subscribe(IKeyUpEvent, Self as ISubscriber,
    KeyUPEvent, 'View');
end;

procedure TKeyBoardInteractor.KeyDownEvent(const Event: IInfraEvent);
begin
  // implemented on descendents
end;

procedure TKeyBoardInteractor.KeyPressEvent(const Event: IInfraEvent);
begin
  // implemented on descendents
end;

procedure TKeyBoardInteractor.KeyUpEvent(const Event: IInfraEvent);
begin
  // implemented on descendents
end;

{ TInputNumberInteractor }

procedure TInputNumberInteractor.InfraInitInstance;
begin
  inherited;
  FAcceptDecimals := True;
  FAcceptSignal := True;
end;

function TInputNumberInteractor.GetAcceptDecimals: boolean;
begin
  Result := FAcceptDecimals;
end;

function TInputNumberInteractor.GetAcceptSignal: boolean;
begin
  Result := FAcceptSignal;
end;

procedure TInputNumberInteractor.KeyPressEvent(const Event: IInfraEvent);
var
  KeyPress: IKeyPressEvent;
  OldText: string;
begin
  inherited KeyPressEvent(Event);
  OldText := (View as IVCLCustomEditView).Text;
  KeyPress := Event as IKeyPressEvent;
  if (not FAcceptSignal and (KeyPress.Key in [ThousandSeparator, '-'])) or
    (not FAcceptDecimals and (KeyPress.Key = DecimalSeparator)) then
    KeyPress.Key := #0
  else if FAcceptSignal and (KeyPress.Key = '-') and
    (Pos('-', OldText) <> 0) { TO_DO 001 } then
    KeyPress.Key := #0
  else if FAcceptDecimals and (KeyPress.Key = DecimalSeparator) and
    (Pos(DecimalSeparator, OldText) <> 0) then
    KeyPress.Key := #0
  else if not (KeyPress.Key in ['0'..'9', #8, '-', DecimalSeparator,
    ThousandSeparator]) then
    KeyPress.Key := #0
end;

procedure TInputNumberInteractor.SetAcceptDecimals(
  const Value: boolean);
begin
  FAcceptDecimals := Value;
end;

procedure TInputNumberInteractor.SetAcceptSignal(
  const Value: boolean);
begin
  FAcceptSignal := Value;
end;

end.



