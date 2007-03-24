unit InfraNotify;

{$I Infra.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  SysUtils,
  InfraCommonIntf,
  InfraCommon;

type
  TInfraEvent = class(TMemoryManagedObject, IInfraEvent, IInfraEventType)
  private
    FSource: IElement;
  protected
    function GetSource: IElement;
    procedure SetSource(const Value: IElement);
    property Source: IElement read GetSource write SetSource;
  public
    procedure InfraInitInstance; override;
    constructor Create(const Src: IElement); overload;
    constructor Create(const Sender: IElement; Guid: TGUID); overload;
  end;

  TInfraEventServiceItem = class(TMemoryManagedObject,
    IInfraEventServiceItem)
  private
    FEventType: TGUID;
    FPublishers: IInfraPublisherList;
    FSubscriptions: ISubscriptionList;
    function GetEventType: TGUID;
    function GetPublishers: IInfraPublisherList;
    function GetSubscriptions: ISubscriptionList;
  public
    constructor Create(const EventType: TGUID);
    property EventType: TGUID read GetEventType;
    property Publishers: IInfraPublisherList read GetPublishers;
    property Subscriptions: ISubscriptionList read GetSubscriptions;
  end;

  TInfraEventService = class(TMemoryManagedObject, IInfraEventService)
  private
    FItems: IInfraEventServiceItems;
  protected
    function GetItems: IInfraEventServiceItems;
    function GetPublishersForSubscriberIterator(
      const Subscriber: ISubscriber): IInfraIterator;
    function GetEventItem(const EventType: TGUID): IInfraEventServiceItem;
    function HasSubscribers(const EventType: TGUID): Boolean;
    procedure Publish(const Event: IInfraEvent);
    procedure Subscribe(const EventType: TGUID; const Subscriber: ISubscriber;
      const Filter: IInfraFilter); overload;
    procedure Subscribe(const EventType: TGUID;
      const Subscriber: ISubscriber; const Inform: TSubscriberInform;
      const PropertyName: string = '';
      const Filter: TSubscriberFilter = nil); overload;
    procedure UnSubscribe(const EventType: TGUID;
      const Subscriber: ISubscriber);
    procedure UnSubscribeAll(const Subscriber: ISubscriber);
    property Items: IInfraEventServiceItems read GetItems;
  end;

  TInfraPublisher = class(TElement, IInfraPublisher)
  private
    FController: IElement;
  protected
    function HasSubscribers(const EventType: TGUID): Boolean;
    procedure Publish(const Event: IInfraEvent);
  public
    constructor Create(const Controller: IElement);
  end;

  TSubscription = class(TMemoryManagedObject, ISubscription)
  private
    FSubscriber: ISubscriber;
    function GetSubscriber: ISubscriber;
    procedure Publish(const Event: IInfraEvent); virtual; abstract;
  public
    constructor Create(const pSubscriber: ISubscriber);
    property Subscriber: ISubscriber read GetSubscriber;
  end;

  TCallBackSubscription = class(TSubscription, ICallBackSubscription)
  private
    FFilter: TSubscriberFilter;
    FInform: TSubscriberInform;
    FPropertyName: string;
    function GetFilter: TSubscriberFilter;
    function GetInform: TSubscriberInform;
    procedure Publish(const Event: IInfraEvent); override;
  public
    constructor Create(const Subscriber: ISubscriber;
      pInform: TSubscriberInform; const pPropertyName: string = '';
      pFilter: TSubscriberFilter = nil);
    property Filter: TSubscriberFilter read GetFilter;
    property Inform: TSubscriberInform read GetInform;
  end;

  TClassicSubscription = class(TSubscription, IClassicSubscription)
  private
    FFilter: IInfraFilter;
    function GetFilter: IInfraFilter;
    procedure Publish(const Event: IInfraEvent); override;
  public
    constructor Create(const Subscriber: ISubscriber; const Filter: IInfraFilter);
    property Filter: IInfraFilter read GetFilter;
  end;

implementation

uses
  List_Publisher,
  List_Subscription,
  List_EventServiceItem;

type
  TInfraPublishersForSubscriberIterator = class(TInterfacedObject,
    IInfraIterator)
  private
    FIsDone: boolean;
    FIndexEvent: integer;
    FIndexPublisher: integer;
    FSource: TInfraEventService;
    FSubscriber: ISubscriber;
    procedure FindEventHasSubscriber;
  protected
    function CurrentItem: IInterface;
    function IsDone: Boolean;
    procedure First;
    procedure Next;
  public
    constructor Create(const Source: TInfraEventService;
      const Subscriber: ISubscriber);
    destructor Destroy; override;
  end;

{ TInfraPublishersForSubscriberIterator }

constructor TInfraPublishersForSubscriberIterator.Create(
  const Source: TInfraEventService; const Subscriber: ISubscriber);
begin
  inherited Create;
  FIsDone := False;
  FIndexEvent := 0;
  FIndexPublisher := -1;
  FSource := Source;
  FSubscriber := Subscriber;
end;

function TInfraPublishersForSubscriberIterator.CurrentItem: IInterface;
begin
  if Assigned(FSource.FItems) and (FIndexEvent <> -1) and
    (FIndexPublisher <> -1) then
    Result :=
      FSource.FItems.ValueOfPosition(FIndexEvent).Publishers[FIndexPublisher]
  else
    Result := nil;
end;

function TInfraPublishersForSubscriberIterator.IsDone: Boolean;
begin
  Result := FIsDone;
end;

procedure TInfraPublishersForSubscriberIterator.First;
begin
  FIsDone := False; // *** Nunca é colocado como true
  FIndexEvent := 0; // *** deveria ser -1 nao? senão nunca será -1
  FIndexPublisher := -1;
  if FSource.FItems.Count > 0 then
    Next;
end;

procedure TInfraPublishersForSubscriberIterator.FindEventHasSubscriber;
var
  i: integer;
  vSubscriptions: ISubscriptionList;
begin
  vSubscriptions := FSource.FItems.ValueOfPosition(FIndexEvent).Subscriptions;
  for i := 0 to vSubscriptions.Count-1 do
    if vSubscriptions[i].Subscriber <> FSubscriber then
    begin
      FIndexPublisher := 0;
      Exit;
    end;
  Inc(FIndexEvent);
end;

procedure TInfraPublishersForSubscriberIterator.Next;
var
  Publishers: IInfraPublisherList;
begin
  while (FIndexEvent < FSource.FItems.Count) do
  begin
    Publishers := FSource.FItems.ValueOfPosition(FIndexEvent).Publishers;
    if (FIndexPublisher < 0) or (FIndexPublisher = Publishers.Count) then
      FindEventHasSubscriber
    else
      Inc(FIndexPublisher);
  end;
end;

destructor TInfraPublishersForSubscriberIterator.Destroy;
begin
  {$IFDEF INFRA_CLASSNAMEDESTROYED}
  SendDebug('<<< '+Self.ClassName);
  {$ENDIF}
  inherited;
end;

{ TInfraEvent }

constructor TInfraEvent.Create(const Src: IElement);
begin
  inherited Create;
  FSource := Src;
end;

constructor TInfraEvent.Create(const Sender: IElement; Guid: TGUID);
begin
  Create(Sender);
  // *** solerman: se isto aqui está correto? 
  // *** antes era: InjectedList.Add(Guid, Self);
  Inject(Guid, Self);
end;

procedure TInfraEvent.InfraInitInstance;
begin
  inherited;
end;

function TInfraEvent.GetSource: IElement;
begin
  Result := FSource;
end;

procedure TInfraEvent.SetSource(const Value: IElement);
begin
  if Value <> FSource then
    FSource := Value;
end;

{ TInfraEventServiceItem }

constructor TInfraEventServiceItem.Create(const EventType: TGUID);
begin
  inherited Create;
  FEventType := EventType;
end;

function TInfraEventServiceItem.GetEventType: TGUID;
begin
  Result := FEventType;
end;

function TInfraEventServiceItem.GetPublishers: IInfraPublisherList;
begin
  if not Assigned(FPublishers) then
    FPublishers := TInfraPublisherList.Create;
  Result := FPublishers;
end;

function TInfraEventServiceItem.GetSubscriptions: ISubscriptionList;
begin
  if not Assigned(FSubscriptions) then
    FSubscriptions := TSubscriptions.Create;
  Result := FSubscriptions;
end;

{ TInfraEventService }

function TInfraEventService.GetItems: IInfraEventServiceItems;
begin
  if not Assigned(FItems) then
    FItems := TInfraEventServiceItems.Create;
  Result := FItems;
end;

function TInfraEventService.GetPublishersForSubscriberIterator(
  const Subscriber: ISubscriber): IInfraIterator;
begin
  Result := TInfraPublishersForSubscriberIterator.Create(Self, Subscriber);
end;

function TInfraEventService.GetEventItem(
  const EventType: TGUID): IInfraEventServiceItem;
begin
  Result := Items[EventType];
  if not Assigned(Result) then
  begin
    Result := TInfraEventServiceItem.Create(EventType);
    FItems.Add(EventType, Result);
  end;
end;

function TInfraEventService.HasSubscribers(const EventType: TGUID): Boolean;
var
  EventItem: IInfraEventServiceItem;
begin
  if Assigned(FItems) then
  begin
    EventItem := FItems[EventType];
    Result := Assigned(EventItem) and (EventItem.Subscriptions.Count <> 0);
  end else
    Result := False;
end;

procedure TInfraEventService.Publish(const Event: IInfraEvent);
var
  EvtSvcItem: IInfraEventServiceItem;
  Guid: TGUID;
  ActualItem, ActualSubscription: Integer;
begin
  if not Assigned(FItems) then
    Exit;
  ActualItem := 0;
  while ActualItem < FItems.Count do
  begin
    Guid := FItems.IndexOfPosition(ActualItem);
    if Supports(Event, Guid) then
    begin
      EvtSvcItem := FItems[Guid];
      ActualSubscription := 0;
      while ActualSubscription < EvtSvcItem.Subscriptions.Count do
      begin
        EvtSvcItem.Subscriptions[ActualSubscription].Publish(Event);
        Inc(ActualSubscription);
      end;
      Break;
    end;
    Inc(ActualItem);
  end;
end;

procedure TInfraEventService.Subscribe(const EventType: TGUID;
  const Subscriber: ISubscriber; const Filter: IInfraFilter);
var
  EvtSvcItem: IInfraEventServiceItem;
begin
  EvtSvcItem := GetEventItem(EventType);
  EvtSvcItem.Subscriptions.Add(TClassicSubscription.Create(Subscriber,
    Filter) as IClassicSubscription);
end;

procedure TInfraEventService.Subscribe(const EventType: TGUID;
  const Subscriber: ISubscriber; const Inform: TSubscriberInform;
  const PropertyName: string = ''; const Filter: TSubscriberFilter = nil);
var
  EvtSvcItem: IInfraEventServiceItem;
begin
  EvtSvcItem := GetEventItem(EventType);
  EvtSvcItem.Subscriptions.Add(TCallBackSubscription.Create(Subscriber,
    Inform, PropertyName, Filter) as ISubscription);
end;

procedure TInfraEventService.UnSubscribe(const EventType: TGUID;
  const Subscriber: ISubscriber);
var
  i: Integer;
  EvtSvcItem: IInfraEventServiceItem;
begin
  if not Assigned(FItems) then
    Exit;
  EvtSvcItem := FItems[EventType];
  if Assigned(EvtSvcItem) then
    for i := EvtSvcItem.Subscriptions.Count-1 downto 0 do
      if Subscriber = EvtSvcItem.Subscriptions[i].Subscriber then
        EvtSvcItem.Subscriptions.Delete(i);
end;

procedure TInfraEventService.UnSubscribeAll(const Subscriber: ISubscriber);
var
  i, j: Integer;
  EvtSvcItem: IInfraEventServiceItem;
  Guid: TGUID;
begin
  if not Assigned(FItems) then
    Exit;
  for i := 0 to FItems.Count-1 do
  begin
    Guid := FItems.IndexOfPosition(i);
    EvtSvcItem := FItems[Guid] as IInfraEventServiceItem;
    with EvtSvcItem.Subscriptions do
      for j := Count-1 downto 0 do
        if Subscriber = Items[j].Subscriber then
          Delete(j);
  end;
end;

{ TInfraPublisher }

constructor TInfraPublisher.Create(const Controller: IElement);
begin
  inherited Create;
  SetReference(IInterface(FController), Controller);
end;

function TInfraPublisher.HasSubscribers(const EventType: TGUID): Boolean;
begin
  Result := EventService.HasSubscribers(EventType);
end;

procedure TInfraPublisher.Publish(const Event: IInfraEvent);
begin
  EventService.Publish(Event);
end;

{ TSubscription }

constructor TSubscription.Create(const pSubscriber: ISubscriber);
begin
  SetReference(IInterface(FSubscriber), pSubscriber);
  inherited Create;
end;

function TSubscription.GetSubscriber: ISubscriber;
begin
  Result := FSubscriber;
end;

{ TCallBackSubscription }

constructor TCallBackSubscription.Create(const Subscriber: ISubscriber;
  pInform: TSubscriberInform; const pPropertyName: string = '';
  pFilter: TSubscriberFilter = nil);
begin
  inherited Create(Subscriber);
  FPropertyName := pPropertyName;
  FFilter := pFilter;
  FInform := pInform;
end;

function TCallBackSubscription.GetFilter: TSubscriberFilter;
begin
  Result := FFilter;
end;

function TCallBackSubscription.GetInform: TSubscriberInform;
begin
  Result := FInform;
end;

procedure TCallBackSubscription.Publish(const Event: IInfraEvent);
var
  TheValue: IElement;
  bByProperty: Boolean;
  TheType: IClassInfo;
begin
  bByProperty := FPropertyName <> EmptyStr;
  if bByProperty then
  begin
    TheType := (FSubscriber as IElement).TypeInfo;
    TheValue := IElement(TheType.GetProperty(
      FSubscriber as IElement, FPropertyName));
    if Assigned(TheValue) and
      not IsEqualGUID(TheValue.TypeInfo.TypeID, IElement) then
      TheValue := TheValue as IElement;
  end;
  if (not bByProperty or (Assigned(TheValue) and (TheValue = Event.Source)))
    and (not Assigned(FFilter) or FFilter(Event)) then
    FInform(Event);
end;

{ TClassicSubscription }

constructor TClassicSubscription.Create(const Subscriber: ISubscriber;
  const Filter: IInfraFilter);
begin
  inherited Create(Subscriber);
  SetReference(IInterface(FFilter), Filter);
end;

function TClassicSubscription.GetFilter: IInfraFilter;
begin
  Result := FFilter;
end;

procedure TClassicSubscription.Publish(const Event: IInfraEvent);
begin
  if not Assigned(FFilter) or FFilter.Apply(Event) then
    FSubscriber.Inform(Event);
end;

end.
