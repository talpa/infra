unit InfraCommon;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBase, InfraCommonIntf;

type
  TBaseElement = class(TInfraBaseObject, IInterface,
    IBaseElement, IInfraReferenceKeeper, IInfraInstance)
  private
    FInjectedList: IInjectedList;
  protected
    function GetInjectedList: IInjectedList;
    function GetInstance: TObject;
    procedure InfraInitInstance; virtual;
    function Inject(const pID: TGUID; const pItem: IInterface): IInjectedItem;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    property InjectedList: IInjectedList read GetInjectedList;
    {$IFDEF SEE_REFCOUNT}
    function GetRefCount: integer;
    {$ENDIF}
    {$IFDEF TEST_REFCOUNT}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {$ENDIF}
  public
    procedure SetReference(var Ref: IInterface; const Value: IInterface);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
  end;

  TBaseElementClass = class of TBaseElement;

  TElement = class(TBaseElement, IElement,
    IInfraPublisher, ISubscriber, IInfraFilter)
  private
    FPublisher: IInfraPublisher;
  protected
    FTypeInfo: IClassInfo;
    function Apply(const Event: IInfraEvent): Boolean; virtual;
    function GetPublisher: IInfraPublisher;
    function GetTypeInfo: IClassInfo;
    procedure InitTypeInfo; virtual;
    procedure Inform(const Event: IInfraEvent); virtual;
    procedure SetTypeInfo(const Value: IClassInfo);
    property TypeInfo: IClassInfo read GetTypeInfo;
  public
    procedure InfraInitInstance; override;
    destructor Destroy; override;
    property Publisher: IInfraPublisher read GetPublisher
      implements IInfraPublisher;
  end;

  TElementClass = Class of TElement;

{$IFDEF INFRA_LEAKOBJECTS}
var
  _InfraObjectsInfras: TObjectList;

function InfraLeakObjects: TObjectList;
{$ENDIF}

implementation

uses
  SysUtils,
  List_InjectedItem,
  InfraNotify, InfraInjected;

{$IFDEF INFRA_LEAKOBJECTS}
function InfraLeakObjects: TObjectList;
begin
  if not Assigned(_InfraObjectsInfras) then
  begin
    _InfraObjectsInfras := TObjectList.Create;
    _InfraObjectsInfras.OwnsObjects := False;
  end;
  Result := _InfraObjectsInfras;
end;
{$ENDIF}

{ TBaseElement }

procedure TBaseElement.AfterConstruction;
begin
  {$IFDEF INFRA_LEAKOBJECTS}
  InfraLeakObjects.Add(Self);
  {$ENDIF}
  // Obs: Explicity change the refcount to make sure that cast inside the
  // construction process dont trigger refcouting which is
  // still zero to avoid the object being destroyied
  FRefCount := Low(Integer);
  InfraInitInstance;
  FRefCount := 0;
end;

procedure TBaseElement.InfraInitInstance;
begin
  // nothing. inplemented on descedents.
end;

procedure TBaseElement.BeforeDestruction;
begin
  inherited;
  // Obs: See coments on AfterConstruction
  FRefCount := Low(Integer);
end;

destructor TBaseElement.Destroy;
begin
  {$IFDEF INFRA_CLASSNAMEDESTROYED}
  SendDebug('<<< '+Self.ClassName);
  {$ENDIF}
  {$IFDEF INFRA_LEAKOBJECTS}
  if Assigned(_InfraObjectsInfras) and
    (_InfraObjectsInfras.Count <> 0) then
    _InfraObjectsInfras.Remove(Self);
  {$ENDIF}
  ReferenceService.NotifyDestruction(Self);
  inherited;
end;

{$IFDEF TEST_REFCOUNT}
function TBaseElement._AddRef: Integer;
begin
  Result := inherited _AddRef;
  SendDebug(Format('ADDREF:  %s.RefCount = %d', [ClassName, RefCount]));
end;

function TBaseElement._Release: Integer;
begin
  SendDebug(Format('RELEASE: %s.RefCount = %d', [ClassName, RefCount-1]));
  Result := inherited _Release;
end;
{$ENDIF}

function TBaseElement.GetInjectedList: IInjectedList;
begin
  if not Assigned(FInjectedList) then
    FInjectedList := TInjectedList.Create(Self);
  Result := FInjectedList;
end;

{
  Qualquer objeto infra aceita interfaces injetadas (sejam anotações ou não)
  Quando o programa verifica se o objeto atual suporta uma interface ou faz-se
  cast com "As" o delphi chama este método.
  Aqui primeiro verificamos se existe uma injeção da interface passada,
  se houver, testamos ainda se ela está instanciada e a retorna.
  Se nao está instanciada devemos verificar se é uma anotação por instancia
  e então instanciamos a mesma e a retornamos. Caso contrário chamamos este
  mesmo método no pai.
}
function TBaseElement.QueryInterface(const IID: TGUID;
  out Obj): HResult;
var
  i: integer;
begin
  if not Assigned(FInjectedList) then
    Result := inherited Queryinterface(IID, Obj)
  else
  begin
    i := FInjectedList.IndexByGUID(IID);
    if (i <> -1) then
    begin
      if Assigned(FInjectedList.Item[i].InjectedInterface) then
      begin
        IInterface(Obj) := FInjectedList.Item[i].InjectedInterface;
        Result := 0;
      end else
        Result := inherited Queryinterface(IID, Obj);
    end else
      Result := inherited Queryinterface(IID, Obj);
  end;
end;

function TBaseElement.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TBaseElement.SetReference(var Ref: IInterface;
  const Value: IInterface);
begin
  ReferenceService.SetReference(Self, Ref, Value);
end;

{$IFDEF SEE_REFCOUNT}
function TBaseElement.GetRefCount: integer;
begin
  Result := FRefCount;
end;
{$ENDIF}

function TBaseElement.Inject(const pID: TGUID;
  const pItem: IInterface): IInjectedItem;
var
  Index: integer;
  vItem: IInterface;
begin
  Supports(pItem, pId, vItem);
  Index := InjectedList.Add(pID, vItem);
  Result := FInjectedList.Item[Index];
  Result.IsAnnotation := False;
end;

{ TElement }

procedure TElement.InfraInitInstance;
begin
  inherited;
  InitTypeInfo;
end;

function TElement.Apply(const Event: IInfraEvent): Boolean;
begin
  Result := False;
end;

destructor TElement.Destroy;
begin
  EventService.UnSubscribeAll(Self as ISubscriber);
  inherited;
end;

function TElement.GetTypeInfo: IClassInfo;
begin
  Result := FTypeInfo;
end;

// Im not sure about this, but probably we need that
function TElement.GetPublisher: IInfraPublisher;
begin
  if not Assigned(FPublisher) then
    FPublisher := TInfraPublisher.Create(Self);
  Result := FPublisher;
end;

procedure TElement.Inform(const Event: IInfraEvent);
begin
  // nothing. inplemented on descedents.
end;

procedure TElement.InitTypeInfo;
begin
  FTypeInfo := TypeService.GetType(TInfraBaseObjectClass(Self.ClassType));
end;

procedure TElement.SetTypeInfo(const Value: IClassInfo);
begin
  if Value <> FTypeInfo then
    FTypeInfo := Value;
end;

initialization

finalization
  {$IFDEF INFRA_LEAKOBJECTS}
  if Assigned(_InfraObjectsInfras) then
    FreeAndNil(_InfraObjectsInfras);
  {$ENDIF}

end.
