unit List_InjectedItem;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Classes, InfraCommon, InfraCommonIntf;

type
  TInjectedList = class(TMemoryManagedObject, IInjectedList)
  private
    FItems: IInterfaceList;
    FOwner: IMemoryManagedObject;
    function Add(const ID: TGUID; const pObject: IInterface): Integer;
    function GetCount: Integer;
    function GetItem(Index: integer): IInjectedItem;
    function IndexByGUID(const Item: TGUID): Integer;
    procedure Clear;
  public
    property Count: Integer read GetCount;
    property Item[Index: integer]: IInjectedItem read GetItem; default;
    constructor Create(const AOwner: IMemoryManagedObject); reintroduce;
    destructor Destroy; override;
  end;

  TAnnotationsIterator = class(TInterfacedObject, IAnnotationsIterator)
  private
    FCurrentIndex: integer;
    FList: IInjectedList;
  protected
    function CurrentItem: IInjectedItem;
    procedure First; virtual;
    function IsDone: Boolean; virtual;
    procedure Next; virtual;
  public
    constructor Create(const List: IInjectedList);
  end;

implementation

uses
  SysUtils, InfraInjected;

{ TInjectedList }

constructor TInjectedList.Create(const AOwner: IMemoryManagedObject);
begin
  inherited Create;
  FItems := TInterfaceList.Create;
  SetReference(IInterface(FOwner), AOwner);
end;

destructor TInjectedList.Destroy;
begin
  FItems.Clear;
  inherited;
end;

function TInjectedList.Add(const ID: TGUID; const pObject: IInterface): Integer;
begin
  if not Supports(FOwner, ID) then
    Result := FItems.Add(TInjectedItem.Create(ID, pObject,
      IElement(FOwner) = pObject))
  else
    Result := IndexByGUID(ID);
end;

function TInjectedList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TInjectedList.GetItem(Index: integer): IInjectedItem;
begin
  if Assigned(FItems[index]) then
    Result := (FItems[index] as IInjectedItem)
  else
    Result := nil;
end;

function TInjectedList.IndexByGUID(const Item: TGUID): Integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FItems.Count-1 do
    if IsEqualGUID((FItems[i] as IInjectedItem).ID, Item) then
    begin
      Result := i;
      Break;
    end;
end;

procedure TInjectedList.Clear;
begin
  FItems.Clear;
end;

{ TAnnotationsIterator }

constructor TAnnotationsIterator.Create(const List: IInjectedList);
begin
  inherited Create;
  FList := List;
  First;
end;

function TAnnotationsIterator.CurrentItem: IInjectedItem;
begin
  if Assigned(FList) and (fCurrentIndex <> -1)
    and (fCurrentIndex < FList.Count) then
    Result := FList[fCurrentIndex] as IInjectedItem
  else
    Result := nil;
end;

procedure TAnnotationsIterator.First;
var
  i: integer;
begin
  fCurrentIndex := -1;
  if FList.Count > 0 then
    for i := 0 to Pred(FList.Count) do
    begin
      if (FList[i] as IInjectedItem).IsAnnotation then
      begin
        fCurrentIndex := i;
        Break;
      end;
    end;
end;

function TAnnotationsIterator.IsDone: Boolean;
begin
  Result := (fCurrentIndex < 0) or (fCurrentIndex >= FList.Count);
end;

procedure TAnnotationsIterator.Next;
begin
  if (FList.Count > 0) then
    while (FCurrentIndex < FList.Count) and
      not (FList[fCurrentIndex+1] as IInjectedItem).IsAnnotation do
      Inc(fCurrentIndex);
end;

end.
