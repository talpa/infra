unit List_InterceptMethod;

{$I InfraAspect.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBasicList,
  InfraAspectIntf,
  InfraCommonIntf,
  InfraCommon;

type
  _STACK_BASE_ = TBaseElement;
  _STACK_BASE_INTF_ = IInterceptedStack;
  _ITEM_INTF_ = IInterceptedStackItem;
  _ITERATOR_INTF_ = IInfraIterator;               // Iterator's Interface
  {$I ..\Templates\InfraTempl_Stack.inc}
  protected
    function NewInverseIterator: IInfraIterator; virtual;
  end;

  TInterceptedInverseIterator = class(TInterfacedObject, IInfraIterator)
  private
    FCurrentIndex: integer;
    FList: _STACK_;
  protected
    function CurrentItem: IInterface;
    procedure First; virtual;
    function IsDone: Boolean; virtual;
    procedure Next; virtual;
  public
    constructor Create(List: _STACK_);
  end;

  TInterceptedStack = class(_STACK_);

implementation

uses
  SysUtils;

{ TInterceptedInverseIterator }

constructor TInterceptedInverseIterator.Create(List: _STACK_);
begin
  inherited Create;
  FList := List;
  First;
end;

function TInterceptedInverseIterator.CurrentItem: IInterface;
begin
  if Assigned(FList) and (fCurrentIndex <> -1)
    and (fCurrentIndex < FList.Count) then
    Result := FList[fCurrentIndex]
  else
    Result := nil;
end;

procedure TInterceptedInverseIterator.First;
begin
  if FList.Count > 0 then
    fCurrentIndex := FList.Count-1
  else
    fCurrentIndex := -1;
end;

function TInterceptedInverseIterator.IsDone: Boolean;
begin
  Result := (fCurrentIndex < 0) or (fCurrentIndex >= FList.Count);
end;

procedure TInterceptedInverseIterator.Next;
begin
  if (FList.Count > 0) and (FCurrentIndex < FList.Count) then
    Dec(fCurrentIndex);
end;

{ TInterceptStack_Base }

{$I ..\Templates\InfraTempl_Stack.inc}

function _STACK_.NewInverseIterator: IInfraIterator;
begin
  Result := TInterceptedInverseIterator.Create(Self) as IInfraIterator;
end;

destructor _STACK_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

end.
