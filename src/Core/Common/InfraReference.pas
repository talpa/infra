{
  - look for something that can otimize the search and edit the references
    list items;
}
unit InfraReference;

interface

{$I InfraCommon.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraCommonIntf,
  List_Reference;

type
  TInfraReferenceService = class(TInterfacedObject, IInfraReferenceService)
  private
    FReferences: TInfraReferenceList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure NotifyDestruction(const Sender: IInterface);
    procedure SetReference(const Sender: IInterface; var Ref: IInterface;
      const Value: IInterface);
  end;

implementation

{ TInfraReferenceService }

constructor TInfraReferenceService.Create;
begin
  inherited;
  FReferences := TInfraReferenceList.Create;
end;

destructor TInfraReferenceService.Destroy;
begin
  {$IFDEF INFRA_CLASSNAMEDESTROYED}
  SendDebug('<<< '+Self.ClassName);
  {$ENDIF}
  FReferences.Free;
  inherited;
end;

procedure TInfraReferenceService.NotifyDestruction(const Sender: IInterface);
var
 i: Integer;
 P: PPointer;
 X: Pointer;
begin
  X := Pointer(Sender as IInterface);
  for i := FReferences.Count -1 downto 0 do
  begin
    with FReferences do
    begin
      P := ValueOfPosition(i);
      if (IndexOfPosition(i) = X) or
        (Pointer(IInterface(P^) as IInterface) = X) then
      begin
        P^ := nil;
        DeletePosition(i);
      end;
    end;
  end;
end;

procedure TInfraReferenceService.SetReference(const Sender: IInterface;
  var Ref: IInterface; const Value: IInterface);
var
  i: Integer;
  P, PSender: Pointer;
begin
  P := @Ref;
  PSender := Pointer(Sender as IInterface);
  i := FReferences.PositionOf(PSender, P);
  if (Value = nil) and (i >= 0) then
    FReferences.DeletePosition(i)
  else if i < 0 then
    FReferences.Add(PSender, P);
  Pointer(Ref) := Pointer(Value);
end;

end.
