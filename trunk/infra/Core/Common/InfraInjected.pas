unit InfraInjected;

{$I Infra.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Classes,
  InfraCommonIntf,
  InfraCommon;

type
  TInjectedItem = class(TMemoryManagedObject,
    IInjectedItem)
  private
    FID: TGUID;
    FInjectedInterface: IInterface;
    FIsAnnotation: boolean;
  protected
    function GetID: TGUID;
    function GetInjectedInterface: IInterface;
    function GetIsAnnotation: boolean;
    procedure SetID(const Value: TGUID);
    procedure SetInjectedInterface(const Value: IInterface);
    procedure SetIsAnnotation(Value: boolean);
    property ID: TGUID read GetID write SetID;
    property InjectedInterface: IInterface read GetInjectedInterface
      write SetInjectedInterface;
    property IsAnnotation: boolean read GetIsAnnotation write SetIsAnnotation;
  public
    constructor Create(ID: TGUID; const InjectedInterface: IInterface;
      WeakReference: boolean; pIsAnnotation: boolean = False);
  end;

implementation

uses
  SysUtils;

{ TInjectedItem }

constructor TInjectedItem.Create(ID: TGUID;
  const InjectedInterface: IInterface; WeakReference: boolean;
  pIsAnnotation: boolean = False);
begin
  inherited Create;
  FID := ID;
  FIsAnnotation := pIsAnnotation;
  if WeakReference then
    SetReference(IInterface(FInjectedInterface), InjectedInterface)
  else
    FInjectedInterface := InjectedInterface;
end;

function TInjectedItem.GetID: TGUID;
begin
  Result := FID;
end;

function TInjectedItem.GetInjectedInterface: IInterface;
begin
  Result := FInjectedInterface;
end;

function TInjectedItem.GetIsAnnotation: boolean;
begin
  Result := FIsAnnotation;
end;

procedure TInjectedItem.SetID(const Value: TGUID);
begin
  FID := Value;
end;

procedure TInjectedItem.SetInjectedInterface(
  const Value: IInterface);
begin
  FInjectedInterface := Value;
end;

procedure TInjectedItem.SetIsAnnotation(Value: boolean);
begin
  FIsAnnotation := Value;
end;

end.


