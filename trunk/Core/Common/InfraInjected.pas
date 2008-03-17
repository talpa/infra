unit InfraInjected;

interface

{$I InfraCommon.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Classes,
  InfraCommonIntf,
  InfraCommon;

type
  TInjectedItem = class(TBaseElement,
    IInjectedItem)
  private
    FID: TGUID;
    FInjectedInterface: IInterface;
    FIsAnnotation: boolean;
    function GetID: TGUID;
    function GetInjectedInterface: IInterface;
    function GetIsAnnotation: boolean;
    procedure SetID(const Value: TGUID);
    procedure SetInjectedInterface(const Value: IInterface);
    procedure SetIsAnnotation(Value: boolean);
  public
    property ID: TGUID read GetID write SetID;
    property InjectedInterface: IInterface read GetInjectedInterface
      write SetInjectedInterface;
    property IsAnnotation: boolean read GetIsAnnotation write SetIsAnnotation;
    constructor Create(ID: TGUID; {***const }InjectedInterface: IInterface;
      WeakReference: boolean; pIsAnnotation: boolean = False); reintroduce;
  end;

implementation

uses
  SysUtils;

{ TInjectedItem }

constructor TInjectedItem.Create(ID: TGUID;
  InjectedInterface: IInterface; WeakReference: boolean;
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


