unit InfraVCLEvents;

interface

uses
  Forms,
  Controls,
  StdCtrls,
  Classes, 
  Types,
  Messages,
  InfraView,
  InfraFriendClasses,
  InfraNotify,
  InfraCommonIntf,
  InfraMVPIntf,
  InfraRenderer,
  InfraValueTypeIntf,
  InfraMVPVCLIntf;

type
  TVCLNotifyEvent = class(TInfraEvent, IVCLNotifyEvent);

  TVCLDragEvent = class(TInfraEvent, IVCLDragEvent)
  private
    FViewObject: IView;
    FX: Integer;
    FY: Integer;
  protected
    function GetViewObject: IView;
    function GetX: Integer;
    function GetY: Integer;
    property ViewObject: IView read GetViewObject;
    property X: Integer read GetX;
    property Y: Integer read GetY;
  public
    constructor Create(const Sender: IElement;
      aObject: TObject; X, Y: Integer);
  end;

  TVCLDragDropEvent = class(TVCLDragEvent, IVCLDragDropEvent);

  TVCLDragOverEvent = class(TVCLDragEvent, IVCLDragOverEvent)
  private
    FState: TDragState;
    FAccept: PBoolean;
  protected
    function GetAccept: boolean;
    function GetState: TDragState;
    procedure SetAccept(Value: boolean);
    property Accept: boolean read GetAccept write SetAccept;
    property State: TDragState read GetState;
  public
    constructor Create(const Sender: IElement; aObject: TObject;
      X, Y: Integer; State: TDragState; var Accept: boolean);
  end;

  TVCLEndDragEvent = class(TVCLDragEvent, IVCLEndDragEvent);

  TVCLMouseMoveEvent = class(TInfraEvent, IVCLMouseMoveEvent)
  private
    FShift: TShiftState;
    FX: Integer;
    FY: Integer;
  protected
    function GetShift: TShiftState;
    function GetX: Integer;
    function GetY: Integer;
    property Shift: TShiftState read GetShift;
    property X: Integer read GetX;
    property Y: Integer read GetY;
  public
    constructor Create(const Sender: IElement; Shift: TShiftState;
      X, Y: Integer);
  end;

  TVCLMouseEvent = class(TVCLMouseMoveEvent, IVCLMouseEvent)
  private
    FButton: TMouseButton;
  protected
    function GetButton: TMouseButton;
    property Button: TMouseButton read GetButton;
  public
    constructor Create(const Sender: IElement; Guid: TGUID;
      aButton: TMouseButton; Shift: TShiftState; X, Y: Integer);
  end;

  TVCLStartDragEvent = class(TInfraEvent, IVCLStartDragEvent)
  private
    FDragObject: ^TDragObject;
  protected
    function GetDragObject: TDragObject;
    procedure SetDragObject(Value: TDragObject);
    property DragObject: TDragObject read GetDragObject write SetDragObject;
  public
    constructor Create(const Sender: IElement; var DragObject: TDragObject);
  end;

  TContextPopupEvent = class(TInfraEvent, IContextPopupEvent)
  private
    FHandled: ^boolean;
    FMousePos: TPoint;
  protected
    function GetHandled: boolean;
    function GetMousePos: TPoint;
    procedure SetHandled(Value: Boolean);
    property MousePos: TPoint read GetMousePos;
    property Handled: Boolean read GetHandled write SetHandled;
  public
    constructor Create(const Sender: IElement;
      MousePos: TPoint; var Handled: Boolean);
  end;

  TKeyBoardEvent = class(TInfraEvent, IKeyBoardEvent);

  TKeyPressEvent = class(TKeyBoardEvent, IKeyPressEvent)
  private
    FKey: PChar;
  protected
    function GetKey: Char;
    procedure SetKey(Value: Char);
    property Key: Char read GetKey write SetKey;
  public
    constructor Create(const Sender: IElement; var Key: Char);
  end;

  TKeyEvent = class(TKeyBoardEvent, IKeyEvent)
  private
    FKey: PWord;
    FShiftState: TShiftState;
  protected
    function GetKey: word;
    function GetShift: TShiftState;
    procedure SetKey(AKey: word);
    property Key: word read GetKey write SetKey;
    property Shift: TShiftState read GetShift;
  public
    constructor Create(const Sender: IElement; Guid: TGUID; var Key: word;
      var ShiftState: TShiftState);
  end;

implementation

uses
  SysUtils;

{ TVCLDragEvent }

constructor TVCLDragEvent.Create(const Sender: IElement;
  aObject: TObject; X, Y: Integer);
begin
  inherited Create(Sender);
  FViewObject := (Source as IVCLView).FindViewFromObject(aObject);
  FX := X;
  FY := Y;
end;

function TVCLDragEvent.GetViewObject: IView;
begin
  Result := FViewObject;
end;

function TVCLDragEvent.GetX: Integer;
begin
  Result := FX;
end;

function TVCLDragEvent.GetY: Integer;
begin
  Result := FY;
end;

{ TVCLDragOverEvent }

constructor TVCLDragOverEvent.Create(const Sender: IElement;
  aObject: TObject; X, Y: Integer; State: TDragState; var Accept: boolean);
begin
  Inherited Create(Sender, aObject, X, Y);
  FAccept := @Accept;
  FState := State;
end;

function TVCLDragOverEvent.GetAccept: boolean;
begin
  Result := FAccept^;
end;

function TVCLDragOverEvent.GetState: TDragState;
begin
  Result := FState;
end;

procedure TVCLDragOverEvent.SetAccept(Value: boolean);
begin
  FAccept^ := Value;
end;

{ TVCLMouseMoveEvent }

constructor TVCLMouseMoveEvent.Create(const Sender: IElement;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited Create(Sender);
  FShift := Shift;
  FX := X;
  FY := Y;
end;

function TVCLMouseMoveEvent.GetX: Integer;
begin
  Result := FX;
end;

function TVCLMouseMoveEvent.GetY: Integer;
begin
  Result := FY;
end;

function TVCLMouseMoveEvent.GetShift: TShiftState;
begin
  Result := FShift;
end;

{ TVCLMouseEvent }

constructor TVCLMouseEvent.Create(const Sender: IElement;
  Guid: TGUID; aButton: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited Create(Sender, Shift, X, Y);
  InjectedInterfaces.Add(Guid, Self);
  FButton := aButton;
end;

function TVCLMouseEvent.GetButton: TMouseButton;
begin
  Result := FButton;
end;

{ TVCLStartDragEvent }

constructor TVCLStartDragEvent.Create(const Sender: IElement;
  var DragObject: TDragObject);
begin
  inherited Create(Sender);
  FDragObject := @DragObject;
end;

function TVCLStartDragEvent.GetDragObject: TDragObject;
begin
  Result := FDragObject^;
end;

procedure TVCLStartDragEvent.SetDragObject(Value: TDragObject);
begin
  FDragObject^ := Value;
end;

{ TContextPopupEvent }

constructor TContextPopupEvent.Create(const Sender: IElement;
  MousePos: TPoint; var Handled: Boolean);
begin
  inherited Create(Sender);
  FHandled := @Handled;
  FMousePos := MousePos;
end;

function TContextPopupEvent.GetHandled: boolean;
begin
  Result := FHandled^;
end;

function TContextPopupEvent.GetMousePos: TPoint;
begin
  Result := FMousePos;
end;

procedure TContextPopupEvent.SetHandled(Value: Boolean);
begin
  FHandled^ := Value;
end;

{ TKeyPressEvent }

constructor TKeyPressEvent.Create(const Sender: IElement;
  var Key: Char);
begin
  Inherited Create(Sender);
  FKey := @Key;
end;

function TKeyPressEvent.GetKey: Char;
begin
  Result := FKey^;
end;

procedure TKeyPressEvent.SetKey(Value: Char);
begin
  FKey^ := Value;
end;

{ TKeyEvent }

constructor TKeyEvent.Create(const Sender: IElement; Guid: TGUID;
  var Key: word; var ShiftState: TShiftState);
begin
  inherited Create(Sender);
  InjectedInterfaces.Add(Guid, Self);
  FKey := @Key;
  FShiftState := ShiftState;
end;

function TKeyEvent.GetKey: word;
begin
  Result := FKey^;
end;

function TKeyEvent.GetShift: TShiftState;
begin
  Result := FShiftState;
end;

procedure TKeyEvent.SetKey(AKey: word);
begin
  FKey^ := AKey;
end;

end.
