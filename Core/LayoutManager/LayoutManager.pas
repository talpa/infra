unit LayoutManager;

interface

uses
  Classes, Controls, ExtCtrls, Messages, Windows, Math, Contnrs, Dialogs,
  StdCtrls, SysUtils, Graphics, Forms, TypInfo;

const
  CM_FREEITEM = WM_USER + 1;
  CM_LMREALIGNITEMS = WM_USER + 2;
  CM_LMRESIZEITEMS = WM_USER + 3;
  CM_POSITIONINGCONTROL = WM_USER + 4;
  CM_REALIGNITEM = WM_USER + 5;
  CM_RESIZECONTROL = WM_USER + 6;
  CM_RESIZEITEM = WM_USER + 7;

type

  THackBoundLabel = class(TBoundLabel);

  TLayoutManager = class;

  TLayoutManagerItem = class;

  TLayoutOrientation = (laHorizontal, laVertical);

  TMeasureType = (mtFix, mtPercent, mtFull);

  TLayoutManagerItemState = set of (isInitializing, isInternalUpdating, isRealigning);

  TLayoutManagerState = set of (lmLoadingItem, lmRealigningItems, lmResizingItems);

  TLayoutManagerItemList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TLayoutManagerItem;
    procedure SetItem(Index: Integer; AObject: TLayoutManagerItem);
  public
    function Add(AObject: TLayoutManagerItem): Integer;
    property Items[Index: Integer]: TLayoutManagerItem read GetItem write SetItem;
  end;

  TLayoutManagerPositions = class(TPersistent)
  private
    FBottom: Integer;
    FLeft: Integer;
    FRight: Integer;
    FTop: Integer;
    function GetBottom: Integer;
    function GetLeft: Integer;
    function GetRight: Integer;
    function GetTop: Integer;
    procedure SetBottom(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetTop(const Value: Integer);
  protected
    procedure Changed; virtual;
    procedure Clear; virtual;
    procedure Init; virtual; abstract;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    class function Equals(APostionsSource, APostionsDest: TLayoutManagerPositions): Boolean;    
  published
    property Bottom: Integer read GetBottom write SetBottom default 1;
    property Left: Integer read GetLeft write SetLeft default 1;
    property Right: Integer read GetRight write SetRight default 1;
    property Top: Integer read GetTop write SetTop default 1;
  end;

  TLayoutManagerPadding = class(TLayoutManagerPositions)
  private
    FContainer: TLayoutManager;
    function GetContainer: TLayoutManager;
    procedure SetContainer(const Value: TLayoutManager);
  protected
    procedure Changed; override;
    procedure Clear; override;
    procedure Init; override;
    property Container: TLayoutManager read GetContainer write SetContainer;
  end;

  TLayoutManagerItemPadding = class(TLayoutManagerPositions)
  private
    FItem: TLayoutManagerItem;
    function GetItem: TLayoutManagerItem;
    procedure SetItem(const Value: TLayoutManagerItem);
  protected
    procedure Changed; override;
    procedure Clear; override;
    procedure Init; override;
    property Item: TLayoutManagerItem read GetItem write SetItem;
  end;

  TLayoutManagerSpacing = class(TLayoutManagerPositions)
  private
    FContainer: TLayoutManager;
    function GetContainer: TLayoutManager;
    procedure SetContainer(const Value: TLayoutManager);
  protected
    procedure Changed; override;
    procedure Init; override;
    property Container: TLayoutManager read GetContainer write SetContainer;
  end;

  TLayoutManagerItemSizeOptions = class(TPersistent)
  private
    FIsSizeLoading: Boolean;
    FItem: TLayoutManagerItem;
    FMax: Extended;
    FMeasureType: TMeasureType;
    FMin: Extended;
    function GetIsSizeLoading: Boolean;
    function GetItem: TLayoutManagerItem;
    function GetIsReady: Boolean;
    function GetMax: Extended;
    function GetMeasureType: TMeasureType;
    function GetMin: Extended;
    function GetSize: Extended; virtual; abstract;
    procedure SetItem(const Value: TLayoutManagerItem);
    procedure SetMax(const Value: Extended);
    procedure SetMeasureType(const Value: TMeasureType);
    procedure SetMin(const Value: Extended);
    procedure SetSize(const Value: Extended); virtual; abstract;
  protected
    procedure AdjustMaxBounds;
    procedure AdjustMinBounds;
    function GetFixSize(APercentSize: Extended; AContainerSize: Integer): Integer;
    function GetPercentSize(AFixSize: Extended; AContainerSize: Integer): Extended;
    function GetValidSize(ANewSize: Extended): Extended;
    procedure RecalculateSize; virtual; abstract;
    property IsSizeLoading: Boolean read GetIsSizeLoading;
    property Item: TLayoutManagerItem read GetItem write SetItem;
    property IsReady: Boolean read GetIsReady;
  public
    property Max: Extended read GetMax write SetMax;
    property MeasureType: TMeasureType read GetMeasureType write SetMeasureType;
    property Min: Extended read GetMin write SetMin;
    property Size: Extended read GetSize write SetSize;
  end;

  TLayoutManagerItemHeightOptions = class(TLayoutManagerItemSizeOptions)
  private
    FSize: Extended;
    function GetSize: Extended; override;
    procedure SetSize(const Value: Extended); override;
  protected
    function GetAvaibleHeight: Integer;
    function GetConvertedSize(AFixHeight: Extended): Extended;
    function GetFixHeight: Integer; overload;
    function GetFixHeight(APercentHeight: Extended): Integer; overload;
    function GetPercentHeight: Extended; overload;
    function GetPercentHeight(AFixHeight: Extended): Extended; overload;
    procedure RecalculateSize; override;
  public
    constructor Create;
  published
    property Max;
    property MeasureType;
    property Min;
    property Size: Extended read GetSize write SetSize;
  end;

  TLayoutManagerItemWidthOptions = class(TLayoutManagerItemSizeOptions)
  private
    FSize: Extended;
    function GetSize: Extended; override;
    procedure SetSize(const Value: Extended); override;
  protected
    function GetAvaibleWidth: Integer;
    function GetConvertedSize(AFixWidth: Extended): Extended;
    function GetFixWidth: Integer; overload;
    function GetFixWidth(APercentWidth: Extended): Integer; overload;
    function GetPercentWidth: Extended; overload;
    function GetPercentWidth(AFixWidth: Extended): Extended; overload;
    procedure RecalculateSize; override;
  public
    constructor Create;
  published
    property Max;
    property MeasureType;
    property Min;
    property Size: Extended read GetSize write SetSize;
  end;

  TLayoutManagerItemCaptionOptions = class(TPersistent)
  private
    FAdditionalSpacing: Integer;
    FAlignmentVert: TTextLayout;
    FControlSpacing: Integer;
    FItem: TLayoutManagerItem;
    FPosition: TLabelPosition;
    FUseDefPosition: Boolean;
    FUseDefControlSpacing: Boolean;
    function GetAdditionalSpacing: Integer;
    function GetAlignmentHorz: TAlignment;
    function GetAlignmentVert: TTextLayout;
    function GetControlSpacing: Integer;
    function GetFont: TFont;
    function GetItem: TLayoutManagerItem;
    function GetPosition: TLabelPosition;
    function GetUseDefPosition: Boolean;
    function GetUseDefControlSpacing: Boolean;
    function GetWidth: Integer;
    function GetWordWrap: Boolean;
    procedure SetAdditionalSpacing(const Value: Integer);
    procedure SetAlignmentHorz(const Value: TAlignment);
    procedure SetAlignmentVert(const Value: TTextLayout);
    procedure SetControlSpacing(const Value: Integer);
    procedure SetFont(const Value: TFont);
    procedure SetItem(const Value: TLayoutManagerItem);
    procedure SetPosition(const Value: TLabelPosition);
    procedure SetUseDefPosition(const Value: Boolean);
    procedure SetUseDefControlSpacing(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
  protected
    function GetFullSpacing: Integer;
    property Item: TLayoutManagerItem read GetItem write SetItem;
    property AdditionalSpacing: Integer read GetAdditionalSpacing write SetAdditionalSpacing;
  published
    property AlignmentHorz: TAlignment read GetAlignmentHorz write SetAlignmentHorz default taLeftJustify;
    property AlignmentVert: TTextLayout read GetAlignmentVert write SetAlignmentVert default tlCenter;
    property ControlSpacing: Integer read GetControlSpacing write SetControlSpacing default 3;
    property Font: TFont read GetFont write SetFont;
    property Position: TLabelPosition read GetPosition write SetPosition default lpLeft;
    property UseDefPosition: Boolean read GetUseDefPosition write SetUseDefPosition default True;
    property UseDefControlSpacing: Boolean read GetUseDefControlSpacing write SetUseDefControlSpacing default True;
    property Width: Integer read GetWidth write SetWidth;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False;
  end;

  TLayoutManagerItem = class(TWinControl)
  private
    FActualControlPos: TPoint;
    FActualControlSize: TPoint;
    FActualItemPosition: TPoint;
    FActualItemSize: TPoint;
    FAutoAlignCaptions: Boolean;
    FCanvas: TCanvas;
    FCaption: string;
    FCaptionOptions: TLayoutManagerItemCaptionOptions;
    FCaptionVisible: boolean;
    FContainer: TLayoutManager;
    FHasPositioningControlMsgPending: Boolean;
    FHasRealignItemMsgPending: Boolean;
    FHasResizeControlMsgPending: Boolean;
    FHasResizeItemMsgPending: Boolean;
    FHeightOptions: TLayoutManagerItemHeightOptions;
    FItemControl: TControl;
    FItemLabel: TBoundLabel;
    FPadding: TLayoutManagerItemPadding;
    FState: TLayoutManagerItemState;
    FUseDefPadding: Boolean;
    FWidthOptions: TLayoutManagerItemWidthOptions;
    procedure CMBidimodechanged(var Message: TMessage);message CM_BIDIMODECHANGED;
    procedure CMPositioningControl(var Message: TMessage); message CM_POSITIONINGCONTROL;
    procedure CMRealignItem(var Message: TMessage); message CM_REALIGNITEM;
    procedure CMResizeControl(var Message: TMessage); message CM_RESIZECONTROL;
    procedure CMResizeItem(var Message: TMessage); message CM_RESIZEITEM;
    function GetAutoAlignCaptions: Boolean;
    function GetCaption: string;
    function GetCaptionOptions: TLayoutManagerItemCaptionOptions;
    function GetCaptionVisible: boolean;
    function GetContainer: TLayoutManager;
    function GetHeightOptions: TLayoutManagerItemHeightOptions;
    function GetIsReady: Boolean;
    function GetItemControl: TControl;
    function GetItemLabel: TBoundLabel;
    function GetPadding: TLayoutManagerItemPadding;
    function GetState: TLayoutManagerItemState;
    function GetUseDefPadding: Boolean;
    function GetWidthOptions: TLayoutManagerItemWidthOptions;
    function HasControlPosChanged: Boolean;
    function HasControlSizeChanged: Boolean;
    function HasItemPositionChanged: Boolean;
    function HasItemSizeChanged: Boolean;
    procedure PositioningControl;
    procedure PositioningLabel;
    procedure Realign(ANeedResize: Boolean);
    procedure SetAutoAlignCaptions(const Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetCaptionOptions(const Value: TLayoutManagerItemCaptionOptions);
    procedure SetCaptionVisible(const Value: boolean);
    procedure SetContainer(const Value: TLayoutManager);
    procedure SetHeightOptions(const Value: TLayoutManagerItemHeightOptions);
    procedure SetItemControl(const Value: TControl);
    procedure SetItemLabel(const Value: TBoundLabel);
    procedure SetPadding(const Value: TLayoutManagerItemPadding);
    procedure SetState(const Value: TLayoutManagerItemState);
    procedure SetUseDefPadding(const Value: Boolean);
    procedure SetWidthOptions(const Value: TLayoutManagerItemWidthOptions);
    procedure UpdateActualControlPos;
    procedure UpdateActualControlSize;
    procedure UpdateActualItemPosition;
    procedure UpdateActualItemSize;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateWnd; override;
    function GetFullHeight: Integer;
    function GetFullWidth: Integer;
    function GetInternalHeight: Integer;
    function GetInternalWidth: Integer;
    procedure Initialize;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint;
    procedure PaintWindow(DC: HDC); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure WndProc(var Message: TMessage); override;
    property Canvas: TCanvas read FCanvas;
    property Container: TLayoutManager read GetContainer write SetContainer;
    function IsChildLayoutManager: Boolean;
    property IsReady: Boolean read GetIsReady;
    property ItemLabel: TBoundLabel read GetItemLabel write SetItemLabel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RequestPositioningControl;
    procedure RequestRealignItem;
    procedure RequestResizeControl;
    procedure RequestResizeItem;
    procedure ResizeControlHeight;
    procedure ResizeControlWidth;
    procedure ResizeItemHeight;
    procedure ResizeItemWidth;
    property State: TLayoutManagerItemState read GetState write SetState;
  published
    property AutoAlignCaptions: Boolean read GetAutoAlignCaptions write SetAutoAlignCaptions default True;
    property Caption: string read GetCaption write SetCaption;
    property CaptionOptions: TLayoutManagerItemCaptionOptions read GetCaptionOptions write SetCaptionOptions;
    property CaptionVisible: Boolean read GetCaptionVisible write SetCaptionVisible;
    property Enabled;
    property HeightOptions: TLayoutManagerItemHeightOptions read GetHeightOptions write SetHeightOptions;
    property ItemControl: TControl read GetItemControl write SetItemControl;
    property Padding: TLayoutManagerItemPadding read GetPadding write SetPadding;
    property UseDefPadding: Boolean read GetUseDefPadding write SetUseDefPadding default True;
    property WidthOptions: TLayoutManagerItemWidthOptions read GetWidthOptions write SetWidthOptions;
  end;

  TLayoutManager = class(TScrollingWinControl)
  private
    FActualSize: TPoint;
    FAutoWrap: Boolean;
    FItemList: TLayoutManagerItemList;
    FHasRealignItemsMsgPending: Boolean;
    FHasResizeItemsMsgPending: Boolean;
    FItemDefCaptionPos: TLabelPosition;
    FItemDefControlSpacing: Integer;
    FItemDefPadding: TLayoutManagerPadding;
    FItemLayout: TLayoutOrientation;
    FItemSpacing: TLayoutManagerSpacing;
    FState: TLayoutManagerState;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMFreeItem(var Message: TMessage); message CM_FREEITEM;
    procedure CMRealignItems(var Message: TMessage); message CM_LMREALIGNITEMS;
    procedure CMResizeItems(var Message: TMessage); message CM_LMRESIZEITEMS;
    function GetAlignMode: TAlign;
    function GetAutoWrap: Boolean;
    function GetControlItem(AControl: TControl): TLayoutManagerItem;
    function GetIsReady: Boolean;
    function GetItemDefCaptionPos: TLabelPosition;
    function GetItemDefControlSpacing: Integer;
    function GetItemDefPadding: TLayoutManagerPadding;
    function GetItemLayout: TLayoutOrientation;
    function GetItemSpacing: TLayoutManagerSpacing;
    function GetState: TLayoutManagerState;
    function HasSizeChanged: Boolean;
    function IsChildLayoutManager: Boolean;
    procedure PositioningItems(AUseAutoWrap: Boolean; AAvailableSize: Integer = 0);
    procedure RecalculateCaptionSpacings;
    procedure SetAlignMode(const Value: TAlign);
    procedure SetAutoWrap(Value: Boolean);
    procedure SetItemDefCaptionPos(const Value: TLabelPosition);
    procedure SetItemDefControlSpacing(const Value: Integer);
    procedure SetItemDefPadding(const Value: TLayoutManagerPadding);
    procedure SetItemLayout(const Value: TLayoutOrientation);
    procedure SetItemSpacing(const Value: TLayoutManagerSpacing);
    procedure SetState(const Value: TLayoutManagerState);
    procedure UpdateActualSize;
    procedure WMNCHitTest(var Message: TMessage); message WM_NCHITTEST;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure InitializeItem(AItem: TLayoutManagerItem; AControl: TControl);
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateScrollBars;
    procedure WndProc(var Message: TMessage); override;
    property IsReady: Boolean read GetIsReady;
    property State: TLayoutManagerState read GetState write SetState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddControl(AControl: TControl): TLayoutManagerItem;
    procedure CreateWnd; override;
    function GetItemIndexByControlName(AControlName: string): Integer;
    function GetItemsOutOfContainersHeight(AItems: TObjectList = nil): Boolean;
    function GetItemsOutOfContainersWidth(AItems: TObjectList = nil): Boolean;
    function GetScreenHeightRequired: Integer;
    function GetScreenWidthRequired: Integer;
    procedure RealignItems;
    procedure ResizeItems;
    procedure RequestRealignItems;
    procedure RequestResizeItems;
    property ItemList: TLayoutManagerItemList read FItemList write FItemList;
  published
    property AlignMode: TAlign read GetAlignMode write SetAlignMode;
    property AutoSize;
    property AutoWrap: Boolean read GetAutoWrap write SetAutoWrap default True;
    property ItemDefCaptionPos: TLabelPosition read GetItemDefCaptionPos write SetItemDefCaptionPos default lpLeft;
    property ItemDefControlSpacing: Integer read GetItemDefControlSpacing write SetItemDefControlSpacing default 3;
    property ItemDefPadding: TLayoutManagerPadding read GetItemDefPadding write SetItemDefPadding;
    property ItemLayout: TLayoutOrientation read GetItemLayout write SetItemLayout default laHorizontal;
    property ItemSpacing: TLayoutManagerSpacing read GetItemSpacing write SetItemSpacing;
  end;

function RoundLess(AValue: Extended): Integer;

implementation

function RoundLess(AValue: Extended): Integer;
begin
  if Frac(AValue) <= 0.5 then
    Result := Trunc(AValue)
  else
    Result := Round(AValue);
end;

{ TLayoutManagerItemList }

function TLayoutManagerItemList.Add(AObject: TLayoutManagerItem): Integer;
begin
  Result := inherited Add(AObject);
end;

function TLayoutManagerItemList.GetItem(Index: Integer): TLayoutManagerItem;
begin
  Result := inherited GetItem(Index) as TLayoutManagerItem;
end;

procedure TLayoutManagerItemList.SetItem(Index: Integer; AObject: TLayoutManagerItem);
begin
  inherited SetItem(Index, AObject);
end;

{ TLayoutManagerPositions }

procedure TLayoutManagerPositions.Assign(Source: TPersistent);
var
  bChangeNeeded: Boolean;
begin
  //Don't call inherited on this method

  if not Source.InheritsFrom(TLayoutManagerPositions) then
    raise Exception.Create('Invalid class type');

  bChangeNeeded := False;

  if FBottom <> (Source as TLayoutManagerPositions).Bottom then
  begin
    FBottom := (Source as TLayoutManagerPositions).Bottom;
    bChangeNeeded := True;
  end;

  if FLeft <> (Source as TLayoutManagerPositions).Left then
  begin
    FLeft := (Source as TLayoutManagerPositions).Left;
    bChangeNeeded := True;
  end;

  if FRight <> (Source as TLayoutManagerPositions).Right then
  begin
    FRight := (Source as TLayoutManagerPositions).Right;
    bChangeNeeded := True;
  end;

  if FTop <> (Source as TLayoutManagerPositions).FTop then
  begin
    FTop := (Source as TLayoutManagerPositions).FTop;
    bChangeNeeded := True;
  end;

  if bChangeNeeded then
    Changed;
end;

procedure TLayoutManagerPositions.Changed;
begin
  //Do nothing here
end;

procedure TLayoutManagerPositions.Clear;
begin
  FBottom := 0;
  FLeft := 0;
  FRight := 0;
  FTop := 0;
end;

constructor TLayoutManagerPositions.Create;
begin
  Init;
end;

class function TLayoutManagerPositions.Equals(APostionsSource, APostionsDest: TLayoutManagerPositions): Boolean;
begin
  Result := (APostionsSource.Bottom = APostionsDest.Bottom) and
    (APostionsSource.Left = APostionsDest.Left) and
    (APostionsSource.Right = APostionsDest.Right) and
    (APostionsSource.Top = APostionsDest.Top);
end;

function TLayoutManagerPositions.GetBottom: Integer;
begin
  Result := FBottom;
end;

function TLayoutManagerPositions.GetLeft: Integer;
begin
  Result := FLeft;
end;

function TLayoutManagerPositions.GetRight: Integer;
begin
  Result := FRight;
end;

function TLayoutManagerPositions.GetTop: Integer;
begin
  Result := FTop;
end;

procedure TLayoutManagerPositions.SetBottom(const Value: Integer);
begin
  if (FBottom <> Value) and (Value >= 0) then
  begin
    FBottom := Value;
    Changed;
  end;
end;

procedure TLayoutManagerPositions.SetLeft(const Value: Integer);
begin
  if (FLeft <> Value) and (Value >= 0) then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TLayoutManagerPositions.SetRight(const Value: Integer);
begin
  if (FRight <> Value) and (Value >= 0) then
  begin
    FRight := Value;
    Changed;
  end;
end;

procedure TLayoutManagerPositions.SetTop(const Value: Integer);
begin
  if (FTop <> Value) and (Value >= 0) then
  begin
    FTop := Value;
    Changed;
  end;
end;

{ TLayoutManagerPadding }

procedure TLayoutManagerPadding.Changed;
var
  I: Integer;
  lItem: TLayoutManagerItem;
begin
  inherited;

  if not Assigned(Container) then
    Exit;

  for I := 0 to Container.ItemList.Count - 1 do
  begin
    lItem := Container.ItemList.Items[I];

    if lItem.ItemControl is TLayoutManager then
      (lItem.ItemControl as TLayoutManager).ItemDefPadding.Assign(Self)
    else if lItem.UseDefPadding then
      lItem.Padding.Assign(Self);
  end;
end;

procedure TLayoutManagerPadding.Clear;
begin
  FBottom := 1;
  FLeft := 1;
  FRight := 1;
  FTop := 1;
end;

function TLayoutManagerPadding.GetContainer: TLayoutManager;
begin
  Result := FContainer;
end;

procedure TLayoutManagerPadding.Init;
begin
  FBottom := 5;
  FLeft := 5;
  FRight := 5;
  FTop := 5;
end;

procedure TLayoutManagerPadding.SetContainer(const Value: TLayoutManager);
begin
  if FContainer <> Value then
    FContainer := Value;
end;

{ TLayoutManagerItemPadding }

procedure TLayoutManagerItemPadding.Changed;
begin
  inherited;

  if Item.IsReady then
  begin
    //if ItemControl is a TLayoutManager, ItemDefaultPadding will be set to it
    if Item.IsChildLayoutManager then
    begin
      Item.Padding.Clear;
      Item.UseDefPadding := False;
    end
    else
      Item.UseDefPadding := Equals(Self, Item.Container.ItemDefPadding);

    Item.Realign(True);
  end;
end;

procedure TLayoutManagerItemPadding.Clear;
begin
  FBottom := 1;
  FLeft := 1;
  FRight := 1;
  FTop := 1;
end;

function TLayoutManagerItemPadding.GetItem: TLayoutManagerItem;
begin
  Result := FItem;
end;

procedure TLayoutManagerItemPadding.Init;
begin
  FBottom := 5;
  FLeft := 5;
  FRight := 5;
  FTop := 5;
end;

procedure TLayoutManagerItemPadding.SetItem(const Value: TLayoutManagerItem);
begin
  if FItem <> Value then
    FItem := Value;
end;

{ TLayoutManagerSpacing }

procedure TLayoutManagerSpacing.Changed;
begin
  inherited;

  if not Assigned(Container) then
    Exit;

  Container.RequestResizeItems;
end;

function TLayoutManagerSpacing.GetContainer: TLayoutManager;
begin
  Result := FContainer;
end;

procedure TLayoutManagerSpacing.Init;
begin
  FBottom := 5;
  FLeft := 5;
  FRight := 5;
  FTop := 5;
end;

procedure TLayoutManagerSpacing.SetContainer(const Value: TLayoutManager);
begin
  if FContainer <> Value then
    FContainer := Value;
end;

{ TLayoutManagerItemSizeOptions }

procedure TLayoutManagerItemSizeOptions.AdjustMaxBounds;
begin
  if (MeasureType = mtPercent) and (Max > 100) then
    FMax := 100
  else if ((Max > 0) and (Min > 0)) and (Max < Min) then
    FMax := Min;
end;

procedure TLayoutManagerItemSizeOptions.AdjustMinBounds;
begin
  if (MeasureType = mtPercent) and (Min > 100) then
    FMin := 100
  else if ((Min > 0) and (Max > 0)) and (Min > Max) then
    FMin := Max;
end;

function TLayoutManagerItemSizeOptions.GetFixSize(APercentSize: Extended;
  AContainerSize: Integer): Integer;
begin
  Result := RoundLess(APercentSize);

  if (IsReady) and (MeasureType = mtPercent) then
    Result := RoundLess((APercentSize / 100) * AContainerSize);
end;

function TLayoutManagerItemSizeOptions.GetIsSizeLoading: Boolean;
begin
  Result := FIsSizeLoading;
end;

function TLayoutManagerItemSizeOptions.GetIsReady: Boolean;
begin
  Result := (Assigned(Item)) and (Assigned(Item.Container));
end;

function TLayoutManagerItemSizeOptions.GetItem: TLayoutManagerItem;
begin
  Result := FItem;
end;

function TLayoutManagerItemSizeOptions.GetMax: Extended;
begin
  Result := FMax;
end;

function TLayoutManagerItemSizeOptions.GetMeasureType: TMeasureType;
begin
  Result := FMeasureType;
end;

function TLayoutManagerItemSizeOptions.GetMin: Extended;
begin
  Result := FMin;
end;

function TLayoutManagerItemSizeOptions.GetPercentSize(AFixSize: Extended;
  AContainerSize: Integer): Extended;
begin
  Result := AFixSize;

  if (IsReady) and (MeasureType = mtPercent) then
    Result := (AFixSize / AContainerSize) * 100;
end;

function TLayoutManagerItemSizeOptions.GetValidSize(
  ANewSize: Extended): Extended;
begin
  if (MeasureType = mtPercent) and (ANewSize > 100) then //mtPercent bound
    Result := 100
  else if (Max > 0) and (ANewSize > Max) then //Max bound
    Result := Max
  else if (Min > 0) and (ANewSize < Min) then //Min bound
    Result := Min
  else
    Result := ANewSize;
end;

procedure TLayoutManagerItemSizeOptions.SetItem(const Value: TLayoutManagerItem);
begin
  FItem := Value;
end;

procedure TLayoutManagerItemSizeOptions.SetMax(const Value: Extended);
begin
  if (FMax <> Value) and (Value >= 0) then
  begin
    FMax := Value;

    AdjustMaxBounds;

    if IsReady then
    begin
      Size := GetValidSize(Size);
      RecalculateSize;
    end;
  end;
end;

procedure TLayoutManagerItemSizeOptions.SetMeasureType(const Value: TMeasureType);
begin
  if FMeasureType <> Value then
  begin
    FMeasureType := Value;

    if IsReady then
    begin
      AdjustMinBounds;
      AdjustMaxBounds;

      Size := GetValidSize(Size);

      RecalculateSize;
    end;
  end;
end;

procedure TLayoutManagerItemSizeOptions.SetMin(const Value: Extended);
begin
  if (FMin <> Value) and (Value >= 0) then
  begin
    FMin := Value;

    AdjustMinBounds;

    if IsReady then
    begin
      Size := GetValidSize(Size);
      RecalculateSize;
    end;
  end;
end;

{ TLayoutManagerItemHeightOptions }

constructor TLayoutManagerItemHeightOptions.Create;
begin
  FMin := 0;
  FMax := 0;
end;

function TLayoutManagerItemHeightOptions.GetAvaibleHeight: Integer;
var
  I: Integer;
  lItem: TLayoutManagerItem;
begin
  //TODO - Implementar e testar mtFull
  //Aqui deve ser considerado o ItemSpacing também

  Result := Item.Container.Height;

  for I := 0 to Item.Container.ItemList.Count - 1 do
    if Item.Container.ItemList.Items[I] is TLayoutManagerItem then
    begin
      lItem := Item.Container.ItemList.Items[I];

      if (lItem <> Item) and (((lItem.Left + lItem.Width) > Item.Left) and
        (lItem.Left < (Item.Left + Item.Width))) then
        Result := Result - lItem.Height;
    end;
end;

function TLayoutManagerItemHeightOptions.GetConvertedSize(AFixHeight: Extended): Extended;
begin
  if MeasureType = mtPercent then
    Result := GetPercentHeight(AFixHeight)
  else
    Result := AFixHeight;
end;

function TLayoutManagerItemHeightOptions.GetFixHeight: Integer;
begin
  Result := GetFixHeight(Size);
end;

function TLayoutManagerItemHeightOptions.GetFixHeight(APercentHeight: Extended): Integer;
begin
  Result := RoundLess(APercentHeight);

  if IsReady then
  begin
    case MeasureType of
      mtFix: Result := GetFixSize(APercentHeight, Item.Container.Height);
      mtFull: Result := GetAvaibleHeight;
      mtPercent: Result := GetFixSize(APercentHeight, Item.Container.Height) -
        (Item.Container.ItemSpacing.Top + Item.Container.ItemSpacing.Bottom);
    end;
  end;
end;

function TLayoutManagerItemHeightOptions.GetPercentHeight: Extended;
begin
  Result := GetPercentHeight(Size);
end;

function TLayoutManagerItemHeightOptions.GetPercentHeight(AFixHeight: Extended): Extended;
begin
  Result := AFixHeight;

  if IsReady then
    Result := GetPercentSize(AFixHeight, Item.Container.Height);
end;

function TLayoutManagerItemHeightOptions.GetSize: Extended;
begin
  Result := FSize;
end;

procedure TLayoutManagerItemHeightOptions.RecalculateSize;
begin
  if IsReady then
  begin
    FIsSizeLoading := True;

    Item.Height := GetFixHeight;
    Item.UpdateActualItemSize;

    FIsSizeLoading := False;

    Item.ResizeControlHeight;
    Item.PositioningLabel;

    if (not (isInitializing in Item.State)) and (not (isRealigning in Item.State)) then
      Item.Container.RequestRealignItems;
  end;
end;

procedure TLayoutManagerItemHeightOptions.SetSize(const Value: Extended);
begin
  if (Size <> Value) and (Value >= 0) then
  begin
    FSize := GetValidSize(Value);

    RecalculateSize;
  end;
end;

{ TLayoutManagerItemWidthOptions }

constructor TLayoutManagerItemWidthOptions.Create;
begin
  FMin := 0;
  FMax := 0;
end;

function TLayoutManagerItemWidthOptions.GetAvaibleWidth: Integer;
var
  I: Integer;
  lItem: TLayoutManagerItem;
begin
  //TODO - Implementar e testar mtFull
  //Aqui deve ser considerado o ItemSpacing também

  Result := Item.Container.Width;

  for I := 0 to Item.Container.ItemList.Count - 1 do
    if Item.Container.ItemList.Items[I] is TLayoutManagerItem then
    begin
      lItem := Item.Container.ItemList.Items[I];

      if (lItem <> Item) and (((lItem.Top + lItem.Height) > Item.Top) and
        (lItem.Top < (Item.Top + Item.Height))) then
        Result := Result - lItem.Width;
    end;
end;

function TLayoutManagerItemWidthOptions.GetConvertedSize(
  AFixWidth: Extended): Extended;
begin
  if MeasureType = mtPercent then
    Result := GetPercentWidth(AFixWidth)
  else
    Result := AFixWidth;
end;

function TLayoutManagerItemWidthOptions.GetFixWidth: Integer;
begin
  Result := GetFixWidth(Size);
end;

function TLayoutManagerItemWidthOptions.GetFixWidth(APercentWidth: Extended): Integer;
begin
  Result := RoundLess(APercentWidth);

  if IsReady then
  begin
    case MeasureType of
      mtFix: Result := GetFixSize(APercentWidth, Item.Container.Width);
      mtFull: Result := GetAvaibleWidth;
      mtPercent: Result := GetFixSize(APercentWidth, Item.Container.Width) -
        (Item.Container.ItemSpacing.Left + Item.Container.ItemSpacing.Right);
    end;
  end;
end;

function TLayoutManagerItemWidthOptions.GetPercentWidth: Extended;
begin
  Result := GetPercentWidth(Size);
end;

function TLayoutManagerItemWidthOptions.GetPercentWidth(AFixWidth: Extended): Extended;
begin
  Result := AFixWidth;

  if IsReady then
    Result := GetPercentSize(AFixWidth, Item.Container.Width);
end;

function TLayoutManagerItemWidthOptions.GetSize: Extended;
begin
  Result := FSize;
end;

procedure TLayoutManagerItemWidthOptions.RecalculateSize;
begin
  if IsReady then
  begin
    FIsSizeLoading := True;

    Item.Width := GetFixWidth;
    Item.UpdateActualItemSize;

    FIsSizeLoading := False;

    Item.ResizeControlWidth;
    Item.PositioningLabel;

    if (not (isInitializing in Item.State)) and (not (isRealigning in Item.State)) then
      Item.Container.RequestRealignItems;
  end;
end;

procedure TLayoutManagerItemWidthOptions.SetSize(const Value: Extended);
begin
  if (Size <> Value) and (Value >= 0) then
  begin
    FSize := GetValidSize(Value);

    RecalculateSize;
  end;
end;

{ TLayoutManagerItemCaptionOptions }

function TLayoutManagerItemCaptionOptions.GetAdditionalSpacing: Integer;
begin
  Result := FAdditionalSpacing;
end;

function TLayoutManagerItemCaptionOptions.GetAlignmentHorz: TAlignment;
begin
  Result:= THackBoundLabel(Item.ItemLabel).Alignment;
end;

function TLayoutManagerItemCaptionOptions.GetAlignmentVert: TTextLayout;
begin
  Result := FAlignmentVert;
end;

function TLayoutManagerItemCaptionOptions.GetControlSpacing: Integer;
begin
  Result := FControlSpacing;
end;

function TLayoutManagerItemCaptionOptions.GetFont: TFont;
begin
  Result:= Item.ItemLabel.Font;
end;

function TLayoutManagerItemCaptionOptions.GetFullSpacing: Integer;
begin
  Result := ControlSpacing + AdditionalSpacing;
end;

function TLayoutManagerItemCaptionOptions.GetItem: TLayoutManagerItem;
begin
  Result := FItem;
end;

function TLayoutManagerItemCaptionOptions.GetPosition: TLabelPosition;
begin
  Result := FPosition;
end;

function TLayoutManagerItemCaptionOptions.GetUseDefPosition: Boolean;
begin
  Result := FUseDefPosition;
end;

function TLayoutManagerItemCaptionOptions.GetUseDefControlSpacing: Boolean;
begin
  Result := FUseDefControlSpacing;
end;

function TLayoutManagerItemCaptionOptions.GetWidth: Integer;
begin
  Result:= Item.ItemLabel.Width;
end;

function TLayoutManagerItemCaptionOptions.GetWordWrap: Boolean;
begin
  Result:= Item.ItemLabel.WordWrap;
end;

procedure TLayoutManagerItemCaptionOptions.SetAdditionalSpacing(const Value: Integer);
begin
  if FAdditionalSpacing <> Value then
  begin
    if Value < 0 then
      FAdditionalSpacing := 0
    else
      FAdditionalSpacing := Value;
      
    Item.Realign(True);
  end;
end;

procedure TLayoutManagerItemCaptionOptions.SetAlignmentHorz(const Value: TAlignment);
begin
  if THackBoundLabel(Item.ItemLabel).Alignment <> Value then
  begin
    THackBoundLabel(Item.ItemLabel).Alignment:= Value;
    Item.PositioningLabel;
  end;
end;

procedure TLayoutManagerItemCaptionOptions.SetAlignmentVert(const Value: TTextLayout);
begin
  if FAlignmentVert <> Value then
  begin
    FAlignmentVert:= Value;
    Item.PositioningLabel;
  end;
end;

procedure TLayoutManagerItemCaptionOptions.SetControlSpacing(const Value: Integer);
begin
  if (FControlSpacing <> Value) and (Value >= 0) then
  begin
    FControlSpacing := Value;

    if Item.IsReady then
      UseDefControlSpacing := FControlSpacing = Item.Container.ItemDefControlSpacing;

    Item.Realign(True);
  end;
end;

procedure TLayoutManagerItemCaptionOptions.SetFont(const Value: TFont);
begin
  if Item.ItemLabel.Font <> Value then
  begin
    Item.ItemLabel.Font:= Value;
    Item.Realign(True);
  end;
end;

procedure TLayoutManagerItemCaptionOptions.SetItem(const Value: TLayoutManagerItem);
begin
  if FItem <> Value then
  begin
    FItem := Value;

    FAlignmentVert:= tlCenter;
    FControlSpacing:= 3;
    FPosition:= lpLeft;
    FUseDefPosition := True;
    FUseDefControlSpacing := True;
  end;
end;

procedure TLayoutManagerItemCaptionOptions.SetPosition(const Value: TLabelPosition);
begin
  if FPosition <> Value then
  begin
    FPosition:= Value;

    if Item.IsReady then
      UseDefPosition := FPosition = Item.Container.ItemDefCaptionPos;

    Item.Realign(True);
  end;
end;

procedure TLayoutManagerItemCaptionOptions.SetUseDefPosition(const Value: Boolean);
begin
  if FUseDefPosition <> Value then
  begin
    FUseDefPosition := Value;

    if (FUseDefPosition) and (Position <> Item.Container.ItemDefCaptionPos) then
      Position := Item.Container.ItemDefCaptionPos;
  end;
end;

procedure TLayoutManagerItemCaptionOptions.SetUseDefControlSpacing(const Value: Boolean);
begin
  if FUseDefControlSpacing <> Value then
  begin
    FUseDefControlSpacing := Value;

    if (FUseDefControlSpacing) and (ControlSpacing <> Item.Container.ItemDefControlSpacing) then
      ControlSpacing := Item.Container.ItemDefControlSpacing;
  end;
end;

procedure TLayoutManagerItemCaptionOptions.SetWidth(const Value: Integer);
begin
  if Item.ItemLabel.Width <> Value then
  begin
    Item.ItemLabel.Width:= Value;
    THackBoundLabel(Item.ItemLabel).AdjustBounds;
    Item.Realign(True);
  end;
end;

procedure TLayoutManagerItemCaptionOptions.SetWordWrap(const Value: Boolean);
begin
  if Item.ItemLabel.WordWrap <> Value then
  begin
    Item.ItemLabel.WordWrap:= Value;
    THackBoundLabel(Item.ItemLabel).AdjustBounds;
    Item.Realign(True);
  end;
end;

{ TLayoutManagerItem }

constructor TLayoutManagerItem.Create(AOwner: TComponent);
begin
  State := [];

  FHasPositioningControlMsgPending := False;
  FHasRealignItemMsgPending := False;
  FHasResizeControlMsgPending := False;
  FHasResizeItemMsgPending := False;

  inherited;

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  FItemLabel := TBoundLabel.Create(Self);
  FItemLabel.Name:= Name + 'Label';
  FItemLabel.FreeNotification(Self);
  FItemLabel.Parent := Self;
  FItemLabel.Visible := False;

  FCaptionOptions := TLayoutManagerItemCaptionOptions.Create;
  FCaptionOptions.Item := Self;

  FHeightOptions := TLayoutManagerItemHeightOptions.Create;
  FHeightOptions.Item := Self;

  FWidthOptions := TLayoutManagerItemWidthOptions.Create;
  FWidthOptions.Item := Self;

  FPadding := TLayoutManagerItemPadding.Create;
  FPadding.Item := Self;

  FAutoAlignCaptions := True;
  FUseDefPadding := True;
end;

procedure TLayoutManagerItem.CreateWnd;
begin
  inherited;

  if Assigned(ItemControl) then
    ItemControl.Enabled := Enabled;

  if Assigned(ItemLabel) then
    ItemLabel.Enabled := Enabled;
end;

destructor TLayoutManagerItem.Destroy;
begin
  FreeAndNil(FCanvas);
  FreeAndNil(FItemLabel);
  FreeAndNil(FCaptionOptions);
  FreeAndNil(FHeightOptions);
  FreeAndNil(FWidthOptions);
  FreeAndNil(FPadding);

  inherited;
end;

procedure TLayoutManagerItem.CMBidimodechanged(var Message: TMessage);
begin
  if Assigned(ItemLabel) then
    ItemLabel.BiDiMode := BiDiMode;
end;

procedure TLayoutManagerItem.CMPositioningControl(var Message: TMessage);
begin
  FHasPositioningControlMsgPending := False;

  PositioningControl;
end;

procedure TLayoutManagerItem.CMRealignItem(var Message: TMessage);
begin
  FHasRealignItemMsgPending := False;

  Realign(False);
end;

procedure TLayoutManagerItem.CMResizeControl(var Message: TMessage);
begin
  FHasResizeControlMsgPending := False;

  //Resizes Item with the new control size
  ResizeItemHeight;
  ResizeItemWidth;

  //Repositioning control and label
  Realign(False);
end;

procedure TLayoutManagerItem.CMResizeItem(var Message: TMessage);
begin
  FHasResizeItemMsgPending := False;

  if Height <> FActualItemSize.Y then
  begin
    if HeightOptions.MeasureType = mtPercent then
      HeightOptions.Size := HeightOptions.GetConvertedSize(GetFullHeight)
    else
      HeightOptions.Size := HeightOptions.GetConvertedSize(Height);
  end;

  if Width <> FActualItemSize.X then
  begin
    if WidthOptions.MeasureType = mtPercent then
      WidthOptions.Size := WidthOptions.GetConvertedSize(GetFullWidth)
    else
      WidthOptions.Size := WidthOptions.GetConvertedSize(Width);
  end;

  UpdateActualItemSize;

  Realign(False);

  Container.UpdateScrollBars;
end;

function TLayoutManagerItem.GetAutoAlignCaptions: Boolean;
begin
  Result := FAutoAlignCaptions;
end;

function TLayoutManagerItem.GetCaption: string;
begin
  Result := FCaption;
end;

function TLayoutManagerItem.GetCaptionOptions: TLayoutManagerItemCaptionOptions;
begin
  Result := FCaptionOptions;
end;

function TLayoutManagerItem.GetCaptionVisible: boolean;
begin
  Result := FCaptionVisible;
end;

function TLayoutManagerItem.GetContainer: TLayoutManager;
begin
  Result := FContainer;
end;

function TLayoutManagerItem.GetFullHeight: Integer;
begin
  Result := Height;

  if IsReady then
    Result := Result + Container.ItemSpacing.Top + Container.ItemSpacing.Bottom;
end;

function TLayoutManagerItem.GetFullWidth: Integer;
begin
  Result := Width;

  if IsReady then
    Result := Result + Container.ItemSpacing.Left + Container.ItemSpacing.Right;
end;

function TLayoutManagerItem.GetHeightOptions: TLayoutManagerItemHeightOptions;
begin
  Result := FHeightOptions;
end;

function TLayoutManagerItem.GetInternalHeight: Integer;
begin
  Result := Height - Padding.Top - Padding.Bottom;

  if (Assigned(CaptionOptions)) and (CaptionVisible) and
    (CaptionOptions.Position in [lpAbove, lpBelow]) then
    Result := Result - CaptionOptions.GetFullSpacing;
end;

function TLayoutManagerItem.GetInternalWidth: Integer;
begin
  Result := Width - Padding.Left - Padding.Right;

  if (Assigned(CaptionOptions)) and (CaptionVisible) and
    (CaptionOptions.Position in [lpLeft, lpRight]) then
    Result := Result - CaptionOptions.GetFullSpacing;
end;

function TLayoutManagerItem.GetIsReady: Boolean;
begin
  Result := (not (csLoading in ComponentState)) and
    (not (csDestroying in ComponentState)) and
    (not (isInitializing in State)) and
    (Assigned(Container)) and (not (lmLoadingItem in Container.State)) and
    (Assigned(ItemLabel)) and (Assigned(ItemControl));
end;

function TLayoutManagerItem.GetItemControl: TControl;
begin
  Result := FItemControl;
end;

function TLayoutManagerItem.GetItemLabel: TBoundLabel;
begin
  Result := FItemLabel;
end;

function TLayoutManagerItem.GetPadding: TLayoutManagerItemPadding;
begin
  Result := FPadding;
end;

function TLayoutManagerItem.GetState: TLayoutManagerItemState;
begin
  Result := FState;
end;

function TLayoutManagerItem.GetUseDefPadding: Boolean;
begin
  Result := FUseDefPadding;
end;

function TLayoutManagerItem.GetWidthOptions: TLayoutManagerItemWidthOptions;
begin
  Result := FWidthOptions;
end;

function TLayoutManagerItem.HasControlPosChanged: Boolean;
begin
  Result := (FActualControlPos.X <> ItemControl.Left) or
    (FActualControlPos.Y <> ItemControl.Top);
end;

function TLayoutManagerItem.HasControlSizeChanged: Boolean;
begin
  Result := (FActualControlSize.X <> ItemControl.Width) or
    (FActualControlSize.Y <> ItemControl.Height);
end;

function TLayoutManagerItem.HasItemPositionChanged: Boolean;
begin
  Result := (FActualItemPosition.X <> Left) or (FActualItemPosition.Y <> Top);
end;

function TLayoutManagerItem.HasItemSizeChanged: Boolean;
begin
  Result := (FActualItemSize.Y <> Height) or (FActualItemSize.X <> Width);
end;

procedure TLayoutManagerItem.Initialize;
begin
  if not IsReady then
    Exit;

  Include(FState, isInitializing);

  HeightOptions.MeasureType := mtFix;
  HeightOptions.Size := ItemControl.Height;
  HeightOptions.Max := 0;
  HeightOptions.Min := 0;

  if Container.ItemLayout = laHorizontal then
  begin
    WidthOptions.MeasureType := mtFix;
    WidthOptions.Size := ItemControl.Width;
  end
  else
  begin
    WidthOptions.MeasureType := mtPercent;
    WidthOptions.Size := 100;
  end;

  if IsChildLayoutManager then
  begin
    Padding.Clear;
    (ItemControl as TLayoutManager).ItemDefCaptionPos := Container.ItemDefCaptionPos;
    (ItemControl as TLayoutManager).ItemDefControlSpacing := Container.ItemDefControlSpacing;
    (ItemControl as TLayoutManager).ItemDefPadding.Assign(Container.ItemDefPadding);
    UseDefPadding := False;
  end
  else
  begin
    CaptionOptions.Position := Container.ItemDefCaptionPos;
    CaptionOptions.ControlSpacing := Container.ItemDefControlSpacing;
    Padding.Assign(Container.ItemDefPadding);
    UseDefPadding := True;
  end;

  Exclude(FState, isInitializing);

  Realign(True);
end;

function TLayoutManagerItem.IsChildLayoutManager: Boolean;
begin
  if (Assigned(ItemControl)) and (ItemControl is TLayoutManager) then
    Result := True
  else
    Result := False;
end;

procedure TLayoutManagerItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FItemControl) and (Assigned(Container)) then
  begin
    if HandleAllocated then
      PostMessage(Container.Handle, CM_FREEITEM, WParam(Self), 0);

    Container.RequestResizeItems;
  end;
end;

procedure TLayoutManagerItem.Paint;
begin
  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;

    //Top Line
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width - 1, 0);

    //Left line
    Canvas.MoveTo(Width - 1, 0);
    Canvas.LineTo(Width - 1, Height - 1);

    //Bottom line
    Canvas.MoveTo(Width - 1, Height - 1);
    Canvas.LineTo(0, Height - 1);

    //Right line
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(0, Height - 1);
  end;
end;

procedure TLayoutManagerItem.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TLayoutManagerItem.PositioningControl;
begin
  if not IsReady then
    Exit;

  Include(FState, isInternalUpdating);

  //Default position
  ItemControl.Left := Padding.Left;
  ItemControl.Top := Padding.Top;

  if CaptionVisible then
  begin
    case CaptionOptions.Position of
      lpAbove:
        begin
          ItemControl.Left := Padding.Left;
          ItemControl.Top := ItemLabel.Height + CaptionOptions.GetFullSpacing + Padding.Top;
        end;
      lpLeft:
        begin
          ItemControl.Left := ItemLabel.Width + CaptionOptions.GetFullSpacing + Padding.Left;
          ItemControl.Top := Padding.Top;
        end;
    end;
  end;

  UpdateActualControlPos;

  Exclude(FState, isInternalUpdating);
end;

procedure TLayoutManagerItem.PositioningLabel;
var
  P: TPoint;


  function GetHorizontalPosition: Integer;
  begin
    Result := 0;

    case CaptionOptions.AlignmentHorz of
      taLeftJustify:
        begin
          case CaptionOptions.Position of
            lpAbove, lpBelow: Result := ItemControl.Left;
            lpLeft: Result := ItemControl.Left - ItemLabel.Width - CaptionOptions.GetFullSpacing;
            lpRight: Result := ItemControl.Left + ItemControl.Width + CaptionOptions.ControlSpacing;
          end;
        end;
      taRightJustify:
        begin
          case CaptionOptions.Position of
            lpAbove, lpBelow: Result := (ItemControl.Width + ItemControl.Left) - ItemLabel.Width;
            lpLeft: Result := ItemControl.Left - ItemLabel.Width - CaptionOptions.ControlSpacing;
            lpRight: Result := ItemControl.Left + ItemControl.Width + CaptionOptions.GetFullSpacing;
          end;
        end;
      taCenter:
        begin
          case CaptionOptions.Position of
            lpAbove, lpBelow: Result := ItemControl.Left + (ItemControl.Width - ItemLabel.Width) div 2;
            lpLeft: Result := (ItemControl.Left - ItemLabel.Width) div 2;
            lpRight: Result := (ItemControl.Left + ItemControl.Width) + ((CaptionOptions.GetFullSpacing + Padding.Right) div 2);
          end;
        end;
    end;
  end;


  function GetVerticalPosition: Integer;
  begin
    Result := 0;

    case CaptionOptions.AlignmentVert of
      tlTop:
        begin
          case CaptionOptions.Position of
            lpAbove: Result := ItemControl.Top - ItemLabel.Height - CaptionOptions.GetFullSpacing;
            lpBelow: Result := ItemControl.Top + ItemControl.Height + CaptionOptions.GetFullSpacing;
            lpLeft, lpRight: Result := ItemControl.Top;
          end;
        end;
      tlBottom:
        begin
          case CaptionOptions.Position of
            lpAbove: Result := ItemControl.Top - ItemLabel.Height - CaptionOptions.ControlSpacing;
            lpBelow: Result := ItemControl.Top + ItemControl.Height + CaptionOptions.ControlSpacing;
            lpLeft, lpRight: Result := (ItemControl.Top + ItemControl.Height) - ItemLabel.Height;
          end;
        end;
      tlCenter:
        begin
          case CaptionOptions.Position of
            lpAbove: Result := (ItemControl.Top - ItemLabel.Height) div 2;
            lpBelow: Result := (ItemControl.Top + ItemControl.Height) + ((CaptionOptions.GetFullSpacing + Padding.Bottom) div 2);
            lpLeft, lpRight: Result := ItemControl.Top + ((ItemControl.Height - ItemLabel.Height) div 2);
          end;
        end;
    end;
  end;


begin
  if (not IsReady) or (ItemLabel = nil) or (ItemControl = nil) then
    Exit;

  Include(FState, isInternalUpdating);

  if not ItemLabel.Visible then
    ItemLabel.SetBounds(-999, -999, ItemLabel.Width, ItemLabel.Height)
  else
    begin
      ItemLabel.SetBounds(1, 1, ItemLabel.Width, ItemLabel.Height);

      THackBoundLabel(ItemLabel).AdjustBounds;

      P := Point(GetHorizontalPosition, GetVerticalPosition);

      ItemLabel.SetBounds(P.x, P.y, ItemLabel.Width, ItemLabel.Height);
    end;

  Exclude(FState, isInternalUpdating);
end;

procedure TLayoutManagerItem.Realign(ANeedResize: Boolean);
begin
  if not IsReady then
    Exit;

  Include(FState, isRealigning);

  if ANeedResize then
  begin
    if HeightOptions.MeasureType = mtFix then
      ResizeItemHeight
    else
      ResizeControlHeight;

    if WidthOptions.MeasureType = mtFix then
      ResizeItemWidth
    else
      ResizeControlWidth;
  end;

  PositioningControl;
  PositioningLabel;

  Exclude(FState, isRealigning);

  if ANeedResize then
    Container.RequestRealignItems;
end;

procedure TLayoutManagerItem.RequestPositioningControl;
begin
  if (not FHasPositioningControlMsgPending) and (HandleAllocated) then
  begin
    FHasPositioningControlMsgPending := True;
    PostMessage(Handle, CM_POSITIONINGCONTROL, 0, 0);
  end;
end;

procedure TLayoutManagerItem.RequestRealignItem;
begin
  if (not FHasRealignItemMsgPending) and (HandleAllocated) then
  begin
    FHasRealignItemMsgPending := True;
    PostMessage(Handle, CM_REALIGNITEM, 0, 0);
  end;
end;

procedure TLayoutManagerItem.RequestResizeControl;
begin
  if (not FHasResizeControlMsgPending) and (HandleAllocated) then
  begin
    FHasResizeControlMsgPending := True;
    PostMessage(Handle, CM_RESIZECONTROL, 0, 0);
  end;
end;

procedure TLayoutManagerItem.RequestResizeItem;
begin
  if (not FHasResizeItemMsgPending) and (HandleAllocated) then
  begin
    FHasResizeItemMsgPending := True;
    PostMessage(Handle, CM_RESIZEITEM, 0, 0);
  end;
end;

procedure TLayoutManagerItem.ResizeControlHeight;
begin
  if not IsReady then
    Exit;

  Include(FState, isInternalUpdating);

  if (CaptionOptions.Position in [lpAbove, lpBelow]) and (CaptionVisible) then
    ItemControl.Height := GetInternalHeight - ItemLabel.Height
  else
    ItemControl.Height := GetInternalHeight;

  UpdateActualControlSize;

  Exclude(FState, isInternalUpdating);
end;

procedure TLayoutManagerItem.ResizeControlWidth;
begin
  if not IsReady then
    Exit;

  Include(FState, isInternalUpdating);

  if (CaptionOptions.Position in [lpLeft, lpRight]) and (CaptionVisible) then
    ItemControl.Width := GetInternalWidth - ItemLabel.Width
  else
    ItemControl.Width := GetInternalWidth;

  UpdateActualControlSize;

  Exclude(FState, isInternalUpdating);
end;

procedure TLayoutManagerItem.ResizeItemHeight;
var
  iOffSets: Integer;
begin
  if not IsReady then
    Exit;

  Include(FState, isInternalUpdating);

  if HeightOptions.MeasureType = mtPercent then
    iOffSets := Padding.Top + Padding.Bottom + Container.ItemSpacing.Top + Container.ItemSpacing.Bottom
  else
    iOffSets := Padding.Top + Padding.Bottom;

  if CaptionVisible then
  begin
    case CaptionOptions.Position of
      lpAbove, lpBelow: HeightOptions.Size := HeightOptions.GetConvertedSize(ItemControl.Height
        + ItemLabel.Height + CaptionOptions.GetFullSpacing + iOffSets);
      lpLeft, lpRight:
        begin
          if HeightOptions.MeasureType = mtPercent then
            HeightOptions.Size := HeightOptions.GetConvertedSize(ItemControl.Height + iOffSets)
          else
            HeightOptions.Size := HeightOptions.GetConvertedSize(Max(ItemControl.Height,
              ItemLabel.Height) + iOffSets);
        end;
    end;
  end
  else
    HeightOptions.Size := HeightOptions.GetConvertedSize(ItemControl.Height + iOffSets);

  UpdateActualItemSize;
  Container.UpdateScrollBars;

  Exclude(FState, isInternalUpdating);
end;

procedure TLayoutManagerItem.ResizeItemWidth;
var
  iOffSets: Integer;
begin
  if not IsReady then
    Exit;

  Include(FState, isInternalUpdating);

  if WidthOptions.MeasureType = mtPercent then
    iOffSets := Padding.Left + Padding.Right + Container.ItemSpacing.Left + Container.ItemSpacing.Right
  else
    iOffSets := Padding.Left + Padding.Right;

  if CaptionVisible then
  begin
    case CaptionOptions.Position of
      lpAbove, lpBelow:
        begin
          if WidthOptions.MeasureType = mtPercent then
            WidthOptions.Size := WidthOptions.GetConvertedSize(ItemControl.Width + iOffSets)
          else
            WidthOptions.Size := WidthOptions.GetConvertedSize(Max(ItemControl.Width,
              ItemLabel.Width) + iOffSets);
        end;
      lpLeft, lpRight: WidthOptions.Size := WidthOptions.GetConvertedSize(ItemControl.Width +
        ItemLabel.Width + CaptionOptions.GetFullSpacing + iOffSets);
    end;
  end
  else
    WidthOptions.Size := WidthOptions.GetConvertedSize(ItemControl.Width + iOffSets);

  UpdateActualItemSize;
  Container.UpdateScrollBars;

  Exclude(FState, isInternalUpdating);
end;

procedure TLayoutManagerItem.SetAutoAlignCaptions(const Value: Boolean);
begin
  if FAutoAlignCaptions <> Value then
  begin
    FAutoAlignCaptions := Value;
    Realign(True);
  end;
end;

procedure TLayoutManagerItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    ItemLabel.Caption := Value;
    Realign(True);
  end;
end;

procedure TLayoutManagerItem.SetCaptionOptions(const Value: TLayoutManagerItemCaptionOptions);
begin
  if FCaptionOptions <> Value then
    FCaptionOptions := Value;
end;

procedure TLayoutManagerItem.SetCaptionVisible(const Value: boolean);
begin
  if FCaptionVisible <> Value then
  begin
    FCaptionVisible := Value;
    ItemLabel.Visible := Value;
    Realign(True);
  end;
end;

procedure TLayoutManagerItem.SetContainer(const Value: TLayoutManager);
begin
  if FContainer <> Value then
    FContainer := Value;
end;

procedure TLayoutManagerItem.SetEnabled(Value: Boolean);
begin
  if Enabled <> Value then
  begin
    inherited;

    if IsReady then
    begin
      FItemControl.Enabled := Enabled;
      FItemLabel.Enabled := Enabled;
    end;
  end;
end;

procedure TLayoutManagerItem.SetHeightOptions(const Value: TLayoutManagerItemHeightOptions);
begin
  if FHeightOptions <> Value then
    FHeightOptions := Value;
end;

procedure TLayoutManagerItem.SetItemControl(const Value: TControl);
begin
  if FItemControl <> Value then
  begin
    FItemControl := Value;

    if Assigned(ItemControl) and (not (csLoading in ComponentState)) then
    begin
      CaptionVisible := (not IsPublishedProp(FItemControl, 'Caption')) and (not (FItemControl is TLayoutManager));

      if IsPublishedProp(FItemControl, 'AutoSize') then
        SetPropValue(ItemControl, 'AutoSize', False);
    end;
  end;
end;

procedure TLayoutManagerItem.SetItemLabel(const Value: TBoundLabel);
begin
  if FItemLabel <> Value then
    FItemLabel := Value;
end;

procedure TLayoutManagerItem.SetPadding(const Value: TLayoutManagerItemPadding);
begin
  if FPadding <> Value then
    FPadding.Assign(Value);
end;

procedure TLayoutManagerItem.SetState(const Value: TLayoutManagerItemState);
begin
  FState := Value;
end;

procedure TLayoutManagerItem.SetUseDefPadding(const Value: Boolean);
begin
  if FUseDefPadding <> Value then
  begin
    FUseDefPadding := Value;

    if (IsReady) and (FUseDefPadding) then
      Padding.Assign(Container.ItemDefPadding);
  end;
end;

procedure TLayoutManagerItem.SetWidthOptions(const Value: TLayoutManagerItemWidthOptions);
begin
  if FWidthOptions <> Value then
    FWidthOptions := Value;
end;

procedure TLayoutManagerItem.UpdateActualControlPos;
begin
  FActualControlPos.Y := ItemControl.Top;
  FActualControlPos.X := ItemControl.Left;
end;

procedure TLayoutManagerItem.UpdateActualControlSize;
begin
  FActualControlSize.Y := ItemControl.Height;
  FActualControlSize.X := ItemControl.Width;
end;

procedure TLayoutManagerItem.UpdateActualItemPosition;
begin
  FActualItemPosition.Y := Top;
  FActualItemPosition.X := Left;
end;

procedure TLayoutManagerItem.UpdateActualItemSize;
begin
  FActualItemSize.Y := Height;
  FActualItemSize.X := Width;
end;

procedure TLayoutManagerItem.WMPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

procedure TLayoutManagerItem.WndProc(var Message: TMessage);
begin
  inherited;

  if (not IsReady) or (isInternalUpdating in State) or (ItemControl = nil) or
    (HeightOptions.IsSizeLoading) or (WidthOptions.IsSizeLoading) or
    (lmRealigningItems in Container.State) then
    Exit;

  case Message.Msg of
    WM_WINDOWPOSCHANGING:
      begin
        if HasControlPosChanged then
          RequestPositioningControl;

        if HasControlSizeChanged then
          RequestResizeControl;

        if HasItemSizeChanged then
        begin
          RequestResizeItem;
          Container.RequestRealignItems;
        end;
      end;
    WM_WINDOWPOSCHANGED:
      begin
        if HasItemPositionChanged then
        begin
          UpdateActualItemPosition;
          Container.RequestRealignItems;
        end;
      end;
    49476:
      begin
        if ItemControl.Enabled <> Enabled then
          Enabled := ItemControl.Enabled;
      end;
  end;
end;

{ TLayoutManager }

function TLayoutManager.AddControl(AControl: TControl): TLayoutManagerItem;
begin
  AControl.Parent := Self;

  Result := GetControlItem(AControl);
end;

procedure TLayoutManager.CMControlChange(var Message: TCMControlChange);
var
  lItem: TLayoutManagerItem;
  lControl: TControl;
begin
  inherited;

  lControl := Message.Control;

  if (csLoading in ComponentState) or (not Message.Inserting) or
    (lControl.ClassType = TLayoutManagerItemCaptionOptions) or
    ((lControl.ClassType = TLayoutManagerItem) and
    (isInitializing in (lControl as TLayoutManagerItem).State)) then
    Exit;

  if (Assigned(GetControlItem(lControl))) then
  begin
    //Copy Control - ItemControl already exists
    //Called when a control will be copied and dropped on LM
    InitializeItem(GetControlItem(lControl), lControl);
  end
  else if (lControl.ClassType = TLayoutManagerItem) and
    (csDesigning in lControl.ComponentState) then
  begin
    //Copy Item - Item and ItemControl already exists
    //Called when an item will be copied and dropped on LM
    lItem := lControl as TLayoutManagerItem;

    Include(FState, lmLoadingItem);
    Include(lItem.FState, isInitializing);

    lItem.Container := Self;
    lItem.Parent := Self;

    ItemList.Add(lItem);

    Exclude(FState, lmLoadingItem);
    Exclude(lItem.FState, isInitializing);
  end
  else
  begin
    //New Item - Control does not have an item
    //Called when a new control will be dropped on LM
    InitializeItem(TLayoutManagerItem.Create(Owner), lControl);
  end;

  RealignItems;
end;

procedure TLayoutManager.CMFreeItem(var Message: TMessage);
begin
  TObject(Message.WParam).Free;
end;

procedure TLayoutManager.CMRealignItems(var Message: TMessage);
begin
  RealignItems;

  FHasRealignItemsMsgPending := False;
end;

procedure TLayoutManager.CMResizeItems(var Message: TMessage);
begin
  ResizeItems;

  FHasResizeItemsMsgPending := False;
end;

constructor TLayoutManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks];

  FItemDefPadding := TLayoutManagerPadding.Create;
  FItemDefPadding.Container := Self;

  FItemSpacing := TLayoutManagerSpacing.Create;
  FItemSpacing.Container := Self;

  AutoScroll := True;
  FAutoWrap := True;
  FItemList := TLayoutManagerItemList.Create(False);
  FItemDefCaptionPos := lpLeft;
  FItemDefControlSpacing := 3;
  FItemLayout := laHorizontal;
  FHasRealignItemsMsgPending := False;
  FHasResizeItemsMsgPending := False;

  State := [];
end;

procedure TLayoutManager.CreateWnd;
begin
  inherited;
end;

destructor TLayoutManager.Destroy;
begin
  FreeAndNil(FItemList);
  FreeAndNil(FItemDefPadding);
  FreeAndNil(FItemSpacing);

  inherited;
end;

function TLayoutManager.GetAlignMode: TAlign;
begin
   Result := Align;
end;

function TLayoutManager.GetAutoWrap: Boolean;
begin
  Result := FAutoWrap;
end;

procedure TLayoutManager.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to ItemList.Count - 1 do
    Proc(ItemList.Items[I]);
end;

function TLayoutManager.GetControlItem(AControl: TControl): TLayoutManagerItem;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to ItemList.Count - 1 do
    if ItemList.Items[I].ItemControl = AControl then
      Result := ItemList.Items[I];
end;

function TLayoutManager.GetItemDefControlSpacing: Integer;
begin
  Result := FItemDefControlSpacing;
end;

function TLayoutManager.GetItemDefPadding: TLayoutManagerPadding;
begin
  Result := FItemDefPadding;
end;

function TLayoutManager.GetItemSpacing: TLayoutManagerSpacing;
begin
  Result := FItemSpacing;
end;


function TLayoutManager.GetIsReady: Boolean;
begin
  Result := (not (csLoading in ComponentState)) and
    (not (csDestroying in ComponentState)) and (not (lmLoadingItem in State)) and
    (not (lmRealigningItems in State)) and (not (lmResizingItems in State));
end;

function TLayoutManager.GetItemIndexByControlName(AControlName: string): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to ItemList.Count - 1 do
    if ItemList.Items[I].ItemControl.Name = AControlName then
      Result := I;
end;

function TLayoutManager.GetItemDefCaptionPos: TLabelPosition;
begin
  Result := FItemDefCaptionPos;
end;

function TLayoutManager.GetItemLayout: TLayoutOrientation;
begin
  Result := FItemLayout;
end;

function TLayoutManager.GetItemsOutOfContainersHeight(AItems: TObjectList = nil): Boolean;
var
  I: Integer;
  lItem: TLayoutManagerItem;
begin
  Result := False;

  for I := 0 to ItemList.Count - 1 do
  begin
    lItem := ItemList.Items[I];

    if (lItem.Top > ItemSpacing.Top) and ((lItem.Top + lItem.Height) > Height)
      and (lItem.Visible) then
    begin
      Result := True;

      if Assigned(AItems) then
        AItems.Add(lItem);
    end;
  end;
end;

function TLayoutManager.GetItemsOutOfContainersWidth(AItems: TObjectList): Boolean;
var
  I: Integer;
  lItem: TLayoutManagerItem;
begin
  Result := False;

  for I := 0 to ItemList.Count - 1 do
  begin
    lItem := ItemList.Items[I];

    if (lItem.Left > ItemSpacing.Left) and ((lItem.Left + lItem.Width) > Width)
      and (lItem.Visible) then
    begin
      Result := True;

      if Assigned(AItems) then
        AItems.Add(lItem);
    end;
  end;
end;

function TLayoutManager.GetState: TLayoutManagerState;
begin
  Result := FState;
end;

function TLayoutManager.GetScreenHeightRequired: Integer;
var
  I: Integer;
  lItemsOut: TLayoutManagerItemList;
begin
  Result := 0;

  for I := 0 to ItemList.Count - 1 do
    if ItemList.Items[I].Visible then
      Result := Max(ItemList.Items[I].Top + ItemList.Items[I].Height +
        ItemSpacing.Bottom, Result);

  lItemsOut := TLayoutManagerItemList.Create(False);

  try
    GetItemsOutOfContainersWidth(lItemsOut);

    //Add height needed to show items out of container
    for I := 0 to lItemsOut.Count - 1 do
      if lItemsOut.Items[I].Visible then
        Result := Result + lItemsOut.Items[I].GetFullHeight;

  finally
    lItemsOut.Free;
  end;

  if Result > Screen.Height then
    Result := Screen.Height;
end;

function TLayoutManager.GetScreenWidthRequired: Integer;
var
  I: Integer;
  lItemsOut: TLayoutManagerItemList;
begin
  Result := 0;

  for I := 0 to ItemList.Count - 1 do
    if ItemList.Items[I].Visible then
      Result := Max(ItemList.Items[I].Left + ItemList.Items[I].Width +
        ItemSpacing.Right, Result);

  lItemsOut := TLayoutManagerItemList.Create(False);

  try
    GetItemsOutOfContainersHeight(lItemsOut);

    //Add width needed to show items out of container
    for I := 0 to lItemsOut.Count - 1 do
      if lItemsOut.Items[I].Visible then
        Result := Result + lItemsOut.Items[I].GetFullWidth;

  finally
    lItemsOut.Free;
  end;

  if Result > Screen.Width then
    Result := Screen.Width;
end;

function TLayoutManager.HasSizeChanged: Boolean;
begin
  Result := (FActualSize.X <> Width) or (FActualSize.Y <> Height);
end;

procedure TLayoutManager.InitializeItem(AItem: TLayoutManagerItem;
  AControl: TControl);
begin
  if (AItem = nil) or (AControl = nil) then
    Exit;

  Include(FState, lmLoadingItem);
  AItem.State := AItem.State + [isInitializing];

  AItem.Name := AControl.Name + 'Item';
  AItem.Container := Self;
  AItem.ItemControl := AControl;
  AItem.Parent := Self;
  AItem.Caption := AControl.Name;
  AItem.FreeNotification(AControl);

  AControl.Parent := AItem;
  AControl.FreeNotification(AItem);

  if ItemList.IndexOf(AItem) = - 1 then
    ItemList.Add(AItem);

  AItem.State := AItem.State - [isInitializing];
  Exclude(FState, lmLoadingItem);

  AItem.Initialize;
end;

function TLayoutManager.IsChildLayoutManager: Boolean;
begin
  Result := Parent is TLayoutManagerItem;
end;

procedure TLayoutManager.Loaded;
var
  I: Integer;
  lItem: TLayoutManagerItem;
begin
  inherited;

  ItemList.Clear;

  for I := 0 to ControlCount - 1 do
  begin
    if Controls[I] is TLayoutManagerItem then
    begin
      lItem := Controls[I] as TLayoutManagerItem;
      lItem.Container := Self;

      ItemList.Add(lItem);

      lItem.Realign(True);

      lItem.UpdateActualItemSize;
      lItem.UpdateActualControlSize;
    end;
  end;

  UpdateActualSize;

  RealignItems;
end;

procedure TLayoutManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent is TLayoutManagerItem) and
    (Assigned(ItemList)) then
    ItemList.Remove(AComponent);

  inherited;
end;

procedure TLayoutManager.PositioningItems(AUseAutoWrap: Boolean; AAvailableSize: Integer = 0);
var
  I: Integer;
  iMaxHeight, iMaxWidth, iAvailableSize: Integer;
  Position: TPoint;
  Rect: TRect;
  Size: TSize;
  lItem: TLayoutManagerItem;
begin
  if ControlCount = 0 then
    Exit;

  iMaxHeight := 0;
  iMaxWidth := 0;

  if AAvailableSize > 0 then
    iAvailableSize := AAvailableSize
  else
  begin
    if ItemLayout = laHorizontal then
      iAvailableSize := Width
    else
      iAvailableSize := Height;
  end;

  Rect := GetClientRect;

  AdjustClientRect(Rect);

  //If AutoSize is True, Container sizes will be changed with control sizes
  if AutoSize then
    Rect := Classes.Rect(Rect.Left, Rect.Top,
      Rect.Left + (Width - (Width - (Rect.Right - Rect.Left))),
      Rect.Top + (Height - (Height - (Rect.Bottom - Rect.Top))));

  Position := Rect.TopLeft;

  //Increase Letf and Top spacing
  Inc(Position.X, ItemSpacing.Left);
  Inc(Position.Y, ItemSpacing.Top);

  for I := 0 to ItemList.Count - 1 do
  begin
    lItem := ItemList.Items[I];

    if not lItem.Visible and not (csDesigning in ComponentState) then
      Continue;

    //Get actual item's size
    Size.cx := lItem.Width;
    Size.cy := lItem.Height;

    case ItemLayout of
      laHorizontal:
        //If is not the first control, and X position plus actual item width is
        //bigger then available size and AutoWrap is True, put the item on the next line
        if (iMaxHeight > 0) and ((Position.X + Size.cx) > iAvailableSize) and FAutoWrap then
        begin
          //If AUseAutoWrap OR the item is on the next line, X position will be reseted
          if (AUseAutoWrap) or (lItem.Top <> Position.Y) then
            Position.X := Rect.Left + ItemSpacing.Left;

          //Increment Y position with the higher item size
          Inc(Position.Y, iMaxHeight + ItemSpacing.Bottom + ItemSpacing.Top);
          iMaxHeight := 0;
        end;
      laVertical:
        //If is not the first control, and Y position plus actual item height is
        //bigger then available size and AutoWrap is True, put the item on the next column
        if (iMaxWidth > 0) and ((Position.Y + Size.cy) > iAvailableSize) and FAutoWrap then
        begin
          //If AUseAutoWrap OR the item is on the next column, Y position will be reseted
          if (AUseAutoWrap) or (lItem.Left <> Position.X) then
            Position.Y := Rect.Top + ItemSpacing.Top;

          //Increment X position with the wider item size
          Inc(Position.X, iMaxWidth + ItemSpacing.Right + ItemSpacing.Top);
          iMaxWidth := 0;
        end;
    end;

    if Size.cy > iMaxHeight then
      iMaxHeight := Size.cy;

    if Size.cx > iMaxWidth then
      iMaxWidth := Size.cx;

    //If AUseAutoWrap is not in use:
    //laHorizontal: Y (Top) item value can't change
    //laVertical: X (Left) item value can't change
    if not AUseAutoWrap then
    begin
      case ItemLayout of
        laHorizontal:
          //If calculated Y value is different from item, X value should be
          //restarted in order to positioning item from begin of the line
          if Position.Y <> lItem.Top then
          begin
            Position.Y := lItem.Top;
            Position.X := Rect.Left + ItemSpacing.Left;
          end;
        laVertical:
          //If calculated X value is different from item, Y value should be
          //restarted in order to positioning item from begin of the column
          if Position.X <> lItem.Left then
          begin
            Position.X := lItem.Left;
            Position.Y := Rect.Top + ItemSpacing.Top;
          end;
      end;
    end;

    lItem.SetBounds(Position.X, Position.Y, Size.cx, Size.cy);

    //Increment spacing sizes on X and Y positions
    if ItemLayout = laHorizontal then
      Inc(Position.X, Size.cx + ItemSpacing.Right + ItemSpacing.Left)
    else
      Inc(Position.Y, Size.cy + ItemSpacing.Bottom + ItemSpacing.Top);
  end;

  ControlsAligned;

  if Showing then
    AdjustSize;
end;

procedure TLayoutManager.RealignItems;
var
  I: Integer;
begin
  Include(FState, lmRealigningItems);

  PositioningItems(True);

  for I := 0 to ItemList.Count - 1 do
    ItemList.Items[I].CaptionOptions.AdditionalSpacing := 0;

  if AutoWrap then
    RecalculateCaptionSpacings;

  Exclude(FState, lmRealigningItems);
end;

procedure TLayoutManager.RecalculateCaptionSpacings;
type
  TCaptionSizes = array of array [0..1] of integer;
var
  I, K, iPosition, iLimit: Integer;
  aPrimarySizes, aSecundarySizes, aCaptionHeights, aCaptionWidths: TCaptionSizes;
  lItem: TLayoutManagerItem;


  procedure UpdateMaxSize(var ACaptionSizes: TCaptionSizes; APosition, AMax: Integer);
  var
    J: Integer;
  begin
    for J := Low(ACaptionSizes) to High(ACaptionSizes) do
      if  ACaptionSizes[J, 0] = APosition then
      begin
        ACaptionSizes[J, 1] := Max(ACaptionSizes[J, 1], AMax);
        Exit;
      end;

    SetLength(ACaptionSizes, Length(ACaptionSizes) + 1);

    ACaptionSizes[High(ACaptionSizes), 0] := APosition;
    ACaptionSizes[High(ACaptionSizes), 1] := AMax;
  end;


  procedure UpdateMaxCaptionSizes;
  var
    J: Integer;
  begin
    aPrimarySizes := nil;
    aSecundarySizes := nil;

    for J := 0 to ItemList.Count - 1 do
      if (ItemList.Items[J].AutoAlignCaptions) and
        (ItemList.Items[J].CaptionVisible) then
      begin
        lItem := ItemList.Items[J];

        UpdateMaxSize(aCaptionWidths, lItem.Left, lItem.ItemLabel.Width + lItem.CaptionOptions.ControlSpacing);
        UpdateMaxSize(aCaptionHeights, lItem.Top, lItem.ItemLabel.Height + lItem.CaptionOptions.ControlSpacing);
      end;

    if ItemLayout = laHorizontal then
    begin
      aPrimarySizes := aCaptionWidths;
      aSecundarySizes := aCaptionHeights;
    end
    else
    begin
      aPrimarySizes := aCaptionHeights;
      aSecundarySizes := aCaptionWidths;
    end;
  end;


  function GetMaxSize(ACaptionSizes: TCaptionSizes; APosition: Integer): Integer;
  var
    J: Integer;
  begin
    Result := 0;

    for J := Low(ACaptionSizes) to High(ACaptionSizes) do
      if  ACaptionSizes[J, 0] = APosition then
      begin
        Result := ACaptionSizes[J, 1];
        Break;
      end;
  end;


begin
  //Get all items sizes and fill a list with max values for each column and line
  UpdateMaxCaptionSizes;

  I := 0;

  //Iterate items by columns (horizontal LM) or by rows (vertical LM)
  while I <= High(aPrimarySizes) do
  begin
    //Iterate all controls
    for K := 0 to ItemList.Count - 1 do
    begin
      lItem := ItemList.Items[K];

      if ItemLayout = laHorizontal then
        iPosition := lItem.Left
      else
        iPosition := lItem.Top;

      //If item is in the actul column or row, its spacing will be calculated
      if (iPosition = aPrimarySizes[I, 0]) and (lItem.AutoAlignCaptions) and
        (lItem.CaptionVisible) then
      begin
        if lItem.CaptionOptions.Position in [lpLeft, lpRight] then
          lItem.CaptionOptions.AdditionalSpacing := GetMaxSize(aCaptionWidths, lItem.Left) - (lItem.ItemLabel.Width + lItem.CaptionOptions.ControlSpacing)
        else
          lItem.CaptionOptions.AdditionalSpacing := GetMaxSize(aCaptionHeights, lItem.Top) - (lItem.ItemLabel.Height + lItem.CaptionOptions.ControlSpacing);
      end;
    end;

    //Positioning item with limit (higher column or row or LM width)
    if ItemLayout = laVertical then
    begin
      iLimit := GetScreenHeightRequired;

      if iLimit > Height then
        iLimit := Height;
    end
    else
    begin
      iLimit := GetScreenWidthRequired;

      if iLimit > Width then
        iLimit := Width;
    end;

    PositioningItems(False, iLimit);

    //Update sizes list, after re-positioning items
    UpdateMaxCaptionSizes;

    inc(I);
  end;

  if ((ItemLayout = laHorizontal) and (GetItemsOutOfContainersWidth)) or
    ((ItemLayout = laVertical) and (GetItemsOutOfContainersHeight)) then
  begin
    //If has some item out of LM edge, process will restart
    PositioningItems(True);
    RecalculateCaptionSpacings;
  end;
end;

procedure TLayoutManager.RequestRealignItems;
begin
  if (not FHasRealignItemsMsgPending) and (HandleAllocated) and
    (not (lmRealigningItems in State)) and (not (lmResizingItems in State)) then
  begin
    FHasRealignItemsMsgPending := True;
    PostMessage(Handle, CM_LMREALIGNITEMS, 0, 0);
  end;
end;

procedure TLayoutManager.RequestResizeItems;
begin
  if (not FHasResizeItemsMsgPending) and (HandleAllocated) then
  begin
    FHasResizeItemsMsgPending := True;
    PostMessage(Handle, CM_LMRESIZEITEMS, 0, 0);
  end;
end;

procedure TLayoutManager.ResizeItems;
var
  I: Integer;
begin
  Include(FState, lmResizingItems);

  for I := 0 to ItemList.Count - 1 do
  begin
    ItemList.Items[I].HeightOptions.RecalculateSize;
    ItemList.Items[I].WidthOptions.RecalculateSize;
    ItemList.Items[I].Realign(False);
  end;

  Exclude(FState, lmResizingItems);

  UpdateActualSize;

  RealignItems;

  UpdateScrollBars;
end;

procedure TLayoutManager.SetAlignMode(const Value: TAlign);
begin
  if Align <> Value then
  begin
    if IsChildLayoutManager then
    begin
      Align := alNone;

      if Value <> alNone then
        raise Exception.Create('Invalid property value for a child TLayoutManager. Use Height and Width options');
    end
    else
      Align := Value;
  end;
end;

procedure TLayoutManager.SetAutoWrap(Value: Boolean);
begin
  if FAutoWrap <> Value then
  begin
    FAutoWrap := Value;
    RequestRealignItems;
    UpdateScrollBars;
  end;
end;

procedure TLayoutManager.SetItemDefCaptionPos(const Value: TLabelPosition);
var
  I: Integer;
  lItem: TLayoutManagerItem;
begin
  if FItemDefCaptionPos <> Value then
  begin
    FItemDefCaptionPos := Value;

    for I := 0 to ItemList.Count - 1 do
    begin
      lItem := ItemList.Items[I];

      if lItem.ItemControl is TLayoutManager then
        (lItem.ItemControl as TLayoutManager).ItemDefCaptionPos := FItemDefCaptionPos
      else if lItem.CaptionOptions.UseDefPosition then
        lItem.CaptionOptions.Position := FItemDefCaptionPos;
    end;

    RealignItems;
  end;
end;

procedure TLayoutManager.SetItemDefControlSpacing(const Value: Integer);
var
  I: Integer;
  lItem: TLayoutManagerItem;
begin
  if (FItemDefControlSpacing <> Value) and (Value >= 0) then
  begin
    FItemDefControlSpacing := Value;

    for I := 0 to ItemList.Count - 1 do
    begin
      lItem := ItemList.Items[I];

      if lItem.ItemControl is TLayoutManager then
        (lItem.ItemControl as TLayoutManager).ItemDefControlSpacing := FItemDefControlSpacing
      else if lItem.CaptionOptions.UseDefControlSpacing then
        lItem.CaptionOptions.ControlSpacing := FItemDefControlSpacing;
    end;
  end;
end;

procedure TLayoutManager.SetItemDefPadding(const Value: TLayoutManagerPadding);
begin
  if FItemDefPadding <> Value then
    FItemDefPadding.Assign(Value);
end;

procedure TLayoutManager.SetItemSpacing(const Value: TLayoutManagerSpacing);
begin
  if FItemSpacing <> Value then
    FItemSpacing.Assign(Value);
end;

procedure TLayoutManager.SetItemLayout(const Value: TLayoutOrientation);
begin
  if FItemLayout <> Value then
  begin
    FItemLayout := Value;
    RequestRealignItems;
  end;
end;

procedure TLayoutManager.SetState(const Value: TLayoutManagerState);
begin
  FState := Value;
end;

procedure TLayoutManager.UpdateActualSize;
begin
  FActualSize.Y := Height;
  FActualSize.X := Width;
end;

procedure TLayoutManager.UpdateScrollBars;
begin
  if HandleAllocated then
    PostMessage(Handle, WM_SIZE, 0, 0);
end;

procedure TLayoutManager.WMNCHitTest(var Message: TMessage);
begin
  DefaultHandler(Message);
end;

procedure TLayoutManager.WndProc(var Message: TMessage);
begin
  inherited;

  if not IsReady then
    Exit;

  case Message.Msg of
    WM_WINDOWPOSCHANGED:
      begin
        if HasSizeChanged then
          //Call ResizeItems instead RequestResizeItems
          //to avoid problems with child LMs
          ResizeItems;
      end;
  end;
end;

end.
