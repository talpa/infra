unit GUIAnnotation;

interface

uses
  GUIAnnotationIntf, InfraCommon, InfraCommonIntf, InfraValueTypeIntf, Controls,
  ExtCtrls, LayoutManager, InfraValueType, List_Screen, Classes, SysUtils,
  List_ScreenItem;

type

  TScreenItem = class(TElement, IScreenItem)
  private
    FCaption: IInfraString;
    FCaptionPosition: TLabelPosition;
    FCaptionPositionChanged: Boolean;
    FCaptionVisible: IInfraBoolean;
    FItemHeight: IInfraInteger;
    FItemHeightMeasureType: TMeasureType;
    FItemHeightMeasureTypeChanged: Boolean;
    FItemWidth: IInfraInteger;
    FItemWidthMeasureType: TMeasureType;
    FItemWidthMeasureTypeChanged: Boolean;
    FName: string;
    FPutAfter: string;
    FPutBefore: string;
    FVisible: IInfraBoolean;
    function GetCaption: IInfraString;
    function GetCaptionPosition: TLabelPosition;
    function GetCaptionPositionChanged: Boolean;
    function GetCaptionVisible: IInfraBoolean;
    function GetItemHeight: IInfraInteger;
    function GetItemHeightMeasureType: TMeasureType;
    function GetItemHeightMeasureTypeChanged: Boolean;
    function GetItemWidth: IInfraInteger;
    function GetItemWidthMeasureType: TMeasureType;
    function GetItemWidthMeasureTypeChanged: Boolean;
    function GetName: string;
    function GetVisible: IInfraBoolean;
    procedure SetCaption(const Value: IInfraString);
    procedure SetCaptionPosition(const Value: TLabelPosition);
    procedure SetCaptionVisible(const Value: IInfraBoolean);
    procedure SetItemHeight(const Value: IInfraInteger);
    procedure SetItemHeightMeasureType(const Value: TMeasureType);
    procedure SetItemWidth(const Value: IInfraInteger);
    procedure SetItemWidthMeasureType(const Value: TMeasureType);
    procedure SetName(const Value: string);
    procedure SetVisible(const Value: IInfraBoolean);
  public
    constructor Create; override;
    function GetPutAfter: string;
    function GetPutBefore: string;
    procedure SetItemSize(pHeight, pWidth: IInfraInteger);
    procedure PutBefore(pName: string);
    procedure PutAfter(pName: string);
    property Caption: IInfraString read GetCaption write SetCaption;
    property CaptionPosition: TLabelPosition read GetCaptionPosition write SetCaptionPosition;
    property CaptionPositionChanged: Boolean read GetCaptionPositionChanged;
    property CaptionVisible: IInfraBoolean read GetCaptionVisible write SetCaptionVisible;
    property ItemHeight: IInfraInteger read GetItemHeight write SetItemHeight;
    property ItemHeightMeasureType: TMeasureType read GetItemHeightMeasureType write SetItemHeightMeasureType;
    property ItemHeightMeasureTypeChanged: Boolean read GetItemHeightMeasureTypeChanged;
    property ItemWidth: IInfraInteger read GetItemWidth write SetItemWidth;
    property ItemWidthMeasureType: TMeasureType read GetItemWidthMeasureType write SetItemWidthMeasureType;
    property ItemWidthMeasureTypeChanged: Boolean read GetItemWidthMeasureTypeChanged;
    property Name: string read GetName write SetName;
    property Visible: IInfraBoolean read GetVisible write SetVisible;
  end;

  TScreenControl = class(TScreenItem, IScreenControl)
  private
    FControlClass: TControlClass;
    FControlProperty: IInfraString;
    FHeight: IInfraInteger;
    FPropertyName: string;
    FWidth: IInfraInteger;
    function GetControlClass: TControlClass;
    function GetControlProperty: IInfraString;
    function GetHeight: IInfraInteger;
    function GetPropertyName: string;
    function GetWidth: IInfraInteger;
    procedure SetControlClass(const Value: TControlClass);
    procedure SetControlProperty(const Value: IInfraString);
    procedure SetHeight(const Value: IInfraInteger);
    procedure SetPropertyName(const Value: string);
    procedure SetWidth(const Value: IInfraInteger);
  public
    constructor Create; override;
    procedure SetSize(pHeight, pWidth: IInfraInteger);
    property ControlClass: TControlClass read GetControlClass write SetControlClass;
    property ControlProperty: IInfraString read GetControlProperty write SetControlProperty;
    property Height: IInfraInteger read GetHeight write SetHeight;
    property PropertyName: string read GetPropertyName write SetPropertyName;
    property Width: IInfraInteger read GetWidth write SetWidth;
  end;

  TScreenGroup = class(TScreenItem, IScreenGroup)
  private
    FItemLayout: TLayoutOrientation;
    FItems: IScreenItemList;
    function GetItemLayout: TLayoutOrientation;
    function GetItems: IScreenItemList;
    procedure SetItemLayout(const Value: TLayoutOrientation);
  public
    constructor Create; override;
    property ItemLayout: TLayoutOrientation read GetItemLayout write SetItemLayout;
    property Items: IScreenItemList read GetItems;
  end;

  TScreen = class(TElement, IScreen)
  private
    FCaption: IInfraString;
    FCaptionPosition: TLabelPosition;
    FControlSpacing: IInfraInteger;
    FHeight: IInfraInteger;
    FHideProperties: TStrings;
    FItems: IScreenItemList;
    FItemLayout: TLayoutOrientation;
    FItemSpacing: TLayoutManagerSpacing;
    FName: string;
    FPadding: TLayoutManagerPadding;
    FShowProperties: TStrings;
    FWidth: IInfraInteger;
    function GetCaption: IInfraString;
    function GetCaptionPosition: TLabelPosition;
    function GetControlSpacing: IInfraInteger;
    function GetHeight: IInfraInteger;
    function GetHideProperties: TStrings;
    function GetItemLayout: TLayoutOrientation;
    function GetItems: IScreenItemList;
    function GetItemSpacing: TLayoutManagerSpacing;
    function GetName: string;
    function GetPadding: TLayoutManagerPadding;
    function GetShowProperties: TStrings;
    function GetWidth: IInfraInteger;
    procedure SetCaption(const Value: IInfraString);
    procedure SetCaptionPosition(const Value: TLabelPosition);
    procedure SetControlSpacing(const Value: IInfraInteger);
    procedure SetHeight(const Value: IInfraInteger);
    procedure SetItemLayout(const Value: TLayoutOrientation);
    procedure SetName(const Value: string);
    procedure SetWidth(const Value: IInfraInteger);
  public
    constructor Create; override;
    destructor Destroy; override;
    function AddControl(pPropertyName: string): IScreenControl;
    function AddGroup(pName: string): IScreenGroup;
    function GetControl(pPropertyName: string): IScreenControl;
    function GetControlByName(pName: string): IScreenControl;
    function GetGroup(pName: string): IScreenGroup;
    procedure Group(pProperties: TStrings);
    procedure SetSize(pHeight, pWidth: IInfraInteger);
    function UseProperty(pPropertyName: string): Boolean;
    property Caption: IInfraString read GetCaption write SetCaption;
    property CaptionPosition: TLabelPosition read GetCaptionPosition write SetCaptionPosition;
    property ControlSpacing: IInfraInteger read GetControlSpacing write SetControlSpacing;
    property Height: IInfraInteger read GetHeight write SetHeight;
    property HideProperties: TStrings read GetHideProperties;
    property Items: IScreenItemList read GetItems;
    property ItemLayout: TLayoutOrientation read GetItemLayout write SetItemLayout;
    property ItemSpacing: TLayoutManagerSpacing read GetItemSpacing;
    property Name: string read GetName write SetName;
    property Padding: TLayoutManagerPadding read GetPadding;
    property ShowProperties: TStrings read GetShowProperties;
    property Width: IInfraInteger read GetWidth write SetWidth;
  end;

  TScreens = class(TElement, IScreens)
  private
    FScreens: IScreenList;
    function GetScreens: IScreenList;
  public
    constructor Create; override;
    function AddScreen(pName: string): IScreen;
    function GetScreen(pName: string): IScreen;
    property Screens: IScreenList read GetScreens;
  end;

implementation

{ TScreenItem }

constructor TScreenItem.Create;
begin
  inherited;

  FCaptionPositionChanged := False;
  FItemHeightMeasureTypeChanged := False;
  FItemWidthMeasureTypeChanged := False;
  FCaption := TInfraString.Create;
  FCaptionPosition := lpLeft;
  FCaptionVisible := TInfraBoolean.Create;
  FItemHeight := TInfraInteger.Create;
  FItemHeightMeasureType := mtFix;
  FItemWidth := TInfraInteger.Create;
  FItemWidthMeasureType := mtFix;
  FVisible := TInfraBoolean.Create;
end;

function TScreenItem.GetCaption: IInfraString;
begin
  Result := FCaption;
end;

function TScreenItem.GetCaptionPosition: TLabelPosition;
begin
  Result := FCaptionPosition;
end;

function TScreenItem.GetCaptionPositionChanged: Boolean;
begin
  Result := FCaptionPositionChanged;
end;

function TScreenItem.GetCaptionVisible: IInfraBoolean;
begin
  Result := FCaptionVisible;
end;

function TScreenItem.GetItemHeight: IInfraInteger;
begin
  Result := FItemHeight;
end;

function TScreenItem.GetItemHeightMeasureType: TMeasureType;
begin
  Result := FItemHeightMeasureType;
end;

function TScreenItem.GetItemHeightMeasureTypeChanged: Boolean;
begin
  Result := FItemHeightMeasureTypeChanged;
end;

function TScreenItem.GetItemWidth: IInfraInteger;
begin
  Result := FItemWidth;
end;

function TScreenItem.GetItemWidthMeasureType: TMeasureType;
begin
  Result := FItemWidthMeasureType;
end;

function TScreenItem.GetItemWidthMeasureTypeChanged: Boolean;
begin
  Result := FItemWidthMeasureTypeChanged;
end;

function TScreenItem.GetName: string;
begin
  Result := FName;
end;

function TScreenItem.GetPutAfter: string;
begin
  Result := FPutAfter;
end;

function TScreenItem.GetPutBefore: string;
begin
  Result := FPutBefore;
end;

function TScreenItem.GetVisible: IInfraBoolean;
begin
  Result := FVisible;
end;

procedure TScreenItem.PutAfter(pName: string);
begin
  Assert(Length(FPutBefore) = 0, 'PutBefore method was already used for this item');

  FPutAfter := pName;
end;

procedure TScreenItem.PutBefore(pName: string);
begin
  Assert(Length(FPutAfter) = 0, 'PutAfter method was already used for this item');

  FPutBefore := pName;
end;

procedure TScreenItem.SetCaption(const Value: IInfraString);
begin
  FCaption := Value;
end;

procedure TScreenItem.SetCaptionPosition(const Value: TLabelPosition);
begin
  FCaptionPositionChanged := True;
  FCaptionPosition := Value;
end;

procedure TScreenItem.SetCaptionVisible(const Value: IInfraBoolean);
begin
  FCaptionVisible := Value;
end;

procedure TScreenItem.SetItemHeight(const Value: IInfraInteger);
begin
  FItemHeight := Value;
end;

procedure TScreenItem.SetItemHeightMeasureType(const Value: TMeasureType);
begin
  if FItemHeightMeasureType <> Value then
  begin
    FItemHeightMeasureTypeChanged := True;
    FItemHeightMeasureType := Value;
  end;
end;

procedure TScreenItem.SetItemSize(pHeight, pWidth: IInfraInteger);
begin
  FItemHeight := pHeight;
  FItemWidth := pWidth;
end;

procedure TScreenItem.SetItemWidth(const Value: IInfraInteger);
begin
  FItemWidth := Value;
end;

procedure TScreenItem.SetItemWidthMeasureType(const Value: TMeasureType);
begin
  if FItemWidthMeasureType <> Value then
  begin
    FItemWidthMeasureTypeChanged := True;
    FItemWidthMeasureType := Value;
  end;
end;

procedure TScreenItem.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TScreenItem.SetVisible(const Value: IInfraBoolean);
begin
  FVisible := Value;
end;

{ TScreenControl }

constructor TScreenControl.Create;
begin
  inherited;

  FControlProperty := TInfraString.Create;
  FHeight := TInfraInteger.Create;
  FWidth := TInfraInteger.Create;
end;

function TScreenControl.GetControlClass: TControlClass;
begin
  Result := FControlClass;
end;

function TScreenControl.GetControlProperty: IInfraString;
begin
  Result := FControlProperty;
end;

function TScreenControl.GetHeight: IInfraInteger;
begin
  Result := FHeight;
end;

function TScreenControl.GetPropertyName: string;
begin
  Result := FPropertyName;
end;

function TScreenControl.GetWidth: IInfraInteger;
begin
  Result := FWidth;
end;

procedure TScreenControl.SetControlClass(const Value: TControlClass);
begin
  FControlClass := Value;
end;

procedure TScreenControl.SetControlProperty( const Value: IInfraString);
begin
  FControlProperty := Value;
end;

procedure TScreenControl.SetHeight(const Value: IInfraInteger);
begin
  FHeight := Value;
end;

procedure TScreenControl.SetPropertyName(const Value: string);
begin
  FPropertyName := Value;
end;

procedure TScreenControl.SetSize(pHeight, pWidth: IInfraInteger);
begin
  FHeight := pHeight;
  FWidth := pWidth;
end;

procedure TScreenControl.SetWidth(const Value: IInfraInteger);
begin
  FWidth := Value;
end;

{ TScreenGroup }

constructor TScreenGroup.Create;
begin
  inherited;

  FItems := TScreenItemList.Create;
end;

function TScreenGroup.GetItemLayout: TLayoutOrientation;
begin
  Result := FItemLayout;
end;

function TScreenGroup.GetItems: IScreenItemList;
begin
  Result := FItems;
end;

procedure TScreenGroup.SetItemLayout(const Value: TLayoutOrientation);
begin
  FItemLayout := Value;
end;

{ TScreen }

function TScreen.AddControl(pPropertyName: string): IScreenControl;
var
  lControl: IScreenControl;
begin
  lControl := GetControl(pPropertyName);

  if Assigned(lControl) then
    Result := lControl
  else
  begin
    Result := TScreenControl.Create;
    Result.Name := StringReplace(pPropertyName, '.', '', [rfReplaceAll]);
    Result.PropertyName := pPropertyName;

    Items.Add(Result);
  end;
end;

function TScreen.AddGroup(pName: string): IScreenGroup;
var
  lGroup: IScreenGroup;
begin
  lGroup := GetGroup(pName);

  if Assigned(lGroup) then
    Result := lGroup
  else
  begin
    Result := TScreenGroup.Create;
    Result.Name := pName;

    Items.Add(Result);
  end;
end;

constructor TScreen.Create;
begin
  inherited;

  FCaption := TInfraString.Create;
  FCaptionPosition := lpLeft;
  FControlSpacing := TInfraInteger.Create;
  FHeight := TInfraInteger.Create;
  FHideProperties := TStringList.Create;
  FItems := TScreenItemList.Create;
  FItemLayout := laHorizontal;
  FItemSpacing := TLayoutManagerSpacing.Create;
  FPadding := TLayoutManagerPadding.Create;
  FShowProperties := TStringList.Create;
  FWidth := TInfraInteger.Create;
end;

destructor TScreen.Destroy;
begin
  FPadding.Free;
  FItemSpacing.Free;
  FHideProperties.Free;
  FShowProperties.Free;
end;

function TScreen.GetCaption: IInfraString;
begin
  Result := FCaption;
end;

function TScreen.GetCaptionPosition: TLabelPosition;
begin
  Result := FCaptionPosition;
end;

function TScreen.GetControl(pPropertyName : string): IScreenControl;
var
  It: IScreenItemIterator;
begin
  Result := nil;

  It := Items.NewIterator;

  while not It.IsDone do
  begin
    if (Supports(It.CurrentItem, IScreenControl)) and
      (SameText((It.CurrentItem as IScreenControl).PropertyName, pPropertyName)) then
    begin
      Result := It.CurrentItem as IScreenControl;
      Break;
    end;

    It.Next;
  end;
end;

function TScreen.GetControlByName(pName: string): IScreenControl;
var
  It: IScreenItemIterator;
begin
  Result := nil;

  It := Items.NewIterator;

  while not It.IsDone do
  begin
    if (Supports(It.CurrentItem, IScreenControl)) and
      (SameText((It.CurrentItem as IScreenControl).Name, pName)) then
    begin
      Result := It.CurrentItem as IScreenControl;
      Break;
    end;

    It.Next;
  end;
end;

function TScreen.GetControlSpacing: IInfraInteger;
begin
  Result := FControlSpacing;
end;

function TScreen.GetGroup(pName: string): IScreenGroup;
var
  It: IScreenItemIterator;
begin
  Result := nil;

  It := Items.NewIterator;

  while not It.IsDone do
  begin
    if (Supports(It.CurrentItem, IScreenGroup)) and
      (SameText((It.CurrentItem as IScreenGroup).Name, pName)) then
    begin
      Result := It.CurrentItem as IScreenGroup;
      Break;
    end;

    It.Next;
  end;
end;

function TScreen.GetHeight: IInfraInteger;
begin
  Result := FHeight;
end;

function TScreen.GetHideProperties: TStrings;
begin
  Result := FHideProperties;
end;

function TScreen.GetItemLayout: TLayoutOrientation;
begin
  Result := FItemLayout;
end;

function TScreen.GetItems: IScreenItemList;
begin
  Result := FItems;
end;

function TScreen.GetItemSpacing: TLayoutManagerSpacing;
begin
  Result := FItemSpacing;
end;

function TScreen.GetName: string;
begin
  Result := FName;
end;

function TScreen.GetPadding: TLayoutManagerPadding;
begin
  Result := FPadding;
end;

function TScreen.GetShowProperties: TStrings;
begin
  Result := FShowProperties;
end;

function TScreen.GetWidth: IInfraInteger;
begin
  Result := FWidth;
end;

procedure TScreen.Group(pProperties: TStrings);
begin
  //TODO
end;

procedure TScreen.SetCaption(const Value: IInfraString);
begin
  FCaption := Value;
end;

procedure TScreen.SetCaptionPosition(const Value: TLabelPosition);
begin
  FCaptionPosition := Value;
end;

procedure TScreen.SetControlSpacing(const Value: IInfraInteger);
begin
  FControlSpacing := Value;
end;

procedure TScreen.SetHeight(const Value: IInfraInteger);
begin
  FHeight := Value;
end;

procedure TScreen.SetItemLayout(const Value: TLayoutOrientation);
begin
  FItemLayout := Value;
end;

procedure TScreen.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TScreen.SetSize(pHeight, pWidth: IInfraInteger);
begin
  FHeight := pHeight;
  FWidth := pWidth;
end;

procedure TScreen.SetWidth(const Value: IInfraInteger);
begin
  FWidth := Value;
end;

function TScreen.UseProperty(pPropertyName: string): Boolean;
begin
  Result := True;

  if ShowProperties.Count > 0 then
  begin
    if ShowProperties.IndexOf(pPropertyName) = -1 then
      Result := False;
  end
  else if HideProperties.IndexOf(pPropertyName) >= 0 then
    Result := False;
end;

{ TScreens }

function TScreens.AddScreen(pName: string): IScreen;
var
  lScreen: IScreen;
begin
  lScreen :=  GetScreen(pName);

  if Assigned(lScreen) then
    Result := lScreen
  else
  begin
    Result := TScreen.Create;
    Result.Name := pName;

    Screens.Add(Result);
  end;
end;

constructor TScreens.Create;
begin
  inherited;
  
  FScreens := TScreenList.Create;
end;

function TScreens.GetScreen(pName: string): IScreen;
var
  It: IScreenIterator;
begin
  Result := nil;

  It := Screens.NewIterator;

  while not It.IsDone do
  begin
    if SameText((It.CurrentItem as IScreen).Name, pName) then
    begin
      Result := It.CurrentItem as IScreen;
      Break;
    end;

    It.Next;
  end;
end;

function TScreens.GetScreens: IScreenList;
begin
  Result := FScreens;
end;

end.
