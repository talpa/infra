unit GUIAnnotation;

interface

uses
  GUIAnnotationIntf, InfraCommon, InfraCommonIntf, InfraValueTypeIntf, Controls,
  ExtCtrls, LayoutManager, InfraValueType, List_Screen, Classes, SysUtils,
  List_ScreenItem, List_CustomProperty;

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
    function GetPutAfter: string;
    function GetPutBefore: string;
    function GetVisible: IInfraBoolean;
    procedure SetCaption(const Value: IInfraString);
    procedure SetCaptionPosition(const Value: TLabelPosition);
    procedure SetCaptionVisible(const Value: IInfraBoolean);
    procedure SetItemHeight(const Value: IInfraInteger);
    procedure SetItemHeightMeasureType(const Value: TMeasureType);
    procedure SetItemWidth(const Value: IInfraInteger);
    procedure SetItemWidthMeasureType(const Value: TMeasureType);
    procedure SetName(const Value: string);
    procedure SetPutAfter(const Value: string);
    procedure SetPutBefore(const Value: string);
    procedure SetVisible(const Value: IInfraBoolean);
  public
    constructor Create; override;
    function CloneItem: IScreenItem;
    procedure SetItemSize(pHeight, pWidth: IInfraInteger);
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
    property PutAfter: string read GetPutAfter write SetPutAfter;
    property PutBefore: string read GetPutBefore write SetPutBefore;
    property Visible: IInfraBoolean read GetVisible write SetVisible;
  end;

  TCustomProperty = class(TElement, ICustomProperty)
  private
    FPropName: string;
    FPropValue: Variant;
    function GetPropName: string;
    function GetPropValue: Variant;
    function Clone: ICustomProperty;
    procedure SetPropName(const Value: string);
    procedure SetPropValue(const Value: Variant);
  public
    property PropName: string read GetPropName write SetPropName;
    property PropValue: Variant read GetPropValue write SetPropValue;
  end;

  TScreenControl = class(TScreenItem, IScreenControl)
  private
    FControlClass: TControlClass;
    FControlProperty: IInfraString;
    FCustomProperties: ICustomPropertyList;
    FHeight: IInfraInteger;
    FPropertyName: string;
    FWidth: IInfraInteger;
    function GetControlClass: TControlClass;
    function GetControlProperty: IInfraString;
    function GetCustomProperties: ICustomPropertyList;
    function GetHeight: IInfraInteger;
    function GetPropertyName: string;
    function GetWidth: IInfraInteger;
    procedure SetControlClass(const Value: TControlClass);
    procedure SetControlProperty(const Value: IInfraString);
    procedure SetCustomProperties(const Value: ICustomPropertyList);
    procedure SetHeight(const Value: IInfraInteger);
    procedure SetPropertyName(const Value: string);
    procedure SetWidth(const Value: IInfraInteger);
  public
    constructor Create; override;
    function Clone: IScreenControl;
    procedure SetSize(pHeight, pWidth: IInfraInteger);
    property ControlClass: TControlClass read GetControlClass write SetControlClass;
    property ControlProperty: IInfraString read GetControlProperty write SetControlProperty;
    property CustomProperties: ICustomPropertyList read GetCustomProperties write SetCustomProperties;
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
    procedure SetItems(const Value: IScreenItemList);
  public
    constructor Create; override;
    function Clone: IScreenGroup;
    property ItemLayout: TLayoutOrientation read GetItemLayout write SetItemLayout;
    property Items: IScreenItemList read GetItems write SetItems;
  end;

  TScreen = class(TElement, IScreen)
  private
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
    FTitle: IInfraString;
    FWidth: IInfraInteger;
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
    function GetTitle: IInfraString;
    function GetWidth: IInfraInteger;
    procedure SetCaptionPosition(const Value: TLabelPosition);
    procedure SetControlSpacing(const Value: IInfraInteger);
    procedure SetHeight(const Value: IInfraInteger);
    procedure SetItemLayout(const Value: TLayoutOrientation);
    procedure SetName(const Value: string);
    procedure SetTitle(const Value: IInfraString);
    procedure SetWidth(const Value: IInfraInteger);
    procedure SetItems(const Value: IScreenItemList);
  public
    constructor Create; override;
    destructor Destroy; override;
    function Clone: IScreen;
    function AddControl(pPropertyName: string): IScreenControl;
    function AddGroup(pName: string): IScreenGroup;
    function GetControl(pPropertyName: string): IScreenControl;
    function GetControlByName(pName: string): IScreenControl;
    function GetGroup(pName: string): IScreenGroup;
    procedure Group(pProperties: TStrings);
    procedure SetSize(pHeight, pWidth: Integer);
    function UseProperty(pPropertyName: string): Boolean;
    property CaptionPosition: TLabelPosition read GetCaptionPosition write SetCaptionPosition;
    property ControlSpacing: IInfraInteger read GetControlSpacing write SetControlSpacing;
    property Height: IInfraInteger read GetHeight write SetHeight;
    property HideProperties: TStrings read GetHideProperties;
    property Items: IScreenItemList read GetItems write SetItems;
    property ItemLayout: TLayoutOrientation read GetItemLayout write SetItemLayout;
    property ItemSpacing: TLayoutManagerSpacing read GetItemSpacing;
    property Name: string read GetName write SetName;
    property Padding: TLayoutManagerPadding read GetPadding;
    property ShowProperties: TStrings read GetShowProperties;
    property Title: IInfraString read GetTitle write SetTitle;
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

function TScreenItem.CloneItem: IScreenItem;
begin
  Result := TScreenItem.Create;
  Result.Caption := Caption.Clone as IInfraString;
  Result.CaptionPosition := CaptionPosition;
  Result.CaptionVisible := CaptionVisible.Clone as IInfraBoolean;
  Result.ItemHeight := ItemHeight.Clone as IInfraInteger;
  Result.ItemHeightMeasureType := ItemHeightMeasureType;
  Result.ItemWidth := Result.ItemWidth.Clone as IInfraInteger;
  Result.ItemWidthMeasureType := ItemWidthMeasureType;
  Result.Name := Name;
  Result.PutAfter := PutAfter;
  Result.PutBefore := PutBefore;
  Result.Visible := Visible.Clone as IInfraBoolean;
end;

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

procedure TScreenItem.SetPutAfter(const Value: string);
begin
  if (Length(Value) > 0) and (Length(FPutBefore) > 0) then
    raise Exception.Create('PutBefore method was already used for this item');

  FPutAfter := Value;
end;

procedure TScreenItem.SetPutBefore(const Value: string);
begin
  if (Length(Value) > 0) and (Length(FPutAfter) > 0) then
    raise Exception.Create('PutAfter method was already used for this item');

  FPutBefore := Value;
end;

procedure TScreenItem.SetVisible(const Value: IInfraBoolean);
begin
  FVisible := Value;
end;

{ TCustomProperty }

function TCustomProperty.Clone: ICustomProperty;
begin
  Result := TCustomProperty.Create;
  Result.PropName := PropName;
  Result.PropValue := PropValue;
end;

function TCustomProperty.GetPropName: string;
begin
  Result := FPropName;
end;

function TCustomProperty.GetPropValue: Variant;
begin
  Result := FPropValue;
end;

procedure TCustomProperty.SetPropName(const Value: string);
begin
  FPropName := Value;
end;

procedure TCustomProperty.SetPropValue(const Value: Variant);
begin
  FPropValue := Value;
end;

{ TScreenControl }

function TScreenControl.Clone: IScreenControl;
begin
  Result := TScreenControl.Create;
  Result.Caption := Caption.Clone as IInfraString;
  Result.CaptionPosition := CaptionPosition;
  Result.CaptionVisible := CaptionVisible.Clone as IInfraBoolean;
  Result.ItemHeight := ItemHeight.Clone as IInfraInteger;
  Result.ItemHeightMeasureType := ItemHeightMeasureType;
  Result.ItemWidth := ItemWidth.Clone as IInfraInteger;
  Result.ItemWidthMeasureType := ItemWidthMeasureType;
  Result.Name := Name;
  Result.PutAfter := PutAfter;
  Result.PutBefore := PutBefore;
  Result.Visible := Visible.Clone as IInfraBoolean;

  //ScreenControl
  Result.ControlClass := ControlClass;
  Result.ControlProperty := ControlProperty.Clone as IInfraString;
  Result.CustomProperties := CustomProperties.Clone;
  Result.Height := Height.Clone as IInfraInteger;
  Result.PropertyName := PropertyName;
  Result.Width := Width.Clone as IInfraInteger;
end;

constructor TScreenControl.Create;
begin
  inherited;

  FControlProperty := TInfraString.Create;
  FCustomProperties := TCustomPropertyList.Create;
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

function TScreenControl.GetCustomProperties: ICustomPropertyList;
begin
  Result := FCustomProperties;
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

procedure TScreenControl.SetCustomProperties(const Value: ICustomPropertyList);
begin
  FCustomProperties := Value;
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

function TScreenGroup.Clone: IScreenGroup;
begin
  Result := TScreenGroup.Create;
  Result.Caption := Caption.Clone as IInfraString;
  Result.CaptionPosition := CaptionPosition;
  Result.CaptionVisible := CaptionVisible.Clone as IInfraBoolean;
  Result.ItemHeight := ItemHeight.Clone as IInfraInteger;
  Result.ItemHeightMeasureType := ItemHeightMeasureType;
  Result.ItemWidth := Result.ItemWidth.Clone as IInfraInteger;
  Result.ItemWidthMeasureType := ItemWidthMeasureType;
  Result.Name := Name;
  Result.PutAfter := PutAfter;
  Result.PutBefore := PutBefore;
  Result.Visible := Visible.Clone as IInfraBoolean;

  //ScreenGroup
  Result.ItemLayout := ItemLayout;
  Result.Items := Items.Clone;
end;

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

procedure TScreenGroup.SetItems(const Value: IScreenItemList);
begin
  FItems := Value;
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

function TScreen.Clone: IScreen;
begin
  Result := TScreen.Create;
  Result.CaptionPosition := CaptionPosition;
  Result.ControlSpacing := ControlSpacing.Clone as IInfraInteger;
  Result.Height := Height.Clone as IInfraInteger;
  Result.HideProperties.Assign(HideProperties);
  Result.Items := Items.Clone;
  Result.ItemLayout := ItemLayout;
  Result.ItemSpacing.Assign(ItemSpacing);
  Result.Name := Name;
  Result.Padding.Assign(Padding);
  Result.ShowProperties.Assign(ShowProperties);
  Result.Title := Title.Clone as IInfraString;
  Result.Width := Width.Clone as IInfraInteger;
end;

constructor TScreen.Create;
begin
  inherited;

  FCaptionPosition := lpLeft;
  FControlSpacing := TInfraInteger.Create;
  FHeight := TInfraInteger.Create;
  FHideProperties := TStringList.Create;
  FItems := TScreenItemList.Create;
  FItemLayout := laHorizontal;
  FItemSpacing := TLayoutManagerSpacing.Create;
  FPadding := TLayoutManagerPadding.Create;
  FShowProperties := TStringList.Create;
  FTitle := TInfraString.Create;
  FWidth := TInfraInteger.Create;
end;

destructor TScreen.Destroy;
begin
  FPadding.Free;
  FItemSpacing.Free;
  FHideProperties.Free;
  FShowProperties.Free;
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

function TScreen.GetTitle: IInfraString;
begin
  Result := FTitle;
end;

function TScreen.GetWidth: IInfraInteger;
begin
  Result := FWidth;
end;

procedure TScreen.Group(pProperties: TStrings);
begin
  //TODO
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

procedure TScreen.SetItems(const Value: IScreenItemList);
begin
  FItems := Value;
end;

procedure TScreen.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TScreen.SetSize(pHeight, pWidth: Integer);
begin
  FHeight := TInfraInteger.NewFrom(pHeight);
  FWidth := TInfraInteger.NewFrom(pWidth);
end;

procedure TScreen.SetTitle(const Value: IInfraString);
begin
  FTitle := Value;
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
