unit InfraGUIBuilderForm;

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, LayoutManager, StdCtrls,
  ExtCtrls, Windows, Buttons, Messages, Dialogs, List_GUIControl, typinfo,
  InfraGUIBuilderIntf, InfraValueTypeIntf, GUIAnnotationIntf,
  GUIAnnotationLoaderIntf;

const
  sr4to3Coefficient = 1.33;
  sr16to9Coefficient = 1.77;

type

  TScreenSizeRatio = (sr4to3, sr16to9);

  TPanelPosition = (ppAbove, ppBelow);

  TInfraGUIBuilderForm = class(TCustomForm)
  private
    FAdjustFormSize: Boolean;
    FCanClose: Boolean;
    FExecuted: Boolean;
    FGUI: IGUI;
    FGUIResult: TGUIResult;
    FMainLayoutManager: TLayoutManager;
    FScreenSizeRatio: TScreenSizeRatio;
    procedure CalculateFormHeight;
    procedure CalculateFormWidth;
    function GetCanClose: Boolean;
    function GetGUI: IGUI;
    function GetGUIResult: TGUIResult;
    function GetMainLayoutManager: TLayoutManager;
    function GetScreenSizeCoefficient: Extended;
    function GetScreenSizeRatio: TScreenSizeRatio;
    procedure PositioningForm;
    procedure SetCanClose(const Value: Boolean);
    procedure SetGUI(const Value: IGUI);
    procedure SetGUIResult(const Value: TGUIResult);
    procedure SetScreenSizeRatio(const Value: TScreenSizeRatio);
    procedure WMSYSCOMMAND(var message: TWMSYSCOMMAND); message WM_SYSCOMMAND;
  protected
    procedure CreateLayoutManager;
    function GetAdditionalScreenHeight: Integer; virtual;
    function GetAdditionalScreenWidth: Integer; virtual;
    procedure SelectFirstControl;
    procedure SetItemOrder;
    procedure SetPropAnnotationsForItem(pScreenItem: IScreenItem; pItem: TLayoutManagerItem);
    procedure SetObjectAnnotationsForForm(pScreen: IScreen);
    procedure SetValueFromControl(pGUIControl: IGUIControl; pObject: IInfraObject);
    procedure SetValueToControl(pGUIControl: IGUIControl; pObject: IInfraObject);
  public
    function CloseQuery: Boolean; override;
    function Execute(pShowModal: Boolean = True): TGUIResult;
    function ShowModal: Integer; override;
    procedure AdjustFormSize;
    procedure Build;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    procedure CreateParams(var Params: TCreateParams); override;
    property CanClose: Boolean read GetCanClose write SetCanClose;
    property GUI: IGUI read GetGUI write SetGUI;
    property GUIResult: TGUIResult read GetGUIResult write SetGUIResult;
    property MainLayoutManager: TLayoutManager read GetMainLayoutManager;
    property ScreenSizeRatio: TScreenSizeRatio read GetScreenSizeRatio write SetScreenSizeRatio;
  end;

  TInfraGUIBuilderEditForm = class(TInfraGUIBuilderForm)
  private
    FCancelButton: TButton;
    FLayoutManager: TLayoutManager;
    FPanelPosition: TPanelPosition;
    FSaveButton: TButton;
    procedure DoCancelButtonClick(Sender: TObject);
    procedure DoSaveButtonClick(Sender: TObject);
    function GetCancelButton: TButton;
    function GetLayoutManager: TLayoutManager;
    function GetPanelPosition: TPanelPosition;
    function GetSaveButton: TButton;
    procedure SetPanelPosition(const Value: TPanelPosition);
  protected
    function GetAdditionalScreenHeight: Integer; override;
    function GetAdditionalScreenWidth: Integer; override;
    procedure RealignForm;
    property CancelButton: TButton read GetCancelButton;
    property LayoutManager: TLayoutManager read GetLayoutManager;
    property PanelPosition: TPanelPosition read GetPanelPosition write SetPanelPosition default ppBelow;
    property SaveButton: TButton read GetSaveButton;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
  end;

procedure ExtractDotSeparatedName(pFullName: string; var pFirstName, pSecondName: string);
function WritePropValue(pInstance: TObject; const pPropName: string; const pValue: variant): variant;

implementation

uses
  CustomizeScreen, InfraGUIBuilder, InfraValueTypeConvert, GUIAnnotationLoaderXML;

procedure ExtractDotSeparatedName(pFullName: string; var pFirstName, pSecondName: string);
var
  I: integer;
begin
  pFirstName := '';
  pSecondName := pFullName;
  I := Pos('.', pFullName);

  if (I > 0) then
  begin
    pFirstName := Copy(pFullName, 1, I - 1);
    pSecondName := Copy(pFullName, I + 1, Length(pFullName) - I);
  end;
end;

function WritePropValue(pInstance: TObject; const pPropName: string; const pValue: variant): variant;
var
  lSubObject: TObject;
  sClassName, sAttributeName: string;
  Props: PPropInfo;
begin
  ExtractDotSeparatedName(pPropName, sClassName, sAttributeName);

  if sClassName = '' then
    SetPropValue(pInstance, pPropName, pValue)
  else
  begin
    Props := GetPropInfo(pInstance, sClassName);

    if IsPublishedProp(pInstance, sClassName) and PropIsType(pInstance, sClassName, tkClass) and (Props.GetProc <> nil) then
    begin
      lSubObject := GetObjectProp(pInstance, sClassName);

      if Assigned(lSubObject) then
        WritePropValue(lSubObject, sAttributeName, pValue);
    end;
  end;
end;

{ TInfraGUIBuilderForm }

procedure TInfraGUIBuilderForm.AdjustFormSize;


  procedure AdjustHeight;
  var
    iLastHeight: Integer;
  begin
    iLastHeight := 0;

    while iLastHeight <> ClientHeight do
    begin
      iLastHeight := ClientHeight;
      CalculateFormHeight;
    end;
  end;


  procedure AdjustWidth;
  var
    iLastWidth: Integer;
  begin
    iLastWidth := 0;

    while iLastWidth <> ClientWidth do
    begin
      iLastWidth := ClientWidth;
      CalculateFormWidth;
    end;
  end;


begin
  if MainLayoutManager.ItemLayout = laHorizontal then
  begin
    //Set screen height and calculate screen width
    CalculateFormHeight;
    ClientWidth := Trunc(ClientHeight * GetScreenSizeCoefficient);

    //Ajust screen width to cut off the unused space
    AdjustWidth;

    //After cut space, screen height is calculated again to adjust screen ratio
    ClientHeight := Trunc(ClientWidth / GetScreenSizeCoefficient);

    //Ajust screen height to cut off the unused space
    AdjustHeight;
  end
  else
  begin
    //Set screen width and calculate screen height
    CalculateFormWidth;
    ClientHeight := Trunc(ClientWidth / GetScreenSizeCoefficient);

    //Ajust screen height to cut off the unused space
    AdjustHeight;

    //After cut space, screen width is calculated again to adjust screen ratio
    ClientWidth := Trunc(ClientHeight * GetScreenSizeCoefficient);

    //Ajust screen width to cut off the unused space
    AdjustWidth;
  end;
end;

procedure TInfraGUIBuilderForm.Build;
var
  It: IGUIControlIterator;
  ItCustomProps: ICustomPropertyIterator;
  lGUIControl: IGUIControl;
  lCustomProperty: ICustomProperty;
  lXMLLoader: IGUIAnnotationLoader;
begin
  Height := 300;
  Width := 400;

  CreateLayoutManager;

  //Load XML
  lXMLLoader := TGUIAnnotationLoaderXML.Create;
  lXMLLoader.FileName := GUI.GetConfigurationFileName + '.xml';
  lXMLLoader.GUI := GUI;
  lXMLLoader.Load;

  Caption := GUI.BusinessObject.TypeInfo.Name;

  It := GUI.GUIControlList.NewIterator;

  while not It.IsDone do
  begin
    lGUIControl := It.CurrentItem as IGUIControl;

    lGUIControl.Control := lGUIControl.ControlClass.Create(Self);
    lGUIControl.Control.Name := lGUIControl.Name;
    lGUIControl.Item := MainLayoutManager.AddControl(lGUIControl.Control);

    SetValueToControl(lGUIControl, GUI.BusinessObject);

    if Supports(lGUIControl.ScreenItem, IScreenControl) then
    begin
      SetPropAnnotationsForItem(lGUIControl.ScreenItem as IScreenControl, lGUIControl.Item);

      //Add CustomProperties for control
      ItCustomProps := (lGUIControl.ScreenItem as IScreenControl).CustomProperties.NewIterator;

      while not ItCustomProps.IsDone do
      begin
        lCustomProperty := ItCustomProps.CurrentItem as ICustomProperty;

        WritePropValue(lGUIControl.Control, lCustomProperty.PropName, lCustomProperty.PropValue);

        ItCustomProps.Next;
      end;
    end;

    It.Next;
  end;

  if Assigned(GUI.Screen) then
    SetObjectAnnotationsForForm(GUI.Screen);

  SetItemOrder;

  if FAdjustFormSize then
    AdjustFormSize;

  PositioningForm;

  SelectFirstControl;
end;

procedure TInfraGUIBuilderForm.CalculateFormHeight;
var
  iNewHeight: Integer;
  Rect: TRect;
  pClientArea: TPoint;
begin
  //Get new form Height
  iNewHeight := MainLayoutManager.GetScreenHeightRequired + GetAdditionalScreenHeight;

  //Get the screen work area from Windows
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Rect, 0);

  //Convert screen work area Height to ClientHeight
  pClientArea := ScreenToClient(Point(Width, Rect.Bottom - Rect.Top));

  //if new Height is bigger then available space, new size will this space
  if iNewHeight > Int(pClientArea.Y) then
    Height := Rect.Bottom - Rect.Top
  else
    ClientHeight := iNewHeight;
end;

procedure TInfraGUIBuilderForm.CalculateFormWidth;
var
  iNewWidth: Integer;
  Rect: TRect;
  pClientArea: TPoint;
begin
  //Get new form Width
  iNewWidth := MainLayoutManager.GetScreenWidthRequired + GetAdditionalScreenWidth;

  //Get the screen work area from Windows
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Rect, 0);

  //Convert screen work area Width to ClientWidth
  pClientArea := ScreenToClient(Point(Rect.Right - Rect.Left, Height));

  //if new Width is bigger then available space, new size will this space
  if iNewWidth > Int(pClientArea.X) then
    Width := Max(Rect.Right - Rect.Left, Constraints.MinWidth)
  else
    ClientWidth := Max(iNewWidth, Constraints.MinWidth);
end;

function TInfraGUIBuilderForm.CloseQuery: Boolean;
begin
  Result := FCanClose
end;

procedure TInfraGUIBuilderForm.CreateLayoutManager;
begin
  if Assigned(FMainLayoutManager) then
    FreeAndNil(FMainLayoutManager);

  FMainLayoutManager := TLayoutManager.Create(Self);
  FMainLayoutManager.Parent := Self;
  FMainLayoutManager.AlignMode := alClient;
  FMainLayoutManager.ItemLayout :=  laHorizontal;
end;

constructor TInfraGUIBuilderForm.CreateNew(AOwner: TComponent; Dummy: Integer);
const
  MenuCaption = 'C&ustomize screen';
var
  I: Integer;
  SystemMenu: HMenu;
  MENUITEMINFO: TMENUITEMINFO;
begin
  inherited;

  FAdjustFormSize := True;
  FCanClose := True;
  FExecuted := False;
  FGUIResult := grNone;
  FScreenSizeRatio := sr4to3;

  //Add menu items
  SystemMenu := GetSystemMenu(Handle, False);
  I := GetMenuItemCount(SystemMenu);
  FillChar(MENUITEMINFO, SizeOf(MENUITEMINFO), 0);
  MENUITEMINFO.cbSize := 44;

  //Add separator
  MENUITEMINFO.fMask := MIIM_TYPE;
  MENUITEMINFO.fType := MFT_SEPARATOR;
  InsertMenuItem(SystemMenu, I, TRUE, MENUITEMINFO);

  //Add customize item
  MENUITEMINFO.fMask := MIIM_TYPE or MIIM_ID;
  MENUITEMINFO.fType := MFT_STRING;
  MENUITEMINFO.dwTypeData := PChar(MenuCaption);
  MENUITEMINFO.cch := Length(MenuCaption);
  MENUITEMINFO.wID := 1101; // ID must be < $F000
  InsertMenuItem(SystemMenu, I + 1, TRUE, MENUITEMINFO);
end;

procedure TInfraGUIBuilderForm.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  Params.WndParent := GetDesktopWindow;
end;

function TInfraGUIBuilderForm.Execute(pShowModal: Boolean): TGUIResult;
begin
  FExecuted := True;

  if pShowModal then
    ShowModal
  else
    Show;

  Result := FGUIResult;
end;

function TInfraGUIBuilderForm.ShowModal: Integer;
begin
  if not FExecuted then
    raise Exception.Create('Form must be called with <Execute> method');

  Result := inherited ShowModal;
end;

function TInfraGUIBuilderForm.GetAdditionalScreenHeight: Integer;
begin
  Result := 0;
end;

function TInfraGUIBuilderForm.GetAdditionalScreenWidth: Integer;
begin
  Result := 0;
end;

function TInfraGUIBuilderForm.GetCanClose: Boolean;
begin
  Result := FCanClose;
end;

function TInfraGUIBuilderForm.GetGUI: IGUI;
begin
  Result := FGUI;
end;

function TInfraGUIBuilderForm.GetGUIResult: TGUIResult;
begin
  Result := FGUIResult
end;

function TInfraGUIBuilderForm.GetMainLayoutManager: TLayoutManager;
begin
  Result := FMainLayoutManager;
end;

function TInfraGUIBuilderForm.GetScreenSizeCoefficient: Extended;
begin
  case ScreenSizeRatio of
    sr4to3: Result := sr4to3Coefficient;
    sr16to9: Result := sr16to9Coefficient;
  else
    Result := 1;
  end;
end;

function TInfraGUIBuilderForm.GetScreenSizeRatio: TScreenSizeRatio;
begin
  Result := FScreenSizeRatio;
end;

procedure TInfraGUIBuilderForm.PositioningForm;
var
  Rect: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Rect, 0);

  Left := ((Rect.Right - Rect.Left) - Width) div 2;
  Top := ((Rect.Bottom - Rect.Top) - Height) div 2;
end;

procedure TInfraGUIBuilderForm.SelectFirstControl;
var
  It: IGUIControlIterator;
  lGUIControl: IGUIControl;
begin
  It := GUI.GUIControlList.NewIterator;

  while not It.IsDone do
  begin
    lGUIControl := It.CurrentItem as IGUIControl;

    if (lGUIControl.Control.InheritsFrom(TWinControl)) and
      ((lGUIControl.Control as TWinControl).CanFocus) then
    begin
      ActiveControl := lGUIControl.Control as TWinControl;
      Break;
    end;

    It.Next;
  end;
end;

procedure TInfraGUIBuilderForm.SetCanClose(const Value: Boolean);
begin
  FCanClose := Value;
end;

procedure TInfraGUIBuilderForm.SetGUI(const Value: IGUI);
begin
  FGUI := Value;
end;

procedure TInfraGUIBuilderForm.SetGUIResult(const Value: TGUIResult);
begin
  FGUIResult := Value;
end;

procedure TInfraGUIBuilderForm.SetItemOrder;
var
  It: IGUIControlIterator;
  lCurControl, lNewControl: IGUIControl;
  iCurIndex, iNewIndex: Integer;
begin
  It := GUI.GUIControlList.NewIterator;

  while not It.IsDone do
  begin
    iNewIndex := -1;

    if Assigned((It.CurrentItem as IGUIControl).ScreenItem) then
    begin
      lCurControl := It.CurrentItem as IGUIControl;
      iCurIndex := MainLayoutManager.GetItemIndexByControlName(lCurControl.Name);

      if Length(lCurControl.ScreenItem.PutBefore) > 0 then
      begin
        lNewControl := GUI.FindGUIControl(lCurControl.ScreenItem.PutBefore);

        Assert(Assigned(lNewControl), 'PutBefore property: ' +
          lCurControl.ScreenItem.PutBefore + ', does not exists');

        iNewIndex := MainLayoutManager.GetItemIndexByControlName(lNewControl.Name);

        if (iNewIndex >= 0) and (iNewIndex > iCurIndex) then
          Dec(iNewIndex);
      end
      else if Length(lCurControl.ScreenItem.PutAfter) > 0 then
      begin
        lNewControl := GUI.FindGUIControl(lCurControl.ScreenItem.PutAfter);

        Assert(Assigned(lNewControl), 'PutAfter property: ' +
          lCurControl.ScreenItem.PutAfter + ', does not exists');

        iNewIndex := MainLayoutManager.GetItemIndexByControlName(lNewControl.Name);

        if (iNewIndex < MainLayoutManager.ItemList.Count - 1) and (iNewIndex < iCurIndex) then
          Inc(iNewIndex);
      end;

      if (iCurIndex > -1) and (iNewIndex > -1) then
        MainLayoutManager.ItemList.Move(iCurIndex, iNewIndex);
    end;

    It.Next;
  end;
end;

procedure TInfraGUIBuilderForm.SetObjectAnnotationsForForm(pScreen: IScreen);
begin
  if not pScreen.Title.IsNull then
    Caption := pScreen.Title.AsString;

  if not pScreen.Height.IsNull then
  begin
    Height := pScreen.Height.AsInteger;
    FAdjustFormSize := False;
  end;

  if not pScreen.Width.IsNull then
  begin
    Width := pScreen.Width.AsInteger;
    FAdjustFormSize := False;
  end;

  if pScreen.CaptionPosition <> MainLayoutManager.ItemDefCaptionPos then
    MainLayoutManager.ItemDefCaptionPos := pScreen.CaptionPosition;

  if not pScreen.ControlSpacing.IsNull then
    MainLayoutManager.ItemDefControlSpacing :=
      pScreen.ControlSpacing.AsInteger;

  if not TLayoutManagerPositions.Equals(pScreen.Padding,
    MainLayoutManager.ItemDefPadding) then
    MainLayoutManager.ItemDefPadding.Assign(pScreen.Padding);

  if pScreen.ItemLayout <> MainLayoutManager.ItemLayout then
    MainLayoutManager.ItemLayout := pScreen.ItemLayout;

  if not TLayoutManagerPositions.Equals(pScreen.ItemSpacing,
    MainLayoutManager.ItemSpacing) then
    MainLayoutManager.ItemSpacing.Assign(pScreen.ItemSpacing);
end;

procedure TInfraGUIBuilderForm.SetPropAnnotationsForItem(
  pScreenItem: IScreenItem; pItem: TLayoutManagerItem);
begin
  if not Assigned(pScreenItem) then
    Exit;

  if not pScreenItem.Caption.IsNull then
  begin
    pItem.Caption := pScreenItem.Caption.AsString;

    if pItem.ItemControl is TCheckBox then
      (pItem.ItemControl as TCheckBox).Caption := pScreenItem.Caption.AsString;
  end;

  if pScreenItem.CaptionPositionChanged then
    pItem.CaptionOptions.Position := pScreenItem.CaptionPosition;

  if not pScreenItem.CaptionVisible.IsNull then
    pItem.CaptionVisible := pScreenItem.CaptionVisible.AsBoolean;

  if pScreenItem.ItemHeightMeasureTypeChanged then
    pItem.HeightOptions.MeasureType := pScreenItem.ItemHeightMeasureType;

  if not pScreenItem.ItemHeight.IsNull then
    pItem.HeightOptions.Size := pScreenItem.ItemHeight.AsInteger;

  if pScreenItem.ItemWidthMeasureTypeChanged then
    pItem.WidthOptions.MeasureType := pScreenItem.ItemWidthMeasureType;

  if not pScreenItem.ItemWidth.IsNull then
    pItem.WidthOptions.Size := pScreenItem.ItemWidth.AsInteger;

  if not pScreenItem.Visible.IsNull then
    pItem.Visible := pScreenItem.Visible.AsBoolean;

  if Supports(pScreenItem, IScreenControl) then
  begin
    if not (pScreenItem as IScreenControl).Height.IsNull then
    begin
      pItem.ItemControl.Height := (pScreenItem as IScreenControl).Height.AsInteger;
      pItem.ResizeItemHeight;
    end;

    if not (pScreenItem as IScreenControl).Width.IsNull then
    begin
      pItem.ItemControl.Width := (pScreenItem as IScreenControl).Width.AsInteger;
      pItem.ResizeItemWidth;
    end;
  end;
end;

procedure TInfraGUIBuilderForm.SetScreenSizeRatio(const Value: TScreenSizeRatio);
begin
  FScreenSizeRatio := Value;
end;

procedure TInfraGUIBuilderForm.SetValueFromControl(pGUIControl: IGUIControl;
  pObject: IInfraObject);
begin
  if pGUIControl.Control is TMemo then
    SetVariantValue(pGUIControl.PropertyValue, (pGUIControl.Control as TMemo).Lines.Text)
  else if Length(pGUIControl.ControlProperty) > 0 then
    SetVariantValue(pGUIControl.PropertyValue,
      GetPropValue(pGUIControl.Control, pGUIControl.ControlProperty));
end;

procedure TInfraGUIBuilderForm.SetValueToControl(pGUIControl: IGUIControl;
  pObject: IInfraObject);
begin
  if pGUIControl.Control is TMemo then
    (pGUIControl.Control as TMemo).Lines.Text :=
      GetVariantValue(pGUIControl.PropertyValue).AsVariant
  else if Length(pGUIControl.ControlProperty) > 0 then
    SetPropValue(pGUIControl.Control, pGUIControl.ControlProperty,
      GetVariantValue(pGUIControl.PropertyValue).AsVariant);
end;

procedure TInfraGUIBuilderForm.WMSYSCOMMAND(var message: TWMSYSCOMMAND);
var
  CustomizeScreen: TCustomizeScreen;
begin
  inherited;

  case message.CmdType of
    1101:
      begin
        CustomizeScreen := TCustomizeScreen.Create(nil);

        try
          CustomizeScreen.GUI := GUI;
          CustomizeScreen.Form := Self;

          if CustomizeScreen.Execute then
          begin
            GUI := CustomizeScreen.GUI;
            Build;
          end;

        finally
          CustomizeScreen.Free;
        end;
      end;
  end;
end;

{ TInfraGUIBuilderEditForm }

constructor TInfraGUIBuilderEditForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;

  HandleNeeded;

  CanClose := False;
  FPanelPosition := ppBelow;

  FLayoutManager := TLayoutManager.Create(Self);
  FLayoutManager.Parent := Self;
  FLayoutManager.Height := 40;

  FLayoutManager.ItemDefPadding.Left := 2;
  FLayoutManager.ItemDefPadding.Right := 2;
  FLayoutManager.ItemDefPadding.Top := 2;
  FLayoutManager.ItemDefPadding.Bottom := 2;

  FLayoutManager.ItemSpacing.Right := 0;

  //Save button
  FSaveButton := TButton.Create(Self);
  FSaveButton.Name := 'SaveButton';
  FSaveButton.Caption := '&Save';
  FSaveButton.OnClick := DoSaveButtonClick;

  with FLayoutManager.AddControl(FSaveButton) do
  begin
    HeightOptions.Size := 30;
    WidthOptions.Size := 80;
  end;

  //Cancel button
  FCancelButton := TButton.Create(Self);
  FCancelButton.Name := 'CancelButton';
  FCancelButton.Caption := '&Cancel';
  FCancelButton.OnClick := DoCancelButtonClick;

  with FLayoutManager.AddControl(FCancelButton) do
  begin
    HeightOptions.Size := 30;
    WidthOptions.Size := 80;
  end;

  RealignForm;
end;

procedure TInfraGUIBuilderEditForm.DoCancelButtonClick(Sender: TObject);
begin
  FCanClose := True;
  FGUIResult := grCancel;

  Close;
end;

procedure TInfraGUIBuilderEditForm.DoSaveButtonClick(Sender: TObject);
var
  It: IGUIControlIterator;
begin
  It := GUI.GUIControlList.NewIterator;

  while not It.IsDone do
  begin
    SetValueFromControl(It.CurrentItem as IGUIControl, GUI.BusinessObject);

    It.Next;
  end;

  FCanClose := True;
  FGUIResult := grSave;

  Close;
end;

function TInfraGUIBuilderEditForm.GetAdditionalScreenHeight: Integer;
begin
  Result := 0;

  if PanelPosition in [ppAbove, ppBelow] then
    Result := LayoutManager.Height;
end;

function TInfraGUIBuilderEditForm.GetAdditionalScreenWidth: Integer;
begin
  Result := 0;
end;

function TInfraGUIBuilderEditForm.GetCancelButton: TButton;
begin
  Result := FCancelButton;
end;

function TInfraGUIBuilderEditForm.GetLayoutManager: TLayoutManager;
begin
  Result := FLayoutManager;
end;

function TInfraGUIBuilderEditForm.GetPanelPosition: TPanelPosition;
begin
  Result := FPanelPosition;
end;

function TInfraGUIBuilderEditForm.GetSaveButton: TButton;
begin
  Result := FSaveButton;
end;

procedure TInfraGUIBuilderEditForm.RealignForm;
begin
  if PanelPosition = ppAbove then
    LayoutManager.Align := alTop
  else
    LayoutManager.Align := alBottom;

  if PanelPosition in [ppAbove, ppBelow] then
    Constraints.MinWidth := 175;
end;

procedure TInfraGUIBuilderEditForm.SetPanelPosition(const Value: TPanelPosition);
begin
  if FPanelPosition <> Value then
    FPanelPosition := Value;
end;

end.
