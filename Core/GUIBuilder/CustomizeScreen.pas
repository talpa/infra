unit CustomizeScreen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, InfraGUIBuilderIntf, ComCtrls, StdCtrls, ExtCtrls, List_GUIControl,
  ActnList, Mask, LayoutManager, InfraGUIBuilderForm, GUIAnnotationLoaderIntf;

type
  TCustomizeScreen = class(TForm)
    lsvControlList: TListView;
    pnlBottom: TPanel;
    btbtCancel: TButton;
    btbtOK: TButton;
    btbtEditControl: TButton;
    actlActions: TActionList;
    actnClose: TAction;
    LayoutManager1: TLayoutManager;
    editTitle: TEdit;
    Title: TLayoutManagerItem;
    combCaptionPosition: TComboBox;
    CaptionPosition: TLayoutManagerItem;
    editHeight: TMaskEdit;
    Height: TLayoutManagerItem;
    editWidth: TMaskEdit;
    Width: TLayoutManagerItem;
    combItemLayout: TComboBox;
    ItemLayout: TLayoutManagerItem;
    editPaddingLeft: TMaskEdit;
    PaddingLeft: TLayoutManagerItem;
    editPaddingTop: TMaskEdit;
    PaddingTop: TLayoutManagerItem;
    editPaddingRight: TMaskEdit;
    PaddingRight: TLayoutManagerItem;
    editPaddingBottom: TMaskEdit;
    PaddingBottom: TLayoutManagerItem;
    procedure FormShow(Sender: TObject);
    procedure actnCloseExecute(Sender: TObject);
    procedure btbtCancelClick(Sender: TObject);
    procedure btbtEditControlClick(Sender: TObject);
    procedure btbtOKClick(Sender: TObject);
    procedure lsvControlListDblClick(Sender: TObject);
  private
    FExecute: Boolean;
    FForm: TInfraGUIBuilderForm;
    FGUI: IGUI;
    FGUIOriginal: IGUI;
    function GetGUI: IGUI;
    procedure SetGUI(const Value: IGUI);
    function GetForm: TInfraGUIBuilderForm;
    procedure SetForm(const Value: TInfraGUIBuilderForm);
  protected
    procedure InterfarceToObject;
    procedure ObjectToInterface;
  public
    function Execute: Boolean;
    property Form: TInfraGUIBuilderForm read GetForm write SetForm;
    property GUI: IGUI read GetGUI write SetGUI;
  end;

implementation

uses
  CustomizeScreenControl, InfraGUIBuilder, GUIAnnotation,
  GUIAnnotationLoaderXML;

{$R *.dfm}

{ TCustomizeScreen }

procedure TCustomizeScreen.actnCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TCustomizeScreen.btbtCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TCustomizeScreen.btbtEditControlClick(Sender: TObject);
var
  CustomizeScreenControl: TCustomizeScreenControl;
begin
  if lsvControlList.ItemIndex >= 0 then
  begin
    CustomizeScreenControl := TCustomizeScreenControl.Create(nil);

    try
      CustomizeScreenControl.GUIControl := GUI.GUIControlList.Items[lsvControlList.ItemIndex];

      if CustomizeScreenControl.Execute then
      begin
        GUI.GUIControlList.Items[lsvControlList.ItemIndex] := CustomizeScreenControl.GUIControl;
        ObjectToInterface;

      end;
    finally
      CustomizeScreenControl.Free;
    end;
  end;
end;

procedure TCustomizeScreen.btbtOKClick(Sender: TObject);
var
  lXMLLoader: IGUIAnnotationLoader;
begin
  InterfarceToObject;

  FGUIOriginal := GUI;

  FExecute := True;

  lXMLLoader := TGUIAnnotationLoaderXML.Create;
  lXMLLoader.FileName := GUI.GetConfigurationFileName + '.xml';
  lXMLLoader.GUI := FGUIOriginal;
  lXMLLoader.Save;

  Close;
end;

function TCustomizeScreen.Execute: Boolean;
begin
  ShowModal;

  Result := FExecute;
end;

procedure TCustomizeScreen.FormShow(Sender: TObject);
begin
  ObjectToInterface;
end;

function TCustomizeScreen.GetForm: TInfraGUIBuilderForm;
begin
  Result := FForm;
end;

function TCustomizeScreen.GetGUI: IGUI;
begin
  Result := FGUI;
end;

procedure TCustomizeScreen.InterfarceToObject;
var
  vValue: Variant;
  lpValue: TLabelPosition;
  laValue: TLayoutOrientation;

  procedure VerityScreenInstance;
  begin
    if not Assigned(GUI.Screen) then
      GUI.Screen := TScreen.Create;
  end;

begin
  //Title
  vValue := editTitle.Text;

  if (Assigned(GUI.Screen)) and
    (not GUI.Screen.Title.IsNull) and
    (GUI.Screen.Title.AsString <> vValue) or
    (Form.Caption <> vValue) then
  begin
    VerityScreenInstance;
    GUI.Screen.Title.AsString := vValue;
  end;

  //CaptionPosition
  case combCaptionPosition.ItemIndex of
    0: lpValue := lpAbove;
    1: lpValue := lpBelow;
    3: lpValue := lpRight;
  else
    lpValue := lpLeft;
  end;

  if (Assigned(GUI.Screen)) and
    (GUI.Screen.CaptionPosition <> lpValue) or
    (Form.MainLayoutManager.ItemDefCaptionPos <> lpValue) then
  begin
    VerityScreenInstance;
    GUI.Screen.CaptionPosition := lpValue;
  end;

  //Height
  vValue := StrToIntDef(Trim(editHeight.Text), 0);

  if (Assigned(GUI.Screen)) and
    (not GUI.Screen.Height.IsNull) and
    (GUI.Screen.Height.AsInteger <> vValue) or
    (Form.Height <> vValue) then
  begin
    VerityScreenInstance;
    GUI.Screen.Height.AsInteger := vValue;
  end;

  //Width
  vValue := StrToIntDef(Trim(editWidth.Text), 0);

  if (Assigned(GUI.Screen)) and
    (not GUI.Screen.Width.IsNull) and
    (GUI.Screen.Width.AsInteger <> vValue) or
    (Form.Width <> vValue) then
  begin
    VerityScreenInstance;
    GUI.Screen.Width.AsInteger := vValue;
  end;

  //ItemLayout
  if combItemLayout.ItemIndex = 1 then
    laValue := laVertical
  else
    laValue := laHorizontal;

  if (Assigned(GUI.Screen)) and
    (GUI.Screen.ItemLayout <> laValue) or
    (Form.MainLayoutManager.ItemLayout <> laValue) then
  begin
    VerityScreenInstance;
    GUI.Screen.ItemLayout := laValue;
  end;

  //Padding - Left
  vValue := StrToIntDef(Trim(editPaddingLeft.Text), 0);

  if (Assigned(GUI.Screen)) and
    (GUI.Screen.Padding.Left <> vValue) or
    (Form.MainLayoutManager.ItemDefPadding.Left <> vValue) then
  begin
    VerityScreenInstance;
    GUI.Screen.Padding.Left := vValue;
  end;

  //Padding - Top
  vValue := StrToIntDef(Trim(editPaddingTop.Text), 0);

  if (Assigned(GUI.Screen)) and
    (GUI.Screen.Padding.Top <> vValue) or
    (Form.MainLayoutManager.ItemDefPadding.Top <> vValue) then
  begin
    VerityScreenInstance;
    GUI.Screen.Padding.Top := vValue;
  end;

  //Padding - Right
  vValue := StrToIntDef(Trim(editPaddingRight.Text), 0);

  if (Assigned(GUI.Screen)) and
    (GUI.Screen.Padding.Right <> vValue) or
    (Form.MainLayoutManager.ItemDefPadding.Right <> vValue) then
  begin
    VerityScreenInstance;
    GUI.Screen.Padding.Right := vValue;
  end;

  //Padding - Bottom
  vValue := StrToIntDef(Trim(editPaddingBottom.Text), 0);

  if (Assigned(GUI.Screen)) and
    (GUI.Screen.Padding.Bottom <> vValue) or
    (Form.MainLayoutManager.ItemDefPadding.Bottom <> vValue) then
  begin
    VerityScreenInstance;
    GUI.Screen.Padding.Bottom := vValue;
  end;
end;

procedure TCustomizeScreen.lsvControlListDblClick(Sender: TObject);
begin
  btbtEditControl.Click;
end;

procedure TCustomizeScreen.ObjectToInterface;
var
  iIndex: Integer;
  It: IGUIControlIterator;
  lListItem : TListItem;
  lGUIControl: IGUIControl;
  sSufix: string;


  function GetSufix(pMeasureType: TMeasureType): string;
  begin
    if pMeasureType = mtFix then
      Result := 'px'
    else
      Result := '%';
  end;


begin
  if Assigned(GUI.Screen) then
    editTitle.Text := GUI.Screen.Title.AsString
  else
    editTitle.Text := GUI.Title;

  if (Assigned(GUI.Screen)) and (GUI.Screen.CaptionPosition <> lpLeft) then
  begin
    case GUI.Screen.CaptionPosition of
      lpAbove: combCaptionPosition.ItemIndex := 0;
      lpBelow: combCaptionPosition.ItemIndex := 1;
      lpRight: combCaptionPosition.ItemIndex := 3;
    end;
  end
  else
    combCaptionPosition.ItemIndex := 2;

  if (Assigned(GUI.Screen)) and (not GUI.Screen.Height.IsNull) then
    editHeight.Text := IntToStr(GUI.Screen.Height.AsInteger)
  else
    editHeight.Text := IntToStr(Form.Height);

  if (Assigned(GUI.Screen)) and (not GUI.Screen.Width.IsNull) then
    editWidth.Text := IntToStr(GUI.Screen.Width.AsInteger)
  else
    editWidth.Text := IntToStr(Form.Width);

  if (Assigned(GUI.Screen)) and (GUI.Screen.ItemLayout <> laHorizontal) then
    combItemLayout.ItemIndex := 1
  else
    combItemLayout.ItemIndex := 0;

  if Assigned(GUI.Screen) then
    editPaddingLeft.Text := IntToStr(GUI.Screen.Padding.Left)
  else
    editPaddingLeft.Text := IntToStr(Form.MainLayoutManager.ItemDefPadding.Left);

  if Assigned(GUI.Screen) then
    editPaddingTop.Text := IntToStr(GUI.Screen.Padding.Top)
  else
    editPaddingTop.Text := IntToStr(Form.MainLayoutManager.ItemDefPadding.Top);

  if Assigned(GUI.Screen) then
    editPaddingRight.Text := IntToStr(GUI.Screen.Padding.Right)
  else
    editPaddingRight.Text := IntToStr(Form.MainLayoutManager.ItemDefPadding.Right);

  if Assigned(GUI.Screen) then
    editPaddingBottom.Text := IntToStr(GUI.Screen.Padding.Bottom)
  else
    editPaddingBottom.Text := IntToStr(Form.MainLayoutManager.ItemDefPadding.Bottom);

  if Assigned(GUI) then
  begin
    iIndex := lsvControlList.ItemIndex;

    lsvControlList.Items.BeginUpdate;
    lsvControlList.Clear;

    It := GUI.GUIControlList.NewIterator;

    while not It.IsDone do
    begin
      lGUIControl := It.CurrentItem as IGUIControl;

      lListItem := lsvControlList.Items.Add;
      lListItem.Caption := lGUIControl.PropertyName;

      if (Assigned(lGUIControl.ScreenItem)) and
        (not lGUIControl.ScreenItem.Caption.IsNull) then
        lListItem.SubItems.Add(lGUIControl.ScreenItem.Caption.AsString)
      else
        lListItem.SubItems.Add(lGUIControl.PropertyName);

      if (Assigned(lGUIControl.ScreenItem)) and
        (not lGUIControl.ScreenItem.Visible.AsBoolean) and
        (not lGUIControl.ScreenItem.Visible.IsNull) then
        lListItem.SubItems.Add('False')
      else
        lListItem.SubItems.Add('True');

      if (Assigned(lGUIControl.ScreenItem)) and
        (not lGUIControl.ScreenItem.CaptionVisible.IsNull) then
      begin
        if lGUIControl.ScreenItem.CaptionVisible.AsBoolean then
          lListItem.SubItems.Add('True')
        else
          lListItem.SubItems.Add('False');
      end
      else
      begin
        if lGUIControl.Item.CaptionVisible then
          lListItem.SubItems.Add('True')
        else
          lListItem.SubItems.Add('False');
      end;

      if Assigned(lGUIControl.ScreenItem) then
        lListItem.SubItems.Add(lGUIControl.ScreenItem.PutAfter)
      else
        lListItem.SubItems.Add('');

      if Assigned(lGUIControl.ScreenItem) then
        lListItem.SubItems.Add(lGUIControl.ScreenItem.PutBefore)
      else
        lListItem.SubItems.Add('');

      if Assigned(lGUIControl.ScreenItem) then
        sSufix := GetSufix(lGUIControl.ScreenItem.ItemHeightMeasureType)
      else
        sSufix := GetSufix(lGUIControl.Item.HeightOptions.MeasureType);

      if (Assigned(lGUIControl.ScreenItem)) and (not lGUIControl.ScreenItem.ItemHeight.IsNull) then
        lListItem.SubItems.Add(IntToStr(lGUIControl.ScreenItem.ItemHeight.AsInteger) + sSufix)
      else
        lListItem.SubItems.Add(FloatToStr(lGUIControl.Item.HeightOptions.Size) + sSufix);

      if Assigned(lGUIControl.ScreenItem) then
        sSufix := GetSufix(lGUIControl.ScreenItem.ItemWidthMeasureType)
      else
        sSufix := GetSufix(lGUIControl.Item.WidthOptions.MeasureType);

      if (Assigned(lGUIControl.ScreenItem)) and (not lGUIControl.ScreenItem.ItemWidth.IsNull) then
        lListItem.SubItems.Add(IntToStr(lGUIControl.ScreenItem.ItemWidth.AsInteger) + sSufix)
      else
        lListItem.SubItems.Add(FloatToStr(lGUIControl.Item.WidthOptions.Size) + sSufix);

      It.Next;
    end;

    lsvControlList.Items.EndUpdate;
    lsvControlList.ItemIndex := iIndex;
    lsvControlList.SetFocus;
  end;
end;

procedure TCustomizeScreen.SetForm(const Value: TInfraGUIBuilderForm);
begin
  FForm := Value;
end;

procedure TCustomizeScreen.SetGUI(const Value: IGUI);
begin
  FGUIOriginal := Value;
  FGUI := Value.Clone;
end;

end.
