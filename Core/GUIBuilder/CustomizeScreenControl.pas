unit CustomizeScreenControl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, LayoutManager, StdCtrls, ExtCtrls, ActnList, InfraGUIBuilderIntf,
  InfraGUIBuilder, Mask, GUIAnnotationIntf, GUIAnnotation;

type
  TCustomizeScreenControl = class(TForm)
    pnlBottom: TPanel;
    btbtCancel: TButton;
    btbtOK: TButton;
    LayoutManager1: TLayoutManager;
    editCaption: TEdit;
    Caption: TLayoutManagerItem;
    actlActions: TActionList;
    actnClose: TAction;
    ckbxVisible: TCheckBox;
    Visible: TLayoutManagerItem;
    editItemHeight: TMaskEdit;
    ItemHeight: TLayoutManagerItem;
    editItemWidth: TMaskEdit;
    ItemWidth: TLayoutManagerItem;
    combCaptionPosition: TComboBox;
    CaptionPosition: TLayoutManagerItem;
    ckbxCaptionVisible: TCheckBox;
    CaptionVisible: TLayoutManagerItem;
    combItemHeightMeasureType: TComboBox;
    ItemHeightMeasureType: TLayoutManagerItem;
    ItemWidthMeasureType: TLayoutManagerItem;
    combItemWidthMeasureType: TComboBox;
    editName: TEdit;
    Name: TLayoutManagerItem;
    editPutAfter: TEdit;
    PutAfter: TLayoutManagerItem;
    editPutBefore: TEdit;
    PutBefore: TLayoutManagerItem;
    procedure actnCloseExecute(Sender: TObject);
    procedure btbtCancelClick(Sender: TObject);
    procedure btbtOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FExecute: Boolean;
    FGUIControl: IGUIControl;
    function GetGUIControl: IGUIControl;
    procedure SetGUIControl(const Value: IGUIControl);
  protected
    procedure InterfaceToObject;
    procedure ObjectToInterface;
  public
    function Execute: Boolean;
    property GUIControl: IGUIControl read GetGUIControl write SetGUIControl;
  end;

implementation

{$R *.dfm}

procedure TCustomizeScreenControl.actnCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TCustomizeScreenControl.btbtCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TCustomizeScreenControl.btbtOKClick(Sender: TObject);
begin
  InterfaceToObject;

  FExecute := True;

  Close;
end;

function TCustomizeScreenControl.Execute: Boolean;
begin
  ShowModal;

  Result := FExecute;
end;

procedure TCustomizeScreenControl.FormShow(Sender: TObject);
begin
  //Height.Enabled := Supports(GUIControl.ScreenItem, IScreenControl) or
  // (not Assigned(GUIControl.ScreenItem));
  //Width.Enabled := Supports(GUIControl.ScreenItem, IScreenControl) or
  // (not Assigned(GUIControl.ScreenItem));

  ObjectToInterface;
end;

function TCustomizeScreenControl.GetGUIControl: IGUIControl;
begin
  Result := FGUIControl;
end;

procedure TCustomizeScreenControl.InterfaceToObject;
var
  //bAssign: Boolean;
  vValue: Variant;
  lpValue: TLabelPosition;
  mtValue: TMeasureType;

  procedure VerityScreenControlInstance;
  begin
    if not Assigned(GUIControl.ScreenItem) then
      GUIControl.ScreenItem := TScreenControl.Create;
  end;

begin
  //Visible
  vValue := ckbxVisible.Checked;

  if (Assigned(GUIControl.ScreenItem)) and
    (not GUIControl.ScreenItem.Visible.IsNull) and
    (GUIControl.ScreenItem.Visible.AsBoolean <> vValue) or
    (GUIControl.Item.Visible <> vValue) then
  begin
    VerityScreenControlInstance;
    GUIControl.ScreenItem.Visible.AsBoolean := vValue;
  end;

  //Caption
  vValue := Trim(editCaption.Text);

  if (Assigned(GUIControl.ScreenItem)) and
    (not GUIControl.ScreenItem.Caption.IsNull) and
    (GUIControl.ScreenItem.Caption.AsString <> vValue) or
    (GUIControl.Item.Caption <> vValue) then
  begin
    VerityScreenControlInstance;
    GUIControl.ScreenItem.Caption.AsString := vValue;
  end;

  //CaptionVisible
  vValue := ckbxCaptionVisible.Checked;

  if (Assigned(GUIControl.ScreenItem)) and
    (not GUIControl.ScreenItem.CaptionVisible.IsNull) and
    (GUIControl.ScreenItem.CaptionVisible.AsBoolean <> vValue) or
    (GUIControl.Item.CaptionVisible <> vValue) then
  begin
    VerityScreenControlInstance;
    GUIControl.ScreenItem.CaptionVisible.AsBoolean := vValue;
  end;

  //CaptionPosition
  lpValue := TLabelPosition(combCaptionPosition.ItemIndex);

  if (Assigned(GUIControl.ScreenItem)) and
    (GUIControl.ScreenItem.CaptionPosition <> lpValue) or
    (GUIControl.Item.CaptionOptions.Position <> lpValue) then
  begin
    VerityScreenControlInstance;
    GUIControl.ScreenItem.CaptionPosition := lpValue;
  end;

  //ItemHeight
  vValue := StrToIntDef(Trim(editItemHeight.Text), 0);

  if (Assigned(GUIControl.ScreenItem)) and
    (not GUIControl.ScreenItem.ItemHeight.IsNull) and
    (GUIControl.ScreenItem.ItemHeight.AsInteger <> vValue) or
    (GUIControl.Item.HeightOptions.Size <> vValue) then
  begin
    VerityScreenControlInstance;
    GUIControl.ScreenItem.ItemHeight.AsInteger := vValue;
  end;

  //ItemHeightMeasureType
  mtValue := TMeasureType(combItemHeightMeasureType.ItemIndex);

  if (Assigned(GUIControl.ScreenItem)) and
    (GUIControl.ScreenItem.ItemHeightMeasureType <> mtValue) or
    (GUIControl.Item.HeightOptions.MeasureType <> mtValue) then
  begin
    VerityScreenControlInstance;
    GUIControl.ScreenItem.ItemHeightMeasureType := mtValue;
  end;

  //ItemWidth
  vValue := StrToIntDef(Trim(editItemWidth.Text), 0);

  if (Assigned(GUIControl.ScreenItem)) and
    (not GUIControl.ScreenItem.ItemWidth.IsNull) and
    (GUIControl.ScreenItem.ItemWidth.AsInteger <> vValue) or
    (GUIControl.Item.WidthOptions.Size <> vValue) then
  begin
    VerityScreenControlInstance;
    GUIControl.ScreenItem.ItemWidth.AsInteger := vValue;
  end;

  //ItemWidthMeasureType
  mtValue := TMeasureType(combItemWidthMeasureType.ItemIndex);

  if (Assigned(GUIControl.ScreenItem)) and
    (GUIControl.ScreenItem.ItemWidthMeasureType <> mtValue) or
    (GUIControl.Item.WidthOptions.MeasureType <> mtValue) then
  begin
    VerityScreenControlInstance;
    GUIControl.ScreenItem.ItemWidthMeasureType := mtValue;
  end;

  //Height
  {bAssign := False;
  vValue := StrToIntDef(Trim(editHeight.Text), 0);

  if (Assigned(GUIControl.ScreenItem)) and
    (Supports(GUIControl.ScreenItem, IScreenControl) and
    (not (GUIControl.ScreenItem as IScreenControl).Height.IsNull) and
    ((GUIControl.ScreenItem as IScreenControl).Height.AsInteger <> vValue)) then
    bAssign := True
  else if (not (Assigned(GUIControl.ScreenItem))) or
    ((Assigned(GUIControl.ScreenItem)) and
    (GUIControl.ScreenItem.ItemHeightMeasureType <> mtPercent)) and
    (GUIControl.Control.Height <> vValue) then
    bAssign := True;

  if bAssign then
  begin
    VerityScreenControlInstance;
    (GUIControl.ScreenItem as IScreenControl).Height.AsInteger := vValue;
  end; }

  //Width
  {bAssign := False;
  vValue := StrToIntDef(Trim(editWidth.Text), 0);

  if (Assigned(GUIControl.ScreenItem)) and
    (Supports(GUIControl.ScreenItem, IScreenControl) and
    (not (GUIControl.ScreenItem as IScreenControl).Width.IsNull) and
    ((GUIControl.ScreenItem as IScreenControl).Width.AsInteger <> vValue)) then
    bAssign := True
  else if (not (Assigned(GUIControl.ScreenItem))) or
    ((Assigned(GUIControl.ScreenItem)) and
    (GUIControl.ScreenItem.ItemWidthMeasureType <> mtPercent)) and
    (GUIControl.Control.Width <> vValue) then
    bAssign := True;

  if bAssign then
  begin
    VerityScreenControlInstance;
    (GUIControl.ScreenItem as IScreenControl).Width.AsInteger := vValue;
  end;}

  //PutAfter
  vValue := Trim(editPutAfter.Text);

  if Assigned(GUIControl.ScreenItem) and
    (GUIControl.ScreenItem.PutAfter <> vValue) then
  begin
    VerityScreenControlInstance;
    GUIControl.ScreenItem.PutBefore := '';
    GUIControl.ScreenItem.PutAfter := vValue;
  end;

  //PutBefore
  vValue := Trim(editPutBefore.Text);

  if Assigned(GUIControl.ScreenItem) and
    (GUIControl.ScreenItem.PutBefore <> vValue) then
  begin
    VerityScreenControlInstance;
    GUIControl.ScreenItem.PutAfter := '';
    GUIControl.ScreenItem.PutBefore := vValue;
  end;
end;

procedure TCustomizeScreenControl.ObjectToInterface;
begin
  editName.Text := GUIControl.PropertyName;

  if (Assigned(GUIControl.ScreenItem)) and (not GUIControl.ScreenItem.Visible.IsNull) then
    ckbxVisible.Checked := GUIControl.ScreenItem.Visible.AsBoolean
  else
    ckbxVisible.Checked := True;

  if (Assigned(GUIControl.ScreenItem)) and (not GUIControl.ScreenItem.Caption.IsNull) then
    editCaption.Text := GUIControl.ScreenItem.Caption.AsString
  else
    editCaption.Text := GUIControl.PropertyName;

  if (Assigned(GUIControl.ScreenItem)) and (not GUIControl.ScreenItem.CaptionVisible.IsNull) then
    ckbxCaptionVisible.Checked := GUIControl.ScreenItem.CaptionVisible.AsBoolean
  else
    ckbxCaptionVisible.Checked := GUIControl.Item.CaptionVisible;

  if (Assigned(GUIControl.ScreenItem)) and (GUIControl.ScreenItem.CaptionPosition <> lpLeft) then
  begin
    case GUIControl.ScreenItem.CaptionPosition of
      lpAbove: combCaptionPosition.ItemIndex := 0;
      lpBelow: combCaptionPosition.ItemIndex := 1;
      lpRight: combCaptionPosition.ItemIndex := 3;
    end;
  end
  else
    combCaptionPosition.ItemIndex := 2;

  if (Assigned(GUIControl.ScreenItem)) and (not GUIControl.ScreenItem.ItemHeight.IsNull) then
    editItemHeight.Text := IntToStr(GUIControl.ScreenItem.ItemHeight.AsInteger)
  else
    editItemHeight.Text := FloatToStr(GUIControl.Item.HeightOptions.Size);

  if (Assigned(GUIControl.ScreenItem)) and (GUIControl.ScreenItem.ItemHeightMeasureType <> mtFix) then
    combItemHeightMeasureType.ItemIndex := 1
  else
    combItemHeightMeasureType.ItemIndex := 0;

  if (Assigned(GUIControl.ScreenItem)) and (not GUIControl.ScreenItem.ItemWidth.IsNull) then
    editItemWidth.Text := IntToStr(GUIControl.ScreenItem.ItemWidth.AsInteger)
  else
    editItemWidth.Text := FloatToStr(GUIControl.Item.WidthOptions.Size);

  if (Assigned(GUIControl.ScreenItem)) and (GUIControl.ScreenItem.ItemWidthMeasureType <> mtFix) then
    combItemWidthMeasureType.ItemIndex := 1
  else
    combItemWidthMeasureType.ItemIndex := 0;

  {if (Assigned(GUIControl.ScreenItem)) and (Supports(GUIControl.ScreenItem, IScreenControl)) and
    (not (GUIControl.ScreenItem as IScreenControl).Height.IsNull) then
    editHeight.Text := IntToStr((GUIControl.ScreenItem as IScreenControl).Height.AsInteger)
  else
    editHeight.Text := IntToStr(GUIControl.Control.Height);

  if (Assigned(GUIControl.ScreenItem)) and (Supports(GUIControl.ScreenItem, IScreenControl)) and
    (not (GUIControl.ScreenItem as IScreenControl).Width.IsNull) then
    editWidth.Text := IntToStr((GUIControl.ScreenItem as IScreenControl).Width.AsInteger)
  else
    editWidth.Text := IntToStr(GUIControl.Control.Width);}

  if Assigned(GUIControl.ScreenItem) then
    editPutAfter.Text := GUIControl.ScreenItem.PutAfter;

  if Assigned(GUIControl.ScreenItem) then
    editPutBefore.Text := GUIControl.ScreenItem.PutBefore;
end;

procedure TCustomizeScreenControl.SetGUIControl(const Value: IGUIControl);
begin
  FGUIControl := Value;
end;

end.
