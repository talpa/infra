unit CustomizeScreenControl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, LayoutManager, StdCtrls, ExtCtrls, ActnList, InfraGUIBuilderIntf,
  InfraGUIBuilder, Mask;

type
  TCustomizeScreenControl = class(TForm)
    pnlBottom: TPanel;
    btbtCancel: TButton;
    btbtOK: TButton;
    LayoutManager1: TLayoutManager;
    editCaption: TEdit;
    Caption: TLayoutManagerItem;
    editHeight: TMaskEdit;
    Height: TLayoutManagerItem;
    editWidth: TMaskEdit;
    Width: TLayoutManagerItem;
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
    constructor Create(AOwner: TComponent); override;
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
  FExecute := True;

  Close;
end;

constructor TCustomizeScreenControl.Create(AOwner: TComponent);
begin
  inherited;

  FGUIControl := TGUIControl.Create;
end;

function TCustomizeScreenControl.Execute: Boolean;
begin
  ShowModal;

  Result := FExecute;
end;

procedure TCustomizeScreenControl.FormShow(Sender: TObject);
begin
  ObjectToInterface;
end;

function TCustomizeScreenControl.GetGUIControl: IGUIControl;
begin
  Result := FGUIControl;
end;

procedure TCustomizeScreenControl.InterfaceToObject;
begin
  if Assigned(GUIControl.ScreenControl) then
  begin
    GUIControl.ScreenControl.Caption.AsString := editCaption.Text;
  end;
end;

procedure TCustomizeScreenControl.ObjectToInterface;
begin
  if Assigned(GUIControl.ScreenControl) then
  begin
    ckbxVisible.Checked := GUIControl.ScreenControl.Visible.AsBoolean;
    editCaption.Text := GUIControl.ScreenControl.Caption.AsString;
    ckbxCaptionVisible.Checked := GUIControl.ScreenControl.CaptionVisible.AsBoolean;

    case GUIControl.ScreenControl.CaptionPosition of
      lpAbove: combCaptionPosition.ItemIndex := 0;
      lpBelow: combCaptionPosition.ItemIndex := 1;
      lpLeft: combCaptionPosition.ItemIndex := 2;
      lpRight: combCaptionPosition.ItemIndex := 3;
    end;

    editHeight.Text := IntToStr(GUIControl.ScreenControl.Height.AsInteger);
    editWidth.Text := IntToStr(GUIControl.ScreenControl.Width.AsInteger);

    editItemHeight.Text := IntToStr(GUIControl.ScreenControl.ItemHeight.AsInteger);

    case GUIControl.ScreenControl.ItemHeightMeasureType of
      mtFix: combItemHeightMeasureType.ItemIndex := 0;
      mtPercent: combItemHeightMeasureType.ItemIndex := 1;
    end;

    editItemWidth.Text := IntToStr(GUIControl.ScreenControl.ItemWidth.AsInteger);

    case GUIControl.ScreenControl.ItemWidthMeasureType of
      mtFix: combItemWidthMeasureType.ItemIndex := 0;
      mtPercent: combItemWidthMeasureType.ItemIndex := 1;
    end;
  end;
end;

procedure TCustomizeScreenControl.SetGUIControl(const Value: IGUIControl);
begin
  FGUIControl := Value;
end;

end.
