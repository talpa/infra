unit CustomizeScreen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, InfraGUIBuilderIntf, ComCtrls, List_GUIControl, StdCtrls, ExtCtrls,
  ActnList;

type
  TCustomizeScreen = class(TForm)
    lsvControlList: TListView;
    pnlBottom: TPanel;
    btbtCancel: TButton;
    btbtOK: TButton;
    btbtEditControl: TButton;
    actlActions: TActionList;
    actnClose: TAction;
    procedure FormShow(Sender: TObject);
    procedure actnCloseExecute(Sender: TObject);
    procedure btbtCancelClick(Sender: TObject);
    procedure btbtEditControlClick(Sender: TObject);
  private
    FExecute: Boolean;
    FGUIControlList: IGUIControlList;
    function GetGUIControlList: IGUIControlList;
    procedure SetGUIControlList(const Value: IGUIControlList);
  protected
    procedure FillControlList;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
    property GUIControlList: IGUIControlList read GetGUIControlList write SetGUIControlList;
  end;

implementation

uses
  CustomizeScreenControl;

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

constructor TCustomizeScreen.Create(AOwner: TComponent);
begin
  inherited;

  FGUIControlList := TGUIControlList.Create;
end;

procedure TCustomizeScreen.btbtEditControlClick(Sender: TObject);
var
  CustomizeScreenControl: TCustomizeScreenControl;
begin
  if lsvControlList.ItemIndex >= 0 then
  begin
    CustomizeScreenControl := TCustomizeScreenControl.Create(nil);

    try
      CustomizeScreenControl.GUIControl := GUIControlList.Items[lsvControlList.ItemIndex];
      CustomizeScreenControl.Execute;
    finally
      CustomizeScreenControl.Free;
    end;
  end;
end;

function TCustomizeScreen.Execute: Boolean;
begin
  ShowModal;

  Result := FExecute;
end;

procedure TCustomizeScreen.FillControlList;
var
  I: Integer;
  lListItem : TListItem;
begin
  if (Assigned(GUIControlList)) and (GUIControlList.Count > 0) then
  begin
    lsvControlList.Items.BeginUpdate;
    lsvControlList.Clear;

    for I := 0 to GUIControlList.Count - 1 do
    begin
      lListItem := lsvControlList.Items.Add;
     // lListItem.Data := @ControlList.Items[I];
      lListItem.Caption := GUIControlList.Items[I].Name;

      if Assigned(GUIControlList.Items[I].ScreenControl) then
        lListItem.SubItems.Add(GUIControlList.Items[I].ScreenControl.Caption.AsString);

      if (Assigned(GUIControlList.Items[I].ScreenControl)) and
        (Assigned(GUIControlList.Items[I].ScreenControl.ControlClass)) then
        lListItem.SubItems.Add(GUIControlList.Items[I].ScreenControl.ControlClass.ClassName);
    end;

    lsvControlList.Items.EndUpdate;
    lsvControlList.ItemIndex := 0;
    lsvControlList.SetFocus;
  end;
end;

procedure TCustomizeScreen.FormShow(Sender: TObject);
begin
  FillControlList;
end;

function TCustomizeScreen.GetGUIControlList: IGUIControlList;
begin
  Result := FGUIControlList;
end;

procedure TCustomizeScreen.SetGUIControlList(const Value: IGUIControlList);
begin
  FGUIControlList := Value;
end;

end.
