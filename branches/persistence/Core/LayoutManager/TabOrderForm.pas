unit TabOrderForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, LayoutManager, StdCtrls, Buttons, ExtCtrls, ComCtrls, CommCtrl,
  ActnList;

type
  TTabOrderForm = class(TForm)
    pnlBottom: TPanel;
    pnlClient: TPanel;
    btbtCancel: TButton;
    btbtOK: TButton;
    pnlRigthButtons: TPanel;
    btbtUp: TSpeedButton;
    btbtDown: TSpeedButton;
    lsvTabOrder: TListView;
    actlActions: TActionList;
    actnClose: TAction;
    procedure FormShow(Sender: TObject);
    procedure btbtCancelClick(Sender: TObject);
    procedure btbtOKClick(Sender: TObject);
    procedure btbtUpClick(Sender: TObject);
    procedure btbtDownClick(Sender: TObject);
    procedure actnCloseExecute(Sender: TObject);
  private
    FExecute: Boolean;
    FItemList: TLayoutManagerItemList;
    function GetItemList: TLayoutManagerItemList;
    procedure SetItemList(const Value: TLayoutManagerItemList);
  protected
    procedure FillTabOrderList;
    procedure MoveItem(AItemIndex, ANewIndex: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
    property ItemList: TLayoutManagerItemList read GetItemList write SetItemList;
  end;

implementation

{$R *.dfm}

{ TTabOrderForm }

procedure TTabOrderForm.actnCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TTabOrderForm.btbtCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTabOrderForm.btbtDownClick(Sender: TObject);
begin
  if lsvTabOrder.ItemIndex < (lsvTabOrder.Items.Count - 1) then
    MoveItem(lsvTabOrder.ItemIndex, lsvTabOrder.ItemIndex + 1);
end;

procedure TTabOrderForm.btbtOKClick(Sender: TObject);
var
  I, iIndex: Integer;
begin
  for I := 0 to lsvTabOrder.Items.Count - 1 do
  begin
    iIndex := ItemList.IndexOf(lsvTabOrder.Items[I].Data);

    if iIndex <> I then
      ItemList.Move(iIndex, I);
  end;

  FExecute := True;

  Close;
end;

procedure TTabOrderForm.btbtUpClick(Sender: TObject);
begin
  if lsvTabOrder.ItemIndex > 0 then
    MoveItem(lsvTabOrder.ItemIndex, lsvTabOrder.ItemIndex - 1);
end;

constructor TTabOrderForm.Create(AOwner: TComponent);
begin
  inherited;

  FExecute := False;
end;

function TTabOrderForm.Execute: Boolean;
begin
  ShowModal;

  Result := FExecute;
end;

procedure TTabOrderForm.FillTabOrderList;
var
  I: Integer;
  lListItem : TListItem;
begin
  if (Assigned(ItemList)) and (ItemList.Count > 0) then
  begin
    lsvTabOrder.Items.BeginUpdate;
    lsvTabOrder.Clear;

    for I := 0 to ItemList.Count - 1 do
    begin
      lListItem := lsvTabOrder.Items.Add;
      lListItem.Data := ItemList.Items[I];
      lListItem.Caption := ItemList.Items[I].Name;
      lListItem.SubItems.Add(ItemList.Items[I].Caption);

      if Assigned(ItemList.Items[I].ItemControl) then
        lListItem.SubItems.Add(ItemList.Items[I].ItemControl.ClassName);
    end;

    lsvTabOrder.Items.EndUpdate;
    lsvTabOrder.ItemIndex := 0;
    lsvTabOrder.SetFocus;
  end;
end;

procedure TTabOrderForm.FormShow(Sender: TObject);
begin
  FillTabOrderList;
end;

function TTabOrderForm.GetItemList: TLayoutManagerItemList;
begin
  Result := FItemList;
end;

procedure TTabOrderForm.MoveItem(AItemIndex, ANewIndex: Integer);
var
  lItemFrom, lItemTo: TListItem;
begin
  lsvTabOrder.Items.BeginUpdate;

  try
    lItemFrom := TListItem.Create(lsvTabOrder.Items);
    lItemFrom.Assign(lsvTabOrder.Items[AItemIndex]);

    lsvTabOrder.Items.Delete(AItemIndex);

    lItemTo := lsvTabOrder.Items.Insert(ANewIndex);
    lItemTo.Assign(lItemFrom);

    //Force new item to be visible
    ListView_EnsureVisible(lsvTabOrder.Handle, ANewIndex, True);
  finally
    lsvTabOrder.Items.EndUpdate;
  end;

  lsvTabOrder.ItemIndex := ANewIndex;
end;

procedure TTabOrderForm.SetItemList(const Value: TLayoutManagerItemList);
begin
  FItemList := Value;
end;

end.
