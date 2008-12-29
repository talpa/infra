unit List_VCLMenuItem;

{$I Infra.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  ComCtrls,
  Graphics,
  Types,
  InfraCommonIntf,
  InfraMVPIntf,
  InfraMVPVCLIntf,
  InfraBasicList,
  InfraVCLView;

type
  _ITERABLELIST_BASE_ = TVCLView;
  _ITERABLELIST_INTF_ = IVCLMenuItemView;
  _ITEM_INTF_ = IVCLMenuItemView;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I ..\..\Templates\InfraTempl_IntfList.inc}
  private
    FClickCommand: ICommand;
  protected
    function GetClickCommand: ICommand;
    procedure OnClickHandle(Sender: TObject); virtual;
    procedure OnAdvancedDrawItemHandle(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; State: TOwnerDrawState); virtual;
    procedure OnDrawItemHandle(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      Selected: Boolean); virtual;
    procedure OnMeasureItemHandle(Sender: TObject; ACanvas: TCanvas; var Width,
      Height: Integer); virtual;
    procedure SetClickCommand(const Value: ICommand);
    property ClickCommand: ICommand read GetClickCommand write SetClickCommand;
  end;

  TVCLMenuItemView = Class(_ITERABLELIST_)
  private
    procedure ClickCommandEvent(const Event: IInfraEvent);
  protected
    procedure Update; override;
  public
    procedure SetObject(Value: TObject); override;
    procedure InfraInitInstance; override;
  end;


implementation

uses
  SysUtils,
  Menus,
  InfraFriendClasses;

{ TVCLMenuItemView_Base }

{$I ..\..\Templates\InfraTempl_IntfList.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function _ITERABLELIST_.GetClickCommand: ICommand;
begin
  Result := FClickCommand;
end;

procedure _ITERABLELIST_.OnAdvancedDrawItemHandle(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
begin
 (Renderer as IVCLMenuItemRenderer).DoAdvancedDrawItem(ACanvas, ARect,
    State);
end;

procedure _ITERABLELIST_.OnClickHandle(Sender: TObject);
begin
  // ***
  //  if Assigned(FClickCommand) then
  //    FClickCommand.Execute;
end;

procedure _ITERABLELIST_.OnDrawItemHandle(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
begin
  (Renderer as IVCLMenuItemRenderer).DoDrawItem(ACanvas,
    ARect, Selected);
end;

procedure _ITERABLELIST_.OnMeasureItemHandle(Sender: TObject;
  ACanvas: TCanvas; var Width, Height: Integer);
begin
  (Renderer as IVCLMenuItemRenderer).DoMeasureItem(ACanvas, Width, Height);
end;

procedure _ITERABLELIST_.SetClickCommand(const Value: ICommand);
begin
  if Value <> FClickCommand then
  begin
    FClickCommand := Value;
    Update;
  end;
end;

{ TVCLMenuItemView }

procedure TVCLMenuItemView.InfraInitInstance;
begin
  inherited;
  EventService.Subscribe(ICommandChangedEvent, Self as ISubscriber,
    ClickCommandEvent, 'ClickCommand');
end;

procedure TVCLMenuItemView.ClickCommandEvent(const Event: IInfraEvent);
begin
  if Assigned(VCLObject) then
    TMenuItemFriend(VCLObject).Enabled := ClickCommand.Enabled;
end;

procedure TVCLMenuItemView.SetObject(Value: TObject);
begin
  with TMenuItemFriend(VCLObject as TMenuItem) do
  begin
    if Assigned(VCLObject) then
    begin
      OnClick := nil;
      OnDrawItem := nil;
      OnAdvancedDrawItem := nil;
      OnMeasureItem := nil;
    end;
    inherited SetObject(Value);
    if Assigned(VCLObject) then
    begin
      OnClick := OnClickHandle;
      OnDrawItem := OnDrawItemHandle;
      OnAdvancedDrawItem := OnAdvancedDrawItemHandle;
      OnMeasureItem := OnMeasureItemHandle;
    end;
  end;
end;

procedure TVCLMenuItemView.Update;
begin
  inherited;
  if Assigned(FClickCommand) then
    with TMenuItemFriend(VCLObject) do
    begin
      Enabled := FClickCommand.Enabled;
      Checked := FClickCommand.Checked;
      GroupIndex := FClickCommand.GroupIndex;
      RadioItem := FClickCommand.RadioItem;
    end;
end;

end.
