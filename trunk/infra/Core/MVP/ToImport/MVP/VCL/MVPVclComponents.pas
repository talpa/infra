unit MVPVCLComponents;

{$I 'Infra.Inc'}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Classes, Windows, Controls, Graphics, Messages, StdCtrls, Forms,
  InfraFriendClasses, MVPBaseImpl, InfraMVPInterfaces,
  ReferenceImpl, InfraCommandIntf, InfraMVPIntf, InfraViewImpl;

type
  TMVPVCLViewItem = class(TMVPViewItem, IMVPVCLViewItem)
  private
    FComponentClass: string;
    FComponentName: string;
  protected
    function GetComponentClass: string;
    function GetComponentName: string;
    procedure SetComponentClass(const Value: string);
    procedure SetComponentName(const Value: string);
    property ComponentClass: string read GetComponentClass
      write SetComponentClass;
    property ComponentName: string read GetComponentName
      write SetComponentName;
  public
    constructor Create(IDItem: TGUID; const pComponentName: string = '';
      const pRendererItem: IMVPItem = nil;
      const pComponentClass: string = ''); overload;
  end;

  TMVPVCLListBoxViewItem = class(TMVPVCLViewItem,
    IMVPVCLListBoxViewItem)
  private
    FOwnerDraw: boolean;
  protected
    function GetOwnerDraw: boolean;
    procedure SetOwnerDraw(const Value: boolean);
    property OwnerDraw: boolean read GetOwnerDraw
      write SetOwnerDraw;
  public
    constructor Create(pID: TGUID;
      const pComponentName: string = ''; pOwnerDraw: boolean = False;
      const pRendererItem: IMVPItem = nil); overload;
  end;

  TMVPVCLListBoxItemViewItem = class(TMVPViewItem,
    IMVPVCLListBoxItemViewItem)
  public
    constructor Create(IDItem: TGUID;
      const pRendererItem: IMVPItem = nil); overload;
  end;

  TMVPVCLButtonViewItem = class(TMVPVCLViewItem,
    IMVPVCLButtonViewItem)
  private
    FClickCommandItem: IMVPItem;
  protected
    function GetClickCommandItem: IMVPItem;
    procedure SetClickCommandItem(const Value: IMVPItem);
    property ClickCommandItem: IMVPItem read GetClickCommandItem
      write SetClickCommandItem;
  public
    constructor Create(IDItem: TGUID;
      const pComponentName: string;
      const pClickCommandItem: IMVPItem); overload;
  end;

  TMVPVCLListBoxItemRendererItem = class(TMVPRendererItem,
    IMVPVCLListBoxItemRendererItem)
  public
    constructor Create(pID: TGUID;
      const pConverter: IMVPItem = nil;
      const pFormat: IMVPItem = nil); overload;
  end;

  TMVPVCLMenuItemRendererItem = class(TMVPRendererItem,
    IMVPVCLMenuItemRendererItem)
  public
    constructor Create(pID: TGUID;
      const pConverter: IMVPItem = nil;
      const pFormat: IMVPItem = nil); overload;
  end;

  TMVPVCLViewFactory = class(TMVPViewFactory, IMVPVCLViewFactory)
  private
    FVCLContainer: IVCLView;
  protected
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement; override;
    function GetVCLContainer: IVCLView;
    procedure ClearContext(const Item: IMVPItem); override;
    procedure SetVCLContainer(const Value: IVCLView);
    property VCLContainer: IVCLView read GetVCLContainer
      write SetVCLContainer;
  end;

  TMVPVCLButtonViewFactory = class(TMVPViewFactory,
    IMVPVCLButtonViewFactory)
  protected
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement; override;
    procedure LinkInstances(const Item: IMVPItem;
      const Instance: IElement); override;
  end;

  TMVPVCLListBoxViewFactory = class(TMVPViewFactory,
    IMVPVCLListBoxViewFactory)
  protected
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement; override;
  end;

  TMVPVCLListBoxItemViewFactory = class(TMVPViewFactory,
    IMVPVCLListBoxItemViewFactory)
  protected
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement; override;
  end;

implementation

uses SysUtils, List_VCLMenuItem, SimpleTypeImpl;


{ TMVPVCLViewItem }

constructor TMVPVCLViewItem.Create(IDItem: TGUID;
  const pComponentName: string = ''; const pRendererItem: IMVPItem = nil;
  const pComponentClass: string = '');
begin
  inherited Create(IDItem, pRendererItem);
  ItemType := IMVPVCLViewItem;
  FComponentClass := pComponentClass;
  FComponentName := pComponentName;
end;

function TMVPVCLViewItem.GetComponentClass: string;
begin
  Result := FComponentClass;
end;

function TMVPVCLViewItem.GetComponentName: string;
begin
  Result := FComponentName;
end;

procedure TMVPVCLViewItem.SetComponentClass(const Value: string);
begin
  FComponentClass := Value;
end;

procedure TMVPVCLViewItem.SetComponentName(const Value: string);
begin
  FComponentName := Value;
end;

{ TMVPVCLListBoxViewItem }

constructor TMVPVCLListBoxViewItem.Create(pID: TGUID;
  const pComponentName: string = ''; pOwnerDraw: boolean = False;
  const pRendererItem: IMVPItem = nil);
begin
  inherited Create(pID, pComponentName, pRendererItem);
  FOwnerDraw := pOwnerDraw;
  ItemType := IMVPVCLListBoxViewItem;
end;

function TMVPVCLListBoxViewItem.GetOwnerDraw: boolean;
begin
  Result := FOwnerDraw;
end;

procedure TMVPVCLListBoxViewItem.SetOwnerDraw(const Value: boolean);
begin
  FOwnerDraw := Value;
end;

{ TMVPVCLListBoxItemViewItem }

constructor TMVPVCLListBoxItemViewItem.Create(IDItem: TGUID;
  const pRendererItem: IMVPItem);
begin
  inherited Create(IDItem, pRendererItem);
  ItemType := IMVPVCLListBoxItemViewItem;
end;

{ TMVPVCLButtonViewItem }

constructor TMVPVCLButtonViewItem.Create(IDItem: TGUID;
  const pComponentName: string; const pClickCommandItem: IMVPItem);
begin
  inherited Create(IDItem, pComponentName);
  ItemType := IMVPVCLButtonViewItem;
  FClickCommandItem := pClickCommandItem;
end;

function TMVPVCLButtonViewItem.GetClickCommandItem: IMVPItem;
begin
  Result := FClickCommandItem;
end;

procedure TMVPVCLButtonViewItem.SetClickCommandItem(
  const Value: IMVPItem);
begin
  FClickCommandItem := Value;
end;

{ TMVPVCLListBoxItemRendererItem }

constructor TMVPVCLListBoxItemRendererItem.Create(pID: TGUID;
  const pConverter: IMVPItem = nil; const pFormat: IMVPItem = nil);
begin
  inherited Create(pID, pConverter, pFormat);
  ItemType := IMVPVCLListBoxItemRendererItem;
end;

{ TMVPVCLMenuItemRendererItem }

constructor TMVPVCLMenuItemRendererItem.Create(pID: TGUID;
  const pConverter: IMVPItem; const pFormat: IMVPItem);
begin
  inherited Create(pID, pConverter, pFormat);
  ItemType := IMVPVCLMenuItemRendererItem;
end;

{ TMVPVCLViewFactory }

procedure TMVPVCLViewFactory.ClearContext(const Item: IMVPItem);
var
  ViewItem: IMVPVCLViewItem;
  PresenterItem: IMVPPresenterItem;
begin
  inherited;
  if Assigned(FVCLContainer) and
    Supports(Item, IMVPPresenterItem, PresenterItem) and
    Supports(PresenterItem.View, IMVPVCLViewItem, ViewItem) and
    (ViewItem.ComponentClass <> EmptyStr) then
    FVCLContainer := nil;
end;

function TMVPVCLViewFactory.GetNewInstance(const Item: IMVPItem;
  const Value: IInfraObject): IElement;
var
  Instance: IVCLView;
  VCLViewItem: IMVPVCLViewItem;
  FormClass: TComponentClass;
  Form: TCustomForm;
begin
  Instance := inherited GetNewInstance(Item, Value) as IVCLView;
  VCLViewItem := Item as IMVPVCLViewItem;
  with VCLViewItem do
    if ComponentClass <> EmptyStr then
    begin
      FormClass := TComponentClass(GetClass(VCLViewItem.ComponentClass));
      Assert(Assigned(FormClass),
        Format('FormClass %s not registred', [ComponentClass]));
      Application.CreateForm(FormClass, Form);
      Instance.VCLObject := Form;
      FVCLContainer := Instance;
    end else if VCLViewItem.ComponentName <> EmptyStr then
    begin
      Instance.VCLObject :=
        TComponent(FVCLContainer.VCLObject).FindComponent(
          VCLViewItem.ComponentName);
    end;
  Result := Instance as IElement;
end;

function TMVPVCLViewFactory.GetVCLContainer: IVCLView;
begin
  Result := FVCLContainer;
end;

procedure TMVPVCLViewFactory.SetVCLContainer(const Value: IVCLView);
begin
  FVCLContainer := Value;
end;

{ TMVPVCLButtonViewFactory }

function TMVPVCLButtonViewFactory.GetNewInstance(
  const Item: IMVPItem; const Value: IInfraObject): IElement;
var
  Instance: IVCLButtonView;
  VCLViewItem: IMVPVCLButtonViewItem;
  Container: IVCLView;
begin
  Instance := inherited GetNewInstance(Item, Value) as IVCLButtonView;
  VCLViewItem := Item as IMVPVCLButtonViewItem;
  Container := (MVPFactoryService.GetSubFactory(
    IMVPVCLViewItem) as IMVPVCLViewFactory).VCLContainer;
  with VCLViewItem do
    if VCLViewItem.ComponentName <> EmptyStr then
    begin
      Instance.VCLObject :=
        TComponent(Container.VCLObject).FindComponent(
          VCLViewItem.ComponentName);
    end;
  Result := Instance as IElement;
end;

procedure TMVPVCLButtonViewFactory.LinkInstances(
  const Item: IMVPItem; const Instance: IElement);
var
  CommandInstance: ICommand;
  ButtonItem: IMVPVCLButtonViewItem;
begin
  inherited LinkInstances(Item, Instance);
  ButtonItem := Item as IMVPVCLButtonViewItem;
  CommandInstance := MVPFactoryService.FindInstanceOfMVPItem(
    ButtonItem.ClickCommandItem as IMVPItem) as ICommand;
  if Assigned(CommandInstance) then
    with Instance as IVCLButtonView do
      ClickCommand := CommandInstance;
end;

{ TMVPVCLListBoxViewFactory }

function TMVPVCLListBoxViewFactory.GetNewInstance(
  const Item: IMVPItem; const Value: IInfraObject): IElement;
var
  Instance: IVCLCustomListBoxView;
  VCLViewItem: IMVPVCLListBoxViewItem;
  Container: IVCLView;
begin
  Instance := inherited GetNewInstance(Item, Value) as IVCLCustomListBoxView;
  VCLViewItem := Item as IMVPVCLListBoxViewItem;
  Container := (MVPFactoryService.GetSubFactory(
    IMVPVCLViewItem) as IMVPVCLViewFactory).VCLContainer;
  with VCLViewItem do
    if VCLViewItem.ComponentName <> EmptyStr then
    begin
      Instance.OwnerDraw := OwnerDraw;
      Instance.VCLObject :=
        TComponent(Container.VCLObject).FindComponent(
          VCLViewItem.ComponentName);
    end;
  Result := Instance as IElement;
end;

{ TMVPVCLListBoxItemViewFactory }

function TMVPVCLListBoxItemViewFactory.GetNewInstance(
  const Item: IMVPItem; const Value: IInfraObject): IElement;
begin
  Result := inherited GetNewInstance(Item, Value) as IElement;
end;

procedure RegisterMVPFactories;
begin
  with MVPFactoryService do
  begin
    RegisterSubFactory(IMVPVCLViewItem,
      TMVPVCLViewFactory.Create as IMVPFactory);
    RegisterSubFactory(IMVPVCLButtonViewItem,
      TMVPVCLButtonViewFactory.Create as IMVPFactory);
    RegisterSubFactory(IMVPVCLListBoxViewItem,
      TMVPVCLListBoxViewFactory.Create as IMVPFactory);
    RegisterSubFactory(IMVPVCLListBoxItemViewItem,
      TMVPVCLListBoxItemViewFactory.Create as IMVPFactory);
    RegisterSubFactory(IMVPVCLListBoxItemRendererItem,
      TMVPRendererFactory.Create as IMVPFactory);
    RegisterSubFactory(IMVPVCLMenuItemRendererItem,
      TMVPRendererFactory.Create as IMVPFactory);
  end;
end;

initialization
  RegisterMVPFactories;

end.
