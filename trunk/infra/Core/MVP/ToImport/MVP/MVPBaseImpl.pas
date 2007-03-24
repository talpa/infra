{
  TO_DO 001     Verify if the user is putting '-' on first position of string
                I think that we will need a private field to store the new text.
                We think too that this can be Maskedit job;

  TO_DO 002     chage all Try Except from converters to if...else;

  TO_DO 003     Acredito que onde se usa subviewiterators pode ser modificado
                para pegar diretamente da subview.;

Commands:
http://www.blainebuxton.com/weblog/2005/09/dolphins-command-framework.html
http://www.blainebuxton.com/weblog/2005/09/re-dolphins-command-framework.html
http://www.ironspeed.com/Designer/3.0.3/Videos/FieldProperties/Customizing%20Field%20Properties.html
http://www.ironspeed.com/products/Training.aspx
}
unit MVPBaseImpl;

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Contnrs, InfraMVPIntf,
  InfraMVPInterfaces, TypeImpl, SimpleTypeImpl, ReferenceImpl,
  List_MVPFactory, InfraCommandIntf, InfraSelectionIntf;

type
  TMVPItem = class(TElement, IMVPItem)
  private
    FItemId: TGUID;
    FItemType: TGUID;
  protected
    function GetItemID: TGUID;
    function GetItemType: TGUID;
    procedure SetItemID(const Value: TGUID);
    procedure SetItemType(const Value: TGUID);
    property ItemId: TGUID read GetItemId write SetItemId;
    property ItemType: TGUID read GetItemType write SetItemType;
  end;

  TMVPCommandItem = class(TMVPItem, IMVPCommandItem)
  private
    FChecked: boolean;
    FEnabled: boolean;
    FGroupIndex: Integer;
    FRadioItem: boolean;
  protected
    function GetChecked: boolean;
    function GetEnabled: boolean;
    function GetGroupIndex: Integer;
    function GetRadioItem: boolean;
    procedure SetChecked(Value: boolean);
    procedure SetEnabled(Value: boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetRadioItem(Value: boolean);
    property Checked: boolean read GetChecked write SetChecked;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex;
    property RadioItem: boolean read GetRadioItem write SetRadioItem;
  public
    constructor Create(IDItem: TGUID; pEnabled: boolean = True;
      pCheked: boolean = False; pGroupIndex: integer = 0;
      pRadioItem: boolean = False);
  end;

  TMVPModelItem = class(TMVPItem, IMVPModelItem)
  private
    FFeatureName: string;
    FCommandSet: IMVPItemList;
  protected
    function GetCommandSet: IMVPItemList;
    function GetFeatureName: string;
    function HasCommands: boolean;
    procedure SetFeatureName(const Value: string);
    property CommandSet: IMVPItemList read GetCommandSet;
    property FeatureName: string read GetFeatureName write SetFeatureName;
  public
    constructor Create(IDItem: TGUID;
      const FeatureName: string = ''); overload;
  end;

  TMVPPresenterItem = class(TMVPItem, IMVPPresenterItem)
  private
    FModel: IMVPItem;
    FName: string;
    FSubItems: IMVPItemList;
    FView: IMVPItem;
  protected
    function GetModel: IMVPItem;
    function GetName: string;
    function GetSubItems: IMVPItemList;
    function GetView: IMVPItem;
    function HasSubItems: boolean;
    procedure SetModel(const Value: IMVPItem);
    procedure SetName(const Value: string);
    procedure SetView(const Value: IMVPItem);
    property Model: IMVPItem read GetModel write SetModel;
    property Name: string read GetName;
    property Items: IMVPItemList read GetSubItems;
    property View: IMVPItem read GetView write SetView;
  public
    constructor Create(IDItem: TGUID; const NameItem: string;
      const ViewItem: IMVPItem; const ModelItem: IMVPItem); overload;
  end;

  TMVPViewItem = class(TMVPItem, IMVPViewItem)
  private
    FRenderer: IMVPItem;
  protected
    function GetRenderer: IMVPItem;
    procedure SetRenderer(const Value: IMVPItem);
    property Renderer: IMVPItem read GetRenderer write SetRenderer;
  public
    constructor Create(IDItem: TGUID;
      const RenderItem: IMVPItem); overload;
  end;

  TMVPConverterItem = class(TMVPItem, IMVPConverterItem)
  public
    constructor Create(IDItem: TGUID); overload;
  end;

  TMVPObjectToFeatureConverterItem = class(TMVPConverterItem,
    IMVPObjectToFeatureConverterItem)
  public
    constructor Create(IDItem: TGUID); overload;
  end;

  TMVPFormatItem = class(TMVPItem, IMVPFormatItem)
  private
    FFormatValue: IInfraType;
  protected
    function GetFormatValue: IInfraType;
    procedure SetFormatValue(const Value: IInfraType);
    property FormatValue: IInfraType read GetFormatValue write SetFormatValue;
  public
    constructor Create(IDItem: TGUID; const pFormatValue: IInfraType); overload;
  end;

  TMVPFormatFeatureItem = class(TMVPItem, IMVPFormatFeatureItem)
  private
    FFeatureName: string;
    FRenderer: IMVPItem;
  protected
    function GetFeatureName: string;
    function GetRenderer: IMVPItem;
    procedure SetFeatureName(const Value: string);
    procedure SetRenderer(const Value: IMVPItem);
    property FeatureName: string read GetFeatureName write SetFeatureName;
    property Renderer: IMVPItem read GetRenderer write SetRenderer;
  public
    constructor Create(IDItem: TGUID; const pFeatureName: string;
      const pRenderer: IMVPItem = nil); overload;
  end;

  TMVPRendererItem = class(TMVPItem, IMVPRendererItem)
  private
    FConverter: IMVPItem;
    FFormat: IMVPItem;
  protected
    function GetConverter: IMVPItem;
    function GetFormat: IMVPItem;
    procedure SetConverter(const Value: IMVPItem);
    procedure SetFormat(const Value: IMVPItem);
    property Converter: IMVPItem read GetConverter write SetConverter;
    property Format: IMVPItem read GetFormat write SetFormat;
  public
    constructor Create(IDItem: TGUID;
      const ItemConverter: IMVPItem = nil;
      const ItemFormat: IMVPItem = nil); overload;
  end;

  TMVPFactoryService = class(TElement, IMVPFactoryService)
  private
    FItems: IMVPItemList;
    FFactories: TMVPFactories;
    FItemsInstances: IMVPItemsInstances;
    FInstancesLinked: boolean;
  protected
    function FindInstanceOfMVPItem(Item: IMVPItem): IElement;
    function GetNewInstance(ID: TGUID;
      const Value: IInfraObject): IElement; overload;
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement; overload;
    function GetItems: IMVPItemList;
    function GetItemsInstances: IMVPItemsInstances;
    function GetSubFactory(TypeItem: TGUID): IMVPFactory;
    procedure ClearContext(const Item: IMVPItem);
    procedure LinkInstances;
    procedure RegisterSubFactory(ID: TGUID;
      const Factory: IMVPFactory);
    property Items: IMVPItemList read GetItems;
    property ItemsInstances: IMVPItemsInstances read GetItemsInstances;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TMVPFactory = class(TElement, IMVPFactory)
  protected
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement; virtual;
    function InternalGetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement;
    procedure ClearContext(const Item: IMVPItem); virtual;
    procedure LinkInstances(const Item: IMVPItem;
      const Instance: IElement); virtual;
  end;

  TMVPCommandFactory = class(TMVPFactory, IMVPCommandFactory)
  protected
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement; override;
  end;

  TMVPModelFactory = class(TMVPFactory, IMVPFactory)
  private
    procedure InstaciateModelCommandItem(const Instance: IModel;
      const Item: IMVPItem);
  protected
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement; override;
  end;

  TMVPPresenterFactory = class(TMVPFactory, IMVPPresenterFactory)
  private
    procedure InstaciateSubPresenterItem(const Instance: IPresenter;
      const Value: IInfraObject; const MVPSubItem: IMVPItem);
  protected
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement; override;
  end;

  TMVPFormatFactory = class(TMVPFactory, IMVPFormatFactory)
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement; override;
  end;

  TMVPFormatFeatureFactory = class(TMVPFactory,
    IMVPFormatFeatureFactory)
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement; override;
  end;

  TMVPRendererFactory = class(TMVPFactory, IMVPRendererFactory)
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement; override;
  end;

  TMVPViewFactory = class(TMVPFactory, IMVPViewFactory)
  protected
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement; override;
  end;

implementation

uses SysUtils, List_Presenter, List_Feature, List_MVPItem,
  InfraRendererIntf, List_MVPItemInstance, List_MVPView;

{ TMVPItem }

function TMVPItem.GetItemId: TGUID;
begin
  Result := FItemId;
end;

function TMVPItem.GetItemType: TGUID;
begin
  Result := FItemType;
end;

procedure TMVPItem.SetItemId(const Value: TGUID);
begin
  FItemId := Value;
end;

procedure TMVPItem.SetItemType(const Value: TGUID);
begin
  FItemType := Value;
end;

{ TCommandItem }

constructor TMVPCommandItem.Create(IDItem: TGUID; pEnabled,
  pCheked: boolean; pGroupIndex: integer; pRadioItem: boolean);
begin
  inherited Create;
  ItemId := IDItem;
  ItemType := IMVPCommandItem;
  FChecked := pCheked;
  FEnabled := pEnabled;
  FGroupIndex := pGroupIndex;
  FRadioItem := pRadioItem;
end;

function TMVPCommandItem.GetChecked: boolean;
begin
  Result := FChecked;
end;

function TMVPCommandItem.GetEnabled: boolean;
begin
  Result := FEnabled;
end;

function TMVPCommandItem.GetGroupIndex: Integer;
begin
  Result := FGroupIndex;
end;

function TMVPCommandItem.GetRadioItem: boolean;
begin
  Result := FRadioItem;
end;

procedure TMVPCommandItem.SetChecked(Value: boolean);
begin
  FChecked := Value;
end;

procedure TMVPCommandItem.SetEnabled(Value: boolean);
begin
  FEnabled := Value;
end;

procedure TMVPCommandItem.SetGroupIndex(Value: Integer);
begin
  FGroupIndex := Value;
end;

procedure TMVPCommandItem.SetRadioItem(Value: boolean);
begin
  FRadioItem := Value;
end;

{ TMVPModelItem }

constructor TMVPModelItem.Create(IDItem: TGUID;
  const FeatureName: string);
begin
  inherited Create;
  ItemId := IDItem;
  ItemType := IMVPModelItem;
  FFeatureName := FeatureName;
end;

function TMVPModelItem.GetCommandSet: IMVPItemList;
begin
  if not Assigned(FCommandSet) then
    FCommandSet := TMVPItemList.Create;
  Result := FCommandSet;
end;

function TMVPModelItem.GetFeatureName: string;
begin
  Result := FFeatureName;
end;

function TMVPModelItem.HasCommands: boolean;
begin
  Result := Assigned(FCommandSet) and (FCommandSet.Count <> 0);
end;

procedure TMVPModelItem.SetFeatureName(const Value: string);
begin
  FFeatureName := Value;
end;

{ TMVPPresenterItem }

constructor TMVPPresenterItem.Create(IDItem: TGUID; const NameItem: string;
  const ViewItem: IMVPItem; const ModelItem: IMVPItem);
begin
  inherited Create;
  ItemId := IDItem;
  ItemType := IMVPPresenterItem;
  FName := NameItem;
  FModel := ModelItem;
  FView := ViewItem;
end;

function TMVPPresenterItem.GetModel: IMVPItem;
begin
  Result := FModel;
end;

function TMVPPresenterItem.GetName: string;
begin
  Result := FName;
end;

function TMVPPresenterItem.GetSubItems: IMVPItemList;
begin
  if not Assigned(FSubItems) then
    FSubItems := TMVPItemList.Create;
  Result := FSubItems;
end;

function TMVPPresenterItem.GetView: IMVPItem;
begin
  Result := FView;
end;

function TMVPPresenterItem.HasSubItems: boolean;
begin
  Result := Assigned(FSubItems) and (FSubItems.Count <> 0);
end;

procedure TMVPPresenterItem.SetModel(const Value: IMVPItem);
begin
  FModel := Value;
end;

procedure TMVPPresenterItem.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TMVPPresenterItem.SetView(const Value: IMVPItem);
begin
  FView := Value;
end;

{ TMVPViewItem }

constructor TMVPViewItem.Create(IDItem: TGUID;
  const RenderItem: IMVPItem);
begin
  inherited Create;
  ItemId := IDItem;
  ItemType := IMVPViewItem;
  FRenderer := RenderItem;
end;

function TMVPViewItem.GetRenderer: IMVPItem;
begin
  Result := FRenderer;
end;

procedure TMVPViewItem.SetRenderer(const Value: IMVPItem);
begin
  FRenderer := Value;
end;

{ TMVPConverterItem }

constructor TMVPConverterItem.Create(IDItem: TGUID);
begin
  inherited Create;
  ItemId := IDItem;
  ItemType := IMVPConverterItem;
end;

{ TMVPObjectToFeatureConverterItem }

constructor TMVPObjectToFeatureConverterItem.Create(IDItem: TGUID);
begin
  inherited Create;
  ItemId := IDItem;
  ItemType := IMVPObjectToFeatureConverterItem;
end;

{ TMVPFormatItem }

constructor TMVPFormatItem.Create(IDItem: TGUID;
  const pFormatValue: IInfraType);
begin
  inherited Create;
  ItemId := IDItem;
  ItemType := IMVPFormatItem;
  FFormatValue := pFormatValue;
end;

function TMVPFormatItem.GetFormatValue: IInfraType;
begin
  Result := FFormatValue;
end;

procedure TMVPFormatItem.SetFormatValue(const Value: IInfraType);
begin
  FFormatValue := Value;
end;

{ TMVPFeatureFormatItem }

constructor TMVPFormatFeatureItem.Create(IDItem: TGUID;
  const pFeatureName: string; const pRenderer: IMVPItem = nil);
begin
  inherited Create;
  ItemId := IDItem;
  ItemType := IMVPFormatFeatureItem;
  FFeatureName := pFeatureName;
  FRenderer := pRenderer;
end;

function TMVPFormatFeatureItem.GetFeatureName: string;
begin
  Result := FFeatureName;
end;

function TMVPFormatFeatureItem.GetRenderer: IMVPItem;
begin
  Result := FRenderer;
end;

procedure TMVPFormatFeatureItem.SetFeatureName(const Value: string);
begin
  FFeatureName := Value;
end;

procedure TMVPFormatFeatureItem.SetRenderer(
  const Value: IMVPItem);
begin
  FRenderer := Value;
end;

{ TMVPRendererItem }

constructor TMVPRendererItem.Create(IDItem: TGUID;
  const ItemConverter: IMVPItem = nil;
  const ItemFormat: IMVPItem = nil);
begin
  inherited Create;
  ItemId := IDItem;
  ItemType := IMVPRendererItem;
  FConverter := ItemConverter;
  FFormat := ItemFormat;
end;

function TMVPRendererItem.GetConverter: IMVPItem;
begin
  Result := FConverter;
end;

function TMVPRendererItem.GetFormat: IMVPItem;
begin
  Result := FFormat;
end;

procedure TMVPRendererItem.SetConverter(const Value: IMVPItem);
begin
  FConverter := Value;
end;

procedure TMVPRendererItem.SetFormat(const Value: IMVPItem);
begin
  FFormat := Value;
end;

{ TMVPFactoryService }

constructor TMVPFactoryService.Create;
begin
  inherited;
  FFactories := TMVPFactories.Create;
end;

destructor TMVPFactoryService.Destroy;
begin
  if Assigned(FItemsInstances) then
    FItemsInstances.Clear;
  FFactories.Free;
  inherited;
end;

function TMVPFactoryService.GetSubFactory(TypeItem: TGUID): IMVPFactory;
begin
  Result := FFactories[TypeItem];
  Assert(Assigned(Result), 'SubFactory not found!');
end;

function TMVPFactoryService.GetNewInstance(ID: TGUID;
  const Value: IInfraObject): IElement;
var
  Item: IMVPItem;
begin
  Item := FItems[Id];
  Result := GetNewInstance(Item, Value);
end;

function TMVPFactoryService.GetNewInstance(const Item: IMVPItem;
  const Value: IInfraObject): IElement;
var
  bIsTopLevel: Boolean;
begin
  if not FInstancesLinked then
  begin
    FInstancesLinked := True;
    bIsTopLevel := True;
  end
  else
    bIsTopLevel := False;
  Assert(Assigned(FFactories[Item.ItemType]), 'Factory not exists! '+
    GuidToString(Item.ItemType));
  Result := FFactories[Item.ItemType].InternalGetNewInstance(Item, Value);
  if bIsTopLevel then begin
    LinkInstances;
    MVPFactoryService.ItemsInstances.Clear;
    FInstancesLinked := False;
  end;
end;

procedure TMVPFactoryService.ClearContext(const Item: IMVPItem);
var
  Iterator: IMVPFactoryIterator;
begin
  Iterator := FFactories.NewIterator;
  while not Iterator.IsDone do
  begin
    (Iterator.CurrentItem as IMVPFactory).ClearContext(Item);
    Iterator.Next;
  end;
end;

procedure TMVPFactoryService.RegisterSubFactory(ID: TGUID;
  const Factory: IMVPFactory);
begin
  FFactories.Add(ID, Factory);
end;

procedure TMVPFactoryService.LinkInstances;
var
  i: integer;
  Item: IMVPItem;
  Instance: IElement;
begin
  if Assigned(FItemsInstances) then
    for i := 0 to FItemsInstances.Count-1 do
    begin
      Item := FItemsInstances.IndexOfPosition(i);
      Instance := FItemsInstances.ValueOfPosition(i);
      GetSubFactory(Item.ItemType).LinkInstances(Item, Instance);
    end;
end;

function TMVPFactoryService.GetItems: IMVPItemList;
begin
  if not Assigned(FItems) then
    FItems := TMVPItemList.Create;
  Result := FItems;
end;

function TMVPFactoryService.GetItemsInstances: IMVPItemsInstances;
begin
  if not Assigned(FItemsInstances) then
    FItemsInstances := TMVPItemsInstances.Create;
  Result := FItemsInstances;
end;

function TMVPFactoryService.FindInstanceOfMVPItem(
  Item: IMVPItem): IElement;
begin
  if Assigned(FItemsInstances) then
    Result := FItemsInstances.Items[Item]
  else
    Result := nil;
  Assert(Assigned(Result), 'Instance of MVPItem not found');
end;

{ TMVPFactory }

procedure TMVPFactory.ClearContext(const Item: IMVPItem);
begin
  // do nothing here, just on descendents!
end;

function TMVPFactory.GetNewInstance(const Item: IMVPItem;
  const Value: IInfraObject): IElement;
begin
  Result := TypeRegisterService.GetNewInstance(Item.ItemId);
end;

function TMVPFactory.InternalGetNewInstance(const Item: IMVPItem;
  const Value: IInfraObject): IElement;
begin
  Result := GetNewInstance(Item, Value);
  MVPFactoryService.ItemsInstances.Add(Item, Result);
end;

procedure TMVPFactory.LinkInstances(const Item: IMVPItem;
  const Instance: IElement);
begin
  // do nothing here, just on descendents!
end;

{ TMVPCommandFactory }

function TMVPCommandFactory.GetNewInstance(const Item: IMVPItem;
  const Value: IInfraObject): IElement;
var
  Instance: ICommand;
begin
  Result := inherited GetNewInstance(Item, Value);
  Instance := Result as ICommand;
  with (Item as IMVPCommandItem) do
  begin
    Instance.Checked := Checked;
    Instance.RadioItem := RadioItem;
    Instance.GroupIndex := GroupIndex;
    Instance.Enabled := Enabled;
  end;
end;

{ TMVPModelFactory }

function TMVPModelFactory.GetNewInstance(const Item: IMVPItem;
  const Value: IInfraObject): IElement;
var
  Iterator: IMVPItemIterator;
  lModel: IModel;
begin
  Result := inherited GetNewInstance(Item, Value);
  lModel := Result as IModel;
  with (Item as IMVPModelItem) do
  begin
    if FeatureName <> EmptyStr then
      lModel.Value := Value.Feature[FeatureName] as IInfraType
    else
      lModel.Value := Value as IInfraType;
    if HasCommands then
    begin
      Iterator := CommandSet.NewIterator;
      with Iterator do
        while not IsDone do
        begin
          InstaciateModelCommandItem(lModel, CurrentItem);
          Next;
        end;
    end;
  end;
end;

procedure TMVPModelFactory.InstaciateModelCommandItem(
  const Instance: IModel; const Item: IMVPItem);
var
  Command: IElement;
begin
  Command := MVPFactoryService.GetNewInstance(Item, nil);
  Instance.CommandSet.Add(Command as IInfraFeature);
end;

{ TMVPPresenterFactory }

function TMVPPresenterFactory.GetNewInstance(const Item: IMVPItem;
  const Value: IInfraObject): IElement;
var
  PresenterItem: IMVPPresenterItem;
  Iterator: IMVPItemIterator;
begin
  Result := inherited GetNewInstance(Item, Value);
  PresenterItem := Item as IMVPPresenterItem;
  with (Result as IPresenter) do
  begin
    Model := MVPFactoryService.GetNewInstance(PresenterItem.Model,
      Value) as IModel;
    View := MVPFactoryService.GetNewInstance(PresenterItem.View,
      Value) as IView;
    Name := PresenterItem.Name;
  end;
  if PresenterItem.HasSubItems then
  begin
    Iterator := PresenterItem.SubItems.NewIterator;
    with Iterator do
      while not IsDone do
      begin
        InstaciateSubPresenterItem(Result as IPresenter, Value,
          CurrentItem);
        Next;
      end;
  end;
  MvpFactoryService.ClearContext(Item);
end;

procedure TMVPPresenterFactory.InstaciateSubPresenterItem(
  const Instance: IPresenter; const Value: IInfraObject;
  const MVPSubItem: IMVPItem);
var
  SubItemInstance: IElement;
begin
  SubItemInstance :=
    MVPFactoryService.GetNewInstance(MVPSubItem, Value);
  if Supports(MVPSubItem, IMVPPresenterItem) then
  begin
    Instance.SubPresenters.Add(SubItemInstance as IPresenter);
    if Assigned(Instance.View) and
      Assigned((SubItemInstance as IPresenter).View) then
      Instance.View.SubViews.Add((SubItemInstance as IPresenter).View);
  end else if Supports(MVPSubItem, IMVPViewItem) and
    Assigned(Instance.View) and Assigned(SubItemInstance as IView) then
      Instance.View.SubViews.Add(SubItemInstance as IView)
  else if Supports(MVPSubItem, IMVPCommandItem) then
    Instance.CommandSet.Add(SubItemInstance as IInfraFeature);
end;

{ TMVPFormatFactory }

function TMVPFormatFactory.GetNewInstance(const Item: IMVPItem;
  const Value: IInfraObject): IElement;
begin
  Result := inherited GetNewInstance(Item, Value);
  with Result as IInfraType do
    Assign((Item as IMVPFormatItem).FormatValue);
end;

{ TMVPFormatFeatureFactory }

function TMVPFormatFeatureFactory.GetNewInstance(const Item: IMVPItem;
  const Value: IInfraObject): IElement;
var
  FormatFeatureItem: IMVPFormatFeatureItem;
begin
  Result := inherited GetNewInstance(Item, Value);
  FormatFeatureItem := Item as IMVPFormatFeatureItem;
  with Result as IFormatFeature do
  begin
    FeatureName := FormatFeatureItem.FeatureName;
    if Assigned(FormatFeatureItem.Renderer) then
      Renderer := MVPFactoryService.GetNewInstance(FormatFeatureItem.Renderer,
        Value) as IRenderer;
  end;
end;

{ TMVPRendererFactory }

function TMVPRendererFactory.GetNewInstance(const Item: IMVPItem;
  const Value: IInfraObject): IElement;
var
  RendererItem: IMVPRendererItem;
begin
  Result := inherited GetNewInstance(Item, Value);
  RendererItem := Item as IMVPRendererItem;
  with Result as IRenderer do
  begin
    if Assigned(RendererItem.Converter) then
      TypeConverter := MVPFactoryService.GetNewInstance(RendererItem.Converter,
        Value) as ITypeConverter;
    if Assigned(RendererItem.Format) then
      Format := MVPFactoryService.GetNewInstance(RendererItem.Format,
        Value) as IInfraType;
  end;
end;

{ TMVPViewFactory }

function TMVPViewFactory.GetNewInstance(const Item: IMVPItem;
  const Value: IInfraObject): IElement;
begin
  Result := inherited GetNewInstance(Item, Value);
  with (Item as IMVPViewItem) do
    if Assigned(Renderer) then
    begin
      (Result as IView).Renderer :=
        MVPFactoryService.GetNewInstance(Renderer, Value) as IRenderer;
    end;
end;

procedure InjectMVPServices;
begin
  with (ApplicationContext as IMemoryManagedObject).InjectedInterfaces do
  begin
    Add(IMVPFactoryService,
      TMVPFactoryService.Create as IMVPFactoryService);
    Add(IMVPMapperService,
      TMVPMapperService.Create as IMVPMapperService);
  end;
end;

procedure RegisterMVPFactories;
begin
  with MVPFactoryService do
  begin
    RegisterSubFactory(IMVPPresenterItem,
      TMVPPresenterFactory.Create as IMVPFactory);
    RegisterSubFactory(IMVPCommandItem,
      TMVPCommandFactory.Create as IMVPFactory);
    RegisterSubFactory(IMVPModelItem,
      TMVPModelFactory.Create as IMVPFactory);
    RegisterSubFactory(IMVPViewItem,
      TMVPViewFactory.Create as IMVPFactory);
    RegisterSubFactory(IMVPConverterItem,
      TMVPFactory.Create as IMVPFactory);
    RegisterSubFactory(IMVPObjectToFeatureConverterItem,
      TMVPFactory.Create as IMVPFactory);
    RegisterSubFactory(IMVPFormatItem,
      TMVPFormatFactory.Create as IMVPFactory);
    RegisterSubFactory(IMVPFormatFeatureItem,
      TMVPFormatFeatureFactory.Create as IMVPFactory);
    RegisterSubFactory(IMVPRendererItem,
      TMVPRendererFactory.Create as IMVPFactory);
  end;
end;

initialization
  InjectMVPServices;
  RegisterMVPFactories;

end.
