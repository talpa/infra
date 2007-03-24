unit InfraMVPInterfaces;

interface

uses
  Controls, Classes, InfraInterfaces, ReferenceImpl, Windows, Graphics,
  InfraCommandIntf, InfraSelectionIntf, InfraMVPIntf;

type
  IVCLCustomListBoxItemView = interface;
  IVCLMenuItemView = interface;
  IMVPItem = Interface;

  // MVP Items and Factories

  IMVPItemIterator = interface(IInterface)
    ['{BD09652B-DF55-4941-8730-FC8200AF5332}']
    function CurrentItem: IMVPItem;
    procedure First;
    function IsDone: Boolean;
    procedure Next;
  end;

  // *** nao pude por const nos parâmetros interface
  IMVPItemList = Interface(IMemoryManagedObject)
    ['{59778A29-1974-426A-A508-1FF95464CE72}']
    function Add(Index: TGUID; Value: IMVPItem): TGUID;
    function Append(Value: IMVPItem): IMVPItem;
    function GetItem(Index: TGUID): IMVPItem;
    function GetCount: Integer;
    function IndexOfPosition(Index: Integer): TGUID;
    function InvalidIndex: TGUID;
    function InvalidValue: IMVPItem;
    function IsIndexEqual(Index1, Index2: TGUID): boolean;
    function IsValueEqual(Value1, Value2: IMVPItem): boolean;
    function NewIterator: IMVPItemIterator;
    function PositionOf(Index: TGUID; Value: IMVPItem): integer;
    function ValueOfPosition(Index: Integer): IMVPItem;
    procedure Delete(Index: TGUID);
    procedure DeletePosition(Index: integer);
    procedure SetItem(Index: TGUID; Value: IMVPItem);
    property Count: Integer read GetCount;
    property Items[Index: TGUID]: IMVPItem read GetItem write SetItem; default;
  end;

  IMVPItem = Interface(IElement)
    ['{12F06207-6D46-4451-97BE-8EE49A95C40C}']
    function GetItemID: TGUID;
    function GetItemType: TGUID;
    procedure SetItemID(const Value: TGUID);
    procedure SetItemType(const Value: TGUID);
    property ItemId: TGUID read GetItemId write SetItemId ;
    property ItemType: TGUID read GetItemType write SetItemType;
  end;

  IMVPCommandItem = Interface(IMVPItem)
    ['{5D2796BE-9E7E-4A29-AA6E-D8D0581209A6}']
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
  end;

  IMVPModelItem = Interface(IMVPItem)
    ['{82030B94-4D3E-4886-B23D-A3607CAB2C2F}']
    function GetCommandSet: IMVPItemList;
    function GetFeatureName: string;
    function HasCommands: boolean;
    procedure SetFeatureName(const Value: string);
    property CommandSet: IMVPItemList read GetCommandSet;
    property FeatureName: string read GetFeatureName write SetFeatureName;
  end;

  IMVPPresenterItem = Interface(IMVPItem)
    ['{0BEB9397-8940-4069-A9AD-241E1E9FE9A4}']
    function GetSubItems: IMVPItemList;
    function GetModel: IMVPItem;
    function GetName: string;
    function GetView: IMVPItem;
    function HasSubItems: boolean;
    procedure SetModel(const Value: IMVPItem);
    procedure SetName(const Value: string);
    procedure SetView(const Value: IMVPItem);
    property SubItems: IMVPItemList read GetSubItems;
    property Model: IMVPItem read GetModel write SetModel;
    property Name: string read GetName write SetName;
    property View: IMVPItem read GetView write SetView;
  end;

  IMVPViewItem = Interface(IMVPItem)
    ['{3A39E788-571D-40C6-A707-87ED9C3D5228}']
    function GetRenderer: IMVPItem;
    procedure SetRenderer(const Value: IMVPItem);
    property Renderer: IMVPItem read GetRenderer write SetRenderer;
  end;

  IMVPVCLViewItem = interface(IMVPViewItem)
    ['{44BC0E04-9E75-4012-A876-BCDEBAAC6006}']
    function GetComponentClass: string;
    function GetComponentName: string;
    procedure SetComponentClass(const Value: string);
    procedure SetComponentName(const Value: string);
    property ComponentClass: string read GetComponentClass
      write SetComponentClass;
    property ComponentName: string read GetComponentName
      write SetComponentName;
  end;

  IMVPVCLListBoxViewItem = interface(IMVPVCLViewItem)
    ['{B57D20CE-6AB3-4D50-8AAB-5573E44F04FC}']
    function GetOwnerDraw: boolean;
    procedure SetOwnerDraw(const Value: boolean);
    property OwnerDraw: boolean read GetOwnerDraw write SetOwnerDraw;
  end;

  IMVPVCLListBoxItemViewItem = interface(IMVPViewItem)
    ['{F04FB6D5-021C-4160-A159-6E6A75D940F7}']
    {***del}
    {
    function GetFeatureName: string;
    procedure SetFeatureName(const Value: string);
    property FeatureName: string read GetFeatureName write SetFeatureName;
    }
  end;

  IMVPVCLButtonViewItem = interface(IMVPVCLViewItem)
    ['{1BF5DC27-009B-48E3-A98D-8B6DAA192143}']
    function GetClickCommandItem: IMVPItem;
    procedure SetClickCommandItem(const Value: IMVPItem);
    property ClickCommandItem: IMVPItem read GetClickCommandItem
      write SetClickCommandItem;
  end;

  IMVPConverterItem = Interface(IMVPItem)
    ['{3F9CF2A2-FBA9-428C-8379-C0EE757194E9}']
  end;

  IMVPObjectToFeatureConverterItem = Interface(IMVPItem)
    ['{B04448FE-6AD7-480A-A389-9EEF56549021}']
  end;

  IMVPFormatItem = Interface(IMVPItem)
    ['{242F56EF-B4F2-463C-ADB5-A16267745043}']
    function GetFormatValue: IInfraType;
    procedure SetFormatValue(const Value: IInfraType);
    property FormatValue: IInfraType read GetFormatValue write SetFormatValue;
  end;

  IMVPFormatFeatureItem = Interface(IMVPItem)
    ['{F3C820C9-6F54-4A01-AE2C-09DA5C478B28}']
    function GetFeatureName: string;
    function GetRenderer: IMVPItem;
    procedure SetFeatureName(const Value: string);
    procedure SetRenderer(const Value: IMVPItem);
    property FeatureName: string read GetFeatureName write SetFeatureName;
    property Renderer: IMVPItem read GetRenderer write SetRenderer;
  end;

  IMVPRendererItem = Interface(IMVPItem)
    ['{E67546B0-C872-4A76-9C14-9247C47B8C7D}']
    function GetConverter: IMVPItem;
    function GetFormat: IMVPItem;
    procedure SetConverter(const Value: IMVPItem);
    procedure SetFormat(const Value: IMVPItem);
    property Converter: IMVPItem read GetConverter write SetConverter;
    property Format: IMVPItem read GetFormat write SetFormat;
  end;

  IMVPVCLListBoxItemRendererItem = Interface(IMVPRendererItem)
    ['{A86286C5-3368-4081-9CB5-2011F416A444}']
  end;

  IMVPVCLMenuItemRendererItem = Interface(IMVPRendererItem)
    ['{189440FB-2A8F-4F31-AA42-2BD694198354}']
  end;

  IMVPFactory = Interface(IElement)
    ['{C4286572-59CB-4D24-A76B-E5A43D2B358B}']
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement;
    function InternalGetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement;
    procedure ClearContext(const Item: IMVPItem);
    procedure LinkInstances(const Item: IMVPItem;
      const Instance: IElement);
  end;

  IMVPPresenterFactory = Interface(IMVPFactory)
    ['{BA41FA1B-7982-48A7-8522-E3133D0D34C4}']
  end;

  IMVPCommandFactory = Interface(IMVPFactory)
    ['{8AB49B69-2EF7-4E47-96E2-A98268776986}']
  end;

  IMVPModelFactory = Interface(IMVPFactory)
    ['{2D8E8061-1CAA-48A9-A640-774B42725811}']
  end;

  IMVPViewFactory = Interface(IMVPFactory)
    ['{878A28FF-D293-4AE4-AA58-D72A8E87036A}']
  end;

  IMVPVCLViewFactory = interface(IMVPFactory)
    ['{7E50A0D4-FFDF-42D1-B1DC-E657D5891F01}']
    function GetVCLContainer: IVCLView;
    procedure SetVCLContainer(const Value: IVCLView);
    property VCLContainer: IVCLView read GetVCLContainer
      write SetVCLContainer;
  end;

  IMVPVCLListBoxViewFactory = interface(IMVPFactory)
    ['{E4C28D30-E396-4F5A-BC30-6F3125950B26}']
  end;

  IMVPVCLListBoxItemViewFactory = interface(IMVPFactory)
    ['{127A845F-2D81-4B56-9349-B78A9B908974}']
  end;

  IMVPVCLButtonViewFactory = Interface(IMVPFactory)
    ['{F5BAAD75-0F8B-4350-92E1-7C06AD925010}']
  end;

  IMVPConverterFactory = Interface(IMVPFactory)
    ['{0CBB93C0-9523-42E6-A5F0-7D63388827A4}']
  end;

  IMVPFormatFactory = Interface(IMVPFactory)
    ['{858C29BE-7309-456F-B393-712EA0CE6669}']
  end;

  IMVPFormatFeatureFactory = Interface(IMVPFactory)
    ['{3CC4552F-2179-4E8A-B2A5-208E5287BAC6}']
  end;

  IMVPRendererFactory = Interface(IMVPFactory)
    ['{CBF76714-D9B0-4CBF-814B-346D805B7982}']
  end;

  IMVPVCLListBoxItemRendererFactory = Interface(IMVPRendererFactory)
    ['{BA0F3808-8AB4-4687-877F-92C24BC513F1}']
  end;

  IMVPVCLMenuItemRendererFactory = Interface(IMVPRendererFactory)
    ['{00F06A88-D931-4C8E-93AD-454B49B214B0}']
  end;

  IMVPFactoryIterator = interface(IInterface)
    ['{BD09652B-DF55-4941-8730-FC8200AF5332}']
    function CurrentItem: IMVPFactory;
    procedure First;
    function IsDone: Boolean;
    procedure Next;
  end;

  IMVPItemsInstances = Interface(IMemoryManagedObject)
    ['{82492FB1-6882-4817-9389-A581DAF4FF57}']
    function Add(Index: IMVPItem; Value: IElement): IMVPItem;
    function GetItem(Index: IMVPItem): IElement;
    function GetCount: Integer;
    function IndexOfPosition(Index: Integer): IMVPItem;
    function InvalidIndex: IMVPItem;
    function InvalidValue: IElement;
    function IsIndexEqual(Index1, Index2: IMVPItem): boolean;
    function IsValueEqual(Value1, Value2: IElement): boolean;
    function NewIterator: IInterface;
    function PositionOf(Index: IMVPItem; Value: IElement): integer;
    function ValueOfPosition(Index: Integer): IElement;
    procedure Clear;
    procedure Delete(Index: IMVPItem);
    procedure DeletePosition(Index: integer);
    procedure SetItem(Index: IMVPItem; Value: IElement);
    property Count: Integer read GetCount;
    property Items[Index: IMVPItem]: IElement read GetItem write SetItem;
  end;

  IMVPFactoryService = interface(IElement)
    ['{845B31A3-83C7-4A04-B502-A29B51B269F4}']
    function FindInstanceOfMVPItem(Item: IMVPItem): IElement;
    function GetNewInstance(ID: TGUID;
      const Value: IInfraObject): IElement; overload;
    function GetNewInstance(const Item: IMVPItem;
      const Value: IInfraObject): IElement; overload;
    function GetItems: IMVPItemList;
    function GetItemsInstances: IMVPItemsInstances;
    function GetSubFactory(TypeItem: TGUID): IMVPFactory;
    procedure ClearContext(const Item: IMVPItem);
    procedure RegisterSubFactory(ID: TGUID;
      const Factory: IMVPFactory);
    property Items: IMVPItemList read GetItems;
    property ItemsInstances: IMVPItemsInstances read GetItemsInstances;
  end;

function MVPFactoryService: IMVPFactoryService;

implementation

function MVPFactoryService: IMVPFactoryService;
begin
  Result := ApplicationContext as IMVPFactoryService;
end;

end.
