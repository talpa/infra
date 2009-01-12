unit GUIAnnotationIntf;

interface

uses
  InfraCommonIntf, InfraValueTypeIntf, Controls, ExtCtrls, LayoutManager,
  Classes;

type

  IScreenItem = interface(IElement)
    ['{C81D3037-A032-4D17-BE2B-EEDAEC4D6CCD}']
    function GetCaption: IInfraString;
    function GetCaptionPosition: TLabelPosition;
    function GetCaptionPositionChanged: Boolean;
    function GetCaptionVisible: IInfraBoolean;
    function GetItemHeight: IInfraInteger;
    function GetItemHeightMeasureType: TMeasureType;
    function GetItemHeightMeasureTypeChanged: Boolean;
    function GetItemWidth: IInfraInteger;
    function GetItemWidthMeasureType: TMeasureType;
    function GetItemWidthMeasureTypeChanged: Boolean;
    function GetName: string;
    function GetPutAfter: string;
    function GetPutBefore: string;
    function GetVisible: IInfraBoolean;
    procedure SetCaption(const Value: IInfraString);
    procedure SetCaptionPosition(const Value: TLabelPosition);
    procedure SetCaptionVisible(const Value: IInfraBoolean);
    procedure SetItemHeight(const Value: IInfraInteger);
    procedure SetItemHeightMeasureType(const Value: TMeasureType);
    procedure SetItemWidth(const Value: IInfraInteger);
    procedure SetItemWidthMeasureType(const Value: TMeasureType);
    procedure SetName(const Value: string);
    procedure SetPutAfter(const Value: string);
    procedure SetPutBefore(const Value: string);
    procedure SetVisible(const Value: IInfraBoolean);
    function CloneItem: IScreenItem;
    procedure SetItemSize(pHeight, pWidth: IInfraInteger);
    property Caption: IInfraString read GetCaption write SetCaption;
    property CaptionPosition: TLabelPosition read GetCaptionPosition write SetCaptionPosition;
    property CaptionPositionChanged: Boolean read GetCaptionPositionChanged;
    property CaptionVisible: IInfraBoolean read GetCaptionVisible write SetCaptionVisible;
    property ItemHeight: IInfraInteger read GetItemHeight write SetItemHeight;
    property ItemHeightMeasureType: TMeasureType read GetItemHeightMeasureType write SetItemHeightMeasureType;
    property ItemHeightMeasureTypeChanged: Boolean read GetItemHeightMeasureTypeChanged;
    property ItemWidth: IInfraInteger read GetItemWidth write SetItemWidth;
    property ItemWidthMeasureType: TMeasureType read GetItemWidthMeasureType write SetItemWidthMeasureType;
    property ItemWidthMeasureTypeChanged: Boolean read GetItemWidthMeasureTypeChanged;
    property Name: string read GetName write SetName;
    property PutAfter: string read GetPutAfter write SetPutAfter;
    property PutBefore: string read GetPutBefore write SetPutBefore;
    property Visible: IInfraBoolean read GetVisible write SetVisible;
  end;

  IScreenItemIterator = interface(IInterface)
    ['{80348E06-3336-46DA-8BD9-AEFDBF2FCA47}']
    function CurrentItem: IInterface;
    function IsDone: Boolean;
    procedure First;
    procedure Next;
  end;

  IScreenItemListBase = interface(IBaseElement)
    ['{00D52CFB-9FE3-4AD0-BB94-D71B87399A36}']
    function Add(const Item: IScreenItem): Integer;
    function First: IScreenItem;
    function GetCount: Integer;
    function GetItem(Index: Integer): IScreenItem;
    function IndexOf(const Item: IScreenItem): Integer;
    function Last: IScreenItem;
    function NewIterator: IScreenItemIterator;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const Item: IScreenItem);
    procedure SetItem(Index: Integer; const TypeInfo: IScreenItem);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IScreenItem read GetItem write SetItem; default;
  end;

  IScreenItemList = interface(IScreenItemListBase)
    ['{DA9CF4FD-E5E2-4EF3-A936-C5A3642F4116}']
    function Clone: IScreenItemList;
  end;

  ICustomProperty = interface(IElement)
    ['{3E9AEC5A-8572-4F13-8A98-D813382F1FAB}']
    function GetPropName: string;
    function GetPropValue: Variant;
    function Clone: ICustomProperty;
    procedure SetPropName(const Value: string);
    procedure SetPropValue(const Value: Variant);
    property PropName: string read GetPropName write SetPropName;
    property PropValue: Variant read GetPropValue write SetPropValue;
  end;

  ICustomPropertyIterator = interface(IInterface)
    ['{F0B65BF8-2582-4D09-B020-709FB7F10065}']
    function CurrentItem: IInterface;
    function IsDone: Boolean;
    procedure First;
    procedure Next;
  end;

  ICustomPropertyListBase = interface(IBaseElement)
    ['{59D910CC-E093-4541-B8D4-204D3717E6F0}']
    function Add(const Item: ICustomProperty): Integer;
    function First: ICustomProperty;
    function GetCount: Integer;
    function GetItem(Index: Integer): ICustomProperty;
    function IndexOf(const Item: ICustomProperty): Integer;
    function Last: ICustomProperty;
    function NewIterator: ICustomPropertyIterator;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const Item: ICustomProperty);
    procedure SetItem(Index: Integer; const TypeInfo: ICustomProperty);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: ICustomProperty read GetItem write SetItem; default;
  end;

  ICustomPropertyList = interface(ICustomPropertyListBase)
    ['{67C8610E-8EC8-4D6A-BF5E-6F7E658ED9DF}']
    function Clone: ICustomPropertyList;
    procedure AddProp(PropName: string; PropValue: Variant);
  end;

  IScreenControl = interface(IScreenItem)
    ['{AAAACFBB-E1C1-43CA-A8AC-3031F166DD01}']
    function GetControlClass: TControlClass;
    function GetControlProperty: IInfraString;
    function GetCustomProperties: ICustomPropertyList;
    function GetHeight: IInfraInteger;
    function GetPropertyName: string;
    function GetWidth: IInfraInteger;
    procedure SetControlClass(const Value: TControlClass);
    procedure SetControlProperty(const Value: IInfraString);
    procedure SetCustomProperties(const Value: ICustomPropertyList);
    procedure SetHeight(const Value: IInfraInteger);
    procedure SetPropertyName(const Value: string);
    procedure SetWidth(const Value: IInfraInteger);
    procedure SetSize(pHeight, pWidth: IInfraInteger);
    function Clone: IScreenControl;
    property ControlClass: TControlClass read GetControlClass write SetControlClass;
    property ControlProperty: IInfraString read GetControlProperty write SetControlProperty;
    property CustomProperties: ICustomPropertyList read GetCustomProperties write SetCustomProperties;
    property Height: IInfraInteger read GetHeight write SetHeight;
    property PropertyName: string read GetPropertyName write SetPropertyName;
    property Width: IInfraInteger read GetWidth write SetWidth;
  end;

  IScreenGroup = interface(IScreenItem)
    ['{E7E0F1F8-5685-4AD8-AC98-8677C7AB18DD}']
    function GetItemLayout: TLayoutOrientation;
    function GetItems: IScreenItemList;
    procedure SetItemLayout(const Value: TLayoutOrientation);
    procedure SetItems(const Value: IScreenItemList);
    function Clone: IScreenGroup;
    property ItemLayout: TLayoutOrientation read GetItemLayout write SetItemLayout;
    property Items: IScreenItemList read GetItems write SetItems;
  end;

  IScreen = interface(IElement)
    ['{BAAA2E7E-90DA-449A-8AD8-90533B51BDFA}']
    function GetCaptionPosition: TLabelPosition;
    function GetControlSpacing: IInfraInteger;
    function GetHeight: IInfraInteger;
    function GetHideProperties: TStrings;
    function GetItemLayout: TLayoutOrientation;
    function GetItems: IScreenItemList;
    function GetItemSpacing: TLayoutManagerSpacing;
    function GetName: string;
    function GetPadding: TLayoutManagerPadding;
    function GetShowProperties: TStrings;
    function GetTitle: IInfraString;
    function GetWidth: IInfraInteger;
    procedure SetCaptionPosition(const Value: TLabelPosition);
    procedure SetControlSpacing(const Value: IInfraInteger);
    procedure SetHeight(const Value: IInfraInteger);
    procedure SetItemLayout(const Value: TLayoutOrientation);
    procedure SetItems(const Value: IScreenItemList);
    procedure SetName(const Value: string);
    procedure SetTitle(const Value: IInfraString);
    procedure SetWidth(const Value: IInfraInteger);
    function Clone: IScreen;        
    function AddControl(pPropertyName: string): IScreenControl;
    function AddGroup(pName: string): IScreenGroup;
    function GetControl(pPropertyName: string): IScreenControl;
    function GetControlByName(pName: string): IScreenControl;
    function GetGroup(pName: string): IScreenGroup;
    procedure Group(pProperties: TStrings);
    procedure SetSize(pHeight, pWidth: Integer);
    function UseProperty(pPropertyName: string): Boolean;
    property CaptionPosition: TLabelPosition read GetCaptionPosition write SetCaptionPosition;
    property ControlSpacing: IInfraInteger read GetControlSpacing write SetControlSpacing;
    property Height: IInfraInteger read GetHeight write SetHeight;
    property HideProperties: TStrings read GetHideProperties;
    property Items: IScreenItemList read GetItems write SetItems;
    property ItemLayout: TLayoutOrientation read GetItemLayout write SetItemLayout;
    property ItemSpacing: TLayoutManagerSpacing read GetItemSpacing;
    property Name: string read GetName write SetName;
    property Padding: TLayoutManagerPadding read GetPadding;
    property ShowProperties: TStrings read GetShowProperties;
    property Title: IInfraString read GetTitle write SetTitle;
    property Width: IInfraInteger read GetWidth write SetWidth;
  end;

  IScreenIterator = interface(IInterface)
    ['{6A3ED3FF-8B7B-4A05-B0F6-6A932B967CFF}']
    function CurrentItem: IInterface;
    function IsDone: Boolean;
    procedure First;
    procedure Next;
  end;

  IScreenList = interface(IBaseElement)
    ['{E3D036D3-A837-4728-80B5-38116987FB4D}']
    function Add(const Item: IScreen): Integer;
    function First: IScreen;
    function GetCount: Integer;
    function GetItem(Index: Integer): IScreen;
    function IndexOf(const Item: IScreen): Integer;
    function Last: IScreen;
    function NewIterator: IScreenIterator;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const Item: IScreen);
    procedure SetItem(Index: Integer; const TypeInfo: IScreen);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IScreen read GetItem write SetItem; default;
  end;

  IScreens = interface(IElement)
    ['{2F04D99B-5377-482E-9279-096A96DC74B3}']
    function AddScreen(pName: string): IScreen;
    function GetScreen(pName: string): IScreen;
    function GetScreens: IScreenList;
    property Screens: IScreenList read GetScreens;
  end;

  implementation

end.
