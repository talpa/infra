unit InfraGUIBuilderIntf;

interface

uses
  InfraCommonIntf, InfraValueTypeIntf, Controls, ExtCtrls, LayoutManager,
  GUIAnnotationIntf, Forms;

type

  IGUIControl = interface;

  IGUIControlList = interface;

  IGUI = interface(IBaseElement)
    ['{5E2DBC11-126E-4B75-9C2F-BAC58CCD88EA}']
    function GetBusinessObject: IInfraObject;
    function GetGUIControlList: IGUIControlList;
    function GetName: string;
    function GetScreen: IScreen;
    function GetTitle: string;
    procedure SetBusinessObject(const Value: IInfraObject);
    procedure SetGUIControlList(const Value: IGUIControlList);
    procedure SetName(const Value: string);
    procedure SetScreen(const Value: IScreen);
    procedure SetTitle(const Value: string);
    function Clone: IGUI;
    function FindGUIControl(pPropertyName : string): IGUIControl;
    function GetConfigurationFileName: string;
    property BusinessObject: IInfraObject read GetBusinessObject write SetBusinessObject;
    property GUIControlList: IGUIControlList read GetGUIControlList write SetGUIControlList;
    property Name: string read GetName write SetName;
    property Screen: IScreen read GetScreen write SetScreen;
    property Title: string read GetTitle write SetTitle;
  end;

  IGUIControl = interface(IBaseElement)
    ['{048C693A-63AD-46E1-B84B-94B5A8D1BB18}']
    function GetControl: TControl;
    function GetControlClass: TControlClass;
    function GetControlProperty: string;
    function GetItem: TLayoutManagerItem;
    function GetName: string;
    function GetPropertyInfo: IPropertyInfo;
    function GetPropertyName: string;
    function GetPropertyValue: IInfraType;
    function GetScreenItem: IScreenItem;
    procedure SetControl(const Value: TControl);
    procedure SetControlClass(const Value: TControlClass);
    procedure SetControlProperty(const Value: string);
    procedure SetItem(const Value: TLayoutManagerItem);
    procedure SetName(const Value: string);
    procedure SetPropertyInfo(const Value: IPropertyInfo);
    procedure SetPropertyName(const Value: string);
    procedure SetPropertyValue(const Value: IInfraType);
    procedure SetScreenItem(const Value: IScreenItem);
    function Clone: IGUIControl;
    property Control: TControl read GetControl write SetControl;
    property ControlClass: TControlClass read GetControlClass  write SetControlClass;
    property ControlProperty: string read GetControlProperty write SetControlProperty;
    property Item: TLayoutManagerItem read GetItem write SetItem;
    property Name: string read GetName write SetName;
    property PropertyInfo: IPropertyInfo read GetPropertyInfo write SetPropertyInfo;
    property PropertyName: string read GetPropertyName write SetPropertyName;
    property PropertyValue: IInfraType read GetPropertyValue write SetPropertyValue;
    property ScreenItem: IScreenItem read GetScreenItem write SetScreenItem;
  end;

  IGUIControlIterator = interface(IInterface)
    ['{7D128901-D74A-43A0-BB7D-FE9E369C9088}']
    function CurrentItem: IInterface;
    function IsDone: Boolean;
    procedure First;
    procedure Next;
  end;

  IGUIControlListBase = interface(IBaseElement)
    ['{126083E3-1000-4256-8EF0-4326AA3256E6}']
    function Add(const Item: IGUIControl): Integer;
    function First: IGUIControl;
    function GetCount: Integer;
    function GetItem(Index: Integer): IGUIControl;
    function IndexOf(const Item: IGUIControl): Integer;
    function Last: IGUIControl;
    function NewIterator: IGUIControlIterator;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const Item: IGUIControl);
    procedure SetItem(Index: Integer; const TypeInfo: IGUIControl);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IGUIControl read GetItem write SetItem; default;
  end;

  IGUIControlList = interface(IGUIControlListBase)
    ['{4738BE6D-255E-427E-854C-3273060DD5CE}']
    function Clone: IGUIControlList;
  end;

  IGUIMapping = interface(IBaseElement)
    ['{24153D00-151F-4762-A833-DA2E8D4B492C}']
    function GetControlClass: TControlClass;
    function GetControlProperty: string;
    function GetTypeInfo: TGUID;
    procedure SetControlClass(const Value: TControlClass);
    procedure SetControlProperty(const Value: string);
    procedure SetTypeInfo(const Value: TGUID);
    property ControlClass: TControlClass read GetControlClass write SetControlClass;
    property ControlProperty: string read GetControlProperty write SetControlProperty;
    property TypeInfo: TGUID read GetTypeInfo write SetTypeInfo;
  end;

  IGUIMappingIterator = interface(IInterface)
    ['{E90024FF-427C-4DC4-85F6-789B6BEA5D27}']
    function CurrentItem: IInterface;
    function IsDone: Boolean;
    procedure First;
    procedure Next;
  end;

  IGUIMappingList = interface(IBaseElement)
    ['{F10C6886-A282-4691-A5E2-9F5FD5003554}']
    function Add(const Item: IGUIMapping): Integer;
    function First: IGUIMapping;
    function GetCount: Integer;
    function GetItem(Index: Integer): IGUIMapping;
    function IndexOf(const Item: IGUIMapping): Integer;
    function Last: IGUIMapping;
    function NewIterator: IGUIMappingIterator;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const Item: IGUIMapping);
    procedure SetItem(Index: Integer; const TypeInfo: IGUIMapping);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IGUIMapping read GetItem write SetItem; default;
  end;

  IInfraGUIService = interface(IBaseElement)
    ['{99444446-6E4E-4BC3-B282-4119FFE4EBF9}']
    function GetGUIMappings: IGUIMappingList;
    procedure Build(pObject: IInfraObject; pScreen: IScreen = nil);
    procedure RegisterGUIMapping(pControlClass: TControlClass;
      pTypeInfo: TGUID; pBindProperty: string);
    property GUIMappings: IGUIMappingList read GetGUIMappings;
  end;

function GUIService: IInfraGUIService;

implementation

uses
  InfraGUIBuilderRegister;

function GUIService: IInfraGUIService;
begin
  Result := ApplicationContext as IInfraGUIService;
end;

initialization
  InfraGUIBuilderRegister.RegisterOnReflection;

end.
