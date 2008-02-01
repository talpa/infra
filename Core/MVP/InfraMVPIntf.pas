unit InfraMVPIntf;

interface

uses
  Classes,
  InfraCommonIntf,
  InfraValueTypeIntf;

type
  IPresenter = interface;
  IRenderer = interface;
  IView = interface;

  // selection

  ISelection = interface(IInfraList)
    ['{7AA23E2F-B768-4E6B-BC23-8A0E3AD1002C}']
    function GetMultiSelect: boolean;
    procedure SetMultiSelect(const Value: boolean);
    property MultiSelect: boolean read GetMultiSelect write SetMultiSelect;
  end;

  IInfraMultiSelectChanged = interface(IInfraEvent)
    ['{2526FE09-EB83-4F88-BE1E-02B2105B92F0}']
  end;

  // model

  IModel = interface(IElement)
    ['{F9E99695-C1EA-49E8-A79A-4C7F59A247B2}']
    // *** function GetCommandSet: IInfraFeatureList;
    function GetSelection: ISelection;
    function GetValue: IInfraType;
    procedure SetValue(const Value: IInfraType);
    // *** property CommandSet: IInfraFeatureList read GetCommandSet;
    property Selection: ISelection read GetSelection;
    property Value: IInfraType read GetValue write SetValue;
  end;

  IModelValueChanged = interface(IInfraEvent)
    ['{922DD2B3-BA0F-476B-BC19-E9FEDB056009}']
  end;

  IModelValueExchanged = interface(IInfraEvent)
    ['{747EC4F1-B47C-4E7B-A8B6-D8FAADC9A0B7}']
  end;

  // Views

  IViewList = interface(IBaseElement)
    ['{E00030D0-6185-41A8-B234-1240566A14C2}']
    function Add(const Item: IView): Integer;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IView;
    function GetOwner: IView;
    function NewIterator: IInfraIterator;
    function Remove(const Item: IView): Integer;
    procedure Clear;
    procedure SetItem(Index: Integer; const Value: IView);
    procedure SetOwner(const Value: IView);
    property Count: Integer read GetCount;
    property Owner: IView read GetOwner write SetOwner;
    property Items[Index: Integer]: IView read GetItem write SetItem;
  end;

  IView = interface(IElement)
    ['{B26B2455-5DC3-4412-A8EE-D772A3F90346}']
    function GetModel: IModel;
    function GetParentView: IView;
    function GetPresenter: IPresenter;
    function GetRenderer: IRenderer;
    function GetSubViews: IViewList;
    function GetTopView: IView;
    function HasSubViews: boolean;
    procedure SetParentView(const Value: IView);
    procedure SetPresenter(const Value: IPresenter);
    procedure SetRenderer(const Value: IRenderer);
    procedure Update;
    procedure UpdateModel;
    property Model: IModel read GetModel;
    property ParentView: IView read GetParentView write SetParentView;
    property Presenter: IPresenter read GetPresenter write SetPresenter;
    property Renderer: IRenderer read GetRenderer write SetRenderer;
    property SubViews: IViewList read GetSubViews;
    property TopView: IView read GetTopView;
  end;

  // Presenters

  IPresenterList = interface(IBaseElement)
    ['{E00030D0-6185-41A8-B234-1240566A14C2}']
    function Add(const Item: IPresenter): Integer;
    function Append(const Item: IPresenter): IPresenter;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IPresenter;
    function GetItemByName(const Name: string): IPresenter;
    function GetOwner: IPresenter;
    function NewIterator: IInfraIterator;
    function Remove(const Item: IPresenter): Integer;
    procedure Clear;
    procedure SetItem(Index: Integer; const Value: IPresenter);
    procedure SetOwner(const Value: IPresenter);
    property Count: Integer read GetCount;
    property Owner: IPresenter read GetOwner write SetOwner;
    property Items[Index: Integer]: IPresenter read GetItem
      write SetItem;
    property ItemsByName[const Name: string]: IPresenter read GetItemByName; default;
  end;

  IPresenter = interface(IElement)
    ['{3E5A64F8-236C-4D9A-A32B-CE3AD93B6ABD}']
    // *** function GetCommandSet: IInfraFeatureList;
    function GetModel: IModel;
    function GetName: string;
    function GetParentPresenter: IPresenter;
    function GetSubPresenters: IPresenterList;
    function GetView: IView;
    function HasSubPresenters: boolean;
    procedure SetModel(const Value: IModel);
    procedure SetName(const Value: string);
    procedure SetParentPresenter(const Value: IPresenter);
    procedure SetView(const Value: IView);
    procedure UpdatePresenter;
    // *** property CommandSet: IInfraFeatureList read GetCommandSet;
    property Model: IModel read GetModel write SetModel;
    property Name: string read GetName write SetName;
    property ParentPresenter: IPresenter read GetParentPresenter
      write SetParentPresenter;
    property SubPresenters: IPresenterList read GetSubPresenters;
    property View: IView read GetView write SetView;
  end;

  IContainerPresenter = interface(IPresenter)
    ['{43A5549A-D961-4C64-99B4-B669A1F94E28}']
  end;

  IValuePresenter = interface(IPresenter)
    ['{659E0422-7F2A-474A-93C4-47C57AF48590}']
  end;

  ITextPresenter = interface(IValuePresenter)
    ['{086CE750-8EBB-4445-9FF4-04F805B9E76B}']
  end;

  INumberPresenter = interface(IValuePresenter)
    ['{BBDB9921-A26B-4A77-91EF-36143410E485}']
  end;

  IIntegerPresenter = interface(IValuePresenter)
    ['{68DC775C-DEFE-4B72-BDB3-CAD901C05E56}']
  end;

  IDateTimePresenter = interface(IValuePresenter)
    ['{8341D712-C6B9-4687-8500-C1879E99CF66}']
  end;

  IBooleanPresenter = interface(IValuePresenter)
    ['{D2F0271E-595D-4F34-B79E-1F4493124C76}']
  end;

  IListItemPresenter = interface(IPresenter)
    ['{59AF8E7E-42F5-4FE4-9928-F02E97E83350}']
  end;

  IListPresenter = interface(IPresenter)
    ['{948898F7-EDE7-4EFB-9EB7-ACD1208D8C93}']
    function GetListItem: IListItemPresenter;
    property ListItem: IListItemPresenter read GetListItem;
  end;

  // renderes and formats

  IObjectToProperty = interface(ITypeConverter)
    ['{9C04661A-3B2D-4559-8542-792DBA9BA11B}']
  end;

  IFormatProperty = interface(IInfraType)
    ['{EE875657-020D-4919-ABEA-EA37599815CF}']
    function GetPropertyName: string;
    procedure SetPropertyName(const Value: string);
    property PropertyName: string read GetPropertyName write SetPropertyName;
    function GetRenderer: IRenderer;
    procedure SetRenderer(const Value: IRenderer);
    property Renderer: IRenderer read GetRenderer write SetRenderer;
  end;

  IRenderer = interface(IElement)
    ['{86CF9107-BEBF-4551-AD84-475124F8E634}']
    function GetFormat: IInfraType;
    function GetTypeConverter: ITypeConverter;
    function GetView: IView;
    procedure SetFormat(const Value: IInfraType);
    procedure SetTypeConverter(const Value: ITypeConverter);
    procedure SetView(const Value: IView);
    property Format: IInfraType read GetFormat write SetFormat;
    property View: IView read GetView write SetView;
    property TypeConverter: ITypeConverter read GetTypeConverter write SetTypeConverter;
  end;

  // commands

  ICommand = interface(IElement)
    ['{16FCA05E-EC65-4B59-81B5-E1395F3B9778}']
    function CanUndo: Boolean;
    function GetChecked: boolean;
    function GetEnabled: boolean;
    function GetGroupIndex: Integer;
    function GetRadioItem: boolean;
    procedure SetChecked(Value: boolean);
    procedure SetEnabled(Value: boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetRadioItem(Value: boolean);
    procedure Undo;
    property Checked: boolean read GetChecked write SetChecked;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex;
    property RadioItem: boolean read GetRadioItem write SetRadioItem;
  end;

  IAppendCommand = interface(ICommand)
    ['{FF93AA19-1AFC-4CC8-B1FE-9F00950A9394}']
  end;

  IDeleteCommand = interface(ICommand)
    ['{13BDE4C3-1319-4445-9531-D659487AE531}']
  end;

  ISelectionCommand = interface(ICommand)
    ['{DBBABF1F-B945-485E-B0E0-8E739278EF31}']
    function GetSelection: IInfraList;
    property Selection: IInfraList read GetSelection;
  end;

  ICommandChangedEvent = interface(IInfraEvent)
    ['{036B1C82-F67D-4E41-826C-351EB2DC2389}']
  end;

  // interactors

  IInteractor = interface(IElement)
    ['{F4267292-C222-4F51-B998-2739E5D1DDA4}']
    function GetView: IView;
    procedure SetView(const Value: IView);
    property View: IView read GetView write SetView;
  end;

implementation

end.
