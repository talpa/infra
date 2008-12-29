unit InfraMVPVCLIntf;

interface

uses
  Types,
  Windows,
  Graphics,
  Classes,
  Controls,
  InfraCommonIntf,
  InfraMVPIntf,
  InfraValueTypeIntf;

type
  IMVPMapperService = interface(IElement)
    ['{744FE4AA-FF4D-4410-BB26-F056A7FA65D9}']
    function GetKeyInterceptor: IElement;
    function GetModelFromObject(Obj: TObject): IModel;
    function GetMouseInterceptor: IElement;
    procedure SetKeyInterceptor(const Value: IElement);
    procedure RegisterMVPItem(const Model: IModel; Obj: TObject);
    procedure RemoveMVPItem(Obj: TObject);
    procedure SetMouseInterceptor(const Value: IElement);
    property KeyInterceptor: IElement read GetKeyInterceptor
      write SetKeyInterceptor;
    property MouseInterceptor: IElement read GetMouseInterceptor
      write SetMouseInterceptor;
  end;

  // VCL Control events

  IMVPFormViewDestroyEvent = interface(IInfraEvent)
    ['{AA33A2A7-6028-496B-AFE3-67FC807FEC0F}']
  end;

  IVCLNotifyEvent = interface(IInfraEvent)
    ['{EF39470E-1020-448F-A359-D9168560EF72}']
  end;

  IVCLFormDestroyEvent = interface(IVCLNotifyEvent)
    ['{59EA8775-C5DC-4FAA-82F7-E50C7B1EEAEB}']
  end;

  IVCLFormCloseEvent = interface(IVCLNotifyEvent)
    ['{86A4A034-B629-4B78-8696-3520A3784861}']
  end;

  IClickEvent = interface(IVCLNotifyEvent)
    ['{8B04D140-8173-4C05-8B9F-B290C30C9C88}']
  end;

  IVCLDblClickEvent = interface(IVCLNotifyEvent)
    ['{DA197AB2-E779-44A1-8C58-0248D850019E}']
  end;

  IVCLDragEvent = interface(IInfraEvent)
    ['{9DECBBCE-1F87-42F2-8C64-66DB7F9EC1EF}']
    function GetViewObject: IView;
    function GetX: Integer;
    function GetY: Integer;
    property ViewObject: IView read GetViewObject;
    property X: Integer read GetX;
    property Y: Integer read GetY;
  end;

  IVCLDragDropEvent = interface(IVCLDragEvent)
    ['{369D082D-788E-40C6-8362-5603F2D4F7E3}']
  end;

  IVCLDragOverEvent = interface(IVCLDragEvent)
    ['{537510A4-7AC2-4A85-9631-FC0A9236E84F}']
    function GetAccept: boolean;
    function GetState: TDragState;
    procedure SetAccept(Value: boolean);
    property Accept: boolean read GetAccept write SetAccept;
    property State: TDragState read GetState;
  end;

  IVCLEndDragEvent = interface(IVCLDragEvent)
    ['{1D639B4E-15A9-4005-9FAD-08E948D96719}']
  end;

  IVCLMouseMoveEvent = interface(IInfraEvent)
    ['{2B4FF361-C8A9-4746-A0CB-8C8ABA45232F}']
    function GetShift: TShiftState;
    function GetX: Integer;
    function GetY: Integer;
    property Shift: TShiftState read GetShift;
    property X: Integer read GetX;
    property Y: Integer read GetY;
  end;

  IVCLMouseEvent = interface(IVCLMouseMoveEvent)
    ['{4215552C-AA02-42F3-8073-1052FF67A956}']
    function GetButton: TMouseButton;
    property Button: TMouseButton read GetButton;
  end;

  IVCLMouseDownEvent = interface(IVCLMouseEvent)
    ['{26BAD73F-F4F6-4DC8-9F62-F70FFD305359}']
  end;

  IVCLMouseUpEvent = interface(IVCLMouseEvent)
    ['{2705147F-6EAA-4FC4-977F-DD86FE245059}']
  end;

  IVCLStartDragEvent = interface(IInfraEvent)
    ['{E7C600B4-9E46-4C01-98AD-BEA472DFA624}']
    function GetDragObject: TDragObject;
    procedure SetDragObject(Value: TDragObject);
    property DragObject: TDragObject read GetDragObject write SetDragObject;
  end;

  IContextPopupEvent = interface(IInfraEvent)
    ['{397AAE2F-A61D-45D2-8683-B2058836C7D2}']
    function GetHandled: Boolean;
    function GetMousePos: TPoint;
    procedure SetHandled(bHandled: Boolean);
    property MousePos: TPoint read GetMousePos;
    property Handled: Boolean read GetHandled write SetHandled;
  end;

  // WinControl events

  IEnterEvent = interface(IVCLNotifyEvent)
    ['{C40A7730-31B9-4CB6-A567-618547D58DC8}']
  end;

  IExitEvent = interface(IVCLNotifyEvent)
    ['{84DB211F-C279-4CA9-8411-A4007A53DD42}']
  end;

  IKeyBoardEvent = Interface(IInfraEvent)
    ['{09FB8E9A-2DBF-421D-B63E-5AB2D7CE51E3}']
  end;

  IKeyPressEvent = interface(IKeyBoardEvent)
    ['{E70E2294-96AC-4FF8-9C4F-1C518CE31E51}']
    function GetKey: Char;
    procedure SetKey(AKey: Char);
    property Key: Char read GetKey write SetKey;
  end;

  IKeyEvent = interface(IKeyBoardEvent)
    ['{E0453B6C-5ADC-420F-9765-2BD59CD36A94}']
    function GetKey: Word;
    function GetShift: TShiftState;
    procedure SetKey(AKey: Word);
    property Key: Word read GetKey write SetKey;
    property Shift: TShiftState read GetShift;
  end;

  IKeyDownEvent = interface(IKeyEvent)
    ['{5F882367-4FF6-4460-B5D8-55757C20B3EC}']
  end;

  IKeyUpEvent = interface(IKeyEvent)
    ['{80E4479B-A4DA-482F-9548-D5A364139284}']
  end;

  // Views

  ITextInterface = interface(IInterface)
    ['{40473959-0679-4658-B9DE-FA3070120E0B}']
    function GetText: string;
    property Text: string read GetText;
  end;

  IVCLView = interface(IView)
    ['{6F0D7DB9-A4BC-46E5-AAC5-6A6FBF1B5510}']
    function GetSubViewsIterator: IInfraIterator;
    function FindViewFromObject(VCLObject: TObject): IView;
    function GetObject: TObject;
    procedure SetObject(Value: TObject);
    property SubViewsIterator: IInfraIterator read GetSubViewsIterator;
    property VCLObject: TObject read GetObject write SetObject;
  end;

  IVCLControlView = interface(IVCLView)
    ['{9CE501A6-02B4-4BAA-A46A-80E4453854D6}']
    function GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  IVCLWinControlView = interface(IVCLControlView)
    ['{BA5A8F01-E953-43EE-9139-857DCDBAA990}']
  end;

  IVCLCustomFormView = interface(IVCLWinControlView)
    ['{C17378B8-4DB0-4F0D-B450-4BFA55625AC6}']
    procedure Close;
    procedure Hide;
    procedure Show;
    procedure Release;
    procedure ShowAndWait;
  end;

  IVCLCustomEditView = interface(IVCLWinControlView)
    ['{5EBB90D2-A95F-4704-9BAB-6487F0AA5134}']
    function GetText: string;
    property Text: string read GetText;
  end;

  IVCLCutomEditChangeEvent = interface(IVCLNotifyEvent)
    ['{2583987B-2D6A-4A5E-A0C5-08322EE542E8}']
  end;

  IVCLEditView = interface(IVCLCustomEditView)
    ['{297FE78C-F890-4FC4-ABA5-093A010145A1}']
  end;

  // *** Create a view to this
  IVCLMaskEditView = interface(IVCLCustomEditView)
    ['{BDA83001-0370-4AA5-9F73-B9B297B4DE2E}']
  end;

  IVCLEditNumberView = interface(IVCLMaskEditView)
    ['{7D52A40E-07E0-4190-8CC4-9CF767E6FF51}']
  end;

  // *** label herda de graphiccontrol
  IVCLCustomLabelView = interface(IVCLControlView)
    ['{648AE194-66C0-4A1F-A3B1-CBE1CFF49790}']
    function GetLabelText: string;
    property Caption: string read GetLabelText;
  end;

  IVCLCustomDateTimeView = interface(IVCLWinControlView)
    ['{38D5D094-1ABA-4FDD-88AC-228E645FA91D}']
    function GetDateTime: TDateTime;
    property DateTime: TDateTime read GetDateTime;
  end;

  IVCLButtonControlView = interface(IVCLWinControlView)
    ['{4373E49E-DA52-4AAC-948D-33B6E27C4368}']
  end;

  IVCLRadioButtonView = interface(IVCLButtonControlView)
    ['{F3235DA9-03E4-4F0B-B8A0-76F4E4F1860B}']
    function GetChecked: IInfraBoolean;
    property Checked: IInfraBoolean read GetChecked;
  end;

  IVCLCustomCheckBoxView = interface(IVCLButtonControlView)
    ['{9FBC2F06-9EF6-4E4F-ADD9-D2C6078F607A}']
    function GetState: IInfraBoolean;
    property State: IInfraBoolean read GetState;
  end;

  IVCLButtonView = interface(IVCLButtonControlView)
    ['{66A64B64-273A-46EF-8C93-5DD36A78392D}']
    function GetCancel: Boolean;
    function GetClickCommand: ICommand;
    function GetDefault: Boolean;
    function GetModalResult: Integer;
    procedure SetCancel(Value: Boolean);
    procedure SetClickCommand(const Value: ICommand);
    procedure SetDefault(Value: Boolean);
    procedure SetModalResult(Value: Integer);
    property Cancel: Boolean read GetCancel write SetCancel;
    property ClickCommand: ICommand read GetClickCommand write SetClickCommand;
    property Default: Boolean read GetDefault write SetDefault;
    property ModalResult: Integer read GetModalResult write SetModalResult;
  end;

  IVCLCustomListBoxItemView = interface(IView)
    ['{850A40CA-3644-46DA-82B6-CB0F51A3C88A}']
    function GetFeatureName: string;
    function GetValueForIndex(Index: Integer): IInfraType;
    // *** del procedure SetFeatureName(const Value: string);
    property FeatureName: string read GetFeatureName; // *** del  write SetFeatureName;
  end;

  IVCLCustomListBoxView = interface(IVCLWinControlView)
    ['{FE864693-FABB-4CB4-8990-56EE42E04987}']
    function GetCount: Integer;
    function GetItemIndex: Integer;
    function GetListItemView: IVCLCustomListBoxItemView;
    function GetOwnerDraw: boolean;
    procedure SetCount(Value: Integer);
    procedure SetListItemView(const Value: IVCLCustomListBoxItemView);
    procedure SetOwnerDraw(const Value: boolean);
    property Count: Integer read GetCount write SetCount;
    property ItemIndex: Integer read GetItemIndex;
    property ListItemView: IVCLCustomListBoxItemView read GetListItemView
      write SetListItemView;
    property OwnerDraw: boolean read GetOwnerDraw write SetOwnerDraw;
  end;

  IVCLMenuItemView = interface(IVCLView)
    ['{3B55FFE9-032C-4D39-B186-AF22A5F3C619}']
    function GetClickCommand: ICommand;
    procedure SetClickCommand(const Value: ICommand);
    property ClickCommand: ICommand read GetClickCommand write SetClickCommand;
  end;

  IVCLMenuView = interface(IView)
    ['{3B55FFE9-032C-4D39-B186-AF22A5F3C619}']
    function GetItems: IVCLMenuItemView;
    function GetOwnerDraw: boolean;
    procedure SetOwnerDraw(Value: boolean);
    property Items: IVCLMenuItemView read GetItems;
    property OwnerDraw: boolean read GetOwnerDraw write SetOwnerDraw;
  end;

  // Presenters

  ICustomFormPresenter = interface(IContainerPresenter)
    ['{D2D15D4B-B6CB-44E5-B422-B50098FDF6E2}']
    procedure Execute;
  end;

  IFormPresenterReleaseOnClose = interface(ICustomFormPresenter)
    ['{E2B1548B-E1E2-4387-8AAF-A55ED1BA9C5A}']
    function GetNewInstance(ID: TGUID; const Value: IInfraObject): IElement;
  end;

  IUndoableEditPresenter = interface(IElement)
    ['{DDFF9ECA-B39A-4DBA-AE7C-FE67C271B98A}']
    procedure DoUndoableEdit(const ParentPresenter: IFormPresenterReleaseOnClose;
      const PresenterToEdit: IPresenter; EditPresenterInterface: TGUID);
  end;

  // interactors

  IExitEventModelUpdater = interface(IInteractor)
    ['{63CD8D1F-BF26-4468-A92F-21263A22FD1E}']
  end;

  IClickEventModelUpdater = interface(IInteractor)
    ['{17B9EF5E-919F-4B64-BAA3-52096561F48D}']
  end;

  IObjectSelectionUpdater = interface(IInteractor)
    ['{F2D1524A-ED7A-4CE9-B973-F72C32919ACC}']
  end;

  IKeyBoardInteractor = interface(IInteractor)
    ['{4C696FD6-BA52-4D2D-AE3A-BD3CA7A8A888}']
  end;

  IInputNumberInteractor = interface(IKeyBoardInteractor)
    ['{9BA45A52-9A6A-4C3D-9AB8-A070A5E7F566}']
    function GetAcceptDecimals: boolean;
    function GetAcceptSignal: boolean;
    procedure SetAcceptDecimals(const Value: boolean);
    procedure SetAcceptSignal(const Value: boolean);
    property AcceptDecimals: boolean read GetAcceptDecimals
      write SetAcceptDecimals;
    property AcceptSignal: boolean read GetAcceptSignal write SetAcceptSignal;
  end;

  // commands
  
  ICloseFormCommand = interface(ICommand)
    ['{5A690237-1642-43DF-A222-D6C8DFD7D67C}']
  end;

  ICloseFormWithOKCommand = interface(ICloseFormCommand)
    ['{07F39A8C-DDF6-4B4C-94EE-87828E6C64F9}']
  end;

  // Renderer's

  IVCLCustomListBoxRenderer = interface(IRenderer)
    ['{F5BDAA5A-DC60-4EDC-96E6-3E4D2217CA4A}']
  end;

  IVCLCustomListBoxItemRenderer = interface(IRenderer)
    ['{E692E638-89E5-46A7-B961-D1ADBB6A7DB6}']
    function DoDataFind(const FindString: string): Integer;
    procedure DoGetData(Index: Integer; var Data: string);
    procedure DoDrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure DoDataObject(Index: Integer; var DataObject: TObject);
    procedure DoMeasureItem(Index: Integer; var Height: Integer);
  end;

  IVCLMenuItemRenderer = interface(IRenderer)
    ['{8E6D6E39-2A80-435E-8FD9-BC5345B98BDF}']
    procedure DoAdvancedDrawItem(ACanvas: TCanvas; ARect: TRect;
      State: TOwnerDrawState);
    procedure DoDrawItem(ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure DoMeasureItem(ACanvas: TCanvas; var Width, Height: Integer);
  end;

function MVPMApperService: IMVPMapperService;

implementation

function MVPMApperService: IMVPMapperService;
begin
  Result := ApplicationContext as IMVPMapperService;
end;

end.
