unit InfraBindingIntf;

interface

uses
  Controls,
  Classes,
  {Infra}
  InfraCommonIntf,
  InfraValueTypeIntf;

type
  EInfraBindingError = class(EInfraError);

  TBindingMode = (bmLeftToRight, bmTwoWay);
  TUpdateTrigger = (utLostFocus, utPropertyChanged, utExplicit);
  TListModelOperation = (loNone, loAdd, loPutObject, loRemove, loClear,
    loRefresh, loExchange, loSelectionChange);
  TPropertyAccessMode = (paRTTI, paCustom);

  IBindable = interface(IElement)
    ['{E4FF9385-092B-422B-8BCB-0A28CB611C82}']
    function GetUpdating: boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetValue: IInfraType;
    function Support2Way: Boolean;
    procedure SetValue(const Value: IInfraType);
    property Value: IInfraType read GetValue write SetValue;
    property Updating: boolean read GetUpdating;
  end;

  IBindableInfraType = interface(IBindable)
    ['{91B97C6D-6F56-462E-941B-C7E236E71F26}']
  end;

  IBindableInfraList = interface(IBindable)
    ['{A6DC67DE-E637-400A-A372-2BC57C47D9A4}']
  end;

  IBindableVCLProperty = interface(IBindable)
    ['{36236C4B-607B-4287-8D0E-A617832F17CC}']
    function GetControl: TControl;
    property Control: TControl read GetControl;
  end;

  IBindableListModel = interface(IInfraType)
    ['{FAAFAA1A-9CCD-4FEA-BDE2-A7D74C3013EF}']
    function GetCurrent: IInfraType;
    function GetExpression: string;
    function GetItemIndex: integer;
    function GetItemOperated: IInfraType;
    function GetList: IInfraType;
    function GetOperation: TListModelOperation;
    function GetValueOfExpression: string;
    procedure SetCurrent(const Value: IInfraType);
    procedure SetExpression(const Value: string);
    procedure SetItemIndex(Value: integer);
    procedure SetItemOperated(const Value: IInfraType);
    procedure SetList(const Value: IInfraType);
    procedure SetOperation(Value: TListModelOperation);
    property Current: IInfraType read GetCurrent write SetCurrent;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property ItemOperated: IInfraType read GetItemOperated write SetItemOperated;
    property List: IInfraType read GetList write SetList;
    property Operation: TListModelOperation read GetOperation write SetOperation;
    property Expression: string read GetExpression write SetExpression;
  end;

  IBinding = interface(IBaseElement)
    ['{A5945A5F-F235-4BA5-857D-F1BF67660FE3}']
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetName: string;
    procedure SetName(const Value: string);
    function GetMode: TBindingMode;
    function GetLeft: IBindable;
    function GetRight: IBindable;
    function GetConverter: ITypeConverter;
    function GetConverterParameter: IInfraType;
    procedure SetMode(Value: TBindingMode);
    procedure SetConverter(const Value: ITypeConverter);
    procedure SetConverterParameter(const Value: IInfraType);
    procedure UpdateLeft;
    function TwoWay: IBinding;
    property Mode: TBindingMode read GetMode write SetMode;
    property Left: IBindable read GetLeft;
    property Right: IBindable read GetRight;
    property Converter: ITypeConverter read GetConverter write SetConverter;
    property ConverterParameter: IInfraType read GetConverterParameter
      write SetConverterParameter;
    property Active: boolean read GetActive write SetActive;
    property Name: string read GetName write SetName;
  end;

  IBindingList = interface(IBinding)
    procedure AddSelection(const pPropertyName: string);
  end;

  IBindings = interface
    ['{20CF48B1-FAF9-4366-8C70-76D65D071407}']
    function Add(const Item: IBinding): Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): IBinding;
    function NewIterator: IInfraIterator;
    procedure Clear;
    property Items[Index: Integer]: IBinding read GetItem; default;
    property Count: Integer read GetCount;
  end;

  INotifyValueChanged = interface(IInfraEvent)
    ['{28CC7946-79CD-4029-9A4E-3A6947330BEC}']
  end;

  IBindManager = interface(IBaseElement)
    ['{177A1F92-3FF9-4D48-B40F-93CAE8FABE45}']
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetDataContext: IInfraType;
    procedure SetDataContext(const Value: IInfraType);
    function Add(const pLeft, pRight: IBindable;
      const pConverter: ITypeConverter = nil): IBinding; overload;
    function Add(
      pLeftControl: TControl; const pLeftProperty: string;
      pRightControl: TControl; const pRightProperty: string;
      const pConverter: ITypeConverter = nil): IBinding; overload;
    function Add(const pLeftProperty: string;
      pRightControl: TControl; const pRightProperty: string = '';
      const pConverter: ITypeConverter = nil): IBinding; overload;
    function AddList(const pLeftExpression: string;
      pRightControl: TControl; const pRightProperty: string): IBindingList;
    procedure ClearBindings;
    property DataContext: IInfraType read GetDataContext write SetDataContext;
    property Active: boolean read GetActive write SetActive;
  end;

  IInfraBindingService = interface(IBaseElement)
    ['{306425B2-4590-49C8-A4CE-62F5293F1820}']
    function GetNewBindManager: IBindManager;
  end;

function BindingService: IInfraBindingService;

implementation

uses
  // DO NOT REMOVE!!!! Necessary to Register Binding on ApplicationContext
  InfraBinding;

function BindingService: IInfraBindingService;
begin
  Result := ApplicationContext as IInfraBindingService;
end;

end.


