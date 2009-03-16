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

  IBindable = interface(IElement)
    ['{E4FF9385-092B-422B-8BCB-0A28CB611C82}']
    function GetValue: IInfraType;
    function Support2Way: Boolean;
    procedure SetValue(const Value: IInfraType);
    property Value: IInfraType read GetValue write SetValue;
  end;

  IBindableInfraType = interface(IBindable)
    ['{91B97C6D-6F56-462E-941B-C7E236E71F26}']
  end;

  IBindableVCLProperty = interface(IBindable)
    ['{36236C4B-607B-4287-8D0E-A617832F17CC}']
    function GetControl: TControl;
    property Control: TControl read GetControl;
  end;

  IBinding = interface(IBaseElement)
    ['{A5945A5F-F235-4BA5-857D-F1BF67660FE3}']
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetMode: TBindingMode;
    function GetLeft: IBindable;
    function GetRight: IBindable;
    function GetValueConverter: ITypeConverter;
    procedure SetMode(Value: TBindingMode);
    procedure SetValueConverter(const Value: ITypeConverter);
    procedure UpdateLeft;
    function TwoWay: IBinding;
    property Mode: TBindingMode read GetMode write SetMode;
    property Left: IBindable read GetLeft;
    property Right: IBindable read GetRight;
    property ValueConverter: ITypeConverter read GetValueConverter write SetValueConverter;
    property Active: boolean read GetActive write SetActive;
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
    procedure ClearBindings;
    property DataContext: IInfraType read GetDataContext write SetDataContext;
    property Active: boolean read GetActive write SetActive;
  end;
  
  IBindingList = interface
    ['{20CF48B1-FAF9-4366-8C70-76D65D071407}']
    function Add(const Item: IBinding): Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): IBinding;
    function NewIterator: IInfraIterator;
    procedure Clear;
    property Items[Index: Integer]: IBinding read GetItem; default;
    property Count: Integer read GetCount;
  end;

  IInfraBindingService = interface(IInterface)
    ['{306425B2-4590-49C8-A4CE-62F5293F1820}']
    function GetNewBindManager: IBindManager;
  end;

  IInfraValueConverter=interface(IBaseElement)
  ['{0E7CEE73-3C3A-4434-A841-E88E2CD313BC}']
  end;


function BindingService: IInfraBindingService;

implementation

function BindingService: IInfraBindingService;
begin
  Result := ApplicationContext as IInfraBindingService;
end;

end.
