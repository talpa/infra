unit InfraBindingIntf;

interface

uses
  Controls,
  {Infra}
  InfraCommonIntf,
  InfraValueTypeIntf;

type
  EInfraBindingError = class(EInfraError);

  TBindingMode = (bmLeftToRight, bmTwoWay);
  TUpdateTrigger = (utLostFocus, utPropertyChanged, utExplicit);

  IBindable = interface(IBaseElement)
    ['{9ABF8CA0-E75A-48A6-9C20-4E3FDB781A2F}']
    function GetValue: IInfraType;
    procedure SetValue(const Value: IInfraType);
    property Value: IInfraType read GetValue write SetValue;
  end;

  IBindableControl = interface(IBindable)
    ['{CDDB1FF4-D5DC-4BFC-9D6A-A20578D54724}']
    function GetPropertyPath: String;
    property PropertyPath: String read GetPropertyPath;
  end;

  IBinding = interface(IBaseElement)
    ['{CD591A5F-2A29-4605-A398-707F5839F4EB}']
    function GetMode: TBindingMode;
    function GetLeft: IBindable;
    function GetRight: IBindable;
    function GetValueConverter: ITypeConverter;
    procedure SetMode(Value: TBindingMode);
    procedure SetLeft(const Value: IBindable);
    procedure SetRight(const Value: IBindable);
    procedure SetValueConverter(const Value: ITypeConverter);
    procedure UpdateLeft;
    function TwoWay: IBinding;
    property Mode: TBindingMode read GetMode write SetMode;
    property Left: IBindable read GetLeft write SetLeft;
    property Right: IBindable read GetRight write SetRight;
    property ValueConverter: ITypeConverter read GetValueConverter write SetValueConverter;
  end;

  IBindManager = interface(IBaseElement)
    ['{19C48BD7-62C9-4C55-9A86-1702B7BA5E61}']
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
  end;

  IBindingList = interface
    ['{F48F2F24-9324-42B3-AC64-93C15CFA5C4C}']
    function Add(const Item: IBinding): Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): IBinding;
    procedure Clear;
    property Items[Index: Integer]: IBinding read GetItem; default;
    property Count: Integer read GetCount;
  end;

  IInfraBindingService = interface(IInterface)
    ['{AE2A98DE-01B4-49BB-9225-F349C68D2DCD}']
    function GetNewBindManager: IBindManager;
  end;
  
implementation

function BindingService: IInfraBindingService;
begin
  Result := ApplicationContext as IInfraBindingService;
end;

end.
