unit InfraBindingIntf;

interface

uses
  {Infra}
  InfraCommonIntf,
  InfraValueTypeIntf;

type
  TBindingMode = (bmLeftToRight, bmTwoWay);
  TUpdateTrigger = (utLostFocus, utPropertyChanged, utExplicit);

  IBindable = interface(IBaseElement)
    ['{E9FE85E6-91D1-49DA-9A07-BF2F61C2B16A}']
    function GetValue: IInfraType;
    procedure SetValue(const Value: IInfraType);

    property Value: IInfraType read GetValue write SetValue;
  end;

  IBindableControl = interface(IBindable)
    ['{E87E4DE0-B553-4C40-8220-3B16EBBF78EC}']
    function GetPropertyPath: String;
    property PropertyPath: String read GetPropertyPath;
  end;

  IInfraTypeBindable = interface(IBindable)
    ['{F4EAFEA7-323E-4BCE-A068-358E3517A1B1}']
  end;

  IValueConverter = interface(IBaseElement)
    ['{69019EE6-40A5-42DF-9E4F-0DFC17073196}']
  end;

  IBinding = interface(IBaseElement)
    ['{A3171542-76CB-4FD0-8689-70AD8785B727}']
    function GetMode: TBindingMode;
    function GetLeft: IBindable;
    function GetRight: IBindable;
    function GetValueConverter: IValueConverter;
    procedure SetMode(Value: TBindingMode);
    procedure SetLeft(Value: IBindable);
    procedure SetRight(Value: IBindable);
    procedure SetValueConverter(Value: IValueConverter);
    procedure UpdateLeft;
    function TwoWay: IBindable;
    property Mode: TBindingMode read GetMode write SetMode;
    property Left: IBindable read GetLeft write SetLeft;
    property Right: IBindable read GetRight write SetRight;
    property ValueConverter: IValueConverter read GetValueConverter write SetValueConverter;
  end;

  IBindManager = interface(IBaseElement)
    ['{78F93777-11FF-4980-93E7-BABBDA7648BB}']
    procedure Add(Binding: IBinding); overload;
//    procedure Add(pSourceControl: TControl;
//      const pSourcePath: string;
//         pTargetControl: TControl;
//      const TargetProperty: string = '';
//          ValueConverter: IValueConverter = nil); overload;
//    procedure add(const pSourcePath: string; pTargetControl: TControl;
//      const TargetProperty: string = ''; ValueConverter: IValueConverter = nil);   overload;
    procedure ClearBindings ;
  end;

implementation

end.

