unit InfraBindingInf;

interface
uses
  Classes,
  SyncObjs,
  {Infra}
  InfraCommonIntf,
  InfraValueTypeIntf;

type
  TBindingMode = (bmOneWay, bmTwoWay);
  TUpdateTrigger = (utLostFocus, utPropertyChanged, utExplicit);

  IBindable = interface(IBaseElement)
    ['{E9FE85E6-91D1-49DA-9A07-BF2F61C2B16A}']
  end;

  IValueConverter = interface(IBaseElement)
    ['{69019EE6-40A5-42DF-9E4F-0DFC17073196}']
  end;

  IBinding = interface(IBaseElement)
    ['{A3171542-76CB-4FD0-8689-70AD8785B727}']
    function getMode: TBindingMode;
    function getSource: IBindable;
    function getSourceProperty: string;
    function getTarget: IBindable;
    function getTargetProperty: string;
    function getTwoWay: IBinding;
    function getValueConverter: IValueConverter;
    procedure setMode(value: TBindingMode);
    procedure setSource(value: IBindable);
    procedure setSourceProperty(value: string);
    procedure setTarget(value: IBindable);
    procedure setTargetProperty(value: string);
    procedure setTwoWay(value: IBinding);
    procedure setValueConverter(value: IValueConverter);
    procedure UpdateSource;
    property Mode: TBindingMode read getMode write setMode;
    property Source: IBindable read getSource write setSource;
    property SourceProperty: string read getSourceProperty write setSourceProperty;
    property Target: IBindable read getTarget write setTarget;
    property TargetProperty: string read getTargetProperty write setTargetProperty;
    property TwoWay: IBinding read getTwoWay write setTwoWay;
    property ValueConverter: IValueConverter read getValueConverter write setValueConverter;
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

