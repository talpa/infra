unit GUIAnnotationLoaderIntf;

interface

uses
  SysUtils, Classes, Contnrs, InfraCommonIntf, InfraGUIBuilderIntf;

type

  IGUIAnnotationLoader = interface(IMemoryManagedObject)
    ['{A40720CD-3117-40BD-AC5E-1911D326C339}']
    procedure Load;
    procedure Save;
    function GetFileName: string;
    function GetGUI: IGUI;
    procedure SetFileName(const Value: string);
    procedure SetGUI(const Value: IGUI);
    property FileName: string read GetFileName write SetFileName;
    property GUI: IGUI read GetGUI write SetGUI;
  end;

implementation

end.
