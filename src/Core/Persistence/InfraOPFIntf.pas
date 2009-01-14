// xxx
unit InfraOPFIntf;

interface

uses
  Classes,
  {Zeos}
  ZDbcIntfs,
  {Infra}
  InfraCommonIntf,
  InfraValueTypeIntf;

type
  EInfraPersistenceError = class(EInfraError);
  EPersistenceConnectionProviderError = class(EInfraPersistenceError);
  EPersistenceTemplateError = class(EInfraPersistenceError);
  EPersistenceEngineError = class(EInfraPersistenceError);
  EInfraParserError = class(EInfraError);

  ISession = interface;  
  ISQLCommandParams = interface;                  

  IConfiguration = interface(IBaseElement)
    ['{16AF1EFF-FB48-4BAD-BDC7-E0518E83E09E}']
    function GetAsInteger(const pName: string): Integer; overload;
    function GetAsDouble(const pName: string): Double; overload;
    function GetAsString(const pName: string): string; overload;
    function GetValue(const pName: string; const pDefaultValue: Integer): Integer; overload;
    function GetValue(const pName: string; const pDefaultValue: Double): Double; overload;
    function GetValue(const pName: string; const pDefaultValue: string): string; overload;
    procedure SetValue(const pName: string; const Value: Integer); overload;
    procedure SetValue(const pName: string; const Value: Double); overload;
    procedure SetValue(const pName: string; const Value: string); overload;
    procedure Clear;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
  end;

  IConnectionProvider = interface(IBaseElement)
    ['{E4D7AF34-1750-461D-90E3-15F0DFD3167E}']
    function GetConnection: IZConnection;
    procedure ReleaseConnection(const pConnection: IZConnection);
  end;

  IInfraPersistenceService = interface(IInterface)
    ['{0DC6F960-B66E-437E-88BD-BD0BAF6CFFE3}']
    function GetConfiguration: IConfiguration;
    function OpenSession: ISession; overload;
    procedure SetConnection(const pConnection: IZConnection);
    property Configuration: IConfiguration read GetConfiguration;
  end;

  TPersistentStateKind = (osClean, osDirty, osDeleted);

  IPersistentState = interface
    ['{0968A2E4-4195-4843-A0D7-2FE24053EA38}']
    function GetIsPersistent: Boolean;
    function GetState: TPersistentStateKind;
    procedure SetIsPersistent(Value: Boolean);
    procedure SetState(Value: TPersistentStateKind);
    property IsPersistent: Boolean read GetIsPersistent write SetIsPersistent;
    property State: TPersistentStateKind read GetState write SetState;
  end;

  ISQLCommand = interface(IBaseElement)
    ['{8F2E7318-09C1-4EA2-BA6E-6724275E9043}']
    function GetName: String;
    procedure SetName(const Value: String);
    function GetParams :ISQLCommandParams;
    property Name: string read GetName write SetName;
    property Params: ISQLCommandParams read GetParams;   
  end; 

  ISQLCommandQuery = interface(ISQLCommand)
    ['{437E64D0-7DD8-4D87-9B9F-DBEFAB200863}']
    function GetResult: IInfraType;
    function GetList: IInfraList;
    function GetListID: TGUID;
    function GetClassID:TGUID;
    procedure SetListID(const Value: TGUID);
    procedure SetClassID(const Value: TGUID);
    property ClassID: TGUID read GetClassID write SetClassID;
    property ListID: TGUID read GetListID write SetListID;
  end;

  ISQLCommandParams = interface
    ['{3882EC5D-59EC-4839-93F8-B4DCDE3B6B37}']
    function GetItem(Index: String): IInfraType;
    procedure SetItem(Index: String; Value: IInfraType);
    function GetCount: Integer;
    function Add(Index: String; Value: IInfraType): String;
    procedure CreateParamsFrom(const Value: IInfraObject);	
    procedure Delete(Index: String);
    procedure DeletePosition(Index: integer);
    procedure Clear;
    function PositionOf(Index: String; Value: IInfraType): integer;
    function ValueOfPosition(Index: Integer): IInfraType;
    function IndexOfPosition(Index: Integer): String;
    property Count: Integer read GetCount;
    property Params[Index: String]: IInfraType read GetItem write SetItem; default;
  end;

  ISQLCommandList = interface
    ['{3882EC5D-59EC-4839-93F8-B4DCDE3B6B37}']
    function Add(const Item: ISQLCommand): Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): ISQLCommand;
    procedure Clear;

    property Items[Index: Integer]: ISQLCommand read GetItem; default;
    property Count: Integer read GetCount;
  end;

  ISession = interface(IBaseElement)
    ['{693A7815-9A5E-46C7-97DD-04D3E9C245AF}']
    function CreateNamedQuery(const pCommandName: string;
      const pObj: IInfraObject = nil): ISQLCommandQuery; overload;
    function CreateNamedQuery(const pCommandName: string;
      const pClassID: TGUID): ISQLCommandQuery; overload;
    function CreateNamedQuery(const pCommandName: string;
      const pClassID: TGUID; const pListID: TGUID): ISQLCommandQuery; overload;
    function CreateNamedQuery(const pCommandName: string; const pObj: IInfraObject;
      const pListID: TGUID): ISQLCommandQuery; overload;
    function Delete(const pCommandName: string;
      const pObj: IInfraObject): ISQLCommand;
    function Save(const pCommandName: string;
      const pObj: IInfraObject): ISQLCommand;
    function Flush: Integer;
  end;

  IPersistenceEngine = interface(IBaseElement)
    ['{F1C7686A-43B6-4FE7-8BF1-6A9C6BC54AE4}']
    procedure SetConnection(const pConnection: IZConnection);
    function GetConfiguration: IConfiguration;
    function GetConnectionProvider: IConnectionProvider;
    procedure Load(const pSqlCommand: ISQLCommandQuery;
      const pList: IInfraList);
    function Execute(const pConnection: IZConnection; const pSqlCommand: ISqlCommand): Integer;
    property ConnectionProvider: IConnectionProvider read GetConnectionProvider;
    property Configuration: IConfiguration read GetConfiguration;
  end;

  ITemplateReader = interface(IElement)
    ['{AFD2D321-E26B-4E48-93FB-48FD24BCE62B}']
    function ReadFromStream(const pStream: TStream): string;
    function Read(const pTemplateName: string): string;
    function GetConfiguration: IConfiguration;
    procedure SetConfiguration(const Value: IConfiguration);
    property Configuration: IConfiguration read GetConfiguration
      write SetConfiguration;
  end;

  ITemplateReader_IO = interface(ITemplateReader)
    ['{01861C33-9789-4A30-8FCC-A018EA45FF13}']
  end;

  ISQLParamsParser = interface(IBaseElement)
    ['{C0D4B607-4224-44C0-A93C-F10658AE9738}']
    function Parse(const pSQL: string): string;
    function GetParams: TStrings;
    function GetMacroParams: TStrings;
  end;

  TZTypeSetter = procedure (const pStatement: IZPreparedStatement;
    pIndex: Integer; const pParamValue: IInfraType);

  TZTypeGetter = procedure (const pResultSet: IZResultSet;
    pIndex: Integer; const pPropertyValue: IInfraType);

  IZTypeAnnotation = interface(IElement)
    ['{224B7552-1AB1-456B-B5C5-C7A85BA60580}']
    function GetNullSafeGetter: TZTypeGetter;
    function GetNullSafeSetter: TZTypeSetter;
    property NullSafeGet: TZTypeGetter read GetNullSafeGetter;
    property NullSafeSet: TZTypeSetter read GetNullSafeSetter;
    procedure Init(pGetter: TZTypeGetter; pSetter: TZTypeSetter);
  end;

function PersistenceService: IInfraPersistenceService;

implementation

uses
  InfraOPFTemplates,
  InfraOPFAnnotation;

function PersistenceService: IInfraPersistenceService;
begin
  Result := ApplicationContext as IInfraPersistenceService;
end;

initialization
  InfraOPFTemplates.RegisterOnReflection;
  InfraOPFAnnotation.RegisterZeosTypeMapping;

end.

