// xxx
unit InfraOPFIntf;

interface

uses
  Classes,
  SyncObjs,
  {Infra}
  InfraCommonIntf,
  InfraValueTypeIntf,
  {Zeos}
  ZDbcIntfs;

type
  EInfraPersistenceError = class(EInfraError);
  EPersistenceConnectionProviderError = class(EInfraPersistenceError);
  EPersistenceTemplateError = class(EInfraPersistenceError);
  EPersistenceEngineError = class(EInfraPersistenceError);
  EPersistenceParserError = class(EInfraPersistenceError);
  EPersistenceTransactionError = class(EInfraPersistenceError);

  ISession = interface;
  ISQLCommandParams = interface;
  ISessionFactory = interface;
  IDialect = interface;

  TIsolationLevel = (tilNone, tilReadUncommitted, tilReadCommitted,
    tilRepeatableRead, tilSerializable);


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
    procedure LoadFromStream(const Stream: TStream);
    procedure SaveToStream(const Stream: TStream);
    function Clone: IConfiguration;
    function BuildSessionFactory: ISessionFactory;
  end;

  ITransaction = interface(IInterface)
  ['{B9D06F2F-3F18-4AFE-85BF-9525AC1F4773}']
    procedure BeginTransaction(pTransactIsolationLevel: TIsolationLevel = tilReadCommitted);
    procedure Commit;
    procedure Rollback;
  end;

  IConnectionItem = interface
  ['{0580C47C-F40C-48A6-A60E-704EA32DE666}']
    function GetLastAccess: TDateTime;
    function GetRefCount: Integer;
    function Connection: IZConnection;
    procedure Configure(pIsolationLevel: TIsolationLevel);
    property LastAccess: TDateTime read GetLastAccess;
    property RefCount: Integer read GetRefCount;
  end;

  TArrayConnectionItem = array of IConnectionItem;

  IConnectionProvider = interface(IBaseElement)
    ['{E4D7AF34-1750-461D-90E3-15F0DFD3167E}']
    function GetActiveConnections: Integer;
    function GetPoolSize: Integer;
    procedure Lock;
    procedure UnLock;
    function GetItems: TArrayConnectionItem;
    function Acquire: IConnectionItem;
    property ActiveConnections: Integer read GetActiveConnections;
    function GetInternallEvent: TEvent;
    property PoolSize: Integer read GetPoolSize;
  end;

  IInfraPersistenceService = interface(IInterface)
    ['{0DC6F960-B66E-437E-88BD-BD0BAF6CFFE3}']
    function GetConfiguration: IConfiguration;
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
    function GetName: string;
    procedure SetName(const Value: string);
    function GetParams :ISQLCommandParams;
    function GetClassTypeInfo: IClassInfo;
    procedure SetClassTypeInfo(const Value: IClassInfo);
    property ClassTypeInfo: IClassInfo read GetClassTypeInfo write SetClassTypeInfo;
    property Name: string read GetName write SetName;
    property Params: ISQLCommandParams read GetParams;   
  end; 

  ISQLCommandQuery = interface(ISQLCommand)
    ['{437E64D0-7DD8-4D87-9B9F-DBEFAB200863}']
    function GetResult: IInfraType;
    function GetList: IInfraList;
    procedure SetListID(const Value: TGUID);
    function GetListID: TGUID;
    property ListID: TGUID read GetListID write SetListID;
  end;

  ISQLCommandParams = interface
    ['{3882EC5D-59EC-4839-93F8-B4DCDE3B6B37}']
    function GetItem(Index: string): IInfraType;
    procedure SetItem(Index: string; Value: IInfraType);
    function GetCount: Integer;
    function Add(Index: string; Value: IInfraType): string;
    procedure CreateParamsFrom(const Value: IInfraObject);	
    procedure Delete(Index: string);
    procedure DeletePosition(Index: integer);
    procedure Clear;
    function PositionOf(Index: string; Value: IInfraType): integer;
    function ValueOfPosition(Index: Integer): IInfraType;
    function IndexOfPosition(Index: Integer): string;
    property Count: Integer read GetCount;
    property Params[Index: string]: IInfraType read GetItem write SetItem; default;
  end;

  ISQLCacheList = interface
  ['{A05EA7DB-8BDF-4E02-97FE-FC1FAC4F06A3}']
    function GetItem(Index: String) : String;
    procedure SetItem(Index : String; Value : String);
    function GetCount : Integer;
    procedure Delete(Index : String);
    procedure DeletePosition(Index : Integer);
    procedure Clear;
    function PositionOf(Index : String; Value : String) : Integer;
    function ValueOfPosition(Index : Integer) : String;
    function IndexOfPosition(Index : Integer) : String;
    property Count : Integer read GetCount;
    property Items[Index: String]: String read GetItem write SetItem; default;
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
    function CreateNamedQuery(const pCommandName: string; const pObj: IInfraObject): ISQLCommandQuery; overload;
    function CreateNamedQuery(const pCommandName: string; const pClassID: TGUID): ISQLCommandQuery; overload;
    function CreateNamedQuery(const pCommandName: string; const pClassID: TGUID; const pListID: TGUID): ISQLCommandQuery; overload;
    function CreateNamedQuery(const pCommandName: string; const pObj: IInfraObject; const pListID: TGUID): ISQLCommandQuery; overload;
    function Delete(const pCommandName: string; const pObj: IInfraObject): ISQLCommand;
    function Save(const pCommandName: string; const pObj: IInfraObject): ISQLCommand;
    procedure Load(const pObj: IInfraObject);
    function Flush: Integer;
    procedure BeginTransaction(pTransactIsolationLevel: TIsolationLevel = tilReadCommitted);
    procedure Commit;
    procedure Rollback;
  end;

  ISessionFactory = interface(IBaseElement)
    ['{F132108F-D3AB-4D8E-A319-59F8E353FD65}']
    /// Getter da propriedade IsClosed
    function GetIsClosed: Boolean;
    /// Cria uma nova Session
    function OpenSession: ISession; overload;
    /// Fecha essa SessionFactory liberando todos os recursos associados
    procedure Close;
    /// Esta propriedade indica se a SessionFactory est� fechada
    property isClosed: Boolean read GetIsClosed;
  end;

  IPersistenceEngine = interface(IBaseElement)
    ['{F1C7686A-43B6-4FE7-8BF1-6A9C6BC54AE4}']
    procedure Load(const pSqlCommand: ISQLCommandQuery;
      const pList: IInfraList);
    function Execute(const pSqlCommand: ISqlCommand): Integer;
    function ExecuteAll(const pSqlCommands: ISQLCommandList): Integer;
  end;

  ITemplateReader = interface(IElement)
    ['{AFD2D321-E26B-4E48-93FB-48FD24BCE62B}']
    function ReadFromStream(pStream: TStream): string;
    function Read(const pSQLCommand: ISQLCommand): string;
    function GetConfiguration: IConfiguration;
    procedure SetConfiguration(const Value: IConfiguration);
    property Configuration: IConfiguration read GetConfiguration
      write SetConfiguration;
  end;

  ITemplateReader_IO = interface(ITemplateReader)
    ['{01861C33-9789-4A30-8FCC-A018EA45FF13}']
  end;

  ITemplateReader_Build = interface(ITemplateReader)
    ['{091B1C5F-50F3-4579-BDD7-9639A0F5F2DC}']
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

  IEntity = interface(IElement)
    ['{28B94C80-55F6-47BB-9E86-8C38514E1980}']
    procedure SetName( const Value: String);
    function GetName: String;
    property Name: String read GetName write SetName;
  end;

  IColumn = interface(IElement)
    ['{C935A579-F071-44A2-9FE8-E97688943268}']
    procedure SetColumnDefinition( const Value: String);
    function  GetColumnDefinition: String;
    procedure SetInsertable( const Value: Boolean);
    function  GetInsertable: Boolean;
    procedure SetLength( const Value: Integer);
    function  GetLength: Integer;
    procedure SetName(const Value: String);
    function  GetName: String;
    procedure SetNullable( const Value: Boolean);
    function  GetNullable: Boolean;
    procedure SetPrecision(const Value: Integer);
    function  GetPrecision: Integer;
    procedure SetScale(const Value: Integer);
    function  GetScale: Integer;
    procedure SetTable(const Value: String);
    function  GetTable: String;
    procedure SetUnique(const Value: Boolean);
    function  GetUnique: Boolean;
    procedure SetUpdatable(const Value: Boolean);
    function  GetUpdatable: Boolean;
    property ColumnDefinition: String read GetColumnDefinition write SetColumnDefinition;
    property Insertable: Boolean read GetInsertable write SetInsertable;
    property Length: Integer read GetLength write SetLength;
    property Name: String read GetName write SetName;
    property Nullable: Boolean read GetNullable write SetNullable;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Scale: Integer read GetScale write SetScale;
    property Table: String read GetTable write SetTable;
    property Unique: Boolean read GetUnique write SetUnique;
    property Updatable: Boolean read GetUpdatable write SetUpdatable;
  end;

  ICollumns = interface(IBaseElement)
    ['{3B6F6AE0-EB84-47F2-A009-172D235C26A4}']
  end;

  IId = interface(IElement)
    ['{FCA085B9-06FB-445B-9D95-786F9B5BD07D}']
  end;

  ISelectBuilder = interface
    ['{5F484BB5-C2B5-4BAD-A9C6-7BFD5B6FD241}']
    function ToStatementString: String;
    procedure SetFromClause(const pTableName, pAlias: String); overload;
    procedure SetFromClause(const Value: String); overload;
    procedure SetGroupByClause(const Value: String);
    procedure SetOrderByClause(const Value: String);
    procedure SetOuterJoins(const pOuterJoinsAfterFrom,
      pOuterJoinsAfterWhere: String);
    procedure SetSelectClause(const Value: String);
    procedure SetWhereClause(const Value: String); overload;
  end;

  IDeleteBuilder   = interface
    ['{FD1DEBA9-BB94-43CC-B976-B61B74E5F527}']
    procedure SetTableName(const pTableName: String);
    procedure SetWhere(const Value: String);
    procedure setPrimaryKeyColumnNames(const Value: String);
    function toStatementString: String;
  end;

  IDialect = interface(IBaseElement)
    ['{0325E2B1-72CA-46A9-87F5-385E6FBC7AD7}']
  end;
  
  TStringHelper = class
    class function StartsWith(const Value, pFragment: string): boolean;
    class function Substring(const Value: string; pCount: integer): string;
    class function Join(const pSeparator: string; pList: TStrings): string;
  end;

function PersistenceService: IInfraPersistenceService;

implementation

uses
  InfraOPFTemplates,
  InfraOPFAnnotation,
  // Essas units foram adicionadas para que os drivers
  // possam se registrar no DriverManager
  ZDbcInterbase6,
  ZDbcPostgreSql,
  ZDbcMySql,
  ZDbcSqLite,
  ZDbcAdo,
  ZDbcASA,
  ZDbcDbLib,
  ZDbcOracle;

function PersistenceService: IInfraPersistenceService;
begin
  Result := ApplicationContext as IInfraPersistenceService;
end;

{ TStringHelper }

class function TStringHelper.Join(const pSeparator: string;
  pList: TStrings): string;
var
  i : Integer;
begin
  if pList.Count <> 0 then
  begin
    Result := pList[0];
    for i:= 1 to pList.Count - 1 do
      Result := Result + pSeparator + pList[i];
  end
  else
    Result := '';
end;

class function TStringHelper.StartsWith(const Value,
  pFragment: string): boolean;
begin
  Result := Copy(Value, 0, Length(pFragment)) = pFragment;
end;

class function TStringHelper.Substring(const Value: string;
  pCount: integer): string;
begin
  Result := Copy(Value, pCount-1, Length(Value));
end;

initialization
  InfraOPFTemplates.RegisterOnReflection;
  InfraOPFAnnotation.RegisterZeosTypeMapping;

end.
