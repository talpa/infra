unit InfraHibernateIntf;

interface

uses
  Classes, InfraCommonIntf, InfraValueTypeIntf;

type
  TTransactionIsolation = (tiNone, tiReadCommit, tiReadUnCommitted,
    tiReadCommitted, tiRepeatableRead, Serializable);

  IConfiguration = interface;
  IConnection = interface;
  IConnectionProvider = interface;
  ISessionFactory = interface;
  ISession = interface;
  IResultSet = interface;
  ITransaction = interface;
  IDialect = interface;

  IPersistenceService = interface(IMemoryManagedObject)
    ['{0DC6F960-B66E-437E-88BD-BD0BAF6CFFE3}']
    function GetConfiguration: IConfiguration;
    function GetSessionFactory: ISessionFactory;
    property Configuration: IConfiguration read GetConfiguration;
    property SessionFactory: ISessionFactory read GetSessionFactory;
  end;

  IConfiguration = interface(IElement)
    ['{560F3DB6-C462-453D-8E18-B6F086B1AE41}']
    function GetProperties: TStrings;
    function GetPropertyItem(const pName: String): string;
    procedure SetPropertyItem(const pName: String; const Value: string);
    property Properties: TStrings read GetProperties;
    property PropertyItem[const pName: String]: string read GetPropertyItem
        write SetPropertyItem;
  end;

  ISessionFactory = interface(IElement)
    ['{98AA1387-1C54-4E68-A3E3-1DDB8470E6CF}']
    function GetConnectionProvider: IConnectionProvider;
    function GetDialect: IDialect;
    function OpenSession: ISession; overload;
    function OpenSession(const pConnection: IConnection): ISession; overload;
    property ConnectionProvider: IConnectionProvider read GetConnectionProvider;
    property Dialect: IDialect read GetDialect;
  end;

  ISession = interface(IElement)
    ['{244258B8-5A25-48D1-B4BC-804A76D5920D}']
    function BeginTransaction: ITransaction;
    procedure Clear;
    procedure Close;
    procedure Delete(const pObject: IInfraType);
    function GetConnection: IConnection;
    function GetIsDirty: Boolean;
    function GetSessionFactory: ISessionFactory;
    procedure SetConnection(const Value: IConnection);
    procedure Load(const pObject, pOID: IInfraType);
    procedure Save(const pObject: IInfraType);
    procedure SetIsDirty(Value: Boolean);
    property Connection: IConnection read GetConnection write SetConnection;
    property IsDirty: Boolean read GetIsDirty write SetIsDirty;
    property SessionFactory: ISessionFactory read GetSessionFactory;
  end;

  IConnectionProvider = interface(IElement)
    ['{E4D7AF34-1750-461D-90E3-15F0DFD3167E}']
    procedure Close;
    procedure CloseConnection(const pConnection: IConnection);
    function GetConnection: IConnection;
  end;

  ITransaction = interface(IElement)
    ['{73827860-919C-4416-B43F-AD3727D122F5}']
  end;

  IDialect = interface(IElement)
    ['{0325E2B1-72CA-46A9-87F5-385E6FBC7AD7}']
  end;

  IConnection = interface(IElement)
    ['{FA23555A-3724-474D-A318-D37CBC3390D3}']
    procedure Close;
    procedure Commit;
    function ExecuteQuery(const pSQL: String): IResultSet;
    function ExecuteUpdate(const pSQL: String): Integer;
    function GetIsClosed: Boolean;
    function GetTransactionIsolation: TTransactionIsolation;
    procedure Rollback;
    procedure SetTransactionIsolation(Value: TTransactionIsolation);
    property IsClosed: Boolean read GetIsClosed;
    property TransactionIsolation: TTransactionIsolation read
        GetTransactionIsolation write SetTransactionIsolation;
  end;

  IResultSet = interface(IElement)
    ['{A4061B4D-420C-4954-B627-AD8CD699CA7A}']
    function EOF: Boolean;
    procedure First;
    function GetFieldValue(Index: Integer): IInfraType; overload;
    function GetFieldValue(const FieldName: String): IInfraType; overload;
    procedure Next;
  end;

implementation

end.
