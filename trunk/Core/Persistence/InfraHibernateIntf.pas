unit InfraHibernateIntf;

interface

uses
  Classes, InfraCommonIntf, InfraValueTypeIntf, ZDbcIntfs;

type
  // Tipo de heranca
  TInheritanceType = (itSINGLE_TABLE, itJOINED, itTABLE_PER_CLASS);

  // Nivel de isolamento de uma transação
  TTransactionIsolation = (tiNone, tiReadCommit, tiReadUnCommitted,
    tiReadCommitted, tiRepeatableRead, Serializable);

  IConfiguration = interface;
  IConnectionProvider = interface;
  ISessionFactory = interface;
  ISession = interface;
  ITransaction = interface;
  IDialect = interface;
  IEntityPersister  = interface;
  ICriteria = interface;

  {
    Servico de Persistência do InfraHibernate.
    O programador poderá chamar PersistenceService para obter um objeto deste tipo
    e com isso poderá definir as configurações e obter a fábrica de Sessions.
  }
  IPersistenceService = interface(IInterface)
    ['{0DC6F960-B66E-437E-88BD-BD0BAF6CFFE3}']
    // Retorna um objeto Configuration, Criando caso nao exista
    function GetConfiguration: IConfiguration;
    // Retorna um objeto SessionFactory a partir do Configuration
    function GetSessionFactory: ISessionFactory;
    property Configuration: IConfiguration read GetConfiguration;
    property SessionFactory: ISessionFactory read GetSessionFactory;
  end;

  { Responsável por retornar informações do mapeamento de acordo
    com o que foi anotado no ClassInfo, em suas propriedades ou métodos.
    Caso nao tenha havido determinada anotação o PersistentClass já
    retorna algum valor padrão }
  IPersistentClass = interface(IBaseElement)
    ['{45E8AD80-7543-4A99-A19F-71FFC53D61DF}']
    function GetEntityName: string;
    function IsIdentifierColumn(const pPropertyInfo: IPropertyInfo): boolean;
    function GetColumnName(const pPropertyInfo: IPropertyInfo): string;
    function GetEntityTypeInfo: IClassInfo;
    procedure SetEntityTypeInfo(const Value: IClassInfo);
    property EntityTypeInfo: IClassInfo read GetEntityTypeInfo write SetEntityTypeInfo;
  end;

  {
    Permite a aplicação especificar propriedades a serem usadas quando criando
    um SessionFactory. Mantem uma lista do tipo chave-valor, com as diversas
    configurações necessárias ao servico de persistência.
    Alguns exemplos destas configurações são:
    - string de conexão;
    - tipo de classe a ser criado ao conectar;
    - usuario e senha;
    Pode-se usar PropertyItem[Chave] para ler/gravar o valor da Chave.
  }
  IConfiguration = interface(IBaseElement)
    ['{560F3DB6-C462-453D-8E18-B6F086B1AE41}']
    function GetProperties: TStrings;
    // Retorna o valor de um parametro com base na chave passada
    function GetPropertyItem(const pName: String): string;
    // Cria e retorna uma SessionFactory.
    function BuildSessionFactory: ISessionFactory;
    // Altera o valor de uma determinada chave de configuração.
    procedure SetPropertyItem(const pName: String; const Value: string);
    // A lista de configurações com tuplas chaves-valor
    property Properties: TStrings read GetProperties;
    property PropertyItem[const pName: String]: string read GetPropertyItem
        write SetPropertyItem;
  end;

  // Uma fábrica de objetos Session a serem usadas para persistencia.
  ISessionFactory = interface(IBaseElement)
    ['{98AA1387-1C54-4E68-A3E3-1DDB8470E6CF}']
    function GetConnectionProvider: IConnectionProvider;
    // Cria e retorna um objeto dialeto com base no que foi definido nas 
    // propriedades do Configuration.
    function GetDialect: IDialect;
    // Abre uma nova sessão utilizando o connection obtido do ConnectionProvider.
    function OpenSession: ISession; overload;
    // Retorna o EntityPersister referente ao tipo passado
    function GetEntityPersister(const pEntityID: TGUID): IEntityPersister;
    // Cria/Retorna um Session usando o Connection passado como parametro.
    function OpenSession(const pConnection: IZConnection): ISession; overload;
    property ConnectionProvider: IConnectionProvider read GetConnectionProvider;
    property Dialect: IDialect read GetDialect;
  end;

  {
    Interface interna para que o hibernate tenha acesso ao session. 
  }
  IInternalSession = interface
    ['{6AEB35A8-DB61-4377-A8DC-FC1E49350ED7}']
    function List(const Criteria: ICriteria): IInfraList;
  end;

  {
    Implementação concreta de uma Sessão. Uma Sessao é o ponto central de
    utilização do servico de persistência pelo programador. É a partir desta
    classe que o programador irá tratar os objetos (carga, gravação e exclusão).
  }
  ISession = interface(IBaseElement)
    ['{244258B8-5A25-48D1-B4BC-804A76D5920D}']
    function GetConnection: IZConnection;
    function GetSessionFactory: ISessionFactory;
    function Load(const pEntityID: TGUID;
      const pOID: IInfraType): IInfraType; overload;
    function Load(const pInstanceToLoad: IInfraType;
      const pOID: IInfraType): IInfraType; overload;
    procedure SetConnection(const Value: IZConnection);
    function CreateCriteria(const pTypeID: TGUID): ICriteria; overload;
    function CreateCriteria(const pTypeInfo: IClassInfo): ICriteria; overload;
    property Connection: IZConnection read GetConnection write SetConnection;
    property SessionFactory: ISessionFactory read GetSessionFactory;
  end;

  IEntityPersister  = interface(IBaseElement)
    ['{C589237F-0BD5-4FCD-90E9-0423C9EA5ECD}']
    function GetPersistentClass: IPersistentClass;
    function Load(const pOID: IInfraType; const pOptionalInstance: IInfraType;
      const pSession: ISession): IInfraType;
    function Instantiate(const pOID: IInfraType): IInfraType;
    function GetColumnName(const pPropertyInfo: IPropertyInfo): string;
    function GetAllColumns: IMemberInfoList;
    function GetSQLSnapshotSelectString: string;
    property PersistentClass: IPersistentClass read GetPersistentClass;
    property SQLSnapshotSelectString: string read GetSQLSnapshotSelectString;
  end;

  // Map de EntityPersisters
  IEntityPersisters = interface(IBaseElement)
    ['{D12A7D8D-8FD1-40D5-9AC0-21C8FE1B921C}']
    function Add(ID: TGUID; Item: IEntityPersister): TGUID;
    function First: IEntityPersister;
    function GetCount: Integer;
    function GetItem(Index: TGUID): IEntityPersister;
    function IndexOf(Item: IEntityPersister): TGUID;
    function IndexOfPosition(Index: Integer): TGUID;
    function Last: IEntityPersister;
    function Remove(Item: IEntityPersister): TGUID;
    function ValueOfPosition(Index: Integer): IEntityPersister;
    procedure Clear;
    procedure Delete(Index: TGUID);
    procedure SetItem(Index: TGUID; Item: IEntityPersister);
    property Count: Integer read GetCount;
    property Items[Index: TGUID]: IEntityPersister read GetItem
      write SetItem; default;
  end;

  // Provedor de conexões para o servico de persistencia.
  IConnectionProvider = interface(IBaseElement)
    ['{E4D7AF34-1750-461D-90E3-15F0DFD3167E}']
    procedure Close;
    procedure CloseConnection(const pConnection: IZConnection);
    function GetConnection: IZConnection;
  end;

  // Define uma trasação (ainda nao sendo utilizado)
  ITransaction = interface(IBaseElement)
    ['{73827860-919C-4416-B43F-AD3727D122F5}']
  end;

  //  Define o dialeto do banco de dados (ainda nao sendo utilizado)
  IDialect = interface(IBaseElement)
    ['{0325E2B1-72CA-46A9-87F5-385E6FBC7AD7}']
  end;

  // Responsável pela carga de objetos do banco de dados.
  IEntityLoader = interface(IBaseElement)
    ['{DF928002-27B9-4854-A983-D6558BB9A7B0}']
    function Load(const pOID: IInfraType; const pOptionalInstance: IInfraType;
      const pSession: ISession): IInfraType;
  end;

  { O tradutor de criteria }
  ICriteriaQuery = interface(IInterface)
    ['{F6B4B112-BF49-4D84-B6A6-514C948E2AFE}']
  end;

  { ??? }
  ICriterion = interface(IInterface)
    ['{2E91B954-226E-43B9-B2F0-011204997D50}']
    function ToSql(pCriteria: ICriteria;
      pCriteriaQuery: ICriteriaQuery): String;
  end;

  ICriterionList = interface(IBaseElement)
    ['{541A4F62-F13B-4DFA-9456-2CBA7E4563F7}']
    function Add(const Item: ICriterion): Integer;
    function First: ICriterion;
    function GetCount: Integer;
    function GetItem(Index: Integer): ICriterion;
    function IndexOf(const Item: ICriterion): Integer;
    function Last: ICriterion;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const Item: ICriterion);
    procedure SetItem(Index: Integer; const TypeInfo: ICriterion);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: ICriterion read GetItem
      write SetItem; default;
  end;

  { ??? }
  ICriteria = interface
    ['{8ACFFA0B-A163-47C1-8FF1-74DDF44EEF79}']
    function GetClassInfo: IClassInfo;
    function Add(const Criterion: ICriterion): ICriteria;
    function AddOr(const Criterion: ICriterion): ICriteria;
    function List: IInfraList;
    function UniqueResult: IInfraType;
    property ClassInfo: IClassInfo read GetClassInfo;
  end;

  { ??? }
  IPropertyCriterion = interface(ICriterion)
    ['{71642856-FDF1-47A9-8517-48A293383F76}']
    function GetPropertyName: String;
    procedure SetPropertyName(const Value: String);
    property PropertyName: String read GetPropertyName write SetPropertyName;
  end;

  { ??? }
  ISimpleExpression = interface(IPropertyCriterion)
    ['{2F7F3942-F667-41A9-8198-21176AEDFBE8}']
    function GetValue: IInfraType;
    procedure SetValue(const Value: IInfraType);
    function GetCompareOperator: String;
    property CompareOperator: String read GetCompareOperator;
    property Value: IInfraType read GetValue write SetValue;
  end;

  { ??? }
  IBetweenCriterion = interface(IPropertyCriterion)
    ['{E6116943-75CD-4D18-8315-99E959162B38}']
    function GetFromValue: IInfraType;
    procedure SetFromValue(const Value: IInfraType);
    function GetToValue: IInfraType;
    procedure SetToValue(const Value: IInfraType);
    property FromValue: IInfraType read GetFromValue write SetFromValue;
    property ToValue: IInfraType read GetToValue write SetToValue;
  end;

  { ??? }
  IInCriterion = interface(IPropertyCriterion)
    ['{3C29D79D-B5A1-434D-88A7-AF4BED879E81}']
    function GetCount: Integer;
    function GetItem(Index: integer): IInfraType;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IInfraType read GetItem;
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

  TStringHelper = class
    class function StartsWith(const Value, pFragment: string): boolean;
    class function Substring(const Value: string; pCount: integer): string;
    class function Join(const pSeparator: string; pList: TStrings): string;
  end;

function PersistenceService: IPersistenceService;

implementation

uses SysUtils, InfraHibernateRegister;

function PersistenceService: IPersistenceService;
begin
  Result := ApplicationContext as IPersistenceService;
end;

{ TStringHelper }

class function TStringHelper.Join(const pSeparator: string;
  pList: TStrings): string;
var
  i: integer;
begin
  if pList.Count <> 0 then
  begin
    Result := pList[0];
    for i := 1 to pList.Count-1 do
      Result := Result + pSeparator + pList[i];
  end else
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
  InfraHibernateRegister.RegisterOnReflection;

end.


