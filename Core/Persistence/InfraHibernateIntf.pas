unit InfraHibernateIntf;

interface

uses
  Classes, InfraCommonIntf, InfraValueTypeIntf;

type
  // Tipo de heranca
  TInheritanceType = (itSINGLE_TABLE, itJOINED, itTABLE_PER_CLASS);
  // Nivel de isolamento de uma transação
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
    Caso nao tenha havido determinada anotação o HibPersistentClass já
    retorna algum valor padrão }
  IHibPersistentClass = interface(IBaseElement)
    ['{45E8AD80-7543-4A99-A19F-71FFC53D61DF}']
    function GetEntityName: string;
    function GetOIDColumnName: string;
    // *** function GetEntityPersister: IEntityPersister;
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
  IConfiguration = interface(IElement)
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
  ISessionFactory = interface(IElement)
    ['{98AA1387-1C54-4E68-A3E3-1DDB8470E6CF}']
    function GetConnectionProvider: IConnectionProvider;
    // Cria e retorna um objeto dialeto com base no que foi definido nas 
    // propriedades do Configuration.
    function GetDialect: IDialect;
    // Abre uma nova sessão utilizando o connection obtido do ConnectionProvider.
    function OpenSession: ISession; overload;
    // Cria/Retorna um Session usando o Connection passado como parametro.
    function OpenSession(const pConnection: IConnection): ISession; overload;
    property ConnectionProvider: IConnectionProvider read GetConnectionProvider;
    property Dialect: IDialect read GetDialect;
  end;

  {
    Implementação concreta de uma Sessão. Uma Sessao é o ponto central de
    utilização do servico de persistência pelo programador. É a partir desta
    classe que o programador irá tratar os objetos (carga, gravação e exclusão).
  }
  ISession = interface(IElement)
    ['{244258B8-5A25-48D1-B4BC-804A76D5920D}']
    function GetConnection: IConnection;
    function GetSessionFactory: ISessionFactory;
    // Cria o objeto Loader apropriado e tenta carregar o objeto pelo OID.
    function Load(const pTypeID: TGUID; const pOID: IInfraType): IInfraType;
    procedure SetConnection(const Value: IConnection);
    property Connection: IConnection read GetConnection write SetConnection;
    property SessionFactory: ISessionFactory read GetSessionFactory;
  end;

  // Provedor de conexões para o servico de persistencia.
  IConnectionProvider = interface(IElement)
    ['{E4D7AF34-1750-461D-90E3-15F0DFD3167E}']
    procedure Close;
    procedure CloseConnection(const pConnection: IConnection);
    function GetConnection: IConnection;
  end;

  // Define uma trasação (ainda nao sendo utilizado)
  ITransaction = interface(IElement)
    ['{73827860-919C-4416-B43F-AD3727D122F5}']
  end;

  //  Define o dialeto do banco de dados (ainda nao sendo utilizado)
  IDialect = interface(IElement)
    ['{0325E2B1-72CA-46A9-87F5-385E6FBC7AD7}']
  end;

  // Responsável pela carga de objetos do banco de dados.
  ILoader = interface(IElement)
    ['{DF928002-27B9-4854-A983-D6558BB9A7B0}']
    // Cria um objeto e o preenche conforme mapeamento existente na tipo.
    function Load(const pTypeID: TGUID; const pSession: ISession;
      const pOID: IInfraType): IInfraType;
  end;

  {
    Abastração do mecanismo de conexão com banco.
    Responsável pela execução de instruções SQL gerada pelos Loaders
    ou Persisters.
  }
  IConnection = interface(IElement)
    ['{FA23555A-3724-474D-A318-D37CBC3390D3}']
    procedure Close;
    function ExecuteQuery(const pSQL: String): IResultSet;
    function ExecuteUpdate(const pSQL: String): Integer;
    function GetIsClosed: Boolean;
    property IsClosed: Boolean read GetIsClosed;
  end;

  {
    Abstração de um dataset. Utilizado durante a leitura de registros do
    mecanismo de armazenamento.
  }
  IResultSet = interface(IElement)
    ['{A4061B4D-420C-4954-B627-AD8CD699CA7A}']
    procedure Close;
    function EOF: Boolean;
    procedure First;
    function GetValue(const FieldName: String): IInfraType;
    procedure Open(const pSQL: string);
    procedure Next;
  end;

  // Conexão concreta para driver usando DBX
  IDBXConnection = interface(IConnection)
    ['{63DCA0BF-0224-4335-B905-2593E5C89460}']
  end;

  // Conexão concreta para driver usando ZEOS
  IZeosConnection = interface(IConnection)
    ['{1E8B196E-926A-41D8-A5AB-CC9760CAEBFD}']
  end;

  // ResultSet concreto para driver usando DBX
  IDBXResultSet = interface(IResultSet)
    ['{F2A3128A-F9C0-4B07-952D-003CAA5C7AB5}']
  end;

  // ResultSet concreto para driver usando ZEOS
  IZeosResultSet = interface(IResultSet)
    ['{33515232-FE60-4C8F-A65D-40B775159DC8}']
  end;

  IEntityPersister  = interface(IInterface)
    ['{C589237F-0BD5-4FCD-90E9-0423C9EA5ECD}']
    // metodo Load que vai delegar para o TLoader
    function Load(const pTypeID: TGUID; const pSession: ISession;
      const pOID: IInfraType): IInfraType;
  end;

  // Map de EntityPersisters
  IEntityPersisters = interface(IBaseElement)
    ['{D12A7D8D-8FD1-40D5-9AC0-21C8FE1B921C}']
    // *** acho que está faltando os métodos e propriedades
  end;

function PersistenceService: IPersistenceService;

implementation

function PersistenceService: IPersistenceService;
begin
  Result := ApplicationContext as IPersistenceService;
end;

end.

