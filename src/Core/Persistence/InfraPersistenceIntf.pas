unit InfraPersistenceIntf;

interface

uses
  {TStrings}Classes,
  {Zeos}ZDbcIntfs,
  {Infra}InfraCommonIntf, InfraValueTypeIntf;

type
  ISession = interface;

  {
    * Guarda as configurações sobre a persistencia:
      - Informações sobre a conexão;
      - De onde carregar os templates;
      - Classe Reader para leitura de templates
      - etc;
  }
  IConfiguration = interface(IBaseElement)
    ['{16AF1EFF-FB48-4BAD-BDC7-E0518E83E09E}']
    function GetProperties: TStrings;
    function GetPropertyItem(const pName: string): string;
    procedure SetPropertyItem(const pName: string; const Value: string);
    property Properties: TStrings read GetProperties;
    property PropertyItem[const pName: string]: string read GetPropertyItem
    write SetPropertyItem;
  end;

  {
    * Classe responsável por criar e oferecer uma conexão zeos para o
      PersistenceEngine.

    * É possivel setar uma conexão externa através do SetConnection e neste caso
      o connection provider passa a usar esta conexão.
  }
  IConnectionProvider = interface(IBaseElement)
    ['{E4D7AF34-1750-461D-90E3-15F0DFD3167E}']
    function GetConnection: IZConnection;
    procedure Close;
    procedure CloseConnection(const pConnection: IZConnection);
    procedure SetConnection(const pConnection: IZConnection);
  end;

  {
    * O servico deve oferecer um configuration para o programador preencher.

    * Cada OpenSession retorna um novo objeto Session.

    * Um único PersistenceEngine é criado no service e deve ser passado para
      o Session durante a criação do mesmo.

    * Quando o programador chama SetConnection este é repassado para o
      ConnectionProvider do PersistenceEngine;

    * O Session é contextualizado.
  }
  IPersistenceService = interface(IInterface)
    ['{0DC6F960-B66E-437E-88BD-BD0BAF6CFFE3}']
    function GetConfiguration: IConfiguration;
    function OpenSession: ISession; overload;
    procedure SetConnection(const pConnection: IZConnection);
    property Configuration: IConfiguration read GetConfiguration;
  end;

  {
    * O Load gera um SQLCommand e devolve ao programador para que ele possa
      preencher os parametros (se necessário) e chamar o GetResult para obter o
      objeto.

    * O LoadList é similar ao Load só que retorna a lista de objetos quando
      chamando o GetResult do SQLCommand retornado.

    * O Load tem várias sobrecargas:
      * Load(CommandName) - Retorna um SQLCommand sem definir parâmetros, ;
      * Load(CommandName, Objeto) - Retorna um SQLCommand e armazena o objeto
        passado para que o PersistenceEngine possa extrair o valor dos
        parâmetros. SQLCommand.ClassType pode ser extraido do Objeto.
      * Load(CommandName, ClassType) - Retorna um SQLCommand já definindo
        qual o tipo de classe que o PersistenceEngine deve instanciar.

    * O LoadList tem várias sobrecargas:
      * LoadList(CommandName) - Retorna um SQLCommand sem definir parâmetros ou
        tipo de objeto e lista a ser instanciada. O programador vai
        provavelmente definir isso no retorno.
      * LoadList(CommandName, ClassType, ListType) - Retorna um SQLCommand sem
        definir parâmetros mas já definindo o tipo de objeto e lista a ser
        instanciada.
      * LoadList(CommandName, Objeto, ListType) - Retorna um SQLCommand
        e armazena o objeto passado para que o PersistenceEngine possa extrair
        o valor dos parâmetros. ClassType ser extraido do Objeto.
      * LoadList(CommandName, Objeto, List) - Retorna um SQLCommand
        e armazena o objeto passado para que o PersistenceEngine possa extrair
        o valor dos parâmetros. Neste caso o Session usa a lista passada para
        fazer o preenchimento dos objetos.

    * O Save cria um SQLCommand, armazena em uma lista interna de commands
      no session e retorna o mesmo para que o usuário possa preencher os
      parâmetros.

    * O processo do Delete é similar ao do Save.

    * O Session armazena os SQLCommand's de Save e Delete na lista até que
      o programador chame o método o Flush.

    * O Flush chama o PersistenceEngine para cada SQLCommand da lista interna

    * O Flush deve limpar a lista de SQLCommands caso todas as gravações
       tenham tido sucesso.
  }
  ISQLCommand = interface;

  ISession = interface(IBaseElement)
    ['{693A7815-9A5E-46C7-97DD-04D3E9C245AF}']
    function GetPersistenceService: IPersistenceService;
    procedure SetPersistenceService(const Value: IPersistenceService);
    function Load(const pCommandName: string; const pObj: IInfraObject = nil): ISQLCommand; overload;
    function Load(const pCommandName: string; const pClassType: TGUID): ISQLCommand; overload;
    function LoadList(const pCommandName: string): ISQLCommand; overload;
    function LoadList(const pCommandName: string; const pClassType: TGUID): ISQLCommand; overload;
    function LoadList(const pCommandName: string; const pClassType: TGUID; const pListType: TGUID): ISQLCommand; overload;
    function LoadList(const pCommandName: string; const pObj: IInfraObject; const pListType: TGUID): ISQLCommand; overload;
    function LoadList(const pCommandName: string; const pObj: IInfraObject; const pList: IInfraList = nil): ISQLCommand; overload;
    procedure Delete(const pCommandName: string; const pObj: IInfraObject);
    procedure Save(const pCommandName: string; const pObj: IInfraObject);
    function Flush: Integer;
    property PersistenceService: IPersistenceService read GetPersistenceService write SetPersistenceService;
  end;

  {
    * O PersistenceEngine tem de ser passado no construtor do SQLCommand para
      quando o programador chamar o GetResult.

    * GetResult é o método que causará a execução do command e sempre retornará
      um InfraList ou a lista definida no atributo TypeInfoList.

    * Os parametros de um SQLCommand são definidos antes do template ser
      carregado, afinal o template só será carregado no GetResult.

    * O SQLCommand precisa do metada do objeto e da lista ara que o
      PersistenceEngine possa criar os objetos corretamente e preencher
      a lista que é devolvida.
  }
  ISQLCommand = interface(IBaseElement)
    ['{8F2E7318-09C1-4EA2-BA6E-6724275E9043}']
    function GetName: String;
    function GetResult: IInfraType;
    function GetListType: TGUID;
    function GetClassType:TGUID;
    procedure SetListType(const Value: TGUID);
    procedure SetClassType(const Value: TGUID);
    procedure SetName(const Value: String);
    procedure SetParam(const pParamName: string; const value: IInfraType); overload;
    procedure ClearParams;
    property Name: string read GetName write SetName;
    property ClassType: TGUID read GetClassType write SetClassType;
    property ListType: TGUID read GetListType write SetListType;
  end;

  {
    * O service passa o configuration como parametro para o construtor desta
      classe para que ele possa repassar para o Reader de tamplates.

    * O Load vai carregar o template com base no SQLCommand, executar a SQL
      passando os parametros e preencher a List com objetos do tipo definido
      no SQLCommand.

    * O Execute é similar ao Load mas nao preenche lista alguma e deve
      retornar o numero de linhas afetadas pelo comando.
  }
  IPersistenceEngine = interface(IBaseElement)
    ['{F1C7686A-43B6-4FE7-8BF1-6A9C6BC54AE4}']
    procedure Load(const pSqlCommand: ISqlCommand; const List: IInfraList);
    function Execute(const pSqlCommand: ISqlCommand): IInfraInteger;
  end;

  {
    * Recebe o configuration na criação para poder recuperar
      corretamente os templates sql.
  }
  ITemplateReader = interface
    function Read(const pTemplateName: string): string;
  end;

implementation

end.



  {
    function GetConnectionProvider: IConnectionProvider;
     ***

     1) Ver como seria estabelecido filtros nos loads. uma ideia é ter um parametro no template com o caracter #
        que poderia ser substituido atraves da propriedade Template.SetParam do Session. ex:
        
        MySession.Template.SetParam('ClausulaWhere', 'Cidade.ds_uf = '+QuotedStr('Ba'));
        No exemplo acima Cidade é o alias definido para a tabela no template.
        
        Seguindo esta abordagem seria interessante depois ver uma forma de interpretar a stringo valor passado no SetParam
        para que o programador implemente visando apenas atributos das classes em vez de nomes de campos. ex:

        MySession.Template.SetParam('ClausulaWhere', 'Cidade.Estado = '+QuotedStr('Ba'));
        
     2) O Save e o Delete do Session deveria gerar uma exceção ou deveria retornar a quantidade de registros afetados?
     
     3) Nesta abordagem, precisamos de OpenSession para cada operação que vamos fazer, por que cada session tem seu proprio 
        template.

    s := ps.Opensession;

    p := pessoa.create;
    p.oid.AsInteger := 25;
    s.Load('SelectPessoaByOid', p);
    s
    ou 

    s.Load('SelectPessoa1',Reflect.GetType(IPessoa));
   
   
    p := pessoa.create;
    p.oid.AsInteger := 25;
    
    s.sqlcommands.param(gaga);
    s.Load('SelectPessoaByid', p);
    
   
   IParam;
     Name: InfraString;
     Value: Infratype;
   IParams = lista IParam
   
   
   
   sc := Load >>>> ISQLCommand
   sc.SetParams(InfraObject);
   sc.GetResult <<<< InfraObject

   sc := Load >>>> ISQLCommand
   sc.SetParams(InfraObject);
   sc.Param.Add(NameString, InfraType);
   sc.GetResult <<<< InfraObject

   sc := Load >>>> ISQLCommand
   sc.Param.Add(NameString, InfraType);
   sc.GetResult <<<< InfraObject
   
   
   p := Load('pessoaporoid', obj).GetResult as IPessoa;
   
   p := Load(obj).GetResult as IPessoa;
   
   close;
   sql.text := '.....';
   sql.params[0] :=....
   sql.params[0] :=....
   sql.params[0] :=....
   sql.params[0] :=....
   sql.open
  
   sc := load.param.add().getsult  
   
   s.Load(TemplateName, InfraObject).GetResult as IPessoa;



    session.load(....)
    begin
      result := criar o sqlcommand
      usa o leitor para carregar o sql no result
      preencher outros atributos do result com base nos parametros do load
    end;

    sqlcommand.getresult
    begin
      Result := criar a lista do tipo que está em TypeInfoList ou TInfraList
      pe.Load(Self, Result);
    end;

    s.load(...).GetResult as IPessoa;
    s.loadlist(...).GetResult as IPessoas;
    
    sessio
    É responsabilidade do session retornar o objeto ou a lista dependendo do que foi solicitado pelo programador.
  }

