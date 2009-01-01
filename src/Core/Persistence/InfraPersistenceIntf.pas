unit InfraPersistenceIntf;

interface

uses
  {TStrings}Classes,
  {Zeos}ZDbcIntfs,
  {Infra}InfraCommonIntf, InfraValueTypeIntf;

type
  EInfraPersistenceError = Class(EInfraError);
  EInfraConnectionProviderError = class(EInfraPersistenceError);
  EInfraTemplateNotFound = Class(EInfraPersistenceError);

  ISession = interface;                   

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
  end;

  IConnectionProvider = interface(IBaseElement)
    ['{E4D7AF34-1750-461D-90E3-15F0DFD3167E}']
    function GetConnection: IZConnection;
    procedure ReleaseConnection(const pConnection: IZConnection);
    procedure Close;
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
    function GetResult: IInfraType;
    function GetListID: TGUID;
    function GetClassID:TGUID;
    procedure SetListID(const Value: TGUID);
    procedure SetClassID(const Value: TGUID);
    procedure SetName(const Value: String);
    procedure SetParam(const pParamName: string; const value: IInfraType); overload;
    procedure SetParam(const pObj: IInfraType); overload;
    procedure ClearParams;
    property Name: string read GetName write SetName;
    property ClassID: TGUID read GetClassID write SetClassID;
    property ListID: TGUID read GetListID write SetListID;
  end;

  ISQLCommandParams = interface
    ['{3882EC5D-59EC-4839-93F8-B4DCDE3B6B37}']
    function GetItem(Index: String): IInfraType;
    procedure SetItem(Index: String; Value: IInfraType);
    function GetCount: Integer;
    function Add(Index: String; Value: IInfraType): String;
    procedure Delete(Index: String);
    procedure DeletePosition(Index: integer);
    function NewIterator: IInterface;
    function PositionOf(Index: String; Value: IInfraType): integer;
    function ValueOfPosition(Index: Integer): IInfraType;
    function IndexOfPosition(Index: Integer): String;
    property Count: Integer read GetCount;
    property Items[Index: String]: IInfraType read GetItem write SetItem; default;
  end;

  ISession = interface(IBaseElement)
    ['{693A7815-9A5E-46C7-97DD-04D3E9C245AF}']
    function Load(const pCommandName: string; const pObj: IInfraObject = nil): ISQLCommand; overload;
    function Load(const pCommandName: string; const pClassID: TGUID): ISQLCommand; overload;
    function LoadList(const pCommandName: string): ISQLCommand; overload;
    function LoadList(const pCommandName: string; const pClassID: TGUID): ISQLCommand; overload;
    function LoadList(const pCommandName: string; const pClassID: TGUID; const pListID: TGUID): ISQLCommand; overload;
    function LoadList(const pCommandName: string; const pObj: IInfraObject; const pListID: TGUID): ISQLCommand; overload;
    function LoadList(const pCommandName: string; const pObj: IInfraObject; const pList: IInfraList = nil): ISQLCommand; overload;
    procedure Delete(const pCommandName: string; const pObj: IInfraObject);
    procedure Save(const pCommandName: string; const pObj: IInfraObject);
    function Flush: Integer;
  end;

  IPersistenceEngine = interface(IBaseElement)
    ['{F1C7686A-43B6-4FE7-8BF1-6A9C6BC54AE4}']
    procedure SetConnection(const pConnection: IZConnection);
    procedure Load(const pSqlCommand: ISqlCommand; const List: IInfraList);
    function Execute(const pSqlCommand: ISqlCommand): IInfraInteger;
  end;

  ITemplateReader = interface
    ['{AFD2D321-E26B-4E48-93FB-48FD24BCE62B}']
    function Read(const pTemplateName: string): string;
  end;

function PersistenceService: IInfraPersistenceService;

implementation

function PersistenceService: IInfraPersistenceService;
begin
  Result := ApplicationContext as IInfraPersistenceService;
end;

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

end.

