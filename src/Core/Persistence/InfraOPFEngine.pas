unit InfraOPFEngine;

interface

uses
  SysUtils,
  Classes,
  InfraCommon,
  InfraCommonIntf,
  InfraOPFIntf,
  InfraValueTypeIntf,
  ZDbcIntfs;

type
  /// Descrição da classe
  TPersistenceEngine = class(TBaseElement, IPersistenceEngine)
  private
    FConfiguration: IConfiguration;
    FConnnectionProvider: IConnectionProvider;
    /// Parser que procura por parametros e macros na instrução SQL
    FParser: ISQLParamsParser;
    function GetReader: ITemplateReader;
    procedure SetParameters(const pStatement: IZPreparedStatement;
      const pParams: ISqlCommandParams);
    function GetRowFromResultSet(
      const pSqlCommand: ISQLCommandQuery;
      const pResultSet: IZResultSet): IInfraObject;
    procedure DoLoad(const pStatement: IZPreparedStatement; const pSqlCommand:
        ISQLCommandQuery; const pList: IInfraList);
    function GetConnectionProvider: IConnectionProvider;
    function GetConfiguration: IConfiguration;
    function ReadTemplate(const pSqlCommandName: string): string;
  protected
    procedure SetConnection(const pConnection: IZConnection);
    procedure Load(const pSqlCommand: ISQLCommandQuery;
      const pList: IInfraList);
    function Execute(const pConnection: IZConnection;
      const pSqlCommand: ISqlCommand): Integer;
  public
    constructor Create(pConfiguration: IConfiguration;
      pConnectionProvider: IConnectionProvider); reintroduce;
    property ConnectionProvider: IConnectionProvider read GetConnectionProvider;
    property Configuration: IConfiguration read GetConfiguration;
  end;

implementation

uses
  InfraOPFParsers,
  InfraOPFConsts,
  InfraConsts;

{ TPersistenceEngine }

{*
  Cria uma nova instância de TPersistenceEngine
  @param pConfiguration   ParameterDescription
}
constructor TPersistenceEngine.Create(pConfiguration: IConfiguration;
  pConnectionProvider: IConnectionProvider);
begin
  inherited Create;
  // O argumento pConfiguration sempre é requerido, visto que é dele que
  // o Framework obterá as suas configurações
  if not Assigned(pConfiguration) then
    raise EInfraArgumentError.Create('pConfiguration');

  FConfiguration := pConfiguration;
  FConnnectionProvider := pConnectionProvider;
  FParser := TSQLParamsParser.Create;
end;

{*

  @return ResultDescription
}
function TPersistenceEngine.GetReader: ITemplateReader;
var
  vReaderClassName: string;
  vReaderTypeInfo: IClassInfo;
begin
  vReaderClassName := FConfiguration.GetValue(cCONFIGKEY_TEMPLATETYPE, EmptyStr);
  if vReaderClassName = EmptyStr then
    raise EPersistenceTemplateError.Create(cErrorTemplateTypeInvalid);
  vReaderTypeInfo := TypeService.GetType(vReaderClassName, True);
  Result := TypeService.CreateInstance(vReaderTypeInfo) as ITemplateReader;
  Result.Configuration := FConfiguration;
end;

{*
  Executa uma instrução SQL contra o banco baseado nas informações contidas no
  parâmetro passado.
  @param pSqlCommand  Objeto com as informações sobre o que e como executar a instrução.
  @return Retornar a quantidade de registros afetados pela atualização.
}
function TPersistenceEngine.Execute(const pConnection: IZConnection;
  const pSqlCommand: ISqlCommand): Integer;
var
  vSQL: string;
  vStatement: IZPreparedStatement;
begin
  // Carrega a SQL e extrai os parâmetros
  if not Assigned(pSqlCommand) then
    raise EInfraArgumentError.Create('pSqlCommand');

  vSQL := ReadTemplate(pSqlCommand.Name);
  vSQL := FParser.Parse(vSQL);

  // *** 1) Acho que os parâmetros macros de FParse devem ser substituidos aqui
  //   antes de chamar o PrepareStatementWithParams

  // Solicita um connection e prepara a SQL
  vStatement := pConnection.PrepareStatementWithParams(vSQL, FParser.GetParams);
  // Seta os parametros e executa
  SetParameters(vStatement, pSqlCommand.Params);
  Result := vStatement.ExecuteUpdatePrepared;
end;

{**

  @param pStatement   ParameterDescription
  @param pSqlCommand   ParameterDescription
  @param pList   ParameterDescription
}
procedure TPersistenceEngine.DoLoad(const pStatement: IZPreparedStatement;
    const pSqlCommand: ISQLCommandQuery; const pList: IInfraList);
var
  vResultSet: IZResultSet;
  vObject: IInfraObject;
begin
  vResultSet := pStatement.ExecuteQueryPrepared;
  try
    while vResultSet.Next do
    begin
      vObject := GetRowFromResultSet(pSqlCommand, vResultSet);
      pList.Add(vObject);
    end;
  finally
    vResultSet.Close;
  end;
end;

{ carregar a sql usando um reader com base no Name do pSqlCommand
  preencher os params da sql com base nos Params do pSqlCommand
  executa a sql e pega um IZStatement
  Faz um laço para pegar cada registro
  cria um objeto com base no ClassType do pSqlCommand,
  Seta o estado persistent e clean ao objeto criado
  faz a carga dos atributos com base no registro
  Adiciona o novo objeto em pList retorna a lista
}
{**
  Carrega uma lista de objetos do banco de dados usando um SQLCommandQuery

  @param pSqlCommand SqlCommandQuery que será usado para efetuar a consulta no banco de dados
  @param pList Lista que será preenchida com os objetos lidos
}
procedure TPersistenceEngine.Load(const pSqlCommand: ISQLCommandQuery;
  const pList: IInfraList);
var
  vSQL: string;
  vStatement: IZPreparedStatement;
  vConnection: IZConnection;
begin
  if not Assigned(pSqlCommand) then
    raise EInfraArgumentError.Create('pSqlCommand');

  if not Assigned(pList) then
    raise EInfraArgumentError.Create('pList');

  // Acho q o Sql deveria já estar no SqlCommand neste momento
  vSQL := ReadTemplate(pSqlCommand.Name);
  // *** 1) se a SQL está vazia aqui deveria gerar exceção ou deveria ser dentro
  // do vReader.Read????
  vSQL := FParser.Parse(vSQL);

  // *** 2) Acho que os parâmetros macros de FParse devem ser substituidos aqui
  // antes de chamar o PrepareStatementWithParams
  vConnection := FConnnectionProvider.GetConnection;
  vStatement := vConnection.PrepareStatementWithParams(vSQL, FParser.GetParams);
  try
    SetParameters(vStatement, pSqlCommand.Params);
    DoLoad(vStatement, pSqlCommand, pList);
  finally
    vStatement.Close;
  end;
end;

{**

  @return Retorna uma referencia ao ConnectionProvider
}
function TPersistenceEngine.GetConnectionProvider: IConnectionProvider;
begin
  Result := FConnnectionProvider;
end;

{**

  @param pConnection   ParameterDescription
  @return ResultDescription
}
procedure TPersistenceEngine.SetConnection(const pConnection: IZConnection);
begin
  if not Assigned(pConnection) then
    raise EInfraArgumentError.Create('pConnection');

  // TODO: preencher o connection provider com o pConnection
end;

{**
  Chame GetConfiguration para obter uma referencia ao objeto de configuração do Framework
  @return Retorna uma referencia ao objeto de configuração do Framework
}
function TPersistenceEngine.GetConfiguration: IConfiguration;
begin
  Result := FConfiguration;
end;

{**

  @param pSqlCommand   ParameterDescription
  @param pResultSet   ParameterDescription
  @return ResultDescription
}
function TPersistenceEngine.GetRowFromResultSet(const pSqlCommand: ISQLCommandQuery;
  const pResultSet: IZResultSet): IInfraObject;
var
  vIndex: integer;
  vAttribute: IInfraType;
  vZeosType: IZTypeAnnotation;
  vAliasName: string;
begin
  // *** Será que isso deveria estar aqui?????
  if IsEqualGUID(pSqlCommand.GetClassID, InfraConsts.NullGUID) then
    Raise EPersistenceEngineError.Create(
      cErrorPersistenceEngineObjectIDUndefined);
  Result := TypeService.CreateInstance(pSqlCommand.GetClassID) as IInfraObject;
  if Assigned(Result) then
  begin
    // A lista de colunas do ResultSet.GetMetadata do Zeos começa do 1.
    for vIndex := 1 to pResultSet.GetMetadata.GetColumnCount do
    begin
      vAliasName := pResultSet.GetMetadata.GetColumnLabel(vIndex);
      vAttribute := Result.TypeInfo.GetProperty(Result, vAliasName) as IInfraType;
      if not Assigned(vAttribute) then
        Raise EPersistenceEngineError.CreateFmt(
          cErrorPersistenceEngineAttributeNotFound,
          [vAliasName, pResultSet.GetMetadata.GetColumnName(vIndex)]);
      if Supports(vAttribute.TypeInfo, IZTypeAnnotation, vZeosType) then
        vZeosType.NullSafeGet(pResultSet, vIndex, vAttribute)
      else
        raise EPersistenceEngineError.CreateFmt(
          cErrorPersistenceEngineCannotMapAttribute, [vAttribute.TypeInfo.Name]);
    end;
  end;
end;

function TPersistenceEngine.ReadTemplate(const pSqlCommandName: string): string;
var
  vReader: ITemplateReader;
begin
  vReader := GetReader;
  Result := vReader.Read(pSqlCommandName);
end;

{**
  Efetua a substituição dos parametros por seus respectivos valores
  @param pStatement Este parametro representa o comando SQL no qual se efetuará
                    a substuição de parâmetros
  @param pParams Lista de parametros do tipo ISqlCommandParams
}
procedure TPersistenceEngine.SetParameters(const pStatement: IZPreparedStatement;
  const pParams: ISqlCommandParams);
var
  vIndex: integer;
  vParamValue: IInfraType;
  vParams: TStrings;
  vZeosType: IZTypeAnnotation;
begin
  // *** o que acontece caso tenhamos um template com nomes de parametros repetidos?
  vParams := pStatement.GetParameters;
  for vIndex := 0 to vParams.Count-1 do
  begin
    vParamValue := pParams[vParams[vIndex]];
    if Assigned(vParamValue)
      and Supports(vParamValue.TypeInfo, IZTypeAnnotation, vZeosType) then
      // Aumenta o vIndex por que no Zeos as colunas começam de 1
      vZeosType.NullSafeSet(pStatement, vIndex+1, vParamValue)
    else
      raise EPersistenceEngineError.CreateFmt(
        cErrorPersistenceEngineParamNotFound, [vParams[vIndex]]);
  end;
end;

end.
