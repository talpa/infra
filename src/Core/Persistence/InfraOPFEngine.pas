unit InfraOPFEngine;

interface

uses
  {Infra}
  InfraCommon,
  InfraOPFIntf,
  InfraValueTypeIntf,
  {Zeos}
  ZDbcIntfs;

type
  /// Descrição da classe
  TPersistenceEngine = class(TBaseElement, IPersistenceEngine,ITransaction)
  private
    FConnectionProvider: IConnectionProvider;
    /// Configuração definida no session factory (imutável)
    FConfiguration: IConfiguration;
    /// Parser que procura por parametros e macros na instrução SQL
    FParser: ISQLParamsParser;
    function GetReader: ITemplateReader;
    procedure SetParameters(const pStatement: IZPreparedStatement; const pParams: ISqlCommandParams);
    function GetRowFromResultSet(const pSqlCommand: ISQLCommandQuery; const pResultSet: IZResultSet): IInfraObject;
    procedure DoLoad(const pStatement: IZPreparedStatement; const pSqlCommand: ISQLCommandQuery; const pList: IInfraList);
    function ReadTemplate(const pSqlCommandName: string): string;
    function InternallExecute(pSqlCommand: ISqlCommand;
      pConnection: IZConnection): Integer;
    procedure CheckInTransaction;
    class function ConvertToZTransIsolationLevel(
      pTransIsolationLevel: TInfraTransIsolatLevel): TZTransactIsolationLevel;
    function GetInTransaction: Boolean;
  protected
    procedure Load(const pSqlCommand: ISQLCommandQuery; const pList: IInfraList);
    function Execute(const pSqlCommand: ISqlCommand): Integer;
    function ExecuteAll(const pSqlCommands: ISQLCommandList): Integer;
    procedure BeginTransaction(pTransactIsolationLevel: TInfraTransIsolatLevel = tilReadCommitted);
    procedure Commit;
    procedure Rollback;
  public
    constructor Create(pConfiguration: IConfiguration); reintroduce;
  end;

implementation

uses
  SysUtils,
  Classes,
  InfraCommonIntf,
  InfraOPFParsers,
  InfraOPFConsts,
  InfraOPFConnectionProvider,
  TypInfo;

{ TPersistenceEngine }

{**
  Cria uma nova instância de TPersistenceEngine
  @param pConfiguration   ParameterDescription
}
constructor TPersistenceEngine.Create(pConfiguration: IConfiguration);
begin
  inherited Create;
  // O argumento pConfiguration sempre é requerido, visto que é dele que
  // o Framework obterá as suas configurações
  if not Assigned(pConfiguration) then
    raise EInfraArgumentError.CreateFmt(cErrorPersistenceWithoutConfig,
      ['PersistenceEngine.Create']);
  FConfiguration := pConfiguration;
  FConnectionProvider := TConnectionProvider.Create(FConfiguration);
  FParser := TSQLParamsParser.Create;
end;

{**
  Obtem o leitor de template definido no configuration
  @return Retorna um leitor de templates
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

{**
  Executa uma instrução SQL (Insert/Update/Delete) numa dada uma conexao.
  @param pSqlCommand Objeto com as informações sobre o que e como executar a instrução.
  @param pConnection Conexão na qual os comandos serão executados
  @return Retorna a quantidade de registros afetados pela atualização.
}
function TPersistenceEngine.InternallExecute(pSqlCommand: ISqlCommand;
  pConnection: IZConnection): Integer;
var
  vSQL: string;
  vStatement: IZPreparedStatement;
begin
  // Carrega a SQL e extrai os parâmetros
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
  Executa uma instrução SQL (Insert/Update/Delete)
  Executa contra o banco baseado nas informações contidas no parâmetro SqlCommand.
  @param pSqlCommand Objeto com as informações sobre o que e como executar a instrução.
  @return Retorna a quantidade de registros afetados pela atualização.
}
function TPersistenceEngine.Execute(const pSqlCommand: ISqlCommand): Integer;
var
  vConnection: IZConnection;
begin
  if not Assigned(pSqlCommand) then
    raise EInfraArgumentError.CreateFmt(cErrorPersistEngineWithoutSQLCommand,
      ['TPersistenceEngine.Execute']);

  vConnection := FConnectionProvider.GetConnection;
  try
    Result := InternallExecute(pSqlCommand, vConnection);
  finally
    vConnection := nil;
    FConnectionProvider.ReleaseConnection;
  end;
end;

{**
  Executa todas as instruções SQL (Insert/Update/Delete) contidas na lista
  Executa contra o banco baseado nas informações contidas na lista de SqlCommands.
  @param pSqlCommands Lista com as informações sobre as instruçòes e como executá-las
  @return Retorna a quantidade de registros afetados pela atualização.
}
function TPersistenceEngine.ExecuteAll(
  const pSqlCommands: ISQLCommandList): Integer;
var
  vI: integer;
  vConnection: IZConnection;
begin
  if not Assigned(pSqlCommands) then
    raise EInfraArgumentError.Create(cErrorPersistEngineWithoutSQLCommands);
  Result := 0;
  vConnection := FConnectionProvider.GetConnection;
  try
    for vI := 0 to pSqlCommands.Count - 1 do
      Result := Result + InternallExecute(pSqlCommands[vI], vConnection);
  finally
    vConnection := nil;
    FConnectionProvider.ReleaseConnection;
  end;
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
    raise EInfraArgumentError.CreateFmt(cErrorPersistEngineWithoutSQLCommand,
      ['TPersistenceEngine.Load']);
  if not Assigned(pList) then
    raise EInfraArgumentError.Create(cErrorPersistEngineWithoutList);
  // Acho q o Sql deveria já estar no SqlCommand neste momento
  vSQL := ReadTemplate(pSqlCommand.Name);
  // *** 1) se a SQL está vazia aqui deveria gerar exceção ou deveria ser dentro
  // do vReader.Read????
  vSQL := FParser.Parse(vSQL);
  // *** 2) Acho que os parâmetros macros de FParse devem ser substituidos aqui
  // antes de chamar o PrepareStatementWithParams
  vConnection := FConnectionProvider.GetConnection;
  try
    vStatement := vConnection.PrepareStatementWithParams(vSQL, FParser.GetParams);
    SetParameters(vStatement, pSqlCommand.Params);
    DoLoad(vStatement, pSqlCommand, pList);
  finally
    vStatement.Close;
    vConnection := nil;
    FConnectionProvider.ReleaseConnection;
  end;
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
  //  if IsEqualGUID(pSqlCommand.GetClassID, InfraConsts.NullGUID) then
  //    Raise EPersistenceEngineError.Create(
  //      cErrorPersistEngineObjectIDUndefined);
  Result := TypeService.CreateInstance(pSqlCommand.GetClassID) as IInfraObject;
  if Assigned(Result) then
  begin
    // A lista de colunas do ResultSet.GetMetadata do Zeos começa do 1.
    for vIndex := 1 to pResultSet.GetMetadata.GetColumnCount do
    begin
      vAliasName := pResultSet.GetMetadata.GetColumnLabel(vIndex);
      vAttribute := Result.TypeInfo.GetProperty(Result, vAliasName) as IInfraType;
      if not Assigned(vAttribute) then
        raise EPersistenceEngineError.CreateFmt(
          cErrorPersistEngineAttributeNotFound,
          [vAliasName, pResultSet.GetMetadata.GetColumnName(vIndex)]);
      if Supports(vAttribute.TypeInfo, IZTypeAnnotation, vZeosType) then
        vZeosType.NullSafeGet(pResultSet, vIndex, vAttribute)
      else
        raise EPersistenceEngineError.CreateFmt(
          cErrorPersistEngineCannotMapAttribute, [vAttribute.TypeInfo.Name]);
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
procedure TPersistenceEngine.SetParameters(
  const pStatement: IZPreparedStatement; const pParams: ISqlCommandParams);
var
  vIndex: integer;
  vParamValue: IInfraType;
  vParams: TStrings;
  vZeosType: IZTypeAnnotation;
begin
  vParams := pStatement.GetParameters;
  for vIndex := 0 to vParams.Count - 1 do
  begin
    vParamValue := pParams[vParams[vIndex]];
    if Assigned(vParamValue)
      and Supports(vParamValue.TypeInfo, IZTypeAnnotation, vZeosType) then
      // Aumenta o vIndex por que no Zeos as colunas começam de 1
      vZeosType.NullSafeSet(pStatement, vIndex + 1, vParamValue)
    else
      raise EPersistenceEngineError.CreateFmt(
        cErrorPersistEngineParamNotFound, [vParams[vIndex]]);
  end;
end;

{**
  Chame GetInTransaction para verificar se há uma transação em andamento
  @return Retorna True se houver uma transação sendo executada
}
function TPersistenceEngine.GetInTransaction: Boolean;
begin
  Result := not FConnectionProvider.GetConnection.GetAutoCommit;
end;

{**
  Caso nenhuma transação esteja em aberto, levanta ma exceção
}
procedure TPersistenceEngine.CheckInTransaction;
begin
  if not GetInTransaction then
    raise EInfraTransactionError.Create(cErrorNotInTransaction);
end;

{**
  Converte TInfraTransIsolatLevel para TZTransactIsolationLevel.
  Efetua a conversão de maneira segura
  @param pTransIsolationLevel Nível de isolamento do tipo TInfraTransIsolatLevel
  @return Retorna o nível de isolamento da transação já convertido
}
class function TPersistenceEngine.ConvertToZTransIsolationLevel
  (pTransIsolationLevel: TInfraTransIsolatLevel): TZTransactIsolationLevel;
begin
  case pTransIsolationLevel of
    tilNone: Result := tiNone;
    tilReadUncommitted: Result := tiReadUnCommitted;
    tilReadCommitted: Result := tiReadCommitted;
    tilRepeatableRead: Result := tiRepeatableRead;
    tilSerializable: Result := tiSerializable;
  else
    raise EInfraTransactionError.CreateFmt(cErrorTranIsolLevelUnknown,
      [GetEnumName(TypeInfo(TInfraTransIsolatLevel), Ord(pTransIsolationLevel))]);
  end;
end;

{**
  Inicia uma nova transação com o nível de Isolamento especificado
  Se uma transação já estiver em andamento, resultará numa mensagem de erro
  @param pTransactIsolationLevel Nível de isolamento da transaçao
}
procedure TPersistenceEngine.BeginTransaction(pTransactIsolationLevel: TInfraTransIsolatLevel);
begin
  if GetInTransaction then
    raise EInfraTransactionError.Create(cErrorAlreadyInTransaction);

  with FConnectionProvider.GetConnection do
  begin
    SetTransactionIsolation(ConvertToZTransIsolationLevel(pTransactIsolationLevel));
    SetAutoCommit(pTransactIsolationLevel = tilNone);
  end;
end;

{**
  Efetiva a transação sendo executada
  Caso nenhuma transação esteja em aberto, resultará numa mensagem de erro
}
procedure TPersistenceEngine.Commit;
begin
  CheckInTransaction;
  FConnectionProvider.GetConnection.Commit;
end;

{**
  Desfaz a transação corrente
  fazendo com que todas as modificações realizadas pela transação sejam rejeitadas.
  Caso nenhuma transação esteja em aberto, resultará numa mensagem de erro
}
procedure TPersistenceEngine.Rollback;
begin
  CheckInTransaction;
  FConnectionProvider.GetConnection.Rollback;
end;

end.

