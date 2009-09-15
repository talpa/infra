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
  TPersistenceEngine = class(TBaseElement, IPersistenceEngine, ITransaction)
  private
    FSQLGenerator: ITemplateReader_Build;
    FConnectionProvider: IConnectionProvider;
    /// Configuração definida no session factory (imutável)
    FConfiguration: IConfiguration;
    /// Parser que procura por parametros e macros na instrução SQL
    FParser: ISQLParamsParser;
    /// Item de Conexão atual
    FConnectionItem: IConnectionItem;
    function GetReader: ITemplateReader;overload;
    function GetReader(
      const pSqlCommand : ISQLCommand): ITemplateReader_Build; overload;
    procedure SetParameters(const pStatement: IZPreparedStatement;
      const pParams: ISqlCommandParams);
    function GetRowFromResultSet(const pSqlCommand: ISQLCommandQuery;
      const pResultSet: IZResultSet): IInfraObject;
    procedure DoLoad(const pStatement: IZPreparedStatement;
      const pSqlCommand: ISQLCommandQuery; const pList: IInfraList);
    function ReadTemplate(const pSqlCommand: ISQLCommand): string;
    function InternallExecute(const pSqlCommand: ISqlCommand;
      const pConnection: IZConnection): Integer;
    function GetSQLFromCache(const pSqlCommand: ISQLCommand): string;
    procedure AddSQLToCache(const pSqlCommand: ISQLCommand; pValue: string);
    procedure CheckInTransaction;
    function InTransaction: Boolean;
    function GetCurrentConnectionItem: IConnectionItem;
  protected
    { IPersistenceEngine }
    procedure Load(const pSqlCommand: ISQLCommandQuery;
      const pList: IInfraList);
    function Execute(const pSqlCommand: ISqlCommand): Integer;
    function ExecuteAll(const pSqlCommands: ISQLCommandList): Integer;
    { ITransaction }
    procedure BeginTransaction(
      pIsolationLevel: TIsolationLevel = tilReadCommitted);
    procedure Commit;
    procedure Rollback;
  public
    constructor Create(const pConfiguration: IConfiguration;
      const pConnectionProvider: IConnectionProvider); reintroduce;
  end;

implementation

uses
  SysUtils,
  Classes,
  InfraCommonIntf,
  InfraOPFParsers,
  InfraOPFConsts,
  List_SQLCache, InfraOPFTemplates;

{ TPersistenceEngine }

{**
  Cria uma nova instância de TPersistenceEngine
  @param pConfiguration   ParameterDescription
}

constructor TPersistenceEngine.Create(const pConfiguration: IConfiguration;
  const pConnectionProvider: IConnectionProvider);
begin
  inherited Create;
  FConfiguration := pConfiguration;
  FConnectionProvider := pConnectionProvider;
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

function TPersistenceEngine.GetCurrentConnectionItem: IConnectionItem;
begin
  if not Assigned(FConnectionItem) then
    FConnectionItem := FConnectionProvider.Acquire;
  Result := FConnectionItem;
end;

{**
  Executa uma instrução SQL (Insert/Update/Delete) numa dada uma conexao.
  @param pSqlCommand Objeto com as informações sobre o que e como executar a instrução.
  @param pConnection Conexão na qual os comandos serão executados
  @return Retorna a quantidade de registros afetados pela atualização.
}

function TPersistenceEngine.InternallExecute(const pSqlCommand: ISqlCommand;
  const pConnection: IZConnection): Integer;
var
  vSQL: string;
  vStatement: IZPreparedStatement;
begin
  // Carrega a SQL e extrai os parâmetros
  vSQL := ReadTemplate(pSqlCommand);
  vSQL := FParser.Parse(vSQL);
  // *** 1) Acho que os parâmetros macros de FParse devem ser substituidos aqui
  //   antes de chamar o PrepareStatementWithParams
  // Solicita um connection e prepara a SQL
  vStatement := pConnection.PrepareStatementWithParams(
    vSQL, FParser.GetParams);
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
begin
  if not Assigned(pSqlCommand) then
    raise EInfraArgumentError.CreateFmt(cErrorPersistEngineWithoutSQLCommand,
      ['TPersistenceEngine.Execute']);

  Result := InternallExecute(pSqlCommand, GetCurrentConnectionItem.Connection);
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
  vI: Integer;
  vConnection: IZConnection;
begin
  if not Assigned(pSqlCommands) then
    raise EInfraArgumentError.Create(cErrorPersistEngineWithoutSQLCommands);
  Result := 0;
  vConnection := GetCurrentConnectionItem.Connection;
  for vI := 0 to pSqlCommands.Count - 1 do
    Result := Result + InternallExecute(pSqlCommands[vI], vConnection);
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
    vResultSet := nil;
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
  vConnection: IConnectionItem;
begin
  if not Assigned(pSqlCommand) then
    raise EInfraArgumentError.CreateFmt(cErrorPersistEngineWithoutSQLCommand,
      ['TPersistenceEngine.Load']);
  if not Assigned(pList) then
    raise EInfraArgumentError.Create(cErrorPersistEngineWithoutList);
  // Acho q o Sql deveria já estar no SqlCommand neste momento
  vSQL := ReadTemplate(pSqlCommand);
  // *** 1) se a SQL está vazia aqui deveria gerar exceção ou deveria ser dentro
  // do vReader.Read????
  vSQL := FParser.Parse(vSQL);
  // *** 2) Acho que os parâmetros macros de FParse devem ser substituidos aqui
  // antes de chamar o PrepareStatementWithParams
  vConnection := GetCurrentConnectionItem;
  vStatement := nil;
  try
    vStatement := vConnection.Connection.PrepareStatementWithParams(
      vSQL, FParser.GetParams);
    SetParameters(vStatement, pSqlCommand.Params);
    DoLoad(vStatement, pSqlCommand, pList);
  finally
    if Assigned(vStatement) then
      vStatement.Close;
  end;
end;

{**

  @param pSqlCommand   ParameterDescription
  @param pResultSet   ParameterDescription
  @return ResultDescription
}

function TPersistenceEngine.GetRowFromResultSet(
  const pSqlCommand: ISQLCommandQuery;
  const pResultSet: IZResultSet): IInfraObject;
var
  vIndex: integer;
  vAttribute: IInfraType;
  vZeosType: IZTypeAnnotation;
  vAliasName: string;
  vTypeInfo: IClassInfo;
  vMetadata: IZResultSetMetadata;
begin
  // *** Será que isso deveria estar aqui?????
  //  if IsEqualGUID(pSqlCommand.GetClassID, InfraConsts.NullGUID) then
  //    Raise EPersistenceEngineError.Create(
  //      cErrorPersistEngineObjectIDUndefined);
  Result := TypeService.CreateInstance(
    pSqlCommand.ClassTypeInfo) as IInfraObject;
  if Assigned(Result) then
  begin
    vMetadata := pResultSet.GetMetadata;
    vTypeInfo := Result.TypeInfo;
    // A lista de colunas do ResultSet.GetMetadata do Zeos começa do 1.
    for vIndex := 1 to pResultSet.GetMetadata.GetColumnCount do
    begin
      vAliasName := vMetadata.GetColumnLabel(vIndex);
      vAttribute := vTypeInfo.GetProperty(Result, vAliasName) as IInfraType;
      if not Assigned(vAttribute) then
        raise EPersistenceEngineError.CreateFmt(
          cErrorPersistEngineAttributeNotFound,
          [vAliasName, vMetadata.GetColumnName(vIndex)]);
      if Supports(vAttribute.TypeInfo, IZTypeAnnotation, vZeosType) then
        vZeosType.NullSafeGet(pResultSet, vIndex, vAttribute)
      else
        raise EPersistenceEngineError.CreateFmt(
          cErrorPersistEngineCannotMapAttribute, [vAttribute.TypeInfo.Name]);
    end;
  end;
end;

function TPersistenceEngine.ReadTemplate(
  const pSqlCommand: ISQLCommand): string;
var
  vReader: ITemplateReader;
begin
  Result := GetSQLFromCache(pSqlCommand);
  if Result = EmptyStr then
  begin
    if Pos('#',pSQLCommand.Name) > 0 then
      vReader := GetReader(pSQLCommand)
    else
      vReader := GetReader;
    Result := vReader.Read(pSqlCommand);
    AddSQLToCache(pSqlCommand, Result);
  end;
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

function TPersistenceEngine.InTransaction: Boolean;
begin
  Result := not GetCurrentConnectionItem.Connection.GetAutoCommit;
end;

{**
  Caso nenhuma transação esteja em aberto, levanta ma exceção
}

procedure TPersistenceEngine.CheckInTransaction;
begin
  if not InTransaction then
    raise EPersistenceTransactionError.Create(cErrorNotInTransaction);
end;

{**
  Inicia uma nova transação com o nível de Isolamento especificado
  Se uma transação já estiver em andamento, resultará numa mensagem de erro
  @param pIsolationLevel Nível de isolamento da transaçao
}

procedure TPersistenceEngine.BeginTransaction(
  pIsolationLevel: TIsolationLevel);
begin
  if InTransaction then
    raise EPersistenceTransactionError.Create(cErrorAlreadyInTransaction);
  GetCurrentConnectionItem.Configure(pIsolationLevel);
end;

{**
  Efetiva a transação sendo executada
  Caso nenhuma transação esteja em aberto, resultará numa mensagem de erro
}

procedure TPersistenceEngine.Commit;
begin
  CheckInTransaction;
  GetCurrentConnectionItem.Connection.Commit;
end;

{**
  Desfaz a transação corrente
  fazendo com que todas as modificações realizadas pela transação sejam rejeitadas.
  Caso nenhuma transação esteja em aberto, resultará numa mensagem de erro
}

procedure TPersistenceEngine.Rollback;
begin
  CheckInTransaction;
  GetCurrentConnectionItem.Connection.Rollback;
end;

procedure TPersistenceEngine.AddSQLToCache(
  const pSqlCommand: ISQLCommand; pValue: string);
var
  vSqlCache: ISQLCacheList;
begin
  if Supports(pSqlCommand.ClassTypeInfo, ISQLCacheList, vSqlCache) then
    vSqlCache.Items[pSqlCommand.Name] := pValue;
end;

function TPersistenceEngine.GetSQLFromCache(
  const pSqlCommand: ISQLCommand): string;
var
  vSqlCache: ISQLCacheList;
begin
  Result := EmptyStr;
  if not Supports(pSqlCommand.ClassTypeInfo, ISqlCacheList, vSqlCache) then
  begin
    vSqlCache := TInfraSQLCache.Create;
    pSqlCommand.ClassTypeInfo.Inject(ISqlCacheList, vSqlCache);
  end
  else
    Result := vSqlCache.Items[pSqlCommand.Name]
end;

function TPersistenceEngine.GetReader(
  const pSqlCommand : ISQLCommand): ITemplateReader_Build;
begin
  if not Assigned(FSQLGenerator) then
    FSQLGenerator := TTemplateReader_Build.Create;
  Result := FSQLGenerator;
  Result.Configuration := FConfiguration;
end;

end.
