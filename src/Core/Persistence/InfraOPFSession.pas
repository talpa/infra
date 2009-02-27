unit InfraOPFSession;

interface

uses
  SysUtils,
  InfraCommon,
  InfraCommonIntf,
  InfraOPFIntf,
  InfraValueTypeIntf;

type
  /// Descrição da classe                                                                  
  TSession = class(TBaseElement, ISession)
  private
    /// Persistence engine associado, usado para acessar o Storage
    FPersistenceEngine: IPersistenceEngine;
    /// Lista de comandos pendentes. Durante o Flush os comandos são executados e a lista é limpa
    FPendingCommands: ISQLCommandList;
  protected
    function CreateNamedQuery(const pCommandName: string; const pClassID: TGUID): ISQLCommandQuery; overload;
    function CreateNamedQuery(const pCommandName: string; const pObj: IInfraObject): ISQLCommandQuery; overload;
    function CreateNamedQuery(const pCommandName: string; const pClassID: TGUID; const pListID: TGUID): ISQLCommandQuery; overload;
    function CreateNamedQuery(const pCommandName: string; const pObj: IInfraObject; const pListID: TGUID): ISQLCommandQuery; overload;
    function Delete(const pCommandName: string; const pObj: IInfraObject): ISQLCommand;
    function Save(const pCommandName: string; const pObj: IInfraObject): ISQLCommand;
    function Flush: Integer;
    procedure BeginTransaction(pIsolationLevel: TIsolationLevel = tilReadCommitted);
    procedure Commit;
    procedure Rollback;
  public
    constructor Create(const pConfiguration: IConfiguration;
      const pConnectionProvider: IConnectionProvider); reintroduce;
  end;

implementation

uses
  InfraConsts,
  List_SQLCommandList,
  InfraOPFSqlCommands,
  InfraOPFConsts, InfraOPFEngine;

{ TSession }
{*

  @param pConfiguration   Configurações gerais para o Session e agregados
  @param pConnectionProvider   Provedor de Conexões ao banco
}
constructor TSession.Create(const pConfiguration: IConfiguration;
  const pConnectionProvider: IConnectionProvider);
begin
  inherited Create;
  if not Assigned(pConfiguration) then
    raise EInfraArgumentError.CreateFmt(cErrorPersistenceWithoutConfig,
      ['TSession.Create']);
  if not Assigned(pConnectionProvider) then
    raise EInfraArgumentError.CreateFmt(cErrorPersistenceWithoutConnProvider,
      ['TSession.Create']);
  FPersistenceEngine := TPersistenceEngine.Create(pConfiguration,
    pConnectionProvider);
  FPendingCommands := TSQLCommandList.Create;
end;

{*

  @param pCommandName   ParameterDescription
  @param pClassID   ParameterDescription
  @return ResultDescription
}
function TSession.CreateNamedQuery(const pCommandName: string;
  const pClassID: TGUID): ISQLCommandQuery;
begin
  Result := TSQLCommandQuery.Create(FPersistenceEngine);
  Result.Name := pCommandName;
  Result.ListID := IInfraList;
  if not IsEqualGUID(pClassID, NullGUID) then
    Result.ClassID := pClassID;
end;

{*

  @param pCommandName   ParameterDescription
  @param pObj   ParameterDescription
  @param pListID   ParameterDescription
  @return ResultDescription
}
function TSession.CreateNamedQuery(const pCommandName: string;
  const pObj: IInfraObject): ISQLCommandQuery;
begin
  if not Assigned(pObj) then
    raise EInfraArgumentError.Create('Object in TSession.CreateNamedQuery');
  Result := CreateNamedQuery(pCommandName, pObj.TypeInfo.TypeID);
  Result.Params.CreateParamsFrom(pObj);
end;

{*

  @param pCommandName   ParameterDescription
  @param pClassID   ParameterDescription
  @param pListID   ParameterDescription
  @return ResultDescription
}
function TSession.CreateNamedQuery(const pCommandName: string;
  const pClassID, pListID: TGUID): ISQLCommandQuery;
begin
  Result := CreateNamedQuery(pCommandName, pClassID);
  Result.ListID := pListID;
end;

{*

  @param pCommandName   ParameterDescription
  @param pObj   ParameterDescription
  @return ResultDescription
}
function TSession.CreateNamedQuery(const pCommandName: string;
  const pObj: IInfraObject; const pListID: TGUID): ISQLCommandQuery;
begin
  Result := CreateNamedQuery(pCommandName, pObj);
  Result.ListID := pListID;
end;

{*

  @param pCommandName   ParameterDescription
  @param pObj   ParameterDescription
  @return ResultDescription
}
function TSession.Save(const pCommandName: string;
  const pObj: IInfraObject): ISQLCommand;
begin
  if not Assigned(pObj) then
    raise EInfraArgumentError.Create('Object in TSession.Save');
  Result := TSQLCommand.Create(FPersistenceEngine);
  with Result do
  begin
    Name := pCommandName;
    Params.CreateParamsFrom(pObj);
  end;
  FPendingCommands.Add(Result);
end;

{*

  @return ResultDescription
}
function TSession.Delete(const pCommandName: string;
  const pObj: IInfraObject): ISQLCommand;
//var
//  vState: IPersistentState;
begin
//  if Supports(pObj, IPersistentState, vState) then
//  begin
//    vState.State := osDeleted;
    Save(pCommandName, pObj);
//  end;
end;

{*
  Envia todos os comandos pendentes para o banco de dados
  @return A quantidade de registros afetados pela gravação
}
function TSession.Flush: Integer;
begin
  Result := 0;
  if FPendingCommands.Count <> 0 then
  begin
    Result := FPersistenceEngine.ExecuteAll(FPendingCommands);
    FPendingCommands.Clear;
  end;
end;

{**
  Inicia uma nova transação nesta unidade de trabalho
  @param pIsolationLevel Nível de isolamento desejado
}
procedure TSession.BeginTransaction(pIsolationLevel: TIsolationLevel =
  tilReadCommitted);
begin
  (FPersistenceEngine as ITransaction).BeginTransaction(pIsolationLevel);
end;

{**
  Efetiva a transação
  Se houver algum comando na lista de pendencias, executa o Flush antes
}
procedure TSession.Commit;
begin
  // Se houver algum comando na lista de pendencias, executa o Flush
  // para enviá-los ao banco de dados antes de dar o Commit
  if FPendingCommands.Count > 0 then
    Flush;
  // Efetiva a transação
  (FPersistenceEngine as ITransaction).Commit;
end;

{**
  Desfaz a transação e limpa a lista de comandos pendentes
}
procedure TSession.Rollback;
begin
  // Desfaz a transação
  (FPersistenceEngine as ITransaction).Rollback;
  // Limpa a lista de pendências, já que a transação foi desfeita
  FPendingCommands.Clear;
end;

end.
