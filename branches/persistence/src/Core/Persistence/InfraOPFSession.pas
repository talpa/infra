unit InfraOPFSession;

interface

uses
  SysUtils,
  ZDbcIntfs,
  InfraCommon,
  InfraCommonIntf,
  InfraOPFIntf,
  InfraValueTypeIntf;

type
  /// Descrição da classe
  TSession = class(TBaseElement, ISession,ITransaction)
  private
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
    procedure BeginTransaction(pTransactIsolationLevel: TransactionKind = tkReadCommitted);
    procedure Commit;
    procedure Rollback;
  public
    constructor Create(const pConfiguration: IConfiguration); reintroduce;

  end;

implementation

uses
  InfraConsts,
  List_SQLCommandList,
  InfraOPFSqlCommands,
  InfraOPFEngine,
  InfraOPFConsts;

{ TSession }
{*

  @param pPersistenceEngine   ParameterDescription
}
constructor TSession.Create(const pConfiguration: IConfiguration);
begin
  if not Assigned(pConfiguration) then
    raise EInfraArgumentError.CreateFmt(cErrorPersistenceWithoutConfig,
      ['TSession.Create']);
  inherited Create;
  FPersistenceEngine := TPersistenceEngine.Create(pConfiguration);
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

procedure TSession.BeginTransaction(pTransactIsolationLevel: TransactionKind);
begin
  (FPersistenceEngine as ITransaction).BeginTransaction(pTransactIsolationLevel);
end;

procedure TSession.Commit;
begin
  (FPersistenceEngine as ITransaction).Commit;
end;

procedure TSession.Rollback;
begin
  (FPersistenceEngine as ITransaction).Rollback;
end;

end.

