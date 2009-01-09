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
    FPersistenceEngine: IPersistenceEngine;
    /// Lista de comandos pendentes. Durante o Flush os comandos são executados e a lista é limpa
    FPendingCommands: ISQLCommandList;
  protected
    function CreateQuery(const pCommandName: string;
      const pObj: IInfraObject = nil): ISQLCommandQuery; overload;
    function CreateQuery(const pCommandName: string;
      const pClassID: TGUID): ISQLCommandQuery; overload;
    function CreateQuery(const pCommandName: string;
      const pClassID: TGUID; const pListID: TGUID): ISQLCommandQuery; overload;
    function CreateQuery(const pCommandName: string; const pObj: IInfraObject;
      const pListID: TGUID): ISQLCommandQuery; overload;
    function Delete(const pCommandName: string;
      const pObj: IInfraObject): ISQLCommand;
    function Save(const pCommandName: string;
      const pObj: IInfraObject): ISQLCommand;
    function Flush: Integer;
  public
    constructor Create(const pPersistenceEngine: IPersistenceEngine); reintroduce;
  end;

implementation

uses
  ZDbcIntfs,
  InfraConsts,
  List_SQLCommandList,
  InfraOPFSqlCommands;

{ TSession }
{*

  @param pPersistenceEngine   ParameterDescription
}
constructor TSession.Create(const pPersistenceEngine: IPersistenceEngine);
begin
  if not Assigned(pPersistenceEngine) then
    raise EInfraArgumentError.Create('PersistenceEngine in Session.Create');
  inherited Create;
  FPersistenceEngine := pPersistenceEngine;
  FPendingCommands := TSQLCommandList.Create;
end;

{*

  @param pCommandName   ParameterDescription
  @param pClassID   ParameterDescription
  @return ResultDescription
}
function TSession.CreateQuery(const pCommandName: string; const pClassID: TGUID): ISQLCommandQuery;
begin
  Result := TSQLCommandQuery.Create(FPersistenceEngine);
  Result.Name := pCommandName;
  Result.ListID := IInfraList;
  if not IsEqualGUID(pClassID, NullGUID) then
    Result.ClassID := pClassID;
end;

{*

  @param pCommandName   ParameterDescription
  @param pClassID   ParameterDescription
  @param pListID   ParameterDescription
  @return ResultDescription
}
function TSession.CreateQuery(const pCommandName: string; const pClassID, pListID: TGUID): ISQLCommandQuery;
begin
  Result := CreateQuery(pCommandName, pClassID);
  Result.ListID := pListID;
end;

{*

  @param pCommandName   ParameterDescription
  @param pObj   ParameterDescription
  @param pListID   ParameterDescription
  @return ResultDescription
}
function TSession.CreateQuery(const pCommandName: string; const pObj: IInfraObject = nil): ISQLCommandQuery;
begin
  if Assigned(pObj) then
    Result := CreateQuery(pCommandName, pObj.TypeInfo.TypeID)
  else
    Result := CreateQuery(pCommandName, NullGUID);
  if Assigned(pObj) then
    Result.Params.CreateParamsFrom(pObj);
end;

{*

  @param pCommandName   ParameterDescription
  @param pObj   ParameterDescription
  @return ResultDescription
}
function TSession.CreateQuery(const pCommandName: string; const pObj: IInfraObject; const pListID: TGUID): ISQLCommandQuery;
begin
  Result := CreateQuery(pCommandName, pObj.TypeInfo.TypeID, pListID);
  Result.Params.CreateParamsFrom(pObj);
end;

{*

  @param pCommandName   ParameterDescription
  @param pObj   ParameterDescription
  @return ResultDescription
}
function TSession.Save(const pCommandName: string; const pObj: IInfraObject): ISQLCommand;
begin
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
function TSession.Delete(const pCommandName: string; const pObj: IInfraObject): ISQLCommand;
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

  @param pConfiguration   ParameterDescription
  @return ResultDescription
}
function TSession.Flush: Integer;
var
  i: integer;
  vConnection: IZConnection;
begin
  // TODO: Dar atenção a isto porque, pelo modelo atual, pra cada Execute ele vai
  // abrir uma nova conexão. Isto impediria que todas as operações fossem feitas
  // no contexto de UMA transação
  Result := 0;
  vConnection := FPersistenceEngine.ConnectionProvider.GetConnection;
  for i := 0 to FPendingCommands.Count - 1 do
    Result := Result + FPersistenceEngine.Execute(vConnection, FPendingCommands[i]);

  // Se deu tudo ok, limpa a lista de pendencias
  FPendingCommands.Clear;
end;

end.

