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
  TSession = class(TBaseElement, ISession)
  private
    FPersistenceEngine: IPersistenceEngine;
    /// Lista de comandos pendentes. Durante o Flush os comandos são executados e a lista é limpa
    FPendingCommands: ISQLCommandList;
  protected
    function CreateNamedQuery(const pCommandName: string;
      const pObj: IInfraObject = nil): ISQLCommandQuery; overload;
    function CreateNamedQuery(const pCommandName: string;
      const pClassID: TGUID): ISQLCommandQuery; overload;
    function CreateNamedQuery(const pCommandName: string;
      const pClassID: TGUID; const pListID: TGUID): ISQLCommandQuery; overload;
    function CreateNamedQuery(const pCommandName: string; const pObj: IInfraObject;
      const pListID: TGUID): ISQLCommandQuery; overload;
    function Delete(const pCommandName: string;
      const pObj: IInfraObject): ISQLCommand;
    function Save(const pCommandName: string;
      const pObj: IInfraObject): ISQLCommand;
    function Flush: Integer;
  public
    constructor Create(const pConnection: IZConnection;
      const pConfiguration: IConfiguration); reintroduce;
  end;

implementation

uses
  InfraConsts,
  List_SQLCommandList,
  InfraOPFSqlCommands,
  InfraOPFEngine;

{ TSession }
{*

  @param pPersistenceEngine   ParameterDescription
}
constructor TSession.Create(const pConnection: IZConnection;
  const pConfiguration: IConfiguration);
begin
  if not Assigned(pConnection) then
    raise EInfraArgumentError.Create('pConnection');
  if not Assigned(pConfiguration) then
    raise EInfraArgumentError.Create('pConfiguration');
  inherited Create;
  FPersistenceEngine := TPersistenceEngine.Create(pConfiguration, pConnection);
  FPendingCommands := TSQLCommandList.Create;
end;

{*

  @param pCommandName   ParameterDescription
  @param pClassID   ParameterDescription
  @return ResultDescription
}
function TSession.CreateNamedQuery(const pCommandName: string; const pClassID: TGUID): ISQLCommandQuery;
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
function TSession.CreateNamedQuery(const pCommandName: string; const pClassID, pListID: TGUID): ISQLCommandQuery;
begin
  Result := CreateNamedQuery(pCommandName, pClassID);
  Result.ListID := pListID;
end;

{*

  @param pCommandName   ParameterDescription
  @param pObj   ParameterDescription
  @param pListID   ParameterDescription
  @return ResultDescription
}
function TSession.CreateNamedQuery(const pCommandName: string; const pObj: IInfraObject = nil): ISQLCommandQuery;
begin
  if Assigned(pObj) then
    Result := CreateNamedQuery(pCommandName, pObj.TypeInfo.TypeID)
  else
    Result := CreateNamedQuery(pCommandName, NullGUID);
  if Assigned(pObj) then
    Result.Params.CreateParamsFrom(pObj);
end;

{*

  @param pCommandName   ParameterDescription
  @param pObj   ParameterDescription
  @return ResultDescription
}
function TSession.CreateNamedQuery(const pCommandName: string; const pObj: IInfraObject; const pListID: TGUID): ISQLCommandQuery;
begin
  Result := CreateNamedQuery(pCommandName, pObj.TypeInfo.TypeID, pListID);
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
begin
  // TODO: Dar atenção a isto porque, pelo modelo atual, pra cada Execute ele vai
  // abrir uma nova conexão. Isto impediria que todas as operações fossem feitas
  // no contexto de UMA transação
  Result := 0;
  for i := 0 to FPendingCommands.Count - 1 do
    Result := Result + FPersistenceEngine.Execute(FPendingCommands[i]);

  // Se deu tudo ok, limpa a lista de pendencias
  FPendingCommands.Clear;
end;

end.

