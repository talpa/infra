unit InfraOPFSqlCommands;

interface

uses
  InfraCommon,
  InfraCommonIntf,
  InfraOPFIntf,
  InfraValueTypeIntf;

type
  /// Descrição da classe
  TSQLCommand = class(TBaseElement, ISQLCommand)
  private
    FClassTypeInfo: IClassInfo;
    FName: string;
    FParams: ISQLCommandParams;
    FPersistenceEngine: IPersistenceEngine;
  protected
    function GetName: string;
    function GetParams: ISQLCommandParams;
    procedure SetName(const Value: string);
    function GetClassTypeInfo: IClassInfo;
    procedure SetClassTypeInfo(const Value: IClassInfo);
    /// PersistenceEngine ao qual o SQLCommand está vinculado
    property PersistenceEngine: IPersistenceEngine read FPersistenceEngine;
    property Params: ISQLCommandParams read GetParams;
    property ClassTypeInfo: IClassInfo read GetClassTypeInfo write SetClassTypeInfo;
  public
    constructor Create(const pPersistenceEngine: IPersistenceEngine); reintroduce;
  end;

  /// Descrição da classe
  TSQLCommandQuery = class(TSQLCommand, ISQLCommandQuery)
  private
    FListID: TGUID;
    function CreateList: IInfraList;
  protected
    function GetResult: IInfraType;
    function GetList: IInfraList;
    function GetListID: TGUID;
    procedure SetListID(const Value: TGUID);
    property ListID: TGUID read GetListID write SetListID;
  end;

implementation

uses
  List_SQLCommandParam;

{ TSQLCommand }

{*
  Cria uma nova instância de TSQLCommand.
  @param pPersistenceEngine
}
constructor TSQLCommand.Create(const pPersistenceEngine: IPersistenceEngine);
begin
  inherited Create;
  FPersistenceEngine := pPersistenceEngine;
  FParams := TSQLCommandParams.Create;
end;

{*

  @return Retorna o nome do template
}
function TSQLCommand.GetName: string;
begin
  Result := FName;
end;

{*

  @param Value O nome do template
}
procedure TSQLCommand.SetName(const Value: string);
begin
  FName := Value;
end;

{*

  @return ResultDescription
}
function TSQLCommand.GetParams: ISQLCommandParams;
begin
  Result := FParams;
end;

{ TSQLCommandQuery }

{*

  @return ResultDescription
}
function TSQLCommandQuery.GetListID: TGUID;
begin
  Result := FListID;
end;

{*

  @return ResultDescription
}
function TSQLCommandQuery.CreateList: IInfraList;
begin
  Result := TypeService.CreateInstance(FListID) as IInfraList;
end;

{*

  @return ResultDescription
}
function TSQLCommandQuery.GetResult: IInfraType;
var
  vList: IInfraList;
begin
  vList := CreateList;
  PersistenceEngine.Load(Self, vList);
  // *** deveria gerar exceção caso o load acima retornar mais de um objeto na lista????
  Result := vList[0] as IInfratype;
end;

{*

  @return ResultDescription
}
function TSQLCommandQuery.GetList: IInfraList;
begin
  Result := CreateList;
  PersistenceEngine.Load(Self, Result);
end;

{*

  @param Value   ParameterDescription
}
procedure TSQLCommandQuery.SetListID(const Value: TGUID);
begin
  FListID := Value;
end;

{*

  @return ResultDescription
}
function TSQLCommand.GetClassTypeInfo: IClassInfo;
begin
  Result := FClassTypeInfo
end;
{*

  @param Value   ParameterDescription
}
procedure TSQLCommand.SetClassTypeInfo(const Value: IClassInfo);
begin
  FClassTypeInfo := Value;
end;

end.

