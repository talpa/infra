unit InfraTestsUtil;

interface

uses
  InfraPersistenceIntf, ZDbcIntfs;

type
  TDBZeosExecutor = class
  private
    FConnection: IZConnection;
  public
    procedure OpenConnection(pConnString: string);
    function Open(pSql: string): IZResultSet;
    function Execute(pSql: string): Integer;
  end;

  TTestsUtil = class
  public
    class function GetNewConfiguration: IConfiguration;
  end;

implementation

uses
  InfraPersistence, InfraPersistenceConsts;

class function TTestsUtil.GetNewConfiguration: IConfiguration;
begin
  Result := TConfiguration.Create;
  Result.SetValue(cCONFIGKEY_CONNECTIONTIME, 10);
  Result.SetValue(cCONFIGKEY_MAXCONNECTIONS, 2);
end;

{ TDBZeosExecutor }

procedure TDBZeosExecutor.OpenConnection(pConnString: string);
begin
  inherited Create;
  FConnection := DriverManager.GetConnection(pConnString);
end;

function TDBZeosExecutor.Execute(pSql: string): Integer;
var
  vStatement: IZStatement;
begin
  vStatement := FConnection.CreateStatement;
  Result := vStatement.ExecuteUpdate(pSql);
end;

function TDBZeosExecutor.Open(pSql: string): IZResultSet;
var
  vStatement: IZStatement;
begin
  vStatement := FConnection.CreateStatement;
  Result := vStatement.ExecuteQuery(pSql);
end;

end.
