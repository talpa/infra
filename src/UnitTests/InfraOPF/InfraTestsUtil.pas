unit InfraTestsUtil;

interface

uses
  InfraOPFIntf, ZDbcIntfs;

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

function GetZeosExecutor: TDBZeosExecutor;
procedure ReleaseZeosExecutor;

implementation

uses
  SysUtils,
  InfraPersistence,
  InfraOPFConfiguration,
  InfraOPFConsts;

var
  _dbZeosExecutor: TDBZeosExecutor;

function GetZeosExecutor: TDBZeosExecutor;
begin
  if not Assigned(_dbZeosExecutor) then
    _dbZeosExecutor := TDBZeosExecutor.Create;
  Result := _dbZeosExecutor;
end;

procedure ReleaseZeosExecutor;
begin
  _dbZeosExecutor := nil;
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

{ TTestsUtil }

class function TTestsUtil.GetNewConfiguration: IConfiguration;
begin
  Result := TConfiguration.Create;
  Result.SetValue(cCONFIGKEY_DRIVER, 'firebird-2.0');
  Result.SetValue(cCONFIGKEY_CONNECTIONTIME, 10);
  Result.SetValue(cCONFIGKEY_MAXCONNECTIONS, 2);
end;

initialization

finalization
  if Assigned(_dbZeosExecutor) then
    FreeAndNil(_dbZeosExecutor);

end.

