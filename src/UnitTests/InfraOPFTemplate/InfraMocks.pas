unit InfraMocks;

interface

uses
  Classes,
  ZDbcIntfs,
  ZClasses,
  ZDbcLogging;

type
  TDriverManagerMock = class(TInterfacedObject, IZDriverManager)
  public
    function GetConnection(const Url: string): IZConnection;
    function GetConnectionWithParams(const Url: string; Info: TStrings): IZConnection;
    function GetConnectionWithLogin(const Url: string; const User: string;
      const Password: string): IZConnection;

    function GetDriver(const Url: string): IZDriver;
    function GetClientVersion(const Url: string): Integer;
    procedure RegisterDriver(Driver: IZDriver);
    procedure DeregisterDriver(Driver: IZDriver);

    function GetDrivers: IZCollection;

    function GetLoginTimeout: Integer;
    procedure SetLoginTimeout(Seconds: Integer);

    procedure AddLoggingListener(Listener: IZLoggingListener);
    procedure RemoveLoggingListener(Listener: IZLoggingListener);

    procedure LogMessage(Category: TZLoggingCategory; const Protocol: string;
      const Msg: string);
    procedure LogError(Category: TZLoggingCategory; const Protocol: string;
      const Msg: string; ErrorCode: Integer; const Error: string);
  end;

  TZConnectionMock = class(TInterfacedObject, IZConnection)
  private
    FActive: Boolean;
    FAutoCommit: Boolean;
    FInTransaction: Boolean;
    FReadOnly: Boolean;
  public
    function CreateStatement: IZStatement;
    function PrepareStatement(const SQL: string): IZPreparedStatement;
    function PrepareCall(const SQL: string): IZCallableStatement;

    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings):
      IZPreparedStatement;
    function PrepareCallWithParams(const SQL: string; Info: TStrings):
      IZCallableStatement;

    function CreateNotification(const Event: string): IZNotification;
    function CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence;

    function NativeSQL(const SQL: string): string;

    procedure SetAutoCommit(Value: Boolean);
    function GetAutoCommit: Boolean;

    procedure Commit;
    procedure Rollback;

    //2Phase Commit Support initially for PostgresSQL (firmos) 21022006
    procedure PrepareTransaction(const transactionid: string);
    procedure CommitPrepared(const transactionid: string);
    procedure RollbackPrepared(const transactionid: string);


    //Ping Server Support (firmos) 27032006

    function PingServer: Integer;
    function EscapeString(Value : String) : String;


    procedure Open;
    procedure Close;
    function IsClosed: Boolean;

    function GetDriver: IZDriver;
    function GetMetadata: IZDatabaseMetadata;
    function GetParameters: TStrings;
    function GetClientVersion: Integer;
    function GetHostVersion: Integer;

    procedure SetReadOnly(Value: Boolean);
    function IsReadOnly: Boolean;

    procedure SetCatalog(const Value: string);
    function GetCatalog: string;

    procedure SetTransactionIsolation(Value: TZTransactIsolationLevel);
    function GetTransactionIsolation: TZTransactIsolationLevel;

    function GetWarnings: EZSQLWarning;
    procedure ClearWarnings;
  end;

implementation

{ TDriverManagerMock }

procedure TDriverManagerMock.AddLoggingListener(
  Listener: IZLoggingListener);
begin

end;

procedure TDriverManagerMock.DeregisterDriver(Driver: IZDriver);
begin

end;

function TDriverManagerMock.GetClientVersion(const Url: string): Integer;
begin
  Result := -1
end;

function TDriverManagerMock.GetConnection(const Url: string): IZConnection;
begin
  Result := TZConnectionMock.Create;
end;

function TDriverManagerMock.GetConnectionWithLogin(const Url, User,
  Password: string): IZConnection;
begin
  Result := TZConnectionMock.Create;
end;

function TDriverManagerMock.GetConnectionWithParams(const Url: string;
  Info: TStrings): IZConnection;
begin
  Result := TZConnectionMock.Create;
end;

function TDriverManagerMock.GetDriver(const Url: string): IZDriver;
begin

end;

function TDriverManagerMock.GetDrivers: IZCollection;
begin

end;

function TDriverManagerMock.GetLoginTimeout: Integer;
begin
  Result := -1;
end;

procedure TDriverManagerMock.LogError(Category: TZLoggingCategory;
  const Protocol, Msg: string; ErrorCode: Integer; const Error: string);
begin

end;

procedure TDriverManagerMock.LogMessage(Category: TZLoggingCategory;
  const Protocol, Msg: string);
begin

end;

procedure TDriverManagerMock.RegisterDriver(Driver: IZDriver);
begin

end;

procedure TDriverManagerMock.RemoveLoggingListener(
  Listener: IZLoggingListener);
begin

end;

procedure TDriverManagerMock.SetLoginTimeout(Seconds: Integer);
begin

end;

{ TZConnectionMock }

procedure TZConnectionMock.ClearWarnings;
begin

end;

procedure TZConnectionMock.Close;
begin
  FActive := False;
end;

procedure TZConnectionMock.Commit;
begin
  FInTransaction := False;
end;

procedure TZConnectionMock.CommitPrepared(const transactionid: string);
begin
  FInTransaction := False;
end;

function TZConnectionMock.CreateNotification(
  const Event: string): IZNotification;
begin
  Result := nil;
end;

function TZConnectionMock.CreateSequence(const Sequence: string;
  BlockSize: Integer): IZSequence;
begin
  Result := nil;
end;

function TZConnectionMock.CreateStatement: IZStatement;
begin
  Result := nil;
end;

function TZConnectionMock.CreateStatementWithParams(
  Info: TStrings): IZStatement;
begin
  Result := nil;
end;

function TZConnectionMock.EscapeString(Value: String): String;
begin
  Result := '';
end;

function TZConnectionMock.GetAutoCommit: Boolean;
begin
  Result := FAutoCommit;
end;

function TZConnectionMock.GetCatalog: string;
begin
  Result := '';
end;

function TZConnectionMock.GetClientVersion: Integer;
begin
  Result := -1;
end;

function TZConnectionMock.GetDriver: IZDriver;
begin
  Result := nil;
end;

function TZConnectionMock.GetHostVersion: Integer;
begin
  Result := -1;
end;

function TZConnectionMock.GetMetadata: IZDatabaseMetadata;
begin
  Result := nil;
end;

function TZConnectionMock.GetParameters: TStrings;
begin
  Result := nil;
end;

function TZConnectionMock.GetTransactionIsolation: TZTransactIsolationLevel;
begin
  Result := tiReadCommitted;
end;

function TZConnectionMock.GetWarnings: EZSQLWarning;
begin
  Result := nil;
end;

function TZConnectionMock.IsClosed: Boolean;
begin
  Result := not FActive;
end;

function TZConnectionMock.IsReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TZConnectionMock.NativeSQL(const SQL: string): string;
begin
  Result := '';
end;

procedure TZConnectionMock.Open;
begin
  FActive := True;
end;

function TZConnectionMock.PingServer: Integer;
begin
  Result := -1;
end;

function TZConnectionMock.PrepareCall(
  const SQL: string): IZCallableStatement;
begin
  Result := nil;
end;

function TZConnectionMock.PrepareCallWithParams(const SQL: string;
  Info: TStrings): IZCallableStatement;
begin
  Result := nil;
end;

function TZConnectionMock.PrepareStatement(
  const SQL: string): IZPreparedStatement;
begin
  Result := nil;
end;

function TZConnectionMock.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  Result := nil;
end;

procedure TZConnectionMock.PrepareTransaction(const transactionid: string);
begin

end;

procedure TZConnectionMock.Rollback;
begin
  FInTransaction := False;
end;

procedure TZConnectionMock.RollbackPrepared(const transactionid: string);
begin
  FInTransaction := False;
end;

procedure TZConnectionMock.SetAutoCommit(Value: Boolean);
begin
  FAutoCommit := Value;
end;

procedure TZConnectionMock.SetCatalog(const Value: string);
begin

end;

procedure TZConnectionMock.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TZConnectionMock.SetTransactionIsolation(
  Value: TZTransactIsolationLevel);
begin

end;

end.
