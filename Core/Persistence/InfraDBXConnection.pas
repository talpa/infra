unit InfraDBXConnection;

interface

uses
  InfraCommon, InfraValueTypeIntf, InfraHibernateIntf,
  DB, SqlExpr;

type
  TDBXConnection = class(TElement, IConnection, IDBXConnection)
  private
    FConnection: TSQLConnection;
    procedure Close;
    function ExecuteQuery(const pSQL: String): IResultSet;
    function ExecuteUpdate(const pSQL: String): Integer;
    function GetIsClosed: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    property IsClosed: Boolean read GetIsClosed;
  end;

  TDBXResultSet = class(TElement, IResultSet, IDBXResultSet)
  private
    FQuery: TSQLQuery;
    procedure Close;
    function EOF: Boolean;
    function GetValue(const FieldName: String): IInfraType;
    procedure First;
    procedure Next;
    procedure Open(const pSQL: string);
    function GetConverter(pFieldType: TFieldType): ITypeConverter;
  public
    constructor Create(pConnection: TSQLConnection); reintroduce;
  end;

implementation

uses
  SysUtils, InfraValueTypeConvert, InfraValueType;

{ TDBXConnection }

procedure TDBXConnection.Close;
begin
  FConnection.Close;
end;

constructor TDBXConnection.Create;
begin
  inherited Create;
  FConnection := TSQLConnection.Create(nil);
  with FConnection do
  begin
    ConnectionName := 'IBConnection';
    DriverName := 'Interbase';
    GetDriverFunc := 'getSQLDriverINTERBASE';
    LibraryName := 'dbexpint.dll';
    VendorLib := 'gds32.dll';
    with PersistenceService.Configuration do
    begin
      Params.Add('ServerCharSet=WIN1252');
      Params.Add('SQLDialect=3');
      Params.Add('Interbase TransIsolation=ReadCommited');
      Params.Add('RoleName=RoleName');
      Params.Add('ErrorResourceFile=');
      Params.Add('LocaleCode=0000');
      Params.Add('BlobSize=-1');
      Params.Add('CommitRetain=False');
      Params.Add('WaitOnLocks=True');
      Params.Add('Trim Char=False');
      Params.Add('Database=' + PropertyItem['Connection.DatabasePath']);
      Params.Add('User_Name=' + PropertyItem['Connection.DatabaseLogin']);
      Params.Add('Password=' + PropertyItem['Connection.DatabasePassword']);
    end;
  end;
end;

destructor TDBXConnection.Destroy;
begin
  FConnection.Free;
  inherited;
end;

function TDBXConnection.ExecuteQuery(const pSQL: String): IResultSet;
begin
  Result := TDBXResultSet.Create(FConnection) as IResultSet;
  Result.Open(pSQL);
end;

function TDBXConnection.ExecuteUpdate(const pSQL: String): Integer;
begin
  Result := FConnection.ExecuteDirect(pSQL);
end;

function TDBXConnection.GetIsClosed: Boolean;
begin
  Result := not FConnection.Connected;
end;

{ TDBXResultSet }

procedure TDBXResultSet.Close;
begin
  FQuery.Close;
end;

constructor TDBXResultSet.Create(pConnection: TSQLConnection);
begin
  FQuery := TSQLQuery.Create(nil);
  FQuery.SQLConnection := pConnection;
end;

function TDBXResultSet.EOF: Boolean;
begin
  Result := FQuery.Eof;
end;

procedure TDBXResultSet.First;
begin
  FQuery.First;
end;

function TDBXResultSet.GetValue(const FieldName: String): IInfraType;
var
  vVariant: IInfraVariant;
  Field: TField;
begin
  Field := FQuery.FindField(FieldName);
  vVariant := TInfraVariant.NewFrom(Field.Value);
  Result := GetConverter(Field.DataType).ConvertToLeft(vVariant);
end;

function TDBXResultSet.GetConverter(pFieldType: TFieldType): ITypeConverter;
begin
  case pFieldType of
    ftString, ftFixedChar, ftWideString:
      Result := TStringToVariant.Create;
    ftSmallint, ftInteger, ftWord, ftAutoInc, ftLargeint:
      Result := TIntegerToVariant.Create;
    ftBoolean:
      Result := TBooleanToVariant.Create;
    ftFloat, ftCurrency, ftBCD, ftFMTBcd:
      Result := TDoubleToVariant.Create;
    ftDate, ftTime, ftDateTime, ftTimeStamp:
      Result := TDateTimeToVariant.Create;
  else
    Result := TNullConverter.Create;
  end;
end;

procedure TDBXResultSet.Next;
begin
  FQuery.Next;
end;

procedure TDBXResultSet.Open(const pSQL: string);
begin
  FQuery.SQL.Text := pSQL;
  FQuery.Open;
end;

{
function TResultSet.GetTypeByFieldType(pFieldType: TFieldType): TGUID;
begin
  case pFieldType of
    ftString, ftFixedChar, ftWideString:
      Result := IInfraString;
    ftSmallint, ftInteger, ftWord, ftAutoInc, ftLargeint:
      Result := IInfraInteger;
    ftBoolean:
      Result := IInfraBoolean;
    ftFloat, ftCurrency, ftBCD, ftFMTBcd:
      Result := IInfraDouble;
    ftDate:
      Result := IInfraDate;
    ftTime:
      Result := IInfraTime;
    ftDateTime, ftTimeStamp:
      Result := IInfraDateTime;
  else
    Result := NullGUID;
  end;
end;
}

end.
