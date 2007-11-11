unit InfraDBXConnection;

interface

uses
  InfraCommon, InfraValueTypeIntf, InfraHibernateIntf,
  DB, SqlExpr;

type
  // Classe concreta de Conexão utilizando DBExpress (DBX)
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
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, InfraValueTypeConvert, InfraValueType;

{ TDBXConnection }

procedure TDBXConnection.Close;
begin
  FConnection.Close;
end;

{
  Cria um componente de conexão DBExpress e define alguns valores padrões.
  -- Por enquanto utilizando interbase, mas isso deve ser alterado
  futuramente --
}
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
    VendorLib := 'fbclient.dll';
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

// Cria um ResulSet e abre a instrução passada como parâmetro.
function TDBXConnection.ExecuteQuery(const pSQL: String): IResultSet;
begin
  Result := TDBXResultSet.Create(FConnection) as IResultSet;
  Result.Open(pSQL);
end;

// Executa a instrução passada como parâmetro diretamente no objeto connection.
function TDBXConnection.ExecuteUpdate(const pSQL: String): Integer;
begin
  Result := FConnection.ExecuteDirect(pSQL);
end;

// fecha o resultset
function TDBXConnection.GetIsClosed: Boolean;
begin
  Result := not FConnection.Connected;
end;

{ TDBXResultSet }

// fecha o resultset
procedure TDBXResultSet.Close;
begin
  FQuery.Close;
end;

// Cria um dataset e seta sua conexão.
constructor TDBXResultSet.Create(pConnection: TSQLConnection);
begin
  FQuery := TSQLQuery.Create(nil);
  FQuery.SQLConnection := pConnection;
end;

destructor TDBXResultSet.Destroy;
begin
  FQuery.Free;
  inherited;
end;

// verifica se chegou ao final do resultset
function TDBXResultSet.EOF: Boolean;
begin
  Result := FQuery.Eof;
end;

// vai para o primeiro item do resultset
procedure TDBXResultSet.First;
begin
  FQuery.First;
end;

// Retorna um infratype com o valor do campo passado como parâmetro
function TDBXResultSet.GetValue(const FieldName: String): IInfraType;
var
  vVariant: IInfraVariant;
  Field: TField;
begin
  Field := FQuery.FindField(FieldName);
  vVariant := TInfraVariant.NewFrom(Field.Value);
  Result := GetConverter(Field.DataType).ConvertToLeft(vVariant);
end;

// Retorna o conversor de variant adequado ao tipo de campo
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

// vai para o proximo item do resultset
procedure TDBXResultSet.Next;
begin
  FQuery.Next;
end;

// Define e abre o dataset com a instrução SQL passada como parâmetro.
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
