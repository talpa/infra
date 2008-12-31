unit InfraPersistence;

interface
uses
  SysUtils,
  SyncObjs,
  {TStrings}Classes,
  {Zeos}ZDbcIntfs,
  {Infra}InfraCommon,
  {InfraInf}InfraCommonIntf, InfraValueTypeIntf, InfraPersistenceIntf;

type
  EInfraConnectionProviderError = class(Exception);
  
  TConfiguration = class(TBaseElement, IConfiguration)
    FProperties: TStrings;
    function GetProperties: TStrings;
    function GetPropertyItem(const pName: string): string;
    procedure SetPropertyItem(const pName: string; const Value: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetValue(const pName: string; const pDefaultValue: Integer): Integer; overload;
    function GetValue(const pName: string; const pDefaultValue: Double): Double; overload;
    function GetValue(const pName: string; const pDefaultValue: string): string; overload;
    property Properties: TStrings read GetProperties;
    property PropertyItem[const pName: string]: string read GetPropertyItem write SetPropertyItem;
  end;

  /// Classe responsável por prover conexões com o SGDB
  TConnectionProvider = class(TBaseElement, IConnectionProvider)
  private
    FConfiguration: IConfiguration;
    FDriverManager: IZDriverManager;    // O DriverManager que será usado pra criar as conexões
    FPool: array of IZConnection;       // O pool
    FCriticalSection: TCriticalSection;
    function BuildConnectionString(pConfiguration: IConfiguration): string;
    procedure CloseConnections; // CriticalSection usado para evitar conflitos em aplicações multi-thread
  protected
    function GetFreeConnection: IZConnection; // Procura por uma conexao livre
    function CreateConnection: IZConnection;  // Cria uma nova conexao
    function FindConnection(const pConnection: IZConnection): IZConnection; // Procura por uma conexao no pool
  public
    constructor Create(pDriverManager: IZDriverManager; pConfiguration: IConfiguration); reintroduce;
    destructor Destroy; override;
    function GetConnection: IZConnection; // Caso tenha conexoes disponíveis no Pool bloqueia uma e retorna-a
    procedure Close; // Fecha todas as conexões do pool
    procedure ReleaseConnection(const pConnection: IZConnection); // Devolve a conexao ao Pool
  end;

const
  MAX_CONNECTIONS_KEY = 'max_connections';
  POOL_EXPIRATION_TIME_KEY = 'pool_expiration_time';

implementation

{ TConfiguration }

constructor TConfiguration.Create;
begin
  inherited;
  FProperties := TStringList.Create;
end;

destructor TConfiguration.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
end;

function TConfiguration.GetProperties: TStrings;
begin
  Result := FProperties;
end;

function TConfiguration.GetPropertyItem(const pName: string): string;
begin
  Result := FProperties.Values[pName]
end;

function TConfiguration.GetValue(const pName: string;
  const pDefaultValue: Integer): Integer;
begin
  if FProperties.IndexOfName(pName) <> -1 then
    Result := StrToIntDef(PropertyItem[pName], pDefaultValue)
  else
    Result := pDefaultValue;
end;

function TConfiguration.GetValue(const pName: string;
  const pDefaultValue: Double): Double;
begin
  if FProperties.IndexOfName(pName) <> -1 then
    Result := StrToFloatDef(PropertyItem[pName], pDefaultValue)
  else
    Result := pDefaultValue;
end;

function TConfiguration.GetValue(const pName,
  pDefaultValue: string): string;
begin
  if FProperties.IndexOfName(pName) <> -1 then
    Result := PropertyItem[pName]
  else
    Result := pDefaultValue;
end;

procedure TConfiguration.SetPropertyItem(const pName, Value: string);
begin
  FProperties.Values[pName] := Value;
end;

// Onde deve ficar isto?
const
  GlobalMaxConnections = 30;

{ ******************************************************************************

                          TInfraConnectionProvider

*******************************************************************************}

{**
  Cria uma nova instância de TInfraConnectionProvider.
  @param MaxSize Tamanho máximo do Pool de conexões
  @param ADriverManager Um objeto do tipo IZDriverManager que criará as conexões
  @param AConfiguration Um objeto do tipo IConfiguration que contém todas as
    informações para criar uma nova conexão
}
constructor TConnectionProvider.Create(pDriverManager: IZDriverManager; pConfiguration: IConfiguration);
var
  iMax: Integer;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FDriverManager := pDriverManager;
  FConfiguration := pConfiguration;

  iMax := FConfiguration.GetValue(MAX_CONNECTIONS_KEY, GlobalMaxConnections);

  SetLength(FPool, iMax);
end;

destructor TConnectionProvider.Destroy;
begin
  CloseConnections;
  SetLength(FPool, 0);
  FCriticalSection.Free;
  inherited;
end;

procedure TConnectionProvider.CloseConnections;
var
  i: Integer;
begin
  for i := Low(FPool) to High(FPool) do
    if Assigned(FPool[i]) then
      FPool[i].Close;
end;

procedure TConnectionProvider.Close;
var
  i: Integer;
begin
  for i := Low(FPool) to High(FPool) do
    if Assigned(FPool[i]) then
      ReleaseConnection(FPool[i]);
end;

{**
  Localiza um objeto no Pool. Se este não for encontrado retorna nil
  @param pConnection Objeto a ser localizado
  @return Retorna o objeto encontrado ou nil caso não seja localizado
}
function TConnectionProvider.FindConnection(const pConnection: IZConnection): IZConnection;
var
  i: Integer;
begin
  Result := nil;
  for i := Low(FPool) to High(FPool) do
    if FPool[i] = pConnection then
    begin
      Result := FPool[i];
      Break;
    end;
end;

{**
  Libera uma conexão de volta ao pool para ser reutilizada
  @param pConnection Conexão a ser liberada
}
procedure TConnectionProvider.ReleaseConnection(const pConnection: IZConnection);
begin
  if FindConnection(pConnection) = nil then
    raise EInfraConnectionProviderError.Create('Conexão não encontrada no Pool deste Provider');

  if pConnection.IsClosed then
    raise EInfraConnectionProviderError.Create('Conexão já fechada');

  // Ao fechar a conexao, ela, automaticamente, fica disponível no pool
  pConnection.Close;

  // TODO: Criar Thread para verificar o tempo de expiração do objeto
  // ...
end;

{**
  Procura no Pool por uma conexão disponível (ou seja, uma conexao fechada).
  E, caso a encontre, retorna-a.
  @return Retorna um objeto do tipo IZConnection
}
function TConnectionProvider.GetFreeConnection: IZConnection;
var
  i: Integer;
begin
  Result := nil;
  for i := Low(FPool) to High(FPool) do
    if Assigned(FPool[i]) and FPool[i].IsClosed then
    begin
      Result := FPool[i];
      Break;
    end;
end;

function TConnectionProvider.BuildConnectionString(pConfiguration: IConfiguration): string;
begin
  Result := 'zdbc:' + pConfiguration.PropertyItem['protocol'] +
    '://' + pConfiguration.PropertyItem['hostname'] +
    '/' + pConfiguration.PropertyItem['database'] +
    '?username=' + pConfiguration.PropertyItem['username'] +
    ';password=' + pConfiguration.PropertyItem['password'];
end;

{**
  Cria uma nova conexao, caso haja algum slot vazio.
  Caso contrário, levanta uma exceção EInfraConnectionProviderError
  @return Retorna um objeto do tipo IZConnection
}
function TConnectionProvider.CreateConnection: IZConnection;
var
  i: Integer;
begin
  for i := Low(FPool) to High(FPool) do
    if not Assigned(FPool[i]) then
    begin
      FPool[i] := FDriverManager.GetConnection(BuildConnectionString(FConfiguration));
      Result := FPool[i];
      Exit;
    end;

  raise EInfraConnectionProviderError.Create('Número máximo de conexões excedido');
end;

{**
  Procura no Pool por uma conexão disponível e, caso a encontre, retorna-a.
  Caso contrário, tenta criar uma nova conexão. Se isto não for possível,
  levanta uma exceção EInfraConnectionProviderError
  @return Retorna um objeto do tipo IZConnection
}
function TConnectionProvider.GetConnection: IZConnection;
begin
  FCriticalSection.Acquire;
  try
    Result := GetFreeConnection;
    if not Assigned(Result) then
      Result := CreateConnection;
  finally
    FCriticalSection.Release;
  end;
end;

end.

