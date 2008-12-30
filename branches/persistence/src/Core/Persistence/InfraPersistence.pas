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
    property Properties: TStrings read GetProperties;
    property PropertyItem[const pName: string]: string read GetPropertyItem write SetPropertyItem;
  end;

  /// Classe responsável por prover conexões com o SGDB 
  TInfraConnectionProvider = class(TBaseElement, IConnectionProvider)
  private
    FConfiguration: IConfiguration;
    FDriverManager: IZDriverManager;    // O DriverManager que será usado pra criar as conexões
    FPool: array of IZConnection;       // O pool
    FCriticalSection: TCriticalSection;
    function BuildConnectionString(pConfiguration: IConfiguration): string; // CriticalSection usado para evitar conflitos em aplicações multi-thread
  protected
    function GetFreeConnection: IZConnection; // Procura por uma conexao livre
    function CreateConnection: IZConnection;  // Cria uma nova conexao
    function FindConnection(const pConnection: IZConnection): IZConnection; // Procura por uma conexao no pool
  public
    constructor Create(MaxSize, ExpirationTime: Integer;
      ADriverManager: IZDriverManager; AConfiguration: IConfiguration); reintroduce;
    destructor Destroy; override;
    function GetConnection: IZConnection; // Caso tenha conexoes disponíveis no Pool bloqueia uma e retorna-a
    procedure Close; // Fecha todas as conexões do pool
    procedure ReleaseConnection(const pConnection: IZConnection); // Devolve a conexao ao Pool
  end;

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

procedure TConfiguration.SetPropertyItem(const pName, Value: string);
begin
  FProperties.Values[pName] := Value;
end;

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
constructor TInfraConnectionProvider.Create(MaxSize, ExpirationTime: Integer;
  ADriverManager: IZDriverManager; AConfiguration: IConfiguration);
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FDriverManager := ADriverManager;
  FConfiguration := AConfiguration;
  SetLength(FPool, MaxSize);
end;

destructor TInfraConnectionProvider.Destroy;
begin
  SetLength(FPool, 0);
  FConfiguration := nil;
  FCriticalSection.Free;
  inherited;
end;

procedure TInfraConnectionProvider.Close;
var
  i: Integer;
begin
  for i := Low(FPool) to High(FPool) do
    ReleaseConnection(FPool[i]);
end;

{**
  Localiza um objeto no Pool. Se este não for encontrado retorna nil
  @param pConnection Objeto a ser localizado
  @return Retorna o objeto encontrado ou nil caso não seja localizado
}
function TInfraConnectionProvider.FindConnection(const pConnection: IZConnection): IZConnection;
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
  Libera uma conexão para ser reutilizada
  @param pConnection Conexão a ser liberada
}
procedure TInfraConnectionProvider.ReleaseConnection(const pConnection: IZConnection);
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
function TInfraConnectionProvider.GetFreeConnection: IZConnection;
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

function TInfraConnectionProvider.BuildConnectionString(pConfiguration: IConfiguration): string;
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
function TInfraConnectionProvider.CreateConnection: IZConnection;
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
function TInfraConnectionProvider.GetConnection: IZConnection;
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

