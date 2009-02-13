unit InfraOPFConnectionProvider;

interface

uses
  SysUtils,
  Windows,
  Classes,
  {Infra}
  InfraCommon,
  InfraOPFIntf,
  {Zeos}
  ZDbcIntfs,
  SyncObjs;

type
  TCleanupThread = class;

  // classe que gerencia o pool de conexões
  TConnectionProvider = class(TBaseElement, IConnectionProvider)
  private
    FItems: TArrayConnectionItem;
    FPoolSize: Integer;
    FTimeout: Int64;
    FCleanupThread: TCleanupThread;
    FConnectionString: string;
    // Semaforo usado para limitar o numero de conexões simultâneas.
    // Quando a enesima+1 conexão é solicitada, ela irá ser bloqueada até
    // que uma conxão esteja disponível.
    FSemaphore: THandle;
    // Sessão critica para sinconizar o acesso ao Pool.
    FCriticalSection: TCriticalSection;
    FInitialized: Boolean;
    FInternallEvent: TEvent;
    function BuildConnectionString(const pConfiguration: IConfiguration): string;
    function GetActiveConnections: Integer;
    function GetPoolSize: Integer;
  protected
    //This function returns an object
    //that implements the IPoolConnection interface.
    //This object can be a data module, as was
    //done in this example.
    function Acquire: IConnectionItem;
    procedure Lock;
    procedure UnLock;
    function GetInternallEvent: TEvent;
    function GetItems: TArrayConnectionItem;
    property ActiveConnections: Integer read GetActiveConnections;
    property PoolSize: Integer read GetPoolSize;
  public
    // Este construtor pega 3 parametros.
    // 1 - Tamanho do pool (limite máximo de conexões)
    // 2 - Por quanto tempo uma conexão livre deve existir no pool
    // 3 - Quanto tempo alguem pode esperar por uma conexão
    constructor Create(const pConfiguration: IConfiguration); reintroduce;
    destructor Destroy; override;
  end;

  // Esta thread é usada pelo pool de conexões para limpar conexões
  // que estão expirando um periodo de tempo sem uso.
  TCleanupThread = class(TThread)
  private
    FCleanupDelay: Integer;
    FConnectionPool: IConnectionProvider;
  protected
    // Quando a thread é criada, o campo secao critica irá ser definido para
    // a secao critica do pool. Esta secao critica é usada para sincronizar
    // acesso a contagem de referencia do conection.
    procedure Execute; override;
    constructor Create(CreateSuspended: Boolean;
      const CleanupDelayMinutes: Integer);
    property ConnectionPool: IConnectionProvider read FConnectionPool
      write FConnectionPool;
  end;

  /// Classe que implementa a interface IInfraDBConnection.
  TConnectionProviderItem = class(TInterfacedObject, IInterface, IConnectionItem)
  private
    { Private declarations }
    FConnection: IZConnection;
    FRefCount: Integer;
    FLastAccess: TDateTime;
    /// Quando esta classe é criada a seção critica do pool é repassada para cá
    FCriticalSection: TCriticalSection;
    /// Este semaforo aponta para o semáforo do pool. Ele irá ser usado para chamar
    /// ReleaseSemaphore do método _Release desta classe.
    FSemaphore: THandle;
    function ConvertIsolationLevel(
      pIsolationLevel: TIsolationLevel): TZTransactIsolationLevel;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IInfraDBConnection }
    function GetLastAccess: TDateTime;
    function GetRefCount: Integer;
    function Connection: IZConnection;
    procedure Configure(pIsolationLevel: TIsolationLevel);
  public
    { Public declarations }
    constructor Create(const pConnectionString: string;
      pCriticalSection: TCriticalSection; pSemaphore: THandle);
    destructor Destroy; override;
  end;

implementation

uses
  InfraCommonIntf,
  InfraOPFConsts,
  DateUtils,
  TypInfo;

{ TDBXConnectionPool }

constructor TConnectionProvider.Create(const pConfiguration: IConfiguration);
var
  vCleanupDelayMinutes: Integer;
begin
  FInitialized := False;
  if not Assigned(pConfiguration) then
    raise EInfraArgumentError.Create('pConfiguration');

  FConnectionString := BuildConnectionString(pConfiguration);
  FPoolSize := pConfiguration.GetValue(cCONFIGKEY_POOLSIZE, cDefaultPoolSize);
  FTimeout := pConfiguration.GetValue(cCONFIGKEY_CONNECTTIMEOUT, cDefaultGetConnTimeoutMS);
  vCleanupDelayMinutes := pConfiguration.GetValue(cCONFIGKEY_CLEANUPDELAYMINUTES, cDefaultCleanupConnMIN);

  FSemaphore := CreateSemaphore(nil, FPoolSize, FPoolSize, '');
  FCriticalSection := TCriticalSection.Create;
  FInternallEvent := TEvent.Create(nil, False, False, '');
  
  // Define o tamanho do pool
  SetLength(FItems, FPoolSize);
  // Cria e inicia a thread de limpeza
  FCleanupThread := TCleanupThread.Create(True, vCleanupDelayMinutes);
  with FCleanupThread do
  begin
    FreeOnTerminate := True;
    Priority := tpLower;
    FCleanupThread.ConnectionPool := Self;
    Resume;
  end;
  FInitialized := True;
end;

destructor TConnectionProvider.Destroy;
var
  i: Integer;
begin
  if Assigned(FCleanupThread) then
  begin
    // Termina a thread de limpeza
    FCleanupThread.Terminate;
    // Se a thread de limpeza esta esperando o timeout, o evento abaixo irá forçar
    FInternallEvent.SetEvent;

    // Aguarda a conclusão da Thread
    WaitForSingleObject(FCleanupThread.Handle, 5000);
  end;

  if FInitialized then
  begin
    // Libera todas as connections modules restantes.
    Lock;
    try
      for i := Low(FItems) to High(FItems) do
        FItems[i] := nil;
      SetLength(FItems, 0);
    finally
      Unlock;
    end;
    // Libera a seção crítica e o Semáforo
    FInternallEvent.Free;
    FCriticalSection.Free;
    CloseHandle(FSemaphore);
  end;
  inherited;
end;

{*
  Constrói a URL de conexão com o banco
  @param pConfiguration Objeto com as configurações de conexão com o banco de dados
  @return Retorna uma string no formato
    zdbc:<driver>://<hostname>/<databasename>?username=<username>;password=<password>
*}
function TConnectionProvider.BuildConnectionString(const pConfiguration:
    IConfiguration): string;
begin
  Result := 'zdbc:' + pConfiguration.GetAsString(cCONFIGKEY_DRIVER) +
    '://' + pConfiguration.GetAsString(cCONFIGKEY_HOSTNAME) +
    '/' + pConfiguration.GetAsString(cCONFIGKEY_DATABASENAME) +
    '?username=' + pConfiguration.GetAsString(cCONFIGKEY_USERNAME) +
    ';password=' + pConfiguration.GetAsString(cCONFIGKEY_PASSWORD);
end;

procedure TConnectionProvider.Lock;
begin
  FCriticalSection.Enter;
end;

procedure TConnectionProvider.UnLock;
begin
  FCriticalSection.Leave;
end;

function TConnectionProvider.Acquire: IConnectionItem;
var
  vI: Integer;
  vConnection: IConnectionItem;
  vWaitResult: Integer;
begin
  Result := nil;
  vWaitResult := WaitForSingleObject(FSemaphore, FTimeout);
  if vWaitResult <> WAIT_OBJECT_0 then
    raise EPersistenceConnectionProviderError.Create('Connection pool timeout. ' +
      'Cannot obtain a connection');
  Lock;
  try
    for vI := Low(FItems) to High(FItems) do
    begin
      // Se o FItems[i] = nil, o IPoolConnection ainda nao foi criado. Então
      // cria-se, inicializa e o retorna. Se FItems[i] <> nil, então
      // verifica se seu RefCount é 1 (somente o pool está fazendo referencia a
      // ele).
      if FItems[vI] = nil then
      begin
        vConnection := TConnectionProviderItem.Create(FConnectionString,
          Self.FCriticalSection,
          Self.FSemaphore);
        vConnection.Connection.Open;
        FItems[vI] := vConnection;
        Result := FItems[vI];
        Break;
      end
      else
      // se FItems[i].RefCount = 1 então a conexão está disponível para
      // retorná-la.
      if FItems[vI].RefCount = 1 then
      begin
        Result := FItems[vI];
        Break;
      end;
    end; //for
  finally
    UnLock;
  end;
end;

function TConnectionProvider.GetActiveConnections: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Low(FItems) to High(FItems) do
    if Assigned(FItems[i]) then
      Inc(Result);
end;

function TConnectionProvider.GetPoolSize: Integer;
begin
  Result := Length(FItems);
end;

function TConnectionProvider.GetInternallEvent: TEvent;
begin
  Result := FInternallEvent;
end;

function TConnectionProvider.GetItems: TArrayConnectionItem;
begin
  Result := FItems;
end;

{ TCleanupThread }

constructor TCleanupThread.Create(CreateSuspended: Boolean;
  const CleanupDelayMinutes: Integer);
begin
  // sempre cria suspensa
  inherited Create(True);
  FCleanupDelay := CleanupDelayMinutes;
  if not CreateSuspended then
    Resume;
end;

procedure TCleanupThread.Execute;
var
  i: Integer;
  WaitMinutes: Integer;
  vActiveConnection: Boolean;
  vNoMoreReferences: Boolean;
  vElapsedTime: Integer;
begin
  WaitMinutes := FCleanupDelay * 60 * 1000;
  while not Terminated do
  begin
    // espera pelo periodo definido em FCleanupDelay
    // InternalEvent foi assinalado, está em erro, ou abandonado,
    // são situações nas quais a thread deveria terminar.
    if FConnectionPool.GetInternallEvent.WaitFor(WaitMinutes) <> wrTimeout then
      Exit;

    if Terminated then
      Exit;

    // WaitForSingleObject tem expirado. Procura limpar as conecções.
    with FConnectionPool do
    begin
      Lock;
      try
        for i := Low(GetItems) to High(GetItems) do
        begin
          // Libera a conexão se ela existir, nao tem referencias externas e nao
          // foi recentemente usada.
          vActiveConnection := GetItems[i] <> nil;
          if vActiveConnection then
          begin
            vNoMoreReferences := GetItems[i].RefCount = 1;
            vElapsedTime := MinutesBetween(GetItems[i].LastAccess, Now);

            if (vNoMoreReferences) and (vElapsedTime >= FCleanupDelay) then
              // Fecha a conexao
              GetItems[i] := nil;
          end;
        end;
      finally
        Unlock;
      end; // try
    end; // with
  end; // while
end;

{ TConnectionModule }

function TConnectionProviderItem._AddRef: Integer;
begin
  // increment a contagem de referencias
  FCriticalSection.Enter;
  try
    Inc(FRefCount);
    Result := FRefCount;
  finally
    FCriticalSection.Leave;
  end;
end;

function TConnectionProviderItem._Release: Integer;
var
  tmpCriticalSection: TCriticalSection;
  tmpSemaphore: THandle;
begin
  // Guarda referencias locais a sessão critica e ao semaforo
  // Isto é necessário por que estes membros seriam inválidados durante
  // a destruição do ConnectionModule, e só podemos liberar o semáforo após o
  // Destroy que vai acontecer dentro do inherited _Release.
  tmpCriticalSection := FCriticalSection;
  tmpSemaphore := FSemaphore;
  Result := FRefCount;
  // dencrementa a contagem de referencias
  FCriticalSection.Enter;
  try
    Dec(FRefCount);
    Result := FRefCount;
    // se não há mais referencias, chama Destroy
    if Result = 0 then
      Destroy
    else
      Self.FLastAccess := Now;
  finally
    tmpCriticalSection.Leave;
    if Result = 1 then
      ReleaseSemaphore(tmpSemaphore, 1, nil);
  end;
end;

function TConnectionProviderItem.Connection: IZConnection;
begin
  Result := FConnection;
end;

function TConnectionProviderItem.GetRefCount: Integer;
begin
  FCriticalSection.Enter;
  Result := FRefCount;
  FCriticalSection.Leave;
end;

function TConnectionProviderItem.GetLastAccess: TDateTime;
begin
  FCriticalSection.Enter;
  Result := FLastAccess;
  FCriticalSection.Leave;
end;

constructor TConnectionProviderItem.Create(const pConnectionString: string;
  pCriticalSection: TCriticalSection; pSemaphore: THandle);
begin
  inherited Create;
  FCriticalSection := pCriticalSection;
  FSemaphore := pSemaphore;
  FConnection := DriverManager.GetConnection(pConnectionString);
end;

destructor TConnectionProviderItem.Destroy;
begin
  FConnection := nil;
  inherited;
end;

procedure TConnectionProviderItem.Configure(
  pIsolationLevel: TIsolationLevel);
begin
  with FConnection do
  begin
    SetTransactionIsolation(ConvertIsolationLevel(pIsolationLevel));
    SetAutoCommit(pIsolationLevel = tilNone);
  end;
end;

{**
  Converte TIsolationLevel para TZTransactIsolationLevel.
  Efetua a conversão de maneira segura
  @param pTransIsolationLevel Nível de isolamento do tipo TIsolationLevel
  @return Retorna o nível de isolamento da transação já convertido
}
function TConnectionProviderItem.ConvertIsolationLevel(
  pIsolationLevel: TIsolationLevel): TZTransactIsolationLevel;
begin
  case pIsolationLevel of
    tilNone: Result := tiNone;
    tilReadUncommitted: Result := tiReadUnCommitted;
    tilReadCommitted: Result := tiReadCommitted;
    tilRepeatableRead: Result := tiRepeatableRead;
    tilSerializable: Result := tiSerializable;
  else
    raise EPersistenceConnectionProviderError.CreateFmt(
      cErrorTranIsolLevelUnknown,
      [GetEnumName(TypeInfo(TIsolationLevel), Ord(pIsolationLevel))]);
  end;
end;

end.


