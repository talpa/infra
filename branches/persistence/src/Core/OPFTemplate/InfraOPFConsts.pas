unit InfraOPFConsts;

interface

const
  // TODO: Documentar todas as propriedades listando os possíveis valores
  
  // Constantes relacionadas ao pool de conexões
  cCONFIGKEY_POOLSIZE = 'Pool.Size';
  cCONFIGKEY_CONNECTTIMEOUT = 'Pool.ConnectTimeOut';
  cCONFIGKEY_CLEANUPDELAYMINUTES = 'Pool.CleanupDelayMinutes';
  // Constantes relacionadas com a conexao com o banco de dados
  cCONFIGKEY_DRIVER = 'Connection.Driver';
  cCONFIGKEY_HOSTNAME = 'Connection.HostName';
  cCONFIGKEY_PASSWORD = 'Connection.Password';
  cCONFIGKEY_USERNAME = 'Connection.UserName';
  cCONFIGKEY_DATABASENAME = 'Connection.DatabaseName';
  // Constantes relacionadas aos templates SQL
  cCONFIGKEY_TEMPLATETYPE = 'Template.ClassType';
  cCONFIGKEY_TEMPLATEPATH = 'Template.Path';
  cCONFIGKEY_TEMPLATEEXT = 'Template.Ext';

  // Valores padrões para items do configuration
  cGlobalMaxConnections = 30;

  // Tamanho padrão do Pool
  cDefaultPoolSize = 10;
  // Por default, 5 minutos é o tempo q ele vai esperar até liberar uma conexão inativa
  cDefaultCleanupConnMIN = 5;
  // 10s é o tempo que ele vai esperar que uma conexao seja liberada quando
  // solicitada uma nova conexao e o pool estiver cheio, antes de dar erro de
  // timeout
  cDefaultGetConnTimeoutMS = 10000;

resourcestring
  // Erros da persistência
  cErrorPersistenceWithoutConfig = 'Configuration não definido no %s';
  cErrorPersistenceWithoutConnProvider  = 'ConnectionProvider não definido no %s';
  cErrorConnectionNotFoundOnPool = 'Conexão não encontrada no Pool deste Provider';
  cErrorConnectionsLimitExceeded = 'Número máximo de conexões excedido';
  cErrorAlreadyClosedConnection = 'Conexão já fechada';
  cErrorTemplateTryCreateClassBase = 'Classe base TemplateReader não deve ser instanciada';
  cErrorTemplateFileNotFound = 'Template %s não vazio ou não encontrado';
  cErrorTemplateTypeInvalid = 'Classe de leitura de templates inválida ou não definida';
  cErrorPersistEngineObjectIDUndefined = 'Tipo de objeto não definido no SQLCommand';
  cErrorPersistEngineWithoutSQLCommand = 'SQLCommand não definido no %s';
  cErrorPersistEngineWithoutSQLCommands = 'SQLCommands não definido no TPersistenceEngine.ExecuteAll';
  cErrorPersistEngineWithoutList = 'List não definido no TPersistenceEngine.Load';
  cErrorPersistEngineParamNotFound = 'Parâmetro %s não encontrado';
  cErrorPersistEngineAttributeNotFound = 'Atributo não encontrado para o alias %s (coluna: %s)';
  cErrorPersistEngineCannotMapAttribute = 'Não foi possivel mapear valor para o atributo %s';
  cErrorParamParserInvalidParam = 'Parâmetro inválido';
  cErrorNotInTransaction = 'Não há nenhuma transação em andamento';
  cErrorAlreadyInTransaction = 'Já existe uma transação em andamento';
  cErrorTranIsolLevelUnknown = 'Nível de isolamento de Transação desconhecido: %s';

implementation

end.

