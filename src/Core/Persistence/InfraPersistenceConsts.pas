// xxx
unit InfraPersistenceConsts;

interface

const
  // Constantes relacionadas ao pool de conexões
  cCONFIGKEY_MAXCONNECTIONS = 'Pool.MaxConnections';
  cCONFIGKEY_CONNECTIONTIME = 'Pool.TimeExpirationConnection';
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

resourcestring
  // Erros da persistência
  cErrorConfigurationNotDefined = 'Configuration nao foi alimentado';
  cErrorConnectionNotFoundOnPool = 'Conexão não encontrada no Pool deste Provider';
  cErrorConnectionsLimitExceeded = 'Número máximo de conexões excedido';
  cErrorAlreadyClosedConnection = 'Conexão já fechada';
  cErrorTemplateTryCreateClassBase = 'Classe base TemplateReader não deve ser instanciada';
  cErrorTemplateFileNotFound = 'Template %s não vazio ou não encontrado';
  cErrorTemplateTypeInvalid = 'Classe de leitura de templates inválida ou não definida';
  cErrorPersistenceEngineObjectIDUndefined = 'Tipo de objeto não definido no SQLCommand';
  cErrorPersistenceEngineParamNotFound = 'Parâmetro %s não encontrado';
  cErrorPersistenceEngineAttributeNotFound = 'Atributo não encontrado para o alias %s (coluna: %s)';
  cErrorPersistenceEngineCannotMapAttribute = 'Não foi possivel mapear valor para o atributo %s';
  // cErrorTemplatePathNotDefined = 'Caminho dos templates não definido';

implementation

end.