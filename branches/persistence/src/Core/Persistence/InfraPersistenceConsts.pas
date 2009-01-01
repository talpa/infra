unit InfraPersistenceConsts;

interface

const
  // Chaves do Configuration
  cCONFIGKEY_MAXCONNECTIONS = 'Pool.MaxConnections';
  cCONFIGKEY_CONNECTIONTIME = 'Pool.TimeExpirationConnection';

  // Valores padrões para items do configuration
  cGlobalMaxConnections = 30;

resourcestring
  // Erros da persistência
  cErrorConfigurationNotDefined = 'Configuration nao foi alimentado';
  cErrorConnectionNotFoundOnPool = 'Conexão não encontrada no Pool deste Provider';
  cErrorAlreadyClosedConnection = 'Conexão já fechada';

// Constantes relacionadas com a conexao com o banco de dados
const
  cCONFIGKEY_DRIVER = 'Connection.Driver';
  cCONFIGKEY_HOSTNAME = 'Connection.HostName';
  cCONFIGKEY_PASSWORD = 'Connection.Password';
  cCONFIGKEY_USERNAME = 'Connection.UserName';
  cCONFIGKEY_DATABASENAME = 'Connection.DatabaseName';

implementation

end.
