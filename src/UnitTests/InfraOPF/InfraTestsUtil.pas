unit InfraTestsUtil;

interface

uses
  InfraPersistenceIntf;

type
  TTestsUtil = class
  public
    class function GetNewConfiguration: IConfiguration;
  end;

implementation

uses
  InfraPersistence, InfraPersistenceConsts;

class function TTestsUtil.GetNewConfiguration: IConfiguration;
begin
  Result := TConfiguration.Create;
  Result.SetValue(cCONFIGKEY_CONNECTIONTIME, 10);
  Result.SetValue(cCONFIGKEY_MAXCONNECTIONS, 2);
end;

end.
