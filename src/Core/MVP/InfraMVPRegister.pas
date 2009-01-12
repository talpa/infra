unit InfraMVPRegister;

interface

procedure RegisterOnReflection;

implementation

uses
  InfraSelection,
  InfraCommand,
  InfraVCLCommand;

procedure RegisterOnReflection;
begin
  InfraSelection.RegisterOnReflection;
  InfraCommand.RegisterOnReflection;
  InfraVCLCommand.RegisterOnReflection;
end;

end.
