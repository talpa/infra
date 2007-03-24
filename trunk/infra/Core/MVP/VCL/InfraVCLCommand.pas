unit InfraVCLCommand;

interface

uses
  InfraMVPVCLIntf,
  InfraValueTypeIntf,
  InfraCommand;

type
  TCloseFormCommand = class(TCommand, ICloseFormCommand)
  protected
    procedure Execute(const Parameters: IInfraList); override;
  end;

  TCloseFormWithOKCommand = class(TCloseFormCommand, ICloseFormWithOKCommand);

procedure RegisterOnReflection;

implementation

uses
  InfraCommonIntf,
  InfraMVPIntf;

procedure RegisterOnReflection;
begin
  with TypeService do
  begin
    RegisterNewType(ICloseFormCommand, 'CloseForm',
      TCloseFormCommand, ICommand, GetTypeInfo(ICommand));

    RegisterNewType(ICloseFormWithOKCommand, 'CloseFormWithOK',
      TCloseFormWithOKCommand, ICommand, GetTypeInfo(ICommand));
  end;
end;

{ TCloseFormCommand }

procedure TCloseFormCommand.Execute(const Parameters: IInfraList);
begin
  inherited Execute(Parameters);
  ((Owner as IPresenter).View as IVCLCustomFormView).Close;
end;

end.
