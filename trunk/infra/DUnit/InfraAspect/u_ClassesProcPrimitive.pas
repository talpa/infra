// FixUses
unit u_ClassesProcPrimitive;

interface

uses
  InfraInterfaces, SimpleTypeImpl;

type
  IClassC = interface(IInfraObject)
    ['{8CFB1D14-046B-47EC-BB1C-25709705F2F2}']
    procedure NotIntercepted;
    procedure ProcSemPar;
    procedure Proc1Par(const x: Integer);
    procedure Proc2Par(const x, y: Integer);
    procedure Proc3Par(const x, y, z: Integer);
    procedure Proc4Par(const x, y, z, w: Integer);
    procedure Proc5Par(const x, y, z, w, k: Integer);
  end;

  TClassC = class(TInfraObject, IClassC)
  public
    procedure NotIntercepted; virtual;
    procedure ProcSemPar; virtual;
    procedure Proc1Par(const x: Integer); virtual;
    procedure Proc2Par(const x, y: Integer); virtual;
    procedure Proc3Par(const x, y, z: Integer); virtual;
    procedure Proc4Par(const x, y, z, w: Integer); virtual;
    procedure Proc5Par(const x, y, z, w, k: Integer); virtual;
  protected
    procedure InitTypeInfo; override;
  end;

implementation

uses
  SysUtils, u_GlobalTestList;

{ TClassC }

procedure TClassC.InitTypeInfo;
begin
  inherited;
  FTypeInfo := TypeRegisterService.GetTypeInfo(IClassC);
end;

procedure TClassC.ProcSemPar;
begin
  GlobalTestList.Add(Format(cMsgProcedureCalled,
    [Self.TypeInfo.FullName, 'ProcSemPar', EmptyStr]));
end;

procedure TClassC.Proc1Par(const x: Integer);
begin
  GlobalTestList.Add(Format(cMsgProcedureCalled,
    [Self.TypeInfo.FullName, 'Proc1Par',
    IntToStr(x)]));
end;

procedure TClassC.Proc2Par(const x, y: Integer);
begin
  GlobalTestList.Add(Format(cMsgProcedureCalled,
    [Self.TypeInfo.FullName, 'Proc2Par',
    IntToStr(x)+', '+IntToStr(y)]));
end;

procedure TClassC.NotIntercepted;
begin
  GlobalTestList.Add(Format(cMsgProcedureCalled,
    [Self.TypeInfo.FullName, 'NotIntercepted', EmptyStr]));
end;

procedure TClassC.Proc3Par(const x, y, z: Integer);
begin
  GlobalTestList.Add(Format(cMsgProcedureCalled,
    [Self.TypeInfo.FullName, 'Proc3Par',
    IntToStr(x)+', '+IntToStr(y)+', '+
    IntToStr(z)]));
end;

procedure TClassC.Proc4Par(const x, y, z, w: Integer);
begin
  GlobalTestList.Add(Format(cMsgProcedureCalled,
    [Self.TypeInfo.FullName, 'Proc4Par',
    IntToStr(x)+', '+IntToStr(y)+', '+
    IntToStr(z)+', '+IntToStr(w)]));
end;

procedure TClassC.Proc5Par(const x, y, z, w, k: Integer);
begin
  GlobalTestList.Add(Format(cMsgProcedureCalled,
    [Self.TypeInfo.FullName, 'Proc5Par',
    IntToStr(x)+', '+IntToStr(y)+', '+
    IntToStr(z)+', '+IntToStr(w)+', '+
    IntToStr(k)]));
end;

procedure RegisterClassesOnReflection;
var
  InfraObjectInfo: IInfraTypeInfo;
begin
  with TypeRegisterService do
  begin
    InfraObjectInfo := GetTypeInfo(IInfraObject);

    with RegisterNewType(IClassC, 'TClassC', TClassC,
      IInfraObject, InfraObjectInfo) do
    begin
      AddMember('ProcSemPar', TInfraMethod, IInfraPrimitiveMethod,
        @TClassC.ProcSemPar, mrkNone);
      AddMember('Proc1Par', TInfraMethod, IInfraPrimitiveMethod,
        @TClassC.Proc1Par, mrkNone);
      AddMember('Proc2Par', TInfraMethod, IInfraPrimitiveMethod,
        @TClassC.Proc2Par, mrkNone);
      AddMember('Proc3Par', TInfraMethod, IInfraPrimitiveMethod,
        @TClassC.Proc3Par, mrkNone);
      AddMember('Proc4Par', TInfraMethod, IInfraPrimitiveMethod,
        @TClassC.Proc4Par, mrkNone);
      AddMember('Proc5Par', TInfraMethod, IInfraPrimitiveMethod,
        @TClassC.Proc5Par, mrkNone);
    end;
  end;
end;

initialization
  RegisterClassesOnReflection;

end.
