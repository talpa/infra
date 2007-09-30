// FixUses
unit u_ClassesProcType;

interface

uses
  InfraInterfaces, SimpleTypeImpl;

type
  IClassA = interface(IInfraObject)
    ['{6DF4A628-EC6B-4237-9D6E-CE3EAB85836B}']
    procedure NotIntercepted;
    procedure ProcSemPar;
    procedure Proc1Par(const x: IInfraInteger);
    procedure Proc2Par(const x, y: IInfraInteger);
    procedure Proc3Par(const x, y, z: IInfraInteger);
    procedure Proc4Par(const x, y, z, w: IInfraInteger);
    procedure Proc5Par(const x, y, z, w, k: IInfraInteger);
  end;

  TClassA = class(TInfraObject, IClassA)
  public
    procedure NotIntercepted; virtual;
    procedure ProcSemPar; virtual;
    procedure Proc1Par(const x: IInfraInteger); virtual;
    procedure Proc2Par(const x, y: IInfraInteger); virtual;
    procedure Proc3Par(const x, y, z: IInfraInteger); virtual;
    procedure Proc4Par(const x, y, z, w: IInfraInteger); virtual;
    procedure Proc5Par(const x, y, z, w, k: IInfraInteger); virtual;
  protected
    procedure InitTypeInfo; override;
  end;

implementation

uses
  SysUtils, u_GlobalTestList;

{ TClassA }

procedure TClassA.InitTypeInfo;
begin
  inherited;
  FTypeInfo := TypeRegisterService.GetTypeInfo(IClassA);
end;

procedure TClassA.ProcSemPar;
begin
  GlobalTestList.Add(Format(cMsgProcedureCalled,
    [Self.TypeInfo.FullName, 'ProcSemPar', EmptyStr]));
end;

procedure TClassA.Proc1Par(const x: IInfraInteger);
begin
  GlobalTestList.Add(Format(cMsgProcedureCalled,
    [Self.TypeInfo.FullName, 'Proc1Par',
    IntToStr(x.AsInteger)]));
end;

procedure TClassA.Proc2Par(const x, y: IInfraInteger);
begin
  GlobalTestList.Add(Format(cMsgProcedureCalled,
    [Self.TypeInfo.FullName, 'Proc2Par',
    IntToStr(x.AsInteger)+', '+IntToStr(y.AsInteger)]));
end;

procedure TClassA.NotIntercepted;
begin
  GlobalTestList.Add(Format(cMsgProcedureCalled,
    [Self.TypeInfo.FullName, 'NotIntercepted', EmptyStr]));
end;

procedure TClassA.Proc3Par(const x, y, z: IInfraInteger);
begin
  GlobalTestList.Add(Format(cMsgProcedureCalled,
    [Self.TypeInfo.FullName, 'Proc3Par',
    IntToStr(x.AsInteger)+', '+IntToStr(y.AsInteger)+', '+
    IntToStr(z.AsInteger)]));
end;

procedure TClassA.Proc4Par(const x, y, z, w: IInfraInteger);
begin
  GlobalTestList.Add(Format(cMsgProcedureCalled,
    [Self.TypeInfo.FullName, 'Proc4Par',
    IntToStr(x.AsInteger)+', '+IntToStr(y.AsInteger)+', '+
    IntToStr(z.AsInteger)+', '+IntToStr(w.AsInteger)]));
end;

procedure TClassA.Proc5Par(const x, y, z, w, k: IInfraInteger);
begin
  GlobalTestList.Add(Format(cMsgProcedureCalled,
    [Self.TypeInfo.FullName, 'Proc5Par',
    IntToStr(x.AsInteger)+', '+IntToStr(y.AsInteger)+', '+
    IntToStr(z.AsInteger)+', '+IntToStr(w.AsInteger)+', '+
    IntToStr(k.AsInteger)]));
end;

procedure RegisterClassesOnReflection;
var
  InfraObjectInfo: IInfraTypeInfo;
begin
  with TypeRegisterService do
  begin
    InfraObjectInfo := GetTypeInfo(IInfraObject);

    with RegisterNewType(IClassA, 'TClassA', TClassA,
      IInfraObject, InfraObjectInfo) do
    begin
      AddMember('ProcSemPar', TInfraMethod, IInfraMethod,
        @TClassA.ProcSemPar, mrkNone);
      with AddMember('Proc1Par', TInfraMethod, IInfraMethod,
        @TClassA.Proc1Par, mrkNone) do
      begin
         AddMember('x', TInfraInteger, IInfraInteger, IInfraType);
      end;
      with AddMember('Proc2Par', TInfraMethod, IInfraMethod,
        @TClassA.Proc2Par, mrkNone) do
      begin
         AddMember('x', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('y', TInfraInteger, IInfraInteger, IInfraType);
      end;
      with AddMember('Proc3Par', TInfraMethod, IInfraMethod,
        @TClassA.Proc3Par, mrkNone) do
      begin
         AddMember('x', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('y', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('z', TInfraInteger, IInfraInteger, IInfraType);
      end;
      with AddMember('Proc4Par', TInfraMethod, IInfraMethod,
        @TClassA.Proc4Par, mrkNone) do
      begin
         AddMember('x', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('y', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('z', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('w', TInfraInteger, IInfraInteger, IInfraType);
      end;
      with AddMember('Proc5Par', TInfraMethod, IInfraMethod,
        @TClassA.Proc5Par, mrkNone) do
      begin
         AddMember('x', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('y', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('z', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('w', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('k', TInfraInteger, IInfraInteger, IInfraType);
      end;
    end;
  end;
end;

initialization
  RegisterClassesOnReflection;

end.

