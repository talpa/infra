unit u_ClassesFuncType;

interface

uses
  SysUtils, Classes,
  Dialogs, InfraInterfaces, ReferenceImpl, AspectImpl, SimpleTypeImpl;

type
  IClassB = interface(IInfraObject)
    ['{35B89064-540B-457A-9C0C-C16F05B0FF6B}']
    function FuncSemPar: IInfraInteger;
    function FuncSemParWithoutProceed: IInfraInteger;
    function FuncSemParWithProceed: IInfraInteger;
    function Func1Par(const x: IInfraInteger): IInfraInteger;
    function Func2Par(const x, y: IInfraInteger): IInfraInteger;
    function Func3Par(const x, y, z: IInfraInteger): IInfraInteger;
    function Func4Par(const x, y, z, w: IInfraInteger): IInfraInteger;
    function Func5Par(const x, y, z, w, k: IInfraInteger): IInfraInteger;
  end;

  TClassB = class(TInfraObject, IClassB)
  private
    function FuncSemPar: IInfraInteger; virtual;
    function FuncSemParWithoutProceed: IInfraInteger; virtual;
    function FuncSemParWithProceed: IInfraInteger; virtual;
    function Func1Par(const x: IInfraInteger): IInfraInteger; virtual;
    function Func2Par(const x, y: IInfraInteger): IInfraInteger; virtual;
    function Func3Par(const x, y, z: IInfraInteger): IInfraInteger; virtual;
    function Func4Par(const x, y, z, w: IInfraInteger): IInfraInteger; virtual;
    function Func5Par(const x, y, z, w, k: IInfraInteger): IInfraInteger; virtual;
  protected
    procedure InitTypeInfo; override;
  end;

implementation

uses u_GlobalTestList;

{ TClassB }

procedure TClassB.InitTypeInfo;
begin
  inherited;
  FTypeInfo := TypeRegisterService.GetTypeInfo(IClassB);
end;

function TClassB.Func1Par(const x: IInfraInteger): IInfraInteger;
begin
  Result := TInfraInteger.NewFrom(x.AsInteger * 125);
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'Func1Par',
    IntToStr(x.AsInteger),
    IntToStr(Result.AsInteger)]));
end;

function TClassB.Func2Par(const x, y: IInfraInteger): IInfraInteger;
begin
  Result := TInfraInteger.NewFrom(x.AsInteger + y.AsInteger);
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'Func2Par',
    IntToStr(x.AsInteger)+', '+IntToStr(y.AsInteger),
    IntToStr(Result.AsInteger)]));
end;

function TClassB.Func3Par(const x, y, z: IInfraInteger): IInfraInteger;
begin
  Result := TInfraInteger.NewFrom(x.AsInteger + y.AsInteger + z.AsInteger);
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'Func3Par',
    IntToStr(x.AsInteger)+', '+IntToStr(y.AsInteger)+', '+
    IntToStr(z.AsInteger),
    IntToStr(Result.AsInteger)]));
end;

function TClassB.Func4Par(const x, y, z, w: IInfraInteger): IInfraInteger;
begin
  Result := TInfraInteger.NewFrom(x.AsInteger * y.AsInteger * z.AsInteger *
    w.AsInteger);
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'Func4Par',
    IntToStr(x.AsInteger)+', '+IntToStr(y.AsInteger)+', '+
    IntToStr(z.AsInteger)+', '+IntToStr(w.AsInteger),
    IntToStr(Result.AsInteger)]));
end;

function TClassB.Func5Par(const x, y, z, w,
  k: IInfraInteger): IInfraInteger;
begin
  Result := TInfraInteger.NewFrom(x.AsInteger * y.AsInteger * z.AsInteger *
    w.AsInteger * k.AsInteger);
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'Func5Par',
    IntToStr(x.AsInteger)+', '+IntToStr(y.AsInteger)+', '+
    IntToStr(z.AsInteger)+', '+IntToStr(w.AsInteger)+', '+
    IntToStr(k.AsInteger),
    IntToStr(Result.AsInteger)]));
end;

function TClassB.FuncSemPar: IInfraInteger;
begin
  Result := TInfraInteger.NewFrom(55);
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'FuncSemPar',
    EmptyStr,
    IntToStr(Result.AsInteger)]));
end;

function TClassB.FuncSemParWithoutProceed: IInfraInteger;
begin
  // This function don't should be called, because the aspect that intercept it
  // don't call proceed;
  Result := TInfraInteger.NewFrom(5000);
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'FuncSemParWithoutProceed',
    EmptyStr,
    IntToStr(Result.AsInteger)]));
end;

function TClassB.FuncSemParWithProceed: IInfraInteger;
begin
  Result := TInfraInteger.NewFrom(5000);
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'FuncSemParWithProceed',
    EmptyStr,
    IntToStr(Result.AsInteger)]));
end;

procedure RegisterClassesOnReflection;
var
  InfraObjectInfo: IInfraTypeInfo;
begin
  with TypeRegisterService do
  begin
    InfraObjectInfo := GetTypeInfo(IInfraObject);

    with RegisterNewType(IClassB, 'TClassB', TClassB,
      IInfraObject, InfraObjectInfo) do
    begin
      with AddMember('FuncSemPar', TInfraMethod, IInfraMethod,
        @TClassB.FuncSemPar, mrkInterface) do
      begin
         AddMember('Result', TInfraInteger, IInfraInteger, IInfraType);
      end;
      with AddMember('FuncSemParWithoutProceed', TInfraMethod, IInfraMethod,
        @TClassB.FuncSemParWithoutProceed, mrkInterface) do
      begin
         AddMember('Result', TInfraInteger, IInfraInteger, IInfraType);
      end;
      with AddMember('FuncSemParWithProceed', TInfraMethod, IInfraMethod,
        @TClassB.FuncSemParWithProceed, mrkInterface) do
      begin
         AddMember('Result', TInfraInteger, IInfraInteger, IInfraType);
      end;
      with AddMember('Func1Par', TInfraMethod, IInfraMethod,
        @TClassB.Func1Par, mrkInterface) do
      begin
         AddMember('x', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('Result', TInfraInteger, IInfraInteger, IInfraType);
      end;
      with AddMember('Func2Par', TInfraMethod, IInfraMethod,
        @TClassB.Func2Par, mrkInterface) do
      begin
         AddMember('x', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('y', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('Result', TInfraInteger, IInfraInteger, IInfraType);
      end;
      with AddMember('Func3Par', TInfraMethod, IInfraMethod,
        @TClassB.Func3Par, mrkInterface) do
      begin
         AddMember('x', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('y', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('z', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('Result', TInfraInteger, IInfraInteger, IInfraType);
      end;
      with AddMember('Func4Par', TInfraMethod, IInfraMethod,
        @TClassB.Func4Par, mrkInterface) do
      begin
         AddMember('x', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('y', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('z', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('w', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('Result', TInfraInteger, IInfraInteger, IInfraType);
      end;
      with AddMember('Func5Par', TInfraMethod, IInfraMethod,
        @TClassB.Func5Par, mrkInterface) do
      begin
         AddMember('x', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('y', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('z', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('w', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('k', TInfraInteger, IInfraInteger, IInfraType);
         AddMember('Result', TInfraInteger, IInfraInteger, IInfraType);
      end;
    end;
  end;
end;

initialization
  RegisterClassesOnReflection;

end.

