unit u_ClassesFuncPrimitive;

interface

uses
  InfraInterfaces, SimpleTypeImpl;

type
  IClassD = interface(IInfraObject)
    ['{F2923DD7-BF91-4E60-9CA4-CAF932523C16}']
    function FuncSemPar: integer;
    function Func1Par(Value: Integer): integer;
    function Func2Par(Value, Value2: Integer): integer;
    function Func3Par(Value, Value2, Value3: Integer): integer;
    function Func4Par(Value, Value2, Value3, Value4: Integer): integer;
    function Func5Par(Value, Value2, Value3, Value4, Value5: Integer): integer;
  end;

  TClassD = class(TInfraObject, IClassD)
  private
    function FuncSemPar: Integer; virtual;
    function Func1Par(x: Integer): Integer; virtual;
    function Func2Par(x, y: Integer): Integer; virtual;
    function Func3Par(x, y, z: Integer): Integer; virtual;
    function Func4Par(x, y, z, w: Integer): Integer; virtual;
    function Func5Par(x, y, z, w, k: Integer): Integer; virtual;
  protected
    procedure InitTypeInfo; override;
  end;

implementation

uses
  SysUtils, u_GlobalTestList;

{ TClassD }

procedure TClassD.InitTypeInfo;
begin
  inherited;
  FTypeInfo := TypeRegisterService.GetTypeInfo(IClassD);
end;

function TClassD.Func1Par(x: Integer): Integer;
begin
  Result := x * 125;
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'Func1Par',
    IntToStr(x), IntToStr(Result)]));
end;

function TClassD.Func2Par(x, y: Integer): Integer;
begin
  Result := x + y;
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'Func2Par',
    IntToStr(x)+', '+IntToStr(y), IntToStr(Result)]));
end;

function TClassD.Func3Par(x, y, z: Integer): Integer;
begin
  Result := x + y + z;
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'Func3Par',
    IntToStr(x)+', '+IntToStr(y)+', '+IntToStr(z), IntToStr(Result)]));
end;

function TClassD.Func4Par(x, y, z, w: Integer): Integer;
begin
  Result := x * y * z * w;
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'Func4Par',
    IntToStr(x)+', '+IntToStr(y)+', '+IntToStr(z)+', '+IntToStr(w),
    IntToStr(Result)]));
end;

function TClassD.Func5Par(x, y, z, w, k: Integer): Integer;
begin
  Result := x * y * z * w * k;
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'Func5Par',
    IntToStr(x)+', '+IntToStr(y)+', '+IntToStr(z)+', '+
    IntToStr(w)+', '+IntToStr(k),
    IntToStr(Result)]));
end;

function TClassD.FuncSemPar: Integer;
begin
  Result := 55;
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'FuncSemPar',
    EmptyStr, IntToStr(Result)]));
end;

{
function TClassD.FuncSemParWithoutProceed: Integer;
begin
  // This function don't should be called, because the aspect that intercept it
  // don't call proceed;
  Result := 5000;
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'FuncSemParWithoutProceed',
    EmptyStr, IntToStr(Result)]));
end;

function TClassD.FuncSemParWithProceed: Integer;
begin
  Result := 5000;
  GlobalTestList.Add(Format(cMsgFunctionCalled,
    [Self.TypeInfo.FullName, 'FuncSemParWithProceed',
    EmptyStr, IntToStr(Result)]));
end;
}

procedure RegisterClassesOnReflection;
var
  InfraObjectInfo: IInfraTypeInfo;
begin
  with TypeRegisterService do
  begin
    InfraObjectInfo := GetTypeInfo(IInfraObject);

    with RegisterNewType(IClassD, 'TClassD', TClassD,
      IInfraObject, InfraObjectInfo) do
    begin
      AddMember('FuncSemPar', TInfraMethod, IInfraPrimitiveMethod,
        @TClassD.FuncSemPar, mrkInteger);
      AddMember('Func1Par', TInfraMethod, IInfraPrimitiveMethod,
        @TClassD.Func1Par, mrkInteger);
      AddMember('Func2Par', TInfraMethod, IInfraPrimitiveMethod,
        @TClassD.Func2Par, mrkInteger);
      AddMember('Func3Par', TInfraMethod, IInfraPrimitiveMethod,
        @TClassD.Func3Par, mrkInteger);
      AddMember('Func4Par', TInfraMethod, IInfraPrimitiveMethod,
        @TClassD.Func4Par, mrkInteger);
      AddMember('Func5Par', TInfraMethod, IInfraPrimitiveMethod,
        @TClassD.Func5Par, mrkInteger);
    end;
  end;
end;

initialization
  RegisterClassesOnReflection;

end.

