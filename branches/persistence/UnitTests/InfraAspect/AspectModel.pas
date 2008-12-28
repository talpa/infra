unit AspectModel;

interface

uses
  InfraValueTypeIntf, InfraValueType, AspectModelIntf;

type
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

  TClassB = class(TInfraObject, IClassB)
  public
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

uses
  SysUtils, InfraCommonIntf, InfraCommon;

{ TClassA }

procedure TClassA.InitTypeInfo;
begin
  inherited;
  TypeInfo := TypeService.GetType(IClassA);
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

{ TClassB }

procedure TClassB.InitTypeInfo;
begin
  inherited;
  TypeInfo := TypeService.GetType(IClassB);
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

end.
