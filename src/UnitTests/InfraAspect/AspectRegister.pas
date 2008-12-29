unit AspectRegister;

interface

procedure RegisterClassesOnReflection;

implementation

uses
  AspectModel,
  AspectModelIntf,
  InfraCommonIntf,
  InfraValueTypeIntf;

procedure RegisterClassesOnReflection;
begin
  with TypeService do
  begin
    with AddType(IClassA, 'TClassA', TClassA,
      IInfraObject, GetType(IInfraObject)) do
    begin
      AddMethodInfo('ProcSemPar', nil, @TClassA.ProcSemPar);

      with AddMethodInfo('Proc1Par', nil, @TClassA.Proc1Par) do
        AddParam('x', GetType(IInfraInteger));

      with AddMethodInfo('Proc2Par', nil, @TClassA.Proc2Par) do
      begin
        AddParam('x', GetType(IInfraInteger));
        AddParam('y', GetType(IInfraInteger));
      end;

      with AddMethodInfo('Proc3Par', nil, @TClassA.Proc3Par) do
      begin
        AddParam('x', GetType(IInfraInteger));
        AddParam('y', GetType(IInfraInteger));
        AddParam('z', GetType(IInfraInteger));
      end;

      with AddMethodInfo('Proc4Par', nil, @TClassA.Proc4Par) do
      begin
        AddParam('x', GetType(IInfraInteger));
        AddParam('y', GetType(IInfraInteger));
        AddParam('z', GetType(IInfraInteger));
        AddParam('w', GetType(IInfraInteger));
      end;

      with AddMethodInfo('Proc5Par', nil, @TClassA.Proc5Par) do
      begin
        AddParam('x', GetType(IInfraInteger));
        AddParam('y', GetType(IInfraInteger));
        AddParam('z', GetType(IInfraInteger));
        AddParam('w', GetType(IInfraInteger));
        AddParam('k', GetType(IInfraInteger));
      end;
    end;   

    with AddType(IClassB, 'TClassB', TClassB,
      IInfraObject, GetType(IInfraObject)) do
    begin

      with AddMethodInfo('FuncSemPar', nil,
        @TClassB.FuncSemPar, GetType(IInfraInteger)) do
        AddParam('Result', GetType(IInfraInteger));

      with AddMethodInfo('FuncSemParWithoutProceed', nil,
        @TClassB.FuncSemParWithoutProceed, GetType(IInfraInteger)) do
        AddParam('Result', GetType(IInfraInteger));

      with AddMethodInfo('FuncSemParWithProceed', nil,
        @TClassB.FuncSemParWithProceed, GetType(IInfraInteger)) do
        AddParam('Result', GetType(IInfraInteger));

      with AddMethodInfo('Func1Par', nil, @TClassB.Func1Par,
        GetType(IInfraInteger)) do
      begin
        AddParam('x', GetType(IInfraInteger));
        AddParam('Result', GetType(IInfraInteger));
      end;

      with AddMethodInfo('Func2Par', nil, @TClassB.Func2Par,
        GetType(IInfraInteger)) do
      begin
        AddParam('x', GetType(IInfraInteger));
        AddParam('y', GetType(IInfraInteger));
        AddParam('Result', GetType(IInfraInteger));
      end;

      with AddMethodInfo('Func3Par', nil, @TClassB.Func3Par,
        GetType(IInfraInteger)) do
      begin
        AddParam('x', GetType(IInfraInteger));
        AddParam('y', GetType(IInfraInteger));
        AddParam('z', GetType(IInfraInteger));
        AddParam('Result', GetType(IInfraInteger));
      end;

      with AddMethodInfo('Func4Par', nil, @TClassB.Func4Par,
        GetType(IInfraInteger)) do
      begin
        AddParam('x', GetType(IInfraInteger));
        AddParam('y', GetType(IInfraInteger));
        AddParam('z', GetType(IInfraInteger));
        AddParam('w', GetType(IInfraInteger));
        AddParam('Result', GetType(IInfraInteger));
      end;

      with AddMethodInfo('Func5Par', nil, @TClassB.Func5Par,
        GetType(IInfraInteger)) do
      begin
        AddParam('x', GetType(IInfraInteger));
        AddParam('y', GetType(IInfraInteger));
        AddParam('z', GetType(IInfraInteger));
        AddParam('w', GetType(IInfraInteger));
        AddParam('k', GetType(IInfraInteger));
        AddParam('Result', GetType(IInfraInteger));
      end;
    end;
  end;
end;

initialization
  RegisterClassesOnReflection;

end.
