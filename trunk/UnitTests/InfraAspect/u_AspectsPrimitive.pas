// FixUses
unit u_AspectsPrimitive;

interface

uses
  InfraInterfaces, AspectImpl;

type
  TPrimitiveAspect1 = class(TInfraAspectPrimitive)
  public
    procedure Around0(const Sender: IInfraElement);
    procedure Around1(const Sender: IInfraElement; Value: Integer);
    procedure Around2(const Sender: IInfraElement; Value, Value2: Integer);
    procedure Before2(const Sender: IInfraElement; Value, Value2: Integer);
    procedure After2(const Sender: IInfraElement; Value, Value2: Integer);
    procedure Around3(const Sender: IInfraElement; Value, Value2,
      Value3: integer);
    procedure Around5(const Sender: IInfraElement; Value, Value2, Value3,
      Value4, Value5: Integer);
  end;

  TPrimitiveAspect2 = class(TInfraAspectPrimitive)
  public
    procedure Around1(const Sender: IInfraElement; Value: integer);
    procedure After1(const Sender: IInfraElement; Value: integer);
    procedure Before2(const Sender: IInfraElement; Value, Value2: Integer);
    procedure Around5(const Sender: IInfraElement; Value, Value2, Value3,
      Value4, Value5: Integer);
    procedure Before4(const Sender: IInfraElement; Value, Value2, Value3,
      Value4: Integer);
    procedure After4(const Sender: IInfraElement; Value, Value2, Value3,
      Value4: Integer);
  end;

implementation

uses
  SysUtils, u_GlobalTestList, SimpleTypeImpl, u_ClassesProcPrimitive;

{ TPrimitiveAspect1 }

procedure TPrimitiveAspect1.Around0(const Sender: IInfraElement);
begin
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around0', Sender.TypeInfo.FullName,
    '', '', 'Before Proceed ']));
  inherited Proceed;
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around0', Sender.TypeInfo.FullName,
    '', '', 'After Proceed ']));
end;

procedure TPrimitiveAspect1.Around1(const Sender: IInfraElement;
  Value: Integer);
begin
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around1', Sender.TypeInfo.FullName,
    IntToStr(Value), '', 'Before Proceed ']));
  inherited Proceed;
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around1', Sender.TypeInfo.FullName,
    IntToStr(Value), '', 'After Proceed ']));
end;

procedure TPrimitiveAspect1.Before2(const Sender: IInfraElement; Value,
  Value2: Integer);
begin
  GlobalTestList.Add(Format(cMsgAdviceCalled,
    [ClassName, 'Before2', Sender.TypeInfo.FullName,
    IntToStr(Value)+', '+IntToStr(Value2)]));
end;

procedure TPrimitiveAspect1.After2(const Sender: IInfraElement; Value,
  Value2: Integer);
begin
  GlobalTestList.Add(Format(cMsgAdviceCalled,
    [ClassName, 'After2', Sender.TypeInfo.FullName,
    IntToStr(Value)+', '+IntToStr(Value2)]));
end;

procedure TPrimitiveAspect1.Around2(const Sender: IInfraElement; Value,
  Value2: Integer);
begin
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around2', Sender.TypeInfo.FullName,
    IntToStr(Value)+', '+IntToStr(Value2), '', 'Before Proceed ']));
  inherited Proceed;
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around2', Sender.TypeInfo.FullName,
    IntToStr(Value)+', '+IntToStr(Value2), '', 'After Proceed ']));
end;

procedure TPrimitiveAspect1.Around3(const Sender: IInfraElement; Value,
  Value2, Value3: integer);
begin
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around3', Sender.TypeInfo.FullName,
    IntToStr(Value)+', '+IntToStr(Value2)+', '+IntToStr(Value3),
    '', 'Before Proceed ']));
  inherited Proceed;
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around3', Sender.TypeInfo.FullName,
    IntToStr(Value)+', '+IntToStr(Value2)+', '+IntToStr(Value3),
    '', 'After Proceed ']));
end;

procedure TPrimitiveAspect1.Around5(const Sender: IInfraElement; Value,
  Value2, Value3, Value4, Value5: Integer);
begin
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around5', Sender.TypeInfo.FullName,
    IntToStr(Value)+', '+IntToStr(Value2)+', '+IntToStr(Value3)+', '+
    IntToStr(Value4)+', '+IntToStr(Value5), '', 'Before Proceed ']));
  inherited Proceed;
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around5', Sender.TypeInfo.FullName,
    IntToStr(Value)+', '+IntToStr(Value2)+', '+IntToStr(Value3)+', '+
    IntToStr(Value4)+', '+IntToStr(Value5), '', 'After Proceed ']));
end;

{ TPrimitiveAspect2 }

procedure TPrimitiveAspect2.Before4(const Sender: IInfraElement; Value,
  Value2, Value3, Value4: Integer);
begin
  GlobalTestList.Add(Format(cMsgAdviceCalled,
    [ClassName, 'Before4', Sender.TypeInfo.FullName,
    IntToStr(Value)+', '+IntToStr(Value2)+', '+IntToStr(Value3)+', '+
    IntToStr(Value4)]));
end;

procedure TPrimitiveAspect2.After1(const Sender: IInfraElement;
  Value: integer);
begin
  GlobalTestList.Add(Format(cMsgAdviceCalled,
    [ClassName, 'After1', Sender.TypeInfo.FullName,
    IntToStr(Value)]));
end;

procedure TPrimitiveAspect2.After4(const Sender: IInfraElement; Value,
  Value2, Value3, Value4: Integer);
begin
  GlobalTestList.Add(Format(cMsgAdviceCalled,
    [ClassName, 'After4', Sender.TypeInfo.FullName,
    IntToStr(Value)+', '+IntToStr(Value2)+', '+IntToStr(Value3)+', '+
    IntToStr(Value4)]));
end;

procedure TPrimitiveAspect2.Around1(const Sender: IInfraElement;
  Value: integer);
begin
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around1', Sender.TypeInfo.FullName,
    IntToStr(Value), '', 'Before Proceed ']));
  inherited Proceed;
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around1', Sender.TypeInfo.FullName,
    IntToStr(Value), '', 'After Proceed ']));
end;

procedure TPrimitiveAspect2.Around5(const Sender: IInfraElement; Value,
  Value2, Value3, Value4, Value5: Integer);
begin
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around5', Sender.TypeInfo.FullName,
    IntToStr(Value)+', '+IntToStr(Value2)+', '+IntToStr(Value3)+', '+
    IntToStr(Value4)+', '+IntToStr(Value5), '', 'Before Proceed ']));
  inherited Proceed;
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around5', Sender.TypeInfo.FullName,
    IntToStr(Value)+', '+IntToStr(Value2)+', '+IntToStr(Value3)+', '+
    IntToStr(Value4)+', '+IntToStr(Value5), '', 'After Proceed ']));
end;

procedure TPrimitiveAspect2.Before2(const Sender: IInfraElement; Value,
  Value2: Integer);
begin
  GlobalTestList.Add(Format(cMsgAdviceCalled,
    [ClassName, 'Before2', Sender.TypeInfo.FullName,
    IntToStr(Value)+', '+IntToStr(Value2)]));
end;

procedure RegisterAspects;
begin
  with AspectService do
  begin
    Intercept(TypeRegisterService.GetTypeInfo(IClassC),
      'ProcSemPar', atAround, TPrimitiveAspect1, @TPrimitiveAspect1.Around0, 0);

    Intercept(TypeRegisterService.GetTypeInfo(IClassC),
      'Proc1Par', atAround, TPrimitiveAspect1, @TPrimitiveAspect1.Around1, 1);
    Intercept(TypeRegisterService.GetTypeInfo(IClassC),
      'Proc1Par', atAround, TPrimitiveAspect2, @TPrimitiveAspect2.Around1, 1);
    Intercept(TypeRegisterService.GetTypeInfo(IClassC),
      'Proc1Par', atAfter, TPrimitiveAspect2, @TPrimitiveAspect2.After1, 1);

    Intercept(TypeRegisterService.GetTypeInfo(IClassC),
      'Proc2Par', atAround, TPrimitiveAspect1, @TPrimitiveAspect1.Around2, 2);
    Intercept(TypeRegisterService.GetTypeInfo(IClassC),
      'Proc2Par', atBefore, TPrimitiveAspect1, @TPrimitiveAspect1.Before2, 2);
    Intercept(TypeRegisterService.GetTypeInfo(IClassC),
      'Proc2Par', atAfter, TPrimitiveAspect1, @TPrimitiveAspect1.After2, 2);
    Intercept(TypeRegisterService.GetTypeInfo(IClassC),
      'Proc2Par', atBefore, TPrimitiveAspect2, @TPrimitiveAspect2.Before2, 2);

    Intercept(TypeRegisterService.GetTypeInfo(IClassC),
      'Proc3Par', atAround, TPrimitiveAspect1, @TPrimitiveAspect1.Around3, 3);

    Intercept(TypeRegisterService.GetTypeInfo(IClassC),
      'Proc4Par', atBefore, TPrimitiveAspect2, @TPrimitiveAspect2.Before4, 4);
    Intercept(TypeRegisterService.GetTypeInfo(IClassC),
      'Proc4Par', atAfter, TPrimitiveAspect2, @TPrimitiveAspect2.After4, 4);

    Intercept(TypeRegisterService.GetTypeInfo(IClassC),
      'Proc5Par', atAround, TPrimitiveAspect1, @TPrimitiveAspect1.Around5, 5);
    Intercept(TypeRegisterService.GetTypeInfo(IClassC),
      'Proc5Par', atAround, TPrimitiveAspect2, @TPrimitiveAspect2.Around5, 5);
  end;
end;

initialization
  RegisterAspects;

end.

