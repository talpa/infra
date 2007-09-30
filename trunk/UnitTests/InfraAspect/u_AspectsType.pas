// FixUses
unit u_AspectsType;

interface

uses
  InfraInterfaces, AspectImpl;

type
  TAspect1 = class(TInfraAspectType)
    procedure After(const Sender: IInfraElement;
      const Params: IInfraList); override;
    procedure Before(const Sender: IInfraElement;
      const Params: IInfraList); override;
  end;

  TAspect2 = class(TInfraAspectType)
    procedure After(const Sender: IInfraElement;
      const Params: IInfraList); override;
    procedure Before(const Sender: IInfraElement;
      const Params: IInfraList); override;
  end;

  TAspect3 = class(TInfraAspectType)
    function Around(const Sender: IInfraElement;
      const Params: IInfraList): IInfraType; override;
    procedure Before(const Sender: IInfraElement;
      const Params: IInfraList); override;
  end;

  TAspect4 = class(TInfraAspectType)
    function Around(const Sender: IInfraElement;
      const Params: IInfraList): IInfraType; override;
    procedure After(const Sender: IInfraElement;
      const Params: IInfraList); override;
  end;

implementation

uses
  SysUtils, u_GlobalTestList, SimpleTypeImpl;

function ParamsToString(const Params: IInfraList): string;
var
  j: Integer;
  i: IInfraInteger;
  s: IInfraString;
begin
  Result := '';
  if not Assigned(Params) then
    Exit;
  for j := 0 to Pred(Params.Count) do
  begin
    if Result <> EmptyStr then
      Result := Result + ', ';
    if Supports(Params[j], IInfraInteger, i) then
      Result := Result + IntToStr(i.AsInteger)
    else if Supports(Params[j], IInfraString, s) then
      Result := Result + s.AsString
    else
      Result := Result + '<TypeNotTreated>';
  end;
end;

{ TAspect1 }

procedure TAspect1.After(const Sender: IInfraElement;
  const Params: IInfraList);
begin
  inherited;
  GlobalTestList.Add(Format(cMsgAdviceCalled,
    [ClassName, 'After',
    Sender.TypeInfo.FullName, ParamsToString(Params)]));
end;

procedure TAspect1.Before(const Sender: IInfraElement;
  const Params: IInfraList);
begin
  inherited;
  GlobalTestList.Add(Format(cMsgAdviceCalled,
    [ClassName, 'Before',
    Sender.TypeInfo.FullName, ParamsToString(Params)]));
end;

{ TAspect2 }

procedure TAspect2.After(const Sender: IInfraElement;
  const Params: IInfraList);
begin
  inherited;
  GlobalTestList.Add(Format(cMsgAdviceCalled,
    [ClassName, 'After',
    Sender.TypeInfo.FullName, ParamsToString(Params)]));
end;

procedure TAspect2.Before(const Sender: IInfraElement;
  const Params: IInfraList);
begin
  inherited;
  GlobalTestList.Add(Format(cMsgAdviceCalled,
    [ClassName, 'Before',
    Sender.TypeInfo.FullName, ParamsToString(Params)]));
end;

{ TAspect3 }

function TAspect3.Around(const Sender: IInfraElement;
  const Params: IInfraList): IInfraType;
begin
  Result := TInfraInteger.NewFrom(1000);
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around',
    Sender.TypeInfo.FullName, ParamsToString(Params),
    IntToStr((Result as IInfraInteger).AsInteger), '']));
end;

procedure TAspect3.Before(const Sender: IInfraElement; const Params: IInfraList);
begin
  inherited;
  GlobalTestList.Add(Format(cMsgAdviceCalled,
    [ClassName, 'Before',
    Sender.TypeInfo.FullName, ParamsToString(Params)]));
end;

{ TAspect4 }

function TAspect4.Around(const Sender: IInfraElement;
  const Params: IInfraList): IInfraType;
begin
  GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
    [ClassName, 'Around',
    Sender.TypeInfo.FullName, ParamsToString(Params),
    '', 'Before Proceed ']));
  Result := Proceed;
  with (Result as IInfraInteger) do
  begin
    AsInteger := AsInteger + 30;
    GlobalTestList.Add(Format(cMsgAdviceAroundCalled,
      [ClassName, 'Around',
      Sender.TypeInfo.FullName, ParamsToString(Params),
      IntToStr(AsInteger), 'After Proceed ']));
  end;
end;

procedure TAspect4.After(const Sender: IInfraElement;
  const Params: IInfraList);
begin
  inherited;
  GlobalTestList.Add(Format(cMsgAdviceCalled,
    [ClassName, 'After',
    Sender.TypeInfo.FullName, ParamsToString(Params)]));
end;

procedure RegisterAspects;
begin
  with AspectService do
  begin
    // registra os métodos ProcSemPar e Proc2Par de TClasseA ao TAspect1
    AddPointCut('TClassA.ProcSemPar OR TClassA.Proc2Par', TAspect1);

    // registra todos os métodos de TClassA exceto Proc2Par ao TAspect2
    AddPointCut('TClassA.* AND NOT TClassA.Proc2Par', TAspect2);

    // registra todos os métodos de TClassB exceto Func1Par e Func2Par ao TAspect1
    AddPointCut('TClassB.* AND NOT (TClassB.Func1Par OR '+
      'TClassB.Func2Par OR TClassB.FuncSemParWithProceed)', TAspect1);

    // registra os métodos Func1Par e Func2Par de TClasseB ao TAspect2
    AddPointCut('TClassB.Func1Par OR TClassB.Func2Par', TAspect2);

    // registra o método TClassB.FuncSemParWithoutProceed ao TAspect3
    AddPointCut('TClassB.FuncSemParWithoutProceed', TAspect3);

    // registra o método TClassB.FuncSemParWithProceed ao TAspect4
    AddPointCut('TClassB.FuncSemParWithProceed', TAspect4);
  end;
end;

initialization
  RegisterAspects;

end.

