unit AspectModelIntf;

interface

uses
  Classes, InfraValueTypeIntf;

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

const
  cMsgAdviceCalled = '%s.%s(%s, [%s]) called';
  cMsgAdviceAroundCalled = '%s.%s(%s, [%s]):%s %scalled';
  cMsgProcedureCalled = '%s.%s(%s) called';
  cMsgFunctionCalled = '%s.%s(%s):%s called';
  cMsgAdviceOutOfOrder = '%s out of order';

function GlobalTestList: TStrings;

implementation

var
  _GlobalTestList: TStrings;

function GlobalTestList: TStrings;
begin
  if not Assigned(_GlobalTestList) then
   _GlobalTestList := TStringList.Create;
  Result := _GlobalTestList;
end;

initialization

finalization
  if Assigned(_GlobalTestList) then
    _GlobalTestList.Free;

end.
