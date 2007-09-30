unit u_GlobalTestList;

interface

uses Classes;

var
  GlobalTestList: TStrings;

const
  cMsgAdviceCalled = '%s.%s(%s, [%s]) called';
  cMsgAdviceAroundCalled = '%s.%s(%s, [%s]):%s %scalled';
  cMsgProcedureCalled = '%s.%s(%s) called';
  cMsgFunctionCalled = '%s.%s(%s):%s called';
  cMsgAdviceOutOfOrder = '%s out of order';

implementation

initialization
  GlobalTestList := TStringList.Create;

finalization
  GlobalTestList.Free;

end.
