unit InfraInvoke;

interface

{$I 'Infra.Inc'}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Windows,
  InfraCommonIntf,
  InfraValueTypeIntf;

type
  TStubArray = array of Pointer;
  TByteArray = array[0..1024] of byte;
  TDwordArray = array[0..1024] of dword;
  TParams = array[0..50] of dword;
  PParams = ^TParams;

function InvokeMethod(pInstance: TObject;
  const pMethodInfo: IMethodInfo; const pParams: IInfraList): IInfraType;

implementation

uses
  SysUtils,
  InfraValueType;

end.
