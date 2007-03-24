unit MethodInfoTests;

interface

uses
  TestFrameWork,
  InfraCommonIntf,
  ModelIntf;

type
  TMethodInfoTests = class(TTestCase)
  private
    FMockMethod: IClassInfo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Test methods
    procedure TestAddParam;
    procedure TestGetCallingConvention;
    procedure TestGetIsConstructor;
    procedure TestGetMethodPointer;
    procedure TestGetParameters;
    procedure TestGetReturnType;
    procedure TestInvoke;
  end;

implementation

uses
  SysUtils, InfraConsts,
  InfraValueTypeIntf,
  Model;

{ TMethodInfoTests }


procedure TMethodInfoTests.SetUp;
begin
  inherited;
  FMockMethod := TSetupModel.RegisterMockMethod;
end;

procedure TMethodInfoTests.TearDown;
begin
  TSetupModel.RemoveTypeInfo(IMockMethod);
  inherited;
end;

procedure TMethodInfoTests.TestAddParam;
begin
  CheckEquals(0, FMockMethod.GetMethodInfo('MethodProc0').Parameters.Count,
    'MethodProc0 don''''t should be paramaters');
end;

procedure TMethodInfoTests.TestGetCallingConvention;
begin

end;

procedure TMethodInfoTests.TestGetIsConstructor;
begin

end;

procedure TMethodInfoTests.TestGetMethodPointer;
begin

end;

procedure TMethodInfoTests.TestGetParameters;
begin

end;

procedure TMethodInfoTests.TestGetReturnType;
begin

end;

procedure TMethodInfoTests.TestInvoke;
begin

end;

initialization
  TestFramework.RegisterTest('InfraReflectTests Suite', TMethodInfoTests.Suite);

end.
