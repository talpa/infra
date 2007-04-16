unit AnnotationsTests;

interface

uses
  TestFrameWork,
  SysUtils,
  InfraCommonIntf,InfraValueTypeIntf,
  Model,
  ModelIntf;

type
  TAnnotationsTests = class(TTestCase)
  private
    FAddress: IAddress;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAnnotatedWith;
    procedure TestAnnotationUse;
  end;

implementation

{ TAnnotationsTests }

procedure TAnnotationsTests.TestAnnotatedWith;
var
  c: IClassInfo;
begin
  c := TypeService.GetType(IAddress);
  c.Annotate(IVersion);
  CheckTrue((c as IMemoryManagedObject).IsAnnotedWith(IVersion),
    'IVersion should be annotated into IAddress');
end;

procedure TAnnotationsTests.TestAnnotationUse;
var
  vClassInfo: IClassInfo;
  vVersion: IVersion;
begin
  vClassInfo := TypeService.GetType(IAddress);
  vVersion := vClassInfo.Annotate(IVersion) as IVersion;

  CheckEquals('1.0', vVersion.VersionNumber.AsString,
    'Default Version object should be 1.0');
  CheckEquals('1.0', (vClassInfo as IVersion).VersionNumber.AsString,
    'IAddress metadata Version should be 1.0');

  vVersion.VersionNumber.AsString := '1.1.0';

  CheckEquals('1.1.0', (vClassInfo as IVersion).VersionNumber.AsString,
    'IAddress metadata Version should be 1.1.0');
end;

procedure TAnnotationsTests.SetUp;
begin
  inherited;
  TSetupModel.RegisterAddress;
  TSetupModel.RegisterVersion;
  FAddress:= TAddress.Create;
  with FAddress do
  begin
    Street.AsString := 'Filiph Street';
    City.AsString := 'Filiph City';
    Quarter.AsString := 'Filiph Quarter';
    Number.AsInteger := 100;
  end;
end;

procedure TAnnotationsTests.TearDown;
begin
  TSetupModel.RemoveTypeInfo(IVersion);
  TSetupModel.RemoveTypeInfo(IAddress);
  inherited;
end;

initialization
  TestFramework.RegisterTest('AnnotationsTests Suite', TAnnotationsTests.Suite);

end.
