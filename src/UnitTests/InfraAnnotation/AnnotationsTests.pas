unit AnnotationsTests;

interface

uses
  TestFrameWork,
  AnnotationsModelIntf;

type
  TAnnotationsTests = class(TTestCase)
  private
    FAddress1: IAddress;
    FAddress2: IAddress;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAnnotate;
    procedure TestChangeAnnotationProperty;
    procedure TestIsAnnotationPresent;
    procedure TestGetAnnotation;
    procedure TestSupportAnnotation;
    procedure TestAnnotationByClass;
    procedure TestAnnotationByInstance;
    procedure TestIsAnnotation;
  end;

implementation

uses
  SysUtils,
  InfraCommonIntf,   
  AnnotationsModel;

{ TAnnotationsTests }

procedure TAnnotationsTests.SetUp;
begin
  inherited;
  TSetupModel.RegisterAddress;
  TSetupModel.RegisterVersion;
  FAddress1 := TAddress.Create;
  with FAddress1 do
  begin
    Street.AsString := 'Filiph Street';
    City.AsString := 'Filiph City';
    Quarter.AsString := 'Filiph Quarter';
    Number.AsInteger := 100;
  end;
  FAddress2 := TAddress.Create;
  with FAddress2 do
  begin
    Street.AsString := 'John Street';
    City.AsString := 'John City';
    Quarter.AsString := 'John Quarter';
    Number.AsInteger := 2000;
  end;
end;

procedure TAnnotationsTests.TearDown;
begin
  TSetupModel.RemoveTypeInfo(IVersion);
  TSetupModel.RemoveTypeInfo(IAddress);
  inherited;
end;

procedure TAnnotationsTests.TestAnnotate;
var
  c: IClassInfo;
  v: IVersion;
begin
  c := TypeService.GetType(IAddress);
  CheckFalse(c.isAnnotationPresent(IVersion),
    'IAddress não deveria estar anotado com IVersion');
  // Anotando TypeInfo de IAddress.
  v := c.Annotate(IVersion) as IVersion;
  CheckTrue(Assigned(v),
    'Variavel deveria ter uma referencia à anotação IVersion');
  CheckTrue(c.isAnnotationPresent(IVersion),
    'IAddress deveria estar anotado com IVersion');
  CheckEquals('1.0', v.VersionNumber.AsString,
    'Valor de IVersion no TypeInfo de IAddres deveria ser 1.0');
end;

procedure TAnnotationsTests.TestChangeAnnotationProperty;
var
  c: IClassInfo;
  v: IVersion;
begin
  c := TypeService.GetType(IAddress);
  // Anotando TypeInfo de IAddress.
  v := c.Annotate(IVersion) as IVersion;
  CheckEquals('1.0', v.VersionNumber.AsString,
    'Valor de IVersion no TypeInfo de IAddres deveria ser 1.0');
  // Mudando a versão no objeto anotado
  v.VersionNumber.AsString := '2.0';
  CheckEquals('2.0', v.VersionNumber.AsString,
    'Valor de IVersion no TypeInfo de IAddres deveria ser 2.0');
end;

procedure TAnnotationsTests.TestSupportAnnotation;
var
  c: IClassInfo;
begin
  c := TypeService.GetType(IAddress);

  CheckFalse(Supports(c, IVersion),
    'IAddress não deveria estar anotado com IVersion aqui');

  // Anotando TypeInfo de IAddress.
  c.Annotate(IVersion);

  CheckTrue(Supports(c, IVersion),
    'IAddress deveria estar anotado com IVersion aqui');

  CheckEquals('1.0', (c as IVersion).VersionNumber.AsString,
    'Valor de IVersion no TypeInfo de IAddres deveria ser 2.0');
end;

procedure TAnnotationsTests.TestGetAnnotation;
var
  c: IClassInfo;
  v: IVersion;
begin
  c := TypeService.GetType(IAddress);

  v := c.GetAnnotation(IVersion) as IVersion;
  CheckNull(v, 'IAddress não deveria estar anotado com IVersion aqui');

  // Anotando TypeInfo de IAddress.
  c.Annotate(IVersion);

  v := c.GetAnnotation(IVersion) as IVersion;
  CheckNotNull(v, 'IAddress deveria estar anotado com IVersion aqui');

  CheckEquals('1.0', v.VersionNumber.AsString,
    'Valor de IVersion no TypeInfo de IAddres deveria ser 2.0');
end;

procedure TAnnotationsTests.TestIsAnnotationPresent;
var
  c: IClassInfo;
begin
  c := TypeService.GetType(IAddress);
  CheckFalse(c.isAnnotationPresent(IVersion),
    'IAddress não deveria estar presente');

  // Anotando TypeInfo de IAddress com IVersion.
  c.Annotate(IVersion);

  CheckTrue(c.isAnnotationPresent(IVersion),
    'IVersion deveria estar presente');
end;

procedure TAnnotationsTests.TestAnnotationByClass;
begin

end;

procedure TAnnotationsTests.TestAnnotationByInstance;
begin

end;

procedure TAnnotationsTests.TestIsAnnotation;
var
  c: IClassInfo;
begin
  c := TypeService.GetType(IAddress);
  CheckFalse(c.IsAnnotation, 'IAddress não deveria ser uma anotação');

  c := TypeService.GetType(IVersion);
  CheckTrue(c.IsAnnotation, 'IVersion deveria ser uma anotação');
end;

initialization
  TestFramework.RegisterTest('AnnotationsTests Suite',
    TAnnotationsTests.Suite);

end.
