program GUIBuilderDemo;

uses
  FastMM4,
  ApplicationContext,
  Forms,
  SysUtils,
  InfraGUIBuilder,
  InfraGUIBuilderIntf,
  GUIAnnotation,
  GUIAnnotationIntf,
  InfraCommonIntf,
  InfraValueTypeIntf,
  InfraValueType,
  LayoutManager,
  ExtCtrls,
  Model in 'Model.pas',
  ModelIntf in 'ModelIntf.pas',
  StdCtrls,
  cxCalendar;

var
  lPerson: IPerson;
  lPersonInfo: IClassInfo;
  lGUIPersonSimple: IScreen;
  lGUIPerson: IScreen;
  lItem: IScreenControl;

{$R *.res}

begin
  Application.Initialize;
  Application.Run;




  //Data -----------------------------------------------------------------------
  lPerson := TPerson.Create;
  lPersonInfo := lPerson.TypeInfo;

  lPerson.ID.AsInteger := 1;
  lPerson.Name.AsString := 'Diogo Augusto Pereira';
  lPerson.Email.AsString := 'diogoap82@gmail.com';
  lPerson.Address.AsString := 'Rua Alfredo Wust, 838';
  lPerson.City.Name.AsString := 'Rolante';
  lPerson.City.Population.AsInteger := 18500;
  lPerson.State.AsString := 'RS';
  lPerson.Country.AsString := 'Brasil';
  lPerson.Birthday.AsDateTime := 30280;
  lPerson.Active.AsBoolean := True;
  lPerson.Amount.AsDouble := 10000;
  lPerson.Details.AsString := 'ded ekjdhkjdejdhkjedhejkd hekjdhekjd eh ' + #13 +
    'dkjedh kjedhkje dhke dkje dhkje ddededej kljelkdj ekldj edde yt e ' + #13 +
    'yty ttdkj ekljdeklj dkelj dklejdekldjekld jekldjekldje ddekljdeklj ekljdj';




  //Simples --------------------------------------------------------------------
  lGUIPersonSimple := (lPersonInfo.Annotate(IScreens) as IScreens).AddScreen('Cadastro de pessoas - Simples');
  lGUIPersonSimple.Title.AsString := 'Cadastro de pessoas - Simples';
  lGUIPersonSimple.CaptionPosition := lpAbove;
  lGUIPersonSimple.HideProperties.Add('Email');
  lGUIPersonSimple.HideProperties.Add('State');
  lGUIPersonSimple.HideProperties.Add('Country');
  lGUIPersonSimple.HideProperties.Add('Birthday');
  lGUIPersonSimple.HideProperties.Add('Amount');
  lGUIPersonSimple.HideProperties.Add('Details');

  lItem := lGUIPersonSimple.AddControl('ID');
  lItem.Width.AsInteger := 50;

  lItem := lGUIPersonSimple.AddControl('Name');
  lItem.Caption.AsString := 'Nome';
  lItem.SetSize(TInfraInteger.NewFrom(30), TInfraInteger.NewFrom(300));

  lItem := lGUIPersonSimple.AddControl('Address');
  lItem.Caption.AsString := 'Endereço';
  lItem.Width.AsInteger := 300;




  //Completa -------------------------------------------------------------------
  lGUIPerson := (lPersonInfo.Annotate(IScreens) as IScreens).AddScreen('Cadastro de pessoas');
  lGUIPerson.Title.AsString := 'Cadastro de pessoas';
  lGUIPerson.CaptionPosition := lpLeft;
  lGUIPerson.ItemLayout := laHorizontal;

  lItem := lGUIPerson.AddControl('ID');
  lItem.Width.AsInteger := 50;

  lItem := lGUIPerson.AddControl('Name');
  lItem.Caption.AsString := 'Nome';
  lItem.SetSize(TInfraInteger.NewFrom(30), TInfraInteger.NewFrom(450));

  lItem := lGUIPerson.AddControl('Email');
  lItem.ItemWidth.AsInteger := 500;

  lItem := lGUIPerson.AddControl('Address');
  lItem.Caption.AsString := 'Endereço';
  lItem.Width.AsInteger := 500;
  lItem.PutBefore := 'ID';

  lItem := lGUIPerson.AddControl('City.Name');
  lItem.Caption.AsString := 'Cidade - Nome';
  lItem.Width.AsInteger := 300;
  lItem.ControlClass := TComboBox;

  lItem := lGUIPerson.AddControl('City.Population');
  lItem.Caption.AsString := 'Cidade - População';
  lItem.Width.AsInteger := 100;
  lItem.PutBefore := 'Email';

  lItem := lGUIPerson.AddControl('Birthday');
  lItem.Caption.AsString := 'Data de nascimento';
  lItem.ControlClass := TcxDateEdit;
  lItem.CaptionPosition := lpLeft;
  lItem.PutBefore := 'Amount';

  lItem := lGUIPerson.AddControl('Details');
  lItem.Caption.AsString := 'Observações';
  lItem.ControlClass := TMemo;
  lItem.ItemWidth.AsInteger := 500;
  lItem.ItemHeightMeasureType := mtPercent;
  lItem.ItemHeight.AsInteger := 35;
  lItem.PutAfter := 'City.Name';




  //Build ----------------------------------------------------------------------
  //GUIService.Build(lPerson, lGUIPersonSimple);
  GUIService.Build(lPerson, lGUIPerson);
  //GUIService.Build(lPerson);
end.
