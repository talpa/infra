program GUIBuilderDemo;

{$I GUIBuilderDemo.Inc}

uses
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
  StdCtrls,
  ComCtrls,
  Model in 'Model.pas',
  ModelIntf in 'ModelIntf.pas';

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
  lPerson.LoadSampleData;



  //Simples --------------------------------------------------------------------
  lPersonInfo := TypeService.GetType(IPerson);
  lGUIPersonSimple := (lPersonInfo.Annotate(IScreens) as IScreens).
    AddScreen('PersonSimple');
  lGUIPersonSimple.Title.AsString := 'Cadastro de pessoas - Dados básicos';
  lGUIPersonSimple.CaptionPosition := lpAbove;
  lGUIPersonSimple.ShowProperties.Add('ID');
  lGUIPersonSimple.ShowProperties.Add('Name');
  lGUIPersonSimple.ShowProperties.Add('Email');
  lGUIPersonSimple.ShowProperties.Add('Birthday');

  lItem := lGUIPersonSimple.AddControl('ID');
  lItem.Caption.AsString := 'Código';
  lItem.Width.AsInteger := 50;

  lItem := lGUIPersonSimple.AddControl('Name');
  lItem.Caption.AsString := 'Nome';
  lItem.ItemWidthMeasureType := mtPercent;
  lItem.ItemWidth.AsInteger := 100;

  lItem := lGUIPersonSimple.AddControl('Email');
  lItem.ItemWidthMeasureType := mtPercent;
  lItem.ItemWidth.AsInteger := 100;

  lItem := lGUIPersonSimple.AddControl('Birthday');
  lItem.Caption.AsString := 'Data de nascimento';
  lItem.ControlClass := TDateTimePicker;



  //Completa -------------------------------------------------------------------
  lPersonInfo := TypeService.GetType(IPerson);
  lGUIPerson := (lPersonInfo.Annotate(IScreens) as IScreens).AddScreen('Person');
  lGUIPerson.Title.AsString := 'Cadastro de pessoas';

  lItem := lGUIPerson.AddControl('ID');
  lItem.Caption.AsString := 'Código';
  lItem.Width.AsInteger := 50;

  lItem := lGUIPerson.AddControl('Name');
  lItem.Caption.AsString := 'Nome';
  lItem.ItemWidthMeasureType := mtPercent;
  lItem.ItemWidth.AsInteger := 100;

  lItem := lGUIPerson.AddControl('Email');
  lItem.ItemWidthMeasureType := mtPercent;
  lItem.ItemWidth.AsInteger := 100;

  lItem := lGUIPerson.AddControl('Address');
  lItem.Caption.AsString := 'Endereço';
  lItem.ItemWidthMeasureType := mtPercent;
  lItem.ItemWidth.AsInteger := 100;

  lItem := lGUIPerson.AddControl('State');
  lItem.Caption.AsString := 'Estado';
  lItem.Width.AsInteger := 50;

  lGUIPerson.AddControl('Country').Caption.AsString := 'País';
  lGUIPerson.AddControl('Amount').Caption.AsString := 'Saldo';

  lItem := lGUIPerson.AddControl('Active');
  lItem.Caption.AsString := 'Ativo';
  lItem.Width.AsInteger := 65;

  lItem := lGUIPerson.AddControl('Birthday');
  lItem.Caption.AsString := 'Data de nascimento';
  lItem.ControlClass := TDateTimePicker;

  lItem := lGUIPerson.AddControl('Details');
  lItem.Caption.AsString := 'Observações';
  lItem.ControlClass := TMemo;
  lItem.ItemHeightMeasureType := mtPercent;
  lItem.ItemHeight.AsInteger := 35;
  lItem.ItemWidthMeasureType := mtPercent;
  lItem.ItemWidth.AsInteger := 100;

  lItem := lGUIPerson.AddControl('City.Name');
  lItem.Caption.AsString := 'Cidade - Nome';
  lItem.Width.AsInteger := 200;
  lItem.ControlClass := TComboBox;
  lItem.PutAfter := 'Address';


  
  //Build ----------------------------------------------------------------------
  GUIService.Build(lPerson, lGUIPersonSimple);
  GUIService.Build(lPerson, lGUIPerson);
  GUIService.Build(lPerson);
end.
