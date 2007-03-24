unit InfraMVP_TypeInfoRegister;

interface

implementation

uses
  TypeImpl, SimpleTypeImpl, InfraInterfaces, InfraMVPInterfaces, MVPBaseImpl,
  MVPVclComponents, InfraCommandIntf, InfraCommandImpl;

procedure RegisterTypeInfo_MVP;
begin
  with TypeRegisterService do
  begin

    // Converters

    RegisterNewType(ITypeConverter, 'TypeConverter',
      TTypeConverter, ITypeConverter, GetTypeInfo(IElement));

    RegisterNewType(INullConverter, 'NullConverter',
      TNullConverter, ITypeConverter, GetTypeInfo(ITypeConverter));

    RegisterNewType(IDateTimeToText, 'DateTimeToText',
      TDateTimeToText, ITypeConverter, GetTypeInfo(ITypeConverter));

    RegisterNewType(IBooleanToText, 'BooleanToText',
      TBooleanToText, ITypeConverter, GetTypeInfo(ITypeConverter));

    RegisterNewType(IIntegerToText, 'IntegerToText',
      TIntegerToText, ITypeConverter, GetTypeInfo(ITypeConverter));

    RegisterNewType(IDoubleToText, 'DoubleToText',
      TDoubleToText, ITypeConverter, GetTypeInfo(ITypeConverter));

    RegisterNewType(IObjectToFeatureConverter, 'ObjectToFeatureConverter',
      TObjectToFeatureConverter, ITypeConverter, GetTypeInfo(ITypeConverter));

    // Formaters

    RegisterNewType(IFormatFeature, 'FormatFeature',
      TFormatFeature, IInfraType, GetTypeInfo(IInfraType));

    // Renderers

    with RegisterNewType(IRenderer, 'Renderer',
      TRenderer, IRenderer, GetTypeInfo(IInfraObject)) do
    begin
      AddMember('Converter', TTypeConverter, ITypeConverter,
        ITypeConverter, @TRenderer.GetTypeConverter,
        @TRenderer.SetTypeConverter);
      AddMember('Format', TInfraType, IInfraType, IInfraType,
        @TRenderer.GetFormat, @TRenderer.SetFormat);
      AddMember('View', TView, IView, IView,
        @TRenderer.GetView, @TRenderer.SetView);
    end;

    RegisterNewType(IVCLCustomListBoxItemRenderer,
      'VCLCustomListBoxItemRenderer', TVCLCustomListBoxItemRenderer,
      IRenderer, GetTypeInfo(IRenderer));

    RegisterNewType(IVCLMenuItemRenderer, 'VCLMenuItemRenderer',
      TVCLMenuItemRenderer, IRenderer, GetTypeInfo(IRenderer));

    // Interactors

    with RegisterNewType(IInteractor, 'Interactor',
      TInteractor, IInteractor, GetTypeInfo(IInfraObject)) do
    begin
      AddMember('View', TView, IView, IView,
        @TInteractor.GetView, @TInteractor.SetView);
    end;

    RegisterNewType(IClickEventModelUpdater, 'ClickEventModelUpdater',
      TClickEventModelUpdater, IInteractor, GetTypeInfo(IInteractor));

    RegisterNewType(IObjectSelectionUpdater, 'ObjectSelectionUpdater',
      TObjectSelectionUpdater, IInteractor, GetTypeInfo(IInteractor));

    RegisterNewType(IExitEventModelUpdater, 'ExitEventModelUpdater',
      TExitEventModelUpdater, IInteractor, GetTypeInfo(IInteractor));

    RegisterNewType(IKeyBoardInteractor, 'KeyBoardInteractor',
      TKeyBoardInteractor, IInteractor, GetTypeInfo(IInteractor));

    RegisterNewType(IInputNumberInteractor, 'InputNumberInteractor',
      TInputNumberInteractor, IInteractor, GetTypeInfo(IKeyBoardInteractor));

    // Model

    with RegisterNewType(IModel, 'Model',
      TModel, IInfraType, GetTypeInfo(IInfraObject)) do
    begin
      AddMember('Value', TInfraType, IInfraType, IInfraType,
        @TModel.GetValue, @TModel.SetValue);
      AddMember('Selection', TSelection, ISelection, IInfraType,
        @TModel.GetSelection, nil);
    end;

    // Presenters

    with RegisterNewType(IPresenter, 'Presenter',
      TPresenter, IPresenter,
      GetTypeInfo(IInfraObject)) do
    begin
      AddMember('Model', TModel, IModel, IModel,
        @TPresenter.GetModel, @TPresenter.SetModel);
      AddMember('View', TView, IView, IView,
        @TPresenter.GetView, @TPresenter.SetView);
      AddMember('ParentPresenter', TPresenter, IPresenter,
        IPresenter, @TPresenter.GetParentPresenter,
        @TPresenter.SetParentPresenter);
      // *** ver com solerman se é para registrar isso
      { ***
      AddMember('Name', TPresenter, IPresenter,
        IPresenter, @TPresenter.GetParentPresenter,
        @TPresenter.SetParentPresenter);
      }
    end;

    RegisterNewType(IValuePresenter, 'ValuePresenter',
      TPresenter, IPresenter, GetTypeInfo(IPresenter));

    RegisterNewType(ITextPresenter, 'TextPresenter',
      TTextPresenter, IValuePresenter, GetTypeInfo(IValuePresenter));

    RegisterNewType(IBooleanPresenter, 'BooleanPresenter',
      TBooleanPresenter, IValuePresenter, GetTypeInfo(IValuePresenter));

    RegisterNewType(INumberPresenter, 'NumberPresenter',
      TNumberPresenter, IValuePresenter, GetTypeInfo(IValuePresenter));

    RegisterNewType(IIntegerPresenter, 'IntegerPresenter',
      TIntegerPresenter, IValuePresenter, GetTypeInfo(INumberPresenter));

    RegisterNewType(IDateTimePresenter, 'DateTimePresenter',
      TDateTimePresenter, IValuePresenter, GetTypeInfo(IValuePresenter));

    RegisterNewType(IContainerPresenter, 'ContainerPresenter',
      TContainerPresenter, IPresenter, GetTypeInfo(IPresenter));

    RegisterNewType(ICustomFormPresenter, 'CustomFormPresenter',
      TCustomFormPresenter, IContainerPresenter,
      GetTypeInfo(IContainerPresenter));

    RegisterNewType(IListPresenter, 'ListPresenter',
      TListPresenter, IPresenter, GetTypeInfo(IPresenter));

    RegisterNewType(IListItemPresenter, 'ListItemPresenter',
      TListItemPresenter, IPresenter, GetTypeInfo(IPresenter));

    // Views

    with RegisterNewType(IView, 'View',
      TView, IView, GetTypeInfo(IElement)) do
    begin
      AddMember('Model', TModel, IModel, IModel,
        @TView.GetModel);
      AddMember('Presenter', TPresenter, IPresenter, IPresenter,
        @TView.GetPresenter, @TView.SetPresenter);
      AddMember('Renderer', TView, IRenderer, IRenderer,
        @TView.GetRenderer);
    end;

    with RegisterNewType(IVCLView, 'VCLView',
      TVCLView, IView, GetTypeInfo(IView)) do
      AddMember('VCLObject', TInfraNativeObject, IInfraNativeObject,
        IInfraNativeObject, @TVCLView.GetObject,
        @TVCLView.SetObject);

    RegisterNewType(IVCLControlView, 'VCLControlView',
      TVCLControlView, IVCLView,
      GetTypeInfo(IVCLView));

    RegisterNewType(IVCLWinControlView, 'VCLWinControlView',
      TVCLWinControlView, IVCLView,
      GetTypeInfo(IVCLControlView));

    RegisterNewType(IVCLCustomFormView, 'VCLCustomFormView',
      TVCLCustomFormView, IVCLWinControlView,
      GetTypeInfo(IVCLWinControlView));

    RegisterNewType(IVCLCustomEditView, 'VCLCustomEditView',
      TVCLCustomEditView, IVCLWinControlView,
      GetTypeInfo(IVCLWinControlView));

    RegisterNewType(IVCLEditView, 'VCLEditView',
      TVCLEditView, IVCLWinControlView,
      GetTypeInfo(IVCLCustomEditView));

    RegisterNewType(IVCLCustomLabelView, 'VCLCustomLabelView',
      TVCLCustomLabelView, IVCLWinControlView,
      GetTypeInfo(IVCLControlView));

    RegisterNewType(IVCLButtonControlView, 'VCLButtonControlView',
      TVCLButtonControlView, IVCLWinControlView,
      GetTypeInfo(IVCLWinControlView));

    with RegisterNewType(IVCLCustomCheckBoxView, 'VCLCustomCheckBoxView',
      TVCLCustomCheckBoxView, IVCLButtonControlView,
      GetTypeInfo(IVCLButtonControlView)) do
      AddMember('State', TInfraBoolean, IInfraBoolean,
        IInfraType, @TVCLCustomCheckBoxView.GetState, nil);

    RegisterNewType(IVCLRadioButtonView, 'VCLRadioButtonView',
      TVCLRadioButtonView, IVCLButtonControlView,
      GetTypeInfo(IVCLButtonControlView));

    with RegisterNewType(IVCLButtonView, 'VCLButtonView',
      TVCLButtonView, IVCLButtonControlView,
      GetTypeInfo(IVCLButtonControlView)) do
      AddMember('ClickCommand', ICommand,
        @TVCLButtonView.GetClickCommand,
        @TVCLButtonView.SetClickCommand);

    RegisterNewType(IVCLCustomDateTimeView, 'VCLCustomDateTimeView',
      TVCLCustomDateTimeView, IVCLWinControlView,
      GetTypeInfo(IVCLWinControlView));

    RegisterNewType(IVCLCustomListBoxView, 'VCLCustomListBoxView',
      TVCLCustomListBoxView, IVCLWinControlView,
      GetTypeInfo(IVCLWinControlView));

    RegisterNewType(IVCLCustomListBoxItemView, 'VCLCustomListBoxItemView',
      TVCLCustomListBoxItemView, IVCLWinControlView,
      GetTypeInfo(IView));

    {TInfraMenuView}

    {TInfraMenuItemView}

  end;
end;

initialization
  RegisterTypeInfo_MVP;

end.
