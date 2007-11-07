unit InfraGUIBuilder;

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, InfraBase, InfraCommon,
  InfraCommonIntf, InfraValueType, InfraValueTypeIntf, LayoutManager, StdCtrls,
  InfraGUIBuilderIntf, ComCtrls, Mask, ExtCtrls, typinfo, InfraValueTypeConvert,
  InfraGUIBuilderForm, GUIAnnotationIntf, List_GUIControl;

type

  TGUIControl = class(TMemoryManagedObject, IGUIControl)
  private
    FControlClass: TControlClass;
    FName: string;
    FPropertyInfo: IPropertyInfo;
    FPropertyName: string;
    FPropertyValue: IInfraType;
    FScreenControl: IScreenControl;
    function GetName: string;
    function GetPropertyInfo: IPropertyInfo;
    function GetPropertyName: string;
    function GetPropertyValue: IInfraType;
    function GetScreenControl: IScreenControl;
    procedure SetName(const Value: string);
    procedure SetPropertyInfo(const Value: IPropertyInfo);
    procedure SetPropertyName(const Value: string);
    procedure SetPropertyValue(const Value: IInfraType);
    procedure SetScreenControl(const Value: IScreenControl);
    function GetControlClass: TControlClass;
    procedure SetControlClass(const Value: TControlClass);
  public
    property ControlClass: TControlClass read GetControlClass  write SetControlClass;
    property Name: string read GetName write SetName;
    property PropertyInfo: IPropertyInfo read GetPropertyInfo write SetPropertyInfo;
    property PropertyName: string read GetPropertyName write SetPropertyName;
    property PropertyValue: IInfraType read GetPropertyValue write SetPropertyValue;
    property ScreenControl: IScreenControl read GetScreenControl write SetScreenControl;
  end;

  TGUIMapping = class(TMemoryManagedObject, IGUIMapping)
  private
    FControlClass: TControlClass;
    FControlProperty: string;
    FTypeInfo: TGUID;
    function GetControlClass: TControlClass;
    function GetControlProperty: string;
    function GetTypeInfo: TGUID;
    procedure SetControlClass(const Value: TControlClass);
    procedure SetControlProperty(const Value: string);
    procedure SetTypeInfo(const Value: TGUID);
  protected
    property ControlClass: TControlClass read GetControlClass write SetControlClass;
    property ControlProperty: string read GetControlProperty write SetControlProperty;
    property TypeInfo: TGUID read GetTypeInfo write SetTypeInfo;
  public
    constructor Create(pControlClass: TControlClass; pTypeInfo: TGUID;
      pControlProperty: string = ''); reintroduce;
  end;

  TInfraGUIService = class(TMemoryManagedObject, IInfraGUIService)
  private
    FAdjustFormSize: Boolean;
    FGUIMappings: IGUIMappingList;
    function FindGUIControl(pGUIControlList: IGUIControlList;
      pPropertyName: string): IGUIControl;
    function GetControlProperty(pPropertyID: TGUID;
      pControlInfo: IScreenControl = nil): string;
    function GetControlClass(pControlInfo: IScreenControl;
      pTypeID: TGUID): TControlClass;
    function GetGUIControlList(pObject: IInfraObject;
      pScreen: IScreen = nil): IGUIControlList;
    function GetGUIMapping(pTypeInfo: TGUID): IGUIMapping;
    function GetGUIMappings: IGUIMappingList;
    function GetVariantValue(pValue: IInfraType;
      pObject: IInfraObject): IInfraVariant;
    procedure SetControlValue(pControl: TControl;
      pControlProperty: string; pValue: IInfraType; pObject: IInfraObject);
    procedure SetPropAnnotationsForItem(pControlInfo: IScreenControl;
      pItem: TLayoutManagerItem);
    procedure SetObjectAnnotationsForForm(pObjectInfo: IClassInfo;
      pScreen: IScreen; pForm: TInfraGUIBuilderForm);
    procedure SetItemOrder(pScreen: IScreen; pLayoutManager: TLayoutManager);
  protected
    property GUIMappings: IGUIMappingList read GetGUIMappings;
  public
    procedure Build(pObject: IInfraObject; pScreen: IScreen = nil);
    constructor Create; override;
    procedure RegisterGUIMapping(pControlClass: TControlClass; pTypeInfo: TGUID;
      pControlProperty: string = '');
  end;

implementation

uses
  List_GUIMapping;

{ TGUIControl }

function TGUIControl.GetControlClass: TControlClass;
begin
  Result := FControlClass;
end;

function TGUIControl.GetName: string;
begin
  Result := FName;
end;

function TGUIControl.GetPropertyInfo: IPropertyInfo;
begin
  Result := FPropertyInfo;
end;

function TGUIControl.GetPropertyName: string;
begin
  Result := FPropertyName;
end;

function TGUIControl.GetPropertyValue: IInfraType;
begin
  Result := FPropertyValue;
end;

function TGUIControl.GetScreenControl: IScreenControl;
begin
  Result := FScreenControl;
end;

procedure TGUIControl.SetControlClass(const Value: TControlClass);
begin
  FControlClass := Value;
end;

procedure TGUIControl.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TGUIControl.SetPropertyInfo(const Value: IPropertyInfo);
begin
  FPropertyInfo := Value;
end;

procedure TGUIControl.SetPropertyName(const Value: string);
begin
  FPropertyName := Value;
end;

procedure TGUIControl.SetPropertyValue(const Value: IInfraType);
begin
  FPropertyValue := Value;
end;

procedure TGUIControl.SetScreenControl(const Value: IScreenControl);
begin
  FScreenControl := Value;
end;

{ TGUIMapping }

constructor TGUIMapping.Create(pControlClass: TControlClass; pTypeInfo: TGUID;
  pControlProperty: string);
begin
  FControlClass := pControlClass;
  FTypeInfo := pTypeInfo;
  FControlProperty := pControlProperty;
end;

function TGUIMapping.GetControlClass: TControlClass;
begin
  Result := FControlClass;
end;

function TGUIMapping.GetControlProperty: string;
begin
  Result := FControlProperty;
end;

function TGUIMapping.GetTypeInfo: TGUID;
begin
  Result := FTypeInfo;
end;

procedure TGUIMapping.SetControlClass(const Value: TControlClass);
begin
  FControlClass := Value;
end;

procedure TGUIMapping.SetControlProperty(const Value: string);
begin
  FControlProperty := Value;
end;

procedure TGUIMapping.SetTypeInfo(const Value: TGUID);
begin
  FTypeInfo := Value;
end;

{ TInfraGUIService }

procedure TInfraGUIService.Build(pObject: IInfraObject; pScreen: IScreen = nil);
var
  It: IGUIControlIterator;
  lGUIControlList: IGUIControlList;
  lForm: TInfraGUIBuilderEditForm;
  lControl: TControl;
  lControlClass: TControlClass;
  lItem: TLayoutManagerItem;
  lGUIControl: IGUIControl;
  sControlProperty: string;
begin
  //Form creation
  lForm := TInfraGUIBuilderEditForm.CreateNew(nil);
  lForm.Caption := pObject.TypeInfo.Name;

  //If object has one screen annotated for it
  if Assigned(pScreen) then
    SetObjectAnnotationsForForm(pObject.TypeInfo, pScreen, lForm);

  lGUIControlList := GetGUIControlList(pObject, pScreen);
  lForm.GUIControlList := lGUIControlList;

  It := lGUIControlList.NewIterator;

  while not It.IsDone do
  begin
    lGUIControl := It.CurrentItem as IGUIControl;

    //If there are ControlClass information for this property
    if Assigned(lGUIControl.ControlClass) then
    begin
      lControl := lGUIControl.ControlClass.Create(lForm);
      lControl.Name := lGUIControl.Name;
      lItem := lForm.MainLayoutManager.AddControl(lControl);

      sControlProperty := GetControlProperty(
        lGUIControl.PropertyInfo.TypeInfo.TypeID, lGUIControl.ScreenControl);

      //If control has ControlProperty, property data will be set to it
      SetControlValue(lControl, sControlProperty, lGUIControl.PropertyValue, pObject);

      //Set property annotations for layout manager item
      SetPropAnnotationsForItem(lGUIControl.ScreenControl, lItem);
    end;

    It.Next;
  end;

  SetItemOrder(pScreen, lForm.MainLayoutManager);

  if FAdjustFormSize then
    lForm.AdjustFormSize;

  lForm.ShowModal;

  lForm.Free;
end;

constructor TInfraGUIService.Create;
begin
  inherited;

  FAdjustFormSize := True;
end;

function TInfraGUIService.FindGUIControl(pGUIControlList: IGUIControlList;
  pPropertyName: string): IGUIControl;
var
  It: IGUIControlIterator;
begin
  Result := nil;

  It := pGUIControlList.NewIterator;

  while not It.IsDone do
  begin
    if SameText((It.CurrentItem as IGUIControl).PropertyName, pPropertyName) then
    begin
      Result := It.CurrentItem as IGUIControl;
      Break;
    end;

    It.Next;
  end;
end;

function TInfraGUIService.GetControlProperty(pPropertyID: TGUID;
  pControlInfo: IScreenControl = nil): string;
var
  lGUIMapping: IGUIMapping;
begin
  if (Assigned(pControlInfo)) and (not pControlInfo.ControlProperty.IsNull) then
    Result := pControlInfo.ControlProperty.AsString
  else
  begin
    lGUIMapping := GetGUIMapping(pPropertyID);

    if Assigned(lGUIMapping) then
      Result := lGUIMapping.ControlProperty;
  end;
end;

function TInfraGUIService.GetControlClass(
  pControlInfo: IScreenControl; pTypeID: TGUID): TControlClass;
var
  lGUIMapping: IGUIMapping;
begin
  Result := nil;

  if (Assigned(pControlInfo)) and (Assigned(pControlInfo.ControlClass)) then
    Result := pControlInfo.ControlClass
  else
  begin
    lGUIMapping := GetGUIMapping(pTypeID);

    if Assigned(lGUIMapping) then
      Result := lGUIMapping.ControlClass;
  end;
end;

function TInfraGUIService.GetGUIControlList(pObject: IInfraObject;
  pScreen: IScreen): IGUIControlList;
var
  ItMember: IMemberInfoIterator;
  ItScreenItem: IScreenItemIterator;
  lItem: IGUIControl;
  lScreenControl: IScreenControl;
begin
  Result := TGUIControlList.Create;

  //Get all object properties
  ItMember := pObject.TypeInfo.FindMembers([mtProperty]);

  while not ItMember.IsDone do
  begin
    if Supports(ItMember.CurrentItem, IPropertyInfo) then
    begin
      if (not Assigned(pScreen)) or ((Assigned(pScreen)) and
        (pScreen.UseProperty((ItMember.CurrentItem as IPropertyInfo).Name))) then
      begin
        lItem := TGUIControl.Create;
        lItem.Name := (ItMember.CurrentItem as IPropertyInfo).Name;
        lItem.PropertyName := (ItMember.CurrentItem as IPropertyInfo).Name;
        lItem.PropertyValue := pObject.TypeInfo.GetProperty(pObject,
          lItem.PropertyName) as IInfraType;
        lItem.PropertyInfo := pObject.TypeInfo.GetPropertyInfo(lItem.PropertyName);

        if Assigned(pScreen) then
          lItem.ScreenControl := pScreen.GetControl(lItem.PropertyName);

        lItem.ControlClass := GetControlClass(lItem.ScreenControl,
          lItem.PropertyInfo.TypeInfo.TypeID);

        Result.Add(lItem);
      end;
    end;

    ItMember.Next;
  end;

  //Get additional properties which were added in screen
  if Assigned(pScreen) then
  begin
    ItScreenItem := pScreen.Items.NewIterator;

    while not ItScreenItem.IsDone do
    begin
      if Supports(ItScreenItem.CurrentItem, IScreenControl) then
      begin
        lScreenControl := ItScreenItem.CurrentItem as IScreenControl;

        //Test if property was not added previously and if it could be used
        if (not Assigned(FindGUIControl(Result, lScreenControl.PropertyName)))
          and (pScreen.UseProperty(lScreenControl.PropertyName)) then
        begin
          lItem := TGUIControl.Create;
          lItem.Name := lScreenControl.Name;
          lItem.PropertyName := lScreenControl.PropertyName;
          lItem.ScreenControl := lScreenControl;
          lItem.PropertyInfo := pObject.TypeInfo.GetPropertyInfo(lItem.PropertyName);
          lItem.PropertyValue := pObject.TypeInfo.GetProperty(pObject,
            lItem.PropertyName) as IInfraType;
          lItem.ControlClass := GetControlClass(lItem.ScreenControl,
            lItem.PropertyInfo.TypeInfo.TypeID);

          Result.Add(lItem);
        end;
      end;

      ItScreenItem.Next;
    end;
  end;
end;

function TInfraGUIService.GetGUIMapping(pTypeInfo: TGUID): IGUIMapping;
var
  It: IGUIMappingIterator;
begin
  Result := nil;

  It := GUIMappings.NewIterator;

  while not It.IsDone do
  begin
    if IsEqualGUID((It.CurrentItem as IGUIMapping).TypeInfo, pTypeInfo) then
    begin
      Result := It.CurrentItem as IGUIMapping;
      Break;
    end;

    It.Next;
  end;
end;

function TInfraGUIService.GetGUIMappings: IGUIMappingList;
begin
  if not Assigned(FGUIMappings) then
    FGUIMappings := TGUIMappingList.Create;
  Result := FGUIMappings;
end;

function TInfraGUIService.GetVariantValue(pValue: IInfraType;
  pObject: IInfraObject): IInfraVariant;
var
  lTypeConverter: ITypeConverter;
begin
  if Supports(pValue, IInfraString) then
    lTypeConverter := TStringToVariant.Create
  else if Supports(pValue, IInfraBoolean) then
    lTypeConverter := TBooleanToVariant.Create
  else if Supports(pValue, IInfraDateTime) then
    lTypeConverter := TDateTimeToVariant.Create
  else if Supports(pValue, IInfraDate) then
    lTypeConverter := TDateTimeToVariant.Create
  else if Supports(pValue, IInfraTime) then
    lTypeConverter := TDateTimeToVariant.Create
  else if Supports(pValue, IInfraDouble) then
    lTypeConverter := TDoubleToVariant.Create
  else if Supports(pValue, IInfraInteger) then
    lTypeConverter := TIntegerToVariant.Create;

  if Supports(pValue, IInfraVariant) then
    Result := pValue as IInfraVariant
  else
    Result := lTypeConverter.ConvertToRight(
      pValue) as IInfraVariant;
end;

procedure TInfraGUIService.SetControlValue(pControl: TControl;
  pControlProperty: string; pValue: IInfraType; pObject: IInfraObject);
begin
  if pControl is TMemo then
    (pControl as TMemo).Lines.Text := GetVariantValue(pValue, pObject).AsVariant
  else if Length(pControlProperty) > 0 then
    SetPropValue(pControl, pControlProperty,
      GetVariantValue(pValue, pObject).AsVariant);
end;

procedure TInfraGUIService.SetItemOrder(pScreen: IScreen;
  pLayoutManager: TLayoutManager);
var
  ItScreenItem: IScreenItemIterator;
  lCurControl, lNewControl: IScreenControl;
  iCurIndex, iNewIndex: Integer;
begin
  if not Assigned(pScreen) then
    Exit;

  ItScreenItem := pScreen.Items.NewIterator;

  while not ItScreenItem.IsDone do
  begin
    iCurIndex := -1;
    iNewIndex := -1;

    //Set order for ScreenControl items
    if Supports(ItScreenItem.CurrentItem, IScreenControl) then
    begin
      lCurControl := ItScreenItem.CurrentItem as IScreenControl;
      iCurIndex := pLayoutManager.GetItemIndexByControlName(lCurControl.Name);

      if Length(lCurControl.GetPutBefore) > 0 then
      begin
        lNewControl := pScreen.GetControl(lCurControl.GetPutBefore);

        Assert(Assigned(lNewControl), 'PutBefore property: ' +
          lCurControl.GetPutBefore + ', does not exists');

        iNewIndex := pLayoutManager.GetItemIndexByControlName(lNewControl.Name);
      end
      else if Length(lCurControl.GetPutAfter) > 0 then
      begin
        lNewControl := pScreen.GetControl(lCurControl.GetPutAfter);

        Assert(Assigned(lNewControl), 'PutAfter property: ' +
          lCurControl.GetPutAfter + ', does not exists');

        iNewIndex := pLayoutManager.GetItemIndexByControlName(lNewControl.Name);

        if (iNewIndex > -1) and (iNewIndex < pLayoutManager.ItemList.Count - 1) then
          Inc(iNewIndex);
      end;
    end;

    if (iCurIndex > -1) and (iNewIndex > -1) then
      pLayoutManager.ItemList.Move(iCurIndex, iNewIndex);

    ItScreenItem.Next;
  end;
end;

procedure TInfraGUIService.SetObjectAnnotationsForForm(pObjectInfo: IClassInfo;
  pScreen: IScreen; pForm: TInfraGUIBuilderForm);
begin
  if not pScreen.Caption.IsNull then
    pForm.Caption := pScreen.Caption.AsString;

  if not pScreen.Height.IsNull then
  begin
    pForm.Height := pScreen.Height.AsInteger;
    FAdjustFormSize := False;
  end;

  if not pScreen.Width.IsNull then
  begin
    pForm.Width := pScreen.Width.AsInteger;
    FAdjustFormSize := False;
  end;

  if pScreen.CaptionPosition <> pForm.MainLayoutManager.ItemDefCaptionPos then
    pForm.MainLayoutManager.ItemDefCaptionPos := pScreen.CaptionPosition;

  if not pScreen.ControlSpacing.IsNull then
    pForm.MainLayoutManager.ItemDefControlSpacing :=
      pScreen.ControlSpacing.AsInteger;

  if not TLayoutManagerPositions.Equals(pScreen.Padding,
    pForm.MainLayoutManager.ItemDefPadding) then
    pForm.MainLayoutManager.ItemDefPadding.Assign(pScreen.Padding);

  if pScreen.ItemLayout <> pForm.MainLayoutManager.ItemLayout then
    pForm.MainLayoutManager.ItemLayout := pScreen.ItemLayout;

  if not TLayoutManagerPositions.Equals(pScreen.ItemSpacing,
    pForm.MainLayoutManager.ItemSpacing) then
    pForm.MainLayoutManager.ItemSpacing.Assign(pScreen.ItemSpacing);
end;

procedure TInfraGUIService.SetPropAnnotationsForItem(
  pControlInfo: IScreenControl; pItem: TLayoutManagerItem);
begin
  if not Assigned(pControlInfo) then
    Exit;

  if not pControlInfo.Caption.IsNull then
    pItem.Caption := pControlInfo.Caption.AsString;

  if pControlInfo.CaptionPositionChanged then
    pItem.CaptionOptions.Position := pControlInfo.CaptionPosition;

  if not pControlInfo.CaptionVisible.IsNull then
    pItem.CaptionVisible := pControlInfo.CaptionVisible.AsBoolean;

  if not pControlInfo.Height.IsNull then
  begin
    pItem.ItemControl.Height := pControlInfo.Height.AsInteger;
    pItem.ResizeItemHeight;
  end;

  if pControlInfo.ItemHeightMeasureTypeChanged then
    pItem.HeightOptions.MeasureType := pControlInfo.ItemHeightMeasureType;

  if not pControlInfo.ItemHeight.IsNull then
    pItem.HeightOptions.Size := pControlInfo.ItemHeight.AsInteger;

  if pControlInfo.ItemWidthMeasureTypeChanged then
    pItem.WidthOptions.MeasureType := pControlInfo.ItemWidthMeasureType;

  if not pControlInfo.ItemWidth.IsNull then
    pItem.WidthOptions.Size := pControlInfo.ItemWidth.AsInteger;

  if not pControlInfo.Visible.IsNull then
    pItem.Visible := pControlInfo.Visible.AsBoolean;

  if not pControlInfo.Width.IsNull then
  begin
    pItem.ItemControl.Width := pControlInfo.Width.AsInteger;
    pItem.ResizeItemWidth;
  end;
end;

procedure TInfraGUIService.RegisterGUIMapping(pControlClass: TControlClass;
  pTypeInfo: TGUID; pControlProperty: string);
begin
  if Assigned(GetGUIMapping(pTypeInfo)) then
    raise Exception.Create('GUI mapping already registered for type: ' +
      GUIDToString(pTypeInfo))
  else
    GUIMappings.Add(TGUIMapping.Create(pControlClass, pTypeInfo, pControlProperty));
end;

procedure InjectGUIService;
begin
  (ApplicationContext as IMemoryManagedObject).Inject(
    IInfraGUIService, TInfraGUIService.Create as IInfraGUIService);
end;

procedure RegisterGUIMappings;
begin
  with GUIService do
  begin
    RegisterGUIMapping(TEdit, IInfraString, 'Text');
    RegisterGUIMapping(TEdit, IInfraVariant, 'Text');
    RegisterGUIMapping(TCheckBox, IInfraBoolean, 'Checked');
    RegisterGUIMapping(TDateTimePicker, IInfraDateTime, 'Date');
    RegisterGUIMapping(TDateTimePicker, IInfraDate, 'Date');
    RegisterGUIMapping(TDateTimePicker, IInfraTime, 'Date');
    RegisterGUIMapping(TMaskEdit, IInfraDouble, 'Text');
    RegisterGUIMapping(TMaskEdit, IInfraInteger, 'Text');
  end;
end;

initialization
  InjectGUIService;
  RegisterGUIMappings;

end.
