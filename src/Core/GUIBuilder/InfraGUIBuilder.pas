unit InfraGUIBuilder;

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, InfraBase, InfraCommon,
  InfraCommonIntf, InfraValueType, InfraValueTypeIntf, LayoutManager, StdCtrls,
  InfraGUIBuilderIntf, ComCtrls, Mask, ExtCtrls, InfraValueTypeConvert,
  InfraGUIBuilderForm, GUIAnnotationIntf, List_GUIControl;

type

  TGUI = class(TBaseElement, IGUI)
  private
    FBusinessObject: IInfraObject;
    FGUIControlList: IGUIControlList;
    FName: string;
    FScreen: IScreen;
    FTitle: string;
    function GetBusinessObject: IInfraObject;
    function GetGUIControlList: IGUIControlList;
    function GetName: string;
    function GetScreen: IScreen;
    function GetTitle: string;
    procedure SetBusinessObject(const Value: IInfraObject);
    procedure SetGUIControlList(const Value: IGUIControlList);
    procedure SetName(const Value: string);
    procedure SetScreen(const Value: IScreen);
    procedure SetTitle(const Value: string);
  public
    constructor Create; override;
    function Clone: IGUI;
    function FindGUIControl(pPropertyName : string): IGUIControl;
    function GetConfigurationFileName: string;    
    property BusinessObject: IInfraObject read GetBusinessObject write SetBusinessObject;
    property GUIControlList: IGUIControlList read GetGUIControlList write SetGUIControlList;
    property Name: string read GetName write SetName;    
    property Screen: IScreen read GetScreen write SetScreen;
    property Title: string read GetTitle write SetTitle;
  end;

  TGUIControl = class(TBaseElement, IGUIControl)
  private
    FControl: TControl;
    FControlClass: TControlClass;
    FControlProperty: string;
    FItem: TLayoutManagerItem;
    FName: string;
    FPropertyInfo: IPropertyInfo;
    FPropertyName: string;
    FPropertyValue: IInfraType;
    FScreenItem: IScreenItem;
    function GetControl: TControl;
    function GetControlClass: TControlClass;
    function GetControlProperty: string;
    function GetItem: TLayoutManagerItem;
    function GetName: string;
    function GetPropertyInfo: IPropertyInfo;
    function GetPropertyName: string;
    function GetPropertyValue: IInfraType;
    function GetScreenItem: IScreenItem;
    procedure SetControl(const Value: TControl);
    procedure SetControlClass(const Value: TControlClass);
    procedure SetControlProperty(const Value: string);
    procedure SetItem(const Value: TLayoutManagerItem);
    procedure SetName(const Value: string);
    procedure SetPropertyInfo(const Value: IPropertyInfo);
    procedure SetPropertyName(const Value: string);
    procedure SetPropertyValue(const Value: IInfraType);
    procedure SetScreenItem(const Value: IScreenItem);
  public
    function Clone: IGUIControl;
    property Control: TControl read GetControl write SetControl;
    property ControlClass: TControlClass read GetControlClass  write SetControlClass;
    property ControlProperty: string read GetControlProperty write SetControlProperty;    
    property Item: TLayoutManagerItem read GetItem write SetItem;
    property Name: string read GetName write SetName;
    property PropertyInfo: IPropertyInfo read GetPropertyInfo write SetPropertyInfo;
    property PropertyName: string read GetPropertyName write SetPropertyName;
    property PropertyValue: IInfraType read GetPropertyValue write SetPropertyValue;
    property ScreenItem: IScreenItem read GetScreenItem write SetScreenItem;
  end;

  TGUIMapping = class(TBaseElement, IGUIMapping)
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

  TInfraGUIService = class(TBaseElement, IInfraGUIService)
  private
    FGUIMappings: IGUIMappingList;
    FUserRepository: string;
    function FindGUIControl(pGUIControlList: IGUIControlList;
      pPropertyName: string): IGUIControl;
    function GetControlProperty(pPropertyID: TGUID;
      pControlInfo: IScreenControl = nil): string;
    function GetControlClass(pControlInfo: IScreenControl;
      pTypeID: TGUID): TControlClass;
    function GetGUIMapping(pTypeInfo: TGUID): IGUIMapping;
    function GetGUIMappings: IGUIMappingList;
    function GetUserRepository: string;
    function PrepareGUIInfo(pObject: IInfraObject;
      pScreen: IScreen = nil): IGUI;
    procedure SetUserRepository(const Value: string);
  protected
    property GUIMappings: IGUIMappingList read GetGUIMappings;
  public
    function Build(pObject: IInfraObject; pScreen: IScreen = nil): TGUIResult;
    procedure RegisterGUIMapping(pControlClass: TControlClass; pTypeInfo: TGUID;
      pControlProperty: string = '');
    property UserRepository: string read GetUserRepository write SetUserRepository;
  end;

implementation

uses
  List_GUIMapping, GUIAnnotation;

{ TGUI }

function TGUI.Clone: IGUI;
begin
  Result := TGUI.Create;

  Result.BusinessObject := BusinessObject;
  Result.GUIControlList := GUIControlList.Clone;

  if Assigned(Screen) then  
    Result.Screen := Screen.Clone
  else
    Result.Screen := TScreen.Create;

  Result.Title := Title;
end;

constructor TGUI.Create;
begin
  inherited;

  FGUIControlList := TGUIControlList.Create;
end;

function TGUI.FindGUIControl(pPropertyName : string): IGUIControl;
var
  It: IGUIControlIterator;
begin
  Result := nil;

  It := GUIControlList.NewIterator;

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

function TGUI.GetBusinessObject: IInfraObject;
begin
  Result := FBusinessObject;
end;

function TGUI.GetConfigurationFileName: string;
begin
  if Assigned(BusinessObject) then
  begin
    Result := GUIDToString(BusinessObject.TypeInfo.TypeID);

    if Assigned(Screen) and (Length(Screen.Name) > 0) then
      Result := Result + '_' + Screen.Name;
  end;
end;

function TGUI.GetGUIControlList: IGUIControlList;
begin
  Result := FGUIControlList;
end;

function TGUI.GetName: string;
begin
  Result := FName;
end;

function TGUI.GetScreen: IScreen;
begin
  Result := FScreen;
end;

function TGUI.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TGUI.SetBusinessObject(const Value: IInfraObject);
begin
  FBusinessObject := Value;
end;

procedure TGUI.SetGUIControlList(const Value: IGUIControlList);
begin
  FGUIControlList := Value;
end;

procedure TGUI.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TGUI.SetScreen(const Value: IScreen);
begin
  FScreen := Value;
end;

procedure TGUI.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

{ TGUIControl }

function TGUIControl.Clone: IGUIControl;
begin
  Result := TGUIControl.Create;

  Result.Control := Control;
  Result.ControlClass := ControlClass;
  Result.ControlProperty := ControlProperty;
  Result.Item := Item;
  Result.Name := Name;
  Result.PropertyInfo := PropertyInfo;
  Result.PropertyName := PropertyName;
  Result.PropertyValue := PropertyValue.Clone;

  if Assigned(ScreenItem) then
  begin
    if Supports(ScreenItem, IScreenControl) then
      Result.ScreenItem := (ScreenItem as IScreenControl).Clone
    else if Supports(ScreenItem, IScreenGroup) then
      Result.ScreenItem := (ScreenItem as IScreenGroup).Clone
    else
      Result.ScreenItem := ScreenItem.CloneItem;
  end;
end;

function TGUIControl.GetControl: TControl;
begin
  Result := FControl;
end;

function TGUIControl.GetControlClass: TControlClass;
begin
  Result := FControlClass;
end;

function TGUIControl.GetControlProperty: string;
begin
  Result := FControlProperty;
end;

function TGUIControl.GetItem: TLayoutManagerItem;
begin
  Result := FItem;
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

function TGUIControl.GetScreenItem: IScreenItem;
begin
  Result := FScreenItem;
end;

procedure TGUIControl.SetControl(const Value: TControl);
begin
  FControl := Value;
end;

procedure TGUIControl.SetControlClass(const Value: TControlClass);
begin
  FControlClass := Value;
end;

procedure TGUIControl.SetControlProperty(const Value: string);
begin
  FControlProperty := Value;
end;

procedure TGUIControl.SetItem(const Value: TLayoutManagerItem);
begin
  FItem := Value;
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

procedure TGUIControl.SetScreenItem(const Value: IScreenItem);
begin
  FScreenItem := Value;
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

function TInfraGUIService.Build(pObject: IInfraObject; pScreen: IScreen = nil): TGUIResult;
var
  lGUI: IGUI;
  lForm: TInfraGUIBuilderEditForm;
begin
  lGUI := PrepareGUIInfo(pObject, pScreen);

  lForm := TInfraGUIBuilderEditForm.CreateNew(nil);
  try
    lForm.GUI := lGUI;
    lForm.Build;
    Result := lForm.Execute;
  finally
    lForm.Free;
  end;
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

function TInfraGUIService.GetUserRepository: string;
begin
  Result := FUserRepository;
end;

function TInfraGUIService.PrepareGUIInfo(pObject: IInfraObject;
  pScreen: IScreen): IGUI;
var
  ItMember: IMemberInfoIterator;
  ItScreenItem: IScreenItemIterator;
  lItem: IGUIControl;
  lScreenControl: IScreenControl;
begin
  Result := TGUI.Create;
  Result.BusinessObject := pObject;
  Result.Name := pObject.TypeInfo.Name;
  Result.Title := pObject.TypeInfo.Name;
  Result.Screen := pScreen;

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
          lItem.ScreenItem := pScreen.GetControl(lItem.PropertyName);

        lItem.ControlClass := GetControlClass(lItem.ScreenItem as IScreenControl,
          lItem.PropertyInfo.TypeInfo.TypeID);

        if (Assigned(lItem)) and (Supports(lItem.ScreenItem, IScreenControl)) then
          lItem.ControlProperty := GetControlProperty(
            lItem.PropertyInfo.TypeInfo.TypeID, lItem.ScreenItem as IScreenControl)
        else
          lItem.ControlProperty := GetControlProperty(
            lItem.PropertyInfo.TypeInfo.TypeID, nil);

        //If does not have ControlClass, it cannot be added into screen
        if Assigned(lItem.ControlClass) then
          Result.GUIControlList.Add(lItem);
      end;
    end;

    ItMember.Next;
  end;

  //Get additional properties which were added in screen
  if Assigned(pScreen) then
  begin
    Result.Name := pScreen.Name;

    ItScreenItem := pScreen.Items.NewIterator;

    while not ItScreenItem.IsDone do
    begin
      if Supports(ItScreenItem.CurrentItem, IScreenControl) then
      begin
        lScreenControl := ItScreenItem.CurrentItem as IScreenControl;

        //Test if property was not added previously and if it could be used
        if (not Assigned(FindGUIControl(Result.GUIControlList, lScreenControl.PropertyName)))
          and (pScreen.UseProperty(lScreenControl.PropertyName)) then
        begin
          lItem := TGUIControl.Create;
          lItem.Name := lScreenControl.Name;
          lItem.PropertyName := lScreenControl.PropertyName;
          lItem.ScreenItem := lScreenControl;
          lItem.PropertyInfo := pObject.TypeInfo.GetPropertyInfo(lItem.PropertyName);
          lItem.PropertyValue := pObject.TypeInfo.GetProperty(pObject,
            lItem.PropertyName) as IInfraType;
          lItem.ControlClass := GetControlClass(lItem.ScreenItem as IScreenControl,
            lItem.PropertyInfo.TypeInfo.TypeID);
          lItem.ControlProperty := GetControlProperty(lItem.PropertyInfo.TypeInfo.TypeID,
            lItem.ScreenItem as IScreenControl);

          //If does not have ControlClass, it cannot be added into screen
          if Assigned(lItem.ControlClass) then
             Result.GUIControlList.Add(lItem);
        end;
      end;

      ItScreenItem.Next;
    end;
  end;
end;

procedure TInfraGUIService.RegisterGUIMapping(pControlClass: TControlClass;
  pTypeInfo: TGUID; pControlProperty: string);
var
  lGUIMapping: IGUIMapping;
begin
  lGUIMapping := GetGUIMapping(pTypeInfo);

  if Assigned(lGUIMapping) then
  begin
    lGUIMapping.ControlClass := pControlClass;
    lGUIMapping.TypeInfo := pTypeInfo;
    lGUIMapping.ControlProperty := pControlProperty;
  end
  else
    GUIMappings.Add(TGUIMapping.Create(pControlClass, pTypeInfo, pControlProperty));
end;

procedure TInfraGUIService.SetUserRepository(const Value: string);
begin
  FUserRepository := Value;
end;

procedure InjectGUIService;
begin
  (ApplicationContext as IBaseElement).Inject(
    IInfraGUIService, TInfraGUIService.Create);
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
