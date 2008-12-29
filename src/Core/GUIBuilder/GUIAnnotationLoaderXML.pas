unit GUIAnnotationLoaderXML;

interface

uses
  SysUtils, Classes, Contnrs, InfraCommon, GUIAnnotationLoaderIntf, ExtCtrls,
  InfraGUIBuilderIntf, ComObj, XMLIntf, XMLDoc, Forms, msxml, Variants,
  GUIAnnotationIntf;

type

  TGUIAnnotationLoaderXML = class(TBaseElement, IGUIAnnotationLoader)
  private
    FFileName: string;
    FGUI: IGUI;
    function GetRepository(CanCreate: Boolean = True): string;
    function GetFileName: string;
    function GetGUI: IGUI;
    procedure SetFileName(const Value: string);
    procedure SetGUI(const Value: IGUI);
  public
    procedure Load;
    procedure Save;
    property FileName: string read GetFileName write SetFileName;
    property GUI: IGUI read GetGUI write SetGUI;
  end;

implementation

uses
  GUIAnnotation, LayoutManager;

{ TGUIAnnotationLoaderXML }

function TGUIAnnotationLoaderXML.GetFileName: string;
begin
  Result := FFileName;
end;

function TGUIAnnotationLoaderXML.GetGUI: IGUI;
begin
  Result := FGUI;
end;

function TGUIAnnotationLoaderXML.GetRepository(CanCreate: Boolean): string;
var
  sDir: string;
begin
  if Length(GUIService.UserRepository) > 0 then
    sDir := GUIService.UserRepository
  else
    sDir := ExtractFileDir(Application.ExeName) + '\Screens';

  if (CanCreate) and (not DirectoryExists(sDir)) then
    CreateDir(sDir);

  Result := sDir + '\' + FileName;
end;

procedure TGUIAnnotationLoaderXML.Load;
var
  I : Integer;
  XMLDoc: TXMLDocument;
  ANode, AChild, AChildNode: IXMLNode;
  lGUIControl: IGUIControl;
  lScreenItem: IScreenItem;
begin
  if not FileExists(GetRepository(False)) then
    Exit;

  if not Assigned(GUI.Screen) then
    GUI.Screen := TScreen.Create;

  XMLDoc:= TXMLDocument.Create(Application);

  try
    XMLDoc.Active:= False;
    XMLDoc.LoadFromFile(GetRepository);

    //Title
    ANode := XMLDoc.DocumentElement.ChildNodes.Nodes['Title'];
    if (Assigned(ANode)) and (not (ANode.NodeValue = null)) then
      GUI.Screen.Title.AsString := ANode.NodeValue;

    //CaptionPosition
    ANode := XMLDoc.DocumentElement.ChildNodes.Nodes['CaptionPosition'];
    if (Assigned(ANode)) and (not (ANode.NodeValue = null)) then
      GUI.Screen.CaptionPosition := GetLabelPosition(ANode.NodeValue);

    //Height
    ANode := XMLDoc.DocumentElement.ChildNodes.Nodes['Height'];
    if (Assigned(ANode)) and (not (ANode.NodeValue = null)) then
      GUI.Screen.Height.AsInteger := ANode.NodeValue;

    //Width
    ANode := XMLDoc.DocumentElement.ChildNodes.Nodes['Width'];
    if (Assigned(ANode)) and (not (ANode.NodeValue = null)) then
      GUI.Screen.Width.AsInteger := ANode.NodeValue;

    //ItemLayout
    ANode := XMLDoc.DocumentElement.ChildNodes.Nodes['ItemLayout'];
    if (Assigned(ANode)) and (not (ANode.NodeValue = null)) then
      GUI.Screen.ItemLayout := GetLayoutOrientation(ANode.NodeValue);

    //Padding - Left
    ANode := XMLDoc.DocumentElement.ChildNodes.Nodes['PaddingLeft'];
    if (Assigned(ANode)) and (not (ANode.NodeValue = null)) then
      GUI.Screen.Padding.Left := ANode.NodeValue;

    //Padding - Top
    ANode := XMLDoc.DocumentElement.ChildNodes.Nodes['PaddingTop'];
    if (Assigned(ANode)) and (not (ANode.NodeValue = null)) then
      GUI.Screen.Padding.Top := ANode.NodeValue;

    //Padding - Right
    ANode := XMLDoc.DocumentElement.ChildNodes.Nodes['PaddingRight'];
    if (Assigned(ANode)) and (not (ANode.NodeValue = null)) then
      GUI.Screen.Padding.Right := ANode.NodeValue;

    //Padding - Bottom
    ANode := XMLDoc.DocumentElement.ChildNodes.Nodes['PaddingBottom'];
    if (Assigned(ANode)) and (not (ANode.NodeValue = null)) then
      GUI.Screen.Padding.Bottom := ANode.NodeValue;

    ANode := XMLDoc.DocumentElement.ChildNodes['ScreenItems'];

    for I := 0 to ANode.ChildNodes.Count - 1 do
    begin
      AChild := ANode.ChildNodes.Nodes[I];

      lGUIControl := GUI.FindGUIControl(AChild.NodeName);

      if Assigned(lGUIControl) then
      begin
        if not Assigned(lGUIControl.ScreenItem) then
          lGUIControl.ScreenItem := TScreenControl.Create;

        lScreenItem := lGUIControl.ScreenItem;

        //Caption
        AChildNode := AChild.ChildNodes['Caption'];
        if (Assigned(AChildNode)) and (not (AChildNode.NodeValue = null)) then
          lScreenItem.Caption.AsString := AChildNode.NodeValue;

        //Visible
        AChildNode := AChild.ChildNodes['Visible'];
        if (Assigned(AChildNode)) and (not (AChildNode.NodeValue = null)) then
          lScreenItem.Visible.AsBoolean := AChildNode.NodeValue;

        //CaptionVisible
        AChildNode := AChild.ChildNodes['CaptionVisible'];
        if (Assigned(AChildNode)) and (not (AChildNode.NodeValue = null)) then
          lScreenItem.CaptionVisible.AsBoolean := AChildNode.NodeValue;

        //CaptionPosition
        AChildNode := AChild.ChildNodes['CaptionPosition'];
        if (Assigned(AChildNode)) and (not (AChildNode.NodeValue = null)) then
          lScreenItem.CaptionPosition := GetLabelPosition(AChildNode.NodeValue);

        //ItemHeight
        AChildNode := AChild.ChildNodes['ItemHeight'];
        if (Assigned(AChildNode)) and (not (AChildNode.NodeValue = null)) then
          lScreenItem.ItemHeight.AsInteger := AChildNode.NodeValue;

        //ItemHeightMeasureType
        AChildNode := AChild.ChildNodes['ItemHeightMeasureType'];
        if (Assigned(AChildNode)) and (not (AChildNode.NodeValue = null)) then
          lScreenItem.ItemHeightMeasureType := GetMeasureType(AChildNode.NodeValue);

        //ItemWidth
        AChildNode := AChild.ChildNodes['ItemWidth'];
        if (Assigned(AChildNode)) and (not (AChildNode.NodeValue = null)) then
          lScreenItem.ItemWidth.AsInteger := AChildNode.NodeValue;

        //ItemWidthMeasureType
        AChildNode := AChild.ChildNodes['ItemWidthMeasureType'];
        if (Assigned(AChildNode)) and (not (AChildNode.NodeValue = null)) then
          lScreenItem.ItemWidthMeasureType := GetMeasureType(AChildNode.NodeValue);

        //PutAfter
        AChildNode := AChild.ChildNodes['PutAfter'];
        if (Assigned(AChildNode)) and (not (AChildNode.NodeValue = null)) then
          lScreenItem.PutAfter := AChildNode.NodeValue;

        //PutBefore
        AChildNode := AChild.ChildNodes['PutBefore'];
        if (Assigned(AChildNode)) and (not (AChildNode.NodeValue = null)) then
          lScreenItem.PutBefore := AChildNode.NodeValue;
      end;
    end;
  finally
    XMLDoc.Free;
  end;
end;

procedure TGUIAnnotationLoaderXML.Save;
var
  It: IGUIControlIterator;
  lScreenItem: IScreenItem;
  XMLDoc: IXMLDOMDocument;
  root, child, root_item, item: IXMLDomElement;
begin
  if Length(GetRepository) = 0 then
    raise Exception.Create('Invalid FileName');

  XMLDoc:= CreateOleObject('Microsoft.XMLDOM') as IXMLDomDocument;

  //Root
  root:= XMLDoc.createElement('Screen');
  XMLDoc.appendchild(root);

  //Title
  if not GUI.Screen.Title.IsNull then
  begin
    child:= XMLDoc.createElement('Title');
    child.appendChild(XMLDoc.createTextNode(GUI.Screen.Title.AsString));
    root.appendchild(child);
  end;

  //CaptionPosition
  if GUI.Screen.CaptionPosition <> lpLeft then
  begin
    child:= XMLDoc.createElement('CaptionPosition');
    child.appendChild(XMLDoc.createTextNode(TLabelPositionText[GUI.Screen.CaptionPosition]));
    root.appendchild(child);
  end;

  //Height
  if not GUI.Screen.Height.IsNull then
  begin
    child:= XMLDoc.createElement('Height');
    child.appendChild(XMLDoc.createTextNode(IntToStr(GUI.Screen.Height.AsInteger)));
    root.appendchild(child);
  end;

  //Width
  if not GUI.Screen.Width.IsNull then
  begin
    child:= XMLDoc.createElement('Width');
    child.appendChild(XMLDoc.createTextNode(IntToStr(GUI.Screen.Width.AsInteger)));
    root.appendchild(child);
  end;

  //ItemLayout
  if GUI.Screen.ItemLayout <> laHorizontal then
  begin
    child:= XMLDoc.createElement('ItemLayout');
    child.appendChild(XMLDoc.createTextNode(TLayoutOrientationText[GUI.Screen.ItemLayout]));
    root.appendchild(child);
  end;

  //Padding - Left
  if GUI.Screen.Padding.Left <> 5 then
  begin
    child:= XMLDoc.createElement('PaddingLeft');
    child.appendChild(XMLDoc.createTextNode(IntToStr(GUI.Screen.Padding.Left)));
    root.appendchild(child);
  end;

  //Padding - Top
  if GUI.Screen.Padding.Top <> 5 then
  begin
    child:= XMLDoc.createElement('PaddingTop');
    child.appendChild(XMLDoc.createTextNode(IntToStr(GUI.Screen.Padding.Top)));
    root.appendchild(child);
  end;

  //Padding - Right
  if GUI.Screen.Padding.Right <> 5 then
  begin
    child:= XMLDoc.createElement('PaddingRight');
    child.appendChild(XMLDoc.createTextNode(IntToStr(GUI.Screen.Padding.Right)));
    root.appendchild(child);
  end;

  //Padding - Bottom
  if GUI.Screen.Padding.Bottom <> 5 then
  begin
    child:= XMLDoc.createElement('PaddingBottom');
    child.appendChild(XMLDoc.createTextNode(IntToStr(GUI.Screen.Padding.Bottom)));
    root.appendchild(child);
  end;

  child:= XMLDoc.createElement('ScreenItems');
  root.appendchild(child);

  //ScreenItems
  It := GUI.GUIControlList.NewIterator;

  while not It.IsDone do
  begin
    if Assigned((It.CurrentItem as IGUIControl).ScreenItem) then
    begin
      lScreenItem := (It.CurrentItem as IGUIControl).ScreenItem;

      root_item:= XMLDoc.createElement((It.CurrentItem as IGUIControl).PropertyName);

      child.appendchild(root_item);

      //Caption
      if not lScreenItem.Caption.IsNull then
      begin
        item:= XMLDoc.createElement('Caption');
        item.appendChild(XMLDoc.createTextNode(lScreenItem.Caption.AsString));
        root_item.appendChild(item);
      end;

      //Visible
      if not lScreenItem.Visible.IsNull then
      begin
        item:= XMLDoc.createElement('Visible');
        item.appendChild(XMLDoc.createTextNode(BoolToStr(lScreenItem.Visible.AsBoolean, True)));
        root_item.appendChild(item);
      end;

      //CaptionVisible
      if not lScreenItem.CaptionVisible.IsNull then
      begin
        item:= XMLDoc.createElement('CaptionVisible');
        item.appendChild(XMLDoc.createTextNode(BoolToStr(lScreenItem.CaptionVisible.AsBoolean, True)));
        root_item.appendChild(item);
      end;

      //CaptionPosition
      if lScreenItem.CaptionPosition <> lpLeft then
      begin
        item:= XMLDoc.createElement('CaptionPosition');
        item.appendChild(XMLDoc.createTextNode(TLabelPositionText[lScreenItem.CaptionPosition]));
        root_item.appendChild(item);
      end;

      //ItemHeight
      if not lScreenItem.ItemHeight.IsNull then
      begin
        item:= XMLDoc.createElement('ItemHeight');
        item.appendChild(XMLDoc.createTextNode(IntToStr(lScreenItem.ItemHeight.AsInteger)));
        root_item.appendChild(item);
      end;

      //ItemHeightMeasureType
      if lScreenItem.ItemHeightMeasureType <> mtFix then
      begin
        item:= XMLDoc.createElement('ItemHeightMeasureType');
        item.appendChild(XMLDoc.createTextNode(TMeasureTypeText[lScreenItem.ItemHeightMeasureType]));
        root_item.appendChild(item);
      end;

      //ItemWidth
      if not lScreenItem.ItemWidth.IsNull then
      begin
        item:= XMLDoc.createElement('ItemWidth');
        item.appendChild(XMLDoc.createTextNode(IntToStr(lScreenItem.ItemWidth.AsInteger)));
        root_item.appendChild(item);
      end;

      //ItemWidthMeasureType
      if lScreenItem.ItemWidthMeasureType <> mtFix then
      begin
        item:= XMLDoc.createElement('ItemWidthMeasureType');
        item.appendChild(XMLDoc.createTextNode(TMeasureTypeText[lScreenItem.ItemWidthMeasureType]));
        root_item.appendChild(item);
      end;

      //PutAfter
      if Length(lScreenItem.PutAfter) > 0 then
      begin
        item:= XMLDoc.createElement('PutAfter');
        item.appendChild(XMLDoc.createTextNode(lScreenItem.PutAfter));
        root_item.appendChild(item);
      end;

      //PutBefore
      if Length(lScreenItem.PutBefore) > 0 then
      begin
        item:= XMLDoc.createElement('PutBefore');
        item.appendChild(XMLDoc.createTextNode(lScreenItem.PutBefore));
        root_item.appendChild(item);
      end;
    end;

    It.Next;
  end;

  //Salva o documento
  XMLDoc.save(GetRepository);
end;

procedure TGUIAnnotationLoaderXML.SetFileName(const Value: string);
begin
  FFileName:= Value;
end;

procedure TGUIAnnotationLoaderXML.SetGUI(const Value: IGUI);
begin
  FGUI := Value;
end;


end.
