unit List_CustomProperty;

interface

{$I InfraGUI.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBasicList,
  InfraCommonIntf,
  GUIAnnotationIntf,
  InfraCommon;

type
  _ITERABLELIST_BASE_ = TBaseElement;
  _ITERABLELIST_INTF_ = ICustomPropertyListBase;
  _ITEM_INTF_ = ICustomProperty;
  _ITERATOR_INTF_ = ICustomPropertyIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  end;

  TCustomPropertyListBase = class(_ITERABLELIST_);

  TCustomPropertyList = class(TCustomPropertyListBase, ICustomPropertyList)
  public
    function Clone: ICustomPropertyList;
    procedure AddProp(PropName: string; PropValue: Variant);
  end;

implementation

uses
  SysUtils, GUIAnnotation;

{$I ..\Templates\InfraTempl_IntfList.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

{ TCustomPropertyList }

function TCustomPropertyList.Clone: ICustomPropertyList;
var
  It: ICustomPropertyIterator;
  lCustomProperty: ICustomProperty;
begin
  Result := TCustomPropertyList.Create;

  It := NewIterator;

  while not It.IsDone do
  begin
    lCustomProperty := (It.CurrentItem as ICustomProperty).Clone;

    Result.Add(lCustomProperty);

    It.Next;
  end;
end;

procedure TCustomPropertyList.AddProp(PropName: string; PropValue: Variant);
var
  lCustomProperty: ICustomProperty;
begin
  lCustomProperty := TCustomProperty.Create;
  lCustomProperty.PropName := PropName;
  lCustomProperty.PropValue := PropValue;

  Add(lCustomProperty);
end;

end.
