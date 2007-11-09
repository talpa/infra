unit List_GUIControl;

interface

{$I Infra.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBasicList,
  InfraCommonIntf,
  InfraGUIBuilderIntf,
  InfraCommon;

type
  _ITERABLELIST_BASE_ = TMemoryManagedObject;
  _ITERABLELIST_INTF_ = IGUIControlListBase;
  _ITEM_INTF_ = IGUIControl;
  _ITERATOR_INTF_ = IGUIControlIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  end;

  TGUIControlListBase = class(_ITERABLELIST_);

  TGUIControlList = class(TGUIControlListBase, IGUIControlList)
  public
    function Clone: IGUIControlList;
  end;

implementation

uses
  SysUtils;

{ TTypeMappingList }

{$I ..\Templates\InfraTempl_IntfList.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

{ TGUIControlList }

function TGUIControlList.Clone: IGUIControlList;
var
  It: IGUIControlIterator;
  lGUIControl: IGUIControl;
begin
  Result := TGUIControlList.Create;

  It := NewIterator;

  while not It.IsDone do
  begin
    lGUIControl := (It.CurrentItem as IGUIControl).Clone;
    Result.Add(lGUIControl);

    It.Next;
  end;
end;

end.
