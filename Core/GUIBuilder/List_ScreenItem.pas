unit List_ScreenItem;

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
  _ITERABLELIST_INTF_ = IScreenItemListBase;
  _ITEM_INTF_ = IScreenItem;
  _ITERATOR_INTF_ = IScreenItemIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  end;

  TScreenItemListBase = class(_ITERABLELIST_);

  TScreenItemList = class(TScreenItemListBase, IScreenItemList)
  public
    function Clone: IScreenItemList;
  end;

implementation

uses
  SysUtils;

{$I ..\Templates\InfraTempl_IntfList.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

{ TScreenItemList }

function TScreenItemList.Clone: IScreenItemList;
var
  It: IScreenItemIterator;
  lScreenItem: IScreenItem;
begin
  Result := TScreenItemList.Create;

  It := NewIterator;

  while not It.IsDone do
  begin
    lScreenItem := It.CurrentItem as IScreenItem;

    if Supports(lScreenItem, IScreenControl) then
      lScreenItem := (lScreenItem as IScreenControl).Clone
    else if Supports(lScreenItem, IScreenGroup) then
      lScreenItem := (lScreenItem as IScreenGroup).Clone
    else
      lScreenItem := lScreenItem.CloneItem;

    Result.Add(lScreenItem);

    It.Next;
  end;
end;

end.
