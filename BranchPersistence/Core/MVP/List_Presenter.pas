unit List_Presenter;

{$I Infra.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraMVPIntf, InfraBasicList, InfraCommon, InfraCommonIntf;

type
  _ITERABLELIST_BASE_ = TBaseElement;
  _ITERABLELIST_INTF_ = IPresenterList;
  _ITEM_INTF_ = IPresenter;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I Templates\InfraTempl_IntfList.inc}
  private
    FOwner: _ITEM_INTF_;
  protected
    function GetItemByName(const Name: string): _ITEM_INTF_;
    function GetOwner: _ITEM_INTF_;
    procedure SetOwner(const Value: _ITEM_INTF_);
    function Append(const Item: _ITEM_INTF_): _ITEM_INTF_;
    property Owner: _ITEM_INTF_ read GetOwner write SetOwner;
    property ItemByName[const Name: string]: _ITEM_INTF_
      read GetItemByName;
  end;

  TPresenterList = class(_ITERABLELIST_)
  protected
    function Add(const Item: _ITEM_INTF_): Integer; override;
    function Remove(const Item: _ITEM_INTF_): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const Item: _ITEM_INTF_); override;
  end;

implementation

uses SysUtils;

{ TPresenterListBase }

{$I Templates\InfraTempl_IntfList.inc}

function _ITERABLELIST_.GetItemByName(const Name: string): _ITEM_INTF_;
var
  i: Integer;
  Presenter: _ITEM_INTF_;
begin
  Result := nil;
  for i := 0 to FItems.Count-1 do
  begin
    Presenter := (FItems.Items[i] as _ITEM_INTF_);
    if Presenter.Name = Name then
    begin
      Result := Presenter;
      Break;
    end;
  end;
end;

function _ITERABLELIST_.GetOwner: _ITEM_INTF_;
begin
  Result := FOwner;
end;

procedure _ITERABLELIST_.SetOwner(const Value: _ITEM_INTF_);
begin
  SetReference(IInterface(FOwner), Value);
end;

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

{ TPresenterList }

function TPresenterList.Add(const Item: _ITEM_INTF_): Integer;
begin
  Result := inherited Add(Item);
  if Result <> -1 then
    Item.ParentPresenter := Owner;
end;

function TPresenterList.Remove(const Item: _ITEM_INTF_): Integer;
begin
  Item.ParentPresenter := nil;
  Result := inherited Remove(Item);
end;

procedure TPresenterList.Clear;
var
  Iterator: _ITERATOR_INTF_;
begin
  Iterator := NewIterator;
  while not Iterator.IsDone do
  begin
    (Iterator.CurrentItem as _ITEM_INTF_).ParentPresenter := nil;
    Iterator.Next;
  end;
  inherited;
end;

procedure TPresenterList.Delete(Index: Integer);
begin
  (Items[Index] as _ITEM_INTF_).ParentPresenter := nil;
  inherited Delete(Index);
end;

procedure TPresenterList.Insert(Index: Integer; const Item: _ITEM_INTF_);
begin
  inherited Insert(Index, Item);
  (Items[Index] as _ITEM_INTF_).ParentPresenter := Owner;
end;

function _ITERABLELIST_.Append(const Item: _ITEM_INTF_): _ITEM_INTF_;
var
  i: Integer;
begin
  i := Add(Item);
  if i <> -1 then
    Result := Item
  else
    Result := nil;
end;

end.
