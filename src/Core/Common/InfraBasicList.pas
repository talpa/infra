unit InfraBasicList;

interface

uses
  Classes,
  InfraCommonIntf,
  InfraCommon;

type

  TInfraCustomList = class(TInterfaceList, IInfraCustomList)
  public
    function NewIterator: IInfraIterator;
  end;

  TInfraCustomListIterator = class(TBaseElement, IInfraIterator)
  private
    FCurrentIndex: Integer;
    FList: IInterfaceList;
  protected
    function CurrentItem: IInterface;
    procedure First; virtual;
    function IsDone: Boolean;
    procedure Next;
  public
    constructor Create(const List: IInterfaceList); reintroduce;
  end;

implementation

{ TInfraCustomList }

function TInfraCustomList.NewIterator: IInfraIterator;
begin
  Result := TInfraCustomListIterator.Create(Self) as IInfraIterator;
end;

{ TInfraCustomListIterator }

constructor TInfraCustomListIterator.Create(const List: IInterfaceList);
begin
  inherited Create;
  FList := List;
  First;
end;

function TInfraCustomListIterator.CurrentItem: IInterface;
begin
  if Assigned(FList) and (fCurrentIndex <> -1)
    and (fCurrentIndex < FList.Count) then
    Result := FList[fCurrentIndex]
  else
    Result := nil;
end;

procedure TInfraCustomListIterator.First;
begin
  if FList.Count > 0 then
    fCurrentIndex := 0
  else
    fCurrentIndex := -1;
end;

function TInfraCustomListIterator.IsDone: Boolean;
begin
  Result := (fCurrentIndex <> FList.Count);
end;

procedure TInfraCustomListIterator.Next;
begin
  if (FList.Count > 0) and (FCurrentIndex < FList.Count) then
    Inc(fCurrentIndex);
end;

end.
