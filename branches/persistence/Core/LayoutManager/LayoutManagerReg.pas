unit LayoutManagerReg;

interface

uses
  Classes, LayoutManager, DesignEditors, DesignIntf;

type
  TLayoutManagerTabOrder = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  TabOrderForm;

procedure Register;
begin
  RegisterComponents('Infra', [TLayoutManager]);
  RegisterComponentEditor(TLayoutManager, TLayoutManagerTabOrder);
  RegisterClasses([TLayoutManagerItem]);
end;

{ TLayoutManagerTabOrder }

procedure TLayoutManagerTabOrder.Edit;
begin
  ExecuteVerb(0);
end;

procedure TLayoutManagerTabOrder.ExecuteVerb(Index: Integer);
var
  TabOrderForm: TTabOrderForm;
begin
  if Index = 0 then
  begin
    TabOrderForm := TTabOrderForm.Create(nil);

    try
      TabOrderForm.ItemList := (Component as TLayoutManager).ItemList;

      if TabOrderForm.Execute then
        (Component as TLayoutManager).ResizeItems;
    finally
      TabOrderForm.Free;
    end;
  end;
end;

function TLayoutManagerTabOrder.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit Item Order';
  end;
end;

function TLayoutManagerTabOrder.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
