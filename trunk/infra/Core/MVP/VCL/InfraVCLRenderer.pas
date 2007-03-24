unit InfraVCLRenderer;

interface

uses
  Types,
  Windows,
  Graphics,
  InfraRenderer,
  InfraMVPVCLIntf;

type
  TVCLCustomListBoxItemRenderer = class(TRenderer,
    IVCLCustomListBoxItemRenderer)
  private
    function DoDataFind(const FindString: string): Integer;
    procedure DoGetData(Index: Integer; var Data: string);
    procedure DoDrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure DoMeasureItem(Index: Integer; var Height: Integer);
    procedure DoDataObject(Index: Integer; var DataObject: TObject);
  end;

  TVCLMenuItemRenderer = class(TRenderer, IVCLMenuItemRenderer)
  protected
    procedure DoAdvancedDrawItem(ACanvas: TCanvas; ARect: TRect;
      State: TOwnerDrawState);
    procedure DoDrawItem(ACanvas: TCanvas; ARect: TRect;
      Selected: Boolean);
    procedure DoMeasureItem(ACanvas: TCanvas; var Width,
      Height: Integer);
  end;

implementation

uses
  InfraValueTypeIntf;

{ TVCLCustomListBoxItemRenderer }

function TVCLCustomListBoxItemRenderer.DoDataFind(
  const FindString: string): Integer;
begin
  // do nothing here, just on descendents!
  Result := -1;
end;

procedure TVCLCustomListBoxItemRenderer.DoGetData(Index: Integer;
  var Data: string);
var
  ItemView: IVCLCustomListBoxItemView;
  Item: IInfraType;
begin
  ItemView := (View as IVCLCustomListBoxItemView);
  Item := ItemView.GetValueForIndex(Index);
  Data := (TypeConverter.ConvertFromLeftToRight(Item,
    Format) as IInfraString).AsString;
end;

procedure TVCLCustomListBoxItemRenderer.DoDrawItem(Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  // do nothing here, just on descendents!
end;

procedure TVCLCustomListBoxItemRenderer.DoMeasureItem(Index: Integer;
  var Height: Integer);
begin
  // do nothing here, just on descendents!
end;

procedure TVCLCustomListBoxItemRenderer.DoDataObject(Index: Integer;
  var DataObject: TObject);
begin
  // do nothing here, just on descendents!
end;

{ TVCLMenuItemRenderer }

procedure TVCLMenuItemRenderer.DoAdvancedDrawItem(ACanvas: TCanvas;
  ARect: TRect; State: TOwnerDrawState);
begin
  // do nothing here, just on descendents!
end;

procedure TVCLMenuItemRenderer.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  Selected: Boolean);
begin
  // do nothing here, just on descendents!
end;

procedure TVCLMenuItemRenderer.DoMeasureItem(ACanvas: TCanvas; var Width,
  Height: Integer);
begin
  // do nothing here, just on descendents!
end;

end.
