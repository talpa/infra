unit InfraFriendClasses;

interface

uses
  Forms,
  Controls,
  StdCtrls,
  ComCtrls,
  Menus;

type
  TControlFriend = class(TControl)
  public
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TWinControlFriend = class(TWinControl)
  public
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TCustomEditFriend = class(TCustomEdit)
  public
    function GetSelText: string; override;
    property OnChange;
  end;

  TDateTimePickerFriend = class(TDateTimePicker)
  public
    property OnChange;
    property DateTime;
  end;

  TCustomCheckBoxFriend = class(TCustomCheckBox)
  public
    property Checked;
    property State;
  end;

  TRadioButtonFriend = class(TRadioButton)
  public
    property Checked;
  end;

  TCustomListBoxFriend = class(TCustomListBox)
  public
    property Style;
    property OnDrawItem;
    property OnMeasureItem;
    property OnData;
    property OnDataObject;
    property OnDataFind;
  end;

  TButtonFriend = class(TButton);

  TMenuItemFriend = class(TMenuItem);

  TCustomFormFriend = class(TCustomForm)
  public
    property OnDestroy;
    property OnClose;
  end;

implementation

{ TCustomEditFriend }

function TCustomEditFriend.GetSelText: string;
begin
  Result := inherited GetSelText;
end;

end.
