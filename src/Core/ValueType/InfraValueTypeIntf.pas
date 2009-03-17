unit InfraValueTypeIntf;

interface

uses
  Classes,
  InfraCommonIntf;

type
  IProperty = interface;

  IInfraType = interface(IElement)
    ['{CC993C1B-321B-49E6-8167-23002B298000}']
    function Clone: IInfraType;
    function Equals(const Obj: IInfraType): Boolean;
    function GetIsDerived: Boolean;
    function GetIsNull: Boolean;
    procedure Assign(const Source: IInfraType);
    procedure Clear;
    procedure InvalidateCache;
    procedure SetIsDerived(const Value: Boolean);
    procedure SetIsNull(Value: Boolean);
    property IsDerived: boolean read GetIsDerived write SetIsDerived;
    property IsNull: Boolean read GetIsNull write SetIsNull;
  end;

  IInfraString = interface(IInfraType)
    ['{29BCFBBF-4510-4314-99E2-C283C6813A29}']
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    property AsString: string read GetAsString write SetAsString;
  end;

  IInfraDouble = interface(IInfraType)
    ['{F5C2EBC7-98F7-4C89-A7D5-DA4792F8E903}']
    function GetAsDouble: Double;
    procedure SetAsDouble(const Value: Double);
    property AsDouble: Double read GetAsDouble write SetAsDouble;
  end;

  IInfraVariant = interface(IInfraType)
    ['{E054B243-82A8-47CE-BA53-016A3CA21AD0}']
    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  end;

  IInfraInteger = interface(IInfraType)
    ['{3A4A4AF2-2EB1-41DE-B637-8CBBB4627ACB}']
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
  end;

  IInfraBoolean = Interface(IInfraType)
    ['{A42BA3FD-38C3-4DE3-A53A-7AE235584927}']
    function GetAsBoolean: boolean;
    procedure SetAsBoolean(Value: boolean);
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
  end;

  IInfraDateTime = interface(IInfraType)
    ['{50B72C0A-AFFB-4E00-9BE6-B8CF3EA9D4A3}']
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(Value: TDateTime);
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
  end;

  IInfraDate = interface(IInfraType)
    ['{959CEE75-01D5-41FE-85BF-9F494A76562D}']
    function GetAsDate: TDateTime;
    procedure SetAsDate(Value: TDateTime);
    property AsDate: TDateTime read GetAsDate write SetAsDate;
  end;

  IInfraTime = interface(IInfraType)
    ['{0BAEA84F-8D7B-46DB-B344-E6491DAF8BBC}']
    function GetAsTime: TDateTime;
    procedure SetAsTime(Value: TDateTime);
    property AsTime: TDateTime read GetAsTime write SetAsTime;
  end;

  IInfraList = interface(IInfraType)
    ['{F03B020D-4454-451F-9EB6-A9ED86E63999}']
    function Add(const Item: IInfraType): Integer;
    function Append(const Item: IInfraType): IInfraType;
    function First: IInfraType;
    function GetCount: Integer;
    function GetItem(Index: Integer): IInfraType;
    function IndexOf(const Item: IInfraType): Integer;
    function Last: IInfraType;
    function Remove(const Item: IInfraType): Integer;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; const Item: IInfraType);
    procedure Lock;
    procedure SetItem(Index: Integer; const Item: IInfraType);
    procedure Unlock;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IInfraType read GetItem write SetItem; default;
  end;

  IInfraMap = interface(IInfraType)
    ['{B9FF65A6-5356-4DA8-9C98-107B7E2610AE}']
    function ContainsKey(const Key: IInfraType): boolean;
    function ContainsValue(const Value: IInfraType): boolean;
    function GetItem(const Key: IInfraType): IInfraType;
    function GetKeyByValue(const Value: IInfraType): IInfraType;
    function Keys: IInfraList;
    function Values: IInfraList;
    procedure SetItem(const Key: IInfratype; Value: IInfraType);
    property Items[const Key: IInfraType]: IInfraType
      read GetItem write SetItem; default;
  end;

  IInfraStream = interface(IInfraType)
    ['{2307265E-AFEA-4B25-8B12-DF632808DFBC}']
    function GetAsStream: TMemoryStream;
    procedure SetAsStream(const Value: TMemoryStream);
    property AsStream: TMemoryStream read GetAsStream write SetAsStream;
  end;

  IInfraObject = interface(IInfraType)
    ['{2201AD61-3E07-4F09-B8A9-8BB5F8EF67A1}']
    function AddProperty(const PropertyName: string): IProperty;
    function GetProperty(const PropertyName: string): IProperty;
  end;

  IProperty = interface(IInfraType)
    ['{CF3CD862-2171-48FF-853B-AF35D61BE4D9}']
    function GetName: string;
    function GetOwner: IElement;
    procedure SetOwner(const Value: IElement);
    property Name: string read GetName;
    property Owner: IElement read GetOwner write SetOwner;
  end;

  IInfraLiteral = interface(IElement)
    ['{14557BB1-5918-415F-AEAA-050E9CCB3B7D}']
    function GetName: string;
    procedure SetName(const Value: string);
    property Name: string read GetName write SetName;
  end;

  IInfraLiteralList = interface(IElement)
    ['{24B8D2A9-61BC-489A-9D34-B9A7CC773772}']
    function Add(const Item: IInfraLiteral): Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): IInfraLiteral;
    property Items[Index: Integer]: IInfraLiteral read GetItem; default;
    property Count: Integer read GetCount;
  end;

  IInfraEnumeration = interface(IInfraType)
    ['{E9841A05-AA0F-4F78-9365-9A99F1148A81}']
    function GetAsLiteral: IInfraLiteral;
    function GetLiterals: IInfraLiteralList;
    procedure SetAsLiteral(Value: IInfraLiteral);
    property AsLiteral: IInfraLiteral read GetAsLiteral write SetAsLiteral;
    property Literals: IInfraLiteralList read GetLiterals;
  end;

  IInfraNativeObject = interface(IInfraType)
    ['{CE984875-8BC4-4BB1-A5D8-986EE6941345}']
    function GetAsNativeObject: TObject;
    procedure SetAsNativeObject(const Value: TObject);
    property AsNativeObject: TObject read GetAsNativeObject
      write SetAsNativeObject;
  end;

  // Converters

  ITypeConverter = interface(IElement)
    ['{D2D5AB4E-18FA-4300-945B-E2658FF537E2}']
    function ConvertToRight(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType;
    function ConvertToLeft(const Value: IInfraType;
      const Parameter: IInfraType = nil): IInfraType;
  end;

  INullConverter = interface(ITypeConverter)
    ['{B95670BD-469A-4FD3-9ECE-E6436232BB39}']
  end;

  IDateTimeToText = interface(ITypeConverter)
    ['{693EF0A1-B6EE-4982-B461-B87174891CD6}']
  end;

  IBooleanToText = interface(ITypeConverter)
    ['{E7663123-B8FF-41A0-B9A4-D6E3CBBB49BA}']
  end;

  IIntegerToText = interface(ITypeConverter)
    ['{F77F44CF-C3C9-416A-B995-397D63A7646B}']
  end;

  IDoubleToText = interface(ITypeConverter)
    ['{081486E8-F388-4645-AEF2-CF5C2501933B}']
  end;

  IStringToVariant = interface(ITypeConverter)
    ['{378EC4F3-439F-4A91-9967-6CF9C1226642}']
  end;

  IDoubleToVariant = interface(ITypeConverter)
    ['{79BE60E6-C134-4CB4-A098-9052BF838A56}']
  end;

  IIntegerToVariant = interface(ITypeConverter)
    ['{62A4A6FC-3410-43BD-875D-6FEF5C7D2489}']
  end;

  IBooleanToVariant = interface(ITypeConverter)
    ['{1C80883D-5656-4942-B325-5F2B5EC0DD41}']
  end;

  IDateTimeToVariant = interface(ITypeConverter)
    ['{0F5BC38A-81DE-4CC0-B01D-EE34DBE7B288}']
  end;

  // Events

  IInfraChangedEvent = interface(IInfraEvent)
    ['{B80877CC-77F9-471F-B396-B064B5F545E6}']
  end;

  IInfraAddItemEvent = interface(IInfraEvent)
    ['{47403892-46E2-4FA7-9AE3-470C39FE1F85}']
    function GetItemIndex: Integer;
    function GetNewItem: IInfraType;
    procedure SetItemIndex(Index: Integer);
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property NewItem: IInfraType read GetNewItem;
  end;

  IInfraCanAddItemEvent = interface(IInfraAddItemEvent)
    ['{309679D4-2DE8-49DB-A66B-576F4CBE7A56}']
  end;

  IInfraClearListEvent = interface(IInfraEvent)
    ['{AEF7BA4E-4716-40E9-B1A2-2AA480A1D99C}']
  end;

  IInfraRemoveItemEvent = interface(IInfraEvent)
    ['{04745DB5-7AFF-4641-A9EA-26474C27C60F}']
    function GetItemIndex: Integer;
    function GetRemovedItem: IInfraType;
    property ItemIndex: Integer read GetItemIndex;
    property RemovedItem: IInfraType read GetRemovedItem;
  end;

  IInfraCanRemoveItemEvent = interface(IInfraRemoveItemEvent)
    ['{53B5E4C5-C434-4712-B281-431C626EF78C}']
  end;

  IInfraAfterMethodExecute = interface(IInfraEvent)
    ['{8B4639F1-D4D6-415C-9EE1-A187CF49815E}']
    function GetParameters: IInfraList;
    property Parameters: IInfraList read GetParameters;
  end;

function GetRefCountedPointerFromInterface(const Intf: IInterface): Pointer;

implementation

uses
  InfraValueTypeRegister;

function GetRefCountedPointerFromInterface(const Intf: IInterface): Pointer;
begin
  Result := nil;
  IInterface(Result) := Intf;
end;

initialization
  InfraValueTypeRegister.RegisterOnReflection;

end.
