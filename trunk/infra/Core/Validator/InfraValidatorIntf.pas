unit InfraValidatorIntf;

interface

uses
  InfraCommonIntf, InfraValueTypeIntf;

type
  IInfraValidatorList = interface;

  IInfraValidator = interface(IInfratype)
    ['{F735488D-57AA-4971-87A7-61CC1FCE7421}']
    function GetConstrainedObject: IInfraType;
    function GetInnerValidators: IInfraValidatorList;
    function GetIsValid: Boolean;
    procedure SetConstrainedObject(const Value: IInfraType);
    procedure SetIsValid(Value: boolean);
    property IsValid: Boolean read GetIsValid write SetIsValid;
    property ConstrainedObject: IInfraType read GetConstrainedObject write SetConstrainedObject;
    property InnerValidators: IInfraValidatorList read GetInnerValidators;
  end;

  IInfraValidatorList = interface(IMemoryManagedObject)
    ['{F452890C-7EBC-4FBF-AE33-B1498F60259F}']
    function Add(const Item: IInfraValidator): Integer;
    function First: IInfraValidator;
    function GetCount: Integer;
    function GetItem(Index: Integer): IInfraValidator;
    function GetOwner: IInfraValidator;
    function IndexOf(const Item: IInfraValidator): Integer;
    function Last: IInfraValidator;
    function Remove(const Item: IInfraValidator): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; const Item: IInfraValidator);
    procedure InvalidateCache;
    procedure Lock;
    procedure SetItem(Index: Integer; const Item: IInfraValidator);
    procedure SetOwner(const Value: IInfraValidator);
    procedure Unlock;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IInfraValidator read GetItem write SetItem; default;
    property Owner: IInfraValidator read GetOwner write SetOwner;
  end;

  IInfraValidationService = interface(IInterface)
    ['{0D75BA51-CA6D-4127-A0A5-C980E015A1BC}']
    function GetValidatorFor(const InfraType: IInfraType): IInfraValidator;
  end;

function ValidationService: IInfraValidationService;

implementation

function ValidationService: IInfraValidationService;
begin
  result := (ApplicationContext as IInfraValidationService);
end;

end.
