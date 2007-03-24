unit List_Validator;

{$I Infra.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraCommonIntf,
  InfraBasicList,
  InfraCommon,
  InfraValidatorIntf;

type
  _ITERABLELIST_BASE_ = TMemoryManagedObject;
  _ITERABLELIST_INTF_ = IInfraValidatorList;
  _ITEM_INTF_ = IInfraValidator;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I Templates\InfraTempl_IntfList.inc}
  private
    FOwner: IInfraValidator;
  protected
    function GetOwner: IInfraValidator;
    procedure InvalidateCache;
    procedure SetOwner(const Value: IInfraValidator);
    property Owner: IInfraValidator read GetOwner write SetOwner;
  public
    constructor Create(const AOwner: IInfraValidator); overload;
  end;

  TValidatorList = class(_ITERABLELIST_);

implementation

uses
  SysUtils,
  InfraValueTypeIntf;

{ TValidatorList }

{$I Templates\InfraTempl_IntfList.inc}

function _ITERABLELIST_.GetOwner: IInfraValidator;
begin
  Result := FOwner;
end;

procedure _ITERABLELIST_.InvalidateCache;
begin
  (FOwner as IInfraType).InvalidateCache;
end;

procedure _ITERABLELIST_.SetOwner(const Value: IInfraValidator);
begin
  SetReference(IInterface(FOwner), Value);
end;

constructor _ITERABLELIST_.Create(const AOwner: IInfraValidator);
begin
  inherited Create;
  Owner := AOwner;
end;

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

end.
