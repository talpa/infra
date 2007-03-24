unit List_JointPoint;

{$I Infra.Inc}

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraBasicList,
  InfraAspectIntf,
  InfraCommonIntf,
  InfraCommon;

type
  _ITERABLELIST_BASE_ = TMemoryManagedObject;
  _ITERABLELIST_INTF_ = IInfraJointPoints;
  _ITEM_INTF_ = IInfraJointPoint;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  end;

  TInfraJointPoints = class(_ITERABLELIST_);

implementation

uses
  SysUtils;

{ TInfraJointPoints }

{$I ..\Templates\InfraTempl_IntfList.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

end.
