unit List_SQLCommandList;

interface

{$I InfraPersistence.Inc} 

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraOPFIntf,
  InfraCommonIntf,
  InfraCommon,
  InfraBasicList;

type
  _ITERABLELIST_BASE_ = TElement;
  _ITERABLELIST_INTF_ = ISQLCommandList;
  _ITEM_INTF_ = ISQLCommand;
  _ITERATOR_INTF_ = IInfraIterator;
  {$I ..\Templates\InfraTempl_IntfList.inc}
  end;

  TSQLCommandList = class(_ITERABLELIST_);

implementation

uses
  SysUtils;

{ TSQLCommandList }

{$I ..\Templates\InfraTempl_IntfList.inc}

destructor _ITERABLELIST_.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

end.

