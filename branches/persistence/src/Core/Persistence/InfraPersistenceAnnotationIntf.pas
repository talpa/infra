unit InfraPersistenceAnnotationIntf;

interface


Uses
  Classes,
  {Zeos} ZDbcIntfs,
  {Infra} InfraCommonIntf,
   InfraValueTypeIntf;

Type
  TZTypeSetter = procedure (const pStatement: IZPreparedStatement;  pIndex: Integer; const pParamValue: IInfraType);

  TZTypeGetter = procedure (const pResultSet: IZResultSet; pIndex: Integer; const pPropertyValue: IInfraType);

  IZTypeAnnotation = interface(IElement)
    ['{224B7552-1AB1-456B-B5C5-C7A85BA60580}']
    function GetNullSafeGetter: TZTypeGetter;
    function GetNullSafeSetter: TZTypeSetter;
    property NullSafeGet: TZTypeGetter read GetNullSafeGetter;
    property NullSafeSet: TZTypeSetter read GetNullSafeSetter;
    procedure Init(pGetter: TZTypeGetter; pSetter: TZTypeSetter);
  end;
  

implementation

end.




