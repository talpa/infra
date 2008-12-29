unit InfraBase;

interface

type
  TInfraBaseObject = class(TInterfacedObject)
  public
    constructor Create; virtual;
  end;

  TInfraBaseObjectClass = class of TInfraBaseObject;

implementation

{ TInfraBaseObject }

// this virtual constructor is necessary to reflection create rightly instances.
// This will permit Reflection use the right constructor of ImplClass. 
constructor TInfraBaseObject.Create;
begin
  inherited;

end;

end.





