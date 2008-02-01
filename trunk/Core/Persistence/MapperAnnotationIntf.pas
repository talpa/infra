unit MapperAnnotationIntf;

interface


Uses
  Classes, InfraCommonIntf, InfraValueTypeIntf;


Type   
  IEntity = interface(IElement)
    ['{28B94C80-55F6-47BB-9E86-8C38514E1980}']
    procedure SetName( const Value: String);
    function GetName: String;     
    property Name: String read GetName write SetName;    
  end;

  IColumn = interface(IElement)
    ['{C935A579-F071-44A2-9FE8-E97688943268}']
    procedure SetColumnDefinition( const Value: String);
    function  GetColumnDefinition: String;
    procedure SetInsertable( const Value: Boolean);
    function  GetInsertable: Boolean;
    procedure SetLength( const Value: Integer);
    function  GetLength: Integer;
    procedure SetName(const Value: String);
    function  GetName: String;
    procedure SetNullable( const Value: Boolean);
    function  GetNullable: Boolean;
    procedure SetPrecision(const Value: Integer);
    function  GetPrecision: Integer;
    procedure SetScale(const Value: Integer);
    function  GetScale: Integer;
    procedure SetTable(const Value: String);
    function  GetTable: String;
    procedure SetUnique(const Value: Boolean);
    function  GetUnique: Boolean;
    procedure SetUpdatable(const Value: Boolean);
    function  GetUpdatable: Boolean; 
    property ColumnDefinition: String read GetColumnDefinition write SetColumnDefinition;
    property Insertable: Boolean read GetInsertable write SetInsertable;
    property Length: Integer read GetLength write SetLength;
    property Name: String read GetName write SetName; 
    property Nullable: Boolean read GetNullable write SetNullable;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Scale: Integer read GetScale write SetScale;
    property Table: String read GetTable write SetTable;
    property Unique: Boolean read GetUnique write SetUnique;
    property Updatable: Boolean read GetUpdatable write SetUpdatable;
  end;

  ICollumns = interface(IBaseElement)
    ['{3B6F6AE0-EB84-47F2-A009-172D235C26A4}']
  end;

  IId = interface(IElement)
    ['{FCA085B9-06FB-445B-9D95-786F9B5BD07D}']
  end;

implementation

end.

