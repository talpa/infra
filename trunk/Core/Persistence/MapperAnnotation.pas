unit MapperAnnotation;

interface

Uses
  MapperAnnotationIntf, InfraCommonIntf, InfraCommon;

Type       
  TEntity = Class(TElement, IEntity)
  private
    FName: String;
  public
    procedure SetName( const Value: String);
    function GetName: String;     
    property Name: String read GetName write SetName;    
  end;

  TColumn = Class(TElement, IColumn)
  private
    FColumnDefinition: String;
    FInsertable: Boolean;
    FLength: Integer;
    FName: String;
    FNullable: Boolean;
    FPrecision: Integer;
    FScale: Integer;
    FTable: String;
    FUnique: Boolean;
    FUpdatable: Boolean;
  public
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

  TId = class(TElement, IId);

{
  TColumns = interface(IElement)
    property columns: IColumn;
  end;


  TColumn = interface(IElement)
    property ColumnDefinition: string = "";
    property Insertable: boolean = true;
    property Length: integer = 255;
    property Name: string = "";
    property Nullable: boolean = true;
    property Precision: integer = 0;
    property Scale: integer = 0;
    property Table: string = "";
    property Unique: boolean = false;
    property Updatable: boolean = true;
  end;}

implementation

{ TEntity }

function TEntity.GetName: String;
begin
  Result:= FName;
end;

procedure TEntity.SetName(const Value: String);
begin
  FName:= Value;
end;

{ TColumn }
function TColumn.GetColumnDefinition: String;
begin
  Result:= FColumnDefinition;
end;

function TColumn.GetInsertable: Boolean;
begin
  Result:= FInsertable;
end;

function TColumn.GetLength: Integer;
begin
  Result:= FLength;
end;

function TColumn.GetName: String;
begin
  Result:= FName;
end;

function TColumn.GetNullable: Boolean;
begin
  Result:= FNullable;
end;

function TColumn.GetPrecision: Integer;
begin
  Result:= FPrecision;
end;

function TColumn.GetScale: Integer;
begin
  Result:= FScale;
end;

function TColumn.GetTable: String;
begin
  Result:= FTable;
end;

function TColumn.GetUnique: Boolean;
begin
  Result:= FUnique;
end;

function TColumn.GetUpdatable: Boolean;
begin
  Result:= FUpdatable
end;

procedure TColumn.SetColumnDefinition(const Value: String);
begin
  ColumnDefinition:= Value;
end;

procedure TColumn.SetInsertable(const Value: Boolean);
begin
  FInsertable:= Value;
end;

procedure TColumn.SetLength(const Value: Integer);
begin
  FLength:= Value;
end;

procedure TColumn.SetName(const Value: String);
begin
  FName:= Value;
end;

procedure TColumn.SetNullable(const Value: Boolean);
begin
  FNullable:= Value;
end;

procedure TColumn.SetPrecision(const Value: Integer);
begin
  FPrecision:= Value;
end;

procedure TColumn.SetScale(const Value: Integer);
begin
  FScale:= Value;
end;

procedure TColumn.SetTable(const Value: String);
begin
  FTable:= Value;
end;

procedure TColumn.SetUnique(const Value: Boolean);
begin
  FUnique:= Value;
end;

procedure TColumn.SetUpdatable(const Value: Boolean);
begin
  FUpdatable:= Value;
end;

end.
