unit InfraSQLBuilder;

interface

uses
  InfraBase, InfraHibernateIntf;

type
  TSelectBuilder = class(TInfraBaseObject, ISelectBuilder)
  private
    FDialect: IDialect;
    FFromClause: String;
    FGroupByClause: String;
    FOrderByClause: String;
    FOuterJoinsAfterFrom: String;
    FOuterJoinsAfterWhere: String;
    FSelectClause: String;
    FWhereClause: String;
    function ToStatementString: String;
    procedure SetFromClause(const pTableName, pAlias: String); overload;
    procedure SetFromClause(const Value: String); overload;
    procedure SetGroupByClause(const Value: String);
    procedure SetOrderByClause(const Value: String);
    procedure SetOuterJoins(const pOuterJoinsAfterFrom,
      pOuterJoinsAfterWhere: String);
    procedure SetSelectClause(const Value: String);
    procedure SetWhereClause(const Value: String); overload;
  public
    constructor Create(const pDialect: IDialect); reintroduce;
  end;

  (*
  TDeleteBuilder = class(TElement, IDeleteBuilder)
  private
    FTableName: String;
    FPrimaryKeyColumnNames: TArrayString;
    FVersionColumnName: String;
    FWhereClause: String;
    function ToStatementString: String;
    procedure SetTableName(const Value: String);
    procedure SetWhereClause(const Value: String);
    procedure AddWhereFragment(const Value: String);
    procedure SetPrimaryKeyColumnNames(Value: TArrayString);
    procedure SetVersionColumnName(const Value: String);
  end;

  TUpdateBuilder = class(TElement, IUpdateBuilder)
  private
    FTableName: String;
    FPrimaryKeyColumnNames: TArrayString;
    FVersionColumnName: String;
    FWhereClause: String;
    FAssignments: String;
    FColumns: Map;
    FWhereColumns: Map; 
    FDialect: IDialect;
    function GetTableName: string;
    function ToStatementString: String;
    procedure AddColum(const ColumnName, Value: string);
    procedure AddColum(const ColumnName: string);
    procedure AddColum(const ColumnName: string; const Value: IInfraType);
    procedure AddColumns(Value: TArrayString);
    procedure AddColumns(Value: TArrayString; const Value: string);
    procedure AddColumns(Value: TArrayString; Updateable: TArrayBoolean);
  	procedure AddWhereColumn(const ColumnName, Value: String);
  	procedure AddWhereColumn(const ColumnName: String);
  	procedure AddWhereColumns(ColumnNames: TArrayString);
  	procedure AddWhereColumns(ColumnNames: TArrayString; const Value: String);
    procedure AppendAssignmentFragment(const Value: String);
    procedure SetPrimaryKeyColumnNames(Value: TArrayString);
    procedure SetTableName(const Value: string);
  	procedure SetWhere(const Where: String);
    property TableName: string read GetTableName write SetTableName;
  end;
  *)
  
implementation

uses Classes, SysUtils;

{ TSelectBuilder }

constructor TSelectBuilder.Create(const pDialect: IDialect);
begin
  // *** Leak aqui?
  FDialect := pDialect;
end;

function TSelectBuilder.ToStatementString: String;
var
  vBuf: TStrings;
begin
  vBuf := TStringList.Create;
  try
    with vBuf do
    begin
      Add('SELECT ');
      Add(FSelectClause);
      Add('FROM ');
      Add(FFromClause);
      Add(FOuterJoinsAfterFrom);
      if (FWhereClause <> EmptyStr) or (FOuterJoinsAfterWhere <> EmptyStr) then
      begin
        Add('WHERE ');
        if (FOuterJoinsAfterWhere <> EmptyStr) then
        begin
          Add(FOuterJoinsAfterWhere);
          if (FWhereClause <> EmptyStr) then
            Add(' AND ');
        end;
        if (FWhereClause <> EmptyStr) then
          Add(FWhereClause);
      end;
      if (FGroupByClause <> EmptyStr) then
      begin
        Add('GROUP BY ');
        Add(FGroupByClause);
      end;
      if (FOrderByClause <> EmptyStr) then
      begin
        Add('ORDER BY ');
        Add(FOrderByClause);
      end;
    end;
  finally
    // ### No hibernate:
    // Há a possibilidade de transformar a instrução pelo Dialect. Ver isso
    // depois, lembrando que esta classe terá de receber
    // Result := Dialect.TransformSelectString(vBuf.Text);
    // em vez do result abaixo.
    Result := vBuf.Text;
    vBuf.Free;
  end;
end;

procedure TSelectBuilder.SetFromClause(const Value: String);
begin
  FFromClause := Value;
end;

procedure TSelectBuilder.SetFromClause(const pTableName, pAlias: String);
begin
  FFromClause := pTableName + ' ' + pAlias;
end;

procedure TSelectBuilder.SetOrderByClause(const Value: String);
begin
  FOrderByClause := Value;
end;

procedure TSelectBuilder.SetGroupByClause(const Value: String);
begin
  FGroupByClause := Value;
end;

procedure TSelectBuilder.SetOuterJoins(const pOuterJoinsAfterFrom,
  pOuterJoinsAfterWhere: String);
var
  vTmpOuterJoinsAfterWhere: String;
begin
  FOuterJoinsAfterFrom := pOuterJoinsAfterFrom;
  vTmpOuterJoinsAfterWhere := Trim(pOuterJoinsAfterWhere);
  if TStringHelper.StartsWith(vTmpOuterJoinsAfterWhere, 'and') then
    vTmpOuterJoinsAfterWhere :=
      TStringHelper.Substring(vTmpOuterJoinsAfterWhere, 4);
  FOuterJoinsAfterWhere := vTmpOuterJoinsAfterWhere;
end;

procedure TSelectBuilder.SetSelectClause(const Value: String);
begin
  FSelectClause := Value;
end;

procedure TSelectBuilder.SetWhereClause(const Value: String);
begin
  FWhereClause := Value;
end;

(*
{ TDeleteBuilder }

procedure TDeleteBuilder.AddWhereFragment(const Value: String);
begin

end;

procedure TDeleteBuilder.SetPrimaryKeyColumnNames(Value: TArrayString);
begin

end;

procedure TDeleteBuilder.SetTableName(const Value: String);
begin

end;

procedure TDeleteBuilder.SetVersionColumnName(const Value: String);
begin

end;

procedure TDeleteBuilder.SetWhereClause(const Value: String);
begin

end;

function TDeleteBuilder.ToStatementString: String;
begin

end;

{ TUpdateBuilder }

function TUpdateBuilder.GetTableName: string;
begin

end;

procedure TUpdateBuilder.SetTableName(const Value: string);
begin
  FTableName := Value;
end;

function TUpdateBuilder.ToStatementString: String;
begin

end;
*)

end.
