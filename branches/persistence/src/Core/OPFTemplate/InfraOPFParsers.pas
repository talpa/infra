unit InfraOPFParsers;

interface

uses
  Classes,
  InfraCommon,
  InfraOPFIntf;

type
  /// Classe utilitária para obter parâmetros e macros de uma instrução SQL
  TSQLParamsParser = class(TBaseElement, ISQLParamsParser)
  private
    FParams: TStrings;
    FMacroParams: TStrings;
  protected
    function Parse(const pSQL: string): string;
    function GetParams: TStrings;
    function GetMacroParams: TStrings;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  RegExpr, InfraOPFConsts;

{ TParseParams }

///  Cria uma nova instância de TParseParams
constructor TSQLParamsParser.Create;
begin
  inherited;
  FParams := TStringList.Create;
  FMacroParams := TStringList.Create;
end;

///  Destrói o objeto
destructor TSQLParamsParser.Destroy;
begin
  FParams.Free;
  FMacroParams.Free;
  inherited;
end;

{**
  Parse analisa a instrução SQL à procura de parâmetros e macros.
  Procura por parâmetros no formato :<nome_param>
  e macros no formato #<nome_da_macro>. Os parâmetros encontrados são colocados
  numa lista e podem ser recuperados através da função GetParams.
  As macros encontradas são colocadas numa lista e podem ser recuperados através
  da função GetMacroParams.

  @param pSql instrução SQL que será analisada
}
function TSQLParamsParser.Parse(const pSQL: string): string;
const
  cExpRegCommentsML = '(\/\*(.*?)\*\/)'; // comentarios no formato /* ... */
  cExpRegCommentsInLine = '--(.*?)$'; // comentarios no formato -- ...
  cExpRegInvalidMacros = '##\w+'; // macros no formato ##<nome> são inválidas
  cExpRegInvalidParams = '::\w+'; // params no formato ::<nome> são inválidos
var
  vSql: string;
  vRegEx: TRegExpr;
begin
  FParams.Clear;
  FMacroParams.Clear;
  vRegEx := TRegExpr.Create;
  try
    // Elimina do texto tudo que deve ser ignorado: comentários,
    // parametros e macros inválidas
    vRegEx.Expression := cExpRegCommentsML+'|'+cExpRegInvalidMacros+'|'+
      cExpRegInvalidParams+'|'+cExpRegCommentsInline;
    vRegEx.ModifierM := True;
    vSql := vRegEx.Replace(pSQL, '', False)+' ';

    // Verifica se existe algum(a) param/macro sem nome
    vRegEx.Expression := '[:#]$|\s[:#]\s';
    if vRegEx.Exec (vSql) then
      raise EPersistenceParserError.Create(cErrorParamParserInvalidParam);

    // Depois de remover do texto as partes a serem ignoradas,
    // procuramos por parametros e macros válidos
    vRegEx.Expression := '[\s\(=]:(\w+)[^w]|[\s\(=]#(\w+)[^w]|^#(\w+)[^w]';
    if vRegEx.Exec(vSql) then
    repeat
      if vRegEx.MatchPos[1] > 0 then
        FParams.Add(System.Copy(vSql, vRegEx.MatchPos[1], vRegEx.MatchLen[1]));
      if vRegEx.MatchPos[2] > 0 then
        FMacroParams.Add(System.Copy(vSql, vRegEx.MatchPos[2], vRegEx.MatchLen[2]));
      if vRegEx.MatchPos[3] > 0 then
        FMacroParams.Add(System.Copy(vSql, vRegEx.MatchPos[3], vRegEx.MatchLen[3]));
    until not vRegEx.ExecNext;

    vRegEx.Expression := ':(\w+)';
    Result := vRegEx.Replace(pSQL, '?', False);
  finally
    vRegEx.Free;
  end;
end;

{**
  Retorna um TStrings com a lista de macros
  
  @return Retorna um TStrings com a lista de parametros encontrados na instrução
    SQL durante o Parse
}
function TSQLParamsParser.GetMacroParams: TStrings;
begin
  Result := FMacroParams;
end;

{**
  Retorna um TStrings com a lista de parametros

  @return Retorna um TStrings com a lista de parametros encontrados na instrução
    SQL durante o Parse
}
function TSQLParamsParser.GetParams: TStrings;
begin
  Result := FParams;
end;

end.

