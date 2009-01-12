unit InfraAspectEval;

{$I 'InfraAspect.Inc'}                                                   

interface

uses
  {$IFDEF USE_GXDEBUG}DBugIntf,{$ENDIF}
  Classes,
  InfraAspectIntf;

const
  cTokenNOT = -1;
  cTokenAND = -2;
  cTokenOR = -3;
  cTokenOpeningParenthesis = -4;
  cTokenClosingParenthesis = -5;
  cTokenEND = -6;
  cStartLetters = ['a'..'z', 'A'..'Z'];
  cLetters = cStartLetters + ['_', '0'..'9', '*', '?', '.'];

type
  (*
    Avaliador de expressoes onde pode-se pesquisar por alguma string
    usando expressoes lógicas como por exemplo:

    1 - (TPessoa.Get* OR TNota*.SetMetodo?) and not TPessoa.GetNome
    2 - (uml AND pattern) OR (design AND (NOT "use case"))

    As regras de precedencia são dadas pela seguinte gramática IEBNF
    (Indented Extended Backus Naur):
    
    expressao = termo { OR termo }
    termo = fator { AND fator }
    fator = STRING | NOT fator | '(' expressao ')'

    Usado pela programação orientada a Aspectos no Infra mas pode ser utilizado
    para outros propósitos.

    Um PointCut são pontos na aplicação que serão interceptados por aspectos
    que devem ser executados no luga do método real.

    Este código é uma adaptação do código existente no site:
    Felix John COLIBRI 2004 at http://www.felix-colibri.com

    http://www.felix-colibri.com/papers/colibri_utilities/
      the_coliget_search_engine/the_coliget_search_engine.html
  *)
  TAspectPointcutEval = class(TInterfacedObject, IAspectPointcutEval)
  private
    FExpression: string;
    FExpressionIndex: Integer;
    FExpressionLength: Integer;
    FExpressionIdentifiers: TStrings;
    FTokens: array of Integer;
    FTokenPositions: array of Integer;
    FTokenLengths: array of Integer;
    FTokenIndex: Integer;
    FTokenCount: Integer;
    FErrorIndex: Integer;
    FErrorLength: Integer;
    FErrorCount: Integer;
    FFirstErrorTokenIndex: Integer;
    FSymbolType: Integer;
    FTextToMatch: string;
    // Avalia uma expressao
    function EvaluateExpression: Boolean;
    // Ler o proximo token
    procedure ReadNextSymbol;
    // Avalia um Fator. Os fatores são os pedaços de string
    // que podem ser avaliados em relação a string que está sendo procurada
    // neste caso utilizamos o Mask.MatchesMask.
    function EvaluateFactor: Boolean;
    // Avalia um Termo
    function EvaluateTerm: Boolean;
    // Analiza identificadores
    procedure CountAnalyzeIdentifier;
    // Conta os tokens e identifica erros na expressão.
    procedure CountIdentifiersAndTokens;
    // Conta as string literais, estilo "alguma string"
    procedure CountAnalyzeLitteralString;
    // Analiza a matriz de identificadores e tokens
    procedure FillIdentifierListAndTokenArray;
    // Analiza identificador
    procedure FillAnalyzeIdentifier;
    // Analiza string literal, estilo "alguma string"
    procedure FillAnalyzeLitteralString;
    // Conta os identificadores e tokens preenchendo uma matriz caso não
    // haja erros na expressão.
    procedure TokenizeRequest;
  public
    // Avalia se uma string é compativel com a expressao.
    function Evaluate(const TextToMatch, Expression: string): Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Masks;

constructor TAspectPointcutEval.Create;
begin
  FExpressionIdentifiers:= TStringList.Create;
end;

destructor TAspectPointcutEval.Destroy;
begin
  FExpressionIdentifiers.Free;
  inherited;
end;

procedure TAspectPointcutEval.TokenizeRequest;
begin
  CountIdentifiersAndTokens;
  if FErrorIndex = 0 then
    FillIdentifierListAndTokenArray;
end;

procedure TAspectPointcutEval.CountAnalyzeIdentifier;
begin
  while (FExpressionIndex <= FExpressionLength)
    and (FExpression[FExpressionIndex] in cLetters) do
    Inc(FExpressionIndex);
  Inc(FTokenCount);
end;

procedure TAspectPointcutEval.CountAnalyzeLitteralString;
begin
  // skip "
  Inc(FExpressionIndex);
  while (FExpressionIndex <= FExpressionLength) and
    (FExpression[FExpressionIndex] <> '"') do
    Inc(FExpressionIndex);
  // skip "
  Inc(FExpressionIndex);
  Inc(FTokenCount);
end;

procedure TAspectPointcutEval.CountIdentifiersAndTokens;
var
  FQtyFreeParentesis: integer;
begin
  FExpressionIndex:= 1;
  FTokenCount:= 0;
  FQtyFreeParentesis := 0;
  while FExpressionIndex <= FExpressionLength do
  begin
    case FExpression[FExpressionIndex] of
      'a'..'z', 'A'..'Z', '0'..'9', '*', '?', '.': CountAnalyzeIdentifier;
      ' ': Inc(FExpressionIndex);
      '(', ')', '|':
      begin
        case FExpression[FExpressionIndex] of
          '(': Inc(FQtyFreeParentesis);
          ')': Dec(FQtyFreeParentesis);
        end;
        Inc(FExpressionIndex);
        Inc(FTokenCount);
      end;
      '"': CountAnalyzeLitteralString;
    else
      raise Exception.Create('Invalid token at position '+
        IntToStr(FExpressionIndex) +
        #13 + 'Token: ' + FExpression[FExpressionIndex]);
      FErrorIndex:= FExpressionLength;
      FErrorLength:= 1;
      Break;
    end;
  end;
  if FQtyFreeParentesis > 0 then
    raise Exception.Create('Closing parenthesis expected')
  else if FQtyFreeParentesis < 0 then
    raise Exception.Create('Opening parenthesis expected');
  if FErrorIndex = 0 then
  begin
    SetLength(FTokens, FTokenCount);
    SetLength(FTokenPositions, FTokenCount);
    SetLength(FTokenLengths, FTokenCount);
  end;
end;

procedure TAspectPointcutEval.FillAnalyzeIdentifier;
var
  iStartIndex, iIdentifierLength, iIndex: Integer;
  sIdentifier, sLowercaseIdentifier: string;
begin
  iStartIndex:= FExpressionIndex;
  while (FExpressionIndex <= FExpressionLength)
    and (FExpression[FExpressionIndex] in cLetters) do
    Inc(FExpressionIndex);
  iIdentifierLength:= FExpressionIndex - iStartIndex;
  SetLength(sIdentifier, iIdentifierLength);
  Move(FExpression[iStartIndex], sIdentifier[1], iIdentifierLength);
  sLowercaseIdentifier:= LowerCase(sIdentifier);
  if sLowercaseIdentifier = 'and' then
    FTokens[FTokenCount]:= cTokenAND
  else if sLowercaseIdentifier = 'or' then
    FTokens[FTokenCount]:= cTokenOR
  else if sLowercaseIdentifier = 'not' then
    FTokens[FTokenCount]:= cTokenNOT
  else
  begin
    iIndex:= FExpressionIdentifiers.Add(sLowercaseIdentifier);
    FTokens[FTokenCount]:= iIndex;
  end;
  FTokenPositions[FTokenCount]:= iStartIndex;
  FTokenLengths[FTokenCount]:= iIdentifierLength;
  Inc(FTokenCount);
end;

procedure TAspectPointcutEval.FillAnalyzeLitteralString;
var
  iStartIndex, iIdentifierLength, iIndex: Integer;
  sIdentifier, sLowercaseIdentifier: string;
begin
  // -- skip "
  Inc(FExpressionIndex);
  iStartIndex:= FExpressionIndex;
  while (FExpressionIndex <= FExpressionLength) and
    (FExpression[FExpressionIndex] <> '"') do
    Inc(FExpressionIndex);
  iIdentifierLength:= FExpressionIndex - iStartIndex;
  SetLength(sIdentifier, iIdentifierLength);
  Move(FExpression[iStartIndex], sIdentifier[1], iIdentifierLength);
  sLowercaseIdentifier:= LowerCase(sIdentifier);
  iIndex:= FExpressionIdentifiers.Add(sLowercaseIdentifier);
  FTokens[FTokenCount]:= iIndex;
  FTokenPositions[FTokenCount]:= iStartIndex;
  FTokenLengths[FTokenCount]:= iIdentifierLength;
  // -- skip "
  Inc(FExpressionIndex);
  Inc(FTokenCount);
end;


procedure TAspectPointcutEval.FillIdentifierListAndTokenArray;
begin
  FExpressionIndex:= 1;
  FTokenCount:= 0;
  while (FExpressionIndex <= FExpressionLength) and (FErrorIndex = 0) do
  begin
    case FExpression[FExpressionIndex] of
      'a'..'z', 'A'..'Z', '0'..'9', '*', '?', '.': FillAnalyzeIdentifier;
      '"': FillAnalyzeLitteralString;
      ' ': Inc(FExpressionIndex);
      '(':
      begin
        FTokens[FTokenCount]:= cTokenOpeningParenthesis;
        FTokenPositions[FTokenCount]:= FExpressionIndex;
        FTokenLengths[FTokenCount]:= 1;
        Inc(FExpressionIndex);
        Inc(FTokenCount);
      end;
      ')':
      begin
        FTokens[FTokenCount]:= cTokenClosingParenthesis;
        FTokenPositions[FTokenCount]:= FExpressionIndex;
        FTokenLengths[FTokenCount]:= 1;
        Inc(FExpressionIndex);
        Inc(FTokenCount);
      end;
      '|':
      begin
        FTokens[FTokenCount]:= cTokenEND;
        FTokenPositions[FTokenCount]:= FExpressionIndex;
        FTokenLengths[FTokenCount]:= 1;
        Inc(FExpressionIndex);
        Inc(FTokenCount);
      end;
    else
      FErrorIndex:= FExpressionIndex;
      FErrorLength:= 1;
      Break;
    end;
  end;
end;

procedure TAspectPointcutEval.ReadNextSymbol;
begin
  FSymbolType := FTokens[FTokenIndex];
  Inc(FTokenIndex);
end;

function TAspectPointcutEval.Evaluate(const TextToMatch,
  Expression: string): Boolean;
begin
  Result:= False;
  FExpressionIdentifiers.Clear;
  FTokens:= nil;
  FTokenPositions:= nil;
  FTokenLengths:= nil;
  FTextToMatch:= TextToMatch;
  FExpression:= Expression + '|';
  FExpressionLength:= Length(FExpression);
  TokenizeRequest;
  if FTokenCount > 0 then
  begin
    FErrorCount:= 0;
    FFirstErrorTokenIndex:= -1;
    FTokenIndex:= 0;
    ReadNextSymbol;
    Result := EvaluateExpression;
  end;
end;

function TAspectPointcutEval.EvaluateExpression: Boolean;
begin
  Result:= EvaluateTerm;
  while FSymbolType = cTokenOR do
  begin
    ReadNextSymbol;
    Result:= Result or EvaluateTerm;
  end;
end;

function TAspectPointcutEval.EvaluateTerm: Boolean;
begin
  Result:= EvaluateFactor;
  while FSymbolType = cTokenAND do
  begin
    ReadNextSymbol;
    Result:= Result and EvaluateFactor;
  end;
end;

function TAspectPointcutEval.EvaluateFactor: Boolean;
var
  sIdentifier: string;
begin
  case FSymbolType of
    cTokenOpeningParenthesis:
    begin
      ReadNextSymbol;
      Result := EvaluateExpression;
      if FSymbolType = cTokenClosingParenthesis then
        ReadNextSymbol;
    end;
    cTokenNOT:
    begin
      ReadNextSymbol;
      Result:= not EvaluateFactor;
    end;
  else
    if FSymbolType >= 0 then
    begin
      sIdentifier:= FExpressionIdentifiers[FSymbolType];
      Result:= MatchesMask(FTextToMatch, sIdentifier);
      ReadNextSymbol;
    end else
      Result:= False;
  end;
end;

end.

