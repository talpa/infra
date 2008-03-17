// fix uses
unit InfraAspectUtil;

interface

{$I 'InfraAspect.Inc'}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraCommonIntf;

type
  TStubArray = array of Pointer;

function CreateStub(var FStubs: TStubArray;
  pMethodInfo: IMethodInfo): integer;

const
  SIZE_OF_STUB = $3E;   // 62 (size of stub's instructions)

implementation

uses
  Types,
  Classes,
  Windows,
  SysUtils,
  InfraVMTUtil,
  InfraAspectIntf;

const
  METHOD_INDEX = 12;  // the position of MethodIndex on stub
  START_STACK = 32;   // the stack is 32 positions after actual stack's point
                      // because pushs on starting stub

type
  TByteArray = array[0..1024] of byte;
  TDwordArray = array[0..1024] of dword;
             
{
  Adiciona o parametro que corresponde ao Resultado de uma função no final
  da mesma lista onde estão os parâmetros do método.
}
procedure PackResult(const List: IInterfaceList; const Index: integer;
  const pMethodInfo: IMethodInfo; Param: Pointer);
var
  Count: integer;
begin
  if pMethodInfo.IsFunction then
    Count := pMethodInfo.Parameters.Count-1
  else
    Count := pMethodInfo.Parameters.Count;
  if Index >= Count then
    raise exception.Create('MethodMember index out of bounds');
  List.Add(IInterface(Param));
end;

{
  Empacota os registradores, pilha, e resultado (caso método seja uma função)
  em uma InterfaceList para poder passar isso para a chamada dos Advices.

  Quando método é uma função o numero de parametros tem de ser incrementado
  aqui por que o resultado deve ser adicionado a lista de interfaces.
}
function PackParams(const pMethodInfo: IMethodInfo; reg_edx,
  reg_ecx, reg_esp, reg_eax: Pointer): IInterfaceList;
var
  StackParams: PParams;
  i, j, ParamsPacked: integer;
  QtdParams: integer;
begin
  Result := nil;
  ParamsPacked := 0;
  QtdParams := pMethodInfo.Parameters.Count;
  j := 0;
  if Assigned(pMethodInfo.ReturnType) then
  begin
    Dec(QtdParams);
    Inc(j);
  end;
  if (QtdParams = 0) then
    Exit;
  if QtdParams > 0 then
  begin
    Result := TInterFaceList.Create;
    PackResult(Result, 0, pMethodInfo, reg_edx);
    Inc(ParamsPacked)
  end;
  if QtdParams > 1 then
  begin
    PackResult(Result, 1, pMethodInfo, reg_ecx);
    Inc(ParamsPacked);
  end;
  if QtdParams > 2 then
  begin
    StackParams := Pointer(Integer(reg_esp)+START_STACK);
    for i := QtdParams-ParamsPacked-1 downto 0 do
      PackResult(Result, i+ParamsPacked, pMethodInfo, Pointer(StackParams[i+j]))
  end;
end;

{
  Este método é chamado pelo Stub quando um método interceptado é alcancado
  pela execução da aplicacao.

  O Objetivo é encontrar o primeiro JointPoint compatível com o método
  chamado pelo programador e chamar os Advices (Before, After e/ou Around)
  dos seus aspectos.

  Antes de chamar os Advices os parametros precisam ser empacotados em uma
  IInterfaceList. Métodos do tipo register guardam os parametros da seguinte
  forma:
  Primeiro Parametro:         Registrador EAX
  Segundo Parametro:          Registrador ECX
  Terceiro Parametro:         Registrador EDX
  Quarto Parametro em diante: na pilha, Registrador ESP aponta para o topo da
                              pilha.

  Quando o Método do tipo register chamado é uma função, o retorno da função
  tambem pode estar em diferentes lugares dependendo da quantidade de
  parametros dos métodos. O resultado da função estará em:

  EDX:            caso a função tenha 1 parâmetro;
  ECX:            caso a função tenha 2 parâmetros;
  No topo de ESP: caso a função tenha mais de 2 parâmetros;

  Esta função precisa retorna a quantidade de parametros definidos em
  MethodInfo (Method Metadata Information) para que o stub possa ser limpo
  corretamente.
}
function Call_Aspects(var pParamCount: Integer; pMethodIndex: integer;
  reg_esp, reg_edx, reg_ecx, reg_eax: Pointer): dword; stdcall;
var
  i: integer;
  MethodInfo: IMethodInfo;
  Params: IInterfaceList;
  MethodResult: IInterface;
  ResultAdr: Pointer;
begin
  Result := 0;
  with AspectService do
  begin
    for i := 0 to JointPoints.Count-1 do
    begin
      MethodInfo := JointPoints[i].MethodInfo;
      if (MethodInfo.DeclaringType.ImplClass = TObject(reg_eax).ClassType) and
        (JointPoints[i].MethodIndex = pMethodIndex) then
      begin
        pParamCount := JointPoints[i].ParamsCount;
        Params := PackParams(MethodInfo, reg_edx, reg_ecx, reg_esp,
          reg_eax);
        MethodResult := CallAdvices(JointPoints[i], reg_eax, Params);
        if MethodInfo.IsFunction then
        begin
          case pParamCount of
            1: ResultAdr := reg_edx;
            2: ResultAdr := reg_ecx;
          else
            ResultAdr := Pointer(PParams(Integer(reg_esp)+START_STACK)[0]);
          end;
          Pointer(ResultAdr^) := Pointer(MethodResult);
          IInterface(ResultAdr^)._AddRef;
        end;
        Break;
      end;
    end;
  end;
end;

var
  ADDR_CALLASPECTS: Pointer = @Call_Aspects;

{
  Atenção !!!
  Se houver alterações entre o primeiro push e o call, os seguintes parâmetros
  deverão ser alterados:
  - SIZE_OF_STUB: O tamanho, contado de 2 em 2, das instruções exibidas na
    CPU Window;
  - METHOD_INDEX: O quantidade de instruções, contado de 2 em 2, até se chegar
    ao segundo push$ 00;
  - START_STACK: A quantidade de parametro de Call_aspect * 4;

  O SIZE_OF_STUB provavelmente será alterada sempre que houver alguma
  mudança no stub;
}
procedure stub;
asm
  push esi         // armazena ESI
  push edi         // armazena EDI
  push ebx         // armazena EBX
  push $00         // variavel local para guarda a quantidade de parametros
  mov ebx, esp     // EBX = @qtd_params
  push eax         // reg_eax, contar START_STACK até aqui (quantidade de bytes das instruções)
  push ecx         // reg_ecx
  push edx         // reg_edx
  push esp         // reg_esp
  push $00         // pmethodIndex (substituido na criação do stub), METHOD_INDEX até aqui (quantide de instruções)
  push ebx         // joga qtdParams (nossa variável local) na pilha
  // Este call remove todos os parametros colocados na pilha voltando
  // para a posição onde foi dada o primeiro push $00
  call [ADDR_CALLASPECTS]
  pop ecx           // joga qtdParams em ECX
  mov esi, esp      // move posição da pilha para esi para recuperação dos registradores no final
  add esp, 16       // move ponteiro da pilha para posição do primeiro parâmetro empilhado (se houve empilhamento)
  mov edi, eax      // guarda o resultado da função em EDI (quando resultado de callaspects é primitivo)
  sub ecx, 2        // calcula a quantidade de parâmetros empilhados
  cmp ecx, 1        // Se for menor que 1
  jl @skip          // então pula essa parte
  mov eax, 4        // coloca 4 em eax para poder corrigir a pilha
  mul eax, ecx      // multiplica EAX pela quantidade de parâmetros
  add esp, eax      // posiciona a pilha após o parâmetros empilhados quando na entrada do stub
@skip:
  mov eax, edi      // restaura o result (quando resultado de callaspects é primitivo)
  mov ecx, esi      // poe em ECX a posição da pilha onde estão os registradores
  mov ebx, [ecx]    // Restaura EBX
  mov edi, [ecx+4]  // restaura EDI
  mov esi, [ecx+8]  // restaura ESI
  mov ecx, [ecx+12] // restautra endereço de retorno
  jmp ecx           // pula para o endereço de retorno e continua a execução
end;

function CreateStub(var FStubs: TStubArray;
  pMethodInfo: IMethodInfo): integer;
var
  stub_buf: Pointer;
  i: byte;
  mbi: TMemoryBasicInformation;
  old: cardinal;
  VMT: TClass;
begin
  VMT := pMethodInfo.DeclaringType.ImplClass;
  GetMem(stub_buf, SIZE_OF_STUB);
  SetLength(FStubs, Length(FStubs)+1);
  FStubs[Length(FStubs)-1] := stub_buf;
  CopyMemory(stub_buf, @stub, SIZE_OF_STUB);
  {$IFDEF USE_DEBUG_STUB}
  stub_buf := @stub;
  {$ENDIF}
  i := GetVirtualMethodCount(VMT);
  while i > 0 do
    if GetVirtualMethod(VMT, i) = pMethodInfo.MethodPointer then
      Break
    else
      Dec(i);
  if i = 0 then
    Exception.Create('Virtual Method "'+pMethodInfo.Name+'" not Found!');
  VirtualQueryEx(GetCurrentProcess, Pointer(vmt), mbi, sizeof(mbi));
  VirtualProtect(Pointer(integer(VMT)+i*4), 4, PAGE_EXECUTE_READWRITE, old);
  if (mbi.Protect and PAGE_READWRITE) <> 0 then
    Exception.Create('Bad Read/Write Page');
  if IsBadReadPtr(Pointer(VMT),4) then
    raise Exception.Create('Bad Read Ptr');
  TDwordArray(Pointer(VMT)^)[i] := dword(stub_buf);
  {$IFDEF USE_DEBUG_STUB}
  VirtualProtect(stub_buf, SIZE_OF_STUB, PAGE_EXECUTE_READWRITE, old);
  {$ENDIF}
  TByteArray(stub_buf^)[METHOD_INDEX] := dword(i);
  FlushInstructionCache(GetCurrentProcess, stub_buf, SIZE_OF_STUB);
  Result := i;
end;

end.
