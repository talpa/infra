unit InfraAspectVMTUtil;

interface

{$I 'Infra.Inc'}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  Windows,
  InfraCommonIntf,
  InfraValueTypeIntf;

type
  TStubArray = array of Pointer;
  TByteArray = array[0..1024] of byte;
  TDwordArray = array[0..1024] of dword;
  TParams = array[0..50] of dword;
  PParams = ^TParams;

function InvokeMethod(pInstance: TObject; pMethodInfo: IClassInfo;
  const pParams: IInfraList): IInfraType;

function CreateStub(var FStubs: TStubArray;
  pMethodInfo: IClassInfo): integer;

function InvokeRealMethod(pInstance: TObject; pMethod: Pointer;
  pParamCount: integer; pArrayParams: Pointer;
  pMethodResultKind: TMethodResultKind): IInfraType;

const
  SIZE_OF_STUB = $3E;   // 62 (size of stub's instructions)

implementation

uses
  SysUtils,
  InfraValueType,
  InfraAspectIntf;

const
  METHOD_INDEX = 12;  // the position of MethodIndex on stub
  START_STACK = 32;   // the stack is 32 positions after actual stack's point
                      // because pushs on starting stub

procedure AddParam(const List: IInfraList; const Index: integer;
  const pMethodInfo: IClassInfo; Param: Pointer);
var
  Count: integer;
begin
  if pMethodInfo.IsFunction then
    Count := pMethodInfo.MemberInfos.Count-1
  else
    Count := pMethodInfo.MemberInfos.Count;
  if Index >= Count then
    raise exception.Create('MethodMember index out of bounds');
  if IsEqualGUID(
    (pMethodInfo.MemberInfos[Index] as IClassInfo).TypeId, IInfraType) then
    List.Add(IInfraType(Param))
  else
    List.Add(IInterface(Param) as IInfraType);
end;

function GetParamPointer(const Index: integer;
  const pMethodInfo: IClassInfo; const Param: IInterface): Pointer;
var
  pType: IInfraType;
begin
  if pMethodInfo.MemberInfos.Count <= Index then
    raise exception.Create('MethodMember index out of bounds');
  if Supports(Param,
    (pMethodInfo.MemberInfos[Index] as IClassInfo).TypeId, pType) then
    Result := Pointer(pType)
  else
    raise exception.Create('Cannot unpack param. Incompatible types');
end;

// Convert te array of dwords (TParams) and registers to a InfraList.
// When a method is function the number of parameters is incremented in 1.
function PackTypeParams(const pMethodInfo: IClassInfo; reg_edx,
  reg_ecx, reg_esp, reg_eax: Pointer): IInfraList;
var
  StackParams: PParams;
  i, j, ParamsPacked: integer;
  QtdParams: integer;
begin
  Result := nil;
  ParamsPacked := 0;
  QtdParams := pMethodInfo.MemberInfos.Count;
  j := 0;
  if pMethodInfo.IsFunction then
  begin
    Dec(QtdParams);
    Inc(j);
  end;
  if (QtdParams = 0) then
    Exit;
  if QtdParams > 0 then
  begin
    Result := TInfraList.Create;
    AddParam(Result, 0, pMethodInfo, reg_edx);
    Inc(ParamsPacked)
  end;
  if QtdParams > 1 then
  begin
    AddParam(Result, 1, pMethodInfo, reg_ecx);
    Inc(ParamsPacked);
  end;
  if QtdParams > 2 then
  begin
    StackParams := Pointer(Integer(reg_esp)+START_STACK);
    for i := QtdParams-ParamsPacked-1 downto 0 do
      AddParam(Result, i+ParamsPacked, pMethodInfo, Pointer(StackParams[i+j]))
  end;
end;

procedure UnPackTypeParams(const pMethodInfo: IClassInfo;
  const Params: IInfraList; var StackParams: TParams);
var
  i, QtdParams: integer;
begin
  if pMethodInfo.IsFunction then
    QtdParams := pMethodInfo.MemberInfos.Count-1
  else
    QtdParams := pMethodInfo.MemberInfos.Count;
  if QtdParams <> 0 then
    for i := 0 to Pred(QtdParams) do
      StackParams[i] :=
        dword(GetParamPointer(i, pMethodInfo, IInterface(Params[i])));
end;

// Search the first JointPoint match with the method that was called by
// user and call your Advices. This method return the number of parameters
// defined on MethodInfo (Method Metadata Information) to that stub can do
// cleaning.
function Call_Aspects(var pParamCount: Integer; pMethodIndex: integer;
  reg_esp, reg_edx, reg_ecx, reg_eax: Pointer): dword; stdcall;
var
  i: integer;
  MethodInfo: IClassInfo;
  Params: IInfraList;
  ResultType: IInfraType;
  ResultAdr: Pointer;
begin
  Result := 0;
  with AspectService do
  begin
    for i := 0 to JointPoints.Count-1 do
    begin
      MethodInfo := JointPoints[i].MethodInfo;
      if (MethodInfo.Owner.ImplClass = TObject(reg_eax).ClassType) and
        (JointPoints[i].MethodIndex = pMethodIndex) then
      begin
        pParamCount := JointPoints[i].ParamsCount;
        Params := PackTypeParams(MethodInfo, reg_edx, reg_ecx, reg_esp,
          reg_eax);
        ResultType := CallAdvices(JointPoints[i], reg_eax, Params);
        if MethodInfo.IsFunction then
        begin
          case pParamCount of
            1: ResultAdr := reg_edx;
            2: ResultAdr := reg_ecx;
          else
            ResultAdr := Pointer(PParams(Integer(reg_esp)+START_STACK)[0]);
          end;
          Pointer(ResultAdr^) := Pointer(ResultType);
          IInfraType(ResultAdr^)._AddRef;
        end;
        Break;
      end;
    end;
  end;
end;

var
  ADDR_CALLASPECTS: Pointer = @Call_Aspects;

function InvokeMethod(pInstance: TObject; pMethodInfo: IClassInfo;
  const pParams: IInfraList): IInfraType;
var
  vStackParams: TParams;
begin
  FillChar(vStackParams, SizeOf(TParams), 0);
  UnPackTypeParams(pMethodInfo, pParams, vStackParams);
  with pMethodInfo do
    Result := InvokeRealMethod(pInstance, Method, MemberInfos.Count,
      @vStackParams, pMethodInfo.MethodResultKind);
end;

// Invoke the real method passing to it the parameters.
function InvokeRealMethod(pInstance: TObject; pMethod: Pointer;
  pParamCount: integer; pArrayParams: Pointer;
  pMethodResultKind: TMethodResultKind): IInfraType;
begin
  // Is need extra param when returning interface, string or record type.
  if pMethodResultKind <> mrkNone then
    TParams(pArrayParams^)[pParamCount-1] := Dword(@Result);
  asm
    push esp                        // store ESP
    push ebx                        // store EBX
    mov ebx, pParamCount            // EBX := pParamCount
    cmp ebx, 0                      // if pParamCount = 0 then
    je @CallMethod                  //   CallMethod
    cmp ebx, 1                      // else if pParamCount <> 1 then
    jne @Two_Params                 //   Two_Params
                                    // else
    mov ebx, pArrayParams           //   EBX := pArrayParams
    mov edx, [ebx]                  //   put the 1st param into EDX
    jmp @CallMethod                 //   CallMethod
  @Two_Params:
    cmp ebx, 2                      // if pParamCount <> 2 then
    jne @GreaterThanTwoParams       //   GreaterThanTwoParams
                                    // else
    mov ebx, pArrayParams           //   EBX := pArrayParams
    mov edx, [ebx]                  //   put the 1st param into EDX
    mov ecx, [ebx+4]                //   put the 2nd param into ECX
    jmp @CallMethod                 //   CallMethod
  @GreaterThanTwoParams:
    mov ebx, pArrayParams           // EBX := pArrayParams
    add ebx, 4                      // position at 2nd parameter
    mov ecx, pParamCount            // put pParamCount into ECX
    sub ecx, 2                      // pParamCount := pParamCount - 2
  @StackUP:
    add ebx, 4                      // position at next parameter
    mov eax, [ebx]                  // move the value of parameter to EAX
    push eax                        // stack up EAX
    loop @StackUP                   // loop (Decrement ECX to 0)
    mov ebx, pArrayParams           // put pArrayParams into EBX
    mov edx, [ebx]                  // put the 1st array item into EDX (1nd par)
    mov ecx, [ebx+4]                // put the 2nd array item into ECX (2nd par)
  @CallMethod:
    mov eax, pInstance              // put pInstance into EAX
    call pMethod                    // Call real method
    pop ebx                         // remove stack item ebx
    pop esp                         // remove stack item esp
  end;
end;

{
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
  // Este call remove todos os parametros colocados na pilha voltando para a posição onde foi dada o primeiro push $00
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

procedure stub_o;
asm
  push $00      // var qtd_params: integer = 0; // vai receber o número de parâmetros
  mov ebx, esp  // ebx = @qtd_params
  push eax
  push ecx
  push edx
  push esp
  push $00
  push ebx
  call [ADDR_CALLASPECTS]
  pop ecx         // ecx = qtd_params
  mov ebx, [esp]  // ebx = endereço de retorno
  mov esi, eax    // guarda o resultado da função
  sub ecx, 2
  cmp ecx, 1
  jl @skip        // se recebeu mais de 2 parâmetros, corrige a pilha
  mov eax, 4
  mul eax, ecx
  add esp, eax    // remove os parâmetros da pilha
@skip:
  add esp, 4      // remove o endereço de retorno
  mov eax, esi    // restaura o result
  jmp ebx
end;

function GetVirtualMethodCount(AClass: TClass): Integer;
var
  BeginVMT: Longint;
  EndVMT: Longint;
  TablePointer: Longint;
  I: Integer;
begin
  BeginVMT := Longint(AClass);
  // Scan the offset entries in the class table for the various fields,
  // namely vmtIntfTable, vmtAutoTable, ..., vmtDynamicTable
  // The last entry is always the vmtClassName, so stop once we got there
  // After the last virtual method there is one of these entries.
  EndVMT := PLongint(LongInt(AClass) + vmtClassName)^;
  // Set iterator to first item behind VMT table Pointer
  I := vmtSelfPtr + SizeOf(Pointer);
  repeat
    TablePointer := PLongint(Longint(AClass) + I)^;
    if (TablePointer <> 0) and (TablePointer >= BeginVMT) and
       (TablePointer < EndVMT) then
      EndVMT := Longint(TablePointer);
    Inc(I, SizeOf(Pointer));
  until I >= vmtClassName;
  Result := (EndVMT - BeginVMT) div SizeOf(Pointer);
end;

function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
begin
  Result := PPointer(Integer(AClass) + Index * SizeOf(Pointer))^;
end;

function CreateStub(var FStubs: TStubArray;
  pMethodInfo: IClassInfo): integer;
var
  stub_buf: Pointer;
  i: byte;
  mbi: TMemoryBasicInformation;
  old: cardinal;
  VMT: TClass;
begin
  VMT := pMethodInfo.Owner.ImplClass;
  GetMem(stub_buf, SIZE_OF_STUB);
  SetLength(FStubs, Length(FStubs)+1);
  FStubs[Length(FStubs)-1] := stub_buf;
  CopyMemory(stub_buf, @stub, SIZE_OF_STUB);
  {$IFDEF USE_DEBUG_STUB}
  stub_buf := @stub;
  {$ENDIF}
  i := GetVirtualMethodCount(VMT);
  while i > 0 do
    if GetVirtualMethod(VMT, i) = pMethodInfo.Method then
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
