unit InfraVMTUtil;

interface

uses
  Types;
  
type
  TParams = array[0..50] of dword;
  PParams = ^TParams;

function GetVirtualMethodCount(AClass: TClass): Integer;
function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;

implementation

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

end.
