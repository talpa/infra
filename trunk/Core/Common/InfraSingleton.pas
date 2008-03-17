unit InfraSingleton;

interface

{$I InfraCommon.Inc}

uses
  {$IFDEF USE_GXDEBUG}DBugIntf, {$ENDIF}
  InfraCommon;

type
  TInfraSingleton = class(TElement)
  private
    procedure Dispose;
  protected
    procedure Init; virtual;
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

implementation

uses
  Classes;

var
  InfraSingletons: TStringList;

{ TInfraSingleton }

procedure TInfraSingleton.Dispose;
begin
  Inherited FreeInstance;
end;

procedure TInfraSingleton.FreeInstance;
begin
  // ... do nothing
end;

procedure TInfraSingleton.Init;
begin
  // ... do nothing
end;

class function TInfraSingleton.NewInstance: TObject;
var
  Singleton: TInfraSingleton;
  Index: integer;
begin
  if InfraSingletons = nil then
    InfraSingletons := TStringList.Create;
  Index := InfraSingletons.IndexOf(Self.Classname);
  if Index < 0 then
  begin
    Singleton := TInfraSingleton(inherited NewInstance);
    try
      Singleton.Init;
      Index := InfraSingletons.AddObject(Self.Classname, Singleton);
      Singleton._Addref;
    except
      Singleton.Dispose;
      raise;
    end;
  end;
  Result := InfraSingletons.Objects[Index] as TInfraSingleton;
end;

procedure ClearInfraSingletons;
var
  i: Integer;
begin
  // free all singleton and InterfacedSingleton objects.
  for I := 0 to InfraSingletons.Count - 1 do
  begin
    if InfraSingletons.Objects[I] Is TInfraSingleton then
      TInfraSingleton(InfraSingletons.Objects[I]).Dispose
    else
      TInfraSingleton(InfraSingletons.Objects[I])._Release;
  end;
  InfraSingletons.Free;
end;

initialization
  InfraSingletons := nil;

finalization
  if Assigned(InfraSingletons) then
    ClearInfraSingletons;

end.
