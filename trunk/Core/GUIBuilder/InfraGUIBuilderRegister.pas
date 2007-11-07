unit InfraGUIBuilderRegister;

interface

procedure RegisterOnReflection;

implementation

uses
  InfraCommonIntf,
  GUIAnnotation,
  GUIAnnotationIntf;

procedure RegisterOnReflection;
begin
  with TypeService do
  begin
    AddType(IScreenItem, 'ScreenItem', TScreenItem, IElement);
    AddType(IScreenControl, 'ScreenControl', TScreenControl, IScreenItem);
    AddType(IScreenGroup, 'ScreenGroup', TScreenGroup, IScreenItem);
    AddType(IScreen, 'Screen', TScreen, IElement);
    AddType(IScreens, 'Screens', TScreens, IElement);
  end;
end;

end.
