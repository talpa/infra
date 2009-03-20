unit InfraBindingManager;

interface

uses
  Controls,
  InfraCommon,
  InfraNotify,
  InfraCommonIntf,
  InfraBindingIntf,
  InfraValueTypeIntf;

type
  {
    TBinding
    - Esta classe define as regras de como a informação será refletida. Pense
      nesta classe como a ponte entre a origem do dado e o seu apresentador.
      ligação entre dois objetos (bindable)
    - Ela é responsável tambem por receber a notificação de mudança de um dos
      lados afim de atualizar o outro.
    - Guarga o tipo de conversor a ser usado para transferência da informação
      entre os objetos (bindable).
    - Quando seta-se Active := True o objeto bindable Right (apresentador) é
      atualizado com o valor do objeto bindable Left (origem da informação).
    - Left é o objeto que nós queremos exibir;
    - Right é o objeto que irá apresentar e/ou modificar o dado de alguma
      maneira;
  }
  TBinding = class(TElement, IBinding)
  private
    FActive: Boolean;
    FLeft, FRight: IBindable;
    FConverter: ITypeConverter;
    FConverterParameter: IInfraType;
    FMode: TBindingMode;
    procedure UpdateRight;
    procedure PropertyChanged(const Event: IInfraEvent);
    function PropertyChangedFilter(const Event: IInfraEvent): Boolean;
  protected
    function GetLeft: IBindable;
    function GetMode: TBindingMode;
    function GetRight: IBindable;
    function GetConverter: ITypeConverter;
    function GetConverterParameter: IInfraType;
    procedure SetMode(Value: TBindingMode);
    procedure SetConverter(const Value: ITypeConverter);
    procedure SetConverterParameter(const Value: IInfraType);
    procedure UpdateLeft;
    function TwoWay: IBinding;
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
  public
    constructor Create(const Left, Right: IBindable); reintroduce;
  end;

  {
    TBindManager
    - Esta classe é um container de objetos binding.
    - Quando seta-se Active para true todos os objetos Binding são ativados.
    - O relacionamento (binding) pode acontecer entre:
      ControleVCL1.AlgumaPropriedade <-> ControleVCL2.AlgumaPropriedade
      InfraObject1.AlgumaAtributo <-> ControleVCL.AlgumaPropriedade
    - DataContext é utilizado para definir o InfraObject (Left) que contem os
      dados a serem apresentados. Quando ligando InfraObject <-> ControleVCL.
  }
  TBindManager = class(TElement, IBindManager)
  private
    FActive: Boolean;
    FBindingList: IBindingList;
    FDataContext: IInfraType;
  protected
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetDataContext: IInfraType;
    procedure SetDataContext(const Value: IInfraType);
    function Add(const pLeft, pRight: IBindable;
      const pConverter: ITypeConverter = nil): IBinding; overload;
    function Add(
      pLeftControl: TControl; const pLeftProperty: string;
      pRightControl: TControl; const pRightProperty: string;
      const pConverter: ITypeConverter = nil): IBinding; overload;
    function Add(
      const pLeftProperty: string;
      pRightControl: TControl; const pRightProperty: string = '';
      const pConverter: ITypeConverter = nil): IBinding; overload;
    procedure ClearBindings;
    property DataContext: IInfraType read GetDataContext write SetDataContext;
    property Active: boolean read GetActive write SetActive;
  public
    constructor Create; override;
  end;

  { Evento de notificação de mudança de valor no Bindable }
  TNotifyValueChanged = class(TInfraEvent, INotifyValueChanged);

implementation

uses
  List_Binding,
  InfraBindingControl,
  InfraBindingType,
  InfraBindingConsts,
  SysUtils;

{ TBinding }

constructor TBinding.Create(const Left, Right: IBindable);
begin
  if not Assigned(Left) then
    Raise EInfraBindingError.Create(cErrorLeftBindableNotDefined);
  if not Assigned(Right) then
    Raise EInfraBindingError.Create(cErrorRightBindableNotDefined);
  FLeft := Left;
  FRight := Right;
  SetMode(bmLeftToRight);
  EventService.Subscribe(INotifyValueChanged, Self as ISubscriber,
    PropertyChanged, EmptyStr, PropertyChangedFilter);
end;

procedure TBinding.PropertyChanged(const Event: IInfraEvent);
var
  vBindable: IBindable;
begin
  vBindable := (Event.Source as IBindable);
  if vBindable = FLeft then
    UpdateRight
  else
    UpdateLeft;
end;

function TBinding.PropertyChangedFilter(const Event: IInfraEvent): Boolean;
var
  vSource: IBindable;
begin
  vSource := Event.Source As IBindable;
  Result := (vSource = FLeft) or (vSource = FRight);
end;

function TBinding.GetLeft: IBindable;
begin
  Result := FLeft;
end;

function TBinding.GetMode: TBindingMode;
begin
  Result := FMode;
end;

function TBinding.GetRight: IBindable;
begin
  Result := FRight;
end;

function TBinding.GetConverter: ITypeConverter;
begin
  Result := FConverter;
end;

procedure TBinding.SetMode(Value: TBindingMode);
begin
  if (Value = bmTwoWay)
    and not FRight.Support2Way then
    Raise EInfraBindingError.Create(cErrorBindable2WayNotSupported);
  FMode := Value;
end;

procedure TBinding.SetConverter(const Value: ITypeConverter);
begin
  FConverter := Value;
end;

function TBinding.TwoWay: IBinding;
begin
  SetMode(bmTwoWay);
  Result := Self;
end;

procedure TBinding.UpdateLeft;
var
  vRightValue: IInfraType;
begin
  vRightValue := FRight.Value;
  if FMode = bmTwoWay then
  begin
    if Assigned(FConverter) then
      vRightValue := FConverter.ConvertToLeft(vRightValue, FConverterParameter);
    FLeft.Value := vRightValue;
  end;
end;

procedure TBinding.UpdateRight;
var
  vLeftValue: IInfraType;
begin
  vLeftValue := FLeft.Value;
  if Assigned(FConverter) then
    vLeftValue := FConverter.ConvertToRight(vLeftValue, FConverterParameter);
  FRight.Value := vLeftValue;
end;

function TBinding.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TBinding.SetActive(Value: Boolean);
begin
  UpdateRight;
end;

function TBinding.GetConverterParameter: IInfraType;
begin
  Result := FConverterParameter;
end;

procedure TBinding.SetConverterParameter(const Value: IInfraType);
begin
  FConverterParameter := Value;
end;

{ TBindManager }

constructor TBindManager.Create;
begin
  inherited Create;
  FBindingList := TBindingList.Create;
end;

function TBindManager.Add(
  pLeftControl: TControl; const pLeftProperty: string;
  pRightControl: TControl; const pRightProperty: string;
  const pConverter: ITypeConverter = nil): IBinding;
var
  vLeft, vRight: IBindable;
begin
  vLeft := GetBindableVCL(pLeftControl, pLeftProperty);
  vRight := GetBindableVCL(pRightControl, pRightProperty);
  Result := Add(vLeft, vRight, pConverter);
end;

function TBindManager.Add(const pLeftProperty: string;
  pRightControl: TControl; const pRightProperty: string = '';
  const pConverter: ITypeConverter = nil): IBinding;
var
  vLeft, vRight: IBindable;
begin
  vLeft := TBindableInfraType.GetBindable(FDataContext, pLeftProperty);
  vRight := GetBindableVCL(pRightControl, pRightProperty);
  Result := Add(vLeft, vRight, pConverter);
end;

function TBindManager.Add(const pLeft, pRight: IBindable;
  const pConverter: ITypeConverter = nil): IBinding;
begin
  Result := TBinding.Create(pLeft, pRight);
  Result.Converter := pConverter;
  FBindingList.Add(Result);
end;

procedure TBindManager.ClearBindings;
begin
  FBindingList.Clear;
end;

function TBindManager.GetDataContext: IInfraType;
begin
  Result := FDataContext;
end;

procedure TBindManager.SetDataContext(const Value: IInfraType);
begin
  FDataContext := Value;
end;

function TBindManager.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TBindManager.SetActive(Value: Boolean);
var
  vIterator: IInfraIterator;
begin
  vIterator := nil;
  if FActive <> Value then
    FActive := Value;
  if FActive then
  begin
    vIterator := FBindingList.NewIterator;
    while not vIterator.IsDone do
    begin
      (vIterator.CurrentItem as IBinding).Active := True;
      vIterator.Next;
    end;
  end;
end;

end.
