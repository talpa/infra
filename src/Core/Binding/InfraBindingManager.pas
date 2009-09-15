unit InfraBindingManager;

interface

uses
  Controls,
  InfraCommon,
  InfraNotify,
  InfraCommonIntf,
  InfraBindingIntf,
  InfraValueType,
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
    FUpdatingRight: Boolean;
    FName: string;
    FLeft, FRight: IBindable;
    FConverter: ITypeConverter;
    FConverterParameter: IInfraType;
    FMode: TBindingMode;
    procedure UpdateRight;
    procedure PropertyChanged(const Event: IInfraEvent);
    function PropertyChangedFilter(const Event: IInfraEvent): Boolean;
  protected
    function GetName: string;
    function GetLeft: IBindable;
    function GetMode: TBindingMode;
    function GetRight: IBindable;
    function GetConverter: ITypeConverter;
    function GetConverterParameter: IInfraType;
    procedure SetName(const Value: string);
    procedure SetMode(Value: TBindingMode);
    procedure SetConverter(const Value: ITypeConverter);
    procedure SetConverterParameter(const Value: IInfraType);
    procedure SetLeft(const Value: IBindable);
    procedure SetRight(const Value: IBindable);
    procedure UpdateLeft;
    function TwoWay: IBinding;
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    property Name: string read GetName write SetName;
    property Left: IBindable read GetLeft write SetLeft;
    property Right: IBindable read GetRight write SetRight;
  public
    constructor Create; override;
  end;

  // Model para objetos bindables que possui listas
  TBindableListModel = class(TInfraType, IBindableListModel)
  private
    FCurrent: IInfraType;
    FItemIndex: Integer;
    FItemOperated: IInfraType;
    FList: IInfraType;
    FOperation: TListModelOperation;
    FExpression: string;
  protected
    function GetCurrent: IInfraType;
    function GetExpression: string;
    function GetItemIndex: integer;
    function GetItemOperated: IInfraType;
    function GetList: IInfraType;
    function GetOperation: TListModelOperation;
    function GetValueOfExpression(const pObject: IInfraType): string;
    procedure Assign(const Source: IInfraType); override;
    procedure Clear; override;
    procedure SetCurrent(const Value: IInfraType);
    procedure SetExpression(const Value: string);
    procedure SetItemIndex(Value: integer);
    procedure SetItemOperated(const Value: IInfraType);
    procedure SetList(const Value: IInfraType);
    procedure SetOperation(Value: TListModelOperation);
    property Current: IInfraType read GetCurrent write SetCurrent;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property ItemOperated: IInfraType read GetItemOperated write SetItemOperated;
    property List: IInfraType read GetList write SetList;
    property Operation: TListModelOperation read GetOperation write SetOperation;
    property Expression: string read GetExpression write SetExpression;
  public
    procedure InfraInitInstance; override;
  end;

  TBindingList = class(TBinding, IBindingList)
  private
    FDataContext: IInfraType;
    FListModel: IBindableListModel;
    function GetFirstPropertyName(var pExpression: string): string;
    function GetListModel: IBindableListModel;
  protected
    procedure AddSelection(const pLeftProperty, pRightProperty: string);
    constructor Create(const pDataContext: IInfraType;
      const pExpression: string); reintroduce;
    property ListModel: IBindableListModel read GetListModel;
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
    FBindingList: IBindings;
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
    function AddList(const pLeftExpression: string;
      pRightControl: TControl; const pRightProperty: string;
      const pConverter: ITypeConverter = nil): IBindingList;
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

constructor TBinding.Create;
begin
  inherited Create;
  FUpdatingRight := False;
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
  vSource := Event.Source as IBindable;
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
    raise EInfraBindingError.Create(cErrorBindable2WayNotSupported);
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
  if FRight.Updating then
    Exit;
  vRightValue := FRight.Value;
  if (FMode = bmTwoWay) then
  begin
    if Assigned(FConverter) then
      vRightValue := FConverter.ConvertToLeft(vRightValue, FConverterParameter);
    if not Supports(FLeft.Value, vRightValue.TypeInfo.TypeID) then
      raise EInfraBindingError.CreateFmt(
        cErrorBindableValuesIncompatibles, [Name]);
    FLeft.BeginUpdate;
    try
      FLeft.Value := vRightValue;
    finally
      FLeft.EndUpdate;
    end;
  end;
end;

procedure TBinding.UpdateRight;
var
  vLeftValue: IInfraType;
begin
  if FLeft.Updating then
    Exit;
  vLeftValue := FLeft.Value;
  if Assigned(FConverter) then
    vLeftValue := FConverter.ConvertToRight(vLeftValue, FConverterParameter);
// *** deu problema por que TBindableCustomListItems GetValue retorna um TInfraStrings por
// *** podemos estar ligando um listbox a outro, e nao posso herdar o getvalue por que
// *** nao sei se está usando ou nao o bindablelistmodel
//  if not Supports(FRight.Value, vLeftValue.TypeInfo.TypeID) then
//    raise EInfraBindingError.CreateFmt(
//      cErrorBindableValuesIncompatibles, [Name]);
  FRight.BeginUpdate;
  try
    FRight.Value := vLeftValue
  finally
    FRight.EndUpdate;
  end;
end;

function TBinding.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TBinding.SetActive(Value: Boolean);
begin
  if Value then
  begin
    if not Assigned(Left) then
      raise EInfraBindingError.CreateFmt(cErrorLeftBindableNotDefined, [Name]);
    if not Assigned(Right) then
      raise EInfraBindingError.CreateFmt(cErrorRightBindableNotDefined, [Name]);
    UpdateRight;
  end;
end;

function TBinding.GetConverterParameter: IInfraType;
begin
  Result := FConverterParameter;
end;

procedure TBinding.SetConverterParameter(const Value: IInfraType);
begin
  FConverterParameter := Value;
end;

function TBinding.GetName: string;
begin
  Result := FName;
end;

procedure TBinding.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TBinding.SetLeft(const Value: IBindable);
begin
  FLeft := Value;
end;

procedure TBinding.SetRight(const Value: IBindable);
begin
  FRight := Value;
end;

{ TBindManager }

constructor TBindManager.Create;
begin
  inherited Create;
  FBindingList := TListBinding.Create;
end;

function TBindManager.Add(const pLeft, pRight: IBindable;
  const pConverter: ITypeConverter = nil): IBinding;
begin
  Result := TBinding.Create;
  with Result do
  begin
    Left := pLeft;
    Right := pRight;
    Converter := pConverter;
  end;
  FBindingList.Add(Result);
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
  Result.Name := Format('%s.%s -> %s.%s', [
    pLeftControl.Name, pLeftProperty, pRightControl.Name, pRightProperty])
end;

function TBindManager.Add(const pLeftProperty: string;
  pRightControl: TControl; const pRightProperty: string = '';
  const pConverter: ITypeConverter = nil): IBinding;
var
  vLeft, vRight: IBindable;
  vLeftProperty: IProperty;
begin
  vLeftProperty := FDataContext as IProperty;
  vLeft := GetBindableType(vLeftProperty, pLeftProperty);
  vRight := GetBindableVCL(pRightControl, pRightProperty);
  Result := Add(vLeft, vRight, pConverter);
  Result.Name := Format('Datacontext.%s - %s.%s', [
    pLeftProperty, pRightControl.Name, pRightProperty])
end;

function TBindManager.AddList(const pLeftExpression: string;
  pRightControl: TControl; const pRightProperty: string;
  const pConverter: ITypeConverter = nil): IBindingList;
begin
  Result := TBindingList.Create(FDataContext, pLeftExpression);
  with Result do
  begin
    Left := GetBindableType(Result.ListModel);
    Right := GetBindableVCL(pRightControl, pRightProperty);
    Converter := pConverter;
    Name := Format('Datacontext.%s - %s.%s', [
      pLeftExpression, pRightControl.Name, pRightProperty]);
    (Left as IBindableList).ListModel := Result.ListModel;
    (Right as IBindableList).ListModel := Result.ListModel;
  end;
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

{ TBindableListModel }

procedure TBindableListModel.InfraInitInstance;
begin
  inherited;
  Clear;
end;

procedure TBindableListModel.Assign(const Source: IInfraType);
var
  vModel: IBindableListModel;
begin
  if Assigned(Source)
    and Supports(Source, IBindableListModel, vModel) then
  begin
    SetCurrent(vModel.Current);
    SetList(vModel.List);
    SetItemIndex(vModel.ItemIndex);
    SetOperation(vModel.Operation);
  end else
    inherited Assign(Source);
end;

procedure TBindableListModel.Clear;
begin
  inherited Clear;
  FCurrent := nil;
  FList := nil;
  FItemIndex := -1;
  FOperation := loNone;
  FExpression := EmptyStr;
end;

function TBindableListModel.GetCurrent: IInfraType;
begin
  Result := FCurrent;
end;

function TBindableListModel.GetItemIndex: integer;
begin
  Result := FItemIndex;
end;

function TBindableListModel.GetItemOperated: IInfraType;
begin
  Result := FItemOperated;
end;

function TBindableListModel.GetList: IInfraType;
begin
  Result := FList;
end;

function TBindableListModel.GetOperation: TListModelOperation;
begin
  Result := FOperation;
end;

procedure TBindableListModel.SetCurrent(const Value: IInfraType);
begin
  FCurrent := Value;
end;

procedure TBindableListModel.SetItemIndex(Value: integer);
begin
  FItemIndex := Value;
end;

procedure TBindableListModel.SetItemOperated(const Value: IInfraType);
begin
  FItemOperated := Value;
end;

procedure TBindableListModel.SetList(const Value: IInfraType);
begin
  FList := Value;
end;

procedure TBindableListModel.SetOperation(Value: TListModelOperation);
begin
  FOperation := Value;
end;

function TBindableListModel.GetExpression: string;
begin
  Result := FExpression;
end;

procedure TBindableListModel.SetExpression(const Value: string);
begin
  FExpression := Value;
end;

function TBindableListModel.GetValueOfExpression(const pObject: IInfraType): string;
var
  vObject: IInfraObject;
  vValue: IInfraString;
  vProperty: IProperty;
begin
  // *** Precisamos usar um converter dependendo do tipo de pObject
  if Supports(pObject, IInfraObject, vObject) then
  begin
    vProperty := vObject.GetProperty(FExpression);
    if Supports(vProperty, IInfraString, vValue) then
      Result := vValue.AsString;
  end else if Supports(pObject, IInfraString, vValue) then
    Result := vValue.AsString;
end;

{ TBindingList }

constructor TBindingList.Create(const pDataContext: IInfraType;
  const pExpression: string);
var
  vRestOfExpression: string;
  vObject: IInfraObject;
  vProperty: IProperty;
begin
  inherited Create;
  FDataContext := pDataContext;
  if not Supports(FDataContext, IInfraObject, vObject) then
    raise EInfraBindingError.Create(cErrorDataContextNotIsInfraObject);
  vRestOfExpression := pExpression;
  vProperty := vObject.GetProperty(GetFirstPropertyName(vRestOfExpression));
  if not Assigned(vProperty) then
    raise EInfraBindingError.Create(cErrorBindingExpressionNotsupported);
  FListModel := TBindableListModel.Create;
  with FListModel do
  begin
    Operation := loRefresh;
    List := vProperty as IInfraType;
    Expression := vRestOfExpression;
  end;
end;

procedure TBindingList.AddSelection(const pLeftProperty, pRightProperty: string);
begin
  // *** Acho que teria de ter um binding aqui.
end;

function TBindingList.GetFirstPropertyName(var pExpression: string): string;
var
  vCommaPosition: Integer;
begin
  Result := pExpression;
  vCommaPosition := Pos('.', pExpression);
  if vCommaPosition = 0 then
  begin
    pExpression := '';
    Exit;
  end;
  Result := Copy(pExpression, 0, vCommaPosition-1);
  pExpression := Copy(pExpression, vCommaPosition+1, Length(pExpression));
end;

function TBindingList.GetListModel: IBindableListModel;
begin
  Result := FListModel;
end;

end.

