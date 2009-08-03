unit InfraBinding;

interface

uses
  InfraBindingIntf,
  InfraValueTypeIntf,
  InfraValueType,
  InfraCommon;

type
  // Serviço de Binding
  TInfraBindingService = class(TBaseElement, IInfraBindingService)
  protected
    function GetNewBindManager: IBindManager;
  end;

  // Classe base para objetos que representam um dos lados de um binding
  TBindable = class(TElement, IBindable)
  private
    FUpdateCount: integer;
  protected
    function GetUpdating: boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Changed;
    function Support2Way: Boolean; virtual;
    function GetValue: IInfraType; virtual; abstract;
    procedure SetValue(const Value: IInfraType); virtual; abstract;
    property Updating: boolean read GetUpdating;
  end;

  // Model para objetos bindables que possui listas
  TBindableListModel = class(TInfraType, IBindableListModel)
  private
    FCurrent: IInfraType;
    FItemIndex: Integer;
    FList: IInfraType;
    FOperation: TListModelOperation;
    FExpression: string;
  protected
    function GetCurrent: IInfraType;
    function GetExpression: string;
    function GetItemIndex: integer;
    function GetList: IInfraType;
    function GetOperation: TListModelOperation;
    procedure Assign(const Source: IInfraType); override;
    procedure Clear; override;
    procedure SetCurrent(const Value: IInfraType);
    procedure SetExpression(const Value: string);
    procedure SetItemIndex(Value: integer);
    procedure SetList(const Value: IInfraType);
    procedure SetOperation(Value: TListModelOperation);
    property Current: IInfraType read GetCurrent write SetCurrent;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property List: IInfraType read GetList write SetList;
    property Operation: TListModelOperation read GetOperation write SetOperation;
    property Expression: string read GetExpression write SetExpression;
  public
    procedure InfraInitInstance; override;
  end;

implementation

uses
  Forms,
  SysUtils,
  InfraCommonIntf,
  InfraBindingManager;

{ TInfraBindingService }

{**
  Cria um novo objeto BindManager
  Chame GetNewBindManager para obter um novo objeto BindManager, com o qual
  poderá fazer a ligação entre controles de tela, ou entre controles de tela
  com infratypes.

  @return Retorna um objeto que implementa IBindManager
*}
function TInfraBindingService.GetNewBindManager: IBindManager;
begin
  Result := TBindManager.Create;
end;

{ TBindable }

procedure TBindable.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TBindable.Changed;
begin
  if not Application.Terminated then
    Publisher.Publish(TNotifyValueChanged.Create(Self) as INotifyValueChanged);
end;

procedure TBindable.EndUpdate;
begin
  Dec(FUpdateCount);
end;

function TBindable.GetUpdating: boolean;
begin
  Result := FUpdateCount > 0;
end;

function TBindable.Support2Way: Boolean;
begin
  Result := False;
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

// Não entendi, mas se pôr direto no Initialization acontece Access Violations.
// ATENÇÃO: Vc não deve atribuir BindingService para uma variável de
// instancia nem global sem que no final da aplicação atribuia nil a ela explicitamente,
// sob pena de acontecer um AV no final da aplicação
procedure InjectBindingService;
begin
  (ApplicationContext as IBaseElement).Inject(
    IInfraBindingService, TInfraBindingService.Create);
end;

initialization
  InjectBindingService;

end.

