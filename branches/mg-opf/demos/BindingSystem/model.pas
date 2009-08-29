unit model;

interface

uses
  InfraCommon,
  InfraCommonIntf,
  InfraValueTypeIntf,
  InfraValueType,
  ModelInf;

type

  TNotaFiscal = class(TInfraObject, INotaFiscal)
  private
    FDocumento: IInfrastring;
    FFornecedor: IFornecedor;
    FItems: IItems;
    FTipoNota: IInfraInteger;
  protected
    function getDocumento: IInfraString;
    function getFornecedor: IFornecedor;
    function getItems: IItems;
    function getTipoNota: IInfraInteger;
    procedure setDocumento(value: IInfraString);
    procedure setFornecedor(value: IFornecedor);
    procedure setItems(value: IItems);
    procedure setTipoNota(value: IInfraInteger);
  public
    property Documento: IInfraString read getDocumento write setDocumento;
    property Fornecedor: IFornecedor read getFornecedor write setFornecedor;
    property Items: IItems read getItems write setItems;
    property TipoNota: IInfraInteger read getTipoNota write setTipoNota;
  end;

  TFornecedor = class(TinfraObject, IFornecedor)
  private
    FEndereco: IInfraString;
    FNome: IInfrastring;
    FNumero: IInfraInteger;
  protected
    function getEndereco: IInfraString;
    function getNome: IInfrastring;
    function getNumero: Iinfrainteger;
    procedure setEndereco(value: IInfraString);
    procedure setNome(value: IInfrastring);
    procedure setNumero(value: Iinfrainteger);
  public
    property Endereco: IInfraString read getEndereco write setEndereco;
    property Nome: IInfrastring read getNome write setNome;
    property Numero: Iinfrainteger read getNumero write setNumero;
  end;

  TItems = class(TInfraList, IItems)
  end;

  TItem = class(TInfraObject, IItem)
  private
    FProduto: IProduto;
    FQuantidade: IInfraInteger;
    FValor: IInfraDouble;
  protected
    function getProduto: IProduto;
    function getQuantidade: IInfraInteger;
    function getValor: IInfraDouble;
    procedure setProduto(value: IProduto);
    procedure setQuantidade(value: IInfraInteger);
    procedure setValor(value: IInfraDouble);
  public
    property Produto: IProduto read getProduto write setProduto;
    property Quantidade: IInfraInteger read getQuantidade write setQuantidade;
    property Valor: IInfraDouble read getValor write setValor;
  end;

  TProduto = class(TInfraObject, IProduto)
  private
    FNome: IInfraString;
    FUnidade: IInfraString;
  protected
    function getNome: IInfraString;
    function getUnidade: IInfraString;
    procedure setNome(value: IInfraString);
    procedure setUnidade(value: IInfraString);
  public
    property Nome: IInfraString read getNome write setNome;
    property Unidade: IInfraString read getUnidade write setUnidade;
  end;

implementation

{ TNotaFiscal }

function TNotaFiscal.getDocumento: IInfraString;
begin
  result := FDocumento
end;

function TNotaFiscal.getFornecedor: IFornecedor;
begin
  result := FFornecedor;
end;

function TNotaFiscal.getItems: IItems;
begin
  result := FItems;
end;

function TNotaFiscal.getTipoNota: IInfraInteger;
begin
  result := FTipoNota;
end;

procedure TNotaFiscal.setDocumento(value: IInfraString);
begin
  FDocumento := value;
end;

procedure TNotaFiscal.setFornecedor(value: IFornecedor);
begin
  FFornecedor := value;
end;

procedure TNotaFiscal.setItems(value: IItems);
begin
  FItems := value;
end;

procedure TNotaFiscal.setTipoNota(value: IInfraInteger);
begin
  FTipoNota := value
end;

{ TFornecedor }

function TFornecedor.getEndereco: IInfraString;
begin
  result := FEndereco;
end;

function TFornecedor.getNome: IInfrastring;
begin
  result := FNome;
end;

function TFornecedor.getNumero: Iinfrainteger;
begin
  result := FNumero;
end;

procedure TFornecedor.setEndereco(value: IInfraString);
begin
  FEndereco := value;
end;

procedure TFornecedor.setNome(value: IInfrastring);
begin
  FNome := value;
end;

procedure TFornecedor.setNumero(value: Iinfrainteger);
begin
  FNumero := value;
end;

{ TItem }

function TItem.getProduto: IProduto;
begin
  result := FProduto;
end;

function TItem.getQuantidade: IInfraInteger;
begin
  result := FQuantidade;
end;

function TItem.getValor: IInfraDouble;
begin
  Result := FValor;
end;

procedure TItem.setProduto(value: IProduto);
begin
  FProduto := value;
end;

procedure TItem.setQuantidade(value: IInfraInteger);
begin
  FQuantidade := value;
end;

procedure TItem.setValor(value: IInfraDouble);
begin
  FValor := value;
end;

{ TProduto }

function TProduto.getNome: IInfraString;
begin
  Result := FNome
end;

function TProduto.getUnidade: IInfraString;
begin
   result := FUnidade;
end;

procedure TProduto.setNome(value: IInfraString);
begin
  FNome := value;
end;

procedure TProduto.setUnidade(value: IInfraString);
begin
  FUnidade := value;
end;

end.

