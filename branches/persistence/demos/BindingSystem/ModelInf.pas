unit ModelInf;

interface

uses InfraCommonIntf,
  InfraValueTypeIntf;

type

  IFornecedor = interface(Iinfraobject)
    ['{C2921579-47B5-4F47-9E17-3A0A652A6622}']
    function getEndereco: IInfraString;
    function getNome: IInfrastring;
    function getNumero: Iinfrainteger;
    procedure setEndereco(value: IInfraString);
    procedure setNome(value: IInfrastring);
    procedure setNumero(value: Iinfrainteger);
    property Endereco: IInfraString read getEndereco write setEndereco;
    property Nome: IInfrastring read getNome write setNome;
    property Numero: Iinfrainteger read getNumero write setNumero;
  end;

  IProduto = interface(Iinfraobject)
    ['{CA13D6BB-B22F-478E-A683-553D9CE84777}']
    function getNome: IInfraString;
    function getUnidade: IInfraString;
    procedure setNome(value: IInfraString);
    procedure setUnidade(value: IInfraString);
    property Nome: IInfraString read getNome write setNome;
    property Unidade: IInfraString read getUnidade write setUnidade;
  end;

  IItems = interface(IInfraList)
    ['{5B38E7EE-D641-4523-A2CA-B8D5BF58DD97}']
  end;

  INotaFiscal = interface(Iinfraobject)
    ['{468EAA30-5120-483B-B093-EB07446097C5}']
    function getDocumento: IInfraString;
    function getFornecedor: IFornecedor;
    function getItems: IItems;
    function getTipoNota: IInfraInteger;
    procedure setDocumento(value: IInfraString);
    procedure setFornecedor(value: IFornecedor);
    procedure setItems(value: IItems);
    procedure setTipoNota(value: IInfraInteger);
    property Documento: IInfraString read getDocumento write setDocumento;
    property Fornecedor: IFornecedor read getFornecedor write setFornecedor;
    property Items: IItems read getItems write setItems;
    property TipoNota: IInfraInteger read getTipoNota write setTipoNota;
  end;

  IItem = interface(Iinfraobject)
    ['{0096B990-DA6F-405A-8EDE-C4332E26D013}']
    function getProduto: IProduto;
    function getQuantidade: IInfraInteger;
    function getValor: IInfraDouble;
    procedure setProduto(value: IProduto);
    procedure setQuantidade(value: IInfraInteger);
    procedure setValor(value: IInfraDouble);
    property Produto: IProduto read getProduto write setProduto;
    property Quantidade: IInfraInteger read getQuantidade write setQuantidade;
    property Valor: IInfraDouble read getValor write setValor;
  end;

implementation

end.

