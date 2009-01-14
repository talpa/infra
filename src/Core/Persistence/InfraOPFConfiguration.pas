unit InfraOPFConfiguration;

interface

uses
  SysUtils,
  Classes,
  InfraCommon,
  InfraOPFIntf,
  InfraCommonIntf,
  XmlIntf;

type
  /// Classe para armazenar as configurações do Framework
  TConfiguration = class(TBaseElement, IConfiguration, IXmlSerializable)
  private
    /// Aqui são armazenadas as configurações no formato <nome>=<valor>
    FProperties: TStrings;
    function GetProperties: TStrings;
  protected
    property Properties: TStrings read GetProperties;
    {IConfiguration members}
    function GetAsInteger(const pName: string): Integer; overload;
    function GetAsDouble(const pName: string): Double; overload;
    function GetAsString(const pName: string): string; overload;
    function GetValue(const pName: string; const pDefaultValue: Integer): Integer; overload;
    function GetValue(const pName: string; const pDefaultValue: Double): Double; overload;
    function GetValue(const pName: string; const pDefaultValue: string): string; overload;
    procedure SetValue(const pName: string; const Value: Integer); overload;
    procedure SetValue(const pName: string; const Value: Double); overload;
    procedure SetValue(const pName: string; const Value: string); overload;
    procedure Clear;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(const Stream: TStream);
    procedure SaveToStream(const Stream: TStream);
    function BuildSessionFactory: ISessionFactory;
    function Clone: IConfiguration;
    {IXmlSerializable members}
    procedure WriteXml(pXmlDoc: IXmlDocument);
    procedure ReadXml(pXmlDoc: IXmlDocument);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  Variants,
  InfraOPFSessionFactory;

{ TConfiguration }

/// Cria uma nova instância de TConfiguration
constructor TConfiguration.Create;
begin
  inherited;
  FProperties := TStringList.Create;
end;

/// Destrói a instância de TConfiguration
destructor TConfiguration.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
end;

///  Limpa todas as propriedades
procedure TConfiguration.Clear;
begin
  FProperties.Clear;
end;

{**
  Obtem o valor de uma propriedade como Double
  @param pName Nome da propriedade da qual se quer obter o valor
  @returns O valor da propriedade como Double
}
function TConfiguration.GetAsDouble(const pName: string): Double;
begin
  Result := StrToFloat(FProperties.Values[pName]);
end;

{**
  Obtem o valor de uma propriedade como Integer
  @param pName Nome da propriedade da qual se quer obter o valor
  @returns O valor da propriedade como Integer
}
function TConfiguration.GetAsInteger(const pName: string): Integer;
begin
  Result := StrToInt(FProperties.Values[pName]);
end;

{**
  Obtem o valor de uma propriedade como string
  @param pName Nome da propriedade da qual se quer obter o valor
  @returns O valor da propriedade como string
}
function TConfiguration.GetAsString(const pName: string): string;
begin
  Result := FProperties.Values[pName];
end;

{**
  Obtem uma referencia ao objeto que contém as propriedades
  @returns Um objeto do tipo TStrings
}
function TConfiguration.GetProperties: TStrings;
begin
  Result := FProperties;
end;

{**
  Obtem o valor de uma propriedade como Integer e, se não existir, o valor default
  @param pName Nome da propriedade da qual se quer obter o valor
  @returns O valor da propriedade como Integer ou o valor default
}
function TConfiguration.GetValue(const pName: string;
  const pDefaultValue: Integer): Integer;
begin
  if FProperties.IndexOfName(pName) <> -1 then
    Result := StrToIntDef(FProperties.Values[pName], pDefaultValue)
  else
    Result := pDefaultValue;
end;

{**
  Obtem o valor de uma propriedade como Double e, se não existir, o valor default
  @param pName Nome da propriedade da qual se quer obter o valor
  @returns O valor da propriedade como Double ou o valor default
}
function TConfiguration.GetValue(const pName: string;
  const pDefaultValue: Double): Double;
begin
  if FProperties.IndexOfName(pName) <> -1 then
    Result := StrToFloatDef(FProperties.Values[pName], pDefaultValue)
  else
    Result := pDefaultValue;
end;

{**
  Obtem o valor de uma propriedade como string e, se não existir, o valor default
  @param pName Nome da propriedade da qual se quer obter o valor
  @returns O valor da propriedade como string ou o valor default
}
function TConfiguration.GetValue(const pName, pDefaultValue: string): string;
begin
  if FProperties.IndexOfName(pName) <> -1 then
    Result := FProperties.Values[pName]
  else
    Result := pDefaultValue;
end;

{**
  Atribui o valor de uma propriedade como Integer
  @param pName Nome da propriedade à qual se quer atribuir o valor
}
procedure TConfiguration.SetValue(const pName: string; const Value: Integer);
begin
  FProperties.Values[pName] := IntToStr(Value);
end;

{**
  Atribui o valor de uma propriedade como Double
  @param pName Nome da propriedade à qual se quer atribuir o valor
}
procedure TConfiguration.SetValue(const pName: string; const Value: Double);
begin
  FProperties.Values[pName] := FloatToStr(Value);
end;

{**
  Atribui o valor de uma propriedade como string
  @param pName Nome da propriedade à qual se quer atribuir o valor
}
procedure TConfiguration.SetValue(const pName, Value: string);
begin
  FProperties.Values[pName] := Value;
end;

{**
  Desserializa o objeto, lendo as configurações de um arquivo Xml

  @param pXmlDoc Objeto que implemente IXmlDocument que contenha as informações a serem lidas
}
procedure TConfiguration.ReadXml(pXmlDoc: IXmlDocument);
var
  i: Integer;
begin
  FProperties.Clear;
  for i := 0 to pXmlDoc.DocumentElement.ChildNodes.Count - 1 do
    FProperties.Add(pXmlDoc.DocumentElement.ChildNodes[i].NodeName + '=' + VarToStr(pXmlDoc.DocumentElement.ChildNodes[i].NodeValue));
end;

{**
  Serializa o objeto para um Xml

  @param pXmlDoc Objeto para o qual o objeto será serializado
}
procedure TConfiguration.WriteXml(pXmlDoc: IXmlDocument);
var
  i: Integer;
begin
  pXmlDoc.Xml.Text := '';
  pXmlDoc.Active := True;
  pXmlDoc.Version := '1.0';
  pXmlDoc.Encoding := 'utf-8';
  pXmlDoc.AddChild(ClassName);
  for i := 0 to FProperties.Count - 1 do
    pXmlDoc.DocumentElement.AddChild(FProperties.Names[i]).NodeValue := FProperties.ValueFromIndex[i];
  pXmlDoc.Xml.Text := StringReplace(pXmlDoc.Xml.Text, #13#10, '', [rfReplaceAll]);
end;

{**
  Lê as configurações de um arquivo texto (.conf, .ini, etc)

  @param FileName Nome do arquivo que contém as configurações a serem lidas
}
procedure TConfiguration.LoadFromFile(const FileName: string);
begin
  FProperties.LoadFromFile(FileName);
end;

{**
  Grava as configurações para um arquivo texto (.conf, .ini, etc)

  @param FileName Nome do arquivo que será criado
}
procedure TConfiguration.SaveToFile(const FileName: string);
begin
  FProperties.SaveToFile(FileName);
end;

{**
  Constrói uma nova SessionFactory

  @return Retorna um SessionFactory
}
function TConfiguration.BuildSessionFactory: ISessionFactory;
begin
  Result := TSessionFactory.Create(Self);
end;

procedure TConfiguration.LoadFromStream(const Stream: TStream);
begin
  FProperties.LoadFromStream(Stream);
end;

procedure TConfiguration.SaveToStream(const Stream: TStream);
begin
  FProperties.SaveToStream(Stream);
end;

function TConfiguration.Clone: IConfiguration;
var
  vStm: TMemoryStream;
begin
  vStm := TMemoryStream.Create;
  try
    Self.SaveToStream(vStm);

    vStm.Seek(0, 0);
    
    Result := TConfiguration.Create;
    Result.LoadFromStream(vStm);
  finally
    vStm.Free;
  end;
end;

end.

