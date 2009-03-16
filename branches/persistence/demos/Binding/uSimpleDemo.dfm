object Form1: TForm1
  Left = 405
  Top = 169
  Width = 551
  Height = 308
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 136
    Top = 115
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Edit1: TEdit
    Left = 8
    Top = 112
    Width = 121
    Height = 19
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    Text = 'Edit1'
  end
  object CheckBox1: TCheckBox
    Left = 136
    Top = 136
    Width = 81
    Height = 17
    Caption = 'CheckBox1'
    Checked = True
    Ctl3D = False
    ParentCtl3D = False
    State = cbChecked
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 8
    Top = 163
    Width = 529
    Height = 105
    Caption = 'Panel1'
    TabOrder = 2
    object Memo1: TMemo
      Left = 208
      Top = 8
      Width = 312
      Height = 89
      Ctl3D = False
      Lines.Strings = (
        'Memo1')
      ParentCtl3D = False
      TabOrder = 0
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 193
      Height = 25
      Caption = 'Mudar Edit1.Text por programa'#231#227'o '
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 40
      Width = 193
      Height = 25
      Caption = 'Desmarca CheckBox'
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 8
      Top = 72
      Width = 193
      Height = 25
      Caption = 'Button3'
      TabOrder = 3
    end
  end
  object Memo2: TMemo
    Left = 8
    Top = 8
    Width = 529
    Height = 97
    Ctl3D = False
    Lines.Strings = (
      'Exemplo simples de binding entre controles:'
      '1) Edit1.Text -> Label1.Caption'
      '2) Edit2.Text -> Edit2.Color'
      
        '3) Checkbox1.Checked -> Checkbox1.Caption. Usando TBooleanToText' +
        ' sem format)'
      
        '4) Checkbox2.Checked -> Checkbox2.Caption. Usando TBooleanToText' +
        ' com Format [Invisivel;Visivel]'
      '5) Checkbox1.Checked -> Panel1.Visible')
    ParentCtl3D = False
    TabOrder = 3
  end
  object Edit2: TEdit
    Left = 8
    Top = 136
    Width = 121
    Height = 19
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 4
    Text = 'clSkyBlue'
  end
  object CheckBox2: TCheckBox
    Left = 232
    Top = 136
    Width = 81
    Height = 17
    Caption = 'CheckBox2'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 5
  end
end
