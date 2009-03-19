object Form1: TForm1
  Left = 376
  Top = 163
  Width = 554
  Height = 320
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
  object Label2: TLabel
    Left = 8
    Top = 131
    Width = 64
    Height = 13
    Caption = 'Person.Name'
  end
  object Label1: TLabel
    Left = 8
    Top = 155
    Width = 72
    Height = 13
    Caption = 'Person.Country'
  end
  object Label3: TLabel
    Left = 216
    Top = 131
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 216
    Top = 155
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label5: TLabel
    Left = 440
    Top = 152
    Width = 32
    Height = 13
    Caption = 'Label5'
  end
  object Label6: TLabel
    Left = 368
    Top = 129
    Width = 66
    Height = 13
    Caption = 'Person.Active'
  end
  object Panel1: TPanel
    Left = 8
    Top = 179
    Width = 529
    Height = 105
    Caption = 'Panel1'
    TabOrder = 0
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
      Caption = 'Set Person.Name by Code'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 40
      Width = 193
      Height = 25
      Caption = 'Set Person.Country by Code'
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 8
      Top = 72
      Width = 193
      Height = 25
      Caption = 'Set Person.Active by Code'
      TabOrder = 3
      OnClick = Button3Click
    end
  end
  object Memo2: TMemo
    Left = 8
    Top = 8
    Width = 529
    Height = 113
    Ctl3D = False
    Lines.Strings = (
      'Simple example how to bind InfraType to Control'
      '1) Edit3.Text <-> Person.Name'
      '2) Edit1.Text <-> Person.Country'
      '3) Label3.Caption <-> Person.Name'
      '4) Label4.Caption <-> Person.Country'
      '5) CheckBox1.Checked <-> Person.Active'
      
        '6) CheckBox1.Caption <-> Person.Active converted by TBooleanToTe' +
        'xt'
      '7) Label5.Caption <-> Person.Active converted by TBooleanToText')
    ParentCtl3D = False
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 88
    Top = 128
    Width = 121
    Height = 19
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 2
    Text = 'Edit3'
  end
  object Edit1: TEdit
    Left = 88
    Top = 152
    Width = 121
    Height = 19
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 3
    Text = 'Edit1'
  end
  object CheckBox1: TCheckBox
    Left = 440
    Top = 128
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 4
  end
end
