object Form1: TForm1
  Left = 185
  Top = 168
  Width = 731
  Height = 372
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
  object PageControl1: TPageControl
    Left = 5
    Top = 4
    Width = 713
    Height = 329
    ActivePage = TabSheet2
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Person - Standard Controls'
      DesignSize = (
        705
        301)
      object Label25: TLabel
        Left = 2
        Top = 3
        Width = 701
        Height = 11
        AutoSize = False
        Color = 9276813
        ParentColor = False
      end
      object Label26: TLabel
        Left = 2
        Top = 14
        Width = 701
        Height = 12
        AutoSize = False
        Color = 5197647
        ParentColor = False
      end
      object Label27: TLabel
        Left = 2
        Top = 7
        Width = 701
        Height = 12
        Alignment = taCenter
        AutoSize = False
        Caption = 'Edits, Labels, CheckBox'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label28: TLabel
        Left = 2
        Top = 24
        Width = 701
        Height = 2
        AutoSize = False
        Color = 3947580
        ParentColor = False
      end
      object Label2: TLabel
        Left = 3
        Top = 194
        Width = 64
        Height = 13
        Caption = 'Person.Name'
      end
      object Label1: TLabel
        Left = 3
        Top = 224
        Width = 72
        Height = 13
        Caption = 'Person.Country'
      end
      object Label3: TLabel
        Left = 211
        Top = 194
        Width = 32
        Height = 13
        Caption = 'Label3'
      end
      object Label4: TLabel
        Left = 211
        Top = 224
        Width = 32
        Height = 13
        Caption = 'Label4'
      end
      object Label5: TLabel
        Left = 107
        Top = 255
        Width = 32
        Height = 13
        Caption = 'Label5'
      end
      object Memo1: TMemo
        Left = 2
        Top = 32
        Width = 701
        Height = 153
        Anchors = [akLeft, akTop, akRight]
        Color = clInfoBk
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        Lines.Strings = (
          'Simple example how to bind InfraType to Control'
          'DataContext =  Person'
          '1) Person.Name <-> Edit3.Text'
          '    Quando muda o texto e sai do Edit o Label1 se atualiza'
          '2) Person.Name <-> Label3.Caption'
          '3) Person.Country <-> Edit1.Text'
          '4) Person.Country <-> Label4.Caption '
          '5) Person.Active <-> CheckBox1.Checked '
          '6) Person.Active <-> Label5.Caption [TBooleanToTextConverter]'
          'Obs:'
          
            '    O botao mostra que a atualiza'#231'ao funciona por programa'#231'ao ta' +
            'mbem')
        ParentCtl3D = False
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
      object Edit3: TEdit
        Left = 83
        Top = 191
        Width = 121
        Height = 19
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
        Text = 'Edit3'
      end
      object Edit1: TEdit
        Left = 83
        Top = 221
        Width = 121
        Height = 19
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 2
        Text = 'Edit1'
      end
      object CheckBox1: TCheckBox
        Left = 3
        Top = 253
        Width = 97
        Height = 17
        Caption = 'Person.Active'
        TabOrder = 3
      end
      object Button1: TButton
        Left = 531
        Top = 188
        Width = 172
        Height = 25
        Caption = 'Set Person.Name by Code'
        TabOrder = 4
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 531
        Top = 218
        Width = 172
        Height = 25
        Caption = 'Set Person.Country by Code'
        TabOrder = 5
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 531
        Top = 249
        Width = 172
        Height = 25
        Caption = 'Togle Person.Active by Code'
        TabOrder = 6
        OnClick = Button3Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Company e Employees'
      ImageIndex = 1
      DesignSize = (
        705
        301)
      object Label6: TLabel
        Left = 2
        Top = 3
        Width = 701
        Height = 11
        AutoSize = False
        Color = 9276813
        ParentColor = False
      end
      object Label7: TLabel
        Left = 2
        Top = 14
        Width = 701
        Height = 12
        AutoSize = False
        Color = 5197647
        ParentColor = False
      end
      object Label8: TLabel
        Left = 2
        Top = 7
        Width = 701
        Height = 12
        Alignment = taCenter
        AutoSize = False
        Caption = 'ListBox'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label9: TLabel
        Left = 2
        Top = 24
        Width = 701
        Height = 2
        AutoSize = False
        Color = 3947580
        ParentColor = False
      end
      object Label10: TLabel
        Left = 85
        Top = 114
        Width = 121
        Height = 13
        Caption = 'Label10'
      end
      object Label11: TLabel
        Left = 3
        Top = 114
        Width = 77
        Height = 13
        Caption = 'company.Name:'
      end
      object Memo3: TMemo
        Left = 2
        Top = 32
        Width = 701
        Height = 78
        Anchors = [akLeft, akTop, akRight]
        Color = clInfoBk
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        Lines.Strings = (
          'Simple example how to bind InfraType to Control'
          'DataContext =  Company'
          '1) Company.Name <-> Label10.Caption'
          
            '2) Company.Employees <-> ListBox1.Items [TInfraListToText with P' +
            'arameter='#39'Name'#39']')
        ParentCtl3D = False
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
      object ListBox1: TListBox
        Left = 2
        Top = 131
        Width = 201
        Height = 167
        Ctl3D = False
        ItemHeight = 13
        ParentCtl3D = False
        TabOrder = 1
      end
      object Button7: TButton
        Left = 208
        Top = 128
        Width = 145
        Height = 25
        Caption = 'Add Employees'
        TabOrder = 2
        OnClick = Button4Click
      end
      object Button8: TButton
        Left = 208
        Top = 187
        Width = 145
        Height = 25
        Caption = 'Clear'
        TabOrder = 3
        OnClick = Button5Click
      end
      object Button9: TButton
        Left = 208
        Top = 157
        Width = 145
        Height = 25
        Caption = 'Delete selected'
        TabOrder = 4
        OnClick = Button6Click
      end
    end
  end
end
