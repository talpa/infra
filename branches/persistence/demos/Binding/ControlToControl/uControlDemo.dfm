object Form1: TForm1
  Left = 187
  Top = 123
  Width = 733
  Height = 401
  Caption = 'Binding entre Controles VCL'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  DesignSize = (
    725
    374)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 5
    Top = 6
    Width = 715
    Height = 356
    ActivePage = TabSheet3
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Binding Edits'
      DesignSize = (
        707
        328)
      object Label1: TLabel
        Left = 65
        Top = 164
        Width = 32
        Height = 13
        Caption = 'Label1'
      end
      object l1: TLabel
        Left = 4
        Top = 140
        Width = 48
        Height = 13
        Caption = 'Edit1 ----->'
      end
      object l2: TLabel
        Left = 4
        Top = 164
        Width = 47
        Height = 13
        Caption = 'Label1 -->'
      end
      object l3: TLabel
        Left = 4
        Top = 188
        Width = 45
        Height = 13
        Caption = 'Edit2 ---->'
      end
      object Label5: TLabel
        Left = 196
        Top = 188
        Width = 266
        Height = 13
        Caption = '<------- Mude o valor para clRed por exemplo e tecle TAB'
      end
      object Edit1: TEdit
        Left = 64
        Top = 137
        Width = 257
        Height = 19
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        Text = 'Edit1'
      end
      object Button1: TButton
        Left = 333
        Top = 133
        Width = 204
        Height = 25
        Caption = 'Mudar Edit1.Text por programa'#231#227'o '
        TabOrder = 1
        OnClick = Button1Click
      end
      object Edit2: TEdit
        Left = 64
        Top = 185
        Width = 121
        Height = 19
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 2
        Text = 'clSkyBlue'
      end
      object Memo2: TMemo
        Left = 2
        Top = 2
        Width = 702
        Height = 119
        Anchors = [akLeft, akTop, akRight]
        Color = clInfoBk
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        Lines.Strings = (
          '1) Edit1.Text -> Label1.Caption'
          '    Quando muda o texto e sai do Edit o Label1 se atualiza'
          ''
          '2) Edit2.Text -> Edit2.Color'
          
            '    Quando se define um texto no Edi2 correspondente a uma cor a' +
            ' propriedade cor '#233' modificada'
          ''
          'Obs:'
          
            '    O bot'#227'o mostra que a atualiza'#231#227'o funciona por programa'#231#227'o ta' +
            'mbem')
        ParentCtl3D = False
        ParentFont = False
        ReadOnly = True
        TabOrder = 3
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Bind CheckBox'
      ImageIndex = 1
      DesignSize = (
        707
        328)
      object Label2: TLabel
        Left = 4
        Top = 190
        Width = 79
        Height = 13
        Caption = 'CheckBox1 ----->'
      end
      object Label3: TLabel
        Left = 4
        Top = 244
        Width = 79
        Height = 13
        Caption = 'CheckBox2 ----->'
      end
      object Label4: TLabel
        Left = 4
        Top = 295
        Width = 79
        Height = 13
        Caption = 'CheckBox3 ----->'
      end
      object CheckBox1: TCheckBox
        Left = 96
        Top = 188
        Width = 81
        Height = 17
        Caption = 'CheckBox1'
        Checked = True
        Ctl3D = False
        ParentCtl3D = False
        State = cbChecked
        TabOrder = 0
      end
      object CheckBox2: TCheckBox
        Left = 96
        Top = 242
        Width = 81
        Height = 17
        Caption = 'CheckBox2'
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
      end
      object Button2: TButton
        Left = 184
        Top = 184
        Width = 185
        Height = 25
        Caption = 'Togle CheckBox1'
        TabOrder = 2
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 184
        Top = 238
        Width = 185
        Height = 25
        Caption = 'Togle CheckBox2'
        TabOrder = 3
        OnClick = Button3Click
      end
      object Memo3: TMemo
        Left = 2
        Top = 2
        Width = 702
        Height = 175
        Anchors = [akLeft, akTop, akRight]
        Color = clInfoBk
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        Lines.Strings = (
          '1) Checkbox1.Checked -> Checkbox1.Caption. '
          
            '    Quando marca ou desmarca o Checkbox1 o seu caption '#233' alterad' +
            'o'
          '    Estamos usando o converter padr'#227'o TBooleanToText'
          ''
          '2) Checkbox2.Checked -> Checkbox2.Caption. '
          
            '    An'#225'logo ao anterior mas agora usando um par'#225'metro para conve' +
            'rs'#227'o'
          '    TBooleanToText [Invisivel;Visivel]'
          ''
          '3) Checkbox3.Checked -> Panel1.Visible'
          '    O Checked do Checkbox3 determina a visibilidade do Panel1'
          ''
          'Obs:'
          
            '    Os bot'#245'es mostram que a atualiza'#231#227'o funciona por programa'#231#227'o' +
            ' tambem')
        ParentCtl3D = False
        ParentFont = False
        ReadOnly = True
        TabOrder = 4
      end
      object CheckBox3: TCheckBox
        Left = 96
        Top = 293
        Width = 81
        Height = 17
        Caption = 'CheckBox3'
        Checked = True
        Ctl3D = False
        ParentCtl3D = False
        State = cbChecked
        TabOrder = 5
      end
      object Panel1: TPanel
        Left = 376
        Top = 184
        Width = 327
        Height = 140
        Caption = 'A visibilidade deste panel depende do CheckBox3.Checked'
        TabOrder = 6
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Binding ListBox'
      ImageIndex = 2
      DesignSize = (
        707
        328)
      object SpeedButton1: TSpeedButton
        Left = 247
        Top = 120
        Width = 23
        Height = 22
        Caption = '+'
        OnClick = SpeedButton1Click
      end
      object SpeedButton2: TSpeedButton
        Left = 271
        Top = 120
        Width = 23
        Height = 22
        Caption = '-'
        OnClick = SpeedButton2Click
      end
      object Label7: TLabel
        Left = 2
        Top = 146
        Width = 42
        Height = 13
        Caption = 'Listbox1:'
      end
      object Label8: TLabel
        Left = 153
        Top = 146
        Width = 42
        Height = 13
        Caption = 'Listbox2:'
      end
      object Label6: TLabel
        Left = 2
        Top = 103
        Width = 218
        Height = 13
        Caption = 'Texto a ser adicionado no listbox selecionado:'
      end
      object Label9: TLabel
        Left = 314
        Top = 146
        Width = 42
        Height = 13
        Caption = 'Listbox3:'
      end
      object Label10: TLabel
        Left = 465
        Top = 146
        Width = 42
        Height = 13
        Caption = 'Listbox4:'
      end
      object Bevel1: TBevel
        Left = 304
        Top = 104
        Width = 2
        Height = 145
      end
      object Label11: TLabel
        Left = 314
        Top = 125
        Width = 107
        Height = 13
        Caption = 'O item selecionado foi:'
      end
      object Label12: TLabel
        Left = 434
        Top = 125
        Width = 5
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 8404992
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ListBox1: TListBox
        Left = 2
        Top = 166
        Width = 144
        Height = 80
        Ctl3D = False
        ItemHeight = 13
        Items.Strings = (
          'ma'#231'a'
          'pera'
          'uva'
          'melancia')
        ParentCtl3D = False
        TabOrder = 0
      end
      object ListBox2: TListBox
        Left = 153
        Top = 166
        Width = 142
        Height = 80
        Ctl3D = False
        ItemHeight = 13
        ParentCtl3D = False
        TabOrder = 1
      end
      object Edit3: TEdit
        Left = 2
        Top = 121
        Width = 242
        Height = 19
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 2
      end
      object Memo1: TMemo
        Left = 2
        Top = 2
        Width = 702
        Height = 79
        Anchors = [akLeft, akTop, akRight]
        Color = clInfoBk
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        Lines.Strings = (
          '1) ListBox1.Items -> ListBox2.Items'
          
            '    Quando adiciona um novo item no ListBox1 automaticamente '#233' a' +
            'dicionado no ListBox2'
          ''
          '2) ListBox3.Items -> ListBox4.Items'
          '3) ListBox3.ItemIndex -> ListBox4.ItemIndex'
          '3) ListBox3.Text -> ListBox4.ItemIndex'
          ''
          
            '    Quando selecioadiciona um novo item no ListBox1 automaticame' +
            'nte '#233' adicionado no ListBox2')
        ParentCtl3D = False
        ParentFont = False
        ReadOnly = True
        TabOrder = 3
      end
      object ListBox3: TListBox
        Left = 314
        Top = 166
        Width = 144
        Height = 80
        Ctl3D = False
        ItemHeight = 13
        Items.Strings = (
          'fusca'
          'uno mille'
          'fox'
          'vectra'
          'ferrari')
        ParentCtl3D = False
        TabOrder = 4
      end
      object ListBox4: TListBox
        Left = 465
        Top = 166
        Width = 142
        Height = 80
        Ctl3D = False
        ItemHeight = 13
        ParentCtl3D = False
        TabOrder = 5
      end
    end
  end
end
