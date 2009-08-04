object Form1: TForm1
  Left = 494
  Top = 25
  Width = 732
  Height = 477
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
    724
    443)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 5
    Top = 4
    Width = 714
    Height = 435
    ActivePage = TabSheet3
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Binding Edits'
      DesignSize = (
        706
        407)
      object Label1: TLabel
        Left = 65
        Top = 194
        Width = 32
        Height = 13
        Caption = 'Label1'
      end
      object l1: TLabel
        Left = 4
        Top = 170
        Width = 48
        Height = 13
        Caption = 'Edit1 ----->'
      end
      object l2: TLabel
        Left = 4
        Top = 194
        Width = 47
        Height = 13
        Caption = 'Label1 -->'
      end
      object l3: TLabel
        Left = 4
        Top = 218
        Width = 45
        Height = 13
        Caption = 'Edit2 ---->'
      end
      object Label5: TLabel
        Left = 196
        Top = 218
        Width = 266
        Height = 13
        Caption = '<------- Mude o valor para clRed por exemplo e tecle TAB'
      end
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
        Caption = 'Tratando Edits'
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
      object Label34: TLabel
        Left = 5
        Top = 15
        Width = 701
        Height = 12
        Alignment = taCenter
        AutoSize = False
        Caption = 'Tratando ListBoxes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Edit1: TEdit
        Left = 64
        Top = 167
        Width = 257
        Height = 19
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        Text = 'Edit1'
      end
      object Button1: TButton
        Left = 333
        Top = 163
        Width = 204
        Height = 25
        Caption = 'Mudar Edit1.Text por programa'#231'ao '
        TabOrder = 1
        OnClick = Button1Click
      end
      object Edit2: TEdit
        Left = 64
        Top = 215
        Width = 121
        Height = 19
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 2
        Text = 'clSkyBlue'
      end
      object Memo2: TMemo
        Left = 2
        Top = 32
        Width = 701
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
          
            '    O botao mostra que a atualiza'#231'ao funciona por programa'#231'ao ta' +
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
        706
        407)
      object Label2: TLabel
        Left = 4
        Top = 222
        Width = 79
        Height = 13
        Caption = 'CheckBox1 ----->'
      end
      object Label3: TLabel
        Left = 4
        Top = 260
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
      object Label16: TLabel
        Left = 2
        Top = 3
        Width = 701
        Height = 11
        AutoSize = False
        Color = 9276813
        ParentColor = False
      end
      object Label22: TLabel
        Left = 2
        Top = 14
        Width = 701
        Height = 12
        AutoSize = False
        Color = 5197647
        ParentColor = False
      end
      object Label23: TLabel
        Left = 2
        Top = 7
        Width = 701
        Height = 12
        Alignment = taCenter
        AutoSize = False
        Caption = 'Tratando CheckBox'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label24: TLabel
        Left = 2
        Top = 24
        Width = 701
        Height = 2
        AutoSize = False
        Color = 3947580
        ParentColor = False
      end
      object CheckBox1: TCheckBox
        Left = 96
        Top = 220
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
        Top = 258
        Width = 81
        Height = 17
        Caption = 'CheckBox2'
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
      end
      object Button2: TButton
        Left = 184
        Top = 216
        Width = 185
        Height = 25
        Caption = 'Togle CheckBox1'
        TabOrder = 2
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 184
        Top = 254
        Width = 185
        Height = 25
        Caption = 'Togle CheckBox2'
        TabOrder = 3
        OnClick = Button3Click
      end
      object Memo3: TMemo
        Left = 2
        Top = 32
        Width = 701
        Height = 177
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
          '    Estamos usando o converter padrao TBooleanToText'
          ''
          '2) Checkbox2.Checked -> Checkbox2.Caption. '
          
            '    An'#225'logo ao anterior mas agora usando um par'#225'metro para conve' +
            'rsao'
          '    TBooleanToText [Invisivel;Visivel]'
          ''
          '3) Checkbox3.Checked -> Panel1.Visible'
          '    O Checked do Checkbox3 determina a visibilidade do Panel1'
          ''
          'Obs:'
          
            '    Os botoes mostram que a atualiza'#231'ao funciona por programa'#231'ao' +
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
        Top = 216
        Width = 327
        Height = 108
        Caption = 'A visibilidade deste panel depende do CheckBox3.Checked'
        TabOrder = 6
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Binding ListBox'
      ImageIndex = 2
      DesignSize = (
        706
        407)
      object Label7: TLabel
        Left = 2
        Top = 294
        Width = 42
        Height = 13
        Caption = 'Listbox1:'
      end
      object Label8: TLabel
        Left = 153
        Top = 294
        Width = 42
        Height = 13
        Caption = 'Listbox2:'
      end
      object Label6: TLabel
        Left = 2
        Top = 251
        Width = 218
        Height = 13
        Caption = 'Texto a ser adicionado no listbox selecionado:'
      end
      object Label9: TLabel
        Left = 314
        Top = 254
        Width = 42
        Height = 13
        Caption = 'Listbox3:'
      end
      object Label10: TLabel
        Left = 521
        Top = 254
        Width = 42
        Height = 13
        Caption = 'Listbox4:'
      end
      object Bevel1: TBevel
        Left = 304
        Top = 252
        Width = 2
        Height = 145
      end
      object Label11: TLabel
        Left = 314
        Top = 385
        Width = 141
        Height = 13
        Caption = 'Item selecionado no ListBox3:'
      end
      object Label12: TLabel
        Left = 464
        Top = 385
        Width = 49
        Height = 13
        AutoSize = False
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 8404992
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object Label17: TLabel
        Left = 2
        Top = 3
        Width = 701
        Height = 11
        AutoSize = False
        Color = 9276813
        ParentColor = False
      end
      object Label18: TLabel
        Left = 2
        Top = 14
        Width = 701
        Height = 12
        AutoSize = False
        Color = 5197647
        ParentColor = False
      end
      object Label19: TLabel
        Left = 2
        Top = 7
        Width = 701
        Height = 12
        Alignment = taCenter
        AutoSize = False
        Caption = 'Tratando ListBoxes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label21: TLabel
        Left = 2
        Top = 24
        Width = 701
        Height = 2
        AutoSize = False
        Color = 3947580
        ParentColor = False
      end
      object Label20: TLabel
        Left = 2
        Top = 3
        Width = 701
        Height = 2
        AutoSize = False
        Color = 8289918
        ParentColor = False
      end
      object Label13: TLabel
        Left = 520
        Top = 385
        Width = 183
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 8404992
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object Label14: TLabel
        Left = 312
        Top = 364
        Width = 24
        Height = 13
        Caption = 'Edit4'
      end
      object Label15: TLabel
        Left = 523
        Top = 364
        Width = 24
        Height = 13
        Caption = 'Edit5'
      end
      object Label29: TLabel
        Left = 134
        Top = 352
        Width = 21
        Height = 13
        Caption = '<--->'
      end
      object Label30: TLabel
        Left = 494
        Top = 308
        Width = 15
        Height = 13
        Caption = '--->'
      end
      object ListBox1: TListBox
        Left = 2
        Top = 314
        Width = 127
        Height = 84
        Ctl3D = False
        ItemHeight = 13
        Items.Strings = (
          'ma'#231'a'
          'pera'
          'uva'
          'melancia')
        ParentCtl3D = False
        TabOrder = 0
        OnExit = ListBox1Exit
      end
      object ListBox2: TListBox
        Left = 160
        Top = 314
        Width = 135
        Height = 84
        Ctl3D = False
        ItemHeight = 13
        ParentCtl3D = False
        TabOrder = 1
        OnExit = ListBox1Exit
      end
      object Edit3: TEdit
        Left = 2
        Top = 269
        Width = 215
        Height = 19
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 2
      end
      object Memo1: TMemo
        Left = 2
        Top = 32
        Width = 701
        Height = 209
        Anchors = [akLeft, akTop, akRight]
        Color = clInfoBk
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        Lines.Strings = (
          '1) ListBox1.Items -> ListBox2.Items - 2Way'
          
            '    Quando adiciona/remove/clear um novo item no ListBox1 ou Lis' +
            'tBox2 automaticamente '#233' adicionado no ListBox '
          'contr'#225'rio'
          '2) ListBox3.Items -> ListBox4.Items'
          '    Os items do ListBox3 aparecem no ListBox4'
          '3) ListBox3.ItemIndex -> ListBox4.ItemIndex'
          
            '    Ao selecionar um item no ListBox3 o mesmo item '#233' selecionado' +
            'ListBox4'
          '4) ListBox3.ItemIndex -> Label12.Caption  -  TItemIndexToText'
          
            '    O Label12.Caption mostra o valor do item selecionado no List' +
            'Box3'
          
            '5) ListBox3.ItemIndex -> Edit4.Text   -   TItemIndexToText   -  ' +
            '2Way;'
          
            '    Ao por um texto no Edit4 e sair do mesmo o item corresponden' +
            'te '#233' selecionado no ListBox3.'
          
            '6) ListBox4.ItemIndex -> Label13.Caption  -  TItemIndexToInteger' +
            'Text'
          
            '    O Label13.Caption mostra o index do item selecionado no List' +
            'Box4'
          
            '7) ListBox4.ItemIndex -> Edit5.Text   -   TItemIndexToIntegerTex' +
            'te   -  2Way;'
          
            '    Ao por um numero no Edit4 e sair do mesmo o itemindex do Lis' +
            'tBox4 '#233' modificado.')
        ParentCtl3D = False
        ParentFont = False
        ReadOnly = True
        TabOrder = 3
      end
      object ListBox3: TListBox
        Left = 312
        Top = 274
        Width = 177
        Height = 83
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
        Left = 522
        Top = 274
        Width = 180
        Height = 83
        Ctl3D = False
        ItemHeight = 13
        ParentCtl3D = False
        TabOrder = 5
      end
      object SpeedButton1: TButton
        Left = 223
        Top = 268
        Width = 23
        Height = 22
        Caption = '+'
        TabOrder = 6
        OnClick = SpeedButton1Click
      end
      object SpeedButton2: TButton
        Left = 247
        Top = 268
        Width = 23
        Height = 22
        Caption = '-'
        TabOrder = 7
        OnClick = SpeedButton2Click
      end
      object Button4: TButton
        Left = 271
        Top = 268
        Width = 23
        Height = 22
        Caption = 'C'
        TabOrder = 8
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 397
        Top = 249
        Width = 91
        Height = 22
        Caption = 'ItemIndex=2'
        TabOrder = 9
        OnClick = Button5Click
      end
      object Edit4: TEdit
        Left = 343
        Top = 361
        Width = 146
        Height = 19
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 10
        Text = 'Edit4'
      end
      object Edit5: TEdit
        Left = 552
        Top = 361
        Width = 151
        Height = 19
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 11
        Text = 'Edit5'
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'TabSheet4'
      ImageIndex = 3
      DesignSize = (
        706
        407)
      object Label31: TLabel
        Left = 2
        Top = 3
        Width = 701
        Height = 11
        AutoSize = False
        Color = 9276813
        ParentColor = False
      end
      object Label32: TLabel
        Left = 2
        Top = 14
        Width = 701
        Height = 12
        AutoSize = False
        Color = 5197647
        ParentColor = False
      end
      object Label35: TLabel
        Left = 2
        Top = 7
        Width = 701
        Height = 12
        Alignment = taCenter
        AutoSize = False
        Caption = 'Tratando ListBoxes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label33: TLabel
        Left = 10
        Top = 289
        Width = 93
        Height = 13
        Caption = 'O item selecionado:'
      end
      object Label36: TLabel
        Left = 112
        Top = 289
        Width = 121
        Height = 13
        AutoSize = False
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 8404992
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object Label37: TLabel
        Left = 240
        Top = 289
        Width = 39
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 8404992
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object Memo4: TMemo
        Left = 2
        Top = 32
        Width = 701
        Height = 209
        Anchors = [akLeft, akTop, akRight]
        Color = clInfoBk
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        Lines.Strings = (
          '1) ListBox1.Items -> ListBox2.Items - 2Way'
          
            '    Quando adiciona/remove/clear um novo item no ListBox1 ou Lis' +
            'tBox2 automaticamente '#233' adicionado no ListBox '
          'contr'#225'rio'
          '2) ListBox3.Items -> ListBox4.Items'
          '    Os items do ListBox3 aparecem no ListBox4'
          '3) ListBox3.ItemIndex -> ListBox4.ItemIndex'
          
            '    Ao selecionar um item no ListBox3 o mesmo item '#233' selecionado' +
            'ListBox4'
          '4) ListBox3.ItemIndex -> Label12.Caption  -  TItemIndexToText'
          
            '    O Label12.Caption mostra o valor do item selecionado no List' +
            'Box3'
          
            '5) ListBox3.ItemIndex -> Edit4.Text   -   TItemIndexToText   -  ' +
            '2Way;'
          
            '    Ao por um texto no Edit4 e sair do mesmo o item corresponden' +
            'te '#233' selecionado no ListBox3.'
          
            '6) ListBox4.ItemIndex -> Label13.Caption  -  TItemIndexToInteger' +
            'Text'
          
            '    O Label13.Caption mostra o index do item selecionado no List' +
            'Box4'
          
            '7) ListBox4.ItemIndex -> Edit5.Text   -   TItemIndexToIntegerTex' +
            'te   -  2Way;'
          
            '    Ao por um numero no Edit4 e sair do mesmo o itemindex do Lis' +
            'tBox4 '#233' modificado.')
        ParentCtl3D = False
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
      object ComboBox1: TComboBox
        Left = 16
        Top = 256
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 1
        Items.Strings = (
          'fusca'
          'uno mille'
          'fox'
          'vectra'
          'ferrari')
      end
    end
  end
end
