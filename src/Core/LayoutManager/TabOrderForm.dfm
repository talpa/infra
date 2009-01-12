object TabOrderForm: TTabOrderForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Edit Item Order'
  ClientHeight = 355
  ClientWidth = 526
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 320
    Width = 526
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btbtCancel: TButton
      Left = 85
      Top = 5
      Width = 75
      Height = 25
      Caption = '&Cancel'
      TabOrder = 1
      OnClick = btbtCancelClick
    end
    object btbtOK: TButton
      Left = 4
      Top = 5
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btbtOKClick
    end
  end
  object pnlClient: TPanel
    Left = 0
    Top = 0
    Width = 526
    Height = 320
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlRigthButtons: TPanel
      Left = 493
      Top = 0
      Width = 33
      Height = 320
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object btbtUp: TSpeedButton
        Left = 3
        Top = 73
        Width = 26
        Height = 55
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
          3333333333777F33333333333309033333333333337F7F333333333333090333
          33333333337F7F33333333333309033333333333337F7F333333333333090333
          33333333337F7F33333333333309033333333333FF7F7FFFF333333000090000
          3333333777737777F333333099999990333333373F3333373333333309999903
          333333337F33337F33333333099999033333333373F333733333333330999033
          3333333337F337F3333333333099903333333333373F37333333333333090333
          33333333337F7F33333333333309033333333333337373333333333333303333
          333333333337F333333333333330333333333333333733333333}
        NumGlyphs = 2
        OnClick = btbtUpClick
      end
      object btbtDown: TSpeedButton
        Left = 3
        Top = 130
        Width = 26
        Height = 55
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
          333333333337F33333333333333033333333333333373F333333333333090333
          33333333337F7F33333333333309033333333333337373F33333333330999033
          3333333337F337F33333333330999033333333333733373F3333333309999903
          333333337F33337F33333333099999033333333373333373F333333099999990
          33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
          33333333337F7F33333333333309033333333333337F7F333333333333090333
          33333333337F7F33333333333309033333333333337F7F333333333333090333
          33333333337F7F33333333333300033333333333337773333333}
        NumGlyphs = 2
        OnClick = btbtDownClick
      end
    end
    object lsvTabOrder: TListView
      Left = 0
      Top = 0
      Width = 493
      Height = 320
      Align = alClient
      BorderWidth = 1
      Columns = <
        item
          Caption = 'Name'
          Width = 155
        end
        item
          Caption = 'Caption'
          Width = 185
        end
        item
          Caption = 'Control class'
          Width = 132
        end>
      ColumnClick = False
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object actlActions: TActionList
    Left = 248
    Top = 320
    object actnClose: TAction
      Caption = 'Close'
      ShortCut = 27
      OnExecute = actnCloseExecute
    end
  end
end
