object CustomizeScreenControl: TCustomizeScreenControl
  Left = 113
  Top = 302
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Customize screen control'
  ClientHeight = 281
  ClientWidth = 586
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 246
    Width = 586
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
      Left = 5
      Top = 5
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      TabOrder = 0
      OnClick = btbtOKClick
    end
  end
  object actlActions: TActionList
    Left = 184
    Top = 336
    object actnClose: TAction
      Caption = 'Close'
      ShortCut = 27
      OnExecute = actnCloseExecute
    end
  end
end
