object Form2: TForm2
  Left = 256
  Height = 420
  Top = 250
  Width = 711
  Caption = 'Внутренее состояние виртуальной машины'
  ClientHeight = 420
  ClientWidth = 711
  LCLVersion = '6.1'
  object Memo1: TMemo
    Left = 8
    Height = 368
    Top = 16
    Width = 696
    ScrollBars = ssAutoBoth
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Height = 25
    Top = 392
    Width = 75
    Caption = 'В файл'
    OnClick = Button1Click
    TabOrder = 1
  end
  object SaveDialog1: TSaveDialog
    Left = 672
    Top = 392
  end
end
