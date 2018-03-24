object Form1: TForm1
  Left = 256
  Height = 631
  Top = 169
  Width = 743
  Caption = 'Стенд для сборки интерпретатора'
  ClientHeight = 631
  ClientWidth = 743
  OnCreate = FormCreate
  LCLVersion = '6.1'
  object Label1: TLabel
    Left = 8
    Height = 15
    Top = 8
    Width = 56
    Caption = 'Исходник:'
    ParentColor = False
  end
  object Button1: TButton
    Left = 664
    Height = 25
    Top = 8
    Width = 75
    Caption = 'Выбрать'
    OnClick = Button1Click
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 5
    Height = 104
    Top = 520
    Width = 728
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 5
    Height = 379
    Top = 112
    Width = 733
    ScrollBars = ssAutoBoth
    TabOrder = 2
  end
  object Label2: TLabel
    Left = 5
    Height = 15
    Top = 96
    Width = 20
    Caption = 'Код'
    ParentColor = False
  end
  object Label3: TLabel
    Left = 5
    Height = 15
    Top = 504
    Width = 66
    Caption = 'Сообщения'
    ParentColor = False
  end
  object Button2: TButton
    Left = 8
    Height = 25
    Top = 32
    Width = 75
    Caption = 'Старт'
    OnClick = Button2Click
    TabOrder = 3
  end
  object Button3: TButton
    Left = 8
    Height = 25
    Top = 64
    Width = 75
    Caption = 'Состояние'
    OnClick = Button3Click
    TabOrder = 4
  end
  object Button4: TButton
    Left = 96
    Height = 25
    Top = 64
    Width = 115
    Caption = 'Ошибки в файл'
    OnClick = Button4Click
    TabOrder = 5
  end
  object OpenDialog1: TOpenDialog
    Left = 592
    Top = 8
  end
end
