object Form1: TForm1
  Left = 321
  Height = 620
  Top = 250
  Width = 972
  Caption = 'Стенд для отладки систем'
  ClientHeight = 620
  ClientWidth = 972
  OnCreate = FormCreate
  LCLVersion = '6.1'
  object Memo1: TMemo
    Left = 11
    Height = 442
    Top = 168
    Width = 352
    Font.CharSet = RUSSIAN_CHARSET
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Quality = fqDraft
    Lines.Strings = (
      ''
    )
    ParentFont = False
    ScrollBars = ssAutoBoth
    TabOrder = 0
    WordWrap = False
  end
  object Memo2: TMemo
    Left = 392
    Height = 444
    Top = 168
    Width = 352
    Font.CharSet = RUSSIAN_CHARSET
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Quality = fqDraft
    ParentFont = False
    ScrollBars = ssAutoBoth
    TabOrder = 1
    WordWrap = False
  end
  object Label1: TLabel
    Left = 11
    Height = 15
    Top = 152
    Width = 50
    Caption = 'Система:'
    ParentColor = False
  end
  object Label2: TLabel
    Left = 392
    Height = 15
    Top = 152
    Width = 113
    Caption = 'Результат операции:'
    ParentColor = False
  end
  object Button1: TButton
    Left = 11
    Height = 25
    Top = 8
    Width = 133
    Caption = 'Инициализация'
    OnClick = Button1Click
    TabOrder = 2
  end
  object Button2: TButton
    Left = 11
    Height = 25
    Top = 48
    Width = 133
    Caption = 'Добавить подсистему'
    OnClick = Button2Click
    TabOrder = 3
  end
  object Edit1: TEdit
    Left = 11
    Height = 23
    Top = 120
    Width = 352
    TabOrder = 4
  end
  object Button3: TButton
    Left = 208
    Height = 25
    Top = 48
    Width = 155
    Caption = 'Удалить подсистему'
    OnClick = Button3Click
    TabOrder = 5
  end
  object Button5: TButton
    Left = 392
    Height = 25
    Top = 48
    Width = 160
    Caption = 'Переместить подсистему'
    OnClick = Button5Click
    TabOrder = 6
  end
  object Edit2: TEdit
    Left = 392
    Height = 23
    Top = 120
    Width = 352
    TabOrder = 7
  end
  object Label3: TLabel
    Left = 11
    Height = 15
    Top = 104
    Width = 23
    Caption = 'Что:'
    ParentColor = False
  end
  object Label4: TLabel
    Left = 392
    Height = 15
    Top = 102
    Width = 28
    Caption = 'Куда:'
    ParentColor = False
  end
  object Button4: TButton
    Left = 11
    Height = 25
    Top = 80
    Width = 133
    Caption = 'Обновить'
    OnClick = Button4Click
    TabOrder = 8
  end
  object Button6: TButton
    Left = 208
    Height = 25
    Top = 80
    Width = 152
    Caption = 'Создать копированием'
    OnClick = Button6Click
    TabOrder = 9
  end
  object Button7: TButton
    Left = 392
    Height = 25
    Top = 80
    Width = 163
    Caption = 'Копирование системы'
    OnClick = Button7Click
    TabOrder = 10
  end
  object Button8: TButton
    Left = 584
    Height = 25
    Top = 48
    Width = 160
    Caption = 'Инициализация системы'
    OnClick = Button8Click
    TabOrder = 11
  end
  object Button9: TButton
    Left = 584
    Height = 25
    Top = 80
    Width = 160
    Caption = 'Свойства системы'
    OnClick = Button9Click
    TabOrder = 12
  end
end
