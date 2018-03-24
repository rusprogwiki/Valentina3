unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, core, unit2;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

    procedure ReadErrors();                                                  // Вывод списка с ошибками
    procedure OutInMemo(var Memo: TMemo; Source: TStringList);               // Вывод списка строк в поле мемо
    procedure ReadSource(Index: Integer);                                    // Вывод исходного текста программы
    procedure ReadVMTree();                                                  // Вывод содержимого виртуальной машины
  public

  end;

var
  Form1: TForm1;
  Vm: TVm;
  NameSource: UnicodeString;

///////////////////////////////////////////////////////////////////////////////////////

implementation

{$R *.frm}

{ TForm1 }

// Вывод содержимого виртуальной машины
procedure TForm1.ReadVMTree();
var
  temp: TStringList;
begin

     // Очистим предыдущую информацию
     Form2.Memo1.Clear;

     // Получим новое состояние
     temp:=TStringList.Create;
     Vm.ReportStorage(temp);

     // Перенесем состояние в соответствующее окно
     OutinMemo(Form2.Memo1, temp);

     // Освободим список
     temp.Free;
end;
///////////////////////////////////////////////////////////////////////////////////////

// Вывод исходного текста программы
procedure TForm1.ReadSource(Index: Integer);
var
  Temp: TStringList;
begin

     // Инциализация
     Temp:=TStringList.Create;

     // Читаем исходник
     Vm.ReadSource(Temp, Index);

     // Выводим исходник
     OutInMemo(Memo2, Temp);

     // Освобождаем список
     Temp.Free;
end;
///////////////////////////////////////////////////////////////////////////////////////

// Кнопка выбрать
procedure TForm1.Button1Click(Sender: TObject);
begin

     // Загрузка исходников
     If OpenDialog1.Execute Then
     begin

          // Пытаемся загрузить данные
          NameSource:=OpenDialog1.FileName+'';
          Memo2.Lines.LoadFromFile(NameSource);
     end;
end;
///////////////////////////////////////////////////////////////////////////////////////

// Кнопка Старт
procedure TForm1.Button2Click(Sender: TObject);
var
  temp: TStringList;
begin

     // Инциализация
     temp:=TStringList.Create;
     Memo1.Clear;

     // Скопируем содержимое исходника в интерпретатор
     temp.Text:=Memo2.Lines.Text;
     Vm.LoadProgram(temp, NameSource);

     // Есть ошибки?
     if Vm.GetErrorCount()>0 then
     begin

          ReadErrors();
          Exit;
     end;

     // Разбор программы
     Vm.Analisys();

     // Есть ошибки?
     if Vm.GetErrorCount()>0 then ReadErrors();

     // Освободим список
     temp.Free;
end;
///////////////////////////////////////////////////////////////////////////////////////

// Кнопка Состояние
procedure TForm1.Button3Click(Sender: TObject);
begin

     // Покажем состояние машины
     ReadVmTree();
     Form2.ShowModal;
end;
///////////////////////////////////////////////////////////////////////////////////////

// Ошибки в файл
procedure TForm1.Button4Click(Sender: TObject);
begin

     // Диалог открытия
     If OpenDialog1.Execute then
     begin

          // Сохраняем ошибки
          Memo2.Lines.SaveToFile(OpenDialog1.FileName);
     end;
end;
///////////////////////////////////////////////////////////////////////////////////////

// Вывод списка с ошибками
procedure TForm1.ReadErrors();
var
  Temp: TStringList;
  i, count: Integer;
Begin

     // Инцииализируем список
     Temp:=TStringList.Create;

     // Переносим сообщения об ошибках
     Vm.SmallErorrsReport(Temp);

     // Подготовим окно сообщений
     Memo1.Lines.Clear;

     // Количество сообщений об ошибках
     count:=Temp.Count;

     // В цикле
     for i:=0 to count-1 do
     begin

          // Выводим каждую строку с ошибкой
          Memo1.Lines.Add(Temp.Strings[i]);
     end;

     // Освобождаем список
     Temp.Free;
end;
///////////////////////////////////////////////////////////////////////////////////////

// Вывод списка строк в поле мемо
procedure TForm1.OutInMemo(var Memo: TMemo; Source: TStringList);
var
  i, count: Integer;
begin

     // Инциализация
     Memo.Clear;

     // Общее количество передаваемых строк
     count:=Source.Count;

     // В цикле
     for i:=0 to count-1 do
     begin

          // Каждую строку
          Memo.Lines.Add(Source.Strings[i]);
     end;
     Memo.SelStart:=0;
     Memo.SelLength := 0;

end;
///////////////////////////////////////////////////////////////////////////////////////

// Действия при создании формы
procedure TForm1.FormCreate(Sender: TObject);
begin

     // Инциализация виртуальной машины
     Vm:=TVm.Create;
end;
///////////////////////////////////////////////////////////////////////////////////////

end.

