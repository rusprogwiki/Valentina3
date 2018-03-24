unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, storage, unit2;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

    procedure OutInMemo(Memo: TMemo; data: TStringList);                     // Вывод списка в TMemo
    procedure OutTree();                                                     // Обновляет информацию в Мемо
  end;

var
  Form1: TForm1;
  x: TStorage;
  rep: TStringList;
  rez: Integer;

implementation

{$R *.frm}

{ TForm1 }

// Действия при создании формы
procedure TForm1.FormCreate(Sender: TObject);
begin

     // Создаем хранилище
     x:=TStorage.Create;
     rep:=TStringList.Create;

     // Добавляем системы
     x.AddSystem('Машина', '', false);
     x.AddSystem('Марка', 'Машина', False);
     x.AddSystem('Название', 'Машина', false);
     x.AddSystem('Ходовая часть', 'Машина', false);
     x.AddSystem('Салон', 'Машина', true);
     x.AddSystem('Руль', 'Машина.Ходовая часть', false);
     x.AddSystem('Трансмиссия', 'Машина.Ходовая часть', false);
     x.AddSystem('Сцепление', 'Машина.Ходовая часть.Трансмиссия', false);
     x.AddSystem('Фары', 'Машина', false);
     x.AddSystem('Вертолет', '', false);
     x.AddSystem('Самолет', '', false);
     x.AddSystem('Танк', '', False);

     // Выводим информацию о путях в хранилище
     OutTree();

end;
////////////////////////////////////////////////////////////////////////////////////

// Инициализация всего хранилища
procedure TForm1.Button1Click(Sender: TObject);
begin

     // Очистим хранилище
     x.Init();

     // Информация на экран
     OutTree();
end;
////////////////////////////////////////////////////////////////////////////////////

// Добавить подсистему
procedure TForm1.Button2Click(Sender: TObject);
begin

     // Пытаемся добавить систему
     Rez:=x.AddSystem(Edit1.Text, Edit2.Text, False);

     // Анализ
     If Rez=-1 then ShowMessage('Подсистема '+Edit1.Text+' уже существует!!!');
     If Rez=-2 then ShowMessage('Надсистема '+Edit2.Text+' не найдена!!!');
     If Rez=-3 then ShowMessage('Нужно задать имя системы в "ЧТО"!!!');

     // На экран
     OutTree();
end;
////////////////////////////////////////////////////////////////////////////////////

// Удаление подсистемы
procedure TForm1.Button3Click(Sender: TObject);
begin

     // Пытаемся удалить систему
     Rez:=x.EraseSystem(Edit1.Text);

     // Анализ
     If Rez=-1 Then ShowMessage('Система '+Edit1.Text+' не найдена :(');

     // На экран
     OutTree();
end;
////////////////////////////////////////////////////////////////////////////////////

// Кнопка Обновить
procedure TForm1.Button4Click(Sender: TObject);
begin

     OutTree();
end;
////////////////////////////////////////////////////////////////////////////////////

// Переименовать подсистему
// Edit1 - подсистема
// Edit2 - новое имя
procedure TForm1.Button5Click(Sender: TObject);
begin

     // Перемещаем/переименовываем подсистему
     Rez:=x.RemoveSystem(Edit1.Text, Edit2.Text);

     // На экран
     OutTree();

     // Анализ
     If Rez=-1 then ShowMessage('Перемещаемая система '+Edit1.text+' не найдена');
     if Rez=-2 then ShowMessage('Подсистема '+Edit2.Text+' не найдена!');
     if Rez=-3 then ShowMessage('Система '+Edit2.Text+' вложена в '+Edit1.Text);
end;
////////////////////////////////////////////////////////////////////////////////////

// Копировать подсистему
// Eit1 - что копировать
// Edit2 - куда копировать
procedure TForm1.Button6Click(Sender: TObject);
begin

     // Копируем подсистему
     Rez:=x.AddSystem2(Edit1.Text, Edit2.Text);

     // На экран
     OutTree();

     // Анализ
     If Rez=-1 then ShowMessage('Источник '+Edit1.text+' не найден!');
     If Rez=-2 then ShowMessage('Такая система '+Edit2.text+' уже существует');
     If Rez=-3 then ShowMessage('Путь до системы '+Edit2.text+' не найден');
     If Rez=-4 then ShowMessage('Система '+Edit2.Text+' вложена в '+Edit1.Text);
end;
////////////////////////////////////////////////////////////////////////////////////

// Копирование системы
// Edit1 - что копируем
// Edit2 - куда копируем
procedure TForm1.Button7Click(Sender: TObject);
begin

     // Пытаемся выполнить операцию
     Rez:=x.CopySystem(Edit1.Text, Edit2.Text);

     // На экран
     OutTree();

     // Анализ
     If Rez=-1 then ShowMessage('Источник '+Edit1.text+' не найден!');
     If Rez=-2 then ShowMessage('Приемник '+Edit2.text+' не найден!');
     If Rez=-3 then ShowMessage('Система '+Edit2.Text+' вложена в '+Edit1.Text);
end;
////////////////////////////////////////////////////////////////////////////////////

// Инициализация системы
// Edit1 - система для инициализации
procedure TForm1.Button8Click(Sender: TObject);
begin

     // Операция
     Rez:=x.InitSystem(Edit1.Text);

     // На экран
     OutTree();

     // Анализ
     If Rez=-1 then ShowMessage('Система '+Edit1.text+' не найдена!');
end;
/////////////////////////////////////////////////////////////////////////////////////

// Свойства системы
// Искомая система в Edit1
procedure TForm1.Button9Click(Sender: TObject);
var
  str1: String;
begin

     // Проверим существование системы
     If x.CheckSystem(Edit1.Text)=False then
     begin

          // Сообщим об ошибке
          ShowMessage('Система '+Edit1.Text+' не найдена!');
          Exit;
     end;

     // Настроим окно со свойствами системы
     // Имя системы
     Form2.Label1.Caption:=Edit1.Text;

     // Строковое значение системы
     Form2.Label2.Caption:='Строковое значение системы:';
     x.GetValueSystem(Edit1.Text, str1);
     Form2.Edit1.Text:=str1;

     // Тип системы
     Form2.Label3.Caption:='Тип системы:';
     x.GetTypeSystem(Edit1.Text, str1);
     Form2.Edit2.Text:=str1;

     // Природа системы
     x.GetNatureSystem(Edit1.Text, Rez);
     Form2.Label4.Caption:='Природа системы: '+IntToStr(Rez);

     // Ссылка на функцию
     x.GetFunckLink(Edit1.Text, Rez);
     Form2.Label5.Caption:='Ссылка на функцию: '+IntToStr(Rez);

     // Отобразим форму
     Form2.ShowModal;

     // Вносим изменения
     // Строковое значение
     str1:=Form2.Edit1.Text;
     x.SetValue(Edit1.Text, str1);

     // Тип системы
     str1:=Form2.Edit2.Text;
     x.SetTypeSystem(Edit1.Text, str1);
end;
////////////////////////////////////////////////////////////////////////////////////

// Вывод списка в TMemo
procedure TForm1.OutInMemo(Memo: TMemo; data: TStringList);
var
  i, count: Integer;
begin

     // Число строк в списке
     count:=data.Count;

     // Построчно
     for i:=0 to count-1 do
     begin

          // Вносим в Memo
          Memo.Lines.Add(data[i]);
     end;
end;
////////////////////////////////////////////////////////////////////////////////////

// Обновляет информацию в Мемо
procedure TForm1.OutTree();
begin

     // Подготовка
     Memo1.Clear;
     Memo2.Clear;
     rep.Clear;

     // Первый отчет
     x.Report_Keys(rep);
     OutInMemo(Memo1, rep);

     // Второй отчет
     rep.Clear;
     x.ReportRoot(rep, false);
     //x.Report_Keys(rep);
     OutInMemo(Memo2, rep);
end;
////////////////////////////////////////////////////////////////////////////////////

end.

