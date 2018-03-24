unit storage;

{$mode objfpc}{$H+}
{$codepage UTF8}

interface

uses
  {$IFNDEF WINDOWS}
    cwstring,
    {$ENDIF}
   Classes, SysUtils, Contnrs, lazutf8;

// Юнит для хранения узлов, предоставляет основные структуры хранения данных и первоначальную инфраструктуру доступа
// Ядро
type
    TCore=record

       Name: UnicodeString;              // Имя системы
       Value: UnicodeString;             // Значение системы
       Nature: Integer;                  // Простая/сложная система 0/1
                                                   // 2 - закрытая часть системы
                                                   // 3 - система-функция
                                                   // 4 - закрытая система-функция
       AType: UnicodeString;             // Тип системы
       Start_Point: UnicodeString;       // Точка старта системы
       Construct: UnicodeString;         // Конструктор системы
       FuncLink: Integer;                // Ссылка на тело функции
    end;
//////////////////////////////////////////////////////////////////////////

// Система
type
    TSystem=class

        private

            procedure InitCore();                                         // Инициализация ядра системы
        public
            Core: TCore;                                                  // Ядро системы
            ExtModules: Array of UnicodeString;                           // Список внешних модулей
            constructor Create;                                           // Конструктор
            procedure Init();                                             // Инициализатор
            destructor Destroy(); override;                               // Деструктор
    end;
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

// Параметры учета систем в хранилище
type
    TParams=record

        Divider: UnicodeString;                                           // Символ-разделитель в сцепленном ключе
        Hide_marker: UnicodeString;                                       // Символ-маркер скрытого раздела системы
    end;
//////////////////////////////////////////////////////////////////////////

// Ядро инструкции
type
    TInstruction=record
        Start: Integer;                                                   // Местоположение в исходном коде
        Id: Integer;                                                      // Идентификатор конструкции/блока
        Link: Integer;                                                    // Ссылка на внешний блок
        Param: Array [1..4] of  UnicodeString;                            // Параметры конструкции
    end;
//////////////////////////////////////////////////////////////////////////

// Полная блок-инструкция
type
    TConstruct=record
        Core: TInstruction;                                               // Ядро инструкции
        Link1: Integer;                                                   // Ссылка на другой блок/конструкцию
        Link2: Integer;                                                   // Ссылка на другой блок/конструкцию

        Codes: Array of TInstruction;                                     // Инструкции, содержащиеся в блоке
    end;
//////////////////////////////////////////////////////////////////////////

// Вспомогательная структура для передачи параметров-массивов в функции
type
    TMass=array of UnicodeString;
//////////////////////////////////////////////////////////////////////////

// Список систем
type
    TStorage=class
        private
            data: TObjectList;                                                                    // Список систем
            Param: TParams;                                                                       // Параметры хранения систем
            Progs: Array of TConstruct;                                                           // Описание функций систем

            // Служебные методы
            function  InsertedIndex(Name: UnicodeString; Parent: Integer): Integer;               // Индекс для добавления системы в хранилище
            function  FindSystem(Name: UnicodeString; StartPos: Integer): Integer;                // Поиск системы в хранилище
            procedure InitParams();                                                               // Инициализация параметров хранения систем
            procedure AddRoot();                                                                  // Добавление корневой системы
            function  GetLastNode(Line: String): UnicodeString;                                   // Извлечние последнего узла в пути
            function  Spacer(Count: Integer; Line: String): UnicodeString;                        // Забивает пробелами указанное количество символов
            function  NewNamer(OldKey, Node, NewKey: String): UnicodeString;                      // Генератор путей для переноса подсистем
            function  Move(Index1, Index2: Integer; ANewName: String): Integer;                   // Перемещение системы и всех ее подсистем
            function  CheckRename(OldKey, NewKey: String): Boolean;                               // Проверка, является ли новый путь переименованием системы
            function  DivideKey(var Key: String): UnicodeString;                                  // Разделение ключа на путь и имя
            procedure CopySystem(Source, Dest: Integer);                                          // Копирование системы
            procedure InitSystem(Index: Integer);                                                 // Инициализация системы
            function GenerateSpace(Count: Integer): UnicodeString;                                // Генерируем пробелы
            function GetAllSubSystems(Index: Integer; var SusbSystems: TMass): Integer;           // Возвращает массив подсистем первого уровня
        public

            // Служебные
            constructor Create;                                                                   // Конструктор
            destructor  Destroy; override;                                                        // Деструктор
            procedure   Init();                                                                   // Инициализатор
            function    AddSystem(Name, Value, AType: UnicodeString;
                               Nature, Index: Integer): Integer;                                  // Добавление системы

            // Пользовательские - полный доступ
            function AddSystem(Name, SuperSystem: UnicodeString; Part: Boolean): Integer;         // Добавляем систему
            function AddSystem(Name, Value, SuperSystem: UnicodeString; Part: Boolean): Integer;  // Добавляем простую систему
            function AddSystem(Name, SuperSystem, Value, AType: UnicodeString;
                               Nature: Integer): Integer;                                         // Добавляет подсистему с заданными свойствами
            function EraseSystem(Key: String): Integer;                                           // Удаление подсистемы
            function RemoveSystem(OldKey, NewKey: String): Integer;                               // Перемещение/переменование системы
            function AddSystem2(OldKey, NewKey: String): Integer;                                 // Добавляет систему на основе уже существующей
            function CopySystem(Source, Dest: String): Integer;                                   // Копирует систему на место другой системы
            function InitSystem(Sys: String): Integer;                                            // Инициализация системы

            // Пользовательские - свойства и индикаторы системы
            function CheckSystem(Sys: String): Boolean;                                           // Проверка существования системы по указанному ключу
            function GetValueSystem(Sys: UnicodeString; var Rez: String): Boolean;                // Чтение текстового значения системы
            function GetNatureSystem(Sys: UnicodeString; var Rez: Integer): Boolean;              // Чтение внутреннего представления системы
            function GetTypeSystem(Sys: UnicodeString; var Rez: String): Boolean;                 // Чтение типа системы
            function GetFunckLink(Sys: UnicodeString; var Rez: Integer): Boolean;                 // Чтение ссылки на функцию
            function CheckSubSystem(Sys: String): Integer;                                        // Проверка существования подситем
            function GetCountHideSystem(Sys: String): Integer;                                    // Количество скрытых подсистем
            function CheckNesting(OldKey, NewKey: String): Boolean;                               // Определяет вложенность ключей
            function SetValue(Sys, NewValue: String): Integer;                                    // Изменение текстового значения функции
            function SetTypeSystem(Sys, NewType: String): Integer;                                // Изменение типа системы

            // Работа с кодом
            function GetCountBlock(): Integer;                                                    // Общее количество блоков кода в системе

            // Пользовательские - открытая часть

            // Отчеты
            procedure Report_Keys(var rep: TStringList);                                          // Все пути
            procedure ReportRoot(var rep: TStringList; detail: Boolean);                          // Полное дерево хранилища
            procedure FullReportRoot(var Rep: TStringList);                                       // Подробный отчет по дереву хранилища

            // Прочие
            procedure AddServiceSystem();                                                         // Добавление служебных систем для исполнения программ

            // ПОДДЕРЖКА МОДЕЛИ ВЫЧИСЛЕНИЙ
            // Методы по объявлению систем
            procedure AddModule(NameModule: UnicodeString);                                       // Добавление модуля для хранения описаний внешних систем
            function AddExternalSystem(NameModule, NameSystem: UnicodeString): Integer;           // Добавление внешней системы в указанный модуль
            function Add_StartPoint_In_Declaration(Module, Sys,
                                                   StartPoint: UnicodeString): Integer;           // Добавляет в описание системы стартовую точку
            function Add_Constructor_In_Declaration(Module, Sys,
                                                    Construct: UnicodeString): Integer;           // Добавляет в описание системы функцию-конструктор
            function FindExtModules(Module, Sys, ExtModule: UnicodeString): Integer;              // Поиск имени модуля среди внешних, подключаемых модулей системы

    end;
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

implementation

// Возвращает массив подсистем первого уровня!!!
// Подсистемы нижнего уровня не рассматриваются!!!
// Index - индекс системы в хранилище
// SusbSystems - массив с именами подсистем первого уровня
// 0 - Операция прошла успешно
// -1 - Система не найдена в хранилище
function TStorage.GetAllSubSystems(Index: Integer; var SusbSystems: TMass): Integer;
var
  Index, count, i, len, position, uk: Integer;
  Path, NameSystem: UnicodeString;
begin

     // Инициализация
     Result:=0;

     // Полный путь
     Path:='1'+Param.Divider+'0'+Param.Divider+Module+Param.Divider+Sys;

     // Позиция системы в хранилище
     Index:=FindSystem(Path, 0);

     // Нашли систему?
     If Index<0 then
     begin

          // Сообщим об ошибке
          Result:=-1;
          Exit;
     end;

     // Инициализируем список подсистем
     SetLength(SubSystems, 0);

     // Определим количество всех систем в хранилище
     count:=data.Count;

     // Сканируем имена систем
     for i:=Index+1 to count-1 do
     begin

          // Получим путь исследуемой системы
          NameSystem:=TSystem(data[Index]).Core.Name;

          // Проверим, является ли данная система вложенной в нашу систему
          if UTF8Pos(Path, NameSystem)<>1 then
          begin

               // Это уже не наши системы пошли
               Exit;
          end;

          // Размер пути до системы
          len:=UTF8Length(Path+Param.Divider)+1;

          // Получим имя без пути
          NameSystem:=UTF8Copy(NameSystem, len, UTF8Length(NameSystem)-len);

          // Отсекаем подсистемы
          position:=UTF8Pos(Param.Divider, NameSystem);
          if position>0 then
          begin

               // Берем имя до первого разделителя
               NameSystem:=UTF8Copy(NameSystem, 1, position-UTF8Length(Param.Divider));
          end;

          // Такое имя уже есть?
          uk:=Length(SubSystem);
          if uk>0 then
          begin

               // Сравним с последней добавленной подсистемой
               If SubSystem[uk-1]<>NameSystem Then
               begin

                    // Таких еще не добавляли
                    SetLength(SubSystem, uk+1);
                    SubSystem[uk]:=NameSystem;
               end;
          end;
     end;
end;
//////////////////////////////////////////////////////////////////////////

// Поиск имени модуля среди внешних, подключаемых модулей системы
// Предполагается поиск только в ОПИСАНИЯХ СИСТЕМ!!!!
// ФУНКЦИЯ НЕ ПРЕДНАЗНАЧЕНА ДЛЯ РАБОТЫ ВО ВРЕМЯ ИСПОЛНЕНИЯ!!!!
// Sys - система, в которой осуществляется поиск
// Module - модуль в котором содержится данная система
// ExtModule - исхомый внешний модуль
// >-1 - индекс внешнего модуля в хранилище
// -1 - модуль не найден
// -2 - система не найдена
// ФУНКЦИЯ ОСУЩЕСТВЛЯЕТ ПОИСК ПРОСТЫМ ПЕРЕБОРОМ В ЛОБ
// ЕСЛИ ДОРАБОТАТЬ ФУНКЦИЮ ДОБАВЛНИЯ ВНЕШНИХ МОДУЛЕЙ
// МОЖНО БУДЕТ ИСПОЛЬЗОВАТЬ БИНАРНЫЙ ПОИСК ЧТО НЕМНОГО БЫСТРЕЙ
// НЕОБХОДИМО ПРОВОДИТЬ ИССЛЕДОВАНИЯ
// ВОЗМОЖНО ОПЕРАЦИЯ ДОБАВЛЕНИЯ ВНЕШНЕГО МОДУЛЯ СКРОЕТ ВСЮ СКОРОСТЬ ПОИСКА
// ТАК КАК ПРЕДПОЛАГАЕТСЯ НЕ ЧАСТЫЙ ВЫЗОВ ВНЕШНИХ МОДУЛЕЙ
// (ТОЛЬКО В СЕКЦИИ ОПИСАНИЯ СИСТЕМ)
function TStorage.FindExtModules(Module, Sys, ExtModule: UnicodeString): Integer;
var
  Index: Integer;
  Path: UnicodeString;
  i, count: Integer;
begin

     // Инициализация
     Result:=0;

     // Полный путь
     Path:='1'+Param.Divider+'0'+Param.Divider+Module+Param.Divider+Sys;

     // Позиция системы в хранилище
     Index:=FindSystem(Path, 0);

     // Нашли систему?
     If Index<0 then
     begin

          // Сообщим об ошибке
          Result:=-2;
          Exit;
     end;

     // Количество внешних модулей системы
     count:=Length(TSystem(data[Index]).ExtModules);

     // Сканируем внешние модули системы
     for i:=0 to count-1 do
     begin

          // Совпало?
          If TSystem(data[Index]).ExtModules[i]=ExtModule Then
          begin

               // Передаем индекс, операция завершена
               Result:=i;
               Exit;
          end;
     end;

     // Ничего не нашли
     Result:=-1;
end;
//////////////////////////////////////////////////////////////////////////

// Добавляет в описание системы функцию-конструктор
// Module - Имя модуля
// Sys - Имя системы
// StartPoint - имя стартовой точки
// 0 - операция прошла успешно
// -1 - Система не обнаружена
function TStorage.Add_Constructor_In_Declaration(Module, Sys, Construct: UnicodeString): Integer;
var
   Index: Integer;
   Path: UnicodeString;
begin

     // Инициализация
     Result:=0;

     // Полный путь
     Path:='1'+Param.Divider+'0'+Param.Divider+Module+Param.Divider+Sys;

     // Позиция системы в хранилище
     Index:=FindSystem(Path, 0);

     // Найдено?
     If Index<0 then
     begin

          // Сообщим о проблеме
          Result:=-1;
          Exit;
     end;

     // Вносим стартовую точку системы
     TSystem(data[Index]).Core.Construct:=Construct;
end;
//////////////////////////////////////////////////////////////////////////

// Добавляет в описание системы стартовую точку
// Module - Имя модуля
// Sys - Имя системы
// StartPoint - имя стартовой точки
// 0 - операция прошла успешно
// -1 - Система не обнаружена
function TStorage.Add_StartPoint_In_Declaration(Module, Sys, StartPoint: UnicodeString): Integer;
var
   Index: Integer;
   Path: UnicodeString;
begin

     // Инициализация
     Result:=0;

     // Полный путь
     Path:='1'+Param.Divider+'0'+Param.Divider+Module+Param.Divider+Sys;

     // Позиция системы в хранилище
     Index:=FindSystem(Path, 0);

     // Найдено?
     If Index<0 then
     begin

          // Сообщим о проблеме
          Result:=-1;
          Exit;
     end;

     // Вносим стартовую точку системы
     TSystem(data[Index]).Core.Start_Point:=StartPoint;
end;
//////////////////////////////////////////////////////////////////////////

// Подробный отчет по дереву хранилища
procedure TStorage.FullReportRoot(var Rep: TStringList);
var
   i, count, s, nature: Integer;
   Node, Last, space: UnicodeString;
begin

     // Количество систем в хранилище
     count:=data.Count-1;

     // Отображаем данные по узлам
     for i:=0 to count do
     begin

          // Имя узла
          Node:=TSystem(data[i]).Core.Name;

          // Последний элемент в системе
          Last:=GetLastNode(Node);

          // Забиваем путь
          s:=UTF8Length(Node)-UTF8Length(Last);
          Node:=Spacer(s, Last);
          Space:=GenerateSpace(UTF8Length(Node));

          // Закрытая система?
          If (TSystem(data[i]).Core.Nature=2) or (TSystem(data[i]).Core.Nature=2) Then
          begin

               Node:=Node+'<!>';
          end;

          // Отобразим результат
          rep.Add(Node);

          // Тип
          Last:='   [Тип: '+TSystem(data[i]).Core.AType+']';
          rep.Add(Space+Last);

          // Значение
          Last:='   [Значение: '+TSystem(data[i]).Core.Value+']';
          rep.Add(Space+Last);

          // Природа системы
          Nature:=TSystem(data[i]).Core.Nature;
          case Nature of

               0: Last:='Простая система';
               1: Last:='Сложная система';
               2: Last:='Закрытая подсистема';
               3: Last:='Система-функция';
               4: Last:='Закрытая система-функция';
          end;
          Last:='   [Природа: '+Last+']';
          rep.Add(Space+Last);

          // Конструктор системы
          Last:='   [Конструктор: '+TSystem(data[i]).Core.Construct+']';
          rep.Add(Space+Last);

          // Точка входа
          Last:='   [Точка входа: '+TSystem(data[i]).Core.Start_Point+']';
          rep.Add(Space+Last);
     end;
end;
//////////////////////////////////////////////////////////////////////////

// Генерируем пробелы
// Count - количество пробелов
function TStorage.GenerateSpace(Count: Integer): UnicodeString;
var
   i: Integer;
Begin

     // Инициализации
     Result:='';
     for i:=1 to Count do
     begin

          Result:=Result+' ';
     end;
end;
//////////////////////////////////////////////////////////////////////////

// Добавление внешней системы в указанный модуль
// NameModule - имя внешнего модуля куда требуется добавить внешнюю систему
// NameSystem - имя добавляемой системы
// -1 Система с таким именем уже существует
// -2 Не найден внешний модуль
// Положительное число - идентификатор системы в хранилище
function TStorage.AddExternalSystem(NameModule, NameSystem: UnicodeString): Integer;
begin

     // Инициализация
     Result:=-1;

     // Добавим модуль
     AddModule(NameModule);

     // Пытаемся добавить в него внешнюю систему
     Result:=AddSystem(NameSystem, '0'+Param.Divider+NameModule, '', NameSystem, 1);
end;
//////////////////////////////////////////////////////////////////////////

// Добавляет подсистему с заданными свойствами
// Name - добавляемой системы
// SuperSystem - путь до родителя
// Value - значение системы
// AType - тип системы
// Nature - природа системы
// Положительное число - индекс системы в хранилище
// -1 - Система уже существует
// -2 - Родитель не найден
// -3 - Имя системы не задано
function TStorage.AddSystem(Name, SuperSystem, Value, AType: UnicodeString; Nature: Integer): Integer;
var
  Index: Integer;
begin

     // Инициализация
     Result:=-2;

     // Найдем индекс родителя
     if SuperSystem='' then
     begin

          // Адрес по умолчанию
          SuperSystem:='1';
     end
     Else
     begin

          // Глобальное имя
          SuperSystem:='1'+Param.Divider+SuperSystem;
     end;
     Index:=FindSystem(SuperSystem, 0);

     // Добавляем систему
     Result:=AddSystem(Name, Value, AType, Nature, Index);
end;
//////////////////////////////////////////////////////////////////////////

// Добавление модуля для хранения описаний внешних систем
// NameModule - имя добавляемого внешнего модуля (является контейнером описаний внешних систем)
procedure TStorage.AddModule(NameModule: UnicodeString);
Begin

     // Просто добавляем новую систему по адресу 0.
     AddSystem(NameModule, '', NameModule, 1, 1);
end;
//////////////////////////////////////////////////////////////////////////

// Добавление служебных систем для исполнения программ
// Служебная процедура настраивает собственную вселенную для хранения систем из внешних модулей
// Выполнена отдельно, для возможности использования хранилища с иными целями
// Создает в хранилище 1-d - декларация описаний систем из исходников
//                     1-e - фактическое расположение текущей системы
procedure TStorage.AddServiceSystem();
var
  count: Integer;
begin

     // Общее число систем в хранилище
     count:=data.Count;

     // Анализ
     If (count<>1) then Exit;

     // Создаем узлы
     data.Add(TSystem.Create);
     data.Add(TSystem.Create);

     // Имя узла
     TSystem(data[count]).Core.Name:='1'+param.Divider+'d';
     TSystem(data[count+1]).Core.Name:='1'+param.Divider+'e';

end;
//////////////////////////////////////////////////////////////////////////

// Общее количество блоков кода в системе
function TStorage.GetCountBlock(): Integer;
begin

     // Обертка
     Result:=Length(Progs);
end;
//////////////////////////////////////////////////////////////////////////

// Изменение типа системы
// Sys - ключ системы
// NewType - новый тип системы
// -1 - система не найдена
// 0 - операция прошла успешно
function TStorage.SetTypeSystem(Sys, NewType: String): Integer;
var
   Index: Integer;
begin

     // Инциализация
     Result:=-1;
     Sys:='1'+Param.Divider+Sys;

     // Найдем систему
     Index:=FindSystem(Sys, 0);

     // Система существует?
     If Index<0 then Exit;

     // Пишем результат
     TSystem(data[Index]).Core.AType:=NewType;

     // Операция прошла успешно
     Result:=0;
end;
//////////////////////////////////////////////////////////////////////////

// Изменение текстового значения функции
// Sys - ключ системы
// NewValue - новое текстовое значение системы
// -1 - система не найдена
// 0 - операция прошла успешно
function TStorage.SetValue(Sys, NewValue: String): Integer;
var
   Index: Integer;
begin

     // Инциализация
     Result:=-1;
     Sys:='1'+Param.Divider+Sys;

     // Найдем систему
     Index:=FindSystem(Sys, 0);

     // Система существует?
     If Index<0 then Exit;

     // Пишем результат
     TSystem(data[Index]).Core.Value:=NewValue;

     // Операция прошла успешно
     Result:=0;
end;
//////////////////////////////////////////////////////////////////////////

// Количество скрытых подсистем (включая вложенные!!!!)
// Sys - ключ системы
// -1 - система не найдена
// 0 - система не имеет подсистем
// другое число - количество подсистем
function TStorage.GetCountHideSystem(Sys: String): Integer;
var
  Index, SubIndex, Nature: Integer;
begin

     // Инцииализация
     Result:=-1;
     Sys:='1'+Param.Divider+Sys;

     // Найдем систему
     Index:=FindSystem(Sys, 0);

     // Система существует?
     If Index<0 then Exit;

     // Индекс предполагаемой подсистемы
     SubIndex:=Index+1;

     // Граница хранилища?
     Result:=0;
     If SubIndex=data.Count Then
     begin

          // Это последняя система в хранилище и у нее нет подсистем
          Exit;
     end;

     // Сканирование систем
     While Pos(TSystem(data[Index]).Core.Name+Param.Divider, TSystem(data[SubIndex]).Core.Name)=1 do
     begin

          // Тип подсистемы
          Nature:=TSystem(data[SubIndex]).Core.Nature;

          // Скрытая подсистема?
          if ((Nature=2) or (Nature=4)) then Inc(Result);

          // Следующий индекс
          Inc(SubIndex);

          // Проверка выхода за нижнюю границу хранилище
          If SubIndex=data.Count Then Exit;
     end;
end;
//////////////////////////////////////////////////////////////////////////

// Проверка существования подситем (включая скрытые). Подсчет ведется и по вложенынм подсистемам!!!
// Sys - ключ системы
// -1 - система не найдена
// 0 - система не имеет подсистем
// другое число - количество подсистем
function TStorage.CheckSubSystem(Sys: String): Integer;
var
  Index, SubIndex: Integer;
begin

     // Инцииализация
     Result:=-1;
     Sys:='1'+Param.Divider+Sys;

     // Найдем систему
     Index:=FindSystem(Sys, 0);

     // Система существует?
     If Index<0 then Exit;

     // Индекс предполагаемой подсистемы
     SubIndex:=Index+1;

     // Граница хранилища?
     Result:=0;
     If SubIndex=data.Count Then
     begin

          // Это последняя система в хранилище и у нее нет подсистем
          Exit;
     end;

     // Сканирование систем
     While Pos(TSystem(data[Index]).Core.Name+Param.Divider, TSystem(data[SubIndex]).Core.Name)=1 do
     begin

          // Количество подсистем
          Inc(Result);

          // Следующий индекс
          Inc(SubIndex);

          // Проверка выхода за нижнюю границу хранилище
          If SubIndex=data.Count Then Exit;
     end;
end;
//////////////////////////////////////////////////////////////////////////

// Чтение ссылки на функцию
// Sys - ключ системы
// Rez - результат работы
// False - система не найдена
function TStorage.GetFunckLink(Sys: UnicodeString; var Rez: Integer): Boolean;
var
   Index: Integer;
begin

     // Инциализация
     Result:=False;
     Sys:='1'+Param.Divider+Sys;
     Rez:=-1;

     // Найдем систему
     Index:=FindSystem(Sys, 0);

     // Система существует?
     If Index<0 then Exit;

     // Пишем результат
     Rez:=TSystem(data[Index]).Core.FuncLink;

     // Операция прошла успешно
     Result:=True;
end;
//////////////////////////////////////////////////////////////////////////

// Чтение типа системы
// Sys - ключ системы
// Rez - результат работы
// False - система не найдена
function TStorage.GetTypeSystem(Sys: UnicodeString; var Rez: String): Boolean;
var
   Index: Integer;
begin

     // Инциализация
     Result:=False;
     Sys:='1'+Param.Divider+Sys;
     Rez:='';

     // Найдем систему
     Index:=FindSystem(Sys, 0);

     // Система существует?
     If Index<0 then Exit;

     // Пишем результат
     Rez:=TSystem(data[Index]).Core.AType;

     // Операция прошла успешно
     Result:=True;
end;
//////////////////////////////////////////////////////////////////////////

// Чтение внутреннего представления системы
// Sys - ключ системы
// Rez - результат работы
// False - система не найдена
function TStorage.GetNatureSystem(Sys: UnicodeString; var Rez: Integer): Boolean;
var
   Index: Integer;
begin

     // Инциализация
     Result:=False;
     Sys:='1'+Param.Divider+Sys;
     Rez:=-1;

     // Найдем систему
     Index:=FindSystem(Sys, 0);

     // Система существует?
     If Index<0 then Exit;

     // Пишем результат
     Rez:=TSystem(data[Index]).Core.Nature;

     // Операция прошла успешно
     Result:=True;
end;
//////////////////////////////////////////////////////////////////////////

// Чтение текстового значения системы
// Sys - ключ системы
// Rez - результат работы
// False - система не найдена
function TStorage.GetValueSystem(Sys: UnicodeString; var Rez: String): Boolean;
var
   Index: Integer;
begin

     // Инциализация
     Result:=False;
     Sys:='1'+Param.Divider+Sys;
     Rez:='';

     // Найдем систему
     Index:=FindSystem(Sys, 0);

     // Система существует?
     If Index<0 then Exit;

     // Пишем результат
     Rez:=TSystem(data[Index]).Core.Value;

     // Операция прошла успешно
     Result:=True;
end;
//////////////////////////////////////////////////////////////////////////

// Проверка существования системы по указанному ключу
// Sys - ключ системы
// True - система существует
function TStorage.CheckSystem(Sys: String): Boolean;
var
   Index: Integer;
begin

     // Инициализация
     Result:=False;
     Sys:='1'+Param.Divider+Sys;

     // Поиск
     Index:=FindSystem(Sys, 0);

     // Проверка существования
     If Index>-1 then Result:=True;
end;
//////////////////////////////////////////////////////////////////////////

// Инициализация системы
// 0 - операция прошла успешно
// -1 - система не найдена
function TStorage.InitSystem(Sys: String): Integer;
var
  Index: Integer;
begin

     // Инициализация
     Result:=-1;
     Sys:='1'+Param.Divider+Sys;

     // Найдем систему в хранилище
     Index:=FindSystem(Sys, 0);

     // Найдена?
     If Index<0 Then Exit;

     // Инициализация системы
     InitSystem(Index);

     // Операция прошла успешно
     Result:=0;
end;
//////////////////////////////////////////////////////////////////////////

// Копирует систему на место другой системы
// Source - что копировать
// Dest - куда копировать
// 0 - Операция прошла успешно
// -1 - Source не найден
// -2 - Dest не найден
// -3 - Копирование основной ветви в дочернуюю
function TStorage.CopySystem(Source, Dest: String): Integer;
var
  Index1, Index2: Integer;
begin

     // Инциализация
     Result:=-1;
     Source:='1'+Param.Divider+Source;
     Dest:='1'+Param.Divider+Dest;

     // Индекс источника
     Index1:=FindSystem(Source, 0);

     // Система существует?
     If Index1<0 Then Exit;

     // Индекс приемника
     Index2:=FindSystem(Dest, 0);

     // Система существует?
     If Index2<0 Then
     begin

          // Сообщим об ошибке
          Result:=-2;
          Exit;
     end;

     // Системы вложены?
     If CheckNesting(Source, Dest)=True then
     begin

          // Сообщим о проблеме
          Result:=-3;
          Exit;
     end;

     // Инциализируем приемник
     InitSystem(Index2);

     // Копирование системы
     CopySystem(Index1, Index2);

     // Операция прошла успешно
     Result:=0;
end;
//////////////////////////////////////////////////////////////////////////

// Инициализация системы
// Index - индекс инициализируемой системы в хранилище
procedure TStorage.InitSystem(Index: Integer);
var
  Index2: Integer;
begin

     // Обнуляем все данные системы (за исключением имени)
     TSystem(data[Index]).Core.AType:='';
     TSystem(data[Index]).Core.FuncLink:=-1;
     TSystem(data[Index]).Core.Nature:=0;
     TSystem(data[Index]).Core.Value:='';

     // Подсистемы
     If Index=data.Count-1 Then exit;
     Index2:=Index+1;

     // Каждая подсистема данной системы
     while Pos(TSystem(data[Index]).Core.Name+Param.Divider, TSystem(data[Index2]).Core.Name)=1 do
     begin

          // Нужно удалить данную подсистему
          data.Delete(Index2);

          // Анализ границы
          If Index=data.Count-1 Then Exit;
     end;
end;
//////////////////////////////////////////////////////////////////////////

// Добавляет систему на основе уже существующей
// OldKey - система которая будет копироваться под новым именем
// NewKey - новый ключ для создаваемой системы (последний элемент - новое имя)
// 0 - Операция прошла успешно
// -1 - OldKey не найдена
// -2 - NewKey уже существует
// -3 - Путь до NewKey не найден
// -4 - Самокопирование (когда NewKey это дочерняя ветка OldKey)
function TStorage.AddSystem2(OldKey, NewKey: String): Integer;
var
  index1, Index2: Integer;
  Last: UnicodeString;
begin

     // Инициализация
     Result:=-1;
     OldKey:='1'+Param.Divider+OldKey;
     NewKey:='1'+Param.Divider+NewKey;


     // Найдем адрес системы в хранилище
     Index1:=FindSystem(OldKey, 0);

     // Нашли?
     If Index1<0 then Exit;

     // Проверим существование системы по новому ключу
     Index2:=FindSystem(NewKey, 0);

     // Нашли?
     If Index2>-1 then
     begin

          // Система с таким именем уже существует
          Result:=-2;
          Exit;
     end;

     // Вложенное копирование
     If CheckNesting(OldKey, NewKey)=True Then
     begin

          // Сообщим об ошибке
          Result:=-4;
          Exit;
     end;

     // Разделим ключ на путь и конечное имя
     Last:=DivideKey(NewKey);

     // Проверим существование приемника копируемой системы
     Index2:=FindSystem(NewKey, 0);

     // Нашли?
     If Index2<0 then
     begin

          // Приемник не найден
          Result:=-3;
          Exit;
     end;

     // Создаем систему
     AddSystem(Last, '', '', 0, Index2);

     // Индекс системы
     Index2:=FindSystem(NewKey+Param.Divider+Last, Index2);
     Index1:=FindSystem(OldKey, 0);

     // Копирование системы и ее подсистем
     CopySystem(Index1, Index2);

     // Операция прошла успешно
     Result:=0;
end;
//////////////////////////////////////////////////////////////////////////

// Копирование системы и всех ее подсистем
// Source - индекс копируемой системы
// Dest - индекс системы в которую будет скопирована Source
// Имя системы НЕ ИЗМЕНЯЕТСЯ!!!!
procedure TStorage.CopySystem(Source, Dest: Integer);
var
  SubSource, Index, len: Integer;
  Name, SourceName, CopyName: UnicodeString;
begin

     // Перенос полей
     SourceName:=TSystem(data[Source]).Core.Name;
     CopyName:=TSystem(data[Dest]).Core.Name;
     TSystem(data[Dest]).Core.AType    :=TSystem(data[Source]).Core.AType;
     TSystem(data[Dest]).Core.FuncLink :=TSystem(data[Source]).Core.FuncLink;
     TSystem(data[Dest]).Core.Nature   :=TSystem(data[Source]).Core.Nature;
     TSystem(data[Dest]).Core.Value    :=TSystem(data[Source]).Core.Value;

     // Есть что добавлять?
     SubSource:=Source+1;
     If SubSource>data.Count-1 Then Exit;

     // Добавляем каждую систему
     while UTF8pos(TSystem(data[Source]).Core.Name+Param.Divider, TSystem(data[SubSource]).Core.Name)=1 do
     begin

          // Генерируем новое имя
          Name:=TSystem(data[SubSource]).Core.Name;
          len:=Utf8Length(TSystem(data[Source]).Core.Name);
          Name:=Utf8Copy(Name, len+2, (UTF8Length(Name)-len-1));

          // Вставляем новую систему
          AddSystem(Name, '', '', 0, Dest);

          // Ключ новой системы
          Name:=TSystem(data[Dest]).Core.Name+Param.Divider+Name;

          // Индекс новой системы
          Index:=FindSystem(Name, Dest);

          // Коррекция индексов из-за добавления новой системы
          If Dest<=Source Then
          begin

               // Смещене
               Inc(SubSource);
               Inc(Source);
          end;

          // Копирование
          len:=TSystem(data[SubSource]).Core.Nature;
          SourceName:=TSystem(data[SubSource]).Core.Name;
          CopyName:=TSystem(data[Index]).Core.Name;
          TSystem(data[Index]).Core.AType    :=TSystem(data[SubSource]).Core.AType;
          TSystem(data[Index]).Core.FuncLink :=TSystem(data[SubSource]).Core.FuncLink;
          TSystem(data[Index]).Core.Nature   :=TSystem(data[SubSource]).Core.Nature;
          TSystem(data[Index]).Core.Value    :=TSystem(data[SubSource]).Core.Value;

          // Следующий шаг
          Inc(SubSource);
     end;
end;
//////////////////////////////////////////////////////////////////////////

// Разделение ключа на путь и имя
// Key - ключ (туда же будет возвращен путь до системы)
// Возвращает имя системы
function TStorage.DivideKey(var Key: String): UnicodeString;
begin

     // Имя системы
     Result:=GetLastNode(Key);

     // Получим сам путь
     Key:=UTF8Copy(Key, 1, UTF8Length(Key)-1-Utf8Length(Result));
end;
//////////////////////////////////////////////////////////////////////////

// Перемещение/переменование системы
// OldKey - текущий путь до системы
// NewKey - новый путь до системы
// 0 - Операция прошла успешно
// -1 - Система по OldKey не найдена
// -2 - NewKey не существует
// -3 - Нельзя копировать родителя в потомка
// -4 - Система с таким именем уже существует в NewKey
function TStorage.RemoveSystem(OldKey, NewKey: String): Integer;
var
  Index, Index2: Integer;
  Name: UnicodeString;
begin

     // Инциализация
     Result:=-3;
     OldKey:='1'+Param.Divider+OldKey;
     NewKey:='1'+Param.Divider+NewKey;

     // Найдем переносимую систему
     Index:=FindSystem(OldKey, 0);

     // Система указана корректно?
     If Index<0 then
     begin

          // Сообщим об ошибке
          Result:=-1;
          Exit;
     end;

     // Проверим вложенность систем
     If CheckNesting(OldKey, NewKey)=True Then Exit;

     // Проверим существование приемника
     Index2:=FindSystem(NewKey, 0);

     If Index2<0 then
     begin

          // Сообщим об ошибке
          Result:=-2;
          Exit;
     end;

     // Мы пытаемся переименовать систему?
     Name:='';
     If CheckRename(OldKey, NewKey)=True then
     begin

          // Получим имя для переименования
          Name:=GetLastNode(NewKey);

          // Ключ для передачи
          NewKey:=Utf8Copy(NewKey, 1, UTF8Length(NewKey)-1-UTF8Length(Name));
     end;

     // Проверим существование приемника
     Index2:=FindSystem(NewKey, 0);

     If Index2<0 then
     begin

          // Сообщим об ошибке
          Result:=-2;
          Exit;
     end;

     // Перенос системы
     Result:=Move(Index, Index2, Name);
end;
//////////////////////////////////////////////////////////////////////////

// Проверка, является ли новый путь переименованием системы
// OldKey - Текущий ключ системы
// NewKey - Новый ключ системы
// TRUE - новый путь переименование - системы
function TStorage.CheckRename(OldKey, NewKey: String): Boolean;
var
  OldLast, NewLast, OldRoot, NewRoot: UnicodeString;
  len1, len2: Integer;
begin

     // Инициализация
     Result:=False;

     // Получим последние элементы
     OldLast:=GetLastNode(OldKey);
     NewLast:=GetLastNode(NewKey);

     // Получим родителя старого ключа
     len1:=UTF8Length(OldLast);
     len1:=UTF8Length(OldKey)-len1;
     OldRoot:=Utf8Copy(OldKey, 1, len1);

     // Получим родителя нового ключа
     len2:=UTF8Length(NewLast);
     len2:=UTF8Length(NewKey)-len2;
     NewRoot:=Utf8Copy(NewKey, 1, len2);

     // Анализ
     if OldRoot=NewRoot Then Result:=True;
end;
//////////////////////////////////////////////////////////////////////////

// Перемещение системы и всех ее подсистем
// Index1 - что переносим
// Index2 - куда переносим
// 0 - Операция прошла успешно
// -4 - система с таким именем уже существует в приемнике
function TStorage.Move(Index1, Index2: Integer; ANewName: String): Integer;
var
  NewName, Last, Name, str1: UnicodeString;
  Index: Integer;
begin

     // Инцииализация
     Result:=-4;

     // Генерируем новое имя
     Name:=TSystem(data[Index1]).Core.Name;

     // Имя передали?
     If ANewName<>'' Then
     begin

          // Просто используем переданное имя
          Last:=ANewName;
     end
     Else
     begin

          // Получим имя из ключа
          Last:=GetLastNode(Name);
     end;

     NewName:=TSystem(data[Index2]).Core.Name+Param.Divider+Last;

     // Место свободно?
     If FindSystem(NewName, Index2)>-1 then Exit;

     // Индекс для перемещения
     Index:=InsertedIndex(NewName, Index2);

     // Корректор индекса
     If Index>=data.Count Then Index:=data.Count-1;

     // Восстановим первоначальынй индекс
     If (Index>Index2) and (Index1<Index2) Then Dec(Index2);
     If (Index>Index2) and (Index1<Index2) Then Dec(Index);

     // Перенос
     data.Move(Index1, Index);

     // Новое имя элемента
     TSystem(data[Index]).Core.Name:=NewName;

     // Шаг приращения
     If Index1>=Index Then Inc(Index1);
     If Index1=data.Count then Exit;

     // Апргрейд имени для подсистем
     Str1:=TSystem(data[Index2]).Core.Name+Param.Divider+Last;

     // Цикл переноса подсистем
     while Utf8Pos(Name+Param.Divider, TSystem(data[Index1]).Core.Name)=1 do
     begin

          // Генерируем новое имя
          NewName:=NewNamer(Name, TSystem(data[Index1]).Core.Name, str1);

          // Новый индекс
          Index:=InsertedIndex(NewName, Index2);

          // Корректор индекса
          If Index>=data.Count Then Index:=data.Count-1;

          // Восстановим первоначальынй индекс
          If (Index>Index2) and (Index1<Index2) Then Dec(Index2);
          If (Index>Index2) and (Index1<Index2) Then Dec(Index);

          // Перенос
          data.Move(Index1, Index);

          // Новое имя
          TSystem(data[Index]).Core.Name:=NewName;

          // Шаг приращения
          If Index1>=Index Then Inc(Index1);
          If Index1=data.Count then Exit;
     end;

     // Операция прошла успешно
     Result:=0;
end;
//////////////////////////////////////////////////////////////////////////

// Генератор путей для переноса подсистем
// OldKey - источник, надсистема
// Node - ключ переносимой подсистемы
// NewKey - адрес куда требуется перенести подсистему
// Системы уже должны содержать корневой элемент '1'
function TStorage.NewNamer(OldKey, Node, NewKey: String): UnicodeString;
var
  len: Integer;
begin

     // Вычитаем старый путь из переносимой системы
     len:=UTF8Length(OldKey)+1;
     Node:=UTF8copy(Node, len+1, UTF8Length(Node)-len);
     Node:=NewKey+Param.Divider+Node;

     // Результат
     Result:=Node;
end;
//////////////////////////////////////////////////////////////////////////

// Определяет вложенность ключей
// Осуществляет проверку того, что NewKey является частью OldKey, т.е. что NewKey более высокого уровня
// True - да, False - нет
function TStorage.CheckNesting(OldKey, NewKey: String): Boolean;
begin

     // Инициализация
     Result:=False;

     If UTF8Pos(NewKey+Param.Divider, OldKey)=1 Then Result:=True;
end;
//////////////////////////////////////////////////////////////////////////

// Удаление подсистемы
// Name - имя удаляемой подсистемы
// SuperSystem - система в которой нужно провести удаление
// 0 - операция прошла успешно
// -1 - Удаляемая система не найдена
function TStorage.EraseSystem(Key: String): Integer;
var
  Index: Integer;
begin

     // Инциализация
     Result:=-1;

     // Индекс надсистемы
     Key:='1'+Param.Divider+Key;
     Index:=FindSystem(Key, 0);

     // Найдена?
     If Index<0 then Exit;

     // Удалим данную систему
     data.Delete(Index);
     if Index>data.Count-1 Then
     begin

          // Больше нечего удалять
          Result:=0;
          Exit;
     end;

     // Удаление подсистем
     while UTF8Pos(key+Param.Divider, TSystem(data[Index]).Core.Name)=1 do
     begin


          data.Delete(Index);
          if Index>data.Count-1 Then
          begin

               // Больше нечего удалять
               Result:=0;
               Exit;
          end;
     end;

     // Операция завершена
     Result:=0;
end;
//////////////////////////////////////////////////////////////////////////

// Полное дерево хранилища
// rep - список для отчета
// Предыдущая информация сохраняется
// detail - Нужен ли подробный вывод - TRUE - да
procedure TStorage.ReportRoot(var rep: TStringList; detail: Boolean);
var
   i, count, s: Integer;
   Node, Last: UnicodeString;
begin

     // Количество систем в хранилище
     count:=data.Count-1;

     // Отображаем данные по узлам
     for i:=0 to count do
     begin

          // Имя узла
          Node:=TSystem(data[i]).Core.Name;

          // Последний элемент в систе
          Last:=GetLastNode(Node);

          // Забиваем путь
          s:=UTF8Length(Node)-UTF8Length(Last);
          Node:=Spacer(s, Last);

          // Закрытая система?
          If (TSystem(data[i]).Core.Nature=2) or (TSystem(data[i]).Core.Nature=2) Then
          begin

               Node:=Node+'<!>';
          end;

          // Отобразим результат
          rep.Add(Node);

          If detail=True then
          begin

              // Тип
              Node:=Spacer(Length(Node), Node);
              Last:=Node+'<Тип: '+TSystem(data[i]).Core.AType+'>';
              rep.Add(Last);

              // Значение
              Last:=Node+'<Значение: '+TSystem(data[i]).Core.Value+'>';
              rep.Add(Last);
          end;
     end;
end;
//////////////////////////////////////////////////////////////////////////

// Забивает проблеами указанное количество символов
// Count - количество символов, которые нужно добавить перед
function TStorage.Spacer(Count: Integer; Line: String): UnicodeString;
var
  i: Integer;
begin

     // Забиваем символы
     Result:='';
     for i:=1 to Count do
     begin

          Result:=Result+' ';
     end;

     // Результат
     Result:=Result+Line;
end;
//////////////////////////////////////////////////////////////////////////

// Извлечние последнего узла в пути
function TStorage.GetLastNode(Line: String): UnicodeString;
var
  i, count: Integer;
begin

     // Инициализация
     Result:=Line;

     // Количество элементов в строке
     count:=UTF8Length(Line);

     // Сканируем в обратном порядке
     for i:=count downto 1 do
     begin

          // Сверим с разделителем узлов
          if UTF8Copy(Line, i, 1)=Param.Divider then
          begin

               // Остаток
               Result:=UTF8copy(Line, i+1, count-i);
               Exit;
          end;
     end;
end;
/////////////////////////////////////////////////////////////////////////

// Добавляем простую систему
// Name - добавляемой системы
// Value - значение системы
// SuperSystem - путь до родителя
// Part - в какую часть надсистемы добавлять подсистему
//        False - открытая часть
//        True - закрытая часть
// Положительное число - индекс системы в хранилище
// -1 - Система уже существует
// -2 - Родитель не найден
// -3 - Имя системы не задано
function TStorage.AddSystem(Name, Value, SuperSystem: UnicodeString; Part: Boolean): Integer;
var
  Index: Integer;
begin

     // Инициализация
     Result:=-2;

     // Найдем индекс родителя
     if SuperSystem='' then
     begin

          // Адрес по умолчанию
          SuperSystem:='1';
     end
     Else
     begin

          // Глобальное имя
          SuperSystem:='1'+Param.Divider+SuperSystem;
     end;
     Index:=FindSystem(SuperSystem, 0);

     // Добавляем систему
     If Part=True then
     begin

          // Пометка, что нужна закрытая часть системы
          Result:=AddSystem(Name, Value, '', 2, Index);
     end
     Else
     begin

          // Обычная система
          Result:=AddSystem(Name, Value, '', 1, Index);
     end;
end;
//////////////////////////////////////////////////////////////////////////

// Все пути
// Собирает и выводит все пути в хранилище
// rep - список для отчета
// Предыдущая информация не удаляется
procedure TStorage.Report_Keys(var rep: TStringList);
var
  i, count: Integer;
begin

     // Количество элементов в системе
     count:=data.Count;

     // Просматриваем хранилище
     for i:=0 to count-1 do
     begin

          // Добавляем пути
          rep.Add(TSystem(data[i]).Core.Name);
     end;
end;
//////////////////////////////////////////////////////////////////////////

// Добавление корневой системы
// Корневая система создается в нулевом индексе
// Если система уже есть, она не будет создаваться
procedure TStorage.AddRoot();
var
  count: Integer;
begin

     // Проверим существует ли корневой узел
     count:=data.Count;

     // Уже есть?
     If count>0 then Exit;

     // Создаем узел
     data.Add(TSystem.Create);

     // Имя узла
     TSystem(data[0]).Core.Name:='1';

     // Создаем узел
     data.Add(TSystem.Create);

     // Имя узла
     TSystem(data[1]).Core.Name:='1.0';                          // Описание систем
     TSystem(data[1]).Core.Nature:=2;                            // Закрытая система

     // Создаем узел
     data.Add(TSystem.Create);

     // Имя узла
     TSystem(data[2]).Core.Name:='1.1';                          // Место исполнение вычислительного процесса
end;
//////////////////////////////////////////////////////////////////////////

// Добавляем систему
// Name - добавляемой системы
// SuperSystem - путь до родителя
// Part - в какую часть надсистемы добавлять подсистему
//        False - открытая часть
//        True - закрытая часть
// Положительное число - индекс системы в хранилище
// -1 - Система уже существует
// -2 - Родитель не найден
// -3 - Имя системы не задано
function TStorage.AddSystem(Name, SuperSystem: UnicodeString; Part: Boolean): Integer;
var
  Index: Integer;
begin

     // Инициализация
     Result:=-2;

     // Найдем индекс родителя
     if SuperSystem='' then
     begin

          // Адрес по умолчанию
          SuperSystem:='1';
     end
     Else
     begin

          // Глобальное имя
          SuperSystem:='1'+Param.Divider+SuperSystem;
     end;
     Index:=FindSystem(SuperSystem, 0);

     // Добавляем систему
     if Part=True then
     begin

          // Закрытая система
          Result:=AddSystem(Name, '', '', 2, Index);
     end
     Else
     begin

          // Открытая система
          Result:=AddSystem(Name, '', '', 1, Index);
     end;
end;
//////////////////////////////////////////////////////////////////////////

// Добавление системы
// Name - добавляемой системы
// Index - индекс родителя в хранилище
// Положительное число - индекс системы в хранилище
// -1 - Система уже существует
// -2 - Родитель не найден
// -3 - Имя системы не задано
function TStorage.AddSystem(Name, Value, AType: UnicodeString; Nature, Index: Integer): Integer;
var
    FullKey: UnicodeString;
begin

    // Инициализация
    Result:=-2;

    // Родитель указан корректно?
    If Index<0 then Exit;

    // Имя системы указано?
    If Name='' then
    begin

         // Сообщим об ошибке
         Result:=-3;
         Exit;
    end;

    // Сцепленный ключ
    FullKey:=TSystem(data[Index]).Core.Name+Param.Divider+Name;

    // Найдем индекс для вставки нового элемента
    Index:=InsertedIndex(FullKey, Index);

    // Система уже существует?
    If index<0 then
    begin

         // Сообщим о проблеме
         Result:=-1;
         Exit;
    end;

    // Вставляем новую систему
    data.Insert(Index, TSystem.Create);

    // Имя новой системе
    TSystem(data[Index]).Core.Name:=FullKey;

    // Значение новой системе
    TSystem(data[Index]).Core.Value:=Value;

    // Природа системы
    TSystem(data[Index]).Core.Nature:=Nature;

    // Тип системы
    TSystem(data[Index]).Core.AType:=AType;

    // Операция прошла успешно
    Result:=Index;
end;
//////////////////////////////////////////////////////////////////////////

// Поиск системы в хранилище
// Name -Имя/путь до системы
// Parent - Родитель
// Возвращает идентификатор системы в хранилище
// -1 Если система не найдена
function TStorage.FindSystem(Name: UnicodeString; StartPos: Integer): Integer;
var
  start,finish, ex: Integer;
  s: UnicodeString;
begin

     // Инициализация
     Result:=-1;
     finish:=data.Count-1;
     start:=StartPos;

     // Частный случай
     If Name='' Then Exit;

     // Сканируем хранилище половинным делением интервала
     while finish>=start do
     begin

          // Новая точка для анализа
          ex:=start+((finish-start) div 2);

          // Совпадение
          s:=TSystem(data[ex]).Core.Name;
          If s=Name Then
          begin

               // Передаем индекс
               Result:=ex;
               Exit;
          end;

          // Корректировка интервала поиска
          If s>Name Then
          begin

               // Подтягиваем нижнюю границу
               finish:=ex-1;
          end
          Else
          begin

               // Подтягиваем верхнуюю границу диапазона
               start:=ex+1;
          end;
     end;
end;
//////////////////////////////////////////////////////////////////////////

// Индекс для добавления системы в хранилище
// Name - Полное имя для добавления в хранилище
// -1 - Система с таким именем уже существует
function TStorage.InsertedIndex(Name: UnicodeString; Parent: Integer): Integer;
var
  ex, start, finish: Integer;
begin

     // Инициализация
     result:=-1;
     start:=Parent;
     finish:=data.Count-1;

     // Частный случай
     if finish<0 then
     begin

          // Сообщим о первом индексе
          Result:=0;
          Exit;
     end;

     // Сканируем имена половинным делением интервала
     while start<=finish do
     begin

          // Получим интервал
          ex:=start+((finish-start) div 2);

          // Совпало?
          if Name=TSystem(data[ex]).Core.Name then
          begin

               // Сообщим о существовании системы
               Exit;
          end;

          // Нижняя граница
          if Name>TSystem(data[ex]).Core.Name then
          begin

               // Подтягиваем нижнюю границу
               start:=ex+1;
          end
          Else
          begin

               // Подтягиваем верхнюю границу
               finish:=ex-1;
          end;
     end;

     // Передаем индекс
     Result:=start;
end;
//////////////////////////////////////////////////////////////////////////

// Конструктор
constructor TStorage.Create;
begin

     // Предок
     Inherited Create;

     // Хранилище
     data:=TObjectList.Create();

     // Параметры
     InitParams();

     // Суперсистема
     AddRoot();
end;
//////////////////////////////////////////////////////////////////////////

// Деструктор
destructor TStorage.Destroy;
begin

     // Инициализация
     Init();

     // Хранилище
     data.Free;

     // Предок
     Inherited Destroy;
end;
//////////////////////////////////////////////////////////////////////////

// Инициализатор
procedure TStorage.Init();
begin

     // Количество доступных систем
     data.OwnsObjects:=true;

     data.Clear;

     // Параметры
     InitParams();

     // Корневой узел
     AddRoot();
end;
//////////////////////////////////////////////////////////////////////////

// Инициализация параметров хранения систем
procedure TStorage.InitParams();
begin

     // Установки по умолчанию
     Param.Divider:='.';
     Param.Hide_marker:='?';
end;
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

// Деструктор
destructor TSystem.Destroy();
begin

     // Инициализация
     Init();

     // Предок
     Inherited Destroy;
end;
//////////////////////////////////////////////////////////////////////////

// Инициализатор
procedure TSystem.Init();
begin

     // Ядро
     InitCore();

     // Список внешних модулей
     SetLength(ExtModules, 0);
end;
//////////////////////////////////////////////////////////////////////////

// Инициализация ядра системы
procedure TSystem.InitCore();
begin

     // Обнулей полей
     Core.AType:='';
     Core.Name:='';
     Core.Nature:=0;
     Core.Value:='';
     Core.Start_Point:='';
     Core.Construct:='';
end;
//////////////////////////////////////////////////////////////////////////

// Конструктор
constructor TSystem.Create;
begin

     // Предок
     Inherited Create;
end;
//////////////////////////////////////////////////////////////////////////

end.

// Иерархия систем
// 1 - Метавселенная
// 1.0 - Описание систем
// 1.1 - Среда исполнения систем

