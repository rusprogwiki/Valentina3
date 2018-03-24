unit core;

{$mode objfpc}
    {$H+}
    {$codepage UTF8}

// Ядро интерепретатора языка программирования Валентина

interface

uses
  {$IFNDEF WINDOWS}
    cwstring,
    {$ENDIF}lazutf8, lazutf8classes, Classes, SysUtils, parser, storage;


// Константы для хранения внешнезависимой информации
Const
  DialectPath='dct/main.dct';


// Парсер
type
    TPar=record

       Parser: TParse;                         // Парсер
       NameLang: UnicodeString;                // Имя файла с описанием языковых конструкций
    end;
////////////////////////////////////////////////////////////////////////////////////////

// Формат ошибки исходного текста программы
type
    TErr=record

       SourceId: Integer;                      // Исходник с ошибкой
       Line: Integer;                          // Номер строки с ошибкой
       Id: Integer;                            // Код ошибки
       Message: UnicodeString;                 // Сообщение об ошибке
       Param: UnicodeString;                   // Дополнительная служебная информация о проблеме
    end;
////////////////////////////////////////////////////////////////////////////////////////

// Структура промежуточного разбора секции общих параметров системы
type
    TTempOptionSystem=record
       Type_system: UnicodeString;                                 // Информация о типе системы
       External_Systems: TStrMass;                                 // Ссылки на внешние модули (TStrMass объявлен в Parser.pas)
       Flag_external_systems: Boolean;                             // Признак рассмотрения ссылок на внешние модули
       Start_Point: UnicodeString;                                 // Функция - точка входа в программу
       Flag_start_point: Boolean;                                  // Признак рассмотрения функции, точки входа в программу
       Constructor_system: UnicodeString;                          // Функция - конструктор системы
       Flag_constructor_system: Boolean;                           // Признак рассмотрения флага конструкции
       Flag_i18: Boolean;                                          // Признак рассмотрения таблицы интернационализации
       IdSource: Integer;                                          // Идентификатор исходника в хранилище
       Position: Integer;                                          // Указатель на текущую строку в исходнике
    end;
////////////////////////////////////////////////////////////////////////////////////////

// Описание исходных текстов
type
    TSource=record

       Source:   TStringList;                  // Исходный код
       Name:     UnicodeString;                // Обозначение в программе
       NameFile: UnicodeString;                // Имя файла, в котором содержится исходный код программы
       NameLang: UnicodeString;                // Имя файла с описанием языковых конструкций
    end;
////////////////////////////////////////////////////////////////////////////////////////

// Описание виртуальной машины
type
    TVm = class
          private

                ForbbidenSymbols2: UnicodeString;                                       // Список запрещенных к использованию символов
                ForbbidenSymbols: UnicodeString;                                        // Список запрещенных к использованию символов

                Storage: TStorage;                                                      // Хранилище иерархии
                Parser: Array of TPar;                                                  // Парсер
                Source: Array of TSource;                                               // Исходные коды модулей с системами
                Errors: Array of TErr;                                                  // Список сообщений об ошибках

                // Служебные методы
                function SkipEmptyLines(Line: TStringList; Start: Integer): Integer;    // Указывает на первую не пустую строку в тексте
                procedure InitStorage();                                                // Инцииализация хранилища систем
                procedure InitSource();                                                 // Инициализация хранилища исходных текстов
                procedure InitParsers();                                                // Инциализация парсеров
                procedure ClearErrors();                                                // Очистка списка сообщений об ошибках
                function AddError(): Integer;                                           // Добавить структуру ошибки в конец
                function GetError(Index: Integer): UnicodeString;                       // Упаковывает ошибку в текстовую строку
                procedure GenerateError(Id, Line, SourceId: Integer;
                                        Mess, Param: UnicodeString);                    // Генерация ошибки
                procedure InitOption(var Option: TTempOptionSystem);                    // Подготовка опций первой секции
                function DeclarationSystem(var Option: TTempOptionSystem): Integer;     // Описание системы из внешнего модуля

                function ManageFirstSection(var Option: TTempOptionSystem): Integer;    // Чтение параметров первой сеции описания
                procedure DecompositeLine(Id, LineCode: Integer; Line: UnicodeString;
                                          var Mass: TStrMass);                          // Разложение строки на элементы массива
                function CheckCyrillic(Line: UnicodeString): Integer;                   // Проверяет состоит ли данная строка сплошь из символов кириллицы
                function Parse_i18(var Option: TTempOptionSystem): Integer;             // Разбор и проверка секции интернационализации
                function CheckFirstOption(Option: TTempOptionSystem): Integer;          // Проверка все ли компоненты первой секции были введены

                // Проверка имени системы
                function CheckFirstNumber(Line: UnicodeString): Integer;                // Проверка, начинается ли строка с цифры
                function CheckForbbidenSymb(Line: UnicodeString): Integer;              // Поиск в имени запрещенных символов (точку можно)
                function CheckForbbidenSymb2(Line: UnicodeString): Integer;             // Поиск в имени запрещенных символов (точку нельзя)
                function FormalCheckName(Line: UnicodeString): Integer;                 // Формальная проверка имени системы (точку можно)
                function FormalCheckName2(Line: UnicodeString): Integer;                // Формальная проверка имени системы (точку нельзя)

                // Методы по изменению внутренного состояния виртуальной машины
                function AddNewDeclaraionSystem(NameSystem, Module:
                                                UnicodeString): Integer;                // Добавление новой системы в раздел ОПИСАНИЙ СИСТЕМ
                function InsertStartPoint(Name, Module, StartPoint:
                                                UnicodeString): Integer;                // Вставка стартовой точки системы
                function InsertConstructor(Name, Module, NameConstr:
                                                UnicodeString): Integer;                // Вставка конструктора системы
                function IncludeExternal(NameModule,  NameSystem,
                                         NameExternalModule: UnicodeString): Integer;   // Подключение внешнего модуля

          public


                // Служебные
                constructor Create;                                                     // Конструктор класса
                procedure Init();                                                       // Инциализация виртуальной машины

                // Вспомогательные
                function LoadSyntax(NameFile: UnicodeString;
                                    ParserID: Integer): Integer;                        // Загрузка синтаксиса
                function LoadSourceUnit(NameFile: UnicodeString;
                                        SourceId: Integer): Integer;                    // Загрузка исходного текста программы
                procedure SmallErorrsReport(var Rep: TStringList);                      // Выводит краткий отчет по ошибкам
                function ReadSource(var Txt: TStringList; Index: Integer): Integer;     // Передает исходник

                // Пользовательские
                function LoadProgram(NameFile: UnicodeString): Integer;                 // Загрузка исходника
                function GetErrorCount(): Integer;                                      // Передает количество записей в журнале ошибок
                function LoadProgram(StringList: TStringList;
                                     NameSource: UnicodeString): Integer;               // Загрузка исходника

                // Отчеты
                procedure ReportStorage(var Rep: TStringList);                          // Отчет о внутреннем состоянии хранилища систем

                // В разработке
                function Analisys(): Integer;                                           // Разбор программы
                function ParseModule(Id: Integer): Integer;                             // Разбор конкретного модуля
                function ReadFirstSectionModule(Id, start: Integer;
                                                var finish: Integer): Integer;          // Разбор секции настроек системы

                function EnterFirstOption(Option: TTempOptionSystem): Integer;          // Ввод данных первой опции в описание систем


    end;
////////////////////////////////////////////////////////////////////////////////////////

implementation

// Разбор программы
// NameFile - имя файла с текстом программы
// 0 - Операция прошла успешно
// -1 - в хода выполнения функции возникли ошибки (смотреть Errors)
function TVm.Analisys(): Integer;
var
  Msg: Integer;
begin

     // Инцииализация
     Result:=-1;

     // Разбор главного модуля
     Msg:=ParseModule(0);

     Result:=Msg;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Разбор конкретного модуля
// Id - идентификатор загруженного исходника
// 0 - Операция прошла успешно
// -1 - Во время выполнения функции возникли ошибки (смотреть Errors)
// -2 - Исходник с таким идентификатором не существует
function TVm.ParseModule(Id: Integer): Integer;
var
     Msg: Integer;                        // Сообщения об результатах выполнения функций
     Uk: Integer;                         // Указатель на текущую строку в исходном тексте
begin

     // Инициализация
     Result:=-2;
     Uk:=0;

     // Проверка входящего параметра
     If Id>Length(Source)-1 Then Exit;
     Result:=-1;

     // Читаем первую секцию (Общие параметры)
     Msg:=ReadFirstSectionModule(0, uk, uk);
     if Msg<0 then Exit;

     // Читаем вторую секцию (Состав и структура системы)
     // Читаем третью секцию (Описание функций)
end;
////////////////////////////////////////////////////////////////////////////////////////

// Разбор секции настроек системы
// Id - идентификатор разбираемого исходного текста
// start - указатель на текущую рассматриваемую строку
// finish - указатель на строку, на которой остановился разбор
// 0 - Операция прошла успешно
// -1 - В ходе выполнения работы возникли ошибки (смотреть в Errors)
function TVm.ReadFirstSectionModule(Id, start: Integer; var finish: Integer): Integer;
var
     uk: Integer;                                        // Указатель на строку
     Msg: Integer;
     Option: TTempOptionSystem;                          // Опции, загрузки секции
begin

     // Инициализация
     result:=-1;
     InitOption(Option);

     // Пропустим все пустые строки в тексте модуля
     uk:=SkipEmptyLines(Source[id].Source, start);

     // Файл состит из пустых строк?
     If uk<0 then Exit;

     // Информация о исходнике и позиции для поиска
     Option.IdSource:=Id;
     Option.Position:=uk;

     // Пытаемся добавить объявление системы из описания
     Msg:=DeclarationSystem(Option);
     If Msg<0 Then Exit;

     // На выбор, но не более одного раза!
     Msg:=ManageFirstSection(Option);
     If Msg<0 then Exit;

     // Есть ошибки?
     If getErrorCount()>0 Then Exit;

     // Внесем сведения, полученные при чтении секции
     EnterFirstOption(Option);

     // Указатель на последнюю строку для дальнейшего разбора
     finish:=Option.Position;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Ввод данных первой опции в описание систем
// Option - Опции системы
// 0 - Операция прошла успешно
// -1 - В ходе работы функции возникли проблемы (смотреть Errors)
function TVm.EnterFirstOption(Option: TTempOptionSystem): Integer;
var
     Msg: Integer;                    // Сообщения от функций
begin

     // Создаем систему по указанному описанию
     Msg:=AddNewDeclaraionSystem(Option.Type_system, Source[Option.IdSource].Name);
     If Msg<0 Then
     begin

          // Селектор ошибки
          case Msg of

               -1: GenerateError(5, Option.IdSource, Option.Position, '[Описание системы]: Описание системы уже сущесвует', Option.Type_system);             // Повторная декларация
               -2: GenerateError(6, Option.IdSource, Option.Position, '[Описание системы]: Некорректное имя системы', Option.Type_system);                      // Некорректное имя системы
               -3: GenerateError(7, Option.IdSource, Option.Position, '[Описание системы]: Имя системы начинается с цифры', Option.Type_system);                // Имя начинается с цифры
               -4: GenerateError(8, Option.IdSource, Option.Position, '[Описание системы]: Требуется указать имя объявляемой системы', Option.Type_system);     // Требуется указать имя системы
          end;

         // Сообщим о проблеме
         Result:=-1;
         Exit;
     end;

     // Вносим точку входа в систему
     Msg:=InsertStartPoint(Option.Type_system, Source[Option.IdSource].Name, Option.Start_Point);
     If Msg<0 Then
     begin

          // Селектор ошибки
          case Msg of

               -1: GenerateError(8, Option.IdSource, Option.Position, '[Точка входа]: Требуется указать объект', Option.Type_system);
               -2: GenerateError(7, Option.IdSource, Option.Position, '[Точка входа]: Имя начинается с цифры', Option.Start_Point);
               -3: GenerateError(7, Option.IdSource, Option.Position, '[Точка входа]: Имя содержит некорректные символы', Option.Start_Point);
          end;

         // Сообщим о проблеме
         Result:=-1;
         Exit;
     end;

     // Вносим конструктор системы
     Msg:=InsertConstructor(Option.Type_system, Source[Option.IdSource].Name, Option.Constructor_system);
     If Msg<0 Then
     begin

          // Селектор ошибки
          case Msg of

               -1: GenerateError(8, Option.IdSource, Option.Position, '[Конструктор системы]: Требуется указать объект', Option.Type_system);
               -2: GenerateError(7, Option.IdSource, Option.Position, '[Конструктор системы]: Имя начинается с цифры', Option.Start_Point);
               -3: GenerateError(7, Option.IdSource, Option.Position, '[Конструктор системы]: Имя содержит некорректные символы', Option.Start_Point);
          end;

         // Сообщим о проблеме
         Result:=-1;
         Exit;
     end;

     // Подключаем внешние модули
end;
////////////////////////////////////////////////////////////////////////////////////////

// Подключение внешнего модуля
// Module - имя подключаемого модуля
// 0 - Операция прошла успешно
// -1 - Файловая ошибка
function TVm.IncludeExternal(NameModule, NameSystem, NameExternalModule: UnicodeString): Integer;
begin

     // Проверим - модуль уже загружен?
     if Storage.FindExtModules(NameModule, NameSystem, NameExternalModule)>-1 then
     begin

         // Уже загружен, ничего делать не надо
     end;

     // Загружаем исходник

     // Разбираем исходник

end;
////////////////////////////////////////////////////////////////////////////////////////

// Вставка конструктора системы
// Name -  имя описания системы
// Module - модуль, где находится описание системы
// StartPoint - функция, стартовая точка системы
// 0 - Операция прошла успешно
// -1 - Функция не указана
// -2 - Имя функции начинается с цифры
// -3 - Имя функции содержит недопустимые символы
function TVm.InsertConstructor(Name, Module, NameConstr: UnicodeString): Integer;
begin

     // Инициализация
     Result:=0;

     // Формальная проверка имени конструктора объявляемой системы
     Result:=FormalCheckName(NameConstr);
     If Result<0 then Exit;

     // Добавляем конструкто в систему
     Storage.Add_Constructor_In_Declaration(Module, Name, NameConstr);
end;
////////////////////////////////////////////////////////////////////////////////////////

// Вставка стартовой точки системы
// Name -  имя описания системы
// Module - модуль, где находится описание системы
// StartPoint - функция, стартовая точка системы
// 0 - Операция прошла успешно
// -1 - Функция не указана
// -2 - Имя функции начинается с цифры
// -3 - Имя функции содержит недопустимые символы
function TVm.InsertStartPoint(Name, Module, StartPoint: UnicodeString): Integer;
Begin

     // Инициализация
     Result:=0;

     // Формальная проверка имени стартовой функции
     Result:=FormalCheckName(StartPoint);
     If Result<0 then Exit;

     // Добавляем стартовую точку к системе
     Storage.Add_StartPoint_In_Declaration(Module, Name, StartPoint);
end;
////////////////////////////////////////////////////////////////////////////////////////

// Формальная проверка имени системы
// ФУНКЦИЯ НЕ ПОЗВОЛЯЕТ ИСПОЛЬЗОВАТЬ СОСТАВНЫЕ ИМЕНА!!!! ТОЧКА В ИМЕНИ ЗАПРЕЩЕНА!!!!
// Line - проверяемая последовательность (имя функции/системы)
// 0 - Line формально синтаксически корректное имя функции/системы
// -1 - Имя не задано
// -2 - Начинается с цифры
// -3 - Содержит недопустимые символы
function TVm.FormalCheckName2(Line: UnicodeString): Integer;
begin

     // Инициализация
     Result:=0;

     // Имя указано?
     If UTF8Length(Line)<1 then Result:=-1;

     // Имя начинается с цифры?
     If CheckFirstNumber(Line)<0 then Result:=-2;

     // Имя содержит недопустимые символы?
     If CheckForbbidenSymb2(Line)<0 then Result:=-3;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Формальная проверка имени системы
// ФУНКЦИЯ НЕ ПРОВЕРЯЕТ СОСТАВНОЕ ИМЯ, НЕ ИЩЕТ ТОЧКУ
// Line - проверяемая последовательность (имя функции/системы)
// 0 - Line формально синтаксически корректное имя функции/системы
// -1 - Имя не задано
// -2 - Начинается с цифры
// -3 - Содержит недопустимые символы
function TVm.FormalCheckName(Line: UnicodeString): Integer;
begin

     // Инициализация
     Result:=0;

     // Имя указано?
     If UTF8Length(Line)<1 then Result:=-1;

     // Имя начинается с цифры?
     If CheckFirstNumber(Line)<0 then Result:=-2;

     // Имя содержит недопустимые символы?
     If CheckForbbidenSymb(Line)<0 then Result:=-3;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Поиск в имени запрещенных символов /*-+()'". и пробел с табуляцией
// ФУНКЦИЯ НЕ ДОПУСКАЕТ СОСТАВНЫХ ИМЕН!!! ТОЧКА СЧИТАЕТСЯ ЗАПРЕЩЕННЫМ СИМВОЛОМ!
// Line - проверяемая последовательность (имя функции/системы)
// 0 - Line не содержит запрещенных символов, все ОК
// -1 - Line содержит недопустимые символы
function TVm.CheckForbbidenSymb2(Line: UnicodeString): Integer;
var
     symb: UnicodeString;
     count, i, position: Integer;
begin

     // Инициализация
     Result:=0;

     // Общее количество символов в переданной строке
     count:=Utf8Length(Line);

     // Сканируем строку
     for i:=1 to count do
     begin

          // Получим очередной символ из юникодной строки
          Symb:=Utf8Copy(Line, i, 1);

          // Поиск символа в шаблоне с запрещенными символами
          position:=UTF8Pos(Symb, ForbbidenSymbols2);

          // Найдено среди запрещенных?
          If position>0 then
          begin

              // Найдены запретные символы
              Result:=-1;
              Exit;
          end;
     end;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Поиск в имени запрещенных символов /*-+()'" и пробел с табуляцией
// ФУНКЦИЯ НЕ ИЩЕТ ТОЧКУ!!!! ДОПУСКАЕТ ИСПОЛЬЗОВАНИЕ СОСТАВНЫХ ИМЕН СИСТЕМ И ФУНКЦИЙ
// Line - проверяемая последовательность (имя функции/системы)
// 0 - Line не содержит запрещенных символов, все ОК
// -1 - Line содержит недопустимые символы
function TVm.CheckForbbidenSymb(Line: UnicodeString): Integer;
var
     symb: UnicodeString;
     count, i, position: Integer;
begin

     // Инициализация
     Result:=0;

     // Общее количество символов в переданной строке
     count:=Utf8Length(Line);

     // Сканируем строку
     for i:=1 to count do
     begin

          // Получим очередной символ из юникодной строки
          Symb:=Utf8Copy(Line, i, 1);

          // Поиск символа в шаблоне с запрещенными символами
          position:=UTF8Pos(Symb, ForbbidenSymbols);

          // Найдено среди запрещенных?
          If position>0 then
          begin

              // Найдены запретные символы
              Result:=-1;
              Exit;
          end;
     end;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Проверка, начинается ли строка с цифры
// Line - проверяемая последовательность (имя функции/системы)
// 0 - Line начниается не с цифры
// -1 - Line начинается с цифры
function TVm.CheckFirstNumber(Line: UnicodeString): Integer;
var
     temp, symb: Unicodestring;
     count: Integer;
begin

     // Инициализация
     Result:=0;
     temp:='0123456789';

     // Число символов в строке
     Count:=UTF8Length(Line);

     // Символы имеются?
     If count<1 then Exit;

     // Получим первый символ строки
     symb:=Utf8Copy(Line, 1, 1);

     // Поиск среди цифр
     If Utf8Pos(symb, temp)>0 Then Result:=-1;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Добавление новой системы в раздел ОПИСАНИЙ СИСТЕМ
// NameSystem - имя добавляемой системы
// 0 - операция прошла успешно
// -1 - повторная декларация системы
// -2 - имя системы не является корректным
// -3 - имя начинается с цифры
// -4 - имя отсутствует
// Положительное число - идентификатор системы в хранилище
function TVm.AddNewDeclaraionSystem(NameSystem, Module: UnicodeString): Integer;
var
     Msg: Integer;                              // Сообщения от функций
begin

     // Проверим имя системы на недопустимые символы
     Msg:=FormalCheckName(NameSystem);

     // Анализ проблем
     If Msg<0 Then
     begin

          // Сообщим о проблеме
          case Msg of

               -1: Result:=-4;
               -2: Result:=-3;
               -3: Result:=-2;
          end;
          Exit;
     end;

     // Пытаемся внести систему (В РАЗДЕЛ ОПИСАНИЯ!!!)
     Msg:=Storage.AddExternalSystem(Module, NameSystem);

     // Передаем полученный результат
     Result:=Msg;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Отчет о внутреннем состоянии хранилища систем
// Rep - список для вывода отчета
procedure TVm.ReportStorage(var Rep: TStringList);
begin

     // Обертка
     Storage.FullReportRoot(rep);
end;
////////////////////////////////////////////////////////////////////////////////////////

// Чтение параметров первой сеции описания
// Id - Идентификатор исходного текста в хранилище
// Position - Указатель на строку в исходном тексте
// Option - структура для сохранения полученных параметров
// 0 - Строка распознана успешно
// -1 - Возникли проблемы, смотреть Errors
function TVm.ManageFirstSection(var Option: TTempOptionSystem): Integer;
var
     uk: Integer;
     Rez: TInternalConstr;
     full: Boolean;                           // Признак полного разбора секции
     Line: UnicodeString;                     // Строка для разбора
begin

     // Инцииализация
     full:=False;
     uk:=Option.Position;

     // Цикл разбора секции
     while full=false do
     begin


          // Пропустим все пустые строки в тексте модуля
          uk:=SkipEmptyLines(Source[Option.IdSource].Source, uk);
          if uk<0 then
          begin

               // Все ОК, просто кончился модуль
                Break;
          end;

          // Читаем строку
          Line:=Source[Option.IdSource].Source.Strings[uk];
          rez:=Parser[Option.IdSource].Parser.Lexer(Line);

          // Анализ
          // Перед нами подключение внешних модулей?
          If rez.id=3 then
          begin

               // Уже подключали?
               If Option.Flag_external_systems=True then
               begin

                    // Ошибка - Внешние модули уже подключались
                    GenerateError(9, Option.IdSource, uk, '[Секция Внешние модули]: Внешние модули уже объявлены ранее', rez.Param1);
                    Inc(uk);
                    Continue;
               end;

               // Внесем данные
               DecompositeLine(Option.IdSource, Uk, rez.Param1, Option.External_Systems);
               Option.Flag_external_systems:=True;
               Inc(uk);
               Continue;
          end;

          // Перед нами точка входа?
          If rez.id=5 then
          begin

               // Уже подключали?
               If Option.Flag_start_point=True Then
               begin

                    // Сообщим об ошибке: 13 - Точка входа уже объявлена
                    GenerateError(13, Option.IdSource, uk, '[Точка входа]: Точка входа уже объявлена', rez.Param1);
                    Inc(uk);
                    Continue;
               end;

               // Внесем данные о точке входа в систему
               Option.Start_Point:=rez.Param1;
               Option.Flag_start_point:=True;
               Inc(uk);
               Continue;
          end;

          // Перед нами конструктор?
          If rez.id=4 then
          begin

               // Уже подключали?
               if Option.Flag_constructor_system=True then
               begin

                    // Сообщим об ошибке: 14 - Конструктор уже объявлен ранее
                    GenerateError(14, Option.IdSource, uk, '[Начальная секция]: Конструктор уже объявлен ранее', rez.Param1);
                    Inc(uk);
                    Continue;
               end;

               // Внесем данные о конструкторе
               Option.Constructor_system:=rez.Param1;
               Option.Flag_constructor_system:=True;
               Inc(uk);
               Continue;
          end;

          // Перед нами секция интернационализации?
          If rez.id=6 then
          begin

               // Уже подключали?
               If Option.Flag_i18=True then
               begin

                    // Сообщим об ошибке: 15 - Секция интернационализации объявлена ранее
                    GenerateError(15, Option.IdSource, uk, '[Секция интернационализации]: Секция интернационализации объявлена ранее', rez.Param1);
                    Inc(uk);
                    Continue;
               end;

               // Внесем данные о секции интернационализации
               Option.Position:=uk;
               Parse_i18(Option);
               uk:=Option.Position;
               Option.Flag_i18:=True;
               Continue;
          end;

          // Проверим, все ли секции были рассмотрены
          If CheckFirstOption(Option)>-1 Then Break;

          // Встретилась неизвестная конструкция
          if (Rez.id>6) Or (Rez.id<3) Then Break;
          Inc(Uk);
     end;

     // Позиция для следующей секции
     Option.Position:=uk;
     Result:=0;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Проверка все ли компоненты первой секции были введены
// Option - Опции первой секции
// 0 - Все секции были указаны
// -1 - Не все секции были обработаны
function TVm.CheckFirstOption(Option: TTempOptionSystem): Integer;
var
     count: Integer;
Begin

     // Инициализация
     Result:=-1;
     count:=0;

     // Считаем опции
     If Option.Flag_constructor_system=True Then Inc(Count);
     If Option.Flag_external_systems=True   Then Inc(Count);
     If Option.Flag_i18=True                Then Inc(Count);
     If Option.Flag_start_point=True        Then Inc(Count);

     If Count=4 Then Result:=0;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Разбор и проверка секции интернационализации
// ФУНКЦИЯ НА ДАННЫЙ МОМЕНТ НИГДЕ НЕ СОХРАНЯЕТ СВЕДЕНИЯ О СЕКЦИИ ИНТЕРНАЦИОНАЛИЗАЦИИ!!!!!!!!!!!!
// Option - опции для внесения информации о разборе первой секции описания системы
// 0 - Операция прошла успешно
// -1 - В ходе работы функции вознкла ошибка (подробности в Errors)
function TVm.Parse_i18(var Option: TTempOptionSystem): Integer;
var
     uk: Integer;
     point_divider: Integer;
     s: TSource;
     Line: UnicodeString;
     Key: UnicodeString;
begin

     // Инциализация
     Result:=0;
     Uk:=Option.Position+1;

     // Разбираем код, пока не закончится модуль
     s:=Source[Option.IdSource];
     While uk<s.Source.Count-1 do
     begin

          // Пропускаем пустые строки
          uk:=SkipEmptyLines(Source[Option.IdSource].Source, uk);

          // Модуль закончился?
          If uk<0 Then
          begin

               // Все отлично
               Break;
          end;

          // Ищем строку вида "Ключ = Значение"
          Line:=UTF8Trim(s.Source[uk]);

          // Найдем знак равно
          point_divider:=UTF8Pos('=', Line, 1);

          // Есть знак равно?
          If point_divider<1 then
          begin

               // Не найдено, перед нами иная конструкция
               Break;
          end;

          // Ключ указан?
          If point_divider=1 then
          begin

               // Сообщим о ошибке
               GenerateError(16, Option.IdSource, uk, '[Секция интернационализации]: Пропущено значение', Line);
               Inc(uk);
               Continue;
          end;

          // Получим отдельно ключ
          Key:=UTF8Trim(UTF8Copy(Line, 1, point_divider-1));

          // Проверим соответствие ключа на русские символы
          If CheckCyrillic(UTF8UpperCase(Key))<0 then
          begin

               // Сообщим об ошибке 17 - Требуется правильное кириллическое обозначение
               GenerateError(17, Option.IdSource, uk, '[Секция интернационализации]: Требуется правильное кириллическое обозначение', Key);
          end;

          // Следующий шаг
          Inc(Uk);
     end;

     // Операция прошла успешно
     If uk>s.Source.Count-1 Then Uk:=-1;
     Option.Position:=Uk;
     If GetErrorCount()<1 then Result:=0;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Проверяет состоит ли данная строка сплошь из символов кириллицы
// Функция позволяет использовать спецзнаки и цифры
// 0 - да, состоит
// -1 - пусто или имеются иные символы
function TVm.CheckCyrillic(Line: UnicodeString): Integer;
var
     template, symb: UnicodeString;
     count, i: Integer;
begin

     // Инициализация
     Result:=-1;
     template:='0123456789!@#$%^&_~?АБВГДЕЁЖЗИКЛМНОПРСТУФХЦЧШЩЪЬЫЭЮЯ';

     // Общее количество символов
     count:=UTF8Length(Line);

     // Проверяем каждый символ
     for i:=1 to count do
     begin

          // Получим символ
          symb:=UTF8Copy(Line, i, 1);

          // Проверим символ
          If UTF8Pos(symb, template, 1)<1 then
          begin

               // Это недопустимый символ
               Exit;
          end;
     end;

     // Все символы были проверены и корректны
     Result:=0;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Разложение строки на элементы массива, разложение происходит
// Предыдущее содержимое массива будет удалено
// Id - идентификатор парсера для обработки указанного исходника
procedure TVm.DecompositeLine(Id, LineCode: Integer; Line: UnicodeString; var Mass: TStrMass);
var
     Msg: Integer;                // Сообщения от функций
     i, count: Integer;
begin

     // Проведем разложение строки
     Msg:=Parser[Id].Parser.ParseListInLine(Line, Mass);

     // Анализ проблем
     case Msg of
          -1: GenerateError(10, LineCode, Id, 'Встретились два разделителя подряд', Line);
          -2: GenerateError(11, LineCode, Id, 'При перечислении пропущено последнее слово  ', Line);
          -3: GenerateError(12, LineCode, Id, 'Требуется указать перечисление', Line);
     end;

     // Анализ имен добавляемых модулей
     // Общее число внешних модулей
     count:=Length(Mass);

     // Сканируем имена внешних модулей
     for i:=0 to count-1 do
     begin

          // Строгая формальная проверка имени внешнего модуля
          Msg:=FormalCheckName2(Mass[i]);

          case Msg of

               -1: GenerateError(8, LineCode, Id, 'Требуется указать объект', Line);
               -2: GenerateError(7, LineCode, Id, 'Имя модуля начинается с цифры', Mass[i]);
               -3: GenerateError(6, LineCode, Id, '[Добавление внешних модулей]: Некорректное имя', Mass[i]);
          end;
     end;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Подготовка опций первой секции
// Option - опции первой секции
procedure TVm.InitOption(var Option: TTempOptionSystem);
begin

     // Устанавливаем параметры по умолчанию
     SetLength(Option.External_Systems, 0);

     Option.Flag_constructor_system:=False;
     Option.Flag_external_systems:=False;
     Option.Flag_start_point:=False;
     Option.Flag_i18:=False;

     Option.Constructor_system:='';
     Option.Start_Point:='';
     Option.Type_system:='';

     Option.IdSource:=0;
     Option.Position:=0;

end;
////////////////////////////////////////////////////////////////////////////////////////

// Описание системы из внешнего модуля
// Id - Идентификатор исходного текста в хранилище
// Position - Указатель на строку в исходном тексте
// 0 - Строка распознана успешно
// -1 - Возникли проблемы, смотреть Errors
function TVm.DeclarationSystem(var Option: TTempOptionSystem): Integer;
Var
     Msg: Integer;
     Rez: TInternalConstr;
begin

     // Инцииализация
     Result:=-1;

     // Обязательно первым должно идти обозначение системы
     rez:=Parser[Option.IdSource].Parser.Lexer(Source[Option.IdSource].Source.Strings[Option.Position]);
     if rez.id<>2 then
     begin

          // Сообщение об ошибке
          GenerateError(4, Option.IdSource, Option.IdSource, '[Имя системы]: Первое объявление должно быть описанием типа системы',
                        Source[Option.IdSource].Source.Strings[Option.Position]);
          Exit;
     end;

     // Внесем имя создаваемой системы в информацию о настройках
     Option.Type_system:=rez.Param1;

     // Операция прошла успешно
     Option.Position:=Option.Position+1;
     Result:=0;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Генерация ошибки
// Id - код ошибки
// Line - Указатель на строку с ошибкой
// Mess - Текстовое описание ошибки
// Param - Дополнительный параметр описания
// Source - Модуль-источник ошибки
procedure TVm.GenerateError(Id, Line, SourceID: Integer; Mess, Param: UnicodeString);
var
     ErrPosition: Integer;
begin

     // Добавляем новую ошибку
     ErrPosition:=AddError();

     // Настраиваем поля
     Errors[ErrPosition].Id:=ID;
     Errors[ErrPosition].Line:=Line;
     Errors[ErrPosition].Message:=Mess;
     Errors[ErrPosition].Param:=Param;
     Errors[ErrPosition].SourceId:=SourceId;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Загрузка исходника
// 0 - операция прошла успешно
// -1 - во время работы возникла ошибка (смотреть в Errors)
function TVm.LoadProgram(StringList: TStringList; NameSource: UnicodeString): Integer;
var
     msg: Integer;                     // Сообщения из других функций
     uk: Integer;
begin

     // Инициализация
     ClearErrors;
     Init();

     // Загрузка основного диалекта
     SetLength(Parser, 1);
     Parser[0].Parser:=TParse.Create;
     msg:=LoadSyntax(DialectPath, 0);

     // Анализ
     If msg=-1 then
     begin

          // Сообщим об ошибке (Ошибка внутри структуры файла синтаксиса)
          Result:=-1;
          uk:=AddError();
          Errors[uk].Id:=2;
          Exit;
     end;
     If msg=-2 then
     begin

          // Сообщим об ошибке (Не удалось загрузить файл синтаксиса)
          Result:=-1;
          uk:=AddError();
          Errors[uk].Id:=1;
          Errors[uk].Param:=DialectPath;
          Exit;
     end;

     // Загрузка первого переданного исходного текста программы
     SetLength(Source, 1);
     Source[0].Source:=TStringList.Create;
     Source[0].Source.Text:= StringList.Text;
     Source[0].NameFile:=NameSource;

     // Имя исходника
     Source[0].Name:=ExtractFileName(NameSource);
     Source[0].Name:=ChangeFileExt(Source[0].Name, '');

     // Операция прошла успешно
     Result:=0;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Передает исходник
// Txt - список строк с исходным текстом
// Index - номер исходного текста
// 0 - операция прошла успешно
// -1 - номер исходного текста указан неверно
function TVm.ReadSource(var Txt: TStringList; Index: Integer): Integer;
var
  i, count: Integer;

begin

     // Инициализация
     Result:=-1;
     txt.Clear;

     // Определим количество исходных текстов
     count:=Source[Index].Source.Count;

     // Передаем в цикле
     for i:=0 to count-1 do
     begin

          // Каждую строку
          Txt.Add(Source[Index].Source.Strings[i]);
     end;

     // Операция прошла успешно
     Result:=0;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Передает количество записей в журнале ошибок
function TVm.GetErrorCount(): Integer;
begin

     // Обертка
     Result:=Length(Errors);
end;
////////////////////////////////////////////////////////////////////////////////////////

// Загрузка исходника
// 0 - операция прошла успешно
// -1 - во время работы возникла ошибка (смотреть в Errors)
function TVm.LoadProgram(NameFile: UnicodeString): Integer;
var
     msg: Integer;                     // Сообщения из других функций
     uk: Integer;
begin

     // Инициализация
     Init();

     // Загрузка основного диалекта
     SetLength(Parser, 1);
     Parser[0].Parser:=TParse.Create;
     msg:=LoadSyntax(DialectPath, 0);

     // Анализ
     If msg=-1 then
     begin

          // Сообщим об ошибке (Ошибка внутри структуры файла синтаксиса)
          Result:=-1;
          uk:=AddError();
          Errors[uk].Id:=2;
          Exit;
     end;
     If msg=-2 then
     begin

          // Сообщим об ошибке (Не удалось загрузить файл синтаксиса)
          Result:=-1;
          uk:=AddError();
          Errors[uk].Id:=1;
          Errors[uk].Param:=DialectPath;
          Exit;
     end;

     // Загрузка первого переданного исходного текста программы
     SetLength(Source, 1);
     msg:=LoadSourceUnit(NameFile, 0);

     // Анализ
     If msg=-2 then
     begin

          // Сообщим об ошибке (не удалось загрузить исходный файл)
          Result:=-1;
          uk:=AddError();
          Errors[uk].Id:=3;
          Errors[uk].Param:=NameFile;
          Exit;
     end;

     // Информация о источнике
     Source[0].NameFile:=NameFile;

     // Операция прошла успешно
     Result:=0;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Выводит краткий отчет по ошибкам
// Rep - список строк для вывода отчета
procedure TVm.SmallErorrsReport(var Rep: TStringList);
var
     i, count: Integer;
begin

     // Количество записей в журнале ошибок
     count:=Length(Errors);

     // Выводим все ошибки
     for i:=0 to count-1 do
     begin

          // Построчно
          Rep.Add(GetError(i));
     end;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Упаковывает ошибку в текстовую строку
// Index - номер ошибки в журнале
function TVm.GetError(Index: Integer): UnicodeString;
var
     s: UnicodeString;
begin

     // Инициализация
     Result:='';

     // Исходник загружен?
     if Errors[Index].SourceId>Length(Source)-1 then
     begin

          s:='Не загружен';
     end
     Else
     begin

          s:=Source[Errors[Index].SourceId].NameFile;
     end;

     // Сборка строки
     Result:='Исходник: '+s+' | Строка: ';
     Result:=Result+IntToStr(Errors[Index].Line)+' | Код ошибки: ';
     Result:=Result+IntToStr(Errors[Index].Id)+ #13+#10+'Сообщение: ';
     Result:=Result+Errors[Index].Message+#13+#10+ 'Сведения: ';
     Result:=Result+Errors[Index].Param+#13+#10+'======================================';
end;
////////////////////////////////////////////////////////////////////////////////////////

// Добавить структуру ошибки в конец
// Возвращает позицию ошибки в журнале
function TVm.AddError(): Integer;
var
  count: Integer;
begin

     // Определим количество записей в журнале ошибок
     count:=Length(Errors);

     // Добавим запись в конец
     SetLength(Errors, count+1);

     // Вернем указатель на последнюю запись
     Result:=count;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Инциализация виртуальной машины
procedure TVm.Init();
begin

     // Хранилище систем
     InitStorage();

     // Инцииализация исходных текстов
     InitSource();

     // Инцииализация парсеров
     InitParsers();

     // Список сообщений об ошибках
     ClearErrors();
end;
////////////////////////////////////////////////////////////////////////////////////////

// Очистка списка сообщений об ошибках
procedure TVm.ClearErrors();
begin

     // Обертка
     SetLength(Errors, 0);
end;
////////////////////////////////////////////////////////////////////////////////////////

// Инциализация парсеров
procedure TVm.InitParsers();
var
  i, count: Integer;
begin

     // Общее количество открытых парсеров
     count:=Length(Parser);

     // Инциализация каждого парсера
     for i:=0 to count-1 do
     begin

          // Освобождение каждого парсера
          Parser[i].Parser.Free;
     end;

     // Удаляем все парсеры
     Setlength(Parser, 0);
end;
////////////////////////////////////////////////////////////////////////////////////////

// Инициализация хранилища исходных текстов
procedure TVm.InitSource();
var
  i, count: Integer;
begin

     // Общее количество исходных текстов
     count:=Length(Source);

     // Инциализация каждого исходного текста
     for i:=0 to count-1 do
     begin

          // Очистка каждого исходника
          Source[i].Source.Free;
     end;

     // Удаляем все исходники
     SetLength(Source, 0);
end;
////////////////////////////////////////////////////////////////////////////////////////

// Инцииализация хранилища систем
procedure TVm.InitStorage();
begin

     // Полная инициализация
     Storage.Init();

     // Инфраструктура для работы виртуальной машины
     // Storage.AddServiceSystem();
end;
////////////////////////////////////////////////////////////////////////////////////////

// Указывает на первую не пустую строку в тексте
// Line - Список строк
// Start - строка с которой нужно производить поиск
// Возвращает указатель на не пустую строку
// -1 - конец списка или непустых строк не найдено
function TVm.SkipEmptyLines(Line: TStringList; Start: Integer): Integer;
var
  i, count, x: Integer;
  a: UnicodeString;
begin

     // Инициализация
     Result:=-1;

     // Количество строк в списке
     count:=Line.Count;

     // Данные указаны верно?
     If start>count-1 then Exit;

     // Сканируем поток строк
     for i:=start to count-1 do
     begin

          // Не пустая строка?
          a:=UTF8Trim(Line[i]);
          x:=UTF8Length(a);
          If x>0 then
          begin

               // Передаем информацию о первой не пустой строке
               Result:=i;
               Exit;
          end;
     end;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Загрузка исходного текста программы
// NameFile - имя файла-исходного текста программы
// SourceId - идентификатор исходного текста
// 0 - Операция прошла успешно
// -1 - Неверно указан исходник
// -2 - Файловая ошибка
function TVm.LoadSourceUnit(NameFile: UnicodeString; SourceId: Integer): Integer;
var
  count: Integer;
begin

     // Инцииализация
     Result:=-1;

     // Проверка входящего параметра
     count:=Length(Source);
     If (SourceId<0) or (SourceId>count-1) Then
     begin

          // Сообщим об ошибке
          Exit;
     end;

     // Проверим иницилиазацию исходника
     If Source[SourceId].Source=nil then Source[SourceId].Source:=TStringList.Create;

     // Пытаемся загрузить данные
     try

        // Загрузка исходного текста
        Source[SourceId].Source.LoadFromFile(NameFile);
     except

        // Произошла ошибка загрузки файла
        Result:=-2;
        Exit;
     end;

     // Внесем информацию о имени файла
     Source[SourceId].NameFile:=NameFile;

     // Операция прошла успешно
     Result:=0;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Загрузка синтаксиса
// NameFile - имя файла-синтаксиса для загрузки
// ParserId - идентификатор парсера
// 0 - операция прошла успешно
// -1 - ошибка при загрузке данных
// -2 - файловая ошибка
// -3 - нет такого парсера
function TVm.LoadSyntax(NameFile: UnicodeString; ParserID: Integer): Integer;
var
  count: Integer;
begin

     // Инициализация
     Result:=-3;

     // Проверим входящий параметр
     count:=Length(Parser);
     if (ParserId<0) or (ParserId>count-1) Then Exit;

     // Загружаем данные из файла
     Result:=Parser[ParserId].Parser.LoadConstrSet(NameFile);
end;
////////////////////////////////////////////////////////////////////////////////////////

// Конструктор класса
constructor TVm.Create;
begin

     // Предок
     Inherited Create;

     // Создание подсистем
     SetLength(Parser, 1);
     Setlength(Source, 1);

     // Инициализация подсистем
     Storage:=TStorage.Create;
     Parser[0].Parser:=TParse.Create();
     Source[0].Source:=TStringList.Create();

     // Список запрещенных символов
     ForbbidenSymbols:='/*-+():''" '+#13+#10+#9;
     ForbbidenSymbols2:='/*-+():''" .'+#13+#10+#9;        // Добавлена точка, запрет на использование составных имен
end;
////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////

// СПИСОК КОДОВ ОШИБОК, ЗАПИСЫВАЕМЫХ В ERRORS РАЗЛИЧНЫМИ ФУНКЦИЯМИ

// LoadProgram - загрузка главного модуля программы
// 1 - Не удалось загрузить основной диалект краткой записи main.dct - файловая ошибка
// 2 - Диалект краткой записи main.dct содержит ошибки в своей структуре
// 3 - Не удалось загрузить файл с исходным текстом программы

// ReadFirstSectionModule - разбор секции настроек системы (самая первая секция)
// 4 - Первое объявление должно быть описанием типа системы
// 5 - Добавляемая система уже существует
// 6 - Некорректное имя добавляемой системы
// 7 - Имя начинается с цифры
// 8 - Имя системы не задано
// 9 - Внешние модули уже объявлены
// 10 - Два разделителя подряд
// 11 - При перечислении пропущено последнее слово
// 12 - Требуется указать перечисление
// 13 - Точка входа уже объявлена
// 14 - Конструктор уже объявлен ранее
// 15 - Секция интернационализации объявлена ранее
// 16 - Пропущено значение
// 17 - Требуется правильное кириллическое обозначение

end.

