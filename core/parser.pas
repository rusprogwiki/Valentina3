// Парсер для разбора строк языков прораммирования
unit parser;

{$mode objfpc}{$H+}
{$codepage UTF8}

interface

uses
  {$IFNDEF WINDOWS}
    cwstring,
    {$ENDIF}
  Classes, SysUtils, lazutf8, lazutf8classes;

// Массив строк
Type
  TStrMass = array of UnicodeString;
////////////////////////////////////////////////////////////////////////////////////////////

// Описание конструкции
type
  TTemplateConstr=record

    Remark: UnicodeString;                    // Описание конструкции
    Id:     Integer;                          // Семнатический идентификатор конструкции
    Words:  Array [1..8] of UnicodeString     // Слова-шаблоны для распознавания
  end;
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// Описание функции
type
  TFunct=record

    Name: UnicodeString;                       // Имя функции
    Id: Integer;                               // Идентификатор функции
    Remark: UnicodeString;                     // Описание функции
    Params: Integer;                           // Количество параметров функции
  end;
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// Описание набора конструкций
type
  TConstrs=record

    Name: UnicodeString;                       // Название набора конструкций
    Remark: UnicodeString;                     // Описание набора конструкций
    Open_qoute: UnicodeString;                 // Открывающая кавычка текстовой константы
    Close_qoute: UnicodeString;                // Закрывающая кавычка текстовой константы
    Open_bracket: UnicodeString;               // Открывающая скобка
    Close_bracket: UnicodeString;              // Закрывающая скобка
    Divider: UnicodeString;                    // Разделитель при перечислении

    Constr: Array of TTemplateConstr;          // Описание конструкций
    Funct: Array of TFunct;                    // Описание используемых функций
  end;
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// Внутреннее представление конструкции
type
  TInternalConstr=record

    id: Integer;                               // Идентификатор конструкции
    Param1: UnicodeString;                     // Параметр1
    Param2: UnicodeString;                     // Параметр2
    Param3: UnicodeString;                     // Параметр3
    param4: UnicodeString;                     // Параметр4
    Original: Integer;                         // Положение строки в исходном коде
  end;
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// Результат поиска независимой лексемы
type
  TFindFreeLex=record

    IndexLex: Integer;                         // Индекс лексемы в хранилище лексем
    IndexString: Integer;                      // Индекс независимой лексемы в искомой строке
  end;
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// Класс парсера
type
  TParse=class
    protected
    private

      // Поля
      Constr: TConstrs;                                                                 // Описание основного набора конструкций
      NonLex: Array of TTemplateConstr;                                                 // Семантически не значащие конструкции
      Remarks: Array of TTemplateConstr;                                                // Комментарии
      Tokens: Array of UnicodeString;                                                   // Независимые лексемы (в сплошном тексте выделяются как самостоятельные)


      // Методы
      procedure FreeFinder(Line: UnicodeString; var FindFreeLex: TFindFreeLex);         // Поиск независимой лексемы в указанной строке
      function SectionStatus(a: UnicodeString): Integer;                                // Определяет указана ли в строке секция
      procedure ClearTemplate(var Cnstr: TTemplateConstr;
                              var Fcnt:  TFunct);                                       // Инициализация шаблонов
      procedure ParseFunct(a: UnicodeString; var Fcnt: TFunct);                         // Распознование команды для секции Funct
      procedure ParseGeneral(a: UnicodeString);                                         // Распознование команды для секции General
      procedure ParseConstr(a: UnicodeString; var Cnstr:
                            TTemplateConstr);                                           // Распознование команды для секции Constr
      function CheckFunct(Fcnt: TFunct): Boolean;                                       // Проверка корректности введеных данных для функции
      procedure AddFunct(Fcnt: TFunct);                                                 // Добавление функции в набор
      function CheckConstr(Cnstr: TTemplateConstr): Boolean;                            // Проверка корректности введенных данных для конструкции
      procedure AddConstr(Cnstr: TTemplateConstr);                                      // Добавление конструкции в набор
      function TypeSymb(symb: UnicodeString): Integer;                                  // Возвращает тип символа (для парсинга строки кода)
      procedure FindNonLex();                                                           // Поиск и копирование незначащих лексем
      procedure CopyNonLex(PointMain, PointNonLex: Integer);                            // Копирование незначащей лексемы из основного набора конструкций
      function FindFreeTokens(var Lex: UnicodeString): UnicodeString;                   // Ищет в строке независимые лексемы
      procedure FreeToken(var Lex: UnicodeString; var rez: TStringListUTF8);            // Разбивает строку на независимые токены, если они там есть
      function GetParamEnd(Lexems: TStringListUtf8; uk: Integer): UnicodeString;        // Переносит все от указанной позиции в параметр
      function CheckParam(Storage: Array of TTemplateConstr;
                          Temp, uk: Integer): Integer;                                  // Проверяет, является ли указанное слово в конструкции номером параметра
      function CheckLexTemp(Lexems: TStringListUtf8;
                            uk: Integer;
                            Storage: Array of TTemplateConstr;
                            Temp, posit: Integer): Integer;                             // Проверка соответствует ли конкретная лексема шаблону
      function CheckLexTemp2(Lexems: TStringListUtf8;
                             uk: Integer;
                             LexTemp: UnicodeString): Integer;                          // Проверка соответствует ли конкретная лексема шаблону
      function GetParamConstr(Lexems: TStringListUtf8;
                              var uk: Integer;
                              Template: UnicodeString): UnicodeString;                  // Получение параметра конструкции
      procedure ClearInternalConstr(var Cnstr: TInternalConstr);                        // Инициализация внутренного представления конструкции
      procedure AddToken(Token: UnicodeString);                                         // Добавление независимой лексемы
      procedure DropNonLex(var ListLex: TStringListUTF8);                               // Удаляет незначащие лексемы из списка лексем
      function LengthConstr(Cnstr: TTemplateConstr): Integer;                           // Определяет количество действительных лексем в конструкции
      function CheckLexem(Cnstr: TTemplateConstr; ListLex: TStringListUTF8): Integer;   // Сравнение набора лексем с конструкцией
      function CheckWord(template, Lex: UnicodeString): Boolean;                        // Проверка соответствия лексемы указанному набору

    public

      // Служебные
      constructor Create;                                                               // Конструктор
      destructor Destroy; override;                                                     // Деструктор
      procedure Init;                                                                   // Инициализация набора конструкций
      function CheckNameSystem(Name: UnicodeString): Integer;                           // Проверка имени системы
      procedure DropRemark(var Lexems: TStringListUtf8);                                // Удаление комментария из списка лексем

      // Пользовательские
      function LoadConstrSet(NameFile: UnicodeString): Integer;                         // Загрузка шаблонов конструкций
      function LoadConstrSet(Constrs: TStringListUTF8): Integer;                        // Загрузка шаблонов конструкций
      procedure ParseString(a: UnicodeString; var rez: TStringListUTF8);                // Разложение строки на лексемы
      procedure EraseConstr(IndexConstr: Integer);                                      // Удаление конструкции из набора
      procedure AddToken2(Token: UnicodeString);                                        // Добавление независимых лексем
      procedure AddRemark(Cnstr: TTemplateConstr);                                      // Добавление конструкции в набор комментариев
      function ParseListInLine(Line: UnicodeString;
                               var Mass: TStrMass): Integer;                            // Разбор перечисления в указанный массив строк
      function GetWordLine(var Line: UnicodeString): UnicodeString;                     // Получаем слово из строки
      function ParseLexems(Lexems: TStringListUtf8;
                           Storage: Array of TTemplateConstr;
                           Temp: Integer): TInternalConstr;                             // Проверка соответствия списка лексем конструкции
      function Lexer(Line: UnicodeString): TInternalConstr;                             // Лексический анализ строки

      // Отчеты
      procedure ReportNonLex(var rez: TStringListUtf8);                                 // Отчет по незначащим лексемам
      procedure ReportConstr(var rez: TStringListUtf8);                                 // Отчет по конструкциям
      procedure ReportFunct(var rez: TStringListUtf8);                                  // Отчет по функциям
      procedure ReportTokens(var rez: TStringListUtf8);                                 // Отчет по независимым лексемам
      procedure ReportGeneral(var rez: TStringListUtf8);                                // Отчет по общим параметрам
      procedure Report(var rep: TStringListUTF8);                                       // Общий отчет
      procedure ReportRemarks(var rez: TStringListUtf8);                                // Отчет по комментариям

      // В разработке






  end;
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

implementation

// Разбор перечисления в указанный массив строк
// Line - строка, содержащая перечисление
// Mass - массив, куда необходимо поместить перечисление
// 0 - Операция прошла успешно
// -1 - Встретились два разделителя (пропущено слово)
// -2 - Пропущено последнее слово
// -3 - Перечисление отсутствуеь
function TParse.ParseListInLine(Line: UnicodeString; var Mass: TStrMass): Integer;
var
     temp: UnicodeString;                          // Временное хранилище обрабатываемой строки
     count: Integer;                               // Счетчик элементов в строковом массиве
     uk: Integer;                                  // Позиция первого разделителя в строке
     words: UnicodeString;                         // Слово, полученное из перечисления
begin

     // Инициализация
     SetLength(Mass, 0);
     count:=0;
     temp:=UTF8Trim(Line);
     Result:=0;

     // Перечисление есть?
     If temp='' Then Result:=-3;

     // Цикличная обработка строк
     while temp<>'' do
     begin

          // Поиск разделителя
          uk:=UTF8Pos(Constr.Divider, temp);

          // Нашли разделитель?
          if uk>0 then
          begin

               // Значит перед нами как минимум два слова в перечислении
               // Откусим слово
               words:=UTF8Trim(UTF8Copy(temp, 1, UTF8Length(Constr.Divider)+uk-2));

               // Анализ
               If words='' then Result:=-1;

               // Поместим слово в массив
               Inc(count);
               SetLength(Mass, count);
               Mass[count-1]:=words;

               // Разделитель последний символ в строке?
               If uk+UTF8Length(Constr.Divider)-1=UTF8Length(temp) then
               begin

                    // Сообщим об ошибке - не указано последнее слово
                    Result:=-2;
                    Exit;
               end;

               // Откусим слово из строки
               temp:=UTF8Copy(temp, uk+UTF8Length(Constr.Divider), UTF8Length(temp)-(uk+UTF8Length(Constr.Divider))+1);
               Continue;
          end;

          // Значит у нас только одно слово ]
          temp:=UTF8Trim(temp);

          // Слово есть?
          If temp='' then
          begin

               // Сообщим о проблеме
               Result:=-1;
          end;

          // Поместим слово в массив
          Inc(count);
          SetLength(Mass, count);
          Mass[count-1]:=temp;
          Exit;
     end;
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Проверка имени системы
// Name - Имя системы
// 0 - Имя корректно
// -1 - Имя начинается с цифры
// -2 - Имя содержит недопустимые символы
// -3 - Имя отсутствует
function TParse.CheckNameSystem(Name: UnicodeString): Integer;
var
  i, count: Integer;
begin

     // Инцииализация
     Result:=0;

     // Имя имеет символы?
     count:=UTF8Length(Name);
     If count<1 then
     begin

          // Имя не содержит символов в своем составе
          Result:=-3;
          Exit;
     end;

     // Проверим на цифру
     If UTF8Copy(Name, 1, 1)='0' Then Result:=-1;
     If UTF8Copy(Name, 1, 1)='1' Then Result:=-1;
     If UTF8Copy(Name, 1, 1)='2' Then Result:=-1;
     If UTF8Copy(Name, 1, 1)='3' Then Result:=-1;
     If UTF8Copy(Name, 1, 1)='4' Then Result:=-1;
     If UTF8Copy(Name, 1, 1)='5' Then Result:=-1;
     If UTF8Copy(Name, 1, 1)='6' Then Result:=-1;
     If UTF8Copy(Name, 1, 1)='7' Then Result:=-1;
     If UTF8Copy(Name, 1, 1)='8' Then Result:=-1;
     If UTF8Copy(Name, 1, 1)='9' Then Result:=-1;
     If Result<0 Then Exit;

     // Сканируем символы
     for i:=1 to count do
     begin

          // Недопустимый символ?
          If UTF8Copy(Name, i, 1)=' ' Then Result:=-2;
          If UTF8Copy(Name, i, 1)=#13 Then Result:=-2;
          If UTF8Copy(Name, i, 1)=#9  Then Result:=-2;
          If UTF8Copy(Name, i, 1)=#10 Then Result:=-2;
          If Result<0 Then Exit;
     end;

     // Недопустимые символы из диалекта
     If Utf8Pos(Constr.Open_qoute, Name)>0 then Result:=-2;
     If Utf8Pos(Constr.Close_qoute, Name)>0 then Result:=-2;
     If Utf8Pos(Constr.Open_bracket, Name)>0 then Result:=-2;
     If Utf8Pos(Constr.Close_qoute, Name)>0 then Result:=-2;
     If Utf8Pos(Constr.Close_bracket, Name)>0 then Result:=-2;
     If Utf8Pos(Constr.Divider, Name)>0 then Result:=-2;

end;
////////////////////////////////////////////////////////////////////////////////////////////

// Лексический анализ строки
function TParse.Lexer(Line: UnicodeString): TInternalConstr;
var
  Lexems: TStringListUtf8;
  ConstrCount, i: Integer;
begin

    // Инициализация
    Lexems:=TStringListUtf8.Create;
    ClearInternalConstr(Result);

    // Распарсим строку
    ParseString(Line, Lexems);

    // Откинем комментарий
    DropRemark(Lexems);

    // Определим количество конструкций в наборе
    ConstrCount:=Length(Constr.Constr);

    // Сравниваем с каждой конструкцией в наборе
    for i:=0 to ConstrCount-1 do
    begin

      // Совпадает?
      Result:=ParseLexems(Lexems, Constr.Constr, i);
      If Result.id>-1 then Break;
      //ClearInternalConstr(Result);
    end;

    // Заключительные операции
    Lexems.Free;
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Удаление комментария из списка лексем
procedure TParse.DropRemark(var Lexems: TStringListUtf8);
var
  i, RemCount: Integer;
  temp, a: TInternalConstr;
begin

     // Количество лексем комментариев
     RemCount:=Length(Remarks);

     // Анализ на соответствие
     for i:=0 to RemCount-1 do
     begin

       // Проверим на соответсвие конкретной конструкции из хранилища
       temp:=ParseLexems(Lexems, Remarks, i);

       // Перед нами комментарий?
       If temp.id>-1 then
       begin

         // Убираем все лексемы
         Lexems.Clear;

         // Распарсим снова уже без комментария
         ParseString(temp.Param1, Lexems);
         Exit;
       end;
     end;
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Проверка соответствия списка лексем конструкции
// Lexems - список лексем
// Storage - хранилище конструкций
// temp - указатель на конструкцию в хранилище
// Если конструкция не соответствует будет возвращено Result.id:=-1;
function TParse.ParseLexems(Lexems: TStringListUtf8; Storage: Array of TTemplateConstr; Temp: Integer): TInternalConstr;
var
  LexemCount, WordCount, i, param, uk: Integer;
  ParamText: UnicodeString;
begin

    // Определим количество лексем в списке
    LexemCount:=Lexems.Count;

    // Пустая строка?
    If LexemCount<1 then
    begin

      // Перед нами пустая строка
      ClearInternalConstr(Result);
      result.id:=-2;
      Exit;
    end;

    // Определим количество значащих слов в конструкции
    WordCount:=LengthConstr(Storage[Temp]);

    // Быстрое сравнение
    If LexemCount<WordCount then
    begin

      // В списке меньше лексем, чем в конструкции
      ClearInternalConstr(Result);
      Exit;
    end;

    // Сканируем шаблон
    i:=1;              // Рассматриваемая лексема в шаблоне
    uk:=0;             // Рассматриваемая лексема в списке лексем
    while i<WordCount+1 do
    begin

      // Должен быть параметр?
      param:=CheckParam(Storage, temp, i);
      If param=0 then
      begin

        // Проведем лексическое сравнение
        if CheckLexTemp(Lexems, uk, Storage, Temp, i)=-1 then
        begin

          // Список лексем не соответствует Шаблону конструкции
          ClearInternalConstr(Result);
          Exit;
        end;
      end
      Else
      begin

        // Действия при получении параметра
        // Параметр это последнее слово в шаблоне?
        If i=WordCount then
        begin

          // Собираем последние лексемы в параметр
          ParamText:=GetParamEnd(Lexems, uk);

          // Вносим данные в параметр
          If param=1 then Result.Param1:=ParamText;
          If param=2 then Result.Param2:=ParamText;
          If param=3 then Result.Param3:=ParamText;
          If param=4 then Result.Param4:=ParamText;

          // Конструкция определена
          Result.id:=Storage[temp].Id;

          // Работа завершена
          Exit;
        end;

        // Копируем параметр
        ParamText:=GetParamConstr(Lexems, uk, Storage[temp].Words[i+1]);
        Dec(Uk);

        // Вносим данные в параметр
        If param=1 then Result.Param1:=ParamText;
        If param=2 then Result.Param2:=ParamText;
        If param=3 then Result.Param3:=ParamText;
        If param=4 then Result.Param4:=ParamText;
      end;

      // Сравниваем следующие элементы
      Inc(i);
      Inc(uk);
    end;

    // Анализ хвоста
    If uk<LexemCount then
    begin

      // Значит остались еще сведения
      ClearInternalConstr(Result);
      Exit;
    end;

    // Конструкция совпала
    Result.id:=Storage[Temp].Id;
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Получение параметра конструкции
function TParse.GetParamConstr(Lexems: TStringListUtf8; var uk: Integer; Template: UnicodeString): UnicodeString;
var
   countLex: Integer;
   Param: UnicodeString;
begin

    // Инициализация
    Param:='';

    // Определим число лексем в списке
    countLex:=Lexems.Count;

    // Перебираем лексемы пока не встретим искомый шаблон
    while uk<countLex do
    begin

      // Нужное слово?
      If CheckLexTemp2(Lexems, uk, Template)=0 then
      begin

        // Операция завершена
        Result:=Param;
        Exit;
      end;

      // Копируем лексему в параметр
      If Param<>'' then
      begin

        Param:=Param+' '+Lexems.Strings[uk];
      end
      Else
      begin
        Param:=Lexems.Strings[uk];
      end;

      // Следующая лексема
      Inc(Uk);
    end;

    // Результат
    Result:=Param;
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Инициализация внутренного представления конструкции
// Cnstr - внутренне представление конструкции
procedure TParse.ClearInternalConstr(var Cnstr: TInternalConstr);
begin

    Cnstr.id:=-1;
    Cnstr.Original:=-1;
    Cnstr.Param1:='';
    Cnstr.Param2:='';
    Cnstr.Param3:='';
    Cnstr.param4:='';
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Проверка соответствует ли конкретная лексема шаблону
// Lexems - список лексем для анализа
// uk - указатель на проверяемую лексему в списке лексем
// LexTemp - шаблон для анализа
// 0 - лексема соответствует шаблону
// -1 - Лексема не соответствует шаблону
function TParse.CheckLexTemp2(Lexems: TStringListUtf8; uk: Integer; LexTemp: UnicodeString): Integer;
var
  Sample, Lex, Lex2: UnicodeString;
begin

    // Инициализация
    Result:=-1;

    // Получим лексему
    Lex:=Lexems[uk];

    // Получим шаблон для сравнения
    Sample:=LexTemp;

    // Выбираем из шаблона
    While Sample<>'' do
    begin

      // Получим лексему
      Lex2:=GetWordLine(Sample);

      // Совпадает?
      If Lex2=Lex then
      begin

        // Сообщим о совпадении
        Result:=0;
        Exit;
      end;
    end;
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Проверка соответствует ли конкретная лексема шаблону
// Lexems - список лексем для анализа
// uk - указатель на проверяемую лексему в списке лексем
// Strorage - хранилище конструкций
// Temp - указатель на конкретную конструкцию
// posit - указатель на слово в конструкции
// 0 - лексема соответствует шаблону
// -1 - Лексема не соответствует шаблону
function TParse.CheckLexTemp(Lexems: TStringListUtf8; uk: Integer; Storage: Array of TTemplateConstr; Temp, posit: Integer): Integer;
var
  Sample, Lex, Lex2: UnicodeString;
begin

    // Инициализация
    Result:=-1;

    // Получим лексему
    // Безопасное приведение
    If lexems.Count-1<uk then uk:=Lexems.Count-1;
    Lex:=Utf8UpperCase(Lexems[uk]);

    // Получим шаблон для сравнения
    Sample:=Storage[temp].Words[posit];

    // Выбираем из шаблона
    While Sample<>'' do
    begin

      // Получим лексему
      Lex2:=GetWordLine(Sample);

      // Совпадает?
      If Lex2=Lex then
      begin

        // Сообщим о совпадении
        Result:=0;
        Exit;
      end;
    end;
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Проверяет, является ли указанное слово в конструкции номером параметра
// Storage - хранилище, где нужно искать лексему
// Temp - шаблон конструкции
// Uk - номер проверяемого слова
// Возвращает номер содержащегося параметра (от 1 до 4)
// 0 - Это не параметр
function TParse.CheckParam(Storage: Array of TTemplateConstr; Temp, uk: Integer): Integer;
var
  Lex: UnicodeString;
begin

    // Инициализация
    Result:=0;

    // Получим лексему для анализа
    Lex:=Storage[temp].Words[uk];

    // Пытаемся получить параметр
    try

      Result:=StrToInt(Lex);
    Except

      Result:=0;
    end;

    // Анализ
    If (Result>4) or (Result<1) then Result:=0;
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Переносит все от указанной позиции в параметр
function TParse.GetParamEnd(Lexems: TStringListUtf8; uk: Integer): UnicodeString;
var
  i, count: Integer;
begin

    // Инцииализация
    Result:='';

    // Определим количество лексем
    count:=Lexems.Count;

    // Переносим в цикле
    for i:=uk to count-1 do
    begin

      // Соединяем лексемы
      If Result<>'' then
      begin

        Result:=Result+' '+Lexems[i];
      end
      Else
      begin

        Result:=Lexems[i];
      end;
    end;
end;
///////////////////////////////////////////////////////////////////////////////////////////

// Добавление независимых лексем
// Token - список независимых лексем в строке через пробел
procedure TParse.AddToken2(Token: UnicodeString);
var
  Line, Tok: UnicodeString;
begin

    // Подготовка
    Line:=Token;

    // Передаем пословно
    While Line<>'' do
    begin

      // Получаем отдельный токен
      Tok:=GetWordLine(Line);

      // Добавляем токен
      AddToken(Tok);
    end;
end;
///////////////////////////////////////////////////////////////////////////////////////////

// Получаем слово из строки
// Line - Входящая строка
// Возвращает первое слово, удаляя его из Line
function TParse.GetWordLine(var Line: UnicodeString): UnicodeString;
var
  uk: Integer;
begin

     // Нормализация
    Line:=Trim(Line);

    // Ищем первый пробел
    uk:=Pos(' ', Line);

    // Слова есть?
    If uk<1 then
    begin

      // Входящая строка единственное слово
      Result:=Line;
      Line:='';
      Exit;
    end;

    // Получаем слово
    Result:=copy(Line, 1, uk-1);

    // Отрезаем слово
    Line:=copy(Line, uk+1, Length(Line)-Length(Result)-1);
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Разбивает строку на независимые токены, если они там есть
// Lex - входная строка лексем
// rez - результирующий список лексем (при этом предыдущие сведения из него не удаляются)
procedure TParse.FreeToken(var Lex: UnicodeString; var rez: TStringListUTF8);
var
  i, count: Integer;
  lex2, lex1: UnicodeString;
begin

    // Максимальное количество итераций
    count:=Length(Lex);

    // Инцииализация
    lex2:='';
    lex1:=lex;

    // Сканируем лексемы в цикле
    for i:=0 to count-1 do
    begin

      // Выбираем лексемы
      lex2:=FindFreeTokens(lex);

      // Анализ
      if lex2='' then
      begin

        // Частный случай
        if lex1=lex then Exit;

        // Больше нечего разбирать
        if (lex<>'') then
        begin

          rez.Add(lex);
          lex:='';
          Exit;
        end;
      end;

      // Кончились лексемы?
      if lex='' then
      begin

        // Добавим остаток
        if lex2<>'' then  rez.Add(lex2);
        Exit;
      end;

      // Добавляем лексему
      rez.Add(lex2);
    end;
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Ищет в строке независимые лексемы
// Функция будет резать текстовую последовательность до тех пор пока в ней содержатся независимые лексемы
// Пример: Независимая лексема -->, строка для разбора x-->x+1
// Входной параметр Lex:=x-->x+1
// Шаг 1: Lex:=-->x+1, FindFreeTokens:=x
// Шаг 2: Lex:=x+1, FindFreeTokens:=-->
// Шаг 2: Lex:=x+1, FindFreeTokens:=''
// Lex - строка для анализа (предполагается лексема)
// Результат лексема
function TParse.FindFreeTokens(var Lex: UnicodeString): UnicodeString;
var
  i, count, uk, LengthToken, LengthLex: Integer;
  Rez: TFindFreeLex;
begin

    // Инициализация
    Result:='';

    // Количество символов во входящей строке
    LengthLex:=Length(Lex);

    // Ищем лексему
    FreeFinder(Lex, Rez);

    // Нашли?
    If Rez.IndexLex<0 then
    begin

         // Значит это сплошная лексема
         Result:=Lex;
         Lex:='';
         Exit;
    end;

    // Лексема первая?
    If Rez.IndexString=1 then
    begin

         // Лексема и есть строка?
         If Lex=Tokens[Rez.IndexLex] then
         begin

              // Частный случай
              Result:=Lex;
              Lex:='';
              Exit;
         end;

         // Копируем
         Result:=Utf8copy(Lex, 1, Length(Tokens[Rez.IndexLex]));

         // Отрезаем
         Lex:=Utf8Trim(Utf8copy(lex, Length(Tokens[Rez.IndexLex])+1, LengthLex-Length(Tokens[Rez.IndexLex])));
         Exit;
    end;

    // Лексема в середине
    Result:=Utf8Trim(Utf8Copy(lex, 1, Rez.IndexString-1));
    count:=LengthLex-(Rez.IndexString-1);
    Lex:=Utf8Trim(Utf8Copy(Lex, Rez.IndexString, count));
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Поиск независимой лексемы в указанной строке
// Функция ищет первую по списку независимую лексему,
// поэтому для корректной работы с независимыми лексемами длинные нужно выставлять раньше коротких
// Line - строка в которой производится поиск
// FindFreeLex - результат поиска
// -1, -1 - если ничего не найдено
procedure TParse.FreeFinder(Line: UnicodeString; var FindFreeLex: TFindFreeLex);
var
  i, count, uk: Integer;
begin

    // Инцииализация
    FindFreeLex.IndexLex:=-1;
    FindFreeLex.IndexString:=-1;

    // Частный случай
    if Line='' then Exit;

    // Определим количество независимых лексем в хранилище
    count:=Length(Tokens);

    // Сканируем лексемы
    for i:=0 to count-1 do
    begin

      // Нашли?
      uk:=Utf8Pos(Tokens[i], Line);
      if uk>0 then
      begin

        // Готовим результат
        FindFreeLex.IndexString:=uk;
        FindFreeLex.IndexLex:=i;
        Exit;
      end;
    end;
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Краткий отчет по набору конструкций
// rep - список для отчета
procedure TParse.Report(var rep: TStringListUTF8);
begin

    // Подготовим список
    rep.Clear;

    // Общий отчет
    ReportGeneral(rep);

    // Конструкции
    ReportConstr(rep);

    // Комментарии
    ReportRemarks(rep);

    // Функции
    ReportFunct(rep);

    // Незначащие лексемы
    ReportNonLex(rep);

    // Независимые лексемы
    ReportTokens(rep);
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Отчет по общим параметрам
// rez - список для отчета
procedure TParse.ReportGeneral(var rez: TStringListUtf8);
begin

    // Формируем заголовок
    rez.Add('');
    rez.Add('Общие параметры:');
    rez.Add('Имя набора конструкций: '+Constr.Name);
    rez.Add('Описание набора конструкций: '+Constr.Remark);
    rez.Add('Открывающая кавычка: <'+Constr.Open_qoute+'>');
    rez.Add('Закрывающая кавычка: <'+Constr.Close_qoute+'>');
    rez.Add('Открывающая скобка: <'+Constr.Open_bracket+'>');
    rez.Add('Закрывающая скобка: <'+Constr.Close_bracket+'>');
    rez.Add('Разделитель параметров: <'+Constr.Divider+'>');
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Отчет по независимым лексемам
// rez - список для отчета
procedure TParse.ReportTokens(var rez: TStringListUtf8);
var
  i, count: Integer;
  buffer: UnicodeString;
begin

    // Формируем заголовок
    rez.Add('');
    rez.Add('Независимые лексемы:');

    // Определим количество независимых лексем
    count:=length(Tokens);

    // Добавим сведения в отчет
    buffer:='Количество: ';
    buffer:=buffer+IntToStr(count);
    rez.Add(buffer);

    // Есть токены?
    If count>0 then
    begin

      // Форматирование
      rez.Add('');

      // Выводим информацию о функциях
      for i:=0 to count-1 do
      begin

        // Готовим строку с номером
        buffer:='   №:'+IntToStr(i+1);
        rez.Add(buffer);

        // Токен
        buffer:='   Токен: '+Tokens[i];
        rez.Add(buffer);

        // Форматирование
        rez.Add('============================');
      end;
    end
    else
    begin

        // Форматирование
        rez.Add('============================');
    end;
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Отчет по функциям
// rez - список для отчета
procedure TParse.ReportFunct(var rez: TStringListUtf8);
var
  i, count: Integer;
  buffer: UnicodeString;
begin

    // Формируем заголовок
    rez.Add('');
    rez.Add('Функции:');

    // Определим количество конструкций
    count:=length(Constr.Funct);

    // Добавим сведения в отчет
    buffer:='Количество: ';
    buffer:=buffer+IntToStr(count);
    rez.Add(buffer);

    // Есть Функции?
    If count>0 then
    begin

      // Форматирование
      rez.Add('');

      // Выводим информацию о функциях
      for i:=0 to count-1 do
      begin

        // Готовим строку с номером
        buffer:='   №:'+IntToStr(i+1);
        rez.Add(buffer);

        // Имя функции
        buffer:='   Имя: '+Constr.Funct[i].Name;
        rez.Add(buffer);

        // Количество параметров
        buffer:='   Количество параметров: '+IntToStr(Constr.Funct[i].Params);
        rez.Add(buffer);

        // Идентификатор
        buffer:='   Идентификатор: '+IntToStr(Constr.Funct[i].Id);
        rez.Add(buffer);

        // Строка с описанием
        buffer:='   Описание: '+Constr.Funct[i].Remark;
        rez.Add(buffer);

        // Форматирование
        rez.Add('============================');
      end;
    end
    else
    begin

        // Форматирование
        rez.Add('============================');
    end;
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Отчет по комментариям
// rez - список для отчета
procedure TParse.ReportRemarks(var rez: TStringListUtf8);
var
  i, count, j: Integer;
  buffer: UnicodeString;
begin

    // Формируем заголовок
    rez.Add('');
    rez.Add('Комментарии:');

    // Определим количество конструкций
    count:=length(Remarks);

    // Добавим сведения в отчет
    buffer:='Количество: ';
    buffer:=buffer+IntToStr(count);
    rez.Add(buffer);

    // Есть конструкции?
    If count>0 then
    begin

      // Форматирование
      rez.Add('');

      // Выводим информацию о конструкциях
      for i:=0 to count-1 do
      begin

        // Готовим строку с номером
        buffer:='   №:'+IntToStr(i+1);
        rez.Add(buffer);

        // Идентификатор
        buffer:='   Идентификатор: '+IntToStr(Remarks[i].Id);
        rez.Add(buffer);

        // Строка с описанием
        buffer:='   Описание: '+Remarks[i].Remark;
        rez.Add(buffer);

        // Слова входящие в конструкцию
        for j:=1 to 8 do
        begin

          // Построчно
          buffer:='   Word'+IntToStr(j)+': '+Remarks[i].Words[j];
          rez.Add(buffer);
        end;

        // Форматирование
        rez.Add('============================');
      end;
    end
    else
    begin

        // Форматирование
        rez.Add('============================');
    end;
end;
//////////////////////////////////////////////////////////////////////////////////////////////

// Отчет по конструкциям
// rez - список для отчета
procedure TParse.ReportConstr(var rez: TStringListUtf8);
var
  i, count, j: Integer;
  buffer: UnicodeString;
begin

    // Формируем заголовок
    rez.Add('');
    rez.Add('Конструкции:');

    // Определим количество конструкций
    count:=length(Constr.Constr);

    // Добавим сведения в отчет
    buffer:='Количество: ';
    buffer:=buffer+IntToStr(count);
    rez.Add(buffer);

    // Есть конструкции?
    If count>0 then
    begin

      // Форматирование
      rez.Add('');

      // Выводим информацию о конструкциях
      for i:=0 to count-1 do
      begin

        // Готовим строку с номером
        buffer:='   №:'+IntToStr(i+1);
        rez.Add(buffer);

        // Идентификатор
        buffer:='   Идентификатор: '+IntToStr(Constr.Constr[i].Id);
        rez.Add(buffer);

        // Строка с описанием
        buffer:='   Описание: '+Constr.Constr[i].Remark;
        rez.Add(buffer);

        // Слова входящие в конструкцию
        for j:=1 to 8 do
        begin

          // Построчно
          buffer:='   Word'+IntToStr(j)+': '+Constr.Constr[i].Words[j];
          rez.Add(buffer);
        end;

        // Форматирование
        rez.Add('============================');
      end;
    end
    else
    begin

        // Форматирование
        rez.Add('============================');
    end;
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Отчет по незначащим лексемам
// rez - список для отчета
procedure TParse.ReportNonLex(var rez: TStringListUtf8);
var
  i, count, j: Integer;
  buffer: UnicodeString;
begin

    // Формируем заголовок
    rez.Add('');
    rez.Add('Незначащие лексемы:');

    // Определим количество незначащих лексем
    count:=length(NonLex);

    // Добавим сведения в отчет
    buffer:='Количество: ';
    buffer:=buffer+IntToStr(count);
    rez.Add(buffer);

    // Есть незначащие лексемы?
    If count>0 then
    begin

      // Форматирование
      rez.Add('');

      // Выводим информацию о незначащих лексемах
      for i:=0 to count-1 do
      begin

        // Готовим строку с номером
        buffer:='   №:'+IntToStr(i+1);
        rez.Add(buffer);

        // Идентификатор
        buffer:='   Идентификатор: '+IntToStr(NonLex[i].Id);
        rez.Add(buffer);

        // Строка с описанием
        buffer:='   Описание: '+NonLex[i].Remark;
        rez.Add(buffer);

        // Слова входящие в конструкцию
        for j:=1 to 8 do
        begin

          // Построчно
          buffer:='   Word'+IntToStr(j)+': '+NonLex[i].Words[j];
          rez.Add(buffer);
        end;

        // Форматирование
        rez.Add('============================');
      end;
    end
    else
    begin

        // Форматирование
        rez.Add('============================');
    end;
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Удаляет незначащие лексемы из списка лексем
// ListLex - список лексем для разбора
procedure TParse.DropNonLex(var ListLex: TStringListUTF8);
var
  countLex, countNonLex, i, j, rez: Integer;
begin

    // Определим число лексем, переданных для разбора
    countLex:=ListLex.Count;

    // В переданном для разбора списке вообще есть лексемы?
    if countLex<1 then
    begin

      // Нет лексем для анализа
      Exit;
    end;

    // Определим количество незначащих лексем
    countNonLex:=Length(NonLex);

    // Перебираем все незначащие лексемы
    for i:=0 to countNonlex-1 do
    begin

      // Получим количество слов в незначащем выражении
      rez:=CheckLexem(Nonlex[i], ListLex);

      // Анализ
      If rez=0 then
      begin

        // Список это полностью незначащая конструкция
        ListLex.Clear;
        Exit;
      end;

      // Есть остаток?
      If rez>0 then
      begin

        // Удалим лишние элементы
        for j:=0 to rez-1 do
        begin

          // Поэлементно
          ListLex.Delete(0);
        end;

        Exit;
      end;
    end;
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Сравнение набора лексем с конструкцией
// Данная версия не учитывает параметры в наборе лексем
// Данная версия не учитывает оставшиеся лексемы в списке
// Cnstr - шаблон конструкции
// ListLex - список лексем для проверки
// -1 - список лексем не совпадает
// другое положительное число указывает на значащую лексему сразу после проверенных
// 0 - лексемы совпадают с конструкцией, но свободных больше не осталось
function TParse.CheckLexem(Cnstr: TTemplateConstr; ListLex: TStringListUTF8): Integer;
var
  i, count, countLex: Integer;
begin

     // Инициализация
     result:=-1;

     // Определим количество лексем в шаблоне конструкции
     count:=LengthConstr(Cnstr);

     // Шаблон задан верно?
     if count<1 then Exit;

     // Определим количество лексем в списке лексем
     countLex:=ListLex.Count;

     // В списке лексем меньше чем в шаблоне?
     If countLex<count then Exit;

     // Лексемное сравнение
     for i:=1 to count do
     begin

          // Проверим на соответствие лексемы
          if CheckWord(Cnstr.Words[i], ListLex.Strings[i-1])=False then
          begin

            // Данный набор лексем не совпадает с шаблоном конструкции
            Exit;
          end;
     end;

     // Анализ остатка
     if count=countLex then
     begin

       // Остатка больше нет
       Result:=0;
       Exit;
     end;

     // Укажем на остаток
     Result:=count;
end;
//////////////////////////////////////////////////////////////////////////////////////////////

// Проверка соответствия лексемы указанному набору
// template - шаблон для проверки - лексемы разделенные пробелом
// lex - лексема для сравнения
// True - лексема соответствует одной из шаблона
function TParse.CheckWord(template, Lex: UnicodeString): Boolean;
var
  temp, wordLex: UnicodeString;
  uk: Integer;
begin

     // Инициализация
     Result:=False;
     temp:=Utf8UpperCase(Utf8Trim(template));
     Lex:=Utf8UpperCase(Lex);
     WordLex:='';

     // Сканируем лексемы из шаблона
     while length(temp)>0 do
     begin

       // Получаем очередную лексему из шаблона
       uk:=Utf8Pos(' ', temp);
       if uk>0 then
       begin

         // Исключаем лишние пробелы
         if uk=1 then
         begin
           temp:=Utf8copy(temp, 2, length(temp)-1);
           continue;
         end;

         // Отсекаем лексему
         wordLex:=Utf8Trim(copy(temp, 1, uk));
         Delete(temp, 1, uk);
       end
       Else
       begin

         // Последняя лексема в шаблоне
         WordLex:=temp;
         temp:='';
       end;

       // Лексема совпала?
       If WordLex=lex then
       begin

         // Сообщим о совпадении
         Result:=True;
         Exit;
       end;
     end;
end;
///////////////////////////////////////////////////////////////////////////////////////////////

// Определяет количество действительных лексем в конструкции
function TParse.LengthConstr(Cnstr: TTemplateConstr): Integer;
var
  i: Integer;
begin

    // Сканируем лексемы конструкции
    for i:=1 to 8 do
    begin

      // Пустая строка?
      If Cnstr.Words[i]='' then
      begin

        // Передадим действительное число лексем в конструкции
        Result:=i-1;
        Exit;
      end;
    end;
end;
//////////////////////////////////////////////////////////////////////////////////////////////

// Поиск и копирование незначащих лексем
procedure TParse.FindNonLex();
var
    count, uk, countNonLex: Integer;
begin

    // Подготовка хранилища
    SetLength(NonLex, 0);

    // Определим количество конструкций
    count:=length(Constr.Constr);
    uk:=0;

    // Сканируем конструкции
    while uk<count do
    begin

      // Идентификатор конструкции 0?
      if Constr.Constr[uk].Id=0 then
      begin

        // Добавляем новую незначащую лексему
        countNonLex:=Length(NonLex);
        Setlength(NonLex, countNonLex+1);

        // Копируем из основного источника
        CopyNonLex(uk, CountNonLex);

        // Удаляем конструкцию из набора
        EraseConstr(uk);
        count:=length(Constr.Constr);
        continue;
      end;

      // Обновляем параметры
      Inc(Uk);
      count:=length(Constr.Constr);
    end;
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Удаление конструкции из набора
// IndexConstr - указатель на конструкцию в наборе
procedure TParse.EraseConstr(IndexConstr: Integer);
var
    count: Integer;
begin

    // Количество конструкций в наборе
    count:=Length(Constr.Constr);

    // Контроль входного параметра
    If (IndexConstr<0) or (IndexConstr>count-1) then Exit;

    // Частный случай - единственная конструкция
    If count=1 then
    begin

      // Удаляем конструкцию
      SetLength(Constr.Constr, 0);
      Exit;
    end;

    // Копируем последнюю на место удаляемой
    Dec(Count);
    Constr.Constr[IndexConstr].Id:=Constr.Constr[count].Id;
    Constr.Constr[IndexConstr].Remark:=Constr.Constr[count].Remark;
    Constr.Constr[IndexConstr].Words[1]:=Constr.Constr[count].Words[1];
    Constr.Constr[IndexConstr].Words[2]:=Constr.Constr[count].Words[2];
    Constr.Constr[IndexConstr].Words[3]:=Constr.Constr[count].Words[3];
    Constr.Constr[IndexConstr].Words[4]:=Constr.Constr[count].Words[4];
    Constr.Constr[IndexConstr].Words[5]:=Constr.Constr[count].Words[5];
    Constr.Constr[IndexConstr].Words[6]:=Constr.Constr[count].Words[6];
    Constr.Constr[IndexConstr].Words[7]:=Constr.Constr[count].Words[7];
    Constr.Constr[IndexConstr].Words[8]:=Constr.Constr[count].Words[8];

    // Удаляем последнюю конструкцию
    SetLength(Constr.Constr, count);
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Копирование незначащей лексемы из основного набора конструкций
// PointMain - указатель на конструкцию в основном наборе
// PointNonLex - указатель на конструкцию в наборе незначащих лексем
procedure TParse.CopyNonLex(PointMain, PointNonLex: Integer);
begin

    // Копирование полей структуры
    NonLex[PointNonLex].Id:=Constr.Constr[PointMain].Id;
    NonLex[PointNonLex].Remark:=Constr.Constr[PointMain].Remark;
    NonLex[PointNonLex].Words[1]:=Constr.Constr[PointMain].Words[1];
    NonLex[PointNonLex].Words[2]:=Constr.Constr[PointMain].Words[2];
    NonLex[PointNonLex].Words[3]:=Constr.Constr[PointMain].Words[3];
    NonLex[PointNonLex].Words[4]:=Constr.Constr[PointMain].Words[4];
    NonLex[PointNonLex].Words[5]:=Constr.Constr[PointMain].Words[5];
    NonLex[PointNonLex].Words[6]:=Constr.Constr[PointMain].Words[6];
    NonLex[PointNonLex].Words[7]:=Constr.Constr[PointMain].Words[7];
    NonLex[PointNonLex].Words[8]:=Constr.Constr[PointMain].Words[8];
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Разложение строки на лексемы
// a - Строка для разбора
// rez - Возращаемый список лексем
procedure TParse.ParseString(a: UnicodeString; var rez: TStringListUTF8);
var
  count, i, tp_symb: Integer;
  symb, lex: UnicodeString;
  status: Integer;          // 0 - ввод слова и прочих символов
                            // 1 - текстовая константа
begin

    // Инициализация результата
    rez.Clear;

    // Вводим обычный текст
    status:=0;

    // Определим количество символов в строке
    count:=Length(a);

    // Готовим слово
    lex:='';

    // Сканируем строку
    for i:=1 to count do
    begin

      // Получим символ
      symb:=Utf8Copy(a, i, 1);

      // Получим тип символа
      tp_symb:=TypeSymb(symb);

      // Мы в режиме ввода текстовой константы?
      if status=1 then
      begin

        // Открывающая и закрывающая кавычка один и тот же символ?
        if (Constr.Open_qoute=Constr.Close_qoute) and (tp_symb=2) then
        begin

          // Значит уже закрывающая кавычка
          tp_symb:=3;
        end;

        // Перед нами закрывающая кавычка?
        if tp_symb=3 then
        begin

          // Ввод текстовой константы закончен
          lex:=lex+symb;
          rez.Add(lex);
          lex:='';
          status:=0;
          Continue;
        end
        Else
        begin

          // Иначе просто вводим дальше
          lex:=lex+symb;
          continue;
        end;
      end;

      // Перед нами просто символ?
      if (tp_symb=0) or (tp_symb=3) then
      begin

        // Просто добавим его к текущему слову
        lex:=lex+symb;

        // Следующий символ
        continue;
      end;

      // Перед нами пробел или символ табуляции?
      if tp_symb=1 then
      begin

        // Есть что вносить?
        if lex<>'' then
        begin

          // Ищем независимые лексемы
          FreeToken(lex, rez);

          // Ввод слова закончен
          if lex<>'' then rez.Add(lex);
          lex:='';
        end;

        // Готовимся к вводу нового слова
        lex:='';
        continue;
      end;

      // Начало текстовой константы?
      If tp_symb=2 then
      begin

        // Ввод закончен
        if lex<>'' then
        begin

          // Ищем независимые лексемы
          FreeToken(lex, rez);

          // Добавим лексему
          if lex<>'' then rez.Add(lex);
          lex:='';
        end;
        lex:=symb;

        // Переходим в режим ввода текстовой константы
        status:=1;
        continue;
      end;

      // В случае прочих знаков
      if tp_symb>3 then
      begin

        // Ввод слова закончен
        if lex<>'' then
        begin

          // Ищем независимые лексемы
          FreeToken(lex, rez);

          // Заносим слово, если оно есть
          if lex<>'' then rez.Add(lex);
          lex:='';
        end;

        // Добавим сам знак
        rez.Add(symb);

        // Обнуляем лексему
        lex:='';
      end;
    end;

    // Дозаполним последнюю строку
    if lex<>'' then
    begin

      // Ищем независимые лексемы
      FreeToken(lex, rez);

      // Заполняем слово
      if lex<>'' then rez.Add(lex);
    end;

    // Отбрасываем незначащие лексемы
    DropNonLex(rez);
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Возвращает тип символа (для парсинга строки кода)
// symb - символ для распознавания
// 0 - прочий символ
// 1 - пробел или символ табуляции
// 2 - открывающая кавычка текстовой константы
// 3 - закрывающая кавычка текстовой константы
// 4 - открывающая скобка
// 5 - закрывающая скобка
// 6 - разделитель при перечислении
function TParse.TypeSymb(symb: UnicodeString): Integer;
var
  template: UnicodeString;
begin

    // Пробел или символ табуляции
    if (symb=' ') or (symb=#32) then
    begin

      // Передаем код разделителя
      Result:=1;
    end;

    // Готовим шаблон для сравнения
    template:=' '+Constr.Open_qoute[1]+Constr.Close_qoute[1]+Constr.Open_bracket[1]+Constr.Close_bracket[1]+Constr.Divider[1];

    // Найдем индекс символа
    Result:=Utf8Pos(symb, template);
end;
//////////////////////////////////////////////////////////////////////////////////////////////

// Загрузка шаблонов конструкций
// Constrs - список для загрузки
// 0 - операция прошла успешно
// -1 - ошибка при загрузке данных
// -2 - файловая ошибка
function TParse.LoadConstrSet(NameFile: UnicodeString): Integer;
var
  Constrs: TStringListUTF8;             // Хранилище для анализа строк
begin

    // Создадим список-контейнер данных, получаемых из файла
    Constrs:=TStringListUTF8.Create;

    // Пытаемся получить данные из указанного файла
    try

           // Загрузка
           Constrs.LoadFromFile(NameFile);
    except

          // Сообщим о проблеме
          Result:=-2;

          // Освобождаем список
          Constrs.Free;
          Exit;
    end;

    // Загрузка данных из списка
    Result:=LoadConstrSet(Constrs);

    // Освобождаем список
    Constrs.Free;
end;
//////////////////////////////////////////////////////////////////////////////////////////////

// Загрузка шаблонов конструкций
// Constrs - список для загрузки
// 0 - операция прошла успешно
// -1 - ошибка при загрузке данных
function TParse.LoadConstrSet(Constrs: TStringListUTF8): Integer;
var
  i, count, status, z: Integer;
  a: UnicodeString;
  fnct: TFunct;              // Шаблон функции для записи при смене секции
  cnstr: TTemplateConstr;    // Шаблон конструкции для записи при смене секции
begin

    // Инцииализация набора
    Result:=0;
    Init;
    SetNeedRTLAnsi(True);

    // Инициализация наборов
    ClearTemplate(cnstr, fnct);

    // Статус General
    status:=0;
    //Constrs.Add('GENERAL');

    // Количество строк в списке
    count:=Constrs.Count;

    // Сканируем в цикле построчно
    for i:=0 to count-1 do
    begin

      // Получим строку для анализа
      a:=Utf8UpperCase(Utf8Trim(Constrs.Strings[i]));

      // Пустая строка?
      If a='' then
      begin

        // Просто продолжаем работу
        Continue;
      end;

      // Новая секция?
      z:=SectionStatus(a);
      if z>-1 then
      begin

        // Был режим ввода функции?
        If status=2 then
        begin

          // Проверим корректность ввода
          If CheckFunct(fnct)=False then
          begin

            // Сообщим об ошибке
            Result:=-1;
            Exit;
          end;

          // Запишем данные
          AddFunct(fnct);
        end;

        // Был режим ввода конструкции?
        If status=1 then
        begin

          // Проверим корректность ввода
          If CheckConstr(cnstr)=False then
          begin

            // Сообщим об ошибке
            Result:=-1;
            Exit;
          end;

          // Запишем данные
          AddConstr(Cnstr);
        end;

        // Режим ввода общих настроек
        if status=0 then
        begin

          // Распознаем настройки
          ParseGeneral(a);
        end;

        // Меняем режим
        status:=z;

        // Очистим заготовки
        ClearTemplate(cnstr, fnct);

        // Продолжим
        continue;
      end
      Else
      begin

        // Если секция не изменилась
        // Режим ввода функции
        If status=2 then
        begin

          // Парсим параметры функции
          ParseFunct(a, fnct);
        end;

        // Режим ввода настроек
        If status=0 then
        begin

          // Парсим общие параметры
          ParseGeneral(a);
        end;

        // Режим ввода конструкции
        If status=1 then
        begin

          // Парсим конструкцию
          ParseConstr(a, cnstr);
        end;
      end;
    end;

    ParseGeneral(a);
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Добавление функции в набор
procedure TParse.AddFunct(Fcnt: TFunct);
var
  count: Integer;
begin

    // Определим количество функций в наборе
    count:=Length(Constr.Funct);

    // Добавим еще одну функцию
    Setlength(Constr.Funct, count+1);

    // Перенос данных
    Constr.Funct[count].Params:=Fcnt.Params;
    Constr.Funct[count].Remark:=Fcnt.Remark;
    Constr.Funct[count].Id:=Fcnt.Id;
    Constr.Funct[count].Name:=Fcnt.Name;
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Добавление конструкции в набор
procedure TParse.AddConstr(Cnstr: TTemplateConstr);
var
  count, i, countNonLex: Integer;

begin

    // Параметр - незначащая лексема?
    if Cnstr.Id=0 then
    begin

        // Добавляем новую незначащую лексему
        countNonLex:=Length(NonLex);
        Setlength(NonLex, countNonLex+1);

        // Копируем из переданного параметра
        NonLex[countNonLex].Id:=Cnstr.Id;
        NonLex[countNonLex].Remark:=Cnstr.Remark;
        NonLex[countNonLex].Words[1]:=Cnstr.Words[1];
        NonLex[countNonLex].Words[2]:=Cnstr.Words[2];
        NonLex[countNonLex].Words[3]:=Cnstr.Words[3];
        NonLex[countNonLex].Words[4]:=Cnstr.Words[4];
        NonLex[countNonLex].Words[5]:=Cnstr.Words[5];
        NonLex[countNonLex].Words[6]:=Cnstr.Words[6];
        NonLex[countNonLex].Words[7]:=Cnstr.Words[7];
        NonLex[countNonLex].Words[8]:=Cnstr.Words[8];
        Exit;
    end;

    // Параметр - комментарий?
    if Cnstr.Id=1 then
    begin

        // Добавляем комментарий в хранилище
        AddRemark(Cnstr);
        Exit;
    end;

    // Определим количество конструкций в наборе
    count:=Length(Constr.Constr);

    // Добавим еще одну конструкцию
    Setlength(Constr.Constr, count+1);

    // Перенос данных
    Constr.Constr[count].Id:=Cnstr.Id;
    Constr.Constr[count].Remark:=Cnstr.Remark;

    // Слова
    for i:=1 to 8 do
    begin

      // Перенос
      Constr.Constr[count].Words[i]:=Cnstr.Words[i];
    end;
end;
////////////////////////////////////////////////////////////////////////////////////////

// Добавление конструкции в набор комментариев
// Cnstr - добавляемая конструкция
procedure TParse.AddRemark(Cnstr: TTemplateConstr);
var
  count: Integer;
begin

    // Добавляем новый комментарий
    count:=Length(Remarks);
    Setlength(Remarks, count+1);

    // Копируем из переданного параметра
    Remarks[count].Id:=Cnstr.Id;
    Remarks[count].Remark:=Cnstr.Remark;
    Remarks[count].Words[1]:=Cnstr.Words[1];
    Remarks[count].Words[2]:=Cnstr.Words[2];
    Remarks[count].Words[3]:=Cnstr.Words[3];
    Remarks[count].Words[4]:=Cnstr.Words[4];
    Remarks[count].Words[5]:=Cnstr.Words[5];
    Remarks[count].Words[6]:=Cnstr.Words[6];
    Remarks[count].Words[7]:=Cnstr.Words[7];
    Remarks[count].Words[8]:=Cnstr.Words[8];
end;
////////////////////////////////////////////////////////////////////////////////////////

// Проверка корректности введеных данных для функции
// True - Данные введены корректно
function TParse.CheckFunct(Fcnt: TFunct): Boolean;
begin

    // Инициализация
    Result:=True;

    // Идентификатор функции
    if Fcnt.Id<0 then Result:=False;

    // Имя функции
    If Fcnt.Name='' then Result:=False;

    // Описание функции
    If Fcnt.Remark='' then Result:=False;

    // Параметры функции
    If (Fcnt.Params<0) or (Fcnt.Params>8) then Result:=False;
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Проверка корректности введенных данных для конструкции
// True - параметры заполненны верно
function TParse.CheckConstr(Cnstr: TTemplateConstr): Boolean;
var
  words: Integer;
begin

    // Инициализация
    Result:=True;
    words:=1;

    // Проверка параметров
    // Идентифкатор конструкции
    If Cnstr.Id<0 then Result:=False;

    // Описание конструкции
    If Cnstr.Remark='' then Result:=False;

    // Первое слово конструкции должно быть обязатель
    If Cnstr.Words[1]='' then Result:=False;

    // Второе слово
    If Cnstr.Words[2]<>'' then Inc(words);

    // Третье слово
    If (Cnstr.Words[3]<>'') and (words<2) then
    begin

      // Нарушена целостность
      Result:=False;
      Exit;
    end
    Else
    begin

      // Считаем слова
      Inc(words);
    end;

    // Четвертое слово
    If (Cnstr.Words[4]<>'') and (words<3) then
    begin

      // Нарушена целостность
      Result:=False;
      Exit;
    end
    Else
    begin

      // Считаем слова
      Inc(words);
    end;

    // Пятое слово
    If (Cnstr.Words[5]<>'') and (words<4) then
    begin

      // Нарушена целостность
      Result:=False;
      Exit;
    end
    Else
    begin

      // Считаем слова
      Inc(words);
    end;

    // Шестое слово
    If (Cnstr.Words[6]<>'') and (words<5) then
    begin

      // Нарушена целостность
      Result:=False;
      Exit;
    end
    Else
    begin

      // Считаем слова
      Inc(words);
    end;

    // Седьмое слово
    If (Cnstr.Words[7]<>'') and (words<6) then
    begin

      // Нарушена целостность
      Result:=False;
      Exit;
    end
    Else
    begin

      // Считаем слова
      Inc(words);
    end;

    // Восьмое слово
    If (Cnstr.Words[8]<>'') and (words<7) then
    begin

      // Нарушена целостность
      Result:=False;
    end;
end;
//////////////////////////////////////////////////////////////////////////////////////////////

// Распознование команды для секции Constr
procedure TParse.ParseConstr(a: UnicodeString; var Cnstr: TTemplateConstr);
var
  uk, z, code, b: Integer;
  name, value: UnicodeString;
begin

    // Ищем знак равно
    uk:=pos('=', a);

    // Знак есть?
    // Просто пропустим данную строку
    If uk<2 then Exit;

    // Сколько всего символов в строке
    z:=Utf8Length(a);

    // Получим команду и параметр
    name:=Utf8copy(a, 1, uk-1);
    if z=uk then
    begin

      // Параметр не установлен
      Exit;
    end
    Else
    begin

      // Получим параметр
      value:=utf8copy(a, uk+1, z-(uk));
    end;

    // Анализируем значения
    // Идентификатор конструкции
    if (name='ID') or (name='№') then
    begin

      // Внесем параметр
      val(value, b, Code);
      if Code<>0 then Exit;
      Cnstr.Id:=b;
    end;

    // Описание конструкции
    if (name='REMARK') or (Name='ОПИСАНИЕ') then
    begin

      // Внесем параметр
      Cnstr.Remark:=value;
      Exit;
    end;

    // Первое слово
    if (name='WORD1') or (name='СЛОВО1') then
    begin

      // Внесем параметр
      Cnstr.Words[1]:=value;
      Exit;
    end;

    // Второе слово
    if (name='WORD2') or (Name='СЛОВО2') then
    begin

      // Внесем параметр
      Cnstr.Words[2]:=value;
      Exit;
    end;

    // Третье слово
    if (name='WORD3') or (name='СЛОВО3') then
    begin

      // Внесем параметр
      Cnstr.Words[3]:=value;
      Exit;
    end;

    // Четвертое слово
    if (name='WORD4') or (name='СЛОВО4') then
    begin

      // Внесем параметр
      Cnstr.Words[4]:=value;
      Exit;
    end;

    // Пятое слово
    if (name='WORD5') or (name='СЛОВО5') then
    begin

      // Внесем параметр
      Cnstr.Words[5]:=value;
      Exit;
    end;

    // Шестое слово
    if (name='WORD6') or (name='СЛОВО6') then
    begin

      // Внесем параметр
      Cnstr.Words[6]:=value;
      Exit;
    end;

    // Седьмое слово
    if (name='WORD7') or (name='СЛОВО7') then
    begin

      // Внесем параметр
      Cnstr.Words[7]:=value;
      Exit;
    end;

    // Восьмое слово
    if (name='WORD8') or (name='СЛОВО8') then
    begin

      // Внесем параметр
      Cnstr.Words[8]:=value;
      Exit;
    end;
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Распознование команды для секции General
procedure TParse.ParseGeneral(a: UnicodeString);
var
  uk, z: Integer;
  name, value: UnicodeString;
begin

    // Ищем знак равно
    uk:=Utf8Pos('=', a);

    // Знак есть?
    // Просто пропустим данную строку
    If uk<2 then Exit;

    // Сколько всего символов в строке
    z:=Utf8Length(a);

    // Получим команду и параметр
    name:=(Utf8copy(a, 1, uk-1));
    if z=uk then
    begin

      // Параметр не установлен
      Exit;
    end
    Else
    begin

      // Получим параметр
      value:=Utf8copy(a, uk+1, z-(uk));
    end;

    // Анализируем значения
    // Имя
    If (name='NAME') or (name='ИМЯ') then
    begin

      // Внесем параметр
      Constr.Name:=value;
      Exit;
    end;

    // Описание
    if (name='REMARK') or (name='ОПИСАНИЕ') then
    begin

      // Внесем параметр
      Constr.Remark:=value;
      Exit;
    end;

    // Открывающая кавычка
    if (name='OPENQOUTE') or (name='ОТКРЫВАЮЩАЯ_КАВЫЧКА') then
    begin

      // Внесем параметр
      Constr.Open_qoute:=value;
      Exit;
    end;

    // Закрывающая кавычка
    if (name='CLOSEQOUTE') or (name='ЗАКРЫВАЮЩАЯ_КАВЫЧКА') then
    begin

      // Внесем параметр
      Constr.Close_qoute:=value;
      Exit;
    end;

    // Открывающая скобка
    if (name='OPENBRACKET') or (name='ОТКРЫВАЮЩАЯ_СКОБКА') then
    begin

      // Внесем параметр
      Constr.Open_bracket:=value;
      Exit;
    end;

    // Закрывающая скобка
    if (name='CLOSEBRACKET') or (name='ЗАКРЫВАЮЩАЯ_СКОБКА') then
    begin

      // Внесем параметр
      Constr.Close_bracket:=value;
      Exit;
    end;

    // Разделитель между параметрами
    if (name='DIVIDER') or (name='РАЗДЕЛИТЕЛЬ') then
    begin

      // Внесем параметр
      Constr.Divider:=value;
    end;

    // Независимые лексемы
    if (name='TOKEN') or (name='ТОКЕН') then
    begin

      // Добавляем токен
      AddToken2(value);
    end;
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Добавление независимой лексемы
procedure TParse.AddToken(Token: UnicodeString);
var
  count: Integer;
begin

    // Определяем количество независимых лексем
    count:=Length(Tokens);

    // Добавляем место под новую независимвую лексему
    SetLength(Tokens, count+1);

    // Записываем независимую лексему
    Tokens[count]:=Token;
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Распознование команды для секции Funct
procedure TParse.ParseFunct(a: UnicodeString; var Fcnt: TFunct);
var
  uk, z, code, b: Integer;
  name, value: UnicodeString;
begin

    // Ищем знак равно
    uk:=Utf8pos('=', a);

    // Знак есть?
    // Просто пропустим данную строку
    If uk<2 then Exit;

    // Сколько всего символов в строке
    z:=Utf8Length(a);

    // Получим команду и параметр
    name:=Utf8copy(a, 1, uk-1);
    if z=uk then
    begin

      // Параметр не установлен
      Exit;
    end
    Else
    begin

      // Получим параметр
      value:=Utf8copy(a, uk+1, z-(uk));
    end;

    // Анализируем значения
    If (name='NAME') or (name='ИМЯ') then
    begin

      // Внесем параметр
      Fcnt.Name:=value;
      Exit;
    end;

    if (name='ID') or (name='№') then
    begin

      // Внесем параметр
      val(value, b, Code);
      if Code<>0 then Exit;
      Fcnt.Id:=b;
    end;

    if (name='REMARK') or (name='ОПИСАНИЕ') then
    begin

      // Внесем параметр
      Fcnt.Remark:=value;
      Exit;
    end;

    if (name='PARAMS') or (name='ПАРАМЕТРЫ') then
    begin

      // Внесем параметр
      val(value, b, Code);
      if Code<>0 then Exit;
      Fcnt.Params:=b;
    end;
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Инициализация шаблонов
procedure TParse.ClearTemplate(var Cnstr: TTemplateConstr; var Fcnt:  TFunct);
begin

    // Очищаем поля конструкции
    Cnstr.Id:=0;
    Cnstr.Remark:='';
    Cnstr.Words[1]:='';
    Cnstr.Words[2]:='';
    Cnstr.Words[3]:='';
    Cnstr.Words[4]:='';
    Cnstr.Words[5]:='';
    Cnstr.Words[6]:='';
    Cnstr.Words[7]:='';
    Cnstr.Words[8]:='';

    // Очищаем поля функции
    Fcnt.Remark:='';
    Fcnt.Id:=0;
    Fcnt.Name:='';
    Fcnt.Params:=0;
end;
/////////////////////////////////////////////////////////////////////////////////////////////

// Определяет указана ли в строке секция
// a - входная строка
// 0 - секция General
// 1 - секция Constr
// 2 - секция Funct
// -1 - это не статус
function TParse.SectionStatus(a: UnicodeString): Integer;
var
  first: UnicodeString;
  z: Integer;
begin

         // Инициализация
         Result:=-1;

         // Первое слово
         z:=Utf8Pos(' ', a);
         if z>0 then
         begin

           // Получим слово
           First:=Utf8Trim(Utf8copy(a, 1, z));
         end
         Else
         begin

           // Вся строка состоит из одного слова
           First:=a;
         end;

         // Секция General?
         if (first='GENERAL') or (first='ОБЩИЕ')  then
         begin

           // Установим статус
           Result:=0;
           Exit;
         end;

         // Секция Constr?
         if (first='CONSTR') or (First='ШАБЛОН') then
         begin

           // Установим статус
           Result:=1;
           Exit;
         end;

         // Секция Funct?
         if (first='FUNCT') or (first='ФУНКЦИЯ') then
         begin

           // Установим статус
           Result:=2;
         end;
end;
//////////////////////////////////////////////////////////////////////////////////////////////

// Инициализация набора конструкций
procedure TParse.Init;
begin

    // Уничтожаем наборы
    SetLength(Constr.Funct, 0);
    SetLength(Constr.Constr, 0);
    SetLength(NonLex, 0);
    Setlength(Tokens, 0);
    Setlength(Remarks, 0);

    // Имя и параметры
    Constr.Name:='Default';
    Constr.Remark:='Default';
    Constr.Open_qoute:='''';
    Constr.Close_qoute:='''';
    Constr.Open_bracket:='(';
    Constr.Close_Bracket:=')';
    Constr.Divider:=',';
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Деструктор
destructor TParse.Destroy;
begin

    // Уничтожаем наборы
    Init;

    // Предок
    Inherited Destroy;
end;
////////////////////////////////////////////////////////////////////////////////////////////

// Конструктор
constructor TParse.Create;
begin

    // Предок
    Inherited Create;

    // Имя и параметры
    Init;
end;
////////////////////////////////////////////////////////////////////////////////////////////

end.

// Простая текстовая структура набора конструкций
{
   Набор конструкций представляет собой файл из набора секций
   Каждая следующая секция переключает на ввод параметров для нового значения
   Секция General - описание настроек набора
   В секции General описываются следующие параметры набора конструкций:
     Name - имя набора
     Remark - описание набора
     OpenQoute - открывающая кавычка (используется для описания текстовых констант)
     CloseQuote - закрывающая кавычка (используется для описания текстовых констант)
     OpenBracket - открывающая скобка
     CloseBracket - закрывающая скобка
     Divider - разделитель при перечислении параметров
     Token - лексема, которая в сплошном тексте должна рассматриваться как отдельный элемент
             Например: х-->х+1, лексему --> следует рассматривать отдельно. Тогда имеем: х, -->, х+1
                       Это не влияет на содержимое текстовых констант
             В Token через пробел возможно использование нескольких токенов в одной команде
   В секции Constr описывается одна отдельная конструкция языка
     Remark - Описание конструкции
     Id - Семнатический идентификатор конструкции
     Word1, word2...word8 - Слова-шаблоны для распознавания
   В секции Funct описывается одна отдельная функция языка
     Name - Имя функции
     Id - идентификатор функции
     Remark - Описание функции
     Params - Количество параметров функции
   Первоначально считается, что открыта секция General
   Секции заполненные не полностью будут проигнорированы

   ПРИМЕЧАНИЕ:
   1. Конструкции с идентификатором 0 являются незначащими лексемами
   и будут откидываться из начала строки

   2. Конструкции с идентификатором 1 являются однострочными комментариями
   и все что после них будет откидываться
   Вид данного комментария:  1 лексемы_комментария 2
                             1 - текст программы
                             2 - комментарий

   Если в слове конструкции указан номер параметра, то он считается единственно возможным вариантом



Семантаческие идентификаторы:
0 - Незначащая лексема
1 - Комментарий
2 - Описание системы
3 - Подключение внешних модулей
4 - Точка входа в программу
5 - Конструктор системы
6 - Секция интернационализации


}

