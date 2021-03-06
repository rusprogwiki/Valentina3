unit uin;

// Генерация и хранение уникальных идентификаторов

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// Класс создающий и отслеживающий неповторяющиеся идентификаторы
type
  TUin=class

      private
         Number: Integer;                       // Следующий доступный идентификатор
         Erased: Array of Integer;              // Список удаленных идентификаторов
      public

         constructor Create();                  // Конструктор
         destructor Destroy(); override;        // Деструктор
         function GetNewId(): Integer;          // Получение нового неповторяющегося идентификатора
         procedure AddErasedId(Id: Integer);    // Возврат ненужного идентификатора
         procedure Init();                      // Начальное состояние генератора
  end;
//////////////////////////////////////////////////////////////////////////////////////

implementation

// Начальное состояние генератора
procedure TUin.Init();
begin

    // Обнуляем данные
    SetLength(Erased, 0);
    Number:=0;
end;
//////////////////////////////////////////////////////////////////////////////////////

// Возврат ненужного идентификатора
procedure TUin.AddErasedId(Id: Integer);
var
  count: Integer;
begin

     // Количество идентификаторов
     Count:=length(Erased);
     SetLength(Erased, count+1);
     Erased[count]:=Id;
end;
//////////////////////////////////////////////////////////////////////////////////////

// Получение нового неповторяющегося идентификатора
function TUin.GetNewId(): Integer;
var
  count: Integer;
begin

    // Если среди удаленных идентификаторов?
    count:=Length(Erased);
    if count>0 then
    begin

         // Отдадим удаленный ранее идентификатор
         Dec(count);
         Result:=Erased[count];
         SetLength(Erased, count);
         Exit;
    end;

    // Новый идентификатор
    Result:=Number;
    Inc(Number);
end;
//////////////////////////////////////////////////////////////////////////////////////

// Деструктор
destructor TUin.Destroy();
begin

    // Поля
    SetLength(Erased, 0);

    // Предок
    Inherited Destroy;
end;
//////////////////////////////////////////////////////////////////////////////////////

// Конструктор
constructor TUin.Create();
begin

    // Предок
    Inherited Create;

    // Начальное значение
    Number:=0;

    // Удаленные идентификаторы
    SetLength(Erased, 0);
end;
//////////////////////////////////////////////////////////////////////////////////////

end.

