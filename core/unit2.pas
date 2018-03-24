unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form2: TForm2;

implementation

{$R *.frm}

{ TForm2 }

// Кнопка "В Файл"
procedure TForm2.Button1Click(Sender: TObject);
begin

     // Выбран файл для сохранения?
     If SaveDialog1.Execute Then
     begin

          Memo1.Lines.SaveToFile(SaveDialog1.FileName);
     end;
end;
///////////////////////////////////////////////////////////////////

end.

