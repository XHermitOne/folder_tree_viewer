{
Функции выполнения внешних команд

Версия: 0.0.0.1
}
unit execfunc;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Process, FileUtil, StrUtils;

{
Запуск внешней программы
@param Command комманда запуска
}
procedure ExecuteSystem(Command: AnsiString);

implementation

uses
  strfunc, logfunc, exttypes;

{ Запуск внешней программы }
procedure ExecuteSystem(Command: AnsiString);
var
  cmd: TArrayOfString;
  i: Integer;
begin
  cmd := strfunc.SplitStr(Command, ' ');
  with TProcess.Create(nil) do
  try
    Executable := FindDefaultExecutablePath(cmd[0]);
    logfunc.DebugMsgFmt('Выполняющая программа <%s>', [Executable]);
    for i := 1 to Length(cmd) - 1 do
    begin
      Parameters.Add(Trim(cmd[i]));
      logfunc.DebugMsgFmt('%d. Параметр коммандной строки <%s>', [i, Trim(cmd[i])]);
    end;
    Execute;
  finally
    Free;
  end;
end;

end.

