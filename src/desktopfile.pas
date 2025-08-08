{
Класс управления *.desktop файлами в Linux.

Версия: 0.0.0.1
}
unit desktopfile;

{$mode ObjFPC}{$H+}

interface

uses
  IniFiles;

const 
  DEFAULT_DESKTOP_ENTRY_SECTION = 'Desktop Entry';

type
  TDesktopFile = class(TIniFile)

  public
    { Имя файла иконки }
    function GetIconFileName(): String;
    { Команда выполнения }
    function GetExecCmd(): String;
    { Имя }
    function GetName(): String;
    { Комментарий }
    function GetComment(): String;

    { Кодовая страница }
    function GetEncoding(): String;
    
    property IconFileName: String read GetIconFileName;
    property ExecCmd: String read GetExecCmd;
    property Name: String read GetName;
    property Comment: String read GetComment;
    property Encoding: String read GetEncoding;

  end;


implementation


{ Имя файла иконки }
function TDesktopFile.GetIconFileName(): String;
begin
  Result := ReadString(DEFAULT_DESKTOP_ENTRY_SECTION, 'Icon', '');
end; 

{ Команда выполнения }
function TDesktopFile.GetExecCmd(): String;
begin
  Result := ReadString(DEFAULT_DESKTOP_ENTRY_SECTION, 'Exec', '');
end; 

{ Имя }
function TDesktopFile.GetName(): String;
begin
  Result := ReadString(DEFAULT_DESKTOP_ENTRY_SECTION, 'Name', '');
end; 

{ Комментарий }
function TDesktopFile.GetComment(): String;
begin
  Result := ReadString(DEFAULT_DESKTOP_ENTRY_SECTION, 'Comment', '');
end; 


{ Кодовая страница }
function TDesktopFile.GetEncoding(): String;
begin
  Result := ReadString(DEFAULT_DESKTOP_ENTRY_SECTION, 'Encoding', 'utf-8');
end; 


end.