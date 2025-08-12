{
Класс управления *.desktop файлами в Linux.

Версия: 0.0.0.1
}
unit desktopfile;

{$mode ObjFPC}{$H+}

interface

uses
  IniFiles, SysUtils;

const 
  DEFAULT_DESKTOP_ENTRY_SECTION = 'Desktop Entry';

  DEFAULT_SYS_ICON_GNOME_PATH = '/usr/share/icons/gnome/16x16/';
  DEFAULT_SYS_PIXMAPS_PATH = '/usr/share/pixmaps/';
  DEFAULT_SYS_ICON_MATCH  = '*.png';

type
  TDesktopFile = class(TIniFile)

  public
    { Имя файла иконки }
    function GetIconFileName(): AnsiString;
    { Поиск системного файла иконки по ее имени }
    function FindSysIconFileName(AIconName: AnsiString; AFindPaths: Array Of String): AnsiString;

    { Команда выполнения }
    function GetExecCmd(): AnsiString;
    { Имя }
    function GetName(): AnsiString;
    { Комментарий }
    function GetComment(): AnsiString;

    { Кодовая страница }
    function GetEncoding(): String;
    
    property IconFileName: AnsiString read GetIconFileName;
    property ExecCmd: AnsiString read GetExecCmd;
    property Name: AnsiString read GetName;
    property Comment: AnsiString read GetComment;
    property Encoding: String read GetEncoding;

  end;


implementation

uses
  strfunc, exttypes, filefunc, logfunc;

{ Имя файла иконки }
function TDesktopFile.GetIconFileName(): AnsiString;
begin
  Result := ReadString(DEFAULT_DESKTOP_ENTRY_SECTION, 'Icon', '');
  // Иконка задается не полным именем файла, а коротким наименованием системной иконки
  if (not FileExists(Result)) and (not strfunc.IsWordInStr(PathDelim, Result)) then
  begin
    Result := self.FindSysIconFileName(Result, [DEFAULT_SYS_ICON_GNOME_PATH, DEFAULT_SYS_PIXMAPS_PATH]);
  end;
end; 

{ Поиск системного файла иконки по ее имени }
function TDesktopFile.FindSysIconFileName(AIconName: AnsiString; AFindPaths: Array Of String): AnsiString;
var
  i_path, i_filename: Integer;
  filenames: TArrayOfString;
begin
  Result := '';
  for i_path := 0 to Length(AFindPaths) - 1 do
    if DirectoryExists(AFindPaths[i_path]) then
    begin
      filenames := filefunc.GetFileNameListCascade(AFindPaths[i_path], DEFAULT_SYS_ICON_MATCH);
      for i_filename := 0 to Length(filenames) - 1 do
        if filefunc.GetBaseName(filenames[i_filename]) = AIconName then
        begin
          Result := filenames[i_filename];
          Exit;
        end;
    end;
  if strfunc.IsEmptyStr(Result) then
    logfunc.WarningMsgFmt('Не найден файл системной иконки <%s>', [AIconName]);
end;

{ Команда выполнения }
function TDesktopFile.GetExecCmd(): AnsiString;
begin
  Result := ReadString(DEFAULT_DESKTOP_ENTRY_SECTION, 'Exec', '');
end; 

{ Имя }
function TDesktopFile.GetName(): AnsiString;
begin
  Result := ReadString(DEFAULT_DESKTOP_ENTRY_SECTION, 'Name', '');
end; 

{ Комментарий }
function TDesktopFile.GetComment(): AnsiString;
begin
  Result := ReadString(DEFAULT_DESKTOP_ENTRY_SECTION, 'Comment', '');
end; 


{ Кодовая страница }
function TDesktopFile.GetEncoding(): String;
begin
  Result := ReadString(DEFAULT_DESKTOP_ENTRY_SECTION, 'Encoding', 'utf-8');
end; 


end.
