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

  DEFAULT_SYS_ICON_GNOME_PATH = '/usr/share/icons/gnome/16x16';
  DEFAULT_SYS_PIXMAPS_PATH = '/usr/share/pixmaps';
  // В кеш системных иконок помещаем только PNG файлы
  DEFAULT_SYS_ICON_MATCH  = '*.png';

type
  TDesktopFile = class(TIniFile)

  public
    { Имя файла иконки }
    function GetIconFileName(): AnsiString;
    { Поиск системного файла иконки по ее имени }
    function FindSysIconFileName(const AIconName: AnsiString; AFindPaths: Array Of String): AnsiString;

    { Команда выполнения }
    function GetExecCmd(): AnsiString;
    { Выполнить }
    procedure Execute();

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


{ Заполнение кеша списка файлов системных иконок }
procedure CreateSysIconFileNameCache(AFindPaths: Array Of String);
{ Получить из кеша имя файла иконки по имени иконки }
function GetSysIconFileNameByName(const AIconName: AnsiString): AnsiString;
{ Очистить и удалить кеш списка файлов системных иконок }
procedure DestroySysIconFileNameCache();

implementation

uses
  FileUtil, LazFileUtils, Classes, 
  strfunc, exttypes, filefunc, logfunc, execfunc;

var 
  SYS_ICON_FILENAMES_CACHE: TStringList = nil;

{ Заполнение кеша списка файлов системных иконок }
procedure CreateSysIconFileNameCache(AFindPaths: Array Of String);
var
  i_path, i_filename: Integer;
  filenames: TStringList;
begin
  if SYS_ICON_FILENAMES_CACHE = nil then
    SYS_ICON_FILENAMES_CACHE := TStringList.Create();

  for i_path := 0 to Length(AFindPaths) - 1 do
  begin
    // logfunc.DebugMsgFmt('Поиск в папке <%s = %s>', [AIconName, AFindPaths[i_path]]);
    if DirectoryExists(AFindPaths[i_path]) then
    begin
      try
        // Здесь создается список имен файлов. Память выделяется динамически
        filenames := FileUtil.FindAllFiles(AFindPaths[i_path], DEFAULT_SYS_ICON_MATCH, True);

        for i_filename := 0 to filenames.Count - 1 do
        begin
          // logfunc.DebugMsgFmt('Проверка соответствия <%s/%s = %s - %s>', [AIconName, LazFileUtils.ExtractFileNameOnly(filenames[i_filename]), AFindPaths[i_path], filenames[i_filename]]);
          SYS_ICON_FILENAMES_CACHE.Add(filenames[i_filename]);
        end;
      finally
        // Освобождаем список имен файлов. Т.к. память выделялась динамически
        filenames.Destroy;
      end;
    end
    else
      logfunc.WarningMsgFmt('Не найдена папка поиска файлов системной иконки <%s>', [AFindPaths[i_path]]);
  end;
  // if strfunc.IsEmptyStr(Result) then logfunc.WarningMsgFmt('Не найден файл системной иконки <%s>', [AIconName]);
end; 

{ Получить из кеша имя файла иконки по имени иконки }
function GetSysIconFileNameByName(const AIconName: AnsiString): AnsiString;
var
  i: Integer;
begin
  if SYS_ICON_FILENAMES_CACHE <> nil then
  begin
    for i := 0 to SYS_ICON_FILENAMES_CACHE.Count - 1 do
    begin
      if LazFileUtils.ExtractFileNameOnly(SYS_ICON_FILENAMES_CACHE[i]) = AIconName then
      begin
        Result := SYS_ICON_FILENAMES_CACHE[i];
        break;
      end;
    end;
  end
  else
  begin
    logfunc.WarningMsgFmt('Не найден файл системной иконки <%s>', [AIconName]);
    Result := '';
  end;
end; 

{ Очистить и удалить кеш списка файлов системных иконок }
procedure DestroySysIconFileNameCache();
begin
  if SYS_ICON_FILENAMES_CACHE <> nil then
  begin
    SYS_ICON_FILENAMES_CACHE.Destroy;
  end;
end; 

{ Имя файла иконки. Получаем полное наименование файла иконки из .desktop файла }
function TDesktopFile.GetIconFileName(): AnsiString;
begin
  // Читаем ключ Icon из .desktop файла, который по сути является INI файлом
  Result := ReadString(DEFAULT_DESKTOP_ENTRY_SECTION, 'Icon', '');
  // Если иконка задается не полным именем файла, а коротким наименованием, то считается что это системная иконка
  if (not FileExists(Result)) and (not strfunc.IsWordInStr(PathDelim, Result)) then
  begin
    // Если кеш имен системных иконок не заполнен, то заполняем его
    if SYS_ICON_FILENAMES_CACHE = nil then
      CreateSysIconFileNameCache([DEFAULT_SYS_ICON_GNOME_PATH, DEFAULT_SYS_PIXMAPS_PATH]);
    // Полное имя файла системной иконки ищем уже в кеше по имени файла
    Result := GetSysIconFileNameByName(Result);
  end;
end; 

{ Поиск системного файла иконки по ее имени }
function TDesktopFile.FindSysIconFileName(const AIconName: AnsiString; AFindPaths: Array Of String): AnsiString;
var
  i_path, i_filename: Integer;
  filenames: TStringList;
begin
  Result := '';
  for i_path := 0 to Length(AFindPaths) - 1 do
  begin
    // logfunc.DebugMsgFmt('Поиск в папке <%s = %s>', [AIconName, AFindPaths[i_path]]);
    if DirectoryExists(AFindPaths[i_path]) then
    begin
      try
        // Здесь создается список имен файлов. Память выделяется динамически
        filenames := FileUtil.FindAllFiles(AFindPaths[i_path], DEFAULT_SYS_ICON_MATCH, True);

        for i_filename := 0 to filenames.Count - 1 do
        begin
          // logfunc.DebugMsgFmt('Проверка соответствия <%s/%s = %s - %s>', [AIconName, LazFileUtils.ExtractFileNameOnly(filenames[i_filename]), AFindPaths[i_path], filenames[i_filename]]);
          if LazFileUtils.ExtractFileNameOnly(filenames[i_filename]) = AIconName then
          begin
            Result := filenames[i_filename];
            break;
          end;
        end;
      finally
        // Освобождаем список имен файлов. Т.к. память выделялась динамически
        filenames.Destroy;
      end;
    end
    else
      logfunc.WarningMsgFmt('Не найдена папка поиска файлов системной иконки <%s>', [AFindPaths[i_path]]);
  end;
  // if strfunc.IsEmptyStr(Result) then logfunc.WarningMsgFmt('Не найден файл системной иконки <%s>', [AIconName]);
end;

{ Команда выполнения }
function TDesktopFile.GetExecCmd(): AnsiString;
begin
  Result := ReadString(DEFAULT_DESKTOP_ENTRY_SECTION, 'Exec', '');
end; 

procedure TDesktopFile.Execute();
var
  cmd: AnsiString;
begin
  cmd := self.GetExecCmd();
  if not strfunc.IsEmptyStr(cmd) then
  begin
    // перед запуском удаляем все управляющие символы
    cmd := StringReplace(cmd, '%f', '', [rfReplaceAll]); 
    cmd := StringReplace(cmd, '%F', '', [rfReplaceAll]); 
    cmd := StringReplace(cmd, '%u', '', [rfReplaceAll]); 
    cmd := StringReplace(cmd, '%U', '', [rfReplaceAll]); 
    // Запуск комманды системы
    execfunc.ExecuteSystem(cmd);
  end
  else
    logfunc.WarningMsgFmt('Не определена комманда ярлыка <%s>', [FileName]);
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
