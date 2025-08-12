{
Функции работы с файлами.

Версия: 0.0.5.1
}
unit filefunc;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF windows}
  Windows,
  {$ENDIF}
  Classes, SysUtils,
  LazFileUtils,
  sysfunc,
  strfunc,
  exttypes;

{ Определить папку домашней директории }
function GetHomeDir(): AnsiString;

{$IFDEF windows}
{ Домашняя папка в Windows системах }
function GetOSWindowsHomeDir(): AnsiString;
{$ELSE}
{ Домашняя папка в Linux системах }
function GetOSLinuxHomeDir(): AnsiString;
{$ENDIF}

{ Функция соединяет пути с учётом особенностей операционной системы }
function JoinPath(PathParts: Array Of String): AnsiString;

{ Функция разделяет путь на составляющие }
function SplitPath(sPath: AnsiString): TArrayOfString;

{ Создать весь путь папки }
function CreateDirPath(sPath: AnsiString): Boolean;
{ Создать весь путь папки. Путь должен быть нормализован. }
function CreateDirPathTree(sPath: AnsiString): Boolean;

{ Создать пустой файл }
function CreateEmptyFile(sPath: AnsiString): Boolean;

{ Создать пустой файл если он не существует }
function CreateEmptyFileIfNotExists(sPath: AnsiString): Boolean;

{ Нормализовать путь до файла }
function NormalPathFileName(sPath: AnsiString): AnsiString;

{
Чтение текстового файла как строки
@param sTxtFileName: Полное имя текстового файла.
@return: Текст, содержащийся внутри файла в виде строки
или пустая строка в случае ошибки.
}
function ReadTxtFile(sTxtFileName: AnsiString): AnsiString;

{
Запись текстового файла
@param sTxtFileName: Полное имя текстового файла.
@param sTxtContent: Записываемый текст.
@return: True - запись прошла успешно / False - ошибка записи.
}
function WriteTxtFile(sTxtFileName: AnsiString; sTxtContent: AnsiString): Boolean;

{ Преобразование Даты-времени }
{$IFDEF windows}
function DateTimeToFileTime(dtFileTime: TDateTime): TFileTime;
{$ENDIF}

{$IFDEF windows}
function FileTimeToDateTime(const ftFileTime: TFileTime): TDateTime;
{$ENDIF}


{ Определить полный путь до папки }
function GetDirName(aPath: AnsiString): AnsiString;

{ Определить базовое имя файла }
function GetBaseName(aPath: AnsiString): AnsiString;


{ Список каталогов в папке }
function GetDirList(APath: AnsiString; AFullPath: Boolean=True; ASort: Boolean = False): TArrayOfString;

{ Список файлов в папке }
function GetFileNameList(APath: AnsiString; AFullPath: Boolean=True; ASort: Boolean = False): TArrayOfString;

{ Список файлов в папке и всех подпапках }
function GetFileNameListCascade(APath: AnsiString; AFileNameMatch: AnsiString): TArrayOfString;

implementation

uses
  logfunc;

{
Определить папку домашней директории
}
function GetHomeDir(): AnsiString;
begin
  Result := '';
  {$IFDEF linux}
  Result := GetOSLinuxHomeDir();
  {$ENDIF}
  {$IFDEF windows}
  Result := GetOSWindowsHomeDir();
  {$ENDIF}

  if Result = '' then
    logfunc.WarningMsgFmt('Не поддерживаемая ОС <%s>', [GetOSType()]);
end;

{
Домашняя папка в Linux системах.
}
function GetOSLinuxHomeDir(): AnsiString;
begin
  Result := '';
  {$IFDEF linux}
  Result := GetEnvironmentVariable('HOME');
  {$ENDIF}
end;

{
Домашняя папка в Windows системах.
}
function GetOSWindowsHomeDir(): AnsiString;
begin
  Result := '';
  {$IFDEF windows}
  Result := GetAppConfigDir(False);
  {$ENDIF}
end;

{
Функция соединяет пути с учётом особенностей операционной системы.
}
function JoinPath(PathParts: Array Of String): AnsiString;
begin
  Result := JoinStr(PathParts, PathDelim);
end;

{
Функция разделяет путь на составляющие.
}
function SplitPath(sPath: AnsiString): TArrayOfString;
begin
  Result := SplitStr(sPath, PathDelim);
end;


{
Создать весь путь папки
}
function CreateDirPath(sPath: AnsiString): Boolean;
begin
  Result := False;

  // Нормализация пути
  sPath := NormalPathFileName(sPath);

  if not DirectoryExists(sPath) then
  begin
     logfunc.InfoMsgFmt('Создание папки <%s>', [sPath]);
     Result := CreateDirPathTree(sPath);
  end;
end;

{
Создать весь путь папки. Путь должен быть нормализован.
}
function CreateDirPathTree(sPath: AnsiString): Boolean;
var
  parent_path: AnsiString;
begin
  if not DirectoryExists(sPath) then
  begin
    parent_path := ExtractFileDir(sPath);
    if not DirectoryExists(parent_path) then
       Result := CreateDirPathTree(parent_path);
    CreateDir(sPath);
    Result := True;
    Exit;
  end;
  Result := False;
end;

{
Создать пустой файл.
}
function CreateEmptyFile(sPath: AnsiString): Boolean;
var
  file_tmp: Text;
begin
  // Нормализация пути
  sPath := NormalPathFileName(sPath);

  logfunc.InfoMsgFmt('Создание пустого файла <%s>', [sPath]);
  AssignFile(file_tmp, sPath);
  try
    Rewrite(file_tmp);
    Writeln(file_tmp, '');   //Remember AnsiStrings are case sensitive
    CloseFile(file_tmp);
    Result := True;
  except
    Result := False;
    CloseFile(file_tmp);
  end;
end;

{
Создать пустой файл если он не существует.
}
function CreateEmptyFileIfNotExists(sPath: AnsiString): Boolean;
begin
  Result := False;
  if not FileExists(sPath) then
    Result := CreateEmptyFile(sPath)
end;

{
Нормализовать путь до файла.
}
function NormalPathFileName(sPath: AnsiString): AnsiString;
begin
  // Замена двойных слешей
  sPath := StringReplace(sPath, PathDelim + PathDelim, PathDelim, [rfReplaceAll]);
  Result := ExpandFileName(sPath);
end;

{
Чтение текстового файла как строки
@param sTxtFileName: Полное имя текстового файла.
@return: Текст, содержащийся внутри файла в виде строки
или пустая строка в случае ошибки.
}
function ReadTxtFile(sTxtFileName: AnsiString): AnsiString;
var
  txt_file: file of Char;
  symbol: Char;

begin
  Result := '';
  if sTxtFileName = '' then
  begin
    logfunc.WarningMsg('Не определен текстовый файл');
    Exit;
  end;

  if not FileExists(sTxtFileName) then
  begin
    logfunc.WarningMsgFmt('Текстовый файл <%s> не найден', [sTxtFileName]);
    Exit;
  end;

  try
    Assign (txt_file, sTxtFileName);
    Reset(txt_file);

    while not eof(txt_file) do
    begin
      Read(txt_file, symbol);
      Result := Result + symbol;
    end;

    Close(txt_file);
  except
    Close(txt_file);
    logfunc.FatalMsgFmt('Ошибка чтения текстового файла <%s>', [sTxtFileName]);
    Result := '';
  end;
end;

{
Запись текстового файла
@param sTxtFileName: Полное имя текстового файла.
@param sTxtContent: Записываемый текст.
@return: True - запись прошла успешно / False - ошибка записи.
}
function WriteTxtFile(sTxtFileName: AnsiString; sTxtContent: AnsiString): Boolean;
var
  txt_file: TextFile;
  lines: TStringList;
  i_line: Integer;
begin
  Result := False;

  if sTxtFileName = '' then
  begin
    logfunc.WarningMsg('Не определен текстовый файл');
    Exit;
  end;

  try
    Assign (txt_file, sTxtFileName);
    Rewrite(txt_file);

    lines := strfunc.ParseStrLines(sTxtContent);
    for i_line := 0 to lines.Count - 1 do
      WriteLn(txt_file, lines[i_line]);
    lines.Destroy;

    Close(txt_file);
    Result := True;
  except
    Close(txt_file);
    logfunc.FatalMsgFmt('Ошибка записи текстового файла <%s>', [sTxtFileName]);
  end;
end;

{$IFDEF windows}
{
Преобразование Даты-времени
}
function DateTimeToFileTime(dtFileTime: TDateTime): TFileTime;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result.dwLowDateTime  := 0;
  Result.dwHighDateTime := 0;
  DateTimeToSystemTime(dtFileTime, SystemTime);
  //logfunc.DebugMsgFmt('System time: %d-%d-%d %d:%d:%d.%d', [SystemTime.Year, SystemTime.Month, SystemTime.Day,
  //                                                     SystemTime.Hour, SystemTime.Minute, SystemTime.Second, SystemTime.Millisecond]);
  SystemTimeToFileTime(SystemTime, LocalFileTime);
  //logfunc.DebugMsgFmt('Local file time: %s', [FormatDateTime('YYYY-MM-DD HH:NN:SS', FileTimeToDateTime(LocalFileTime))]);
  // Перевод к времени по гринвичу:
  //LocalFileTimeToFileTime(LocalFileTime, Ft);
  //logfunc.DebugMsgFmt('File time: %s', [FormatDateTime('YYYY-MM-DD HH:NN:SS', FileTimeToDateTime(Ft))]);
  Result := LocalFileTime;
end;
{$ENDIF}

{$IFDEF windows}
function FileTimeToDateTime(const ftFileTime: TFileTime): TDateTime;
const
  FileTimeBase = -109205.0;
  FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; // 100 nSek per Day
begin
  Result := Int64(ftFileTime) / FileTimeStep;
  Result := Result + FileTimeBase;
end;
{$ENDIF}

{ Определить полный путь до папки }
function GetDirName(aPath: AnsiString): AnsiString;
begin
  Result := ExtractFilePath(aPath);
end;

{ Определить базовое имя файла }
function GetBaseName(aPath: AnsiString): AnsiString;
begin
  Result := ExtractFileNameOnly(aPath) + ExtractFileExt(aPath);
end;

{ Список каталогов в папке }
function GetDirList(APath: AnsiString; AFullPath: Boolean=True; ASort: Boolean = False): TArrayOfString;
var
  search_rec: TSearchRec;
  i: Integer;
begin
  i := 0;
  // Только каталоги ------------------------V
  if FindFirst(JoinPath([APath, '*']), faDirectory, search_rec) = 0 then
  begin
    repeat
      if ((search_rec.Attr and faDirectory) <> 0) and (search_rec.Name <> '.') and (search_rec.Name <> '..') then
      begin
        SetLength(Result, i + 1);
        // logfunc.DebugMsgFmt('Формирование списка папок <%s : %s>', [APath, search_rec.Name]);
        if AFullPath then
          Result[i] := JoinPath([APath, search_rec.Name])
        else
          Result[i] := search_rec.Name;
        Inc(i);
      end;
    until FindNext(search_rec) <> 0;
    FindClose(search_rec);
  end;
end;

{ Список файлов в папке }
function GetFileNameList(APath: AnsiString; AFullPath: Boolean=True; ASort: Boolean = False): TArrayOfString;
var
  search_rec: TSearchRec;
  i: Integer;
begin
  i := 0;
  // Только файлы --------------------------V
  if FindFirst(JoinPath([APath, '*']), faAnyFile - faDirectory, search_rec) = 0 then
  begin
    repeat
      SetLength(Result, i + 1);
      // logfunc.DebugMsgFmt('Формирование списка файлов <%s : %s>', [APath, search_rec.Name]);
      if AFullPath then
        Result[i] := JoinPath([APath, search_rec.Name])
      else
        Result[i] := search_rec.Name;
      Inc(i);
    until FindNext(search_rec) <> 0;
    FindClose(search_rec);
  end;
end;


{ Список файлов в папке и всех подпапках }
function GetFileNameListCascade(APath: AnsiString; AFileNameMatch: AnsiString): TArrayOfString;
var
  search_rec: TSearchRec;
  i: Integer;
  sub_folder_list: TArrayOfString;
begin
  i := 0;
  if FindFirst(JoinPath([APath, AFileNameMatch]), faAnyFile, search_rec) = 0 then
  begin
    repeat
      if ((search_rec.Attr and faDirectory) <> 0) and (search_rec.Name <> '.') and (search_rec.Name <> '..') then
      begin
        // Обработка подпапок рекурсивно
        sub_folder_list := GetFileNameListCascade(JoinPath([APath, search_rec.Name]), AFileNameMatch);
        Result := strfunc.ConcatArrayOfString(Result, sub_folder_list);
        i := i + Length(sub_folder_list);
      end
      else
      begin
        SetLength(Result, i + 1);
        // logfunc.DebugMsgFmt('Формирование списка файлов <%s : %s>', [APath, search_rec.Name]);
        Result[i] := JoinPath([APath, search_rec.Name]);
        Inc(i);
      end;
    until FindNext(search_rec) <> 0;
    FindClose(search_rec);
  end;
end;

end.

