{
@bold(Поиск утечек памяти)

Включение поиска утечек:
Меню Lazarus -> Проект -> Параметры проекта... ->
Параметры проекта -> Отладка -> Выставить галки для ключей -gl и -gh

Вывод делаем в текстовый файл *.mem в:

@longcode(#
***********************************************************
if UseHeapTrace then     // Test if reporting is on
   SetHeapTraceOutput(ChangeFileExt(ParamStr(0), '.mem'));
***********************************************************
#)

Допустим, имеем код, который заведомо без утечек:

@longcode(#
***********************************************************
uses heaptrc;
var
  p1, p2, p3: pointer;

begin
  getmem(p1, 100);
  getmem(p2, 200);
  getmem(p3, 300);

  // ...

  freemem(p3);
  freemem(p2);
  freemem(p1);
end.
***********************************************************
#)

, после запуска и завершения работы программы, в консоли наблюдаем отчет:

@longcode(#
***********************************************************
Running "f:\programs\pascal\tst.exe "
Heap dump by heaptrc unit
3 memory blocks allocated : 600/608
3 memory blocks freed     : 600/608
0 unfreed memory blocks : 0
True heap size : 163840 (80 used in System startup)
True free heap : 163760
***********************************************************
#)

Утечек нет, раз "0 unfreed memory blocks"
Теперь внесем утечку, "забудем" вернуть память выделенную под p2:

@longcode(#
***********************************************************
uses heaptrc;
var
  p1, p2, p3: pointer;

begin
  getmem(p1, 100);
  getmem(p2, 200);
  getmem(p3, 300);

  // ...

  freemem(p3);
  // freemem(p2);
  freemem(p1);
end.
***********************************************************
#)

и смотрим на результат:

@longcode(#
***********************************************************
Running "f:\programs\pascal\tst.exe "
Heap dump by heaptrc unit
3 memory blocks allocated : 600/608
2 memory blocks freed     : 400/408
1 unfreed memory blocks : 200
True heap size : 163840 (80 used in System startup)
True free heap : 163488
Should be : 163496
Call trace for block $0005D210 size 200
  $00408231
***********************************************************
#)

200 байт - утечка...
Если будешь компилировать еще и с ключом -gl,
то ко всему прочему получишь и место, где была выделена "утекающая" память.
}

program menu_folder_explorer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, menu_folder_epxlorer_form,
  { you can add units after this }
  Classes, CustApp, Mouse,
  SysUtils,
  logfunc, filefunc;

{$R *.res}
const
  // Версия программы
  VERSION: AnsiString = '0.0.1.1';

type

  { TUniAlarmCheckerApplication }

  TMenuFolderExplorerApplication = class(TApplication)
  protected
    FMainForm: TMainForm;

    procedure Init;
    // procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure WriteHelp;
begin
  { add your help code here }
  logfunc.PrintColorTxt('menu_folder_explorer - Программа просмотра папки в виде меню', logfunc.CYAN_COLOR_TEXT);
  logfunc.PrintColorTxt(Format('Версия: %s', [VERSION]), logfunc.CYAN_COLOR_TEXT);
  logfunc.PrintColorTxt('Парметры коммандной строки:', logfunc.CYAN_COLOR_TEXT);
  logfunc.PrintColorTxt('    Помощь: --help', logfunc.CYAN_COLOR_TEXT);
  logfunc.PrintColorTxt('    Версия программы: --version', CYAN_COLOR_TEXT);
  logfunc.PrintColorTxt('    Режим вывода сообщений в консоль: --debug', logfunc.CYAN_COLOR_TEXT);
  logfunc.PrintColorTxt('', logfunc.CYAN_COLOR_TEXT);
  logfunc.PrintColorTxt('    Позиция вывода окна (Если не указывается, то выводится по текущему положению мыши):', logfunc.CYAN_COLOR_TEXT);
  logfunc.PrintColorTxt('      --left=', logfunc.CYAN_COLOR_TEXT);
  logfunc.PrintColorTxt('      --top=', logfunc.CYAN_COLOR_TEXT);
  logfunc.PrintColorTxt('    Размер окна:', logfunc.CYAN_COLOR_TEXT);
  logfunc.PrintColorTxt('      --width=', logfunc.CYAN_COLOR_TEXT);
  logfunc.PrintColorTxt('      --height=', logfunc.CYAN_COLOR_TEXT);
  logfunc.PrintColorTxt('    Папка меню (Обязательный параметр):', logfunc.CYAN_COLOR_TEXT);
  logfunc.PrintColorTxt('      --folder=', logfunc.CYAN_COLOR_TEXT);
end;

procedure WriteVersion;
begin
  logfunc.PrintColorTxt(Format('menu_folder_explorer. Версия: %s', [VERSION]), logfunc.CYAN_COLOR_TEXT);
end;

procedure TMenuFolderExplorerApplication.Init;
var
  ErrorMsg: String;
  folder: AnsiString;
  left, top, width, height: Integer;
begin
  // Чтание параметров коммандной строки
  ErrorMsg := CheckOptions('hvdL:T:W:H:f:', 'help version debug left: top: width: height: folder:');
  if ErrorMsg <> '' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('v', 'version') then
  begin
    WriteVersion;
    Terminate;
    Exit;
  end;

  if HasOption('d', 'debug') then
    logfunc.DEBUG_MODE := True;

  if HasOption('f', 'folder') then
  begin
    folder := Trim(GetOptionValue('f', 'folder'));
    folder := filefunc.NormalPathFileName(folder);

    if not DirectoryExists(folder) then
    begin
      logfunc.ErrorMsgFmt('Папка меню <%s> не найдена', [folder], True);
      WriteHelp;
      Terminate;
      Exit;
    end;
  end
  else
  begin
    // Обязательный параметр
    logfunc.PrintColorTxt('Отсутствует обязательный параметр --folder=', logfunc.RED_COLOR_TEXT);
    WriteHelp;
    Terminate;
    Exit;
  end;

  try
    if HasOption('L', 'left') then
      left := StrToInt(GetOptionValue('L', 'left'))
    else
      left := Mouse.GetMouseX();

    if HasOption('T', 'top') then
      top := StrToInt(GetOptionValue('T', 'top'))
    else
      top := Mouse.GetMouseY();

    if HasOption('W', 'width') then
      width := StrToInt(GetOptionValue('W', 'width'))
    else
      width := 0;

    if HasOption('H', 'height') then
      height := StrToInt(GetOptionValue('H', 'height'))
    else
      height := 0;
  except
    logfunc.FatalMsg('Ошибка параметров коммандной строки');
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  logfunc.DebugMsg('Инициализация приложения');
  Initialize;

  logfunc.DebugMsg('Создание главной формы');
  FMainForm := TMainForm.Create(self);

  logfunc.DebugMsgFmt('Установка параметров главной формы. Позиция [%d x %d]. Размер [%d x %d]', [left, top, width, height]);
  if width > 0 then
    FMainForm.Width := width;
  if height > 0 then
    FMainForm.Height := height;
  if left > 0 then
    FMainForm.Left := left;
  if top > 0 then
    FMainForm.Top := top;

  logfunc.DebugMsgFmt('Установка папки меню <%s>', [folder]);
  FMainForm.SetExplorerFolder(folder);

  logfunc.DebugMsg('Открытие главной формы');
  FMainForm.Show();

end;

constructor TMenuFolderExplorerApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FMainForm := nil;
end;

destructor TMenuFolderExplorerApplication.Destroy;
begin
  if FMainForm <> nil then
    FMainForm.Destroy();

  inherited Destroy;
end;

var
  Application: TMenuFolderExplorerApplication;

begin

  // Запуск приложения
  Application := TMenuFolderExplorerApplication.Create(nil);
  Application.Scaled := True;
  Application.Title := 'MenuFolderExplorer';
  Application.Init;
  Application.Run;
  logfunc.DebugMsg('Выход из основного цикла обработки событий');
  Application.Free;

  // Учет утечек памяти. Вывод делаем в текстовый файл *.mem
  {$if declared(UseHeapTrace)}
  if UseHeapTrace then // Test if reporting is on
     SetHeapTraceOutput(ChangeFileExt(ParamStr(0), '.mem'));
  {$ifend}
end.

