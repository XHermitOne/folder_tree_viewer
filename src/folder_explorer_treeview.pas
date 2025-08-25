{
Компонент просмотра содержимого папки в древовидном виде.

Версия: 0.0.0.1
}

unit folder_explorer_treeview;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls;

const
  FOLDER_IMG_INDEX = 1;
  NO_IMG_INDEX = 0;
  FILE_IMG_INDEX = 2;

  DEFAULT_DIRECTORY_DESKTOP_FILENAME = '.directory';
  DEFAULT_DESKTOP_FILE_EXT = '.desktop';

type
  TIconMode = (imCached, imGtk);

  { Класс элемента списка хранения всплывающих подсказок узлов }   
  TTreeNodeHintItem = class
    Name: AnsiString;
    Hint: AnsiString;
    NodeIndex: Integer;
  end;
 
  { Класс списка хранения всплывающих подсказок узлов } 
  TTreeNodeHintList = class(TList)

    destructor Destroy; override;

    function GetHint(ANodeIndex: Integer): AnsiString;
    function Add(AName, AHint: AnsiString; ANodeIndex: Integer): Integer;
  end;
 
  { Класс контрола дерева проводника }
  TFolderEplorerTreeView = class(TTreeView)
  private

  protected
    { Режим получения доступа к системным иконкам }
    FSysIconMode: TIconMode;

    { Полный путь к папке }
    FRootFolderPath: String;
    { Список используемых иконок }
    FImageList: TImageList;
    { Список всплывающих подсказок узлов }
    FNodeHintList: TTreeNodeHintList;

    { Установить полный путь к папке просмотра и сразу обновить контрол }
    procedure SetRootFolderPath(ARootFolderPath: String);

    { Установить путь для узла }
    procedure SetNodeFolderPath(AFolderPath: String; ANode: TTreeNode);

    { Установить иконку узла по имени файла desktop }
    procedure SetNodeByDesktop(ANode: TTreeNode; const ADesktopFileName: AnsiString; const ADefaultImgIndex: Integer);

    { Установить режим получения доступа к системным иконкам }
    procedure SetSysIconMode(ASysIconMode: TIconMode);

  public
    // Конструктор
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Проверка узла-ярлыка }
    function IsDesktopNode(ANode: TTreeNode): Boolean;
    { Проверка узла-папки }
    function IsFolderNode(ANode: TTreeNode): Boolean;

    { Запуск узла-ярлыка }
    procedure ExecDesktopNode(ANode: TTreeNode);
    { Запуск узла по ассоциации }
    procedure ExecAssociateNode(ANode: TTreeNode);
    { Запуск узла в зависимости от его типа }
    procedure ExecNode(ANode: TTreeNode);
    { Открыть в проводнике папку }
    procedure OpenExplorerFolderNode(ANode: TTreeNode);

    { Показать всплывающую подсказку узла }
    procedure ShowNodeHint(AHintInfo: PHintInfo; APosition: TPoint);

  published
    property RootFolderPath: String read FRootFolderPath write SetRootFolderPath;   

    property SysIconMode: TIconMode read FSysIconMode write SetSysIconMode;
  end;

procedure Register;

implementation

uses
  LazFileUtils,
  strfunc, filefunc, exttypes, logfunc, execfunc,
  desktopfile;

procedure Register;
begin
  {$I folder_explorer_treeview_icon.lrs}
  RegisterComponents('Misc',[TFolderEplorerTreeView]);
end;

{ === TTreeNodeHintList === }
function TTreeNodeHintList.GetHint(ANodeIndex: Integer): AnsiString;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to Count - 1 do
  begin
    // logfunc.DebugMsgFmt('Поиск подсказки в списке [%d : %d]', [ANodeIndex, TTreeNodeHintItem(Items[i]).NodeIndex]);
    if TTreeNodeHintItem(Items[i]).NodeIndex = ANodeIndex then
    begin
      Result := TTreeNodeHintItem(Items[i]).Hint;
      // logfunc.DebugMsgFmt('Получение подсказки <%d : %s>', [ANodeIndex, Result]);
      break;
    end;
  end;
end;
 
function TTreeNodeHintList.Add(AName, AHint: AnsiString; ANodeIndex: Integer): Integer;
var
  item: TTreeNodeHintItem;
begin
  // logfunc.DebugMsgFmt('Добавление подсказки <%s : %s : %d>', [AName, AHint, ANodeIndex]);
  item := TTreeNodeHintItem.Create;
  item.Name := AName;
  item.Hint := AHint;
  item.NodeIndex := ANodeIndex;
  Result := inherited Add(item);
end;

destructor TTreeNodeHintList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    TTreeNodeHintItem(Items[i]).Destroy;
  end;

  inherited;
end;

 
{ === TFolderEplorerTreeView === }
constructor TFolderEplorerTreeView.Create(AOwner: TComponent);
var
  bmp: TBitmap;
begin
  inherited Create(AOwner);

  FImageList := TImageList.Create(self);
  // FImageList.AddLazarusResource('default_item_images');
  //FImageList.Handles := [NO_IMG_INDEX, FOLDER_IMG_INDEX]; // Индексы иконок для папок и файлов
  // Загрузка битмапов для иконок из ресурса
  //FImageList.Images := [CreateBitmapFromLazarusResource('no_image'), 
  //                      CreateBitmapFromLazarusResource('folder')]; 
  bmp := TBitmap(CreateBitmapFromLazarusResource('no_image'));
  FImageList.Add(bmp, nil);
  bmp.Free;

  bmp := TBitmap(CreateBitmapFromLazarusResource('folder'));
  FImageList.Add(bmp, nil);
  bmp.Free; 

  bmp := TBitmap(CreateBitmapFromLazarusResource('document_empty'));
  FImageList.Add(bmp, nil);
  bmp.Free; 
  // Загрузка иконок из файлов
  //FImageList.Images[NO_IMG_INDEX].LoadFromFile();
  //FImageList.Images[FOLDER_IMG_INDEX].LoadFromFile();  
  // Загрузка иконок из ресурса
  //FImageList.Images[NO_IMG_INDEX].LoadFromFile();
  //FImageList.Images[FOLDER_IMG_INDEX].LoadFromFile();  
  //FImageList.Images[NO_IMG_INDEX].LoadFromLazarusResource('image');
  // Назначение ImageList для TTreeView
  self.Images := FImageList;

  // Инициализация списка всплывающих подсказок узлов
  FNodeHintList := TTreeNodeHintList.Create;
end;


destructor TFolderEplorerTreeView.Destroy;
begin
  FNodeHintList.Destroy;

  FreeAndNil(FImageList);

  if FSysIconMode = imCached then
    desktopfile.DestroySysIconFileNameCache();
  inherited;
end;


{ Установить режим получения доступа к системным иконкам }
procedure TFolderEplorerTreeView.SetSysIconMode(ASysIconMode: TIconMode);
begin
  if not (csDesigning in ComponentState) then
    // Если компонент не редактируется в IDE, то производим манипуляции с кешем
    if ASysIconMode = imCached then
      // Если режим кеширования, то создаем кеш
      desktopfile.CreateSysIconFileNameCache([desktopfile.DEFAULT_SYS_ICON_GNOME_PATH_16, desktopfile.DEFAULT_SYS_PIXMAPS_PATH])
    else
      // Не нужен кеш. Удаляем
      desktopfile.DestroySysIconFileNameCache();
  FSysIconMode := ASysIconMode;
end;


{ Установить полный путь к папке просмотра и сразу обновить контрол }
procedure TFolderEplorerTreeView.SetRootFolderPath(ARootFolderPath: String);
begin
  ARootFolderPath := filefunc.NormalPathFileName(ARootFolderPath);
  // ARootFolderPath := LazFileUtils.ExpandFileNameUTF8(ARootFolderPath);

  if not DirectoryExists(ARootFolderPath) then
  begin
     logfunc.ErrorMsgFmt('Папка <%s> не найдена', [ARootFolderPath]);
     FRootFolderPath := '';
     Exit;
  end;

  FRootFolderPath := ARootFolderPath;

  // Сначала полностью очищаем компонент дерева
  self.Items.Clear;

  SetNodeFolderPath(ARootFolderPath, nil);  
end;

{ Установить путь для узла }
procedure TFolderEplorerTreeView.SetNodeFolderPath(AFolderPath: String; ANode: TTreeNode);
var
  dirs: TArrayOfString;
  filenames: TArrayOfString;
  i: Integer;
  node: TTreeNode;
  base_filename: AnsiString;
  desktop_filename: AnsiString;
  desktop_file: TDesktopFile;
begin
  if not DirectoryExists(AFolderPath) then
  begin
     logfunc.ErrorMsgFmt('Папка <%s> не найдена', [AFolderPath]);
     Exit;
  end;

  // Обновить древовидное содержание
  // Папки
  dirs := filefunc.GetDirList(AFolderPath);
  for i := 0 to Length(dirs) - 1 do
  begin
    base_filename := filefunc.GetBaseName(dirs[i]);
    if not strfunc.IsStartsWith(base_filename, '.') then
    begin
      // logfunc.InfoMsgFmt('Добавление папки <%s>', [dirs[i]]);
      node := Items.AddChild(ANode, base_filename);
      desktop_filename := filefunc.JoinPath([dirs[i], DEFAULT_DIRECTORY_DESKTOP_FILENAME]);
      if FileExists(desktop_filename) then
        self.SetNodeByDesktop(node, desktop_filename, FOLDER_IMG_INDEX)
      else
        node.ImageIndex := FOLDER_IMG_INDEX;
      // Рекурсивно вызываем обработку подпапок
      self.SetNodeFolderPath(dirs[i], node);
    end;
  end;

  // Файлы
  filenames := filefunc.GetFileNameList(AFolderPath);
  for i := 0 to Length(filenames) - 1 do
  begin
    base_filename := filefunc.GetBaseName(filenames[i]);
    // logfunc.DebugMsgFmt('Проверка скрытых файлов <%s>', [base_filename]);
    if (not strfunc.IsEmptyStr(base_filename)) and (not strfunc.IsStartsWith(base_filename, '.')) then
    begin
      // logfunc.InfoMsgFmt('Добавление файла <%s>', [filenames[i]]);
      node := Items.AddChild(ANode, base_filename);
      if ExtractFileExt(base_filename) = DEFAULT_DESKTOP_FILE_EXT then
        self.SetNodeByDesktop(node, filenames[i], FILE_IMG_INDEX)
      else 
        node.ImageIndex := FILE_IMG_INDEX;
    end;
  end;
  
end;


{ Установить иконку узла по имени файла desktop }
procedure TFolderEplorerTreeView.SetNodeByDesktop(ANode: TTreeNode; const ADesktopFileName: AnsiString; const ADefaultImgIndex: Integer);
var
  desktop_file: TDesktopFile;
  bmp: TBitmap;
  icon_filename: AnsiString;
  comment: AnsiString;
begin
  if not FileExists(ADesktopFileName) then
  begin
    logfunc.ErrorMsgFmt('Файл ярлыка <%s> не найден', [ADesktopFileName]);
    Exit;
  end;

  desktop_file := TDesktopFile.Create(ADesktopFileName);
  try
    icon_filename := desktop_file.GetIconFileName(FSysIconMode = imCached);

    if (not strfunc.IsEmptyStr(icon_filename)) and (ExtractFileExt(icon_filename) <> '.png') then
    begin
      logfunc.WarningMsgFmt('Не поддерживаемый формат иконки <%s>', [icon_filename]);
      ANode.ImageIndex := ADefaultImgIndex;
    end
    else if strfunc.IsEmptyStr(icon_filename) then
      // Файл иконки просто не определен
      ANode.ImageIndex := ADefaultImgIndex
    else if FileExists(icon_filename) then
    begin
      bmp := TBitmap.Create();
      try
        logfunc.DebugMsgFmt('Файл иконки <%s>', [icon_filename]);

        // Пример загрузки файла PNG в TBitmap
        with TPicture.Create do
        try
          LoadFromFile(icon_filename);
          bmp.Assign(Graphic);
        finally
          Free;
        end;

        // Устанавливаем уконку узла дерева 
        ANode.ImageIndex := FImageList.Add(bmp, nil);
      finally
        bmp.Free;  
      end;
    end
    else
    begin
      logfunc.WarningMsgFmt('Файл иконки <%s> не найден', [icon_filename]);
      ANode.ImageIndex := ADefaultImgIndex;
    end;

    comment := desktop_file.GetComment();
    // Установить всплывающую подсказку как комментарий
    if ShowHint and (not strfunc.IsEmptyStr(comment)) then
      FNodeHintList.Add(ANode.Text, comment, ANode.AbsoluteIndex);
  finally
    desktop_file.Free;
  end;  
end;

{ Проверка узла-ярлыка }
function TFolderEplorerTreeView.IsDesktopNode(ANode: TTreeNode): Boolean;
begin
  Result := ExtractFileExt(ANode.Text) = DEFAULT_DESKTOP_FILE_EXT
end;

{ Запуск узла-ярлыка }
procedure TFolderEplorerTreeView.ExecDesktopNode(ANode: TTreeNode);
var 
  desktop_filename: AnsiString;
  desktop_file: TDesktopFile;
begin
  desktop_filename := filefunc.JoinPath([FRootFolderPath, ANode.GetTextPath()]);
  if FileExists(desktop_filename) then
  begin
    desktop_file := TDesktopFile.Create(desktop_filename);
    try
      desktop_file.Execute();
    finally
      desktop_file.Free;
    end;    
  end
  else
    logfunc.WarningMsgFmt('Файл ярлыка <%s> не найден', [desktop_filename]);  
end;

{ Запуск узла по ассоциации }
procedure TFolderEplorerTreeView.ExecAssociateNode(ANode: TTreeNode);
var
  node_path, cmd: AnsiString;
begin
  node_path := filefunc.JoinPath([FRootFolderPath, ANode.GetTextPath()]);

  {$IFDEF linux}
  cmd := Format('xdg-open %s', [node_path]);
  execfunc.ExecuteSystem(cmd); 
  {$ENDIF}
  {$IFDEF windows}
  cmd := Format('start "%s"', [node_path]);
  execfunc.ExecuteSystem(cmd); 
  {$ENDIF}  
end;

{ Запуск узла в зависимости от его типа }
procedure TFolderEplorerTreeView.ExecNode(ANode: TTreeNode);
begin
  if ANode = nil then
    Exit;
  if IsDesktopNode(ANode) then
    // Если это файл ярлыка .desktop, тогда берем из него строку запуска и выполняем
    ExecDesktopNode(ANode)
  else if IsFolderNode(ANode) then
    // Если это папка, тогда открываем ее в проводнике
    OpenExplorerFolderNode(ANode)
  else
    // Во всех других случаях запускаем программу-ассоциацию файла
    ExecAssociateNode(ANode);
end;


{ Проверка узла-папки }
function TFolderEplorerTreeView.IsFolderNode(ANode: TTreeNode): Boolean;
var
  node_path: AnsiString;
begin
  node_path := filefunc.JoinPath([FRootFolderPath, ANode.GetTextPath()]);
  Result := DirectoryExists(node_path);
end;


{ Открыть в проводнике папку }
procedure TFolderEplorerTreeView.OpenExplorerFolderNode(ANode: TTreeNode);
var
  node_path, cmd: AnsiString;
begin
  node_path := filefunc.JoinPath([FRootFolderPath, ANode.GetTextPath()]);
  {$IFDEF linux}
  cmd := Format('xdg-open %s', [node_path]);
  execfunc.ExecuteSystem(cmd); 
  {$ENDIF}
  {$IFDEF windows}
  cmd := Format('explorer.exe "%s"', [node_path]);
  execfunc.ExecuteSystem(cmd); 
  {$ENDIF}  
end;


{ Показать всплывающую подсказку узла }
procedure TFolderEplorerTreeView.ShowNodeHint(AHintInfo: PHintInfo; APosition: TPoint);
var
  node: TTreeNode;
  hint_str: AnsiString;
begin
  // Если отключено отбражение всплывающей подсказки узлов, то просто выходим
  if not ShowHint then
  begin
    // logfunc.WarningMsg('Отключено отображение всплывающей подсказки узлов');
    Exit;
  end;

  // Если позиция вывода всплывающей подсказки не определена, то определяем по позиции курсора мыши
  if (APosition.X = 0) and (APosition.Y = 0) then
    APosition := Mouse.CursorPos;
  APosition := self.ScreenToClient(APosition);
  // Определяем объект узла по координатам
  node := self.GetNodeAt(APosition.X, APosition.Y);
  // Если какойто узел соответствует координатам
  if node <> nil then
  begin
    // то заполняем параметры отображения всплывающей подсказки
    hint_str := FNodeHintList.GetHint(node.AbsoluteIndex);
    // logfunc.DebugMsgFmt('Подсказка <%s>', [hint_str]);
    AHintInfo^.HintStr := hint_str;
    AHintInfo^.ReshowTimeout := 100;
  end;
end;


initialization
  {$I default_item_images.lrs}

end.
