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

type
  TFolderEplorerTreeView = class(TTreeView)
  private

  protected
    { Полный путь к папке }
    FRootFolderPath: String;
    { Список используемых иконок }
    FImageList: TImageList;

    { Установить полный путь к папке просмотра и сразу обновить контрол }
    procedure SetRootFolderPath(ARootFolderPath: String);

    { Установить путь для узла }
    procedure SetNodeFolderPath(AFolderPath: String; ANode: TTreeNode);

    { Установить иконку узла по имени файла desktop }
    procedure SetNodeIconByDesktop(ANode: TTreeNode; ADesktopFileName: AnsiString);

  public
    // Конструктор
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property RootFolderPath: String read FRootFolderPath write SetRootFolderPath;   

  end;

procedure Register;

implementation

uses
  strfunc, filefunc, exttypes, logfunc, desktopfile;

procedure Register;
begin
  {$I folder_explorer_treeview_icon.lrs}
  RegisterComponents('Misc',[TFolderEplorerTreeView]);
end;


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
end;


destructor TFolderEplorerTreeView.Destroy;
begin
  FreeAndNil(FImageList);
  inherited;
end;


{ Установить полный путь к папке просмотра и сразу обновить контрол }
procedure TFolderEplorerTreeView.SetRootFolderPath(ARootFolderPath: String);
begin
  if not DirectoryExists(ARootFolderPath) then
  begin
     logfunc.ErrorMsgFmt('Папка <%s> не найдена', [ARootFolderPath]);
     FRootFolderPath := '';
     Exit;
  end;

  FRootFolderPath := ARootFolderPath;

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
        self.SetNodeIconByDesktop(node, desktop_filename)
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
    if not strfunc.IsStartsWith(base_filename, '.') then
    begin
      // logfunc.InfoMsgFmt('Добавление файла <%s>', [filenames[i]]);
      node := Items.AddChild(ANode, base_filename);
      node.ImageIndex := FILE_IMG_INDEX;
    end;
  end;
  
end;


{ Установить иконку узла по имени файла desktop }
procedure TFolderEplorerTreeView.SetNodeIconByDesktop(ANode: TTreeNode; ADesktopFileName: AnsiString);
var
  desktop_file: TDesktopFile;
  bmp: TBitmap;
  icon_filename: AnsiString;
begin
  if not FileExists(ADesktopFileName) then
  begin
    logfunc.ErrorMsgFmt('Файл ярлыка <%s> не найден', [ADesktopFileName]);
    Exit;
  end;

  desktop_file := TDesktopFile.Create(ADesktopFileName);
  try
    icon_filename := desktop_file.GetIconFileName();
    if (not strfunc.IsEmptyStr(icon_filename)) and FileExists(icon_filename) then
    begin
      bmp := TBitmap.Create();
      try
        bmp.LoadFromFile(icon_filename);
        ANode.ImageIndex := FImageList.Add(bmp, nil);
      finally
        bmp.Free;  
      end;
    end
    else
      ANode.ImageIndex := NO_IMG_INDEX;
  finally
    desktop_file.Free;
  end;  
end;


initialization
  {$I default_item_images.lrs}

end.
