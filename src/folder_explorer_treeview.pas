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
  filefunc, exttypes, logfunc;

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
    // logfunc.InfoMsgFmt('Добавление папки <%s>', [dirs[i]]);
    node := Items.AddChild(ANode, filefunc.GetBaseName(dirs[i]));
    node.ImageIndex := FOLDER_IMG_INDEX;
  end;

  // Файлы
  filenames := filefunc.GetFileNameList(AFolderPath);
  for i := 0 to Length(filenames) - 1 do
  begin
    // logfunc.InfoMsgFmt('Добавление файла <%s>', [filenames[i]]);
    node := Items.AddChild(ANode, filefunc.GetBaseName(filenames[i]));
    node.ImageIndex := FILE_IMG_INDEX;
  end;
  
end;


initialization
  {$I default_item_images.lrs}

end.
