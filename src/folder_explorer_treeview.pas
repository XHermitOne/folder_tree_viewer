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
  public
    // Конструктор
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property RootFolderPath: String read FRootFolderPath write SetRootFolderPath;   

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I folder_explorer_treeview_icon.lrs}
  RegisterComponents('Misc',[TFolderEplorerTreeView]);
end;


constructor TFolderEplorerTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FImageList := TImageList.Create(self);
  FImageList.AddLazarusResource('default_item_images');
  //FImageList.Handles := [NO_IMG_INDEX, FOLDER_IMG_INDEX]; // Индексы иконок для папок и файлов
  //FImageList.Images := [TBitmap.Create, TBitmap.Create]; // Создание битмапов для иконок
  // Загрузка иконок из файлов
  //FImageList.Images[NO_IMG_INDEX].LoadFromFile();
  //FImageList.Images[FOLDER_IMG_INDEX].LoadFromFile();  
  // Загрузка иконок из ресурса
  //FImageList.Images[NO_IMG_INDEX].LoadFromFile();
  //FImageList.Images[FOLDER_IMG_INDEX].LoadFromFile();  
  //FImageList.Images[NO_IMG_INDEX].LoadFromLazarusResource('image');
  // Назначение ImageList для TTreeView
  self.ImageList := FImageList;
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

  // Обновить древовидное содержание
  
end;

end.
