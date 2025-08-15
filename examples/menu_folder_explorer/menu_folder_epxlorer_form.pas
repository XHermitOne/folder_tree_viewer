unit menu_folder_epxlorer_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  folder_explorer_treeview;

type

  { TMainForm }

  TMainForm = class(TForm)
    FolderEplorerTreeView1: TFolderEplorerTreeView;
    procedure FolderEplorerTreeView1DblClick(Sender: TObject);
    procedure FolderEplorerTreeView1ShowHint(Sender: TObject;
      HintInfo: PHintInfo);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public
    { Установить путь к папке меню }
    procedure SetExplorerFolder(AFolderPath: AnsiString);

  end;

var
  MainForm: TMainForm;

implementation

uses
  logfunc;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FolderEplorerTreeView1DblClick(Sender: TObject);
begin
  // Запустить выбранный узел
  FolderEplorerTreeView1.ExecNode(FolderEplorerTreeView1.Selected);
end;

procedure TMainForm.FolderEplorerTreeView1ShowHint(Sender: TObject;
  HintInfo: PHintInfo);
begin
  FolderEplorerTreeView1.ShowNodeHint(HintInfo, Mouse.CursorPos);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Выйти из главного цикла обработки приложения
  if self.Owner <> nil then
    TApplication(self.Owner).Terminate;
end;

{ Установить путь к папке меню }
procedure TMainForm.SetExplorerFolder(AFolderPath: AnsiString);
begin
  // logfunc.DebugMsgFmt('Установка папки меню <%s>', [AFolderPath]);
  FolderEplorerTreeView1.RootFolderPath := AFolderPath;
  self.Caption := AFolderPath;
end;

end.

