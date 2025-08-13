unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  folder_explorer_treeview;

type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    FolderEplorerTreeView1: TFolderEplorerTreeView;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SpeedButton1: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure FolderEplorerTreeView1DblClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses
  filefunc;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  // FolderEplorerTreeView1.RootFolderPath := filefunc.JoinPath([filefunc.GetHomeDir(), 'tmp']);
end;

procedure TForm1.FolderEplorerTreeView1DblClick(Sender: TObject);
begin
  // Запустить выбранный узел
  FolderEplorerTreeView1.ExecNode(FolderEplorerTreeView1.Selected);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
  begin
    Edit1.Text := SelectDirectoryDialog1.FileName;
    FolderEplorerTreeView1.RootFolderPath := SelectDirectoryDialog1.FileName;
  end;
end;

end.

