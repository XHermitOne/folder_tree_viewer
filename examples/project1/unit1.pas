unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  folder_explorer_treeview;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    FolderEplorerTreeView1: TFolderEplorerTreeView;
    procedure Button1Click(Sender: TObject);
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
  FolderEplorerTreeView1.RootFolderPath := filefunc.JoinPath([filefunc.GetHomeDir(), 'tmp']);
end;

end.

