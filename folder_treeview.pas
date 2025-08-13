{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit folder_treeview;

{$warn 5023 off : no warning about unused units}
interface

uses
  desktopfile, folder_explorer_treeview, exttypes, filefunc, logfunc, strfunc, 
  sysfunc, execfunc, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('folder_explorer_treeview', @folder_explorer_treeview.Register);
end;

initialization
  RegisterPackage('folder_treeview', @Register);
end.
