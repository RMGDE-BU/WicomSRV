{------------------------------------------------------------------------------}
{ 12.04.1999 GD; Verzeichnis-Auswahl-Dialog                                    }
{                                                                              }
{ Copyright Karl Wieser GmbH 1999                                              }
{------------------------------------------------------------------------------}
unit FDirDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, FileCtrl;

type
  TFormChooseDirectory = class(TForm)
    DriveComboBox: TDriveComboBox;
    DirectoryListBox: TDirectoryListBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    bbtnOk: TBitBtn;
    bbtnCancel: TBitBtn;
  private
    { Private-Deklarationen }
    function GetDirectory: string;
    procedure SetDirectory(Value: string);
  public
    { Public-Deklarationen }
    property Directory: string read GetDirectory write SetDirectory;
  end;

var
  FormChooseDirectory: TFormChooseDirectory;

implementation

{$R *.DFM}

{---------------------------------------------}
function TFormChooseDirectory.GetDirectory: string;
{---------------------------------------------}
begin
  result:= DirectoryListBox.Directory;
  if result[Length(result)] <> '\' then result:= result + '\';
end;

{---------------------------------------------}
procedure TFormChooseDirectory.SetDirectory(Value: string);
{---------------------------------------------}
var
  s      : string;
begin
  s:= DirectoryListBox.Directory;
  try
    DirectoryListBox.Directory:= Value;
  except
    DirectoryListBox.Directory:= s;
  end;
end;

end.
