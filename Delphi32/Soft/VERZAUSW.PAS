{******************************************************************************}
{* Unit: Verzeichnis-Auswahl über Laufwerksliste und Verzeichnis-Listbox      *}
{* Version: 17.06.1997       WW                                               *}
{* !!! Nicht mehr verwenden -> Problem mit ENG/ENU-Version !!!                *}
{* !!! -> Stattdessen SelectDirectory verwenden !!! 08.11.2016, WW            *}
{******************************************************************************}
unit VerzAusw;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, FileCtrl, ExtCtrls;

type
  TVerzeichnisAuswahlDlg = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Bevel: TBevel;
    DirectoryListBox: TDirectoryListBox;
    DriveComboBox: TDriveComboBox;
    LaufwerkeLabel: TLabel;
    OrdnerLabel: TLabel;
    AktuOrdnerLabel: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VerzeichnisAuswahlDlg: TVerzeichnisAuswahlDlg;

implementation

{$R *.DFM}

end.
