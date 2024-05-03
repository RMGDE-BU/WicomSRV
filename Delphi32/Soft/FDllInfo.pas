{------------------------------------------------------------------------------}
{ Info-Fenster für DLL-Versionen                                               }
{                                                                              }
{ 22.07.2001  GD  Neu                                                          }
{ 09.10.2004  GD  Datum jetzt sprachabhängig formatiert                        }
{ 23.03.2005  GD  Datei wird auf Vorhandensein geprüft                         }
{ 10.12.2013  WW  Versionsanzeige: Nebenversion u. Ausgabe durch Punkt getrennt}
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, 2013                                    }
{------------------------------------------------------------------------------}
unit FDllInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, Buttons, ExtCtrls,
  GD_Utils;

type
  TFormDllVersions = class(TForm)
    pnBottom: TPanel;
    pnClient: TPanel;
    bbtnOk: TBitBtn;
    Image: TImage;
    pnCaption: TPanel;
    pnListboxes: TPanel;
    lbBeschreibung: TListBox;
    lbVersion: TListBox;
    lbExeName: TListBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    procedure AddDll(sDllName: TFileName; sBeschreibung: string);
  end;

implementation

{$R *.DFM}

{----------------------------------------------------}
procedure TFormDllVersions.FormCreate(Sender: TObject);
{----------------------------------------------------}
begin
  pnCaption.Caption := Application.MainForm.Caption;
end;

{ Fügt DLL-Eintrag hinzu                             }
{ Parameter: DLL-Dateiname, -Beschreibung            }
{----------------------------------------------------}
procedure TFormDllVersions.AddDll(sDllName: TFileName; sBeschreibung: string);
{----------------------------------------------------}
begin
  if (lbBeschreibung.Items.Count > 0) then lbBeschreibung.Items.Add('');
  lbBeschreibung.Items.Add(sBeschreibung);
  if (lbExeName.Items.Count > 0) then lbExeName.Items.Add('');
  lbExeName.Items.Add(UpperCase(ChangeFileExt(ExtractFileName(sDllName), '')));
  if (lbVersion.Items.Count > 0) then lbVersion.Items.Add('');
  
  if (FileExists(sDllName))  // 23.03.2005
  then lbVersion.Items.Add(
      // Mehrstellige Unterversionen; 21.09.2015, WW
      IntToStr(HiWord(GetVersionInfo(sDllName).dwFileVersionMS)) + '.' +
      IntToStr(LoWord(GetVersionInfo(sDllName).dwFileVersionMS)) + '.' +  // 10.12.2013, WW
      IntToStr(HiWord(GetVersionInfo(sDllName).dwFileVersionLS)) + #9 +
      DateToStr(FileDateToDateTime(FileAge(sDllName))))
  else lbVersion.Items.Add('');
end;

end.
