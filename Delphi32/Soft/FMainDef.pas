{------------------------------------------------------------------------------}
{ Default-Unit für Hauptfenster (B&B-System)                                   }
{                                                                              }
{ 24.01.2003  GD  Neu                                                          }
{ 20.11.2007  WW  resourcestrings                                              }
{ 22.03.2009  GD  Globale Fehlernehandlung                                     }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2003, RMG Messtechnik GmbH 2009               }
{------------------------------------------------------------------------------}
unit FMainDef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, StdActns, WDialogs, Menus, Buttons, ExtCtrls, ComCtrls, ShellApi;

type
  TFormMainWindowDefault = class(TForm)
    pnTop: TPanel;
    pnClient: TPanel;
    pnTopRest: TPanel;
    pnTop1: TPanel;
    sbtnClose: TSpeedButton;
    ActionList: TActionList;
    MainMenu: TMainMenu;
    DialogInfo: TDialogInfo;
    miProgram: TMenuItem;
    miClose: TMenuItem;
    miHelp: TMenuItem;
    miContent: TMenuItem;
    N1: TMenuItem;
    miInfo: TMenuItem;
    aHelpContents: THelpContents;
    aHelpInfo: TAction;
    StatusBar: TStatusBar;
    aClose: TAction;
    procedure aHelpInfoExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aCloseExecute(Sender: TObject);
    procedure aHelpContentsExecute(Sender: TObject);
  private
    { Private-Deklarationen }
    FLastError : string;
  protected
    { Protected-Deklarationen }
    procedure DisplayHint(Sender: TObject); virtual;
    procedure InitComponents(bState: boolean); virtual;
    procedure InitControls(bState: boolean); virtual;
    procedure MyErrorHandling(const sError: string); virtual;
    property LastError: string read FLastError write FLastError;
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.DFM}

resourcestring
  S_NotAvailable = 'Online-Hilfe nicht verfügbar !';
  S_Empty = '';


{-------------------------------------------------------}
procedure TFormMainWindowDefault.FormCreate(Sender: TObject);
{-------------------------------------------------------}
begin
  InitComponents(True);
  InitControls(True);
end;

{-------------------------------------------------------}
procedure TFormMainWindowDefault.FormDestroy(Sender: TObject);
{-------------------------------------------------------}
begin
  InitComponents(False);
  InitControls(False);
end;

{-------------------------------------------------------}
procedure TFormMainWindowDefault.aCloseExecute(Sender: TObject);
{-------------------------------------------------------}
begin
  Close;
end;

{-------------------------------------------------------}
procedure TFormMainWindowDefault.aHelpContentsExecute(Sender: TObject);
{-------------------------------------------------------}
begin
  if (FileExists(Application.HelpFile)) then Application.HelpContext(1)
  else if (FileExists(ChangeFileExt(ParamStr(0), '.PDF'))) then begin
    ShellExecute(Application.Handle, nil,
      PChar(ExtractFileName(ChangeFileExt(ParamStr(0), '.PDF'))), nil,
      PChar(ExtractFilePath(ParamStr(0))), SW_ShowNormal);
  end
  else MessageDlg(S_NotAvailable, mtInformation, [mbOk], 0);
end;

{-------------------------------------------------------}
procedure TFormMainWindowDefault.aHelpInfoExecute(Sender: TObject);
{-------------------------------------------------------}
begin
  DialogInfo.Execute;
end;

{-------------------------------------------------------}
procedure TFormMainWindowDefault.DisplayHint(Sender: TObject);
{-------------------------------------------------------}
begin
  StatusBar.SimpleText := GetLongHint(Application.Hint);
end;

{ Initialisieren bzw. Freigeben der Komponenten         }
{ Parameter: T=Initialisieren, T=Freigeben              }
{-------------------------------------------------------}
procedure TFormMainWindowDefault.InitComponents(bState: boolean);
{-------------------------------------------------------}
begin
  if (bState) then begin
    Application.OnHint := DisplayHint;
    Application.HelpFile := ChangeFileExt(ParamStr(0), '.HLP');
  end
  else begin
  end;
end;

{ Initialisieren bzw. Freigeben der Controls            }
{ Parameter: T=Initialisieren, T=Freigeben              }
{-------------------------------------------------------}
procedure TFormMainWindowDefault.InitControls(bState: boolean);
{-------------------------------------------------------}
begin
  if (bState) then begin
    sbtnClose.Caption := S_Empty;
  end
  else begin
  end;
end;

// Allgemeine Fehlerbehandlung
// Parameter: Fehlertext
//-----------------------------------------
procedure TFormMainWindowDefault.MyErrorHandling(const sError: string);
//-----------------------------------------
begin
  FLastError := sError;
end;

end.
