{------------------------------------------------------------------------------}
{ Sichten von Terminal(=ASCII)-Daten in DS901                                  }
{                                                                              }
{ 07.04.2007  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) RMG Messtechnik GmbH 2007                                      }
{------------------------------------------------------------------------------}
unit FMemoData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, FileCtrl, Buttons, Printers,
  FSubDef, GD_Utils;

type
  TFormMemoData = class(TFormSubWindowDefault)
    reMemoText: TRichEdit;
    pnClose: TPanel;
    bbtnClose: TBitBtn;
    pnRight3: TPanel;
    bbtnPrint: TBitBtn;
    pnAsciiExport: TPanel;
    bbtnAsciiExport: TBitBtn;
    pnRight5: TPanel;
    bbtnExcel: TBitBtn;
    pnRight6: TPanel;
    bbtnDelete: TBitBtn;
    procedure bbtnPrintClick(Sender: TObject);
    procedure bbtnAsciiExportClick(Sender: TObject);
    procedure bbtnExcelClick(Sender: TObject);
    procedure bbtnDeleteClick(Sender: TObject);
    procedure bbtnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
    FFileName    : string;
    FOrientation : TPrinterOrientation;
  protected
    { Protected-Deklarationen }
    procedure InitControls(bState: boolean); override;
    procedure SetCaptions; virtual;
    procedure CloseAction; virtual;
    procedure PrintAction; virtual;
    procedure AsciiExportAction; virtual;
    procedure ExcelAction; virtual;
    procedure DeleteAction; virtual;
  public
    { Public-Deklarationen }
    procedure ShowFile(const sFileName: string); virtual;
    property PrinterOrientation: TPrinterOrientation
      read FOrientation write FOrientation;
  end;

implementation

{$R *.dfm}

resourcestring
  S_Directory = 'Verzeichnis';
  S_FunctionNotAvailable =
    'Funktion ist in dieser Version noch nicht verfügbar';
  S_ConfirmFileDelete = 'Soll die Datei %s endgültig gelöscht werden?';
  S_FileNotFound  = 'Datei %s nicht gefunden!';
  S_FileSaved     = 'Datei(en) wurde gespeichert.';
  S_FileDeleted   = 'Datei(en) wurde gelöscht.';
  S_NoSuchFile    = 'Es sind keine weiteren Daten vorhanden.';
  S_All           = 'Alle';

{------------------------- Allgemeine Funktionen ------------------------------}

{--------------------------- TFormDS901MemoData -------------------------------}

{ Initialisieren bzw. Freigeben der Controls            }
{ Parameter: T=Initialisieren, T=Freigeben              }
{-------------------------------------------------------}
procedure TFormMemoData.InitControls(bState: boolean);
{-------------------------------------------------------}
begin
  if (bState) then begin
    FOrientation := poPortrait;
    FFileName := '';
  end
  else begin
  end;
end;

{-------------------------------------------------------}
procedure TFormMemoData.FormClose(Sender: TObject; var Action: TCloseAction);
{-------------------------------------------------------}
begin
  inherited;

  Action := caFree;
end;

{ Setzt Beschriftungen                                  }
{-------------------------------------------------------}
procedure TFormMemoData.SetCaptions;
{-------------------------------------------------------}
begin
  Self.Caption := FFileName;
end;

{ Zeigt übergebene Datei an                             }
{ Parameter: Dateiname                                  }
{-------------------------------------------------------}
procedure TFormMemoData.ShowFile(const sFileName: string);
{-------------------------------------------------------}
begin
  try
    FFileName := sFileName;
    if (FileExists(sFileName)) then begin
      reMemoText.Clear;
      reMemoText.Lines.LoadFromFile(sFileName);
    end
    else MessageDlg(Format(S_FileNotFound, [sFileName]), mtError, [mbOk], 0);
    SetCaptions;
  except
    on E:Exception do WriteErrorLog('TFormMemoData.ShowFile - ' +
      sFileName + ': ' + E.Message);
  end;
end;

{-------------------------------------------------------}
procedure TFormMemoData.bbtnCloseClick(Sender: TObject);
{-------------------------------------------------------}
begin
  bbtnClose.Enabled := False;
  Screen.Cursor := crHourGlass;
  try
    CloseAction;
  finally
    Screen.Cursor := crDefault;
    bbtnClose.Enabled := True;
  end;
end;

{-------------------------------------------------------}
procedure TFormMemoData.bbtnPrintClick(Sender: TObject);
{-------------------------------------------------------}
begin
  bbtnPrint.Enabled := False;
  Screen.Cursor := crHourGlass;
  try
    PrintAction;
  finally
    Screen.Cursor := crDefault;
    bbtnPrint.Enabled := True;
  end;
end;

{-------------------------------------------------------}
procedure TFormMemoData.bbtnAsciiExportClick(Sender: TObject);
{-------------------------------------------------------}
begin
  bbtnAsciiExport.Enabled := False;
  Screen.Cursor := crHourGlass;
  try
    AsciiExportAction;
  finally
    Screen.Cursor := crDefault;
    bbtnAsciiExport.Enabled := True;
  end;
end;

{-------------------------------------------------------}
procedure TFormMemoData.bbtnExcelClick(Sender: TObject);
{-------------------------------------------------------}
begin
  bbtnExcel.Enabled := False;
  Screen.Cursor := crHourGlass;
  try
    ExcelAction;
  finally
    Screen.Cursor := crDefault;
    bbtnExcel.Enabled := True;
  end;
end;

{-------------------------------------------------------}
procedure TFormMemoData.bbtnDeleteClick(Sender: TObject);
{-------------------------------------------------------}
begin
  bbtnDelete.Enabled := False;
  Screen.Cursor := crHourGlass;
  try
    DeleteAction;
  finally
    Screen.Cursor := crDefault;
    bbtnDelete.Enabled := True;
  end;
end;

{-------------------------------------------------------}
procedure TFormMemoData.CloseAction;
{-------------------------------------------------------}
begin
  Close;
end;

{-------------------------------------------------------}
procedure TFormMemoData.PrintAction;
{-------------------------------------------------------}
var
  i, iLeft, iLine : Integer;
begin
  with Printer do begin
    Orientation := FOrientation;
    iLeft := PageWidth div 15;
    BeginDoc;
    try
      Canvas.Font.Assign(reMemoText.Font);
      iLine := 0;
      for i := 0 to reMemoText.Lines.Count-1 do begin
        Canvas.TextOut(iLeft,
          iLeft + (iLine * Canvas.TextHeight(reMemoText.Lines.Strings[i])),
          reMemoText.Lines.Strings[i]);
        Inc(iLine);

        if (((iLine+1) * Canvas.TextHeight(reMemoText.Lines.Strings[i])) >
          PageHeight - (2*iLeft)) then
        begin
          NewPage;
          iLine := 0;
        end;
      end;
    finally
      EndDoc;
    end;
  end;
end;

{-------------------------------------------------------}
procedure TFormMemoData.AsciiExportAction;
{-------------------------------------------------------}
var
  s : string;
begin
  if (FileExists(FFileName)) and (SelectDirectory(S_Directory, '', s)) then
  begin
    reMemoText.Lines.SaveToFile(IncludeTrailingBackslash(s) +
      ExtractFileName(FFileName));
    MessageDlg(S_FileSaved, mtInformation, [mbOk], 0);
  end;
end;

{-------------------------------------------------------}
procedure TFormMemoData.ExcelAction;
{-------------------------------------------------------}
begin
  MessageDlg(S_FunctionNotAvailable, mtInformation, [mbOk], 0);
end;

{-------------------------------------------------------}
procedure TFormMemoData.DeleteAction;
{-------------------------------------------------------}
begin
  if (FileExists(FFileName)) and (MessageDlg(
    Format(S_ConfirmFileDelete, [FFileName]), mtConfirmation,
    [mbYes, mbCancel], 0) = mrYes) then
  begin
    DeleteFile(FFileName);
    MessageDlg(S_FileDeleted, mtInformation, [mbOk], 0);
    SetCaptions;
    Close;
  end;
end;

end.
