{------------------------------------------------------------------------------}
{ Utilities für COM/DCOM                                                       }
{                                                                              }
{ 13.11.2000  GD  Neu                                                          }
{ 06.10.2001  GD  Erweitert um OpenWordDocument                                }
{ 09.12.2001  GD  Erweitert um 'Abbrechen' bei Excel                           }
{ 08.03.2002  GD  Ausgabe aller Tabellen in einer Excel-Instanz                }
{ 14.03.2002  GD  Ausgabe einer Excel-Tabelle als Stringliste                  }
{ 21.05.2002  GD  Erweiterung um Ausgabe von Excel-Tabellen-Namen              }
{ 10.06.2002  GD  Automatische Spaltenbreiten bei Exelexport                   }
{ 24.10.2002  GD  Exelexport einer Datei                                       }
{ 05.01.2005  GD  Modifizierungen an TComInfoWindow                            }
{ 25.04.2005  GD  TComInfoWindow: automatische Breitenanpassung an Infotext    }
{ 30.11.2005  GD  Neu: TInfoThread                                             }
{ 26.10.2007  WW  resourcestrings                                              }
{ 13.02.2008  GD  Spaltenanzahl beim Einlesen von Exceltabellen erweitert      }
{ 07.11.2008  GD  InfoForm: Panel in Label umgewidmet für mehrzeilige Ausgabe  }
{ 31.03.2011  WN  Excel-Sheet öffnen                                           }
{ 24.04.2013  WW  InsertToExcel: Bugfix verbotene Zeichen für Sheetname, CSV-  }
{                 Ausgabe                                                      }
{ 05.07.2013  GD  Excel öffnen und auf Benden warten: OpenWorkSheetAndWait     }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, RMG Messtechnik GmbH 2008, 2013         }
{------------------------------------------------------------------------------}
unit COM_Utils;

interface

uses
  Windows, Forms, Graphics, Classes, SysUtils, ComObj, Controls, ExtCtrls,
  Buttons, StdCtrls, Dialogs,
  GD_Utils, T_Zeit;

resourcestring
  S_Export1 = 'Export in Excel';
  S_Export2 = 'Exportiere %d Einträge';
  S_Export3 = 'Export in Excel - %d Einträge';
  S_Export4 = 'Exportiere %d von %d';
  S_Export5 = 'Formatiere Spalte %d für Ausgabetext';
  S_ExportErr = 'Fehler beim Excel-Export:';

  S_Import1 = 'Import aus Excel';
  S_Import2 = 'Importiere %d Einträge';
  S_Import3 = 'Import aus Excel - %d Einträge';
  S_Import4 = 'Importiere %d von %d';

type
  TComInfoWindow = class(TCustomForm)
    constructor CreateInfoWindow(sCaption: string = '');
  private
    FPanel       : TLabel;   // 07.11.2008
    FStop        : boolean;
    FLabel       : TPanel;
    FBtn         : TBitBtn;
    FMemMaxWidth : boolean;
    procedure StopClick(Sender: TObject);
  protected
    procedure SetStop(bValue: boolean); virtual;
    procedure SetEnableStop(bValue: boolean); virtual;
  public
    procedure SetInfoText(sText: string);
    procedure SetInfoCaption(sText: string);
    property Stoped: boolean read FStop write SetStop;
    property EnableStop: boolean write SetEnableStop;
    property CaptionControl: TPanel read FLabel;
    property TextControl: TLabel read FPanel;
    property MemMaxWidth : boolean write FMemMaxWidth;
  end;

  TInfoThread = class(TThread)
    constructor Create(const sCaption: string = '');
    destructor Destroy; override;
  private
    FForm : TComInfoWindow;
    function GetStop: boolean;
    procedure SetStop(bValue: boolean);
    procedure SetEnableStop(bValue: boolean);
  protected
    procedure Execute; override;
  public
    procedure SetInfoText(sText: string);
    procedure SetInfoCaption(sText: string);
    property Stoped: boolean read GetStop write SetStop;
    property EnableStop: boolean write SetEnableStop;
  end;

function InsertToExcel(pSl: TStrings; cSeparator: char; iRow, iCol: integer;
  bNewInstance: boolean = False; bShowFortschritt: boolean = True;
  bShowCaption: boolean = False; pInfoForm: TComInfoWindow = nil;
  sBookName: string = ''; sSavePath: string = '';
  bSaveAsCSV: boolean = False; bQuit: boolean = False): boolean; overload;
function InsertToExcel(pSl: TStrings; cSeparator: char; iRow, iCol: integer;
  vExcel: Variant; bShowCaption: boolean = False; pInfoForm: TComInfoWindow = nil;
  sBookName: string = ''): boolean; overload;
function InsertFileToExcel(sFile: TFilename; cSeparator: char; iRow, iCol: integer;
  bNewInstance: boolean = False; bShowFortschritt: boolean = True;  // 24.10.2002
  bShowCaption: boolean = False; pInfoForm: TComInfoWindow = nil): boolean;
function OpenWordDocument(
  sDoc: TFileName; sMakro: string = ''; sParam: string = ''): boolean;
function GetWorkSheetAsStringList(
  sFileName, sSheetName: string; bShowFortschritt: boolean = True): TStrings;
function GetWorkSheetNames(sFileName: string): TStrings;  // 21.05.2002
function OpenWorkBook(sFileName: string): boolean;   // 05.07.2013
function OpenWorkSheet(sFileName, sSheetName: String): boolean;   // 31.03.2011  WN
function OpenWorkSheetAndWait(sFileName, sSheetName: string): boolean;  // 05.07.2013

implementation

const
  C_InfoWindow_Width = 250;
  C_ExcelCaption_MaxLength = 31;

{-------------------------------- TInfoThread ---------------------------------}

{-----------------------------------------}
constructor TInfoThread.Create(const sCaption: string = '');
{-----------------------------------------}
begin
  inherited Create(True);

  FForm := TComInfoWindow.CreateInfoWindow(sCaption);
  FForm.Show;

  FreeOnTerminate := False;  // Thread soll sich beim Beenden selbst freigeben
  Priority := tpNormal;     // Thread hat normale Priorität
  Suspended := False;       // Thread fortsetzen
end;

{-----------------------------------------}
destructor TInfoThread.Destroy;
{-----------------------------------------}
begin
  FreeAndNil(FForm);

  inherited Destroy;
end;

{-----------------------------------------}
procedure TInfoThread.SetStop(bValue: boolean);
{-----------------------------------------}
begin
  FForm.Stoped := bValue;
end;

{-----------------------------------------}
function TInfoThread.GetStop: boolean;
{-----------------------------------------}
begin
  Result := FForm.Stoped;
end;

{-----------------------------------------}
procedure TInfoThread.SetEnableStop(bValue: boolean);
{-----------------------------------------}
begin
  FForm.EnableStop := bValue;
end;

{-----------------------------------------}
procedure TInfoThread.SetInfoText(sText: string);
{-----------------------------------------}
begin
  FForm.SetInfoText(sText);
end;

{-----------------------------------------}
procedure TInfoThread.SetInfoCaption(sText: string);
{-----------------------------------------}
begin
  FForm.SetInfoCaption(sText);
end;

{-----------------------------------------}
procedure TInfoThread.Execute;
{-----------------------------------------}
begin
  while (not Terminated) do Delay(1);
end;

{------------------------------ TComInfoWindow --------------------------------}

{-----------------------------------------}
constructor TComInfoWindow.CreateInfoWindow(sCaption: string = '');
{-----------------------------------------}
const
  C_Wnd_Height = 100;
var
  iBtnHeight : integer;
  pPn1, pPn2 : TPanel;
begin
  inherited CreateNew(nil);

  FormStyle := fsStayOnTop;
  BorderStyle := bsNone;
  Position := poScreenCenter;
  Caption := sCaption;
  Height := C_Wnd_Height;
  Width := C_InfoWindow_Width;
  FMemMaxWidth := False;

  FStop := False;

  pPn1 := TPanel.Create(Self);
  with pPn1 do begin
    Parent := Self;
    Align := alClient;
    Caption := '';
    BevelInner := bvRaised;
    BevelOuter := bvLowered;
  end;

  FLabel := TPanel.Create(Self);
  with FLabel do begin
    Parent := pPn1;
    Align := alTop;
    Height := GetSystemMetrics(SM_CYSIZE);
    Color := clActiveCaption;
    Font.Color := clCaptionText;
    Font.Style := [fsBold];
    Caption := Self.Caption;
    BevelInner := bvNone;
    BevelOuter := bvNone;
  end;

  FBtn := TBitBtn.Create(Self);
  with FBtn do begin
    Parent := pPn1;
    Kind := bkCancel;
    Width := 92;
    Spacing := -1;
    Top := Self.Height - Height - 4;
    Left := (C_InfoWindow_Width - Width) div 2;
    iBtnHeight := Height;
    OnClick := Self.StopClick;
  end;

  pPn2 := TPanel.Create(Self);
  with pPn2 do begin
    Parent := pPn1;
    Height := Self.Height - iBtnHeight - 8 - GetSystemMetrics(SM_CYSIZE);
    Align := alTop;
    Caption := '';
    BevelInner := bvNone;
    BevelOuter := bvNone;
  end;

  with TImage.Create(Self) do begin
    Parent := pPn2;
    Align := alLeft;
    Width := pPn2.Height;
    Transparent := True;
    Center := True;
    Picture.Icon.Handle := Application.Icon.Handle;
  end;

  FPanel := TLabel.Create(Self);    // 07.11.2008
  with FPanel do begin
    Parent := pPn2;
    Align := alClient;
    Caption := '';
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Alignment := taCenter;
    Layout := tlCenter;
  end;
end;

{-----------------------------------------}
procedure TComInfoWindow.SetInfoCaption(sText: string);
{-----------------------------------------}
begin
  FLabel.Caption := sText;

  Application.ProcessMessages;
end;

{-----------------------------------------}
procedure TComInfoWindow.SetInfoText(sText: string);
{-----------------------------------------}
var
  i, iW, iWidth : integer;
begin
  FPanel.Caption := sText;

  // Größenanpassung des Fensters  // 25.04.2005 // 07.11.2008
  iWidth := 0;
  with TStringList.Create do
  try
    Text := sText;
    for i := 0 to Count-1 do begin
      iW := Self.Canvas.TextWidth(Strings[i]);
      if (iW > iWidth) then iWidth := iW;
    end;
  finally
    Free;
  end;
  iWidth := FPanel.Left + iWidth + 10;
  if (FMemMaxWidth) then begin
    if (iWidth > Self.Width) then Self.Width := iWidth;
  end
  else begin
    if (iWidth > (C_InfoWindow_Width - 4)) then
      Self.Width := iWidth
    else if (iWidth > C_InfoWindow_Width) and (iWidth < (C_InfoWindow_Width - 4))
    then
      Self.Width := C_InfoWindow_Width;
  end;
  FBtn.Left := (Self.Width - FBtn.Width) div 2;

  Self.Left := (Screen.Width - Self.Width) div 2;
  Application.ProcessMessages;
end;

{-----------------------------------------}
procedure TComInfoWindow.StopClick(Sender: TObject);
{-----------------------------------------}
begin
  Stoped := True;
end;

{-----------------------------------------}
procedure TComInfoWindow.SetStop(bValue: boolean);
{-----------------------------------------}
begin
  if (bValue <> FStop) then begin
    FStop := bValue;
    FBtn.Enabled := not FStop;
  end;
end;

{-----------------------------------------}
procedure TComInfoWindow.SetEnableStop(bValue: boolean);
{-----------------------------------------}
begin
  FBtn.Visible := bValue;
end;

{--------------------------- Allgemeine Funktionen ----------------------------}

{ Fügt Text aus Stringliste in EXCEL ein  }
{ Parameter: Stringliste mit Zeilen,      }
{            Spaltentrennzeichen,         }
{            Start-Zeile, -Spalte         }
{ Rückgabe: Erfolg Ja/Nein                }
{-----------------------------------------}
function InsertToExcel(pSl: TStrings; cSeparator: char; iRow, iCol: integer;
  vExcel: Variant; bShowCaption: boolean = False; pInfoForm: TComInfoWindow = nil;
  sBookName: string = ''): boolean; overload;
{-----------------------------------------}
type
  TByteSet = set of byte;
var
  vWorkBook, vWorkSheet : Variant;
  i, j, iPos : integer;
  iR, iC     : integer;
  s, sCell, sRange : string;
  oldCursor  : TCursor;
  pByteSet   : TByteSet;
  pForm      : TComInfoWindow;
begin
  Result := False;

  try
    pByteSet := [];
    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;

    if (Assigned(pInfoForm))
    then pForm := pInfoForm
    else pForm := TComInfoWindow.CreateInfoWindow(S_Export1);
    pForm.Show;

    try

      if (Assigned(pSl)) then begin
        if (Assigned(pForm)) then begin
          pForm.SetInfoText(Format(S_Export2, [pSl.Count]));
          pForm.SetInfoCaption(Format(S_Export3, [pSl.Count]));
        end;


        iR := iRow;
        iC := iCol;
        sRange := '';

        try
          vWorkBook := vExcel.WorkBooks[vExcel.WorkBooks.Count];
          vWorkSheet := vWorkBook.Worksheets.Add;
          vExcel.DisplayAlerts := True;

          if (bShowCaption) and (pSl.Count > 0) then begin
            // überprüfen, ob Name bereits existiert
            s := '';
            j := 1;
            while (s = '') do begin
              // Bugfix verbotene Zeichen statt erlaubte; 24.04.2013, WW
              s := FilterCharsFromString(pSl[0], [], ['[',']','{','}']);
              if (Length(s) > C_ExcelCaption_MaxLength) then
                s := Copy(s, 1, C_ExcelCaption_MaxLength-3) + '...';
              if (j > 1) then begin
                if (Length(s) > C_ExcelCaption_MaxLength-5) then
                  s := Copy(s, 1, C_ExcelCaption_MaxLength-8) + '...';
                s := s + ' (' + IntToStr(j) + ')';
              end;
              for i := 1 to vWorkBook.WorkSheets.Count do
                if (s = vWorkBook.WorkSheets[i].Name) then begin
                  Inc(j);
                  s := '';
                  Break;
                end;
            end;

            vWorkSheet.Name := s;
            j := 1;  // erste Zeile ist Überschrift und wird nicht übernommen
          end
          else j := 0;

          for i := j to pSl.Count-1 do begin
            s := pSl[i];
            iC := iCol;

            if (Assigned(pForm)) then pForm.SetInfoText(
              Format(S_Export4, [i+1, pSl.Count]));

            while (Length(s) > 0) do begin
              // Falls noch nicht geschehen: NumberFormat der Spalte auf 'Text'
              if (not (iC in pByteSet)) then begin
                if (Assigned(pForm)) then
                  pForm.SetInfoText(Format(S_Export5, [iC]));

                if (iC < 27) then begin
                  sCell := Chr(Ord('A') - 1 + iC) + IntToStr(iR) + ':';
                  sCell := sCell + Chr(Ord('A') - 1 + iC) +
                    IntToStr(iR + pSl.Count-1 + i);
                end
                else begin
                  sCell := Chr(Ord('A') - 1 + ((iC-1) div 26)) +
                    Chr(Ord('A') + ((iC-1) mod 26)) + IntToStr(iR) + ':';
                  sCell := sCell + Chr(Ord('A') - 1 + ((iC-1) div 26)) +
                    Chr(Ord('A') + ((iC-1) mod 26)) + IntToStr(iR + pSl.Count-1 + i);
                end;

                vWorkSheet.Range[sCell].NumberFormat := '@';

                if (sRange = '')     // 10.06.2002
                then sRange := sCell
                else sRange := GetStringPart(sRange, 1, ':') + ':' +
                  GetStringPart(sCell, 2, ':');

                Include(pByteSet, iC);
                if (Assigned(pForm)) and (pForm.Stoped) then Break;
              end;

              // Wert eintragen
              iPos := Pos(cSeparator, s);
              if (iPos = 0) then iPos := Length(s) + 1;
              sCell := Chr(Ord('A') - 1 + iC) + IntToStr(iR);
              vWorkSheet.Cells(iR, iC) := Copy(s, 1, iPos-1);
              Inc(iC);
              Delete(s, 1, iPos);
            end;

            if (Assigned(pForm)) and (pForm.Stoped) then Break;

            Inc(iR);
          end;

          if (sRange <> '') then vWorkSheet.Range[sRange].Columns.AutoFit;  // 10.06.2002

          Result := True;
        except
        // Fehlermeldung unterdrücken; Result ist bereits 'False'
        end;

      end;

    finally
      if (Assigned(pForm)) and (not Assigned(pInfoForm)) then pForm.Free;
      Screen.Cursor := oldCursor;
    end;
  except
    on E:Exception do
      MessageDlg(S_ExportErr + #13#10 + E.Message, mtError, [mbOk], 0);
  end;
end;

{ Fügt Text aus Stringliste in EXCEL ein  }
{ Parameter: Stringliste mit Zeilen,      }
{            Spaltentrennzeichen,         }
{            Start-Zeile, -Spalte         }
{ Rückgabe: Erfolg Ja/Nein                }
{-----------------------------------------}
function InsertToExcel(pSl: TStrings; cSeparator: char; iRow, iCol: integer;
  bNewInstance: boolean = False; bShowFortschritt: boolean = True;
  bShowCaption: boolean = False; pInfoForm: TComInfoWindow = nil;
  sBookName: string = ''; sSavePath: string = ''; bSaveAsCSV: boolean = False;
  bQuit: boolean = False): boolean;
{-----------------------------------------}
type
  TByteSet = set of byte;
var
  vExcel, vWorkBook, vWorkSheet : Variant;
  vxlCSV6    : OleVariant;
  i, j, iPos : integer;
  iR, iC     : integer;
  s, sCsvFilename, sCell, sRange : string;
  oldCursor  : TCursor;
  pByteSet   : TByteSet;
  pForm      : TComInfoWindow;
begin
  Result := False;

  try
    pByteSet := [];
    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    if (bShowFortschritt) then begin
      if (Assigned(pInfoForm))
      then pForm := pInfoForm
      else pForm := TComInfoWindow.CreateInfoWindow(S_Export1);
      pForm.Show;
    end
    else pForm := nil;
    try

      if (Assigned(pSl)) then begin
        if (Assigned(pForm)) then begin
          pForm.SetInfoText(Format(S_Export2, [pSl.Count]));
          pForm.SetInfoCaption(Format(S_Export3, [pSl.Count]));
        end;


        iR := iRow;
        iC := iCol;
        sRange := '';

        try
          if (not bNewInstance) then begin
            try
              vExcel := GetActiveOleObject('Excel.Application');
              vWorkBook := vExcel.WorkBooks[vExcel.WorkBooks.Count];
              vWorkSheet := vWorkBook.Worksheets.Add;
            except
              vExcel := CreateOleObject('Excel.Application');
              vWorkBook := vExcel.WorkBooks.Add;
              if (sBookName <> '') then vWorkBook.SaveAs(FileName := sBookName);
              vExcel.DisplayAlerts := False;
              for i := vWorkBook.Worksheets.Count downto 2 do
                vWorkBook.Worksheets[i].Delete;
              vWorkSheet := vWorkBook.WorkSheets[1];
            end;
          end
          else begin
            vExcel := CreateOleObject('Excel.Application');
            vWorkBook := vExcel.WorkBooks.Add;
            if (sBookName <> '') then vWorkBook.SaveAs(FileName := sBookName);
            vExcel.DisplayAlerts := False;
            for i := vWorkBook.Worksheets.Count downto 2 do
              vWorkBook.Worksheets[i].Delete;
            vWorkSheet := vWorkBook.WorkSheets[1];
          end;
          vExcel.DisplayAlerts := True;
          if (bShowCaption) and (pSl.Count > 0) then begin
            // überprüfen, ob Name bereits existiert
            s := '';
            j := 1;
            while (s = '') do begin
              // Bugfix verbotene Zeichen statt erlaubte; 24.04.2013, WW
              s := FilterCharsFromString(pSl[0], [], ['[',']','{','}']);
              if length (sCsvFilename) = 0 then
                sCsvFilename := s;  // 24.04.2013, WW

              if (Length(s) > C_ExcelCaption_MaxLength) then
                s := Copy(s, 1, C_ExcelCaption_MaxLength-3) + '...';
              if (j > 1) then begin
                if (Length(s) > C_ExcelCaption_MaxLength-5) then
                  s := Copy(s, 1, C_ExcelCaption_MaxLength-8) + '...';
                s := s + ' (' + IntToStr(j) + ')';
              end;
              for i := 1 to vWorkBook.WorkSheets.Count do
                if (s = vWorkBook.WorkSheets[i].Name) then begin
                  Inc(j);
                  s := '';
                  Break;
                end;
            end;

            vWorkSheet.Name := s;
            j := 1;  // erste Zeile ist Überschrift und wird nicht übernommen
          end
          else j := 0;

          for i := j to pSl.Count-1 do begin
            s := pSl[i];
            iC := iCol;

            if (Assigned(pForm)) then pForm.SetInfoText(
              Format(S_Export4, [i+1, pSl.Count]));

            while (Length(s) > 0) do begin
              // Falls noch nicht geschehen: NumberFormat der Spalte auf 'Text'
              if (not (iC in pByteSet)) then begin
                if (Assigned(pForm)) then
                  pForm.SetInfoText(Format(S_Export5, [iC]));

                if (iC < 27) then begin
                  sCell := Chr(Ord('A') - 1 + iC) + IntToStr(iR) + ':';
                  sCell := sCell + Chr(Ord('A') - 1 + iC) +
                    IntToStr(iR + pSl.Count-1 + i);
                end
                else begin
                  sCell := Chr(Ord('A') - 1 + ((iC-1) div 26)) +
                    Chr(Ord('A') + ((iC-1) mod 26)) + IntToStr(iR) + ':';
                  sCell := sCell + Chr(Ord('A') - 1 + ((iC-1) div 26)) +
                    Chr(Ord('A') + ((iC-1) mod 26)) + IntToStr(iR + pSl.Count-1 + i);
                end;

                vWorkSheet.Range[sCell].NumberFormat := '@';

                if (sRange = '')     // 10.06.2002
                then sRange := sCell
                else sRange := GetStringPart(sRange, 1, ':') + ':' +
                  GetStringPart(sCell, 2, ':');

                Include(pByteSet, iC);
                if (Assigned(pForm)) and (pForm.Stoped) then Break;
              end;

              // Wert eintragen
              iPos := Pos(cSeparator, s);
              if (iPos = 0) then iPos := Length(s) + 1;
              sCell := Chr(Ord('A') - 1 + iC) + IntToStr(iR);
              vWorkSheet.Cells(iR, iC) := Copy(s, 1, iPos-1);
              Inc(iC);
              Delete(s, 1, iPos);
            end;

            if (Assigned(pForm)) and (pForm.Stoped) then Break;

            Inc(iR);
          end;

          if (bSaveAsCSV) and (sCsvFilename <> '') then begin

            if (sSavePath <> '') and (DirectoryExists(sSavePath))
            then s := IncludeTrailingBackslash(sSavePath)
            else s := ExtractFilePath(ParamStr(0));

            vExcel.DisplayAlerts := False;
            vxlCSV6 := 6;
            vWorkbook.SaveAs(FileFormat := vxlCSV6,
              Filename := s + sCsvFilename + '.csv', CreateBackup := False);
            vExcel.Quit;
          end
          else if (bQuit) then begin
            vExcel.DisplayAlerts := False;
            if (sBookName <> '') then vWorkBook.SaveAs(FileName := sBookName);
            vExcel.Quit;
          end
          else begin
            vExcel.Visible := True;
            if (sRange <> '') then vWorkSheet.Range[sRange].Columns.AutoFit;  // 10.06.2002
          end;

          Result := True;
        except
        // Fehlermeldung unterdrücken; Result ist bereits 'False'
        end;

      end;

    finally
      if (Assigned(pForm)) and (not Assigned(pInfoForm)) then pForm.Free;
      Screen.Cursor := oldCursor;
    end;
  except
    on E:Exception do
      MessageDlg(S_ExportErr + #13#10 + E.Message, mtError, [mbOk], 0);
  end;
end;

{ Fügt Text aus Stringliste in EXCEL ein  }
{ Parameter: Stringliste mit Zeilen,      }
{            Spaltentrennzeichen,         }
{            Start-Zeile, -Spalte         }
{ Rückgabe: Erfolg Ja/Nein                }
{-----------------------------------------}
function InsertFileToExcel(sFile: TFilename; cSeparator: char; iRow, iCol: integer;
  bNewInstance: boolean = False; bShowFortschritt: boolean = True;
  bShowCaption: boolean = False; pInfoForm: TComInfoWindow = nil): boolean;
{-----------------------------------------}
type
  TByteSet = set of byte;
var
  vExcel, vWorkBook, vWorkSheet : Variant;
  i, j, iPos : integer;
  iCount, iR, iC     : integer;
  s, sLine, sCell, sRange : string;
  oldCursor  : TCursor;
  pByteSet   : TByteSet;
  pForm      : TComInfoWindow;
  f          : TextFile;
begin
  Result := False;

  try

    AssignFile(f, sFile);
    Reset(f);
    try

      pByteSet := [];
      oldCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      if (bShowFortschritt) then begin
        if (Assigned(pInfoForm))
        then pForm := pInfoForm
        else pForm := TComInfoWindow.CreateInfoWindow(S_Export1);
        pForm.Show;
      end
      else pForm := nil;
      try

        // Anzahl der zu exportierenden Sätze ermitteln
        iCount := 0;
        while (not Eof(f)) do begin
          Readln(f, sLine);
          Inc(iCount);
        end;
        CloseFile(f);
        Reset(f);
        if (Assigned(pForm)) then begin
          pForm.SetInfoText(Format(S_Export2, [iCount]));
          pForm.SetInfoCaption(Format(S_Export3, [iCount]));
        end;


        iR := iRow;
        iC := iCol;
        sRange := '';

        try
          if (not bNewInstance) then begin
            try
              vExcel := GetActiveOleObject('Excel.Application');
              vWorkBook := vExcel.WorkBooks[vExcel.WorkBooks.Count];
              vWorkSheet := vWorkBook.Worksheets.Add;
            except
              vExcel := CreateOleObject('Excel.Application');
              vWorkBook := vExcel.WorkBooks.Add;
              vExcel.DisplayAlerts := False;
              for i := vWorkBook.Worksheets.Count downto 2 do
                vWorkBook.Worksheets[i].Delete;
              vWorkSheet := vWorkBook.WorkSheets[1];
            end;
          end
          else begin
            vExcel := CreateOleObject('Excel.Application');
            vWorkBook := vExcel.WorkBooks.Add;
            vExcel.DisplayAlerts := False;
            for i := vWorkBook.Worksheets.Count downto 2 do
              vWorkBook.Worksheets[i].Delete;
            vWorkSheet := vWorkBook.WorkSheets[1];
          end;
          vExcel.DisplayAlerts := True;

          if (bShowCaption) and (iCount > 0) then begin
            Readln(f, sLine);
            // überprüfen, ob Name bereits existiert
            s := '';
            j := 1;
            while (s = '') do begin
              // Bugfix verbotene Zeichen statt erlaubte; 24.04.2013, WW
              s := FilterCharsFromString(sLine, [], ['[',']','{','}']);
              if (j > 1) then s := s + ' (' + IntToStr(j) + ')';
              for i := 1 to vWorkBook.WorkSheets.Count do
                if (s = vWorkBook.WorkSheets[i].Name) then begin
                  Inc(j);
                  s := '';
                  Break;
                end;
            end;
            vWorkSheet.Name := s;
          end
          else sLine := '';

          i := -1;
          while (not Eof(f)) do begin
            Inc(i);
            if (sLine = '')
            then Readln(f, s)
            else begin
             s := sLine;
             sLine := '';
            end;

            iC := iCol;

            if (Assigned(pForm)) then pForm.SetInfoText(
              Format(S_Export4, [i+1, iCount]));

            while (Length(s) > 0) do begin
              // Falls noch nicht geschehen: NumberFormat der Spalte auf 'Text'
              if (not (iC in pByteSet)) then begin
                if (Assigned(pForm)) then
                  pForm.SetInfoText(Format(S_Export5, [iC]));

                if (iC < 27) then begin
                  sCell := Chr(Ord('A') - 1 + iC) + IntToStr(iR) + ':';
                  sCell := sCell + Chr(Ord('A') - 1 + iC) +
                    IntToStr(iR + iCount-1 + i);
                end
                else begin
                  sCell := Chr(Ord('A') - 1 + ((iC-1) div 26)) +
                    Chr(Ord('A') + ((iC-1) mod 26)) + IntToStr(iR) + ':';
                  sCell := sCell + Chr(Ord('A') - 1 + ((iC-1) div 26)) +
                    Chr(Ord('A') + ((iC-1) mod 26)) + IntToStr(iR + iCount-1 + i);
                end;

                vWorkSheet.Range[sCell].NumberFormat := '@';

                if (sRange = '')     // 10.06.2002
                then sRange := sCell
                else sRange := GetStringPart(sRange, 1, ':') + ':' +
                  GetStringPart(sCell, 2, ':');

                Include(pByteSet, iC);
                if (Assigned(pForm)) and (pForm.Stoped) then Break;
              end;

              // Wert eintragen
              iPos := Pos(cSeparator, s);
              if (iPos = 0) then iPos := Length(s) + 1;
              sCell := Chr(Ord('A') - 1 + iC) + IntToStr(iR);
              vWorkSheet.Cells(iR, iC) := Copy(s, 1, iPos-1);
              Inc(iC);
              Delete(s, 1, iPos);
            end;

            if (Assigned(pForm)) and (pForm.Stoped) then Break;

            Inc(iR);
          end;

          vExcel.Visible := True;
          if (sRange <> '') then vWorkSheet.Range[sRange].Columns.AutoFit;  // 10.06.2002

          Result := True;
        except
        // Fehlermeldung unterdrücken; Result ist bereits 'False'
        end;

      finally
        if (Assigned(pForm)) and (not Assigned(pInfoForm)) then pForm.Free;
        Screen.Cursor := oldCursor;
      end;

    finally
      CloseFile(f);
    end;
  except
    on E:Exception do
      MessageDlg(S_ExportErr + '#13#10' + E.Message, mtError, [mbOk], 0);
  end;
end;

{-----------------------------------------}
function OpenWordDocument(
  sDoc: TFileName; sMakro: string = ''; sParam: string = ''): boolean;
{-----------------------------------------}
var
  vWord : Variant;
  oldCursor : TCursor;
  s1, s2, s3, s4, s5, b1, b2, b3, b4, i1 : OleVariant;
begin
  Result := False;

  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try

    try
      s1 := sDoc;
      s2 := '';
      s3 := '';
      s4 := '';
      s5 := '';
      b1 := False;
      b2 := False;
      b3 := False;
      b4 := False;
      i1 := 0;
      vWord := CreateOleObject('Word.Application');
      vWord.Documents.Open(s1, b1, b2, b3, s2, s3, b4, s4, s5, i1);
      vWord.Visible := True;

      if (sMakro <> '') then begin
        if (sParam = '') then vWord.Run(sMakro) else vWord.Run(sMakro, sParam);
      end;

      Result := True;

    except
    // Fehlermeldung unterdrücken; Result ist bereits 'False'
    end;

  finally
    Screen.Cursor := oldCursor;
  end;
end;

{ Gibt eine Exceltabelle als Liste zurück }  // 14.03.2002
{ Parameter: Dateiname, Tabellenname      }
{ Rückgabe: Inhalt als Stringliste (TAB)  }
{-----------------------------------------}
function GetWorkSheetAsStringList(
  sFileName, sSheetName: string; bShowFortschritt: boolean = True): TStrings;
{-----------------------------------------}
var
  vExcel, vWorkBook, vWorkSheet : Variant;
  i, j, xMax, yMax              : integer;
  s, sCell                      : string;
  pForm                         : TComInfoWindow;
begin
  Result := TStringList.Create;
  try
    if (bShowFortschritt)
    then begin
      pForm := TComInfoWindow.CreateInfoWindow(S_Import1);
      pForm.Show;
    end
    else pForm := nil;

    try
      vExcel := CreateOleObject('Excel.Application');
      try
        vWorkBook := vExcel.Workbooks.Open(sFileName);
        vWorkSheet := vWorkBook.WorkSheets[sSheetName];

        xMax := vWorkSheet.Cells.SpecialCells(11).Column;  // 13.02.2008
        yMax := vWorkSheet.Cells.SpecialCells(11).Row;  //  11 = xlLastCell

        if (bShowFortschritt) then begin
          pForm.SetInfoText(Format(S_Import2, [yMax]));
          pForm.SetInfoCaption(Format(S_Import3, [yMax]));
        end;

        for i := 1 to yMax do begin
          s := '';
          for j := 1 to xMax do begin
            sCell := vWorkSheet.Cells[i, j];
            s := s + sCell + #9;
          end;
          Result.Add(s);
          if (Assigned(pForm)) then
            pForm.SetInfoText(Format(S_Import4, [i, yMax]));
          if (Assigned(pForm)) and (pForm.Stoped) then Break;
        end;

        vWorkBook.Saved := True;
      finally
        vExcel.Quit;
      end;
    finally
      if (Assigned(pForm)) then pForm.Free;
    end;
  except
    Result.Clear;
  // Liste ist leer
  end;
end;

{ Gibt  Exceltabellenamen als Liste zur.  }  // 21.05.2002
{ Parameter: Dateiname                    }
{ Rückgabe: Namen als Stringliste         }
{-----------------------------------------}
function GetWorkSheetNames(sFileName: string): TStrings;  // 21.05.2002
{-----------------------------------------}
var
  vExcel, vWorkBook : Variant;
  i                 : integer;
begin
  Result := TStringList.Create;
  try
    vExcel := CreateOleObject('Excel.Application');
    try
      vWorkBook := vExcel.Workbooks.Open(sFileName);
      for i := 1 to vWorkBook.WorkSheets.Count do
        Result.Add(vWorkBook.WorkSheets[i].Name);
      vWorkBook.Saved := True;
    finally
      vExcel.Quit;
    end;
  except
    Result.Clear;
  // Liste ist leer
  end;
end;

// 31.03.2011  WN
{ Öffnen eines Excelsheets                }
{ Parameter: Dateiname, Sheetname         }
{-----------------------------------------}
function OpenWorkSheet(sFileName, sSheetName: String): boolean;
{-----------------------------------------}
const
  C_Excel = 'Excel.Application';
var
  vExcel,
  vWorkBook,
  vWorkSheet : Variant;
begin
  try
    vExcel := GetActiveOleObject(C_Excel);
  except
    vExcel := CreateOleObject(C_Excel);
  end;
  try
    vWorkBook  := vExcel.Workbooks.Open(sFileName);
    vWorkSheet := vWorkBook.WorkSheets[sSheetName];
    vWorkSheet.Activate;
    vExcel.Visible := True;
    vExcel.UserControl := True;
    Result := True;
  except
    Result := False;
    vExcel.Quit;   // 05.07.2013
  end;
end;

{ Öffnen einer Exceldatei                 }
{ Parameter: Dateiname                    }
{-----------------------------------------}
function OpenWorkBook(sFileName: string): boolean;     // 05.07.2013
{-----------------------------------------}
const
  C_Excel = 'Excel.Application';
var
  vExcel,
  vWorkBook : Variant;
begin
  try
    vExcel := GetActiveOleObject(C_Excel);
  except
    vExcel := CreateOleObject(C_Excel);
  end;
  try
    vWorkBook  := vExcel.Workbooks.Open(sFileName);
    vExcel.Visible := True;
    vExcel.UserControl := True;
    Result := True;
  except
    Result := False;
    vExcel.Quit;
  end;
end;

{ Öffnen eines Excelsheets,               }
{ Warten auf Beenden                      }
{ Parameter: Dateiname, Sheetname         }
{-----------------------------------------}
function OpenWorkSheetAndWait(sFileName, sSheetName: string): boolean;
{-----------------------------------------}
const
  C_Excel = 'Excel.Application';
var
  vExcel : Variant;
  s          : string;
begin
  try
    try
      vExcel := CreateOleObject(C_Excel);
      s := vExcel.path;
    finally
      vExcel.Quit;
    end;

    WWaitExecute(
      '"' + IncludeTrailingBackslash(s) + 'EXCEL.EXE" "' + sFileName + '"');
    Result := True;
  except
    result := False;
  end;
end;

end.
