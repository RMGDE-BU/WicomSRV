{******************************************************************************}
{* Unit: Konvertieren von DSfG-Instanzwerten (Datenelemente) in Tabelle       *}
{* 07.12.1999 WW                                                              *}
{* 13.02.2001 WW NetReset für Rohdateizugriff (Gamess !)                      *}
{******************************************************************************}
unit DIWKonv;

interface

uses
  Forms, Classes, Db, DbTables, SysUtils, TbDSfGIw, DListen, ErrConst, DJournal,
  DSfGUtil, WStrUtils, WSysCon, DSysCon, WChars, novell;

type

  { Objekt zur Konvertierung von Datenelementen in Instanzwert-Tabelle }

  TDInstanzwertKonv = class(TTbDInstanzwert)
  private
    RohLoeschen: boolean;
    RohfileName: TFilename;
    FS_Rohfile: TFileStreamExt;
    procedure CreatetbDInstanzwert;
    function Konv_Instanzwerte (InstanzId: integer; Zeitangaben: TZeitangaben): integer;
    function OpenRohfile: integer;
    procedure CloseRohfile;
    function CheckDSfGHeader (var ZAE: integer): Integer;
    procedure InstanzwertDataLoop (InstanzId: integer; Zeitangaben: TZeitangaben; Count: integer);
    procedure Split(s: string; var DELAdr: string; var Wert: string);
  public
    procedure Konvertiere (KonvListe: TKonvList; JournalId: integer; ARohLoeschen: boolean);
  end;

implementation


{ TDInstanzwertKonv }

{-----------------------------------------------}
procedure TDInstanzwertKonv.CreatetbDInstanzwert;
{-----------------------------------------------}
{ Instanzwert-Tabelle anlegen }
begin
  with tbDInstanzwert.FieldDefs do begin
    Clear;
    Add(C_TfDInst_InstanzId, ftInteger, 0, false);
    Add(C_TfDInst_DEL_Adr, ftString, 10, false);
    Add(C_TfDInst_Wert, ftString, 40, false);      // 06.03.2002 WW, erweitert von 20 auf 40
    Add(C_TfDInst_DatumZeit, ftDateTime, 0, false);
    Add(C_TfDInst_Zeitzone, ftString, 1, false);
    Add(C_TfDInst_Zeitquelle, ftString, 10, false);
  end;
  with tbDInstanzwert.IndexDefs do begin
    Clear;
    Add('PRefNr', C_TfDInst_InstanzId+';'+C_TfDInst_DEL_Adr, [ixPrimary, ixUnique]);  { Primärindex }
  end;
  tbDInstanzwert.CreateTable;
end;

{--------------------------------------------------------------------------------------------------------}
procedure TDInstanzwertKonv.Konvertiere (KonvListe: TKonvList; JournalId: integer; ARohLoeschen: boolean);
{--------------------------------------------------------------------------------------------------------}
{ Konvertiert die in KonvListe übergebenen Rohdatenfiles.
  Zusätzliche Informationen zu jedem Rohfile:
  - KonvListe.Objects: InstanzId, DatumZeit, Zeitzone
  Übergabe: KonvListe
            JournalId
            ARohLoeschen (wenn true, werden Rohfiles nach der Konvertierung gelöscht) }

var
  i: integer;
  InstanzId: integer;
  Zeitangaben: TZeitangaben;
  Status: integer;
  Fehler_AA: boolean;
  Fehler_sonst: boolean;

begin
  RohLoeschen:=ARohLoeschen;

  { Vorbelegung für globalen Journalfehler bei Instanzwert-Konvertierung: keine Fehler/Warnungen }
  Fehler_AA:=false;     { keine außerplanmäßige Antwort }
  Fehler_sonst:=false;  { kein sonstiger Konvertierungsfehler }

  { wenn Tabelle noch nicht existiert, dann anlegen: }
  if not tbDInstanzwert.Exists then
    CreatetbDInstanzwert;

  if tbDInstanzwert.OpenShared then begin
    try
      { Rohfiles jetzt konvertieren: }
      for i:=0 to KonvListe.Count-1 do begin
        RohfileName:=KonvListe[i];
        InstanzId:=TKonvListObj(KonvListe.Objects[i]).InstanzId;
        Zeitangaben:=TKonvListObj(KonvListe.Objects[i]).Zeitangaben;

        Status:=Konv_Instanzwerte (InstanzId, Zeitangaben);
        if (Status <> DSFGKONVERR_OK) AND (Status <> DSFGKONVERR_NODATA) then begin
          if (Status >= C_MinCode_DSfGKonvErr_AA) then  { außerplanmäßige Antwort }
            Fehler_AA:=true
          else
            Fehler_sonst:=true;
        end;

        { Ist-Zeitbereich und Konvertierungsstatus der DSfG-Datenelemente in
          Journaldetail-Tabelle eintragen (für Archiv/LogbuchNr. und Archivkanal wird -1 gesetzt): }
        WriteDatenzeitbereich_Ist (JournalId, InstanzId, -1, -1, Status,
                                   -1, -1, Zeitangaben.DatumZeit, Zeitangaben.DatumZeit);
      end; { for i:=0 }
    finally
      tbDInstanzwert.Close;
    end;
  end;  { tbDInstanzwert.OpenShared }

  { globalen Fehler in Journal-Fehlertabelle eintragen:
    -> sonstige Konvertierungsfehler vor außerplanmäßiger Antwort }
  if Fehler_sonst then
    WriteJournalFehler (JournalId, ST_KONVERROR, SKERR_INSTWERTKONV)
  else if Fehler_AA then
    WriteJournalFehler (JournalId, ST_KONVERROR, SKERR_AUSSERPLANMAESSIGEANTW_DE); // 20.07.2004, WW
end;

{----------------------------------------------------------------------------------------------------}
function TDInstanzwertKonv.Konv_Instanzwerte (InstanzId: integer; Zeitangaben: TZeitangaben): integer;
{----------------------------------------------------------------------------------------------------}
{ konvertiert Daten aus einem Instanzwerte-Rohfile in Instanzwert-Tabelle.
  Existiert diese nicht, wird sie zuvor angelegt. Vorhandene Datensätze werden
  zuvor gelöscht }
var
  DataCount: integer;

begin
  Result:=OpenRohfile;                                  { Rohdatenfile öffnen }
  try
    if Result <> DSFGKONVERR_OK then exit;
    { Validierung des HDCL im DSfG-Telegramm: }
    Result:=CheckDSfGHeader (DataCount);
    if Result = DSFGKONVERR_OK then                  { jetzt wird konvertiert }
      InstanzwertDataLoop (InstanzId, Zeitangaben, DataCount)
  finally
    CloseRohfile;                                    { Rohdatenfile schließen }
  end;

  { Rohfile löschen: }
  if RohLoeschen AND (Result <> DSFGKONVERR_HEADER) AND (Result <> DSFGKONVERR_DATA) then
    DeleteFile (RohfileName);
end;

{----------------------------------------------}
function TDInstanzwertKonv.OpenRohfile: integer;
{----------------------------------------------}
var
  OK: boolean;
begin
  if FileExists (RohfileName) then begin
    FS_Rohfile:=TFileStreamExt.Create (RohfileName, fmOpenRead OR fmShareDenyWrite, OK);
    if OK then
      Result:=DSFGKONVERR_OK
    else
      Result:=DSFGKONVERR_FILEACCESS;
  end else
    Result:=DSFGKONVERR_FILEACCESS;
end;

{---------------------------------------}
procedure TDInstanzwertKonv.CloseRohfile;
{---------------------------------------}
begin
  FS_Rohfile.Free;
end;

{---------------------------------------------------------------------}
function TDInstanzwertKonv.CheckDSfGHeader (var ZAE: integer): Integer;
{---------------------------------------------------------------------}
{ Verifizierung des DSfG-Telegramm HDCL's }
var
  Code: Integer;
  S: string;
  DID: integer;
  Ausserplanmaessige_Antwort: boolean;

begin
  S := read_hdcl(FS_Rohfile);     { alles bis zum ersten US (keine Auswertung) }

  { HDCL auswerten: }
  S := read_hdcl(FS_Rohfile);                                            { DID }
  val(S, DID, Code);
  if Code <> 0 then begin
    Result := DSFGKONVERR_HEADER;
    exit;
  end;
  if DID < C_DID_DataSimple then begin
    Result := DSFGKONVERR_HEADER;
    exit;
  end;

  S := read_hdcl(FS_Rohfile);                                            { TID }
  S := read_hdcl(FS_Rohfile);                                            { BLO }
  S := read_hdcl(FS_Rohfile);                                            { BNO }
  S := read_hdcl(FS_Rohfile);                                            { DNO }
  S := read_hdcl(FS_Rohfile);                                            { NTY }
  if S <> '' then begin
    if (S[1] <> 'R') AND (S[1] <> 'U') then begin
      Result:=DSFGKONVERR_HEADER;
      exit;
    end;
  end
  else begin
    Result:=DSFGKONVERR_HEADER;
    exit;
  end;

  S := read_hdcl(FS_Rohfile);                                            { DFO }
  S := read_hdcl(FS_Rohfile);                                            { DEB }
  if S <> '' then begin
    if (S[1] <> 'M') AND (S[1] <> 'A') then begin
      Result:=DSFGKONVERR_HEADER;
      exit;
    end;
    Ausserplanmaessige_Antwort:=S[1] = 'A';  // 20.07.2004, WW
  end
  else begin
    Result:=DSFGKONVERR_HEADER;
    exit;
  end;

  S := read_hdcl(FS_Rohfile);                                            { ZAE }
  val(S, ZAE, Code);                       { Anzahl der Datenelemente im File }
  if Code <> 0 then begin
    Result := DSFGKONVERR_HEADER;
    exit;
  end;
  if ZAE = 0 then begin
    if not Ausserplanmaessige_Antwort then begin   // 20.07.2004, WW
      Result := DSFGKONVERR_NODATA;
      exit;
    end;
  end
  else if ZAE < 0 then begin
    Result := DSFGKONVERR_HEADER;
    exit;
  end;

  { Test, ob im HDCL noch weitere Felder vorhanden sind (keine Auswertung): }
  if (DID AND C_PAS) <> 0 then
    S := read_hdcl(FS_Rohfile);                                          { PAS }
  if (DID AND C_DTY) <> 0 then
    S := read_hdcl(FS_Rohfile);                                          { DTY }
  if (DID AND C_ABS) <> 0 then
    S := read_hdcl(FS_Rohfile);                                          { ABS }
  if (DID AND C_EMF) <> 0 then
    S := read_hdcl(FS_Rohfile);                                          { EMF }
  if (DID AND C_TDA) <> 0 then
    S := read_hdcl(FS_Rohfile);                                          { TDA }
  if (DID AND C_TTI) <> 0 then
    S := read_hdcl(FS_Rohfile);                                          { TTI }
  if (DID AND C_PTB) <> 0 then
    S := read_hdcl(FS_Rohfile);                                          { PTB }

  { außerplanmäßige Antwort: Datenteil lesen und prüfen, 20.07.2004, WW }
  if Ausserplanmaessige_Antwort then begin
    S := read_hdcl(FS_Rohfile);
    if S = '?' then
      Result:=DSFGKONVERR_AA_NICHT_PLAUSIBEL
    else if S = '*' then
      Result:=DSFGKONVERR_AA_BESTAETIGUNG
    else if S = '!' then
      Result:=DSFGKONVERR_AA_KEINE_BERECHTIGUNG
    else if S = '#' then
      Result:=DSFGKONVERR_AA_UNBEKANNT
    else if S = ':' then
      Result:=DSFGKONVERR_AA_NICHT_BEHAND
    else
      Result:=DSFGKONVERR_AA_SONSTIGE;
    exit;
  end;

  Result := DSFGKONVERR_OK;
end;

{--------------------------------------------------------------------------------------------------------------}
procedure TDInstanzwertKonv.InstanzwertDataLoop (InstanzId: integer; Zeitangaben: TZeitangaben; Count: integer);
{--------------------------------------------------------------------------------------------------------------}
{ Schleife durch Datenelemente im DSfG-Telegramm. Instanzwerte werden in Instanzwerte-Tabelle eingetragen. }
const
  C_PC = 'PC';
var
  i: integer;
  S: string;
  DELAdr, Wert: string;

begin
  for i:=0 to Count-1 do begin
    Application.ProcessMessages;
    S:=read_ascii(FS_Rohfile);                       { 1 Rohdatenelement lesen }
    Split(S, DELAdr, Wert);            { in DEL-Adresse und Wert konvertieren }
    if tbDInstanzwert.FindKey ([InstanzId, DELAdr]) then begin
      tbDInstanzwert.Edit;
      tbDInstanzwert.FieldByName(C_TfDInst_Wert).AsString:=Wert;
      tbDInstanzwert.FieldByName(C_TfDInst_DatumZeit).AsDateTime:=Zeitangaben.DatumZeit;
      tbDInstanzwert.FieldByName(C_TfDInst_Zeitzone).AsString:=Zeitangaben.Zeitzone;
      if Zeitangaben.vom_PC then
        tbDInstanzwert.FieldByName(C_TfDInst_Zeitquelle).AsString:=C_PC;
      tbDInstanzwert.Post;
    end
    else begin
      if Zeitangaben.vom_PC then
        tbDInstanzwert.InsertRecord([InstanzId, DELAdr, Wert, Zeitangaben.DatumZeit, Zeitangaben.Zeitzone, C_PC])
      else
        tbDInstanzwert.InsertRecord([InstanzId, DELAdr, Wert, Zeitangaben.DatumZeit, Zeitangaben.Zeitzone, nil])
    end;
  end;  { end of for }
end;

{---------------------------------------------------------------------------------}
procedure TDInstanzwertKonv.Split(s: string; var DELAdr: string; var Wert: string);
{---------------------------------------------------------------------------------}
{ aufsplitten eines Datenelements in seine Bestandteile "DEL-Adresse" und "Wert" }
var
  sbuf, zbuf: PChar;
  Len : Word;

begin
  Len := Length (s) + 1;
  getmem(sbuf,Len);
  try
    getmem(zbuf,Len);
    try
      StrPCopy(sbuf,s);
      If FilterString (zbuf, sbuf, US, US, nil, 0) <> Nil Then                   { DEL-Adresse }
        DELAdr := string(zbuf)
      else
        DELAdr:='';
      If FilterString (zbuf, sbuf, US, US, nil, 1) <> Nil Then             { Datenelement-Wert }
        Wert := string(zbuf)
      else
        Wert:='';
    finally
      freemem(zbuf,Len);
    end;
  finally
    freemem(sbuf,Len);
  end;
end;

end.

