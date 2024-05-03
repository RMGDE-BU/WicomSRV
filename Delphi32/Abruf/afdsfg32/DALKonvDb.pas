{******************************************************************************}
{* Unit: Konvertieren von DSfG-Archiv- und Logbuchdaten in Tabellen           *}
{* 30.11.1999  WW                                                             *}
{* 01.02.2001  WW vorhandene Archiv-/Logbuchdaten werden nicht mehr           *}
{*                überschrieben; in der Zeitbereich-Journaltabelle wird der   *}
{*                von/bis-Bereich der neu geschriebenen Daten vermerkt        *}
{* 13.02.2001  WW NetReset für Rohdateizugriff (Gamess !)                     *}
{* 28.06.2001 WW  Umstellung auf gemeinsame MRG/DSfG-Meldungstabellen         *}
{* 09.07.2001 GD  Einbindung automatischer Export                             *}
{* 05.11.2001 GD  Delay eingebaut, damit Tabellenzugriff mehr Zeit hat        *}
{* 12.02.2002 GD  Erstellung von Tabellen in Basisobjekt TTbDArchiv verlagert *}
(* 02.04.2002 GD  Journal-Funktionen mit optionaler Datenbankübergabe         *)
(* 13.12.2002 GD  Meldungs-Alarm-Tabelle füllen                               *)
{* 03.03.2003 WW  Klassenname geändert, jetzt TDArchivLogbuchKonvDB           *}
{* 10.11.2003 GD  Bugfix: Leere Kanäle nicht exportieren                      *}
{* 03.02.2005 GD  Export-Zwischen-Dateien anders benennen                     *}
{******************************************************************************}
unit DALKonvDB;

interface

uses
  Forms, Db, DbTables, SysUtils, TbDSfGAr, DListen, ErrConst, DJournal, DSfGUtil,
  UnixDT, WStrUtils, WSysCon, T_Zeit, DSysCon, WChars, novell, MeldungenDb,
  MeldKonfigDb, DDbSta, AsciiExportDLL, Exportskript, DALKonv;

type

  { Objekt zur Konvertierung von Archiv- und Logbuchdaten }

  TDArchivLogbuchKonvDB = class(TTbDArchiv)
  private
    AktuZeitAngaben: TZeitangaben;  { akt. Zeitangaben des DSfG-Bus zur Berechnung der Zeitzonen der Archiv-/Logb.daten }
    RohLoeschen: boolean;
    Benutzer: string;
    StammPath: string;
    WorkPath: string;
    AsciiPath: string;
    LogbuchBufLen: integer;
    SZ_WZ_Wechsel_in_Logbuchdaten_gefunden: boolean;
    function GetLetztSatzReferenzNr: integer;
    function Konv_ArchivKanal (Index: integer; Gruppe: integer; Kanal: integer; Kanaltyp: string; Werteart: string;
                               var vonOrdNr: integer; var bisOrdNr: integer;
                               var vonDZ: TDateTime; var bisDZ: TDateTime;
                               var ValidStatus: integer): integer;
    function Konv_Logbuch (InstanzId: integer; LogbuchNr: integer;
                           InstanzId_Quelle: integer; GerTypNr_Quelle: integer;
                           var vonOrdNr: integer; var bisOrdNr: integer;
                           var vonDZ: TDateTime; var bisDZ: TDateTime;
                           var ValidStatus: integer): integer;
    function ArchivDataLoop (Index, Gruppe, Kanal: integer; KanalTyp, WerteArt: string; Count: integer;
                             var vonOrdNr: integer; var bisOrdNr: integer;
                             var vonDZ: TDateTime; var bisDZ: TDateTime;
                             var ValidStatus: integer): integer;
    function LogbuchDataLoop (InstanzId: integer; LogbuchNr: integer;
                              InstanzId_Quelle: integer; GerTypNr_Quelle: integer;
                              Count: integer;
                              var vonOrdNr: integer; var bisOrdNr: integer;
                              var vonDZ: TDateTime; var bisDZ: TDateTime;
                              var ValidStatus: integer): integer;
    function Berechne_Zeitzone_Archiv (DatumZeit: TDateTime): string;
    function Berechne_Zeitzone_Logbuch (DatumZeit: TDateTime): string;
    procedure ExportiereArchivgruppe  // 09.07.2001
      (iInstId, iArchId: integer; pKanaele: TByteSet; dtVon, dtBis: TDateTime);
  public
    constructor Create (AArchivPath: string; AStammPath: string; AWorkPath: string;
                        AAsciiPath: string; ABenutzer: string;
                        AStationId: integer;
                        ALogbuchBufLen: integer);
    procedure Konvertiere (KonvListe: TKonvList; AAktuZeitAngaben: TZeitangaben; JournalId: integer;
                           ARohLoeschen: boolean);
  end;

implementation

{ TDArchivLogbuchKonvDB }

{--------------------------------------------------------------------------------}
constructor TDArchivLogbuchKonvDB.Create (AArchivPath: string; AStammPath: string;
                                          AWorkPath: string; AAsciiPath: string;
                                          ABenutzer: string; AStationId: integer;
                                          ALogbuchBufLen: integer);
{--------------------------------------------------------------------------------}
begin
  inherited Create (AArchivPath, AStationId);
  Benutzer:=ABenutzer;                            { für Logbuch-Konvertierung }
  StammPath:=AStammPath;                          { für Logbuch-Konvertierung }
  WorkPath:=AWorkPath;                            { für Ascii-Export }
  AsciiPath:=AAsciiPath;                          { für Ascii-Export }
  LogbuchBufLen:=ALogbuchBufLen;                  { Rundpuffergröße für Logbuch-Einträge }

  SZ_WZ_Wechsel_in_Logbuchdaten_gefunden:=false;
end;

{-------------------------------------------------------------}
function TDArchivLogbuchKonvDB.GetLetztSatzReferenzNr: integer;
{-------------------------------------------------------------}
{ öffnet Satz-Tabelle über Primärindex (ReferenzNr), positioniert auf den letzten
  Datensatz und liefert als Ergebnis dessen ReferenzNr (-1, wenn ReferenzNr nicht ermittelt werden konnte) }
begin
  Result := 0;
  if not tbDSatz.Exists then exit;                                                      { Satz-Tabelle nicht vorhanden }
  if tbDSatz.OpenShared then begin
    try
      if tbDSatz.RecordCount > 0 then begin
        tbDSatz.Last;
        Result:=tbDSatz.FieldByName(C_TfDSatz_ReferenzNr).AsInteger;
      end;
    finally
      tbDSatz.Close;
    end;
  end else
    Result:=-1;
end;

{------------------------------------------------------------------------------------------------}
procedure TDArchivLogbuchKonvDB.Konvertiere (KonvListe: TKonvList; AAktuZeitAngaben: TZeitangaben;
                                             JournalId: integer; ARohLoeschen: boolean);
{------------------------------------------------------------------------------------------------}
{ Konvertiert die in KonvListe übergebenen Rohdatenfiles.
  Zusätzliche Informationen zu jedem Rohfile:
  - StationID global im Objekt
  - KonvListe.Objects: InstanzId, Gruppe, Kanal, Kanaltyp, Werteart, Logbuch-Quelladresse
                       (Kanal < 0: Rohfile enthält Logbuchdaten)
  Übergabe: KonvListe
            AAktuZeitangaben (zur Berechnung der Zeitzone der Archivdaten)
            JournalId
            ARohLoeschen (wenn true, werden Rohfiles nach der Konvertierung gelöscht) }

var
  i: integer;
  Idx: integer;
  LObj: TKonvListObj;
  IstVon_OrdNr, IstBis_OrdNr: integer;
  IstVon_DZ, IstBis_DZ: TDateTime;
  Status: integer;
  ArchivFehler_AA: boolean;
  ArchivFehler_sonst: boolean;
  LogbuchFehler_AA: boolean;
  LogbuchFehler_sonst: boolean;
  ValidStatus: integer;
  ArchivValid: boolean;
  LogbuchValid: boolean;

  pKanaele : TByteSet;   // 09.07.2001
  iArchNr  : integer;    // 09.07.2001
  iInstId  : integer;    // 09.07.2001
  dtVon, dtBis : TDateTime;  // 09.07.2001
begin
  AktuZeitangaben:=AAktuZeitangaben;
  RohLoeschen:=ARohLoeschen;

  { Vorbelegungen für globalen Journalfehler bei Archiv/Logbuch-Konvertierung: keine Fehler, keine Warnungen }
  ArchivFehler_AA:=false;     { keine außerplanmäßige Antwort, Archiv }
  ArchivFehler_sonst:=false;  { kein sonstiger Konvertierungsfehler, Archiv }
  LogbuchFehler_AA:=false;     { keine außerplanmäßige Antwort, Logbuch }
  LogbuchFehler_sonst:=false;  { kein sonstiger Konvertierungsfehler, Logbuch }
  ArchivValid:=true;
  LogbuchValid:=true;

  // Variablen für Export vorbelegen
  pKanaele := [];
  iArchNr := -1;
  iInstId := -1;
  dtVon := 0;
  dtBis := 0;

  for i:=0 to KonvListe.Count-1 do begin
    RohfileName:=KonvListe[i];
    LObj:=TKonvListObj(KonvListe.Objects[i]);

    { Vorbelegung für Ist-Datenbereich: keine Daten }
    IstVon_OrdNr:=0;
    IstBis_OrdNr:=0;
    IstVon_DZ:=0;
    IstBis_DZ:=0;

    { Vorbelegung für ValidStatus: keine Formatfehler }
    ValidStatus:=DSFGKONVERR_OK;

    if LObj.Kanal > 0 then begin                   { Archivkanal konvertieren }

      // 09.07.2001
      if (LObj.InstanzID <> iInstId) or (LObj.Gruppe <> iArchNr) then begin
        ExportiereArchivgruppe(iInstId, iArchNr, pKanaele, dtVon, dtBis);
        pKanaele := [];
        iArchNr := LObj.Gruppe;
        iInstId := LObj.InstanzID;
        dtVon := 0;
        dtBis := 0;
      end;

      { DSfG-Datenindex über INDEX.DB ermitteln, wenn nötig anlegen: }
      Idx:=Get_DSfGDatenIndex (ArchivPath, StationId, LObj.InstanzID, LObj.Gruppe, true);

      if Idx > 0 then begin
        Status:=Konv_ArchivKanal (Idx, LObj.Gruppe, LObj.Kanal, LObj.Kanaltyp, LObj.Werteart,
                                  IstVon_OrdNr, IstBis_OrdNr,
                                  IstVon_DZ, IstBis_DZ,
                                  ValidStatus);
        if (Status = DSFGKONVERR_OK) then begin  // 09.07.2001
          if (IstVon_DZ > 0) and (IstBis_DZ > 0) then begin  // 10.11.2003
            if (dtVon = 0) or (dtVon > IstVon_DZ) then dtVon := IstVon_DZ;
            if (dtBis = 0) or (dtBis < IstBis_DZ) then dtBis := IstBis_DZ;
          end;
          Include(pKanaele, LObj.Kanal);
        end;
      end
      else
        Status:=DSFGKONVERR_INDEX;

      { Fehler: }
      if (Status <> DSFGKONVERR_OK) AND (Status <> DSFGKONVERR_NODATA) then begin
        if (Status >= C_MinCode_DSfGKonvErr_AA) then  { außerplanmäßige Antwort }
          ArchivFehler_AA:=true
        else
          ArchivFehler_sonst:=true;
      end;
      { Datengültigkeit: }
      if ValidStatus <> DSFGKONVERR_OK then
        ArchivValid:=false;
    end
    else begin                                { Logbuch konvertieren }
      Status:=Konv_Logbuch (LObj.InstanzId, LObj.Gruppe,
                            LObj.InstanzId_Quelle, LObj.GerTypNr_Quelle,
                            IstVon_OrdNr, IstBis_OrdNr,
                            IstVon_DZ, IstBis_DZ,
                            ValidStatus);
      { Fehler: }
      if (Status <> DSFGKONVERR_OK) AND (Status <> DSFGKONVERR_NODATA) then begin
        if (Status >= C_MinCode_DSfGKonvErr_AA) then  { außerplanmäßige Antwort }
          LogbuchFehler_AA:=true
        else
          LogbuchFehler_sonst:=true;
      end;
      { Datengültigkeit: }
      if ValidStatus <> DSFGKONVERR_OK then
        LogbuchValid:=false;
    end;

    { kein Fehler in Status enthalten, dann mit ValidStatus überschreiben (hat niedrigere Priorität): }
    if Status = DSFGKONVERR_OK then
      Status:=ValidStatus;

    { Ist-Bereich (Zeit oder Ordnungsnummern) und Konvertierungsstatus der DSfG-Daten
      in Journaldetail-Tabelle eintragen: }
    WriteDatenzeitbereich_Ist (JournalId, LObj.InstanzId, LObj.Gruppe, LObj.Kanal, Status,
                               IstVon_OrdNr, IstBis_OrdNr, IstVon_DZ, IstBis_DZ,
                               StammPath);
  end; { for i:=0 }

  // 09.07.2001 - Letzte Archivgruppe exportieren
  ExportiereArchivgruppe(iInstId, iArchNr, pKanaele, dtVon, dtBis);

  { globale Fehler in Journal-Fehlertabelle eintragen:
    -> sonstige Konvertierungsfehler vor außerplanmäßiger Antwort }
  if ArchivFehler_sonst then
    WriteJournalFehler (JournalId, ST_KONVERROR, SKERR_ARCHIVKONV, StammPath)
  else if ArchivFehler_AA then
    WriteJournalFehler (JournalId, ST_KONVERROR, SKERR_AUSSERPLANMAESSIGEANTW_AR, StammPath); // 20.07.2004, WW

  if LogbuchFehler_sonst then
    WriteJournalFehler (JournalId, ST_KONVERROR, SKERR_LOGBKONV, StammPath)
  else if LogbuchFehler_AA then
    WriteJournalFehler (JournalId, ST_KONVERROR, SKERR_AUSSERPLANMAESSIGEANTW_LB, StammPath); // 20.07.2004, WW

  { ungültige Daten in Journal-Fehlertabelle eintragen: }
  if not ArchivValid then
    WriteJournalFehler (JournalId, ST_DATACHECK, DCH_ARINVALID, StammPath);
  if not LogbuchValid then
    WriteJournalFehler (JournalId, ST_DATACHECK, DCH_LBINVALID, StammPath);
end;

{-----------------------------------------------------------------------------------------------}
function TDArchivLogbuchKonvDB.Konv_ArchivKanal (Index: integer; Gruppe: integer; Kanal: integer;
                                                 Kanaltyp: string; Werteart: string;
                                                 var vonOrdNr: integer; var bisOrdNr: integer;
                                                 var vonDZ: TDateTime; var bisDZ: TDateTime;
                                                 var ValidStatus: integer): integer;
{-----------------------------------------------------------------------------------------------}
{ konvertiert Daten aus einem Rohdatenfile (ein Archivkanal) in Satz- und
  Wert-/Hex-Tabelle.
  Rückgabe: Datum/Zeit des ersten und letzten Datensatzes aus ArchivDataLoop }
var
  DataCount: integer;

begin
  Result:=OpenRohfile;                                  { Rohdatenfile öffnen }
  if Result <> DSFGKONVERR_OK then exit;
  try
    { Validierung des HDCL im DSfG-Telegramm: }
    Result:=CheckDSfGHeader (DataCount);
    if Result = DSFGKONVERR_OK then                  { jetzt wird konvertiert }
      Result:=ArchivDataLoop (Index,Gruppe,Kanal,Kanaltyp,Werteart,DataCount,
                              vonOrdNr,bisOrdNr,vonDZ,bisDZ,ValidStatus);
  finally
    CloseRohfile;                                    { Rohdatenfile schließen }
  end;

  { Rohfile löschen: }
  if RohLoeschen AND (Result <> DSFGKONVERR_HEADER) AND (Result <> DSFGKONVERR_DATA) then
    DeleteFile (RohfileName);
end;

{-----------------------------------------------------------------------------------------------}
function TDArchivLogbuchKonvDB.Konv_Logbuch (InstanzId: integer; LogbuchNr: integer;
                                             InstanzId_Quelle: integer; GerTypNr_Quelle: integer;
                                             var vonOrdNr: integer; var bisOrdNr: integer;
                                             var vonDZ: TDateTime; var bisDZ: TDateTime;
                                             var ValidStatus: integer): integer;
{-----------------------------------------------------------------------------------------------}
{ konvertiert Daten aus einem Rohdatenfile (ein Logbuch) in Logbuchtabelle.
  Rückgabe: Datum/Zeit des ersten und letzten Datensatzes aus LogbuchDataLoop }
var
  DataCount: longint;

begin
  Result:=OpenRohfile;                                  { Rohdatenfile öffnen }
  if Result <> DSFGKONVERR_OK then exit;
  try
    { Validierung des HDCL im DSfG-Telegramm: }
    Result:=CheckDSfGHeader (DataCount);
    if Result = DSFGKONVERR_OK then                 { jetzt wird konvertiert }
      Result:=LogbuchDataLoop (InstanzId, LogbuchNr,
                               InstanzId_Quelle, GerTypNr_Quelle, DataCount,
                               vonOrdNr, bisOrdNr, vonDZ, bisDZ, ValidStatus);
  finally
    CloseRohfile;                                    { Rohdatenfile schließen }
  end;

  { Rohfile löschen: }
  if RohLoeschen AND (Result <> DSFGKONVERR_HEADER) AND (Result <> DSFGKONVERR_DATA) then
    DeleteFile (RohfileName);
end;

{-----------------------------------------------------------------------------------------------------}
function TDArchivLogbuchKonvDB.ArchivDataLoop (Index, Gruppe, Kanal: integer; KanalTyp, WerteArt: string; Count: integer;
                                               var vonOrdNr: integer; var bisOrdNr: integer;
                                               var vonDZ: TDateTime; var bisDZ: TDateTime;
                                               var ValidStatus: integer): integer;
{-----------------------------------------------------------------------------------------------------}
{ Schleife durch Datenelemente im DSfG-Telegramm. Archivdaten werden in Satz- und Wertetabellen eingetragen.
  Vorhandene Datensätze werden nicht überschrieben (ab 01.02.2001), neue angehängt.
  Rückgabe: Datum/Zeit und Ordnungsnummer des ersten und letzten neu geschriebenen Datensatzes }
Const
  C_DSFGSATZSTATUS = '0';                       { Satztabellen-Status immer 0 }

var
  SatzData: TDSfGSatzData;
  i: integer;
  ReferenzMax: integer;
  negative: boolean;
  RefNrSatz: integer;
  erster_neuer_Satz: boolean;
  Status: integer;
  Zeitzone: string;
  Opened: boolean;
  ValueAsDouble: boolean;

begin
  Result:=DSFGKONVERR_OK;
  { Tabellennamen festlegen, mit denen gearbeitet werden soll: }
  SetTablenames(Index, Kanal);

  { wenn Tabellen noch nicht existieren, dann anlegen: }
  if not tbDSatz.Exists then
    CreatetbDSatz (tbDSatz);

  if Kanaltyp <> kt_ST then begin                                                       { bei Status-Kanal: Hex-Tabelle }
    if not tbDWert.Exists then
      CreatetbDWert (tbDWert);
  end
  else begin
    if not tbDHex.Exists then
      CreatetbDHex (tbDHex);
  end;

  { höchste Referenznummer der Satztabelle ermitteln: }
  ReferenzMax := GetLetztSatzReferenzNr;
  ValueAsDouble:=Kanaltyp <> kt_ST;  { bei Statuskanälen Rohwert nicht in Typ Double konvertieren }

  if ReferenzMax >= 0 then begin
    tbDSatz.IndexName:=CSDatOrd;
    try
      if tbDSatz.OpenShared then begin
        try
          if Kanaltyp <> kt_ST then
            Opened:=tbDWert.OpenShared
          else
            Opened:=tbDHex.OpenShared;
          if Opened then begin
            try
              erster_neuer_Satz:=false;
              for i:=0 to Count-1 do begin
                Application.ProcessMessages;
                Status:=ReadDataRohSatz (SatzData, Gruppe, Kanal, ValueAsDouble, negative, ValidStatus);
                if Status <> DSFGKONVERR_OK then begin
                  Result:=Status;
                  Continue;
                end;

                { Zeitzone des Archivdatensatzes berechnen: }
                Zeitzone:=Berechne_Zeitzone_Archiv (SatzData.DatumZeit);

                { Datensatz in Satztabelle bereits vorhanden ? }
                if tbDSatz.FindKey([SatzData.DatumZeit,SatzData.DatumZeit,SatzData.OrdnungsNr]) then begin
                  RefNrSatz:=tbDSatz.FieldByName(C_TfDSatz_ReferenzNr).AsInteger;

                  if Kanaltyp <> kt_ST then begin
                    { Datensatz in Werttabelle bereits vorhanden ? }
                    if not tbDWert.FindKey([RefNrSatz]) then begin
                      { wenn nicht, neuen Datensatz einfügen }
                      tbDWert.InsertRecord([RefNrSatz,Kanaltyp,Werteart,SatzData.WertAsDouble,SatzData.Status,SatzData.CRC]);

                      { Zeitstempel bzw. Ordnungsnummer des ersten und letzten
                        neu geschriebenen Datensatzes: }
                      if not erster_neuer_Satz then begin
                        erster_neuer_Satz:=true;
                        vonOrdNr:=SatzData.OrdnungsNr;
                        vonDZ:=SatzData.DatumZeit;
                      end;
                      bisOrdNr:=SatzData.OrdnungsNr;
                      bisDZ:=SatzData.DatumZeit;
                    end;
                  end
                  else begin
                    { Datensatz in Hextabelle bereits vorhanden ? }
                    if not tbDHex.FindKey([RefNrSatz]) then begin
                      { wenn nicht, neuen Datensatz einfügen }
                      tbDHex.InsertRecord([RefNrSatz,Kanaltyp,Werteart,SatzData.WertAsString,SatzData.Status,SatzData.CRC]);

                      { Zeitstempel bzw. Ordnungsnummer des ersten und letzten
                        neu geschriebenen Datensatzes: }
                      if not erster_neuer_Satz then begin
                        erster_neuer_Satz:=true;
                        vonOrdNr:=SatzData.OrdnungsNr;
                        vonDZ:=SatzData.DatumZeit;
                      end;
                      bisOrdNr:=SatzData.OrdnungsNr;
                      bisDZ:=SatzData.DatumZeit;
                    end;
                  end;
                end
                else begin     { wenn nicht, neuen Datensatz in Satz- und Werttabelle einfügen }
                  Inc(ReferenzMax);
                  tbDSatz.InsertRecord([ReferenzMax,Index,SatzData.OrdnungsNr,SatzData.DatumZeit,
                                        SatzData.DatumZeit,C_DSFGSATZSTATUS,Zeitzone]);
                  if Kanaltyp <> kt_ST then begin
                    { Datensatz in Werttabelle bereits vorhanden ? }
                    if not tbDWert.FindKey([ReferenzMax]) then
                      { wenn nicht, neuen Datensatz einfügen (Normalfall) }
                      tbDWert.InsertRecord([ReferenzMax,Kanaltyp,Werteart,SatzData.WertAsDouble,SatzData.Status,SatzData.CRC])
                    else begin
                      { Datensatz in Wertetabelle schon vorhanden, Datenbanktechnisch möglich, aber eigtl.
                        nicht erlaubt, da in diesem Fall Satz-/Wertetabelle inkonsistent !
                        Überschreiben, 05.05.2003 WW }
                      tbDWert.Edit;
                      tbDWert.FieldByName (C_TfDWert_Kanaltyp).AsString:=Kanaltyp;
                      tbDWert.FieldByName (C_TfDWert_Werteart).AsString:=Werteart;
                      tbDWert.FieldByName (C_TfDWert_Wert).AsFloat:=SatzData.WertAsDouble;
                      tbDWert.FieldByName (C_TfDWert_Status).AsString:=SatzData.Status;
                      tbDWert.FieldByName (C_TfDWert_CRC).AsString:=SatzData.CRC;
                      tbDWert.Post;
                    end;
                  end
                  else begin
                    if not tbDHex.FindKey([ReferenzMax]) then
                      { wenn nicht, neuen Datensatz einfügen (Normalfall) }
                      tbDHex.InsertRecord([ReferenzMax,Kanaltyp,Werteart,SatzData.WertAsString,SatzData.Status,SatzData.CRC])
                    else begin
                      { Datensatz in Hextabelle schon vorhanden, Datenbanktechnisch möglich, aber eigtl.
                        nicht erlaubt, da in diesem Fall Satz-/Hextabelle inkonsistent !
                        Überschreiben, 05.05.2003 WW }
                      tbDHex.Edit;
                      tbDHex.FieldByName (C_TfDWert_Kanaltyp).AsString:=Kanaltyp;
                      tbDHex.FieldByName (C_TfDWert_Werteart).AsString:=Werteart;
                      tbDHex.FieldByName (C_TfDWert_Wert).AsString:=SatzData.WertAsString;
                      tbDHex.FieldByName (C_TfDWert_Status).AsString:=SatzData.Status;
                      tbDHex.FieldByName (C_TfDWert_CRC).AsString:=SatzData.CRC;
                      tbDHex.Post;
                    end;
                  end;

                  { Zeitstempel bzw. Ordnungsnummer des ersten und letzten
                    neu geschriebenen Datensatzes: }
                  if not erster_neuer_Satz then begin
                    erster_neuer_Satz:=true;
                    vonOrdNr:=SatzData.OrdnungsNr;
                    vonDZ:=SatzData.DatumZeit;
                  end;
                  bisOrdNr:=SatzData.OrdnungsNr;
                  bisDZ:=SatzData.DatumZeit;
                end;
              end;  { end of for }
            finally
              if Kanaltyp <> kt_ST then
                tbDWert.Close
              else
                tbDHex.Close;
            end;
          end;  { if Opened }
        finally
          tbDSatz.Close;
        end;
      end;  { if tbDSatz.OpenShared }
    finally
      tbDSatz.IndexName:='';
    end;
  end;
end;

{--------------------------------------------------------------------------------------------------}
function TDArchivLogbuchKonvDB.LogbuchDataLoop (InstanzId: integer; LogbuchNr: integer;
                                                InstanzId_Quelle: integer; GerTypNr_Quelle: integer;
                                                Count: integer;
                                                var vonOrdNr: integer; var bisOrdNr: integer;
                                                var vonDZ: TDateTime; var bisDZ: TDateTime;
                                                var ValidStatus: integer): integer;
{--------------------------------------------------------------------------------------------------}
{ Schleife durch Datenelemente im DSfG-Telegramm. Logbuchdaten werden in Logbuch-Tabelle eingetragen.
  Vorhandene Datensätze werden nicht überschrieben (ab 01.02.2001), neue angehängt.
  Rückgabe: Datum/Zeit und Ordnungsnummer des ersten und letzten neu geschriebenen Datensatzes }
var
  SatzData: TDSfGSatzData;
  i: integer;
  negative: boolean;
  erster_neuer_Satz: boolean;
  Status: integer;
  MeldungenDb: TMeldungenDb;
  MeldungenData: TMeldungenData;
  MeldungenDSfGData: TMeldungenDSfGData;
  MeldungenAlarmData: TMeldungenAlarmData;  // 13.12.2002
  MeldKonfigurationDb: TMeldKonfigurationDb;
  AMNrAllg: string;
  MeldTextData: TMeldTextDataDb;
  AFilter: string;
  Stammdaten: TDSfGStammdaten;
  DSfGDefData: TDSfGDefData;

begin
  if GerTypNr_Quelle < 0 then begin
    Result:=DSFGKONVERR_UNKNOWNLBSOURCE;  { Quell-Instanz ist nicht in den Stammdaten enthalten }
    exit;
  end;

  Result:=DSFGKONVERR_KonfData;
  { Meldungsgruppe aus Gerätetyp-Nr. der Quell-Instanz ermitteln
    (steht in DSfGDefData.MeldGrp): }
  Stammdaten:=TDSfGStammdaten.Create (StammPath);
  try
    if Stammdaten.InitTabellen then
      if Stammdaten.GetDSfGDefData (GerTypNr_Quelle, DSfGDefData) then
        Result:=DSFGKONVERR_OK;
  finally
    Stammdaten.Free;
  end;
  if Result <> DSFGKONVERR_OK then exit;

  { Meldungen in Tabelle konvertieren: }
  Result:=DSFGKONVERR_OK;
  MeldungenDb:=TMeldungenDb.Create (StammPath);
  try
    { manueller Abruf: alle manuellen Logbucheinträge des Logbuchs der Instanz löschen }
    if Benutzer <> C_BenutzerAuto then
      MeldungenDb.DeleteDSfGMeldungen (Benutzer, InstanzId, LogbuchNr);

    { Sekundärindex auf Meldungen-Tabelle zum Sortieren nach Datum/Zeit und Ordnungsnummer }
    MeldungenDb.SetIndexNameTbWMeldungen (C_TI_WMeldungen_ixDatOrd);
    try
      if MeldungenDb.OpenMeldungenTable (true) then begin     { Exklusiv öffnen wegen Zählfeld }
        try
          if MeldungenDb.OpenMeldungenDSfGTable (true) then begin    { Exlusiv öffnen zur Sicherheit }
            try
              if MeldungenDb.OpenMeldungenAlarmTable (true) then begin  { Exlusiv öffnen zur Sicherheit }
                try
                  MeldKonfigurationDb:=TMeldKonfigurationDb.Create (StammPath);
                  try
                    if MeldKonfigurationDb.OpenMeldNrTable AND
                       MeldKonfigurationDb.OpenMeldNrStationTable AND
                       MeldKonfigurationDb.OpenMeldTextStationTable AND
                       MeldKonfigurationDb.OpenMeldTextTable then begin

                      { Filter für LogbuchNr der InstanzId auf Haupttabelle: }
                      AFilter:='(' + C_Tf_WMeldungen_Benutzer + ' = ''' + Benutzer + ''') AND ' +
                               '(' + C_Tf_WMeldungen_GeraeteArt + ' = ''' + C_GerArtDSfG + ''') AND ' +
                               '(' + C_Tf_WMeldungen_InstanzId_Archiv + ' = ' + IntToStr(InstanzId) + ') AND ' +
                               '(' + C_Tf_WMeldungen_LogbuchNr_Archiv + ' = ' + IntToStr(LogbuchNr) + ')';
                      MeldungenDb.SetFilterTbWMeldungen (AFilter);

                      SZ_WZ_Wechsel_in_Logbuchdaten_gefunden:=false;  { Vorbelegung: SZ/WZ-Wechsel nicht gefunden }
                      erster_neuer_Satz:=false;
                      for i:=0 to Count-1 do begin
                        Application.ProcessMessages;
                        Status:=ReadDataRohSatz(SatzData, -1, -1, false, negative, ValidStatus);
                        if Status <> DSFGKONVERR_OK then begin
                          Result := Status;
                          Continue;
                        end;

                        { Datensatz in Logbuchtabelle bereits vorhanden ? }
                        if not MeldungenDb.DSfGMeldungAlreadyExists (SatzData.DatumZeit, SatzData.OrdnungsNr) then begin
                          { Meldung wird in neuem Datensatz zur Haupt-Tabelle hinzugefügt }
                          MeldungenData.Benutzer:=Benutzer;
                          with MeldungenData do begin
                            MeldungId:=-1;                 { wird von Tabellen-Zählfeld vergeben }
                            GeraeteArt:=C_GerArtDSfG;
                            GeraeteId:=InstanzId_Quelle;
                            InstanzId_Archiv:=InstanzId;
                            LogbuchNr_Archiv:=LogbuchNr;
                            OrdnungsNr:=SatzData.OrdnungsNr;
                            DatumZeit:=SatzData.DatumZeit;
                            { Zeitzone des Logbuchdatensatzes berechnen: }
                            Zeitzone:=Berechne_Zeitzone_Logbuch (SatzData.DatumZeit);
                            MNrGeraet:=SatzData.WertAsString;
                            { allgemeine Meldungsnummer ermitteln: }
                            if MeldKonfigurationDb.GetAllgMeldNr (C_GerArtDSfG, DSfGDefData.MeldGrp,
                                                                  MNrGeraet, false, AMNrAllg) then
                              MNrAllg:=AMNrAllg
                            else
                              MNrAllg:='';
                            { Meldungstext ermitteln: }
                            if MeldKonfigurationDb.GetMeldungData (C_GerArtDSfG,
                                                                   GeraeteId,
                                                                   MNrAllg,
                                                                   false,
                                                                   MeldTextData) then begin
                              MText:=MeldTextData.MText;
                              MArt:=MeldTextData.MArt;
                            end
                            else begin
                              MText:='unbekannt';
                              MArt:='';
                            end;

                            { Meldungstyp aus den abgerufenen Daten ermitteln: }
                            if negative then
                              MTyp:=mtyp_geht
                            else
                              MTyp:=mtyp_unbestimmt;    { einwertige oder kommt-Meldung, nicht zu unterscheiden !  }
                            Quittiert:=false;
                            Bemerkung:='';
                            MeldungId:=MeldungenDb.WriteDSfGMeldung (MeldungenData);
                          end; { with }

                          if MeldungenData.MeldungId > 0 then begin   { Hauptsatz konnte geschrieben werden }
                            { DSfG-Detail-Tabelle für schreiben: }
                              with MeldungenDSfGData do begin
                                MeldungenDSfGData.MeldungId:=MeldungenData.MeldungId;
                                MeldungenDSfGData.Status:=SatzData.Status;
                                MeldungenDSfGData.CRC:=SatzData.CRC;
                              end;
                              MeldungenDb.WriteDSfGMeldungDSfG (MeldungenDSfGData);

                            { Alarm-Detailtabelle schreiben:  // 13.12.2002
                              -> alle Alarme auf "false" bedeutet, daß die Alarme noch
                                 nicht ausgeführt wurden }
                            MeldungenAlarmData.MeldungId:=MeldungenData.MeldungId;
                            MeldungenAlarmData.PCAlarm:=false;
                            MeldungenAlarmData.RelaisAlarm:=false;
                            MeldungenAlarmData.VoiceAlarm:=false;
                            MeldungenAlarmData.Gedruckt:=true;   { nix wird gedruckt }
                            MeldungenDb.WriteMeldungAlarm (MeldungenAlarmData);
                          end;

                          { Zeitstempel bzw. Ordnungsnummer des ersten und letzten
                            neu geschriebenen Datensatzes: }
                          if not erster_neuer_Satz then begin
                            erster_neuer_Satz:=true;
                            vonOrdNr:=SatzData.OrdnungsNr;
                            vonDZ:=SatzData.DatumZeit;
                          end;
                          bisOrdNr:=SatzData.OrdnungsNr;
                          bisDZ:=SatzData.DatumZeit;
                        end;
                      end;  { end of for }
                    end;  { if MeldKonfigurationDb.OpenMeldNrTable AND ...}
                  finally
                    MeldKonfigurationDb.CloseMeldNrStationTable;
                    MeldKonfigurationDb.CloseMeldTextStationTable;
                    MeldKonfigurationDb.CloseMeldTextTable;
                    MeldKonfigurationDb.CloseMeldNrTable;
                    MeldKonfigurationDb.Free;
                  end;
                finally
                  MeldungenDb.CloseMeldungenAlarmTable (true);  { Close und zur Sicherheit Flush }
                end;
              end;   { if Meldungen.OpenMeldungenAlarmTable }
            finally
              MeldungenDb.CloseMeldungenDSfGTable (true);  { Close und zur Sicherheit Flush }
            end;
          end;   { if Meldungen.OpenMeldungenDSfGTable }
        finally
          MeldungenDb.ClearFilterTbWMeldungen;
          MeldungenDb.CloseMeldungenTable (true);  { Close und zur Sicherheit Flush }
        end;
      end;  { if Meldungen.OpenMeldungenTable }
    finally
      MeldungenDb.SetIndexNameTbWMeldungen ('');      { Sekundärindex wieder löschen }
    end;

    { Automatischer Abruf: Meldungs-Tabellen als Rundpuffer organisieren }
    if Benutzer = C_BenutzerAuto then
      MeldungenDb.ReorganizeDSfGMeldungen (InstanzId, LogbuchNr, LogbuchBufLen);
  finally
    MeldungenDb.Free;
  end;
end;

{-------------------------------------------------------------------------------------}
function TDArchivLogbuchKonvDB.Berechne_Zeitzone_Archiv (DatumZeit: TDateTime): string;
{-------------------------------------------------------------------------------------}
{ Zeitzonen-Berechnung für Archivdaten;
  Ergebnis: Zeitzonen-String ('M' = MEZ; 'S' = MESZ; Leerstring, wenn Zeitzone
            aufgrund unvollständiger Vorgaben nicht berechnet werde kann) }
var
  Zeitzone: string;
begin
  Zeitzone:=UpperCase (AktuZeitangaben.Zeitzone);
  if (Zeitzone = CMEZ) OR (Zeitzone = CMESZ) then begin
    if AktuZeitangaben.LetztVerstZZ > 0 then begin
      if CmpDateTime (DatumZeit, AktuZeitangaben.LetztVerstZZ) >= 0 then
        Result:=Zeitzone
      else begin
        if Zeitzone = CMEZ then { aktuelle Zeitzone = M -> vorhergehende Zeitzone = S }
          Result:=CMESZ
        else                    { aktuelle Zeitzone = S -> vorhergehende Zeitzone = M }
          Result:=CMEZ;
      end;
    end else                    { noch keine Verstellung der Zeitzone erfolgt }
      Result:=Zeitzone;
  end else
    Result:='';
end;

{--------------------------------------------------------------------------------------}
function TDArchivLogbuchKonvDB.Berechne_Zeitzone_Logbuch (DatumZeit: TDateTime): string;
{--------------------------------------------------------------------------------------}
{ Zeitzonen-Berechnung für Logbuchdaten;
  Ergebnis: Zeitzonen-String ('M' = MEZ; 'S' = MESZ; Leerstring, wenn Zeitzone
            aufgrund unvollständiger Vorgaben nicht berechnet werde kann) }
var
  Zeitzone: string;
begin
  Zeitzone:=UpperCase (AktuZeitangaben.Zeitzone);
  if (Zeitzone = CMEZ) OR (Zeitzone = CMESZ) then begin
    if AktuZeitangaben.LetztVerstZZ > 0 then begin
      if Zeitzone = CMEZ then begin
        { aktuelle Zeitzone = Winterzeit: in den Daten auf Wechsel von Sommer- auf Winterzeit prüfen
          -> damit die Zeitzonen-Berechnung richtig funktioniert, muß sichergestellt sein, daß
             die Rohdaten nicht allein Logbucheinträge aus der zweiten 2-Uhr-Stunde enthalten !
             Abrufbereich anpassen ! }
        if (CmpDateTime (DatumZeit, AktuZeitangaben.LetztVerstZZ) <= 0) AND
          not SZ_WZ_Wechsel_in_Logbuchdaten_gefunden then begin
            if Zeitzone = CMEZ then { aktuelle Zeitzone = M -> vorhergehende Zeitzone = S }
              Result:=CMESZ
            else                    { aktuelle Zeitzone = S -> vorhergehende Zeitzone = M }
              Result:=CMEZ;
        end else
          Result:=Zeitzone;

        if CmpDateTime (DatumZeit, AktuZeitangaben.LetztVerstZZ) >= 0 then
          SZ_WZ_Wechsel_in_Logbuchdaten_gefunden:=true;  { Zeitzonenwechsel in den Logbuchdaten erkannt ! }
      end
      else begin
        { aktuelle Zeitzone = Sommerzeit: in den Daten auf Wechsel von Winter- auf Sommerzeit prüfen }
        if CmpDateTime (DatumZeit, AktuZeitangaben.LetztVerstZZ) >= 0 then
          Result:=Zeitzone
        else begin
          if Zeitzone = CMEZ then { aktuelle Zeitzone = M -> vorhergehende Zeitzone = S }
            Result:=CMESZ
          else                    { aktuelle Zeitzone = S -> vorhergehende Zeitzone = M }
            Result:=CMEZ;
        end;
      end;
    end else                    { noch keine Verstellung der Zeitzone erfolgt }
      Result:=Zeitzone;
  end else
    Result:='';
end;

{ Exportiert eine Archivgruppe                       }
{ Parameter: InstanzId, Nr. der Archivgruppe, Kanal- }
{            Set, Zeit von, bis                      }
{----------------------------------------------------}
procedure TDArchivLogbuchKonvDB.ExportiereArchivgruppe  // 09.07.2001
  (iInstId, iArchId: integer; pKanaele: TByteSet; dtVon, dtBis: TDateTime);
{----------------------------------------------------}

  {--------------------------------------------------}
  function BuildRohFile: boolean;
  {--------------------------------------------------}
  begin
    // Rohfile für den Export erzeugen
    Result := BuildDArchiveExportFile(WorkPath + '~' +
      ChangeFileExt(ExtractFileName(ParamStr(0)), '') +
      IntToStr(Integer(Application.MainForm)) + '.TMP',   // 03.02.2005
//      ChangeFileExt(ExtractFileName(ParamStr(0)), '.TMP'),
      ArchivPath, iInstId, iArchId, pKanaele, dtVon, dtBis);
  end;


begin
  if (Benutzer <> C_BenutzerAuto) or
     (LowerCase(ExtractFileName(ParamStr(0))) <> 'afdsfg32.exe') or
     (pKanaele = []) then Exit;

  Delay(100);  // 05.11.2001

  InitASCIIExport(C_GerArtDSfG, iInstId, [aetDArchive],
    FormatDateTime('yyyymmddhhnnss', Now), 0, 0, ASCIIPath);
  try
    if (BuildRohFile) then begin
      NewASCIIExportFile('D', iInstId, aetDArchive, WorkPath + '~' +
      ChangeFileExt(ExtractFileName(ParamStr(0)), '') +
      IntToStr(Integer(Application.MainForm)) + '.TMP', 0);  // 03.02.2005
//        ChangeFileExt(ExtractFileName(ParamStr(0)), '.TMP'), 0);
      SaveASCIIExportFiles;
      DeleteFile(WorkPath + '~' +
        ChangeFileExt(ExtractFileName(ParamStr(0)), '') +
        IntToStr(Integer(Application.MainForm)) + '.TMP');  // 03.02.2005
//        ChangeFileExt(ExtractFileName(ParamStr(0)), '.TMP'));
    end;
  finally
    DoneASCIIExport;
  end;
end;

end.

