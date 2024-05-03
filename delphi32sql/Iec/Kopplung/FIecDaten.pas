{******************************************************************************}
{* Unit: Zugriff auf Daten- und Konfigurations-Tabellen für IEC-Kopplung      *}
{* 26.05.2003                                                                 *}
{* 27.08.2007 mit "WriteNewTime"-Aufruf bei Schreibzugriff auf Daten-Tabellen *}
{******************************************************************************}
unit FIecDaten;

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms,
  DBTables, DB, Langzeit, IecConst, WSysCon, WTables, TbDSfGAr, DB_Utils,
  T_Zeit, DKurzzeitWerte, DDbSta, DListen, IecKonfDb, IecIniFile,
  DB_Attn, Iec870Util, O_IecConfig_XML, O_DH_Config_XML, O_DH_Defs, IecLog,
  LogFile;

type
  { Strukturen für gesendete Daten }
  { nur für MRG-Daten: }
  TGesendetMRG = record
    Kennung: string[14];
    Kanal_MeldNr: integer;
  end;

  { nur für DSfG-Daten: }
  TGesendetDSfG = record
    StationId: integer;
    InstanzId: integer;
    Arch_LogbNr: integer;
    Kanal_MeldNr: integer;
  end;

  TGesendetDaten = record
    Datentyp: char;
    BisOrdNr: integer;
    BisDZ: TDateTime;
    BisRefNr: integer;    { nur für DSfG-Daten verwendet }
    MRG: TGesendetMRG;    { nur für MRG-Daten verwendet }
    DSfG: TGesendetDSfG;  { nur für DSfG-Daten verwendet }
    DSfG_KZW_Index: integer;  { nur für DSfG-Kurzzeitwerte verwendet }
  end;

  { TFormDatenIec }

  TFormDatenIec = class(TForm)
    LGZStd: TLGZStd;
    DatenTelegrQuery: TQuery;
  private
    { Private-Deklarationen }
    FStammDb: TDatabase;
    FAutoDb: TDatabase;
    FStammDir: string;
    FNetProgDir: string;
    FIniDate: TDateTime;

    DSfGStammdaten: TDSfGStammdaten;
    IECKonfigDb: TIECKonfigDb;
    FIecValueList :TIecValueList;
    FIecConfigXml : TIecConfigXml;
    DSfGSta_OK: boolean;
    ErsteMeldungenDatum: TDateTime;  { für allererstes Meldungs-Telegramm }
    MRG_ErsteDatenAnzWerte: integer;  { Anzahl der letzten MRG-Werte zurück }
    DSfG_ErsteDatenAnzWerte: integer;  { Anzahl der letzten DSfG-Werte zurück }
    Kurzzeitwerte_loeschen_Stunden: integer;  { Anzahl der Stunden bis nicht übertragene Kurzzeitwert-
                                                Dateien endgültig gelöscht werden }
    Kurzzeitwerte_loeschen_Alt: boolean;  { true: es wird, falls mehrere Kurzzeitwerte je Kanal/Logbuch
                                                  vorhanden sind, nur der aktuellste übertragen }
    MRG_GeneralAbfrageAnzWerte: integer;  { Anzahl der letzten MRG-Werte zurück bei Generalabfrage }
    DSfG_GeneralAbfrageAnzWerte: integer;  { Anzahl der letzten DSfG-Werte zurück bei Generalabfrage }
    DualeZeit2a_Bytes: byte;  { Anzahl Bytes in Dualer Zeit 2a }
    procedure GetIniDaten (bCheckIniChanged: boolean);
//    procedure Delete_QueryTempTables;
    function GetIEC_Typkennung (IECProzessInfo: TIECProzessInfo; Time2aBytes: byte): byte;
    function GetIEC_LogHeader (AKonfigIEC: TKonfigIEC; InfoObj_Bytes: byte): string;
    procedure SetGesendetBis_General_MRG (LinienNr: integer; StationsNr: longint;
                                          LinienNr_Bytes: byte; StationsNr_Bytes: byte);
    procedure SetGesendetBis_General_DSfG (LinienNr: integer; StationsNr: longint;
                                           LinienNr_Bytes: byte; StationsNr_Bytes: byte);
    procedure F_Lock_MRG_MW (AMRGKennung: string; AMRGKanalNr: integer);
    procedure F_Lock_MRG_ME (AMRGKennung: string; AMeldNr: integer);
    procedure F_Lock_DSfG_AK (AStationId, AInstanzId, AArchNr, AKanalNr: integer);
    procedure F_Lock_DSfG_LB (AInstanzId, ALogbNr, AMeldNr: integer);
    procedure F_Unlock_MRG_MW;
    procedure F_Unlock_MRG_ME;
    procedure F_Unlock_DSfG_AK;
    procedure F_Unlock_DSfG_LB;

    function GetNeueDaten_Custom (IECLinienNr: integer; InfoObj_Bytes: byte;
      var IEC_TypKennung: byte; var KonfigDSfG_Arch: TKonfigDSfG_Arch; var LogHeader: string;
      DatenListe: TDatenListe; var KZWList_Index: integer; var fMin, fMax: double;
      iCause: byte = C_URS_spontan): boolean;
  public
    { Public-Deklarationen }
    KZWListe: TKZWListe;
    DSfGArchivAuswahl: integer;
    constructor Create (AOwner: TComponent; AStammDb, AAutoDb: TDatabase;
                        ANetProgDir, AStammDir, ANetWorkDir: string;
                        AMaxLGZKanaele: word; var bOK: boolean); reintroduce;
    destructor Destroy; override;
    function DT_Initialisieren: TDateTime;
    function Check_KonfigDB (aAdressfeld_Bytes, aASDU_Bytes, aInfoObj_Bytes: byte): boolean;

    procedure F_Reset;
    procedure F_Unlock;
    procedure F_Update (AMRG_MW, AMRG_ME, ADSfG_AK, ADSfG_LB: boolean);
    function F_Update_MRG_MW: boolean;
    function F_Update_MRG_ME: boolean;
    function F_Update_DSfG_AK: boolean;
    function F_Update_DSfG_LB: boolean;
    procedure SetGesendetBis_General (LinienNr: integer; StationsNr: longint;
                                      LinienNr_Bytes: byte; StationsNr_Bytes: byte);
    function UpdateGesendetBis_MRG(MRGKennung: string; MRGKanal_MeldNr: integer;
                                   OrdNr_Bis: integer; DZ_Bis: TDateTime;
                                   Datentyp: char): boolean;
    function UpdateGesendetBis_DSfG (DSfGStationId: integer; DSfGInstanzId: integer;
                                     DSfGArch_LogbNr: integer; DSfGKanal_MeldNr: integer;
                                     RefNr_Bis: integer; DZ_Bis: TDateTime; OrdNr_Bis: integer;
                                     Datentyp: char): boolean;
(*    procedure CopyDaten; nur für Redundanz *)

    function GetNeueDatenMRG_Mess (IECLinienNr: integer; InfoObj_Bytes: byte;
      var IEC_TypKennung: byte; var KonfigMRG_Mess: TKonfigMRG_Mess; var AufzMax: integer;
      var LogHeader: string; DatenListe: TDatenListe; bSperren: boolean): boolean;
    function GetNeueDatenMRG_Meld (IECLinienNr: integer; InfoObj_Bytes: byte;
      var IEC_TypKennung: byte; var KonfigMRG_Meld: TKonfigMRG_Meld; var LogHeader: string;
      DatenListe: TDatenListe; bSperren: boolean): boolean;
    function GetNeueDatenDSfG_Arch (IECLinienNr: integer; InfoObj_Bytes: byte;
      var IEC_TypKennung: byte; var KonfigDSfG_Arch: TKonfigDSfG_Arch; var LogHeader: string;
      DatenListe: TDatenListe; bSperren: boolean): boolean;
    function GetNeueDatenDSfG_Logb (IECLinienNr: integer; InfoObj_Bytes: byte;
      var IEC_TypKennung: byte; var KonfigDSfG_Logb: TKonfigDSfG_Logb; var LogHeader: string;
      DatenListe: TDatenListe; bSperren: boolean): boolean;
    function GetNeueDatenDSfG_KZW (IECLinienNr: integer; InfoObj_Bytes: byte;
      var IEC_TypKennung: byte; var KonfigDSfG_Arch: TKonfigDSfG_Arch; var LogHeader: string;
      DatenListe: TDatenListe; var KZWList_Index: integer; var fMin, fMax: double;
      iCause: byte = C_URS_spontan): boolean;

    function GetMeldung_Redundanz_AktivPassiv (bRedundanzAktiv: boolean;
      IECLinienNr: integer; IECASDU: integer; IECInfoObj: integer;
      InfoObj_Bytes: byte; var IEC_TypKennung: byte; var KonfigIEC: TKonfigIEC;
      var LogHeader: string; DatenListe: TDatenListe): boolean;

    procedure FillGA_StationsNrListe_Alle (aLiniennummer: integer;aList: TStrings);
  end;

var
  FormDatenIec: TFormDatenIec;

implementation

{$R *.DFM}

uses
  DateUtils;

resourcestring
  SMsgFehlerDStaInit = 'Fehler beim Initialisieren der DSfG-Stammdaten !';
  SMsgDSfGArchivAuswahl = 'DSfG-Archivauswahl: %s';
  SMsgUebertragungArchivKZW = 'Übertragung von Archivdaten und Kurzzeitwerten ist eingeschaltet';
  SMsgUebertragungArchiv = 'Übertragung nur von Archivdaten ist eingeschaltet';
  SMsgUebertragungKZW = 'Übertragung nur von Kurzzeitwerten ist eingeschaltet';
  SMsgEinstellungUndef = 'Undefinierte Einstellung';

const
  CRefNr_Init = 0;
  COrdNr_Init = 0;


{ TFormDatenIec }

{----------------------------------------------------------------------------}
constructor TFormDatenIec.Create (AOwner: TComponent;
                                  AStammDb, AAutoDb: TDatabase;
                                  ANetProgDir, AStammDir, ANetWorkDir: string;
                                  AMaxLGZKanaele: word;
                                  var bOK: boolean);
{----------------------------------------------------------------------------}
begin
  bOK:=true;  // Default: Create OK
  inherited Create (AOwner);

  FStammDb:=AStammDb;
  FAutoDb:=AAutoDb;
  FStammDir:=AStammDir;
  FNetProgDir:=ANetProgDir;

  FIniDate:=-1;  // Default: Zeitstempel der INI-Datei unbekannt
  GetIniDaten (false);  { liest Ini-Einstellungen für zu übertragende Daten }

  if (Assigned(FAutoDb)) then begin
    LGZStd.DatabaseName:=FAutoDb.DatabaseName;
    LGZStd.KanalZahl:=AMaxLGZKanaele;
    LGZStd.isLangzeit:=true;
  end;

  KZWListe:=TKZWListe.Create (ANetWorkDir, Kurzzeitwerte_loeschen_Stunden,
                              Kurzzeitwerte_loeschen_Alt);
  if (KZW_freigeschaltet) and (Assigned(FStammDb)) then begin
    DSfGStammdaten:=TDSfGStammdaten.Create (FStammDb, nil, nil);
    DSfGSta_OK:=DSfGStammdaten.InitTabellen;
    if not DSfGSta_OK then begin
      WriteProgramLog (SMsgFehlerDStaInit, lt_Error);
      MessageBoxWhenExe (Handle, pchar (SMsgFehlerDStaInit), pchar (Application.Title), MB_ICONERROR + MB_OK);
      bOK:=false;  // Create Fehler
    end;
  end
  else begin
    DSfGStammdaten:=nil;
    DSfGSta_OK:=false;
  end;

  if (Assigned(FStammDb))
  then IECKonfigDb:=TIECKonfigDb.Create (FStammDb)
  else IECKonfigDb := nil;
  FIecConfigXml := TIecConfigXml.Create(ANetProgDir); // 28.06.2010
  FIecValueList := TIecValueList.Create; // 28.06.2010
end;

{-------------------------------}
destructor TFormDatenIec.Destroy;
{-------------------------------}
begin
  IECKonfigDb.Free;
  DSfGStammdaten.Free;
  KZWListe.Free;
  FreeAndNil(FIecConfigXml); // 28.06.2010
  FreeAndNil(FIecValueList); // 28.06.2010

  inherited Destroy;
end;

{--------------------------------------------------------------}
procedure TFormDatenIec.GetIniDaten (bCheckIniChanged: boolean);
{--------------------------------------------------------------}
{ INI-Konfiguration für Daten lesen;
  Übergabe: Flag 'bCheckIniChanged': wenn true, wird nur bei geänderter INI-
              Konfiguration gelesen }
var
  Iec32Ini: TIec32Ini;
  bLog: boolean;
  iOld: integer;

begin
  Iec32Ini:=TIec32Ini.Create (FNetProgDir);
  try
    if (not bCheckIniChanged) OR
       (bCheckIniChanged AND
        not (SameDateTime (Iec32Ini.FileDate, FIniDate))) then begin
      FIniDate:=Iec32Ini.FileDate;  // Zeitstempel der INI-Datei merken; 10.03.2015, WW

      ErsteMeldungenDatum:=Date - Iec32Ini.ErsteMeldungenTage;  { Tage zurück ab heute für MRG/DSfG-Meldungen }
      MRG_ErsteDatenAnzWerte:=Iec32Ini.MRGErsteDatenAnzahlWerte;    { für MRG-Langzeitdaten (normiert und original) }
      DSfG_ErsteDatenAnzWerte:=Iec32Ini.DSfGErsteDatenAnzahlWerte;   { für DSfG-Archivwerte }
      Kurzzeitwerte_loeschen_Stunden:=Iec32Ini.KurzzeitwerteLoeschenStunden;   { für DSfG-Kurzzeitwerte }
      Kurzzeitwerte_loeschen_Alt:=Iec32Ini.KurzzeitwerteLoeschenAlt;   { für DSfG-Kurzzeitwerte }

      iOld:=DSfGArchivAuswahl;
      DSfGArchivAuswahl:=Iec32Ini.DSfGArchivAuswahl;
      bLog:=(not bCheckIniChanged) OR
            (bCheckIniChanged AND (iOld <> DSfGArchivAuswahl));
      if bLog then begin  // 10.03.2015, WW
        case DSfGArchivAuswahl of
          daa_Archiv_KZW: WriteProgramLog (Format (SMsgDSfGArchivAuswahl, [SMsgUebertragungArchivKZW]), lt_Info);
          daa_nur_Archiv: WriteProgramLog (Format (SMsgDSfGArchivAuswahl, [SMsgUebertragungArchiv]), lt_Info);
          daa_nur_KZW:    WriteProgramLog (Format (SMsgDSfGArchivAuswahl, [SMsgUebertragungKZW]), lt_Info);
        else
          WriteProgramLog (Format (SMsgDSfGArchivAuswahl, [SMsgEinstellungUndef]), lt_Error);
        end;
      end;

      MRG_GeneralAbfrageAnzWerte:=Iec32Ini.Generalabfrage_MRGAnzahlWerte;
      DSfG_GeneralAbfrageAnzWerte:=Iec32Ini.Generalabfrage_DSfGAnzahlWerte;

      DualeZeit2a_Bytes:=Iec32Ini.DualeZeit2a_Bytes;    { Anzahl Bytes in Dualer Zeit 2a }


    end;
  finally
    Iec32Ini.Free;
  end;
end;

{--------------------------------------------------------------------}
function TFormDatenIec.Check_KonfigDB (aAdressfeld_Bytes, aASDU_Bytes,
                                       aInfoObj_Bytes: byte): boolean;
{--------------------------------------------------------------------}
{ IEC-Konfigurationstabellen und Inhalte prüfen;
  Ergebnis: true, wenn ohne Fehler }
var
  Check_Erg: integer;
  S: string;

begin
  Result := (not Assigned(FStammDb));
  if (Result) then Exit;
  
  Check_Erg:=IECKonfigDb.Check (aAdressfeld_Bytes, aASDU_Bytes, aInfoObj_Bytes);
  case Check_Erg of
     0: begin  // OK
          Result:=true;
        end;

    -1: begin
          S:='Tabelle ' + FStammDb.DatabaseName + '.' + DBIecKonfig + ' nicht gefunden !';
          WriteProgramLog (S, lt_Error);
          MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
        end;

    -2: begin
          S:='Tabelle ' + FStammDb.DatabaseName + '.' + DBIecMeldKonfig + ' nicht gefunden !';
          WriteProgramLog (S, lt_Error);
          MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
        end;

    -3: begin
          S:='Tabelle ' + FStammDb.DatabaseName + '.' + DBIecKonfig + ' kann nicht geöffnet werden !';
          WriteProgramLog (S, lt_Error);
          MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
        end;

    -4: begin
          S:='Tabelle ' + FStammDb.DatabaseName + '.' + DBIecMeldKonfig + ' kann nicht geöffnet werden !';
          WriteProgramLog (S, lt_Error);
          MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
        end;

    -5: begin
          S:='Die IEC-Konfigurationsdaten enthalten ungültige Werte !' + #13#13 +
             'Bitte IEC-Adressen im Programm "IEC-Konfiguration" und INI-Einstellungen ' +
             'für die Anzahl der Oktette der IEC-Adressen prüfen.';
          WriteProgramLog (S, lt_Error);
          MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
        end;
  else
    S:='Undefinierter IEC-Konfigurationsdatenfehler !';
    WriteProgramLog (S, lt_Error);
    MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
  end;  // case
end;

{--------------------------------------------------}
function TFormDatenIec.DT_Initialisieren: TDateTime;
{--------------------------------------------------}
begin
  Result:=EncodeDate(1980,1,1) + EncodeTime(0,0,0,0);
end;
(*
{---------------------------------------------}
procedure TFormDatenIec.Delete_QueryTempTables;
{---------------------------------------------}
{ temporäre Query-Tabellen _qsq*.db löschen;
  -> sollten eigentlich immer automatisch von der BDE gelöscht werden, bleiben
     aber manchmal stehen ! }
var
  F: TSearchRec;
  FileDT: TDateTime;
begin
  { alle _qsq-Tabellen die schon länger als 10 min existieren löschen: }
  if FindFirst(BaseDir + '_qsq*', 0, F) = 0 then begin
    FileDT:=FileDateToDateTime (F.Time);
    if (now - FileDT) > EncodeTime (0, 10, 0, 0) then
      DeleteFile(BaseDir + F.Name);

    while FindNext (F) = 0 do begin
      FileDT:=FileDateToDateTime (F.Time);
      if (now - FileDT) > EncodeTime (0, 10, 0, 0) then
        DeleteFile(BaseDir + F.Name);
    end;
    FindClose(F);
  end;
end;
*)
{------------------ allgemeine Methoden für MRG und DSfG ----------------------}

{-------------------------------------------------------------------------------}
procedure TFormDatenIec.F_Update (AMRG_MW, AMRG_ME, ADSfG_AK, ADSfG_LB: boolean);
{-------------------------------------------------------------------------------}
{ lädt vorhandenen Datenbereich in IEC-Datenverwaltungs-Tabellen (MRG/DSfG) }
begin
//  Delete_QueryTempTables;   { übriggebliebene temporäre _qsq-Tabellen löschen }

  if MRG_freigeschaltet then begin
    if AMRG_MW then
      F_Update_MRG_MW;
    if AMRG_ME then
      F_Update_MRG_ME;
  end;
  if DSfG_freigeschaltet then begin
    if ADSfG_AK AND ((DSfGArchivAuswahl = daa_Archiv_KZW) OR
                     (DSfGArchivAuswahl = daa_nur_Archiv)) then
      F_Update_DSfG_AK;
    if ADSfG_LB then
      F_Update_DSfG_LB;
  end;
end;

{--------------------------------------------------------------------------------------------}
procedure TFormDatenIec.SetGesendetBis_General (LinienNr: integer; StationsNr: longint;
                                                LinienNr_Bytes: byte; StationsNr_Bytes: byte);
{--------------------------------------------------------------------------------------------}
{ setzt für Generalabfrage zu sendenden Datenbereich in IEC-Datenverwaltungs-
  Tabellen (MRG/DSfG) }
begin
  if MRG_freigeschaltet then
    SetGesendetBis_General_MRG (LinienNr, StationsNr, LinienNr_Bytes, StationsNr_Bytes);
  if DSfG_freigeschaltet then
    SetGesendetBis_General_DSfG (LinienNr, StationsNr, LinienNr_Bytes, StationsNr_Bytes);
end;

{------------------------------}
procedure TFormDatenIec.F_Reset;
{------------------------------}
{ IEC-Datenverwaltungs-Tabellen leeren und Datenbereiche neu einlesen;
  Übergabe: Datenbank für IEC-Datentabellen;
  Ergebnis: true, wenn neue LGZ-Daten vorhanden sind }
var
  q: TQuery;

begin
  if (not Assigned(FStammDb)) then Exit;

  { IEC-MRG-Datentabellen leeren: }
  if MRG_freigeschaltet then begin
    if not TableExists(FStammDb, DBIecDatenMRG) then exit;
    if not TableExists(FStammDb, DBIecMeldDatenMRG) then exit;

    { leeren mit Query, um Exklusiv-Zugriff mit EmptyTable zu vermeiden: }
    q:=TQuery.Create (nil);
    try
      q.DatabaseName:=FStammDb.DatabaseName;
      q.sql.clear;
      q.sql.add('Delete from ' + DBIecDatenMRG);      { Daten-Tabelle leeren }
      q.execsql;

      q.sql.clear;
      q.sql.add('Delete from ' + DBIecMeldDatenMRG);  { MeldDaten-Tabelle leeren }
      q.execsql;
    finally
      q.free;
    end;
  end;
  { Trigger-Datei für MRG-Daten- und MeldDaten-Tabelle schreiben, da Schreibzugriff
    erfolgt; 27.08.2007, WW }
  WriteNewTime(FStammDir + DBIecDatenMRG);
  WriteNewTime(FStammDir + DBIecMeldDatenMRG);

  { IEC-DSfG-Datentabellen leeren: }
  if DSfG_freigeschaltet then begin
    if not TableExists(FStammDb, DBIecDatenDSfG) then exit;
    if not TableExists(FStammDb, DBIecMeldDatenDSfG) then exit;

    { leeren mit Query, um Exklusiv-Zugriff mit EmptyTable zu vermeiden: }
    q:=TQuery.Create (nil);
    try
      q.DatabaseName:=FStammDb.DatabaseName;
      q.sql.clear;
      q.sql.add('Delete from ' + DBIecDatenDSfG);      { Daten-Tabelle leeren }
      q.execsql;

      q.sql.clear;
      q.sql.add('Delete from ' + DBIecMeldDatenDSfG);  { MeldDaten-Tabelle leeren }
      q.execsql;
    finally
      q.free;
    end;
  end;
  { Trigger-Datei für DSfG-Daten- und MeldDaten-Tabelle schreiben, da Schreibzugriff
    erfolgt; 27.08.2007, WW }
  WriteNewTime(FStammDir + DBIecDatenDSfG);
  WriteNewTime(FStammDir + DBIecMeldDatenDSfG);

  { vorhandenen Datenbereich einlesen (alle Datentypen): }
  F_Update (true, true, true, true);
end;

{-------------------------------}
procedure TFormDatenIec.F_Unlock;
{-------------------------------}
{ Gesperrt-Datensätze in IEC-Datenverwaltungs-Tabellen (MRG/DSfG) freigeben }
begin
  if MRG_freigeschaltet then begin
    F_Unlock_MRG_MW;
    F_Unlock_MRG_ME;
  end;
  if DSfG_freigeschaltet then begin
    if ((DSfGArchivAuswahl = daa_Archiv_KZW) OR (DSfGArchivAuswahl = daa_nur_Archiv)) then
      F_Unlock_DSfG_AK;
    F_Unlock_DSfG_LB;
  end;
end;


{------------------------- Methoden für MRG -----------------------------------}

{----------------------------------------------}
function TFormDatenIec.F_Update_MRG_MW: boolean;
{----------------------------------------------}
 { Ergebnis: true, wenn neuere MRG-LGZ-Stundendaten vorhanden sind als bisher
  gesendet wurden }
var
  MerkKennung, KMRGKennung: string[14];
  KMRGKanalNr: integer;
  DT_Init: TDateTime;
  Daten_BisOrdNr: integer;
  Daten_BisDZ: TDateTime;
  Gesendet_BisOrdNr: integer;
  DatenTable: TTable;
  KonfQuery: TQueryExt;
  Save_Cursor: TCursor;
  bDbUpdated: boolean;

begin
  Result := False;
  if (not Assigned(FStammDb)) then Exit;

  if not TableExists(FStammDb, DBIecKonfig) then exit;
  if not TableExists(FStammDb, DBIecDatenMRG) then exit;
  if not TableExists(FStammDb, CDBSta) then exit;

  {-------------------- Update MRG-Stundenwerte -------------------------------}
  Save_Cursor:=Screen.Cursor;
  Screen.Cursor:=crHourglass;
  try
    DT_Init:=DT_Initialisieren;

    KonfQuery:=TQueryExt.Create (nil);
    try
      KonfQuery.DatabaseName:=FStammDb.DatabaseName;
      KonfQuery.SQL.Add('SELECT');
      KonfQuery.SQL.Add('  A.' + C_IecKonfig_MrgKennung + ',');
      KonfQuery.SQL.Add('  A.' + C_IecKonfig_KanalNr + ',');
      KonfQuery.SQL.Add('  B.' + C_Sta_ArchDatenTyp);  // erweitert für MRG-Originalwerte, 05.09.2005 WW
      KonfQuery.SQL.Add('FROM');
      KonfQuery.SQL.Add('  ' + DBIecKonfig + ' A,');
      KonfQuery.SQL.Add('  ' + CDBSta + ' B');
      KonfQuery.SQL.Add('WHERE');
      KonfQuery.SQL.Add('  A.' + C_IecKonfig_GeraeteArt + ' = :GeraeteArt and');
      KonfQuery.SQL.Add('  A.' + C_IecKonfig_MrgKennung + ' = B.' + C_Sta_Kennung + ' and');
      KonfQuery.SQL.Add('  B.' + C_Sta_Aktiv + ' = 0');
      KonfQuery.SQL.Add('ORDER BY');
      KonfQuery.SQL.Add('  A.' + C_IecKonfig_MrgKennung + ',');
      KonfQuery.SQL.Add('  A.' + C_IecKonfig_KanalNr);
      KonfQuery.ParamByName('GeraeteArt').asString:=C_GerArtMrg;  { ...aus MRG's }
      if KonfQuery.Open then begin
        DatenTable:=TTable.Create (nil);
        try
          DatenTable.DatabaseName:=FStammDb.DatabaseName;
          DatenTable.TableName:=DBIecDatenMRG;
          DatenTable.Open;
          try
            bDbUpdated:=false;
            MerkKennung:=#0;
            Daten_BisDZ:=DT_Init;
            Daten_BisOrdNr:=COrdNr_Init;
            while not KonfQuery.EOF do begin
              Application.ProcessMessages;
              KMRGKennung:=KonfQuery.FieldByName(C_IecKonfig_MRGKennung).AsString;
              KMRGKanalNr:=KonfQuery.FieldByName(C_IecKonfig_KanalNr).AsInteger;

              // Sta-Flag für LGZ-Originalwerte-Zugriff abfragen:
              LGZStd.IsOriData:=KonfQuery.FieldByName(C_Sta_ArchDatenTyp).AsInteger <> mrg_arch_LGZnorm;

              if (KMRGKanalNr < 1) OR (KMRGKanalNr > LGZStd.Kanalzahl) then begin
                KonfQuery.Next;
                Continue;
              end;

              if MerkKennung <> KMRGKennung then begin       { andere Kennung als im Vorgängersatz }
                MerkKennung:=KMRGKennung;
                Daten_BisDZ:=DT_Init;
                Daten_BisOrdNr:=COrdNr_Init;

                { letzten Eintrag in Langzeitdatei suchen: }
                if LGZStd.Search(KMRGKennung) then begin
                  try
                    LGZStd.Open;
                    try
                      if LGZStd.Size > 0 then begin
                        LGZStd.Last;
                        Daten_BisDZ:=LGZStd.Datum;
                        Daten_BisOrdNr:=LGZStd.Ordnungsnummer;
                      end;
                    finally
                      LGZStd.Close;
                    end;
                  except
                    on Exception do;
                  end;
                end;
              end;

              { iecdatenmrg.db aktualisieren: }
              if DatenTable.FindKey ([KMRGKennung, KMRGKanalNr]) then begin
                if Daten_BisOrdNr > COrdNr_Init then begin
                  Gesendet_BisOrdNr:=DatenTable.FieldByName(C_IecDatenMRG_OrdNrGesendetBis).AsInteger;
                  DatenTable.Edit;
                  DatenTable.FieldByName(C_IecDatenMRG_DatenOrdNrBis).AsInteger:=Daten_BisOrdNr;
                  DatenTable.FieldByName(C_IecDatenMRG_DatenDZBis).AsDateTime:=Daten_BisDZ;
                  DatenTable.Post;                                     { Datensatz aktualisieren }
                  if (Daten_BisOrdNr <> Gesendet_BisOrdNr) AND
                     not DatenTable.FieldByName(C_IecDatenMRG_Gesperrt).AsBoolean then
                    Result:=true;     { neue LGZ-Daten da }
                  { -> Es wird auf "ungleich" geprüft, da die Ordnungsnummer bei neueren Daten
                       nicht zwangsläufig höher sein muß. Wenn alle Daten gelöscht wurden,
                       fängt die Ordnungsnummer wieder bei 1 an ! }
                end else
                  DatenTable.Delete;                                     { Datensatz rauslöschen }
                bDbUpdated:=true;
              end
              else begin
                if Daten_BisOrdNr > COrdNr_Init then begin                         { neuen Datensatz einfügen }
                  DatenTable.InsertRecord([KMRGKennung,
                                           KMRGKanalNr,
                                           Daten_BisOrdNr,
                                           Daten_BisDZ,
                                           COrdNr_Init,
                                           DT_Init,
                                           false]);  // nicht gesperrt
                  bDbUpdated:=true;
                  Result:=true;
                end;
              end;

              KonfQuery.Next;
            end; { while not KonfQuery.EOF }
          finally
            DatenTable.Close;
          end;
        finally
          DatenTable.Free;
        end;

        { Trigger-Datei für MRG-Daten-Tabelle schreiben, wenn Schreibzugriff
          erfolgt; 27.08.29007, WW }
        if bDbUpdated then
          WriteNewTime(FStammDir + DBIecDatenMRG);
      end;  { if KonfQuery.Open }
    finally
      KonfQuery.Close;
      KonfQuery.Free;
    end;
  finally
    Screen.Cursor:=Save_Cursor;
  end;
end;

{----------------------------------------------}
function TFormDatenIec.F_Update_MRG_ME: boolean;
{----------------------------------------------}
 { Ergebnis: true, wenn neuere MRG-LGZ-Meldungen vorhanden sind als bisher
  gesendet wurden }
var
  KMRGKennung: string[14];
  KMRGMeldnr: string [5];
  DT_Init: TDateTime;
  Daten_BisDZ: TDateTime;
  Gesendet_BisDZ: TDateTime;
  MeldDatTable: TTable;
  MeldKonfQuery: TQueryExt;
  Save_Cursor: TCursor;
  bDbUpdated: boolean;

begin
  Result := False;
  if (not Assigned(FStammDb)) then Exit;

  if not TableExists(FStammDb, DBIecMeldKonfig) then exit;
  if not TableExists(FStammDb, DBIecMeldDatenMRG) then exit;
  if not TableExists(FStammDb, CDBSta) then exit;
  if not TableExists(FStammDb, C_Tb_WMeldungen) then exit;

  {--------------------- Update MRG-Meldungen ---------------------------------}
  Save_Cursor:=Screen.Cursor;
  Screen.Cursor:=crHourglass;
  try
    DT_Init:=DT_Initialisieren;

    MeldKonfQuery:=TQueryExt.Create (nil);
    try
      MeldKonfQuery.DatabaseName:=FStammDb.DatabaseName;
      MeldKonfQuery.SQL.Add('SELECT');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_MrgKennung + ',');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_MNrAllg + ',');
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_MeldungId + ',');
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_DatumZeit);
      MeldKonfQuery.SQL.Add('FROM');
      MeldKonfQuery.SQL.Add('  ' + DBIecMeldKonfig + ' A,');
      MeldKonfQuery.SQL.Add('  ' + CDBSta + ' B,');
      MeldKonfQuery.SQL.Add('  ' + C_Tb_WMeldungen + ' C');
      MeldKonfQuery.SQL.Add('WHERE');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_GeraeteArt + ' = :GeraeteArt and');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_MrgKennung + ' = B.' + C_Sta_Kennung + ' and');
      MeldKonfQuery.SQL.Add('  B.' + C_Sta_Aktiv + ' = 0 and');
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_GeraeteId + ' = B.' + C_Sta_MrgId + ' and');
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_MNrAllg + ' = A.' + C_IecMeldKonfig_MNrAllg + ' and');
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_Benutzer + ' = :Benutzer and');
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_GeraeteArt + ' = :GeraeteArt');
      MeldKonfQuery.SQL.Add('ORDER BY');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_MrgKennung + ',');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_MNrAllg + ',');
      // Sortieren nach Zeit, damit neueste Meldungen sicher erkannt werden
      // (bisher sortieren nach MeldungId); 16.04.2015, WW
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_DatumZeit);
      MeldKonfQuery.ParamByName('Benutzer').asString:=C_BenutzerAuto; { Automatik-Daten }
      MeldKonfQuery.ParamByName('GeraeteArt').asString:=C_GerArtMrg;  { ...aus MRG's }
      if MeldKonfQuery.Open then begin
        MeldDatTable:=TTable.Create (nil);
        try
          MeldDatTable.DatabaseName:=FStammDb.DatabaseName;
          MeldDatTable.TableName:=DBIecMeldDatenMRG;
          MeldDatTable.Open;
          try
            bDbUpdated:=false;
            while not MeldKonfQuery.Eof do begin
              Application.ProcessMessages;
              KMRGKennung:=MeldKonfQuery.FieldByName(C_IecMeldKonfig_MRGKennung).AsString;
              KMRGMeldNr:=MeldKonfQuery.FieldByName(C_IecMeldKonfig_MNrAllg).AsString;
              Daten_BisDZ:=MeldKonfQuery.FieldByName(C_Tf_WMeldungen_DatumZeit).asDateTime;

              { iecmelddatenmrg.db aktualisieren: }
              if MeldDatTable.FindKey ([KMRGKennung, KMRGMeldNr]) then begin
                if Daten_BisDZ <> DT_Init then begin
                  Gesendet_BisDZ:=MeldDatTable.FieldByName(C_IecMeldDatenMRG_MeldGesendetBis).AsDateTime;
                  MeldDatTable.Edit;
                  MeldDatTable.FieldByName(C_IecMeldDatenMRG_LGZDatenBis).AsDateTime:=Daten_BisDZ;
                  MeldDatTable.Post;                                     { Datensatz aktualisieren }
                  if (Daten_BisDZ > Gesendet_BisDZ) AND
                     not MeldDatTable.FieldByName(C_IecMeldDatenMRG_Gesperrt).AsBoolean then
                    Result:=true;                 { neue LGZ-Daten da }
                end else
                  MeldDatTable.Delete;                                     { Datensatz rauslöschen }
                bDbUpdated:=true;
              end
              else begin               { Datensatz in iecmelddatenmrg.db nicht gefunden }
                if Daten_BisDZ <> DT_Init then begin                         { neuen Datensatz einfügen }
                  MeldDatTable.InsertRecord([KMRGKennung,
                                             KMRGMeldNr,
                                             Daten_BisDZ,
                                             DT_Init,
                                             false]);  // nicht gesperrt
                  bDbUpdated:=true;
                  Result:=true;
                end;
              end;

              MeldKonfQuery.Next;
            end; { while not MeldKonfQuery.EOF }
           finally
            MeldDatTable.Close;
          end;
        finally
          MeldDatTable.Free;
        end;

        { Trigger-Datei für MRG-MeldDaten-Tabelle schreiben, wenn Schreibzugriff
          erfolgt; 27.08.29007, WW }
        if bDbUpdated then
          WriteNewTime(FStammDir + DBIecMeldDatenMRG);
      end;  { if MeldKonfQuery.Open }
    finally
      MeldKonfQuery.Close;
      MeldKonfQuery.Free;
    end;
  finally
    Screen.Cursor:=Save_Cursor;
  end;
end;

{-----------------------------------------------------------------------------------------}
function TFormDatenIec.UpdateGesendetBis_MRG (MRGKennung: string; MRGKanal_MeldNr: integer;
                                              OrdNr_Bis: integer; DZ_Bis: TDateTime;
                                              Datentyp: char): boolean;
{-----------------------------------------------------------------------------------------}
{ Bis-Zeitpunkt nach erfolgreicher MRG-Telegrammübertragung aktualisieren (getrennt für
  Stundenwerte und Meldungen);
  Übergabe: MRGKennung
            MRGKanal- bzw. MRGMeldungsnummer
            Bis-Ordnungsnummer
            Bis-Zeitpunkt
            zu updatender Datentyp ('S' = Stundenwert
                                    'M' = Meldung)
  Ergebnis: true, wenn neues Bis eingetragen werden konnte }
var
  DatenTable: TTable;
  MeldDatTable: TTable;
  MNrStr: string[5];

begin
  Result:=false;
  if (not Assigned(FStammDb)) then Exit;

  if Datentyp = 'S' then begin                        { Stundenwertzeitpunkt aktualisieren }
    if not TableExists(FStammDb, DBIecDatenMRG) then exit;

    DatenTable:=TTable.Create (nil);
    try
      DatenTable.DatabaseName:=FStammDb.DatabaseName;
      DatenTable.TableName:=DBIecDatenMRG;
      DatenTable.Open;
      try
        if DatenTable.FindKey ([MRGKennung, MRGKanal_MeldNr]) then begin
          DatenTable.Edit;
          DatenTable.FieldByName(C_IecDatenMRG_OrdNrGesendetBis).AsInteger:=OrdNr_Bis;
          DatenTable.FieldByName(C_IecDatenMRG_DZGesendetBis).AsDateTime:=DZ_Bis;
          DatenTable.Post;                                     { Datensatz aktualisieren }
          Result:=true;
        end;
      finally
        DatenTable.Close;
      end;
    finally
      DatenTable.Free;
    end;

    { Trigger-Datei für MRG-Daten-Tabelle schreiben, wenn Schreibzugriff
      erfolgt; 27.08.29007, WW }
    if Result then
      WriteNewTime(FStammDir + DBIecDatenMRG);
  end
  else begin                                   { Meldungszeitpunkt und -Zustand aktualisieren }
    if not TableExists(FStammDb, DBIecMeldDatenMRG) then exit;

    MeldDatTable:=TTable.Create (nil);
    try
      MeldDatTable.DatabaseName:=FStammDb.DatabaseName;
      MeldDatTable.TableName:=DBIecMeldDatenMRG;
      MeldDatTable.Open;
      try
        MNrStr:=Format('%.5d',[MRGKanal_MeldNr]);
        if MeldDatTable.FindKey ([MRGKennung, MNrStr]) then begin
          MeldDatTable.Edit;
          MeldDatTable.FieldByName(C_IecMeldDatenMRG_MeldGesendetBis).AsDateTime:=DZ_Bis;
          MeldDatTable.Post;                        { Datensatz aktualisieren }
          Result:=true;
        end;
      finally
        MeldDatTable.Close;
      end;
    finally
      MeldDatTable.Free;
    end;

    { Trigger-Datei für MRG-MeldDaten-Tabelle schreiben, wenn Schreibzugriff
      erfolgt; 27.08.29007, WW }
    if Result then
      WriteNewTime(FStammDir + DBIecMeldDatenMRG);
  end;
end;

{------------------------------------------------------------------------------------------------}
procedure TFormDatenIec.SetGesendetBis_General_MRG (LinienNr: integer; StationsNr: longint;
                                                    LinienNr_Bytes: byte; StationsNr_Bytes: byte);
{------------------------------------------------------------------------------------------------}
{ für MRG-Einträge Feld 'STDGesendetBis' nach eingegangenem Generalabfrage-Telegramm auf "Generalabfrage-
  Zeitpunkt" setzen;
  Übergabe: IEC-Liniennummer (wenn < 0: ohne Liniennummer) }
var
  AlleLinien: boolean;
  AlleStationen: boolean;
  KLinienNr: integer;
  KStationsNr: longint;
  KMRGKennung: string[14];
  KMRGKanalnr: integer;
  KonfigTable: TTable;
  DatenTable: TTable;
  Daten_BisOrdNr: integer;
  Bis: integer;
  bDbUpdated: boolean;

begin
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecKonfig) then exit;
  if not TableExists(FStammDb, DBIecDatenMRG) then exit;

  if MRG_GeneralAbfrageAnzWerte <= 0 then exit;  { Generalabfrage deaktiviert; 10.12.2004 WW }

  if LinienNr_Bytes = 0 then
    AlleLinien:=true  { ohne Liniennummer }
  else if LinienNr_Bytes > 1 then
    AlleLinien:=LinienNr = $FFFF  { 2 Byte Liniennummer }
  else
    AlleLinien:=LinienNr = $FF;  { 1 Byte Liniennummer }
  if StationsNr_Bytes > 1 then
    AlleStationen:=StationsNr = $FFFF  { 2 Byte Stationsnummer }
  else
    AlleStationen:=StationsNr = $FF;  { 1 Byte Stationsnummer }

  KonfigTable:=TTable.Create (nil);
  try
    KonfigTable.DatabaseName:=FStammDb.DatabaseName;
    KonfigTable.TableName:=DBIecKonfig;
    KonfigTable.Open;
    try
      { Filter auf MRG-Einträge }
      KonfigTable.Filter:=C_IecKonfig_GeraeteArt + ' = ''' + C_GerArtMrg + '''';
      KonfigTable.Filtered:=true;                  { Aktivieren des Filters }

      DatenTable:=TTable.Create (nil);
      try
        DatenTable.DatabaseName:=FStammDb.DatabaseName;
        DatenTable.TableName:=DBIecDatenMRG;
        DatenTable.Open;
        try
          bDbUpdated:=false;
          while not KonfigTable.EOF do begin
            Application.ProcessMessages;
            KLinienNr:=KonfigTable.FieldByName(C_IecKonfig_IEC_LinienNr).AsInteger;
            if not AlleLinien AND (KLinienNr <> LinienNr) then begin
              KonfigTable.Next;
              Continue;
            end;
            KStationsNr:=KonfigTable.FieldByName(C_IecKonfig_IEC_ASDU).AsInteger;
            if not AlleStationen AND (KStationsNr <> StationsNr) then begin
              KonfigTable.Next;
              Continue;
            end;
            KMRGKanalNr:=KonfigTable.FieldByName(C_IecKonfig_KanalNr).AsInteger;
            if (KMRGKanalNr < 1) OR (KMRGKanalNr > LGZStd.Kanalzahl) then begin
              KonfigTable.Next;
              Continue;
            end;
            KMRGKennung:=KonfigTable.FieldByName(C_IecKonfig_MRGKennung).AsString;

            { iecdatenmrg.db aktualisieren: }
            if DatenTable.FindKey ([KMRGKennung, KMRGKanalNr]) then begin
              Daten_BisOrdNr:=DatenTable.FieldByName(C_IecDatenMRG_DatenOrdNrBis).AsInteger;
              { GesendetBis-OrdNr für MRG-Generalabfrage ermitteln (Anzahl der letzten Werte zurück): }
              Bis:=Daten_BisOrdNr - MRG_GeneralabfrageAnzWerte;
              if Bis < 1 then
                Bis:=1;      { mit der ersten Ordnungsnummer beginnen }

              // OrdNrGesendet-bis darf nicht kleiner als Bis sein
              if DatenTable.FieldByName(C_IecDatenMRG_OrdNrGesendetBis).AsInteger > Bis then begin
                DatenTable.Edit;
                DatenTable.FieldByName(C_IecDatenMRG_OrdNrGesendetBis).AsInteger:=Bis;
                DatenTable.FieldByName(C_IecDatenMRG_DZGesendetBis).Clear;   { Feld leeren, da nicht bekannt }
                { -> durch das Leeren des Datum/Zeit-Feldes wird definiert, daß
                     der Zeitstempel des Satzes nicht bekannt ist.
                     (Prüfung in Prozedur SendTelegramm_DatenKl2_MRG in
                     FIecTelegr.pas) }
                DatenTable.Post;                    { Datensatz aktualisieren }
                bDbUpdated:=true;
              end;
            end;

            KonfigTable.Next;
          end; { while not KonfigTable.EOF }
        finally
          DatenTable.Close;
        end;
      finally
        DatenTable.Free;
      end;

      { Trigger-Datei für MRG-Daten-Tabelle schreiben, wenn Schreibzugriff
        erfolgt; 27.08.29007, WW }
      if bDbUpdated then
        WriteNewTime(FStammDir + DBIecDatenMRG);
    finally
      KonfigTable.Close;
    end;
  finally
    KonfigTable.Free;
  end;
end;

{--------------------------------------------------------------------------------}
procedure TFormDatenIec.F_Lock_MRG_MW (AMRGKennung: string; AMRGKanalNr: integer);
{--------------------------------------------------------------------------------}
var
  q: TQuery;

begin
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecDatenMRG) then exit;

  q:=TQuery.Create(nil);
  try
    with q do begin
      DatabaseName:=FStammDb.DatabaseName;
      Sql.Clear;
      Sql.Add('UPDATE ' + DBIecDatenMRG);
      Sql.Add('SET ' + C_IecDatenMRG_Gesperrt + ' = :Gesperrt');
      Sql.Add('WHERE ' + C_IecDatenMRG_MRGKennung + ' = :Kennung and');
      Sql.Add(C_IecDatenMRG_KanalNr + ' = :KanalNr');
      ParamByName('Gesperrt').asBoolean:=true;
      ParamByName('Kennung').AsString:=AMRGKennung;
      ParamByName('KanalNr').AsInteger:=AMRGKanalNr;
      ExecSql;
    end; // with q
  finally
    q.free;
  end;

  { Trigger-Datei für MRG-Daten-Tabelle schreiben, da Schreibzugriff
    erfolgt; 27.08.29007, WW }
  WriteNewTime(FStammDir + DBIecDatenMRG);
end;

{----------------------------------------------------------------------------}
procedure TFormDatenIec.F_Lock_MRG_ME (AMRGKennung: string; AMeldNr: integer);
{----------------------------------------------------------------------------}
var
  q: TQuery;

begin
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecMeldDatenMRG) then exit;

  q:=TQuery.Create(nil);
  try
    with q do begin
      DatabaseName:=FStammDb.DatabaseName;
      Sql.Clear;
      Sql.Add('UPDATE ' + DBIecMeldDatenMRG);
      Sql.Add('SET ' + C_IecMeldDatenMRG_Gesperrt + ' = :Gesperrt');
      Sql.Add('WHERE ' + C_IecMeldDatenMRG_MRGKennung + ' = :Kennung and');
      Sql.Add(C_IecMeldDatenMRG_MNrAllg + ' = :MeldNr');
      ParamByName('Gesperrt').asBoolean:=true;
      ParamByName('Kennung').AsString:=AMRGKennung;
      ParamByName('MeldNr').AsString:=Format('%.5d', [AMeldNr]);
      ExecSql;
    end; // with q
  finally
    q.free;
  end;

  { Trigger-Datei für MRG-MeldDaten-Tabelle schreiben, da Schreibzugriff
    erfolgt; 27.08.29007, WW }
  WriteNewTime(FStammDir + DBIecMeldDatenMRG);
end;

{--------------------------------------}
procedure TFormDatenIec.F_Unlock_MRG_MW;
{--------------------------------------}
var
  q: TQuery;

begin
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecDatenMRG) then exit;

  q:=TQuery.Create(nil);
  try
    with q do begin
      DatabaseName:=FStammDb.DatabaseName;
      Sql.Clear;
      Sql.Add('UPDATE ' + DBIecDatenMRG);
      Sql.Add('SET ' + C_IecDatenMRG_Gesperrt + ' = :Gesperrt');
      Sql.Add('WHERE ' + C_IecDatenMRG_Gesperrt + ' <> :Gesperrt');
      ParamByName('Gesperrt').asBoolean:=false;
      ExecSql;
    end; // with q
  finally
    q.free;
  end;

  { Trigger-Datei für MRG-Daten-Tabelle schreiben, da Schreibzugriff
    erfolgt; 27.08.29007, WW }
  WriteNewTime(FStammDir + DBIecDatenMRG);
end;

{--------------------------------------}
procedure TFormDatenIec.F_Unlock_MRG_ME;
{--------------------------------------}
var
  q: TQuery;

begin
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecMeldDatenMRG) then exit;

  q:=TQuery.Create(nil);
  try
    with q do begin
      DatabaseName:=FStammDb.DatabaseName;
      Sql.Clear;
      Sql.Add('UPDATE ' + DBIecMeldDatenMRG);
      Sql.Add('SET ' + C_IecMeldDatenMRG_Gesperrt + ' = :Gesperrt');
      Sql.Add('WHERE ' + C_IecMeldDatenMRG_Gesperrt + ' <> :Gesperrt');
      ParamByName('Gesperrt').asBoolean:=false;
      ExecSql;
    end; // with q
  finally
    q.free;
  end;

  { Trigger-Datei für MRG-MeldDaten-Tabelle schreiben, da Schreibzugriff
    erfolgt; 27.08.29007, WW }
  WriteNewTime(FStammDir + DBIecMeldDatenMRG);
end;


{------------------------- Methoden für DSfG ----------------------------------}

{-----------------------------------------------}
function TFormDatenIec.F_Update_DSfG_AK: boolean;
{-----------------------------------------------}
 { Ergebnis: true, wenn neuere DSfG-Automatik-Archivdaten vorhanden sind als bisher
  gesendet wurden }
var
  KDSfGStationId: integer;
  KDSfGInstId: integer;
  KDSfGArchivNr: integer;
  KDSfGKanalNr: integer;
  KDSfGKanaltyp: string;
  Daten_BisRefNr: integer;
  Daten_BisDZ: TDateTime;
  Daten_BisOrdNr: integer;
  Gesendet_BisRefNr: integer;
  KonfigTable: TTable;
  DatenTable: TTable;
  Save_Cursor: TCursor;
  DT_Init: TDateTime;
  Idx: integer;
  MerkInstId: integer;
  MerkArchivNr: integer;
  SatzTable: TTableExt;
  WertHexTable: TTableExt;
  bDbUpdated: boolean;

begin
  Result:=false;
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecKonfig) then exit;
  if not TableExists(FStammDb, DBIecDatenDSfG) then exit;

  {-------------------- Update DSfG-Archivdaten -------------------------------}
  Save_Cursor:=Screen.Cursor;
  Screen.Cursor:=crHourglass;
  try
    DT_Init:=DT_Initialisieren;

    KonfigTable:=TTable.Create (nil);
    try
      KonfigTable.DatabaseName:=FStammDb.DatabaseName;
      KonfigTable.TableName:=DBIecKonfig;
      KonfigTable.Open;
      try
        { Filter auf DSfG-Einträge }
        KonfigTable.Filter:=C_IecKonfig_GeraeteArt + ' = ''' + C_GerArtDSfG + '''';
        KonfigTable.Filtered:=true;                  { Aktivieren des Filters }

        DatenTable:=TTable.Create (nil);
        try
          DatenTable.DatabaseName:=FStammDb.DatabaseName;
          DatenTable.TableName:=DBIecDatenDSfG;
          DatenTable.Open;
          try
            bDbUpdated:=false;
            SatzTable:=TTableExt.Create (nil);
            try
              SatzTable.DatabaseName:=FAutoDb.DatabaseName;
              WertHexTable:=TTableExt.Create (nil);
              try
                WertHexTable.DatabaseName:=FAutoDb.DatabaseName;

                Idx:=-1;
                MerkInstId:=-1;
                MerkArchivNr:=-1;
                while not KonfigTable.EOF do begin
                  Application.ProcessMessages;
                  KDSfGStationId:=KonfigTable.FieldByName(C_IecKonfig_DSfGStationID).AsInteger;
                  KDSfGInstId:=KonfigTable.FieldByName(C_IecKonfig_DSfGInstanzID).AsInteger;
                  KDSfGArchivNr:=KonfigTable.FieldByName(C_IecKonfig_DSfGArchivNr).AsInteger;
                  KDSfGKanalNr:=KonfigTable.FieldByName(C_IecKonfig_KanalNr).AsInteger;
                  KDSfGKanaltyp:=KonfigTable.FieldByName(C_IecKonfig_Kanaltyp).AsString;

                  { andere InstanzId oder ArchivNr als im Vorgängersatz: Daten-Index neu ermitteln }
                  if (MerkInstId <> KDSfGInstId) OR (MerkArchivNr <> KDSfGArchivNr) then begin
                    MerkInstId:=KDSfGInstId;
                    MerkArchivNr:=KDSfGArchivNr;
                    Idx:=Get_DSfGDatenIndex (FAutoDb, KDSfGStationId, KDSfGInstId, KDSfGArchivNr, false);
                  end;
                  if Idx <= 0 then begin
                    KonfigTable.Next;
                    Continue;
                  end;

                  Daten_BisRefNr:=CRefNr_Init;
                  Daten_BisDZ:=DT_Init;
                  Daten_BisOrdNr:=COrdNr_Init;

                  { letzten Daten-Eintrag für Archivkanal suchen: zuerst in Wert/Hex-Tabelle }
                  if KDSfGKanaltyp <> kt_ST then
                    WertHexTable.TableName:=Format(C_TbDWert+'%.3d',[KDSfGKanalNr])+Format('%.4d',[Idx])  { Wert-Tabelle }
                  else
                    WertHexTable.TableName:=Format(C_TbDHex+'%.3d',[KDSfGKanalNr])+Format('%.4d',[Idx]);  { Hex- Tabelle }

                  if WertHexTable.Exists then begin        { wenn Wert/Hex-Tabelle vorhanden ist }
                    if WertHexTable.OpenShared then begin
                      try
                        if WertHexTable.RecordCount > 0 then begin     { wenn mind. 1 Datensatz enthalten }
                          WertHexTable.Last;
                          if KDSfGKanaltyp <> kt_ST then
                            Daten_BisRefNr:=WertHexTable.FieldByName (C_TfDWert_ReferenzNr).AsInteger
                          else
                            Daten_BisRefNr:=WertHexTable.FieldByName (C_TfDHex_ReferenzNr).AsInteger;
                        end;
                      finally
                        WertHexTable.Close;
                      end;
                    end  { if WertHexTable.OpenShared }
                    else begin
                      { Problem beim Öffnen der Wert/Hex-Tabelle -> auf Aktualisierung der
                        Tabelle iecdatendsfg.db verzichten, weiter mit nächstem Konfig-Eintrag }
                      KonfigTable.Next;
                      Continue;
                    end;
                  end;  { if WertHexTable.Exists }

                  { ...dann in Satz-Tabelle }
                  if Daten_BisRefNr > CRefNr_Init then begin
                    SatzTable.TableName:=Format(C_TbDSatz+'%.4d',[Idx]);
                    if SatzTable.Exists then begin        { wenn Satz-Tabelle vorhanden ist }
                      if SatzTable.OpenShared then begin
                        try
                          if SatzTable.FindKey ([Daten_BisRefNr]) then begin
                            Daten_BisOrdNr:=SatzTable.FieldByName (C_TfDSatz_OrdNr).AsInteger;
                            Daten_BisDZ:=SatzTable.FieldByName (C_TfDSatz_DatumZeit).AsDateTime;
                          end
                          else begin
                            { Inhalt von Satz- und Wert/Hex-Tabelle ist nicht konsistent -> auf Aktualisierung der
                              Tabelle iecdatendsfg.db verzichten, weiter mit nächstem Konfig-Eintrag }
                            KonfigTable.Next;
                            Continue;
                          end;
                        finally
                          SatzTable.Close;
                        end;
                      end   { if SatzTable.OpenShared }
                      else begin
                        { Problem beim Öffnen der Satz-Tabelle -> auf Aktualisierung der
                          Tabelle iecdatendsfg.db verzichten, weiter mit nächstem Konfig-Eintrag }
                        KonfigTable.Next;
                        Continue;
                      end;
                    end;  { if SatzTable.Exists }
                  end;

                  { iecdatendsfg.db aktualisieren: }
                  if DatenTable.FindKey ([KDSfGStationId, KDSfGInstId, KDSfGArchivNr, KDSfGKanalNr]) then begin
                    { Datensatz gefunden }
                    if Daten_BisRefNr > CRefNr_Init then begin
                      Gesendet_BisRefNr:=DatenTable.FieldByName(C_IecDatenDSfG_RefNrGesendetBis).AsInteger;
                      DatenTable.Edit;
                      DatenTable.FieldByName(C_IecDatenDSfG_DatenRefNrBis).AsInteger:=Daten_BisRefNr;
                      DatenTable.FieldByName(C_IecDatenDSfG_DatenDZBis).AsDateTime:=Daten_BisDZ;
                      DatenTable.FieldByName(C_IecDatenDSfG_DatenOrdNrBis).AsInteger:=Daten_BisOrdNr;
                      DatenTable.Post;                                     { Datensatz aktualisieren }
                      if (Daten_BisRefNr <> Gesendet_BisRefNr) AND
                         not DatenTable.FieldByName(C_IecDatenDSfG_Gesperrt).AsBoolean then
                        Result:=true;   { neue Daten da }
                      { -> Es wird auf "ungleich" geprüft, da die Referenznummer bei neueren Daten
                           nicht zwangsläufig höher sein muß. Wenn alle Daten gelöscht wurden,
                           fängt die Referenznummer wieder bei 1 an ! }
                    end else
                      DatenTable.Delete;              { Datensatz rauslöschen }
                    bDbUpdated:=true;
                  end
                  else begin
                    if Daten_BisRefNr > CRefNr_Init then begin                  { neuen Datensatz einfügen }
                      DatenTable.InsertRecord([KDSfGStationId,
                                               KDSfGInstId,
                                               KDSfGArchivNr,
                                               KDSfGKanalNr,
                                               Daten_BisRefNr,
                                               Daten_BisDZ,
                                               Daten_BisOrdNr,
                                               CRefNr_Init,
                                               DT_Init,
                                               COrdNr_Init,
                                               false]);  // nicht gesperrt
                      bDbUpdated:=true;
                      Result:=true;
                    end;
                  end;

                  KonfigTable.Next;
                end; { while not KonfigTable.EOF }
              finally
                WertHexTable.Free;
              end;
            finally
              SatzTable.Free;
            end;
          finally
            DatenTable.Close;
          end;
        finally
          DatenTable.Free;
        end;

        { Trigger-Datei für DSfG-Daten-Tabelle schreiben, wenn Schreibzugriff
          erfolgt; 27.08.29007, WW }
        if bDbUpdated then
          WriteNewTime(FStammDir + DBIecDatenDSfG);
      finally
        KonfigTable.Close;
      end;
    finally
      KonfigTable.Free;
    end;
  finally
    Screen.Cursor:=Save_Cursor;
  end;
end;

{-----------------------------------------------}
function TFormDatenIec.F_Update_DSfG_LB: boolean;
{-----------------------------------------------}
 { Ergebnis: true, wenn neuere DSfG-Automatik-Logbuchdaten vorhanden sind als bisher
  gesendet wurden }
var
  KDSfGInstId: integer;
  KDSfGLogbuchNr: integer;
  KMNrAllg: string [5];
  Daten_BisDZ: TDateTime;
  Gesendet_BisDZ: TDateTime;
  MeldDatTable: TTable;
  MeldKonfQuery: TQueryExt;
  Save_Cursor: TCursor;
  DT_Init: TDateTime;
  bDbUpdated: boolean;

begin
  Result:=false;
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecMeldKonfig) then exit;
  if not TableExists(FStammDb, DBIecMeldDatenDSfG) then exit;
  if not TableExists(FStammDb, C_Tb_WMeldungen) then exit;

  Save_Cursor:=Screen.Cursor;
  Screen.Cursor:=crHourglass;
  try
    {--------------------- Update DSfG-Meldungen -------------------------------}
    DT_Init:=DT_Initialisieren;

    MeldKonfQuery:=TQueryExt.Create (nil);
    try
      MeldKonfQuery.DatabaseName:=FStammDb.DatabaseName;
      { Abfrage über alle projektierten DSfG-Meldungen: }
      MeldKonfQuery.SQL.Add('SELECT');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_DSfGStationID + ',');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_DSfGInstanzID + ',');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_DSfGLogbuchNr + ',');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_MNrAllg + ',');
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_MeldungId + ',');
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_DatumZeit);
      MeldKonfQuery.SQL.Add('FROM');
      MeldKonfQuery.SQL.Add('  ' + DBIecMeldKonfig + ' A,');
      MeldKonfQuery.SQL.Add('  ' + C_Tb_WMeldungen + ' C');
      MeldKonfQuery.SQL.Add('WHERE');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_GeraeteArt + ' = :GeraeteArt and');
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_InstanzId_Archiv + ' = A.' + C_IecMeldKonfig_DSfGInstanzID + ' and');
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_LogbuchNr_Archiv + ' = A.' + C_IecMeldKonfig_DSfGLogbuchNr + ' and');
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_MNrAllg + ' = A.' + C_IecMeldKonfig_MNrAllg + ' and');
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_Benutzer + ' = :Benutzer and');
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_GeraeteArt + ' = :GeraeteArt');
      MeldKonfQuery.SQL.Add('ORDER BY');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_DSfGStationID + ',');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_DSfGInstanzID + ',');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_DSfGLogbuchNr + ',');
      MeldKonfQuery.SQL.Add('  A.' + C_IecMeldKonfig_MNrAllg + ',');
      // Sortieren nach Zeit, damit neueste Meldungen sicher erkannt werden
      // (bisher sortieren nach MeldungId); 16.04.2015, WW
      MeldKonfQuery.SQL.Add('  C.' + C_Tf_WMeldungen_DatumZeit);
      MeldKonfQuery.ParamByName('Benutzer').asString:=C_BenutzerAuto; { Automatik-Daten }
      MeldKonfQuery.ParamByName('GeraeteArt').asString:=C_GerArtDSfG;  { ...aus DSfG-Stationen }
      if MeldKonfQuery.Open then begin
        MeldDatTable:=TTable.Create (nil);
        try
          MeldDatTable.DatabaseName:=FStammDb.DatabaseName;
          MeldDatTable.TableName:=DBIecMeldDatenDSfG;
          MeldDatTable.Open;
          try
            bDbUpdated:=false;
            while not MeldKonfQuery.Eof do begin
              Application.ProcessMessages;
              KDSfGInstId:=MeldKonfQuery.FieldByName(C_IecMeldKonfig_DSfGInstanzId).AsInteger;
              KDSfGLogbuchNr:=MeldKonfQuery.FieldByName(C_IecMeldKonfig_DSfGLogbuchNr).AsInteger;
              KMNrAllg:=MeldKonfQuery.FieldByName(C_IecMeldKonfig_MNrAllg).AsString;
              Daten_BisDZ:=MeldKonfQuery.FieldByName(C_Tf_WMeldungen_DatumZeit).asDateTime;

              { iecmelddatendsfg.db aktualisieren: }
              if MeldDatTable.FindKey ([KDSfGInstId, KDSfGLogbuchNr, KMNrAllg]) then begin
                if Daten_BisDZ <> DT_Init then begin
                  Gesendet_BisDZ:=MeldDatTable.FieldByName(C_IecMeldDatenDSfG_MeldGesendetBis).AsDateTime;
                  MeldDatTable.Edit;
                  MeldDatTable.FieldByName(C_IecMeldDatenDSfG_MeldDatenBis).AsDateTime:=Daten_BisDZ;
                  MeldDatTable.Post;                                     { Datensatz aktualisieren }
                  if (Daten_BisDZ > Gesendet_BisDZ) AND
                     not MeldDatTable.FieldByName(C_IecMeldDatenDSfG_Gesperrt).AsBoolean then
                    Result:=true;         { neue DSfG-Meldungen da }
                  // wenn die Uhr in der Station zurückgestellt wurde, werden alle Meldungen, die
                  // innerhalb des Rückstellzeitraums auflaufen nicht weitergegeben (auch bei MRG so) !
                end else
                  MeldDatTable.Delete;                                     { Datensatz rauslöschen }
                bDbUpdated:=true;
              end
              else begin               { Datensatz in iecmelddatendsfg.db nicht gefunden }
                if Daten_BisDZ <> DT_Init then begin                         { neuen Datensatz einfügen }
                  MeldDatTable.InsertRecord([KDSfGInstId,
                                             KDSfGLogbuchNr,
                                             KMNrAllg,
                                             Daten_BisDZ,
                                             DT_Init,
                                             false]);  // nicht gesperrt
                  bDbUpdated:=true;
                  Result:=true;
                end;
              end;

              MeldKonfQuery.Next;
            end; { while not MeldKonfQuery.EOF }
           finally
            MeldDatTable.Close;
          end;
        finally
          MeldDatTable.Free;
        end;

        { Trigger-Datei für DSfG-MeldDaten-Tabelle schreiben, wenn Schreibzugriff
          erfolgt; 27.08.29007, WW }
        if bDbUpdated then
          WriteNewTime(FStammDir + DBIecMeldDatenDSfG);
      end;  { if MeldKonfQuery.Open }
    finally
      MeldKonfQuery.Close;
      MeldKonfQuery.Free;
    end;
  finally
    Screen.Cursor:=Save_Cursor;
  end;
end;

{-------------------------------------------------------------------------------------------------------}
function TFormDatenIec.UpdateGesendetBis_DSfG (DSfGStationId: integer; DSfGInstanzId: integer;
                                               DSfGArch_LogbNr: integer; DSfGKanal_MeldNr: integer;
                                               RefNr_Bis: integer; DZ_Bis: TDateTime; OrdNr_Bis: integer;
                                               Datentyp: char): boolean;
{-------------------------------------------------------------------------------------------------------}
{ Bis-ReferenzNr/Zeitpunkt/Ordnungsnummer nach erfolgreicher DSfG-Telegrammübertragung
  aktualisieren (getrennt für Archivkanaldaten und Meldungen);
  Übergabe: DSfG-StationId
            DSfG-InstanzId
            DSfG-Archiv/Logbuchnummer
            DSfG-Archivkanal- bzw. Meldungsnummer
            Bis-Referenznummer (Archivdaten)
            Bis-Zeitpunkt
            Bis-Ordnungsnummer
            zu updatender Datentyp ('S' = Archivdaten
                                    'M' = Meldung)
  Ergebnis: true, wenn neues Bis eingetragen werden konnte }
var
  MNrStr: string[5];
  DatenTable: TTable;
  MeldDatTable: TTable;

begin
  Result:=false;
  if (not Assigned(FStammDb)) then Exit;

  if Datentyp = 'S' then begin                  { Archivdaten-Bis-Werte aktualisieren }
    if not TableExists(FStammDb, DBIecDatenDSfG) then exit;

    DatenTable:=TTable.Create (nil);
    try
      DatenTable.DatabaseName:=FStammDb.DatabaseName;
      DatenTable.TableName:=DBIecDatenDSfG;
      DatenTable.Open;
      try
        if DatenTable.FindKey ([DSfGStationId, DSfGInstanzId, DSfGArch_LogbNr, DSfGKanal_MeldNr]) then begin
          DatenTable.Edit;
          DatenTable.FieldByName(C_IecDatenDSfG_RefNrGesendetBis).AsInteger:=RefNr_Bis;
          DatenTable.FieldByName(C_IecDatenDSfG_DZGesendetBis).AsDateTime:=DZ_Bis;
          DatenTable.FieldByName(C_IecDatenDSfG_OrdNrGesendetBis).AsInteger:=OrdNr_Bis;
          DatenTable.Post;                          { Datensatz aktualisieren }
          Result:=true;
        end;
      finally
        DatenTable.Close;
      end;
    finally
      DatenTable.Free;
    end;

    { Trigger-Datei für DSfG-Daten-Tabelle schreiben, wenn Schreibzugriff
      erfolgt; 27.08.29007, WW }
    if Result then
      WriteNewTime(FStammDir + DBIecDatenDSfG);
  end
  else begin                       { Meldungs-Bis-Werte aktualisieren }
    if not TableExists(FStammDb, DBIecMeldDatenDSfG) then exit;

    MeldDatTable:=TTable.Create (nil);
    try
      MeldDatTable.DatabaseName:=FStammDb.DatabaseName;
      MeldDatTable.TableName:=DBIecMeldDatenDSfG;
      MeldDatTable.Open;
      try
        MNrStr:=Format('%.5d',[DSfGKanal_MeldNr]);
        if MeldDatTable.FindKey ([DSfGInstanzId, DSfGArch_LogbNr, MNrStr]) then begin
          MeldDatTable.Edit;
          MeldDatTable.FieldByName(C_IecMeldDatenDSfG_MeldGesendetBis).AsDateTime:=DZ_Bis;
          MeldDatTable.Post;                        { Datensatz aktualisieren }
          Result:=true;
        end;
      finally
        MeldDatTable.Close;
      end;
    finally
      MeldDatTable.Free;
    end;

    { Trigger-Datei für DSfG-MeldDaten-Tabelle schreiben, wenn Schreibzugriff
      erfolgt; 27.08.29007, WW }
    if Result then
      WriteNewTime(FStammDir + DBIecMeldDatenDSfG);
  end;
end;

{-------------------------------------------------------------------------------------------------}
procedure TFormDatenIec.SetGesendetBis_General_DSfG (LinienNr: integer; StationsNr: longint;
                                                     LinienNr_Bytes: byte; StationsNr_Bytes: byte);
{-------------------------------------------------------------------------------------------------}
{ für DSfG-Einträge Feld 'RefNrGesendetBis' nach eingegangenem Generalabfrage-Telegramm auf "Generalabfrage-
  Referenznummer" setzen;
  Übergabe: IEC-Liniennummer (wenn < 0: ohne Liniennummer) }
var
  AlleLinien: boolean;
  AlleStationen: boolean;
  KLinienNr: integer;
  KStationsNr: longint;
  KDSfGStationId: integer;
  KDSfGInstId: integer;
  KDSfGArchivNr: integer;
  KDSfGKanalNr: integer;
  KonfigTable: TTable;
  DatenTable: TTable;
  Daten_BisRefNr: integer;
  Bis: integer;
  bDbUpdated: boolean;

begin
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecKonfig) then exit;
  if not TableExists(FStammDb, DBIecDatenDSfG) then exit;

  if DSfG_GeneralabfrageAnzWerte <= 0 then exit;  { Generalabfrage deaktiviert; 10.12.2004 WW }

  if LinienNr_Bytes = 0 then
    AlleLinien:=true  { ohne Liniennummer }
  else if LinienNr_Bytes > 1 then
    AlleLinien:=LinienNr = $FFFF  { 2 Byte Liniennummer }
  else
    AlleLinien:=LinienNr = $FF;  { 1 Byte Liniennummer }
  if StationsNr_Bytes > 1 then
    AlleStationen:=StationsNr = $FFFF  { 2 Byte Stationsnummer }
  else
    AlleStationen:=StationsNr = $FF;  { 1 Byte Stationsnummer }

  KonfigTable:=TTable.Create (nil);
  try
    KonfigTable.DatabaseName:=FStammDb.DatabaseName;
    KonfigTable.TableName:=DBIecKonfig;
    KonfigTable.Open;
    try
      { Filter auf DSfG-Einträge }
      KonfigTable.Filter:=C_IecKonfig_GeraeteArt + ' = ''' + C_GerArtDSfG + '''';
      KonfigTable.Filtered:=true;                  { Aktivieren des Filters }

      DatenTable:=TTable.Create (nil);
      try
        DatenTable.DatabaseName:=FStammDb.DatabaseName;
        DatenTable.TableName:=DBIecDatenDSfG;
        DatenTable.Open;
        try
          bDbUpdated:=false;
          while not KonfigTable.EOF do begin
            Application.ProcessMessages;
            KLinienNr:=KonfigTable.FieldByName(C_IecMeldKonfig_IEC_LinienNr).AsInteger;
            if not AlleLinien AND (KLinienNr <> LinienNr) then begin
              KonfigTable.Next;
              Continue;
            end;
            KStationsNr:=KonfigTable.FieldByName(C_IecKonfig_IEC_ASDU).AsInteger;
            if not AlleStationen AND (KStationsNr <> StationsNr) then begin
              KonfigTable.Next;
              Continue;
            end;
            KDSfGStationID:=KonfigTable.FieldByName(C_IecKonfig_DSfGStationID).AsInteger;
            KDSfGInstID:=KonfigTable.FieldByName(C_IecKonfig_DSfGInstanzID).AsInteger;
            KDSfGArchivNr:=KonfigTable.FieldByName(C_IecKonfig_DSfGArchivNr).AsInteger;
            KDSfGKanalNr:=KonfigTable.FieldByName(C_IecKonfig_KanalNr).AsInteger;

            { iecdatendsfg.db aktualisieren: }
            if DatenTable.FindKey ([KDSfGStationID, KDSfGInstID, KDSfGArchivNr, KDSfGKanalNr]) then begin
              Daten_BisRefNr:=DatenTable.FieldByName(C_IecDatenDSfG_DatenRefNrBis).AsInteger;
              { GesendetBis-RefNr für DSfG-Generalabfrage ermitteln (Anzahl der letzten Werte zurück): }
              Bis:=Daten_BisRefNr - DSfG_GeneralabfrageAnzWerte;
              if Bis < 1 then
                Bis:=1;      { mit der ersten Referenznummer beginnen, 10.12.2004 WW }

              // RefNrGesendet-bis darf nicht kleiner als Bis sein, 06.10.2004 WW
              if DatenTable.FieldByName(C_IecDatenDSfG_RefNrGesendetBis).AsInteger > Bis then begin
                DatenTable.Edit;
                DatenTable.FieldByName(C_IecDatenDSfG_RefNrGesendetBis).AsInteger:=Bis;
                DatenTable.FieldByName(C_IecDatenDSfG_DZGesendetBis).Clear;                      { Feld leeren, da nicht bekannt }
                DatenTable.FieldByName(C_IecDatenDSfG_OrdNrGesendetBis).AsInteger:=COrdNr_Init;  { mit Init-OrdNr belegen }
                { -> durch das belegen mit der Init-Ordnungsnummer wird definiert, daß
                     Zeitstempel und Ordnungsnummer des Satzes nicht bekannt sind.
                     (Prüfung in Prozedur SendTelegramm_DatenKl2_DSfG in
                     FIecTelegr.pas); 10.12.2004, WW }
                DatenTable.Post;                    { Datensatz aktualisieren }
                bDbUpdated:=true;
              end;
            end;
            KonfigTable.Next;
          end; { while not KonfigTable.EOF }
        finally
          DatenTable.Close;
        end;
      finally
        DatenTable.Free;
      end;

      { Trigger-Datei für DSfG-Daten-Tabelle schreiben, wenn Schreibzugriff
        erfolgt; 27.08.29007, WW }
      if bDbUpdated then
        WriteNewTime(FStammDir + DBIecDatenDSfG);
    finally
      KonfigTable.Close;
    end;
  finally
    KonfigTable.Free;
  end;
end;

{------------------------------------------------------------------------------------------}
procedure TFormDatenIec.F_Lock_DSfG_AK (AStationId, AInstanzId, AArchNr, AKanalNr: integer);
{------------------------------------------------------------------------------------------}
var
  q: TQuery;

begin
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecDatenDSfG) then exit;

  q:=TQuery.Create(nil);
  try
    with q do begin
      DatabaseName:=FStammDb.DatabaseName;
      Sql.Clear;
      Sql.Add('UPDATE ' + DBIecDatenDSfG);
      Sql.Add('SET ' + C_IecDatenDSfG_Gesperrt + ' = :Gesperrt');
      Sql.Add('WHERE ' + C_IecDatenDSfG_StationID + ' = :StationId and');
      Sql.Add(C_IecDatenDSfG_InstanzID + ' = :InstanzId and');
      Sql.Add(C_IecDatenDSfG_ArchivNr + ' = :ArchNr and');
      Sql.Add(C_IecDatenDSfG_KanalNr + ' = :KanalNr');
      ParamByName('Gesperrt').asBoolean:=true;
      ParamByName('StationId').AsInteger:=AStationId;
      ParamByName('InstanzId').AsInteger:=AInstanzId;
      ParamByName('ArchNr').AsInteger:=AArchNr;
      ParamByName('KanalNr').AsInteger:=AKanalNr;
      ExecSql;
    end; // with q
  finally
    q.free;
  end;

  { Trigger-Datei für DSfG-Daten-Tabelle schreiben, da Schreibzugriff
    erfolgt; 27.08.29007, WW }
  WriteNewTime(FStammDir + DBIecDatenDSfG);
end;

{-----------------------------------------------------------------------------}
procedure TFormDatenIec.F_Lock_DSfG_LB (AInstanzId, ALogbNr, AMeldNr: integer);
{-----------------------------------------------------------------------------}
var
  q: TQuery;

begin
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecMeldDatenDSfG) then exit;

  q:=TQuery.Create(nil);
  try
    with q do begin
      DatabaseName:=FStammDb.DatabaseName;
      Sql.Clear;
      Sql.Add('UPDATE ' + DBIecMeldDatenDSfG);
      Sql.Add('SET ' + C_IecMeldDatenDSfG_Gesperrt + ' = :Gesperrt');
      Sql.Add('WHERE ' + C_IecMeldDatenDSfG_InstanzID + ' = :InstanzId and');
      Sql.Add(C_IecMeldDatenDSfG_LogbuchNr + ' = :LogbNr and');
      Sql.Add(C_IecMeldDatenDSfG_MNrAllg + ' = :MeldNr');
      ParamByName('Gesperrt').asBoolean:=true;
      ParamByName('InstanzId').AsInteger:=AInstanzId;
      ParamByName('LogbNr').AsInteger:=ALogbNr;
      ParamByName('MeldNr').AsString:=Format('%.5d', [AMeldNr]);
      ExecSql;
    end; // with q
  finally
    q.free;
  end;

  { Trigger-Datei für DSfG-MeldDaten-Tabelle schreiben, da Schreibzugriff
    erfolgt; 27.08.29007, WW }
  WriteNewTime(FStammDir + DBIecMeldDatenDSfG);
end;

{---------------------------------------}
procedure TFormDatenIec.F_Unlock_DSfG_AK;
{---------------------------------------}
var
  q: TQuery;

begin
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecDatenDSfG) then exit;

  q:=TQuery.Create(nil);
  try
    with q do begin
      DatabaseName:=FStammDb.DatabaseName;
      Sql.Clear;
      Sql.Add('UPDATE ' + DBIecDatenDSfG);
      Sql.Add('SET ' + C_IecDatenDSfG_Gesperrt + ' = :Gesperrt');
      Sql.Add('WHERE ' + C_IecDatenDSfG_Gesperrt + ' <> :Gesperrt');
      ParamByName('Gesperrt').asBoolean:=false;
      ExecSql;
    end; // with q
  finally
    q.free;
  end;

  { Trigger-Datei für DSfG-Daten-Tabelle schreiben, da Schreibzugriff
    erfolgt; 27.08.29007, WW }
  WriteNewTime(FStammDir + DBIecDatenDSfG);
end;

{---------------------------------------}
procedure TFormDatenIec.F_Unlock_DSfG_LB;
{---------------------------------------}
var
  q: TQuery;

begin
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecMeldDatenDSfG) then exit;

  q:=TQuery.Create(nil);
  try
    with q do begin
      DatabaseName:=FStammDb.DatabaseName;
      Sql.Clear;
      Sql.Add('UPDATE ' + DBIecMeldDatenDSfG);
      Sql.Add('SET ' + C_IecMeldDatenDSfG_Gesperrt + ' = :Gesperrt');
      Sql.Add('WHERE ' + C_IecMeldDatenDSfG_Gesperrt + ' <> :Gesperrt');
      ParamByName('Gesperrt').asBoolean:=false;
      ExecSql;
    end; // with q
  finally
    q.free;
  end;

  { Trigger-Datei für DSfG-MeldDaten-Tabelle schreiben, da Schreibzugriff
    erfolgt; 27.08.29007, WW }
  WriteNewTime(FStammDir + DBIecMeldDatenDSfG);
end;


{------------------------ Daten in Datenliste laden ---------------------------}

{-------------------------------------------------------------------------------------}
function TFormDatenIec.GetNeueDatenMRG_Mess (IECLinienNr: integer; InfoObj_Bytes: byte;
  var IEC_TypKennung: byte; var KonfigMRG_Mess: TKonfigMRG_Mess; var AufzMax: integer;
  var LogHeader: string; DatenListe: TDatenListe; bSperren: boolean): boolean;
{-------------------------------------------------------------------------------------}
{ Liste mit zu übertragenden MRG-Messwerten füllen und zurückgeben;
  -> Die Datenliste wird mit den Daten eines Kanals gefüllt.
  -> Alle Rückgaben enthalten die dem Kanal zugeordneten Werte.
  Übergabe: IEC-Liniennummer (wenn < 0: ohne Liniennummer) }
var
  BisherDatum: TDateTime;
  BisherOrdNr: integer;
  MrgKanaltyp: string;
  MrgTyp: integer;
  MrgAufzMax: integer;
  DZ: TDateTime;
  MessDaten: TMessDaten;
  MessDatenObj: TMessDatenObj;

begin
  Result:=false;

  // Vorbelegungen für Rückgaben:
  IEC_TypKennung:=0;
  with KonfigMRG_Mess do begin
    Kennung:='';
    KanalNr:=0;
    with IEC do begin
      LinienNr:=IECLinienNr;  // wie in Übergabe
      StationsNr:=0;
      InfoObj_high:=0;
      InfoObj_medium:=0;
      InfoObj_low:=0;
    end;
  end;
  AufzMax:=0;
  LogHeader:='';
  if not Assigned(DatenListe) then exit;
  DatenListe.Clear;

  { Tabellen auf Vorhandensein prüfen: }
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecKonfig) then exit;
  if not TableExists(FStammDb, DBIecDatenMRG) then exit;
  if not TableExists(FStammDb, CDBSta) then exit;
  if not TableExists(FStammDb, CDBMrgKanal) then exit;

  { INI neu lesen, wenn geändert; 10.03.2015, WW }
  GetIniDaten (true);

  { Abfrage aller der IEC-Liniennummer zugeordneten und zu übertragenden MRG-Messwerte
    (Impuls- und Analogwerte): }
  DatenTelegrQuery.Close;
  DatenTelegrQuery.DatabaseName:=FStammDb.DatabaseName;
  DatenTelegrQuery.SQL.Clear;
  DatenTelegrQuery.SQL.Add('SELECT');
  DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenMRG_MrgKennung + ',');
  DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenMRG_KanalNr + ',');
  DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenMRG_OrdNrGesendetBis + ',');
  DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenMRG_DZGesendetBis + ',');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_Kanaltyp + ',');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_IEC_ASDU + ',');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_IEC_InfoObj_high + ',');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_IEC_InfoObj_medium + ',');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_IEC_InfoObj_low + ',');
  DatenTelegrQuery.SQL.Add('  C.' + C_Sta_MrgTyp + ',');  // erweitert für MRG-Originalwerte, 05.09.2005 WW
  DatenTelegrQuery.SQL.Add('  C.' + C_Sta_ArchDatenTyp + ',');  // erweitert für MRG-Originalwerte, 05.09.2005 WW
  DatenTelegrQuery.SQL.Add('  E.' + C_MrgAna_AufzMax);
  DatenTelegrQuery.SQL.Add('FROM');
  DatenTelegrQuery.SQL.Add('  ' + DBIecKonfig + ' A,');
  DatenTelegrQuery.SQL.Add('  ' + DBIecDatenMRG + ' B,');
  DatenTelegrQuery.SQL.Add('  ' + CDBSta + ' C,');
  DatenTelegrQuery.SQL.Add('  ' + CDBMrgKanal + ' D');
  DatenTelegrQuery.SQL.Add('LEFT JOIN ' + CDBMrgAna + ' E');
  DatenTelegrQuery.SQL.Add('ON E.' + C_MrgAna_KanalId + ' = D.' + C_MrgKanal_KanalId);

  DatenTelegrQuery.SQL.Add('WHERE');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_GeraeteArt + ' = :GerArt and');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_MrgKennung + ' = B.' + C_IecDatenMRG_MrgKennung + ' and');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_KanalNr + ' = B.' + C_IecDatenMRG_KanalNr + ' and');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_MrgKennung + ' = C.' + C_Sta_Kennung + ' and');
  DatenTelegrQuery.SQL.Add('  C.' + C_Sta_Aktiv + ' = 0 and');
  DatenTelegrQuery.SQL.Add('  C.' + C_Sta_MrgTyp + ' = D.' + C_MrgKanal_MrgTyp + ' and');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_KanalNr + ' = D.' + C_MrgKanal_MrgKanal + ' and');
  if IECLinienNr >= 0 then
    DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_IEC_LinienNr + ' = :IECLiniennummer and');
  DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenMRG_Gesperrt + ' =:Gesperrt and');  // 29.06.2007, WW
  DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenMRG_DatenOrdNrBis + ' <> B.' + C_IecDatenMRG_OrdNrGesendetbis);

  DatenTelegrQuery.ParamByName('GerArt').asString:=C_GerArtMrg;   { nur MRG-Einträge }
  DatenTelegrQuery.ParamByName('Gesperrt').asBoolean:=false;   { nur nicht-gesperrte Einträge }
  if IECLinienNr >= 0 then
    DatenTelegrQuery.ParamByName('IECLiniennummer').asInteger:=IECLinienNr;
  DatenTelegrQuery.Open;
  while not DatenTelegrQuery.EOF do begin
    Application.ProcessMessages;
    MrgKanaltyp:=DatenTelegrQuery.FieldByName(C_IecKonfig_Kanaltyp).AsString;
    KonfigMRG_Mess.IEC.StationsNr:=DatenTelegrQuery.FieldByName(C_IecKonfig_IEC_ASDU).AsInteger;
    KonfigMRG_Mess.IEC.InfoObj_high:=DatenTelegrQuery.FieldByName(C_IecKonfig_IEC_InfoObj_high).AsInteger;
    KonfigMRG_Mess.IEC.InfoObj_medium:=DatenTelegrQuery.FieldByName(C_IecKonfig_IEC_InfoObj_medium).AsInteger;
    KonfigMRG_Mess.IEC.InfoObj_low:=DatenTelegrQuery.FieldByName(C_IecKonfig_IEC_InfoObj_low).AsInteger;
    KonfigMRG_Mess.KanalNr:=DatenTelegrQuery.FieldByName(C_IecDatenMRG_KanalNr).AsInteger;
    KonfigMRG_Mess.Kennung:=DatenTelegrQuery.FieldByName(C_IecDatenMRG_MRGKennung).AsString;
    BisherOrdNr:=DatenTelegrQuery.FieldByName(C_IecDatenMRG_OrdNrGesendetBis).AsInteger;
    BisherDatum:=DatenTelegrQuery.FieldByName(C_IecDatenMRG_DZGesendetBis).AsDateTime;
    MrgTyp:=DatenTelegrQuery.FieldByName(C_Sta_MrgTyp).AsInteger;
    MrgAufzMax:=DatenTelegrQuery.FieldByName(C_MrgAna_AufzMax).AsInteger;

    // Sta-Flag für LGZ-Originalwerte-Zugriff abfragen und in LGZ-Komponente setzen:
    LGZStd.IsOriData:=DatenTelegrQuery.FieldByName(C_Sta_ArchDatenTyp).AsInteger <> mrg_arch_LGZnorm;

    // dem Kanal zugeordnete Rückgaben belegen:
    // Kopf für Log-Eintrag des Kanals:
    LogHeader:='MRG  Kennung: >' + KonfigMRG_Mess.Kennung +
               '<  Kanal: ' + IntToStr (KonfigMRG_Mess.KanalNr) +
               ' (' + MRGKanaltyp + ')' +
               '  ' + GetIEC_LogHeader (KonfigMRG_Mess.IEC, InfoObj_Bytes);

    // Aufzeichnungsbereich der Kanaldaten
    if LGZStd.isOriData then
      AufzMax:=MrgAufzMax         // max. Aufzeichnungsbereich des MRG-Typ
    else
      AufzMax:=C_AufzMaxLGZnorm;  // max. Aufzeichnungsbereich der LGZ-Daten
    // IEC-Typkennung für die Kanaldaten:
    if MrgKanaltyp = 'A' then begin                         { Analogkanal }
      { Elster DL210/220/240, EK260 und RMG EC694 liefern Analog-Originalwerte als
        Gleitkommazahlen, deshalb mit entsprechendem Telegramm-Typ übertragen: }
      if LGZStd.IsOriData AND
         ((MrgTyp = mrgtyp_DL210) OR (MrgTyp = mrgtyp_DL220) OR (MrgTyp = mrgtyp_DL240) OR
          (MrgTyp = mrgtyp_EK260) OR (MrgTyp = mrgtyp_EC694)) then
        IEC_TypKennung:=GetIEC_Typkennung (iecpi_Messwert_Gleitkomma, DualeZeit2a_Bytes)
      else
        IEC_TypKennung:=GetIEC_Typkennung (iecpi_Messwert_normiert, DualeZeit2a_Bytes);
    end
    else begin                                              { Impulskanal }
      { Elster DL210/220/240 und EK260 liefern Impuls-Originalwerte als Gleitkommazahlen,
        deshalb mit entsprechendem Telegramm-Typ übertragen: }
      if LGZStd.IsOriData AND
         ((MrgTyp = mrgtyp_DL210) OR (MrgTyp = mrgtyp_DL220) OR (MrgTyp = mrgtyp_DL240) OR
          (MrgTyp = mrgtyp_EK260)) then
        IEC_TypKennung:=GetIEC_Typkennung (iecpi_Messwert_Gleitkomma, DualeZeit2a_Bytes)
      else
        IEC_TypKennung:=GetIEC_Typkennung (iecpi_Zaehlwert, DualeZeit2a_Bytes);
    end;

    // zu übertragende MRG-Messwerte in Datenliste laden:
    if (KonfigMRG_Mess.KanalNr < 1) OR (KonfigMRG_Mess.KanalNr > LGZStd.Kanalzahl) then begin
      DatenTelegrQuery.Next;
      Continue;
    end;
    if not LGZStd.Search(KonfigMRG_Mess.Kennung) then begin
      DatenTelegrQuery.Next;
      Continue;
    end;

    try
      LGZStd.Open;
      try
        if LGZStd.Size <= 0 then begin
          DatenTelegrQuery.Next;
          Continue;
        end;

        if BisherOrdNr = COrdNr_Init then begin   { bisher wurde für die Kennung noch nichts übertragen }
          { LGZStd so positionieren, daß die letzten n Werte übertragen
            werden (n = MRG_ErsteDatenAnzWerte): }
          if LGZStd.Size > MRG_ErsteDatenAnzWerte then
            LGZStd.Seek (MRG_ErsteDatenAnzWerte*(-1), soFromEnd)
          else
            LGZStd.First;
        end
        else begin
          if LGZStd.SearchOrdnungsnummer (BisherOrdNr) then begin
            { LGZ-Satz mit Ordnungsnummer des letzten bisher übertragenen Satzes gefunden }
            DZ:=LGZStd.Datum;

            if (CmpDateTime (DZ, BisherDatum) = 0) OR
               DatenTelegrQuery.FieldByName(C_IecDatenMRG_DZGesendetBis).IsNull then begin
              { auch Zeitstempel des LGZ-Satzes stimmt mit letztem bisher übertragenem
                Satz überein (wenn Datum/Zeit des letzten übertragenen Satzes nicht
                bekannt ist (leer bei Generalabfrage !), nicht prüfen):
                -> letzter übertragener Satz wurde in LGZ-Daten gefunden, ab nächstem
                   LGZ-Satz übertragen }
              LGZStd.Next;
              if LGZStd.Eof then begin
                DatenTelegrQuery.Next;
                Continue;
              end;
            end else
              { OrdNr, Zeitstempel stimmen nicht überein (LGZ-Daten der Kennung wurden
                offensichtlich ganz gelöscht): gesamte LGZ-Daten übertragen }
              LGZStd.First;
          end
          else begin
            { LGZ-Satz mit OrdNr des letzten bisher übertragenen Satzes wurde
              nicht gefunden }
            LGZStd.Last;
            DZ:=LGZStd.Datum;
            if DZ < BisherDatum then begin
              { keine neueren Werte in LGZ-Daten vorhanden (jüngste LGZ-Daten
                wurden offensichtlich gelöscht): weiter mit nächstem Query-Eintrag }
              DatenTelegrQuery.Next;
              Continue;
            end else
              { neuere Werte vorhanden (LGZ-Daten wurden offensichtlich ganz
                gelöscht): gesamte LGZ-Daten übertragen }
              LGZStd.First;
          end;
        end;

        // schneller Zugriff auf alle vorhandenen LGZ-Originaldaten ab Ordnungsnummer
        // des aktuellen LGZ-Satzes
        LGZStd.Prepare (LGZStd.Ordnungsnummer, 0);
        try
          while not LGZStd.Eof do begin
            Application.ProcessMessages;

            with MessDaten do begin
              Wert:=LGZStd.Wert[KonfigMRG_Mess.KanalNr-1];
              { Prüfung, ob Wert gültig oder ungültig ist: }
              if (LGZStd.Satzstatus <> 0) OR
                 ((LGZStd.Kanalstatus[KonfigMRG_Mess.KanalNr-1] AND $89) <> 0) then
                Wert_gueltig:=false  { Wert ungültig bei Satzstatus: Uhr gestellt, Revision und Kanalstatus: Überlauf, inaktiv, fehlend }
              else
                Wert_gueltig:=true;
              DatumZeit:=LGZStd.Datum;
              Ordnungsnummer:=LGZStd.Ordnungsnummer;
              Referenznummer:=0;  // wird bei MRG-Daten nicht verwendet
              { Datensatz-LogInfo }
              LogInfo:='-> DZ: ' + FormatDateTime ('dd.mm.yyyy hh:nn:ss', LGZStd.Datum) +
                       '  Wert: ' + FloatToStr (LGZStd.Wert[KonfigMRG_Mess.KanalNr-1]) +
                       '  S-St: ' + IntTostr (LGZStd.Satzstatus) +
                       '  K-St: ' + IntToStr (LGZStd.Kanalstatus[KonfigMRG_Mess.KanalNr-1]);
            end;
            MessDatenObj:=TMessDatenObj.Create(MessDaten);
            DatenListe.Add(MessDatenObj);  // Messwert in Datenliste anhängen

            Result:=true;  // Ergebnis true, wenn Datenliste mind. 1 Eintrag enthält
            LGZStd.Next;
          end;  { while not LGZStd.Eof }
        finally
          LGZStd.UnPrepare;
        end;
      finally
        LGZStd.Close;
      end;
    except
      on Exception do;
    end;

    if Result then Break;  // nur MRG-Messwerte eines Kanals in Datenliste laden

    DatenTelegrQuery.Next;
  end;  { while not DatenQuery.Eof }
  DatenTelegrQuery.Close;

  { Sperre setzen, wenn Daten geladen wurden: 02.07.2007, WW }
  if Result AND bSperren then
    F_Lock_MRG_MW (KonfigMRG_Mess.Kennung, KonfigMRG_Mess.KanalNr);
end;

{-------------------------------------------------------------------------------------}
function TFormDatenIec.GetNeueDatenMRG_Meld (IECLinienNr: integer; InfoObj_Bytes: byte;
  var IEC_TypKennung: byte; var KonfigMRG_Meld: TKonfigMRG_Meld; var LogHeader: string;
  DatenListe: TDatenListe; bSperren: boolean): boolean;
{-------------------------------------------------------------------------------------}
{ Liste mit zu übertragenden MRG-Meldungen füllen und zurückgeben;
  -> Die Datenliste wird mit den Daten einer Meldung gefüllt.
  -> Alle Rückgaben enthalten die der Meldung zugeordneten Werte.
  Übergabe: IEC-Liniennummer (wenn < 0: ohne Liniennummer) }
var
  BisherDatum: TDateTime;
  WMeldungenQuery: TQueryExt;
  MrgId: integer;
  MeldDatumZeit: TDateTime;
  MeldTyp: string;
  MeldDaten: TMeldDaten;
  MeldDatenObj: TMeldDatenObj;

begin
  Result:=false;
  // Vorbelegungen für Rückgaben:
  IEC_TypKennung:=GetIEC_Typkennung (iecpi_Einzelmeldung, DualeZeit2a_Bytes);
  with KonfigMRG_Meld do begin
    Kennung:='';
    MeldNr:=0;
    with IEC do begin
      LinienNr:=IECLinienNr;  // wie in Übergabe
      StationsNr:=0;
      InfoObj_high:=0;
      InfoObj_medium:=0;
      InfoObj_low:=0;
    end;
  end;
  LogHeader:='';
  if not Assigned(DatenListe) then exit;
  DatenListe.Clear;

  { Tabellen auf Vorhandensein prüfen: }
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecMeldKonfig) then exit;
  if not TableExists(FStammDb, DBIecMeldDatenMRG) then exit;
  if not TableExists(FStammDb, CDBSta) then exit;
  if not TableExists(FStammDb, C_Tb_WMeldungen) then exit;

  { INI neu lesen, wenn geändert; 10.03.2015, WW }
  GetIniDaten (true);

  { Abfrage aller der IEC-Liniennummer zugeordneten und zu übertragenden MRG-Meldungen: }
  DatenTelegrQuery.Close;
  DatenTelegrQuery.DatabaseName:=FStammDb.DatabaseName;
  DatenTelegrQuery.SQL.Clear;
  DatenTelegrQuery.SQL.Add('SELECT');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_MrgKennung + ',');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_MNrAllg + ',');         { nur Nummer für Kommt-Meldungen }
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_IEC_ASDU + ',');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_IEC_InfoObj_high + ',');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_IEC_InfoObj_medium + ',');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_IEC_InfoObj_low + ',');
  DatenTelegrQuery.SQL.Add('  B.' + C_IecMeldDatenMRG_MeldGesendetBis + ',');
  DatenTelegrQuery.SQL.Add('  C.' + C_Sta_MrgId);
  DatenTelegrQuery.SQL.Add('FROM');
  DatenTelegrQuery.SQL.Add('  ' + DBIecMeldKonfig + ' A,');
  DatenTelegrQuery.SQL.Add('  ' + DBIecMeldDatenMRG + ' B,');
  DatenTelegrQuery.SQL.Add('  ' + CDBSta + ' C');
  DatenTelegrQuery.SQL.Add('WHERE');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_GeraeteArt + ' = :GerArt and');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_MrgKennung + ' = B.' + C_IecMeldDatenMRG_MrgKennung + ' and');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_MNrAllg + ' = B.' + C_IecMeldDatenMRG_MNrAllg + ' and');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_MrgKennung + ' = C.' + C_Sta_Kennung + ' and');
  DatenTelegrQuery.SQL.Add('  C.' + C_Sta_Aktiv + ' = 0 and');
  if IECLinienNr >= 0 then
    DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_IEC_LinienNr + ' = :IECLiniennummer and');
  DatenTelegrQuery.SQL.Add('  B.' + C_IecMeldDatenMRG_Gesperrt + ' =:Gesperrt and');  // 29.06.2007, WW
  DatenTelegrQuery.SQL.Add('  B.' + C_IecMeldDatenMRG_LgzDatenBis + ' > B.' + C_IecMeldDatenMRG_MeldGesendetBis + ' and');
  DatenTelegrQuery.SQL.Add('  B.' + C_IecMeldDatenMRG_LgzDatenBis + ' > :ErsteDatenDatum');

  DatenTelegrQuery.ParamByName('GerArt').asString:=C_GerArtMrg;   { nur MRG-Einträge }
  DatenTelegrQuery.ParamByName('Gesperrt').asBoolean:=false;   { nur nicht-gesperrte Einträge }
  DatenTelegrQuery.ParamByName('ErsteDatenDatum').asDateTime:=ErsteMeldungenDatum;
  if IECLinienNr >= 0 then
    DatenTelegrQuery.ParamByName('IECLiniennummer').asInteger:=IECLinienNr;
  DatenTelegrQuery.Open;

  WMeldungenQuery:=TQueryExt.Create (nil);
  try
    WMeldungenQuery.DatabaseName:=FStammDb.DatabaseName;

    while not DatenTelegrQuery.EOF do begin
      Application.ProcessMessages;
      KonfigMRG_Meld.IEC.StationsNr:=DatenTelegrQuery.FieldByName(C_IecMeldKonfig_IEC_ASDU).AsInteger;
      KonfigMRG_Meld.IEC.InfoObj_high:=DatenTelegrQuery.FieldByName(C_IecMeldKonfig_IEC_InfoObj_high).AsInteger;
      KonfigMRG_Meld.IEC.InfoObj_medium:=DatenTelegrQuery.FieldByName(C_IecMeldKonfig_IEC_InfoObj_medium).AsInteger;
      KonfigMRG_Meld.IEC.InfoObj_low:=DatenTelegrQuery.FieldByName(C_IecMeldKonfig_IEC_InfoObj_low).AsInteger;
      KonfigMRG_Meld.MeldNr:=DatenTelegrQuery.FieldByName(C_IecMeldKonfig_MNrAllg).AsInteger;
      KonfigMRG_Meld.Kennung:=DatenTelegrQuery.FieldByName(C_IecMeldKonfig_MRGKennung).AsString;
      BisherDatum:=DatenTelegrQuery.FieldByName(C_IecMeldDatenMRG_MeldGesendetBis).AsDateTime;
      MrgId:=DatenTelegrQuery.FieldByName(C_Sta_MrgId).AsInteger;

      // der Meldungsnummer zugeordnete Rückgaben belegen:
      // Kopf für Log-Eintrag der Meldung:
      LogHeader:='MRG  Kennung: >' + KonfigMRG_Meld.Kennung +
                  '<  MNrAllg: ' + IntToStr (KonfigMRG_Meld.MeldNr) +
                  '  ' + GetIEC_LogHeader (KonfigMRG_Meld.IEC, InfoObj_Bytes);

      // zu übertragende MRG-Meldungen in Datenliste laden:
      if CmpDateTime (BisherDatum, DT_Initialisieren) = 0 then
        BisherDatum:=ErsteMeldungenDatum;

      WMeldungenQuery.Close;
      WMeldungenQuery.SQL.Clear;
      WMeldungenQuery.SQL.Add('SELECT');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_DatumZeit + ',');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_MNrAllg+ ',');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_MTyp);
      WMeldungenQuery.SQL.Add('FROM');
      WMeldungenQuery.SQL.Add(C_Tb_WMeldungen);
      WMeldungenQuery.SQL.Add('WHERE');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_Benutzer + ' = :Benutzer and');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_GeraeteArt + ' = :GeraeteArt and');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_GeraeteId + ' = :MrgId and');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_MNrAllg + ' = :MeldNr and');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_DatumZeit + ' > :BisherDatum');
      WMeldungenQuery.ParamByName('Benutzer').asString:=C_BenutzerAuto; { Automatik-Daten }
      WMeldungenQuery.ParamByName('GeraeteArt').asString:=C_GerArtMrg;  { ...aus MRG's }
      WMeldungenQuery.ParamByName('MrgId').asInteger:=MrgId;
      WMeldungenQuery.ParamByName('MeldNr').asString:=Format('%.5d', [KonfigMRG_Meld.MeldNr]);
      WMeldungenQuery.ParamByName('BisherDatum').asDateTime:=BisherDatum;

      if not WMeldungenQuery.Open then begin
        DatenTelegrQuery.Next;
        Continue;
      end;
      if WMeldungenQuery.RecordCount < 1 then begin
        DatenTelegrQuery.Next;
        Continue;
      end;

      while not WMeldungenQuery.Eof do begin
        Application.ProcessMessages;
        MeldDatumZeit:=WMeldungenQuery.FieldByName(C_Tf_WMeldungen_DatumZeit).AsDateTime;
        MeldTyp:=WMeldungenQuery.FieldByName(C_Tf_WMeldungen_MTyp).AsString;
        with MeldDaten do begin
          if MeldTyp <> 'G' then  { Kommt- bzw. einwertige Meldung }
            Ein:=true    { Ein bei Kommt-Meldung }
          else
            Ein:=false;  { Aus bei Geht-Meldung }
          DatumZeit:=MeldDatumZeit;
          { Datensatz-LogInfo }
          LogInfo:='-> DZ: ' + FormatDateTime ('dd.mm.yyyy hh:nn:ss', MeldDatumZeit) +
                   '  M-Typ: ' + MeldTyp;
        end;
        MeldDatenObj:=TMeldDatenObj.Create(MeldDaten);
        DatenListe.Add(MeldDatenObj);  // Meldung in Datenliste anhängen

        Result:=true;  // Ergebnis true, wenn Datenliste mind. 1 Eintrag enthält
        WMeldungenQuery.Next;
      end;  { while not WMeldungenQuery.Eof }

      if Result then Break;  // nur MRG-Meldungen einer Meldungsnummer in Datenliste laden

      DatenTelegrQuery.Next;
    end;  { while not DatenQuery.Eof }
    DatenTelegrQuery.Close;
  finally
    WMeldungenQuery.Close;
    WMeldungenQuery.Free;
  end;

  { Sperre setzen, wenn Daten geladen wurden: 02.07.2007, WW }
  if Result AND bSperren then
    F_Lock_MRG_ME (KonfigMRG_Meld.Kennung, KonfigMRG_Meld.MeldNr);
end;

{---------------------------------------------------------------------------------------}
function TFormDatenIec.GetNeueDatenDSfG_Arch (IECLinienNr: integer; InfoObj_Bytes: byte;
  var IEC_TypKennung: byte; var KonfigDSfG_Arch: TKonfigDSfG_Arch; var LogHeader: string;
  DatenListe: TDatenListe; bSperren: boolean): boolean;
{---------------------------------------------------------------------------------------}
{ Liste mit zu übertragenden DSfG-Archivdaten füllen und zurückgeben;
  -> Die Datenliste wird mit den Daten eines Archivkanals gefüllt.
  -> Alle Rückgaben enthalten die dem Archivkanal zugeordneten Werte.
  Übergabe: IEC-Liniennummer (wenn < 0: ohne Liniennummer) }
var
  SatzTable: TTableExt;
  WertHexTable: TTableExt;
  DSfGKanaltyp: string;
  BisherRefNr: integer;
  BisherDatum: TDateTime;
  BisherOrdNr: integer;
  Idx: integer;
  OrdNr: integer;
  DZ: TDateTime;
  RefNr: integer;
  MessDaten: TMessDaten;
  MessDatenObj: TMessDatenObj;

begin
  Result:=false;
  // Vorbelegungen für Rückgaben:
  IEC_TypKennung:=0;
  with KonfigDSfG_Arch do begin
    StationId:=0;
    InstanzId:=0;
    ArchNr:=0;
    KanalNr:=0;
    with IEC do begin
      LinienNr:=IECLinienNr;  // wie in Übergabe
      StationsNr:=0;
      InfoObj_high:=0;
      InfoObj_medium:=0;
      InfoObj_low:=0;
    end;
  end;
  LogHeader:='';
  if not Assigned(DatenListe) then exit;
  DatenListe.Clear;

  { Tabellen auf Vorhandensein prüfen: }
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecKonfig) then exit;
  if not TableExists(FStammDb, DBIecDatenDSfG) then exit;

  { INI neu lesen, wenn geändert; 10.03.2015, WW }
  GetIniDaten (true);

  SatzTable:=TTableExt.Create (nil);
  try
    SatzTable.DatabaseName:=FAutoDb.DatabaseName;
    WertHexTable:=TTableExt.Create (nil);
    try
      WertHexTable.DatabaseName:=FAutoDb.DatabaseName;

      { Abfrage aller der IEC-Liniennummer zugeordneten und zu übertragenden DSfG-Archivwerte: }
      DatenTelegrQuery.Close;
      DatenTelegrQuery.DatabaseName:=FStammDb.DatabaseName;
      DatenTelegrQuery.SQL.Clear;
      DatenTelegrQuery.SQL.Add('SELECT');
      DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenDSfG_StationID + ',');
      DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenDSfG_InstanzID + ',');
      DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenDSfG_ArchivNr + ',');
      DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenDSfG_KanalNr + ',');
      DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenDSfG_RefNrGesendetBis + ',');
      DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenDSfG_DZGesendetBis + ',');
      DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenDSfG_OrdNrGesendetBis + ',');
      DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_Kanaltyp + ',');
      DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_IEC_ASDU + ',');
      DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_IEC_InfoObj_high + ',');
      DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_IEC_InfoObj_medium + ',');
      DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_IEC_InfoObj_low);
      DatenTelegrQuery.SQL.Add('FROM');
      DatenTelegrQuery.SQL.Add('  ' + DBIecKonfig + ' A,');
      DatenTelegrQuery.SQL.Add('  ' + DBIecDatenDSfG + ' B');
      DatenTelegrQuery.SQL.Add('WHERE');
      DatenTelegrQuery.SQL.Add('  A. '+ C_IecKonfig_GeraeteArt + ' = :GerArt and');
      DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_DSfGStationID + ' = B.' + C_IecDatenDSfG_StationID + ' and');
      DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_DSfGInstanzID + ' = B.' + C_IecDatenDSfG_InstanzID + ' and');
      DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_DSfGArchivNr + ' = B.' + C_IecDatenDSfG_ArchivNr + ' and');
      DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_KanalNr + ' = B.' + C_IecDatenDSfG_KanalNr + ' and');
      if IECLinienNr >= 0 then
        DatenTelegrQuery.SQL.Add('  A.' + C_IecKonfig_IEC_LinienNr + ' = :IECLiniennummer and');
      DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenDSfG_Gesperrt + ' =:Gesperrt and');  // 29.06.2007, WW
      DatenTelegrQuery.SQL.Add('  B.' + C_IecDatenDSfG_DatenRefNrBis + ' <> B.' + C_IecDatenDSfG_RefNrGesendetBis);
      DatenTelegrQuery.ParamByName('GerArt').asString:=C_GerArtDSfG;   { nur DSfG-Einträge }
      DatenTelegrQuery.ParamByName('Gesperrt').asBoolean:=false;   { nur nicht-gesperrte Einträge }
      if IECLinienNr >= 0 then
        DatenTelegrQuery.ParamByName('IECLiniennummer').asInteger:=IECLinienNr;
      DatenTelegrQuery.Open;

      while not DatenTelegrQuery.EOF do begin
        Application.ProcessMessages;
        DSfGKanaltyp:=DatenTelegrQuery.FieldByName(C_IecKonfig_Kanaltyp).AsString;
        KonfigDSfG_Arch.IEC.StationsNr:=DatenTelegrQuery.FieldByName(C_IecKonfig_IEC_ASDU).AsInteger;
        KonfigDSfG_Arch.IEC.InfoObj_high:=DatenTelegrQuery.FieldByName(C_IecKonfig_IEC_InfoObj_high).AsInteger;
        KonfigDSfG_Arch.IEC.InfoObj_medium:=DatenTelegrQuery.FieldByName(C_IecKonfig_IEC_InfoObj_medium).AsInteger;
        KonfigDSfG_Arch.IEC.InfoObj_low:=DatenTelegrQuery.FieldByName(C_IecKonfig_IEC_InfoObj_low).AsInteger;
        KonfigDSfG_Arch.StationId:=DatenTelegrQuery.FieldByName(C_IecDatenDSfG_StationID).AsInteger;
        KonfigDSfG_Arch.InstanzId:=DatenTelegrQuery.FieldByName(C_IecDatenDSfG_InstanzID).AsInteger;
        KonfigDSfG_Arch.ArchNr:=DatenTelegrQuery.FieldByName(C_IecDatenDSfG_ArchivNr).AsInteger;
        KonfigDSfG_Arch.KanalNr:=DatenTelegrQuery.FieldByName(C_IecDatenDSfG_KanalNr).AsInteger;
        BisherRefNr:=DatenTelegrQuery.FieldByName(C_IecDatenDSfG_RefNrGesendetBis).AsInteger;
        BisherDatum:=DatenTelegrQuery.FieldByName(C_IecDatenDSfG_DZGesendetBis).AsDateTime;
        BisherOrdNr:=DatenTelegrQuery.FieldByName(C_IecDatenDSfG_OrdNrGesendetBis).AsInteger;

        // dem Archivkanal zugeordnete Rückgaben belegen:
        // Kopf für Log-Eintrag des Kanals:
        LogHeader:='DSfG  StationId: ' + IntTostr (KonfigDSfG_Arch.StationId) +
                   '  InstanzId: ' + IntToStr (KonfigDSfG_Arch.InstanzId) +
                   '  ArchivNr: ' + IntToStr (KonfigDSfG_Arch.ArchNr) +
                   '  KanalNr: ' + IntToStr (KonfigDSfG_Arch.KanalNr) +
                   ' (' + DSfGKanaltyp + ')' +
                   '  ' + GetIEC_LogHeader (KonfigDSfG_Arch.IEC, InfoObj_Bytes);

        // IEC-Typkennung für die Archivkanaldaten:
        if DSfGKanaltyp = 'MW' then  { Messwertkanal (physikalischer Wert) }
          IEC_TypKennung:=GetIEC_Typkennung (iecpi_Messwert_Gleitkomma, DualeZeit2a_Bytes)
        else                         { Zählerstände, Zählwerte }
          IEC_TypKennung:=GetIEC_Typkennung (iecpi_Zaehlwert, DualeZeit2a_Bytes);

        { nur bestimmte Kanaltypen erlaubt: }
        if (DSfGKanaltyp <> 'ZS') AND (DSfGKanaltyp <> 'ZW') AND (DSfGKanaltyp <> 'MW') then begin
          DatenTelegrQuery.Next;
          Continue;
        end;
        Idx:=Get_DSfGDatenIndex (FAutoDb, KonfigDSfG_Arch.StationId, KonfigDSfG_Arch.InstanzId,
                                 KonfigDSfG_Arch.ArchNr, false);
        if Idx <= 0 then begin   { kein Index für Archivdaten vorhanden }
          DatenTelegrQuery.Next;
          Continue;
        end;
        SatzTable.TableName:=Format(C_TbDSatz+'%.4d',[Idx]);
        if not SatzTable.Exists then begin     { Satz-Tabelle nicht vorhanden }
          DatenTelegrQuery.Next;
          Continue;
        end;

        if SatzTable.OpenShared then begin
          try
            if SatzTable.RecordCount = 0 then begin
              DatenTelegrQuery.Next;
              Continue;
            end;
            if BisherRefNr = CRefNr_Init then begin   { bisher wurde für den Kanal noch nichts übertragen }
              { Satztabelle so positionieren, daß die letzten n Werte übertragen
                werden (n = DSfG_ErsteDatenAnzWerte): }
              SatzTable.Last;
              Satztable.MoveBy ((DSfG_ErsteDatenAnzWerte*(-1)) + 1);
              if SatzTable.Bof then
                SatzTable.First;
            end
            else begin
              if SatzTable.FindKey ([BisherRefNr]) then begin
                { Tabellensatz mit RefNr des letzten bisher übertragenen Satzes gefunden }
                OrdNr:=SatzTable.FieldByName (C_TfDSatz_OrdNr).AsInteger;
                DZ:=SatzTable.FieldByName (C_TfDSatz_DatumZeit).AsDateTime;

                if ((OrdNr = BisherOrdNr) AND (CmpDateTime (DZ, BisherDatum) = 0)) OR
                   (BisherOrdNr = COrdNr_Init) then begin
                  { auch Ordnungsnummer und Zeitstempel des Tabellensatzes stimmen mit
                    letztem bisher übertragenem Satz überein (wenn Ordnungsnummer des
                    letzten übertragenen Satzes nicht bekannt ist (COrdNr_Init bei
                    Generalabfrage !), nicht prüfen; 10.12.2004, WW:
                    -> letzter übertragener Satz wurde in Tabelle gefunden, ab nächstem
                       Tabellensatz übertragen }
                  SatzTable.Next;
                  if SatzTable.Eof then begin
                    DatenTelegrQuery.Next;
                    Continue;
                  end;
                end else
                  { RefNr, OrdNr, Zeitstempel stimmen nicht überein (Kanal-Archivdaten wurden
                    offensichtlich ganz gelöscht): gesamten Tabelleninhalt übertragen }
                  SatzTable.First;
              end
              else begin
                { Tabellensatz mit RefNr des letzten bisher übertragenen Satzes wurde
                  nicht gefunden }
                SatzTable.Last;
                DZ:=SatzTable.FieldByName (C_TfDSatz_DatumZeit).AsDateTime;
                if DZ < BisherDatum then begin
                  { keine neueren Werte in Tabelle vorhanden (jüngste Kanal-Archivdaten
                    wurden offensichtlich gelöscht): weiter mit nächstem Query-Eintrag }
                  DatenTelegrQuery.Next;
                  Continue;
                end else
                  { neuere Werte vorhanden (Kanal-Archivdaten wurden offensichtlich ganz
                    gelöscht): gesamten Tabelleninhalt übertragen }
                  SatzTable.First;
              end;
            end;

            { Wert/Hex-Tabelle: }
            if DSfGKanaltyp <> kt_ST then
              WertHexTable.TableName:=Format(C_TbDWert+'%.3d',[KonfigDSfG_Arch.KanalNr])+Format('%.4d',[Idx])  { Wert-Tabelle }
            else
              WertHexTable.TableName:=Format(C_TbDHex+'%.3d',[KonfigDSfG_Arch.KanalNr])+Format('%.4d',[Idx]);  { Hex- Tabelle }
            if not WertHexTable.Exists then begin        { Wert/Hex-Tabelle nicht vorhanden }
              DatenTelegrQuery.Next;
              Continue;
            end;

            if WertHexTable.OpenShared then begin
              try
                while not SatzTable.Eof do begin
                  Application.ProcessMessages;

                  RefNr:=SatzTable.FieldByName (C_TfDSatz_ReferenzNr).AsInteger;
                  DZ:=SatzTable.FieldByName (C_TfDSatz_DatumZeit).AsDateTime;
                  { zugehörigen Eintrag in Wert/Hex-Tabelle suchen: }
                  if WertHexTable.FindKey ([RefNr]) then begin
                    with MessDaten do begin
                      Wert:=WertHexTable.FieldByName (C_TfDWert_Wert).AsFloat;
                      { aus DSfG-Satz/Kanalstati kann kein sinnvoller gültig/ungültig-
                        Zustand ermittelt werden: }
                      Wert_gueltig:=true;
                      DatumZeit:=DZ;
                      Ordnungsnummer:=SatzTable.FieldByName (C_TfDSatz_OrdNr).AsInteger;
                      Referenznummer:=RefNr;
                      { Datensatz-LogInfo }
                      LogInfo:='-> DZ: ' + FormatDateTime ('dd.mm.yyyy hh:nn:ss', DZ) +
                               '  Wert: ' + WertHexTable.FieldByName (C_TfDWert_Wert).AsString;
                    end;
                    MessDatenObj:=TMessDatenObj.Create(MessDaten);
                    DatenListe.Add(MessDatenObj);  // Messwert in Datenliste anhängen

                    Result:=true;  // Ergebnis true, wenn Datenliste mind. 1 Eintrag enthält
                  end;  { if WertHexTable.FindKey }
                  SatzTable.Next;
                end;  { while not SatzTable.Eof }
              finally
                WertHexTable.Close;
              end;
            end;   { if WertHexTable.OpenShared }
          finally
            SatzTable.Close;
          end;
        end;   { if SatzTable.OpenShared }

        if Result then Break;  // nur DSfG-Archivwerte eines Kanals in Datenliste laden

        DatenTelegrQuery.Next;
      end;  { while not DatenQuery.Eof }
      DatenTelegrQuery.Close;
    finally
      WertHexTable.Free;
    end;
  finally
    SatzTable.Free;
  end;

  { Sperre setzen, wenn Daten geladen wurden: 02.07.2007, WW }
  if Result AND bSperren then
    F_Lock_DSfG_AK (KonfigDSfG_Arch.StationId, KonfigDSfG_Arch.InstanzId,
                    KonfigDSfG_Arch.ArchNr, KonfigDSfG_Arch.KanalNr);
end;

{---------------------------------------------------------------------------------------}
function TFormDatenIec.GetNeueDatenDSfG_Logb (IECLinienNr: integer; InfoObj_Bytes: byte;
  var IEC_TypKennung: byte; var KonfigDSfG_Logb: TKonfigDSfG_Logb; var LogHeader: string;
  DatenListe: TDatenListe; bSperren: boolean): boolean;
{---------------------------------------------------------------------------------------}
{ Liste mit zu übertragenden DSfG-Meldungen (Logbucheinträge) füllen und zurückgeben;
  -> Die Datenliste wird mit den Daten einer Meldung gefüllt.
  -> Alle Rückgaben enthalten die der Meldung zugeordneten Werte.
  Übergabe: IEC-Liniennummer (wenn < 0: ohne Liniennummer) }
var
  BisherDatum: TDateTime;
  WMeldungenQuery: TQueryExt;
  MeldDatumZeit: TDateTime;
  MeldTyp: string;
  MeldDaten: TMeldDaten;
  MeldDatenObj: TMeldDatenObj;

begin
  Result:=false;
  // Vorbelegungen für Rückgaben:
  IEC_TypKennung:=GetIEC_Typkennung (iecpi_Einzelmeldung, DualeZeit2a_Bytes);
  with KonfigDSfG_Logb do begin
    InstanzId:=0;
    LogbNr:=0;
    MeldNr:=0;
    with IEC do begin
      LinienNr:=IECLinienNr;  // wie in Übergabe
      StationsNr:=0;
      InfoObj_high:=0;
      InfoObj_medium:=0;
      InfoObj_low:=0;
    end;
  end;
  LogHeader:='';
  if not Assigned(DatenListe) then exit;
  DatenListe.Clear;

  { Tabellen auf Vorhandensein prüfen: }
  if (not Assigned(FStammDb)) then Exit;
  if not TableExists(FStammDb, DBIecMeldKonfig) then exit;
  if not TableExists(FStammDb, DBIecMeldDatenDSfG) then exit;
  if not TableExists(FStammDb, C_Tb_WMeldungen) then exit;

  { INI neu lesen, wenn geändert; 10.03.2015, WW }
  GetIniDaten (true);
  
  { Abfrage aller der IEC-Liniennummer zugeordneten und zu übertragenden DSfG-Logbucheinträge: }
  DatenTelegrQuery.Close;
  DatenTelegrQuery.DatabaseName:=FStammDb.DatabaseName;
  DatenTelegrQuery.SQL.Clear;
  DatenTelegrQuery.SQL.Add('SELECT');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_DSfGInstanzID + ',');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_DSfGLogbuchNr + ',');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_MNrAllg + ',');         { nur Nummer für Kommt-Meldungen }
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_IEC_ASDU + ',');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_IEC_InfoObj_high + ',');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_IEC_InfoObj_medium + ',');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_IEC_InfoObj_low + ',');
  DatenTelegrQuery.SQL.Add('  B.' + C_IecMeldDatenDSfG_MeldGesendetBis);
  DatenTelegrQuery.SQL.Add('FROM');
  DatenTelegrQuery.SQL.Add('  ' + DBIecMeldKonfig + ' A,');
  DatenTelegrQuery.SQL.Add('  ' + DBIecMeldDatenDSfG + ' B');
  DatenTelegrQuery.SQL.Add('WHERE');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_GeraeteArt + ' = :GerArt and');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_DSfGInstanzID + ' = B.' + C_IecMeldDatenDSfG_InstanzID + ' and');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_DSfGLogbuchNr + ' = B.' + C_IecMeldDatenDSfG_LogbuchNr + ' and');
  DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_MNrAllg + ' = B.' + C_IecMeldDatenDSfG_MNrAllg + ' and');
  if IECLinienNr >= 0 then
    DatenTelegrQuery.SQL.Add('  A.' + C_IecMeldKonfig_IEC_LinienNr + ' = :IECLiniennummer and');
  DatenTelegrQuery.SQL.Add('  B.' + C_IecMeldDatenDSfG_Gesperrt + ' =:Gesperrt and');  // 29.06.2007, WW
  DatenTelegrQuery.SQL.Add('  B.' + C_IecMeldDatenDSfG_MeldDatenBis + ' > B.' + C_IecMeldDatenDSfG_MeldGesendetbis + ' and');
  DatenTelegrQuery.SQL.Add('  B.' + C_IecMeldDatenDSfG_MeldDatenBis + ' > :ErsteDatenDatum');

  DatenTelegrQuery.ParamByName('GerArt').asString:=C_GerArtDSfG;   { nur DSfG-Einträge }
  DatenTelegrQuery.ParamByName('Gesperrt').asBoolean:=false;   { nur nicht-gesperrte Einträge }
  DatenTelegrQuery.ParamByName('ErsteDatenDatum').asDateTime:=ErsteMeldungenDatum;
  if IECLinienNr >= 0 then
    DatenTelegrQuery.ParamByName('IECLiniennummer').asInteger:=IECLinienNr;
  DatenTelegrQuery.Open;

  WMeldungenQuery:=TQueryExt.Create (nil);
  try
    WMeldungenQuery.DatabaseName:=FStammDb.DatabaseName;

    while not DatenTelegrQuery.EOF do begin
      Application.ProcessMessages;
      KonfigDSfG_Logb.IEC.StationsNr:=DatenTelegrQuery.FieldByName(C_IecMeldKonfig_IEC_ASDU).AsInteger;
      KonfigDSfG_Logb.IEC.InfoObj_high:=DatenTelegrQuery.FieldByName(C_IecMeldKonfig_IEC_InfoObj_high).AsInteger;
      KonfigDSfG_Logb.IEC.InfoObj_medium:=DatenTelegrQuery.FieldByName(C_IecMeldKonfig_IEC_InfoObj_medium).AsInteger;
      KonfigDSfG_Logb.IEC.InfoObj_low:=DatenTelegrQuery.FieldByName(C_IecMeldKonfig_IEC_InfoObj_low).AsInteger;
      KonfigDSfG_Logb.InstanzId:=DatenTelegrQuery.FieldByName(C_IecMeldKonfig_DSfGInstanzID).AsInteger;
      KonfigDSfG_Logb.LogbNr:=DatenTelegrQuery.FieldByName(C_IecMeldKonfig_DSfGLogbuchNr).AsInteger;
      KonfigDSfG_Logb.MeldNr:=DatenTelegrQuery.FieldByName(C_IecMeldKonfig_MNrAllg).AsInteger;
      BisherDatum:=DatenTelegrQuery.FieldByName(C_IecMeldDatenDSfG_MeldGesendetBis).AsDateTime;

      // der Meldungsnummer zugeordnete Rückgaben belegen:
      // Kopf für Log-Eintrag der Meldung:
      LogHeader:='DSfG  InstanzId: ' + IntToStr (KonfigDSfG_Logb.InstanzId) +
                 '  LogbNr: ' + IntToStr (KonfigDSfG_Logb.LogbNr) +
                 '  MNrAllg: ' + IntToStr (KonfigDSfG_Logb.MeldNr) +
                 '  ' + GetIEC_LogHeader (KonfigDSfG_Logb.IEC, InfoObj_Bytes);

      // zu übertragende DSfG-Meldungen in Datenliste laden:
      if CmpDateTime (BisherDatum, DT_Initialisieren) = 0 then
        BisherDatum:=ErsteMeldungenDatum;

      WMeldungenQuery.Close;
      WMeldungenQuery.SQL.Clear;
      WMeldungenQuery.SQL.Add('SELECT');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_DatumZeit + ',');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_MNrAllg+ ',');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_MTyp);
      WMeldungenQuery.SQL.Add('FROM');
      WMeldungenQuery.SQL.Add(C_Tb_WMeldungen);
      WMeldungenQuery.SQL.Add('WHERE');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_Benutzer + ' = :Benutzer and');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_GeraeteArt + ' = :GeraeteArt and');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_InstanzId_Archiv + ' = :InstID and');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_LogbuchNr_Archiv + ' = :LogbNr and');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_MNrAllg + ' = :MeldNr and');
      WMeldungenQuery.SQL.Add(C_Tf_WMeldungen_DatumZeit + ' > :BisherDatum');
      WMeldungenQuery.ParamByName('Benutzer').asString:=C_BenutzerAuto; { Automatik-Daten }
      WMeldungenQuery.ParamByName('GeraeteArt').asString:=C_GerArtDSfG;  { ...aus DSfG-Stationen }
      WMeldungenQuery.ParamByName('InstId').asInteger:=KonfigDSfG_Logb.InstanzId;
      WMeldungenQuery.ParamByName('LogbNr').asInteger:=KonfigDSfG_Logb.LogbNr;
      WMeldungenQuery.ParamByName('MeldNr').asString:=Format('%.5d', [KonfigDSfG_Logb.MeldNr]);
      WMeldungenQuery.ParamByName('BisherDatum').asDateTime:=BisherDatum;

      if not WMeldungenQuery.Open then begin
        DatenTelegrQuery.Next;
        Continue;
      end;
      if WMeldungenQuery.RecordCount < 1 then begin
        DatenTelegrQuery.Next;
        Continue;
      end;

      while not WMeldungenQuery.Eof do begin
        Application.ProcessMessages;
        MeldDatumZeit:=WMeldungenQuery.FieldByName(C_Tf_WMeldungen_DatumZeit).AsDateTime;
        MeldTyp:=WMeldungenQuery.FieldByName(C_Tf_WMeldungen_MTyp).AsString;
        with MeldDaten do begin
          if MeldTyp <> 'G' then  { keine Geht-Meldung }
            Ein:=true    { Ein bei Kommt-Meldung }
          else
            Ein:=false;  { Aus bei Geht-Meldung }
          DatumZeit:=MeldDatumZeit;
          { Datensatz-LogInfo }
          LogInfo:='-> DZ: ' + FormatDateTime ('dd.mm.yyyy hh:nn:ss', MeldDatumZeit) +
                   '  M-Typ: ' + MeldTyp;
        end;
        MeldDatenObj:=TMeldDatenObj.Create(MeldDaten);
        DatenListe.Add(MeldDatenObj);  // Meldung in Datenliste anhängen

        Result:=true;  // Ergebnis true, wenn Datenliste mind. 1 Eintrag enthält
        WMeldungenQuery.Next;
      end;  { while not WMeldungenQuery.Eof }

      if Result then Break;  // nur MRG-Meldungen einer Meldungsnummer in Datenliste laden

      DatenTelegrQuery.Next;
    end;  { while not DatenQuery.Eof }
    DatenTelegrQuery.Close;
  finally
    WMeldungenQuery.Close;
    WMeldungenQuery.Free;
  end;

  { Sperre setzen, wenn Daten geladen wurden: 02.07.2007, WW }
  if Result AND bSperren then
    F_Lock_DSfG_LB (KonfigDSfG_Logb.InstanzId, KonfigDSfG_Logb.LogbNr,
                    KonfigDSfG_Logb.MeldNr);
end;


{------------------------- Kurzzeitwerte --------------------------------------}

{---------------------------------------------------------------------------------------}
function TFormDatenIec.GetNeueDatenDSfG_KZW (IECLinienNr: integer; InfoObj_Bytes: byte;
  var IEC_TypKennung: byte; var KonfigDSfG_Arch: TKonfigDSfG_Arch; var LogHeader: string;
  DatenListe: TDatenListe; var KZWList_Index: integer; var fMin, fMax: double;
  iCause: byte = C_URS_spontan): boolean;
{---------------------------------------------------------------------------------------}
{ Liste mit zu übertragenden DSfG-Kurzzeitwerten füllen und zurückgeben;
  -> Die Datenliste wird mit den Daten eines Archivkanals gefüllt.
  -> Alle Rückgaben enthalten die dem Archivkanal zugeordneten Werte.
  Übergabe: IEC-Liniennummer (wenn < 0: ohne Liniennummer) }
var
  DSfGKanaltyp: string;
  MessDaten: TMessDaten;
  MessDatenObj: TMessDatenObj;
  i: integer;
  Kennung: string;
  EAdr: string;
  DEL: string;
  RufStammdaten: TRufStammdaten;
  Kennung_in_Stammdaten_eindeutig: boolean;
  InstanzData: TInstanzData;
  AG: integer;
  AK: integer;
  AKanaeleData: TAKanaeleData;
  bIecKonfiguriert: boolean;

begin
  { INI neu lesen, wenn geändert; 10.03.2015, WW }
  GetIniDaten (true);

  // Zunächst auf allgemeingültigen (XML-konfigurierten) Wert prüfen
  Result := GetNeueDaten_Custom (IECLinienNr, InfoObj_Bytes, IEC_TypKennung,
    KonfigDSfG_Arch, LogHeader, DatenListe, KZWList_Index, fMin, fMax, iCause);
  if (Result) then Exit;

  // Vorbelegungen für Rückgaben:
  IEC_TypKennung:=0;
  with KonfigDSfG_Arch do begin
    StationId:=0;
    InstanzId:=0;
    ArchNr:=0;
    KanalNr:=0;
    with IEC do begin
      LinienNr:=0;  // nur Vorbelegung, wird in IECKonfigDb.GetDSfGDataByArchivkanal gesetzt
      StationsNr:=0;
      InfoObj_high:=0;
      InfoObj_medium:=0;
      InfoObj_low:=0;
    end;
  end;
  LogHeader:='';
  KZWList_Index:=-1;
  if not Assigned(DatenListe) then exit;
  DatenListe.Clear;

  if not Assigned(DSfGStammdaten) OR not DSfGSta_OK then exit;
  for i:=(KZWListe.Count - 1) downto 0 do begin  // Liste vom Ende her lesen, da Listeneinträge evtl. gelöscht werden
    Kennung:=TKZWListObj (KZWListe [i]).KZWData.Kennung;
    EAdr:=TKZWListObj (KZWListe [i]).KZWData.EAdr;
    DEL:=TKZWListObj (KZWListe [i]).KZWData.DEL;

    if length (DEL) >= 4 then begin  // DE-Adresse gültig
      AG:=Ord (DEL [3]) - C_INT_DELGRUPPEOFFSET;  // Archivgruppen-Nummer
      AK:=Ord (DEL [4]) - C_INT_DELKANALOFFSET;   // Archivkanal-Nummer

      { DSfG-Archivkanal-Stammsatz für Kurzzeitwert suchen: }
      { Stammsatz für Kennung: }
      if DSfGStammdaten.GetRufStammdatenByKennung (Kennung, RufStammdaten,
                                                   Kennung_in_Stammdaten_eindeutig) then begin
        if Kennung_in_Stammdaten_eindeutig then begin
          if DSfGStammdaten.GetInstanzData_Adresse (RufStammDaten.StationId,
                                                    EAdr, InstanzData) then begin
            if DSfGStammdaten.GetAKanaeleData (InstanzData.InstanzId, AG, AK,
                                               AKanaeleData) then begin
              { DSfG-Stammsatz für Archivkanal des Kurzzeitwert gefunden.
                Jetzt nach IEC-Konfiguration des Archivkanals suchen: }
              bIecKonfiguriert:=false;  // Vorbelegung: Archivkanal ist nicht oder falsch IEC-konfiguriert
              if IECKonfigDb.GetDSfGDataByArchivkanal (RufStammDaten.StationId,
                AKanaeleData.InstanzId, AKanaeleData.ArchivNr, AKanaeleData.KanalNr,
                DSfGKanaltyp, KonfigDSfG_Arch.IEC) then begin

                { nur bestimmte Kanaltypen erlaubt: }
                if (DSfGKanaltyp = 'ZS') OR (DSfGKanaltyp = 'ZW') OR (DSfGKanaltyp = 'MW') then begin
                  bIecKonfiguriert:=true;  // Archivkanal ist (korrekt) IEC-konfiguriert
                  with KonfigDSfG_Arch do begin
                    StationId:=RufStammDaten.StationId;
                    InstanzId:=AKanaeleData.InstanzId;
                    ArchNr:=AKanaeleData.ArchivNr;
                    KanalNr:=AKanaeleData.KanalNr;
                  end;

                  { prüfen, ob Archivkanal zu übergebener Liniennummer konfiguriert ist: }
                  if (IECLinienNr < 0) OR (KonfigDSfG_Arch.IEC.LinienNr = IECLinienNr) then begin
                    // dem Archivkanal zugeordnete Rückgaben belegen:
                    // Kopf für Log-Eintrag des Kanals:
                    LogHeader:='KZW  StationId: ' + IntTostr (KonfigDSfG_Arch.StationId) +
                               '  InstanzId: ' + IntToStr (KonfigDSfG_Arch.InstanzId) +
                               '  ArchivNr: ' + IntToStr (KonfigDSfG_Arch.ArchNr) +
                               '  KanalNr: ' + IntToStr (KonfigDSfG_Arch.KanalNr) +
                               ' (' + DSfGKanaltyp + ')' +
                               '  ' + GetIEC_LogHeader (KonfigDSfG_Arch.IEC, InfoObj_Bytes);

                    // IEC-Typkennung für die Archivkanaldaten:
                    if DSfGKanaltyp = 'MW' then  { Messwertkanal (physikalischer Wert) }
                      IEC_TypKennung:=GetIEC_Typkennung (iecpi_Messwert_Gleitkomma, DualeZeit2a_Bytes)
                    else                         { Zählerstände, Zählwerte }
                      IEC_TypKennung:=GetIEC_Typkennung (iecpi_Zaehlwert, DualeZeit2a_Bytes);

                    with MessDaten do begin
                      Wert:=TKZWListObj (KZWListe [i]).KZWData.Wert;
                      Wert_gueltig:=true;  { bei Kurzzweitwerten kein gültig/ungültig-Zustand verfügbar }
                      DatumZeit:=TKZWListObj (KZWListe [i]).KZWData.DatumZeit;
                      Ordnungsnummer:=-1;  { bei Kurzzweitwerten keine Ordnungsnummer verfügbar }
                      Referenznummer:=-1;  { bei Kurzzweitwerten keine Referenznummer verfügbar }
                      { Datensatz-LogInfo }
                      LogInfo:='-> DZ: ' + FormatDateTime ('dd.mm.yyyy hh:nn:ss', DatumZeit) +
                               '  Wert: ' + FloatToStr (Wert);
                    end;
                    MessDatenObj:=TMessDatenObj.Create(MessDaten);
                    DatenListe.Add(MessDatenObj);  // Messwert in Datenliste anhängen

                    KZWList_Index:=i;
                    Result:=true;  // Ergebnis true, wenn Datenliste mind. 1 Eintrag enthält
                    Break;  // nur ein Kurzzeitwert
                  end;  { if KonfigDSfG_Arch.IEC.LinienNr = IECLinienNr }
                end;  { if (DSfGKanaltyp = 'ZS') OR .. }
              end;  { if IECKonfigDb.GetDSfGDataByArchivkanal }

              // bei nicht vorhandener oder falscher Iec-Konfiguration: Kurzzeitwert aus
              // Kurzzeitwert-Liste und Kurzzeitwert-Dateien löschen:
              if not bIecKonfiguriert then begin
                if not KZWListe.DeleteKZWFile (i) then;
              end;
            end;  { if DSfGStammdaten.GetAKanaeleData }
          end;  { if DSfGStammdaten.GetInstanzData_Adresse }
        end;  { if Kennung_in_Stammdaten_eindeutig }
      end;  { if DSfGStammdaten.GetRufStammdatenByKennung }
    end;  { if length (DEL) >= 4 }
  end;  { for i }
end;

{---------------------------------------------------------------------------------------}
function TFormDatenIec.GetNeueDaten_Custom (IECLinienNr: integer; InfoObj_Bytes: byte;
  var IEC_TypKennung: byte; var KonfigDSfG_Arch: TKonfigDSfG_Arch; var LogHeader: string;
  DatenListe: TDatenListe; var KZWList_Index: integer; var fMin, fMax: double;
  iCause: byte = C_URS_spontan): boolean;
{---------------------------------------------------------------------------------------}
{ Liste mit zu übertragenden Daten aus Datei füllen und zurückgeben;
  -> Die Datenliste wird mit den Daten eines Datenpunktes gefüllt.
  -> Alle Rückgaben enthalten die dem Datenpunkt zugeordneten Werte.
  Übergabe: IEC-Liniennummer (wenn < 0: ohne Liniennummer) }
var
  DSfGKanaltyp: string;
  MessDaten: TMessDaten;
  MessDatenObj: TMessDatenObj;
  MeldDaten: TMeldDaten;
  MeldDatenObj: TMeldDatenObj;
  i: integer;
  iTyp : TConfigInputType;
  Kennung: string;
  EAdr: string;
  DEL: string;
  pRec : TIecConfigRec;
  pVal : TIecValueObject;
  KZWListObj : TKZWListObj;
  DKZWData : TDKZWData;
  iTransferCause : integer;
begin
  Result:=false;
  // Vorbelegungen für Rückgaben:
  IEC_TypKennung:=0;
  with KonfigDSfG_Arch do begin
    StationId:=0;
    InstanzId:=0;
    ArchNr:=0;
    KanalNr:=0;
    with IEC do begin
      LinienNr:=0;  // nur Vorbelegung, wird in IECKonfigDb.GetDSfGDataByArchivkanal gesetzt
      StationsNr:=0;
      InfoObj_high:=0;
      InfoObj_medium:=0;
      InfoObj_low:=0;
    end;
  end;
  LogHeader:='';
  KZWList_Index:=-1;
  if not Assigned(DatenListe) then Exit;
  DatenListe.Clear;

  // Bei Generalabfrage Liste aus bisherigen Werten bilden
  if (iCause = C_URS_GeneralAbfr) then begin
    for i := FIecValueList.Count-1 downto 0 do begin
      pVal := TIecValueObject(FIecValueList[i]);
      // Wert für Generalabfrage vorgesehen?
      if (pVal.HasGeneralValue) then begin
        DKZWData := ConfigInputToDKZWData(pVal.ConfigRec.Input);
        DKZWData.DatumZeit := pVal.TimeStamp;
        DKZWData.Wert := pVal.Value;
        pVal.SendedFist := False;  // Damit wirklich gesendet wird

        KZWListObj := TKZWListObj.Create;
        KZWListObj.SetData(DKZWData, '', 0, C_URS_GeneralAbfr);
        KZWListe.Insert(0, KZWListObj);
      end;
    end;
  end;

  for i:=(KZWListe.Count - 1) downto 0 do begin  // Liste vom Ende her lesen, da Listeneinträge evtl. gelöscht werden
    Kennung:=TKZWListObj (KZWListe [i]).KZWData.Kennung;
    EAdr:=TKZWListObj (KZWListe [i]).KZWData.EAdr;
    DEL:=TKZWListObj (KZWListe [i]).KZWData.DEL;
    iTyp := TConfigInputType(TKZWListObj (KZWListe [i]).KZWData.Datentyp);
    iTransferCause := TKZWListObj (KZWListe [i]).TransferCause;
    if (iTyp <> citUnknown) and
      (Trim(Kennung) <> '') and (Length (DEL) >= 1) and
      (FIecConfigXml.HasCustomListNode(iTyp, Kennung, EAdr, '', DEL, pRec))
    then begin
      DSfGKanaltyp := Trim(pRec.Input.DataType);
      { nur bestimmte Kanaltypen erlaubt: }
      if (DSfGKanaltyp = 'ZS') OR (DSfGKanaltyp = 'ZW') OR (DSfGKanaltyp = 'MW')
        OR (DSfGKanaltyp = 'MN') OR (DSfGKanaltyp = 'ST') then
      begin
        // Wert in interne Liste aufnehmen
        pVal := FIecValueList.GetValueObject(pRec);
        pVal.Value := TKZWListObj(KZWListe [i]).KZWData.Wert;
        pVal.TimeStamp := TKZWListObj(KZWListe [i]).KZWData.DatumZeit;
        if (not pVal.HasSendValue) then begin
          KZWListe.DeleteKZWFile(i);
          Continue;
        end;

        with KonfigDSfG_Arch do begin
          with IEC do begin
            LinienNr := pRec.LineNumber;
            StationsNr := pRec.IEC_ASDU;
            InfoObj_high := pRec.IEC_Info_High;
            InfoObj_medium := pRec.IEC_Info_Medium;
            InfoObj_low := pRec.IEC_Info_Low;
          end;
        end;

        // dem Archivkanal zugeordnete Rückgaben belegen:
        // Kopf für Log-Eintrag des Kanals:
        LogHeader:= 'KZW; Kanaltyp: ' + DSfGKanaltyp +
                   '  ' + GetIEC_LogHeader (KonfigDSfG_Arch.IEC, InfoObj_Bytes);

        // IEC-Typkennung für die Archivkanaldaten:
        if DSfGKanaltyp = 'MW' then  { Messwertkanal (physikalischer Wert) }
          IEC_TypKennung:=GetIEC_Typkennung (iecpi_Messwert_Gleitkomma, DualeZeit2a_Bytes)
        else if DSfGKanaltyp = 'MN' then  { Messwertkanal (normalisierter Wert) }
          IEC_TypKennung:=GetIEC_Typkennung (iecpi_Messwert_normiert, DualeZeit2a_Bytes)
        else if DSfGKanaltyp = 'ST' then  { Zustand (Meldung 0 / 1) }
          IEC_TypKennung := GetIEC_Typkennung (iecpi_Einzelmeldung, DualeZeit2a_Bytes)
        else                         { Zählerstände, Zählwerte }
          IEC_TypKennung:=GetIEC_Typkennung (iecpi_Zaehlwert, DualeZeit2a_Bytes);

        if (IEC_TypKennung in [C_TK_Einzelmeldung,
          C_TK_Einzelmeldung_CP24Time2a, C_TK_Einzelmeldung_CP56Time2a]) then
        begin
          with MeldDaten do begin
            Ein := (pVal.Value > 0.5);
            DatumZeit := pVal.TimeStamp;
            { Datensatz-LogInfo }
            LogInfo := '-> DZ: ' + FormatDateTime ('dd.mm.yyyy hh:nn:ss', DatumZeit) +
                     '  Meldung: ' + BoolToStr(Ein, True);
          end;
          MeldDatenObj := TMeldDatenObj.Create(MeldDaten);
          MeldDatenObj.TransferCause := iTransferCause mod C_TK_Generalabfrage;
          DatenListe.Add(MeldDatenObj);  // Meldung in Datenliste anhängen
        end
        else begin
          with MessDaten do begin
            Wert := pVal.Value;

            Wert_gueltig:=true;  { bei Kurzzweitwerten kein gültig/ungültig-Zustand verfügbar }
            DatumZeit := pVal.TimeStamp;
            fMin := pVal.ConfigRec.IEC_Norm_Min;
            fMax := pVal.ConfigRec.IEC_Norm_Max;
            Ordnungsnummer:=-1;  { bei Kurzzweitwerten keine Ordnungsnummer verfügbar }
            Referenznummer:=-1;  { bei Kurzzweitwerten keine Referenznummer verfügbar }
            { Datensatz-LogInfo }
            LogInfo:='-> DZ: ' + FormatDateTime ('dd.mm.yyyy hh:nn:ss', DatumZeit) +
                     '  Wert: ' + FloatToStr (Wert);
          end;
          MessDatenObj:=TMessDatenObj.Create(MessDaten);
          if (iTransferCause = C_URS_GeneralAbfr) then
            MessDatenObj.TransferCause := iTransferCause;
          DatenListe.Add(MessDatenObj);  // Messwert in Datenliste anhängen
        end;

        KZWList_Index:=i;
        Result := True;  // Ergebnis true, wenn Datenliste mind. 1 Eintrag enthält
        Break;  // nur ein Kurzzeitwert
      end;  { if (DSfGKanaltyp = 'ZS') OR .. }
    end  { if length (DEL) >= 1 }
    else if (iTransferCause > C_TK_Generalabfrage) then begin
      with KonfigDSfG_Arch do begin
        with IEC do begin
          LinienNr := IECLinienNr;
          StationsNr := IECLinienNr;
          InfoObj_high := 0;
          InfoObj_medium := 0;
          InfoObj_low := 0;
        end;
      end;

      // dem Archivkanal zugeordnete Rückgaben belegen:
      // Kopf für Log-Eintrag des Kanals:
      LogHeader:= 'KZW; Kanaltyp: ' + DSfGKanaltyp +
                 '  ' + GetIEC_LogHeader (KonfigDSfG_Arch.IEC, InfoObj_Bytes);

      with MessDaten do begin
        Wert := 0;

        Wert_gueltig := true;  { bei Kurzzweitwerten kein gültig/ungültig-Zustand verfügbar }
        DatumZeit := Now;
        fMin := 0;
        fMax := 0;
        Ordnungsnummer := -1;  { bei Kurzzweitwerten keine Ordnungsnummer verfügbar }
        Referenznummer := -1;  { bei Kurzzweitwerten keine Referenznummer verfügbar }
        { Datensatz-LogInfo }
        LogInfo:='-> Generalabfrage';
      end;
      MessDatenObj:=TMessDatenObj.Create(MessDaten);
      MessDatenObj.TransferCause := iTransferCause mod C_TK_Generalabfrage;
      DatenListe.Add(MessDatenObj);  // Messwert in Datenliste anhängen
      IEC_TypKennung := C_TK_Generalabfrage;

      KZWList_Index:=i;
      Result := True;  // Ergebnis true, wenn Datenliste mind. 1 Eintrag enthält
      Break;  // nur ein Kurzzeitwert
    end;
  end;  { for i }
end;


{------------------ Redundanzbetrieb: Meldung für Aktiv/Passiv-----------------}

{------------------------------------------------------------------------------------}
function TFormDatenIec.GetMeldung_Redundanz_AktivPassiv (bRedundanzAktiv: boolean;
  IECLinienNr: integer; IECASDU: integer; IECInfoObj: integer;
  InfoObj_Bytes: byte; var IEC_TypKennung: byte; var KonfigIEC: TKonfigIEC;
  var LogHeader: string; DatenListe: TDatenListe): boolean;
{------------------------------------------------------------------------------------}
{ Liste mit zu übertragender Meldung für Redundanz-Betriebszustand (aktiv/passiv)
  füllen und zurückgeben;
  -> Alle Rückgaben enthalten die der Meldung zugeordneten Werte.
  Übergabe: IEC-Liniennummer (wenn < 0: ohne Liniennummer) }
var
  sAktivPassiv: string;
  MeldDaten: TMeldDaten;
  MeldDatenObj: TMeldDatenObj;
  B1, B2, B3: byte;

begin
  Result:=false;
  // der Redundanz-Betriebszustands-Meldung zugeordnete Rückgaben belegen:
  // Kopf für Log-Eintrag der Meldung:
  IEC_TypKennung:=GetIEC_Typkennung (iecpi_Einzelmeldung, DualeZeit2a_Bytes);
  with KonfigIEC do begin
    LinienNr:=IECLinienNr;  // wie in Übergabe
    StationsNr:=IECASDU;  // wie in Übergabe
    { Infoobjekt-Adresse in Byte-Darstellung wandeln: }
    IEC870_InfoObj_IntToBytes (IECInfoObj, B1, B2, B3);
    if InfoObj_Bytes > 2 then begin  { 3 Byte InfoObj-Adresse }
      InfoObj_high:=B3;
      InfoObj_medium:=B2;
    end
    else if InfoObj_Bytes > 1 then begin  { 2 Byte InfoObj-Adresse }
      InfoObj_high:=B2;
      InfoObj_medium:=0;  // unbenutzt
    end
    else begin  { 1 Byte InfoObj-Adresse }
      InfoObj_high:=0;    // unbenutzt
      InfoObj_medium:=0;  // unbenutzt
    end;
    InfoObj_low:=B1;
  end;
  LogHeader:='Redundanz  ' + GetIEC_LogHeader (KonfigIEC, InfoObj_Bytes);

  if not Assigned(DatenListe) then exit;
  DatenListe.Clear;

  with MeldDaten do begin
    if bRedundanzAktiv then begin
      Ein:=false;   { Aus bei aktivem Redundanz-Betriebszustand }
      sAktivPassiv:='Aktiv';
    end
    else begin
      Ein:=true;  { Ein bei passivem Redundanz-Betriebszustand }
      sAktivPassiv:='Passiv';
    end;
    DatumZeit:=Now;
    { Datensatz-LogInfo }
    LogInfo:='-> DZ: ' + FormatDateTime ('dd.mm.yyyy hh:nn:ss', DatumZeit) +
             '  Betriebszustand: ' + sAktivPassiv;
  end;
  MeldDatenObj:=TMeldDatenObj.Create(MeldDaten);
  DatenListe.Add(MeldDatenObj);  // Meldung in Datenliste anhängen

  Result:=true;  // Ergebnis true, wenn Datenliste mind. 1 Eintrag enthält
end;


{------------------------------------------------------------------------------}

{------------------------------------------------------------------------}
function TFormDatenIec.GetIEC_Typkennung (IECProzessInfo: TIECProzessInfo;
                                          Time2aBytes: byte): byte;
{------------------------------------------------------------------------}
{ liefert Typkennung zu IEC-Prozessinfo und Zeitmarke 'Duale Zeit 2a' (0 Bytes
  (ohne Zeitmarke), 3 Bytes (CP24Time2a) oder 7 Bytes (CP56Time2a)) }
begin
  Result:=0;  // Vorbelegung: ungültige Typkennung
  case IECProzessInfo of
    iecpi_Einzelmeldung:
      begin
        if Time2aBytes = 0 then
          Result:=C_TK_Einzelmeldung
        else if Time2aBytes = 3 then
          Result:=C_TK_Einzelmeldung_CP24Time2a
        else if Time2aBytes = 7 then
          Result:=C_TK_Einzelmeldung_CP56Time2a;
      end;

    iecpi_Messwert_normiert:
      begin
        if Time2aBytes = 0 then
          Result:=C_TK_Messwert_normiert
        else if Time2aBytes = 3 then
          Result:=C_TK_Messwert_normiert_CP24Time2a
        else if Time2aBytes = 7 then
          Result:=C_TK_Messwert_normiert_CP56Time2a;
      end;

    iecpi_Messwert_Gleitkomma:
      begin
        if Time2aBytes = 0 then
          Result:=C_TK_Messwert_Gleitkomma
        else if Time2aBytes = 3 then
          Result:=C_TK_Messwert_Gleitkomma_CP24Time2a
        else if Time2aBytes = 7 then
          Result:=C_TK_Messwert_Gleitkomma_CP56Time2a;
      end;

    iecpi_Zaehlwert:
      begin
        if Time2aBytes = 0 then
          Result:=C_TK_Zaehlwert
        else if Time2aBytes = 3 then
          Result:=C_TK_Zaehlwert_CP24Time2a
        else if Time2aBytes = 7 then
          Result:=C_TK_Zaehlwert_CP56Time2a;
      end;
  end;
end;

{--------------------------------------------------------------------------------------------}
function TFormDatenIec.GetIEC_LogHeader (AKonfigIEC: TKonfigIEC; InfoObj_Bytes: byte): string;
{--------------------------------------------------------------------------------------------}
{ Ergebnis: Log-Header mit IEC-Informationen (Liniennummer, ASDU, InfoObj-Adresse) }
begin
  Result:='Linie: ' + IntToStr (AKonfigIEC.LinienNr) +
          '  ASDU: '  + IntToStr (AKonfigIEC.StationsNr);
  if InfoObj_Bytes > 2 then  { 3 Byte InfoObj-Adresse }
    Result:=Result + '  InfoObj high: ' + IntToStr (AKonfigIEC.InfoObj_high) +
                     '  InfoObj medium: ' + IntToStr (AKonfigIEC.InfoObj_medium) +
                     '  InfoObj low: ' + IntToStr (AKonfigIEC.InfoObj_low)
  else if InfoObj_Bytes > 1 then  { 2 Byte InfoObj-Adresse }
    Result:=Result + '  InfoObj high: ' + IntToStr (AKonfigIEC.InfoObj_high) +
                           '  InfoObj low: ' + IntToStr (AKonfigIEC.InfoObj_low)
  else  { 1 Byte InfoObj-Adresse }
    Result:=Result + '  InfoObj low: ' + IntToStr (AKonfigIEC.InfoObj_low);
end;


{------------------------------------------------------------------------------}

{--------------------------------------------------------------------------}
procedure TFormDatenIec.FillGA_StationsNrListe_Alle (aLiniennummer: integer;
  aList: TStrings);
{--------------------------------------------------------------------------}
{ Füllt übergebene Liste mit allen auf Generalabfrage konfigurierten IEC-ASDUs }
begin
  if Assigned (IECKonfigDb) then
    IECKonfigDb.GetIEC_ASDU_List_General (aLiniennummer, aList)
  else
    FIecConfigXml.GetIEC_ASDU_List_General (aLiniennummer, aList);
end;

end.
