{******************************************************************************}
{* Unit: Thread für Konvertierung empfangener GPRS-Datentelegramme            *}
{* 13.03.2006  WW                                                             *}
{******************************************************************************}
unit GPRSKonvThr;

interface

uses
  Windows, Messages, Classes, StdCtrls, SysUtils, DbTables, PathIni, WChars, WErrMsg,
  WStrUtils, WSysCon, ErrConst, DListen, DGPRSKonv, GPRSVerbList, GPRS_Util,
  DGPRSKonvManager, GPRSTelegrList, WSysDat, JournlDb, AusgabeDirList, DGPRS_KZWKonv;

const
  wm_AktuIPVerbindungen = WM_User +  9;
  wm_SendWieserMsg_NKD  = WM_User + 10;

type

  { GPRS-Datenkonvertierung-Thread }

  TGPRSKonvThread = class(TThread)
  private
    GPRSTelegrammListe: TGPRSTelegrList;
    ZwFileLoeschen: boolean;
    KonvErrorLog: boolean;
    KonvDataLog: boolean;
    GPRSVerbindungenListe: TGPRSVerbList;
    CallingFormHandle: HWND;
    WieserIniFilename: string;
    FAusgabeDirList: TAusgabeDirList;
    AusgabeKurzzeitwerte: boolean;
    LastJournalReorganizeDate: TDateTime;
    procedure AbrufJournal_Reorganisieren;
  protected
    procedure Execute; override;
  public
    constructor CreateIt (AGPRSTelegrammListe: TGPRSTelegrList;
                          AZwFileLoeschen: boolean;
                          AKonvErrorLog: boolean; AKonvDataLog: boolean;
                          AGPRSVerbindungenListe: TGPRSVerbList;
                          ACallingFormHandle: HWND; AWieserIniFilename: string;
                          AAusgabeDirList: TAusgabeDirList;
                          AAusgabeKurzzeitwerte: boolean);
  end;

implementation


{ TGPRSKonvThread }

{----------------------------------------------------------------------------------}
constructor TGPRSKonvThread.CreateIt (AGPRSTelegrammListe: TGPRSTelegrList;
                                      AZwFileLoeschen: boolean;
                                      AKonvErrorLog: boolean; AKonvDataLog: boolean;
                                      AGPRSVerbindungenListe: TGPRSVerbList;
                                      ACallingFormHandle: HWND;
                                      AWieserIniFilename: string;
                                      AAusgabeDirList: TAusgabeDirList;
                                      AAusgabeKurzzeitwerte: boolean);
{----------------------------------------------------------------------------------}
{ Konstruktor für GPRS-Datenkonvertierung-Thread;
  Übergaben: Zeiger auf Liste mit GPRS-Telegrammen
             Schalter 'AZwFileLoeschen' (true: bei der Konvertierung entstehende temporäre
                                         Zwischendateien werden gelöscht)
             Schalter 'AKonvErrorLog' (true: Konvertierungsfehler-Logdatei wird geschrieben)
             Schalter 'AKonvDataLog' (true: Konvertierungsdaten-Logdatei wird geschrieben)
             Zeiger auf GPRS-Verbindungenliste
             Handle des aufrufenden Fensters
             vollständiger Dateiname der WIESER.INI
             Ausgabe-VerzeichnisListe für Bereitstellung der Kurzzeitwerte
             Flag 'AAusgabeKurzzeitwerte' (true: Kurzzeitwert-Dateien werden erzeugt) }
begin
  inherited Create(true); // Thread createn und gleich wieder anhalten
  FreeOnTerminate:=true;  // Thread soll sich beim Beenden selbst freigeben
  Priority:=tpNormal;     // Thread soll mit normaler Priorität ausgeführt werden

  GPRSTelegrammListe:=AGPRSTelegrammListe;
  ZwFileLoeschen:=AZwFileLoeschen;
  KonvErrorLog:=AKonvErrorLog;
  KonvDataLog:=AKonvDataLog;
  GPRSVerbindungenListe:=AGPRSVerbindungenListe;
  CallingFormHandle:=ACallingFormHandle;
  WieserIniFilename:=AWieserIniFilename;
  FAusgabeDirList:=AAusgabeDirList;
  AusgabeKurzzeitwerte:=AAusgabeKurzzeitwerte;

  LastJournalReorganizeDate:=0;   { Zeitpunkt der letzten Journal-Reorganisation initialisieren }
  Suspended:=false;         // Thread jetzt fortsetzen
end;

{--------------------------------}
procedure TGPRSKonvThread.Execute;
{--------------------------------}
{ Thread-Ausführung }
var
  GPRSTelegrData: TGPRSTelegrData;
  Kennung: string;
  Datentyp: integer;
  KonvErg: integer;
  SMSKonvDSfGRec: TSMSKonvDSfG;
  Fehlergruppe: integer;
  Fehlercode: integer;
  GPRSKonvManagerDSfG: TGPRSKonvManagerDSfG;
  ArLbDatenListe: TDSfGDataList;
  S: string;
  sKonvLog: string;
  OK: boolean;
  dummy: integer;
  sLogPath: string;

begin
  sLogPath:=ExtractFilePath (ParamStr(0));  // 21.04.2015, WW
  try
    // PathServer initialisieren:
    PathServer:=TPathServer.Create(WieserIniFilename,
                                   [WNetProgDir,
                                    WStammDir,
                                    WWorkDir,
                                    WNetWorkDir,
                                    AsciiDir,
                                    WStammDb,
                                    AutoDb,
                                    ManuDb
                                   ], false, false); // keine eigene Session, Zugriff nicht prüfen; 01.03.2007, WW
    try
      PathServer.Check;

      while true do begin
        // MRG-Telegramm aus Telegrammliste holen und konvertieren, solange vorhanden: }
        if GPRSTelegrammListe.GetTelegramm (GPRSTelegrData) then begin
          { Fehlergruppe und Fehlercode für Journal mit "OK" vorbelegen: }
          Fehlergruppe:=0;
          Fehlercode:=0;
          ArLbDatenListe:=TDSfGDataList.Create;  { Archiv/Logbuch-Datenliste }
          try
            Kennung:='';  { Vorbelegung: Kennung unbekannt }
            Datentyp:=0;  { Vorbelegung: Datentyp unbekannt }

            // CRC-Prüfung für MRG-Telegramm durchführen:
            if FCheckPushTelegramm_Pruefsumme (GPRSTelegrData.Telegramm, mrgtyp_MRG910,
                                               dummy, dummy) then begin
              { DSfG-SMS-Konvertierungsrecord belegen:
                -> den MRG-Telegramminhalt zwischen STX und ETX übergeben }
              SMSKonvDSfGRec.SMS_Data:=ExtractString (GPRSTelegrData.Telegramm, STX, ETX, 0);
              SMSKonvDSfGRec.GeraeteTyp:=mrgtyp_MRG910; // bislang nur MRG 905/910-Daten konvertierbar
              SMSKonvDSfGRec.ZielPfad:=PathServer [WWorkDir];
              SMSKonvDSfGRec.ArLbDatenListe:=ArLbDatenListe;  { Archiv/Logbuch-Datenliste }

              { Telegramm in Zwischendatei mit TDSfGSatzData-Struktur konvertieren,
                Kennung wird zurückgegeben: }
              OK:=KonvSMS_DSfG (SMSKonvDSfGRec, Kennung, Datentyp, KonvErg, sKonvLog, true);
              // Konvertierungsdaten-Logdatei schreiben
              if KonvDataLog then
                WriteGPRSKonvDataLog (sKonvLog, GPRSTelegrData.Telegramm, sLogPath);
              if OK then begin
                // Kennung und Gerätetyp in GPRS-Verbindungsliste updaten, wenn noch nicht erfolgt:
                if Assigned (GPRSVerbindungenListe) then begin
                  if GPRSVerbindungenListe.UpdateMRG (GPRSTelegrData.ClientAdresse,
                                                      Kennung, GetGerTypName (SMSKonvDSfGRec.GeraeteTyp)) then
                    { Botschaft an das aufrufende Fenster schicken: IP-Verbindungsanzeige aktualisieren }
                    PostMessage(CallingFormHandle, wm_AktuIPVerbindungen, 0, 0);
                end;
              end
              else begin  { Telegramm enthält keine gültigen Daten }
                Fehlergruppe:=ST_DATACHECK;
                if Datentyp = C_IsArchive then
                  Fehlercode:=DCH_ARINVALID
                else if Datentyp = C_IsLogbuecher then
                  Fehlercode:=DCH_LBINVALID
                else if Datentyp = C_IsKurzzeitwerte then
                  Fehlercode:=DCH_KZWINVALID
                else
                  Fehlercode:=DCH_INVALID;

                // in Konvertierungfehler-Logdatei eintragen
                if KonvErrorLog then begin
                  S:='Kennung: ' + Kennung + '  Gerätetyp: ' + IntToStr(SMSKonvDSfGRec.GeraeteTyp) +
                     '  KonvErg: ' + IntToStr (KonvErg);
                  WriteGPRSKonvErrorLog (Fehlergruppe, Fehlercode, S, GPRSTelegrData.Telegramm, sLogPath);
                end;
              end;
            end
            else begin  // CRC-Fehler
              Fehlergruppe:=COM_KOMMERROR;
              Fehlercode:=KOMMERR_CRC;

              // in Konvertierungfehler-Logdatei eintragen
              if KonvErrorLog then
                WriteGPRSKonvErrorLog (Fehlergruppe, Fehlercode, '', GPRSTelegrData.Telegramm, sLogPath);
            end;

            { GPRS-Telegrammdaten (File mit TDSfGData-Struktur) in Zieldaten (Archiv-/Logbuch-DB,
              Ascii-File) konvertieren: }
            GPRSKonvManagerDSfG:=TGPRSKonvManagerDSfG.Create (PathServer.Database [WStammDb],
              PathServer.Database [AutoDb], PathServer.Database [ManuDb], FAusgabeDirList,
              ZwFileLoeschen, sLogPath);
            try
              GPRSKonvManagerDSfG.Konvert (SMSKonvDSfGRec.ArLbDatenListe, Kennung,
                Datentyp, Fehlergruppe, Fehlercode, GPRSTelegrData.Telegramm,
                GPRSTelegrData.TelegrNr, AusgabeKurzzeitwerte);
            finally
              GPRSKonvManagerDSfG.Free;
            end;

            if Datentyp = C_IsKurzzeitwerte then begin
              { Botschaft an das aufrufende Fenster schicken: Wieser-Benachrichtigung
                für neue Kurzzeitwerte verschicken }
              PostMessage(CallingFormHandle, wm_SendWieserMsg_NKD, 0, 0);
            end;
          finally
            ArLbDatenListe.Free;
          end;

          // Journal reorganisieren:
          AbrufJournal_Reorganisieren;
        end
        else begin
          if Terminated then begin
            Break;  // Thread von außen erst beenden, wenn keine Telegramme mehr vorliegen
          end else
            Sleep (1);  { Prozessorauslastung niedrig halten }
         end;
      end;  { while true }
    finally
      PathServer.Free;
    end;
  except
    on E: Exception do begin
      S:='!!! ' + ExceptionErrorMsgStr (E) + ' !!!';
      WriteGPRSKonvErrorLog (-1, -1, S, GPRSTelegrData.Telegramm, sLogPath);
    end;
  end;
end;

{----------------------------------------------------}
procedure TGPRSKonvThread.AbrufJournal_Reorganisieren;
{----------------------------------------------------}
{ Funktion reorganisert nur 1 mal am Tag }
var
  SystemEinstellungen: TSystemEinstellungen;
  JournalDB: TJournalDB;

begin
  if Date > LastJournalReorganizeDate then begin
    LastJournalReorganizeDate:=Date;

    SystemEinstellungen:=TSystemEinstellungen.Create (PathServer [WNetProgDir]);
    try
      JournalDB:=TJournalDB.Create (PathServer.Database [WStammDb]);
      try
        JournalDB.Reorganize (SystemEinstellungen.GroesseJournalPuffer);
      finally
        JournalDB.Free;
      end;
    finally
      SystemEinstellungen.Free;
    end;
  end;
end;

end.
