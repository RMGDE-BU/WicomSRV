{******************************************************************************}
{* Unit: Routinen für Ausführung der GPRS-Kommandos                           *}
{* 16.03.2009  WW                                                             *}
{******************************************************************************}
unit GPRSCmdExec;

interface

uses
  Windows, Classes, SysUtils, ScktComp, DateUtils, T_Tools, T_Zeit, WStrUtils,
  WChars, PathIni, AbrufCmd, AbrufAnsw, ErrConst, LogFile, Lizenz32, RespConst,
  AbrufSrvIniFile, GPRSVerbList, GPRSTelegrList, WSysCon, DListen, GPRS_Util,
  DGPRSKonv, WicomSrvUtil, WComm, DGPRS_KZWKonv, AusgabeDirList, MRG_ObjKonv,
  AbrufConst, MResMrg, LGZType, MrgBefehl, MValidAnswer, MDTFormt, Crc16;

type
  { Objekt zur GPRS-Kommando-Ausführung }
  TGPRSCmdExec = class(TObject)
  private
    GPRSServerSocket: TServerSocket;
    GPRSVerbindungenListe: TGPRSVerbList;
    GPRSTelegrammListe: TGPRSTelegrList;
    AbrufLogFile: TCustomLogFile;
    DebugServiceIO: boolean;
    RohdatenLoeschen: boolean;
    FAusgabeDirList: TAusgabeDirList;
    Kommando_Rufentgegennahme: string; { zuletzt gesendetes Rufentgegennahme-Kommando }
    CallingFormHandle: HWND;
    FWLizenz32: TWLizenz32;  // 07.04.2014, WW
    { aus Programm-Ini-File: }
    bKonvDataLog: boolean;
    bAusgabeKurzzeitwerte: boolean;
    ZeitSyncAbweichungMin_Cfg: integer;
    ZeitSyncAbweichungMax_Cfg: integer;
    bZeitSyncGPRSAktiv_Cfg: boolean;
    dtZeitSyncStart: TDateTime;
    bISO646_Cfg: boolean;
    { für GPRS-Empfang: }
    SendeGPRSFiles: boolean;
    LetztGPRSFilename: string;
    procedure Init_FehlerGruppeCode (var Fehlergruppe: integer;
      var Fehlercode: integer);
    procedure KonvGPRS_MRGToXMLFile (sRohdaten: string; MrgTyp: integer;
      var Kennung: string);
    function GetKennungFromGPRS_MRGRohdaten (sRohdaten: string; MrgTyp: integer): string;

    procedure SendGPRSBefehl (ASocket: TCustomWinSocket; ABefehl: string);

    procedure ZeitSync_LeseGeraeteZeit (ClientAdresse: string;
      ClientSocket: TCustomWinSocket);
    procedure ZeitSync_AntwortAuswerten_GeraetezeitLesen (
      GPRSTelegrData: TGPRSTelegrData);
    procedure ZeitSync_SetzeGeraeteZeit (ClientAdresse: string;
      ClientSocket: TCustomWinSocket; NeueMRGZeit: TDateTime);
    procedure ZeitSync_AntwortAuswerten_GeraetezeitSetzen (
      GPRSTelegrData: TGPRSTelegrData);
  public
    constructor Create (AGPRSServerSocket: TServerSocket;
                        AGPRSVerbindungenListe: TGPRSVerbList;
                        AGPRSTelegrammListe: TGPRSTelegrList;
                        AAusgabeDirList: TAusgabeDirList;
                        AAbrufLogFile: TCustomLogFile;
                        ADebugServiceIO: boolean;
                        ADebugRohdaten: boolean;
                        AWLizenz32: TWLizenz32;
                        ACallingFormHandle: HWND);
    function Rufentgegennahme (Kommando: string; var GPRS_gestartet: boolean): string;
    function GPRS_Abfragen (var bSendWieserMsg_NKD: boolean): string;
    procedure GPRS_ZeitSync;
  end;

implementation

{ TGPRSCmdExec }

{---------------------------------------------------------------------}
constructor TGPRSCmdExec.Create (AGPRSServerSocket: TServerSocket;
                                 AGPRSVerbindungenListe: TGPRSVerbList;
                                 AGPRSTelegrammListe: TGPRSTelegrList;
                                 AAusgabeDirList: TAusgabeDirList;
                                 AAbrufLogFile: TCustomLogFile;
                                 ADebugServiceIO: boolean;
                                 ADebugRohdaten: boolean;
                                 AWLizenz32: TWLizenz32;
                                 ACallingFormHandle: HWND);
{---------------------------------------------------------------------}
var
  AI: TAbrufSrvIni;

begin
  inherited Create;
  GPRSServerSocket:=AGPRSServerSocket;
  GPRSVerbindungenListe:=AGPRSVerbindungenListe;
  GPRSTelegrammListe:=AGPRSTelegrammListe;
  FAusgabeDirList:=AAusgabeDirList;
  AbrufLogFile:=AAbrufLogFile;
  DebugServiceIO:=ADebugServiceIO;
  RohdatenLoeschen:=not ADebugRohdaten;
  FWLizenz32:=AWLizenz32;
  CallingFormHandle:=ACallingFormHandle;

  AI:=TAbrufSrvIni.Create;
  try
    bKonvDataLog:=AI.GPRS_DebugDatenProtokoll;
    bAusgabeKurzzeitwerte:=AI.GPRS_AusgabeKurzzeitwerte;
    { Zeitsynchronisation: }
    ZeitSyncAbweichungMin_Cfg:=AI.ZeitSyncAbweichungMin;
    ZeitSyncAbweichungMax_Cfg:=AI.ZeitSyncAbweichungMax;
    bZeitSyncGPRSAktiv_Cfg:=AI.ZeitSyncGPRSAktiv;
    dtZeitSyncStart:=AI.ZeitSyncGPRSStart;  { Startzeit (ohne Datum) }
    if dtZeitSyncStart > Time then
      dtZeitSyncStart:=Date + dtZeitSyncStart  { heute }
    else
      dtZeitSyncStart:=IncDay (Date) + dtZeitSyncStart;  { morgen }

    { Zeichen-Konvertierung ASCII -> ISO 646 ein/aus: }
    bISO646_Cfg:=AI.DSfG_ISO646;  // 03.03.2010, WW
  finally
    AI.Free;
  end;

  Kommando_Rufentgegennahme:='';

  SendeGPRSFiles:=true;  // nach Programmstart sollen evtl. noch vorhandene GPRS-Files gesendet werden
  LetztGPRSFilename:='';
end;

{----------------------------------------------------------------------}
procedure TGPRSCmdExec.Init_FehlerGruppeCode (var Fehlergruppe: integer;
                                              var Fehlercode: integer);
{----------------------------------------------------------------------}
{ Fehlergruppe und Fehlercode mit "OK" vorbelegen;
  Übergabe/Rückgabe: AFehlergruppe
                     AFehlercode }
begin
  Fehlergruppe:=0;
  Fehlercode:=0;
end;

{---------------------------------------------------------------------------------------------}
function TGPRSCmdExec.Rufentgegennahme (Kommando: string; var GPRS_gestartet: boolean): string;
{---------------------------------------------------------------------------------------------}
{ Rufentgegennahme-Kommando ausführen;
  Übergabe: Kommando-String
  Rückgabe: Flag GPRS_gestartet (true, wenn GPRS-Datenempfang mit dem Kommando gestartet wurde)
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  RufModus: integer;
  Lic_OK: boolean;
  sFilename: string;
  sXML: string;
  dtDummy: TDateTime;

begin
  GPRS_gestartet:=false;
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);     // Vorbelegung für Fehlergruppe/-code: OK

  RufModus:=GetRufCmdData (Kommando);

  if RufModus = C_CmdRufStart then begin           { Rufentgegennahme starten }
    Lic_OK:=FWLizenz32.ReadGlobaleFunktionFromLizenzFile (FNExt_GPRS);
    if Lic_OK then begin
      Kommando_Rufentgegennahme:=Kommando;     { Kommando merken für GPRS-Datentelegramm-Antwort }

      GPRS_gestartet:=true;  // GPRS-Datenempfang wurde mit dem Kommando gestartet

      { Botschaft an das aufrufende Fenster schicken: GPRS-Server öffnen }
      PostMessage(CallingFormHandle, WM_OpenGPRSServer, 0, 0);
    end
    else begin
      // Programmfunktion nicht lizenziert:
      Fehlergruppe:=SYS_LICENCEERROR;
      Fehlercode:=LICENCEERR_PROGFUNKTION;
    end;
  end
  else if RufModus = C_CmdRufEnde then begin       { Rufentgegennahme beenden }
    { Botschaft an das aufrufende Fenster schicken: GPRS-Server schließen }
    PostMessage(CallingFormHandle, WM_CloseGPRSServer, 0, 0);
  end
  else if (RufModus = C_CmdRufSMS_OK) then begin  { GPRS-Bestätigung vom Client }
    if AbrufLogFile <> nil then
      AbrufLogFile.Write ('GPRS-Datei quittiert: ' + IntToStr (RufModus));  { Logfile-Protokollierung }

    { wenn der letzte GPRS-Datensatz vom Client erfolgreich empfangen/verarbeitet wurde, kann
      die GPRS-Datei jetzt gelöscht werden: }
    if length (LetztGPRSFilename) > 0 then
      DeleteFile (LetztGPRSFilename);

    // ...und die nächste GPRS-Datei kann gesendet werden, falls vorhanden:
    // Client-GPRS-Antwort aus Datei lesen, Dateiname wird mit zurückgegeben:
    sXML:=GetNextSMSFromTempFile (PathServer.PathName [WWorkDir], prefix_GPRS_XML,
                                  fas_Alle, sFileName, dtDummy);
    if length (sFileName) > 0 then begin  // GPRS-Datei ist vorhanden
      if AbrufLogFile <> nil then
        AbrufLogFile.Write ('GPRS-Datei gelesen (Rufentgegennahme)');  { Logfile-Protokollierung }
      { GPRS-Dateiname merken (muß nach positiver Client-Rückmeldung gelöscht werden !): }
      LetztGPRSFilename:=sFileName;
      Result:=sXML;  { Antwort für Client }
    end
    else begin  // keine weitere GPRS-Datei vorhanden
      LetztGPRSFilename:='';   // GPRS-Merk-Dateiname zurücksetzen
      Result:='';  { keine Antwort für Client ! }
    end;
    exit;
  end
  else begin
    Fehlergruppe:=SYS_ABRUFERROR;
    Fehlercode:=SYSABRFERR_KOMMANDOUNGUELTIG;
  end;

  { Logfile-Protokollierung von Fehlergruppe/-code: }
  if AbrufLogFile <> nil then
    AbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

  { Antwort für Client: }
  Result:=GetClnt_XMLAntwort_Ruf (Fehlergruppe, Fehlercode, Kommando,
                                  '', rc_Ruf_steht_nicht_an,
                                  '',  // unbenutzt
                                  DebugServiceIO, PathServer.PathName [WWorkDir]);
end;

{----------------------------------------------------------------------------}
function TGPRSCmdExec.GPRS_Abfragen (var bSendWieserMsg_NKD: boolean): string;
{----------------------------------------------------------------------------}
{ auf empfangene GPRS-Telegramme prüfen;
  Rückgabe: Flag 'bSendWieserMsg_NKD' (true: Wieser-Nachricht 'Neue Kurzzeitwerte'
              soll versendet werden)
  Ergebnis: XML-Antwort-String, wenn GPRS-Empfangstelegramm ansteht, ansonsten Leer-String }
var
  GPRSTelegrData: TGPRSTelegrData;
  Fehlergruppe: integer;
  Fehlercode: integer;
  AFehlergruppe: integer;
  AFehlercode: integer;
  ArLbDatenListe: TDSfGDataList;
  Kennung: string;
  Datentyp: integer;
  KonvErg: integer;
  SMSKonvDSfGRec: TSMSKonvDSfG;
  OK: boolean;
  sKonvLog: string;
  sXML: string;
  j: integer;
  sFilename: string;
  dtDummy: TDateTime;
  S: string;
  Anford_Nr: string;

begin
  Result:='';
  bSendWieserMsg_NKD:=false;  // Vorbelegung: Wieser-Nachricht "Neue KZW" nicht verschicken

  // Telegramm aus GPRS-Telegrammliste holen und konvertieren, solange vorhanden: }
  while GPRSTelegrammListe.GetTelegramm (GPRSTelegrData) do begin
    if AbrufLogFile <> nil then begin
      // Kennung aus GPRS-Verbindungsliste lesen:
      Kennung:='';
      if Assigned (GPRSVerbindungenListe) then
        Kennung:=GPRSVerbindungenListe.GetKennung (GPRSTelegrData.ClientAdresse);
      AbrufLogFile.Write ('Verarbeite GPRS-Telegramm von >' + GPRSTelegrData.ClientAdresse + '<: ' +
                          'Gerätetyp: ' + IntToStr(GPRSTelegrData.GerTypNr) + '  ' +
                          'Kennung: ' + Kennung);  { Logfile-Protokollierung }
    end;

    case GPRSTelegrData.GerTypNr of
      mrgtyp_MRG910: // MRG 905/910 (Zieldaten: DSfG)
        begin
          if Copy (GPRSTelegrData.Telegramm, 1, 5) = (STX + 'B001') then begin  // Antwort auf B001-Befehl (für Zeitsynchronisation)
            ZeitSync_AntwortAuswerten_GeraetezeitLesen (GPRSTelegrData);
          end
          else if Copy (GPRSTelegrData.Telegramm, 1, 5) = (STX + 'C001') then begin  // Antwort auf C001-Befehl (Zeitsynchronisation)
            ZeitSync_AntwortAuswerten_GeraetezeitSetzen (GPRSTelegrData);
          end
          else begin  // GPRS-Push-Telegramme
            { Fehlergruppe und Fehlercode für Journal mit "OK" vorbelegen: }
            Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);

            { Logfile-Protokollierung: }
            if AbrufLogFile <> nil then
              AbrufLogFile.Write ('Datentelegramm');

            ArLbDatenListe:=TDSfGDataList.Create;  { Archiv/Logbuch-Datenliste }
            try
              Kennung:='';  { Vorbelegung: Kennung unbekannt }
              Datentyp:=0;  { Vorbelegung: Datentyp unbekannt }

              // CRC-Prüfung für MRG-Telegramm durchführen:
              if FCheckPushTelegramm_Pruefsumme (GPRSTelegrData.Telegramm,
                                                 GPRSTelegrData.GerTypNr,
                                                 AFehlergruppe,
                                                 AFehlercode) then begin
                { DSfG-SMS-Konvertierungsrecord belegen:
                  -> den MRG-Telegramminhalt zwischen STX und ETX übergeben }
                SMSKonvDSfGRec.SMS_Data:=ExtractString (GPRSTelegrData.Telegramm, STX, ETX, 0);
                SMSKonvDSfGRec.GeraeteTyp:=GPRSTelegrData.GerTypNr;
                SMSKonvDSfGRec.ZielPfad:=PathServer [WWorkDir];
                SMSKonvDSfGRec.ArLbDatenListe:=ArLbDatenListe;  { Archiv/Logbuch-Datenliste }

                { Telegramm in Zwischendatei mit TDSfGSatzData-Struktur konvertieren,
                  Kennung wird zurückgegeben: }
                OK:=KonvSMS_DSfG (SMSKonvDSfGRec, Kennung, Datentyp, KonvErg, sKonvLog, true);
                // Konvertierungsdaten-Logdatei schreiben
                if bKonvDataLog then
                  WriteGPRSKonvDataLog (sKonvLog, GPRSTelegrData.Telegramm,
                                        PathServer.PathName [WLogDir]);
                if OK then begin
                  // Kennung in GPRS-Verbindungsliste updaten, wenn noch nicht erfolgt:
                  if Assigned (GPRSVerbindungenListe) then
                    GPRSVerbindungenListe.UpdateKennung (GPRSTelegrData.ClientAdresse,
                                                         Kennung);
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
                  S:='Kennung: ' + Kennung + '  Gerätetyp: ' + IntToStr(SMSKonvDSfGRec.GeraeteTyp) +
                     '  KonvErg: ' + IntToStr (KonvErg);
                  WriteGPRSKonvErrorLog (Fehlergruppe, Fehlercode, S, GPRSTelegrData.Telegramm,
                                         PathServer.PathName [WLogDir]);
                end;
              end
              else begin  // CRC-Fehler
                Fehlergruppe:=AFehlergruppe;
                Fehlercode:=AFehlercode;

                // in Konvertierungfehler-Logdatei eintragen
                WriteGPRSKonvErrorLog (Fehlergruppe, Fehlercode, '', GPRSTelegrData.Telegramm,
                                       PathServer.PathName [WLogDir]);
              end;

              if Datentyp = C_IsKurzzeitwerte then begin
                { Kurzzeitwerte werden direkt in die konfigurierten Ausgabeverzeichnisse
                  gestellt: }
                if bAusgabeKurzzeitwerte then begin
                  KonvertDSfG_KZW (ArLbDatenListe, Kennung, GPRSTelegrData.Telegramm,
                                   GPRSTelegrData.TelegrNr, FAusgabeDirList,
                                   PathServer.PathName [WLogDir]);

                  { Flag setzen zum Verschicken der Wieser-Benachrichtigung für neue
                    Kurzzeitwerte: }
                  bSendWieserMsg_NKD:=true;
                end;
              end
              else begin  { Archiv-und Logbuchdaten werden per XML an den Client gesendet }
                { Client-GPRS-Antwort bilden: }
                SendeGPRSFiles:=true;
                if Datentyp = C_IsArchive then  { Ruf-Antwort (DSfG-GPRS) mit Archivkanal-Daten }
                  sXML:=GetClnt_XMLAntwort_AR (Fehlergruppe, Fehlercode,
                                               Kommando_Rufentgegennahme,
                                               Kennung,
                                               ArLbDatenListe, nil, nil, nil,
                                               -1, -1, true, rc_Ruf_SMS_GPRS_DSfG,
                                               '',  // unbenutzt
                                               DebugServiceIO, PathServer.PathName [WWorkDir],
                                               bISO646_Cfg)
                else if Datentyp = C_IsLogbuecher then  { Ruf-Antwort (DSfG-GPRS) mit Logbuch-Daten }
                  sXML:=GetClnt_XMLAntwort_LB (Fehlergruppe, Fehlercode,
                                               Kommando_Rufentgegennahme,
                                               Kennung,
                                               ArLbDatenListe, nil, nil,
                                               -1, -1, true, rc_Ruf_SMS_GPRS_DSfG,
                                               '',  // unbenutzt
                                               DebugServiceIO, PathServer.PathName [WWorkDir])
                else  { Ruf-Antwort (DSfG-GPRS) ohne Daten: }
                  sXML:=GetClnt_XMLAntwort_Ruf (Fehlergruppe, Fehlercode,
                                                Kommando_Rufentgegennahme,
                                                Kennung, rc_Ruf_SMS_GPRS_DSfG,
                                                '',  // unbenutzt
                                                DebugServiceIO,
                                                PathServer.PathName [WWorkDir]);
                { Client-GPRS-Antwort in Datei sichern: }
                SaveSMSToTempFile (PathServer.PathName [WWorkDir], prefix_GPRS_XML, sXML, Now);
              end;

              { SatzData-Dateien wurden konvertiert, können jetzt gelöscht werden }
              for j:=0 to ArLbDatenListe.Count-1 do
                if RohdatenLoeschen then
                  DeleteFile (ArLbDatenListe [j]);
            finally
              ArLbDatenListe.Free;
            end;

            { Logfile-Protokollierung von Fehlergruppe/-code und Konvertierungsergebnis: }
            if AbrufLogFile <> nil then
              AbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode) +
                                  ', ' + IntToStr (KonvErg));
          end;
        end;

      mrgtyp_TDS, mrgtyp_MCO:
        begin
          if Copy (GPRSTelegrData.Telegramm, 1, 1) = STX then begin  // Push-Datentelegramm beginnt mit STX
              { Logfile-Protokollierung: }
              if AbrufLogFile <> nil then
                AbrufLogFile.Write ('Datentelegramm');

            // Datentelegramm konvertieren und in XML-Datei ablegen:
            KonvGPRS_MRGToXMLFile (GPRSTelegrData.Telegramm, GPRSTelegrData.GerTypNr, Kennung);
            SendeGPRSFiles:=true;

            // Kennung in GPRS-Verbindungsliste updaten, wenn noch nicht erfolgt:
            if Assigned (GPRSVerbindungenListe) then
              GPRSVerbindungenListe.UpdateKennung (GPRSTelegrData.ClientAdresse,
                                                   Kennung);

            // Quittung an Gerät senden:
            S:=GetTritschler_IECKommando_40_Quittung_GPRS;
            SendGPRSBefehl (GPRSTelegrData.ClientSocket, S);
          end
          else if Copy (GPRSTelegrData.Telegramm, 1, 2) = '/?' then begin  // Push-Anforderungstelegramm beginnt mit /?
            Anford_Nr:=Copy (GPRSTelegrData.Telegramm, 3, 2);  // Nummer im Anforderungstelegramm
            if Anford_Nr = '43' then begin  // Zeitsynchronisierungsanforderung vom Gerät
              { Logfile-Protokollierung: }
              if AbrufLogFile <> nil then
                AbrufLogFile.Write ('GPRS-Zeitsynchronisation >' + GPRSTelegrData.ClientAdresse + '<: ' +
                                    'Zeitsynchronisierungsanforderung');

              if bZeitSyncGPRSAktiv_Cfg then begin  // GPRS-Zeitsynchronisation aktiv
                // Zeitsynchronisierungsbefehl an Gerät senden:
                S:=GetTritschler_IECKommando_42_ZeitSync_GPRS;
                SendGPRSBefehl (GPRSTelegrData.ClientSocket, S);
              end;
            end
            else begin
              { Logfile-Protokollierung: }
              if AbrufLogFile <> nil then
                AbrufLogFile.Write ('Unbekanntes GPRS-Anforderungstelegramm');
            end;
          end
          else begin
            { Logfile-Protokollierung: }
            if AbrufLogFile <> nil then
              AbrufLogFile.Write ('Unbekanntes GPRS-Telegramm');
          end;
        end;
    else
      { Logfile-Protokollierung: }
      if AbrufLogFile <> nil then
        AbrufLogFile.Write ('Unbekannter Gerätetyp !');
    end;
  end;  { while GPRSTelegrammListe.GetTelegramm }

  { GPRS-Datei versenden:
    - SendeGPRSFiles-Flag gesetzt ist und die letzte versendete GPRS-Datei
      bereits quittiert ist }
  if SendeGPRSFiles AND (length (LetztGPRSFilename) = 0) then begin
    SendeGPRSFiles:=false;
    { Client-GPRS-Antwort aus Datei lesen, Dateiname wird mit zurückgegeben: }
    sXML:=GetNextSMSFromTempFile (PathServer.PathName [WWorkDir], prefix_GPRS_XML,
                                  fas_Alle, sFileName, dtDummy);
    if length (sFileName) > 0 then begin  // GPRS-Datei ist vorhanden
      if AbrufLogFile <> nil then
        AbrufLogFile.Write ('GPRS-Datei gelesen (GPRS_Abfragen)');  { Logfile-Protokollierung }

      { GPRS-Dateiname merken (muß nach positiver Client-Rückmeldung gelöscht werden !): }
      LetztGPRSFilename:=sFileName;
      { Antwort für Client: }
      Result:=sXML;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TGPRSCmdExec.KonvGPRS_MRGToXMLFile (sRohdaten: string; MrgTyp: integer;
                                              var Kennung: string);
{-------------------------------------------------------------------------------}
{ Konvertiert GPRS-MRG-Rohdaten in Datei mit XML-Format (Client-Antwort);
  Übergabe: GPRS-Rohdaten
            Gerätetyp
  Rückgabe: Kennung
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  AFehlergruppe, AFehlercode: integer;
  MessFilenameListe: TStringList;
  TagFilenameListe: TStringList;
  MessFilename: string;
  TagFilename: string;
  MaxMessKanal: integer;
  MaxTagKanal: integer;
  MrgDefData: TMrgDefData;
  Filename: string;
  i: integer;
  Tagesende: integer;
  sXML: string;
  MRGKonvert: TMRGKonvert;
  sRohdateiname: string;
  iDummy: integer;

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);   // Vorbelegung für Fehlergruppe/-code: OK
  { Vorbelegungen für GPRS-MRG-Konvertierung }
  MaxMessKanal:=0;
  MaxTagKanal:=0;
  Tagesende:=CTagesende;  // Default-Tagesende
  Kennung:='';  // Default: Kennung unbekannt

  MessFilenameListe:=TStringList.Create;
  try
    TagFilenameListe:=TStringList.Create;
    try
      // Prüfsummen-Prüfung für MRG-Telegramm durchführen:
      if FCheckPushTelegramm_Pruefsumme (sRohdaten, MrgTyp,
                                         AFehlergruppe, AFehlercode) then begin
        // Kennung aus GPRS-Rohdaten ermitteln:
        Kennung:=GetKennungFromGPRS_MRGRohdaten (sRohdaten, MrgTyp);

        sRohdateiname:=CreateTempRohFile (PathServer.PathName [WWorkDir], prefix_GPRS_Roh);  // File anlegen
        if WriteRohfile (sRohdateiname, sRohdaten) then begin  // Rohdaten in File schreiben
          // GPRS-MRG-Telegramm konvertieren:
          MRGKonvert:=TMRGKonvert.Create (MrgTyp, RohdatenLoeschen, PathServer [WStammDir]);
          try
            MRGKonvert.AddFileName (sRohdateiname, dt_Messwerte);
            if not MRGKonvert.MessTagKonvert (Tagesende,
                                              '',  // KanalaktivMaske für implementierte MRG-Typen nicht benötigt
                                              nil,  // Kanalliste nicht benötigt/verfügbar (nur für norm. LGZ-Werte)
                                              nil,  // Parameterliste nicht benötigt/verfügbar (nur für norm. LGZ-Werte)
                                              false,  // für implementierte MRG-Typen keine Analogwerte-Normierung
                                              MessFilenameListe,
                                              TagFilenameListe,
                                              iDummy) then begin
              Fehlergruppe:=ST_KONFIGERROR;
              Fehlercode:=KFERR_KONFIGDATANOTFOUND;
            end;
          finally
            MRGKonvert.Free;
          end;
        end
        else begin
          Fehlergruppe:=ST_FILEERROR;
          Fehlercode:=FILEERR_COULDNOTWRITE;
        end;
      end
      else begin  // Prüfsummen-Fehler
        Fehlergruppe:=AFehlergruppe;
        Fehlercode:=AFehlercode;

        // in Konvertierungfehler-Logdatei eintragen
        WriteGPRSKonvErrorLog (Fehlergruppe, Fehlercode, '', sRohdaten,
                               PathServer.PathName [WLogDir]);
      end;

      // gerätespezifische Kanalzahlen für Messwert- und Zählerstandsdaten aus Konfiguration lesen
      if GetMrgDefData (MrgTyp, MrgDefData, PathServer [WStammDir]) then begin
        MaxMessKanal:=MrgDefData.AnzahlKanaele;
        MaxTagKanal:=MrgDefData.AnzahlZaehlerkanaele;
      end
      else begin
        Fehlergruppe:=ST_KONFIGERROR;
        Fehlercode:=KFERR_KONFIGDATANOTFOUND;
      end;

      { XML-Antwort für Client: }
      if MessFilenameListe.Count > 0 then
        MessFilename:=MessFilenameListe [0]
      else
        MessFilename:='';
      if TagFilenameListe.Count > 0 then
        TagFilename:=TagFilenameListe [0]
      else
        TagFilename:='';
      sXML:=GetClnt_XMLAntwort_SMS_MWTA (Fehlergruppe, Fehlercode,
                                         Kommando_Rufentgegennahme,
                                         Kennung, rc_Ruf_angenommen_SMS_GPRS_MRG,
                                         MrgTyp,
                                         MessFilename, TagFilename,
                                         MaxMessKanal, MaxTagKanal,
                                         Tagesende,
                                         '',  // unbenutzt
                                         DebugServiceIO,
                                         PathServer.PathName [WWorkDir]);

      { Client-GPRS-Antwort in Datei sichern: }
      SaveSMSToTempFile (PathServer.PathName [WWorkDir], prefix_GPRS_XML, sXML, Now);

      { MRG-Datenfiles können jetzt gelöscht werden: }
      for i:=0 to MessFilenameListe.Count-1 do begin  { alle Messwert-Dateien }
        Filename:=MessFilenameListe [i];
        if length (Filename) > 0 then
          DeleteFile (Filename);
      end;
      for i:=0 to TagFilenameListe.Count-1 do begin   { alle Tagessatz-Dateien }
        Filename:=TagFilenameListe [i];
        if length (Filename) > 0 then
          DeleteFile (Filename);
      end;
    finally
      TagFilenameListe.Free;
    end;
  finally
    MessFilenameListe.Free;
  end;

  { Logfile-Protokollierung von Fehlergruppe/-code: }
  if AbrufLogFile <> nil then
    AbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));
end;

{-----------------------------------------------------------------------------}
function TGPRSCmdExec.GetKennungFromGPRS_MRGRohdaten (sRohdaten: string;
                                                      MrgTyp: integer): string;
{-----------------------------------------------------------------------------}
{ Ermittelt Kennung aus GPRS-MRG-Rohdaten;
  Übergabe: GPRS-Rohdaten
            Gerätetyp
  Ergebnis: Kennung }
var
  Kennziffer_Geraetenummer: string;
  sBuf: string;
  S: string;

begin
  Result:='';  // Vorbelegung: Keine Kennung

  case MrgTyp of
    mrgtyp_TDS, mrgTyp_MCO:
      begin
        Kennziffer_Geraetenummer:='0.0.';      { Gerätenummer-Kennziffer }
        sBuf:=sRohdaten;
        while length (sBuf) > 0 do begin
          S:=F_Zerlegen (sBuf, LF);
          if length (S) > 0 then begin
            if Copy (S, 1, length (Kennziffer_Geraetenummer)) = Kennziffer_Geraetenummer then begin  { Gerätenummer gefunden }
              Result:=ExtractString (S, '(', ')', 0);  { Gerätenummer steht zwischen den runden Klammern }
              Break;
            end;
          end;
        end;  { while length (Antw) }
      end;
  end;  { case MrgTyp }

  Result:=Copy (Result, 1, C_KennungLen);
end;

{---------------------------------------------------------------------------------}
procedure TGPRSCmdExec.SendGPRSBefehl (ASocket: TCustomWinSocket; ABefehl: string);
{---------------------------------------------------------------------------------}
{ Befehl an GPRS-Client senden;
  Übergabe: Zeiger auf Socket, an den der Befehl gesendet werden soll
            Befehl-String }
begin
  if length (ABefehl) > 0 then begin
    if Assigned (ASocket) then begin
      try
        if ASocket.Connected then begin
          { Logfile-Protokollierung: }
          if AbrufLogFile <> nil then
            AbrufLogFile.Write ('GPRS-Befehl senden an >' + ASocket.RemoteAddress + ':' +
                                IntToStr (ASocket.RemotePort) + '<: ' +
                                SonderzeichenString (ABefehl));
          ASocket.SendText (ABefehl);
          if AbrufLogFile <> nil then
            AbrufLogFile.Write ('GPRS-Befehl gesendet');

          if Assigned (GPRSVerbindungenListe) then
            GPRSVerbindungenListe.IncrementSendCounts (ASocket.RemoteAddress, 1, length (ABefehl));
        end
        else begin
          { Logfile-Protokollierung: }
          if AbrufLogFile <> nil then
            AbrufLogFile.Write ('GPRS-Socket ist nicht verbunden ' +
                                '(>' + ASocket.RemoteAddress + ':' +
                                IntToStr (ASocket.RemotePort) + '<');
        end;
      except
        on E: Exception do begin
          { Logfile-Protokollierung bei Client-Socket-Fehler: }
          if AbrufLogFile <> nil then
            AbrufLogFile.Write ('GPRS-Socketverbindung nicht verfügbar');
        end;
      end;
    end
    else begin
      { Logfile-Protokollierung: }
      if AbrufLogFile <> nil then
        AbrufLogFile.Write ('GPRS-Socket ist NIL');
    end;
  end;
end;


{---------------------- Zeitsynchronisation -----------------------------------}

{-----------------------------------}
procedure TGPRSCmdExec.GPRS_ZeitSync;
{-----------------------------------}
{ prüft GPRS-Zeitsynchronisations-Startzeitpunkt und startet Zeitsynchronisation
  für alle verbundenen Geräte, welche nicht von sich aus eine Zeitsynchronisation
  anfordern }
var
  L: TList;
  i, j: integer;
  GeraeteTyp: integer;
  IP_Adresse: string;
  ClientSocket: TCustomWinSocket;

begin
  if not bZeitSyncGPRSAktiv_Cfg then exit;  // nur wenn GPRS-Zeitsynchronisation aktiv

  if Now > dtZeitSyncStart then begin
    dtZeitSyncStart:=IncDay (dtZeitSyncStart);  // Zeitpunkt der nächste Zeitsynchronisation

    { Logfile-Protokollierung: }
    if AbrufLogFile <> nil then
      AbrufLogFile.Write ('Start GPRS-Zeitsynchronisation');

    // Zeitsynchronisation für alle Geräte starten, welche nicht von sich aus
    // eine Zeitsynchronisation anfordern (z.B. Tritschler TDS, MCO):
    if GPRSVerbindungenListe <> nil then begin
      L:=TList.Create;
      try
        GPRSVerbindungenListe.GetList (L);
        for i:=0 to L.Count - 1 do begin
          GeraeteTyp:=TGPRSVerbDataObj (L [i]).Data.GerTypNr;
          if GeraeteTyp = mrgtyp_MRG910 then begin
            IP_Adresse:=TGPRSVerbDataObj (L [i]).Data.IP_Adresse;
            if not TGPRSVerbDataObj (L [i]).Data.Pull_Aktiv then begin  // es darf kein Pull-Abruf aktiv sein
              ClientSocket:=nil;
              if length (IP_Adresse) > 0 then begin
                { Socket der Verbindung zu GPRS-IP-Adresse ermitteln: }
                if GPRSServerSocket <> nil then begin
                  if GPRSServerSocket.Socket.ActiveConnections > 0 then begin
                    for j:=GPRSServerSocket.Socket.ActiveConnections-1 downto 0 do begin
                      ClientSocket:=GPRSServerSocket.Socket.Connections [j];
                      if ClientSocket <> nil then
                        if (ClientSocket.RemoteAddress = IP_Adresse) then
                          Break;  { GPRS-Connection gefunden }
                      ClientSocket:=nil;
                    end; { for }
                  end;
                end;
              end;

              if Assigned (ClientSocket) then
                if ClientSocket.Connected then
                  ZeitSync_LeseGeraeteZeit (IP_Adresse, ClientSocket);  { zuerst aktuelle Gerätezeit auslesen }
            end
            else begin
              { Logfile-Protokollierung: }
              if AbrufLogFile <> nil then
                AbrufLogFile.Write ('Keine GPRS-Zeitsynchronisation für >' + IP_Adresse + '<: Pull-Abruf ist aktiv');
            end;
          end;  { if GeraeteTyp = mrgtyp_MRG910 }
        end;  { for i }
      finally
        for i:=0 to L.Count - 1 do
          if Assigned (L [i]) then
            TGPRSVerbDataObj (L [i]).Free;
        L.Free;
      end;
    end
    else begin
      { Logfile-Protokollierung: }
      if AbrufLogFile <> nil then
        AbrufLogFile.Write ('Keine GPRS-Zeitsynchronisation: GPRS-Verbindungsliste = nil');
    end;  { if GPRSVerbindungenListe <> nil }
  end;  { if Now > dtZeitSyncStart }
end;

{-------------------------------------------------------------------------------}
procedure TGPRSCmdExec.ZeitSync_LeseGeraeteZeit (ClientAdresse: string;
                                                 ClientSocket: TCustomWinSocket);
{-------------------------------------------------------------------------------}
{ liest Gerätezeit aus;
  Übergabe: IP-Adresse
            Zeiger auf Socket, an den der Lese-Befehl gesendet werden soll }
var
  Befehl: string;

begin
  { Logfile-Protokollierung: }
  if AbrufLogFile <> nil then
    AbrufLogFile.Write ('GPRS-Zeitsynchronisation >' + ClientAdresse + '<: Gerätezeit lesen');

  { Befehl zum Lesen der Gerätezeit: }
  Befehl:=GetMRGKommando_B ('001');
  Befehl:=Befehl + GetCRC16_Chars_Hex (scrc16 (Befehl, 0));

  SendGPRSBefehl (ClientSocket, Befehl);

  { PC-Zeit in GPRS-Verbindungsliste merken: }
  if Assigned (GPRSVerbindungenListe) then
    GPRSVerbindungenListe.SetZeitSync_PCTime (ClientAdresse, Time);  // aktuelle Zeit
end;

{-----------------------------------------------------------------}
procedure TGPRSCmdExec.ZeitSync_AntwortAuswerten_GeraetezeitLesen (
  GPRSTelegrData: TGPRSTelegrData);
{-----------------------------------------------------------------}
{ wertet Antwort auf Gerätezeit-Lesebefehl aus;
  Übergabe: GPRS-Telegrammdaten (enthält u.a. Antwort) }
Const
  C_Sicherheitsabstand = 30; { s }  { keine Sync in diesem Zeitraum vor einer vollen Stunde }
  C_KorrGetMRGZeit =  600; { ms }    { ungefährer Korrekturwert für ausgelesene MRG-Zeit  }
  C_KorrSetMRGZeit =  750; { ms }    { ungefährer Korrekturwert für Übertragung der neuen MRG-Zeit }

var
  Fehlergruppe: integer;
  Fehlercode: integer;
  MRGTimeStr: string;
  MRGDateTime: TDateTime;
  PCDateTime: TDateTime;
  MRG_hour, MRG_min, MRG_sec, MRG_msec: word;
  PC_hour, PC_min, PC_sec, PC_msec: word;
  NewMRGDateTime: TDateTime;
  Naechste_volle_Stunde_DateTime: TDateTime;
  DiffSec: longint;
  DiffSec_StdWechsel: longint;

begin
  { Logfile-Protokollierung: }
  if AbrufLogFile <> nil then
    AbrufLogFile.Write ('GPRS-Zeitsynchronisation >' + GPRSTelegrData.ClientAdresse + '<: ' +
                        'Antwort Gerätezeit lesen');

  { In GPRS-Verbindungsliste gemerkte PC-Zeit lesen: }
  if Assigned (GPRSVerbindungenListe) then begin
    GPRSVerbindungenListe.GetZeitSync_PCTime (GPRSTelegrData.ClientAdresse, PCDateTime);
    DecodeTime (PCDateTime, PC_hour, PC_min, PC_sec, PC_msec);
  end
  else begin
    { Logfile-Protokollierung: }
    if AbrufLogFile <> nil then
      AbrufLogFile.Write ('Fehler GPRSVerbindungenListe.GetZeitSync_PCTime', true, lt_Error);
    // -> Fehler, keine weiteren ZeitSync-Aktionen
    exit;
  end;

  { Fehlergruppe und Fehlercode für Journal mit "OK" vorbelegen: }
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);

  { Antwort auf Kommando für "Zeit abrufen" auswerten: }
  if not ValidMRGAntwort ('B001', 4,GPRSTelegrData.Telegramm, Fehlergruppe, Fehlercode) then begin
    { Logfile-Protokollierung von Fehlergruppe/-code: }
    if AbrufLogFile <> nil then
      AbrufLogFile.Write ('Ergebnis: ' +
        IntToStr (EST_ZEITSYNCERROR + Fehlergruppe) + ', ' + IntToStr (Fehlercode));
    // -> Fehler, keine weiteren ZeitSync-Aktionen
  end
  else begin  // OK
    MRGTimeStr:=ExtractString (GPRSTelegrData.Telegramm, STX, ETX, 0);  // Rohdatenteil zwischen STX und ETX
    MRGTimeStr:=Copy (MRGTimeStr, 5, length (MRGTimeStr));
    { Gerätezeit-String in TDateTime wandeln: }
    if not EncodeTimeStr (MRGTimeStr, 'HHMMSS', MRGDateTime) then begin
      { Logfile-Protokollierung: }
      if AbrufLogFile <> nil then
        AbrufLogFile.Write ('Fehler EncodeTimeStr', true, lt_Error);
      // -> Fehler, keine weiteren ZeitSync-Aktionen
    end
    else begin  // OK
      { Gerätezeit:
        -> Korrektur wegen Übertragungslaufzeit }
      MRGDateTime:=MRGDateTime - EncodeTime (0, 0, 0, C_KorrGetMRGZeit);
      DecodeTime (MRGDateTime, MRG_hour, MRG_min, MRG_sec, MRG_msec);

      { Zur Synchronisierung (Feinjustierung) der Gerätezeit werden von
        der aktuellen PC-Zeit nur die Minuten und Sekunden verwendet. Somit
        bleibt auch die im Gerät herrschende Zeitzone erhalten. }
      MRGDateTime:=EncodeTime (0, MRG_min, MRG_sec, MRG_msec);
      PCDateTime:=EncodeTime (0, PC_min, PC_sec, PC_msec);
      F_TimeDiff(PCDateTime, MRGDateTime, DiffSec);
      { Abweichung in GPRS-Verbindungsliste merken für spätere Auswertung des
        Parametrierergebnisses: }
      if Assigned (GPRSVerbindungenListe) then
        GPRSVerbindungenListe.SetZeitSync_DiffSec (GPRSTelegrData.ClientAdresse, DiffSec);

      { Logfile-Protokollierung: }
      if AbrufLogFile <> nil then
        AbrufLogFile.Write ('DiffSec: ' + IntToStr (DiffSec));

      // ZeitSync nur wenn Betrag der Abweichung <= max. Abweichung (auch um
      // Parametrierung über Stundenwechsel zu vermeiden !)
      if Abs (DiffSec) > ZeitSyncAbweichungMax_Cfg then begin
        { Logfile-Protokollierung von Fehlergruppe/-code: }
        if AbrufLogFile <> nil then
          AbrufLogFile.Write ('Ergebnis: ' +
            IntToStr (EST_ZEITSYNCERROR) + ', ' + IntToStr (ZSYNCERR_HIGHERMAX));
        // -> Abbruch, keine weiteren ZeitSync-Aktionen
      end
      else begin
        { ZeitSync nur, wenn Betrag der Abweichung >= min. Abweichung, ansonsten alles OK
          (innerhalb Toleranz) und fertig: }
        if Abs (DiffSec) < ZeitSyncAbweichungMin_Cfg then begin
          { Logfile-Protokollierung von Fehlergruppe/-code: }
          if AbrufLogFile <> nil then
            AbrufLogFile.Write ('Abweichung < Min, Zeitsynchronisation nicht erforderlich');
            // -> Fertig, keine weiteren ZeitSync-Aktionen notwendig
        end
        else begin
          { MRG-Zeit und PC-Zeit müssen den Sicherheitsabstand zum nächsten
            Stundenwechsel einhalten: }
          Naechste_volle_Stunde_DateTime:=EncodeTime(1, 0, 0, 0);
          if MRGDateTime < PCDateTime then
            F_TimeDiff(PCDateTime, Naechste_volle_Stunde_DateTime, DiffSec_StdWechsel)
          else
            F_TimeDiff(MRGDateTime, Naechste_volle_Stunde_DateTime, DiffSec_StdWechsel);

          { ZeitSync nur bei genügend Sicherheitsabstand zum nächsten Stundenwechsel: }
          if DiffSec_StdWechsel <= C_Sicherheitsabstand then begin
            { Logfile-Protokollierung von Fehlergruppe/-code: }
            if AbrufLogFile <> nil then
              AbrufLogFile.Write ('Ergebnis: ' +
                IntToStr (EST_ZEITSYNCERROR) + ', ' + IntToStr (ZSYNCERR_PERIODEND));
            // -> Abbruch, keine weiteren ZeitSync-Aktionen
          end
          else begin
            { Neue Zeit im Gerät parametrieren:
              -> Korrektur wegen Übertragungslaufzeit }
            DecodeTime (Time, PC_hour, PC_min, PC_sec, PC_msec);  // aktuelle Zeit
            NewMRGDateTime:=EncodeTime (MRG_hour, PC_min, PC_sec, PC_msec);
            NewMRGDateTime:=IncMilliSecond (NewMRGDateTime, C_KorrSetMRGZeit);
            ZeitSync_SetzeGeraeteZeit (GPRSTelegrData.ClientAdresse, GPRSTelegrData.ClientSocket, NewMRGDateTime);
          end;
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------}
procedure TGPRSCmdExec.ZeitSync_SetzeGeraeteZeit (ClientAdresse: string;
  ClientSocket: TCustomWinSocket; NeueMRGZeit: TDateTime);
{----------------------------------------------------------------------}
{ setzt Gerätezeit im Gerät;
  Übergabe: IP-Adresse
            Zeiger auf Socket, an den der Lese-Befehl gesendet werden soll
            Neue Gerätezeit }
var
  Befehl: string;
  MRGTimeStr: string;

begin
  { Logfile-Protokollierung: }
  if AbrufLogFile <> nil then
    AbrufLogFile.Write ('GPRS-Zeitsynchronisation >' + ClientAdresse + '<: ' +
                        'Gerätezeit setzen');

  { Befehl zum Setzen der Gerätezeit: }
  MRGTimeStr:=FormatMRGTime('HHMMSS', NeueMRGZeit);

  Befehl:=STX + 'C00112345678 ' + MRGTimeStr + ETX;   // MRG-Standard-Passwort
  Befehl:=Befehl + GetCRC16_Chars_Hex (scrc16 (Befehl, 0));

  SendGPRSBefehl (ClientSocket, Befehl);
end;

{------------------------------------------------------------------}
procedure TGPRSCmdExec.ZeitSync_AntwortAuswerten_GeraetezeitSetzen (
  GPRSTelegrData: TGPRSTelegrData);
{-----------------------------------------------------------------}
{ wertet Antwort auf Gerätezeit-Setzbefehl aus;
  Übergabe: GPRS-Telegrammdaten (enthält u.a. Antwort) }
var
  Fehlergruppe, Fehlercode: integer;
  ZeitSync_erfolgreich: boolean;
  AltWert, NeuWert: string;
  bDiffSec_lesen: boolean;
  DiffSec: longint;

begin
  { Logfile-Protokollierung: }
  if AbrufLogFile <> nil then
    AbrufLogFile.Write ('GPRS-Zeitsynchronisation >' + GPRSTelegrData.ClientAdresse + '<: ' +
                        'Antwort Gerätezeit setzen');

  { Fehlergruppe und Fehlercode für Journal mit "OK" vorbelegen: }
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);

  { Antwort auf "Zeit übertragen"-Kommando auswerten: }
  if not ValidMRGAntwort ('C001', 4, GPRSTelegrData.Telegramm, Fehlergruppe, Fehlercode) then begin
    if (Fehlergruppe = COM_MRGERROR) AND
       ((Fehlercode = MRGERR_AENDERUNGNICHTZULAESSIG) OR (Fehlercode = MRGERR_KEINEBERECHTIGUNG)) then begin
      Fehlergruppe:=EST_ZEITSYNCERROR;
      Fehlercode:=ZSYNCERR_NOSUCCESS;
    end else
      Fehlergruppe:=EST_ZEITSYNCERROR+Fehlergruppe;

    { Logfile-Protokollierung von Fehlergruppe/-code: }
    if AbrufLogFile <> nil then
      AbrufLogFile.Write ('Ergebnis: ' +
        IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));
    // -> Fehler, keine weiteren ZeitSync-Aktionen
  end
  else begin  // OK
    ZeitSync_erfolgreich:=true;
    { Prüfen, ob MRG-Zeit tatsächlich geändert wurde:
      -> für NeuWert wird Dummy-Wert übergeben, dient nur der Längenbestimmung für
         Neuwert }
    NeuWert:='hhmmss';
    if not MRGParameter_geaendert (GPRSTelegrData.Telegramm, AltWert, NeuWert) then begin
      // ZeitSync-Abweichung aus GPRS-Verbindungsliste lesen:
      bDiffSec_lesen:=false;
      if Assigned (GPRSVerbindungenListe) then begin
        if GPRSVerbindungenListe.GetZeitSync_DiffSec (GPRSTelegrData.ClientAdresse, DiffSec) then begin
          bDiffSec_lesen:=true;
          if Abs(DiffSec) > 1 then
            ZeitSync_erfolgreich:=false;
        { bei DiffSec = 1 liefert das Gerät evtl. Altwert = Neuwert. Das ist in diesem Fall
          aber kein Fehler, da wegen unvermeidbarer Ungenauigkeiten evtl. versucht wurde,
          die aktuelle Gerätezeit wieder zu parametrieren. }
        end;
      end;
      if not bDiffSec_lesen then begin
        { Logfile-Protokollierung: }
        if AbrufLogFile <> nil then
            AbrufLogFile.Write ('Fehler GetZeitSync_DiffSec', true, lt_Error);
        exit;
      end;
    end;

    { Logfile-Protokollierung: }
    if AbrufLogFile <> nil then begin
      if ZeitSync_Erfolgreich then
        AbrufLogFile.Write ('GPRS-Zeitsynchronisation >' + GPRSTelegrData.ClientAdresse + '<: ' +
                            'Erfolgreich')
      else
        AbrufLogFile.Write ('GPRS-Zeitsynchronisation >' + GPRSTelegrData.ClientAdresse + '<: ' +
                            'Nicht erfolgreich');
    end;
  end;
end;

end.
