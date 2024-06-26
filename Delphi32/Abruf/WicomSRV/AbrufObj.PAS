{******************************************************************************}
{* Unit: Grund-Objekt f�r Stationsabruf                                       *}
{* 24.01.2003 WW                                                              *}
{* 03.01.2022 WW �berarbeitung COM-Tracelog-Methoden                          *}
{******************************************************************************}
Unit AbrufObj;

INTERFACE

uses
  Windows, Classes, SysUtils, EncdDecd,
  T_Tools, T_Zeit, ErrConst, CommUtil, T_FirmwareBin,
  WComm, T_BinMask, O_Comm, LogFile, DListen, AbrufCmd, RespConst, GD_Utils,
  LogCom, O_ResFilesList, MResUtil;

Const
  { Datenanfragemodi }

  d_Alles  = 1;  { Abfrage der gesamten Datenmenge eines Datentyps (nur MRG) }
  d_VonBis = 2;  { Abfrage �ber Zeitbereich }
  d_DelAdr = 3;  { Abfrage �ber DEL-Adresse (nur DSfG) }
  d_OrdNr  = 4;  { Abfrage �ber Ordnungsnummer (nur DSfG) }

  { Firmware-Update-Stati }

  sfwupd_AktFwVersionLesenOK = 1;  { Akuelle Firmwareversion lesen erfolgreich }
  sfwupd_ParaSichernOK       = 2;  { Parameter sichern erfolgreich }
  sfwupd_FwUebertragenOK     = 3;  { Firmware ins Ger�t �bertragen erfolgreich }
  sfwupd_FwFlashenOK         = 4;  { Neue Firmware im Ger�t flashen erfolgreich }
  sfwupd_ParaRestoreOK       = 5;  { Parameter wiederherstellen erfolgreich }
  sfwupd_NeueVerbindungOK    = 6;  { Verbindung neu aufbauen nach flashen erfolgreich }

Type
  { Callback-Prozedurtypen }
  TCBAbrufIntegerProc = procedure (i: integer) of object;
  TCBAbrufFuncBoolean = function: boolean of object;   
  TCBAbrufStatusText = function (iStatus: integer): shortstring;
  TCBAbrufErrorText = function (iStatus, iError: integer): shortstring;

  { Allgemeines Objekt zum Datenabruf }

  TAbruf = class (TObject)
  private
    FRetFreierText: string;  { Zusatzinformation f�r Ergebnis-R�ckgabe; 13.03.2018, WW }
    FCOMTraceTextList: TStringList;  { Liste zum Zwischenpuffern von TraceLog-Texten; 03.01.2022, WW }
    procedure SetRetFreierText (S: string);
    procedure ClearCOMTraceLog;
  protected
    FCOMNr: integer;  { Schnittstellennummer, auf der der Abruf l�uft }
    FCOMNr_Kommando: integer;  { Schnittstellennummer aus Kommando }
    StationsKennung: string;             { aus Station gelesene Kennung, nicht die f�r den Abruf vorgegebene }
    Fehlergruppe: integer;   { Fehlergruppe serielle Kommunikation }
    Fehlercode: integer;     { Fehlercode serielle Kommunikation, wenn Fehlergruppe <> 0 }
    ModemName: string;
    DebugCOMProtokoll: boolean;
    RohdatenLoeschen: boolean;
    FLogInfo_AbrufLinie: string;
    FKonfigPath: string;
    FWorkPath: string;
    FNetProgPath: string;
    FLogPath: string;
    FFirmwareBinFilePath: string;  { Pfad zu Firmware-Bin�r-Dateien }
    FSignatur_freigeschaltet: boolean;
    FXMLResponseEncodeRohdaten: integer;
    FResourceFilesList: TResourceFilesList;
    FWZ_SZResourceList: TWZ_SZKonfigList;  // WZ_SZ-Ressourcedatenliste; 06.08.2021, WW
    FComTraceLog: TComLogFile;  // COM-Logfile-Instanz f�r TraceLog

    FCBFwUpdStatus: TCBAbrufIntegerProc;
    FCBFwUpdConfirmOldVersion: TCBAbrufFuncBoolean;
    Procedure FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode: integer;
                                      bInit: boolean = false); virtual;
    function GetRetFreierText: string; virtual;
    function CheckModemConnect (ModemAntwort: string): boolean;
    function CopyDatei (sExistingFileName, sNewFileName: string): boolean;
    { Ger�te-Firmware-Update: }
    function CheckFirmwareBinFile (sBinFilename: string): boolean;
    function ValidFirmwareBinData (FwBinInfo: TFwBinInfo; sFwTyp_Geraet: string;
      sFwBuild_Geraet: string; var bGeraeteFwIstAktuell: boolean): boolean;
    function ValidFirmwareUpdateAntwort_NewBaud (sAntwort_NewBaud: string;
      var iNewBaud: integer): boolean;
    function FirmwareBinData_Uebertragen (sBinData: string; iStartAdresse: integer;
     ACommObj: TCommObj): boolean;
    procedure WriteLog_FirmwareUpdate (iFktCode: integer; sAktion: string;
      bErgebnis: boolean);
    procedure Write_FirmwareUpdate_ParaRestoreResultFile (sFilename,
      sResult: string);
    { Kodierte Rohdaten aus Abruf: }
    function CreateRohdatenFiles_XMLResponse (RohfileSrcListe,
      RohfileDestListe: TDSfGDataList; sEAdr, sDEL: string): boolean;
    { Zeitsynchronisation: }
    function Get_DevicePC_TimezoneDiff (sUtcOffsetDev: string): TDateTime;
    { COM-Tracelog }
    procedure WriteCOMTraceLog (S: string);
  public
    NoCarrier: boolean;      { true, wenn keine Verbindung besteht }
    Constructor Create (ACOMNr: integer; ACOMNr_Kommando: integer;
                        AModemname: string; ADebugCOMProtokoll: boolean;
                        ARohdatenLoeschen: boolean; AKonfigPath, AWorkPath,
                        ANetProgPath, ALogPath, AFirmwareBinFilePath: string;
                        ASignatur_freigeschaltet: boolean;
                        AXMLResponseEncodeRohdaten: integer;
                        AResourceFilesList: TResourceFilesList;
                        ACBFwUpdStatus: TCBAbrufIntegerProc);
    procedure GetFehlerGruppeCode (var AFehlergruppe: integer; var AFehlercode: integer);
    procedure CreateCOMTraceLog; virtual;
    procedure FreeCOMTraceLog; virtual;
    procedure AddCOMTraceLogToList (AResponseTraceLogList: TDSfGDataList;
      sDSfG_EAdr: string = '');

    property RetFreierText: string read GetRetFreierText write SetRetFreierText;
    property CBFwUpdConfirmOldVersion: TCBAbrufFuncBoolean read
      FCBFwUpdConfirmOldVersion write FCBFwUpdConfirmOldVersion;
  end;

IMPLEMENTATION


{ TAbruf }

{------------------------------------------------------------------------------}
Constructor TAbruf.Create (ACOMNr: integer; ACOMNr_Kommando: integer;
                           AModemname: string; ADebugCOMProtokoll: boolean;
                           ARohdatenLoeschen: boolean; AKonfigPath, AWorkPath,
                           ANetProgPath, ALogPath, AFirmwareBinFilePath: string;
                           ASignatur_freigeschaltet: boolean;
                           AXMLResponseEncodeRohdaten: integer;
                           AResourceFilesList: TResourceFilesList;
                           ACBFwUpdStatus: TCBAbrufIntegerProc);
{------------------------------------------------------------------------------}
{ �bergaben: COM-Port (wenn > 0) bzw. IP-Abrufnummer (wenn < 0), auf der der Abruf l�uft
             COM-Port/IP-Abrufnummer aus Kommando
             Name des angeschlossenen Modems
             Flag 'ADebugCOMProtokoll': wenn true, werden fehlerhafte Ger�terohdaten
                                        protokolliert (f�r Debugging)
             Flag 'ARohdatenLoeschen': wenn false, bleiben Rohdaten nach der Weiter-
                                       verarbeitung erhalten (f�r Debugging)
             Pfad zu Konfigurations-Dateien (Ressourcendateien)
             Work-Pfad
             Netz-Pfad zu Programmdateien
             Pfad f�r Log-Dateien
             Pfad zu Firmware-Bin�r-Dateien (f�r Ger�te-Firmware-Update)
             Flag 'Signatur_freigeschaltet': Signatur per Lizenz freigeschaltet ja/nein
             Code f�r Kodierverfahren f�r Rohdaten in XML-Response (0 = Keine
               Rohdaten in XML-Response)
             Liste der Ressourcedatenlisten
             Callback-Procedure f�r Firmware-Update-Status }
Begin
  inherited Create;
  FCOMNr:=ACOMNr;
  FCOMNr_Kommando:=ACOMNr_Kommando;
  Modemname:=AModemname;
  DebugCOMProtokoll:=ADebugCOMProtokoll;
  RohdatenLoeschen:=ARohdatenLoeschen;
  { Abruflinie f�r Log-Protokollierung }
  if ACOMNr > 0 then  { COM-Port }
    FLogInfo_AbrufLinie:='COM' + IntToStr (ACOMNr)
  else                 { IP-Abrufnummer }
    FLogInfo_AbrufLinie:='IP' + IntToStr (Abs (ACOMNr));

  FKonfigPath:=AKonfigPath;
  FWorkPath:=AWorkPath;
  FNetProgPath:=ANetProgPath;
  FLogPath:=ALogPath;
  FFirmwareBinFilePath:=AFirmwareBinFilePath;
  FSignatur_freigeschaltet:=ASignatur_freigeschaltet;
  FXMLResponseEncodeRohdaten:=AXMLResponseEncodeRohdaten;
  FResourceFilesList:=AResourceFilesList;

  if Assigned (FResourceFilesList) then  // 06.08.2021, WW
    FWZ_SZResourceList:=TWZ_SZKonfigList (FResourceFilesList.ListObject [rft_WZ_SZ])
  else
    FWZ_SZResourceList:=nil;

  FCBFwUpdStatus:=ACBFwUpdStatus;

  FComTraceLog:=nil;  // 03.01.2022, WW
  FCOMTraceTextList:=nil;  // 03.01.2022, WW

  { Initialisieren der Fehlerstati }
  Fehlergruppe:=0;
  Fehlercode:=0;
  FRetFreierText:='';

  StationsKennung:='';
  NoCarrier:=true;                 { Vorbelegung: es besteht keine Verbindung }
End;

{---------------------------------------------------------------------------}
Procedure TAbruf.FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode: integer;
                                         bInit: boolean = false);
{---------------------------------------------------------------------------}
{ setzt Fehlergruppe und Fehlercode;
  �bergabe: AFehlergruppe
            AFehlercode
            Flag zum Initialisieren der erweiterten Ergebnis-R�ckgabe }
Begin
  Fehlergruppe := AFehlergruppe;
  Fehlercode := AFehlercode;

  if bInit then
    SetRetFreierText('');  // Freien Text f�r Ergebnis-R�ckgabe l�schen
End;

{------------------------------------------------------------------------------------------}
Procedure TAbruf.GetFehlerGruppeCode (var AFehlergruppe: integer; var AFehlercode: integer);
{------------------------------------------------------------------------------------------}
{ global in der Klasse verwendete Fehlergruppe und Fehlercode zur�ckgeben;
  R�ckgaben: AFehlergruppe
             AFehlercode }
Begin
  AFehlergruppe:=Fehlergruppe;
  AFehlercode:=Fehlercode;
End;

{---------------------------------------}
function TAbruf.GetRetFreierText: string;
{---------------------------------------}
{ Liefert freien Text f�r erweiterte Ergebnis-R�ckgabe }
begin
  Result:=FRetFreierText;
end;

{--------------------------------------------}
procedure TAbruf.SetRetFreierText (S: string);
{--------------------------------------------}
{ Setzt freien Text f�r erweiterte Ergebnis-R�ckgabe }
begin
  FRetFreierText:=S;
end;

{----------------------------------------------------------------}
function TAbruf.CheckModemConnect (ModemAntwort: string): boolean;
{----------------------------------------------------------------}
{ Modemantwort auf Verbindungsaufbau-Befehl bzw. Rufannahme-Befehl pr�fen;
  Ergebnis: true, wenn CONNECT erfolgt ist.
  Falls kein CONNECT erfolgt ist, werden Fehlergruppe und Fehlercode gesetzt }
var
  AFehlergruppe: integer;
  AFehlercode: integer;

begin
  Result:=ValidModemConnectAnswer (ModemAntwort, AFehlergruppe, AFehlercode);
  if not Result then
    FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode);
end;

{---------------------------------------------------------------------------}
function TAbruf.CopyDatei (sExistingFileName, sNewFileName: string): boolean;
{---------------------------------------------------------------------------}
{ Kopiert eine Datei. Zielverzeichnis der neuen Datei mu� vorhanden sein.
  �bergabe: Quelldatei
            Neue Datei
  Ergebnis: true, wenn Datei kopiert werden konnte }
begin
  Result:=CopyFile (pchar (sExistingFileName), pchar (sNewFileName), false);
  if not Result then
    FehlerGruppeCodeUpdate (ST_FILEERROR, FILEERR_COULDNOTWRITE);
end;


{--------------------- Firmware-Update ----------------------------------------}

{-------------------------------------------------------------------}
function TAbruf.CheckFirmwareBinFile (sBinFilename: string): boolean;
{-------------------------------------------------------------------}
{ Datei mit Firmware-Bin�rdaten pr�fen;
  �bergabe: Name der Firmware-Bin�rdatendatei
  Ergebnis: true, wenn Pr�fung OK }
const
  C_MaxBinFileSize = 10000000;  { Max. 10 MB Bin�rdateigr��e (obere Sicherheitsgrenze) }

begin
  Result:=false;
  if not FileExists (sBinFilename) then begin  { Datei mit Firmware-Bin�rdaten nicht gefunden }
    FehlerGruppeCodeUpdate (EST_FIRMWAREUPDATEERROR, FIRMWUPDERR_BINDATA_NICHTGEFUNDEN);
    exit;
  end;

  if F_Check (sBinFilename) > C_MaxBinFileSize then begin  { Datei mit Firmware-Bin�rdaten zu gro� }
    FehlerGruppeCodeUpdate (EST_FIRMWAREUPDATEERROR, FIRMWUPDERR_BINDATA_ZUGROSS);
    exit;
  end;
  Result:=true;
end;

{---------------------------------------------------------------------------------}
function TAbruf.ValidFirmwareBinData (FwBinInfo: TFwBinInfo; sFwTyp_Geraet: string;
  sFwBuild_Geraet: string; var bGeraeteFwIstAktuell: boolean): boolean;
{---------------------------------------------------------------------------------}
{ Firmware-Bin�rdaten analysieren und auf G�ltigkeit pr�fen;
  �bergabe: Bin�rdaten-Informationen-Record
            Firmware-Typ-Information des Ger�ts
            Firmware-Build-Information des Ger�ts (Format yyyymmddhhmmss)
  R�ckgabe: Flag 'bGeraeteFwIstAktuell' (true, wenn Firmware im Ger�t auf aktuellem
                                         Stand ist)
  Ergebnis: true, wenn Firmware-Bin�rdaten g�ltig sind }
var
  iTyp_Geraet: integer;
  sDate: string;
  sTime: string;
  dtBuild_Geraet: TDateTime;
  dtBuild_Bin: TDateTime;
  dtBuf: TDateTime;
  bFlashOldVersion: boolean;

begin
  Result:=false;
  { Vorbelegung R�ckgabe: }
  bGeraeteFwIstAktuell:=false;  // Firmware im Ger�t ist nicht aktuell

  { Firmware-Bin�rdaten analysieren: }
  with FwBinInfo do begin
    if (StartAdresse < $00000) OR (StartAdresse > $FFFFF) then begin  { Startadresse in Bin�rdaten ung�ltig }
      FehlerGruppeCodeUpdate (EST_FIRMWAREUPDATEERROR, FIRMWUPDERR_BINDATA_STARTADRUNGUELTIG);
      exit;
    end;

    if (EndAdresse < $00000) OR (EndAdresse > $FFFFF) then begin  { Endadresse in Bin�rdaten ung�ltig }
      FehlerGruppeCodeUpdate (EST_FIRMWAREUPDATEERROR, FIRMWUPDERR_BINDATA_ENDADRUNGUELTIG);
      exit;
    end;

    if Typ = -1 then begin  { Typ-Information fehlt in Bin�rdaten }
      FehlerGruppeCodeUpdate (EST_FIRMWAREUPDATEERROR, FIRMWUPDERR_BINDATA_TYP_FEHLT);
      exit;
    end;

    iTyp_Geraet:=StrToIntDef (sFwTyp_Geraet, -1);
    if Typ <> iTyp_Geraet then begin  { Typ-Information in Bin�rdaten pa�t nicht zum Ger�t }
      FehlerGruppeCodeUpdate (EST_FIRMWAREUPDATEERROR, FIRMWUPDERR_BINDATA_TYP_FALSCH);
      exit;
    end;

    if length (Build) = 0 then begin  { Build-Information fehlt in Bin�rdaten }
      FehlerGruppeCodeUpdate (EST_FIRMWAREUPDATEERROR, FIRMWUPDERR_BINDATA_BUILD_FEHLT);
      exit;
    end;

    { Aktuelle Build-Info des Ger�ts in DateTime-Struktur wandeln (Format yyyymmddhhmmss): }
    sDate:=Copy (sFwBuild_Geraet, 1, 8);
    sTime:=Copy (sFwBuild_Geraet, 9, 6);
    if EncodeDateStr (sDate, 'YYYYMMDD', dtBuf) then begin
      dtBuild_Geraet:=dtBuf;
      if EncodeTimeStr (sTime, 'HHMMSS', dtBuf) then
        dtBuild_Geraet:=dtBuild_Geraet + dtBuf
      else
        dtBuild_Geraet:=0;  // bei ung�ltiger Build-Info des Ger�ts immer updaten
    end else
      dtBuild_Geraet:=0;  // bei ung�ltiger Build-Info des Ger�ts immer updaten

    { Build-Info in Bin�rdaten in DateTime-Struktur wandeln (Format yyyymmddhhmmss): }
    sDate:=Copy (Build, 1, 8);
    sTime:=Copy (Build, 9, 6);
    if EncodeDateStr (sDate, 'YYYYMMDD', dtBuf) then begin
      dtBuild_Bin:=dtBuf;
      if EncodeTimeStr (sTime, 'HHMMSS', dtBuf) then
        dtBuild_Bin:=dtBuild_Bin + dtBuf
      else
        dtBuild_Bin:=-1;
    end else
      dtBuild_Bin:=-1;

    if dtBuild_Bin < 0 then begin  { Ung�ltige Build-Information in Bin�rdaten }
      FehlerGruppeCodeUpdate (EST_FIRMWAREUPDATEERROR, FIRMWUPDERR_BINDATA_BUILD_UNGUELTIG);
      exit;
    end;

    if dtBuild_Bin < dtBuild_Geraet then begin  { Build-Information in Bin�rdaten ist �lter als im Ger�t }
      bFlashOldVersion:=false;  // Vorbelegung: �ltere Version nicht flashen
      if Assigned (CBFwUpdConfirmOldVersion) then  // 25.07.2012, WW
        if CBFwUpdConfirmOldVersion then
          bFlashOldVersion:=true;  // �ltere Version soll geflasht werden 

      if not bFlashOldVersion then begin
        FehlerGruppeCodeUpdate (EST_FIRMWAREUPDATEERROR, FIRMWUPDERR_BINDATA_BUILD_ALT);
        exit;
      end;
    end;

    if CmpDateTime (dtBuild_Bin, dtBuild_Geraet) = 0 then  { Build-Information in Bin�rdaten und Ger�t sind gleich }
      bGeraeteFwIstAktuell:=true;  // R�ckgabe: Firmware im Ger�t ist aktuell
  end;
  Result:=true;
end;

{---------------------------------------------------------------------------}
function TAbruf.ValidFirmwareUpdateAntwort_NewBaud (sAntwort_NewBaud: string;
  var iNewBaud: integer): boolean;
{---------------------------------------------------------------------------}
{ �berpr�ft den in der Antwort auf den Firmwareupdate-Befehl '1F' enthaltenen
  Code f�r die neue Baudrate und liefert die neue Baudrate als Ganzzahl zur�ck;
  �bergabe: Code f�r neue Baudrate
  R�ckgabe: Neue Baudrate (-1, wenn Baudrate gleich bleiben soll)
  Ergebnis: true, wenn �berpr�fung des Baudraten-Codes OK }
begin
  Result:=false;

  iNewBaud:=0;
  if length (sAntwort_NewBaud) > 0 then begin
    case sAntwort_NewBaud [1] of
      '0': iNewBaud:=   -1;   // Bd nicht ver�ndern
      '1': iNewBaud:= 9600;   // 9600 Bd
      '2': iNewBaud:=19200;   // 19200 Bd
      '3': iNewBaud:=38400;   // 19200 Bd
      '4': iNewBaud:=57600;   // 19200 Bd
      '5': iNewBaud:=115200;  // 19200 Bd
    end;
  end;
  if iNewBaud = 0 then begin
    // Baudrate wird vom Programm nicht unterst�tzt
    FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_BAUD_NICHT_UNTERSTUETZT);
    exit;
  end;

  Result:=true;
end;

{------------------------------------------------------------}
function TAbruf.FirmwareBinData_Uebertragen (sBinData: string;
  iStartAdresse: integer; ACommObj: TCommObj): boolean;
{------------------------------------------------------------}
{ Firmware-Bin�rdaten ins Ger�t �bertragen;
  �bergabe: Bin�rdaten
            Startadresse in Bin�rdaten (Adresse des ersten Blocks der ein Zeichen
              ungleich $FF enth�lt)
            Zeiger auf CommObj
  Ergebnis: true, wenn �bertragung erfolgreich }
const
  C_FwAdrOffset = $F00000;

  C_TO_FwInit     =  3500;  { Timeout in ms f�rs Initialisieren der Firmware-�bertragung
                              -> hochgesetzt von 2 auf 3,5 s wegen l�ngerer Laufzeiten
                                 bei GSM-/GPRS-Verbindung; 03.12.2013, WW }
  C_TO_FwTransfer = 60000;  { Timeout in ms f�rs �bertragen der Firmware ins Ger�t
                              -> hochgesetzt von 2 auf 60 s wegen l�ngerer Laufzeiten
                                 bei GSM-/GPRS-Verbindung; 31.07.2013, WW }
  C_TO_FwFlash    = 60000;  { Timeout in ms f�rs Flashen der Firmware im Ger�t
                              -> hochgesetzt von 10 auf 60 s wegen l�ngerer Laufzeiten
                                 bei GSM-/GPRS-Verbindung; 31.07.2013, WW }
var
  i: integer;
  sBlock: string;
  bSendAdresssatz: boolean;          
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;
  { Es wird zun�chst solange ein '?' gesendet, bis vom Ger�t als Antwort 'R' kommt: }
  for i:=1 to 10 do begin  // max. 10 Versuche
    if not ACommObj.SendCommand ('?', ['R'], 1, C_TO_FwInit, ad_String, R, NoCarrier) then begin
      if NoCarrier OR (i = 10) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
    end else
      Break;  // OK
  end;  { for i }

  { Ger�t ist jetzt bereit f�r �bertragung der Bin�rdaten (Adress- und Datens�tze). }

  { In Bin�rdaten auf Startadresse positionieren (erster Block der ein Zeichen
    ungleich $FF enth�lt): }
  i:=iStartAdresse + 1;
  bSendAdresssatz:=true;  // Flag: Adresssatz mu� gesendet werden
  while i <= length (sBinData) do begin
    sBlock:=Copy (sBinData, i, C_BlockGroesseFirmware);  // einen Block ausschneiden

    { Pr�fen, ob Block Daten enth�lt oder "leer" ist: }
    if FirmwareBinDataEmpty (sBlock) then begin
      { Block enth�lt keine Daten }
      bSendAdresssatz:=true;  // Flag setzen: Adresssatz mu� bei n�chstem Block mit Daten gesendet werden
      { Block-Daten werden nicht gesendet, da leer }
    end
    else begin
      { Block enth�lt Daten }
      if bSendAdresssatz then begin  // Flag auswerten: Adresssatz mu� gesendet werden
        bSendAdresssatz:=false;  // Flag l�schen

        { Adresssatz senden: }
        Befehl:='a' + Copy (Integer2Bin (C_FwAdrOffset + i - 1), 1, 3);  // a, 3 Zeichen Adresse
        Befehl:=Befehl + Chr (GetXOR (Befehl));  // 1 Zeichen XOR anh�ngen
        if not ACommObj.SendFwBinData (Befehl, C_TO_FwTransfer, R, NoCarrier) then begin
          FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
          exit;
        end;
      end;

      { Datensatz senden: }
      Befehl:='d' + sBlock;  // d, Bin�rdaten-Block
      Befehl:=Befehl + Chr (GetXOR (Befehl));  // 1 Zeichen XOR anh�ngen
      if not ACommObj.SendFwBinData (Befehl, C_TO_FwTransfer, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
    end;

    inc (i, C_BlockGroesseFirmware)
  end;  { while i <= length (sBinData) }

  if Assigned (FCBFwUpdStatus) then
    FCBFwUpdStatus (sfwupd_FwUebertragenOK);

  { �bertragung der Firmware-Bin�rdaten ist abgeschlossen: END-Befehl an Ger�t
    senden und Zeichen empfangen w�hrend Ger�t die neue Firmware in den Flash
    schreibt }
  if not ACommObj.SendFwEndCommand (C_TO_FwFlash, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  if Assigned (FCBFwUpdStatus) then
    FCBFwUpdStatus (sfwupd_FwFlashenOK);

  Result:=true;
end;

{-----------------------------------------------------------------------------}
procedure TAbruf.WriteLog_FirmwareUpdate (iFktCode: integer; sAktion: string;
  bErgebnis: boolean);
{-----------------------------------------------------------------------------}
{ Firmware-Update-Logdatei schreiben }
var
  S: string;
  dt: TDateTime;
  sErgebnis: string;
  sLogFilename: string;

begin
  if (iFktCode = C_CmdFktCode_FwUpdGeraet) OR
     (iFktCode = C_CmdFktCode_FwUpdGeraet_Param) then begin
    sLogFilename:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_FwUpdate';

    if not FileExists (FLogPath + sLogFilename + '.log') then begin
      { 1. Eintrag im Logfile enth�lt �berschriften: }
      S:='Datum' + #9 + 'Zeit' + #9 + 'Abruflinie' + #9 + 'Aktion' + #9 + 'Ergebnis';
      WriteDebugLog (FLogPath, sLogFilename, S, false);
    end;

    { OK/Fehler: }
    if not bErgebnis then
      sErgebnis:='Fehler'
    else
      sErgebnis:='OK';

    dt:=Now;
    S:=FormatDateTime ('dd.mm.yyyy', dt) + #9 +
       FormatDateTime ('hh:nn:ss', dt) + #9 +
       FLogInfo_AbrufLinie + #9 + sAktion + #9 + sErgebnis;

    WriteDebugLog (FLogPath, sLogFilename, S, false);
  end;
end;

{------------------------------------------------------------------------------}
procedure TAbruf.Write_FirmwareUpdate_ParaRestoreResultFile (sFilename,
  sResult: string);
{------------------------------------------------------------------------------}
{ Datei mit Ergebnissen beim Wiederherstellen der Parameter bei Firmware-Update
  schreiben }
var
  S: string;

begin
  if not FileExists (sFilename) then begin
    { 1. Eintrag in der Datei enth�lt �berschriften: }
    S:='Ergebnis' + #9 + 'Parameternummer' + #9 + '�nderung Name' + #9 +
       'Name (Vorversion)' + #9 + 'Name (neue Version)' + #9 +
       'Wert alt (neue Version)' + #9 + 'Wert neu (neue Version)' + #9 +
       'Fehlerbeschreibung';
    StringToFile (S, sFilename, true, true, true);
  end;

  StringToFile (sResult, sFilename, false, true, true);
end;


{------------------------ COM-Tracelog ----------------------------------------}

{---------------------------------}
procedure TAbruf.CreateCOMTraceLog;
{---------------------------------}
// COM-Tracelog createn
begin
  // COM-Tracelog-Textliste zum Zwischenpuffern von TraceLog-Texten createn
  FCOMTraceTextList:=TStringList.Create;
  FCOMTraceTextList.Duplicates:=dupAccept;
  FCOMTraceTextList.Sorted:=false;

  // COM-Logfile-Instanz mit Callback-Aufruf von WriteCOMTraceLog createn
  FComTraceLog:=TComLogFile.CreateCB (WriteCOMTraceLog);
end;

{-------------------------------}
procedure TAbruf.FreeCOMTraceLog;
{-------------------------------}
// COM-Tracelog freigeben
begin
  FreeAndNil (FComTraceLog);
  FreeAndNil (FCOMTraceTextList);
end;

{--------------------------------------------}
procedure TAbruf.WriteCOMTraceLog (S: string);
{--------------------------------------------}
// COM-Tracelog-Textliste fortschreiben
const
  C_ListStrLen = 1024;  // max. String-L�nge in COM-Tracelog-Textliste
                        // -> Zur Vermeidung eines evtl. m�glichen Geschwindigkeits-
                          //  problems bei sehr langen Strings (1 MB)
begin
  if Assigned (FCOMTraceTextList) then begin
    if FCOMTraceTextList.Count = 0 then
      FCOMTraceTextList.Add('')  // mit dem ersten Listeneintrag f�ngt es an...
    else if length (FCOMTraceTextList [FCOMTraceTextList.Count-1]) > C_ListStrLen then
      FCOMTraceTextList.Add('');  // neuer Listeneintrag, wenn max. L�nge erreicht ist
    FCOMTraceTextList [FCOMTraceTextList.Count-1]:=
      FCOMTraceTextList [FCOMTraceTextList.Count-1] + S;  // Liste fortschreiben
  end;
end;

{--------------------------------}
procedure TAbruf.ClearCOMTraceLog;
{--------------------------------}
// COM-Tracelog-Textliste leeren
begin
  if Assigned (FCOMTraceTextList) then
    FCOMTraceTextList.Clear;
end;

{--------------------------------------------------------------------------}
procedure TAbruf.AddCOMTraceLogToList (AResponseTraceLogList: TDSfGDataList;
  sDSfG_EAdr: string = '');
{--------------------------------------------------------------------------}
// Inhalt der COM-Tracelog-Textliste (Zwischenpuffer) in Responseliste eintragen;
// �bergaben: ResponseTraceLog-Liste
//            DSfG-Busadresse (optional, f�r MRG leer)
var
  i: integer;
  TraceLogListObj: TDSfGDataListObj;

begin
  if Assigned (AResponseTraceLogList) AND
     Assigned (FCOMTraceTextList) then begin
    if FCOMTraceTextList.Count > 0 then begin
      // Zwischenpuffer in ResponseTraceLog-Liste eintragen:
      for i:=0 to FCOMTraceTextList.Count-1 do begin
        TraceLogListObj:=TDSfGDataListObj.Create;  // 03.01.2022, WW
        TraceLogListObj.SetData (sDSfG_EAdr, '');  // Feld DEL: Leer = Default f�r
                                                   // XML-Response-Ausgabe unerledigt
        AResponseTraceLogList.AddObject (FCOMTraceTextList [i], TraceLogListObj);
      end;

      ClearCOMTraceLog;  // Zwischenpuffer leeren
    end;
  end;
end;


{--------------------- Sonstige Funktionen ------------------------------------}

{---------------------------------------------------------------}
function TAbruf.CreateRohdatenFiles_XMLResponse (RohfileSrcListe,
  RohfileDestListe: TDSfGDataList; sEAdr, sDEL: string): boolean;
{---------------------------------------------------------------}
{ Erstellt Dateien mit kodierten Rohdaten;
  �bergabe: Liste mit Namen der Rohdatendateien aus Abruf (Quelle)
            Liste mit Namen der erzeugten Dateien mit kodierten Rohdaten (Ziel)
            DSfG-Busadresse
            DSfG-Datenelementadresse
  Ergebnis: false, wenn Fehler beim Erzeugen der Dateien mit kodierten Rohdaten
            aufgetreten ist }
var
  sFilenameSrc, sFilenameDest: string;
  fsSrc, fsDest: TFileStream;
  i: integer;
  ResponseRohdatenObj: TResponseRohdatenObj;

begin
  Result:=true;
  if FXMLResponseEncodeRohdaten > 0 then begin
    for i:=0 to RohfileSrcListe.Count-1 do begin
      sFilenameSrc:=RohfileSrcListe[i];
      if not FileExists (sFilenameSrc) then Continue;

      sFilenameDest:=ChangeFileExt (sFilenameSrc, ext_Base64);
      try
        fsSrc:=TFileStream.Create (sFilenameSrc, fmOpenRead OR fmShareDenyWrite);
        try
          fsDest:=TFileStream.Create (sFilenameDest, fmCreate);
          try
            EncodeStream(fsSrc, fsDest);  // Base64-kodierte Rohdaten in Zieldatei schreiben
          finally
            fsDest.Free;
          end;
        finally
          fsSrc.Free;
        end;

        { Name der erzeugten, kodierten Rohdatendatei mit DSfG-Busadresse,
          DE-Adresse und Code f�r Kodierverfahren in Ziel-Liste eintragen: }
        if Assigned (RohfileDestListe) then begin
          ResponseRohdatenObj:=TResponseRohdatenObj.Create;
          ResponseRohdatenObj.SetData (sEAdr, sDEL, rdenc_Base64);
          RohfileDestListe.AddObject (sFilenameDest, ResponseRohdatenObj);
        end;
      except
        Result:=false;
      end;
    end;  { for i }
  end;
end;

// 24.02.2020, WW
{---------------------------------------------------------------------------}
function TAbruf.Get_DevicePC_TimezoneDiff (sUtcOffsetDev: string): TDateTime;
{---------------------------------------------------------------------------}
{ Liefert UTC-Zeitzonen-Differenz zwischen Normalzeit des Ger�ts und Normalzeit des PC;
  �bergabe: UTC-Normalzeit-Offset des Ger�ts (Format: +-hhmm)
  Ergebnis: UTC-Zeitzonen-Differenz }
var
  bNegative: boolean;
  sOffset: string;
  dtUtcOffsetDev: TDateTime;

begin
  if length (sUtcOffsetDev) > 0  then begin
    // UTC-Normalzeit-Offset des Ger�ts
    bNegative:=false;
    sOffset:=sUtcOffsetDev;
    if sOffset [1] in ['+', '-'] then begin
      bNegative:=sOffset [1] = '-';
      sOffset:=Copy (sOffset, 2, length (sOffset));  // Vorzeichen ist optional
    end;

    EncodeTimeStr (sOffset, 'HHMM', dtUtcOffsetDev);
    if bNegative then
      dtUtcOffsetDev:=dtUtcOffsetDev * (-1);

    // Abweichung UTC-Normalzeit-Offset des Ger�ts von UTC-Normalzeit-Offset der PC-Zeit
    Result:=dtUtcOffsetDev - GetTimeZoneBias(false);
  end else  // wenn kein UTC-Normalzeit-Offset des Ger�ts vorliegt
    Result:=0;  // Default: PC und Ger�t in gleicher UTC-Zeitzone
end;

End.

