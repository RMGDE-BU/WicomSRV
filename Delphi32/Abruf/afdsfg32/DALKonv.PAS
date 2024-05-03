{******************************************************************************}
{* Unit: Routinen zur DSfG-Rohdatenkonvertierung (keine DB-Zugriffe)          *}
{* 19.02.2003 WW  Objekt-Funktionalitäten von TDArchivLogbuchKonv aufgeteilt  *}
{*                auf TDArchivLogbuchKonv (datenbankunabhängige Konvertierung)*}
{*                und TDArchivLogbuchKonvDB (Konvertieren in Tabellen,        *}
{*                ASCII-Export)                                               *}
{* 07.04.2014 WW  Signatur-Lizenz                                             *}
{* 06.10.2015 WW  Beschleunigte Rohdateizugriffe mit TFileOfCharStream        *}
{* 11.07.2016 WW  Anpassungen für zeilenweise abgefragte Daten einer Archiv-  *}
{*                gruppe                                                      *}
{* 13.03.2018 WW  Neuer Signaturverifizierungsstatus bei fehlendem öffent-    *}
{*                lichem Schlüssel (nur Gas-X)                                *}
{* 22.06.2022 WW  Anpassung für Archivdaten aus Standardabfragen              *}
{******************************************************************************}
unit DALKonv;

interface

uses                                
  Classes, GD_Utils, Forms, Windows, SysUtils, Contnrs, ErrConst, DSfGUtil,
  WSysCon, DListen, UnixDT, WStrUtils, WChars, WStream, T_Tools, WComm, O_Signatur;

type

{$H-}
  { Struktur für DSfG-Datendatei und zum Zwischenpuffern }

  TDSfGSatzData = packed record
    DEA          : string [5];   // ab 11.07.2016; für zeilenweises Lesen einer AG
    OrdnungsNr   : integer;
    DatumZeit    : TDateTime;
    UnixDatumZeit: string [8];
    WertAsDouble : Double;
    WertAsString : string [40];  // 20.03.2003, WW (alter Wert 10 ist für Zählerstände zu klein !)
    Status       : string [16];
    CRC          : string [10];
    Zeitzone     : string[1];    // ab 31.07.2006; für SMS-Daten aus MRG 900
    SigVerifyStatus: integer;    // ab 15.03.2012; für digitale Signatur
  end;
{$H+}

  { Liste für SatzData eines Archivkanals }
  
  TSatzDataList = class (TObjectList)
  private
    FKanalIdent: string;  // bei Standardabfragen: Datenelementadresse des Kanals; 22.06.2022, WW
  public
    constructor Create;
    function SatzDataExist (AData: TDSfGSatzData): boolean;
    property KanalIdent: string read FKanalIdent write FKanalIdent;
  end;

  { Liste der SatzData-Listen der Archivkanäle }

  TSatzDataKanaeleList = class (TObjectList)
  public
    constructor Create;
    function GetListIndexByKanalIdent (AKanalIdent: string): integer;
  end;

  { Objekt zu TDSfGSatzData }

  TDSfGSatzDataObj = class(TObject)
    Data: TDSfGSatzData;
  public
    procedure SetData (AData: TDSfGSatzData);
  end;

  { Objekt zur Konvertierung von Archiv- und Logbuchdaten }

  TDArchivLogbuchKonv = class(TObject)
  private
    FSignaturClient: TSignaturClient;
    function Konv_Rohfile (pSatzDataKanaeleList: TSatzDataKanaeleList;
                           isArchiv: boolean; Receiver: char;
                           DEL: string; RohLoeschen: boolean;
                           bSignatur_freigeschaltet: boolean;
                           bPublicKey_vorhanden: boolean;
                           var ValidStatus: integer;
                           var Fehlergruppe_Signatur: integer;
                           var Fehlercode_Signatur: integer): integer;
  protected
    RohfileName: TFilename;
    FS_Rohfile: TFileOfCharStream;  // 06.10.2015, WW
    function OpenRohfile: integer;
    procedure CloseRohfile;
    function ReadDataRohSatz(var SatzData: TDSfGSatzData;
                             DEL: string; bValueAsDouble: boolean;
                             var Negative: Boolean;
                             var ValidStatus: integer;
                             bStatusWithSigStatus: boolean;
                             pKanalKonvList: TKanalKonvList = nil): integer;
  public
    constructor Create;
    function KonvertToFile (RohdatenListe: TDSfGDataList; isArchiv: boolean;
                            Receiver: char; DEL: string; RohLoeschen: boolean;
                            bSignatur_freigeschaltet: boolean;
                            bPublicKey_vorhanden: boolean;
                            var KonvFileName: TFileName;
                            var Fehlergruppe: integer; var Fehlercode: integer): integer;
    property SignaturClient: TSignaturClient read FSignaturClient write
      FSignaturClient;
  end;


function ConvertSatzDataFileToTelegramFile(sSatzDataFile: TFileName;
  sAdr, sDEA: string; Datentyp: integer): TFileName;

function CalcZeitangabenFromSatzDataFile(sSatzDataFile: TFileName; sAdr: string;
  var Zeitangaben: TZeitangaben): boolean;

implementation

uses DateUtils;


{ TDSfGSatzDataObj }

{-------------------------------------------------------}
procedure TDSfGSatzDataObj.SetData(AData: TDSfGSatzData);
{-------------------------------------------------------}
begin
  Data:=AData;
end;


{ TSatzDataList }

{-------------------------------}
constructor TSatzDataList.Create;
{-------------------------------}
begin
  inherited Create;

  FKanalIdent:='';
end;

{-------------------------------------------------------------------}
function TSatzDataList.SatzDataExist (AData: TDSfGSatzData): boolean;
{-------------------------------------------------------------------}
{ Prüft, ob übergebener Datensatz bereits in der Liste enthalten ist;
  Ergebnis: true, wenn enthalten }
var
  LastData: TDSfGSatzData;

begin
  Result:=false;
  // Die Ordnungsnummer des letzten in der Liste enthaltenen Datensatzes wird
  // geprüft:
  if (Count > 0) then begin
    LastData:=TDSfGSatzDataObj (Items [Count-1]).Data;
    if (AData.OrdnungsNr = LastData.OrdnungsNr) then
      Result:=true;
  end;
end;


{ TSatzDataKanaeleList }

{--------------------------------------}
constructor TSatzDataKanaeleList.Create;
{--------------------------------------}
const
  C_MaxArchivKanaele = 21;  // Max. Anzahl Registrierinstanz-Archivkanäle f..z

var
  i: integer;
  pSatzDataList: TSatzDataList;

begin
  inherited Create;

  // SatzData-Listen für die Registrierinstanz-Archivkanäle anlegen (Standard)
  for i:=1 to C_MaxArchivKanaele do begin
    pSatzDataList:=TSatzDataList.Create;
    Add (pSatzDataList);
  end;
end;

{------------------------------------------------------}
function TSatzDataKanaeleList.GetListIndexByKanalIdent (
  AKanalIdent: string): integer;
{------------------------------------------------------}
{ Liefert Index der SatzData-Liste zu KanalIdent;
  Übergabe: KanalIdent
  Ergebnis: Index der Liste }
var
  i: integer;
  pSatzDataList: TSatzDataList;

begin
  Result:=-1;  // Vorbelegung: Liste nicht gefunden

  for i:=0 to Count - 1 do begin
    pSatzDataList:=TSatzDataList (Items [i]);
    if pSatzDataList.FKanalIdent = AKanalIdent then begin  // Liste zu KanalIdent
      Result:=i;
      Break;  // gefunden
    end
    else if pSatzDataList.FKanalIdent = '' then begin  // freie Liste
      pSatzDataList.FKanalIdent:=AKanalIdent;  // KanalIdent setzen
      Result:=i;
      Break;  // gefunden
    end;
  end;

  // Wenn Liste nicht gefunden wurde, eine neue für KanalIdent anlegen:
  if Result < 0 then begin
    pSatzDataList:=TSatzDataList.Create;
    pSatzDataList.FKanalIdent:=AKanalIdent;  // KanalIdent setzen
    Add (pSatzDataList);
    Result:=Count - 1;
  end;
end;


{ TDArchivLogbuchKonv }

{-------------------------------------}
constructor TDArchivLogbuchKonv.Create;
{-------------------------------------}
begin
  inherited Create;
  FSignaturClient:=nil;
end;

{----------------------------------------------------------------------------}
function TDArchivLogbuchKonv.KonvertToFile (RohdatenListe: TDSfGDataList;
                                            isArchiv: boolean;
                                            Receiver: char; DEL: string;
                                            RohLoeschen: boolean;
                                            bSignatur_freigeschaltet: boolean;
                                            bPublicKey_vorhanden: boolean;
                                            var KonvFileName: TFileName;
                                            var Fehlergruppe: integer;
                                            var Fehlercode: integer): integer;
{----------------------------------------------------------------------------}
{ Konvertiert die in DatenListe übergebenen Rohdatenfiles eines Archivkanals/Logbuchs.
  Übergabe: RohdatenListe (Name der Rohdateien)
            Flag 'isArchiv' (true übergeben, wenn es sich um Archivdaten handelt,
                             false bei Logbuchdaten )
            Busadresse
            DE-Adresse
            Flag "RohLoeschen" (wenn true, werden die Rohdateien nach der Konvertierung gelöscht)
            Flag "Signatur freigeschaltet" (false übergeben, wenn Signaturprüfung
                                            nicht per Lizenz freigeschaltet ist)
            Flag "Public Key vorhanden" (false übergeben, wenn kein öffentlicher Schlüssel
                                         für Signatur-Verifizierung vorliegt)
  Rückgabe: Name der Datei mit konvertierten Daten
            Fehlergruppe
            Fehlercode
  Ergebnis: DSFGKONVERR_...-Code }
var
  i: integer;
  Status, StatusBuf: integer;
  ValidStatus, ValidStatusBuf: integer;
  ArchivFehler_AA: boolean;
  ArchivFehler_sonst: boolean;
  LogbuchFehler_AA: boolean;
  LogbuchFehler_sonst: boolean;
  ArchivValid: boolean;
  LogbuchValid: boolean;
  Pref: string;
  Pfad: string;
  pFileName: array[0..255] of char;
  Fehlergruppe_Signatur: integer;
  Fehlercode_Signatur: integer;
  FehlergruppeBuf_Signatur: integer;
  FehlercodeBuf_Signatur: integer;
  pSatzDataKanaeleList: TSatzDataKanaeleList;  // 22.06.2022, WW
  pSatzDataList: TSatzDataList;
  SatzData: TDSfGSatzData;
  k: integer;
  FS_Konv: TFileStream;

begin
  Result:=DSFGKONVERR_OK;
  { Vorbelegungen für Rückgabe Fehlergruppe/Fehlercode: OK }
  Fehlergruppe:=0;
  Fehlercode:=0;

  { Vorbelegungen für Archiv/Logbuch-Konvertierung: keine Fehler, keine Warnungen
    in allen Rohfiles }
  Status:=DSFGKONVERR_OK;
  ValidStatus:=DSFGKONVERR_OK;

  ArchivFehler_AA:=false;     { keine außerplanmäßige Antwort, Archiv }
  ArchivFehler_sonst:=false;  { kein sonstiger Konvertierungsfehler, Archiv }
  LogbuchFehler_AA:=false;     { keine außerplanmäßige Antwort, Logbuch }
  LogbuchFehler_sonst:=false;  { kein sonstiger Konvertierungsfehler, Logbuch }
  ArchivValid:=true;
  LogbuchValid:=true;

  // Vorbelegung Fehlergruppe/-code für Signatur-Verifizierung: OK
  Fehlergruppe_Signatur:=0;
  Fehlercode_Signatur:=0;

  { Prefix für Zieldateiname: }
  if isArchiv then
    Pref:=prefix_DSfG_Ar      { Prefix für DSfG-Archivdaten }
  else
    Pref:=prefix_DSfG_Lb;     { Prefix für DSfG-Logbuchdaten }

  // SatzData-Kanalliste als Zwischenspeicher für Kanaldaten anlegen; 11.07.2016
  pSatzDataKanaeleList:=TSatzDataKanaeleList.Create;
  try
    for i:=0 to RohdatenListe.Count-1 do begin
      RohfileName:=RohdatenListe[i];
      { bei erstem Rohfile: Zieldatei ermitteln und anlegen }
      if i = 0 then begin
        Pfad:=ExtractFilePath (RohfileName);           { Pfad wie Rohdatendatei }
        GetTempFileName (pchar (Pfad), pchar (Pref), 0, pFileName);  { Zieldatei anlegen }
        KonvFileName:=string (pFileName);
      end;

      { Vorbelegung für ValidStatusBuf: keine Formatfehler im Rohfile }
      ValidStatusBuf:=DSFGKONVERR_OK;
      StatusBuf:=Konv_Rohfile (pSatzDataKanaeleList, isArchiv, Receiver, DEL,
                               RohLoeschen, bSignatur_freigeschaltet,
                               bPublicKey_vorhanden, ValidStatusBuf,
                               FehlergruppeBuf_Signatur, FehlercodeBuf_Signatur);
      if isArchiv then begin                   { Archivkanal konvertiert }
        { Fehler: }
        if (StatusBuf <> DSFGKONVERR_OK) AND (StatusBuf <> DSFGKONVERR_NODATA) then begin
          if (StatusBuf >= C_MinCode_DSfGKonvErr_AA) then  { außerplanmäßige Antwort }
            ArchivFehler_AA:=true
          else if (StatusBuf >= C_MinCode_DSfGKonvErr_SigVerify) then begin
            { Signatur-Fehlerstatus hier unberücksichtigt; 19.06.2012, WW }
          end else
            ArchivFehler_sonst:=true;
        end;
        { Gültigkeit: }
        if ValidStatusBuf <> DSFGKONVERR_OK then
          ArchivValid:=false;
      end
      else begin                                { Logbuch konvertiert }
        { Fehler: }
        if (StatusBuf <> DSFGKONVERR_OK) AND (StatusBuf <> DSFGKONVERR_NODATA) then begin
          if (StatusBuf >= C_MinCode_DSfGKonvErr_AA) then  { außerplanmäßige Antwort }
            LogbuchFehler_AA:=true
          else if (StatusBuf >= C_MinCode_DSfGKonvErr_SigVerify) then begin
            { Signatur-Fehlerstatus hier unberücksichtigt; 19.06.2012, WW }
          end else
            LogbuchFehler_sonst:=true;
        end;
        { Gültigkeit: }
        if ValidStatusBuf <> DSFGKONVERR_OK then
          LogbuchValid:=false;
      end;

      { ersten Fehler-Status merken: }
      if (StatusBuf <> DSFGKONVERR_OK) AND (Status = DSFGKONVERR_OK) then
        Status:=StatusBuf;
      { ersten Invalid-Status merken: }
      if (ValidStatusBuf <> DSFGKONVERR_OK) AND (ValidStatus = DSFGKONVERR_OK) then
        ValidStatus:=ValidStatusBuf;
      { ersten Signatur-Fehler merken: }
      if not FehlerGruppeCode_OK (FehlergruppeBuf_Signatur, FehlercodeBuf_Signatur) then begin
        if FehlerGruppeCode_OK (Fehlergruppe_Signatur, Fehlercode_Signatur) then begin
          Fehlergruppe_Signatur:=FehlergruppeBuf_Signatur;
          Fehlercode_Signatur:=FehlercodeBuf_Signatur;
        end;
      end;
    end; { for i:=0 }

    // In SatzData-Kanalliste zwischengespeicherte Kanaldaten in Zieldatei schreiben; 11.07.2016
    for k:=0 to pSatzDataKanaeleList.Count - 1 do begin
      pSatzDataList:=TSatzDataList(pSatzDataKanaeleList [k]);

      if (length (KonvFileName) > 0) AND (pSatzDataList.Count > 0) then begin
        FS_Konv:=TFileStream.Create (KonvFileName, fmOpenReadWrite OR fmShareDenyWrite);
        try
          FS_Konv.Seek (0, soFromEnd);
          for i:=0 to pSatzDataList.Count - 1 do begin
            SatzData:=TDSfGSatzDataObj (pSatzDataList [i]).Data;
            FS_Konv.Write (SatzData, SizeOf (SatzData));
          end;
        finally
          FS_Konv.Free;
        end;
      end;

      // Zwischengespeicherte Kanaldaten aus Liste löschen
      pSatzDataList.Clear;
    end;  // for c
  finally
    pSatzDataKanaeleList.Free;
  end;

  { Konvertierungsfehler-Rückgaben:
    -> sonstiger Konvertierungsfehler geht vor außerplanmäßiger Antwort geht vor
       ungültigen Daten geht vor Signaturverifizierungs-Ergebnis }
  if isArchiv then begin
    if ArchivFehler_sonst then begin
      Fehlergruppe:=ST_KONVERROR;
      Fehlercode:=SKERR_ARCHIVKONV;
    end
    else if ArchivFehler_AA then begin
      Fehlergruppe:=ST_KONVERROR;
      Fehlercode:=SKERR_AUSSERPLANMAESSIGEANTW_AR;
    end
    else if not ArchivValid then begin
      Fehlergruppe:=ST_DATACHECK;
      Fehlercode:=DCH_ARINVALID;
    end;
  end
  else begin
    if LogbuchFehler_sonst then begin
      Fehlergruppe:=ST_KONVERROR;
      Fehlercode:=SKERR_LOGBKONV;
    end
    else if LogbuchFehler_AA then begin
      Fehlergruppe:=ST_KONVERROR;
      Fehlercode:=SKERR_AUSSERPLANMAESSIGEANTW_LB;
    end
    else if not LogbuchValid then begin
      Fehlergruppe:=ST_DATACHECK;
      Fehlercode:=DCH_LBINVALID;
    end;
  end;

  if FehlerGruppeCode_OK (Fehlergruppe, Fehlercode) then begin  // 19.06.2012, WW
    if not FehlerGruppeCode_OK (Fehlergruppe_Signatur, Fehlercode_Signatur) then begin
      { Signatur-Fehler liegt vor }
      Fehlergruppe:=Fehlergruppe_Signatur;
      Fehlercode:=Fehlercode_Signatur;
    end;
  end;

  { Ergebnis zuweisen, wenn Fehler oder Warnung aufgetreten: }
  if Status <> DSFGKONVERR_OK then
    Result:=Status
  else if ValidStatus <> DSFGKONVERR_OK then
    Result:=ValidStatus;
end;

{------------------------------------------------------------------------------------}
function TDArchivLogbuchKonv.Konv_Rohfile (pSatzDataKanaeleList: TSatzDataKanaeleList;
                                           isArchiv: boolean;
                                           Receiver: char; DEL: string;
                                           RohLoeschen: boolean;
                                           bSignatur_freigeschaltet: boolean;
                                           bPublicKey_vorhanden: boolean;
                                           var ValidStatus: integer;
                                           var Fehlergruppe_Signatur: integer;
                                           var Fehlercode_Signatur: integer): integer;
{------------------------------------------------------------------------------------}
{ konvertiert Daten aus einem Rohdatenfile (ein Archivkanal oder ein Logbuch) in
  SatzData-Kanalliste;
  Übergabe: SatzData-Kanalliste
            Flag 'isArchiv' (true übergeben, wenn es sich um Archivdaten handelt,
                             false bei Logbuchdaten )
            Busadresse
            Datenelement-Adresse aus Abfrage
            Flag "RohLoeschen" (wenn true, wird die Rohdatei nach der Konvertierung gelöscht)
            Flag "Signatur freigeschaltet" (false übergeben, wenn Signaturprüfung
                                            nicht per Lizenz freigeschaltet ist)
            Flag "Public Key vorhanden" (false übergeben, wenn kein öffentlicher Schlüssel
                                         für Signatur-Verifizierung vorliegt)
  Rückgabe: ValidStatus (Daten-Gültigkeitsstatus)
            Fehlergruppe und -code für Signatur-Verifizierung
  Ergebnis: Konvertierungs-Status }
var
  DataCount: integer;
  SatzData: TDSfGSatzData;
  i: integer;
  negative: boolean;
  Status: integer;
  ValidStatusBuf: integer;
  FSize_Roh: integer;
  dummy: boolean;
  sPTB: string;
  iTID: integer;
  sData: string;
  DSfGPTBData: TDSfGPTBData;
  SigVerifyStatus: integer;
  SatzDataObj: TDSfGSatzDataObj;
  k: integer;
  bDoVerify: boolean;
  pSatzDataList: TSatzDataList;

begin
  // Vorbelegung Fehlergruppe/-code für Signatur-Verifizierung: OK
  Fehlergruppe_Signatur:=0;
  Fehlercode_Signatur:=0;

  Result:=OpenRohfile;                                  { Rohdatenfile öffnen }
  try
    if Result <> DSFGKONVERR_OK then exit;
    { Validierung des HDCL im DSfG-Telegramm: }
    Result:=CheckDSfGHeader (FS_Rohfile, Receiver, debc_OrdNr_Zeit, DataCount, dummy,
                             sPTB, iTID);
    if Result = DSFGKONVERR_OK then begin            { jetzt wird konvertiert }
      try
        { PTB-Feld des HDCL mit Inhalt: Signatur verifizieren; 17.11.2011, WW }
        if length (sPTB) > 0 then begin
          if Assigned (FSignaturClient) then begin  // Signaturclient ist aktiv
            bDoVerify:=true;
            SigVerifyStatus:=C_SigVerifyState_NotVerified;
{$IFDEF GAS-X}
            // Signatur vorhanden, öffentlicher Schlüssel fehlt (nur für Gas-X):
            if not bPublicKey_vorhanden then begin
              SigVerifyStatus:=C_SigVerifyState_PubKeyMissing;  // 13.03.2018, WW
              bDoVerify:=false;  // dann auch nicht verifizieren...
            end;
{$ENDIF}
            if bDoVerify then begin
              sData:=ReadFile_DSfGDataPart (FS_Rohfile);  // Datenteil aus Rohdaten extrahieren
              GetDSfGPTBDataFromDCL (sPTB, DSfGPTBData);
              // Signatur verifizieren:
              SigVerifyStatus:=FSignaturClient.Verify (iTID, DSfGPTBData.Kenner, sData,
                DSfGPTBData.Signatur_r, DSfGPTBData.Signatur_s, RohfileName,
                Receiver, DEL);
              // Fehlergruppe/-code aus Signatur-Verifizierung:
              FSignaturClient.GetFehlerGruppeCode (Fehlergruppe_Signatur,
                                                   Fehlercode_Signatur);
            end;
          end
          else begin  // Signatur wurde nicht verifiziert
            SigVerifyStatus:=C_SigVerifyState_NotVerified;
{$IFDEF GAS-X}
            // Signatur vorhanden, öffentlicher Schlüssel fehlt (nur für Gas-X):
            if not bPublicKey_vorhanden then
              SigVerifyStatus:=C_SigVerifyState_PubKeyMissing;  // 13.03.2018, WW
{$ENDIF}
            if bSignatur_freigeschaltet then begin
              Fehlergruppe_Signatur:=SYS_SIGNSRV_ERROR;
              Fehlercode_Signatur:=SIGNSRVERR_NOTVERIFIED;
            end
            else begin  // Signatur nicht freigeschaltet; 07.04.2014, WW
              Fehlergruppe_Signatur:=SYS_SIGNSRV_ERROR;
              Fehlercode_Signatur:=SIGNSRVERR_LICENCE;
            end;
          end;
        end else
          SigVerifyStatus:=C_SigVerifyState_None;

        FSize_Roh:=FS_Rohfile.Size;  { Rohfilegröße }
        for i:=0 to DataCount-1 do begin
          Application.ProcessMessages;
          if FS_Rohfile.Position >= FSize_Roh then  // zusätzlich auf Rohfileende prüfen; 14.03.2007 WW
            Break;

          Status:=ReadDataRohSatz (SatzData, DEL, false, negative, ValidStatusBuf, false);
          { Signatur-Verifizierungsstatus in SatzData-Struktur eintragen: }
          SatzData.SigVerifyStatus:=SigVerifyStatus;  // 15.03.2012, WW
          if (Status <> DSFGKONVERR_OK) OR (ValidStatusBuf <> DSFGKONVERR_OK) then begin
            if Status <> DSFGKONVERR_OK then
              Result:=Status;
            if ValidStatusBuf <> DSFGKONVERR_OK then
              ValidStatus:=ValidStatusBuf;
            Continue;  // ungültigen Datensatz weglassen, nicht weiterverarbeiten
          end;

          // Kanal-/Logbuch-Daten zwischenspeichern in SatzData-Kanalliste; 11.07.2016
          if isArchiv then begin
            k:=-1;  // Default: Undefinierter Kanal
            if length (SatzData.DEA) > 0 then begin
              if SatzData.DEA [1] = 'c' then begin // Archivkanal einer Registrierinstanz
                // 4. Stelle der Datenelement-Adresse -> Nr. des Archivkanals (f = 1, g = 2 usw.); 22.06.2022, WW
                if length (SatzData.DEA) >= 4 then
                  k:=Ord (SatzData.DEA [4]) - Ord ('f') + 1;
              end else  // Archivkanal eines anderen Instanztyps (z.B. aus Standardabfrage Umwerter)
                k:=pSatzDataKanaeleList.GetListIndexByKanalIdent (SatzData.DEA) + 1;  // 22.06.2022, WW
            end;
          end else
            k:=1;  // Logbuch-Daten immer in erster Kanalliste zwischenspeichern
                   // (Logbücher können nicht zeilenweise abgefragt werden)

          { ab 14.03.2007 keine "überlappenden" Folgetelegramm-Abrufe mehr, daher
            ALLE Rohsätze konvertieren
            ab 11.07.2016 sind "überlappende" Folgetelegramme (wieder) möglich,
            wenn Archive zeilenweise ausgelesen werden -> prüfen, ob in Kanalliste
            bereits enthalten ist }

          if (k > 0) AND (k <= pSatzDataKanaeleList.Count) then begin  // 22.06.2022, WW
            pSatzDataList:=TSatzDataList (pSatzDataKanaeleList [k - 1]);
            if not pSatzDataList.SatzDataExist (SatzData) then begin
              // Wenn noch nicht in Kanalliste enthalten, eintragen:
              SatzDataObj:=TDSfGSatzDataObj.Create;
              SatzDataObj.SetData(SatzData);
              pSatzDataList.Add (SatzDataObj);
            end;
          end;
        end;  // for i

        { Signatur-Verifizierungsstatus, welche "Fehler" darstellen, in
          Konvertierungs-Status übernehmen: 19.06.2012, WW }
        if Result = DSFGKONVERR_OK then begin  { Konvertierungsfehler nicht überschreiben }
          case SigVerifyStatus of
            C_SigVerifyState_Invalid:
              Result:=DSFGKONVERR_SIGVERIFY_INVALID;

            C_SigVerifyState_NotVerified,
            C_SigVerifyState_PubKeyMissing:  // 13.03.3018, WW
              Result:=DSFGKONVERR_SIGVERIFY_NOTVERIFIED;
          end;
        end;
      except
        Result:=DSFGKONVERR_DATAKONV_INTERN;  // 05.04.2007 WW
        exit;
      end;
    end;  { if Result = DSFGKONVERR_OK }
  finally
    CloseRohfile;                                    { Rohdatenfile schließen }
  end;

  { Rohfile löschen: }
  if RohLoeschen AND (Result <> DSFGKONVERR_HEADER) AND (Result <> DSFGKONVERR_DATA) then
    DeleteFile (RohfileName);
end;

{------------------------------------------------}
function TDArchivLogbuchKonv.OpenRohfile: integer;
{------------------------------------------------}
begin
  if FileExists (RohfileName) then begin
    try
      FS_Rohfile:=TFileOfCharStream.Create (RohfileName, fmOpenRead OR fmShareDenyWrite);
      Result:=DSFGKONVERR_OK;
    except
      Result:=DSFGKONVERR_FILEACCESS;
    end;
  end else
    Result:=DSFGKONVERR_FILENOTFOUND;  // 05.04.2007, WW
end;

{-----------------------------------------}
procedure TDArchivLogbuchKonv.CloseRohfile;
{-----------------------------------------}
begin
  FreeAndNil (FS_Rohfile);  // 14.03.2007, WW
end;

{-----------------------------------------------------------------------------------}
function TDArchivLogbuchKonv.ReadDataRohSatz(var SatzData: TDSfGSatzData;
                                             DEL: string;
                                             bValueAsDouble: boolean;
                                             var Negative: Boolean;
                                             var ValidStatus: integer;
                                             bStatusWithSigStatus: boolean;
                                             pKanalKonvList: TKanalKonvList = nil): integer;
{-----------------------------------------------------------------------------------}
{ liest einen Rohdatensatz aus Rohdatenfile }
var
  S: string;
  RohRec: TDSfGRohRec;
  Code: integer;
  Erg: integer;
  bletzt_ascii: boolean;

begin
  Result := DSFGKONVERR_OK;
  FillChar (SatzData, SizeOf (SatzData), 0);  // Vorbelegung SatzData
  negative:=false;  // Vorbelegung: Wert ist nicht negativ
  ValidStatus:=DSFGKONVERR_OK;  // Vorbelegung: Datensatz gültig

  read_ascii(FS_Rohfile, S, bletzt_ascii);          { 1 Rohdatenelement lesen }
  split_ascii(S, RohRec);  { in Rohdaten-Struktur konvertieren }

  SatzData.DEA:=RohRec.Adresse;  // 11.07.2016, WW
  
  { Gültigkeitsprüfung: Archiv-/Logbuch-Rohsatz syntaktisch richtig ? }
  Erg:=CheckDSfGArchivLogbuchRohsatz (RohRec, bValueAsDouble, bStatusWithSigStatus,
                                      pKanalKonvList);
  if Erg <> DSFGKONVERR_OK then begin
    ValidStatus:=Erg;  // Rohdaten ungültig
    exit;
  end;

  { Gültigkeitsprüfung: richtige Zuordnung der Daten zu DE-Adresse des Archivkanals
    bzw. Logbuchs: 14.03.2007, WW }
  if (DEL <> '') then begin
    if (DEL [1] = 'c') then begin  // Datenelement-Adresse einer Registrier-Instanz
      if length (DEL) = 3 then begin  // bei zeilenweiser Abfrage einer ganzen AG; 11.07.2016, WW
        if (DEL <> Copy (RohRec.Adresse, 1, 3)) then begin  // Kanaldaten gehören nicht zur abgefragten AG
          Result:=DSFGKONVERR_DATA;  // Daten falsch zugeordnet
          exit;
        end;
      end
      else if (DEL <> RohRec.Adresse) then begin
        Result:=DSFGKONVERR_DATA;  // Daten falsch zugeordnet
        exit;
      end;
    end
    else begin
      // 22.06.2022, WW
      if (DEL [1] <> Copy (RohRec.Adresse, 1, 1)) then begin  // Archivdaten gehören nicht zum abgefragten Instanztyp
        Result:=DSFGKONVERR_DATA;  // Daten falsch zugeordnet
        exit;
      end;
    end;
  end;

  { TDSfGRohRec-Struktur in TDSfGSatzData-Struktur wandeln:
    -> Rohwerte wurden zuvor bereits auf Gültigkeit (und damit fehlerfreie
       Wandelbarkeit) geprüft }
  SatzData.UnixDatumZeit:=RohRec.UTime;
  UnixTimeStrToDateTime (RohRec.UTime, SatzData.DatumZeit);  { UNIX-Zeitstring -> DateTime-Format }

  try
    Val(RohRec.OrdNr, SatzData.OrdnungsNr, Code);
  except
  end;

  if length (RohRec.Wert) > 0 then
    negative:=RohRec.Wert [1] = '-';
  SatzData.WertAsString := RohRec.Wert;
  try
    Val(RohRec.Wert, SatzData.WertAsDouble, Code);
    if Code <> 0 then
      SatzData.WertAsDouble := -1;  // Wert, wenn nicht wandelbar; 12.07.2016 WW
  except
  end;

  SatzData.Status := RohRec.Status;
  Satzdata.CRC:=RohRec.CRC;
  SatzData.Zeitzone:='';  // unbenutzt; keine Zeitzonen-Information im DSfG-Protokoll
  SatzData.SigVerifyStatus:=0;  // unbenutzt; keine Information über Signatur-Verifizierung im DSfG-Protokoll
end;


{------------------------------------------------------------------------------}

{------------------------------------------------------------------}
function ConvertSatzDataFileToTelegramFile(sSatzDataFile: TFileName;
  sAdr, sDEA: string; Datentyp: integer): TFileName;
{------------------------------------------------------------------}
{ erzeugt aus Datei mit TDSfGSatzData-Struktur eine Datei mit DSfG-Protokollstruktur;
  Übergabe: Name der Datei mit TDSfGSatzData-Struktur
            Busadresse
            Datenelement-Adresse
            Datentyp (C_IsArchive, C_IsLogbuecher je nach enthaltenen Quelldaten)
  Ergebnis: Name der Datei mit DSfG-Protokollstruktur (leer, wenn Fehler aufgetreten) }
var
  pSatzDataStream  : TFileOfRecStream;
  FSize: integer;
  SatzData: TDSfGSatzData;
  sTyp  : string;
  sResultLine : string;
  iDECount    : integer;
  sWert,sUxDT,sONr,sStatus,sCRC : string;
begin
  Result := '';
  sResultLine := '';  // Resultierendes Telegramm
  iDECount := 0;  // Anzahl der Datenelemente

  // Prüfung: Busadresse
  if (Length(sAdr) <> 1) then Exit;

  // Prüfung: Datentyp
  if (Datentyp = C_IsArchive) OR (Datentyp = C_IsLogbuecher) then begin
    sTyp := 'O';
  end
  else if (Datentyp = C_IsDatenelemente) then begin
    sTyp := 'M';
  end;

  if (FileExists(sSatzDataFile)) then
  try
    pSatzDataStream := TFileOfRecStream.Create(sSatzDataFile, fmOpenRead OR fmShareDenyWrite,
                                               SizeOf (TDSfGSatzData));
    try
      // Schleife durch die SatzData-Quelldatei
      FSize:=pSatzDataStream.Size;
      while pSatzDataStream.Position < FSize do begin
        Application.ProcessMessages;
        pSatzDataStream.ReadRec (SatzData);

        sWert:=SatzData.WertAsString;
        sUxDT:=SatzData.UnixDatumZeit;
        sONr:=IntToStr (SatzData.OrdnungsNr);
        sStatus:=SatzData.Status;
        sCRC:=SatzData.CRC;

        if (iDECount > 0) then sResultLine := sResultLine + gs;
        if (sTyp = 'M') then
          sResultLine := sResultLine + sDea + us + sWert
        else
          sResultLine := sResultLine + sDEA + us + sWert + us +
            sUxDT + us + sONr + us + sStatus + us + sCRC;
        Inc(iDECount);
      end;

      // Telegramm vervollständigen
      sResultLine := stx + '_' + us  // Header-Teil
        + '255' + us + '1' + us + '1' + us + '1' + us
        + sAdr + us                    { DNO }
        + 'R' + us                     { NTY }
        + 'N' + us                     { DFO }
        + sTyp + us                    { DEB }
        + IntToStr(iDECount) + us      { ZAE }
        + sResultLine + fs + etx; { Daten und Abschluss }

    finally
      pSatzDataStream.Free;
    end;

    // Ergebnis in Datei schreiben
    Result := CreateTempRohFile(ExtractFilePath(sSatzDataFile), 'TMP');
    try
      WriteRohFile(Result, sResultLine);
    except
      Result := '';
    end
  except
  // Result ist bereits ''
  end;
end;

{------------------------------------------------------------------------------}
function CalcZeitangabenFromSatzDataFile(sSatzDataFile: TFileName; sAdr: string;
  var Zeitangaben: TZeitangaben): boolean;
{------------------------------------------------------------------------------}
{ erzeugt aus Datei mit TDSfGSatzData-Struktur einen Zeitangaben-Record;
  Übergabe: Name der Datei mit TDSfGSatzData-Struktur
            Busadresse
            Datenelement-Adresse
            Datentyp (C_IsArchive, C_IsLogbuecher je nach enthaltenen Quelldaten)
  Rückgabe: Zeitangaben-Record }
var
  pSatzDataStream  : TFileOfRecStream;
  i, iSize         : integer;
  SatzData         : TDSfGSatzData;
  pSlZeitangaben   : TStringList;
  h, m, s, ms: word;
begin
  Result := False;
  try
    // Prüfung: Muss / Kann TZeitangaben ermittelt werden ?
    if (Zeitangaben.EAdr = NUL) and (Length(sAdr) = 1) and
      (FileExists(sSatzDataFile)) then
    begin
      pSlZeitangaben := TStringList.Create;
      try
        pSatzDataStream := TFileOfRecStream.Create(
          sSatzDataFile, fmOpenRead OR fmShareDenyWrite, SizeOf (TDSfGSatzData));
        try
          // Schleife durch die SatzData-Quelldatei
          iSize := pSatzDataStream.Size;
          while (pSatzDataStream.Position < iSize) do begin
            pSatzDataStream.ReadRec(SatzData);
            pSlZeitangaben.AddObject(SatzData.Zeitzone,
              TObject(StrToInt('$' + SatzData.UnixDatumZeit)));
          end;
        finally
          pSatzDataStream.Free;
        end;

        // Zeiten auswerten, um Zeitzoneninfo zu erhalten
        if (pSlZeitangaben.Count > 0) then begin                //!! WZSZ-Algorithnmus kapseln
          pSlZeitangaben.CustomSort(StringListObjectSort);  // Nach Zeiten sortieren
          if (pSlZeitangaben[pSlZeitangaben.Count-1] = CMEZ) or
            (pSlZeitangaben[pSlZeitangaben.Count-1] = CMESZ) then
          begin
            Zeitangaben.EAdr := sAdr[1];
            Zeitangaben.DatumZeit := UnixToDateTime(
              Integer(pSlZeitangaben.Objects[pSlZeitangaben.Count-1]));
            Zeitangaben.Zeitzone := UpperCase(
              pSlZeitangaben[pSlZeitangaben.Count-1]);
            Zeitangaben.LetztVerstZZ := 0;
            // Schleife abwärts, um Wechsel zu prüfen
            if (Zeitangaben.Zeitzone = CMEZ) or (Zeitangaben.Zeitzone = CMESZ)
            then begin
              for i := pSlZeitangaben.Count-2 downto 0 do begin
                if (UpperCase(pSlZeitangaben[i]) <> Zeitangaben.Zeitzone) then
                begin
                  if (UpperCase(pSlZeitangaben[i]) = CMEZ) or
                    (UpperCase(pSlZeitangaben[i]) = CMESZ) then
                  begin
                    DecodeTime(UnixToDateTime(
                      Integer(pSlZeitangaben.Objects[i])), h, m, s, ms);
                    Zeitangaben.LetztVerstZZ :=
                      Trunc(UnixToDateTime(Integer(pSlZeitangaben.Objects[i]))) +
                      EncodeTime(h, 0, 0, 0) + EncodeTime(1, 0, 0, 0);
                  end;
                  Break;
                end;
              end;
            end;
            Zeitangaben.vom_PC := False;
            Result:=true;
          end;
        end;
      finally
        pSlZeitangaben.Free;
      end;
    end;
  except
  // Result ist bereits False
  end;
end;

end.

