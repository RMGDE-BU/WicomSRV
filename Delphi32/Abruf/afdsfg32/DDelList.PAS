{******************************************************************************}
{* Unit: Datenelementeliste                                                   *}
{* 27.12.1999  WW                                                             *}
{* 21.11.2003  GD  Erweiterung für Konvertierung aus XML-Strings              *}
{* 24.02.2011  GD  XML-Ersatzzeichen in Parametertexten                       *}
{* 24.03.2011  GD  Bugfix: XML-Ersatzzeichen in Parametertexten               *}
{* 04.01.2013  WW  mit Verifizieren vorhandener Signatur in DSfG-Rohdaten     *}
{* 06.10.2015 WW  Beschleunigte Rohdateizugriffe mit TFileOfCharStream        *}
{* 13.03.2018 WW  Neuer Signaturverifizierungsstatus bei fehlendem öffent-    *}
{*                lichem Schlüssel (nur Gas-X)                                *}
{* 22.06.2022 WW  Optionales Überschreiben von Datenelementwerten             *}
{******************************************************************************}
unit DDELList;

interface

uses
  Classes, Forms, SysUtils, WStrUtils, ErrConst, GD_Utils, WChars, DSfGUtil,
  WSysCon, DListen, WComm, DSfGXmlDecodeObj, WXmlConst, O_Signatur, DD_Allg,
  T_GerZustand, WStream;

type
  { Objekt für TDELList }

  TDELListObj = class(TObject)
    EAdr: string [1];
    DEL: string [10];
    Wert: string [40];               // 06.03.2002 WW, erweitert von 20 auf 40
    SigVerifyStatus: integer;        // ab 04.01.2013; für digitale Signatur
  public
    procedure SetData (AEAdr: string; ADEL: string; AWert: string;
                       ASigVerifyStatus: integer = C_SigVerifyState_None);
  end;

  { Datenelementeliste }

  TDELList = class(TList)
  private
    RohfileName: TFilename;
    FS_Rohfile: TFileOfCharStream;  // 06.10.2015, WW
    FSignaturClient: TSignaturClient;  // 04.01.2013, WW
    FOverwriteValue: boolean;  // 22.06.2022, WW
    function OpenRohfile: integer;
    procedure CloseRohfile;
    function GetDELListObj (EAdr: string; DEL: string): TDELListObj;

    function read_ascii_Str (Rohstring: string; var StringPos: integer): string;
    function CheckDSfGHeader_Str (Rohstring: string; var StringPos: integer;
      Receiver: char; var ZAE: integer): integer;
    procedure DELDataLoop_Str (Rohstring: string; StringPos: integer;
      Receiver: char; DataCount: integer);

    function Konv_Rohfile (Receiver: char; RohLoeschen: boolean;
                           bPublicKey_vorhanden: boolean;
                           var Fehlergruppe_Signatur: integer;
                           var Fehlercode_Signatur: integer): integer;
    procedure DELDataLoop_File (Receiver: char; DataCount: integer;
                                SigVerifyStatus: integer);

    function BuildGeraeteZustand (Receiver: char; sVirtualDEA: string;
      var sGerZustandHex: string): boolean;
    function BuildGeraeteZustand_MRG900 (Receiver: char; sVirtualDEA: string;
      var iGerZustand: Int64): boolean;
  public
    constructor Create (bOverwriteValue: boolean = true);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    function GetWert (EAdr: string; DEL: string; var Wert: string): boolean;
    procedure Eintragen (EAdr: string; DEL: string; Wert: string;
                         SigVerifyStatus: integer = C_SigVerifyState_None);
    function LoadFromRohstring (
      Rohstring: string; Receiver: char): integer;
    function LoadFromRohfileList (RohdatenListe: TDSfGDataList; Receiver: char;
      RohLoeschen: boolean; bPublicKey_vorhanden: boolean;
      var Fehlergruppe: integer; var Fehlercode: integer): integer;
    function LoadFromRohstringXML (
      Rohstring: string; Receiver: char): integer;
    function LoadFromRohfileListXML(RohdatenListe: TDSfGDataList;
      Receiver: char; RohLoeschen: boolean): boolean;
    function LoadGeraeteZustand (Receiver: char; sVirtualDEA: string): integer;
    property SignaturClient: TSignaturClient read FSignaturClient write FSignaturClient;  // 04.01.2013, WW
  end;

implementation

{--------------------------------------------------------}
function EAdr_DELCompare (Item1, Item2: Pointer): Integer;
{--------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TDELList-Objekten nach:
  1. der Busadresse
  2. der Datenelementadresse }
begin
  Result := CompareStr (TDELListObj (Item1).EAdr, TDELListObj (Item2).EAdr);               { 1. Sortierkriterium: EAdr }
  if Result = 0 then
    Result := CompareStr (TDELListObj (Item1).DEL, TDELListObj (Item2).DEL);               { 2. Sortierkriterium: DEL }
end;

{ TDELListObj }

{--------------------------------------------------------------------------------}
procedure TDELListObj.SetData (AEAdr: string; ADEL: string; AWert: string;
                               ASigVerifyStatus: integer = C_SigVerifyState_None);
{--------------------------------------------------------------------------------}
begin
  EAdr:=AEAdr;
  DEL:=ADEL;
  Wert:= DecodeXml(AWert);  // 24.03.2011
  SigVerifyStatus:=ASigVerifyStatus;  // 04.01.2013, WW
end;


{ TDELList }

{------------------------------------------------------------}
constructor TDELList.Create (bOverwriteValue: boolean = true);
{------------------------------------------------------------}
{ Übergabe: Datenelementwert überschreiben erlaubt (Standard) }
begin
  inherited Create;
  FSignaturClient:=nil;
  FOverwriteValue:=bOverwriteValue;  // 22.06.2022, WW
end;

{--------------------------}
Destructor TDELList.Destroy;
{--------------------------}
begin
  Clear;

  inherited Destroy;
end;

{--------------------------}
procedure TDELList.Clear;
{--------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if (Assigned(Items[i])) then TObject(Items[i]).Free;

  inherited;
end;

{--------------------------}
procedure TDELList.Delete(iIndex: integer);
{--------------------------}
begin
  if (Assigned(Items[iIndex])) then TObject(Items[iIndex]).Free;

  inherited;
end;

{-----------------------------------------------------------------------}
Function TDELList.GetDELListObj (EAdr: string; DEL: string): TDELListObj;
{-----------------------------------------------------------------------}
{ Listenobjekt mit EAdr und DEL ermitteln;
  Ergebnis: nil, wenn nicht gefunden, sonst Listenobjekt }
var
  i: integer;
begin
  Result:=nil;
  for i:=0 to Count - 1 do begin
    if (EAdr = TDELListObj (Items[i]).EAdr) AND (DEL = TDELListObj (Items[i]).DEL) then begin
      Result:=TDELListObj (Items[i]);
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
function TDELList.GetWert (EAdr: string; DEL: string; var Wert: string): boolean;
{-------------------------------------------------------------------------------}
{ Listenobjekt mit EAdr und DEL ermitteln und Datenelement-Wert zurückgeben;
  Rückgabe: Datenelement-Wert
  Ergebnis: true, wenn Listenobjekt gefunden }
var
  DELListObj: TDELListObj;
begin
  DELListObj:=GetDELListObj (EAdr, DEL);
  if DELListObj <> nil then begin
    Wert:= DecodeXml(DELListObj.Wert); // 24.02.2011
    Result:=true;
  end
  else begin
    Wert:='';
    Result:=false;
  end;
end;

{------------------------------------------------------------------------------}
procedure TDELList.Eintragen (EAdr: string; DEL: string; Wert: string;
                              SigVerifyStatus: integer = C_SigVerifyState_None);
{------------------------------------------------------------------------------}
{ Daten in Liste eintragen;
  -> neuer Eintrag, wenn noch keiner für EAdr und DEL vorhanden
  -> Wert und Signatur-Verifizierungsstatus updaten, wenn Eintrag für EAdr und
     DEL bereits vorhanden }
var
  DELListObj: TDELListObj;
begin
  DELListObj:=GetDELListObj (EAdr, DEL);
  if DELListObj <> nil then begin
    if (FOverwriteValue) then begin  // 22.06.2022, WW
      DELListObj.Wert:= DecodeXml(Wert); { Wert updaten in vorhandenem Listenobjekt } // 24.03.2011
      DELListObj.SigVerifyStatus:=SigVerifyStatus;  // 04.01.2013, WW
    end;
  end
  else begin
    DELListObj:=TDELListObj.Create;
    DELListObj.SetData (EAdr, DEL, Wert, SigVerifyStatus);  // 04.01.2013, WW
    Add (DELListObj);                   { in Datenelementeliste neu eintragen }
  end;
end;

{-------------------------------------------------------------------------------}
function TDELList.LoadFromRohstring (Rohstring: string; Receiver: char): integer;
{-------------------------------------------------------------------------------}
{ Konvertiert die im Rohstring übergebenen Datenelemente in die Datenelementeliste.
  Übergabe: Rohstring
            Receiver (EAdr der Instanz, von der die Datenelemente angefordert wurden)
  Ergebnis: Konvertierungsstatus }
var
  DataCount: integer;
  StringPos: integer;

begin
  { Validierung des HDCL im DSfG-Telegramm: }
  Result:=CheckDSfGHeader_Str (Rohstring, StringPos, Receiver, DataCount);
  if Result = DSFGKONVERR_OK then                    { jetzt wird konvertiert }
    DELDataLoop_Str (Rohstring, StringPos, Receiver, DataCount);
  Sort (EAdr_DELCompare);                 { Liste nach EAdr und DEL sortieren }
end;

{-----------------------------------------------------------------------------------}
function TDELList.read_ascii_Str (Rohstring: string; var StringPos: integer): string;
{-----------------------------------------------------------------------------------}
{ liest aus Rohstring ein Datenelement zwischen Gruppen- und Fileseparator
  und hängt am Anfang und Ende des Datenelement-Strings einen Unitseparator an }
var
  s: string;
  zeich: char;
begin
  S:='';
  while StringPos <= length(Rohstring) do begin
    zeich:=Rohstring[StringPos];
    inc(StringPos);
    if (zeich = FS) or (zeich = GS) then
      Break
    else
      S:=S + zeich;
  end;
  Result:=US + S + US;
end;

{---------------------------------------------------------------------------------------------------------------------------}
function TDELList.CheckDSfGHeader_Str (Rohstring: string; var StringPos: integer; Receiver: char; var ZAE: integer): integer;
{---------------------------------------------------------------------------------------------------------------------------}
{ Verifizierung des DSfG-Telegramm HDCL's aus String }
var
  Code: Integer;
  S: string;
  DID: integer;
  Ausserplanmaessige_Antwort: boolean;

begin
  StringPos:=1;
  S := read_hdcl_fromString(Rohstring, StringPos);         { alles bis zum ersten US (keine Auswertung) }

  { HDCL auswerten: }
  S := read_hdcl_fromString(Rohstring, StringPos);                       { DID }
  val(S, DID, Code);
  if Code <> 0 then begin
    Result := DSFGKONVERR_HEADER;
    exit;
  end;
  if DID < C_DID_DataSimple then begin
    Result := DSFGKONVERR_HEADER;
    exit;
  end;

  S := read_hdcl_fromString(Rohstring, StringPos);                      { TID }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { BLO }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { BNR }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { DNO }
  if S <> Receiver then begin                      { Überprüfen des Absenders }
    Result := DSFGKONVERR_DATA;
    exit;
  end;

  S := read_hdcl_fromString(Rohstring, StringPos);                      { NTY }
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

  S := read_hdcl_fromString(Rohstring, StringPos);                      { DFO }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { DEB }
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

  S := read_hdcl_fromString(Rohstring, StringPos);                      { ZAE }
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
    S := read_hdcl_fromString(Rohstring, StringPos);                    { PAS }
  if (DID AND C_DTY) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { DTY }
  if (DID AND C_ABS) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { ABS }
  if (DID AND C_EMF) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { EMF }
  if (DID AND C_TDA) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { TDA }
  if (DID AND C_TTI) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { TTI }
  if (DID AND C_PTB) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { PTB }   

  { außerplanmäßige Antwort: Datenteil lesen und prüfen, 20.07.2004, WW }
  if Ausserplanmaessige_Antwort then begin
    S := read_hdcl_fromString(Rohstring, StringPos);
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

{------------------------------------------------------------------------}
procedure TDELList.DELDataLoop_Str (Rohstring: string; StringPos: integer;
                                    Receiver: char; DataCount: integer);
{------------------------------------------------------------------------}
{ Schleife durch Datenelemente im DSfG-Telegramm (String). Listenobjekt wird in Datenelementeliste eingetragen. }
var
  i: integer;
  S: string;
  RohRec: TDSfGRohRec;

begin
  for i:=0 to DataCount-1 do begin
    Application.ProcessMessages;
    S:=read_ascii_Str(Rohstring, StringPos);    { 1 Rohdatenelement lesen }
    split_ascii(S, RohRec);  { in Rohdaten-Struktur konvertieren }  
    Eintragen (Receiver, RohRec.Adresse, RohRec.Wert);
  end;  { end of for }
end;

{----------------------------------------------------------------------------------}
function TDELList.LoadFromRohfileList (RohdatenListe: TDSfGDataList; Receiver: char;
                                       RohLoeschen: boolean;
                                       bPublicKey_vorhanden: boolean;
                                       var Fehlergruppe: integer;
                                       var Fehlercode: integer): integer;
{----------------------------------------------------------------------------------}
{ Konvertiert die in RohdatenListe übergebenen Rohdatenfiles einer Instanz.
  Übergabe: RohdatenListe (Name der Rohdateien)
            Instanz-Adresse
            Flag "RohLoeschen" (wenn true, werden die Rohdateien nach der Konvertierung gelöscht)
            Flag "Public Key vorhanden" (false übergeben, wenn kein öffentlicher Schlüssel
                                         für Signatur-Verifizierung vorliegt)
  Rückgabe: Fehlergruppe
            Fehlercode
  Ergebnis: DSFGKONVERR_...-Code }
var
  i: integer;
  StatusBuf: integer;
  Fehler_AA: boolean;
  Fehler_sonst: boolean;
  Fehlergruppe_Signatur: integer;
  Fehlercode_Signatur: integer;
  FehlergruppeBuf_Signatur: integer;  
  FehlercodeBuf_Signatur: integer;

begin
  Result:=DSFGKONVERR_OK;
  { Vorbelegungen für Rückgabe Fehlergruppe/Fehlercode: OK }
  Fehlergruppe:=0;
  Fehlercode:=0;

  { Vorbelegung für Konvertierung: kein Fehler }
  Fehler_AA:=false;     { keine außerplanmäßige Antwort }
  Fehler_sonst:=false;  { kein sonstiger Konvertierungsfehler }

  // Vorbelegung Fehlergruppe/-code für Signatur-Verifizierung: OK
  Fehlergruppe_Signatur:=0;
  Fehlercode_Signatur:=0;

  for i:=0 to RohdatenListe.Count-1 do begin
    RohfileName:=RohdatenListe[i];
    StatusBuf:=Konv_Rohfile (Receiver, RohLoeschen, bPublicKey_vorhanden,
                             FehlergruppeBuf_Signatur, FehlercodeBuf_Signatur);
    { Fehler: }
    if (StatusBuf <> DSFGKONVERR_OK) AND (StatusBuf <> DSFGKONVERR_NODATA) then begin
      if (StatusBuf >= C_MinCode_DSfGKonvErr_AA) then  { außerplanmäßige Antwort }
        Fehler_AA:=true
      else if (StatusBuf >= C_MinCode_DSfGKonvErr_SigVerify) then begin
        { Signatur-Fehlerstatus hier unberücksichtigt; 24.01.2013, WW }
      end else
        Fehler_sonst:=true;
    end;

    { ersten Fehler-Status merken: }
    if (StatusBuf <> DSFGKONVERR_OK) AND (Result = DSFGKONVERR_OK) then
      Result:=StatusBuf;
    { ersten Signatur-Fehler merken: }
    if not FehlerGruppeCode_OK (FehlergruppeBuf_Signatur, FehlercodeBuf_Signatur) then begin
      if FehlerGruppeCode_OK (Fehlergruppe_Signatur, Fehlercode_Signatur) then begin
        Fehlergruppe_Signatur:=FehlergruppeBuf_Signatur;
        Fehlercode_Signatur:=FehlercodeBuf_Signatur;
      end;
    end;
  end; { for i:=0 }

  { Konvertierungsfehler-Rückgaben:
    -> sonstige Konvertierungsfehler vor außerplanmäßiger Antwort }
  if Fehler_sonst then begin
    Fehlergruppe:=ST_KONVERROR;
    Fehlercode:=SKERR_INSTWERTKONV;
  end
  else if Fehler_AA then begin
    Fehlergruppe:=ST_KONVERROR;
    Fehlercode:=SKERR_AUSSERPLANMAESSIGEANTW_DE; // 20.07.2004, WW
  end;

  if FehlerGruppeCode_OK (Fehlergruppe, Fehlercode) then begin  // 04.01.2013, WW
    if not FehlerGruppeCode_OK (Fehlergruppe_Signatur, Fehlercode_Signatur) then begin
      { Signatur-Fehler liegt vor }
      Fehlergruppe:=Fehlergruppe_Signatur;
      Fehlercode:=Fehlercode_Signatur;
    end;
  end;
end;

{-------------------------------------------------------------------------}
function TDELList.Konv_Rohfile (Receiver: char; RohLoeschen: boolean;
                                bPublicKey_vorhanden: boolean;
                                var Fehlergruppe_Signatur: integer;
                                var Fehlercode_Signatur: integer): integer;
{-------------------------------------------------------------------------}
{ Konvertiert Datenelemente aus einer Rohdatei in die Datenelementeliste.
  Übergabe: Receiver (Busadresse)
            Flag "RohLoeschen" (wenn true, wird die Rohdatei nach der Konvertierung gelöscht)
            Flag "Public Key vorhanden" (false übergeben, wenn kein öffentlicher Schlüssel
                                         für Signatur-Verifizierung vorliegt)
  Rückgabe: Fehlergruppe und -code für Signatur-Verifizierung
  Ergebnis: Konvertierungs-Status }
var
  DataCount: integer;
  dummy: boolean;
  sPTB: string;
  iTID: integer;
  sData: string;
  DSfGPTBData: TDSfGPTBData;
  SigVerifyStatus: integer;
  bDoVerify: boolean;

begin
  // Vorbelegung Fehlergruppe/-code für Signatur-Verifizierung: OK
  Fehlergruppe_Signatur:=0;
  Fehlercode_Signatur:=0;

  Result:=OpenRohfile;                                  { Rohdatenfile öffnen }
  try
    if Result <> DSFGKONVERR_OK then exit;
    { Validierung des HDCL im DSfG-Telegramm: }
    Result:=CheckDSfGHeader (FS_Rohfile, Receiver, debc_MengeDE, DataCount, dummy,
                             sPTB, iTID);
    if Result = DSFGKONVERR_OK then begin            { jetzt wird konvertiert }
      { PTB-Feld des HDCL mit Inhalt: Signatur verifizieren; 04.01.2013, WW }
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
              Receiver, '');
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
          Fehlergruppe_Signatur:=SYS_SIGNSRV_ERROR;
          Fehlercode_Signatur:=SIGNSRVERR_NOTVERIFIED;
        end;
      end else
        SigVerifyStatus:=C_SigVerifyState_None;

      DELDataLoop_File (Receiver, DataCount, SigVerifyStatus);

      { Signatur-Verifizierungsstatus, welche "Fehler" darstellen, in
        Konvertierungs-Status übernehmen: 04.01.2013, WW }
      if Result = DSFGKONVERR_OK then begin  { Konvertierungsfehler nicht überschreiben }
        case SigVerifyStatus of
          C_SigVerifyState_Invalid:
            Result:=DSFGKONVERR_SIGVERIFY_INVALID;

          C_SigVerifyState_NotVerified,
          C_SigVerifyState_PubKeyMissing:  // 13.03.3018, WW
            Result:=DSFGKONVERR_SIGVERIFY_NOTVERIFIED;
        end;
      end;
    end;  { if Result = DSFGKONVERR_OK }
  finally
    CloseRohfile;                                    { Rohdatenfile schließen }
  end;

  { Rohfile löschen: }
  if RohLoeschen AND (Result <> DSFGKONVERR_HEADER) AND (Result <> DSFGKONVERR_DATA) then
    DeleteFile (RohfileName);
end;

{-------------------------------------}
function TDELList.OpenRohfile: integer;
{-------------------------------------}
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

{------------------------------}
procedure TDELList.CloseRohfile;
{------------------------------}
begin
  FreeAndNil (FS_Rohfile);  // 14.03.2007, WW
end;

{----------------------------------------------------------------------}
procedure TDELList.DELDataLoop_File (Receiver: char; DataCount: integer;
                                     SigVerifyStatus: integer);
{----------------------------------------------------------------------}
{ Schleife durch Datenelemente im DSfG-Telegramm (Datei). Listenobjekt wird in
  Datenelementeliste eingetragen.
  Übergabe: Busadresse
            Anzahl der Datenelemente
            Signatur-Verifizierungsstatus }
var
  i: integer;
  S: string;
  RohRec: TDSfGRohRec;
  FSize_Roh: integer;
  bletzt_ascii: boolean;

begin
  FSize_Roh:=FS_Rohfile.Size;  { Rohfilegröße }
  for i:=0 to DataCount-1 do begin
    Application.ProcessMessages;
    if FS_Rohfile.Position >= FSize_Roh then  // zusätzlich auf Rohfileende prüfen; 14.03.2007 WW
      Break;

    read_ascii(FS_Rohfile, S, bletzt_ascii);  { 1 Rohdatenelement lesen }
    split_ascii(S, RohRec);  { in Rohdaten-Struktur konvertieren }
    Eintragen (Receiver, RohRec.Adresse, RohRec.Wert, SigVerifyStatus);  // 04.01.2013, WW
  end;  { end of for }
end;

{---------------------------------------------------------------------------------}
function TDELList.LoadFromRohstringXML(Rohstring: string; Receiver: char): integer;
{---------------------------------------------------------------------------------}
{ Konvertiert die im Rohstring übergebenen Datenelemente in die Datenelementeliste.
  Übergabe: Rohstring
            Receiver (EAdr der Instanz, von der die Datenelemente angefordert wurden)
  Ergebnis: Konvertierungsstatus }
var
  i                  : integer;
  sData, sTyp        : string;
  sIAdr, sDea, sWert : string;
begin
  Result := DSFGKONVERR_DATA;

  with TDSfGXmlDecodeObject.Create do
  try
    if (ConvertDataToList(Rohstring, sData, sTyp)) then begin
      with TStringList.Create do
      try
        Text := sData;
        for i := 0 to Count-1 do begin
          // Struktur: IAdr<us>DEA<us>Wert<us>UnixDT<us>ONr<us>Status<us>CRC
          sIAdr := GetStringPart(Strings[i], 1);
          if (sIAdr = Receiver) then begin
            sDea := GetStringPart(Strings[i], 2);
            sWert := GetStringPart(Strings[i], 3);
            if (sDea <> '') then Eintragen(sIAdr, sDea, sWert);
          end;
        end;
      finally
        Free;
      end;
      Sort (EAdr_DELCompare);    { Liste nach EAdr und DEL sortieren }
      Result := DSFGKONVERR_OK;
    end;
  finally
    Free;
  end;
end;

{-------------------------------------------------------------------------------------}
function TDelList.LoadFromRohfileListXML (RohdatenListe: TDSfGDataList; Receiver: char;
                                          RohLoeschen: boolean): boolean;
{-------------------------------------------------------------------------------------}
var
  i, j: integer;
  iStatus: integer;
  sRoh, sData, sTyp        : string;
  sIAdr, sDea, sWert : string;
begin
  iStatus := DSFGKONVERR_OK;

  with TDSfGXmlDecodeObject.Create do
  try

    for i := 0 to RohdatenListe.Count-1 do begin

      with TStringList.Create do
      try
        if (FileExists(RohdatenListe[i])) then LoadFromFile(RohdatenListe[i])
        else begin
          iStatus := DSFGKONVERR_NODATA;
          Continue;
        end;
        sRoh := Text;
      finally
        Free;
      end;

      if (ConvertDataToList(sRoh, sData, sTyp)) then begin
        with TStringList.Create do
        try
          Text := sData;
          for j := 0 to Count-1 do begin
            // Struktur: IAdr<us>DEA<us>Wert<us>UnixDT<us>ONr<us>Status<us>CRC
            sIAdr := GetStringPart(Strings[j], 1);
            if (sIAdr = Receiver) then begin
              sDea := GetStringPart(Strings[j], 2);
              sWert := GetStringPart(Strings[j], 3);
              if (sDea <> '') then Eintragen(sIAdr, sDea, sWert);
            end;
          end;
        finally
          Free;
        end;

        { Ggf. Dateien löschen }
        if (RohLoeschen) then DeleteFile(RohdatenListe[i]);
      end
      else iStatus := DSFGKONVERR_DATA;
    end;

    Sort (EAdr_DELCompare);    { Liste nach EAdr und DEL sortieren }
  finally
    Free;
  end;

  Result := (iStatus = DSFGKONVERR_OK);
end;

{----------------------------------------------------------------------------------}
function TDELList.LoadGeraeteZustand (Receiver: char; sVirtualDEA: string): integer;
{----------------------------------------------------------------------------------}
{ Wert für virtuellen Gerätezustand-Parameter bilden und in Parameterliste eintragen;
  Übergabe: Receiver (Busadresse)
            Virtuelle DE-Adresse für Gerätezustand
  Ergebnis: Konvertierungsstatus }
var
  sGerZustandHex: string;
  bOK: boolean;

begin
  Result:=DSFGKONVERR_DATA;
  bOK:=BuildGeraeteZustand (Receiver, sVirtualDEA, sGerZustandHex);

  if bOK then begin
    { Gerätezustand in Datenelementeliste eintragen: }
    Eintragen (Receiver, sVirtualDEA, sGerZustandHex);
    Sort (EAdr_DELCompare);    { Liste nach EAdr und DEL sortieren }
    Result:=DSFGKONVERR_OK;
  end;
end;

{-------------------------------------------------------------------------}
function TDELList.BuildGeraeteZustand (Receiver: char; sVirtualDEA: string;
  var sGerZustandHex: string): boolean;
{-------------------------------------------------------------------------}
{ Wert für virtuellen Gerätezustand-Parameter bilden;
  Übergabe: Receiver (Busadresse)
            Virtuelle DE-Adresse für Gerätezustand
  Rückgabe: Hex-codierter Gerätezustand
  Ergebnis: true, wenn bilden des Werts ohne Fehler }
var
  iGerZustand: Int64;

begin
  Result:=false;
  sGerZustandHex:='';  // Vorbelegung Rückgabe

  if sVirtualDEA = CD_ALLG_MRG900_GerZustand_aktuell then begin
    if not BuildGeraeteZustand_MRG900 (Receiver, sVirtualDEA, iGerZustand) then exit;
  end else
    exit;  // Fehler: Gerätezustand wurde nicht gebildet

  sGerZustandHex:=IntToHex (iGerZustand, 1);  // Hex, nur soviele Stellen wie nötig
  Result:=true;
end;

{--------------------------------------------------------------------------------}
function TDELList.BuildGeraeteZustand_MRG900 (Receiver: char; sVirtualDEA: string;
  var iGerZustand: Int64): boolean;
{--------------------------------------------------------------------------------}
{ Virtuellen Gerätezustand für MRG 905/910 bilden;
  Übergabe: Receiver (Busadresse)
            Virtuelle DE-Adresse für Gerätezustand
  Rückgabe: Gerätezustand als Integer
  Ergebnis: true, wenn bilden des Gerätezustands ohne Fehler }
var
  Wert: string;

begin
  Result:=false;
  iGerZustand:=0;  // Vorbelegung Gerätezustand: Alle Bits gelöscht

  // Aktueller Gerätezustand:
  if (sVirtualDEA = CD_ALLG_MRG900_GerZustand_aktuell) then begin
    // DE: Eichschalter
    if not GetWert (Receiver, CD_ALLG_Eichschalter, Wert) then exit;
    if not SetGeraeteZustand_MRG900 (Wert, CD_ALLG_Eichschalter,
                                     iGerZustand) then exit;

    // DE: Signalausgang 1 Zustand
    if not GetWert (Receiver, CD_WIESER_Ausg1_Zustand, Wert) then exit;
    if not SetGeraeteZustand_MRG900 (Wert, CD_WIESER_Ausg1_Zustand,
                                     iGerZustand) then exit;

    // DE: Signalausgang 2 Zustand
    if not GetWert (Receiver, CD_WIESER_Ausg2_Zustand, Wert) then exit;
    if not SetGeraeteZustand_MRG900 (Wert, CD_WIESER_Ausg2_Zustand,
                                     iGerZustand) then exit;
    Result:=true;
  end;
end;

end.

