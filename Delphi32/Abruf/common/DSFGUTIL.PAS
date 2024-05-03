{******************************************************************************}
{* Unit: Hilfsfunktionen für DSfG-Abrufmodul                                  *}
{* Version: 18.11.1999       WW                                               *}
{* 06.10.2015 WW  Beschleunigte Rohdateizugriffe mit TFileOfCharStream        *}
{* 22.06.2022 WW  Anpassung für Archivdaten aus Standardabfragen              *}
{* 19.12.2022 WW  Anpassung CheckDSfGHeader für unvollständige Antwort ohne   *}
{*                Daten                                                       *}
{******************************************************************************}
unit DSfGUtil;

interface

uses
  Forms, Classes, SysUtils, WChars, WStrUtils, WSysCon, ErrConst, UnixDT,
  Dsg_Utils, WStream, DListen;

type
  { Typen für Prüfung des DCL-Feldes DEB }
  TDEBCheck = (debc_OrdNr_Zeit, debc_MengeDE);

  { Struktur für DCL-Feld PTB }
  TDSfGPTBData = record
    Kenner: string;
    DatumZeit: string;
    Signatur_r: string;
    Signatur_s: string;
  end;


function read_hdcl (FStream: TFileOfCharStream): string;
function read_hdcl_fromString (Rohstring: string; var StringPos: integer): string;
function read_ascii (FStream: TFileOfCharStream; var ascii: string;
                     var bletzt_ascii: boolean): boolean;
function read_ascii_fromString (Rohstring: string; var StringPos: integer;
                                var ascii: string;
                                var bletzt_ascii: boolean): boolean;
procedure split_ascii (s: string; var RohRec: TDSfGRohRec);
function CheckDSfGHeader (FS_Rohfile: TFileOfCharStream;
                          Receiver: char; DEBCheck: TDEBCheck;
                          var ZAE: integer; var alles_da: boolean;
                          var PTB: string; var TID: integer): integer;
function CheckDSfGTelegram (sTelegramm: string;
  var cEAdr, cDNO, cNTY, cDFO, cDEB: char; var iZAE: integer;
  DEList: TDSfGTelegrDEList): integer;
function CheckDSfGArchivLogbuchRohsatz (RohRec: TDSfGRohRec;
                                        bCheckValueAsDouble,
                                        bCheckStatusWithSigStatus: boolean;
                                        pKanalKonvList: TKanalKonvList): integer;
function CheckDSfGDatenelementRohsatz (RohRec: TDSfGRohRec;
                                       var KonvErr_Ext: integer): integer;
function GetNextDatenelementAdresse (DEL: string): string;
function GetNextDatenelementeMenge (DEL_Menge: string; DEL: string): string;
function ReadFile_DSfGDataPart (FStream: TFileOfCharStream): string;
procedure GetDSfGPTBDataFromDCL (sPTB: string; var DSfGPTBData: TDSfGPTBData);

implementation

const
  // gültige Längen von Datenelement-Feldern
  CMaxLenDE_Adresse = 5;  // max. Länge für Datenelement-Adresse
  CLenDE_UnixTime   = 8;  // feste Länge für Datenelement-Zeitstempel (Unixzeit)
  CMaxLenDE_CRC     = 3;  // max. Länge für Datenelement-Prüfsumme (CRC12)

  // gültige Datenelement-Adresszeichen
  CValidDEAdrCharMin = 'a';
  CValidDEAdrCharMax = 'z';
  CValidDEAdrCharSet = [CValidDEAdrCharMin..CValidDEAdrCharMax];


{------------------------------------------------}
function read_hdcl (FStream: TFileOfCharStream): string;  // 06.10.2015, WW
{------------------------------------------------}
{ liest HDCL aus geöffnetem FileStream und gibt diesen zurück }
var
  s: string;
  zeich: char;

begin
  Result:='';
  if not Assigned (FStream) then exit;

  if FStream.Read (zeich) < 1 then exit;
  s:='';
  while (zeich <> US) and (zeich <> FS) and (zeich <> GS) do begin
    s:=s + zeich;
    if FStream.Read (zeich) < 1 then exit;
  end;
  Result:=s;
end;

{--------------------------------------------------------------------------------}
function read_hdcl_fromString (Rohstring: string; var StringPos: integer): string;
{--------------------------------------------------------------------------------}
{ liest HDCL aus Rohstring und gibt diesen zurück }
var
  s: string;
  zeich: char;
begin
  S:='';
  while StringPos <= length(Rohstring) do begin
    zeich:=Rohstring[StringPos];
    inc(StringPos);
    if (zeich = US) or (zeich = FS) or (zeich = GS) then
      Break
    else
      S:=S + zeich;
  end;
  Result:=S;
end;

{-----------------------------------------------------------}
function read_ascii (FStream: TFileOfCharStream;  // 06.10.2015, WW
  var ascii: string; var bletzt_ascii: boolean): boolean;
{-----------------------------------------------------------}
{ liest aus geöffnetem DSfG-Rohdaten-FileStream ein Datenelement zwischen Gruppen-
  oder Fileseparator und hängt am Anfang und Ende des Datenelement-Strings einen
  Unitseparator an;
  Ergebnis: true, wenn Roh-Datenelement mit GS oder FS abgeschlossen ist }
var
  s: string;
  zeich: char;

begin
  Result:=false;
  ascii:='';
  bletzt_ascii:=false;
  if not Assigned (FStream) then exit;

  if FStream.Read (zeich) < 1 then exit;
  s:='';
  while (zeich <> FS) and (zeich <> GS) do begin
    s:=s + zeich;
    if FStream.Read (zeich) < 1 then exit;
  end;

  if zeich = FS then begin  // der letzte Datensatz ist mit einem FS abgeschlossen
    bletzt_ascii:=true;
    Result:=true;
  end
  else if zeich = GS then  // alle Datensätze davor mit GS
    Result:=true;

  ascii:=US + s + US;
end;

{------------------------------------------------------------------------}
function read_ascii_fromString (Rohstring: string; var StringPos: integer;
                                var ascii: string;
                                var bletzt_ascii: boolean): boolean;
{------------------------------------------------------------------------}
{ liest aus DSfG-Rohstring ein Datenelement zwischen Gruppen- oder Fileseparator
  und hängt am Anfang und Ende des Datenelement-Strings einen Unitseparator an;
  Ergebnis: true, wenn Roh-Datenelement mit GS oder FS abgeschlossen ist }
var
  s: string;
  zeich: char;

begin
  Result:=false;
  ascii:='';
  bletzt_ascii:=false;

  S:='';
  zeich:=NUL;
  while StringPos <= length(Rohstring) do begin
    zeich:=Rohstring[StringPos];
    inc(StringPos);
    if (zeich = FS) or (zeich = GS) then
      Break
    else
      S:=S + zeich;
  end;

  if zeich = FS then begin  // der letzte Datensatz ist mit einem FS abgeschlossen
    bletzt_ascii:=true;
    Result:=true;
  end
  else if zeich = GS then  // alle Datensätze davor mit GS
    Result:=true;

  ascii:=US + s + US;
end;

{---------------------------------------------------------}
procedure split_ascii (s: string; var RohRec: TDSfGRohRec);
{---------------------------------------------------------}
{ aufsplitten eines Datenelements in seine einzelnen Bestandteile;
  Übergabe: Datenelement-String (am Anfang und Ende durch US begrenzt)
  Rückgabe: in RohRec konvertierte Datenelementteile }
var
  sbuf, zbuf: PChar;
  Len : integer;

begin
  Len := Length (s) + 1;
  getmem(sbuf,Len);
  try
    getmem(zbuf,Len);
    try
      StrPCopy(sbuf,s);
      If FilterString (zbuf, sbuf, US, US, nil, 0) <> Nil Then                   { DEL-Adresse }
        RohRec.Adresse := string(zbuf)
      else
        RohRec.Adresse := '';

      If FilterString (zbuf, sbuf, US, US, nil, 1) <> Nil Then                   { Datenelement-Wert }
        RohRec.Wert := string(zbuf)
      else
        RohRec.Wert:='';

      If FilterString (zbuf, sbuf, US, US, nil, 2) <> Nil Then                   { UNIX-Zeit }
        RohRec.UTime := string(zbuf)
      else
        RohRec.UTime := '';

      If FilterString (zbuf, sbuf, US, US, nil, 3) <> Nil Then                   { Ordnungsnummer }
        RohRec.OrdNr := string(zbuf)
      else
        RohRec.OrdNr:='';

      If FilterString (zbuf, sbuf, US, US, nil, 4) <> Nil Then                   { Status }
        RohRec.Status:=string(zbuf)
      else
        RohRec.Status:='';

      If FilterString (zbuf, sbuf, US, US, nil, 5) <> Nil Then                   { PTB-Prüfsumme }
        RohRec.CRC:=string(zbuf)
      else
        RohRec.CRC:='';
    finally
      freemem(zbuf,Len);
    end;
  finally
    freemem(sbuf,Len);
  end;
end;

{--------------------------------------------------------------------------}
function CheckDSfGHeader (FS_Rohfile: TFileOfCharStream;  // 06.10.2015, WW
                          Receiver: char; DEBCheck: TDEBCheck;
                          var ZAE: integer; var alles_da: boolean;
                          var PTB: string; var TID: integer): integer;
{--------------------------------------------------------------------------}
{ Verifizierung des DSfG-Telegramm HDCL's }
var
  Code: Integer;
  S: string;
  DID: integer;
  Ausserplanmaessige_Antwort: boolean;

begin
  { Vorbelegung für Rückgaben: }
  ZAE:=0;
  alles_da:=false;                                  { Antwort unvollständig }
  PTB:='';
  TID:=0;

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
  val(S, TID, Code);  // 17.11.2011, WW
  if Code <> 0 then begin
    Result := DSFGKONVERR_HEADER;
    exit;
  end;

  S := read_hdcl(FS_Rohfile);                                            { BLO }
  S := read_hdcl(FS_Rohfile);                                            { BNR }
  S := read_hdcl(FS_Rohfile);                                            { DNO }
  if (Receiver <> NUL) AND (Receiver <> S) then begin   { Überprüfen des Absenders }
    Result := DSFGKONVERR_DATA;
    exit;
  end;

  S := read_hdcl(FS_Rohfile);                                            { NTY }
  if S <> '' then begin
    if (S[1] <> 'R') AND (S[1] <> 'U') then begin  { nur R und U sind erlaubt }
      Result:=DSFGKONVERR_HEADER;
      exit;
    end;
    if S[1] = 'R' then
      alles_da:=true;                                { vollständige Antwort }
  end
  else begin
    Result:=DSFGKONVERR_HEADER;
    exit;
  end;

  S := read_hdcl(FS_Rohfile);                                            { DFO }
  S := read_hdcl(FS_Rohfile);                                            { DEB }
  if S <> '' then begin
    case DEBCheck of
      debc_OrdNr_Zeit:  { bei Abfrage nach Ordnungsnummer, Zeit }
        if (S[1] <> 'O') AND (S[1] <> 'Z') AND (S[1] <> 'A') then begin
          Result:=DSFGKONVERR_HEADER;
          exit;
        end;

      debc_MengeDE:     { bei Abfrage von Datenelementen }
        if (S[1] <> 'M') AND (S[1] <> 'A') then begin
          Result:=DSFGKONVERR_HEADER;
          exit;
        end;
    end;

    Ausserplanmaessige_Antwort:=S[1] = 'A';      // 20.07.2004, WW
    if Ausserplanmaessige_Antwort then
      alles_da:=true;                                { vollständige Antwort }
  end
  else begin
    Result:=DSFGKONVERR_HEADER;
    exit;
  end;

  S := read_hdcl(FS_Rohfile);                                            { ZAE }
  val(S, ZAE, Code);                     { Anzahl der Datenelemente im File }
  if (Code <> 0) OR (ZAE < 0) then begin
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
    PTB := read_hdcl(FS_Rohfile);                                        { PTB }

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
  end
  else begin
    if ZAE = 0 then begin
      Result := DSFGKONVERR_NODATA;
      { Keine Daten = vollständige Antwort
        -> Bugfix für Kombination NTY = U, ZAE = 0 (Endlosschleife in Abruf !).
        -> Aufgetreten bei Registrierung 'Marquis MRG2203A Alternative' mit DFÜ-
           Instanz 'Elster enCore MC1' (GAS-X, Thyssengas); 19.12.2022, WW }
      alles_da:=true;
    end else
      Result := DSFGKONVERR_OK;
  end;
end;

{-----------------------------------------------------------}
function CheckDSfGTelegram (sTelegramm: string;
  var cEAdr, cDNO, cNTY, cDFO, cDEB: char; var iZAE: integer;
  DEList: TDSfGTelegrDEList): integer;
{-----------------------------------------------------------}
{ DSfG-Telegramm syntaktisch und inhaltlich prüfen;
  Übergabe: Telegramm-String
  Rückgaben: Adresse des Empfängers
             DNO, NTY, DFO, DEB als Zeichen
             ZAE als Gannzahl
             Datenelementeliste (Datenteil des Telegramms)
  Ergebnis:  0 = Telegramm OK
            -1 = Busadresse des Empfängers fehlt
            -2 = DID fehlt oder ungültig
            -3 = DNO fehlt
            -4 = NTY fehlt
            -5 = DFO fehlt
            -6 = DEB fehlt
            -7 = ZAE fehlt oder ungültig
            -8 = Datenelement im Datenteil unvollständig
            -9 = ZAE <> Anzahl Datenelemente }
var
  StringPos: integer;
  S: string;
  iDID: integer;
  Code: integer;
  iDECount: integer;
  bLetztDE_gelesen: boolean;
  bLetzt_ascii: boolean;
  i: integer;
  sRohsatz: string;
  RohRec: TDSfGRohRec;
  DSfGTelegrDEObj: TDSfGTelegrDEObj;

begin
  { Vorbelegungen für Rückgaben: }
  cEAdr:=NUL;
  cDNO:=NUL;
  cNTY:=NUL;
  cDFO:=NUL;
  cDEB:=NUL;
  iZAE:=0;
  if Assigned (DEList) then
    DEList.Clear;

  StringPos:=1;
  S := read_hdcl_fromString(sTelegramm, StringPos);    { Busadresse Empfänger }
  if length (S) = 0 then begin
    Result:=-1;
    exit;
  end;
  cEAdr:=S[1];

  { HDCL auswerten: }
  S := read_hdcl_fromString(sTelegramm, StringPos);                     { DID }
  Val(S, iDID, Code);
  if (Code <> 0) OR (iDID < C_DID_DataSimple) then begin
    Result:=-2;
    exit;
  end;

  S := read_hdcl_fromString(sTelegramm, StringPos);                     { TID }
  S := read_hdcl_fromString(sTelegramm, StringPos);                     { BLO }
  S := read_hdcl_fromString(sTelegramm, StringPos);                     { BNR }
  S := read_hdcl_fromString(sTelegramm, StringPos);                     { DNO }
  if length (S) = 0 then begin
    Result:=-3;
    exit;
  end;
  cDNO:=S[1];

  S := read_hdcl_fromString(sTelegramm, StringPos);                     { NTY }
  if length (S) = 0 then begin
    Result:=-4;
    exit;
  end;
  cNTY:=S[1];

  S := read_hdcl_fromString(sTelegramm, StringPos);                     { DFO }
  if length (S) = 0 then begin
    Result:=-5;
    exit;
  end;
  cDFO:=S[1];

  S := read_hdcl_fromString(sTelegramm, StringPos);                     { DEB }
  if length (S) = 0 then begin
    Result:=-6;
    exit;
  end;
  cDEB:=S[1];

  S := read_hdcl_fromString(sTelegramm, StringPos);                     { ZAE }
  Val(S, iZAE, Code);
  if (Code <> 0) OR (iZAE < 0) then begin
    Result:=-7;
    exit;
  end;

  { weitere optionale Felder: }
  if (iDID AND C_PAS) <> 0 then
    S := read_hdcl_fromString(sTelegramm, StringPos);                   { PAS }
  if (iDID AND C_DTY) <> 0 then
    S := read_hdcl_fromString(sTelegramm, StringPos);                   { DTY }
  if (iDID AND C_ABS) <> 0 then
    S := read_hdcl_fromString(sTelegramm, StringPos);                   { ABS }
  if (iDID AND C_EMF) <> 0 then
    S := read_hdcl_fromString(sTelegramm, StringPos);                   { EMF }
  if (iDID AND C_TDA) <> 0 then
    S := read_hdcl_fromString(sTelegramm, StringPos);                   { TDA }
  if (iDID AND C_TTI) <> 0 then
    S := read_hdcl_fromString(sTelegramm, StringPos);                   { TTI }
  if (iDID AND C_PTB) <> 0 then
    S := read_hdcl_fromString(sTelegramm, StringPos);                   { PTB }

  { Datenteil lesen: }
  iDECount:=0;  // Vorbelegung: Anzahl der tatsächlich vorhandenen DE-Sätze
  bLetztDE_gelesen:=false;
  for i:=1 to iZAE do begin
    Application.ProcessMessages;
    { 1 DE-Satz lesen: }
    if not read_ascii_fromString(sTelegramm, StringPos, sRohsatz, bLetzt_ascii) then begin
      Result:=-8;
      exit;
    end;

    if not bLetztDE_gelesen then
      inc (iDECount);  // Anzahl DE-Sätze weiterzählen
    if bLetzt_ascii then  // letzter DE-Satz wurde gelesen
      bLetztDE_gelesen:=true;
    split_ascii(sRohsatz, RohRec);  { in Rohdaten-Struktur konvertieren }

    DSfGTelegrDEObj:=TDSfGTelegrDEObj.Create(RohRec);
    DEList.Add (DSfGTelegrDEObj);
  end;  { for i }

  { Anzahl der gelesenen DE mit ZAE vergleichen: }
  if iZAE <> iDECount then begin
    Result:=-9;
    exit;
  end;

  Result:=0;  { OK }
end;

{-------------------------------------------------------------------------------}
function CheckDSfGArchivLogbuchRohsatz (RohRec: TDSfGRohRec;
                                        bCheckValueAsDouble,
                                        bCheckStatusWithSigStatus: boolean;
                                        pKanalKonvList: TKanalKonvList): integer;
{-------------------------------------------------------------------------------}
{ Gültigkeitsprüfung für DSfG-Rohsatz aus Archiv-/Logbuch-Abfrage;
  Übergabe: DSfG-RohRec
            Flag 'bCheckValueAsDouble': wenn true, wird geprüft, ob Wert in Double wandelbar ist
            Flag 'bCheckStatusWithSigStatus': wenn true, wird im Status ein evtl. vorhandener
              Signaturstatus mitberücksichtigt
  Ergebnis: DSFGKONVERR_...-Konstante }
var
  i: integer;
  d: double;
  dt: TDateTime;
  Code: integer;
  sStatus: string;
  b: boolean;
  KanalNr: integer;
  Kanaltyp: string;
  sDummy: string;

begin
  { Datenelement-Adresse: }
  if length (RohRec.Adresse) > 0 then begin  // Längenprüfung: nicht leer !
    if RohRec.Adresse [1] = 'c' then begin  // Datenelement-Adresse einer Registrier-Instanz
      if length (RohRec.Adresse) <> CMaxLenDE_Adresse then begin  // Längenprüfung: genau 5 Stellen !
        Result:=DSFGKONVERR_INVADDR;  // ungültige Datenelement-Adresse
        exit;
      end;
      for i:=1 to length (RohRec.Adresse) do begin
        if ((i = 5) AND (RohRec.Adresse [i] <> 'd')) OR
           ((i > 1) AND (i < 5) AND not (RohRec.Adresse [i] in CValidDEAdrCharSet)) then begin
          Result:=DSFGKONVERR_INVADDR;  // ungültige Datenelement-Adresse
          exit;
        end;
      end;
    end
    else begin  // Datenelement-Adresse anderer Instanztypen (z.B. aus Standardabfrage Umwerter)
      // 22.06.2022, WW
      if length (RohRec.Adresse) > CMaxLenDE_Adresse then begin  // Längenprüfung: max. 5 Stellen !
        Result:=DSFGKONVERR_INVADDR;  // ungültige Datenelement-Adresse
        exit;
      end;
      for i:=1 to length (RohRec.Adresse) do begin
        if not (RohRec.Adresse [i] in CValidDEAdrCharSet) then begin
          Result:=DSFGKONVERR_INVADDR;  // ungültige Datenelement-Adresse
          exit;
        end;
      end;
    end;
  end
  else begin
    Result:=DSFGKONVERR_INVADDR;  // ungültige Datenelement-Adresse
    exit;
  end;

  { Unix-Zeitstempel: }
  if length (RohRec.UTime) <> CLenDE_UnixTime then begin  // Längenprüfung: genau 8 Stellen !
    Result:=DSFGKONVERR_INVTIME;
    exit;
  end;
  { Umrechnung: UNIX-Zeitstring -> DateTime-Format für Tabelle }
  if not UnixTimeStrToDateTime (RohRec.UTime, dt) then begin
    Result:=DSFGKONVERR_INVTIME;
    exit;
  end;

  { Ordnungsnummer: }
  if length (RohRec.OrdNr) = 0 then begin  // Längenprüfung: nicht leer !
    Result:=DSFGKONVERR_INVNUMBER;
    exit;
  end;
  for i:=1 to length (RohRec.OrdNr) do begin
    if not (RohRec.OrdNr [i] in ['0'..'9','+']) then begin  // dezimal, nicht negativ
      Result:=DSFGKONVERR_INVNUMBER;  // ungültige Ordnungsnummer
      exit;
    end;
  end;
  try
    Val(RohRec.OrdNr, i, Code);
    if (Code <> 0) OR (i < 0) then begin
      Result:=DSFGKONVERR_INVNUMBER;
      exit;
    end;
  except
    Result:=DSFGKONVERR_INVNUMBER;
    exit;
  end;

  { Wert: }
{  if length (RohRec.Wert) = 0 then begin  // Längenprüfung: nicht leer !
    Result:=DSFGKONVERR_INVVALUE;
    exit;
  end;  ab 19.06.2009 keine Längenprüfung mehr, Text-Archivwerte können leer sein; WW }


  b:=bCheckValueAsDouble;
  if (not b) AND (Assigned (pKanalKonvList)) then begin  // 12.07.2016, WW
    // bei Status- und Text-Kanälen Rohwert nicht auf Konvertierbarkeit in Typ
    // Double prüfen
    if RohRec.Adresse [4] in ['f'..'z'] then begin
      KanalNr:= Ord (RohRec.Adresse [4]) - C_INT_DELKANALOFFSET;
      if pKanalKonvList.GetKanalData(KanalNr, Kanaltyp, sDummy) then
        b := (Kanaltyp <> kt_ST) and (Kanaltyp <> kt_TX);
    end;
  end;

  if b then begin
    try
      Val(RohRec.Wert, d, Code);
      if d <> 0 then;  //  Delphi-Compilerhinweis unterdrücken
      if Code <> 0 then begin
        Result:=DSFGKONVERR_INVVALUE;
        exit;
      end;
    except
      Result:=DSFGKONVERR_INVVALUE;
      exit;
    end;
  end;

  { Status: }
  // Länge: darf leer sein, kein Pflichfeld !
  if bCheckStatusWithSigStatus then  // 19.03.2012, WW
    sStatus:=F_Zerlegen (RohRec.Status, '_')  // Signaturstatus durch Unterstrich vom DSfG-Status getrennt
  else
    sStatus:=RohRec.Status;
  for i:=1 to length (sStatus) do begin
    if not (sStatus [i] in ['0'..'9','+','-']) then begin  // dezimal
      Result:=DSFGKONVERR_INVSTATE;  // ungültiger Status
      exit;
    end;
  end;

  { CRC: }
  // Länge: darf leer sein, kein Pflichfeld !
  if length (RohRec.CRC) > CMaxLenDE_CRC then begin  // Längenprüfung: max. 3 Stellen (CRC12)
    Result:=DSFGKONVERR_INVCRC;
    exit;
  end;
  if length (RohRec.CRC) > 0 then begin
    try
      Val ('$' + RohRec.CRC, i, code);
      if (Code <> 0) OR (i < 0) then begin  // keine negativen Werte !
        Result:=DSFGKONVERR_INVCRC;  // ungültiges CRC
        exit;
      end;
    except
      Result:=DSFGKONVERR_INVCRC;  // ungültiges CRC
      exit;
    end;
  end;

  Result:=DSFGKONVERR_OK;
end;

{------------------------------------------------------------------------}
function CheckDSfGDatenelementRohsatz (RohRec: TDSfGRohRec;
                                       var KonvErr_Ext: integer): integer;
{------------------------------------------------------------------------}
{ Gültigkeitsprüfung für DSfG-Rohsatz aus Datenelement-Abfrage;
  Übergabe: DSfG-RohRec
  Rückgabe: erweiterter DSfGKonv-Errorcode (Prüfergebnis für Rohsätze, welche auch die Felder
            Unixzeit, Ordnungsnummer, Status, CRC enthaten, z.B. jüngste Archivkanaldaten
            in ca??d)
  Ergebnis: DSFGKONVERR_...-Konstante }
var
  i: integer;
  dt: TDateTime;
  Code: integer;

begin
  KonvErr_Ext:=DSFGKONVERR_OK;  // Vorbelegung für Rückgabe

  { Datenelement-Adresse: }
  if length (RohRec.Adresse) > CMaxLenDE_Adresse then begin  // Längenprüfung: max. 5 Stellen !
    Result:=DSFGKONVERR_INVADDR;  // ungültige Datenelement-Adresse
    exit;
  end;
  for i:=1 to length (RohRec.Adresse) do begin
    if not (RohRec.Adresse [i] in CValidDEAdrCharSet) then begin
      Result:=DSFGKONVERR_INVADDR;  // ungültige Datenelement-Adresse
      exit;
    end;
  end;

  { Wert: keine Prüfung (kann alles sein !) }
  Result:=DSFGKONVERR_OK;

  { ab hier Prüfungen für KonvErr_Ext: }
  { Unix-Zeitstempel: }
  if length (RohRec.UTime) <> 0 then begin  // Länge: darf leer sein, kein Pflichtfeld !
    if length (RohRec.UTime) <> CLenDE_UnixTime then begin  // oder genau 8 Stellen !
      KonvErr_Ext:=DSFGKONVERR_INVTIME;
      exit;
    end;
    { Umrechnung: UNIX-Zeitstring -> DateTime-Format für Tabelle }
    if not UnixTimeStrToDateTime (RohRec.UTime, dt) then begin
      KonvErr_Ext:=DSFGKONVERR_INVTIME;
      exit;
    end;
  end;

  { Ordnungsnummer: }
  if length (RohRec.OrdNr) <> 0 then begin  // Länge: darf leer sein, kein Pflichtfeld !
    for i:=1 to length (RohRec.OrdNr) do begin
      if not (RohRec.OrdNr [i] in ['0'..'9','+']) then begin  // dezimal, nicht negativ
        KonvErr_Ext:=DSFGKONVERR_INVNUMBER;  // ungültige Ordnungsnummer
        exit;
      end;
    end;
    try
      Val(RohRec.OrdNr, i, Code);
      if (Code <> 0) OR (i < 0) then begin
        KonvErr_Ext:=DSFGKONVERR_INVNUMBER;
        exit;
      end;
    except
      KonvErr_Ext:=DSFGKONVERR_INVNUMBER;
      exit;
    end;
  end;

  { Status: }
  // Länge: darf leer sein, kein Pflichfeld !
  for i:=1 to length (RohRec.Status) do begin
    if not (RohRec.Status [i] in ['0'..'9','+','-']) then begin  // dezimal
      KonvErr_Ext:=DSFGKONVERR_INVSTATE;  // ungültiger Status
      exit;
    end;
  end;

  { CRC: }
  // Länge: darf leer sein, kein Pflichfeld !
  if length (RohRec.CRC) > CMaxLenDE_CRC then begin  // Längenprüfung: max. 3 Stellen (CRC12)
    KonvErr_Ext:=DSFGKONVERR_INVCRC;
    exit;
  end;
  if length (RohRec.CRC) > 0 then begin
    try
      Val ('$' + RohRec.CRC, i, code);
      if (Code <> 0) OR (i < 0) then begin  // keine negativen Werte !
        KonvErr_Ext:=DSFGKONVERR_INVCRC;  // ungültiges CRC
        exit;
      end;
    except
      KonvErr_Ext:=DSFGKONVERR_INVCRC;  // ungültiges CRC
      exit;
    end;
  end;
end;

{--------------------------------------------------------}
function GetNextDatenelementAdresse (DEL: string): string;
{--------------------------------------------------------}
{ Ergebnis: liefert nächsthöhere Datenelement-Adresse }
var
  S: string;
  Space_Pos: integer;
  i: integer;

begin
  // mit Space als Platzhalter auffüllen, wenn DE-Adresse nicht volle Länge hat:
  S:=F_LeftTrunc (DEL, ' ');
  S:=Copy (S, 1, CMaxLenDE_Adresse);
  S:=F_RightPad (DEL, ' ', CMaxLenDE_Adresse);

  Space_Pos:=Pos (' ', S);
  if Space_Pos > 0 then  // DE-Adresse hat noch nicht volle Länge
    S[Space_Pos]:=CValidDEAdrCharMin  // nächstniedrigere Stelle beginnt mit Min-Zeichen
  else begin
    for i:=length (S) downto 1 do begin
      if S[i] < CValidDEAdrCharMax then begin
        S[i]:=Chr (Ord (S[i]) + 1);  // nächsthöheres Zeichen
        Break;
      end else
        Delete (S, length (S), 1);  // Stelle löschen
    end;

    if length (S) = 0 then  // alle Stellen gelöscht, Eingangs-Adresse ist bereits max. mögliche Adresse zzz..
      for i:=1 to CMaxLenDE_Adresse do
        S:=S + CValidDEAdrCharMax;
  end;

  Result:=F_RightTrunc (S, ' ');  // abschließende Platzhalter-Spaces wieder entfernen
end;

{--------------------------------------------------------------------------}
function GetNextDatenelementeMenge (DEL_Menge: string; DEL: string): string;
{--------------------------------------------------------------------------}
{ Ergebnis: liefert Rest-Datenelementemenge nach der übergebenen DE-Adresse
  ->  in DEL-Menge müssen die einzelnen DE-Adressen durch <GS> getrennt sein }
var
  DEL_Pos: integer;
  sSearch: string;

begin
  sSearch:=DEL + GS;
  DEL_Pos:=Pos (DEL, DEL_Menge);
  if DEL_Pos > 0 then  // DE-Adresse gefunden
    Result:=Copy (DEL_Menge, DEL_Pos + length (DEL) + 1, length (DEL_Menge))  // Rest ab DEL und <GS>
  else
    Result:='';
end;

{------------------------------------------------------------}
function ReadFile_DSfGDataPart (FStream: TFileOfCharStream): string;  // 06.10.2015, WW
{------------------------------------------------------------}
{ liest Datenteil aus geöffnetem DSfG-Rohdaten-FileStream; Dateiposition auf
  erstem zum Datenteil gehörenden Zeichen
  Übergabe: Filestream
  Ergebnis: Datenteil }
var
  FS_Pos: integer;
  iCount: integer;
  StrS: TStringStream;
  S: string;

begin
  Result:='';
  if not Assigned (FStream) then exit;

  FS_Pos:=FStream.Position;  // Position in der Datei merken

  // Dateiinhalt bis Dateiende in String einlesen
  StrS:=TStringStream.Create ('');
  try
    iCount:=FStream.Size - FS_Pos;
    StrS.CopyFrom (FStream.FileStream, iCount);
    S:=StrS.DataString;  // ..ETX BCC1 BBC2
    Result:=ExtractString (S, NUL, ETX, 0);  // Datenteil endet mit letztem Zeichen vor dem ETX
  finally
    StrS.Free;
  end;

  FStream.Position:=FS_Pos;  // auf gemerkte Position in der Datei zurücksetzen
end;

{----------------------------------------------------------------------------}
procedure GetDSfGPTBDataFromDCL (sPTB: string; var DSfGPTBData: TDSfGPTBData);
{----------------------------------------------------------------------------}
{ Konvertiert das DCL-Feld PTB in DSfGPTBData-Struktur;
  Übergabe: PTB als String
  Rückgabe: DSfGPTBData-Struktur
  Ergebnis: true, wenn Konvertierung OK }
const
  CTrennerPTB = ';';  // Trennzeichen im PTB-Feld

var
  S: string;

begin
  S:=sPTB;
  StrSubst (S, ':', CTrennerPTB);  // wg. RMG ERZ2000 (der verwendet nicht DSfG-konformen
                                   // Doppelpunkt als Trennzeichen; 07.05.2012, WW
  with DSfGPTBData do begin
    Kenner:=ExtractString (S, NUL, CTrennerPTB, 0);
    DatumZeit:=ExtractString (S, CTrennerPTB, CTrennerPTB, 0);
    Signatur_r:=ExtractString (S, CTrennerPTB, CTrennerPTB, 1);
    Signatur_s:=ExtractString (S, CTrennerPTB, NUL, 2);
  end;
end;

End.
