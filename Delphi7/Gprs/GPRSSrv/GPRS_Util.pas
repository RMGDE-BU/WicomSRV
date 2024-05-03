{******************************************************************************}
{* Unit: Hilfsfunktionen für GPRS-Server                                      *}
{* 06.06.2006 WW                                                              *}
{******************************************************************************}
unit GPRS_Util;

interface

uses
  Forms, Classes, SysUtils, WChars, WSysCon, CRC16, Novell, T_Tools, ErrConst;

type
  TCBGPRSDataProc = procedure (sRohdaten: string; RemoteAddress: string;
    RemotePort: integer) of object;

function GetGerTypName (GerTypNr: integer): string;
function Identifiziere_Geraetetyp (sRohdaten: string): integer;
function FIsCompletePushTelegramm (sRohdaten: string; GerTypNr: integer): boolean;
function FCutOnePushTelegramm (var sRohdaten: string; GerTypNr: integer): string;
function FCheckPushTelegramm_Pruefsumme (sRohdaten: string; GerTypNr: integer;
                                         var Fehlergruppe: integer;
                                         var Fehlercode: integer): boolean;

function GetFilename_GPRS_Verbindungen (Pfad: string): string;

implementation

{-------------------------------------------------}
function GetGerTypName (GerTypNr: integer): string;
{-------------------------------------------------}
{ liefert zur übergebenen Gerätetyp-Nummer den Gerätetyp-Namen }
begin
  case GerTypNr of
    mrgtyp_MRG910: Result:='MRG 900';  // MRG 905 oder 910
    mrgtyp_MCO:    Result:='MCO';  // Tritschler MCO
    mrgtyp_TDS:    Result:='TDS';  // Tritschler TDS
  else
    Result:='unbekannt';
  end;
end;

{-------------------------------------------------------------}
function Identifiziere_Geraetetyp (sRohdaten: string): integer;
{-------------------------------------------------------------}
{ ermittelt aus den GPRS-Geräterohdaten den Gerätetyp;
  Übergabe: Rohdaten
  Ergebnis: Gerätetypnummer (-1: unbekannter Gerätetyp}
var
  iPos: integer;
  S: string;

begin
  Result:=-1;  // unbekannter Gerätetyp

  if (Pos (STX + 'p', sRohdaten) > 0) OR
     (Pos (STX + 'q', sRohdaten) > 0) OR                              
     (Pos (STX + 'r', sRohdaten) > 0) OR
     (Pos (STX + 'v', sRohdaten) > 0) then begin
    Result:=mrgtyp_MRG910;  // MRG 905, MRG 910
    exit;
  end;

  iPos:=Pos ('/FTL', sRohdaten);
  if iPos > 0 then begin
    // Gerätetypkürzel analysieren:
    if length (sRohdaten) >= (iPos + 7) then begin
      S:=Copy (sRohdaten, 5, 3);  // Gerätetypkürzel
      if S = 'MCO' then
        Result:=mrgtyp_MCO  // Tritschler MCO
      else if S = 'TDS' then
        Result:=mrgtyp_TDS;  // Tritschler TDS
    end;
    exit;
  end;
end;

{--------------------------------------------------------------------------------}
function FIsCompletePushTelegramm (sRohdaten: string; GerTypNr: integer): boolean;
{--------------------------------------------------------------------------------}
{ prüft auf vollständiges GPRS-Push-Telegramm;
  Übergabe: Rohdaten
            Gerätetypnummer
  Ergebnis: true, wenn Push-Telegramm vollständig ist }
var
  P_Beginn: integer;
  P_Ende: integer;

begin
  case GerTypNr of
    mrgtyp_MRG910:
      begin
        P_Ende:=Pos(ETX, sRohdaten);
        Result:=P_Ende > 0;  // mit ETX ist das Telegramm abgeschlossen (eagl, ob mit oder ohne CRC)
      end;

    mrgtyp_TDS, mrgtyp_MCO:
      begin
        // Prüfung, ob ein Datentelegrammheader vorliegt
        P_Beginn:=Pos ('/FTL', sRohdaten);  // Header beginnt mit /FTL
        if P_Beginn > 0 then begin
          P_Ende:=Pos (CR + LF, sRohdaten);  // Header endet mit CR LF
          Result:=length (sRohdaten) >= (P_Ende + 1);
          exit;
        end;

        // Prüfung, ob ein Datentelegramm vorliegt
        P_Beginn:=Pos (STX, sRohdaten);  // Datentelegramm beginnt mit STX
        if P_Beginn > 0 then begin
          P_Ende:=Pos (ETX, sRohdaten);  // Datentelegramm endet mit ETX BCC
          Result:=length (sRohdaten) >= (P_Ende + 1);
          exit;
        end;

        // Prüfung, ob ein Anforderungstelegramm vorliegt
        P_Beginn:=Pos ('/?', sRohdaten);  // Anforderungstelegramm beginnt mit /?
        if P_Beginn > 0 then begin
          P_Ende:=Pos (CR + LF, sRohdaten);  // Anforderungstelegramm endet mit CR LF
          Result:=length (sRohdaten) >= (P_Ende + 1);
          exit;
        end;

        Result:=false;
      end;
  else
    Result:=false;
  end;
end;

{-------------------------------------------------------------------------------}
function FCutOnePushTelegramm (var sRohdaten: string; GerTypNr: integer): string;
{-------------------------------------------------------------------------------}
{ liefert das erste in GPRS-Push-Rohdaten enthaltene Telegramm (Ergebnis) und
  schneidet dieses aus sRohdaten aus (Rückgabe)
  Übergabe: Gerätetypnummer }
var
  P_Beginn: integer;
  P_Ende: integer;
  Len: integer;
  LenCRC: integer;

begin
  case GerTypNr of
    mrgtyp_MRG910:
      begin
        LenCRC:=0;
        Len:=length (sRohdaten);
        if Len > 0 then
          if sRohdaten [Len] <> ETX then
            LenCRC:=4;

        P_Ende:=Pos (ETX, sRohdaten);
        if P_Ende = 0 then
          P_Ende:=length (sRohdaten);
        { ein MRG-Telegramm von evtl. mehreren enthaltenen rauskopieren: }
        Result:=Copy (sRohdaten, 1, P_Ende + LenCRC);
        Delete (sRohdaten, 1, P_Ende + LenCRC);   { aus Gesamt-Antwort löschen }
      end;

    mrgtyp_TDS, mrgtyp_MCO:
      begin
        // Prüfung, ob ein Datentelegrammheader vorliegt
        P_Beginn:=Pos ('/FTL', sRohdaten);  // Header beginnt mit /FTL
        if P_Beginn > 0 then begin
          P_Ende:=Pos (CR + LF, sRohdaten);  // Header endet mit CR LF
          if P_Ende = 0 then
            P_Ende:=length (sRohdaten);
          { ein MRG-Telegramm von evtl. mehreren enthaltenen rauskopieren: }
          Result:=Copy (sRohdaten, 1, P_Ende + 1);
          Delete (sRohdaten, 1, P_Ende + 1);   { aus Gesamt-Antwort löschen }
          exit;
        end;

        // Prüfung, ob ein Datentelegramm vorliegt
        P_Beginn:=Pos (STX, sRohdaten);  // Datentelegramm beginnt mit STX
        if P_Beginn > 0 then begin
          P_Ende:=Pos (ETX, sRohdaten);  // Datentelegramm endet mit ETX BCC
          if P_Ende = 0 then
            P_Ende:=length (sRohdaten);
          { ein MRG-Telegramm von evtl. mehreren enthaltenen rauskopieren: }
          Result:=Copy (sRohdaten, 1, P_Ende + 1);
          Delete (sRohdaten, 1, P_Ende + 1);   { aus Gesamt-Antwort löschen }
          exit;
        end;

        // Prüfung, ob ein Anforderungstelegramm vorliegt
        P_Beginn:=Pos ('/?', sRohdaten);  // Anforderungstelegramm beginnt mit /?
        if P_Beginn > 0 then begin
          P_Ende:=Pos (CR + LF, sRohdaten);  // Anforderungstelegramm endet mit CR LF
          if P_Ende = 0 then
            P_Ende:=length (sRohdaten);
          { ein MRG-Telegramm von evtl. mehreren enthaltenen rauskopieren: }
          Result:=Copy (sRohdaten, 1, P_Ende + 1);
          Delete (sRohdaten, 1, P_Ende + 1);   { aus Gesamt-Antwort löschen }
          exit;
        end;

        // unbekanntes TDS/MCO-Telegramm liegt vor
        Result:=sRohdaten;
        sRohdaten:='';
      end;
  else
    Result:=sRohdaten;
    sRohdaten:='';
  end;
end;

{----------------------------------------------------------------------------}
function FCheckPushTelegramm_Pruefsumme (sRohdaten: string; GerTypNr: integer;
                                         var Fehlergruppe: integer;
                                         var Fehlercode: integer): boolean;
{----------------------------------------------------------------------------}
{ Prüfsumme eines GPRS-Push-Telegramms überprüfen;
  Übergabe: Rohdaten
            Gerätetypnummer
  Rückgabe: Fehlergruppe
            Fehlercode
  Ergebnis: false, wenn Prüfsumme falsch }
var
  Len: integer;
  S: string;
  sCRC: string;
  sCRC_calc: string;
  CRC_calc: word;
  BCC: byte;
  pruefen: boolean;
  i: integer;

begin
  Result:=true;
  // Vorbelegung für Rückgabe: OK
  Fehlergruppe:=0;
  Fehlercode:=0;

  case GerTypNr of
    mrgtyp_MRG910:  // CRC-Prüfung
      begin
        S:=sRohdaten;
        { alles bis zum STX wegschneiden, gehört nicht zum Telegramm: }
        while (length (S) > 0) AND (S [1] <> STX) do
          Delete (S, 1, 1);

        Len:=length (S);
        if Len > 0 then begin
          if S [Len] <> ETX then begin  // nur, wenn CRC im Telegramm enthalten ist
            sCRC:=Copy (S, Len-3, Len);

            CRC_calc:=scrc16 (S, 0);
            sCRC_calc:=GetCRC16_Chars_Hex (CRC_calc);
            if UpperCase (sCRC) <> UpperCase (sCRC_calc) then begin  // CRC-Fehler in der Antwort
              Fehlergruppe:=COM_KOMMERROR;
              Fehlercode:=KOMMERR_CRC;
              Result:=false;
            end;
          end;
        end;
      end;

    mrgtyp_TDS, mrgtyp_MCO:  // BCC-Prüfung
      begin
        S:=sRohdaten;
        pruefen:=false;
        BCC:=0;
        for i:=1 to length (S)-1 do begin
          if pruefen then
            BCC:=BCC XOR ord (S [i]);
          if (S [i] = SOH) OR (S [i] = STX) then   { Startzeichen: STX oder SOH }
            pruefen:=true;
        end;

        if Ord (S [length(S)]) <> BCC then begin
          Fehlergruppe:=COM_KOMMERROR;
          Fehlercode:=KOMMERR_BCC;
          Result:=false;
        end;
      end;
  end;  { case GerTypNr }
end;

{------------------------------------------------------------}
function GetFilename_GPRS_Verbindungen (Pfad: string): string;
{------------------------------------------------------------}
{ liefert Dateiname für GPRS-Verbindungsdaten (ASCII);
  Struktur der Einträge (für jeden Client/Gerät ein Eintrag):
    IP-Adresse <US> Port <US> Verbindung aktiv (J/N) <US> Anzahl empfangener Telegramme <US>
    Anzahl empfangener Blöcke <US> Anzahl empfangener Bytes <US>
    Anzahl geöffneter Verbindungen <US> Anzahl geschlossener Verbindungen <US>
    Kennung <US> GerTypName <US>
    Anzahl gesendeter Telegramme <US> Anzahl gesendeter Bytes }
begin
  Result:=IncludeTrailingBackslash(Pfad) + 'GPRS_VB.DAT';
end;

end.


