{******************************************************************************}
{* Unit: MRG-Befehle bilden                                                   *}
{* 19.12.2000 WW                                                              *}
{******************************************************************************}
unit MrgBefehl;

interface

uses
  SysUtils, WChars, WStrUtils;

const
  { Kennzahlen für den Messwertabruf von Geräten des Typs Datacon FWU (Lastprofile): }
  FWU_MessAbruf_Kennzahlen: array [1..16] of string = (
    '7-1z:V.b.p',           { Vb Z, Tarifgerät 1 }
    '7-1m:V.b.p',           { Vb MU, Tarifgerät 1 }
    '7-1m:V.n.p',           { Vn MU, Tarifgerät 1 }
    '7-1j:V.b.p',           { Vb Z Impuls, FWU MS 1 }
    '7-1i:V.b.p',           { Vb MU Impuls, FWU MS 1 }
    '7-1i:V.n.p',           { Vn MU Impuls, FWU MS 1 }
    '7-1X:T.0.p',           { Temperatur, FWU MS 1 }
    '7-1X:P.0.p',           { Druck, FWU MS 1 }
    '7-2z:V.b.p',           { Vb Z, Tarifgerät 2 }
    '7-2m:V.b.p',           { Vb MU, Tarifgerät 2 }
    '7-2m:V.n.p',           { Vn MU, Tarifgerät 2 }
    '7-2j:V.b.p',           { Vb Z Impuls, FWU MS 2 }
    '7-2i:V.b.p',           { Vb MU Impuls, FWU MS 2 }
    '7-2i:V.n.p',           { Vn MU Impuls, FWU MS 2 }
    '7-2X:T.0.p',           { Temperatur, FWU MS 2 }
    '7-2X:P.0.p');          { Druck, FWU MS 2 }

  { Kennzahlen für den Tagessatzabruf von Geräten des Typs Datacon FWU (Tages- bzw. Revisionswerte): }
  FWU_TagAbruf_Kennzahlen: array [1..10] of string = (
    '7-1M:V.b.t',           { Vb, MU1 }
    '7-1M:V.n.t',           { Vn, MU1 }
    '7-1z:V.b.t',           { Vb Z, Tarifgerät 1 }
    '7-1m:V.b.t',           { Vb MU, Tarifgerät 1 }
    '7-1m:V.n.t',           { Vn MU, Tarifgerät 1 }
    '7-2M:V.b.t',           { Vb, MU2 }                    
    '7-2M:V.n.t',           { Vn, MU2 }
    '7-2z:V.b.t',           { Vb Z, Tarifgerät 2 }
    '7-2m:V.b.t',           { Vb MU, Tarifgerät 2 }
    '7-2m:V.n.t');          { Vn MU, Tarifgerät 2 }

type

  { Record mit Informationen für MRG-Datenabruf-Befehl }

  TAbrufRec = record
    KanalMaske : string;
    AlleDaten : Boolean;
    vonJahr : Word;
    vonMonat : Word;
    vonTag : Word;
    vonStunde : Word;
    vonMinute : Word;
    vonSekunde : Word;
    bisJahr : Word;
    bisMonat : Word;
    bisTag : Word;
    bisStunde : Word;
    bisMinute : Word;
    bisSekunde : Word;
  End;


procedure GetMRGKommando_Archiv (AbrufRec: TAbrufRec;
                                 KommandoTyp: string;
                                 Abrufkommando_Def: string;
                                 Abrufkommando_alle_Daten_Def: string;
                                 Jahreswechsel: boolean;
                                 var MRGKommando: string);
function GetMRGKommando_B (ParaNr: string): string;
function GetMRGKommando_C (ParaNr: string; NeuerWert: string): string;
function GetMRGKommando_k: string;

function GetLIS200Kommando_Archiv (AbrufRec: TAbrufRec; ArchivNr: integer): string;
function GetLIS200Kommando_Lesen (DatenAdresse: string): string;
function GetLIS200Kommando_Schreiben (DatenAdresse: string; NeuerWert: string): string;

function GetKE_Kommando (Kommandostr: string): string;
function GetKE_Kommando_Zeitbereich_von (AbrufRec: TAbrufRec): string;
function GetKE_Kommando_Zeitbereich_bis (AbrufRec: TAbrufRec): string;

function GetFWU_Kommando_Archiv (AbrufRec: TAbrufRec; KennzifferStr: string): string;

implementation

{--------------------- Befehlsroutinen für Wieser-Geräte ----------------------}

{---------------------------------------------------------------------}
procedure CorrectAbrufRec (KommandoTyp: string; Jahreswechsel: boolean;
                           var AbrufRec: TAbrufRec);
{---------------------------------------------------------------------}
{ Korrektur des Abrufzeitraums;
  Übergabe: Kommandotyp-Zeichen
            Jahreswechsel (true = Gerät kann über den Jahreswechsel hinaus abgerufen werden)
            AbrufRec
  Rückgabe: korrigierter AbrufRec }
begin
  { Meßwerte: }
  if KommandoTyp = 'E' then begin
    { Wenn ein MRG nicht über einen Jahreswechsel hinaus abgefragt werden kann,
      wird nur bis zum Jahresende gelesen: }
    if not Jahreswechsel then begin
      if AbrufRec.bisJahr > AbrufRec.vonJahr then begin
        AbrufRec.bisJahr := AbrufRec.vonJahr;
        AbrufRec.bisMonat := 12;
        AbrufRec.bisTag := 31;
        AbrufRec.bisStunde := 23;
        AbrufRec.bisMinute := 59;
        AbrufRec.bisSekunde := 59;
      end;
    end;
  end;

  { Meldungen: }
  if KommandoTyp = 'M' then begin
    { bei Jahrtausendwechsel: alle Meldungen }
    if (AbrufRec.vonJahr < 2000) AND (AbrufRec.bisJahr >= 2000) then
      AbrufRec.AlleDaten := True;
  end;

  { Prüfungssätze: }
  if KommandoTyp = 'X' then begin
    { bei Jahrtausendwechsel: alle Prüfungssätze }
    if (AbrufRec.vonJahr < 2000) AND (AbrufRec.bisJahr >= 2000) then
      AbrufRec.AlleDaten := True;
  end;
end;

{------------------------------------------------------------------------------}
function GetMaskString (Const AbrufRec: TAbrufRec; Const Maske: string): string;
{------------------------------------------------------------------------------}
Var
  W: Word;
  S: string;
Begin
  S:='';
  W := 0;
  If Length (Maske) > 0 Then Begin
    Case Maske [1] of
      'J', 'M', 'T', 'h', 'm', 's':
        Begin
          If Length (Maske) > 1 Then Begin
            Case Maske [2] of
              'v' : Begin
                      Case Maske [1] of
                        'J': W:=AbrufRec.vonJahr MOD 100;
                        'M': W:=AbrufRec.vonMonat;
                        'T': W:=AbrufRec.vonTag;
                        'h': W:=AbrufRec.vonStunde;
                        'm': W:=AbrufRec.vonMinute;
                        's': W:=AbrufRec.vonSekunde;
                      End;
                      S:=Format('%.2d', [W]);
                    End;
              'b' : Begin
                      Case Maske [1] of
                        'J': W:=AbrufRec.bisJahr MOD 100;
                        'M': W:=AbrufRec.bisMonat;
                        'T': W:=AbrufRec.bisTag;
                        'h': W:=AbrufRec.bisStunde;
                        'm': W:=AbrufRec.bisMinute;
                        's': W:=AbrufRec.bisSekunde;
                      End;
                      S:=Format('%.2d', [W]);
                    End;
            End;
          End;
        End;

      'K': Begin
             S:=AbrufRec.KanalMaske;
           End;
    End;
  End;
  Result:=S;
End;

{--------------------------------------------------------------------------------------}
function BuildMRGCommand (const AbrufRec: TAbrufRec; const KommandoDef: string): string;
{--------------------------------------------------------------------------------------}
Const
  m_copy = 0;
  m_mask = 1;
Var
  i : Byte;
  Modus : Word;
  Buffer : String;
  MRGKommando: string;
Begin
  MRGKommando:='';
  Modus := m_copy;
  For i := 1 to Length (KommandoDef) Do Begin
    Case Modus of
      m_copy :
        Begin
          If KommandoDef [i] = '<' Then Begin
            Modus := m_mask;
            Buffer := '';
          End Else
            MRGKommando:=MRGKommando + Copy (KommandoDef, i, 1);
        End;
      m_mask :
        Begin
          If KommandoDef [i] = '>' Then Begin
            Modus := m_copy;
            MRGKommando:=MRGKommando + GetMaskString (AbrufRec, Buffer);
          End Else
            Buffer := Buffer + Copy (KommandoDef, i, 1);
        End;
    End;
  End;
  if length (MRGKommando) > 0 then
    MRGKommando:=STX + MRGKommando + ETX;
  Result:=MRGKommando;
End;

{----------------------------------------------------------------------}
procedure GetMRGKommando_Archiv (AbrufRec: TAbrufRec;
                                 KommandoTyp: string;
                                 Abrufkommando_Def: string;
                                 Abrufkommando_alle_Daten_Def: string;
                                 Jahreswechsel: boolean;
                                 var MRGKommando: string);
{----------------------------------------------------------------------}
{ gerätespezifischen MRG-Datenabruf-Befehl für Meldungen, Meßwerte, Tagessätze
  oder Prüfungssätze bilden;
  Übergabe: AbrufRec
            Kommandotyp-Zeichen
            Abrufkommando-Definitionsmaske (aus Konfigurationsdatei MrgAbruf.DB bzw. MrgAbruf.dat)
            Abrufkommando-Definitionsmaske für "alle Daten" (aus Konfigurationsdatei MrgAbruf.DB bzw. MrgAbruf.dat)
            Jahreswechsel (true = Gerät kann über den Jahreswechsel hinaus abgerufen werden)
  Rückgabe: MRGKommando }
begin
  CorrectAbrufRec (KommandoTyp, Jahreswechsel, AbrufRec);
  if AbrufRec.AlleDaten then
    MRGKommando:=BuildMRGCommand (AbrufRec, Abrufkommando_alle_Daten_Def)
  else
    MRGKommando:=BuildMRGCommand (AbrufRec, Abrufkommando_Def);
end;

{-------------------------------------------------}
function GetMRGKommando_B (ParaNr: string): string;
{-------------------------------------------------}
{ MRG-Abrufbefehl für Parameter bilden;
  Übergabe: ParaNr (3-stellige MRG-Parameternummer oder
                    Leer-String für alle Parameter)
  Ergebnis: B-Befehlstring }
begin
  Result:=STX+'B'+ParaNr+ETX;
end;

{--------------------------------------------------------------------}
function GetMRGKommando_C (ParaNr: string; NeuerWert: string): string;
{--------------------------------------------------------------------}
{ MRG-Befehl zum Einstellen eines Parameters bilden;
  Übergabe: ParaNr (3-stellige MRG-Parameternummer)
            neuer Parameterwert
  Ergebnis: C-Befehlstring }
begin
  Result:=STX+'C'+ParaNr+NeuerWert+ETX;
end;

{--------------------------------}
function GetMRGKommando_k: string;
{--------------------------------}
{ MRG-Abrufbefehl für Kennung bilden (bei MRG 910 mit Anruf-Funktion);
  -> Damit kann die Kennung ohne Kenntnis des im Gerät eingestellten Passworts
     abgerufen werden !
  Ergebnis: k-Befehlstring }
begin
  Result:=STX+'k'+ETX;
end;


{-------------- Befehlsroutinen für Elster-Geräte (LIS-200) -------------------}

{---------------------------------------------------------------------------------}
function GetLIS200Kommando_Archiv (AbrufRec: TAbrufRec; ArchivNr: integer): string;
{---------------------------------------------------------------------------------}
{ LIS-200-Archiv-Abrufbefehl bilden;
  Übergabe: AbrufRec
            Archivnummer
  Ergebnis: Kommando-String }
begin
  with AbrufRec do begin
    if AlleDaten then
      Result:=Format ('%sR3%s%d:V.0(;;;1)%s', [SOH, STX, ArchivNr, ETX])
    else
      Result:=Format ('%sR3%s%d:V.0(3;%.4d-%.2d-%.2d,%.2d:%.2d:%.2d;%.4d-%.2d-%.2d,%.2d:%.2d:%.2d;1)%s',
                      [SOH, STX, ArchivNr,
                       vonJahr, vonMonat, vonTag,
                       vonStunde,vonMinute, vonSekunde,
                       bisJahr, bisMonat, bisTag,
                       bisStunde, bisMinute, bisSekunde,
                       ETX]);
  end;
end;

{--------------------------------------------------------------}
function GetLIS200Kommando_Lesen (DatenAdresse: string): string;
{--------------------------------------------------------------}
{ LIS-200-Daten-Lesebefehl bilden;
  Übergabe: Daten-Adresse (z.B. 3:180 für Stationsnummer)
  Ergebnis: Kommando-String }
begin
  Result:=SOH+'R1'+STX+DatenAdresse+'.0(1)'+ETX;
end;

{-------------------------------------------------------------------------------------}
function GetLIS200Kommando_Schreiben (DatenAdresse: string; NeuerWert: string): string;
{-------------------------------------------------------------------------------------}
{ LIS-200-Daten-Schreibbefehl bilden (Parametrierung);
  Übergabe: Daten-Adresse (z.B. 3:180 für Stationsnummer)
            neuer Wert
  Ergebnis: Kommando-String }
begin
  Result:=SOH+'W1'+STX+DatenAdresse+'.0('+NeuerWert+')'+ETX;
end;


{--------------------- Befehlsroutinen für KE-Anlagen -------------------------}

{----------------------------------------------------}
function GetKE_Kommando (Kommandostr: string): string;
{----------------------------------------------------}
{ KE-Abrufbefehl bilden;
  Übergabe: Kommando-String (Kommandobuchstabe und Nummer)
  Ergebnis: KE-Befehlstring }
begin
  Result:=STX+KommandoStr+ETX;
end;

{--------------------------------------------------------------------}
function GetKE_Kommando_Zeitbereich_von (AbrufRec: TAbrufRec): string;
{--------------------------------------------------------------------}
{ KE-Befehl zum Setzen des von-Auslesezeitraums bilden;
  Übergabe: AbrufRec
  Ergebnis: Kommando-String }
var
  Jahr: integer;
  Monat: integer;
  Tag: integer;
  Stunde: integer;
  Minute: integer;
  Sekunde: integer;
  JahrBuf: integer;

begin
  with AbrufRec do begin
    if AlleDaten then begin
      { Ersatz-von-Zeitpunkt für "alle Daten" bilden: ab 1.1.2000 }
      Jahr:=2000;
      Monat:=1;
      Tag:=1;
      Stunde:=0;
      Minute:=0;
      Sekunde:=0;
    end
    else begin
      Jahr:=vonJahr;
      Monat:=vonMonat;
      Tag:=vonTag;
      Stunde:=vonStunde;
      Minute:=vonMinute;
      Sekunde:=vonSekunde;
    end;
  end;

  { Jahr in C-Format wandeln (z.B. 2003 -> 103): }
  JahrBuf:=Jahr MOD 100;
  if Jahr >= 2000 then
    JahrBuf:=JahrBuf + 100;

  Result:=Format ('%sP201=%.3d/%.2d/%.2d/%.2d/%.2d/%.2d%s',
                  [STX, JahrBuf, Monat, Tag, Stunde, Minute, Sekunde, ETX]);
end;

{--------------------------------------------------------------------}
function GetKE_Kommando_Zeitbereich_bis (AbrufRec: TAbrufRec): string;
{--------------------------------------------------------------------}
{ KE-Befehl zum Setzen des bis-Auslesezeitraums bilden;
  Übergabe: AbrufRec
  Ergebnis: Kommando-String }
var
  Jahr: word;
  Monat: word;
  Tag: word;
  Stunde: word;
  Minute: word;
  Sekunde: word;
  dummy: word;
  JahrBuf: integer;

begin
  with AbrufRec do begin
    if AlleDaten then begin
      { Ersatz-bis-Zeitpunkt für "alle Daten" bilden: aktuelle PC-Zeit }
      DecodeDate (Date,  Jahr, Monat, Tag);
      DecodeTime (Time, Stunde, Minute, Sekunde, dummy);
    end
    else begin
      Jahr:=bisJahr;
      Monat:=bisMonat;
      Tag:=bisTag;
      Stunde:=bisStunde;
      Minute:=bisMinute;
      Sekunde:=bisSekunde;
    end;
  end;

  { Jahr in C-Format wandeln (z.B. 2003 -> 103): }
  JahrBuf:=Jahr MOD 100;
  if Jahr >= 2000 then
    JahrBuf:=JahrBuf + 100;

  Result:=Format ('%sP202=%.3d/%.2d/%.2d/%.2d/%.2d/%.2d%s',
                  [STX, JahrBuf, Monat, Tag, Stunde, Minute, Sekunde, ETX]);
end;


{------------------ Befehlsroutinen für Datacon-Geräte (FWU) ------------------}

{-----------------------------------------------------------------------------------}
function GetFWU_Kommando_Archiv (AbrufRec: TAbrufRec; KennzifferStr: string): string;
{-----------------------------------------------------------------------------------}
{ FWU-Datenlesebefehl bilden;
  Übergabe: AbrufRec
            Kennziffer (z.B. 7-1m:V.b.p für Werte-Profil Vb MU, Tarifgerät 1)
  Ergebnis: Kommando-String }
var
  JahrBuf_von: integer;
  MonatBuf_von: integer;
  TagBuf_von: integer;
  JahrBuf_bis: word;
  MonatBuf_bis: word;
  TagBuf_bis: word;

begin
  with AbrufRec do begin
    if AlleDaten then begin
      { Ersatz-von-Zeitpunkt für "alle Daten" bilden: ab 1.1.2000 }
      JahrBuf_von:=2000;
      MonatBuf_von:=1;
      TagBuf_von:=1;
      { Ersatz-bis-Zeitpunkt für "alle Daten" bilden: aktuelle PC-Zeit }
      DecodeDate (Date, JahrBuf_bis, MonatBuf_bis, TagBuf_bis);
    end
    else begin
      JahrBuf_von:=vonJahr;
      MonatBuf_von:=vonMonat;
      TagBuf_von:=vonTag;
      JahrBuf_bis:=bisJahr;
      MonatBuf_bis:=bisMonat;
      TagBuf_bis:=bisTag;
    end;
  end;

  { von/bis-Jahr zweistellig }
  JahrBuf_von:=JahrBuf_von MOD 100;
  JahrBuf_bis:=JahrBuf_bis MOD 100;
  Result:=Format ('/?%s;%.2d%.2d%.2d;%.2d%.2d%.2d!%s',
                  [KennzifferStr,
                   TagBuf_von, MonatBuf_von, Jahrbuf_von,
                   TagBuf_bis, MonatBuf_bis, Jahrbuf_bis,
                   CR + LF]);
end;

end.
