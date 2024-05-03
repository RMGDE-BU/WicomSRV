{******************************************************************************}
{* Unit: Abruf-Kommandos auf Plausibilit�t pr�fen                             *}
{* 19.12.2002  WW                                                             *}
{******************************************************************************}
unit AbrufCmdPlausib;

interface

uses
  SysUtils, WStrUtils, WChars, AbrufCmd, T_Zeit;

function AbrufKommando_plausibel (KommandoTyp: TKommandoTyp; Kommando: string): boolean;

implementation

{-----------------------------------------------------------------}
function Kommando_VerbAufbau_plausibel (Kommando: string): boolean;
{-----------------------------------------------------------------}
{ Verbindungsaufbau-Kommando auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
  isDSfG: boolean;
  MaxLen: integer;
  KommandoBuf: string;
  sWZSZ: string;
  sUtcOffset: string;
  dummy: TDateTime;

begin
  Result:=false;

  { optional im Kommando enthaltene DSfG-ZeitSync-Kommandodaten aus Kommando-String extrahieren
    -> geblockt durch Pipe-Zeichen: |ZeitSync-Kommandodaten| }
  KommandoBuf:=ExtractString (Kommando, C_CmdSeparatorDyn, C_CmdSeparatorDyn, 0);   { DSfG-ZeitSync-Kommandodaten }
  if length (KommandoBuf) > 0 then begin
    S:=ExtractString (KommandoBuf, NUL, C_CmdSeparator, 0);             { ZeitSync aktiv }
    if StrToIntDef (S, -1) < 0 then exit;

    S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 0);  { Zeitbasis im Ger�t }
    sWZSZ:=Copy (S, 1, 1);
    if (sWZSZ <> C_CmdZSyncBasis_W) AND (sWZSZ <> C_CmdZSyncBasis_S) then exit;
    // ab 24.02.2020, WW: Zus�tzlich mit optionalem UTC-Offset
    sUtcOffset:=Copy (S, 2, length (S));
    if length (sUtcOffset) > 0  then begin
      if sUtcOffset[1] in ['+', '-'] then
        sUtcOffset:=Copy (sUtcOffset, 2, length (sUtcOffset));  // Vorzeichen ist optional

      if length (sUtcOffset) <> 4 then exit;
      if not EncodeTimeStr (sUtcOffset, 'HHMM', dummy) then exit;  // g�ltiger Zeit-String ?
    end;

    S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 1);  { min. Abweichung in s }
    if length (S) > C_CmdZSyncAbweichLen then exit;
    if not isIntString (S) then exit;

    S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 2);  { max. Abweichung in s }
    if length (S) > C_CmdZSyncAbweichLen then exit;
    if not isIntString (S) then exit;
  end;

  { Kommandoteil mit DSfG-ZeitSync-Kommandodaten kann jetzt abgeschnitten werden: }
  S:=Kommando;
  KommandoBuf:=F_Zerlegen (S, C_CmdSeparatorDyn);

  isDSfG:=false;
  { pr�fen auf Ger�tetyp "DSfG" (leer oder DSfG-Typnummer) }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 4);   { Ger�te-Typ }
  if length (S) > 0 then begin
    if length (S) <> C_CmdGeraeteTypLen then exit;
    if not isIntString (S) then exit;
    if StrToInt (S) = C_GeraeteTypDSfG then
      isDSfG:=true;
  end else
    isDSfG:=true;

  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 2);   { Kennung }
  if isDSfG then
    MaxLen:=C_MaxDSfGKennungLen
  else
    MaxLen:=C_MaxMRGKennungLen;
  if length (S) > MaxLen then exit;

  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 3); { Rufnummer }
  if length (S) > C_CmdRufnummerLen then exit;  // leere Rufnummer erlaubt wegen GPRS; 26.02.2009

  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 5);  { Passwort }
  if isDSfG then
    MaxLen:=C_MaxDSfGPasswortLen
  else
    MaxLen:=C_MaxMRGPasswortLen;
  if (length (S) = 0) OR (length (S) > MaxLen) then exit;

  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 6);  { Passwort-Nummer }
  if not isDSfG then begin       { Passwort-Nummer ist f�r DSfG-Abruf nicht relevant }
    if length (S) <> C_CmdPasswortNrLen then exit;
    if not isIntString (S) then exit;
  end;

  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 7);  { Kennung pr�fen J/N }
  if length (S) <> C_CmdKennPruefLen then exit;

  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 8); { Modemtyp }
  if not isDSfG then begin { (FUP-)Modemtyp ist f�r DSfG-Abruf nicht relevant }
    if (length (S) = 0) OR (length (S) > C_CmdModemTypLen) then exit;
    // Pr�fung auf Integer-String entf�llt jetzt wegen Umstellung auf internen
    // Feldtyp 'string' f�r erweiterten �bergabe-Wertebereich f�r Schnittstellen-
    // Parameter-Definition; 21.04.2006, WW
  end;

  { optionales Feld "DSfG-DF� transparent" (leer erlaubt wegen Kompatibilit�t zu
    fr�heren Versionen): }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 9);  { DSfG-DF� transparent J/N }
  if isDSfG then { "DSfG-DF� transparent" ist nur f�r DSfG-Abruf relevant }
    if length (S) > C_CmdTransparentModusLen then exit;

  { optionales Feld "Funktionscode" (leer erlaubt wegen Kompatibilit�t zu
    fr�heren Versionen): }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 10);  { Funktionscode }
  if length (S) > 0 then begin
    if length (S) > C_CmdFktCodeLen then exit;
    if not isIntString (S) then exit;
  end;

  { optionales Feld "TraceLog" (leer erlaubt wegen Kompatibilit�t zu
    fr�heren Versionen): }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 11);  { TraceLog }
  if length (S) > 0 then begin
    if length (S) > C_CmdTraceLogLen then exit;
    if not isIntString (S) then exit;
  end;

  Result:=true;
end;

{----------------------------------------------------------------}
function Kommando_VerbAbbau_plausibel (Kommando: string): boolean;
{----------------------------------------------------------------}
{ Verbindungsabbau-Kommando auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
begin
  Result:=false;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 2);  { Kennung }
  if length (S) > C_CmdKennungLen then exit;

  Result:=true;
end;

{---------------------------------------------------------------------------}
function Kommando_Mess_Meld_PruefAbruf_plausibel (Kommando: string): boolean;
{---------------------------------------------------------------------------}
{ Kommando f�r Messwert-, Meldungs- oder Pr�fsatzabruf auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
  dummy: TDateTime;
  vonTagStr: string;
  vonOrdNrStr: string;
  bisOrdNrStr: string;
  isOrdNrCmd: boolean;
  KommandoBuf: string;

begin
  Result:=false;

  { Kommandoteil mit DSfG-Adressliste abschneiden, wird nicht gepr�ft: }
  S:=Kommando;
  KommandoBuf:=F_Zerlegen (S, C_CmdSeparatorDyn);

  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 2);  { Kennung }
  if length (S) > C_CmdKennungLen then exit;

  vonOrdNrStr:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 7);
  bisOrdNrStr:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 8);
  { optionaler Ordnungsnummern-Eintrag geht vor Datum/Zeit-Eintrag: }
  isOrdNrCmd:=(length (vonOrdNrStr) > 0) AND (length (bisOrdNrStr) > 0);
  if isOrdNrCmd then begin
    { von-Ordnungsnummer: }
    if not isIntString (vonOrdNrStr) then exit;
    if StrToInt (vonOrdNrStr) < 0 then exit;  { keine negativen Ordnungsnummern erlaubt }
    { bis-Ordnungsnummer: }
    if not isIntString (bisOrdNrStr) then exit;
    if StrToInt (bisOrdNrStr) < 0 then exit;  { keine negativen Ordnungsnummern erlaubt }

    { von-bis-Datum/Zeit braucht nicht gepr�ft werden }
  end
  else begin
    S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 3);  { von-Datum }
    if length (S) <> C_CmdDatumLen then exit;
    vonTagStr:=Copy (S, 7, 2);
    if vonTagStr <> '00' then     { von-Tag 0 erlaubt -> bedeutet alles abrufen }
      if not EncodeDateStr (S, 'YYYYMMDD', dummy) then exit;  { g�ltiger Datum-String ? }

    S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 4);  { von-Zeit }
    if length (S) <> C_CmdZeitLen then exit;
    if not EncodeTimeStr (S, 'HHMMSS', dummy) then exit;  { g�ltiger Zeit-String ? }

    S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 5);  { bis-Datum }
    if length (S) <> C_CmdDatumLen then exit;
    if not EncodeDateStr (S, 'YYYYMMDD', dummy) then exit;  { g�ltiger Datum-String ? }

    S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 6);  { bis-Zeit }
    if length (S) <> C_CmdZeitLen then exit;
    if not EncodeTimeStr (S, 'HHMMSS', dummy) then exit;  { g�ltiger Zeit-String ? }

    { von-bis-Ordnungsnummer braucht nicht gepr�ft werden }
  end;

  { Kanalaktiv-Maske ist optional f�r MRG-Messwertabruf, darf auch leer sein
    (= alle Kan�le aktiv) und braucht daher nicht extra auf Plausibilit�t gepr�ft werden }

  { Archivdatentyp ist optional f�r MRG-Messwertabruf, darf auch leer sein: }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 10);    { Archivdaten-Typ }
  if (length (S) > 0) AND not isIntString (S) then exit;

  { Dateiname und Dateiname_TA sind optional f�r MRG-Abruf, darf auch leer sein
    und braucht daher nicht extra auf Plausibilit�t gepr�ft werden }

  Result:=true;
end;

{----------------------------------------------------------------}
function Kommando_ParaAbruf_plausibel (Kommando: string): boolean;
{----------------------------------------------------------------}
{ Kommando f�r Parameterabruf auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
  KommandoBuf: string;
  iSepNr_Timeout: integer;

begin
  Result:=false;

  { Kommandoteil mit DSfG-Adressliste abschneiden, wird nicht gepr�ft: }
  S:=Kommando;
  KommandoBuf:=F_Zerlegen (S, C_CmdSeparatorDyn);

  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 2);  { Kennung }
  if length (S) > C_CmdKennungLen then exit;

  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 3);  { allg. Parameternummer }
  { leer erlaubt (bedeutet bei MRG: "alle Parameter abrufen", bei DSfG wird der
    Eintrag nicht verwendet) }
  if length (S) > C_CmdParaNrLen then exit;



  { Archivdatentyp ist optional f�r MRG-Parameterabruf, darf auch leer sein: }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 4);  { Archivdaten-Typ; 10.08.2011, WW }
  if (length (S) > 0) AND not isIntString (S) then exit;

  { Dateiname ist optional f�r MRG-Parameterabruf, darf auch leer sein
    und braucht daher nicht extra auf Plausibilit�t gepr�ft werden }

{$IFDEF GAS-X}
  { Gas-X-Version: }
  iSepNr_Timeout:=4;  // 09.03.2020, WW
{$ELSE}
  { RMG-Version: }
  { Archivdatentyp ist optional f�r MRG-Parameterabruf, darf auch leer sein: }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 4);  { Archivdaten-Typ; 10.08.2011, WW }
  if (length (S) > 0) AND not isIntString (S) then exit;

  { Dateiname ist optional f�r MRG-Parameterabruf, darf auch leer sein
    und braucht daher nicht extra auf Plausibilit�t gepr�ft werden }

  iSepNr_Timeout:=6;
{$ENDIF}

  { Timeout ist optional, darf auch leer sein: }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, iSepNr_Timeout);    { Timeout; 16.08.2012, WW }
  if (length (S) > 0) AND not isIntString (S) then exit;

  Result:=true;
end;

{---------------------------------------------------------------}
function Kommando_ZeitSync_plausibel (Kommando: string): boolean;
{---------------------------------------------------------------}
{ Kommando f�r Zeitsynchronisation auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
  sWZSZ: string;
  sUtcOffset: string;
  dummy: TDateTime;

begin
  Result:=false;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 2);   { Kennung }
  if length (S) > C_CmdKennungLen then exit;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3);   { Zeitbasis im Ger�t }
  sWZSZ:=Copy (S, 1, 1);
  if (sWZSZ <> C_CmdZSyncBasis_W) AND (sWZSZ <> C_CmdZSyncBasis_S) then exit;
  // ab 24.02.2020, WW: Zus�tzlich mit optionalem UTC-Offset
  sUtcOffset:=Copy (S, 2, length (S));
  if length (sUtcOffset) > 0  then begin
    if sUtcOffset[1] in ['+', '-'] then
      sUtcOffset:=Copy (sUtcOffset, 2, length (sUtcOffset));  // Vorzeichen ist optional

    if length (sUtcOffset) <> 4 then exit;
    if not EncodeTimeStr (sUtcOffset, 'HHMM', dummy) then exit;  // g�ltiger Zeit-String ?
  end;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 4);   { min. Abweichung in  s }
  if length (S) > C_CmdZSyncAbweichLen then exit;
  if (length (S) > 0) AND not isIntString (S) then exit;  { leer erlaubt -> Standardwert aus INI nehmen }

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 5);   { max. Abweichung in  s }
  if length (S) > C_CmdZSyncAbweichLen then exit;
  if (length (S) > 0) AND not isIntString (S) then exit;  { leer erlaubt -> Standardwert aus INI nehmen }

  { DSfG-Busadresse ist optional f�r DSfG-Zeitsynchronisation, darf auch leer sein
    und braucht daher nicht extra auf Plausibilit�t gepr�ft werden }

  Result:=true;
end;

{-------------------------------------------------------------------}
function Kommando_DSfGUmschalt_plausibel (Kommando: string): boolean;
{-------------------------------------------------------------------}
{ Kommando f�r DSfG-Slave-Umschaltung auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;

begin
  Result:=false;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 2);   { Kennung alt }
  if length (S) > C_MaxMRGKennungLen then exit;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3);   { Kennung neu }
  if length (S) > C_MaxMRGKennungLen then exit;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 4);   { Adresse }
  if length (S) <> C_CmdDSfGUmleitAdrLen then exit;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 5);   { Ger�te-Typ }
  if length (S) <> C_CmdGeraeteTypLen then exit;
  if not isIntString (S) then exit;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 6);   { Passwort }
  if (length (S) = 0) OR (length (S) > C_MaxMRGPasswortLen) then exit;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 7);   { Passwort-Nummer }
  if length (S) <> C_CmdPasswortNrLen then exit;
  if not isIntString (S) then exit;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 8);   { Kennung pr�fen J/N }
  if length (S) <> C_CmdKennPruefLen then exit;

  Result:=true;
end;

{---------------------------------------------------------------------}
function Kommando_DSfGBusAnalyse_plausibel (Kommando: string): boolean;
{---------------------------------------------------------------------}
{ DSfG-Busanalyse-Kommando auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
begin
  Result:=false;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 2);  { Kennung }
  if length (S) > C_CmdKennungLen then exit;

  Result:=true;
end;

{--------------------------------------------------------------}
function Kommando_RpReset_plausibel (Kommando: string): boolean;
{--------------------------------------------------------------}
{ Rundpufferreset-Kommando auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
begin
  Result:=false;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 2);  { Kennung }
  if length (S) > C_MaxMRGKennungLen then exit;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3);  { Rundpuffer-Typ }
  if (S <> C_CmdRpTyp_MW) AND (S <> C_CmdRpTyp_ME) AND (S <> C_CmdRpTyp_PR) then exit;
  Result:=true;
end;

{--------------------------------------------------------------------}
function Kommando_Parametrieren_plausibel (Kommando: string): boolean;
{--------------------------------------------------------------------}
{ Kommando f�r Parameter-Einstellen auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
  ParaTyp: string;

begin
  Result:=false;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 2);        { Kennung }
  if length (S) > C_CmdKennungLen then exit;

  ParaTyp:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3);  { Parameter-Typ }
  if (ParaTyp <> C_CmdParatyp_MRG) AND (ParaTyp <> C_CmdParatyp_DSfG) AND
     (ParaTyp <> C_CmdParatyp_DSfGDfue) then exit;

  if ParaTyp = C_CmdParatyp_MRG then begin
    { Feld "BAdr" nicht pr�fen (wird nicht verwendet) }
    S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 5);      { allg. MRG-Parameternummer }
    if length (S) = 0 then exit;

    { Felder "Zugangscode 1, 2" werden ab 30.04.2019 optional f�r
      Parametrier-Passwort, Passwort-Nummer verwendet (zuvor nicht verwendet) }
    { ZCode1 -> Passwort: keine Beschr�nkungen }
    S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 7);  { ZCode2 -> Passwort-Nummer }
    if length (S) > 0 then begin  // darf leer sein (optional)
      if length (S) <> C_CmdPasswortNrLen then exit;
      if not isIntString (S) then exit;
    end;
  end
  else if ParaTyp = C_CmdParatyp_DSfG then begin
    S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 4);      { Busadresse }
    if length (S) = 0 then exit;

    S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 5);      { DE-Adresse }
    if length (S) = 0 then exit;
  end
  else if ParaTyp = C_CmdParatyp_DSfGDfue then begin
    { Felder "Zugangscode 1, 2" nicht pr�fen (werden nicht verwendet) }
    S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 4);      { DF�-Befehl }
    if length (S) = 0 then exit;

   { Feld "Parameter-Adresse" nicht pr�fen (ist bei manchen DF�-Befehlen leer) }
  end;

  Result:=true;
end;

{------------------------------------------------------------------}
function Kommando_Transparent_plausibel (Kommando: string): boolean;
{------------------------------------------------------------------}
{ Kommando f�r Transparentbefehl auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
  KommandoBuf: string;

begin
  Result:=false;

  { Kommandoteil mit Transparentbefehl abschneiden, wird nicht gepr�ft: }
  S:=Kommando;
  KommandoBuf:=F_Zerlegen (S, C_CmdSeparatorDyn);

  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 2);  { Kennung }
  if length (S) > C_CmdKennungLen then exit;

  { Timeout ist optional, darf auch leer sein: }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 3);
  if (length (S) > 0) AND not isIntString (S) then exit;

  { Empfangsmodus ist optional f�r DSfG-Transparentbefehl, darf auch leer sein: }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 4);
  if (length (S) > 0) AND not isIntString (S) then exit;

  Result:=true;
end;

{----------------------------------------------------------}
function Kommando_Ruf_plausibel (Kommando: string): boolean;
{----------------------------------------------------------}
{ Rufentgegennahme-Kommando auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
begin
  Result:=false;

  { Kennung ist leer, wird daher nicht gepr�ft }

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3); { Modus }
  if not isIntString (S) then exit;
  Result:=true;
end;

{-----------------------------------------------------------------}
function Kommando_Rufannahme_plausibel (Kommando: string): boolean;
{-----------------------------------------------------------------}
{ Rufannahme-Kommando auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
  isDSfG: boolean;
  MaxLen: integer;

begin
  Result:=false;
  isDSfG:=false;

  { pr�fen auf Ger�tetyp "DSfG" (leer oder DSfG-Typnummer) }
  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3);   { Ger�te-Typ }
  if length (S) > 0 then begin
    if length (S) <> C_CmdGeraeteTypLen then exit;
    if not isIntString (S) then exit;
    if StrToInt (S) = C_GeraeteTypDSfG then
      isDSfG:=true;
  end else
    isDSfG:=true;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 2);   { Kennung }
  if isDSfG then
    MaxLen:=C_MaxDSfGKennungLen
  else
    MaxLen:=C_MaxMRGKennungLen;
  if length (S) > MaxLen then exit;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 4);  { Passwort }
  if isDSfG then
    MaxLen:=C_MaxDSfGPasswortLen
  else
    MaxLen:=C_MaxMRGPasswortLen;
  if (length (S) = 0) OR (length (S) > MaxLen) then exit;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 5);  { Passwort-Nummer }
  if not isDSfG then begin       { Passwort-Nummer ist f�r DSfG-Abruf nicht relevant }
    if length (S) <> C_CmdPasswortNrLen then exit;
    if not isIntString (S) then exit;
  end;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 6);  { DSfG-Rufdeaktivierung pr�fen J/N }
  if isDSfG then          { Rufdeaktivierung ist f�r MRG-Abruf nicht relevant }
    if length (S) <> C_CmdDSfGRufDeaktLen then exit;

  Result:=true;
end;

{---------------------------------------------------------------}
function Kommando_Rufliste_plausibel (Kommando: string): boolean;
{---------------------------------------------------------------}
{ Ruflistenabfrage-Kommando auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
begin
  Result:=false;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 2);  { Kennung }
  if length (S) > C_CmdKennungLen then exit;

  Result:=true;
end;

{--------------------------------------------------------------------}
function Kommando_SlaveRufQuitt_plausibel (Kommando: string): boolean;
{--------------------------------------------------------------------}
{ Slave-Rufquittierung-Kommando auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
begin
  Result:=false;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 2);  { Kennung }
  if length (S) > C_CmdKennungLen then exit;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3);  { Adresse }
  if length (S) <> C_CmdDSfGUmleitAdrLen then exit;

  Result:=true;
end;

{---------------------------------------------------------------}
function Kommando_Rueckruf_plausibel (Kommando: string): boolean;
{---------------------------------------------------------------}
{ R�ckruf-Kommando auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
begin
  Result:=false;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 2);  { Kennung }
  if length (S) > C_CmdKennungLen then exit;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3);  { Zentrale }
  if length (S) <> C_CmdZentraleNrLen then exit;
  if not isIntString (S) then exit;

  Result:=true;
end;

{---------------------------------------------------------------}
function Kommando_ZeitAbruf_plausibel (Kommando: string): boolean;  // 17.08.2010
{---------------------------------------------------------------}
{ R�ckruf-Kommando auf Plausibilit�t pr�fen;
  �bergabe: Kommando
  Ergebnis: true, wenn Kommando plausibel }
var
  S: string;
begin
  Result:=false;

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 2);  { Kennung }
  if length (S) > C_CmdKennungLen then exit;

  Result:=true;
end;

{--------------------------------------------------------------------------------------}
function AbrufKommando_plausibel (KommandoTyp: TKommandoTyp; Kommando: string): boolean;
{--------------------------------------------------------------------------------------}
{ Abruf-Kommando auf Plausibilit�t pr�fen;
  �bergabe: Abruf-Kommandotyp
  Ergebnis: true, wenn Kommando plausibel }
begin
  case KommandoTyp of
    kt_VerbAufbau:     Result:=Kommando_VerbAufbau_plausibel (Kommando);
    kt_VerbAbbau:      Result:=Kommando_VerbAbbau_plausibel (Kommando);
    kt_MessAbruf,
    kt_MeldAbruf,
    kt_PruefAbruf:     Result:=Kommando_Mess_Meld_PruefAbruf_plausibel (Kommando);
    kt_ParaAbruf:      Result:=Kommando_ParaAbruf_plausibel (Kommando);
    kt_ZeitSync:       Result:=Kommando_ZeitSync_plausibel (Kommando);
    kt_DSfGUmschalt:   Result:=Kommando_DSfGUmschalt_plausibel (Kommando);
    kt_DSfGBusAnalyse: Result:=Kommando_DSfGBusanalyse_plausibel (Kommando);
    kt_RpReset:        Result:=Kommando_RpReset_plausibel (Kommando);
    kt_Parametrieren:  Result:=Kommando_Parametrieren_plausibel (Kommando);
    kt_Transparent:    Result:=Kommando_Transparent_plausibel (Kommando);
    kt_Ruf:            Result:=Kommando_Ruf_plausibel (Kommando);
    kt_Rufannahme:     Result:=Kommando_Rufannahme_plausibel (Kommando);
    kt_Rufliste:       Result:=Kommando_Rufliste_plausibel (Kommando);
    kt_SlaveRufQuitt:  Result:=Kommando_SlaveRufQuitt_plausibel (Kommando);
    kt_Rueckruf:       Result:=Kommando_Rueckruf_plausibel (Kommando);
    kt_ZeitAbruf:      Result:=Kommando_ZeitAbruf_plausibel (Kommando);  // 17.08.2010
  else
    Result:=false;
  end;  { case }
end;

end.
