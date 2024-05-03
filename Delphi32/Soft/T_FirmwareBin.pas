{******************************************************************************}
{* Unit: Tools f�r Firmware-Bin�rdaten                                        *}
{* 23.02.2012  WW  Neu                                                        *}
{*                                                                            *}
{* Copyright � RMG Messtechnik GmbH 2012                                      *}
{******************************************************************************}
unit T_FirmwareBin;

interface

uses
  StrUtils, T_BinMask, WStrUtils, WChars;

const
  C_BlockGroesseFirmware = 256;  // Blockgr��e f�r Firmware-Bin�rdaten

  C_FwAdrOffsetBefehl = $F00000;  // Offset f�r Bin�rdaten-Adressen in Firmware-Update-Befehlen

  C_FwBinFilePrefix_Hersteller = 'RMG';  { Hersteller-Prefix im Dateinamen von
                                           RMG-Firmware-Bin�rdatendateien }

type
  { Struktur f�r Firmware-Bin�rdaten-Informationen }
  TFwBinInfo = record
    StartAdresse: integer;  // Start-Adresse des ersten Blocks mit Daten
    EndAdresse: integer;    // End-Adresse des letzten Blocks
    Typ: integer;           // Typ-Information (FW-Ger�tetypnummer)
    Version: integer;       // Version-Information (xxx.yy)
    Build: string;          // Build-Information (yyyymmddhhmmss)
    AnzBytesGesamt: integer;     // Gesamtanzahl Bytes in Firmware-Bin�rdaten
    AnzBytes: integer;           // Anzahl Bytes ab Startadresse
    AnzBloecke: integer;         // Anzahl enthaltener Bl�cke ab Startadresse
    AnzBloeckeBenutzt: integer;  // Anzahl mit Daten belegter Bl�cke
  end;

  { Struktur f�r in Firmware enthaltene Fabrikationsdaten einer NG-DSfG-DF� }
  TFwFabrikationsdaten_NG = record
    Fabriknummer: string;
    Baujahr: integer;
    SysConfig: integer;  // enth�lt Ausf�hrung (Kassette, Geh�use) und Spannungsversorgung
    Flashgroesse: integer;
  end;


function GetFirmwareBinFilename (sHerstellerDSfG: string; sTyp: string;
  sPfad: string): string;
function FirmwareBinDataEmpty (sBinData: string): boolean;
procedure ScanFirmwareBinData_NG (sBinData: string; var FwBinInfo: TFwBinInfo);
procedure UpdateFirmwareBinData_NG (FwFabrikationsdaten: TFwFabrikationsdaten_NG;
  var sBinData: string);

implementation

const
  { Firmware-Bin�rdaten-Adressen und -L�ngen von Fabrikationsdaten der NG-DSfG-DF�: }
  C_NG_FwBinAdr_Build = $FE000;  // Adresse an der die Build-Info steht
  C_NG_FwBinLen_Build = 12;      // Bin�rdaten-L�nge der Build-Info

  C_NG_FwBinAdr_Typ = $FE070;  // Adresse an der die Typ-Info steht
  C_NG_FwBinLen_Typ = 2;       // Bin�rdaten-L�nge der Typ-Info

  C_NG_FwBinAdr_Version = $FE072;  // Adresse an der die Version-Info steht
  C_NG_FwBinLen_Version = 2;       // Bin�rdaten-L�nge der Version-Info

  C_NG_FwBinAdr_FabrikNr = $FE074;  // Adresse an der die Fabriknummer steht
  C_NG_FwBinLen_FabrikNr = 12;      // Bin�rdaten-L�nge der Fabriknummer

  C_NG_FwBinAdr_Baujahr = $FE081;  // Adresse an der das Baujahr steht
  C_NG_FwBinLen_Baujahr = 2;       // Bin�rdaten-L�nge des Baujahr

  C_NG_FwBinAdr_Flashgroesse = $FE08A;  // Adresse an der die Flashgr��e steht
  C_NG_FwBinLen_Flashgroesse = 2;       // Bin�rdaten-L�nge der Flashgroesse

  C_NG_FwBinAdr_SysConfig = $FE08C;  // Adresse an der die SysConfig-Info steht
  C_NG_FwBinLen_SysConfig = 2;       // Bin�rdaten-L�nge der SysConfig-Info


{---------------------------------------------------------------------}
function GetFirmwareBinFilename (sHerstellerDSfG: string; sTyp: string;
  sPfad: string): string;
{---------------------------------------------------------------------}
{ Bildet vollst�ndigen Namen der Firmware-Bin�rdatendatei mit Pfad;
  �bergabe: Herstellerk�rzel aus DSfG-Protokoll
            Ger�tetyp
            Pfad
  Ergebnis: Dateiname }
var
  sHerstPrefix: string;

begin
  if sHerstellerDSfG = 'W' then  // Wieser 
    sHerstPrefix:=C_FwBinFilePrefix_Hersteller
  else
    sHerstPrefix:='';

  Result:=sPfad + sHerstPrefix + '_' + sTyp + '.BIN';
end;

{--------------------------------------------------------}
function FirmwareBinDataEmpty (sBinData: string): boolean;
{--------------------------------------------------------}
{ Pr�ft, ob Firmware-Bin�rdaten "leer" sind (alle Zeichen sind $FF);
  �bergabe: Firmware-Bin�rdaten
  Ergebnis: false, wenn in Bin�rdaten mind. ein Zeichen ungleich $FF ist }
var
  i: integer;

begin
  Result:=true;
  for i:=1 to length (sBinData) do begin
    if sBinData [i] <> Chr ($FF) then begin
      Result:=false;
      Break;
    end;
  end;  { for i }
end;

{-----------------------------------------------------------------------------}
procedure ScanFirmwareBinData_NG (sBinData: string; var FwBinInfo: TFwBinInfo);
{-----------------------------------------------------------------------------}
{ Liest Firmware-Bin�rdaten einer NG-DSfG-DF� und liefert Informationen zur�ck;
  �bergabe: Firmware-Bin�rdaten
  R�ckgabe: Bin�rdaten-Informationen }
var
  i: integer;
  S: string;

begin
  with FwBinInfo do begin
    { Vorbelegung f�r R�ckgabe: }
    StartAdresse:=-1;  // unbekannt
    EndAdresse:=length (sBinData) - 1;  // L�nge der Firmware-Bin�rdaten
    Typ:=-1;  // unbekannt
    Version:=-1;  // unbekannt
    Build:='';  // unbekannt
    AnzBytesGesamt:=length (sBinData);
    AnzBytes:=0;
    AnzBloecke:=0;
    AnzBloeckeBenutzt:=0;

    { Firmware-Bin�rdaten blockweise durchgehen: }
    i:=1;
    while i <= length (sBinData) do begin
      S:=Copy (sBinData, i, C_BlockGroesseFirmware);  // einen Block ausschneiden
      { Pr�fen, ob Block Daten enth�lt oder "leer" ist: }
      if not FirmwareBinDataEmpty (S) then begin
        if StartAdresse < 0 then  // Startadresse noch nicht ermittelt
          StartAdresse:=i - 1;  // Startadresse (erster Block der ein Zeichen ungleich $FF enth�lt)

        if StartAdresse >= 0 then  // Startadresse ermittelt
          inc (AnzBloeckeBenutzt);  // Anzahl der Bl�cke mit Daten hochz�hlen
      end;

      if StartAdresse >= 0 then  // Startadresse ermittelt
        inc (AnzBloecke);  // Anzahl der Bl�cke hochz�hlen

      inc (i, C_BlockGroesseFirmware)
    end;  { while i <= length (sBinData) }

    { Anzahl der Bytes ab Startadresse: }
    AnzBytes:=AnzBytesGesamt - StartAdresse;

    { Typ: }
    if length (sBinData) >= (C_NG_FwBinAdr_Typ + C_NG_FwBinLen_Typ) then begin
      S:=Copy (sBinData, C_NG_FwBinAdr_Typ + 1, C_NG_FwBinLen_Typ);
      Typ:=Bin2Word (S);
    end;

    { Version: }
    if length (sBinData) >= (C_NG_FwBinAdr_Version + C_NG_FwBinLen_Version) then begin
      S:=Copy (sBinData, C_NG_FwBinAdr_Version + 1, C_NG_FwBinLen_Version);
      Version:=Bin2Word (S);
    end;

    { Build: }
    if length (sBinData) >= (C_NG_FwBinAdr_Build + C_NG_FwBinLen_Build) then begin
      S:=Copy (sBinData, C_NG_FwBinAdr_Build + 1, C_NG_FwBinLen_Build);  // Format ddmmyyyyhhmm
      { R�ckgabe im Format yyyymmddhhmmss: }
      Build:=Copy (S, 5, 4) + Copy (S, 3, 2) + Copy (S, 1, 2) +
             Copy (S, 9, length (S)) + '00';
    end;
  end;  { with FwBinInfo }
end;

{-------------------------------------------------------------------------------}
procedure UpdateFirmwareBinData_NG (FwFabrikationsdaten: TFwFabrikationsdaten_NG;
  var sBinData: string);
{-------------------------------------------------------------------------------}
{ Firmware-Bin�rdaten mit Fabrikationsdaten einer NG-DSfG-DF� updaten (�berschreiben);
  �bergabe: NG-Fabrikationsdaten
  �bergabe/R�ckgabe: Firmware-Bin�rdaten }
var
  sUpdate: string;

begin
  { Fabriknummer: }
  sUpdate:=Copy (FwFabrikationsdaten.Fabriknummer, 1, C_NG_FwBinLen_FabrikNr);  // auf max. L�nge begrenzen
  sUpdate:=F_RightPad (sUpdate, NUL, C_NG_FwBinLen_FabrikNr);  // mit abschlie�enden NUL-Zeichen auff�llen
  sBinData:=StuffString (sBinData, C_NG_FwBinAdr_FabrikNr + 1, C_NG_FwBinLen_FabrikNr, sUpdate);

  { Baujahr: }
  sUpdate:=Word2Bin (FwFabrikationsdaten.Baujahr);
  sBinData:=StuffString (sBinData, C_NG_FwBinAdr_Baujahr + 1, C_NG_FwBinLen_Baujahr, sUpdate);

  { Flashgr��e: }
  sUpdate:=Word2Bin (FwFabrikationsdaten.Flashgroesse);
  sBinData:=StuffString (sBinData, C_NG_FwBinAdr_Flashgroesse + 1, C_NG_FwBinLen_Flashgroesse, sUpdate);

  { SysConfig: }
  sUpdate:=Word2Bin (FwFabrikationsdaten.SysConfig);
  sBinData:=StuffString (sBinData, C_NG_FwBinAdr_SysConfig + 1, C_NG_FwBinLen_SysConfig, sUpdate);
end;

end.
