{******************************************************************************}
{* Unit: Tools für Firmware-Binärdaten                                        *}
{* 23.02.2012  WW  Neu                                                        *}
{*                                                                            *}
{* Copyright © RMG Messtechnik GmbH 2012                                      *}
{******************************************************************************}
unit T_FirmwareBin;

interface

uses
  StrUtils, T_BinMask, WStrUtils, WChars;

const
  C_BlockGroesseFirmware = 256;  // Blockgröße für Firmware-Binärdaten

  C_FwAdrOffsetBefehl = $F00000;  // Offset für Binärdaten-Adressen in Firmware-Update-Befehlen

  C_FwBinFilePrefix_Hersteller = 'RMG';  { Hersteller-Prefix im Dateinamen von
                                           RMG-Firmware-Binärdatendateien }

type
  { Struktur für Firmware-Binärdaten-Informationen }
  TFwBinInfo = record
    StartAdresse: integer;  // Start-Adresse des ersten Blocks mit Daten
    EndAdresse: integer;    // End-Adresse des letzten Blocks
    Typ: integer;           // Typ-Information (FW-Gerätetypnummer)
    Version: integer;       // Version-Information (xxx.yy)
    Build: string;          // Build-Information (yyyymmddhhmmss)
    AnzBytesGesamt: integer;     // Gesamtanzahl Bytes in Firmware-Binärdaten
    AnzBytes: integer;           // Anzahl Bytes ab Startadresse
    AnzBloecke: integer;         // Anzahl enthaltener Blöcke ab Startadresse
    AnzBloeckeBenutzt: integer;  // Anzahl mit Daten belegter Blöcke
  end;

  { Struktur für in Firmware enthaltene Fabrikationsdaten einer NG-DSfG-DFÜ }
  TFwFabrikationsdaten_NG = record
    Fabriknummer: string;
    Baujahr: integer;
    SysConfig: integer;  // enthält Ausführung (Kassette, Gehäuse) und Spannungsversorgung
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
  { Firmware-Binärdaten-Adressen und -Längen von Fabrikationsdaten der NG-DSfG-DFÜ: }
  C_NG_FwBinAdr_Build = $FE000;  // Adresse an der die Build-Info steht
  C_NG_FwBinLen_Build = 12;      // Binärdaten-Länge der Build-Info

  C_NG_FwBinAdr_Typ = $FE070;  // Adresse an der die Typ-Info steht
  C_NG_FwBinLen_Typ = 2;       // Binärdaten-Länge der Typ-Info

  C_NG_FwBinAdr_Version = $FE072;  // Adresse an der die Version-Info steht
  C_NG_FwBinLen_Version = 2;       // Binärdaten-Länge der Version-Info

  C_NG_FwBinAdr_FabrikNr = $FE074;  // Adresse an der die Fabriknummer steht
  C_NG_FwBinLen_FabrikNr = 12;      // Binärdaten-Länge der Fabriknummer

  C_NG_FwBinAdr_Baujahr = $FE081;  // Adresse an der das Baujahr steht
  C_NG_FwBinLen_Baujahr = 2;       // Binärdaten-Länge des Baujahr

  C_NG_FwBinAdr_Flashgroesse = $FE08A;  // Adresse an der die Flashgröße steht
  C_NG_FwBinLen_Flashgroesse = 2;       // Binärdaten-Länge der Flashgroesse

  C_NG_FwBinAdr_SysConfig = $FE08C;  // Adresse an der die SysConfig-Info steht
  C_NG_FwBinLen_SysConfig = 2;       // Binärdaten-Länge der SysConfig-Info


{---------------------------------------------------------------------}
function GetFirmwareBinFilename (sHerstellerDSfG: string; sTyp: string;
  sPfad: string): string;
{---------------------------------------------------------------------}
{ Bildet vollständigen Namen der Firmware-Binärdatendatei mit Pfad;
  Übergabe: Herstellerkürzel aus DSfG-Protokoll
            Gerätetyp
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
{ Prüft, ob Firmware-Binärdaten "leer" sind (alle Zeichen sind $FF);
  Übergabe: Firmware-Binärdaten
  Ergebnis: false, wenn in Binärdaten mind. ein Zeichen ungleich $FF ist }
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
{ Liest Firmware-Binärdaten einer NG-DSfG-DFÜ und liefert Informationen zurück;
  Übergabe: Firmware-Binärdaten
  Rückgabe: Binärdaten-Informationen }
var
  i: integer;
  S: string;

begin
  with FwBinInfo do begin
    { Vorbelegung für Rückgabe: }
    StartAdresse:=-1;  // unbekannt
    EndAdresse:=length (sBinData) - 1;  // Länge der Firmware-Binärdaten
    Typ:=-1;  // unbekannt
    Version:=-1;  // unbekannt
    Build:='';  // unbekannt
    AnzBytesGesamt:=length (sBinData);
    AnzBytes:=0;
    AnzBloecke:=0;
    AnzBloeckeBenutzt:=0;

    { Firmware-Binärdaten blockweise durchgehen: }
    i:=1;
    while i <= length (sBinData) do begin
      S:=Copy (sBinData, i, C_BlockGroesseFirmware);  // einen Block ausschneiden
      { Prüfen, ob Block Daten enthält oder "leer" ist: }
      if not FirmwareBinDataEmpty (S) then begin
        if StartAdresse < 0 then  // Startadresse noch nicht ermittelt
          StartAdresse:=i - 1;  // Startadresse (erster Block der ein Zeichen ungleich $FF enthält)

        if StartAdresse >= 0 then  // Startadresse ermittelt
          inc (AnzBloeckeBenutzt);  // Anzahl der Blöcke mit Daten hochzählen
      end;

      if StartAdresse >= 0 then  // Startadresse ermittelt
        inc (AnzBloecke);  // Anzahl der Blöcke hochzählen

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
      { Rückgabe im Format yyyymmddhhmmss: }
      Build:=Copy (S, 5, 4) + Copy (S, 3, 2) + Copy (S, 1, 2) +
             Copy (S, 9, length (S)) + '00';
    end;
  end;  { with FwBinInfo }
end;

{-------------------------------------------------------------------------------}
procedure UpdateFirmwareBinData_NG (FwFabrikationsdaten: TFwFabrikationsdaten_NG;
  var sBinData: string);
{-------------------------------------------------------------------------------}
{ Firmware-Binärdaten mit Fabrikationsdaten einer NG-DSfG-DFÜ updaten (überschreiben);
  Übergabe: NG-Fabrikationsdaten
  Übergabe/Rückgabe: Firmware-Binärdaten }
var
  sUpdate: string;

begin
  { Fabriknummer: }
  sUpdate:=Copy (FwFabrikationsdaten.Fabriknummer, 1, C_NG_FwBinLen_FabrikNr);  // auf max. Länge begrenzen
  sUpdate:=F_RightPad (sUpdate, NUL, C_NG_FwBinLen_FabrikNr);  // mit abschließenden NUL-Zeichen auffüllen
  sBinData:=StuffString (sBinData, C_NG_FwBinAdr_FabrikNr + 1, C_NG_FwBinLen_FabrikNr, sUpdate);

  { Baujahr: }
  sUpdate:=Word2Bin (FwFabrikationsdaten.Baujahr);
  sBinData:=StuffString (sBinData, C_NG_FwBinAdr_Baujahr + 1, C_NG_FwBinLen_Baujahr, sUpdate);

  { Flashgröße: }
  sUpdate:=Word2Bin (FwFabrikationsdaten.Flashgroesse);
  sBinData:=StuffString (sBinData, C_NG_FwBinAdr_Flashgroesse + 1, C_NG_FwBinLen_Flashgroesse, sUpdate);

  { SysConfig: }
  sUpdate:=Word2Bin (FwFabrikationsdaten.SysConfig);
  sBinData:=StuffString (sBinData, C_NG_FwBinAdr_SysConfig + 1, C_NG_FwBinLen_SysConfig, sUpdate);
end;

end.
