{******************************************************************************}
{* Unit: Namen für MRG-Datenfiles                                             *}
{* 02.12.98 WW                                                                *}
{******************************************************************************}
Unit MFileNam;

INTERFACE

Uses
  Windows, SysUtils, PathIni, WStrUtils;

Const
  { Codes für Zwischendaten-Filenamen }
  dd_Mess   = 1;
  dd_Tags   = 2;
  dd_Pruef  = 3;

  { Präfixe für Zwischendaten-Filenamen }
  prefix_Mess  = 'ZST';
  prefix_Tags  = 'ZTA';
  prefix_Pruef = 'ZPR';


Function CreateZwischenDateiNameTemp (Code: Word): TFileName;
function Get_natGAS_ASCIIFilename (Stationsname: string): string;

IMPLEMENTATION

{-----------------------------------------------------------}
Function CreateZwischenDateiNameTemp (Code: Word): TFileName;
{-----------------------------------------------------------}
{ Zwischendatei mit eindeutigem Namen anlegen und Filenamen zurückgeben
  -> Prefixes 3-stellig, da GetTempFileName nicht mehr Stellen verarbeitet }
var
  Pref: string;
  pFileName: array[0..255] of char;
Begin
  Result:='';
  Case Code of
    dd_Mess : Pref:=prefix_Mess;
    dd_Tags : Pref:=prefix_Tags;
    dd_Pruef: Pref:=prefix_Pruef;
  Else
    Exit;
  End;
  GetTempFileName (pchar (PathServer.PathName [WWorkDir]), pchar (Pref), 0, pFileName);
  Result := string (pFileName);
End;

{---------------------------------------------------------------}
function Get_natGAS_ASCIIFilename (Stationsname: string): string;
{---------------------------------------------------------------}
{ liefert Filename für ASCII-Daten aus Stationsname des Stammsatzes
  -> speziell für natGAS
  -> Der Filename ist im Stationsnamen des Stammsatzes enthalten zwischen dem
     1. und 2. Unterstrich, z.B. Stationsname = '3002_NATGAS001.DAT_Speicher Potsdam:
     ASCII-Filename = NATGAS001.DAT }
Const
  CTrenner = '_';
var
  ASCIIFilename: TFileName;
begin
  ASCIIFilename:=ExtractString (Stationsname, CTrenner, CTrenner, 0);
  if length (ASCIIFilename) = 0 then
    ASCIIFilename:='UNBEKANNT.ASC';
  Result:=PathServer.PathName [AsciiDir] + ASCIIFilename;
end;

End.

