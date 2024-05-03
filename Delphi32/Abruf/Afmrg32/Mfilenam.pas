{******************************************************************************}
{* Unit: Namen für MRG-Datenfiles                                             *}
{* 02.12.98 WW                                                                *}
{******************************************************************************}
Unit MFileNam;

INTERFACE

Uses
  Windows, SysUtils, PathIni, LGZType, WStrUtils;

Const

  { Codes für Daten-Filenamen }

  dd_Mess   = 1;
  dd_Tags   = 2;
  dd_Pruef  = 3;
  dd_Tags14 = 4;
  dd_Meld   = 5;
  dd_Para   = 6;

Function GetLGZDVerzDateiName: TFileName;
Function GetZwischenDateiName_MrgId (MrgId, Code: Word): TFileName;
Function CreateZwischenDateiNameTemp (Code: Word): TFileName;
Function GetLGZDateiName (LGZNr, Code: Word): TFileName;
Function GetLGZFehlerDateiName (LGZNr, Code: Word): TFileName;
Function GetManuDateiName (MrgId, Code: Word): TFileName;
Function GetManuFehlerDateiName (MrgId, Code: Word): TFileName;
function Get_natGAS_ASCIIFilename (Stationsname: string): string;

IMPLEMENTATION

{---------------------------------------}
Function GetLGZDVerzDateiName: TFileName;
{---------------------------------------}
begin
  Result:=PathServer.PathName [LangZeitDir] + CLangZeitVerz;
end;

{-----------------------------------------------------------------}
Function GetZwischenDateiName_MrgId (MrgId, Code: Word): TFileName;
{-----------------------------------------------------------------}
var
  FileName: TFileName;
Begin
  Result:='';
  FileName:=PathServer.PathName [WWorkDir];
  Case Code of
    dd_Mess  : FileName:=FileName + 'ZSTD';
    dd_Tags  : FileName:=FileName + 'ZTAG';
    dd_Pruef : FileName:=FileName + 'ZPRF';
    dd_Tags14: FileName:=FileName + 'ZZST';
{$IFDEF GVT}
    dd_Meld  : FileName:=FileName + 'ZMEL';
    dd_Para  : FileName:=FileName + 'ZPAR';
{$ENDIF}
  Else
    Exit;
  End;
  FileName:=FileName + Format('%.4d.DAT', [MrgId]);
  Result := FileName;
End;

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
    dd_Mess  : Pref:='ZST';
    dd_Tags  : Pref:='ZTA';
    dd_Pruef : Pref:='ZPR';
    dd_Tags14: Pref:='ZZS';
{$IFDEF GVT}
    dd_Meld  : Pref:='ZME';
    dd_Para  : Pref:='ZPA';
{$ENDIF}
  Else
    Exit;
  End;
  GetTempFileName (pchar (PathServer.PathName [WWorkDir]), pchar (Pref), 0, pFileName);
  Result := string (pFileName);
End;

{------------------------------------------------------}
Function GetLGZDateiName (LGZNr, Code: Word): TFileName;
{------------------------------------------------------}
var
  FileName: TFileName;
Begin
  Result:='';
  FileName:=PathServer.PathName [LangzeitDir];
  Case Code of
    dd_Mess  : FileName:=FileName + CLangZeitMess;
    dd_Tags  : FileName:=FileName + CLangZeitTag;
    dd_Pruef : FileName:=FileName + CLangZeitPrf;
    dd_Tags14: FileName:=FileName + CLangZeitTag14;
{$IFDEF GVT}
    dd_Meld  : FileName:=FileName + CLangZeitMeld;
    dd_Para  : FileName:=FileName + CLangZeitPara;
{$ENDIF}
  Else
    Exit;
  End;
  FileName:=FileName + Format('%.4d.DAT', [LGZNr]);
  Result := FileName;
End;

{------------------------------------------------------------}
Function GetLGZFehlerDateiName (LGZNr, Code: Word): TFileName;
{------------------------------------------------------------}
var
  FileName: TFileName;
Begin
  Result:='';
  FileName:=PathServer.PathName [LangzeitDir];
  Case Code of
    dd_Mess  : FileName:=FileName + CLangZeitMess;
    dd_Tags  : FileName:=FileName + CLangZeitTag;
    dd_Pruef : FileName:=FileName + CLangZeitPrf;
    dd_Tags14: FileName:=FileName + CLangZeitTag14;
  Else
    Exit;
  End;
  FileName:=FileName + Format('%.4d.ERR', [LGZNr]);
  Result := FileName;
End;

{-------------------------------------------------------}
Function GetManuDateiName (MrgId, Code: Word): TFileName;
{-------------------------------------------------------}
var
  FileName: TFileName;
Begin
  Result:='';
  FileName:=PathServer.PathName [ManuDir];
  Case Code of
    dd_Mess  : FileName:=FileName + 'MSTD';
    dd_Tags  : FileName:=FileName + 'MTAG';
    dd_Pruef : FileName:=FileName + 'MPRF';
    dd_Tags14: FileName:=FileName + 'MZST';
  Else
    Exit;
  End;
  FileName:=FileName + Format('%.4d.DAT', [MrgId]);
  Result := FileName;
End;

{-------------------------------------------------------------}
Function GetManuFehlerDateiName (MrgId, Code: Word): TFileName;
{-------------------------------------------------------------}
var
  FileName: TFileName;
Begin
  Result:='';
  FileName:=PathServer.PathName [ManuDir];
  Case Code of
    dd_Mess  : FileName:=FileName + 'MSTD';
    dd_Tags  : FileName:=FileName + 'MTAG';
    dd_Pruef : FileName:=FileName + 'MPRF';
    dd_Tags14: FileName:=FileName + 'MZST';
  Else
    Exit;
  End;
  FileName:=FileName + Format('%.4d.ERR', [MrgId]);
  Result := FileName;
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
