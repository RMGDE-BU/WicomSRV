{------------------------------------------------------------------------------}
{ Einbinden von AsciiExport.DLL                                                }
{                                                                              }
{ 05.02.2001  GD  Neu                                                          }
{ 08.06.2001  GD  Erweiterung für DSfG                                         }
{ 18.07.2001  GD  Initialization, Finalization                                 }
{ 07.08.2001  GD  Erweitert um Debug-Handling                                  }
{ 21.10.2002  GD  Erweitert um Aktualisieren                                   }
{ 23.05.2003  GD  Anpassung an geändertes Rohdatenformat                       }
{ 30.09.2003  GD  Erweiterung um direkten Excel-Export                         }
{ 10.01.2007  GD  Abhhängigkeit von EXPORTSKRIPT.PAS entfernt                  }
{                                                                              }
{ Copyright(C) Karl Wieser GmH 2001, 2007                                      }
{------------------------------------------------------------------------------}
unit AsciiExportDLL;

interface

uses
  Windows, SysUtils, DbTables,
  Mrgstd, PathIni, WSysCon;

const
  C_DLL_AsciiExport = 'AsciiExp.DLL';     // DLL

type
  TByteSet = set of byte;

  TASCIIExportTyp = (aetStundenwerte, aetTageswerte, aetMeldungen,
    aetFinalisation, aetDArchive, aetDLogbuecher, aetWical); // 08.06.2001

  TASCIIExportOverrideState = (aeosAppend, aeosOverride, aeosIgnore);

  TExportSet = set of TASCIIExportTyp;

  TMExportInfoDataRec = record
    MrgId          : integer;
    StationsName   : string [szlen_StationsName];
    Kennung        : string [szlen_Kennung];
    MRGTyp         : string [10];
    Kanalzahl      : byte;
    WzSzUmstellung : boolean;
    KanalList      : TMSNewKanalList;
    IsOriData      : boolean;  // 12.12.2006
  end;

function ASCIIExportDllVersion(s: string): boolean;

function InitDll: boolean;  // 18.07.2001
function DoneDll: boolean;  // 18.07.2001
function Aktualisieren: boolean;  // 21.10.2002
function InitASCIIExport(cGerTyp: char; iGerId: integer; pExportSet: TExportSet;
  sExportPrefix: string; dtVon, dtBis: TDateTime; sFilePath: TFilename = '';
  sDefFile: TFilename = ''; bExcel: boolean = False): boolean;
function InitASCIIExportWithData(iGerId: integer; pExportSet: TExportSet;
  pInfoRec: TMExportInfoDataRec; sExportPrefix: string; dtVon, dtBis: TDateTime;
  sFilePath: TFilename = ''; sDefFile: TFilename = ''): boolean;
function NewASCIIExportFile(cGerTyp: char; iGerId: integer;
  pExportTyp: TASCIIExportTyp; sFilename: TFilename; iRecKanalZahl: byte;
  dtVon: TDateTime = 0; dtBis: TDateTime = 0;
  bLgzStruc: boolean = True): boolean;    // 23.05.2003
function NewASCIIExportObject(
  cGerTyp: char; iGerId: integer; pExportTyp: TASCIIExportTyp;  // 12.02.2001
  var pObject; dtVon: TDateTime = 0; dtBis: TDateTime = 0): boolean;
function SaveASCIIExportFiles(bFinalize: boolean = True): boolean;
function DoneASCIIExport: boolean;

function BuildDArchiveExportFile(sFileName: TFileName; sDatabaseName: string;
  iInstanzId: integer; iArchivGrpNr: byte; pKanaele: TByteSet;
  dtVon, dtBis: TDateTime): boolean;
procedure SetASCIIDebugFlag(bValue: boolean); register;  // 07.08.2001

implementation

const
  C_DLL_AsciiExport_DllVersion = 'DllVersion';  // Methode
  C_DLL_AsciiExport_InitDll = 'InitDll';  // Methode
  C_DLL_AsciiExport_DoneDll = 'DoneDll';  // Methode
  C_DLL_AsciiExport_InitExport = 'InitExport';  // Methode
  C_DLL_AsciiExport_InitExportWithData = 'InitExportWithData';  // Methode
  C_DLL_AsciiExport_NewExportFile = 'NewExportFile';  // Methode
  C_DLL_AsciiExport_NewExportObject = 'NewExportObject';  // Methode
  C_DLL_AsciiExport_SaveExportFiles = 'SaveAllExportFiles';  // Methode
  C_DLL_AsciiExport_DoneExport = 'DoneExport';  // Methode

type
  TBoolProc = procedure (b: boolean);

  TResultCall = function (): boolean;
  TParamBoolResultCall = function(Value: boolean): boolean;
  TPCharBoolCall = function (p: PChar): boolean;
  TDArchivFileCall = function(s1: TFileName; s2: string; i1: integer;
    i2: byte; p: TByteSet; dt1, dt2: TDateTime): boolean;

  TInitAsciiExport = function (cGerTyp: char; iGerId: integer;
    pExportSet: TExportSet; sExportPrefix: string; dtVon, dtBis: TDateTime;
    sFilePath: TFilename = ''; sDefFile: TFilename = '';
    bExcel: boolean = False): boolean;
  TInitAsciiExportWithData = function (cGerTyp: char; iGerId: integer;
    pExportSet: TExportSet; pInfoRec: TMExportInfoDataRec;
    sExportPrefix: string; dtVon, dtBis: TDateTime; sFilePath: TFilename = '';
    sDefFile: TFilename = ''): boolean;
  TNewAsciiExportFile = function(cGerTyp: char; iGerId: integer;
    pExportTyp: TASCIIExportTyp; sFilename: TFilename; iRecKanalZahl: byte;
    dtVon: TDateTime = 0; dtBis: TDateTime = 0; b: boolean = True): boolean;
  TNewAsciiExportObject = function(cGerTyp: char; iGerId: integer;
    pExportTyp: TASCIIExportTyp; var pObject; dtVon: TDateTime = 0;
    dtBis: TDateTime = 0): boolean;

var
  iAsciiExportHandle : integer = 0;

{------------------------- Allgemeine Funktionen ------------------------------}

{ Holt Wieser-Hauptpfad                       }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function GetWieserPath: string;
{---------------------------------------------}
begin
  with TProgramIni.Create do
  try
    Result := ExtractFilePath(WieserIniFile);
  finally
    Free;
  end;
end;

{-------------------------------- ASCIIExport.DLL -----------------------------}

{ Gibt DLL-Version zurück                     }
{ Parameter: Übergabevariable für Versionsinfo}
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function ASCIIExportDllVersion(s : string): boolean;
{---------------------------------------------}
var
  p     : PChar;
  pCall : TPCharBoolCall;
begin
  Result := False;  // Default
  try
    GetMem(p, 80);
    try
      if (iAsciiExportHandle <> 0) then begin
        @pCall := GetProcAddress(
          iAsciiExportHandle, C_DLL_AsciiExport_DllVersion);
        if (@pCall <> nil) then Result := pCall(p);
        if (Result) then s := StrPas(p);
      end;
    finally
      FreeMem(p, 80);
    end;
  except
  //  Result ist False
  end;
end;

{ Lädt DLL                                    }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function InitDll: boolean;  // 18.07.2001
{---------------------------------------------}
var
  pInit : TResultCall;
  s     : string;
begin
  Result := False;  // Default

  s := Session.PrivateDir;  // 05.08.2004
  try
    try
      if (iAsciiExportHandle = 0) then
        iAsciiExportHandle := LoadLibrary(PChar(GetWieserPath + C_DLL_AsciiExport));
      if (iAsciiExportHandle <> 0) then begin
        @pInit := GetProcAddress(
          iAsciiExportHandle, C_DLL_AsciiExport_InitDll);
        if (@pInit <> nil) then Result := pInit;
      end;
    except
    // Result ist False
    end;
  finally
    Session.PrivateDir := s;
  end;
end;

{ Gibt DLL frei                               }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function DoneDll: boolean;  // 18.07.2001
{---------------------------------------------}
var
  pDone : TResultCall;
begin
  Result := False;  // Default

  if (iAsciiExportHandle <> 0) then
  try
    if (iAsciiExportHandle <> 0) then
    try

      // Exportliste freigeben
      @pDone := GetProcAddress(
        iAsciiExportHandle, C_DLL_AsciiExport_DoneExport);
      if (@pDone <> nil) then pDone;
      // Objekte freigeben
      @pDone := GetProcAddress(
        iAsciiExportHandle, C_DLL_AsciiExport_DoneDll);
      if (@pDone <> nil) then Result := pDone;

    finally
      FreeLibrary(iAsciiExportHandle);
    end;
    iAsciiExportHandle := 0;
  except
  // Result ist False
    iAsciiExportHandle := 0;
  end;
end;

{ Aktualisiert die Export-Dll                 }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function Aktualisieren: boolean;  // 21.10.2002
{---------------------------------------------}
begin
  Result := False;  // Default

  try
    Result := (DoneDll) and (InitDll);
  except
  // Result ist False
  end;
end;

{ Initialisiert Export                        }
{ Parameter: Geräte-ID, Export-Typen, Prefix, }
{            von./bis-DatumZeit, Exportpfad,  }
{            Definitionsdatei                 }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function InitASCIIExport(cGerTyp: char; iGerId: integer; pExportSet: TExportSet;
  sExportPrefix: string; dtVon, dtBis: TDateTime; sFilePath: TFilename = '';
  sDefFile: TFilename = ''; bExcel: boolean = False): boolean;   // 08.06.2001
{---------------------------------------------}
var
  pGetInit : TInitAsciiExport;
begin
  Result := False;  // Default

  try
    if (iAsciiExportHandle = 0) then
      iAsciiExportHandle := LoadLibrary(PChar(GetWieserPath + C_DLL_AsciiExport));
    if (iAsciiExportHandle <> 0) then begin
      @pGetInit := GetProcAddress(
        iAsciiExportHandle, C_DLL_AsciiExport_InitExport);
      if (@pGetInit <> nil) then
        Result := pGetInit(cGerTyp, iGerId, pExportSet, sExportPrefix, dtVon,
          dtBis, sFilePath, sDefFile, bExcel);
    end;
  except
  // Result ist False
  end;
end;

{ Initialisiert Export mit Stammdatenrecord   }
{ Parameter: Geräte-ID, Export-Typen, Prefix, }
{            Stammdaten, von./bis-DatumZeit,  }
{            Exportpfad, Definitionsdatei     }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function InitASCIIExportWithData(iGerId: integer; pExportSet: TExportSet;
  pInfoRec: TMExportInfoDataRec; sExportPrefix: string; dtVon, dtBis: TDateTime;
  sFilePath: TFilename = ''; sDefFile: TFilename = ''): boolean;
{---------------------------------------------}
var
  pGetInit : TInitAsciiExportWithData;
begin
  Result := False;  // Default

  try
    if (iAsciiExportHandle = 0) then
      iAsciiExportHandle := LoadLibrary(PChar(GetWieserPath + C_DLL_AsciiExport));

    if (iAsciiExportHandle <> 0) then begin
      @pGetInit := GetProcAddress(
        iAsciiExportHandle, C_DLL_AsciiExport_InitExportWithData);
      if (@pGetInit <> nil) then
        Result := pGetInit('M', iGerId, pExportSet, pInfoRec, sExportPrefix, dtVon,
          dtBis, sFilePath, sDefFile);
    end;

  except
  // Result ist False
  end;
end;

{ Legt neues Exportfile an                    }
{ Parameter: Export-Typ, Quelldat., Kanalzahl,}
{            Von-Bis-Datum                    }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function NewASCIIExportFile(cGerTyp: char; iGerId: integer;
  pExportTyp: TASCIIExportTyp; sFilename: TFilename; iRecKanalZahl: byte;
  dtVon: TDateTime = 0; dtBis: TDateTime = 0;
  bLgzStruc: boolean = True): boolean;  // 08.06.2001
{---------------------------------------------}
var
  pGetNewFile : TNewAsciiExportFile;
begin
  Result := False;  // Default

  if (iAsciiExportHandle <> 0) then
  try

    @pGetNewFile := GetProcAddress(
      iAsciiExportHandle, C_DLL_AsciiExport_NewExportFile);
    if (@pGetNewFile <> nil) then Result := pGetNewFile(cGerTyp, iGerId,
      pExportTyp, sFilename, iRecKanalZahl, dtVon, dtBis, bLgzStruc);

  except
  // Result ist False
  end;
end;

{ Legt neues Exportfile an                    }
{ Parameter: Export-Typ, Quellobjekt, von-bis }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function NewASCIIExportObject(cGerTyp: char; iGerId: integer;
  pExportTyp: TASCIIExportTyp; var pObject; dtVon: TDateTime = 0;
  dtBis: TDateTime = 0): boolean;
{---------------------------------------------}
var
  pGetNewObject : TNewAsciiExportObject;
begin
  Result := False;  // Default

  if (iAsciiExportHandle <> 0) then
  try

    @pGetNewObject := GetProcAddress(
      iAsciiExportHandle, C_DLL_AsciiExport_NewExportObject);
    if (@pGetNewObject <> nil) then
      Result := pGetNewObject(
        cGerTyp, iGerId, pExportTyp, pObject, dtVon, dtBis);

  except
  // Result ist False
  end;
end;

{ Speichert die Exportfiles endgültig         }
{ Parameter: Abschlußdatei schreiben ja/nein  }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function SaveASCIIExportFiles(bFinalize: boolean = True): boolean;
{---------------------------------------------}
var
  pGetSave : TParamBoolResultCall;
begin
  Result := False;
  if (iAsciiExportHandle <> 0) then
  try

    @pGetSave := GetProcAddress(
      iAsciiExportHandle, C_DLL_AsciiExport_SaveExportFiles);
    if (@pGetSave <> nil) then Result := pGetSave(bFinalize);

  except
  // Result ist False
  end;
end;

{ Gibt Export-Objekt frei                     }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function DoneASCIIExport: boolean;
{---------------------------------------------}
var
  pGetDone : TResultCall;
begin
  Result := False;  // Default

  if (iAsciiExportHandle <> 0) then
  try
    if (iAsciiExportHandle <> 0) then
    try

      @pGetDone := GetProcAddress(
        iAsciiExportHandle, C_DLL_AsciiExport_DoneExport);
      if (@pGetDone <> nil) then begin
        if (not pGetDone) then Exit;
        Result := True;
      end;

    finally
//      FreeLibrary(iAsciiExportHandle);  // 18.07.2001
    end;
//    iAsciiExportHandle := 0;            // 18.07.2001
  except
  // Result ist False
//    iAsciiExportHandle := 0;            // 18.07.2001
  end;
end;

{ Erstellt Datenfile für Archivexport         }
{ Parameter: Ausgabedatei, Archivdatenbank,   }
{            InstanzId, Archivgrpnr., Kanäle  }
{            als set, Zeitraum von-bis        }
{ Rückgabe: Erfolg ja/nein                    }
{---------------------------------------------}
function BuildDArchiveExportFile(sFileName: TFileName; sDatabaseName: string;
  iInstanzId: integer; iArchivGrpNr: byte; pKanaele: TByteSet;
  dtVon, dtBis: TDateTime): boolean;
{---------------------------------------------}
var
  p : TDArchivFileCall;
begin
  Result := False;  // Default
  if (iAsciiExportHandle <> 0) then
  try

    @p := GetProcAddress(iAsciiExportHandle, 'BuildDArchiveExportFile');
    if (@p <> nil) then
      Result := p(sFileName, sDatabaseName, iInstanzId, iArchivGrpNr, pKanaele,
        dtVon, dtBis);

  except
  // Result ist False
  end;
end;

{ Setzt Flag für Fileoutput Debug-Meldungen   }
{ Parameter: Debugmode: Ja/Nein               }
{---------------------------------------------}
procedure SetASCIIDebugFlag(bValue: boolean); register;  // 07.08.2001
{---------------------------------------------}
var
  p : TBoolProc;
begin
  if (iAsciiExportHandle <> 0) then begin
    @p := GetProcAddress(iAsciiExportHandle, 'SetDebugFlag');
    if (@p <> nil) then p(bValue);
  end;
end;

initialization
  InitDll;

finalization
  DoneDll;

end.
