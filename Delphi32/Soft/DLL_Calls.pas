{------------------------------------------------------------------------------}
{ Unit zum Aufrufen von DLLs                                                   }
{                                                                              }
{ 22.11.2000  GD  Neu                                                          }
{ 05.06.2001  GD  DFÜ-Parametrierung                                           }
{ 15.11.2006  GD  Journalaufruf                                                }
{ 29.10.2009  WN  Aufruf Zählpunkt-Verwaltung                                  }
{ 19.06.2013  GD  Logging für TabGen                                           }
{ 23.10.2018  GD  Erweiterter Journalaufruf mit Gruppen-Filter                 }
{ 28.05.2019  WW  Anzeige des WICO22-Abrufjournals maximiert                   }
{                                                                              }
{ Copyright(C) Karl Wieser GmH 2000, RMG Messtechnik GmbH 2019                 }
{------------------------------------------------------------------------------}
unit DLL_Calls;

interface

uses
  Windows, SysUtils, Forms, PathIni, DbTables, Controls;

// DSfG-Monitor für DSfGCard
function DLLCallMonitor: TForm;
// DSfG-Monitor-Files für DSfGCard
function DLLCallFileView: TForm;

// Tabellen-Generierung via Script
function CreateAllTablesFromScript(
  sScript: TFileName = ''; iStatus: byte = 1; iLog: byte = 0): boolean;
function CopyIntoNewTable(sDatabaseName, sCopyTable, sNewTable: string;
  sScript: TFileName = ''; iStatus: byte = 0): boolean;

// DFÜ-Parametrierung
procedure DFU_Parametrierung(iStationsId: integer);

procedure DLLCallJournal;         // 15.11.2006
procedure DLLCallJournalExtended (sGrpFilter: string);  // 23.10.2018
procedure DLLCallZpktVerw;        // 29.10.2009

implementation

const
  C_DLL_DSfGMon32 = 'DSfGMon32.DLL';     // DLL
  C_DLL_DSfGFiles = 'DSfGFiles.DLL';     // DLL

  C_DLL_DSfGMon32_Aufruf = 'Aufruf';  // Methode


  C_DLL_TabGen = 'TabGen.DLL';     // DLL

  C_DLL_TabGen_CreateAll = 'CreateAllTables';  // Methode
  C_DLL_TabGen_CopyNew = 'CopyTable';  // Methode


  C_DLL_FDFUParam = 'FDFUParam.DLL';     // DLL

  C_DLL_FDFUParam_Param = 'DFU_Parametrierung';  // Methode

  // Aufruf des WICO22-Abrufjournals als modales Fenster  // 15.11.2006
  C_DLL_Wico22Journal                = 'JournalDLL';     // DLL
  C_DLL_Wico22Journal_GetJournalCtrl = 'GetJournalCtrl'; // Methode
  C_DLL_Wico22Journal_GetJournalCtrlExtended = 'GetJournalCtrlExtended'; // Methode; 23.10.2018, WW

  // Aufruf Zählpunkt-Verwaltung als modales Fenster         // 29.10.2009
  C_DLL_Wico22ZpktVerw                 = 'ZpktVerwDLL';      // DLL
  C_DLL_Wico22ZpktVerw_GetZpktVerwCtrl = 'GetZpktVerwCtrl';  // Methode

type
  TDLLFormCall = function (): TForm;
  TDLLIntProc = procedure (i: integer);
  TTabGenCreate = function(sScript: string; iStatus: integer; iLog: byte): boolean;
  TTabGenCopy = function(
    sScript, sDatabase, sCopyTable, sNewTable: string; iStatus: integer): boolean;
  TWinCtrlCall = function: TWinControl;
  TWinCtrlCallExtended = function(sGrpFilter: string): TWinControl;  // 23.10.2018, WW
  TZpktVerwProc = procedure ();  // 29.10.2009  WN

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

{------------------------------ DLL-Aufrufe -----------------------------------}

{ Aufruf der DSfG-Monitor-DLL        }
{ Rückgabe: Formular                 }
{------------------------------------}
function DLLCallMonitor: TForm;
{------------------------------------}
var
  iHandle     : integer;
  pGetMonitor : TDLLFormCall;
  s           : string;
begin
  Result := nil;  // Default

  s := Session.PrivateDir;  // 05.08.2004
  try
    iHandle := LoadLibrary(C_DLL_DSfGMon32);
    if (iHandle <> 0) then begin
      @pGetMonitor := GetProcAddress(iHandle, C_DLL_DSfGMon32_Aufruf);
      if (@pGetMonitor <> nil) then Result := pGetMonitor;

    // else RaiseLastWin32Error;
    end;
  // else RaiseLastWin32Error;
  finally
    Session.PrivateDir := s;
  end;
end;

{ Aufruf der DSfG-Monitor-Files      }
{ Rückgabe: Formular                 }
{------------------------------------}
function DLLCallFileView: TForm;
{------------------------------------}
var
  iHandle     : integer;
  pGetMonitor : TDLLFormCall;
  s           : string;
begin
  Result := nil;  // Default

  s := Session.PrivateDir;  // 05.08.2004
  try
    iHandle := LoadLibrary(C_DLL_DSfGFiles);
    if (iHandle <> 0) then begin
      @pGetMonitor := GetProcAddress(iHandle, C_DLL_DSfGMon32_Aufruf);
      if (@pGetMonitor <> nil) then Result := pGetMonitor;

    // else RaiseLastWin32Error;
    end;
  // else RaiseLastWin32Error;
  finally
    Session.PrivateDir := s;
  end;
end;

{ Erzeugt alle Tabellen e. Scripts   }
{ Parameter: Scriptname (''=EXE.WTS) }
{         Status (0-Ign,1-Mod,2-New) }
{         Logging (0-nein,1-ja)      }   // 19.06.2013
{ Rückgabe: Erfolg Ja/Nein           }
{------------------------------------}
function CreateAllTablesFromScript(
  sScript: TFileName = ''; iStatus: byte = 1; iLog: byte = 0): boolean;
{------------------------------------}
var
  iHandle    : integer;
  pGetCreate : TTabGenCreate;
  s          : string;
begin
  Result := False;  // Default

  s := Session.PrivateDir;  // 05.08.2004
  try
    iHandle := LoadLibrary(C_DLL_TabGen);
    if (iHandle <> 0) then
    try
      @pGetCreate := GetProcAddress(iHandle, C_DLL_TabGen_CreateAll);
      if (@pGetCreate <> nil) then Result := pGetCreate(sScript, iStatus, iLog);
    finally
      FreeLibrary(iHandle);
    end;
  finally
    Session.PrivateDir := s;
  end;
end;

{ Kopiert Struktur in neue Tabelle   }
{ Parameter: DatabaseName, Muster-   }
{            tabelle, neue Tabelle   }
{         Status (0-Ign,1-Mod,2-New) }
{ Rückgabe: Erfolg Ja/Nein           }
{------------------------------------}
function CopyIntoNewTable(sDatabaseName, sCopyTable, sNewTable: string;
  sScript: TFileName = ''; iStatus: byte = 0): boolean;
{------------------------------------}
var
  iHandle  : integer;
  pGetCopy : TTabGenCopy;
  s        : string;
begin
  Result := False;  // Default

  s := Session.PrivateDir;  // 05.08.2004
  try
    iHandle := LoadLibrary(C_DLL_TabGen);
    if (iHandle <> 0) then
    try
      @pGetCopy := GetProcAddress(iHandle, C_DLL_TabGen_CreateAll);
      if (@pGetCopy <> nil) then
        Result := pGetCopy(sScript, sDatabaseName, sCopyTable, sNewTable, iStatus);
    finally
      FreeLibrary(iHandle);
    end;
  finally
    Session.PrivateDir := s;
  end;
end;

{------------------------------------}
procedure DFU_Parametrierung(iStationsId: integer);  // 05.06.2001
{------------------------------------}
var
  iHandle : integer;
  pProc   : TDLLIntProc;
  s       : string;
begin
  s := Session.PrivateDir;  // 05.08.2004
  try
    iHandle := LoadLibrary(PChar(GetWieserPath + C_DLL_FDFUParam));
    if (iHandle <> 0) then
    try
      @pProc := GetProcAddress(iHandle, C_DLL_FDFUParam_Param);
      if (@pProc <> nil) then pProc(iStationsId);
    finally
      FreeLibrary(iHandle);
    end;
  finally
    Session.PrivateDir := s;
  end;
end;

{ Aufruf des WICO22-Abrufjournals    }
{------------------------------------}
procedure DLLCallJournal;         // 15.11.2006
{------------------------------------}
var
  iHandle     : integer;
  pGetJournal : TWinCtrlCall;
begin
  iHandle := LoadLibrary(PChar(GetWieserPath + C_DLL_Wico22Journal));
  if (iHandle <> 0) then begin
    @pGetJournal := GetProcAddress(iHandle, C_DLL_Wico22Journal_GetJournalCtrl);
    if (@pGetJournal <> nil) then
      with TForm(pGetJournal) do begin
        WindowState:=wsMaximized;  // Programm-Fenster maximiert; 28.05.2019, WW
        ShowModal;
        Free;
      end;
  end;
end;

{ Erweiterter Aufruf des WICO22-Abrufjournals
  -> mit Gruppen-Filter }
{------------------------------------}
procedure DLLCallJournalExtended (sGrpFilter: string);  // 23.10.2018
{------------------------------------}
var
  iHandle     : integer;
  pGetJournal : TWinCtrlCallExtended;
begin
  iHandle := LoadLibrary(PChar(GetWieserPath + C_DLL_Wico22Journal));
  if (iHandle <> 0) then begin
    @pGetJournal := GetProcAddress(iHandle, C_DLL_Wico22Journal_GetJournalCtrlExtended);
    if (@pGetJournal <> nil) then
      with TForm(pGetJournal(sGrpFilter)) do begin
        WindowState:=wsMaximized;  // Programm-Fenster maximiert; 28.05.2019, WW
        ShowModal;
        Free;
      end;
  end;
end;

// -----------------------------------------------------------------------------
// 29.10.2009  WN
// Aufruf der Zählpunkt-Verwaltung für MSCONS-Export
// -----------------------------------------------------------------------------
procedure DLLCallZpktVerw;
// -----------------------------------------------------------------------------
var
  iHandle : integer;
  pProc   : TZpktVerwProc;
begin
  iHandle := LoadLibrary(PChar(GetWieserPath + C_DLL_Wico22ZpktVerw));
  if (iHandle <> 0) then
  try
    @pProc := GetProcAddress(iHandle, C_DLL_Wico22ZpktVerw_GetZpktVerwCtrl);
    if (@pProc <> nil) then pProc;
  finally
    FreeLibrary(iHandle);
  end;
end;

end.
