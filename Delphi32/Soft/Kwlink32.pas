{------------------------------------------------------------------------------}
{ 04.01.1999 GD; Unit für Message-Handling                                     }
{ 02.02.2002 GD; Erweitert um 'Schließen'-Methoden                             }
{ 03.12.2008 NW; Erweitert um Nachrichten Import32-Programm und FileScanner    }
{ 20.04.2009 NW; Erweitert um Nachrichten Programm Mittelwert-Ermittlung       }
{                                                                              }
{  (C) Karl Wieser GmbH 1999, 2009                                             }
{------------------------------------------------------------------------------}
unit Kwlink32;

interface

uses
  windows, SysUtils, messages,
  InstTest, PathIni;

const
  C_ClassNameReceiver = 'TFormMainMRGAbruf';
  C_DSfGAbrufModul    = 'TFormMainDSfGAbruf';
  C_ClassName_ALM32   = 'TFAuftrag';  // 03.02.2002
  C_ProgName_ALM32    = 'ALM32.EXE';  // 03.02.2002

  WM_KWLink           = WM_USER + 777;

  { Abruf-Manager }
  KWE_MrgAbrufzeit_erreicht  = 100;
  KWE_AbrufManager_Beendet   = 101;
  KWE_LAKSAbrufzeit_erreicht = 102;
  KWE_DSfGAbrufzeit_erreicht = 103;

  { Abruf-Manager Import32Proc }
  KWE_Import32Proc_Beendet = 400;  // 03.12.2008

  { Abruf-Manager MittelwertProc }
  KWE_MittelwertProc_Beendet = 500;  // 20.04.2009

  { Abrufprozess-Exe für WFileScanSrv }
  C_ClassName_FormMainWFileScan = 'TFormMainWFileScanSrv';  // 03.12.2008


function SendToReceiver(
  anEvent, anInfo: integer; aClassName: string = C_ClassNameReceiver): integer;
function CloseThisWindow(aClassName: string; iMsg: integer = WM_Close): integer;
function OpenAlm32: byte;
function CloseALM32: integer;

implementation

{ Sendet 'C_KWLinkMessage' an Fenster mit 'aClassName'                    }
{ Parameter: anEvent: 1. Parameter; anInfo: 2. Parameter                  }
{            aClassName: Name des Empfängers                              }
{ Ergebnis: Anzahl der Empfänger                                          }
{-------------------------------------------------------------------------}
function SendToReceiver(anEvent, anInfo: integer;
                        aClassName: string = C_ClassNameReceiver): integer;
{-------------------------------------------------------------------------}
var
  FWnd: HWnd;
  pc: PChar;
begin
  result:= 0; { default }
  getMem(pc, 100);
  try
    { Fängt bei dem ersten Fenster mit 'aClassName' an zu suchen }
    FWnd:= FindWindow(PChar(aClassName), nil);
    while FWnd <> 0 do begin
      GetClassName(FWnd, pc, 100);
      if StrPos(pc, PChar(aClassName)) <> nil then begin
        PostMessage (FWnd, WM_KWLink, anEvent, anInfo);
        result:= result+1;
      end;
    { Handle des nächsten Fensters }
      FWnd:= GetNextWindow(FWnd, GW_HWNDNEXT);
    end;
  finally
    freeMem(pc, 100);
  end;
end;

{ Sendet 'WM_Close' an alle Fenster mit 'aClassName'                      }
{ Parameter: aClassName: Name des Empfängers                              }
{ Ergebnis: Anzahl der Empfänger                                          }
{-------------------------------------------------------------------------}
function CloseThisWindow(aClassName: string; iMsg: integer = WM_Close): integer;
{-------------------------------------------------------------------------}
var
  iWnd: HWnd;
  pc: PChar;
begin
  Result := 0; { default }
  GetMem(pc, 100);
  try
    { Fängt bei dem ersten Fenster mit 'aClassName' an zu suchen }
    iWnd:= FindWindow(PChar(aClassName), nil);
    while (iWnd <> 0) do begin
      GetClassName(iWnd, pc, 100);
      if StrPos(pc, PChar(aClassName)) <> nil then begin
        PostMessage (iWnd, iMsg, 0, 0);
        Inc(Result);
      end;
    { Handle des nächsten Fensters }
      iWnd:= GetNextWindow(iWnd, GW_HWNDNEXT);
    end;
  finally
    FreeMem(pc, 100);
  end;
end;

{ Sendet 'WM_Close' an alle Fenster mit ALM                               }
{ Ergebnis: Anzahl der Empfänger                                          }
{-------------------------------------------------------------------------}
function CloseALM32: integer;
{-------------------------------------------------------------------------}
begin
  Result := CloseThisWindow(C_ClassName_ALM32);
end;

{ Startet lokalen ALM32, falls noch nicht geschehen                       }
{ Ergebnis: 0=nicht vorhanden, 1=jetzt gestartet, 2=ALM war bereits aktiv }
{           3=vorhanden, aber WinExec scheitert                           }
{-------------------------------------------------------------------------}
function OpenAlm32: byte;
{-------------------------------------------------------------------------}
var
  s    : string;
begin
  with TProgramIni.Create do
  try
    with TPathServer.Create(WieserIniFile, [WNetProgDir]) do
    try
      s := IncludeTrailingBackslash(Pathname[WNetProgDir]) + C_ProgName_ALM32;
    finally
      Free;
    end;
  finally
    Free;
  end;

  if (not FileExists(s)) then Result := 0
  else begin
    if (AnzahlInstanzen(C_ClassName_ALM32, '') > 0) then Result := 2
    else begin
      if (WinExec(PChar(s), SW_NORMAL) > 31) then Result := 1 else Result := 3;
    end;
  end;
end;

end.
