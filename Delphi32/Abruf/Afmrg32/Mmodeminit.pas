{******************************************************************************}
{* Unit: Modem initialisieren für MRG-Abruf                                   *}
{* Version: 05.12.2000  WW                                                    *}
{* 25.09.2001  WW  FUP-Reset angepaßt für FUP-1200                            *}
{* 17.09.2002  H.-P.R. FUP-Reset auch mit RTS Signal                          *}
{******************************************************************************}

unit MModemInit;

interface

uses
  Windows, Forms, SysUtils, SerMrg, SerMrgModem, SerMrgFup, AbrfInfo, MZustand,
  ModemIni, PathIni, SrvCfgIni, WStrUtils, WChars, T_Zeit, ErrConst, MSysCon,
  Serial, WComm;

procedure Fup_Reset (SerialMRGFup: TSerialMRGFup);
function Fup_Init (SerialMRGFup: TSerialMRGFup): boolean;
procedure Modem_Initialisieren (SerialMRGModem: TSerialMRGModem; MRG800PTB: boolean);

IMPLEMENTATION

uses
  FMain;

{---------------------------}
procedure ShowFupResetFehler;
{---------------------------}
begin
  FormMainMRGAbruf.SetError (true);
  FormMainMRGAbruf.SetStatusColor (ps_FupModemInitFehler);
  ZustandMessage (-1, -1, z_FupResetFehler, '', false);
  Delay (5000);
end;

{--------------------------}
procedure ShowFupInitFehler;
{--------------------------}
begin
  FormMainMRGAbruf.SetError (true);
  FormMainMRGAbruf.SetStatusColor (ps_FupModemInitFehler);
  ZustandMessage (-1, -1, z_FupInitFehler, '', false);
  Delay (5000);
end;

{----------------------------}
procedure ShowModemInitFehler;
{----------------------------}
begin
  FormMainMRGAbruf.SetError (true);
  FormMainMRGAbruf.SetStatusColor (ps_FupModemInitFehler);
  ZustandMessage (-1, -1, z_ModemInitFehler, '', false);
  { Delay erfolgt in ResetRTS_DTR-Routine }
end;

{------------------------------------------------}
procedure Fup_Reset (SerialMRGFup: TSerialMRGFup);
{------------------------------------------------}
var
  ResetOK: boolean;
  R: TRueckgabe;
  dummy: boolean;
  Version: string;

begin
  ResetOK:=false;
  while not ResetOK And not Application.Terminated do begin
    Sleep (1);
    FormMainMRGAbruf.SetStatusColor (ps_FupModemInit);
    ZustandMessage (-1, -1, z_FupReset, '', false);

    { FUP zurücksetzen (Break, DTR-Pegel für verschiedene (?) FUP-Versionen): }
    SerialMRGFup.SetBreakZustand;                  // WW  25.09.2001
    Delay (500);
    SerialMRGFup.ClearBreakZustand;
    Delay (500);

    SerialMRGFup.ClearDTRSignal;
    Delay (500);
    SerialMRGFup.SetDTRSignal;
    Delay (500);

    SerialMRGFup.ClearRTSSignal;                   // H.-P.R. 17.09.02 wg GVT
    Delay (500);
    SerialMRGFup.SetRTSSignal;

    Delay (1000);    { kurze Wartezeit, sonst wird der erste Befehl vom FUP evtl.
                       nicht erkannt (WW  25.09.2001) }
    { Wahlinformation abfragen als Funktionstest: }
    if not SerialMRGFup.SendCommand (ESC+'w'+CR, [CR], 1, Timeout_FupReset, ad_String, R, dummy) then begin
      ShowFupResetFehler;
      Application.ProcessMessages;
      Continue;
    end;
    if ExtractString (R.Antwort, ESC, CR, 0) <> 'W' then begin      { FUP-Antwort nicht ok }
      ShowFupResetFehler;
      Application.ProcessMessages;
      Continue;
    end;

    { FUP-Version abfragen: }
    ZustandMessage (-1, -1, z_FupVersion, '', false);
    if not SerialMRGFup.SendCommand (ESC+'V'+CR, [CR], 1, Timeout_FupAntwort, ad_String, R, dummy) then begin
      { falls keine Antwort kommt, hängt ein alter Fup-1200 dran, der kennt keinen V-Befehl ! }
      if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) then
        Version:='FUP-1200'
      else begin
        ShowFupResetFehler;
        Application.ProcessMessages;
        Continue;
      end;
    end else
      Version:=ExtractString (R.Antwort, ESC, CR, 0);
    FormMainMRGAbruf.FupVersionMessage (Version);
    SerialMRGFup.isFup1200:=Pos ('1200', Version) <> 0;

    { FUP initialisieren: }
    ResetOK:=Fup_Init (SerialMrgFup);
  end;  { while not ResetOK }

  if ResetOK then begin
    FormMainMRGAbruf.SetError (false);
    FormMainMRGAbruf.SetStatusColor (ps_KeineVerbindung);
    ZustandMessage (-1, -1, z_Bereit, '', false);
  end;
end;

{-------------------------------------------------------}
function Fup_Init (SerialMRGFup: TSerialMRGFup): boolean;
{-------------------------------------------------------}
var
  SI: TSrvCfg32Ini;
  S, FupInitStr, InitStr: string;
  Befehl: string;
  BefehlChar: char;
  R: TRueckgabe;
  dummy: boolean;

begin
  Result:=false;
  FormMainMRGAbruf.SetStatusColor (ps_FupModemInit);
  ZustandMessage (-1, -1, z_FupInit, '', false);

  { FUP-Initialisierungsstring lesen: }
  SI:=TSrvCfg32Ini.Create (PathServer.Pathname [WNetProgDir]);
  try
    FupInitStr:=SI.FupInit [SerialMRGFup.COMPort];
  finally
    SI.Free;
  end;

  { FUP initialisieren, InitStr enthält durch Trennzeichen abgegrenzte Teil-Initialisierungen: }
  InitStr:=FupInitStr;
  while (length (InitStr) > 0) AND not Application.Terminated do begin
    S:=F_Zerlegen (InitStr, CInitTrenner);
    if length (S) > 0 then begin
      BefehlChar:=S[1];
      if IsCharUpper (BefehlChar) then begin
        if SerialMRGFup.isFup1200 AND (BefehlChar in ['S', 'M']) then begin
          Application.ProcessMessages;
          Continue;              { S- und M-Sequenz gibts im FUP-1200 nicht }
        end;
        Befehl:=ESC+S+CR+ESC+LowerCase (BefehlChar)+CR;
        if not SerialMRGFup.SendCommand (Befehl, [CR], 1, Timeout_FupAntwort, ad_String, R, dummy) then begin
          ShowFupInitFehler;
          Application.ProcessMessages;
          exit;
        end;
        if ExtractString (R.Antwort, ESC, CR, 0) <> S then begin     { FUP-Antwort nicht ok }
          ShowFupInitFehler;
          Application.ProcessMessages;
          exit;
        end;
      end;
    end;
  end;  { while length (InitStr) }

  FormMainMRGAbruf.SetError (false);
  FormMainMRGAbruf.SetStatusColor (ps_KeineVerbindung);
  ZustandMessage (-1, -1, z_Bereit, '', false);
  Result:=true;
end;

{-------------------------------------------------------}
procedure ResetRTS_DTR (SerialMRGModem: TSerialMRGModem);
{-------------------------------------------------------}
{ Rücksetzen und Setzen des RTS/DTR-Signals }
begin
  SerialMRGModem.ClearRTSSignal;
  SerialMRGModem.ClearDTRSignal;
  Delay (5000);
  SerialMRGModem.SetDTRSignal;
  SerialMRGModem.SetRTSSignal;
end;

{-----------------------------------------------------------------------------------}
procedure Modem_Initialisieren (SerialMRGModem: TSerialMRGModem; MRG800PTB: boolean);
{-----------------------------------------------------------------------------------}
Const
  CModemAntwortOK = 'OK';
var
  ModemName: string;
  S, ResetStr, InitStr: string;
  MI: TModemIni;
  SI: TSrvCfg32Ini;
  Initialisiert: boolean;
  R: TRueckgabe;

begin
  { Initialisierungsstrings lesen: }
  MI:=TModemIni.Create (PathServer.Pathname [WNetProgDir]);
  try
    SI:=TSrvCfg32Ini.Create (PathServer.Pathname [WNetProgDir]);
    try
      ModemName:=SI.Modem [SerialMRGModem.COMPort];
    finally
      SI.Free;
    end;
    if MRG800PTB then
      S:=MI.GetMRG800PTBInitString(ModemName)
    else
      S:=MI.GetMRGInitString(ModemName);
    ResetStr:=F_Zerlegen (S, CInitTrenner);     { "Reset"-String (für Werkseinstellung laden) }
    InitStr:=S;                                 { "Init"-String (für spezielle Modemeinstellungen) }
  finally
    MI.Free;
  end;

  { vor der Modem-Initialisierung müssen die Schnittstellen-Parameter auf die
    Standardwerte 9600,7,e,1 gesetzt werden, um:
    1. das Modem überhaupt ansprechen zu können (andere Datenformate können vom
       Modem evtl. nicht automatisch erkannt werden)
    2. einen ankommenden Ruf (RING) im definierten Datenformat erkennen zu können
    3. bei der Rufentgegennahme die MRG-Daten lesen zu können (es können nur Rufe
       von MRGs, die ihre Daten mit 7,e,1 senden ("FUP-Geräte"), entgegengenommen werden !) }
  SerialMRGModem.SetDCDCheck (false);

  Initialisiert:=false;
  while not Initialisiert And not Application.Terminated do begin
    Sleep (1);
    FormMainMRGAbruf.SetStatusColor (ps_FupModemInit);
    ZustandMessage (-1, -1, z_ModemInit, '', false);
    { "Reset": }
    if length (ResetStr) > 0 then begin
      if not SerialMRGModem.SendModemCommand (ResetStr+CR, Timeout_ModemInit, R) then begin
        ShowModemInitFehler;
        Application.ProcessMessages;
        ResetRTS_DTR (SerialMRGModem);
        Continue;
      end;
      if Pos (CModemAntwortOK, R.Antwort) = 0 then begin
        ShowModemInitFehler;
        Application.ProcessMessages;
        ResetRTS_DTR (SerialMRGModem);
        Continue;
      end;
    end;

    { "Init": }
    if length (InitStr) > 0 then begin
      if not SerialMRGModem.SendModemCommand (InitStr+CR, Timeout_ModemInit, R) then begin
        ShowModemInitFehler;
        Application.ProcessMessages;
        ResetRTS_DTR (SerialMRGModem);
        Continue;
      end;
      if Pos (CModemAntwortOK, R.Antwort) = 0 then begin
        ShowModemInitFehler;
        Application.ProcessMessages;
        ResetRTS_DTR (SerialMRGModem);
        Continue;
      end;
    end;
    Initialisiert:=true;
  end;  { while }

  if Initialisiert then begin
    FormMainMRGAbruf.SetError (false);
    FormMainMRGAbruf.SetStatusColor (ps_KeineVerbindung);
    ZustandMessage (-1, -1, z_Bereit, '', false);
  end;
end;

end.
