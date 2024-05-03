{******************************************************************************}
{* Unit: Modem initialisieren für DSfG-DFÜ                                    *}
{* Version: 18.11.1999  WW                                                    *}
{******************************************************************************}
unit DMoInit;

interface

uses
  Windows, Forms, SysUtils, WComm, SerDSfG, AbrfInfo, DZustand, ModemIni,
  PathIni, SrvCfgIni, WStrUtils, DSysCon, T_Zeit, WChars;


procedure Modem_Initialisieren (SerialDSfG: TSerialDSfG);

IMPLEMENTATION

uses
  FMain;

{----------------------------}
procedure ShowModemInitFehler;
{----------------------------}
begin
  FormMainDSfGAbruf.SetError (true);
  FormMainDSfGAbruf.SetStatusColor (ps_ModemInitFehler);
  ZustandMessage (-1, -1, z_ModemInitFehler, '', false);
end;

{-----------------------------------------------}
procedure ResetRTS_DTR (SerialDSfG: TSerialDSfG);
{-----------------------------------------------}
{ Rücksetzen und Setzen der RTS/DTR-Leitungen }
begin
  SerialDSfG.ClearRTSSignal;
  SerialDSfG.ClearDTRSignal;
  Delay (5000);
  SerialDSfG.SetDTRSignal;
  SerialDSfG.SetRTSSignal;
end;

{-------------------------------------------------------}
procedure Modem_Initialisieren (SerialDSfG: TSerialDSfG);
{-------------------------------------------------------}
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
      ModemName:=SI.Modem [SerialDSfG.COMPort];
    finally
      SI.Free;
    end;
    S:=MI.GetDSfGInitString(ModemName);
    ResetStr:=F_Zerlegen (S, CInitTrenner);     { "Reset"-String (für Werkseinstellung laden) }
    InitStr:=S;                                 { "Init"-String (für spezielle Modemeinstellungen) }
  finally
    MI.Free;
  end;

  SerialDSfG.SetDCDCheck (false);

  Initialisiert:=false;
  while not Initialisiert And not Application.Terminated do begin
    Sleep (1);
    FormMainDSfGAbruf.SetStatusColor (ps_ModemInit);
    ZustandMessage (-1, -1, z_ModemInit, '', false);
    { "Reset": }
    if length (ResetStr) > 0 then begin
      if not SerialDSfG.SendModemCommand (ResetStr+CR, Timeout_ModemInit, R) then begin
        ShowModemInitFehler;
        Application.ProcessMessages;
        ResetRTS_DTR (SerialDSfG);
        Continue;
      end;
      if Pos (CModemAntwortOK, R.Antwort) = 0 then begin
        ShowModemInitFehler;
        Application.ProcessMessages;
        ResetRTS_DTR (SerialDSfG);
        Continue;
      end;
    end;

    { "Init": }
    if length (InitStr) > 0 then begin
      if not SerialDSfG.SendModemCommand (InitStr+CR, Timeout_ModemInit, R) then begin
        ShowModemInitFehler;
        Application.ProcessMessages;
        ResetRTS_DTR (SerialDSfG);
        Continue;
      end;
      if Pos (CModemAntwortOK, R.Antwort) = 0 then begin
        ShowModemInitFehler;
        Application.ProcessMessages;
        ResetRTS_DTR (SerialDSfG);
        Continue;
      end;
    end;
    Initialisiert:=true;
  end;  { while }

  if Initialisiert then begin
    FormMainDSfGAbruf.SetError (false);
    FormMainDSfGAbruf.SetStatusColor (ps_KeineVerbindung);
    ZustandMessage (-1, -1, z_Bereit, '', false);
  end;
end;

end.
