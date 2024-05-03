//------------------------------------------------------------------------------
// Steuerung von Wico22-Diensten mit StartAsService
//
// 10.06.2014  WW  Neu: Starten/Stoppen von Wico22-Diensten; Ansteuerung und
//                      Rückmeldung über Triggerdateien
//
// Copyright (C) RMG Messtechnik GmbH 2014
//------------------------------------------------------------------------------
unit O_WicoServiceControl;

interface

uses
  Windows, SysUtils, Classes, IniFiles,
  T_Zeit, Novell, GD_Utils, StartAsServiceConst;

type

  { Klasse zur Steuerung von Wico22-Diensten }

  TWicoServiceControl = class(TObject)
  private
    FSASIniPath: string;
    FLastError: string;
    function ReadSAS_StartStopTriggerFilename (bStart: boolean;
      sServiceName: string): string;
    function ReadSAS_StartTriggerFilename (sServiceName: string): string;
    function ReadSAS_StopTriggerFilename (sServiceName: string): string;
    function ReadResultTriggerfile (sTriggerFilename: string;
      var iResultCode: integer): boolean;
    function StartStopService (bStart: boolean; sServiceName: string): integer;
  protected
  public
    constructor Create (sSASIniPath: string);
    function StartService (sServiceName: string;
      var bAlreadyRunning: boolean): boolean;
    function StopService (sServiceName: string): boolean;
    function GetSAS_ResultTriggerFilename (sTriggerFilename: string): string;
    function WriteResultTriggerfile (sTriggerFilename: string;
      iResultCode: integer): boolean;
    property LastError: string read FLastError;
  end;

implementation

uses DateUtils;

const
  CTimeoutStartStop = 30;  // in s


// TWicoServiceControl

//------------------------------------------------------------------------------
constructor TWicoServiceControl.Create (sSASIniPath: string);
//------------------------------------------------------------------------------
begin
  inherited Create;
  FSASIniPath:=IncludeTrailingBackslash (sSASIniPath);

  FLastError:='';
end;

//------------------------------------------------------------------------------
function TWicoServiceControl.StartStopService (bStart: boolean;
  sServiceName: string): integer;
//------------------------------------------------------------------------------
{ Starten/Stoppen eines Dienstes;
  Übergabe: Flag 'Start/Stop'
            Name des Dienstes, der gestartet/gestoppt werden soll
  Ergebnis: Resultcode rc_SAS_... }
var
  sStartStop: string;
  sTriggerFilename: string;
  sResultTriggerFilename: string;
  bOK: boolean;
  dtTimeOut: TDateTime;
  bTimeout: boolean;
  iResultCode: integer;

begin
  if bStart then begin
    sStartStop:='Start service - Service name ' + sServicename + ': ';
    // Name der Triggerdatei zum Starten des Dienstes aus StartAsService-
    // Konfiguration lesen
    sTriggerFilename:=ReadSAS_StartTriggerFilename (sServiceName);

    if length (sTriggerFilename) = 0 then begin
      Result:=rc_SAS_StartTriggerdatei_nicht_definiert;
      FLastError:=sStartStop + Get_SAS_ResultcodeText (Result);
      exit;
    end;
  end
  else begin
    sStartStop:='Stop service - Service name ' + sServicename + ': ';
    // Name der Triggerdatei zum Stoppen des Dienstes aus StartAsService-
    // Konfiguration lesen
    sTriggerFilename:=ReadSAS_StopTriggerFilename (sServiceName);

    if length (sTriggerFilename) = 0 then begin
      Result:=rc_SAS_StopTriggerdatei_nicht_definiert;
      FLastError:=sStartStop + Get_SAS_ResultcodeText (Result);
      exit;
    end;
  end;

  // Pfad der Start/Stop-Triggerdatei prüfen
  if not DirectoryExists (ExtractFilePath (sTriggerFilename)) then begin
    Result:=rc_SAS_Pfad_Triggerdatei_existiert_nicht;
    FLastError:=sStartStop + Get_SAS_ResultcodeText (Result) +
      '; Trigger file: ' + sTriggerFilename;
    exit;
  end;

  // Name der Start/Stop-Ergebnisdatei, welche von StartAsService als Antwort
  // geschrieben wird
  sResultTriggerFilename:=GetSAS_ResultTriggerFilename (sTriggerFilename);
  // Start/Stop-Ergebnisdatei löschen
  if FileExists (sResultTriggerFilename) then begin
    if not DeleteFile (sResultTriggerFilename) then begin
      Result:=rc_SAS_Fehler_Loeschen_ErgebnisTriggerdatei;
      FLastError:=sStartStop + Get_SAS_ResultcodeText (Result) +
        '; Result trigger file: ' + sResultTriggerFilename;
      exit;
    end;
  end;

  // Start/Stop-Triggerdatei schreiben
  with TFileStreamExt.Create (sTriggerFilename, fmCreate, bOK) do Free;

  // auf Start/Stop-Ergebnisdatei warten
  Result:=rc_SAS_Timeout_ErgebnisTriggerdatei;
  FLastError:=sStartStop + Get_SAS_ResultcodeText (Result) +
    '; Result trigger file: ' + sResultTriggerFilename +
    '; Timeout: ' + IntToStr (CTimeoutStartStop) + ' s';  // Vorbelegung: Timeout-Fehler

  dtTimeOut:=Now;
  dtTimeOut:=IncSecond (dtTimeOut, CTimeoutStartStop);
  bTimeout:=false;
  while not bTimeout do begin
    Delay (100);
    if FileExists (sResultTriggerFilename) then begin
      // Resultcode aus Start/Stop-Ergebnisdatei lesen
      if ReadResultTriggerfile (sResultTriggerFilename, iResultCode) then
        Result:=iResultCode
      else
        Result:=rc_SAS_Fehler_Lesen_ErgebnisTriggerdatei;
      FLastError:=sStartStop + Get_SAS_ResultcodeText (Result) +
        '; Result trigger file: ' + sResultTriggerFilename;
      Break;
    end;  // if FileExists

    if (Now > dtTimeOut) then
      bTimeout:=true;  // Timeout
  end;  // while not bTimeout
end;

//------------------------------------------------------------------------------
function TWicoServiceControl.StartService (sServiceName: string;
  var bAlreadyRunning: boolean): boolean;
//------------------------------------------------------------------------------
{ Starten eines Dienstes;
  Übergabe: Name des Dienstes, der gestartet werden soll
  Rückgabe: Flag 'bAlreadyRunning' (true, wenn Dienst bereits läuft)
  Ergebnis: true, wenn Dienst erfolgreich gestartet wurde oder bereits läuft }
var
  iResultCode: integer;

begin
  iResultCode:=StartStopService (true, sServiceName);
  Result:=(iResultCode = rc_SAS_DienstGestartet_OK) OR
          (iResultCode = rc_SAS_DienstLaueft_OK);
  bAlreadyRunning:=(iResultCode = rc_SAS_DienstLaueft_OK);
end;

//------------------------------------------------------------------------------
function TWicoServiceControl.StopService (sServiceName: string): boolean;
//------------------------------------------------------------------------------
{ Stoppen eines Dienstes;
  Übergabe: Name des Dienstes, der gestoppt werden soll
  Ergebnis: true, wenn Dienst erfolgreich gestoppt wurde }
var
  iResultCode: integer;

begin
  iResultCode:=StartStopService (false, sServiceName);
  Result:=(iResultCode = rc_SAS_DienstGestoppt_OK);
end;

//------------------------------------------------------------------------------
function TWicoServiceControl.ReadSAS_StartStopTriggerFilename (bStart: boolean;
  sServiceName: string): string;
//------------------------------------------------------------------------------
{ Triggerdateiname zum Starten/Stoppen eines Dienstes aus StartAsService-
  Konfiguration lesen;
  Übergabe: Flag 'Start/Stop'
            Name des Dienstes
  Ergebnis: Name der Start/Stop-Triggerdatei }
var
  SASIniFile: TIniFile;
  sServiceNameIni: string;
  i: integer;

begin
  Result:='';
  SASIniFile:=TIniFile.Create (FSASIniPath + CStartAsServiceIni);
  try
    // Servicename in definierten Programm-Sections suchen
    with TStringList.Create do
    try
      CommaText := SASIniFile.ReadString(CIniSectionProgStart, CIniIdentProgSections, '');
      for i := 0 to Count-1 do begin
        sServiceNameIni := SASIniFile.ReadString(Strings[i], CIniIdentServicename, '');
        if SameText (sServiceNameIni, sServiceName) then begin
          if bStart then
            Result:=GDExpandFilePath (SASIniFile.ReadString (Strings[i], CIniIdentStartFile, ''),
                                      FSASIniPath)
          else
            Result:=GDExpandFilePath (SASIniFile.ReadString (Strings[i], CIniIdentStopFile, ''),
                                      FSASIniPath);
          Result:=ExpandUNCFileName (Result);
          Break;
        end;
      end;
    finally
      Free;
    end;
  finally
    SASIniFile.Free;
  end;
end;

//------------------------------------------------------------------------------
function TWicoServiceControl.ReadSAS_StartTriggerFilename (
  sServiceName: string): string;
//------------------------------------------------------------------------------
{ Name der Triggerdatei zum Starten eines Dienstes aus StartAsService-Konfiguration
  lesen;
  Übergabe: Name des Dienstes
  Ergebnis: Name der Start-Triggerdatei }
begin
  Result:=ReadSAS_StartStopTriggerFilename (true, sServiceName);
end;

//------------------------------------------------------------------------------
function TWicoServiceControl.ReadSAS_StopTriggerFilename (
  sServiceName: string): string;
//------------------------------------------------------------------------------
{ Name der Triggerdatei zum Stoppen eines Dienstes aus StartAsService-Konfiguration
  lesen;
  Übergabe: Name des Dienstes
  Ergebnis: Name der Stop-Triggerdatei }
begin
  Result:=ReadSAS_StartStopTriggerFilename (false, sServiceName);
end;

//------------------------------------------------------------------------------
function TWicoServiceControl.GetSAS_ResultTriggerFilename (
  sTriggerFilename: string): string;
//------------------------------------------------------------------------------
{ Liefert Ergebnis-Triggerdateiname zu einer Steuerungs-Triggerdatei }
begin
  Result:=ChangeFileExt (sTriggerFilename, CResultFileExtension + ExtractFileExt (sTriggerFilename));
end;

//------------------------------------------------------------------------------
function TWicoServiceControl.ReadResultTriggerfile (sTriggerFilename: string;
  var iResultCode: integer): boolean;
//------------------------------------------------------------------------------
{ Liest Resultcode aus einer Ergebnis-Triggerdatei;
  Übergabe: Name der Ergebnis-Triggerdatei
  Rückgabe: Resultcode rc_SAS_...
  Ergebnis: true, wenn Lesen erfolgreich }
var
  fsResult: TTextFileStreamExt;
  bOK: boolean;
  S: string;

begin
  iResultCode:=rc_SAS_Ergebnis_undefiniert;  // Vorbelegung: Rückgabe

  fsResult:=TTextFileStreamExt.Create (sTriggerFilename, fmOpenRead OR fmShareDenyWrite, bOK);
  try
    if bOK then begin
      fsResult.ReadLn (S);
      iResultCode:=StrToIntDef (S, rc_SAS_Ergebnis_undefiniert);
      Result:=true;
    end else
      Result:=false;
  finally
    fsResult.Free;
  end;
end;

//------------------------------------------------------------------------------
function TWicoServiceControl.WriteResultTriggerfile (sTriggerFilename: string;
  iResultCode: integer): boolean;
//------------------------------------------------------------------------------
{ Schreibt Ergebnis-Triggerdatei mit Resultcode;
  Übergabe: Name der Ergebnis-Triggerdatei
            Resultcode rc_SAS_...
  Ergebnis: true, wenn Schreiben erfolgreich }
var
  fsResult: TTextFileStreamExt;
  bOK: boolean;

begin
  fsResult:=TTextFileStreamExt.Create (sTriggerFilename, fmCreate, bOK);
  try
    if bOK then begin
      fsResult.WriteLn (IntToStr (iResultCode));
      Result:=true;
    end else
      Result:=false;
  finally
    fsResult.Free;
  end;
end;

end.

