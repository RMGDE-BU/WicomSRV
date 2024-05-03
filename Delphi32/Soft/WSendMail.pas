// -----------------------------------------------------------------------------
// Unit: E-Mail versenden per Aufruf des WSendMail-Konsolenprogramms
// -> Die Konfiguration des E-Mail-Servers ist in WSendMail.ini zentral hinterlegt.
//
// 28.06.2023  WW  Neu
//
// Copyright © RMG Messtechnik GmbH 2023
// -----------------------------------------------------------------------------
unit WSendMail;

interface

uses
  Windows, Classes, SysUtils, GD_Utils;


function Send_EMail (sFrom, sTo, sCc, sBcc, sSubject, sBody: string;
  pAttFileList: TStrings; sLogFilename: string; bLogError, bLogInfo: boolean;
  iLogFileSize: integer; sExePath: string; var sError: string): boolean;

implementation

const
  C_WSendMailExe = 'WSendMail.exe';  // E-Mail-Sendemodul


{----------------------------------------------------}
function EscapeCmdDoubleQuotes (sCmd: string): string;
{----------------------------------------------------}
{ Doppelapostroph-Zeichen in Kommandozeilenparameter escapen }
begin
  Result := StringReplace(sCmd, '"', '""', [rfReplaceAll]);
end;

{---------------------------------------------------------------------------}
function Send_EMail (sFrom, sTo, sCc, sBcc, sSubject, sBody: string;
  pAttFileList: TStrings; sLogFilename: string; bLogError, bLogInfo: boolean;
  iLogFileSize: integer; sExePath: string; var sError: string): boolean;
{---------------------------------------------------------------------------}
{ E-Mail versenden;
  Übergaben: Absender-Adresse
             Empfänger-Adresse(n) An, getrennt durch ;
             Empfänger-Adresse(n) Cc, getrennt durch ;
             Empfänger-Adresse(n) Bcc, getrennt durch ;
             Betreff
             Inhalt
             Liste mit Anhang-Dateinamen, Platzhalter ? und * möglich
             Logdateiname (ohne Pfad)
             Fehler loggen ja/nein
             Info loggen ja/nein
             Max. Logdateigröße
             Absoluter Pfad des WSendMail-Moduls
  Rückgabe: Fehlermeldung
  Ergebnis: true, wenn Versenden erfolgreich }
var
  sSendMailExe: string;
  sArgs: string;
  iExitCode: cardinal;
  sAttachments: string;
  i: integer;

begin
  // Name des Programms zum Versenden der E-Mail mit Pfad:
  sSendMailExe := IncludeTrailingBackslash (sExePath) + C_WSendMailExe;
  if FileExists (sSendMailExe) then begin
    try
      // Liste der Anhänge -> String für Kommandozeilen-Parameter
      sAttachments := '';
      if Assigned (pAttFileList) then begin
        for i := 0 to pAttFileList.Count-1 do begin
          if pAttFileList[i] <> '' then begin
            if sAttachments <> '' then
              sAttachments := sAttachments + ';';
            sAttachments := sAttachments + '"' + pAttFileList[i] + '"';
          end;
        end;
      end;

      // Kommandozeilen-Parameter
      sArgs := 'FROM:"' + sFrom + '" ' +
               'TO:"' + sTo + '" ' +
               'CC:"' + sCc + '" ' +
               'BCC:"' + sBcc + '" ' +
               'S:"' + EscapeCmdDoubleQuotes(sSubject) + '" ' +
               'B:"' + EscapeCmdDoubleQuotes(sBody) + '" ' +
               'A:"' + sAttachments + '" ' +
               'LN:"' + sLogFilename + '" ' +
               'LE:' + BoolToStr (bLogError) + ' ' +
               'LI:' + BoolToStr (bLogInfo) + ' ' +
               'LS:' + IntToStr (iLogFileSize);

      // Aufruf WSendMail-Exe
      iExitCode := WWaitExecute ('"' + sSendMailExe + '" ' + sArgs, SW_MINIMIZE);
      if iExitCode = 0 then begin  // Versenden OK
        Result := true;
        sError := '';
      end
      else begin  // Versenden Fehler
        Result := false;
        sError := 'Sending e-mail not successful';
      end;
    except
      on E: Exception do begin
        Result := false;
        sError := E.Message;
      end
    end;
  end
  else begin  // WSendMail-Exe nicht gefunden
    Result := false;
    sError := 'File ' + sSendMailExe + ' not found';
  end;
end;

end.
