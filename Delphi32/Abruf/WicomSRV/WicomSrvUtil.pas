{******************************************************************************}
{* Unit: Hilfsfunktionen für Abrufserver-Dienst                               *}
{* 11.10.2006 WW                                                              *}
{* 17.11.2010 GD/WN  Bugfix GetNextSMSFromTempFile: Verzeichnisse ignorieren  *}
{* 25.04.2014 WW  SMS-Dateien mit Zeitstempel der Erstellung im Dateiname     *}
{******************************************************************************}
unit WicomSrvUtil;

interface

uses
  Windows, Forms, SysUtils, Classes, WStrUtils, T_Tools, WComm, GD_Utils, WSysCon;

type
  { Archiv-Stati für Dateien }
  TFileArchivStatus = (fas_Alle, fas_NurArchivfaehige, fas_NurNichtarchivfaehige);


function SaveSMSToTempFile (sPath, sPrefix, sData: string; dtSMS: TDateTime): boolean;
function GetNextSMSFromTempFile (sPath, sPrefix: string;
  FileArchivStatus: TFileArchivStatus; var sFileName: string;
  var dtFileDate: TDateTime): string;
function SetFileArchivbit (sFileName: string): boolean;
function ClearFileArchivbit (sFileName: string): boolean;
function SetArchivbitForAllSMSTempFiles (Path, sPrefix: string): boolean;
function BackupSMSXMLFile (XMLFileName, DestPath: string): integer;
function Get_Veribox_MPSaetzeProSMS (MP: integer; Kanalmaske: string;
  MaxAnzahlKanaele: integer): integer;
function GetDSfGDfueParaSaveFilename (sPath, sRufnummer: string): string;
function GetDSfGDfueParaRestoreResultFilename (sPath, sRufnummer: string): string;

implementation

{------------------------------------------------------------------------------------}
function SaveSMSToTempFile (sPath, sPrefix, sData: string; dtSMS: TDateTime): boolean;
{------------------------------------------------------------------------------------}
{ SMS-Temporärdatei anlegen/schreiben;
  Übergabe: Pfad für SMS-Temporärdatei
            Prefix für SMS-Temporärdatei
            in SMS-Temporärdatei zu schreibender Daten-String
            SMS-Zeitstempel
  Ergebnis: true, wenn Anlegen/Schreiben der SMS-Temporärdatei erfolgreich }
var
  FileName: string;
  dtNow: TDateTime;

begin
  // Im Dateinamen jetzt Zeitstempel der Erstellung; 25.04.2014, WW
  // -> Als zusätzliches Sortierkriterium, falls SMS mit gleichem Zeitstempel
  //    vorliegen.
  Sleep (20);  // um sicherzugehen, daß eindeutige Zeitstempel entstehen
  dtNow:=Now;
  Filename:=sPath + sPrefix + FormatDateTime ('yyyymmdd_hhnnsszzz', dtNow) + '.tmp';
  Result:=ClearRohFile (FileName);  // File anlegen
  if Result then begin
    Result:=WriteRohFile (FileName, sData);  // Daten-String in File schreiben

    SetFileArchivbit (FileName);  // Archiv-Bit setzen

    // Der SMS-Zeitstempel wird im Datei-Zeitstempel mitgespeichert, um beim späteren
    // Lesen der SMS-Files den chronologisch richtigen Dateizugriff zu ermöglichen:
    FileSetDate (FileName, DateTimeToFileDate (dtSMS));
  end;
end;

{--------------------------------------------------------------------------------}
function SMSFileDateCompare (List: TStringList; Index1, Index2: Integer): Integer;
{--------------------------------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren der SMS-Datei-Stringliste nach:
  1. dem Datei-Zeitstempel
  2. dem Zeitstempel im Dateinamen (Zeitpunkt der Erstellung der Datei); 25.04.2014, WW }
begin
  Result:=Integer (List.Objects [Index1]) - Integer (List.Objects [Index2]);  { 1. Sortierkriterium }
  if Result = 0 then
    Result:=CompareStr (List [Index1], List [Index2]);  { 2. Sortierkriterium }
end;

{-------------------------------------------------------------------}
function GetNextSMSFromTempFile (sPath, sPrefix: string;
                                 FileArchivStatus: TFileArchivStatus;
                                 var sFileName: string;
                                 var dtFileDate: TDateTime): string;
{-------------------------------------------------------------------}
{ liefert Name und Inhalt der ältesten SMS-Temporärdatei;
  Übergabe: Pfad zu SMS-Temporärdatei
            Prefix für SMS-Temporärdatei
            Datei-Archivbit-Status
  Rückgabe: Name der ältesten SMS-Temporärdatei (leer, wenn keine Datei vorhanden)
            Zeitstempel der ältesten SMS-Temporärdatei
  Ergebnis: Inhalt der ältesten SMS-Temporärdatei (leer, wenn keine Datei vorhanden) }
var
  DateiListe: TStringList;
  i: integer;
  SR: TSearchRec;
  S: string;
  bAdd: boolean;
  iFileAttr: integer;

begin
  Result:='';
  sFileName:='';
  dtFileDate:=0;
  // alle mit dem SMS-Prefix beginnenden Dateinamen in DateiListe laden:
  DateiListe:=TStringList.Create;
  try
    i:=FindFirst (sPath + sPrefix + '*', faAnyFile, SR);
    try
      while i = 0 do begin
        Application.ProcessMessages;
        S:=sPath + SR.Name;  // vollständiger Dateiname

        bAdd:=true;
        if (FileArchivStatus = fas_NurArchivfaehige) OR
           (FileArchivStatus = fas_NurNichtarchivfaehige) then begin
          iFileAttr:=FileGetAttr (S);

          if (FileArchivStatus = fas_NurArchivfaehige) AND
             ((iFileAttr and faArchive) = 0) then
            bAdd:=false;
          if (FileArchivStatus = fas_NurNichtarchivfaehige) AND
             ((iFileAttr and faArchive) <> 0) then
            bAdd:=false;
          // 17.11.2010  GD/WN
          // kein Eintrag bei gefundenen Verzeichnissen (nur Dateinamen)
          if ((iFileAttr and faDirectory) <> 0) then bAdd := False;
        end;

        if bAdd then
          DateiListe.AddObject (S, Pointer (SR.Time));

        i:=FindNext (SR);
      end;  { while i = 0 }
    finally
      FindClose (SR);
    end;

    // Dateiliste nach Datei-Zeitstempel sortieren:
    DateiListe.CustomSort (SMSFileDateCompare);

    if DateiListe.Count > 0 then begin
      sFileName:=DateiListe [0];  { Name der ältesten SMS-Datei }
      dtFileDate:=FileDateToDateTime (Integer (DateiListe.Objects [0]));  { Zeitstempel der ältesten SMS-Datei }
      Result:=StringFromFile (sFileName); { Datei-Inhalt in String kopieren }
    end;
  finally
    DateiListe.Free;
  end;
end;

{-----------------------------------------------------------------------}
function SetArchivbitForAllSMSTempFiles (Path, sPrefix: string): boolean;
{-----------------------------------------------------------------------}
{ setzt Archivbit für alle SMS-Temporärdateien;
  Übergabe: Pfad zu SMS-Temporärdateien
            Prefix für SMS-Temporärdateien
  Ergebnis: true, wenn Archivbit setzen OK }
var
  i: integer;
  SR: TSearchRec;
  S: string;

begin
  Result:=true;
  i:=FindFirst (Path + sPrefix + '*', faAnyFile, SR);
  try
    while i = 0 do begin
      Application.ProcessMessages;
      S:=Path + SR.Name;  // vollständiger Dateiname

      if not SetFileArchivbit (S) then  // Archiv-Bit setzen
        Result:=false;

      i:=FindNext (SR);
    end;  { while i = 0 }
  finally
    FindClose (SR);
  end;
end;

{-----------------------------------------------------}
function SetFileArchivbit (sFileName: string): boolean;
{-----------------------------------------------------}
{ setzt Archivbit für eine Datei;
  Übergabe: Dateiname
  Ergebnis: true, wenn Archivbit setzen OK }
var
  iFileAttr: integer;

begin
  Result:=true;
  iFileAttr:=FileGetAttr (sFileName);
  if (iFileAttr and faArchive) = 0 then  // Archiv-Bit nicht gesetzt
    if FileSetAttr (sFileName, iFileAttr OR faArchive) <> 0 then  // Archiv-Bit setzen
      Result:=false;
end;

{-------------------------------------------------------}
function ClearFileArchivbit (sFileName: string): boolean;
{-------------------------------------------------------}
{ löscht Archivbit für eine Datei;
  Übergabe: Dateiname
  Ergebnis: true, wenn Archivbit löschen OK }
var
  iFileAttr: integer;

begin
  Result:=true;
  iFileAttr:=FileGetAttr (sFileName);
  if (iFileAttr and faArchive) > 0 then  // Archiv-Bit gesetzt
    if FileSetAttr (sFileName, iFileAttr AND not faArchive) <> 0 then  // Archiv-Bit löschen
      Result:=false;
end;

{-----------------------------------------------------------------}
function BackupSMSXMLFile (XMLFileName, DestPath: string): integer;
{-----------------------------------------------------------------}
{ SMS-Datei kopieren und Info-Datei mit Name des Quell-Verzeichnisses schreiben;
  Übergabe: vollständiger XML-Filename
            Ziel-Pfad
  Ergebnis: 0, wenn Backup SMS-Datei erfolgreich }
var
  FileName: string;
  iFileAttr: integer;

begin
  Result:=0;  // Vorbelegung: Kein Backup durchgeführt

  if length (XMLFileName) > 0 then begin
    { Ziel-Verzeichnis für SMS-XML-Backup-Datei anlegen, wenn nicht vorhanden: }
    if not DirectoryExists (DestPath) then begin
      if not ForceDirectories (DestPath) then begin
        Result:=-1;
        exit;
      end;
    end;

    iFileAttr:=FileGetAttr (XMLFileName);
    if (iFileAttr and faArchive) > 0 then begin  // Backup nur bei gesetztem Archiv-Bit
      Result:=1;  // Backup durchgeführt

      { SMS-XML-Datei in Zielverzeichnis kopieren: }
      FileName:=ExtractFileName (XMLFileName);  // SMS-XML-Dateiname ohne Pfad
      if not CopyFile (pchar (XMLFileName), pchar (DestPath + FileName), false) then
        Result:=-2;
      { zusätzliche Datei mit Name der SMS-XML-Quelldatei in Zielverzeichnis schreiben: }
      Filename:=ChangeFileExt (Filename, '_Info.txt');
      if not StringToFile (ExpandUNCFilename (XMLFileName), DestPath + FileName) then
        Result:=-3;
    end;
  end;
end;

{-------------------------------------------------------------------}
function Get_Veribox_MPSaetzeProSMS (MP: integer; Kanalmaske: string;
  MaxAnzahlKanaele: integer): integer;
{-------------------------------------------------------------------}
{ Berechnet näherungsweise die max. mögliche Anzahl der Messperiodensaetze je
  Daten-SMS;
  Übergabe: Messperiodenlänge
            Aktive Kanäle (Kanalmaske)
            Maximale Anzahl der Kanäle
  Rückgabe: Anzahl der Messperiodensätze }
var
  iAktiveKanaele: integer;

begin
  if length (Kanalmaske) > 0 then
    iAktiveKanaele:=F_TotalChars (Kanalmaske, '1')  // Anzahl der aktiven Kanäle
  else
    iAktiveKanaele:=MaxAnzahlKanaele;  // Ersatzwert, wenn Kanalmaske fehlt

  { Der Einfachheit halber werden die absoluten Analogwerte wie die Impulswerte
    mit je 6 Bytes veranschlagt (eigtl. Analog nur 2 Bytes): }
  Result:=160 -  // max. Anzahl Zeichen pro SMS
          (14 +  // Vorspann
          (iAktiveKanaele * 6) +  // Absolute Werte
          4);  // Vorspann relativ
  if Result >= 0 then
    Result:=Result DIV (iAktiveKanaele * 2) +  // Messperiodensätze
            1  // Messperiodensatz aus Vorspann
  else
    Result:=0;
end;

{-----------------------------------------------------------------------}
function GetDSfGDfueParaSaveFilename (sPath, sRufnummer: string): string;
{-----------------------------------------------------------------------}
{ Bildet vollständigen Namen der Sicherungsdatei mit einstellbaren DSfG-DFÜ-
  Parametern (Rohdaten, nur DSfG-DFÜ-NG);
  Übergabe: Pfad
            Rufnummer zum Gerät
  Ergebnis: Dateiname }
var
  S: string;
  i: integer;

begin
  S:=sRufnummer;
  // im Dateinamen nicht erlaubte Zeichen ausfiltern:
  for i:=1 to length (CNotAllowedChars_FileName) do
    S:=StringReplace (S, CNotAllowedChars_FileName [i], '', [rfReplaceAll]);

  Result:=sPath + 'ParaSave_FwUpd_' + S + '.tmp';  // Name an DFÜ-Installer angepaßt; 06.11.2014, WW
end;

{--------------------------------------------------------------------------------}
function GetDSfGDfueParaRestoreResultFilename (sPath, sRufnummer: string): string;
{--------------------------------------------------------------------------------}
{ Bildet vollständigen Namen der Datei mit Ergebnis des Parameter-
  Wiederherstellens bei Firmware-Update (CSV-Format);
  Übergabe: Pfad
            Rufnummer zum Gerät
  Ergebnis: Dateiname }
var
  S: string;
  i: integer;

begin
  S:=sRufnummer;
  // im Dateinamen nicht erlaubte Zeichen ausfiltern:
  for i:=1 to length (CNotAllowedChars_FileName) do
    S:=StringReplace (S, CNotAllowedChars_FileName [i], '', [rfReplaceAll]);

  S:=S + '_' + FormatDateTime ('yyyymmddhhnnss', Now);
  Result:=sPath + 'ParaRestoreResult_FwUpd_' + S + '.log';
end;

end.

