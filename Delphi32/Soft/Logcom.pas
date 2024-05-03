{******************************************************************************}
{* Unit: Log-File für serielle Schnittstelle                                  *}
{* 08.07.1999 WW                                                              *}
{* 04.04.2002 WW  Log-Filename mit vollständigem Pfad (Pfad der Exe-Datei)    *}
{* 06.09.2002 WW  Write-Methode: Daten jetzt als String übergeben             *}
{* 21.04.2005 WW  zusätzlicher Constructor für USB-Daten-Protokollierung      *}
{* 23.08.2005 WW  mit optionaler Ausgabe der Anzahl empfangener Zeichen       *}
{* 14.02.2011 WW  Property FileName                                           *}
{* 18.12.2013 WW  mit Rundpuffer-Funktion (vererbt von TLogFileObject)        *}
{* 21.04.2015 WW  Log-Filename optional mit IP-Linien-Nr. aus Client-Kommando *}
{* 11.01.2016 WW  Speicherermittlung mit WGlobalMemoryStatusEx (> 4 GB)       *}
{* 25.02.2020 WW  TCPIP-Message mit Hostname                                  *}
{******************************************************************************}
Unit LogCom;

INTERFACE

uses
  Windows, SysUtils, Classes, GD_Utils, WChars, WSysCon, LogFile;

type
  { Callback-Prozedurtypen }
  TCBComLogMsgProc = procedure (S: string) of object;

  { Objekt für Protokollierung der Sende-/Empfangsdaten einer seriellen Schnittstelle }

  TComLogFile = class (TLogFileObject)
  private
    FFile_OK: boolean;
    FCBComLog: TCBComLogMsgProc;
  public
    Path: TFileName;
    Constructor Create (APath: string; AComPort: integer; doRewrite: boolean;
                        AFilenamePrefix: string = '';
                        AIPLinie_Kommando: integer = 0);
    Constructor CreateUSB (APath: string; AUSBNr: integer; doRewrite: boolean;
                           AFilenamePrefix: string = '');
    Constructor CreateCB (ACBComLog: TCBComLogMsgProc);
    Procedure Write (Modus: char; Daten: string; AnzBytes: integer = -1;
                     HexDaten: boolean = false);
    Procedure WriteMsg (Msg: string);
    Procedure WriteTCPIP_Msg (Msg, Host, Adresse: string; Port: integer);
    property FileName: string read FFileName;  // 13.02.2012, WW
  End;

IMPLEMENTATION

{ TComLogFile }

{-----------------------------------------------------------------------------------}
Constructor TComLogFile.Create (APath: string; AComPort: integer; doRewrite: boolean;
                                AFilenamePrefix: string = '';
                                AIPLinie_Kommando: integer = 0);
{-----------------------------------------------------------------------------------}
{ Construktor für die Protokollierung von Daten einer seriellen Schnittstelle (COM)
  oder TCP/IP-Verbindung (IP);
{ Übergaben: APath (Pfad für Log-Datei)
             AComPort (COM-Port, dessen Daten aufgezeichnet werden sollen -> für Dateiname
                       positiver ComPort: COM-Nummer
                       negativer ComPort: IP-Abrufnummer)
             doRewrite (true: Log-Datei wird neu geschrieben
                        false: eine bestehende Log-Datei wird fortgeschrieben)
             Prefix für Log-Lateiname (optional)
             IP-Linien-Nr. aus Client-Kommando (optional; kann als zusätzliche
                Kennzeichnung im Log-Dateinamen herangezogen werden) }
var
  FileNameBak: TFileName;
  FS: TFileStream;

Begin
  Inherited Create;
  Path:=APath;
  if AComPort = CComTCP_IP then
    FFileName:='COM_TCP_IP.LOG'     // 03.04.2003  WW
  else begin
    if AComPort > 0 then  { COM-Port }
      FFileName:='COM_' + Format ('%.3d', [AComPort]) + '.LOG'     // 20.12.2002  WW
    else begin            { IP-Abrufnummer }
      if (AIPLinie_Kommando = 0) OR (AComPort = AIPLinie_Kommando) then
        FFileName:='IP_' + Format ('%.3d', [Abs (AComPort)]) + '.LOG'     // 17.11.2004  WW
      else
        // Logfilename mit IP-Linien-Nr. aus Kommando und intern verwendeter:
        // IP_<IP-Linie Kommando>_<IP-Linie intern>.log
        FFileName:='IP_' + Format ('%.3d', [Abs (AIPLinie_Kommando)]) +
                   '_' + Format ('%.3d', [Abs (AComPort)]) + '.LOG';  // 21.04.2015, WW
    end;
  end;
  if length (AFilenamePrefix) > 0 then  // mit Prefix; 24.07.2009, WW
    FFileName:=AFilenamePrefix + '_' + FFileName;
  FFileName:=APath + FFileName;

  if doRewrite OR not FileExists (FFileName) then begin
    if AComPort = CComTCP_IP then
      FileNameBak:='COM_TCP_IP.~LO'     // 03.04.2003  WW
    else begin
      if AComPort > 0 then  { COM-Port }
        FileNameBak:='COM_' + Format ('%.3d', [AComPort]) + '.~LO'  // 20.12.2002  WW
      else begin            { IP-Abrufnummer }
        if (AIPLinie_Kommando = 0) OR (AComPort = AIPLinie_Kommando) then
          FileNameBak:='IP_' + Format ('%.3d', [Abs (AComPort)]) + '.~LO'  // 17.11.2002  WW
        else
          // Logfilename mit IP-Linien-Nr. aus Kommando und intern verwendeter:
          // IP_<IP-Linie Kommando>_<IP-Linie intern>.~lo
          FileNameBak:='IP_' + Format ('%.3d', [Abs (AIPLinie_Kommando)]) +
                       '_' + Format ('%.3d', [Abs (AComPort)]) + '.~LO';  // 21.04.2015, WW
      end;
    end;
    if length (AFilenamePrefix) > 0 then  // mit Prefix; 24.07.2009, WW
      FileNameBak:=AFilenamePrefix + '_' + FileNameBak;
    FileNameBak:=APath + FileNameBak;

    DeleteFile (FileNameBak);
    RenameFile (FFileName, FileNameBak);
    { Logfile neu anlegen: }
    try
      FS:=TFileStream.Create (FFileName, fmCreate);
      try
        FFile_OK:=true;
      finally
        FS.Free;
      end;
    except
      FFile_OK:=false;
    end;
  end else
    FFile_OK:=true;

  FCBComLog:=nil;  // keine Ausgabe über Callback-Procedure
End;

{------------------------------------------------------------------------------------}
Constructor TComLogFile.CreateUSB (APath: string; AUSBNr: integer; doRewrite: boolean;
                                   AFilenamePrefix: string = '');
{------------------------------------------------------------------------------------}
{ Construktor für die Protokollierung von Daten einer USB-Verbindung (USB);
{ Übergaben: APath (Pfad für Log-Datei)
             AUSBPortNr (USB-Abrufnummer, deren Daten aufgezeichnet werden sollen -> für Dateiname)
             doRewrite (true: Log-Datei wird neu geschrieben
                        false: eine bestehende Log-Datei wird fortgeschrieben)
             Prefix für Log-Lateiname (optional) }
var
  FileNameBak: TFileName;
  FS: TFileStream;

Begin
  Inherited Create;
  Path:=APath;
  FFileName:='USB_' + Format ('%.3d', [AUSBNr]) + '.LOG';
  if length (AFilenamePrefix) > 0 then  // mit Prefix; 24.07.2009, WW
    FFileName:=AFilenamePrefix + '_' + FFileName;
  FFileName:=APath + FFileName;

  if doRewrite OR not FileExists (FFileName) then begin
    FileNameBak:='USB_' + Format ('%.3d', [AUSBNr]) + '.~LO';
    if length (AFilenamePrefix) > 0 then  // mit Prefix; 24.07.2009, WW
      FileNameBak:=AFilenamePrefix + '_' + FileNameBak;
    FileNameBak:=APath + FileNameBak;

    DeleteFile (FileNameBak);
    RenameFile (FFileName, FileNameBak);
    { Logfile neu anlegen: }
    try
      FS:=TFileStream.Create (FFileName, fmCreate);
      try
        FFile_OK:=true;
      finally
        FS.Free;
      end;
    except
      FFile_OK:=false;
    end;
  end else
    FFile_OK:=true;

  FCBComLog:=nil;  // keine Ausgabe über Callback-Procedure
End;

{-------------------------------------------------------------}
Constructor TComLogFile.CreateCB (ACBComLog: TCBComLogMsgProc);
{-------------------------------------------------------------}
begin
  Inherited Create;
  FCBComLog:=ACBComLog;  // Ausgabe über Callback-Procedure

  FFileName:='';
  FFile_OK:=false;  // keine File-Ausgabe
end;

{------------------------------------------------------------------------------}
Procedure TComLogFile.Write (Modus: char; Daten: string; AnzBytes: integer = -1;
                             HexDaten: boolean = false);
{------------------------------------------------------------------------------}
var
  year, month, day: word;
  hour, min, sec, msec: word;
  sKopf: string;
  sAnzBytes: string;
  sDaten: string;
  FS : TFileStream;
  i: cardinal;
  ms: TMemoryStatus;
  MemStatusEx: TMemoryStatusEx;
  intwert: integer;

Begin
  try
    { Kopf: }
    case Modus of
      'S',
      'E': begin  { mit Kopf }
             if Modus = 'S' then
               sKopf:='*** Sendedaten:     '
             else
               sKopf:='*** Empfangsdaten:  ';
             DecodeDate (Date, year, month, day);      { Datum }
             DecodeTime (Time, hour, min, sec, msec);  { Zeit }
             sKopf:=sKopf + Format ('%.2d.', [day]) +
                            Format ('%.2d.', [month]) +
                            Format ('%.4d', [year]) + '  ' +
                            Format ('%.2d:', [hour]) +
                            Format ('%.2d:', [min]) +
                            Format ('%.2d,', [sec]) +
                            Format ('%.3d', [msec]) + ' ***';

             sKopf:=CR + LF + CR + LF + sKopf + CR + LF;

             sKopf:=sKopf +  '*** ';
             if WGlobalMemoryStatusEx(MemStatusEx) then  // Speicherinformationen für Werte > 4 GB; 11.01.2016, WW
               sKopf := sKopf +
                 'ML: ' + IntToStr (MemStatusEx.dwMemoryLoad) + ' %  ' +                     // percent of memory in use
                 'TP: ' + IntToStr(MemStatusEx.ullTotalPhys div (1024*1024)) + '  ' +        // Mbytes of physical memory
                 'AP: ' + IntToStr(MemStatusEx.ullAvailPhys div (1024*1024)) + '  ' +        // free physical memory Mbytes
                 'TPF: ' + IntToStr (MemStatusEx.ullTotalPageFile div (1024*1024)) + '  ' +  // Mbytes of paging file
                 'APF: ' + IntToStr (MemStatusEx.ullAvailPageFile div (1024*1024)) + '  ' +  // free Mbytes of paging file
                 'TV: ' + IntToStr(MemStatusEx.ullTotalVirtual div (1024*1024)) + '  ' +     // user Mbytes of address space
                 'AV: ' + IntToStr(MemStatusEx.ullAvailVirtual div (1024*1024)) + '  ' +     // free user Mbytes
                 'AEV: ' + IntToStr(MemStatusEx.ullAvailExtendedVirtual div (1024*1024))     // free extended user Mbytes
             else begin
               ms.dwLength:=SizeOf(TMemoryStatus);  // Speicherinformationen für Werte < 4 GB
               GlobalMemoryStatus (ms);
               sKopf := sKopf +
                 'ML: ' + IntToStr (ms.dwMemoryLoad) + ' %  ' +                    // percent of memory in use
                 'TP: ' + IntToStr (ms.dwTotalPhys div (1024*1024)) + '  ' +       // Mbytes of physical memory
                 'AP: ' + IntToStr (ms.dwAvailPhys div (1024*1024)) + '  ' +       // free physical memory Mbytes
                 'TPF: ' + IntToStr (ms.dwTotalPageFile div (1024*1024)) + '  ' +  // Mbytes of paging file
                 'APF: ' + IntToStr (ms.dwAvailPageFile div (1024*1024)) + '  ' +  // free Mbytes of paging file
                 'TV: ' + IntToStr (ms.dwTotalVirtual div (1024*1024)) + '  ' +    // user Mbytes of address space
                 'AV: ' + IntToStr (ms.dwAvailVirtual div (1024*1024));            // free user Mbytes
             end;
             sKopf:=sKopf +  ' [MB] ***' + CR + LF + CR + LF;
           end;
    else
      sKopf:='';
    end;

    { Anzahl Bytes: }
    if AnzBytes > -1 then
      sAnzBytes:=CR + LF + '-> ' + IntToStr (AnzBytes) + ' Byte' + CR + LF
    else
      sAnzBytes:='';

    { Daten: }
    sDaten:='';
    for i:=1 to length (Daten) do begin
      if HexDaten then begin
        { Daten in lesbare Hex-Werte konvertieren; 24.06.2009, WW }
        intwert:=integer (Daten[i]);
        sDaten:=sDaten + IntToHex (intwert, 2) + ' ';
      end
      else begin
        if (Daten [i] >= Low (CSonderzeichen)) AND (Daten[i] <= High (CSonderzeichen)) then
          sDaten:=sDaten + string (CSonderzeichen [Daten [i]]) + CR + LF
        else
          sDaten:=sDaten + Daten [i];
      end;
    end;


    if FFile_OK then begin  // Ausgabe per File
      FS:=TFileStream.Create (FFileName, fmOpenReadWrite OR fmShareDenyWrite);
      try
        FS.Seek (0, soFromEnd);

        if length (sKopf) > 0 then
          FS.Write (sKopf [1], length (sKopf));

        if length (sAnzBytes) > 0 then
          FS.Write (sAnzBytes [1], length (sAnzBytes));

        if length (sDaten) > 0 then
          FS.Write (sDaten [1], length (sDaten));
      finally
        FS.Free;
      end;
    end
    else if Assigned (FCBComLog) then begin  // Ausgabe per Callback
      if length (sKopf) > 0 then
        FCBComLog (sKopf);

      if length (sAnzBytes) > 0 then
        FCBComLog (sAnzBytes);

      if length (sDaten) > 0 then
        FCBComLog (sDaten);
    end;
  except
  end;
End;

{-------------------------------------------}
Procedure TComLogFile.WriteMsg (Msg: string);
{-------------------------------------------}
var
  S: string;
  year, month, day: word;
  hour, min, sec, msec: word;
  FS: TFileStream;

begin
  try
    S:='*** ' + Msg + '   ';
    DecodeDate (Date, year, month, day);                                { Datum }
    DecodeTime (Time, hour, min, sec, msec);                             { Zeit }
    S:=S + Format ('%.2d.', [day]) +
          Format ('%.2d.', [month]) +
          Format ('%.4d', [year]) + '  ' +
          Format ('%.2d:', [hour]) +
          Format ('%.2d:', [min]) +
          Format ('%.2d,', [sec]) +
          Format ('%.3d', [msec]) + ' ***';
    S:=CR + LF + S + CR + LF + CR + LF;

    if FFile_OK then begin  // Ausgabe per File
      FS:=TFileStream.Create (FFileName, fmOpenReadWrite OR fmShareDenyWrite);
      try
        FS.Seek (0, soFromEnd);
        FS.Write (S [1], length (S));
      finally
        FS.Free;
      end;
    end
    else if Assigned (FCBComLog) then begin  // Ausgabe per Callback
      FCBComLog (S);
    end;
  except
  end;
end;

{-------------------------------------------------------------------------------}
Procedure TComLogFile.WriteTCPIP_Msg (Msg, Host, Adresse: string; Port: integer);
{-------------------------------------------------------------------------------}
var
  S: string;
  year, month, day: word;
  hour, min, sec, msec: word;
  FS: TFileStream;

begin
  try
    S:='*** ' + Msg + '   ';
    DecodeDate (Date, year, month, day);                                { Datum }
    DecodeTime (Time, hour, min, sec, msec);                             { Zeit }
    S:=S + Format ('%.2d.', [day]) +
          Format ('%.2d.', [month]) +
          Format ('%.4d', [year]) + '  ' +
          Format ('%.2d:', [hour]) +
          Format ('%.2d:', [min]) +
          Format ('%.2d,', [sec]) +
          Format ('%.3d', [msec]) + ' ***';
    S:=CR + LF + S + CR + LF;

    S:=S + '***';
    if Host <> '' then
      S:=S + ' Host: ' + Host + ' ';  // 25.02.2020, WW
    if Adresse <> '' then  // 25.02.2020, WW
      S:=S + ' Adresse: ' + Adresse + ' ';
    S:=S + ' Port: ' + IntToStr (Port) + ' ***' +
       CR + LF + CR + LF;

    if FFile_OK then begin  // Ausgabe per File
      FS:=TFileStream.Create (FFileName, fmOpenReadWrite OR fmShareDenyWrite);
      try
        FS.Seek (0, soFromEnd);
        FS.Write (S [1], length (S));
      finally
        FS.Free;
      end;
    end
    else if Assigned (FCBComLog) then begin  // Ausgabe per Callback
      FCBComLog (S);
    end;
  except
  end;
end;

End.
