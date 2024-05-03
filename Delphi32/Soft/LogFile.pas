{******************************************************************************}
{* Unit: Universelle Log-Datei zum Protokollieren von Strings                 *}
{* 30.12.2002 WW                                                              *}
{* 21.12.2002 WW  umgestellt auf netzwerkfähige Filezugriffe                  *}
{* 20.05.2011 GD  TAB als Trennzeichen für VerExcelung                        *}
{* 18.12.2013 WW  Basis-Logdatei-Objekt mit Rundpuffer-Funktion               *}
{* 30.04.2014 WW  Log-Eintrag mit Spalte Type (DEBUG, INFO, WARNING, ERROR)   *}
{* 14.07.2015 WW  Bugfix TCustomLogFile.Create bei übergebenem Leer-Pfad      *}
{*                (Root-Verzeichnis des aktuellen Laufwerks wurde verwendet)  *}
{******************************************************************************}
Unit LogFile;

INTERFACE

uses
  Classes, SysUtils, WChars, Novell, T_Tools;

type
  { Basis-Objekt für Log-Datei }

  { Typisierung der Log-Einträge }
  TLogType = (lt_Debug, lt_Info, lt_Warning, lt_Error);  

  TLogFileObject = class (TObject)
  private
    function GetLogTypeText (Value: TLogType): string;
    property LogTypeText[Value: TLogType]: string read GetLogTypeText;
  protected
    FFileName: string;
  public
    constructor Create;
    function DoRundpuffer (iMaxByte: integer): boolean;
  end;

  { Objekt für Protokollierung in Datei }

  TCustomLogFile = class (TLogFileObject)
  protected
    FFile_OK: boolean;
  public
    Constructor Create (APath: string; ALogFilename: string; doRewrite: boolean);
    Procedure Write (S: string; mitHeader: boolean = true;
      ALogType: TLogType = lt_Info);
  End;

procedure WriteDebugLog (APath: string; ALogFilename: string; S: string;
  mitHeader: boolean = true; ALogType: TLogType = lt_Info;
  doRewrite: boolean = false);

IMPLEMENTATION

{ TLogFileObject }

{--------------------------------}
constructor TLogFileObject.Create;
{--------------------------------}
begin
  inherited Create;
  FFileName:='';
end;

{----------------------------------------------------------------}
function TLogFileObject.DoRundpuffer (iMaxByte: integer): boolean;
{----------------------------------------------------------------}
{ Rundpuffer für Logdatei ausführen: Die letzten MaxByte bleiben erhalten, alle
  Bytes davor werden gelöscht;
  Übergabe: Maximale Dateigröße (Bytes)
  Ergebnis: true, wenn Rundpuffer auf Logdatei erfolgreich }
var
  TmpFileName: string;
  TmpFileStream: TFileStream;
  FileStream: TFileStream;

begin
  try
    if length (FFileName) = 0 then begin
      Result:=false;
      exit;
    end;

    Result:=true;
    if WFileSize (FFileName) <= iMaxByte then exit;  // Dateigröße übersteigt nicht Maximum, keine Aktion

    TmpFileName:=ExtractFilePath (FFileName) +
                 '~' + ChangeFileExt (ExtractFileName (FFileName), '.tmp');
    FileStream:=TFileStream.Create (FFileName, fmOpenRead or fmShareDenyWrite);
    try
      TmpFileStream:=TFileStream.Create (TmpFileName, fmCreate or fmShareDenyRead);
      try
        FileStream.Position:=FileStream.Size - iMaxByte;  // Position, ab der kopiert wird
        TmpFileStream.CopyFrom (FileStream, iMaxByte);  // MaxByte in Tmp-Datei kopieren
      finally
        TmpFileStream.Free;
      end;
    finally
      FileStream.Free;
    end;
    DeleteFile (FFileName);  // bisherige Datei löschen
    RenameFile (TmpFileName, FFileName);  // Tmp-Datei umbenennen
  except
    Result:=false;
  end;
end;

{---------------------------------------------------------------}
function TLogFileObject.GetLogTypeText (Value: TLogType): string;
{---------------------------------------------------------------}
{ Liefert Text zu Log-Type }
begin
  case Value of
    lt_Debug: Result:='DEBUG';
    lt_Info: Result:='INFO';
    lt_Warning: Result:= 'WARNING';
    lt_Error: Result:='ERROR';
  else
    Result:='';
  end;
  Result:='[' + Result + ']';
end;


{ TCustomLogFile }

{------------------------------------------------------------------------------------------}
Constructor TCustomLogFile.Create (APath: string; ALogFilename: string; doRewrite: boolean);
{------------------------------------------------------------------------------------------}
{ Übergaben: APath (Pfad für Log-Datei)
             ALogFilename (Name der Log-Datei ohne Pfad und ohne Extension)
             doRewrite (true: Log-Datei wird neu geschrieben
                        false: eine bestehende Log-Datei wird fortgeschrieben) }
var
  FileNameBak: TFileName;
  FS: TFileStreamExt;

Begin
  Inherited Create;
  if length (ALogFilename) = 0 then
    FFileName:='UNBEKANNT'
  else
    FFileName:=ALogFilename;
  if length (APath) > 0 then  // 14.07.2015, WW
    FFileName:= IncludeTrailingBackslash(APath) + FFileName;

  if (ExtractFileExt(FFileName) = '') then
    FFileName:=ChangeFileExt(FFileName, '.LOG');

  if doRewrite OR not FileExists (FFileName) then begin
    FileNameBak:=ChangeFileExt (FFileName, '.~' + Copy(ExtractFileExt(FFileName), 2, 2));
    DeleteFile (FileNameBak);
    RenameFile (FFileName, FileNameBak);
    { Logfile neu anlegen: }
    try
      FS:=TFileStreamExt.Create (FFileName, fmCreate, FFile_OK);
      FS.Free;
    except
      FFile_OK:=false;
    end;
  end else
    FFile_OK:=true;
End;

{--------------------------------------------------------------------}
Procedure TCustomLogFile.Write (S: string; mitHeader: boolean = true;
  ALogType: TLogType = lt_Info);
{--------------------------------------------------------------------}
var
  SBuf: string;
  TFS: TTextFileStreamExt;
  isOpened: boolean;

Begin
  if not FFile_OK then exit;
  try
    TFS:=TTextFileStreamExt.Create (FFileName, fmOpenReadWrite OR fmShareDenyWrite, isOpened);
    try
      if isOpened then begin
        TFS.Seek (0, soFromEnd);
        if mitHeader then begin  // Header: Datum/Zeit und Programmname
          SBuf:=FormatDateTime ('yyyy-mm-dd hh:nn:ss,zzz', Now) + #9 +  // 20.05.2011
            // Datum-Format angepaßt; 30.04.2014, WW   
            ChangeFileExt(ExtractFileName(ParamStr(0)), '') + #9 +
            LogTypeText[ALogType] + #9 + S;  // mit Log-Type; 30.04.2014, WW
          TFS.WriteLn (SBuf);
        end else
          TFS.WriteLn (S);
      end;
    finally
      TFS.Free;
    end;
  except
  end;
End;

{----------------------------------------------------------------------}
procedure WriteDebugLog (APath: string; ALogFilename: string; S: string;
  mitHeader: boolean = true; ALogType: TLogType = lt_Info;
  doRewrite: boolean = false);
{----------------------------------------------------------------------}
var
  CLF: TCustomLogFile;
begin
  CLF:=TCustomLogFile.Create (APath, ALogFilename, doRewrite);
  try
   CLF.Write (S, mitHeader, ALogType);
  finally
    CLF.Free;
  end;
end;

End.
