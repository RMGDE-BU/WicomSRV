{------------------------------------------------------------------------------}
{ Überwachungsflags für WICO22 mit Redundanzüberwachung                        }
{                                                                              }
{ 21.09.2007  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) RMG Messtechnik GmbH 2007                                      }
{------------------------------------------------------------------------------}
unit VerifyFlags;

interface

uses
  SysUtils, ExtCtrls, Classes, IniFiles,
  PathIni, WStream, GD_Utils;

const
  C_VerifyFlag_OK         = 0;
  C_VerifyFlag_Warning    = 1;
  C_VerifyFlag_Error      = 2;
  C_VerifyFlag_FatalError = 3;
  C_VerifyExtension_Module   = '.VEM';
  C_VerifyExtension_Control  = '.VEC';
  C_VerifyMask_OK         = '%s_0' + C_VerifyExtension_Module;
  C_VerifyMask_FatalError = '%s_3' + C_VerifyExtension_Module;

type
  TVerifyFlags = class(TObject)
    constructor Create(const sFlagPath: TFileName = ''); virtual;
     destructor Destroy; override;
  private
    FFlagPath   : TFileName;
    FTimer      : TTimer;
    FInterval   : Cardinal;
    FFatalError : boolean;
    FMyFileName : TFileName;
    function WriteVerifyFlag(iType: integer): boolean;
    procedure OnOkTimer(Sender: TObject);
  protected
  public
    property WriteTerminalErrorFlag: boolean index C_VerifyFlag_FatalError
      read WriteVerifyFlag;
    property TimerInterval: Cardinal write FInterval;
  end;

var
  VerifyFlagObject : TVerifyFlags;

implementation

const
  C_OkTimer_Interval         = 10000;

{----------------------------- Allgemeine Funktionen --------------------------}

{--------------------------------------------}
function MyDeleteFile(const sFileName: string): boolean;
{--------------------------------------------}
var
  s : string;
begin
  s := sFileName;
  // Datei löschen
  if (FileExists(s))
  then Result := DeleteFile(s)
  else Result := True;
  // Ggf. ID-File löschen
  s := ChangeFileExt(s, C_VerifyExtension_Control);
  if (FileExists(s)) then Result := Result and DeleteFile(s);
end;

{------------------------------- TVerifyFlags ----------------------------}

{--------------------------------------------}
constructor TVerifyFlags.Create(const sFlagPath: TFileName = '');
{--------------------------------------------}
begin
  inherited Create;

  if (sFlagPath = '') then begin
    with TProgramIni.Create(True, False) do
    try
      with TIniFile.Create(WieserIniFile) do
      try
        FFlagPath := ReadString('MONITORING', 'MonitoringDir', '');
      finally
        Free;
      end;
    finally
      Free;
    end;
  end
  else FFlagPath := sFlagPath;

  if (FFlagPath <> '') and (DirectoryExists(FFlagPath)) then begin
    FFlagPath := IncludeTrailingBackslash(FFlagPath);
    FMyFileName := StringReplace(  // '_' eleminieren
       ChangeFileExt(ExtractFileName(ParamStr(0)), ''), '_', '', [rfReplaceAll]);
    FFatalError := False;
    FInterval := C_OkTimer_Interval;

    FTimer := TTimer.Create(nil);
    FTimer.Interval := FInterval;
    FTimer.OnTimer := Self.OnOkTimer;
    FTimer.Enabled := True;
  end
  else FTimer := nil;
end;

{--------------------------------------------}
destructor TVerifyFlags.Destroy;
{--------------------------------------------}
begin
  if (Assigned(FTimer)) then begin
    FTimer.OnTimer := nil;
    FTimer.Enabled := False;
    FreeAndNil(FTimer);
  end;

  inherited;
end;

{--------------------------------------------}
procedure TVerifyFlags.OnOkTimer(Sender: TObject);
{--------------------------------------------}
begin
  FTimer.Enabled := True;
  try
    if (FInterval <> FTimer.Interval) then FTimer.Interval := FInterval;

    WriteVerifyFlag(C_VerifyFlag_OK);
  finally
    FTimer.Enabled := (not FFatalError);
  end;
end;

{--------------------------------------------}
function TVerifyFlags.WriteVerifyFlag(iType: integer): boolean;
{--------------------------------------------}
begin
  if (DirectoryExists(FFlagPath)) then begin
    try
      if (not FFatalError) then begin  // Abschaltfehler beendet alles !
        // Ggf. Flag für Abschaltfehler setzen
        FFatalError := ((FFatalError) or (iType = C_VerifyFlag_FatalError));
        // Im Abschaltfehler-Fall Timer außer Kraft setzen
        if (FFatalError) then begin
          FTimer.Enabled := False;
          FTimer.OnTimer := nil;
        end;
        // Meldungsdatei schreiben
        with TFileStream.Create(FFlagPath + FMyFileName + '_' + IntToStr(iType) +
          C_VerifyExtension_Module, fmCreate) do Free;
        // Triggerdatei schreiben
        with TFileStream.Create(FFlagPath + FMyFileName + '_' + IntToStr(iType) +
          C_VerifyExtension_Control, fmCreate) do Free;

        Result := True;
      end
      else Result := False;
    except
      Result := False
    end;
  end
  else Result := True;   // Mechanismus deaktiviert
end;

end.
