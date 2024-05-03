{------------------------------------------------------------------------------}
{ Basisobjekt für DSfG-Abrufe                                                  }
{                                                                              }
{ 05.03.2001 GD  Neu                                                           }
{ 11.04.2002 GD  Erweitert für standartisierte Archivabrufe                    }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, 2002                                    }
{------------------------------------------------------------------------------}
unit DConnect;

interface

uses
  SysUtils, Classes, Controls,
  PathIni, WSysDat, GD_Utils, T_Zeit;

type

  TArchivAbrufList = class(TMyTabList)
    constructor Create(iStaId: integer);
  private
    FStationsId  : integer;
  protected
  public
    function GetInstanzList(iInstId: integer): TMyTabList;
    function GetArchivKanaele(iInstId, iArchivNr: integer): TMyTabList;
    function HasArchivKanal(iInstId, iArchivNr, iAKanal: integer): boolean;
    property StationsId: integer read FStationsId;
  end;

  TCustomConnection = class(TObject)
  private
  protected
  public
  end;

  TCustomDSfGConnection = class(TCustomConnection)
    constructor Create; virtual;
    destructor Destroy; override;
  private
    FTimeOut : integer;
    FOpened  : boolean;
    FAdresse : char;
    FSystemEinstellungen : TSystemEinstellungen;
  protected
    procedure InitComponents(aState: boolean); virtual;
    procedure SetTimeOut(Value: integer); virtual;
    procedure SetOpened(Value: boolean); virtual;
    procedure SetAdresse(Value: char); virtual;
    property SystemEinstellungen : TSystemEinstellungen
      read FSystemEinstellungen;
  public
    procedure Release(iRelease: integer);
    procedure SendTelegramm(sTelegramm: string); virtual; abstract;
    function ReceiveTelegramm: string; virtual; abstract;
    function SendReceiveTelegramm(sTelegramm: string;
      bFolgetelegramme: boolean = True): string; virtual; abstract;
    function StartConnectionTime: integer; virtual; abstract;
    procedure InitConnection(aState: boolean; iId: integer = -1); virtual; abstract;
    procedure InitParameter(aState: boolean); virtual; abstract;
    function IsTeilnehmer: Boolean; virtual; abstract;
    function GetTeilnehmer: string; virtual; abstract;
    function InitAutoDatenAbruf(iId: integer): boolean; virtual; abstract;
    function InitManuDatenAbruf(iStaId: integer; pAbrufListe: TArchivAbrufList;
      dtVon, dtBis: TDateTime): boolean; virtual; abstract;
    function ReadParameter(cIAdr: char; sDEAVon: string;
      sDEABis: string = ''): string; virtual; abstract;
    function ReadStammdaten(iState: byte): TStrings; virtual; abstract;
    property Adresse: char read FAdresse write SetAdresse;
    property TimeOut: integer read FTimeOut write SetTimeOut;
    property Opened: boolean read FOpened write SetOpened;
  end;

implementation

{----------------------------- TArchivAbrufList -------------------------------}

{-----------------------------------------}
constructor TArchivAbrufList.Create(iStaId: integer);
{-----------------------------------------}
begin
  inherited Create;

  FStationsId := iStaId;
end;

{ Gibt Stringliste einer Instanz zurück   }
{ Parameter: InstanzId                    }
{ Rückgabe: Tabellarische Stringliste     }
{-----------------------------------------}
function TArchivAbrufList.GetInstanzList(iInstId: integer): TMyTabList;
{-----------------------------------------}
var
  i : integer;
begin
  i := IndexOf(IntToStr(iInstId));

  if (i < 0) then Result := nil else Result := TMyTabList(Objects[i]);
end;

{ Gibt Stringliste einer Archivgrp zurück }
{ Parameter: InstanzId, Archivgruppennr.  }
{ Rückgabe: Tabellarische Stringliste     }
{-----------------------------------------}
function TArchivAbrufList.GetArchivKanaele(
  iInstId, iArchivNr: integer): TMyTabList;
{-----------------------------------------}
var
  i : integer;
begin
  Result := GetInstanzList(iInstId);

  if (Assigned(Result)) then begin
    i := Result.IndexOf(IntToStr(iArchivNr));
    if (i < 0) then Result := nil else Result := TMyTabList(Result.Objects[i]);
  end;
end;

{ Gibt zurück, ob Archivkanal enthalten i.}
{ Parameter: InstanzId, Archivgrp, -kanal }
{ Rückgabe: Enthalten ja/nein             }
{-----------------------------------------}
function TArchivAbrufList.HasArchivKanal(
  iInstId, iArchivNr, iAKanal: integer): boolean;
{-----------------------------------------}
var
  p : TMyTabList;
begin
  Result := False;  // Default

  p := GetArchivKanaele(iInstId, iArchivNr);
  if (Assigned(p)) then Result := (p.IndexOf(IntToStr(iAKanal)) >= 0);
end;

{-------------------------- TCustomDSfGConnection -----------------------------}

{-----------------------------------------}
constructor TCustomDSfGConnection.Create;
{-----------------------------------------}
begin
  inherited Create;

  FOpened := False;     // Keine Verbindung
  FTimeOut := 60000;    // Timeout 60.000 ms = 60 sec.
  FAdresse := '0';      // Default-Adresse für 'nicht angemeldet'
  InitComponents(True);
end;

{-----------------------------------------}
destructor TCustomDSfGConnection.Destroy;
{-----------------------------------------}
begin
  TimeOut := 0;
  FOpened := False;     // Keine Verbindung

  InitConnection(False);
  InitComponents(False);

  inherited Destroy;
end;

{ Zeitverzögertes Freigeben des Objects   }
{ Parameter: Wartezeit                    }
{-----------------------------------------}
procedure TCustomDSfGConnection.Release(iRelease: integer);
{-----------------------------------------}
begin
  TimeOut := 0;
  InitConnection(False);
  FreeTimer(Self, iRelease);
end;

{ Initialisieren/Freigeben v. Komponenten }
{ Parameter: T-Init.; F-Freigeben         }
{-----------------------------------------}
procedure TCustomDSfGConnection.InitComponents(aState: boolean);
{-----------------------------------------}
begin
  if (aState) then begin
    with TProgramIni.Create() do
    try
      with TPathServer.Create(WieserIniFile, [WNetProgDir], False, False) do
      try
        FSystemEinstellungen :=
          TSystemEinstellungen.Create(PathName[WNetProgDir]);
      finally
        Free;
      end;
    finally
      Free;
    end;
  end
  else begin
    FreeAndNil(FSystemEinstellungen);
  end;
end;

{ Setzt den Timeout für DSfG-Telegramme   }
{ Parameter: Timeout in ms                }
{-----------------------------------------}
procedure TCustomDSfGConnection.SetTimeOut(Value: integer);
{-----------------------------------------}
begin
  if (Value <> FTimeOut) then FTimeOut := Value;
end;

{ Setzt den 'Geöffnet'-Zustand            }
{ Parameter: 'Geöffnet'-Zustand           }
{-----------------------------------------}
procedure TCustomDSfGConnection.SetOpened(Value: boolean);
{-----------------------------------------}
begin
  if (Value <> FOpened) then FOpened := Value;
end;

{ Setzt die eigene DSfG-Adresse           }
{ Parameter: DSfGAdresse                  }
{-----------------------------------------}
procedure TCustomDSfGConnection.SetAdresse(Value: char);
{-----------------------------------------}
begin
  if (Value <> FAdresse) and (Value in ['0', 'A'..'_']) then FAdresse := Value;
end;

end.

