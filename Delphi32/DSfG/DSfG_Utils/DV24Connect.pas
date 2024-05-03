{------------------------------------------------------------------------------}
{ Objekt für DSfG-Abrufe via serielle Schnittstelle                            }
{                                                                              }
{ 05.03.2001 GD  Neu                                                           }
{ 10.06.2002 GD  Timeouts hochgesetzt (MRG 2100)                               }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001                                          }
{------------------------------------------------------------------------------}
unit DV24Connect;

interface

uses
  SysUtils, Windows, Classes, Controls, Forms, ExtCtrls, DConnect, Serial,
  GD_Utils, DSG_Utils, T_Zeit, DMomLists, WSysCon, DListen, DStaDll, DSysCon,
  DArchivDll;

type
  TV24AnzeigeProc = procedure(sText: string; iState: byte) of Object;

  TDSfGV24Connection = class(TCustomDSfGConnection)
    constructor Create; override;
  private
    FSerial  : TSerial;
    FComPort : byte;
    FBusy    : boolean;
    FV24AnzeigeProc : TV24AnzeigeProc;
    FParametrierung : boolean;
    FTeilnehmer     : string;
    FAbruf          : boolean;
    function GetZeitangaben: TZeitangaben;
    function GetArchivByONummer(cInstAdr: char; iAGNr, iAKNr: byte;
      iONrVon, iONrBis: integer): string;
    function GetArchivByTime(cInstAdr: char; iAGNr, iAKNr: byte;
      dtVon, dtBis: TDateTime): string;
    function GetLogbuchByONummer(cInstAdr, cQuellAdresse: char;
      iONrVon, iONrBis: integer): string;
    function GetLogbuchByTime(cInstAdr, cQuellAdresse: char;
      dtVon, dtBis: TDateTime): string;
    function CheckArchivTelegramm(sTelegramm: string; iOVon: integer;
      dtVon: TDateTime): boolean;
  protected
    procedure InitComponents(aState: boolean); override;
    procedure SetComPort(Value: byte); virtual;
    property IsParametrierung: boolean
      read FParametrierung write FParametrierung;
  public
    procedure InitParameter(aState: boolean); override;
    function StartConnectionTime: integer; override;
    procedure InitConnection(aState: boolean; iId: integer = -1); override;
    function InitAutoDatenAbruf(iStaId: integer): boolean; override;
    function InitManuDatenAbruf(iStaId: integer; pAbrufListe: TArchivAbrufList;
      dtVon, dtBis: TDateTime): boolean; override;
    procedure SendTelegramm(sTelegramm: string); override;
    function ReceiveTelegramm: string; override;
    function SendReceiveTelegramm(
      sTelegramm: string; bFolgetelegramme: boolean = True): string; override;
    function ReadParameter(
      cIAdr: char; sDEAVon: string; sDEABis: string = ''): string; override;
    function WriteParameter(
      cIAdr: char; sZC1, sZC2, sDEAdr, sValue: string; iType: byte = 0): string;
    function ReadStammdaten(iState: byte = 2): TStrings; override;
    function IsTeilnehmer: Boolean; override;
    function GetTeilnehmer: string; override;
    property V24AnzeigeProc: TV24AnzeigeProc
      read FV24AnzeigeProc write FV24AnzeigeProc;
    property ComPort: byte read FComPort write SetComPort;
    property Busy: boolean read FBusy;
    property AbrufAktiv: boolean read FAbruf;
  end;

  TDV24MomConnection = class(TDSfGV24Connection)
  private
    FMomList     : TDSfGMomDefList;
    FNewDataList : TStrings;
    FMomCallBack : TNeueMomentanwerteCB;
    FTempTimer   : TTimer;
    FNewData     : boolean;
    procedure TempTimerTimer(Sender: TObject);
    procedure HoleNeueDaten;
  protected
    procedure InitComponents(aState: boolean); override;
    function GetNewData: boolean; virtual;
  public
    procedure InsertMomData(cIAdr: char; sDEAVon, sDEABis: string);
    procedure DeleteMomData(cIAdr: char; sDEAVon, sDEABis: string);
    function GetAnzeigeDaten: TDSfGMomValueList; virtual;
    procedure CallMomData;
    procedure ClearMomData;
    property MomList: TDSfGMomDefList read FMomList;
    property NewData: boolean read GetNewData;
    property NeueMomentanwerteCB: TNeueMomentanwerteCB
      read FMomCallBack write FMomCallBack;
  end;


implementation

const
  C_StartConnectionTime = 10000;

{--------------------------- Allgemeine Funktionen ----------------------------}

{ Wandelt ein DSfG- in ein V24-Telegramm  }
{ Achtung: Sendetelegramm !               }
{ Parameter: DSfG-Telegramm               }
{ Rückgabe: V24-Telegramm oder ''         }
{-----------------------------------------}
function ConvertDSfGToV24Telegram(sDSfGTelegram: string): string;
{-----------------------------------------}
var
  cReceiveAddress : char;
  s               : string;
  i, j            : integer;
begin
  Result := '';  // Vorgabe

  if (Length(sDSfGTelegram) > 10) and (sDSfGTelegram[1] = Chr(stx)) and
     (sDSfGTelegram[Length(sDSfGTelegram)] = Chr(etx)) then
  begin
    CReceiveAddress := sDSfGTelegram[2];
    s := sDSfGTelegram;

    // die ersten 6 <us> müssen raus ...
    for i := 1 to 6 do begin
      j := Pos(Chr(us), s);
      if (j > 0) then System.Delete(s, 1, j) else Exit;
    end;

    // Ergebnis zusammensetzen
    Result := Chr(stx) + 'D' + Chr(us) + cReceiveAddress + Chr(us) + s;
  end;
end;

{ Wandelt ein V24- in ein DSfG-Telegramm  }
{ Achtung: Empfangstelegramm !            }
{ Parameter: V24-Telegramm                }
{ Rückgabe: DSfG-Telegramm oder ''        }
{-----------------------------------------}
function ConvertV24ToDSfGTelegram(sDSfGTelegram: string): string;
{-----------------------------------------}
var
  cReceiveAddress : char;
begin
  Result := '';  // Vorgabe

  if (Length(sDSfGTelegram) > 10) and (sDSfGTelegram[1] = Chr(stx)) and
     (sDSfGTelegram[Length(sDSfGTelegram)] = Chr(etx)) then
  begin
    CReceiveAddress := sDSfGTelegram[4];
    Result := Copy(sDSfGTelegram, 5, Length(sDSfGTelegram)-4);

    Result := Chr(stx) + Chr(us)
                   + '255' + Chr(us)
                   + '1' + Chr(us)
                   + '1' + Chr(us)
                   + '1' + Chr(us)
                   + CReceiveAddress + Result;
  end;
end;

{---------------------------- TDSfGV24Connection ------------------------------}

{-----------------------------------------}
constructor TDSfGV24Connection.Create;
{-----------------------------------------}
begin
  inherited Create;

  FComPort := 1;
  TimeOut := 60000;    // 10.06.2002
  FBusy := False;
  FAbruf := False;  // Flag, ob gerade ein Abruf aktiv ist
  FV24AnzeigeProc := nil;
  FTeilnehmer := '';  // Speichert zuletzt abgerufene Teilnehmerliste intern
end;

{ Initialisieren/Freigeben v. Komponenten }
{ Parameter: T-Init.; F-Freigeben         }
{-----------------------------------------}
procedure TDSfGV24Connection.InitComponents(aState: boolean);
{-----------------------------------------}
begin
  inherited;

  if (aState) then begin
    // Serielle Komponente
    if (not Assigned(FSerial)) then begin
      FSerial := TSerial.Create(nil);
      FSerial.BufSizeRec := 8192;
      FSerial.BufSizeTrm := 8192;
      FSerial.COMPort := 1;
      FSerial.Baudrate := br_009600;
      FSerial.DataBits := db_8;
      FSerial.StopBits := sb_1;
      FSerial.ParityBit := even;
    end;
  end
  else begin
    // Serielle Komponente
    if (Assigned(FSerial)) then begin
      if (FSerial.Active) then FSerial.Active := False;
      FSerial.Free;
    end;
  end;
end;

{ Max. Zeit für Verbindungsaufbau         }
{ Rückgabe: Max. Zeit in Millisec.        }    
{-----------------------------------------}
function TDSfGV24Connection.StartConnectionTime: integer;
{-----------------------------------------}
begin
  Result := C_StartConnectionTime;
end;

{ Initialisiert/Beendet Verbindung        }
{ Parameter: T/F-Init./Beend.; StationsId }
{-----------------------------------------}
procedure TDSfGV24Connection.InitConnection(aState: boolean; iId: integer = -1);
{-----------------------------------------}
begin
  if (aState) and (not FSerial.Active) then begin
    FSerial.Active := True;
  end
  else if (not aState) and (FSerial.Active) then begin
    FSerial.Active := False;
  end;

  Opened := FSerial.Active;
end;

{ Gibt Zeitangaben für Station zurück     }
{ Rückgabe: Zeitangabenrecord             }
{-----------------------------------------}
function TDSfGV24Connection.GetZeitangaben: TZeitangaben;
{-----------------------------------------}
var
  i        : integer;
  s, sWert : string;
  tzi      : TIME_ZONE_INFORMATION;
begin
  FillChar(Result, SizeOf(Result), 0);  // Default

  // Schleife über alle aktuellen Teilnehmer (setzt GetTeilnehmer vorraus)
  for i := 1 to Length(FTeilnehmer) do
    if (FTeilnehmer[i] in ['0', 'A'..'_']) then begin
      // Telegramm zusammenstellen und abrufen
      s := C_DEA_DatumZeit + chr(gs) + C_DEA_Zeitzone + chr(gs) +
        C_DEA_LetztVerstZz;
      s := GetMultiDEDSfGTelegramm(Adresse, FTeilnehmer[i], s, 3);
      s := SendReceiveTelegramm(s);
      // Antwortdaten auswerten
      sWert := GetWertFromTelegramm(s, C_DEA_Zeitzone);
      if (Trim(sWert) <> '') then begin
        Result.EAdr := FTeilnehmer[i];
        Result.Zeitzone := Trim(sWert[1]);
        Result.DatumZeit := UnixToDateTime(StrToIntDef(
          '$' + GetWertFromTelegramm(s, C_DEA_DatumZeit), 0));
        Result.LetztVerstZZ := UnixToDateTime(StrToIntDef(
          '$' + GetWertFromTelegramm(s, C_DEA_LetztVerstZz), 0));
        Result.vom_PC := False;

        Break;
      end;
    end;

  // Als Ersatz PC-Zeit und PC-Zeitzone, falls Gerät keine Zeitangaben liefert
  if Result.DatumZeit = 0 then begin
    Result.DatumZeit:=Now;
    if (GetTimeZoneInformation (tzi) = TIME_ZONE_ID_DAYLIGHT)
    then Result.Zeitzone := CMESZ
    else Result.Zeitzone := CMEZ;
    Result.vom_PC:=true;
  end;

end;

{ Archivkanal über Ordungsnummer abfragen }
{ Parameter: Instanzadresse, Gruppe, Kanal}
{            Ordnungsnummer von-bis       }
{ Rückgabe: Antworttelegramm              }
{-----------------------------------------}
function TDSfGV24Connection.GetArchivByONummer(cInstAdr: char;
  iAGNr, iAKNr: byte; iONrVon, iONrBis: integer): string;
{-----------------------------------------}
var
  s : string;
begin
  s := GetDeaArchivkanal(iAGNr, iAKNr) + 'd';  // Archivdaten
  s := GetDatenabfrageONrTelegramm(Adresse, cInstAdr, s, iONrVon, iONrBis);
  Result := SendReceiveTelegramm(s);
end;

{ Archivkanal über Zeitbereich abfragen   }
{ Parameter: Instanzadresse, Gruppe, Kanal}
{            Zeitbereich von-bis          }
{ Rückgabe: Antworttelegramm              }
{-----------------------------------------}
function TDSfGV24Connection.GetArchivByTime(cInstAdr: char; iAGNr, iAKNr: byte;
  dtVon, dtBis: TDateTime): string;
{-----------------------------------------}
var
  s : string;
begin
  s := GetDeaArchivkanal(iAGNr, iAKNr) + 'd';  // Archivdaten
  s := GetDatenabfrageZeitbereichTelegramm(Adresse, cInstAdr, s,
    IntToHex(DateTimeToUnix(dtVon), 8), IntToHex(DateTimeToUnix(dtBis), 8));
  Result := SendReceiveTelegramm(s);
end;

{ Archivkanal über Ordungsnummer abfragen }
{ Parameter: Instanzadresse, Gruppe, Kanal}
{            Ordnungsnummer von-bis       }
{ Rückgabe: Antworttelegramm              }
{-----------------------------------------}
function TDSfGV24Connection.GetLogbuchByONummer(
  cInstAdr, cQuellAdresse: char; iONrVon, iONrBis: integer): string;
{-----------------------------------------}
var
  s : string;
begin
  s := GetDeaLogbuch(cQuellAdresse) + 'd';  // Archivdaten
  s := GetDatenabfrageONrTelegramm(Adresse, cInstAdr, s, iONrVon, iONrBis);
  Result := SendReceiveTelegramm(s);
end;

{ Archivkanal über Zeitbereich abfragen   }
{ Parameter: Instanzadresse, Gruppe, Kanal}
{            Zeitbereich von-bis          }
{ Rückgabe: Antworttelegramm              }
{-----------------------------------------}
function TDSfGV24Connection.GetLogbuchByTime(
  cInstAdr, cQuellAdresse: char; dtVon, dtBis: TDateTime): string;
{-----------------------------------------}
var
  s : string;
begin
  s := GetDeaLogbuch(cQuellAdresse) + 'd';  // Archivdaten
  s := GetDatenabfrageZeitbereichTelegramm(Adresse, cInstAdr, s,
    IntToHex(DateTimeToUnix(dtVon), 8), IntToHex(DateTimeToUnix(dtBis), 8));
  Result := SendReceiveTelegramm(s);
end;

{ Prüft Telegramm formal und auf Daten    }
{ Parameter: Telegramm, OrdNr von, Zeit v.}
{ Rückgabe: OK ja/nein                    }
{-----------------------------------------}
function TDSfGV24Connection.CheckArchivTelegramm(
  sTelegramm: string; iOVon: integer; dtVon: TDateTime): boolean;
{-----------------------------------------}
var
   i, iZAE     : integer;
   iONr        : integer;
   sDtVon      : string;
begin
  Result := False;

  // HDCL prüfen, Anzahl der Datenelemente ermitteln
  if (CheckDSfGHeader(sTelegramm, iZAE) <> 0) then Exit;

  // Wenn Ordnungsnummer oder Zeit = 0, dann Prüfung beenden
  if (iOVon = 0) or (dtVon = 0) then begin
    Result := True;
    Exit;
  end;

  sDtVon := IntToHex(DateTimeToUnix(dtVon), 8);  // DatumZeit in DSfG-Unix-String

  with GetDatenListe(sTelegramm) do
  try
    for i := 0 to Count-1 do begin
      iONr := StrToInt(GetStringPart(Strings[i], 4));
      if (iONr = iOVon) then begin
        Result := (sDtVon = GetStringPart(Strings[i], 3)); // Datumsvergleich
        Break;
      end;
    end;
  finally
    Free;
  end;
end;

{ Abruf von Auto-Daten anstoßen           }
{ Parameter: StationsId                   }
{ Rückgabe: Erfolg ja/nein                }
{-----------------------------------------}
function TDSfGV24Connection.InitAutoDatenAbruf(iStaId: integer): boolean;
{-----------------------------------------}
var
  pArchivAbrufListe   : TAbrufList;
  pLogbuchAbrufListe  : TAbrufList;
  s, sTeilnehmer      : string;
  i, j, iOVon, iOBis  : integer;
  b                   : boolean;
  dtVon, dtBis        : TDateTime;
  pAbrufListObj       : TAbrufListObj;
  pArchivDatenZugriff : TArchivDatenZugriff;
begin
  FAbruf := True;
  Result := False;

  // Ggf. aktuelle Verbingung beenden
  if (Opened) then begin
    InitConnection(False);
    Delay(3000);
  end;

  pArchivAbrufListe := TAbrufList.Create;
  pLogbuchAbrufListe := TAbrufList.Create;
  pArchivDatenZugriff := TArchivDatenZugriff.Create;
  try
    // Verbindung zur angeschlossenen Station herstellen
    InitConnection(True);
    sTeilnehmer := StringReplace(GetTeilnehmer, '.', '', [rfReplaceAll]);
    if (sTeilnehmer = '') then Exit;

    // Stimmt die Station ?
    s := '';  //  Zwischenspeicher für Registrierinstanzen
    with ClientStammdaten.FillDInstanzListe(ClientStammdaten.DStationsName(iStaId))
    do begin
      for i := 1 to Length(sTeilnehmer) do begin
        b := False;  // Teilnehmer stimmt nicht mit Stammdaten überein
        for j := 0 to Count-1 do
          if (InstanzAdresse[j] = sTeilnehmer[i]) then begin
            // Prüfen von Instanztyp, Gerätetyp und Fabriknummer
            if (Pos(InstanzTyp[j], GetWertFromTelegramm(ReadParameter(
              sTeilnehmer[i], C_DEA_InstTyp), C_DEA_InstTyp)) = 0) or
               (Pos(InstanzGeraetetyp[j], GetWertFromTelegramm(ReadParameter(
              sTeilnehmer[i], C_DEA_GerTyp), C_DEA_GerTyp)) = 0) or
               (InstanzFabriknummer[j] <> Trim(GetWertFromTelegramm(ReadParameter(
              sTeilnehmer[i], C_DEA_FabrikNr), C_DEA_FabrikNr)))
            then Break
            else begin
              b := True;
              if (InstanzTyp[j] = C_D_Instanztyp_Reg) then
                s := s + sTeilnehmer[i];
              Break;
            end;
          end;
        if (not (b)) then Exit;
      end;
    end;
    if (Length(s) = 0) then Exit else sTeilnehmer := s;  // Reg.Inst. vorhanden ?

    // Abruflisten füllen
    if (not ClientStammdaten.GetDatenAbrufliste(
      iStaId, C_IsArchive, pArchivAbrufListe, True)) then Exit;
    if (not ClientStammdaten.GetDatenAbrufliste(
      iStaId, C_IsLogbuecher, pLogbuchAbrufListe, True)) then Exit;


    pArchivDatenZugriff.NewArchive;  // Abrufliste für Konvertierung initial.

    // Archivabrufe durchführen
    for i := 0 to pArchivAbrufListe.Count-1 do begin
      pAbrufListObj := TAbrufListObj(pArchivAbrufListe.Items[i]);
      if (Pos(pAbrufListObj.EAdr, s) = 0) or
         (not (pAbrufListObj.BusAdr_Quelle[1] in ['A'..'_']))
      then Continue;

      // Bisherige Archiveckwerte holen
      if (not pArchivDatenZugriff.GetLetztWertOrdNr_DatumZeit(iStaId,
        pAbrufListObj.InstanzId, pAbrufListObj.Gruppe, pAbrufListObj.Kanal,
        pAbrufListObj.Kanaltyp, iOVon, dtVon))
      then Break;  // Schwerer Fehler

      // Aktuellen Archivfüllstand holen
      dtBis := 0;
      iOBis := 0;
      if (iOVon = 1) and (iOBis = 1) then;  // Dummy zur Hinweisvermeidung
      s := GetDeaArchivgruppe(pAbrufListObj.Gruppe) + 'd';  // Füllstand
      s := GetOneDEDSfGTelegramm(Adresse, pAbrufListObj.EAdr[1], s);
      s := SendReceiveTelegramm(s);
      s := Trim(GetWertFromTelegramm(s, 0));
      if (s = '') then Break else iOBis := StrToInt(s);

      // Parameter 'ab Ordnungsnummer' ermitteln
      if (iOVon = 0) then iOVon := iOBis - 24;  // noch keine Daten vorhanden
      if (iOVon < 1) then iOVon := 1;
      if (iOVon > iOBis) then dtBis := Now + 1; // Daten gelöscht -> Zeitabruf

      // Archivdaten holen
      if (dtBis > 0) then s := GetArchivByTime(pAbrufListObj.EAdr[1],
        pAbrufListObj.Gruppe, pAbrufListObj.Kanal, dtVon, dtBis)
      else begin
        s := GetArchivByONummer(pAbrufListObj.EAdr[1],
          pAbrufListObj.Gruppe, pAbrufListObj.Kanal, iOVon, iOBis);
        if (not CheckArchivTelegramm(s, iOVon, dtVon)) then
          s := GetArchivByTime(pAbrufListObj.EAdr[1],
            pAbrufListObj.Gruppe, pAbrufListObj.Kanal, dtVon, Now+1)
      end;
      pArchivDatenZugriff.InsertArchivTelegramm(pAbrufListObj.InstanzId,
        pAbrufListObj.Gruppe, pAbrufListObj.Kanal,
        pAbrufListObj.InstanzId_Quelle, pAbrufListObj.GerTypNr_Quelle, s);
    end;

    // Logbuchabrufe durchführen
    for i := 0 to pLogbuchAbrufListe.Count-1 do begin
      pAbrufListObj := TAbrufListObj(pLogbuchAbrufListe.Items[i]);
      if (Pos(pAbrufListObj.EAdr, s) = 0) or
         (not (pAbrufListObj.BusAdr_Quelle[1] in ['A'..'_']))
      then Continue;

      // Bisherige Logbucheckwerte holen
      if (not pArchivDatenZugriff.GetLetztDSfGMeldungOrdNr_DatumZeit(
        C_AbrArtAuto, pAbrufListObj.InstanzId, pAbrufListObj.Gruppe, iOVon, dtVon))
      then Break;  // Schwerer Fehler

      // Aktuellen Archivfüllstand holen
      dtBis := 0;
      iOBis := 0;
      if (iOVon = 1) and (iOBis = 1) then;  // Dummy zur Hinweisvermeidung
      s := GetDeaLogbuch(pAbrufListObj.BusAdr_Quelle[1]) + 'b';  // Füllstand
      s := GetOneDEDSfGTelegramm(Adresse, pAbrufListObj.EAdr[1], s);
      s := SendReceiveTelegramm(s);
      s := Trim(GetWertFromTelegramm(s, 0));
      if (s = '') then Break else iOBis := StrToInt(s);

      // Parameter 'ab Ordnungsnummer' ermitteln
      if (iOVon = 0) then iOVon := iOBis - 50;  // noch keine Daten vorhanden
      if (iOVon < 1) then iOVon := 1;
      if (iOVon > iOBis) then dtBis := Now + 1; // Daten gelöscht -> Zeitabruf

      // Logbuchdaten holen
      if (dtBis > 0) then s := GetLogbuchByTime(pAbrufListObj.EAdr[1],
        pAbrufListObj.BusAdr_Quelle[1], dtVon, dtBis)
      else begin
        s := GetLogbuchByONummer(pAbrufListObj.EAdr[1],
          pAbrufListObj.BusAdr_Quelle[1], iOVon, iOBis);
        if (not CheckArchivTelegramm(s, iOVon, dtVon)) then
          s := GetLogbuchByTime(pAbrufListObj.EAdr[1],
            pAbrufListObj.BusAdr_Quelle[1], dtVon, Now+1)
      end;
      pArchivDatenZugriff.InsertArchivTelegramm(pAbrufListObj.InstanzId,
        pAbrufListObj.Gruppe, pAbrufListObj.Kanal,
        pAbrufListObj.InstanzId_Quelle, pAbrufListObj.GerTypNr_Quelle, s);
    end;

    Result := pArchivDatenZugriff.KonvertiereArchive(
      C_AbrArtAuto, GetZeitangaben, iStaId, 1000, -1, True);
  finally
    pArchivAbrufListe.Free;
    pLogbuchAbrufListe.Free;
    pArchivDatenZugriff.Free;
    InitConnection(False);
    FAbruf := False;
  end;
end;

{ Abruf von Manu-Daten anstoßen           }
{ Parameter: Stations-, Liste mit InstID/ }
{            Archiv-/Kanal-Nr., Zeitraum  }
{ Rückgabe: Erfolg ja/nein                }
{--------------------------------------------}
function TDSfGV24Connection.InitManuDatenAbruf(iStaId: integer;
  pAbrufListe: TArchivAbrufList; dtVon, dtBis: TDateTime): boolean;
{-----------------------------------------}
var
  pArchivAbrufListe   : TAbrufList;
  pLogbuchAbrufListe  : TAbrufList;
  s, sTeilnehmer      : string;
  i, j                : integer;
  b                   : boolean;
  pAbrufListObj       : TAbrufListObj;
  pArchivDatenZugriff : TArchivDatenZugriff;
begin
  FAbruf := True;
  Result := False;

  // Ggf. aktuelle Verbindung beenden
  if (Opened) then begin
    InitConnection(False);
    Delay(3000);
  end;

  pArchivAbrufListe := TAbrufList.Create;
  pLogbuchAbrufListe := TAbrufList.Create;
  pArchivDatenZugriff := TArchivDatenZugriff.Create(False);
  try
    // Bisherige Archivdaten löschen
    if (not pArchivDatenZugriff.DeleteStationsDaten(iStaId)) then Exit;  // Schwerer Fehler

    // Verbindung zur angeschlossenen Station herstellen
    InitConnection(True);
    sTeilnehmer := StringReplace(GetTeilnehmer, '.', '', [rfReplaceAll]);
    if (sTeilnehmer = '') then Exit;

    // Stimmt die Station ?
    s := '';  //  Zwischenspeicher für Registrierinstanzen
    with ClientStammdaten.FillDInstanzListe(ClientStammdaten.DStationsName(iStaId))
    do begin
      for i := 1 to Length(sTeilnehmer) do begin
        b := False;  // Teilnehmer stimmt nicht mit Stammdaten überein
        for j := 0 to Count-1 do
          if (InstanzAdresse[j] = sTeilnehmer[i]) then begin
            // Prüfen von Instanztyp, Gerätetyp und Fabriknummer
            if (Pos(InstanzTyp[j], GetWertFromTelegramm(ReadParameter(
              sTeilnehmer[i], C_DEA_InstTyp), C_DEA_InstTyp)) = 0) or
               (Pos(InstanzGeraetetyp[j], GetWertFromTelegramm(ReadParameter(
              sTeilnehmer[i], C_DEA_GerTyp), C_DEA_GerTyp)) = 0) or
               (InstanzFabriknummer[j] <> Trim(GetWertFromTelegramm(ReadParameter(
              sTeilnehmer[i], C_DEA_FabrikNr), C_DEA_FabrikNr)))
            then Break
            else begin
              b := True;
              if (InstanzTyp[j] = C_D_Instanztyp_Reg) then
                s := s + sTeilnehmer[i];
              Break;
            end;
          end;
        if (not (b)) then Exit;
      end;
    end;
    if (Length(s) = 0) then Exit else sTeilnehmer := s;  // Reg.Inst. vorhanden ?

    // Abruflisten füllen
    if (not ClientStammdaten.GetDatenAbrufliste(
      iStaId, C_IsArchive, pArchivAbrufListe, False)) then Exit;
    if (not ClientStammdaten.GetDatenAbrufliste(
      iStaId, C_IsLogbuecher, pLogbuchAbrufListe, False)) then Exit;


    pArchivDatenZugriff.NewArchive;  // Abrufliste für Konvertierung initial.

    // Archivabrufe durchführen
    for i := 0 to pArchivAbrufListe.Count-1 do begin
      pAbrufListObj := TAbrufListObj(pArchivAbrufListe.Items[i]);
      if (Pos(pAbrufListObj.EAdr, sTeilnehmer) = 0) or
         (not (pAbrufListObj.BusAdr_Quelle[1] in ['A'..'_'])) or
         (not pAbrufListe.HasArchivKanal(pAbrufListObj.InstanzId,
           pAbrufListObj.Gruppe, pAbrufListObj.Kanal))
      then Continue;

      // Ggf. Zeitraum modifizieren
      if (dtBis = 0) then dtBis := Now + 1;

      // Archivdaten holen
      s := GetArchivByTime(pAbrufListObj.EAdr[1],
        pAbrufListObj.Gruppe, pAbrufListObj.Kanal, dtVon, dtBis);
      pArchivDatenZugriff.InsertArchivTelegramm(pAbrufListObj.InstanzId,
        pAbrufListObj.Gruppe, pAbrufListObj.Kanal,
        pAbrufListObj.InstanzId_Quelle, pAbrufListObj.GerTypNr_Quelle, s);
    end;

    // Logbuchabrufe durchführen
    for i := 0 to pLogbuchAbrufListe.Count-1 do begin
      pAbrufListObj := TAbrufListObj(pLogbuchAbrufListe.Items[i]);
      if (Pos(pAbrufListObj.EAdr, sTeilnehmer) = 0) or
         (not (pAbrufListObj.BusAdr_Quelle[1] in ['A'..'_'])) or
         (not pAbrufListe.HasArchivKanal(pAbrufListObj.InstanzId,
           pAbrufListObj.Gruppe, 0))
      then Continue;

      // Ggf. Zeitraum modifizieren
      if (dtBis = 0) then dtBis := Now + 1;

      // Logbuchdaten holen
      s := GetLogbuchByTime(pAbrufListObj.EAdr[1],
        pAbrufListObj.BusAdr_Quelle[1], dtVon, dtBis);
      pArchivDatenZugriff.InsertArchivTelegramm(pAbrufListObj.InstanzId,
        pAbrufListObj.Gruppe, pAbrufListObj.Kanal,
        pAbrufListObj.InstanzId_Quelle, pAbrufListObj.GerTypNr_Quelle, s);
    end;

    Result := pArchivDatenZugriff.KonvertiereArchive(
      C_AbrArtManu, GetZeitangaben, iStaId, 1000, -1, True);
  finally
    pArchivAbrufListe.Free;
    pLogbuchAbrufListe.Free;
    pArchivDatenZugriff.Free;
    InitConnection(False);
    FAbruf := False;
  end;
end;

{ Initialisiert/Beendet Parametrierung       }
{ Parameter: Init. (T), Beenden (F)          }
{--------------------------------------------}
procedure TDSfGV24Connection.InitParameter(aState: boolean);
{--------------------------------------------}
begin
  FParametrierung := aState;
end;

{ Versendet ein DSfG-Telegramm            }
{ Parameter: DSfG-Telegramm               }
{-----------------------------------------}
procedure TDSfGV24Connection.SendTelegramm(sTelegramm: string);
{-----------------------------------------}
var
  s : string;
begin
  if (Opened) then begin
    if (FSerial.TransmittText(ConvertDSfGToV24Telegram(sTelegramm)) > 0)
    then s := sTelegramm
    else s := 'Fehler beim Senden: ' + sTelegramm;

    if (Assigned(FV24AnzeigeProc)) then begin
      FV24AnzeigeProc('', 2);
      FV24AnzeigeProc(s, 1);
    end;
  end;
end;

{ Empfängt ein DSfG-Telegramm             }
{ Rückgabe: DSfG-Telegramm oder ''        }
{-----------------------------------------}
function TDSfGV24Connection.ReceiveTelegramm: string;
{-----------------------------------------}
var
  s, s1 : string;
  oldCursor : TCursor;
  iStop : Cardinal;
begin
  Result := '';  // Default

  if (Opened) then begin
    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      s := FSerial.ReceiveText;
      if (Pos(Chr(stx), s) = 0) then begin
        s := '';
      end
      else if (Pos(Chr(etx), s) = 0) then begin
        iStop := GetTickCount + Cardinal(TimeOut);

        while (Pos(Chr(etx), s) = 0) and (GetTickCount < iStop) do begin

          if (not Opened) then Exit;
          Delay(10);
          if (not Opened) then Exit;

          s1 := FSerial.ReceiveText;
          if (Length(s1) > 0) then begin
            if (Assigned(FV24AnzeigeProc)) then FV24AnzeigeProc(s1, 2);
            s := s + s1;
          end;
        end;
      end;
    finally
      Screen.Cursor := oldCursor;
    end;

    if (Pos(Chr(stx), s) > 0) and (Pos(Chr(etx), s) > 0) then Result := s;
  end;
end;

{ Sendet ein DSfG-Telegramm und holt die  }
{ Antwort                                 }
{ Parameter: DSfG-Telegramm;              }
{            Flag für Folgetelegramme     }
{ Rückgabe: DSfG-Telegramm oder ''        }
{-----------------------------------------}
function TDSfGV24Connection.SendReceiveTelegramm(
  sTelegramm: string; bFolgetelegramme: boolean = True): string;
{-----------------------------------------}
var
  s : string;
  iStop : Cardinal;
  i, j : integer;
  pSl  : TStrings;
  oldCursor : TCursor;
begin
  Result := '';
  if (not Opened) or ((Busy) and (bFolgetelegramme)) then Exit;

  FBusy := True;
  try
    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try

      SendTelegramm(sTelegramm);

      iStop := GetTickCount + Cardinal(TimeOut);

      while (Result = '') and (GetTickCount < iStop)
      do begin
        if (not Opened) then Exit;
        Delay(10);
        Result := ReceiveTelegramm;
        if (not Opened) then Exit;
      end;

      // Prüfung auf Folgetelegramme (bei 'U' und DEB = 'V', 'O', 'Z')
      Result := ConvertV24ToDSfGTelegram(Result);
      s := Result;
      if (bFolgetelegramme) and (
          (GetDSfGDEB(sTelegramm) = 'V') or (GetDSfGDEB(sTelegramm) = 'O')  or
          (GetDSfGDEB(sTelegramm) = 'Z'))
      then

        while (GetDSfGNTY(s) = 'U') do begin

          if (not Opened) then Exit;


      // Bei Bereichsabfragen
          if (GetDSfGDEB(sTelegramm) = 'V') then
            sTelegramm := ModifyDSfGParamTelegramm(sTelegramm, s)
        // Bei Ordnungsnummernabfragen
          else if (GetDSfGDEB(sTelegramm) = 'O') then
            sTelegramm := ModifyDSfGONrTelegramm(sTelegramm, s)
        // Bei Zeitbereichsabfragen
          else if (GetDSfGDEB(sTelegramm) = 'Z') then
            sTelegramm := ModifyDSfGZeitBereichsTelegramm(sTelegramm, s)
        // sonst abbrechen
          else sTelegramm := '';

          if (sTelegramm <> '') then begin
            s := SendReceiveTelegramm(sTelegramm, False);

            if (Length(s) > 8) then begin
        // Telegramme zusammensetzen
              // Datenteil bisheriges Ergebnis
              pSl := GetDatenListe(Result);
              try
                j := pSl.Count;
                Result := '';
                for i := 0 to pSl.Count-1 do Result := Result + pSl[i] + Chr(gs);
              finally
                pSl.Free;
              end;
              // Datenteil neu hinzugekommenes Ergebnis
              pSl := GetDatenListe(s);
              try
                j := j + pSl.Count;
                for i := 0 to pSl.Count-2 do Result := Result + pSl[i] + Chr(gs);
                Result := Result + pSl[pSl.Count-1] + Chr(fs) + Chr(etx);
              finally
                pSl.Free;
              end;
              // Deklarationsteil erstellen
              Result := IntToStr(j) + Chr(us) + Result;  // ZAE
              for i := 9 downto 1 do Result := GetStringPart(s, i) + Chr(us) + Result;
            end;
          end;
        end;

    finally
      Screen.Cursor := oldCursor;
    end;
  finally
    FBusy := False;
  end;
end;

{ Liest einen Parameter(-bereich) aus     }
{ Parameter: Instanzadresse; DEA von;     }
{            DEA bis                      }
{ Rückgabe: Antworttelegramm oder ''      }
{-----------------------------------------}
function TDSfGV24Connection.ReadParameter(
  cIAdr: char; sDEAVon: string; sDEABis: string = ''): string;
{-----------------------------------------}
var
  s : string;
begin
  if (Opened) and (cIAdr in ['0', 'A'..'_']) and (sDEAVon <> '') then begin
  // Telegramm zusammenstellen
    if (sDEABis = '')
    then s := GetOneDEDSfGTelegramm(Adresse, cIAdr, sDEAVon)
    else s := GetBereichsabfrageDSfGTelegramm(Adresse, cIAdr, sDEAVon, sDEABis);

    Result := SendReceiveTelegramm(s);
  end
  else Result := '';
end;

{ Ruft Parameteränderung auf                 }
{ Parameter: Instanzadresse, Zugangscode 1+2 }
{            DEA, neuer Wert                 }
{ Rückgabe: Wert nach Änderung oder ''       }
{--------------------------------------------}
function TDSfGV24Connection.WriteParameter(
  cIAdr: char; sZC1, sZC2, sDEAdr, sValue: string; iType: byte = 0): string;
{--------------------------------------------}
var
  oldCursor : TCursor;
  iStellen  : integer;
  sTelegram : string;
begin
  Result := '';  // Default;

  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try

  // Datenelement abrufen, Stellenzahl ermitteln
    if (iType = StrToIntDef(C_DEA_Typ_Zeichenkette, 0)) then begin
      sTelegram := ReadParameter(cIAdr, sDEAdr);
      if (sTelegram = '')
      then Exit
      else with GetDatenListe(sTelegram) do
      try
        if (Count = 1)
        then iStellen := Length(GetStringPart(Strings[0], 2))
        else Exit;
        if (iStellen = 0) and (Pos(sDEAdr, Strings[0]) = 0) then Exit;
      finally
        Free;
      end;
  // Neuen Wert formatieren
      while (Length(sValue) < iStellen) do sValue := sValue + ' ';
    end;

  // Parameter einstellen
    sTelegram := GetEinstDSfGTelegramm('0', cIAdr, sZC1, sZC2, sDEAdr, sValue);

    if (not Opened) then Exit;

    sTelegram := SendReceiveTelegramm(sTelegram);

    if (not Opened) then Exit;

    if (sTelegram = '') or (GetDSfGDatenTeil(sTelegram) = '?' + Chr(fs)) then
    begin                             // Wert einzeln abfragen
      sTelegram := ReadParameter(cIAdr, sDEAdr);
      if (sTelegram = '')
      then Exit
      else with GetDatenListe(sTelegram) do
      try
        if (Count = 1) then Result := GetStringPart(Strings[0], 2);
      finally
        Free;
      end;
    end
    else with GetDatenListe(sTelegram) do  // Sonst Wert aus Antwort
    try
      if (Count > 0) then Result := GetStringPart(Strings[Count-1], 2);
    finally
      Free;
    end;

  finally
    Screen.Cursor := oldCursor;
  end;
end;

{ Ist das Objekt ein Busteilnehmer ?      }
{ Rückgabe: Ja/Nein                       }
{-----------------------------------------}
function TDSfGV24Connection.IsTeilnehmer: Boolean;
{-----------------------------------------}
var
  s          : string;
  oldCursor  : TCursor;
  iOldTimeOut : integer;
  iTimeOut    : Cardinal;
begin
  Result := False;  // Default;

  if (Opened) then begin
    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try

      SendTelegramm(GetOneDEDSfGTelegramm(Adresse, 'A', 'aaa'));
      Delay(200);

      iOldTimeOut := TimeOut;
      Timeout := 5000;  // 10.06.2002
      try
        s := '';
        iTimeOut := GetTickCount + 5000;
        while (GetTickCount < iTimeOut) and (Pos(Chr(stx), s) = 0) do begin
          s := ReceiveTelegramm;
          Result := (Pos(Chr(stx), s) > 0) and (Pos(Chr(etx), s) > 0);
        end;
      finally
        TimeOut := iOldTimeOut;
      end;

    finally
      Screen.Cursor := oldCursor;
    end;
  end;
end;

{ Holt alle erreichbaren Busteilnehmer    }
{ Rückgabe: Teilnehmer als DSfG-String    }
{-----------------------------------------}
function TDSfGV24Connection.GetTeilnehmer: string;
{-----------------------------------------}
var
  c          : char;
  oldCursor  : TCursor;
  oldTimeOut : integer;
begin
  Result := '';

  if (Opened) then begin
    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      oldTimeOut := TimeOut;
      TimeOut := 20000;  // 10.06.2002
      try
        for c := 'A' to '_' do begin

          if (not Opened) then Exit;

          if (ReadParameter(c, C_DEA_InstTyp) <> '')
          then Result := Result + c
          else Result := Result + '.';
        end;
      finally
        TimeOut := oldTimeOut;
      end;
    finally
      Screen.Cursor := oldCursor;
    end;
  end
  else for c := 'A' to '_' do Result := Result + '.';

  FTeilnehmer := Result;
end;

{ Setzt die Nummer der Schnittstelle      }
{ Parameter: Schnittstellennummer (ab 1)  }
{-----------------------------------------}
procedure TDSfGV24Connection.SetComPort(Value: byte);
{-----------------------------------------}
var
  bActive : boolean;
begin
  if (Value <> FComPort) and (Value > 0) then begin
    bActive := FSerial.Active;
    if (FSerial.Active) then FSerial.Active := False;
    FSerial.COMPort := Value;
    FComPort := FSerial.COMPort;
    if (bActive) then FSerial.Active := True;
  end;
end;

{ Einlesen der Stammdaten in Rohfileliste }
{ Parameter: Status (1-allg., 2-alles)    }
{ Rückgabe: Antworttelegramme mit stamm-  }
{           datenrelevanten Datenelementen}
{-----------------------------------------}
function TDSfGV24Connection.ReadStammdaten(iState: byte = 2): TStrings;
{-----------------------------------------}
var
  sTeilnehmer  : string;
  sTelegramm   : string;
  i            : integer;

  function GetInstTyp(s: string): char;
  var
    x : integer;
  begin
    Result := 'X';  // Default

    with GetDatenListe(s) do
    try
      for x := 0 to Count-1 do
        if (GetStringPart(Strings[x], 1) = C_DEA_InstTyp) then begin
          if (Length(GetStringPart(Strings[x], 2)) > 0) then
            Result := GetStringPart(Strings[x], 2)[1];
          Break;
        end;
    finally
      Free;
    end;
  end;

begin
  Result := TStringList.Create;

  // Erreichbare Busteilnehmer holen
  sTeilnehmer := GetTeilnehmer;
  // Schleife über alle Teilnehmer
  for i := 1 to Length(sTeilnehmer) do
    if (sTeilnehmer[i] in ['0', 'A'..'_']) then begin
    // Allgemeine Datenelemente holen
      sTelegramm := C_DEA_InstTyp + Chr(gs) + C_DEA_Hersteller + Chr(gs) +
        C_DEA_GerTyp + Chr(gs) + C_DEA_FabrikNr + Chr(gs) + C_DEA_Baujahr +
        Chr(gs) + C_DEA_GerSwVs + Chr(gs) + C_DEA_Inbetriebnahme;
      sTelegramm := GetMultiDEDSfGTelegramm('0',sTeilnehmer[i], sTelegramm, 7 );

      if (not Opened) then Exit;

      sTelegramm := SendReceiveTelegramm(sTelegramm);
      if (not Opened) then Exit;

      if (sTelegramm = '') then Continue
      else begin
        Result.Add(sTeilnehmer[i] + ' ' + sTelegramm);
    // Datenelemente der Registrierinstanz holen
        if (iState = 2) and (GetInstTyp(sTelegramm) = C_D_Instanztyp_Reg) then
        begin
          sTelegramm :=
            GetBereichsabfrageDSfGTelegramm('0', sTeilnehmer[i], 'c', 'd');

          if (not Opened) then Exit;

          sTelegramm := SendReceiveTelegramm(sTelegramm);

          if (not Opened) then Exit;

          if (sTelegramm = '')
          then Continue
          else Result.Add(sTeilnehmer[i] + 'R' + sTelegramm);
        end;

      end;
    end;
end;

{---------------------------- TDV24MomConnection ------------------------------}

{ Initialisieren/Freigeben v. Komponenten }
{ Parameter: T-Init.; F-Freigeben         }
{-----------------------------------------}
procedure TDV24MomConnection.InitComponents(aState: boolean);
{-----------------------------------------}
begin
  inherited;

  if (aState) then begin
    FNewDataList := TStringList.Create;

    FMomList := TDSfGMomDefList.Create;
    FTempTimer := TTimer.Create(nil);
    FTempTimer.Enabled := False;
    FTempTimer.Interval := 2000;
    FTempTimer.OnTimer := TempTimerTimer;
    FTempTimer.Enabled := True;
  end
  else begin
    FTempTimer.Enabled := False;
    FMomCallBack := nil;
    if (Assigned(FTempTimer)) then FTempTimer.Free;
    if (Assigned(FMomList)) then FMomList.Free;
    if (Assigned(FNewDataList)) then FNewDataList.Free;
  end;
end;

{ Fragt die definierten Momentanwerte ab  }
{-----------------------------------------}
procedure TDV24MomConnection.CallMomData;
{-----------------------------------------}
begin
  if (Assigned(FMomCallBack)) then FMomCallBack(GetAnzeigeDaten);
end;

{ Fügt Momentanwerte in Abrufliste ein    }
{ Parameter: InstanzAdr, DEA von, DEA bis }
{-----------------------------------------}
procedure TDV24MomConnection.InsertMomData(cIAdr: char; sDEAVon, sDEABis: string);
{-----------------------------------------}
begin
  if (not Opened) then Exit;

  FMomList.InsertMomDefValue(cIAdr, sDEAVon, sDEABis);
  FTempTimer.Enabled := True;
end;

{ Löscht Daten aus der Abrufdef.-Liste    }
{ Parameter: InstanzAdr, DEA von, DEA bis }
{-----------------------------------------}
procedure TDV24MomConnection.DeleteMomData(cIAdr: char; sDEAVon, sDEABis: string);
{-----------------------------------------}
var
  i : integer;
  c1, c2 : char;
begin
  if (not Opened) then Exit;

  i := FMomList.Indexof(cIAdr + sDEAVon);
  if (i >= 0) then FMomList.Delete(i);

  if (sDEABis <> '') then begin
    c1 := sDEAVon[1];
    c2 := sDEABis[1];
  // DE-Bereich löschen
    for i := FMomList.Count-1 downto 0 do begin
      if (PDMDInsertRec(FMomList.Objects[i])^.cIAdr = cIAdr) and
         (PDMDInsertRec(FMomList.Objects[i])^.sDEAVon[1] in [c1..c2]) then
      begin
        FMomList.Delete(i);
      end;
    end;
  end;

//  FTempTimer.Enabled := True;
end;

{ Leert die Abruf-Definitions-Liste       }
{-----------------------------------------}
procedure TDV24MomConnection.ClearMomData;
{-----------------------------------------}
begin
  FMomList.Clear;
end;

{ Holt die eingestellten Parameter        }
{ Rückgabe: Liste mit Abruf-Ergebnis      }
{-----------------------------------------}
function TDV24MomConnection.GetAnzeigeDaten: TDSfGMomValueList;
{-----------------------------------------}
var
  i : integer;
  p : PDMomValueRec;
begin
  Result := TDSfGMomValueList.Create;

  for i := 0 to FNewDataList.Count-1 do begin
    New(p);
    p^.cIAdr := GetStringPart(FNewDataList[i], 1);
    p^.sDEA := GetStringPart(FNewDataList[i], 2);
    p^.sWert := GetStringPart(FNewDataList[i], 3);
    p^.iStellen := StrToIntDef(GetStringPart(FNewDataList[i], 4), 0);
    Result.AddObject(p^.cIAdr + p^.sDEA, TObject(p));
  end;
end;

{ Holt die eingestellten Parameter        }
{-----------------------------------------}
procedure TDV24MomConnection.HoleNeueDaten;
{-----------------------------------------}
var
  s : string;
  i, j : integer;
  c : char;
  pSl : TStrings;

  procedure InsertToResult;
  var
    iCount : integer;
  begin
    if (Length(s) > 0) then begin
      Delete(s, Length(s), 1);    // Letztes <gs> löschen
      s := GetMultiDEDSfGTelegramm(Adresse, c, s, j);

      if (not Opened) then Exit;

      s := SendReceiveTelegramm(s);

      if (not Opened) then Exit;

      if (s <> '') then
        with GetDatenListe(s) do  // DEAs in Liste eintragen
        try
          for iCount := 0 to Count-1 do begin
            pSl.Add(c + Chr(us) + GetStringPart(Strings[iCount], 1) + Chr(us) +
              GetStringPart(Strings[iCount], 2) + Chr(us) +
              IntToStr(Length(GetStringPart(Strings[iCount], 2))));
          end;
        finally
          Free;
        end;
    end;
  end;

begin
  pSl := TStringList.Create;
  try

    // Telegramm für abzurufende Daten zusammenstellen
    FMomList.Sort;  // Sortieren, damit zusammengehörende DEAs zusammenstehen
    s := '';        // DEAs für ein Telegramm
    j := 0;         // Anzahl der DEAs in dem Telegramm
    for i := 0 to FMomList.Count-1 do begin

      if (not Opened) then Exit;

      if (i = 0) then c := FMomList[i][1];  // 1. Instanzadresse
      if (c = FMomList[i][1])
      then begin
        s := s + FMomList.DeaVon[i] + Chr(gs);
        Inc(j);
      end
      else begin
        InsertToResult;
        if (FMomList.Count <= i) then Exit; // Liste wurde extern gelöscht !
        c := FMomList[i][1];
        s := FMomList.DeaVon[i] + Chr(gs);
        j := 1;
      end;
    end;
    InsertToResult;

    FNewDataList.Assign(pSl);
  finally
    pSl.Free;
  end;
end;

{ Fragt ab, ob neue Werte da sind         }
{ Rückgabe: Neue Werte Ja/Nein            }
{-----------------------------------------}
function TDV24MomConnection.GetNewData: boolean;
{-----------------------------------------}
begin
  Result := FNewData;
  FNewData := False;
end;

{ Ereignis für TempTimer                  }
{-----------------------------------------}
procedure TDV24MomConnection.TempTimerTimer(Sender: TObject);
{-----------------------------------------}
begin
  FTempTimer.Enabled := False;
  if (not IsParametrierung) then begin
    HoleNeueDaten;
    FNewData := True;
  end;
  FTempTimer.Enabled := (Opened);
end;

end.
