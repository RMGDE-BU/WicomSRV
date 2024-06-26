{------------------------------------------------------------------------------}
{ Objekt f�r DSfG-Abrufe via DF� (WICOM)                                       }
{                                                                              }
{ - DSta.DLL mu� initialisert sein                                             }
{ - PathServer mu� mit WSTammDir und WNetProgDir initialisiert sein            }
{                                                                              }
{ 22.03.2001  GD  Neu                                                          }
{ 24.09.2001  GD  Wartezeit f�r Dateieingabe verl�ngert                        }
{ 29.01.2002  GD  Holen von Automatik-Abrufdaten                               }
{ 10.06.2002  GD  Bugfix                                                       }
{ 17.06.2002  GD  Bugfix DF�-Connection                                        }
{ 25.10.2002  GD  Momentanwertliste beim Beenden leeren                        }
{ 15.03.2004  GD  Erweitert f�r TCP/IP                                         }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, 2004                                    }
{------------------------------------------------------------------------------}
unit DDfueConnect;

interface

uses
  Windows, Classes, Controls, Forms, ExtCtrls, DConnect, GD_Utils, WTables,
  DSG_Utils, T_Zeit, DMomLists, WSysCon, Db_Attn, TBDSFGMo, TbDSfGDE, Dialogs,
  ZustndDb, DDbAbruf, PathIni, My_Utils, SysUtils, FKonfEinlesen, InstTest,
  DStaDLL, SrvCfgIni, KWLink32, AuftrgDb;

type
  TDSfGParamValueList = class(TStringList)
    destructor Destroy; override;
  private
    function GetInstanzAdresse(iIndex: integer): string;
    function GetDEAdresse(iIndex: integer): string;
    function GetWertAlt(iIndex: integer): string;
    function GetWertNeu(iIndex: integer): string;
  public
    procedure Clear; override;
    function GetIndex(cAdr: char; sDEA: string): integer;
    property InstanzAdresse[iIndex: integer]: string read GetInstanzAdresse;
    property DEAdresse[iIndex: integer]: string read GetDEAdresse;
    property WertAlt[iIndex: integer]: string read GetWertAlt;
    property WertNeu[iIndex: integer]: string read GetWertNeu;
  end;

  TDSfGDfueConnection = class(TCustomDSfGConnection)
    constructor Create; override;
  private
    FStationsId : integer;
  protected
    FBusy    : boolean;
    FLogPort : integer;
    procedure InitAbrufmodul(bState: boolean); virtual;
    procedure WriteStationToAbruf(sAbrufArt: string = C_AbrArtMomStop;
      iDaten: integer = C_IsParameter;
      dtVon: TDateTime = 0; dtBis: TDateTime = 0); virtual;
  public
    function StartConnectionTime: integer; override;
    function InitAutoDatenAbruf(iId: integer): boolean;  override;
    function InitManuDatenAbruf(iStaId: integer;
  pAbrufListe: TArchivAbrufList; dtVon, dtBis: TDateTime): boolean; override;
    procedure InitConnection(bState: boolean; iId: integer = -1); override;
    function SendReceiveTelegramm(
      sTelegramm: string; bFolgetelegramme: boolean = True): string; override;
    function ReadParameter(
      cIAdr: char; sDEAVon: string; sDEABis: string = ''): string; override;
    function ReadStammdaten(iState: byte): TStrings; override;
    function IsTeilnehmer: Boolean; override;
    function GetTeilnehmer: string; override;
    property StationsId: integer read FStationsId;
    property Busy: boolean read FBusy;
  end;

  TDDfueMomConnection = class(TDSfGDfueConnection)
  private
    FTriggerTimeMOMTb : integer;
    FDFUFlag          : boolean;
    FMomFlag          : boolean;
    FTbDSfGMomentanDef   : TTbDSfGMomentanDef;
    FTbDSfGMomentanDaten : TTbDSfGMomentanDaten;
    FMomList     : TDSfGMomDefList;
    FMomCallBack : TNeueMomentanwerteCB;
    FTempTimer   : TTimer;
    FNewData     : boolean;
    FStarted     : boolean;
    FParametrierung : boolean;
    StartAdresse : char;       // 10.06.2002
    function GetMomTable(anId: integer): string;
    function GetMDMTable(anId: integer): string;
    function GetDDETable(anId: integer): string;
    procedure SetAnzeigeDaten;
    procedure TempTimerTimer(Sender: TObject);
  protected
    procedure InitComponents(bState: boolean); override;
    function GetNewData: boolean; virtual;
  public
    procedure InitConnection(bState: boolean; iId: integer = -1); override;
    procedure InitMomConnection(bState: boolean; iId: integer = -1);
    procedure InitDFUConnection(bState: boolean; iId: integer = -1); virtual;
    function IsTeilnehmer: Boolean; override;
    function GetTeilnehmer: string; override;
    procedure SendTelegramm(sTelegramm: string); override;
    function ReceiveTelegramm: string; override;
    function SendReceiveTelegramm(
      sTelegramm: string; bFolgetelegramme: boolean = True): string; override;
    procedure InitParameter(bState: boolean); override;
    function ReadParameter(
      cIAdr: char; sDEAVon: string; sDEABis: string = ''): string; override;
    function WriteParameter(
      cIAdr: char; sZC1, sZC2, sDEAdr, sValue: string; iType: byte = 0): string;
    procedure InsertMomData(cIAdr: char; sDEAVon, sDEABis: string);
    procedure DeleteMomData(cIAdr: char; sDEAVon, sDEABis: string);
    function GetAnzeigeDaten: TDSfGMomValueList; virtual;
    procedure CallMomData;
    procedure ClearMomData;
    property NewData: boolean read GetNewData;
    property NeueMomentanwerteCB: TNeueMomentanwerteCB
      read FMomCallBack write FMomCallBack;
    property MomList: TDSfGMomDefList read FMomList;
  end;


implementation

const
  C_Database_NetWorkDir  = 'WNetWorkDir';  // Databases - m�ssen vorhanden sein

  C_StartConnectionTime = 120000;

{------------------------------ TDSfGParamValueList ---------------------------}

{----------------------------------------------}
destructor TDSfGParamValueList.Destroy;
{----------------------------------------------}
var
  i : integer;
begin
  for i := 0 to Count-1 do Dispose(PtrParaErgebnisDataRec(Objects[i]));

  inherited;
end;

{----------------------------------------------}
procedure TDSfGParamValueList.Clear;
{----------------------------------------------}
var
  i : integer;
begin
  for i := 0 to Count-1 do Dispose(PtrParaErgebnisDataRec(Objects[i]));

  inherited;
end;

{ Gibt den Index eines Listeneintrags zur�ck   }
{ Parameter: Instanzadresse, DEA               }
{ R�ckgabe: Index des Listeneintrags oder -1   }
{----------------------------------------------}
function TDSfGParamValueList.GetIndex(cAdr: char; sDEA: string): integer;
{----------------------------------------------}
begin
  Result := IndexOf(cAdr + sDEA);
end;

{ Gibt die IAdr eines Listeneintrags zur�ck    }
{ Parameter: Index des Listeneintrags          }
{ R�ckgabe: Instanzadresse oder ''             }
{----------------------------------------------}
function TDSfGParamValueList.GetInstanzAdresse(iIndex: integer): string;
{----------------------------------------------}
begin
  Result := '';

  try
    Result := PtrParaErgebnisDataRec(Objects[iIndex])^.EADR;
  except
  // Default als Fehlermeldung
  end;
end;

{ Gibt die DEA eines Listeneintrags zur�ck     }
{ Parameter: Index des Listeneintrags          }
{ R�ckgabe: Datenelementadresse oder ''        }
{----------------------------------------------}
function TDSfGParamValueList.GetDEAdresse(iIndex: integer): string;
{----------------------------------------------}
begin
  Result := '';

  try
    Result := PtrParaErgebnisDataRec(Objects[iIndex])^.DEL_Adr;
  except
  // Default als Fehlermeldung
  end;
end;

{ Gibt den alten W. eines Listeneintrags zur�ck}
{ Parameter: Index des Listeneintrags          }
{ R�ckgabe: Alter Wert oder ''                 }
{----------------------------------------------}
function TDSfGParamValueList.GetWertAlt(iIndex: integer): string;
{----------------------------------------------}
begin
  Result := '';

  try
    Result := PtrParaErgebnisDataRec(Objects[iIndex])^.WertNeu_Soll;
  except
  // Default als Fehlermeldung
  end;
end;

{ Gibt den neuen W. eines Listeneintrags zur�ck}
{ Parameter: Index des Listeneintrags          }
{ R�ckgabe: Neuer Wert oder ''                 }
{----------------------------------------------}
function TDSfGParamValueList.GetWertNeu(iIndex: integer): string;
{----------------------------------------------}
begin
  Result := '';

  try
    Result := PtrParaErgebnisDataRec(Objects[iIndex])^.WertNeu_Ist;
  except
  // Default als Fehlermeldung
  end;
end;

{------------------------------- TDSfGDfueConnection --------------------------}

{-----------------------------------------}
constructor TDSfGDfueConnection.Create;
{-----------------------------------------}
begin
  inherited Create;

  FBusy := False;
  FStationsId := -1;     // Default StationsId
  FLogPort := 0;
end;

{ Max. Zeit f�r Verbindungsaufbau         }
{ R�ckgabe: Max. Zeit in Millisec.        }
{-----------------------------------------}
function TDSfGDfueConnection.StartConnectionTime: integer;
{-----------------------------------------}
begin
  Result := C_StartConnectionTime;
end;

{ Baut Verbindung auf                     }
{ Parameter: T-Aufb., F-Abb.; StationsId  }
{-----------------------------------------}
procedure TDSfGDfueConnection.InitConnection(bState: boolean; iId: integer = -1);
{-----------------------------------------}
begin
//  inherited;  - sollte man nicht bei einem 'ABSTRACT'-Vorfahren aufrufen !

  if (bState) then begin
    FStationsId := iId;
    if (AnzahlInstanzen(C_DSfGAbrufModul, '') = 0) then begin
      InitAbrufmodul(True);

      // Pr�ft, ob Tabelle f�r die Momentanwertdarstellung bereits vorhanden ist
      with TTableExt.Create(nil) do
      try
        DatabaseName := PathServer[WNetWorkDir];
        TableName := C_TbDMD + Format('%.4d', [iId]);
        if (Exists) then DeleteTable;
      finally
        Free;
      end;
      DeleteTriggerFile(
        Pathserver.Pathname[WNetWorkDir] + C_TbDMD + Format('%4.4d.DB', [iId]));
    end;

    Opened := (AnzahlInstanzen(C_DSfGAbrufModul, '') > 0);
  end
  else begin
    InitAbrufmodul(False);
    FStationsId := -1;
    Opened := False;
  end;
end;

{ Laden/Feigeben eines DSfG-Abrufmoduls              }
{ Parameter: T=Laden, F=Freigeben                    }
{----------------------------------------------------}
procedure TDSfGDfueConnection.InitAbrufmodul(bState: boolean);  // 29.01.2002
{----------------------------------------------------}
var
  i       : integer;
  stThema : string;
  cmd     : string;
begin
  if (bState) then begin
    FLogPort := 0;

    with TSrvCfg32Ini.Create(PathServer[WNetProgDir]) do
    try
      for i := 1 to MaxCOMName do begin
        stThema := UpperCase(Thema[i]);

        { DSFG: DSfG-Abrufmodul }
        if (stThema = uppercase(Themen[thDSFG])) then begin
          cmd:= IncludeTrailingBackslash(PathServer[WNetProgDir]) +
            'AFDSFG32.EXE ' + ComName[i-1];
          WinExec(PChar(cmd),SW_ShowNormal);
          FLogPort := i;
          Break;
        end;
      end;

      // Pr�fung auf TCP/IP 15.03.2004
      if (FLogPort < 1) and (TCP_IP_DSfG) then begin
        cmd:= IncludeTrailingBackslash(PathServer[WNetProgDir]) +
          'AFDSFG32.EXE ' + sthTCP_IP;
        WinExec(PChar(cmd),SW_ShowNormal);
        FLogPort := CComTCP_IP;
      end;
    finally
      Free;
    end;
  end
  else begin
    // Message �ber Schlie�en an Abrufmodule schicken
    SendToReceiver(KWE_AbrufManager_Beendet, 0, C_DSfGAbrufModul);
    FLogPort := 0;
  end;
end;

{ Schreibt Anruf an Station in Abrufliste    }
{ Parameter: Abrufart als string,            }
{            Datentypen                      }
{--------------------------------------------}
procedure TDSfGDfueConnection.WriteStationToAbruf(
  sAbrufArt: string = C_AbrArtMomStop; iDaten: integer = C_IsParameter;
  dtVon: TDateTime = 0; dtBis: TDateTime = 0);
{--------------------------------------------}
var
  Id, Lp, Dat, Vers  : integer;
  AArt               : string;
  Von, Bis           : TDateTime;
begin
  if (not Opened) then Exit;

  { Parameter f�r Abruf einstellen }
  Id := StationsId;
  Dat := iDaten;
  Vers := 1;
  Von:= dtVon;
  Bis:= dtBis;
  Lp := FLogPort;
  AArt := sAbrufArt;

  { Abruf in Auftrags-Tabelle eintragen }
  if (sAbrufArt = C_AbrArtAuto) or (sAbrufArt = C_AbrArtManu) or
      (sAbrufArt = C_AbrArtMomStart)
  then begin
    ClientStammdaten.InsertWICOMAuftrag(
      C_GerArtDSfG, Id, Now, dat, AArt, Von, Bis);
    with TAuftragDb.Create(PathServer[WStammDir], PathServer[WNetProgDir]) do
    try
      SetBelegt(C_GerArtDSfG, Id, AArt, True);
    finally
      Free;
    end;
  end
  else if (sAbrufArt = C_AbrArtMomStop) then begin
    with TAuftragDb.Create(PathServer[WStammDir], PathServer[WNetProgDir]) do
    try
      DeleteAuftrag (C_GerArtDSfG, Id, C_AbrArtMomStart)
    finally
      Free;
    end;
  end;


  { Abruf in Abruf-Tabelle eintragen }
  with TDSfGAbruf.Create(PathServer[WStammDir]) do
  try
    InsertToDSfGAbruf(Id, Lp, Dat, Vers, AArt, Von, Bis);
  finally
    Free;
  end;

  { Abrufmodule bei Start oder Stop benachrichtigen }
  if (sAbrufArt = C_AbrArtAuto) or (sAbrufArt = C_AbrArtManu) or
      (sAbrufArt = C_AbrArtMomStart) or (sAbrufArt = C_AbrArtMomStop) or
      (sAbrufArt = C_AbrArtMomDfueStart) or (sAbrufArt = C_AbrArtMomDfueStop)
  then SendToReceiver(KWE_DSfGAbrufzeit_erreicht, 0, C_DSfGAbrufModul);
end;

{ Abruf von Auto-Daten ansto�en           }
{ Parameter: InstanzId                    }
{ R�ckgabe: Erfolg ja/nein                }
{-----------------------------------------}
function TDSfGDfueConnection.InitAutoDatenAbruf(iId: integer): boolean;
{-----------------------------------------}
begin
  Result := False;
  if (Opened) then begin
    InitConnection(False);
    Delay(5000);
  end;

  InitConnection(True, iId);
  if (not Opened) then begin
    InitConnection (False);
  end
  else begin
    WriteStationToAbruf(C_AbrArtAuto, (C_IsArchive or C_IsLogbuecher));
    Result := True;
  end;
end;

{ Abruf von Manu-Daten ansto�en           }
{ Parameter: Stations-, Liste mit InstID/ }
{            Archiv-/Kanal-Nr., Zeitraum  }
{ R�ckgabe: Erfolg ja/nein                }
{-----------------------------------------}
function TDSfGDfueConnection.InitManuDatenAbruf(iStaId: integer;
  pAbrufListe: TArchivAbrufList; dtVon, dtBis: TDateTime): boolean;
{-----------------------------------------}
const
  C_Max_Kanal = 21;
var
  i, j, k        : integer;
  iInstId, iAGNr : integer;
  pAList, pKList : TMyTabList;
begin
  Result := False;
  if (Opened) then begin
    InitConnection(False);
    Delay(5000);
  end;

  InitConnection(True, iStaId);
  if (not Opened) then begin
    InitConnection (False);
    Exit;
  end;

  with TADManu.Create(PathServer[WStammDir]) do
  try
    // Scheife �ber alle Instanzen
    for i := 0 to pAbrufListe.Count-1 do begin
      iInstId := StrToInt(pAbrufListe[i]);
      pAList := pAbrufListe.GetInstanzList(iInstId);

      // Scheife �ber alle Archivgruppen
      for j := 0 to pAList.Count-1 do begin
        iAGNr := StrToInt(pAList[j]);
        pKList := pAbrufListe.GetArchivKanaele(iInstId, iAGNr);

        // Scheife �ber alle Kan�le
        for k := 0 to pKList.Count-1 do
          if (StrToInt(pKList[k]) > 0)
          then Set_AppendArchivKanal(
            iStaId, iInstId, iAGNr, StrToInt(pKList[k]), 0, 0)
          else Set_AppendLogbuch(iStaId, iInstId, iAGNr, 0, 0);

      end; // Scheife �ber Archivgruppen

    end;   // Schleife �ber Instanzen
  finally
    Free;
  end;

  WriteStationToAbruf(
    C_AbrArtManu, (C_IsArchive or C_IsLogbuecher), dtVon, dtBis);
  Result := True;

end;

{ Tele. senden + empfangen - nicht m�glich}
{-----------------------------------------}
function TDSfGDfueConnection.SendReceiveTelegramm(
  sTelegramm: string; bFolgetelegramme: boolean = True): string;
{-----------------------------------------}
begin
  Result := '';
end;

{ Parameter lesen - noch nicht m�glich    }
{-----------------------------------------}
function TDSfGDfueConnection.ReadParameter(
  cIAdr: char; sDEAVon: string; sDEABis: string = ''): string;
{-----------------------------------------}
begin
  Result := '';
end;

{ Stammdaten einlesen                     }
{ Parameter: bei DF� nicht verwendet      }
{ Stringliste: leere Liste oder nil       }
{-----------------------------------------}
function TDSfGDfueConnection.ReadStammdaten(iState: byte): TStrings;
{-----------------------------------------}
begin
  Result := nil;  // Default

  try
    with TFormDSfGKonfigurationLesenDFU.Create(nil) do
    try
      if (ShowModal = mrOk)
      then begin
        ClientStammdaten.AktualisiereStammdaten;
        Self.FStationsId := ClientStammdaten.DStationsId(cbStationsname.Text);
      end
      else Self.FStationsId := -1;
      Result := TStringList.Create; // bleibt leer, da Stammdaten bereits gespeichert sind
      Result.Add('');  // Damit man etwas zum Empfangen hat
    finally
      Free;
    end;
  except
  // Result ist nil
  end;
end;

{ Aktive Komunikation mit Bus hergestellt?}
{ R�ckgabe: Ja/Nein                       }
{-----------------------------------------}
function TDSfGDfueConnection.IsTeilnehmer: Boolean;
{-----------------------------------------}
var
  pQuery : TQueryExt;
begin
  Result := False;  // Default

  pQuery := TQueryExt.Create(nil);
  try
    with TZustandDB.Create(PathServer[WStammDir]) do
    try
      GetMRGZustand(pQuery, C_GerArtDSfG, StationsId);
    finally
      Free;
    end;

    if (pQuery.Active) then begin
      while (not pQuery.Eof) do begin
        if (pQuery.FieldByName(C_WZustand_Zustand).asString = 'Verbindung steht')
        then begin
          Result := True;
          Break;
        end;
        pQuery.Next;
      end;
    end;

    if (pQuery.Active) then pQuery.Close;
  finally
    pQuery.Free;
  end;
end;

{ Aktive DSfG-Busteilnehmer als String    }
{ R�ckgabe: DSfG-Busteilnehmer            }
{-----------------------------------------}
function TDSfGDfueConnection.GetTeilnehmer: string;
{-----------------------------------------}
begin
  Result := '';
end;

{-------------------------------- TDDfueMomConnection -------------------------}

{ Komponenten zur Laufzeit erstellen/init.}
{ Parameter: T-Initialisieren, F-Freigeben}
{-----------------------------------------}
procedure TDDfueMomConnection.InitComponents(bState: boolean);
{-----------------------------------------}
const
  C_InitPathServer : boolean = False;
begin
  inherited;

  if (bState) then begin
    C_InitPathServer := not Assigned(PathServer);
    if (C_InitPathServer) then
    with TProgramIni.Create do
    try
      PathServer := TPathServer.Create(
        WieserIniFile, [WNetWorkDir, WNetProgDir, WStammDir]);
    finally
      Free;
    end;

    FMomList := TDSfGMomDefList.Create;
    FTriggerTimeMOMTb := GetTriggerTime(GetMomTable(StationsId));

    FParametrierung := False; // Flag, dass aktuell keine Parametrierung durchgef�hrt wird

    FTempTimer := TTimer.Create(nil);
    FTempTimer.Enabled := False;
    FTempTimer.OnTimer := TempTimerTimer;
    FTempTimer.Interval := 1000;

    FStarted := False;  // Flag, das es sich um keine Startdaten handelt
    FNewData := False;  // Keine neuen Daten
    StartAdresse := '0';
  end
  else begin
    FTempTimer.Enabled := False;
    FTempTimer.OnTimer := nil;
    FTempTimer.Free;

    if (Assigned(FMomList)) then FMomList.Free;

    if (C_InitPathServer) then PathServer.Free;

    FStarted := False;  // Flag, das es sich um keine Startdaten handelt
    FDFUFlag := False;  // Flag: Keine DF�
    FMomFlag := False;  // Flag: Keine Momentanwertdarstellung
  end;
end;

{ Ereignis f�r internen Timer             }
{-----------------------------------------}
procedure TDDfueMomConnection.TempTimerTimer(Sender: TObject);
{-----------------------------------------}
const
  C_iHalten = 30000; // ms zwischen 'Halten'-Befehlen
  C_iStop : Cardinal = 0;
var
  i : integer;
begin
  if (not Opened) then Exit;

  FTempTimer.Enabled := False;

  { Evtl. Schreiben eines Haltesatzes f�r die Verbindung }
  if (C_iStop = 0) then C_iStop := GetTickCount + C_iHalten;
  if (C_iStop <= GetTickCount) then begin
    if (FDFUFlag)
    then WriteStationToAbruf(C_AbrArtMomDfueHalten)
    else WriteStationToAbruf(C_AbrArtMomHalten);
    C_iStop := GetTickCount + C_iHalten;
  end;

  if (not FParametrierung) then begin
    { Aktualisierung der Liste �ber Triggerdatei bei externen Aufrufen }
    i:= GetTriggerTime(GetMomTable(StationsId));
    if (i <> FTriggerTimeMOMTb) then begin
      if (FStarted) then begin  // handelt sich um Startdaten
        if (IsTeilnehmer) then begin
          DeleteMomData(StartAdresse, C_DEA_InstTyp, '');  // 10.06.2002
          DeleteMomData(Adresse, C_DEA_InstTyp, '');
          FStarted := False;  // Flag, da� es sich um keine Startdaten handelt
        end;
      end
      else begin
        if (Assigned(FMomCallBack)) then FMomCallBack(GetAnzeigeDaten);
        FNewData := True;
      end;
      FTriggerTimeMOMTb:= i;
    end;
  end;

  FTempTimer.Enabled := True;
end;

{ Baut Verbindung f�r                     }
{ Parameter: T-Aufb., F-Abb.; StationsId  }
{-----------------------------------------}
procedure TDDfueMomConnection.InitConnection(
  bState: boolean; iId: integer = -1);
{-----------------------------------------}
begin
  if (bState) then begin
    inherited;
  end
  else begin
    FStarted := False;  // Flag, das es sich um keine Startdaten handelt

    if (FDFUFlag) then InitDFUConnection(False)
    else if (FMomFlag) then InitMomConnection(False);

    inherited;
  end;
end;

{ Baut Verbindung f�r Momentanwert-       }
{ darstelllung auf                        }
{ Parameter: T-Aufb., F-Abb.; StationsId  }
{-----------------------------------------}
procedure TDDfueMomConnection.InitMomConnection(
  bState: boolean; iId: integer = -1);
{-----------------------------------------}
var
  i : integer;
begin
  if (bState) then begin
    FMomList.Clear;  // 25.10.2002

    InitConnection(bState, iId);

    // Pr�ft, ob Tabelle f�r die Momentanwertdarstellung bereits vorhanden ist
    with TTableExt.Create(nil) do
    try
      DatabaseName := C_Database_NetWorkDir;
      TableName := C_TbDMD + Format('%.4d', [iId]);
      Opened := not Exists;
    finally
      Free;
    end;

    if (iId < 0) then Exit
    else if (Opened) then begin
      FTbDSfGMomentanDef :=
        TTbDSfGMomentanDef.Create(C_Database_NetWorkDir, StationsId);
      FTbDSfGMomentanDaten :=
        TTbDSfGMomentanDaten.Create(C_Database_NetWorkDir, StationsId);

      WriteStationToAbruf(C_AbrArtMomStart);

      // Antwort provozieren
      with ClientStammdaten.FillDInstanzListe(  // 10.06.2002
        ClientStammdaten.DStationsName(StationsId)) do
      begin
        for i := 0 to Count-1 do
          if (Instanztyp[i] in [C_D_Instanztyp_Gas, C_D_Instanztyp_Reg,
            C_D_Instanztyp_Umw, C_D_Instanztyp_Wieser]) then
          begin
            StartAdresse := InstanzAdresse[i];
            Break;
          end;
      end;
      InsertMomData(StartAdresse, C_DEA_InstTyp, '');
      FStarted := True;   // Flag, das es sich um Startdaten handelt

      FDFUFlag := False;  // Flag: Momentanwertdarstellung
      FMomFlag := True;   // Flag: Momentanwertdarstellung
      FTempTimer.Enabled := True;  // Timer starten
    end;
  end
  else begin
    FTempTimer.Enabled := False;  // Timer starten
    FMomList.Clear;  // 25.10.2002

    if (StationsId > 0) then WriteStationToAbruf(C_AbrArtMomStop);
    if (Assigned(FTbDSfGMomentanDef)) then begin
      FTbDSfGMomentanDef.DeleteTable;
      FTbDSfGMomentanDef.Free;
      FTbDSfGMomentanDef := nil;
    end;
    if (Assigned(FTbDSfGMomentanDaten)) then begin
      FTbDSfGMomentanDaten.Free;
      FTbDSfGMomentanDaten := nil;
    end;
    DeleteTriggerFile(Self.GetMDMTable(Self.StationsId));

    FMomFlag := False;  // Flag: Momentanwertdarstellung

    InitConnection(False);
  end;

end;

{ Baut Verbindung auf                     }
{ Parameter: T-Aufb., F-Abb.; StationsId  }
{-----------------------------------------}
procedure TDDfueMomConnection.InitDFUConnection(
  bState: boolean; iId: integer = -1);
{-----------------------------------------}
begin
  if (bState) then begin
    if (iId >= 0) then begin
      FDFUFlag := False;   // Flag: DF�-Verbindung
      FMomFlag := False;   // Flag: Momentanwertdarstellung
      InitConnection(bState, iId);
      if (iId >= 0) and (Opened) then begin
        WriteStationToAbruf(C_AbrArtMomDfueStart);
        FDFUFlag := True;    // Flag: DF�-Verbindung
        FTempTimer.Enabled := True;
      end;
    end;
  end
  else begin
    FTempTimer.Enabled := False;
    if (StationsId > 0) then WriteStationToAbruf(C_AbrArtMomDfueStop);
    FDFUFlag := False;     // Flag: DF�-Verbindung
    FMomFlag := False;     // Flag: Momentanwertdarstellung
    InitConnection(False, iId);
  end;
end;

{ Initialisiert Parametrierungs-Modus     }
{ Parameter: T-Initialisieren, F-Beenden  }
{-----------------------------------------}
procedure TDDfueMomConnection.InitParameter(bState: boolean);
{-----------------------------------------}
const
  oldCallBack : TNeueMomentanwerteCB = nil;
begin
  if (not Opened) then Exit;

  FParametrierung := bState;
  if (bState) then begin
//    oldCallBack := FMomCallBack;
//    FMomCallBack := nil;
//    FTempTimer.Enabled := False;
//    FTempTimer.OnTimer := nil;

    WriteStationToAbruf(C_AbrArtParaStart);     // ParaStart
  end
  else begin
    WriteStationToAbruf(C_AbrArtParaEnde);   // ParaEnde

//    FTempTimer.OnTimer := TempTimerTimer;
//    FMomCallBack := oldCallBack;
//    FTempTimer.Enabled := True;
  end;
end;

{ Liest Parameter aus                     }
{ Parameter: Instanzadr, Dea's von-bis    }
{ R�ckgabe: Antworttelegramm              }
{-----------------------------------------}
function TDDfueMomConnection.ReadParameter(
  cIAdr: char; sDEAVon: string; sDEABis: string = ''): string;
{-----------------------------------------}
{
var
  s : string;
begin
  if (Opened) and (cIAdr in ['A'..'_']) and (sDEAVon <> '') then begin
  // Telegramm zusammenstellen
    if (sDEABis = '')
    then s := GetOneDEDSfGTelegramm(Adresse, cIAdr, sDEAVon)
    else s := GetBereichsabfrageDSfGTelegramm(Adresse, cIAdr, sDEAVon, sDEABis);

    Result := SendReceiveTelegramm(s);
  end
  else Result := '';
}

const
  C_Stop : Cardinal = 120000;
var
  pSl    : TDSfGMomDefList;
  s      : string;
  i, iTrigg      : integer;
  iStop  : Cardinal;
  oldCallBack : TNeueMomentanwerteCB;

  function CompleteTelegramm(sTelePart: string; iCount: integer): string;
  begin
    Result := Chr(stx) + Self.Adresse + Chr(us) + '255' + Chr(us) + '1' +
      Chr(us) + '1' + Chr(us) + '1' + Chr(us) + cIAdr + Chr(us) + 'R' +
      Chr(us) + 'N' + Chr(us) + 'V' + Chr(us) + IntToStr(iCount) + Chr(us) +
      Copy(sTelePart, 2, Length(sTelePart)-1) + Chr(fs) + Chr(etx);
  end;

begin
  Result := '';
  if (not Opened) or (StationsId < 0) or (not(cIAdr in ['A'..'_'])) then Exit;

  // Kurzer Weg
  if (sDEABis = '') then begin
    s := GetOneDEDSfGTelegramm(Adresse, cIAdr, sDEAVon);
    Result := SendReceiveTelegramm(s);
    Exit;
  end;

  if (FDFUFlag)
  then WriteStationToAbruf(C_AbrArtMomDfueHalten)
  else WriteStationToAbruf(C_AbrArtMomHalten);

  // Hier geht's ab ...
  oldCallBack := FMomCallBack;
  FMomCallBack := nil;
//  FTempTimer.Enabled := False;
//  FTempTimer.OnTimer := nil;
  FParametrierung := True;
  try

    pSl := TDSfGMomDefList.Create;
    try
      // Leere Liste in Abruf-Definitionstabelle eintragen
      FTbDSfGMomentanDef.WriteNewDefTb(pSl);
      WriteNewTime(GetMDMTable(StationsId));
      Delay(10000);  // Zeit lassen ...

      // Parameterabruf erstellen
      pSl.InsertMomDefValue(cIAdr, sDEAVon, sDEABis);
      FTbDSfGMomentanDef.WriteNewDefTb(pSl);
      WriteNewTime(GetMDMTable(StationsId));
      FTriggerTimeMOMTb := GetTriggerTime(GetMomTable(StationsId));
    finally
      pSl.Free;
    end;

    try

      // Warten auf eine Antwort (max. 120 Sekunden)
      iStop := GetTickCount + C_Stop;
      while (GetTickCount < iStop) do begin
        Delay(100);
        // Selbst�ndig Haltesatz schreiben
        WriteStationToAbruf(C_AbrArtMomHalten);

        // Aktualisierung der Liste �ber Triggerdatei bei externen Aufrufen
        iTrigg := GetTriggerTime(GetMomTable(StationsId));
        if (iTrigg <> FTriggerTimeMOMTb) then begin
          FTriggerTimeMOMTb:= iTrigg;

          with GetAnzeigeDaten do
          try
            if (Count > 0) {and (Wert[0] <> '')} then begin

              for i := 0 to Count-1 do
                s := s + Chr(gs) + DEAdresse[i] + Chr(us) + Wert[i];
              Result := CompleteTelegramm(s, Count);
              Break;
            end;
          finally
            Free;
          end;
        end;
      end;

    finally
      // Werte aus FMomList in Abruf-Definitionstabelle eintragen
      FTbDSfGMomentanDef.WriteNewDefTb(FMomList);
      // Abrufmodule bei neuen Daten benachrichtigen
      WriteNewTime(GetMDMTable(StationsId));
    end;

  finally
    FParametrierung := False;
//    FTempTimer.OnTimer := TempTimerTimer;
//    FTempTimer.Enabled := True;
    FMomCallBack := oldCallBack;
  end;

  if (FDFUFlag)
  then WriteStationToAbruf(C_AbrArtMomDfueHalten)
  else WriteStationToAbruf(C_AbrArtMomHalten);

end;

{ Schreibt einen Parameter                }
{ Parameter: Instanzadresse, Zugangscodes }
{            DEA, neuer Wert              }
{ R�ckgabe: Wert nach �nderung oder ''    }
{-----------------------------------------}
function TDDfueMomConnection.WriteParameter(
  cIAdr: char; sZC1, sZC2, sDEAdr, sValue: string; iType: byte = 0): string;
{-----------------------------------------}
const
  C_Wartezeit = 10000;
var
  oldCursor : TCursor;
  pSlMom    : TDSfGMomValueList;
  pSlPar    : TDSfGParamValueList;
  iIndex    : integer;
  iStellen  : integer;
  iFileTime : integer;
  lTotzeit  : LongWord;
  sTelegram : string;
begin
  Result := '';  // Default;
  if (not Opened) then Exit;

  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    iStellen := Length(sValue);

    // Datenelement abrufen, Stellenzahl ermitteln
    if (iType = StrToIntDef(C_DEA_Typ_Zeichenkette, 0)) then begin

      // Datenelement bereits abgerufen ?
      pSlMom := TDSfGMomValueList(FTbDSfGMomentanDaten.GetMomValueList);
      try
        iIndex := pSlMom.GetIndex(cIAdr, sDEAdr);

        if (iIndex >= 0) then iStellen := pSlMom.Stellen[iIndex]
        else begin

          // Stellenanzahl durch Parameterabruf feststellen
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

        end;
      finally
        pSlMom.Free;
      end;

    end;

    with TTbDSfGParametrierung.Create(C_Database_NetWorkDir, StationsId) do
    try
      InsertToParaTable(cIAdr, sDEAdr, sValue, '', sZC1, sZC2, iStellen);

      WriteStationToAbruf(C_AbrArtParaSenden);   // ParaSenden

      lTotzeit := GetTickCount;
      iFileTime := GetTriggerTime(Self.GetDDETable(Self.StationsId));

      while (lTotzeit + C_Wartezeit > GetTickCount) and
        (iFileTime = GetTriggerTime(Self.GetDDETable(Self.StationsId))) do
      begin
        Application.ProcessMessages;
        Sleep(10);
      end;

      if (iFileTime <> GetTriggerTime(Self.GetDDETable(Self.StationsId))) then
      begin
         pSlPar := TDSfGParamValueList(GetParamValueList);
         try
           if (pSlPar.GetIndex(cIAdr, sDEAdr) >= 0) then
             Result := pSlPar.WertNeu[pSlPar.GetIndex(cIAdr, sDEAdr)];
         finally
           pSlPar.Free;
         end;
      end;

    finally
      DeleteTriggerFile(Self.GetDDETable(Self.StationsId));
      DeleteTable;
      Free;
    end;

  finally
    Screen.Cursor := oldCursor;
  end;
end;

{ Gibt Momentanwert-Tabellennamen zur�ck     }
{ Parameter: Id der abzurufenden Station     }
{--------------------------------------------}
function TDDfueMomConnection.GetMomTable(anId: integer): string;
{--------------------------------------------}
begin
  Result := Pathserver.Pathname[WNetWorkDir] + C_TbDMom + Format('%4.4d.DB', [anId]);
end;

{ Gibt Momentanwert-Def.-Tab.namen zur�ck    }
{ Parameter: Id der abzurufenden Station     }
{--------------------------------------------}
function TDDfueMomConnection.GetMDMTable(anId: integer): string;
{--------------------------------------------}
begin
  Result := Pathserver.Pathname[WNetWorkDir] + C_TbDMD + Format('%4.4d.DB', [anId]);
end;

{ Gibt Parameter-Def.-Tabellennamen zur�ck   }
{ Parameter: Id der abzurufenden Station     }
{--------------------------------------------}
function TDDfueMomConnection.GetDDETable(anId: integer): string;
{--------------------------------------------}
begin
  Result := Pathserver.Pathname[WNetWorkDir] + C_TbDDE + Format('%4.4d.DB', [anId]);
end;

{ Schreibt abzurufende Daten in DMD-Tabelle  }
{--------------------------------------------}
procedure TDDfueMomConnection.SetAnzeigeDaten;
{--------------------------------------------}
begin
  if (not Opened) then Exit;

  // Werte aus FMomList in Abruf-Definitionstabelle eintragen
  FTbDSfGMomentanDef.WriteNewDefTb(FMomList);

  // Abrufmodule bei neuen Daten benachrichtigen
  WriteNewTime(GetMDMTable(StationsId));
end;

{ Schreibt Daten in Abruf-Definitions-Liste  }
{--------------------------------------------}
procedure TDDfueMomConnection.InsertMomData(cIAdr: char; sDEAVon, sDEABis: string);
{--------------------------------------------}
begin
  if (not Opened) then Exit;

  FMomList.InsertMomDefValue(cIAdr, sDEAVon, sDEABis);
  SetAnzeigeDaten;
end;

{ L�scht Daten aus der  Abrufdefinitionsliste}
{--------------------------------------------}
procedure TDDfueMomConnection.DeleteMomData(cIAdr: char; sDEAVon, sDEABis: string);
{--------------------------------------------}
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
  // DE-Bereich l�schen
    for i := FMomList.Count-1 downto 0 do begin
      if (PDMDInsertRec(FMomList.Objects[i])^.cIAdr = cIAdr) and
         (PDMDInsertRec(FMomList.Objects[i])^.sDEAVon[1] in [c1..c2]) then
      begin
        FMomList.Delete(i);
      end;
    end;
  end;
  SetAnzeigeDaten;
end;

{ Leert die Abruf-Definitions-Liste          }
{--------------------------------------------}
procedure TDDfueMomConnection.ClearMomData;
{--------------------------------------------}
begin
  FMomList.Clear;
  SetAnzeigeDaten;
end;

{ Holt die Anzeigedaten und gibt sie aus     }
{--------------------------------------------}
function TDDfueMomConnection.GetAnzeigeDaten: TDSfGMomValueList;
{--------------------------------------------}
begin
  if (not Opened) then begin
    Result := TDSfGMomValueList.Create;
    Exit;
  end;

  Result := TDSfGMomValueList(FTbDSfGMomentanDaten.GetMomValueList);
end;

{ Fragt die definierten Momentanwerte ab  }
{-----------------------------------------}
procedure TDDfueMomConnection.CallMomData;
{-----------------------------------------}
begin
  if (not Opened) then Exit;

  if (Assigned(FMomCallBack)) then FMomCallBack(GetAnzeigeDaten);
end;

{ Fragt ab, ob neue Werte da sind         }
{ R�ckgabe: Neue Werte Ja/Nein            }
{-----------------------------------------}
function TDDfueMomConnection.GetNewData: boolean;
{-----------------------------------------}
begin
  Result := FNewData;
  FNewData := False;
end;

{ Aktive Komunikation mit Bus hergestellt?}
{ R�ckgabe: Ja/Nein                       }
{-----------------------------------------}
function TDDfueMomConnection.IsTeilnehmer: Boolean;
{-----------------------------------------}
begin
  // Pr�ft, ob Tabelle f�r die Momentanwertdarstellung vorhanden ist
  with TTableExt.Create(nil) do
  try
    DatabaseName := C_Database_NetWorkDir;
    TableName := C_TbDMom + Format('%.4d', [StationsId]);
    Result := Exists;
  finally
    Free;
  end;
end;

{ Aktive DSfG-Busteilnehmer als String    }
{ R�ckgabe: DSfG-Busteilnehmer            }
{--------------------------------------------}
function TDDfueMomConnection.GetTeilnehmer: string;
{--------------------------------------------}
const
  C_Stop : Cardinal = 30000;
var
  pSl    : TDSfGMomDefList;
  s      : string;
  i      : integer;
  iStop  : Cardinal;
//  oldCallBack : TNeueMomentanwerteCB;
begin
  for i := Ord('A') to Ord('_') do Result := Result + '.';  // Default

  if (not Opened) or (StationsId < 0) then Exit;

  s := '';  // Teilnehmer aus den Stammdaten
  with ClientStammdaten.FillDInstanzListe(
    ClientStammdaten.DStationsName(StationsId)) do
  begin
    for i := 0 to Count-1 do s := s + InstanzAdresse[i];
  end;

//  oldCallBack := FMomCallBack;
//  FMomCallBack := nil;
//  FTempTimer.Enabled := False;
//  FTempTimer.OnTimer := nil;
  FParametrierung := True;
  try

    FTriggerTimeMOMTb := GetTriggerTime(GetMomTable(StationsId));

    pSl := TDSfGMomDefList.Create;
    try
      // Liste mit Typ-Anforderung aller Instanzen erstellen
      for i := 1 to Length(s) do pSl.InsertMomDefValue(s[i], C_DEA_InstTyp, '');
      // Werte aus Liste in Abruf-Definitionstabelle eintragen
      FTbDSfGMomentanDef.WriteNewDefTb(pSl);
      // Abrufmodule bei neuen Daten benachrichtigen
      WriteNewTime(GetMDMTable(StationsId));
    finally
      pSl.Free;
    end;

    try

      // Warten auf eine Antwort (max. 30 Sekunden)
      iStop := GetTickCount + C_Stop;
      while (GetTickCount < iStop) do begin
        Delay(100);
        // Aktualisierung der Liste �ber Triggerdatei bei externen Aufrufen
        i:= GetTriggerTime(GetMomTable(StationsId));
        if (i <> FTriggerTimeMOMTb) then begin
          FTriggerTimeMOMTb:= i;
          with GetAnzeigeDaten do
          try
            if (Count > 0) then begin
              for i := 0 to Count-1 do begin
                if (Wert[i] <> '') and (InstanzAdresse[i] <> '') then begin
                  Result[Ord(InstanzAdresse[i][1]) - Ord('A') + 1] :=
                    InstanzAdresse[i][1];
                end;
              end;
              Break;
            end;
          finally
            Free;
          end;
        end;
      end;

    finally
      // Werte aus FMomList in Abruf-Definitionstabelle eintragen
      FTbDSfGMomentanDef.WriteNewDefTb(FMomList);
      // Abrufmodule bei neuen Daten benachrichtigen
      WriteNewTime(GetMDMTable(StationsId));
    end;

  finally
//    FTempTimer.OnTimer := TempTimerTimer;
//    FMomCallBack := oldCallBack;
//    FTempTimer.Enabled := True;
    FParametrierung := False;
  end;
end;

{ Telegramm senden                        }
{ Parameter: DSfG-Telegramm               }
{-----------------------------------------}
procedure TDDfueMomConnection.SendTelegramm(sTelegramm: string);
{-----------------------------------------}
var
  s : string;
  i : integer;
begin
  if (not Opened) or (StationsId < 0) then Exit;

  if (Pos(Chr(stx), sTelegramm) > 0) and (Pos(Chr(etx), sTelegramm) > 0) then
  begin
    // Byteweise Kopie (sonst Zeigerproblem ...)
    s := '';
    for i := 2 to Length(sTelegramm) - 1 do s := s + sTelegramm[i];

    // �bergabe-Schreib-Datei schreiben
    with TFileStream.Create(PathServer[WNetWorkDir] + C_Tb_DBinaerBefehl +
      Format('%.4d', [StationsId]) + '.DAT', fmCreate) do
    try
      WriteBuffer(Pointer(s)^, Length(s));
    finally
      Free;
    end;

    // Ggf. �bergabe-Lese-Datei l�schen
    DeleteFile(PathServer[WNetWorkDir] + C_Tb_DBinaerAntwort +
      Format('%.4d', [StationsId]) + '.DAT');

    // Abrufmodul benachrichtigen
    WriteStationToAbruf(C_AbrArtBinaerSenden);
  end;
end;

{ Telegramm empfangen                     }
{ R�ckgabe: Telegramm oder ''             }
{-----------------------------------------}
function TDDfueMomConnection.ReceiveTelegramm: string;
{-----------------------------------------}
var
  iTimeOut  : Cardinal;
  sFileName : TFileName;
begin
  if (not Opened) or (StationsId < 0) then Exit;

  // Antwort-Datei
  sFileName := PathServer[WNetWorkDir] + C_Tb_DBinaerAntwort +
    Format('%.4d', [StationsId]) + '.DAT';

  iTimeOut := GetTickCount + Cardinal(TimeOut);

  while (not FileExists(sFileName)) and (GetTickCount < iTimeOut) do Sleep(10);

  if (FileExists(sFileName)) then begin
    Delay(100);  // 24.09.2001
    with TFileStream.Create(sFileName, fmOpenRead) do
    try
      SetLength(Result, Size);
      ReadBuffer(Pointer(Result)^, Size);
    finally
      Free;
    end;
  end
  else Result := '';

  // Ggf. �bergabe-Dateien l�schen
  DeleteFile(PathServer[WNetWorkDir] + C_Tb_DBinaerBefehl +
    Format('%.4d', [StationsId]) + '.DAT');
  DeleteFile(sFileName);
end;

{ Sendet ein DSfG-Telegramm und holt die  }
{ Antwort                                 }
{ Parameter: DSfG-Telegramm;              }
{            Flag f�r Folgetelegramme     }
{ R�ckgabe: DSfG-Telegramm oder ''        }
{-----------------------------------------}
function TDDfueMomConnection.SendReceiveTelegramm(
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

      // Pr�fung auf Folgetelegramme (bei 'U' und DEB = 'V', 'O', 'Z')
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

end.
