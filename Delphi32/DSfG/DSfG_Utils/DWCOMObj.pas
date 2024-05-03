{------------------------------------------------------------------------------}
{ Daten für DSfG über WICOM-Stammdaten und Abrufmodule                         }
{                                                                              }
{ 04.10.2000  GD    Neu                                                        }
{ 19.01.2001  GD    Erweitert um  Veröffentlichung der abgefr. Momentanwerte   }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000                                          }
{------------------------------------------------------------------------------}
unit DWCOMObj;

interface

uses
  Windows, SysUtils, Classes, Forms, Controls, DDbAbruf, PathIni, WSysCon,
  My_Utils, Db_Attn, ExtCtrls, TBDSFGMo, TbDSfGDE, Dialogs, ZustndDb, DbTables,
  WTables;

type
  TDSfGMomDefList = class;
  TDSfGMomValueList = class;

  TNeueMomentanwerteCB = procedure(pSl: TDSfGMomValueList) of object;

  TDSfGMomDefList = class(TStringList)
    destructor Destroy; override;
  private
    function GetInstanzAdresse(iIndex: integer): char;  // 19.01.2001
    function GetDeaVon(iIndex: integer): string;        // 19.01.2001
    function GetDeaBis(iIndex: integer): string;        // 19.01.2001
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure InsertMomDefValue(cIAdr: char; sDEAVon, sDEABis: string);
    property InstanzAdresse [iIndex: integer]: char read GetInstanzAdresse;
    property DeaVon [iIndex: integer]: string read GetDeaVon;
    property DeaBis [iIndex: integer]: string read GetDeaBis;
  end;

  TDSfGMomValueList = class(TStringList)
    destructor Destroy; override;
  private
    function GetInstanzAdresse(iIndex: integer): string;
    function GetDEAdresse(iIndex: integer): string;
    function GetWert(iIndex: integer): string;
    procedure SetWert(iIndex: integer; Value: string);
    function GetStellen(iIndex: integer): SmallInt;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function GetIndex(cAdr: char; sDEA: string): integer;
    property InstanzAdresse[iIndex: integer]: string read GetInstanzAdresse;
    property DEAdresse[iIndex: integer]: string read GetDEAdresse;
    property Wert[iIndex: integer]: string read GetWert write SetWert;
    property Stellen[iIndex: integer]: SmallInt read GetStellen;
  end;

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

  TCustomDSfGWICOMObject = class(TObject)
    constructor Create(aStaId: integer); virtual;
    destructor Destroy; override;
  private
    FStaId  : integer;
  protected
    procedure InitDataConnection(bState: boolean); virtual; abstract;
  public
    function IsWaitingForCall(sAbrufart: string): boolean;
    function IsTeilnehmer: string;
    property StaId: integer read FStaId;
  end;

  TDSfGWICOMMomObject = class(TCustomDSfGWICOMObject)
    constructor Create(aStaId: integer); override;
    destructor Destroy; override;
    procedure TimerTimer(Sender: TObject);
  private
    FTimer  : TTimer;
    FTriggerTimeMOMTb    : integer;
    FMomList             : TDSfGMomDefList;
    FTbDSfGMomentanDef   : TTbDSfGMomentanDef;
    FTbDSfGMomentanDaten : TTbDSfGMomentanDaten;
    FTimerCallBack       : TNeueMomentanwerteCB;
    procedure WriteStationToAbruf(aState: byte);
    function GetMomTable(anId: integer): string;
    function GetMDMTable(anId: integer): string;
    function GetDDETable(anId: integer): string;
    procedure SetAnzeigeDaten;
  protected
    procedure InitDataConnection(bState: boolean); override;
  public
    procedure InitParameter(aState: boolean);
    function WriteParameter(cIAdr: char; sZC1, sZC2, sDEAdr, sValue: string): string;
    procedure InsertMomData(cIAdr: char; sDEAVon, sDEABis: string);
    procedure DeleteMomData(cIAdr: char; sDEAVon, sDEABis: string);
    procedure ClearMomData;
    function GetAnzeigeDaten: TDSfGMomValueList;
    property TriggerTimeMOMTb: integer read FTriggerTimeMOMTb;
    property TimerCallBack: TNeueMomentanwerteCB
      read FTimerCallBack write FTimerCallBack;
  end;

implementation

const
  C_Database_NetWorkDir  = 'WNetWorkDir';
  C_Database_StammdatDir = 'WStammDir';

{-------------------------------- TDSfGMomDefList -----------------------------}

{----------------------------------------------}
destructor TDSfGMomDefList.Destroy;
{----------------------------------------------}
var
  i : integer;
begin
  for i := 0 to Count-1 do Dispose(PDMDInsertRec(Objects[i]));

//  ClearPDMDInsertRecList(Self);

  inherited;
end;

{----------------------------------------------}
procedure TDSfGMomDefList.Clear;
{----------------------------------------------}
var
  i : integer;
begin
  for i := 0 to Count-1 do Dispose(PDMDInsertRec(Objects[i]));

//  ClearPDMDInsertRecList(Self);

  inherited;
end;

{----------------------------------------------}
procedure TDSfGMomDefList.Delete(Index: integer);
{----------------------------------------------}
begin
  if (Index >= 0) and (Index < Count) then Dispose(PDMDInsertRec(Objects[Index]));

  inherited;
end;

{----------------------------------------------}
procedure TDSfGMomDefList.InsertMomDefValue(cIAdr: char; sDEAVon, sDEABis: string);
{----------------------------------------------}
var
  i : integer;
  p : PDMDInsertRec;
begin
  // Prüfen auf evtl. bereits vorhandenen Auftrag
  i := Self.IndexOf(cIAdr+sDEAVon+sDEABis);

  if (i < 0) then begin
    New(p);
    p^.cIAdr := cIAdr;
    p^.sDEAVon := sDEAVon;
    p^.sDEABis := sDEABis;
    Self.AddObject(cIAdr+sDEAVon+sDEABis, TObject(p));
  end;
end;

{----------------------------------------------}
function TDSfGMomDefList.GetInstanzAdresse(iIndex: integer): char;
{----------------------------------------------}
begin
  Result := '0';  // Default

  if ((iIndex < 0) or (iIndex >= Count)) then begin
    MessageDlg('Index (' + IntToStr(iIndex) + ') außerhalb des gültigen ' +
      'Bereichs (' + IntToStr(Count) + ')', mtError, [mbOk], 0);
    Abort;
  end
  else begin
    Result := PDMDInsertRec(Objects[iIndex]).cIAdr[1];
  end;
end;

{----------------------------------------------}
function TDSfGMomDefList.GetDeaVon(iIndex: integer): string;
{----------------------------------------------}
begin
  Result := '0';  // Default

  if ((iIndex < 0) or (iIndex >= Count)) then begin
    MessageDlg('Index (' + IntToStr(iIndex) + ') außerhalb des gültigen ' +
      'Bereichs (' + IntToStr(Count) + ')', mtError, [mbOk], 0);
    Abort;
  end
  else begin
    Result := PDMDInsertRec(Objects[iIndex]).sDEAVon;
  end;
end;

{----------------------------------------------}
function TDSfGMomDefList.GetDeaBis(iIndex: integer): string;
{----------------------------------------------}
begin
  Result := '0';  // Default

  if ((iIndex < 0) or (iIndex >= Count)) then begin
    MessageDlg('Index (' + IntToStr(iIndex) + ') außerhalb des gültigen ' +
      'Bereichs (' + IntToStr(Count) + ')', mtError, [mbOk], 0);
    Abort;
  end
  else begin
    Result := PDMDInsertRec(Objects[iIndex]).sDEABis;
  end;
end;

{------------------------------- TDSfGMomValueList ----------------------------}

{----------------------------------------------}
destructor TDSfGMomValueList.Destroy;
{----------------------------------------------}
var
  i : integer;
begin
  try
    for i := 0 to Count-1 do Dispose(PDMomValueRec(Objects[i]));
  except
  // nix
  end;

//  ClearPDMomValueRecList(Self);

  inherited;
end;

{----------------------------------------------}
procedure TDSfGMomValueList.Delete(Index: integer);
{----------------------------------------------}
begin
  if (Index >= 0) and (Index < Count) then Dispose(PDMomValueRec(Objects[Index]));

  inherited;
end;

{----------------------------------------------}
procedure TDSfGMomValueList.Clear;
{----------------------------------------------}
var
  i : integer;
begin
  try
    for i := 0 to Count-1 do Dispose(PDMomValueRec(Objects[i]));
  except
  // nix
  end;

//  ClearPDMomValueRecList(Self);

  inherited;
end;

{ Gibt den Index eines Listeneintrags zurück   }
{ Parameter: Instanzadresse, DEA               }
{ Rückgabe: Index des Listeneintrags oder -1   }
{----------------------------------------------}
function TDSfGMomValueList.GetIndex(cAdr: char; sDEA: string): integer;
{----------------------------------------------}
begin
  Result := IndexOf(cAdr + sDEA);
end;

{ Gibt die IADr eines Listeneintrags zurück    }
{ Parameter: Index des Listeneintrags          }
{ Rückgabe: IAdr oder ' '                      }
{----------------------------------------------}
function TDSfGMomValueList.GetInstanzAdresse(iIndex: integer): string;
{----------------------------------------------}
begin
  Result := ' ';  // Default;

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PDMomValueRec(Self.Objects[iIndex])^.cIAdr[1];
  except
  // Fehler unterdrücken - Default gibt bereits Fehler bekannt
  end;
end;

{ Gibt die DEA eines Listeneintrags zurück     }
{ Parameter: Index des Listeneintrags          }
{ Rückgabe: DEA oder ''                        }
{----------------------------------------------}
function TDSfGMomValueList.GetDEAdresse(iIndex: integer): string;
{----------------------------------------------}
begin
  Result := '';  // Default;

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PDMomValueRec(Self.Objects[iIndex])^.sDEA;
  except
  // Fehler unterdrücken - Default gibt bereits Fehler bekannt
  end;
end;

{ Gibt den Wert eines Listeneintrags zurück    }
{ Parameter: Index des Listeneintrags          }
{ Rückgabe: DEA oder ''                        }
{----------------------------------------------}
function TDSfGMomValueList.GetWert(iIndex: integer): string;
{----------------------------------------------}
begin
  Result := '';  // Default;

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PDMomValueRec(Self.Objects[iIndex])^.sWert;
  except
  // Fehler unterdrücken - Default gibt bereits Fehler bekannt
  end;
end;

{ Schreibt den Wert eines Listeneintrags       }
{ Parameter: Index des Listeneintrags, Wert    }
{----------------------------------------------}
procedure TDSfGMomValueList.SetWert(iIndex: integer; Value: string);
{----------------------------------------------}
begin
  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
      PDMomValueRec(Self.Objects[iIndex])^.sWert := Value;
  except
  // Fehler unterdrücken - Default gibt bereits Fehler bekannt
  end;
end;

{ Gibt die Stellen eines Listeneintrags zurück }
{ Parameter: Index des Listeneintrags          }
{ Rückgabe: Stellen oder -1                    }
{----------------------------------------------}
function TDSfGMomValueList.GetStellen(iIndex: integer): SmallInt;
{----------------------------------------------}
begin
  Result := -1;  // Default;

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PDMomValueRec(Self.Objects[iIndex])^.iStellen;
  except
  // Fehler unterdrücken - Default gibt bereits Fehler bekannt
  end;
end;

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

{ Gibt den Index eines Listeneintrags zurück   }
{ Parameter: Instanzadresse, DEA               }
{ Rückgabe: Index des Listeneintrags oder -1   }
{----------------------------------------------}
function TDSfGParamValueList.GetIndex(cAdr: char; sDEA: string): integer;
{----------------------------------------------}
begin
  Result := IndexOf(cAdr + sDEA);
end;

{ Gibt die IAdr eines Listeneintrags zurück    }
{ Parameter: Index des Listeneintrags          }
{ Rückgabe: Instanzadresse oder ''             }
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

{ Gibt die DEA eines Listeneintrags zurück     }
{ Parameter: Index des Listeneintrags          }
{ Rückgabe: Datenelementadresse oder ''        }
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

{ Gibt den alten W. eines Listeneintrags zurück}
{ Parameter: Index des Listeneintrags          }
{ Rückgabe: Alter Wert oder ''                 }
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

{ Gibt den neuen W. eines Listeneintrags zurück}
{ Parameter: Index des Listeneintrags          }
{ Rückgabe: Neuer Wert oder ''                 }
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

{----------------------------- TCustomDSfGWICOMObject -------------------------}

{----------------------------------------------}
constructor TCustomDSfGWICOMObject.Create(aStaId: integer);
{----------------------------------------------}
begin
  inherited Create;

  FStaId := aStaId;

  InitDataConnection(True);
end;

{----------------------------------------------}
destructor TCustomDSfGWICOMObject.Destroy;
{----------------------------------------------}
begin
  InitDataConnection(False);

  inherited;
end;

{ Station wartet auf Abruf (AbrfDSfG.DB) ?     }
{ Parameter: Abrufart oder ''                  }
{ Rückgabe: Ja/Nein                            }
{----------------------------------------------}
function TCustomDSfGWICOMObject.IsWaitingForCall(sAbrufart: string): boolean;
{----------------------------------------------}
begin
  with TQuery.Create(nil) do
  try
    DatabaseName := C_Database_StammdatDir;
    Sql.Add('SELECT * FROM ' + CDBDSfGAbruf);
    Sql.Add('WHERE ' + C_DSfGAbruf_StationId + '=' + IntToStr(StaId));
    if (sAbrufart <> '') then
      Sql.Add('AND ' + C_DSfGAbruf_Abrufart + '=''' + sAbrufart + '''');
    Open;
    Result := (not Eof);
    Close;
  finally
    Free;
  end;
end;

{ Station im Abruf (Eintrag in WZustand.DB) ?  }
{ Rückgabe: Aktueller Status oder ''           }
{----------------------------------------------}
function TCustomDSfGWICOMObject.IsTeilnehmer: string;
{----------------------------------------------}
var
  pQuery : TQueryExt;
begin
  Result := '';  // Default

  pQuery := TQueryExt.Create(nil);
  try
    with TZustandDB.Create(PathServer[WStammDir]) do
    try
      GetMRGZustand(pQuery, C_GerArtDSfG, StaId);
    finally
      Free;
    end;

    if (pQuery.Active) and (not pQuery.Eof) then begin
      pQuery.Last;
      Result := pQuery.FieldByName(C_WZustand_Zustand).asString;
      pQuery.Close;
    end;
  finally
    pQuery.Free;
  end;
end;

{----------------------------- TDSfGWICOMMomObject -------------------------}

{----------------------------------------------}
constructor TDSfGWICOMMomObject.Create(aStaId: integer);
{----------------------------------------------}
begin
  inherited Create(aStaId);

  FMomList := TDSfGMomDefList.Create;
  FTriggerTimeMOMTb := GetTriggerTime(GetMomTable(FStaId));

  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := TimerTimer;
  FTimer.Interval := 1000;
  FTimer.Enabled := True;
end;

{----------------------------------------------}
destructor TDSfGWICOMMomObject.Destroy;
{----------------------------------------------}
begin
  FTimer.Enabled := False;
  FTimer.OnTimer := nil;
  FTimer.Free;

  if (Assigned(FMomList)) then FMomList.Free;

  inherited;
end;

{ Öffnet/Schließt Kommunikation                }
{ Parameter: Öffnen (T), Schließen (F)         }
{----------------------------------------------}
procedure TDSfGWICOMMomObject.InitDataConnection(bState: boolean);
{----------------------------------------------}
begin
  if (bState) then begin
    FTbDSfGMomentanDef :=
      TTbDSfGMomentanDef.Create('WNetWorkDir', FStaId);
    FTbDSfGMomentanDaten :=
      TTbDSfGMomentanDaten.Create('WNetWorkDir', FStaId);

    WriteStationToAbruf(1);
  end
  else begin
    WriteStationToAbruf(2);
    FTbDSfGMomentanDef.DeleteTable;
//    DeleteTriggerFile(Self.GetMDMTable(Self.StaId));
    FTbDSfGMomentanDef.Free;
    FTbDSfGMomentanDaten.Free;
  end;
end;

{ Schreibt die zu beobachtende Station in    }
{ die Abrufliste                             }
{ Parameter: 1 - start, 2 - stop, 3 - halten }
{ Parameter: 4 - PStart, 5 - PSend, 6 - PEnde}
{--------------------------------------------}
procedure TDSfGWICOMMomObject.WriteStationToAbruf(aState: byte);
{--------------------------------------------}
var
  i, Id, Lp, Dat, Vers  : integer;
  s, AArt               : string;
  Von, Bis           : TDateTime;
  pSl                : TStringList;
begin
  // Vor Freigeben Verbindung beenden
  if (csDestroying in Application.ComponentState) then begin
    with TDSfGAbruf.Create('WStammDir') do
    try
      InsertToDSfGAbruf(FStaId, 0, C_IsParameter, 0, C_AbrArtMomStop, 0, 0);
    finally
      Free;
    end;
    Exit;
  end;

  { Parameter für Abruf einstellen }
  Id := FStaId;
  Dat := C_IsParameter;
  Vers := 1;
  Von:= 0;
  Bis:= 0;
  Lp := 0;

  pSl := TStringList.Create;
  try
    GetStationsnameListe(pSl, C_AuswahlDSfG);
    s := C_GerArtDSfG + IntToStr(Id);
    i := pSl.IndexOf(s);
    if (i >= 0) then begin
      Lp := TIdRec(pSl.Objects[i]).LogPort;
    end
    else raise Exception.Create('DSfG - ID ''' + IntToStr(FStaId) + ''' nicht gefunden !');
  finally
    pSl.Free;
  end;

  case aState of
    1: AArt:= C_AbrArtMomStart;
    2: AArt:= C_AbrArtMomStop;
    3: AArt:= C_AbrArtMomHalten;
    4: AArt:= C_AbrArtParaStart;
    5: AArt:= C_AbrArtParaSenden;
    6: AArt:= C_AbrArtParaEnde;
    else AArt:= C_AbrArtMomStop;
  end;

  { Abruf in Tabelle eintragen }
  with TDSfGAbruf.Create('WStammDir') do
  try
    InsertToDSfGAbruf(Id, Lp, Dat, Vers, AArt, Von, Bis);
  finally
    Free;
  end;

  { Abrufmodule bei Start oder Stop benachrichtigen }
  if (aState = 1) or (aState = 2) then WriteNewTime(Pathserver.Pathname[WStammDir] + CDBDSfGAbruf);
end;

{--------------------------------------------}
procedure TDSfGWICOMMomObject.TimerTimer(Sender: TObject);
{--------------------------------------------}
const
  C_iZaehler : integer = 0;
  C_iHalten = 30; //Sekunden zwischen 'Halten'-Befehlen
var
  i : integer;
begin
  FTimer.Enabled := False;

  { Evtl. Schreiben eines Haltesatzes für die Verbindung }
  if (C_iZaehler < ((C_iHalten*Integer(FTimer.Interval)) div 1000)) then inc(C_iZaehler)
  else begin
    C_iZaehler := 0;
    WriteStationToAbruf(3);
    SetAnzeigeDaten;
  end;

  { Aktualisierung der Liste über Triggerdatei bei externen Aufrufen }
  i:= GetTriggerTime(GetMomTable(FStaId));
  if (i <> FTriggerTimeMOMTb) then begin
    if (Assigned(FTimerCallBack)) then FTimerCallBack(GetAnzeigeDaten);
    FTriggerTimeMOMTb:= i;
  end;

  FTimer.Enabled := True;
end;

{ Gibt Momentanwert-Tabellennamen zurück     }
{ Parameter: Id der abzurufenden Station     }
{--------------------------------------------}
function TDSfGWICOMMomObject.GetMomTable(anId: integer): string;
{--------------------------------------------}
begin
  Result := Pathserver.Pathname[WNetWorkDir] + C_TbDMom + Format('%4.4d.DB', [anId]);
end;

{ Gibt Momentanwert-Def.-Tab.namen zurück    }
{ Parameter: Id der abzurufenden Station     }
{--------------------------------------------}
function TDSfGWICOMMomObject.GetMDMTable(anId: integer): string;
{--------------------------------------------}
begin
  Result := Pathserver.Pathname[WNetWorkDir] + C_TbDMD + Format('%4.4d.DB', [anId]);
end;

{ Gibt Parameter-Def.-Tabellennamen zurück   }
{ Parameter: Id der abzurufenden Station     }
{--------------------------------------------}
function TDSfGWICOMMomObject.GetDDETable(anId: integer): string;
{--------------------------------------------}
begin
  Result := Pathserver.Pathname[WNetWorkDir] + C_TbDDE + Format('%4.4d.DB', [anId]);
end;

{ Holt die Anzeigedaten und gibt sie aus     }
{--------------------------------------------}
function TDSfGWICOMMomObject.GetAnzeigeDaten: TDSfGMomValueList;
{--------------------------------------------}
begin
  Result := TDSfGMomValueList(FTbDSfGMomentanDaten.GetMomValueList);
end;

{ Schreibt abzurufende Daten in DMD-Tabelle  }
{--------------------------------------------}
procedure TDSfGWICOMMomObject.SetAnzeigeDaten;
{--------------------------------------------}
begin
  // Werte aus FMomList in Abruf-Definitionstabelle eintragen
  FTbDSfGMomentanDef.WriteNewDefTb(FMomList);

  // Abrufmodule bei neuen Daten benachrichtigen
  WriteNewTime(GetMDMTable(FStaId));
end;

{ Schreibt Daten in Abruf-Definitions-Liste  }
{--------------------------------------------}
procedure TDSfGWICOMMomObject.InsertMomData(cIAdr: char; sDEAVon, sDEABis: string);
{--------------------------------------------}
begin
  FMomList.InsertMomDefValue(cIAdr, sDEAVon, sDEABis);
  SetAnzeigeDaten;
end;

{ Löscht Daten aus der  Abrufdefinitionsliste}
{--------------------------------------------}
procedure TDSfGWICOMMomObject.DeleteMomData(cIAdr: char; sDEAVon, sDEABis: string);
{--------------------------------------------}
var
  i : integer;
  c1, c2 : char;
begin
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
  SetAnzeigeDaten;
end;

{ Leert die Abruf-Definitions-Liste          }
{--------------------------------------------}
procedure TDSfGWICOMMomObject.ClearMomData;
{--------------------------------------------}
begin
  FMomList.Clear;
end;

{ Initialisiert/Beendet Parametrierung       }
{ Parameter: Init. (T), Beenden (F)          }
{--------------------------------------------}
procedure TDSfGWICOMMomObject.InitParameter(aState: boolean);
{--------------------------------------------}
const
  oldCallBack : TNeueMomentanwerteCB = nil;
begin
  if (aState) then begin
    oldCallBack := FTimerCallBack;
    FTimerCallBack := nil;
    FTimer.Enabled := False;
    FTimer.OnTimer := nil;

    WriteStationToAbruf(4);     // ParaStart
  end
  else begin
    WriteStationToAbruf(6);   // ParaEnde

    FTimer.OnTimer := TimerTimer;
    FTimer.Enabled := True;
    FTimerCallBack := oldCallBack;
    FTimer.Enabled := True;
  end;
end;

{ Ruft Parameteränderung auf                 }
{ Parameter: Instanzadresse, Zugangscode 1+2 }
{            DEA, neuer Wert                 }
{ Rückgabe: Wert nach Änderung oder ''       }
{--------------------------------------------}
function TDSfGWICOMMomObject.WriteParameter(
  cIAdr: char; sZC1, sZC2, sDEAdr, sValue: string): string;
{--------------------------------------------}
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
begin
  Result := '';  // Default;

  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try

// Datenelement bereits abgerufen ?
    pSlMom := TDSfGMomValueList(FTbDSfGMomentanDaten.GetMomValueList);
    try
      iIndex := pSlMom.GetIndex(cIAdr, sDEAdr);

      if (iIndex < 0) then Exit;

      iStellen := pSlMom.Stellen[iIndex];
    finally
      pSlMom.Free;
    end;

    with TTbDSfGParametrierung.Create(PathServer[WNetWorkDir], StaId) do
    try
      InsertToParaTable(cIAdr, sDEAdr, sValue, '', sZC1, sZC2, iStellen);

      WriteStationToAbruf(5);   // ParaSenden

      lTotzeit := GetTickCount;
      iFileTime := GetTriggerTime(Self.GetDDETable(Self.StaId));

      while (lTotzeit + C_Wartezeit > GetTickCount) and
        (iFileTime = GetTriggerTime(Self.GetDDETable(Self.StaId))) do
      begin
        Application.ProcessMessages;
        Sleep(10);
      end;

      if (iFileTime <> GetTriggerTime(Self.GetDDETable(Self.StaId))) then
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
      DeleteTriggerFile(Self.GetDDETable(Self.StaId));
      DeleteTable;
      Free;
    end;

  finally
    Screen.Cursor := oldCursor;
  end;
end;

end.
