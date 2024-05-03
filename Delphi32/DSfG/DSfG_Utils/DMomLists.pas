{------------------------------------------------------------------------------}
{ Listen-Objekt für DSfG-Momentanwertdarstellung                               }
{                                                                              }
{ 07.03.2001  GD  Neu                                                          }
{ 09.04.2001  GD  mit DListen abgeglichen                                      }
{ 06.08.2001  GD  Debug-Flag                                                   }
{ 16.11.2001  GD  Stand                                                        }
{ 14.06.2002  GD  Änderungen in DelList-Liste                                  }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, 2002                                    }
{------------------------------------------------------------------------------}
unit DMomLists;

interface

uses
  SysUtils, Classes, WSysCon, Dialogs, GD_Utils, DListen;

type
  TDSfGMomDefList = class;
  TDSfGMomValueList = class;
  TDeaDefList = class;
  TOneDeaDefList = class;
  TStaDefDeaList = class;

  TProofModify = function: boolean of object;
  TNeueMomentanwerteCB = procedure(pSl: TDSfGMomValueList) of object;

  TDeaValueType = (dvtAlle, dvtNone, dvtLesen, dvtCode, dvtSchreiben, dvtEich,
    dvtBenSchloss);
  TDeaValueTypes = set of TDeaValueType;

  Str40 = string[40];
  PStr40 = ^Str40;

  PStaRecord = ^TStaRecord; // Record für Stationsbaum
  TStaRecord = record
    GTyp     : char;
    GName    : string[40];
    InstId   : integer;
    MyId     : integer;
  end;

  TDDEARec = record
    DEA         : string[5];
    Bezeichnung : string[100];
    Zugriff     : string[5];
    DEATyp      : string[1];
    GTypNr      : SmallInt;
  end;
  PDDEARec    = ^TDDEARec;

  TDSfGDEAList = class(TStringList)
    constructor Create;
    destructor Destroy; override;
  private
  protected
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure AddDEA(sDEA, sBez, sZugriff, sDEATyp: string; iGTyp: SmallInt = 7;
      bDbl: boolean = True);
    function GetBezeichnung(
      sDEA: string; iGTyp: SmallInt = C_GTypNr_Normal): string;
    function GetRecord(
      sDEA: string; iGTyp: SmallInt = C_GTypNr_Normal): PDDEARec;
  end;

  TDSfGStationList = class(TStringList)
    constructor Create(sName: string; iId: integer);
    destructor Destroy; override;
  private
    FName : string;
    FId   : integer;
    function GetId(iIndex: integer): integer;
    function GetAdresse(iIndex: integer): char;
    function GetTyp(iIndex: integer): char;
    function GetHersteller(iIndex: integer): string;
    function GetGeraetetyp(iIndex: integer): string;
    function GetFabriknummer(iIndex: integer): string;
    function GetName(iIndex: integer): string;
    function GetStand(iIndex: integer): integer; // 16.11.2001
  protected
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure AddInstanz(iIId: integer; cIAdr, cITyp: char;
      sHersteller, sGTyp, sFabrNr, sName: string; iStand: integer);
    function GetTeilnehmer: string;
    property StationsName: string read FName write FName;
    property StationsId: integer read FId write FId;
    property InstanzId[iIndex: integer]: integer read GetId;
    property InstanzAdresse[iIndex: integer]: char read GetAdresse;
    property InstanzTyp[iIndex: integer]: char read GetTyp;
    property InstanzHersteller[iIndex: integer]: string read GetHersteller;
    property InstanzGeraetetyp[iIndex: integer]: string read GetGeraetetyp;
    property InstanzFabriknummer[iIndex: integer]: string read GetFabriknummer;
    property InstanzName[iIndex: integer]: string read GetName;
    property InstanzStand[iIndex: integer]: integer read GetStand;
  end;

  TDSfGStationenList = class(TStringList)
    constructor Create;
    destructor Destroy; override;
  private
    function GetInstanzIndex(sStationsName: string; cInstAdr: char): PInstanzData;
  protected
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure AddStation(sName: string; iId: integer);
    procedure AddInstanz(sStationsName: string;
      iIId: integer; cIAdr, cITyp: char;
      sHersteller, sGTyp, sFabrNr, sName: string; iStand: integer);
    function GetStationsId(sStationsName: string): integer;
    function GetInstanzId(sStationsName: string; cInstAdr: char): integer;
    function GetInstanzTyp(sStationsName: string; cInstAdr: char): char;
    function GetInstanzHersteller(sStationsName: string; cInstAdr: char): string;
    function GetInstanzGeraetetyp(sStationsName: string; cInstAdr: char): string;
    function GetInstanzFabriknummer(sStationsName: string; cInstAdr: char): string;
  end;

  TDMDInsertRec = record
    cIAdr    : string[1];
    sDEAVon  : string[10];
    sDEABis  : string[10];
  end;
  PDMDInsertRec = ^TDMDInsertRec;

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

  TDeaDefList = class(TStringList)
  private
    function GetBezeichnung(iIndex: integer): string;
    function GetGruppenCount(iIndex: integer): integer;
  protected
  public
    procedure NeuerEintrag(pOneDeaList: TOneDeaDefList);
    function GetDea(iIndex, iLfdNr: integer): string;
    property GruppenCount [iIndex: integer]: integer read GetGruppenCount;
    property Bezeichnung [iIndex: integer]: string read GetBezeichnung;
  end;

  TOneDeaDefList = class(TStringList)
  private
    FBezeichnung : string;
  protected
  public
    property Bezeichnung: string read FBezeichnung write FBezeichnung;
  end;

  TStaDefDeaList = class(TStringList)
    destructor Destroy; override;
  private
    function GetDeaDefList(iIndex: integer): TDeaDefList;
  public
    property DeaDefList[iIndex: integer]: TDeaDefList read GetDeaDefList;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure AddDeaDefList(pList: TDeaDefList; sBezeichnung: string);
  end;

implementation

{--------------------------- Allgemeine Funktionen ----------------------------}

{ Leert Liste mit Integer-Pointern     }
{ Parameter: zu leerende Liste         }
{--------------------------------------}
procedure ClearIntegerList(pSl: TStrings);
{--------------------------------------}
var
  i : integer;
begin
  for i := 0 to pSl.Count-1 do Dispose(PInteger(pSl.Objects[i]));
  pSl.Clear;

end;

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
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGMomValueList.Destroy';
var
  i : integer;
begin
  try
    for i := 0 to Count-1 do Dispose(PDMomValueRec(Objects[i]));
  except
  // nix
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
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
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGMomValueList.Clear';
var
  i : integer;
begin
  try
    for i := 0 to Count-1 do Dispose(PDMomValueRec(Objects[i]));
  except
  // nix
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
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
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGMomValueList.GetInstanzAdresse';
begin
  Result := ' ';  // Default;

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PDMomValueRec(Self.Objects[iIndex])^.cIAdr[1];
  except
  // Fehler unterdrücken - Default gibt bereits Fehler bekannt
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
  end;
end;

{ Gibt die DEA eines Listeneintrags zurück     }
{ Parameter: Index des Listeneintrags          }
{ Rückgabe: DEA oder ''                        }
{----------------------------------------------}
function TDSfGMomValueList.GetDEAdresse(iIndex: integer): string;
{----------------------------------------------}
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGMomValueList.GetDEAdresse';
begin
  Result := '';  // Default;

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PDMomValueRec(Self.Objects[iIndex])^.sDEA;
  except
  // Fehler unterdrücken - Default gibt bereits Fehler bekannt
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
  end;
end;

{ Gibt den Wert eines Listeneintrags zurück    }
{ Parameter: Index des Listeneintrags          }
{ Rückgabe: DEA oder ''                        }
{----------------------------------------------}
function TDSfGMomValueList.GetWert(iIndex: integer): string;
{----------------------------------------------}
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGMomValueList.GetWert';
begin
  Result := '';  // Default;

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PDMomValueRec(Self.Objects[iIndex])^.sWert;
  except
  // Fehler unterdrücken - Default gibt bereits Fehler bekannt
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
  end;
end;

{ Schreibt den Wert eines Listeneintrags       }
{ Parameter: Index des Listeneintrags, Wert    }
{----------------------------------------------}
procedure TDSfGMomValueList.SetWert(iIndex: integer; Value: string);
{----------------------------------------------}
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGMomValueList.SetWert';
begin
  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
      PDMomValueRec(Self.Objects[iIndex])^.sWert := Value;
  except
  // Fehler unterdrücken - Default gibt bereits Fehler bekannt
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
  end;
end;

{ Gibt die Stellen eines Listeneintrags zurück }
{ Parameter: Index des Listeneintrags          }
{ Rückgabe: Stellen oder -1                    }
{----------------------------------------------}
function TDSfGMomValueList.GetStellen(iIndex: integer): SmallInt;
{----------------------------------------------}
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGMomValueList.GetStellen';
begin
  Result := -1;  // Default;

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PDMomValueRec(Self.Objects[iIndex])^.iStellen;
  except
  // Fehler unterdrücken - Default gibt bereits Fehler bekannt
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
  end;
end;

{-------------------------------- TDSfGDEAList --------------------------------}

{--------------------------------------}
constructor TDSfGDEAList.Create;
{--------------------------------------}
begin
  inherited Create;

  Sorted := True;
end;

{--------------------------------------}
destructor TDSfGDEAList.Destroy;
{--------------------------------------}
var
  i : integer;
begin
  for i := 0 to Self.Count-1 do Dispose(PDDEARec(Self.Objects[i]));

  inherited Destroy;
end;

{ Überschreibt geerbte Methode 'Clear' }
{--------------------------------------}
procedure TDSfGDEAList.Clear;
{--------------------------------------}
var
  i : integer;
begin
  for i := 0 to Self.Count-1 do Dispose(PDDEARec(Self.Objects[i]));

  inherited Clear;
end;

{ Überschreibt geerbte Methode 'Delete'}
{--------------------------------------}
procedure TDSfGDEAList.Delete(Index: Integer);
{--------------------------------------}
begin
  if (Index >= 0) and (Index < Self.Count) then begin
    Dispose(PDDEARec(Self.Objects[Index]));

    inherited Delete(Index);
  end;
end;

{ Fügt Datenelement zu Liste hinzu     }
{ Parameter: DEA, Bezeichnung, Zugriff,}
{            Typ, Gerätetyp (Def=0),   }
{            doppelte Einträge (Def=Ja)}
{--------------------------------------}
procedure TDSfGDEAList.AddDEA(sDEA, sBez, sZugriff, sDEATyp: string;
  iGTyp: SmallInt = 7; bDbl: boolean = True);
{--------------------------------------}
var
  s : string;
  p : PDDEARec;
begin
  // Einzutragende Werte zusammenstellen
  s := sDEA + IntToStr(iGTyp);  // 'Index'-String besteht aus DEA und GTyp
  New(p);
  p^.DEA := sDEA;
  p^.Bezeichnung := sBez;
  p^.Zugriff := sZugriff;
  p^.DEATyp := sDEATyp;
  p^.GTypNr := iGTyp;

  // Evtl. bereits vorhandenen Eintrag löschen
  if (not bDbl) and (Self.IndexOf(s) >= 0) then Self.Delete(Self.IndexOf(s));

  // Neuen Eintrag einfügen
  Self.AddObject(s, TObject(p));
end;

{ Gibt Datenelement-Bezeichnung zurück }
{ Parameter: DEA, Gerätetyp (Def=0)    }
{ Rückgabe: Bezeichnung oder ''        }
{--------------------------------------}
function TDSfGDEAList.GetBezeichnung(
  sDEA: string; iGTyp: SmallInt = C_GTypNr_Normal): string;
{--------------------------------------}
var
  p : PDDEARec;
begin
  p := GetRecord(sDEA, iGTyp);
  if (Assigned(p)) then Result := p^.Bezeichnung else Result := '';
end;

{ Gibt Datenelement-Record zurück      }
{ Parameter: DEA, Gerätetyp (Def=0)    }
{ Rückgabe: Record-Pointer oder nil    }
{--------------------------------------}
function TDSfGDEAList.GetRecord(sDEA: string;
  iGTyp: SmallInt = C_GTypNr_Normal): PDDEARec;
{--------------------------------------}
var
  i : integer;
  s : string;
begin
  Result := nil;  // Default

  // Test mit Gerätetyp
  s := sDEA + IntToStr(iGTyp);
  i := Self.IndexOf(s);

  // Bei keinem Eintrag Test mit Gerätetyp '0'
  if (i < 0) then begin
    s := sDEA + IntToStr(C_GTypNr_Normal);
    i := Self.IndexOf(s);
  end;

  // Bei keinem Eintrag Test mit Gerätetyp 'MRG2200'
  if (i < 0) then begin
    s := sDEA + IntToStr(C_GTypNr_MRG2200);
    i := Self.IndexOf(s);
  end;

  // Bei keinem Eintrag Test mit Gerätetyp 'MRG2100D'
  if (i < 0) then begin
    s := sDEA + IntToStr(C_GTypNr_MRG2100D);
    i := Self.IndexOf(s);
  end;

  if (i >= 0) then Result := PDDEARec(Self.Objects[i]);
end;

{------------------------------ TDSfGStationList ------------------------------}

{--------------------------------------}
constructor TDSfGStationList.Create(sName: string; iId: integer);
{--------------------------------------}
begin
  inherited Create;

  FName := sName;
  FId := iId;
end;

{--------------------------------------}
destructor TDSfGStationList.Destroy;
{--------------------------------------}
var
  i : integer;
begin
  for i := 0 to Self.Count-1 do Dispose(PInstanzData(Self.Objects[i]));

  inherited Destroy;
end;

{ Überschreibt geerbte Methode 'Clear' }
{--------------------------------------}
procedure TDSfGStationList.Clear;
{--------------------------------------}
var
  i : integer;
begin
  for i := 0 to Self.Count-1 do Dispose(PInstanzData(Self.Objects[i]));

  inherited Clear;
end;

{ Überschreibt geerbte Methode 'Delete'}
{--------------------------------------}
procedure TDSfGStationList.Delete(Index: Integer);
{--------------------------------------}
begin
  if (Index >= 0) and (Index < Self.Count) then begin
    Dispose(PInstanzData(Self.Objects[Index]));

    inherited Delete(Index);
  end;
end;

{ Gibt Busteilnehmer der Station zurück}
{ Rückgabe: Teilnehmer in DSfG-Schreib.}
{--------------------------------------}
function TDSfGStationList.GetTeilnehmer: string;
{--------------------------------------}
var
  c : char;
begin
  for c := 'A' to '_' do
    if (Self.IndexOf(c) >= 0) then Result := Result + c else Result := Result + '.';
end;

{ Fügt Instanz zur Liste hinzu         }
{ Parameter: InstanzId, InstanzAdr,    }
{            InstanzTyp, Hersteller,   }
{            Gerätetyp, Stand          }
{--------------------------------------}
procedure TDSfGStationList.AddInstanz(iIId: integer; cIAdr, cITyp: char;
  sHersteller, sGTyp, sFabrNr, sName: string; iStand: integer);
{--------------------------------------}
var
  i : integer;
  p : PInstanzData;
begin
  // Prüfen, ob Instanz bereits vorhanden ist
  i := Self.IndexOf(cIAdr);

  if (i < 0) then New(p) else p := PInstanzData(Self.Objects[i]);

  p^.InstanzId := iIId;
  p^.Busadresse := cIAdr;
  p^.Instanztyp := cITyp;
  p^.Hersteller := sHersteller;
  p^.Geraetetyp := sGTyp;
  p^.FabrikNr := sFabrNr;
  p^.Instanzname := sName;
  p^.Stand := iStand;  // 16.11.2001

  if (i < 0) then Self.AddObject(cIAdr, TObject(p));
  CustomSort(StringListStrCompare);
end;

{ Gibt IId eines Eintrags zurück       }
{ Parameter: Index des Eintrags        }
{ Rückgabe: IId oder -1                }
{--------------------------------------}
function TDSfGStationList.GetId(iIndex: integer): integer;
{--------------------------------------}
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGStationList.GetId';
begin
  Result := -1; // Default

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PInstanzData(Self.Objects[iIndex])^.InstanzId;
  except
  // Fehlermeldung unterdrücken - wird durch Default mitgeteilt
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
  end;
end;

{ Gibt IAdr eines Eintrags zurück      }
{ Parameter: Index des Eintrags        }
{ Rückgabe: IAdr oder ' '              }
{--------------------------------------}
function TDSfGStationList.GetAdresse(iIndex: integer): char;
{--------------------------------------}
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGStationList.GetAdresse';
begin
  Result := ' ';  // Default

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PInstanzData(Self.Objects[iIndex])^.Busadresse[1];
  except
  // Fehlermeldung unterdrücken - wird durch Default mitgeteilt
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
  end;
end;

{ Gibt ITyp eines Eintrags zurück      }
{ Parameter: Index des Eintrags        }
{ Rückgabe: ITyp oder ' '              }
{--------------------------------------}
function TDSfGStationList.GetTyp(iIndex: integer): char;
{--------------------------------------}
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGStationList.GetTyp';
begin
  Result := 'X';  // Default

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PInstanzData(Self.Objects[iIndex])^.InstanzTyp[1];
    if (not(Result in ['0', 'A'..'_'])) then Result := 'X';
  except
  // Fehlermeldung unterdrücken - wird durch Default mitgeteilt
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
  end;
end;

{ Gibt Hersteller eines Eintrags zurück}
{ Parameter: Index des Eintrags        }
{ Rückgabe: Hersteller oder ''         }
{--------------------------------------}
function TDSfGStationList.GetHersteller(iIndex: integer): string;
{--------------------------------------}
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGStationList.GetHersteller';
begin
  Result := '';  // Default

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PInstanzData(Self.Objects[iIndex])^.Hersteller;
  except
  // Fehlermeldung unterdrücken - wird durch Default mitgeteilt
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
  end;
end;

{ Gibt GId eines Eintrags zurück       }
{ Parameter: Index des Eintrags        }
{ Rückgabe: GId oder ''                }
{--------------------------------------}
function TDSfGStationList.GetGeraetetyp(iIndex: integer): string;
{--------------------------------------}
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGStationList.GetGeraetetyp';
begin
  Result := '';  // Default

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PInstanzData(Self.Objects[iIndex])^.Geraetetyp;
  except
  // Fehlermeldung unterdrücken - wird durch Default mitgeteilt
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
  end;
end;

{ Gibt Fabriknummer eines Eintrags zur.}
{ Parameter: Index des Eintrags        }
{ Rückgabe: Fabriknummer oder ''       }
{--------------------------------------}
function TDSfGStationList.GetFabriknummer(iIndex: integer): string;
{--------------------------------------}
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGStationList.GetFabriknummer';
begin
  Result := '';  // Default

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PInstanzData(Self.Objects[iIndex])^.FabrikNr;
  except
  // Fehlermeldung unterdrücken - wird durch Default mitgeteilt
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
  end;
end;

{ Gibt Instanzname eines Eintrags zur. }
{ Parameter: Index des Eintrags        }
{ Rückgabe: Instanzname oder ''        }
{--------------------------------------}
function TDSfGStationList.GetName(iIndex: integer): string;
{--------------------------------------}
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGStationList.GetName';
begin
  Result := '';  // Default

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PInstanzData(Self.Objects[iIndex])^.Instanzname;
  except
  // Fehlermeldung unterdrücken - wird durch Default mitgeteilt
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
  end;
end;

{ Gibt Instanzname eines Eintrags zur. }
{ Parameter: Index des Eintrags        }
{ Rückgabe: Fabriknummer oder ''       }
{--------------------------------------}
function TDSfGStationList.GetStand(iIndex: integer): integer;
{--------------------------------------}
resourcestring
  C_ErrorMessage = 'DMomLists-TDSfGStationList.GetStand';
begin
  Result := 2;  // Default

  if (iIndex >= 0) and (iIndex < Self.Count) then
  try
    Result := PInstanzData(Self.Objects[iIndex])^.Stand;
  except
  // Fehlermeldung unterdrücken - wird durch Default mitgeteilt
    on E: Exception do begin    // 06.08.2001
      if (IsDebugFlag) then
        StringToFile(FormatDateTime(C_FormatDateTime, Now) + ': ' +
          C_ErrorMessage + ' --> ' +  E.Message,
          ChangeFileExt(ParamStr(0), '.err'), False);
    end;
  end;
end;

{------------------------------ TDSfGStationenList ------------------------------}

{--------------------------------------}
constructor TDSfGStationenList.Create;
{--------------------------------------}
begin
  inherited Create;

end;

{--------------------------------------}
destructor TDSfGStationenList.Destroy;
{--------------------------------------}
var
  i : integer;
begin
  for i := 0 to Self.Count-1 do TDSfGStationList(Self.Objects[i]).Free;

  inherited Destroy;
end;

{ Überschreibt geerbte Methode 'Clear' }
{--------------------------------------}
procedure TDSfGStationenList.Clear;
{--------------------------------------}
var
  i : integer;
begin
  for i := 0 to Self.Count-1 do TDSfGStationList(Self.Objects[i]).Free;

  inherited Clear;
end;

{ Überschreibt geerbte Methode 'Delete'}
{--------------------------------------}
procedure TDSfGStationenList.Delete(Index: Integer);
{--------------------------------------}
begin
  if (Index >= 0) and (Index < Self.Count) then begin
    TDSfGStationList(Self.Objects[Index]).Free;

    inherited Delete(Index);
  end;
end;

{ Fügt Station zur Liste hinzu         }
{ Parameter: Stationsname, StationsId  }
{--------------------------------------}
procedure TDSfGStationenList.AddStation(sName: string; iId: integer);
{--------------------------------------}
var
  p : TDSfGStationList;
  i : integer;
begin
  // Gibt es die Station bereits ? - ACHTUNG: es wird nur der Name geprüft
  i := Self.IndexOf(sName);
  if (i >= 0) then Self.Delete(i);

  p := TDSfGStationList.Create(sName, iId);
  Self.AddObject(sName, p);
end;

{ Fügt Station zur Liste hinzu         }
{ Parameter: Stationsname, StationsId  }
{--------------------------------------}
procedure TDSfGStationenList.AddInstanz(sStationsName: string;
  iIId: integer; cIAdr, cITyp: char;
  sHersteller, sGTyp, sFabrNr, sName: string; iStand: integer);
{--------------------------------------}
var
  p : TDSfGStationList;
  i : integer;
begin
  i := Self.IndexOf(sStationsName);
  if (i >= 0) then begin
    p := TDSfGStationList(Self.Objects[i]);
    p.AddInstanz(iIId, cIAdr, cITyp, sHersteller, sGTyp, sFabrNr, sName, iStand);
  end;
end;

{ Gibt Zeiger auf eine Instanz zurück  }
{ Rückgabe: Pecord-Pointer oder nil    }
{--------------------------------------}
function TDSfGStationenList.GetInstanzIndex(
  sStationsName: string; cInstAdr: char): PInstanzData;
{--------------------------------------}
var
  i : integer;
  p : TDSfGStationList;
begin
  Result := nil;  // Default

  i := Self.IndexOf(sStationsName);
  if (i >= 0) then p := TDSfGStationList(Self.Objects[i]) else Exit;
  if (p.IndexOf(cInstAdr) >= 0) then 
    Result := PInstanzData(TStrings(p.Objects[p.IndexOf(cInstAdr)]));
end;

{ Gibt Id einer DSfG-Station zurück    }
{ Parameter: Stationsname              }
{ Rückgabe: Id oder '-1'               }
{--------------------------------------}
function TDSfGStationenList.GetStationsId(sStationsName: string): integer;
{--------------------------------------}
var
  i : integer;
begin
  Result := -1;

  i := Self.IndexOf(sStationsName);
  if (i >= 0) then
    Result := TDSfGStationList(Self.Objects[i]).StationsId;
end;

{ Gibt Id einer DSfG-Instanz zurück    }
{ Parameter: Stationsname, InstAdresse }
{ Rückgabe: Id oder '-1'               }
{--------------------------------------}
function TDSfGStationenList.GetInstanzId(sStationsName: string; cInstAdr: char): integer;
{--------------------------------------}
var
  p : PInstanzData;
begin
  Result := -1;

  p := Self.GetInstanzIndex(sStationsName, cInstAdr);
  if (Assigned(p)) then Result := p^.InstanzId;
end;

{ Gibt Id einer DSfG-Station zurück    }
{ Parameter: Stationsname, InstAdresse }
{ Rückgabe: Id oder ' '                }
{--------------------------------------}
function TDSfGStationenList.GetInstanzTyp(sStationsName: string; cInstAdr: char): char;
{--------------------------------------}
var
  p : PInstanzData;
begin
  Result := ' ';

  p := Self.GetInstanzIndex(sStationsName, cInstAdr);
  if (Assigned(p)) then Result := p^.InstanzTyp[1];
end;

{ Gibt Id einer DSfG-Station zurück    }
{ Parameter: Stationsname, InstAdresse }
{ Rückgabe: Id oder ''                 }
{--------------------------------------}
function TDSfGStationenList.GetInstanzHersteller(sStationsName: string; cInstAdr: char): string;
{--------------------------------------}
var
  p : PInstanzData;
begin
  Result := '';

  p := Self.GetInstanzIndex(sStationsName, cInstAdr);
  if (Assigned(p)) then Result := p^.Hersteller;
end;

{ Gibt Id einer DSfG-Station zurück    }
{ Parameter: Stationsname, InstAdresse }
{ Rückgabe: Id oder ''                 }
{--------------------------------------}
function TDSfGStationenList.GetInstanzGeraetetyp(sStationsName: string; cInstAdr: char): string;
{--------------------------------------}
var
  p : PInstanzData;
begin
  Result := '';

  p := Self.GetInstanzIndex(sStationsName, cInstAdr);
  if (Assigned(p)) then Result := p^.Geraetetyp;
end;

{ Gibt FNr einer DSfG-Station zurück   }
{ Parameter: Stationsname, InstAdresse }
{ Rückgabe: Fabriknummer oder ''       }
{--------------------------------------}
function TDSfGStationenList.GetInstanzFabriknummer(sStationsName: string; cInstAdr: char): string;
{--------------------------------------}
var
  p : PInstanzData;
begin
  Result := '';

  p := Self.GetInstanzIndex(sStationsName, cInstAdr);
  if (Assigned(p)) then Result := p^.FabrikNr;
end;

{----------------------------------- TDeaDefList ------------------------------}

{ Bezeichnung einer Dea-Gruppe                 }
{ Parameter: Index                             }
{ Rückgabe: Bezeichnung                        }
{----------------------------------------------}
function TDeaDefList.GetBezeichnung(iIndex: integer): string;
{----------------------------------------------}
begin
  if (iIndex >= 0) and (iIndex < Count) then begin
    Result := GetStringPart(Strings[iIndex], 1)
  end
  else begin
    raise Exception.Create(
      'Index (' + IntToStr(iIndex) + ') überschreitet die Anzahl ' +
      'der Listenelemente (' + IntToStr(Count) + ')');
  end;
end;

{ Anzahl der Datenelement in einer Gruppe      }
{ Parameter: Index                             }
{ Rückgabe: Anzahl                             }
{----------------------------------------------}
function TDeaDefList.GetGruppenCount(iIndex: integer): integer;
{----------------------------------------------}
var
  i : integer;
begin
  Result := 0;
  if (iIndex >= 0) and (iIndex < Count) then begin
    i := 2;
    while (GetStringPart(Strings[iIndex], i) <> '') do begin
      Inc(Result);
      Inc(i);
    end;
  end
  else begin
    raise Exception.Create(
      'Index (' + IntToStr(iIndex) + ') überschreitet die Anzahl ' +
      'der Listenelemente (' + IntToStr(Count) + ')');
  end;
end;

{ Ein Datenelement in einer Gruppe             }
{ Parameter: Index, LfdNr innerhalb der Gruppe }
{ Rückgabe: DEA                                }
{----------------------------------------------}
function TDeaDefList.GetDea(iIndex, iLfdNr: integer): string;
{----------------------------------------------}
begin
  Result := '';
  if (iIndex >= 0) and (iIndex < Count) then begin
    Result := GetStringPart(Strings[iIndex], iLfdNr+2);
  end
  else begin
    raise Exception.Create(
      'Index (' + IntToStr(iIndex) + ') überschreitet die Anzahl ' +
      'der Listenelemente (' + IntToStr(Count) + ')');
    Abort;
  end;
end;

{ Bezeichnung einer Dea-Gruppe                 }
{ Parameter: Liste mit Werten eines Eintrags   }
{----------------------------------------------}
procedure TDeaDefList.NeuerEintrag(pOneDeaList: TOneDeaDefList);
{----------------------------------------------}
var
  i : integer;
  s : string;
begin
  if (Assigned(pOneDeaList)) then begin
    s := pOneDeaList.Bezeichnung;
    for i := 0 to pOneDeaList.Count-1 do s := s + Chr(us) + pOneDeaList[i];
    if (Length(s) > 0) then Self.Add(s);
  end;
end;

{----------------------------------- TStaDefDeaList ------------------------------}

{----------------------------------------------}
destructor TStaDefDeaList.Destroy;
{----------------------------------------------}
var
  i : integer;
begin
  for i := 0 to Count-1 do TDeaDefList(Objects[i]).Free;

  inherited;
end;

{----------------------------------------------}
procedure TStaDefDeaList.Clear;
{----------------------------------------------}
var
  i : integer;
begin
  for i := 0 to Count-1 do TDeaDefList(Objects[i]).Free;

  inherited;
end;

{----------------------------------------------}
procedure TStaDefDeaList.Delete(Index: integer);
{----------------------------------------------}
begin
  if (Index >= 0) and (Index < Count) then TDeaDefList(Objects[Index]).Free;

  inherited;
end;

{ Fügt eine Stations-Definition hinzu          }
{ Parameter: Übergabeliste, Def.-Bezeichnung   }
{----------------------------------------------}
function TStaDefDeaList.GetDeaDefList(iIndex: integer): TDeaDefList;
{----------------------------------------------}
begin
  if (iIndex >= 0) and (iIndex < Count) then begin
    Result := TDeaDefList(Objects[iIndex]);
  end
  else begin
    raise Exception.Create(
      'Index (' + IntToStr(iIndex) + ') überschreitet die Anzahl ' +
      'der Listenelemente (' + IntToStr(Count) + ')');
  end;
end;

{ Fügt eine Stations-Definition hinzu          }
{ Parameter: Übergabeliste, Def.-Bezeichnung   }
{----------------------------------------------}
procedure TStaDefDeaList.AddDeaDefList(pList: TDeaDefList; sBezeichnung: string);
{----------------------------------------------}
var
  p : TDeaDefList;
begin
  p := TDeaDefList.Create;
  p.Assign(pList);

  Self.AddObject(sBezeichnung, p);
end;

end.
