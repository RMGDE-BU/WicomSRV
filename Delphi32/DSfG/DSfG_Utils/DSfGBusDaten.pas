{------------------------------------------------------------------------------}
{ Objekt für DSfG-Busdaten einer DSfG-Station                                  }
{                                                                              }
{ 26.07.2000  GD    Neu                                                        }
{ 12.01.2001  GD    Kleine Wartungsarbeiten                                    }
{ 17.08.2001  GD    Kleine Wartungsarbeiten                                    }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, 2001                                    }
{------------------------------------------------------------------------------}
unit DSfGBusDaten;

interface

uses
  Classes, SysUtils,
  DSG_Utils, GD_Utils, WSysCon;

type
  TDSfGStation = class;
  TDSfGInstanz = class;

  TDSfGInstanz = class(TObject)
    constructor Create(cInstAdr: char);
    destructor Destroy; override;
  private
    FInstanzId : integer;
    FInstAdr   : char;
    FInstTyp   : char;
    FGerTypNr  : SmallInt;
    FDEList    : TStrings;
    procedure SetDEList(Value: TStrings);
    procedure GetInstAdrFromDEList;
    procedure GetInstTypFromDEList;
    procedure GetGerTypNrFromDEList;
  public
    procedure AddDatenelement(sDEA, sWert: string);
    function HasDatenelement(sDEA: string): boolean;
    function GetWert(sDEA: string): string;
    procedure AddDatenListe(pSl: TStrings);
    property InstanzId: integer read FInstanzId write FInstanzId;
    property InstAdr: char read FInstAdr write FInstAdr;
    property InstTyp: char read FInstTyp write FInstTyp;
    property GerTypNr: SmallInt read FGerTypNr write FGerTypNr;
    property DatenListe: TStrings read FDEList write SetDEList;
  end;

  TDSfGStation = class(TObject)
    constructor Create; virtual;
    destructor Destroy; override;
  private
    FStationsId : integer;
    FStatName   : string;
    FInstList   : TStrings;
  protected
  public
    function GetInstanz(cInstAdr: char): TDSfGInstanz;
    function GetTeilnehmer: string;
    function GetTeilnehmerCount: integer;
    procedure AddInstanz(cInstAdr: char; pDEList: TStrings); virtual;
    procedure DeleteInstanz(cInstAdr: char); virtual;
    function HasInstanz(cInstAdr: char): boolean;
    function GetInstanzDECount(cInstAdr: char): integer;
    function HasDatenelement(cInstAdr: char; sDEA: string): boolean;
    function GetDatenelement(cInstAdr: char; sDEA: string): string;
    function GetInstTyp(cInstAdr: char): char;
    function GetGerTypNr(cInstAdr: char): SmallInt;
    property StationsId: integer read FStationsId write FStationsId;
    property StatName: string read FStatName write FStatName;
  end;

implementation

type
  TStr40 = string[40];
  PStr40 = ^TStr40;

{--------------------------- Allgemeine Funktionen ----------------------------}

{ Leert eine Liste mit TDSfGInstanz-Objekten         }
{ Parameter: Liste, die geleert werden soll          }
{----------------------------------------------------}
procedure ClearInstanzList(pSl: TStrings);
{----------------------------------------------------}
var
  i : integer;
begin
  if (Assigned(pSl)) then begin
    try
      for i := 0 to pSl.Count-1 do TDSfGInstanz(pSl.Objects[i]).Free;
    finally
      pSl.Clear;
    end;
  end;
end;

{ Leert eine Liste mit PStr40-Objekten               }
{ Parameter: Liste, die geleert werden soll          }
{----------------------------------------------------}
procedure ClearDEList(pSl: TStrings);
{----------------------------------------------------}
var
  i : integer;
begin
  if (Assigned(pSl)) then begin
    try
      for i := 0 to pSl.Count-1 do Dispose(PStr40(pSl.Objects[i]));
    finally
      pSl.Clear;
    end;
  end;
end;

{--------------------------------- TDSfGInstanz -------------------------------}

{----------------------------------------------------}
constructor TDSfGInstanz.Create(cInstAdr: char);
{----------------------------------------------------}
begin
  inherited Create;
  FInstanzId := 0;
  FInstAdr := cInstAdr;
  FInstTyp := C_D_Instanztyp_unbest;
  FGerTypNr := 0;
  FDEList := TStringList.Create;
end;

{----------------------------------------------------}
destructor TDSfGInstanz.Destroy;
{----------------------------------------------------}
begin
  if (Assigned(FDEList)) then begin
    ClearDEList(FDEList);
    FDEList.Free;
  end;
  inherited Destroy;
end;

{ Übergibt eine Liste mit Datenelementen             }
{ Der bisherige Inhalt der Datenliste wird gelöscht  }
{ Parameter: Stringliste, DEA und Wert durch us getr.}
{----------------------------------------------------}
procedure TDSfGInstanz.SetDEList(Value: TStrings);
{----------------------------------------------------}
var
  i, j    : integer;
  s, sDEA : string;
  pWert   : PStr40;
begin
  ClearDEList(FDEList);
  for i := 0 to Value.Count-1 do begin
    s := Value[i];
    if (Length(s) > 0) then begin
      New(pWert);
      pWert^ := '';
      j := Pos(Chr(us), s);
      if (j > 0) then begin
        sDEA := Copy(s, 1, j-1);
        if (Length(s) > j) then pWert^ := Copy(s, j+1, Length(s)-j);
      end
      else sDEA := s;
      FDEList.AddObject(sDEA, TObject(pWert));
    end;
  end;
  GetInstAdrFromDEList;
  GetInstTypFromDEList;
  GetGerTypNrFromDEList;
end;

{ Übergibt eine Liste mit Datenelementen             }
{ Der Inhalt wir an die bisherige DEL angehängt      }
{ vorhandene DEA werden überschrieben                }
{ Parameter: Stringliste, DEA und Wert durch us getr.}
{----------------------------------------------------}
procedure TDSfGInstanz.AddDatenListe(pSl: TStrings);
{----------------------------------------------------}
var
  i, j    : integer;
  s, sDEA : string;
  pWert   : PStr40;
begin
  if (not Assigned(pSl)) then Exit;

  for i := 0 to pSl.Count-1 do begin
    s := pSl[i];
    if (Length(s) > 0) then begin
      New(pWert);
      pWert^ := '';
      j := Pos(Chr(us), s);
      if (j > 0) then begin
        sDEA := Copy(s, 1, j-1);
        if (Length(s) > j) then pWert^ := Copy(s, j+1, Length(s)-j);
      end
      else sDEA := s;
  // vorhandenen Eintrag ggf. löschen
      j := FDEList.IndexOf(sDEA);
      if (j >= 0) then begin
        Dispose(PStr40(FDEList.Objects[j]));
        FDEList.Delete(j);
      end;
  // Datenelement eintragen
      FDEList.AddObject(sDEA, TObject(pWert));
    end;
  end;
  GetInstAdrFromDEList;
  GetInstTypFromDEList;
  GetGerTypNrFromDEList;
end;

{ Fügt Dateneleent hinzu, löscht ggf. bisheriges     }
{ Parameter: Datenelementadresse, Wert               }
{----------------------------------------------------}
procedure TDSfGInstanz.AddDatenelement(sDEA, sWert: string);
{----------------------------------------------------}
var
  pWert   : PStr40;
  i       : integer;
begin
  New(pWert);
  pWert^ := sWert;
  // vorhandenen Eintrag ggf. löschen
  i := FDEList.IndexOf(sDEA);
  if (i >= 0) then begin
    Dispose(PStr40(FDEList.Objects[i]));
    FDEList.Delete(i);
  end;
  // Datenelement eintragen
  FDEList.AddObject(sDEA, TObject(pWert));
end;

{ Prüft, ob eine Datenelementadresse vorhanden ist   }
{ Parameter: Datenelementadresse                     }
{ Rückgabe: Vorhanden Ja/Nein                        }
{----------------------------------------------------}
function TDSfGInstanz.HasDatenelement(sDEA: string): boolean;
{----------------------------------------------------}
begin
  Result := (FDEList.IndexOf(sDEA) >= 0);
end;

{ Gibt den Wert zu einer Datenelementadresse zurück  }
{ Parameter: Datenelementadresse                     }
{ Rückgabe: Datenelementwert oder ''                 }
{----------------------------------------------------}
function TDSfGInstanz.GetWert(sDEA: string): string;
{----------------------------------------------------}
var
  i : integer;
begin
  i := FDEList.IndexOf(sDEA);
  if (i >= 0) then Result := PStr40(FDEList.Objects[i])^ else Result := '';
end;

{ Busadresse der Instanz aus Datenliste ermitteln    }
{----------------------------------------------------}
procedure TDSfGInstanz.GetInstAdrFromDEList;
{----------------------------------------------------}
var
  s : string;
begin
  if (HasDatenelement(C_DEA_InstAdr)) then begin
    s := GetWert(C_DEA_InstAdr);
    if (Length(s) = 1) then InstAdr := s[1];
  end
  else AddDatenelement(C_DEA_InstAdr, Self.InstAdr);  // 17.08.2001
end;

{ Instanz-Typ aus Datenliste ermitteln                }
{----------------------------------------------------}
procedure TDSfGInstanz.GetInstTypFromDEList;
{----------------------------------------------------}
var
  s : string;
begin
  if (HasDatenelement(C_DEA_InstTyp)) then begin
    s := GetWert(C_DEA_InstTyp);
    if (Length(s) > 0) then InstTyp := s[1];
  end;
end;

{ Gerätetyp-Nummer aus Datenliste ermitteln          }
{----------------------------------------------------}
procedure TDSfGInstanz.GetGerTypNrFromDEList;
{----------------------------------------------------}
begin
// 17.08.2001
  if (HasDatenelement(C_DEA_Hersteller)) and (HasDatenelement(C_DEA_GerTyp))
  then GerTypNr := GetGerTypNr(GetWert(C_DEA_Hersteller), GetWert(C_DEA_GerTyp))
  else GerTypNr := 0;
end;

{--------------------------------- TDSfGStation -------------------------------}

{----------------------------------------------------}
constructor TDSfGStation.Create;
{----------------------------------------------------}
begin
  inherited Create;
  FStationsId := 0;
  FStatName := '';
  FInstList := TStringList.Create;
end;

{----------------------------------------------------}
destructor TDSfGStation.Destroy;
{----------------------------------------------------}
begin
  if (Assigned(FInstList)) then begin
    ClearInstanzList(FInstList);
    FInstList.Free;
  end;
  inherited Destroy;
end;

{ Fügt DSfG-Instanz zu Objekt hinzu                  }
{ Parameter: Instanzadresse, Datenelementliste       }
{----------------------------------------------------}
procedure TDSfGStation.AddInstanz(cInstAdr: char; pDEList: TStrings);
{----------------------------------------------------}
var
  pInst : TDSfGInstanz;
begin
  if (FInstList.IndexOf(cInstAdr) < 0) then begin
    pInst := TDSfGInstanz.Create(cInstAdr);
    if (Assigned(pDEList)) then pInst.SetDEList(pDEList);
    FInstList.AddObject(UpperCase(cInstAdr), pInst);
  end
  else begin     // 17.08.2001
    pInst := GetInstanz(cInstAdr);
    if (Assigned(pDEList)) and (Assigned(pInst)) then
      pInst.AddDatenListe(pDEList);
  end;
end;

{ Löscht eine DSfG-Instanz aus dem Objekt            }
{ Parameter: Instanzadresse                          }
{----------------------------------------------------}
procedure TDSfGStation.DeleteInstanz(cInstAdr: char);
{----------------------------------------------------}
var
  i : integer;
begin
  i := FInstList.IndexOf(cInstAdr);
  if (i >= 0) then begin
    TDSfGInstanz(FInstList.Objects[i]).Free;
    FInstList.Delete(i);
  end;
end;

{ Gibt die Liste der gespeicherten Instanzen zurück  }
{ Rückgabe: Instanzen in DSfG-Darstellung (AB..E...) }
{----------------------------------------------------}
function TDSfGStation.GetTeilnehmer: string;
{----------------------------------------------------}
var
  c : char;
begin
  Result := '';
  for c := 'A' to '_' do
    if (FInstList.IndexOf(c) >= 0) then Result := Result + c
      else Result := Result + '.';
end;

{ Prüft, ob eine DSfG-Instanz vorhanden ist          }
{ Parameter: Instanz-Adresse                         }
{ Rückgabe: Vorhanden Ja/Nein                        }
{----------------------------------------------------}
function TDSfGStation.HasInstanz(cInstAdr: char): boolean;
{----------------------------------------------------}
begin
  Result := (FInstList.IndexOf(cInstAdr) >= 0);
end;

{ Anzahl der Datenelemente einer Instanz zurück      }
{ Parameter: Instanz-Adresse                         }
{ Rückgabe: Anzahl der Datenelemente, -1             }
{----------------------------------------------------}
function TDSfGStation.GetInstanzDECount(cInstAdr: char): integer;
{----------------------------------------------------}
var
  i : integer;
begin
  i := FInstList.IndexOf(cInstAdr);
  if (i < 0) then Result := -1
    else Result := TDSfGInstanz(FInstList.Objects[i]).DatenListe.Count;
end;

{ Prüft, ob eine DEA in einer Instanz vorhanden ist  }
{ Parameter: Instanz-Adresse, Dateelement-Adresse    }
{ Rückgabe: Vorhanden Ja/Nein                        }
{----------------------------------------------------}
function TDSfGStation.HasDatenelement(cInstAdr: char; sDEA: string): boolean;
{----------------------------------------------------}
var
  i : integer;
begin
  i := FInstList.IndexOf(cInstAdr);
  if (i < 0) then Result := False
    else Result := TDSfGInstanz(FInstList.Objects[i]).HasDatenelement(sDEA);
end;

{ Gibt die Liste der gespeicherten Instanzen zurück  }
{ Parameter: Instanz-Adresse, Dateelement-Adresse    }
{ Rückgabe: Wert oder ''                             }
{----------------------------------------------------}
function TDSfGStation.GetDatenelement(cInstAdr: char; sDEA: string): string;
{----------------------------------------------------}
var
  i : integer;
begin
  i := FInstList.IndexOf(cInstAdr);
  if (i < 0) then Result := ''
    else Result := TDSfGInstanz(FInstList.Objects[i]).GetWert(sDEA);
end;

{ Gibt den Typ der DSfG-Instanz zurück (1-stellig)   }
{ Parameter: Instanz-Adresse                         }
{ Rückgabe: DSfG-Instanztyp oder ' '                 }
{----------------------------------------------------}
function TDSfGStation.GetInstTyp(cInstAdr: char): char;
{----------------------------------------------------}
var
  i : integer;
begin
  i := FInstList.IndexOf(cInstAdr);
  if (i < 0) then Result := ' '
    else Result := TDSfGInstanz(FInstList.Objects[i]).InstTyp;
end;

{ Gibt die Gerätetyp-Nummer der DSfG-Instanz zurück  }
{ Parameter: Instanz-Adresse                         }
{ Rückgabe: Gerätetyp-Nummer                         }
{----------------------------------------------------}
function TDSfGStation.GetGerTypNr(cInstAdr: char): SmallInt;
{----------------------------------------------------}
var
  i : integer;
begin
  i := FInstList.IndexOf(cInstAdr);
  if (i < 0) then Result := 0
    else Result := TDSfGInstanz(FInstList.Objects[i]).GerTypNr;
end;

{ Gibt einen Zeiger auf eine DSfG-Instanz zurück     }
{ Parameter: Instanz-Adresse                         }
{ Rückgabe: Zeiger oder nil                          }
{----------------------------------------------------}
function TDSfGStation.GetInstanz(cInstAdr: char): TDSfGInstanz;
{----------------------------------------------------}
var
  i : integer;
begin
  i := FInstList.IndexOf(cInstAdr);
  if (i < 0) then Result := nil
    else Result := TDSfGInstanz(FInstList.Objects[i]);
end;

{ Gibt Anzahl der gespeicherten Teilnehmer zurück    }
{ Rückgabe: Anzahl der gespeicherten Teilnehmer      }
{----------------------------------------------------}
function TDSfGStation.GetTeilnehmerCount: integer;
{----------------------------------------------------}
begin
  Result := FInstList.Count;
end;

end.
