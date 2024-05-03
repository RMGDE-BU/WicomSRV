{------------------------------------------------------------------------------}
{ Unit mit Infodatei zu Daten einer Tabelle                                    }
{                                                                              }
{ 05.08.2002  GD  Neu                                                          }
{ 26.02.2004  GD  RebuildInfoList kann public einzel aufgerufen werden         }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2002, 2004                                    }
{------------------------------------------------------------------------------}
unit DatenInfo;

interface

uses
  SysUtils, Classes, IniFiles,
  GD_Utils;

const
  C_DatenInfoSeparator = #9;
  C_InfoFileExtension  = '.INF';

type
  TDatenInfoIni = class(TIniFile)
  private
  protected
    function GetMinDate: TDateTime;
    procedure SetMinDate(dtValue: TDateTime);
    function GetMaxDate: TDateTime;
    procedure SetMaxDate(dtValue: TDateTime);
    function GetMinReference: integer;
    procedure SetMinReference(iValue: integer);
    function GetMaxReference: integer;
    procedure SetMaxReference(iValue: integer);
    function GetAktReference: integer;
    procedure SetAktReference(iValue: integer);
  public
    property MinDate: TDateTime read GetMinDate write SetMinDate;
    property MaxDate: TDateTime read GetMaxDate write SetMaxDate;
    property MinReference: integer read GetMinReference write SetMinReference;
    property MaxReference: integer read GetMaxReference write SetMaxReference;
    property AktReference: integer read GetAktReference write SetAktReference;
  end;

  TDatenInfoList = class(TStringList)
    constructor Create(sFileName: TFileName; sDtMask: string = ''); virtual;
    destructor Destroy; override;
  private
    FFileName     : string;
    FDateTimeMask : string;
    FDateList     : TStrings;
    FRefVonList   : TStrings;
    FRefBisList   : TStrings;
    FModified     : boolean;
    FDateTimeVon  : TDateTime;
    FDateTimeBis  : TDateTime;
    FRefVon       : integer;
    FRefBis       : integer;
    FRefAkt       : integer;
    FIniFile      : TDatenInfoIni;
    procedure ExtractDataList;
    function GetDateTimeFromString(
      sDateTime: string; sMask: string = ''): TDateTime;
  protected
  public
    procedure RebuildInfoLists;
    procedure LoadFile(sFileName: TFileName = '');
    procedure SaveFile(sFileName: TFileName = '');
    procedure InsertRecord(
      dtDateTime: TDateTime; iRef: integer; bRebuild: boolean = True);
    function GetPriorByDateTime(
      var dtAkt: TDateTime; var iRefVon, iRefBis: integer): boolean;
    function GetNextByDateTime(
      var dtAkt: TDateTime; var iRefVon, iRefBis: integer): boolean;
    function GetReferenceByDateTime(
      dtAkt: TDateTime; var iRefVon, iRefBis: integer): boolean;
    property MinDate: TDateTime read FDateTimeVon;
    property MaxDate: TDateTime read FDateTimeBis;
    property MinReference: integer read FRefVon;
    property MaxReference: integer read FRefBis;
    property AktReference: integer read FRefAkt;
    property FileName: string read FFileName;
  end;

implementation

const
  C_DateTimeMask          = 'yyyymmdd';

  C_Section_KeyValues     = 'KEYVALUES';

  C_Ident_DateTimeMin     = 'DATETIMEMIN';
  C_Ident_DateTimeMax     = 'DATETIMEMAX';
  C_Ident_RefMin          = 'REFMIN';
  C_Ident_RefMax          = 'REFMAX';
  C_Ident_RefAkt          = 'REFAKT';

  C_Section_DataList      = 'DATALIST';


{------------------------------- TDatenInfoIni -------------------------------}

{ Holt untere Datum/Zeit-Grenze aus IniFile         }
{ Rückgabe: Untere Datum/Zeit-Grenze oder 0         }
{---------------------------------------------------}
function TDatenInfoIni.GetMinDate: TDateTime;
{---------------------------------------------------}
begin
  Result := ReadDateTime(C_Section_KeyValues, C_Ident_DateTimeMin, 0);
end;

{ Setzt untere Datum/Zeit-Grenze in IniFile         }
{ Parameter: Untere Datum/Zeit-Grenze               }
{---------------------------------------------------}
procedure TDatenInfoIni.SetMinDate(dtValue: TDateTime);
{---------------------------------------------------}
begin
  WriteDateTime(C_Section_KeyValues, C_Ident_DateTimeMin, dtValue);
end;

{ Holt obere Datum/Zeit-Grenze aus IniFile          }
{ Rückgabe: Obere Datum/Zeit-Grenze oder 0          }
{---------------------------------------------------}
function TDatenInfoIni.GetMaxDate: TDateTime;
{---------------------------------------------------}
begin
  Result := ReadDateTime(C_Section_KeyValues, C_Ident_DateTimeMax, 0);
end;

{ Setzt untere Datum/Zeit-Grenze in IniFile         }
{ Parameter: Untere Datum/Zeit-Grenze               }
{---------------------------------------------------}
procedure TDatenInfoIni.SetMaxDate(dtValue: TDateTime);
{---------------------------------------------------}
begin
  WriteDateTime(C_Section_KeyValues, C_Ident_DateTimeMax, dtValue);
end;

{ Holt niedrigste Referenznummer aus IniFile        }
{ Rückgabe: Niedrigste Referenznummer oder -1       }
{---------------------------------------------------}
function TDatenInfoIni.GetMinReference: integer;
{---------------------------------------------------}
begin
  Result := ReadInteger(C_Section_KeyValues, C_Ident_RefMin, -1);
end;

{ Setzt niedrigste Referenznummer in IniFile        }
{ Parameter: Niedrigste Referenznummer              }
{---------------------------------------------------}
procedure TDatenInfoIni.SetMinReference(iValue: integer);
{---------------------------------------------------}
begin
  WriteInteger(C_Section_KeyValues, C_Ident_RefMin, iValue);
end;

{ Holt höchste Referenznummer aus IniFile           }
{ Rückgabe: Höchste Referenznummer oder -1          }
{---------------------------------------------------}
function TDatenInfoIni.GetMaxReference: integer;
{---------------------------------------------------}
begin
  Result := ReadInteger(C_Section_KeyValues, C_Ident_RefMax, -1);
end;

{ Setzt höchste Referenznummer in IniFile           }
{ Parameter: Höchste Referenznummer                 }
{---------------------------------------------------}
procedure TDatenInfoIni.SetMaxReference(iValue: integer);
{---------------------------------------------------}
begin
  WriteInteger(C_Section_KeyValues, C_Ident_RefMax, iValue);
end;

{ Holt aktuelle Referenznummer aus IniFile          }
{ Rückgabe: Aktuelle Referenznummer oder -1         }
{---------------------------------------------------}
function TDatenInfoIni.GetAktReference: integer;
{---------------------------------------------------}
begin
  Result := ReadInteger(C_Section_KeyValues, C_Ident_RefAkt, -1);
end;

{ Setzt aktuelle Referenznummer in IniFile          }
{ Parameter: Aktuelle Referenznummer                }
{---------------------------------------------------}
procedure TDatenInfoIni.SetAktReference(iValue: integer);
{---------------------------------------------------}
begin
  WriteInteger(C_Section_KeyValues, C_Ident_RefAkt, iValue);
end;

{------------------------------- TDatenInfoList -------------------------------}

{ Konstruktor                                       }
{ Parameter: Name der Infodatei, Maske für DatumZeit}
{---------------------------------------------------}
constructor TDatenInfoList.Create(sFileName: TFileName; sDtMask: string = '');
{---------------------------------------------------}
begin
  inherited Create;


  FFileName := sFileName;
  FDateTimeMask := sDtMask;  // ACHTUNG: FESTE LÄNGEN BEI MASKE ERFORDERLICH
  if (FDateTimeMask = '') then FDateTimeMask := C_DateTimeMask;
  if (sFileName <>  '') then  // Infodatei als INI-File für zusätzliche Infos
    FIniFile := TDatenInfoIni.Create(FFileName);

  // Sublisten für schnellen Zugriff
  FDateList := TStringList.Create;
  FRefVonList := TStringList.Create;
  FRefBisList := TStringList.Create;

  // Kenndaten erster, letzter Satz
  FDateTimeVon := 0;    // Gültige Daten sind > 0
  FDateTimeBis := 0;
  FRefVon := -1;        // Gültige Referenzen sind >= 0
  FRefBis := -1;
  FRefAkt := -1;

  FModified := False;  // Flag, ob Änderungen vorgenommen wurden
  LoadFile(FFileName);
end;

{ Destruktor                                        }
{---------------------------------------------------}
destructor TDatenInfoList.Destroy;
{---------------------------------------------------}
begin
  FDateList.Free;
  FRefVonList.Free;
  FRefBisList.Free;

  FIniFile.Free;

  inherited Destroy;
end;

{ Generiert aus String DateTime gemäß interner Maske}
{ Parameter: DateTime-String, Maske für Format      }
{ Rückgabe: DateTime oder 0                         }
// ACHTUNG: FESTE LÄNGEN BEI MASKE ERFORDERLICH    //
{---------------------------------------------------}
function TDatenInfoList.GetDateTimeFromString(
  sDateTime: string; sMask: string = ''): TDateTime;
{---------------------------------------------------}
var
  sy, sm, sd, sh, sn, ss, sz : string;
  i                          : integer;
begin
  if (sMask = '') then sMask :=  FDateTimeMask;
  if (Length(sDateTime) = Length(sMask)) then begin
    // alle Strings für die DateTime-Teilinfos mit '' initialisieren
    sy := '';
    sm := '';
    sd := '';
    sh := '';
    sn := '';
    ss := '';
    sz := '';

    // Schleife über den übergebenen string
    for i := 1 to Length(sMask) do
      case sMask[i] of
        'y' : sy := sy + sDateTime[i];
        'm' : sm := sm + sDateTime[i];
        'd' : sd := sd + sDateTime[i];
        'h' : sh := sh + sDateTime[i];
        'n' : sn := sn + sDateTime[i];
        's' : ss := ss + sDateTime[i];
        'z' : sz := sz + sDateTime[i];
      end;

    Result := EncodeDate(                               // Date-Anteil
      StrToIntDef(sy, 0), StrToIntDef(sm, 0), StrToIntDef(sd, 0));
    Result := Result + EncodeTime(StrToIntDef(sh, 0),   // Time-Anteil
      StrToIntDef(sn, 0), StrToIntDef(ss, 0), StrToIntDef(sz, 0));
  end
  else Result := 0;
end;

{ Baut Infolisten neu auf                           }
{ Parameter: Name der Infodatei                     }
{---------------------------------------------------}
procedure TDatenInfoList.RebuildInfoLists;
{---------------------------------------------------}

  // Prüft Werte, fügt sie ein
  function InsertValues(sDt: string; iVon, iBis: integer): boolean;
  var
    i, iRef : integer;
    dt      : TDateTime;
  begin
    dt := GetDateTimeFromString(sDt);
    Result := (dt > 0);
    if (Result) then begin
      i := FDateList.IndexOf(sDt);
      Result := (i < 0);

      // Eckwerte prüfen
      if (FDateTimeVon <= 0) or (dt < Trunc(FDateTimeVon)) then
        FDateTimeVon := dt;
      if (FDateTimeBis <= 0) or (dt > FDateTimeBis) then
        FDateTimeBis := dt;
      if (FRefVon < 0) or (FRefVon > iVon) then FRefVon := iVon;
      if (FRefBis < 0) or (FRefBis < iBis) then FRefBis := iBis;

      // Alles OK: Werte eintragen
      if (Result) then begin
        FDateList.Add(sDt);
        FRefVonList.Add(IntToStr(iVon));
        FRefBisList.Add(IntToStr(iBis));
      end
      // Nicht OK: Einträge korrigieren
      else begin
        if (iVon >= 0) then begin  // Ggf. Von-Referenz korrigieren
          iRef := StrToInt(FRefVonList[i]);
          if (iRef < 0) or (iRef > iVon) then FRefVonList[i] := IntToStr(iVon);
        end;
        if (iBis >= 0) then begin  // Ggf. Bis-Referenz korrigieren
          iRef := StrToInt(FRefBisList[i]);
          if (iRef < 0) or (iRef < iBis) then FRefBisList[i] := IntToStr(iBis);
        end;
      end;
    end;
  end;

var
  i, iRefVon, iRefBis : integer;
  sDateTime           : string;
  bKorrektur          : boolean;
begin
  BeginUpdate;
  try

    FDateList.Clear;
    FRefVonList.Clear;
    FRefBisList.Clear;

    if (Count > 0) then begin  // mindestens eine Datenzeile
      Sort;
      bKorrektur := False;  // Flag, ob die Liste korrigiert werden muss
      // Schleife über Datenzeilen
      for i := 0 to Count-1 do begin
        sDateTime := GetStringPart(Strings[i], 1, C_DatenInfoSeparator);
        iRefVon :=
          StrToIntDef(GetStringPart(Strings[i], 2, C_DatenInfoSeparator), -1);
        iRefBis :=
          StrToIntDef(GetStringPart(Strings[i], 3, C_DatenInfoSeparator), -1);
        if (not InsertValues(sDateTime, iRefVon, iRefBis)) then
          bKorrektur := True;
      end;

      if (bKorrektur) then begin
        Clear;
        for i := 0 to FDateList.Count-1 do
          Add(FDateList[i] + C_DatenInfoSeparator + FRefVonList[i]
            + C_DatenInfoSeparator + FRefBisList[i]);
      end;
    end;

  finally
    EndUpdate;
  end;
end;

{ Extrahiert den reinen Dateninhalt aus der Liste   }
{---------------------------------------------------}
procedure TDatenInfoList.ExtractDataList;
{---------------------------------------------------}
var
  i, j : integer;
  s    : string;
begin
  // Alles ausser Datenteil entfernen - oberer Teil
  while (Count > 0) do begin
    s := Strings[0];
    Delete(0);
    if (Pos(C_Section_DataList, s) > 0) then Break;
  end;
  // Alles ausser Datenteil entfernen - unterer Teil
  i := 0;
  while (i < Count) do begin
    if (Pos(C_DatenInfoSeparator, Strings[i]) = 0) then Break;
    Inc(i);
  end;
  for j := Count-1 downto i do Delete(j);
end;

{ Dateninfo aus Datei laden                         }
{ Parameter: Name der Infodatei                     }
{---------------------------------------------------}
procedure TDatenInfoList.LoadFile(sFileName: TFileName = '');
{---------------------------------------------------}
begin
  if (sFileName = '') then sFileName := FFileName;
  if (sFileName = '') then Exit;

  if (FileExists(sFileName)) then begin
    if (UpperCase(sFileName) <> UpperCase(FFileName)) then begin
      FFileName := sFileName;
      FIniFile.Free;
      FIniFile := TDatenInfoIni.Create(FFileName);
    end;

    Clear;

    FDateTimeVon := FIniFile.MinDate;
    FDateTimeBis := FIniFile.MaxDate;
    FRefVon := FIniFile.MinReference;
    FRefBis := FIniFile.MaxReference;
    FRefAkt := FIniFile.AktReference;

    LoadFromFile(FFileName);
    ExtractDataList;
    RebuildInfoLists;
  end;
end;

{ Dateninfo in Datei speichern                      }
{ Parameter: Name der Infodatei                     }
{---------------------------------------------------}
procedure TDatenInfoList.SaveFile(sFileName: TFileName = '');
{---------------------------------------------------}
begin
  if (sFileName = '') then sFileName := FFileName;
  if (sFileName = '') then Exit;

  Insert(0, '[' + C_Section_DataList + ']');
  SaveToFile(sFileName);

  with TDatenInfoIni.Create(sFileName) do
  try
    MinDate := FDateTimeVon;
    MaxDate := FDateTimeBis;
    MinReference := FRefVon;
    MaxReference := FRefBis;
    AktReference := FRefAkt;
  finally
    Free;
  end;

  FModified := False;
end;

{ Erweitert Infoliste um neuen Eintrag              }
{ Parameter: DatumZeit, Referenznummer              }
// ACHTUNG bei großen Datenmengen:
//   Anzahl der Einträge (Count) entscheidend
{---------------------------------------------------}
procedure TDatenInfoList.InsertRecord(
  dtDateTime: TDateTime; iRef: integer; bRebuild: boolean = True);
{---------------------------------------------------}
var
  i, iRefVon, iRefBis : integer;
  bModified           : boolean;
begin
  if (iRef < 0) then Exit;

  // Einträge für Eckwerte prüfen und ggf. neu setzen
  if (FDateTimeVon = 0) or (FDateTimeVon > dtDateTime) then
    FDateTimeVon := dtDateTime;
  if (FDateTimeBis = 0) or (FDateTimeBis < dtDateTime) then
    FDateTimeBis := dtDateTime;
  if (FRefVon < 0) or (FRefVon > iRef) then FRefVon := iRef;
  if (FRefBis < 0) or (FRefBis < iRef) then FRefBis := iRef;
  FRefAkt := iRef;

  i := FDateList.IndexOf(FormatDateTime(FDateTimeMask, dtDateTime));

  // noch kein Eintrag vorhanden
  if (i < 0) then begin
    Add(FormatDateTime(FDateTimeMask, dtDateTime) + C_DatenInfoSeparator +
      IntToStr(iRef) + C_DatenInfoSeparator + IntToStr(iRef));
    if (bRebuild) and (iRef > 0)
    then RebuildInfoLists
    else begin
      FDateList.Add(FormatDateTime(FDateTimeMask, dtDateTime));
      FRefVonList.Add(IntToStr(iRef));
      FRefBisList.Add(IntToStr(iRef));
    end;
    FModified := True;
  end
  else begin
    bModified := False;
    iRefVon := StrToIntDef(FRefVonList[i], -1);
    iRefBis := StrToIntDef(FRefBisList[i], -1);
    if (iRefVon < 0) or (iRefVon > iRef) then begin
      FRefVonList[i] := IntToStr(iRef);
      bModified := True;
    end;
    if (iRefBis < 0) or (iRefBis < iRef) then begin
      FRefBisList[i] := IntToStr(iRef);
      bModified := True;
    end;
    if (bModified) then begin
      Strings[i] := FDateList[i] +  C_DatenInfoSeparator + FRefVonList[i] +
        C_DatenInfoSeparator + FRefBisList[i];
      FModified := True;
    end;
  end;
end;

{ Referenzdaten des vorhergehenden Datensatzes holen}
{ Parameter: Datum/Zeit aktuell, Übergabevariablen  }
{ Rückgabe: Vorhergehender Datensaz vorhanden ?     }
{---------------------------------------------------}
function TDatenInfoList.GetPriorByDateTime(
  var dtAkt: TDateTime; var iRefVon, iRefBis: integer): boolean;
{---------------------------------------------------}
var
  i : integer;
begin
  Result := False;
  iRefVon := -1;
  iRefBis := -1;

  // 1. Möglichkeit: Prüfen, ob aktuelle Zeit explizit gespeichert ist
  i := FDateList.IndexOf(FormatDateTime(FDateTimeMask, dtAkt));
  if (i >= 0) then begin
    // Bei i = 0 gibt es keinen vorhergehenden Satz ...
    if (i > 0) then begin
      dtAkt := GetDateTimeFromString(FDateList[i-1]);
      iRefVon := StrToIntDef(FRefVonList[i-1], 0);
      iRefBis := StrToIntDef(FRefBisList[i-1], 0);
      Result := True;
    end;
    Exit;
  end;

  // 2. Möglichkeit: Prüfen, ob aktuelle Zeit ausserhalb der Grenzen liegt
  i := Count;
  if (i = 0) or (dtAkt < FDateTimeVon) then Exit;
  if (dtAkt > FDateTimeBis) then begin
    dtAkt := GetDateTimeFromString(FDateList[i-1]);
    iRefVon := StrToIntDef(FRefVonList[i-1], 0);
    iRefBis := StrToIntDef(FRefBisList[i-1], 0);
    Result := True;
    Exit;
  end;

  // 3. Möglichkeit: Liste durchsuchen
  for i := 1 to FDateList.Count-1 do
    if (GetDateTimeFromString(FDateList[i]) > dtAkt) then begin
      dtAkt := GetDateTimeFromString(FDateList[i-1]);
      iRefVon := StrToIntDef(FRefVonList[i-1], 0);
      iRefBis := StrToIntDef(FRefBisList[i-1], 0);
      Result := True;
      Exit;
    end;
end;

{ Referenzdaten des nachfolgenden Datensatzes holen }
{ Parameter: Datum/Zeit aktuell, Übergabevariablen  }
{ Rückgabe: Nachfolgender Datensaz vorhanden ?      }
{---------------------------------------------------}
function TDatenInfoList.GetNextByDateTime(
  var dtAkt: TDateTime; var iRefVon, iRefBis: integer): boolean;
{---------------------------------------------------}
var
  i : integer;
begin
  Result := False;
  iRefVon := -1;
  iRefBis := -1;

  // 1. Möglichkeit: Prüfen, ob aktuelle Zeit explizit gespeichert ist
  i := FDateList.IndexOf(FormatDateTime(FDateTimeMask, dtAkt));
  if (i >= 0) then begin
    // Bei i = Count-1 gibt es keinen nachfolgenden Satz ...
    if (i < FDateList.Count-1) then begin
      dtAkt := GetDateTimeFromString(FDateList[i+1]);
      iRefVon := StrToIntDef(FRefVonList[i+1], 0);
      iRefBis := StrToIntDef(FRefBisList[i+1], 0);
      Result := True;
    end;
    Exit;
  end;

  // 2. Möglichkeit: Prüfen, ob aktuelle Zeit ausserhalb der Grenzen liegt
  i := Count;
  if (i = 0) or (dtAkt > FDateTimeBis) then Exit;
  if (dtAkt < FDateTimeVon) then begin
    dtAkt := GetDateTimeFromString(FDateList[0]);
    iRefVon := StrToIntDef(FRefVonList[0], 0);
    iRefBis := StrToIntDef(FRefBisList[0], 0);
    Result := True;
    Exit;
  end;

  // 3. Möglichkeit: Liste durchsuchen
  for i := FDateList.Count-2 downto 0 do
    if (GetDateTimeFromString(FDateList[i]) < dtAkt) then begin
      dtAkt := GetDateTimeFromString(FDateList[i+1]);
      iRefVon := StrToIntDef(FRefVonList[i+1], 0);
      iRefBis := StrToIntDef(FRefBisList[i+1], 0);
      Result := True;
      Break;
    end;
end;

{ Referenzdaten eines Datensatzes holen             }
{ Parameter: Datum/Zeit, Übergabevariablen          }
{ Rückgabe: Gesuchter Datensaz vorhanden ?          }
{---------------------------------------------------}
function TDatenInfoList.GetReferenceByDateTime(
  dtAkt: TDateTime; var iRefVon, iRefBis: integer): boolean;
{---------------------------------------------------}
var
  i : integer;
begin
  iRefVon := -1;
  iRefBis := -1;

  // 1. Möglichkeit: Prüfen, ob aktuelle Zeit explizit gespeichert ist
  i := FDateList.IndexOf(FormatDateTime(FDateTimeMask, dtAkt));
  if (i >= 0) then begin
    iRefVon := StrToIntDef(FRefVonList[i], 0);
    iRefBis := StrToIntDef(FRefBisList[i], 0);
    Result := True;
  end
  else Result := False;
end;

end.
