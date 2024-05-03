//------------------------------------------------------------------------------
// Liste zur Bildung von Mittelwerten und Differenzen
//
// 20.03.2013  GD  Neu
//
// Copyright (C) RMG Messtechnik GmbH 2013
//------------------------------------------------------------------------------
unit O_AverageList;

interface

uses
  SysUtils, Classes, DateUtils;

type
  TMyAverageType = (atNone, atAverage, atDifference);

  TMyAveragePoint = class(TObject)
    MyDateTime : TDateTime;
    MyValue    : double;
  end;

  TMyAverageList = class(TList)
    constructor Create(iAvgType: TMyAverageType; iInterval : integer); virtual;
  private
    FInterval      : integer;
    FType          : TMyAverageType;
    FLastTimestamp : TDateTime;
    FCurrentValue  : double;
    FSumValue      : double;
  protected
    function GetDataPoint(iIndex: integer): TMyAveragePoint;
    function RecalculateValue(pDataPoint: TMyAveragePoint): double;
  public
    procedure Delete(iIndex: integer); virtual;
    procedure Clear; override;
    function AddPoint(dt: TDateTime; fValue: double): integer;
    function RecalculateValues: double;
    property Interval: integer read FInterval;
    property CalcType: TMyAverageType read FType;
    property LastTimestamp: TDateTime read FLastTimestamp;
    property CurrentValue: double read FCurrentValue;
  end;

implementation

//------------------------------- TMyAverageList -------------------------------

//----------------------------------------------------
constructor TMyAverageList.Create(iAvgType: TMyAverageType; iInterval: integer);
//----------------------------------------------------
begin
  inherited Create;

  FType := iAvgType;       // Typ der Berechnung (Keiner, Mittelwert, Differenz
  FInterval := iInterval;  // Intervall den zu berechnenden Bereich in Sekunden
  FLastTimestamp := 0;     // Zeitstempel des zuletzt hinzugefügten Wertes
  FCurrentValue := 0;      // Aktuell zurückzugebender Wert
  FSumValue := 0;          // Intern: Aufsummierte Einzelwerte für Mittelwerte
end;

// Löscht einen Punkt aus der Liste und gibt das zugehörige Objekt frei
// Parameter: Index des Punktes
//----------------------------------------------------
procedure TMyAverageList.Delete(iIndex: integer);
//----------------------------------------------------
begin
  if (Assigned(Items[iIndex])) then
  try
    TMyAveragePoint(Items[iIndex]).Free;
  except
  // nix machen
  end;

  inherited Delete(iIndex);
end;

// Löscht alle Punkte aus der Liste und gibt die zugehörigen Objekt frei
//   --> wird bei "Destroy" der Liste mit aufgerufen
// Parameter: Index des Punktes
//----------------------------------------------------
procedure TMyAverageList.Clear;
{----------------------------------------------------}
var
  i : integer;
begin
  for i := Count-1 downto 0 do
    if (Assigned(Items[i])) then
    try
      TMyAveragePoint(Items[i]).Free;
    except
    // nix machen
    end;

  inherited;
end;

// Fügt einen Datenpunktwert zur Liste hinzu
// Parameter: Datum/Zeit und Wert des Datenpunktwertes
// Rückgabe: Index des hinzugefügten Punktes; -1 bei Fehler
//----------------------------------------------------
function TMyAverageList.AddPoint(dt: TDateTime; fValue: double): integer;
//----------------------------------------------------
var
  p : TMyAveragePoint;
begin
  try
    p := TMyAveragePoint.Create;
    p.MyDateTime := dt;
    p.MyValue := fValue;
    Result := Add(p);
    FLastTimestamp := p.MyDateTime;
    FCurrentValue := RecalculateValue(p);
  except
    Result := -1;
  end;
end;

// Gibt ein Datenpunktwert-Objekt aus Liste zurück
// Parameter: Listenindex des Datenpunktwertes
// Rückgabe: Datenpunktwert-Objekt oder nil
//----------------------------------------------------
function TMyAverageList.GetDataPoint(iIndex: integer): TMyAveragePoint;
//----------------------------------------------------
begin
  if (iIndex >= 0) and (iIndex < Count) then
  try
    Result := TMyAveragePoint(Items[iIndex]);
  except
    Result := nil;
  end
  else Result := nil;
end;

// Berechnet den Ergebniswert (Differenz, Durchschnitt) über alle Werte
// Rückgabe: Gibt neuen Ergebniswert zurück
//----------------------------------------------------
function TMyAverageList.RecalculateValues: double;
//----------------------------------------------------
var
  pCurrent  : TMyAveragePoint;
  i         : integer;
  dtOld     : TDateTime;
  fFirstVal : double;
begin
  if (Count > 0) then begin
    // Letzten Datenpunkt als Ausgangspunkt
    pCurrent := GetDataPoint(Count-1);
    // Anhand des letzten Datenpunktes veraltete Datenpunkte aus Liste entfernen
    dtOld := IncSecond(pCurrent.MyDateTime, -FInterval);
    while (Self.Count > 0) do begin
      if (GetDataPoint(0).MyDateTime < dtOld) then Delete(0) else Break;
    end;  // while Count > 0

    // Werte vorhanden?
    if (Count > 0) then begin
      // Bei Mittelwertbildung ...
      if (FType = atAverage) then begin
        // Summenwert zurücksetzen
        FSumValue := 0;
        // Werte über alle Datenpunkten aufsummieren
        for i := 0 to Count-1 do
          FSumValue := FSumValue + GetDataPoint(i).MyValue;
        // Ergebnis mitteln
        Result := FSumValue / Count;
      end
      // Bei Differenzbildung ...
      else if (FType = atDifference) then begin
        // ... ersten Wert ermitteln ...
        fFirstVal := GetDataPoint(0).MyValue;
        // ... und Differenz ermitteln
        Result := pCurrent.MyValue - fFirstVal;
        // Ergebnis kleiner Null wird auf Null gesetzt
        if (Result < 0) then Result := 0;
      end
      else Result := 0;
    end  // if Count > 0
    else Result := 0;
  end
  else Result := 0;
end;

// Berechnet den Ergebniswert (Differenz, Durchschnitt)
//   nur für hinzugekommenen Wert
// Parameter: Neuer Datenpunkt
// Rückgabe: Gibt neuen Ergebniswert zurück
//----------------------------------------------------
function TMyAverageList.RecalculateValue(pDataPoint: TMyAveragePoint): double;
//----------------------------------------------------
var
  dtOld     : TDateTime;
  fFirstVal : double;
begin
  // Anhand des aktuell übergebenen Datenpunktes veraltete Datenpunkte aus Liste entfernen
  dtOld := IncSecond(pDataPoint.MyDateTime, -FInterval);
  while (Self.Count > 0) do begin
    if (GetDataPoint(0).MyDateTime < dtOld) then begin
      // Bei Mittelwertbildung alten Wert von Summe abziehen
      if (FType = atAverage) then
        FSumValue := FSumValue - GetDataPoint(0).MyValue;
      Delete(0);
    end
    else Break;
  end;  // while Count > 0

  // Werte vorhanden?
  if (Count > 0) then begin
    // Bei Mittelwertbildung ...
    if (FType = atAverage) then begin
      // ...neuen Wert in Summe aufnehmen ...
      FSumValue := FSumValue + pDataPoint.MyValue;
      // ... und neu mitteln
      Result := FSumValue / Count;
    end
    // Bei Differenzbildung ...
    else if (FType = atDifference) then begin
      // ... ersten Wert ermitteln ...
      fFirstVal := GetDataPoint(0).MyValue;
      // ... und Differenz ermitteln
      Result := pDataPoint.MyValue - fFirstVal;
      // Ergebnis kleiner Null wird auf Null gesetzt
      if (Result < 0) then Result := 0;
    end
    else Result := 0;
  end  // if Count > 0
  else Result := 0;
end;

end.
