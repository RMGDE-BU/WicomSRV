{------------------------------------------------------------------------------}
{ Bereitstellung von MRG-Abrufdaten                                            }
{                                                                              }
{ 31.10.2001  GD  Neu                                                          }
{ 31.07.2003  GD  Bugfix: Stati bei Tagessätzen                                }
{ 14.11.2003  GD  Bugfix: OrgFaktor bei Tagessätzen                            }
{ 06.08.2004  GD  Bugfix: Zeitbereich bis                                      }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, 2004                                    }
{------------------------------------------------------------------------------}
unit MRGAbrufdaten;

interface

uses
  SysUtils, Classes,
  Abrufdaten, MrgInfos, Langzeit, MrgStd, WSysCon, GD_Utils;

type
  TValueType = (vt_Rohwert, vt_Physikalisch);

  TMrgAbrufDaten = class(TAbrufDaten)
    constructor Create(sDatabaseName: string; iMrgId: integer;
      pValueType: TValueType; pMrgInfo: TMRGAbrufInfos); virtual;
    destructor Destroy; override;
  private
    FMrgInfo     : TMRGAbrufInfos;
    FThisMrg     : TMrgInfoList;
    FKanalList   : TList;
    FValueType   : TValueType;
    FKennung     : string;
    FKanaele     : TByteSet;
    function GetValueStr(pKanal: TMyMSNewKanal; fValue: Double): string;
    function GetKanalStatusStr(
      pKanal: TMyMSNewKanal; iKanalStatus: Byte): string;
    function GetKanalStatusText (KanalStatus: Byte): string;  // 31.07.2003
    function GetImpulsKanalStatusText (KanalStatus: Byte): string;
    function GetAnalogKanalStatusText (KanalStatus: Byte): string;
    function GetSatzStatusText (SatzStatus: Byte): string;
    procedure LoadStundenwerte;
    procedure LoadTagessaetze;
  protected
    procedure AutoDelete; override;
  public
    procedure Aktualisieren;
    procedure LoadData(dtVon: TDateTime = 0; dtBis: TDateTime = 0); override;
    property ValueType: TValueType read FValueType write FValueType;
    property StatusListe;
    property Kanaele: TByteSet read FKanaele write FKanaele;
  end;

implementation

const
  C_Index_DatumZeit = 0;

{----------------------------- TMrgAbrufDaten ---------------------------------}

{------------------------------------------------------}
constructor TMrgAbrufDaten.Create(sDatabaseName: string; iMrgId: integer;
  pValueType: TValueType; pMrgInfo: TMRGAbrufInfos);
{------------------------------------------------------}
begin
  inherited Create(sDatabaseName, iMrgId);

  FMrgInfo := pMrgInfo;
  FValueType := pValueType;
  FThisMrg := FMrgInfo.GetStationsInfo(GerId);
  if (not Assigned(FThisMrg)) then raise Exception.Create(
    'MRG mit der ID ' + IntToStr(GerId) +  ' nicht vorhanden !');
  FKennung := FThisMrg.Kennung;
  FKanalList := TList.Create;
  IndexDatumZeit := C_Index_DatumZeit;
  FKanaele := [];
end;

{------------------------------------------------------}
destructor TMrgAbrufDaten.Destroy;
{------------------------------------------------------}
begin
  FKanalList.Free;

  inherited;
end;

{ Abrufinformationen aktualisieren                     }
{------------------------------------------------------}
procedure TMrgAbrufDaten.Aktualisieren;
{------------------------------------------------------}
begin
  FThisMrg := FMrgInfo.GetStationsInfo(GerId);
end;

{------------------------------------------------------}
procedure TMrgAbrufDaten.LoadStundenwerte;
{------------------------------------------------------}
var
  i        : integer;
  pMSKanal : TMyMSNewKanal;
begin
  OldVon := 0;
  OldBis := 0;
  // Werte in Spalten eintragen
  FMrgInfo.LgzStd.ManuNr:= GerId;
  if (FMrgInfo.LgzStd.Search(FKennung)) then begin
    FMrgInfo.LGZStd.Open;
    try
      if FMrgInfo.LgzStd.SearchDatum(DatumVon) then begin
        OldVon := DatumVon;
        while (not FMrgInfo.LgzStd.EOF) and (FMrgInfo.LgzStd.Datum <= DatumBis)
        do begin
          OldBis := FMrgInfo.LgzStd.Datum;
          for i := 0 to FKanalList.Count-1 do begin
            if (i = 0) then begin
              GetCol(0).AddString(FormatDateTime(C_FormatDateTime, OldBis));
              StatusListe.GetCol(0).Add(
                GetSatzStatusText(FMrgInfo.LgzStd.SatzStatus));
            end;
            pMSKanal := TMyMSNewKanal(FKanalList[i]);
            if (FKanaele <> []) and
               (not (pMSKanal.MrgKanal.asInteger in FKanaele))
            then Continue;
            GetCol(i+1).AddString(GetValueStr(
              pMSKanal, FMrgInfo.LGZStd.Wert[pMSKanal.MRGKanal.asInteger - 1]));
            StatusListe.GetCol(i+1).Add(GetKanalStatusStr(pMSKanal,
              FMrgInfo.LgzStd.KanalStatus [pMSKanal.MRGKanal.asInteger - 1]));
          end;
          FMrgInfo.LgzStd.Next;
        end;
      end;
    finally
      FMrgInfo.LGZStd.Close;
    end;
  end;
end;

{------------------------------------------------------}
procedure TMrgAbrufDaten.LoadTagessaetze;
{------------------------------------------------------}
var
  i        : integer;
  pMSKanal : TMyMSNewKanal;
begin
  OldVon := 0;
  OldBis := 0;
  // Werte in Spalten eintragen
  FMrgInfo.LgzTag.ManuNr:= GerId;
  if (FMrgInfo.LgzTag.Search(FKennung)) then begin
    FMrgInfo.LgzTag.Open;
    try
      if FMrgInfo.LgzTag.SearchDatum(DatumVon) then begin
        OldVon := DatumVon;
        while (not FMrgInfo.LgzTag.EOF) and (FMrgInfo.LgzTag.Datum <= DatumBis)
        do begin
          OldBis := FMrgInfo.LgzTag.Datum;
          for i := 0 to FKanalList.Count-1 do begin
            if (i = 0) then begin
              GetCol(0).AddString(FormatDateTime(C_FormatDateTime, OldBis));
              StatusListe.GetCol(0).Add(
                GetSatzStatusText(FMrgInfo.LgzTag.SatzStatus));
            end;
            pMSKanal := TMyMSNewKanal(FKanalList[i]);
            if (FKanaele <> []) and
               (not (pMSKanal.MrgKanal.asInteger in FKanaele))
            then Continue;
            if (GetCol(i+1).Kontroll) then begin
              GetCol(i+1).AddString(GetValueStr(pMSKanal,
                FMrgInfo.LgzTag.KontrollWert[pMSKanal.MRGKanal.asInteger - 1]));
              StatusListe.GetCol(i+1).Add(GetKanalStatusStr(pMSKanal,
                FMrgInfo.LgzTag.KontrollStatus [pMSKanal.MRGKanal.asInteger - 1]));
            end
            else begin
              GetCol(i+1).AddString(GetValueStr(pMSKanal,
                FMrgInfo.LgzTag.EingangWert[pMSKanal.MRGKanal.asInteger - 1]));
              StatusListe.GetCol(i+1).Add(GetKanalStatusStr(pMSKanal,
                FMrgInfo.LgzTag.EingangStatus [pMSKanal.MRGKanal.asInteger - 1]));
            end;
          end;
          FMrgInfo.LgzTag.Next;
        end;
      end;
    finally
      FMrgInfo.LgzTag.Close;
    end;
  end;
end;

{ Lädt Daten für den Archivgruppen-Index               }
{------------------------------------------------------}
procedure TMrgAbrufDaten.LoadData(dtVon: TDateTime = 0; dtBis: TDateTime = 0);
{------------------------------------------------------}
var
  i    : integer;
  iTyp : byte;
  y, m, d : word;
begin
  Clear;
  StatusListe.Clear;
  AddCol(S_Text_DatumZeit, C_SpaltenTyp_DatumZeit);
  StatusListe.Add(TStringList.Create);  // Liste für Satzstatus

  DatumVon := dtVon;
  DatumBis := dtBis;

  // Zeitbereich bestimmen
  if (StundenWerte) then begin
    if (DatumVon = 0) then DatumVon := FMrgInfo.GetStationsInfo(GerId).DatenVon;
    if (DatumBis = 0) then DatumBis := FMrgInfo.GetStationsInfo(GerId).DatenBis;
    if (DatumVon = 0) or (DatumBis = 0) then Exit;

    if (dtVon = 0) then begin
      if (Frac(DatumBis) > 0)
      then DatumVon := Trunc(DatumBis)
      else DatumVon := Trunc(DatumBis-1);
    end;

    if (DatumVon < FMrgInfo.GetStationsInfo(GerId).DatenVon) then
      DatumVon := FMrgInfo.GetStationsInfo(GerId).DatenVon;
    if (DatumBis > FMrgInfo.GetStationsInfo(GerId).DatenBis) then
      DatumBis := FMrgInfo.GetStationsInfo(GerId).DatenBis;
  end
  else begin
    if (DatumVon = 0) then DatumVon := FMrgInfo.GetStationsInfo(GerId).TDatenVon;
    if (DatumBis = 0) then DatumBis := FMrgInfo.GetStationsInfo(GerId).TDatenBis;
    if (DatumVon = 0) or (DatumBis = 0) then Exit;

    if (dtVon = 0) then begin
      DecodeDate(DatumBis, y, m, d);
      DatumVon := EncodeDate(y, m, 1);
    end;

    if (DatumVon < FMrgInfo.GetStationsInfo(GerId).TDatenVon) then
      DatumVon := FMrgInfo.GetStationsInfo(GerId).TDatenVon;
    if (DatumBis > FMrgInfo.GetStationsInfo(GerId).TDatenBis) then // 06.08.2004
      DatumBis := FMrgInfo.GetStationsInfo(GerId).TDatenBis;
  end;

  // Aktive Kanäle in Liste aufnehmen
  FKanalList.Clear;
  for i := 0 to FThisMrg.Count-1 do begin
    if (FKanaele <> []) and
       (not (FThisMrg.Kanal[i].MrgKanal.asInteger in FKanaele))
    then Continue;

    if (FThisMrg.Kanal[i].Aktiv.asBoolean) then begin
      if (FThisMrg.Kanal[i].Kanaltyp = KTypImpuls) then
        iTyp := C_SpaltenTyp_Zaehlerstand
      else if (FThisMrg.Kanal[i].Kanaltyp = KTypAnalog) then
        iTyp := C_SpaltenTyp_Messwert
      else iTyp := 0;

      if (not StundenWerte) then begin
        if (FThisMrg.Kanal[i].Eingang.asBoolean) then begin
          Self.AddCol(FThisMrg.Kanal[i].Kanalname.asString + ' EZ [' +
            FThisMrg.Kanal[i].Einheit.asString + ']', iTyp);
          Self.GetCol(Self.Count-1).Kontroll := False;
          FKanalList.Add(FThisMrg.Kanal[i]);
          StatusListe.Add(TStringList.Create);  // KanalStatus
        end;
        if (FThisMrg.Kanal[i].Kontroll.asBoolean) then begin
          Self.AddCol(FThisMrg.Kanal[i].Kanalname.asString + ' KZ [' +
            FThisMrg.Kanal[i].Einheit.asString + ']', iTyp);
          Self.GetCol(Self.Count-1).Kontroll := True;
          FKanalList.Add(FThisMrg.Kanal[i]);
          StatusListe.Add(TStringList.Create);  // KanalStatus
        end;
      end
      else begin
        Self.AddCol(FThisMrg.Kanal[i].Kanalname.asString + ' [' +
          FThisMrg.Kanal[i].Einheit.asString + ']', iTyp);
        if (FValueType = vt_Physikalisch) and (iTyp = C_SpaltenTyp_Messwert)
        then TKanalListe(Self.GetCol(Self.Count-1)).ValNotValid :=
          C_Value_NotValid;
        FKanalList.Add(FThisMrg.Kanal[i]);
        StatusListe.Add(TStringList.Create);  // KanalStatus
      end;
    end;
  end;

  if (StundenWerte) then LoadStundenwerte else LoadTagessaetze;

  inherited;
end;

{------------------------------------------------------}
function TMrgAbrufDaten.GetValueStr(
  pKanal: TMyMSNewKanal; fValue: Double): string;
{------------------------------------------------------}
var
  f : Double;
begin
  case FValueType of
    vt_Rohwert:
      begin
        if (pKanal.KanalTyp = KTypAnalog)
        then Result := FormatFloat(MWFormat, fValue)
        else Result := FormatFloat(ZSFormat, fValue);
//        Result := Format ('%.0f', [fValue]);
      end;
    vt_Physikalisch:
      begin
        if (pKanal.KanalTyp = KTypAnalog) then begin
          with pKanal.Analog do
          begin
            if (not Offset.asBoolean) or (Strombereich.asInteger = 0) then
              f := fValue
            else begin
              f := fValue - 2000;
              f := f*10000/8000;
            end;
            if (f >= 0) and (f <= 10000) then begin
            { Wert ist gültig }
              f := MessbereichMin.asFloat +
                ((MessbereichMax.asFloat - MessbereichMin.asFloat) /
                10000 * f);
            end
            else begin
            { Wert ist ungültig }
              Result := IntToStr(C_Value_NotValid);
              exit;
            end;
          end;
          Result := FormatFloat(MWFormat, f);
        end
        else begin
          if (StundenWerte)
          then f := pKanal.Impuls.OrgFaktor.asFloat * fValue
          else f := fValue;
          Result := FormatFloat(ZSFormat, f);
        end;

//        Result := Format ('%.' + pKanal.KommaStellen.asString + 'f', [f]);
      end;
  end;
end;

{------------------------------------------------------}
function TMrgAbrufDaten.GetKanalStatusStr(
  pKanal: TMyMSNewKanal; iKanalStatus: Byte): string;
{------------------------------------------------------}
begin
  if (StundenWerte) then begin
    if (pKanal.KanalTyp = KTypAnalog)
    then Result := GetAnalogKanalStatusText(iKanalStatus)
    else Result := GetImpulsKanalStatusText(iKanalStatus);
  end
  else Result := GetKanalStatusText(iKanalStatus);  // 31.07.2003
end;

{------------------------------------------------------}
function TMrgAbrufDaten.GetKanalStatusText (KanalStatus: Byte): string;
{------------------------------------------------------}
var
  i: Integer;
  First: Boolean;
begin
  Result := '';
  First := True;
  for i := 0 to 7 do
  begin
    if (KanalStatus and CKanalMaske and (1 shl i)) <> 0 then
    begin
      if not First then
        Result := Result + ', '
      else
        First := False;
      Result := Result + strPas (CKanalStatus [i]);
    end;
  end;
end;

{------------------------------------------------------}
function TMrgAbrufDaten.GetImpulsKanalStatusText (KanalStatus: Byte): string;
{------------------------------------------------------}
var
  i: Integer;
  First: Boolean;
begin
  Result := '';
  First := True;
  for i := 0 to 7 do
  begin
    if (KanalStatus and CImpulsMaske and (1 shl i)) <> 0 then
    begin
      if not First then
        Result := Result + ', '
      else
        First := False;
      Result := Result + strPas (CImpulsKanalStatus [i]);
    end;
  end;
end;

{------------------------------------------------------}
function TMrgAbrufDaten.GetAnalogKanalStatusText (KanalStatus: Byte): string;
{------------------------------------------------------}
var
  i: Integer;
  First: Boolean;
begin
  Result := '';
  First := True;
  for i := 0 to 7 do
  begin
    if (KanalStatus and CAnalogMaske and (1 shl i)) <> 0 then
    begin
      if not First then
        Result := Result + ', '
      else
        First := False;
      Result := Result + strPas (CAnalogKanalStatus [i]);
    end;
  end;
end;

{------------------------------------------------------}
function TMrgAbrufDaten.GetSatzStatusText (SatzStatus: Byte): string;
{------------------------------------------------------}
var
  i: Integer;
  First: Boolean;
begin
  Result := '';
  First := True;
  for i := 0 to 7 do
  begin
    if (SatzStatus and (1 shl i)) <> 0 then
    begin
      if not First then
        Result := Result + ', '
      else
        First := False;
      Result := Result + strPas (CSatzStatus [i]);
    end;
  end;
end;

{ Entfernt überzählige Daten, passt Parameter an       }
{------------------------------------------------------}
procedure TMrgAbrufDaten.AutoDelete;
{------------------------------------------------------}
begin
  inherited;

  if (Count > 0) and (GetCol(0).Count > 0) then begin
    DatumVon := GetCol(C_Index_DatumZeit).AsDateTime[0];
    DatumBis := GetCol(C_Index_DatumZeit).AsDateTime[GetCol(0).Count-1];
  end
  else begin
    DatumVon := 0;
    DatumBis := 0;
    OldVon := 0;
    OldBis := 0;
  end;
end;

end.
