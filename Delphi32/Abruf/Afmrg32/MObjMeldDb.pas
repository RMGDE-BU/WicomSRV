{*************************************************************************************}
{* Unit: MRG-Meldungskonvertierung und -verarbeitung mit Datenbank-Zugriff           *}
{* 03.03.2003 WW                                                                     *}
{ 10.01.2007  GD  Abhhängigkeit von EXPORTSKRIPT.PAS entfernt                  }
{*************************************************************************************}
Unit MObjMeldDB;

INTERFACE

Uses
  Windows, Forms, SysUtils, Classes, DBTables, DB, MObjMeld, MObjList, MeldKonfigDb,
  MDBSta, PathIni, WStrUtils, MrgMeldExportList, WSysCon, MeldungenDB,
  WResMeld;


Type

  { Liste von Meldungen, Zugriff auf Datenbank }

  TMeldungsListeDB = class (TMeldungsListe)
  private
    Abrufart: TAbrufart;           { automatisch/manuell }
    StationID : TMrgId;            { aktuelle Station }
    ListIndex_erste_neue_Meldung : Integer;
    FAnzahlNeueMeldungen: integer; { Anzahl der neu in Tabelle eingefügten Meldungen }
    procedure TransformNrAllgByDB (MeldKonv: TMeldKonv);
  public
    constructor Create (KonfigPfad: string);
    procedure SetStation_Abrufart (AStationID: integer; AAbrufart: TAbrufart);
    function LoadFromFile (FileName: TFileName; MeldKonv: TMeldKonv;
                           ConfigFromDB: boolean): boolean;

    procedure SaveToMeldungenDb (BufLen: integer);
    Procedure SaveToExcel (ExcelCaption: string; ConfigFromDB: boolean);

    function VonDatum: TDateTime;
    function ExportList: TMrgMeldExportList;

    property AnzahlNeueMeldungen: integer read FAnzahlNeueMeldungen;
  end;

IMPLEMENTATION

{ TMeldungsListeDB }

{-------------------------------------------------------}
Constructor TMeldungsListeDB.Create (KonfigPfad: string);
{-------------------------------------------------------}
Begin
  Inherited Create (KonfigPfad);

  StationID := 0;
  Abrufart:=aa_automatisch;
  ListIndex_erste_neue_Meldung := 0;
  FAnzahlNeueMeldungen:=0;            { Vorbelegung: keine neuen Meldungen abgespeichert }
End;

{ SetStation : Setzen der StationsId und Abrufart (automatisch/manuell), welche
               für LGZ-Zugriffe (Konvertierung, ASCII-Export) benötigt werden }
{--------------------------------------------------------------------}
procedure TMeldungsListeDB.SetStation_Abrufart (AStationID: integer;
                                                AAbrufart: TAbrufart);
{--------------------------------------------------------------------}
begin
  StationID := AStationID;
  Abrufart:=AAbrufart;
end;

{-------------------------------------------------------------------------------}
function TMeldungsListeDB.LoadFromFile (FileName: TFileName; MeldKonv: TMeldKonv;
                                        ConfigFromDB: boolean): boolean;
{-------------------------------------------------------------------------------}
{ Konvertieren der Meldungen aus Rohdatenfile in Meldungsliste und Ermitteln der
  allgemeinen Meldungsnummern wahlweise über Datenbank-Tabellen oder Resourcendatei;
  Übergabe: Rohfilename
            Record mit Angaben für MRG-Meldungskonvertierung
            Flag ConfigFromDB (wenn true, werden Meldungs-Konfigurationsdaten aus
                               Datenbank-Tabellen gelesen, sonst aus Resourcendatei)
  Ergebnis: true, wenn Laden der Meldungen OK }
begin
  ListIndex_erste_neue_Meldung := 0;
  FAnzahlNeueMeldungen:=0; { Vorbelegung: keine neuen Meldungen abgespeichert }

  Result:=KonvRohdaten (FileName, MeldKonv);
  if ConfigFromDB then
    TransformNrAllgByDB (MeldKonv)
  else
    TransformNrAllgByResourcefile (MeldKonv);
end;

{ Ermitteln der allgemeinen Meldungs- und Parameternummern jedes Eintrages in der
  Liste aus Datenbank-Tabelle }
{-------------------------------------------------------------------}
procedure TMeldungsListeDB.TransformNrAllgByDB (MeldKonv: TMeldKonv);
{-------------------------------------------------------------------}
var
  MeldKonfigurationDb: TMeldKonfigurationDb;
  ParamMRGTable: TTable;
  MNrGeraet: string [szLen_MNrGeraet];
  MNrAllg: string;
  i: integer;
  Meldung: TMeldung;
  ParaNr_MRG: string [szLen_ParaNrMrg];
  ParaNr_Allg: string;

begin
  if FileExists(PathServer.PathName [WStammDir] + CDBParamMRG) then begin
    MeldKonfigurationDb:=TMeldKonfigurationDb.Create (PathServer.PathName [WStammDir]);
    try
      ParamMRGTable:=TTable.Create (nil);
      try
        ParamMRGTable.DatabaseName:=PathServer.PathName[WStammDir];
        ParamMRGTable.TableName:=CDBParamMRG;
        ParamMRGTable.IndexName:=CSPara3To5;
        if MeldKonfigurationDb.OpenMeldNrTable then begin
          try
            ParamMRGTable.Open;
            try
              for i:=0 to Count-1 do begin
                Application.ProcessMessages;
                Meldung:=TMeldung (Items [i]);
                MNrGeraet:=Meldung.Vz + Meldung.NrMrg;
                { allgemeine Meldungsnummer ermitteln: }
                if MeldKonfigurationDb.GetAllgMeldNr (C_GerArtMrg, MeldKonv.Meldungsgruppe,
                                                      MNrGeraet, false, MNrAllg) then
                  Meldung.SetNrAllg (MNrAllg);
                { bei Parameteränderungen: allgemeine Parameternummer ermitteln }
                if Meldung.GetParaChange <> nil then begin
                  ParaNr_MRG:=Meldung.GetParaChange.NrMrg;
                  if ParamMrgTable.FindKey ([MeldKonv.Parametergruppe, ParaNr_MRG]) then begin
                    ParaNr_Allg:=ParamMrgTable.FieldByName(C_ParamMRG_Parameternummer).AsString;
                    Meldung.GetParaChange.SetNrAllg (ParaNr_Allg);
                  end;
                end;
              end;
            finally
              ParamMrgTable.Close;
            end;
          finally
            MeldKonfigurationDb.CloseMeldNrTable;
          end;
        end;  { if MeldKonfigurationDb.OpenMeldNrTable }
      finally
        ParamMrgTable.Free;
      end;
    finally
      MeldKonfigurationDb.Free;
    end;
  end;
end;

{-------------------------------------------------------------}
procedure TMeldungsListeDB.SaveToMeldungenDb (BufLen: integer);
{-------------------------------------------------------------}
{ Aufbereitung der in der Meldungsliste enthaltenen Meldungs/Parameteränderungsobjekte
  in die Meldungs-Tabellen;
  Übergabe: BufLen (Rundpuffergröße für Automatik-Meldungen einer Station; wenn
                    BufLen = 0, dann erfolgt keine Organisation der Tabelle als
                    Rundpuffer) }
var
  MeldungenDb: TMeldungenDb;
  MeldKonfigurationDb: TMeldKonfigurationDb;
  MeldTextDataDB: TMeldTextDataDB;
  MeldungenData: TMeldungenData;
  MeldungenParaData: TMeldungenParaData;
  MeldungenAlarmData: TMeldungenAlarmData;
  Meldung: TMeldung;
  ParaChange: TParaChange;
  ABenutzer: string;
  AMText: string;
  AMTyp: string;
  AMArt: string;
  LetztDatumZeit: TDateTime;
  LetztMNrAllg: string;
  i : integer;

begin
  if Count <= 0 then exit;

  if Abrufart = aa_manuell then
    ABenutzer:=C_BenutzerManu
  else
    ABenutzer:=C_BenutzerAuto;

  MeldungenDb:=TMeldungenDb.Create (PathServer.PathName [WStammDir]);
  try
    { manueller Abruf: alle manuellen Meldungen der Station löschen }
    if Abrufart = aa_manuell then
      MeldungenDb.DeleteMRGMeldungen (ABenutzer, StationId);

    { ermitteln der letzten in der Tabelle vorhandenen Meldung der
      Station und damit den Listenindex der ersten neuen Meldung: }
    ListIndex_erste_neue_Meldung:=0;
    if MeldungenDb.GetLetztMRGMeldung (ABenutzer, StationID, LetztDatumZeit, LetztMNrAllg) then begin
      { die erste "gleiche" Meldung (Zeit und Nummer) in der Liste finden: }
      for i:=0 to Count - 1 do begin
        Meldung:=TMeldung (Items [i]);
        if Meldung.IsEqual (LetztDatumZeit, LetztMNrAllg, true) then begin
          ListIndex_erste_neue_Meldung:=i + 1;
          Break;
        end;
      end;

      { jetzt noch alle zeitgleichen Meldungen in der Liste überspringen, um
        Mehrfacheinträge in der Tabelle zu vermeiden: }
      for i:=ListIndex_erste_neue_Meldung to Count - 1 do begin
        Meldung := TMeldung (Items [i]);
        if not Meldung.IsEqual (LetztDatumZeit, '', false) then
          break
        else
          inc (ListIndex_erste_neue_Meldung);
      end;
    end;

    if MeldungenDb.OpenMeldungenTable (true) then begin   { Exklusiv öffnen wegen neuen }
      try                                                 { Datensätzen mit Zählfeld }
        if MeldungenDb.OpenMeldungenParaTable (true) then begin  { Exlusiv öffnen zur Sicherheit }
          try
            if MeldungenDb.OpenMeldungenAlarmTable (true) then begin  { Exlusiv öffnen zur Sicherheit }
              try
                MeldKonfigurationDb:=TMeldKonfigurationDb.Create (PathServer.PathName [WStammDir]);
                try
                  if MeldKonfigurationDb.OpenMeldNrStationTable AND
                     MeldKonfigurationDb.OpenMeldTextStationTable AND
                     MeldKonfigurationDb.OpenMeldTextTable then begin
                    { Konvertieren der neuen Meldungen aus Liste in Tabelle }
                    for i:=ListIndex_erste_neue_Meldung to Count - 1 do begin
                      Application.ProcessMessages;
                      Meldung:=TMeldung (Items [i]);
                      { Meldungstext und -typ aus Meldungskonfigurationstabellen
                        ermitteln: }
                      if MeldKonfigurationDb.GetMeldungData (C_GerArtMrg,
                                                             StationID,
                                                             Meldung.NrAllg,
                                                             false,
                                                             MeldTextDataDB) then begin
                        AMText:=MeldTextDataDB.MText;
                        AMTyp:=MeldTextDataDB.MTyp;
                        AMArt:=MeldTextDataDB.MArt;
                      end
                      else begin
                        AMText:='unbekannt';
                        AMTyp:=mtyp_unbestimmt;
                        AMArt:='';
                      end;

                      { Meldung wird in neuem Datensatz zur Haupt-Tabelle hinzugefügt }
                      with MeldungenData do begin
                        MeldungId:=-1;                 { wird von Tabellen-Zählfeld vergeben }
                        Benutzer:=ABenutzer;
                        GeraeteArt:=C_GerArtMrg;
                        GeraeteId:=StationID;
                        InstanzId_Archiv:=-1;          { wird bei MRG nicht verwendet }
                        LogbuchNr_Archiv:=-1;          { wird bei MRG nicht verwendet }
                        OrdnungsNr:=-1;                { wird bei MRG nicht verwendet }
                        DatumZeit:=EncodeDate (Meldung.Jahr, Meldung.Monat, Meldung.Tag) +
                                   EncodeTime (Meldung.Stunde, Meldung.Minute, Meldung.Sekunde, 0);
                        Zeitzone:='';
                        MNrAllg:=Meldung.NrAllg;
                        MNrGeraet:=Meldung.Vz + Meldung.NrMrg;
                        MText:=AMText;
                        MTyp:=AMTyp;
                        MArt:=AMArt;
                        Quittiert:=false;
                        Bemerkung:='';
                        MeldungId:=MeldungenDb.WriteMRGMeldung (MeldungenData);
                        inc (FAnzahlNeueMeldungen);
                      end; { with }

                      if MeldungenData.MeldungId > 0 then begin   { Hauptsatz konnte geschrieben werden }
                        { bei Parameteränderung, Daten in Detail-Tabelle für
                          Parameteränderungen schreiben: }
                        ParaChange:=Meldung.GetParaChange;
                        if ParaChange <> nil then begin
                          MeldungenParaData.MeldungId:=MeldungenData.MeldungId;
                          MeldungenParaData.ParaNrAllg:=ParaChange.NrAllg;
                          MeldungenParaData.WertAlt:=ParaChange.OldValue;
                          MeldungenParaData.WertNeu:=ParaChange.NewValue;
                          MeldungenDb.WriteMeldungPara (MeldungenParaData);
                        end;
                        { Alarm-Detailtabelle schreiben:
                          -> alle Alarme auf "false" bedeutet, daß die Alarme noch
                             nicht ausgeführt wurden }
                        MeldungenAlarmData.MeldungId:=MeldungenData.MeldungId;
                        MeldungenAlarmData.PCAlarm:=false;
                        MeldungenAlarmData.RelaisAlarm:=false;
                        MeldungenAlarmData.VoiceAlarm:=false;
                        MeldungenAlarmData.Gedruckt:=true;   { nix wird gedruckt }
                        MeldungenDb.WriteMeldungAlarm (MeldungenAlarmData);
                      end;
                    end; { for }
                  end;
                finally
                  MeldKonfigurationDb.CloseMeldNrStationTable;
                  MeldKonfigurationDb.CloseMeldTextStationTable;
                  MeldKonfigurationDb.CloseMeldTextTable;
                  MeldKonfigurationDb.Free;
                end;
              finally
                MeldungenDb.CloseMeldungenAlarmTable (true);  { Close und zur Sicherheit Flush }
              end;
            end;   { if Meldungen.OpenMeldungenAlarmTable }
          finally
            MeldungenDb.CloseMeldungenParaTable (true);   { Close und zur Sicherheit Flush }
          end;
        end;   { if Meldungen.OpenMeldungenParaTable }
      finally
        MeldungenDb.CloseMeldungenTable (true);   { Close und zur Sicherheit Flush }
      end;
    end;  { if Meldungen.OpenMeldungenTable }

    { Automatischer Abruf: Meldungs-Tabellen als Rundpuffer organisieren }
    if Abrufart <> aa_manuell then
      MeldungenDb.ReorganizeMRGMeldungen (StationId, BufLen);
  finally
    MeldungenDb.Free;
  end;
end;

{-----------------------------------------------------------------------------------}
Procedure TMeldungsListeDB.SaveToExcel (ExcelCaption: string; ConfigFromDB: boolean);
{-----------------------------------------------------------------------------------}
{ Meldungen aus Meldungsliste in Excel konvertieren;
  Konfigurationsdaten-Zugriff wahlweise über Datenbank-Tabellen oder Resourcendatei
  Übergabe: Titel für Excel-Tabelle
            Flag ConfigFromDB (wenn true, werden Meldungstexte aus
                               Datenbank-Tabelle gelesen, sonst aus Resourcendatei) }
var
  MeldTextKonfigList: TMeldTextKonfigList;
  MeldKonfigurationDb: TMeldKonfigurationDb;

begin
  MeldTextKonfigList:=TMeldTextKonfigList.Create;
  try
    if ConfigFromDB then begin
      MeldKonfigurationDb:=TMeldKonfigurationDb.Create (PathServer.PathName [WStammDir]);
      try
      { Meldungstext-Konfigurationsliste aus Tabelle laden (die StationId wird
        für evtl. vorhandene stationspezifische Meldungstexte benötigt !): }
        MeldKonfigurationDb.GetMeldText_KonfigList (C_GerArtMrg, StationId, MeldTextKonfigList);
      finally
        MeldKonfigurationDb.Free;
      end;
    end else
      { Meldungstext-Konfigurationsliste aus Resourcendatei laden: }
      GetMeldText_KonfigList (MeldTextKonfigList, KonfigPfad);

    { Meldungslisten-Einträge mit Meldungstexten in Excel-Blatt schreiben: }
    WriteExcelSheet (ExcelCaption, MeldTextKonfigList);
  finally
    MeldTextKonfigList.Free;
  end;
end;

{ Datum der ersten neuen Meldung                }
{ wird benötigt für ASCII-Export                }
{ Rückgabe: Datum-Zeit                          }
{--------------------------------------------}
function TMeldungsListeDB.VonDatum: TDateTime;       // // 12.02.2001
{--------------------------------------------}
var
  Benutzer: string;
  DatumZeit: TDateTime;
  MeldungenDb: TMeldungenDb;
  dummy: string;

begin
  Result := 0;  // Default

  if Abrufart = aa_manuell then
    Benutzer:=C_BenutzerManu
  else
    Benutzer:=C_BenutzerAuto;

  MeldungenDb:=TMeldungenDb.Create (PathServer.PathName [WStammDir]);
  try
    if MeldungenDb.GetLetztMRGMeldung (Benutzer, StationID, DatumZeit, dummy) then
      Result:=DatumZeit + EncodeTime(0, 0, 1, 0);
  finally
    MeldungenDb.Free;
  end;
end;

{ Erzeugt eine Liste für ASCII-Export           }
{ Rückgabe: Meldungsexportliste                 }
{-----------------------------------------------}
function TMeldungsListeDB.ExportList: TMrgMeldExportList;  // 12.02.2001
{-----------------------------------------------}
var
  i : integer;
  dt : TDateTime;
  MeldTextDataDB: TMeldTextDataDB;

begin
  Result := TMrgMeldExportList.Create;

  with TMeldKonfigurationDb.Create (PathServer.PathName [WStammDir]) do
  try
    if OpenMeldNrStationTable AND
       OpenMeldTextStationTable AND
       OpenMeldTextTable then begin
      for i := 0 to Self.Count-1 do begin
        Application.ProcessMessages;
        // Überprüfung auf gültiges Datum
        try
          dt := (EncodeDate(TMeldung(Self.Items[i]).Jahr,
            TMeldung(Self.Items[i]).Monat, TMeldung(Self.Items[i]).Tag) +
            EncodeTime(TMeldung(Self.Items[i]).Stunde,
            TMeldung(Self.Items[i]).Minute, TMeldung(Self.Items[i]).Sekunde, 0));
        except
          dt := 0;
        end;

        // Meldungs-Parameter in Liste eintragen
        MeldTextDataDB.MText := 'unbekannt';  // Default-Meldung     21.03.2001
        MeldTextDataDB.MTyp := mtyp_unbestimmt;
        GetMeldungData(C_GerArtMrg, Self.StationID, TMeldung(Self.Items[i]).NrAllg, false,
                       MeldTextDataDB);

        Result.InsertMeldung(
          TMeldung(
            Self.Items[i]).Vz + TMeldung(Self.Items[i]).NrMrg,  // 21.03.2001
          TMeldung(Self.Items[i]).NrAllg,           // 21.03.2001
          dt, MeldTextDataDB.MText, (MeldTextDataDB.MTyp <> mtyp_geht));
      end;
    end;
  finally
    CloseMeldNrStationTable;
    CloseMeldTextStationTable;
    CloseMeldTextTable;
    Free;
  end;
end;

End.

