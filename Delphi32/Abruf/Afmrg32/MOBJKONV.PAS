{******************************************************************************}
{* Unit: Objekte zur MRG-Datenkonvertierung                                   *}
{* 01.12.98 WW                                                                *}
{* 12.02.2001 GD  erweitert um automatischen Export                           *}
{******************************************************************************}
unit MObjKonv;

INTERFACE

Uses
  Classes, SysUtils, MDBSta, MFileNam, MObjList, MObjMeld, MObjMeldDb, MObjParaDB,
  MDBMRG, MLGZKonv, MLGZKonvList, MLGZAufb, MSysCon, ZBDatDB, T_Zeit, MJournal,
  ErrConst, MSysDat, My_Utils, PathIni, WSysCon, AsciiExportDLL, ExportSkript
{$IFDEF GVT}
  , GVTMKonv
{$ENDIF}
  ;

type

  { Objekt zur Konvertierung der Daten eines MRG }

  TMrgKonvert = class (TObject)
  private
    MrgId: TMrgId;
    MrgTyp: TMrgTyp;
    JournalId: integer;
    Abrufart: TAbrufart;
    ParameterAbspeichern: boolean;    { true, wenn ParameterListe in Tabelle abgespeichert
                                        werden soll }
    MessRohFileNameListe: TTextListe; { je Abruf können mehrere Messwert-Rohdateien entstanden sein }
    TagRohFileName: TFileName;
    MeldRohFileName: TFileName;
    PruefRohFileName: TFileName;
    MeldungsListe: TMeldungsListeDB;
    ParameterListe: TParameterListeDB;
    procedure Check_MWZeitbereich (MWDataInfo: TDataKonvInfo);
  public
    constructor Create (AMrgId: TMrgId; AMrgTyp: TMrgTyp; AAbrufart: TAbrufart; AJournalId: integer);
    destructor Destroy; override;
    procedure AddFileName (FileName: TFileName; Datenart: integer);
    function ParamKonvert: boolean;
    function MeldKonvert: boolean;
    procedure MessKonvert;
    procedure PruefKonvert;
  end;

  { Objekt zur Konvertierung der Daten eines Stationsabrufs
    (bei DSfG-Umleitung mehrere MRGs !) }

  TAbrufKonvert = class (TObject)
  private
    Abrufart: TAbrufart;
    MrgKonvertList: TList;
  public
    constructor Create (AAbrufart: TAbrufart);
    destructor Destroy; override;
    procedure AddParameter (MrgId: TMrgId; MrgTyp: TMrgTyp;
                            ParameterListe: TParameterListeDB;
                            JournalId: integer; Abspeichern: boolean);
    procedure AddFileName (MrgId: TMrgId; MrgTyp: TMrgTyp;
                           FileName: TFileName; Datenart: integer;
                           JournalId: integer);
    procedure ParamKonvert;
    procedure MeldKonvert;
    procedure MessKonvert;
    procedure PruefKonvert;
  end;


IMPLEMENTATION

{ TMrgKonvert }

{-----------------------------------------------------------------------------------------------------------}
constructor TMrgKonvert.Create (AMrgId: TMrgId; AMrgTyp: TMrgTyp; AAbrufart: TAbrufart; AJournalId: integer);
{-----------------------------------------------------------------------------------------------------------}
begin
  inherited Create;
  MrgId := AMrgId;
  MrgTyp := AMrgTyp;
  Abrufart:=AAbrufart;
  JournalId:=AJournalId;
  MeldungsListe := TMeldungsListeDB.Create (PathServer.PathName [WStammDir]);
  MeldungsListe.SetStation_Abrufart (MrgId, AAbrufart);
  ParameterListe := TParameterListeDB.Create (PathServer.PathName [WStammDir]);
  MessRohFileNameListe := TTextListe.Create;
  TagRohFileName:='';
  MeldRohFileName:='';
  PruefRohFileName:='';
  ParameterAbspeichern:=false;
end;

{-----------------------------}
destructor TMrgKonvert.Destroy;
{-----------------------------}
begin
  MessRohFileNameListe.Free;
  ParameterListe.Free;
  MeldungsListe.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------}
procedure TMrgKonvert.AddFileName (FileName: TFileName; Datenart: integer);
{-------------------------------------------------------------------------}
begin
  if FileName <> '' then
    case Datenart of
      dt_Messwerte: MessRohFileNameListe.Add (FileName);
      dt_Meldungen: MeldRohFileName := FileName;
      dt_Tagessaetze: TagRohFileName := FileName;
      dt_Pruefsaetze: PruefRohFileName := FileName;
    end; { case }
end;

{-----------------------------------------}
function TMrgKonvert.ParamKonvert: boolean;
{-----------------------------------------}
{ Aufruf der eigentlichen Routine zur Parameterkonvertierung
  Ergebnis: true, wenn konvertiert wurde }
begin
  if ParameterAbspeichern then begin
    ParameterListe.SaveToParamTable (MrgId);
    Result:=true;
  end else
    Result:=false;
end;

{----------------------------------------}
function TMrgKonvert.MeldKonvert: boolean;
{----------------------------------------}
{ Aufruf der eigentlichen Routinen zur Meldungskonvertierung
  Ergebnis: true, wenn konvertiert wurde }
var
  p : Pointer;
  MrgKonvData: TMrgKonvDataDb;
  MeldKonv: TMeldKonv;
{$IFDEF GVT}
  MeldKonvRec_GVT: TMeldKonvGVT;
  AufberRec: TAufberRec;
{$ENDIF}

begin
  Result:=false;
  if Length (MeldRohFileName) > 0 then begin
{$IFDEF GVT}
    if Abrufart <> aa_manuell then begin
      { spezielle Meldungskonvertierung in alte LGZ-Datenstruktur für GVT-Leitwarte: }
      MeldKonvRec_GVT.MrgId:=MrgId;
      with MeldKonvRec_GVT do begin
        QuelldateiName:=MeldRohFileName;
        ZieldateiName:=GetZwischenDateiName_MrgId (MrgId, dd_Meld);
        ParadateiName:=GetZwischenDateiName_MrgId (MrgId, dd_Para);
        loeschen:=RohdatenLoeschen;
      end;
      Meld_Konv_GVT (MeldKonvRec_GVT);                    { GVT-Konvertierung }

      AufberRec.MrgId:=MrgId;
      AufberRec.Select:=1;       { GVT-Aufbereitung immer in LGZ-Daten }
      AufberRec.Kanalzahl:=0;    { nicht verwendet }
      AufberRec.loeschen:=RohdatenLoeschen;
      Meld_Aufb_GVT (AufberRec);
    end;
{$ENDIF}

    { Konfigurationsdaten für Meldungskonvertierung lesen: }
    if GetMrgKonvDataDb (MrgTyp, MrgKonvData) then begin
      { Übergaberecord für Meldungskonvertierung zusammenstellen: }
      with MeldKonv do begin
        MeldKonvGruppe:=MrgKonvData.MeldKonvGruppe;
        MNrParaStart:=MrgKonvData.MNrParaStart;
        MNrParameter:=MrgKonvData.MNrParameter;
        MeldungsGruppe:=MrgKonvData.MeldungsGruppe;
        ParameterGruppe:=MrgKonvData.ParameterGruppe;
        RohLoeschen:=RohdatenLoeschen;
      end;
      MeldungsListe.LoadFromFile (MeldRohFileName, MeldKonv, true);
    end else
      WriteJournalFehler (JournalId, ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);

    { bei ungültigen Meldungs-Rohdaten Journaleintrag schreiben: }
    if not Meldungsliste.DataValid then
      WriteJournalFehler (JournalId, ST_DATACHECK, DCH_MEINVALID);

    if Abrufart <> aa_manuell then begin
      // ASCII-Export Meldungen
      p := MeldungsListe.ExportList;
      try
        NewASCIIExportObject(
          C_GerArtMrg, MrgId, aetMeldungen, p, MeldungsListe.VonDatum);
      finally
        TList(p).Free;
      end;
    end;
    { Meldungen in Tabelle abspeichern: }
    MeldungsListe.SaveToMeldungenDb (Systemdaten.GrMeldPuffer);
    Result:=true;
  end;
end;

{--------------------------------}
procedure TMrgKonvert.MessKonvert;
{--------------------------------}
{ Aufruf der eigentlichen Routinen zur Meßwert/Tagessatz-Konvertierung und Aufbereitung }
var
  MessTagKonvRec: TMessTagKonv;
  AufberRec: TAufberRec;
  i: integer;
  MWDataInfo, MWDataInfoTemp: TDataKonvInfo;
  ManuAppend: boolean;
  dummy: TDateTime;
  Dummy2: TDataKonvInfo;
  fertig: boolean;
  Stammdaten: TMRGStammdaten;
  StaData: TStaData;
  MrgKonvData: TMrgKonvDataDb;
  KanalListObj: TKanalListObj;
  k: integer;
  StaKanalKonvDataList: TStaKanalKonvDataList;
  StaKanalKonvDataObj: TStaKanalKonvDataObj;
  StaKanalKonvData: TStaKanalKonvData;

begin
  { Vorbelegung für Datenbereich-Überprüfung: }
  with MWDataInfo do begin
    NeueDaten:=false;
    DatenFehlen:=false;
    WriteDatenzeitbereich_Ist (JournalId, 0, 0, SollVon, SollBis);   { nur Soll lesen }
    IstVon:=-1;
    IstBis:=-1;
  end;

  Stammdaten:=TMRGStammdaten.Create (PathServer.PathName [WStammDir]);
  try
    if Stammdaten.InitTabellen then begin
      if Stammdaten.GetStaData (MrgId, StaData) then begin
        if GetMrgKonvDataDb (MrgTyp, MrgKonvData) then begin
          KanalListObj:=TKanalListObj.Create (MrgId, PathServer.PathName [WStammDir]);
          try
            StaKanalKonvDataList:=TStaKanalKonvDataList.Create;
            try
              { Übergaberecord für Konvertierung: Kanalaktiv-Maske und Kanalliste zusammenstellen }
              MessTagKonvRec.KanalAktivMaske:='';
              for k:=1 to KanalListObj.GetKanalAnzahl do begin
                { Kanalaktiv-Maske: }
                if KanalListObj.IsAktiv (k) then
                  MessTagKonvRec.KanalAktivMaske:=MessTagKonvRec.KanalaktivMaske + '1'
                else
                  MessTagKonvRec.KanalAktivMaske:=MessTagKonvRec.KanalaktivMaske + '0';

                { Kanalliste: }
                with StaKanalKonvData do begin
                  KanalNr:=k;
                  { Vorbelegungen: }
                  OrgFaktor:=0;
                  MessBereichMin:=0;
                  MessBereichMax:=0;

                  if KanalListObj.GetKanalTyp (k) = kTyp_Impuls then
                    OrgFaktor:=KanalListObj.GetMRGKanal (k).Impulskanal.OrgFaktor
                  else if KanalListObj.GetKanalTyp (k) = kTyp_Analog then begin
                    MessBereichMin:=KanalListObj.GetMRGKanal (k).Analogkanal.MessBereichMin;
                    MessBereichMax:=KanalListObj.GetMRGKanal (k).Analogkanal.MessBereichMax;
                  end;
                end;
                StaKanalKonvDataObj:=TStaKanalKonvDataObj.Create (StaKanalKonvData);
                StaKanalKonvDataList.Add (StaKanalKonvDataObj);
              end;
              MessTagKonvRec.KanalList:=StaKanalKonvDataList;

              { alle Meßwertrohdateien konvertieren und aufbereiten:
                wenn mehrere Rohdateien existieren gibt es zwei Varianten:
                  1. Rohdateien aus Abrufen per DSfG-Umleitung werden nacheinander einzeln konvertiert, da
                     sie nur die Daten für einen begrenzten Zeitbereich (1 Tag) enthalten  (for i:=0 to...)
                  2. Rohdateien aus Abrufen, welche je Kanal eine eigene Rohdatei erzeugt haben, werden
                     alle mit einem einzigen Aufruf von 'MessTag_Konv' konvertiert. 'MessTag_Konv' muß in
                     diesem Fall das gesetzte Schleifenabbruch-Flag 'fertig' zurückgeben. }
              for i:=0 to MessRohFileNameListe.Count - 1 do begin
                with MessTagKonvRec do begin
                  KonvGruppe:=MrgKonvData.KonvGruppe;
                  Tagesende:=Stadata.Tagesende;
                  AnzEingZaehler:=KanalListObj.GetEingangszaehlerAnzahl;
                  AnzKontrZaehler:=KanalListObj.GetKontrollzaehlerAnzahl;
                  KanalListObj.GetAnalogkanal_von_bis (AnalogKanal_von, AnalogKanal_bis);
                  AufzMax_Analog:=KanalListObj.GetAnalogkanal_AufzMax;

                  StdQuellDateiName := MessRohFileNameListe [i];
                  StdQuellDateiNameListe := MessRohFileNameListe;  { für Konvertierungen nötig, welche mehrere Rohdateien
                                                                     parallel verarbeiten, z.B. Elster DL240 }
                  StdZielDateiName:=CreateZwischenDateiNameTemp (dd_Mess);  { Messwert-Zielfile mit eindeutigem Namen wird hier bereits erzeugt }
                  TagQuellDateiName := TagRohFileName;
                  TagZielDateiName:=CreateZwischenDateiNameTemp (dd_Tags);  { Tagessatz-Zielfile mit eindeutigem Namen wird hier bereits erzeugt }
                  Loeschen := RohdatenLoeschen;
                  if Abrufart = aa_manuell then
                    Select:=0                                     { Manudaten }
                  else
                    Select:=1;   { Automatikdaten (auch bei Rufentgegennahme) }
                  Sta_Stationsname:=StaData.Stationsname;
                  bAktZaehler:=false;  // aktuelle ZS nicht konvertieren (VC2); 16.05.2007, WW
                end; { with MessTagKonv }
                MessTagKonvRec.ParameterListe := ParameterListe;
                MessTag_Konv(MessTagKonvRec, fertig);                           { Konvertierung }

                { Aufbereitung: }
                AufberRec.StdQuelldateiName:=MessTagKonvRec.StdZielDateiName;
                AufberRec.TagQuelldateiName:=MessTagKonvRec.TagZielDateiName;
                AufberRec.PrfQuelldateiName:='';
                AufberRec.MrgId:=MrgId;
                if Abrufart = aa_manuell then
                  AufberRec.Select:=0                                         { Manudaten }
                else
                  AufberRec.Select:=1;       { Automatikdaten (auch bei Rufentgegennahme) }
                AufberRec.Kanalzahl:=V_maxKanalZahl;
                AufberRec.loeschen:=RohdatenLoeschen;
                ManuAppend:=i > 0;    { Manu-Abruf mit mehr als 1 Rohfile: ab dem 2. Rohfile appenden ! }
                Mess_Aufb (AufberRec, ManuAppend, MWDataInfoTemp, Dummy2);

                { Protokollierung der aufbereiteten Messwert-Daten in Datenzeitbereich-Tabelle: }
                if MWDataInfoTemp.NeueDaten then        { mind. 1 korrekter Messwert wurde geschrieben }
                  WriteDatenzeitbereich_Ist (JournalId, MWDataInfoTemp.IstVon, MWDataInfoTemp.IstBis,
                                             dummy, dummy);       { Ist-Bereich schreiben }

                if MWDataInfoTemp.NeueDaten then MWDataInfo.NeueDaten:=true;
                if MWDataInfoTemp.DatenFehlen then MWDataInfo.DatenFehlen:=true;
                with MWDataInfo do begin
                  if IstVon < 0 then                      { nie überschreiben ! }
                    IstVon:=MWDataInfoTemp.IstVon;
                  if MWDataInfoTemp.IstBis > IstBis then  { nie späteren Zeitpunkt überschreiben ! }
                    IstBis:=MWDataInfoTemp.IstBis;
                end;

                if fertig then Break;
              end;  { for }

              { Soll-Ist-Zeitbereich der abgerufenen Meßwerte prüfen, wenn MW-Rohfile(s) vorhanden: }
              if MessRohFileNameListe.Count > 0 then
                Check_MWZeitbereich (MWDataInfo);
            finally
              StaKanalKonvDataList.Free;
            end;
          finally
            KanalListObj.Free;
          end;
        end;    { if GetMrgKonvData }
      end;   { if MRGStammdaten.GetStaData }
    end;  { if MRGStammdaten.InitTabellen }
  finally
    Stammdaten.Free;
  end;
end;

{---------------------------------}
procedure TMrgKonvert.PruefKonvert;
{---------------------------------}
{ Aufruf der eigentlichen Routinen zur Prüfsatz-Konvertierung }
var
  PruefKonvRec: TPruefKonv;
  AufberRec: TAufberRec;

begin
  if Length (PruefRohFileName) > 0 then begin
    with PruefKonvRec do begin
      QuellDateiName := PruefRohFileName;
      ZielDateiName:=CreateZwischenDateiNameTemp (dd_Pruef);
      loeschen := RohdatenLoeschen;
    end;
    Pruef_konv (PruefKonvRec);                                { Konvertierung }

    AufberRec.StdQuelldateiName:='';
    AufberRec.TagQuelldateiName:='';
    AufberRec.PrfQuelldateiName:=PruefKonvRec.ZielDateiName;
    AufberRec.MrgId:=MrgId;
    if Abrufart = aa_manuell then
      AufberRec.Select:=0                                         { Manudaten }
    else
      AufberRec.Select:=1;       { Automatikdaten (auch bei Rufentgegennahme) }
    AufberRec.Kanalzahl:=V_maxKanalZahl;
    AufberRec.loeschen:=RohdatenLoeschen;
    Pruef_Aufb (AufberRec);                                    { Aufbereitung }
  end;
end;

{--------------------------------------------------------------------}
procedure TMrgKonvert.Check_MWZeitbereich (MWDataInfo: TDataKonvInfo);
{--------------------------------------------------------------------}
{ Soll-Ist-Zeitbereich der abgerufenen Meßwerte prüfen;
  Übergabe: MWDataInfo.NeueDaten = true, wenn mind. 1 "richtiger" LGZ-Satz geschrieben wurde
                        (nicht fehlend !)
            MWDataInfo.DatenFehlen = true, wenn mind. 1 "Fehlend"-LGZSatz geschrieben wurde
            MWDataInfo.Zeitbereich (enthält Soll-Ist-Zeitbereich) }
var
  NextSZToWZ: TDateTime;
  NextSZToWZHour: word;
  PCDateTime: TDateTime;
  PCHour: word;
  dummy1, dummy2, dummy3: word;

begin
  with MWDataInfo do begin
    if not DatenFehlen then begin            { keine Fehlend-Sätze, dann Zeitbereich prüfen }
      { in LGZ-Datei geschriebenen Ist-Zeitbereich mit Soll vergleichen: }
      if Abrufart <> aa_manuell then begin                     { stundengenau vergleichen }
        if NeueDaten then begin                    { es wurden neue LGZ-Daten geschrieben }
          if SollVon > 0 then begin        { mit SollVon-Vorgabe: SollVon, SollBis prüfen }
            if CmpDateTime (SollVon, SollBis) <= 0 then begin   { andernfalls keine neuen Daten zu erwarten }
              if (CmpDateTime (IstVon, SollVon) > 0) OR (CmpDateTime (IstBis, SollBis) < 0) then
                DatenFehlen:=true;                                         { "Ist < Soll" }
            end;
          end
          else begin                          { keine SollVon-Vorgabe: nur SollBis prüfen }
            if CmpDateTime (IstBis, SollBis) < 0 then                                             { "Ist < Soll" }
              DatenFehlen:=true;
          end;
        end
        else begin
          DatenFehlen:=true;
          if (SollVon > 0) AND (CmpDateTime (SollVon, SollBis) > 0) then { keine neuen Daten zu erwarten }
            DatenFehlen:=false;
        end;
      end

      else begin                                 { manueller Abruf: nur Datum vergleichen }
        if NeueDaten then begin                   { es wurden neue Manu-Daten geschrieben }
          if SollVon > 0 then begin
            if not (Int (SollVon) > Int (SollBis)) then begin { andernfalls keine neuen Daten zu erwarten }
              if (Int (IstVon) > Int (SollVon)) OR (Int (IstBis) < Int (SollBis)) then  { "Ist < Soll" }
                DatenFehlen:=true;
            end;
          end
          else begin                          { keine SollVon-Vorgabe: nur SollBis prüfen }
            if Int (IstBis) < Int (SollBis) then
              DatenFehlen:=true;
          end;
        end else
          DatenFehlen:=true;
      end;
    end; { if not DatenFehlen }

    { Protokollierung in Journal-Fehlertabelle bei fehlenden Daten: }
    if DatenFehlen then begin
      { keine Protokollierung zum Zeitpunkt der Umstellung von Sommer- auf Winterzeit wegen
        evtl. doppelt vorhandener Stunde: }
      PCDateTime:=Now;
      NextSZToWZ:=GetNextSZ_To_WZFromDB;
      DecodeTime (PCDateTime, PCHour, dummy1, dummy2, dummy3);
      DecodeTime (NextSZToWZ, NextSZToWZHour, dummy1, dummy2, dummy3);
      if not ((Int (PCDateTime) = Int (NextSZToWZ)) AND (PCHour = (NextSZToWZHour-1))) then
        WriteJournalFehler (JournalId, ST_DATACHECK, DCH_MWMISSING);
    end;
  end;
end;

{ TAbrufKonvert }

{------------------------------------------------------}
constructor TAbrufKonvert.Create (AAbrufart: TAbrufart);
{------------------------------------------------------}
begin
  inherited Create;
  Abrufart:=AAbrufart;
  MrgKonvertList := TList.Create;
end;

{-------------------------------}
destructor TAbrufKonvert.Destroy;
{-------------------------------}
var
  i: integer;
begin
  for i:=0 to MrgKonvertList.Count - 1 do
    TMrgKonvert (MrgKonvertList [i]).Free;
  MrgKonvertList.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
procedure TAbrufKonvert.AddParameter (MrgId: TMrgId; MrgTyp: TMrgTyp;
                                      ParameterListe: TParameterListeDB;
                                      JournalId: integer; Abspeichern: boolean);
{------------------------------------------------------------------------------}
var
  MrgKonvert: TMrgKonvert;
  i: integer;

begin
  MrgKonvert := nil;
  for i:=0 to MrgKonvertList.Count - 1 do begin
    if TMrgKonvert (MrgKonvertList [i]).MrgId = MrgId then begin
      MrgKonvert := TMrgKonvert (MrgKonvertList [i]);
      Break;
    end;
  end;
  if MrgKonvert = nil then begin
    MrgKonvert := TMrgKonvert.Create (MrgId, MrgTyp, Abrufart, JournalId);
    MrgKonvertList.Add (MrgKonvert);
  end;
  MrgKonvert.ParameterListe.LoadFromParameterlist (ParameterListe);
  MrgKonvert.ParameterAbspeichern:=Abspeichern;
end;

{ Fügt den übergebenen Dateinamen in das MrgKonvert-Objekt ein, welches die
  übergebene MrgId hat. Falls noch kein MrgKonvert-Objekt mit der entsprechenden MrgId
  existiert, wird vorher ein neues Objekt angelegt }
{--------------------------------------------------------------------------}
procedure TAbrufKonvert.AddFileName (MrgId: TMrgId; MrgTyp: TMrgTyp;
                                     FileName: TFileName; Datenart: integer;
                                     JournalId: integer);
{--------------------------------------------------------------------------}
var
  MrgKonvert: TMrgKonvert;
  i: integer;

begin
  MrgKonvert := nil;
  for i:=0 to MrgKonvertList.Count - 1 do begin
    if TMrgKonvert (MrgKonvertList [i]).MrgId = MrgId then begin
      MrgKonvert := TMrgKonvert (MrgKonvertList [i]);
      Break;
    end;
  end;
  if MrgKonvert = nil then begin
    MrgKonvert := TMrgKonvert.Create (MrgId, MrgTyp, Abrufart, JournalId);
    MrgKonvertList.Add (MrgKonvert);
  end;
  MrgKonvert.AddFileName (FileName, Datenart);
end;

{ Konvertiert alle Parameter aller MRGS des Stationsabrufs }
{-----------------------------------}
procedure TAbrufKonvert.ParamKonvert;
{-----------------------------------}
var
  i: integer;
begin
  for i:=0 to MrgKonvertList.Count - 1 do
    TMrgKonvert (MrgKonvertList [i]).ParamKonvert;
end;

{ Konvertiert alle Meldungen aller MRGS des Stationsabrufs }
{----------------------------------}
procedure TAbrufKonvert.MeldKonvert;
{----------------------------------}
var
  i: integer;
begin
  for i:=0 to MrgKonvertList.Count - 1 do
    TMrgKonvert (MrgKonvertList [i]).MeldKonvert;
end;

{ Konvertiert alle Meßwerte/Tagessätze aller MRGS des Stationsabrufs }
{----------------------------------}
procedure TAbrufKonvert.MessKonvert;
{----------------------------------}
var
  i: integer;
begin
  for i:=0 to MrgKonvertList.Count - 1 do
    TMrgKonvert (MrgKonvertList [i]).MessKonvert;
end;

{ Konvertiert alle Prüfungssätze aller MRGS des Stationsabrufs }
{-----------------------------------}
procedure TAbrufKonvert.PruefKonvert;
{-----------------------------------}
var
  i: integer;
begin
  for i:=0 to MrgKonvertList.Count - 1 do
    TMrgKonvert (MrgKonvertList [i]).PruefKonvert;
end;

end.
