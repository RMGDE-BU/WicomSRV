{******************************************************************************}
{* Unit: Zugriff auf MRG/DSfG-Meldungs-Konfigurationsdateien                  *}
{* 17.12.2002 WW  Neu                                                         *}
{* 06.08.2021 WW  Erweiterungen zum Laden und Suchen in Listen                *}
{* 29.09.2021 WW  mit Feld 'MAdrGeraet' in MeldNr.dat                         *}
{* 23.02.2024 WW  Anpassung an DSfG-Ereignisliste mit herstellerunabh�ngigem  *}
{*                Meldungsnummern-Bereich 1000-9999                           *}
{******************************************************************************}
unit WResMeld;

interface

uses
  Forms, Classes, Contnrs, SysUtils, WStrUtils, WResConst, WSysCon, WStream;

const
  { Dateinamen }
  CResMeldNr   = 'MeldNr.dat';
  CResMeldText = 'MeldText.dat';
  
type
  { Record f�r Meldungsnummern-Konfigurationsdaten }

  TMeldNrData = record
    GeraeteArt: string;
    MeldGrpNr: integer;
    MNrGeraet: string;                    
    MNrAllg: string;
    MAdrGeraet: string; // f�r Ereignisse, welche nicht �ber die MRG-Meldungsnummer
                        // im Ger�t adressiert werden (z.B. FLOWSIC500 interne EventID); ab 29.09.2021, WW
  end;    

  { Objekt f�r Meldungsnummern-Konfigurationsdaten }

  TMeldNrDataObj = class (TObject)
    Data: TMeldNrData;
  public
    procedure SetData (AMeldNrData: TMeldNrData);
  end;

  { Liste f�r Meldungsnummern-Konfigurationsdaten }

  TMeldNrKonfigList = class(TObjectList)
  public
    function LoadFromList_ByGerArt_MeldGrpNr (AGeraeteArt: string;
      AMeldGrpNr: integer; ListSrc: TMeldNrKonfigList): boolean;
    function FindMeldNrData (AGeraeteArt: string; AMeldGrpNr: integer;
      AMNrGeraet: string; var MeldNrData: TMeldNrData;
      bSearchMeldAdr_Geraet: boolean = false): boolean;
  end;

  { Record f�r Meldungstext-Konfigurationsdaten }

  TMeldTextData = record
    MNrAllg: string;
    MText: string;
    MArt: string;
    MTyp: string;
  end;

  { Objekt f�r Meldungstext-Konfigurationsdaten }

  TMeldTextDataObj = class (TObject)
    Data: TMeldTextData;
  public
    procedure SetData (AMeldTextData: TMeldTextData);
  end;

  { Liste f�r Meldungstext-Konfigurationsdaten }

  TMeldTextKonfigList = class(TObjectList)
  public
    function FindMeldText (AMNr_Allg: string; var MText: string; var MArt: string;
      var MTyp: string): boolean;
  end;

function GetMeldNr_KonfigList (AGeraeteArt: string; AMeldGrpNr: integer;
  MeldNrKonfigList: TMeldNrKonfigList; Pfad: string): boolean;
function GetMeldText_KonfigList (MeldTextKonfigList: TMeldTextKonfigList;
  Pfad: string): boolean;

implementation

{ MeldNrDataObj }

{----------------------------------------------------------}
procedure TMeldNrDataObj.SetData (AMeldNrData: TMeldNrData);
{----------------------------------------------------------}
begin
  Data:=AMeldNrData;
end;

{ MeldTextDataObj }

{----------------------------------------------------------------}
procedure TMeldTextDataObj.SetData (AMeldTextData: TMeldTextData);
{----------------------------------------------------------------}
begin
  Data:=AMeldTextData;
end;

{ TMeldNrKonfigList }

{------------------------------------------------------------------------------}
function TMeldNrKonfigList.LoadFromList_ByGerArt_MeldGrpNr (AGeraeteArt: string;
  AMeldGrpNr: integer; ListSrc: TMeldNrKonfigList): boolean;
{------------------------------------------------------------------------------}
{ L�dt MeldNrData-Eintr�ge zu Ger�teart und Meldungsgruppe aus Quell-Liste in die
  Liste;
  �bergaben: Ger�teart
             Meldungsgruppe
             Quell-Liste
  Ergebnis: true, wenn Quell-Liste nicht nil }
var
  i: integer;
  MeldNrDataObj: TMeldNrDataObj;
  MeldNrDataSrc: TMeldNrData;

begin
  Result:=false;
  Clear;  // Liste leeren

  if Assigned (ListSrc) then begin
    Result:=true;
    for i:=0 to ListSrc.Count - 1 do begin
      Application.ProcessMessages;
      MeldNrDataSrc:=TMeldNrDataObj (ListSrc.Items [i]).Data;
      if (MeldNrDataSrc.GeraeteArt = AGeraeteArt) then begin
        { bei DSfG zus�tzlich allgemeing�ltige Meldungsnummern-Bereiche 1-999
         (Meldungsgruppe 0) und 1000-9999 (Meldungsgruppe 1, DSfG-Ereignisliste
         ab 12/2023) mitladen: }
        if (MeldNrDataSrc.MeldGrpNr = AMeldGrpNr) OR
           ((AGeraeteArt = C_GerArtDSfG) AND
            (MeldNrDataSrc.MeldGrpNr in [0, 1])) then begin  // 23.02.2024, WW 
          { Listenobjekt createn und in Liste einf�gen: }
          MeldNrDataObj:=TMeldNrDataObj.Create;
          MeldNrDataObj.SetData (MeldNrDataSrc);
          Add (MeldNrDataObj);
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
function TMeldNrKonfigList.FindMeldNrData (AGeraeteArt: string;
  AMeldGrpNr: integer; AMNrGeraet: string; var MeldNrData: TMeldNrData;
  bSearchMeldAdr_Geraet: boolean = false): boolean;
{-------------------------------------------------------------------------------}
{ MeldNrData zu der �bergebenen Ger�teart, Meldungsgruppe und ger�tespezifischen
  Meldungsnummer in der Liste suchen;
  �bergaben: Ger�teart
             Meldungsgruppe
             ger�tespezifische Meldungsnummer
             Optional:
               Flag: Suchen nach ger�tespezifischer Meldungsnummer in Feld 'MAdrGeraet'
                     ja/nein
  R�ckgabe: MeldNrData-Struktur
  Ergebnis: true, wenn Eintrag gefunden wurde }
var
  MeldNrDataObj: TMeldNrDataObj;
  SearchMNrGeraet: string;
  i: integer;
  bFound: boolean;

begin
  Result:=false;
  { Vorbelegung: R�ckgabe }
  MeldNrData.GeraeteArt:='';
  MeldNrData.MeldGrpNr:=-1;
  MeldNrData.MNrGeraet:='';
  MeldNrData.MNrAllg:='';
  MeldNrData.MAdrGeraet:='';

  { Bei DSfG mu� der String der ger�tespezifischen Meldungsnummer f�r die
    Suche in der Tabelle ohne Vorzeichen sein und 4-stellig mit f�hrenden Nullen
    versehen werden. Bei MRG-Nummern sind keine Anpassungen n�tig: }
  SearchMNrGeraet:=AMNrGeraet;
  if AGeraeteArt = C_GerArtDSfG then begin
    SearchMNrGeraet:=F_LeftTrunc (SearchMNrGeraet, '+');  // 02.08.2006; WW
    SearchMNrGeraet:=F_LeftTrunc (SearchMNrGeraet, '-');
    if IsIntString (SearchMNrGeraet) then  // nur bei Zahlen; 16.06.2009, WW
      SearchMNrGeraet:=F_LeftPad (SearchMNrGeraet, '0', 4);
  end;

  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    MeldNrDataObj:=TMeldNrDataObj (Items [i]);
    { Suchen im der Ger�teart und Meldungsgruppe zugeordneten Meldungsnummern-
      Bereich: }
    if (MeldNrDataObj.Data.GeraeteArt = AGeraeteArt) AND
       (MeldNrDataObj.Data.MeldGrpNr = AMeldGrpNr) then begin
      if bSearchMeldAdr_Geraet AND
        (MeldNrDataObj.Data.MAdrGeraet <> '') then  // 29.09.2021, WW
        // Suchen nach ger�tespezifischer Meldungsnummer in Feld 'MAdrGeraet',
        // wenn eine ger�tespezifische Meldungsadresse definiert ist:
        bFound:=MeldNrDataObj.Data.MAdrGeraet = SearchMNrGeraet
      else
        // Suchen nach ger�tespezifischer Meldungsnummer in Feld 'MNrGeraet':
        bFound:=MeldNrDataObj.Data.MNrGeraet = SearchMNrGeraet;

      if bFound then begin
        MeldNrData:=MeldNrDataObj.Data;  // 29.09.2021, WW
        Result:=true;
        Break;
      end;
    end;
  end;  { for i }

  { Bei DSfG zus�tzlich in allgemeing�ltigen Meldungsnummern-Bereichen suchen;
    15.06.2009, WW }
  if (not Result) AND (AGeraeteArt = C_GerArtDSfG) then begin
    for i:=0 to Count - 1 do begin
      Application.ProcessMessages;
      MeldNrDataObj:=TMeldNrDataObj (Items [i]);

      { - Meldungsnummern-Bereich 1-999 (Meldungs-Gruppe 0)
        - DSfG-Ereignisliste ab 12/2023: Meldungsnummern-Bereich 1000-9999
          (Meldungs-Gruppe 1) -> nicht f�r DSfG-Ger�te der Meldungsgruppen 6 und 9, da
          diese Ger�te Meldungsnummern mit ggf. abweichender Bedeutung haben; 23.02.2024, WW }
      if (MeldNrDataObj.Data.GeraeteArt = AGeraeteArt) AND
         ((MeldNrDataObj.Data.MeldGrpNr = 0) OR
          ((MeldNrDataObj.Data.MeldGrpNr = 1) AND not (AMeldGrpNr in [6, 9]))) AND
         (MeldNrDataObj.Data.MNrGeraet = SearchMNrGeraet) then begin
        MeldNrData:=MeldNrDataObj.Data;  // 29.09.2021, WW
        Result:=true;
        Break;
      end;
    end;  { for i }
  end;
end;


{ TMeldTextKonfigList }

{------------------------------------------------------------------------------}
function TMeldTextKonfigList.FindMeldText (AMNr_Allg: string; var MText: string;
  var MArt: string; var MTyp: string): boolean;
{------------------------------------------------------------------------------}
{ Meldungstext, -Art und -Typ zu der �bergebenen allgemeinen Meldungsnummer in
  der Liste suchen;
  �bergabe: Allgemeine Meldungsnummer
  R�ckgabe: Meldungstext
            Meldungsart (Warnung, Hinweis etc.)
            Meldungstyp (einwertig, kommt, geht)
  Ergebnis: true, wenn Eintrag gefunden wurde }
var
  MeldTextDataObj: TMeldTextDataObj;
  i: integer;

begin
  Result:=false;
  MText:='';
  MArt:='';
  MTyp:='';
  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    MeldTextDataObj:=TMeldTextDataObj (Items [i]);
    if MeldTextDataObj.Data.MNrAllg = AMNr_Allg then begin
      MText:=MeldTextDataObj.Data.MText;
      MArt:=MeldTextDataObj.Data.MArt;
      MTyp:=MeldTextDataObj.Data.MTyp;
      Result:=true;
      Break;
    end;
  end;  { for }
end;


{------------------------------------------------------------------------------}

{----------------------------------------------------------------------}
function GetMeldNr_KonfigList (AGeraeteArt: string; AMeldGrpNr: integer;
  MeldNrKonfigList: TMeldNrKonfigList; Pfad: string): boolean;
{----------------------------------------------------------------------}
{ L�dt auf �bergebene Ger�teart und Meldungsgruppe gefilterten oder gesamten
  Inhalt von MeldNr.Dat in Liste;
  �bergabe: Ger�teart (wenn leer: alle Ger�tearten werden geladen)
            Meldungsgruppe (wenn < 0: alle Meldungsgruppen werden geladen)
            Pfad zur Ressourcendatei
  R�ckgabe: Liste mit Meldungsnummern-Konfigurationsdaten
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  FieldCount: integer;
  MeldNrData: TMeldNrData;
  MeldNrDataObj: TMeldNrDataObj;

begin
  Result:=false;
  if MeldNrKonfigList = nil then exit;
  MeldNrKonfigList.Clear;  // 06.08.2021, WW

  FName:=Pfad+CResMeldNr;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);              { Feld 1: Ger�teart }
          if length (FieldStr) = 1 then begin  // Ger�teart hat L�nge 1; 06.08.2021, WW
            MeldNrData.GeraeteArt:=FieldStr;

            if (AGeraeteArt = '') OR  // alle Ger�tearten; 06.08.2021, WW
               (MeldNrData.GeraeteArt = AGeraeteArt) then begin
              try
                // Record vorbelegen
                MeldNrData.MeldGrpNr:=0;
                MeldNrData.MNrGeraet:='';
                MeldNrData.MNrAllg:='';
                MeldNrData.MAdrGeraet:='';

                FieldCount:=1;
                while length (S) > 0 do begin
                  FieldStr:=F_Zerlegen (S, CResTrenner);
                  inc (FieldCount);
                  with MeldNrData do begin
                    case FieldCount of
                      2: MeldGrpNr:=StrToInt (FieldStr);
                      3: MNrGeraet:=FieldStr;
                      4: MNrAllg:=FieldStr;
                      5: MAdrGeraet:=FieldStr;  // 29.09.2021, WW
                    end;  { case }
                  end;  { with }
                end;  { while length (S) }

                { bei DSfG zus�tzlich allgemeing�ltige Meldungen mitladen:
                  - Meldungsnummern-Bereich 1-999 (Meldungs-Gruppe 0)
                  - DSfG-Ereignisliste ab 12/2023: Meldungsnummern-Bereich 1000-9999
                    (Meldungs-Gruppe 1) -> nicht f�r DSfG-Ger�te der Meldungsgruppen 6 und 9, da
                    diese Ger�te Meldungsnummern mit ggf. abweichender Bedeutung haben; 23.02.2024, WW }
                if (AMeldGrpNr < 0) OR  // alle Meldungsgruppen; 06.08.2021, WW
                   (MeldNrData.MeldGrpNr = AMeldGrpNr) OR
                   ((AGeraeteArt = C_GerArtDSfG) AND (MeldNrData.MeldGrpNr = 0)) OR
                   ((AGeraeteArt = C_GerArtDSfG) AND (MeldNrData.MeldGrpNr = 1) AND
                     not (AMeldGrpNr in [6, 9])) then begin
                  { Listenobjekt createn und in Liste einf�gen: }
                  MeldNrDataObj:=TMeldNrDataObj.Create;
                  MeldNrDataObj.SetData (MeldNrData);
                  MeldNrKonfigList.Add (MeldNrDataObj);
                end;
              except
                Result:=false;
              end;
            end;
          end;  // if length (FieldStr)
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

{-----------------------------------------------------------------------}
function GetMeldText_KonfigList (MeldTextKonfigList: TMeldTextKonfigList;
  Pfad: string): boolean;
{-----------------------------------------------------------------------}
{ L�dt Inhalt von MeldText.Dat in Liste;
  �bergabe: Pfad zur Ressourcendatei
  R�ckgabe: Liste mit Meldungstexten
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  S: string;
  FieldStr: string;
  FieldCount: integer;
  MeldTextData: TMeldTextData;
  MeldTextDataObj: TMeldTextDataObj;
  FName: string;
  TFS: TTextFileStream;
  FSize: integer;

begin
  Result:=false;
  if MeldTextKonfigList = nil then exit;
  MeldTextKonfigList.Clear;  // 06.08.2021, WW

  FName:=Pfad+CResMeldText;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);              { Feld 1: allgemeine Meldungsnummer }
          if StrToIntDef (FieldStr, -1) > 0 then begin    { Feld 1 enth�lt Zahlenwert }
            try
              // Record vorbelegen
              MeldTextData.MNrAllg:=FieldStr;
              MeldTextData.MText:='';
              MeldTextData.MArt:='';
              MeldTextData.MTyp:='';

              FieldCount:=1;
              while length (S) > 0 do begin
                FieldStr:=F_Zerlegen (S, CResTrenner);
                inc (FieldCount);
                with MeldTextData do begin
                  case FieldCount of
                    2: MText:=FieldStr;
                    3: MArt:=FieldStr;
                    4: MTyp:=FieldStr;
                  end;  { case }
                end;  { with }
              end;  { while length (S) }

              { Listenobjekt createn und in Liste einf�gen: }
              MeldTextDataObj:=TMeldTextDataObj.Create;
              MeldTextDataObj.SetData (MeldTextData);
              MeldTextKonfigList.Add (MeldTextDataObj);
            except
              Result:=false;
            end;
          end;
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

end.

