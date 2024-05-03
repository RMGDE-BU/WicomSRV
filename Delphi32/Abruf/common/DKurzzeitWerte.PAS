{******************************************************************************}
{* Unit: Zugriff auf Dateien mit DSfG-Kurzzeitwerten im ASCII-Format          *}
{* 08.11.2006 WW                                                              *}
{* 28.06.2010 GD  Erweitert um optionales Datentyp-Feld                       *}
{******************************************************************************}
unit DKurzzeitWerte;

interface

uses
  Classes, Forms, Windows, SysUtils, Contnrs, T_Tools, T_Zeit, GD_Utils, WChars,
  WStrUtils, Novell, WSysCon, IecConst;

const
  prefix_IEC_Kurzzeitwert = 'IKW';
  prefix_GPRS_Debug       = 'GPRSDEB';
  extension_Id            = '.id';

type
  { Struktur für Kurzzeitwerte }

  TDKZWData = record
    Datentyp: byte;     // 28.06.2010
    TelegrNr: integer;  // fortlaufende Nummer wie vom Server empfangen
    Kennung: string;
    EAdr: string;
    DEL: string;
    DatumZeit: TDateTime;
    Wert: double;
  end;

  { Objekt für Zugriff auf Kurzzeitwert-Dateien }

  TDSfGKurzzeitwert = class(TObject)
  private
    Pfad: string;
  public
    constructor Create (APfad: string);
    function ReadFile_IECKopplung (
      DataFileName: string; var DKZWData: TDKZWData): boolean;
    function WriteFile_IECKopplung (
      DKZWData: TDKZWData; bIncludeDataType: boolean = False): boolean;
    function DeleteFile_IECKopplung (DataFileName: string): boolean;

    function WriteFile_Debug (DKZWData: TDKZWData): boolean;
    function WriteFile_Ascii (DKZWData: TDKZWData): boolean;
  end;

  { Objekt für Kurzzeitwertliste }

  TKZWListObj = class (TObject)
    KZWData: TDKZWData;
    DataFileName: string;
    FileDate: integer;
    TransferCause: integer;
  public
    procedure SetData(AKZWData: TDKZWData; ADataFileName: string;
      AFileDate: integer; iTransferCause: integer = C_URS_spontan);
  end;

  { Kurzzeitwertliste }
  TKZWListe = class(TObjectList)
  private
    Path: string;
    MaxHours: integer;
    LoescheAlt: boolean;
    DSfGKurzzeitwert: TDSfGKurzzeitwert;
  public
    constructor Create (APath: string; AMaxHours: integer; ALoescheAlt: boolean);
    destructor Destroy; override;
    function LadeKZWFiles: boolean;
    function FindDataFilename (ADataFileName: string): boolean;
    function DeleteKZWFile (AIndex: integer): boolean;
    function DeleteOldKZWFiles: boolean;
  end;

implementation

const
  CTrennzeichen        = #9;  // TAB ist Trennzeichen innerhalb einer Zeile
  C_FormatDateTime_KZW = 'yyyymmddhhnnss';
  C_FormatDateTime_Asc = 'yyyymmddhhnnss';


{ TDSfGKurzzeitwert }

{---------------------------------------------------}
constructor TDSfGKurzzeitwert.Create (APfad: string);
{---------------------------------------------------}
begin
  inherited Create;
  Pfad:=APfad;
end;

{---------------------------------------------------------------------------------}
function TDSfGKurzzeitwert.ReadFile_IECKopplung (DataFileName: string;
                                                 var DKZWData: TDKZWData): boolean;
{---------------------------------------------------------------------------------}
{ Kurzzeitwert aus Datei lesen (ASCII-Format für IEC-Kopplung);
  Übergabe: Name der zu lesenden Kurzzeitwert-Datei (ohne Pfad)
  Rückgabe: Kurzzeitwert-Datenrecord
  Ergebnis: true, wenn Datei gelesen werden konnte }
var
  TFS: TTextFileStreamExt;
  isOpened: boolean;
  FSize: integer;
  S: string;
  sDZ: string;
  TimeBuf: TDateTime;
  Code: integer;

begin
  Result:=false;
  with DKZWData do begin  // Vorbelegung für Rückgabe
    Datentyp := 0;
    TelegrNr:=-1;
    Kennung:='';
    EAdr:='';
    DEL:='';
    DatumZeit:=-1;
    Wert:=0;
  end;

  if FileExists (Pfad + DataFileName) then begin
    TFS:=TTextFileStreamExt.Create (Pfad + DataFileName, fmOpenRead OR fmShareDenyWrite, isOpened);
    try
      if isOpened then begin
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);  { 1. Zeile: Telegramm-Nummer optional <TAB> Datentyp}
          DKZWData.TelegrNr:=StrToInt (F_Zerlegen(S, CTrennzeichen));
          if (Length(S) > 0)      // 28.06.2010
          then DKZWData.Datentyp := StrToIntDef(S, 0)
          else DKZWData.Datentyp := 0;
          TFS.ReadLn (S);  { 2. Zeile: DSfG-Kennung <TAB> EAdr <TAB> DE-Adresse }
          DKZWData.Kennung:=F_Zerlegen (S, CTrennzeichen);
          DKZWData.EAdr:=F_Zerlegen (S, CTrennzeichen);
          DKZWData.DEL:=S;
          TFS.ReadLn (S);  { 3. Zeile: Datum/Zeit <TAB> Wert }
          sDZ:=F_Zerlegen (S, CTrennzeichen);
          EncodeDateStr (Copy (sDZ, 1, 8), 'YYYYMMDD', DKZWData.DatumZeit);
          EncodeTimeStr (Copy (sDZ, 9, 6), 'HHMMSS', TimeBuf);
          DKZWData.DatumZeit:=DKZWData.DatumZeit + TimeBuf;
          Val (S, DKZWData.Wert, Code);
          if Code <> 0 then
            Break;
          Result:=true;
        end;  { while FS.Position < FSize }
      end;
    finally
      TFS.Free;
    end;
  end;
end;

{------------------------------------------------------------------------------}
function TDSfGKurzzeitwert.WriteFile_IECKopplung (
  DKZWData: TDKZWData; bIncludeDataType: boolean = False): boolean;
{------------------------------------------------------------------------------}
{ Kurzzeitwert in Datei schreiben (ASCII-Format für IEC-Kopplung); Es entsteht
  eine Daten-Datei (Erweiterung '.tmp') und eine gleichnamige ID-Datei (Triggerdatei,
  Erweiterung '.id'). Der Dateiname wird automatisch gebildet und ist eindeutig.
  Übergabe: Kurzzeitwert-Datenrecord }
var
  FName_Data: string;
  FName_Id: string;
  sData: string;
  sWert: string;

begin
  // Daten-File schreiben:
  //   1. Zeile: Telegramm-Nummer (Optional mit Datentyp)
  //   2. Zeile: Kennung, DSfG-Busadresse, DSfG-Datenelementadresse
  //   3. Zeile: Datum/Zeit, Wert
  sData := IntToStr (DKZWData.TelegrNr);
  if (bIncludeDataType) then  // 28.06.2010
    sData := sData + CTrennzeichen + IntToStr(DKZWData.Datentyp);
  sData:= sData + CR + LF + DKZWData.Kennung + CTrennzeichen + DKZWData.EAdr +
    CTrennzeichen + DKZWData.DEL + CR + LF +
    FormatDateTime (C_FormatDateTime_KZW, DKZWData.DatumZeit);
  sWert:=FloatToStr (DKZWData.Wert);
  StrSubst(sWert, ',', '.');  { Ausgabe fest mit Dezimal-Punkt }
  sData:=sData + CTrennzeichen + sWert + CR + LF;

  FName_Data:=CreateTempRohFile (Pfad, prefix_IEC_Kurzzeitwert);
  if not WriteRohFile (FName_Data, sData) then begin
    Result:=false;
    exit;
  end;

  // ID-File schreiben (Triggerfile):
  // -> Das ID-File signalisiert dem weiterverarbeitenden Programm, daß ein
  //    zugehöriges Datenfile bereit liegt.
  FName_Id:=ChangeFileExt (FName_Data, extension_Id);
  with TFileStreamExt.Create (FName_Id, fmCreate, Result) do Free;
end;

{--------------------------------------------------------------------------------}
function TDSfGKurzzeitwert.DeleteFile_IECKopplung (DataFileName: string): boolean;
{--------------------------------------------------------------------------------}
{ Kurzzeitwert-Datendatei und zugehörige Id-Datei löschen;
  Übergabe: Name der Daten-Datei (ohne Pfad)
  Ergebnis: true, wenn Dateien gelöscht werden konnten }
var
  IdFileName: string;
  OK_Data: boolean;
  OK_Id: boolean;

begin
  IdFileName:=ChangeFileExt (Pfad + DataFileName, extension_Id);  // Id-Dateiname
  OK_Id:=DeleteFile (IdFileName);  // zuerst die Id-Datei löschen
  OK_Data:=DeleteFile (Pfad + DataFileName);  // dann die Daten-Datei
  Result:=OK_Data AND OK_Id;
end;

{------------------------------------------------------------------------}
function TDSfGKurzzeitwert.WriteFile_Debug (DKZWData: TDKZWData): boolean;
{------------------------------------------------------------------------}
{ Kurzzeitwert in Debug-Datei schreiben (ASCII-Format); Es entsteht
  für jeden Kanal eines Gerätes eine eigene Daten-Datei, welche fortgeschrieben
  wird (Erweiterung '.dat'). Der Dateiname wird aus Kennung, DSfG-Busadresse und
  DSfG-Datenelementadresse gebildet.
  Übergabe: Kurzzeitwert-Datenrecord }
var
  FName_Data: string;
  sData: string;
  sWert: string;
  i: integer;

begin
  // Daten-File schreiben: Je Wert 1 Zeile mit Datum, Zeit, Wert
  sData:=DateToStr (DKZWData.DatumZeit) + CTrennzeichen +
         TimeToStr (DKZWData.DatumZeit);
  sWert:=FloatToStr (DKZWData.Wert);
  StrSubst(sWert, ',', '.');  { Ausgabe fest mit Dezimal-Punkt }
  sData:=sData + CTrennzeichen + sWert;

  // Dateiname mit Kennung, DSfG-Busadresse, DSfG-Datenelementadresse:
  FName_Data:=prefix_GPRS_Debug + '_' + DKZWData.Kennung + '_' +
              DKZWData.EAdr + '_' + DKZWData.DEL + '.dat';
  // im Dateinamen nicht erlaubte Zeichen ausfiltern:
  for i:=1 to length (CNotAllowedChars_FileName) do
    FName_Data:=StringReplace (FName_Data, CNotAllowedChars_FileName [i],
                               '', [rfReplaceAll]);
  Result:=StringToFile (sData, Pfad + FName_Data, false);
end;

{------------------------------------------------------------------------}
function TDSfGKurzzeitwert.WriteFile_Ascii (DKZWData: TDKZWData): boolean;
{------------------------------------------------------------------------}
{ Kurzzeitwert in ASCII-Datei schreiben (Standard-ASCII-Format); Es entsteht
  für jeden Kanal eines Gerätes eine eigene Daten-Datei, welche fortgeschrieben
  wird (Erweiterung '.dat'). Der Dateiname wird aus Kennung, DSfG-Busadresse und
  DSfG-Datenelementadresse gebildet.
  Übergabe: Kurzzeitwert-Datenrecord }
var
  FName_Data: string;
  sData: string;
  sHeader: string;
  i: integer;

begin
  // Header schreiben: Kennzeichen, DSfG-Kennung, Gerätetyp 'DSfG'
  sHeader:=#$1D + 'G' + DKZWData.Kennung + CR + LF +
           'DSfG';
  // Daten-File schreiben: Je Wert 1 Zeile mit Datum, Zeit, Wert
  sData:=FormatDateTime (C_FormatDateTime_Asc, DKZWData.DatumZeit) +
         '@@' + Format ('%.14d', [Round (DKZWData.Wert)]);

  // Dateiname mit Kennung, DSfG-Busadresse, DSfG-Datenelementadresse:
  FName_Data:=prefix_GPRS_Debug + '_' + DKZWData.Kennung + '_' +
              DKZWData.EAdr + '_' + DKZWData.DEL + '.dat';
  // im Dateinamen nicht erlaubte Zeichen ausfiltern:
  for i:=1 to length (CNotAllowedChars_FileName) do
    FName_Data:=StringReplace (FName_Data, CNotAllowedChars_FileName [i],
                               '', [rfReplaceAll]);
  // Header schreiben, wenn Datei nicht existiert:
  if not FileExists (Pfad + FName_Data) then begin
    Result:=StringToFile (sHeader, Pfad + FName_Data, false);
    if not Result then exit;
  end;
  // Daten schreiben:
  Result:=StringToFile (sData, Pfad + FName_Data, false);
end;


{------------------------- Kurzzeitwertliste ----------------------------------}

{-----------------------------------------------------------}
function TelegrammNrCompare (Item1, Item2: Pointer): Integer;
{-----------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren der Kurzzeitwert-Liste nach der Telegramm-Nummer
  (in ABSTEIGENDER Reihenfolge (älteste am Ende) ! Für aufsteigende Reihenfolge
  muß die Liste vom Ende her gelesen werden ! }
begin
  Result:=TKZWListObj (Item2).KZWData.TelegrNr - TKZWListObj (Item1).KZWData.TelegrNr;
end;

{----------------------------------------------------------------------------}
function Kennung_EAdr_DEL_TelegrammNrCompare (Item1, Item2: Pointer): Integer;
{----------------------------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren der Kurzzeitwert-Liste nach:
  1. Kennung
  2. EAdr
  3. DEL
  4. Telegramm-Nummer
  -> in AUFSTEIGENDER Reihenfolge (jüngste je Kennung/EAdr/DEL am Ende) ! }
begin
  Result:=CompareStr (TKZWListObj (Item1).KZWData.Kennung, TKZWListObj (Item2).KZWData.Kennung);  { 1. Sortierkriterium: Kennung }
  if Result = 0 then begin
    Result:=CompareStr (TKZWListObj (Item1).KZWData.EAdr, TKZWListObj (Item2).KZWData.EAdr);  { 2. Sortierkriterium: EAdr }
    if Result = 0 then begin
      Result:=CompareStr (TKZWListObj (Item1).KZWData.DEL, TKZWListObj (Item2).KZWData.DEL);  { 3. Sortierkriterium: DEL }
      if Result = 0 then
        Result:=TKZWListObj (Item1).KZWData.TelegrNr - TKZWListObj (Item2).KZWData.TelegrNr;  { 4. Sortierkriterium: Telegrammnummer }
    end;
  end;
end;


{ TKZWListObj }

{---------------------------------------------------------------------------------------------}
procedure TKZWListObj.SetData (AKZWData: TDKZWData; ADataFileName: string;
  AFileDate: integer; iTransferCause: integer = C_URS_spontan);
{---------------------------------------------------------------------------------------------}
{ KZW-Listenobjekt belegen }
begin
  KZWData:=AKZWData;
  DataFileName:=ADataFileName;
  FileDate:=AFileDate;
  TransferCause := iTransferCause;
end;


{ TKZWListe }

{-------------------------------------------------------------------------------------}
constructor TKZWListe.Create (APath: string; AMaxHours: integer; ALoescheAlt: boolean);
{-------------------------------------------------------------------------------------}
{ Übergaben: Pfad zu den Kurzzeitwert-Dateien
             Anzahl der Stunden bis noch nicht übertragene Dateien gelöscht werden
             Flag 'ALoescheAlt' (true: es wird, falls mehrere Kurzzeitwerte je Kanal/Logbuch
                                       vorhanden sind, nur der aktuellste in die Liste
                                       übernommen. Alle älteren werden gelöscht.) }
begin
  inherited Create (true);
  Path:=APath;
  MaxHours:=AMaxHours;
  LoescheAlt:=ALoescheAlt;
  DSfGKurzzeitwert:=TDSfGKurzzeitwert.Create (APath);
end;

{---------------------------}
destructor TKZWListe.Destroy;
{---------------------------}
begin
  DSfGKurzzeitwert.Free;
  inherited Destroy;
end;

{---------------------------------------}
function TKZWListe.LadeKZWFiles: boolean;
{---------------------------------------}
{ lädt alle vorhandenen Dateien mit DSfG-Kurzzeitwerten in Liste;
  Ergebnis: true, wenn neue Dateien in Liste geladen wurden }
var
  i: integer;
  SR: TSearchRec;
  FNameData: string;
  FNameDataWithPath: string;
  KZWData: TDKZWData;
  KZWListObj: TKZWListObj;

begin
  // alle vorhandenen Kurzzeitwert-Id-Dateien zusammensuchen:
  i:=FindFirst (Path + prefix_IEC_Kurzzeitwert + '*' + extension_Id, faAnyFile, SR);
  try
    while i = 0 do begin
      Application.ProcessMessages;
      FNameData:=ChangeFileExt (SR.Name, '.tmp');  // Daten-Dateiname
      FNameDataWithPath:=Path + FNameData;
      if FileExists (FNameDataWithPath) then begin
        // neuer Eintrag in Kurzzweitwert-Liste, wenn Daten-Datei noch nicht
        // enthalten ist:
        if not FindDataFileName (FNameData) then begin
          { Kurzzeitwert-Daten aus Datei auslesen: }
          if DSfGKurzzeitwert.ReadFile_IECKopplung (FNameData, KZWData) then begin
            { KZW-Daten incl. Daten-Filename in Liste laden: }
            KZWListObj:=TKZWListObj.Create;
            KZWListObj.SetData (KZWData, FNameData, SR.Time);
            Add (KZWListObj);
          end;
        end;
      end;
      i:=FindNext (SR);
    end;  { while i = 0 }
  finally
    FindClose (SR);
  end;

  // alle "überalteten" Einträge der Kurzzeitwert-Liste incl. Dateien rauslöschen:
  DeleteOldKZWFiles;

  Sort (TelegrammNrCompare);  // Kurzzeitwert-Liste nach Telegramm-Nummer sortieren

  Result := (Count > 0);
end;

{-------------------------------------------------------------------}
function TKZWListe.FindDataFilename (ADataFileName: string): boolean;
{-------------------------------------------------------------------}
{ sucht Dateiname in KZW-Liste }
var
  i: integer;
begin
  Result:=false;
  for i:=0 to Count - 1 do begin
    if TKZWListObj (Items [i]).DataFileName = ADataFileName then begin
      Result:=true;
      Break;
    end;
  end;
end;

{----------------------------------------------------------}
function TKZWListe.DeleteKZWFile (AIndex: integer): boolean;
{----------------------------------------------------------}
{ löscht Listeneintrag mit Index incl. der zugehörigen Daten- und Id-Datei }
var
  FNameData: string;
begin
  if (AIndex >= 0) AND (AIndex < Count) then begin
    FNameData:=TKZWListObj (Items [AIndex]).DataFileName;
    if (FNameData = '') then Result := True
    else Result:=DSfGKurzzeitwert.DeleteFile_IECKopplung (FNameData); // Daten- und Id-Datei löschen
    Delete (AIndex);  // Listeneintrag löschen
  end else
    Result:=false;
end;

{--------------------------------------------}
function TKZWListe.DeleteOldKZWFiles: boolean;
{--------------------------------------------}
{ löscht alte Listeneinträge incl. der zugehörigen Daten- und Id-Datei }
var
  i: integer;
  AktDateTime: TDateTime;
  DiffSec: longint;
  Kennung_Merk: string;
  EAdr_Merk: string;
  DEL_Merk: string;

begin
  Result:=true;
  // es werden alle Einträge gelöscht, die älter als MaxHours sind (keine Sortierung
  // notwendig):
  AktDateTime:=Now;
  for i:=(Count - 1) downto 0 do begin  // Liste vom Ende her lesen, da Listeneinträge evtl. gelöscht werden
    F_TimeDiff (FileDateToDateTime (TKZWListObj (Items [i]).FileDate), AktDateTime, DiffSec);
    if Diffsec > (MaxHours * 3600) then
      if not DeleteKZWFile (i) then  // Listeneintrag, Daten- und Id-Datei löschen
        Result:=false;
  end;

  if LoescheAlt then begin
    // es werden alle (alten) Einträge eines Kanals/Logbuchs außer dem jüngsten
    // (aktuellsten) gelöscht (Sortierung der Listeneinträge nach Kennung, EAdr,
    // DEL, Datum/Zeit notwendig !):
    Sort (Kennung_EAdr_DEL_TelegrammNrCompare);  // Kurzzeitwert-Liste nach Kennung,
                                                 // EAdr, DEL, Telegramm-Nummer sortieren
    Kennung_Merk:='';
    EAdr_Merk:='';
    DEL_Merk:='';
    for i:=(Count - 1) downto 0 do begin  // Liste vom Ende her lesen, da Listeneinträge evtl. gelöscht werden
      if (CompareStr (TKZWListObj (Items [i]).KZWData.Kennung, Kennung_Merk) = 0) AND
         (CompareStr (TKZWListObj (Items [i]).KZWData.EAdr, EAdr_Merk) = 0) AND
         (CompareStr (TKZWListObj (Items [i]).KZWData.DEL, DEL_Merk) = 0) then begin   // gleicher Kanal/Logbuch
        if not DeleteKZWFile (i) then  // Listeneintrag, Daten- und Id-Datei löschen
          Result:=false;
      end;

      Kennung_Merk:=TKZWListObj (Items [i]).KZWData.Kennung;
      EAdr_Merk:=TKZWListObj (Items [i]).KZWData.EAdr;
      DEL_Merk:=TKZWListObj (Items [i]).KZWData.DEL;
    end;
  end;
end;

end.

