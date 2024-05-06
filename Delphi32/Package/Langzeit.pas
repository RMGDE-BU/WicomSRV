{-------------------------------------------------------------------}
{ Unit Langzeit                                                     }
{ 15.02.1996 D. Achter                                              }
{ Komponenten zum Zugriff auf Langzeitdaten                         }
{ 14.04.1999 GD; Umstellung auf Dephi32 und manuelle Daten          }
{ 19.06.2002 WW; FindClose-Aufrufe ergänzt (Speicherleck !)         }
{-------------------------------------------------------------------}

unit Langzeit;

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, objfile, novell;

{$A-,B-,H-}

const                                                

  CImpulsMaske = $CF;

  CImpulsKanalStatus : array [0..7] of PChar = (
    'Überlauf',
    'Kanal in Prüfung',
    'Primärgerät gestört',
    'Kanal inaktiv',
    'Bit 4',
    'Bit 5',
    'Kanal wurde korrigiert',
    'Wert fehlt'
  );

  CImpulsKanalStatusChar : array [0..7] of Char = (
    'Ü',
    'P',
    'S',
    'I',
    ' ',
    ' ',
    'K',
    'F'
  );

  CAnalogMaske = $CF;

  CAnalogKanalStatus : array [0..7] of PChar = (
    'Analogkarte nicht gesteckt',
    'Kanal in Prüfung',
    'Primärgerät gestört',
    'Kanal inaktiv',
    'Bit 4',
    'Bit 5',
    'Kanal wurde korrigiert',
    'Wert fehlt'
  );

  CAnalogKanalStatusChar : array [0..7] of Char = (
    'A',
    'P',
    'S',
    'I',
    ' ',
    ' ',
    'K',
    'F'
  );

  CSatzMaske = $4A;

  CSatzStatus : array [0..7] of PChar = (
    'Bit 0',
    'Uhr gestellt',
    'Bit 2',
    'Revision',
    'Bit 4',
    'Bit 5',
    'Stunde mehrfach vorhanden',
    ''   { Bit 7: Satz hinzugefügt von Aufbereitung (keine Angabe, Fehlend-Info steckt schon im Kanalstatus) }
  );

  CSatzStatusChar : array [0..7] of Char = (
    ' ',
    'U',
    ' ',
    'R',
    ' ',
    ' ',
    'M',
    ' '
  );

  CKanalMaske = $80;

  CKanalStatus : array [0..7] of PChar = (
    'Bit 0',
    'Kontrollzähler',
    'Bit 2',
    'Bit 3',
    'Bit 4',
    'Bit 5',
    'Bit 6',
    'Wert fehlt'
  );

  CKanalStatusChar : array [0..7] of Char = (
    ' ',
    ' ',
    ' ',
    ' ',
    ' ',
    ' ',
    ' ',
    'F'
  );

type

  TLGZException = class (Exception);

  TKennung = string [14];

  TLangzeitState = (
    lgzClosed,         { Langzeitdatei geschlossen }
    lgzKennung,        { Kennung in Verzeichnisdatei gefunden }
    lgzOpened          { Langzeitdatei geöffnet }
  );

  TLangzeit = class (TComponent)
  private
    FState: TLangzeitState;           { Status }
    FDirectory: TFileName;            { Verzeichnis der Langzeitdaten }
    FKennung: TKennung;               { Kennung der Langzeitdatei }
    FKanalZahl: Word;                 { Kanalzahl }
    BufFile: TBufFile;                { gepufferter Dateizugriff }
    procedure SetKanalZahl (Value: Word);
    function GetEof: Boolean;
    function GetKennung: TKennung;
    procedure SetKennung (Value: TKennung);
    function GetSize: LongInt;
    function SpaceTrunc (const AStr: string): string;
  protected
    DataFile: TFileName;              { Langzeitdatei }
    DataBuf: Pointer;                 { Langzeitdatensatz }
    procedure SetDataFile (FileNr: Word); virtual; abstract;
    function GetDataSize: word; virtual; abstract;
    procedure ReadBuf;
    procedure CheckOpened;
    procedure CheckIndex (Index: Word);
    function EqualKennung (const Kennung1, Kennung2: string): Boolean;
  public
    isLangzeit: boolean;              { Flag, ob Auto (TRUE) oder Manu-Daten - GeDa }
    ManuNr: integer;                  { File-Nr. für Manu-Daten (=MrgId) - GeDa }
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    function Search (const AKennung: TKennung): Boolean; virtual;
    function GetFileName (const AKennung: TKennung): TFileName;
    procedure Open; virtual;
    procedure Close; virtual;
    procedure First; virtual;
    procedure Last; virtual;
    procedure Next; virtual;
    function Seek (Position: LongInt): Boolean; virtual;
    function SearchPos (Position: LongInt): Boolean; virtual;
    procedure Truncate;
    property State: TLangzeitState read FState;
    property Kennung: TKennung read GetKennung write SetKennung;
    property EOF: Boolean read GetEOF;
  published
    property Directory: TFileName read FDirectory write FDirectory;
    property KanalZahl: Word read FKanalZahl write SetKanalZahl;
    property Size: LongInt read GetSize;
  end;

  TLGZStdData = class (TObject)
  private
    FKanalzahl: Word;
    FBuf: Pointer;
    FBufSize: Word;
    function GetDatum: TDateTime;
    procedure SetDatum (Value: TDateTime);
    function GetSatzStatus: Byte;
    procedure SetSatzStatus (Value: Byte);
    function GetWert (Index: Word): Word;
    procedure SetWert (Index: Word; Value: Word);
    function GetKanalStatus (Index: Word): Byte;
    procedure SetKanalStatus (Index: Word; Value: Byte);
  public
    constructor Create (AKanalzahl: Word);
    destructor Destroy; override;
    procedure Clear (ADatum: TDateTime);
    property Kanalzahl: Word read FKanalZahl;
    property DataBuf: Pointer read FBuf;
    property DataSize: Word read FBufSize;
    property Datum: TDateTime read GetDatum;
    property SatzStatus: Byte
               read GetSatzStatus write SetSatzStatus;
    property Wert [Index: Word]: Word
               read GetWert write SetWert;
    property KanalStatus [Index: Word]: Byte
               read GetKanalStatus write SetKanalStatus;
  end;

  TLGZStd = class (TLangzeit)
  private
    FirstOrdnungsNummer: LongInt;
    function GetOrdnungsnummer: LongInt;
    function GetDatum: TDateTime;
    function GetYear: Word;
    function GetMonth: Word;
    function GetDay: Word;
    function GetHour: Word;
    function GetSatzStatus: Byte;
    function GetWert (Index: Word): Word;
    function GetKanalStatus (Index: Word): Byte;
    function GetDateiName: TFileName;   {kb 8.7.97}
  protected
    procedure SetDataFile (FileNr: Word); override;
    function GetDataSize: word; override;
  public
    procedure Open; override;
    function CreateLGZStdData: TLGZStdData;
    function SearchOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
    function SearchGEOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
    function SearchDatum (ADatum: TDateTime): Boolean;
    function SearchGEDatum (ADatum: TDateTime): Boolean;
    property Ordnungsnummer: LongInt read GetOrdnungsnummer;
    property Datum: TDateTime read GetDatum;
    property Year: Word read GetYear;
    property Month: Word read GetMonth;
    property Day: Word read GetDay;
    property Hour: Word read GetHour;
    property SatzStatus: Byte read GetSatzStatus;
    property Wert [Index: Word]: Word read GetWert;
    property KanalStatus [Index: Word]: Byte read GetKanalStatus;
    property DateiName: TFileName read GetDateiName; {kbkb 8.7.97}
  end;

  TLGZTag = class (TLangzeit)
  private
    FirstOrdnungsNummer: LongInt;
    function GetOrdnungsnummer: LongInt;
    function GetSatzStatus: Byte;
    function GetDatum: TDateTime;
    function GetStunden: Byte;
    function GetEingangStatus (Index: Word): Byte;
    function GetEingangWert (Index: Word): LongInt;
    function GetKontrollStatus (Index: Word): Byte;
    function GetKontrollWert (Index: Word): LongInt;
    function GetDateiName : TFileName;  {kbkb 8.7.97}
  protected
    procedure SetDataFile (FileNr: Word); override;
    function GetDataSize: word; override;
  public
    procedure Open; override;
    function SearchOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
    function SearchGEOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
    function SearchDatum (ADatum: TDateTime): Boolean;
    function SearchGEDatum (ADatum: TDateTime): Boolean;
    property Ordnungsnummer: LongInt read GetOrdnungsnummer;
    property SatzStatus: Byte read GetSatzStatus;
    property Datum: TDateTime read GetDatum;
    property Stunden: Byte read GetStunden;
    property EingangStatus [Index: Word]: Byte read GetEingangStatus;
    property EingangWert [Index: Word]: LongInt read GetEingangWert;
    property KontrollStatus [Index: Word]: Byte read GetKontrollStatus;
    property KontrollWert [Index: Word]: Longint read GetKontrollWert;
    property DateiName: TFileName read GetDateiName; {kbkb 8.7.97}
  end;

procedure Register;

implementation

const
  CLGZDVerz = 'LGZDVERZ.DAT';

type

  DateRec = Record
    Year  : SmallInt {Integer};          { wegen Kompabilität zu Delphi 1 }
    Month : SmallInt {Integer};          { wegen Kompabilität zu Delphi 1 }
    Day   : SmallInt {Integer};          { wegen Kompabilität zu Delphi 1 }
  End;

  TimeRec = Record
    Hour : SmallInt {Integer};           { wegen Kompabilität zu Delphi 1 }
    Min  : SmallInt {Integer};           { wegen Kompabilität zu Delphi 1 }
    Sec  : SmallInt {Integer};           { wegen Kompabilität zu Delphi 1 }
    Hsec : SmallInt {Integer};           { wegen Kompabilität zu Delphi 1 }
  End;

  { Langzeitverzeichnisrecord }

  TLGZDVerz = record
    Kennung: string [14];
    FileNr: Word;
    Status: Byte;
    Index: word;
  end;

  TLGZDKanal = record
    KanalStatus: Byte;
    Wert: Word;
  end;

  TLGZD = record
    SatzStatus: Byte;
    Datum: DateRec;
    Zeit: TimeRec;
  end;

  TLGZDExt = record
    LGZD: TLGZD;
    Kanal: array [0..100] of TLGZDKanal;
  end;

  TLGZTKanal = record
    ZaehlerStatus: Byte;
    Wert: LongInt;
  end;

  TLGZT = record
    SatzStatus: Byte;
    Datum: DateRec;
    Stunden: Byte;
  end;

  TLGZTExt = record
    LGZT: TLGZT;
    Kanal: array [0..100] of TLGZTKanal;
  end;

{-----------}
{ TLangzeit }
{-----------}

{---------------------------------------------}
procedure TLangzeit.SetKanalZahl (Value: Word);
{---------------------------------------------}
begin
  if Value <> FKanalZahl then
  begin
    FreeMem (DataBuf, GetDataSize);
    FKanalZahl := Value;
    GetMem (DataBuf, GetDataSize);
  end;
end;

{---------------------------------}
function TLangzeit.GetEof: Boolean;
{---------------------------------}
begin
  Result := BufFile.Eof;
end;

{ Liefert Kennung der Langzeitdatei }
{--------------------------------------}
function TLangzeit.GetKennung: TKennung;
{--------------------------------------}
begin
  if FState <> lgzClosed then
    Result := FKennung
  else
    raise TLGZException.Create ('GetKennung: is closed');
end;

{ setzt Kennung für Langzeitdatei }
{-----------------------------------------------}
procedure TLangzeit.SetKennung (Value: TKennung);
{-----------------------------------------------}
var
  FS: TFileStreamExt;    // 25.04.2002 WW, statt TFileStream
  DataHandle: Integer;
  LGZDVerz: TLGZDVerz;
  SR: TSearchRec;
  FoundIt: Boolean;
  FileNrs: TList;
  OK: boolean;
begin
  if FState <> lgzOpened then
  begin
    FState := lgzClosed;

    { sucht Langzeitverzeichnisdatei }

    if FindFirst (FDirectory + CLGZDVerz, faAnyFile, SR) <> 0 then
    begin
      FS := TFileStreamExt.Create (FDirectory + CLGZDVerz, fmCreate, OK);
      FS.Free;
    end;
    FindClose (SR);

    FS := TFileStreamExt.Create (FDirectory + CLGZDVerz,
            fmOpenReadWrite + fmShareDenyWrite, OK);
    try
      if (FS.Size mod sizeof (LGZDVerz) = 0) then
      begin

        FileNrs := TList.Create;
        try

          { sucht Kennung }

          FoundIt := False;
          while not FoundIt and
                (FS.Read (LGZDVerz, sizeof (LGZDVerz)) = sizeof (LGZDVerz)) do
          begin
            FileNrs.Add (Pointer (LGZDVerz.FileNr));

            { Kennung gefunden }

            if EqualKennung (LGZDVerz.Kennung, Value) then
            begin
              FKennung := SpaceTrunc (LGZDVerz.Kennung);
              FoundIt := True;
            end;
          end;

          if not FoundIt then
          begin
            FillChar (LGZDVerz, sizeof (LGZDVerz), 0);
            LGZDVerz.Kennung := Value;
            LGZDVerz.FileNr := 1;
            while FileNrs.IndexOf (Pointer (LGZDVerz.FileNr)) >= 0 do
              Inc (LGZDVerz.FileNr);
            FS.Seek (0, 2);
            FS.WriteBuffer (LGZDVerz, sizeof (LGZDVerz));
          end;

        finally
          FileNrs.Free;
        end;

        SetDataFile (LGZDVerz.FileNr);
        if FindFirst (Directory + DataFile, faAnyFile, SR) <> 0 then
        begin
          DataHandle := FileCreate (Directory + DataFile);
          if DataHandle > 0 then
          begin
            FileClose (DataHandle);
            FState := lgzKennung;
          end;
        end
        else
          FState := lgzKennung;
        FindClose (SR);

      end
        else
          raise TLGZException.Create ('Search: incorrect Filesize');

    finally
      FS.Free;
    end;
  end
  else
    raise TLGZException.Create ('Search: not closed');
end;

{ Liefert Größe der Langzeitdatei }
{----------------------------------}
function TLangzeit.GetSize: LongInt;
{----------------------------------}
begin
  if (FState = lgzOpened) then
    Result := BufFile.Size
  else
    Result := 0;
end;

{ Entfernt alle Leer- und Nullzeichen am Anfang und Ende von AStr }
{---------------------------------------------------------}
function TLangzeit.SpaceTrunc (const AStr: string): string;
{---------------------------------------------------------}
begin
  Result := AStr;
  while (Length (Result) > 0) and (Result [1] in [' ', #0]) do
    Result := copy (Result, 2, Length (Result) - 1);
  while (Length (Result) > 0) and (Result [Length (Result)] in [' ', #0]) do
    Result := copy (result, 1, Length (Result) - 1);
end;

{ Liest Datensatz aus Datei }
{--------------------------}
procedure TLangzeit.ReadBuf;
{--------------------------}
begin
  CheckOpened;
  BufFile.Read (DataBuf^)
end;

{------------------------------}
procedure TLangzeit.CheckOpened;
{------------------------------}
begin
  if (FState <> lgzOpened) then
    raise TLGZException.Create ('File not opened');
end;

{-------------------------------------------}
procedure TLangzeit.CheckIndex (Index: Word);
{-------------------------------------------}
begin
  if (Index >= FKanalZahl) then
    raise TLGZException.Create ('Out of Index');
end;

{ Kennungsvergleich }
{--------------------------------------------------------------------------}
function TLangzeit.EqualKennung (const Kennung1, Kennung2: string): Boolean;
{--------------------------------------------------------------------------}
begin
  EqualKennung := CompareStr (SpaceTrunc (Kennung1),
                              SpaceTrunc (Kennung2)) = 0;
end;

{ Konstruktor }
{------------------------------------------------}
constructor TLangzeit.Create (AOwner: TComponent);
{------------------------------------------------}
begin
  inherited Create (AOwner);
  FState := lgzClosed;
  FDirectory := '';
  FKennung := '';
  FKanalZahl := 28;
  isLangzeit:= true; { Auto-Daten - GeDa }
  ManuNr:= 0;       { Manu-Default - GeDa }
  GetMem (DataBuf, GetDataSize);
end;

{ Destruktor }
{---------------------------}
destructor TLangzeit.Destroy;
{---------------------------}
begin                                                         
  Close;
  FreeMem (DataBuf, GetDataSize);
  inherited Destroy;
end;

{ Sucht nach Kennung in der Verzeichnisdatei, liefert True, wenn
  Kennung gefunden wurde, und zugehörige Datendatei vorhanden ist }
{------------------------------------------------------------}
function TLangzeit.Search (const AKennung: TKennung): Boolean;
{------------------------------------------------------------}
var
  FS: TFileStreamExt;   // 25.04.2002 WW, statt TFileStream
  LGZDVerz: TLGZDVerz;
  SR: TSearchRec;
  OK: boolean;
begin
  Result := False;
  if FState <> lgzOpened then begin
    if isLangzeit then begin   { GeDa }
      FState := lgzClosed;
      OK:=FindFirst (FDirectory + CLGZDVerz, faAnyFile, SR) = 0;
      FindClose (SR);
      if OK then begin
        FS := TFileStreamExt.Create (FDirectory + CLGZDVerz,
                fmOpenRead + fmShareDenyWrite, OK);
        try
          if OK then begin
            if (FS.Size mod sizeof (LGZDVerz) = 0) then
            begin
              while FS.Read (LGZDVerz, sizeof (LGZDVerz)) = sizeof (LGZDVerz) do
              begin
                if EqualKennung (LGZDVerz.Kennung, AKennung) then
                begin
                  FKennung := SpaceTrunc (LGZDVerz.Kennung);
                  SetDataFile (LGZDVerz.FileNr);
                  if FindFirst (Directory + DataFile, faAnyFile, SR) = 0 then
                  begin
                    FState := lgzKennung;
                    Result := True;
                  end;
                  FindClose (SR);
                  break;
                end;
              end;
            end
            else
              raise TLGZException.Create ('Search: incorrect Kanalzahl');
          end;
        finally
          FS.Free;
        end;
      end;
    end  { GeDa }
    else begin  { GeDa - neuer Zweig für Manu-Daten }
      SetDataFile (LGZDVerz.FileNr);
      if FindFirst (Directory + DataFile, faAnyFile, SR) = 0 then
      begin
        FState := lgzKennung;
        Result := True;
      end;
      FindClose (SR);
    end
  end
  else
    raise TLGZException.Create ('Search: not closed');
end;

{-------------------------------------------------------------------}
function TLangzeit.GetFileName (const AKennung: TKennung): TFileName;
{-------------------------------------------------------------------}
begin
  SetKennung (Akennung);
  Result := Directory + DataFile;
end;

{ Öffnet Datendatei }
{-----------------------}
procedure TLangzeit.Open;
{-----------------------}
begin
  if FState = lgzKennung then
  begin
    BufFile := TBufFile.Create (GetDataSize, 100);
    BufFile.Open (Directory + DataFile);
    FState := lgzOpened;
    First;
  end
  else
    if FState = lgzClosed then
      raise TLGZException.Create ('Open: No Kennung');
end;

{ Schließt Datendatei, falls diese geöffnet war }
{------------------------}
procedure TLangzeit.Close;
{------------------------}
begin
  if FState = lgzOpened then
  begin
    BufFile.Free;
    FState := lgzKennung;
  end;
end;

{ Setzt Cursor an den Anfang der Datei und liest ersten Satz in Puffer }
{------------------------}
procedure TLangzeit.First;
{------------------------}
begin
  CheckOpened;
  BufFile.Position := 0;
  ReadBuf;
end;

{ Setzt Cursor an das Ende der Datei, nachdem der letzte Satz in den
  Puffer gelesen wurde }
{-----------------------}
procedure TLangzeit.Last;
{-----------------------}
begin
  CheckOpened;
  BufFile.Position := BufFile.Size - 1;
  ReadBuf;
end;

{ Setzt Cursor auf nächsten Datensatz und liest diesen in Puffer }
{-----------------------}
procedure TLangzeit.Next;
{-----------------------}
begin
  CheckOpened;
  BufFile.Next;
  ReadBuf;
end;

{ Positioniert Cursor auf beliebige Position }
{---------------------------------------------------}
function TLangzeit.Seek (Position: LongInt): Boolean;
{---------------------------------------------------}
begin
  Result := False;
  CheckOpened;
  BufFile.Position := Position;
  if not BufFile.Eof then
  begin
    Result := True;
  end;
end;

{ Positioniert Cursor auf beliebige Position und liest Datensatz ein }
{--------------------------------------------------------}
function TLangzeit.SearchPos (Position: LongInt): Boolean;
{--------------------------------------------------------}
begin
  Result := False;
  CheckOpened;
  BufFile.Position := Position;
  if not BufFile.Eof then
  begin
    ReadBuf;
    Result := True;
  end;
end;

procedure TLangzeit.Truncate;
begin
  CheckOpened;
  BufFile.Position := BufFile.Position - 1;
  BufFile.Truncate;
end;

{ TLGZStdData }

{---------------------------------------}
function TLGZStdData.GetDatum: TDateTime;
{---------------------------------------}
begin
  Result := EncodeDate (TLGZD (FBuf^).Datum.Year,
                        TLGZD (FBuf^).Datum.Month,
                        TLGZD (FBuf^).Datum.Day) +
            EncodeTime (TLGZD (FBuf^).Zeit.Hour,
                        0, 0, 0);
end;

{------------------------------------------------}
procedure TLGZStdData.SetDatum (Value: TDateTime);
{------------------------------------------------}
var
  FYear, FMonth, FDay, FHour, FMin, FSec, FMSec: Word;
begin
  DecodeDate (Value, FYear, FMonth, FDay);
  DecodeTime (Value, FHour, FMin, FSec, FMSec);
  TLGZD (FBuf^).Datum.Year := FYear;
  TLGZD (FBuf^).Datum.Month := FMonth;
  TLGZD (FBuf^).Datum.Day := FDay;
  TLGZD (FBuf^).Zeit.Hour := FHour;
end;

{---------------------------------------}
function TLGZStdData.GetSatzStatus: Byte;
{---------------------------------------}
begin
  Result := TLGZD (FBuf^).SatzStatus;
end;

{------------------------------------------------}
procedure TLGZStdData.SetSatzStatus (Value: Byte);
{------------------------------------------------}
begin
  TLGZD (FBuf^).SatzStatus := Value;
end;

{-----------------------------------------------}
function TLGZStdData.GetWert (Index: Word): Word;
{-----------------------------------------------}
begin
  Result := TLGZDExt (FBuf^).Kanal [Index].Wert;
end;

{-------------------------------------------------------}
procedure TLGZStdData.SetWert (Index: Word; Value: Word);
{-------------------------------------------------------}
begin
  TLGZDExt (FBuf^).Kanal [Index].Wert := Value;
end;

{------------------------------------------------------}
function TLGZStdData.GetKanalStatus (Index: Word): Byte;
{------------------------------------------------------}
begin
  Result := TLGZDExt (FBuf^).Kanal [Index].KanalStatus;
end;

{--------------------------------------------------------------}
procedure TLGZStdData.SetKanalStatus (Index: Word; Value: Byte);
{--------------------------------------------------------------}
begin
  TLGZDExt (FBuf^).Kanal [Index].KanalStatus := Value;
end;

{------------------------------------------------}
constructor TLGZStdData.Create (AKanalzahl: Word);
{------------------------------------------------}
begin
  inherited Create;
  FKanalzahl := AKanalzahl;
  FBufSize := sizeof (TLGZD) + FKanalZahl * sizeof (TLGZDKanal);
  GetMem (FBuf, FBufSize);
end;

{-----------------------------}
destructor TLGZStdData.Destroy;
{-----------------------------}
begin
  FreeMem (FBuf, FBufSize);
  inherited Destroy;
end;

{----------------------------------------------}
procedure TLGZStdData.Clear (ADatum: TDateTime);
{----------------------------------------------}
var
  i: Word;
begin
  FillChar (FBuf^, FBufSize, 0);
  SetDatum (ADatum);
  SetSatzStatus (0);
  for i := 0 to FKanalZahl - 1 do
  begin
    SetKanalStatus (i, $80);
    SetWert (i, 0);
  end;
end;

{ TLGZStd }

{------------------------------------------}
function TLGZStd.GetOrdnungsnummer: LongInt;
{------------------------------------------}
begin
  CheckOpened;
  Result := Round (Datum * 24);
end;

{-----------------------------------}
function TLGZStd.GetDatum: TDateTime;
{-----------------------------------}
begin
  CheckOpened;
  Result := EncodeDate (TLGZD (DataBuf^).Datum.Year,
                        TLGZD (DataBuf^).Datum.Month,
                        TLGZD (DataBuf^).Datum.Day) +
            EncodeTime (TLGZD (DataBuf^).Zeit.Hour,
                        0, 0, 0);
end;

{-----------------------------}
function TLGZStd.GetYear: Word;
{-----------------------------}
begin
  CheckOpened;
  Result := TLGZD (DataBuf^).Datum.Year;
end;

{------------------------------}
function TLGZStd.GetMonth: Word;
{------------------------------}
begin
  CheckOpened;
  Result := TLGZD (DataBuf^).Datum.Month;
end;

{----------------------------}
function TLGZStd.GetDay: Word;
{----------------------------}
begin
  CheckOpened;
  Result := TLGZD (DataBuf^).Datum.Day;
end;

{-----------------------------}
function TLGZStd.GetHour: Word;
{-----------------------------}
begin
  CheckOpened;
  Result := TLGZD (DataBuf^).Zeit.Hour;
end;

{-----------------------------------}
function TLGZStd.GetSatzStatus: Byte;
{-----------------------------------}
begin
  CheckOpened;
  Result := TLGZD (DataBuf^).SatzStatus;
end;

{-------------------------------------------}
function TLGZStd.GetWert (Index: Word): Word;
{-------------------------------------------}
begin
  CheckOpened;
  CheckIndex (Index);
  Result := TLGZDExt (DataBuf^).Kanal [Index].Wert;
end;

{--------------------------------------------------}
function TLGZStd.GetKanalStatus (Index: Word): Byte;
{--------------------------------------------------}
begin
  CheckOpened;
  CheckIndex (Index);
  Result := TLGZDExt (DataBuf^).Kanal [Index].KanalStatus;
end;

{-------------------------------------------}
procedure TLGZStd.SetDataFile (FileNr: Word);
{-------------------------------------------}
begin
  if isLangzeit
    then DataFile := Format ('LGZD%4.4d.DAT', [FileNr])
    else DataFile := Format ('MSTD%4.4d.DAT', [ManuNr]); { GeDa }
end;

{---------------------------------------}
function TLGZStd.GetDateiName: TFileName;  {kb 8.7.97}
{---------------------------------------}
begin
  if Length(DataFile) > 0 then
    Result:=DataFile
  else
    Result:='';
end;

{---------------------}
procedure TLGZStd.Open;
{---------------------}
begin
  inherited Open;
  FirstOrdnungsNummer := GetOrdnungsNummer;
end;

{---------------------------------------------}
function TLGZStd.CreateLGZStdData: TLGZStdData;
{---------------------------------------------}
begin
  Result := TLGZStdData.Create (FKanalzahl);
  Move (DataBuf^, Result.DataBuf^, Result.DataSize);
end;

{------------------------------------------------------------------------}
function TLGZStd.SearchOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
{------------------------------------------------------------------------}
begin
  Result := SearchPos (AOrdnungsNummer - FirstOrdnungsNummer);
end;

{--------------------------------------------------------------------------}
function TLGZStd.SearchGEOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
{--------------------------------------------------------------------------}
var
  Delta: LongInt;
begin
  Delta := AOrdnungsNummer - FirstOrdnungsNummer;
  if Delta <= 0 then
    Result := SearchPos (0)
  else
    Result := SearchPos (Delta);
end;

{--------------------------------------------------------}
function TLGZStd.SearchDatum (ADatum: TDateTime): Boolean;
{--------------------------------------------------------}
begin
  Result := SearchOrdnungsNummer (Round (ADatum * 24));
end;

{----------------------------------------------------------}
function TLGZStd.SearchGEDatum (ADatum: TDateTime): Boolean;
{----------------------------------------------------------}
begin
  Result := SearchGEOrdnungsNummer (Round (ADatum * 24));
end;

{---------------------------------}
function TLGZStd.GetDataSize: word;
{---------------------------------}
begin
  Result := sizeof (TLGZD) + FKanalZahl * sizeof (TLGZDKanal);
end;

{ TLGZTag }

function TLGZTag.GetOrdnungsnummer: LongInt;
begin
  CheckOpened;
  Result := Round (Datum);
end;

function TLGZTag.GetSatzStatus: Byte;
begin
  CheckOpened;
  Result := TLGZT (DataBuf^).SatzStatus;
end;

function TLGZTag.GetDatum: TDateTime;
begin
  CheckOpened;
  with TLGZT (DataBuf^).Datum do
    Result := EncodeDate (Year, Month, Day) +
              EncodeTime (GetStunden, 0, 0, 0);
end;

function TLGZTag.GetStunden: Byte;
begin
  CheckOpened;
  Result := TLGZT (DataBuf^).Stunden;
end;

function TLGZTag.GetEingangStatus (Index: Word): Byte;
begin
  CheckOpened;
  CheckIndex (Index);
  Result := TLGZTExt (DataBuf^).Kanal [Index].ZaehlerStatus
end;

function TLGZTag.GetEingangWert (Index: Word): LongInt;
begin
  CheckOpened;
  CheckIndex (Index);
  Result := TLGZTExt (DataBuf^).Kanal [Index].Wert;
end;

function TLGZTag.GetKontrollStatus (Index: Word): Byte;
begin
  CheckOpened;
  CheckIndex (Index);
  Result := TLGZTExt (DataBuf^).Kanal [Index + FKanalZahl].ZaehlerStatus
end;

function TLGZTag.GetKontrollWert (Index: Word): LongInt;
begin
  CheckOpened;
  CheckIndex (Index);
  Result := TLGZTExt (DataBuf^).Kanal [Index + FKanalZahl].Wert;
end;

procedure TLGZTag.SetDataFile (FileNr: Word);
begin
  if isLangzeit
    then DataFile := Format ('LGZT%4.4d.DAT', [FileNr])
    else DataFile := Format ('MTAG%4.4d.DAT', [ManuNr]); { GeDa }
end;

function TLGZTag.GetDateiName: TFileName;  {kb 8.7.97}
begin
  if Length(DataFile) > 0 then Result:=DataFile;
end;

function TLGZTag.GetDataSize: Word;
begin
  Result := sizeof (TLGZT) + 2 * FKanalZahl * sizeof (TLGZTKanal);
end;

procedure TLGZTag.Open;
begin
  inherited Open;
  FirstOrdnungsNummer := GetOrdnungsNummer;
end;

function TLGZTag.SearchOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
begin
  Result := SearchPos (AOrdnungsNummer - FirstOrdnungsNummer);
end;

function TLGZTag.SearchGEOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
var
  Delta: LongInt;
begin
  Delta := AOrdnungsNummer - FirstOrdnungsNummer;
  if Delta <= 0 then
    Result := SearchPos (0)
  else
    Result := SearchPos (Delta);
end;

function TLGZTag.SearchDatum (ADatum: TDateTime): Boolean;
begin
  Result := SearchOrdnungsNummer (Round (ADatum));
end;

function TLGZTag.SearchGEDatum (ADatum: TDateTime): Boolean;
begin
  Result := SearchGEOrdnungsNummer (Round (ADatum));
end;

procedure Register;
begin
  RegisterComponents ('Wieser', [TLGZStd]);
  RegisterComponents ('Wieser', [TLGZTag]);
end;
    
end.
