{-------------------------------------------------------------------}
{ unit ObjFile                                                      }
{ 30.09.1996 DA, Copyright Karl Wieser GmbH                         }
{ Klasse für gepufferten Zugriff auf Dateien                        }
{ 20.01.2006 WW, resourcestrings                                    }
{-------------------------------------------------------------------}

unit Objfile;

interface

uses
  winprocs,Sysutils;

{$I-}

type

  EBufFileException = class (Exception);

  TBufFile = class (TObject)
  private
    FHandle: File;         { Dateihandle }
    FDataSize: Word;       { Datensatzgröße in Byte }
    FBufData: Word;        { Maximale Anzahl der Datensätze in Puffer }
    FBuffer: PChar;        { Puffer }
    FXBeg: Longint;        { Dateiposition des ersten Datensatzes in Puffer }
    FXEnd: Longint;        { Dateiposition des letzten Datensatzes in Puffer }
    FOpened: Boolean;
    FPosition: LongInt;
    FSize: LongInt;
    function GetEof: Boolean;
    function GetPosition: LongInt;
    procedure SetPosition (Value: LongInt);
    function GetSize: LongInt;
    procedure CheckOpen;
    procedure CheckSize;
    procedure LoadBuffer;
    function OpenHandle (CreateIt: Boolean): Boolean;
    procedure CloseHandle;
  public
    constructor Create (ADataSize, ABufData: Word);
    destructor Destroy; override;
    procedure Open (const FileName: string);
    procedure OpenCreate (const FileName: string);
    procedure Close;
    procedure Read (var Buffer);
    procedure Next;
    procedure Truncate;
    property Eof: Boolean read GetEof;
    property Position: LongInt read GetPosition write SetPosition;
    property Size: LongInt read GetSize;
  end;

implementation

resourcestring
  CBF_NotOpen    = 'Datei nicht geöffnet';
  CBF_CanNotOpen = 'Datei kann nicht geöffnet werden';             
  CBF_Corrupted  = 'Datei beschädigt';

{----------}
{ TBufFile }
{----------}

{ private - Methoden }

{ Eof = True, wenn Cursor nicht auf gültigen Datensatz zeigt }
{--------------------------------}
function TBufFile.GetEof: Boolean;
{--------------------------------}
begin
  Result := (Position < 0) or (Position >= Size);
end;

{ Liefert die aktuelle Cursorposition }
{-------------------------------------}
function TBufFile.GetPosition: LongInt;
{-------------------------------------}
begin
  CheckOpen;
  Result := FPosition;
end;

{ Setzt Cursor auf beliebige Position }
{----------------------------------------------}
procedure TBufFile.SetPosition (Value: LongInt);
{----------------------------------------------}
begin
  CheckOpen;
  FPosition := Value;
end;

{ Liefert Größe der Datei in Anzahl von Datensätzen }
{---------------------------------}
function TBufFile.GetSize: LongInt;
{---------------------------------}
begin
  CheckOpen;
  Result := FSize DIV FDataSize
end;

{ Überprüft Datei, ob sie geöffnet ist }
{---------------------------}
procedure TBufFile.CheckOpen;
{---------------------------}
begin
  if not FOpened then
    raise EBufFileException.Create (CBF_NotOpen);
end;

{ Größe der Datei muß ein ganzzahlig Vielfaches der Datensatzgröße sein }
{---------------------------}
procedure TBufFile.CheckSize;
{---------------------------}
begin
  if (FSize MOD FDataSize) <> 0 then
    raise EBufFileException.Create (CBF_Corrupted);
end;

{ Liest einen Bereich von Datensätzen in der nähe der aktuellen
  Cursorposition in den Puffer. Die Datei wird hierzu kurz geöffnet }
{----------------------------}
procedure TBufFile.LoadBuffer;
{----------------------------}
var
  n: LongInt;
begin
  CheckOpen;
  if OpenHandle (False) then
  begin
    try
      FXBeg := FPosition - FBufData SHR 2;
      if FXBeg < 0 then
        FXBeg := 0;
      n := FBufData;
      if (FXBeg + n) > Size then
        n := Size - FXBeg;
      Seek (FHandle, FXBeg * FDataSize);
      BlockRead (FHandle, FBuffer^, FDataSize * n);
      FXEnd := FXBeg + n;
    finally
      CloseHandle;
    end;
  end;
end;

{ öffnet Dateihandle }
{--------------------------------------------------------}
function TBufFile.OpenHandle (CreateIt: Boolean): Boolean;
{--------------------------------------------------------}
var
  OldFileMode: Byte;
  Ticks: DWord;
  OpenResult: Integer;
begin
  Result := False;
  OldFileMode := FileMode;
  FileMode := $12;
  try
    Ticks := GetTickCount;
    repeat
      Reset (FHandle, 1);
      OpenResult := IoResult;
      Result := (OpenResult = 0);
    until ((GetTickCount - Ticks) > 5000) or (OpenResult <> 5);
    if OpenResult = 2 then
    begin
      Rewrite (FHandle, 1);
      OpenResult := IoResult;
      Result := (OpenResult = 0);
    end;
  finally
    FileMode := OldFileMode;
    if Result then
      FSize := FileSize (FHandle)
    else
      FSize := 0;
  end;
end;

{ schließt Dateihandle }
{-----------------------------}
procedure TBufFile.CloseHandle;
{-----------------------------}
begin
  System.Close (FHandle);
  if IOResult <> 0 then;
end;

{ Konstruktor }
{------------------------------------------------------}
constructor TBufFile.Create (ADataSize, ABufData: Word);
{------------------------------------------------------}
begin
  inherited Create;
  FOpened := False;
  FDataSize := ADataSize;
  FBufData := ABufData;
  GetMem (FBuffer, FDataSize * FBufData);
  FXBeg := 0;
  FXEnd := 0;
  FPosition := 0;
  FSize := 0;
end;

{ Destruktor }
{--------------------------}
destructor TBufFile.Destroy;
{--------------------------}
begin
  Close;
  FreeMem (FBuffer, FDataSize * FBufData);
  inherited Destroy;
end;

{ Öffnet Datei }
{-----------------------------------------------}
procedure TBufFile.Open (const FileName: string);
{-----------------------------------------------}
begin
  Close;
  Assign (FHandle, FileName);
  FOpened := OpenHandle (False);
  CloseHandle;
  if not FOpened then
    raise EBufFileException.Create (CBF_CanNotOpen)
  else
  begin
    CheckSize;
  end;
end;

{ Öffnet Datei }
{-----------------------------------------------------}
procedure TBufFile.OpenCreate (const FileName: string);
{-----------------------------------------------------}
begin
  Close;
  Assign (FHandle, FileName);
  FOpened := OpenHandle (True);
  CloseHandle;
  if not FOpened then
    raise EBufFileException.Create (CBF_CanNotOpen)
  else
  begin
    CheckSize;
  end;
end;

{ Schließt Datei }
{-----------------------}
procedure TBufFile.Close;
{-----------------------}
begin
  if FOpened then
  begin
    CloseHandle;
    FOpened := False;
  end;
end;

{ Liest aktuellen Datensatz und inkrementiert die Cursorposition um 1 }
{-----------------------------------}
procedure TBufFile.Read (var Buffer);
{-----------------------------------}
begin
  if (FPosition < 0) or (FPosition >= Size) then
    FillChar (Buffer, FDataSize, 0)
  else
  begin
    if (FPosition < FXBeg) or (FPosition >= FXEnd) then
      LoadBuffer;
    Move (FBuffer [(FPosition - FXBeg) * FDataSize], Buffer, FDataSize);
  end;
end;

{----------------------}
procedure TBufFile.Next;
{----------------------}
begin
  Inc (FPosition);
end;

{--------------------------}
procedure TBufFile.Truncate;
{--------------------------}
begin
  CheckOpen;
  if OpenHandle (False) then
  begin
    Seek (FHandle, (FPosition + 1) * FDataSize);
    System.Truncate (FHandle);
    CloseHandle;
  end;
  FXBeg := 0;
  FXEnd := 0;
end;

end.

