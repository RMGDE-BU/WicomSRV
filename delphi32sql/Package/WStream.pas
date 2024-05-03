{******************************************************************************}
{* Unit: erweiterte Stream-Klassen                                            *}
{* 09.12.2003 WW                                                              *}
{* 14.01.2015 WW  TTextFileStream.ReadLn beschleunigt                         *}
{* 23.09.2015 WW  TFileOfCharStream für gepuffertes, zeichenweises Lesen aus  *}
{*                Datei                                                       *}
{******************************************************************************}
unit WStream;

interface

uses
  Classes, WChars;

const
  C_FileOfChar_BufLen = 1024;  // Empirisch ermittelt, größerer Puffer bringt
                               // keinen weiteren Zeitgewinn 

type

  { FileStream-Klasse für Textdateien }

  TTextFileStream = class (TFileStream)
  public
    procedure ReadLn (var S: string);
    procedure WriteLn (S: string);
  end;

  { FileStream-Klasse für strukturierte Dateien }

  TFileOfRecStream = class (TFileStream)
  private
    function GetRecPosition: Longint;
    procedure SetRecPosition (RecPos: Longint);
    function GetRecCount: Longint;
    procedure SetRecCount (NewRecCount: Longint);
  protected
    SizeOfRec: integer;
  public
    constructor Create(const AFileName: string; AMode: Word; ASizeOfRec: integer);
    function ReadRec (var Rec): Longint;
    function WriteRec (const Rec): Longint;
    function SeekRec (RecOffset: Longint; Origin: Word): Longint;
    property RecPosition: Longint read GetRecPosition write SetRecPosition;
    property RecCount: Longint read GetRecCount write SetRecCount;
  end;

  { FileStream-Klasse für gepuffertes, zeichenweises Lesen }

  TFileOfCharStream = class (TObject)
  private
    FFileStream: TFileStream;
    FBuffer: array [0..C_FileOfChar_BufLen-1] of char;
    FBufferCount: integer;
    FBufferPos: integer;
    function GetPosition: Int64;
    procedure SetPosition(const Pos: Int64);
    function GetSize: Int64;
    function GetFileStream: TFileStream;
  public
    constructor Create(const FileName: string; Mode: Word); overload;
    destructor Destroy; override;
    function Read(var ch: char): Longint;
    function Seek(Offset: Longint; Origin: Word): Longint; overload;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload;
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize;
//    property FileStream: TFileStream read FFileStream;
    property FileStream: TFileStream read GetFileStream;
  end;

implementation

{ TTextFileStream }

{-----------------------------------------------}
procedure TTextFileStream.ReadLn (var S: string);
{-----------------------------------------------}
{ liest eine mit <CR>, <LF>, <CR> <LF> oder <LF> <CR> abgeschlossene Zeile aus Datei;
  Rückgabe: Zeile ohne Zeilenabschlusszeichen }
const
  C_ReadBufLen = 512;

var
  NextGelesen: integer;
  c, cNext: char;
  ReadBuf: array [0..C_ReadBufLen-1] of char;
  iReadBufPos: integer;
  iReadBufCount: integer;

begin
  S:='';

  iReadBufCount:=Read (ReadBuf, C_ReadBufLen);  // 14.01.2015, WW
  if iReadBufCount > 0 then
    iReadBufPos:=0
  else
    iReadBufPos:=-1;

  while (iReadBufPos >= 0) AND (iReadBufPos < iReadBufCount) do begin
    c:=ReadBuf [iReadBufPos];
    inc (iReadBufPos);

    // Zeilenabschluss-Prüfung erweitert, 18.01.2005 WW
    if (c = CR) OR (c = LF) then begin  // Zeilenabschluss
      // nachfolgendes Zeichen prüfen:
      if iReadBufPos = iReadBufCount then
        NextGelesen:=Read (cNext, 1)
      else begin
        cNext:=ReadBuf [iReadBufPos];  // 14.01.2015, WW
        NextGelesen:=1;

        { Position in Datei synchronisieren: }
        Position:=Position - (iReadBufCount - iReadBufPos - 1);
      end;

      if NextGelesen > 0 then begin
        if c = CR then begin
          // LF gehört noch zum Zeilenabschluss, alle andereren Zeichen bereits
          // zur nächsten Zeile:
          if cNext <> LF then
            Seek (-1, soFromCurrent);
        end
        else if c = LF then begin
          // CR gehört noch zum Zeilenabschluss, alle andereren Zeichen bereits
          // zur nächsten Zeile:
          if cNext <> CR then
            Seek (-1, soFromCurrent);
        end;
      end;
      Break;
    end
    else begin
      S:=S + c;

      if iReadBufPos = iReadBufCount then begin  // 14.01.2015, WW
        iReadBufCount:=Read (ReadBuf, C_ReadBufLen);
        if iReadBufCount > 0 then
          iReadBufPos:=0
        else
          iReadBufPos:=-1;
      end;
    end;
  end;
end;

{--------------------------------------------}
procedure TTextFileStream.WriteLn (S: string);
{--------------------------------------------}
{ schreibt eine mit CR LF abgeschlossene Zeile in Datei;
  Übergabe: Zeile ohne CR LF }
var
  Zeile: string;
begin
  Zeile:=S + CR + LF;
  Write (Zeile [1], length (Zeile));
end;


{ TFileOfRecStream }

{---------------------------------------------------------------------------------------------}
constructor TFileOfRecStream.Create(const AFileName: string; AMode: Word; ASizeOfRec: integer);
{---------------------------------------------------------------------------------------------}
{ FileStream für strukturierte Datei createn (ASizeOfRec = Record-Größe) }
begin
  inherited Create (AFileName, AMode);
  SizeOfRec:=ASizeOfRec;
end;

{---------------------------------------------------}
function TFileOfRecStream.ReadRec (var Rec): Longint;
{---------------------------------------------------}
{ liest einen Record aus Datei }
begin
  Result:=Read (Rec, SizeOfRec);
end;

{------------------------------------------------------}
function TFileOfRecStream.WriteRec (const Rec): Longint;
{------------------------------------------------------}
{ schreibt einen Record in Datei }
begin
  Result:=Write (Rec, SizeOfRec);
end;

{----------------------------------------------------------------------------}
function TFileOfRecStream.SeekRec (RecOffset: Longint; Origin: Word): Longint;
{----------------------------------------------------------------------------}
{ positioniert record-bezogen in der Datei (Origin wie bei TFileStream.Seek) }
begin
  Result:=Seek (RecOffset * SizeOfRec, Origin);
end;

{------------------------------------------------}
function TFileOfRecStream.GetRecPosition: Longint;
{------------------------------------------------}
{ gibt die aktuelle, recordbezogene Position in der Datei zurück }

begin
  Result:=Position DIV SizeOfRec;
end;

{---------------------------------------------------------}
procedure TFileOfRecStream.SetRecPosition(RecPos: Longint);
{---------------------------------------------------------}
{ positioniert record-bezogen in der Datei }
begin
  Position:=RecPos * SizeOfRec;
end;

{---------------------------------------------}
function TFileOfRecStream.GetRecCount: Longint;
{---------------------------------------------}
{ gibt die Anzahl der in der Datei enthaltenen Records zurück }
begin
  Result:=Size DIV SizeOfRec;
end;

{------------------------------------------------------------}
procedure TFileOfRecStream.SetRecCount (NewRecCount: Longint);
{------------------------------------------------------------}
{ begrenzt die Datei auf die übergebene Anzahl von Records (am Ende abschneiden) }
begin
  Size:=NewRecCount * SizeOfRec;
end;


{ TFileOfCharStream }

{-----------------------------------------------------------------------}
constructor TFileOfCharStream.Create(const FileName: string; Mode: Word);
{-----------------------------------------------------------------------}
begin
  inherited Create;
  FBufferCount:=0;
  FBufferPos:=0;

  FFileStream:=TFileStream.Create(Filename, Mode);
end;

{-----------------------------------}
destructor TFileOfCharStream.Destroy;
{-----------------------------------}
begin
  FFileStream.Free;
  inherited Destroy;
end;

{-----------------------------------------------------}
function TFileOfCharStream.Read(var ch: char): Longint;
{-----------------------------------------------------}
begin
  if (FBufferCount = 0) OR (FBufferPos >= C_FileOfChar_BufLen) then begin
    // aus Datei lesen und Puffer füllen:
    FillChar(FBuffer, SizeOf(FBuffer), 0);
    FBufferCount:=FFileStream.Read(FBuffer, C_FileOfChar_BufLen);
    FBufferPos:=0
  end;

  if (FBufferCount > 0) AND (FBufferPos < FBufferCount) then begin
    // Zeichen aus Puffer lesen
    ch:=FBuffer[FBufferPos];
    Result:=1;
    inc (FBufferPos);
  end else
    Result:=0;
end;

{----------------------------------------------------------------------}
function TFileOfCharStream.Seek(Offset: Longint; Origin: Word): Longint;
{----------------------------------------------------------------------}
begin
  if (Origin = soFromCurrent) then
    Result:=FFileStream.Seek(Offset - FBufferCount + FBufferPos, Origin)
  else
    Result:=FFileStream.Seek(Offset, Origin);

  FBufferCount:=0;  // Puffer muß neu gelesen werden
  FBufferPos:=0;
end;

{-------------------------------------------------------------------------------}
function TFileOfCharStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{-------------------------------------------------------------------------------}
begin
  if (Origin = soCurrent) then
    Result:=FFileStream.Seek(Offset - FBufferCount + FBufferPos, Origin)
  else
    Result:=FFileStream.Seek(Offset, Origin);

  FBufferCount:=0;  // Puffer muß neu gelesen werden
  FBufferPos:=0;
end;

{--------------------------------------------}
function TFileOfCharStream.GetPosition: Int64;
{--------------------------------------------}
begin
  Result:=FFileStream.Position - FBufferCount + FBufferPos;
end;

{--------------------------------------------------------}
procedure TFileOfCharStream.SetPosition(const Pos: Int64);
{--------------------------------------------------------}
begin
  FFileStream.Position:=Pos;
  FBufferCount:=0;  // Puffer muß neu gelesen werden
  FBufferPos:=0;
end;

{----------------------------------------}
function TFileOfCharStream.GetSize: Int64;
{----------------------------------------}
begin
  Result:=FFileStream.Size;
end;

{----------------------------------------------------}
function TFileOfCharStream.GetFileStream: TFileStream;
{----------------------------------------------------}
begin
  Result:=FFileStream;
  SetPosition(GetPosition);  // bei externem Zugriff auf FileStream muß die
                             // physikalische Dateiposition synchronisiert sein !
end;

end.
