{***************************************************************************}
{ H.-P.R. aus CHIP Pascal Heft Nr.19  07.07.93                              }
{ Funktionen fÅr Netzwerk-Programmierung 10.09.93  CP                       }
{ Erweiterungen fÅr Windows-Zielplattform  04.07.94 CP                      }
{ Fehler im Sperrdateiobjekt beseitigt 27.10.94 CP                          }
{ 22.03.95 CP Fehlerkonstanten bei Dateizugriffen                           }
{ 17.02.99 WW f¸r 32-Bit-Delphi                                             }
{ 23.01.01 WW um Delphi-5-Fehlercode f¸r "Access denied" erweitert          }
{ 12.02.01 SM NETDelete Funktion hinzugef¸gt                                }
{ 18.08.03 GD NETAppendText und NETRewriteText Funktion hinzugef¸gt         }
{***************************************************************************}
unit novell;

interface

uses
  Classes, SysUtils, T_Zeit, WStream;

const

   { FileMode-Konstanten : }

   { Was will ich mit der Datei machen }
   ForRead   = $00;  { Nur lesen                         }
   ForWrite  = $01;  { Nur schreiben                     }
   ForAll    = $02;  { Beides = Default bei Turbo-Pascal }

   { Was d¸rfen die anderen noch }
   Compatible = 0;   { Normalerweise gleich DenyAll      }
                     { Default bei Turbo-Pascal          }
   DenyAll    = $10; { Alles verboten (auch ÷ffnen!)     }
   DenyWrite  = $20; { Lesen erlaubt, schreiben verboten }
   DenyRead   = $30; { Schreiben erlaubt, lesen verboten }
   DenyNone   = $40; { Nichts verboten, alles erlaubt    }


   { Fehlerr¸ckgabe-Konstanten bei Dateizugriffen mit NetReset }
   ERR_FILE_NOT_FOUND = -2;


type

  { f¸r Netzzugriff erweiterte TFileStream-Klassen }

  TFileStreamExt = class (TFileStream)
  public
    constructor Create (const AFileName: string; AMode: Word; var OK: boolean);
  end;

  TTextFileStreamExt = class (TTextFileStream)
  public
    constructor Create (const AFileName: string; AMode: Word; var OK: boolean);
  end;

  TFileOfRecStreamExt = class (TFileOfRecStream)
  public
    constructor Create(const AFileName: string; AMode: Word; ASizeOfRec: integer;
                       var OK: boolean);
  end;

function NetReset(var EineDatei; Groesse: integer; Mode: byte): integer;
{ ‹bergabeparameter:
  EineDatei: mit AssignFile initialisierte Dateivariable;
  Groesse:   mit SizeOf(...) bestimmte Strukturgrˆﬂe;
  Mode:      bestimmt Zugriffsberechtigung (s. o.);
  R¸ckgabewert: (-1)*IOResult des Reset-Versuchs bzw. Dateigrˆﬂe  }

function NetDelete(var EineDatei): Integer;
{ ‹bergabeparameter:
  EineDatei: mit AssignFile initialisierte Dateivariable;
  R¸ckgabewert: (-1)*IOResult des Reset-Versuchs bzw. 0 falls lˆschen ok }

function NetAppendText(var pDatei: TextFile): boolean;

function NetRewriteText(var pDatei: TextFile): boolean;


function NetResetText(var pDatei: TextFile): boolean;

implementation

const
  CMaxVersuche =   40;   { max. Anzahl der Leseversuche beim Netzzugriff;
                           bisher 1000,  WW 23.01.2001 }
  CMaxVerzZeit = 1000;   { max. Wartezeit zwischen zwei Leseversuchen in ms }


{----------------------------------------------------------------------}
function NetAppendText(var pDatei: TextFile): boolean;
{----------------------------------------------------------------------}
var
  ResetFehler : integer;
  i           : integer;
begin
  Randomize;
  i:=0;
  {$I-}
  repeat
    Append(pDatei);
    inc(i);
    ResetFehler := IOResult;
    if (ResetFehler = 32) OR (ResetFehler = 5) then Delay(random(CMaxVerzZeit));
  until ((ResetFehler <> 32) and     { 32: Access denied unter Delphi 5 }
         (ResetFehler <>  5)) or     {  5: wegen Kompabilit‰t zu fr¸heren Compilerversionen }
        (i > CMaxVersuche);
  {$I+}
  Result := (ResetFehler = 0);
end;

{----------------------------------------------------------------------}
function NetRewriteText(var pDatei: TextFile): boolean;
{----------------------------------------------------------------------}
var
  ResetFehler : integer;
  i           : integer;
begin
  Randomize;
  i:=0;
  {$I-}
  repeat
    Rewrite(pDatei);
    inc(i);
    ResetFehler := IOResult;
    if (ResetFehler = 32) OR (ResetFehler = 5) then Delay(random(CMaxVerzZeit));
  until ((ResetFehler <> 32) and     { 32: Access denied unter Delphi 5 }
         (ResetFehler <>  5)) or     {  5: wegen Kompabilit‰t zu fr¸heren Compilerversionen }
        (i > CMaxVersuche);
  {$I+}
  Result := (ResetFehler = 0);
end;

{----------------------------------------------------------------------}
function NetResetText(var pDatei: TextFile): boolean;
{----------------------------------------------------------------------}
var
  ResetFehler : integer;
  i           : integer;
begin
  Randomize;
  i:=0;
  {$I-}
  repeat
    Reset(pDatei);
    inc(i);
    ResetFehler := IOResult;
    if (ResetFehler = 32) OR (ResetFehler = 5) then Delay(random(CMaxVerzZeit));
  until ((ResetFehler <> 32) and     { 32: Access denied unter Delphi 5 }
         (ResetFehler <>  5)) or     {  5: wegen Kompabilit‰t zu fr¸heren Compilerversionen }
        (i > CMaxVersuche);
  {$I+}
  Result := (ResetFehler = 0);
end;

{----------------------------------------------------------------------}
function NetReset(var EineDatei; Groesse: integer; Mode: byte): integer;
{----------------------------------------------------------------------}
var
  ResetFehler : integer;
  Datei       : file;
  i           : integer;

begin
  i:=0;
  TFileRec(Datei):=TFileRec(EineDatei);
  TFileRec(Datei).RecSize:=Groesse;
  Randomize;
  {$I-}
  FileMode:=Mode;
  repeat
    reset(Datei,TFileRec(Datei).RecSize);
    inc(i);
    ResetFehler:=IOResult;
    if (ResetFehler = 32) OR (ResetFehler = 5) then
      delay(random(CMaxVerzZeit));
  until ((ResetFehler <> 32) and     { 32: Access denied unter Delphi 5 }
         (ResetFehler <>  5)) or     {  5: wegen Kompabilit‰t zu fr¸heren Compilerversionen }
        (i > CMaxVersuche);
  FileMode:=ForAll; { zur¸ck auf Standardwert }
  {$I+}
  TFileRec(EineDatei):=TFileRec(Datei);
  if ResetFehler=0 then
    Result:=FileSize(Datei)
  else
    Result:=-ResetFehler;
end;

{----------------------------------------------------------------------}
function NetDelete(var EineDatei): Integer;
{----------------------------------------------------------------------}
var
  DeleteFehler : integer;
  Datei       : file;
  i           : integer;

begin
  i:=0;
  Randomize;
  TFileRec(Datei):=TFileRec(EineDatei);
  {$I-}
  repeat
    inc(i);
    erase(Datei);
    DeleteFehler:=IOResult;
    if ((DeleteFehler=32) or (DeleteFehler=5)) then delay(random(CMaxVerzZeit));
  until ((DeleteFehler <> 32) and     { 32: Access denied unter Delphi 5 }
         (DeleteFehler <>  5)) or     {  5: wegen Kompabilit‰t zu fr¸heren Compilerversionen }
        (i > CMaxVersuche);
  if (DeleteFehler=0) then
    Result:=0
  else
    Result:=-DeleteFehler;
end;


{ TFileStreamExt }

{----------------------------------------------------------------------------------------}
constructor TFileStreamExt.Create (const AFileName: string; AMode: Word; var OK: boolean);
{----------------------------------------------------------------------------------------}
{ File f¸r gemeinsamen Zugriff ˆffnen/erzeugen;
  ‹bergabe: FileName (wie bei TFileStream.Create)
            Mode (wie bei TFileStream.Create)
  R¸ckgabe: Ok - true, wenn File sp‰testens nach MaxZugriffVersuche geˆffnet werden konnte }
var
  i: integer;
begin
  OK:=false;
  Randomize;
  i:=0;
  repeat
    inc (i);
    try
      inherited Create (AFileName, AMode);
      OK:=true;
    except
      on Exception do
        Delay (Random (CMaxVerzZeit));             { keine weiteren gleichzeitigen Zugriffe }
    end;
  until Ok OR (i >= CMaxVersuche);
end;


{ TTextFileStreamExt }

{--------------------------------------------------------------------------------------------}
constructor TTextFileStreamExt.Create (const AFileName: string; AMode: Word; var OK: boolean);
{--------------------------------------------------------------------------------------------}
{ Text-File gemeinsamen Zugriff ˆffnen/erzeugen;
  ‹bergabe: FileName (wie bei TFileStream.Create)
            Mode (wie bei TFileStream.Create)
  R¸ckgabe: Ok - true, wenn File sp‰testens nach MaxZugriffVersuche geˆffnet werden konnte }
var
  i: integer;
begin
  OK:=false;
  Randomize;
  i:=0;
  repeat
    inc (i);
    try
      inherited Create (AFileName, AMode);
      OK:=true;
    except
      on Exception do
        Delay (Random (CMaxVerzZeit));             { keine weiteren gleichzeitigen Zugriffe }
    end;
  until Ok OR (i >= CMaxVersuche);
end;


{ TFileOfRecStreamExt }

{----------------------------------------------------------------------------}
constructor TFileOfRecStreamExt.Create (const AFileName: string; AMode: Word;
                                        ASizeOfRec: integer; var OK: boolean);
{----------------------------------------------------------------------------}
{ strukturiertes File gemeinsamen Zugriff ˆffnen/erzeugen;
  ‹bergabe: FileName (wie bei TFileStream.Create)
            Mode (wie bei TFileStream.Create)
            ASizeOfRec = Record-Grˆﬂe
  R¸ckgabe: Ok - true, wenn File sp‰testens nach MaxZugriffVersuche geˆffnet werden konnte }
var
  i: integer;
begin
  OK:=false;
  Randomize;
  i:=0;
  repeat
    inc (i);
    try
      inherited Create (AFileName, AMode, ASizeOfRec);
      OK:=true;
    except
      on Exception do
        Delay (Random (CMaxVerzZeit));             { keine weiteren gleichzeitigen Zugriffe }
    end;
  until Ok OR (i >= CMaxVersuche);
end;

end.
