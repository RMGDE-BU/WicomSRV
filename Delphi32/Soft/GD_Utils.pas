{------------------------------------------------------------------------------}
{ 22.12.1997 GD;  Unit mit Konvertierungsproceduren für PChar                  }
{ 19.06.1998 GD;  Änderung: ADR, OFS fallen heraus (nicht bei 32-Bit)          }
{ 20.09.2000 GD;  'UnixTimeToDateTime' für negative Eingangswerte              }
{ 22.01.2001 GD;  'GetMyUserName' für Dummies                                  }
{ 22.01.2001 GD;  'Delay' herausgenommen (ist in T_Tools)                      }
{ 15.03.2001 GD;  Fehler bei UnixToDateTime korrigiert                         }
{ 03.04.2001 GD;  Timerproc für das Freigeben von Objekten                     }
{ 23.05.2001 GD;  Zusammenführen von Menüs (AutoMerge funktioniert nicht)      }
{ 22.07.2001 GD;  Versionsinfos für Dummies                                    }
{ 20.10.2001 GD;  WStrToFloatDef                                               }
{ 24.01.2002 GD;  Typ TMyTabStringList                                         }
{ 24.01.2002 GD;  Overload-Variante von Round (Runden auf Stellen hinter Komma)}
{ 24.01.2002 GD;  ReconvertLine                                                }
{ 22.05.2002 GD;  AnsiToOem, OemToAnsi                                         }
{ 19.07.2002 GD;  Typ TMyPointerList                                           }
{ 14.08.2003 GD;  Typ TFileStringList                                          }
{ 04.01.2005 GD;  WGetLogicalDrives, WGetUserName                              }
{ 04.01.2006 GD;  WOSShutDown, WWaitExecute                                    }
{ 09.03.2006 GD;  GDCopyControl, GDFindControlWithThisTagProperty              }
{ 13.02.2007 WW;  WGetVersionInfo                                              }
{ 13.06.2007 GD;  GDCopyControl erweitert, GDDeleteControl neu                 }
{ 26.10.2007 WW;  resourcestrings                                              }
{ 03.09.2009 GD;  GDRunProcess, WriteErrorLog                                  }
{ 27.08.2010 GD;  TMyThreadListCustom                                          }
{ 04.04.2013 GD;  Default beim Beenden von Applikationen auf 10 sek. erhöht    }
{ 25.06.2013 WW;  GDGetServiceImagePath                                        }
{ 10.12.2013 WW;  WGetVersionInfo: Nebenversion u. Ausgabe durch Punkt getrennt}
{ 01.04.2013 GD;  GDPostMessage, TMyStringObject, TMy2StringObject             }
{ 23.05.2014 WN;  WriteErrorLog: zus. Log-Type (DEBUG, INFO, WARNING, ERROR)   }
{ 26.06.2014 WN;  WriteErrorLog: mit Möglichkeit der einmaligen Definition     }
{                 eines globalen LogPfades                                     }
{ 26.01.2015 GD;  GDCopyControl erweitert                                      }
{ 12.08.2015 WW;  WriteErrorLog: bei leerem Fehlertext nur den LogPfad setzen  }
{                 (ohne Schreiben)                                             }
{ 21.09.2015 WW;  WGlobalMemoryStatusEx                                        }
{ 26.04.2018 WW;  WWaitExecute liefert Exitcode als Ergebnis; TStringObjectList}
{ 07.06.2022 WW;  Anpassung Cursor setzen in EnableMyControls                  }
{ 14.06.2022 WW;  SetControlCheckedNoEvent                                     }
{ 28.06.2023 WW;  WWaitExecute mit optionalem Parameter "Anzeige"              }
{------------------------------------------------------------------------------}

unit GD_Utils;

interface

uses
  Windows, sysutils, classes, Forms, Graphics, StdCtrls, Controls, Clipbrd,
  ExtCtrls, Menus, Math, FileCtrl, ShlObj, Buttons, Registry, ComCtrls, WinSock,
  WStream, Novell, PsApi, Messages, WinSvc, Grids;

const
   NUL = $00;
   SOH = $01;
   STX = $02;
   ETX = $03;
   EOT = $04;
   ENQ = $05;
   ACK = $06;
   BEL = $07;
   BS  = $08;
   HT  = $09;
   LF  = $0A;
   VT  = $0B;
   FF  = $0C;
   CR  = $0D;
   SO  = $0E;
   SI  = $0F;
   DLE = $10;
   DC1 = $11;
   DC2 = $12;
   DC3 = $13;
   DC4 = $14;
   NAK = $15;
   SYN = $16;
   ETB = $17;
   CAN = $18;
   EM  = $19;
   SUB = $1A;
   ESC = $1B;
   FS  = $1C;
   GS  = $1D;
   RS  = $1E;
   US  = $1F;

type
  { Typisierung der Log-Einträge }
  TErrorLogType = (elt_Debug, elt_Info, elt_Warning, elt_Error);  // 26.06.2014  WN

  // Callback-Proceduren
  TSimpleProcedure = procedure () of object;
  TCBCaptionFunction = function (const sCaption: string): integer of object;
  TSetCaptionEvent = procedure (sCaption: string) of object;
  TTwoTextsEvent = procedure (const sText1, sText2: string) of object;
  TNotifyString = procedure(iState: byte; sText: string) of object;
  TNotifyIntegerString = procedure(iInt: integer; sText: string) of object;
  TNotifyInteger = procedure(iInteger: integer) of object;
  TNotifyIntInt = procedure(iInt1, iInt2: integer) of object;
  TNotifyBoolean = procedure(bBool: boolean) of object;
  TNotifyFloatFloatInteger =
    procedure(fFloat1, fFloat2: double; iInteger: integer) of object;
  TCbBoolFunction = function: boolean of object;
  TCBLogTypeProc =
    procedure (const sText: string; aLogType: TErrorLogType) of object;  // 12.08.2019, WW

  TStr40           = String[40];
  PStr40           = ^TStr40;
  PInteger         = ^integer;
  PInt64           = ^Int64;
  PDouble          = ^double;
  TCharSet         = set of Char;
  TByteSet         = set of Byte;

  DWORDLONG = UInt64;

  PMemoryStatusEx = ^TMemoryStatusEx;
  TMemoryStatusEx = packed record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: DWORDLONG;
    ullAvailPhys: DWORDLONG;
    ullTotalPageFile: DWORDLONG;
    ullAvailPageFile: DWORDLONG;
    ullTotalVirtual: DWORDLONG;
    ullAvailVirtual: DWORDLONG;
    ullAvailExtendedVirtual: DWORDLONG;
  end;

  TMyTabList       = class;

  TMyStringObject = class(TObject)
    constructor Create(const sText: string);
  private
    FText : string;
  protected
  public
    property Text: string read FText;
  end;

  TMy2StringObject = class(TObject)
    constructor Create(const sText1, sText2: string);
  private
    FText1 : string;
    FText2 : string;
  protected
  public
    property Text1: string read FText1 write FText1;
    property Text2: string read FText2 write FText2;
  end;

  TMyPointerList = class(TList)
  private
  protected
  public
    procedure Delete(iIndex: integer); virtual;
    procedure Clear; override;
  end;

  TMyObjectList = class(TList)
  private
  protected
  public
    procedure Delete(iIndex: integer); virtual;
    procedure Clear; override;
  end;

  TMyTabList = class(TStringList)
    destructor Destroy; override;
  private
    function GetCol(iIndex: integer): TStrings;
  protected
  public
    procedure Delete(iIndex: integer); override;
    procedure Clear; override;
    property Col [iIndex: integer]: TStrings read GetCol;
  end;

  { Stringliste mit Objekten }
  TStringObjectList = class (TStringList)
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(iIndex: integer); override;
  end;

  TMyThreadListCustom = class(TThreadList)
    constructor Create; overload; virtual;
    constructor Create(pObjectClass: TClass); overload; virtual;
    destructor Destroy; override;
  private
    FObjectClass : TClass;
    FModified    : boolean;
  protected
    function GetCount: integer; virtual;
    function GetObject(iIndex: integer): TObject; virtual;
    procedure SetObject(iIndex: integer; pObject: TObject); virtual;
  public
    function AddObject(pObject: TObject): integer; virtual;
    procedure InsertObject(iIndex: integer; pObject: TObject); virtual;
    procedure DeleteObject(iIndex: integer); virtual;
    procedure Clear; virtual;
    property ObjectClass: TClass read FObjectClass write FObjectClass;
    property Count: integer read GetCount;
    property MyObject [iIndex: integer]: TObject read GetObject write SetObject;
    property Modified: boolean read FModified write FModified;
  end;

{ CB-Fktn. für String-Sortierung in Stringliste      }
function StringListStrCompare(
  List: TStringList; Index1, Index2: Integer): Integer;
function StringListObjectSort(
  List: TStringList; Index1, Index2: Integer): integer;

function GetLastSteuerzeichen(Value: string): char;
function GetStringPart(
  const sString: string; iIndex: integer; cSeparator: char = #31): string;
function ReplaceStringPart(const sString, sPart: string;
  iIndex: integer; cSeparator: char = #31): string;
function SeparatedToCommaText(const sSeparator, sText: string): string;
function PipedToCommaText(const sPipedText: string): string;
function CommaToSeparatedText(const sSeparator, sCommaText: string): string;
function CommaToPipedText(const sCommaText: string): string;
function ChangeStringPart(sString, sPart1, sPart2: string): string;
function charToString(thisChar: char): string;
function CharStrToString(sCharstring: string): string;
function MyAnsiToOem(sText: string): string;   // 22.05.20002
function MyOemToAnsi(sText: string): string;   // 22.05.20002

function SelectElement(vZeile: string; cStart, cStop: char):string;
function SelectDatenElement(vZeile: string):string;
//function GetElementNachSteuerzeichenIndex(
//  sString: string; iIndex: integer): string;

procedure WriteErrorLog(sFehler: string; sFileName: TFileName = '';
                        aLogType: TErrorLogType = elt_Error;
                        sLogPath: TFileName = '');  // 26.06.2014  WN
function EstimatedTextLengthOf(Value: string; pFont: TFont): integer;
function EstimatedTextHeightOf(Value: string; pFont: TFont): integer;
function GetControlImage(pControl: TWinControl): TBitmap;
procedure CopyControlImageToClipBoard(pControl: TWinControl; pHandle: HWND = 0);
function UnixToDateTime(iUnixTime: longint): TDateTime;
function DateTimeToUnix(dtDateTime: TDateTime): LongWord;

function WStrToFloatDef(const sZahl: string; dDefault: double): double; // 20.10.2001
function Round(const fZahl: Extended): Int64; overload; // 24.10.2001
function Round(const fZahl: Extended; iPrec: byte): Extended; overload; // 24.10.2001
function WRangeSet(fMinVal, fMaxVal: double; var fLow, fHigh: double): boolean;
function WIntToBin(iInt: integer; iDigits: byte): string;

function WOSShutDown(iState: word = EWX_SHUTDOWN + EWX_POWEROFF): boolean;
function GetMyUserName: string;
function WGetUserDir: TFileName;
function WGetComputerName: string; // 04.01.2004
function GDGetLocalIP: string;
function WGetWindowsVersion: OSVERSIONINFO;
function WGlobalMemoryStatus: TMEMORYSTATUS;
function WGlobalMemoryStatusEx(var MemStatus: TMemoryStatusEx): boolean;
function WGetLogicalDrives(pDriveTypes: TByteSet = []): string;  // 04.01.2005
function GetVersionInfo(sFileName: TFileName): VS_FIXEDFILEINFO;  // 22.07.2001
function WGetDiskSpace(cLw: char; iType: byte): Int64;
function GDGetShareProperties(const sShareName: string): TStrings;
function GDGetShareDirectory(const sShareName: string): string;
function GDIsLocalPath(
  const sPath: TFileName; var sLocalPath: TFileName): boolean;
function GDIsSamePath(const sPath1, sPath2: TFileName): boolean;
function GDExpandFilePath(const sFileName: TFileName;
  sExpandPath: string = ''): string;

function HiDWord(i : DWord): word;  // 22.07.2001
function LoDWord(i : DWord): word;  // 22.07.2001

function SearchFiles(sDirectory, sFileName: TFileName;
  pFiles: TStrings; var iAnzahl: integer; pCaptionControl: TControl): integer;
function StringToFile(s, DestFile: string; bOverwrite: boolean = True;
  bCRLF: boolean = true; bNet: boolean = false): boolean;
function StringFromFile(SourceFile: string; bNet: boolean = false): string;

procedure FreeTimer(pObject: TObject; iMs: integer);  // 03.04.2001
procedure ActionTimer(pEvent: TNotifyEvent; iMs: integer);  // 03.04.2001

procedure MergeMenu(pMenu: TMainMenu; bState: boolean); // 23.05.2001
function FilterCharsFromString(sText: string; pCSAllowed: TCharSet = [];
  pCSForbidden: TCharSet = []): string;  // 06.03.2001
function ReconvertLine(sLine: string): string;

procedure EnableMyControls(Sender: TWinControl; bState: boolean;
  bHourGlass: boolean = True; bFirstParent: boolean = True);
function ControlVisible(Sender: TWinControl): boolean;
procedure SetControlCheckedNoEvent(AControl: TControl; AChecked: Boolean);

function GDGetProcessList: TStrings;
function GDPostMessage(const sClassName: string; iMsg: Cardinal): integer;  // 01.04.2014
function GDKillProcess(PID: DWord): Bool;  // 03.09.2009
function GDKillExe(const sExe: string): integer;
function GDQuitExe(const sClassName: string): integer;
function GDStopService(
  const sExe, sServiceName: string; iTimeOut: integer): boolean;
function GDStopExe(
  const sExeFile, sClassName: string; iTimeOut: integer): integer;
function GDRunProcess(const sFileName: string;  // 03.09.2009
  bWait: Boolean; var iProcID: Cardinal): boolean; overload;
function GDRunProcess(const sFileName, sParams: string;   // 01.06.2011
  bWait: Boolean; var iProcID: Cardinal): boolean; overload;
function WWaitExecute(sEXE: string; iShowCmd: integer = -1): cardinal;
function GDGetExeCount(const sEXE: string): integer;
function GDRestartService(const sExeName, sServiceName: string;
  iTimeOut: integer): boolean;
function GDRestartExe(const sExeFile, sParams: string;
  iTimeOut: integer): boolean; overload;
function GDRestartExe(const sExeFile, sClassname, sParams: string;
  iTimeOut: integer): boolean; overload;

function GDCopyControl(
  pControl: TControl; pNewOwner: TControl; pNewParent: TWinControl): TControl;
function GDDeleteControl(pControl: TControl): boolean;
function GDFindControlWithThisTagProperty(pControl: TControl; iTag: integer;
  bIncludeSelf: boolean = True): TControl; overload;
function GDFindControlWithThisTagProperty(pControl: TControl; iTag: integer;
  const sClassname: string; bIncludeSelf: boolean = True): TControl; overload;

function WGetVersionInfo (Filename: string; var sVersion: string;
                          var sFileDate: string): boolean;

// Stream-Funktionen
procedure InsertDataToStream(pStream: TStream;
  const iPos, iLen: Int64; pInsert: TStream = nil);
procedure RemoveDataFromStream(
  pStream: TStream; iPos: Int64; const iLen: Int64; pRemoved: TStream = nil);

function GDGetServiceImagePath(const sServiceName: string): string;

implementation

function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL; stdcall; external kernel32;

type
  TTrickControl = class(TControl);

resourcestring
  S_ClipboardImage = 'Bild für die Zwischenablage';


{----------------------------- Lokale Funktionen ------------------------------}

{--------------------------------------------}
function WOpenSCManager(
  iAccess: DWord = SC_MANAGER_CONNECT+SC_MANAGER_ENUMERATE_SERVICE): SC_HANDLE;
{--------------------------------------------}
begin
  Result := OpenSCManager(nil, nil, iAccess);
end;

{--------------------------------------------}
function WOpenService(iSCHandle: SC_HANDLE; sServiceName: string;
  iAccess: DWord = SERVICE_START+SERVICE_STOP+SERVICE_QUERY_STATUS): SC_HANDLE;
{--------------------------------------------}
begin
  Result := OpenService(iSCHandle, PChar(sServiceName), iAccess);
end;

{--------------------------------------------}
function WCloseServiceHandle(iSCHandle: SC_HANDLE): boolean;
{--------------------------------------------}
begin
  Result := CloseServiceHandle(iSCHandle);
end;

{--------------------------------------------}
function WQueryServiceStatus(iSrvHdl: SC_HANDLE): TServiceStatus;
{--------------------------------------------}
begin
  FillChar(Result, SizeOf(TServiceStatus), 0);
  QueryServiceStatus(iSrvHdl, Result);
end;

{--------------------------------------------}
function WStartService(sServiceName: string): boolean;
{--------------------------------------------}
var
  iSCM, iSrvHdl  : SC_HANDLE;
  pArgv          : PChar;
  pServiceStatus : TServiceStatus;
begin
  Result := False;

  iSCM := WOpenSCManager;
  if (iSCM > 0) then
  try
    iSrvHdl := WOpenService(iSCM, sServiceName);
    if (iSrvHdl > 0) then
    try
      // Aktuellen Status abfragen
      pServiceStatus := WQueryServiceStatus(iSrvHdl);
      if (pServiceStatus.dwServiceType <> 0) then begin
        if ((pServiceStatus.dwCurrentState = SERVICE_RUNNING) or
            (pServiceStatus.dwCurrentState = SERVICE_START_PENDING))
        then Result := True
        else Result := (Winsvc.StartService(iSrvHdl, 0, pArgv));
      end;
    finally
      WCloseServiceHandle(iSrvHdl);
    end;
  finally
    WCloseServiceHandle(iSCM);
  end;
end;

{--------------------------------------------}
function WStopService(sServiceName: string; iTimeOut: Cardinal): boolean;
{--------------------------------------------}
var
  iSCM, iSrvHdl  : SC_HANDLE;
  pServiceStatus : TServiceStatus;
  iStop          : Cardinal;
begin
  Result := False;

  iSCM := WOpenSCManager;
  if (iSCM > 0) then
  try
    iSrvHdl := WOpenService(iSCM, sServiceName);
    if (iSrvHdl > 0) then
    try
      // Aktuellen Status abfragen
      pServiceStatus := WQueryServiceStatus(iSrvHdl);
      if (pServiceStatus.dwServiceType <> 0) then begin
        if ((pServiceStatus.dwCurrentState <> SERVICE_STOPPED) and
            (pServiceStatus.dwCurrentState <> SERVICE_STOP_PENDING)) then
        begin
          ControlService(iSrvHdl, SERVICE_CONTROL_STOP, pServiceStatus);
        end;

        // Warten, ob der Dienst innerhalb der Totzeit beendet wurde
        iStop := GetTickCount + iTimeOut;
        while ((GetTickCount < iStop) and
               (pServiceStatus.dwCurrentState <> SERVICE_STOPPED)) do
        begin
          Sleep(100);
          Application.ProcessMessages;
          pServiceStatus := WQueryServiceStatus(iSrvHdl);
          if (not (pServiceStatus.dwCurrentState in
            [SERVICE_STOPPED, SERVICE_STOP_PENDING]))
          then ControlService(iSrvHdl, SERVICE_CONTROL_STOP, pServiceStatus);
        end;

        Result := (pServiceStatus.dwCurrentState = SERVICE_STOPPED);
      end;
    finally
      WCloseServiceHandle(iSrvHdl);
    end;
  finally
    WCloseServiceHandle(iSCM);
  end;
end;

{----------------------------------- TMyStringObject -------------------------------}

{----------------------------------------------------}
constructor TMyStringObject.Create(const sText: string);
{----------------------------------------------------}
begin
  inherited Create;

  FText := sText;
end;

{----------------------------------- TMy2StringObject -------------------------------}

{----------------------------------------------------}
constructor TMy2StringObject.Create(const sText1, sText2: string);
{----------------------------------------------------}
begin
  inherited Create;

  FText1 := sText1;
  FText2 := sText2;
end;

{----------------------------------- TMyPointerList -------------------------------}

{----------------------------------------------------}
procedure TMyPointerList.Delete(iIndex: integer);
{----------------------------------------------------}
begin
  if (Assigned(Items[iIndex])) then Dispose(Items[iIndex]);

  inherited Delete(iIndex);
end;

{----------------------------------------------------}
procedure TMyPointerList.Clear;
{----------------------------------------------------}
var
  i : integer;
begin
  for i := 0 to Count-1 do
    if (Assigned(Items[i])) then Dispose(Items[i]);

  inherited;
end;

{--------------------------------- TMyObjectList ------------------------------}

{----------------------------------------------------}
procedure TMyObjectList.Delete(iIndex: integer);
{----------------------------------------------------}
begin
  if (Assigned(Items[iIndex])) then TObject(Items[iIndex]).Free;

  inherited Delete(iIndex);
end;

{----------------------------------------------------}
procedure TMyObjectList.Clear;
{----------------------------------------------------}
var
  i : integer;
begin
  for i := 0 to Count-1 do
    if (Assigned(Items[i])) then TObject(Items[i]).Free;

  inherited;
end;

{----------------------------------- TMyTabList -------------------------------}

{----------------------------------------------------}
destructor TMyTabList.Destroy;
{----------------------------------------------------}
begin
  Clear;

  inherited;
end;

{----------------------------------------------------}
procedure TMyTabList.Delete(iIndex: integer);
{----------------------------------------------------}
begin
  if (Assigned(Objects[iIndex])) then Objects[iIndex].Free;

  inherited;
end;

{----------------------------------------------------}
procedure TMyTabList.Clear;
{----------------------------------------------------}
var
  i : integer;
begin
  for i := 0 to Count-1 do if (Assigned(Objects[i])) then Objects[i].Free;

  inherited;
end;

{----------------------------------------------------}
function TMyTabList.GetCol(iIndex: integer): TStrings;
{----------------------------------------------------}
begin
  if (Assigned(Objects[iIndex]))
  then Result := TStrings(Objects[iIndex])
  else Result := nil;
end;

//--------------------------- TStringObjectList --------------------------------

{-----------------------------------}
destructor TStringObjectList.Destroy;
{-----------------------------------}
begin
  Clear;

  inherited Destroy;
end;

{--------------------------------}
procedure TStringObjectList.Clear;
{--------------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if (Assigned(Objects[i])) then
      Objects[i].Free;

  inherited Clear;
end;

{--------------------------------------------------}
procedure TStringObjectList.Delete(iIndex: integer);
{--------------------------------------------------}
begin
  if (Assigned(Objects[iIndex])) then
    Objects[iIndex].Free;

  inherited Delete(iIndex);
end;

//------------------------------ TMyThreadListCustom ---------------------------

//---------------------------------------------
constructor TMyThreadListCustom.Create;
//---------------------------------------------
begin
  inherited Create;

  FObjectClass := TObject;
  FModified := False;
end;

//---------------------------------------------
constructor TMyThreadListCustom.Create(pObjectClass: TClass);
//---------------------------------------------
begin
  inherited Create;

  FObjectClass := pObjectClass;
  FModified := False;
end;

//---------------------------------------------
destructor TMyThreadListCustom.Destroy;
//---------------------------------------------
begin
  Clear;

  inherited Destroy;
end;

//---------------------------------------------
procedure TMyThreadListCustom.Clear;
//---------------------------------------------
var
  i : integer;
begin
  with LockList.Create do
  try
    for i := Count-1 downto 0 do
    try
      TObject(Items[i]).Free;
    except
    // Fehler ignorieren
    end;
    Clear;
    FModified := True;
  finally
    UnlockList;
  end;
end;

//---------------------------------------------
function TMyThreadListCustom.GetCount: integer;
//---------------------------------------------
begin
  with LockList.Create do
  try
    Result := Count;
  finally
    UnlockList;
  end;
end;

//---------------------------------------------
function TMyThreadListCustom.AddObject(pObject: TObject): integer;
//---------------------------------------------
begin
  if (pObject is FObjectClass) then begin
    with LockList.Create do
    try
      Result := Add(pObject);
      FModified := True;
    finally
      UnlockList;
    end
  end
  else Result := -1;
end;

//---------------------------------------------
procedure TMyThreadListCustom.InsertObject(iIndex: integer; pObject: TObject);
//---------------------------------------------
begin
  if (pObject is FObjectClass) then begin
    with LockList.Create do
    try
      Insert(iIndex, pObject);
      FModified := True;
    finally
      UnlockList;
    end
  end;
end;

//---------------------------------------------
procedure TMyThreadListCustom.DeleteObject(iIndex: integer);
//---------------------------------------------
begin
  with LockList.Create do
  try
    try
      TObject(Items[iIndex]).Free;
    except
    // Fehler ignorieren
    end;
    Delete(iIndex);
    FModified := True;
  finally
    UnlockList;
  end;
end;

//---------------------------------------------
function TMyThreadListCustom.GetObject(iIndex: integer): TObject;
//---------------------------------------------
var
  p : TObject;
begin
  with LockList.Create do
  try
    p := TObject(Items[iIndex]);
    if (p is FObjectClass) then Result := p else Result := nil;
  finally
    UnlockList;
  end;
end;

//---------------------------------------------
procedure TMyThreadListCustom.SetObject(iIndex: integer; pObject: TObject);
//---------------------------------------------
begin
  DeleteObject(iIndex);
  InsertObject(iIndex, pObject);
  Modified := True;
end;

{---------------------------- Funktionen für TStream --------------------------}

// Fügt Daten in Stream ein
//---------------------------------------------
procedure InsertDataToStream(pStream: TStream;
  const iPos, iLen: Int64; pInsert: TStream = nil);
//---------------------------------------------
const
  CBufSize = $00010000;
var
  pBuf       : PChar;
  iCnt       : Integer;
  iSize, iIx : Int64;
begin
  // Puffer für Kopieraktion initialisieren
  GetMem(pBuf, CBufSize);
  try
    // Variablen vorbelegen
    iIx := 0;
    iSize := pStream.Size;

    // Platz im Stream schaffen
    repeat
      if ((iSize-iIx)-iPos < CBufSize)
      then iCnt := (iSize-iIx)-iPos
      else iCnt := CBufSize;
      pStream.Position := iSize -iIx - iCnt;
      pStream.ReadBuffer(pBuf^, iCnt);
      pStream.Position := iSize - iIx - iCnt + iLen;
      pStream.WriteBuffer(pBuf^, iCnt);
      Inc(iIx, iCnt);
    until ((iSize-iIx) = iPos);

    // Daten von Insert einfügen
    if (pInsert <> nil) then begin
      pStream.Position := iPos;
      pInsert.Position := 0;
      if (iLen <= pInsert.Size)
      then pStream.CopyFrom(pInsert, iLen)
      else pStream.CopyFrom(pInsert, 0);
    end;
  finally
    FreeMem(pBuf, CBufSize);
  end;
end;

// Löscht Daten aus Stream
//---------------------------------------------
procedure RemoveDataFromStream(
  pStream: TStream; iPos: Int64; const iLen: Int64; pRemoved: TStream = nil);
//---------------------------------------------
const
  CBufSize = $00010000;
var
  pBuf : PChar;
  iCnt : Integer;
begin
  GetMem(pBuf, CBufSize);
  try
    // Daten, die gelöscht werden, ggf. in Removed kopieren
    if (PRemoved <> nil) then begin
      pStream.Position := iPos;
      pRemoved.Position := 0;
      pRemoved.CopyFrom(pStream, iLen);
    end;
    repeat
      if (pStream.Size-iPos-iLen < CBufSize)
      then iCnt := pStream.Size-iPos-iLen
      else iCnt := CBufSize;
      pStream.Position := iPos+iLen;
      pStream.ReadBuffer(pBuf^, iCnt);
      pStream.Position := iPos;
      pStream.WriteBuffer(pBuf^, iCnt);
      Inc(iPos, iCnt);
    until (iPos = pStream.Size-iLen);
    pStream.Size := pStream.Size - iLen;
  finally
    FreeMem(pBuf, CBufSize);
  end;
end;

{----------------------------- Allgemeine Funktionen --------------------------}

// Setzen von System-Privilegien
// Parameter: Token
// Rückgabe: Erfolg ja/nein
//----------------------------------------
function SetToken(const sPrivileg: string): boolean;
//----------------------------------------
var
  pTokenPriv                  : TTokenPrivileges;
  iTokenHandle, iCurrrentProc : THandle;
  iReturn                     : DWord;
  pOSInfo                     : TOSVersionInfo;
begin
  Result := False;

  pOSInfo.dwOSVersionInfoSize := SizeOf(pOSInfo);
  GetVersionEx(pOSInfo);

  if (pOSInfo.dwPlatformId <> VER_PLATFORM_WIN32_WINDOWS) then begin  // Win9x
    iCurrrentProc := GetCurrentProcess;
    if (OpenProcessToken(iCurrrentProc, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
      iTokenHandle)) then
    begin
      if (LookupPrivilegeValue(nil, PChar(sPrivileg), pTokenPriv.Privileges[0].LUID)) then
      begin
        pTokenPriv.PrivilegeCount := 1;
        pTokenPriv.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

        iReturn := 0;
        Result := (AdjustTokenPrivileges(iTokenHandle, False, pTokenPriv, 0, nil, iReturn));
        CloseHandle(iTokenHandle);
      end;
    end;
  end;
end;

// Gibt Liste mit Processnamen und -IDs zurück
// Rückgabe: Liste (Namen in Großbuchstaben)
//----------------------------------------------------
function GDGetProcessList: TStrings;
//----------------------------------------------------
var
  i         : Integer;
  pidNeeded : DWORD;
  PIDList   : array[0..1000] of Integer; // Obergrenze !!!
  PIDName   : array [0..MAX_PATH - 1] of char;
  PH        : THandle;
begin
  Result := TStringList.Create;
  if (not Psapi.EnumProcesses(@PIDList, 1000, pidNeeded)) then
    raise Exception.Create('!!! PSAPI.DLL not found !!!');
  for i := 0 to (pidNeeded div sizeof (Integer)- 1) do begin
    PH := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
      False, PIDList[i]);
    if (PH <> 0) then begin
      if (Psapi.GetModuleBaseName(PH, 0, PIDName, Sizeof (PIDName)) > 0) then
      begin
        Result.AddObject(UpperCase(PIDName), TObject(PIDList[i]));
        CloseHandle(PH);
      end;
    end;
  end;
end;

// Anzahl der laufenden Instanzen eines EXEs
{--------------------------------------------}
function GDExeCount(const sEXE: string): integer;
{--------------------------------------------}
var
  s : string;
  i : integer;
begin
  Result := 0;
  s := ExtractFileName(sEXE);
  s := UpperCase(ChangeFileExt(s, '.EXE'));
  with GDGetProcessList do
  try
    for i := 0 to Count-1 do
      if (Strings[i] = s) then Inc(Result)
  finally
    Free;
  end;
end;

// Hartes Beenden eines Prozesses
// Parameter: Prozess-ID
// Rückgabe: Erfolg ja/nein
//----------------------------------------
function GDKillProcess(PID: DWord): Bool;
//----------------------------------------
var
  hProcess: THandle;
begin
  SetToken('SEDEBUGPRIVILEGE');  // Privileg für das Beenden von System-Diensten
  hProcess := OpenProcess(PROCESS_TERMINATE, False, PID);
  Result := TerminateProcess(hProcess, 0);
end;

// Hartes Beenden von EXEs
{--------------------------------------------}
function GDKillExe(const sExe: string): integer;
{--------------------------------------------}
var
  s : string;
  i : integer;
begin
  Result := 0;
  s := ExtractFileName(sExe);
  s := UpperCase(ChangeFileExt(s, '.EXE'));
  with GDGetProcessList do
  try
    for i := 0 to Count-1 do
      if (Strings[i] = s) then begin
        if (GDKillProcess(Integer(Objects[i]))) and (Result >= 0)
        then Inc(Result)
        else Result := -1;
      end;
  finally
    Free;
  end;
end;

// Sendet Message an EXEs
// Parameter: Klassenname des Hauptfensters
{--------------------------------------------}
function GDPostMessage(const sClassName: string; iMsg: Cardinal): integer;
{--------------------------------------------}
var
  iHWND : HWND;
  pBuf  : array [Byte] of Char;
begin
  Result := 0;

  iHWND := FindWindow(PChar(sClassName), nil);
  while (iHWND <> 0) do begin
    GetClassName(iHWND, pBuf, SizeOf(pBuf));
    if (StrComp(pBuf, PChar(sClassName)) = 0) then begin
      PostMessage(iHWND, iMsg, 0, 0);
      Inc(Result);
    end;
    iHWND := GetNextWindow(iHWND, GW_HWNDNEXT);
  end;
end;

// Weiches Beenden von EXEs
// Parameter: Klassenname des Hauptfensters
{--------------------------------------------}
function GDQuitExe(const sClassName: string): integer;
{--------------------------------------------}
var
  iHWND : HWND;
  pBuf  : array [Byte] of Char;
begin
  Result := 0;

  iHWND := FindWindow(PChar(sClassName), nil);
  while (iHWND <> 0) do begin
    GetClassName(iHWND, pBuf, SizeOf(pBuf));
    if (StrComp(pBuf, PChar(sClassName)) = 0) then begin
      PostMessage(iHWND, WM_Quit, 0, 0);
      PostMessage(iHWND, WM_ENDSESSION, 0, 0);
      Inc(Result);
    end;
    iHWND := GetNextWindow(iHWND, GW_HWNDNEXT);
  end;
end;

// Stoppt Dienst - gütlich oder gewaltsam ...
// Rückgabe: Name des EXE, Dienstename, Timeout
// Rückgabe: Erfolg ja / nein
//----------------------------------------------------
function GDStopService(
  const sExe, sServiceName: string; iTimeOut: integer): boolean;
//----------------------------------------------------
begin
  Result :=
    (WStopService(sServiceName, iTimeOut)) or (GDKillExe(sExe) >= 0);
end;

// Stoppt Exe - gütlich oder gewaltsam ...
// Rückgabe: Name des EXE, Klassenname, Timeout
// Rückgabe: Anzahl der immer noch laufenden Anwendungen oder -1
//----------------------------------------------------
function GDStopExe(
  const sExeFile, sClassName: string; iTimeOut: integer): integer;
//----------------------------------------------------
var
  iCnt : integer;
  iTO  : Cardinal;
begin
  try
    if (sClassname <> '') then iCnt := GDQuitExe(sClassname) else iCnt := 0;
    if (iCnt >= 0) then begin
      if (iCnt > 0) then Sleep(iCnt * 1000);

      if (iTimeOut > 0)
      then iTO := GetTickCount + Cardinal(iTimeOut)
      else iTO := GetTickCount + 1000;
      iCnt := GDKillExe(sExeFile);
      while (iCnt <> 0) and (GetTickCount <= iTO) do begin
        Sleep(1000);
        iCnt := GDKillExe(sExeFile);
      end;
    end;

    Result := iCnt;
  except
    Result := -1;
  end;
end;

// Prozess starten
// Parameter: Dateiname, Warten auf Ende?, Prozess-ID
// Rückgabe: Erfolg ja/nein
//----------------------------------------
function GDRunProcess(
  const sFileName: string; bWait: Boolean; var iProcID: Cardinal): boolean;
//----------------------------------------
var
  pTSI : TStartupInfo;
  pTPI : TProcessInformation;
begin
  Result := False;
  iProcID := 0;

  FillChar(pTSI, SizeOf(TStartupInfo), #0);
  FillChar(pTPI, SizeOf(TProcessInformation), #0);
  pTSI.cb := SizeOf(StartupInfo);

  if (CreateProcess(nil, PChar(sFilename), nil, nil, False, 0, nil,
    nil, pTSI, pTPI))
  then try
    iProcID := pTPI.dwProcessId;
    if (bWait) then WaitForSingleObject(pTPI.hProcess, INFINITE);
    Result := True;
  finally
    if (pTPI.hProcess <> 0) then CloseHandle(pTPI.hProcess);
    if (pTPI.hThread <> 0) then CloseHandle(pTPI.hThread);
  end;
end;

// Prozess starten
// Parameter: Dateiname, Warten auf Ende?, Prozess-ID
// Rückgabe: Erfolg ja/nein
//----------------------------------------
function GDRunProcess(const sFileName, sParams: string;   // 01.06.2011
  bWait: Boolean; var iProcID: Cardinal): boolean;
//----------------------------------------
var
  pTSI : TStartupInfo;
  pTPI : TProcessInformation;
begin
  Result := False;
  iProcID := 0;

  FillChar(pTSI, SizeOf(TStartupInfo), #0);
  FillChar(pTPI, SizeOf(TProcessInformation), #0);
  pTSI.cb := SizeOf(StartupInfo);

  if (CreateProcess(nil,
    PChar(Format('"%s" %s', [sFilename, TrimRight(sParams)])), nil, nil, False,
    0, nil, PChar(ExcludeTrailingBackslash(ExtractFilePath(sFileName))),
    pTSI, pTPI))
  then try
    iProcID := pTPI.dwProcessId;
    if (bWait) then WaitForSingleObject(pTPI.hProcess, INFINITE);
    Result := True;
  finally
    if (pTPI.hProcess <> 0) then CloseHandle(pTPI.hProcess);
    if (pTPI.hThread <> 0) then CloseHandle(pTPI.hThread);
  end;
end;

// Prozess starten und auf Ende warten
// Parameter: Dateiname
//            Anzeige (SW_...-Konstante, z.B. SW_MINIMIZE)
// Rückgabe: Exitcode des Prozesses
{---------------------------------------}
function WWaitExecute(sEXE: string; iShowCmd: integer = -1): cardinal;
{---------------------------------------}
var
  aTSI : TStartupInfo;
  aTPI : TProcessInformation;
  iRet : Integer;

begin
  FillChar(aTSI, SizeOf(aTSI), #0);
  FillChar(aTPI, SizeOf(aTPI), #0);
  aTSI.CB := SizeOf(aTSI);
  if iShowCmd >= 0 then begin  // 28.06.2023, WW
    aTSI.dwFlags := STARTF_USESHOWWINDOW;          
    aTSI.wShowWindow := iShowCmd;
  end;

  if (not CreateProcess(nil, PChar(sEXE), nil, nil, False,
    0, nil, nil, aTSI, aTPI)) //HIGH_PRIORITY_CLASS
  then RaiseLastWin32Error;

  // Default-Exitcode: STATUS_PENDING (259)
  GetExitCodeProcess(aTPI.hProcess, Result);  // 26.04.2018, WW

  repeat
    iRet := MsgWaitForMultipleObjects(
      1, aTPI.hProcess, False, INFINITE, (QS_ALLINPUT));
    if iRet <> (WAIT_OBJECT_0) then begin
      Sleep(500);  // verkürzt von 1 auf 0,5 s; 26.04.2018, WW
      Application.ProcessMessages;
    end else
      GetExitCodeProcess(aTPI.hProcess, Result);  // Exitcode nach Prozessende; 26.04.2018, WW
  until iRet = (WAIT_OBJECT_0);

  CloseHandle(aTPI.hProcess);
end;

// Gibt Anzahl der laufenden Programme zurück
{--------------------------------------------}
function GDGetExeCount(const sEXE: string): integer;
{--------------------------------------------}
var
  s : string;
  i : integer;
begin
  Result := 0;
  s := ExtractFileName(sEXE);
  s := UpperCase(ChangeFileExt(s, '.EXE'));
  with GDGetProcessList do
  try
    for i := 0 to Count-1 do
      if (Strings[i] = s) then Inc(Result);
  finally
    Free;
  end;
end;

// Dienst ggf. anhalten und neu starten
// Parameter: Dateiname, Dienstename, Timeout für das Beenden
// Rückgabe: Erfolg ja/nein
//----------------------------------------
function GDRestartService(
  const sExeName, sServiceName: string; iTimeOut: integer): boolean;
//----------------------------------------
begin
  try
    if (WStopService(sServiceName, iTimeOut)) or (GDKillExe(sExeName) >= 0) then
    begin
      Sleep(1000);
      Result := WStartService(sServiceName);
    end
    else Result := False;
  except
    Result := False;
  end;
end;

// Programm ggf. anhalten und neu starten
// Parameter: Dateiname, Timeout für das Beenden
// Rückgabe: Erfolg ja/nein
//----------------------------------------
function GDRestartExe(
  const sExeFile, sParams: string; iTimeOut: integer): boolean;
//----------------------------------------
var
  iCnt : integer;
  iPId : Cardinal;
  iTO  : Cardinal;
begin
  try
    iCnt := GDKillExe(sExeFile);
    if (iTimeOut > 0)
    then iTO := GetTickCount + Cardinal(iTimeOut)
    else iTO := GetTickCount + 10000;  // 04.04.2013
    while (iCnt <> 0) and (GetTickCount <= iTO) do begin
      Sleep(1000);
      iCnt := GDKillExe(sExeFile);
    end;
    if (iCnt = 0) then begin
      Result := GDRunProcess(sExeFile, sParams, False, iPId);
    end
    else Result := False;
  except
    Result := False;
  end;
end;

// Programm ggf. anhalten und neu starten
// Parameter: Dateiname, Klassenname des Hauptfensters, Timeout für das Beenden
// Rückgabe: Erfolg ja/nein
//----------------------------------------
function GDRestartExe(
  const sExeFile, sClassname, sParams: string; iTimeOut: integer): boolean;
//----------------------------------------
var
  iCnt : integer;
begin
  try
    if (sClassname <> '') then iCnt := GDQuitExe(sClassname) else iCnt := 0;
    if (iCnt >= 0) then begin
      if (iCnt > 0) then Sleep(iCnt * 1000);
      Result := GDRestartExe(sExeFile, sParams, iTimeOut - (iCnt*100));
    end
    else Result := False;
  except
    Result := False;
  end;
end;

{--------------------------------------------}
function WOSShutDown(iState: word = EWX_SHUTDOWN + EWX_POWEROFF): boolean;
{--------------------------------------------}

  procedure CloseDesktop;
  var
    iExpId : Longword;
    iHExplorer, iHDesktop : HWND;
  begin
    iHDesktop := GetDesktopWindow;
    GetWindowThreadProcessId(iHDesktop, iExpId);
    iHExplorer := OpenProcess(PROCESS_ALL_ACCESS, False, iExpId);
    try
      TerminateProcess(iHExplorer, 1);
    finally
      CloseHandle(iHExplorer);
    end;
  end;

var
  pOSInfo : TOSVersionInfo;
begin
  Result := False;

  pOSInfo.dwOSVersionInfoSize := SizeOf(pOSInfo);
  GetVersionEx(pOSInfo);

  if (pOSInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS) then begin  // Win9x
    CloseDesktop;
    Result := ExitWindowsEx(iState, 0);
  end
  else begin
    if (SetToken('SeShutdownPrivilege')) then
      Result := ExitWindowsEx(iState, 0);
  end;
end;

{----------------------------------------------------}
function WGetComputerName: string;
{----------------------------------------------------}
var
  p : PChar;
  i : DWord;
begin
  GetMem(p, 100);
  try
    p[0] := #0;
    i := 99;
    GetComputerName(p, i);
    Result := StrPas(p);
  finally
    FreeMem(p, 100);
  end;
end;

{----------------------------------------------------}
function WGetUserDir: TFileName;
{----------------------------------------------------}
var
  PpIdl : PItemIdList;
  PPath : array of char;
begin
  try
    SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PpIdl);
    SetLength(PPath, MAX_PATH);
    SHGetPathFromIDList(PpIdl, PChar(PPath));
    Result := IncludeTrailingBackslash(Trim(string(PPath)));
    if (Result <> '') and DirectoryExists(Result)
    then Result := IncludeTrailingBackslash(Result)
    else Result := '';
  except
    Result := '';
  end;
end;

{----------------------------------------------------}
function WGetWindowsVersion: OSVERSIONINFO;
{----------------------------------------------------}
begin
  try
    Result.dwOSVersionInfoSize := SizeOf(OSVERSIONINFO);
    if (not GetVersionEx(Result)) then
      FillChar(Result, SizeOf(OSVERSIONINFO), 0);
  except
    FillChar(Result, SizeOf(OSVERSIONINFO), 0);
  end;
end;

{----------------------------------------------------}
function WGlobalMemoryStatus: TMEMORYSTATUS;
{----------------------------------------------------}
// Anmerkung: Kann Werte mit max. 4096 MB liefern (bedingt durch TMEMORYSTATUS-Struktur)
begin
  GlobalMemoryStatus(Result);
end;

{----------------------------------------------------}
function WGlobalMemoryStatusEx(var MemStatus: TMemoryStatusEx): boolean;
{----------------------------------------------------}
// Anmerkung: Kann Werte > 4096 MB liefern
begin
  try
    // initialize the structure
    FillChar(MemStatus, SizeOf(MemStatus), 0);
    MemStatus.dwLength := SizeOf(MemStatus);
    Result:=GlobalMemoryStatusEx(MemStatus);
  except
    Result:=false;
  end;
end;

{ Gibt den Platz auf einem Laufwerk zurück           }
{ Parameter: Laufwerksbuchstabe, Typ (1=tot,, 2=frei)}
{ Rückgabe: Platz oder 0/-1                          }
{----------------------------------------------------}
function WGetDiskSpace(cLw: char; iType: byte): Int64;
{----------------------------------------------------}
begin
  try
    if (iType = 1) then
      Result := DiskSize(Ord(UpperCase(cLw)[1]) - Ord('A') + 1)
    else if (iType = 2) then
      Result := DiskFree(Ord(UpperCase(cLw)[1]) - Ord('A') + 1)
    else Result := 0;
  except
    Result := -1;
  end;
end;

{ Gibt die Eigenschaften einer lok. Freigabe zurück  }
{ Parameter: Freigabename                            }
{ Rückgabe: Stringliste mit Eigenschaften oder nil   }
{----------------------------------------------------}
function GDGetShareProperties(const sShareName: string): TStrings;
{----------------------------------------------------}
var
  p        : array [1..1000] of char;
  s        : string;
  i, iSize : integer;
begin
  Result := nil;
  try
    with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if (OpenKey('\SYSTEM\CurrentControlSet\Services\lanmanserver\Shares', False))
      then begin
        iSize := GetDataSize(sShareName);
        ReadBinaryData(sShareName, p, iSize);
        Result := TStringList.Create;
        s := '';
        for i := 1 to iSize do
          if (p[i] = #0) then s := s + #13#10 else s := s + p[i];
        Result.Text := s;
      end;
    finally
      Free;
    end;
  except
    FreeAndNil(Result);
  end;
end;

{ Gibt die Pfadangabe zu einer lok. Freigabe zurück  }
{ Parameter: Freigabename                            }
{ Rückgabe: Pfad oder ''                             }
{ ALTERNATIVES VORGEHEN: über NET SHARE und Dateiuml.}
{----------------------------------------------------}
function GDGetShareDirectory(const sShareName: string): string;
{----------------------------------------------------}
var
  pSl : TStrings;
begin
  Result := '';  // Default

  pSl := GDGetShareProperties(sShareName);
  if (Assigned(pSl)) then
  try
    Result := pSl.Values['Path'];
  finally
    pSl.Free;
  end;
end;

{ Gibt an, ob es sich um einen loakelen Pfad handelt }
{ Parameter: Pfadname, lokale Bezeichnung            }
{ Rückgabe: ja/nein                                  }
{ ALTERNATIVES VORGEHEN: über NET SHARE und Dateiuml.}
{----------------------------------------------------}
function GDIsLocalPath(
  const sPath: TFileName; var sLocalPath: TFileName): boolean;
{----------------------------------------------------}
var
  s, sPcName : string;
  sShare     : TFileName;
begin
  sLocalPath := IncludeTrailingBackslash(ExpandUNCFileName(sPath));
  if (Pos('\\', sLocalPath) = 1) then begin  // UNC-Notation
    s := ExtractFileDrive(sLocalPath);
    System.Delete(s, 1, 2);
    sPcName := GetStringPart(s, 1, '\');
    if (UpperCase(WGetComputerName) = UpperCase(sPcName)) then begin
      s := GetStringPart(s, 2, '\');
      sShare := GDGetShareDirectory(s);
      if (sShare <> '') then begin
        sLocalPath := StringReplace(sLocalPath,
          IncludeTrailingBackslash(ExtractFileDrive(sLocalPath)),
          IncludeTrailingBackslash(sShare), [rfIgnoreCase]);
        Result := True;
      end
      else Result := False;
    end
    else Result := False;
  end
  else Result := (DirectoryExists(sLocalPath));
end;

{ Gibt an, ob es sich um den gleichen Pfad handelt   }
{ Parameter: Pfadname 1, Pfadname 2                  }
{ Rückgabe: ja/nein                                  }
{----------------------------------------------------}
function GDIsSamePath(const sPath1, sPath2: TFileName): boolean;
{----------------------------------------------------}
var
  sP1, sP2 : TFileName;
begin
  GDIsLocalPath(sPath1, sP1);
  GDIsLocalPath(sPath2, sP2);
  Result := (UpperCase(sP1) = UpperCase(sP2));
end;

// Vervollständigt ggf. relativen Pfad mit Applikationspfad (ohne Prüfung)
// Parameter: Pfad-/Dateiname
//            optional: Pfad zum Vervollständigen des relativen Pfades (Leer =
//                      Applikationspfad); 12.06.1014, WW
// Rückgabe: vervollständigter Pfad
//----------------------------------------------------
function GDExpandFilePath(const sFileName: TFileName;
  sExpandPath: string = ''): string;
//----------------------------------------------------
var
  S: string;
begin
  Result := Trim(sFileName);
  if (Pos('.', Result) = 1) then begin
    if length (sExpandPath) = 0 then
      S:=ExtractFilePath(ParamStr(0))
    else
      S:=IncludeTrailingBackslash (sExpandPath);  // 12.06.2014, WW
    Result := S + Result;
  end;
end;

{ Trägt die Pfade aller sFileName-Files im übergebenen  }
{ und untergeordneten Verzeichnissen in Stringliste ein }
{ Parameter: Startverzeichnis, Maske, Übergabeliste,    }
{    direkt untergeordnete Vereichnisse, Anzeigecontrol }
{ Rückgabe: Anzahl der insgesamt gefundenen Dateien     }
{-------------------------------------------------------}
function SearchFiles(sDirectory, sFileName: TFileName;
  pFiles: TStrings; var iAnzahl: integer; pCaptionControl: TControl): integer;
{-------------------------------------------------------}
var
  pSR  : TSearchRec;
  i    : integer;
  sDir : string;
begin
  try
    // Directory ggf. erweitern
    sDir := sDirectory;
    if (Pos('.', sDir) = 1) then
      sDir := ExtractFilePath(ParamStr(0)) + sDir;

    if (DirectoryExists(sDir)) then begin
      if (Assigned(pCaptionControl)) then
        TTrickControl(pCaptionControl).Caption := sDir;
      sDir:= IncludeTrailingBackslash(sDir);
      if (Assigned(pCaptionControl)) then
        TTrickControl(pCaptionControl).Caption := sDir;
      Result := 0;
      // Suchen nach übergebener Datei
      if (FindFirst(sDir + sFileName, 0, pSR) = 0) then
      try
        pFiles.Add(sDir + pSR.Name);
        Inc(Result);
        while (FindNext(pSR) = 0) do begin
          pFiles.Add(sDir + pSR.Name);
          Inc(Result);
          Application.ProcessMessages;
        end;
      finally
        FindClose(pSR);
      end;
      // Rekursive Suche in untergeordneten Verzeichnissen
      if (FindFirst(sDir + '*.', faDirectory, pSR) = 0) then
      try
        if ((pSR.Attr and faDirectory) > 0) and (Pos('.', pSR.Name) = 0) then
        begin
          i := SearchFiles(
            sDir + pSR.Name, sFileName, pFiles, iAnzahl, pCaptionControl);
          if (i > 0) then Inc(Result, i);
        end;
        while (FindNext(pSR) = 0) do begin
          if ((pSR.Attr and faDirectory) > 0) and (Pos('.', pSR.Name) = 0) then
          begin
            i := SearchFiles(
              sDir + pSR.Name, sFileName, pFiles, iAnzahl, pCaptionControl);
            if (i > 0) then Inc(Result, i);
          end;
          Application.ProcessMessages;
        end;
      finally
        FindClose(pSR);
      end;

      iAnzahl := iAnzahl + Result;
    end
    else Result := -1;
  except
    Result := -1;
  end;
end;

{ CB-Fktn. für String-Sortierung in Stringliste      }
{ Parameter: Liste, die sortiert werden soll;        }
{            Indizes der zu vergleichenden Strings   }
{----------------------------------------------------}
function StringListStrCompare(
  List: TStringList; Index1, Index2: Integer): Integer;
{----------------------------------------------------}
begin
  Result := CompareStr(List[Index1], List[Index2]);
end;

{------------------------------------------------------}
function StringListObjectSort(
  List: TStringList; Index1, Index2: Integer): integer;
{------------------------------------------------------}
begin
  Result := Integer(List.Objects[Index1]) - Integer(List.Objects[Index2]);
end;

{ Gibt das letzte Steuerzeichen im String zurück         }
{ Parameter: Source-String                               }
{ Rückgabe: Char oder #0                                 }
{--------------------------------------------------------}
function GetLastSteuerzeichen(Value: string): char;
{--------------------------------------------------------}
var
  i : integer;
begin
  Result := #0;
  for i := Length(Value) downto 1 do
    if (Ord(Value[i]) < 32) then begin
      Result := Value[i];
      Break;
    end;
end;

(*
{ Gibt aus einem string das Element zwischen dem         }
{ 'iIndex-ten' und 'iIndex-ten+1' Steuerzeichen zurück   }
{ Parameter: string, Steuerzeichen-Index (ab 1)          }
{ Rückgabe: Element oder ''                         }
{--------------------------------------------------------}
function GetElementNachSteuerzeichenIndex(
  sString: string; iIndex: integer): string;
{--------------------------------------------------------}
var
  i, j : integer;
begin
  Result := '';

  // iIndex-tes Steuerzeichen ermitteln
  i := 1;
  j := 0;
  while (i < Length(sString)) and (j < iIndex) do begin
    if (sString[i] < ' ') then Inc(j);
    Inc(i);
  end;
  if (i < Length(sString)) then Result := Copy(sString, i, Length(sString)-1);

  // iIndex-tes+1 Steuerzeichen ermitteln
  i := 1;
  while (i < Length(Result)) and (Result[i] >= ' ') do Inc(i);
  if (i < Length(Result)) then Result := Copy(Result, 1, i-1);
end;
*)

{ Schreibt einen Text in eine Fehlerdatei                }
{ Parameter: Fehler-String                               }
{            Logfilename                                 }
{            Logtype                                     }
{            LogPfad (optional)                          }
{--------------------------------------------------------}
procedure WriteErrorLog(sFehler: string; sFileName: TFileName = '';
                        aLogType: TErrorLogType = elt_Error;
                        sLogPath: TFileName = '');  // 26.06.2014  WN
{--------------------------------------------------------}
const
  C_LogFilePath : String = '';  // durch Angabe Logpfad gesetzt
var
  TFS   : TTextFileStreamExt;
  sFile : string;
  bOK   : boolean;
  sLogType : String;  // 23.05.2014  WN
  sLog : String;      // 23.05.2014  WN
begin
  // 26.06.2014  WN
  // wenn LogPfad angegeben, dann diesen verwenden
  if (sLogPath <> '') then C_LogFilePath := IncludeTrailingBackslash(sLogPath);

  if length (sFehler) > 0 then begin  // 12.08.2015, WW
    // wenn Filename leer -> in FEHLER.txt schreiben
    if (sFileName = '') then
    begin
      if (C_LogFilePath <> '') then sFile := C_LogFilePath
      else sFile := ExtractFilePath(ParamStr(0));
      sFile := sFile + 'FEHLER.TXT';
    end else
    begin
      // wenn LogPfad in Konstante angegeben, dann diesen Pfad verwenden
      if (C_LogFilePath <> '') then sFile := C_LogFilePath + ExtractFileName(sFileName)
      else sFile := sFileName;
    end;

    // 23.05.2014  WN
    sLogType := 'ERROR';
    case aLogType of
      elt_Debug: sLogType := 'DEBUG';
      elt_Info: sLogType := 'INFO';
      elt_Warning: sLogType := 'WARNING';
      elt_Error: sLogType := 'ERROR';
    end;
    sLogType := '[' + sLogType + ']';

    try
      if (not FileExists(sFile))
      then TFS := TTextFileStreamExt.Create(sFile, fmCreate, bOK)
      else TFS := TTextFileStreamExt.Create(
        sFile, fmOpenReadWrite OR fmShareDenyWrite, bOK);
      if (bOK) then
      try
        TFS.Seek(0, soFromEnd);
        sLog := FormatDateTime ('yyyy-mm-dd hh:nn:ss,zzz', Now) + #9 +  // Datum-Format angepaßt; 23.05.2014  WN
                ChangeFileExt(ExtractFileName(ParamStr(0)), '') + #9 +
                sLogType + #9 + sFehler;  // mit Log-Type; 23.05.2014  WN
        TFS.WriteLn(sLog);
      finally
        TFS.Free;
      end;
    except
    end;
  end;  // if length (sFehler) > 0
end;

{ Gibt den ersten Teilstring zurück, der von den über-   }
{ gebenen Charactern eingegrenzt wird                    }
{ Parameter: Source-String, Anfangs-, Endzeichen         }
{ Rückgabe: Teilstring oder ''                           }
{--------------------------------------------------------}
function SelectElement(vZeile: string; cStart, cStop: char):string;
{--------------------------------------------------------}
var
  s : string;
  i : integer;
begin
  Result := ''; // default
  i := Pos(cStart, vZeile);
  if (i > 0) then begin
    s := System.Copy(vZeile, i+1, Length(vZeile)-i-1);
    i := Pos(cStop, s);
    if (i > 0) then Result := System.Copy(s, 1, i-1);
  end
end;

{ Gibt den ersten Teilstring zurück, der von Steuer-     }
{ zeichen eingegrenzt wird                               }
{ Parameter: Source-String                               }
{ Rückgabe: Teilstring oder ''                           }
{--------------------------------------------------------}
function SelectDatenElement(vZeile: string):string;
{--------------------------------------------------------}
var
  i, j : integer;
begin
  Result := ''; // default
  i := 1;
  j := 0;
  // Anfang herausfinden
  while (i <= Length(vZeile)) do if (vZeile[i] < ' ') then Break else Inc(i);
  Inc(i);
  while (i <= Length(vZeile)) do if (vZeile[i] < ' ') then Break
    else begin
      Inc(i);
      Inc(j);
    end;
  if (i < Length(vZeile)) then Result := Copy(vZeile, i-j, j);
end;

{ Wandelt die Steuerzeichen eines Strings  }
{ mit 'CharToString' in lesbare Zeichen um }
{ Parameter: string mit Steuerzeichen      }
{ Rückgabe: string mit Klartext            }
{------------------------------------------}
function CharstrToString(sCharstring: string): string;
{------------------------------------------}
var
  i : integer;
begin
  Result := '';
  for i := 1 to Length(sCharstring) do
    Result := Result + CharToString(sCharstring[i]);
end;

{------------------------------------------}
function charToString(thisChar: char): string;
{------------------------------------------}
{ In 'thisChar' wird ein Char übergeben. Wenn es sich um ein Steuer-
  zeichen handelt, wird es in einen lesbaren string umgewandelt }
begin
  if (thisChar >= ' ') then begin
    if (Ord(thisChar) in [32..127, Ord('ä'), Ord('Ä'), Ord('ö'), Ord('Ö'),
      Ord('ü'), Ord('Ü'), Ord('ß')])
    then Result := thisChar
    else Result := '<' + IntToStr(Ord(thisChar)) + '>';
  end
  else case ord(thisChar) of
    nul: result:= '<nul>' ;
    soh: result:= '<soh>' ;
    stx: result:= '<stx>' ;
    etx: result:=  '<etx>' ;
    eot: result:= '<eot>' ;
    enq: result:= '<enq>' ;
    ack: result:= '<ack>' ;
    bel: result:= '<bel>' ;
    bs: result:= '<bs>' ;
    ht: result:= '<ht>' ;
    lf: result:= '<lf>' ;
    vt: result:=  '<vt>' ;
    ff: result:= '<ff>' ;
    cr: result:=  '<cr>' ;
    so: result:= '<so>' ;
    si: result:= '<si>' ;
    dle: result:= '<dle>' ;
    dc1: result:= '<dc1>' ;
    dc2: result:= '<dc2>' ;
    dc3: result:= '<dc3>' ;
    dc4: result:= '<dc4>' ;
    nak: result:= '<nak>' ;
    syn: result:= '<syn>' ;
    etb: result:= '<etb>' ;
    can: result:= '<can>' ;
    em: result:= '<em>' ;
    sub: result:= '<sub>' ;
    esc: result:=  '<esc>' ;
    fs: result:=  '<fs>' ;
    gs: result:=  '<gs>' ;
    rs: result:=  '<rs>' ;
    us: result:=  '<us>' ;
    else Result := '<' + IntToStr(Ord(thisChar)) + '>';
  end;
end;

{------------------------------------------}
function ReadToString(thisPChar: Pchar; maxL: integer): string;
{------------------------------------------}
{ In 'thisPChar' wird ein PChar übergeben. Er wird bis 'maxL' in einen String
  umgewandelt, wobei Steuerzeichen lesbar wiedergegeben werden }
var
  i         : cardinal;
  TextZeile : string;
begin
  result:= '';   { default }
  TextZeile:= '';
  i:= 0;
  if (thisPChar[0] <> #0) then begin
    while (i < strLen(thisPChar)) and (length(Textzeile) < maxL) do begin
      textZeile:= textZeile + charToString(thisPChar[i]);
      inc(i);
    end;
  end;
  result:= textZeile;
end;

{------------------------------------------}
procedure convertPCharToStringList(var Dest: TStringList; thisSource: PChar);
{------------------------------------------}
{ In 'thisSource' wird ein PChar übergeben. Rückgabe ist die StringListe 'Dest',
  die als Elemente den Inhalt von 'thisSource' als Strings hat }
var
  Anzahl:    cardinal;
  i, j:      cardinal;
  TextZeile: string;
begin
  { thisSource muß < 0 sein }
  if (thisSource[0] <> #0) then begin
  { thisSource muß darf maximal die Blocklänge der CANCard haben }
    if strLen(thisSource) < 1025 then begin
  { Es wird die Anzahl der Strings mit 255 Zeichen ermittelt }
      Anzahl:= strLen(thisSource) div 255;
      Anzahl:= Anzahl-1;
      for i:= 0 to Anzahl do begin
  { Diese werden mit Charaktern aus ThisSource aufgefüllt }
        TextZeile:= '';
        for j:= 0 to 254 do begin
          TextZeile:= TextZeile + thisSource[i*255+j];
        end;
        Dest.add(TextZeile);
      end;
  { Die restlichen Character aus ThisSource werden in den letzten String geschrieben }
      i:= Anzahl + 1;
      Anzahl:= strLen(thisSource) - (Anzahl+1)*255 -1;
      TextZeile:= '';
      for j:= 0 to Anzahl do begin
        TextZeile:= TextZeile + thisSource[i*255+j];
      end;
      Dest.add(TextZeile);
    end;
  end;
end;

{------------------------------------------}
procedure addOffsetToPChar(thisPChar: PChar; thisOfs: integer);
{------------------------------------------}
{ In 'thisPChar' wird ein PChar übergeben. Rückgabe ist dieser PChar,
  wobei die 'ORD'-Werte mit einem Offset von 'thisOfs' versehen werden }
var
  i, j:   integer;
begin
  if thisPChar[0] <> #0 then
    for i:= 0 to strLen(thisPChar)-1 do begin
      j:= ord(thisPChar[i]) + thisOfs;
      thisPChar[i]:= chr(j);
    end;
end;

{------------------------------------------}
function addOffsetToString(thisString: string; thisOfs: integer): string;
{------------------------------------------}
{ In 'thisString' wird ein string übergeben. Rückgabe ist dieser string,
  wobei die 'ORD'-Werte mit einem Offset von 'thisOfs' versehen werden }
var
  i, j:   integer;
begin
  result:= '';
  for i:= 1 to Length(thisString) do begin
    j:= ord(thisString[i]) + thisOfs;
    result:= result + chr(j);
  end;
end;

{--------------------------------------------}
function StringFromFile(SourceFile: string; bNet: boolean = false): string;
{--------------------------------------------}
{ Der Inhalt eines File 'SourceFile' wird in einen Ausgabestring geschrieben;
  Übergaben: Dateiname
             Flag 'bNet': Wdh. bei gesperrtem Dateizugiff (05.03.2012, WW)
  Ergebnis: Dateinhalt als String }
var
  FS: TFileStream;
  StrS: TStringStream;
  bIsOpened: boolean;

begin
  { Initialisieren der Ausgabevariablen }
  Result := '';  { default }
  if (not FileExists(SourceFile)) then exit;
  try
    if bNet then  // 11.09.2008, WW
      FS:=TFileStreamExt.Create(SourceFile, fmOpenRead OR fmShareDenyWrite, bIsOpened)
    else begin
      FS:=TFileStream.Create(SourceFile, fmOpenRead OR fmShareDenyWrite);
      bIsOpened:=true;
    end;
    try
      if bIsOpened then begin
        StrS:=TStringStream.Create ('');
        try
          StrS.CopyFrom (FS, 0);
          Result:=StrS.DataString;
        finally
          StrS.Free;
        end;
      end;
    finally
      FS.Free;
    end;
  except
    result:= '';  { default }
  end;
end;

{--------------------------------------------}
function StringToFile(s, DestFile: string; bOverwrite: boolean = True;
  bCRLF: boolean = true; bNet: boolean = false): boolean;
{--------------------------------------------}
{ Der Inhalt eines Strings s wird in ein File 'DestFile' geschrieben;
  Übergaben: Zu schreibender String
             Dateiname
             Flag 'bOverwrite': Datei überschreiben ja/nein
             Flag 'bCRLF': String mit CR LF abschließen ja/nein
             Flag 'bNet': Wdh. bei gesperrtem Dateizugiff
  Ergebnis: true, wenn in Datei schreiben erfolgreich }
var
  TFS: TTextFileStream;
  bCreate: boolean;
  bIsOpened: boolean;

begin
  Result:=true;
  bCreate:=bOverwrite OR not FileExists (DestFile);
  try
    if bNet then begin  // 11.09.2008, WW
      if bCreate then
        TFS:=TTextFileStreamExt.Create (DestFile, fmCreate, bIsOpened)
      else
        TFS:=TTextFileStreamExt.Create (DestFile, fmOpenReadWrite OR fmShareDenyWrite,
                                        bIsOpened);
    end
    else begin
      if bCreate then
        TFS:=TTextFileStream.Create (DestFile, fmCreate)
      else
        TFS:=TTextFileStream.Create (DestFile, fmOpenReadWrite OR fmShareDenyWrite);
      bIsOpened:=true;
    end;
    try
      if bIsOpened then begin
        TFS.Seek (0, soFromEnd);
        if bCRLF then
          TFS.WriteLn (s)
        else if length (s) > 0 then
          TFS.Write(s[1], length (s));
      end else
        Result:=false;
    finally
      TFS.Free;
    end;
  except
    Result:=false;
  end;
end;

{ Gibt die Länge eines Textes zurück }
{ Parameter: Text, Font             }
{ Rückgabe: Länge auf dem Canvas     }
{------------------------------------}
function EstimatedTextLengthOf(Value: string; pFont: TFont): integer;
{------------------------------------}
begin
  with TLabel.Create(nil) do
  try
    Font.Assign(pFont);
    AutoSize := True;
    Caption := Value;
    Result := Width;
  finally
    Free;
  end;
end;

{------------------------------------}
function EstimatedTextHeightOf(Value: string; pFont: TFont): integer;
{------------------------------------}
begin
  with TLabel.Create(nil) do
  try
    Font.Assign(pFont);
    AutoSize := True;
    Caption := Value;
    Result := Height;
  finally
    Free;
  end;
end;

{ Gibt das Controlbitmap zurück      }
{ Parameter: abzubildendes Control   }
{------------------------------------}
function GetControlImage(pControl: TWinControl): TBitmap;
{------------------------------------}
var
  Ofs: Integer;
begin
  Result := TBitmap.Create;
  try
    Result.Width := pControl.Width;
    Result.Height := pControl.Height;
    Result.Canvas.Brush := pControl.Brush;
    Result.Canvas.FillRect(pControl.ClientRect);
    Result.Canvas.Lock;
    try
      if GetWindowLong(pControl.Handle, GWL_STYLE) and WS_BORDER <> 0 then
        Ofs := -1  // Don't draw form border
      else
        Ofs := 0;  // There is no border
      pControl.PaintTo(Result.Canvas.Handle, Ofs, Ofs);
    finally
      Result.Canvas.Unlock;
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ Wandelt Unixzeit in TDateTime um           }
{ Parameter: Unixzeit                        }
{ Rückgabe: Zeit als TDateTime               }
{--------------------------------------------}
function UnixToDateTime(iUnixTime: longint): TDateTime;
{--------------------------------------------}
var
  j,m,d,h,n,s : word;
  iLongWord   : Longword;
begin
  iLongWord := Longword(iUnixTime); // 20.09.2000
  j:=1970;
  m:=1;
  d:=1;
  h:=0;
  n:=0;
  if (iLongWord)>=31536000 then begin			{1970}
    while ((iLongWord>=31536000) and (j mod 4 <>0)) or
          ((iLongWord>=31622400)and(j mod 4 = 0))
	  do begin
	    if (j mod 4 = 0) then iLongWord:=iLongWord-31622400
	    else iLongWord:=iLongWord-31536000;
	    j:=j+1;
    end;
  end;

  if iLongWord>=2678400 then begin			{01/70}
    while ((iLongWord>=2678400)and((m=1)or(m=3)or(m=5)
        or(m=7)or(m=8)or(m=10)or(m=12))) or
	  ((iLongWord>=2592000)and((m=4)or(m=6)or(m=9)
	or (m=11))) or
	   ((iLongWord>=2419200)and((m=2)and(j mod 4 <>0))) or  // 15.03.2001
	   ((iLongWord>=2505600)and((m=2)and(j mod 4 = 0)))
	do begin
	  case m of
	    1,3,5,7,8,10,12: iLongWord:=iLongWord-2678400;
	    4,6,9,11	   : iLongWord:=iLongWord-2592000;
	    2		   : if (j mod 4 = 0) then iLongWord:=iLongWord-2505600
	                     else iLongWord:=iLongWord-2419200;
	  end;
	  m:=m+1;
    end;
  end;

  if iLongWord>=86400 then begin
    while (iLongWord>=86400) do begin
      iLongWord:=iLongWord-86400;
      d:=d+1;
    end;
  end;

  if iLongWord>=3600 then begin
    while (iLongWord>=3600) do begin
      iLongWord:=iLongWord-3600;
      h:=h+1;
    end;
  end;

  if iLongWord>=60 then begin
    while (iLongWord>=60) do begin
      iLongWord:=iLongWord-60;
      n:=n+1;
    end;
  end;

  s := iLongWord;

  Result := EncodeDate(j, m, d) + EncodeTime(h, n, s, 0);
end;

{ Wandelt Unixzeit in TDateTime um           }
{ Parameter: Unixzeit                        }
{ Rückgabe: Zeit als TDateTime               }
{--------------------------------------------}
function DateTimeToUnix(dtDateTime: TDateTime): LongWord;
{--------------------------------------------}
var
  y,m,d,h,n,s,ms : word;
  i              : integer;
begin
  DecodeDate(dtDateTime, y, m, d);
  DecodeTime(dtDateTime, h, n, s, ms);
  Result := 0;

  if (y > 1970) then
    for i := 1970 to y-1 do begin
      if (i mod 4 = 0) then Result := Result + 31622400
      else Result := Result + 31536000;
    end;

  if (m > 1) then
     for i := 1 to m-1 do begin
      case i of
        1,3,5,7,8,10,12: Result := Result + 2678400;
        4,6,9,11       : Result := Result + 2592000;
	2	       : if (y mod 4 = 0) then Result := Result + 2505600
	                 else Result := Result + 2419200;
      end;
    end;

  if (d > 1) then for i := 1 to d - 1 do Result := Result + 86400;

  if (h > 0) then for i := 1 to h do Result := Result + 3600;

  if (n > 0) then for i := 1 to n do Result := Result + 60;

  Result := Result + s;
end;


{ Gibt Element aus String mit <US> zurück    }
{ Parameter: String, Index des Elementes     }
{ Rückgabe: Element oder ''                  }
{--------------------------------------------}
function GetStringPart(
  const sString: string; iIndex: integer; cSeparator: char = #31): string;
{--------------------------------------------}
var
  iPos  : integer;
  iLoop : integer;
  s     : string;
begin
  Result := '';
  s := sString;
  for iLoop := 1 to iIndex-1 do begin
    iPos := Pos(cSeparator, s);
    if (iPos = 0) then Exit
    else Delete(s, 1, iPos);
  end;
  iPos := Pos(cSeparator, s);
  if (iPos > 0) then Result := Copy(s, 1, iPos-1)
  else if (Length(s) > 0) then Result := s;
end;


// Ersetzt Feld aus Parameterdefinition
// Parameter: Definitionszeile, Neuer Feldinhalt, Feldindex (ab 1)
// Rückgabe: Resultierender Listeneintrag
//-------------------------------------------------------
function ReplaceStringPart(const sString, sPart: string;
  iIndex: integer; cSeparator: char = #31): string;
//-------------------------------------------------------
var
  i, iCnt : integer;
begin
  Result := '';
  // Anzahl der Elemente ermitteln (über Separator)
  iCnt := 1;
  for i := 1 to Length(sString) do if (sString[i] = cSeparator) then Inc(iCnt);

  if (iCnt >= iIndex) then begin
    for i := 1 to iCnt do begin
      if (i = iIndex)
      then Result :=
        Result + sPart + cSeparator
      else Result :=
        Result + GetStringPart(sString, i, cSeparator) + cSeparator;
    end;
  end
  else begin
    Result := sString;
    for i := iCnt+1 to iIndex do Result := Result + cSeparator;
    Result := Result + sPart;
  end;
end;

// Wandelt durch Separaor getrennten Text in CommaText um
//--------------------------------------------
function SeparatedToCommaText(const sSeparator, sText: string): string;
//--------------------------------------------
begin
  if (Trim(sText) <> '') then begin
    Result := StringReplace(sText, sSeparator, '","', [rfReplaceAll]);
    Result := '"' + Result + '"';
  end
  else Result := sText;
end;

// Wandelt durch Pipes getrennten Text in CommaText um
//--------------------------------------------
function PipedToCommaText(const sPipedText: string): string;
//--------------------------------------------
begin
  Result := SeparatedToCommaText('|', sPipedText);
end;

// Wandelt durch Separator getrennten Text in CommaText um
//--------------------------------------------
function CommaToSeparatedText(const sSeparator, sCommaText: string): string;
//--------------------------------------------
var
  i : integer;
begin
  Result := '';
  with TStringList.Create do
  try
    CommaText := sCommaText;
    for i := 0 to Count-1 do begin
      if (Result <> '') then Result := Result + sSeparator;
      Result := Result + Strings[i];
    end;
  finally
    Free;
  end;
end;

// Wandelt durch Pipes getrennten Text in CommaText um
//--------------------------------------------
function CommaToPipedText(const sCommaText: string): string;
//--------------------------------------------
begin
  Result := CommaToSeparatedText('|', sCommaText);
end;

{ Tauscht Teilstring aus                     }
{ Parameter: String, alter Teil, neuer Teil  }
{ Rückgabe: Resultierender string            }
{--------------------------------------------}
function ChangeStringPart(sString, sPart1, sPart2: string): string;
{--------------------------------------------}
var
  i : integer;
begin
  Result := sString;
  if (sPart1 <> sPart2) then
    while (Pos(sPart1, Result) > 0) do begin
      i := Pos(sPart1, Result);
      Delete(Result, i, Length(sPart1));
      Insert(sPart2, Result, i);
    end;
end;

{ Tauscht Teilstring aus                     }
{ Parameter: String, alter Teil, neuer Teil  }
{ Rückgabe: Resultierender string            }
{--------------------------------------------}
function GetMyUserName: string;
{--------------------------------------------}
var
  p : PChar;
  i : DWord;
begin
  GetMem(p, 100);
  try
    p[0] := #0;
    i := 99;
    GetUserName(p, i);
    Result := StrPas(p);
  finally
    FreeMem(p, 100);
  end;
end;

{------------------------------------}
procedure CopyControlImageToClipBoard(pControl: TWinControl; pHandle: HWND = 0);
{------------------------------------}
var
  MyFormat : Word;
  Bitmap   : TBitmap;
  AData    : THandle;
  APalette : HPalette;
  pForm    : TForm;
  oldParent : TWinControl;
  oldAlign : TAlign;
  oldClientRect : TRect;
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  LockWindowUpdate(pControl.Handle);
  try
    pForm := TForm.Create(nil);
    oldAlign := pControl.Align;
    if (Assigned(pControl.Parent)) then begin
      oldClientRect := pControl.ClientRect;
      oldParent := pControl.Parent;
    end
    else begin
      oldParent := nil;
    end;
    try
      pForm.Height := 1;
      pForm.Width := 1;
      pForm.Caption := S_ClipboardImage;
      pForm.Visible := True;
      pForm.SendToBack;

      pForm.Width := Screen.Width;
      pForm.Height := Screen.Height;
      pControl.Parent := pForm;
      pControl.Align := alClient;
      pForm.Invalidate;
      pControl.Invalidate;
      Application.ProcessMessages;
      pForm.Hide;
      Bitmap := GetControlImage(pControl);
      try
        Bitmap.SaveToClipBoardFormat(MyFormat, AData, APalette);
        ClipBoard.SetAsHandle(MyFormat,AData);
      finally
        Bitmap.Free;
      end;
    finally
      pControl.Parent := oldParent;
      pControl.Align := oldAlign;
      if (Assigned(pControl.Parent)) then begin
        pControl.SetBounds(oldClientRect.Left, oldClientRect.Top,
          (oldClientRect.Right - oldClientRect.Left),
          (oldClientRect.Bottom - oldClientRect.Top));
      end;
      pForm.Free;
    end;
  finally
    LockWindowUpdate(0);
    Screen.Cursor := oldCursor;
  end;
end;


{ Ereignisproc für Freigabetimer     }
{ Parameter: Objekt                  }
{ Timer-Callback                     }
{------------------------------------}
procedure ThisTimerProc(
  MyHandle: HWND; MyMsg, MyEvent: UINT; MyTime: DWord); stdcall; // 03.04.2001
{------------------------------------}
const
  C_Object : TObject = nil;
begin
  if (MyTime = 0) then begin
    C_Object := TObject(MyHandle);
  end
  else begin
    KillTimer(0, MyEvent);
    if (C_Object is TForm) then TForm(C_Object).Release else C_Object.Free;
    C_Object := nil;
  end;
end;

{ Timerproc zum Freig. von Objekten  }
{ Parameter: Objekt, Totzeit         }
{------------------------------------}
procedure FreeTimer(pObject: TObject; iMs: integer);  // 03.04.2001
{------------------------------------}
begin
  ThisTimerProc(Integer(pObject), SetTimer(0, 1, iMs, @ThisTimerProc), 0, 0);
end;

{ Timerproc zum Durchführen e. Aktion}
{ Parameter: Timerevent, Totzeit     }
{------------------------------------}
procedure ActionTimer(pEvent: TNotifyEvent; iMs: integer);  // 03.04.2001
{------------------------------------}
begin
  with TTimer.Create(nil) do begin
    Enabled := False;
    Interval := iMs;
    OnTimer := pEvent;
    Enabled := True;
  end;
end;

{ Verbindet oder trennt Menüs                  }
{ Parameter: Menu; Status (T-verb.,F-trennen)  }
{----------------------------------------------}
procedure MergeMenu(pMenu: TMainMenu; bState: boolean); // 23.05.2001
{----------------------------------------------}
var
  pCtrl : TControl;
  pParentForm, pForm : TForm;
  pParentMenu : TMainMenu;
  i     : integer;
begin
  // AutoMerge funktioniert nicht -> manuell einbetten
  if (Assigned(pMenu.Owner)) and (pMenu.Owner is TForm) then begin
    pForm := TForm(pMenu.Owner);
    pCtrl := pForm.Parent;

    while (Assigned(pCtrl)) and (Assigned(pCtrl.Parent)) do
      pCtrl := pCtrl.Parent;

    if (Assigned(pCtrl)) and (pCtrl is TForm) then begin
      pParentForm := TForm(pCtrl);
      for i := 0 to pParentForm.ComponentCount-1 do
        if (pParentForm.Components[i] is TMainMenu) then begin
          pParentMenu := TMainMenu(pParentForm.Components[i]);
          if (bState)
          then pParentMenu.Merge(pMenu)
          else pParentMenu.Unmerge(pMenu);
          Break;
        end;
    end;
  end;
end;

{ Gibt Versionsinfo zu einer Datei zurück      }
{ Parameter: Dateiname                         }
{ Rückgabe: Inforecord                         }
{----------------------------------------------}
function GetVersionInfo(sFileName: TFileName): VS_FIXEDFILEINFO;  // 22.07.2001
{----------------------------------------------}
var
  p     : pointer;
  i, j  : cardinal;
  pInfo : ^VS_FIXEDFILEINFO;
begin
  FillChar(Result, SizeOf(Result), 0);
  j := GetFileVersionInfoSize(PChar(sFileName), i);

  if (j > 0) then begin
    GetMem(p, j);
    try
      if (GetFileVersionInfo(PChar(sFileName), 0, j, p)) then begin  // j statt 200; 18.01.2008, WW
        i := SizeOf(VS_FIXEDFILEINFO);
        if (VerQueryValue(p, '\', Pointer(pInfo), i)) then Result := pInfo^;
      end;
    finally
      Freemem(p, j);
    end;
  end;
end;

{---------------------------------------------------------------}
function WGetVersionInfo (Filename: string; var sVersion: string;
                          var sFileDate: string): boolean;
{---------------------------------------------------------------}
{ gibt Datei-Version/-Datum zurück; 13.02.2007, WW }
var
  FileInfo: VS_FIXEDFILEINFO;

begin
  if FileExists (Filename) then begin
    FileInfo:=GetVersionInfo(FileName);
    sVersion:=IntToStr (HiWord(FileInfo.dwFileVersionMS)) + '.' +
              IntToStr (LoWord(FileInfo.dwFileVersionMS)) + '.' +  // 10.12.2013, WW
              IntToStr (HiWord(FileInfo.dwFileVersionLS));
    sFileDate:=DateToStr(FileDateToDateTime(FileAge(FileName)));
    Result:=true;
  end
  else begin
    sVersion:='';
    sFileDate:='';
    Result:=false;
  end;
end;

{ Gibt höhere beiden Bytes eines DWords zurück }
{ Parameter: DWord                             }
{ Rückgabe: Ergebnis als Word                  }
{----------------------------------------------}
function HiDWord(i: DWord): word;  // 22.07.2001
{----------------------------------------------}
var
  s : string;
begin
  s := IntToHex(i, 8);
  Result := StrToInt('$' + Copy(s, 1, 4));
end;

{ Gibt nidere beiden Bytes eines DWords zurück }
{ Parameter: DWord                             }
{ Rückgabe: Ergebnis als Word                  }
{----------------------------------------------}
function LoDWord(i: DWord): word;  // 22.07.2001
{----------------------------------------------}
var
  s : string;
begin
  s := IntToHex(i, 8);
  Result := StrToInt('$' + Copy(s, 5, 4));
end;

{ Filtert unerlaubte Zeichen aus String       }
{ Parameter: Ursprünglicher String; Set der   }
{   erlaubten/verbotenen Zeichen              }
{ Rückgabe: Gefilterter String                }
{---------------------------------------------}
function FilterCharsFromString(sText: string; pCSAllowed: TCharSet = [];
  pCSForbidden: TCharSet = []): string;  // 06.03.2001
{---------------------------------------------}
var
  i : integer;
begin
  Result := sText;
  for i := Length(sText) downto 1 do
    if ((Result[i] in ['\','/',':','*','?','"','<','>','|']) or
        (Result[i] in pCSForbidden)) and
       (not (Result[i] in pCSAllowed))
    then System.Delete(Result, i, 1);
end;

{---------------------------------------------}
function WStrToFloatDef(const sZahl: string; dDefault: double): double; // 20.10.2001
{---------------------------------------------}
begin
  try
    if (Trim(sZahl) = '')
    then Result := dDefault
    else Result :=
      StrToFloat(StringReplace(sZahl, ThousandSeparator, '', [rfReplaceAll]));
  except
    Result := dDefault;
  end;
end;

{---------------------------------------------}
function Round(const fZahl: Extended): Int64; // 24.10.2001
{---------------------------------------------}
begin
  Result := System.Round(fZahl);
end;

{---------------------------------------------}
function Round(const fZahl: Extended; iPrec: byte): Extended; // 24.10.2001
{---------------------------------------------}
begin
  Result := GD_Utils.Round(IntPower(10, iPrec) * fZahl) / IntPower(10, iPrec);
end;

{ Gerundete Grenzen für einen Bereich         }
{ Paramete: Minimal-/Maximalwert,             }
{   Rückgabe untere/obere Grenze              }
{ Ergebnis: Erfolg ja/nein                    }
{---------------------------------------------}
function WRangeSet(fMinVal, fMaxVal: double; var fLow, fHigh: double): boolean;
{---------------------------------------------}
var
  sFrac, sTrunc : string;
  fDiff         : double;
  iDigits       : integer;
begin
  try
    if (fMinVal = fMaxVal) then begin
      sFrac := FloatToStr(Frac(fMinVal));
      if (sFrac = '0') then begin
        fLow := fMinVal-1;
        fHigh := fMaxVal+1;
      end
      else begin
        iDigits := Length(sFrac)-2;  // Länge der Nachkommastellen
        iDigits := -(iDigits-1);     // Dimension zum Runden
        SetRoundMode(rmUp);          // Aufrunden;
        fHigh := RoundTo(fMaxVal, iDigits);
        SetRoundMode(rmDown);        // Abrunden;
        fLow := RoundTo(fMinVal, iDigits);
      end;
    end
    else begin
      fDiff := fMaxVal - fMinVal;
      if (fDiff < 1) then begin
        sFrac := FloatToStr(Frac(fDiff));
        iDigits := Length(sFrac)-2;  // Länge der Nachkommastellen
        iDigits := -(iDigits-1);     // Dimension zum Runden
        SetRoundMode(rmUp);          // Aufrunden;
        fHigh := RoundTo(fMaxVal, iDigits);
        SetRoundMode(rmDown);        // Abrunden;
        fLow := RoundTo(fMinVal, iDigits);
      end
      else begin
        sTrunc := IntToStr(Trunc(fDiff));
        iDigits := Length(sTrunc);   // Länge des Integerwertes
        iDigits := iDigits-1;        // Dimension zum Runden
        SetRoundMode(rmUp);          // Aufrunden;
        fHigh := RoundTo(fMaxVal, iDigits);
        if (fHigh = fMaxVal) then fHigh := fMaxVal+1;
        SetRoundMode(rmDown);        // Abrunden;
        fLow := RoundTo(fMinVal, iDigits);
        if (fLow = fMinVal) then fLow := fMinVal-1;
      end;
    end;
    Result := True;
  except
    fLow := fMinVal;
    fHigh := fMaxVal;
    Result := False;
  end;
end;

{ Wandelt einen Integer in die binäre Stringdarstellung um }
{ Parameter: anInt: Integerzahl, die darzustellen ist      }
{            Digits: minimal angezeigte Zeichen            }
{----------------------------------------------------}
function WIntToBin(iInt: integer; iDigits: byte): string;
{----------------------------------------------------}
var
  i : integer;
begin
  Result := '';  { default }
  i := iInt;
  { Umwandeln in Binär-Format }
  while (i > 0) do begin
    Result := IntToStr((i mod 2)) + Result;
    i := i - ((i div 2)+(i mod 2));
  end;
  { Länge auf 'Digits' auffülln }
  while (Length(Result) < iDigits) do Result := '0' + Result;
end;

{ Sonderzeicher wurden als <ORD> geschrieben  }
{ Parameter: String mit decodierten Zeichen   }
{ Rückgabe: String mit Sonderzeichen          }
{---------------------------------------------}
function ReconvertLine(sLine: string): string;
{---------------------------------------------}
var
  s : string;
  i, j : integer;
begin
  Result := '';

  while (Length(sLine) > 0) do begin
    i := Pos('<', sLine);
    j := Pos('>', sLine);

  // Auf reservierte Platzhalter überprüfen und entsprechend bearbeiten
    if (i > 0) and (j > 0) then begin
      s := Copy(sLine, i+1, j-i-1);
      if (StrToIntDef(s, -1) >= 0)
        then s := Chr(StrToInt(s)) else s := Copy(sLine, i, j-i+1);
      Result := Result + Copy(sLine, 1, i-1) + s;
      System.Delete(sLine, 1, j);
    end
    else begin
      Result := Result + sLine;
      sLine := '';
    end;
  end;

end;

{ En-/Disablen von Controls                            }
{ Parameter: Control, Status, Cursorbild,              }
{            Flag, ob übergeordnete Parent einbez. w.  }
{------------------------------------------------------}
procedure EnableMyControls(Sender: TWinControl; bState: boolean;
  bHourGlass: boolean = True; bFirstParent: boolean = True);
{------------------------------------------------------}
const
  C_OldCursor : TCursor = crDefault;
  C_Disabled  : boolean = True;
var
  i : integer;
  p : TWinControl;
  f : TForm;
begin
  if (bState = C_Disabled) and (bFirstParent)
  then Exit
  else if (bFirstParent) then C_Disabled := bState;

  if (bHourGlass) then begin
    if (bState)
    then Screen.Cursor := C_OldCursor
    else begin
      if (Screen.Cursor <> crHourGlass) then begin  // für Mehrfachaufrufe hintereinander; 07.06.2022, WW
        C_OldCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
      end;
    end;
  end;

  p := Sender;
  if (bFirstParent) then begin
    while (Assigned(p.Parent)) do p := p.Parent;
    if (p is TForm) and (TForm(p).FormStyle = fsMDIChild)
    then f := Application.MainForm
    else f := nil;

    if (Assigned(f)) then begin
      EnableMyControls(f, bState, False, False);
      for i := 0 to f.MDIChildCount-1 do
        EnableMyControls(f.MDIChildren[i], bState, False, False);
    end;
  end;

  // Controls en-/disablen
  for i := 0 to p.ControlCount-1 do begin
    if (p.Controls[i] is TWinControl) then
      EnableMyControls(TWinControl(p.Controls[i]), bState, False, False);
    p.Controls[i].Enabled := bState;
  end;

  // Menu-Items en-/disablen
  for i := 0 to p.ComponentCount-1 do
    if (p.Components[i] is TMenuItem) then
      TMenuItem(p.Components[i]).Enabled := bState;
end;

{ Prüft, ob Control (mit Parentcontrols) sichtbar ist  }
{ Parameter: zu prüfendes Control                      }
{------------------------------------------------------}
function ControlVisible(Sender: TWinControl): boolean;
{------------------------------------------------------}
var
  p : TWinControl;
begin
  p := Sender;
  Result := Assigned(p);

  while (Assigned(p)) do begin
    Result := p.Visible;
    if (not Result) then Break;
    p := p.Parent;
  end;
end;

{ Ändert die Checked-Eigenschaft eines Checkbox- oder Radiobutton- }
{ Controls, ohne das OnClick-Ereignis auszulösen;                  }
{ Übergaben: Control, Checked-Status                               }
{------------------------------------------------------------------}
procedure SetControlCheckedNoEvent(AControl: TControl; AChecked: Boolean);
{------------------------------------------------------------------}
var
  OldEvent: TNotifyEvent;

begin
  if AControl is TCheckBox then begin  // Checkbox
    // Altes Event merken
    OldEvent := (AControl as TCheckBox).OnClick;
    (AControl as TCheckBox).OnClick := nil;

    // Änderungen durchführen, zum Beispiel:
    (AControl as TCheckBox).Checked := AChecked;

    // Event zurücksetzen
    (AControl as TCheckBox).OnClick := OldEvent;
  end
  else if AControl is TRadioButton then begin  // Radiobutton
    // Altes Event merken
    OldEvent := (AControl as TRadioButton).OnClick;
    (AControl as TRadioButton).OnClick := nil;

    // Änderungen durchführen, zum Beispiel:
    (AControl as TRadioButton).Checked := AChecked;

    // Event zurücksetzen
    (AControl as TRadioButton).OnClick := OldEvent;
  end;
end;

{------------------------------------------------------}
function MyAnsiToOem(sText: string): string;
{------------------------------------------------------}
var
  pDest : PChar;
begin
  GetMem(pDest, Length(sText) + 1);
  try
    if (OemToChar(PChar(sText), pDest))
    then Result := pDest
    else Result := sText;

    if (CharToOem(PChar(Result), pDest)) then Result := pDest;
  finally
    FreeMem(pDest, Length(sText) + 1);
  end;
end;

{------------------------------------------------------}
function MyOemToAnsi(sText: string): string;
{------------------------------------------------------}
var
  pDest : PChar;
begin
  GetMem(pDest, Length(sText) + 1);
  try
    if (CharToOem(PChar(sText), pDest))
    then Result := pDest
    else Result := Result;

    if (OemToChar(PChar(Result), pDest)) then Result := pDest;
  finally
    FreeMem(pDest, Length(sText) + 1);
  end;
end;

{ Gibt logische Laufwerke in string zurück              }
{   Separator: ';'                                      }
{ Parameter: Set: 1=HD, 2=CD, 3=OtherRemovable, 4=Remote}
{-------------------------------------------------------}
function WGetLogicalDrives(pDriveTypes: TByteSet = []): string;
{-------------------------------------------------------}
var
  iDrive, iDrives : longint;
  i               : byte;
  iDriveType      : integer;
  sDrive          : string;
begin
  Result := '';
  iDrives := GetLogicalDrives;
  for i := 0 to (Ord('Z')-Ord('A')) do begin
    iDrive := Trunc(Power(2, i));
    if ((iDrives and iDrive) > 0) then begin
      sDrive := Chr(Ord('A') + i) + ':\';
      if (pDriveTypes <> []) then begin
        iDriveType := GetDriveType(PChar(sDrive));
        if ((iDriveType = DRIVE_FIXED) and (not (1 in pDriveTypes))) or
           ((iDriveType = DRIVE_CDROM) and (not (2 in pDriveTypes))) or
           ((iDriveType = DRIVE_REMOVABLE) and (not (3 in pDriveTypes))) or
           ((iDriveType = DRIVE_REMOTE) and (not (4 in pDriveTypes)))
        then Continue;
      end;

      if (Result <> '') then Result := Result + ';';
      Result := Result + sDrive;
    end;
  end;
end;

{--------------------------------------------}
function GDCopyControl(
  pControl: TControl; pNewOwner: TControl; pNewParent: TWinControl): TControl;
{--------------------------------------------}
var
  pClass : TControlClass;
  i      : integer;
begin
  pClass := TControlClass(pControl.ClassType);
  Result := pClass.Create(pNewOwner);
  Result.Parent := pNewParent;
  Result.Align := pControl.Align;
  Result.BoundsRect := pControl.BoundsRect;
  Result.Tag := pControl.Tag;
  Result.Enabled := pControl.Enabled;
  TTrickControl(Result).Color := TTrickControl(pControl).Color;
  TTrickControl(Result).Text := TTrickControl(pControl).Text;
  TTrickControl(Result).Font := TTrickControl(pControl).Font;
  TTrickControl(Result).OnClick := TTrickControl(pControl).OnClick;
  TTrickControl(Result).OnDblClick := TTrickControl(pControl).OnDblClick;
  TTrickControl(Result).OnResize := TTrickControl(pControl).OnResize;
  TTrickControl(Result).PopupMenu := TTrickControl(pControl).PopupMenu;

  if (pControl is TWinControl) then begin
    if (pControl is TPanel) then
      with TPanel(Result) do begin
        BevelInner := TPanel(pControl).BevelInner;
        BevelOuter := TPanel(pControl).BevelOuter;
        BorderWidth := TPanel(pControl).BorderWidth;
        BorderStyle := TPanel(pControl).BorderStyle;
        Alignment := TPanel(pControl).Alignment;
      end
    else if (pControl is TComboBox) then
      with TComboBox(Result) do begin
        Style := TComboBox(pControl).Style;
        Items.Text := TComboBox(pControl).Items.Text;
        ItemIndex := TComboBox(pControl).ItemIndex;
        OnCloseUp :=  TComboBox(pControl).OnCloseUp;
      end
    else if (pControl is TPageControl) then
      with TPageControl(Result) do begin
        Style := TPageControl(pControl).Style;
        TabPosition := TPageControl(pControl).TabPosition;
      end
    else if (pControl is TCheckbox) then
      with TCheckbox(Result) do begin
        Checked := TCheckbox(pControl).Checked;
      end
    else if (pControl is TMemo) then
      with TMemo(Result) do begin
        ScrollBars := TMemo(pControl).ScrollBars;
      end
    else if (pControl is TDrawGrid) then
      with TDrawGrid(Result) do begin
        DefaultRowHeight := TDrawGrid(pControl).DefaultRowHeight;
        Options := TDrawGrid(pControl).Options;
        OnDrawCell := TDrawGrid(pControl).OnDrawCell;
        OnMouseWheelDown := TDrawGrid(pControl).OnMouseWheelDown;
      end
    else if (pControl is TListBox) then  // 26.01.2015
      with TListBox(Result) do begin
        Style := TListBox(pControl).Style;
        BorderStyle := TListBox(pControl).BorderStyle;
        TabWidth := TListBox(pControl).TabWidth;
        OnDrawItem := TListBox(pControl).OnDrawItem;
      end
    else if (pControl is TTabSheet) then
      with TTabSheet(Result) do begin
        PageControl := TPageControl(pNewParent);
        if (TTabSheet(pControl).PageControl = PageControl) then
          PageIndex := PageControl.PageCount-1
        else if (TTabSheet(pControl).PageIndex < PageControl.PageCount) then
          PageIndex := TTabSheet(pControl).PageIndex;
      end;

    with TWinControl(pControl) do begin
      for i := 0 to ControlCount-1 do begin
        GDCopyControl(Controls[i], pNewOwner, TWinControl(Result));
      end;
    end;
  end
  else if (pControl is TSpeedButton) then begin
    TSpeedButton(Result).Flat := TSpeedButton(pControl).Flat;
    TSpeedButton(Result).AllowAllUp := TSpeedButton(pControl).AllowAllUp;
    TSpeedButton(Result).GroupIndex := TSpeedButton(pControl).GroupIndex;
  end
  else if (pControl is TLabel) then begin
    TLabel(Result).Alignment := TLabel(pControl).Alignment;
    TLabel(Result).AutoSize := TLabel(pControl).AutoSize;
    TLabel(Result).Transparent := TLabel(pControl).Transparent;
    if (not TLabel(Result).AutoSize) then
      Result.BoundsRect := pControl.BoundsRect;
  end
  else if (pControl is TShape) then begin
    TShape(Result).Shape := TShape(pControl).Shape;
    TShape(Result).Brush.Color := TShape(pControl).Brush.Color;
  end
  else if (pControl is TImage) then begin
    TImage(Result).Transparent := TImage(pControl).Transparent;
    TImage(Result).Visible := TImage(pControl).Visible;
    TImage(Result).Proportional := TImage(pControl).Proportional;
    TImage(Result).Stretch := TImage(pControl).Stretch;
    TImage(Result).Picture.Assign(TImage(pControl).Picture);
  end;
end;

{--------------------------------------------}
function GDDeleteControl(pControl: TControl): boolean;
{--------------------------------------------}
var
  i : integer;
begin
  try
    if (pControl is TWinControl) then begin
      for i := TWinControl(pControl).ControlCount-1 downto 0 do begin
        if (not GDDeleteControl(TWinControl(pControl).Controls[i])) then begin
          Result := False;
          Exit;
        end;
      end;
    end;
    pControl.Free;
    Result := True;
  except
    Result := False;
  end;
end;

{--------------------------------------------}
function GDFindControlWithThisTagProperty(
  pControl: TControl; iTag: integer; bIncludeSelf: boolean = True): TControl;
{--------------------------------------------}
var
  i : integer;
begin
  Result := nil;
  if (bIncludeSelf) and (pControl.Tag = iTag) then Result := pControl
  else if (pControl is TWinControl) then begin
    for i := 0 to TWinControl(pControl).ControlCount-1 do begin
      Result := GDFindControlWithThisTagProperty(
        TWinControl(pControl).Controls[i], iTag);
      if (Assigned(Result)) then Break;
    end;
  end;
end;

{--------------------------------------------}
function GDFindControlWithThisTagProperty(pControl: TControl; iTag: integer;
  const sClassname: string; bIncludeSelf: boolean = True): TControl;
{--------------------------------------------}
var
  i : integer;
begin
  Result := nil;
  if (bIncludeSelf) and (pControl.Tag = iTag) and
    (UpperCase(pControl.ClassName) = UpperCase(sClassname))
  then Result := pControl
  else if (pControl is TWinControl) then begin
    for i := 0 to TWinControl(pControl).ControlCount-1 do begin
      Result := GDFindControlWithThisTagProperty(
        TWinControl(pControl).Controls[i], iTag);
      if (Assigned(Result)) then Break;
    end;
  end;
end;

{--------------------------------------------}
function GDGetHostName: string;
{--------------------------------------------}
var
  rSockVer : WordRec;
  aWSAData : TWSAData;
  szHostName : array[0..255] of Char;
begin
  // WinSock initialisieren
  rSockVer.Hi := 1;
  rSockVer.Lo := 1;
  WSAStartup(Word(rSockVer), aWSAData );
  try
    FillChar(szHostName, SizeOf(szHostName), #0);
    GetHostName(szHostName, SizeOf(szHostName));

    Result := szHostName;
  finally
    WSACleanup;
  end;
end;

{--------------------------------------------}
function GDGetLocalIP: string;
{--------------------------------------------}
resourcestring
  cTxtIP = '%d.%d.%d.%d';
var
  rSockVer : WordRec;
  aWSAData : TWSAData;
  szHostName : array[0..255] of Char;
  pHE : PHostEnt;
  sIP : String;
begin
  // WinSock initialisieren
  rSockVer.Hi := 1;
  rSockVer.Lo := 1;
  WSAStartup(Word(rSockVer), aWSAData );
  try
    FillChar(szHostName, SizeOf(szHostName), #0);
    GetHostName(szHostName, SizeOf(szHostName));
    pHE := GetHostByName(szHostName);
    if Assigned(pHE) then
      with pHE^ do
        sIP := Format(cTxtIP, [Byte(h_addr^[0]), Byte(h_addr^[1]),
          Byte(h_addr^[2]), Byte(h_addr^[3])]);
  finally
    WSACleanup;
  end;
  Result := sIP;
end;

{ Liest den Installationspfad eines Dienstes aus der }
{ Registrierdatenbank und gibt diesen zurück         }
{ Parameter: Dienstname                              }
{ Rückgabe: Installationspfad oder leer              }
{----------------------------------------------------}
function GDGetServiceImagePath(const sServiceName: string): string;
{----------------------------------------------------}
begin
  Result := '';
  try
    if length (sServiceName) > 0 then begin
      with TRegistry.Create do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if (OpenKey('\SYSTEM\CurrentControlSet\Services\' + sServiceName, False)) then
          Result := ReadString('ImagePath');
      finally
        Free;
      end;
    end;
  except
    Result := '';
  end;
end;

end.
