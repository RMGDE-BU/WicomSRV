unit ZipMstr;
(* TZipMaster VCL by Chris Vleghert and Eric W. Engler
   e-mail: englere@abraxis.com
   www:    http://www.geocities.com/SiliconValley/Network/2114
 v1.73 by Russell Peters October 3, 2003.

            *)

{$DEFINE DEBUGCALLBACK}
//{$DEFINE NO_SPAN}
//{$DEFINE NO_SFX}
// define 'INTERNAL_SFX' to include SFXSlave
{$DEFINE INTERNAL_SFX}

{$INCLUDE ZipVers.inc}
{$IFDEF VER140}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
{$IFDEF INTERNAL_SFX}
{$UNDEF NO_SFX}
{$ENDIF}

interface

uses
  Forms, WinTypes, WinProcs, SysUtils, Classes, Messages, Dialogs, Controls,
{$IFNDEF NO_SFX}
  ZipSFX,
{$ENDIF}
  ZipStructs, ZipDLL, UnzDLL, ZCallBck, ZipMsg,
  ShellApi, Graphics, Buttons, StdCtrls, FileCtrl;

const
  ZIPMASTERVERSION: string = '1.73';
  ZIPMASTERBUILD: string = '1.73.3.0';
  ZIPMASTERVER: integer = 173;
  Min_ZipDll_Vers: integer = 173;
  Min_UnzDll_Vers: integer = 172;

  BUSY_ERROR = -255;

{$IFDEF VERD2D3}
type
  LargeInt = Comp;
type
  pLargeInt = ^Comp;
type
  LongWord = Cardinal;
type
  TDate = type TDateTime;
const
  mrNoToAll = mrNo + 1;
{$ENDIF}
{$IFDEF VERD4+}
type
  LargeInt = Int64;
type
  pLargeInt = ^Int64;
{$ENDIF}
{$IFDEF NO_SFX}
type
  TCustomZipSFX = pointer;
{$ENDIF}

  //------------------------------------------------------------------------
const
  zprFile   = 0;
  zprArchive = 1;
  zprCopyTemp = 2;
  zprSFX    = 3;
  zprHeader = 4;
  zprFinish = 5;
  zprCompressed = 6;
  zprCentral = 7;
type
  // new 1.73
  ActionCodes = (zacTick, zacItem, zacProgress, zacEndOfBatch, zacMessage,
    zacCount, zacSize, zacNewName, zacPassword, zacCRCError, zacOverwrite,
    zacSkipped, zacComment, zacStream, zacData, zacXItem, zacXProgress, zacDLL);

  ProgressType = (NewFile, ProgressUpdate, EndOfBatch, TotalFiles2Process,
    TotalSize2Process, NewExtra, ExtraUpdate);

  // 1.73 added AddForceDest
  AddOptsEnum = (AddDirNames, AddRecurseDirs, AddMove, AddFreshen, AddUpdate,
    AddZipTime, AddForceDOS, AddHiddenFiles, AddArchiveOnly, AddResetArchive,
    AddEncrypt, AddSeparateDirs, AddVolume, AddFromDate, AddSafe, AddForceDest,
    AddDiskSpan, AddDiskSpanErase);
  AddOpts = set of AddOptsEnum;
  // new 1.72
  SpanOptsEnum = (spNoVolumeName, spCompatName, spWipeFiles, spTryFormat);
  SpanOpts = set of SpanOptsEnum;

  // When changing this enum also change the pointer array in the function AddSuffix,
  // and the initialisation of ZipMaster. Also keep assGIF as first and assEXE as last value.
  AddStoreSuffixEnum = (assGIF, assPNG, assZ, assZIP, assZOO, assARC,
    assLZH, assARJ, assTAZ, assTGZ, assLHA, assRAR,
    assACE, assCAB, assGZ, assGZIP, assJAR, assEXE, assEXT); // 1.71 added assEXT

  AddStoreExts = set of AddStoreSuffixEnum;

  ExtrOptsEnum = (ExtrDirNames, ExtrOverWrite, ExtrFreshen, ExtrUpdate,
    ExtrTest, ExtrForceDirs); // 1.73 added ExtrForceDirs
  ExtrOpts = set of ExtrOptsEnum;

  SFXOptsEnum = (SFXAskCmdLine, SFXAskFiles, SFXAutoRun, SFXHideOverWriteBox,
    SFXCheckSize, SFXNoSuccessMsg);
  SFXOpts = set of SFXOptsEnum;

  OvrOpts = (OvrConfirm, OvrAlways, OvrNever);

  CodePageOpts = (cpAuto, cpNone, cpOEM);
  CodePageDirection = (cpdOEM2ISO, cpdISO2OEM);

  DeleteOpts = (htdFinal, htdAllowUndo);

  UnZipSkipTypes = (stOnFreshen, stNoOverwrite, stFileExists, stBadPassword,
    stNoEncryptionDLL, stCompressionUnknown, stUnknownZipHost,
    stZipFileFormatWrong, stGeneralExtractError);

  ZipDiskStatusEnum = (zdsEmpty, zdsHasFiles, zdsPreviousDisk, zdsSameFileName,
    zdsNotEnoughSpace);
  TZipDiskStatus = set of ZipDiskStatusEnum;
  TZipDiskAction = (zdaOk, zdaErase, zdaReject, zdaCancel);

type
  ZipDirEntry = packed record // fixed part size = 42
    MadeByVersion: Byte;
    HostVersionNo: Byte;
    Version: Word;
    Flag: Word;
    CompressionMethod: Word;
    DateTime: Integer;        // Time: Word; Date: Word; }
    CRC32: Integer;
    CompressedSize: Integer;
    UncompressedSize: Integer;
    FileNameLength: Word;
    ExtraFieldLength: Word;
    FileCommentLen: Word;
    StartOnDisk: Word;
    IntFileAttrib: Word;
    ExtFileAttrib: LongWord;
    RelOffLocalHdr: LongWord;
    FileName: string;         // variable size
    FileComment: string;      // variable size
    Encrypted: Boolean;
    ExtraData: string;        // New v1.6, used in CopyZippedFiles()
  end;
  pZipDirEntry = ^ZipDirEntry;

type
  ZipRenameRec = record
    Source: string;
    Dest: string;
    DateTime: Integer;
  end;
  pZipRenameRec = ^ZipRenameRec;

type
  TPasswordButton = (pwbOk, pwbCancel, pwbCancelAll, pwbAbort);
  TPasswordButtons = set of TPasswordButton;

type
  // 1.72.1.0 - FileSize changed to cardinal
  TProgressEvent = procedure(Sender: TObject; ProgrType: ProgressType;
    Filename: string; FileSize: cardinal) of object;
  TMessageEvent = procedure(Sender: TObject; ErrCode: Integer; Message: string)
    of object;
  TSetNewNameEvent = procedure(Sender: TObject; var OldFileName: string;
    var IsChanged: Boolean) of object;
  TNewNameEvent = procedure(Sender: TObject; SeqNo: Integer;
    ZipEntry: ZipDirEntry) of object;
  TPasswordErrorEvent = procedure(Sender: TObject; IsZipAction: Boolean;
    var NewPassword: string; ForFile: string; var RepeatCount: LongWord;
    var Action: TPasswordButton) of object;
  TCRC32ErrorEvent = procedure(Sender: TObject; ForFile: string;
    FoundCRC, ExpectedCRC: LongWord; var DoExtract: Boolean) of object;
  TExtractOverwriteEvent = procedure(Sender: TObject; ForFile: string;
    IsOlder: Boolean; var DoOverwrite: Boolean; DirIndex: Integer) of object;
  TExtractSkippedEvent = procedure(Sender: TObject; ForFile: string;
    SkipType: UnZipSkipTypes; ExtError: Integer) of object;
  TCopyZipOverwriteEvent = procedure(Sender: TObject; ForFile: string;
    var DoOverwrite: Boolean) of object;
  TGetNextDiskEvent = procedure(Sender: TObject; DiskSeqNo, DiskTotal: Integer;
    Drive: string; var AbortAction: Boolean) of object;
  TStatusDiskEvent = procedure(Sender: TObject; PreviousDisk: Integer;
    PreviousFile: string; Status: TZipDiskStatus;
    var Action: TZipDiskAction) of object;
  TFileCommentEvent = procedure(Sender: TObject; ForFile: string;
    var FileComment: string; var IsChanged: Boolean) of object;
  TAfterCallbackEvent = procedure(Sender: TObject; var abort: boolean) of object;
  TTickEvent = procedure(Sender: TObject) of object;
  TFileExtraEvent = procedure(Sender: TObject; ForFile: string; var Data: string;
    var IsChanged: Boolean) of object;
  TItemProgressEvent = procedure(Sender: TObject; Item: string;
    TotalSize: cardinal; PerCent: integer) of object; // new 1.73
  TTotalProgressEvent = procedure(Sender: TObject; TotalSize: cardinal; PerCent: integer)
    of object;                // new 1.73
  TZipStrEvent = procedure(Sender: TObject; ID: integer; var Str: string) of object; // new 1.73

type
  // 1.73 10 July 2003 RP changed to allow later translations
  EZipMaster = class(Exception)
  public
    FDisplayMsg: Boolean;     // We do not always want to see a message after an exception.
    // We also save the Resource ID in case the resource is not linked in the application.
    FResIdent: Integer;
    Arg1, Arg2: string;       // save arguments for translation

    constructor CreateResDisp(const Ident: Integer; const Display: Boolean);
    constructor CreateResDisk(const Ident: Integer; const DiskNo: Integer);
    constructor CreateResDrive(const Ident: Integer; const Drive: string);
    constructor CreateResFile(const Ident: Integer; const File1, File2: string);
  end;

type
  TZipStream = class(TMemoryStream)
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetPointer(Ptr: Pointer; Size: Integer); virtual;
  end;

type
  TZipMaster = class(TComponent)
  private
    // fields of published properties
    FAddCompLevel: Integer;
    fAddOptions: AddOpts;
    FAddStoreSuffixes: AddStoreExts;
    fExtAddStoreSuffixes: string;
    { Private versions of property variables }
    fCancel: Boolean;
    FDirOnlyCount: Integer;
    fErrCode: Integer;
    fFullErrCode: Integer;
    fHandle: HWND;
    FIsSpanned: Boolean;
    fMessage: string;
    fVerbose: Boolean;
    fTrace: Boolean;
    fZipContents: TList;
    fExtrBaseDir: string;
    fZipBusy: Boolean;
    fUnzBusy: Boolean;
    FExtrOptions: ExtrOpts;
    FFSpecArgs: TStrings;
    FZipFileName: string;
    FSuccessCnt: Integer;
    FPassword: string;
    FEncrypt: Boolean;
    FSFXOffset: Integer;
    FDLLDirectory: string;
    FUnattended: Boolean;
    FEventErr: string;
    FSpanOptions: SpanOpts;
    FBusy: boolean;
    FVer: integer;

    AutoExeViaAdd: Boolean;
    FVolumeName: string;
    FSizeOfDisk: LargeInt;    { Int64 or Comp }
    FDiskFree: LargeInt;
    FFreeOnDisk: LargeInt;
    //1.72        FDiskSerial: Integer;
    FDrive: string;
    FDriveFixed: Boolean;
    FHowToDelete: DeleteOpts;
    FTotalSizeToProcess: Cardinal;
    FTotalPosition: Cardinal; // new 1.73
    FItemSizeToProcess: Cardinal; // new 1.73
    FItemPosition: Cardinal;  // new 1.73
    FItemName: string;        // new 1.73
    FStoredExtraData: string; // new 1.73
    fIsDestructing: boolean;  // new 1.73

    FDiskNr: Integer;
    FTotalDisks: Integer;
    FFileSize: Integer;
    FRealFileSize: Cardinal;
    FWrongZipStruct: Boolean;
    FInFileName: string;
    FInFileHandle: Integer;
    FOutFileHandle: Integer;
    FVersionMadeBy1: Integer;
    FVersionMadeBy0: Integer;
    FDateStamp: Integer; { DOS formatted date/time - use Delphi's
    FileDateToDateTime function to give you TDateTime format.}
    fFromDate: TDate;
    FTempDir: string;
    FShowProgress: Boolean;
    FFreeOnDisk1: Integer;
    FFreeOnAllDisks: cardinal; // new 1.72
    FMaxVolumeSize: Integer;
    FMinFreeVolSize: Integer;
    FCodePage: CodePageOpts;
    FZipEOC: Integer;         // End-Of-Central-Dir location
    FZipSOC: Integer;         // Start-Of-Central-Dir location
    FZipComment: string;
    FVersionInfo: string;
    FZipStream: TZipStream;
    FPasswordReqCount: LongWord;
    GAssignPassword: Boolean;
    GModalResult: TModalResult;
    FFSpecArgsExcl: TStrings;
    FUseDirOnlyEntries: Boolean;
    FRootDir: string;
    FCurWaitCount: Integer;
    FSaveCursor: TCursor;
    // Dll related variables
    fZipDll: TZipDll;         // new 1.73
    fUnzDll: TUnzDll;         // new 1.73

    ZipParms: pZipParms;      { declare an instance of ZipParms 1 or 2 }
    UnZipParms: pUnZipParms;  { declare an instance of UnZipParms 2 }

    { Event variables }
    FOnDirUpdate: TNotifyEvent;
    FOnProgress: TProgressEvent;
    FOnMessage: TMessageEvent;
    FOnSetNewName: TSetNewNameEvent;
    FOnNewName: TNewNameEvent;
    FOnPasswordError: TPasswordErrorEvent;
    FOnCRC32Error: TCRC32ErrorEvent;
    FOnExtractOverwrite: TExtractOverwriteEvent;
    FOnExtractSkipped: TExtractSkippedEvent;
    FOnCopyZipOverwrite: TCopyZipOverwriteEvent;
    FOnFileComment: TFileCommentEvent;
    FOnAfterCallback: TAfterCallbackEvent;
    FOnTick: TTickEvent;
    FOnFileExtra: TFileExtraEvent;
    FOnTotalProgress: TTotalProgressEvent; // new 1.73
    FOnItemProgress: TItemProgressEvent; // new 1.73
    FOnZipStr: TZipStrEvent;  // new 1.73

{$IFNDEF NO_SPAN}
    fConfirmErase: Boolean;
    FDiskWritten: Integer;
    FDriveNr: Integer;
    // 1.72		FFormatErase: Boolean;          // New 1.70
    FInteger: Integer;
    FNewDisk: Boolean;
    FOnGetNextDisk: TGetNextDiskEvent;
    FOnStatusDisk: TStatusDiskEvent;
    FOutFileName: string;
    FZipDiskAction: TZipDiskAction;
    FZipDiskStatus: TZipDiskStatus;
{$ENDIF}
    FSFX: TCustomZipSFX;      // 1.72y
{$IFDEF INTERNAL_SFX}
    FAutoSFXSlave: TCustomZipSFX; // 1.72x
    FSFXCaption: string;      // dflt='Self-extracting Archive'
    FSFXCommandLine: string;  // dflt=''
    FSFXDefaultDir: string;   // dflt=''
    FSFXIcon: TIcon;
    FSFXMessage: string;
    FSFXOptions: SFXOpts;
    FSFXOverWriteMode: OvrOpts; // ovrConfirm  (others: ovrAlways, ovrNever)
    FSFXPath: string;
{$ENDIF}

    { Property get/set functions }
    function GetMessage: string; // new 1.73
    function GetCount: Integer;
    procedure SetFSpecArgs(Value: TStrings);
    procedure SetFileName(Value: string);
    function GetZipVers: Integer;
    function GetUnzVers: Integer;
    procedure SetDLLDirectory(Value: string);
    procedure SetVersionInfo(Value: string);
    function GetZipComment: string;
    procedure SetZipComment(zComment: string);
    procedure SetPasswordReqCount(Value: LongWord);
    procedure SetFSpecArgsExcl(Value: TStrings);
    procedure SetExtAddStoreSuffixes(Value: string); // new 1.71
    procedure NoWrite(Value: integer);

    { Private "helper" functions }
    function IsFixedDrive(drv: string): boolean; // new 1.72
    function GetDirEntry(idx: integer): pZipDirEntry; // changed 1.73
    procedure FreeZipDirEntryRecords;
    procedure SetZipSwitches(var NameOfZipFile: string; zpVersion: Integer);
    procedure SetUnZipSwitches(var NameOfZipFile: string; uzpVersion: Integer);
    //    function ConvCodePage(Source: string; Direction: CodePageDirection): string;
    function ConvertOEM(const Source: string; Direction: CodePageDirection): string;
    function GetDriveProps: Boolean;
    function OpenEOC(var EOC: ZipEndOfCentral; DoExcept: Boolean): boolean;
    //		function ReplaceForwardSlash(aStr: string): string;
    function CopyBuffer(InFile, OutFile, ReadLen: Integer): Integer;
    procedure WriteJoin(Buffer: pChar; BufferSize, DSErrIdent: Integer);
    procedure ReadJoin(var Buffer; BufferSize, DSErrIdent: Integer); // new 1.73
    procedure GetNewDisk(DiskSeq: Integer);
    function ZipStr(id: integer): string; // new 1.73

    procedure DiskFreeAndSize(Action: Integer);
    procedure AddSuffix(const SufOption: AddStoreSuffixEnum; var sStr: string;
      sPos: Integer);
    procedure ExtExtract(UseStream: Integer; MemStream: TMemoryStream);
    procedure ExtAdd(UseStream: Integer; StrFileDate, StrFileAttr: DWORD; MemStream: TMemoryStream);
    procedure SetDeleteSwitches;
    procedure StartWaitCursor;
    procedure StopWaitCursor;
    procedure TraceMessage(Msg: string);
    procedure CreateMVFileName(var FileName: string; StripPartNbr: Boolean); //new in 1.72

{$IFNDEF NO_SPAN}
    procedure CheckForDisk(writing: bool); // 1.70/2 changed
    procedure ClearFloppy(dir: string); // New 1.70
    function IsRightDisk {(drt: Integer)}: Boolean; // 1.72 Changed
    function MakeString(Buffer: pChar; Size: Integer): string;
    procedure RWJoinData(Buffer: pChar; ReadLen, DSErrIdent: Integer);
    procedure RWSplitData(Buffer: pChar; ReadLen, ZSErrVal: Integer);
    procedure WriteSplit(Buffer: pChar; Len: Integer; MinSize: Integer);
    function ZipFormat: Integer; // New 1.70
    function GetLastVolume(FileName: string; var EOC: ZipEndOfCentral; AllowNotExists: Boolean): integer; //173
{$ENDIF}
{$IFNDEF NO_SFX}
    function GetSFXSlave: TCustomZipSFX; // new 1.73
{$IFDEF INTERNAL_SFX}
    procedure SetSFXIcon(aIcon: TIcon); // rest added 1.72.0.4
    function GetSFXIcon: TIcon;
    function GetSFXCaption: string;
    procedure SetSFXCaption(aString: string);
    function GetSFXCommandLine: string;
    procedure SetSFXCommandLine(aString: string);
    function GetSFXDefaultDir: string;
    procedure SetSFXDefaultDir(aString: string);
    function GetSFXMessage: string;
    procedure SetSFXMessage(aString: string);
    function GetSFXOptions: SfxOpts;
    procedure SetSFXOptions(aOpts: SfxOpts);
    function GetSFXOverWriteMode: OvrOpts;
    procedure SetSFXOverWriteMode(aOpts: OvrOpts);
    function GetSFXPath: string;
    procedure SetSFXPath(aString: string);
{$ENDIF}
{$ENDIF}
  protected
    function CallBack(ActionCode: ActionCodes; ErrorCode: integer; Msg: string;
      FileSize: cardinal): integer;
    function LoadZipStr(Ident: Integer; DefaultStr: string): string;

    procedure _List;          // 1.72 the original
    procedure _Delete;        // 1.72 original

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFDEF VERD4+}
    procedure BeforeDestruction; override;
{$ENDIF}

    { Public Properties (run-time only) }
    property Handle: HWND read fHandle write fHandle;
    property ErrCode: Integer read fErrCode write fErrCode;
    property Message: string read GetMessage; // write fMessage;

    property ZipContents: TList read FZipContents;
    property Cancel: Boolean read fCancel write fCancel;
    property ZipBusy: Boolean read fZipBusy;
    property UnzBusy: Boolean read fUnzBusy;

    property Count: Integer read GetCount;
    property SuccessCnt: Integer read FSuccessCnt;

    property ZipVers: Integer read GetZipVers;
    property UnzVers: Integer read GetUnzVers;
    property Ver: integer read fVer write NoWrite;

    property SFXOffset: Integer read FSFXOffset;
    property ZipSOC: Integer read FZipSOC default 0;
    property ZipEOC: Integer read FZipEOC default 0;
    property IsSpanned: Boolean read FIsSpanned default False;
    property ZipFileSize: Cardinal read FRealFileSize default 0;
    property FullErrCode: Integer read FFullErrCode;
    property TotalSizeToProcess: Cardinal read FTotalSizeToProcess;

    property ZipComment: string read GetZipComment write SetZipComment;
    property ZipStream: TZipStream read FZipStream;
    property DirOnlyCount: Integer read FDirOnlyCount default 0;

    { Public Methods }
{ NOTE: Test is an sub-option of extract }
    function Add: integer;    // changed 1.72
    function Delete: integer; // changed 1.72
    function Extract: integer; // changed 1.72
    function List: integer;   // changed 1.72
    // load dll - return version
    function Load_Zip_Dll: integer;
    function Load_Unz_Dll: integer;
    procedure Unload_Zip_Dll;
    procedure Unload_Unz_Dll;
    function ZipDllPath: string; // new 1.72
    function UnzDllPath: string; // new 1.72
    procedure AbortDlls;
    function Busy: boolean;   // new 1.72
    function CopyFile(const InFileName, OutFileName: string): Integer;
    function EraseFile(const Fname: string; How: DeleteOpts): Integer;
    function GetAddPassword: string;
    function GetExtrPassword: string;
    function AppendSlash(sDir: string): string;
    { New in v1.6 }
    function Rename(RenameList: TList; DateTime: Integer): Integer;
    function ExtractFileToStream(Filename: string): TZipStream;
    function AddStreamToStream(InStream: TMemoryStream): TZipStream;
    (*{$IFDEF xVERD4+}
      function ExtractStreamToStream(InStream: TMemoryStream; OutSize: LongWord =
       32768): TZipStream;
      // 1.72 changed
      function AddStreamToFile(Filename: string = ''; FileDate: DWord = 0;
       FileAttr: DWord = 0): integer;
      function MakeTempFileName(Prefix: string = 'zip'; Extension: string =
       '.zip'): string;
      procedure ShowZipMessage(Ident: Integer; UserStr: string = '');
    { $ELSE}   *)
    function AddStreamToFile(Filename: string; FileDate, FileAttr: Dword): integer;
    function ExtractStreamToStream(InStream: TMemoryStream; OutSize: Longword):
      TZipStream;
    function MakeTempFileName(Prefix, Extension: string): string;
    procedure ShowZipMessage(Ident: Integer; UserStr: string);
    //{$ENDIF}
    procedure ShowExceptionError(const ZMExcept: EZipMaster); //1.72 promoted
    function GetPassword(DialogCaption, MsgTxt: string; pwb: TPasswordButtons;
      var ResultStr: string): TPasswordButton;
    function CopyZippedFiles(DestZipMaster: TZipMaster; DeleteFromSource:
      boolean; OverwriteDest: OvrOpts): Integer;

    property DirEntry[idx: integer]: pZipDirEntry read GetDirEntry; // changed 1.73
    function FullVersionString: string; // New 1.70
{$IFNDEF NO_SPAN}
    function ReadSpan(InFileName: string; var OutFilePath: string): Integer;
    function WriteSpan(InFileName, OutFileName: string): Integer;
{$ENDIF}
{$IFNDEF NO_SFX}
    function NewSFXFile(const ExeName: string): Integer;
    function ConvertSFX: Integer;
    function ConvertZIP: Integer;
    function IsZipSFX(const SFXExeName: string): Integer;
{$ENDIF}

  published
    { Public properties that also show on Object Inspector }
    property Verbose: Boolean read FVerbose
      write FVerbose;
    property Trace: Boolean read FTrace
      write FTrace;
    property AddCompLevel: Integer read FAddCompLevel
      write FAddCompLevel;
    property AddOptions: AddOpts read FAddOptions
      write fAddOptions;
    property AddFrom: TDate read fFromDate write fFromDate;
    property ExtrBaseDir: string read FExtrBaseDir
      write FExtrBaseDir;
    property ExtrOptions: ExtrOpts read FExtrOptions
      write FExtrOptions;
    property FSpecArgs: TStrings read FFSpecArgs
      write SetFSpecArgs;
    property Unattended: Boolean read FUnattended
      write FUnattended;
    { At runtime: every time the filename is assigned a value,
the ZipDir will automatically be read. }
    property ZipFileName: string read FZipFileName
      write SetFileName;
    property Password: string read FPassword
      write FPassword;
    property DLLDirectory: string read FDLLDirectory
      write SetDLLDirectory;
    property TempDir: string read FTempDir
      write FTempDir;
    property CodePage: CodePageOpts read FCodePage
      write FCodePage default cpAuto;
    property HowToDelete: DeleteOpts read FHowToDelete
      write FHowToDelete default htdAllowUndo;
    property VersionInfo: string read FVersionInfo
      write SetVersionInfo;
    property AddStoreSuffixes: AddStoreExts read FAddStoreSuffixes
      write FAddStoreSuffixes;
    property ExtAddStoreSuffixes: string read fExtAddStoreSuffixes
      write SetExtAddStoreSuffixes; // new 1.71
    property PasswordReqCount: LongWord read FPasswordReqCount
      write SetPasswordReqCount default 1;
    property FSpecArgsExcl: TStrings read FFSpecArgsExcl
      write SetFSpecArgsExcl;
    property UseDirOnlyEntries: Boolean read FUseDirOnlyEntries
      write FUseDirOnlyEntries default False;
    property RootDir: string read FRootDir
      write fRootDir;
    { Events }
    property OnDirUpdate: TNotifyEvent read FOnDirUpdate
      write FOnDirUpdate;
    property OnTotalProgress: TTotalProgressEvent
      read FOnTotalProgress write FOnTotalProgress; // new 1.73
    property OnItemProgress: TItemProgressEvent
      read FOnItemProgress write FOnItemProgress; // new 1.73
    property OnProgress: TProgressEvent read FOnProgress
      write FOnProgress;
    property OnMessage: TMessageEvent read fOnMessage write fOnMessage;
    property OnSetNewName: TSetNewNameEvent read FOnSetNewName
      write FOnSetNewName;
    property OnNewName: TNewNameEvent read FOnNewName
      write FOnNewName;
    property OnCRC32Error: TCRC32ErrorEvent read FOnCRC32Error
      write FOnCRC32Error;
    property OnPasswordError: TPasswordErrorEvent read FOnPasswordError
      write FOnPasswordError;
    property OnExtractOverwrite: TExtractOverwriteEvent read FOnExtractOverwrite
      write FOnExtractOverwrite;
    property OnExtractSkipped: TExtractSkippedEvent read FOnExtractSkipped
      write FOnExtractSkipped;
    property OnCopyZipOverwrite: TCopyZipOverwriteEvent read FOnCopyZipOverwrite
      write FOnCopyZipOverwrite;
    property OnFileComment: TFileCommentEvent read FOnFileComment
      write FOnFileComment;
    property OnAfterCallback: TAfterCallbackEvent read fOnAfterCallback
      write fOnAfterCallback; // new 1.72
    property OnTick: TTickEvent read FOnTick
      write FOnTick;
    property OnFileExtra: TFileExtraEvent read FOnFileExtra
      write FOnFileExtra;     // new 1.72
    property OnZipStr: TZipStrEvent read FOnZipStr write FOnZipStr; // new 1.73
{$IFNDEF NO_SPAN}
    property SpanOptions: SpanOpts read FSpanOptions write FSpanOptions;
    property ConfirmErase: Boolean read fConfirmErase write fConfirmErase default
      True;
    // 1.72       property FormatErase: Boolean read FFormatErase write FFormatErase default False;
    property KeepFreeOnDisk1: Integer read FFreeOnDisk1 write FFreeOnDisk1;
    property KeepFreeOnAllDisks: Cardinal read FFreeOnAllDisks write
      FFreeOnAllDisks;        // new 1.72
    property MaxVolumeSize: Integer read FMaxVolumeSize write FMaxVolumesize
      default 0;
    property MinFreeVolumeSize: Integer read FMinFreeVolSize write
      FMinFreeVolSize default 65536;
    property OnGetNextDisk: TGetNextDiskEvent read FOnGetNextDisk write
      FOnGetNextDisk;
    property OnStatusDisk: TStatusDiskEvent read FOnStatusDisk write
      FOnStatusDisk;
{$ENDIF}
{$IFNDEF NO_SFX}
    property SFXSlave: TCustomZipSFX read fSFX write fSFX;
{$IFDEF INTERNAL_SFX}
    property SFXCaption: string read GetSFXCaption write SetSFXCaption;
    property SFXCommandLine: string read GetSFXCommandLine write
      SetSFXCommandLine;
    property SFXDefaultDir: string read GetSFXDefaultDir write SetSFXDefaultDir;
    property SFXIcon: TIcon read GetSFXIcon write SetSFXIcon;
    property SFXMessage: string read GetSFXMessage write SetSFXMessage;
    property SFXOptions: SfxOpts read GetSFXOptions write SetSFXOptions default
      [SFXCheckSize];
    property SFXOverWriteMode: OvrOpts read GetSFXOverWriteMode write
      SetSFXOverWriteMode;
    property SFXPath: string read GetSFXPath write SetSFXPath;
{$ENDIF}
{$ENDIF}
  end;

{$IFDEF DEBUGCALLBACK}
type
  CallBackInfo = class
    zip: TZipMaster;
    code, error: integer;
    msg: string;
    size: cardinal;
  end;
{$ENDIF}
  //	add extra to path ensuring single backslash
function PathConcat(path, extra: string): string;
function PatternMatch(const pat: string; const str: string): boolean;
function ForceDirectory(const Dir: string): Boolean;
function SetSlash(const path: string; forwardSlash: boolean): string;
function DelimitPath(const path: string; sep: boolean): string;
function DirExists(const Fname: string): boolean;
function QueryZip(const Fname: string): integer; // new 1.73

procedure Register;

implementation

{$IFNDEF NO_SFX}
uses
  SFXInterface;
{$ENDIF}
{$R ZipMstr.Res}

const                         { these are stored in reverse order }
  LocalFileHeaderSig = $04034B50; { 'PK'34  (in file: 504b0304) }
  CentralFileHeaderSig = $02014B50; { 'PK'12 }
  EndCentralDirSig = $06054B50; { 'PK'56 }
  ExtLocalSig = $08074B50;    { 'PK'78 }
  EndCentral64Sig = $06064B50; { 'PK'66 }
  EOC64LocatorSig = $07064B50; { 'PK'67 }
  BufSize   = 10240;          //8192;                     // Keep under 12K to avoid Winsock problems on Win95.
  // If chunks are too large, the Winsock stack can
  // lose bytes being sent or received.
  FlopBufSize = 65536;
  RESOURCE_ERROR: string =
    'ZipMsgXX.res is probably not linked to the executable' + #10 +
    'Missing String ID is: ';

  ZIPVERSION = 173;
  UNZIPVERSION = 173;

  MAX_PERCENT = MAXINT div 10000; // new 1.73

  //type
  //	TBuffer = array[0..BufSize - 1] of Byte;
  //	pBuffer = ^TBuffer;
    { Define the functions that are not part of the TZipMaster class. }
    { The callback function must NOT be a member of a class. }
  { We use the same callback function for ZIP and UNZIP. }

function ZCallback(ZCallBackRec: pZCallBackStruct): LongBool; stdcall; forward;

type
  CallBackData = record
    case boolean of
      false: (FilenameOrMessage: array[0..511] of byte);
      true: (FileName: array[0..503] of byte;
        Data: pByteArray);
  end;

{$IFNDEF NO_SFX}
type
  TFriendSFX = class(TCustomZipSFX)
  end;
{$ENDIF}
type
  TPasswordDlg = class(TForm)
  private
    PwBtn: array[0..3] of TBitBtn;
    PwdEdit: TEdit;
    PwdTxt: TLabel;

  public
    constructor CreateNew2(Owner: TComponent; pwb: TPasswordButtons); virtual;
    destructor Destroy; override;

    function ShowModalPwdDlg(DlgCaption, MsgTxt: string): string; virtual;
  end;

type
  MDZipData = record          // MyDirZipData
    Diskstart: Word;          // The disk number where this file begins
    RelOffLocal: LongWord;    // offset from the start of the first disk
    FileNameLen: Word;        // length of current filename
    FileName: array[0..254] of Char; // Array of current filename
    CRC32: LongWord;
    ComprSize: LongWord;
    UnComprSize: LongWord;
    DateTime: Integer;
  end;
  pMZipData = ^MDZipData;

  TMZipDataList = class(TList)
  private
    function GetItems(Index: integer): pMZipData;
  public
    constructor Create(TotalEntries: integer);
    destructor Destroy; override;
    property Items[Index: integer]: pMZipData read GetItems;
    function IndexOf(fname: string): integer;
  end;

  // ==================================================================
{$IFDEF VER90}                // if Delphi 2

function AnsiStrPos(S1, S2: pChar): pChar;
begin
  Result := StrPos(S1, S2);   // not will not work with MBCS
end;

function AnsiStrIComp(S1, S2: pChar): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, -1, S2, -1)
    - 2;
end;

function AnsiPos(const Substr, S: string): Integer;
begin
  Result := Pos(Substr, S);
end;
{$ENDIF}
// -------------------------- ------------ -------------------------

{ Implementation of TZipMaster class member functions }
// ================== changed or New functions =============================

(*? TZipMaster.GetLastVolume
1.73.2.7 13 September 2003 RP avoid neg part numbers
1.73  9 July 2003 RA creation of first part name corrected
1.73 28 June 2003 new fuction
*)
{$IFNDEF NO_SPAN}

function TZipMaster.GetLastVolume(FileName: string; var EOC: ZipEndOfCentral; AllowNotExists: Boolean): integer;
var
  Path, Fname, Ext, sName: string;
  PartNbr   : integer;
  Abort, FMVolume: boolean;
  function NameOfPart(fn: string; compat: boolean): string;
  var
    r, n    : integer;
    sRec    : TSearchRec;
    fs      : string;
  begin
    Result := '';
    if compat then
      fs := fn + '.z??'
    else
      fs := fn + '???.zip';
    r := FindFirst(fs, faAnyFile, SRec);
    while r = 0 do
    begin
      if compat then
      begin
        fs := UpperCase(copy(SRec.Name, Length(SRec.Name) - 1, 2));
        if fs = 'IP' then
          n := 99
        else
          n := StrToIntDef(fs, 0);
      end
      else
        n := StrToIntDef(copy(SRec.Name, Length(SRec.Name) - 6, 3), 0);
      if n > 0 then
      begin
        Result := SRec.Name;  // possible name
        break;
      end;
      r := FindNext(SRec);
    end;
    FindClose(SRec);
  end;
begin
  PartNbr := -1;
  FInFileHandle := -1;
  FMVolume := false;
  FDrive := ExtractFileDrive(ExpandFileName(FileName)) + '\';
  Path := ExtractFilePath(FileName);
  Ext := Uppercase(ExtractFileExt(FileName));
  try
    FDriveFixed := IsFixedDrive(FDrive);
    GetDriveProps;            // check valid drive
    if not FileExists(FileName) then
    begin
      Fname := copy(FileName, 1, Length(FileName) - Length(Ext)); // remove extension
      FMVolume := true;       // file did not exist maybe it is a multi volume
      if FDriveFixed then     // if file not exists on harddisk then only Multi volume parts are possible
      begin                   // filename is of type ArchiveXXX.zip
        // MV files are series with consecutive partnbrs in filename, highest number has EOC
        while FileExists(Fname + copy(IntToStr(1002 + PartNbr), 2, 3) + '.zip') do
          inc(PartNbr);
        if PartNbr = -1 then
        begin
          if not AllowNotExists then
            raise EZipMaster.CreateResDisp(DS_FileOpen, true);
          Result := 1;
          exit;               // non found
        end;
        FileName := Fname + copy(IntToStr(1001 + PartNbr), 2, 3) + '.zip';
        // check if filename.z01 exists then it is part of MV with compat names and cannot be used
        if (FileExists(ChangeFileExt(FileName, '.z01'))) then
          raise EZipMaster.CreateResDisp(DS_FileOpen, true); // cannot be used
      end
      else                    // if we have an MV archive copied to a removable disk
      begin
        // accept any MV filename on disk
        sName := NameOfPart(Fname, false);
        if sName = '' then
          sName := NameOfPart(Fname, true);
        if sName = '' then    // none
          raise EZipMaster.CreateResDisp(DS_FileOpen, true);
        FileName := Path + sName;
      end;
    end;                      // if not exists
    // zip file exists or we got an acceptable part in multivolume or spanned archive
    FInFileName := FileName;  // use class variable for other functions
    while not OpenEOC(EOC, false) do // does this part contains the central dir
    begin                     // it is not the disk with central dir so ask for the last disk
      if FInFileHandle <> -1 then
      begin
        FileClose(FInFileHandle); //each check does FileOpen
        FInFileHandle := -1;  // avoid further closing attempts
      end;
      CheckForDisk(false);    // does the request for new disk
      if FDriveFixed then
      begin
        if FMVolume then
          raise EZipMaster.CreateResDisp(DS_FileOpen, true); // it was not a valable part
        AllowNotExists := false; // next error needs to be displayed always
        raise EZipMaster.CreateResDisp(DS_NoValidZip, true); // file with EOC is not on fixed disk
      end;
      // for spanned archives on cdrom's or floppies
      if assigned(FOnGetNextDisk) then
      begin                   // v1.60L
        Abort := false;
        OnGetNextDisk(self, 0, 0, copy(FDrive, 1, 1), Abort);
        if Abort then         // we allow abort by the user
        begin
          if FInFileHandle <> -1 then
            FileClose(FInFileHandle);
          Result := 1;
          exit;
        end;
        GetDriveProps;        // check drive spec and get volume name
      end
      else
      begin                   // if no event handler is used
        FNewDisk := true;
        FDiskNr := -1;        // read operation
        CheckForDisk(false);  // ask for new disk
      end;
      if FMVolume then
      begin                   // we have removable disks with multi volume archives
        //  get the file name on this disk
        sName := NameOfPart(Fname, spCompatName in FSpanOptions);
        if sName = '' then
        begin                 // no acceptable file on this disk so not a disk of the set
          ShowZipMessage(DS_FileOpen, '');
          Result := -1;       //error
          exit;
        end;
        FInFileName := Path + sName;
      end;
    end;                      // while
    if FMVolume then          // got a multi volume part so we need more checks
    begin
      // is this first file of a spanned
      if (not FIsSpanned) and ((EOC.ThisDiskNo = 0) and (PartNbr > 0 {<> -1})) then
        raise EZipMaster.CreateResDisp(DS_FileOpen, true);
      // part and EOC equal?
      if FDriveFixed and (EOC.ThisDiskNo <> PartNbr) then
        raise EZipMaster.CreateResDisp(DS_NoValidZip, true);
    end;

	except
    on E: EZipMaster do
    begin
      if not AllowNotExists then
        ShowExceptionError(E);
      FInFileName := '';      // don't use the file
      if FInFileHandle <> -1 then
        FileClose(FInFileHandle); //close filehandle if open
      Result := -1;
      exit;
    end;
  end;
	Result := 0;
end;
{$ENDIF}
//? TZipMaster.GetLastVolume

//---------------------------------------------------------------------------
(*? TZipMaster.OpenEOC
1.73.2.7 12 September 2003 RP stop Disk number < 0
1.73 13 July 2003 RP changed find EOC
// Function to find the EOC record at the end of the archive (on the last disk.)
// We can get a return value( true::Found, false::Not Found ) or an exception if not found.
1.73 28 June 2003 RP change handling split files
*)

function TZipMaster.OpenEOC(var EOC: ZipEndOfCentral; DoExcept: boolean):
  boolean;
var
  Sig       : Cardinal;
  DiskNo, Size, i, j: Integer;
  ShowGarbageMsg: Boolean;
  First     : Boolean;
  ZipBuf    : pChar;
  ext       : string;
begin
  FZipComment := '';
  First := False;
  ZipBuf := nil;
  FZipEOC := 0;

  // Open the input archive, presumably the last disk.
  FInFileHandle := FileOpen(FInFileName, fmShareDenyWrite or fmOpenRead);
  if FInFileHandle = -1 then
  begin
    if DoExcept = True then
      raise EZipMaster.CreateResDisp(DS_NoInFile, True);
    ShowZipMessage(DS_FileOpen, '');
    Result := False;
    Exit;
  end;

  // First a check for the first disk of a spanned archive,
  // could also be the last so we don't issue a warning yet.
  if (FileRead(FInFileHandle, Sig, 4) = 4) and (Sig = ExtLocalSig) and
    (FileRead(FInFileHandle, Sig, 4) = 4) and (Sig = LocalFileHeaderSig) then
  begin
    First := True;
    FIsSpanned := True;
  end;

  // Next we do a check at the end of the file to speed things up if
 // there isn't a Zip archive comment.
  FFileSize := FileSeek(FInFileHandle, -SizeOf(EOC), 2);
  if FFileSize <> -1 then
  begin
    Inc(FFileSize, SizeOf(EOC)); // Save the archive size as a side effect.
    FRealFileSize := FFileSize; // There could follow a correction on FFileSize.
    if (FileRead(FInFileHandle, EOC, SizeOf(EOC)) = SizeOf(EOC)) and
      (EOC.HeaderSig = EndCentralDirSig) then
    begin
      FZipEOC := FFileSize - SizeOf(EOC);
      Result := True;
      Exit;
    end;
  end;

  // Now we try to find the EOC record within the last 65535 + sizeof( EOC ) bytes
  // of this file because we don't know the Zip archive comment length at this time.
  try
    Size := 65535 + SizeOf(EOC);
    if FFileSize < Size then
      Size := FFileSize;
    GetMem(ZipBuf, Size + 1);
    if FileSeek(FInFileHandle, -Size, 2) = -1 then
      raise EZipMaster.CreateResDisp(DS_FailedSeek, True);
    ReadJoin(ZipBuf^, Size, DS_EOCBadRead);
    for i := Size - SizeOf(EOC) - 1 downto 0 do
      if pZipEndOfCentral(ZipBuf + i)^.HeaderSig = EndCentralDirSig then
      begin
        FZipEOC := FFileSize - Size + i;
        Move(ZipBuf[i], EOC, SizeOf(EOC)); // Copy from our buffer to the EOC record.
        // Check if we really are at the end of the file, if not correct the filesize
            // and give a warning. (It should be an error but we are nice.)
        if not (i + SizeOf(EOC) + EOC.ZipCommentLen - Size = 0) then
        begin
          Inc(FFileSize, i + SizeOf(EOC) + Integer(EOC.ZipCommentLen) - Size);
          // Now we need a check for WinZip Self Extractor which makes SFX files which
               // allmost always have garbage at the end (Zero filled at 512 byte boundary!)
               // In this special case 'we' don't give a warning.
          ShowGarbageMsg := True;
          if (FRealFileSize - Cardinal(FFileSize) < 512) and ((FRealFileSize mod
            512) = 0) then
          begin
            j := i + SizeOf(EOC) + EOC.ZipCommentLen;
            while (ZipBuf[j] = #0) and (j <= Size) do
              Inc(j);
            if j = Size + 1 then
              ShowGarbageMsg := False;
          end;
          if ShowGarbageMsg then
            ShowZipMessage(LI_GarbageAtEOF, '');
        end;
        // If we have ZipComment: Save it, must be after Garbage check because a #0 is set!
        if not (EOC.ZipCommentLen = 0) then
        begin
          ZipBuf[i + SizeOf(EOC) + EOC.ZipCommentLen] := #0;
          FZipComment := ZipBuf + i + SizeOf(EOC); // No codepage translation yet, wait for CEH read.
        end;
        FreeMem(ZipBuf);
        Result := True;
        Exit;
      end;
    FreeMem(ZipBuf);
  except
    FreeMem(ZipBuf);
    if DoExcept = True then
      raise;
  end;
  if FInFileHandle <> -1 then // don't leave open
    FileClose(FInFileHandle);
  FInFileHandle := -1;
  if DoExcept = True then
  begin
    // Get the volume number if it's disk from a set. - 1.72 moved
    DiskNo := 0;
    if Pos('PKBACK# ', FVolumeName) = 1 then
      DiskNo := StrToIntDef(Copy(FVolumeName, 9, 3), 0);
    //    else
    if DiskNo <= 0 then
    begin
      ext := UpperCase(ExtractFileExt(FInFileName));
      DiskNo := 0;
      if copy(ext, 1, 2) = '.Z' then
        DiskNo := StrToIntDef(copy(ext, 2, 3), 0);
      if (DiskNo <= 0) then
        DiskNo := StrToIntDef(Copy(FInFileName, length(FInFileName)
          - length(ext) - 3 + 1, 3), 0);
    end;
    if (not First) and (DiskNo {<} > 0) then
      raise EZipMaster.CreateResDisk(DS_NotLastInSet, DiskNo);
    if First = True then
      if DiskNo = 1 then
        raise EZipMaster.CreateResDisp(DS_FirstInSet, True)
      else
        raise EZipMaster.CreateResDisp(DS_FirstFileOnHD, True)
    else
      raise EZipMaster.CreateResDisp(DS_NoValidZip, True);
  end;
  Result := False;
end;
//? TZipMaster.OpenEOC

(*? TZipMaster.WriteSplit ---------------------------------------------------
1.73.2.7 12 September 2003 RP stoppped disk number<0
1.73 11 July 2003 RP corrected asking disk status
1.73 9 July 2003 RA corrected use of disk status and OnStatusDisk
1.73 7 July 2003 RA changed OnMessage and OnProgress to Callback calls
1.73 28 June 2003 changed Split file handling
// This function actually writes the zipped file to the destination while
// taking care of disk changes and disk boundary crossings.
// In case of an write error, or user abort, an exception is raised.
*)
{$IFNDEF NO_SPAN}

procedure TZipMaster.WriteSplit(Buffer: pChar; Len: Integer; MinSize: Integer);
var
  Res, MaxLen: Integer;
  Buf       : pChar;          // Used if Buffer doesn't fit on the present disk.
  DiskSeq   : Integer;
  DiskFile, MsgQ: string;
begin
  Buf := Buffer;
  if ForegroundTask then
    Application.ProcessMessages;
  if Cancel then
    raise EZipMaster.CreateResDisp(DS_Canceled, False);

  while True do               // Keep writing until error or buffer is empty.
  begin
    // Check if we have an output file already opened, if not: create one,
      // do checks, gather info.
    if FOutFileHandle = -1 then
    begin
      FDriveFixed := IsFixedDrive(FDrive); // 1.72
      CheckForDisk(true);     // 1.70 changed
      DiskFile := FOutFileName;

      // If we write on a fixed disk the filename must change.
      // We will get something like: FileNamexxx.zip where xxx is 001,002 etc.
   // if CompatNames are used we get FileName.zxx where xx is 01, 02 etc.. last .zip
      if FDriveFixed or (spNoVolumeName in FSpanOptions) then
        CreateMVFileName(DiskFile, false);

      // Allow clearing of removeable media even if no volume names
      if (not FDriveFixed) and (spWipeFiles in SpanOptions)
        {(AddDiskSpanErase In FAddOptions)}then
      begin
        if (not Assigned(FOnGetNextDisk))
          or (Assigned(FOnGetNextDisk)
          and (FZipDiskAction = zdaErase)) then // Added v1.60L
        begin
          // Do we want a format first?
          FDriveNr := Ord(UpperCase(FDrive)[1]) - Ord('A');
          if (spNoVolumeName in SpanOptions) then
            FVolumeName := 'ZipSet_' + IntToStr(succ(FDiskNr)) // default name
          else
            FVolumeName := 'PKBACK# ' + Copy(IntToStr(1001 + FDiskNr), 2, 3);
          // Ok=6 NoFormat=-3, Cancel=-2, Error=-1
          case ZipFormat of   // Start formating and wait until finished...
            -1: raise EZipMaster.CreateResDisp(DS_Canceled, True);
            -2: raise EZipMaster.CreateResDisp(DS_Canceled, False);
          end;
        end;
      end;
      if FDriveFixed or (spNoVolumeName in SpanOptions) then
        DiskSeq := FDiskNr + 1
      else
      begin
        DiskSeq := StrToIntDef(Copy(FVolumeName, 9, 3), 1);
        if DiskSeq < 0 then
          DiskSeq := 1;
      end;
      FZipDiskStatus := [];   // v1.60L
      // Do we want to overwrite an existing file?
      if FileExists(DiskFile) then
      begin
        if Unattended then
          raise EZipMaster.CreateResDisp(DS_NoUnattSpan, True); // we assume we don't.

        // A more specific check if we have a previous disk from this set.
        if (FileAge(DiskFile) = FDateStamp) and (Pred(DiskSeq) < FDiskNr) then
        begin
          MsgQ := Format(LoadZipStr(DS_AskPrevFile,
            'Overwrite previous disk no %d'), [DiskSeq]);
          FZipDiskStatus := FZipDiskStatus + [zdsPreviousDisk]; // v1.60L
        end
        else
        begin
          MsgQ := Format(LoadZipStr(DS_AskDeleteFile,
            'Overwrite previous file %s'), [DiskFile]);
          FZipDiskStatus := FZipDiskStatus + [zdsSameFileName]; // v1.60L
        end;
      end
      else
      begin
        if not FDriveFixed then
        begin
          if FSizeOfDisk - FFreeOnDisk <> 0 then // v1.60L
            FZipDiskStatus := FZipDiskStatus + [zdsHasFiles] // But not the same name
          else
            FZipDiskStatus := FZipDiskStatus + [zdsEmpty];
        end;
      end;
      if Assigned(FOnStatusDisk) then // v1.60L
      begin
        FZipDiskAction := zdaOk; // The default action
        FOnStatusDisk(Self, DiskSeq, DiskFile, FZipDiskStatus,
          FZipDiskAction);
        case FZipDiskAction of
          zdaCancel: Res := IDCANCEL;
          zdaReject: Res := IDNO;
          zdaErase: Res := IDOK;
          zdaOk: Res := IDOK;
        else
          Res := IDOK;
        end;
      end
      else
        if (FZipDiskStatus * [zdsPreviousDisk, zdsSameFileName]) <> [] then
          Res := MessageBox(Handle, pChar(MsgQ), pChar('Confirm'), MB_YESNOCANCEL
            or MB_DEFBUTTON2 or MB_ICONWARNING)
        else
          Res := IDOK;
      if (Res = 0) or (Res = IDCANCEL) or (Res = IDNO) then
        raise EZipMaster.CreateResDisp(DS_Canceled, False);

      if Res = IDNO then
      begin                   // we will try again...
        FDiskWritten := 0;
        FNewDisk := True;
        Continue;
      end;
      // Create the output file.
      FOutFileHandle := FileCreate(DiskFile);
      if FOutFileHandle = -1 then
        raise EZipMaster.CreateResDisp(DS_NoOutFile, True);

      // Get the free space on this disk, correct later if neccessary.
      DiskFreeAndSize(1);     // RCV150199

      // Set the maximum number of bytes that can be written to this disk(file).
   // Reserve space on/in all the disk/file.
      FFreeOnDisk := FFreeOnDisk - KeepFreeOnAllDisks;
      // Reserve additional space on/in the first disk/file.
      if FDiskNr = 0 then
        FFreeOnDisk := FFreeOnDisk - KeepFreeOnDisk1;

      if (MaxVolumeSize > 0) and (MaxVolumeSize < FFreeOnDisk) then
        FFreeOnDisk := MaxVolumeSize;

      // Do we still have enough free space on this disk.
      if FFreeOnDisk < MinFreeVolumeSize then // No, too bad...
      begin
        FileClose(FOutFileHandle);
        DeleteFile(DiskFile);
        FOutFileHandle := -1;
        if FUnattended then
          raise EZipMaster.CreateResDisp(DS_NoUnattSpan, True);
        if Assigned(FOnStatusDisk) then // v1.60L
        begin
          if spNoVolumeName in SpanOptions then
            DiskSeq := FDiskNr + 1
          else
          begin
            DiskSeq := StrToIntDef(Copy(FVolumeName, 9, 3), 1);
            if DiskSeq < 0 then
              DiskSeq := 1;
          end;
          FZipDiskAction := zdaOk; // The default action
          FZipDiskStatus := [zdsNotEnoughSpace];
          FOnStatusDisk(Self, DiskSeq, DiskFile, FZipDiskStatus,
            FZipDiskAction);
          case FZipDiskAction of
            zdaCancel: Res := IDCANCEL;
            zdaOk: Res := IDRETRY;
            zdaErase: Res := IDRETRY;
            zdaReject: Res := IDRETRY;
          else
            Res := IDRETRY;
          end;
        end
        else
        begin
          MsgQ := LoadZipStr(DS_NoDiskSpace,
            'This disk has not enough free space available');
          Res := MessageBox(Handle, pChar(MsgQ), pChar(Application.Title),
            MB_RETRYCANCEL or MB_ICONERROR);
        end;
        if Res = 0 then
          raise EZipMaster.CreateResDisp(DS_NoMem, True);
        if Res <> IDRETRY then
          raise EZipMaster.CreateResDisp(DS_Canceled, False);
        FDiskWritten := 0;
        FNewDisk := True;
        // If all this was on a HD then this would't be useful but...
        Continue;
      end;

      // Set the volume label of this disk if it is not a fixed one.
      if not (FDriveFixed or (spNoVolumeName in SpanOptions)) then
      begin
        FVolumeName := 'PKBACK# ' + Copy(IntToStr(1001 + FDiskNr), 2, 3);
        if not SetVolumeLabel(pChar(FDrive), pChar(FVolumeName)) then
          raise EZipMaster.CreateResDisp(DS_NoVolume, True);
      end;
    end;                      // END OF: if FOutFileHandle = -1

    // Check if we have at least MinSize available on this disk,
// headers are not allowed to cross disk boundaries. ( if zero than don't care.)
    if (MinSize > 0) and (MinSize > FFreeOnDisk) then
    begin
      FileSetDate(FOutFileHandle, FDateStamp);
      FileClose(FOutFileHandle);
      FOutFileHandle := -1;
      FDiskWritten := 0;
      FNewDisk := True;
      Inc(FDiskNr);           // RCV270299
      Continue;
    end;

    // Don't try to write more bytes than allowed on this disk.
{$IFDEF VERD4+}
    MaxLen := Integer(FFreeOnDisk);
{$ELSE}
    MaxLen := Trunc(FFreeOnDisk);
{$ENDIF}
    //		MaxLen :=
    //{$IFDEF VERD4+}Integer(FFreeOnDisk){$ELSE}Trunc(FFreeOnDisk){$ENDIF}; // RCV150199
    if Len < FFreeOnDisk then
      MaxLen := Len;
    Res := FileWrite(FOutFileHandle, Buf^, MaxLen);

    // Give some progress info while writing
      // While processing the central header we don't want messages.
    if FShowProgress then
      Callback(zacProgress, 0, '', MaxLen);

    if Res = -1 then
      raise EZipMaster.CreateResDisp(DS_NoWrite, True); // A write error (disk removed?)
    Inc(FDiskWritten, Res);
    FFreeOnDisk := FFreeOnDisk - MaxLen; // RCV150199
    if MaxLen = Len then
      Break;

    // We still have some data left, we need a new disk.
    FileSetDate(FOutFileHandle, FDateStamp);
    FileClose(FOutFileHandle);
    FOutFileHandle := -1;
    FFreeOnDisk := 0;
    FDiskWritten := 0;
    Inc(FDiskNr);
    FNewDisk := True;
    Inc(Buf, MaxLen);
    Dec(Len, MaxLen);
  end;
end;
{$ENDIF}
//? TZipMaster.WriteSplit

(*? TZipMaster.WriteSpan    -----------------------------------------
1.73.2.4 31 August 2003 don't delete last part on floppy
1.73 11 July 2003 RP buffer Central Directory writes
1.73 9 July 2003 RA changed OnMessage and OnProgress to Callback calls
1.73 27 June 2003 RP changed Split file handling
// Function to read a Zip source file and write it back to one or more disks.
// Return values:
//  0           All Ok.
// -7           WriteSpan errors. See ZipMsgXX.rc
// -8           Memory allocation error.
// -9           General unknown WriteSpan error.
*)
{$IFNDEF NO_SPAN}

function TZipMaster.WriteSpan(InFileName, OutFileName: string): Integer;
type
  pZipCentralHeader = ^ZipCentralHeader;
  pZipLocalHeader = ^ZipLocalHeader;
var
  EOC       : ZipEndOfCentral;
  Res, i, k : Integer;
  LastName, MsgStr: string;
  TotalBytesWrite: Integer;
  StartCentral: Integer;
  CentralOffset: Integer;
  Buffer    : array[0..BufSize - 1] of Char;
  MDZD      : TMZipDataList;
  MDZDp     : pMZipData;
  EBuf      : string;
  ELen, VLen: integer;
  Ebufp     : pChar;
  CEHp      : pZipCentralHeader;
  LOHp      : pZipLocalHeader;
  Fname     : string;
begin
  Result := 0;
  FErrCode := 0;
  FMessage := '';
  fZipBusy := True;
  FDiskNr := 0;
  FFreeOnDisk := 0;
  FNewDisk := True;
  FDiskWritten := 0;
  FTotalDisks := -1;          // 1.72 don't know number
  FInFileName := InFileName;
  FOutFileName := OutFileName;
  FOutFileHandle := -1;
  FShowProgress := False;
  CentralOffset := 0;
  MDZD := nil;

  FDrive := ExtractFileDrive(OutFileName) + '\';
  FDriveFixed := IsFixedDrive(FDrive); // 1.72

  if ExtractFileName(OutFileName) = '' then
    raise EZipMaster.CreateResDisp(DS_NoOutFile, True);
  if not FileExists(InFileName) then
    raise EZipMaster.CreateResDisp(DS_NoInFile, True);

  StartWaitCursor;
  try
    // The following function will read the EOC and some other stuff:
    OpenEOC(EOC, True);

    // Get the date-time stamp and save for later.
    FDateStamp := FileGetDate(FInFileHandle);

    // go back to the start the zip archive.
    if (FileSeek(FInFileHandle, 0, 0) = -1) then
      raise EZipMaster.CreateResDisp(DS_FailedSeek, True);

    MDZD := TMZipDataList.Create(EOC.TotalEntries);

    // Write extended local Sig. needed for a spanned archive.
    FInteger := ExtLocalSig;
    WriteSplit(@FInteger, 4, 0);

    // Read for every zipped entry: The local header, variable data, fixed data
    // and, if present, the Data decriptor area.
    FShowProgress := True;
    CallBack(zacCount, 0, '', EOC.TotalEntries);
    CallBack(zacSize, 0, '', FFileSize);

    // 1.73 buffer writes of local header
    SetLength(EBuf, sizeof(ZipLocalHeader) + 70);
    for i := 0 to (EOC.TotalEntries - 1) do
    begin
      LOHp := pZipLocalHeader(pChar(EBuf));
      // First the local header.
      ReadJoin(LOHp^, SizeOf(ZipLocalHeader), DS_LOHBadRead);
      if not (LOHp^.HeaderSig = LocalFileHeaderSig) then
        raise EZipMaster.CreateResDisp(DS_LOHWrongSig, True);
      VLen := LOHp^.FileNameLen + LOHp^.ExtraLen;
      ELen := sizeof(ZipLocalHeader) + VLen;
      if ELen > Length(EBuf) then
      begin
        SetLength(EBuf, ELen);
        LOHp := pZipLocalHeader(pChar(EBuf)); // moved
      end;
      EBufp := pChar(EBuf) + sizeof(ZipLocalHeader);
      // Now the variable data
      ReadJoin(EBufp^, VLen, DS_LOHBadRead);
      // Save some information for later. ( on the last disk(s) ).
      MDZDp := MDZD.Items[i];
      MDZDp^.DiskStart := FDiskNr;
      MDZDp^.FileNameLen := LOHp^.FileNameLen;

      StrLCopy(MDZDp^.FileName, EBufp, LOHp^.FileNameLen); // like makestring
      Fname := SetSlash(MakeString(EBufp, LOHp^.FileNameLen), false);
      // Give message and progress info on the start of this new file read.
      MsgStr := LoadZipStr(GE_CopyFile, 'Copying: ') + Fname;
      CallBack(zacMessage, 0, MsgStr, 0);

      TotalBytesWrite := ELen + Integer(LOHp^.ComprSize);
      if (LOHp^.Flag and Word(#$0008)) = 8 then
        Inc(TotalBytesWrite, SizeOf(ZipDataDescriptor));

      CallBack(zacItem, 0, Fname, TotalBytesWrite);

      // Write the local header to the destination.
      WriteSplit(pChar(EBuf), ELen, ELen);

      // Save the offset of the LOH on this disk for later.
      MDZDp^.RelOffLocal := FDiskWritten - ELen;

      // Read Zipped data !!!For now assume we know the size!!!
      RWSplitData(Buffer, TotalBytesWrite - ELen, DS_ZipData);
    end;
    // We have written all entries to disk.
    CallBack(zacMessage, 0, LoadZipStr(GE_CopyFile, 'Copying: ')
      + LoadZipStr(DS_CopyCentral, 'central directory'), 0);
    CallBack(zacItem, 0, LoadZipStr(DS_CopyCentral, 'central directory'),
      EOC.CentralSize + sizeof(EOC) + EOC.ZipCommentLen);

    // Now write the central directory with changed offsets.
    SetLength(EBuf, sizeof(ZipCentralHeader) + 30);
    StartCentral := FDiskNr;
    for i := 0 to (EOC.TotalEntries - 1) do
    begin
      // Read a central header.
      CEHp := pZipCentralHeader(pChar(EBuf));
      ReadJoin(CEHp^, SizeOf(ZipCentralHeader), DS_CEHBadRead);
      if CEHp^.HeaderSig <> CentralFileHeaderSig then
        raise EZipMaster.CreateResDisp(DS_CEHWrongSig, True);
      // 1.73 RP copy full central record to buffer then write it
      VLen := CEHp^.FileNameLen + CEHp^.ExtraLen + CEHp^.FileComLen;
      ELen := SizeOf(ZipCentralHeader) + VLen;
      if ELen > Length(EBuf) then
      begin
        SetLength(EBuf, ELen);
        CEHp := pZipCentralHeader(pChar(EBuf)); // may have moved
      end;
      EBufp := pChar(CEHp) + sizeof(ZipCentralHeader);
      // Now the variable length fields.
      ReadJoin(EBufp^, VLen, DS_CEHBadRead);

      // Change the central directory with information stored previously in MDZD.
      k := MDZD.IndexOf(MakeString(EBufp, CEHp^.FileNameLen));
      MDZDp := MDZD[k];
      CEHp^.DiskStart := MDZDp^.DiskStart;
      CEHp^.RelOffLocal := MDZDp^.RelOffLocal;

      // Write this changed central header to disk
      // and make sure it fit's on one and the same disk.
      WriteSplit(pChar(EBuf), ELen, ELen);

      // Save the first Central directory offset for use in EOC record.
      if i = 0 then
        CentralOffset := FDiskWritten - ELen;
    end;

    // Write the changed EndOfCentral directory record.
    EOC.CentralDiskNo := StartCentral;
    EOC.ThisDiskNo := FDiskNr;
    EOC.CentralOffset := CentralOffset;
    WriteSplit(@EOC, SizeOf(EOC), SizeOf(EOC) + EOC.ZipCommentLen);

    // Skip past the original EOC to get to the ZipComment if present. v1.52j
    if (FileSeek(FInFileHandle, SizeOf(EOC), 1) = -1) then
      raise EZipMaster.CreateResDisp(DS_FailedSeek, True);

    // And finally the archive comment
    RWSplitData(Buffer, EOC.ZipCommentLen, DS_EOArchComLen);
    FShowProgress := False;
  except
    on ews: EZipMaster do     // All WriteSpan specific errors.
    begin
      ShowExceptionError(ews);
      Result := -7;
    end;
    on EOutOfMemory do        // All memory allocation errors.
    begin
      ShowZipMessage(GE_NoMem, '');
      Result := -8;
    end;
    on E: Exception do
    begin
      // The remaining errors, should not occur.
      ShowZipMessage(DS_ErrorUnknown, E.Message);
      Result := -9;
    end;
  end;

  StopWaitCursor;
  // Give the last progress info on the end of this file read.
  CallBack(zacEndOfBatch, 0, '', 0);

  if Assigned(MDZD) then
    MDZD.Free;

  FileSetDate(FOutFileHandle, FDateStamp);
  if FOutFileHandle <> -1 then
    FileClose(FOutFileHandle);
  if FInFileHandle <> -1 then
    FileClose(FInFileHandle);
  if (Result = 0) then
  begin
    // change extn of last file
    LastName := FOutFileName;
    CreateMVFileName(LastName, false);
    if FDriveFixed and (spCompatName in FSpanOptions) then
    begin
      if (FileExists(FOutFileName)) then
      begin
        MsgStr := Format(LoadZipStr(DS_AskDeleteFile, 'Overwrite previous file %s'),
          [FOutFileName]);
        FZipDiskStatus := FZipDiskStatus + [zdsSameFileName];
        if assigned(FOnStatusDisk) then
        begin
          FZipDiskAction := zdaOk; // The default action
          OnStatusDisk(self, FDiskNr, FOutFileName, FZipDiskStatus, FZipDiskAction);
          if FZipDiskAction = zdaOk then
            Res := IDYES
          else
            Res := IDNO;
        end
        else
        begin                 // if no OnStatusDisk event
          Res := Application.MessageBox(pChar(MsgStr),
            pChar(LoadZipStr(FM_Confirm, 'Confirm')),
            MB_YESNO or MB_DEFBUTTON2 or MB_ICONWARNING);
        end;
        if (Res = 0) then
          ShowZipMessage(DS_NoMem, '');
        if (Res = IDNO) then
          CallBack(zacMessage, 11010, 'Last part left as : ' + LastName, 0);
        if (Res = IDYES) then
          DeleteFile(FOutFileName); // if it exists delete old one
      end;
      if FileExists(LastName) then // should be there but ...
        RenameFile(LastName, FOutFileName);
    end;
  end;
  FTotalDisks := FDiskNr;
  fZipBusy := False;
end;
{$ENDIF}
//? TZipMaster.WriteSpan

(*? TZipMaster.Rename -------------------------------------------------------
1.73.2.1 23 August 2003 RP remove use of undefined variable 'name'
1.73  8 August 2003 RA clear outFileHandle
1.73 16 July 2003 RP use SetSlash + ConvertOEM
1.73 14 July 2003 RA convertion/re-convertion of filenames with OEM WChars
1.73 13 July 2003 RA test on date/time in RenRec + test for wildcards
// Function to read a Zip archive and change one or more file specifications.
// Source and Destination should be of the same type. (path or file)
// If NewDateTime is 0 then no change is made in the date/time fields.
// Return values:
// 0            All Ok.
// -7           Rename errors. See ZipMsgXX.rc
// -8           Memory allocation error.
// -9           General unknown Rename error.
// -10          Dest should also be a filename.
*)

function TZipMaster.Rename(RenameList: TList; DateTime: Integer): Integer;
var
  EOC       : ZipEndOfCentral;
  CEH       : ZipCentralHeader;
  LOH       : ZipLocalHeader;
  OrigFileName: string;
  MsgStr    : string;
  OutFilePath: string;
  Fname     : string;
  Buffer    : array[0..BufSize - 1] of Char;
  i, k, m   : Integer;
  TotalBytesToRead: Integer;
  TotalBytesWrite: Integer;
  RenRec    : pZipRenameRec;
  MDZD      : TMZipDataList;
  MDZDp     : pMZipData;
begin
  Result := BUSY_ERROR;       // error
  if Busy then
    exit;
  Result := 0;
  TotalBytesToRead := 0;
  fZipBusy := True;
  FShowProgress := False;

  FInFileName := FZipFileName;
  FInFileHandle := -1;
  FOutFileHandle := -1;
  MDZD := nil;

  StartWaitCursor;

  // If we only have a source path make sure the destination is also a path.
  for i := 0 to RenameList.Count - 1 do
  begin
    RenRec := RenameList.Items[i];
    RenRec^.Source := SetSlash(RenRec^.Source, false);
    RenRec^.Dest := SetSlash(RenRec^.Dest, false);
    if (AnsiPos('*', RenRec^.Source) > 0) or
      (AnsiPos('?', RenRec^.Source) > 0) or
      (AnsiPos('*', RenRec^.Dest) > 0) or
      (AnsiPos('?', RenRec^.Dest) > 0) then
    begin
      ShowZipMessage(AD_InvalidName, ''); // no wildcards allowed
      StopWaitCursor;
      fZipBusy := false;
      Result := -7;           // Rename error
      exit;
    end;
    if Length(ExtractFileName(RenRec^.Source)) = 0 then // Assume it's a path.
    begin                     // Make sure destination is a path also.
      RenRec^.Dest := AppendSlash(ExtractFilePath(RenRec^.Dest));
      RenRec^.Source := AppendSlash(RenRec^.Source);
    end
    else
      if Length(ExtractFileName(RenRec^.Dest)) = 0 then
      begin
        StopWaitCursor;
        fZipBusy := false;
        Result := -10;        // Dest should also be a filename.
        Exit;
      end;
  end;
  try
    // Check the input file.
    if not FileExists(FZipFileName) then
      raise EZipMaster.CreateResDisp(GE_NoZipSpecified {DS_NoInFile}, True);
    // Make a temporary filename like: C:\...\zipxxxx.zip
    OutFilePath := MakeTempFileName('', '');
    if OutFilePath = '' then
      raise EZipMaster.CreateResDisp(DS_NoTempFile, True);

    // Create the output file.
    FOutFileHandle := FileCreate(OutFilePath);
    if FOutFileHandle = -1 then
      raise EZipMaster.CreateResDisp(DS_NoOutFile, True);

    // The following function will read the EOC and some other stuff:
    OpenEOC(EOC, True);

    // Get the date-time stamp and save for later.
    FDateStamp := FileGetDate(FInFileHandle);

    // Now we now the number of zipped entries in the zip archive
    FTotalDisks := EOC.ThisDiskNo;
    if EOC.ThisDiskNo <> 0 then
      raise EZipMaster.CreateResDisp(RN_NoRenOnSpan, True);

    // Go to the start of the input file.
    if FileSeek(FInFileHandle, 0, 0) = -1 then
      raise EZipMaster.CreateResDisp(DS_FailedSeek, True);

    // Write the SFX header if present.
    if CopyBuffer(FInFileHandle, FOutFileHandle, FSFXOffset) <> 0 then
      raise EZipMaster.CreateResDisp(RN_ZipSFXData, True);

    // Go to the start of the Central directory.
    if FileSeek(FInFileHandle, EOC.CentralOffset, 0) = -1 then
      raise EZipMaster.CreateResDisp(DS_FailedSeek, True);

    MDZD := TMZipDataList.Create(EOC.TotalEntries);

    // Read for every entry: The central header and save information for later use.
    for i := 0 to (EOC.TotalEntries - 1) do
    begin
      // Read a central header.
      ReadJoin(CEH, SizeOf(CEH), DS_CEHBadRead);

      if CEH.HeaderSig <> CentralFileHeaderSig then
        raise EZipMaster.CreateResDisp(DS_CEHWrongSig, True);

      // Now the filename.
      ReadJoin(Buffer, CEH.FileNameLen, DS_CENameLen);
      Buffer[CEH.FileNameLen] := #0;
      Fname := Buffer;

      // Save the file name info in the MDZD structure.
      MDZDp := MDZD[i];
      MDZDp^.FileNameLen := CEH.FileNameLen;
      //			StrLCopy(MDZDp^.FileName, Buffer, CEH.FileNameLen);
            // convert OEM char set in original file else we don't find the file
      FVersionMadeBy1 := CEH.VersionMadeBy1;
      FVersionMadeBy0 := CEH.VersionMadeBy0;
      Fname := ConvertOEM(Fname, cpdOEM2ISO);
      StrLCopy(MDZDp^.FileName, pChar(Fname), 253);
      //DiskStart is not used in this function and we need FHostNum later
      MDZDp^.DiskStart := (FVersionMadeBy1 shl 8) or FVersionMadeBy0;
      MDZDp^.RelOffLocal := CEH.RelOffLocal;
      MDZDp^.DateTime := DateTime;

      // We need the total number of bytes we are going to read for the progress event.
      TotalBytesToRead := TotalBytesToRead + Integer(CEH.ComprSize +
        CEH.FileNameLen + CEH.ExtraLen);

      // Seek past the extra field and the file comment.
      if FileSeek(FInFileHandle, CEH.ExtraLen + CEH.FileComLen, 1) = -1 then
        raise EZipMaster.CreateResDisp(DS_FailedSeek, True);
    end;

    FShowProgress := True;
    CallBack(zacCount, 0, '', EOC.TotalEntries);
    CallBack(zacSize, 0, '', TotalBytesToRead);

    // Read for every zipped entry: The local header, variable data, fixed data
    // and if present the Data descriptor area.
    for i := 0 to (EOC.TotalEntries - 1) do
    begin
      // Seek to the first entry.
      MDZDp := MDZD[i];
      FileSeek(FInFileHandle, MDZDp^.RelOffLocal, 0);

      // First the local header.
      ReadJoin(LOH, SizeOf(LOH), DS_LOHBadRead);
      if LOH.HeaderSig <> LocalFileHeaderSig then
        raise EZipMaster.CreateResDisp(DS_LOHWrongSig, True);

      // Now the filename.
      ReadJoin(Buffer, LOH.FileNameLen, DS_LONameLen);

      // Set message info on the start of this new fileread because we still have the old filename.
      MsgStr := LoadZipStr(RN_ProcessFile, 'Processing: ') + MDZDp^.FileName;

      // Calculate the bytes we are going to write; we 'forget' the difference
   // between the old and new filespecification.
      TotalBytesWrite := LOH.FileNameLen + LOH.ExtraLen + LOH.ComprSize;

      // Check if the original path and/or filename needs to be changed.
      OrigFileName := SetSlash(MDZDp^.FileName, false); //ReplaceForwardSlash(MDZDp^.FileName);
      for m := 0 to RenameList.Count - 1 do
      begin
        RenRec := RenameList.Items[m];
        k := Pos(UpperCase(RenRec^.Source), UpperCase(OrigFileName));
        if k <> 0 then
        begin
          System.Delete(OrigFileName, k, Length(RenRec^.Source));
          Insert(RenRec^.Dest, OrigFileName, k);
          LOH.FileNameLen := Length(OrigFileName);
          for k := 1 to Length(OrigFileName) do
            if OrigFileName[k] = '\' then
              OrigFileName[k] := '/';
          MsgStr := MsgStr + LoadZipStr(RN_RenameTo, ' renamed to: ') +
            OrigFileName;
          //allow OEM char sets in Rename
          //we replaced the filename look if we need to reconvert it
          FVersionMadeBy1 := (MDZDp^.DiskStart and $FF00) shl 8;
          FVersionMadeBy0 := (MDZDp^.DiskStart and $FF);
          OrigFileName := ConvertOEM(OrigFileName, cpdISO2OEM);
          //          OrigFileName := ConvCodePage(OrigFileName, cpdISO2OEM);
          StrPLCopy(MDZDp^.FileName, OrigFileName, Length(OrigFileName) + 1);
          MDZDp^.FileNameLen := Length(OrigFileName);

          // Change Date and Time if needed.
          if RenRec^.DateTime <> 0 then
          try
            // test if valid date/time will throw error if not
            FileDateToDateTime(RenRec^.DateTime);
            MDZDp^.DateTime := RenRec^.DateTime;
          except
            ShowZipMessage(RN_InvalidDateTime, MDZDp^.FileName);
          end;
        end;
      end;
      CallBack(zacMessage, 0, MsgStr, 0);

      // Change Date and/or Time if needed.
      if MDZDp^.DateTime <> 0 then
      begin
        LOH.ModifDate := HIWORD(MDZDp^.DateTime);
        LOH.ModifTime := LOWORD(MDZDp^.DateTime);
      end;
      // Change info for later while writing the central dir.
      MDZDp^.RelOffLocal := FileSeek(FOutFileHandle, 0, 1);

      CallBack(zacItem, 0, SetSlash(MDZDp^.FileName, false), TotalBytesWrite);

      // Write the local header to the destination.
      WriteJoin(@LOH, SizeOf(LOH), DS_LOHBadWrite);

      // Write the filename.
      WriteJoin(MDZDp^.FileName, LOH.FileNameLen, DS_LOHBadWrite);

      // And the extra field
      if CopyBuffer(FInFileHandle, FOutFileHandle, LOH.ExtraLen) <> 0 then
        raise EZipMaster.CreateResDisp(DS_LOExtraLen, True);

      // Read and write Zipped data
      if CopyBuffer(FInFileHandle, FOutFileHandle, LOH.ComprSize) <> 0 then
        raise EZipMaster.CreateResDisp(DS_ZipData, True);

      // Read DataDescriptor if present.
      if (LOH.Flag and Word(#$0008)) = 8 then
        if CopyBuffer(FInFileHandle, FOutFileHandle, SizeOf(ZipDataDescriptor))
          <> 0 then
          raise EZipMaster.CreateResDisp(DS_DataDesc, True);
    end;                      // Now we have written all entries.

    // Now write the central directory with possibly changed offsets and filename(s).
    FShowProgress := False;
    for i := 0 to (EOC.TotalEntries - 1) do
    begin
      MDZDp := MDZD[i];
      // Read a central header which can be span more than one disk.
      ReadJoin(CEH, SizeOf(CEH), DS_CEHBadRead);
      if CEH.HeaderSig <> CentralFileHeaderSig then
        raise EZipMaster.CreateResDisp(DS_CEHWrongSig, True);

      // Change Date and/or Time if needed.
      if MDZDp^.DateTime <> 0 then
      begin
        CEH.ModifDate := HIWORD(MDZDp^.DateTime);
        CEH.ModifTime := LOWORD(MDZDp^.DateTime);
      end;

      // Now the filename.
      ReadJoin(Buffer, CEH.FileNameLen, DS_CENameLen);

      // Save the first Central directory offset for use in EOC record.
      if i = 0 then
        EOC.CentralOffset := FileSeek(FOutFileHandle, 0, 1);

      // Change the central header info with our saved information.
      CEH.RelOffLocal := MDZDp^.RelOffLocal;
      CEH.DiskStart := 0;
      EOC.CentralSize := EOC.CentralSize - CEH.FileNameLen + MDZDp^.FileNameLen;
      CEH.FileNameLen := MDZDp^.FileNameLen;

      // Write this changed central header to disk
      WriteJoin(@CEH, SizeOf(CEH), DS_CEHBadWrite);

      // Write to destination the central filename and the extra field.
      WriteJoin(MDZDp^.FileName, CEH.FileNameLen, DS_CEHBadWrite);

      // And the extra field
      if CopyBuffer(FInFileHandle, FOutFileHandle, CEH.ExtraLen) <> 0 then
        raise EZipMaster.CreateResDisp(DS_CEExtraLen, True);

      // And the file comment.
      if CopyBuffer(FInFileHandle, FOutFileHandle, CEH.FileComLen) <> 0 then
        raise EZipMaster.CreateResDisp(DS_CECommentLen, True);
    end;
    // Write the changed EndOfCentral directory record.
    EOC.CentralDiskNo := 0;
    EOC.ThisDiskNo := 0;
    WriteJoin(@EOC, SizeOf(EOC), DS_EOCBadWrite);

    // And finally the archive comment
  { ==================== Changed by Jin Turner ===================}
    if (FZipComment <> '')
      and (FileWrite(FOutFileHandle, FZipComment[1], Length(FZipComment)) < 0)
      then
      //        if CopyBuffer(FInFileHandle, FOutFileHandle, EOC.ZipCommentLen) <> 0 then
      raise EZipMaster.CreateResDisp(DS_EOArchComLen, True);
  except
    on ers: EZipMaster do     // All Rename specific errors.
    begin
      ShowExceptionError(ers);
      Result := -7;
    end;
    on EOutOfMemory do        // All memory allocation errors.
    begin
      ShowZipMessage(GE_NoMem, '');
      Result := -8;
    end;
    on E: Exception do
    begin
      // the error message of an unknown error is displayed ...
      ShowZipMessage(DS_ErrorUnknown, E.Message);
      Result := -9;
    end;
  end;
  if Assigned(MDZD) then
    MDZD.Free;

  // Give final progress info at the end.
  CallBack(zacEndOfBatch, 0, '', 0);

  if FInFileHandle <> -1 then
    FileClose(FInFileHandle);
  if FOutFileHandle <> -1 then
  begin
    FileSetDate(FOutFileHandle, FDateStamp);
    FileClose(FOutFileHandle);
    if Result <> 0 then       // An error somewhere, OutFile is not reliable.
      DeleteFile(OutFilePath)
    else
    begin
      EraseFile(FZipFileName, FHowToDelete);
      RenameFile(OutFilePath, FZipFileName);
      _List;
    end;
  end;

  fZipBusy := False;
  StopWaitCursor;
end;
//? TZipMaster.Rename

(*? TZipMaster.SetZipSwitches
1.73  1 August 2003 RP - set required dll interface version
*)

procedure TZipMaster.SetZipSwitches(var NameOfZipFile: string; zpVersion:
  Integer);
var
  i         : Integer;
  SufStr, Dts: string;
  pExFiles  : pExcludedFileSpec;
begin
  with ZipParms^ do
  begin
    if Length(FZipComment) <> 0 then
    begin
      fArchComment := StrAlloc(Length(FZipComment) + 1);
      StrPLCopy(fArchComment, FZipComment, Length(FZipComment) + 1);
    end;
    if AddArchiveOnly in fAddOptions then
      fArchiveFilesOnly := 1;
    if AddResetArchive in fAddOptions then
      fResetArchiveBit := 1;

    if (FFSpecArgsExcl.Count <> 0) then
    begin
      fTotExFileSpecs := FFSpecArgsExcl.Count;
      fExFiles := AllocMem(SizeOf(ExcludedFileSpec) * FFSpecArgsExcl.Count);
      for i := 0 to (fFSpecArgsExcl.Count - 1) do
      begin
        pExFiles := fExFiles;
        Inc(pExFiles, i);
        pExFiles.fFileSpec := StrAlloc(Length(fFSpecArgsExcl[i]) + 1);
        StrPLCopy(pExFiles.fFileSpec, fFSpecArgsExcl[i],
          Length(fFSpecArgsExcl[i]) + 1);
      end;
    end;
    // New in v 1.6M Dll 1.6017, used when Add Move is choosen.
    if FHowToDelete = htdAllowUndo then
      fHowToMove := True;
    if FCodePage = cpOEM then
      fWantedCodePage := 2;
  end;                        { end with }

  if (Length(FTempDir) <> 0) then
  begin
    ZipParms.fTempPath := StrAlloc(Length(FTempDir) + 1);
    StrPLCopy(ZipParms.fTempPath, FTempDir, Length(FTempDir) + 1);
  end;

  with ZipParms^ do
  begin
    Version := ZIPVERSION;    // version we expect the DLL to be
    Caller := Self;           // point to our VCL instance; returned in callback

    fQuiet := True;           { we'll report errors upon notification in our callback }
    { So, we don't want the DLL to issue error dialogs }

    ZCallbackFunc := ZCallback; // pass addr of function to be called from DLL
    fJunkSFX := False;        { if True, convert input .EXE file to .ZIP }

    SufStr := '';
    for i := 0 to Integer(assEXE) do
      AddSuffix(AddStoreSuffixEnum(i), SufStr, i);
    if assEXT in fAddStoreSuffixes then // new 1.71
      SufStr := SufStr + fExtAddStoreSuffixes;
    if Length(SufStr) <> 0 then
    begin
      System.Delete(SufStr, Length(SufStr), 1);
      pSuffix := StrAlloc(Length(SufStr) + 1);
      StrPLCopy(pSuffix, SufStr, Length(SufStr) + 1);
    end;
    // fComprSpecial := False;     { if True, try to compr already compressed files }

    fSystem := False;         { if True, include system and hidden files }

    if AddVolume in fAddOptions then
      fVolume := True         { if True, include volume label from root dir }
    else
      fVolume := False;

    fExtra := False;          { if True, include extended file attributes-NOT SUPTED }

    fDate := AddFromDate in fAddOptions; { if True, exclude files earlier than specified date }
    { Date := '100592'; }{ Date to include files after; only used if fDate=TRUE }
    dts := FormatDateTime('mm dd yy', fFromDate);
    for i := 0 to 7 do
      Date[i] := dts[i + 1];

    fLevel := FAddCompLevel;  { Compression level (0 - 9, 0=none and 9=best) }
    fCRLF_LF := False;        { if True, translate text file CRLF to LF (if dest Unix)}
    if AddSafe in FAddOptions then
      fGrow := false
    else
      fGrow := True;          { if True, Allow appending to a zip file (-g)}

    fDeleteEntries := False;  { distinguish bet. Add and Delete }

    if fTrace then
      fTraceEnabled := True
    else
      fTraceEnabled := False;
    if fVerbose then
      fVerboseEnabled := True
    else
      fVerboseEnabled := False;
    if (fTraceEnabled and not fVerbose) then
      fVerboseEnabled := True; { if tracing, we want verbose also }

    if FUnattended then
      Handle := 0
    else
      Handle := fHandle;

    if AddForceDOS in fAddOptions then
      fForce := True          { convert all filenames to 8x3 format }
    else
      fForce := False;
    if AddZipTime in fAddOptions then
      fLatestTime := True     { make zipfile's timestamp same as newest file }
    else
      fLatestTime := False;
    if AddMove in fAddOptions then
      fMove := True           { dangerous, beware! }
    else
      fMove := False;
    if AddFreshen in fAddOptions then
      fFreshen := True
    else
      fFreshen := False;
    if AddUpdate in fAddOptions then
      fUpdate := True
    else
      fUpdate := False;
    if (fFreshen and fUpdate) then
      fFreshen := False;      { Update has precedence over freshen }

    if AddEncrypt in fAddOptions then
      fEncrypt := True        { DLL will prompt for password }
    else
      fEncrypt := False;

    { NOTE: if user wants recursion, then he probably also wants
        AddDirNames, but we won't demand it. }
    if AddRecurseDirs in fAddOptions then
      fRecurse := True
    else
      fRecurse := False;

    if AddHiddenFiles in fAddOptions then
      fSystem := True
    else
      fSystem := False;

    if AddSeparateDirs in fAddOptions then
      fNoDirEntries := False { do make separate dirname entries - and also
      include dirnames with filenames }
    else
      fNoDirEntries := True; { normal zip file - dirnames only stored
    with filenames }

    if AddDirNames in fAddOptions then
      fJunkDir := False       { we want dirnames with filenames }
    else
      fJunkDir := True;       { don't store dirnames with filenames }

    pZipFN := StrAlloc(Length(NameOfZipFile) + 1); { allocate room for null terminated string }
    StrPLCopy(pZipFN, NameOfZipFile, Length(NameOfZipFile) + 1); { name of zip file }
    if Length(FPassword) > 0 then
    begin
      pZipPassword := StrAlloc(Length(FPassword) + 1); { allocate room for null terminated string }
      StrPLCopy(pZipPassword, FPassword, PWLEN + 1); { password for encryption/decryption }
    end;
  end;                        {end else with do }
end;
//? TZipMaster.SetZipSwitches

(*? TZipMaster.CallBack
1.73  1 August 2003 RP new progress percent changed to integer (-1 = end of batch)
1.73 12 July 2003 RA set flag IsDll
1.73 12 July 2003 RP fix handling Extra Data
1.73 30 June 2003 RP changed action codes & progress codes
// 1.73 (29 June 2003) fIsDestructing stops callbacks
// 1.73 ( 4 June 2003) don't count extra progress in total
// 1.73 ( 1 June 2003) new signiture
// 1.72 use class function to process callbacks (allows vcl to call callback)
// now catches exceptions in events
// 1.72 added ActionCode = 0 - Tick
{  1.73 added extra events for progress
  changed 'case's 1,3,4,6
  changed sub function ProgMsg to use new resource strings
  added sub function Percent to safely calculate percentage done
 added ActionCode = 14 - Get Extra Data                  }
*)

function TZipMaster.CallBack(ActionCode: ActionCodes; ErrorCode: integer; Msg: string;
  FileSize: cardinal): integer;
var
  OldFileName, pwd, FileComment: string;
  DoStop, IsChanged, DoExtract, DoOverwrite: Boolean;
  RptCount  : LongWord;
  Response  : TPasswordButton;
  //  XData        : string;
  ZRec      : PZCallBackStruct;
  xlen      : integer;
  IsDll     : Boolean;
{$IFDEF DEBUGCALLBACK}
  Info      : CallBackInfo;
{$ENDIF}

  // new 1.73
  function PerCent(total, position: cardinal): cardinal;
  begin
    Result := 0;
    if (total = 0) or (position = 0) then
      exit;                   // not strictly correct but will do
    if total < MAX_PERCENT then
      Result := (100 * position) div total
    else
      Result := position div (total div 100);
  end;

begin
  Result := 0;
  if fIsDestructing then
  begin                       // in destructor return
    if ActionCode = zacDLL then
      Result := -1;           // return abort
    exit;
  end;
  IsDll := false;
  ZRec := nil;
  if ActionCode = zacDLL then
  begin
    IsDll := true;
    ZRec := PZCallBackStruct(pointer(FileSize));
    ActionCode := ActionCodes(ZRec^.ActionCode);
    ErrorCode := ZRec^.ErrorCode;
    FileSize := ZRec^.FileSize;
    Msg := SetSlash(TrimRight(string(ZRec^.FileNameOrMsg)), false);
  end;
  try
    case ActionCode of
      zacTick:                { 'Tick' Just checking / processing messages}
        if assigned(fOnTick) then
          fOnTick(self);

      zacItem:                { progress type 1 = starting any ZIP operation on a new file }
        begin
          if Assigned(FOnProgress) then
            FOnProgress(self, NewFile, Msg, FileSize);
          if Assigned(FOnItemProgress) then
          begin
            FItemSizeToProcess := FileSize;
            FItemPosition := 0;
            FItemName := Msg;
            FOnItemProgress(self, FItemName, FileSize, 0);
          end;
        end;
      zacProgress:            { progress type 2 = increment bar }
        begin
          if Assigned(FOnProgress) then
            FOnProgress(self, ProgressUpdate, '', FileSize);
          if Assigned(FOnItemProgress) then
          begin
            inc(FItemPosition, FileSize);
            FOnItemProgress(self, FItemName, FItemSizeToProcess,
              PerCent(FItemSizeToProcess, FItemPosition));
          end;
          if Assigned(OnTotalProgress) then
          begin
            inc(FTotalPosition, FileSize);
            OnTotalProgress(self, FTotalSizeToProcess,
              PerCent(FTotalSizeToProcess, FTotalPosition));
          end;
        end;
      zacEndOfBatch:          { end of a batch of 1 or more files }
        begin
          if Assigned(FOnProgress) then
            FOnProgress(self, EndOfBatch, '', 0);
          // 1.73 new
          FItemSizeToProcess := 0;
          FItemPosition := 0;
          FItemName := '';
          //        FTotalSizeToProcess := 0;
          FTotalPosition := 0;
          if Assigned(FOnItemProgress) then
            FOnItemProgress(self, FItemName, 0, -1); //100);
          if Assigned(FOnTotalProgress) then
            FOnTotalProgress(self, 0, -1); //100);
          // end 1.73 new
        end;
      zacMessage:             { a routine status message }
        begin
          FMessage := Msg;
          if ErrorCode <> 0 then // W'll always keep the last ErrorCode
          begin
            ErrCode := Integer(Char(ErrorCode and $FF));
            if (ErrCode = 9) and (fEventErr <> '') then // user cancel
              FMessage := 'Exception in Event ' + fEventErr;
            fFullErrCode := ErrorCode;
          end;
          if Assigned(OnMessage) then
            OnMessage(self, ErrorCode, FMessage);
        end;

      zacCount:               { total number of files to process }
        if Assigned(OnProgress) then
          OnProgress(self, TotalFiles2Process, '', FileSize);

      zacSize:                { total size of all files to be processed }
        begin
          FTotalSizeToProcess := FileSize;
          FTotalPosition := 0;
          // new 1.73
          FItemName := '';
          FItemSizeToProcess := 0;
          // end new 1.73
          if Assigned(FOnProgress) then
            FOnProgress(self, TotalSize2Process, '', FileSize);
        end;

      zacNewName:             { request for a new path+name just before zipping or extracting }
        if Assigned(FOnSetNewName) then
        begin
          OldFileName := Msg;
          IsChanged := False;

          FOnSetNewName(self, OldFileName, IsChanged);
          if IsChanged then
          begin
            StrPLCopy(ZRec^.FileNameOrMsg, OldFileName, 512);
            ZRec^.ErrorCode := 1;
          end
          else
            ZRec^.ErrorCode := 0;
        end;

      zacPassword:            { New or other password needed during Extract() }
        begin
          pwd := '';
          RptCount := FileSize;
          Response := pwbOk;

          GAssignPassword := False;
          if Assigned(FOnPasswordError) then
          begin
            GModalResult := mrNone;
            FOnPasswordError(self, ZRec^.IsOperationZip, pwd, Msg, RptCount,
              Response);
            if Response <> pwbOk then
              pwd := '';
            if Response = pwbCancelAll then
              GModalResult := mrNoToAll;
            if Response = pwbAbort then
              GModalResult := mrAbort;
          end
          else
            if (ErrorCode and $01) <> 0 then
              pwd := GetAddPassword()
            else
              pwd := GetExtrPassword();

          if pwd <> '' then
          begin
            StrPLCopy(ZRec^.FileNameOrMsg, pwd, PWLEN);
            ZRec^.ErrorCode := 1;
          end
          else
          begin
            RptCount := 0;
            ZRec^.ErrorCode := 0;
          end;
          if RptCount > 15 then
            RptCount := 15;
          ZRec^.FileSize := RptCount;
          if GModalResult = mrNoToAll then // Cancel all
            ZRec^.ActionCode := 0;
          if GModalResult = mrAbort then // Abort
            Cancel := True;
          GAssignPassword := True;
        end;

      zacCRCError:            { CRC32 error, (default action is extract/test the file) }
        begin
          DoExtract := true;  // This was default for versions <1.6
          if Assigned(FOnCRC32Error) then
            FOnCRC32Error(self, Msg, ErrorCode, FileSize, DoExtract);
          ZRec^.ErrorCode := Integer(DoExtract);
          { This will let the Dll know it should send some warnings }
          if not Assigned(FOnCRC32Error) then
            ZRec^.ErrorCode := 2;
        end;

      zacOverwrite:           { Extract(UnZip) Overwrite ask }
        if Assigned(FOnExtractOverwrite) then
        begin
          DoOverwrite := Boolean(FileSize);
          FOnExtractOverwrite(self, Msg, (ErrorCode and $10000) = $10000,
            DoOverwrite, ErrorCode and $FFFF);
          ZRec^.FileSize := Integer(DoOverwrite);
        end;

      zacSkipped:             { Extract(UnZip) and Skipped }
        begin
          if ErrorCode <> 0 then
          begin
            ErrCode := Integer(Char(ErrorCode and $FF));
            FFullErrCode := ErrorCode;
          end;
          if Assigned(FOnExtractSkipped) then
            FOnExtractSkipped(self, Msg, UnZipSkipTypes((FileSize and $FF) - 1),
              ZRec^.ErrorCode);
        end;

      zacComment:             { Add(Zip) FileComments. v1.60L }
        if Assigned(FOnFileComment) then
        begin
          FileComment := ZRec^.FileNameOrMsg + 256;
          IsChanged := False;
          FOnFileComment(self, Msg, FileComment, IsChanged);
          if IsChanged then
          begin
            if (FileComment <> '') then
              StrPLCopy(ZRec^.FileNameOrMsg, FileComment, 511)
            else
              ZRec^.FileNameOrMsg[0] := #0;
          end;
          ZRec^.ErrorCode := Integer(IsChanged);
          FileSize := Length(FileComment);
          if FileSize > 511 then
            FileSize := 511;
          ZRec^.FileSize := FileSize;
        end;

      zacStream:              { Stream2Stream extract. v1.60M }
        begin
          try
            FZipStream.SetSize(FileSize);
          except
            ZRec^.ErrorCode := 1;
            ZRec^.FileSize := 0;
          end;
          if ZRec^.ErrorCode <> 1 then
            ZRec^.FileSize := Integer(FZipStream.Memory);
        end;

      zacData:                { Set Extra Data v1.72 }
        if Assigned(FOnFileExtra) then
        begin
          SetLength(FStoredExtraData, FileSize);
          if FileSize > 0 then
            move(CallbackData(ZRec^.FileNameOrMsg).Data^, pChar(FStoredExtraData)^, FileSize);
          //					FStoredExtraData := XData;    // hold copy
          IsChanged := False;
          FOnFileExtra(self, Msg, FStoredExtraData, IsChanged);
          if IsChanged then
          begin
            xlen := Length(FStoredExtraData);
            if (xlen > 0) then
            begin
              if (xlen < 512) then
                move(pChar(FStoredExtraData)^, ZRec^.FileNameOrMsg, xlen)
              else
                CallBackData(ZRec^.FileNameOrMsg).Data := pByteArray(pChar(FStoredExtraData));
            end;
            ZRec^.FileSize := xlen;
            ZRec^.ErrorCode := -1;
          end;
        end;

      zacXItem:               { progress type 15 = starting new extra operation }
        begin
          if Assigned(FOnProgress) then
            FOnProgress(self, NewExtra, Msg, FileSize);
          // 1.73 new
          if Assigned(FOnItemProgress) then
          begin
            FItemSizeToProcess := FileSize;
            FItemPosition := 0;
            FItemName := LoadZipStr(PR_Progress + ErrorCode, Msg);
            FOnItemProgress(self, FItemName, FileSize, 0);
          end;
        end;
      zacXProgress:           { progress type 16 = increment bar for extra operation}
        begin
          if Assigned(FOnProgress) then
            FOnProgress(self, ExtraUpdate, FItemName, FileSize);
          if Assigned(FOnItemProgress) then
          begin
            inc(FItemPosition, FileSize);
            FOnItemProgress(self, FItemName, FItemSizeToProcess,
              PerCent(FItemSizeToProcess, FItemPosition));
          end;
        end;
    end;                      {end case }
  except
    on E: Exception do
    begin
      if not IsDLL then
        raise;                // in vcl
      if fEventErr = '' then  // catch first exception only
        fEventErr := ' #' + IntToStr(ord(ActionCode)) + ' "' + E.Message + '"';
      Cancel := true;
    end;
  end;
  if assigned(fOnAfterCallback) then
  begin
    DoStop := Cancel;
{$IFDEF DEBUGCALLBACK}
    info := CallBackInfo.Create;
    Info.msg := Msg;
    Info.zip := self;
    Info.code := ord(ActionCode);
    Info.error := ErrorCode;
    Info.size := FileSize;

    fOnAfterCallback(Info, DoStop);
    Info.Free;
{$ELSE}
    fOnAfterCallback(self, DoStop);
{$ENDIF}
    if DoStop then
      Cancel := true;
  end
  else
    Application.ProcessMessages;
  { If you return TRUE, then the DLL will abort it's current
   batch job as soon as it can. }
  Result := ord(Cancel);
  if Cancel and (not IsDLL) then // not from dll
    raise EZipMaster.CreateResDisp(DS_Canceled, true); // within vcl take exception
end;
//? TZipMaster.CallBack

(*? TZipMaster.ConvertOEM
1.73 24 July 2003 RA adjust result string length
1.73 ( 2 June 2003) RP replacement function that should be able to handle MBCS
//---------------------------------------------------------------------------
( * Convert filename (and file comment string) into 'internal' charset (ISO).
 * This function assumes that Zip entry filenames are coded in OEM (IBM DOS)
 * codepage when made on:
 *  -> DOS (this includes 16-bit Windows 3.1)  (FS_FAT_  {0} )
 *  -> OS/2                                    (FS_HPFS_ {6} )
 *  -> Win95/WinNT with Nico Mak''s WinZip      (FS_NTFS_ {11} && hostver == '5.0' {50} )
 *
 * All other ports are assumed to code zip entry filenames in ISO 8859-1.
 * But norton Zip v1.0 sets the host byte as OEM(0) but does use the ISO set,
 * thus archives made by NortonZip are not recognized as being ISO.
 * (In this case you need to set the CodePage property manualy to cpNone.)
 * When ISO is used in the zip archive there is no need for translation
 * and so we call this cpNone.
 *)

function TZipMaster.ConvertOEM(const Source: string; Direction: CodePageDirection): string;
const
  FS_FAT    : Integer = 0;
  FS_HPFS   : Integer = 6;
  FS_NTFS   : Integer = 11;
var
  buf       : string;
begin
  Result := Source;
  if ((FCodePage = cpAuto) and (FVersionMadeBy1 = FS_FAT) or (FVersionMadeBy1 =
    FS_HPFS)
    or ((FVersionMadeBy1 = FS_NTFS) and (FVersionMadeBy0 = 50))) or (FCodePage =
    cpOEM) then
  begin
    SetLength(buf, 2 * Length(Source) + 1); // allow worst case - all double
    if (Direction = cpdOEM2ISO) then
      OemToChar(pChar(Source), pChar(buf))
    else
      CharToOem(pChar(Source), pChar(buf));
    Result := pChar(buf);
  end;
end;
//? TZipMaster.ConvertOEM

//---------------------------------------------------------------------------
(*? TZipMaster.ReadJoin
1.73 15 July 2003 new function
*)

procedure TZipMaster.ReadJoin(var Buffer; BufferSize, DSErrIdent: Integer);
begin
  if FileRead(FInFileHandle, Buffer, BufferSize) <> BufferSize then
    raise EZipMaster.CreateResDisp(DSErrIdent, true);
end;
//? TZipMaster.ReadJoin

(*? TZipMaster.GetMessage
1.73 13 July 2003 RP only return message if error
*)

function TZipMaster.GetMessage: string;
begin
  Result := '';
  if FErrCode <> 0 then
  begin
    Result := fMessage;
    if Result = '' then
      Result := LoadZipStr(FErrCode, 'unknown error ' + IntToStr(FErrCode));
  end;
end;
//? TZipMaster.GetMessage

(*? TZipMaster._List
1.73 15 July 2003 RP ReadJoin
1.73 13 July 2003 RP change handling part of span
1.73 12 July 2003 RP string Extra Data
1.73 27 June 2003 RP changed Split disk handling
*)

procedure TZipMaster._List;   { all work is local - no DLL calls }
var
  pzd       : pZipDirEntry;
  EOC       : ZipEndOfCentral;
  CEH       : ZipCentralHeader;
  OffsetDiff: Integer;
  Fname     : string;
  i, LiE    : Integer;
begin
  LiE := 0;
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;                     { can't do LIST at design time }

  { zero out any previous entries }
  FreeZipDirEntryRecords;

  FRealFileSize := 0;
  FZipSOC := 0;
  FSFXOffset := 0;            // must be before the following "if"
  FZipComment := '';
  OffsetDiff := 0;
  FIsSpanned := False;
  FDirOnlyCount := 0;
  fErrCode := 0;              // 1.72

  if (FZipFileName = '') or not FileExists(FZipFileName) then
  begin
    { let user's program know there's no entries }
    if Assigned(FOnDirUpdate) then
      FOnDirUpdate(Self);
    Exit;
  end;

  FInfileName := FZipFileName;
  FDrive := ExtractFileDrive(ExpandFileName(FInFileName)) + '\';
  FDriveFixed := IsFixedDrive(FDrive);
  GetDriveProps;

  try
    OpenEOC(EOC, true);       // exception if not
  except
    on E: EZipMaster do
      ShowExceptionError(E);
  end;                        { let user's program know there's no entries }
  if FInFileHandle = -1 then  // was problem
  begin
    if Assigned(FOnDirUpdate) then
      FOnDirUpdate(Self);
    Exit;
  end;

  try
    StartWaitCursor;
    try
      FTotalDisks := EOC.ThisDiskNo; // Needed in case GetNewDisk is called.

      // This could also be set to True if it's the first and only disk.
      if EOC.ThisDiskNo > 0 then
        FIsSpanned := True;

      // Do we have to request for a previous disk first?
      if EOC.ThisDiskNo <> EOC.CentralDiskNo then
      begin
        GetNewDisk(EOC.CentralDiskNo);
        FFileSize := FileSeek(FInFileHandle, 0, 2); //v1.52i
        OffsetDiff := EOC.CentralOffset; //v1.52i
      end
      else                    //v1.52i
        // Due to the fact that v1.3 and v1.4x programs do not change the archives
        // EOC and CEH records in case of a SFX conversion (and back) we have to
        // make this extra check.
        OffsetDiff := Longword(FFileSize) - EOC.CentralSize - SizeOf(EOC) -
          EOC.ZipCommentLen;
      FZipSOC := OffsetDiff;  // save the location of the Start Of Central dir
      FSFXOffset := FFileSize; // initialize this - we will reduce it later
      if FFileSize = 22 then
        FSFXOffset := 0;

      FWrongZipStruct := False;
      if EOC.CentralOffset <> Longword(OffsetDiff) then
      begin
        FWrongZipStruct := True; // We need this in the ConvertXxx functions.
        ShowZipMessage(LI_WrongZipStruct, '');
      end;

      // Now we can go to the start of the Central directory.
      if FileSeek(FInFileHandle, OffsetDiff, 0) = -1 then
        raise EZipMaster.CreateResDisp(LI_ReadZipError, True);

      // Read every entry: The central header and save the information.
      for i := 0 to (EOC.TotalEntries - 1) do
      begin
        // Read a central header entry for 1 file
        while FileRead(FInFileHandle, CEH, SizeOf(CEH)) <> SizeOf(CEH) do //v1.52i
        begin
          // It's possible that we have the central header split up.
          if FDiskNr >= EOC.ThisDiskNo then
            raise EZipMaster.CreateResDisp(DS_CEHBadRead, True);
          // We need the next disk with central header info.
          GetNewDisk(FDiskNr + 1);
        end;

        //validate the signature of the central header entry
        if CEH.HeaderSig <> CentralFileHeaderSig then
          raise EZipMaster.CreateResDisp(DS_CEHWrongSig, True);

        // Now the filename
        SetLength(Fname, CEH.FileNameLen);
				ReadJoin(Fname[1], CEH.FileNameLen, DS_CENameLen);

				// Save version info globally for use by codepage translation routine
				FVersionMadeBy0 := CEH.VersionMadeBy0;
				FVersionMadeBy1 := CEH.VersionMadeBy1;
				Fname := ConvertOEM(Fname, cpdOEM2ISO);

        // Create a new ZipDirEntry pointer.
        New(pzd);             // These will be deleted in: FreeZipDirEntryRecords.

        // Copy the needed file info from the central header.
        CopyMemory(pzd, @CEH.VersionMadeBy0, 42);
        pzd^.FileName := SetSlash(Fname, false); //ReplaceForwardSlash(Fname);
        pzd^.Encrypted := (pzd^.Flag and 1) > 0;

        pzd^.ExtraData := '';
        // Read the extra data if present new v1.6
        if pzd^.ExtraFieldLength > 0 then
        begin
          SetLength(pzd^.ExtraData, pzd^.ExtraFieldLength);
          ReadJoin(pzd^.ExtraData[1], CEH.ExtraLen, LI_ReadZipError);
        end;

        // Read the FileComment, if present, and save.
        if CEH.FileComLen > 0 then
        begin
          // get the file comment
          SetLength(pzd^.FileComment, CEH.FileComLen);
          ReadJoin(pzd^.FileComment[1], CEH.FileComLen, DS_CECommentLen);
          pzd^.FileComment := ConvertOEM(pzd^.FileComment, cpdOEM2ISO);
          //					pzd^.FileComment := ConvCodePage(pzd^.FileComment, cpdOEM2ISO);
        end;

        if FUseDirOnlyEntries or (ExtractFileName(pzd^.FileName) <> '') then
        begin                 // Add it to our contents tabel.
          ZipContents.Add(pzd);
          // Notify user, when needed, of the next entry in the ZipDir.
          if Assigned(FOnNewName) then
            FOnNewName(self, i + 1, pzd^);
        end
        else
        begin
          Inc(FDirOnlyCount);
          pzd^.ExtraData := '';
          Dispose(pzd);
        end;

        // Calculate the earliest Local Header start
        if Longword(FSFXOffset) > CEH.RelOffLocal then
          FSFXOffset := CEH.RelOffLocal;
      end;
      FTotalDisks := EOC.ThisDiskNo; // We need this when we are going to extract.
    except
      on ezl: EZipMaster do   // Catch all Zip List specific errors.
      begin
        ShowExceptionError(ezl);
        LiE := 1;
      end;
      on EOutOfMemory do
      begin
        ShowZipMessage(GE_NoMem, '');
        LiE := 1;
      end;
      on E: Exception do
      begin
        // the error message of an unknown error is displayed ...
        ShowZipMessage(LI_ErrorUnknown, E.Message);
        LiE := 1;
      end;
    end;
  finally
    StopWaitCursor;
    if FInFileHandle <> -1 then
      FileClose(FInFileHandle);
    FInFileHandle := -1;
    if LiE = 1 then
    begin
      FZipFileName := '';
      FSFXOffset := 0;
    end
    else
      FSFXOffset := FSFXOffset + (OffsetDiff - Integer(EOC.CentralOffset)); // Correct the offset for v1.3 and 1.4x

    // Let the user's program know we just refreshed the zip dir contents.
    if Assigned(FOnDirUpdate) then
      FOnDirUpdate(Self);
  end;
end;
//? TZipMaster._List

(*?  TZipMaster.GetNewDisk
1.73 12 July 2003 RA clear file handle, change loop
*)

procedure TZipMaster.GetNewDisk(DiskSeq: Integer);
begin
{$IFDEF NO_SPAN}
  raise EZipMaster.CreateResDisp(DS_NODISKSPAN, True);
{$ELSE}
  FileClose(FInFileHandle);   // Close the file on the old disk first.
  FDiskNr := DiskSeq;
  repeat
    if FInFileHandle = -1 then
    begin
      if FDriveFixed then
        raise EZipMaster.CreateResDisp(DS_NoInFile, True)
      else
        ShowZipMessage(DS_NoInFile, '');
      { This prevents and endless loop if for some reason spanned parts
         on harddisk are missing.}
    end;
    repeat
      FNewDisk := True;
      FInFileHandle := -1;    // 173 mark closed
      CheckForDisk(false);
    until IsRightDisk;

    if Verbose then
      Callback(zacMessage, 0, 'Trace : GetNewDisk Opening ' + FInFileName, 0);
    // Open the the input archive on this disk.
    FInFileHandle := FileOpen(FInFileName, fmShareDenyWrite or fmOpenRead);
  until not (FInFileHandle = -1);
{$ENDIF}
end;
//? TZipMaster.GetNewDisk

(*? TZipMaster.ExtExtract
1.73.2.6 17 September 2003 RA stop duplicate 'cannot open file' messages
1.73 22 July 2003 RA exception handling for EZipMaster + fUnzBusy := False when dll load error
1.73 16 July 2003 RA catch and display dll load errors
1.73 12 July 2003 RP allow ForceDirectories
// UseStream = 0 ==> Extract file from zip archive file.
// UseStream = 1 ==> Extract stream from zip archive file.
// UseStream = 2 ==> Extract (zipped) stream from another stream.
*)

procedure TZipMaster.ExtExtract(UseStream: Integer; MemStream: TMemoryStream);
var
  i, UnzDLLVers: Integer;
	OldPRC    : Integer;
  TmpZipName: string;
  pUFDS     : pUnzFileData;
{$IFNDEF NO_SPAN}
  NewName   : array[0..512] of Char;
{$ENDIF}
begin
  FSuccessCnt := 0;
  FErrCode := 0;
  FMessage := '';
  OldPRC := FPasswordReqCount;

  if (UseStream < 2) then
  begin
    if (FZipFileName = '') then
    begin
      ShowZipMessage(GE_NoZipSpecified, '');
      Exit;
    end;
    if (not FileExists(FZipFileName)) then
    begin
      ShowZipMessage(DS_NoInFile, '');
      Exit;
    end;
    if Count = 0 then
      _List;                  // try again
    if Count = 0 then
		begin
			if FErrCode=0 then	// only show once
				ShowZipMessage(DS_FileOpen, '');
      exit;
    end;
  end;

  { Make sure we can't get back in here while work is going on }
  if fUnzBusy then
    Exit;

  // We have to be carefull doing an unattended Extract when a password is needed
   // for some file in the archive.
  if FUnattended and (FPassword = '') and not Assigned(FOnPasswordError) then
  begin
    FPasswordReqCount := 0;
    ShowZipMessage(EX_UnAttPassword, '');
  end;

  Cancel := False;
  fUnzBusy := True;

  // We do a check if we need UnSpanning first, this depends on
  // The number of the disk the EOC record was found on. ( provided by List() )
  // If we have a spanned set consisting of only one disk we don't use ReadSpan().
  if FTotalDisks <> 0 then
  begin
{$IFDEF NO_SPAN}
    fUnzBusy := False;
    ShowZipMessage(DS_NODISKSPAN, '');
    exit;
{$ELSE}
    if FTempDir = '' then
    begin
      GetTempPath(MAX_PATH, NewName);
      TmpZipName := NewName;
    end
    else
      TmpZipName := AppendSlash(FTempDir);
    if ReadSpan(FZipFileName, TmpZipName) <> 0 then
    begin
			fUnzBusy := False;
      Exit;
    end;
    // We returned without an error, now  TmpZipName contains a real name.
{$ENDIF}
  end
  else
    TmpZipName := FZipFileName;
  try
    UnzDLLVers := FUnzDll.LoadDll(Min_UnzDll_Vers, false);
  except
    on ews: EZipMaster do
    begin
      ShowExceptionError(ews);
      UnzDLLVers := 0;
    end;
  end;
  if UnzDllVers = 0 then
  begin
    FUnzBusy := false;
    exit;                     // could not load valid DLL
  end;
  try
    try
      UnZipParms := AllocMem(SizeOf(UnZipParms2));
      SetUnZipSwitches(TmpZipName, UnzDLLVers);

      with UnzipParms^ do
      begin
        if ExtrBaseDir <> '' then
        begin
          if ExtrForceDirs in FExtrOptions then
            ForceDirectory(ExtrBaseDir);
          if not DirExists(ExtrBaseDir) then
            raise EZipMaster.CreateResDrive(EX_NoExtrDir, ExtrBaseDir);
          fExtractDir := StrAlloc(Length(fExtrBaseDir) + 1);
          StrPLCopy(fExtractDir, fExtrBaseDir, Length(fExtrBaseDir));
        end
        else
          fExtractDir := nil;

        fUFDS := AllocMem(SizeOf(UnzFileData) * FFSpecArgs.Count);
        // 1.70 added test - speeds up extract all
        if (fFSpecArgs.Count <> 0) and (fFSpecArgs[0] <> '*.*') then
        begin
          for i := 0 to (fFSpecArgs.Count - 1) do
          begin
            pUFDS := fUFDS;
            Inc(pUFDS, i);
            pUFDS.fFileSpec := StrAlloc(Length(fFSpecArgs[i]) + 1);
						StrPLCopy(pUFDS.fFileSpec, fFSpecArgs[i], Length(fFSpecArgs[i]) + 1);
          end;
          fArgc := FFSpecArgs.Count;
        end
        else
          fArgc := 0;
        if UseStream = 1 then
        begin
          for i := 0 to Count - 1 do { Find the wanted file in the ZipDirEntry list. }
          begin
            with ZipDirEntry(ZipContents[i]^) do
            begin
              if AnsiStrIComp(pChar(FFSpecArgs.Strings[0]), pChar(FileName)) = 0
                then          { Found? }
              begin
                FZipStream.SetSize(UncompressedSize);
                fUseOutStream := True;
                fOutStream := FZipStream.Memory;
                fOutStreamSize := UncompressedSize;
                fArgc := 1;
                Break;
              end;
            end;
          end;
        end;
        if UseStream = 2 then
        begin
          fUseInStream := True;
          fInStream := MemStream.Memory;
          fInStreamSize := MemStream.Size;
          fUseOutStream := True;
          fOutStream := FZipStream.Memory;
          fOutStreamSize := FZipStream.Size;
        end;
        fSeven := 7;
      end;
      FEventErr := '';        // added
      { Argc is now the no. of filespecs we want extracted }
      if (UseStream = 0) or ((UseStream > 0) and UnZipParms.fUseOutStream) then
      begin
        fSuccessCnt := fUnzDLL.Exec(Pointer(UnZipParms));
        if fSuccessCnt < 0 then
        begin
          fSuccessCnt := 0;
          ShowZipMessage(GE_FatalZip, 'fatal Dll error');
        end;
      end;
      { Remove from memory if stream is not Ok. }
      if (UseStream > 0) and (FSuccessCnt <> 1) then
        FZipStream.Clear();
      { If UnSpanned we still have this temporary file hanging around. }
      if FTotalDisks > 0 then
        DeleteFile(TmpZipName);
    except
      on ezl: EZipMaster do
        ShowExceptionError(ezl)
    else
      ShowZipMessage(EX_FatalUnZip, '');
    end;
  finally
    fFSpecArgs.Clear;
    with UnZipParms^ do
    begin
      StrDispose(pZipFN);
      StrDispose(pZipPassword);
      if (fExtractDir <> nil) then
        StrDispose(fExtractDir);

      for i := (fArgc - 1) downto 0 do
      begin
        pUFDS := fUFDS;
        Inc(pUFDS, i);
        StrDispose(pUFDS.fFileSpec);
      end;
      FreeMem(fUFDS);
    end;
    FreeMem(UnZipParms);

    UnZipParms := nil;
  end;

  if FUnattended and (FPassword = '') and not Assigned(FOnPasswordError) then
    FPasswordReqCount := OldPRC;

	FUnzDll.Unload(false);
  Cancel := False;
  fUnzBusy := False;
  { no need to call the List method; contents unchanged }
end;
//? TZipMaster.ExtExtract

(*? TZipMaster.CopyZippedFiles
1.73.2.8 2 Oct 2003 RA fix slash
1.73  1 August 2003 RA close file or error
1.73 24 July 2003 RA init OutFileHandle
1.73 13 July 2003 RA removed second OpenEOC
1.73 12 July 2003 RP string extra data
// Function to copy one or more zipped files from the zip archive to another zip archive
// FSpecArgs in source is used to hold the filename(s) to be copied.
// When this function is ready FSpecArgs contains the file(s) that where not copied.
// Return values:
// 0            All Ok.
// -6           CopyZippedFiles Busy
// -7           CopyZippedFiles errors. See ZipMsgXX.rc
// -8           Memory allocation error.
// -9           General unknown CopyZippedFiles error.
*)

function TZipMaster.CopyZippedFiles(DestZipMaster: TZipMaster; DeleteFromSource:
  boolean; OverwriteDest: OvrOpts): Integer;
var
  EOC       : ZipEndOfCentral;
  CEH       : ZipCentralHeader;
  OutFilePath: string;
  In2FileHandle: Integer;
  Found, Overwrite: Boolean;
  DestMemCount: Integer;
  NotCopiedFiles: TStringList;
  pzd, zde  : pZipDirEntry;
  s, d      : Integer;
  MDZD      : TMZipDataList;
  MDZDp     : pMZipData;
begin
  if Busy then
  begin
    Result := BUSY_ERROR;
    Exit;
  end;
  fZipBusy := True;
  FShowProgress := False;
  NotCopiedFiles := nil;
  Result := 0;
  In2FileHandle := -1;
  FOutFileHandle := -1;
  MDZD := nil;

  StartWaitCursor;
  try
    // Are source and destination different?
    if (DestZipMaster = self) or (AnsiStrIComp(pChar(ZipFileName),
      pChar(DestZipMaster.ZipFileName)) = 0) then
      raise EZipMaster.CreateResDisp(CF_SourceIsDest, True);
    // The following function a.o. open the input file no. 1.
        // new 1.7 - stop attempt to copy spanned file
    OpenEOC(EOC, True);
    if (DestZipMaster.IsSpanned or IsSpanned) then
    begin
      if FInFileHandle <> -1 then
        FileClose(FInFileHandle);
      FInFileHandle := -1;
      raise EZipMaster.CreateResDisp(CF_NoCopyOnSpan, True);
    end;
    // Now check for every source file if it is in the destination archive and determine what to do.
   // we use the three most significant bits from the Flag field from ZipDirEntry to specify the action
          // None           = 000xxxxx, Destination no change. Action: Copy old Dest to New Dest
          // Add            = 001xxxxx (New).                  Action: Copy Source to New Dest
          // Overwrite      = 010xxxxx (OvrAlways)             Action: Copy Source to New Dest
          // AskToOverwrite = 011xxxxx (OvrConfirm)	Action to perform: Overwrite or NeverOverwrite
          // NeverOverwrite = 100xxxxx (OvrNever)				  Action: Copy old Dest to New Dest
    for s := 0 to FSpecArgs.Count - 1 do
    begin
      Found := False;
      for d := 0 to DestZipMaster.Count - 1 do
      begin
        zde := pZipDirEntry(DestZipMaster.ZipContents.Items[d]);
        if AnsiStrIComp(pChar(FSpecArgs.Strings[s]), pChar(zde^.FileName)) = 0
          then
        begin
          Found := True;
          zde^.Flag := zde^.Flag and $1FFF; // Clear the three upper bits.
          if OverwriteDest = OvrAlways then
            zde^.Flag := zde^.Flag or $4000
          else
            if OverwriteDest = OvrNever then
              zde^.Flag := zde^.Flag or $8000
            else
              zde^.Flag := zde^.Flag or $6000;
          Break;
        end;
      end;
      if not Found then
      begin                   // Add the Filename to the list and set flag
        New(zde);
        DestZipMaster.ZipContents.Add(zde);
        zde^.FileName := FSpecArgs.Strings[s];
        zde^.FileNameLength := Length(FSpecArgs.Strings[s]);
        zde^.Flag := zde^.Flag or $2000; // (a new entry)
        //				zde^.ExtraData := nil;          // Needed when deleting zde
        zde^.ExtraData := ''; // Needed when deleting zde
      end;
    end;
    // Make a temporary filename like: C:\...\zipxxxx.zip for the new destination
    OutFilePath := MakeTempFileName('', '');
    if OutFilePath = '' then
      raise EZipMaster.CreateResDisp(DS_NoTempFile, True);

    // Create the output file.
    FOutFileHandle := FileCreate(OutFilePath);
    if FOutFileHandle = -1 then
      raise EZipMaster.CreateResDisp(DS_NoOutFile, True);

    // Open the second input archive, i.e. the original destination.
    In2FileHandle := FileOpen(DestZipMaster.ZipFileName, fmShareDenyWrite or
      fmOpenRead);
    if In2FileHAndle = -1 then
      raise EZipMaster.CreateResDisp(CF_DestFileNoOpen, True);

    // Get the date-time stamp and save for later.
    FDateStamp := FileGetDate(In2FileHandle);

    // Write the SFX header if present.
    if CopyBuffer(In2FileHandle, FOutFileHandle, DestZipMaster.SFXOffset) <> 0
      then
      raise EZipMaster.CreateResDisp(CF_SFXCopyError, True);

    NotCopiedFiles := TStringList.Create();
    // Now walk trough the destination, copying and replacing
    DestMemCount := DestZipMaster.ZipContents.Count;

    MDZD := TMZipDataList.Create(DestMemCount);

    // Copy the local data and save central header info for later use.
    for d := 0 to DestMemCount - 1 do
    begin
      zde := pZipDirEntry(DestZipMaster.ZipContents.Items[d]);
      if (zde^.Flag and $E000) = $6000 then // Ask first if we may overwrite.
      begin
        Overwrite := False;
        // Do we have a event assigned for this then don't ask.
        if Assigned(FOnCopyZipOverwrite) then
          FOnCopyZipOverwrite(DestZipMaster, zde^.FileName, Overwrite)
        else
          if MessageBox(Handle, pChar(Format(LoadZipStr(CF_OverwriteYN,
            'Overwrite %s in %s ?'), [zde^.FileName, DestZipMaster.ZipFileName])),
            pChar(Application.Title), MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2)
            = IDYES then
            Overwrite := True;
        zde^.Flag := zde^.Flag and $1FFF; // Clear the three upper bits.
        if Overwrite then
          zde^.Flag := zde^.Flag or $4000
        else
          zde^.Flag := zde^.Flag or $8000;
      end;
      // Change info for later while writing the central dir in new Dest.
      MDZDp := MDZD[d];
      MDZDp^.RelOffLocal := FileSeek(FOutFileHandle, 0, 1);

      if (zde^.Flag and $6000) = $0000 then // Copy from original dest to new dest.
      begin
        // Set the file pointer to the start of the local header.
        FileSeek(In2FileHandle, zde^.RelOffLocalHdr, 0);
        if CopyBuffer(In2FileHandle, FOutFileHandle, SizeOf(ZipLocalHeader) +
          zde^.FileNameLength + zde^.ExtraFieldLength + zde^.CompressedSize) <> 0
          then
          raise EZipMaster.CreateResFile(CF_CopyFailed,
            DestZipMaster.ZipFileName, DestZipMaster.ZipFileName);
        if zde^.Flag and $8000 <> 0 then
        begin
          NotCopiedFiles.Add(zde^.FileName);
          // Delete also from FSpecArgs, should not be deleted from source later.
          FSpecArgs.Delete(FSpecArgs.IndexOf(zde^.FileName));
        end;
      end
      else
      begin                   // Copy from source to new dest.
        // Find the filename in the source archive and position the file pointer.
        for s := 0 to Count - 1 do
        begin
          pzd := pZipDirEntry(ZipContents.Items[s]);
          if AnsiStrIComp(pChar(pzd^.FileName), pChar(zde^.FileName)) = 0 then
          begin
            FileSeek(FInFileHandle, pzd^.RelOffLocalHdr, 0);
            if CopyBuffer(FInFileHandle, FOutFileHandle, SizeOf(ZipLocalHeader)
              + pzd^.FileNameLength + pzd^.ExtraFieldLength + pzd^.CompressedSize)
              <> 0 then
              raise EZipMaster.CreateResFile(CF_CopyFailed, ZipFileName,
                DestZipMaster.ZipFileName);
            Break;
          end;
        end;
      end;
      // Save the file name info in the MDZD structure.
      MDZDp^.FileNameLen := zde^.FileNameLength;
      StrPLCopy(MDZDp^.FileName, zde^.FileName, zde^.FileNameLength);
    end;                      // Now we have written al entries.

    // Now write the central directory with possibly changed offsets.
    // Remember the EOC we are going to use is from the wrong input file!
    EOC.CentralSize := 0;
    for d := 0 to DestMemCount - 1 do
    begin
      zde := pZipDirEntry(DestZipMaster.ZipContents.Items[d]);
      pzd := nil;
      Found := False;
      // Rebuild the CEH structure.
      if (zde^.Flag and $6000) = $0000 then // Copy from original dest to new dest.
      begin
        pzd := pZipDirEntry(DestZipMaster.ZipContents.Items[d]);
        Found := True;
      end
      else                    // Copy from source to new dest.
      begin
        // Find the filename in the source archive and position the file pointer.
        for s := 0 to Count - 1 do
        begin
          pzd := pZipDirEntry(ZipContents.Items[s]);
          if AnsiStrIComp(pChar(pzd^.FileName), pChar(zde^.FileName)) = 0 then
          begin
            Found := True;
            Break;
          end;
        end;
      end;
      if not Found then
        raise EZipMaster.CreateResFile(CF_SourceNotFound, zde^.FileName,
          ZipFileName);
      CopyMemory(@CEH.VersionMadeBy0, pzd, SizeOf(ZipCentralHeader) - 4);
      CEH.HeaderSig := CentralFileHeaderSig;
      CEH.Flag := CEH.Flag and $1FFF;
      MDZDp := MDZD[d];
      CEH.RelOffLocal := MDZDp^.RelOffLocal;
      // Save the first Central directory offset for use in EOC record.
      if d = 0 then
        EOC.CentralOffset := FileSeek(FOutFileHandle, 0, 1);
      EOC.CentralSize := EOC.CentralSize + SizeOf(CEH) + CEH.FileNameLen +
        CEH.ExtraLen + CEH.FileComLen;

      // Write this changed central header to disk
      WriteJoin(@CEH, SizeOf(CEH), DS_CEHBadWrite);
      //if filename was converted OEM2ISO then we have to reconvert before copying
      FVersionMadeBy1 := CEH.VersionMadeBy1;
      FVersionMadeBy0 := CEH.VersionMadeBy0;
			StrCopy(MDZDp^.FileName, pChar(SetSlash(ConvertOEM(MDZDp^.FileName, cpdISO2OEM),true)));

      // Write to destination the central filename.
      WriteJoin(MDZDp^.FileName, CEH.FileNameLen, DS_CEHBadWrite);

      // And the extra field from zde or pzd.
      if CEH.ExtraLen <> 0 then
        WriteJoin(pChar(pzd^.ExtraData), CEH.ExtraLen, DS_CEExtraLen);

      // And the file comment.
      if CEH.FileComLen <> 0 then
        WriteJoin(pChar(pzd^.FileComment), CEH.FileComLen, DS_CECommentLen);
    end;
    EOC.CentralEntries := DestMemCount;
    EOC.TotalEntries := EOC.CentralEntries;
    EOC.ZipCommentLen := Length(DestZipMaster.ZipComment);

    // Write the changed EndOfCentral directory record.
    WriteJoin(@EOC, SizeOf(EOC), DS_EOCBadWrite);

    // And finally the archive comment
    FileSeek(In2FileHandle, DestZipMaster.ZipEOC + SizeOf(EOC), 0);
    if CopyBuffer(In2FileHandle, FOutFileHandle,
      Length(DestZipMaster.ZipComment)) <> 0 then
      raise EZipMaster.CreateResDisp(DS_EOArchComLen, True);

    if FInFileHandle <> -1 then
      FileClose(FInFileHandle);
    // Now delete all copied files from the source when deletion is wanted.
    if DeleteFromSource and (FSpecArgs.Count > 0) then
    begin
      fZipBusy := False;
      Delete();               // Delete files specified in FSpecArgs and update the contents.
    end;
    FSpecArgs.Assign(NotCopiedFiles); // Info for the caller.
  except
    on ers: EZipMaster do     // All CopyZippedFiles specific errors..
    begin
      ShowExceptionError(ers);
      Result := -7;
    end;
    on EOutOfMemory do        // All memory allocation errors.
    begin
      ShowZipMessage(GE_NoMem, '');
      Result := -8;
    end;
    on E: Exception do
    begin
      ShowZipMessage(DS_ErrorUnknown, E.Message);
      Result := -9;
    end;
  end;

  if Assigned(MDZD) then
    MDZD.Free;
  NotCopiedFiles.Free;

  if In2FileHandle <> -1 then
    FileClose(In2FileHandle);
  if FOutFileHandle <> -1 then
  begin
    FileSetDate(FOutFileHandle, FDateStamp);
    FileClose(FOutFileHandle);
    if Result <> 0 then       // An error somewhere, OutFile is not reliable.
      DeleteFile(OutFilePath)
    else
    begin
      EraseFile(DestZipMaster.FZipFileName, DestZipMaster.HowToDelete);
      if not RenameFile(OutFilePath, DestZipMaster.FZipFileName) then
        EraseFile(OutFilePath, DestZipMaster.HowToDelete);
    end;
  end;
  DestZipMaster.List;         // Update the old(possibly some entries were added temporarily) or new destination.
  StopWaitCursor;
  fZipBusy := False;
end;
//? TZipMaster.CopyZippedFiles

(*? TZipMaster.FreeZipDirEntryRecords
1.73 12 July 2003 RP string ExtraData
{ Empty fZipContents and free the storage used for dir entries }
*)

procedure TZipMaster.FreeZipDirEntryRecords;
var
  i         : Integer;
begin
  if ZipContents.Count = 0 then
    Exit;
  for i := (ZipContents.Count - 1) downto 0 do
  begin
    if Assigned(ZipContents[i]) then
		begin
      pZipDirEntry(ZipContents[i]).ExtraData := '';
      // dispose of the memory pointed-to by this entry
      Dispose(pZipDirEntry(ZipContents[i]));
    end;
    ZipContents.Delete(i);    // delete the TList pointer itself
  end;                        { end for }
  // The caller will free the FZipContents TList itself, if needed
end;
//? TZipMaster.FreeZipDirEntryRecords

(*? TZipMaster.ExtAdd  ----------------------------------------------------
1.73.2.6 7 September 2003 RP allow Freshen/Update with no args
1.73  4 August 2003 RA fix removal of '< '
1.73 17 July 2003 RP reject '< ' as password ' '
1.73 16 July 2003 RA load Dll in try except
1.73 16 July 2003 RP trim filenames
1.73 15 July 2003 RA remove '<' from filename
1.73 12 July 2003 RP release held File Data  + test destination drive
1.73 27 June 2003 RP changed slplit file support
// UseStream = 0 ==> Add file to zip archive file.
// UseStream = 1 ==> Add stream to zip archive file.
// UseStream = 2 ==> Add stream to another (zipped) stream.
*)

procedure TZipMaster.ExtAdd(UseStream: Integer; StrFileDate, StrFileAttr: DWORD;
  MemStream: TMemoryStream);
var
  i, DLLVers: Integer;
{$IFNDEF NO_SFX}
  SFXResult : Integer;
{$ENDIF}
  Tmp, TmpZipName: string;
  pFDS      : pFileData;
  pExFiles  : pExcludedFileSpec;
  len, b, p, RootLen: Integer;
  rdir      : string;
begin
  FSuccessCnt := 0;
  {	if (UseStream = 0) and (fFSpecArgs.Count = 0) then
   begin
    ShowZipMessage(AD_NothingToZip, '');
    Exit;
   end;      }
  if (UseStream = 0) and (fFSpecArgs.Count = 0) then
  begin
    if AddUpdate in FAddOptions then
      FAddOptions := FAddOptions - [AddUpdate] + [AddFreshen];
    if AddFreshen in FAddOptions then
      fFSpecArgs.Add('*.*')
    else
    begin
      ShowZipMessage(AD_NothingToZip, '');
      Exit;
    end;
  end;
  if (AddDiskSpanErase in FAddOptions) then
  begin
    FAddOptions := FAddOptions + [AddDiskSpan]; // make certain set
    FSpanOptions := FSpanOptions + [spWipeFiles];
  end;
{$IFDEF NO_SPAN}
  if (AddDiskSpan in FAddOptions) then
  begin
    ShowZipMessage(DS_NODISKSPAN, '');
    Exit;
  end;
{$ENDIF}
  { We must allow a zipfile to be specified that doesn't already exist,
    so don't check here for existance. }
  if (UseStream < 2) and (FZipFileName = '') then { make sure we have a zip filename }
  begin
    ShowZipMessage(GE_NoZipSpecified, '');
    Exit;
  end;
  // We can not do an Unattended Add if we don't have a password.
  if FUnattended and (AddEncrypt in FAddOptions) and (FPassword = '') then
  begin
    ShowZipMessage(AD_UnattPassword, '');
    Exit
  end;

  // If we are using disk spanning, first create a temporary file
  if (UseStream < 2) and (AddDiskSpan in FAddOptions) then
  begin
{$IFDEF NO_SPAN}
    ShowZipMessage(DS_NoDiskSpan, '');
    exit;
{$ELSE}
    // We can't do this type of Add() on a spanned archive.
    if (AddFreshen in FAddOptions) or (AddUpdate in FAddOptions) then
    begin
      ShowZipMessage(AD_NoFreshenUpdate, '');
      Exit;
    end;
    // We can't make a spanned SFX archive
    if (UpperCase(ExtractFileExt(FZipFileName)) = '.EXE') then
    begin
      ShowZipMessage(DS_NoSFXSpan, '');
      Exit;
    end;
    TmpZipName := MakeTempFileName('', '');

    if FVerbose and Assigned(FOnMessage) then
      FOnMessage(Self, 0, 'Temporary zipfile: ' + TmpZipName);
{$ENDIF}
  end
  else
    TmpZipName := FZipFileName; // not spanned - create the outfile directly

  { Make sure we can't get back in here while work is going on }
  if fZipBusy then
    Exit;

  if (UseStream < 2) and (Uppercase(ExtractFileExt(FZipFileName)) = '.EXE')
    and (FSFXOffset = 0) and not FileExists(FZipFileName) then
  begin
{$IFDEF NO_SFX}
    ShowZipMessage(SF_NOSFXSUPPORT, '');
    exit;
{$ELSE}
    try
      { This is the first "add" operation following creation of a new
        .EXE archive.  We need to add the SFX code now, before we add
        the files. }
      AutoExeViaAdd := True;
      SFXResult := NewSFXFile(FZipFileName); //1.72x ConvertSFX;
      AutoExeViaAdd := False;
      if SFXResult <> 0 then
        raise EZipMaster.CreateResDisk(AD_AutoSFXWrong, SFXResult);
    except
      on ews: EZipMaster do   // All SFX creation errors will be caught and returned in this one message.
      begin
        ShowExceptionError(ews);
        Exit;
      end;
    end;
{$ENDIF}
  end;

  try
    DLLVers := FZipDll.LoadDll(Min_ZipDll_Vers, false); //Load_ZipDll(AutoLoad);
  except
    on ews: EZipMaster do
    begin
      ShowExceptionError(ews);
      exit;
    end;
  end;
  if DLLVers = 0 then
    exit;                     // could not load valid dll

  fZipBusy := True;
  Cancel := False;

  try
    try
      ZipParms := AllocMem(SizeOf(ZipParms2));
      SetZipSwitches(TmpZipName, DLLVers);

      // make certain destination can exist
      if (UseStream < 2) then
      begin
        if AddForceDest in FAddOptions then
          ForceDirectory(ExtractFilePath(FZipFileName));
        if not DirExists(ExtractFilePath(FZipFileName)) then
          raise EZipMaster.CreateResDrive(AD_NoDestDir, ExtractFilePath(FZipFileName));
      end;

      with ZipParms^ do
      begin
        if UseStream = 1 then
        begin
          fUseInStream := True;
          fInStream := FZipStream.Memory;
          fInStreamSize := FZipStream.Size;
          fStrFileAttr := StrFileAttr;
          fStrFileDate := StrFileDate;
        end;
        if UseStream = 2 then
        begin
          fUseOutStream := True;
          fOutStream := FZipStream.Memory;
          fOutStreamSize := MemStream.Size + 6;
          fUseInStream := True;
          fInStream := MemStream.Memory;
          fInStreamSize := MemStream.Size;
        end;
        fFDS := AllocMem(SizeOf(FileData) * FFSpecArgs.Count);
        for i := 0 to (fFSpecArgs.Count - 1) do
        begin
          len := Length(FFSpecArgs.Strings[i]);
          p := 1;
          pFDS := fFDS;
          Inc(pFDS, i);

          // Added to version 1.60L to support recursion and encryption on a FFileSpec basis.
          // Regardless of what AddRecurseDirs is set to, a '>' will force recursion, and a '|' will stop recursion.
          pFDS.fRecurse := Word(fRecurse); // Set default
          if Copy(FFSpecArgs.Strings[i], 1, 1) = '>' then
          begin
            pFDS.fRecurse := $FFFF;
            Inc(p);
          end;
          if Copy(FFSpecArgs.Strings[i], 1, 1) = '|' then
          begin
            pFDS.fRecurse := 0;
            Inc(p);
          end;

          // Also it is possible to specify a password after the FFileSpec, separated by a '<'
          // If there is no other text after the '<' then, an existing password, is temporarily canceled.
          pFDS.fEncrypt := LongWord(fEncrypt); // Set default
          if Length(pZipPassword) > 0 then // v1.60L
          begin
            pFDS.fPassword := StrAlloc(Length(pZipPassword) + 1);
            StrLCopy(pFDS.fPassword, pZipPassword, Length(pZipPassword));
          end;
          b := AnsiPos('<', FFSpecArgs.Strings[i]);
          if b <> 0 then
          begin               // Found...
            pFDS.fEncrypt := $FFFF; // the new default, but...
            StrDispose(pFDS.fPassword);
            pFDS.fPassword := nil;
            //						if Copy(FFSpecArgs.Strings[i], b + 1, 1) = '' then
            tmp := Copy(FFSpecArgs.Strings[i], b + 1, 1);
            if (tmp = '') or (tmp = ' ') then
            begin
              pFDS.fEncrypt := 0; // No password, so cancel for this FFspecArg
              dec(len, Length(tmp));
            end
            else
            begin
              pFDS.fPassword := StrAlloc(len - b + 1);
              StrPLCopy(pFDS.fPassword, Copy(FFSpecArgs.Strings[i], b + 1, len -
                b), len - b + 1);
              len := b - 1;
            end;
          end;

          // And to set the RootDir, possibly later with override per FSpecArg v1.70
          if RootDir <> '' then
          begin
            rdir := ExpandFileName(fRootDir); // allow relative root
            RootLen := Length(rdir);
            pFDS.fRootDir := StrAlloc(RootLen + 1);
            StrPLCopy(pFDS.fRootDir, rdir, RootLen + 1);
					end;
          tmp := Trim(Copy(FFSpecArgs.Strings[i], p, len - p + 1));
          pFDS.fFileSpec := StrAlloc(Length(tmp) + 1);
          StrPLCopy(pFDS.fFileSpec, tmp, Length(tmp) + 1);
        end;
        fSeven := 7;
      end;                    { end with }

      ZipParms.argc := fSpecArgs.Count;
      FEventErr := '';        // added
      { pass in a ptr to parms }
      fSuccessCnt := FZipDLL.Exec(ZipParms);
      if fSuccessCnt < 0 then
      begin
        fSuccessCnt := 0;
        ShowZipMessage(GE_FatalZip, 'fatal Dll error');
      end;
      // If Add was successful and we want spanning, copy the
      // temporary file to the destination.
      if (UseStream < 2) and (fSuccessCnt > 0) and
        (AddDiskSpan in FAddOptions) then
{$IFDEF NO_SPAN}
        raise EZipMaster.CreateResDisp(DS_NODISKSPAN, true);
{$ELSE}
      begin
        // write the temp zipfile to the right target:
        if WriteSpan(TmpZipName, FZipFileName) <> 0 then
          fSuccessCnt := 0;   // error occurred during write span
        DeleteFile(TmpZipName);
      end;
{$ENDIF}
      if (UseStream = 2) and (FSuccessCnt = 1) then
        FZipStream.SetSize(ZipParms.fOutStreamSize);
    except
      ShowZipMessage(GE_FatalZip, '');
    end;
  finally
    fFSpecArgs.Clear;
    fFSpecArgsExcl.Clear;
    with ZipParms^ do
    begin
      { Free the memory for the zipfilename and parameters }
      { we know we had a filename, so we'll dispose it's space }
      StrDispose(pZipFN);
      StrDispose(pZipPassword);
      StrDispose(pSuffix);
      pZipPassword := nil;    // v1.60L

      StrDispose(fTempPath);
      StrDispose(fArchComment);
      for i := (Argc - 1) downto 0 do
      begin
        pFDS := fFDS;
        Inc(pFDS, i);
        StrDispose(pFDS.fFileSpec);
        StrDispose(pFDS.fPassword); // v1.60L
        StrDispose(pFDS.fRootDir); // v1.60L
      end;
      FreeMem(fFDS);
      for i := (fTotExFileSpecs - 1) downto 0 do
      begin
        pExFiles := fExFiles;
        Inc(pExFiles, i);
        StrDispose(pExFiles.fFileSpec);
      end;
      FreeMem(fExFiles);
    end;
    FreeMem(ZipParms);
    ZipParms := nil;
  end;                        {end try finally }

	FZipDll.Unload(false);

  FStoredExtraData := '';     // release held data
  Cancel := False;
  fZipBusy := False;
  if fSuccessCnt > 0 then
    _List;                    { Update the Zip Directory by calling List method }
end;
//? TZipMaster.ExtAdd

(*? ForceDirectory
1.73 RP utilities
*)

function ForceDirectory(const Dir: string): Boolean;
var
  sDir      : string;
begin
  Result := true;
  if Dir <> '' then
  begin
    sDir := DelimitPath(Dir, false);
    if DirExists(sDir) or (ExtractFilePath(sDir) = sDir) then
      exit;                   // avoid 'c:\xyz:\' problem.
    if ForceDirectory(ExtractFilePath(sDir)) then
      Result := CreateDirectory(pChar(sDir), nil)
    else
      Result := false;
  end;
end;
//? ForceDirectory

(*? DirExists
1.73 12 July 2003 return true empty string (current directory)
*)

function DirExists(const Fname: string): boolean;
var
  Code      : DWORD;
begin
  Result := true;             // current directory exists
  if Fname <> '' then
  begin
    Code := GetFileAttributes(pChar(Fname));
    Result := (Code <> $FFFFFFFF) and ((FILE_ATTRIBUTE_DIRECTORY and Code) <> 0);
  end;
end;
//? DirExists

(*? TZipMaster.ShowExceptionError
1.73 10 July 2003 RP translate exception messages (again)
// Somewhat different from ShowZipMessage() because the loading of the resource
// string is already done in the constructor of the exception class.
*)

procedure TZipMaster.ShowExceptionError(const ZMExcept: EZipMaster);
var
  Msg       : string;
  int       : integer;
begin
  with ZMExcept do
  begin
    ErrCode := FResIdent;
    Msg := ZipStr(FResIdent);
    if Msg = '' then
      Msg := RESOURCE_ERROR + IntToStr(FResIdent)
    else
    begin
      if Arg1 = '' then       // no args or DiskNo
      begin
        if Arg2 <> '' then
        begin
          int := StrToIntDef(Arg2, 0);
          Msg := Format(Msg, [Int]);
        end;
      end
      else
        Msg := Format(Msg, [Arg1, Arg2]);
    end;
  end;
  if (ZMExcept.FDisplayMsg = True) and (Unattended = False) then
    ShowMessage(Msg);

  //	ErrCode := FResIdent;
  FMessage := Msg;

  if Assigned(OnMessage) then
    OnMessage(Self, ErrCode {0}, Msg); // 1.72
end;
//? TZipMaster.ShowExceptionError

(*? TZipMaster.LoadZipStr
1.73 5 July 2003 - changed to use global function
*)

function TZipMaster.LoadZipStr(Ident: Integer; DefaultStr: string): string;
begin
  Result := ZipStr(Ident);

  if Result = '' then
    Result := DefaultStr;
end;
//? TZipMaster.LoadZipStr

(*? TZipMaster.ShowZipMessage
*)

procedure TZipMaster.ShowZipMessage(Ident: Integer; UserStr: string);
var
  Msg       : string;
begin
  //    Msg := LoadZipStr(Ident, RESOURCE_ERROR + IntToStr(Ident)) + UserStr;
  Msg := ZipStr(Ident);
  if Msg = '' then
    Msg := RESOURCE_ERROR + IntToStr(Ident) + ' ' + UserStr;
  Msg := Msg + UserStr;
  FMessage := Msg;
  ErrCode := Ident;

  if FUnattended = False then
    ShowMessage(Msg);

  if Assigned(OnMessage) then
    OnMessage(Self, 0, Msg);  // No ErrCode here else w'll get a msg from the application
end;
//? TZipMaster.ShowZipMessage

(*? TZipMaster.ZipStr
*)

function TZipMaster.ZipStr(id: integer): string;
begin
  Result := sysUtils.LoadStr(id);
  if assigned(OnZipStr) then
    OnZipStr(self, id, Result);
end;
//? TZipMaster.ZipStr

(*? EZipMaster.CreateRes ======================================================
1.73 10 July 2003 RP added support for external translation
*)
// The default exception constructor used.

constructor EZipMaster.CreateResDisp(const Ident: Integer; const Display:
  Boolean);
begin
  inherited CreateRes(Ident);

  if Message = '' then
    Message := Format('ZipMaster [ %d ]', [Ident]);
  FDisplayMsg := Display;
  FResIdent := Ident;
  Arg1 := '';
  Arg2 := '';
end;

constructor EZipMaster.CreateResDisk(const Ident: Integer; const DiskNo: Integer);
begin
  inherited CreateRes(Ident);

  if Message = '' then
    Message := Format('ZipMaster [ %d ] ,%d', [Ident, DiskNo])
  else
    Message := Format(Message, [DiskNo]);
  FDisplayMsg := True;
  FResIdent := Ident;
  Arg1 := '';
  Arg2 := IntToStr(DiskNo);
end;

constructor EZipMaster.CreateResDrive(const Ident: Integer; const Drive: string);
begin
  inherited CreateRes(Ident);

  if Message = '' then
    Message := Format('ZipMaster [ %d ], %s', [Ident, Drive])
  else
    Message := Format(Message, [Drive]);
  FDisplayMsg := True;
  FResIdent := Ident;
  Arg1 := Drive;
  Arg2 := '';
end;

constructor EZipMaster.CreateResFile(const Ident: Integer; const File1, File2:
  string);
begin
  inherited CreateRes(Ident);

  if Message = '' then
    Message := Format('ZipMaster [ %d ],%s,%s', [Ident, File1, File2])
  else
    Message := Format(Message, [File1, File2]);
  FDisplayMsg := True;
  FResIdent := Ident;
  Arg1 := File1;
  Arg2 := File2;
end;

//? EZipMaster.CreateRes =======================================================

(*? QueryZip
1.73 05 July 2003 RP
 Return value:
 Bit Mapped result (if > 0)
 bits 0..3 = S L P D     - start of file
 S 1 = EXE file
 L 2 = Local Header
 P 4 = first part of split archive
 D 8 = Central Header
 bit 4 = Correct comment length (clear if junk at end of file)
 bits  5..7 = Loc Cen End
End 128 = end of central found (must be set for any chance at archive)
Cen 64 = linked Central Directory start (should be set unless split directory)
Loc 32 = Local Header linked to first Central (should be set unless split file)
 -7  = Open, read or seek error
 -9  = exception error
 Good file results    All cases 16 less if comment length (or file length) is wrong
 Zip = 128+64+32+16+2 = 242
 SFX = 128+64+32+16+1 = 241
 Last Part Span = 128+16 (144 - only EOC)
        = 128+16+8 (152 - split directory)
        = 128+64+16 (208 - split file)
*)

function QueryZip(const Fname: string): integer;
var
  EOC       : ZipEndOfCentral;
  pEOC      : pZipEndOfCentral;
  CentralHead: ZipCentralHeader;
  Sig       : Cardinal;
  pBuf      : pChar;
  EOCpossible: boolean;
  ReadPos, bufPos: cardinal;
  res, fileSize, fileHandle, Size: integer;
begin
  EOCPossible := false;
  fileHandle := -1;
  res := 0;
  Result := -7;
  pBuf := nil;
  try
    try
      // Open the input archive, presumably the last disk.
      fileHandle := FileOpen(Fname, fmShareDenyWrite or fmOpenRead);
      if fileHandle = -1 then
        exit;
      Result := 0;            // rest errors normally file too small
      // first we check if the start of the file has an IMAGE_DOS_SIGNATURE
      if (FileRead(fileHandle, Sig, 4) <> 4) then
        exit;
      if LongRec(Sig).Lo = IMAGE_DOS_SIGNATURE then
        res := 1
      else
        if Sig = LocalFileHeaderSig then
          res := 2
        else
          if Sig = CentralFileHeaderSig then
            res := 8          // part of split Central Directory
          else
            if Sig = ExtLocalSig then
              res := 4;       // first part of span

      // A test for a zip archive without a ZipComment.
      fileSize := FileSeek(fileHandle, -sizeof(EOC), 2);
      if fileSize = -1 then
        exit;                 // not zip - too small
      // try no comment
      if (FileRead(fileHandle, EOC, sizeof(EOC)) = sizeof(EOC)) and
        (EOC.HeaderSig = EndCentralDirSig) then
      begin
        EOCPossible := true;
        res := res or 128;    // EOC
        if EOC.ZipCommentLen = 0 then
          res := res or 16;   // good comment length
        if EOC.CentralDiskNo = EOC.ThisDiskNo then
        begin                 // verify start of central
          if (FileSeek(fileHandle, EOC.CentralOffset, 0) <> -1)
            and (FileRead(fileHandle, CentralHead, sizeof(CentralHead)) = sizeof(CentralHead))
            and (CentralHead.HeaderSig = CentralFileHeaderSig) then
          begin
            res := res or 64; // has linked Central
            if (CentralHead.DiskStart = EOC.ThisDiskNo)
              and (FileSeek(fileHandle, CentralHead.RelOffLocal, 0) <> -1)
              and (FileRead(fileHandle, Sig, sizeof(Sig)) = sizeof(Sig))
              and (Sig = LocalFileHeaderSig) then
              res := Res or 32; // linked local
          end;
          exit;
        end;
        res := res and $01F;  // remove rest
      end;
      // try to locate EOC
      inc(fileSize, sizeof(EOC));
      Size := 65535 + sizeof(EOC);
      if Size > fileSize then
        Size := fileSize;
      GetMem(pBuf, Size);
      ReadPos := filesize - Size;
      if (FileSeek(fileHandle, ReadPos, 0) <> -1)
        and (FileRead(fileHandle, pBuf^, size) = size) then
      begin
        // Finally try to find the EOC record within the last 65K...
        pEOC := pZipEndOfCentral(pBuf + Size - sizeof(EOC));
        while pChar(pEOC) > pBuf do // reverse search
        begin
          dec(pChar(pEOC));
          if pZipEndOfCentral(pEOC)^.HeaderSig = EndCentralDirSig then
          begin               // possible EOC found
            res := res or 128; // EOC
            // check correct length comment
            BufPos := (ReadPos + pChar(pEOC) - pBuf);
            if (BufPos + sizeof(EOC) + pEOC^.ZipCommentLen) = cardinal(filesize) then
              res := res or 16; // good comment length
            if pEOC^.CentralDiskNo = pEOC^.ThisDiskNo then
            begin             // verify start of central
              if (FileSeek(fileHandle, pEOC^.CentralOffset, 0) <> -1)
                and (FileRead(fileHandle, CentralHead, sizeof(CentralHead)) = sizeof(CentralHead))
                and (CentralHead.HeaderSig = CentralFileHeaderSig) then
              begin
                res := res or 64; // has linked Central
                if (CentralHead.DiskStart = pEOC^.ThisDiskNo)
                  and (FileSeek(fileHandle, CentralHead.RelOffLocal, 0) <> -1)
                  and (FileRead(fileHandle, Sig, sizeof(Sig)) = sizeof(Sig))
                  and (Sig = LocalFileHeaderSig) then
                  res := Res or 32; // linked local
              end;
              break;
            end;
            res := res and $01F; // remove rest
          end;
        end;                  // while
      end;
      if EOCPossible and ((res and 128) = 0) then
        res := res or 128;
    except
      Result := -9;           // exception
    end;
  finally
    if (fileHandle <> -1) then
      FileClose(fileHandle);
    if Result = 0 then
      Result := res;
    if assigned(pBuf) then
      FreeMem(pBuf);
  end;
end;
//? QueryZip

(*? TZipMaster.GetSFXSlave
1.73 15 Juli 2003 RA added passing message type in MessageFlags to slave
1.73 4 July 2003
*)
{$IFNDEF NO_SFX}

function TZipMaster.GetSFXSlave: TCustomZipSFX;
{$IFDEF INTERNAL_SFX}
var
  o         : TSFXOptions;
{$ENDIF}
begin
{$IFNDEF NO_SFX}
  Result := FSFX;
{$IFDEF INTERNAL_SFX}
  if not assigned(Result) then
  begin
    FAutoSFXSlave := TZipSFX.create(nil);
    // set properties
    with FAutoSFXSlave as TZipSFX do
    begin
      Icon := fSFXIcon;
      DialogTitle := FSFXCaption;
      CommandLine := FSFXCommandLine;
      DefaultExtractPath := FSFXDefaultDir;
      MessageFlags := MB_OK;
      if FSFXMessage <> '' then
        case FSFXMessage[1] of
          #1:
            begin
              MessageFlags := MB_OKCANCEL or MB_ICONINFORMATION;
              System.Delete(FSFXMessage, 1, 1);
            end;
          #2:
            begin
              MessageFlags := MB_YESNO or MB_ICONQUESTION;
              System.Delete(FSFXMessage, 1, 1);
            end;
        end;
      Message := FSFXMessage;
      o := [];
      if SFXAskCmdLine in FSFXOptions then
        o := o + [soAskCmdLine];
      if SFXAskFiles in FSFXOptions then
        o := o + [soAskFiles];
      if SFXHideOverWriteBox in FSFXOptions then
        o := o + [soHideOverWriteBox];
      if SFXAutoRun in FSFXOptions then
        o := o + [soAutoRun];
      if SFXNoSuccessMsg in FSFXOptions then
        o := o + [soNoSuccessMsg];
      Options := o;
      case FSFXOverWriteMode of
        ovrConfirm: OverwriteMode := somAsk;
        ovrAlways: OverwriteMode := somOverwrite;
        ovrNever: OverwriteMode := somSkip;
      end;
      SFXPath := FSFXPath;
    end;
    Result := FAutoSFXSlave;
  end;
{$ENDIF}
  if not assigned(Result) then
{$ENDIF}
    raise EZipMaster.CreateResDisp(SF_NoSFXSupport, true);
end;
{$ENDIF}
//? TZipMaster.GetSFXSlave

//----------------------------------------------------------------------------
(*? ZCallback
// 1.73 ( 1 June 2003) changed for new callback
{ Dennis Passmore (Compuserve: 71640,2464) contributed the idea of passing an
 instance handle to the DLL, and, in turn, getting it back from the callback.
 This lets us referance variables in the TZipMaster class from within the
 callback function.  Way to go Dennis!
 Modified by Russell Peters }
*)

function ZCallback(ZCallBackRec: PZCallBackStruct): LongBool; stdcall;
begin
  with TObject(ZCallBackRec^.Caller) as TZipMaster do
    Result := CallBack(zacDLL, 0, '', cardinal(ZCallBackRec)) <> 0;
end;
//? ZCallback

{$IFDEF VERD4+}
(*? TZipMaster.BeforeDestruction
1.73 3 July 2003 RP stop callbacks
*)

procedure TZipMaster.BeforeDestruction;
begin
  fIsDestructing := true;     // stop callbacks
  inherited;
end;
//? TZipMaster.BeforeDestruction
{$ENDIF}

(*? TZipMaster.Create
// 1.73 ( 5 June 2003) - updated constructor/destructor
*)

constructor TZipMaster.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fIsDestructing := false;    // new 1.73
  fZipContents := TList.Create;
  FFSpecArgs := TStringList.Create;
  FFSpecArgsExcl := TStringList.Create; { New in v1.6 }
  fHandle := Application.Handle;
  fZipDll := TZipDll.Create(self); // new 1.73
  fUnzDll := TUnzDll.Create(self); // new 1.73
  FZipBusy := false;          // 1.72
  FUnzBusy := false;          // 1.72
  FBusy := false;             // new 1.72
  FOnAfterCallback := nil;    // new 1.72
  FOnTick := nil;             // new 1.72
  FOnFileExtra := nil;        // new 1.72
  FOnDirUpdate := nil;
  FOnProgress := nil;
  FOnTotalProgress := nil;    // new 1.73
  FOnItemProgress := nil;     // new 1.73
  FOnMessage := nil;
  FOnSetNewName := nil;
  FOnNewName := nil;
  FOnPasswordError := nil;
  FOnCRC32Error := nil;
  FOnExtractOverwrite := nil;
  FOnExtractSkipped := nil;
  FOnCopyZipOverwrite := nil;
  FOnFileComment := nil;
  FOnFileExtra := nil;        // 1.72
	FOnZipStr := nil;
  ZipParms := nil;
  UnZipParms := nil;
  FZipFileName := '';
  FPassword := '';
  FPasswordReqCount := 1;     { New in v1.6 }
  FEncrypt := False;
  FSuccessCnt := 0;
  FAddCompLevel := 9;         { dflt to tightest compression }
  FDLLDirectory := '';
  AutoExeViaAdd := False;
  FUnattended := False;
  FRealFileSize := 0;
  FSFXOffset := 0;
  FZipSOC := 0;
  FFreeOnDisk1 := 0;          { Don't leave any freespace on disk 1. }
  FFreeOnAllDisks := 0;       // 1.72 { use all space }
  FMaxVolumeSize := 0;        { Use the maximum disk size. }
  FMinFreeVolSize := 65536;   { Reject disks with less free bytes than... }
  FCodePage := cpAuto;
  FIsSpanned := False;
  FZipComment := '';
  HowToDelete := htdAllowUndo;
  FAddStoreSuffixes := [assGIF, assPNG, assZ, assZIP, assZOO, assARC, assLZH,
    assARJ, assTAZ
    , assTGZ, assLHA, assRAR, assACE, assCAB, assGZ, assGZIP, assJAR];
  FZipStream := TZipStream.Create;
  FUseDirOnlyEntries := False;
  FDirOnlyCount := 0;
  FVersionInfo := ZIPMASTERVERSION;
  FVer := ZIPMASTERVER;       // 1.72.0.4
	FCurWaitCount := 0;
  FSpanOptions := [];         // new 1.72
{$IFNDEF NO_SPAN}
  FOnGetNextDisk := nil;
  FOnStatusDisk := nil;
  fConfirmErase := True;
{$ENDIF}
  FSFX := nil;                // 1.72
{$IFNDEF NO_SFX}
{$IFDEF INTERNAL_SFX}
  FAutoSFXSlave := nil;       // 1.72x
  FSFXIcon := TIcon.Create;   // 1.72.0.4
  FSFXOverWriteMode := ovrConfirm;
  FSFXCaption := 'Self-extracting Archive';
  FSFXDefaultDir := '';
  FSFXCommandLine := '';
  FSFXOptions := [SFXCheckSize]; { Select this opt by default. }
  FSFXPath := 'DZSFXUS.BIN';  //'ZipSFX.bin';
{$ENDIF}
{$ENDIF}
end;
//? TZipMaster.Create

(*? TZipMaster.Destroy
1.73  1 June 2003 RP destructing flag to stop callbacks
*)

destructor TZipMaster.Destroy;
begin
	fIsDestructing := true;     // 1.73 - stop callbacks
  FZipStream.Free;
  FreeZipDirEntryRecords;
  fZipContents.Free;
  FFSpecArgsExcl.Free;
  FFSpecArgs.Free;
  //{$IFNDEF NO_SPAN}
  //{$ENDIF}
  fUnzDll.Free;               // new 1.73
  fZipDll.Free;               // new 1.73
{$IFNDEF NO_SFX}
{$IFDEF INTERNAL_SFX}
  if assigned(FSFXIcon) then
    FSFXIcon.Free;
  if (not (csDesigning in ComponentState)) and assigned(FAutoSFXSlave) then
    FAutoSFXSlave.Free;
  FAutoSFXSlave := nil;
{$ENDIF}
{$ENDIF}
  inherited Destroy;
end;
//? TZipMaster.Destroy

//---------------------------------------------------------------------------
(*? TZipMaster.IsRightDisk
1.73 29 June 2003 RP amended
*)

function TZipMaster.IsRightDisk: Boolean;
begin
  Result := true;
  if (not FDriveFixed) and (FVolumeName = 'PKBACK# ' + copy(IntToStr(1001 + FDiskNr), 2, 3))
    and (FileExists(FInfileName)) then
    exit;
  CreateMVFileName(FInFileName, true);
  if not FDriveFixed then     // allways right only needed new filename
    Result := FileExists(FInFileName); // must exist
end;
//? TZipMaster.IsRightDisk

(*? TZipMaster.ReadSpan ----------------------------------------------------------
1.73 12 July 2003 RA made callback type zacItem for each file + handling Result from GetLastVolume
1.73 11 July 2003 RP changed split file handling
// Function to read a split up Zip source file from multiple disks and write it to one destination file.
// Return values:
// 0            All Ok.
// -7           ReadSpan errors. See ZipMsgXX.rc
// -8           Memory allocation error.
// -9           General unknown ReadSpan error.
 *)
{$IFNDEF NO_SPAN}

function TZipMaster.ReadSpan(InFileName: string; var OutFilePath: string):
  Integer;
var
  Buffer    : array[0..BufSize - 1] of Char;
  TotalBytesToRead: Integer;
  EOC       : ZipEndOfCentral;
  LOH       : ZipLocalHeader;
  DD        : ZipDataDescriptor;
  CEH       : ZipCentralHeader;
  {DiskNo,}i, k: Integer;
  ExtendedSig: Integer;
  MsgStr    : string;
  TotalBytesWrite: Integer;
  MDZD      : TMZipDataList;
  MDZDp     : pMZipData;
begin {
  If Busy Then
  Begin
   Result := -9;
   exit;
  End;      }
 //	Result := 0;
  TotalBytesToRead := 0;

  fErrCode := 0;
  fUnzBusy := True;
  FDrive := ExtractFileDrive(InFileName) + '\';
  FDriveFixed := IsFixedDrive(FDrive); // 1.72
  FDiskNr := -1;
  FNewDisk := False;
  FShowProgress := False;
  FInFileName := InFileName;
  FInFileHandle := -1;
  MDZD := nil;

  StartWaitCursor;
  try
    // If we don't have a filename we make one first.
    if ExtractFileName(OutFilePath) = '' then
    begin
      OutFilePath := MakeTempFileName('', '');
      if OutFilePath = '' then
        raise EZipMaster.CreateResDisp(DS_NoTempFile, True);
    end
    else
    begin
      EraseFile(OutFilePath, FHowToDelete);
      OutFilePath := ChangeFileExt(OutFilePath, '.zip');
    end;

    // Try to get the last disk from the user if part of Volume numbered set
    Result := GetLastVolume(FInFileName, EOC, false);
    if Result < 0 then
    begin
      StopWaitCursor;
      FUnzBusy := false;
      Result := -9;
      exit;
    end;
    if Result > 0 then
      raise EZipMaster.CreateResDisp(DS_Canceled, true);

    // Create the output file.
    FOutFileHandle := FileCreate(OutFilePath);
    if FOutFileHandle = -1 then
      raise EZipMaster.CreateResDisp(DS_NoOutFile, True);

    // Get the date-time stamp and save for later.
    FDateStamp := FileGetDate(FInFileHandle);

    // Now we now the number of zipped entries in the zip archive
    // and the starting disk number of the central directory.
    FTotalDisks := EOC.ThisDiskNo;
    if EOC.ThisDiskNo <> EOC.CentralDiskNo then
      GetNewDisk(EOC.CentralDiskNo); // request a previous disk first
    // We go to the start of the Central directory. v1.52i
    if FileSeek(FInFileHandle, EOC.CentralOffset, 0) = -1 then
      raise EZipMaster.CreateResDisp(DS_FailedSeek, True);

    MDZD := TMZipDataList.Create(EOC.TotalEntries);

    // Read for every entry: The central header and save information for later use.
    for i := 0 to (EOC.TotalEntries - 1) do
    begin
      // Read a central header.
      while FileRead(FInFileHandle, CEH, SizeOf(CEH)) <> SizeOf(CEH) do //v1.52i
      begin
        // It's possible that we have the central header split up
        if FDiskNr >= EOC.ThisDiskNo then
          raise EZipMaster.CreateResDisp(DS_CEHBadRead, True);
        // We need the next disk with central header info.
        GetNewDisk(FDiskNr + 1);
      end;

      if CEH.HeaderSig <> CentralFileHeaderSig then
        raise EZipMaster.CreateResDisp(DS_CEHWrongSig, True);

      // Now the filename.
			ReadJoin(Buffer, CEH.FileNameLen, DS_CENameLen);

			// Save the file name info in the MDZD structure.
      MDZDp := MDZD[i];
      MDZDp^.FileNameLen := CEH.FileNameLen;
      StrLCopy(MDZDp^.FileName, Buffer, CEH.FileNameLen);

      // Save the compressed size, we need this because WinZip sometimes sets this to
      // zero in the local header. New v1.52d
      MDZDp^.ComprSize := CEH.ComprSize;

      // We need the total number of bytes we are going to read for the progress event.
      TotalBytesToRead := TotalBytesToRead + Integer(CEH.ComprSize +
        CEH.FileNameLen + CEH.ExtraLen + CEH.FileComLen);

      // Seek past the extra field and the file comment.
      if FileSeek(FInFileHandle, CEH.ExtraLen + CEH.FileComLen, 1) = -1 then
        raise EZipMaster.CreateResDisp(DS_FailedSeek, True);
    end;

    // Now we need the first disk and start reading.
    GetNewDisk(0);

    FShowProgress := True;
    Callback(zacCount, 6, '', EOC.TotalEntries);
    Callback(zacSize, 6, '', TotalBytesToRead);

    // Read extended local Sig. first; is only present if it's a spanned archive.
		ReadJoin(ExtendedSig, 4, DS_ExtWrongSig);
    if ExtendedSig <> ExtLocalSig then
      raise EZipMaster.CreateResDisp(DS_ExtWrongSig, True);

    // Read for every zipped entry: The local header, variable data, fixed data
      // and if present the Data decriptor area.
    for i := 0 to (EOC.TotalEntries - 1) do
    begin
      // First the local header.
      while FileRead(FInFileHandle, LOH, SizeOf(LOH)) <> SizeOf(LOH) do
      begin
        // Check if we are at the end of a input disk not very likely but...
        if FileSeek(FInFileHandle, 0, 1) <> FileSeek(FInFileHandle, 0, 2) then
          raise EZipMaster.CreateResDisp(DS_LOHBadRead, True);
        // Well it seems we are at the end, so get a next disk.
        GetNewDisk(FDiskNr + 1);
      end;
      if LOH.HeaderSig <> LocalFileHeaderSig then
        raise EZipMaster.CreateResDisp(DS_LOHWrongSig, True);

      // Now the filename, should be on the same disk as the LOH record.
			ReadJoin(Buffer, LOH.FileNameLen, DS_LONameLen);

			// Change some info for later while writing the central dir.
      k := MDZD.IndexOf(MakeString(Buffer, LOH.FileNameLen));
      MDZDp := MDZD[k];
      MDZDp^.DiskStart := 0;
      MDZDp^.RelOffLocal := FileSeek(FOutFileHandle, 0, 1);

      // Give message and progress info on the start of this new file read.
			MsgStr := LoadZipStr(GE_CopyFile, 'Copying: ') + SetSlash(MDZDp^.FileName, false);
      CallBack(zacMessage, 0, MsgStr, 0);

      TotalBytesWrite := SizeOf(LOH) + LOH.FileNameLen + LOH.ExtraLen +
        LOH.ComprSize;
      if (LOH.Flag and Word(#$0008)) = 8 then
        Inc(TotalBytesWrite, SizeOf(DD));
      Callback(zacItem, 0, MDZDp^.FileName, TotalBytesWrite);

      // Write the local header to the destination.
      WriteJoin(@LOH, SizeOf(LOH), DS_LOHBadWrite);

      // Write the filename.
      WriteJoin(Buffer, LOH.FileNameLen, DS_LOHBadWrite);

      // And the extra field
      RWJoinData(Buffer, LOH.ExtraLen, DS_LOExtraLen);

      // Read Zipped data, if the size is not known use the size from the central header.
      if LOH.ComprSize = 0 then
        LOH.ComprSize := MDZDp^.ComprSize; // New v1.52d
      RWJoinData(Buffer, LOH.ComprSize, DS_ZipData);

      // Read DataDescriptor if present.
      if (LOH.Flag and Word(#$0008)) = 8 then
        RWJoinData(@DD, SizeOf(DD), DS_DataDesc);
    end;                      // Now we have written al entries to the (hard)disk.
    Callback(zacEndOfBatch, 6, '', 0); // end of batch

    // Now write the central directory with changed offsets.
    FShowProgress := False;
    Callback(zacCount, 7, '', 1);
    Callback(zacSize, 7, '', EOC.TotalEntries);
    for i := 0 to (EOC.TotalEntries - 1) do
    begin
      // Read a central header which can be span more than one disk.
      while FileRead(FInFileHandle, CEH, SizeOf(CEH)) <> SizeOf(CEH) do
      begin
        // Check if we are at the end of a input disk.
        if FileSeek(FInFileHandle, 0, 1) <> FileSeek(FInFileHandle, 0, 2) then
          raise EZipMaster.CreateResDisp(DS_CEHBadRead, True);
        // Well it seems we are at the end, so get a next disk.
        GetNewDisk(FDiskNr + 1);
      end;
      if CEH.HeaderSig <> CentralFileHeaderSig then
        raise EZipMaster.CreateResDisp(DS_CEHWrongSig, True);

      // Now the filename.
			ReadJoin(Buffer, CEH.FileNameLen, DS_CENameLen);

			// Save the first Central directory offset for use in EOC record.
      if i = 0 then
        EOC.CentralOffset := FileSeek(FOutFileHandle, 0, 1);

      // Change the central header info with our saved information.
      k := MDZD.IndexOf(MakeString(Buffer, CEH.FileNameLen));
      MDZDp := MDZD[k];
      CEH.RelOffLocal := MDZDp^.RelOffLocal;
      CEH.DiskStart := 0;

      // Write this changed central header to disk
      // and make sure it fit's on one and the same disk.
      WriteJoin(@CEH, SizeOf(CEH), DS_CEHBadWrite);

      // Write to destination the central filename and the extra field.
      WriteJoin(Buffer, CEH.FileNameLen, DS_CEHBadWrite);

      // And the extra field
      RWJoinData(Buffer, CEH.ExtraLen, DS_CEExtraLen);

      // And the file comment.
      RWJoinData(Buffer, CEH.FileComLen, DS_CECommentLen);
    end;

    // Write the changed EndOfCentral directory record.
    EOC.CentralDiskNo := 0;
    EOC.ThisDiskNo := 0;
    WriteJoin(@EOC, SizeOf(EOC), DS_EOCBadWrite);

    // Skip past the original EOC to get to the ZipComment if present. v1.52M
    if (FileSeek(FInFileHandle, SizeOf(EOC), 1) = -1) then
      raise EZipMaster.CreateResDisp(DS_FailedSeek, True);

    // And finally the archive comment
    RWJoinData(Buffer, EOC.ZipCommentLen, DS_EOArchComLen);
    Callback(zacEndOfBatch, 7, '', 0);
  except
    on ers: EZipMaster do     // All ReadSpan specific errors.
    begin
      ShowExceptionError(ers);
      Result := -7;
    end;
    on EOutOfMemory do        // All memory allocation errors.
    begin
      ShowZipMessage(GE_NoMem, '');
      Result := -8;
    end;
    on E: Exception do
    begin
      // The remaining errors, should not occur.
      ShowZipMessage(DS_ErrorUnknown, E.Message);
      Result := -9;
    end;
  end;

	// Give final progress info at the end.
  CallBack(zacEndOfBatch, 0, '', 0);

  if Assigned(MDZD) then
    MDZD.Free;

  if FInFileHandle <> -1 then
    FileClose(FInFileHandle);
  if FOutFileHandle > 0 then  //<> -1 Then
  begin
    FileSetDate(FOutFileHandle, FDateStamp);
    FileClose(FOutFileHandle);
    if Result <> 0 then       // An error somewhere, OutFile is not reliable.
    begin
      DeleteFile(OutFilePath);
      OutFilePath := '';
    end;
  end;

  fUnzBusy := False;
  StopWaitCursor;
end;
{$ENDIF}
//? TZipMaster.ReadSpan

(*? TZipMaster.ZipFormat ------------------------------------------------------
1.73 10 July 2003 RP changed trace messages
1.73 9 July 2003 RA use of property ConfirmErase + confirmation messages translatable
1.73 27 June 2003 RP change handling of 'No'
         *)
{$IFNDEF NO_SPAN}
//  Format floppy disk

procedure TZipMaster.ClearFloppy(dir: string);
var
  SRec      : TSearchRec;
  Fname     : string;
begin
  if FindFirst(Dir + '*.*', faAnyFile, SRec) = 0 then
    repeat
      Fname := Dir + SRec.Name;
      if ((SRec.Attr and faDirectory) <> 0) and (SRec.Name <> '.') and (SRec.Name
        <> '..') then
      begin
        Fname := Fname + '\';
        ClearFloppy(Fname);
        if (Trace) then
          CallBack(zacMessage, 0, 'EraseFloppy - Removing ' + Fname, 0)
        else
					CallBack(zacTick, 0, '', 0); //allow time for OS to delete last file
        RemoveDir(Fname);
      end
      else
			begin
        if (Trace) then
          CallBack(zacMessage, 0, 'EraseFloppy - Deleting ' + Fname, 0);
        DeleteFile(Fname);
      end;
    until FindNext(SRec) <> 0;
  FindClose(SRec);
end;

function FormatFloppy(WND: HWND; Drive: string): integer;
const
  SHFMT_ID_DEFAULT = $FFFF;
  {options}
  SHFMT_OPT_FULL = $0001;
  SHFMT_OPT_SYSONLY = $0002;
  {return values}
  SHFMT_ERROR = $FFFFFFFF;    // -1 Error on last format, drive may be formatable
  SHFMT_CANCEL = $FFFFFFFE;   // -2 last format cancelled
  SHFMT_NOFORMAT = $FFFFFFFD; // -3 drive is not formatable
type
  TSHFormatDrive = function(WND: HWND; Drive, fmtID, Options: DWORD): DWORD;
  stdcall;
const
  SHFormatDrive: TSHFormatDrive = nil;
var
  drv       : integer;
  hLib      : THandle;
  OldErrMode: integer;
begin
  result := -3;               // error
  if not ((Length(Drive) > 1) and (Drive[2] = ':') and (Upcase(Drive[1]) in
    ['A'..'Z'])) then
    exit;
  if GetDriveType(pChar(Drive)) <> DRIVE_REMOVABLE then
    exit;
  drv := ord(Upcase(Drive[1])) - ord('A');
  OldErrMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  hLib := LoadLibrary('Shell32');
  if hLib <> 0 then
  begin
    @SHFormatDrive := GetProcAddress(hLib, 'SHFormatDrive');
    if @SHFormatDrive <> nil then
    try
      Result := SHFormatDrive(WND, drv, SHFMT_ID_DEFAULT, SHFMT_OPT_FULL);
    finally
      FreeLibrary(hLib);
    end;
    SetErrorMode(OldErrMode);
  end;
end;

function TZipMaster.ZipFormat: Integer;
var
  Msg, Vol  : string;
  Res {, drt}: Integer;
begin
  Result := -3;
	if (spTryFormat in SpanOptions) and not FDriveFixed then
    Result := FormatFloppy(Application.Handle, FDrive);
  if Result = -3 then
  begin
    Msg := LoadZipStr(FM_Erase, 'Erase ') + FDrive;
    Res := MessageBox(Handle, pChar(Msg), pChar(LoadZipStr(FM_Confirm, 'Confirm'))
      , MB_YESNO or MB_DEFBUTTON2 or MB_ICONWARNING);
    if Res <> IDYES then
    begin
      Result := -3;           // no  was -2; // cancel
      Exit;
    end;
    ClearFloppy(FDrive);
    Result := 0;
  end;
  if Length(FVolumeName) > 11 then
    Vol := Copy(FVolumeName, 1, 11)
  else
    Vol := FVolumeName;
  if (Result = 0) and not (spNoVolumeName in SpanOptions) then // did it
    SetVolumeLabel(pChar(FDrive), pChar(Vol));
end;
{$ENDIF}
//? TZipMaster.ZipFormat

// 1.73 (31 May 2003) RP - Pattern Match similar to dll

function PatternMatch(const pat: string; const str: string): boolean;
var
  pl, px, sl, sx: cardinal;
  c         : char;

  function pat_match(ps, pe: cardinal; ss, se: cardinal): boolean;
  var
    c, cs   : char;
    pa, sa  : cardinal;       // position of current '*'
  begin
    pa := 0;                  // no '*'
    sa := 0;
    repeat                    // scan till end or unrecoverable error
      if ss > se then         // run out of string - only '*' allowed in pattern
      begin
        while pat[ps] = '*' do
          inc(ps);
        Result := (ps > pe);  // ok end of pattern
        exit;
      end;
      // more string to process
      c := pat[ps];
      if c = '*' then
      begin                   // on mask
        inc(ps);
        if (ps > pe) then     // last char - matches rest of string
        begin
          Result := true;
          exit
        end;
        // note scan point incase must back up
        pa := ps;
        sa := ss;
      end
      else
      begin
        // if successful match, inc pointers and keep trying
        cs := str[ss];
        if (cs = c) or (UpCase(cs) = UpCase(c))
          or ((c = '\') and (cs = '/')) or ((c = '/') and (cs = '\'))
          or ((c = '?') and (cs <> char(0))) then
        begin
          inc(ps);
          inc(ss);
        end
        else                  // no match
        begin
          if (pa = 0) then    // any asterisk?
          begin
            Result := false;  // no - no match
            exit;
          end;
          ps := pa;           // backup
          inc(sa);
          ss := sa;
        end;
      end;
    until false;
  end;

begin
  //  Result := false;
  pl := length(pat);          // find start of ext
  sl := length(str);
  px := pl;
  while px >= 1 do
  begin
    c := pat[px];
    if (c = '\') or (c = '/') then
      px := 0                 // no name left - skip path
    else
      if c = '.' then
        break
      else
        dec(px);
  end;
  if px = 0 then
  begin
    Result := pat_match(1, pl, 1, sl);
    exit;
  end;
  sx := sl;
  while sx >= 1 do
  begin
    c := str[sx];
    if (c = '\') or (c = '/') then
      sx := 0                 // no name left - skip path
    else
      if c <> '.' then
        dec(sx)
      else
        break;
  end;
  if (sx = 0) then
    sx := succ(sl);           // past end

  Result := pat_match(1, pred(px), 1, pred(sx));
  if Result and ((px <> pred(pl)) or (pat[pl] <> '*')) then
    Result := pat_match(px, pl, sx, sl); // test exts
end;

(*? SetSlash
1.73
forwardSlash = false = Windows normal backslash '\'
forwardSlash = true = forward slash '/'
*)

function SetSlash(const path: string; forwardSlash: boolean): string;
var
  c, f, r   : char;
  i, len    : integer;
begin
  Result := path;
  len := Length(path);
  if forwardSlash then
  begin
    f := '\';
    r := '/';
  end
  else
  begin
    f := '/';
    r := '\';
  end;
  i := 1;
  while i <= len do
  begin
    c := path[i];
    if c in LeadBytes then
    begin
      inc(i, 2);
      continue;
    end;
    if c = f then
      Result[i] := r;
    inc(i);
  end;
end;
//? SetSlash

function DelimitPath(const path: string; sep: boolean): string;
var
  c, used   : char;
  i         : integer;
begin
  Result := path;
  if Length(path) = 0 then
  begin
    if sep then
      Result := '\';
    exit;
  end;
  used := '\';
  c := #0;
  i := 1;
  // find last char
  while i <= Length(path) do
  begin
    c := path[i];
    inc(i);
    if c in LeadBytes then
    begin
      inc(i);
      continue;
    end;
    if (c = '/') or (c = '\') then
      used := c;
  end;
  // is last delimiter
  if ((c = '/') or (c = '\')) <> sep then
  begin
    if sep then
      Result := path + used   // append delim
    else
      Result := copy(path, 1, pred(Length(path))); // remove delim
  end;
end;

// ---------------------------- ZipDataList --------------------------------

function TMZipDataList.GetItems(Index: integer): pMZipData;
begin
  if Index >= Count then
    raise Exception.CreateFmt('Index (%d) outside range 1..%d',
      [Index, Count - 1]);
  Result := inherited Items[Index];
end;

constructor TMZipDataList.Create(TotalEntries: integer);
var
  i         : Integer;
  MDZDp     : pMZipData;
begin
  inherited Create;
  Capacity := TotalEntries;
  for i := 1 to TotalEntries do
  begin
    New(MDZDp);
    MDZDp^.FileName := '';
    Add(MDZDp);
  end;
end;

destructor TMZipDataList.Destroy;
var
  i         : Integer;
  MDZDp     : pMZipData;
begin
  if Count > 0 then
  begin
    for i := (Count - 1) downto 0 do
    begin
      MDZDp := Items[i];
      if Assigned(MDZDp) then // dispose of the memory pointed-to by this entry
        Dispose(MDZDp);

      Delete(i);              // delete the TList pointer itself
    end;
  end;
  inherited Destroy;
end;

function TMZipDataList.IndexOf(fname: string): integer;
var
  MDZDp     : pMZipData;
begin
  for Result := 0 to (Count - 1) do
  begin
    MDZDp := Items[Result];
    if CompareText(fname, MDZDp^.FileName) = 0 then // case insensitive compare
      break;
  end;

  // Should not happen, but maybe in a bad archive...
  if Result = Count then
    raise EZipMaster.CreateResDisp(DS_EntryLost, True);
end;

procedure TZipMaster.NoWrite(Value: integer); // 1.72.0.4
begin
end;

function TPasswordDlg.ShowModalPwdDlg(DlgCaption, MsgTxt: string): string;
begin
  Caption := DlgCaption;
  PwdTxt.Caption := MsgTxt;
  ShowModal();
  if ModalResult = mrOk then
    Result := PwdEdit.Text
  else
    Result := '';
end;

constructor TPasswordDlg.CreateNew2(Owner: TComponent; pwb: TPasswordButtons);
var
  BtnCnt, Btns, i, k: Integer;
begin
{$IFDEF VERD4+}
  inherited CreateNew(Owner, 0);
{$ELSE}
  inherited CreateNew(Owner);
{$ENDIF}
  //	inherited CreateNew(Owner{$IFDEF VERD4+}, 0{$ENDIF});
    // Convert Button Set to a bitfield
  BtnCnt := 1;                // We need at least the Ok button
  Btns := 1;

  if pwbCancel in pwb then
  begin
    Inc(BtnCnt);
    Btns := Btns or 2;
  end;
  if pwbCancelAll in pwb then
  begin
    Inc(BtnCnt);
    Btns := Btns or 4;
  end;
  if pwbAbort in pwb then
  begin
    Inc(BtnCnt);
    Btns := Btns or 8;
  end;

  Parent := Self;
  Width := 124 * BtnCnt + 35;
  Height := 137;
  Font.Name := 'Arial';
  Font.Height := -12;
  Font.Style := Font.Style + [fsBold];
  BorderStyle := bsDialog;
  Position := poScreenCenter;

  PwdTxt := TLabel.Create(Self);
  PwdTxt.Parent := Self;
  PwdTxt.Left := 20;
  PwdTxt.Top := 8;
  PwdTxt.Width := 297;
  PwdTxt.Height := 18;
  PwdTxt.AutoSize := False;

  PwdEdit := TEdit.Create(Self);
  PwdEdit.Parent := Self;
  PwdEdit.Left := 20;
  PwdEdit.Top := 40;
  PwdEdit.Width := 124 * BtnCnt - 10;
  PwdEdit.PasswordChar := '*';
  PwdEdit.MaxLength := PWLEN;

  for i := 1 to 3 do
    PwBtn[i] := nil;
  k := 0;
  for i := 1 to 8 do
  begin
    if (i = 3) or ((i > 4) and (i < 8)) then
      Continue;
    if (Btns and i) = 0 then
      Continue;
    PwBtn[k] := TBitBtn.Create(Self);
    PwBtn[k].Parent := Self;
    PwBtn[k].Top := 72;
    PwBtn[k].Height := 28;
    PwBtn[k].Width := 114;
    PwBtn[k].Left := 20 + 124 * k;
    case i of
      1: PwBtn[k].Kind := bkOk;
      2: PwBtn[k].Kind := bkCancel;
      4: PwBtn[k].Kind := bkNo;
      8: PwBtn[k].Kind := bkAbort;
    end;
    if i = 4 then
      PwBtn[k].ModalResult := mrNoToAll;
    case i of
      1: PwBtn[k].Caption := LoadStr(PW_Ok);
      2: PwBtn[k].Caption := LoadStr(PW_Cancel);
      4: PwBtn[k].Caption := LoadStr(PW_CancelAll);
      8: PwBtn[k].Caption := LoadStr(PW_Abort);
    end;
    Inc(k);
  end;
end;

destructor TPasswordDlg.Destroy;
var
  i         : Integer;
begin
  for i := 0 to 3 do
    PwBtn[i].Free;
  PwdEdit.Free;
  PwdTxt.Free;
  inherited Destroy;
end;

procedure TZipMaster.TraceMessage(Msg: string);
begin
  if Trace and Assigned(OnMessage) then
    OnMessage(Self, 0, Msg);  // No ErrCode here else w'll get a msg from the application
end;

//---------------------------------------------------------------------------
{ We'll normally have a TStringList value, since TStrings itself is an
	abstract class. }

procedure TZipMaster.SetFSpecArgs(Value: TStrings);
begin
  FFSpecArgs.Assign(Value);
end;

procedure TZipMaster.SetFSpecArgsExcl(Value: TStrings);
begin
  FFSpecArgsExcl.Assign(Value);
end;

procedure TZipMaster.SetFilename(Value: string);
begin
  FZipFileName := Value;
  if not (csDesigning in ComponentState) then
    _List;                    { automatically build a new TLIST of contents in "ZipContents" }
end;

// NOTE: we will allow a dir to be specified that doesn't exist,
// since this is not the only way to locate the DLLs.

procedure TZipMaster.SetDLLDirectory(Value: string);
var
  ValLen    : Integer;
begin
  if Value <> FDLLDirectory then
  begin
    ValLen := Length(Value);
    // if there is a trailing \ in dirname, cut it off:
    if ValLen > 0 then
      if Value[ValLen] = '\' then
        SetLength(Value, ValLen - 1); // shorten the dirname by one
    FDLLDirectory := Value;
  end;
end;

function TZipMaster.GetCount: Integer;
begin
  if ZipFileName <> '' then
    Result := ZipContents.Count
  else
    Result := 0;
end;

// We do not want that this can be changed, but we do want to see it in the OI.

procedure TZipMaster.SetVersionInfo(Value: string);
begin
end;

procedure TZipMaster.SetPasswordReqCount(Value: LongWord);
begin
  if Value <> FPasswordReqCount then
  begin
    if Value > 15 then
      Value := 15;
    FPasswordReqCount := Value;
  end;
end;

function TZipMaster.GetZipComment: string;
begin
	Result := ConvertOEM(FZipComment, cpdOEM2ISO);
end;

(*? TZipMaster.SetZipComment
// 1.73 ( 21 July 2003) RA user Get Lastvolume to add ZipComment to splitted archive
*)

procedure TZipMaster.SetZipComment(zComment: string);
var
  EOC       : ZipEndOfCentral;
  len       : Integer;
  CommentBuf: pChar;
  Fatal     : Boolean;
begin
  FInFileHandle := -1;
  Fatal := False;
  CommentBuf := nil;

  try
    { ============================ Changed by Jim Turner =========================}
    if Length(zComment) = 0 then
      FZipComment := ''
    else
      FZipComment := ConvertOEM(zComment, cpdISO2OEM);
    //			FZipComment := ConvCodePage(zComment, cpdISO2OEM);
    //    if Length(ZipFileName) = 0 then
    //      raise EZipMaster.CreateResDisp(GE_NoZipSpecified {DS_NoInFile}, True);
    len := Length(FZipComment);
    GetMem(CommentBuf, len + 1);
    StrPLCopy(CommentBuf, zComment, len + 1);
    if Length(ZipFileName) <> 0 then // RP 1.73
      //FInFileHandle := FileOpen(ZipFileName, fmShareDenyWrite or fmOpenReadWrite);
      GetLastVolume(ZipFileName, EOC, True);
    // FInFileName opened by OpenEOC() only for Read
    if (FInFileHandle <> -1) then
      FileClose(FInFileHandle);
    FInFileHandle := FileOpen(FInFileName, fmShareDenyWrite or fmOpenReadWrite);
    if FInFileHandle <> -1 then // RP 1.60
    begin
      if FileSeek(FInFileHandle, FZipEOC, 0) = -1 then
        raise EZipMaster.CreateResDisp(DS_FailedSeek, True);
      if (FileRead(FInFileHandle, EOC, SizeOf(EOC)) <> SizeOf(EOC)) or
        (EOC.HeaderSig <> EndCentralDirSig) then
        raise EZipMaster.CreateResDisp(DS_EOCBadRead, True);
      EOC.ZipCommentLen := len;
      if FileSeek(FInFileHandle, -SizeOf(EOC), 1) = -1 then
        raise EZipMaster.CreateResDisp(DS_FailedSeek, True);
      Fatal := True;
      if FileWrite(FInFileHandle, EOC, SizeOf(EOC)) <> SizeOf(EOC) then
        raise EZipMaster.CreateResDisp(DS_EOCBadWrite, True);
      if FileWrite(FInFileHandle, CommentBuf^, len) <> len then
        raise EZipMaster.CreateResDisp(DS_NoWrite, True);
      Fatal := False;
      // if SetEOF fails we get garbage at the end of the file, not nice but
               // also not important.
      SetEndOfFile(FInFileHandle);
    end;
  except
    on ews: EZipMaster do
    begin
      ShowExceptionError(ews);
      FZipComment := '';
    end;
    on EOutOfMemory do
    begin
      ShowZipMessage(GE_NoMem, '');
      FZipComment := '';
    end;
  end;
  FreeMem(CommentBuf);
  if FInFileHandle <> -1 then
    FileClose(FInFileHandle);
  if Fatal then               // Try to read the zipfile, maybe it still works.
    _List;
end;
//? TZipMaster.SetZipComment

procedure TZipMaster.StartWaitCursor;
begin
  if ForegroundTask then      // 1.72
  begin
    if FCurWaitCount = 0 then
    begin
      FSaveCursor := Screen.Cursor;
      Screen.Cursor := crHourglass;
    end;
    Inc(FCurWaitCount);
  end;
end;

procedure TZipMaster.StopWaitCursor;
begin
  if ForegroundTask then      // 1.72
  begin
    if FCurWaitCount > 0 then
    begin
      Dec(FCurWaitCount);
      if FCurWaitCount = 0 then
        Screen.Cursor := FSaveCursor;
    end;
  end;
end;

// new 1.72

function TZipMaster.Busy: Boolean;
begin
  Result := FBusy or FZipBusy or FUnzBusy;
end;

{ New in v1.50: We are now looking at the Central zip Dir, instead of
  the local zip dir.  This change was needed so we could support
  Disk-Spanning, where the dir for the whole disk set is on the last disk.}
{ The List method reads thru all entries in the central Zip directory.
  This is triggered by an assignment to the ZipFilename, or by calling
  this method directly. }

function TZipMaster.List: integer; // public
begin
  Result := BUSY_ERROR;
  if Busy then
    exit;
  try
    fBusy := true;
    _List;
  finally
    fBusy := false;
  end;
  Result := fErrCode;
end;

// new 1.72 tests for 'fixed' drives

function TZipMaster.IsFixedDrive(drv: string): boolean;
var
  drt       : integer;
begin
  drt := GetDriveType(pChar(drv));
  Result := (drt = DRIVE_FIXED) or (drt = DRIVE_REMOTE) or (drt =
    DRIVE_RAMDISK);
end;

// new 1.72

procedure TZipMaster.CreateMVFileName(var FileName: string; StripPartNbr: Boolean);
var
  ext       : string;
  StripLen  : integer;
begin                         // changes filename into multi volume filename
  if (spCompatName in FSpanOptions) then
  begin
    if (FDiskNr <> FTotalDisks) then
      ext := '.z' + copy(IntToStr(101 + FDiskNr), 2, 2)
    else
      ext := '.zip';
    FileName := ChangeFileExt(FileName, ext);
  end
  else
  begin
    StripLen := 0;
    if StripPartNbr then
      StripLen := 3;
    FileName := Copy(FileName, 1, length(FileName) -
      Length(ExtractFileExt(FileName)) - StripLen)
      + Copy(IntToStr(1001 + FDiskNr), 2, 3)
      + ExtractFileExt(FileName);
  end;

end;

procedure TZipMaster.SetExtAddStoreSuffixes(Value: string);
var
  str       : string;
  i         : integer;
  c         : char;
begin
  if Value <> '' then
  begin
    c := ':';
    i := 1;
    while i <= length(Value) do
    begin
      c := Value[i];
      if c <> '.' then
        str := str + '.';
      while (c <> ':') and (i <= length(Value)) do
      begin
        c := Value[i];
        if (c = ';') or (c = ':') or (c = ',') then
          c := ':';
        str := str + c;
        inc(i);
      end;
    end;
    if c <> ':' then
      str := str + ':';
    fAddStoreSuffixes := fAddStoreSuffixes + [assEXT];
    fExtAddStoreSuffixes := Lowercase(str);
  end
  else
  begin
    fAddStoreSuffixes := fAddStoreSuffixes - [assEXT];
    fExtAddStoreSuffixes := '';
  end;
end;
// Add a new suffix to the suffix string if contained in the set 'FAddStoreSuffixes'
// changed 1.71

procedure TZipMaster.AddSuffix(const SufOption: AddStoreSuffixEnum; var sStr:
  string; sPos: Integer);
const
  SuffixStrings: array[0..17, 0..3] of Char = ('gif', 'png', 'z', 'zip', 'zoo',
    'arc', 'lzh', 'arj', 'taz', 'tgz', 'lha', 'rar', 'ace', 'cab', 'gz', 'gzip',
    'jar', 'exe');
begin
  if SufOption = assEXT then
  begin
    sStr := sStr + fExtAddStoreSuffixes;
  end
  else
    if SufOption in fAddStoreSuffixes then
      sStr := sStr + '.' + string(SuffixStrings[sPos]) + ':';
end;

procedure TZipMaster.SetDeleteSwitches; { override "add" behavior assumed by SetZipSwitches: }
begin
  with ZipParms^ do
  begin
    fDeleteEntries := True;
    fGrow := False;
    fJunkDir := False;
    fMove := False;
    fFreshen := False;
    fUpdate := False;
    fRecurse := False;        // bug fix per Angus Johnson
    fEncrypt := False;        // you don't need the pwd to delete a file
  end;
end;

procedure TZipMaster.SetUnZipSwitches(var NameOfZipFile: string; uzpVersion:
  Integer);
begin
  with UnZipParms^ do
  begin
    Version := uzpVersion;    //UNZIPVERSION;        // version we expect the DLL to be
    Caller := Self;           // point to our VCL instance; returned in callback

    fQuiet := True;           { we'll report errors upon notification in our callback }
    { So, we don't want the DLL to issue error dialogs }

    ZCallbackFunc := ZCallback; // pass addr of function to be called from DLL

    if fTrace then
      fTraceEnabled := True
    else
      fTraceEnabled := False;
    if fVerbose then
      fVerboseEnabled := True
    else
      fVerboseEnabled := False;
    if (fTraceEnabled and not fVerboseEnabled) then
      fVerboseEnabled := True; { if tracing, we want verbose also }

    if FUnattended then
      Handle := 0
    else
      Handle := fHandle;      // used for dialogs (like the pwd dialogs)

    fQuiet := True;           { no DLL error reporting }
    fComments := False;       { zipfile comments - not supported }
    fConvert := False;        { ascii/EBCDIC conversion - not supported }

    if ExtrDirNames in fExtrOptions then
      fDirectories := True
    else
      fDirectories := False;
    if ExtrOverWrite in fExtrOptions then
      fOverwrite := True
    else
      fOverwrite := False;

    if ExtrFreshen in fExtrOptions then
      fFreshen := True
    else
      fFreshen := False;
    if ExtrUpdate in fExtrOptions then
      fUpdate := True
    else
      fUpdate := False;
    if fFreshen and fUpdate then
      fFreshen := False;      { Update has precedence over freshen }

    if ExtrTest in fExtrOptions then
      fTest := True
    else
      fTest := False;

    { allocate room for null terminated string }
    pZipFN := StrAlloc(Length(NameOfZipFile) + 1);
    StrPLCopy(pZipFN, NameOfZipFile, Length(NameOfZipFile) + 1); { name of zip file }

    UnZipParms.fPwdReqCount := FPasswordReqCount;
    { We have to be carefull doing an unattended Extract when a password is needed
           for some file in the archive. We set it to an unlikely password, this way
     encrypted files won't be extracted.
             From verion 1.60 and up the event OnPasswordError is called in this case. }

    pZipPassword := StrAlloc(Length(FPassword) + 1); // Allocate room for null terminated string.
    StrPLCopy(pZipPassword, FPassword, Length(FPassword) + 1); // Password for encryption/decryption.
  end;                        { end with }
end;

function TZipMaster.GetAddPassword: string;
var
  p1, p2    : string;
begin
  p2 := '';
  if FUnattended then
    ShowZipMessage(PW_UnatAddPWMiss, '')
  else
  begin
    if (GetPassword(LoadZipStr(PW_Caption, RESOURCE_ERROR),
      LoadStr(PW_MessageEnter), [pwbCancel], p1) = pwbOk) and (p1 <> '') then
    begin
      if (GetPassword(LoadZipStr(PW_Caption, RESOURCE_ERROR),
        LoadStr(PW_MessageConfirm), [pwbCancel], p2) = pwbOk) and (p2 <> '') then
      begin
        if AnsiCompareStr(p1, p2) <> 0 then
        begin
          ShowZipMessage(GE_WrongPassword, '');
          p2 := '';
        end
        else
          if GAssignPassword then
            FPassword := p2;
      end;
    end;
  end;
  Result := p2;
end;

// Same as GetAddPassword, but does NOT verify

function TZipMaster.GetExtrPassword: string;
var
  p1        : string;
begin
  p1 := '';
  if FUnattended then
    ShowZipMessage(PW_UnatExtPWMiss, '')
  else
    if (GetPassword(LoadZipStr(PW_Caption, RESOURCE_ERROR),
      LoadStr(PW_MessageEnter), [pwbCancel, pwbCancelAll], p1) = pwbOk) and (p1 <>
      '') then
      if GAssignPassword then
        FPassword := p1;
  Result := p1;
end;

function TZipMaster.GetPassword(DialogCaption, MsgTxt: string; pwb:
  TPasswordButtons; var ResultStr: string): TPasswordButton;
var
  Pdlg      : TPasswordDlg;
begin
  Pdlg := TPasswordDlg.CreateNew2(Self, pwb);
  ResultStr := Pdlg.ShowModalPwdDlg(DialogCaption, MsgTxt);
  GModalResult := Pdlg.ModalResult;
  Pdlg.Free;
  case GModalResult of
    mrOk: Result := pwbOk;
    mrCancel: Result := pwbCancel;
    mrNoToAll: Result := pwbCancelAll;
  else
    Result := pwbAbort;
  end;
end;

function TZipMaster.Add: integer;
begin
  Result := BUSY_ERROR;
  if Busy then
    exit;
  try
    FBusy := true;
    ExtAdd(0, 0, 0, nil);
  finally
    FBusy := false;
  end;
  Result := fErrCode;
end;

//---------------------------------------------------------------------------
(*? TZipMaster.AddStreamToFile
1.73 14 JUly 2003 RA check wildcards & initial FSuccessCnt
// FileAttr are set to 0 as default.
// FileAttr can be one or a logical combination of the following types:
// FILE_ATTRIBUTE_ARCHIVE, FILE_ATTRIBUTE_HIDDEN, FILE_ATTRIBUTE_READONLY, FILE_ATTRIBUTE_SYSTEM.
// FileName is as default an empty string.
// FileDate is default the system date.

// EWE: I think 'Filename' is the name you want to use in the zip file to
// store the contents of the stream under.
*)

function TZipMaster.AddStreamToFile(Filename: string; FileDate, FileAttr:
  DWORD): integer;
var
  st        : TSystemTime;
  ft        : TFileTime;
  FatDate, FatTime: Word;
begin
  Result := BUSY_ERROR;
  if Busy then
    exit;
  try
    FBusy := true;
    TraceMessage('AddStreamToFile, fname=' + Filename); //  qqq
    if Length(Filename) > 0 then
    begin
      FFSpecArgs.Clear();
      FFSpecArgs.Append(FileName);
    end;
    if FileDate = 0 then
    begin
      GetLocalTime(st);
      SystemTimeToFileTime(st, ft);
      FileTimeToDosDateTime(ft, FatDate, FatTime);
      FileDate := (DWORD(FatDate) shl 16) + FatTime;
    end;
    FSuccessCnt := 0;
    // Check if wildcards are set.
    if FFSpecArgs.Count > 0 then
    begin
      if (AnsiPos('*', FFSpecArgs.Strings[0]) > 0) or
        (AnsiPos('?', FFSpecArgs.Strings[0]) > 0) then
        ShowZipMessage(AD_InvalidName, '')
      else
        ExtAdd(1, FileDate, FileAttr, nil);
    end
    else
      ShowZipMessage(AD_NothingToZip, '');
  finally
    fBusy := false;
  end;
  Result := fErrCode;
end;
//? TZipMaster.AddStreamToFile

(*? TZipMaster.AddStreamToStream ---------------------------------------------
1.73 14 July 2003 RA Initial FSuccesCnt
*)

function TZipMaster.AddStreamToStream(InStream: TMemoryStream): TZipStream;
begin
  Result := nil;
  if Busy then
    Exit;
  FSuccessCnt := 0;
  if InStream = FZipStream then
  begin
    ShowZipMessage(AD_InIsOutStream, '');
    Exit;
  end;
  if InStream.Size > 0 then
  begin
    FZipStream.SetSize(InStream.Size + 6);
    // Call the extended Add procedure:
    ExtAdd(2, 0, 0, InStream);
    { The size of the output stream is reset by the dll in ZipParms2 in fOutStreamSize.
     Also the size is 6 bytes more than the actual output size because:
     - the first two bytes are used as flag, STORED=0 or DEFLATED=8.
     - the next four bytes are set to the calculated CRC value.
     The size is reset from Inputsize +6 to the actual data size +6.
     (you do not have to set the size yourself, in fact it won't be taken into account.
     The start of the stream is set to the actual data start. }
    if FSuccessCnt = 1 then
      FZipStream.Position := 6
    else
      FZipStream.SetSize(0);
  end
  else
    ShowZipMessage(AD_NothingToZip, '');
  Result := FZipStream;
end;
//? TZipMster.AddStreamToStream

function TZipMaster.Delete: integer; // 1.72 new public version
begin
  Result := BUSY_ERROR;
  if Busy then
    exit;
  try
    fBusy := true;
    _Delete;
  finally
    fBusy := false;
  end;
  Result := fErrCode;
end;

(*? TZipMaster._Delete
1.73 16 July 2003 RA catch and display dll load errors
1.73 13 July 2003 RA for spanned archive no exception but show mesage
*)

procedure TZipMaster._Delete;
var
  i, DLLVers: Integer;
  //  AutoLoad     : Boolean;
  pFDS      : pFileData;
  EOC       : ZipEndOfCentral;
  pExFiles  : pExcludedFileSpec;
begin
  FSuccessCnt := 0;
  if fFSpecArgs.Count = 0 then
  begin
    ShowZipMessage(DL_NothingToDel, '');
    Exit;
  end;
  if not FileExists(FZipFileName) then
  begin
    ShowZipMessage(GE_NoZipSpecified, '');
    Exit;
  end;
  // new 1.7 - stop delete from spanned
  OpenEOC(EOC, false);        //1.72 true);
  FileClose(fInFileHandle);   // only needed to test it
  if (IsSpanned) then
    //    raise EZipMaster.CreateResDisp(DL_NoDelOnSpan, true);
  begin
    ShowZipMessage(DL_NoDelOnSpan, '');
    exit;
  end;

  { Make sure we can't get back in here while work is going on }
  if fZipBusy then
    Exit;
  fZipBusy := True;           { delete uses the ZIPDLL, so it shares the FZipBusy flag }
  Cancel := False;

  try
    DLLVers := FZipDll.LoadDll(Min_ZipDll_Vers, false); //Load_ZipDll(AutoLoad);
  except
    on ews: EZipMaster do
    begin
      ShowExceptionError(ews);
      fBusy := false;
      exit;
    end;
  end;
  try
    try
      ZipParms := AllocMem(SizeOf(ZipParms2));
      SetZipSwitches(fZipFileName, DLLVers);
      SetDeleteSwitches;

      with ZipParms^ do
      begin
        fFDS := AllocMem(SizeOf(FileData) * FFSpecArgs.Count);
        for i := 0 to (fFSpecArgs.Count - 1) do
        begin
          pFDS := fFDS;
          Inc(pFDS, i);
          pFDS.fFileSpec := StrAlloc(Length(fFSpecArgs[i]) + 1);
          StrPLCopy(pFDS.fFileSpec, fFSpecArgs[i], Length(fFSpecArgs[i]) + 1);
        end;
        Argc := fSpecArgs.Count;
        fSeven := 7;
      end;                    { end with }
      { pass in a ptr to parms }
      FEventErr := '';        // added
      fSuccessCnt := fZipDLL.Exec(ZipParms);
      if fSuccessCnt < 0 then
      begin
        fSuccessCnt := 0;
        ShowZipMessage(GE_FatalZip, 'fatal Dll error');
      end;
    except
      ShowZipMessage(GE_FatalZip, '');
    end;
  finally
    fFSpecArgs.Clear;
    fFSpecArgsExcl.Clear;

    with ZipParms^ do
    begin
      StrDispose(pZipFN);
      StrDispose(pZipPassword);
      StrDispose(pSuffix);
      StrDispose(fTempPath);
      StrDispose(fArchComment);
      for i := (Argc - 1) downto 0 do
      begin
        pFDS := fFDS;
        Inc(pFDS, i);
        StrDispose(pFDS.fFileSpec);
      end;
      FreeMem(fFDS);
      for i := (fTotExFileSpecs - 1) downto 0 do
      begin
        pExFiles := fExFiles;
        Inc(pExFiles, i);
        StrDispose(pExFiles.fFileSpec);
      end;
      FreeMem(fExFiles);
    end;
    FreeMem(ZipParms);
    ZipParms := nil;
  end;

  FZipDll.Unload(false);
  fZipBusy := False;

  Cancel := False;
  if fSuccessCnt > 0 then
    _List;                    { Update the Zip Directory by calling List method }
end;
//? TZipMaster._Delete

constructor TZipStream.Create;
begin
  inherited Create;
  Clear();
end;

destructor TZipStream.Destroy;
begin
  inherited Destroy;
end;

procedure TZipStream.SetPointer(Ptr: Pointer; Size: Integer);
begin
  inherited SetPointer(Ptr, Size);
end;

(*? TZipMaster.ExtractFileToStream ------------------------------------------
1.73 15 July 2003 RA add check on filename in FSpecArgs + return on busy
*)

function TZipMaster.ExtractFileToStream(FileName: string): TZipStream;
begin
  // Use FileName if set, if not expect the filename in the FFSpecArgs.
  if Busy then
  begin
    Result := nil;
    Exit;
  end;
  FSuccessCnt := 0;
  if FileName <> '' then
  begin
    FFSpecArgs.Clear();
    FFSpecArgs.Add(FileName);
  end;
  if (FFSpecArgs.Count <> 0) then
  begin
    FZipStream.Clear();
    ExtExtract(1, nil);
    if FSuccessCnt <> 1 then
      Result := nil
    else
      Result := FZipStream;
  end
  else
  begin
    ShowZipMessage(AD_NothingToZip, '');
    Result := nil;
  end;
end;
//? TZipMaster.ExtractFileToStream

(*? TZipMaster.ExtractStreamToStream
1.73 14 July 2003 RA initial FSuccessCnt
*)

function TZipMaster.ExtractStreamToStream(InStream: TMemoryStream; OutSize:
  Longword): TZipStream;
begin
  if Busy then
  begin
    Result := nil;
    Exit;
  end;
  FSuccessCnt := 0;
  if InStream = FZipStream then
  begin
    ShowZipMessage(AD_InIsOutStream, '');
    Result := nil;
    Exit;
  end;
  FZipStream.Clear();
  FZipStream.SetSize(OutSize);
  ExtExtract(2, InStream);
  if FSuccessCnt <> 1 then
    Result := nil
  else
    Result := FZipStream;
end;
//? TZipMaster.ExtractStreamToStream

function TZipMaster.Extract: integer;
begin
  Result := BUSY_ERROR;
  if Busy then
    exit;
  try
    fBusy := true;
    ExtExtract(0, nil);
  finally
    fBusy := false;
  end;
  Result := fErrCode;
end;

//---------------------------------------------------------------------------
// Returns 0 if good copy, or a negative error code.

function TZipMaster.CopyFile(const InFileName, OutFileName: string): Integer;
const
  SE_CreateError = -1;        { Error in open or creation of OutFile. }
  SE_OpenReadError = -3;      { Error in open or Seek of InFile.      }
  SE_SetDateError = -4;       { Error setting date/time of OutFile.   }
  SE_GeneralError = -9;
var
  InFile, OutFile, InSize, OutSize: Integer;
begin
  InSize := -1;
  OutSize := -1;
  Result := SE_OpenReadError;
  FShowProgress := False;

  if not FileExists(InFileName) then
    Exit;
  StartWaitCursor;
  InFile := FileOpen(InFileName, fmOpenRead or fmShareDenyWrite);
  if InFile <> -1 then
  begin
    if FileExists(OutFileName) then
      EraseFile(OutFileName, FHowToDelete);
    OutFile := FileCreate(OutFileName);
    if OutFile <> -1 then
    begin
      Result := CopyBuffer(InFile, OutFile, -1);
      if (Result = 0) and (FileSetDate(OutFile, FileGetDate(InFile)) <> 0) then
        Result := SE_SetDateError;
      OutSize := FileSeek(OutFile, 0, 2);
      FileClose(OutFile);
    end
    else
      Result := SE_CreateError;
    InSize := FileSeek(InFile, 0, 2);
    FileClose(InFile);
  end;
  // An extra check if the filesizes are the same.
  if (Result = 0) and ((InSize = -1) or (OutSize = -1) or (InSize <> OutSize))
    then
    Result := SE_GeneralError;
  // Don't leave a corrupted outfile lying around. (SetDateError is not fatal!)
  if (Result <> 0) and (Result <> SE_SetDateError) then
    DeleteFile(OutFileName);

  StopWaitCursor;
end;

{ Delete a file and put it in the recyclebin on demand. }

function TZipMaster.EraseFile(const Fname: string; How: DeleteOpts): Integer;
var
  SHF       : TSHFileOpStruct;
  DelFileName: string;
begin
  // If we do not have a full path then FOF_ALLOWUNDO does not work!?
  DelFileName := Fname;
  if ExtractFilePath(Fname) = '' then
    DelFileName := GetCurrentDir() + '\' + Fname;

  Result := -1;
  // We need to be able to 'Delete' without getting an error
  // if the file does not exists as in ReadSpan() can occur.
  if not FileExists(DelFileName) then
    Exit;
  with SHF do
  begin
    Wnd := Application.Handle;
    wFunc := FO_DELETE;
    pFrom := pChar(DelFileName + #0);
    pTo := nil;
    fFlags := FOF_SILENT or FOF_NOCONFIRMATION;
    if How = htdAllowUndo then
      fFlags := fFlags or FOF_ALLOWUNDO;
  end;
  Result := SHFileOperation(SHF);
end;

// Make a temporary filename like: C:\...\zipxxxx.zip
// Prefix and extension are default: 'zip' and '.zip'

function TZipMaster.MakeTempFileName(Prefix, Extension: string): string;
var
  Buffer    : pChar;
  len       : DWORD;
begin
  Buffer := nil;
  if Prefix = '' then
    Prefix := 'zip';
  if Extension = '' then
    Extension := '.zip';
  try
    if Length(FTempDir) = 0 then // Get the system temp dir
    begin
      // 1.	The path specified by the TMP environment variable.
      //	2.	The path specified by the TEMP environment variable, if TMP is not defined.
   //	3.	The current directory, if both TMP and TEMP are not defined.
      len := GetTempPath(0, Buffer);
      GetMem(Buffer, len + 12);
      GetTempPath(len, Buffer);
    end
    else                      // Use Temp dir provided by ZipMaster
    begin
      FTempDir := AppendSlash(FTempDir);
      GetMem(Buffer, Length(FTempDir) + 13);
      StrPLCopy(Buffer, FTempDir, Length(FTempDir) + 1);
    end;
    if GetTempFileName(Buffer, pChar(Prefix), 0, Buffer) <> 0 then
    begin
      DeleteFile(Buffer);     // Needed because GetTempFileName creates the file also.
      Result := ChangeFileExt(Buffer, Extension); // And finally change the extension.
    end;
  finally
    FreeMem(Buffer);
  end;
end;
(*? TZipMaster.CopyBuffer
1.73 15 July 2003 RP progress
1.73 10 Jul 2003 RP use String as buffer
*)

function TZipMaster.CopyBuffer(InFile, OutFile, ReadLen: Integer): Integer;
const
  SE_CopyError = -2;          // Write error or no memory during copy.
var
  SizeR, ToRead: Integer;
  Buffer    : string;
begin
  // both files are already open
  Result := 0;
	ToRead := BufSize;
  try
    SetLength(Buffer, BufSize);
    repeat
      if ReadLen >= 0 then
      begin
        ToRead := ReadLen;
        if BufSize < ReadLen then
          ToRead := BufSize;
      end;
      SizeR := FileRead(InFile, pChar(Buffer)^, ToRead);
      if FileWrite(OutFile, pChar(Buffer)^, SizeR) <> SizeR then
      begin
        Result := SE_CopyError;
        Break;
      end;
      if ReadLen > 0 then
        Dec(ReadLen, SizeR);
      if FShowProgress then
        CallBack(zacProgress, 0, '', SizeR)
      else
				CallBack(zacTick, 0, '', 0); // Mostly for winsock.
    until ((ReadLen = 0) or (SizeR <> ToRead));
  except
    Result := SE_CopyError;
	end;
    // leave both files open
end;
//? TZipMaster.CopyBuffer

//---------------------------------------------------------------------------
// concat path

function PathConcat(path, extra: string): string;
var
  pathLst   : char;
  pathLen   : integer;
begin
  pathLen := Length(path);
  Result := path;
  if pathLen > 0 then
  begin
    pathLst := path[pathLen];
    if (pathLst <> ':') and (Length(extra) > 0) then
    begin
      if (extra[1] = '\') = (pathLst = '\') then
      begin
        if pathLst = '\' then
          Result := Copy(path, 1, pathLen - 1) // remove trailing
        else
          Result := path + '\'; // append trailing
      end;
    end;
  end;
  Result := Result + extra;
end;

(*? TZipMaster.Load_Zip_Dll
1.73 16 July 2003 RA catch and diplay dll load errors
*)

function TZipMaster.Load_Zip_Dll: integer; // CHANGED 1.70
begin
  try
    Result := FZipDll.LoadDll(Min_ZipDll_Vers, true);
  except
    on ews: EZipMaster do
    begin
      ShowExceptionError(ews);
      Result := 0;
    end;
  end;
end;
//? TZipMaster.Load_Zip_Dll
// CHANGED 1.70 - return version if loaded

(*? TZipMaster.Load_Unz_Dll
1.73 16 July 2003 RA catch and display dll load errors
*)

function TZipMaster.Load_Unz_Dll: integer;
begin
  try
    Result := fUnzDll.LoadDll(Min_UnzDll_Vers, true);
  except
    on ews: EZipMaster do
    begin
      ShowExceptionError(ews);
      Result := 0;
    end;
  end;
end;
//? TZipMaster.Load_Unz_Dll

procedure TZipMaster.Unload_Zip_Dll;
begin
  FZipDll.Unload(true);
end;

procedure TZipMaster.Unload_Unz_Dll;
begin
  FUnzDll.Unload(true);
end;

// new 1.72
(*? TZipMaster.ZipDllPath
*)
function TZipMaster.ZipDllPath: string;
begin
	Result := FZipDll.Path;
end;
//? TZipMaster.ZipDllPath
                        
(*? TZipMaster.UnzDllPath
*)
function TZipMaster.UnzDllPath: string;
begin
	Result := FUnzDll.Path;
end;       
//? TZipMaster.UnzDllPath
                        
(*? TZipMaster.GetZipVers
*)
function TZipMaster.GetZipVers: Integer;
begin
	Result := FZipDll.Version;
end;           
//? TZipMaster.GetZipVers
                   
(*? TZipMaster.GetUnzVers
*)
function TZipMaster.GetUnzVers: Integer;
begin
	Result := FUnzDll.Version;
end;
//? TZipMaster.GetUnzVers

procedure TZipMaster.AbortDlls;
begin
  Cancel := true;
end;

{ Replacement for the functions DiskFree and DiskSize. }
{ This should solve problems with drives > 2Gb and UNC filenames. }
{ Path FDrive ends with a backslash. }
{ Action=1 FreeOnDisk, 2=SizeOfDisk, 3=Both }

procedure TZipMaster.DiskFreeAndSize(Action: Integer); // RCV150199
var
  GetDiskFreeSpaceEx: function(RootName: pChar; var FreeForCaller, TotNoOfBytes:
    LargeInt; TotNoOfFreeBytes: pLargeInt): BOOL; stdcall;
  SectorsPCluster, BytesPSector, FreeClusters, TotalClusters: DWORD;
  LDiskFree, LSizeOfDisk: LargeInt;
  Lib       : THandle;
begin
  LDiskFree := -1;
  LSizeOfDisk := -1;
  Lib := GetModuleHandle('Kernel32');
  if Lib <> 0 then
  begin
    @GetDiskFreeSpaceEx := GetProcAddress(Lib, 'GetDiskFreeSpaceExA');
    if (@GetDiskFreeSpaceEx <> nil) then // We probably have W95+OSR2 or better.
      if not GetDiskFreeSpaceEx(pChar(FDrive), LDiskFree, LSizeOfDisk, nil) then
      begin
        LDiskFree := -1;
        LSizeOfDisk := -1;
      end;
    FreeLibrary(Lib);         //v1.52i
  end;
  if (LDiskFree = -1) then    // We have W95 original or W95+OSR1 or an error.
  begin                       // We use this because DiskFree/Size don't support UNC drive names.
    if GetDiskFreeSpace(pChar(FDrive), SectorsPCluster, BytesPSector,
      FreeClusters, TotalClusters) then
    begin
{$IFDEF VERD2D3}
      LDiskFree := (1.0 * BytesPSector) * SectorsPCluster * FreeClusters;
      LSizeOfDisk := (1.0 * BytesPSector) * SectorsPCluster * TotalClusters;
{$ELSE}
      LDiskFree := LargeInt(BytesPSector) * SectorsPCluster * FreeClusters;
      LSizeOfDisk := LargeInt(BytesPSector) * SectorsPCluster * TotalClusters;
{$ENDIF}
    end;
  end;
  if (Action and 1) <> 0 then
    FFreeOnDisk := LDiskFree;
  if (Action and 2) <> 0 then
    FSizeOfDisk := LSizeOfDisk;
end;

// Check to see if drive in FDrive is a valid drive.
// If so, put it's volume label in FVolumeName,
//        put it's size in FSizeOfDisk,
//        put it's free space in FDiskFree,
//        and return true.
// was IsDiskPresent
// If not valid, return false.
// Called by _List() and CheckForDisk().

function TZipMaster.GetDriveProps: Boolean;
var
  SysFlags, OldErrMode: DWord;
  NamLen    : Cardinal;
{$IFDEF VERD2D3}
  SysLen    : Cardinal;
{$ELSE}
  SysLen    : DWord;
{$ENDIF}
  //	SysLen    : {$IFDEF VERD2D3}Integer{$ELSE}DWord{$ENDIF};
  VolNameAry: array[0..255] of Char;
  Num       : Integer;
  Bits      : set of 0..25;
  DriveLetter: Char;
  DiskSerial: integer;
begin
  NamLen := 255;
  SysLen := 255;
  FSizeOfDisk := 0;
  FDiskFree := 0;
  FVolumeName := '';
  VolNameAry[0] := #0;
  Result := False;
  DriveLetter := UpperCase(FDrive)[1];

  if DriveLetter <> '\' then  // Only for local drives
  begin
    if (DriveLetter < 'A') or (DriveLetter > 'Z') then
      raise EZipMaster.CreateResDrive(DS_NotaDrive, FDrive);

    Integer(Bits) := GetLogicalDrives();
    Num := Ord(DriveLetter) - Ord('A');
    if not (Num in Bits) then
      raise EZipMaster.CreateResDrive(DS_DriveNoMount, FDrive);
  end;

  OldErrMode := SetErrorMode(SEM_FAILCRITICALERRORS); // Turn off critical errors:

  // Since v1.52c no exception will be raised here; moved to List() itself.
  if (not FDriveFixed) and    // 1.72 only get Volume label for removable drives
  {If}(not GetVolumeInformation(pChar(FDrive), VolNameAry, NamLen, @DiskSerial,
    SysLen, SysFlags, nil, 0)) then
  begin
    // W'll get this if there is a disk but it is not or wrong formatted
      // so this disk can only be used when we also want formatting.
    if (GetLastError() = 31) and (AddDiskSpanErase in FAddOptions) then
      Result := True;
    SetErrorMode(OldErrMode); //v1.52i
    Exit;
  end;

  FVolumeName := VolNameAry;
  { get free disk space and size. }
  DiskFreeAndSize(3);         // RCV150199

  SetErrorMode(OldErrMode);   // Restore critical errors:

  // -1 is not very likely to happen since GetVolumeInformation catches errors.
  // But on W95(+OSR1) and a UNC filename w'll get also -1, this would prevent
  // opening the file. !!!Potential error while using spanning with a UNC filename!!!
  if (DriveLetter = '\') or ((DriveLetter <> '\') and (FSizeOfDisk <> -1)) then
    Result := True;
end;

function TZipMaster.AppendSlash(sDir: string): string;
begin
  if (sDir <> '') and (sDir[Length(sDir)] <> '\') then
    Result := sDir + '\'
  else
    Result := sDir;
end;

//---------------------------------------------------------------------------
(*? TZipMaster.WriteJoin
1.73 15 July 2003 RP progress
*)

procedure TZipMaster.WriteJoin(Buffer: pChar; BufferSize, DSErrIdent: Integer);
begin
  if FileWrite(FOutFileHandle, Buffer^, BufferSize) <> BufferSize then
    raise EZipMaster.CreateResDisp(DSErrIdent, True);

  // Give some progress info while writing.
  // While processing the central header we don't want messages.
  if FShowProgress then
    Callback(zacProgress, 0, '', BufferSize);
end;
//? TZipMaster.WriteJoin

//---------------------------------------------------------------------------

function TZipMaster.GetDirEntry(idx: integer): pZipDirEntry;
begin
  Result := pZipDirEntry(ZipContents.Items[idx]); //^;
end;

function FileVersion(fname: string): string;
var
  siz       : Integer;
  buf, value: pChar;
  hndl      : DWORD;
begin
  Result := '?.?.?.?';
  siz := GetFileVersionInfoSize(PChar(fname), hndl);
  if siz > 0 then
  begin
    buf := AllocMem(siz);
    try
      GetFileVersionInfo(PChar(fname), 0, siz, buf);
      if VerQueryValue(buf, pChar('StringFileInfo\040904E4\FileVersion')
        , pointer(value), hndl) then
        Result := value
      else
        if VerQueryValue(buf, pChar('StringFileInfo\040904B0\FileVersion')
          , pointer(value), hndl) then
          Result := value;
    finally
      FreeMem(buf);
    end;
  end;
end;

function TZipMaster.FullVersionString: string;
begin
{$IFDEF NO_SPAN}
  Result := 'ZipMaster ' + ZIPMASTERBUILD + ' -SPAN ';
{$ELSE}
  Result := 'ZipMaster ' + ZIPMASTERBUILD + ' ';
{$ENDIF}
{$IFDEF NO_SFX}
  Result := Result + ' ,SFX- ';
{$ELSE}
  Result := Result + ' ,SFX = ';
{$IFDEF INTERNAL_SFX}
  if assigned(fAutoSFXSlave) then
    Result := Result + ' [' + (fAutoSFXSlave as TZipSFX).Version + ']'
  else
    Result := Result + ' []';
{$ENDIF}
  if assigned(fSFX) then
    Result := Result + ' ' + (fSFX as TZipSFX).Version;
{$ENDIF}
  //	if ZipDllHandle <> 0 then
  Result := Result + ', ZipDll ' + FileVersion(FZipDll.Path);
  //	if UnzDllHandle <> 0 then
  Result := Result + ', UnzDll ' + FileVersion(FUnzDll.Path);
end;

// 1.70 changed - no longer check fZipBusy uses writing instead
// 1.72 changed - now a procedure
// ask for disk with required part (FDriveNr)
{$IFNDEF NO_SPAN}

procedure TZipMaster.CheckForDisk(writing: bool);
var
  Res, MsgFlag: Integer;
  SizeOfDisk: LargeInt;       // RCV150199
  MsgStr    : string;
  AbortAction: Boolean;
begin
  FDriveFixed := IsFixedDrive(FDrive);
  if FDriveFixed then
  begin                       // If it is a fixed disk we don't want a new one.
    FNewDisk := False;
    exit;
  end;
  Callback(zacTick, 0, '', 0); // just ProcessMessages

  Res := IDOK;
  MsgFlag := MB_OKCANCEL;

  // First check if we want a new one or if there is a disk (still) present.
  while (FNewDisk or ((Res = IDOK) and not GetDriveProps)) do
  begin
    if FUnattended then
      raise EZipMaster.CreateResDisp(DS_NoUnattSpan, True);
    if FDiskNr < 0 then       // -1=ReadSpan(), 0=WriteSpan()
    begin
      MsgStr := LoadZipStr(DS_InsertDisk, 'Please insert last disk in set');
      MsgFlag := MsgFlag or MB_ICONERROR;
    end
    else
    begin
      if writing then         // Are we from ReadSpan() or WriteSpan()?
      begin
        // This is an estimate, we can't know if every future disk has the same space available and
        // if there is no disk present we can't determine the size unless it's set by MaxVolumeSize.
        SizeOfDisk := FSizeOfDisk - FFreeOnAllDisks;
        if (FMaxVolumeSize <> 0) and (FMaxVolumeSize < FSizeOfDisk) then
          SizeOfDisk := FMaxVolumeSize;

        FTotalDisks := FDiskNr;
        if (SizeOfDisk > 0) and (FTotalDisks < Trunc((FFileSize + 4 +
          FFreeOnDisk1) / SizeOfDisk)) then // RCV150199
          FTotalDisks := Trunc((FFileSize + 4 + FFreeOnDisk1) / SizeOfDisk);
        if SizeOfDisk > 0 then
          MsgStr := Format(LoadZipStr(DS_InsertVolume,
            'Please insert disk volume %.1d of %.1d'), [FDiskNr + 1, FTotalDisks +
            1])
        else
          MsgStr := Format(LoadZipStr(DS_InsertAVolume,
            'Please insert disk volume %.1d'), [FDiskNr + 1]);
      end
      else
        MsgStr := Format(LoadZipStr(DS_InsertVolume,
          'Please insert disk volume %.1d of %.1d'), [FDiskNr + 1, FTotalDisks +
          1]);
    end;
    MsgStr := MsgStr + Format(LoadZipStr(DS_InDrive, #13#10'in drive: %s'),
      [FDrive]);

    if not ((FDiskNr = 0) and GetDriveProps and writing) then
    begin
      if Assigned(FOnGetNextDisk) then // v1.60L
      begin
        AbortAction := False;
        FOnGetNextDisk(self, FDiskNr + 1, FTotalDisks + 1, Copy(FDrive, 1, 1),
          AbortAction);
        if AbortAction then
          Res := IDABORT
        else
          Res := IDOK;
      end
      else
        Res := MessageBox(Handle, pChar(MsgStr), pChar(Application.Title), MsgFlag);
    end;
    FNewDisk := False;
  end;

  // Check if user pressed Cancel or memory is running out.
  if Res <> IDOK then
    raise EZipMaster.CreateResDisp(DS_Canceled, False);
  if Res = 0 then
    raise EZipMaster.CreateResDisp(DS_NoMem, True);
end;

//---------------------------------------------------------------------------
// Read data from the input file with a maximum of 8192(BufSize) bytes per read
// and write this to the output file.
// In case of an error an Exception is raised and this will
// be caught in WriteSpan.

procedure TZipMaster.RWSplitData(Buffer: pChar; ReadLen, ZSErrVal: Integer);
var
  SizeR, ToRead: Integer;
begin
  while ReadLen > 0 do
  begin
    ToRead := BufSize;
    if ReadLen < BufSize then
      ToRead := ReadLen;
    SizeR := FileRead(FInFileHandle, Buffer^, ToRead);
    if SizeR <> ToRead then
      raise EZipMaster.CreateResDisp(ZSErrVal, True);
    WriteSplit(Buffer, SizeR, 0);
    Dec(ReadLen, SizeR);
  end;
end;

//---------------------------------------------------------------------------

procedure TZipMaster.RWJoinData(Buffer: pChar; ReadLen, DSErrIdent: Integer);
var
  ToRead, SizeR: Integer;
begin
  while ReadLen > 0 do
  begin
    ToRead := BufSize;
    if ReadLen < BufSize then
      ToRead := ReadLen;
    SizeR := FileRead(FInFileHandle, Buffer^, ToRead);
    if SizeR <> ToRead then
    begin
      // Check if we are at the end of a input disk.
      if FileSeek(FInFileHandle, 0, 1) <> FileSeek(FInFileHandle, 0, 2) then
        raise EZipMaster.CreateResDisp(DSErrIdent, True);
      // It seems we are at the end, so get a next disk.
      GetNewDisk(FDiskNr + 1);
    end;
    if SizeR > 0 then         // Fix by Scott Schmidt v1.52n
    begin
      WriteJoin(Buffer, SizeR, DSErrIdent);
      Dec(ReadLen, SizeR);
    end;
  end;
end;

//---------------------------------------------------------------------------

function TZipMaster.MakeString(Buffer: pChar; Size: Integer): string;
begin
  SetLength(Result, Size);
  StrLCopy(pChar(Result), Buffer, Size);
end;

{$ENDIF}
{$IFNDEF NO_SFX}
// SFX support
{$IFDEF INTERNAL_SFX}

procedure TZipMaster.SetSFXIcon(aIcon: TIcon);
begin
  if aIcon <> SFXIcon then
  begin
    fSFXIcon := aIcon;
{$IFDEF INTERNAL_SFX}
    if assigned(FAutoSFXSlave) then
      TFriendSFX(FAutoSFXSlave).Icon := aIcon;
{$ENDIF}
  end;
end;

function TZipMaster.GetSFXIcon: TIcon;
begin
{$IFDEF INTERNAL_SFX}
  if assigned(FAutoSFXSlave) then
    FSFXIcon := TFriendSFX(FAutoSFXSlave).Icon;
{$ENDIF}
  Result := FSFXIcon;
end;

function TZipMaster.GetSFXCaption: string;
begin
{$IFDEF INTERNAL_SFX}
  if assigned(FAutoSFXSlave) then
    FSFXCaption := TFriendSFX(FAutoSFXSlave).DialogTitle;
{$ENDIF}
  Result := FSFXCaption;
end;

procedure TZipMaster.SetSFXCaption(aString: string);
begin
  if aString <> SFXCaption then
  begin
    FSFXCaption := aString;
{$IFDEF INTERNAL_SFX}
    if assigned(FAutoSFXSlave) then
      TFriendSFX(FAutoSFXSlave).DialogTitle := aString;
{$ENDIF}
  end;
end;

function TZipMaster.GetSFXCommandLine: string;
begin
{$IFDEF INTERNAL_SFX}
  if assigned(FAutoSFXSlave) then
    FSFXCommandLine := TFriendSFX(FAutoSFXSlave).CommandLine;
{$ENDIF}
  Result := FSFXCommandLine;
end;

procedure TZipMaster.SetSFXCommandLine(aString: string);
begin
  if aString <> SFXCommandLine then
  begin
    FSFXCommandLine := aString;
{$IFDEF INTERNAL_SFX}
    if assigned(FAutoSFXSlave) then
      TFriendSFX(FAutoSFXSlave).CommandLine := aString;
{$ENDIF}
  end;
end;

function TZipMaster.GetSFXDefaultDir: string;
begin
{$IFDEF INTERNAL_SFX}
  if assigned(FAutoSFXSlave) then
    FSFXDefaultDir := TFriendSFX(FAutoSFXSlave).DefaultExtractPath;
{$ENDIF}
  Result := FSFXDefaultDir;
end;

procedure TZipMaster.SetSFXDefaultDir(aString: string);
begin
  if aString <> SFXDefaultDir then
  begin
    FSFXDefaultDir := aString;
{$IFDEF INTERNAL_SFX}
    if assigned(FAutoSFXSlave) then
      TFriendSFX(FAutoSFXSlave).DefaultExtractPath := aString;
{$ENDIF}
  end;
end;

function TZipMaster.GetSFXMessage: string;
begin
{$IFDEF INTERNAL_SFX}
  if assigned(FAutoSFXSlave) then
    FSFXMessage := TFriendSFX(FAutoSFXSlave).Message;
{$ENDIF}
  Result := FSFXMessage;
end;

procedure TZipMaster.SetSFXMessage(aString: string);
begin
  if aString <> SFXMessage then
  begin
    FSFXMessage := aString;
{$IFDEF INTERNAL_SFX}
    if assigned(FAutoSFXSlave) then
      TFriendSFX(FAutoSFXSlave).Message := aString;
{$ENDIF}
  end;
end;

function TZipMaster.GetSFXOptions: SfxOpts;
{$IFDEF INTERNAL_SFX}
var
  o         : TSFXOptions;
{$ENDIF}
begin
{$IFDEF INTERNAL_SFX}
  if assigned(FAutoSFXSlave) then
  begin
    FSFXOptions := [];
    o := TFriendSFX(FAutoSFXSlave).Options;
    if soAskCmdLine in o then // allow user to prevent execution of the command line
      FSFXOptions := [SFXAskCmdLine];
    if soAskFiles in o then   // allow user to prevent certain files from extraction
      FSFXOptions := FSFXOptions + [SFXAskFiles];
    if soHideOverWriteBox in o then // do not allow user to choose the overwrite mode
      FSFXOptions := FSFXOptions + [SFXHideOverWriteBox];
    if soAutoRun in o then    // start extraction + evtl. command line automatically
      FSFXOptions := FSFXOptions + [SFXAutoRun]; // only if sfx filename starts with "!" or is "setup.exe"
    if soNoSuccessMsg in o then // don't show success message after extraction
      FSFXOptions := FSFXOptions + [SFXNoSuccessMsg];
  end;
{$ENDIF}
  Result := FSFXOptions;
end;

procedure TZipMaster.SetSFXOptions(aOpts: SfxOpts);
{$IFDEF INTERNAL_SFX}
var
  o         : TSFXOptions;
{$ENDIF}
begin
  if aOpts <> SFXOptions then
  begin
    FSFXOptions := aOpts;
{$IFDEF INTERNAL_SFX}
    if assigned(FAutoSFXSlave) then
    begin
      o := [];
      if SFXAskCmdLine in aOpts then
        o := o + [soAskCmdLine];
      if SFXAskFiles in aOpts then
        o := o + [soAskFiles];
      if SFXHideOverWriteBox in aOpts then
        o := o + [soHideOverWriteBox];
      if SFXAutoRun in aOpts then
        o := o + [soAutoRun];
      if SFXNoSuccessMsg in aOpts then
        o := o + [soNoSuccessMsg];
      TFriendSFX(FAutoSFXSlave).Options := o;
    end;
{$ENDIF}
  end;
end;

function TZipMaster.GetSFXOverWriteMode: OvrOpts;
begin
{$IFDEF INTERNAL_SFX}
  if assigned(FAutoSFXSlave) then
    case TFriendSFX(FAutoSFXSlave).OverwriteMode of
      somAsk: FSFXOverWriteMode := ovrConfirm;
      somOverwrite: FSFXOverWriteMode := ovrAlways;
      somSkip: FSFXOverWriteMode := ovrNever;
    end;
{$ENDIF}
  Result := FSFXOverWriteMode;
end;

procedure TZipMaster.SetSFXOverWriteMode(aOpts: OvrOpts);
begin
  if aOpts <> SFXOverWriteMode then
  begin
    FSFXOverWriteMode := aOpts;
{$IFDEF INTERNAL_SFX}
    if assigned(FAutoSFXSlave) then
    begin
      case aOpts of
        ovrConfirm: TFriendSFX(FAutoSFXSlave).OverwriteMode := somAsk;
        ovrAlways: TFriendSFX(FAutoSFXSlave).OverwriteMode := somOverwrite;
        ovrNever: TFriendSFX(FAutoSFXSlave).OverwriteMode := somSkip;
      end;
    end;
{$ENDIF}
  end;
end;

function TZipMaster.GetSFXPath: string;
begin
{$IFDEF INTERNAL_SFX}
  if assigned(FAutoSFXSlave) then
    FSFXPath := TFriendSFX(FAutoSFXSlave).SFXPath;
{$ENDIF}
  Result := FSFXPath;
end;

procedure TZipMaster.SetSFXPath(aString: string);
begin
  if aString <> SFXPath then
  begin
    fSFXPath := aString;
{$IFDEF INTERNAL_SFX}
    if assigned(FAutoSFXSlave) then
      TFriendSFX(FAutoSFXSlave).SFXPath := aString;
{$ENDIF}
  end;
end;
{$ENDIF}

(*? TZipMaster.ConvertSFX
1.73 15 July 2003 RA handling of exceptions
*)

function TZipMaster.ConvertSFX: Integer;
var
  slave     : TCustomZipSFX;
begin
  slave := GetSFXSlave;
  ErrCode := 0;
  FSuccessCnt := 1;

  Result := 0;
  with TFriendSFX(slave) do
  begin
    SourceFile := FZipFileName;
    TargetFile := ChangeFileExt(FZipFileName, '.exe');
  end;
  try
    Slave.ConvertToSFX;
  except
    on E: Exception do        // All CopyZippedFiles specific errors..
    begin
      if FUnattended = False then
        ShowMessage(E.Message);

      if Assigned(OnMessage) then
        OnMessage(Self, 0, E.Message);
      Result := 1;
      FSuccessCnt := 0;
    end;
  end;
end;
//? TZipMaster.ConvertSFX

function TZipMaster.NewSFXFile(const ExeName: string): Integer;
var
  slave     : TCustomZipSFX;
begin
  slave := GetSFXSlave;
  Result := 0;
  with TFriendSFX(slave) do
  begin
    SourceFile := FZipFileName;
    TargetFile := ExeName;
  end;
  Slave.CreateNewSFX;
end;

(*? TZipMaster.ConvertZIP
1.73 15 July 2003 RA handling of exceptions
{ Convert an .EXE archive to a .ZIP archive. }
{ returns 0 if good, or else a negative error code }
*)

function TZipMaster.ConvertZIP: Integer;
var
  slave     : TCustomZipSFX;
begin
  slave := GetSFXSlave;
  Result := 0;
  ErrCode := 0;
  FSuccessCnt := 1;
  with TFriendSFX(slave) do
  begin
    SourceFile := FZipFileName;
    TargetFile := ChangeFileExt(FZipFileName, '.zip');
  end;
  try
    Slave.ConvertToZip;
  except
    on E: Exception do        // All CopyZippedFiles specific errors..
    begin
      if FUnattended = False then
        ShowMessage(E.Message);

      if Assigned(OnMessage) then
        OnMessage(Self, 0, E.Message);
      Result := 1;
      FSuccessCnt := 0;
    end;
  end;
end;
//? TZipMaster.ConvertZIP

{* Return value:
 0 = The specified file is not a SFX
 1 = It is one
 -7  = Open, read or seek error
 -8  = memory error
 -9  = exception error
 -10 = all other exceptions
*}

function TZipMaster.IsZipSFX(const SFXExeName: string): Integer;
var
  r         : integer;
begin
  r := QueryZip(SFXExeName);  // SFX = 1 + 128 + 64
  Result := 0;
  if (r and (1 or 128 or 64)) = (1 or 128 or 64) then
    Result := 1;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('Delphi Zip', [TZipMaster]);
end;

end.

