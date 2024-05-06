(******************************************************************************)
(*   Komponente zur Datenübertragung über die serielle Schnittstelle          *)
(*                                                                            *)
(*   (c) 1998, 1999 Rainer Reusch und Toolbox                                 *)
(*   (c) 1998, 1999 Rainer Reusch, Computer & Literatur Verlag                *)
(*                                                                            *)
(*   Borland Delphi 2.0, 3.0, 4.01, 5.0                                       *)
(*                                                                            *)
(*   20.07.1999   WW  Änderungen für rein synchronen Zugriff ohne Ereignis-   *)
(*                    verarbeitung                                            *)
(******************************************************************************)

{
Hinweis:
Die im Implementation-Teil aufgeführte Konstante MaxPort ist auf den Wert 3
gesetzt. Das heißt, es wird bis COM4 geprüft, welche Schnittstellen vorhanden
sind. Sollen mehr Schnittstellen geprüft werden, ist der Wert der Konstanten
entsprechend zu erhöhen. Beachten Sie jedoch, daß sich damit die erforderliche
Zeit zur Initialisierung verlängert.

Änderung 20.07.1999  WW: MaxPort = 7
Änderung 29.11.2000  WW: MaxPort = 15
Änderung 09.12.2002  WW: MaxPort = 49 ( -> für MDETrans32)
Änderung 09.12.2005  WW: unterstützt Baudraten 50, 150, 28800; neue Funktionen
                         CheckOneSerPort, CheckOpenComm
Änderung 16.12.2005  WW: Aufruf von CheckSerPorts im Initialisation-Teil per
                         IFDEF AUTOSERIALPORTCHECK_OFF abschaltbar
Änderung 26.07.2011  WW: vorhandene COMs aus Registrierdatenbank lesen mit
                         EnumComPorts);
Änderung 05.10.2011  WW: threadsichere Methoden AktuOnePort, SetOneAvailPort,
                         ClearAvailPorts (TCustomSerial) und CheckOneSerPort
                         (global)
Änderung 05.01.2012  WW: neue Funktion TCustomSerial.AktuPorts
Änderung 14.11.2012  WW: Konstantendefinitionen in SerialConst.pas ausgelagert
}

unit Serial;

interface

uses
  Windows, SysUtils, Classes, SerialConst;

type
  // Übertragungsparameter
  TBaudrate  = (br_000050,br_000110,br_000150,br_000300,br_000600,br_001200,
                br_002400,br_004800,br_009600,br_014400,br_019200,br_028800,
                br_038400,br_056000,br_057600,br_115200,br_128000,br_230400,
                br_256000,br_460800,br_921600);
  TDataBits  = (db_4,db_5,db_6,db_7,db_8);
  TParityBit = (none,odd,even,mark,space);
  TStopBits  = (sb_1,sb_15,sb_2);

  // Struktur der Übertragungsparameter-Matrix
  TDataFormat = record
    Desc: string;
    DataBits: TDataBits;
    ParityBit: TParityBit;
    StopBits: TStopBits;
  end;

  // Typ für vorhandene Schnittstellen
  TAvailPorts = array[0..MaxPort] of boolean;

type
  // --- Basiskomponente TCustomSerial ---
  TCustomSerial = class(TComponent)
  private
    FPorts             : TStringList;      // vorhandene Schnittstellen ('COM1',...)
    FCOMPort           : integer;          // zu öffnende Schnittstelle (1..), 0 wenn keine vorhanden
    FSerHandle         : THandle;          // Handle der geöffneten Schnittstelle
    dcb                : TDCB;             // Device Control Block
    FAvailPorts        : TAvailPorts;  // vorhandene Schnittstellen (COM1... = true); 05.10.2011, WW
    FPortsExist        : boolean;  // true: mind. eine Schnittstelle ist vorhanden; 05.10.2011, WW
    procedure SetCOMPort(Value : integer);     // Schnittstelle auswählen
  public
    property SerHandle : THandle read FSerHandle;              // Schnittstellen-Handle
    constructor Create(AOwner: TComponent); override;          // Konstruktor
    destructor  Destroy; override;                             // Destruktor
    procedure SetPorts;
    procedure AktuOnePort (ACOMPort: integer);
    procedure AktuPorts;
    procedure SetOneAvailPort (ACOMPort: integer; bAvail: boolean);
    procedure ClearAvailPorts;
  published
    property Ports : TStringList read FPorts;        // Liste der verfügbaren Schnittstellen
    property COMPort : integer read FCOMPort write SetCOMPort; // zu verwendende Schnittstelle (1..)
  end;

  // --- Komponente TSerial ---
  TSerial = class(TCustomSerial)
  private
    { Private-Deklarationen }
    FActive            : boolean;          // true: Schnittstelle geöffnet/öffnen
    FBufSizeTrm        : cardinal;          // Größe des Sendepuffers
    FBufSizeRec        : cardinal;          // Größe des Empfangspuffers
    FBaudrate          : TBaudrate;        // Übertragungsrate
    FDataBits          : TDataBits;        // Anzahl Datenbits
    FParityBit         : TParityBit;       // Paritätsbit
    FStopBits          : TStopBits;        // Anzahl Stopp-Bits
    FHandshakeRtsCts   : boolean;          // true: Hardwire-Handshake RTS/CTS
    FHandshakeDtrDsr   : boolean;          // true: Hardwire-Handshake DTR/DSR
    FRTSActive         : boolean;          // true: RTS-Leitung aktiv (wenn nicht für Handshake verwendet)
    FDTRActive         : boolean;          // true: DTR-Leitung aktiv (wenn nicht für Handshake verwendet)
    FHandshakeXOnXOff  : boolean;          // true: Handshake nach XOn/XOff-Protokoll
    FXOnChar           : char;             // XOn-Zeichen
    FXOffChar          : char;             // XOff-Zeichen
    FXOffLimit         : cardinal;         // XOff-Limit (nicht größer als FBufSizeRec)
    FXOnLimit          : cardinal;         // XOn-Limit (nicht größer als XOff-Limit)
    FErrorChar         : char;             // Fehlerersatzzeichen (wenn UseErrorChar)
    FEofChar           : char;             // End-Of-File-Zeichen
    FEventChar         : char;             // Ereignis-Zeichen
    FParityCheck       : boolean;          // true: Paritätsprüfung (Paritätsbit einstellen!)
    FContinueOnXOff    : boolean;          // true: Übertragung auch bei XOff fortsetzen
    FUseErrorChar      : boolean;          // true: Fehlerersatzzeichen verwenden
    FEliminateNullChar : boolean;          // true: Null-Zeichen entfernen
    FAbortOnError      : boolean;          // true: Bei Fehler abbrechen
    function OpenIt (aCOMPort: integer): boolean;
    procedure SetActive(Value : boolean);      // öffnen/schließen
    procedure SetParityBit(Value : TParityBit);// Paritätsbit setzen
    function GetBufSizeTrm: cardinal;          // Größe des Sendepuffers lesen
    procedure SetBufSizeTrm(Value : cardinal); // Größe des Sendepuffers setzen
    function GetBufSizeRec: cardinal;          // Größe des Empfangspuffers lesen
    procedure SetBufSizeRec(Value : cardinal); // Größe des Empfangspuffers setzen
    procedure SetXOffLimit(Value : cardinal);  // XOff-Limit setzen
    procedure SetXOnLimit(Value : cardinal);   // XOn-Limit setzen
    function GetBufTrm : cardinal;             // Anzahl Zeichen, die sich noch im Sendepuffer befinden
    function GetBufRec : cardinal;             // Anzahl Zeichen, die sich im Empfangspuffer befinden
    procedure DataInBuffer(var InQueue, OutQueue : cardinal);  // Ermitteln, wieviele Zeichen sind im Sende-/Empfangspuffer befinden
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;  // Konstruktor
    destructor  Destroy; override;                     // Destruktor
    function OpenComm : boolean; virtual;   // Schnittstelle öffnen
    procedure CloseComm; virtual;           // Schnittstelle schließen
    function CheckOpenComm (aCOMPort: integer): boolean;  // beliebige Schnittstelle öffnen und wieder schließen
    function SetCommParameter (ABaudrate: TBaudrate; ADatabits: TDataBits;  // Schnittstellenparameter ändern
                               AParityBit: TParityBit; AStopBits: TStopBits): boolean;
    function SetDTRSignal: boolean;                                // DTR-Signal setzen
    function SetRTSSignal: boolean;                                // RTS-Signal setzen
    function SetBreakZustand: boolean;                             // Break-Zustand setzen
    function ClearDTRSignal: boolean;                              // DTR-Signal löschen
    function ClearRTSSignal: boolean;                              // RTS-Signal löschen
    function ClearBreakZustand: boolean;                           // Break-Zustand rückgängig machen
    function CheckCTSSignal: boolean;                              // CTS-Signal prüfen
    function CheckRLSDSignal: boolean;                             // RLSD-Signal prüfen
    procedure ResetComm;                                                        // Schnittstellenfehler löschen,
                                                                                // Sende-und Empfangspuffer löschen
    function TransmittData(var Data; DataSize: cardinal; bRxClear: boolean = true;
                           bTxClear: boolean = true): cardinal; virtual;        // Daten senden
    function TransmittChar(c: char): cardinal; virtual;                         // einzelnes Zeichen senden
    function ReceiveData(var Buf; BufSize: cardinal): cardinal; virtual;        // Daten empfangen
    function TransmittText(s : string; bRxClear: boolean = true;
                           bTxClear: boolean = true): cardinal; virtual;        // Text senden
    function ReceiveText: string; virtual;                                      // Text empfangen
    function KonvertBaudrate (ABaudrate: integer): TBaudrate;
    function KonvertDatabits (ADatabits: byte): TDatabits;
    function KonvertParity (AParity: byte): TParityBit;
    function KonvertStopBits (AStopBits: byte): TStopBits;
    property BufTrm : cardinal read GetBufTrm;                                  // Anzahl Zeichen im Sendepuffer
    property BufRec : cardinal read GetBufRec;                                  // Anzahl Zeichen im Empfangspuffer
  published
    { Published-Deklarationen }
    property Active : boolean read FActive write SetActive;                     // Schnittstelle öffnen/schließen
    property Baudrate : TBaudrate read FBaudrate write FBaudrate;               // Übertragungsrate
    property DataBits : TDataBits read FDataBits write FDataBits;               // Anzahl datenbits
    property ParityBit : TParityBit read FParityBit write SetParityBit;         // Paritätsbit
    property StopBits : TStopBits read FStopBits write FStopBits;               // Anzahl Stoppbits
    property BufSizeTrm : cardinal read GetBufSizeTrm write SetBufSizeTrm;      // Größe des Sendepuffers
    property BufSizeRec : cardinal read GetBufSizeRec write SetBufSizeRec;      // Größe des Empfangspuffers
    property HandshakeRtsCts : boolean read FHandshakeRtsCts write FHandshakeRtsCts;     // Hardwire-Handshake RTS/CTS
    property HandshakeDtrDsr : boolean read FHandshakeDtrDsr write FHandshakeDtrDsr;     // Hardwire-Handshake DTR/DSR
    property HandshakeXOnXOff : boolean read FHandshakeXOnXOff write FHandshakeXOnXOff;  // Handshake XOn/XOff-Protokoll
    property RTSActive : boolean read FRTSActive write FRTSActive;              // RTS-Ausgang aktiv setzen (wenn nicht zum Handshake benutzt)
    property DTRActive : boolean read FDTRActive write FDTRActive;              // DTR-Ausgang aktiv setzen (wenn nicht zum Handshake benutzt)
    property XOnChar : char read FXOnChar write FXOnChar;                       // XOn-Zeichen
    property XOffChar : char read FXOffChar write FXOffChar;                    // XOff-Zeichen
    property XOffLimit : cardinal read FXOffLimit write SetXOffLimit;           // XOff-Limit
    property XOnLimit : cardinal read FXOnLimit write SetXOnLimit;              // XOn-Limit
    property ErrorChar : char read FErrorChar write FErrorChar;                 // Fehlerersatzzeichen
    property EofChar : char read FEofChar write FEofChar;                       // EOF-Zeichen
    property EventChar : char read FEventChar write FEventChar;                 // Ereigniszeichen
    property ParityCheck : boolean read FParityCheck;                           // Paritätsprüfung verwenden
    property ContinueOnXOff : boolean read FContinueOnXOff write FContinueOnXOff;  // Übertragung fortsetzen auch bei XOff
    property UseErrorChar : boolean read FUseErrorChar write FUseErrorChar;     // Fehlerersatzzeichen verwenden
    property EliminateNullChar : boolean read FEliminateNullChar write FEliminateNullChar;  // Nullzeichen entfernen
    property AbortOnError : boolean read FAbortOnError write FAbortOnError;     // bei Fehler abbrechen
  end;


function CheckOneSerPort (ACOMPort: integer; ARegPorts: TStrings): boolean;
procedure EnumComPorts (COMPorts: TStrings);

function GetSerialDataFormat (sDataFormat: string; var DataBits: TDataBits;
  var ParityBit: TParityBit; var StopBits: TStopBits): boolean; overload;
function GetSerialDataFormat(iDataBits: TDataBits; iParityBit: TParityBit;
  iStopBits: TStopBits): string; overload;
function GetSerialDataString (ABaudrate: integer; ADataBits: TDataBits;
  AParityBit: TParityBit; AStopBits: TStopBits): string;


procedure Register;

implementation

resourcestring
  S1_5 = '1,5';   // 1,5 Stopbits

type
  // Flags von dcb.Flags
  TDCBFlag = (fBinary,fParity,fOutxCtsFlow,fOutxDsrFlow,fDtrControl,Dummy1,
              fDsrSensitivity,fTXContinueOnXoff,fOutX,fInX,fErrorChar,
              fNull,fRtsControl,Dummy2,fAbortOnError);

const
  Baudrates : array[0..20] of integer =
    (50,CBR_110,150,CBR_300,CBR_600,CBR_1200,CBR_2400,CBR_4800,
     CBR_9600,CBR_14400,CBR_19200,28800,CBR_38400,CBR_56000,
     CBR_57600,CBR_115200,CBR_128000,230400,CBR_256000,460800,921600);

  // Feld mit Definitionen für serielle Datenformate
  SerialDataFormats: array[1..4] of TDataFormat =
    ((Desc: CSDataFormat_8N1; Databits: db_8; ParityBit: none; StopBits: sb_1),
     (Desc: CSDataFormat_8E1; Databits: db_8; ParityBit: even; StopBits: sb_1),
     (Desc: CSDataFormat_8O1; Databits: db_8; ParityBit: none; StopBits: sb_1),
     (Desc: CSDataFormat_7E1; Databits: db_7; ParityBit: even; StopBits: sb_1)
    );

var
  AvailPorts: TAvailPorts;  // vorhandene Schnittstellen


(*** Allgemein ***)

function CheckOneSerPort (ACOMPort: integer; ARegPorts: TStrings): boolean;
{ prüft, ob eine serielle Schnittstelle vorhanden ist;
  Übergabe: COM-Nummer
  Ergebnis: true, wenn vorhanden }
var
  pDevName: array[0..10] of char;
  CommConfig: TCommConfig;
  CommConfigSize: cardinal;

begin
  Result:=false;
  CommConfigSize:=SizeOf(CommConfig);
  StrPCopy (pDevName,'COM'+IntToStr(ACOMPort));
  if Assigned (ARegPorts) then
    Result:=ARegPorts.IndexOf (string (pDevName)) >= 0;  // prüfen, ob COM in Registrierdatenbank eingetragen ist; 25.07.2011, WW
  if not Result then
    Result:=GetDefaultCommConfig(pDevName,CommConfig,CommConfigSize);  // Abwärtskompatibilität: Über API-Funktion prüfen, ob COM vorhanden
end;

procedure CheckSerPorts (var Avails: TAvailPorts);
// Prüft, welche seriellen Schnittstellen vorhanden sind
// Rückgabe: Avails
var
  j: cardinal;
  RegPorts: TStrings;

begin
  RegPorts:=TStringList.Create;
  try
    EnumComPorts (RegPorts);  // vorhandene COMs aus Registrierdatenbank lesen; 25.07.2011, WW
    for j:=0 to MaxPort do
      Avails[j]:=CheckOneSerPort(j+1, RegPorts);
  finally
    RegPorts.Free;
  end;
end;

{------------------------------------------}
procedure EnumComPorts (COMPorts: TStrings);
{------------------------------------------}
{ vorhandene COMs aus Registrierdatenbank lesen;
  Quelle: http://sourceforge.net/projects/comport/
  Übergabe: Stringliste mit vorhandenen COM-Ports (COM1, COM2 usw.) }
var
  KeyHandle: HKEY;
  ErrCode, Index: Integer;
  ValueName, Data: string;
  ValueLen, DataLen, ValueType: DWORD;
  TmpPorts: TStringList;

begin
  ErrCode := RegOpenKeyEx(
    HKEY_LOCAL_MACHINE,
    'HARDWARE\DEVICEMAP\SERIALCOMM',
    0,
    KEY_READ,
    KeyHandle);

  if ErrCode <> ERROR_SUCCESS then
  begin
    //raise EComPort.Create(CError_RegError, ErrCode);
    exit;
  end;

  try
    TmpPorts := TStringList.Create;
    try
      Index := 0;
      repeat
        ValueLen := 256;
        DataLen := 256;
        SetLength(ValueName, ValueLen);
        SetLength(Data, DataLen);
        ErrCode := RegEnumValue(
          KeyHandle,
          Index,
          PChar(ValueName),
          {$IFDEF DELPHI_4_OR_HIGHER}
          Cardinal(ValueLen),
          {$ELSE}
          ValueLen,
          {$ENDIF}
          nil,
          @ValueType,
          PByte(PChar(Data)),
          @DataLen);

        if ErrCode = ERROR_SUCCESS then
        begin
          if ValueType = REG_SZ then
            SetLength(Data, Pos (#0, Data) - 1)  // bei virtuellen Bluetooth-COMs liefert DataLen
                                                 // eine falsche Länge; 25.07.2011 WW
          else
            SetLength(Data, DataLen - 1);

          TmpPorts.Add(Data);
          Inc(Index);
        end
        else
          if ErrCode <> ERROR_NO_MORE_ITEMS then break;
            //raise EComPort.Create(CError_RegError, ErrCode);

      until (ErrCode <> ERROR_SUCCESS);

      TmpPorts.Sort;
      COMPorts.Assign(TmpPorts);
    finally
      TmpPorts.Free;
    end;
  finally
    RegCloseKey(KeyHandle);
  end;
end;

function GetSerialDataFormat (sDataFormat: string; var DataBits: TDataBits;
  var ParityBit: TParityBit; var StopBits: TStopBits): boolean;
// liefert zum übergebenen Datenformat-String (z.B. '8N1', '7E1') die zugeordneten
// Datenformat-Parameter; 21.04.2006 WW
// Ergebnis: true, wenn Datenformat-String übergeben wurde, für den eine Zuordnung
//           definiert ist
var
  i: byte;
begin
  Result:=false;
  for i:=Low (SerialDataFormats) to High (SerialDataFormats) do begin
    if SerialDataFormats [i].Desc = UpperCase (sDataFormat) then begin
      DataBits:=SerialDataFormats [i].DataBits;
      ParityBit:=SerialDataFormats [i].ParityBit;
      StopBits:=SerialDataFormats [i].StopBits;
      Result:=true;
      Break;
    end;
  end;
end;

// Gibt Datenstring aus Datenformat-Parametern zurück
// Parameter: Datenbits, Paritybit, Stopbits
// Rückgabe: Datenstring oder ''
//---------------------------------------------------------
function GetSerialDataFormat(iDataBits: TDataBits; iParityBit: TParityBit;
  iStopBits: TStopBits): string;
//---------------------------------------------------------
var
  i : byte;
begin
  Result := '';
  for i:=Low(SerialDataFormats) to High(SerialDataFormats) do begin
    if (SerialDataFormats[i].DataBits = iDataBits) and
      (SerialDataFormats[i].DataBits = iDataBits) and
      (SerialDataFormats[i].DataBits = iDataBits) then
    begin
      Result := SerialDataFormats[i].Desc;
      Break;
    end;
  end;
end;

{---------------------------------------------------------------------}
function GetSerialDataString (ABaudrate: integer; ADataBits: TDataBits;
  AParityBit: TParityBit; AStopBits: TStopBits): string;
{---------------------------------------------------------------------}
{ liefert Anzeigestring mit Schnittstellenparametern (z.B. 9600 8 E 1);
  Übergabe: Baudrate
            Datenbits
            Parität
            Stopbits
  Ergebnis: Anzeigestring }
var
  S: string;

begin
  Result:=IntToStr (ABaudrate);

  case ADataBits of
    db_4: S:='4';
    db_5: S:='5';
    db_6: S:='6';
    db_7: S:='7';
    db_8: S:='8';
  else
    S:='?';
  end;
  Result:=Result + ' ' + S;

  case AParityBit of
    none:  S:='N';
    odd:   S:='O';
    even:  S:='E';
    mark:  S:='M';
    space: S:='S';
  else
    S:='?';
  end;
  Result:=Result + ' ' + S;

  case AStopBits of
    sb_1:  S:='1';
    sb_15: S:=S1_5;
    sb_2:  S:='2';
  else
    S:='?';
  end;
  Result:=Result + ' ' + S;
end;


(*** TCustomSerial ***)

constructor TCustomSerial.Create(AOwner: TComponent);
// Konstruktor
var
  i: integer;

begin
  inherited Create(AOwner);
  FSerHandle:=INVALID_HANDLE_VALUE;  // ungültiges Handle

  FPortsExist:=false;
  for i:=Low (FAvailPorts) to High (FAvailPorts) do begin
    FAvailPorts [i]:=AvailPorts [i];  // aus globaler Variable übernehmen; 05.10.2011, WW
    FPortsExist:=FPortsExist or FAvailPorts [i];
  end;

  // vorhandene Schnittstellen übernehmen
  FPorts:=TStringList.Create;
  SetPorts;
end;

destructor TCustomSerial.Destroy;
// Destruktor
begin
  FPorts.Free;
  inherited Destroy;
end;

procedure TCustomSerial.SetPorts;
// vorhandene Schnittstellen in FPorts übernehmen
var
  j : integer;
begin
  FPorts.Clear;
  for j:=0 to MaxPort do
    if FAvailPorts[j] then FPorts.Add('COM'+IntToStr(j+1));
  if FPortsExist then
  begin
    for j:=MaxPort downto 0 do
      if FAvailPorts[j] then FCOMPort:=j+1;   // default: niedrigste verfügbare Schnittstelle
  end
  else FCOMPort:=0;  // keine Schnittstelle vorhanden
end;

procedure TCustomSerial.SetCOMPort(Value : integer);
// Zu verwendende Schnittstelle festlegen
// Value=1 -> COM1
// Nicht vorhandene Schnittstelle wird nicht akzeptiert
// Value=0 -> keine Schnittstelle
begin
  if (Value>=0) and (Value<=MaxPort+1) then
  begin
    FCOMPort:=0;
    if FPortsExist and (Value>0) then
    begin
      if FAvailPorts[Value-1] then FCOMPort:=Value;
    end;
  end;
end;

procedure TCustomSerial.AktuOnePort (ACOMPort: integer);
// Übergebene Schnittstelle auf Vorhandensein prüfen. FAvailPorts, FPortsExist und
// und FPorts aktualisieren; 05.10.2011, WW }
var
  ARegPorts: TStrings;
  bAvail: boolean;

begin
  ARegPorts:=TStringList.Create;
  try
    EnumComPorts (ARegPorts);  // vorhandene COMs aus Registrierdatenbank lesen; 05.10.2011, WW
    bAvail:=CheckOneSerPort (ACOMPort, ARegPorts);
  finally
    ARegPorts.Free;
  end;
  SetOneAvailPort (ACOMPort, bAvail);
  SetPorts;
end;

procedure TCustomSerial.AktuPorts;
// Alle Schnittstelle auf Vorhandensein prüfen. FAvailPorts, FPortsExist und
// und FPorts aktualisieren; 05.01.2012, WW }
var
  i: integer;
  
begin
  CheckSerPorts (FAvailPorts);

  FPortsExist:=false;
  for i:=Low (FAvailPorts) to High (FAvailPorts) do
    FPortsExist:=FPortsExist or FAvailPorts [i];
  SetPorts;
end;

procedure TCustomSerial.SetOneAvailPort (ACOMPort: integer; bAvail: boolean);
// setzt 'Vorhanden'-Status für serielle Schnittstelle in FAvailPorts ab
// FPortsExist wird aktualisiert
var
  i, j: integer;

begin
  j:=ACOMPort-1;
  if (j <= MaxPort) then begin
    FAvailPorts[j]:=bAvail;

    FPortsExist:=false;
    for i:=Low (FAvailPorts) to High (FAvailPorts) do begin
      if FAvailPorts [i] then begin
        FPortsExist:=true;
        Break;
      end;
    end;
  end;
end;

procedure TCustomSerial.ClearAvailPorts;
// löscht alle seriellen Schnittstellen aus FAvailPorts
// setzt FPortsExist auf false
var
  j: integer;

begin
  FPortsExist:=false;
  for j:=0 to MaxPort do
    FAvailPorts[j]:=false;
end;


(*** TSerial ***)

constructor TSerial.Create(AOwner: TComponent);
// Konstruktor
begin
  inherited Create(AOwner);
  // Grundeinstellungen
  FActive:=false;                    // geschlossen
  FBaudrate:=br_009600;              // 9600 Baud
  FDataBits:=db_8;                   // 8 Datenbit
  FStopBits:=sb_1;                   // 1 Stopbit
  FParityBit:=none;                  // keine Parität
  FBufSizeTrm:=1024;                 // Sendepuffergröße
  FBufSizeRec:=1024;                 // Empfangspuffergröße
  FHandshakeRtsCts:=false;           // kein Handshake
  FHandshakeDtrDsr:=false;
  FHandshakeXOnXOff:=false;
  FRTSActive:=false;
  FDTRActive:=false;
  FXOnChar:=#17;                     // XOn-Zeichen
  FXOffChar:=#19;                    // XOff-zeichen
  FXOffLimit:=1600;                  // XOff-Limit
  FXOnLimit:=400;                    // XOn-Limit
  FErrorChar:='?';                   // Fehlerersatzzeichen
  FEofChar:=#$1A;                    // EOF-Zeichen (^Z)
  FEventChar:=#$0D;                  // Ereigniszeichen
  FParityCheck:=false;               // Paritätsprüfung
  FContinueOnXOff:=false;            // Weiter senden trotz XOff
  FUseErrorChar:=false;              // Fehlerersatzzeichen nicht verwenden
  FEliminateNullChar:=false;         // Nullzeichen nicht entfernen
  FAbortOnError:=true;               // Bei Fehler abbrechen
end;

destructor TSerial.Destroy;
// Destruktor
begin
  CloseComm;
  inherited Destroy;
end;

function TSerial.OpenComm : boolean;
// Schnittstelle öffnen
begin
  Result:=false;
  if (not FActive) and (FCOMPort>0) then
    Result:=OpenIt (FCOMPort);
end;

function TSerial.OpenIt (aCOMPort: integer): boolean;
// übergebene Schnittstelle öffnen ohne Prüfung, ob sie vorhanden ist
var
  c : array[0..63] of char;
  ct: TCommTimeouts;
begin
  // Schnittstelle öffnen für synchronen Zugriff
  StrPCopy(c, '\\.\COM'+IntToStr(aCOMPort));
  FSerHandle:=CreateFile(c,
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0);
  if (FSerHandle<>INVALID_HANDLE_VALUE) then
  begin
    // Puffergrößen festlegen
    if (not SetupComm(FSerHandle,BufSizeRec,BufSizeTrm)) then
    begin
      CloseHandle(FSerHandle);
      FSerHandle:=INVALID_HANDLE_VALUE;
    end;
  end;
  if (FSerHandle<>INVALID_HANDLE_VALUE) then
  begin  // Schnittstellen initialisieren (DCB)
    dcb.DCBLength:=SizeOf(TDCB);
    c:='baud=9600 parity=N data=8 stop=1'+#0;
    BuildCommDCB(c,dcb);  // dcb
    dcb.BaudRate:=Baudrates[ord(Baudrate)];
    dcb.ByteSize:=ord(DataBits)+4;    // number of bits/byte, 4-8
    dcb.Parity:=ord(ParityBit);       // 0-4=no,odd,even,mark,space
    dcb.StopBits:=ord(StopBits);      // 0,1,2 = 1, 1.5, 2
    dcb.Flags:=1;  // fBinary
    dcb.Flags:=dcb.Flags or (ord(ParityCheck) shl 1); // fParity
    dcb.Flags:=dcb.Flags or (ord(HandshakeRTSCTS) shl 2); // fOutxCtsFlow
    dcb.Flags:=dcb.Flags or (ord(HandshakeDTRDSR) shl 3); // fOutxDsrFlow
    if HandshakeDTRDSR then dcb.Flags:=dcb.Flags or (DTR_CONTROL_HANDSHAKE shl 4)
    else
    if DTRActive then dcb.Flags:=dcb.Flags or (DTR_CONTROL_ENABLE shl 4)  // fDtrControl
    else dcb.Flags:=dcb.Flags or (DTR_CONTROL_DISABLE shl 4);  // fDtrControl
    // fDsrSensitivity=0
    dcb.Flags:=dcb.Flags or (ord (ContinueOnXOff) shl 7);  // fTXContinueOnXoff
    dcb.Flags:=dcb.Flags or (ord (HandshakeXOnXOff) shl 8);  // fOutX
    dcb.Flags:=dcb.Flags or (ord (HandshakeXOnXOff) shl 9);  // fInX
    dcb.Flags:=dcb.Flags or (ord (UseErrorChar) shl 10);  // fErrorChar
    dcb.Flags:=dcb.Flags or (ord (EliminateNullChar) shl 11);  // fNull
    if HandshakeRTSCTS then dcb.Flags:=dcb.Flags or (RTS_CONTROL_HANDSHAKE shl 12)
    else
    if RTSActive then dcb.Flags:=dcb.Flags or (RTS_CONTROL_ENABLE shl 12)  // fRtsControl
    else dcb.Flags:=dcb.Flags or (RTS_CONTROL_DISABLE shl 12);  // fRtsControl
    dcb.Flags:=dcb.Flags or (ord (AbortOnError) shl 14);  // fAbortOnError
    // fDummy2=0
    dcb.XonChar:=XOnChar;
    dcb.XoffChar:=XOffChar;
    dcb.XOnLim:=XOnLimit;
    dcb.XOffLim:=XOffLimit;
    dcb.ErrorChar:=ErrorChar;
    dcb.EofChar:=EofChar;
    dcb.EvtChar:=EventChar;
    if (not SetCommState(FSerHandle,dcb)) then
    begin  // Initialisierung fehlgeschlagen
      CloseHandle(FSerHandle);  // Schnittstelle schließen
      FSerHandle:=INVALID_HANDLE_VALUE;
    end;
  end;
  if (FSerHandle<>INVALID_HANDLE_VALUE) then
  begin  // Schnittstellen-Timeouts festlegen
    ct.ReadIntervalTimeout:=MaxDWord;        { sonst hängt irgendwann ReadFile unter NT ! }
    ct.ReadTotalTimeoutMultiplier:=0;
    ct.ReadTotalTimeoutConstant:=0;
    ct.WriteTotalTimeoutMultiplier:=0;
    ct.WriteTotalTimeoutConstant:=0;
    if not SetCommTimeouts(SerHandle, ct) then
    begin
      CloseHandle(FSerHandle);  // Schnittstelle schließen
      FSerHandle:=INVALID_HANDLE_VALUE;
    end;
  end;
  Result:=FSerHandle<>INVALID_HANDLE_VALUE;
  FActive:=Result;
end;

procedure TSerial.CloseComm;
// Schnittstelle schließen
begin
  if FActive then
  begin
    CloseHandle(FSerHandle);
    FSerHandle:=INVALID_HANDLE_VALUE;
    FActive:=false;
  end;
end;

function TSerial.CheckOpenComm (aCOMPort: integer): boolean;  // beliebige Schnittstelle öffnen und wieder schließen
// Prüfung, ob übergebene Schnittstelle geöffnet werden kann. Anschließend
// Schnittstelle gleich wieder schließen.
// Ergebnis: true, wenn Öffnen/Schließen erfolgreich
begin
  Result:=OpenIt(aCOMPort);
  CloseComm;
end;

procedure TSerial.DataInBuffer(var InQueue, OutQueue : cardinal);
// Anzahl Zeichen, die im Sende- und Empfangspuffer stehen
var
  ComStat : TComStat;
  e : cardinal;
begin
  InQueue:=0;
  OutQueue:=0;
  if FActive then
  begin
    if ClearCommError(FSerHandle,e,@ComStat) then
    begin
      InQueue:=ComStat.cbInQue;
      OutQueue:=ComStat.cbOutQue;
    end;
  end;
end;

procedure TSerial.SetActive(Value : boolean);
// Schnittstelle öffnen/schließen
begin
  if Value and (not FActive) then FActive:=OpenComm  // öffnen
  else
  if (not Value) and FActive then CloseComm;  // schließen
end;

procedure TSerial.SetParityBit(Value : TParityBit);
// Paritätsbit setzen
begin
  FParityBit:=Value;
  FParityCheck:=Value>none;  // Flag fParity (DCB)
end;

function TSerial.GetBufSizeTrm: cardinal;
// Größe des Sendepuffers lesen
begin
  Result:=FBufSizeTrm;
end;

procedure TSerial.SetBufSizeTrm(Value : cardinal);
// Größe des Sendepuffers festlegen
// Mindestgröße 16 Byte
begin
  if Value<16 then Value:=16;
  FBufSizeTrm:=Value;
end;

function TSerial.GetBufSizeRec: cardinal;
// Größe des Empfangspuffers lesen
begin
  Result:=FBufSizeRec;
end;

procedure TSerial.SetBufSizeRec(Value : cardinal);
// Größe des Empfangspuffers festlegen
// Mindestgröße 16 Byte
// XOff- und XOn-Limit werden bei Bedarf angepaßt
begin
  if Value<16 then Value:=16;
  FBufSizeRec:=Value;
  if FXOffLimit>Value then FXOffLimit:=Value;
  if FXOnLimit>=FXOffLimit then FXOnLimit:=FXOffLimit-1;
end;

procedure TSerial.SetXOffLimit(Value : cardinal);
// Oberer Füllgrad des Empfangspuffers, bei dem das XOff-Zeichen ausgegeben wird
// (XOn/XOff-Protokoll)
// Wert wird bei Bedarf korrigiert (nicht größer als Empfangspuffergröße und
// nicht kleiner als XOn-Limit)
begin
  if Value<=FXOnLimit then Value:=FXOnLimit+1;
  if Value>FBufSizeRec then Value:=FBufSizeRec;
  FXOffLimit:=Value;
end;

procedure TSerial.SetXOnLimit(Value : cardinal);
// Unterer Füllgrad des Empfangspuffers, bei dem das XOn-Zeichen ausgegeben wird
// (XOn/XOff-Protokoll)
// Wert wird bei Bedarf korrigiert (nicht größer XOff-Limit und
// nicht kleiner als 0)
begin
  if (Value>=FXOffLimit) AND (Value > 0) then Value:=FXOffLimit-1;
  FXOnLimit:=Value;
end;

function TSerial.GetBufTrm : cardinal;
// Anzahl Zeichen, die sich im Sendepuffer befinden, ermitteln
var
  i : cardinal;
begin
  DataInBuffer(i,Result);
end;

function TSerial.GetBufRec : cardinal;
// Anzahl Zeichen, die sich im Empfangspuffer befinden, ermitteln
var
  i : cardinal;
begin
  DataInBuffer(Result,i);
end;

function TSerial.SetCommParameter (ABaudrate: TBaudrate; ADatabits: TDataBits;
                                   AParityBit: TParityBit; AStopBits: TStopBits): boolean;
// Schnittstellen-Parameter ändern  WW
//  Übergabe: ABaudrate
//            ADataBits
//            AParityBit
//            AStopBit
//  Ergebnis: true, wenn Schnittstellenparameter geändert werden konnten, sonst false
var
  adcb: TDCB;
  aParityCheck: boolean;
  ComStat: TComStat;
  e: cardinal;
begin
  Result:=false;
  if FActive then begin
    ClearCommError(FSerHandle,e,@ComStat);
    if GetCommState (FSerHandle, adcb) then begin
      adcb.Baudrate:=Baudrates[ord(ABaudrate)];
      adcb.ByteSize:=ord(ADataBits)+4;    // number of bits/byte, 4-8
      adcb.Parity:=ord(AParityBit);       // 0-4=no,odd,even,mark,space
      adcb.StopBits:=ord(AStopBits);      // 0,1,2 = 1, 1.5, 2
      aParityCheck:=aParityBit > none;    // Flag fParity (DCB)
      if aParityCheck then
        adcb.Flags:=adcb.Flags or $02       // fParity setzen
      else
        adcb.Flags:=adcb.Flags and not $02; // fParity löschen
      if SetCommState(FSerHandle,adcb) then begin
        Baudrate:=ABaudrate;
        DataBits:=ADataBits;
        ParityBit:=AParityBit;
        StopBits:=AStopBits;
        FParityCheck:=AParityBit > none;
        Result:=true;
      end;
    end;
  end;
end;

function TSerial.SetDTRSignal: boolean;
// Sendet das Data-Terminal-Ready-Signal  WW
var
  ComStat: TComStat;
  e: cardinal;
begin
  ClearCommError(FSerHandle,e,@ComStat);
  Result:=EscapeCommFunction (SerHandle, SETDTR);
end;

function TSerial.SetRTSSignal: boolean;
// Sendet das Request-To-Send-Signal  WW
var
  ComStat: TComStat;
  e: cardinal;
begin
  ClearCommError(FSerHandle,e,@ComStat);
  Result:=EscapeCommFunction (SerHandle, SETRTS);
end;

function TSerial.SetBreakZustand: boolean;
// Break-Zustand setzen  WW
var
  ComStat: TComStat;
  e: cardinal;
begin
  ClearCommError(FSerHandle,e,@ComStat);
  Result:=EscapeCommFunction (SerHandle, SETBREAK);
end;

function TSerial.ClearDTRSignal: boolean;
// Löscht das Data-Terminal-Ready-Signal  WW
var
  ComStat: TComStat;
  e: cardinal;
begin
  ClearCommError(FSerHandle,e,@ComStat);
  Result:=EscapeCommFunction (SerHandle, CLRDTR);
end;

function TSerial.ClearRTSSignal: boolean;
// Löscht das Request-To-Send-Signal  WW
var
  ComStat: TComStat;
  e: cardinal;
begin
  ClearCommError(FSerHandle,e,@ComStat);
  Result:=EscapeCommFunction (SerHandle, CLRRTS);
end;

function TSerial.ClearBreakZustand: boolean;
// Break-Zustand rückgängig machen  WW
var
  ComStat: TComStat;
  e: cardinal;
begin
  ClearCommError(FSerHandle,e,@ComStat);
  Result:=EscapeCommFunction (SerHandle, CLRBREAK);
end;

function TSerial.CheckCTSSignal: boolean;
// Prüft das Clear-To-Send-Signal  WW
var
  ModemStat: cardinal;
begin
  if GetCommModemStatus(SerHandle, ModemStat) then
    Result:=(ModemStat AND MS_CTS_ON) <> 0
  else
    Result:=false;
end;

function TSerial.CheckRLSDSignal: boolean;
// Prüft das Receive-Line-Signal-Detect-Signal  WW
var
  ModemStat: cardinal;
begin
  if GetCommModemStatus(SerHandle, ModemStat) then
    Result:=(ModemStat AND MS_RLSD_ON) <> 0
  else
    Result:=false;
end;

procedure TSerial.ResetComm;
// Schnittstellen-Reset
var
  ComStat: TComStat;
  e: cardinal;

begin
  if FActive then begin
    ClearCommError(FSerHandle,e,@ComStat);
    PurgeComm (FSerHandle, PURGE_TXCLEAR);               { Sendepuffer leeren }
    PurgeComm (FSerHandle, PURGE_RXCLEAR);            { Empfangspuffer leeren }
  end;
end;

function TSerial.TransmittData(var Data; DataSize: cardinal;
                               bRxClear: boolean = true;
                               bTxClear: boolean = true): cardinal;
// Schnittstellen-Reset, Daten übertragen
// Data: Zeiger auf zu übertragende Daten
// DataSize: Umfang der zu übertragenden Daten
// bRxClear: wenn true, wird vor dem Senden der Empfangspuffer geleert
// bTxClear: wenn true, wird vor dem Senden der Sendepuffer geleert
// Ergebnis: tatsächlich übertragene (in Sendepuffer gestellte) Daten
var
  ComStat: TComStat;
  e: cardinal;
  count, p: cardinal;
  BytesWritten: cardinal;

begin
  if FActive AND (DataSize > 0) then begin
    if bTxClear OR bRxClear then
      ClearCommError(FSerHandle,e,@ComStat);
    if bTxClear then
      PurgeComm (FSerHandle, PURGE_TXCLEAR);             { Sendepuffer leeren }
    if bRxClear then
      PurgeComm (FSerHandle, PURGE_RXCLEAR);          { Empfangspuffer leeren }

    // Blockung der Sendedaten (unter Win98 werden sonst nicht mehr als BufSizeTrm
    // Zeichen gesendet !), 19.07.2004, WW
    p:=0;
    Result:=0;
    while p < DataSize do begin
      count:=DataSize - p;
      if count > BufSizeTrm then
        count:=BufSizeTrm;    // Blockung auf BufSizeTrm
      WriteFile(FSerHandle,pchar(@Data)[p],count,BytesWritten,nil);
      Result:=Result + BytesWritten;
      p:=p + count;
    end;
  end else
    Result:=0;
end;

function TSerial.TransmittChar(c: char): cardinal;
// einzelnes Zeichen übertragen ohne Schnittstellen-Reset
// c: zu übertragendes Zeichen
// Ergebnis: tatsächlich übertragene (in Sendepuffer gestellte) Daten
begin
  if FActive then begin
    WriteFile(FSerHandle,c,1,Result,nil);
  end else
    Result:=0;
end;

function TSerial.ReceiveData(var Buf; BufSize: cardinal): cardinal;
// Daten empfangen (Empfangspuffer leeren)
// Buf: Zeiger auf Puffer, welcher die Daten übernehmen soll
// BufSize: Größe des Puffers
// Ergebnis: Anzahl Datenbytes, die tatsächlich in den Puffer geschrieben wurden
begin
  if FActive then
    ReadFile(FSerHandle,Buf,BufSize,Result,nil)
  else
    Result:=0;
end;

function TSerial.TransmittText(s : string; bRxClear: boolean = true;
                               bTxClear: boolean = true): cardinal;
// Schnittstellen-Reset, Text übertragen
// s: Zu sendender Text
// bRxClear: wenn true, wird vor dem Senden der Empfangspuffer geleert
// bTxClear: wenn true, wird vor dem Senden der Sendepuffer geleert
// Ergebnis: tatsächlich übertragene (in Sendepuffer gestellte) Zeichen
//           (bei nicht geöffneter Schnittstelle 0)
var
  ComStat: TComStat;
  e: cardinal;
  Len: cardinal;
  BytesWritten: cardinal;
  count, p: cardinal;

begin
  Len:=length (s);
  if FActive AND (Len > 0) then begin
    if bTxClear OR bRxClear then
      ClearCommError(FSerHandle,e,@ComStat);
    if bTxClear then
      PurgeComm (FSerHandle, PURGE_TXCLEAR);             { Sendepuffer leeren }
    if bRxClear then
      PurgeComm (FSerHandle, PURGE_RXCLEAR);          { Empfangspuffer leeren }

    // Blockung der Sendedaten (unter Win98 werden sonst nicht mehr als BufSizeTrm
    // Zeichen gesendet !), 19.07.2004, WW
    p:=0;
    Result:=0;
    while p < Len do begin
      count:=Len - p;
      if count > BufSizeTrm then
        count:=BufSizeTrm;    // Blockung auf BufSizeTrm
      WriteFile(FSerHandle,s[p+1],count,BytesWritten,nil);
      Result:=Result + BytesWritten;
      p:=p + count;
    end;
  end else
    Result:=0;
end;

function TSerial.ReceiveText : string;
// Text empfangen (Empfangspuffer leeren)
// Ergebnis: Eingelesener Text
//           (bei nicht geöffneter Schnittstelle Leerstring)
var
  n, m : cardinal;
begin
  Result:='';
  if FActive then
  begin
    n:=BufRec;
    if n > 0 then begin
      SetLength(Result,n);
      ReadFile(FSerHandle,Result[1],n,m,nil);
    end;
  end;
end;

function TSerial.KonvertBaudrate (ABaudrate: integer): TBaudrate;
// Umsetzung Baudrate: Integer -> TBaudrate
begin
  if ABaudrate >= 921600 then
    Result := br_921600
  else if ABaudrate >= 460800 then
    Result := br_460800
  else if ABaudrate >= CBR_256000 then
    Result := br_256000
  else if ABaudrate >= 230400 then
    Result := br_230400
  else if ABaudrate >= CBR_128000 then
    Result := br_128000
  else if ABaudrate >= CBR_115200 then
    Result := br_115200
  else if ABaudrate >= CBR_57600 then
    Result := br_057600
  else if ABaudrate >= CBR_56000 then
    Result := br_056000
  else if ABaudrate >= CBR_38400 then
    Result := br_038400
  else if ABaudrate >= 28800 then
    Result := br_028800
  else if ABaudrate >= CBR_19200 then
    Result := br_019200
  else if ABaudrate >= CBR_14400 then
    Result := br_014400
  else if ABaudrate >= CBR_9600 then
    Result := br_009600
  else if ABaudrate >= CBR_4800 then
    Result := br_004800
  else if ABaudrate >= CBR_2400 then
    Result := br_002400
  else if ABaudrate >= CBR_1200 then
    Result := br_001200
  else if ABaudrate >= CBR_600 then
    Result := br_000600
  else if ABaudrate >= CBR_300 then
    Result := br_000300
  else if ABaudrate >= 150 then
    Result := br_000150
  else if ABaudrate >= CBR_110 then
    Result := br_000110
  else
    Result := br_000050;
end;

function TSerial.KonvertDatabits (ADatabits: byte): TDataBits;
// Umsetzung Datenbits: Integer -> TDataBits
begin
  if ADatabits >= 8 then
    Result := db_8
  else if ADatabits >= 7 then
    Result := db_7
  else if ADatabits >= 6 then
    Result := db_6
  else if ADatabits >= 5 then
    Result := db_5
  else
    Result := db_4;
end;

function TSerial.KonvertParity (AParity: byte): TParityBit;
// Umsetzung Parität: Integer -> TParityBit
begin
  if AParity >= SPACEPARITY then
    Result := space
  else if AParity >= MARKPARITY then
    Result := mark
  else if AParity >= EVENPARITY then
    Result := even
  else if AParity >= ODDPARITY then
    Result := odd
  else
    Result := none;
end;

function TSerial.KonvertStopBits (AStopBits: byte): TStopBits;
// Umsetzung Stopbits: Integer -> TStopBits
begin
  if AStopBits >= TWOSTOPBITS then
    Result := sb_2
  else if AStopBits >= ONE5STOPBITS then
    Result := sb_15
  else
    Result := sb_1;
end;


procedure Register;
// Komponentenregistrierung
begin
  RegisterComponents('Wieser', [TSerial]);
end;

begin
{$IFNDEF AUTOSERIALPORTCHECK_OFF}
  CheckSerPorts (AvailPorts);
{$ENDIF}

end.

