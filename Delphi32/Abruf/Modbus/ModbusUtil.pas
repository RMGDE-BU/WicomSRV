{******************************************************************************}
{* Unit: Basisroutinen für Modbus-Telegramme                                  *}
{* 23.06.2009  WW                                                             *}
{* 28.10.2009  WW: Bugfix Get_Modbus_LRC; Funktion "RMG lesen von/bis"        *}
{* 02.08.2010  GD: Funktionen für Modbus TCP/IP                               *}
{* 16.02.2015  GD: Funktionen überladen                                       *}
{* 28.06.2017  WW: Response-Validierung erweitert um Exception-Codes 0A, 0B;  *}
{*                 mit Modbusfunktionen 01 (Read Coil Status), 02 (Read Input *}
{*                 Status), 04 (Read Input Registers)                         *}
{* 08.03.2019  WW  Funktionen Get_Modbus_StartAdresse_Index0,                 *}
{*                 Get_Modbus_BinData; Modbus-Werttyp DT2                     *}
{* 22.07.2019  WW  mit Modbusfunktion 05 (Force single coil)                  *}
{* 13.11.2019  WW  ByteCount Ist/Soll-Plausibilisierung präzisiert in         *}
{*                 Get_Modbus_...-Funktionen; Modbus-Werttyp ile              *}
{* 16.12.2019  WW  Globale Modbus-TCP/IP-TID eliminiert (nicht Thread-sicher);*}
{*                 neue Funktionen InitTID_ModbusTCPIP,                       *}
{*                 IncrementTID_ModbusTCPIP; Get_Modbus_BinData mit Platz-    *}
{*                 halter-Definitionen <SYSTIME_Y_W>, <SYSTIME_M_W>,          *}
{*                 <SYSTIME_D_W>, <SYSTIME_H_W>, <SYSTIME_N_W>, <SYSTIME_S_W> *}
{* 18.02.2021  WW  mit Modbusfunktion 20 (Read General Reference) und Funktion*}
{*                 Init_RegisterKonvDataArchivRec                             *}
{* 29.09.2021  WW  Modbus-Werttypen Nle, Sle                                  *}
{* 09.01.2024  WW  Bugfix Binärdatenwandlung 0-terminierter Strings           *}
{******************************************************************************}
unit ModbusUtil;

interface

uses
  Windows, Classes, SysUtils, Contnrs, DateUtils,
  GD_Utils, WChars, WStrUtils, T_Zeit, T_BinMask, ErrConst, UnixDT, WSysCon,
  T_Tools;

resourcestring
  SModbusRTU   = 'Modbus RTU';
  SModbusASCII = 'Modbus ASCII';
  SModbusTCP   = 'Modbus TCP';

const
  { Modbus-Werttypen }
  C_MBWertTyp_B   = 'B';    // BYTE, numerisch unsigned char
  C_MBWertTyp_W   = 'W';    // WORD, unsigned integer
  C_MBWertTyp_W2  = 'W2';   // 2 WORD, unsigned integer (TME400); 18.02.2021, WW
  C_MBWertTyp_D   = 'D';    // DWORD, unsigned long
  C_MBWertTyp_U   = 'U';    // UNIX-Zeitinfo, DWORD
  C_MBWertTyp_N   = 'N';    // 0-terminierter String
  C_MBWertTyp_DT2 = 'DT2';  // DateTime 2 (Elgas, Primus); 08.03.2019, WW
  C_MBWertTyp_ile = 'ile';  // IPv4-Adresse, Little-Endian (Elgas, Primus); 13.11.2019, WW
  C_MBWertTyp_Nle = 'Nle';  // 0-terminierter String, Little-Endian (FLOWSIC500); 29.09.2021, WW
  C_MBWertTyp_Sle = 'Sle';  // String, Little-Endian (FLOWSIC500); 29.09.2021, WW

type
  TMyModbusSortList=class(TStringList)  // 16.02.2015
  private
    FCalculated   : boolean;
    FMySortedList : TMyTabList;
  protected
    function GetCalculatedCount: integer;
    function GetCalculatedRegAddr(iIndex: integer): integer;
    function GetCalculatedRegCount(iIndex: integer): integer;
    function GetBinaryString(iIndex: integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    function AddValue(iAddr: integer; cTyp: char; sValue: string): boolean;
    function Calculate: boolean;
    property Calculated: boolean read FCalculated;
    property CalcCount: integer read GetCalculatedCount;
    property CalcRegAddress [iIndex: integer]: integer
      read GetCalculatedRegAddr;
    property CalcRegCount [iIndex: integer]: integer
      read GetCalculatedRegCount;
    property CalcBinaryString [iIndex: integer]: string
      read GetBinaryString;
  end;

  { Modbus-Übertragungsmodi }
  TModbusModus = (modbus_RTU, modbus_ASCII, modbus_TCPIP);

  { Objektliste für Register-Konvertierungsdaten }
  TRegisterKonvList = class (TObjectList);

  { Struktur für Register-Konvertierungsdaten
    08.03.2019, WW: Jetzt packed und feste String-Längen für Speichern in Datei }
  TRegisterKonvData = packed record
    StartAdresse: word;
    Name: string [100];
    Typ: string [10];
    AnzahlBytes: integer;
    Wert: string [100];
    FktCode: byte;  // ab 28.06.2017, WW
  end;

  { Objekt für Register-Konvertierungsdaten }
  TRegisterKonvDataObj = class (TObject)
    Data: TRegisterKonvData;
  public
    procedure SetData (AData: TRegisterKonvData);
  end;

  { Struktur für Register-Konvertierungsdaten eines Modbus-Archivdatensatzes }
  TRegisterKonvDataArchivRec = array [1..31] of TRegisterKonvData;
  { -> Die max. Array-Größe ist so festzulegen, daß damit alle Werte der
       Modbus-Archivdatensätze (Messwerte, Meldungen) der im WICO unterstützten
       MRG-Gerätetypen zugewiesen werden können. Bei Bedarf erhöhen... }
  // 22.07.2019, WW: erhöht von 20 auf 31 für Primus 400

  { Objekt für Register-Konvertierungsdaten eines Modbus-Archivdatensatzes }
  TRegisterKonvDataArchivRecObj = class (TObject)
    Data: TRegisterKonvDataArchivRec;
  public
    procedure SetData (AData: TRegisterKonvDataArchivRec);
  end;

  { Liste für Register-Konvertierungsdaten eines Modbus-Archivdatensatzes }
  TRegisterKonvDataArchivRecList = class(TObjectList);


function Init_RegisterKonvDataArchivRec: TRegisterKonvDataArchivRec;

function InitTID_ModbusTCPIP: word;
function Get_Modbus_LRC (sBinMsg: string): byte;
function Get_Modbus_CRC (sBinMsg: string): word;

function Calc_Modbus_AnzahlRegister (AnzahlBytes: word): word; overload;
function Calc_Modbus_AnzahlRegister (cDataType: char): word; overload; {* 16.02.2015 *}
function Calc_Modbus_AnzahlBytes (AnzahlRegister: word): word; overload;
function Calc_Modbus_AnzahlBytes (cDataType: char): word; overload; {* 16.02.2015 *}
function Calc_Modbus_AnzahlBytes_Status (AnzahlStati: integer): word;

function Str2Modbus_BinData (sWert: string; ByteOrder: TByteOrder;
  sWertTyp: string; AnzahlBytes: word; var sBinData: string): integer;

function Get_Modbus_MaxDataBytes_01: integer;
function Get_Modbus_MaxDataBytes_02: integer;
function Get_Modbus_MaxDataBytes_03: integer;
function Get_Modbus_MaxDataBytes_04: integer;
function Get_Modbus_MaxDataBytes_20: integer;

function Get_Modbus_Query (SlaveAdresse, Funktion: byte; BinDaten: string;
  Modus: TModbusModus; var iTID: word): string;
function Get_Modbus_Query_01 (SlaveAdresse: byte; StartAusgang, AnzahlAusgaenge: word;
  Modus: TModbusModus; var iTID: word): string;
function Get_Modbus_Query_02 (SlaveAdresse: byte; StartEingang, AnzahlEingaenge: word;
  Modus: TModbusModus; var iTID: word): string;
function Get_Modbus_Query_03 (SlaveAdresse: byte;
  StartRegister, AnzahlRegister: word; Modus: TModbusModus; var iTID: word): string;
function Get_Modbus_Query_04 (SlaveAdresse: byte;
  StartRegister, AnzahlRegister: word; Modus: TModbusModus; var iTID: word): string;  // 28.06.2017, WW
function Get_Modbus_Query_05 (SlaveAdresse: byte; AusgangAdresse: word;
  BinAusgangData: string; Modus: TModbusModus; var iTID: word): string;
function Get_Modbus_Query_06 (SlaveAdresse: byte; RegisterAdresse: word;
  BinRegisterData: string; Modus: TModbusModus; var iTID: word): string;
function Get_Modbus_Query_08 (SlaveAdresse: byte; Unterfunktion: word;
  BinDiagnostikData: string; Modus: TModbusModus; var iTID: word): string;
function Get_Modbus_Query_16 (SlaveAdresse: byte; StartRegister, AnzahlRegister: word;
  BinRegisterData: string; Modus: TModbusModus; var Query: string; var iTID: word): boolean;
function Get_Modbus_Query_20 (SlaveAdresse: byte; FileNumber: word;
  StartRegister, AnzahlRegister: word; Modus: TModbusModus; var iTID: word): string;
function Get_Modbus_Query_52 (SlaveAdresse: byte;
  StartRegister, AnzahlRegister, Index_von, Index_bis: word; Modus: TModbusModus;
  var iTID: word): string;
function Get_Modbus_Quittung_52 (SlaveAdresse: byte; bPositiv: boolean;
  AnzahlRegister: word; Modus: TModbusModus; var iTID: word): string;

function Valid_Modbus_Response (SlaveAdresse_Soll, Funktion_Soll: byte;
  TID_Soll: word; Modus: TModbusModus; var Response: string;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;

function Get_Modbus_Response_01 (Response: string; Modus: TModbusModus;
  ByteOrder: TByteOrder; RegisterKonvList: TRegisterKonvList; AnzahlBytes_Soll: byte;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function Get_Modbus_Response_02 (Response: string; Modus: TModbusModus;
  ByteOrder: TByteOrder; RegisterKonvList: TRegisterKonvList; AnzahlBytes_Soll: byte;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function Get_Modbus_Response_03 (Response: string; Modus: TModbusModus;
  ByteOrder: TByteOrder; RegisterTyp: string; AnzahlBytes_Soll: byte;
  var sValue: string; var Fehlergruppe: integer; var Fehlercode: integer): boolean; overload;
function Get_Modbus_Response_03 (Response: string; Modus: TModbusModus;
  ByteOrder: TByteOrder; RegisterKonvList: TRegisterKonvList; AnzahlBytes_Soll: byte;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean; overload;
function Get_Modbus_Response_04 (Response: string; Modus: TModbusModus;
  ByteOrder: TByteOrder; RegisterKonvList: TRegisterKonvList; AnzahlBytes_Soll: byte;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;  // 28.06.2017, WW
function Get_Modbus_Response_05 (Response: string; Modus: TModbusModus;
  CoilAdresse_Soll: word; BinCoilData_Soll: string; var bPreset: boolean;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function Get_Modbus_Response_06 (Response: string; Modus: TModbusModus;
  RegisterAdresse_Soll: word; BinRegisterData_Soll: string; var bPreset: boolean;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function Get_Modbus_Response_16 (Response: string; Modus: TModbusModus;
  StartRegister_Soll, AnzahlRegister_Soll: word; var bPreset: boolean;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function Get_Modbus_Response_20 (Response: string; Modus: TModbusModus;
  ByteOrder: TByteOrder; RegisterKonvList: TRegisterKonvList; AnzahlBytes_Soll: byte;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function Get_Modbus_Response_52 (Response: string; Modus: TModbusModus;
  ByteOrder: TByteOrder; AnzahlBytes_Soll: byte;
  var sValue: string; var Fehlergruppe: integer; var Fehlercode: integer): boolean;

function Modbus_BinData2Str (sBinData: string; ByteOrder: TByteOrder;
  sWertTyp: string): string;

function Get_Modbus_StartAdresse_Index0 (iStartAdresse: word): word;
function Get_Modbus_BinData (sWert: string; ByteOrder: TByteOrder;
  WertTyp: string; Anzahl: word; var sBinData: string;
  var sInfoWert_Aktuell: string): integer;

implementation


type
  TTableCRC = array [0..255] of byte;

const
  { Tabelle mit CRC-Werten für das high-order Byte: }
  TableCRCHi: TTableCRC = (
    $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,
    $00,$C1,$81,$40,$01,$C0,$80,$41,$00,$C1,$81,$40,
    $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,
    $00,$C1,$81,$40,$00,$C1,$81,$40,$01,$C0,$80,$41,
    $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,
    $00,$C1,$81,$40,$01,$C0,$80,$41,$00,$C1,$81,$40,
    $00,$C1,$81,$40,$01,$C0,$80,$41,$00,$C1,$81,$40,
    $01,$C0,$80,$41,$01,$C0,$80,$41,$00,$C1,$81,$40,
    $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,
    $00,$C1,$81,$40,$01,$C0,$80,$41,$00,$C1,$81,$40,
    $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,
    $00,$C1,$81,$40,$00,$C1,$81,$40,$01,$C0,$80,$41,
    $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,
    $00,$C1,$81,$40,$00,$C1,$81,$40,$01,$C0,$80,$41,
    $01,$C0,$80,$41,$00,$C1,$81,$40,$01,$C0,$80,$41,
    $00,$C1,$81,$40,$00,$C1,$81,$40,$01,$C0,$80,$41,
    $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,
    $00,$C1,$81,$40,$01,$C0,$80,$41,$00,$C1,$81,$40,
    $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,
    $00,$C1,$81,$40,$00,$C1,$81,$40,$01,$C0,$80,$41,
    $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,
    $00,$C1,$81,$40
  );

  { Tabelle mit CRC-Werten für das low-order Byte: }

  TableCRCLo: TTableCRC = (
    $00,$C0,$C1,$01,$C3,$03,$02,$C2,$C6,$06,$07,$C7,
    $05,$C5,$C4,$04,$CC,$0C,$0D,$CD,$0F,$CF,$CE,$0E,
    $0A,$CA,$CB,$0B,$C9,$09,$08,$C8,$D8,$18,$19,$D9,
    $1B,$DB,$DA,$1A,$1E,$DE,$DF,$1F,$DD,$1D,$1C,$DC,
    $14,$D4,$D5,$15,$D7,$17,$16,$D6,$D2,$12,$13,$D3,
    $11,$D1,$D0,$10,$F0,$30,$31,$F1,$33,$F3,$F2,$32,
    $36,$F6,$F7,$37,$F5,$35,$34,$F4,$3C,$FC,$FD,$3D,
    $FF,$3F,$3E,$FE,$FA,$3A,$3B,$FB,$39,$F9,$F8,$38,
    $28,$E8,$E9,$29,$EB,$2B,$2A,$EA,$EE,$2E,$2F,$EF,
    $2D,$ED,$EC,$2C,$E4,$24,$25,$E5,$27,$E7,$E6,$26,
    $22,$E2,$E3,$23,$E1,$21,$20,$E0,$A0,$60,$61,$A1,
    $63,$A3,$A2,$62,$66,$A6,$A7,$67,$A5,$65,$64,$A4,
    $6C,$AC,$AD,$6D,$AF,$6F,$6E,$AE,$AA,$6A,$6B,$AB,
    $69,$A9,$A8,$68,$78,$B8,$B9,$79,$BB,$7B,$7A,$BA,
    $BE,$7E,$7F,$BF,$7D,$BD,$BC,$7C,$B4,$74,$75,$B5,
    $77,$B7,$B6,$76,$72,$B2,$B3,$73,$B1,$71,$70,$B0,
    $50,$90,$91,$51,$93,$53,$52,$92,$96,$56,$57,$97,
    $55,$95,$94,$54,$9C,$5C,$5D,$9D,$5F,$9F,$9E,$5E,
    $5A,$9A,$9B,$5B,$99,$59,$58,$98,$88,$48,$49,$89,
    $4B,$8B,$8A,$4A,$4E,$8E,$8F,$4F,$8D,$4D,$4C,$8C,
    $44,$84,$85,$45,$87,$47,$46,$86,$82,$42,$43,$83,
    $41,$81,$80,$40
  );


{ TRegisterKonvDataObj }

{----------------------------------------------------------------}
procedure TRegisterKonvDataObj.SetData (AData: TRegisterKonvData);
{----------------------------------------------------------------}
begin
  Data:=AData;
end;


{ TRegisterKonvDataArchivRec }

{------------------------------------------------------------------}
function Init_RegisterKonvDataArchivRec: TRegisterKonvDataArchivRec;
{------------------------------------------------------------------}
{ Liefert mit 'fehlend' vorbelegten Modbus-Archivdatensatz }
var
  i: integer;

begin
  for i:=Low (Result) to High (Result) do begin
    with Result [i] do begin
      StartAdresse:=0;
      Name:='';
      Typ:='';
      AnzahlBytes:=-1;
      Wert:='';
      FktCode:=0;
    end;
  end;
end;


{ TRegisterKonvDataArchivRecObj }

{----------------------------------------------------------------------------------}
procedure TRegisterKonvDataArchivRecObj.SetData (AData: TRegisterKonvDataArchivRec);
{----------------------------------------------------------------------------------}
begin
  Data:=AData;
end;


//------------------------------ TMyModbusSortList -----------------------------

//----------------------------------------------
constructor TMyModbusSortList.Create;
//----------------------------------------------
begin
  inherited;

  FMySortedList := TMyTabList.Create;
  FCalculated := False;
end;

//----------------------------------------------
destructor TMyModbusSortList.Destroy;
//----------------------------------------------
begin
  FreeAndNil(FMySortedList);

  inherited;
end;

//----------------------------------------------
function TMyModbusSortList.AddValue(
  iAddr: integer; cTyp: char; sValue: string): boolean;
//----------------------------------------------
var
  sBinData : string;
  i        : integer;
begin
  Result := (Str2Modbus_BinData(
    sValue, bo_BigEndian, cTyp, Calc_Modbus_AnzahlBytes(cTyp), sBinData) = 0);
  if (Result) then begin
    for i := 1 to Calc_Modbus_AnzahlRegister(cTyp) do begin
      Add(Format('%.8d', [iAddr + (i-1)]) + ';' + Copy(sBinData, (i-1)*2 + 1, 2));
    end;
    FCalculated := False;
  end;
end;

//----------------------------------------------
function TMyModbusSortList.Calculate: boolean;
//----------------------------------------------
var
  i, iRegAddr, iAktAddr : integer;
begin
  try
    // Bisherige Ausgaben löschen
    FMySortedList.Clear;
    // Gespeicherte Strings nach Registeradressen sortieren
    Self.Sort;
    // Schleife über die nach Registeradressen sortierten Strings
    iRegAddr := -2;  // Aktuelle Registeradresse
    for i := 0 to Count-1 do begin
      // Aktuelles Register steht vor ';'
      iAktAddr := StrToInt(GetStringPart(Strings[i], 1, ';'));
      // Wenn Registernummer nicht fortlaufend oder Puffer zu groß :
      //   neuen Puffer eröffnen
      if (iAktAddr <> iRegAddr+1) or
        (FMySortedList.Col[FMySortedList.Count-1].Count > 100)
      then FMySortedList.AddObject(IntToStr(iAktAddr), TStringList.Create);
      // Aktueller Registerinhalt steht nach ';'
      FMySortedList.Col[FMySortedList.Count-1].Add(
        GetStringPart(Strings[i], 2, ';'));
      // Registernummer merken
      iRegAddr := iAktAddr;
    end;
    Result := True;
  except
    Result := False;
  end;
  FCalculated := Result;
end;

//----------------------------------------------
function TMyModbusSortList.GetCalculatedCount: integer;
//----------------------------------------------
begin
  if (FCalculated) then Result := FMySortedList.Count else Result := -1;
end;

//----------------------------------------------
function TMyModbusSortList.GetCalculatedRegAddr(iIndex: integer): integer;
//----------------------------------------------
begin
  if (FCalculated) and (iIndex >= 0) and (iIndex < FMySortedList.Count)
  then Result := StrToInt(FMySortedList[iIndex])
  else Result := -1;
end;

//----------------------------------------------
function TMyModbusSortList.GetCalculatedRegCount(iIndex: integer): integer;
//----------------------------------------------
begin
  if (FCalculated) and (iIndex >= 0) and (iIndex < FMySortedList.Count)
  then Result := FMySortedList.Col[iIndex].Count
  else Result := -1;
end;

//----------------------------------------------
function TMyModbusSortList.GetBinaryString(iIndex: integer): string;
//----------------------------------------------
var
  i : integer;
begin
  if (FCalculated) and (iIndex >= 0) and (iIndex < FMySortedList.Count) then
  begin
    Result := '';
    with FMySortedList.Col[iIndex] do
      for i := 0 to Count-1 do Result := Result + Strings[i];
  end
  else Result := '';
end;

{------------------------------------------------------------------------------}

{---------------------------------}
function InitTID_ModbusTCPIP: word;
{---------------------------------}
{ Modbus-TCP/IP-Transaktionsnummer initialisieren;
  Ergebnis: Initialisierte Transaktionsnummer }
begin
  Result := 0;
end;

{--------------------------------------------------}
procedure IncrementTID_ModbusTCPIP (var iTID: word);
{--------------------------------------------------}
{ Modbus-TCP/IP-Transaktionsnummer erhöhen;
  Übergabe: Aktuelle Transaktionsnummer
  Rückgabe: Nächste Transaktionsnummer }
begin
  if iTID < $FFFF then  // Anm.: bisher Überlauf bei $0FFF ?? (GD)
    inc (iTID)
  else
    iTID := 1;
end;

{----------------------------------------------}
function Get_Modbus_LRC (sBinMsg: string): byte;
{----------------------------------------------}
{ liefert LRC für Modbus ASCII-Telegramm (Message);
  Übergabe: Message als Binär-String }
var
  i: integer;
  iLRC: byte;

begin
  iLRC:=0;
  for i:=1 to length (sBinMsg) do
    iLRC:=(iLRC + Ord (sBinMsg [i])) AND $FF;
  // 2er-Komplement bilden:
  if iLRC > 0 then
    Result := (not iLRC) + 1
  else
    Result:=iLRC;
end;

{----------------------------------------------}
function Get_Modbus_CRC (sBinMsg: string): word;
{----------------------------------------------}
{ liefert CRC für Modbus RTU-Telegramm (Message);
  Übergabe: Message als Binär-String }
var
  bCRCHi: byte;
  bCRCLo: byte;
  iIndex: integer;
  i: integer;

begin
  bCRCHi:=$FF;
  bCRCLo:=$FF;

  for i:=1 to length (sBinMsg) do begin
    iIndex:=bCRCHi XOR Ord (sBinMsg [i]);
    bCRCHi:=bCRCLo XOR TableCRCHi [iIndex AND $FF];
    bCRCLo:=TableCRCLo [iIndex];
  end;
  Result:=(bCRCHi SHL 8) OR bCRCLo;
end;

{------------------------------------------------------------}
function Calc_Modbus_AnzahlRegister (AnzahlBytes: word): word;
{------------------------------------------------------------}
{ ermittelt für eine Anzahl von Bytes die Anzahl der benötigten Register;
  Übergabe: Anzahl Bytes
  Ergebnis: Anzahl der Register }
begin
  Result:=AnzahlBytes DIV 2;  // Registerlänge ist word (2 Byte)
  if Odd (AnzahlBytes) then
    Result:=Result + 1;  // bei ungerader Byte-Anzahl wird ein Register mehr benötigt
end;

{------------------------------------------------------------}
function Calc_Modbus_AnzahlRegister (cDataType: char): word;
{------------------------------------------------------------}
{ ermittelt für eine Anzahl von Bytes die Anzahl der benötigten Register;
  Übergabe: Kenn-Zeichen
  Ergebnis: Anzahl der Register }
begin
  Result := (Calc_Modbus_AnzahlBytes(cDataType) div 2);
end;

{------------------------------------------------------------}
function Calc_Modbus_AnzahlBytes (AnzahlRegister: word): word;
{------------------------------------------------------------}
{ ermittelt für eine Anzahl von Registern die entsprechende Anzahl der Bytes;
  Übergabe: Anzahl der Register
  Ergebnis: Anzahl Bytes }
begin
  Result:=AnzahlRegister * 2;  // Registerlänge ist word (2 Byte)
end;

{------------------------------------------------------------}
function Calc_Modbus_AnzahlBytes (cDataType: char): word;
{------------------------------------------------------------}
{ ermittelt für eine Anzahl von Registern die entsprechende Anzahl der Bytes;
  Übergabe: Kenn-Zeichen
  Ergebnis: Anzahl Bytes }
begin
  case cDataType of
    'I', C_MBWertTyp_W: Result := 2;
    'F', 'L', C_MBWertTyp_D: Result := 4;
    'E': Result := 8;
    else Result := 0;
  end;
end;

{-------------------------------------------------------------------}
function Calc_Modbus_AnzahlBytes_Status (AnzahlStati: integer): word;
{-------------------------------------------------------------------}
{ ermittelt für eine Anzahl von Stati (Bits) die entsprechende Anzahl der Bytes;
  Übergabe: Anzahl der Stati
  Ergebnis: Anzahl Bytes }
begin
  Result:=((AnzahlStati - 1) DIV 8) + 1;  // 8 Status-Bits je Byte
end;

{--------------------------------------------------------------------}
function Str2Modbus_BinData (sWert: string; ByteOrder: TByteOrder;
  sWertTyp: string; AnzahlBytes: word; var sBinData: string): integer;
{--------------------------------------------------------------------}
{ wandelt typabhängig einen Wert-String in Binärdaten-Format;
  Übergabe: Wert als String
               -> Floats (Typ E, F): Dezimalzeichen wie in Systemsteuerung festegelegt
               -> Datum/Zeitinfo (Typ U, s): lesbares Format wie in Systemsteuerung
                  festegelegt (z.B. deutsch: dd.mm.yyyy hh:nn:ss)
               -> IP-Adressen (Typ i): Format xxx.xxx.xxx.xxx
            Byte-Order (Big-Endian/Little-Endian)
            Typ des Wertes
            Anzahl Bytes in Binärdaten-Rückgabe (für Registertyp N, S)
  Rückgabe: Wert in Binärdaten-Format
  Ergebnis:  0 = Wandlung in Binärdaten-Format OK
            -1 = unbekannter Werttyp
            -2 = Wert ist außerhalb des Gültigkeitsbereichs des Werttyps
            -3 = kein Werttyp angegeben }
var
  c: cardinal;
  w: word;
  smi: smallint;
  l: longint;
  b: byte;
  si: single;
  d: double;
  dt: TDateTime;
  S: string;
  year, month, day, hour, minute, second, millisecond: word;
  i64: Int64;
  sBuf: string;

begin
  Result:=-2;  // Wert ist außerhalb des Gültigkeitsbereichs des Werttyps
  sBinData:='';  // Vorbelegung Rückgabe

  if length (sWertTyp) > 0 then begin
    // RMG-Werttypen (EC 900, M. Ullmann)
    if (sWertTyp = 'F') then begin  // float
      try
        si:=StrToFloat (sWert);
        sBinData:=Single2Bin (si, ByteOrder);
      except
        exit;
      end;
    end
    else if (sWertTyp = 'E') then begin  // double
      try
        d:=StrToFloat (sWert);
        sBinData:=Double2Bin (d, ByteOrder);
      except
        exit;
      end;
    end
    else if (sWertTyp = 'I') then begin  // integer
      try
        smi:=StrToInt (sWert);
        sBinData:=SmallInt2Bin (smi, ByteOrder);
      except
        exit;
      end;
    end
    else if (sWertTyp = C_MBWertTyp_W) then begin  // WORD, unsigned integer
      try
        w:=StrToInt (sWert);
        sBinData:=Word2Bin (w, ByteOrder);
      except
        exit;
      end;
    end
    else if (sWertTyp = C_MBWertTyp_W2) then begin  // 2 WORD, unsigned integer; 18.02.2021, WW
      try
        c:=StrToInt (sWert);
        sBinData:=Longword2Bin (c, ByteOrder);
        sBinData:=Copy (sBinData, 3, 2) + Copy (sBinData, 1, 2);
      except
        exit;
      end;
    end
    else if (sWertTyp = 'L') then begin  // long
      try
        l:=StrToInt (sWert);
        sBinData:=Integer2Bin (l, ByteOrder);
      except
        exit;
      end;
    end
    else if (sWertTyp = C_MBWertTyp_D) then begin  // DWORD, unsigned long
      try
        c:=StrToInt (sWert);
        sBinData:=Longword2Bin (c, ByteOrder);
      except
        exit;
      end;
    end
    else if (sWertTyp = 'LL') then begin  // long long; 08.03.2019, WW
      try
        i64:=StrToInt64 (sWert);
        sBinData:=Int64_2Bin (i64, ByteOrder);
      except
        exit;
      end;
    end
    else if (sWertTyp = 'C') then begin  // char
      if length (sWert) = 0 then exit;
      try
        b:=Ord (sWert [1]);
        sBinData:=Word2Bin (b, ByteOrder);  // Registerlänge ist immer mind. Word
      except
        exit;
      end;
    end
    else if (sWertTyp = C_MBWertTyp_B) then begin  // BYTE, numerisch unsigned char
      try
        b:=StrToInt (sWert);
        sBinData:=Word2Bin (b, ByteOrder);  // Registerlänge ist immer mind. Word
      except
        exit;
      end;
    end
    else if (sWertTyp = '1') OR (sWertTyp = '2') OR (sWertTyp = '3') OR
            (sWertTyp = '4') OR (sWertTyp = '5') OR (sWertTyp = '6') OR
            (sWertTyp = '7') OR (sWertTyp = '8') OR (sWertTyp = '9') then begin  // BYTE, mit Wertebereich
      try
        b:=StrToInt (sWert);
        if b > StrToInt (sWertTyp) then exit;
        sBinData:=Word2Bin (b, ByteOrder);  // Registerlänge ist immer mind. Word
      except
        exit;
      end;
    end
    else if (sWertTyp = C_MBWertTyp_N) OR (sWertTyp = 'S') OR  // 0-terminierter String, String
            (sWertTyp = C_MBWertTyp_Nle) OR  // 0-terminierter String (fix Little-Endian); 29.09.2021, WW
            (sWertTyp = C_MBWertTyp_Sle) then begin  // String (fix Little-Endian); 29.09.2021, WW
      sBinData:=Copy (sWert, 1, AnzahlBytes);
      if length (sBinData) < AnzahlBytes then
        sBinData:=F_RightPad (sBinData, NUL, AnzahlBytes);  // mit NUL-Zeichen bis zur vollen Länge auffüllen
      if (sWertTyp = C_MBWertTyp_Nle) OR
         (sWertTyp = C_MBWertTyp_Sle) then
        sBinData:=EndianString (sBinData);  // 29.09.2021, WW
    end
    else if (sWertTyp = C_MBWertTyp_U) OR  // UNIX-Zeitinfo, DWORD
            (sWertTyp = 's') then begin  // Systemzeit (UNIX)
      try
        dt:=StrToDateTime (sWert);
      except
        exit;
      end;
      c:=GetUnixSekundenFromDateTime (dt);
      sBinData:=Longword2Bin (c, ByteOrder);
    end
    else if ((sWertTyp = 'i')) OR  // IPv4
            ((sWertTyp = C_MBWertTyp_ile)) then begin  // IPv4 (fix Little-Endian); 13.11.2019, WW
      try
        S:='';
        while sWert <> '' do begin
          sBuf:=F_Zerlegen (sWert, '.');  // Adress-Segmente mit variabler Länge (bisher fest Länge 3); 13.11.2019, WW
          b:=StrToInt (sBuf);
          S:=S + Chr (b);
        end;
      except
        exit;
      end;

      if length (S) <> 4 then exit;  // Prüfung Länge IPv4; 13.11.2019, WW
      if (sWertTyp = 'i') then  // IPv4
        c:=Bin2Longword (S, bo_BigEndian)  // IP-Adresse xxx.xxx.xxx.xxx liegt in "Big-Endian" vor
      else
        c:=Bin2Longword (S, bo_LittleEndian);  // 13.11.2019, WW
      sBinData:=Longword2Bin (c, ByteOrder)
    end

    // Elgas-Werttypen (Primus)
    else if (sWertTyp = C_MBWertTyp_DT2) then begin  // DateTime 2; 08.03.2019, WW
      try
        dt:=StrToDateTime (sWert);
      except
        exit;
      end;

      DecodeDateTime(dt, year, month, day, hour, minute, second, millisecond);
      if year >= 2000 then
        // 6 Bytes BCD: y m d h n s
        sBinData:=Chr (IntToBCD (year - 2000)) +  // Jahr
                  Chr (IntToBCD (month)) +        // Monat
                  Chr (IntToBCD (day)) +          // Tag
                  Chr (IntToBCD (hour)) +         // Stunde
                  Chr (IntToBCD (minute)) +       // Minute
                  Chr (IntToBCD (second))         // Sekunde
      else
        exit;  // ungültiges Jahr
    end
    else begin
      Result:=-1;  // unbekannter Registertyp
      exit;
    end;
  end
  else begin
    Result:=-3;  // kein Registertyp
    exit;
  end;

  Result:=0;  // OK
end;

{-------------------------------------------------------------------}
function Modbus_BinData2Str (sBinData: string; ByteOrder: TByteOrder;
  sWertTyp: string): string;
{-------------------------------------------------------------------}
{ wandelt typabhängig Modbus-Binärdaten in formatierten Wert-String;
  Übergabe: Binärdaten-String
            Byte-Order (Big-Endian/Little-Endian)
            Typ des Wertes
  Ergebnis: formatierter Wert }
var
  c: cardinal;
  dt: TDateTime;
  S: string;

begin
  if length (sWertTyp) > 0 then begin
    // RMG-Werttypen (EC 900, M. Ullmann)
    if (sWertTyp = 'F') then
      Result:=FloatToStr (Bin2Single (sBinData, ByteOrder))  // float
    else if (sWertTyp = 'E') then
      Result:=FloatToStr (Bin2Double (sBinData, ByteOrder))  // double
    else if (sWertTyp = 'I') then
      Result:=IntToStr (Bin2SmallInt (sBinData, ByteOrder))  // integer
    else if (sWertTyp = C_MBWertTyp_W) then
      Result:=IntToStr (Bin2Word (sBinData, ByteOrder))  // WORD, unsigned integer
    else if (sWertTyp = C_MBWertTyp_W2) then begin
      S:=Copy (sBinData, 3, 2) + Copy (sBinData, 1, 2);  // 2 WORD, unsigned integer; 18.02.2021, WW
      Result:=IntToStr (Bin2LongWord (S, ByteOrder));
    end
    else if (sWertTyp = 'L') then
      Result:=IntToStr (Bin2Integer (sBinData, ByteOrder))  // long
    else if (sWertTyp = C_MBWertTyp_D) then
      Result:=IntToStr (Bin2LongWord (sBinData, ByteOrder))  // DWORD, unsigned long
    else if (sWertTyp = 'LL') then
      Result:=IntToStr (Bin2Int64 (sBinData, ByteOrder))  // long long; 08.03.2019, WW
    else if (sWertTyp = 'C') then
      Result:=sBinData  // char
    else if (sWertTyp = C_MBWertTyp_B) OR  // BYTE, numerisch unsigned char
            (sWertTyp = '1') OR (sWertTyp = '2') OR (sWertTyp = '3') OR
            (sWertTyp = '4') OR (sWertTyp = '5') OR (sWertTyp = '6') OR
            (sWertTyp = '7') OR (sWertTyp = '8') OR (sWertTyp = '9') then  // BYTE, mit Wertebereich 1..9
      Result:=IntToStr (Bin2Word (sBinData, ByteOrder))  // Registerlänge ist immer mind. Word
    else if (sWertTyp = 'S') then  // String
      Result:=sBinData
    else if (sWertTyp = C_MBWertTyp_Sle) then  // String (fix Little-Endian); 29.09.2021, WW
      Result:=EndianString (sBinData)
    else if (sWertTyp = C_MBWertTyp_N) then  // 0-terminierter String
      Result:=PChar (sBinData)  // 09.01.2024, WW
    else if (sWertTyp = C_MBWertTyp_Nle) then begin  // 0-terminierter String (fix Little-Endian); 29.09.2021, WW
      S:=EndianString (sBinData);
      Result:=PChar (S);  // 09.01.2024, WW
    end
    else if (sWertTyp = C_MBWertTyp_U) OR  // UNIX-Zeitinfo, DWORD
            (sWertTyp = 's') then begin  // Systemzeit (UNIX)
      c:=Bin2Longword (sBinData, ByteOrder);
      UnixSekundenToDateTime (c, dt);
      Result:=DateTimeToStr (dt);
    end
    else if ((sWertTyp = 'i')) OR  // IPv4
            ((sWertTyp = C_MBWertTyp_ile)) then begin  // IPv4 (fix Little-Endian); 13.11.2019, WW
      if (sWertTyp = 'i') then
        c:=Bin2Longword (sBinData, ByteOrder)
      else
        c:=Bin2Longword (sBinData, bo_LittleEndian);  // 13.11.2019, WW
      Result:=Format ('%u.%u.%u.%u', [(c AND $FF000000) SHR 24,
                                      (c AND $FF0000) SHR 16,
                                      (c AND $FF00) SHR 8,
                                      (c AND $FF)]);
    end

    // Elgas-Werttypen (Primus)
    else if (sWertTyp = C_MBWertTyp_DT2) then begin  // DateTime 2; 08.03.2019, WW
      if length (sBinData) = 6 then begin
        try
          // 6 Bytes BCD: y m d h n s
          dt:=EncodeDateTime (BCDToInt (Ord (sBinData [1])) + 2000,  // Jahr
                              BCDToInt (Ord (sBinData [2])),  // Monat
                              BCDToInt (Ord (sBinData [3])),  // Tag
                              BCDToInt (Ord (sBinData [4])),  // Stunde
                              BCDToInt (Ord (sBinData [5])),  // Minute
                              BCDToInt (Ord (sBinData [6])),  // Sekunde
                              0);  // Millisekunde
        except
          dt:=0;
        end;
      end else
        dt:=0;
      Result:=DateTimeToStr (dt);
    end else  // unbekannter Typ
      Result:=Bin2Hex (sBinData, true);  // Hex-Darstellung mit Space
  end else  // kein Typ angegeben
    Result:=sBinData;  // Rohformat; 15.10.2009, WW
end;


{-------------------------------------------}
function Get_Modbus_MaxDataBytes_01: integer;
{-------------------------------------------}
{ berechnet die maximal mögliche Anzahl an Daten-Bytes in einer Modbus-Response,
  Funktion 01 (Read Coil Status);
  Ergebnis: max. Anzahl Daten-Bytes }
const
  CLenSlaveAddress = 1;
  CLenFunction     = 1;
  CLenByteCount    = 1;
  CLenErrorCheck   = 2;

begin
  Result:=256 - CLenSlaveAddress - CLenFunction - CLenByteCount - CLenErrorCheck;
  // Die theoretisch berechnete maximale Byte-Anzahl an "Register-Raster" (2 Bytes)
  // anpassen. Erfolgreicher Test mit ERZ 2000 NG, Vs. 1.6.0 bestätigt diese
  // notwendige Korrektur:
  if Odd(Result) then
    Result:=Result - 1;
end;

{-------------------------------------------}
function Get_Modbus_MaxDataBytes_02: integer;
{-------------------------------------------}
{ berechnet die maximal mögliche Anzahl an Daten-Bytes in einer Modbus-Response,
  Funktion 02 (Read Input Status);
  Ergebnis: max. Anzahl Daten-Bytes }
const
  CLenSlaveAddress = 1;
  CLenFunction     = 1;
  CLenByteCount    = 1;
  CLenErrorCheck   = 2;

begin
  Result:=Get_Modbus_MaxDataBytes_01;  // wie bei Funktion 01
end;

{-------------------------------------------}
function Get_Modbus_MaxDataBytes_03: integer;
{-------------------------------------------}
{ berechnet die maximal mögliche Anzahl an Daten-Bytes in einer Modbus-Response,
  Funktion 03 (Read Holding Registers);
  Ergebnis: max. Anzahl Daten-Bytes }
const
  CLenSlaveAddress = 1;
  CLenFunction     = 1;
  CLenByteCount    = 1;
  CLenErrorCheck   = 2;

begin
  Result:=256 - CLenSlaveAddress - CLenFunction - CLenByteCount - CLenErrorCheck;
end;

{-------------------------------------------}
function Get_Modbus_MaxDataBytes_04: integer;
{-------------------------------------------}
{ berechnet die maximal mögliche Anzahl an Daten-Bytes in einer Modbus-Response,
  Funktion 04 (Read Input Registers);
  Ergebnis: max. Anzahl Daten-Bytes }
begin
  Result:=Get_Modbus_MaxDataBytes_03;  // wie bei Funktion 03
end;

{-------------------------------------------}
function Get_Modbus_MaxDataBytes_20: integer;
{-------------------------------------------}
{ berechnet die maximal mögliche Anzahl an Daten-Bytes in einer Modbus-Response,
  Funktion 20 (Read General Reference);
  Ergebnis: max. Anzahl Daten-Bytes }
const
  CLenSlaveAddress    = 1;
  CLenFunction        = 1;
  CLenByteCount       = 1;
  CLenSubResByteCount = 1;
  CLenSubResRefType   = 1;
  CLenErrorCheck      = 2;

begin
  // Berechnung für einen einzelnen Sub-Response/Request:
  Result:=256 - CLenSlaveAddress - CLenFunction - CLenByteCount -
          CLenSubResByteCount - CLenSubResRefType - CLenErrorCheck;
end;


{------------------------------------------------------------------------}
function Get_Modbus_Query (SlaveAdresse, Funktion: byte; BinDaten: string;
  Modus: TModbusModus; var iTID: word): string;
{------------------------------------------------------------------------}
{ liefert Modbus-Anfragetelegramm;
  Übergabe: Adresse des Slave
            Funktionscode
            Daten als Binär-String
            Modbus-Modus (ASCII/RTU/TCP)
  Übergabe/Rückgabe: Transaktionsnummer (Modbus TCP)
  Ergebnis: Anfragetelegramm (Query) }
var
  S: string;
  i: integer;
  CRC: word;
  LRC: byte;

begin
  S:=Chr (SlaveAdresse) + Chr (Funktion);
  for i:=1 to length (BinDaten) do
    S:=S + Chr (Ord (BinDaten [i]));
  if (Modus = modbus_ASCII) then begin
    LRC:=Get_Modbus_LRC (S);  // LRC über Binärdaten
    S:=S + Chr (LRC);
    Result:=':' + Bin2Hex (S) + CR + LF;  // Query in ASCII hex
  end
  else if (Modus = modbus_RTU)then begin  // RTU
    CRC:=Get_Modbus_CRC (S);
    Result:=S + Word2Bin (CRC, bo_BigEndian);  // Query binär, CRC immer Big-Endian
  end
  else if (Modus = modbus_TCPIP)then begin  // TCP/IP
    // Nächste Transaktionsnummer:
    IncrementTID_ModbusTCPIP (iTID);  // 16.12.2019, WW
    // Transaktionsnummer, Protokollnummer (0 = MODBUS protocol), Anzahl der
    // Zeichen, Query binär, kein CRC
    Result := Word2Bin(iTID, bo_BigEndian) + Word2Bin(0, bo_BigEndian) +
      Word2Bin(Length(S), bo_BigEndian) + S;
  end
  else Result := '';
end;

{------------------------------------------------------------------------------------}
function Get_Modbus_Query_01 (SlaveAdresse: byte; StartAusgang, AnzahlAusgaenge: word;
  Modus: TModbusModus; var iTID: word): string;
{------------------------------------------------------------------------------------}
{ liefert Modbus-Anfragetelegramm, Funktion 01 (Read Coil Status);
  Übergabe: Adresse des Slave
            Startadresse des zu lesenden Ausgangs (indiziert, bei 0 beginnend)
            Anzahl der zu lesenden Ausgänge
            Modbus-Modus (ASCII/RTU/TCP)
  Übergabe/Rückgabe: Transaktionsnummer (Modbus TCP)
  Ergebnis: Anfragetelegramm (Query) }
var
  sBinDaten: string;

begin
  sBinDaten:=Word2Bin (StartAusgang, bo_BigEndian) +
             Word2Bin (AnzahlAusgaenge, bo_BigEndian);  // Start und Anzahl der Ausgänge immer Big-Endian
  Result:=Get_Modbus_Query (SlaveAdresse, 1, sBinDaten, Modus, iTID);
end;

{------------------------------------------------------------------------------------}
function Get_Modbus_Query_02 (SlaveAdresse: byte; StartEingang, AnzahlEingaenge: word;
  Modus: TModbusModus; var iTID: word): string;
{------------------------------------------------------------------------------------}
{ liefert Modbus-Anfragetelegramm, Funktion 02 (Read Input Status);
  Übergabe: Adresse des Slave
            Startadresse des zu lesenden Eingangs (indiziert, bei 0 beginnend)
            Anzahl der zu lesenden Eingänge
            Modbus-Modus (ASCII/RTU/TCP)
  Übergabe/Rückgabe: Transaktionsnummer (Modbus TCP)
  Ergebnis: Anfragetelegramm (Query) }
var
  sBinDaten: string;

begin
  sBinDaten:=Word2Bin (StartEingang, bo_BigEndian) +
             Word2Bin (AnzahlEingaenge, bo_BigEndian);  // Start und Anzahl der Eingänge immer Big-Endian
  Result:=Get_Modbus_Query (SlaveAdresse, 2, sBinDaten, Modus, iTID);
end;

{------------------------------------------------------------------------------------}
function Get_Modbus_Query_03 (SlaveAdresse: byte; StartRegister, AnzahlRegister: word;
  Modus: TModbusModus; var iTID: word): string;
{------------------------------------------------------------------------------------}
{ liefert Modbus-Anfragetelegramm, Funktion 03 (Read Holding Registers);
  Übergabe: Adresse des Slave
            Startadresse des zu lesenden Registers (indiziert, bei 0 beginnend)
            Anzahl der zu lesenden Register
            Modbus-Modus (ASCII/RTU/TCP)
  Übergabe/Rückgabe: Transaktionsnummer (Modbus TCP)
  Ergebnis: Anfragetelegramm (Query) }
var
  sBinDaten: string;

begin
  sBinDaten:=Word2Bin (StartRegister, bo_BigEndian) +
             Word2Bin (AnzahlRegister, bo_BigEndian);  // Start und Anzahl der Register immer Big-Endian
  Result:=Get_Modbus_Query (SlaveAdresse, 3, sBinDaten, Modus, iTID);
end;

{------------------------------------------------------------------------------------}
function Get_Modbus_Query_04 (SlaveAdresse: byte; StartRegister, AnzahlRegister: word;
  Modus: TModbusModus; var iTID: word): string;
{------------------------------------------------------------------------------------}
{ liefert Modbus-Anfragetelegramm, Funktion 04 (Read Input Registers);
  Übergabe: Adresse des Slave
            Startadresse des zu lesenden Registers (indiziert, bei 0 beginnend)
            Anzahl der zu lesenden Register
            Modbus-Modus (ASCII/RTU/TCP)
  Übergabe/Rückgabe: Transaktionsnummer (Modbus TCP)
  Ergebnis: Anfragetelegramm (Query) }
var
  sBinDaten: string;

begin
  sBinDaten:=Word2Bin (StartRegister, bo_BigEndian) +
             Word2Bin (AnzahlRegister, bo_BigEndian);  // Start und Anzahl der Register immer Big-Endian
  Result:=Get_Modbus_Query (SlaveAdresse, 4, sBinDaten, Modus, iTID);
end;

{---------------------------------------------------------------------}
function Get_Modbus_Query_05 (SlaveAdresse: byte; AusgangAdresse: word;
  BinAusgangData: string; Modus: TModbusModus; var iTID: word): string;
{---------------------------------------------------------------------}
{ liefert Modbus-Anfragetelegramm, Funktion 05 (Force Single Coil);
  Übergabe: Adresse des Slave
            Adresse des zu setzenden Ausgangs (indiziert, bei 0 beginnend)
            Binärdaten, mit denen der Ausgang beschrieben werden sollen (mit richtiger Byte-Order !)
            Modbus-Modus (ASCII/RTU/TCP)
  Übergabe/Rückgabe: Transaktionsnummer (Modbus TCP)
  Ergebnis: Anfragetelegramm (Query) }
var
  sBinDaten: string;

begin
  sBinDaten:=Word2Bin (AusgangAdresse, bo_BigEndian) +  // Ausgang-Adresse immer Big-Endian
             BinAusgangData;
  Result:=Get_Modbus_Query (SlaveAdresse, 5, sBinDaten, Modus, iTID);
end;

{----------------------------------------------------------------------}
function Get_Modbus_Query_06 (SlaveAdresse: byte; RegisterAdresse: word;
  BinRegisterData: string; Modus: TModbusModus; var iTID: word): string;
{----------------------------------------------------------------------}
{ liefert Modbus-Anfragetelegramm, Funktion 06 (Preset Single Registers);
  Übergabe: Adresse des Slave
            Adresse des zu schreibenden Registers (indiziert, bei 0 beginnend)
            Binärdaten, mit denen das Register beschrieben werden sollen (mit richtiger Byte-Order !)
            Modbus-Modus (ASCII/RTU/TCP)
  Übergabe/Rückgabe: Transaktionsnummer (Modbus TCP)
  Ergebnis: Anfragetelegramm (Query) }
var
  sBinDaten: string;

begin
  sBinDaten:=Word2Bin (RegisterAdresse, bo_BigEndian) +  // Register-Adresse immer Big-Endian
             BinRegisterData;
  Result:=Get_Modbus_Query (SlaveAdresse, 6, sBinDaten, Modus, iTID);
end;

{------------------------------------------------------------------------}
function Get_Modbus_Query_08 (SlaveAdresse: byte; Unterfunktion: word;
  BinDiagnostikData: string; Modus: TModbusModus; var iTID: word): string;
{------------------------------------------------------------------------}
{ liefert Modbus-Anfragetelegramm, Funktion 08 (Diagnostics);
  Übergabe: Adresse des Slave
            Unterfunktionscode
            Diagnostik-Binärdaten (mit richtiger Byte-Order !)
            Modbus-Modus (ASCII/RTU/TCP)
  Übergabe/Rückgabe: Transaktionsnummer (Modbus TCP)
  Ergebnis: Anfragetelegramm (Query) }
var
  sBinDaten: string;

begin
  sBinDaten:=Word2Bin (Unterfunktion, bo_BigEndian) +  // Unterfunktionscode immer Big-Endian
             BinDiagnostikData;
  Result:=Get_Modbus_Query (SlaveAdresse, 8, sBinDaten, Modus, iTID);
end;

{------------------------------------------------------------------------------------}
function Get_Modbus_Query_16 (SlaveAdresse: byte; StartRegister, AnzahlRegister: word;
  BinRegisterData: string; Modus: TModbusModus;
  var Query: string; var iTID: word): boolean;
{------------------------------------------------------------------------------------}
{ liefert Modbus-Anfragetelegramm, Funktion 16 (Preset Multiple Registers);
  Übergabe: Adresse des Slave
            Startadresse des zu schreibenden Registers (indiziert, bei 0 beginnend)
            Anzahl der zu schreibenden Register
            Binärdaten, mit denen die Register beschrieben werden sollen (mit richtiger Byte-Order !)
            Modbus-Modus (ASCII/RTU/TCP)
  Rückgabe: Anfragetelegramm (Query)
  Übergabe/Rückgabe: Transaktionsnummer (Modbus TCP)
  Ergebnis: true, wenn Anfragetelegramm gebildet werden konnte }
var
  iByteCount: integer;
  sBinDaten: string;

begin
  Result:=false;
  Query:='';  // Vorbelegung Rückgabe

  iByteCount:=length (BinRegisterData);
  if (iByteCount > 0) AND (iByteCount <= High (Byte)) then begin
    sBinDaten:=Word2Bin (StartRegister, bo_BigEndian) +
               Word2Bin (AnzahlRegister, bo_BigEndian) +  // Start und Anzahl der Register immer Big-Endian
               Chr (iByteCount) +  // Anzahl Datenbytes
               BinRegisterData;
    Query:=Get_Modbus_Query (SlaveAdresse, 16, sBinDaten, Modus, iTID);
    Result:=true;
  end;
end;

{---------------------------------------------------------------------}
function Get_Modbus_Query_20 (SlaveAdresse: byte;
  FileNumber, StartRegister, AnzahlRegister: word; Modus: TModbusModus;
  var iTID: word): string;
{---------------------------------------------------------------------}
{ liefert Modbus-Anfragetelegramm, Funktion 20 (Read General Reference) für einen
  einzelnen Sub-Request;
  Übergabe: Adresse des Slave
            Filenummer (Sub-Request)
            Startadresse des zu lesenden Registers (Sub-Request)
            Anzahl der zu lesenden Register (Sub-Request)
            Modbus-Modus (ASCII/RTU/TCP)
  Übergabe/Rückgabe: Transaktionsnummer (Modbus TCP)
  Ergebnis: Anfragetelegramm (Query) }
var
  iByteCount: integer;
  sBinDaten: string;

begin
  sBinDaten:=Chr (6) +  // lt. Modbus-Spezifikation Referenztyp 6 fix
             Word2Bin (FileNumber, bo_BigEndian) +
             Word2Bin (StartRegister, bo_BigEndian) +
             Word2Bin (AnzahlRegister, bo_BigEndian);  // Filenummer, Start und Anzahl der Register immer Big-Endian
  // Anzahl Datenbytes
  iByteCount:=length (sBinDaten);
  sBinDaten:=Chr (iByteCount) + sBinDaten;
  Result:=Get_Modbus_Query (SlaveAdresse, 20, sBinDaten, Modus, iTID);
end;

{-------------------------------------------------------------------------------}
function Get_Modbus_Query_52 (SlaveAdresse: byte;
  StartRegister, AnzahlRegister, Index_von, Index_bis: word; Modus: TModbusModus;
  var iTID: word): string;
{-------------------------------------------------------------------------------}
{ liefert Modbus-Anfragetelegramm, Funktion 52 (Read RMG from to);
  Übergabe: Adresse des Slave
            Startadresse des zu lesenden Registers (indiziert, bei 0 beginnend)
            Anzahl der zu lesenden Register
            von-Index
            bis-Index
            Modbus-Modus (ASCII/RTU/TCP)
  Übergabe/Rückgabe: Transaktionsnummer (Modbus TCP)
  Ergebnis: Anfragetelegramm (Query) }
var
  sBinDaten: string;

begin
  sBinDaten:=Word2Bin (StartRegister, bo_BigEndian) +
             Word2Bin (AnzahlRegister, bo_BigEndian) +
             Word2Bin (Index_von, bo_BigEndian) +
             Word2Bin (Index_bis, bo_BigEndian);  // Start, Anzahl, von-Index, bis-Index der Register immer Big-Endian
  Result:=Get_Modbus_Query (SlaveAdresse, 52, sBinDaten, Modus, iTID);
end;

{---------------------------------------------------------------------}
function Get_Modbus_Quittung_52 (SlaveAdresse: byte; bPositiv: boolean;
  AnzahlRegister: word; Modus: TModbusModus; var iTID: word): string;
{---------------------------------------------------------------------}
{ liefert Modbus-Quittungstelegramm für Funktion 52 (Read RMG from to);
  Übergabe: Adresse des Slave
            Flag: positive/negative Quittung
            Anzahl der zu quittierenden Register
            Modbus-Modus (ASCII/RTU/TCP)
  Übergabe/Rückgabe: Transaktionsnummer (Modbus TCP)
  Ergebnis: Quittungstelegramm (Query) }
var
  sBinDaten: string;

begin
  if bPositiv then
    sBinDaten:=Word2Bin (17, bo_BigEndian)  // ACK
  else
    sBinDaten:=Word2Bin (18, bo_BigEndian);  // NAK
  sBinDaten:=sBinDaten + Word2Bin (AnzahlRegister, bo_BigEndian);  // Quittungscode und Anzahl der Register immer Big-Endian
  Result:=Get_Modbus_Query (SlaveAdresse, 180, sBinDaten, Modus, iTID);
end;

{---------------------------------------------------------------------}
function Valid_Modbus_Response (SlaveAdresse_Soll, Funktion_Soll: byte;
  TID_Soll: word; Modus: TModbusModus; var Response: string;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{---------------------------------------------------------------------}
{ überprüft eine Modbus-Response auf Richtigkeit/Exception;
  Übergabe: in Response erwartete Adresse des Slave
            in Response erwarteter Funktionscode
            in Response erwartete Transaktionsnummer (für Modbus TCP/IP)
            Modbus-Modus (ASCII/RTU)
  Übergabe/Rückgabe: Response-String
  Rückgabe: Fehlergruppe, Fehlercode
  Ergebnis: true, wenn Response ok }
var
  iSlaveAdresse: byte;
  iFunktion: byte;
  bIsExceptionResp: boolean;
  iExceptionCode: byte;
  S: string;
  sBin: string;
  P: integer;
  iRespByteCount: byte;
  iTID: word;
  iProtID: word;
  iLen: word;

begin
  Result:=false;
  Fehlergruppe:=COM_MODBUSERROR;  // Fehlergruppe: Modbus-Fehler
  Fehlercode:=-1;  // Fehlercode: unbelegt

  if (Modus = modbus_ASCII) then begin  // ASCII
    { Alles vor dem ':' in der Response ist Müll und muß weg }
    P:=Pos (':', Response);
    Response:=Copy (Response, P, length (Response));
    S:=ExtractString (Response, ':', CR, 0);  // Datenteil zwischen Start und Ende (ASCII hex)
    sBin:=Hex2Bin (S);  // Hex-String in Binär-String wandeln
  end
  else if (Modus = modbus_RTU) then  // RTU
    sBin:=Response
  else if (Modus = modbus_TCPIP) then begin  // TCP/IP
    // Felder im MBAP-Header prüfen:
    S:=Copy (Response, 1, 2);  // Transaktionsnummer; 16.12.2019, WW
    iTID := Bin2Word(S, bo_BigEndian);
    if iTID <> TID_Soll then begin  // 16.12.2019, WW
      Fehlercode:=MODBUSERR_RESPTCP_WRONGTID;                                   
      exit;
    end;

    S:=Copy (Response, 3, 2);  // Protokoll-ID; 16.12.2019, WW
    iProtID := Bin2Word(S, bo_BigEndian);
    if iProtID <> 0 then begin  // 0 = Modbus Protokoll; 16.12.2019, WW
      Fehlercode:=MODBUSERR_RESPTCP_WRONGPROTID;
      exit;
    end;

    S:=Copy (Response, 5, 2);  // Länge; 16.12.2019, WW
    iLen := Bin2Word(S, bo_BigEndian);
    sBin:=Copy (Response, 7, length (Response) - 6);  // MBAP-Header: erste 6 Bytes abschneiden (nicht Slave-Adresse)
    if iLen <> length (sBin) then begin  // 16.12.2019, WW
      Fehlercode:=MODBUSERR_RESPTCP_LENGTH_SOLL_IST;
      exit;
    end;
  end else
    sBin:=Response;

  if length (sBin) >= 2 then begin
    iSlaveAdresse:=Ord (sBin [1]);
    { Soll/Ist-Vergleich Slave-Adresse: }
    if iSlaveAdresse = SlaveAdresse_Soll then begin
      iFunktion:=Ord (sBin [2]);  { Funktionscode }
      if iFunktion = 180 then begin  { Funktionscode in Response auf Quittung "Read RMG from to" }
        { Keine Exception-Auswertung von Bit 7 möglich ! }

        { Soll/Ist-Vergleich Funktionscode: }
        if iFunktion = Funktion_Soll then begin
          // Fehler-Auswertung über Anzahl der Datenbytes:
          if length (sBin) >= 3 then begin
            iRespByteCount:=Bin2Byte (sBin [3]);
            case iRespByteCount of
                2: Fehlercode:=MODBUSERR_RMG_INDEX;  // Fehler in Index
              255: Fehlercode:=MODBUSERR_RMG_EXCEPTION;  // Exception
            else
              { Response ist OK: mit oder ohne Daten }
              Fehlergruppe:=0;
              Fehlercode:=0;
              Result:=true;
            end;
          end else
            Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Response }
        end else
          Fehlercode:=MODBUSERR_RESP_WRONGFUNCTION;
      end
      else begin  // Standard
        bIsExceptionResp:=(iFunktion AND $80) <> 0;  { Bit 7 = 1: Exception Response ! }
        iFunktion:=iFunktion AND not $80;  { Bit 7 löschen für Soll/Ist-Vergleich }

        { Soll/Ist-Vergleich Funktionscode: }
        if iFunktion = Funktion_Soll then begin
          if bIsExceptionResp then begin  { Exception Response }
            if length (sBin) >= 3 then begin
              iExceptionCode:=Ord (sBin [3]);  { Exceptioncode }
              case iExceptionCode of
                01: Fehlercode:=MODBUSERR_EXC_ILLEGALFUNCTION;
                02: Fehlercode:=MODBUSERR_EXC_ILLEGALDATAADDRESS;
                03: Fehlercode:=MODBUSERR_EXC_ILLEGALDATAVALUE;
                04: Fehlercode:=MODBUSERR_EXC_SLAVEDEVICEFAILURE;
                05: Fehlercode:=MODBUSERR_EXC_ACKNOWLEDGE;
                06: Fehlercode:=MODBUSERR_EXC_SLAVEDEVICEBUSY;
                07: Fehlercode:=MODBUSERR_EXC_NEGATIVEACKNOWLEDGE;
                08: Fehlercode:=MODBUSERR_EXC_MEMORYPARITYERROR;
                // 27.06.2017, WW
                10: Fehlercode:=MODBUSERR_EXC_GATEWAYPATHUNAVAILABLE;
                11: Fehlercode:=MODBUSERR_EXC_GATEWAYTARGETDEVICEFAILEDTORESPOND;
              else
                Fehlercode:=MODBUSERR_EXC_UNDEFINED;
              end;
            end else
              Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Exception Response }
          end
          else begin
            { Response ist OK: }
            Fehlergruppe:=0;
            Fehlercode:=0;
            Result:=true;
          end;
        end else
          Fehlercode:=MODBUSERR_RESP_WRONGFUNCTION;
      end;
    end else
      Fehlercode:=MODBUSERR_RESP_WRONGSLAVEADDRESS;
  end else
    Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Response }
end;


// 13.11.2019, WW
{-------------------------------------------------------------------}
function Cut_Modbus_Response_SlaveAdr_FktCode_Data (Response: string;
  Modus: TModbusModus): string;
{-------------------------------------------------------------------}
{ Schneidet aus einer Modbus-Response Slave-Adresse, Funktionscode und Daten aus;
  Übergabe: Response-String
            Modbus-Modus (ASCII/RTU/TCP)
  Ergebnis: Ausgeschnittener Response-Teil mit Slave-Adresse, Funktionscode und
            Daten }
var
  sRespBin: string;
  S: string;

begin
  if (Modus = modbus_ASCII) then begin
    S:=ExtractString (Response, ':', CR, 0);  // Datenteil zwischen Start und Ende (ASCII hex)
    sRespBin:=Hex2Bin (S);  // Hex-String in Binär-String wandeln
    sRespBin:=Copy (sRespBin, 1, length (sRespBin) - 1);  // Binär-LRC abschneiden
  end
  else if (Modus = modbus_RTU) then // RTU
    sRespBin:=Copy (Response, 1, length (Response) - 2)  // CRC abschneiden
  else if (Modus = modbus_TCPIP) then   // TCP/IP
    sRespBin := Copy (Response, 7, Length (Response) - 6)  // MBAP-Header: erste 6 Bytes abschneiden (nicht Slave-Adresse)
  else
    sRespBin := '';  // sonst nix

  Result:=sRespBin;
end;

{-----------------------------------------------------------------------------------}
function Get_Modbus_Response_01 (Response: string; Modus: TModbusModus;
  ByteOrder: TByteOrder; RegisterKonvList: TRegisterKonvList; AnzahlBytes_Soll: byte;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{-----------------------------------------------------------------------------------}
{ liefert Coil-Statuswerte aus Modbus-Response, Funktion 01 (Read Coil Status);
  Übergabe: Response-String
            Modbus-Modus (ASCII/RTU)
            Byte-Order (Big-Endian/Little-Endian)
            Liste mit Register-Konvertierungsdaten (enthält als Rückgabe die
              Coil-Statuswerte: 0 = OFF, 1 = ON)
            Soll-Anzahl Daten-Bytes in der Response
  Rückgabe: Fehlergruppe, Fehlercode
  Ergebnis: true, wenn Coil-Statuswerte aus Response gelesen und konvertiert werden konnten }
var
  sRespBin: string;
  iRespByteCount: byte;
  sRespData: string;
  sRespDataGesamt: string;
  i: integer;
  iCoilPos, iBytePos, iBitPos, iLen: integer;
  iStartAddr, iAddr : word;
  iByte: byte;

begin
  Result:=false;

  if Assigned (RegisterKonvList) then
    for i:=0 to RegisterKonvList.Count - 1 do
      TRegisterKonvDataObj (RegisterKonvList [i]).Data.Wert:='';  // Vorbelegung Rückgabe

  sRespBin := Cut_Modbus_Response_SlaveAdr_FktCode_Data (Response, Modus);  // 13.11.2019, WW

  Fehlergruppe:=COM_MODBUSERROR;  // Fehlergruppe: Modbus-Fehler
  if length (sRespBin) >= 3 then begin
    iRespByteCount:=Bin2Byte (sRespBin [3]);
    if length (sRespBin) >= (3 + iRespByteCount) then begin
      if iRespByteCount = AnzahlBytes_Soll then begin  // auf Data-ByteCount Ist = Soll prüfen (>= zu lasch !); 13.11.2019, WW
        sRespDataGesamt:=Copy (sRespBin, 4, AnzahlBytes_Soll);  // gesamte Data ausschneiden

        // Für alle in der Register-Konvertierungsdatenliste enthaltenen Coils
        // die im Data-String enthaltenen Statuswerte konvertieren:
        if Assigned (RegisterKonvList) and (RegisterKonvList.Count > 0) then begin
          // Coil-Adresse des ersten Listeneintrags:
          iStartAddr := TRegisterKonvDataObj(RegisterKonvList[0]).Data.StartAdresse;
          for i:=0 to RegisterKonvList.Count - 1 do begin
            iAddr := TRegisterKonvDataObj(RegisterKonvList[i]).Data.StartAdresse;

            // Coil-Position (= Bit-Position) im Data-String (beginnend mit 1):
            iCoilPos := 1 + (iAddr - iStartAddr);
            // Das zur Coil-Position gehörende Byte im Data-String ermitteln
            // -> 1. bis 8. Coil im 1. Byte, 9. bis 16. Coil im 2. Byte usw.
            iBytePos := ((iCoilPos - 1) DIV 8) + 1;

            iLen:=1;  // Länge Byte
            // Byte aus Data rauskopieren
            sRespData:=Copy (sRespDataGesamt, iBytePos, iLen);

            if length (sRespData) = iLen then begin
              // Coil-Position (= Bit-Position) im Byte (beginnend mit 1):
              iBitPos := ((iCoilPos - 1) MOD 8) + 1;
              // Bit an der Coil-Position auswerten und Coil-Status in
              // Registerliste eintragen:
              iByte := Ord (sRespData[iLen]);
              if  (iByte AND ($01 SHL (iBitPos-1))) <> 0 then
                TRegisterKonvDataObj (RegisterKonvList [i]).Data.Wert:='1'   // ON
              else
                TRegisterKonvDataObj (RegisterKonvList [i]).Data.Wert:='0';  // OFF
            end;  { if length (sRespData) = iLen }   
          end;  { for i }
        end;  { if Assigned (RegisterKonvList) }

        { 01-Response ist OK: }
        Fehlergruppe:=0;
        Fehlercode:=0;
        Result:=true;
      end else
        Fehlercode:=MODBUSERR_RESP_BYTECOUNT_SOLL_IST;  { Soll/Ist-Fehler Bytecount }
    end else
      Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Response }
  end else
    Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Response }
end;

{-----------------------------------------------------------------------------------}
function Get_Modbus_Response_02 (Response: string; Modus: TModbusModus;
  ByteOrder: TByteOrder; RegisterKonvList: TRegisterKonvList; AnzahlBytes_Soll: byte;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{-----------------------------------------------------------------------------------}
{ liefert Input-Statuswerte aus Modbus-Response, Funktion 02 (Read Input Status);
  Übergabe: Response-String
            Modbus-Modus (ASCII/RTU)
            Byte-Order (Big-Endian/Little-Endian)
            Liste mit Register-Konvertierungsdaten (enthält als Rückgabe die
              Input-Statuswerte: 0 = OFF, 1 = ON)
            Soll-Anzahl Daten-Bytes in der Response
  Rückgabe: Fehlergruppe, Fehlercode
  Ergebnis: true, wenn Input-Statuswerte aus Response gelesen und konvertiert werden konnten }
begin
  // wie für Funktion 01:
  Result:=Get_Modbus_Response_01 (Response, Modus, ByteOrder, RegisterKonvList,
                                  AnzahlBytes_Soll, Fehlergruppe, Fehlercode);
end;

{---------------------------------------------------------------------------------}
function Get_Modbus_Response_03 (Response: string; Modus: TModbusModus;
  ByteOrder: TByteOrder; RegisterTyp: string; AnzahlBytes_Soll: byte;
  var sValue: string; var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{---------------------------------------------------------------------------------}
{ liefert Registerwert aus Modbus-Response, Funktion 03 (Read Holding Registers);
  Übergabe: Response-String
            Modbus-Modus (ASCII/RTU)
            Byte-Order (Big-Endian/Little-Endian)
            Typ des Registerwertes
            Soll-Anzahl Daten-Bytes in der Response
  Rückgabe: Registerwert als String
            Fehlergruppe, Fehlercode
  Ergebnis: true, wenn Registerwert aus Response gelesen und Registertyp-abhängig
            in String konvertiert werden konnte }
var
  sRespBin: string;
  iRespByteCount: byte;
  sRespData: string;

begin
  Result:=false;
  sValue:='';  // Vorbelegung Rückgabe

  sRespBin := Cut_Modbus_Response_SlaveAdr_FktCode_Data (Response, Modus);  // 13.11.2019, WW

  Fehlergruppe:=COM_MODBUSERROR;  // Fehlergruppe: Modbus-Fehler
  if length (sRespBin) >= 3 then begin
    iRespByteCount:=Bin2Byte (sRespBin [3]);
    if length (sRespBin) >= (3 + iRespByteCount) then begin
      if iRespByteCount = AnzahlBytes_Soll then begin  // auf Data-ByteCount Ist = Soll prüfen (>= zu lasch !); 13.11.2019, WW
        sRespData:=Copy (sRespBin, 4, AnzahlBytes_Soll);  // Data ausschneiden
        // Binärdaten in formatierten Registerwert wandeln:
        sValue:=Modbus_BinData2Str (sRespData, ByteOrder, RegisterTyp);

        { 03-Response ist OK: }
        Fehlergruppe:=0;
        Fehlercode:=0;
        Result:=true;
      end else
        Fehlercode:=MODBUSERR_RESP_BYTECOUNT_SOLL_IST;  { Soll/Ist-Fehler Bytecount }
    end else
      Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Response }
  end else
    Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Response }
end;

{-----------------------------------------------------------------------------------}
function Get_Modbus_Response_03 (Response: string; Modus: TModbusModus;
  ByteOrder: TByteOrder; RegisterKonvList: TRegisterKonvList; AnzahlBytes_Soll: byte;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{-----------------------------------------------------------------------------------}
{ liefert Registerwerte aus Modbus-Response, Funktion 03 (Read Holding Registers);
  Übergabe: Response-String
            Modbus-Modus (ASCII/RTU)
            Byte-Order (Big-Endian/Little-Endian)
            Liste mit Register-Konvertierungsdaten (enthält als Rückgabe die Registerwerte)
            Soll-Anzahl Daten-Bytes in der Response
  Rückgabe: Fehlergruppe, Fehlercode
  Ergebnis: true, wenn Registerwerte aus Response gelesen und Registertyp-abhängig
            in Strings konvertiert werden konnten }
var
  sRespBin: string;
  iRespByteCount: byte;
  sRespData: string;
  sRespDataGesamt: string;
  i: integer;
  iPos, iLen: integer;
  sRegisterTyp: string;
  iStartAddr, iAddr : word;
begin
  Result:=false;         

  if Assigned (RegisterKonvList) then
    for i:=0 to RegisterKonvList.Count - 1 do
      TRegisterKonvDataObj (RegisterKonvList [i]).Data.Wert:='';  // Vorbelegung Rückgabe

  sRespBin := Cut_Modbus_Response_SlaveAdr_FktCode_Data (Response, Modus);  // 13.11.2019, WW

  Fehlergruppe:=COM_MODBUSERROR;  // Fehlergruppe: Modbus-Fehler
  if length (sRespBin) >= 3 then begin
    iRespByteCount:=Bin2Byte (sRespBin [3]);
    if length (sRespBin) >= (3 + iRespByteCount) then begin
      if iRespByteCount = AnzahlBytes_Soll then begin  // auf Data-ByteCount Ist = Soll prüfen (>= zu lasch !); 13.11.2019, WW
        sRespDataGesamt:=Copy (sRespBin, 4, AnzahlBytes_Soll);  // gesamte Data ausschneiden

        // Für alle in der Register-Konvertierungsdatenliste enthaltenen Register
        // die im Data-String enthaltenen Registerwerte konvertieren:
        if Assigned (RegisterKonvList) and (RegisterKonvList.Count > 0) then begin
          // Register-Adresse des ersten Listeneintrags:
          iStartAddr := TRegisterKonvDataObj(RegisterKonvList[0]).Data.StartAdresse;
          for i:=0 to RegisterKonvList.Count - 1 do begin
            iAddr := TRegisterKonvDataObj(RegisterKonvList[i]).Data.StartAdresse;
            sRegisterTyp:=TRegisterKonvDataObj (RegisterKonvList [i]).Data.Typ;
            // Position im Data-String:
            iPos := 1 + Calc_Modbus_AnzahlBytes(iAddr - iStartAddr);
            iLen:=TRegisterKonvDataObj (RegisterKonvList [i]).Data.AnzahlBytes;
            // einzelnen Registerwert aus Data rauskopieren
            sRespData:=Copy (sRespDataGesamt, iPos, iLen);
            if length (sRespData) = iLen then begin
              // Binärdaten in formatierten Registerwert wandeln und in
              // Registerliste eintragen:
              TRegisterKonvDataObj (RegisterKonvList [i]).Data.Wert:=
                Modbus_BinData2Str (sRespData, ByteOrder, sRegisterTyp);
            end;  { if length (sRespData) = iLen }
          end;  { for i }
        end;  { if Assigned (RegisterKonvList) }

        { 03-Response ist OK: }
        Fehlergruppe:=0;
        Fehlercode:=0;
        Result:=true;
      end else
        Fehlercode:=MODBUSERR_RESP_BYTECOUNT_SOLL_IST;  { Soll/Ist-Fehler Bytecount }
    end else
      Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Response }
  end else
    Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Response }
end;

{-----------------------------------------------------------------------------------}
function Get_Modbus_Response_04 (Response: string; Modus: TModbusModus;
  ByteOrder: TByteOrder; RegisterKonvList: TRegisterKonvList; AnzahlBytes_Soll: byte;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{-----------------------------------------------------------------------------------}
{ liefert Registerwerte aus Modbus-Response, Funktion 04 (Read Input Registers);
  Übergabe: Response-String
            Modbus-Modus (ASCII/RTU)
            Byte-Order (Big-Endian/Little-Endian)
            Liste mit Register-Konvertierungsdaten (enthält als Rückgabe die Registerwerte)
            Soll-Anzahl Daten-Bytes in der Response
  Rückgabe: Fehlergruppe, Fehlercode
  Ergebnis: true, wenn Registerwerte aus Response gelesen und Registertyp-abhängig
            in Strings konvertiert werden konnten }
begin
  // wie für Funktion 03:
  Result:=Get_Modbus_Response_03 (Response, Modus, ByteOrder, RegisterKonvList,
                                  AnzahlBytes_Soll, Fehlergruppe, Fehlercode);
end;

{-----------------------------------------------------------------------}
function Get_Modbus_Response_05 (Response: string; Modus: TModbusModus;
  CoilAdresse_Soll: word; BinCoilData_Soll: string; var bPreset: boolean;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{-----------------------------------------------------------------------}
{ liefert Ergebnis des Coil-Setzens aus Modbus-Response, Funktion 05 (Force
  Single Coil);
  Übergabe: Response-String
            Modbus-Modus (ASCII/RTU)
            in Response erwartete Coil-Adresse (indiziert, bei 0 beginnend)
            in Response erwartete Binärdaten (Wert, mit dem versucht wurde, den
                                              Coil zu setzen)
  Rückgabe: Flag 'bPreset' (wenn true, Coil-Setzen erfolgreich (Soll = Ist für
                            Coil-Adresse und Binärdaten)
            Fehlergruppe, Fehlercode
  Ergebnis: true, wenn Coil-Adresse und Binärdaten aus Response gelesen werden
            konnten }
begin
  // wie für Funktion 06:
  Result:=Get_Modbus_Response_06 (Response, Modus,
                                  CoilAdresse_Soll, BinCoilData_Soll, bPreset,
                                  Fehlergruppe, Fehlercode);
end;

{------------------------------------------------------------------------------}
function Get_Modbus_Response_06 (Response: string; Modus: TModbusModus;
  RegisterAdresse_Soll: word; BinRegisterData_Soll: string; var bPreset: boolean;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{------------------------------------------------------------------------------}
{ liefert Ergebnis des Register-Setzens aus Modbus-Response, Funktion 06 (Preset
  Single Register);
  Übergabe: Response-String
            Modbus-Modus (ASCII/RTU)
            in Response erwartete Registeradresse (indiziert, bei 0 beginnend)
            in Response erwartete Binärdaten (Wert, mit dem versucht wurde, das
                                              Register zu setzen)
  Rückgabe: Flag 'bPreset' (wenn true, Register-Setzen erfolgreich (Soll = Ist für
                            Registeradresse und Binärdaten)
            Fehlergruppe, Fehlercode
  Ergebnis: true, wenn Registeradresse und Binärdaten aus Response gelesen werden
            konnten }
var
  sRespBin: string;
  S: string;
  iRespRegisterAdresse: word;
  sRespBinRegisterData: string;

begin
  Result:=false;
  bPreset:=false;  // Vorbelegung Rückgabe

  sRespBin := Cut_Modbus_Response_SlaveAdr_FktCode_Data (Response, Modus);  // 13.11.2019, WW

  Fehlergruppe:=COM_MODBUSERROR;  // Fehlergruppe: Modbus-Fehler
  sRespBin:=Copy (sRespBin, 3, length (sRespBin));  // Slave-Adresse und Funktionscode abschneiden
  if length (sRespBin) = 4 then begin
    S:=Copy (sRespBin, 1, 2);
    iRespRegisterAdresse:=Bin2Word (S, bo_BigEndian);  // Adresse des Registers immer Big-Endian
    sRespBinRegisterData:=Copy (sRespBin, 3, 2);
    bPreset:=(RegisterAdresse_Soll = iRespRegisterAdresse) AND
             (BinRegisterData_Soll = sRespBinRegisterData);

    { 06-Response ist OK: }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Result:=true;
  end else
    Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Response }
end;

{------------------------------------------------------------------------------}
function Get_Modbus_Response_16 (Response: string; Modus: TModbusModus;
  StartRegister_Soll, AnzahlRegister_Soll: word; var bPreset: boolean;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{------------------------------------------------------------------------------}
{ liefert Ergebnis des Register-Setzens aus Modbus-Response, Funktion 16 (Preset
  Multiple Registers);
  Übergabe: Response-String
            Modbus-Modus (ASCII/RTU)
            in Response erwartete Register-Startadresse (indiziert, bei 0 beginnend)
            in Response erwartete Anzahl der Register
  Rückgabe: Flag 'bPreset' (wenn true, Register-Setzen erfolgreich (Soll = Ist für
                            Register-Startadresse und Anzahl der Register)
            Fehlergruppe, Fehlercode
  Ergebnis: true, wenn Register-Startadresse und Anzahl der Register aus Response
            gelesen werden konnten }
var
  sRespBin: string;
  S: string;
  iRespStartRegister: word;
  iRespAnzahlRegister: word;

begin
  Result:=false;
  bPreset:=false;  // Vorbelegung Rückgabe

  sRespBin := Cut_Modbus_Response_SlaveAdr_FktCode_Data (Response, Modus);  // 13.11.2019, WW

  Fehlergruppe:=COM_MODBUSERROR;  // Fehlergruppe: Modbus-Fehler
  sRespBin:=Copy (sRespBin, 3, length (sRespBin));  // Slave-Adresse und Funktionscode abschneiden
  if length (sRespBin) = 4 then begin
    // Start und Anzahl der Register immer Big-Endian:
    S:=Copy (sRespBin, 1, 2);
    iRespStartRegister:=Bin2Word (S, bo_BigEndian);
    S:=Copy (sRespBin, 3, 2);
    iRespAnzahlRegister:=Bin2Word (S, bo_BigEndian);
    bPreset:=(StartRegister_Soll = iRespStartRegister) AND
             (AnzahlRegister_Soll = iRespAnzahlRegister);

    { 16-Response ist OK: }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Result:=true;
  end else
    Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Response }
end;

{-----------------------------------------------------------------------------------}
function Get_Modbus_Response_20 (Response: string; Modus: TModbusModus;
  ByteOrder: TByteOrder; RegisterKonvList: TRegisterKonvList; AnzahlBytes_Soll: byte;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{-----------------------------------------------------------------------------------}
{ liefert Registerwerte aus Modbus-Response, Funktion 20 (Read General Reference)
  mit einem einzelnen Sub-Response;
  Übergabe: Response-String
            Modbus-Modus (ASCII/RTU)
            Byte-Order (Big-Endian/Little-Endian)
            Liste mit Register-Konvertierungsdaten (enthält als Rückgabe die Registerwerte)
            Soll-Anzahl Daten-Bytes in der Sub-Response
  Rückgabe: Fehlergruppe, Fehlercode
  Ergebnis: true, wenn Registerwerte aus Response gelesen und Registertyp-abhängig
            in Strings konvertiert werden konnten }
var
  sRespBin: string;
  iRespByteCount: byte;
  iSubRespByteCount: byte;
  sRespData: string;
  sSubRespGesamt: string;
  sSubResp: string;
  sSubRespData: string;
  iLenSubRespData: integer;
  i: integer;
  iPos, iLen: integer;
  sRegisterTyp: string;
  iStartAddr, iAddr : word;

begin
  Result:=false;

  if Assigned (RegisterKonvList) then
    for i:=0 to RegisterKonvList.Count - 1 do
      TRegisterKonvDataObj (RegisterKonvList [i]).Data.Wert:='';  // Vorbelegung Rückgabe

  sRespBin := Cut_Modbus_Response_SlaveAdr_FktCode_Data (Response, Modus);

  Fehlergruppe:=COM_MODBUSERROR;  // Fehlergruppe: Modbus-Fehler
  if length (sRespBin) >= 3 then begin
    iRespByteCount:=Bin2Byte (sRespBin [3]);
    if length (sRespBin) >= (3 + iRespByteCount) then begin
      // Alle Sub-Responses ausschneiden
      sSubRespGesamt:=Copy (sRespBin, 4, length (sRespBin));

      // Sub-Response 1 ausschneiden
      if length (sSubRespGesamt) > 0 then begin
        iSubRespByteCount:=Bin2Byte (sSubRespGesamt [1]);  // Byte Count der Sub-Response
        sSubResp:=Copy (sSubRespGesamt, 1, iSubRespByteCount + 1);
      end else
        sSubResp:='';

      // Data der Sub-Response ausschneiden
      sSubRespData:=Copy (sSubResp, 3, length (sSubResp));  // 2 Bytes wegschneiden: Byte Count, Referenztyp
      iLenSubRespData:=length (sSubRespData);

      if iLenSubRespData = AnzahlBytes_Soll then begin  // auf Data-ByteCount Ist = Soll prüfen (>= zu lasch !)
        // Für alle in der Register-Konvertierungsdatenliste enthaltenen Register
        // die im Data-String der Sub-Response enthaltenen Registerwerte konvertieren:
        if Assigned (RegisterKonvList) and (RegisterKonvList.Count > 0) then begin
          // Register-Adresse des ersten Listeneintrags:
          iStartAddr := TRegisterKonvDataObj(RegisterKonvList[0]).Data.StartAdresse;
          for i:=0 to RegisterKonvList.Count - 1 do begin
            iAddr := TRegisterKonvDataObj(RegisterKonvList[i]).Data.StartAdresse;
            sRegisterTyp:=TRegisterKonvDataObj (RegisterKonvList [i]).Data.Typ;
            // Position im Data-String:
            iPos := 1 + Calc_Modbus_AnzahlBytes(iAddr - iStartAddr);
            iLen:=TRegisterKonvDataObj (RegisterKonvList [i]).Data.AnzahlBytes;
            // einzelnen Registerwert aus Data rauskopieren
            sRespData:=Copy (sSubRespData, iPos, iLen);
            if length (sRespData) = iLen then begin
              // Binärdaten in formatierten Registerwert wandeln und in
              // Registerliste eintragen:
              TRegisterKonvDataObj (RegisterKonvList [i]).Data.Wert:=
                Modbus_BinData2Str (sRespData, ByteOrder, sRegisterTyp);
            end;  { if length (sRespData) = iLen }
          end;  { for i }
        end;  { if Assigned (RegisterKonvList) }

        { 20-Response ist OK: }
        Fehlergruppe:=0;
        Fehlercode:=0;
        Result:=true;
      end else
        Fehlercode:=MODBUSERR_RESP_BYTECOUNT_SOLL_IST;  { Soll/Ist-Fehler Bytecount }
    end else
      Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Response }
  end else
    Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Response }
end;

{---------------------------------------------------------------------------------}
function Get_Modbus_Response_52 (Response: string; Modus: TModbusModus;
  ByteOrder: TByteOrder; AnzahlBytes_Soll: byte;
  var sValue: string; var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{---------------------------------------------------------------------------------}
{ liefert Registerwert aus Modbus-Response, Funktion 52 (Read RMG from to);
  Übergabe: Response-String
            Modbus-Modus (ASCII/RTU)
            Byte-Order (Big-Endian/Little-Endian)
            Soll-Anzahl Daten-Bytes für Registerwert
  Rückgabe: Register-Rohdaten
            Fehlergruppe, Fehlercode
  Ergebnis: true, wenn Register-Rohdaten aus Response gelesen werden konnten }
var
  sRespBin: string;
  iRespByteCount: byte;
  sRespData: string;

begin
  Result:=false;
  sValue:='';  // Vorbelegung Rückgabe

  sRespBin := Cut_Modbus_Response_SlaveAdr_FktCode_Data (Response, Modus);  // 13.11.2019, WW

  Fehlergruppe:=COM_MODBUSERROR;  // Fehlergruppe: Modbus-Fehler
  if length (sRespBin) >= 3 then begin
    iRespByteCount:=Bin2Byte (sRespBin [3]);
    if length (sRespBin) >= (3 + iRespByteCount) then begin
      if iRespByteCount = AnzahlBytes_Soll then begin  // auf Data-ByteCount Ist = Soll prüfen (>= zu lasch !); 13.11.2019, WW
        sRespData:=Copy (sRespBin, 4, AnzahlBytes_Soll);  // Data ausschneiden
        sValue:=sRespData;  // Rohformat

        { 52-Response ist OK: }
        Fehlergruppe:=0;
        Fehlercode:=0;
        Result:=true;
      end else
        Fehlercode:=MODBUSERR_RESP_BYTECOUNT_SOLL_IST;  { Soll/Ist-Fehler Bytecount }
    end else
      Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Response }
  end else
    Fehlercode:=MODBUSERR_RESP_INCOMPLETE;  { unvollständige Response }
end;


// 08.03.2019, WW
{------------------------------------------------------------------------------}
function Get_Modbus_StartAdresse_Index0 (iStartAdresse: word): word;
{------------------------------------------------------------------------------}
{ Liefert die indizierte Startadresse, bei 0 beginnend;
  Übergabe: Startadresse (bei 1 beginnend)
  Ergebnis: Indizierte Startadresse }
begin
  if iStartAdresse > 0 then
    Result:=iStartAdresse - 1
  else
    Result:=MaxWord;
end;

{------------------------------------------------------------------------------}
function Get_Modbus_BinData (sWert: string; ByteOrder: TByteOrder;
  WertTyp: string; Anzahl: word; var sBinData: string;
  var sInfoWert_Aktuell: string): integer;
{------------------------------------------------------------------------------}
{ Wandelt typabhängig einen Wert-String in Binärdaten-Format. Falls der Wert
  einen Platzhalter enthält, wird dieser durch einen aktuellen Wert ersetzt; 28.06.2017, WW
  Übergabe: Wert als String
            Byte-Order
            Typ des Wertes
            Anzahl der Register
  Rückgabe: Wert in Binärdaten-Format
            Aktueller Wert zur Info
  Ergebnis: siehe Str2Modbus_BinData }
var
  iAnzahlBytes: word;
  S: string;
  dtNow: TDateTime;
//  sBinBuf: string;
  w: word;

begin
  sBinData:='';  // Vorbelegung Rückgabe
  sInfoWert_Aktuell:=sWert;  // Vorbelegung Rückgabe

  iAnzahlBytes:=Calc_Modbus_AnzahlBytes (Anzahl);
  S:=sWert;

  // Platzhalter-Wert durch aktuellen Wert ersetzen:
  if UpperCase(S) = '<SYSTIME_DOUBLE>' then begin
    dtNow:=Now;
    sInfoWert_Aktuell:=FormatDateTime(C_FormatDateTime, dtNow);  // Rückgabe aktueller Wert

    // PC-Zeit als Double, codiert als Delphi-TDateTime (für Vympel-Geräte)
    S:=FloatToStr(dtNow);
    { Double-String in Binärdaten-Format wandeln: }
    Result:=Str2Modbus_BinData (S, ByteOrder, 'E', 4, sBinData);
  end

  (* Auskommentiert: Zeitsynchronisation Siemens GC als Blockparametrierung wird
     vom Gerät nicht korrekt verarbeitet (Grund unbekannt; Informationen dazu sind
     seitens Siemens nicht verfügbar); 07.01.2020, WW
  else if UpperCase(S) = '<SYSTIME_YMDHNS_W+1>' then begin
    dtNow:=Now;
    sInfoWert_Aktuell:=FormatDateTime(C_FormatDateTime, dtNow);  // Rückgabe aktueller Wert

    // PC-Zeit Jahr, Monat, Tag, Stunde, Minute, Sekunde jeweils als Word mit
    // Offset +1 (für Siemens GC)
    { Jahr: }
    w:=YearOf(dtNow) + 1;
    S:=IntToStr(w);
    { Word-String in Binärdaten-Format wandeln: }
    Result:=Str2Modbus_BinData (S, ByteOrder, C_MBWertTyp_W, 2, sBinData);

    if Result = 0 then begin
      { Monat: }
      w:=MonthOf(dtNow) + 1;
      S:=IntToStr(w);
      { Word-String in Binärdaten-Format wandeln: }
      Result:=Str2Modbus_BinData (S, ByteOrder, C_MBWertTyp_W, 2, sBinBuf);
      sBinData:=sBinData + sBinBuf;
    end;

    if Result = 0 then begin
      { Tag: }
      w:=DayOf(dtNow) + 1;
      S:=IntToStr(w);
      { Word-String in Binärdaten-Format wandeln: }
      Result:=Str2Modbus_BinData (S, ByteOrder, C_MBWertTyp_W, 2, sBinBuf);
      sBinData:=sBinData + sBinBuf;
    end;

    if Result = 0 then begin
      { Stunde: }
      w:=HourOf(dtNow) + 1;
      S:=IntToStr(w);
      { Word-String in Binärdaten-Format wandeln: }
      Result:=Str2Modbus_BinData (S, ByteOrder, C_MBWertTyp_W, 2, sBinBuf);
      sBinData:=sBinData + sBinBuf;
    end;

    if Result = 0 then begin
      { Minute: }
      w:=MinuteOf(dtNow) + 1;
      S:=IntToStr(w);
      { Word-String in Binärdaten-Format wandeln: }
      Result:=Str2Modbus_BinData (S, ByteOrder, C_MBWertTyp_W, 2, sBinBuf);
      sBinData:=sBinData + sBinBuf;
    end;

    if Result = 0 then begin
      { Sekunde: }
      w:=SecondOf(dtNow) + 1;
      S:=IntToStr(w);
      { Word-String in Binärdaten-Format wandeln: }
      Result:=Str2Modbus_BinData (S, ByteOrder, C_MBWertTyp_W, 2, sBinBuf);
      sBinData:=sBinData + sBinBuf;
    end;
  end *)

  // Zeitsynchronisation Siemens GC, Einzelparametrierung; 07.01.2020, WW
  else if UpperCase(S) = '<SYSTIME_Y_W>' then begin
    dtNow:=Now;
    sInfoWert_Aktuell:=FormatDateTime('yyyy', dtNow);  // Rückgabe aktueller Wert Jahr

    // PC-Zeit Jahr als Word (für Siemens GC)
    { Jahr: }
    w:=YearOf(dtNow);
    S:=IntToStr(w);
    { Word-String in Binärdaten-Format wandeln: }
    Result:=Str2Modbus_BinData (S, ByteOrder, C_MBWertTyp_W, 2, sBinData);
  end
  else if UpperCase(S) = '<SYSTIME_M_W>' then begin
    dtNow:=Now;
    sInfoWert_Aktuell:=FormatDateTime('mm', dtNow);  // Rückgabe aktueller Wert Monat

    // PC-Zeit Monat als Word (für Siemens GC)
    { Monat: }
    w:=MonthOf(dtNow);
    S:=IntToStr(w);
    { Word-String in Binärdaten-Format wandeln: }
    Result:=Str2Modbus_BinData (S, ByteOrder, C_MBWertTyp_W, 2, sBinData);
  end
  else if UpperCase(S) = '<SYSTIME_D_W>' then begin
    dtNow:=Now;
    sInfoWert_Aktuell:=FormatDateTime('dd', dtNow);  // Rückgabe aktueller Wert Tag

    // PC-Zeit Tag als Word (für Siemens GC)
    { Tag: }
    w:=DayOf(dtNow);
    S:=IntToStr(w);
    { Word-String in Binärdaten-Format wandeln: }
    Result:=Str2Modbus_BinData (S, ByteOrder, C_MBWertTyp_W, 2, sBinData);
  end
  else if UpperCase(S) = '<SYSTIME_H_W>' then begin
    dtNow:=Now;
    sInfoWert_Aktuell:=FormatDateTime('hh', dtNow);  // Rückgabe aktueller Wert Stunde

    // PC-Zeit Stunde als Word (für Siemens GC)
    { Stunde: }
    w:=HourOf(dtNow);
    S:=IntToStr(w);
    { Word-String in Binärdaten-Format wandeln: }
    Result:=Str2Modbus_BinData (S, ByteOrder, C_MBWertTyp_W, 2, sBinData);
  end
  else if UpperCase(S) = '<SYSTIME_N_W>' then begin
    dtNow:=Now;
    sInfoWert_Aktuell:=FormatDateTime('nn', dtNow);  // Rückgabe aktueller Wert Minute

    // PC-Zeit Minute als Word (für Siemens GC)
    { Minute: }
    w:=MinuteOf(dtNow);
    S:=IntToStr(w);
    { Word-String in Binärdaten-Format wandeln: }
    Result:=Str2Modbus_BinData (S, ByteOrder, C_MBWertTyp_W, 2, sBinData);
  end
  else if UpperCase(S) = '<SYSTIME_S_W>' then begin
    dtNow:=Now;
    sInfoWert_Aktuell:=FormatDateTime('ss', dtNow);  // Rückgabe aktueller Wert Sekunde

    // PC-Zeit Sekunde als Word (für Siemens GC)
    { Sekunde: }
    w:=SecondOf(dtNow);
    S:=IntToStr(w);
    { Word-String in Binärdaten-Format wandeln: }
    Result:=Str2Modbus_BinData (S, ByteOrder, C_MBWertTyp_W, 2, sBinData);
  end

  else begin
    { Wert-String typabhängig in Binärdaten-Format wandeln: }
    Result:=Str2Modbus_BinData (S, ByteOrder, WertTyp, iAnzahlBytes, sBinData);
  end;
end;

end.
