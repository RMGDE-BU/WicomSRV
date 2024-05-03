{------------------------------------------------------------------------------}
{ Objekt f�r serielle Kommunikation mit Encodereingang lt. technischer Spezifi-}
{ kation "Digitale Schnittstelle f�r Prim�rger�te", Version 1.1 vom April 1999 }
{ Funktionen: - Schnittstelle �ffnen                                           }
{             - Datenframe "Z�hlwerkstand" senden                              }
{             - Schnittselle schlie�en                                         }
{                                                                              }
{ 18.07.2008  WW                                                               }
{ 25.02.2009  WN  Encoder-Signal ohne DTR und RTS                              }
{                                                                              }
{ Copyright (C) RMG Messtechnik GmbH 2008, 2009                                }
{------------------------------------------------------------------------------}
unit O_SerEncoder;

interface

uses
  Windows, SysUtils, Forms, Classes, ExtCtrls, Math, Serial, LogCom, WChars,
  WStrUtils;

type
  { Callback-Prozedurtypen zur Anzeige in Fremdmodulen }
  TCBSerEncoderDataProc = procedure (Data: string; Bytes: string) of object;
  TCBSerEncoderStringMsgProc = procedure (Msg: string) of object;

  { Objekt f�r serielle Kommunikation mit Encodereingang }
  TSerialEncoder = class(TSerial)
  private
    { Private-Deklarationen }
    ComLogFile: TComLogFile;
    FTxD: TCBSerEncoderDataProc;
    function OpenCOM: boolean;
    procedure CloseCOM;
    function GetBCC (s: string): byte;
  public
    { Public-Deklarationen }
    constructor Create (AOwner: TComponent; AComLogFile: TComLogFile); reintroduce;
    destructor Destroy; override;
    function Connect (AComPort: integer): integer;
    function SendZaehlerstand (dZaehlerstand: double; iMaxNachkommastellen: byte;
      sEinheit: string): shortint;
    property CBTxD: TCBSerEncoderDataProc read FTxD write FTxD;
  end;

implementation

{-------------------------------------------------------------------------------}
constructor TSerialEncoder.Create (AOwner: TComponent; AComLogFile: TComLogFile);
{-------------------------------------------------------------------------------}
{ �bergabe: �bergeordnete Komponente
            Zeiger auf COM-Logfile (nil: es wird kein Logfile geschrieben) }
begin
  inherited Create(AOwner);
  ComLogFile:=AComLogFile;

  { DTR- und RTS-Leitung setzen: }
  DTRActive:=False;  // 25.02.2009  WN -> False bei Kommunikation mit EC900
  RTSActive:=False;  //                   ben�tigt
end;

{--------------------------------}
destructor TSerialEncoder.Destroy;
{--------------------------------}
begin
  CloseCOM;
  inherited Destroy;
end;

{---------------------------------------}
function TSerialEncoder.OpenCOM: boolean;
{---------------------------------------}
{ Schnittstelle �ffnen }
begin
  Result:=true;
  if ComLogFile <> nil then
    ComLogFile.WriteMsg ('COM �ffnen...');  { Logfileeintrag }

  if not OpenComm then begin
    Result:=false;
    exit;
  end;

  if ComLogFile <> nil then
    ComLogFile.WriteMsg ('COM ist ge�ffnet');  { Logfileeintrag }
end;

{--------------------------------}
procedure TSerialEncoder.CloseCOM;
{--------------------------------}
{ Schnittstelle schlie�en }
begin
  if Active then begin
    if ComLogFile <> nil then
      ComLogFile.WriteMsg ('COM schlie�en...');  { Logfileeintrag }

    CloseComm;

    if ComLogFile <> nil then
      ComLogFile.WriteMsg ('COM ist geschlossen');  { Logfileeintrag }
  end;
end;

{-----------------------------------------------------------}
function TSerialEncoder.Connect (AComPort: integer): integer;
{-----------------------------------------------------------}
{ Schnittstelle �ffnen;
  �bergabe: COM-Port
  Ergebnis:  0 = Schnittstelle konnte korrekt ge�ffnet werden
            -1 = Schnittstelle ist nicht vorhanden
            -2 = Schnittstelle konnte nicht ge�ffnet werden }
var
  i: integer;
  PortOK: boolean;

begin
  Result:=0;

  { pr�fen, ob COM vorhanden ist: }
  PortOK:=false;
  for i:=0 to Ports.Count-1 do begin
    PortOK:=Ports [i] = 'COM' + IntToStr (ACOMPort);
    if PortOK then Break;
  end;
  if not PortOK then begin
    if ComLogFile <> nil then
      ComLogFile.WriteMsg ('COM nicht vorhanden');  { Logfileeintrag }

    Result:=-1;
    exit;
  end;

  { Schnittstellen-Parameter belegen: }
  COMPort:=AComPort;
  Baudrate:=br_002400;
  DataBits:=db_7;
  ParityBit:=even;
  StopBits:=sb_1;

  { Schnittstelle �ffnen: }
  if not OpenCOM then begin
    Result:=-2;
    exit;
  end;
end;

{--------------------------------------------------------------}
function TSerialEncoder.SendZaehlerstand (dZaehlerstand: double;
  iMaxNachkommastellen: byte; sEinheit: string): shortint;
{--------------------------------------------------------------}
{ Datenframe "Z�hlwerkstand" senden;
  �bergabe: Z�hlerstand
            max. Anzahl an Nachkommastellen
            Einheit
  Ergebnis:  0 = Versenden erfolgreich
            -1 = �bergebener Z�hlerstand ist au�erhalb des Wertebereichs
            -2 = Fehler beim Versenden }
const
  C_MaxVorkommastellen = 14;  // lt. Spezifiaktion

  C_ZaehlerStatus = #$30;  // kein Fehler

var
  sBefehl: string;
  sZaehler: string;
  sWertigkeit: string;
  BytesWritten: cardinal;
  d: double;
  i: integer;
  i64: Int64;
  iNachkomma: integer;
  S: string;

begin
  d:=dZaehlerstand;
  if Frac (d) > 0 then
    d:=RoundTo (d, iMaxNachkommastellen * (-1));  // auf max. Nachkommastellen begrenzen

  // �bergebenen Z�hlerstand f�r Datenframe formatieren (Z�hlerstand, Wertigkeit):
  S:=DoubleToStrWithKomma (d, iMaxNachkommaStellen);
  StrSubst (S, ',', '.');
  F_Zerlegen (S, '.');
  iNachkomma:=length (S);
  if iNachkomma > 9 then begin
    Result:=-1;  // Z�hlerstand au�erhalb des Wertebereichs (Wertigkeit mehrstellig)
    exit;
  end;
  for i:=1 to iNachkomma do
    d:=d * 10;

  i64:=Trunc (d);
  sZaehler:=IntToStr (i64);
  if length (sZaehler) > C_MaxVorkommastellen then begin
    Result:=-1;  // Z�hlerstand au�erhalb des Wertebereichs (zu gro�)
    exit;
  end;
  sZaehler:=F_LeftPad (sZaehler, '0', C_MaxVorkommastellen);  // mit f�hrenden Nullen auff�llen
  sWertigkeit:=IntToStr (iNachkomma * (-1));

  // Datenframe zusammensetzen:
  sBefehl:='a' + US + sZaehler + US + sWertigkeit + US +
           Copy (sEinheit, 1, 3) + US +
           C_ZaehlerStatus + FS;
  sBefehl:=sBefehl + char (GetBCC (sBefehl)) + CR + LF;  // mit BCC CR LF

  { TxD-Anzeige: }
  if Assigned (CBTxD) then
    CBTxD (SonderzeichenString (sBefehl), IntToStr (length (sBefehl)) + ' Byte');
  Application.ProcessMessages;

  if (length (sBefehl) > 0) AND (ComLogFile <> nil) then
    ComLogFile.Write ('S', sBefehl);  // Logfileeintrag mit Kopf "Sendedaten"

  BytesWritten:=TransmittText (sBefehl);  // Befehl in Sendepuffer schreiben
  if BytesWritten = cardinal (length (sBefehl)) then  // alle Zeichen versendet ?
    Result:=0  // OK
  else
    Result:=-2;  // Fehler beim Senden
end;

{-----------------------------------------------}
function TSerialEncoder.GetBCC (s: string): byte;
{-----------------------------------------------}
{ BCC berechnen;
  �bergabe: Daten-String, �ber den BCC gebildet werden soll
  Ergebnis: BCC }
var
  i: integer;
begin
  Result:=0;
  for i:=1 to length(s) do
    Result:=Result XOR Byte (s[i]);
end;

end.

