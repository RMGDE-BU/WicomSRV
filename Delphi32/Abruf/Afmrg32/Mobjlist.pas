{******************************************************************************}
{* Unit: Listen-Objekte zur MRG-Datenkonvertierung                            *}
{* 03.12.98 WW                                                                *}
{* 18.01.00 WW  Rohfile nur bis ETX lesen                                     *}
{* 18.02.21 WW  TME400-Archivheaderliste                                      *}
{******************************************************************************}
Unit MObjList;

INTERFACE

Uses
  SysUtils, Classes, Forms, Contnrs, WStrUtils, ModbusMasterRes, ModbusUtil,
  T_BinMask;

{$B-}

Type

  TTextListe = class (TStringList)
    constructor Create;
  End;

  TRohTextListe = class (TTextListe)
    Constructor LoadFromMeldRohFile (FileName: TFileName);
  End;

  TRohFileListe = class (TTextListe)
    function DeleteRohFiles: boolean;
  End;


  { Record für TME400-Archivheader-Datensatz }
  TTME400Archivheader = record
    OrdNr: word;
    Index_aeltest: word;
    Index_neuest: word;
    CRC16: word;
  end;

  { Objekt für TME400-Archivheader-Datensatz }
  TTME400ArchivheaderObj = class (TObject)
    Data: TTME400Archivheader;
  public
    procedure SetData (AData: TTME400Archivheader);
  end;

  { Objektliste für TME400-Archivheader-Datensätze }
  TTME400ArchivheaderList = class (TObjectList)
  public
    function LoadFromModbusRegisterRequestList (
      RegisterRequestListe: TRegisterRequestList; ByteOrder: TByteOrder): boolean;
    function GetHeaderAktuell (var HeaderAkt: TTME400Archivheader): boolean;
  end;


function Init_TME400Archivheader: TTME400Archivheader;

IMPLEMENTATION

{ TTextListe }

{----------------------------}
constructor TTextListe.Create;
{----------------------------}
Begin
  inherited Create;
  Duplicates := dupAccept;
  Sorted := false;
End;

{ TRohTextListe }

{------------------------------------------------------------------}
Constructor TRohTextListe.LoadFromMeldRohFile (FileName: TFileName);
{------------------------------------------------------------------}
{ Meldungsrohfile lesen, Rohsätze in Liste eintragen;
  Übergabe: Rohfilename }
Const
  szLen_RohData = 100;

Var
  FS : TFileStream;
  Zeichen : Char;
  OldPos : LongInt;
  AnzGelesen : LongInt;
  Ende : Boolean;
  Len : LongInt;
  Buffer : array [0..szLen_RohData] of char;

Begin
  Inherited Create;
  if FileExists (FileName) then begin
    FS := TFileStream.Create (FileName, fmOpenRead OR fmShareDenyWrite);
    try
      FS.Read (Zeichen, 1);  { STX einlesen }
      FS.Read (Zeichen, 1);  { Identifikationsbuchstaben einlesen }
      OldPos := FS.Position;
      Ende := False;
      While (Not Ende) AND (Count < MaxListSize) Do
      Begin
        AnzGelesen:=FS.Read (Zeichen, 1);
        Ende := (AnzGelesen < 1) OR (Zeichen = #$3);
        If Ende or (Zeichen < #$20) Then
        Begin
          Len := FS.Position - OldPos - 1;
          If (Len > 0) AND (Len <= szLen_RohData) Then
          Begin
            FS.Seek (OldPos, soFromBeginning);
            FS.Read (Buffer, Len);
            Buffer [Len] := #0;
            Add (StrPas(Buffer));
            FS.Read (Zeichen, 1);
          End;
          OldPos := FS.Position;
        End;
      End;
    finally
      FS.Free;
    end;
  end;
End;


{ TRohFileListe }

{---------------------------------------------}
function TRohFileListe.DeleteRohFiles: boolean;
{---------------------------------------------}
{ löscht in Liste enthaltene Rohdateien;
  Ergebnis: true, wenn Löschen aller Rohdateien erfolgreich }
var
  i: integer;
  S: string;

begin
  Result:=true;
  for i:=0 to Count - 1 do begin
    S:=Strings [i];
    if Pos (';', S) > 0 then  { optionales Trennzeichen bei vorangestellter Zusatzinformation }
      F_Zerlegen (S, ';');
    if not DeleteFile (S) then
      Result:=false;
  end;
end;


{------------------------ Archivheader TME400 ---------------------------------}
{ -> Auch für RSM200                                                           }

{------------------------------------------------------------------------------}
function TME400Archivheader_OrdNr_Desc_Compare (Item1, Item2: Pointer): Integer;
{------------------------------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TTME400Archivheader-Objekten nach der
  Ordnungsnummer absteigend }
var
  OrdNr1: word;
  OrdNr2: word;

begin
  OrdNr1:=TTME400ArchivheaderObj (Item1).Data.OrdNr;
  OrdNr2:=TTME400ArchivheaderObj (Item2).Data.OrdNr;

  if OrdNr1 < OrdNr2 then
    Result:=1
  else if OrdNr1 > OrdNr2 then
    Result:=-1
  else
    Result:=0;
end;

{----------------------------------------------------}
function Init_TME400Archivheader: TTME400Archivheader;
{----------------------------------------------------}
{ Liefert mit 'Leer, fehlend' vorbelegten TME400-Archivheaderdatensatz }
begin
  FillChar (Result, SizeOf (Result), $FFFF);  // "Leer, fehlend"
end;


{ TTME400ArchivheaderObj }

{--------------------------------------------------------------------}
procedure TTME400ArchivheaderObj.SetData (AData: TTME400Archivheader);
{--------------------------------------------------------------------}
begin
  Data:=AData;
end;


{ TTME400ArchivheaderList }

{----------------------------------------------------------------------------}
function TTME400ArchivheaderList.LoadFromModbusRegisterRequestList (
  RegisterRequestListe: TRegisterRequestList; ByteOrder: TByteOrder): boolean;
{----------------------------------------------------------------------------}
{ TME400-Archivheaderliste mit den in der Modbus-Register-Requestliste enthaltenen
  Konvertierungslisten-Werten neu füllen;
  Übergaben: Register-Requestliste
             Modbus Byte-Order
  Ergebnis: true, wenn Werte erfolgreich in Liste konvertiert werden konnten }
var
  i, j: integer;
  RegisterKonvListe: TRegisterKonvList;
  RegisterKonvData: TRegisterKonvData;
  Archivheader: TTME400Archivheader;
  ArchivheaderObj: TTME400ArchivheaderObj;
  sBinData: string;
  CRC16: word;
  CRC16_calc: word;

begin
  Result:=true;
  Clear;

  if Assigned (RegisterRequestListe) then begin
    for i:=0 to RegisterRequestListe.Count - 1 do begin
      { Register-Konvliste enthält die Archivheader-Daten: }
      RegisterKonvListe:=
        TRegisterRequestDataObj (RegisterRequestListe [i]).Data.RegisterKonvListe;

      if Assigned (RegisterKonvListe) then begin
        for j:=0 to RegisterKonvListe.Count - 1 do begin
          Application.ProcessMessages;
          RegisterKonvData:=TRegisterKonvDataObj (RegisterKonvListe [j]).Data;

          try
            case RegisterKonvData.StartAdresse of
               0, 4, 8, 12:
                 begin
                   // Archivheader-Record neu initialisieren
                   Archivheader:=Init_TME400Archivheader;

                   Archivheader.OrdNr:=StrToInt (RegisterKonvData.Wert);  // Ordnungsnummer Header 0..3
                 end;

               1, 5, 9, 13:
                 Archivheader.Index_aeltest:=StrToInt (RegisterKonvData.Wert);  // Index ältester Eintrag Header 0..3

               2, 6, 10, 14:
                 Archivheader.Index_neuest:=StrToInt (RegisterKonvData.Wert);  // Index neuester Eintrag Header 0..3

               3, 7, 11, 15:
                 begin
                   Archivheader.CRC16:=StrToInt (RegisterKonvData.Wert);  // CRC16 Header 0..3

                   // Wenn ein CRC16-Fehler über die Felder Ordnungsnummer, Index ältester
                   // und Index neuester Eintrag erkannt wird: Archivheader-Datensatz
                   // darf nicht verwendet werden !
                   sBinData:=Word2Bin (Archivheader.OrdNr, ByteOrder) +
                             Word2Bin (Archivheader.Index_aeltest, ByteOrder) +
                             Word2Bin (Archivheader.Index_neuest, ByteOrder);
                   CRC16_calc:=Get_Modbus_CRC (sBinData);

                   if (ByteOrder = bo_BigEndian) OR
                      (ByteOrder = bo_LittleByteSwap) then
                     CRC16:=EndianWord (Archivheader.CRC16)
                   else
                     CRC16:=Archivheader.CRC16;

                   if CRC16 = CRC16_calc then begin  // nur wenn CRC16 OK
                     // Archivheader-Record in Liste eintragen
                     ArchivheaderObj:=TTME400ArchivheaderObj.Create;
                     ArchivheaderObj.SetData (Archivheader);
                     Add (ArchivheaderObj);
                   end;
                 end;
            end;  // case RegisterKonvData.StartAdresse
          except
            Result:=false;  // Fehler beim Konvertieren in Archivheaderliste
            exit;
          end;
        end;  // for j
      end;  // if Assigned (RegisterKonvListe)
    end;  // for i
  end;  // if Assigned (RegisterRequestListe)
end;

{-------------------------------------------------}
function TTME400ArchivheaderList.GetHeaderAktuell (
  var HeaderAkt: TTME400Archivheader): boolean;
{-------------------------------------------------}
{ Liefert den aktuellen von den in der Liste enthaltenen TME400-Archivheadern;
  Rückgabe: Aktueller Archivheader
  Ergebnis: True, wenn mind. 1 gültiger Archivheader in der Liste enthalten ist }
const
  C_MaxOrdNr = 9999;

begin
  Result:=false;
  HeaderAkt:=Init_TME400Archivheader;  // Vorbelegung Rückgabe

  // Liste nach Ordnungsnummer absteigend sortieren
  Sort (TME400Archivheader_OrdNr_Desc_Compare);

  if Count > 0 then begin          
    // Default: Archivheader mit der höchsten Ordnungsnummer
    HeaderAkt:=TTME400ArchivheaderObj (Items [0]).Data;

    if HeaderAkt.OrdNr = C_MaxOrdNr then begin
      // Überlauf der Ordnungsummer prüfen für die 4 Archivheader:
      if (Count > 1) AND
         (TTME400ArchivheaderObj (Items [0]).Data.OrdNr >
          (TTME400ArchivheaderObj (Items [1]).Data.OrdNr + 1)) then                   
        HeaderAkt:=TTME400ArchivheaderObj (Items [1]).Data  // 9999, 2, 1, 0
      else if (Count > 2) AND
         (TTME400ArchivheaderObj (Items [1]).Data.OrdNr >
          (TTME400ArchivheaderObj (Items [2]).Data.OrdNr + 1)) then
        HeaderAkt:=TTME400ArchivheaderObj (Items [2]).Data  // 9999, 9998, 1, 0
      else if (Count > 3) AND
         (TTME400ArchivheaderObj (Items [2]).Data.OrdNr >
          (TTME400ArchivheaderObj (Items [3]).Data.OrdNr + 1)) then
        HeaderAkt:=TTME400ArchivheaderObj (Items [2]).Data  // 9999, 9998, 9997, 0
    end;
    
    Result:=true;
  end;
end;

End.
