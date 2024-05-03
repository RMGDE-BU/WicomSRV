{******************************************************************************}
{* Unit: Zugriff auf Modbus-Master-Konfigurationsdateien                      *}
{* 24.02.2010  WW                                                             *}
{* 28.06.2017  WW mit Modbusfunktionen 01 (Read Coil Status), 02 (Read Input  *}
{*                Status), 04 (Read Input Registers), 06 (Preset Single Regis-*}
{*                ter), 16 (Preset Multiple Registers); Zusammenfassen der    *}
{*                Startadressen f�r Request optimiert                         *}
{* 08.03.2019  WW Bugfix LoadFromRegisterDataList: Bereich ohne L�cke         *}
{*                zusammenfassen                                              *}
{* 18.02.2021  WW mit Modbusfunktion 20 (Read General Reference)              *}
{******************************************************************************}
unit ModbusMasterRes;

interface

uses
  Forms, Classes, contnrs, SysUtils, T_BinMask, WStrUtils, WStream, T_Zeit,
  ModbusUtil;

type
  { Struktur f�r Register-Requestdaten }
  TRegisterRequestData = record
    FktCode: byte;  // ab 28.06.2017, WW
    StartAdresse: word;
    AnzahlBytes: integer; // f�r Register-Requests
    AnzahlBits: integer;  // f�r Status-Requests; ab 28.06.2017, WW
    RegisterKonvListe: TRegisterKonvList;
    Typ: string;   // f�r Einstell- und Einzelwert-Lese-Requests; ab 28.06.2017, WW
    Wert_Einstellen: string;  // f�r Einstell-Requests; ab 28.06.2017, WW
    FileNr: word;  // f�r General Reference-Requests (z.B Funktionscode 20); ab 18.02.2021, WW
  end;

  { Struktur f�r Registerdaten }
  TRegisterData = record
    RegisterRequestData: TRegisterRequestData;
    RegisterKonvData: TRegisterKonvData;
  end;

  { Objekt f�r Registerdaten }
  TRegisterDataObj = class (TObject)
    Data: TRegisterData;
  public
    procedure SetData (AData: TRegisterData);
  end;

  { Objektliste f�r Registerdaten }
  TRegisterDataList = class (TObjectList)
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    function AddRegisterData (iFktCode: byte; iStartAdresse: word;
      sName: string; sTyp: string; iAnzahlBytes: integer;
      sWert_Einstellen: string = ''; iFileNr: word = 0): boolean;
  end;

  { Objekt f�r Register-Requestdaten }
  TRegisterRequestDataObj = class (TObject)
    Data: TRegisterRequestData;
  public
    procedure SetData (AData: TRegisterRequestData);
  end;

  { Objektliste f�r Register-Requestdaten }
  TRegisterRequestList = class (TObjectList)
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    function LoadFromRegisterDataList (RDL: TRegisterDataList;
      bWrite: boolean): boolean;
  end;

  { Struktur f�r Slavedaten }
  TSlaveData = record
    SlaveAdresse: byte;
    ByteOrder: TByteOrder;
    PollingZyklus: integer;
    RegisterRequestListe: TRegisterRequestList;
    // f�r �berwachung des Polling-Zyklus:
    NextCallTime: TDateTime;  // n�chster Abrufzeitpunkt
    // f�r Ausf�hrung zu einer festen Uhrzeit:
    Uhrzeit: TDateTime;
  end;

  { Objekt f�r Slavedaten }
  TSlaveDataObj = class (TObject)
    Data: TSlaveData;
  public
    procedure SetData (AData: TSlaveData);
  end;

  { Objektliste f�r Slavedaten }
  TSlaveList = class (TObjectList)
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
  end;


function GetSlaveListe (COMNr: integer; SL: TSlaveList): integer;

implementation

const
  CTrenner = ';';


{------------------------------------------------------------}
function StartAdresseCompare (Item1, Item2: Pointer): Integer;
{------------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TRegisterData-Objekten nach der
  Startadresse der Requestdaten }
begin
  Result:=integer (TRegisterDataObj (Item1).Data.RegisterRequestData.StartAdresse) -
          integer (TRegisterDataObj (Item2).Data.RegisterRequestData.StartAdresse);
end;


{ TRegisterDataObj }

{--------------------------------------------------------}
procedure TRegisterDataObj.SetData (AData: TRegisterData);
{--------------------------------------------------------}
begin
  Data:=AData;
end;


{ TRegisterRequestDataObj }

{----------------------------------------------------------------------}
procedure TRegisterRequestDataObj.SetData (AData: TRegisterRequestData);
{----------------------------------------------------------------------}
begin
  Data:=AData;
end;


{ TSlaveDataObj }

{--------------------------------------------------}
procedure TSlaveDataObj.SetData (AData: TSlaveData);
{--------------------------------------------------}
begin
  Data:=AData;
end;


{ TRegisterDataList }

{-----------------------------------}
destructor TRegisterDataList.Destroy;
{-----------------------------------}
begin
  Clear;

  inherited Destroy;
end;

{--------------------------------}
procedure TRegisterDataList.Clear;
{--------------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if Assigned (TRegisterDataObj (Items [i]).Data.RegisterRequestData.RegisterKonvListe) then
      TRegisterDataObj (Items [i]).Data.RegisterRequestData.RegisterKonvListe.Free;  { Register-Konvertierungsliste freigeben }

  inherited Clear;
end;

{--------------------------------------------------}
procedure TRegisterDataList.Delete(iIndex: integer);
{--------------------------------------------------}
begin
  if Assigned (TRegisterDataObj (Items [iIndex]).Data.RegisterRequestData.RegisterKonvListe) then
    TRegisterDataObj (Items [iIndex]).Data.RegisterRequestData.RegisterKonvListe.Free;  { Register-Konvertierungsliste freigeben }

  inherited Delete(iIndex);
end;

{------------------------------------------------------------------------------}
function TRegisterDataList.AddRegisterData (iFktCode: byte; iStartAdresse: word;
  sName: string; sTyp: string; iAnzahlBytes: integer;
  sWert_Einstellen: string = ''; iFileNr: word = 0): boolean;
{------------------------------------------------------------------------------}
{ Eintrag der Registerdaten-Liste hinzuf�gen;
  �bergaben: Funktionscode
             Register-Startadresse
             Registerbezeichnung
             Typ des Registerwertes
             Anzahl Bytes des Registerwertes (f�r Status-Requests nicht erforderlich)
             Neuer Registerwert (nur f�r Einstell-Befehl erforderlich)
             Filenummer (nur f�r General Reference-Befehl erforderlich)
  Ergebnis: true, wenn Werte f�r Eintrag plausibel sind }
var
  RegisterData: TRegisterData;
  RegisterDataObj: TRegisterDataObj;
  RegisterRequestData: TRegisterRequestData;
  RegisterKonvData: TRegisterKonvData;

begin
  Result:=true;

  // Record f�r Register-Requestdaten belegen
  RegisterRequestData.FktCode:=iFktCode;
  RegisterRequestData.StartAdresse:=iStartAdresse;
  RegisterRequestData.AnzahlBytes:=iAnzahlBytes;
  RegisterRequestData.AnzahlBits:=1;  // Default f�r einen Status: Anzahl = 1 Bit
  RegisterRequestData.RegisterKonvListe:=nil;  // nicht verwendet
  RegisterRequestData.Typ:=sTyp;
  RegisterRequestData.Wert_Einstellen:=sWert_Einstellen;
  RegisterRequestData.FileNr:=iFileNr;  // 18.02.2021, WW

  // Record f�r Register-Konvertierungsdaten belegen
  RegisterKonvData.StartAdresse:=RegisterRequestData.StartAdresse;
  RegisterKonvData.Name:=sName;
  RegisterKonvData.Typ:=sTyp;
  RegisterKonvData.AnzahlBytes:=RegisterRequestData.AnzahlBytes;
  RegisterKonvData.Wert:='';  // Default, Wert fehlend (noch nicht konvertiert)
  RegisterKonvData.FktCode:=RegisterRequestData.FktCode;

  // F�r Funktionscodes 01 und 02 ist eine Angabe der Byte-Anzahl nicht
  // erforderlich (f�r die anderen Funktionscodes schon !):
  if not ((RegisterRequestData.FktCode = 1) OR
          (RegisterRequestData.FktCode = 2)) then begin
    if RegisterRequestData.AnzahlBytes <= -1 then
      Result:=false;
  end;

  { Listenobjekt f�r Registerdaten createn und in Registerdaten-Liste eintragen: }
  RegisterData.RegisterRequestData:=RegisterRequestData;
  RegisterData.RegisterKonvData:=RegisterKonvData;

  RegisterDataObj:=TRegisterDataObj.Create;
  RegisterDataObj.SetData (RegisterData);
  Add(RegisterDataObj);
end;               


{ TRegisterRequestList }

{--------------------------------------}
destructor TRegisterRequestList.Destroy;
{--------------------------------------}
begin
  Clear;

  inherited Destroy;
end;

{-----------------------------------}
procedure TRegisterRequestList.Clear;
{-----------------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if Assigned (TRegisterRequestDataObj (Items [i]).Data.RegisterKonvListe) then
      TRegisterRequestDataObj (Items [i]).Data.RegisterKonvListe.Free;  { Register-Konvertierungsliste freigeben }

  inherited Clear;
end;

{-----------------------------------------------------}
procedure TRegisterRequestList.Delete(iIndex: integer);
{-----------------------------------------------------}
begin
  if Assigned (TRegisterRequestDataObj (Items [iIndex]).Data.RegisterKonvListe) then
    TRegisterRequestDataObj (Items [iIndex]).Data.RegisterKonvListe.Free;  { Register-Konvertierungsliste freigeben }

  inherited Delete(iIndex);
end;

{-----------------------------------------------------------------------------}
function TRegisterRequestList.LoadFromRegisterDataList (RDL: TRegisterDataList;
  bWrite: boolean): boolean;
{-----------------------------------------------------------------------------}
{ In der Registerdaten-Liste enthaltene Einzel-Register zu geblockten Requests
  zusammenfassen und in Register-Requestliste eintragen;
  -> Hinweis: F�r General Reference-Requests (z.B. Funktionscode 20) wird nur
              1 Filenummer unterst�tzt. Die Blockung der Einzel-Register erfolgt
              OHNE Ber�cksichtigung der Filenummer !
  �bergaben: Registerdaten-Liste
             Flag 'bWrite' ja/nein: Auf nein setzen, wenn Einstell-Requests enthalten
               sind (dann wird nicht zusammengefa�t)
  Ergebnis: true, wenn Eintragen ohne Fehler }
var
  i, j, iIx: integer;
  RegisterRequestData: TRegisterRequestData;
  RegisterRequestDataObj: TRegisterRequestDataObj;
  RegisterKonvData: TRegisterKonvData;
  RegisterKonvDataObj: TRegisterKonvDataObj;
  bAdd: boolean;
  iCntByte: word;
  iCntBit: word;
  MaxDataBytes: integer;
  iAnzRegister: word;

begin
  Result:=true;                                 

  if Assigned (RDL) then begin
    // Registerdaten-Liste nach der Startadresse sortieren f�r maximales
    // Zusammenfassen hintereinanderliegender Register-Adressen f�r von-bis-Abfrage:   
    RDL.Sort(StartAdresseCompare);  // 28.06.2017, WW

    for j:=0 to RDL.Count - 1 do begin
      RegisterRequestData := TRegisterDataObj (RDL [j]).Data.RegisterRequestData;
      RegisterKonvData := TRegisterDataObj (RDL [j]).Data.RegisterKonvData;

      try
        { Listenobjekt f�r Register-Konvertierungsdaten createn: }
        RegisterKonvDataObj:=TRegisterKonvDataObj.Create;
        RegisterKonvDataObj.SetData (RegisterKonvData);

        bAdd:=true;
        // Kein Zusammenfassen bei Einstell-Requests:  28.06.2017, WW
        if not bWrite then begin
          if (Self.Count > 0) then begin
            // Ist ein Registereintrag f�r ein Zusammenfassen bereits vorhanden ?
            iIx := Self.Count;
            for i := Self.Count-1 downto 0 do begin
              iIx := i;  // Index zum Eintragen in RRL-Liste
              if (TRegisterRequestDataObj(Self[i]).Data.FktCode = RegisterRequestData.FktCode) AND
                 (TRegisterRequestDataObj(Self[i]).Data.StartAdresse <= RegisterRequestData.StartAdresse) then
                Break
              else
                Dec(iIx);
            end;

            if (iIx >= 0) then begin  // ist vorhanden
              if (TRegisterRequestDataObj(Self[iIx]).Data.FktCode = RegisterRequestData.FktCode) AND
                 (TRegisterRequestDataObj(Self[iIx]).Data.StartAdresse < RegisterRequestData.StartAdresse) then
              begin
                // Anzahl der Register des bisher zusammengefa�ten Bereichs:
                iAnzRegister:=Calc_Modbus_AnzahlRegister (TRegisterRequestDataObj(Self[iIx]).Data.AnzahlBytes);

                // Bereich "nahtlos", d.h. OHNE L�cke zusammenfassen; 08.03.2019, WW
                if (TRegisterRequestDataObj(Self[iIx]).Data.FktCode = RegisterRequestData.FktCode) AND
                   ((TRegisterRequestDataObj(Self[iIx]).Data.StartAdresse + iAnzRegister) = RegisterRequestData.StartAdresse) then
                begin
                  // Anzahl der abzufragenden Bytes hochrechnen, Funktionscode-
                  // abh�ngig:
                  case RegisterRequestData.FktCode of  // 28.06.2017, WW
                    1, 2:
                      begin
                        // Status-Request (Funktionen 01, 02):
                        // -> Die ben�tigten Bytes zur Abfrage der Stati von der
                        // Startadresse des Requests bis zur Startadresse des
                        // aktuellen Eintrags:
                        iCntByte := Calc_Modbus_AnzahlBytes_Status (
                          RegisterRequestData.StartAdresse -
                          TRegisterRequestDataObj(Self[iIx]).Data.StartAdresse + 1);
                        // -> Die Anzahl der Stati f�r den zusammengefa�ten Bereich:
                        iCntBit := RegisterRequestData.StartAdresse -
                          TRegisterRequestDataObj(Self[iIx]).Data.StartAdresse + 1;
                      end;

                    3, 4, 20:
                      begin
                        // Register-Request (Funktionen 03, 04, 20):
                        // -> Die Bytes von der Register-Startadresse des Requests
                        // bis zur Register-Startadresse des aktuellen Eintrags
                        // plus die Bytes f�r den aktuellen Eintrag:
                        iCntByte := Calc_Modbus_AnzahlBytes (RegisterRequestData.StartAdresse -
                          TRegisterRequestDataObj(Self[iIx]).Data.StartAdresse) +
                          RegisterRequestData.AnzahlBytes;

                        iCntBit := 1;  // Dummy, f�r Register-Request nicht ben�tigt
                      end;
                  else
                    iCntByte:=0;
                    iCntBit:=0;
                  end;

                  if (iCntByte > 0) AND (iCntBit > 0) then begin
                    // Maximal m�gliche Anzahl an Daten-Bytes f�r eine Modbus-Query,
                    // Funktionscode-abh�ngig:
                    case RegisterRequestData.FktCode of  // 28.06.2017, WW
                       1: MaxDataBytes:=Get_Modbus_MaxDataBytes_01;
                       2: MaxDataBytes:=Get_Modbus_MaxDataBytes_02;
                       3: MaxDataBytes:=Get_Modbus_MaxDataBytes_03;
                       4: MaxDataBytes:=Get_Modbus_MaxDataBytes_04;
                      20: MaxDataBytes:=Get_Modbus_MaxDataBytes_20;  // 18.02.2021, WW
                    else
                      MaxDataBytes:=0;
                    end;

                    // Wenn die maximal abfragbare Anzahl Bytes damit nicht �berschritten ist:
                    if (iCntByte <= MaxDataBytes) then begin
                      { F�r Register-Requests: Anzahl der Bytes in der Register-Requestliste aktualisieren }
                      if (TRegisterRequestDataObj(Self[iIx]).Data.AnzahlBytes < iCntByte) then
                        TRegisterRequestDataObj(Self[iIx]).Data.AnzahlBytes := iCntByte;

                      { F�r Status-Requests: Anzahl der Bits in der Register-Requestliste aktualisieren }
                      if (TRegisterRequestDataObj(Self[iIx]).Data.AnzahlBits < iCntBit) then
                        TRegisterRequestDataObj(Self[iIx]).Data.AnzahlBits := iCntBit;

                      { Listenobjekt f�r Register-Konvertierungsdaten in Liste einf�gen: }
                      TRegisterRequestDataObj(Self[iIx]).Data.RegisterKonvListe.
                        Add(RegisterKonvDataObj);

                      bAdd:=false;  // Anzahl der Bytes wurde erh�ht, kein neuer Eintrag in Registerliste
                    end;
                  end;  // if (iCntByte > 0) AND (iCntBit > 0)
                end;
              end else
                bAdd := False; // ... meckere, dass ein Hampel die Adresse 2x vergeben hat !
            end;
          end;
        end;  // if not bWrite

        if bAdd then begin  // neuer Eintrag in Register-Requestliste
          if bWrite then
            iIx := Self.Count  // am Ende anh�ngen
          else begin
            // Eintr�ge nach Funktionscode, Startadresse sortiert eingeben
            iIx := 0;
            for i := 0 to Self.Count-1 do begin
              iIx := i;  // Index zum Eintragen in RRL-Liste
              if (TRegisterRequestDataObj(Self[i]).Data.FktCode = RegisterRequestData.FktCode) AND
                 (TRegisterRequestDataObj(Self[i]).Data.StartAdresse >= RegisterRequestData.StartAdresse) then
                Break
              else
                Inc(iIx);
            end;
          end;

          { Register-Konvertierungsliste createn: }
          RegisterRequestData.RegisterKonvListe:=TRegisterKonvList.Create;
          { Listenobjekt f�r Register-Konvertierungsdaten in Liste einf�gen: }
          RegisterRequestData.RegisterKonvListe.Add (RegisterKonvDataObj);

          { Listenobjekt f�r Registerdaten createn und in Liste einf�gen: }
          RegisterRequestDataObj:=TRegisterRequestDataObj.Create;
          RegisterRequestDataObj.SetData (RegisterRequestData);
          Self.Insert(iIx, RegisterRequestDataObj);
        end;
      except
        Result:=false;
      end;
    end;  { for j:=0 to RDL.Count - 1 }
  end;
end;


{ TSlaveList }

{----------------------------}
destructor TSlaveList.Destroy;
{----------------------------}
begin
  Clear;

  inherited Destroy;
end;

{-------------------------}
procedure TSlaveList.Clear;
{-------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if Assigned (TSlaveDataObj (Items [i]).Data.RegisterRequestListe) then
      TSlaveDataObj (Items [i]).Data.RegisterRequestListe.Free;  { Register-Konvertierungsliste freigeben }

  inherited Clear;
end;

{-------------------------------------------}
procedure TSlaveList.Delete(iIndex: integer);
{-------------------------------------------}
begin
  if Assigned (TSlaveDataObj (Items [iIndex]).Data.RegisterRequestListe) then
    TSlaveDataObj (Items [iIndex]).Data.RegisterRequestListe.Free;  { Register-Konvertierungsliste freigeben }

  inherited Delete(iIndex);
end;


{------------------------------------------------------------------------------}

{-------------------------------------------------------------------}
function GetRegisterRequestListe (COMNr: integer; SlaveAdresse: byte;
  RRL: TRegisterRequestList; bWrite: boolean): boolean;
{-------------------------------------------------------------------}
{ liefert aus Modbus-Master-Register-Konfigurationsdatei die f�r �bergebene COM-Nummer
  und Slaveadresse definierten, abzufragenden Register als Liste;
  �bergaben: COM-Nummer
             Slave-Adresse
  �bergabe/R�ckgabe: Registerliste
  Ergebnis: true, wenn Inhalt der Register-Konfigurationsdatei g�ltige Daten enth�lt }
var
  TFS: TTextFileStream;
  FSize: integer;
  S: string;
  FieldStr: string;
  FieldCount: integer;
  FName_Register: string;  // Name der Register-Konfigurationsdatei
  ASlaveAdr: integer;
  RegisterDataList: TRegisterDataList;  // 28.06.2017, WW
  iRegFktCode: byte;
  sRegName: string;
  iRegStartAdresse: word;
  iRegAnzahlBytes: integer;
  sRegTyp: string;
  sRegWert_Einstellen: string;

begin
  Result:=true;  // OK
  if RRL = nil then exit;
  RRL.Clear;

  if bWrite then
    FName_Register:=ChangeFileExt (ParamStr(0), '_Register_Write.dat')  // 28.06.2017, WW
  else
    FName_Register:=ChangeFileExt (ParamStr(0), '_Register.dat');

  if not FileExists (FName_Register) then exit;

  RegisterDataList:=TRegisterDataList.Create;  // Zwischenliste f�r nach Startadresse sortierte Datei-Registerdaten
  try
    TFS:=TTextFileStream.Create (FName_Register, fmOpenRead OR fmShareDenyWrite);
    try
      FSize:=TFS.Size;
      while TFS.Position < FSize do begin
        Application.ProcessMessages;
        TFS.ReadLn (S);   // eine Zeile lesen
        FieldStr:=F_Zerlegen (S, CTrenner);  { Feld 1: COM-Nummer }
        if StrToIntDef (FieldStr, -1) = COMNr then begin  { Feld 1 enth�lt gesuchte COM-Nummer }
          FieldStr:=F_Zerlegen (S, CTrenner);  { Feld 2: Slave-Adresse }
          ASlaveAdr:=StrToIntDef (FieldStr, -1);
          if ASlaveAdr = SlaveAdresse then begin  { Feld 2 enth�lt gesuchte Slaveadresse }
            try
              // Register-Daten vorbelegen
              iRegFktCode:=3;  // abw�rtskomatibel, bisher wurde nur Funktion 3 unterst�tzt
              sRegName:='';
              iRegStartAdresse:=0;
              iRegAnzahlBytes:=-1;  // Anzahl unbekannt
              sRegTyp:='';   // Typ unbekannt
              sRegWert_Einstellen:='';  // Einstellwert unbekannt

              FieldCount:=2;
              while length (S) > 0 do begin
                FieldStr:=F_Zerlegen (S, CTrenner);
                inc (FieldCount);
                case FieldCount of
                  3: sRegName:=FieldStr;
                  4: iRegStartAdresse:=StrToInt (FieldStr);
                  5: sRegTyp:=FieldStr;
                  6: iRegAnzahlBytes:=StrToIntDef (FieldStr, -1);  // f�r Status-Requests nicht erforderlich
                  7: iRegFktCode:=StrToInt (FieldStr);  // 28.06.2017, WW
                  8: sRegWert_Einstellen:=FieldStr;  // 28.06.2017, WW
                end;  { case }
              end;  { while length (S) }

              // Eintrag der Registerdaten-Liste hinzuf�gen:
              if not RegisterDataList.AddRegisterData (iRegFktCode, iRegStartAdresse,
                        sRegName, sRegTyp, iRegAnzahlBytes, sRegWert_Einstellen) then
                Result:=false;
            except
              Result:=false;
            end;
          end;  { if SlaveAdresse .. }
        end;  { if COMNr ... }
      end;  { while TFS.Position < FSize }
    finally
      TFS.Free;
    end;

    // In Registerdaten-Liste enthaltene Einzel-Register zu geblockten Requests
    // zusammenfassen und in Register-Requestliste eintragen:
    if not RRL.LoadFromRegisterDataList (RegisterDataList, bWrite) then  // 08.03.2019, WW
      Result:=false;
  finally
    RegisterDataList.Free;
  end;
end;

{-------------------------------------------------------------}
function GetSlaveListeFromFile (COMNr: integer; SL: TSlaveList;
  bWrite: boolean): integer;
{-------------------------------------------------------------}
{ liefert f�r �bergebene COM-Nummer definierte Slave-Konfigurationsdaten als Liste;
  �bergabe: COM-Nummer
  �bergabe/R�ckgabe: Slaveliste
  Ergebnis:  0 = OK, Modbus-Master-Konfigurationsdateien enthalten g�ltige Daten
            -1 = Fehler, Inhalt der Slave-Konfigurationsdatei enth�lt ung�ltige Daten
            -2 = Fehler, Inhalt der Register-Konfigurationsdatei enth�lt ung�ltige Daten }
var
  TFS: TTextFileStream;
  FSize: integer;
  S: string;
  FieldStr: string;
  FieldCount: integer;
  SlaveData: TSlaveData;
  SlaveDataObj: TSlaveDataObj;
  FName_Slave: string;  // Name der Slave-Konfigurationsdatei
  dt: TDateTime;

begin
  Result:=0;  // OK
  if SL = nil then exit;

  if bWrite then
    FName_Slave:=ChangeFileExt (ParamStr(0), '_Slave_Write.dat')  // 28.06.2017, WW
  else
    FName_Slave:=ChangeFileExt (ParamStr(0), '_Slave.dat');

  if not FileExists (FName_Slave) then exit;
  TFS:=TTextFileStream.Create (FName_Slave, fmOpenRead OR fmShareDenyWrite);
  try
    FSize:=TFS.Size;
    while TFS.Position < FSize do begin
      Application.ProcessMessages;
      TFS.ReadLn (S);   // eine Zeile lesen
      FieldStr:=F_Zerlegen (S, CTrenner);  { Feld 1: COM-Nummer }
      if StrToIntDef (FieldStr, -1) = COMNr then begin  { Feld 1 enth�lt gesuchte COM-Nummer }
        try
          with SlaveData do begin
            // Record vorbelegen
            SlaveAdresse:=0;  // Slaveadresse unbekannt
            ByteOrder:=bo_BigEndian;  // Default
            PollingZyklus:=5000;  // Default
            RegisterRequestListe:=TRegisterRequestList.Create;  { Registerliste createn }
            NextCallTime:=0;  // Default
            Uhrzeit:=-1;  // Default

            FieldCount:=1;
            while length (S) > 0 do begin
              FieldStr:=F_Zerlegen (S, CTrenner);
              inc (FieldCount);
              case FieldCount of
                2: SlaveAdresse:=StrToInt (FieldStr);
                3: begin
                     case StrToInt (FieldStr) of
                       integer (bo_BigEndian):      ByteOrder:=bo_BigEndian;
                       integer (bo_LittleEndian):   ByteOrder:=bo_LittleEndian;
                       integer (bo_LittleByteSwap): ByteOrder:=bo_LittleByteSwap;
                     end;
                   end;
                4: begin
                     // Fester Zeitpunkt definiert ?  28.06.2017, WW
                     if (Pos (':', FieldStr) > 0) AND
                        (EncodeTimeStr (FieldStr, 'HH:MM:SS', dt)) then begin
                       Uhrzeit:=dt;
                       PollingZyklus:=-1;
                     end else  // Zyklus definiert
                       PollingZyklus:=StrToInt (FieldStr);
                   end;
              end;  { case }
            end;  { while length (S) }

            { COM-Nummer und Slaveadresse zugeordnete Modbus-Register f�r
              Read/Write in Liste laden: }
            if not GetRegisterRequestListe (COMNr, SlaveAdresse, RegisterRequestListe,
                                            bWrite) then
              Result:=-2;  // Register-Konfigurationsdaten ung�ltig

            { Listenobjekt createn und in Liste einf�gen: }
            SlaveDataObj:=TSlaveDataObj.Create;
            SlaveDataObj.SetData (SlaveData);

            SL.Add (SlaveDataObj);
          end;  { with RegisterData }
        except
          Result:=-1; // Slave-Konfigurationsdaten ung�ltig
        end;
      end;
    end;  { while TFS.Position < FSize }
  finally
    TFS.Free;
  end;
end;

{---------------------------------------------------------------}
function GetSlaveListe (COMNr: integer; SL: TSlaveList): integer;
{---------------------------------------------------------------}

begin
  Result:=0;  // OK
  if SL = nil then exit;
  SL.Clear;

  // Slave-Konfiguration lesen (Read)
  Result:=GetSlaveListeFromFile (COMNr, SL, false);
  if Result = 0 then // OK
    // Slave-Konfiguration lesen (Write)
    Result:=GetSlaveListeFromFile (COMNr, SL, true);
end;

end.

