{******************************************************************************}
{* Unit: Basis-Objekt für MRG-Abruf                                           *}
{* 03.12.2021 WW Neu; mit allgemeinen Modbus-Routinen                         *}
{* 03.01.2022 WW mit COM-Tracelog                                             *}
{* 11.02.2022 WW mit allgemeinen "statischen" Methoden                        *}
{* 09.01.2024 WW Anpassung Modbus-Routinen für RSM200                         *}
{******************************************************************************}
Unit AbrufObjMrgCustom;

INTERFACE

uses
  Classes, SysUtils,
  WStream, O_Comm, SerMRGModem, ErrPrc32, ErrConst, MrgBefehl, AbrufObj, T_Zeit,
  WComm, WSysCon, ModbusMasterRes, ModbusMasterUtil, ModbusUtil, LogFile,
  O_ResFilesList, MResMrg, WStrUtils, T_BinMask;

Type
  { Basis-Objekt zum MRG-Datenabruf }

  TMRGCustomAbruf = class (TAbruf)
  private
    FModbusTID: word;  // Transaktionsnummer für Modbus TCP/IP
    procedure WriteModbusErrLog (sErr: string; aCOMNr: integer);
  protected
    CommObj: TMRGCommObj;
    // Allgemeine "statische" Methoden
    function Init_AbrufRec: TAbrufRec;
    function CheckGeraeteTyp (iGeraeteTyp_Soll: integer;
      sGeraeteTyp_Ist: string): boolean;
    function PasswortNrToDevicePasswortID (iMrgTyp, iPasswortNr: integer): integer;
    // Allgemeine Modbus-Routinen
    procedure SetModbusSlaveData_AdresseByteOrder (AMrgTyp: integer;
      var SlaveData: TSlaveData);
    function SendModbusRequest (SlaveData: TSlaveData;
      RegisterRequestData: TRegisterRequestData; ATimeout: integer;
      var bPreset: boolean; var sValue: string;
      iErweiterteStatusgruppe: integer = 0): boolean;
    function GetModbusArchivRec_DatumZeit (AMrgTyp: integer; AArchivtyp: TArchivtyp;
      iRecOffset: word; RegisterRequestListe: TRegisterRequestList;
      MBAbrufData: TMBAbrufData): TDateTime;
    function WriteModbusArchivRecFile (AMrgTyp: integer; AArchivtyp: TArchivtyp;
      AbrufRec: TAbrufRec; dtVon, dtBis: TDateTime; sFileName: string;
      RegisterRequestListe: TRegisterRequestList; MBAbrufData: TMBAbrufData): boolean;
  public
    Constructor Create (ACOMNr: integer; ACOMNr_Kommando: integer;
      ACommObj: TMRGCommObj;
      AModemname: string;
      ADebugCOMProtokoll: boolean;
      ARohdatenLoeschen: boolean;
      AKonfigPath, AWorkPath, ANetProgPath, ALogPath: string;
      ASignatur_freigeschaltet: boolean;
      AXMLResponseEncodeRohdaten: integer;
      AResourceFilesList: TResourceFilesList);
    procedure CreateCOMTraceLog; override;
    procedure FreeCOMTraceLog; override;
  end;

IMPLEMENTATION

{ TMRGCustomAbruf }

{----------------------------------------------------------------------------}
Constructor TMRGCustomAbruf.Create (ACOMNr: integer; ACOMNr_Kommando: integer;
  ACommObj: TMRGCommObj;
  AModemname: string;
  ADebugCOMProtokoll: boolean;
  ARohdatenLoeschen: boolean;
  AKonfigPath, AWorkPath, ANetProgPath, ALogPath: string;
  ASignatur_freigeschaltet: boolean;
  AXMLResponseEncodeRohdaten: integer;
  AResourceFilesList: TResourceFilesList);
{----------------------------------------------------------------------------}
Begin
  inherited Create (ACOMNr, ACOMNr_Kommando, AModemname,
                    ADebugCOMProtokoll, ARohdatenLoeschen,
                    AKonfigPath, AWorkPath, ANetProgPath, ALogPath, '',
                    ASignatur_freigeschaltet, AXMLResponseEncodeRohdaten,
                    AResourceFilesList, nil);
  CommObj:=ACommObj;

  // Transaktionsnummer für Modbus TCP/IP initialisieren; 16.12.2019 WW
  FModbusTID:=InitTID_ModbusTCPIP;
End;


{------------------ Allgemeine "statische" Methoden ---------------------------}

{------------------------------------------------}
function TMRGCustomAbruf.Init_AbrufRec: TAbrufRec;
{------------------------------------------------}
{ Ergebnis: vorbelegter Abruf-Record }
begin
  with Result do begin
    KanalMaske:='';
    AlleDaten:=false;
    vonJahr:=0;
    vonMonat:=0;
    vonTag:=0;
    vonStunde:=0;
    vonMinute:=0;
    vonSekunde:=0;
    bisJahr:=0;
    bisMonat:=0;
    bisTag:=0;
    bisStunde:=0;
    bisMinute:=0;
    bisSekunde:=0;
  end;
end;

{------------------------------------------------------------------}
function TMRGCustomAbruf.CheckGeraeteTyp (iGeraeteTyp_Soll: integer;
  sGeraeteTyp_Ist: string): boolean;
{------------------------------------------------------------------}
{ Führt Soll/Ist-Vergleich für Gerätetyp durch;
  Übergabe: Soll-Gerätetyp (MRG-Typnummer)
            Ist-Gerätetyp-String
  Ergebnis: true, wenn Soll- und Ist-Gerätetyp übereinstimmt oder Gerätetyp nicht
            geprüft wird }
var
  S: string;

begin
  Result:=true;
  if iGeraeteTyp_Soll > -1 then begin  // nur gültige Gerätetyp-Nummern
    S:=StrFilter (sGeraeteTyp_Ist, ' ');
    case iGeraeteTyp_Soll of
      { RMG: }
      mrgtyp_MRG800PTB:
        if Pos ('800', S) <> 1 then Result:=false;

      mrgtyp_MRG905:
        if Pos ('905', S) <> 1 then Result:=false;

      mrgtyp_MRG910:
        if Pos ('910', S) <> 1 then Result:=false;

      mrgtyp_EC900:
        if Pos ('EC900', S) <> 1 then Result:=false;

      { Elster: }
      mrgtyp_DL210:
        if Pos ('DL210', S) = 0 then Result:=false;

      mrgtyp_DL220:
        if Pos ('DL220', S) = 0 then Result:=false;

      mrgtyp_DL230:
        if Pos ('DL230', S) = 0 then Result:=false;  // Meldet sich mit \2DL230

      mrgtyp_DL240:
        if Pos ('DL240', S) = 0 then Result:=false;  // DL230 im Komp.modus meldet sich mit \2DL240

      mrgtyp_EK260:
        if Pos ('EK260', S) = 0 then Result:=false;  // EK280 im Komp.modus meldet sich mit \2EK260

      mrgtyp_EK280:
        if Pos ('EK280', S) = 0 then Result:=false;  // Meldet sich mit \2EK280

      { Actaris: }
      mrgtyp_Sparklog:
        if Pos ('\@DL4', S) <> 1 then Result:=false;

      mrgtyp_Corus:
        if Pos ('MINICOR', S) <> 1 then Result:=false;

      { Kamstrup: }
      mrgtyp_Unigas300:
        if Pos ('UG', S) <> 1 then Result:=false;

      { Tritschler: }
      mrgtyp_MCO:
        if Pos ('MCO', S) <> 1 then Result:=false;

      mrgtyp_TDS:
        if Pos ('TDS', S) <> 1 then Result:=false;

      mrgtyp_VC2:
        if Pos ('VC2', S) <> 1 then Result:=false;

      mrgtyp_VC3_VC2komp:
        if Pos ('VC3', S) <> 1 then Result:=false;

      mrgtyp_VC3:
        if Pos ('VC3', S) <> 1 then Result:=false;

      mrgtyp_VCC:
        if Pos ('VCC', S) <> 1 then Result:=false;  // 11.01.2016, WW

      mrgtyp_MC2:
        if Pos ('MC2', S) <> 1 then Result:=false;  // 28.06.2018, WW

    end;  { case iGeraeteTyp_Soll }
  end;  { if iGeraeteTyp_Soll > -1 }
end;

{-----------------------------------------------------}
function TMRGCustomAbruf.PasswortNrToDevicePasswortID (
  iMrgTyp, iPasswortNr: integer): integer;
{-----------------------------------------------------}
{ Liefert zu einer Passwort-Nummer die geräteabhängige Passwort-ID (Primus/Prilog)
  bzw. User-ID (FLOWSIC500);
  Übergaben: Gerätetyp
             Passwort-Nummer
  Ergebnis: Passwort/User-ID des Geräts }
begin
  case iMrgTyp of
    mrgtyp_Primus,
    mrgtyp_Prilog:  // 25.11.2020, WW
      begin
        case iPasswortNr of
          1: Result:=802;  // Erstes "Nicht-Admin-Passwort" aus Passwort-Gruppe "Administratoren"
          2: Result:=822;  // Erstes "Nicht-Admin-Passwort" aus Passwort-Gruppe "Benutzer 1"
          3: Result:=832;  // Erstes "Nicht-Admin-Passwort" aus Passwort-Gruppe "Benutzer 2"
          4: Result:=842;  // Erstes "Nicht-Admin-Passwort" aus Passwort-Gruppe "Benutzer 3"
        else
          Result:=802;  // wie Passwort-Nummer 1
        end;
      end;

    mrgtyp_FLOWSIC500:  // 29.09.2021, WW
      begin
        case iPasswortNr of
          1: Result:=6;  // Autorisierter Nutzer 1 (Admin)
          2: Result:=5;  // Autorisierter Nutzer 2
          3: Result:=3;  // Nutzer 1
          4: Result:=0;  // Gast
        else
          Result:=6;  // wie Passwort-Nummer 1
        end;
      end;
  else
    Result:=0;  // Default
  end;
end;


{------------------------ Allgemeine Modbus-Routinen --------------------------}

{------------------------------------------------------------------------------}
procedure TMRGCustomAbruf.SetModbusSlaveData_AdresseByteOrder (AMrgTyp: integer;
  var SlaveData: TSlaveData);
{------------------------------------------------------------------------------}
{ Setzt gerätetypabhängig die Modbus Slave-Adresse und Byte-Order im SlaveData-
  Record;
  Übergabe: Gerätetyp
  Rückgabe: SlaveData-Record }
begin
  case AMrgTyp of
    mrgtyp_Primus,
    mrgtyp_Prilog:  // Primus/Prilog 400
    begin
      SlaveData.SlaveAdresse:=1;  // geändert auf 1 (bisher 248 im reservierten Bereich); 04.10.2022, WW
      SlaveData.ByteOrder:=bo_BigEndian;
    end;

    mrgtyp_TME400_VCF,
    mrgtyp_TME400_VMF:  // TME400
    begin  // 18.02.2021, WW
      SlaveData.SlaveAdresse:=1;
      SlaveData.ByteOrder:=bo_BigEndian;
    end;

    mrgtyp_FLOWSIC500:  // FLOWSIC500
    begin  // 29.09.2021, WW
      SlaveData.SlaveAdresse:=1;
      SlaveData.ByteOrder:=bo_BigEndian;
    end;

    mrgtyp_RSM200_VCF,
    mrgtyp_RSM200_VMF:  // RSM200
    begin  // 09.01.2024, WW
      SlaveData.SlaveAdresse:=1;
      SlaveData.ByteOrder:=bo_BigEndian;
    end;
  else
    SlaveData.SlaveAdresse:=0;
    SlaveData.ByteOrder:=bo_BigEndian;
  end;
end;

{----------------------------------------------------------------}
function TMRGCustomAbruf.SendModbusRequest (SlaveData: TSlaveData;
  RegisterRequestData: TRegisterRequestData; ATimeout: integer;
  var bPreset: boolean; var sValue: string;
  iErweiterteStatusgruppe: integer = 0): boolean;
{----------------------------------------------------------------}
{ Modbus-Request an Slave senden, Response empfangen und konvertieren;
  Übergaben: Slave-Daten
             Timeout beim Warten auf Antwort
             Erweiterte Statusgruppe (optional, EST_...-Konstanten)
  Übergabe/Rückgabe: Register-Requestdaten (mit Liste der konvertierten Registerwerte)
  Rückgabe: Flag 'bPreset': true, wenn Einstellen erfolgreich (nur bei Einstell-
              Request, sonst false)
            Gelesener Wert (nur, wenn ein einzelner Wert gelesen wird, sonst leer)
  Ergebnis: true, wenn Senden des Requests erfolgreich }
var
  sQuery: string;
  iStartAdresse: word;
  R: TRueckgabe;
  AFehlergruppe: integer;
  AFehlercode: integer;
  sErr: string;
  sRequestBinData: string;
  sInfoWert_Aktuell: string;
  iNoOfPoints: word;
  ModbusModus: TModbusModus;

begin
  Result:=false;
  // Vorbelegungen Rückgabe
  bPreset:=false;
  sValue:='';

  ModbusModus:=TMRGCustomCommObj (CommObj).ModbusModus;  // 16.12.2019, WW

  // Indizierte Startadresse, bei 0 beginnend:
  // -> nicht bei Funktion 20; 18.02.2021, WW
  if RegisterRequestData.FktCode in [20] then
    iStartAdresse:=RegisterRequestData.StartAdresse
  else
    iStartAdresse:=Get_Modbus_StartAdresse_Index0 (RegisterRequestData.StartAdresse);

  // Anzahl Register oder Stati ("No. of Points"):
  iNoOfPoints:=Get_Modbus_NoOfPoints(RegisterRequestData);

  sErr:='';
  // Query bilden:
  // 16.12.2019, WW: mit TID
  if not Build_Modbus_Query(ModbusModus, SlaveData, RegisterRequestData,
                            iStartAdresse, iNoOfPoints, sQuery,
                            sRequestBinData, sInfoWert_Aktuell, sErr, FModbusTID) then begin
    FehlerGruppeCodeUpdate (iErweiterteStatusgruppe + COM_MODBUSERROR, MODBUSERR_QUERY_CREATE);
    WriteModbusErrLog (sErr, FCOMNr);
    exit;
  end;

  // Query versenden, Antwort empfangen:
  if not TMRGCustomCommObj (CommObj).SendModbusQuery (sQuery, ATimeout,
                                                      R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (iErweiterteStatusgruppe + R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  // Response auf Gültigkeit prüfen:
  // 16.12.2019, WW: mit Soll-TID
  if not Valid_Modbus_Response (SlaveData.SlaveAdresse, RegisterRequestData.FktCode,
                                FModbusTID, ModbusModus, R.Antwort,
                                AFehlergruppe, AFehlercode) then begin
    FehlerGruppeCodeUpdate (iErweiterteStatusgruppe + AFehlergruppe, AFehlercode);
    sErr:=Format ('Invalid modbus response (function %.2d): ',
                  [RegisterRequestData.FktCode]) +
          GetErrorText (AFehlergruppe, AFehlercode);
    WriteModbusErrLog (sErr, FCOMNr);
    exit;
  end;

  // Response auswerten und konvertieren:
  if not Konv_Modbus_Response (ModbusModus, SlaveData, RegisterRequestData,
                               iStartAdresse, iNoOfPoints,
                               R.Antwort, sRequestBinData, bPreset, sValue,
                               AFehlergruppe, AFehlercode, sErr) then begin
    FehlerGruppeCodeUpdate (iErweiterteStatusgruppe + AFehlergruppe, AFehlercode);
    sErr:=sErr + GetErrorText (AFehlergruppe, AFehlercode);
    WriteModbusErrLog (sErr, FCOMNr);
    exit;
  end;

  Result:=true;
end;

{----------------------------------------------------------------------------------}
function TMRGCustomAbruf.GetModbusArchivRec_DatumZeit (AMrgTyp: integer;
  AArchivtyp: TArchivtyp; iRecOffset: word;
  RegisterRequestListe: TRegisterRequestList; MBAbrufData: TMBAbrufData): TDateTime;
{----------------------------------------------------------------------------------}
{ Liefert Zeitstempel eines Modbus-Archivdatensatzes (Archivdatensatz, bei dem
  jedem Datensatz-Feld eine eigene Modbus-Startregisteradresse zugeordnet ist);
  Übergaben: Gerätetyp
             Archivtyp
             Datensatz-Offset (0 = jüngster Datensatz, 1 = 2.-jüngster usw.)
             Modbus-Register-Requestliste
             MBAbrufData-Record mit Modbusregister-Konfiguration des Archivs
  Ergebnis: Zeitstempel (-1, wenn nicht vorhanden) }
var
  i, j: integer;
  RegisterKonvListe: TRegisterKonvList;
  RegisterKonvData: TRegisterKonvData;
  iStartAdresse: word;

begin
  Result:=-1;  // Keine Datum/Zeit-Information vorhanden

  if Assigned (RegisterRequestListe) then begin
    // Startadresse des Registers ermitteln, welches die Datum/Zeit-Information
    // des Archivdatensatzes trägt:
    iStartAdresse:=GetStartAdresse_MBRegister_Archiv (AMrgTyp, AArchivtyp,
                                                      C_MBKanalDef_DZ, iRecOffset,
                                                      MBAbrufData);

    if iStartAdresse > 0 then begin  // Startadresse konnte ermittelt werden
      // Die Listen rückwärts durchlesen. Bringt evtl. einen kleinen Performance-Vorteil,
      // bei häufigem Suchen des Zeitstempels des letzten Datensatzes (spielt letztlich
      // aber keine Rolle); 22.07.2019, WW
      for i:=RegisterRequestListe.Count - 1 downto 0 do begin
        { Register-Konvliste enthält die abgerufenen Modbus-Daten: }
        RegisterKonvListe:=
          TRegisterRequestDataObj (RegisterRequestListe [i]).Data.RegisterKonvListe;

        for j:=RegisterKonvListe.Count - 1 downto 0 do begin
          RegisterKonvData:=TRegisterKonvDataObj (RegisterKonvListe [j]).Data;

          if (RegisterKonvData.StartAdresse = iStartAdresse) then begin
            try
              Result:=StrToDateTime (RegisterKonvData.Wert);
              { Anm.: Die Kodierung in der Modbus-Konvertierung erfolgt mittels DateTimeToStr }
            except
              //
            end;
            exit;  // Zeitstempel gefunden, raus
          end;
        end;  // for j
      end;  // for i
    end;  // if iStartAdresse > 0
  end;
end;

{---------------------------------------------------------------------}
function TMRGCustomAbruf.WriteModbusArchivRecFile (AMrgTyp: integer;
  AArchivtyp: TArchivtyp; AbrufRec: TAbrufRec; dtVon, dtBis: TDateTime;
  sFileName: string; RegisterRequestListe: TRegisterRequestList;
  MBAbrufData: TMBAbrufData): boolean;
{---------------------------------------------------------------------}
{ Schreibt die in der Modbus-Register-Requestliste enthaltenen
  Konvertierungslisten-Werte als Archivdatensätze in eine bestehende Datei;
  Übergaben: Gerätetyp
             Archivtyp (für Messwerte, Meldungen)
             Abruf-Record
             Daten-Zeitbereich von-bis
             Dateiname
             Register-Requestliste
             MBAbrufData-Record mit Modbusregister-Konfiguration des Archivs
  Ergebnis: true, wenn Schreiben in Datei erfolgreich } 

  {---------------------------------------------------------}
  function DoSaveArchivRec (dtArchivRec: TDateTime): boolean;
  {---------------------------------------------------------}
  { Prüfen, ob Modbus-Archivdatensatz gespeichert werden soll;
    Ergebnis: true, wenn gespeichert werden soll }
  begin
    // Archivdatensatz speichern ? Wenn alle Daten gelesen werden sollen
    // oder der Zeitstempel im Abrufzeitraum liegt !
    // 22.07.2019, WW: Nur speichern, wenn der Zeitstempel belegt ist (unbelegte
    //                 Archivdatensätze tragen den Zeitstempel 0 !)
    Result:=(dtArchivRec > 0) AND   // 22.07.2019, WW
            (AbrufRec.AlleDaten OR
             ((CmpDateTime (dtArchivRec, dtVon) >= 0) AND
              (CmpDateTime (dtArchivRec, dtBis) <= 0)));
  end;

var
  i, j: integer;
  iCount: integer;
  RegisterKonvListe: TRegisterKonvList;
  RegisterKonvData: TRegisterKonvData;
  RegisterKonvDataArchivRec: TRegisterKonvDataArchivRec;
  RegisterKonvDataArchivRecObj: TRegisterKonvDataArchivRecObj;
  RegisterKonvDataArchivRecList: TRegisterKonvDataArchivRecList;
  iStartAdresse: word;
  iStartAdresse_First: word;
  bStartAdresse_First: boolean;
  iArchivRecSize: word;
  dtArchivRec: TDateTime;
  sMB_KanalDef: string;
  FS: TFileOfRecStream;
  bSave: boolean;
  iKanalNr: integer;
  bReverse: boolean;

begin
  Result:=true;

  // Register-Größe des Modbus-Archivdatensatzes
  iArchivRecSize:=GetMB_ArchivRecSize (AMrgTyp, AArchivtyp, MBAbrufData);

  if Assigned (RegisterRequestListe) AND (iArchivRecSize > 0) then begin
    // Liste zum Zwischenspeichern der Register-Konvertierungsdaten der Modbus-
    // Archivdatensätze:
    RegisterKonvDataArchivRecList:=TRegisterKonvDataArchivRecList.Create;
    try
      // Vorbelegung: Register-Startadresse des ersten Werts noch nicht gemerkt
      iStartAdresse_First:=0;
      bStartAdresse_First:=false;

      // Vorbelegung Modbus-Archivdatensatz mit 'fehlend'
      RegisterKonvDataArchivRec:=Init_RegisterKonvDataArchivRec;  // 18.02.2021, WW
      // Vorbelegung Archivdatensatz-Zeitstempel fehlend
      dtArchivRec:=-1;
      iCount:=0;

      // Modbus-Archivdatensatz mit den abgerufenen Modbus-Daten belegen
      for i:=0 to RegisterRequestListe.Count - 1 do begin
        { Register-Konvliste enthält die abgerufenen Modbus-Daten: }
        RegisterKonvListe:=
          TRegisterRequestDataObj (RegisterRequestListe [i]).Data.RegisterKonvListe;

        for j:=0 to RegisterKonvListe.Count - 1 do begin
          RegisterKonvData:=TRegisterKonvDataObj (RegisterKonvListe [j]).Data;

          // Registeradresse des ersten Werts des ersten Archivdatensatzes merken
          if not bStartAdresse_First then begin  // 18.02.2021, WW
            iStartAdresse_First:=RegisterKonvData.StartAdresse;
            bStartAdresse_First:=true;
          end;

          // Prüfen, ob der Wert zum nächsten Archivdatensatz gehört
          if RegisterKonvData.StartAdresse >= (iStartAdresse_First + iArchivRecSize) then begin
            // Wert gehört zum nächsten Archivdatensatz

            if DoSaveArchivRec (dtArchivRec) then begin  // Prüfen, ob gespeichert werden soll
              // Aktuellen Modbus-Archivdatensatz in Zwischenliste eintragen
              RegisterKonvDataArchivRecObj:=TRegisterKonvDataArchivRecObj.Create;
              RegisterKonvDataArchivRecObj.SetData (RegisterKonvDataArchivRec);
              RegisterKonvDataArchivRecList.Add (RegisterKonvDataArchivRecObj);
            end
            else begin
              if not (dtArchivRec > -1) then begin
                // Archivdatensatz-Zeitstempel nicht vorhanden
                case AArchivtyp of
                  at_Periodenarchiv:
                    FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_MESSKONV);

                  at_Ereignisarchiv,
                  at_Parameterarchiv_eichamtlich,
                  at_Parameterarchiv_nichteichamtlich:
                    FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_MELDKONV);
                end;
                Result:=false;
                exit;
              end;
            end;

            // Vorbelegungen für den nächsten Modbus-Archivdatensatz:
            iStartAdresse_First:=RegisterKonvData.StartAdresse;  // Register-Startadresse des ersten Werts
            // Modbus-Archivdatensatz 'fehlend':
            RegisterKonvDataArchivRec:=Init_RegisterKonvDataArchivRec;
            // Archivdatensatz-Zeitstempel fehlend
            dtArchivRec:=-1;
            iCount:=0;
          end;

          inc (iCount);

          if (iCount >= Low (RegisterKonvDataArchivRec)) AND
             (iCount <= High (RegisterKonvDataArchivRec)) then begin
            // Kanal-Definition des Werts:
            case AMrgTyp of
              mrgtyp_TME400_VCF,
              mrgtyp_TME400_VMF,  // 18.02.2021, WW
              mrgtyp_RSM200_VCF,
              mrgtyp_RSM200_VMF:  // 09.01.2024, WW
                iStartAdresse:=RegisterKonvData.StartAdresse - iStartAdresse_First;
            else
              iStartAdresse:=RegisterKonvData.StartAdresse;
            end;

            sMB_KanalDef:=GetKanalDef_MBRegister_Archiv (AMrgTyp, AArchivtyp,
                                                         iStartAdresse, MBAbrufData);
            bSave:=true;  // Default: Wert speichern
            // Kanalmaske bei Messwerten auswerten:
            if (AArchivtyp = at_Periodenarchiv) AND (AbrufRec.KanalMaske <> '') then begin
              iKanalNr:=StrToIntDef (sMB_KanalDef, 0);
              if (iKanalNr > 0) then begin  // nur Kanal-Werte
                bSave:=false;
                if length (AbrufRec.KanalMaske) >= iKanalNr then begin
                  if AbrufRec.KanalMaske [iKanalNr] = '1' then
                    bSave:=true;
                end;
              end;
            end;

            if bSave then begin
              RegisterKonvData.StartAdresse:=iStartAdresse;  // 18.02.2021, WW
              RegisterKonvDataArchivRec [iCount]:=RegisterKonvData;
            end;

            // Wenn der Wert der Zeitstempel des Archivdatensatzes ist, merken:
            if sMB_KanalDef = C_MBKanalDef_DZ then begin
              try
                dtArchivRec:=StrToDateTime (RegisterKonvData.Wert);
                { Anm.: Die Kodierung in der Modbus-Konvertierung erfolgt mittels DateTimeToStr }
              except
                //
              end;
            end;
          end;
        end;  // for j
      end;  // for i

      // Letzten Modbus-Archivdatensatz in Zwischenliste eintragen:
      if iCount > 0 then begin
        if DoSaveArchivRec (dtArchivRec) then begin  // Prüfen, ob gespeichert werden soll
          RegisterKonvDataArchivRecObj:=TRegisterKonvDataArchivRecObj.Create;
          RegisterKonvDataArchivRecObj.SetData (RegisterKonvDataArchivRec);
          RegisterKonvDataArchivRecList.Add (RegisterKonvDataArchivRecObj);
        end
        else begin
          if not (dtArchivRec > -1) then begin
            // Archivdatensatz-Zeitstempel nicht vorhanden
            case AArchivtyp of
              at_Periodenarchiv:
                FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_MESSKONV);

              at_Ereignisarchiv,
              at_Parameterarchiv_eichamtlich,
              at_Parameterarchiv_nichteichamtlich:
                FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_MELDKONV);
            end;
            Result:=false;
            exit;
          end;
        end;
      end;

      // In Liste zwischengespeicherte Modbus-Archivdatensätze in Datei schreiben:
      if FileExists (sFileName) then begin  // Datei muß existieren
        FS:=TFileOfRecStream.Create (sFileName, fmOpenReadWrite OR fmShareDenyWrite,
                                     SizeOf (TRegisterKonvDataArchivRec));
        try
          FS.SeekRec (0, soFromEnd);  // ans Dateiende positionieren

          // Zwischenliste rückwärts lesen ?
          bReverse:=AMrgTyp in [mrgtyp_TME400_VCF, mrgtyp_TME400_VMF,
                                mrgtyp_RSM200_VCF, mrgtyp_RSM200_VMF];  // 09.01.2024, WW

          if bReverse then begin
            for i:=RegisterKonvDataArchivRecList.Count - 1 downto 0 do begin
              RegisterKonvDataArchivRec:=
                TRegisterKonvDataArchivRecObj (RegisterKonvDataArchivRecList [i]).Data;
              FS.WriteRec (RegisterKonvDataArchivRec);
            end;
          end
          else begin
            for i:=0 to RegisterKonvDataArchivRecList.Count - 1 do begin
              RegisterKonvDataArchivRec:=
                TRegisterKonvDataArchivRecObj (RegisterKonvDataArchivRecList [i]).Data;
              FS.WriteRec (RegisterKonvDataArchivRec);
            end;
          end;
        finally
          FS.Free;
        end;
      end
      else begin  // Datei existiert nicht
        // Archivdatensätze konnten nicht in Datei geschrieben werden
        FehlerGruppeCodeUpdate(ST_FILEERROR, FILEERR_COULDNOTWRITE);
        Result:=false;
      end;
    finally
      RegisterKonvDataArchivRecList.Free;
    end;
  end;  // if Assigned (RegisterRequestListe) AND (iArchivRecSize > 0) then
end;

{--------------------------------------------------------------------------}
procedure TMRGCustomAbruf.WriteModbusErrLog (sErr: string; aCOMNr: integer);
{--------------------------------------------------------------------------}
{ Modbus-Error-Logfile schreiben;
  Übergaben: Log-Text
             COM-Nummer }
const
  CLogfileName_ModbusErr = 'WICOMSRV_ModbusErr';  // Filename ohne Extension

var
  S: string;

begin
  if DebugCOMProtokoll then begin  // nur bei aktiviertem COM-Protokoll
    S:=sErr;
    if aCOMNr > 0 then  { COM-Port }
      S:=S + ' (COM' + IntToStr (aCOMNr) + ')'
    else  { IP-Abrufnummer }
      S:=S + ' (IP' + IntToStr (Abs (aCOMNr)) + ')';
    WriteDebugLog (FLogPath, CLogfileName_ModbusErr, S, true, lt_Error);
  end;
end;


{------------------------ COM-Tracelog ----------------------------------------}

{------------------------------------------}
procedure TMRGCustomAbruf.CreateCOMTraceLog;
{------------------------------------------}
// COM-Tracelog für MRG-Abruf createn
begin
  inherited CreateCOMTraceLog;

  if Assigned (CommObj) then
    CommObj.ComTraceLog:=FComTraceLog;
end;

{----------------------------------------}
procedure TMRGCustomAbruf.FreeCOMTraceLog;
{----------------------------------------}
// COM-Tracelog für MRG-Abruf freigeben
begin
  inherited FreeCOMTraceLog;

  if Assigned (CommObj) then
    CommObj.ComTraceLog:=FComTraceLog;
end;

end.


