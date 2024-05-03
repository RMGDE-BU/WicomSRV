{******************************************************************************}
{* Unit: Hilfsroutinen f�r Modbus-Master                                      *}
{* 08.03.2019  WW  Neu                                                        *}
{* 22.07.2019  WW  Funktionscode 05 (Force single coil)                       *}
{* 16.12.2019  WW  Build_Modbus_Query mit Modbus-TCPIP-Transaktionsnummer     *}
{* 18.02.2021  WW  Funktionscode 20 (Read General Reference)                  *}
{* 29.09.2021  WW  Konv_Modbus_Response erweitert f�r Funktionscode 3 mit     *}
{*                 Einzelwert                                                 *}
{******************************************************************************}
unit ModbusMasterUtil;

interface

uses
  Windows, Classes, SysUtils, ErrConst, ModbusUtil, ModbusMasterRes;

function Get_Modbus_NoOfPoints (RegisterRequestData: TRegisterRequestData): word;
function Build_Modbus_Query (ModbusModus: TModbusModus;
  SlaveData: TSlaveData; RegisterRequestData: TRegisterRequestData;
  iStartAdresse_Index0: word; iNoOfPoints: word;
  var sQuery: string; var sRequestBinData: string; var sInfoWert_Aktuell: string;
  var sErr: string; var iTID: word): boolean;
function Konv_Modbus_Response (ModbusModus: TModbusModus;
  SlaveData: TSlaveData; RegisterRequestData: TRegisterRequestData;
  iStartAdresse_Index0: word; iNoOfPoints: word;
  sResponse: string; sRequestBinData: string;
  var bPreset: boolean; var sValue: string;
  var AFehlergruppe: integer; var AFehlercode: integer;
  var sErr: string): boolean;

implementation

{------------------------------------------------------------------------------}
function Get_Modbus_NoOfPoints (RegisterRequestData: TRegisterRequestData): word;
{------------------------------------------------------------------------------}
{ Liefert Anzahl f�r Register oder Stati ("No. of Points");
  �bergabe: Register-Requestdaten
  Ergebnis: "No. of Points" }
begin
  with RegisterRequestData do begin
    case RegisterRequestData.FktCode of
      1, 2, 5:
        begin
          // Anzahl der Stati (Bits):
          Result:=RegisterRequestData.AnzahlBits;
        end;

      3, 4, 6, 16, 20:
        begin
          // aus der Anzahl der Bytes die ben�tigte Anzahl der Register berechnen:
          Result:=Calc_Modbus_AnzahlRegister (RegisterRequestData.AnzahlBytes);
        end;
    else
      Result:=0;
    end;
  end;
end;

{------------------------------------------------------------------------------}
function Build_Modbus_Query (ModbusModus: TModbusModus;
  SlaveData: TSlaveData; RegisterRequestData: TRegisterRequestData;
  iStartAdresse_Index0: word; iNoOfPoints: word;
  var sQuery: string; var sRequestBinData: string; var sInfoWert_Aktuell: string;
  var sErr: string; var iTID: word): boolean;
{------------------------------------------------------------------------------}
{ Bildet Modbus-Query;
  �bergaben: Modbus-Modus
             Slave-Daten
             Register-Requestdaten
             Startadresse (bei 0 beginnend)
             No. of Points (Anzahl Register oder Stati)
  R�ckgaben: Query
             Wert in Bin�rdaten-Format (bei Einstell-Query)
             Aktueller Wert zur Info (bei Einstell-Query)
             Fehler-Text (leer, wenn Query erfolgreich gebildet werden konnte)
  �bergabe/R�ckgabe: Transaktionsnummer (Modbus TCP)
  Ergebnis: true, wenn Query erfolgreich gebildet werden konnte }
var
  bRequestData: boolean;
  Erg: integer;
  bOK: boolean;

begin
  Result:=false;
  // Vorbelegung R�ckgaben:
  sQuery:='';
  sRequestBinData:='';
  sInfoWert_Aktuell:='';
  sErr:='';

  with RegisterRequestData do begin
    with SlaveData do begin
      // Request mit Datenteil (f�r Register-Einstell-Requests): 28.06.2017, WW
      bRequestData:=(FktCode = 5) OR  // 22.07.2019, WW
                    (FktCode = 6) OR (FktCode = 16);
      if bRequestData then begin   // nur bei Einstell-Funktion
        { Wert-String in Bin�rdaten-Format wandeln: }
        Erg:=Get_Modbus_BinData (Wert_Einstellen, ByteOrder, Typ,
                                 iNoOfPoints, sRequestBinData, sInfoWert_Aktuell);
        case Erg of
          -1: begin
                sErr:=Format ('Function %.2d: Unknown value type %s',
                  [FktCode, Typ]);
                exit;
              end;

          -2: begin
                sErr:=Format ('Function %.2d: Value %s out of range of type %s',
                  [FktCode, Wert_Einstellen, Typ]);
                exit;
              end;

          -3: begin
                sErr:=Format ('Function %.2d: Value type missing', [FktCode]);
                exit;
              end;
        end;  { case Erg }
      end;

      // Query bilden:
      bOK:=true;
      case FktCode of
        1: begin
             sQuery:=Get_Modbus_Query_01 (SlaveAdresse, iStartAdresse_Index0,
                                          iNoOfPoints, ModbusModus, iTID);  // 28.06.2017, WW
           end;

        2: begin
             sQuery:=Get_Modbus_Query_02 (SlaveAdresse, iStartAdresse_Index0,
                                          iNoOfPoints, ModbusModus, iTID);  // 28.06.2017, WW
           end;

        3: begin
             sQuery:=Get_Modbus_Query_03 (SlaveAdresse, iStartAdresse_Index0,
                                          iNoOfPoints, ModbusModus, iTID);
           end;

        4: begin
             sQuery:=Get_Modbus_Query_04 (SlaveAdresse, iStartAdresse_Index0,
                                          iNoOfPoints, ModbusModus, iTID);  // 28.06.2017, WW
           end;
    
        5: begin
             sQuery:=Get_Modbus_Query_05 (SlaveAdresse, iStartAdresse_Index0,
                                          sRequestBinData, ModbusModus, iTID);  // 22.07.2019, WW
           end;

        6: begin
             sQuery:=Get_Modbus_Query_06 (SlaveAdresse, iStartAdresse_Index0,
                                          sRequestBinData, ModbusModus, iTID);  // 28.06.2017, WW
           end;

        16: begin
              bOK:=Get_Modbus_Query_16 (SlaveAdresse, iStartAdresse_Index0,
                                        iNoOfPoints, sRequestBinData,
                                        ModbusModus, sQuery, iTID);
            end;

        20: begin
             sQuery:=Get_Modbus_Query_20 (SlaveAdresse, FileNr, iStartAdresse_Index0,
                                          iNoOfPoints, ModbusModus, iTID);  // 18.02.2021, WW
           end;
      else
        sErr:=Format ('Function %.2d not implemented', [FktCode]);
        exit;
      end;

      if not bOK then begin
        sErr:=Format ('Function %.2d: Error creating modbus query', [FktCode]);
        exit;
      end;
    end;
  end;

  Result:=true;
end;

{---------------------------------------------------------------------------------}
function Konv_Modbus_Response (ModbusModus: TModbusModus;
  SlaveData: TSlaveData; RegisterRequestData: TRegisterRequestData;
  iStartAdresse_Index0: word; iNoOfPoints: word;
  sResponse: string; sRequestBinData: string;
  var bPreset: boolean; var sValue: string;
  var AFehlergruppe: integer; var AFehlercode: integer; var sErr: string): boolean;
{---------------------------------------------------------------------------------}
{ Modbus-Response auswerten und konvertieren;
  �bergaben: Modbus-Modus
             Slave-Daten
             Register-Requestdaten
             Startadresse (bei 0 beginnend)
             No. of Points (Anzahl Register oder Stati)
             Response
             Wert in Bin�rdaten-Format (bei Einstell-Query)
  R�ckgaben: Flag 'bPreset': true, wenn Einstellen erfolgreich (nur bei Einstell-
               Response)
             Gelesener Wert (nur, wenn ein einzelner Wert gelesen wird, sonst leer)
               -> Zum Konvertieren eines einzelnen gelesenen Werts die Liste der
                  konvertierten Registerwerte in Register-Requestdaten auf nil setzen
               -> Aktuell wird nur Funktionscode 3 (Read holding Registers) unterst�tzt
             Fehlergruppe, Fehlercode
             Fehler-Text (leer, wenn Query erfolgreich gebildet werden konnte)
  Ergebnis: true, wenn Response erfolgreich ausgewertet und konvertiert werden konnte }
var
  iAnzahlBytesResponse_Soll: word;
  bOK: boolean;

begin
  Result:=false;
  // Vorbelegung R�ckgaben:
  bPreset:=false;
  sValue:='';
  AFehlergruppe:=0;
  AFehlercode:=0;
  sErr:='';

  with RegisterRequestData do begin
    with SlaveData do begin
      case FktCode of
        1: begin  // 28.06.2017, WW
             iAnzahlBytesResponse_Soll:=Calc_Modbus_AnzahlBytes_Status (iNoOfPoints);
             if not Get_Modbus_Response_01 (sResponse, ModbusModus, ByteOrder,
                                            RegisterKonvListe, iAnzahlBytesResponse_Soll,
                                            AFehlergruppe, AFehlercode) then begin
               sErr:='Error in modbus response (function 01): ';
               exit;
             end;
           end;

        2: begin  // 28.06.2017, WW
             iAnzahlBytesResponse_Soll:=Calc_Modbus_AnzahlBytes_Status (iNoOfPoints);
             if not Get_Modbus_Response_02 (sResponse, ModbusModus, ByteOrder,
                                            RegisterKonvListe, iAnzahlBytesResponse_Soll,
                                            AFehlergruppe, AFehlercode) then begin
               sErr:='Error in modbus response (function 02): ';
               exit;
             end;
           end;

        3: begin
             iAnzahlBytesResponse_Soll:=Calc_Modbus_AnzahlBytes (iNoOfPoints);
             if Assigned (RegisterKonvListe) then  // 29.09.2021, WW
               bOK:=Get_Modbus_Response_03 (sResponse, ModbusModus, ByteOrder,
                                            RegisterKonvListe, iAnzahlBytesResponse_Soll,
                                            AFehlergruppe, AFehlercode)
             else  // Einzel-Wert konvertieren; 29.09.2021, WW
               bOK:=Get_Modbus_Response_03 (sResponse, ModbusModus, ByteOrder,
                                            Typ, iAnzahlBytesResponse_Soll, sValue,
                                            AFehlergruppe, AFehlercode);
             if not bOK then begin
               sErr:='Error in modbus response (function 03): ';
               exit;
             end;
           end;

        4: begin  // 28.06.2017, WW
             iAnzahlBytesResponse_Soll:=Calc_Modbus_AnzahlBytes (iNoOfPoints);
             if not Get_Modbus_Response_04 (sResponse, ModbusModus, ByteOrder,
                                            RegisterKonvListe, iAnzahlBytesResponse_Soll,
                                            AFehlergruppe, AFehlercode) then begin
               sErr:='Error in modbus response (function 04): ';
               exit;
             end;
           end;

        5: begin  // 22.07.2019, WW
             if not Get_Modbus_Response_05 (sResponse, ModbusModus, iStartAdresse_Index0,
                                            sRequestBinData, bPreset,
                                            AFehlergruppe, AFehlercode) then begin
               sErr:='Error in modbus response (function 05): ';
               exit;
             end;
           end;

        6: begin  // 28.06.2017, WW
             if not Get_Modbus_Response_06 (sResponse, ModbusModus, iStartAdresse_Index0,
                                            sRequestBinData, bPreset,
                                            AFehlergruppe, AFehlercode) then begin
               sErr:='Error in modbus response (function 06): ';
               exit;
             end;
           end;

       16: begin  // 28.06.2017, WW
             if not Get_Modbus_Response_16 (sResponse, ModbusModus, iStartAdresse_Index0,
                                            iNoOfPoints, bPreset,
                                            AFehlergruppe, AFehlercode) then begin
               sErr:='Error in modbus response (function 16): ';
               exit;
             end;
           end;

       20: begin  // 18.02.2021, WW
             iAnzahlBytesResponse_Soll:=Calc_Modbus_AnzahlBytes (iNoOfPoints);
             if not Get_Modbus_Response_20 (sResponse, ModbusModus, ByteOrder,
                                            RegisterKonvListe, iAnzahlBytesResponse_Soll,
                                            AFehlergruppe, AFehlercode) then begin
               sErr:='Error in modbus response (function 20): ';
               exit;
             end;
           end;
      else
        AFehlergruppe:=COM_MODBUSERROR;
        AFehlercode:=MODBUSERR_RESP_UNKNOWNFUNCTION;  // 08.03.2019, WW
        sErr:=Format ('Response evaluation not implemented (function %.2d): ',
                      [FktCode]);
        exit;
      end;
    end;
  end;

  Result:=true;
end;

end.

