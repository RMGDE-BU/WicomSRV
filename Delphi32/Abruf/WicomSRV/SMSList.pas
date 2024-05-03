{******************************************************************************}
{* Unit: Liste mit Informationen empfangener SMS                              *}
{* 10.11.2004 WW                                                              *}
{******************************************************************************}
unit SMSList;

interface

uses
  contnrs, SysUtils, WChars, WStrUtils, MDTFormt, WSysCon, LogFile, SMSDecode;

type
  { Record für SMS-Daten }

  TSMS = record
    Index: integer;
    DatumZeit: TDateTime;
    SMS_Data: string;
    geloescht: boolean;
  end;

  { Objekt für SMS-Daten }

  TSMSListObj = class (TObject)
  public
    Daten: TSMS;
    constructor Create (ADaten: TSMS);
  end;

  { Liste für SMS }

  TSMSList = class(TObjectList)
  public
    function LoadFromModemSMSList_Rohstring (ARohstring: string;
                                             LogFilePfad: string;
                                             SMSLog: boolean;
                                             SMSLog_COMNr: integer): integer;
  end;

implementation

{---------------------------------------------------------}
function DatumZeitCompare (Item1, Item2: Pointer): Integer;
{---------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TSMSListObj-Objekten nach Feld 'DatumZeit' }
var
  Diff: TDateTime;
begin
  Diff:=TSMSListObj (Item1).Daten.DatumZeit - TSMSListObj (Item2).Daten.DatumZeit;
  if Diff < 0 then
    Result:=-1
  else if Diff > 0 then
    Result:=1
  else
    Result:=0;
end;


{ TSMSListObj }

{--------------------------------------------}
constructor TSMSListObj.Create (ADaten: TSMS);
{--------------------------------------------}
begin
  inherited Create;
  Daten:=ADaten;
end;


{ TSMSList }

{--------------------------------------------------------------------------------}
function TSMSList.LoadFromModemSMSList_Rohstring (ARohstring: string;
                                                  LogFilePfad: string;
                                                  SMSLog: boolean;
                                                  SMSLog_COMNr: integer): integer;
{--------------------------------------------------------------------------------}
{ lädt Rohantwort auf Modembefehl AT+CMGL in SMS-Liste und protokolliert SMS in
  SMS-Logfile;
  Übergabe: Rohantwort als String
            Pfad für SMS-Logfile
            Flag 'SMSLog' (wenn true, werden SMS in SMS-Logfile protokolliert
            COM-Nr, über die SMS empfangen wurden (für Logfilename)
  Ergebnis: Anzahl der in die Liste geladenen SMS }
var
  S: string;
  Zeile: string;
  sBuf: string;
  P: integer;
  DateStr: string;
  TimeStr: string;
  SMS: TSMS;
  SMSListObj: TSMSListObj;
  LogFilename: string;
  SMSLogFile: TCustomLogFile;
  sLog: string;
  GeraeteTyp: integer;
  isDigKanal_Imp: boolean;

begin
  Result:=0;
  { SMS-Logfile createn, wenn erforderlich: }
  if SMSLog then begin
    LogFilename:='SMS_' + Format ('%.3d', [SMSLog_COMNr]);
    SMSLogFile:=TCustomLogFile.Create (LogFilePfad, LogFilename, false);
  end else
    SMSLogFile:=nil;
  try
    sLog:='';

    { Rohantwort in SMS-Liste konvertieren: }
    S:=ARohString;
    while length (S) > 0 do begin
      P:=Pos (CR + LF, S);
      if P = 0 then
        P:=length (S) + 1;  { Position hinter Stringende }

      Zeile:=Copy (S, 1, P - 1);  { Zeile rauskopieren }
      System.Delete (S, 1, P + 1);  { Zeile incl. CR LF aus Quellstring löschen }

      if length (Zeile) > 0 then begin
        { prüfen, ob Zeile SMS-Kopf enthält: }
        if Pos ('+CMGL', Zeile) = 1 then begin    { SMS-Kopf beginnt mit +CMGL }
          if SMSLogFile <> nil then
            sLog:=sLog + Zeile + CR + LF;   // SMS-Kopf in Logfile protokollieren

          { SMS-Index: }
          sBuf:=ExtractString (Zeile, ':', ',', 0);
          sBuf:=F_LeftTrunc (sBuf, ' ');
          SMS.Index:=StrToInt (sBuf);

          { Servicecenter Zeitstempel (Zeitpunkt, zu dem die SMS vom Sender abgeschickt wurde): }
          sBuf:=ExtractString (Zeile, ',', ',', 3, '"');
          sBuf:=ExtractString (sBuf, '"', '"', 0);
          DateStr:=ExtractString (sBuf, NUL, ',', 0);
          TimeStr:=ExtractString (sBuf, ',', NUL, 0);
          TimeStr:=Copy (TimeStr, 1, 8);
          try
            SMS.DatumZeit:=MRGStrToDateTime('JJ/MM/TT', DateStr, 'HH:MM:SS', TimeStr);
          except
            SMS.DatumZeit:=0;
          end;

          { nächste Zeile enthält SMS-Daten: }
          P:=Pos (CR + LF, S);
          if P = 0 then
            P:=length (S) + 1;  { Position hinter Stringende }

          Zeile:=Copy (S, 1, P - 1);  { Zeile rauskopieren }
          System.Delete (S, 1, P + 1);  { Zeile incl. CR LF aus Quellstring löschen }

          if SMSLogFile <> nil then begin
            sLog:=sLog + Zeile + CR + LF + CR + LF;  // SMS-Daten im Rohformat protokollieren
            { prüfen, von welchem Gerätetyp die SMS stammt: }
            GeraeteTyp:=CheckSMSData_Geraetetyp (Zeile);
            { SMS-Daten gerätetypabhängig decodieren (Klartext): }
            sLog:=sLog + 'Gerätetyp: ';
            case GeraeteTyp of
              mrgtyp_Veribox_Mini:
                begin
                  sLog:=sLog + 'Veribox Mini';
                  isDigKanal_Imp:=true;  // alle (max. 4) Digitalkanäle der Veribox müssen als Impuls-Zählerkanäle konfiguriert sein !
                  sBuf:=DecodeSMSData_VeriboxMini (Zeile,
                                                   isDigKanal_Imp, isDigKanal_Imp,
                                                   isDigKanal_Imp, isDigKanal_Imp);
                end;

              mrgtyp_MRG910:  // gesamte 900er-Serie (MRG 905/910)
                begin
                  sLog:=sLog + 'MRG 900';  // MRG 905/910
                  sBuf:=DecodeSMSData_MRG900 (Zeile);
                end;
            else
              sLog:=sLog + 'unbekannt';
              sBuf:='';
            end;
            sLog:=sLog + CR + LF + CR + LF + sBuf + CR + LF + CR + LF;
          end;

          SMS.SMS_Data:=Zeile;
          SMS.geloescht:=false;  // Vorbelegung: SMS noch nicht im Modem gelöscht

          { Listenobjekt createn und in Liste einfügen: }
          SMSListObj:=TSMSListObj.Create (SMS);
          Add (SMSListObj);
          inc (Result);
        end;
      end;
    end;  { while }

    { SMS-Logfile nur schreiben, wenn SMS in Rohdaten enthalten ist: }
    if (SMSLogFile <> nil) AND (length (sLog) > 0) then
      SMSLogFile.Write (CR + LF + sLog);  { Logfile-Protokollierung }

    Sort (DatumZeitCompare);   { SMS-Liste nach dem SMS-Zeitstempel sortieren }
  finally
    SMSLogFile.Free;
  end;
end;

end.
