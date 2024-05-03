{******************************************************************************}
{* Unit: Allgemeine Konstanten, Typen und Funktionen für Geräte-Kommunikation *}
{* 24.03.2003 WW  für DSfG                                                    *}
{******************************************************************************}
unit WComm;

interface

uses
  Contnrs, WChars, ErrConst;

Const
  prefix_MRG_Roh    = '~MR';    { Präfix für MRG-Rohdaten-Filename }
  prefix_DSfG_Roh   = '~DS';    { Präfix für DSfG-Rohdaten-Filename }
  prefix_DSfG_Ar    = 'DAR';    { Präfix für DSfG-Archivdaten-Filename (TDSfGSatzData-Format) }
  prefix_DSfG_Lb    = 'DLB';    { Präfix für DSfG-Logbuchdaten-Filename (TDSfGSatzData-Format) }
  prefix_SrvData    = '~SV';    { Präfix für Abrufserver-Daten-Filename }
  prefix_SMS_XML    = 'SMS';    { Präfix für SMS-Datenantwort (XML-Format) }
  prefix_SMS_Roh    = '~SM';    { Präfix für SMS-Rohdaten-Filename }
  prefix_SMS_Import = 'SMI';    { Präfix für SMS-Import-Filename (SMS-Format) }
  prefix_GPRS_XML   = 'GPR';    { Präfix für GPRS-Datenantwort (XML-Format) }
  prefix_GPRS_Roh   = '~GP';    { Präfix für GPRS-Rohdaten-Filename }
  prefix_MB_Konv    = '~MB';    { Präfix für Modbus-Konvdaten-Filename }

  ext_MRG_TempRoh = '.roh';     { Filenamen-Erweiterung für Dateien mit temporären MRG-Rohdaten }
  ext_Base64      = '.base64';  { Filenamen-Erweiterung für Dateien mit Base64-kodierten Rohdaten }

type
  TAnswerDest = (ad_File, ad_String);  { Antwortdaten-Ziel: Dateipuffer, Stringpuffer }

  TRueckgabe = record                  { Rückgabe-Record von Sende-Routinen }
    Fehlergruppe: integer;             { Fehlergruppe lt. Errtxt32.dll }
    Fehlercode: integer;               { Fehlercode lt. Errtxt32.dll }
    Antwort: string;                   { Antwortdaten bzw. Filename mit Antwortdaten }
  end;

  { Allgemeiner Fehler-Record für Fehlergruppen und -Konstanten aus ErrConst.Pas }
  TFehler = record
    Gruppe: integer;
    Code: integer;
  end;

  { Record für Befehldaten }
  TBefehlData = record
    sBefehl: string;  // der Befehl selbst
    sInfo: string;  // Zusatzinfo zum Befehl
  end;

  { Objekt für Befehlliste }
  TBefehlDataObj = class (TObject)
  public
    Daten: TBefehlData;
    constructor Create (ADaten: TBefehlData);
  end;

  { Befehlliste }
  TBefehlList = class (TObjectList)
  end;

function FehlerGruppeCode_OK (AFehlergruppe, AFehlercode: integer): boolean;
function IsSignaturFehler (AFehlergruppe: integer): boolean;

implementation

{--------------------------------------------------------------------------}
function FehlerGruppeCode_OK (AFehlergruppe, AFehlercode: integer): boolean;
{--------------------------------------------------------------------------}
{ Ergebnis: true, wenn übergebene Fehlergruppe/Fehlercode-Kombination aus Errtxt32.dll
            den OK-Status darstellt }
begin
  Result:=AFehlercode = 0;  //  Fehler/Warnung, wenn Fehlercode <> 0
end;

{----------------------------------------------------------}
function IsSignaturFehler (AFehlergruppe: integer): boolean;  // 25.01.2013, WW
{----------------------------------------------------------}
{ Auf Signatur-Fehler prüfen;
{ Ergebnis: true, wenn Fehlergruppe einer Signatur-Fehlergruppe entspricht }
begin
  Result:=(AFehlergruppe = SYS_SIGNSRVSYSERROR) OR
          (AFehlergruppe = COM_SIGNSRV_ERROR) OR
          (AFehlergruppe = SYS_SIGNSRV_ERROR);
end;


{ TBefehlDataObj }

{------------------------------------------------------}
constructor TBefehlDataObj.Create (ADaten: TBefehlData);
{------------------------------------------------------}
begin
  inherited Create;
  Daten:=ADaten;
end;

end.

