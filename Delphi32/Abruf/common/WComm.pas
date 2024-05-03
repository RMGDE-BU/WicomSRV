{******************************************************************************}
{* Unit: Allgemeine Konstanten, Typen und Funktionen f�r Ger�te-Kommunikation *}
{* 24.03.2003 WW  f�r DSfG                                                    *}
{******************************************************************************}
unit WComm;

interface

uses
  Contnrs, WChars, ErrConst;

Const
  prefix_MRG_Roh    = '~MR';    { Pr�fix f�r MRG-Rohdaten-Filename }
  prefix_DSfG_Roh   = '~DS';    { Pr�fix f�r DSfG-Rohdaten-Filename }
  prefix_DSfG_Ar    = 'DAR';    { Pr�fix f�r DSfG-Archivdaten-Filename (TDSfGSatzData-Format) }
  prefix_DSfG_Lb    = 'DLB';    { Pr�fix f�r DSfG-Logbuchdaten-Filename (TDSfGSatzData-Format) }
  prefix_SrvData    = '~SV';    { Pr�fix f�r Abrufserver-Daten-Filename }
  prefix_SMS_XML    = 'SMS';    { Pr�fix f�r SMS-Datenantwort (XML-Format) }
  prefix_SMS_Roh    = '~SM';    { Pr�fix f�r SMS-Rohdaten-Filename }
  prefix_SMS_Import = 'SMI';    { Pr�fix f�r SMS-Import-Filename (SMS-Format) }
  prefix_GPRS_XML   = 'GPR';    { Pr�fix f�r GPRS-Datenantwort (XML-Format) }
  prefix_GPRS_Roh   = '~GP';    { Pr�fix f�r GPRS-Rohdaten-Filename }
  prefix_MB_Konv    = '~MB';    { Pr�fix f�r Modbus-Konvdaten-Filename }

  ext_MRG_TempRoh = '.roh';     { Filenamen-Erweiterung f�r Dateien mit tempor�ren MRG-Rohdaten }
  ext_Base64      = '.base64';  { Filenamen-Erweiterung f�r Dateien mit Base64-kodierten Rohdaten }

type
  TAnswerDest = (ad_File, ad_String);  { Antwortdaten-Ziel: Dateipuffer, Stringpuffer }

  TRueckgabe = record                  { R�ckgabe-Record von Sende-Routinen }
    Fehlergruppe: integer;             { Fehlergruppe lt. Errtxt32.dll }
    Fehlercode: integer;               { Fehlercode lt. Errtxt32.dll }
    Antwort: string;                   { Antwortdaten bzw. Filename mit Antwortdaten }
  end;

  { Allgemeiner Fehler-Record f�r Fehlergruppen und -Konstanten aus ErrConst.Pas }
  TFehler = record
    Gruppe: integer;
    Code: integer;
  end;

  { Record f�r Befehldaten }
  TBefehlData = record
    sBefehl: string;  // der Befehl selbst
    sInfo: string;  // Zusatzinfo zum Befehl
  end;

  { Objekt f�r Befehlliste }
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
{ Ergebnis: true, wenn �bergebene Fehlergruppe/Fehlercode-Kombination aus Errtxt32.dll
            den OK-Status darstellt }
begin
  Result:=AFehlercode = 0;  //  Fehler/Warnung, wenn Fehlercode <> 0
end;

{----------------------------------------------------------}
function IsSignaturFehler (AFehlergruppe: integer): boolean;  // 25.01.2013, WW
{----------------------------------------------------------}
{ Auf Signatur-Fehler pr�fen;
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

