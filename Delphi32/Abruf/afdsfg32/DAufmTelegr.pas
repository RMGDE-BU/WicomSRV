{******************************************************************************}
{* Unit: Aufmerksamkeits-Telegramme bearbeiten (Rohdaten konvertieren und in  *}
{*       Telegramm-Journaldetailtabelle abspeichern)                          *}
{* 15.05.2001 WW                                                              *}
{******************************************************************************}
unit DAufmTelegr;

interface

uses
  Classes, SysUtils, WStrUtils, WChars, DSfGUtil, DSysCon, DJournal, WSysCon;


type
  { Objekt für TAufmTelegrammList }

  TAufmTelegrammListObj = class(TObject)
    Busadresse: string [1];
    Nachrichtentyp: string [1];
    DatumZeit: TDateTime;
    Zeitzone: string [1];
  public
    constructor Create (ABusadresse: string; ANachrichtentyp: string;
                        ADatumZeit: TDateTime; AZeitzone: string);
  end;

  { Liste für Aufmnerksamkeits-Telegramme }

  TAufmTelegrammList = class(TList)
  public
    Destructor Destroy; override;
  end;


function SaveAufmerksamkeitsTelegramme (JournalId: integer; RohTelegramme: string;
                                        Extensionmode: byte;
                                        var SenderBusadressen: string): boolean;

implementation

{ TAufmTelegrammListObj }

{------------------------------------------------------------------------------------------}
constructor TAufmTelegrammListObj.Create (ABusadresse: string; ANachrichtentyp: string;
                                          ADatumZeit: TDateTime; AZeitzone: string);
{------------------------------------------------------------------------------------------}
begin
  inherited Create;
  Busadresse:=ABusadresse;
  Nachrichtentyp:=ANachrichtentyp;
  DatumZeit:=ADatumZeit;
  Zeitzone:=AZeitzone;
end;


{ TAufmTelegrammList }

{------------------------------------}
Destructor TAufmTelegrammList.Destroy;
{------------------------------------}
var
  i: integer;
Begin
  for i:=0 to Count-1 do
    TAufmTelegrammListObj (Items [i]).Free;
  inherited Destroy;
end;


{-----------------------------------------------------------------------------}
function GetTelegrammDaten (Rohstring: string;
                            var Busadresse: string; var Nachrichtentyp: string;
                            var DatumZeit: TDateTime;
                            var Zeitzone: string): boolean;
{-----------------------------------------------------------------------------}
{ Verifizierung des DSfG-Telegramm HDCL's }
var
  StringPos: integer;
  S: string;
  DID: integer;
  Code: integer;
  DatumStr: string;
  ZeitStr: string;
  Jahr: integer;
  Zeit: TDateTime;

begin
  Result:=false;
  Busadresse:='';
  Nachrichtentyp:='';
  DatumZeit:=-1;
  Zeitzone:='';

  StringPos:=1;
  S := read_hdcl_fromString(Rohstring, StringPos);         { alles bis zum ersten US (keine Auswertung) }

  { HDCL auswerten: }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { DID }
  val(S, DID, Code);
  if Code <> 0 then exit;
  if DID < C_DID_DataSimple then exit;

  S := read_hdcl_fromString(Rohstring, StringPos);                      { TID }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { BLO }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { BNR }
  Busadresse := read_hdcl_fromString(Rohstring, StringPos);             { DNO }
  Nachrichtentyp := read_hdcl_fromString(Rohstring, StringPos);         { NTY }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { DFO }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { DEB }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { ZAE }

  { weitere Felder (nur TDA und TTI werden ausgewertet): }
  if (DID AND C_PAS) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { PAS }
  if (DID AND C_DTY) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { DTY }
  if (DID AND C_ABS) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { ABS }
  if (DID AND C_EMF) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { EMF }
  if (DID AND C_TDA) <> 0 then
    DatumStr := read_hdcl_fromString(Rohstring, StringPos);    { TDA, obligat }
  if (DID AND C_TTI) <> 0 then begin
    S := read_hdcl_fromString(Rohstring, StringPos);           { TTI, obligat }
    ZeitStr := Copy (S, 1, 6);                               { 6 Stellen Zeit }
    if length (S) > 6 then
      Zeitzone := S [7];                      { optional mit Zeitzonen-Kenner }
  end;
  if (DID AND C_PTB) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { PTB }
  Result := true;

  if length (DatumStr) > 0 then begin                 { DatumStr -> TDateTime }
    try
      Jahr:=StrToInt (Copy(DatumStr, 1, 2));
      if Jahr > 80 then
        Jahr:=Jahr + 1900
      else
        Jahr:=Jahr + 2000;
      DatumZeit:=EncodeDate (Jahr,
                             StrToInt (Copy (DatumStr, 3, 2)),
                             StrToInt (Copy (DatumStr, 5, 2)));
    except
      Result:=false;
    end;
  end;

  if length (ZeitStr) > 0 then begin                   { ZeitStr -> TDateTime }
    try
      Zeit:=EncodeTime (StrToInt (Copy (ZeitStr, 1, 2)),
                        StrToInt (Copy (ZeitStr, 3, 2)),
                        StrToInt (Copy (ZeitStr, 5, 2)),
                        0);
      if DatumZeit > -1 then
        DatumZeit:=DatumZeit + Zeit
      else
        DatumZeit:=Zeit;
    except
      Result:=false;
    end;
  end;
end;

{--------------------------------------------------------------------------------}
function SaveAufmerksamkeitsTelegramme (JournalId: integer; RohTelegramme: string;
                                        Extensionmode: byte;
                                        var SenderBusadressen: string): boolean;
{--------------------------------------------------------------------------------}
{ Roh-Telegramme konvertieren und in Telegramm-Journaltabelle abspeichern;
  Übergabe: JournalId des Abrufs
            Roh-Telegramme (Schnittstellen-Rohdaten eines oder mehrerer Aufmerksamkeits-Telegramme)
            Extensionmode, mit dem die Telegramme gesendet wurden
  Rückgabe: Busadressen der Instanzen, welche die Telegramme versendet haben
  Ergebnis: false, wenn ein oder mehrere Telegramme inhaltlich fehlerhaft }
var
  S: string;
  Trenner: char;
  einTelegramm: string;
  Busadresse: string;
  Nachrichtentyp: string;
  DatumZeit: TDateTime;
  Zeitzone: string;
  TelegrammList: TAufmTelegrammList;
  TelegrammListObj: TAufmTelegrammListObj;

begin
  Result:=true;
  SenderBusadressen:='';

  S:=RohTelegramme;
  if Extensionmode = 1 then begin
    S:=Copy (S, 2, length (S));                            { STX wegschneiden }
    Trenner:=STX;
  end else
    Trenner:=FS;

  TelegrammList:=TAufmTelegrammList.Create;
  try
    { Roh-Telegramme in Einzel-Telegramme zerlegen und in Telegrammliste konvertieren: }
    einTelegramm:=F_Zerlegen (S, Trenner);
    while length (einTelegramm) > 0 do begin
      if not GetTelegrammDaten (einTelegramm, Busadresse, Nachrichtentyp,
                            DatumZeit, Zeitzone ) then Result:=false;
      TelegrammListObj:=TAufmTelegrammListObj.Create (Busadresse, Nachrichtentyp,
                                                      DatumZeit, Zeitzone);
      TelegrammList.Add (TelegrammListObj);

      { Sender-Busadressen sammeln (jede Adresse nur einmal): }
      if Pos (Busadresse, SenderBusadressen) = 0 then
        SenderBusadressen:=SenderBusadressen + Busadresse;
      einTelegramm:=F_Zerlegen (S, Trenner);
    end; { while }

    { Telegrammliste in Telegramm-Tabelle abspeichern: }
    WriteJournalTelegramme (JournalId, TelegrammList);
  finally
    TelegrammList.Free;
  end;
end;

end.

