{******************************************************************************}
{* Unit: Aufmerksamkeits-Telegramme aus Rohformat in Liste konvertieren       *}
{* 07.01.2003 WW                                                              *}
{******************************************************************************}
unit DAufmTelegr;

interface

uses
  SysUtils, WStrUtils, WChars, DSfGUtil, WSysCon, DListen;


function GetAufmerksamkeitsTelegramme (RohTelegramme: string;
                                       Extensionmode: byte;
                                       TelegrammList: TAufmTelegrammList): boolean;

implementation

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

{---------------------------------------------------------------------------------}
function GetAufmerksamkeitsTelegramme (RohTelegramme: string;
                                       Extensionmode: byte;
                                       TelegrammList: TAufmTelegrammList): boolean;
{---------------------------------------------------------------------------------}
{ Roh-Telegramme in Telegrammliste konvertieren;
  Übergabe: Roh-Telegramme (Schnittstellen-Rohdaten eines oder mehrerer Aufmerksamkeits-Telegramme)
            Extensionmode, mit dem die Telegramme gesendet wurden
  Rückgabe: Liste mit Aufmerksamkeits-Telegrammen
  Ergebnis: false, wenn ein oder mehrere Telegramme inhaltlich fehlerhaft }
var
  S: string;
  Trenner: char;
  einTelegramm: string;
  Busadresse: string;
  Nachrichtentyp: string;
  DatumZeit: TDateTime;
  Zeitzone: string;
  TelegrammListObj: TAufmTelegrammListObj;

begin
  Result:=true;

  S:=RohTelegramme;
  if Extensionmode >= 1 then begin  // 20.07.2012, WW
    S:=Copy (S, 2, length (S));                            { STX wegschneiden }
    Trenner:=STX;
  end else
    Trenner:=FS;

  if Assigned (TelegrammList) then begin
    { Roh-Telegramme in Einzel-Telegramme zerlegen und in Telegrammliste konvertieren: }
    einTelegramm:=F_Zerlegen (S, Trenner);
    while length (einTelegramm) > 0 do begin
      if not GetTelegrammDaten (einTelegramm, Busadresse, Nachrichtentyp,
                            DatumZeit, Zeitzone ) then Result:=false;
      TelegrammListObj:=TAufmTelegrammListObj.Create (Busadresse, Nachrichtentyp,
                                                      DatumZeit, Zeitzone);
      TelegrammList.Add (TelegrammListObj);
      einTelegramm:=F_Zerlegen (S, Trenner);
    end; { while }
  end;
end;

end.

