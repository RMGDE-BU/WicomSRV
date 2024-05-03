{******************************************************************************}
{* Unit: Definitionen und Funktionen zu den Modemtyp-Konstanten für Abruf-    *}
{*       Serverdienst                                                         *}
{* 24.04.2006 WW                                                              *}
{******************************************************************************}
unit SrvModemtyp;

interface

uses
  SerialConst, WSysCon;

function GetSrvModemTyp_SerialDataFormatDesc (ModemTyp: string;
  var sDataFormatDesc: string): boolean;

implementation

type
  // Struktur für Modemtyp-Feld
  TSrvModemTyp = record
    Modemtyp: string;
    DataFormatDesc: string;
  end;

const
  { Feld mit Definitionen für Modemtyp-Konstanten, welche seriellen Datenformaten
    entsprechen }
  SrvModemTypen: array[1..2] of TSrvModemTyp =
    ((Modemtyp: srv_modem_8N1; DataFormatDesc: CSDataFormat_8N1),
     (Modemtyp: srv_modem_7E1; DataFormatDesc: CSDataFormat_7E1)
    );

    
{----------------------------------------------------------------}
function GetSrvModemTyp_SerialDataFormatDesc (ModemTyp: string;
  var sDataFormatDesc: string): boolean;
{----------------------------------------------------------------}
{ liefert zum übergebenen Modemtyp das zugeordnete serielle Datenformat
  (z.B. '8N1', '7E1'); 21.04.2006 WW
  Ergebnis: true, wenn Modemtyp übergeben wurde, für den eine Zuordnung
            definiert ist }
var
  i: byte;
begin
  Result:=false;
  sDataFormatDesc:='';
  for i:=Low (SrvModemTypen) to High (SrvModemTypen) do begin
    if Pos (SrvModemTypen [i].Modemtyp, ModemTyp) > 0 then begin
      sDataFormatDesc:=SrvModemTypen [i].DataFormatDesc;
      Result:=true;
      Break;
    end;
  end;
end;

end.

