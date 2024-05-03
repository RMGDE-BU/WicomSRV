{******************************************************************************}
{* Unit: Auswerten von Schnittstellen-Antworten auf DSfG-DFÜ-Befehle          *}
{* 11.04.2001 WW                                                              *}
{******************************************************************************}
unit DValidAnswer;

INTERFACE

uses
  WStrUtils, WChars, ErrConst;


function ValidDSfGDfueAntwort (Wieser: boolean; Antwort: string;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function ValidFirmwareUpdateAntwort (Antwort: string; var Fehlergruppe: integer;
  var Fehlercode: integer; var NewBaudrate: string): boolean;

IMPLEMENTATION

{--------------------------------------------------------------}
function ValidDSfGDfueAntwort (Wieser: boolean; Antwort: string;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{--------------------------------------------------------------}
{ überprüft eine DSfG-DFÜ-Antwort auf Richtigkeit;
  Übergabe: Wieser (true, wenn es sich um eine Antwort auf einen Wieser-
                    DSfG-DFÜ-Befehl handelt)
            Antwort auf DSfG-DFÜ-Befehl;
  Rückgabe: Fehlergruppe, Fehlercode für Journal
  Ergebnis: true, wenn Antwort ok }
begin
  Result:=false;
  Fehlergruppe:=COM_DSFGDFUERROR;
  if length (Antwort) >= 3 then begin
    if Wieser then begin                            { Antwort Wieser-DSfG-DFÜ }
      case Antwort[2] of         { Befehlszeichen oder DSfG-DFÜ-Fehlerzeichen }
        '?': Fehlercode:=DSFGDFUERR_WIESER_UNBEKANNT;
        '#': Fehlercode:=DSFGDFUERR_WIESER_FALSCHENUMMER;
        '!': Fehlercode:=DSFGDFUERR_WIESER_AENDEREUNGNICHTERLAUBT;
        '=': Fehlercode:=DSFGDFUERR_WIESER_VERBOTEN;  // bei NG; 22.02.2012, WW
      else
        { Antwort ist OK: }
        Fehlergruppe:=0;
        Fehlercode:=0;
        Result:=true;
      end;
    end
    else begin                        { Antwort auf genormten DSfG-DFÜ-Befehl }
      { Befehlszeichen oder DSfG-DFÜ-Fehlerzeichen prüfen; zusätzlich Länge prüfen,
        da z.B. '?' auch als Befehlszeichen existiert ! }
      if (Antwort[2] = '?') AND not (length (Antwort) > 3) then
        Fehlercode:=DSFGDFUERR_UNBEKANNT
      else if (Antwort[2] = '!') AND not (length (Antwort) > 3) then
        Fehlercode:=DSFGDFUERR_FALSCHESYNTAX
      else begin
        { Antwort ist OK: }
        Fehlergruppe:=0;
        Fehlercode:=0;
        Result:=true;
      end;
    end;
  end else
    Fehlercode:=DSFGDFUERR_ANTWORTUNVOLLSTAENDIG;
end;

{------------------------------------------------------------------------------}
function ValidFirmwareUpdateAntwort (Antwort: string; var Fehlergruppe: integer;
  var Fehlercode: integer; var NewBaudrate: string): boolean;
{------------------------------------------------------------------------------}
{ überprüft die Antwort auf den Firmwareupdate-Befehl '1F' auf Richtigkeit und
  liefert den Code für die neue Baudrate, mit der weiter kommuniziert wird, zurück;
  Übergabe: Antwort auf Firmwareupdate-Befehl;
  Rückgabe: Fehlergruppe, Fehlercode für Journal
            Code für neue Baudrate, mit der weiter kommuniziert wird
  Ergebnis: true, wenn Antwort ok }
var
  S: string;

begin
  Result:=false;
  // Vorbelegungen für Rückgabe
  NewBaudrate:='';
  Fehlergruppe:=EST_FIRMWAREUPDATEERROR;

  S:=ExtractString (Antwort, STX, ETX, 0);
  if Copy (S, 1, 2) = '1F' then begin
    if length (S) >= 4 then begin
      if S [3] = '!' then begin  // Fehler-Antwort
        case S [4] of         { Fehlercode }
          '0': Fehlercode:=FIRMWUPDERR_ADRSTARTGLEICHENDE;
          '1': Fehlercode:=FIRMWUPDERR_ADRSTARTZUKLEIN;
          '2': Fehlercode:=FIRMWUPDERR_ADRSTARTGROESSERENDE;
        else
          Fehlercode:=FIRMWUPDERR_UNDEFINIERT;
        end;
      end
      else begin
        { Antwort ist OK: }
        Fehlergruppe:=0;
        Fehlercode:=0;

        NewBaudrate:=S [4];  // Rückgabe: Code für neue Baudrate
        Result:=true;
      end;
    end else
      Fehlercode:=FIRMWUPDERR_ANTWORTUNVOLLSTAENDIG;
  end else
    Fehlercode:=FIRMWUPDERR_ANTWORTUNERWARTET;
end;

end.

