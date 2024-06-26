{******************************************************************************}
{* Unit: Auswerten von Schnittstellen-Antworten auf DSfG-DF�-Befehle          *}
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
{ �berpr�ft eine DSfG-DF�-Antwort auf Richtigkeit;
  �bergabe: Wieser (true, wenn es sich um eine Antwort auf einen Wieser-
                    DSfG-DF�-Befehl handelt)
            Antwort auf DSfG-DF�-Befehl;
  R�ckgabe: Fehlergruppe, Fehlercode f�r Journal
  Ergebnis: true, wenn Antwort ok }
begin
  Result:=false;
  Fehlergruppe:=COM_DSFGDFUERROR;
  if length (Antwort) >= 3 then begin
    if Wieser then begin                            { Antwort Wieser-DSfG-DF� }
      case Antwort[2] of         { Befehlszeichen oder DSfG-DF�-Fehlerzeichen }
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
    else begin                        { Antwort auf genormten DSfG-DF�-Befehl }
      { Befehlszeichen oder DSfG-DF�-Fehlerzeichen pr�fen; zus�tzlich L�nge pr�fen,
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
{ �berpr�ft die Antwort auf den Firmwareupdate-Befehl '1F' auf Richtigkeit und
  liefert den Code f�r die neue Baudrate, mit der weiter kommuniziert wird, zur�ck;
  �bergabe: Antwort auf Firmwareupdate-Befehl;
  R�ckgabe: Fehlergruppe, Fehlercode f�r Journal
            Code f�r neue Baudrate, mit der weiter kommuniziert wird
  Ergebnis: true, wenn Antwort ok }
var
  S: string;

begin
  Result:=false;
  // Vorbelegungen f�r R�ckgabe
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

        NewBaudrate:=S [4];  // R�ckgabe: Code f�r neue Baudrate
        Result:=true;
      end;
    end else
      Fehlercode:=FIRMWUPDERR_ANTWORTUNVOLLSTAENDIG;
  end else
    Fehlercode:=FIRMWUPDERR_ANTWORTUNERWARTET;
end;

end.

