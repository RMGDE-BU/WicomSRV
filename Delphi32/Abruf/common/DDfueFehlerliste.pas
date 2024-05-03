{******************************************************************************}
{* Unit: Fehlerliste der Wieser-DSfG-DFÜ decodieren                           *}
{* Version: 07.05.2001 WW                                                     *}
{******************************************************************************}
unit DDfueFehlerliste;

interface

uses
  SysUtils, WStrUtils, WChars;


procedure Decode_WieserDSfGDfueFehlerliste (Fehlerliste: string;
                                            var Fehler_1: string;
                                            var Fehler_2: string;
                                            var Fehler_3: string;
                                            var Fehler_Anstehend: string;
                                            var Fehler_DSfG: integer);

implementation


{-------------------------------------------------------------------------------}
procedure Decode_WieserDSfGDfueFehlerliste (Fehlerliste: string;
                                            var Fehler_1: string;
                                            var Fehler_2: string;
                                            var Fehler_3: string;
                                            var Fehler_Anstehend: string;
                                            var Fehler_DSfG: integer);
{-------------------------------------------------------------------------------}
{ scannt die eingelesene Fehlerliste der DSfG-DFÜ (Roh-Antwort) nach den
  verschiedenen Fehlerarten (DSFG-Fehler, anstehende Fehler);
  Übergabe: Fehlerliste (Antwort auf F-Befehl)
  Rückgabe: Detail-Fehler lt. DSfG-DFÜ-Beschreibung }
var
  fehl_count: integer;
  aPos: integer;
  S: string;

begin
  Fehler_1 := '';
  Fehler_2 := '';
  Fehler_3 := '';
  Fehler_Anstehend := '';
  Fehler_DSFG := 0;

  fehl_count := 0;
  if Pos ('#', Fehlerliste) <> 0 then begin
    aPos := Pos('#1', Fehlerliste);
    if aPos <> 0 then begin                                 { Fehler im WIROS }
      Fehler_1:=Copy (Fehlerliste, aPos + 2, 4);
      inc(fehl_count);
    end;

    aPos := Pos('#2', Fehlerliste);
    if aPos <> 0 then begin                                   { Stacküberlauf }
      Fehler_2:=Copy (Fehlerliste, aPos + 2, 4);
      inc(fehl_count);
    end;

    aPos := Pos('#3', Fehlerliste);
    if aPos <> 0 then begin                                  { weitere Fehler }
      Fehler_3:=Copy (Fehlerliste, aPos + 2, 4);
      inc(fehl_count);
    end;
  end;

  aPos := Pos(US, Fehlerliste);
  if aPos <> 0 then begin                                       { DSfG-Fehler }
    S:=ExtractString (Fehlerliste, US, ETX, 0);
    Fehler_DSFG:=StrToInt (S);
  end;

  aPos:=(fehl_count * 6) + 3;     { Position der anstehenden Fehler im String }
  S:=Copy (Fehlerliste, aPos, length (Fehlerliste) - 1);
  S:=ExtractString (S, NUL, ETX, 0);                   { ohne ETX }
  Fehler_Anstehend:=ExtractString (S, NUL, US, 0);
end;

end.
