{*******************************************************************************************}
{* Unit: Hilfsfunktionen für LAKS-Programme                                                *}
{* Version: 29.06.99   WW                                                                  *}
{*******************************************************************************************}
Unit LaksUtil;

INTERFACE

uses
  SysUtils, WStrUtils, ParaNum;

Const

  { Zeichen }

  NUL = #0;
  STX = #2;
  ETX = #3;
  LF  = #10;
  CR  = #13;
  RS  = #30;
  US  = #31;

  { Maximale Lastenanzahl }
  C_Laks_MaxLasten = 32;

  { Maximale Energieband-Anzahl }
  C_MaxEnergiebaender = 8;

  { Flags }
  C_Laks_PVerf_Negativ = $20;

  { Formate der LAKS-Parameter-Strings }

  fp_FKZ = 1;      { Festkommazahl: Anzahl der Nachkommastellen in Parameter "Kommastellen",
                                    kein Dezimalzeichen, keine führenden Nullen }
  fp_NK2 = 2;      { 2 Nachkommastellen fest, kein Dezimalzeichen, führende Nullen }
  fp_NK4 = 4;      { 4 Nachkommastellen fest, kein Dezimalzeichen, führende Nullen }


Function Decode_LAKSRealStr(S: string; var Wert: single): boolean;
Function ParaWertStr(Wert: string; ParaNr: integer; Format: integer; Komma: integer;
                     BWErfassung_ueber_AE: boolean): string;
function GetParaFormat (ParaNr: integer): integer;
Function BinStrToHexStr(BinStr: string): string;
function GetTeilStr(p: pChar; Trunc: pChar; Before: word): string;
function Get_ParaAltNeuWertString(Antwort: string;
                                  var Altwert: string; var Neuwert: string): boolean;
function GetLeistungEinheit(anEinheit: string): string;

IMPLEMENTATION

Const
  C_Dezimalzeichen = ',';

{ Erzeugt aus der Einheit für Arbeit die Einheit für Leistung    }
{ Parameter: string mit der Einheit für Arbeit                   }
{ Rückgabe: string mit der Einheit für Leistung                  }
{----------------------------------------------------------------}
function GetLeistungEinheit(anEinheit: string): string;
{----------------------------------------------------------------}
begin
  { Dimension für Leistung: }
  if Pos('3', anEinheit) <> 0 then    { z.B. bei m3, cm3 ("Mengen-Einheiten") }
    result:= anEinheit + '/h'
  else                        { z.B. Wh, kWh, MWh, kvarh ("Energie-Einheiten") }
    result:= StrFilter(anEinheit,'h');
end;

{----------------------------------}
Function NAN(Wert: single): boolean;
{----------------------------------}
var
  S: string;
begin
  Str(Wert,S);
  Result:=UpperCase (StrFilter(S,' ')) = 'NAN';
end;

{----------------------------------------------------------------}
Function Decode_LAKSRealStr(S: string; var Wert: single): boolean;
{----------------------------------------------------------------}
{ Kanalwerte des LAKS-Kurzzeitarchiv und Meßwertarchiv decodieren:
  -> 32 Bit (8 Blöcke mit je 4 Bit)
  Block 8 enthält die 4 höherwertigsten, Block 1 die 4 niederwertigsten Bit;
  S[1] -> Block 8, S[2] -> Block 7 usw. ;
  Reihenfolge Bitmanipulation der Blöcke: 8, 87, 876 ... 87654321
  Übergabe: Rohwert als String
  Rückgeb: decodierter Wert als Single
  Ergebnis: true, wenn Decodierung ok }
var
  i: byte;
  L: integer;
  PInteger: ^integer;
  P: pointer;

begin
  L := Byte (S[1]) AND $0F;
  for i:=2 to 8 do
    L := (L SHL 4) OR (Byte (S[i]) AND $0F);
  New (PInteger);
  try
    PInteger^:=L;
    P:=PInteger;
    Wert:=single(P^);
  finally
    Dispose (PInteger);
  end;
  Result:=not NAN(Wert);
end;

{--------------------------------------}
Function NullString(S: string): boolean;
{--------------------------------------}
{ true, wenn S = 0.0, 0.00..., -0.0, -0.0... }
var
  i: byte;
begin
  Result:=true;
  i:=1;
  while i <= Length(S) do begin
    if (S[i] <> '0') AND (S[i] <> C_Dezimalzeichen) then begin
      Result:=false;
      Break;
    end;
    inc(i);
  end;
end;

{--------------------------------------------------------------------}
Function InsertDezimal(WertStr: string; Dezimalstellen: byte): string;
{--------------------------------------------------------------------}
{ Dezimalzeichen in Wertstring einfügen;
  Übergabe: WertStr
            Dezimalstellen (Nachkomma)
  Ergebnis: WertStr mit eingefügtem Dezimalzeichen }
var
  L: integer;
  i: byte;
  S, Svor, Snach, Serg: string;
  Vorzeichen: string[1];
  Null: boolean;

begin
  Result:=WertStr;
  try
    StrToInt(WertStr);                        { Prüfung, ob WertStr in Zahlenwert wandelbar }
  except
    exit;
  end;

  if (WertStr[1] = '+') OR (WertStr[1] = '-') then begin          { Vorzeichen in WertStr ? }
    S:=Copy(WertStr,2,length(WertStr));
    Vorzeichen:=WertStr[1];
  end
  else begin
    S:=WertStr;
    Vorzeichen:='';
  end;
  L:=length(S);

  if Dezimalstellen = 0 then exit;
  if Dezimalstellen < L then begin
    Svor:=copy(S, 1, L - Dezimalstellen);
    Snach:=copy(S, L - Dezimalstellen + 1, L);
    Serg:=Vorzeichen + Svor + C_DezimalZeichen + Snach;
  end
  else begin
    Snach:=S;
    i:=1;
    while i <= Dezimalstellen - L do begin
      Snach:='0' + Snach;
      inc(i);
    end;
    Serg:=Vorzeichen + '0' + C_Dezimalzeichen + Snach;
  end;

  Null:=NullString(S);                                     { 0.0, 0.00..., -0.0, -0.0... ? }
  if Null AND (Vorzeichen <> '') then
    Serg:=copy(Serg,2,length(Serg));                                     { ohne Vorzeichen }

  Result:=Serg;
end;

{------------------------------------------------------------------------}
Function Zerlege_ParaNr(ParaNr: integer; var HG: integer; var UG: integer;
                        var Nr: integer): boolean;
{------------------------------------------------------------------------}
{ LAKS-Parameternummer in Hauptgruppe, Untergruppe und Nummer zerlegen;
  Übergabe: LAKS-Parameternummer
  Rückgabe: Hauptgruppe, Untergruppe, Nr
  Ergebnis:  true, wenn Zerlegen ok }
var
  code: integer;
  S: string;
begin
  Result:=false;
  HG:=-1; UG:=-1; Nr:=-1;
  S:=Format ('%.6d', [ParaNr]);
  Val(Copy(S,1,2), HG, code);
  if code <> 0 then exit;
  Val(Copy(S,3,2), UG, code);
  if code <> 0 then exit;
  Val(Copy(S,5,2), Nr, code);
  if code <> 0 then exit;
  Result:=true;
end;

{----------------------------------------------------------------------------------}
Function ParaWertStr(Wert: string; ParaNr: integer; Format: integer; Komma: integer;
                     BWErfassung_ueber_AE: boolean): string;
{----------------------------------------------------------------------------------}
{ Parameterwert formatieren (Dezimalzeichen einfügen);
  Übergabe: Wert (Parameterrohwert)
            Nr. des Parameters
            Format des Parameters (fp_FKZ etc.)
            Kommastellen (Nachkomma)
            BWErfassung_ueber_AE (true, wenn Parameter von Gerät stammt, das eine Brennwert-
            erfassung über Analogeingang macht -> Sonderfall )
  Ergebnis: formatierter Wertstring }
var
  S: string;
  HG,UG,Nr: integer;
begin
  case Format of
    fp_FKZ: begin
              if Zerlege_ParaNr (ParaNr,HG,UG,Nr) then begin
                { Spezialfall: Werte für Analogeingang 1 bei Brennwerterfassung }
                if (HG = HG_AnalogEingPara) AND (UG = UG_AnalogEing1Para) AND
                    BWErfassung_ueber_AE then begin
                  S:=F_LeftPad (Wert, '0', 6);
                  S:=F_TruncNumStr(copy(S,1,2)+C_Dezimalzeichen+copy(S,3,4));
                end else                                                    { Standardfall }
                  S:=InsertDezimal(Wert, Komma);
              end else
                S:=Wert;
            end;
    fp_NK2: S:=F_TruncNumStr(copy(Wert,1,length(Wert)-2)+C_Dezimalzeichen+
                             copy(Wert,length(Wert)-1,2));
    fp_NK4: S:=F_TruncNumStr(copy(Wert,1,length(Wert)-4)+C_Dezimalzeichen+
                                  copy(Wert,length(Wert)-3,4));
  else
    S:=Wert;
  end;
  Result:=S;
end;

{------------------------------------------------}
function GetParaFormat (ParaNr: integer): integer;
{------------------------------------------------}
{ liefert für ParaNr das Format eines LAKS-Parameters }
type
  TMomParameter = record
                    Nr: integer;                                          { Parameternummer }
                    Format: integer;                                   { Format: fp_FKZ etc.}
                  end;
Const
  ParameterFormatInfo: array[1..16] of TMomParameter =
    ((Nr: P_Datum; Format: 0),
     (Nr: P_Zeit; Format: 0),
     (Nr: P_Pmom_Summe1; Format: fp_FKZ),
     (Nr: P_Wist_Summe1; Format: fp_FKZ),
     (Nr: P_Restzeit; Format: 0),
     (Nr: P_Pverf_Summe1; Format: fp_FKZ),
     (Nr: P_Pziel_Summe1; Format: fp_FKZ),
     (Nr: P_Pmax_Summe1; Format: fp_FKZ),
     (Nr: P_AktMaximumZone; Format: 0),
     (Nr: P_ZFBreite1; Format: 0),
     (Nr: P_ZFBreite2; Format: 0),
     (Nr: P_ZFBreite3; Format: 0),
     (Nr: P_ZFBreite4; Format: 0),
     (Nr: P_Messperiode; Format: 0),
     (Nr: P_Kommastellen; Format: 0),
     (Nr: P_EinheitSumme1; Format: 0)
    );
var
  i: integer;
begin
  Result:=0;
  for i:=Low (ParameterFormatInfo) to High (ParameterFormatInfo) do begin
    if ParameterFormatInfo[i].Nr = ParaNr then begin
      Result:=ParameterFormatInfo[i].Format;
      Break;
    end;
  end;
end;

{----------------------------------------------}
Function BinStrToHexStr(BinStr: string): string;
{----------------------------------------------}
{ wandelt einen Binär-String in Hex-String um, z.B. '00011101' -> '1D' }
var
  S, HexStr: string;
  i: integer;
  count: integer;

begin
  S:='';
  HexStr:='';
  count:=0;
  for i:=length(BinStr) downto 1 do begin
    S:=BinStr[i] + S;
    inc(count);
    if (count MOD 4 = 0) OR (i = 1) then begin
      S:=F_LeftPad (S,'0',4);
      if (S[1]='0') AND (S[2]='0') AND (S[3]='0') AND (S[4]='0') then HexStr:='0' + HexStr;
      if (S[1]='0') AND (S[2]='0') AND (S[3]='0') AND (S[4]='1') then HexStr:='1' + HexStr;
      if (S[1]='0') AND (S[2]='0') AND (S[3]='1') AND (S[4]='0') then HexStr:='2' + HexStr;
      if (S[1]='0') AND (S[2]='0') AND (S[3]='1') AND (S[4]='1') then HexStr:='3' + HexStr;
      if (S[1]='0') AND (S[2]='1') AND (S[3]='0') AND (S[4]='0') then HexStr:='4' + HexStr;
      if (S[1]='0') AND (S[2]='1') AND (S[3]='0') AND (S[4]='1') then HexStr:='5' + HexStr;
      if (S[1]='0') AND (S[2]='1') AND (S[3]='1') AND (S[4]='0') then HexStr:='6' + HexStr;
      if (S[1]='0') AND (S[2]='1') AND (S[3]='1') AND (S[4]='1') then HexStr:='7' + HexStr;
      if (S[1]='1') AND (S[2]='0') AND (S[3]='0') AND (S[4]='0') then HexStr:='8' + HexStr;
      if (S[1]='1') AND (S[2]='0') AND (S[3]='0') AND (S[4]='1') then HexStr:='9' + HexStr;
      if (S[1]='1') AND (S[2]='0') AND (S[3]='1') AND (S[4]='0') then HexStr:='A' + HexStr;
      if (S[1]='1') AND (S[2]='0') AND (S[3]='1') AND (S[4]='1') then HexStr:='B' + HexStr;
      if (S[1]='1') AND (S[2]='1') AND (S[3]='0') AND (S[4]='0') then HexStr:='C' + HexStr;
      if (S[1]='1') AND (S[2]='1') AND (S[3]='0') AND (S[4]='1') then HexStr:='D' + HexStr;
      if (S[1]='1') AND (S[2]='1') AND (S[3]='1') AND (S[4]='0') then HexStr:='E' + HexStr;
      if (S[1]='1') AND (S[2]='1') AND (S[3]='1') AND (S[4]='1') then HexStr:='F' + HexStr;
      S:='';
      count:=0;
    end;
  end; { for }
  Result:=HexStr;
end;

{----------------------------------------------------------------}
function GetTeilStr(p: pChar; Trunc: pChar; Before: word): string;
{----------------------------------------------------------------}
var
  DestLen: cardinal;
  Dest: pChar;

begin
  Result:='';
  DestLen := StrLen (p) + 1;
  GetMem(Dest, DestLen);
  try
    if FilterString (Dest, p, Trunc, Trunc, nil, Before) <> nil then Result:=string(Dest);
  finally
    FreeMem (Dest, DestLen);
  end;
end;

{-----------------------------------------------------------------------------------}
function Get_ParaAltNeuWertString(Antwort: string;
                                  var Altwert: string; var Neuwert: string): boolean;
{-----------------------------------------------------------------------------------}
{ Antwort auf Parametereinstellen auswerten
  Ergebnis: true, wenn alter und neuer Parameterwert in Antwort enthalten sind }
begin
  Altwert:='';
  Neuwert:='';
  if length(Antwort) > 4 then begin
    Altwert:=F_Zerlegen(Antwort,US);
    Altwert:=copy(Altwert,9,length(Altwert));
    Neuwert:=copy(Antwort,1,length(Antwort)-2);                             { ohne ETX, BCC }
    Result:=true;
  end else
    Result:=false;
end;

End.
