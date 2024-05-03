{******************************************************************************}
{* Unit: SMS-Daten eines MRG decodieren                                       *}
{*       -> MRG-Gerätetyp ermitteln, von dem die SMS stammt                   *}
{*       -> SMS-Dateninhalt in Klartext ausgeben                              *}
{* 09.11.2004 WW                                                              *}
{* 23.06.2008 WW  Bugfix Integerüberlauf bei Veribox SMS (Datum in Rohdaten)  *}
{******************************************************************************}
unit SMSDecode;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UnixDT, T_Tools, WChars, WSysCon, WStrUtils, T_Zeit;

function CheckSMSData_Geraetetyp (SMS_Data: string): integer;
function DecodeSMSData_VeriboxMini (SMSData: string;
                                    bKanal1_Imp: boolean; bKanal2_Imp: boolean;
                                    bKanal3_Imp: boolean; bKanal4_Imp: boolean): string;
function DecodeSMSData_MRG900 (SMSData: string): string;

implementation

const
  Cungueltig = 'ungültig';


{-----------------------------------------------------------}
function CheckSMSData_Geraetetyp (SMS_Data: string): integer;
{-----------------------------------------------------------}
{ prüft, von welchem MRG-Gerätetyp die SMS-Daten stammen;
  Ergebnis: -1   -> unbekannter Gerätetyp
            >= 0 -> Gerätetypnummer lt. Definition }
var
  S: string;
  IBuf: Int64;
  OK: boolean;

begin
  Result:=-1;  { Vorbelegung: unbekannter Gerätetyp }
  { prüfen, ob SMS von einem MRG 905/910 stammt: }
  if length (SMS_Data) >= 1 then begin   { Prüfen des ersten Bytes }
    if (SMS_Data[1] = 'r') OR (SMS_Data[1] = 'v') then  begin   // Meldungs- oder Messwert-SMS, neues Format
      Result:=mrgtyp_MRG910;  // Universal-Gerätetyp für SMS aller MRGs der 900er-Serie
      exit;
    end;
  end;

  { prüfen, ob SMS von einer Veribox-Mini stammt: }
  if length (SMS_Data) >= 12 then begin   { Prüfen der ersten 12 Bytes }
    OK:=Basis64CodeStrToInt (SMS_Data[1], IBuf);
    if OK AND (IBuf >= 1) then begin  { 1. Byte: MessageTyp im Bereich 1.. }
      S:=Copy (SMS_Data, 2, 4);       { 2.-5. Byte: Seriennummer im Bereich 1..16777215 }
      OK:=Basis64CodeStrToInt (S, IBuf);
      if OK AND (IBuf >= 1) AND (IBuf <= 16777215) then begin
        S:=Copy (SMS_Data, 6, 6);     { 6.-11. Byte: Sekunden ab 1.1.2000 }
        OK:=Basis64CodeStrToInt (S, IBuf);
        if OK AND (IBuf >= 0) then begin
          OK:=Basis64CodeStrToInt (SMS_Data[12], IBuf);
          if OK AND (IBuf >= 0) then  { 12. Byte: Batteriezustand im Bereich 0.. }
            Result:=mrgtyp_Veribox_Mini;
        end;
      end;
    end;
  end;
end;


{--------------------------- Veribox Mini -------------------------------------}

{--------------------------------------------------------------------------------------}
function DecodeSMSData_VeriboxMini (SMSData: string;
                                    bKanal1_Imp: boolean; bKanal2_Imp: boolean;
                                    bKanal3_Imp: boolean; bKanal4_Imp: boolean): string;
{--------------------------------------------------------------------------------------}
const
  CMaxKanaele_Veribox = 6;
  { ab Vs. 6.1: max. 6 auswertbare Bits in der Kanalinfo (4 Impuls/2 Analog)
    -> solange nur Impulskanäle im Gerät aktiviert sind, werden von der Konvertierung
       auch Versionen < 6.1 (3 Impuls/1 Analog) unterstützt.
    -> Ab Vs. 7.0 ist der hinzugekommene 3. Analogkanal (Kanal 7) im Gerät unbedingt
       zu deaktivieren ! Grund: Es sind nur 6 auswertbare Bits in der Kanalinfo
       vorhanden, die Daten des 3. Analogkanals werden aber trotzdem mitgeliefert.
       Die SMS-Daten können somit nicht korrekt interpretiert werden ! }
       
  CAnalogKanaeleAb    = 5;    { Nummer des ersten Analogkanals }

  CByteLenIstwertImpuls    = 6;
  CByteLenZeitwertImpuls   = 2;
  CByteLenSchalter         = 1;
  CByteLenAnalog           = 2;

var
  S: string;
  OK: boolean;
  IBuf: Int64;
  sec_BezugIstWerte: cardinal;
  sec_ZeitWerte: cardinal;
  dt_offset: TDateTime;
  sec_offset: cardinal;
  Kanaele_IstWerte: byte;
  Kanaele_ZeitWerte: byte;
  Maske: byte;
  Pos: integer;
  Pos_merk: integer;
  i: integer;
  Speicherintervall_min: integer;
  DatumZeit: TDateTime;
  SBuf: string;
  LenRohwert: integer;

begin
  Result:='';
  if length (SMSData) = 0 then exit;

  { Rohdaten: }
  S:=Copy (SMSData, 1, 1);   { 1. Byte: MessageTyp }
  if length (S) = 1 then
    OK:=Basis64CodeStrToInt (S, IBuf)
  else
    OK:=false;
  if OK then
    Result:=Result + 'MessageTyp: ' + IntToStr (IBuf) + CR + LF
  else begin
    Result:=Result + 'MessageTyp: ' + Cungueltig + CR + LF;
    exit;
  end;

  { nur SMS-Daten vom MessageTyp 2 (= Datensendung an Zentrale) können
    konvertiert werden: }
  if IBuf <> 2 then exit;

  S:=Copy (SMSData, 2, 4);   { 2.-5. Byte: Seriennummer }
  if length (S) = 4 then
    OK:=Basis64CodeStrToInt (S, IBuf)
  else
    OK:=false;
  if OK then
    Result:=Result + 'Seriennummer: ' + IntToStr (IBuf) + CR + LF
  else begin
    Result:=Result + 'Seriennummer: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=Copy (SMSData, 6, 6);   { 6.-11. Byte: Bezugszeit für Ist-Werte (Sekunden ab 1.1.2000) }
  if length (S) = 6 then
    OK:=Basis64CodeStrToInt (S, IBuf)
  else
    OK:=false;
  if not OK then begin
    Result:=Result +  'Datum/Zeit: ' + Cungueltig + CR + LF;
    exit;
  end;
  sec_BezugIstWerte:=IBuf;

  { Sekunden von 1.1.1970 bis 1.1.2000 berechnen (wird als Offset zur
    Berechnung der Zeitstempel benötigt: }
  dt_offset:=EncodeDate (2000, 1, 1);
  sec_offset:=GetUnixSekundenFromDateTime (dt_offset);
  { Zeitstempel: }
  UnixSekundenToDateTime (sec_BezugIstWerte + sec_offset, DatumZeit);
  Result:=Result +  'Datum/Zeit: ' + FormatDateTime ('dd.mm.yyyy hh:nn:ss', DatumZeit) + CR + LF;

  S:=Copy (SMSData, 12, 1);   { 12. Byte: Batteriezustand }
  if length (S) = 1 then
    OK:=Basis64CodeStrToInt (S, IBuf)
  else
    OK:=false;
  if OK then
    Result:=Result + 'Batteriezustand: ' + IntToStr (IBuf) + CR + LF
  else begin
    Result:=Result + 'Batteriezustand: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=Copy (SMSData, 13, 1);   { 13. Byte: Prüfzeichen }
  if length (S) = 1 then
    OK:=Basis64CodeStrToInt (S, IBuf)
  else
    OK:=false;
  if OK then
    Result:=Result + 'Prüfzeichen: ' + IntToStr (IBuf) + CR + LF
  else begin
    Result:=Result + 'Prüfzeichen: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=Copy (SMSData, 14, 1);   { 14. Byte: Kanalinfo Ist-Werte }
  if length (S) = 1 then
    OK:=Basis64CodeStrToInt (S, IBuf)
  else
    OK:=false;
  if OK then
    Result:=Result + 'Ist-Werte vorhanden: ' + IntToStr (IBuf) + CR + LF
  else begin
    Result:=Result + 'Ist-Werte vorhanden: ' + Cungueltig + CR + LF;
    exit;
  end;
  Kanaele_IstWerte:=IBuf;

  Pos:=15;   { Ist-Werte der aktiven Kanäle beginnen mit dem 15. Byte }
  Maske:=1;
  SBuf:='Ist-Werte: ';
  for i:=1 to CMaxKanaele_Veribox do begin
    Application.ProcessMessages;
    if (Kanaele_IstWerte AND Maske) <> 0 then begin   { Bit gesetzt -> Kanal ist aktiv }

      if (i = 1) AND not bKanal1_Imp then
        LenRohwert:=CByteLenSchalter
      else if (i = 2) AND not bKanal2_Imp then
        LenRohwert:=CByteLenSchalter
      else if (i = 3) AND not bKanal3_Imp then
        LenRohwert:=CByteLenSchalter
      else if (i = 4) AND not bKanal4_Imp then
        LenRohwert:=CByteLenSchalter
      else if (i >= CAnalogKanaeleAb) AND (i <= CMaxKanaele_Veribox) then  { Analogkanäle }
        LenRohwert:=CByteLenAnalog
      else
        LenRohwert:=CByteLenIstWertImpuls;

      { Rohwert ist Zählerstand (Impulskanal) oder Binär-Zustand (Schalterkanal) oder
        Analogwert (Analogkanal) }
      S:=Copy (SMSData, Pos, LenRohwert);
      if length (S) = LenRohwert then
        OK:=Basis64CodeStrToInt (S, IBuf)
      else
        OK:=false;

      if i < CAnalogKanaeleAb then  // Impulskanal
        SBuf:=SBuf + 'I'+ IntToStr (i) + ': '
      else  // Analogkanal
        SBuf:=SBuf + 'A'+ IntToStr (i + 1 - CAnalogKanaeleAb) + ': ';

      if OK then
        SBuf:=SBuf + IntToStr (IBuf) + '  '
      else begin
        SBuf:=SBuf + Cungueltig + '  ';
        Result:=Result + SBuf + CR + LF;
        exit;
      end;
      inc (Pos, LenRohwert);
    end;
    Maske:=Maske SHL 1;
  end;
  if Pos > 15 then  { Ist-Werte sind in den Rohdaten vorhanden }
    Result:=Result + SBuf + CR + LF
  else
    Result:=Result + SBuf + 'keine' + CR + LF;

  { Zeitwerte der aktiven Kanäle: }
  S:=Copy (SMSData, Pos, 1);    { Kanalinfo Zeit-Werte (1 Byte) }
  if length (S) = 1 then
    OK:=Basis64CodeStrToInt (S, IBuf)
  else
    OK:=false;
  if OK then
    Result:=Result + 'Zeitwerte vorhanden: ' + IntToStr (IBuf) + CR + LF
  else begin
    Result:=Result + 'Zeitwerte vorhanden: ' + Cungueltig + CR + LF;
    exit;
  end;
  Kanaele_ZeitWerte:=IBuf;
  inc (Pos);

  S:=Copy (SMSData, Pos, 2);  { Speicherintervall (2 Bytes) }
  if length (S) = 2 then
    OK:=Basis64CodeStrToInt (S, IBuf)
  else
    OK:=false;
  if OK then
    Result:=Result + 'Speicherintervall: ' + IntToStr (IBuf) + CR + LF
  else begin
    Result:=Result + 'Speicherintervall: ' + Cungueltig + CR + LF;
    exit;
  end;
  Speicherintervall_min:=IBuf;   { Speicherintervall in min }
  inc (Pos, 2);

  S:=Copy (SMSData, Pos, 1);    { Anzahl Zeitwertblöcke (1 Byte) }
  if length (S) = 1 then
    OK:=Basis64CodeStrToInt (S, IBuf)
  else
    OK:=false;
  if OK then
    Result:=Result + 'Anzahl Zeitwertblöcke: ' + IntToStr (IBuf) + CR + LF
  else begin
    Result:=Result + 'Anzahl Zeitwertblöcke: ' + Cungueltig + CR + LF;
    exit;
  end;
  inc (Pos);

  sec_Zeitwerte:=sec_BezugIstWerte + sec_offset;    { Vorbelegung }
  while Pos <= length (SMSData) do begin
    Pos_merk:=Pos;
    { Zeitstempel: }
    inc (sec_ZeitWerte, Speicherintervall_min * 60);
    UnixSekundenToDateTime (sec_ZeitWerte, DatumZeit);

    SBuf:='Zeitwert: ' + FormatDateTime ('dd.mm.yyyy hh:nn:ss', DatumZeit) + '  ';
    Maske:=1;
    for i:=1 to CMaxKanaele_Veribox do begin
      Application.ProcessMessages;
      if (Kanaele_ZeitWerte AND Maske) <> 0 then begin   { Bit gesetzt -> Kanal ist aktiv }

        if (i = 1) AND not bKanal1_Imp then
          LenRohwert:=CByteLenSchalter
        else if (i = 2) AND not bKanal2_Imp then
          LenRohwert:=CByteLenSchalter
        else if (i = 3) AND not bKanal3_Imp then
          LenRohwert:=CByteLenSchalter
        else if (i = 4) AND not bKanal4_Imp then
          LenRohwert:=CByteLenSchalter
        else if (i >= CAnalogKanaeleAb) AND (i <= CMaxKanaele_Veribox) then  { Analogkanäle }
          LenRohwert:=CByteLenAnalog
        else
          LenRohwert:=CByteLenZeitWertImpuls;

        { Rohwert ist Inkrementwert (Impulskanal) oder Binär-Zustand (Schalterkanal) oder
          Analogwert (Analogkanal) }
        S:=Copy (SMSData, Pos, LenRohwert);
        if length (S) = LenRohwert then
          OK:=Basis64CodeStrToInt (S, IBuf)
        else
          OK:=false;

        if i < CAnalogKanaeleAb then  // Impulskanal
          SBuf:=SBuf + 'I'+ IntToStr (i) + ': '
        else  // Analogkanal
          SBuf:=SBuf + 'A'+ IntToStr (i + 1 - CAnalogKanaeleAb) + ': ';

        if OK then
          SBuf:=SBuf + IntToStr (IBuf) + '  '
        else begin
          SBuf:=SBuf + Cungueltig + '  ';
          Result:=Result + SBuf + CR + LF;
          exit;
        end;
        inc (Pos, LenRohwert);
      end;
      Maske:=Maske SHL 1;
    end;
    if Pos > Pos_merk then   { Zeit-Werte sind in den Rohdaten vorhanden }
      Result:=Result + SBuf + CR + LF
    else if Pos = Pos_merk then begin   { wenn Kanaele_Zeitwerte = 0 und Rest-Rohdaten vorhanden sind }
      SBuf:=SBuf + 'Alle ungültig';
      Result:=Result + SBuf + CR + LF;
      exit;
    end else
      Result:=Result + 'keine' + CR + LF;
  end;  { while Pos }
end;


{--------------------------- MRG 910 ------------------------------------------}

{-------------------------------------------------------------}
function DecodeSMSMesswerte_MRG910_w (SMSData: string): string;
{-------------------------------------------------------------}
{ MRG 910-Messwert-SMS, altes Format w }
const
  C_MWDatenArtSet = ['1'..'4', 'A'..'D', 'Q'..'T', 'a'..'d'];

  CByteLenZaehler = 5;
  CByteLenMPWert  = 4;

var
  S: string;
  SBuf: string;
  OK: boolean;
  IBuf: Int64;
  Pos: integer;
  Pos_merk: integer;
  i: integer;
  DatumZeit: TDateTime;
  AnzZaehler: integer;
  cArt: char;
  sec_Bezug: integer;
  sec_Werte: integer;
  Messperiodenlaenge_min: integer;
  LenRohwert: integer;
  nur_AktEingangs_Kontrollzaehler: boolean;
  nur_AktKontrollzaehler: boolean;
  mit_Eingangszaehler: boolean;
  sWertname: string;

begin
  Result:='';

  S:=Copy (SMSData, 2, 14);   { 2.-15. Byte: Kennung }
  OK:=length (S) = 14;
  if OK then
    Result:=Result + 'Kennung: ' + S + CR + LF
  else begin
    Result:=Result + 'Kennung: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=Copy (SMSData, 16, 6);   { 16.-21. Byte: Zeit des ältesten Eintrags (Unix-Zeit) }
  if length (S) = 6 then
    OK:=Basis64CodeStrToInt (S, IBuf)
  else
    OK:=false;
  if not OK then begin
    Result:=Result +  'Datum/Zeit: ' + Cungueltig + CR + LF;
    exit;
  end;
  sec_Bezug:=IBuf;

  { Zeitstempel: }
  UnixSekundenToDateTime (sec_Bezug, DatumZeit);
  Result:=Result +  'Datum/Zeit: ' + FormatDateTime ('dd.mm.yyyy hh:nn:ss', DatumZeit) + CR + LF;

  S:=Copy (SMSData, 22, 1);   { 22. Byte: Art }
  OK:=length (S) = 1;
  if OK then
    OK:=S[1] in C_MWDatenArtSet;  // auf gültige Art prüfen
  if OK then begin
    Result:=Result + 'Art: ' + S + CR + LF;

    cArt:=S[1];
    AnzZaehler:=Ord (cArt) AND $0F;  // Bit 0..3 interressiert nur
  end
  else begin
    Result:=Result + 'Art: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=Copy (SMSData, 23, 1);   { 23. Byte: Messperiodenlänge }
  if length (S) = 1 then
    OK:=Basis64CodeStrToInt (S, IBuf)
  else
    OK:=false;
  if OK then
    Result:=Result + 'Messperiodenlänge: ' + IntToStr (IBuf) + CR + LF
  else begin
    Result:=Result + 'Messperiodenlänge: ' + Cungueltig + CR + LF;
    exit;
  end;
  Messperiodenlaenge_min:=IBuf;   { Messperiodenlänge in min }

  nur_AktKontrollzaehler:=cArt in ['Q'..'T'];  // es folgen nur aktuelle Kontrollzähler
  nur_AktEingangs_Kontrollzaehler:=cArt in ['a'..'d'];  // es folgen nur aktuelle Eingangs- und Kontrollzähler

  Pos:=24;   { aktuelle Zähler (Eingangs- oder Kontroll) beginnen mit dem 24. Byte }
  if not nur_AktKontrollzaehler then begin
    // aktuelle Eingangszähler
    for i:=1 to AnzZaehler do begin
      Application.ProcessMessages;

      S:=Copy (SMSData, Pos, CByteLenZaehler);
      if length (S) = CByteLenZaehler then
        OK:=Basis64CodeStrToInt (S, IBuf)
      else
        OK:=false;
      if OK then
        Result:=Result + 'Aktueller Eingangszähler ' + IntToStr (i) + ': ' + IntToStr (IBuf) + CR + LF
      else begin
        Result:=Result + 'Aktueller Eingangszähler ' + IntToStr (i) + ': ' + Cungueltig + CR + LF;
        exit;
      end;

      inc (Pos, CByteLenZaehler);
    end;
  end;

  // aktuelle Kontrollzähler
  for i:=1 to AnzZaehler do begin
    Application.ProcessMessages;

    S:=Copy (SMSData, Pos, CByteLenZaehler);
    if length (S) = CByteLenZaehler then
      OK:=Basis64CodeStrToInt (S, IBuf)
    else
      OK:=false;
    if OK then
      Result:=Result + 'Aktueller Kontrollzähler ' + IntToStr (i) + ': ' + IntToStr (IBuf) + CR + LF
    else begin
      Result:=Result + 'Aktueller Kontrollzähler ' + IntToStr (i) + ': ' + Cungueltig + CR + LF;
      exit;
    end;

    inc (Pos, CByteLenZaehler);
  end;  // for i

  if nur_AktEingangs_Kontrollzaehler OR nur_AktKontrollzaehler then begin
    // es darf nichts mehr kommen
    if Pos < length (SMSData) then  // es kommt doch noch was
      Result:=Result + 'Alle ungültig' + CR + LF;
    exit;
  end;

  if cArt in ['1'..'4'] then begin  // es folgen Messperiodenwerte
    LenRohwert:=CByteLenMPWert;  // Länge für Messperioden-Rohwert
    mit_Eingangszaehler:=false;
    sWertname:='MP-Z ';  // Messperiodenzähler
  end
  else if cArt in ['A'..'D'] then begin  // es folgen Eingangs- und Kontrollzähler
    LenRohwert:=CByteLenZaehler;  // Länge für Eingangs-/Kontrollzähler-Rohwert
    mit_Eingangszaehler:=true;
    sWertname:='Kontr.Z ';  // Kontrollzähler
  end else
    exit;

  sec_Werte:=sec_Bezug;    { Vorbelegung }
  while Pos <= length (SMSData) do begin
    Pos_merk:=Pos;
    { Zeitstempel: }
    UnixSekundenToDateTime (sec_Werte, DatumZeit);
    SBuf:=FormatDateTime ('dd.mm.yyyy hh:nn:ss', DatumZeit) + '  ';

    // Status-Bit
    S:=Copy (SMSData, Pos, 1);
    OK:=length (S) = 1;
    if OK then
      SBuf:=SBuf + 'Status: ' + S + '  '
    else begin
      SBuf:=SBuf + 'Status: ' + Cungueltig + '  ';
      Result:=Result + SBuf + CR + LF;
      exit;
    end;
    inc (Pos, 1);

    // Werte
    for i:=1 to AnzZaehler do begin
      Application.ProcessMessages;

      if mit_Eingangszaehler then begin  // zusätzliche Werteart: Eingangszähler
        S:=Copy (SMSData, Pos, LenRohwert);
        if length (S) = LenRohwert then
          OK:=Basis64CodeStrToInt (S, IBuf)
        else
          OK:=false;
        if OK then
          SBuf:=SBuf + 'Eing.Z ' + IntToStr (i) + ': ' + IntToStr (IBuf) + '  '
        else begin
          SBuf:=SBuf + 'Eing.Z ' + IntToStr (i) + ': ' + Cungueltig + '  ';
          Result:=Result + SBuf + CR + LF;
          exit;
        end;
        inc (Pos, LenRohwert);
      end;

      S:=Copy (SMSData, Pos, LenRohwert);
      if length (S) = LenRohwert then
        OK:=Basis64CodeStrToInt (S, IBuf)
      else
        OK:=false;
      if OK then
        SBuf:=SBuf + sWertname + IntToStr (i) + ': ' + IntToStr (IBuf) + '  '
      else begin
        SBuf:=SBuf + sWertname + IntToStr (i) + ': ' + Cungueltig + '  ';
        Result:=Result + SBuf + CR + LF;
        exit;
      end;
      inc (Pos, LenRohwert);
    end;

    if Pos > Pos_merk then   { Zeit-Werte sind in den Rohdaten vorhanden }
      Result:=Result + SBuf + CR + LF
    else if Pos = Pos_merk then begin   { wenn Kanaele_Zeitwerte = 0 und Rest-Rohdaten vorhanden sind }
      SBuf:=SBuf + 'Alle ungültig';
      Result:=Result + SBuf + CR + LF;
      exit;
    end else
      Result:=Result + 'keine' + CR + LF;

    inc (sec_Werte, Messperiodenlaenge_min * 60);
  end;  { while Pos }
end;

{-----------------------------------------------------------}
function DecodeSMSMeldung_MRG910_w (SMSData: string): string;
{-----------------------------------------------------------}
{ MRG 910-Meldungs-SMS, altes Format w }
const
  CTrennzeichen = ' ';
var
  S: string;
  SBuf: string;
  OK: boolean;
  dt: TDateTime;

begin
  Result:='';

  S:=ExtractString (SMSData, CTrennzeichen, CTrennzeichen, 0);   { zwischen 1. und 2. Trennzeichen: Stationsname }
  OK:=length (S) <= 16;
  if OK then
    Result:=Result + 'Stationsname: ' + S + CR + LF
  else begin
    Result:=Result + 'Stationsname: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=ExtractString (SMSData, CTrennzeichen, CTrennzeichen, 1);   { zwischen 2. und 3. Trennzeichen: Kennung }
  OK:=length (S) <= 14;
  if OK then
    Result:=Result + 'Kennung: ' + S + CR + LF
  else begin
    Result:=Result + 'Kennung: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=ExtractString (SMSData, CTrennzeichen, CTrennzeichen, 2);   { zwischen 3. und 4. Trennzeichen: Datum }
  OK:=EncodeDateStr (S, 'DD.MM.YY', dt);
  if OK then
    Result:=Result + 'Datum: ' + S + CR + LF
  else begin
    Result:=Result + 'Datum: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=ExtractString (SMSData, CTrennzeichen, CTrennzeichen, 3);   { zwischen 4. und 5. Trennzeichen: Zeit }
  OK:=EncodeTimeStr (S, 'HH:MM:SS', dt);
  if OK then
    Result:=Result + 'Zeit: ' + S + CR + LF
  else begin
    Result:=Result + 'Zeit: ' + Cungueltig + CR + LF;
    exit;
  end;

  { Meldungstext und Meldungsnummer kommen nach dem 5. Trennzeichen }
  SBuf:=ExtractString (SMSData, CTrennzeichen, NUL, 4);
  S:=Copy (SBuf, 1, length (SBuf) - 3);  { die letzten 3 Zeichen (Meldungsnummer) weglassen }
  OK:=length (S) > 0;
  if OK then
    OK:=S [length (S)] = CTrennzeichen;
  if OK then
    Result:=Result + 'Meldungstext: ' + S + CR + LF
  else begin
    Result:=Result + 'Meldungstext: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=Copy (SBuf, length (SBuf) - 2, 3);  { die letzten 3 Zeichen sind die Meldungsnummer }
  OK:=length (S) = 3;
  if OK then
    Result:=Result + 'Meldungsnummer: ' + S + CR + LF
  else begin
    Result:=Result + 'Meldungsnummer: ' + Cungueltig + CR + LF;
    exit;
  end;
end;

{-------------------------------------------------------------}
function DecodeSMSMesswerte_MRG900_v (SMSData: string): string;
{-------------------------------------------------------------}
{ MRG 905/910-Messwert-SMS, neues Format v (konvertierbar in DSfG-Archiv) }
var
  S: string;
  SBuf: string;
  OK: boolean;
  IBuf: Int64;
  DatumZeit: TDateTime;
  UnixSec_Kopf: integer;
  UnixSec_MP: integer;
  Messperiodenlaenge_sec: integer;
  OrdnungsNr_Kopf: integer;
  i: integer;
  cSMSData: char;
  cTrennz: char;
  c: char;
  cKontrZ: char;
  sWertTyp: string;
  AktKontrollZaehler: array['p'..'s'] of integer;
  sKopf: string;

begin
  Result:='';

  S:=Copy (SMSData, 2, 12);  { 2.-13. Zeichen: DSfG-Kennung }
  OK:=length (S) = 12;
  if OK then
    Result:=Result + 'DSfG-Kennung: ' + S + CR + LF
  else begin
    Result:=Result + 'DSfG-Kennung: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:='';
  cTrennz:=NUL;
  sKopf:='';
  UnixSec_MP:=-1;
  Messperiodenlaenge_sec:=-1;
  OrdnungsNr_Kopf:=-1;
  for c:=Low (AktKontrollZaehler) To High (AktKontrollZaehler) do
    AktKontrollZaehler [c]:=-1;

  for i:=14 to length (SMSData) do begin   // Reststring ab Ordnungsnummer bis zum Ende
    Application.ProcessMessages;

    cSMSData:=SMSData[i];
    if ((cSMSData in ['p'..'w','z']) AND (length (sKopf) > 9)) OR
       (i = length (SMSData)) then begin  // neues Trennzeichen gefunden oder Stringende

      if i = length (SMSData) then  // bei Stringende letztes Zeichen anhängen
        S:=S + SMSData[i];

      if cSMSData = 'z' then  // es beginnt ein neuer Kopf
        sKopf:='';

      if (cTrennz = NUL) OR (cTrennz = 'z') then begin  // Startsatz oder Sondersatz z
        if cTrennz = 'z' then
          Result:=Result +  'Sondersatz: ' + CR + LF;

        // Zeit, MP-Länge und Ordnungsnummer verarbeiten
        SBuf:=Copy (S, 1, 6);   { 1.-6. Byte: Zeit der Kontrollzähler (Unix-Zeit) }
        if length (SBuf) = 6 then
          OK:=Basis64CodeStrToInt (SBuf, IBuf)
        else
          OK:=false;
        if not OK then begin
          Result:=Result +  'Datum/Zeit: ' + Cungueltig + CR + LF;
          exit;
        end;
        UnixSec_Kopf:=IBuf;

        { Zeitstempel: }
        UnixSekundenToDateTime (UnixSec_Kopf, DatumZeit);
        Result:=Result +  'Datum/Zeit: ' + FormatDateTime ('dd.mm.yyyy hh:nn:ss', DatumZeit) + CR + LF;

        SBuf:=Copy (S, 7, 1);   { 7. Byte: Zeitzonen-Kennzeichen }
        OK:=length(SBuf) = 1;
        if OK then
          Result:=Result + 'Zeitzone: ' + SBuf + CR + LF
        else begin
          Result:=Result + 'Zeitzone: ' + Cungueltig + CR + LF;
          exit;
        end;

        SBuf:=Copy (S, 8, 1);   { 8. Byte: Messperiodenlänge }
        if length (SBuf) = 1 then
          OK:=Basis64CodeStrToInt (SBuf, IBuf)
        else
          OK:=false;
        if OK then
          Result:=Result + 'Messperiodenlänge [min]: ' + IntToStr (IBuf) + CR + LF
        else begin
          Result:=Result + 'Messperiodenlänge [min]: ' + Cungueltig + CR + LF;
          exit;
        end;
        Messperiodenlaenge_sec:=IBuf * 60;   { Messperiodenlänge in sec }
        UnixSec_MP:=UnixSec_Kopf - (UnixSec_Kopf MOD Messperiodenlaenge_sec);  { Unix-Zeit der letzten vollen Messperiode }

        SBuf:=Copy (S, 9, 1);   { 9. Byte: Adresse der Reg.Instanz }
        OK:=length(SBuf) = 1;
        if OK then
          Result:=Result + 'Adresse der Registrierinstanz: ' + SBuf + CR + LF
        else begin
          Result:=Result + 'Adresse der Registrierinstanz: ' + Cungueltig + CR + LF;
          exit;
        end;

        SBuf:=Copy (S, 10, length(S));   { Rest: Ordnungsnummer }
        if length (SBuf) > 0 then
          OK:=Basis64CodeStrToInt (SBuf, IBuf)
        else
          OK:=false;
        if OK then
          Result:=Result + 'Ordnungsnummer: ' + IntToStr (IBuf) + CR + LF
        else begin
          Result:=Result + 'Ordnungsnummer: ' + Cungueltig + CR + LF;
          exit;
        end;
        OrdnungsNr_Kopf:=IBuf;
      end
      else begin
        // Rohwert (Zählerstand, Messperiodenwert) verarbeiten und zuweisen
        if length (S) > 0 then
          OK:=Basis64CodeStrToInt (S, IBuf)
        else
          OK:=false;

        case cTrennz of
          'p': sWertTyp:='Kontrollzähler A: ';    // Trennzeichen Kontrollzähler A
          'q': sWertTyp:='Kontrollzähler B: ';    // Trennzeichen Kontrollzähler B
          'r': sWertTyp:='Kontrollzähler C: ';    // Trennzeichen Kontrollzähler C
          's': sWertTyp:='Kontrollzähler D: ';    // Trennzeichen Kontrollzähler D
          't': sWertTyp:='Messperiodenwert A: ';  // Trennzeichen Messperiodenwert A (Inkrement)
          'u': sWertTyp:='Messperiodenwert B: ';  // Trennzeichen Messperiodenwert B (Inkrement)
          'v': sWertTyp:='Messperiodenwert C: ';  // Trennzeichen Messperiodenwert C (Inkrement)
          'w': sWertTyp:='Messperiodenwert D: ';  // Trennzeichen Messperiodenwert D (Inkrement)
        else
          sWertTyp:='Unbekannter Wert: ';
        end;

        if not OK then begin
          Result:=Result + sWertTyp + Cungueltig + CR + LF;
          exit;
        end;

        if cTrennz in ['p'..'s'] then begin  // Rohwert ist Kontrollzähler
          AktKontrollZaehler [cTrennz]:=IBuf;   // Kontrollzähler neu setzen
          Result:=Result + sWertTyp + IntToStr (AktKontrollZaehler [cTrennz]) + CR + LF;
        end
        else if cTrennz in ['t'..'w'] then begin  // Rohwert ist MP-Wert
          cKontrZ:=Chr (Ord (cTrennz) - 4);
          if AktKontrollZaehler [cKontrZ] < 0 then begin // Fehler: Kontrollzähler nicht gesetzt
            Result:=Result + sWertTyp + IntToStr (IBuf) + #9 +
                    'Kontrollzähler: ' +  Cungueltig + CR + LF;
            exit;
          end
          else begin  // Kontrollzähler fortschreiben durch Aufaddieren des MP-Werts
            AktKontrollZaehler [cKontrZ]:=AktKontrollZaehler [cKontrZ] + IBuf;
            // Ausgabe: MP-Wert und aufaddierter Kontrollzähler
            Result:=Result + sWertTyp + IntToStr (IBuf) + #9 +
                    'Kontrollzähler: ' +  IntToStr (AktKontrollzaehler[cKontrZ]) + CR + LF;
          end;
        end else  // Rohwert ist unbekannt
          Result:=Result + sWertTyp + IntToStr (IBuf) + CR + LF;

        // neue Messperiode beginnt, wenn:
        if (cSMSData in ['t'..'w']) AND  // ...Wert ein Messperiodenwert ist
           (not (cTrennz in ['t'..'w']) OR  // ...es der erste Messperiodenwert ist
            (cSMSData <= cTrennz)) then begin // ...ASCII-Code aktuelles MP-Wert-Trennzeichen <= vorheriges MP-Wert-Trennzeichen
          if (UnixSec_MP < 0) OR (Messperiodenlaenge_sec < 0) OR
             (OrdnungsNr_Kopf < 0) then begin
            // Fehler: UnixSec_MP, Messperiodenlaenge_sec oder RdnungsNr_Kopf
            // enthalten Vorbelegungswerte
            Result:=Result + 'Neue MP: ' + Cungueltig + CR + LF;
            exit;
          end
          else begin
            inc (UnixSec_MP, Messperiodenlaenge_sec);
            UnixSekundenToDateTime (UnixSec_MP, DatumZeit);
            inc (OrdnungsNr_Kopf);
            Result:=Result + 'Neue MP - Datum/Zeit: ' +
                    FormatDateTime ('dd.mm.yyyy hh:nn:ss', DatumZeit) +
                    '  Ordnungsnummer: ' + IntToStr(OrdnungsNr_Kopf) + CR + LF;
          end;
        end;
      end;

      cTrennz:=cSMSData;  // neu gefundenes Trennzeichen setzen
      S:='';  // Rohwert neu initialisieren
    end
    else begin
      S:=S + SMSData[i];
      sKopf:=sKopf + SMSData[i];
    end;
  end;  { for i }
end;

{-----------------------------------------------------------}
function DecodeSMSMeldung_MRG900_r (SMSData: string): string;
{-----------------------------------------------------------}
{ MRG 905/910-Meldungs-SMS, neues Format r (konvertierbar in DSfG-Logbuch-Archiv) }
const
  CTrennzeichen = ' ';
var
  S: string;
  SBuf: string;
  OK: boolean;
  dt: TDateTime;

begin
  Result:='';

  S:=Copy (SMSData, 2, 6);  { 2.-7. Zeichen: Gerätetyp }
  OK:=length (S) = 6;
  if OK then
    Result:=Result + 'Gerätetyp: ' + S + CR + LF
  else begin
    Result:=Result + 'Gerätetyp: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=Copy (SMSData, 9, 16);  { 9.-24. Zeichen: Stationsname }
  OK:=length (S) = 16;
  if OK then
    Result:=Result + 'Stationsname: ' + S + CR + LF
  else begin
    Result:=Result + 'Stationsname: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=Copy (SMSData, 26, 14);  { 26.-39. Zeichen: MRG-Kennung }
  OK:=length (S) = 14;
  if OK then
    Result:=Result + 'MRG-Kennung: ' + S + CR + LF
  else begin
    Result:=Result + 'MRG-Kennung: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=Copy (SMSData, 41, 10);  { 41.-50. Zeichen: Datum }
  OK:=EncodeDateStr (S, 'DD.MM.YYYY', dt);
  if OK then
    Result:=Result + 'Datum: ' + S + CR + LF
  else begin
    Result:=Result + 'Datum: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=Copy (SMSData, 52, 8);  { 52.-59. Zeichen: Zeit }
  OK:=EncodeTimeStr (S, 'HH:MM:SS', dt);
  if OK then
    Result:=Result + 'Zeit: ' + S + CR + LF
  else begin
    Result:=Result + 'Zeit: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=Copy (SMSData, 60, 1);  { 60. Zeichen: Trennzeichen für Zeitzone (optional) oder
                                            Trennzeichen zu Meldungstext }
  if (S = ',') OR (S = ';') then begin  { Zeitzonen-Trennzeichen }
    SBuf:=Copy (SMSData, 62, length (SMSData));  { Rest bis zum Ende }
    S:=F_Zerlegen (SBuf, CTrennzeichen);
    OK:=length (S) > 0;
    if OK then
      Result:=Result + 'Zeitzone: ' + S + CR + LF
    else begin
      Result:=Result + 'Zeitzone: ' + Cungueltig + CR + LF;
      exit;
    end;
  end else
    SBuf:=Copy(SMSData, 61, length (SMSData));  { Rest bis zum Ende }

  S:=Copy (SBuf, 1, 16);  { 1.-16. Zeichen des Reststrings: Meldungstext }
  OK:=length (S) = 16;
  if OK then
    Result:=Result + 'Meldungstext: ' + S + CR + LF
  else begin
    Result:=Result + 'Meldungstext: ' + Cungueltig + CR + LF;
    exit;
  end;

  SBuf:=Copy (SBuf, 18, length (SMSData));  { Rest bis zum Ende }
  S:=F_Zerlegen (SBuf, CTrennzeichen);  { bis zum nächsten Trennzeichen: DSfG-Meldungsnummer }
  OK:=isIntString (S);
  if OK then
    Result:=Result + 'DSfG-Meldungsnummer: ' + S + CR + LF
  else begin
    Result:=Result + 'DSfG-Meldungsnummer: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=F_Zerlegen (SBuf, CTrennzeichen);  { bis zum nächsten Trennzeichen: DSfG-Ordnungsnummer }
  OK:=isIntString (S);  { DSfG-Ordnungsnummer }
  if OK then
    Result:=Result + 'DSfG-Ordnungsnummer: ' + S + CR + LF
  else begin
    Result:=Result + 'DSfG-Ordnungsnummer: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=F_Zerlegen (SBuf, CTrennzeichen);  { bis zum nächsten Trennzeichen: Adresse der Reg.Instanz }
  OK:=length (S) = 1;
  if OK then
    Result:=Result + 'Adresse der Registrierinstanz: ' + S + CR + LF
  else begin
    Result:=Result + 'Adresse der Registrierinstanz: ' + Cungueltig + CR + LF;
    exit;
  end;

  S:=F_Zerlegen (SBuf, CTrennzeichen);  { bis zum nächsten Trennzeichen: Adresse der Wieser-Instanz }
  OK:=length (S) = 1;
  if OK then
    Result:=Result + 'Adresse der Wieser-Instanz: ' + S + CR + LF
  else begin
    Result:=Result + 'Adresse der Wieser-Instanz: ' + Cungueltig + CR + LF;
    exit;
  end;

  OK:=length (SBuf) = 12;  { Rest: DSfG-Kennung }
  if OK then
    Result:=Result + 'DSfG-Kennung: ' + SBuf + CR + LF
  else begin
    Result:=Result + 'DSfG-Kennung: ' + Cungueltig + CR + LF;
    exit;
  end;
end;

{------------------------------------------------------}
function DecodeSMSData_MRG900 (SMSData: string): string;
{------------------------------------------------------}
var
  S: string;
  OK: boolean;
  isMeldungsSMS: boolean;

begin
  Result:='';
  if length (SMSData) = 0 then exit;

  { Rohdaten: }
  S:=Copy (SMSData, 1, 1);   { 1. Byte: Kennzeichen MRG 910 }
  OK:=length (S) = 1;
  if OK then
    Result:=Result + 'Typ: ' + S + CR + LF
  else begin
    Result:=Result + 'Typ: ' + Cungueltig + CR + LF;
    exit;
  end;

  { nur SMS-Daten vom Typ w, v, r können konvertiert werden: }
  if S = 'w' then begin  // MRG 910, altes Format (Messwerte, Meldungen)
    { Unterscheidung Messwert-SMS/Meldungs-SMS:
      -> in der Meldungs-SMS steht im 2.-8. Byte 'MRG910 ' (Space ist Trennzeichen
         zum Stationsnamen) }
    S:=Copy (SMSData, 2, 7);
    isMeldungsSMS:=S = 'MRG910 ';

    if isMeldungsSMS then
      Result:=Result + DecodeSMSMeldung_MRG910_w (SMSData)
    else
      Result:=Result + DecodeSMSMesswerte_MRG910_w (SMSData);
  end
  else if S = 'r' then  // MRG 905/910, neues Format Meldungen
    Result:=Result + DecodeSMSMeldung_MRG900_r (SMSData)
  else if S = 'v' then  // MRG 905/910, neues Format Messwerte
    Result:=Result + DecodeSMSMesswerte_MRG900_v (SMSData);
end;

end.
