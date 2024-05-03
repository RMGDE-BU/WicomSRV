{******************************************************************************}
{* Unit: Konvertierung von SMS-Daten in MRG-Zwischendateien vom Typ RohSRec   *}
{*       (Messwerte) und RohTRec (Tagess�tze)                                 *}
{*       -> alle Zeitstempel tragen das physikalische Datum (MW und TA)       *}
{*       -> Stundenwerte werden in RohSRec nur als Originalwerte geliefert    *}
{*          (keine Berechnung der LGZ-Werte in RohSRec.Kanal[i]...            *}
{* 09.11.2004 WW                                                              *}
{* 17.12.2007 WW  Veribox-Konvertierung mit Analogkan�le                      *}
{* 23.06.2008 WW  Bugfix Integer�berlauf bei Veribox SMS (Datum in Rohdaten)  *}
{* 02.12.2008 WW  Merksatz nur schreiben, wenn Datensatz j�nger               *}
{* 20.01.2009 WW  Veribox-Konvertierung mit Datenl�cke-Pr�fung                *}
{* 25.01.2010 WW  Anpassungen f�r RohTRec-Struktur mit Wert als double        *}
{* 24.03.2010 WN  Plausibilisierung Datum f�r SMS-Zwischendatei               *}
{******************************************************************************}
unit MSMSKonv;

interface

uses
  Forms, Classes, Windows, SysUtils, WStrUtils, WSysCon, LGZType, WStream, UnixDT,
  T_Tools, T_Zeit, novell;

type

  { �bergaberecord f�r SMS-Konvertierung in MRG-Daten }

  TSMSKonvMRG = record
    SMS_Data: string;             { Datenteil einer SMS }
    GeraeteTyp: integer;          { Ger�tetypnummer }
    StdZieldateiName: TFileName;  { vollst�ndiger Name der Zieldatei mit strukturierten Stundens�tzen }
    TagZieldateiName: TFileName;  { vollst�ndiger Name der Zileatei mit strukturierten Tagess�tzen }
    WSystemPfad: string;          { Wieser-System-Pfad (Pfad zur Merkdatei mit gespeicherten
                                    Z�hlerstandss�tzen) }
    dtDatenlueckePruefung_bis: TDateTime;  { PC-Zeitpunkt, bis zu dem die Konvertierung abgebrochen wird, falls
                                             die gemerkten Z�hlerst�nde nicht von der Vorperiode stammen und
                                             somit eine Datenl�cke entst�nde }
  end;

function KonvSMS_MRG (SMSKonvRec: TSMSKonvMRG; var Kennung: string;
  var bDatenlueckePruefungOK: boolean): boolean;

implementation

{$H-}
type
  TMerkTRec = packed record  // 25.01.2010, WW
    satzstatus : byte;
    DatumZeit  : TDateTime;
    E_Zaehler  : Zaehler_typ;
    K_Zaehler  : Zaehler_typ;
  end;

  { Struktur der Merkdatei zum Abspeichern eines Z�hlerstandssatzes einer Kennung }
  TRohTMerk = packed record
    Kennung: string [C_KennungLen];
    TagSatz: TMerkTRec;
  end;
{$H+}

{--------------------------- Hilfsfunktionen ----------------------------------}

{------------------------------------------------------}
function GetTagSatz_MerkFilename (Pfad: string): string;
{------------------------------------------------------}
{ liefert Name der Merkdatei mit gespeicherten Z�hlerstandss�tzen aller konvertierten
  Kennungen;
  �bergabe: Pfad der Merkdatei }
begin
  Result:=Pfad + ChangeFileExt (ExtractFileName (ParamStr (0)), '.TAM');
end;

{--------------------------------------------------------------------}
procedure Save_TagSatz_To_MerkFile (Kennung: string; SaveRec: RohTRec;
                                    WSystemPfad: string);
{--------------------------------------------------------------------}
{ speichert Z�hlerstandssatz einer Kennung in Merkfile }
var
  FileName: string;
  FS: TFileOfRecStreamExt;
  isOK: boolean;
  FileRec: TRohTMerk;
  FRecCount: integer;
  bWrite: boolean;
  i: integer;

begin
  // 24.03.2010  WN
  // Datum des SaveRec muss plausibel sein:
  //  -> es darf nicht mehr als zwei Stunden in der Zukunft liegen und
  //  -> es darf nicht mehr als drei Jahre in der Vergangenheit liegen
  if IsDatumPlausibel(SaveRec.DatumZeit) then
  begin
    FileName:=GetTagSatz_MerkFilename (WSystemPfad);
    if not FileExists (FileName) then
      FS:=TFileOfRecStreamExt.Create (FileName, fmCreate, SizeOf (TRohTMerk), isOK)
    else
      FS:=TFileOfRecStreamExt.Create (FileName, fmOpenReadWrite OR fmShareDenyWrite,
                                      SizeOf (TRohTMerk), isOK);
    try
      if isOK then begin
        FRecCount:=FS.RecCount;
        bWrite:=true;
        while FS.RecPosition < FRecCount do begin
          Application.ProcessMessages;
          FS.ReadRec (FileRec);
          if FileRec.Kennung = Kennung then begin   { Eintrag f�r Kennung gefunden }
            // 24.03.2010  WN
            // Datum in Datei �berschreiben, wenn:
            // -> es nicht plausibel ist oder
            // -> es �lter ist als das des SaveRec
            if not IsDatumPlausibel(FileRec.TagSatz.DatumZeit) or
               (CmpDateTime (SaveRec.DatumZeit, FileRec.TagSatz.DatumZeit) > 0) then
              { auf gefundenen Eintrag positionieren, um ihn zu �berschreiben: }
              FS.RecPosition:=FS.RecPosition - 1
            else
              bWrite:=false;  // nur speichern, wenn j�nger (per SMS angeforderte
                              // (�ltere) Daten d�rfen den Merksatz nicht �berschreiben !)
                              // 02.12.2008, WW
            Break;
          end;
        end;
        if bWrite then begin
          FileRec.Kennung:=Kennung;
          FileRec.TagSatz.satzstatus:=SaveRec.satzstatus;
          FileRec.TagSatz.DatumZeit:=SaveRec.DatumZeit;
          for i:=1 to C_maxkanalzahl do begin  // 25.01.2010, WW
            FileRec.TagSatz.E_Zaehler [i].zaehlerstatus:=SaveRec.E_Zaehler [i].zaehlerstatus;
            FileRec.TagSatz.E_Zaehler [i].wert:=Round (SaveRec.E_Zaehler [i].wert);
            FileRec.TagSatz.K_Zaehler [i].zaehlerstatus:=SaveRec.K_Zaehler [i].zaehlerstatus;
            FileRec.TagSatz.K_Zaehler [i].wert:=Round (SaveRec.K_Zaehler [i].wert);
          end;
          FS.WriteRec (FileRec);
        end;
      end;
    finally
      FS.Free;
    end;
  end;
end;

{------------------------------------------------------------------------}
function Read_TagSatz_From_MerkFile (Kennung: string; WSystemPfad: string;
                                     var KennungRec: RohTRec): boolean;
{------------------------------------------------------------------------}
{ liest Z�hlerstandssatz einer Kennung aus Merkfile }
var
  FileName: string;
  FS: TFileOfRecStreamExt;
  isOK: boolean;
  FileRec: TRohTMerk;
  FRecCount: integer;
  i: integer;

begin
  Result:=false;
  FileName:=GetTagSatz_MerkFilename (WSystemPfad);
  if FileExists (FileName) then begin
    FS:=TFileOfRecStreamExt.Create (FileName, fmOpenRead OR fmShareDenyWrite,
                                    SizeOf (TRohTMerk), isOK);
    try
      if isOK then begin
        FRecCount:=FS.RecCount;
        while FS.RecPosition < FRecCount do begin
          Application.ProcessMessages;
          FS.ReadRec (FileRec);
          if FileRec.Kennung = Kennung then begin   { Eintrag f�r Kennung gefunden }
            KennungRec.satzstatus:=FileRec.TagSatz.satzstatus;
            KennungRec.DatumZeit:=FileRec.TagSatz.DatumZeit;
            for i:=1 to C_maxkanalzahl do begin  // 25.01.2010, WW
              KennungRec.E_Zaehler [i].zaehlerstatus:=FileRec.TagSatz.E_Zaehler [i].zaehlerstatus;
              KennungRec.E_Zaehler [i].wert:=FileRec.TagSatz.E_Zaehler [i].wert;
              KennungRec.K_Zaehler [i].zaehlerstatus:=FileRec.TagSatz.K_Zaehler [i].zaehlerstatus;
              KennungRec.K_Zaehler [i].wert:=FileRec.TagSatz.K_Zaehler [i].wert;
            end;
            Result:=true;
            Break;
          end;
        end;
      end;
    finally
      FS.Free;
    end;
  end;
end;


{------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------}
function KonvSMSData_VeriboxMini (SMSKonvRec: TSMSKonvMRG; var Kennung: string;
  var bDatenlueckePruefungOK: boolean): boolean;
{-----------------------------------------------------------------------------}
{ Konvertiert Datenteil einer SMS aus Veribox-Mini
  -> Alle Digitalkan�le der Veribox m�ssen einheitlich als Impuls-Z�hlerkan�le
     konfiguriert sein, da aus dem Protokoll nicht ersichtlich ist, ob der
     Digitalkanal als Z�hler oder Schalter fungiert (unterschiedliche Rohwertl�ngen !) }

const
  CMaxKanaele_Veribox = 6;
  { ab Vs. 6.1: max. 6 auswertbare Bits in der Kanalinfo (4 Impuls/2 Analog)
    -> solange nur Impulskan�le im Ger�t aktiviert sind, werden von der Konvertierung
       auch Versionen < 6.1 (3 Impuls/1 Analog) unterst�tzt.
    -> Ab Vs. 7.0 ist der hinzugekommene 3. Analogkanal (Kanal 7) im Ger�t unbedingt
       zu deaktivieren ! Grund: Es sind nur 6 auswertbare Bits in der Kanalinfo
       vorhanden, die Daten des 3. Analogkanals werden aber trotzdem mitgeliefert.
       Die SMS-Daten k�nnen somit nicht korrekt interpretiert werden ! }

  CAnalogKanaeleAb    = 5;    { Nummer des ersten Analogkanals }

  CByteLenIstwertImpuls    = 6;
  CByteLenZeitwertImpuls   = 2;
//  CByteLenSchalter         = 1;  in Konvertierung nicht benutzt
  CByteLenAnalog           = 2;

  {-------------------------------------------------}
  procedure Init_Stundensatz (var ARohSRec: RohSRec);
  {-------------------------------------------------}
  { Stundensatz initialisieren (alle Kanalwerte fehlend) }
  var
    i: integer;
  begin
    with ARohSRec do begin
      Satzstatus:=$00;
      for i:=1 to c_maxKanalZahl do begin
        Kanal[i].Wert:=0;
        KanalOrig[i].Wert:=0;
        if (i >= CAnalogKanaeleAb) AND (i <= CMaxKanaele_Veribox) then  { Analogkanal }
          Kanal[i].Kanalstatus:=$A0
        else  { Impulskanal }
          Kanal[i].Kanalstatus:=$80;
        KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
      end;
    end;
  end;

  {-----------------------------------------------}
  procedure Init_Tagessatz (var ARohTRec: RohTRec);
  {-----------------------------------------------}
  { Z�hlerstandssatz initialisieren (alle Kanalwerte fehlend) }
  var
    i: integer;
  begin
    with ARohTRec do begin
      Satzstatus:=$00;
      for i:=1 to c_maxKanalZahl do begin
        E_zaehler[i].Wert:=0;
        K_zaehler[i].Wert:=0;
        if (i >= CAnalogKanaeleAb) AND (i <= CMaxKanaele_Veribox) then begin  { Analogkanal }
          E_zaehler[i].Zaehlerstatus:=$A0;
          K_zaehler[i].Zaehlerstatus:=$A2;
        end
        else begin  { Impulskanal }
          E_zaehler[i].Zaehlerstatus:=$80;
          K_zaehler[i].Zaehlerstatus:=$82;
        end;
      end;
    end;
  end;

var
  fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
  fs_RohTRec: TFileOfRecStream;    { Tagessatz-Datei in RohTRec-Struktur }
  std_satz: RohSRec;
  tag_satz: RohTRec;
  merk_tag_satz: RohTRec;
  MessageTyp: integer;
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
  ZS_Merk: array [1..(CAnalogKanaeleAb - 1)] of integer;  // nur f�r Impulskan�le
  LastSavedTagRec: RohTRec;
  SaveTagRec: RohTRec;
  Tagessatz_merken: boolean;
  ZS_Diff: integer;
  Diff_Sec: integer;
  hour, min, sec, msec: word;
  FRecCount: integer;
  LenRohwert: integer;

begin
  Result:=false;
  Kennung:='';  { Vorbelegung: Kennung unbekannt }
  bDatenlueckePruefungOK:=true;  { Vorbelegung: die gemerkten Z�hlerst�nde stammen
                                   von der Vorperiode (keine Datenl�cke) }

  if length (SMSKonvRec.SMS_Data) = 0 then exit;
  try
    { Sekunden von 1.1.1970 bis 1.1.2000 berechnen (wird als Offset zur
      Berechnung der Zeitstempel ben�tigt: }
    dt_offset:=EncodeDate (2000, 1, 1);
    sec_offset:=GetUnixSekundenFromDateTime (dt_offset);

    { Z�hlerstandsmerker vorbelegen:}
    for i:=Low (ZS_Merk) to High (ZS_Merk) do
      ZS_Merk [i]:=-1;   { ung�ltig }

    { Vorbelegung: kein Tagessatz vorhanden, der im Merkfile gespeichert werden soll }
    Tagessatz_merken:=false;

    { Rohdaten: }
    S:=Copy (SMSKonvRec.SMS_Data, 1, 1);   { 1. Byte: MessageTyp }
    if length (S) = 1 then
      OK:=Basis64CodeStrToInt (S, IBuf)
    else
      OK:=false;
    if not OK then exit;
    MessageTyp:=IBuf;

    { nur SMS-Daten vom MessageTyp 2 (= Datensendung an Zentrale) k�nnen
      konvertiert werden: }
    if MessageTyp = 2 then begin
      S:=Copy (SMSKonvRec.SMS_Data, 2, 4);   { 2.-5. Byte: Seriennummer }
      if length (S) = 4 then
        OK:=Basis64CodeStrToInt (S, IBuf)
      else
        OK:=false;
      if not OK then exit;
      Kennung:=IntToStr (IBuf);     { Seriennummer wird als Kennung benutzt }

      S:=Copy (SMSKonvRec.SMS_Data, 6, 6);   { 6.-11. Byte: Bezugszeit f�r Ist-Werte (Sekunden ab 1.1.2000) }
      if length (S) = 6 then
        OK:=Basis64CodeStrToInt (S, IBuf)
      else
        OK:=false;
      if not OK then exit;
      sec_BezugIstWerte:=IBuf;

      { Ist-Werte der aktiven Kan�le: }
      Init_Tagessatz (tag_satz);
      { Zeitstempel: }
      UnixSekundenToDateTime (sec_BezugIstWerte + sec_offset, tag_satz.DatumZeit);

      { letzten Z�hlerstandssatz der vorangegangenen Konvertierung aus Merkfile
        lesen, wenn vorhanden: -> f�r Berechnung des ersten Stundenwertes }
      if Read_TagSatz_From_MerkFile (Kennung, SMSKonvRec.WSystemPfad, LastSavedTagRec) then begin
        merk_tag_satz:=LastSavedTagRec;
      end else
        merk_tag_satz.DatumZeit:=-1;  // Kennzeichen, da� Merksatz nicht vorhanden ist

      { Pr�fung, ob Merksatz von Vorstunde stammt: 19.01.2009, WW }
      if merk_tag_satz.DatumZeit > 0 then begin  // Merksatz vorhanden
        F_TimeDiff (merk_tag_satz.DatumZeit, tag_satz.DatumZeit, Diff_Sec);
        if Diff_sec > 3600 then begin
          // zwischen dem Zeitstempel des Merksatzes und der neuen Ist-Werte liegt
          // mehr als eine Messperiode (> +1h)
          if (SMSKonvRec.dtDatenlueckePruefung_bis >= Now) then begin
            // der Bis-Zeitpunkt f�r die Pr�fung ist noch nicht erreicht
            bDatenlueckePruefungOK:=false;
            Result:=true;
            exit;  // OK, Konvertierung abbrechen
          end;
        end;
      end;

      { Stundenwert-Zielfile erzeugen: }
      fs_RohSRec:=TFileOfRecStream.Create (SMSKonvRec.StdZieldateiname, fmCreate, SizeOf (RohSRec));
      try
        { Tagessatz-Zielfile erzeugen: }
        fs_RohTRec:=TFileOfRecStream.Create (SMSKonvRec.TagZieldateiname, fmCreate, SizeOf (RohTRec));
        try
          S:=Copy (SMSKonvRec.SMS_Data, 14, 1);   { 14. Byte: Kanalinfo Ist-Werte }
          if length (S) = 1 then
            OK:=Basis64CodeStrToInt (S, IBuf)
          else
            OK:=false;
          if not OK then exit;
          Kanaele_IstWerte:=IBuf;

          Pos:=15;   { Ist-Werte der aktiven Kan�le beginnen mit dem 15. Byte }
          Maske:=1;
          for i:=1 to CMaxKanaele_Veribox do begin
            Application.ProcessMessages;
            if (Kanaele_IstWerte AND Maske) <> 0 then begin   { Bit gesetzt -> Kanal ist aktiv }
              if i < CAnalogKanaeleAb then
                LenRohwert:=CByteLenIstWertImpuls
              else
                LenRohwert:=CByteLenAnalog;

              S:=Copy (SMSKonvRec.SMS_Data, Pos, LenRohwert);
              { Rohwert ist Z�hlerstand bzw. Analog-Mittelwert }
              if length (S) = LenRohwert then
                OK:=Basis64CodeStrToInt (S, IBuf)
              else
                OK:=false;
              if OK then begin
                { Kanalwert in Tagessatz schreiben:
                  -> Analogkanalwerte werden hier nur zwischengepuffert f�r die
                     anschlie�ende �bertragung in die Stundenwertdatei }
                tag_satz.E_Zaehler [i].Wert:=IBuf;
                tag_satz.E_Zaehler [i].Zaehlerstatus:=tag_satz.E_Zaehler [i].Zaehlerstatus and $7F;   { Fehlend-Bit l�schen }

                { Ist-Wert von Impulskan�len merken f�r Aufsummierung der Intervall-Inkremente: }
                if (i >= Low (ZS_Merk)) AND (i <= High (ZS_Merk)) then
                  ZS_Merk [i]:=IBuf;
              end else
                exit;
              inc (Pos, LenRohwert);
            end;
            Maske:=Maske SHL 1;
          end;  { for i }
          if Pos > 15 then begin   { Ist-Werte sind in den Rohdaten vorhanden }
            fs_RohTRec.WriteRec (tag_satz);  { ersten Tagessatz mit Ist-Werten schreiben }

            DecodeTime (tag_satz.DatumZeit, hour, min, sec, msec);
            if (min = 0) AND (sec = 0) then begin
              { aktuell letzter konvertierter Tagessatz einer vollen Stunde:
                -> merken f�r Stundenwert-Berechnung von Daten der n�chsten SMS }
              SaveTagRec:=tag_satz;
              Tagessatz_merken:=true;
            end;
          end;

          { Zeitwerte der aktiven Kan�le: }
          S:=Copy (SMSKonvRec.SMS_Data, Pos, 1);    { Kanalinfo Zeit-Werte (1 Byte) }
          if length (S) = 1 then
            OK:=Basis64CodeStrToInt (S, IBuf)
          else
            OK:=false;
          if not OK then exit;

          Kanaele_ZeitWerte:=IBuf;
          inc (Pos);

          S:=Copy (SMSKonvRec.SMS_Data, Pos, 2);  { Speicherintervall (2 Bytes) }
          if length (S) = 2 then
            OK:=Basis64CodeStrToInt (S, IBuf)
          else
            OK:=false;
          if not OK then exit;

          Speicherintervall_min:=IBuf;   { Speicherintervall in min }
          inc (Pos, 2);

          inc (Pos);  { Anzahl der Zeitwertbl�cke nicht relevant (1 Byte) }

          sec_Zeitwerte:=sec_BezugIstWerte + sec_offset;    { Vorbelegung }
          while Pos <= length (SMSKonvRec.SMS_Data) do begin
            Pos_merk:=Pos;
            { neuer Tagessatz: }
            Init_Tagessatz (tag_satz);
            { Zeitstempel: }
            inc (sec_ZeitWerte, Speicherintervall_min * 60);
            UnixSekundenToDateTime (sec_ZeitWerte, tag_satz.DatumZeit);

            Maske:=1;
            for i:=1 to CMaxKanaele_Veribox do begin
              Application.ProcessMessages;
              if (Kanaele_ZeitWerte AND Maske) <> 0 then begin   { Bit gesetzt -> Kanal ist aktiv }
                if i < CAnalogKanaeleAb then
                  LenRohwert:=CByteLenZeitWertImpuls
                else
                  LenRohwert:=CByteLenAnalog;

                S:=Copy (SMSKonvRec.SMS_Data, Pos, LenRohwert);
                { Rohwert ist Inkrement-Wert bei Impulskanal, Absolutwert bei Analogkanal }
                if length (S) = LenRohwert then
                  OK:=Basis64CodeStrToInt (S, IBuf)
                else
                  OK:=false;
                if OK then begin
                  if (i >= Low (ZS_Merk)) AND (i <= High (ZS_Merk)) then begin
                    { Z�hlerstand f�r Impulskan�le berechnen }
                    ZS_Merk [i]:=ZS_Merk [i] + IBuf;  { Inkrement aufaddieren }
                    { Impulskanalwert in Tagessatz schreiben: }
                    tag_satz.E_Zaehler [i].Wert:=ZS_Merk [i];
                  end else  { Analogkanalwert in Tagessatz schreiben }
                    tag_satz.E_Zaehler [i].Wert:=IBuf;
                  tag_satz.E_Zaehler [i].Zaehlerstatus:=tag_satz.E_Zaehler [i].Zaehlerstatus and $7F;       { Fehlend-Bit l�schen }
                end else
                  exit;
                inc (Pos, LenRohwert);
              end;
              Maske:=Maske SHL 1;
            end;
            if Pos > Pos_merk then begin   { Zeit-Werte sind in den Rohdaten vorhanden }
              fs_RohTRec.WriteRec (tag_satz);  { Tagessatz mit aufsummierten ZS schreiben }

              DecodeTime (tag_satz.DatumZeit, hour, min, sec, msec);
              if (min = 0) AND (sec = 0) then begin
                { aktuell letzter konvertierter Tagessatz einer vollen Stunde:
                  -> merken f�r Stundenwert-Berechnung von Daten der n�chsten SMS }
                SaveTagRec:=tag_satz;
                Tagessatz_merken:=true;
              end;
            end
            else if Pos = Pos_merk then  { wenn Kanaele_Zeitwerte = 0 und Rest-Rohdaten vorhanden sind }
              exit;
          end;  { while Pos }
          Result:=true;

          { letzten konvertierten Tagessatz einer vollen Stunde in Merkfile speichern: }
          if Tagessatz_merken then
            Save_TagSatz_To_MerkFile (Kennung, SaveTagRec, SMSKonvRec.WSystemPfad);


{--------------------- Berechnung der Stundenwerte ----------------------------}

          fs_RohTRec.RecPosition:=0;  { auf Anfang der Tagessatz-Datei positionieren }
          FRecCount:=fs_RohTRec.RecCount;
          while fs_RohTRec.RecPosition < FRecCount do begin
            Application.ProcessMessages;
            fs_RohTRec.ReadRec (tag_satz);

            { ZS-Differenzen bilden: }
            DecodeTime (tag_satz.DatumZeit, hour, min, sec, msec);
            if (min = 0) AND (sec = 0) then begin  { nur volle Stunden interessieren ! }
              Init_Stundensatz (std_satz);
              std_satz.DatumZeit:=tag_satz.DatumZeit;
              for i:=1 to CMaxKanaele_Veribox do begin
                Application.ProcessMessages;
                if (tag_satz.E_Zaehler [i].Zaehlerstatus AND $80) = 0 then begin  { keine fehlenden Werte }
                  if (tag_satz.E_Zaehler [i].Zaehlerstatus AND $20) = 0 then begin  { Impulskanal }
                    { ZS-Differenzbildung nur m�glich, wenn Vor-Z�hlerstand bekannt }
                    if merk_tag_satz.DatumZeit > 0 then begin
                      { Vor-Z�hlerstand mu� von Vorstunde sein: }
                      F_TimeDiff (merk_tag_satz.DatumZeit, tag_satz.DatumZeit, Diff_Sec);
                      if Diff_sec = 3600 then begin
                        if (merk_tag_satz.E_Zaehler [i].Zaehlerstatus AND $80) = 0 then begin
                          ZS_Diff:=Round (tag_satz.E_Zaehler [i].Wert - merk_tag_satz.E_Zaehler [i].Wert);  // 25.01.2010, WW
                          { ZS-Differenz in Stundensatz schreiben (.KanalOrig und .Kanal): 09.07.2009, WW }
                          if ZS_Diff >= 0 then
                            std_satz.Kanal [i].Wert:=ZS_Diff  { berechnete Stundenmenge }
                          else begin  // neg. Z�hlerstandsdifferenz: als �berlauf kennzeichnen; 26.03.2008, WW
                            std_satz.Kanal [i].Wert:=0;  { Ersatzwert }
                            std_satz.Kanal [i].KanalStatus:=std_satz.Kanal [i].KanalStatus or $01;  { �berlauf-Bit setzen }
                          end;
                          std_satz.Kanal [i].KanalStatus:=std_satz.Kanal [i].KanalStatus and $7F;       { Fehlend-Bit l�schen }

                          std_satz.KanalOrig [i].Wert:=std_satz.Kanal [i].Wert;
                          std_satz.KanalOrig [i].KanalStatus:=std_satz.Kanal [i].KanalStatus;
                        end;
                      end;
                    end;
                  end
                  else begin  { Analogkanal }
                    { Kanalwert in Stundensatz schreiben (.KanalOrig und .Kanal): }
                    std_satz.KanalOrig [i].Wert:=tag_satz.E_Zaehler [i].Wert;
                    std_satz.KanalOrig [i].KanalStatus:=std_satz.KanalOrig [i].KanalStatus and $7F;       { Fehlend-Bit l�schen }

                    std_satz.Kanal [i].Wert:=Round (std_satz.KanalOrig [i].Wert);
                    std_satz.Kanal [i].KanalStatus:=std_satz.Kanal [i].KanalStatus and $7F;       { Fehlend-Bit l�schen }

                    { zwischengepufferten Analogwert im Tagessatz-Record l�schen }
                    tag_satz.E_Zaehler [i].Wert := 0;
                    tag_satz.E_Zaehler [i].Zaehlerstatus :=
                      tag_satz.E_Zaehler [i].Zaehlerstatus OR $80;  { Fehlend-Bit setzen }
                  end;

                  { Impulskan�le: Felder std_satz.Kanal und std_satz.KanalOrig werden mit
                    berechneten Stundenmengen belegt (Differenzen der Original-Z�hlerst�nde).
                    Analogkan�le: Felder std_satz.Kanal und std_satz.KanalOrig werden mit
                    Original-Analogwerten belegt.
                    Normierte LGZ-Stundenwerte k�nnen nicht berechnet werden, da:
                      -> die f�r die R�ckrechnung notwendigen Informationen (Faktoren (Imp)/
                         Me�bereiche (Ana)) nicht bekannt sind.
                      -> neue LGZ-Stundenwerte nur zusammen mit den schon vorhandenen
                         LGZ-Daten berechnet werden k�nnen (Ist-Wert (Rohwert) ist Z�hlerstand !)
                    Die Berechnung der normierten LGZ-Werte mu� daher im Abruf-Client erfolgen ! }
                end;
              end;  { for i }
              fs_RohSRec.WriteRec (std_satz);  { Stundensatz mit ZS-Differenzen schreiben }

              fs_RohTRec.SeekRec (-1, soFromCurrent);
              fs_RohTRec.WriteRec (tag_satz);  { Tagessatz �berschreiben zum L�schen der Analogwerte }

              merk_tag_satz:=tag_satz;   { Tagessatz einer vollen Stunde merken }
            end;  { if (min = 0) AND (sec = 0) }
          end;  { while fs_RohTRec.RecPosition < FRecCount }
        finally
          fs_RohTRec.Free;
        end;
      finally
        fs_RohSRec.Free;
      end;
    end;  { if MessageTyp = 2 }
  except
    Result:=false;
    exit;
  end;
end;

{------------------------------------------------------------------------------}

{-----------------------------------------------------------------}
function KonvSMS_MRG (SMSKonvRec: TSMSKonvMRG; var Kennung: string;
  var bDatenlueckePruefungOK: boolean): boolean;
{-----------------------------------------------------------------}
{ konvertiert SMS-Daten in MRG-Zwischendateien f�r Stundenwerte und Tagess�tze;
  �bergabe: SMS-Konvertierungsrecord
  R�ckgabe: Kennung des SMS-Senders
            Flag 'bDatenlueckePruefungOK': true, wenn die gemerkten Z�hlerst�nde
                                           von der Vorperiode stammen (keine Datenl�cke)
  Ergebnis: true, wenn Konvertierung OK }
begin
  Result:=false;
  Kennung:='';   { Vorbelegung: Kennung unbekannt }
  bDatenlueckePruefungOK:=true;  { Vorbelegung: die gemerkten Z�hlerst�nde
                                   stammen von der Vorperiode (keine Datenl�cke) }
  { SMS-Datenteil ger�tetypabh�ngig konvertieren: }
  case SMSKonvRec.GeraeteTyp of
    mrgtyp_Veribox_Mini: Result:=KonvSMSData_VeriboxMini (SMSKonvRec, Kennung,
                                                          bDatenlueckePruefungOK);
  end;
end;

end.
