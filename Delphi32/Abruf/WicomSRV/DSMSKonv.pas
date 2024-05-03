{******************************************************************************}
{* Unit: Konvertierung von SMS-Daten in DSfG-Zwischendateien vom Typ          *}
{*        TDSfGSatzData (DSfG-Archivkanalwerte und Logbuchdaten)              *}
{* 26.07.2006 WW                                                              *}
{******************************************************************************}
unit DSMSKonv;

interface

uses
  Forms, Classes, Windows, SysUtils, WStrUtils, WSysCon, WChars, WStream, UnixDT,
  T_Tools, T_Zeit, DListen, DALKonv, WComm;

type

  { �bergaberecord f�r SMS-Konvertierung in DSfG-Daten }

  TSMSKonvDSfG = record
    SMS_Data: string;               { Datenteil einer SMS }
    GeraeteTyp: integer;            { Ger�tetypnummer }
    ZielPfad: string;               { Pfad f�r Ziel-Dateien mit Archiv/Logbuchdaten }
    ArLbDatenListe: TDSfGDataList;  { Archiv/Logbuch-Datenliste }
  end;

function KonvSMS_DSfG (SMSKonvRec: TSMSKonvDSfG; var Kennung: string;
                       var Datentyp: integer; var KonvErg: integer): boolean;

implementation

{------------------------------------------------------------------------------------------}
function KonvSMSMesswerte_MRG900_v (SMSKonvRec: TSMSKonvDSfG; var Kennung: string): integer;
{------------------------------------------------------------------------------------------}
{ konvertiert MRG 905/910-Messwert-SMS, neues Format v;
  Ergebnis: 0 = OK
            Fehler, wenn < 0 }

  {-----------------------------------------------------}
  procedure Init_SatzData (var ASatzData: TDSfGSatzData);
  {-----------------------------------------------------}
  { DSfG-Datensatz initialisieren (Wert fehlend) }
  begin
    with ASatzData do begin
      OrdnungsNr:=-1;
      DatumZeit:=0;
      UnixDatumZeit:='';
      WertAsDouble:=-1;
      WertAsString:='';  // Wert fehlend
      Status:='';  // Ersatzwert, da kein Status in der Messwert-SMS enthalten ist
      CRC:='';     // Ersatzwert, da kein CRC in der Messwert-SMS enthalten ist
    end;
  end;

const
  // Stati f�r Zustandskanal (derzeit nur "Uhr gestellt" im MRG implementiert):
  CDSfGStatMRG900_OK           = 0;
  CDSfGStatMRG900_Uhr_gestellt = 8;

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
  sUnixTime: string;
  sZeitzone: string;
  i: integer;
  c: char;
  cTrennz: char;
  cSMSData: char;
  cKontrZ: char;
  AktKontrollZaehler: array['p'..'s'] of integer;  // f�r alle m�glichen Kontrollz�hler A..D
  SatzData: array['p'..'s'] of TDSfGSatzData;  // f�r alle m�glichen Kontrollz�hler A..D
  SatzData_Zustand: TDSfGSatzData;  // f�r DSfG-Zustandskanal
  FS_Konv: array['p'..'s'] of TFileOfRecStream;  // f�r alle m�glichen Kontrollz�hler A..D
  FS_Konv_Zustand: TFileOfRecStream;  // f�r DSfG-Zustandskanal
  Pref: string;
  pFileName: array[0..255] of char;
  Filename: string;
  DSfGDataListObj: TDSfGDataListObj;
  EAdr_Reg: string;
  DEL: string;
  sKopf: string;
  bDatensatz_speichern: boolean;

begin
  Result:=0;  // OK
  Kennung:='';  { Vorbelegung: Kennung unbekannt }
  if length (SMSKonvRec.SMS_Data) = 0 then begin
    Result:=-1;
    exit;
  end;

  try
    S:=Copy (SMSKonvRec.SMS_Data, 2, 12);  { 2.-13. Zeichen: DSfG-Kennung }
    OK:=length (S) = 12;
    if not OK then begin
      Result:=-2;
      exit;
    end;
    Kennung:=S;

    // jetzt m�ssen Daten folgen:
    if length (SMSKonvRec.SMS_Data) < 14 then begin  // es folgen keine
      Result:=-12;
      exit;
    end;

    // Vorbelegungen:
    S:='';
    cTrennz:=NUL;
    sKopf:='';
    UnixSec_MP:=-1;
    Messperiodenlaenge_sec:=-1;
    EAdr_Reg:='';
    OrdnungsNr_Kopf:=-1;
    sZeitzone:='';
    bDatensatz_speichern:=false;
    for c:=Low (AktKontrollZaehler) to High (AktKontrollZaehler) do
      AktKontrollZaehler[c]:=-1;  // kein Kontrollz�hler-Wert zugewiesen

    // Zieldateien sind geschlossen
    for c:=Low (FS_Konv) to High (FS_Konv) do
      FS_Konv[c]:=nil;
    FS_Konv_Zustand:=nil;
    try
      for i:=14 to length (SMSKonvRec.SMS_Data) do begin   // Reststring ab Zeit bis zum Ende
        Application.ProcessMessages;

        cSMSData:=SMSKonvRec.SMS_Data[i];
        // neues Trennzeichen gefunden oder Stringende:
        if ((cSMSData in ['p'..'w','z']) AND (length (sKopf) > 9)) OR
           (i = length (SMSKonvRec.SMS_Data)) then begin

          if i = length (SMSKonvRec.SMS_Data) then  // bei Stringende letztes Zeichen anh�ngen
            S:=S + SMSKonvRec.SMS_Data[i];

          if cSMSData = 'z' then  // es beginnt ein neuer Kopf
            sKopf:='';

          if (cTrennz = NUL) OR (cTrennz = 'z') then begin  // String S ist Startsatz oder Sondersatz z
            // Zeit, MP-L�nge und Ordnungsnummer verarbeiten
            SBuf:=Copy (S, 1, 6);   { 1.-6. Byte: Zeit der Kontrollz�hler (Unix-Zeit) }
            if length (SBuf) = 6 then
              OK:=Basis64CodeStrToInt (SBuf, IBuf)
            else
              OK:=false;
            if not OK then begin
              Result:=-3;
              exit;
            end;
            UnixSec_Kopf:=IBuf;

            { Zeitstempel: }
            UnixSekundenToDateTime (UnixSec_Kopf, DatumZeit);
            DateTimeToUnixTimeStr (DatumZeit, sUnixTime);  // in Unix-Zeit wandeln

            SBuf:=Copy (S, 7, 1);   { 7. Byte: Zeitzonen-Kennzeichen }
            bDatensatz_speichern:=true;
            if SBuf = 'r' then begin  // Satz nach Neustart des MW-SMS-Systems
              sZeitzone:='';  // keine Zeitzonen-Info vorhanden
              bDatensatz_speichern:=false;  // kein Archiv-Datensatz ! Nicht speichern !
            end
            else if UpperCase(SBuf) = 'M' then
              sZeitzone:=CMEZ   // Winterzeit
            else if UpperCase(SBuf) = 'S' then
              sZeitzone:=CMESZ  // Sommerzeit
            else begin  // ung�ltiges Zeitzonen-Kennzeichen
              Result:=-4;
              exit;
            end;

            SBuf:=Copy (S, 8, 1);   { 8. Byte: Messperiodenl�nge }
            if length (SBuf) = 1 then
              OK:=Basis64CodeStrToInt (SBuf, IBuf)
            else
              OK:=false;
            if not OK then begin
              Result:=-5;
              exit;
            end;
            Messperiodenlaenge_sec:=IBuf * 60;   { Messperiodenl�nge in sec }
            UnixSec_MP:=UnixSec_Kopf - (UnixSec_Kopf MOD Messperiodenlaenge_sec);  { Unix-Zeit der letzten vollen Messperiode }

            SBuf:=Copy (S, 9, 1);   { 9. Byte: Adresse der Reg.Instanz }
            OK:=length(SBuf) = 1;
            if not OK then begin
              Result:=-6;
              exit;
            end;
            EAdr_Reg:=SBuf;

            SBuf:=Copy (S, 10, length(S));   { Rest: Ordnungsnummer }
            if length (SBuf) > 0 then
              OK:=Basis64CodeStrToInt (SBuf, IBuf)
            else
              OK:=false;
            if not OK then begin
              Result:=-7;
              exit;
            end;
            OrdnungsNr_Kopf:=IBuf;

            // neuer Datensatz f�r alle Kan�le beginnt
            for c:=Low (SatzData) to High (SatzData) do begin
              Init_SatzData (SatzData[c]);
              SatzData[c].DatumZeit:=DatumZeit;
              SatzData[c].UnixDatumZeit:=sUnixTime;
              SatzData[c].OrdnungsNr:=OrdnungsNr_Kopf;
              SatzData[c].Zeitzone:=sZeitzone;
            end;
            SatzData_Zustand:=SatzData[Low (SatzData)];

            // Wert f�r Zustandskanal:
            if cTrennz = 'z' then   // Sondersatz z (Uhr verstellt)
              SatzData_Zustand.WertAsString:=IntToStr (CDSfGStatMRG900_Uhr_gestellt)
            else
              SatzData_Zustand.WertAsString:=IntToStr (CDSfGStatMRG900_OK);
         end
          else begin
            // Rohwert (Z�hlerstand, Messperiodenwert) verarbeiten und zuweisen
            if cTrennz in ['p'..'w'] then begin
              if length (S) > 0 then
                OK:=Basis64CodeStrToInt (S, IBuf)
              else
                OK:=false;
              if not OK then begin
                Result:=-8;
                exit;
              end;

              if cTrennz in ['p'..'s'] then begin  // Rohwert ist Kontrollz�hler A..D
                cKontrZ:=cTrennz;
                AktKontrollZaehler [cKontrZ]:=IBuf;   // Kontrollz�hler neu setzen
              end
              else begin  // Rohwert ist MP-Wert A..D (Inkrement)
                cKontrZ:=Chr (Ord (cTrennz) - 4);
                if AktKontrollZaehler [cKontrZ] < 0 then begin  // Fehler: Kontrollz�hler nicht gesetzt
                  Result:=-9;
                  exit;
                end;
                // Kontrollz�hler fortschreiben durch Aufaddieren des MP-Werts
                AktKontrollZaehler [cKontrZ]:=AktKontrollZaehler [cKontrZ] + IBuf;
              end;

              // Wert f�r Kontrollz�hlerkanal:
              if (cKontrZ >= Low (SatzData)) AND (cKontrZ <= High (SatzData)) then
                SatzData[cKontrZ].WertAsString:=IntToStr (AktKontrollZaehler [cKontrZ]);

              // wenn erster Kanalwert vorhanden ist, Zieldatei f�r Archivkanaldaten
              // ermitteln und File anlegen: }
              if not Assigned (FS_Konv[cKontrZ]) then begin
                Pref:=prefix_DSfG_Ar;     { Prefix f�r DSfG-Archivdaten }
                GetTempFileName (pchar (SMSKonvRec.ZielPfad), pchar (Pref), 0,
                                 pFileName);  { Zieldatei anlegen }
                Filename:=string (pFileName);
                FS_Konv[cKontrZ]:=TFileOfRecStream.Create (FileName, fmOpenReadWrite OR fmShareDenyWrite,
                                                           SizeOf (TDSfGSatzData));

                { Name der Konvertierungs-Datei mit Busadresse und DE-Adresse in
                  Archiv/Logbuch-DatenListe eintragen: }
                if SMSKonvRec.ArLbDatenListe <> nil then begin
                  DSfGDataListObj:=TDSfGDataListObj.Create;
                  { DE-Adresse f�r Kanal bilden:
                    -> Kontrollz�hler des MRG 900 sind der DSfG-Archivgruppe 1 zugeordnet }
                  DEL:='caa' + Chr (Ord (cKontrZ) - 10) + 'd'; { p -> f, q -> g usw.}
                  DSfGDataListObj.SetData (EAdr_Reg, DEL);
                  SMSKonvRec.ArLbDatenListe.AddObject (FileName, DSfGDataListObj);
                end;
              end;

              // ...dito f�r Zustandskanal: }
              if not Assigned (FS_Konv_Zustand) then begin
                Pref:=prefix_DSfG_Ar;     { Prefix f�r DSfG-Archivdaten }
                GetTempFileName (pchar (SMSKonvRec.ZielPfad), pchar (Pref), 0,
                                 pFileName);  { Zieldatei anlegen }
                Filename:=string (pFileName);
                FS_Konv_Zustand:=TFileOfRecStream.Create (FileName, fmOpenReadWrite OR fmShareDenyWrite,
                                                          SizeOf (TDSfGSatzData));

                { Name der Konvertierungs-Datei mit Busadresse und DE-Adresse in
                  Archiv/Logbuch-DatenListe eintragen: }
                if SMSKonvRec.ArLbDatenListe <> nil then begin
                  DSfGDataListObj:=TDSfGDataListObj.Create;
                  { DE-Adresse f�r Zustandskanal in DSfG-Archivgruppe 1 bilden: }
                  DEL:='caald';  { Kanal 7 }
                  DSfGDataListObj.SetData (EAdr_Reg, DEL);
                  SMSKonvRec.ArLbDatenListe.AddObject (FileName, DSfGDataListObj);
                end;
              end;

              if bDatensatz_speichern then begin
                // Kontrollz�hler-Datensatz in Zieldatei schreiben:
                FS_Konv[cKontrZ].WriteRec (SatzData[cKontrZ]);
                // Zustandskanal-Datensatz in Zieldatei schreiben, wenn Kontroll-
                // z�hler von Kanal 1 geschrieben wird (Kanal 1 mu� immer existieren):
                if cKontrZ = 'p' then
                  FS_Konv_Zustand.WriteRec (SatzData_Zustand);
              end;
            end;

            // neue Messperiode beginnt, wenn:
            if (cSMSData in ['t'..'w']) AND  // ...Wert ein Messperiodenwert ist
               (not (cTrennz in ['t'..'w']) OR  // ...es der erste Messperiodenwert ist
                (cSMSData <= cTrennz)) then begin // ...ASCII-Code aktuelles MP-Wert-Trennzeichen <= vorheriges MP-Wert-Trennzeichen
              if (UnixSec_MP < 0) OR (Messperiodenlaenge_sec < 0) OR
                 (OrdnungsNr_Kopf < 0) then begin
                // Fehler: UnixSec_MP, Messperiodenlaenge_sec oder OrdnungsNr_Kopf
                // enthalten Vorbelegungswerte
                Result:=-10;
                exit;
              end
              else begin  // Datum/Zeit und Ordnungsnummer der n�chsten MP fortschreiben
                inc (UnixSec_MP, Messperiodenlaenge_sec);
                UnixSekundenToDateTime (UnixSec_MP, DatumZeit);
                DateTimeToUnixTimeStr (DatumZeit, SBuf);  // in Unix-Zeit wandeln
                inc (OrdnungsNr_Kopf);

                // neuer Datensatz f�r alle Kan�le beginnt
                for c:=Low (SatzData) to High (SatzData) do begin
                  Init_SatzData (SatzData[c]);
                  SatzData[c].DatumZeit:=DatumZeit;
                  SatzData[c].UnixDatumZeit:=SBuf;
                  SatzData[c].OrdnungsNr:=OrdnungsNr_Kopf;
                  SatzData[c].Zeitzone:=sZeitzone;  // gemerktes Zeitzonen-Kennzeichen vom Kopf oder Sondersatz
                end;
                SatzData_Zustand:=SatzData[Low (SatzData)];
                // Wert f�r Zustandskanal:
                SatzData_Zustand.WertAsString:=IntToStr (CDSfGStatMRG900_OK);  // OK

                bDatensatz_speichern:=true;  // MP-S�tze immer speichern
              end;
            end;
          end;

          cTrennz:=cSMSData;  // neu gefundenes Trennzeichen setzen
          S:='';  // Rohwert neu initialisieren
        end
        else begin
          S:=S + SMSKonvRec.SMS_Data[i];
          sKopf:=sKopf + SMSKonvRec.SMS_Data[i];
        end;
      end;  { for i }
    finally
      for c:=Low (FS_Konv) to High (FS_Konv) do
        if Assigned (FS_Konv[c]) then  // Zieldatei f�r Kontrollz�hler ist ge�ffnet
          FS_Konv[c].Free;  // Datei schliessen

      if Assigned (FS_Konv_Zustand) then  // Zieldatei f�r Zustandskanal ist ge�ffnet
        FS_Konv_Zustand.Free;  // Datei schliessen
    end;
  except
    Result:=-11;
    exit;
  end;
end;

{----------------------------------------------------------------------------------------}
function KonvSMSMeldung_MRG900_r (SMSKonvRec: TSMSKonvDSfG; var Kennung: string): integer;
{----------------------------------------------------------------------------------------}
{ konvertiert MRG 905/910-Meldungs-SMS, neues Format r;
  Ergebnis: 0 = OK
            Fehler, wenn < 0 }
const
  CTrennzeichen = ' ';  // Space
var
  S: string;
  SBuf: string;
  OK: boolean;
  dtDatum: TDateTime;
  dtZeit: TDateTime;
  FS_Konv: TFileOfRecStream;    { Datei in TDSfGSatzData-Struktur }
  SatzData: TDSfGSatzData;
  Pref: string;
  pFileName: array[0..255] of char;
  FileName: string;
  Code: integer;
  DSfGDataListObj: TDSfGDataListObj;
  EAdr_Reg: string;
  EAdr_Quelle: char;
  DEL: string;

begin
  Result:=0;  // OK
  Kennung:='';  { Vorbelegung: Kennung unbekannt }
  if length (SMSKonvRec.SMS_Data) = 0 then begin
    Result:=-1;
    exit;
  end;

  try
    { Ger�tetyp (2.-7. Zeichen), Stationsname (9.-24. Zeichen), Kennung (26.-39. Zeichen):
      nicht relevant f�r Konvertierung in DSfG-Daten }

    { Zieldateiname f�r Logbuch-Daten ermitteln und File anlegen: }
    Pref:=prefix_DSfG_Lb;     { Prefix f�r DSfG-Logbuchdaten }
    GetTempFileName (pchar (SMSKonvRec.ZielPfad), pchar (Pref), 0, pFileName);  { Zieldatei anlegen }
    Filename:=string (pFileName);
    FS_Konv:=TFileOfRecStream.Create (FileName, fmOpenReadWrite OR fmShareDenyWrite,
                                      SizeOf (SatzData));
    try
      S:=Copy (SMSKonvRec.SMS_Data, 41, 10);  { 41.-50. Zeichen: Datum }
      OK:=EncodeDateStr (S, 'DD.MM.YYYY', dtDatum);
      if not OK then begin
        Result:=-2;
        exit;
      end;

      S:=Copy (SMSKonvRec.SMS_Data, 52, 8);  { 52.-59. Zeichen: Zeit }
      OK:=EncodeTimeStr (S, 'HH:MM:SS', dtZeit);
      if not OK then begin
        Result:=-3;
        exit;
      end;
      SatzData.DatumZeit:=dtDatum + dtZeit;
      DateTimeToUnixTimeStr (SatzData.DatumZeit, S);  // in Unix-Zeit wandeln
      SatzData.UnixDatumZeit:=S;

      S:=Copy (SMSKonvRec.SMS_Data, 60, 1);  { 60. Zeichen: Trennzeichen f�r Zeitzone (optional) oder
                                               Trennzeichen zu Meldungstext }
      if (S = ',') OR (S = ';') then begin  { Zeitzonen-Trennzeichen }
        SBuf:=Copy (SMSKonvRec.SMS_Data, 62, length (SMSKonvRec.SMS_Data));  { Rest bis zum Ende }
        S:=F_Zerlegen (SBuf, CTrennzeichen);  { bis zum n�chsten Trennzeichen: Zeitzone }
        if S = '' then
          SatzData.Zeitzone:=''     // keine Zeitzonen-Info vorhanden
        else if S = 'MEZ' then
          SatzData.Zeitzone:=CMEZ   // Winterzeit
        else if S = 'MESZ' then
          SatzData.Zeitzone:=CMESZ  // Sommerzeit
        else begin  // ung�ltiges Zeitzonen-Kennzeichen
          Result:=-4;
          exit;
        end;
      end else
        SBuf:=Copy(SMSKonvRec.SMS_Data, 61, length (SMSKonvRec.SMS_Data));  { Rest bis zum Ende }

      { Meldungstext (1.-16. Zeichen des Reststring): wird nicht in DSfG-Daten konvertiert }

      SBuf:=Copy (SBuf, 18, length (SMSKonvRec.SMS_Data));  { Rest bis zum Ende }
      S:=F_Zerlegen (SBuf, CTrennzeichen);  { bis zum n�chsten Trennzeichen: DSfG-Meldungsnummer }
      SatzData.WertAsString:=S;
      SatzData.WertAsDouble := -1;         { wird f�r Logb�cher nicht verwendet }

      S:=F_Zerlegen (SBuf, CTrennzeichen);  { bis zum n�chsten Trennzeichen: DSfG-Ordnungsnummer }
      try
        Val (S, SatzData.OrdnungsNr, Code);
      except
        Result:=-5;
        exit;
      end;

      S:=F_Zerlegen (SBuf, CTrennzeichen);  { bis zum n�chsten Trennzeichen: Adresse der Reg.Instanz }
      OK:=length (S) = 1;
      if not OK then begin
        Result:=-6;
        exit;
      end;
      EAdr_Reg:=S;

      S:=F_Zerlegen (SBuf, CTrennzeichen);  { bis zum n�chsten Trennzeichen: Adresse der Wieser-Instanz }
      OK:=length (S) = 1;
      if not OK then begin
        Result:=-7;
        exit;
      end;
      EAdr_Quelle:=S[1];

      { Rest: DSfG-Kennung }
      OK:=length (SBuf) = 12;
      if not OK then begin
        Result:=-8;
        exit;
      end;
      Kennung:=SBuf;  // f�r R�ckgabe

      SatzData.Status:='';  // Ersatzwert, da kein Status in der Meldungs-SMS enthalten ist
      SatzData.CRC:='';     // Ersatzwert, da kein CRC in der Meldungs-SMS enthalten ist
      FS_Konv.WriteRec (SatzData);  { Meldungssatz schreiben }
    finally
      FS_Konv.Free;
    end;

    { Name der Konvertierungs-Datei mit Busadresse und DE-Adresse in Archiv/Logbuch-DatenListe eintragen: }
    if SMSKonvRec.ArLbDatenListe <> nil then begin
      DSfGDataListObj:=TDSfGDataListObj.Create;
      { DE-Adresse f�r Logbuch aus Quell-Adresse bilden: }
      if (EAdr_Quelle >= Low (CLogbuchDEL)) AND (EAdr_Quelle <= High (CLogbuchDEL)) then
        DEL:=CLogbuchDEL [EAdr_Quelle]+'d'  // DEL-Adresse f�r Logbuch
      else
        DEL:='';
      DSfGDataListObj.SetData (EAdr_Reg, DEL);
      SMSKonvRec.ArLbDatenListe.AddObject (FileName, DSfGDataListObj);
    end;
  except
    Result:=-9;
    exit;
  end;
end;

{-------------------------------------------------------------------------}
function KonvSMSData_MRG900 (SMSKonvRec: TSMSKonvDSfG; var Kennung: string;
                             var Datentyp: integer): integer;
{-------------------------------------------------------------------------}
{ konvertiert Datenteil einer SMS aus MRG 905/910 in DSfG-Datenstruktur;
  Ergebnis: 0 = OK
            Fehler, wenn < 0 }
begin
  Result:=-99;
  Kennung:='';  { Vorbelegung: Kennung unbekannt }
  Datentyp:=0;  { Vorbelegung: Datentyp unbekannt }
  if length (SMSKonvRec.SMS_Data) = 0 then exit;

  { nur SMS-Daten vom Typ v (Messwerte) und r (Meldungen) k�nnen konvertiert werden: }
  if SMSKonvRec.SMS_Data [1] = 'r' then begin  // neues Format Meldungen
    Datentyp:=C_IsLogbuecher;
    Result:=KonvSMSMeldung_MRG900_r (SMSKonvRec, Kennung)
  end
  else if SMSKonvRec.SMS_Data [1] = 'v' then begin  // neues Format Messwerte
    Datentyp:=C_IsArchive;
    Result:=KonvSMSMesswerte_MRG900_v (SMSKonvRec, Kennung);
  end;
end;

{------------------------------------------------------------------------------}

{---------------------------------------------------------------------------}
function KonvSMS_DSfG (SMSKonvRec: TSMSKonvDSfG; var Kennung: string;
                       var Datentyp: integer; var KonvErg: integer): boolean;
{---------------------------------------------------------------------------}
{ konvertiert SMS-Daten in DSfG-Zwischendatei f�r Archivkanal- oder Logbuchdaten;
  �bergabe: SMS-Konvertierungsrecord
            Wieser-System-Pfad
  R�ckgabe: Kennung des SMS-Senders
            Typ der SMS-Daten (Archivdaten, Logbuchdaten)
            Konvertierungsergebnis
  Ergebnis: true, wenn Konvertierung OK }
begin
  Kennung:='';  { Vorbelegung: Kennung unbekannt }
  Datentyp:=0;  { Vorbelegung: Datentyp unbekannt }
  KonvErg:=-100;  { Vorbelegung: Ger�tetyp unbekannt }

  { SMS-Datenteil ger�tetypabh�ngig konvertieren: }
  case SMSKonvRec.GeraeteTyp of
    mrgtyp_MRG910: KonvErg:=KonvSMSData_MRG900 (SMSKonvRec, Kennung, Datentyp);
  end;

  { Kennung: abschlie�ende Leerzeichen wegschneiden wie bei DF� }
  Kennung:=F_RightTrunc (Kennung, ' ');
  Result:=KonvErg = 0;
end;

end.
