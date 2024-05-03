{******************************************************************************}
{* Unit: Konvertierung von SMS-Daten in DSfG-Zwischendateien vom Typ          *}
{*       TDSfGSatzData (DSfG-Archivkanalwerte und Logbuchdaten)               *}
{*       -> auch für per GPRS übertragene Daten                               *}
{* 26.07.2006 WW                                                              *}
{******************************************************************************}
unit DGPRSKonv;

interface

uses
  Forms, Classes, Windows, SysUtils, WStrUtils, WSysCon, WChars, WStream, UnixDT,
  T_Tools, T_Zeit, DListen, DALKonv, WComm;

type

  { Übergaberecord für SMS-Konvertierung in DSfG-Daten }

  TSMSKonvDSfG = record
    SMS_Data: string;               { Datenteil einer SMS }
    GeraeteTyp: integer;            { Gerätetypnummer }
    ZielPfad: string;               { Pfad für Ziel-Dateien mit Archiv/Logbuchdaten }
    ArLbDatenListe: TDSfGDataList;  { Archiv/Logbuch-Datenliste }
  end;

function KonvSMS_DSfG (SMSKonvRec: TSMSKonvDSfG; var Kennung: string;
                       var Datentyp: integer; var KonvErg: integer;
                       var sLog: string; isGPRS: boolean): boolean;

implementation

const
  Cungueltig = 'ungültig';


{--------------------------------------------------------------------------------}
function KonvSMSMesswerte_MRG900_v (SMSKonvRec: TSMSKonvDSfG; var Kennung: string;
                                    var sLog: string): integer;
{--------------------------------------------------------------------------------}
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
      WertAsDouble:=-1;  // Wert fehlend
      WertAsString:='';  // Wert fehlend
      Status:='';  // Ersatzwert, da kein Status in der Messwert-SMS enthalten ist
      CRC:='';     // Ersatzwert, da kein CRC in der Messwert-SMS enthalten ist
      Zeitzone:='';
    end;
  end;

const
  CMaxImpKanaele = 4;
  CMaxAnaKanaele = 4;
  CMaxKanaele    = CMaxImpKanaele + CMaxAnaKanaele;

  CTrennzSetZSImp  = ['p'..'s'];  // Trennzeichen für Zählerstände (Impulskanal)
  CTrennzSetIncImp = ['t'..'w'];  // Trennzeichen für Zählerstands-Inkremente (Impulskanal)
  CTrennzSetAna    = ['!'..'$'];  // Trennzeichen für Analogwerte

  // Stati für Zustandskanal (derzeit nur "Uhr gestellt" im MRG implementiert):
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
  k: integer;
  cTrennz: char;
  cSMSData: char;
  KanalNr: integer;
  SatzData: array[1..CMaxKanaele] of TDSfGSatzData;  // für alle Kanäle
  SatzData_Zustand: TDSfGSatzData;  // für DSfG-Zustandskanal
  FS_Konv: array[1..CMaxKanaele] of TFileOfRecStream;  // für alle Kanäle
  FS_Konv_Zustand: TFileOfRecStream;  // für DSfG-Zustandskanal
  AktKontrollZaehler: array[1..CMaxImpKanaele] of integer;  // für alle möglichen Kontrollzähler A..D
  Pref: string;
  pFileName: array[0..255] of char;
  Filename: string;
  DSfGDataListObj: TDSfGDataListObj;
  EAdr_Reg: string;
  DEL: string;
  sKopf: string;
  bDatensatz_speichern: boolean;
  siValue: single;
  z_Satz_gelesen: boolean;
  sWertTyp: string;
  bWert_speichern: boolean;
  bZustand_speichern: boolean;

begin
  Result:=0;  // OK
  Kennung:='';  { Vorbelegung: Kennung unbekannt }
  sLog:='';     { Vorbelegung: Log-String leer }
  if length (SMSKonvRec.SMS_Data) = 0 then begin
    Result:=-1;
    exit;
  end;

  try
    S:=Copy (SMSKonvRec.SMS_Data, 2, 12);  { 2.-13. Zeichen: DSfG-Kennung }
    OK:=length (S) = 12;
    if not OK then begin
      sLog:=sLog + 'DSfG-Kennung: ' + CUngueltig + CR + LF;
      Result:=-2;
      exit;
    end else
      sLog:=sLog + 'DSfG-Kennung: ' + S + CR + LF;
    Kennung:=S;

    // jetzt müssen Daten folgen:
    if length (SMSKonvRec.SMS_Data) < 14 then begin  // es folgen keine, Fehler
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
    bZustand_speichern:=false;
    z_Satz_gelesen:=false;
    for k:=Low (AktKontrollZaehler) to High (AktKontrollZaehler) do
      AktKontrollZaehler[k]:=-1;  // kein Kontrollzähler-Wert zugewiesen

    // Zieldateien sind geschlossen
    for k:=Low (FS_Konv) to High (FS_Konv) do
      FS_Konv[k]:=nil;
    FS_Konv_Zustand:=nil;
    try
      for i:=14 to length (SMSKonvRec.SMS_Data) do begin   // Reststring ab Zeit bis zum Ende
        Application.ProcessMessages;

        cSMSData:=SMSKonvRec.SMS_Data[i];
        // neues Trennzeichen gefunden oder Stringende:
        if ((cSMSData in ['p'..'w', '!'..'$', 'z']) AND (length (sKopf) > 9)) OR
           (i = length (SMSKonvRec.SMS_Data)) then begin

          if i = length (SMSKonvRec.SMS_Data) then  // bei Stringende letztes Zeichen anhängen
            S:=S + SMSKonvRec.SMS_Data[i];

          if cSMSData = 'z' then begin   // es beginnt ein neuer (Sondersatz-)Kopf
            sKopf:='';
            z_Satz_gelesen:=true;  // Merker für gelesenes z-Satz-Kennzeichen
          end;

          if (cTrennz = NUL) OR (cTrennz = 'z') then begin  // String S ist Startsatz oder Sondersatz z
            if cTrennz = 'z' then
              sLog:=sLog +  'Sondersatz: ' + CR + LF;

            // Zeit, MP-Länge und Ordnungsnummer verarbeiten
            SBuf:=Copy (S, 1, 6);   { 1.-6. Byte: Zeit der Kontrollzähler (Unix-Zeit) }
            if length (SBuf) = 6 then
              OK:=Basis64CodeStrToInt (SBuf, IBuf)
            else
              OK:=false;
            if not OK then begin
              sLog:=sLog +  'Datum/Zeit: ' + Cungueltig + CR + LF;
              Result:=-3;
              exit;
            end;
            UnixSec_Kopf:=IBuf;

            { Zeitstempel: }
            UnixSekundenToDateTime (UnixSec_Kopf, DatumZeit);
            sLog:=sLog +  'Datum/Zeit: ' + FormatDateTime ('dd.mm.yyyy hh:nn:ss', DatumZeit) + CR + LF;
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
            else begin  // ungültiges Zeitzonen-Kennzeichen
              sLog:=sLog + 'Zeitzone: ' + Cungueltig + CR + LF;
              Result:=-4;
              exit;
            end;
            sLog:=sLog + 'Zeitzone: ' + SBuf + CR + LF;

            SBuf:=Copy (S, 8, 1);   { 8. Byte: Messperiodenlänge }
            if length (SBuf) = 1 then
              OK:=Basis64CodeStrToInt (SBuf, IBuf)
            else
              OK:=false;
            if not OK then begin
              sLog:=sLog + 'Messperiodenlänge [min]: ' + Cungueltig + CR + LF;
              Result:=-5;
              exit;
            end else
              sLog:=sLog + 'Messperiodenlänge [min]: ' + IntToStr (IBuf) + CR + LF;
            Messperiodenlaenge_sec:=IBuf * 60;   { Messperiodenlänge in sec }
            UnixSec_MP:=UnixSec_Kopf - (UnixSec_Kopf MOD Messperiodenlaenge_sec);  { Unix-Zeit der letzten vollen Messperiode }

            SBuf:=Copy (S, 9, 1);   { 9. Byte: Adresse der Reg.Instanz }
            OK:=length(SBuf) = 1;
            if not OK then begin
              sLog:=sLog + 'Adresse der Registrierinstanz: ' + Cungueltig + CR + LF;
              Result:=-6;
              exit;
            end else
              sLog:=sLog + 'Adresse der Registrierinstanz: ' + SBuf + CR + LF;
            EAdr_Reg:=SBuf;

            SBuf:=Copy (S, 10, length(S));   { Rest: Ordnungsnummer }
            if length (SBuf) > 0 then
              OK:=Basis64CodeStrToInt (SBuf, IBuf)
            else
              OK:=false;
            if not OK then begin
              sLog:=sLog + 'Ordnungsnummer: ' + Cungueltig + CR + LF;
              Result:=-7;
              exit;
            end else
              sLog:=sLog + 'Ordnungsnummer: ' + IntToStr (IBuf) + CR + LF;
            OrdnungsNr_Kopf:=IBuf;

            // neuer Datensatz für alle Kanäle beginnt
            for k:=Low (SatzData) to High (SatzData) do begin
              Init_SatzData (SatzData[k]);
              SatzData[k].DatumZeit:=DatumZeit;
              SatzData[k].UnixDatumZeit:=sUnixTime;
              SatzData[k].OrdnungsNr:=OrdnungsNr_Kopf;
              SatzData[k].Zeitzone:=sZeitzone;
            end;
            SatzData_Zustand:=SatzData[Low (SatzData)];
            // Wert für Zustandskanal:
            if cTrennz = 'z' then begin  // Sondersatz z (Uhr verstellt)
              SatzData_Zustand.WertAsDouble:=CDSfGStatMRG900_Uhr_gestellt;
              SatzData_Zustand.WertAsString:=IntToStr (CDSfGStatMRG900_Uhr_gestellt);
            end
            else begin
              SatzData_Zustand.WertAsDouble:=CDSfGStatMRG900_OK;
              SatzData_Zustand.WertAsString:=IntToStr (CDSfGStatMRG900_OK);
            end;
            bZustand_speichern:=true;
          end
          else begin
            // Rohwert (Zählerstand, Messperiodenzähler, Analogwert) verarbeiten und zuweisen
            if cTrennz in ['p'..'w', '!'..'$'] then begin
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
                '!': sWertTyp:='Analogwert 1: ';        // Trennzeichen Analogwert 1
                '"': sWertTyp:='Analogwert 2: ';        // Trennzeichen Analogwert 2
                '#': sWertTyp:='Analogwert 3: ';        // Trennzeichen Analogwert 3
                '$': sWertTyp:='Analogwert 4: ';        // Trennzeichen Analogwert 4
              else
                sWertTyp:='Unbekannter Wert: ';
              end;

              if not OK then begin
                sLog:=sLog + sWertTyp + Cungueltig + CR + LF;
                Result:=-8;
                exit;
              end;

              bWert_speichern:=true;
              if cTrennz in ['p'..'s'] then begin  // Rohwert ist Kontrollzähler A..D
                KanalNr:=Ord (cTrennz) - 111;
                AktKontrollZaehler [KanalNr]:=IBuf;   // Kontrollzähler neu setzen
                sLog:=sLog + sWertTyp + IntToStr (AktKontrollZaehler [KanalNr]) + CR + LF;

                // Wert für Kontrollzählerkanal:
                if (KanalNr >= Low (SatzData)) AND (KanalNr <= High (SatzData)) then begin
                  SatzData[KanalNr].WertAsDouble:=AktKontrollZaehler [KanalNr];
                  SatzData[KanalNr].WertAsString:=IntToStr (AktKontrollZaehler [KanalNr]);
                end;
              end
              else if cTrennz in ['t'..'w'] then begin  // Rohwert ist MP-Wert A..D (Inkrement)
                KanalNr:=Ord (cTrennz) - 115;
                // wenn Kontrollzähler nicht gesetzt ist (z.B. Kanalmaske geändert), kann
                // Kontrollzähler nicht fortgeschrieben werden und darf nicht gespeichert werden.
                // 19.07.2007, WW
                if AktKontrollZaehler [KanalNr] < 0 then begin  // Kontrollzähler nicht gesetzt
                  sLog:=sLog + sWertTyp + IntToStr (IBuf) + #9 +
                        'Kontrollzähler: ' +  Cungueltig + CR + LF;
                  bWert_speichern:=false;
                  // kein Fehler mehr (Result -9 nicht mehr verwendet); 19.07.2007, WW
                end
                else begin
                  // Kontrollzähler fortschreiben durch Aufaddieren des MP-Werts
                  AktKontrollZaehler [KanalNr]:=AktKontrollZaehler [KanalNr] + IBuf;
                  // Ausgabe: MP-Wert und aufaddierter Kontrollzähler
                  sLog:=sLog + sWertTyp + IntToStr (IBuf) + #9 +
                        'Kontrollzähler: ' +  IntToStr (AktKontrollzaehler[KanalNr]) + CR + LF;

                  // Wert für Kontrollzählerkanal:
                  if (KanalNr >= Low (SatzData)) AND (KanalNr <= High (SatzData)) then begin
                    SatzData[KanalNr].WertAsDouble:=AktKontrollZaehler [KanalNr];
                    SatzData[KanalNr].WertAsString:=IntToStr (AktKontrollZaehler [KanalNr]);
                  end;
                end;
              end
              else begin  // Rohwert ist Analogwert 1..4
                KanalNr:=Ord (cTrennz) - 28;
                // Wert für Analogkanal:
                if (KanalNr >= Low (SatzData)) AND (KanalNr <= High (SatzData)) then begin
                  siValue:=Int64AsSingle (IBuf);
                  SatzData[KanalNr].WertAsDouble:=siValue;
                  SBuf:=FloatToStr (siValue);
                  StrSubst(SBuf, ',', '.');  { Ausgabe fest mit Dezimal-Punkt }
                  SatzData[KanalNr].WertAsString:=SBuf;
                  sLog:=sLog + sWertTyp + SBuf + CR + LF;
                end;
              end;

              // wenn erster Kanalwert vorhanden ist, Zieldatei für Archivkanaldaten
              // ermitteln und File anlegen: }
              if not Assigned (FS_Konv[KanalNr]) then begin
                Pref:=prefix_DSfG_Ar;     { Prefix für DSfG-Archivdaten }
                if GetTempFileName (pchar (SMSKonvRec.ZielPfad), pchar (Pref), 0,
                                    pFileName) = 0 then begin  { Zieldatei anlegen }
                  Result:=-13;  // Fehler beim TempFile erzeugen; 31.01.2007, WW
                  exit;
                end;

                Filename:=string (pFileName);
                FS_Konv[KanalNr]:=TFileOfRecStream.Create (FileName, fmOpenReadWrite OR fmShareDenyWrite,
                                                           SizeOf (TDSfGSatzData));

                { Name der Konvertierungs-Datei mit Busadresse und DE-Adresse in
                  Archiv/Logbuch-DatenListe eintragen: }
                if SMSKonvRec.ArLbDatenListe <> nil then begin
                  DSfGDataListObj:=TDSfGDataListObj.Create;
                  { DE-Adresse für Kanal bilden: Kontrollzähler/Analogwerte des
                    MRG 900 sind der DSfG-Archivgruppe 1 zugeordnet
                    -> Hinweis: KanalNr enthält fortlaufende Nummern für die Impuls-/
                       Analogkanäle (Kanal 1..4: Impuls; Kanal 5..8: Analog).
                       Abweichend von Kanalnummerierung in DSfG-Stammdaten, dort
                       sind Kanal 7 der Zustandskanal und Kanal 8, 9 die weiteren
                       Analogkanäle ! }
                  if KanalNr < 7 then
                    DEL:='caa' + Chr (KanalNr + 101) + 'd' { KanalNr 1 -> caafd .. KanalNr 6 -> caakd }
                  else
                    DEL:='caa' + Chr (KanalNr + 102) + 'd'; { KanalNr 7 -> caamd .. KanalNr 8 -> caand }
                  DSfGDataListObj.SetData (EAdr_Reg, DEL);
                  SMSKonvRec.ArLbDatenListe.AddObject (FileName, DSfGDataListObj);
                end;
              end;

              // ...dito für Zustandskanal: }
              if not Assigned (FS_Konv_Zustand) then begin
                Pref:=prefix_DSfG_Ar;     { Prefix für DSfG-Archivdaten }
                if GetTempFileName (pchar (SMSKonvRec.ZielPfad), pchar (Pref), 0,
                                   pFileName) = 0 then begin  { Zieldatei anlegen }
                  Result:=-14;  // Fehler beim TempFile erzeugen; 31.01.2007, WW
                  exit;
                end;
                Filename:=string (pFileName);
                FS_Konv_Zustand:=TFileOfRecStream.Create (FileName, fmOpenReadWrite OR fmShareDenyWrite,
                                                          SizeOf (TDSfGSatzData));

                { Name der Konvertierungs-Datei mit Busadresse und DE-Adresse in
                  Archiv/Logbuch-DatenListe eintragen: }
                if SMSKonvRec.ArLbDatenListe <> nil then begin
                  DSfGDataListObj:=TDSfGDataListObj.Create;
                  { DE-Adresse für Zustandskanal in DSfG-Archivgruppe 1 bilden: }
                  DEL:='caald';  { Kanal 7 }
                  DSfGDataListObj.SetData (EAdr_Reg, DEL);
                  SMSKonvRec.ArLbDatenListe.AddObject (FileName, DSfGDataListObj);
                end;
              end;

              if bDatensatz_speichern AND bWert_speichern then begin
                // Kontrollzähler-Datensatz in Zieldatei schreiben:
                FS_Konv[KanalNr].WriteRec (SatzData[KanalNr]);
                // Zustandskanal-Datensatz in Zieldatei schreiben, wenn noch
                // nicht erfolgt; 19.07.2007, WW
                if bZustand_speichern then begin
                  bZustand_speichern:=false;
                  FS_Konv_Zustand.WriteRec (SatzData_Zustand);
                end;
              end;
            end;

            // neue Messperiode beginnt, wenn:
            if (cSMSData in ['t'..'w', '!'..'$']) AND  // ...Wert ein Messperiodenwert ist (Imp, Ana) und
               ((not (cTrennz in ['t'..'w', '!'..'$']) AND not z_Satz_gelesen) OR  // ...es der erste Messperiodenwert ist (Imp, Ana) oder
                 ((cSMSData <= cTrennz) AND  // ...sich der Messperiodenwert wiederholt (kleiner-Vergleich)
                  not ((cSMSData in ['!'..'$']) AND (cTrennz in ['t'..'w'])) AND  // kein kleiner-Vergleich bei Übergang Impuls -> Analog-MP-Wert
                  not ((cSMSData in ['!'..'$']) AND (cTrennz in ['p'..'s']) AND z_Satz_gelesen))) then begin  // kein kleiner-Vergleich bei Übergang Impuls -> Analog-Wert im z-Satz

              if (UnixSec_MP < 0) OR (Messperiodenlaenge_sec < 0) OR
                 (OrdnungsNr_Kopf < 0) then begin
                // Fehler: UnixSec_MP, Messperiodenlaenge_sec oder OrdnungsNr_Kopf
                // enthalten Vorbelegungswerte
                sLog:=sLog + 'Neue MP: ' + Cungueltig + CR + LF;
                Result:=-10;
                exit;
              end
              else begin  // Datum/Zeit und Ordnungsnummer der nächsten MP fortschreiben
                inc (UnixSec_MP, Messperiodenlaenge_sec);
                UnixSekundenToDateTime (UnixSec_MP, DatumZeit);
                DateTimeToUnixTimeStr (DatumZeit, SBuf);  // in Unix-Zeit wandeln
                inc (OrdnungsNr_Kopf);
                sLog:=sLog + 'Neue MP - Datum/Zeit: ' +
                      FormatDateTime ('dd.mm.yyyy hh:nn:ss', DatumZeit) +
                      '  Ordnungsnummer: ' + IntToStr(OrdnungsNr_Kopf) + CR + LF;

                // neuer Datensatz für alle Kanäle beginnt
                for k:=Low (SatzData) to High (SatzData) do begin
                  Init_SatzData (SatzData[k]);
                  SatzData[k].DatumZeit:=DatumZeit;
                  SatzData[k].UnixDatumZeit:=SBuf;
                  SatzData[k].OrdnungsNr:=OrdnungsNr_Kopf;
                  SatzData[k].Zeitzone:=sZeitzone;  // gemerktes Zeitzonen-Kennzeichen vom Kopf oder Sondersatz
                end;
                SatzData_Zustand:=SatzData[Low (SatzData)];
                // Wert für Zustandskanal: OK
                SatzData_Zustand.WertAsDouble:=CDSfGStatMRG900_OK;
                SatzData_Zustand.WertAsString:=IntToStr (CDSfGStatMRG900_OK);
                bZustand_speichern:=true;

                bDatensatz_speichern:=true;  // MP-Sätze immer speichern
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
      for k:=Low (FS_Konv) to High (FS_Konv) do
        if Assigned (FS_Konv[k]) then  // Zieldatei für Kanal ist geöffnet
          FS_Konv[k].Free;  // Datei schliessen

      if Assigned (FS_Konv_Zustand) then  // Zieldatei für Zustandskanal ist geöffnet
        FS_Konv_Zustand.Free;  // Datei schliessen
    end;
  except
    Result:=-11;
    exit;
  end;
end;

{--------------------------------------------------------------------------------------}
function KonvSMSKurzzeitwerte_MRG900_p_q (SMSKonvRec: TSMSKonvDSfG; var Kennung: string;
                                          var sLog: string): integer;
{--------------------------------------------------------------------------------------}
{ konvertiert MRG 905/910-SMS mit Werten im Minutenraster (neues Format p) oder
  Sekundenraster (neues Format q);
  -> Ziel: Ascii-Datei zur Weiterverarbeitung durch IEC-Kopplung !
  Ergebnis: 0 = OK
            Fehler, wenn < 0 }
const
  CTrennzSetImp = ['p'..'s'];  // Trennzeichen für Zählerstände (Impulskanal)
  CTrennzSetAna = ['!'..'$'];  // Trennzeichen für Analogkanäle

var
  S: string;
  OK: boolean;
  IBuf: Int64;
  UnixSec: integer;
  cTrennz: char;
  cSMSData: char;
  i: integer;
  DatumZeit: TDateTime;
  SatzData: TDSfGSatzData;
  FS_Konv: TFileOfRecStream;
  Pref: string;
  pFileName: array[0..255] of char;
  Filename: string;
  DSfGDataListObj: TDSfGDataListObj;
  DEL: string;
  sUnixTime: string;
  sZeitzone: string;
  EAdr_Reg: string;
  siValue: single;
  SBuf: string;
  sWertTyp: string;
  cTyp: char;

begin
  Result:=0;  // OK
  Kennung:='';  { Vorbelegung: Kennung unbekannt }
  sLog:='';     { Vorbelegung: Log-String leer }
  if length (SMSKonvRec.SMS_Data) = 0 then begin
    Result:=-1;
    exit;
  end;

  try
    S:=Copy (SMSKonvRec.SMS_Data, 2, 12);   { 2.-13. Zeichen: DSfG-Kennung }
    OK:=length (S) = 12;
    sLog:=sLog + 'DSfG-Kennung: ';
    if not OK then begin
      sLog:=sLog + CUngueltig + CR + LF;
      Result:=-2;
      exit;
    end else
      sLog:=sLog + S + CR + LF;
    Kennung:=S;

    S:=Copy (SMSKonvRec.SMS_Data, 14, 6);   { 14.-19. Zeichen: Datum/Zeit (Unix-Zeit) }
    if length (S) = 6 then
      OK:=Basis64CodeStrToInt (S, IBuf)
    else
      OK:=false;
    sLog:=sLog + 'Datum/Zeit: ';
    if not OK then begin
      sLog:=sLog + CUngueltig + CR + LF;
      Result:=-3;
      exit;
    end;
    UnixSec:=IBuf;

    { Zeitstempel: }
    UnixSekundenToDateTime (UnixSec, DatumZeit);
    sLog:=sLog + FormatDateTime ('dd.mm.yyyy hh:nn:ss', DatumZeit) + CR + LF;
    DateTimeToUnixTimeStr (DatumZeit, sUnixTime);  // in Unix-Zeit wandeln

    S:=Copy (SMSKonvRec.SMS_Data, 20, 1);   { 20. Byte: Zeitzonen-Kennzeichen }
    sLog:=sLog + 'Zeitzone: ';
    if S = 'r' then begin  // Satz nach Neustart des MW-SMS-Systems
      sZeitzone:='';  // keine Zeitzonen-Info vorhanden
    end
    else if UpperCase(S) = 'M' then
      sZeitzone:=CMEZ   // Winterzeit
    else if UpperCase(S) = 'S' then
      sZeitzone:=CMESZ  // Sommerzeit
    else begin  // ungültiges Zeitzonen-Kennzeichen
      sLog:=sLog + CUngueltig + CR + LF;
      Result:=-4;
      exit;
    end;
    sLog:=sLog + S + CR + LF;

    S:=Copy (SMSKonvRec.SMS_Data, 21, 1);   { 21. Zeichen: Raster (min oder sec) wird nur für Log verwendet }
    if length (S) = 1 then
      OK:=Basis64CodeStrToInt (S, IBuf)
    else
      OK:=false;

    cTyp:=SMSKonvRec.SMS_Data [1];  // Typ-Zeichen; 17.07.2007 WW
    if cTyp = 'p' then
      sLog:=sLog + 'Minutenraster: '
    else if cTyp = 'q' then
      sLog:=sLog + 'Sekundenraster: '
    else
      sLog:=sLog + 'Undefiniertes Raster: ';
    if not OK then begin
      sLog:=sLog + CUngueltig + CR + LF;
      Result:=-8;
      exit;
    end else
      sLog:=sLog + IntToStr(IBuf) + CR + LF;

    S:=Copy (SMSKonvRec.SMS_Data, 22, 1);   { 22. Byte: Adresse der Reg.Instanz }
    OK:=length(S) = 1;
    sLog:=sLog + 'Adresse der Registrierinstanz: ';
    if not OK then begin
      sLog:=sLog + CUngueltig + CR + LF;
      Result:=-5;
      exit;
    end else
      sLog:=sLog + S + CR + LF;
    EAdr_Reg:=S;

    // Vorbelegungen:
    S:='';
    cTrennz:=NUL;

    for i:=23 to length (SMSKonvRec.SMS_Data) do begin   // Reststring ab Minutenraster bis zum Ende
      Application.ProcessMessages;

      cSMSData:=SMSKonvRec.SMS_Data[i];
      // neues Trennzeichen gefunden oder Stringende:
      if (cSMSData in CTrennzSetImp) OR (cSMSData in CTrennzSetAna) OR
         (i = length (SMSKonvRec.SMS_Data)) then begin

        if i = length (SMSKonvRec.SMS_Data) then  // bei Stringende letztes Zeichen anhängen
          S:=S + SMSKonvRec.SMS_Data[i];

        if (cTrennz in CTrennzSetImp) OR (cTrennz in CTrennzSetAna) then begin
          { für Log: }
          case cTrennz of
            'p': sWertTyp:='Kontrollzähler A: ';  // Trennzeichen Kontrollzähler A
            'q': sWertTyp:='Kontrollzähler B: ';  // Trennzeichen Kontrollzähler B
            'r': sWertTyp:='Kontrollzähler C: ';  // Trennzeichen Kontrollzähler C
            's': sWertTyp:='Kontrollzähler D: ';  // Trennzeichen Kontrollzähler D
            '!': sWertTyp:='Analogwert 1: ';      // Trennzeichen Analogwert 1
            '"': sWertTyp:='Analogwert 2: ';      // Trennzeichen Analogwert 2
            '#': sWertTyp:='Analogwert 3: ';      // Trennzeichen Analogwert 3
            '$': sWertTyp:='Analogwert 4: ';      // Trennzeichen Analogwert 4
          else
            sWertTyp:='Unbekannter Wert: ';
          end;

          // Rohwert (Kontroll-Zählerstand oder Analogwert) verarbeiten
          if length (S) > 0 then
            OK:=Basis64CodeStrToInt (S, IBuf)
          else
            OK:=false;
          if not OK then begin
            sLog:=sLog + sWertTyp + CUngueltig + CR + LF;
            Result:=-6;
            exit;
          end;

          // wenn Kanalwert vorhanden ist, in Zieldatei schreiben: }
          Pref:='DKW';     { Prefix für DSfG-Kurzzeitwerte }
          if GetTempFileName (pchar (SMSKonvRec.ZielPfad), pchar (Pref), 0,
                              pFileName) = 0 then begin  { Zieldatei anlegen }
            Result:=-7;  // Fehler beim TempFile erzeugen; 31.01.2007, WW
            exit;
          end;
          Filename:=string (pFileName);
          FS_Konv:=TFileOfRecStream.Create (FileName, fmOpenReadWrite OR fmShareDenyWrite,
                                            SizeOf (TDSfGSatzData));
          try
            { Name der Konvertierungs-Datei mit Busadresse und DE-Adresse in
              Archiv/Logbuch-DatenListe eintragen: }
            if SMSKonvRec.ArLbDatenListe <> nil then begin
              DSfGDataListObj:=TDSfGDataListObj.Create;
              { DE-Adresse für Kanal bilden:
                -> Kurzzeitwerte des MRG 900 sind der DSfG-Archivgruppe 1 zugeordnet }
              if cTrennz in CTrennzSetImp then  // Impulskanal
                DEL:='caa' + Chr (Ord (cTrennz) - 10) + 'd'  { p -> caafd, q -> caagd usw.}
              else begin // Analogkanal
                { Hinweis: in DSfG-Stammdaten ist Kanal 7 der Zustandskanal (caald)
                           und Kanal 8, 9 die weiteren Analogkanäle ! }
                if cTrennz in ['!', '"'] then // Analogkanal 1, 2
                  DEL:='caa' + Chr (Ord (cTrennz) + 73) + 'd'  { ! -> caajd, " -> caakd }
                else
                  DEL:='caa' + Chr (Ord (cTrennz) + 74) + 'd'; { # -> caamd, $ -> caand }
              end;
              DSfGDataListObj.SetData (EAdr_Reg, DEL);
              SMSKonvRec.ArLbDatenListe.AddObject (FileName, DSfGDataListObj);
            end;

            SatzData.OrdnungsNr:=-1;  // gibts nicht, muß bei Bedarf extern ersatzmäßig gebildet werden
            SatzData.DatumZeit:=DatumZeit;
            SatzData.UnixDatumZeit:=sUnixTime;
            if cTrennz in CTrennzSetImp then begin // Impulskanal
              SatzData.WertAsDouble:=IBuf;
              SatzData.WertAsString:=IntToStr (IBuf);
            end
            else begin  // Analogkanal
              siValue:=Int64AsSingle (IBuf);
              SatzData.WertAsDouble:=siValue;
              SBuf:=FloatToStr (siValue);
              StrSubst(SBuf, ',', '.');  { Ausgabe fest mit Dezimal-Punkt }
              SatzData.WertAsString:=SBuf;
            end;
            SatzData.Status:='';  // Ersatzwert, da kein Status in der Messwert-SMS enthalten ist
            SatzData.CRC:='';     // Ersatzwert, da kein CRC in der Messwert-SMS enthalten ist
            SatzData.Zeitzone:=sZeitzone;

            sLog:=sLog + sWertTyp + SatzData.WertAsString + CR + LF;

            FS_Konv.WriteRec (SatzData);
          finally
            FS_Konv.Free;
          end;
        end;

        cTrennz:=cSMSData;  // neu gefundenes Trennzeichen setzen
        S:='';  // Rohwert neu initialisieren
      end else
        S:=S + SMSKonvRec.SMS_Data[i];
    end;  { for i }
  except
    Result:=-9;
    exit;
  end;
end;

{------------------------------------------------------------------------------}
function KonvSMSMeldung_MRG900_r (SMSKonvRec: TSMSKonvDSfG; var Kennung: string;
                                  var sLog: string): integer;
{------------------------------------------------------------------------------}
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
  sLog:='';     { Vorbelegung: Log-String leer }
  if length (SMSKonvRec.SMS_Data) = 0 then begin
    Result:=-1;
    exit;
  end;

  try
    { Gerätetyp (2.-7. Zeichen), Stationsname (9.-24. Zeichen), MRG-Kennung (26.-39. Zeichen):
      wird nicht in DSfG-Daten konvertiert }
    S:=Copy (SMSKonvRec.SMS_Data, 2, 6);  { 2.-7. Zeichen: Gerätetyp }
    OK:=length (S) = 6;
    if not OK then begin
      sLog:=sLog + 'Gerätetyp: ' + Cungueltig + CR + LF;
      Result:=-13;
      exit;
    end else
      sLog:=sLog + 'Gerätetyp: ' + S + CR + LF;

    S:=Copy (SMSKonvRec.SMS_Data, 9, 16);  { 9.-24. Zeichen: Stationsname }
    OK:=length (S) = 16;
    if not OK then begin
      sLog:=sLog + 'Stationsname: ' + Cungueltig + CR + LF;
      Result:=-14;
      exit;
    end else
      sLog:=sLog + 'Stationsname: ' + S + CR + LF;

    S:=Copy (SMSKonvRec.SMS_Data, 26, 14);  { 26.-39. Zeichen: MRG-Kennung }
    OK:=length (S) = 14;
    if not OK then begin
      sLog:=sLog + 'MRG-Kennung: ' + Cungueltig + CR + LF;
      Result:=-15;
      exit;
    end else
      sLog:=sLog + 'MRG-Kennung: ' + S + CR + LF;

    S:=Copy (SMSKonvRec.SMS_Data, 41, 10);  { 41.-50. Zeichen: Datum }
    OK:=EncodeDateStr (S, 'DD.MM.YYYY', dtDatum);
    if not OK then begin
      sLog:=sLog + 'Datum: ' + CUngueltig + CR + LF;
      Result:=-2;
      exit;
    end else
      sLog:=sLog + 'Datum: ' + S + CR + LF;

    S:=Copy (SMSKonvRec.SMS_Data, 52, 8);  { 52.-59. Zeichen: Zeit }
    OK:=EncodeTimeStr (S, 'HH:MM:SS', dtZeit);
    if not OK then begin
      sLog:=sLog + 'Zeit: ' + CUngueltig + CR + LF;
      Result:=-3;
      exit;
    end else
      sLog:=sLog + 'Zeit: ' + S + CR + LF;

    SatzData.DatumZeit:=dtDatum + dtZeit;
    DateTimeToUnixTimeStr (SatzData.DatumZeit, S);  // in Unix-Zeit wandeln
    SatzData.UnixDatumZeit:=S;

    S:=Copy (SMSKonvRec.SMS_Data, 60, 1);  { 60. Zeichen: Trennzeichen für Zeitzone (optional) oder
                                             Trennzeichen zu Meldungstext }
    if (S = ',') OR (S = ';') then begin  { Zeitzonen-Trennzeichen }
      SBuf:=Copy (SMSKonvRec.SMS_Data, 62, length (SMSKonvRec.SMS_Data));  { Rest bis zum Ende }
      S:=F_Zerlegen (SBuf, CTrennzeichen);  { bis zum nächsten Trennzeichen: Zeitzone }
      if S = '' then
        SatzData.Zeitzone:=''     // keine Zeitzonen-Info vorhanden
      else if S = 'MEZ' then
        SatzData.Zeitzone:=CMEZ   // Winterzeit
      else if S = 'MESZ' then
        SatzData.Zeitzone:=CMESZ  // Sommerzeit
      else begin  // ungültiges Zeitzonen-Kennzeichen
        sLog:=sLog + 'Zeitzone: ' + CUngueltig + CR + LF;
        Result:=-4;
        exit;
      end;
      sLog:=sLog + 'Zeitzone: ' + S + CR + LF;
    end else
      SBuf:=Copy(SMSKonvRec.SMS_Data, 61, length (SMSKonvRec.SMS_Data));  { Rest bis zum Ende }

    S:=Copy (SBuf, 1, 16);  { 1.-16. Zeichen des Reststrings: Meldungstext (wird nicht in DSfG-Daten konvertiert) }
    OK:=length (S) = 16;
    if not OK then begin
      sLog:=sLog + 'Meldungstext: ' + CUngueltig + CR + LF;
      Result:=-12;
      exit;
    end else
      sLog:=sLog + 'Meldungstext: ' + S + CR + LF;

    SBuf:=Copy (SBuf, 18, length (SMSKonvRec.SMS_Data));  { Rest bis zum Ende }
    S:=F_Zerlegen (SBuf, CTrennzeichen);  { bis zum nächsten Trennzeichen: DSfG-Meldungsnummer }
    SatzData.WertAsString:=S;
    try
      Val (S, SatzData.WertAsDouble, Code);
    except
      sLog:=sLog + 'DSfG-Meldungsnummer: ' + CUngueltig + CR + LF;
      Result:=-10;
      exit;
    end;
    sLog:=sLog + 'DSfG-Meldungsnummer: ' + S + CR + LF;

    S:=F_Zerlegen (SBuf, CTrennzeichen);  { bis zum nächsten Trennzeichen: DSfG-Ordnungsnummer }
    try
      Val (S, SatzData.OrdnungsNr, Code);
    except
      sLog:=sLog + 'DSfG-Ordnungsnummer: ' + CUngueltig + CR + LF;
      Result:=-5;
      exit;
    end;
    sLog:=sLog + 'DSfG-Ordnungsnummer: ' + S + CR + LF;

    S:=F_Zerlegen (SBuf, CTrennzeichen);  { bis zum nächsten Trennzeichen: Adresse der Reg.Instanz }
    OK:=length (S) = 1;
    if not OK then begin
      sLog:=sLog + 'Adresse der Registrierinstanz: ' + CUngueltig + CR + LF;
      Result:=-6;
      exit;
    end else
      sLog:=sLog + 'Adresse der Registrierinstanz: ' + S + CR + LF;
    EAdr_Reg:=S;

    S:=F_Zerlegen (SBuf, CTrennzeichen);  { bis zum nächsten Trennzeichen: Adresse der Wieser-Instanz }
    OK:=length (S) = 1;
    if not OK then begin
      sLog:=sLog + 'Adresse der Wieser-Instanz: ' + CUngueltig + CR + LF;
      Result:=-7;
      exit;
    end else
      sLog:=sLog + 'Adresse der Wieser-Instanz: ' + S + CR + LF;
    EAdr_Quelle:=S[1];

    { Rest: DSfG-Kennung }
    OK:=length (SBuf) = 12;
    if not OK then begin
      sLog:=sLog + 'DSfG-Kennung: ' + CUngueltig + CR + LF;
      Result:=-8;
      exit;
    end else
      sLog:=sLog + 'DSfG-Kennung: ' + SBuf + CR + LF;
    Kennung:=SBuf;  // für Rückgabe

    SatzData.Status:='';  // Ersatzwert, da kein Status in der Meldungs-SMS enthalten ist
    SatzData.CRC:='';     // Ersatzwert, da kein CRC in der Meldungs-SMS enthalten ist


    { Zieldateiname für Logbuch-Daten ermitteln und File anlegen: }
    Pref:=prefix_DSfG_Lb;     { Prefix für DSfG-Logbuchdaten }
    if GetTempFileName (pchar (SMSKonvRec.ZielPfad), pchar (Pref), 0,
                        pFileName) = 0 then begin  { Zieldatei anlegen }
      Result:=-11;  // Fehler beim TempFile erzeugen; 31.01.2007, WW
      exit;
    end;
    Filename:=string (pFileName);
    FS_Konv:=TFileOfRecStream.Create (FileName, fmOpenReadWrite OR fmShareDenyWrite,
                                      SizeOf (SatzData));
    try
      FS_Konv.WriteRec (SatzData);  { Meldungssatz schreiben }
    finally
      FS_Konv.Free;
    end;

    { Name der Konvertierungs-Datei mit Busadresse und DE-Adresse in Archiv/Logbuch-DatenListe eintragen: }
    if SMSKonvRec.ArLbDatenListe <> nil then begin
      DSfGDataListObj:=TDSfGDataListObj.Create;
      { DE-Adresse für Logbuch aus Quell-Adresse bilden: }
      if (EAdr_Quelle >= Low (CLogbuchDEL)) AND (EAdr_Quelle <= High (CLogbuchDEL)) then
        DEL:=CLogbuchDEL [EAdr_Quelle]+'d'  // DEL-Adresse für Logbuch
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
                             var Datentyp: integer; var sLog: string;
                             isGPRS: boolean): integer;
{-------------------------------------------------------------------------}
{ konvertiert Datenteil einer SMS aus MRG 905/910 in DSfG-Datenstruktur;
  Ergebnis: 0 = OK
            Fehler, wenn < 0 }
var
  sLogKonv: string;
  cTyp: char;

begin
  Result:=-99;
  Kennung:='';  { Vorbelegung: Kennung unbekannt }
  Datentyp:=0;  { Vorbelegung: Datentyp unbekannt }
  sLog:='Gerätetyp: MRG 900' + CR + LF;
  if length (SMSKonvRec.SMS_Data) = 0 then begin
    sLog:=sLog + 'Keine Rohdaten vorhanden' + CR + LF;
    exit;
  end;

  { SMS-Daten vom Typ v (Messwerte), r (Meldungen), p (Kurzzeitwerte im Minutenraster)
    und q (Kurzzeitwerte im Sekundenraster) können konvertiert werden: }
  cTyp:=SMSKonvRec.SMS_Data [1];  // Typ-Zeichen
  if cTyp = 'r' then begin  // neues Format Meldungen
    Datentyp:=C_IsLogbuecher;
    Result:=KonvSMSMeldung_MRG900_r (SMSKonvRec, Kennung, sLogKonv);
    sLog:=sLog + 'Typ: r' + CR + LF + sLogKonv;
  end
  else if cTyp = 'v' then begin  // neues Format Messwerte
    Datentyp:=C_IsArchive;
    Result:=KonvSMSMesswerte_MRG900_v (SMSKonvRec, Kennung, sLogKonv);
    sLog:=sLog + 'Typ: v' + CR + LF + sLogKonv;
  end
  else if isGPRS AND ((cTyp = 'p') OR (cTyp = 'q')) then begin
    // neues Format Kurzzeitwerte (im Minutenraster oder Sekundenraster) nur bei GPRS-Daten
    Datentyp:=C_IsKurzzeitwerte;
    Result:=KonvSMSKurzzeitwerte_MRG900_p_q (SMSKonvRec, Kennung, sLogKonv);
    sLog:=sLog + 'Typ: ' + cTyp + CR + LF + sLogKonv;
  end else
    sLog:=sLog + 'Typ: unbekannt' + CR + LF;
end;

{------------------------------------------------------------------------------}

{---------------------------------------------------------------------}
function KonvSMS_DSfG (SMSKonvRec: TSMSKonvDSfG; var Kennung: string;
                       var Datentyp: integer; var KonvErg: integer;
                       var sLog: string; isGPRS: boolean): boolean;
{---------------------------------------------------------------------}
{ konvertiert SMS-Daten in DSfG-Zwischendatei für Archivkanal- oder Logbuchdaten;
  Übergabe: SMS-Konvertierungsrecord
            Wieser-System-Pfad
            Flag 'isGPRS' (auf true setzen, wenn Daten aus GPRS-Empfang konvertiert
                           werden sollen)
  Rückgabe: Kennung des SMS-Senders
            Typ der SMS-Daten (Archivdaten, Logbuchdaten)
            Konvertierungsergebnis
            Log-String der konvertierten Daten
  Ergebnis: true, wenn Konvertierung OK }
begin
  Kennung:='';  { Vorbelegung: Kennung unbekannt }
  Datentyp:=0;  { Vorbelegung: Datentyp unbekannt }
  KonvErg:=-100;  { Vorbelegung: Gerätetyp unbekannt }

  { SMS-Datenteil gerätetypabhängig konvertieren: }
  case SMSKonvRec.GeraeteTyp of
    mrgtyp_MRG910: KonvErg:=KonvSMSData_MRG900 (SMSKonvRec, Kennung, Datentyp,
                                                sLog, isGPRS);
  else
    sLog:='Gerätetyp: unbekannt' + CR + LF;
  end;

  { Kennung: abschließende Leerzeichen wegschneiden wie bei DFÜ }
  Kennung:=F_RightTrunc (Kennung, ' ');
  Result:=KonvErg = 0;
end;

end.
