{*********************************************************************************}
{* Unit: Langzeitdaten-Aufbereitung                                              *}
{* 26.11.1998 WW  abgeleitet von Aufbereitung f¸r 16-Bit-Win-DF‹                 *}
{*                zus‰tzl. Aufbereitung in alte Struktur f¸r LGZ-Meldungen (GVT) *}
{* 06.02.2001 GD  erweitert um automatischen Export                              *}
{* 14.05.2002 WW  Prozeduraufrufe mit ‹bergaberecord TAufberRec, Messwert-       *}
{*                Aufbereitung zus‰tzlich mit TADataInfo-R¸ckgabe                *}
(* 17.09.2002 H.-P.R. dto f¸r IFDEF GVT						 *)
{* 12.11.2002 H.-P.R. Toleranzbereich auf 3 Jahre zur¸ck erweitert 		 *}
{* 03.03.2003 WW  Umstellung auf Stundensatz-Zwischendateiformat RohSRec;        *}
{*                ‹bergabe-Record um Quelldateinamen erweitert                   *}
{* 23.05.2003 GD  Anpassung ASCII-Export an ge‰ndertes Rohdatenformat             *}
{* 23.09.2004 WW  Umstellung auf Tagessatz-Zwischendateiformat RohTRec           *}
{ 10.01.2007  GD  Abhh‰ngigkeit von EXPORTSKRIPT.PAS entfernt                  }
{*********************************************************************************}
unit MLGZAufb;

INTERFACE

Uses
  Classes, Forms, SysUtils, T_Tools, T_Zeit, MFileNam, Novell, UpUmfo, LGZType,
  MDBSta, MDBMrg, MLGZVerw, PathIni, WSyscon, AsciiExportDLL, LGZUtil;

type

  { ‹bergaberecord f¸r Aufbereitung }

  TAufberRec = record
    StdQuelldateiName: TFileName;  { vollst‰ndiger Pfadname der Zwischendatei Stundens‰tze }
    TagQuelldateiName: TFileName;  { vollst‰ndiger Pfadname der Zwischendatei Tagess‰tze }
    PrfQuelldateiName: TFileName;  { vollst‰ndiger Pfadname der Zwischendatei Pr¸fungss‰tze }
    MrgId: TMrgId;                 { eindeutige Nummer der Station }
    Select: integer;               { 1 = Automatik-Daten/ 0 = manuelle Daten }
    Kanalzahl: word;               { Kanalzahl der LGZ-Dateien }
    loeschen: boolean;             { Quelldateien nach Konvertierung loeschen }
  end;


function Mess_Aufb (AufberRec: TAufberRec; ManuAppend: boolean;
                    var MWDataInfo: TDataKonvInfo;
                    var TADataInfo: TDataKonvInfo): integer;
(*function Mess14_Aufb (MrgId: TMrgId; Select: integer;
                      var MWDataInfo: TMWDataInfo): integer;*)
function Pruef_Aufb (AufberRec: TAufberRec): integer;
{$IFDEF GVT}
function Meld_Aufb_GVT (AufberRec: TAufberRec): integer;
{$ENDIF}

IMPLEMENTATION

var
  LGZSRecSize: integer;
  LGZTRecSize: integer;
{$IFDEF GVT}
  lgz_pos: longint;    { ab lgz_pos werden Meldungen in LGZ-Datei geschrieben }
{$ENDIF}


{ DatumPlausibel: Datum des Stundensatzes muﬂ folgende Kriterien erf¸llen
                  1. Es darf nicht mehr als zwei Stunden in der Zukunft liegen
                  2. Es darf nicht mehr als drei Jahre in der Vergangenheit liegen
                  3. Es muﬂ ein Wert zur vollen Stunde sein }
{------------------------------------------------------}
function DatumPlausibel (StundenSatz: LGZSRec): Boolean;
{------------------------------------------------------}
var
  vgldatum : daterec;
  vglzeit  : timerec;
  PCDatum: DateRec;     { aktuelles Datum }
  PCZeit: TimeRec;      { aktuelle Zeit }
begin
  DatumPlausibel := false;
  DecodeDate(Date, word(PCDatum.Year), word(PCDatum.Month), word(PCDatum.Day));
  DecodeTime(Time, word(PCZeit.Hour), word(PCZeit.Min), word(PCZeit.Sec), word(PCZeit.HSec));
  PCZeit.HSec:=PCZeit.HSec DIV 10;  { hsec in TimeRec }

  with stundensatz, datum, zeit do
  begin
    if (day >= 1) and (day <= F_LetztTag (datum)) and
       (month >= 1) and (month <= 12) and
       (year-1900 >= 80) and (year-1900 <=200) and
       (hour >= 0) and (hour <= 23) and
       (min = 0) and
       (sec = 0) and
       (hsec = 0) then
    begin
      { Toleranzbereich ? }
      vgldatum := PCDatum;
      vglZeit := PCZeit;
      P_Addminuten (vgldatum, vglzeit, 120);   { 2 Stunden Zukunft }
      { Datum des Satzes darf nicht mehr aks zwei Stunden in der Zukunft liegen }
      if CmpDate (vgldatum, datum) + CmpTime (vglzeit, zeit) < 0 then
        exit;
      vgldatum := PCDatum;
      vglZeit := PCZeit;
      P_Addminuten (vgldatum, vglzeit, -1576800);   { 1 Jahr Vergangenheit }
      { Datum des Satzes darf nicht mehr als drei Jahre in der Vergangenheit liegen }
      if CmpDate (vgldatum, datum) + Cmptime (vglzeit, zeit) > 0 then
        exit;
    end
    else
      exit;   { Datum/Zeit realistisch ? }
  end;     { with record.. }
  DatumPlausibel := true;
end;

{ CompareStdSatz: Liefert True, wenn beide Stundens‰tze absolut identisch sind }
{------------------------------------------------------------------------}
function CompareStdSatz (Satz1, Satz2: LGZSRec; Kanalzahl: word): boolean;
{------------------------------------------------------------------------}
var
  CmpZeit: integer;
  i:       byte;    { Index fÅr Kanalzahl }
  CmpSatz: boolean;
begin
  CmpSatz := false;
  CmpZeit := CmpDate (Satz1.Datum, Satz2.Datum) +
             CmpTime (Satz1.Zeit, Satz2.Zeit);
  if (CmpZeit = 0) then
  begin
    i := 1;
    CmpSatz := true;
    while (i <= KanalZahl) and CmpSatz do
      if (Satz1.Kanal [i].Kanalstatus = Satz2.Kanal [i].Kanalstatus) and
         (Satz1.Kanal [i].Wert = Satz2.Kanal [i].Wert) then
        inc (i)
      else
        CmpSatz := false;
  end;
  CompareStdSatz := CmpSatz;
end;

{ WriteStd_FehlerSatz: Stundensatz wird in Fehlerdatei geschrieben
                       AufberRec.Select = 0: ‹berschreiben
                       AufberRec.Select = 1: Erg‰nzen }
{-----------------------------------------------------------------------------}
procedure WriteStd_Fehlersatz (AufberRec: TAufberRec;
                               var Fehlerdatei: TFileStreamExt; Satz: LGZSRec);
{-----------------------------------------------------------------------------}
var
  eh_scho_da: boolean;
  Testsatz: LGZSRec;
  FehlDatPos: longint;
begin
  case AufberRec.select of
    0: Fehlerdatei.WriteBuffer (Satz, LGZSRecSize); { Fehlerdatei ¸berschreiben, }
    1: begin                                                   { sonst fortschreiben }
         if Fehlerdatei.Size = 0 then
           Fehlerdatei.WriteBuffer (Satz, LGZSRecSize)
         else
         begin                               { Datei von hinten durchsuchen }
           Fehlerdatei.Seek (-LGZSRecSize, soFromEnd);
           FehlDatPos := Fehlerdatei.Position;
           eh_scho_da := false;
           while (FehlDatPos >= 0) and not eh_scho_da do
           begin
             Fehlerdatei.Seek (FehlDatPos, soFromBeginning);
             Fehlerdatei.ReadBuffer (Testsatz, LGZSRecSize);
             if CompareStdSatz (Testsatz, Satz, AufberRec.Kanalzahl) then
               eh_scho_da := true;
             FehlDatPos := Fehlerdatei.Position - 2*LGZSRecSize;
           end;
           if not eh_scho_da then        { neuen Satz ans Ende schreiben }
           begin
             Fehlerdatei.Seek(0, soFromEnd);
             Fehlerdatei.WriteBuffer(Satz, LGZSRecSize);
           end;
         end;
       end;
  end;
end;

{------------------------------------------------------------------------}
function Std_Aufbereitung (AufberRec: TAufberRec; const DateiNummer: word;
                           var StaData: TStaData;
                           var MWDataInfo: TDataKonvInfo;
                           ManuAppend: boolean): integer;
{------------------------------------------------------------------------}
type
  Dat_Zeit_Typ = record
    Datum : DateRec;
    Zeit  : TimeRec;
  end;

var
  PCDatum: DateRec;
  PCZeit: TimeRec;
  TypsDatei: TFileStreamExt;         { LGZ-Datendatei }
  Typsdat, MerkDat : LGZSRec;
  ZwDatei: File of RohSRec;          { Zwischendatei RohSRec-Struktur }
  ZwSatz: RohSRec;
  Alt,Letzt: Dat_Zeit_Typ;
  MerkZeiger: LongInt;
  AltEndZeiger: longint;
  ErsterSatz: LGZSRec;               { 1. abgerufener Datensatz }
  first: boolean;
  QuellFileName: TFileName;
{$IFDEF FEHL_KONV}
  Fehlerdatei: TFileStreamExt;
{$ENDIF}
  LetztSatz: LGZSRec;
  Ok: boolean;
  AsciiExportStart : TDateTime;  // 06.02.2001

begin
  Result:=0;
  with MWDataInfo do begin
    NeueDaten:=false;
    DatenFehlen:=false;
    SollVon:=0; SollBis:=0; IstVon:=0; IstBis:=0;
  end;

  DecodeDate(Date, word(PCDatum.Year), word(PCDatum.Month), word(PCDatum.Day));
  DecodeTime(Time, word(PCZeit.Hour), word(PCZeit.Min), word(PCZeit.Sec), word(PCZeit.HSec));
  PCZeit.HSec:=PCZeit.HSec DIV 10;  { hsec in TimeRec }

  { Quelle: Zwischendatei aus Konvertierung }
  QuellFileName:=AufberRec.StdQuelldateiName;
  if not (F_Check (QuellFileName) > 0) then begin
    if AufberRec.loeschen then
      DeleteFile (QuellFileName);
    Result := 2;  {1}
    exit;
  end;

  AssignFile (ZwDatei, QuellFileName);
  RESET (ZwDatei);

  if AufberRec.select = 1 then              { Ziel: \LANGZEIT\LGZDxxxx.DAT }
  begin
    if FileExists (GetLGZDateiName(Dateinummer, dd_mess)) then
      TypsDatei:=TFileStreamExt.Create (GetLGZDateiName(Dateinummer, dd_mess),
                                        fmOpenReadWrite OR fmShareDenyWrite, Ok)
    else
      TypsDatei:=TFileStreamExt.Create (GetLGZDateiName(Dateinummer, dd_mess),
                                        fmCreate OR fmShareDenyWrite, Ok);

{$IFDEF FEHL_KONV}
    if FileExists (GetLGZFehlerDateiName(Dateinummer, dd_mess)) then
      FehlerDatei:=TFileStreamExt.Create (GetLGZFehlerDateiName(Dateinummer, dd_mess),
                                          fmOpenReadWrite OR fmShareDenyWrite, Ok)
    else
      FehlerDatei:=TFileStreamExt.Create (GetLGZFehlerDateiName(Dateinummer, dd_mess),
                                          fmCreate OR fmShareDenyWrite, Ok);
{$ENDIF}

  end
  else                                           { Ziel: \MANU\*.DAT }
  begin
    if ManuAppend then
      TypsDatei:=TFileStreamExt.Create (GetManuDateiName(StaData.MrgId, dd_mess),
                                        fmOpenReadWrite OR fmShareDenyWrite, Ok)
    else
      TypsDatei:=TFileStreamExt.Create (GetManuDateiName(StaData.MrgId, dd_mess),
                                        fmCreate OR fmShareDenyWrite, Ok);

{$IFDEF FEHL_KONV}
    if ManuAppend then
      FehlerDatei:=TFileStreamExt.Create (GetManuFehlerDateiName(StaData.MrgId, dd_mess),
                                          fmOpenReadWrite OR fmShareDenyWrite, Ok)
    else
      FehlerDatei:=TFileStreamExt.Create (GetManuFehlerDateiName(StaData.MrgId, dd_mess),
                                          fmCreate OR fmShareDenyWrite, Ok);
{$ENDIF}

  end;

  IF TypsDatei.Size = 0 THEN
  BEGIN
    Alt.Datum.Year:=0; { Christi Geburt }
    Alt.Datum.Month:=0;
    Alt.Datum.Day:=0;
    Alt.Zeit.Hour:=0;
    Alt.Zeit.Min:=0;
    Alt.Zeit.Sec:=0;
    Alt.Zeit.HSec:=0;
    Letzt:=Alt;
    AltEndZeiger:=-1;
  END
  ELSE
  BEGIN
    TypsDatei.Seek(0, soFromBeginning);
    TypsDatei.ReadBuffer(TypsDat, LgzSRecSize);
    Alt.Datum:=TypsDat.Datum;
    Alt.Zeit:=TypsDat.Zeit;
    TypsDatei.Seek(-LgzSRecSize, soFromEnd);
    Typsdatei.ReadBuffer(Typsdat, LgzSRecSize);
    Letzt.Datum:=Typsdat.Datum;
    Letzt.Zeit:=Typsdat.Zeit;
    AltEndZeiger:=Zeiger(Alt.datum,Letzt.datum,Alt.zeit,Letzt.zeit);
  END;
  TypsDatei.Seek(0, soFromBeginning);

  // 06.02.2001
  if (Letzt.Datum.Month = 0)
  then AsciiExportStart := 0
  else AsciiExportStart :=
    EncodeDate(Letzt.Datum.Year, Letzt.Datum.Month, Letzt.Datum.Day) +
    EncodeTime(Letzt.Zeit.Hour, Letzt.Zeit.Min, Letzt.Zeit.Sec, 0);

  first:=true;
  WHILE Not Eof(ZwDatei) DO
  BEGIN
    Application.ProcessMessages;

    {$I-}
    read(ZwDatei,ZwSatz);
    {$I+}
    IF ioresult=0 THEN
    BEGIN
      { Feldwerte-Zuweisung: RohSRec-Satz aus Zwischendatei -> LGZSRec-Satz f¸r Zieldatei }
      typsdat.SatzStatus:=ZwSatz.SatzStatus;
      DateTimeToRec (ZwSatz.DatumZeit, typsdat.Datum, typsdat.Zeit);
      typsdat.Kanal:=ZwSatz.Kanal;

      if first and DatumPlausibel(Typsdat) then
      begin
        ErsterSatz:=typsdat;  { 1. Satz merken }
        first:=false;
      end;
      IF (Alt.Datum.Year=0) and DatumPlausibel(Typsdat) THEN
      BEGIN
        Alt.Datum:=TypsDat.Datum;
        Alt.Zeit:=TypsDat.Zeit;
        Letzt.Datum:=Alt.Datum;
        Letzt.Zeit:=Alt.Zeit;

        TypsDatei.WriteBuffer(Typsdat, LgzSRecSize);

        if Typsdat.Satzstatus < $80 then begin                        { kein fehlender Satz }
          if not MWDataInfo.NeueDaten then begin             { f¸r Datenzeitbereich: IstVon }
            with Typsdat do
              MWDataInfo.IstVon:=EncodeDate (Datum.Year, Datum.Month, Datum.Day) +
                                             EncodeTime (Zeit.Hour, Zeit.Min, Zeit.Sec, 0);
            MWDataInfo.NeueDaten:=true;
          end;
          LetztSatz:=Typsdat;            { merken f¸r "Ist-Bis" in MWDataInfo }
        end else
          MWDataInfo.DatenFehlen:=true;
      END
      ELSE
      BEGIN
        MerkZeiger:=Zeiger(Alt.datum,Typsdat.datum,Alt.zeit,Typsdat.Zeit);
        IF (MerkZeiger > AltEndZeiger) and DatumPlausibel(typsdat) THEN
        BEGIN { Darf alte Werte nicht ¸berschreiben }
          IF (MerkZeiger*LgzSRecSize) <=
             (TypsDatei.Size-LgzSRecSize) THEN
          BEGIN { Werte ¸berschreiben }
            Typsdatei.Seek(MerkZeiger*LgzSRecSize, soFromBeginning);
            TypsDatei.ReadBuffer(MerkDat, LgzSRecSize);

            { doppelte S‰tze im Bit 6 kennzeichnen }

            if Merkdat.Satzstatus < $80 then { kein hinzugef¸gter Satz dann }
            begin
              MerkDat.Satzstatus := MerkDat.Satzstatus or $40;  { Bit 6 setzen }
{$IFDEF FEHL_KONV}
              WriteStd_FehlerSatz (AufberRec, FehlerDatei, MerkDat);
{$ENDIF}
              TypsDat.Satzstatus:=TypsDat.Satzstatus or $40;    { Bit 6 setzen }
            end;
            TypsDatei.Seek(Merkzeiger*LgzSRecSize,
                           soFromBeginning); { alten mit neuem Satz Åberschreiben }
            TypsDatei.WriteBuffer(TypsDat,LgzSRecSize);

            if Typsdat.Satzstatus < $80 then begin                    { kein fehlender Satz }
              if not MWDataInfo.NeueDaten then begin         { f¸r Datenzeitbereich: IstVon }
                with Typsdat do
                  MWDataInfo.IstVon:=EncodeDate (Datum.Year, Datum.Month, Datum.Day) +
                                                 EncodeTime (Zeit.Hour, Zeit.Min, Zeit.Sec, 0);
                MWDataInfo.NeueDaten:=true;
              end;
              LetztSatz:=Typsdat;        { merken f¸r "Ist-Bis" in MWDataInfo }
            end else
              MWDataInfo.DatenFehlen:=true;
          END
          ELSE
          BEGIN   { Hinten anhÑngen }
            Typsdatei.Seek(Typsdatei.Size, soFromBeginning);  { ans Ende gehen }
            Next_Satz(Letzt.Datum,Letzt.Zeit,merkdat); { nÑchsten Satz erzeugen }
            While (CmpDate(Typsdat.Datum,Merkdat.Datum)+  { bis gelesenen Satz erreicht }
                   CmpTime(Typsdat.Zeit,Merkdat.Zeit)) > 0 do begin
              typsdatei.WriteBuffer(merkdat, LgzSRecSize);
              if merkdat.Satzstatus < $80 then begin                  { kein fehlender Satz }
                if not MWDataInfo.NeueDaten then begin       { f¸r Datenzeitbereich: IstVon }
                  with merkdat do
                    MWDataInfo.IstVon:=EncodeDate (Datum.Year, Datum.Month, Datum.Day) +
                                        EncodeTime (Zeit.Hour, Zeit.Min, Zeit.Sec, 0);
                  MWDataInfo.NeueDaten:=true;
                end;
                LetztSatz:=merkdat;      { merken f¸r "Ist-Bis" in MWDataInfo }
              end else
                MWDataInfo.DatenFehlen:=true;

              Next_Satz(merkdat.datum,merkdat.zeit,merkdat);
            end;

            TypsDatei.Seek(TypsDatei.Size, soFromBeginning); { gelesenen Satz schreiben }
            TypsDatei.WriteBuffer(Typsdat, LgzSRecSize);

            if Typsdat.Satzstatus < $80 then begin                    { kein fehlender Satz }
              if not MWDataInfo.NeueDaten then begin         { f¸r Datenzeitbereich: IstVon }
                with Typsdat do
                  MWDataInfo.IstVon:=EncodeDate (Datum.Year, Datum.Month, Datum.Day) +
                                                 EncodeTime (Zeit.Hour, Zeit.Min, Zeit.Sec, 0);
                MWDataInfo.NeueDaten:=true;
              end;
              LetztSatz:=Typsdat;        { merken f¸r "Ist-Bis" in MWDataInfo }
            end else
              MWDataInfo.DatenFehlen:=true;

            Letzt.Datum:=TypsDat.Datum;
            Letzt.Zeit:=TypsDat.Zeit;
          END; { hinten anhÑngen }
        END {Merkzeiger > altendzeiger UND DatumPlausibel }
        ELSE
        begin  {Entscheidung, ob Satz in Fehlerdatei:}
          {falls Datum nicht plausibel: ---> Fehlerdatei}
{$IFDEF FEHL_KONV}
          if not DatumPlausibel(typsdat) then
            writeStd_FehlerSatz (AufberRec, FehlerDatei, TypsDat);
{$ENDIF}
          {falls Datum < 1. LGZ-Datum: ---> Fehlerdatei}
{$IFDEF FEHL_KONV}
          if (CmpDate(typsdat.Datum, Alt.Datum)+
              CmpTime(typsdat.Zeit, Alt.Zeit) < 0) then
            writeStd_FehlerSatz (AufberRec, FehlerDatei, TypsDat);
{$ENDIF}
          {falls Datum der SÑtze nicht monoton steigend: ---> Fehlerdatei}
{$IFDEF FEHL_KONV}
          if (CmpDate(typsdat.Datum,ErsterSatz.Datum)+
              CmpTime(typsdat.Zeit, ErsterSatz.Zeit) < 0) then
            writeStd_FehlerSatz (AufberRec, FehlerDatei, TypsDat);
{$ENDIF}
          { falls keiner der FÑlle zutrifft: Satz Åbergehen }
        end;
      END  { Alt.Datum.Year<>0 }
    END   { Umsetzung }
    else
      Result := 1;   {2}
  END;   { While }

  if (filesize(ZwDatei)<> 0) and (AufberRec.select = 1) then
  begin
    if typsdat.zeit.hour <= 6{mrgstd.tagesende} then
      P_Vortag(typsdat.datum);
      { vor Tagesende --> Vortag damit sicher alle Daten abgerufen werden }
      { mrgstd.labfrdatum:= typsdat.datum;  }

  end;

  CloseFile(ZwDatei);
{$IFDEF FEHL_KONV}
  Fehlerdatei.Free;
{$ENDIF}
  TypsDatei.Free;

  // 06.02.2001
  if AufberRec.select = 1 then         { nur bei Automatik-Abruf }
    NewASCIIExportFile(C_GerArtMrg, StaData.MrgId, aetStundenwerte, QuellFileName,
      C_MaxKanalZahl, AsciiExportStart + EncodeTime(0, 0, 1, 0), 0, False);  // 23.05.2003

  if AufberRec.loeschen then
    DeleteFile (QuellFileName);

  { IstBis-Zeitpunkt f¸r MWDataInfo: }
  if MWDataInfo.NeueDaten then begin             { mind. 1 korrekter Wert wurde geschrieben }
    with LetztSatz do
      MWDataInfo.IstBis:=EncodeDate (Datum.Year, Datum.Month, Datum.Day) +
                                     EncodeTime (Zeit.Hour, Zeit.Min, Zeit.Sec, 0);
  end;
END;  { Std_Aufbereitung }


{erzeugt einen auf "Datum" folgenden Leersatz }
{---------------------------------------------------------------------------------}
procedure NextTagSatz (Datum: DateRec; TagesEnde: integer; var TagesSatz: LGZTRec);
{---------------------------------------------------------------------------------}
var
  i: byte;
begin
  TagesSatz.SatzStatus:=$80;
  p_Nachtag(Datum);
  Tagessatz.Datum:=Datum;
  TagesSatz.stunden:=TagesEnde;  // ge‰ndert 13.10.2004, WW: vorher fest 0 }
  for i:=1 to C_maxKanalZahl do
  begin
    TagesSatz.E_zaehler[i].zaehlerstatus:=$80;    { fehlend ! }
    TagesSatz.E_zaehler[i].wert:=0;               { was sonst ? }
    TagesSatz.K_zaehler[i].zaehlerstatus:=$82;    { fehlend ! }
    TagesSatz.K_zaehler[i].wert:=0;               { was sonst ? }
  end;
end;

{ Pr¸fung auf realistische Werte, Bereich von PCDatum-3a bis PCDatum+2h }
{ Pr¸fung auf korrekten Satzstatus und Stunden }
{-----------------------------------------------------------------------}
function Plausibel (var tagessatz: LGZTrec; TagesEnde: Integer): boolean;
{-----------------------------------------------------------------------}
var
  vgldatum : daterec;
  vglzeit  : timerec;
  PCDatum: DateRec;
  PCZeit: TimeRec;
begin
  Plausibel:=false;
  DecodeDate(Date, word(PCDatum.Year), word(PCDatum.Month), word(PCDatum.Day));
  DecodeTime(Time, word(PCZeit.Hour), word(PCZeit.Min), word(PCZeit.Sec), word(PCZeit.HSec));
  PCZeit.HSec:=PCZeit.HSec DIV 10;  { hsec in TimeRec }

  with tagessatz,datum do
  begin
    if (day >= 1) and (day <= F_LetztTag(datum)) and
       (month >= 1) and (month <= 12) and
       (year-1900 >= 80) and (year-1900 <=200) then
    begin
      { Toleranzbereich ? }
      vgldatum:=PCDatum;
      vglZeit:=PCZeit;
      P_Addminuten(vgldatum,vglzeit,120);   { 2 h Zuhunft }
      if CmpDate(vgldatum,datum) < 0 then
        exit;
      vgldatum:=PCDatum;
      vglZeit:=PCZeit;
      P_Addminuten(vgldatum,vglzeit,-1576800);   { 3 a Vergangenheit }
      if CmpDate(vgldatum,datum) > 0 then
        exit;
      { vorher: Pr¸fungen in Funktion "korrekt" }
      if satzstatus <> 0 then
        exit;
      if stunden <> Tagesende then
        exit;
    end
    else
      exit;   { Datum/Zeit realistisch ? }
  end;     { with record.. }
  Plausibel:=true;
end;

{ liefert true, falls beide Tagess‰tze in allen Komponenten gleich sind }
{-----------------------------------------------------------------------}
function CompareTagSatz(Satz1, Satz2: LGZTRec; Kanalzahl: word): boolean;
{-----------------------------------------------------------------------}
var
  i:       byte;    { Index f¸r Kanalzahl }
  CmpSatz: boolean;
begin
  CmpSatz:=false;
  if (CmpDate(Satz1.Datum, Satz2.Datum) = 0) and
     (Satz1.Stunden=Satz2.Stunden) then
  begin
    i:=1;
    CmpSatz:=true;
    while (i <= KanalZahl) and CmpSatz do
      if  (Satz1.E_Zaehler[i].Zaehlerstatus = Satz2.E_Zaehler[i].Zaehlerstatus) and
          (Satz1.E_Zaehler[i].Wert = Satz2.E_Zaehler[i].Wert) and
          (Satz1.K_Zaehler[i].Zaehlerstatus = Satz2.K_Zaehler[i].Zaehlerstatus) and
          (Satz1.K_Zaehler[i].Wert = Satz2.K_Zaehler[i].Wert) then
        inc(i)
      else
        CmpSatz:=false;
  end;
  CompareTagSatz:=CmpSatz;
end;

{---------------------------------------------------------------------------}
procedure KontrollZaehlerUmschichten (var TagSatz: LGZTRec; Kanalzahl: word);
{---------------------------------------------------------------------------}
{ Kontrollz‰hler an Eingangsz‰hler anh‰ngen f¸r Schreiben in Langzeitdatei (¸ber
  WIESER.INI einstellbare Kanalzahl !)
  z.B V_MaxKanalzahl = 12:  E-Z‰hler 1..12 -> TagSatz.E_Zaehler [1] .. TagSatz.E_Zaehler [12]
                            K-Z‰hler 1..12 -> TagSatz.E_Zaehler [13].. TagSatz.E_Zaehler [24]
  nach E_Zaehler [24] wird der Record dann "abgeschnitten" }
var
  in_E_Zaehler: boolean;
  k, i: integer;
begin
  if KanalZahl < C_MaxKanalZahl then begin
    k:=KanalZahl;
    in_E_Zaehler:=true;
    for i:=1 to KanalZahl do begin
      inc(k);
      if k > C_MaxKanalzahl then begin
        in_E_Zaehler:=false;
        k:=1;
      end;
      if in_E_Zaehler then
        TagSatz.E_Zaehler[k]:=TagSatz.K_Zaehler[i]
      else
        TagSatz.K_Zaehler[k]:=TagSatz.K_Zaehler[i];
    end;
  end;
end;

{------------------------------------------------------------------------------------}
procedure WriteTag_Fehlersatz (AufberRec: TAufberRec; var FehlerDatei: TFileStreamExt;
                               Satz: LGZTRec);
{------------------------------------------------------------------------------------}
var
  eh_scho_da: boolean;
  Testsatz: LGZTRec;
  FehlDatPos: longint;
BEGIN
  KontrollzaehlerUmschichten (Satz, AufberRec.Kanalzahl);
  case AufberRec.Select of
    0: Fehlerdatei.WriteBuffer(Satz, LgzTRecSize); { Fehlerdatei ¸berschreiben, }
    1: begin                                                  { sonst fortschreiben }
         if Fehlerdatei.Size = 0 then
           Fehlerdatei.WriteBuffer (Satz, LgzTRecSize)
         else
         begin                               { Datei von hinten durchsuchen }
           Fehlerdatei.Seek (-LgzTRecSize, soFromEnd);
           FehlDatPos := Fehlerdatei.Position;
           eh_scho_da := false;
           while (FehlDatPos >= 0) and not eh_scho_da do
           begin
             Fehlerdatei.Seek (FehlDatPos, soFromBeginning);
             Fehlerdatei.ReadBuffer (Testsatz, LgzTRecSize);
             if CompareTagSatz (Testsatz, Satz, AufberRec.Kanalzahl) then
               eh_scho_da := true;
             FehlDatPos := Fehlerdatei.Position - 2*LgzTRecSize;
           end;
           if not eh_scho_da then        { neuen Satz ans Ende schreiben }
           begin
             Fehlerdatei.Seek(0, soFromEnd);
             Fehlerdatei.WriteBuffer(Satz, LgzTRecSize);
           end;
         end;
       end;
  end;
END;

{------------------------------------------------------------------------}
function Tag_Aufbereitung (AufberRec: TAufberRec; const DateiNummer: word;
                           var StaData: TStaData;
                           var TADataInfo: TDataKonvInfo): Integer;
{------------------------------------------------------------------------}
{ Jeden Tag mu· mindestens ein Tagessatz vorhanden sein, falls nicht,
  wird ein Leersatz geschrieben }
const
  maxPlausTagDiff=30;
VAR
  PCDatum: DateRec;
  PCZeit: TimeRec;
  TypsDatei       : TFileStreamExt;           { LGZ-Datendatei }
  ZwDatei         : File of RohTRec;          { Zwischendatei RohTRec-Struktur }
  ZwSatz          : RohTRec;
  NeuDat          : LGZTRec;
  MerkDat         : LGZTRec;
  LGZDat          : LGZTRec;
  alt, letzt      : DateRec;  {1. u. letztes Datum der LGZ-Datei}
  AltEndZeiger    : longint;
  first           : boolean;
  ErsterSatz      : LGZTRec;
  MerkZeiger      : longint;
  QuellFileName   : TFileName;
{$IFDEF FEHL_KONV}
  Fehlerdatei     : TFileStreamExt;
{$ENDIF}
  Ok: boolean;
  AsciiExportStart: TDateTime;  // 06.02.2001
  LetztSatz       : LGZTRec;
  ZeitBuf         : TimeRec;

BEGIN
  Result:=0;
  with TADataInfo do begin
    NeueDaten:=false;
    DatenFehlen:=false;
    SollVon:=0; SollBis:=0; IstVon:=0; IstBis:=0;
  end;

  DecodeDate(Date, word(PCDatum.Year), word(PCDatum.Month), word(PCDatum.Day));
  DecodeTime(Time, word(PCZeit.Hour), word(PCZeit.Min), word(PCZeit.Sec), word(PCZeit.HSec));
  PCZeit.HSec:=PCZeit.HSec DIV 10;  { hsec in TimeRec }

  { Quelle: Zwischendatei aus Konvertierung }
  QuellFileName:=AufberRec.TagQuelldateiName;
  if not (F_Check (QuellFileName) > 0) then begin
    if AufberRec.loeschen then
      DeleteFile (QuellFileName);
    Result := 2;  {5}
    exit;
  end;

  AssignFile (ZwDatei, QuellFileName);
  reset(ZwDatei);

  if AufberRec.select = 1 then
  begin                                { Ziel: \LANGZEIT\LGZTxxxx.DAT }
    if FileExists (GetLGZDateiName(Dateinummer, dd_tags)) then
      TypsDatei:=TFileStreamExt.Create (GetLGZDateiName(Dateinummer, dd_tags),
                                        fmOpenReadWrite OR fmShareDenyWrite, Ok)
    else
      TypsDatei:=TFileStreamExt.Create (GetLGZDateiName(Dateinummer, dd_tags),
                                        fmCreate OR fmShareDenyWrite, Ok);
{$IFDEF FEHL_KONV}
    if FileExists (GetLGZFehlerDateiName(Dateinummer, dd_tags)) then
      FehlerDatei:=TFileStreamExt.Create (GetLGZFehlerDateiName(Dateinummer, dd_tags),
                                          fmOpenReadWrite OR fmShareDenyWrite, Ok)
    else
      FehlerDatei:=TFileStreamExt.Create (GetLGZFehlerDateiName(Dateinummer, dd_tags),
                                          fmCreate OR fmShareDenyWrite, Ok);
{$ENDIF}

  end
  else
  begin                                { Ziel: \MANU\*.DAT }
    TypsDatei:=TFileStreamExt.Create (GetManuDateiName(StaData.MrgId, dd_tags),
                                      fmCreate OR fmShareDenyWrite, Ok);

{$IFDEF FEHL_KONV}
    FehlerDatei:=TFileStreamExt.Create (GetManuFehlerDateiName(StaData.MrgId, dd_tags),
                                        fmCreate OR fmShareDenyWrite, Ok);
{$ENDIF}

  end;

  {zum Erzeugen der LGZT-Datei Ñhnlicher Algorithmus wie in STD-Teil}

  if Typsdatei.Size = 0 then
  begin                              {Datumsangaben vorbelegen}
    Alt.year:=0;
    Alt.month:=0;
    Alt.day:=0;
    Letzt:=Alt;
    AltEndZeiger:=-1;
  end
  else                               {1. u. letztes Datum lesen}
  begin
    Typsdatei.Seek(0, soFromBeginning);
    Typsdatei.ReadBuffer(neudat, LgzTRecSize);
    Alt:=neudat.datum;
    Typsdatei.Seek(-LgzTRecSize, soFromEnd);
    Typsdatei.ReadBuffer(neudat, LgzTRecSize);
    Letzt:=neudat.datum;
    AltEndZeiger:=TagDiff(Alt, Letzt);
  end;
  Typsdatei.Seek(0, soFromBeginning);

  // 06.02.2001
  if (Letzt.Month = 0)
  then AsciiExportStart := 0
  else AsciiExportStart := EncodeDate(Letzt.Year, Letzt.Month, Letzt.Day);

  first:=true;
  WHILE Not Eof(ZwDatei) DO
  BEGIN
    Application.ProcessMessages;

    {$I-}
    read(ZwDatei, ZwSatz);
    {$I+}
    IF ioresult=0 THEN
    BEGIN
      { Feldwerte-Zuweisung: RohTRec-Satz aus Zwischendatei -> LGZTRec-Satz f¸r Zieldatei }
      neudat.SatzStatus:=ZwSatz.SatzStatus;
      DateTimeToRec (ZwSatz.DatumZeit, neudat.Datum, ZeitBuf);
      neudat.Stunden:=ZeitBuf.Hour;
      neudat.E_Zaehler:=ZwSatz.E_Zaehler;
      neudat.K_Zaehler:=ZwSatz.K_Zaehler;

      if first and Plausibel(neudat, StaData.TagesEnde) then
      begin
        ErsterSatz:=neudat;  { 1. Satz merken }
        first:=false;
      end

{$IFDEF FEHL_KONV}
      else
        if first then
          WriteTag_FehlerSatz (AufberRec, FehlerDatei, neuDat)
{$ENDIF}
      ;
      IF (Alt.Year=0) and Plausibel (neudat, StaData.TagesEnde) THEN
      BEGIN
        Alt:=neudat.Datum;
        Letzt:=Alt;
        if Plausibel (neudat, StaData.TagesEnde) then
        begin
          lgzdat:=neudat;
          KontrollzaehlerUmschichten (lgzdat, AufberRec.Kanalzahl);
          TypsDatei.WriteBuffer(lgzdat, LgzTRecSize);

          if lgzdat.Satzstatus < $80 then begin                        { kein fehlender Satz }
            if not TADataInfo.NeueDaten then begin             { f¸r Datenzeitbereich: IstVon }
              with lgzdat do
                TADataInfo.IstVon:=EncodeDate (Datum.Year, Datum.Month, Datum.Day);
              TADataInfo.NeueDaten:=true;
            end;
            LetztSatz:=lgzdat;            { merken f¸r "Ist-Bis" in MWDataInfo }
          end else
            TADataInfo.DatenFehlen:=true;
        end                                
        else
        begin
{$IFDEF Fehl_Konv}
          WriteTag_FehlerSatz (AufberRec, FehlerDatei, neuDat);
{$ENDIF}
        end
      END
      ELSE
      BEGIN
        MerkZeiger:=TagDiff(Alt, neudat.datum);
        IF (MerkZeiger > AltEndZeiger) and Plausibel(neudat, StaData.TagesEnde) THEN
        BEGIN { Darf alte Werte nicht Åberschreiben }
          IF (MerkZeiger*LgzTRecSize) <=
             (TypsDatei.Size-LgzTRecSize) THEN
          BEGIN { Werte Åberschreiben }
            Typsdatei.Seek(MerkZeiger*LgzTRecSize, soFromBeginning);
            TypsDatei.ReadBuffer(MerkDat, LgzTRecSize);
            { doppelte SÑtze im Bit 6 kennzeichnen }
            if Merkdat.Satzstatus < $80 then { kein hinzugefÅgter Satz dann }
            begin
              MerkDat.Satzstatus:=MerkDat.Satzstatus or $40;  { Bit 6 setzen }
{$IFDEF Fehl_Konv}
              WriteTag_FehlerSatz (AufberRec, FehlerDatei, MerkDat);
{$ENDIF}
              neudat.Satzstatus:=neudat.Satzstatus or $40;    { Bit 6 setzen }
            end;
            Typsdatei.Seek(MerkZeiger*LgzTRecSize, soFromBeginning); { alten mit neuem Satz Åberschreiben }

            lgzdat:=neudat;
            KontrollzaehlerUmschichten (lgzdat, AufberRec.Kanalzahl);
            TypsDatei.WriteBuffer(lgzdat, LgzTRecSize);

            if lgzdat.Satzstatus < $80 then begin                        { kein fehlender Satz }
              if not TADataInfo.NeueDaten then begin             { f¸r Datenzeitbereich: IstVon }
                with lgzdat do
                  TADataInfo.IstVon:=EncodeDate (Datum.Year, Datum.Month, Datum.Day);
                TADataInfo.NeueDaten:=true;
              end;
              LetztSatz:=lgzdat;            { merken f¸r "Ist-Bis" in MWDataInfo }
            end else
              TADataInfo.DatenFehlen:=true;
          END
          ELSE
          BEGIN   { Hinten anhÑngen }
            Typsdatei.Seek(0, soFromEnd);  { ans Ende gehen }
            NextTagSatz(Letzt, StaData.TagesEnde, merkdat);   { nÑchsten Satz erzeugen }
            While (CmpDate(neudat.Datum, Merkdat.Datum)) > 0 do
            begin
              lgzdat:=merkdat;
              KontrollzaehlerUmschichten (lgzdat, AufberRec.Kanalzahl);
              typsdatei.WriteBuffer(lgzdat, LgzTRecSize);

              if lgzdat.Satzstatus < $80 then begin                        { kein fehlender Satz }
                if not TADataInfo.NeueDaten then begin             { f¸r Datenzeitbereich: IstVon }
                  with lgzdat do
                    TADataInfo.IstVon:=EncodeDate (Datum.Year, Datum.Month, Datum.Day);
                  TADataInfo.NeueDaten:=true;
                end;
                LetztSatz:=lgzdat;            { merken f¸r "Ist-Bis" in MWDataInfo }
              end else
                TADataInfo.DatenFehlen:=true;
              NextTagSatz(merkdat.datum, StaData.TagesEnde, merkdat);
            end;
            Typsdatei.Seek(0, soFromEnd);  { gelesenen Satz schreiben }
            lgzdat:=neudat;
            KontrollzaehlerUmschichten (lgzdat, AufberRec.Kanalzahl);
            TypsDatei.WriteBuffer(lgzdat, LgzTRecSize);

            if lgzdat.Satzstatus < $80 then begin                        { kein fehlender Satz }
              if not TADataInfo.NeueDaten then begin             { f¸r Datenzeitbereich: IstVon }
                with lgzdat do
                  TADataInfo.IstVon:=EncodeDate (Datum.Year, Datum.Month, Datum.Day);
                TADataInfo.NeueDaten:=true;
              end;
              LetztSatz:=lgzdat;            { merken f¸r "Ist-Bis" in MWDataInfo }
            end else
              TADataInfo.DatenFehlen:=true;
            Letzt:=neudat.Datum;
          END; { hinten anhÑngen }
        END {Merkzeiger > altendzeiger UND plausibel }
        ELSE
        begin  {Entscheidung, ob Satz in Fehlerdatei:}
          {falls Datum nicht plausibel: ---> Fehlerdatei}
{$IFDEF FEHL_KONV}
          if not Plausibel(neudat, StaData.TagesEnde) then
            WriteTag_FehlerSatz (AufberRec, FehlerDatei, neuDat);
{$ENDIF}
          {falls Datum < 1. LGZ-Datum: ---> Fehlerdatei}
{$IFDEF FEHL_KONV}
          if (CmpDate(neudat.Datum, Alt)) < 0 then
            WriteTag_FehlerSatz (AufberRec, FehlerDatei, neuDat);
{$ENDIF}
          {falls Datum der SÑtze nicht monoton steigend: ---> Fehlerdatei}
{$IFDEF FEHL_KONV}
          if (CmpDate(neudat.Datum, ErsterSatz.Datum)) < 0 then
            WriteTag_FehlerSatz (AufberRec, FehlerDatei, neuDat);
{$ENDIF}
          { falls keiner der FÑlle zutrifft: Satz Åbergehen }
        end;
      END  { Alt.Datum.Year<>0 UND plausibel }
    END   { Umsetzung }
    else
      Result := 1;   {2}
  END;   { While }

  CloseFile(ZwDatei);
  TypsDatei.Free;
{$IFDEF FEHL_KONV}
  Fehlerdatei.Free;
{$ENDIF}

  // 06.02.2001
  if AufberRec.select = 1 then         { nur bei Automatik-Abruf }
    NewASCIIExportFile(C_GerArtMrg, StaData.MrgId, aetTageswerte, QuellFileName,
      C_MaxKanalZahl, AsciiExportStart + 1);

  if AufberRec.loeschen then
    DeleteFile (QuellFileName);

  { IstBis-Zeitpunkt f¸r TADataInfo: }
  if TADataInfo.NeueDaten then begin             { mind. 1 korrekter Wert wurde geschrieben }
    with LetztSatz do
      TADataInfo.IstBis:=EncodeDate (Datum.Year, Datum.Month, Datum.Day);
  end;
END;  { Tag_Aufbereitung }

{------------------------------------------------------------------------}
function Prf_Aufbereitung (AufberRec: TAufberRec; const DateiNummer: word;
                           var StaData: TStaData): Integer;
{------------------------------------------------------------------------}
var
  D_ZwFile       : file of PruefRec;   { Zwischenformat }
  D_LGZQFile     : file of PruefRec;   { Langzeitdatei }
  R_LetztPrfsatz : PruefRec;
  R_PrfSatz      : PruefRec;
  ZwPosition     : longint;
  datum_neuer    : boolean;
  ResetErg       : longint;
  QuellFileName  : TFileName;

begin
  Result:=0;
  { Quelle: Zwischendatei aus Konvertierung }
  QuellFileName:=AufberRec.PrfQuelldateiName;
  AssignFile (D_ZwFile, QuellFileName);
  if NetReset(D_ZwFile, SizeOf(PruefRec), ForAll+DenyWrite) < 0 then begin
    if AufberRec.loeschen then
      DeleteFile (QuellFileName);
    Result := 2;
    exit;
  end;

  if AufberRec.select = 1 then begin        { Ziel: \LANGZEIT\LGZQxxxx.DAT }
    AssignFile (D_LGZQFile, GetLGZDateiName(Dateinummer, dd_Pruef));
    ResetErg:=NetReset(D_LGZQFile, SizeOf(PruefRec), ForAll+DenyWrite);
    if ResetErg<=0 then
    begin
      If ResetErg=0 then
        CloseFile(D_LGZQFile);                          
      rewrite(D_LGZQFile);
      FillChar(R_LetztPrfSatz.von_datum,SizeOf(R_LetztPrfSatz.von_datum),0);
      FillChar(R_LetztPrfSatz.von_zeit,SizeOf(R_LetztPrfSatz.von_zeit),0);
    end
    else
    begin
      seek(D_LGZQFile, FileSize(D_LGZQFile)-1);
      read(D_LGZQFile, R_LetztPrfSatz);         { letzten Eintrag lesen }
    end;
  end
  else begin                                              { Ziel: \MANU\*.DAT }
    AssignFile (D_LGZQFile, GetManuDateiName(StaData.MrgId, dd_Pruef));
    rewrite(D_LGZQFile);
    FillChar(R_LetztPrfSatz.von_datum,SizeOf(R_LetztPrfSatz.von_datum),0);
    FillChar(R_LetztPrfSatz.von_zeit,SizeOf(R_LetztPrfSatz.von_zeit),0);
  end;

  datum_neuer:=false;
  ZwPosition:=-1;
  while not eof(D_ZwFile) and not datum_neuer do
  begin
    Application.ProcessMessages;
    read(D_ZwFile, R_PrfSatz);
    if gleich(R_PrfSatz,R_LetztPrfSatz, SizeOf(PruefRec)) then
    begin
      Datum_neuer:=true;
      ZwPosition:=FilePos(D_ZwFile);
    end;
  end;
  if Datum_neuer then                    { Langzeitdaten anhÑngen }
    seek(D_ZwFile, ZwPosition)
  else
    seek(D_ZwFile, 0);
  seek(D_LGZQFile, FileSize(D_LGZQFile));
  while not eof(D_ZwFile) do
  begin
    Application.ProcessMessages;
    read(D_ZwFile, R_PrfSatz);
    write(D_LGZQFile, R_PrfSatz);
  end;
  CloseFile(D_ZwFile);
  CloseFile(D_LGZQFile);

  if AufberRec.loeschen then
    DeleteFile (QuellFileName);

end;  { Prf_Aufbereitung }


{$IFDEF GVT}
{-----------------------------------------------------------------------------}
function Meld_Aufbereitung_GVT (AufberRec: TAufberRec; const DateiNummer: word;
                                var StaData: TStaData): Integer;
{-----------------------------------------------------------------------------}
{ Meldungen aufbereiten }

var LGZMDatei,
    MeldDatei  :  file of meldrec;
    LGZPDatei,
    ParaDatei  :  file of parmrec;
    Msatz1,
    MSatz2     :  meldrec;
    psatz      :  parmrec;

    weitersuchen,
    parameter  : boolean;
    FSZ        : longint;
    diff       : shortint;

    pcdatum    : daterec;
    pczeit     : timerec;
    abstand    : longint;

    MrgKonvData: TMrgKonvData;
    p_start: word;
    WBuf: word;
    Code: integer;
    MeldQuellFileName: TFileName;
    ParaQuellFileName: TFileName;
    ResetErg: LongInt;

begin
  Result:=0;
  DecodeDate(Date, word(PCDatum.Year), word(PCDatum.Month), word(PCDatum.Day));
  DecodeTime(Time, word(PCZeit.Hour), word(PCZeit.Min), word(PCZeit.Sec), word(PCZeit.HSec));
  PCZeit.HSec:=PCZeit.HSec DIV 10;  { hsec in TimeRec }

  AssignFile (LGZMDatei, GetLGZDateiName (Dateinummer, dd_Meld));
  AssignFile (LGZPDatei, GetLGZDateiName (Dateinummer, dd_Para));

  MeldQuellFileName:=GetZwischenDateiName_MrgId (StaData.MrgId, dd_Meld);
  ParaQuellFileName:=GetZwischenDateiName_MrgId (StaData.MrgId, dd_Para);
  AssignFile (MeldDatei, MeldQuellFileName);
  AssignFile (ParaDatei, ParaQuellFileName);

  if not GetMrgKonvData (StaData.MrgTyp, MrgKonvData) then begin
    Result:=1;
    exit;
  end;
  p_start:=no_parms;
  Val (MrgKonvData.MNrParaStart, WBuf, Code);
  if Code = 0 then p_start:=WBuf;

  parameter:=p_start <> no_parms;
  lgz_pos:=maxlongint;

  { keine Meldungen sind wirklich mˆglich: }
  if F_Check (MeldQuellFileName) <= 0 then exit;

  if (F_Check (ParaQuellFileName) < 0) AND parameter then begin
    Result:=2;  {4}               { Ausstieg nur, wenn Parameter vor- }
    exit;                         { handen sein m¸ﬂten }
  end;

  FSZ := NetReset(LGZMDatei, SizeOf(MeldRec), ForAll + DenyWrite);
  if FSZ = ERR_FILE_NOT_FOUND then
    rewrite(LGZMDatei);

  ResetErg := NetReset(LGZPDatei, SizeOf(ParmRec), ForAll + DenyWrite);
  if ResetErg = ERR_FILE_NOT_FOUND then
    rewrite(LGZPDatei);

  reset(MeldDatei);

  if parameter then
  begin
    reset(paraDatei);
    seek(LGZPDatei,filesize(LGZPDatei));    { immer anhÑngen }
  end;

  weitersuchen:=true;

  if FSZ > 0 then
  begin     { LGZ Datei mit Daten vorhanden }
    seek(LGZMDatei,filesize(LGZMDatei)-1);
    read(LGZMDatei,msatz1);
    while (not eof(MeldDatei)) and weitersuchen do
    begin
      read(Melddatei,msatz2);
      if gleich(msatz1,msatz2,sizeof(meldrec)-1) then
        weitersuchen:=false;
      { LÑnge -1, damit status_neu nicht verglichen wird (boolean) }
    end;
  end;

  if weitersuchen then
    reset(MeldDatei);    { komplett anhÑngen }
  lgz_pos:=filepos(MeldDatei);              { merken fÅr Gesamtmeldedatei }
  while not eof(MeldDatei) do
  begin         { teilw. anhÑngen }
    Application.ProcessMessages;
    read(Melddatei,msatz1);
    abstand:=zeiger(msatz1.k_datum,pcdatum,msatz1.k_zeit,pczeit);
    if abstand >= -2 then
    begin             { bis 2h in die Zukunft ok! }
      msatz1.status_neu:=true;                { sicherheitshalber }
      write(LGZMDatei,msatz1);
      { hier neue Meldungen }
      if msatz1.nummer=P_Start then
      begin     { ParameterÑnderung }
        reset(ParaDatei);
        diff:=1;
        while not eof(ParaDatei) and (diff<>0) do
        begin
          read(ParaDatei,psatz);
          diff:=cmpdate(psatz.datum,msatz1.K_Datum)+
                cmptime(psatz.zeit,msatz1.K_Zeit);
        end;

        if diff=0 then
          write(LGZPDatei,psatz); { und in LGZ-Datei Åbernehmen }
      end;
    end;   { Fehlerdatei }
  end;

  if parameter then
    close(ParaDatei);
  close(MeldDatei);
  close(LGZMDatei);
  close(LGZPDatei);

  if AufberRec.loeschen then begin
    DeleteFile (MeldQuellFileName);
    DeleteFile (ParaQuellFileName);
  end;
end;
{$ENDIF}

{------------------------------------------------------------------------------------}
procedure Aufbereitung (var fehler: byte; AufberRec: TAufberRec; ManuAppend: boolean;
                        var MWDataInfo: TDataKonvInfo; var TADataInfo: TDataKonvInfo);
{------------------------------------------------------------------------------------}
var
  Dateinummer: word;        { Nummer f¸r Langzeitdateien z.B.LGZD0002.DAT }
                            { g¸ltig f¸r Stundenwerte,Tagess‰tze,Meldungen,Parameter }
  StaData : TStaData;
  Stammdaten: TMRGStammdaten;

begin
  with MWDataInfo do begin
    NeueDaten:=false;
    DatenFehlen:=false;
    SollVon:=0; SollBis:=0; IstVon:=0; IstBis:=0;
  end;
  with TADataInfo do begin
    NeueDaten:=false;
    DatenFehlen:=false;
    SollVon:=0; SollBis:=0; IstVon:=0; IstBis:=0;
  end;

  { Grˆﬂe der LGZ-Records f¸r die ¸bergebene Kanalzahl berechnen: }
  LGZSRecSize:=F_SizeOfLGZSRec (AufberRec.Kanalzahl);
  LGZTRecSize:=F_SizeOfLGZTRec (AufberRec.Kanalzahl);

  Dateinummer := Verz_Eintrag(AufberRec.MrgId);
  Stammdaten:=TMRGStammdaten.Create (PathServer.PathName [WStammDir]);
  try
    if Stammdaten.InitTabellen then begin
      if Stammdaten.GetStaData (AufberRec.MrgId, StaData) then begin
        case fehler of        { als Eingangsparameter fÅr Modusumschaltung }
          1: begin
               fehler:=0;
               Std_Aufbereitung (AufberRec, DateiNummer, StaData,
                                 MWDataInfo, ManuAppend);   { Fehlercode: 1,2 }
               Tag_Aufbereitung (AufberRec, DateiNummer, StaData,
                                 TADataInfo);
             end;
    {$IFDEF GVT}
          2: begin
               fehler:=0;
               Meld_Aufbereitung_GVT (AufberRec, DateiNummer, StaData);
             end;
    {$ENDIF}
    (*      6: begin
               fehler:=0;
               Std_Aufbereitung (Select, DateiNummer, StaData, MWDataInfo);  { Fehlercode: 1,2 }
               Tag_Aufbereitung (Select, DateiNummer, StaData);
               Tag14_Aufbereitung (Select, DateiNummer, StaData);
             end;*)
          7: begin
               fehler:=0;
               Prf_Aufbereitung (AufberRec, DateiNummer, StaData);
             end;
        end;
      end;
    end;
  finally
    Stammdaten.Free;
  end;
end;

{-------------------------------------------------------------}
function Mess_Aufb (AufberRec: TAufberRec; ManuAppend: boolean;
                    var MWDataInfo: TDataKonvInfo;
                    var TADataInfo: TDataKonvInfo): integer;
{-------------------------------------------------------------}
{ R¸ckgabe: Records mit Informationen ¸ber aufbereitete Meﬂwerte und Tagess‰tze }
var
  fehler: byte;
begin
  Result := 0;
  fehler := 1; { Stundens‰tze und Tagess‰tze }
  Aufbereitung (fehler, AufberRec, ManuAppend, MWDataInfo, TADataInfo);
end;
(*
{-----------------------------------------------------------------------}
function Mess14_Aufb (MrgId: TMrgId; Select: integer;
                      var MWDataInfo: TMWDataInfo): integer;
{-----------------------------------------------------------------------}
var
  fehler: byte;
begin
  Result := 0;
  fehler := 6; { Stundens‰tze, Tagess‰tze und Tagess‰tze 14-stellig }
  Aufbereitung (fehler, Select, MrgId, MWDataInfo);
end; *)

{---------------------------------------------------}
function Pruef_Aufb (AufberRec: TAufberRec): integer;
{---------------------------------------------------}
var
  fehler: byte;
  Dummy1: boolean;
  Dummy2: TDataKonvInfo;
  Dummy3: TDataKonvInfo;
begin
  Result := 0;
  fehler := 7; { Pr¸fungss‰tze }
  Dummy1:=false;
  Aufbereitung (fehler, AufberRec, Dummy1, Dummy2, Dummy3);
end;

{$IFDEF GVT}
{------------------------------------------------------}
function Meld_Aufb_GVT (AufberRec: TAufberRec): integer;
{------------------------------------------------------}
var
  fehler: byte;
  Dummy1: boolean;
  Dummy2: TDataKonvInfo;
  Dummy3: TDataKonvInfo;
begin
  Result := 0;
  fehler := 2; { nur stationsbezogene Meldungen }
  Dummy1:=false;
  Aufbereitung (fehler, AufberRec, Dummy1, Dummy2, Dummy3);
end;
{$ENDIF}

end.