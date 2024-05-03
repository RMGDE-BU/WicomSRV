{*******************************************************************************}
{* Unit: Konvertieren von Meldungen aus der Rohdatei in eine Zwischendatei mit *}
{*       Langzeitstruktur (speziell für GVT-Leitwarte)                         *}
{* Version 02.03.99 WW                                                         *}
{*******************************************************************************}
unit GVTMKonv;

interface

{$IFDEF GVT}
uses
  Forms, SysUtils, MDBSta, MDBMrg, PathIni, LGZType;

type

  { Übergaberecord für Meldungskonvertierung GVT }

  TMeldKonvGVT = record
    MrgId: TMrgId;             { eindeutige Nummer der Station }
    QuelldateiName: TFileName; { vollständiger Pfadname der Rohdatei }
    ZieldateiName: TFileName;  { vollständiger Pfadname der Zwischendatei }
    ParadateiName: TFileName;  { Datei mit Parameteränderungen }
    loeschen: boolean;         { Rohdatei nach Konvertierung loeschen }
  end;


function Meld_Konv_GVT (MeldKonvRec: TMeldKonvGVT): integer;
{ Rückgabewert:
   = 0: fehlerfreie Konvertierung
   < 0: fehlerhafte Konvertierung }
{$ENDIF}

implementation

{$IFDEF GVT}
const
  l_meldung = 17;
  { 15 Zeichen Meldung + 2 Zeichen Antwort bei der 1. Meldung }


function m_wandlung_3113(MRG_ID: integer; s: string; var m: meldrec): integer;
{ wandelt 3113-Rohsatz-String in LGZ-Record-Struktur;
  Rückgabewert:
   0: fehlerfreie Konvertierung,
  -1: Formatfehler, Werte außerhalb des zulässigen Bereichs }
var
  error: integer;

begin
  FillChar(m, SizeOf(MeldRec), #0);
  m_wandlung_3113 := 0;
  with m do
  begin
    kennung := '' {GetMRGKennung(MRG_ID)};     { wird für die LW-Weitergabe nicht benötigt }
    status_neu := true;
    Val(copy(s, 1, 3), nummer, error); { Meldungsnummer }
    if error <> 0 then
      m_wandlung_3113 := -1;
    with k_datum do
    begin                { "Kommt" Datum/Zeit }
    { Jahr }
      val(copy(s, 4, 2), year, error);
      if error <> 0 then
        m_wandlung_3113 := -1
      else
        if year > 80 then
          year := year + 1900
        else
          year := year + 2000;
    { Monat }
      val(copy(s, 6, 2), month, error);
      if error <> 0 then
        m_wandlung_3113 := -1
      else
        if (month < 1) or (month > 12) then
          m_wandlung_3113 := -1;
    { Tag }
      val(copy(s, 8, 2), day, error);
      if error <> 0 then
        m_wandlung_3113 := -1
      else
        if (day < 1) or (day > 31) then
          m_wandlung_3113 := -1;
    end; { with k_datum }
    g_datum := k_datum;
    with k_zeit do
    begin
    { Stunde }
      val(copy(s, 10, 2), hour, error);
      if error <> 0 then
        m_wandlung_3113 := -1
      else
        if (hour < 0) or (hour > 23) then
          m_wandlung_3113 := -1;
    { Minute }
      val(copy(s, 12, 2), min, error);
      if error <> 0 then
        m_wandlung_3113 := -1
      else
        if (min < 0) or (min > 59) then
          m_wandlung_3113 := -1;
    { Sekunde }
      val(copy(s, 14, 2), sec, error);
      if error <> 0 then
        m_wandlung_3113 := -1
      else
        if (sec < 0) or (sec > 59) then
          m_wandlung_3113 := -1;
      hsec := 0;
    end; { with k_zeit }
    g_zeit := k_zeit;
  end;  { with m }
end;


function para_aufbereitung(var s: string; m: meldrec;
                           var ParaSatz: ParmRec): integer;
{ gefilterte 38er Meldungen in }
{ Parameter-Langzeitdatei schreiben }
{ Datum + Zeit aus 24er in m }

const filler  = 'X';      { füllt letzten Rohsatz bis Stringende }
      trenner = '_';      { trennt alten und neuen Parameter }
      maxParaLaenge = 130;

var   psatz: parmrec;
      posi : integer;  { Position im Rohsatz }
      error: integer;
      ok: boolean;

begin
  ok := true;
  Para_aufbereitung := 0;
  FillChar(ParaSatz, SizeOf(ParmRec), #0);
  with psatz do begin
    p_alt := '';
    p_neu := '';
    datum := m.K_datum;
    zeit := m.K_zeit;
    val(copy(s, 1, 3), nummer, error);
    if error <> 0 then
    begin
      ok := false;
      Para_aufbereitung := -1;
    end;
    posi:=4;
    repeat    { alter Parameter bis Trennzeichen }
      p_alt:=p_alt + s[posi];
      inc(posi);
    until (s[posi] = trenner) or (posi > MaxParaLaenge);
    inc(posi);
    if posi > MaxParaLaenge then
    begin
      ok:=false;
      para_aufbereitung := -1;
    end;
    if ok then
    repeat   { neuer Parameter bis Füllzeichen oder Ende }
      p_neu:=p_neu + s[posi];
      inc(posi);
    until (posi > length(s)) or (s[posi] = filler);
  end;  { of with }
  if (m.K_datum.year <> 0) and ok then
    ParaSatz := PSatz
  else
  begin
    psatz.p_alt:='Fehlerhafter Parameter';
    psatz.p_neu:='';
    ParaSatz := PSatz;
  end;
end; { Para_aufbereitung }


function Meld_Konv_3113(MeldKonvRec: TMeldKonvGVT; MrgKonvData: TMrgKonvData): integer;

var
  p_start: word;
  p_parms: word;
  memo: meldrec;          { für Parameteränderungen merken }
  _38: boolean;           { Meldung = Parameteränderung ? }
  s38: string;            { zum Parameter-Sammeln }
  satz: string[l_meldung];{ zum Einlesen }
  msatz: meldrec;
  psatz: parmrec;
  zeichen: char;
  zeiger: longint;
  d_zw_P: File of ParmRec;
  d_zw_M: File of MeldRec;
  d_LGZP: File of Parmrec;
  d_LGZM: File of MeldRec;
  Meld_Roh: File of char; { Rohdatei }
  MeldNr: integer;
  error: integer;
  zwisch_M, zwisch_P: TFileName;
  first: boolean;
  WBuf: word;
  Code: integer;

begin
  Meld_Konv_3113 := 0;

  zwisch_M:=ExtractFilePath(MeldKonvRec.Zieldateiname)+
            'Meld'+IntToStr(MeldKonvRec.MrgId)+'.tmp';
  zwisch_P:=ExtractFilePath(MeldKonvRec.Zieldateiname)+
            'Para'+IntToStr(MeldKonvRec.MrgId)+'.tmp';

  p_start:=no_parms;
  p_parms:=no_parms;
  Val (MrgKonvData.MNrParaStart, WBuf, Code);
  if Code = 0 then p_start:=WBuf;
  Val (MrgKonvData.MNrParameter, WBuf, Code);
  if Code = 0 then p_parms:=WBuf;

  { Zwischendateien stehen im selben Verzeichnis wie Zieldateien }
  assign(d_zw_M, zwisch_M);
  assign(d_zw_P, zwisch_P);
  assign(meld_roh, Meldkonvrec.QuellDateiName);
  assign(d_LGZM, MeldKonvRec.ZielDateiname);
  assign(d_LGZP, MeldKonvRec.ParaDateiname);

  {$I-} reset(meld_roh); {$I+}
  if IOResult <> 0 then
    exit;
  Rewrite(d_zw_M);
  Rewrite(d_zw_P);

  _38  := false;
  s38  := '';

  first := true;
  while not eof(meld_roh) do begin
    satz:='';
    zeichen:=#0;
    while (not eof(meld_roh)) and (zeichen<>#$1f) and
	  (length(satz)<=l_meldung) and (zeichen<>#$03) do begin
      read(meld_roh,zeichen);
      satz:=satz+zeichen;
    end;

    Application.ProcessMessages;

    if first then
    begin { STX und M abschneiden }
      satz := copy(satz, 3, length(satz)-2);
      first := false;
    end;

    val(copy(satz, 1, 3), MeldNr, error);
    if error <> 0 then
      Meld_Konv_3113 := -1
    else
    begin
      if MeldNr <> P_Parms then
      begin
	if m_wandlung_3113(MeldKonvRec.MrgId, satz, msatz) = 0 then       { in Record wandeln }
          write(d_zw_M, msatz); { Meldungs LGZ-Datei schreiben }
        if _38 then
        begin
          para_aufbereitung(s38, memo, psatz);
          write(d_zw_P, psatz);
          _38:=false;
	  s38:='';
        end;
        if (msatz.nummer=P_Start) then
          memo:=msatz  { Datum/Zeit merken }
        else
          memo.K_datum.year:=0;
      end
      else { MeldNr = p_parms }
      begin
        _38 := true;
        s38 := s38 + copy(satz,4,12);    { ohne führende Nummer z.B.038 merken! }
      end;
    end; { error = 0 }
  end; { while not eof(Rohdatei) }
  close(meld_roh);

  { Rohdatei nicht löschen ! Sie wird für die allgemeine Konvertierung noch benötigt. }

  if _38 then begin
    para_aufbereitung(s38, memo, psatz);
    write(d_zw_p, psatz);
    s38:='';
  end;
  close(d_zw_M);
  close(d_zw_P);

  { Umspeichern der Reihenfolge der Meldungen }
  {$I-} Reset(d_zw_M); {$I+}
  if IOResult = 0 then
  begin
    Rewrite(d_LGZM);
    for zeiger:=filesize(d_zw_M)-1 downto 0 do
    begin
      Application.ProcessMessages;
      seek(d_zw_M,zeiger);
      read(d_zw_M,msatz);
      write(d_LGZM,msatz);
    end;
    close(d_LGZM);
    close(d_zw_M);

    if MeldKonvRec.loeschen then begin
      {$I-} erase(d_zw_m); {$I+}
      if IOResult <> 0 then;
    end;
  end;

  { Umspeichern der Reihenfolge der Parameteränderungen }
  {$I-} Reset(d_zw_P); {$I+}
  if IOResult = 0 then
  begin
    rewrite(d_LGZP);
    for zeiger:=filesize(d_zw_P)-1 downto 0 do
    begin
      Application.ProcessMessages;
      seek(d_zw_P,zeiger);
      read(d_zw_P,psatz);
      write(d_LGZP,psatz);
    end;
    close(d_LGZP);
    close(d_zw_P);

    if MeldKonvRec.loeschen then begin
      {$I-} erase(d_zw_P); {$I+}
      if IOResult <> 0 then;
    end;
  end;
end; { Meld_Konv_3113 }



function Meld_Konv_3100(MeldKonvRec: TMeldKonvGVT): integer;
var
  satz           : string[l_meldung];   { zum Einlesen }
  msatz          : meldrec;
  zeichen        : char;
  d_LGZM         : File of MeldRec;
  Meld_Roh       : File of char;
  first          : boolean;

begin
  Meld_Konv_3100 := 0;
  assign(meld_roh, MeldKonvRec.Quelldateiname);
  assign(d_LGZM, MeldKonvRec.Zieldateiname);

  {$I+} Reset(meld_roh); {$I+}
  if IOResult <> 0 then
  begin
    Meld_Konv_3100 := -1;
    exit;
  end;

  {$I-} rewrite(d_LGZM); {$I+}
  if IOResult <> 0 then
  begin
    close(meld_roh);
    Meld_Konv_3100 := -1;
    exit;
  end;

  first := true;
  while not eof(meld_roh) do
  begin
    satz:='';
    zeichen:=#0;
    while (not eof(meld_roh)) and (zeichen<>#$1f) and
	  (length(satz)<=l_meldung) and (zeichen<>#$03) do
    begin
      read(meld_roh, zeichen);
      satz := satz + zeichen;
    end;

    Application.ProcessMessages;

    if first then
    begin
      satz := copy(satz, 3, length(satz)-2);
      first := false;
    end;
    if length(satz) > 1 then  { wg. Schlußsatz }
    begin
      if m_wandlung_3113(MeldKonvRec.MrgId, satz, msatz) = 0 then
        write(d_LGZM,msatz)     { Meldungs LGZ-Datei schreiben }
      else
        Meld_Konv_3100 := -1;
    end;
  end;
  close(meld_roh);
  close(d_LGZM);

  { Rohdatei nicht löschen ! Sie wird für die allgemeine Konvertierung noch benötigt. }

end; { Meld_Konv_3100 }


function HexToInt(x: string): integer;
var
  i: integer;
  error: integer;

begin
  HexToInt := -1;
  if x[1] in ['0'..'9'] then
  begin
    val(x, i, error);
    if error = 0 then
      HexToInt := i;
  end;
  if x[1] in ['A'..'F'] then
    HexToInt := ord(x[1]) - ord('A') + 10;
end;


function m_wandlung_1029(MRG_ID: integer; s: string; var m: meldrec): integer;
var
  error: integer;
  nrbuf: integer;

begin
  FillChar(m, SizeOf(meldrec), #0);
  m_Wandlung_1029 := 0;
  with m do
  begin             { Wandlung Roh-Text => LGZ-Record }
    kennung := '' {GetMRGKennung(MRG_ID)};      { wird für die LW-Weitergabe nicht benötigt }
    status_neu:=true;
    nummer := 0;
    nrbuf := HexToInt(copy(s, 11, 1)); { nur 1. Ziffer wird ausgewertet }
    if nrbuf = -1 then
      m_wandlung_1029 := -1
    else
      nummer := nrbuf;
    with k_datum do
    begin                { "Kommt" Datum/Zeit }
    { Jahr }
      val(copy(s, 5, 2), year, error);
      if error <> 0 then
        m_wandlung_1029 := -1
      else
        if year > 80 then
          year := year + 1900
        else
          year := year + 2000;
    { Monat }
      val(copy(s, 3, 2), month, error);
      if error <> 0 then
        m_wandlung_1029 := -1
      else
        if (month < 1) or (month > 12) then
          m_wandlung_1029 := -1;
    { Tag }
      val(copy(s, 1, 2), day, error);
      if error <> 0 then
        m_wandlung_1029 := -1
      else
        if (day < 1) or (day > 31) then
          m_wandlung_1029 := -1;
    end; { with k_datum }

    with k_zeit do
    begin
    { Stunde }
      val(copy(s, 7, 2), hour, error);
      if error <> 0 then
        m_wandlung_1029 := -1
      else
        if (hour < 0) or (hour > 23) then
          m_wandlung_1029 := -1;
    { Minute }
      val(copy(s, 9, 2), min, error);
      if error <> 0 then
        m_wandlung_1029 := -1
      else
        if (min < 0) or (min > 59) then
          m_wandlung_1029 := -1;
      sec:=0;
      hsec:=0;
    end;
    g_datum := k_datum;
    g_zeit := k_zeit;
  end;  { with }
end;


function Meld_Konv_1029(MeldKonvRec: TMeldKonvGVT): integer;
var
  satz           :  string[l_meldung];   { zum Einlesen }
  msatz          :  meldrec;
  zeichen        :  char;
  d_LGZM         :  File of MeldRec;
  Meld_Roh       :  File of char;

begin
  Meld_Konv_1029 := 0;
  assign(meld_roh, MeldKonvRec.Quelldateiname);
  assign(d_LGZM, MeldKonvRec.Zieldateiname);

  {$I-} Reset(meld_roh); {$I+}
  if IOResult <> 0 then
  begin
    Meld_Konv_1029 := -1;
    exit;
  end;

  {$I-} rewrite(d_LGZM); {$I+}
  if IOResult <> 0 then
  begin
    close(meld_roh);
    Meld_Konv_1029 := -1;
    exit;
  end;

  while not eof(meld_roh) do
  begin
    satz:='';
    zeichen:=#0;
    while (not eof(meld_roh)) and (zeichen<>#$1f) and
	  (length(satz)<=l_meldung) and (zeichen<>#$03) do
    begin
      read(meld_roh,zeichen);
      satz:=satz+zeichen;
    end;

    Application.ProcessMessages;

    if length(satz) > 14 then
    begin      { wg Befehlswiederholung }
      if m_wandlung_1029(MeldKonvRec.MrgId, satz, msatz) = 0 then
        write(d_LGZM,msatz)
      else
        Meld_Konv_1029 := -1;
    end;
  end;
  close(meld_roh);
  close(d_LGZM);

  { Rohdatei nicht löschen ! Sie wird für die allgemeine Konvertierung noch benötigt. }

end; { Meld_Konv_1029 }

{------------------------------------------------------------------------------}

function Meld_Konv_GVT (MeldKonvRec: TMeldKonvGVT): integer;
var
  MrgTyp: TMrgTyp;
  MrgKonvData: TMrgKonvData;
  Stammdaten: TMRGStammdaten;
  
begin
  Meld_Konv_GVT := -1;
  Stammdaten:=TMRGStammdaten.Create (PathServer.PathName [WStammDir]);
  try
    if Stammdaten.InitTabellen then begin
      MrgTyp := StammDaten.GetMrgTyp (MeldKonvRec.MrgId);
      If MrgTyp >= 0 then begin
        if GetMrgKonvData (MrgTyp, MrgKonvData) then begin
          case MrgKonvData.MeldKonvGruppe of
            1: Meld_Konv_GVT := Meld_Konv_3113(MeldKonvRec, MrgKonvData);
            2: Meld_Konv_GVT := Meld_Konv_3100(MeldKonvRec);
            3: Meld_Konv_GVT := Meld_Konv_1029(MeldKonvRec);
          end;
        end;
      end;
    end;
  finally
    Stammdaten.Free;
  end;
end;
{$ENDIF}

end.
