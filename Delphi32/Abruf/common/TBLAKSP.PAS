{------------------------------------------------------------------------------}
{ 15.06.1999 GD; Übergabetabellen für LAKS-Parameter, Kurzzeitarchiv und       }
{                Lastzustände                                                  }
{------------------------------------------------------------------------------}
unit tbLaksP;

interface

uses
  Forms, ParaNum, SysUtils, Windows, WTables, Db_Attn, Db, DbTables, WStrUtils,
  LAKSUtil, Classes;

const
  { Parameternummern für LAKS }
  { ... in Unit ParaNum.Pas ! }

  { Tabelle für darzustellende Parameter }
  C_TbLPar                = 'LPar';

  C_TfLPar_ParaNr         = 'ParameterNr';    { int }
  C_TfLPar_ParaWert       = 'ParameterWert';  { str24 }

  { Tabelle für darzustellende Kurzzeitarchivwerte }
  C_TbLKArchiv            = 'LKA';

  C_TfLPar_LfdNr          = 'LfdNr';          { smallint }
  C_TfLPar_SatzStatus1    = 'SatzStatus1';    { smallint }
  C_TfLPar_SatzStatus2    = 'SatzStatus2';    { smallint }
  C_TfLPar_LastStatus     = 'LastStatus';     { int }
  C_TfLPar_Arbeit1        = 'Arbeit1';        { num }
  C_TfLPar_Leistung1      = 'Leistung1';      { num }
  C_TfLPar_Arbeit2        = 'Arbeit2';        { num }
  C_TfLPar_Leistung2      = 'Leistung2';      { num }
  C_TfLPar_Arbeit3        = 'Arbeit3';        { num }
  C_TfLPar_Leistung3      = 'Leistung3';      { num }
  C_TfLPar_Arbeit4        = 'Arbeit4';        { num }
  C_TfLPar_Leistung4      = 'Leistung4';      { num }
  C_TfLPar_Arbeit5        = 'Arbeit5';        { num }
  C_TfLPar_Leistung5      = 'Leistung5';      { num }
  C_TfLPar_Arbeit6        = 'Arbeit6';        { num }
  C_TfLPar_Leistung6      = 'Leistung6';      { num }
  C_TfLPar_Arbeit7        = 'Arbeit7';        { num }
  C_TfLPar_Leistung7      = 'Leistung7';      { num }
  C_TfLPar_Arbeit8        = 'Arbeit8';        { num }
  C_TfLPar_Leistung8      = 'Leistung8';      { num }

  { Tabelle für Lastzustände }
  C_TbLLZ                 = 'LLZ';

  C_TfLLZ_LastNr          = 'LastNr';         { int }
  C_TfLLZ_Zugeschaltet    = 'Zugeschaltet';   { log }


  C_LaksParameterCount    = 17;

type
  TLaksParameterFormat = string[24];

  PLaksLastenRec = ^TLaksLastenRec;
  TLaksLastenRec = record
    LastNr        : integer;
    Zugeschaltet  : boolean;
  end;

  TLaksParameterRec = record
    MaxZone   : TLaksParameterFormat; { Parameternr. 050107 }
    PMax      : TLaksParameterFormat; { Parameternr. 050106 }
    PZiel     : TLaksParameterFormat; { Parameternr. 050105 }
    PVerf     : TLaksParameterFormat; { Parameternr. 050104 }
    PMom      : TLaksParameterFormat; { Parameternr. 030102 }
    WIst      : TLaksParameterFormat; { Parameternr. 030104 }
    Restzeit  : TLaksParameterFormat; { Parameternr. 050101 }
    LaksZeit  : TLaksParameterFormat; { Parameternr. 010301 }
    LaksDatum : TLaksParameterFormat; { Parameternr. 020302 }
    Einheit   : TLaksParameterFormat; { Parameternr. 030101 }
    MPLaenge  : TLaksParameterFormat; { Parameternr. 010306 }
    GerTyp    : TLaksParameterFormat; { Parameternr. 010101 }
    Version   : TLaksParameterFormat; { Parameternr. 010102 }
    ZFenster1 : TLaksParameterFormat; { Parameternr. 070101 }
    ZFenster2 : TLaksParameterFormat; { Parameternr. 070201 }
    ZFenster3 : TLaksParameterFormat; { Parameternr. 070301 }
    ZFenster4 : TLaksParameterFormat; { Parameternr. 070401 }
  end;

  TLaksKArchivRec = record
    LfdNr     : integer;
    SStatus1  : SmallInt;
    SStatus2  : SmallInt;
    LStatus   : integer;
    Arbeit    : double;
    Leistung  : double;
  end;

  TTbLaksParameter = class(TObject)
    DateiPfad         : string;
    LaksId            : integer;
    TriggerTimePara   : integer;
    TriggerTimeKA     : integer;
    TriggerTimeLZ     : integer;
    LParameterTabelle : string;
    LKArchivTabelle   : string;
    LLZustandTabelle  : string;
  private
    procedure CreateParameterTable;
    procedure CreateKArchivTable;
    procedure CreateLZustandTable;
  public
    constructor Create(anId: integer; aPath: string);
    function GetNewParameterRec(var aRec: TLaksParameterRec): boolean;
    function GetNewLastzustandsListe(var sl: TStringList): boolean;
    function GetNewKArchivQuery(var q: TQueryExt; LfdNr: integer;
                                var aRes: byte): boolean;
    function KonvMomentanParameterDaten (Rohdaten: string; var Restzeit: integer): boolean;
    function KonvKArchivDaten (Rohdaten: string;
                               var LetztDatumStr: string; var LetztZeitStr: string): boolean;
    function KonvLastzustandDaten (Rohdaten: string): boolean;
    procedure EmptyKArchivTable;
    procedure EmptyLastZustandTable;
    function GetKArchivRecordCount: integer;
    function GetMomParaRecordCount: integer;
  end;

procedure ClearLastzustandsListe(var sl: TStringList);

implementation

{--------------------------- Allgemeine Funktionen ----------------------------}

{ Leert Stringliste mit PLaksLastenRec-Objekten                  }
{----------------------------------------------------------------}
procedure ClearLastzustandsListe(var sl: TStringList);
{----------------------------------------------------------------}
var
  i          : integer;
begin
  try
    for i:= 0 to sl.Count-1 do Dispose(PLaksLastenRec(sl.Objects[i]));
  except
  end;
end;

{----------------------------- TTbLaksParameter -------------------------------}

{----------------------------------------------------------------}
constructor TTbLaksParameter.Create(anId: integer; aPath: string);
{----------------------------------------------------------------}
begin
  { Pfad zu den Tabellen mit '\' abgeschlossen }
  DateiPfad:= aPath;
  if DateiPfad[Length(DateiPfad)] <> '\' then DateiPfad:= DateiPfad + '\';
  { StationsId }
  LaksId:= anId;
  { Tabellennamen }
  LParameterTabelle:= DateiPfad + C_TbLPar + Format('%.4d.DB', [LaksId]);
  LKArchivTabelle:= DateiPfad + C_TbLKArchiv + Format('%.4d.DB', [LaksId]);
  LLZustandTabelle:= DateiPfad + C_TbLLZ + Format('%.4d.DB', [LaksId]);
  { Triggerzeiten für Kontrolldateien abfragen }
  TriggerTimePara:= GetTriggerTime(LParameterTabelle);
  TriggerTimeKA:= GetTriggerTime(LKArchivTabelle);
  TriggerTimeLZ:= GetTriggerTime(LLZustandTabelle);

  inherited Create;
end;

{ Überprüft, ob die für diese Station benötigte         }
{ Parametertabelle bereits existiert                    }
{-------------------------------------------------------}
procedure TTbLaksParameter.CreateParameterTable;
{-------------------------------------------------------}
var
  q    : TQueryExt;
begin
  if not FileExists(LParameterTabelle) then begin
    q:= TQueryExt.Create(nil);
    try
      q.Sql.Add('CREATE TABLE "' + LParameterTabelle + '"');
      q.Sql.Add('(');
      q.Sql.Add(C_TfLPar_ParaNr + ' INTEGER,');
      q.Sql.Add(C_TfLPar_ParaWert + ' CHAR(24),');
      q.Sql.Add('PRIMARY KEY (' + C_TfLPar_ParaNr + ')');
      q.Sql.Add(')');
      q.ExecSql;
    finally
      q.Free;
    end;
  end;
end;

{ Überprüft, ob die für diese Station benötigte         }
{ KurzzeitArchivTabelle bereits existiert               }
{-------------------------------------------------------}
procedure TTbLaksParameter.CreateKArchivTable;
{-------------------------------------------------------}
var
  q    : TQueryExt;
begin
  if not FileExists(LKArchivTabelle) then begin
    q:= TQueryExt.Create(nil);
    try
      q.Sql.Add('CREATE TABLE "' + LKArchivTabelle + '"');
      q.Sql.Add('(');
      q.Sql.Add(C_TfLPar_LfdNr + ' SMALLINT,');
      q.Sql.Add(C_TfLPar_SatzStatus1 + ' SMALLINT,');
      q.Sql.Add(C_TfLPar_SatzStatus2 + ' SMALLINT,');
      q.Sql.Add(C_TfLPar_LastStatus + ' INTEGER,');
      q.Sql.Add(C_TfLPar_Arbeit1 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Leistung1 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Arbeit2 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Leistung2 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Arbeit3 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Leistung3 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Arbeit4 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Leistung4 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Arbeit5 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Leistung5 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Arbeit6 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Leistung6 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Arbeit7 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Leistung7 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Arbeit8 + ' NUMERIC,');
      q.Sql.Add(C_TfLPar_Leistung8 + ' NUMERIC,');
      q.Sql.Add('PRIMARY KEY (' + C_TfLPar_LfdNr + ')');
      q.Sql.Add(')');
      q.ExecSql;
    finally
      q.Free;
    end;
  end;
end;

{ Überprüft, ob die für diese Station benötigte         }
{ Lastzustandtabelle bereits existiert                  }
{-------------------------------------------------------}
procedure TTbLaksParameter.CreateLZustandTable;
{-------------------------------------------------------}
var
  q    : TQueryExt;
begin
  if not FileExists(LLZustandTabelle) then begin
    q:= TQueryExt.Create(nil);
    try
      q.Sql.Add('CREATE TABLE "' + LLZustandTabelle + '"');
      q.Sql.Add('(');
      q.Sql.Add(C_TfLLZ_LastNr + ' SMALLINT,');
      q.Sql.Add(C_TfLLZ_Zugeschaltet + ' BOOLEAN');
      q.Sql.Add(')');
      q.ExecSql;
    finally
      q.Free;
    end;
  end;
end;

{ Parameter-Rohdaten in Tabelle konvertieren      }
{ Parameter: Rohdaten-String                      }
{ Rückgabe: Restzeit bis zum Meßperiodenende in s }
{ Ergebnis: false, wenn Fehler bei Konvertierung  }
{------------------------------------------------------------------------------------}
function TTbLaksParameter.KonvMomentanParameterDaten (Rohdaten: string;
                                                      var Restzeit: integer): boolean;
{------------------------------------------------------------------------------------}
Const
  BWErfassung_ueber_AE = false; { LAKS-Spezialversion "Brennwerterfassung über Analogeingang"
                                  vorerst nicht implementiert !!! }
var
  ParaTable: TTableExt;
  Satz: string;
  i: integer;
  zeich: char;
  NrStr: string;
  WertStr: string;
  Nr: integer;
  Kommastellen: byte;
  ParaFormat: integer;

begin
  Result:=false;
  zeich:=NUL;
  i:=1;
  while (i <= length(Rohdaten)) AND (zeich <> STX) do begin                 { lesen bis STX }
    zeich:=Rohdaten[i];
    inc(i);
  end;
  if Rohdaten[i] <> 'B' then exit;                             { erstes Zeichen nach STX: B }

  ParaTable:=TTableExt.Create(nil);
  try
    ParaTable.DatabaseName:=DateiPfad;
    ParaTable.TableName:=C_TbLPar + Format('%.4d.DB', [LaksId]);
    if not ParaTable.Exists then
      CreateParameterTable;
    ParaTable.OpenShared;
    try
      inc(i);
      Satz:='';
      Kommastellen:=0;
      while i <= length(Rohdaten) do begin
        zeich:=Rohdaten[i];
        inc(i);
        if (zeich = RS) OR (zeich = ETX) then begin
          if Satz <> '' then begin
            NrStr:=Copy(Satz,1,6);
            Nr:=StrToInt(NrStr);
            WertStr:=Copy(Satz,7,Length(Satz)-6);

            if Nr = P_Kommastellen then
              Kommastellen:=StrToInt (WertStr); { für Umrechnung von FKZ-Parametern nötig }

            ParaFormat:=GetParaFormat (Nr);
            WertStr:=ParaWertStr (WertStr, Nr, ParaFormat, Kommastellen, BWErfassung_ueber_AE);
            { Werte in Tabelle schreiben: }
            if ParaTable.FindKey ([Nr]) then begin
              ParaTable.Edit;
              ParaTable.FieldByName (C_TfLPar_ParaWert).AsString:=WertStr;
              ParaTable.Post;
            end else
              ParaTable.InsertRecord ([Nr, WertStr]);
            { Restzeit zurückgeben: }
            if Nr = P_Restzeit then Restzeit:=StrToInt (WertStr);
          end;
          Satz:='';
          Application.ProcessMessages;
        end else
          Satz:=Satz+zeich;
      end;  { while }
    finally
      ParaTable.Close;
    end;
  finally
    ParaTable.Free;
  end;
  WriteNewTime(LParameterTabelle);
  Result:=true;
end;

{ Kurzzeitarchiv-Rohdaten in Tabelle konvertieren }
{ Parameter: Rohdaten-String                      }
{ Rückgabe: Datum und Zeit des letzten Eintrags im Kurzzeitarchiv }
{ Ergebnis: false, wenn Fehler bei Konvertierung  }
{-----------------------------------------------------------------------------}
function TTbLaksParameter.KonvKArchivDaten (Rohdaten: string;
                                            var LetztDatumStr: string;
                                            var LetztZeitStr: string): boolean;
{-----------------------------------------------------------------------------}
var
  Satz: string;
  i, j, m: integer;
  zeich: char;
  S, Status: string;
  ErstSatzNr: string;
  WertStr: string;
  ENr: integer;
  L: integer;
  StatusLen: word;
  IndexLen: byte;
  StatusPos: byte;
  Arbeit, Leistung: single;
  Satzstatus1: byte;
  Satzstatus2: byte;
  Laststatus: integer;
  KArchivTable: TTableExt;

begin
  Result:=false;
  IndexLen:=3;          { bei TW = 4 !!! }

  { keine Werte: STX K Datum(6) Zeit(6) Nummer(IndexLen) ETX }
  if length (Rohdaten) <= 16 + IndexLen then begin
    Result:=true;
    exit;
  end;

  Satz:='';
  zeich:=NUL;
  i:=1;

  while (i <= length (Rohdaten)) AND (zeich <> STX) do begin                { lesen bis STX }
    zeich:=Rohdaten[i];
    inc(i);
  end;
  if Rohdaten[i] <> 'K' then exit;                             { erstes Zeichen nach STX: K }
  inc(i);

  LetztDatumStr:='';
  for j:=1 to 6 do begin                                                 { LetztDatum lesen }
    LetztDatumStr:=LetztDatumStr + Rohdaten[i];
    inc(i);
  end;
  LetztZeitStr:='';
  for j:=1 to 6 do begin                                                  { LetztZeit lesen }
    LetztZeitStr:=LetztZeitStr + Rohdaten[i];
    inc(i);
  end;
  ErstSatzNr:='';
  for j:=1 to IndexLen do begin                                          { ErstSatzNr lesen }
    ErstSatzNr:=ErstSatzNr + Rohdaten[i];
    inc(i);
  end;
  ENr:=StrToInt (ErstSatzNr);
  if ENr < 1 then begin
    Result:=true;
    exit;
  end;

  if ENr = 1 then EmptyKArchivTable;         { bei Start oder Beginn einer neuen Meßperiode }

  KArchivTable:=TTableExt.Create(nil);
  try
    KArchivTable.DatabaseName:=DateiPfad;
    KArchivTable.TableName:=C_TbLKArchiv + Format('%.4d.DB', [LaksId]);
    if not KArchivTable.Exists then
      CreateKArchivTable;
    KArchivTable.OpenShared;
    try
      while i <= length (Rohdaten) do begin
        zeich:=Rohdaten[i];
        inc(i);
        if (zeich = RS) OR (zeich = ETX) then begin
          if Satz <> '' then begin
            Satzstatus1:=Ord(Satz[1]);                                     { Satzstatus 1 }
            Satzstatus2:=0;                            { Satzstatus 2 nur bei Doppelkurve }
            StatusPos:=2;
            S:=Copy(Satz,StatusPos,Length(Satz));
            Status:=F_Zerlegen(S,US);                                        { Kanalstati }

            { Status decodieren:
              nur die 6 niederwertigen Bit eines jeden Byte enthalten Informationen) }
            StatusLen:=Length(Status);
            L:=0;
            for m:=StatusLen downto 1 do
              L:= L + ((Byte (Status[m]) AND $3F) SHL (6 * (StatusLen - m)));
            Laststatus:=L;

            { Werte für Arbeit und Leistung: }
            WertStr:=Copy(S,1,8);
            if not Decode_LAKSRealStr (WertStr,Arbeit) then begin
              LetztDatumStr:='';
              LetztZeitStr:='';
              EmptyKArchivTable;
              WriteNewTime(LKArchivTabelle);
              exit;
            end;
            WertStr:=Copy(S,9,8);
            if not Decode_LAKSRealStr (WertStr,Leistung) then begin
              LetztDatumStr:='';
              LetztZeitStr:='';
              EmptyKArchivTable;
              WriteNewTime(LKArchivTabelle);
              exit;
            end;
            if KArchivTable.FindKey ([ENr]) then begin
              KArchivTable.Edit;
              KArchivTable.FieldByName (C_TfLPar_SatzStatus1).AsInteger:=Satzstatus1;
              KArchivTable.FieldByName (C_TfLPar_SatzStatus2).AsInteger:=Satzstatus2;
              KArchivTable.FieldByName (C_TfLPar_LastStatus).AsInteger:=Laststatus;
              KArchivTable.FieldByName (C_TfLPar_Arbeit1).AsFloat:=Arbeit;
              KArchivTable.FieldByName (C_TfLPar_Leistung1).AsFloat:=Leistung;
              KArchivTable.Post;
            end else
              KArchivTable.InsertRecord ([ENr, Satzstatus1, Satzstatus2, Laststatus,
                                          Arbeit, Leistung]);
            inc(ENr);
          end;
          Satz:='';
          Application.ProcessMessages;
        end else
          Satz:=Satz+zeich;
      end; { while }
    finally
      KArchivTable.Close;
    end;
  finally
    KArchivTable.Free;
  end;
  WriteNewTime(LKArchivTabelle);
  Result:=true;
end;

{ Lastzustand-Rohdaten in Tabelle konvertieren }
{ Parameter: Rohdaten-String                      }
{ Ergebnis: false, wenn Fehler bei Konvertierung  }
{-------------------------------------------------------------------------}
function TTbLaksParameter.KonvLastzustandDaten (Rohdaten: string): boolean;
{-------------------------------------------------------------------------}
var
  Satz: string;
  i: integer;
  zeich: char;
  LastNr: integer;
  Lasten_ein: boolean;
  LZustandTable: TTableExt;

begin
  Result:=false;
  EmptyLastzustandTable;

  zeich:=NUL;
  i:=1;
  while (i <= length (Rohdaten)) AND (zeich <> STX) do begin
    zeich:=Rohdaten[i];                                                    { lesen bis STX }
    inc(i);
  end;
  if Rohdaten[i] <> 'S' then exit;                            { erstes Zeichen nach STX: S }
  inc(i);

  LZustandTable:=TTableExt.Create(nil);
  try
    LZustandTable.DatabaseName:=DateiPfad;
    LZustandTable.TableName:=C_TbLLZ + Format('%.4d.DB', [LaksId]);
    if not LZustandTable.Exists then
      CreateLZustandTable;
    LZustandTable.OpenShared;
    try
      Lasten_ein:=true;                          { erst kommen die zugeschalteten Lasten }
      Satz:='';
      while i <= length (Rohdaten) do begin
        zeich:=Rohdaten[i];
        inc(i);
        if (zeich = US) OR (zeich = RS) OR (zeich = ETX) then begin
          if Satz <> '' then begin
            LastNr:=StrToInt(Satz);
            LZustandTable.AppendRecord ([LastNr, Lasten_ein]);
          end;
          if zeich = RS then Lasten_ein:=false;      { ab jetzt die abgeschalteten Lasten }
          Satz:='';
          Application.ProcessMessages;
        end else
          Satz:=Satz+zeich;
      end; { while }
    finally
      LZustandTable.Close;
    end;
  finally
    LZustandTable.Free;
  end;
  WriteNewTime(LLZustandTabelle);
  Result:=true;
end;

{ Leert KurzzeitArchiv-Tabelle                          }
{-------------------------------------------------------}
procedure TTbLaksParameter.EmptyKArchivTable;
{-------------------------------------------------------}
var
  q    : TQueryExt;
begin
  q:= TQueryExt.Create(nil);
  try
    if FileExists(LKArchivTabelle) then begin
      q.Sql.Add('DELETE FROM "' + LKArchivTabelle + '"');
      q.ExecSql;
    end;
  finally
    q.Free;
  end;
end;

{ Leert Lastzustand-Tabelle                             }
{-----------------------------------------------}
procedure TTbLaksParameter.EmptyLastzustandTable;
{-----------------------------------------------}
var
  q    : TQueryExt;
begin
  q:= TQueryExt.Create(nil);
  try
    if FileExists(LLZustandTabelle) then begin
      q.Sql.Add('DELETE FROM "' + LLZustandTabelle + '"');
      q.ExecSql;
    end;
  finally
    q.Free;
  end;
end;

{ Holt neue Lastzustände                                }
{ Parameter: Übergabe-Stringliste                       }
{-------------------------------------------------------}
function TTbLaksParameter.GetNewLastzustandsListe(var sl: TStringList): boolean;
{-------------------------------------------------------}
var
  q      : TQueryExt;
  p      : PLaksLastenRec;
begin
  result:= False;  { default }
  if (FileExists(LLZustandTabelle)) and
     (TriggerTimeLZ <> GetTriggerTime(LLZustandTabelle))
  then begin
    q:= TQueryExt.Create(nil);
    try
      q.Sql.Add('SELECT * FROM "' + LLZustandTabelle + '"');
      if q.Open then begin
        if q.RecordCount > 0 then result:= True;
        q.First;
        while not q.Eof do begin
          New(p);
          p^.LastNr:= q.FieldByName(C_TfLLZ_LastNr).asInteger;
          p^.Zugeschaltet:= q.FieldByName(C_TfLLZ_Zugeschaltet).asBoolean;
          sl.AddObject(IntToStr(p^.LastNr), TObject(p));
          q.Next;
        end;
        TriggerTimeLZ:= GetTriggerTime(LLZustandTabelle);
      end;
    finally
      q.Free;
    end;
  end;
end;

{ Holt neue Parameter in einem Query                    }
{ Parameter: ÜbergabeRecord                             }
{-------------------------------------------------------}
function TTbLaksParameter.GetNewParameterRec(var aRec: TLaksParameterRec): boolean;
{-------------------------------------------------------}
var
  q      : TQueryExt;
begin
  result:= False;  { default }
  aRec.MaxZone:= '';
  aRec.PMax:= '';
  aRec.PZiel:= '';
  aRec.PVerf:= '';
  aRec.PMom:= '';
  aRec.WIst:= '';
  aRec.Restzeit:= '';
  aRec.LaksZeit:= '';
  aRec.LaksDatum:= '';
  aRec.Einheit:= '';
  aRec.MPLaenge:= '';
  aRec.GerTyp:= '';
  aRec.Version:= '';
  aRec.ZFenster1:= '';
  aRec.ZFenster2:= '';
  aRec.ZFenster3:= '';
  aRec.ZFenster4:= '';
  if (FileExists(LParameterTabelle)) and
     (TriggerTimePara <> GetTriggerTime(LParameterTabelle))
  then begin
    q:= TQueryExt.Create(nil);
    try
      q.Sql.Add('SELECT * FROM "' + LParameterTabelle + '"');
     if q.Open then begin
        if q.RecordCount > 0 then result:= True;
        q.First;
        while not q.Eof do begin
          case q.FieldByName(C_TfLPar_ParaNr).asInteger of
            P_Programmtyp: aRec.GerTyp:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_Programmversion: aRec.Version:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_Datum: aRec.LaksDatum:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_Zeit: aRec.LaksZeit:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_Messperiode: aRec.MPLaenge:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_EinheitSumme1: aRec.Einheit:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_Pmom_Summe1: aRec.PMom:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_Wist_Summe1: aRec.WIst:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_Restzeit: aRec.Restzeit:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_Pverf_Summe1: aRec.PVerf:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_Pziel_Summe1: aRec.PZiel:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_Pmax_Summe1: aRec.PMax:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_AktMaximumZone: aRec.MaxZone:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_ZFBreite1: aRec.ZFenster1:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_ZFBreite2: aRec.ZFenster2:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_ZFBreite3: aRec.ZFenster3:= q.FieldByName(C_TfLPar_ParaWert).asString;
            P_ZFBreite4: aRec.ZFenster4:= q.FieldByName(C_TfLPar_ParaWert).asString;
          end;
          q.Next;
        end;
        TriggerTimePara:= GetTriggerTime(LParameterTabelle);
      end;
    finally
      q.Free;
    end;
  end;
end;

{ Holt neue Archivwerte in einem Query                  }
{ Parameter: Übergabequery; LfdNr., ab der geholt wird  }
{            aRes: 0 - OK; 1 - neue Tabelle; 255 - keine Tabelle da }
{-------------------------------------------------------}
function TTbLaksParameter.GetNewKArchivQuery(var q: TQueryExt; LfdNr: integer;
                                             var aRes: byte): boolean;
{-------------------------------------------------------}
begin
  result:= False;  { default }
  aRes:= 0;
  if (FileExists(LKArchivTabelle)) then begin
    if (TriggerTimeKA <> GetTriggerTime(LKArchivTabelle)) then begin
      q.Close;
      q.SQL.Clear;
      q.DatabaseName:= DateiPfad;
      q.Sql.Add('SELECT * FROM "' + LKArchivTabelle + '"');
      q.Sql.Add('WHERE ' + C_TfLPar_LfdNr + ' > ' + IntToStr(LfdNr));
      if q.Open then begin  { Abfrage nach neuen Einträgen }
        if q.recordcount > 0 then begin
          aRes:= 0;
          TriggerTimeKA:= GetTriggerTime(LKArchivTabelle);
          result:= True;
        end
        else begin      { Tabelle vermutlich neu => Abfrage nach allen Einträgen }
          q.Close;
          q.SQL.Clear;
          q.Sql.Add('SELECT * FROM "' + LKArchivTabelle + '"');
          if q.Open then begin
            if (q.recordcount > 0) and (q.recordcount < LfdNr) then begin
              aRes:= 1;
              result:= True;
            end;
            TriggerTimeKA:= GetTriggerTime(LKArchivTabelle);
          end
        end;
      end;
    end;
  end
  else aRes:= 255;
end;

{ Recordcount der KArchiv-Tabelle                       }
{ Rückgabe: Anzahl der Sätze                            }
{-------------------------------------------------------}
function TTbLaksParameter.GetKArchivRecordCount: integer;
{-------------------------------------------------------}
begin
  result:= -1;  { default }
  if (FileExists(LKArchivTabelle)) then begin
    with TQueryExt.Create(nil) do begin
      try
        Sql.Add('SELECT COUNT(' + C_TfLPar_LfdNr + ')');
        Sql.Add('FROM "' + LKArchivTabelle + '"');
        if Open then result:= Fields[0].asInteger;
      finally
        Free;
      end;
    end;
  end;
end;

{ Recordcount der KArchiv-Tabelle                       }
{ Rückgabe: Anzahl der Sätze                            }
{-------------------------------------------------------}
function TTbLaksParameter.GetMomParaRecordCount: integer;
{-------------------------------------------------------}
begin
  result:= -1;  { default }
  if (FileExists(C_TfLPar_ParaNr)) then begin
    with TQueryExt.Create(nil) do begin
      try
        Sql.Add('SELECT COUNT(' + C_TfLPar_ParaNr + ')');
        Sql.Add('FROM "' + LParameterTabelle + '"');
        if Open then result:= Fields[0].asInteger;
      finally
        Free;
      end;
    end;
  end;
end;

end.
