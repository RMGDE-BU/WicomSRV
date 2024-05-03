{********************************************************************************}
{* Unit: Zugriff auf LAKS-Parametrierungstabelle                                *}
{* 21.07.1999 WW                                                                *}
{ 28.07.1999 GD; Erweiteret um Zugriffproperties                               }
{********************************************************************************}
unit tbLaksPe;

interface

uses
  Forms, SysUtils, WTables, Db_Attn, Db, DbTables, LAKSUtil, ParaNum;

const
  { Tabelle für Parametereinstellen }
  C_TbLPE                = 'LPE';

  C_TfLPE_ParaNr         = 'ParameterNr';      { int }
  C_TfLPE_ParaWertAlt    = 'ParameterWertAlt'; { str24 }
  C_TfLPE_ParaWertNeu    = 'ParameterWertNeu'; { str24 }
  C_TfLPE_Stellen        = 'Stellen';          { smallint }
  C_TfLPE_Fertig         = 'Fertig';           { log }


type

  ParaSendenDataRec = record
    ParaNr: integer;
    ParaWert: string [24];
    Stellen: integer;
  end;

  ParaErgebnisDataRec = record
    ParaNr: integer;
    ParaWertAlt: string [24];
    ParaWertNeu: string [24];
  end;

  TEinEBandRec = record
    ParaStatus   : SmallInt;
    Wert         : string[24];
    Startzeit    : TDateTime;
    EndeZeit     : TDateTime;
  end;

  TAlleEBandRec = record
    EBandA       : TEinEBandRec;
    EBandB       : TEinEBandRec;
    EBandC       : TEinEBandRec;
  end;

  { Objekt für Tabelle zur Parametrierung des LAKS }

  TTbLaksParametrierung = class(TObject)
  private
    Path         : TFileName;
    tbLPE        : TTableExt;
    LAKSId       : integer;
    FKommastelle : integer;
    FEinheit     : string;
    FPMax        : string;
    procedure CreateTbLPE;
    function GetFileName: string;
    function DoesTableExist: boolean;
    function GetKommastelle: integer;
    function GetEinheit: string;
    function GetPMax: string;
    function GetWert(EB: integer): string;
    procedure SetWert(EB: integer; Value: string);
    function GetStartZeit(EB: integer): string;
    procedure SetStartZeit(EB: integer; Value: string);
    function GetEndeZeit(EB: integer): string;
    procedure SetEndeZeit(EB: integer; Value: string);
    function GetEBRecord(EB: integer): TAlleEBandRec;
  public
    constructor Create (APath: TFileName; ALAKSId: integer);
    destructor Destroy; override;
    function WriteLAKSParameter (Rohdateiname: string; Loeschen: boolean): boolean;
    function GetSendeParameter (var ParaSendenData: ParaSendenDataRec): boolean;
    function ParameterChangeOk: boolean;
    procedure WriteErgebnis (ParaErgebnisData: ParaErgebnisDataRec);
    property Filename: string read GetFileName;
    property TableExists: boolean read DoesTableExist;
    property Kommastelle: integer read GetKommastelle;
    property Einheit: string read GetEinheit;
    property PMax: string read GetPMax;
    property Wert[index: integer]: string read GetWert write SetWert;
    property StartZeit[index: integer]: string read GetStartZeit write SetStartZeit;
    property EndeZeit[index: integer]: string read GetEndeZeit write SetEndeZeit;
    property EBRecord[index: integer]: TAlleEBandRec read GetEBRecord;
  end;

implementation

{ Allgemeine Funtionen }

{ Konvertiert LaksParameterstring in DateTime-String                         }
{----------------------------------------------------------------------------}
function KonvertPStrToDTStr(aString: string): string;
{----------------------------------------------------------------------------}
const
  C_Offen = '800000000000';
var
  i             : integer;
  j             : string[4];
  m, t, h, n    : string[2];
begin
  if (aString = C_Offen) then begin
    result:= '-';
  end
  else begin
    i:= StrToIntDef(System.Copy(aString, 1, 2), 0);
    if (i >= 80) then i:= i + 1900 else i:= i + 2000;  { Y2K }
    j:= IntToStr(i);
    m:= System.Copy(aString, 3, 2);
    t:= System.Copy(aString, 5, 2);
    h:= System.Copy(aString, 7, 2);
    n:= System.Copy(aString, 9, 2);
    result:= t + '.' + m + '.' + j + '  ' + h + ':' + n;
  end;
end;

{ Konvertiert DateTime-String (TT.MM.JJJJ  HH:NN(:SS)) in DateTime-Format    }
{----------------------------------------------------------------------------}
function KonvertDTStrToDT(aString: string): TDateTime;
{----------------------------------------------------------------------------}
var
  s        : string;
begin
  if Length(aString) > 8 then begin
    s:= aString;
    result:= StrToDate(copy(s, 1, pos(' ', s)-1));
    System.Delete(s, 1, 12);
    if (StrToIntDef(Copy(s, 1, 2), 0) <> 24) then
      result:= result + StrToTime(s)
    else
      result:= result + 1;
  end
  else result:= 0;
end;

{ TTbLaksParametrierung }

{----------------------------------------------------------------------------}
constructor TTbLaksParametrierung.Create (APath: TFileName; ALAKSId: integer);
{----------------------------------------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  LAKSId:=ALAKSId;
  tbLPE:=TTableExt.Create(nil);
  tbLPE.DataBaseName:= Path;
  tbLPE.TableName:= C_TbLPE + Format('%.4d.DB', [LaksId]);
  FKommastelle:= -1;      { default, wenn Kommastelle noch nicht gesetzt wurde }
  FEinheit:= '';          { default, wenn Einheit noch nicht gesetzt wurde }
  FPMax:= '';             { default, wenn PMax noch nicht gesetzt wurde }
end;

{---------------------------------------}
destructor TTbLaksParametrierung.Destroy;
{---------------------------------------}
begin
  tbLPE.Free;
  inherited Destroy;
end;

{------------------------------------------}
procedure TTbLaksParametrierung.CreateTbLPE;
{------------------------------------------}
{ Tabelle anlegen }
begin
  with tbLPE.FieldDefs do begin
    Clear;
    Add(C_TfLPE_ParaNr, ftInteger, 0, false);
    Add(C_TfLPE_ParaWertAlt, ftString, 24, false);
    Add(C_TfLPE_ParaWertNeu, ftString, 24, false);
    Add(C_TfLPE_Stellen, ftSmallInt, 0, false);
    Add(C_TfLPE_Fertig, ftBoolean, 0, false);
  end;
  with tbLPE.IndexDefs do begin
    Clear;
    Add('Primaerindex', C_TfLPE_ParaNr, [ixPrimary, ixUnique]);
  end;
  tbLPE.CreateTable;
end;

{ Gibt Filenamen mit Pafd zurück           }
{------------------------------------------}
function TTbLaksParametrierung.GetFileName: string;
{------------------------------------------}
begin
  result:= Path;
  if (result[Length(result)] <> '\') then result:= result + '\';
  result:= result + tbLPE.TableName;
end;

{ Parameter-Rohdaten in Tabelle konvertieren (keine Formatierung der Rohwerte !) }
{ Parameter: Rohdateiname                                                        }
{ Ergebnis: false, wenn Fehler bei Konvertierung                                 }
{-----------------------------------------------------------------------------}
function TTbLaksParametrierung.WriteLAKSParameter (Rohdateiname: string;
                                                   Loeschen: boolean): boolean;
{-----------------------------------------------------------------------------}
var
  D_file: file of char;
  Satz: string;
  zeich: char;
  NrStr: string;
  WertStr: string;
  Nr: integer;

begin
  Result:=false;
  AssignFile(D_File, RohDateiname);                                         { Rohdatenfile }
  {$I-} Reset(D_file); {$I+}
  if IOResult <> 0 then exit;

  zeich:=NUL;
  while zeich <> STX do                                                    { lesen bis STX }
    read(D_file,zeich);
  read(D_file,zeich);                                         { erstes Zeichen nach STX: B }
  if zeich <> 'B' then begin
    CloseFile(D_file);
    exit;
  end;

  if not tbLPE.Exists then
    CreateTbLPE
  else
    tbLPE.EmptyTable;
  tbLPE.OpenShared;
  try
    Satz:='';
    while not eof(D_file) do begin
      Read(D_file,zeich);
      if (zeich = RS) OR (zeich = ETX) then begin
        if Satz <> '' then begin
          NrStr:=Copy(Satz,1,6);
          Nr:=StrToInt(NrStr);
          WertStr:=Copy(Satz,7,Length(Satz)-6);
          { Nr, Wert und Vorbelegung für "Fertig" in Tabelle schreiben: }
          tbLPE.InsertRecord ([Nr, WertStr, nil, nil, false]);
        end;
        Satz:='';
        Application.ProcessMessages;
      end
      else
        Satz:=Satz+zeich;
    end;
  finally
    tbLPE.Close;
  end;
  { Triggerfile schreiben, wenn alle Parameter in Tabelle konvertiert wurden: }
  WriteNewTime(tbLPE.DatabaseName + tbLPE.TableName);
  CloseFile(D_file);

  if Loeschen then
    DeleteFile (Rohdateiname);
  Result:=true;
end;

{ Eintrag für zu übertragenden Parameter aus Tabelle lesen.                    }
{ Rückgabe: Nummer, neuer Wert und Stellenzahl des zu übertragenden Parameters }
{ Ergebnis: false, wenn kein Eintrag mit neuem Wert mehr vorhanden             }
{-----------------------------------------------------------------------------------------------}
function TTbLaksParametrierung.GetSendeParameter (var ParaSendenData: ParaSendenDataRec): boolean;
{-----------------------------------------------------------------------------------------------}
var
  WertNeu: string;
begin
  Result:=false;
  with ParaSendenData do begin
    ParaNr:=-1;
    Parawert:='';
    Stellen:=-1;
  end;
  if tbLPE.Exists then begin
    tbLPE.OpenShared;
    try
      { Filter auf noch nicht bearbeitete Einträge: }
      tbLPE.Filtered:=false;                                     { Deaktivieren des Filters }
      tbLPE.Filter:=C_TfLPE_Fertig + ' = ''FALSE''';
      tbLPE.Filtered:=true;                                        { Aktivieren des Filters }
      while not tbLPE.Eof do begin
        WertNeu:=tbLPE.FieldByName (C_TfLPE_ParaWertNeu).AsString;
        if WertNeu <> '' then begin               { Daten des zu sendenden Parameters lesen }
          with ParaSendenData do begin
            ParaNr:=tbLPE.FieldByName (C_TfLPE_ParaNr).AsInteger;
            ParaWert:=WertNeu;
            Stellen:=tbLPE.FieldByName (C_TfLPE_Stellen).AsInteger;
          end;
          tbLPE.Edit;
          { Vorbelegung, falls Parameter nicht übertragen werden kann: }
          tbLPE.FieldByName (C_TfLPE_ParaWertAlt).AsString:='';
          tbLPE.FieldByName (C_TfLPE_ParaWertNeu).AsString:='';
          { Kennzeichnung für Abrufmodul, daß Parameter bearbeitet wurde: }
          tbLPE.FieldByName (C_TfLPE_Fertig).AsBoolean:=true;
          tbLPE.Post;
          Result:=true;
          Break;
        end;
        tbLPE.Next;
      end;
      tbLPE.Filtered:=false;                                     { Deaktivieren des Filters }
    finally
      tbLPE.Close;
    end;
  end;
end;

{ Ergebnis der Übertragung eines Parameters in Tabelle schreiben              }
{ Parameter: Nummer, alter und neuer Wert des übertragenen Parameters         }
{ Achtung: Wenn für einen Eintrag in der Ergebnistabelle der alte und neue Parameterwert
           fehlen oder gleich sind, gilt: Der Parameter konnte nicht übertragen werden ! }
{------------------------------------------------------------------------------------}
procedure TTbLaksParametrierung.WriteErgebnis (ParaErgebnisData: ParaErgebnisDataRec);
{------------------------------------------------------------------------------------}
begin
  if tbLPE.Exists then begin
    tbLPE.OpenShared;
    try
      with ParaErgebnisData do begin
        if tbLPE.FindKey ([ParaNr]) then begin
          tbLPE.Edit;
          tbLPE.FieldByName (C_TfLPE_ParaWertAlt).AsString:=ParaWertAlt;
          tbLPE.FieldByName (C_TfLPE_ParaWertNeu).AsString:=ParaWertNeu;
          tbLPE.Post;
        end;
      end;
    finally
      tbLPE.Close;
    end;
  end;
end;

{ Existiert die Tabelle                                                       }
{-----------------------------------------------------------------------------}
function TTbLaksParametrierung.DoesTableExist: boolean;
{-----------------------------------------------------------------------------}
begin
  result:= tbLPE.Exists;
end;

{ Gibt Werterecord eines bestimmten Enerigiebandes zurück                     }
{-----------------------------------------------------------------------------}
function TTbLaksParametrierung.GetEBRecord(EB: integer): TAlleEBandRec;
{-----------------------------------------------------------------------------}
var
  rec      : TEinEBandRec;
  isOk     : boolean;
begin
  if (tbLPE.Exists) then begin
  { Zunächst die Vorgabe (A, B, C)´den Nummern (100, 200, 300) zuordnen }
    result.EBandA.ParaStatus:= 100;
    result.EBandA.Wert:= Wert[100+EB];
    result.EBandA.Startzeit:= KonvertDTStrToDT(Startzeit[100+EB]);
    result.EBandA.Endezeit:= KonvertDTStrToDT(Endezeit[100+EB]);
    result.EBandB.ParaStatus:= 200;
    result.EBandB.Wert:= Wert[200+EB];
    result.EBandB.Startzeit:= KonvertDTStrToDT(Startzeit[200+EB]);
    result.EBandB.Endezeit:= KonvertDTStrToDT(Endezeit[200+EB]);
    result.EBandC.ParaStatus:= 300;
    result.EBandC.Wert:= Wert[300+EB];
    result.EBandC.Startzeit:= KonvertDTStrToDT(Startzeit[300+EB]);
    result.EBandC.Endezeit:= KonvertDTStrToDT(Endezeit[300+EB]);
  { Dann Sortieren }
    isOk:= False;
    while (not isOk) do begin
      isOk:= True;
      if (trunc(result.EBandC.Startzeit) > 0) then begin
         if (trunc(result.EBandB.Startzeit) = 0) or
            ((trunc(result.EBandC.Startzeit) < trunc(result.EBandB.Startzeit)) and
            (trunc(result.EBandB.Startzeit) <> 0))
         then begin
           rec:= result.EBandC;
           result.EBandC:= result.EBandB;
           result.EBandB:= rec;
           isOk:= False;
         end;
      end;
      if (trunc(result.EBandB.Startzeit) > 0) then begin
         if (trunc(result.EBandA.Startzeit) = 0) or
            ((trunc(result.EBandB.Startzeit) < trunc(result.EBandA.Startzeit)) and
            (trunc(result.EBandA.Startzeit) <> 0))
         then begin
           rec:= result.EBandB;
           result.EBandB:= result.EBandA;
           result.EBandA:= rec;
           isOk:= False;
         end;
      end;
    end;
  end;
end;

{ Gibt Kommastelle zurück                                                     }
{-----------------------------------------------------------------------------}
function TTbLaksParametrierung.GetKommastelle: integer;
{-----------------------------------------------------------------------------}
begin
  if (FKommaStelle = -1) and (tbLPE.Exists) then begin
    with TQueryExt.Create(nil) do begin
      try
        DataBaseName:= Path;
        Sql.Add('SELECT ' + C_TfLPE_ParaWertAlt);
        Sql.Add('FROM "' + tbLPE.TableName + '"');
        Sql.Add('WHERE ' + C_TfLPE_ParaNr + ' = ' + IntToStr(P_Kommastellen));
        if (Open) and (RecordCount = 1) then begin
          FKommastelle:= StrToIntDef(FieldByName(C_TfLPE_ParaWertAlt).asString, -1);
        end;
      finally
        Free;
      end;
    end;
  end;
  result:= FKommastelle;
end;

{ Gibt Einheit zurück                                                         }
{-----------------------------------------------------------------------------}
function TTbLaksParametrierung.GetEinheit: string;
{-----------------------------------------------------------------------------}
begin
  if (FEinheit = '') and (tbLPE.Exists) then begin
    with TQueryExt.Create(nil) do begin
      try
        DataBaseName:= Path;
        Sql.Add('SELECT ' + C_TfLPE_ParaWertAlt);
        Sql.Add('FROM "' + tbLPE.TableName + '"');
        Sql.Add('WHERE ' + C_TfLPE_ParaNr + ' = ' + IntToStr(P_EinheitSumme1));
        if (Open) and (RecordCount = 1) then begin
          FEinheit:= GetLeistungEinheit(FieldByName(C_TfLPE_ParaWertAlt).asString);
        end;
      finally
        Free;
      end;
    end;
  end;
  result:= FEinheit;
end;

{ Gibt PMax zurück                                                            }
{-----------------------------------------------------------------------------}
function TTbLaksParametrierung.GetPMax: string;
{-----------------------------------------------------------------------------}
var
  i         : integer;
begin
  if (FPMax = '') and (tbLPE.Exists) then begin
    with TQueryExt.Create(nil) do begin
      try
        DataBaseName:= Path;
        Sql.Add('SELECT ' + C_TfLPE_ParaWertAlt);
        Sql.Add('FROM "' + tbLPE.TableName + '"');
        Sql.Add('WHERE ' + C_TfLPE_ParaNr + ' = ' + IntToStr(P_Pmax_aus_Energieband));
        if (Open) and (RecordCount = 1) then begin
          FPMax:= FieldByName(C_TfLPE_ParaWertAlt).asString;
          if (Length(FPMax) <= Kommastelle) then
            for i:= 0 to (Kommastelle - Length(FPMax)) do FPMax:= '0' + FPMax;
          if (Kommastelle > 0) then
            System.Insert(DecimalSeparator , FPMax, Length(FPMax) - Kommastelle + 1);
        end;
      finally
        Free;
      end;
    end;
  end;
  result:= FPMax;
end;

{ Gibt Wert eines bestimmten Enerigiebandes zurück                            }
{-----------------------------------------------------------------------------}
function TTbLaksParametrierung.GetWert(EB: integer): string;
{-----------------------------------------------------------------------------}
var
  i          : integer;
begin
  result:= ''; { default }
  if (tbLPE.Exists) then begin
  { Bei einer Vorgabe (A, B, C) ist der Parameter als 100 verschlüsselt }
    i:= P_EBandParaOffset + 100*(EB div 100) + (3*((EB mod 100)-1)) + P_EBandWert;
    with TQueryExt.Create(nil) do begin
      try
        DataBaseName:= Path;
        Sql.Add('SELECT ' + C_TfLPE_ParaWertAlt);
        Sql.Add('FROM "' + tbLPE.TableName + '"');
        Sql.Add('WHERE ' + C_TfLPE_ParaNr + ' = ' + IntToStr(i));
        if (Open) and (RecordCount = 1) then begin
          result:= FieldByName(C_TfLPE_ParaWertAlt).asString;
          if (Length(result) <= Kommastelle) then
            for i:= 0 to (Kommastelle - Length(result)) do result:= '0' + result;
          if (Kommastelle > 0) and (Length(result) > Kommastelle) then
            System.Insert(DecimalSeparator , result, Length(result) - Kommastelle + 1);
        end;
      finally
        Free;
      end;
    end;
  end;
end;

{ Setzt neuen Wert eines bestimmten Enerigiebandes                            }
{-----------------------------------------------------------------------------}
procedure TTbLaksParametrierung.SetWert(EB: integer; Value: string);
{-----------------------------------------------------------------------------}
var
  i          : integer;
  s          : string;
begin
  if (tbLPE.Exists) then begin
    s:= Value;
  { Wert richtig formatieren }
    if pos(DecimalSeparator, Value) > 0 then begin
      if ((Kommastelle - (Length(s) - pos(DecimalSeparator, Value))) < 0)
      then begin
        for i:= 1 to ((Length(s) - pos(DecimalSeparator, Value)) - Kommastelle)
          do System.Delete(s, Length(s), 1);
      end;
      for i:= 1 to (Kommastelle - (Length(s) - pos(DecimalSeparator, Value)))
        do s:= s + '0';
      System.Delete(s, Pos(DecimalSeparator, s), 1);
    end
    else begin
      for i:= 1 to Kommastelle do s:= s + '0';
    end;
  { '0' wird ohne Kommastellen übertragen }
    if (StrToIntDef(s, 0) = 0) then s:= '0';
  { Bei einer Vorgabe (A, B, C) ist der Parameter als 100 verschlüsselt }
    i:= P_EBandParaOffset + 100*(EB div 100) + (3*((EB mod 100)-1)) + P_EBandWert;
    with TQueryExt.Create(nil) do begin
      try
        DataBaseName:= Path;
        Sql.Add('UPDATE "' + tbLPE.TableName + '"');
        Sql.Add('SET ' + C_TfLPE_ParaWertNeu + ' = "' + s + '"');
        Sql.Add('WHERE ' + C_TfLPE_ParaNr + ' = ' + IntToStr(i));
        Sql.Add('AND ' + C_TfLPE_ParaWertAlt + ' <> "' + s + '"');
        ExecSql;
      finally
        Free;
      end;
    end;
  end;
end;

{ Gibt Startzeitpunkt eines bestimmten Enerigiebandes zurück                  }
{-----------------------------------------------------------------------------}
function TTbLaksParametrierung.GetStartZeit(EB: integer): string;
{-----------------------------------------------------------------------------}
const
  C_Offen = '800000000000';
var
  s             : string;
  i             : integer;
begin
  result:= ''; { default }
  if (tbLPE.Exists) then begin
  { Bei einer Vorgabe (A, B, C) ist der Parameter als 100 verschlüsselt }
    i:= P_EBandParaOffset + 100*(EB div 100) + (3*((EB mod 100)-1)) + P_EBandStart;
    with TQueryExt.Create(nil) do begin
      try
        DataBaseName:= Path;
        Sql.Add('SELECT ' + C_TfLPE_ParaWertAlt);
        Sql.Add('FROM "' + tbLPE.TableName + '"');
        Sql.Add('WHERE ' + C_TfLPE_ParaNr + ' = ' + IntToStr(i));
        if (Open) and (RecordCount = 1) then begin
          s:= FieldByName(C_TfLPE_ParaWertAlt).asString;
          result:= KonvertPStrToDTStr(s);
        end;
      finally
        Free;
      end;
    end;
  end;
end;

{ Setzt neuen  Startzeitpunkt eines bestimmten Enerigiebandes                 }
{-----------------------------------------------------------------------------}
procedure TTbLaksParametrierung.SetStartZeit(EB: integer; Value: string);
{-----------------------------------------------------------------------------}
const
  C_Offen = '800000000000';
var
  s             : string;
  i             : integer;
begin
  if (tbLPE.Exists) then begin
    s:= Value;
    if (s = '-') then s:= C_Offen;
  { Bei einer Vorgabe (A, B, C) ist der Parameter als 100 verschlüsselt }
    i:= P_EBandParaOffset + 100*(EB div 100) + (3*((EB mod 100)-1)) + P_EBandStart;
    with TQueryExt.Create(nil) do begin
      try
        DataBaseName:= Path;
        Sql.Add('UPDATE "' + tbLPE.TableName + '"');
        Sql.Add('SET ' + C_TfLPE_ParaWertNeu + ' = "' + s + '"');
        Sql.Add('WHERE ' + C_TfLPE_ParaNr + ' = ' + IntToStr(i));
        Sql.Add('AND ' + C_TfLPE_ParaWertAlt + ' <> "' + s + '"');
        ExecSql;
      finally
        Free;
      end;
    end;
  end;
end;

{ Gibt Endezeitpunkt eines bestimmten Enerigiebandes zurück                   }
{-----------------------------------------------------------------------------}
function TTbLaksParametrierung.GetEndeZeit(EB: integer): string;
{-----------------------------------------------------------------------------}
const
  C_Offen = '800000000000';
var
  s             : string;
  i             : integer;
begin
  result:= ''; { default }
  if (tbLPE.Exists) then begin
  { Bei einer Vorgabe (A, B, C) ist der Parameter als 100 verschlüsselt }
    i:= P_EBandParaOffset + 100*(EB div 100) + (3*((EB mod 100)-1)) + P_EBandEnde;
    with TQueryExt.Create(nil) do begin
      try
        DataBaseName:= Path;
        Sql.Add('SELECT ' + C_TfLPE_ParaWertAlt);
        Sql.Add('FROM "' + tbLPE.TableName + '"');
        Sql.Add('WHERE ' + C_TfLPE_ParaNr + ' = ' + IntToStr(i));
        if (Open) and (RecordCount = 1) then begin
          s:= FieldByName(C_TfLPE_ParaWertAlt).asString;
          result:= KonvertPStrToDTStr(s);
        end;
      finally
        Free;
      end;
    end;
  end;
end;

{ Setzt neuen  Startzeitpunkt eines bestimmten Enerigiebandes                 }
{-----------------------------------------------------------------------------}
procedure TTbLaksParametrierung.SetEndeZeit(EB: integer; Value: string);
{-----------------------------------------------------------------------------}
const
  C_Offen = '800000000000';
var
  s             : string;
  i             : integer;
begin
  if (tbLPE.Exists) then begin
    s:= Value;
    if (s = '-') then s:= C_Offen;
  { Bei einer Vorgabe (A, B, C) ist der Parameter als 100 verschlüsselt }
    i:= P_EBandParaOffset + 100*(EB div 100) + (3*((EB mod 100)-1)) + P_EBandEnde;
    with TQueryExt.Create(nil) do begin
      try
        DataBaseName:= Path;
        Sql.Add('UPDATE "' + tbLPE.TableName + '"');
        Sql.Add('SET ' + C_TfLPE_ParaWertNeu + ' = "' + s + '"');
        Sql.Add('WHERE ' + C_TfLPE_ParaNr + ' = ' + IntToStr(i));
        Sql.Add('AND ' + C_TfLPE_ParaWertAlt + ' <> "' + s + '"');
        ExecSql;
      finally
        Free;
      end;
    end;
  end;
end;

{ Setzt neuen  Startzeitpunkt eines bestimmten Enerigiebandes                 }
{-----------------------------------------------------------------------------}
function TTbLaksParametrierung.ParameterChangeOk: boolean;
{-----------------------------------------------------------------------------}
begin
  result:= True; { default }
  if (tbLPE.Exists) then begin
    with TQueryExt.Create(nil) do begin
      try
        DataBaseName:= Path;
  { Alle bearbeiteten Parameter holen }
        Sql.Add('SELECT ' + C_TfLPE_ParaWertAlt + ', ' + C_TfLPE_ParaWertNeu);
        Sql.Add('FROM "' + tbLPE.TableName + '"');
        Sql.Add('WHERE ' + C_TfLPE_Fertig + ' = :TRUE');
        ParamByName('TRUE').asBoolean:= True;
        if Open then begin
          while not eof do begin
            if (FieldByName(C_TfLPE_ParaWertAlt).asString =
                FieldByName(C_TfLPE_ParaWertNeu).asString)
            then result:= False;
            break;
            Next;
          end;
        end;
  { Alle bearbeiteten Parameter auf False zurücksetzen }
        Close;
        Sql.Clear;
        Sql.Add('UPDATE "' + tbLPE.TableName + '"');
        Sql.Add('SET ' + C_TfLPE_Fertig + ' = :FALSE');
        ParamByName('FALSE').asBoolean:= False;
        ExecSql;
      finally
        Free;
      end;
    end;
  end;
end;

end.
