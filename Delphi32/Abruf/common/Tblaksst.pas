{------------------------------------------------------------------------------}
{ 02.06.1999 GD; Tabellenobjekt für LAKS-Stammdaten                            }
{------------------------------------------------------------------------------}
unit tbLaksSt;

interface

uses
  SysUtils, Classes, Controls, Forms, WSysCon, DbTables, WTables, tbLaksAr;

type
  PInteger        = ^integer;

  PLaksStammdaten = ^TLaksStammdaten;
  TLaksStammdaten = record
    StationsId     : SmallInt;
    Stationsname   : string[40];
    Kennung        : string[12];
    LaksTyp        : SmallInt;
    LaksName       : string[12];
    Aktiv          : boolean;
    Summierung     : boolean;
    SStelle_V24    : SmallInt;
    SStelle_DFU    : SmallInt;
    Rufnummer      : string[20];
    Passwort       : string[16];
  end;

  TTbLaksStammdaten = class(TObject)
  private
    StammdatenPfad  : string;
    ArchivdatenPfad : string;
    function VerifyStationsId(anId: integer): boolean;
    function GetLAKSStammdaten(anId: integer): TLaksStammdaten;
    procedure SetLAKSStammdaten(anId: integer; LSta: TLaksStammdaten);
    function CheckTables: boolean;
  public
    constructor Create(StammDatenDir, ArchivDatenDir: string);
    procedure GetLaksGeraeteTypen(sl: TStringlist);
    function KennungExists(anId: integer; aKennung: string): boolean;
    function NewLaksId: integer;
    procedure DeleteLaksStaEntry(anId: integer);
    procedure DeleteLaksArchivDaten(aKennung: string);
    procedure GetLaksSummierungsListe(var sl: TStringList);
    procedure GetLaksSerielleListe(var sl: TStringList);
    property LAKSStammdaten[Index: Integer]: TLaksStammdaten
                                 read GetLAKSStammdaten write SetLAKSStammdaten;
  end;

procedure ClearLaksRecordList(sl: TStringList);

implementation

{-------------------------- Allgemeine Funktionen -----------------------------}

{ Leert eine mit TLaksStammdaten-Record-Objekten gefüllte Stringliste         }
{-----------------------------------------------------------------------------}
procedure ClearLaksRecordList(sl: TStringList);
{-----------------------------------------------------------------------------}
var
  i        : integer;
begin
  try
    for i:= 0 to sl.Count-1 do Dispose(PLaksStammdaten(sl.Objects[i]));;
  except
    sl.Clear;
  end;
  sl.Clear;
end;

{----------------------------- TTbLaksStammdaten ------------------------------}

{-----------------------------------------------------------------------------}
constructor TTbLaksStammdaten.Create(StammDatenDir, ArchivDatenDir: string);
{-----------------------------------------------------------------------------}
begin
  inherited Create;
  StammdatenPfad:= StammDatenDir;
  ArchivdatenPfad:= ArchivDatenDir;
  CheckTables;
end;

{ Überprüft, ob alle benötigten Tabellen vorhanden sind                       }
{-----------------------------------------------------------------------------}
function TTbLaksStammdaten.CheckTables: boolean;
{-----------------------------------------------------------------------------}
begin
  result:= True;  { default }
  if not FileExists(StammdatenPfad + C_LTB_LSta) then result:= False;
end;

{ Überprüft, ob die StationsId in den Stammdaten angelegt und als "Aktiv"     }
{ gekennzeichnet ist                                                          }
{-----------------------------------------------------------------------------}
function TTbLaksStammdaten.VerifyStationsId(anId: integer): boolean;
{-----------------------------------------------------------------------------}
var
  q      : TQueryExt;
begin
  q:= TQueryExt.Create(nil);
  try
    q.DatabaseName:= StammdatenPfad;
    q.Sql.Add('SELECT COUNT (' + C_LTF_LSta_StationsId + ')');
    q.Sql.Add('FROM "' + C_LTB_LSta + '"');
    q.Sql.Add('WHERE ' + C_LTF_LSta_StationsId + ' = ' + IntToStr(anId));
    if q.Open then begin
      if q.Fields[0].asInteger > 0 then result:= True else result:= False;
    end
    else result:= False;
  finally
    q.Free;
  end;
end;

{ Gibt einen Stammdatenrecord für die übergebene ID zurück                    }
{ Bei Mißerfolg wird die StationsId = -1 zurückgegeben                        }
{-----------------------------------------------------------------------------}
function TTbLaksStammdaten.GetLAKSStammdaten(anId: integer): TLaksStammdaten;
{-----------------------------------------------------------------------------}
var
  q      : TQueryExt;
begin
  q:= TQueryExt.Create(nil);
  try
    q.DatabaseName:= StammdatenPfad;
    q.Sql.Add('SELECT A.' + C_LTF_LSta_StationsId + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Stationsname + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Kennung + ',');
    q.Sql.Add('A.' + C_LTF_LSta_LaksTyp + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Aktiv + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Summierung + ',');
    q.Sql.Add('A.' + C_LTF_LSta_SStelleV24 + ',');
    q.Sql.Add('A.' + C_LTF_LSta_SStelleDFU + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Rufnummer + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Passwort + ',');
    q.Sql.Add('B.' + C_LTF_LTyp_LaksName);
    q.Sql.Add('FROM "' + C_LTB_LSta + '" A,');
    q.Sql.Add('"' + C_LTB_LTyp + '" B');
    q.Sql.Add('WHERE A.' + C_LTF_LSta_LaksTyp + ' = B.' + C_LTF_LTyp_LaksTyp);
    q.Sql.Add('AND A.' + C_LTF_LSta_StationsId + ' = ' + IntToStr(anId));
    if q.Open then begin
      result.StationsId:= q.FieldByName(C_LTF_LSta_StationsId).asInteger;
      result.Stationsname:= q.FieldByName(C_LTF_LSta_Stationsname).asString;
      result.Kennung:= q.FieldByName(C_LTF_LSta_Kennung).asString;
      result.LaksTyp:= q.FieldByName(C_LTF_LSta_LaksTyp).asInteger;
      result.LaksName:= q.FieldByName(C_LTF_LTyp_LaksName).asString;

      if (q.FieldByName(C_LTF_LSta_Aktiv).isNull)
      then result.Aktiv:= False
      else result.Aktiv:= q.FieldByName(C_LTF_LSta_Aktiv).asBoolean;

      if (q.FieldByName(C_LTF_LSta_Summierung).isNull)
      then result.Summierung:= False
      else result.Summierung:= q.FieldByName(C_LTF_LSta_Summierung).asBoolean;

      if (q.FieldByName(C_LTF_LSta_SStelleV24).isNull)
      then result.SStelle_V24:= -1
      else result.SStelle_V24:= q.FieldByName(C_LTF_LSta_SStelleV24).asInteger;

      if (q.FieldByName(C_LTF_LSta_SStelleDFU).isNull)
      then result.SStelle_DFU:= -1
      else result.SStelle_DFU:= q.FieldByName(C_LTF_LSta_SStelleDFU).asInteger;

      result.Rufnummer:= q.FieldByName(C_LTF_LSta_Rufnummer).asString;
      result.Passwort:= q.FieldByName(C_LTF_LSta_Passwort).asString;
    end
    else begin
      result.StationsId:= -1; { Heißt: Aufruf ist fehlgeschlagen }
      result.Stationsname:= '';
      result.Kennung:= '';
      result.LaksTyp:= -1;
      result.LaksName:= '';
      result.Aktiv:= False;
      result.Summierung:= False;
      result.SStelle_V24:= -1;
      result.SStelle_DFU:= -1;
      result.Rufnummer:= '';
      result.Passwort:= '';
    end;
  finally
    q.Free;
  end;
end;

{ Schreibt einen StammdatenEintrag für die übergebene ID                      }
{-----------------------------------------------------------------------------}
procedure TTbLaksStammdaten.SetLAKSStammdaten(anId: integer; LSta: TLaksStammdaten);
{-----------------------------------------------------------------------------}
var
  q         : TQueryExt;
  isUpdate  : boolean;
begin
  q:= TQueryExt.Create(nil);
  try
    q.DatabaseName:= StammdatenPfad;
    { Wenn die Id bereits vorhanden ist, wird upgedatet, sonst eingefügt }
    if VerifyStationsId(anId) then isUpdate:= True else isUpdate:= False;

    if isUpdate then begin
      q.Sql.Add('UPDATE "' + C_LTB_LSta + '"');
      q.Sql.Add('SET ' + C_LTF_LSta_Stationsname + '= :SName,');
      q.Sql.Add(C_LTF_LSta_Kennung + '= :Kennung,');
      q.Sql.Add(C_LTF_LSta_LaksTyp + '= :LTyp,');
      q.Sql.Add(C_LTF_LSta_Aktiv + '= :Aktiv,');
      q.Sql.Add(C_LTF_LSta_Summierung + '= :Sum,');
      q.Sql.Add(C_LTF_LSta_SStelleV24 + '= :V24,');
      q.Sql.Add(C_LTF_LSta_SStelleDFU + '= :DFU,');
      q.Sql.Add(C_LTF_LSta_Rufnummer + '= :RNr,');
      q.Sql.Add(C_LTF_LSta_Passwort + '= :PW1');
      q.Sql.Add('WHERE ' + C_LTF_LSta_StationsId + ' = ' + IntToStr(anId));
    end
    else begin
      q.Sql.Add('INSERT INTO "' + C_LTB_LSta + '"');
      q.Sql.Add('(' + C_LTF_LSta_StationsId + ',');
      q.Sql.Add(C_LTF_LSta_Stationsname + ',');
      q.Sql.Add(C_LTF_LSta_Kennung + ',');
      q.Sql.Add(C_LTF_LSta_LaksTyp + ',');
      q.Sql.Add(C_LTF_LSta_Aktiv + ',');
      q.Sql.Add(C_LTF_LSta_Summierung + ',');
      q.Sql.Add(C_LTF_LSta_SStelleV24 + ',');
      q.Sql.Add(C_LTF_LSta_SStelleDFU + ',');
      q.Sql.Add(C_LTF_LSta_Rufnummer + ',');
      q.Sql.Add(C_LTF_LSta_Passwort + ')');
      q.Sql.Add('VALUES');
      q.Sql.Add('(:SId, :SName, :Kennung, :LTyp, :Aktiv, :Sum, :V24, :DFU, :RNr, :PW1)');
      q.ParamByName('SId').asSmallInt:= LSta.StationsId;
    end;
    q.ParamByName('SName').asString:= LSta.Stationsname;
    q.ParamByName('Kennung').asString:= LSta.Kennung;
    q.ParamByName('LTyp').asSmallInt:= LSta.LaksTyp;
    q.ParamByName('Aktiv').asBoolean:= LSta.Aktiv;
    q.ParamByName('Sum').asBoolean:= LSta.Summierung;
    q.ParamByName('V24').asSmallInt:= LSta.SStelle_V24;
    q.ParamByName('DFU').asSmallInt:= LSta.SStelle_DFU;
    q.ParamByName('RNr').asString:= LSta.Rufnummer;
    q.ParamByName('PW1').asString:= LSta.Passwort;
    q.ExecSql;
  finally
    q.Free;
  end;
end;

{ Schreibt alle LAKS.Gerätetypen in die übergebene Stringliste                }
{-----------------------------------------------------------------------------}
procedure TTbLaksStammdaten.GetLaksGeraeteTypen(sl: TStringlist);
{-----------------------------------------------------------------------------}
var
  q         : TQueryExt;
  pi        : PInteger;
  i         : integer;
begin
  { Stringliste leeren }
  try
    for i:= 0 to sl.Count-1 do Dispose(PInteger(sl.Objects[i]));
  except
    // tue nix
  end;
  sl.Clear;

  q:= TQueryExt.Create(nil);
  try
    q.DatabaseName:= StammdatenPfad;
    q.Sql.Add('SELECT ' + C_LTF_LTyp_LaksName + ', ' + C_LTF_LTyp_LaksTyp);
    q.Sql.Add('FROM "' + C_LTB_LTyp + '"');
    if q.Open then begin
      q.First;
      while not q.Eof do begin
        new(pi);
        pi^:= q.FieldByName(C_LTF_LTyp_LaksTyp).asInteger;
        sl.AddObject(q.FieldByName(C_LTF_LTyp_LaksName).asString, TObject(pi));
        q.Next;
      end;
    end;
  finally
    q.Free;
  end;
end;

{ Gibt die nächste freie Id zurück                                            }
{-----------------------------------------------------------------------------}
function TTbLaksStammdaten.NewLaksId: integer;
{-----------------------------------------------------------------------------}
var
  q      : TQueryExt;
begin
  q:= TQueryExt.Create(nil);
  try
    q.DatabaseName:= StammdatenPfad;
    q.Sql.Add('SELECT MAX(' + C_LTF_LSta_StationsId + '),');
    q.Sql.Add('COUNT(' + C_LTF_LSta_StationsId + ')');
    q.Sql.Add('FROM "' + C_LTB_LSta + '"');
    if q.Open then begin
      if (q.Fields[1].asInteger > 0) then result:= q.Fields[0].asInteger + 1
      else result:= 1;
    end
    else result:= -1; { Fehlerfall }
  finally
    q.Free;
  end;
end;

{ Löscht einen Eintrag aus den Stammdaten                                     }
{ Parameter: Id des zu löschenden Eintrags                                    }
{-----------------------------------------------------------------------------}
procedure TTbLaksStammdaten.DeleteLaksStaEntry(anId: integer);
{-----------------------------------------------------------------------------}
var
  q      : TQueryExt;
begin
  q:= TQueryExt.Create(nil);
  try
    q.DatabaseName:= StammdatenPfad;
    q.Sql.Add('DELETE FROM "' + C_LTB_LSta + '"');
    q.Sql.Add('WHERE ' + C_LTF_LSta_StationsId + ' = ' + IntToStr(anId));
    q.ExecSql;
  finally
    q.Free;
  end;
end;

{ Löscht die zu der übergebenen Kennung gehörenden Archivdaten/-dateien       }
{ Parameter: Kennung                                                          }
{-----------------------------------------------------------------------------}
procedure TTbLaksStammdaten.DeleteLaksArchivDaten(aKennung: string);
{-----------------------------------------------------------------------------}
var
  anIndex    : integer;
  oldCursor  : TCursor;

  {--------------------------------}
  procedure DeleteLaksArchivTables;
  {--------------------------------}
  const
    C_Satz     = 'SATZ';
    C_Abwurf   = 'A';
    C_Band     = 'BAND';
    C_Wert     = 'W';
  var
    f          : TSearchRec;
  begin
  {  Satztabellen löschen }
    if (FindFirst(ArchivdatenPfad + C_Satz + Format('%.4d', [anIndex]) + '.*',
                  faAnyFile, f) = 0)
    then begin
      DeleteFile(ArchivdatenPfad + f.Name);
      while (FindNext(f) = 0) do DeleteFile(ArchivdatenPfad + f.Name);
    end;
    FindClose(f);
  {  Werttabellen löschen }
    if (FindFirst(ArchivdatenPfad + C_Wert + Format('???%.4d', [anIndex]) + '.*',
                  faAnyFile, f) = 0)
    then begin
      DeleteFile(ArchivdatenPfad + f.Name);
      while (FindNext(f) = 0) do DeleteFile(ArchivdatenPfad + f.Name);
    end;
    FindClose(f);
  {  Abwurftabellen löschen }
    if (FindFirst(ArchivdatenPfad + C_Abwurf + Format('%.4d', [anIndex]) + '.*',
                  faAnyFile, f) = 0)
    then begin
      DeleteFile(ArchivdatenPfad + f.Name);
      while (FindNext(f) = 0) do DeleteFile(ArchivdatenPfad + f.Name);
    end;
    FindClose(f);
  {  Bandtabellen löschen }
    if (FindFirst(ArchivdatenPfad + C_Band + Format('%.4d', [anIndex]) + '.*',
                  faAnyFile, f) = 0)
    then begin
      DeleteFile(ArchivdatenPfad + f.Name);
      while (FindNext(f) = 0) do DeleteFile(ArchivdatenPfad + f.Name);
    end;
    FindClose(f);
  end;

begin
  oldCursor:= Screen.Cursor;
  Screen.Cursor:= crHourglass;
  try
    anIndex:=Get_LAKSDatenIndex (ArchivdatenPfad, aKennung, false);
    if anIndex > 0 then
      DeleteLaksArchivTables;  { Tabellen löschen }
  finally
    Screen.Cursor:= oldCursor;
  end;
end;

{ Gibt eine Liste aller Stationen, die in die Summierung eingehen, zurück     }
{ Parameter: Übergabestringliste                                              }
{-----------------------------------------------------------------------------}
procedure TTbLaksStammdaten.GetLaksSummierungsListe(var sl: TStringList);
{-----------------------------------------------------------------------------}
var
  q      : TQueryExt;
  p      : PLaksStammdaten;
begin
  ClearLaksRecordList(sl);

  q:= TQueryExt.Create(nil);
  try
    q.DatabaseName:= StammdatenPfad;
    q.Sql.Add('SELECT A.' + C_LTF_LSta_StationsId + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Stationsname + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Kennung + ',');
    q.Sql.Add('A.' + C_LTF_LSta_LaksTyp + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Aktiv + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Summierung + ',');
    q.Sql.Add('A.' + C_LTF_LSta_SStelleV24 + ',');
    q.Sql.Add('A.' + C_LTF_LSta_SStelleDFU + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Rufnummer + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Passwort + ',');
    q.Sql.Add('B.' + C_LTF_LTyp_LaksName);
    q.Sql.Add('FROM "' + C_LTB_LSta + '" A,');
    q.Sql.Add('"' + C_LTB_LTyp + '" B');
    q.Sql.Add('WHERE A.' + C_LTF_LSta_Summierung + ' = TRUE');
    q.Sql.Add('AND A.' + C_LTF_LSta_Aktiv + ' = TRUE');
    q.Sql.Add('AND A.' + C_LTF_LSta_LaksTyp + ' = ' + 'B.' + C_LTF_LTyp_LaksTyp);
    if q.Open then begin
      q.First;
      while not q.Eof do begin
        New(p);
        p^.StationsId:= q.FieldByName(C_LTF_LSta_StationsId).asInteger;
        p^.Stationsname:= q.FieldByName(C_LTF_LSta_Stationsname).asString;
        p^.Kennung:= q.FieldByName(C_LTF_LSta_Kennung).asString;
        p^.LaksTyp:= q.FieldByName(C_LTF_LSta_LaksTyp).asInteger;
        p^.LaksName:= q.FieldByName(C_LTF_LTyp_LaksName).asString;
        p^.Aktiv:= q.FieldByName(C_LTF_LSta_Aktiv).asBoolean;
        p^.Summierung:= q.FieldByName(C_LTF_LSta_Summierung).asBoolean;
        p^.SStelle_V24:= q.FieldByName(C_LTF_LSta_SStelleV24).asInteger;
        p^.SStelle_DFU:= q.FieldByName(C_LTF_LSta_SStelleDFU).asInteger;
        p^.Rufnummer:= q.FieldByName(C_LTF_LSta_Rufnummer).asString;
        p^.Passwort:= q.FieldByName(C_LTF_LSta_Passwort).asString;
        sl.AddObject(p^.Stationsname, TObject(p));
        q.Next;
      end;
    end;
  finally
    q.Free;
  end;
end;
                                                             
{ Übergibt eine Liste aller Stationen, über serielle SStelle connected sind   }
{ Parameter: Übergabestringliste                                              }
{-----------------------------------------------------------------------------}
procedure TTbLaksStammdaten.GetLaksSerielleListe(var sl: TStringList);
{-----------------------------------------------------------------------------}
var
  q      : TQueryExt;
  p      : PLaksStammdaten;
begin
  ClearLaksRecordList(sl);

  q:= TQueryExt.Create(nil);
  try
    q.DatabaseName:= StammdatenPfad;
    q.Sql.Add('SELECT A.' + C_LTF_LSta_StationsId + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Stationsname + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Kennung + ',');
    q.Sql.Add('A.' + C_LTF_LSta_LaksTyp + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Aktiv + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Summierung + ',');
    q.Sql.Add('A.' + C_LTF_LSta_SStelleV24 + ',');
    q.Sql.Add('A.' + C_LTF_LSta_SStelleDFU + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Rufnummer + ',');
    q.Sql.Add('A.' + C_LTF_LSta_Passwort + ',');
    q.Sql.Add('B.' + C_LTF_LTyp_LaksName);
    q.Sql.Add('FROM "' + C_LTB_LSta + '" A,');
    q.Sql.Add('"' + C_LTB_LTyp + '" B');
    q.Sql.Add('WHERE A.' + C_LTF_LSta_SStelleV24 + ' > -1');
    q.Sql.Add('AND A.' + C_LTF_LSta_Aktiv + ' = TRUE');
    q.Sql.Add('AND A.' + C_LTF_LSta_LaksTyp + ' = ' + 'B.' + C_LTF_LTyp_LaksTyp);
    if q.Open then begin
      q.First;
      while not q.Eof do begin
        New(p);
        p^.StationsId:= q.FieldByName(C_LTF_LSta_StationsId).asInteger;
        p^.Stationsname:= q.FieldByName(C_LTF_LSta_Stationsname).asString;
        p^.Kennung:= q.FieldByName(C_LTF_LSta_Kennung).asString;
        p^.LaksTyp:= q.FieldByName(C_LTF_LSta_LaksTyp).asInteger;
        p^.LaksName:= q.FieldByName(C_LTF_LTyp_LaksName).asString;
        p^.Aktiv:= q.FieldByName(C_LTF_LSta_Aktiv).asBoolean;
        p^.Summierung:= q.FieldByName(C_LTF_LSta_Summierung).asBoolean;
        p^.SStelle_V24:= q.FieldByName(C_LTF_LSta_SStelleV24).asInteger;
        p^.SStelle_DFU:= q.FieldByName(C_LTF_LSta_SStelleDFU).asInteger;
        p^.Rufnummer:= q.FieldByName(C_LTF_LSta_Rufnummer).asString;
        p^.Passwort:= q.FieldByName(C_LTF_LSta_Passwort).asString;
        sl.AddObject(p^.Stationsname, TObject(p));
        q.Next;
      end;
    end;
  finally
    q.Free;
  end;
end;

{Prüft ab, ob eine übergeben Kennung in den Stammdaten existiert              }
{ Parameter: zu überprüfende Kennung                                          }
{ Rückgabe: TRUE _ Kennung existiert bereits                                  }
{-----------------------------------------------------------------------------}
function TTbLaksStammdaten.KennungExists(anId: integer; aKennung: string): boolean;
{-----------------------------------------------------------------------------}
var
  q      : TQueryExt;
begin
  result:= false; { default }
  q:= TQueryExt.Create(nil);
  try
    q.DatabaseName:= StammdatenPfad;
    q.Sql.Add('SELECT ' + C_LTF_LSta_StationsId + ' FROM "' + C_LTB_LSta + '"');
    q.Sql.Add('WHERE ' + C_LTF_LSta_Kennung + ' = "' + aKennung + '"');
    q.Sql.Add('AND ' + C_LTF_LSta_StationsId + ' <> ' + IntToStr(anId));
    if q.Open then begin
      if q.RecordCount > 0 then result:= True;
    end;
  finally
    q.Free;
  end;
end;

end.

