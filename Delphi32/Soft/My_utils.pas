{------------------------------------------------------------------------------}
{ 30.11.1998 GD; Unit für den MRG-Abruf                                        }
{                Funktionen, Konstanten und Typen                              }
{                                                                              }
{        -> Benötigt Pathserver[WStammDir]                                     }
{                                                                              }
{ 14.04.1999 GD; Stammdatenliste wird jetzt anders aufgebaut                   }
{ 11.05.1999 GD; Offsetfunktion für Sommerzeit-/Winterzeitumstellung           }
{ 01.06.1999 GD; Stammdatenliste für LAKS und DSfG erweitert (Performance ?)   }
{ 16.07.1999 GD; StrToFloatDef-Funktion                                        }
{ 16.06.2000 WW; Stationslizenzierung-Funktion                                 }
{ 13.07.2000 GD; 'GetWZ_SZ_OffsetNow' korrigiert                               }
{ 30.08.2000 GD; 'GetWZ_SZ_OffsetNow' noch einmal korrigiert                   }
{ 06.03.2003 WW; Tabellen-unabh. Algorithmus von 'GetWZ_SZ_OffsetNowFromDB'    }
{                nach 'GetWZ_SZ_Offset' ausgelagert (T_Zeit.pas)               }
{                                                                              }
{  (C) Karl Wieser GmbH 1998, 1999                                             }
{------------------------------------------------------------------------------}
unit my_Utils;

interface

uses
  Classes, SysUtils, PathIni, DbTables, Graphics, Dialogs, WSysCon, DbiProcs,
  Controls, Forms, WTables, Windows, Lizenz32, T_Zeit;

const
  { Flags für Stationslistenauswahl }
  C_AuswahlMrg    = $01;
  C_AuswahlDSfG   = $02;
  C_AuswahlLAKS   = $04;
  C_AuswahlGrp    = $08;
  C_AuswahlAlle   = $0F;

type

  PIdRec = ^TIdRec;
  TIdRec = class(TObject)
    Id:         integer;
    Typ:        string[8];
    Name:       string[40];
    Kennung:    string[20];
    LogPort:    SmallInt;
    Prioritaet: SmallInt;
    constructor create(anId: integer; aTyp, aName, aKennung: string;
                       aLogPort, aPrioritaet: SmallInt);
  end;

procedure CheckMrgGrpTable(Pfad: string);
procedure CheckGrpTable(Pfad: string);
function DateToStrYYYY(Value: TDateTime): string;
function StrToDateYYYY(Value: string): TDateTime;
function GetTimeDiffInSec(Time1, Time2: TDateTime): integer;
function GetTimeDiffStr(Time1, Time2: TDateTime): string;
function GetStationsname(anId: integer; Typ: string): string;
function GetStationsnameListe(var aStringList: TStringList;
                              Auswahl: byte = C_AuswahlMrg+C_AuswahlGrp): boolean;
function CheckMRGKennung (const Kennung: string; const MrgId: integer): boolean;
function GetKennung(anId: integer; Typ: string): string;
function GetMrgIdFromName(StationsName: string): integer;
function GetNextSZ_To_WZFromDB: TDateTime;
function GetWZ_SZ_OffsetNowFromDB(anId: integer; aTyp: string): TDateTime;
function IntToBin(anInt: integer; Digits: byte): string;
function GetPathFileNameWithoutExt(aName: string): string;
//function RestructDaten(aDatabaseName: string; aTableName: string;
//                       ErrMsgOnOff: boolean): boolean;
function WStrToFloatDef(aString: string; aFloat: double): double;
function WStationslizenz32_MRG (StaPfad: string): boolean;
function WStationslizenz32_DSfG (StaPfad: string;
                                 Wieser_MRG910_frei: boolean;
                                 Wieser_ohne_MRG910_frei: boolean;
                                 MsgDlgOut: boolean): integer;

implementation

{----------------------------------- TIdRec -----------------------------------}

{----------------------------------------------------}
constructor TIdRec.create(anId: integer; aTyp, aName, aKennung: string;
                          aLogPort, aPrioritaet: SmallInt);
{----------------------------------------------------}
begin
  inherited create;
  Id:= anId;
  Typ:= aTyp;
  Name:= aName;
  Kennung:= aKennung;
  LogPort:= aLogPort;
  Prioritaet:= aPrioritaet
end;

{---------------- Tabellen-Funktionen -------------------------------}

{ Erstellen der GRP.DB, falls nicht vorhanden }
{----------------------------------------------------}
procedure CheckGrpTable(Pfad: string);
{----------------------------------------------------}
var
  q          : TQuery;
  t          : TTable;
  Feldzahl   : integer;

begin
  if not fileExists(Pfad + C_TbGrpStamm) then begin
    q:= TQuery.Create(nil);
    try
      q.DatabaseName:= Pfad;
      q.Sql.Add('CREATE TABLE "' + C_TbGrpStamm + '"');
      q.Sql.Add('(' + C_TfGrpId + ' SMALLINT,');
      q.Sql.Add(C_TfGrpName + ' CHAR(20),');
      q.Sql.Add(C_TfGrpPrioritaet + ' SMALLINT,');
      q.Sql.Add(C_TfGrpOffset + ' SMALLINT,');
      q.Sql.Add('PRIMARY KEY (' + C_TfGrpId + '))');
      q.ExecSql;
    except
      on exception do begin
        q.Free;
        exit;
      end;
    end;
    q.Free;
  end else begin // Tabelle ist da ! evtl Feld Offset anhängen !
    t:=TTable.Create (nil);
    t.DatabaseName:=Pfad;
    t.TableName:=C_TbGrpStamm;
    t.open;
    Feldzahl:=T.fieldcount;
    t.close;
    t.free;
    q:= TQuery.Create(nil);
    if (Feldzahl = 3) then begin  // alte Struktur
       try
         q.DatabaseName:= Pfad;
         q.Sql.clear;
         q.Sql.Add('ALTER TABLE grp');
         q.Sql.Add('ADD GruppenOffset SMALLINT');
         q.execSql;

         q.Sql.Clear;
         q.Sql.Add('Update grp');
         q.Sql.Add('SET GruppenOffset = 0');
         q.ExecSql;

       except
         on exception do begin
           q.Free;
           exit;
         end;
       end;
    end;
    q.free;
  end;
end;

{ Erstellen der MRGGRP.DB, falls nicht vorhanden }
{----------------------------------------------------}
procedure CheckMrgGrpTable(Pfad: string);
{----------------------------------------------------}
var
  q          : TQuery;
begin
  if not fileExists(Pfad + C_TbMrgGrp) then begin
    q:= TQuery.Create(nil);
    try
      q.DatabaseName:= Pfad;
      q.Sql.Add('CREATE TABLE "' + C_TbMrgGrp + '"');
      q.Sql.Add('(' + C_TFGrpId + ' SMALLINT,');
      q.Sql.Add(C_TFMrgId + ' SMALLINT,');
      q.Sql.Add('PRIMARY KEY (' + C_TFGrpId + ',' + C_TFMrgId + '))');
      q.ExecSql;
    except
      on exception do begin
        q.Free;
        if IsDebugFlag then showMessage('My_Utils - CheckMrg...');
        exit;
      end;
    end;
    q.Free;
  end;
end;


{---------------- Allgemeine Funktionen ---------------------------------------}

{ Gibt Datum mit 4-stelliger Jahreszahl zurück       }
{ Separator ist '.' - besser: FormatDateTime         }
{ Parameter: Datum als TDateTime                     }
{ Rückgabewert: Datum als string                     }
{----------------------------------------------------}
function DateToStrYYYY(Value: TDateTime): string;
{----------------------------------------------------}
var
  t, m, j: word;
begin
  DecodeDate(Value, j, m, t);
  result:= format('%.2d', [t]) + '.' + format('%.2d', [m])
           + '.' + format('%.4d', [j]);
end;

{ Gibt TDateTime bei 4-stelliger Jahreszahl zurück   }
{  - besser: FormatDateTime                          }
{ Parameter: Datum als string - Separator ist '.'    }
{ Rückgabewert: Datum als TDateTime                  }
{----------------------------------------------------}
function StrToDateYYYY(Value: string): TDateTime;
{----------------------------------------------------}
var
  i: integer;
  t, m, j: word;
  s: string;
begin
  s:= Value;
  i:= pos('.', s);
  t:= strToIntDef(copy(s, i-2, 2), 0);
  delete(s, 1, i);
  i:= pos('.', s);
  m:= strToIntDef(copy(s, 1, i-1), 0);
  delete(s, 1, i);
  j:= strToIntDef(s, 0);
  if (t > 0) and (m > 0) and (j > 0) then result:= EncodeDate(j, m, t)
    else result:= 0;
end;

{ Holt die Stammdaten aller Stationen ein            }
{ Für jeden Gerätetyp muß die function um einen      }
{ Query erweitert werden                             }
{ Parameter: Übergabestringliste,                    }
{            Flag für einzulesende Stationen         }
{ Rückgabe: Erfolg: True/False                       }
{----------------------------------------------------}
function GetStationsnameListe(var aStringList: TStringList;
                              Auswahl: byte = C_AuswahlMrg+C_AuswahlGrp): boolean;
{----------------------------------------------------}
var
  q          : TQueryExt;
  rec        : TIdRec;
  i          : integer;
  lp, pr     : integer;
begin
  { Stringliste leeren, Objekte freigeben }
  if aStringList.Count > 0 then begin
    try
      for i:= 0 to aStringList.Count-1 do
        TIdRec(aStringList.Objects[i]).free;
    except
      // tue nix
      if IsDebugFlag then showMessage('My_Utils - StatNameListe1');
    end;
    aStringList.Clear;
  end;


  result:= False; { default }
  q:= TQueryExt.create(nil);
  try
  { Einlesen aller MRG-Stationen }
    try
    if ((Auswahl and C_AuswahlMrg) > 0) then
    if FileExists(Pathserver.Pathname[WStammDir] + CDBSta) then begin
      q.DatabaseName:= Pathserver.Pathname[WStammDir];
      q.Sql.Add('SELECT A.' + C_Sta_StationsName + ', A.' + C_Sta_MrgId + ',');
      q.Sql.Add('A.' + C_Sta_Prioritaet + ', A.' + C_Sta_Kennung + ',');
      q.Sql.Add('B.' + C_StaDfu_LogPort);
      q.Sql.Add('FROM "' + CDBSta + '" A');
      q.Sql.Add('LEFT JOIN "' + CDBStaDfu + '" B');
      q.Sql.Add('ON A.' + C_Sta_MrgId + '= B.' + C_StaDfu_MrgId);
      q.Sql.Add('WHERE A.' + C_Sta_Aktiv + '= :Aktiv');
      q.Sql.Add('ORDER BY ' + C_Sta_StationsName);
      q.ParamByName('Aktiv').asInteger:= 0;
      q.open;
      q.first;
      while not q.Eof do begin
        if q.FieldByName(C_StaDfu_LogPort).IsNull then lp:= 0
          else lp:= q.FieldByName(C_StaDfu_LogPort).asInteger;
        if q.FieldByName(C_Sta_Prioritaet).IsNull then pr:= 1
          else pr:= q.FieldByName(C_Sta_Prioritaet).asInteger;
        rec:= TIdRec.Create(q.FieldByName(C_Sta_MrgId).asInteger, C_GerArtMrg,
                            q.FieldByName(C_Sta_StationsName).asString,
                            q.FieldByName(C_Sta_Kennung).asString, lp, pr);
        aStringList.AddObject(C_GerArtMrg + IntToStr(rec.Id), rec);
        q.Next;
      end;
    end;
    except
    end;

  { Einlesen aller DSfG-Stationen }
    try
    if ((Auswahl and C_AuswahlDSfG) > 0) then
    if FileExists(Pathserver.Pathname[WStammDir] + C_DTB_Station) then begin
      q.close;
      q.DatabaseName:= Pathserver.Pathname[WStammDir];
      q.Sql.Clear;
      q.sql.Add('SELECT A.' + C_DTF_Station_Stationsname + ',');
      q.Sql.Add('A.' + C_DTF_Station_StationId + ',');
      q.Sql.Add('A.' + C_DTF_Station_Prioritaet + ',');
      q.Sql.Add('C.' + C_DTF_InstDfu_Kennung + ',');
      q.Sql.Add('C.' + C_DTF_InstDfu_LogPort);
      q.sql.Add('FROM "' + C_DTB_Station + '" A');
      q.sql.Add('LEFT JOIN "' + C_DTB_Instanz + '" B');
      q.sql.Add('ON A.' + C_DTF_Station_StationId + ' = B.' + C_DTF_Instanz_StationId);
      q.sql.Add('LEFT JOIN "' + C_DTB_InstDfu + '" C');
      q.Sql.Add('ON C.'+ C_DTF_InstDfu_InstanzId + ' = B.' + C_DTF_Instanz_InstanzId);
      q.Sql.Add('WHERE A."'+ C_DTF_Station_LIInstanz + '" = B.' + C_DTF_Instanz_Instanzname);
      q.Sql.Add('ORDER BY ' + C_DTF_Station_Stationsname);
// geändert 10.05.2001 WW: LEFT JOIN auf Instanz.db und Inst_dfu.db, Verknüpfung über Login-Instanzname
      if q.open then begin
        q.first;
        while not q.Eof do begin
          if q.FieldByName(C_DTF_InstDfu_LogPort).IsNull then
            lp:= 0
          else
            lp:= q.FieldByName(C_DTF_InstDfu_LogPort).asInteger;
          if q.FieldByName(C_DTF_Station_Prioritaet).IsNull then
            pr:= 1
          else
            pr:= q.FieldByName(C_DTF_Station_Prioritaet).asInteger;
          rec:= TIdRec.Create(q.FieldByName(C_DTF_Station_StationId).asInteger, C_GerArtDSfG,
                              q.FieldByName(C_DTF_Station_Stationsname).asString,
                              q.FieldByName(C_DTF_InstDfu_Kennung).asString, lp, pr);
          aStringList.AddObject(C_GerArtDSfG + IntToStr(rec.Id), rec);
          q.Next;
        end;
        result:= true; { default }
      end
      else result:= False;
    end;
    except
    end;

  { Einlesen aller LAKS-Stationen }
    try
    if ((Auswahl and C_AuswahlLAKS) > 0) then
    if FileExists(Pathserver.Pathname[WStammDir] + C_LTB_LSta) then begin
      q.close;
      q.DatabaseName:= Pathserver.Pathname[WStammDir];
      q.Sql.Clear;
      q.sql.Add('SELECT ' + C_LTF_LSta_Stationsname + ',');
      q.Sql.Add(C_LTF_LSta_StationsId + ',');
      q.Sql.Add(C_LTF_LSta_Kennung);
      q.sql.Add('FROM "' + C_LTB_LSta + '"');
      q.Sql.Add('ORDER BY ' + C_LTF_LSta_Stationsname);
      if q.open then begin
        q.first;
        while not q.Eof do begin
          rec:= TIdRec.Create(q.FieldByName(C_LTF_LSta_StationsId).asInteger,
                              C_GerArtLAKS,
                              q.FieldByName(C_LTF_LSta_Stationsname).asString,
                              q.FieldByName(C_LTF_LSta_Kennung).asString,
                              0, 1);
          aStringList.AddObject(C_GerArtLaks + IntToStr(rec.Id), rec);
          q.Next;
        end;
        result:= true; { default }
      end
      else result:= False;
    end;
    except
    end;

  { Einlesen aller Gruppen }
    try
    if ((Auswahl and C_AuswahlGrp) > 0) then
    if FileExists(Pathserver.Pathname[WStammDir] + C_TbGrpStamm) then begin
      q.close;
      q.DatabaseName:= Pathserver.Pathname[WStammDir];
      q.Sql.Clear;
      q.sql.add('SELECT ' + C_TfGrpName + ',' + C_TfGrpId + ',' + C_TfGrpPrioritaet);
      q.sql.add('FROM "' + C_TbGrpStamm + '"');
      q.Sql.Add('ORDER BY ' + C_TfGrpName);
      q.open;
      q.first;
      while not q.Eof do begin
        rec:= TIdRec.Create(q.FieldByName(C_TfGrpId).asInteger, C_GerArtGruppe,
                            q.FieldByName(C_TfGrpName).asString, '', 0,
                            q.FieldByName(C_TfGrpPrioritaet).asInteger);
        aStringList.AddObject(C_GerArtGruppe + IntToStr(rec.Id), rec);
        q.Next;
      end;
    end;
    except
    end;

    result:= true; { default }
  except
    q.Free;
    if IsDebugFlag then
      showMessage('My_Utils - StatNameListe2; Pfad: ' + Pathserver.Pathname[WStammDir]);
    exit;
  end;
  q.Free;
end;

{----------------------------------------------------}
function GetStationsname(anId: integer; Typ: string): string;
{----------------------------------------------------}
var
  q: TQuery;
begin
  result:= ''; { default }
  q:= TQuery.create(nil);
  try
    if Pos(C_GerArtMrg, Typ) > 0
    then begin
  { Typ ist MRG }
      q.DatabaseName:= Pathserver.Pathname[WStammDir];
      q.sql.add('SELECT ' + C_Sta_StationsName + ',' + C_Sta_MrgId + ',' + C_Sta_Aktiv);
      q.sql.add('FROM ' + CDBSta);
      q.sql.add('WHERE ' + C_Sta_MrgId + '= :MRGId');
      q.sql.add('AND ' + C_Sta_Aktiv + '= :Aktiv');
      q.ParamByName('MRGId').asInteger:= anId;
      q.ParamByName('Aktiv').asInteger:= 0;
      q.open;
      if q.RecordCount = 1 then result:= q.FieldByName(C_Sta_StationsName).asString;
    end
    else if Pos(C_GerArtGruppe, Typ) > 0
    then begin
  { Typ ist GRUPPE }
      q.DatabaseName:= Pathserver.Pathname[WStammDir];
      q.sql.add('SELECT ' + C_TfGrpName + ',' + C_TfGrpId);
      q.sql.add('FROM ' + C_TbGrpStamm);
      q.sql.add('WHERE ' + C_TfGrpId + '= :GrpId');
      q.ParamByName('GrpId').asInteger:= anId;
      q.open;
      if q.RecordCount = 1 then result:= q.FieldByName(C_TfGrpName).asString;
    end
    else if Pos(C_GerArtDSfG, Typ) > 0
    then begin
  { Typ ist DSfG }
      q.DatabaseName:= Pathserver.Pathname[WStammDir];
      q.sql.add('SELECT ' + C_DTF_Station_Stationsname);
      q.sql.add('FROM "' + C_DTB_Station + '"');
      q.sql.add('WHERE ' + C_DTF_Station_StationId + '= :Id');
      q.ParamByName('Id').asInteger:= anId;
      q.open;
      if q.RecordCount = 1 then result:= q.FieldByName(C_DTF_Station_Stationsname).asString;
    end
    else if Pos(C_GerArtLaks, Typ) > 0
    then begin
  { Typ ist LAKS }
      q.DatabaseName:= Pathserver.Pathname[WStammDir];
      q.sql.add('SELECT ' + C_LTF_LSta_Stationsname);
      q.sql.add('FROM "' + C_LTB_LSta + '"');
      q.sql.add('WHERE ' + C_LTF_LSta_StationsId + '= :Id');
      q.ParamByName('Id').asInteger:= anId;
      q.open;
      if q.RecordCount = 1 then result:= q.FieldByName(C_LTF_LSta_Stationsname).asString;
    end;
  except
    q.Free;
    if IsDebugFlag then showMessage('My_Utils - GetStatName');
    exit;
  end;
  q.Free;
end;

{------------------------------------------------------------------------------}
function CheckMRGKennung (const Kennung: string; const MrgId: integer): boolean;
{------------------------------------------------------------------------------}
{ prüft, ob Kennung in MRG-Stammdaten bereits vorhanden ist und nicht zu MrgId gehört }
var
  q: TQuery;
begin
  q:= TQuery.create(nil);
  try
    with q do begin
      DatabaseName:= Pathserver.Pathname[WStammDir];
      SQL.Clear;
      SQL.Add ('SELECT ' + C_Sta_Kennung);
      SQL.Add ('FROM ' + CDBSta);
      SQL.Add ('WHERE ' + C_Sta_Kennung + ' = :Kennung');
      SQL.Add ('AND ' + C_Sta_MrgId + ' <> :MrgId');
      SQL.Add ('AND ' + C_Sta_Aktiv + ' = 0');
      ParamByName ('Kennung').asString := Kennung;
      ParamByName ('MrgId').asInteger := MrgId;
      Open;
      Result:=RecordCount > 0;
    end;
  finally
    q.free;
  end;
end;

{----------------------------------------------------}
function GetKennung(anId: integer; Typ: string): string;
{----------------------------------------------------}
var
  q: TQuery;
begin
  result:= ''; { default }
  q:= TQuery.create(nil);
  try
    if Pos(C_GerArtMrg, Typ) > 0 then begin
  { Typ ist MRG }
      q.DatabaseName:= Pathserver.Pathname[WStammDir];
      q.sql.add('SELECT ' + C_Sta_Kennung);
      q.sql.add('FROM ' + CDBSta);
      q.sql.add('WHERE ' + C_Sta_MrgId + '= :MRGId');
      q.sql.add('AND ' + C_Sta_Aktiv + '= :Aktiv');
      q.ParamByName('MRGId').asInteger:= anId;
      q.ParamByName('Aktiv').asInteger:= 0;
      q.open;
      if q.RecordCount = 1 then result:= q.FieldByName(C_Sta_Kennung).asString;
    end
    else if Pos(C_GerArtDSfG, Typ) > 0 then begin
  { Typ ist DSfG }
      q.DatabaseName:= Pathserver.Pathname[WStammDir];
      q.sql.Add('SELECT C.' + C_DTF_InstDfu_Kennung);
      q.sql.Add('FROM "' + C_DTB_Station + '" A,');
      q.sql.Add('     "' + C_DTB_Instanz + '" B,');
      q.sql.Add('     "' + C_DTB_InstDfu + '" C');
      q.Sql.Add('WHERE ((A."'+ C_DTF_Station_StationId + '" = :StationId) AND');
      q.Sql.Add('       (B."'+ C_DTF_Instanz_StationId + '" = :StationId) AND');
      q.Sql.Add('       (A."'+ C_DTF_Station_LIInstanz + '" = B.' + C_DTF_Instanz_Instanzname + ') AND');
      q.Sql.Add('       (B."'+ C_DTF_Instanz_InstanzId + '" = C.' + C_DTF_InstDfu_InstanzId + '))');
      q.ParamByName('StationId').asInteger:= anId;
      q.open;
      if q.RecordCount >= 1 then result:= q.FieldByName(C_DTF_InstDfu_Kennung).asString;
    end;  
  except
    q.Free;
    if IsDebugFlag then showMessage('My_Utils - GetKennung');
    exit;
  end;
  q.free;
end;

{----------------------------------------------------}
function GetMrgIdFromName(StationsName: string): integer;
{----------------------------------------------------}
var
  q: TQuery;
begin
  result:= -1; { default }
  q:= TQuery.create(nil);
  try
    q.DatabaseName:= Pathserver.Pathname[WStammDir];
    q.sql.add('SELECT ' + C_Sta_MrgId);
    q.sql.add('FROM ' + CDBSta);
    q.sql.add('WHERE ' +  C_Sta_StationsName + '= :Name');
    q.sql.add('AND ' + C_Sta_Aktiv + '= :Aktiv');
    q.ParamByName('Name').asString:= StationsName;
    q.ParamByName('Aktiv').asInteger:= 0;
    q.open;
    if q.RecordCount = 1 then result:= q.FieldByName(C_Sta_MrgId).asInteger;
  except
    q.Free;
    if IsDebugFlag then showMessage('My_Utils - GetMrgIdFrom...');
    exit;
  end;
  q.Free;
end;

{ Gibt für ein Gerät an, ob eine Winter-/Sommerzeit-  }
{ Umstellung erfolgt                                  }
{ Parameter: Id und Typ des Gerätes                   }
{ Rückgabewert: Ja oder nein                          }
{-----------------------------------------------------}
function GetWZSZ_FlagFromStaDB(anId: integer; aTyp: string): boolean;
{-----------------------------------------------------}
var
  q       : TQueryExt;
begin
  result:= False;         { default }
  if (aTyp = C_GerArtMrg) then begin
  { Es handelt sich um ein MRG }
    q:= TQueryExt.Create(nil);
    try
      q.DatabaseName:= Pathserver.Pathname[WStammDir];
      q.Sql.Add('SELECT ' + C_Sta_IsWZSZ);
      q.Sql.Add('FROM "' + CDBSta + '"');
      q.Sql.Add('WHERE ' + C_Sta_MrgId + ' = ' + IntToStr(anId));
      q.Sql.Add('AND ' + C_Sta_Aktiv + ' = 0');
      if q.Open then begin
        if q.RecordCount = 1
          then result:= q.FieldByName(C_Sta_IsWZSZ).asBoolean;
      end;
      q.Close;
    finally
      q.Free;
    end;
  end;
end;

{ Gibt nächste Umstellung SZ auf WZ an (Tabelle)        }
{ Rückgabewert: 0 - Default (keine Information aus Tabelle erhältlich)
                sonst Umstellungszeitpunkt als DateTime }
{-----------------------------------------------------}
function GetNextSZ_To_WZFromDB: TDateTime;
{-----------------------------------------------------}
var
  q       : TQueryExt;
begin
  result:= 0;         { default }
  q:= TQueryExt.Create(nil);
  try
    q.DatabaseName:= Pathserver.Pathname[WStammDir];
    q.Sql.Add('SELECT ' + C_TfWZSZ_Termin);
    q.Sql.Add('FROM "' + C_TbWZSZ + '"');
    q.Sql.Add('WHERE ' + C_TfWZSZ_Termin + ' > :DatumZeit');
    q.Sql.Add('AND ' + C_TfWZSZ_WZSZ + ' = 0');
    q.ParamByName('DatumZeit').asDateTime:= now;
    if q.Open then begin
      if q.RecordCount > 0 then
        result:= q.FieldByName(C_TfWZSZ_Termin).asDateTime;
    end;
    q.Close;
  finally
    q.Free;
  end;
end;

{ Gibt an, ob die Zeitzone WZ oder SZ ist (Tabelle)   }
{ Rückgabewert: 0 - WZ; 1 - SZ                        }
{-----------------------------------------------------}
function GetWZ_SZ_NowFromDB: SmallInt;
{-----------------------------------------------------}
var
  q       : TQueryExt;
begin
  result:= 0;         { default }
  { Es handelt sich um ein MRG }
  q:= TQueryExt.Create(nil);
  try
    q.DatabaseName:= Pathserver.Pathname[WStammDir];
    q.Sql.Add('SELECT ' + C_TfWZSZ_WZSZ + ',' + C_TfWZSZ_Termin); // 13.07.2000
    q.Sql.Add('FROM "' + C_TbWZSZ + '"');
    q.Sql.Add('WHERE ' + C_TfWZSZ_Termin + ' <= :DatumZeit');
    q.Sql.Add('ORDER BY ' + C_TfWZSZ_Termin);                     // 13.07.2000
    q.ParamByName('DatumZeit').asDateTime:= now;
    if q.Open then if q.RecordCount > 0 then begin
      q.Last;
      result:= q.FieldByName(C_TfWZSZ_WZSZ).asInteger;
    end;
    q.Close;
  finally
    q.Free;
  end;
end;

{ Gibt für 'now' für ein Gerät an, mit welchem        }
{ Winter-/Sommerzeit-Offset PC- und MRG-Zeit ver-     }
{ rechnet werden müssen                               }
{ Parameter: Id und Typ des Gerätes                   }
{ Rückgabewert: Zeitoffset (0 oder 1 Stunde)          }
{-----------------------------------------------------}
function GetWZ_SZ_OffsetNowFromDB(anId: integer; aTyp: string): TDateTime;
{-----------------------------------------------------}
var
  i: byte;
  b: boolean;
begin
  { Feststellen, ob die aktuelle Zeitzone in WZ oder SZ ist (Tabelle) }
  i:= GetWZ_SZ_NowFromDB;
  { Feststellen, ob  das Gerät sich umstellt (Stammdaten-Tabelle) }
  b:= GetWZSZ_FlagFromStaDB(anId, aTyp);
  result:=GetWZ_SZ_Offset (i, b);
end;

{--------------------------- allgemeine Funktionen ----------------------------}

{ Wandelt einen Integer in die binäre Stringdarstellung um }
{ Parameter: anInt: Integerzahl, die darzustellen ist      }
{            Digits: minimal angezeigte Zeichen            }
{----------------------------------------------------}
function IntToBin(anInt: integer; Digits: byte): string;
{----------------------------------------------------}
var
  i: integer;
begin
  result:= '';  { default }
  i:= anInt;
  { Umwandeln in Binär-Format }
  while i > 0 do begin
    result:= intToStr((i mod 2)) + result;
    i:= i - ((i div 2)+(i mod 2));
  end;
  { Länge auf 'Digits' auffülln }
  while length(result) < Digits do result:= '0' + result;
end;

{ Gibt die Differenz zwischen zwei Zeiten in Sekunden aus }
{ Parameter: Time1, Time2: Zeiten, deren Differenz zu bilden ist }
{ Rückgabe: Differenz in Sekunden                    }
{----------------------------------------------------}
function GetTimeDiffInSec(Time1, Time2: TDateTime): integer;
{----------------------------------------------------}
var
  h1, n1, s1, ms1: word;
  h2, n2, s2, ms2: word;
begin
//  DecodeTime(abs(Time1-Time2), h, n, s, ms);
  DecodeTime(Time1, h1, n1, s1, ms1);
  DecodeTime(Time2, h2, n2, s2, ms2);
  result:= abs(24*3600*(trunc(Time1)-trunc(Time2)) + 3600*(h1-h2)
           + 60*(n1-n2) + (s1-s2));
end;

{ Gibt die Differenz zwischen zwei Zeiten als String aus }
{ Parameter: Time1, Time2: Zeiten, deren Differenz zu bilden ist }
{ Rückgabe: Differenz alsString                      }
{----------------------------------------------------}
function GetTimeDiffStr(Time1, Time2: TDateTime): string;
{----------------------------------------------------}
var
  i, i1            : integer;
begin
  result:= '';
  i:= GetTimeDiffInSec(Time1, Time2);
  { Anzahl der Tage }
  if (i >= 24*3600) then begin
    i1:= i div (24*3600);
    if length(result) > 0 then result:= result + ' ';
    result:= result + IntToStr(i1) + ' Tag';
    i:= i mod (24*3600);
  end;
  { Anzahl der Stunden }
  if (i >= 3600) then begin
    i1:= i div (3600);
    if length(result) > 0 then result:= result + ' ';
    result:= result + IntToStr(i1) + ' h';
    i:= i mod (3600);
  end;
  { Anzahl der Minuten }
  if (i >= 60) then begin
    i1:= i div (60);
    if length(result) > 0 then result:= result + ' ';
    result:= result + IntToStr(i1) + ' min';
    i:= i mod (60);
  end;
  { Anzahl der Sekunden }
  if i >= 0 then begin
    i1:= i;
    if length(result) > 0 then result:= result + ' ';
    result:= result + IntToStr(i1) + ' s';
  end;
end;

{ Gibt die den übergebenen Dateinamen ohne Extension }
{ zurück - besser ChangeFileExt                      }
{----------------------------------------------------}
function GetPathFileNameWithoutExt(aName: string): string;
{----------------------------------------------------}
var
  s  : string;
  p  : integer;
begin
  result:='';
  s:= aName;
  p:= pos('.', S);
  if p > 0 then begin
    delete(s, p+1, 3);
    result:= S;
  end;
end;

{ **********************   Achtung !  *********************** }

{ Wichtiger Hinweis zur Benutzung der RestructDaten-Funktion:
  Die Funktion darf auf keinen Fall auf Tabellen angewandt
  werden, auf die von mehreren Anwendungen/Prozessen aus zugegriffen
  wird (z.B. im WICOM32 -> WAUFRTAG.DB etc.) ! Beim Restrukturieren
  wird die Tabelle kurzzeitig gelöscht. Ein gleichzeitiger Aufruf
  von FileExists bzw. TTable.Exists kann dann als Ergebnis "false"
  liefern. Mögliche Folge: Die nicht erkannte Tabelle wird evtl.
  neu angelegt und überschrteibt die ursprüngliche ! }

{---------------------------------------------------------------}
function RestructDaten(aDatabaseName: string; aTableName: string;
                       ErrMsgOnOff: boolean): boolean;
{---------------------------------------------------------------}
{ -> entfernt gelöschte Sätze einer Tabelle physikalisch
  -> regeneriert alle "out-of-date" Indizes (gewartete Indizes)
  Parameter: DatabaseName
             TableName
             Schalter, ob Message-Ausgabe bei Fehler erfolgen soll
  Ergebnis: true, wenn Restrukturierung erfolgreich }
var
  pTableDesc : pCrTblDesc;
  hdb        : hDbiDb;
  oldCursor  : TCursor;
  t          : TTableExt;
  Erg        : DBIResult;

begin
  Result:=false;
  oldCursor:= Screen.Cursor;
  Screen.Cursor:= crHourGlass;
  try
    t:=TTableExt.Create (nil);
    try
      t.DatabaseName:=aDatabaseName;
      t.TableName:=aTableName;
      if t.Exists then begin
        if t.OpenExclusive then begin  { DbiDoRestructure braucht Exklusiv-Zugriff }
          try
            hdb:= t.DbHandle;
          finally
            t.Close;
          end;
          GetMem(pTableDesc, SizeOf(CrTblDesc));
          try
            FillChar(pTableDesc^, SizeOf(CrTblDesc), 0);
            StrPCopy(pTableDesc^.szTblName, t.TableName);
            StrCopy(pTableDesc^.szTblType, szParadox);
            pTableDesc^.bpack := true;    { Tabelle packen und Indizes warten }
            Erg:=DbiDoRestructure(hdb, 1, pTableDesc, nil, nil, nil, False);
          finally
            FreeMem(pTableDesc, SizeOf(CrTblDesc));
          end;
          if ErrMsgOnOff then
            Check(Erg);                          { bei Fehler Message-Ausgabe }
          Result:=Erg = 0;
        end;    { if t.OpenExclusive }
      end;  { if t.Exists }
    finally
      t.Free;
    end;
  finally
    Screen.Cursor:= oldCursor;
  end;
end;

{ Gibt von einem String den enthaltenen Float-Wert    }
{ oder im Fehlerfall einen Default-Wert zurück        }
{ Parameter: aString - string mit Wert                }
{            aFloat - Default-Wert                    }
{-----------------------------------------------------}
function WStrToFloatDef(aString: string; aFloat: double): double;
{-----------------------------------------------------}
begin
  try
    result:= StrToFloat(aString);
  except
    result:= aFloat;
  end;
end;

{ Prüft, ob Anzahl der lizenzierten MRG-Stationen erreicht ist         }
{ Übergabe: Pfad auf Stammdaten                                        }
{ Rückgabe: True - Anzahl erreicht; FALSE - Anzahl noch nicht erreicht }
{-------------------------------------------------------}
function WStationslizenz32_MRG (StaPfad: string): boolean;
{-------------------------------------------------------}
var
  res: integer;
  Lizenz: TWLizenz32;
  afn: string;
  StationCount: integer;
  q: TQuery;

begin
  Result:=false;
  { Anzahl der bereits vorhandenen MRG-Stationen ermitteln: }
  if FileExists(StaPfad + CDBSta) then begin
    q:= TQuery.Create(nil);
    try
      q.DatabaseName:=StaPfad;
      q.Sql.Add('SELECT COUNT(*)');
      q.Sql.Add('FROM "' + CDBSta + '"');
      q.Sql.Add('WHERE ' + C_Sta_Aktiv + '= :Aktiv');
      q.ParamByName('Aktiv').asInteger:= 0;
      q.open;
      StationCount:= q.Fields[0].asInteger;
    except
      on exception do begin
        q.Free;
        if IsDebugFlag then showMessage('My_Utils - WStationslizenz32_MRG');
        exit;
      end;
    end;
    q.Free;
  end else
    exit;

  { mit Anzahl der lizenzierten Stationen vergleichen: }
  Lizenz:= TWLizenz32.Create;
  try
    afn:=FN_MRG_Stationen;
    res:= Lizenz.ReadStationenFromLizenzFile(afn, StationCount);
    if res < 0 then begin
      Lizenz.ShowStationsLizenzDialog(afn, res);
      Result:= False;
    end else
      Result:= True;
  finally
    Lizenz.Free;
  end;
end;

{ Prüft, ob Anzahl der lizenzierten DSfG-Stationen erreicht ist                   }
{ Übergabe: Pfad auf Stammdaten                                                   }
{           Flag 'Wieser_MRG910_frei' (wenn true, gehen die DSfG-Stationen        }
{                                      vom Typ Wieser MRG 910 NICHT in die        }
{                                      Berechnung der lizenzierten Stationen ein  }
{           Flag 'Wieser_ohne_MRG910_frei' (wenn true, gehen die DSfG-Stationen   }
{                                      vom Typ Wieser (ohne MRG 910) NICHT in die }
{                                      Berechnung der lizenzierten Stationen ein  }
{           Flag 'MsgDlgOut' (wenn true, erfolgt Ausgabe eines Dialogfensters bei }
{                             ungültiger Lizenz)                                  }
{ Rückgabe: 0 = Anzahl noch nicht erreicht                                        }
{           übrige Rückgabewerte siehe TWLizenz32.ReadStationenFromLizenzFile     }
{----------------------------------------------------------------}
function WStationslizenz32_DSfG (StaPfad: string;
                                 Wieser_MRG910_frei: boolean;
                                 Wieser_ohne_MRG910_frei: boolean;
                                 MsgDlgOut: boolean): integer;
{----------------------------------------------------------------}
var
  Lizenz: TWLizenz32;
  afn: string;
  StationCount: integer;
  q: TQuery;

begin
  Result:=-99;
  { Anzahl der bereits vorhandenen DSfG-Stationen ermitteln: }
  if FileExists(StaPfad + C_DTB_Station) then begin
    q:= TQuery.Create(nil);
    try
      q.DatabaseName:=StaPfad;
      q.Sql.Add('SELECT A.' + C_DTF_Station_StationId);
      q.Sql.Add('FROM "' + C_DTB_Station + '" A,');
      q.sql.Add('"' + C_DTB_Instanz + '" B');
      q.sql.Add('WHERE (A.' + C_DTF_Station_StationId + ' = B.' + C_DTF_Instanz_StationId + ') AND ');
      q.Sql.Add('(A."'+ C_DTF_Station_LIInstanz + '" = B.' + C_DTF_Instanz_Instanzname + ') AND '); { nur die Zugangs-DFÜ-Instanz }
      q.Sql.Add('(B.' + C_DTF_Instanz_Instanztyp + ' = ''' + C_D_Instanztyp_DFU + ''') AND ');
      q.Sql.Add('(B.' + C_DTF_Instanz_Busadresse + ' <> ''0'')');   { ohne die "Start-Stammsätze", welche durch automatisches
                                                                      Konfig-Einlesen entstehen }
      if Wieser_MRG910_frei AND Wieser_ohne_MRG910_frei then begin
        q.Sql.Add(' AND ((Upper(B.' + C_DTF_Instanz_Hersteller + ') NOT LIKE ''%WIES%'') OR');      { alle Fremdhersteller }
        q.Sql.Add('B.' + C_DTF_Instanz_Hersteller + ' IS NULL)');                                   { alle ohne Herstellerangabe }
      end
      else if Wieser_MRG910_frei then begin
        q.Sql.Add(' AND (((Upper(B.' + C_DTF_Instanz_Hersteller + ') LIKE ''%WIES%'') AND ');
        q.Sql.Add('      (Upper(B.' + C_DTF_Instanz_Geraetetyp + ') NOT LIKE ''%MRG910%'')) OR ');  { alle Wieser außer MRG 910 }
        q.Sql.Add('      (Upper(B.' + C_DTF_Instanz_Hersteller + ') NOT LIKE ''%WIES%'') OR ');     { alle Fremdhersteller }
        q.Sql.Add('B.' + C_DTF_Instanz_Hersteller + ' IS NULL)');                                   { alle ohne Herstellerangabe }
      end
      else if Wieser_ohne_MRG910_frei then begin
        q.Sql.Add(' AND (((Upper(B.' + C_DTF_Instanz_Hersteller + ') LIKE ''%WIES%'') AND ');
        q.Sql.Add('      (Upper(B.' + C_DTF_Instanz_Geraetetyp + ') LIKE ''%MRG910%'')) OR ');      { alle Wieser MRG 910 }
        q.Sql.Add('      (Upper(B.' + C_DTF_Instanz_Hersteller + ') NOT LIKE ''%WIES%'') OR ');     { alle Fremdhersteller }
        q.Sql.Add('B.' + C_DTF_Instanz_Hersteller + ' IS NULL)');                                   { alle ohne Herstellerangabe }
      end;
      q.open;
      StationCount:= q.RecordCount;
    except
      on exception do begin
        q.Free;
        if IsDebugFlag then showMessage('My_Utils - WStationslizenz32_DSfG');
        exit;
      end;
    end;
    q.Free;
  end else
    exit;

  { mit Anzahl der lizenzierten Stationen vergleichen: }
  Lizenz:=TWLizenz32.Create;
  try
    afn:=FN_DSFG_Stationen;
    Result:=Lizenz.ReadStationenFromLizenzFile(afn, StationCount);
    if (Result < 0) AND MsgDlgOut then
      Lizenz.ShowStationsLizenzDialog(afn, Result);
  finally
    Lizenz.Free;
  end;
end;

end.
