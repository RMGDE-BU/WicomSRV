{******************************************************************************}
{* Unit: Zugriff auf MRG-Parameter-Tabellen                                   *}
{* 13.12.2000 WW                                                              *}
{******************************************************************************}
unit MDBParam;

interface

uses
  SysUtils, DBTables, DB, PathIni, MDBMrg, WSysCon, MResParam;


function GetParameterNrMrg_ByParaGruppeDB (ParaGruppe: integer; ParaNrAllg: string;
                                           var ParaNrMrg: string): boolean;
function GetParameterNrMrg_ByMrgTypDB (MrgTyp: TMrgTyp; ParaNrAllg: string;
                                       var ParaNrMrg: string): boolean;
function GetAllParameterNrMrg_ByMrgTypDB (MrgTyp: TMrgTyp; aQuery: TQuery): boolean;
function GetParamMrg_KonfigList_ByParaGruppeDB (Parametergruppe: integer;
                                                ParamMrgKonfigList: TParamMrgKonfigList): boolean;
function GetParaText_KonfigListDB (ParaTextKonfigList: TParaTextKonfigList): boolean;

implementation

{---------------------------------------------------------------------------------}
function GetParameterNrMrg_ByParaGruppeDB (ParaGruppe: integer; ParaNrAllg: string;
                                           var ParaNrMrg: string): boolean;
{---------------------------------------------------------------------------------}
{ liefert MRG-spezifische Parameternummer über Parametergruppe aus Tabelle;
  Übergabe: Parametergruppe, allgemeine Parameternummer
  Rückgabe: MRG-spezifische Parameternummer;
  Ergebnis: true, wenn MRG-spezifische Parameternummer gefunden }
var
  ParamMrgTable: TTable;
begin
  Result := false;
  ParaNrMrg:='';
  if FileExists(PathServer.PathName[WStammDir]+CDBParamMRG) then begin
    ParamMrgTable:=TTable.Create(nil);
    try
      ParamMrgTable.DatabaseName:=PathServer.PathName[WStammDir];
      ParamMrgTable.TableName:=CDBParamMRG;
      ParamMrgTable.Open;
      try
        if ParamMrgTable.FindKey ([ParaGruppe, ParaNrAllg]) then begin
          ParaNrMrg:=ParamMrgTable.FieldByName (C_ParamMrg_Parameternummer_im_MRG).AsString;
          Result:=true;
        end;
      finally
        ParamMrgTable.Close;
      end;
    finally
      ParamMrgTable.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------}
function GetParameterNrMrg_ByMrgTypDB (MrgTyp: TMrgTyp; ParaNrAllg: string;
                                       var ParaNrMrg: string): boolean;
{-------------------------------------------------------------------------}
{ liefert MRG-spezifische Parameternummer über MRG-Typ aus Tabelle;
  Übergabe: MRG-Typ, allgemeine Parameternummer
  Rückgabe: MRG-spezifische Parameternummer;
  Ergebnis: true, wenn MRG-spezifische Parameternummer gefunden }
var
  MrgKonvDataDB: TMrgKonvDataDB;
Begin
  Result:=false;
  ParaNrMrg:='';
  if GetMrgKonvDataDB (MrgTyp, MrgKonvDataDB) then begin
    if GetParameterNrMrg_ByParaGruppeDB (MrgKonvDataDB.ParameterGruppe, ParaNrAllg,
                                         ParaNrMrg) then
      Result:=true;
  end;
end;

{----------------------------------------------------------------------------------}
function GetAllParameterNrMrg_ByMrgTypDB (MrgTyp: TMrgTyp; aQuery: TQuery): boolean;
{----------------------------------------------------------------------------------}
{ liefert alle MRG-spezifischen Parameternummern zum übergebenen MRG-Typ aus Tabelle;
  Übergabe: MRG-Typ
  Rückgabe: Query mit MRG-spezifischen Parameternummern
  Ergebnis: true, wenn Rückgabe-Query geöffnet }
var
  MrgKonvDataDB: TMrgKonvDataDB;
begin
  Result:=false;
  if GetMrgKonvDataDB (MrgTyp, MrgKonvDataDB) then begin
    try
      aQuery.Close;
      aQuery.DatabaseName:=PathServer.PathName[WStammDir];
      with aQuery.Sql do begin
        Clear;
        Add('SELECT ' + C_ParamMrg_Parameternummer_im_MRG);
        Add('FROM "' + CDBParamMRG + '"');
        Add('WHERE ' + C_ParamMrg_Parametergruppe + ' = ' + IntToStr (MrgKonvDataDB.ParameterGruppe));
      end;
      aQuery.Open;
      Result:=true;
    except
      Result:=false;
    end;
  end;
end;

{------------------------------------------------------------------------------------------------}
function GetParamMrg_KonfigList_ByParaGruppeDB (Parametergruppe: integer;
                                                ParamMrgKonfigList: TParamMrgKonfigList): boolean;
{------------------------------------------------------------------------------------------------}
{ liefert auf übergebene Parametergruppe gefilterten Inhalt von ParamMrg.db als Liste;
  Übergabe: Parametergruppe
  Rückgabe: Liste mit Parameter-Konfigurationsdaten
  Ergebnis: true, wenn Parameter-Konfigurationsdaten gefunden }
var
  ParamMrgData: TParamMrgData;
  ParamMrgDataObj: TParamMrgDataObj;
  Query: TQuery;

begin
  Result:=false;
  if ParamMrgKonfigList = nil then exit;

  { Query mit allen zum Gerätetyp gehörenden Parameter-Nummern erstellen: }
  Query:=TQuery.Create (nil);
  try
    Query.DatabaseName:=PathServer.PathName[WStammDir];
    with Query.Sql do begin
      Clear;
      Add('SELECT *');
      Add('FROM "' + CDBParamMRG + '"');
      Add('WHERE ' + C_ParamMrg_Parametergruppe + ' = ' + IntToStr (Parametergruppe));
    end;
    Query.Open;
    while not Query.Eof do begin
      with ParamMrgData do begin
        Parametergruppe:=Query.FieldByName (C_ParamMrg_Parametergruppe).AsInteger;
        Parameternummer:=Query.FieldByName (C_ParamMrg_Parameternummer).AsString;
        Parameternummer_im_MRG:=Query.FieldByName (C_ParamMrg_Parameternummer_im_MRG).AsString;
      end;
      { Listenobjekt createn und in Liste einfügen: }
      ParamMrgDataObj:=TParamMrgDataObj.Create;
      ParamMrgDataObj.SetData (ParamMrgData);
      ParamMrgKonfigList.Add (ParamMrgDataObj);
      Query.Next;
    end;
    Result:=true;
  finally
    Query.Close;
    Query.Free;
  end;
end;

{-----------------------------------------------------------------------------------}
function GetParaText_KonfigListDB (ParaTextKonfigList: TParaTextKonfigList): boolean;
{-----------------------------------------------------------------------------------}
{ liefert Inhalt von ParaText.Db als Liste;
  Rückgabe: Liste mit Parametertexten
  Ergebnis: true, wenn Parametertexte in die Liste geladen wurden }
var
  ParaTextTable: TTable;
  ParaTextData: TParaTextData;
  ParaTextDataObj: TParaTextDataObj;

begin
  Result := false;
  if ParaTextKonfigList = nil then exit;

  if FileExists(PathServer.PathName[WStammDir]+C_TbParaText) then begin
    ParaTextTable:=TTable.Create(nil);
    try
      ParaTextTable.DatabaseName:=PathServer.PathName[WStammDir];
      ParaTextTable.TableName:=C_TbParaText;
      ParaTextTable.Open;
      try
        while not ParaTextTable.Eof do begin
          with ParaTextData do begin
            Parameternummer:=ParaTextTable.FieldByName (C_TfParameterNummer).AsString;
            Parametertext:=ParaTextTable.FieldByName (C_TfParameterText).AsString;
          end;
          { Listenobjekt createn und in Liste einfügen: }
          ParaTextDataObj:=TParaTextDataObj.Create;
          ParaTextDataObj.SetData (ParaTextData);
          ParaTextKonfigList.Add (ParaTextDataObj);
          ParaTextTable.Next;
        end;
        Result:=true;
      finally
        ParaTextTable.Close;
      end;
    finally
      ParaTextTable.Free;
    end;
  end;
end;

end.
