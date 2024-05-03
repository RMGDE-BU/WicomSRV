{******************************************************************************}
{* Unit: Zugriff auf Momentanwerte-Tabellen für DSfG-DFÜ-Instanz              *}
{* 12.04.2001 WW                                                              *}
{******************************************************************************}
unit TBDSFGMomDfue;

interface

uses
  Windows, Forms, SysUtils, WTables, Db_Attn, Db, DbTables, DListen, WSysCon,
  Classes, WStrUtils;

type

  { Objekt für Datentabelle mit DSfG-DFÜ-Momentanwerte }

  TTbDSfGMomDfueDaten = class(TObject)
  private
    Path: TFileName;
    StationId: integer;
    function CreatetbDMomDfue: boolean;
  public
    tbDMomDfue: TTableExt;
    constructor Create (APath: TFileName; AStationId: integer);
    destructor Destroy; override;
    function OpenMomDfueTable: boolean;
    procedure CloseMomDfueTable;
    function EmptyMomDfueTable: boolean;
    procedure WriteMomDfueTable (Befehl: string; ParaAdr: string; Wert: string; Stellen: integer);
    procedure UpdateMomDfueTable (Befehl: string; ParaAdr: string; Wert: string);
    function GetMomDfueDaten (aQuery: TQueryExt): boolean;
    function GetMomDfueDatenByList (aList: TDMomDfueList): boolean;
    function GetNormParameter (aQuery: TQueryExt): boolean;
    function GetNormParameterByList (aList: TDMomDfueList): boolean;
    function GetTeilnehmerliste (var Teilnehmerliste: string): boolean;
    function GetWieserParameter (StammdatPath: string;
                                 aQuery: TQueryExt): boolean;
    function GetWieserParameterByList (StammdatPath: string;
                                       aList: TDWieserDfueParaList): boolean;
  end;


implementation


{ TTbDSfGMomDfueDaten }

{-----------------------------------------------------------------------------}
constructor TTbDSfGMomDfueDaten.Create (APath: TFileName; AStationId: integer);
{-----------------------------------------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  StationId:=AStationId;
  tbDMomDfue:=TTableExt.Create(nil);
  tbDMomDfue.DataBaseName:= Path;
  tbDMomDfue.TableName:= C_Tb_DMomDfue + Format('%.4d.DB', [StationId]);
end;

{-------------------------------------}
destructor TTbDSfGMomDfueDaten.Destroy;
{-------------------------------------}
begin
  tbDMomDfue.Free;
  inherited Destroy;
end;

{-----------------------------------------------------}
function TTbDSfGMomDfueDaten.CreatetbDMomDfue: boolean;
{-----------------------------------------------------}
{ Tabelle anlegen }
begin
  with tbDMomDfue.FieldDefs do begin
    Clear;
    Add(C_Tf_DMomDfue_Befehl, ftString, 3, false);
    Add(C_Tf_DMomDfue_ParaAdr, ftString, 3, false);
    Add(C_Tf_DMomDfue_Wert, ftString, 40, false);
    Add(C_Tf_DMomDfue_Stellen, ftSmallint, 0, false);
  end;
  with tbDMomDfue.IndexDefs do begin
    Clear;
    Add('Primaerindex', C_Tf_DMomDfue_Befehl+';'+C_Tf_DMomDfue_ParaAdr, [ixPrimary, ixUnique]);
  end;
  Result:=tbDMomDfue.CreateTable;
end;

{-----------------------------------------------------}
function TTbDSfGMomDfueDaten.OpenMomDfueTable: boolean;
{-----------------------------------------------------}
{ DFÜ-Momentanwert-Tabelle öffnen, falls nicht vorhanden zuvor neuanlegen;
  Ergebnis: true, wenn Tabelle geöffnet werden konnte }
begin
  if not tbDMomDfue.Exists then
    if not CreatetbDMomDfue then begin
      Result:=false;
      exit;                       { Tabelle neuanlegen }
    end;
  Result:=tbDMomDfue.OpenShared;
end;

{----------------------------------------------}
Procedure TTbDSfGMomDfueDaten.CloseMomDfueTable;
{----------------------------------------------}
{ DFÜ-Momentanwert-Tabelle schließen }
begin
  tbDMomDfue.Close;
end;

{------------------------------------------------------}
function TTbDSfGMomDfueDaten.EmptyMomDfueTable: boolean;
{------------------------------------------------------}
{ DFÜ-Momentanwert-Tabelle leeren;
  Ergebnis: true, wenn Tabelle geleert werden konnte }
begin
  if tbDMomDfue.Exists then
    Result:=tbDMomDfue.EmptyTable
  else
    Result:=true;
end;

{----------------------------------------------------------------------------------------------------------------}
procedure TTbDSfGMomDfueDaten.WriteMomDfueTable (Befehl: string; ParaAdr: string; Wert: string; Stellen: integer);
{----------------------------------------------------------------------------------------------------------------}
{ Datensatz in DFÜ-Momentanwert-Tabelle schreiben; Öffnen und schließen der Tabelle muß außerhalb der Prozedur
  vorgenommen werden !
  Übergabe: Befehl
            Parameter-Adresse  (z.B. die Parameternummer bei B-Befehl-Parameter)
            Wert
            Länge des Wert-Strings (abschließende Leerzeichen werden von der Tabelle ignoriert !)
  Ergebnis: true, wenn Datensatz geschrieben wurde }
begin
  if tbDMomDfue.Active then begin
    if tbDMomDfue.FindKey ([Befehl, ParaAdr]) then begin
      tbDMomDfue.Edit;
      tbDMomDfue.FieldByName(C_Tf_DMomDfue_Wert).AsString:=Wert;
      if Stellen > -1 then
        tbDMomDfue.FieldByName(C_Tf_DMomDfue_Stellen).AsInteger:=Stellen;
      tbDMomDfue.Post;
    end
    else begin
      if Stellen > -1 then
        tbDMomDfue.InsertRecord([Befehl, ParaAdr, Wert, Stellen])
      else
        tbDMomDfue.InsertRecord([Befehl, ParaAdr, Wert]);
    end;
  end;
end;

{-----------------------------------------------------------------------------------------------}
Procedure TTbDSfGMomDfueDaten.UpdateMomDfueTable (Befehl: string; ParaAdr: string; Wert: string);
{-----------------------------------------------------------------------------------------------}
{ Wert für einen Parameter (identifiziert durch seinen Befehl und seine Parameter-Adresse)
  in DFÜ-Momentanwert-Tabelle aktualisieren;
  Übergabe: Befehl
            Parameter-Adresse
            Wert }
begin
  if OpenMomDfueTable then begin
    try
      WriteMomDfueTable (Befehl, ParaAdr, Wert, -1);
    finally
      CloseMomDfueTable;
    end;
    { Neuer Wert in der Tabelle: Triggerfile schreiben }
    WriteNewTime (tbDMomDfue.DatabaseName + tbDMomDfue.TableName);
  end;
end;

{------------------------------------------------------------------------}
function TTbDSfGMomDfueDaten.GetMomDfueDaten (aQuery: TQueryExt): boolean;
{------------------------------------------------------------------------}
{ gibt kompletten Inhalt der DFÜ-Momentanwert-Tabelle in einem Query zurück;
  Ergebnis: false, wenn keine DFÜ-Momentanwertdaten vorhanden sind }
begin
  result:= False; { default }
  if tbDMomDfue.Exists then begin
    aQuery.Close;
    aQuery.DatabaseName:= Path;
    with aQuery.Sql do begin
      Clear;
      Add('SELECT ' + C_Tf_DMomDfue_Befehl + ', ' + C_Tf_DMomDfue_ParaAdr + ', ' +
                      C_Tf_DMomDfue_Wert);
      Add('FROM "' + tbDMomDfue.TableName + '"');
    end;
    if aQuery.Open then
      if aQuery.RecordCount > 0 then result:= true;
  end;
end;

{---------------------------------------------------------------------------------}
function TTbDSfGMomDfueDaten.GetMomDfueDatenByList (aList: TDMomDfueList): boolean;
{---------------------------------------------------------------------------------}
{ gibt kompletten Inhalt der DFÜ-Momentanwert-Tabelle in einer Liste zurück;
  Ergebnis: false, wenn keine DFÜ-Momentanwertdaten vorhanden sind }
var
  q: TQueryExt;
  DMomDfueListObj: TDMomDfueListObj;

begin
  Result:=false;
  aList.Clear;   { Liste leeren }

  q:=TQueryExt.Create (nil);
  try
    if GetMomDfueDaten (q) then begin
      q.First;
      while not q.eof do begin
        DMomDfueListObj:=TDMomDfueListObj.Create;
        DMomDfueListObj.SetData (q.FieldByName (C_Tf_DMomDfue_Befehl).AsString,
                                 q.FieldByName (C_Tf_DMomDfue_ParaAdr).AsString,
                                 q.FieldByName (C_Tf_DMomDfue_Wert).AsString,
                                 0);
        aList.Add (DMomDfueListObj);
        q.Next;
      end;  { while not q.eof }
      Result:=true;
    end;  { if GetMomDfueDaten }
  finally
    q.Free;
  end;
end;

{-------------------------------------------------------------------------}
function TTbDSfGMomDfueDaten.GetNormParameter (aQuery: TQueryExt): boolean;
{-------------------------------------------------------------------------}
{ gibt nur die Parameter nach DSfG-Norm aus der DFÜ-Momentanwert-Tabelle in einem
  Query zurück;
  Ergebnis: false, wenn keine Norm-Parameter vorhanden sind }
begin
  result:= False; { default }
  if tbDMomDfue.Exists then begin
    aQuery.Close;
    aQuery.DatabaseName:= Path;
    with aQuery.Sql do begin
      Clear;
      Add('SELECT ' + C_Tf_DMomDfue_Befehl + ', ' + C_Tf_DMomDfue_ParaAdr + ', ' +
                      C_Tf_DMomDfue_Wert);
      Add('FROM "' + tbDMomDfue.TableName + '"');
      Add('WHERE ' + C_Tf_DMomDfue_Befehl + ' = "D" OR');
      Add(C_Tf_DMomDfue_Befehl + ' = "E" OR');
      Add(C_Tf_DMomDfue_Befehl + ' = "I" OR');
      Add(C_Tf_DMomDfue_Befehl + ' = "K" OR');
      Add(C_Tf_DMomDfue_Befehl + ' = "R" OR');
      Add(C_Tf_DMomDfue_Befehl + ' = "U" OR');
      Add(C_Tf_DMomDfue_Befehl + ' = "V"');
    end;
    if aQuery.Open then
      if aQuery.RecordCount > 0 then result:= true;
  end;
end;

{----------------------------------------------------------------------------------}
function TTbDSfGMomDfueDaten.GetNormParameterByList (aList: TDMomDfueList): boolean;
{----------------------------------------------------------------------------------}
{ gibt nur die Parameter nach DSfG-Norm aus der DFÜ-Momentanwert-Tabelle in einer
  Liste zurück;
  Ergebnis: false, wenn keine Norm-Parameter vorhanden sind }
var
  q: TQueryExt;
  DMomDfueListObj: TDMomDfueListObj;

begin
  Result:=false;
  aList.Clear;   { Liste leeren }

  q:=TQueryExt.Create (nil);
  try
    if GetNormParameter (q) then begin
      q.First;
      while not q.eof do begin
        DMomDfueListObj:=TDMomDfueListObj.Create;
        DMomDfueListObj.SetData (q.FieldByName (C_Tf_DMomDfue_Befehl).AsString,
                                 q.FieldByName (C_Tf_DMomDfue_ParaAdr).AsString,
                                 q.FieldByName (C_Tf_DMomDfue_Wert).AsString,
                                 0);
        aList.Add (DMomDfueListObj);
        q.Next;
      end;  { while not q.eof }
      Result:=true;
    end;  { if GetNormParameter }
  finally
    q.Free;
  end;
end;

{-------------------------------------------------------------------------------------}
function TTbDSfGMomDfueDaten.GetTeilnehmerliste (var Teilnehmerliste: string): boolean;
{-------------------------------------------------------------------------------------}
{ gibt nur die Teilnehmerliste aus der DFÜ-Momentanwert-Tabelle in einem
  String zurück;
  Ergebnis: false, wenn keine Teilnehmerliste vorhanden ist }
var
  q: TQueryExt;

begin
  Result:=false;
  Teilnehmerliste:='';

  if tbDMomDfue.Exists then begin
    q:=TQueryExt.Create (nil);
    try
      q.DatabaseName:= Path;
      with q do begin
        Sql.Add('SELECT ' + C_Tf_DMomDfue_Wert);
        Sql.Add('FROM "' + tbDMomDfue.TableName + '"');
        Sql.Add('WHERE ' + C_Tf_DMomDfue_Befehl + ' = "YWT"');
      end;
      if q.Open then begin
        if q.RecordCount > 0 then begin
          Teilnehmerliste:=q.FieldByName (C_Tf_DMomDfue_Wert).AsString;
          result:= true;
        end;
      end;  
    finally
      q.Free;
    end;
  end;
end;

{---------------------------------------------------------------------------}
function TTbDSfGMomDfueDaten.GetWieserParameter (StammdatPath: string;
                                                 aQuery: TQueryExt): boolean;
{---------------------------------------------------------------------------}
{ gibt nur die Wieser-Parameter plus Informationen aus der DFÜ-Momentanwert-Tabelle
  und der DDFUPARA.DB in einem Query zurück;
  Übergabe: Stammdaten-Pfad, in dem sich DDFUPARA.DB befindet
            aQuery
  Ergebnis: false, wenn keine Wieser-Parameter vorhanden sind }
begin
  result:= False; { default }
  if tbDMomDfue.Exists AND FileExists (StammdatPath + C_Tb_DDfuPara) then begin
    aQuery.Close;
    aQuery.DatabaseName:= Path;
    with aQuery.Sql do begin
      Clear;
      Add('SELECT');
      Add('A.' + C_Tf_DMomDfue_ParaAdr +',');
      Add('A.' + C_Tf_DMomDfue_Wert +',');
      Add('A.' + C_Tf_DMomDfue_Stellen +',');
      Add('B.' + C_Tf_DDfuPara_ParaName +',');
      Add('B.' + C_Tf_DDfuPara_aenderbar +',');
      Add('B.' + C_Tf_DDfuPara_Typ +',');
      Add('B.' + C_Tf_DDfuPara_DefText +',');
      Add('B.' + C_Tf_DDfuPara_DefWert);
      Add('FROM "'+ tbDMomDfue.TableName + '" A');
      Add('LEFT JOIN "' + StammdatPath + C_Tb_DDfuPara + '" B');
      Add('ON CAST (A.' + C_Tf_DMomDfue_ParaAdr +' AS SMALLINT) = B.' + C_Tf_DDfuPara_ParaNummer);
      Add('WHERE A.' + C_Tf_DMomDfue_Befehl + ' = "YWB"');
    end;
    if aQuery.Open then
      if aQuery.RecordCount > 0 then result:= true;
  end;
end;

{-------------------------------------------------------------------------------------------}
function TTbDSfGMomDfueDaten.GetWieserParameterByList (StammdatPath: string;
                                                       aList: TDWieserDfueParaList): boolean;
{-------------------------------------------------------------------------------------------}
{ gibt nur die Wieser-Parameter plus Informationen aus der DFÜ-Momentanwert-Tabelle
  und der DDFUPARA.DB in einer Liste zurück;
  Übergabe: Stammdaten-Pfad, in dem sich DDFUPARA.DB befindet
            aList
  Ergebnis: false, wenn keine Wieser-Parameter vorhanden sind }
var
  q: TQueryExt;
  DWieserDfueParaListObj: TDWieserDfueParaListObj;

begin
  Result:=false;
  aList.Clear;   { Liste leeren }

  q:=TQueryExt.Create (nil);
  try
    if GetWieserParameter (StammdatPath, q) then begin
      q.First;
      while not q.eof do begin
        DWieserDfueParaListObj:=TDWieserDfueParaListObj.Create;
        DWieserDfueParaListObj.SetData (q.FieldByName (C_Tf_DMomDfue_ParaAdr).AsInteger,
                                        q.FieldByName (C_Tf_DMomDfue_Wert).AsString,
                                        q.FieldByName (C_Tf_DMomDfue_Stellen).AsInteger,
                                        q.FieldByName (C_Tf_DDfuPara_ParaName).AsString,
                                        q.FieldByName (C_Tf_DDfuPara_aenderbar).AsBoolean,
                                        q.FieldByName (C_Tf_DDfuPara_Typ).AsString,
                                        q.FieldByName (C_Tf_DDfuPara_DefText).AsString,
                                        q.FieldByName (C_Tf_DDfuPara_DefWert).AsString);
        aList.Add (DWieserDfueParaListObj);
        q.Next;
      end;  { while not q.eof }
      Result:=true;
    end;  { if GetWieserParameter }
  finally
    q.Free;
  end;
end;

end.
