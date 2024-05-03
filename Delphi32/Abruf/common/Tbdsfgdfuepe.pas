{********************************************************************************}
{* Unit: Zugriff auf DSfG-DFÜ-Parametrierungstabelle                            *}
{* 20.04.2001 WW                                                                *}
{********************************************************************************}
unit TbDsfgDfuePe;

interface

uses
  Forms, SysUtils, Dialogs, WTables, Db_Attn, Db, DbTables, WSysCon, DListen;

type

  { Objekt für Tabelle zur Parametrierung der DSfG-DFÜ }

  TTbDSfGDfueParametrierung = class(TObject)
  private
    Path        : TFileName;
    tbDfuePe    : TTableExt;
    StationId       : integer;
    function CreatetbDfuePe: boolean;
  public
    constructor Create (APath: TFileName; AStationId: integer);
    destructor Destroy; override;
    procedure CopyFromMomTable;
    function GetSendeParameter (var DfueParaSendenData: DfueParaSendenDataRec): boolean;
    procedure SetSendeparameter(DfueParaSendenData: DfueParaSendenDataRec);
    procedure WriteErgebnis (DfueParaErgebnisData: DfueParaErgebnisDataRec);
    function GetAllParameterChanges (aQuery: TQueryExt): boolean;
    function GetAllParameterChangesByList (aList: TDDfuePEList): boolean;
  end;

implementation

{ TTbDSfGDfueParametrierung }

{-----------------------------------------------------------------------------------}
constructor TTbDSfGDfueParametrierung.Create (APath: TFileName; AStationId: integer);
{-----------------------------------------------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  StationId:=AStationId;
  tbDfuePe:=TTableExt.Create(nil);
  tbDfuePe.DataBaseName:= Path;
  tbDfuePe.TableName:=C_Tb_DDfuePE + Format('%.4d.DB', [StationId]);
end;

{-------------------------------------------}
destructor TTbDSfGDfueParametrierung.Destroy;
{-------------------------------------------}
begin
  tbDfuePe.Free;
  inherited Destroy;
end;

{---------------------------------------------------------}
function TTbDSfGDfueParametrierung.CreatetbDfuePe: boolean;
{---------------------------------------------------------}
{ Tabelle anlegen }
begin
  with tbDfuePe.FieldDefs do begin
    Clear;
    Add(C_Tf_DDfuePE_Befehl, ftString, 3, false);
    Add(C_Tf_DDfuePE_ParaAdr, ftString, 3, false);
    Add(C_Tf_DDfuePE_WertAlt, ftString, 40, false);
    Add(C_Tf_DDfuePE_WertNeu, ftString, 40, false);
    Add(C_Tf_DDfuePE_Stellen, ftSmallInt, 0, false);
    Add(C_Tf_DDfuePE_StaAktu, ftBoolean, 0, false);
    Add(C_Tf_DDfuePE_Aendern, ftBoolean, 0, false);
    Add(C_Tf_DDfuePE_Fertig, ftBoolean, 0, false);
  end;
  with tbDfuePe.IndexDefs do begin
    Clear;
    Add('Primaerindex', C_Tf_DDfuePE_Befehl+';'+C_Tf_DDfuePE_ParaAdr, [ixPrimary, ixUnique]);
  end;
  Result:=tbDfuePe.CreateTable;
end;

{---------------------------------------------------}
procedure TTbDSfGDfueParametrierung.CopyFromMomTable;
{---------------------------------------------------}
{ Parameter aus der DFÜ-Momentanwerttabelle in die DFÜ-Parametrierungstabelle schreiben }
var
  DfueMomTable: TTableExt;
  Befehl: string;
  ParaAdr: string;
  WertStr: string;
  Stellen: integer;
  Ok: boolean;

begin
  if not tbDfuePe.Exists then                            { Zieltabelle: Parametrierungstabelle }
    Ok:=CreatetbDfuePe
  else
    Ok:=tbDfuePe.EmptyTable;

  if Ok then begin
    DfueMomTable:=TTableExt.Create (nil);                { Quelltabelle: Momentanwerttabelle }
    try
      DfueMomTable.DatabaseName:=Path;
      DfueMomTable.TableName:=C_Tb_DMomDfue + Format('%.4d.DB', [StationId]);      { StationId im Tabellennamen }
      if DfueMomTable.Exists then begin
        if DfueMomTable.OpenShared then begin
          try
            if tbDfuePe.OpenShared then begin
              try
                while not DfueMomTable.Eof do begin
                  Befehl:=DfueMomTable.FieldByName (C_Tf_DMomDfue_Befehl).AsString;
                  ParaAdr:=DfueMomTable.FieldByName (C_Tf_DMomDfue_ParaAdr).AsString;
                  WertStr:=DfueMomTable.FieldByName (C_Tf_DMomDfue_Wert).AsString;
                  Stellen:=DfueMomTable.FieldByName (C_Tf_DMomDfue_Stellen).AsInteger;
                  { Nr, Wert, Stellen und False-Vorbelegung für "Aendern" und
                    "Fertig" in Tabelle schreiben: }
                  tbDfuePe.InsertRecord ([Befehl, ParaAdr, WertStr, nil, Stellen, nil, false, false]);
                  Application.ProcessMessages;
                  DfueMomTable.Next;
                end;
              finally
                tbDfuePe.Close;
              end;
            end;
          finally
            DfueMomTable.Close;
          end;
        end;
      end; { if MomTable.Exists }
    finally
      DfueMomTable.Free;
    end;
  end;
end;

{ Schreibt einen Record in die DSfG-DFÜ-Parametrierungstabelle DDFExxxx                  }
{-----------------------------------------------------------------------------------------------}
procedure TTbDSfGDfueParametrierung.SetSendeparameter(DfueParaSendenData: DfueParaSendenDataRec);
{-----------------------------------------------------------------------------------------------}
Var
  q: TQueryExt;
begin
  if tbDfuePe.Exists then begin
    q:=TQueryExt.Create(nil);
    try
      with q do begin
        DatabaseName:=Path;
        Sql.Clear;
        Sql.Add('UPDATE "' + tbDfuePe.TableName + '"');
        Sql.Add('SET ' + C_Tf_DDfuePE_WertNeu + ' = "' + DfueParaSendenData.Wert + '", ');
        Sql.Add(C_Tf_DDfuePE_Stellen + ' = ' + IntToStr(DfueParaSendenData.Stellen)  + ', ');
        Sql.Add(C_Tf_DDfuePE_StaAktu + ' = :StaAktu' + ', ');
        Sql.Add(C_Tf_DDfuePE_Aendern + ' = :Aendern');
        Sql.Add('WHERE ' + C_Tf_DDfuePE_Befehl + ' = :Befehl');
        Sql.Add('AND ' + C_Tf_DDfuePE_ParaAdr + ' = :ParaAdr');
        ParamByName('StaAktu').asBoolean:= DfueParaSendenData.StaAktu;
        ParamByName('Aendern').asBoolean:= true;
        ParamByName('Befehl').asString:= DfueParaSendenData.Befehl;
        ParamByName('ParaAdr').asString:= DfueParaSendenData.ParaAdr;
        ExecSql;
      end; // with q
    finally
      q.free;
    end;
  end;
end;

{ Eintrag für zu übertragenden DSfG-DFÜ-Parameter aus Tabelle lesen.                    }
{ Rückgabe: Befehl, ParaAdr, neuer Wert und Stellenzahl des zu übertragenden Parameters }
{ Ergebnis: false, wenn kein Eintrag mit neuem Wert mehr vorhanden             }
{------------------------------------------------------------------------------------------------------------}
function TTbDSfGDfueParametrierung.GetSendeParameter (var DfueParaSendenData: DfueParaSendenDataRec): boolean;
{------------------------------------------------------------------------------------------------------------}
begin
  Result:=false;
  with DfueParaSendenData do begin
    Befehl:='';
    ParaAdr:='';
    Wert:='';
    Stellen:=-1;
    StaAktu:=false;
  end;
  if tbDfuePe.Exists then begin
    if tbDfuePe.OpenShared then begin
      try
        { Filter auf noch nicht bearbeitete Einträge: }
        tbDfuePe.Filtered:=false;                                     { Deaktivieren des Filters }
        tbDfuePe.Filter:=C_Tf_DDfuePE_Fertig + ' = ''FALSE''';
        tbDfuePe.Filtered:=true;                                        { Aktivieren des Filters }
        while not tbDfuePe.Eof do begin
          { nur Datensätze der zu ändernden Parameter lesen: }
          if tbDfuePe.FieldByName (C_Tf_DDfuePE_Aendern).AsBoolean then begin
            with DfueParaSendenData do begin
              Befehl:=tbDfuePe.FieldByName (C_Tf_DDfuePE_Befehl).AsString;
              ParaAdr:=tbDfuePe.FieldByName (C_Tf_DDfuePE_ParaAdr).AsString;
              Wert:=tbDfuePe.FieldByName (C_Tf_DDfuePE_WertNeu).AsString;
              Stellen:=tbDfuePe.FieldByName (C_Tf_DDfuePE_Stellen).AsInteger;
              StaAktu:=tbDfuePe.FieldByName (C_Tf_DDfuePE_StaAktu).AsBoolean;
            end;
            tbDfuePe.Edit;
            { Vorbelegung, falls Parameter nicht übertragen werden kann: }
            { Achtung: Der Alt-Wert wird bei NTY-Masken-Änderungen (A-Befehl) nicht gelöscht,
                       da hier in der Antwort nur der neue Wert, nicht aber (wie bei den
                       übrigen Befehlen) der Alt-Wert enthalten ist. Das Feld "WertAlt"
                       muß in diesem Fall als "Soll-Neu-Wert" interpretiert werden ! }
            if DfueParaSendenData.Befehl <> 'A' then
              tbDfuePe.FieldByName (C_Tf_DDfuePE_WertAlt).AsString:='';
            tbDfuePe.FieldByName (C_Tf_DDfuePE_WertNeu).AsString:='';
            { Kennzeichnung für Abrufmodul, daß Parameter bearbeitet wurde: }
            tbDfuePe.FieldByName (C_Tf_DDfuePE_Fertig).AsBoolean:=true;
            tbDfuePe.Post;
            Result:=true;
            Break;
          end;
          tbDfuePe.Next;
        end;
        tbDfuePe.Filtered:=false;                                     { Deaktivieren des Filters }
      finally
        tbDfuePe.Close;
      end;
    end;
  end;
end;

{ Ergebnis der Übertragung eines DSfG-DFÜ-Parameters in Tabelle schreiben              }
{ Parameter: Befehl, Parameter-Adresse, alter und neuer Wert des übertragenen Parameters         }
{ Achtung: Wenn für einen Eintrag in der Ergebnistabelle der alte und neue Parameterwert
           fehlen oder gleich sind, gilt: Der Parameter konnte nicht übertragen werden ! }
{------------------------------------------------------------------------------------------------}
procedure TTbDSfGDfueParametrierung.WriteErgebnis (DfueParaErgebnisData: DfueParaErgebnisDataRec);
{------------------------------------------------------------------------------------------------}
begin
  if tbDfuePe.Exists then begin
    if tbDfuePe.OpenShared then begin
      try
        with DfueParaErgebnisData do begin
          if tbDfuePe.FindKey ([Befehl, ParaAdr]) then begin
            tbDfuePe.Edit;
            tbDfuePe.FieldByName (C_Tf_DDfuePE_WertAlt).AsString:=ParaWertAlt;
            tbDfuePe.FieldByName (C_Tf_DDfuePE_WertNeu).AsString:=ParaWertNeu;
            tbDfuePe.Post;
          end;
        end;
      finally
        tbDfuePe.Close;
      end;
    end;
  end;
end;

{ Ergebnis der Übertragung mehrerer Parameter aus Tabelle lesen und in Query zurückgeben }
{ Parameter: Query (Befehl, Parameter-Adresse, alter und neuer Wert)  }
{ Ergebnis: true, wenn Ergebnis der Übertragung gelesen werden konnte }
{------------------------------------------------------------------------------------}
function TTbDSfGDfueParametrierung.GetAllParameterChanges (aQuery: TQueryExt): boolean;
{------------------------------------------------------------------------------------}
begin
  Result:=false;
  if tbDfuePe.Exists then begin
    aQuery.Close;
    aQuery.DatabaseName:= Path;
    with aQuery do begin
      { Ergebnisse für bearbeitete Parameter holen }
      SQL.Add('SELECT ' + C_Tf_DDfuePE_Befehl + ', ' + C_Tf_DDfuePE_ParaAdr + ', ');
      SQL.Add(C_Tf_DDfuePE_WertAlt + ', ' + C_Tf_DDfuePE_WertNeu);
      SQL.Add('FROM "' + tbDfuePe.TableName + '"');
      SQL.Add('WHERE ' + C_Tf_DDfuePE_Fertig + ' = :Fertig');
      ParamByName('Fertig').asBoolean:= true;
    end;
    if aQuery.Open then
      if aQuery.RecordCount > 0 then result:= true;
  end;
end;

{ Ergebnis der Übertragung mehrerer Parameter aus Tabelle lesen und in Liste zurückgeben }
{ Parameter: Liste (Befehl, Parameter-Adresse, alter und neuer Wert)  }
{ Ergebnis: true, wenn Ergebnis der Übertragung gelesen werden konnte }
{---------------------------------------------------------------------------------------------}
function TTbDSfGDfueParametrierung.GetAllParameterChangesByList (aList: TDDfuePEList): boolean;
{---------------------------------------------------------------------------------------------}
var
  q: TQueryExt;
  DDfuePEListObj: TDDfuePEListObj;

begin
  Result:=false;
  aList.Clear;   { Liste leeren }

  q:=TQueryExt.Create (nil);
  try
    if GetAllParameterChanges (q) then begin     { Ergebnis aller Parametrierungen aus Tabelle lesen }
      q.first;                                   { liegt nach Befehl und Parameter-Adresse sortiert vor }
      while not q.eof do begin
        DDfuePEListObj:=TDDfuePEListObj.Create;
        DDfuePEListObj.SetData (q.fieldbyname(C_Tf_DDfuePE_Befehl).AsString,
                                q.fieldbyname(C_Tf_DDfuePE_ParaAdr).AsString,
                                q.FieldByName(C_Tf_DDfuePE_WertAlt).asString,
                                q.FieldByName(C_Tf_DDfuePE_WertNeu).asString,
                                0, false, false, true);
        aList.Add (DDfuePEListObj);
        q.next;
      end;  { while not q.eof }
      Result:=true;
    end;  { if GetAllParameterChanges }
  finally
    q.Free;
  end;
end;

end.
