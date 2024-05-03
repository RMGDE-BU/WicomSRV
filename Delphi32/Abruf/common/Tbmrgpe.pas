{********************************************************************************}
{* Unit: Zugriff auf MRG-Parametrierungstabelle                                 *}
{* 07.10.1999 WW                                                                *}
{********************************************************************************}
unit TbMrgPE;

interface
                      
uses
  Forms, SysUtils, Dialogs,WTables, Db_Attn, Db, DbTables, WSysCon, MP_Allg;

const
  { Tabelle für Parametereinstellen }
  C_TbMPE                = 'MPE';

  C_TfMPE_ParaNr         = 'ParameterNr';      { int }
  C_TfMPE_ParaWertAlt    = 'ParameterWertAlt'; { str24 }
  C_TfMPE_ParaWertNeu    = 'ParameterWertNeu'; { str24 }
  C_TfMPE_Stellen        = 'Stellen';          { smallint }
  C_TfMPE_StaAktu        = 'StaAktu';          { log }
  C_TfMPE_Fertig         = 'Fertig';           { log }


type

  ParaSendenDataRec = record
    ParaNr: string [9];
    ParaWert: string [24];
    Stellen: integer;
    StaAktu: boolean;
  end;

  ParaErgebnisDataRec = record
    ParaNr: string [9];
    ParaWertAlt: string [24];
    ParaWertNeu: string [24];
  end;

  { Objekt für Tabelle zur Parametrierung des MRG }

  TTbMRGParametrierung = class(TObject)
  private
    Path         : TFileName;
    tbMPE        : TTableExt;
    MRGId       : integer;
    function CreatetbMPE: boolean;
  public
    constructor Create (APath: TFileName; AMRGId: integer);
    destructor Destroy; override;
    procedure CopyFromMomTable;
    function GetSendeParameter (var ParaSendenData: ParaSendenDataRec): boolean;
    procedure SetSendeparameter(ParaSendenData: ParaSendenDataRec);
    procedure WriteErgebnis (ParaErgebnisData: ParaErgebnisDataRec);
    function ParameterChangeOk (AP9Nr: string; var AltWert: string; var NeuWert: string): boolean;
    function AllParameterChangesOk (aQuery: TQueryExt): boolean;
    function GetParameter(aQuery: TQueryExt): boolean;
  end;

implementation

{ TTbMRGParametrierung }

{--------------------------------------------------------------------------}
constructor TTbMRGParametrierung.Create (APath: TFileName; AMRGId: integer);
{--------------------------------------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  MRGId:=AMRGId;
  tbMPE:=TTableExt.Create(nil);
  tbMPE.DataBaseName:= Path;
  tbMPE.TableName:= C_tbMPE + Format('%.4d.DB', [MRGId]);
end;

{---------------------------------------}
destructor TTbMRGParametrierung.Destroy;
{---------------------------------------}
begin
  tbMPE.Free;
  inherited Destroy;
end;

{-------------------------------------------------}
function TTbMRGParametrierung.CreatetbMPE: boolean;
{-------------------------------------------------}
{ Tabelle anlegen }
begin
  with tbMPE.FieldDefs do begin
    Clear;
    Add(C_TfMPE_ParaNr, ftString, 9, false);
    Add(C_TfMPE_ParaWertAlt, ftString, 24, false);
    Add(C_TfMPE_ParaWertNeu, ftString, 24, false);
    Add(C_TfMPE_Stellen, ftSmallInt, 0, false);
    Add(C_TfMPE_StaAktu, ftBoolean, 0, false);
    Add(C_TfMPE_Fertig, ftBoolean, 0, false);
  end;
  with tbMPE.IndexDefs do begin
    Clear;
    Add('Primaerindex', C_TfMPE_ParaNr, [ixPrimary, ixUnique]);
  end;
  Result:=tbMPE.CreateTable;
end;

{----------------------------------------------}
procedure TTbMRGParametrierung.CopyFromMomTable;
{----------------------------------------------}
{ Parameter aus der Momentanwerttabelle in die Parametrierungstabelle schreiben }
var
  MomTable: TTableExt;
  Nr: string;
  WertStr: string;
  Stellen: integer;
  Ok: boolean;

begin
  if not tbMPE.Exists then                            { Zieltabelle: Parametrierungstabelle }
    Ok:=CreatetbMPE
  else
    Ok:=tbMPE.EmptyTable;

  if Ok then begin
    MomTable:=TTableExt.Create (nil);                     { Quelltabelle: Momentanwerttabelle }
    try
      MomTable.DatabaseName:=Path;
      MomTable.TableName:=CDBMMom + Format('%.4d.DB', [MrgId]);      { MrgId im Tabellennamen }
      if MomTable.Exists then begin
        if MomTable.OpenShared then begin
          try
            if tbMPE.OpenShared then begin
              try
                while not MomTable.Eof do begin
                  Nr:=MomTable.FieldByName (C_Mom_Parameternummer).AsString;
                  WertStr:=MomTable.FieldByName (C_Mom_Wert).AsString;
                  Stellen:=MomTable.FieldByName (C_Mom_Stellen).AsInteger;
                  { Nr, Wert, Stellen und Vorbelegung für "Fertig" in Tabelle schreiben: }
                  tbMPE.InsertRecord ([Nr, WertStr, nil, Stellen, nil, false]);
                  Application.ProcessMessages;
                  MomTable.Next;
                end;
              finally
                tbMPE.Close;
              end;
              { Triggerfile schreiben, wenn alle Parameter in Tabelle konvertiert wurden: }
              WriteNewTime(tbMPE.DatabaseName + tbMPE.TableName);
            end;
          finally
            MomTable.Close;
          end;
        end;
      end; { if MomTable.Exists }
    finally
      MomTable.Free;
    end;
  end;
end;

{ Schreibt einen Record in die Parametrierungstabelle MPExxxx                  }
{----------------------------------------------------------------------------------}
procedure TTbMRGParametrierung.SetSendeparameter(ParaSendenData: ParaSendenDataRec);
{----------------------------------------------------------------------------------}
Var
  q: TQueryExt;
begin
  if tbMPE.Exists then begin
    q:=TQueryExt.Create(nil);
    try
      with q do begin
        DatabaseName:=Path;
        Sql.Clear;
        Sql.Add('UPDATE "' + tbMPE.TableName + '"');
        Sql.Add('SET ' + C_TfMPE_ParaWertNeu + ' = "' + ParaSendenData.ParaWert + '", ');
        Sql.Add(C_TfMPE_Stellen + ' = ' + IntToStr(ParaSendenData.Stellen)  + ', ');
        Sql.Add(C_TfMPE_StaAktu + ' = :StaAktu');
        Sql.Add('WHERE ' + C_TfMPE_ParaNr + ' = :pNr');
        Sql.Add('AND ' + C_TfMPE_ParaWertAlt + ' <> "' + ParaSendenData.ParaWert + '"');
        ParamByName('StaAktu').asBoolean:= ParaSendenData.StaAktu;
        ParamByName('pNr').asString:= ParaSendenData.ParaNr;
        ExecSql;
      end; // with q
    finally
      q.free;
    end;
  end;
end;


{ Eintrag für zu übertragenden Parameter aus Tabelle lesen.                    }
{ Rückgabe: Nummer, neuer Wert und Stellenzahl des zu übertragenden Parameters }
{ Ergebnis: false, wenn kein Eintrag mit neuem Wert mehr vorhanden             }
{-----------------------------------------------------------------------------------------------}
function TTbMRGParametrierung.GetSendeParameter (var ParaSendenData: ParaSendenDataRec): boolean;
{-----------------------------------------------------------------------------------------------}
var
  WertNeu: string;
begin
  Result:=false;
  with ParaSendenData do begin
    ParaNr:='';
    Parawert:='';
    Stellen:=-1;
    StaAktu:=false;
  end;
  if tbMPE.Exists then begin
    if tbMPE.OpenShared then begin
      try
        { Filter auf noch nicht bearbeitete Einträge: }
        tbMPE.Filtered:=false;                                     { Deaktivieren des Filters }
        tbMPE.Filter:=C_TfMPE_Fertig + ' = ''FALSE''';
        tbMPE.Filtered:=true;                                        { Aktivieren des Filters }
        while not tbMPE.Eof do begin
          WertNeu:=tbMPE.FieldByName (C_TfMPE_ParaWertNeu).AsString;
          if WertNeu <> '' then begin               { Daten des zu sendenden Parameters lesen }
            with ParaSendenData do begin
              ParaNr:=tbMPE.FieldByName (C_TfMPE_ParaNr).AsString;
              ParaWert:=WertNeu;
              Stellen:=tbMPE.FieldByName (C_TfMPE_Stellen).AsInteger;
              StaAktu:=tbMPE.FieldByName (C_TfMPE_StaAktu).AsBoolean;
            end;
            tbMPE.Edit;
            { Vorbelegung, falls Parameter nicht übertragen werden kann: }
            tbMPE.FieldByName (C_TfMPE_ParaWertAlt).AsString:='';
            tbMPE.FieldByName (C_TfMPE_ParaWertNeu).AsString:='';
            { Kennzeichnung für Abrufmodul, daß Parameter bearbeitet wurde: }
            tbMPE.FieldByName (C_TfMPE_Fertig).AsBoolean:=true;
            tbMPE.Post;
            Result:=true;
            Break;
          end;
          tbMPE.Next;
        end;
        tbMPE.Filtered:=false;                                     { Deaktivieren des Filters }
      finally
        tbMPE.Close;
      end;
    end;
  end;
end;

{ Ergebnis der Übertragung eines Parameters in Tabelle schreiben              }
{ Parameter: Nummer, alter und neuer Wert des übertragenen Parameters         }
{ Achtung: Wenn für einen Eintrag in der Ergebnistabelle der alte und neue Parameterwert
           fehlen oder gleich sind, gilt: Der Parameter konnte nicht übertragen werden ! }
{-----------------------------------------------------------------------------------}
procedure TTbMRGParametrierung.WriteErgebnis (ParaErgebnisData: ParaErgebnisDataRec);
{-----------------------------------------------------------------------------------}
begin
  if tbMPE.Exists then begin
    if tbMPE.OpenShared then begin
      try
        with ParaErgebnisData do begin
          if tbMPE.FindKey ([ParaNr]) then begin
            tbMPE.Edit;
            tbMPE.FieldByName (C_TfMPE_ParaWertAlt).AsString:=ParaWertAlt;
            tbMPE.FieldByName (C_TfMPE_ParaWertNeu).AsString:=ParaWertNeu;
            tbMPE.Post;
          end;
        end;
      finally
        tbMPE.Close;
      end;
    end;
  end;
end;

{ Ergebnis der Übertragung eines Parameters aus Tabelle lesen         }
{ Parameter: Nummer des Parameters                                    }
{ Rückgabe: alter und neuer Wert                                      }
{ Ergebnis: true, wenn Ergebnis der Übertragung gelesen werden konnte }
{--------------------------------------------------------------------------------------------------}
function TTbMRGParametrierung.ParameterChangeOk (AP9Nr: string;
                                                 var AltWert: string; var NeuWert: string): boolean;
{--------------------------------------------------------------------------------------------------}
begin
  Result:=false;
  AltWert:='';
  NeuWert:='';
  if tbMPE.Exists then begin
    with TQueryExt.Create(nil) do begin
      try
        DataBaseName:= Path;
        { Ergebnis für bearbeiteten Parameter holen }
        Sql.Add('SELECT ' + C_TfMPE_ParaWertAlt + ', ' + C_TfMPE_ParaWertNeu);
        Sql.Add('FROM "' + tbMPE.TableName + '"');
        Sql.Add('WHERE ' + C_TfMPE_ParaNr + ' = :pNr');
        ParamByName('pNr').asString:= AP9Nr;
        if Open then begin
          if RecordCount > 0 then begin
            AltWert:=FieldByName(C_TfMPE_ParaWertAlt).asString;
            NeuWert:=FieldByName(C_TfMPE_ParaWertNeu).asString;
            Result:=true;
          end;
        end;
        Close;
      finally
        Free;
      end;
    end;
  end;
end;

{ Ergebnis der Übertragung mehrerer Parameter aus Tabelle lesen       }
{ Parameter: Query (Parameternummer, alter und neuer Wert)            }
{ Ergebnis: true, wenn Ergebnis der Übertragung gelesen werden konnte }
{-------------------------------------------------------------------------------}
function TTbMRGParametrierung.AllParameterChangesOk (aQuery: TQueryExt): boolean;
{-------------------------------------------------------------------------------}
begin
  Result:=false;
  if tbMPE.Exists then begin
    aQuery.Close;
    aQuery.DatabaseName:= Path;
    with aQuery do begin
      { Ergebnisse für bearbeitete Parameter holen }
      SQL.Add('SELECT ' + C_TfMPE_ParaNr + ', ' + C_TfMPE_ParaWertAlt + ', ' + C_TfMPE_ParaWertNeu);
      SQL.Add('FROM "' + tbMPE.TableName + '"');
      SQL.Add('WHERE ' + C_TfMPE_Fertig + ' = :Fertig');
      ParamByName('Fertig').asBoolean:= true;
    end;
    if aQuery.Open then
      if aQuery.RecordCount > 0 then result:= true;
  end;
end;

{ Gibt Parameter der Parametrierungstabelle im Query zurück           }
{ Ergebnis: false, wenn keine Parameter vorhanden                     }
{---------------------------------------------------------------------}
function TTbMRGParametrierung.GetParameter(aQuery: TQueryExt): boolean;
{---------------------------------------------------------------------}
begin
  result:= False; { default }
  if tbMPE.Exists then begin
    aQuery.Close;
    aQuery.DatabaseName:= Path;
    with aQuery.Sql do begin
      Clear;
      Add('SELECT ' + C_TfMPE_ParaNr + ', ' + C_TfMPE_ParaWertAlt + ', ' + C_TfMPE_Stellen);
      Add('FROM "' + tbMPE.TableName + '"');
    end;
    if aQuery.Open then
      if aQuery.RecordCount > 0 then result:= true;
  end;
end;

end.
