{******************************************************************************}
{* Unit: Zugriff auf DSfG-Parametrierungstabelle                              *}
{* 28.09.2000 WW                                                              *}
{* 18.10.2000 GD; Schnittstellen zum DSfGParaClient                           *}
{******************************************************************************}
unit TbDSfGDE;

interface

uses
  Forms, SysUtils, Dialogs, WTables, Db_Attn, Db, DbTables, WSysCon, Classes;

const
  { Tabelle für DSfG-Datenelemente einstellen }
  C_TbDDE              = 'DDE';

  C_TfDDE_EADR         = 'EADR';         { str1 }
  C_TfDDE_DEL_Adr      = 'DEL_Adr';      { str10 }
  C_TfDDE_WertNeu_Soll = 'WertNeu_Soll'; { str20 }
  C_TfDDE_WertNeu_Ist  = 'WertNeu_Ist';  { str20 }
  C_TfDDE_Zugangscode1 = 'Zugangscode1'; { str10 }
  C_TfDDE_Zugangscode2 = 'Zugangscode2'; { str10 }
  C_TfDDE_Stellen      = 'Stellen';      { smallint }
  C_TfDDE_StaAktu      = 'StaAktu';      { log }
  C_TfDDE_Fertig       = 'Fertig';       { log }

  
type

  ParaSendenDataRec = record
    EADR: string [1];
    DEL_Adr: string [10];
    Wert: string [40];               // 06.03.2002 WW, erweitert von 20 auf 40
    Zugangscode1: string [10];
    Zugangscode2: string [10];
    Stellen: integer;
    StaAktu: boolean;
  end;

  ParaErgebnisDataRec = record
    EADR: string [1];
    DEL_Adr: string [10];
    WertNeu_Soll: string [40];      // 06.03.2002 WW, erweitert von 20 auf 40
    WertNeu_Ist: string [40];       // 06.03.2002 WW, erweitert von 20 auf 40
  end;
  PtrParaErgebnisDataRec = ^ParaErgebnisDataRec;  // 18.10.2000

  { Objekt für Tabelle zur Parametrierung von DSfG-Instanzen }

  TTbDSfGParametrierung = class(TObject)
  private
    Path: TFileName;
    tbDDE: TTableExt;
    StationId: integer;
    function CreatetbDDE: boolean;
  public
    constructor Create (APath: TFileName; AStationId: integer);
    destructor Destroy; override;
    procedure DeleteTable;
    procedure InsertToParaTable(cAdr: char;
       sDEA, sWAlt, sWNeu, sZgCode1, sZgCode2: string;
      iStellen: integer; bAktu: boolean = False; bFertig: boolean = False);
//    procedure CopyFromMomTable;
    function GetSendeParameter (var ParaSendenData: ParaSendenDataRec): boolean;
//    procedure SetSendeparameter(ParaSendenData: ParaSendenDataRec);
    procedure WriteErgebnis (ParaErgebnisData: ParaErgebnisDataRec);
    function GetParamValueList: TStrings;          // 18.10.2000
//    function ParameterChangeOk (AP9Nr: string; var AltWert: string; var NeuWert: string): boolean;
//    function AllParameterChangesOk (aQuery: TQueryExt): boolean;
//    function GetParameter(aQuery: TQueryExt): boolean;
  end;

implementation

{ TTbDSfGParametrierung }

{-------------------------------------------------------------------------------}
constructor TTbDSfGParametrierung.Create (APath: TFileName; AStationId: integer);
{-------------------------------------------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  StationId:=AStationId;
  tbDDE:=TTableExt.Create(nil);
  tbDDE.DataBaseName:= Path;
  tbDDE.TableName:= C_tbDDE + Format('%.4d.DB', [StationId]);
  CreatetbDDE;  // 18.10.2000
end;

{---------------------------------------}
destructor TTbDSfGParametrierung.Destroy;
{---------------------------------------}
begin
  tbDDE.Free;
  inherited Destroy;
end;

{ Erzeugt Parametrierungstabelle                  }
{ Rückgabe Tabelle jetzt vorhanden: Ja/Nein       }
{-------------------------------------------------}
function TTbDSfGParametrierung.CreatetbDDE: boolean; // 18.10.2000
{-------------------------------------------------}
begin
  if (not tbDDE.Exists) then begin
    with tbDDE.FieldDefs do begin
      Clear;
      Add(C_TfDDE_EADR, ftString, 1, false);
      Add(C_TfDDE_DEL_Adr, ftString, 10, false);
      Add(C_TfDDE_WertNeu_Soll, ftString, 40, false);   // 06.03.2002 WW, erweitert von 20 auf 40
      Add(C_TfDDE_WertNeu_Ist, ftString, 40, false);    // 06.03.2002 WW, erweitert von 20 auf 40
      Add(C_TfDDE_Zugangscode1, ftString, 10, false);
      Add(C_TfDDE_Zugangscode2, ftString, 10, false);
      Add(C_TfDDE_Stellen, ftSmallInt, 0, false);
      Add(C_TfDDE_StaAktu, ftBoolean, 0, false);
      Add(C_TfDDE_Fertig, ftBoolean, 0, false);
    end;
    with tbDDE.IndexDefs do begin
      Clear;
      Add('Primaerindex', C_TfDDE_EADR+';'+C_TfDDE_DEL_Adr, [ixPrimary, ixUnique]);
    end;
    Result:=tbDDE.CreateTable;
  end
  else Result := True;
end;

{ Schreibt Tablleneintrag                      }
{----------------------------------------------}
procedure TTbDSfGParametrierung.InsertToParaTable(cAdr: char;
  sDEA, sWAlt, sWNeu, sZgCode1, sZgCode2: string;
  iStellen: integer; bAktu: boolean = False; bFertig: boolean = False);
{----------------------------------------------}
begin
  if (not tbDDE.Active) then tbDDE.OpenShared;
  with tbDDE do
  try
    if (not FindKey([cAdr, sDEA])) then begin
      Append;
      FieldByName(C_TfDDE_EADR).asString := cAdr;
      FieldByName(C_TfDDE_DEL_Adr).asString := sDEA;
      FieldByName(C_TfDDE_WertNeu_Soll).asString := sWAlt;
      FieldByName(C_TfDDE_WertNeu_Ist).asString := sWNeu;
      FieldByName(C_TfDDE_Zugangscode1).asString := sZgCode1;
      FieldByName(C_TfDDE_Zugangscode2).asString := sZgCode2;
      FieldByName(C_TfDDE_Stellen).asInteger := iStellen;
      FieldByName(C_TfDDE_StaAktu).asBoolean := bAktu;
      FieldByName(C_TfDDE_Fertig).asBoolean := bFertig;
      Post;
    end;
  finally
    Close;
  end;
end;

{ Löscht die Tabelle                                               }
{------------------------------------------------------------------}
procedure TTbDSfGParametrierung.DeleteTable;                       // 18.10.2000
{------------------------------------------------------------------}
begin
  if (tbDDE.Active) then tbDDE.Close;
  if (tbDDE.Exists) then tbDDE.DeleteTable;
end;

(*
{----------------------------------------------}
procedure TTbDSfGParametrierung.CopyFromMomTable;
{----------------------------------------------}
{ Parameter aus der Momentanwerttabelle in die Parametrierungstabelle schreiben }
var
  MomTable: TTableExt;
  Nr: string;
  WertStr: string;
  Stellen: integer;
  Ok: boolean;

begin
  if not tbDDE.Exists then                            { Zieltabelle: Parametrierungstabelle }
    Ok:=CreatetbDDE
  else
    Ok:=tbDDE.EmptyTable;

  if Ok then begin
    MomTable:=TTableExt.Create (nil);                     { Quelltabelle: Momentanwerttabelle }
    try
      MomTable.DatabaseName:=Path;
      MomTable.TableName:=CDBMMom + Format('%.4d.DB', [StationId]);      { StationId im Tabellennamen }
      if MomTable.Exists then begin
        if MomTable.OpenShared then begin
          try
            if tbDDE.OpenShared then begin
              try
                while not MomTable.Eof do begin
                  Nr:=MomTable.FieldByName (C_Mom_Parameternummer).AsString;
                  WertStr:=MomTable.FieldByName (C_Mom_Wert).AsString;
                  Stellen:=MomTable.FieldByName (C_Mom_Stellen).AsInteger;
                  { Nr, Wert, Stellen und Vorbelegung für "Fertig" in Tabelle schreiben: }
                  tbDDE.InsertRecord ([Nr, WertStr, nil, Stellen, nil, false]);
                  Application.ProcessMessages;
                  MomTable.Next;
                end;
              finally
                tbDDE.Close;
              end;
              { Triggerfile schreiben, wenn alle Parameter in Tabelle konvertiert wurden: }
              WriteNewTime(tbDDE.DatabaseName + tbDDE.TableName);
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

{ Schreibt einen Record in die Parametrierungstabelle DDExxxx                  }
{----------------------------------------------------------------------------------}
procedure TTbDSfGParametrierung.SetSendeparameter(ParaSendenData: ParaSendenDataRec);
{----------------------------------------------------------------------------------}
Var
  q: TQueryExt;
begin
  if tbDDE.Exists then begin
    q:=TQueryExt.Create(nil);
    try
      with q do begin
        DatabaseName:=Path;
        Sql.Clear;
        Sql.Add('UPDATE "' + tbDDE.TableName + '"');
        Sql.Add('SET ' + C_TfDDE_ParaWertNeu + ' = "' + ParaSendenData.ParaWert + '", ');
        Sql.Add(C_TfDDE_Stellen + ' = ' + IntToStr(ParaSendenData.Stellen)  + ', ');
        Sql.Add(C_TfDDE_StaAktu + ' = :StaAktu');
        Sql.Add('WHERE ' + C_TfDDE_ParaNr + ' = :pNr');
        Sql.Add('AND ' + C_TfDDE_ParaWertAlt + ' <> "' + ParaSendenData.ParaWert + '"');
        ParamByName('StaAktu').asBoolean:= ParaSendenData.StaAktu;
        ParamByName('pNr').asString:= ParaSendenData.ParaNr;
        ExecSql;
      end; // with q
    finally
      q.free;
    end;
  end;
end;  *)


{ Eintrag für zu übertragenden Parameter (DE) aus Tabelle lesen.               }
{ Rückgabe: ParaSendenDataRec mit Daten des zu übertragenden Parameters        }
{ Ergebnis: false, wenn kein Eintrag mit neuem Wert mehr vorhanden             }
{------------------------------------------------------------------------------------------------}
function TTbDSfGParametrierung.GetSendeParameter (var ParaSendenData: ParaSendenDataRec): boolean;
{------------------------------------------------------------------------------------------------}
var
  WertNeu: string;
begin
  Result:=false;
  with ParaSendenData do begin
    EADR:='';
    DEL_Adr:='';
    Wert:='';
    Zugangscode1:='';
    Zugangscode2:='';
    Stellen:=-1;
    StaAktu:=false;
  end;
  if tbDDE.Exists then begin
    if tbDDE.OpenShared then begin
      try
        { Filter auf noch nicht bearbeitete Einträge: }
        tbDDE.Filtered:=false;                                     { Deaktivieren des Filters }
        tbDDE.Filter:=C_TfDDE_Fertig + ' = ''FALSE''';
        tbDDE.Filtered:=true;                                        { Aktivieren des Filters }
        while not tbDDE.Eof do begin
          WertNeu:=tbDDE.FieldByName (C_TfDDE_WertNeu_Soll).AsString;
          if WertNeu <> '' then begin               { Daten des zu sendenden Parameters lesen }
            with ParaSendenData do begin
              EADR:=tbDDE.FieldByName (C_TfDDE_EADR).AsString;
              DEL_Adr:=tbDDE.FieldByName (C_TfDDE_DEL_Adr).AsString;
              Wert:=WertNeu;
              Zugangscode1:=tbDDE.FieldByName (C_TfDDE_Zugangscode1).AsString;
              Zugangscode2:=tbDDE.FieldByName (C_TfDDE_Zugangscode2).AsString;
              Stellen:=tbDDE.FieldByName (C_TfDDE_Stellen).AsInteger;
              StaAktu:=tbDDE.FieldByName (C_TfDDE_StaAktu).AsBoolean;
            end;
            tbDDE.Edit;
            { Vorbelegung, falls Parameter nicht übertragen werden kann: }
            tbDDE.FieldByName (C_TfDDE_WertNeu_Soll).AsString:='';
            tbDDE.FieldByName (C_TfDDE_WertNeu_Ist).AsString:='';
            { Kennzeichnung für Abrufmodul, daß Parameter bearbeitet wurde: }
            tbDDE.FieldByName (C_TfDDE_Fertig).AsBoolean:=true;
            tbDDE.Post;
            Result:=true;
            Break;
          end;
          tbDDE.Next;
        end;
        tbDDE.Filtered:=false;                                     { Deaktivieren des Filters }
      finally
        tbDDE.Close;
      end;
    end;
  end;
end;

{ Gibt Liste mit Werten zurück                     }
{ Rückgabe: Liste mit Werten ()                    }
{--------------------------------------------------}
function TTbDSfGParametrierung.GetParamValueList: TStrings;          // 18.10.2000
{--------------------------------------------------}
var
  p : ^ParaErgebnisDataRec;
begin
  Result := TStringList.Create;

  if (not tbDDE.Active) then
    if (not tbDDE.Exists) or (not tbDDE.OpenShared) then Exit;

  try
    tbDDE.First;
    while not tbDDE.Eof do begin
      New(p);
      p^.EADR := tbDDE.FieldByName(C_TfDDE_EADR).asString[1];
      p^.DEL_Adr := tbDDE.FieldByName(C_TfDDE_DEL_Adr).asString;
      p^.WertNeu_Soll := tbDDE.FieldByName(C_TfDDE_WertNeu_Soll).asString;
      p^.WertNeu_Ist := tbDDE.FieldByName(C_TfDDE_WertNeu_Ist).asString;
      Result.AddObject(p^.EADR + p^.DEL_Adr, TObject(p));    
      tbDDE.Next;
    end;
  finally
    tbDDE.Close;
  end;
end;

{ Ergebnis der Übertragung eines Parameters in Tabelle schreiben              }
{ Parameter: ParaErgebnisDataRec mit den Daten des übertragenen Parameters    }
{ Achtung: Wenn für einen Eintrag in der Ergebnistabelle der neue Soll- und Ist-Parameterwert
           fehlen oder gleich sind, gilt: Der Parameter konnte nicht übertragen werden ! }
{------------------------------------------------------------------------------------}
procedure TTbDSfGParametrierung.WriteErgebnis (ParaErgebnisData: ParaErgebnisDataRec);
{------------------------------------------------------------------------------------}
begin
  if tbDDE.Exists then begin
    if tbDDE.OpenShared then begin
      try
        with ParaErgebnisData do begin
          if tbDDE.FindKey ([EADR, DEL_Adr]) then begin
            tbDDE.Edit;
            tbDDE.FieldByName (C_TfDDE_WertNeu_Soll).AsString:=WertNeu_Soll;
            tbDDE.FieldByName (C_TfDDE_WertNeu_Ist).AsString:=WertNeu_Ist;
            tbDDE.Post;
          end;
        end;
      finally
        tbDDE.Close;
      end;
    end;
  end;
end;
(*
{ Ergebnis der Übertragung eines Parameters aus Tabelle lesen         }
{ Parameter: Nummer des Parameters                                    }
{ Rückgabe: alter und neuer Wert                                      }
{ Ergebnis: true, wenn Ergebnis der Übertragung gelesen werden konnte }
{--------------------------------------------------------------------------------------------------}
function TTbDSfGParametrierung.ParameterChangeOk (AP9Nr: string;
                                                 var AltWert: string; var NeuWert: string): boolean;
{--------------------------------------------------------------------------------------------------}
begin
  Result:=false;
  AltWert:='';
  NeuWert:='';
  if tbDDE.Exists then begin
    with TQueryExt.Create(nil) do begin
      try
        DataBaseName:= Path;
        { Ergebnis für bearbeiteten Parameter holen }
        Sql.Add('SELECT ' + C_TfDDE_ParaWertAlt + ', ' + C_TfDDE_ParaWertNeu);
        Sql.Add('FROM "' + tbDDE.TableName + '"');
        Sql.Add('WHERE ' + C_TfDDE_ParaNr + ' = :pNr');
        ParamByName('pNr').asString:= AP9Nr;
        if Open then begin
          if RecordCount > 0 then begin
            AltWert:=FieldByName(C_TfDDE_ParaWertAlt).asString;
            NeuWert:=FieldByName(C_TfDDE_ParaWertNeu).asString;
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
function TTbDSfGParametrierung.AllParameterChangesOk (aQuery: TQueryExt): boolean;
{-------------------------------------------------------------------------------}
begin
  Result:=false;
  if tbDDE.Exists then begin
    aQuery.Close;
    aQuery.DatabaseName:= Path;
    with aQuery do begin
      { Ergebnisse für bearbeitete Parameter holen }
      SQL.Add('SELECT ' + C_TfDDE_ParaNr + ', ' + C_TfDDE_ParaWertAlt + ', ' + C_TfDDE_ParaWertNeu);
      SQL.Add('FROM "' + tbDDE.TableName + '"');
      SQL.Add('WHERE ' + C_TfDDE_Fertig + ' = :Fertig');
      ParamByName('Fertig').asBoolean:= true;
    end;
    if aQuery.Open then
      if aQuery.RecordCount > 0 then result:= true;
  end;
end;

{ Gibt Parameter der Parametrierungstabelle im Query zurück           }
{ Ergebnis: false, wenn keine Parameter vorhanden                     }
{---------------------------------------------------------------------}
function TTbDSfGParametrierung.GetParameter(aQuery: TQueryExt): boolean;
{---------------------------------------------------------------------}
begin
  result:= False; { default }
  if tbDDE.Exists then begin
    aQuery.Close;
    aQuery.DatabaseName:= Path;
    with aQuery.Sql do begin
      Clear;
      Add('SELECT ' + C_TfDDE_ParaNr + ', ' + C_TfDDE_ParaWertAlt + ', ' + C_TfDDE_Stellen);
      Add('FROM "' + tbDDE.TableName + '"');
    end;
    if aQuery.Open then
      if aQuery.RecordCount > 0 then result:= true;
  end;
end;
    *)
end.
