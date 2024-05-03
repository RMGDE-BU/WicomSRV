{******************************************************************************}
{* Unit: Zugriff auf DSfG-Momentanwerte-Tabellen                              *}
{* 04.10.2000 WW                                                              *}
{* 10.10.2000 GD  Erweitert um Client-Zugriffe                                *}
{* 02.11.2000 GD  Prüfung Transisolation-Level                                *}
{* 01.06.2001 GD  Value-Record in DListen übersiedelt                         *}
{* 21.03.2003 GD  FindKey vor Tabelleneintrag (Indexfehler)                   *}
{******************************************************************************}
unit TBDSFGMo;

interface

uses
  Windows, Forms, SysUtils, WTables, Db_Attn, Db, DbTables, DListen, WSysCon,
  Classes,
  PathIni;

const

  { Tabelle der für die Momentanwertdarstellung definierten DSfG-Datenelemente }
  C_TbDMD        = 'DMD';

  C_TfDMD_EADR   = 'EADR';      { str1 }
  C_TfDMD_DelVon = 'DelVon';    { str10 }
  C_TfDMD_DelBis = 'DelBis';    { str10 }


  { Tabelle für Momentanwerte }
  C_TbDMom         = 'DMom';

  C_TfDMom_EADR    = 'EADR';      { str1 }
  C_TfDMom_DEL_Adr = 'DEL_Adr';   { str10 }
  C_TfDMom_Wert    = 'Wert';      { str20 }
  C_TfDMom_Stellen = 'Stellen';   { smallint }


type
  TDMDInsertRec = record   // 10.10.2000 Record zum Einfügen von Aufrufen
    cIAdr    : string[1];
    sDEAVon  : string[10];
    sDEABis  : string[10];
  end;
  PDMDInsertRec = ^TDMDInsertRec;

  { Objekt für DSfG-Momentanwerte-Definitionstabelle }

  TTbDSfGMomentanDef = class(TObject)
  private
    Path: TFileName;
    StationId: integer;
    function CreatetbDMD: boolean;
  public
    tbDMD: TTableExt;
    constructor Create (APath: TFileName; AStationId: integer);
    destructor Destroy; override;
    procedure GetAbrufListe (Abrufliste: TAbrufList);
    procedure WriteNewDefTb(pSl: TStrings);      // 10.10.2000
    procedure WriteIntoDefTb(pSl: TStrings);     // 10.10.2000
    procedure DeleteTable;                       // 10.10.2000
  end;


  { Objekt für DSfG-Momentanwerte-Datentabelle }

  TTbDSfGMomentanDaten = class(TObject)
  private
    Path: TFileName;
    StationId: integer;
    function CreatetbDMom: boolean;
  public
    tbDMom: TTableExt;
    constructor Create (APath: TFileName; AStationId: integer);
    destructor Destroy; override;
    function OpenMomTable: boolean;
    procedure CloseMomTable;
    function EmptyMomTable: boolean;
    procedure WriteMomTable (EADR: string; DEL_Adr: string; Wert: string; Stellen: integer);
    procedure UpdateMomTable (EADR: string; DEL_Adr: string; Wert: string);
    function GetMomValueList: TStrings;          // 10.10.2000
  end;

//procedure ClearPDMDInsertRecList(pSl: TStrings);  // 10.10.2000
//procedure ClearPDMomValueRecList(pSl: TStrings);  // 10.10.2000

implementation

const
  GS  = #29;

{ Allgemeine Funktionen }

{ Leeren einer Liste mit PDMDInsertRec-Objekten                              }
{----------------------------------------------------------------------------}
procedure ClearPDMDInsertRecList(pSl: TStrings);
{----------------------------------------------------------------------------}
var
  i : integer;
begin
  if (Assigned(pSl)) then
  try
    for i := 0 to pSl.Count-1 do Dispose(PDMDInsertRec(pSl.Objects[i]));
  except
    pSl.Clear;
  end;
end;

{ Leeren einer Liste mit PDMomValueRec-Objekten                              }
{----------------------------------------------------------------------------}
procedure ClearPDMomValueRecList(pSl: TStrings);
{----------------------------------------------------------------------------}
var
  i : integer;
begin
  if (Assigned(pSl)) then
  try
    for i := 0 to pSl.Count-1 do Dispose(PDMomValueRec(pSl.Objects[i]));
  except
    pSl.Clear;
  end;
end;

{ TTbDSfGMomentanDef }

{----------------------------------------------------------------------------}
constructor TTbDSfGMomentanDef.Create (APath: TFileName; AStationId: integer);
{----------------------------------------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  StationId:=AStationId;
  tbDMD:=TTableExt.Create(nil);
  tbDMD.DataBaseName:= Path;
  tbDMD.TableName:= C_TbDMD + Format('%.4d.DB', [StationId]);
end;

{------------------------------------}
destructor TTbDSfGMomentanDef.Destroy;
{------------------------------------}
begin
  tbDMD.Free;
  inherited Destroy;
end;

{-----------------------------------------------}
function TTbDSfGMomentanDef.CreatetbDMD: boolean;
{-----------------------------------------------}
{ Tabelle anlegen }
begin
  if (not tbDMD.Exists) then begin  // 10.10.2000
    with tbDMD.FieldDefs do begin
      Clear;
      Add(C_TfDMD_EADR, ftString, 1, false);
      Add(C_TfDMD_DelVon, ftString, 10, false);
      Add(C_TfDMD_DelBis, ftString, 10, false);
    end;
    with tbDMD.IndexDefs do begin
      Clear;
      Add('Primaerindex', C_TfDMD_EADR+';'+C_TfDMD_DelVon+';'+C_TfDMD_DelBis, [ixPrimary, ixUnique]);
    end;
    Result:=tbDMD.CreateTable;
  end
  else Result := True;
end;

{ Löscht die Tabelle                                               }
{------------------------------------------------------------------}
procedure TTbDSfGMomentanDef.DeleteTable;                       // 10.10.2000
{------------------------------------------------------------------}
begin
  if (tbDMD.Active) then tbDMD.Close;
  if (tbDMD.Exists) then tbDMD.DeleteTable;
end;

{ Leert Tabelle und trägt Daten aus Liste ein                      }
{ Parameter: Liste mit Daten für Tabelle                           }
{------------------------------------------------------------------}
procedure TTbDSfGMomentanDef.WriteNewDefTb(pSl: TStrings);
{------------------------------------------------------------------}
begin
  CreatetbDMD;
  if (tbDMD.Active) then tbDMD.Close;
  tbDMD.EmptyTable;
  if (Assigned(pSl)) then WriteIntoDefTb(pSl);
end;

{------------------------------------------------------------------}
procedure TTbDSfGMomentanDef.WriteIntoDefTb(pSl: TStrings);
{------------------------------------------------------------------}
var
  i : integer;
  iWdh : integer;
  p : PDMDInsertRec;
begin
  CreatetbDMD;
  if (not tbDMD.Active) then tbDMD.OpenExclusive;
  try
    iWdh := 0;

    tbDMD.Database.TransIsolation := tiDirtyRead;  //  02.11.2000
    while (tbDMD.Database.InTransaction) do begin
      Inc(iWdh);
      if (iWdh > 500) then Exit;        // Fehler -> evtl. Exception auslösen ?
      Sleep(10);
    end;

    tbDMD.Database.StartTransaction;
    try
      for i := 0 to pSl.Count-1 do begin
        p := PDMDInsertRec(pSl.Objects[i]);    // 21.03.2003
        if (not tbDMD.FindKey([p^.cIAdr, p^.sDEAVon, p^.sDEABis])) then begin
          tbDMD.Append;
          tbDMD.FieldByName(C_TfDMD_EADR).asString := p^.cIAdr;
          tbDMD.FieldByName(C_TfDMD_DelVon).asString := p^.sDEAVon;
          tbDMD.FieldByName(C_TfDMD_DelBis).asString := p^.sDEABis;
          tbDMD.Post;
        end;
      end;
      tbDMD.Database.Commit;
    finally
      if (tbDMD.Database.InTransaction) then tbDMD.Database.Rollback;
    end;
  finally
    tbDMD.Close;
  end;
end;

{------------------------------------------------------------------}
procedure TTbDSfGMomentanDef.GetAbrufListe (Abrufliste: TAbrufList);
{------------------------------------------------------------------}
{ erzeugt Abrufliste aller in der Tabelle definierten Datenelemente;
  Rückgabe: Abrufliste }
var
  AbrufListObj: TAbrufListObj;
  Busadresse: string;
  Del_von: string;
  Del_bis: string;
  AddEintrag: boolean;
  i: integer;

begin
  if tbDMD.Exists then begin
    if tbDMD.OpenShared then begin
      try
        AbrufListe.Clear;
        while not tbDMD.EOF do begin
          Busadresse:=tbDMD.FieldByName(C_TfDMD_EADR).AsString;
          Del_von:=tbDMD.FieldByName(C_TfDMD_DelVon).AsString;
          Del_bis:=tbDMD.FieldByName(C_TfDMD_DelBis).AsString;
          AddEintrag:=true;                                                                { Vorbelegung: neuer Eintrag }
          if Del_bis = '' then begin                                                           { einzelnes Datenelement }
            for i:=0 to AbrufListe.Count - 1 do begin                     { Eintrag für Einzelelement schon vorhanden ? }
              if (TAbrufListObj (AbrufListe [i]).EAdr = BusAdresse) AND
                 (TAbrufListObj (AbrufListe [i]).Del_bis = '') then begin
                { Länge noch unter 8k-Grenze ? }
                if length (TAbrufListObj (AbrufListe [i]).Del_von) < CMaxDSfGTelegrammLaenge then begin
                  TAbrufListObj (AbrufListe [i]).Del_von:=TAbrufListObj (AbrufListe [i]).Del_von +
                                                          GS + Del_von;                              { Befehl erweitern }
                  AddEintrag:=false;
                  Break;
                end;
              end;
            end;  { for }
          end;
          if AddEintrag then begin                                                             { neuen Eintrag einfügen }
            AbrufListObj:=TAbrufListObj.Create;
            AbrufListObj.SetData (BusAdresse,
                                  -1, -1, -1, '', '', '',     { für Momentanwert-Datenelemente nicht relevant }
                                  Del_von, Del_bis,
                                  0, 0, -1, -1);              { für Momentanwert-Datenelemente nicht relevant }
            AbrufListe.Add (AbrufListObj);
          end;

          tbDMD.Next;
        end;
      finally
        tbDMD.Close;
      end;
    end;
  end;
end;


{ TTbDSfGMomentanDaten }

{------------------------------------------------------------------------------}
constructor TTbDSfGMomentanDaten.Create (APath: TFileName; AStationId: integer);
{------------------------------------------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  StationId:=AStationId;
  tbDMom:=TTableExt.Create(nil);
  tbDMom.DataBaseName:= Path;
  tbDMom.TableName:= C_TbDMom + Format('%.4d.DB', [StationId]);
end;

{--------------------------------------}
destructor TTbDSfGMomentanDaten.Destroy;
{--------------------------------------}
begin
  tbDMom.Free;
  inherited Destroy;
end;

{--------------------------------------------------}
function TTbDSfGMomentanDaten.CreatetbDMom: boolean;
{--------------------------------------------------}
{ Tabelle anlegen }
begin
  with tbDMom.FieldDefs do begin
    Clear;
    Add(C_TfDMom_EADR, ftString, 1, false);
    Add(C_TfDMom_DEL_Adr, ftString, 10, false);
    Add(C_TfDMom_Wert, ftString, 40, false);     // 06.03.2002 WW, erweitert von 20 auf 40
    Add(C_TfDMom_Stellen, ftSmallint, 0, false);
  end;
  with tbDMom.IndexDefs do begin
    Clear;
    Add('Primaerindex', C_TfDMom_EADR+';'+C_TfDMom_DEL_Adr, [ixPrimary, ixUnique]);
  end;
  Result:=tbDMom.CreateTable;
end;

{ Gibt Liste mit Werten zurück                     }
{ Rückgabe: Liste mit Werten ()                    }
{--------------------------------------------------}
function TTbDSfGMomentanDaten.GetMomValueList: TStrings;          // 10.10.2000
{--------------------------------------------------}
var
  p : PDMomValueRec;
begin
  Result := TStringList.Create;

  if (not tbDMom.Active) then
    if (not tbDMom.Exists) or (not tbDMom.OpenShared) then Exit;

  try
    tbDMom.First;
    while not tbDMom.Eof do begin
      New(p);
      p^.cIAdr := tbDMom.FieldByName(C_TfDMom_EADR).asString[1];
      p^.sDEA := tbDMom.FieldByName(C_TfDMom_DEL_Adr).asString;
      p^.sWert := tbDMom.FieldByName(C_TfDMom_Wert).asString;
      p^.iStellen := tbDMom.FieldByName(C_TfDMom_Stellen).asInteger;
      Result.AddObject(p^.cIAdr + p^.sDEA, TObject(p));
      tbDMom.Next;
    end;
  finally
    tbDMom.Close;
  end;
end;

{--------------------------------------------------}
function TTbDSfGMomentanDaten.OpenMomTable: boolean;
{--------------------------------------------------}
{ Momentanwert-Tabelle öffnen, falls nicht vorhanden zuvor neuanlegen;
  Ergebnis: true, wenn Tabelle geöffnet werden konnte }
begin
  if not tbDMom.Exists then
    if not CreatetbDMom then begin
      Result:=false;
      exit;                       { Tabelle neuanlegen }
    end;
  Result:=tbDMom.OpenShared;
end;

{-------------------------------------------}
Procedure TTbDSfGMomentanDaten.CloseMomTable;
{-------------------------------------------}
{ Momentanwert-Tabelle schließen }
begin
  tbDMom.Close;
end;

{---------------------------------------------------}
function TTbDSfGMomentanDaten.EmptyMomTable: boolean;
{---------------------------------------------------}
{ Momentanwert-Tabelle leeren;
  Ergebnis: true, wenn Tabelle geleert werden konnte }
begin
  if tbDMom.Exists then
    Result:=tbDMom.EmptyTable
  else
    Result:=true;
end;

{-----------------------------------------------------------------------------------------------------------}
procedure TTbDSfGMomentanDaten.WriteMomTable (EADR: string; DEL_Adr: string; Wert: string; Stellen: integer);
{-----------------------------------------------------------------------------------------------------------}
{ Datensatz in Momentanwert-Tabelle schreiben; Öffnen und schließen der Tabelle muß außerhalb der Prozedur
  vorgenommen werden !
  Übergabe: Busadresse
            DE-Adresse
            Wert
            Länge des Wert-Strings (abschließende Leerzeichen werden von der Tabelle ignoriert !)
  Ergebnis: true, wenn Datensatz geschrieben wurde }
begin
  if tbDMom.Active then begin
    if tbDMom.FindKey ([EADR, DEL_Adr]) then begin
      tbDMom.Edit;
      tbDMom.FieldByName(C_TfDMom_Wert).AsString:=Wert;
      if Stellen > -1 then
        tbDMom.FieldByName(C_TfDMom_Stellen).AsInteger:=Stellen;
      tbDMom.Post;
    end
    else begin
      if Stellen > -1 then
        tbDMom.InsertRecord([EADR, DEL_Adr, Wert, Stellen])
      else
        tbDMom.InsertRecord([EADR, DEL_Adr, Wert])
    end;
  end;
end;

{------------------------------------------------------------------------------------------}
Procedure TTbDSfGMomentanDaten.UpdateMomTable (EADR: string; DEL_Adr: string; Wert: string);
{------------------------------------------------------------------------------------------}
{ Wert für Datenelement DEL_Adr einer Instanz mit der Busadresse EADR in Momentanwert-Tabelle aktualisieren;
  Übergabe: Busadresse
            DE-Adresse
            Wert }
begin
  if OpenMomTable then begin
    try
      WriteMomTable (EADR, DEL_Adr, Wert, -1);
    finally
      CloseMomTable;
    end;
    { Neuer Wert in der Tabelle: Triggerfile schreiben }
    WriteNewTime (PathServer[WNetWorkDir] + tbDMom.TableName);
  end;
end;

end.
