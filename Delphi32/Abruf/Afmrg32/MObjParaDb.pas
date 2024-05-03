{******************************************************************************}
{* Unit: MRG-Parameterkonvertierung und -verarbeitung mit Datenbank-Zugriff   *}
{* 03.03.2003 WW                                                              *}
{******************************************************************************}
Unit MObjParaDb;

INTERFACE

Uses
  Forms, Classes, DB, DBTables, SysUtils, WTables, Db_Attn, PathIni, WStrUtils,
  WSysCon, MObjPara, MResParam, MDbParam;

Type

  { Liste von Parametern, Zugriff auf Datenbank }

  TParameterListeDB = class (TParameterListe)
  private
    function CreateMomTable (AMomTable: TTableExt): boolean;
  public
    Function LoadFromFile (FileName: TFileName; ParaKonv: TParaKonv;
                           ConfigFromDB: boolean): boolean;
    Function LoadFromHeapA (HeapStr: string; ParameterGruppe: integer;
                            ConfigFromDB: boolean): boolean;     

    Procedure SaveToParamTable (MrgId: integer);
    Procedure SaveToMomTable (MrgId: integer);
    Procedure SaveToExcel (ExcelCaption: string; ConfigFromDB: boolean);
  End;

Procedure AktuMomTable (MrgId: integer; AllgParaNr: string; Wert: string);
Procedure DeleteFromParamTable (AMrgId: integer);

IMPLEMENTATION

{ TParameterListeDB }

{--------------------------------------------------------------------------------}
Function TParameterListeDB.LoadFromFile (FileName: TFileName; ParaKonv: TParaKonv;
                                         ConfigFromDB: boolean): Boolean;
{--------------------------------------------------------------------------------}
{ Parameterliste mit Inhalt von FileName (Rohdaten) neu füllen und sortieren;
  Konfigurationsdaten-Zugriff wahlweise über Datenbank-Tabellen oder Resourcendatei
  Übergabe: Parameter-Rohfilename
            Record mit Angaben für MRG-Parameterkonvertierung
            Flag ConfigFromDB (wenn true, werden Parameter-Konfigurationsdaten aus
                               Datenbank-Tabellen gelesen, sonst aus Resourcendatei)
  Ergebnis: true, wenn Rohfile erfolgreich in Liste konvertiert werden konnte }
var
  ParamMrgKonfigList: TParamMrgKonfigList;

Begin
  ParamMrgKonfigList:=TParamMrgKonfigList.Create;
  try
    if ConfigFromDB then
      { Parameternummern-Konfigurationsliste aus Tabelle laden: }
      GetParamMrg_KonfigList_ByParaGruppeDB (ParaKonv.ParaGruppe, ParamMrgKonfigList)
    else
      { Parameternummern-Konfigurationsliste aus Resourcendatei laden: }
      GetParamMrg_KonfigList_ByParaGruppe (ParaKonv.ParaGruppe, ParamMrgKonfigList, KonfigPfad);

    { Rohdatenfile konvertieren: }
    Result:=KonvRohdatenFromFile (FileName, ParaKonv, ParamMrgKonfigList);
  finally
    ParamMrgKonfigList.Free;
  end;
End;

{----------------------------------------------------------------------------------}
Function TParameterListeDB.LoadFromHeapA (HeapStr: string; ParameterGruppe: integer;
                                          ConfigFromDB: boolean): Boolean;
{----------------------------------------------------------------------------------}
{ Parameterliste mit Inhalt von HeapStr (Rohdaten eines einzelnen Parameters
  der Gruppe A (ParaKonvGruppe 1 = Wieser)) füllen/updaten und bei Bedarf sortieren;
  Konfigurationsdaten-Zugriff wahlweise über Datenbank-Tabellen oder Resourcendatei
  Übergabe: Parameter-Rohstring
            Parameter-Gruppe
            Flag ConfigFromDB (wenn true, werden Parameter-Konfigurationsdaten aus
                               Datenbank-Tabellen gelesen, sonst aus Resourcendatei)
  Ergebnis: true, wenn Rohstring erfolgreich in Liste konvertiert werden konnte }
var
  ParamMrgKonfigList: TParamMrgKonfigList;

Begin
  ParamMrgKonfigList:=TParamMrgKonfigList.Create;
  try
    if ConfigFromDB then
      { Parameternummern-Konfigurationsliste aus Tabelle laden: }
      GetParamMrg_KonfigList_ByParaGruppeDB (ParameterGruppe, ParamMrgKonfigList)
    else
      { Parameternummern-Konfigurationsliste aus Resourcendatei laden: }
      GetParamMrg_KonfigList_ByParaGruppe (ParameterGruppe, ParamMrgKonfigList, KonfigPfad);

    { Rohdatenstring konvertieren: }
    Result:=KonvEinzelparameterA (HeapStr, ParameterGruppe, ParamMrgKonfigList,
                                  CLen_MrgParaNr_Wieser); 
  finally
    ParamMrgKonfigList.Free;
  end;
End;

{------------------------------------------------------------}
Procedure TParameterListeDB.SaveToParamTable (MrgId: integer);
{------------------------------------------------------------}
{ Inhalt der ParameterListe in Parametertabelle schreiben; bisherige Einträge für MrgId
  werden zuvor gelöscht;
  Übergabe: MrgId }
var
  MParamTable: TTableExt;
  i: integer;
  MrgNr: string;
  AllgNr: string;
  Wert: string;

  {-----------------------}
  procedure CreateMParamDB;
  {-----------------------}
  { MParam-Tabelle anlegen }
  begin
    with MParamTable.FieldDefs do begin
      Clear;
      Add(C_TfMParaMrgId, ftInteger, 0, false);
      Add(C_TfMParaNrMrg, ftString, 10, false);  { ab 22.10.2002 erweitert von 3 auf 10 Stellen
                                                   für Elster DL240, EK260 Daten-Adressen }
      Add(C_TfMParaNr, ftString, 9, false);
      Add(C_TfMParaWert, ftString, 40, false);
    end;
    with MParamTable.IndexDefs do begin
      Clear;
      Add('PMrgId_NrMrg', C_TfMParaMrgId +';'+ C_TfMParaNrMrg, [ixPrimary,ixUnique]); { Primärindex }
    end;
    MParamTable.CreateTable;
  end;

begin
  MParamTable:=TTableExt.Create (nil);
  try
    MParamTable.DatabaseName:=PathServer.PathName [ManuDir];
    MParamTable.TableName:=C_TbMParam;
    if not MParamTable.Exists then
      CreateMParamDB;                              { Tabelle anlegen, falls nicht vorhanden }
    if MParamTable.OpenShared then begin
      try
        MParamTable.Filter:=C_TfMParaMrgId + ' = ' + IntToStr(MrgId);
        MParamTable.Filtered:=true;                          { Aktivieren des Stationsfilters }

        { alle bisherigen Parameter der Station löschen: }
        while not MParamTable.Eof do
          MParamTable.Delete;

        { alle Listeneinträge in Tabelle schreiben: }
        for i:=0 to Count-1 do begin
          MrgNr:=TParameterItem (Items [i]).MrgNum;
          AllgNr:=TParameterItem (Items [i]).AllgNum;
          Wert:=TParameterItem (Items [i]).Wert;
          MParamTable.InsertRecord ([MrgId, MrgNr, AllgNr, Wert]);
        end;
      finally
        MParamTable.Close;
      end;
    end;
  finally
    MParamTable.Free;
  end;
end;

{----------------------------------------------------------}
Procedure TParameterListeDB.SaveToMomTable (MrgId: integer);
{----------------------------------------------------------}
{ Inhalt der ParameterListe in Momentanwerte-Tabelle schreiben; bisherige Einträge
  werden zuvor gelöscht;
  Übergabe: MrgId }
var
  MomTable: TTableExt;
  i: integer;
  MrgNr: string;
  AllgNr: string;
  Wert: string;
  Ok: boolean;

begin
  MomTable:=TTableExt.Create (nil);
  try
    MomTable.DatabaseName:=PathServer.PathName [WNetWorkDir];
    MomTable.TableName:=CDBMMom + Format('%.4d.DB', [MrgId]);      { MrgId im Tabellennamen }

    if MomTable.Exists then
      Ok:=MomTable.EmptyTable                                { Tabelle leeren }
    else
      Ok:=CreateMomTable (MomTable);                     { Tabelle neuanlegen }

    if Ok then begin
      if MomTable.OpenShared then begin
        try
          { alle Listeneinträge in Tabelle schreiben: }
          for i:=0 to Count-1 do begin
            MrgNr:=TParameterItem (Items [i]).MrgNum;
            AllgNr:=TParameterItem (Items [i]).AllgNum;
            Wert:=TParameterItem (Items [i]).Wert;
            MomTable.InsertRecord ([MrgNr, AllgNr, Wert, length (Wert)]);
          end;
        finally
          MomTable.Close;
        end;

       { Neue Werte in der Tabelle: Triggerfile schreiben }
        WriteNewTime (MomTable.DatabaseName + MomTable.TableName);
      end;
    end;  { if Ok }
  finally
    MomTable.Free;
  end;
end;

{------------------------------------------------------------------------}
function TParameterListeDB.CreateMomTable (AMomTable: TTableExt): boolean;
{------------------------------------------------------------------------}
{ Momentanwerte-Tabelle anlegen;
  Übergabe: Tabelle
  Ergebnis: true, wenn Tabelle angelegt wurde }
begin
  Result:=false;
  if AMomTable <> nil then begin
    with AMomTable.FieldDefs do begin
      Clear;
      Add(C_Mom_Parameternummer_im_MRG, ftString, 10, false);   { ab 22.10.2002 erweitert von 3 auf 10 Stellen
                                                                  für Elster DL240, EK260 Daten-Adressen }
      Add(C_Mom_Parameternummer, ftString, 9, false);
      Add(C_Mom_Wert, ftString, 40, false);
      Add(C_Mom_Stellen, ftSmallInt, 0, false);
    end;
    with AMomTable.IndexDefs do begin
      Clear;
      Add('PParameternummer_im_MRG', C_Mom_Parameternummer_im_MRG, [ixPrimary,ixUnique]); { Primärindex }
    end;
    Result:=AMomTable.CreateTable;
  end;
end;

{------------------------------------------------------------------------------------}
Procedure TParameterListeDB.SaveToExcel (ExcelCaption: string; ConfigFromDB: boolean);
{------------------------------------------------------------------------------------}
{ Parameter aus ParameterListe in Excel konvertieren;
  Konfigurationsdaten-Zugriff wahlweise über Datenbank-Tabellen oder Resourcendatei
  Übergabe: Titel für Excel-Tabelle
            Flag ConfigFromDB (wenn true, werden Parameter-Texte aus
                               Datenbank-Tabelle gelesen, sonst aus Resourcendatei) }
var
  ParaTextKonfigList: TParaTextKonfigList;

begin
  ParaTextKonfigList:=TParaTextKonfigList.Create;
  try
    if ConfigFromDB then
      { Parametertext-Konfigurationsliste aus Tabelle laden: }
      GetParaText_KonfigListDB (ParaTextKonfigList)
    else
      { Parametertext-Konfigurationsliste aus Resourcendatei laden: }
      GetParaText_KonfigList (ParaTextKonfigList, KonfigPfad);

    { Parameterlisten-Einträge mit Parametertexten in Excel-Blatt schreiben: }
    WriteExcelSheet (ExcelCaption, ParaTextKonfigList);
  finally
    ParaTextKonfigList.Free;
  end;
end;


{------------------------------------------------------------------------------}

{------------------------------------------------------------------------}
Procedure AktuMomTable (MrgId: integer; AllgParaNr: string; Wert: string);
{------------------------------------------------------------------------}
{ Wert für AllgParaNr in Momentanwerttabelle aktualisieren;
  Übergabe: MrgId
            AllgParaNr
            Wert }
var
  MomTable: TTableExt;

begin
  MomTable:=TTableExt.Create (nil);
  try
    MomTable.DatabaseName:=PathServer.PathName [WNetWorkDir];
    MomTable.TableName:=CDBMMom + Format('%.4d.DB', [MrgId]);      { MrgId im Tabellennamen }
    if MomTable.Exists then begin
      if MomTable.OpenShared then begin
        try
          while not MomTable.Eof do begin
            if MomTable.FieldByName (C_Mom_Parameternummer).AsString = AllgParaNr then begin
              MomTable.Edit;
              MomTable.FieldByName (C_Mom_Wert).AsString:=Wert;
              MomTable.FieldByName (C_Mom_Stellen).AsInteger:=length (Wert);
              MomTable.Post;
              Break;
            end;
            MomTable.Next;
          end;
        finally
          MomTable.Close;
        end;
        WriteNewTime(MomTable.DatabaseName + MomTable.TableName);     { Triggerfile schreiben }
      end;
    end;
  finally
    MomTable.Free;
  end;
end;

{-----------------------------------------------}
Procedure DeleteFromParamTable (AMrgId: integer);
{-----------------------------------------------}
{ alle zu AMrgId gehörenden Einträge aus der Parametertabelle löschen
  Übergabe: AMrgId }
var
  MParamTable: TTableExt;

begin
  MParamTable:=TTableExt.Create (nil);
  try
    MParamTable.DatabaseName:=PathServer.PathName [ManuDir];
    MParamTable.TableName:=C_TbMParam;
    if MParamTable.Exists then begin
      if MParamTable.OpenShared then begin
        try
          MParamTable.Filter:=C_TfMParaMrgId + ' = ' + IntToStr(AMrgId);
          MParamTable.Filtered:=true;                        { Aktivieren des Stationsfilters }
          while not MParamTable.Eof do
            MParamTable.Delete;
        finally
          MParamTable.Close;
        end;
      end;
    end;
  finally
    MParamTable.Free;
  end;
end;

End.

