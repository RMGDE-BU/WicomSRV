{******************************************************************************}
{* Unit: MRG-spezifische Daten                                                *}
{* 02.12.98 WW                                                                *}
{******************************************************************************}
Unit MDBMRG;

INTERFACE

Uses
  Classes, DBTables, SysUtils, PathIni, WStrUtils, WChars;

const

  { Stringlängen von Tabellenfeldern }

  szlen_MrgName                  = 10;
  szlen_MrgParameter             = 3;
  szlen_Datumformat              = 14;
  szlen_Zeitformat               = 14;
  szlen_Infobefehl               = 5;
  szLen_AbrufKommando            = 100;
  szLen_AbrufKommando_alle_Daten = 100;
  szLen_Info                     = 16;

  { Tabellennamen }

  CTblMrgDef   = 'MrgDef.db';
  CTblMrgAbruf = 'MrgAbruf.db';
  CTblMrgKonv  = 'MrgKonv.db';
  CTblMrgInfo  = 'MrgInfo.db';

  { Tabelle 'MrgDef' }

  C_MrgDef_MrgTyp           = 'MrgTyp';
  C_MrgDef_MrgName          = 'MrgName';
  C_MrgDef_Kommandogruppe   = 'Kommandogruppe';
  C_MrgDef_Messwertkanaele  = 'Messwertkanaele';
  C_MrgDef_Zaehlerkanaele   = 'Zaehlerkanaele';
  C_MrgDef_AbrufTagessatz   = 'AbrufTagessatz';
  C_MrgDef_AbrufPruefsatz   = 'AbrufPruefsatz';
  C_MrgDef_AbrufParameter   = 'AbrufParameter';
  C_MrgDef_Ruf_quittieren   = 'Ruf quittieren';
  C_MrgDef_Jahreswechsel    = 'Jahreswechsel';
  C_MrgDef_Datumformat      = 'Datumformat';
  C_MrgDef_Zeitformat       = 'Zeitformat';
  C_MrgDef_RpReset          = 'RpReset';
  C_MrgDef_Infobefehl       = 'Infobefehl';
  C_MrgDef_ModemAbrufgruppe = 'ModemAbrufgruppe';


  { Tabelle 'MrgAbruf' }

  C_MrgAbruf_Kommandotyp              = 'Kommandotyp';
  C_MrgAbruf_Gruppe                   = 'Gruppe';
  C_MrgAbruf_AbrufKommando            = 'Abrufkommando';
  C_MrgAbruf_AbrufKommando_alle_Daten = 'Abrufkommando alle Daten';


  { Tabelle 'MrgKonv' }

  C_MrgKonv_MrgTyp          = 'MrgTyp';
  C_MrgKonv_Konvgruppe      = 'Konvgruppe';
  C_MrgKonv_MeldKonvGruppe  = 'MeldKonvGruppe';
  C_MrgKonv_MNrParastart    = 'MNrParastart';
  C_MrgKonv_MNrParameter    = 'MNrParameter';
  C_MrgKonv_Meldungsgruppe  = 'Meldungsgruppe';
  C_MrgKonv_Parametergruppe = 'Parametergruppe';
  C_MrgKonv_ParaKonvGruppe  = 'ParaKonvGruppe';


  { Tabelle 'MrgInfo' }

  C_MrgInfo_MrgTyp       = 'MrgTyp';
  C_MrgInfo_Kennung      = 'Kennung';
  C_MrgInfo_FabrikNr     = 'FabrikNr';
  C_MrgInfo_TypNr        = 'TypNr';
  C_MrgInfo_PNr_Kennung  = 'PNr_Kennung';
  C_MrgInfo_PNr_FabrikNr = 'PNr_FabrikNr';
  C_MrgInfo_PNr_TypNr    = 'PNr_TypNr';
  C_MrgInfo_PNr_Datum    = 'PNr_Datum';
  C_MrgInfo_PNr_Zeit     = 'PNr_Zeit';


type

  TMrgTyp = Integer;

  { allgemeine MRG-spezifische Daten }

  TMrgDefDataDB = record
    MrgTyp: TMrgTyp;                                   { Gerätetyp }
    MrgName: string [szlen_MrgName];                   { Bezeichnung }
    Kommandogruppe: Integer;                           { Index für Abrufkommando }
    MesswertKanaele: Integer;                          { Länge der Kanalmaske für Messwertabruf }
    Zaehlerkanaele: Integer;                           { Länge der Kanalmaske für Tagessatzabruf }
    AbrufTagessatz: Boolean;                           { spezieller Tagesabruf notwendig }
    AbrufPruefSatz: Boolean;                           { Prüfungssätze vorhanden }
    AbrufParameter: Boolean;                           { Parameterabruf notwendig }
    RufQuittieren: Integer;                            { Ruf muß quittiert werden }
    Jahreswechsel: Boolean;                            { Mrg kann Jahreswechsel beim Abruf verarbeiten }
    Datumformat: string [szlen_Datumformat];           { Formatstring für Parameter "Datum" }
    Zeitformat: string [szlen_Zeitformat];             { Formatstring für Parameter "Zeit" }
    RpReset: Boolean;                                  { Rundpuffer-Reset möglich }
    Infobefehl: string [szlen_Infobefehl];             { Infobefehl-String - liefert Kennung, Fabriknummer etc. }
    ModemAbrufgruppe: integer;                         { FUP/Modem-Abrufprotokolle }
  end;

  { MRG-spezifische Daten für Datenabruf-Befehle }

  TMRGAbrufDataDB = record
    Kommandotyp: string [1];
    Gruppe: integer;
    AbrufKommando: string [szLen_AbrufKommando];
    AbrufKommando_alle_Daten: string [szLen_AbrufKommando_alle_Daten];
  end;

  { MRG-spezifische Daten für Datenkonvertierung }

  TMrgKonvDataDB = record
    MrgTyp: TMrgTyp;
    KonvGruppe: Integer;
    MeldKonvGruppe: Integer;
    MNrParaStart: string [szlen_MrgParameter];
    MNrParameter: string [szlen_MrgParameter];
    MeldungsGruppe: Integer;
    ParameterGruppe: Integer;
    ParaKonvGruppe: Integer;
  end;


  { Dateninhalte der Antwort auf Infobefehl }

  TMrgInfoDataDB = record
    MrgTyp: TMrgTyp;
    Kennung: string [szlen_Info];
    FabrikNr: string [szlen_Info];
    TypNr: string [szlen_Info];
    PNr_Kennung: string [szlen_Info];
    PNr_FabrikNr: string [szlen_Info];
    PNr_TypNr: string [szlen_Info];
    PNr_Datum: string [szlen_Info];
    PNr_Zeit: string [szlen_Info];
  end;


  function GetMrgDefDataDB (MrgTyp: TMrgTyp; var MrgDefDataDB: TMrgDefDataDB): boolean;
  function GetMrgDef_MRGTypNrByNameDB (MRGTypName: string): TMrgTyp;
  function GetMrgAbrufDataDB (Kommandotyp: char; Kommandogruppe: integer;
                              var MrgAbrufDataDB: TMrgAbrufDataDB): boolean;
  function GetMrgKonvDataDB (MrgTyp: TMrgTyp; var MrgKonvDataDB: TMrgKonvDataDB): boolean;
  function GetMrgInfoDataDB (MrgTyp: TMrgTyp; var MrgInfoDataDB: TMrgInfoDataDB): boolean;
  function GetAbrufParameterDB (MrgTyp: TMrgTyp): Boolean;
  function GetAbrufTagessatzDB (MrgTyp: TMrgTyp): Boolean;
  function GetAbrufPruefsatzDB (MrgTyp: TMrgTyp): Boolean;
  procedure SplitMrgInfo (MrgInfo: pchar; var Left_Count: word; Left: pchar; Right: pchar);

IMPLEMENTATION

{-----------------------------------------------------------------------------------}
function GetMrgDefDataDB (MrgTyp: TMrgTyp; var MrgDefDataDB: TMrgDefDataDB): boolean;
{-----------------------------------------------------------------------------------}
{ liefert typspezifische MRG-Informationen aus Tabelle;
  Übergabe: MrgTyp
  Rückgabe: MRG-Informationen in MrgDefDataDB
  Ergebnis: true, wenn Informationen in MRGDEF.DB gefunden }
var
  MrgDefTable: TTable;
begin
  Result := false;
  if FileExists(PathServer.PathName[WStammDir]+CTblMRGDEF) then begin
    MrgDefTable:=TTable.Create(nil);
    try
      MrgDefTable.DatabaseName:=PathServer.PathName[WStammDir];
      MrgDefTable.TableName:=CTblMRGDEF;
      MrgDefTable.Open;
      try
        if MrgDefTable.FindKey ([MrgTyp]) then begin
          with MrgDefDataDB do begin
            MrgTyp:=MrgDefTable.FieldByName (C_MrgDef_MrgTyp).AsInteger;
            MrgName:=MrgDefTable.FieldByName (C_MrgDef_MrgName).AsString;
            Kommandogruppe:=MrgDefTable.FieldByName (C_MrgDef_Kommandogruppe).AsInteger;
            MesswertKanaele:=MrgDefTable.FieldByName (C_MrgDef_Messwertkanaele).AsInteger;
            Zaehlerkanaele:=MrgDefTable.FieldByName (C_MrgDef_Zaehlerkanaele).AsInteger;
            AbrufTagessatz:=MrgDefTable.FieldByName (C_MrgDef_AbrufTagessatz).AsBoolean;
            AbrufPruefSatz:=MrgDefTable.FieldByName (C_MrgDef_AbrufPruefsatz).AsBoolean;
            AbrufParameter:=MrgDefTable.FieldByName (C_MrgDef_AbrufParameter).AsBoolean;
            RufQuittieren:=MrgDefTable.FieldByName (C_MrgDef_Ruf_quittieren).AsInteger;
            Jahreswechsel:=MrgDefTable.FieldByName (C_MrgDef_Jahreswechsel).AsBoolean;
            Datumformat:=MrgDefTable.FieldByName (C_MrgDef_Datumformat).AsString;
            Zeitformat:=MrgDefTable.FieldByName (C_MrgDef_Zeitformat).AsString;
            RpReset:=MrgDefTable.FieldByName (C_MrgDef_RpReset).AsBoolean;
            Infobefehl:=MrgDefTable.FieldByName (C_MrgDef_Infobefehl).AsString;
            ModemAbrufgruppe:=MrgDefTable.FieldByName (C_MrgDef_ModemAbrufgruppe).AsInteger;
          end;
          Result:=true;
        end;
      finally
        MrgDefTable.Close;
      end;
    finally
      MrgDefTable.Free;
    end;
  end;
end;

{----------------------------------------------------------------}
function GetMrgDef_MRGTypNrByNameDB (MRGTypName: string): TMrgTyp;
{----------------------------------------------------------------}
{ liefert für einen MRG-Typnamen die in MRGDEF.DB zugeordnete Gerätetyp-Nummer;
  Übergabe: MRG-Typname
  Ergebnis: MRG-Typnummer (= -1, wenn keine Zuordnung vorhanden, sonst >= 0) }
var
  MrgDefTable: TTable;
  MrgName: string;

begin
  Result:=-1;                     { Vorbelegung für "MRG-Typ nicht vorhanden" }
  if FileExists(PathServer.PathName[WStammDir]+CTblMRGDEF) then begin
    MrgDefTable:=TTable.Create(nil);
    try
      MrgDefTable.DatabaseName:=PathServer.PathName[WStammDir];
      MrgDefTable.TableName:=CTblMRGDEF;
      MrgDefTable.Open;
      try
        while not MrgDefTable.Eof do begin
          MrgName:=MrgDefTable.FieldByName (C_MrgDef_MrgName).AsString;
          { Leerzeichen nicht berücksichtigen beim Vergleich: }
          if UpperCase (StrFilter (MRGTypName, ' ')) = UpperCase (StrFilter (MrgName, ' ')) then begin
            Result:=MrgDefTable.FieldByName (C_MrgDef_MrgTyp).AsInteger;
            Break;
          end;
          MrgDefTable.Next;
        end;  { while not MrgDefTable.Eof }
      finally
        MrgDefTable.Close;
      end;
    finally
      MrgDefTable.Free;
    end;
  end;
end;

{------------------------------------------------------------------------}
function GetMrgAbrufDataDB (Kommandotyp: char; Kommandogruppe: integer;
                            var MrgAbrufDataDB: TMrgAbrufDataDB): boolean;
{------------------------------------------------------------------------}
{ liefert gerätespezifische Daten für Datenabruf-Befehle aus Tabelle;
  Übergabe: KommandoTyp
            Kommandogruppe
  Rückgabe: MrgAbrufData-Record
  Ergebnis: true, wenn MrgAbrufData aus Tabelle gelesen werden konnten }
var
  MrgAbrufTable: TTable;
begin
  Result := false;
  if FileExists(PathServer.PathName[WStammDir]+CTblMRGABRUF) then begin
    MrgAbrufTable:=TTable.Create(nil);
    try
      MrgAbrufTable.DatabaseName:=PathServer.PathName[WStammDir];
      MrgAbrufTable.TableName:=CTblMRGABRUF;
      MrgAbrufTable.Open;
      try
        if MrgAbrufTable.FindKey ([KommandoTyp, Kommandogruppe]) then begin
          with MrgAbrufDataDB do begin
            Kommandotyp:=MrgAbrufTable.FieldByName (C_MrgAbruf_Kommandotyp).AsString;
            Gruppe:=MrgAbrufTable.FieldByName (C_MrgAbruf_Gruppe).AsInteger;
            AbrufKommando:=MrgAbrufTable.FieldByName (C_MrgAbruf_AbrufKommando).AsString;
            AbrufKommando_alle_Daten:=
              MrgAbrufTable.FieldByName (C_MrgAbruf_AbrufKommando_alle_Daten).AsString;
          end;
          Result:=true;
        end;
      finally
        MrgAbrufTable.Close;
      end;
    finally
      MrgAbrufTable.Free;
    end;
  end;
end;

{--------------------------------------------------------------------------------------}
function GetMrgKonvDataDB (MrgTyp: TMrgTyp; var MrgKonvDataDB: TMrgKonvDataDB): boolean;
{--------------------------------------------------------------------------------------}
{ liefert gerätespezifische Daten für die Datenkonvertierung aus Tabelle;
  Übergabe: MrgTyp
  Rückgabe: MrgKonvData-Record
  Ergebnis: true, wenn MrgKonvData aus Tabelle gelesen werden konnten }
var
  MrgKonvTable: TTable;
begin
  Result := false;
  MrgKonvDataDB.MrgTyp := -1;                      { Vorbelegung: keine Daten }
  if FileExists(PathServer.PathName[WStammDir]+CTblMRGKONV) then begin
    MrgKonvTable:=TTable.Create(nil);
    try
      MrgKonvTable.DatabaseName:=PathServer.PathName[WStammDir];
      MrgKonvTable.TableName:=CTblMRGKONV;
      MrgKonvTable.Open;
      try
        if MrgKonvTable.FindKey ([MrgTyp]) then begin
          with MrgKonvDataDB do begin
            MrgTyp := MrgKonvTable.FieldByName(C_MrgKonv_MrgTyp).AsInteger;
            Konvgruppe := MrgKonvTable.FieldByName(C_MrgKonv_Konvgruppe).AsInteger;
            MeldKonvGruppe := MrgKonvTable.FieldByName(C_MrgKonv_MeldKonvGruppe).AsInteger;
            MNrParastart := MrgKonvTable.FieldByName(C_MrgKonv_MNrParastart).AsString;
            MNrParameter := MrgKonvTable.FieldByName(C_MrgKonv_MNrParameter).AsString;
            Meldungsgruppe := MrgKonvTable.FieldByName(C_MrgKonv_Meldungsgruppe).AsInteger;
            Parametergruppe := MrgKonvTable.FieldByName(C_MrgKonv_Parametergruppe).AsInteger;
            ParaKonvGruppe := MrgKonvTable.FieldByName(C_MrgKonv_ParaKonvGruppe).AsInteger;
          end;
          Result:=true;
        end;
      finally
        MrgKonvTable.Close;
      end;
    finally
      MrgKonvTable.Free;
    end;
  end;
end;

{--------------------------------------------------------------------------------------}
function GetMrgInfoDataDB (MrgTyp: TMrgTyp; var MrgInfoDataDB: TMrgInfoDataDB): boolean;
{--------------------------------------------------------------------------------------}
{ liefert typspezifische Informationen zum MRG-Infobefehl aus Tabelle;
  Übergabe: MrgTyp
  Rückgabe: MrgInfoData
  Ergebnis: true, wenn Informationen in MRGINFO.DB gefunden }
var
  MrgInfoTable: TTable;
begin
  Result := false;
  if FileExists(PathServer.PathName[WStammDir]+CTblMRGINFO) then begin
    MrgInfoTable:=TTable.Create(nil);
    try
      MrgInfoTable.DatabaseName:=PathServer.PathName[WStammDir];
      MrgInfoTable.TableName:=CTblMRGINFO;
      MrgInfoTable.Open;
      try
        if MrgInfoTable.FindKey ([MrgTyp]) then begin
          with MrgInfoDataDB do begin
            MrgTyp:=MrgInfoTable.FieldByName (C_MrgInfo_MrgTyp).AsInteger;
            Kennung:=MrgInfoTable.FieldByName (C_MrgInfo_Kennung).AsString;
            FabrikNr:=MrgInfoTable.FieldByName (C_MrgInfo_FabrikNr).AsString;
            TypNr:=MrgInfoTable.FieldByName (C_MrgInfo_TypNr).AsString;
            PNr_Kennung:=MrgInfoTable.FieldByName (C_MrgInfo_PNr_Kennung).AsString;
            PNr_FabrikNr:=MrgInfoTable.FieldByName (C_MrgInfo_PNr_FabrikNr).AsString;
            PNr_TypNr:=MrgInfoTable.FieldByName (C_MrgInfo_PNr_TypNr).AsString;
            PNr_Datum:=MrgInfoTable.FieldByName (C_MrgInfo_PNr_Datum).AsString;
            PNr_Zeit:=MrgInfoTable.FieldByName (C_MrgInfo_PNr_Zeit).AsString;
          end;
          Result:=true;
        end;
      finally
        MrgInfoTable.Close;
      end;
    finally
      MrgInfoTable.Free;
    end;
  end;
end;

{------------------------------------------------------}
function GetAbrufParameterDB (MrgTyp: TMrgTyp): Boolean;
{------------------------------------------------------}
{ liefert aus Tabelle Information, ob für MRG Parameter beim Meßwertabruf geholt
  werden müssen;
  Übergabe: MrgTyp
  Ergebnis: true, wenn Parameterabruf nötig }
var
  MrgDefDataDB: TMrgDefDataDB;
begin
  Result := False;
  if GetMrgDefDataDB (MrgTyp, MrgDefDataDB) then
    Result := MrgDefDataDB.AbrufParameter;
end;

{------------------------------------------------------}
function GetAbrufTagessatzDB (MrgTyp: TMrgTyp): Boolean;
{------------------------------------------------------}
{ liefert aus Tabelle Information, ob für MrgTyp die Tagessätze separat abgerufen
  werden müssen;
  Übergabe: MrgTyp
  Ergebnis: true, wenn separater Tagessatzabruf nötig }
var
  MrgDefDataDB: TMrgDefDataDB;
begin
  Result := False;
  if GetMrgDefDataDB (MrgTyp, MrgDefDataDB) then
    Result := MrgDefDataDB.AbrufTagessatz;
end;

{------------------------------------------------------}
function GetAbrufPruefsatzDB (MrgTyp: TMrgTyp): Boolean;
{------------------------------------------------------}
{ liefert aus Tabelle Information, ob für MRG Prüfungssätze abgerufen werden können;
  Übergabe: MrgTyp
  Ergebnis: true, wenn Prüfsatze abgerufen werden können }
var
  MrgDefDataDB: TMrgDefDataDB;
begin
  Result := False;
  if GetMrgDefDataDB (MrgTyp, MrgDefDataDB) then
    Result := MrgDefDataDB.AbrufPruefsatz;
end;

{---------------------------------------------------------------------------------------}
procedure SplitMrgInfo (MrgInfo: pchar; var Left_Count: word; Left: pchar; Right: pchar);
{---------------------------------------------------------------------------------------}
{ MrgInfo-String aus MRGINFO.DB in seine einzelnen Bestandteile zerlegen }
var
  Dest : PChar;
  DestSize: word;
  code: integer;
  c: char;
  gefunden: boolean;

begin
  Left_Count:=0;
  StrCopy (Left, '');
  StrCopy (Right, '');

  DestSize := 100;
  GetMem (Dest, DestSize);
  If Dest <> Nil Then Begin
      FilterString (Dest, MrgInfo, nil, ',', Nil, 0);                     { Left_Count }
      Val (Dest, Left_Count, code);

      FilterString (Dest, MrgInfo, ',', ',', Nil, 0);                     { Left }
      if StrLen (Dest) > 0 then begin
        if StrLen (Dest) > 1 then begin              { Sonderzeichen }
          gefunden:=false;
          for c:=Low (CSonderzeichen) to High (CSonderzeichen) do begin
            if StrIComp (Dest, CSonderzeichen [c]) = 0 then begin
              gefunden:=true;
              Break;
            end;
          end;
          if gefunden then
            StrPCopy (Left, c)
          else
            StrCopy (Left, '');
        end else                                     { darstellbares Zeichen }
          StrCopy (Left, Dest);
      end;

      FilterString (Dest, MrgInfo, ',', ',', Nil, 1);                     { Right }
      if StrLen (Dest) > 0 then begin
        if StrLen (Dest) > 1 then begin              { Sonderzeichen }
          gefunden:=false;
          for c:=Low (CSonderzeichen) to High (CSonderzeichen) do begin
            if StrIComp (Dest, CSonderzeichen [c]) = 0 then begin
              gefunden:=true;
              Break;
            end;
          end;
          if gefunden then
            StrPCopy (Right, c)
          else
            StrCopy (Right, '');
        end else                                     { darstellbares Zeichen }
          StrCopy (Right, Dest);
      end;

    FreeMem (Dest, DestSize);
  end;
end;

end.

