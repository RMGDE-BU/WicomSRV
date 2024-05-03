{******************************************************************************}
{* Liste zur Pufferung von Ressourcedatei-Daten in Listen                     *}
{* 06.08.2021 WW  Neu                                                         *}
{* 11.02.2022 WW  Ressourcedateityp MBAbruf                                   *}
{* 14.04.2023 WW  Ressourcedateityp ParamMomVerb                              *}
{*                                                                            *}
{* Copyright © RMG Messtechnik GmbH 2021, 2023                                *}
{******************************************************************************}
unit O_ResFilesList;

interface

uses
  Classes, Contnrs, MResParam, MResMrg, MResUtil, WResMeld, MP_Allg;

type
  { Typen von Ressourcedateien }
  TResourceFileType =
    (rft_ParamMrg, rft_ParamEK, rft_ParamMeld, rft_MrgTypGasX, rft_MrgKonv,
     rft_MrgDef, rft_MrgAbruf, rft_MrgInfo, rft_MrgKanalBit, rft_MeldNr,
     rft_WZ_SZ, rft_MBAbruf, rft_ParamMomVerb);

  { Liste der Ressourcedatenlisten }
  TResourceFilesList = class(TObjectList)
  private
    { Private-Deklarationen }
    FListIndizes: array[Low(TResourceFileType)..High(TResourceFileType)] of integer;
    FPath: string;  // Pfad der Ressourcedateien
    procedure InitListIndizes;
    procedure SetPath (Value: string);
    function GetListIndex (ResourceFileType: TResourceFileType): integer;
    function GetListObject (ResourceFileType: TResourceFileType): TObject;
  public
    { Public-Deklarationen }
    constructor Create; overload;
    constructor Create(AOwnsObjects: Boolean); overload;
    function LoadList (ResourceFileType: TResourceFileType): boolean;
    function GetMrgTypGasX (AMrgTyp_GasX: integer; var MrgTyp_Wieser: integer): boolean;
    function GetMrgKonvData (AMrgTyp: integer; var MrgKonvData: TMrgKonvData): boolean;
    function GetMrgDefData (AMrgTyp: integer; var MrgDefData: TMrgDefData): boolean;
    function GetMrgInfoData (AMrgTyp: integer; var MrgInfoData: TMrgInfoData): boolean;
    function GetMBAbrufData (AMrgTyp: integer; AKommandotyp: char;
      AMB_ID: integer; var MBAbrufData: TMBAbrufData): boolean;
    function GetParamNrMomVerb (AParametergruppe: integer; var ParaNr: string): boolean;
    property Path: string write SetPath;
    property ListObject [Value: TResourceFileType]: TObject read GetListObject;
  end;

implementation

{ TResourceFilesList }

{------------------------------------}
constructor TResourceFilesList.Create;
{------------------------------------}
begin
  inherited Create;
  InitListIndizes;
end;

{-----------------------------------------------------------}
constructor TResourceFilesList.Create(AOwnsObjects: Boolean);
{-----------------------------------------------------------}
begin
  inherited Create(AOwnsObjects);
  InitListIndizes;
end;

{-------------------------------------------}
procedure TResourceFilesList.InitListIndizes;
{-------------------------------------------}
{ Vorbelegung der Indizes der Ressourcedatenlisten }
var
  RFT: TResourceFileType;
begin
  for RFT:=Low (FListIndizes) to High (FListIndizes) do
    FListIndizes [RFT]:=-1;  // Default: Liste nicht geladen
end;

{---------------------------------------------------}
procedure TResourceFilesList.SetPath (Value: string);
{---------------------------------------------------}
{ Setzt Pfad der Ressourcedateien }
begin
  FPath:=Value;
end;

{----------------------------------------------------------------------------------}
function TResourceFilesList.LoadList (ResourceFileType: TResourceFileType): boolean;
{----------------------------------------------------------------------------------}
{ Lädt alle Daten einer Ressourcedatei in eine Liste;
  Übergabe: Typ der zu ladenden Ressourcedatei
  Ergebnis: true, wenn Laden erfolgreich }
var
  ParamMrgKonfigList: TParamMrgKonfigList;
  ParamEKKonfigList: TParamEKKonfigList;
  ParamMeldKonfigList: TParamMeldKonfigList;
  MrgTypGasXKonfigList: TMrgTypGasXKonfigList;
  MrgKonvKonfigList: TMrgKonvKonfigList;
  MrgDefKonfigList: TMrgDefKonfigList;
  MrgAbrufKonfigList: TMrgAbrufKonfigList;
  MrgInfoKonfigList: TMrgInfoKonfigList;
  MrgKanalBitKonfigList: TMrgKanalBitKonfigList;
  MeldNrKonfigList: TMeldNrKonfigList;
  WZ_SZKonfigList: TWZ_SZKonfigList;
  MBAbrufKonfigList: TMBAbrufKonfigList;
  ParamMomVerbKonfigList: TParamMomVerbKonfigList;

begin
  Result:=false;
  case ResourceFileType of
    rft_ParamMrg:
      begin
        // ParamMrg.dat laden
        ParamMrgKonfigList:=TParamMrgKonfigList.Create;
        Result:=GetParamMrg_KonfigList_ByParaGruppe (-1, -1, ParamMrgKonfigList, FPath);
        Add (ParamMrgKonfigList);
        FListIndizes [ResourceFileType]:=Count - 1;
      end;

    rft_ParamEK:
      begin
        // ParamEK.dat laden
        ParamEKKonfigList:=TParamEKKonfigList.Create;
        Result:=GetParamEK_KonfigList_ByParaGruppe (-1, ParamEKKonfigList, FPath);
        Add (ParamEKKonfigList);
        FListIndizes [ResourceFileType]:=Count - 1;
      end;

    rft_ParamMeld:
      begin
        // ParamMeld.dat laden
        ParamMeldKonfigList:=TParamMeldKonfigList.Create;
        Result:=GetParamMeld_KonfigList_ByParaGruppe (-1, ParamMeldKonfigList, FPath);
        Add (ParamMeldKonfigList);
        FListIndizes [ResourceFileType]:=Count - 1;
      end;

    rft_MrgTypGasX:
      begin
        // MrgTypGasX.dat laden
        MrgTypGasXKonfigList:=TMrgTypGasXKonfigList.Create;
        Result:=MrgTypGasXKonfigList.LoadFromKonfigFile (FPath);
        Add (MrgTypGasXKonfigList);
        FListIndizes [ResourceFileType]:=Count - 1;
      end;

    rft_MrgKonv:
      begin
        // MrgKonv.dat laden
        MrgKonvKonfigList:=TMrgKonvKonfigList.Create;
        Result:=MrgKonvKonfigList.LoadFromKonfigFile (FPath);
        Add (MrgKonvKonfigList);
        FListIndizes [ResourceFileType]:=Count - 1;
      end;

    rft_MrgDef:
      begin
        // MrgDef.dat
        MrgDefKonfigList:=TMrgDefKonfigList.Create;
        Result:=MrgDefKonfigList.LoadFromKonfigFile (FPath);
        Add (MrgDefKonfigList);
        FListIndizes [ResourceFileType]:=Count - 1;
      end;

    rft_MrgAbruf:
      begin
        // MrgAbruf.dat
        MrgAbrufKonfigList:=TMrgAbrufKonfigList.Create;
        Result:=MrgAbrufKonfigList.LoadFromKonfigFile (FPath);
        Add (MrgAbrufKonfigList);
        FListIndizes [ResourceFileType]:=Count - 1;
      end;

    rft_MrgInfo:
      begin
        // MrgInfo.dat
        MrgInfoKonfigList:=TMrgInfoKonfigList.Create;
        Result:=MrgInfoKonfigList.LoadFromKonfigFile (FPath);
        Add (MrgInfoKonfigList);
        FListIndizes [ResourceFileType]:=Count - 1;
      end;

    rft_MrgKanalBit:
      begin
        // MrgKanalBit.dat
        MrgKanalBitKonfigList:=TMrgKanalBitKonfigList.Create;
        Result:=MrgKanalBitKonfigList.LoadFromKonfigFile (FPath);
        Add (MrgKanalBitKonfigList);
        FListIndizes [ResourceFileType]:=Count - 1;
      end;

    rft_MeldNr:
      begin
        // MeldNr.dat laden
        MeldNrKonfigList:=TMeldNrKonfigList.Create;
        Result:=GetMeldNr_KonfigList ('', -1, MeldNrKonfigList, FPath);
        Add (MeldNrKonfigList);
        FListIndizes [ResourceFileType]:=Count - 1;
      end;

    rft_WZ_SZ:
      begin
        // WZ_SZ.dat laden
        WZ_SZKonfigList:=TWZ_SZKonfigList.Create;
        Result:=GetWZ_SZ_KonfigList ('', WZ_SZKonfigList, FPath);
        Add (WZ_SZKonfigList);
        FListIndizes [ResourceFileType]:=Count - 1;
      end;

    rft_MBAbruf:  // 11.02.2022, WW
      begin
        // MBAbruf.dat
        MBAbrufKonfigList:=TMBAbrufKonfigList.Create;
        Result:=MBAbrufKonfigList.LoadFromKonfigFile (FPath);
        Add (MBAbrufKonfigList);
        FListIndizes [ResourceFileType]:=Count - 1;
      end;

    rft_ParamMomVerb:  // 14.04.2023, WW
      begin
        // ParamMomVerb.dat laden
        ParamMomVerbKonfigList:=TParamMomVerbKonfigList.Create;
        Result:=ParamMomVerbKonfigList.LoadFromKonfigFile (FPath);
        Add (ParamMomVerbKonfigList);
        FListIndizes [ResourceFileType]:=Count - 1;
      end;
  end;
end;

{--------------------------------------------------------------------------------------}
function TResourceFilesList.GetListIndex (ResourceFileType: TResourceFileType): integer;
{--------------------------------------------------------------------------------------}
{ Liefert den Index einer Ressourcedatenliste;
  Übergabe: Typ der Ressourcedatei
  Ergebnis: Listenindex }
begin
  Result:=FListIndizes [ResourceFileType];
end;

{---------------------------------------------------------------------------------------}
function TResourceFilesList.GetListObject (ResourceFileType: TResourceFileType): TObject;
{---------------------------------------------------------------------------------------}
{ Liefert das Objekt einer Ressourcedatenliste;
  Übergabe: Typ der Ressourcedatei
  Ergebnis: Listenobjekt (nil, wenn Ressourcedatenliste nicht vorhanden) }
var
  iIndex: integer;

begin
  Result:=nil;

  iIndex:=GetListIndex (ResourceFileType);  // Index der Ressourcedatenliste ermitteln
  if (iIndex > -1) AND (Count > iIndex) then begin
    case ResourceFileType of
      rft_ParamMrg:
        if Items [iIndex] is TParamMrgKonfigList then
          Result:=Items [iIndex];

      rft_ParamEK:
        if Items [iIndex] is TParamEKKonfigList then
          Result:=Items [iIndex];

      rft_ParamMeld:
        if Items [iIndex] is TParamMeldKonfigList then
          Result:=Items [iIndex];

      rft_MrgTypGasX:
        if Items [iIndex] is TMrgTypGasXKonfigList then
          Result:=Items [iIndex];

      rft_MrgKonv:
        if Items [iIndex] is TMrgKonvKonfigList then
          Result:=Items [iIndex];

      rft_MrgDef:
        if Items [iIndex] is TMrgDefKonfigList then
          Result:=Items [iIndex];

      rft_MrgAbruf:
        if Items [iIndex] is TMrgAbrufKonfigList then
          Result:=Items [iIndex];

      rft_MrgInfo:
        if Items [iIndex] is TMrgInfoKonfigList then
          Result:=Items [iIndex];

      rft_MrgKanalBit:
        if Items [iIndex] is TMrgKanalBitKonfigList then
          Result:=Items [iIndex];

      rft_MeldNr:
        if Items [iIndex] is TMeldNrKonfigList then
          Result:=Items [iIndex];

      rft_WZ_SZ:
        if Items [iIndex] is TWZ_SZKonfigList then
          Result:=Items [iIndex];

      rft_MBAbruf:  // 11.02.2022, WW
        if Items [iIndex] is TMBAbrufKonfigList then
          Result:=Items [iIndex];

      rft_ParamMomVerb:  // 14.04.2023, WW
        if Items [iIndex] is TParamMomVerbKonfigList then
          Result:=Items [iIndex];
    end;  // case ResourceFileType
  end;
end;

{---------------------------------------------------------------}
function TResourceFilesList.GetMrgTypGasX (AMrgTyp_GasX: integer;
  var MrgTyp_Wieser: integer): boolean;
{---------------------------------------------------------------}
{ Sucht in MrgGasX-Ressourcedatenliste nach übergebener MRG-Typnummer des
  Gas-X-Systems und liefert die entsprechende MRG-Typnummer des Wieser-Systems;
  Übergabe: MRG-Typnummer Gas-X
  Rückgabe: MRG-Typnummer Wieser
  Ergebnis: true, wenn MrgTypGasX-Ressourcedatenliste vorhanden }
var
  pListObj: TObject;

begin
  Result:=false;
  pListObj:=GetListObject (rft_MrgTypGasX);
  if Assigned (pListObj) then begin
    if not TMrgTypGasXKonfigList (pListObj).FindMrgTypGasXData (
             AMrgTyp_GasX, MrgTyp_Wieser) then
      // Wenn kein Zuordnungseintrag in Liste vorhanden ist (Standardfall):
      // MRG-Typnummer Gas-X = MRG-Typnummer Wieser
      MrgTyp_Wieser:=AMrgTyp_GasX;
    Result:=true;
  end;
end;

{-----------------------------------------------------------}
function TResourceFilesList.GetMrgKonvData (AMrgTyp: integer;
  var MrgKonvData: TMrgKonvData): boolean;
{-----------------------------------------------------------}
{ Sucht in MrgKonv-Ressourcedatenliste nach MrgKonvData-Listeneintrag zu MRG-Typ;
  Übergabe: MRG-Typ
  Rückgabe: MrgKonvData-Record
  Ergebnis: true, wenn Eintrag gefunden }
var
  pListObj: TObject;

begin
  Result:=false;
  pListObj:=GetListObject (rft_MrgKonv);
  if Assigned (pListObj) then begin
    if TMrgKonvKonfigList (pListObj).FindMrgKonvData (AMrgTyp, MrgKonvData) then
      Result:=true;
  end;
end;

{----------------------------------------------------------}
function TResourceFilesList.GetMrgDefData (AMrgTyp: integer;
  var MrgDefData: TMrgDefData): boolean;
{----------------------------------------------------------}
{ Sucht in MrgDef-Ressourcedatenliste nach MrgDefData-Listeneintrag zu MRG-Typ;
  Übergabe: MRG-Typ
  Rückgabe: MrgDefData-Record
  Ergebnis: true, wenn Eintrag gefunden }
var
  pListObj: TObject;

begin
  Result:=false;
  pListObj:=GetListObject (rft_MrgDef);
  if Assigned (pListObj) then begin
    if TMrgDefKonfigList (pListObj).FindMrgDefData (AMrgTyp, MrgDefData) then
      Result:=true;
  end;
end;

{-----------------------------------------------------------}
function TResourceFilesList.GetMrgInfoData (AMrgTyp: integer;
  var MrgInfoData: TMrgInfoData): boolean;
{-----------------------------------------------------------}
{ Sucht in MrgInfo-Ressourcedatenliste nach MrgInfoData-Listeneintrag zu MRG-Typ;
  Übergabe: MRG-Typ
  Rückgabe: MrgInfoData-Record
  Ergebnis: true, wenn Eintrag gefunden }
var
  pListObj: TObject;

begin
  Result:=false;
  pListObj:=GetListObject (rft_MrgInfo);
  if Assigned (pListObj) then begin
    if TMrgInfoKonfigList (pListObj).FindMrgInfoData (AMrgTyp, MrgInfoData) then
      Result:=true;
  end;
end;

{-------------------------------------------------------------------------------}
function TResourceFilesList.GetMBAbrufData (AMrgTyp: integer; AKommandotyp: char;
  AMB_ID: integer; var MBAbrufData: TMBAbrufData): boolean;
{-------------------------------------------------------------------------------}
{ Sucht in MBAbruf-Ressourcedatenliste nach MBAbrufData-Listeneintrag zu MRG-Typ,
  Kommandotyp und Modbuslisten-ID;
  Übergabe: MRG-Typ
            Kommandotyp
            Modbuslisten-ID
  Rückgabe: MBAbrufData-Record
  Ergebnis: true, wenn Eintrag gefunden }
var
  pListObj: TObject;

begin
  Result:=false;
  pListObj:=GetListObject (rft_MBAbruf);
  if Assigned (pListObj) then begin
    if TMBAbrufKonfigList (pListObj).FindMBAbrufData (AMrgTyp, AKommandotyp,
                           AMB_ID, MBAbrufData) then
      Result:=true;
  end;
end;

{-----------------------------------------------------------------------}
function TResourceFilesList.GetParamNrMomVerb (AParametergruppe: integer;
  var ParaNr: string): boolean;
{-----------------------------------------------------------------------}
{ Sucht in ParamMomVerb-Ressourcedatenliste nach übergebener Parametergruppe und
  liefert die zugeordnete Parameternummer;
  Übergabe: Parametergruppe
  Rückgabe: Allgemeine Parameternummer
  Ergebnis: true, wenn ParamMomVerb-Ressourcedatenliste vorhanden }
var
  pListObj: TObject;

begin
  Result:=false;
  pListObj:=GetListObject (rft_ParamMomVerb);
  if Assigned (pListObj) then begin
    if not TParamMomVerbKonfigList (pListObj).FindParaNr (
             AParametergruppe, ParaNr) then
      // Wenn kein Zuordnungseintrag in Liste vorhanden ist (Standardfall):
      ParaNr:=CP_ALLG_Geraeteversion;  // Allg. Parameternummer für Wieser-Geräteversion
    Result:=true;
  end;
end;

end.

