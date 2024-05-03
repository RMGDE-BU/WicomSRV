{******************************************************************************}
{* Unit: Zugriff auf DSfG-DFÜ-Konfigurationsdateien                           *}
{* 30.11.2009 WW                                                              *}
{* 20.01.2012 WW  mit Konfigurationsdateien für anstehende und DSfG-DFÜ-Fehler*}
{*                                                                            *}
{* Copyright © RMG Messtechnik GmbH 2009                                      *}
{******************************************************************************}
unit WResDSfGDfu;

interface

uses
  Forms, Classes, contnrs, SysUtils, WStrUtils, WResConst, WSysCon, WStream;

type
  { Record für DSfG-DFÜ-Parameter-Konfigurationsdaten }

  TDDfuParaData = record
    ParaNr: integer;
    ParaName: string;
    Aenderbar: boolean;
    Typ: string;
    DefText: string;
    DefWert: string;
    ParaInfo: string;
  end;

  { Objekt für DSfG-DFÜ-Parameter-Konfigurationsdaten }

  TDDfuParaDataObj = class (TObject)
    Data: TDDfuParaData;
  public
    procedure SetData (ADDfuParaData: TDDfuParaData);
  end;

  { Liste für DSfG-DFÜ-Parameter-Konfigurationsdaten }

  TDDfuParaKonfigList = class(TObjectList)
  public
    function FindParaNr (ParaNr: integer; var DDfuParaData: TDDfuParaData): boolean;
  end;

function GetDDfuPara_KonfigList (DDfuParaKonfigList: TDDfuParaKonfigList;
  Pfad: string): boolean;
function GetDDfu_FehlerListAnstehend (Fehler_anstehend: string;
  Fehlertextliste: TStringList; Pfad: string): boolean;
function GetDDfu_FehlerListDSfG (Fehler_DSfG: integer;
  Fehlertextliste: TStringList; Pfad: string): boolean;

implementation

resourcestring
  S_UnbekannterFehler = 'unbekannter Fehler';

const
  { Dateinamen }
  CResDDfuPara = 'DDfuPara.dat';
  CResDErrAnst = 'DErrAnst.dat';
  CResDErrDSfG = 'DErrDSfG.dat';


{ TDDfuParaDataObj }

{----------------------------------------------------------------}
procedure TDDfuParaDataObj.SetData (ADDfuParaData: TDDfuParaData);
{----------------------------------------------------------------}
begin
  Data:=ADDfuParaData;
end;

{ TDDfuParaKonfigList }

{-------------------------------------------------------}
function TDDfuParaKonfigList.FindParaNr (ParaNr: integer;
  var DDfuParaData: TDDfuParaData): boolean;
{-------------------------------------------------------}
{ DSfG-DFÜ-Parameterdaten zu der übergebenen Parameternummer in der Liste suchen;
  Übergaben: Parameternummer
  Rückgabe: DSfG-DFÜ-Parameterdaten
  Ergebnis: true, wenn Eintrag gefunden wurde }
var
  DDfuParaDataObj: TDDfuParaDataObj;
  i: integer;

begin
  Result:=false;
  // Vorbelegung Rückgabe:
  with DDfuParaData do begin
    ParaNr:=-1;
    ParaName:='';
    Aenderbar:=false;
    Typ:='';
    DefText:='';
    DefWert:='';
    ParaInfo:='';
  end;

  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    DDfuParaDataObj:=TDDfuParaDataObj (Items [i]);
    if DDfuParaDataObj.Data.ParaNr = ParaNr then begin
      with DDfuParaData do begin
        ParaNr:=DDfuParaDataObj.Data.ParaNr;
        ParaName:=DDfuParaDataObj.Data.ParaName;
        Aenderbar:=DDfuParaDataObj.Data.Aenderbar;
        Typ:=DDfuParaDataObj.Data.Typ;
        DefText:=DDfuParaDataObj.Data.DefText;
        DefWert:=DDfuParaDataObj.Data.DefWert;
        ParaInfo:=DDfuParaDataObj.Data.ParaInfo;
      end;
      Result:=true;
      Break;
    end;
  end;  { for }
end;


{------------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
function GetDDfuPara_KonfigList (DDfuParaKonfigList: TDDfuParaKonfigList;
  Pfad: string): boolean;
{-----------------------------------------------------------------------}
{ liefert Inhalt von DDfuPara.Dat als Liste;
  Übergabe: Pfad zur Ressourcendatei
  Rückgabe: Liste mit DSfG-DFÜ-Parameterdaten
  Ergebnis: true, wenn DSfG-DFÜ-Parameterdaten in die Liste geladen wurden }
var
  S: string;
  FieldStr: string;
  FieldCount: integer;
  DDfuParaData: TDDfuParaData;
  DDfuParaDataObj: TDDfuParaDataObj;
  FName: string;
  TFS: TTextFileStream;
  FSize: integer;

begin
  Result:=false;
  if DDfuParaKonfigList = nil then exit;

  FName:=Pfad+CResDDfuPara;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);              { Feld 1: Parameternummer }
          if StrToIntDef (FieldStr, -1) > 0 then begin    { Feld 1 enthält Zahlenwert }
            try
              // Record vorbelegen
              DDfuParaData.ParaNr:=StrToInt (FieldStr);
              FieldCount:=1;
              while length (S) > 0 do begin
                FieldStr:=F_Zerlegen (S, CResTrenner);
                inc (FieldCount);
                with DDfuParaData do begin
                  case FieldCount of
                    2: ParaName:=FieldStr;
                    3: Aenderbar:=UpperCase (FieldStr) = CResTrue;
                    4: Typ:=FieldStr;
                    5: DefText:=StringReplace(FieldStr, '|', ';', [rfReplaceAll]);  // 18.07.2011, WW
                    6: DefWert:=StringReplace(FieldStr, '|', ';', [rfReplaceAll]);  // 18.07.2011, WW
                    7: ParaInfo:=FieldStr;
                  end;  { case }
                end;  { with }
              end;  { while length (S) }

              { Listenobjekt createn und in Liste einfügen: }
              DDfuParaDataObj:=TDDfuParaDataObj.Create;
              DDfuParaDataObj.SetData (DDfuParaData);
              DDfuParaKonfigList.Add (DDfuParaDataObj);
            except
              Result:=false;
            end;
          end;
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

{-------------------------------------------------------------}
function GetDDfu_FehlerListAnstehend (Fehler_anstehend: string;
  Fehlertextliste: TStringList; Pfad: string): boolean;
{-------------------------------------------------------------}
{ gibt Fehlertextliste für die anstehenden Fehler einer Wieser-DSfG-DFÜ zurück;
  Übergabe: Fehler_anstehend (Teilstring aus Antwort auf F-Befehl)
  Rückgabe: Fehlertextliste
  Ergebnis: true, wenn Fehlertexte ermittelt werden konnten }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  i: integer;
  chFehler: string;
  bGefunden: boolean;

begin
  Result:=false;
  if not Assigned (Fehlertextliste) then exit;

  FehlertextListe.Clear;  // Vorbelegung Rückgabe

  FName:=Pfad+CResDErrAnst;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        FSize:=TFS.Size;
        for i:=1 to length (Fehler_anstehend) do begin
          chFehler:=Fehler_anstehend [i];

          TFS.Position:=0;  // an Dateianfang gehen
          bGefunden:=false;
          while TFS.Position < FSize do begin
            Application.ProcessMessages;
            TFS.ReadLn (S);
            FieldStr:=F_Zerlegen (S, CResTrenner);        { Feld 1: Fehler }
            if FieldStr = chFehler then begin
              Fehlertextliste.Add (S);
              bGefunden:=true;
              Break;
            end;
          end;  { while FS.Position < FSize }

          if not bGefunden then
            Fehlertextliste.Add (S_UnbekannterFehler + ' ' + chFehler);
        end;  { for i }
      finally
        TFS.Free;
      end;
      Result:=true;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

{-----------------------------------------------------}
function GetDDfu_FehlerListDSfG (Fehler_DSfG: integer;
  Fehlertextliste: TStringList; Pfad: string): boolean;
{-----------------------------------------------------}
{ gibt Fehlertextliste für die DSfG-Fehler einer Wieser-DSfG-DFÜ zurück;
  Übergabe: Fehler_DSfG (Teilstring aus Antwort auf F-Befehl)
  Rückgabe: Fehlertextliste
  Ergebnis: true, wenn Fehlertexte ermittelt werden konnten }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  i: integer;
  DSfGErrorCode: integer;
  bGefunden: boolean;

begin
  Result:=false;
  if not Assigned (Fehlertextliste) then exit;

  FehlertextListe.Clear;  // Vorbelegung Rückgabe

  FName:=Pfad+CResDErrDSfG;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        FSize:=TFS.Size;
        DSfGErrorCode:=1;
        for i:=1 to 16 do begin
          if (Fehler_DSfG AND DSfGErrorCode) <> 0 then begin

            TFS.Position:=0;  // an Dateianfang gehen
            bGefunden:=false;
            while TFS.Position < FSize do begin
              Application.ProcessMessages;
              TFS.ReadLn (S);
              FieldStr:=F_Zerlegen (S, CResTrenner);        { Feld 1: Fehler }

              if StrToIntDef (FieldStr, -1) = DSfGErrorCode then begin
                Fehlertextliste.Add (S);
                bGefunden:=true;
                Break;
              end;
            end;  { while FS.Position < FSize }

            if not bGefunden then
              Fehlertextliste.Add (S_UnbekannterFehler + ' ' + IntToStr (DSfGErrorCode));
          end;
          DSfGErrorCode:=DSfGErrorCode SHL 1;
        end;  { for i }
      finally
        TFS.Free;
      end;
      Result:=true;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

end.

