{******************************************************************************}
{* Unit: Hilfsdaten aus ASCII-Dateien lesen                                   *}
{* 05.03.2003 WW  Neu                                                         *}
{* 06.08.2021 WW  Erweiterungen zum Laden und Suchen in Listen                *}
{******************************************************************************}
Unit MResUtil;

INTERFACE

Uses
  Forms, SysUtils, Contnrs, WStrUtils, T_Zeit, WResConst, WStream;

const
  { Dateinamen: }
  CResWZ_SZ = 'WZ_SZ.dat';

  { WZSZ-Werte: }
  CResSZtoWZ = '0';
  CResWZtoSZ = '1';

type
  { Record für Winterzeit-Sommerzeit-Konfigurationsdaten }

  TWZ_SZData = record
    Umstellungstermin: TDateTime;
    WZSZ: string;
  end;

  { Objekt für Winterzeit-Sommerzeit-Konfigurationsdaten }

  TWZ_SZDataObj = class (TObject)
    Data: TWZ_SZData;
  public
    procedure SetData (AWZ_SZData: TWZ_SZData);
  end;

  { Liste für Winterzeit-Sommerzeit-Konfigurationsdaten }

  TWZ_SZKonfigList = class(TObjectList)
  public
    function FindUmstellungstermin (AUmstellungstermin: TDateTime;
      AWZSZ: string): boolean;  // 06.08.2021, WW
    function FindSZ_WZ_Info (var AktSZ_WZ_Flag: integer;
      var NextSZ_To_WZ: TDateTime): boolean;
  end;


function GetWZ_SZ_KonfigList (AWZSZ: string; WZ_SZKonfigList: TWZ_SZKonfigList;
  Pfad: string): boolean;

IMPLEMENTATION

{ TWZ_SZDataObj }

{-------------------------------------------------------}
procedure TWZ_SZDataObj.SetData (AWZ_SZData: TWZ_SZData);
{-------------------------------------------------------}
begin
  Data:=AWZ_SZData;
end;

{ TWZ_SZKonfigList }

{-----------------------------------------------------------------------------}
function TWZ_SZKonfigList.FindUmstellungstermin (AUmstellungstermin: TDateTime;
  AWZSZ: string): boolean;
{-----------------------------------------------------------------------------}
{ Umstellungstermin in der Liste suchen;
  Übergaben: Umstellungstermin
             WZSZ-Flag
  Ergebnis: true, wenn Umstellungstermin gefunden wurde }
var
  WZ_SZDataObj: TWZ_SZDataObj;
  i: integer;

begin
  Result:=false;
  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    WZ_SZDataObj:=TWZ_SZDataObj (Items [i]);
    if (CmpDateTime (WZ_SZDataObj.Data.Umstellungstermin,
                    AUmstellungstermin) = 0) AND
       (WZ_SZDataObj.Data.WZSZ = AWZSZ) then begin  // 06.08.2021, WW
      Result:=true;
      Break;
    end;
  end;  { for }
end;

{-------------------------------------------------------------------}
function TWZ_SZKonfigList.FindSZ_WZ_Info (var AktSZ_WZ_Flag: integer;
  var NextSZ_To_WZ: TDateTime): boolean;
{-------------------------------------------------------------------}
{ Sucht basierend auf der aktuellen PC-Zeit nach dem nächsten Umstellungszeitpunkt
  SZ auf WZ;
  Rückgaben: Flag zur aktuellen Zeitzone
               (0 = es ist gerade Winterzeit; 1 = es ist gerade Somerzeit)
             nächster Umstellungszeitpunkt SZ auf WZ
  Ergebnis: true, wenn nächster Umstellungszeitpunkt in Liste gefunden wurde }
var
  WZ_SZDataObj: TWZ_SZDataObj;
  i: integer;
  AktDatumZeit: TDateTime;
  SZ_WZ_Flag: integer;

begin
  Result:=false;
  { Vorbelegungen Rückgabe: }
  AktSZ_WZ_Flag:=0;
  NextSZ_To_WZ:=0;

  AktDatumZeit:=Now;

  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    WZ_SZDataObj:=TWZ_SZDataObj (Items [i]);
    try
      SZ_WZ_Flag:=StrToInt (WZ_SZDataObj.Data.WZSZ);

      if WZ_SZDataObj.Data.Umstellungstermin <= AktDatumZeit then
        AktSZ_WZ_Flag:=SZ_WZ_Flag;   { aktuelle Zeitzone: letzter gefundener Eintrag mit Umstellungstermin <= AktDatumZeit }
      if (WZ_SZDataObj.Data.Umstellungstermin > AktDatumZeit) AND
         (SZ_WZ_Flag = 0) then begin
        { nächster Umstellzeitpunkt SZ -> WZ wurde gefunden }
        NextSZ_To_WZ:=WZ_SZDataObj.Data.Umstellungstermin;
        Result:=true;
        Break;
      end
    except
      Break;
    end;
  end;  { for }
end;


{------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------}
function GetWZ_SZ_KonfigList (AWZSZ: string; WZ_SZKonfigList: TWZ_SZKonfigList;
  Pfad: string): boolean;
{-----------------------------------------------------------------------------}
{ Lädt auf übergebenes WZSZ-Flag gefilterten Inhalt von WZ_SZ.Dat in Liste;
  Übergabe: WZSZ-Flag (leer = ohne Filter)
            Pfad zur Ressourcendatei
  Rückgabe: Liste mit WZ-SZ-Konfigurationsdaten
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  sWZSZ: string;
  DateStr: string;
  TimeStr: string;
  DateBuf: TDateTime;
  TimeBuf: TDateTime;
  WZ_SZData: TWZ_SZData;
  WZ_SZDataObj: TWZ_SZDataObj;

begin
  Result:=false;
  if WZ_SZKonfigList = nil then exit;
  WZ_SZKonfigList.Clear;  // 06.08.2021, WW

  FName:=Pfad+CResWZ_SZ;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);  { Feld 1: Umstellungstermin }
          sWZSZ:=S;                               { Feld 2: WZ/SZ-Flag }

          { Filtern auf WZ/SZ-Flag: }
          if (length (AWZSZ) = 0) OR (sWZSZ = AWZSZ) then begin
            DateStr:=F_Zerlegen (FieldStr, CResDZTrenner);       { Datum-String }
            TimeStr:=FieldStr;                                   { Zeit-String }
            if EncodeDateStr (DateStr, 'DD.MM.YYYY', DateBuf) then begin
              if EncodeTimeStr (TimeStr, 'HH:MM:SS', TimeBuf) then begin
                { Record belegen: }
                WZ_SZData.Umstellungstermin:=DateBuf + TimeBuf;
                WZ_SZData.WZSZ:=sWZSZ;

                { Listenobjekt createn und in Liste einfügen: }
                WZ_SZDataObj:=TWZ_SZDataObj.Create;
                WZ_SZDataObj.SetData (WZ_SZData);
                WZ_SZKonfigList.Add (WZ_SZDataObj);
              end else
                Result:=false;
            end else
              Result:=false;
          end;  { if (length (AWZSZ) = 0) OR (sWZSZ = AWZSZ) then }
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

end.
