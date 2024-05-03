{******************************************************************************}
{* Unit: Liste für Kanal-Konvertierungsdaten                                  *}
{* 07.01.2002 WW                                                              *}
{******************************************************************************}
Unit MLGZKonvList;

INTERFACE

Uses
  contnrs;

type
  { Record für Kanal-Konvertierungsdaten aus Stammdaten }

  TStaKanalKonvData = record
    KanalNr: integer;
    { für Impulskanäle: }
    OrgFaktor: double;
    { für Analogkanäle: }
    MessBereichMin: Double;
    MessBereichMax: Double;
  end;

  { Objekt für Kanal-Konvertierungsdaten aus Stammdaten }

  TStaKanalKonvDataObj = class (TObject)
  public
    Daten: TStaKanalKonvData;
    constructor Create (ADaten: TStaKanalKonvData);
  end;

  { Liste für Kanal-Konvertierungsdaten aus Stammdaten }

  TStaKanalKonvDataList = class(TObjectList)
  public
    function GetMRGKanal (MRGKanal: integer): TStaKanalKonvDataObj;
  end;

IMPLEMENTATION

{ StaKanalKonvDataObj }

{------------------------------------------------------------------}
constructor TStaKanalKonvDataObj.Create (ADaten: TStaKanalKonvData);
{------------------------------------------------------------------}
begin
  inherited Create;
  Daten:=ADaten;
end;


{ StaKanalKonvDataList }

{-----------------------------------------------------------------------------------}
function TStaKanalKonvDataList.GetMRGKanal (MRGKanal: integer): TStaKanalKonvDataObj;
{-----------------------------------------------------------------------------------}
{ liefert Kanaldaten für übergebene MRG-Kanalnummer }
var
  i: integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do begin
    if TStaKanalKonvDataObj (Items [i]).Daten.KanalNr = MRGKanal then begin
      Result:=TStaKanalKonvDataObj (Items [i]);
      Break;
    end;
  end;
end;

end.
