{******************************************************************************}
{* Unit: Liste f�r Kanal-Konvertierungsdaten                                  *}
{* 07.01.2002 WW                                                              *}
{******************************************************************************}
Unit MLGZKonvList;

INTERFACE

Uses
  contnrs;

type
  { Record f�r Kanal-Konvertierungsdaten aus Stammdaten }

  TStaKanalKonvData = record
    KanalNr: integer;
    { f�r Impulskan�le: }
    OrgFaktor: double;
    { f�r Analogkan�le: }
    MessBereichMin: Double;
    MessBereichMax: Double;
  end;

  { Objekt f�r Kanal-Konvertierungsdaten aus Stammdaten }

  TStaKanalKonvDataObj = class (TObject)
  public
    Daten: TStaKanalKonvData;
    constructor Create (ADaten: TStaKanalKonvData);
  end;

  { Liste f�r Kanal-Konvertierungsdaten aus Stammdaten }

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
{ liefert Kanaldaten f�r �bergebene MRG-Kanalnummer }
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
