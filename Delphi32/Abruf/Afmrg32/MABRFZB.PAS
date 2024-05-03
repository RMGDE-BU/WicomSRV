{******************************************************************************}
{* Unit: Zeitbereiche für MRG-Abruf                                           *}
{* 17.12.1998 WW                                                              *}
{******************************************************************************}
unit MAbrfZB;

INTERFACE

Uses
  MDBSta, T_Zeit, T_Tools, MAbruf, PathIni;

type

  { Objekt zur Verwaltung der Zeitbereiche beim Datenabruf eines MRG }

  TAbrufZeitbereich = class (TObject)
  private
    MrgId: TMrgId;
    AbrufRecord: TMRGAbrufRec;                  { beliebiger Zeitbereich }
    DSfGAbrufRecord: TMRGAbrufRec;              { 1-Tages-Zeitbereich }
    procedure Nachtag;
  public
    AbrufRecordGesetzt: boolean;
    constructor Create;
    procedure SetMrgId (AMrgId: TMrgId);
    procedure SetzeAbrufRecord (const AAbrufRec: TMRGAbrufRec);
    function GetNextAbrufZeitbereich (var NextAbrufRecord: TMRGAbrufRec): boolean;
    procedure ResetAbrufRecord;
  end;


IMPLEMENTATION

{ TAbrufZeitbereich }

{-----------------------------------}
constructor TAbrufZeitbereich.Create;
{-----------------------------------}
begin
  inherited Create;
  ResetAbrufRecord;
  MrgId := 0;
end;

{-------------------------------------------}
procedure TAbrufZeitbereich.ResetAbrufRecord;
{-------------------------------------------}
begin
  AbrufRecordGesetzt := False;
end;

{----------------------------------}
procedure TAbrufZeitbereich.Nachtag;
{----------------------------------}
var
  AbrufTag: DateRec;
begin
  with DSfGAbrufRecord.AbrufRec do begin
    AbrufTag.year := vonJahr;
    AbrufTag.month := vonMonat;
    AbrufTag.day := vonTag;
  end;
  P_Nachtag (AbrufTag);
  with DSfGAbrufRecord.AbrufRec do begin
    vonJahr := AbrufTag.year;
    vonMonat := AbrufTag.Month;
    vonTag := AbrufTag.Day;
    bisJahr := vonJahr;
    bisMonat := vonMonat;
    bisTag := vonTag;
  end;
end;

{---------------------------------------------------------------------------}
procedure TAbrufZeitbereich.SetzeAbrufRecord (const AAbrufRec: TMRGAbrufRec);
{---------------------------------------------------------------------------}
begin
  AbrufRecord := AAbrufRec;

 { DSfGAbrufRecord auf 1-Tages-Zeitbereich setzen }

  DSfGAbrufRecord := AAbrufRec;
  with DSfGAbrufRecord.AbrufRec do begin
    bisJahr := vonJahr;
    bisMonat := vonMonat;
    bisTag := vonTag;
  end;
  AbrufRecordGesetzt := true;
end;

{----------------------------------------------------}
procedure TAbrufZeitbereich.SetMrgId (AMrgId: TMrgId);
{----------------------------------------------------}
begin
  MrgId := AMrgId;
end;

{--------------------------------------------------------------------------------------------}
function TAbrufZeitbereich.GetNextAbrufZeitbereich (var NextAbrufRecord: TMRGAbrufRec): boolean;
{--------------------------------------------------------------------------------------------}
{ NextAbrufRecord wird mit dem nächsten 1-Tages-Zeitbereich belegt (Rückgabe);
  Ergebnis: false, wenn NextAbrufRecord bei Übergabe den Zeitbereich des letzten Tages enthält }
var
  Dummy1: char;
  Dummy2: TMrgId;
  AktTag, EndeTag: DateRec;
  Stammdaten: TMRGStammdaten;

begin
  Result := false;
  NextAbrufRecord := AbrufRecord;

  Stammdaten:=TMRGStammdaten.Create (PathServer.PathName [WStammDir]);
  try
    if Stammdaten.InitTabellen then begin
      if StammDaten.DSFGTyp (MrgId, Dummy1, Dummy2) = DSFGSLAVE then begin
        NextAbrufRecord := DSfGAbrufRecord;
        NachTag;
        AktTag.year := DSfGAbrufRecord.AbrufRec.vonJahr;
        AktTag.month := DSfGAbrufRecord.AbrufRec.vonMonat;
        AktTag.day := DSfGAbrufRecord.AbrufRec.vonTag;
        EndeTag.year := AbrufRecord.AbrufRec.BisJahr;
        EndeTag.month := AbrufRecord.AbrufRec.BisMonat;
        EndeTag.day := AbrufRecord.AbrufRec.BisTag;
        Result := CmpDate (AktTag, EndeTag) <= 0;
      end;
    end;
  finally
    Stammdaten.Free;
  end;      
end;

end.