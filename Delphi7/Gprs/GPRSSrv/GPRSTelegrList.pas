{******************************************************************************}
{* Unit: Liste empfangener GPRS-Telegramme                                    *}
{* 09.11.2006 WW                                                              *}
{******************************************************************************}
unit GPRSTelegrList;

interface

uses
  Classes, SysUtils, ScktComp, ComCtrls, ExtCtrls;

type
  { Record für Telegramme }

  TGPRSTelegrData = record
    Telegramm: string;
    TelegrNr: integer;
    GerTypNr: integer;
    ClientAdresse: string;
    ClientSocket: TCustomWinSocket;
  end;

  { Objekt für Telegramme }

  TGPRSTelegrDataObj = class (TObject)
    Data: TGPRSTelegrData;
  public
    procedure SetData (AGPRSTelegrData: TGPRSTelegrData);
  end;

  { Callback-Prozedurtyp }

  TCBIntegerProc = procedure (Value: integer) of object;

  { Liste für Telegramme }

  TGPRSTelegrList = class(TThreadList)
  private
    FTelegrNr: integer;
    FSaveTelegrNr: TCBIntegerProc;
    procedure SetNextTelegrammNr;
  public
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure SetTelegramm (ATelegramm: string; AGerTypNr: integer;
      AClientAdresse: string; AClientSocket: TCustomWinSocket);
    function GetTelegramm (var GPRSTelegrData: TGPRSTelegrData): boolean;
    function GetCount: integer;
    property TelegrNr: integer read FTelegrNr write FTelegrNr;
    property CBSaveTelegrNr: TCBIntegerProc read FSaveTelegrNr write FSaveTelegrNr;
  end;

implementation

{ TGPRSTelegrDataObj }

{----------------------------------------------------------------------}
procedure TGPRSTelegrDataObj.SetData (AGPRSTelegrData: TGPRSTelegrData);
{----------------------------------------------------------------------}
{ Telegramm-Record belegen }
begin
  Data:=AGPRSTelegrData;
end;


{ TGPRSTelegrList }

{---------------------------------}
Destructor TGPRSTelegrList.Destroy;
{---------------------------------}
{ Liste freigeben }
begin
  Clear;
  inherited Destroy;
end;

{------------------------------}
procedure TGPRSTelegrList.Clear;
{------------------------------}
{ Liste leeren }
var
  List: TList;
  i: integer;

begin
  List:=LockList;
  try
    for i:=0 to List.Count - 1 do
      if Assigned (List [i]) then
        TGPRSTelegrDataObj (List [i]).Free;
  finally
    UnlockList;
  end;
  inherited;
end;

{-----------------------------------------------------------------------------}
procedure TGPRSTelegrList.SetTelegramm (ATelegramm: string; AGerTypNr: integer;
  AClientAdresse: string; AClientSocket: TCustomWinSocket);
{-----------------------------------------------------------------------------}
{ Telegramm am Ende der Liste einfügen;
  Übergabe: Telegramm
            Gerätetyp
            Client-IP-Adresse
            Zeiger auf Client-Socket }
var
  List: TList;
  GPRSTelegrData: TGPRSTelegrData;
  GPRSTelegrDataObj: TGPRSTelegrDataObj;
begin
  { Telegramm-Nummer hochzählen: }
  SetNextTelegrammNr;
  { Telegramm-Record belegen: }
  GPRSTelegrData.Telegramm:=ATelegramm;
  GPRSTelegrData.TelegrNr:=FTelegrNr;
  GPRSTelegrData.GerTypNr:=AGerTypNr;
  GPRSTelegrData.ClientAdresse:=AClientAdresse;
  GPRSTelegrData.ClientSocket:=AClientSocket;
  { Telegramm-Objekt createn: }
  GPRSTelegrDataObj:=TGPRSTelegrDataObj.Create;
  GPRSTelegrDataObj.SetData (GPRSTelegrData);

  List:=LockList;
  try
    List.Add (GPRSTelegrDataObj);
  finally
    UnlockList;
  end;
end;

{-----------------------------------------------------------------------------------}
function TGPRSTelegrList.GetTelegramm (var GPRSTelegrData: TGPRSTelegrData): boolean;
{-----------------------------------------------------------------------------------}
{ Telegramm-Objekt aus Liste lesen }
var
  List: TList;
begin
  List:=LockList;
  try
    if List.Count > 0 then begin
      GPRSTelegrData:=TGPRSTelegrDataObj (List [0]).Data;
      { Telegramm-Eintrag und Telegramm-Objekt aus Liste löschen: }
      TGPRSTelegrDataObj (List [0]).Free;
      List.Delete (0);
      Result:=true;
    end else
      Result:=false;
  finally
    UnlockList;
  end;
end;

{-----------------------------------------}
function TGPRSTelegrList.GetCount: integer;
{-----------------------------------------}
{ gibt Anzahl der in der Liste enthaltenen Telegramme zurück }
var
  List: TList;
begin
  List:=LockList;
  try
    Result:=List.Count;
  finally
    UnlockList;
  end;
end;

{-------------------------------------------}
procedure TGPRSTelegrList.SetNextTelegrammNr;
{-------------------------------------------}
// fortlaufende Telegramm-Nummer hochzählen und speichern
begin
  if FTelegrNr < High (FTelegrNr) then
    inc (FTelegrNr)
  else
    FTelegrNr:=1;  // beginnt wieder bei 1

  if Assigned (CBSaveTelegrNr) then
    CBSaveTelegrNr (FTelegrNr);  // nächste vergebene Telegrammnummer speichern
end;

end.

