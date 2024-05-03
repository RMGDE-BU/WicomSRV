{******************************************************************************}
{* Unit: Liste zu importierender Iec-Telegramme                               *}
{* 12.02.2009 WW                                                              *}
{******************************************************************************}
unit IecImportTelegrList;

interface

uses
  Classes, IecConst;

type
  { Record f�r Telegramme }

  TIecImportTelegrData = record
    LinienNr: integer;
    Telegramm: string;
    DatenEinheitIdent: TIecDatenEinheitIdent;
    DatenObjekte: string;
  end;

  { Objekt f�r Telegramme }

  TIecImportTelegrDataObj = class (TObject)
    Data: TIecImportTelegrData;
  public
    procedure SetData (AIecImportTelegrData: TIecImportTelegrData);
  end;

  { Liste f�r Telegramme }

  TIecImportTelegrList = class(TThreadList)
  public
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure SetTelegramm (AIecImportTelegrData: TIecImportTelegrData);
    function GetTelegramm (var IecImportTelegrData: TIecImportTelegrData): boolean;
    function GetCount: integer;
  end;

implementation

{ TIecImportTelegrDataObj }

{--------------------------------------------}
procedure TIecImportTelegrDataObj.SetData (
  AIecImportTelegrData: TIecImportTelegrData);
{--------------------------------------------}
{ Telegramm-Record belegen }
begin
  Data:=AIecImportTelegrData;
end;


{ TIecImportTelegrList }

{--------------------------------------}
Destructor TIecImportTelegrList.Destroy;
{--------------------------------------}
{ Liste freigeben }
begin
  Clear;
  inherited Destroy;
end;

{-----------------------------------}
procedure TIecImportTelegrList.Clear;
{-----------------------------------}
{ Liste leeren }
var
  List: TList;
  i: integer;

begin
  List:=LockList;
  try
    for i:=0 to List.Count - 1 do
      if Assigned (List [i]) then
        TIecImportTelegrDataObj (List [i]).Free;
  finally
    UnlockList;
  end;
  inherited;
end;

{--------------------------------------------}
procedure TIecImportTelegrList.SetTelegramm (
  AIecImportTelegrData: TIecImportTelegrData);
{--------------------------------------------}
{ Telegramm am Ende der Liste einf�gen }
var
  List: TList;
  IecImportTelegrDataObj: TIecImportTelegrDataObj;
begin
  List:=LockList;
  try
    IecImportTelegrDataObj:=TIecImportTelegrDataObj.Create;
    IecImportTelegrDataObj.SetData (AIecImportTelegrData);
    List.Add (IecImportTelegrDataObj);
  finally
    UnlockList;
  end;
end;

{--------------------------------------------------------}
function TIecImportTelegrList.GetTelegramm (
  var IecImportTelegrData: TIecImportTelegrData): boolean;
{--------------------------------------------------------}
{ Telegramm-Objekt aus Liste lesen }
var
  List: TList;
begin
  List:=LockList;
  try
    if List.Count > 0 then begin
      IecImportTelegrData:=TIecImportTelegrDataObj (List [0]).Data;
      { Telegramm-Eintrag und Telegramm-Objekt aus Liste l�schen: }
      TIecImportTelegrDataObj (List [0]).Free;
      List.Delete (0);
      Result:=true;
    end else
      Result:=false;
  finally
    UnlockList;
  end;
end;

{----------------------------------------------}
function TIecImportTelegrList.GetCount: integer;
{----------------------------------------------}
{ gibt Anzahl der in der Liste enthaltenen Telegramme zur�ck }
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

end.

