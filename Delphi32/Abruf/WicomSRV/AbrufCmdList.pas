{******************************************************************************}
{* Unit: Abruf-Kommandoliste                                                  *}
{* 13.12.2002  WW                                                             *}
{* 03.06.2020 WW Umstellung auf Indy-10 TCP-Connection (Vs. 10.0.52)          *}
{******************************************************************************}
unit AbrufCmdList;

interface

uses
  Classes, AbrufCmd, MLGZKonvList, IdTCPConnection;

type
  { Objekt für Abruf-Kommandoliste }
  TKommandoObj = class(TObject)
    ClientConnection: TIdTCPConnection;           { TCP-Verbindung zum Client; 03.06.2020, WW }
    Kommando: string;                             { abzuarbeitendeds Server-Kommando }
    StaKanalKonvDataList: TStaKanalKonvDataList;  { MRG-Kanaldaten aus Stammdaten }
  public
    destructor Destroy; override;
    procedure SetData (AClientConnection: TIdTCPConnection;
                       AKommando: string;
                       AStaKanalKonvDataList: TStaKanalKonvDataList);
  end;

  { Abruf-Kommandoliste }
  TAbrufKommandoListe = class(TThreadList)
  public
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Delete(iIndex: integer); virtual;
    procedure SetKommandoObj (ClientConnection: TIdTCPConnection;
                              Kommando: string;
                              StaKanalKonvDataList: TStaKanalKonvDataList);
    function GetKommandoObj: TKommandoObj;
    function GetCount: integer;
  end;

implementation

{ TKommandoObj }

{------------------------------}
Destructor TKommandoObj.Destroy;
{------------------------------}
begin
  StaKanalKonvDataList.Free;
  inherited Destroy;
end;

{----------------------------------------------------------------------------}
procedure TKommandoObj.SetData (AClientConnection: TIdTCPConnection;
                                AKommando: string;
                                AStaKanalKonvDataList: TStaKanalKonvDataList);
{----------------------------------------------------------------------------}
begin
  ClientConnection:=AClientConnection;
  Kommando:=AKommando;
  StaKanalKonvDataList:=AStaKanalKonvDataList;
end;


{ TAbrufKommandoListe }

{-------------------------------------}
Destructor TAbrufKommandoListe.Destroy;
{-------------------------------------}
begin
  Clear;
  inherited Destroy;
end;

{----------------------------------}
procedure TAbrufKommandoListe.Clear;
{----------------------------------}
var
  List: TList;
  i: integer;
begin
  List:=LockList;
  try
    for i:=0 to List.Count - 1 do
      if (Assigned(List[i])) then TKommandoObj (List [i]).Free;
  finally
    UnlockList;
  end;
  inherited;
end;

{----------------------------------------------------}
procedure TAbrufKommandoListe.Delete(iIndex: integer);
{----------------------------------------------------}
begin
  // Löschen der Objekte soll hier nicht erfolgen

  inherited;
end;

{-----------------------------------------------------------------------------------------}
procedure TAbrufKommandoListe.SetKommandoObj (ClientConnection: TIdTCPConnection;
                                              Kommando: string;
                                              StaKanalKonvDataList: TStaKanalKonvDataList);
{-----------------------------------------------------------------------------------------}
{ Kommando am Ende der Liste einfügen }
var
  List: TList;
  KommandoObj: TKommandoObj;
begin
  List:=LockList;
  try
    KommandoObj:=TKommandoObj.Create;
    KommandoObj.SetData (ClientConnection, Kommando, StaKanalKonvDataList);
    List.Add (KommandoObj);
  finally
    UnlockList;
  end;
end;

{--------------------------------------------------------}
function TAbrufKommandoListe.GetKommandoObj: TKommandoObj;
{--------------------------------------------------------}
{ Kommando-Objekt aus Liste lesen }
var
  List: TList;
begin
  List:=LockList;
  try
    if List.Count > 0 then begin
      Result:=TKommandoObj (List [0]);
      { Kommando aus Liste löschen:
        -> Kommando-Objekt hier nicht freigeben, da die enthaltene Information
           für den Abruf benötigt wird ! }
      List.Delete (0);
    end else
      Result:=nil;
  finally
    UnlockList;
  end;
end;

{---------------------------------------------}
function TAbrufKommandoListe.GetCount: integer;
{---------------------------------------------}
{ gibt Anzahl der in der Liste enthaltenen Kommandos zurück }
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
