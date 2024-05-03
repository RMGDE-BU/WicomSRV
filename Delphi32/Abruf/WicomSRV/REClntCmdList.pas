{******************************************************************************}
{* Unit: Kommandoliste für TCP/IP-Rufentgegennahme-Thread                     *}
{* 08.01.2018  WW                                                             *}
{******************************************************************************}
unit REClntCmdList;

interface

uses
  Classes, SysUtils, O_Comm, WComm;

type
  { Record mit Parametern für Kommando 'SendCommand' }

  TREClntCmd_SendCommand = record
    // Übergabeparameter:
    Befehl: string;
    Endezeichen: TEndezeichenSet;
    EndezeichenAnzahl: integer;
    Timeout: integer;
    AnswerDest: TAnswerDest;
    AnzCharsToReceive: cardinal;
    // Rückgabeparameter:
    Rueckgabe: TRueckgabe;
    NoCarrier: boolean;
    // Ergebnis:
    Ergebnis: boolean;
  end;

  { Record mit Parametern für Kommando 'SetExtensionmode' }

  TREClntCmd_SetExtensionmode = record
    // Übergabeparameter:
    ExtMode: byte;
    // Ergebnis:
    Ergebnis: byte;
  end;

  { Record mit Parametern für Kommando 'Disconnect' }

  TREClntCmd_Disconnect = record
    // Ergebnis:
    Ergebnis: boolean;
  end;


  { Basis-Objekt für Kommandoliste }

  TREClntCmdObj = class(TObject)
  private
    FCompleted: boolean;
  public
    procedure SetData;
    property Completed: boolean read FCompleted;
  end;

  { Objekt für Kommandoliste zu Kommando 'SendCommand' }

  TREClntCmdObj_SendCommand = class(TREClntCmdObj)
  private
    FCmd: TREClntCmd_SendCommand;
  public
    procedure SetData (ACmd: TREClntCmd_SendCommand);
    property Cmd: TREClntCmd_SendCommand read FCmd;
  end;

  { Objekt für Kommandoliste zu Kommando 'SetExtensionmode' }

  TREClntCmdObj_SetExtensionmode = class(TREClntCmdObj)
  private
    FCmd: TREClntCmd_SetExtensionmode;
  public
    procedure SetData (ACmd: TREClntCmd_SetExtensionmode);
    property Cmd: TREClntCmd_SetExtensionmode read FCmd;
  end;

  { Objekt für Kommandoliste zu Kommando 'Disconnect' }

  TREClntCmdObj_Disconnect = class(TREClntCmdObj)
  private
    FCmd: TREClntCmd_Disconnect;
  public
    procedure SetData (ACmd: TREClntCmd_Disconnect);
    property Cmd: TREClntCmd_Disconnect read FCmd;
  end;


  { Kommandoliste für TCP/IP-Rufentgegennahme-Thread }

  TREClntKommandoListe = class(TThreadList)
  private
    FCOMNr: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Delete(iIndex: integer); virtual;

    procedure SetKommandoObj_SendCommand (ACmd: TREClntCmd_SendCommand);
    procedure SetKommandoObj_SetExtensionmode (ACmd: TREClntCmd_SetExtensionmode);
    procedure SetKommandoObj_Disconnect (ACmd: TREClntCmd_Disconnect);

    function SetKommandoObj_Completed: boolean;
    function SetKommandoObj_SendCommand_Completed (ARueckgabe: TRueckgabe;
      ANoCarrier: boolean; bErgebnis: boolean): boolean;
    function SetKommandoObj_SetExtensionmode_Completed (iErgebnis: byte): boolean;

    function GetKommandoObj (bCompleted: boolean): TREClntCmdObj;
    procedure DeleteKommandoObj;

    property COMNr: integer read FCOMNr write FCOMNr;
  end;

implementation

uses TypInfo;

{ TREClntCmdObj }

{------------------------------}
procedure TREClntCmdObj.SetData;
{------------------------------}
{ Daten-Record mit Kommando-Basisdaten belegen }
begin
  FCompleted:=false;  // Neues Kommando, noch nicht abgearbeitet
end;


{ TREClntKommandoObj_SendCommand }

{-------------------------------------------------------------------------}
procedure TREClntCmdObj_SendCommand.SetData (ACmd: TREClntCmd_SendCommand);
{-------------------------------------------------------------------------}
{ Daten-Record belegen für Kommando 'SendCommand' }
begin
  inherited SetData;
  FCmd:=ACmd;

  // Rückgabeparameter und Ergebnis vorbelegen:
  with FCmd.Rueckgabe do begin
    Fehlergruppe:=-1;
    Fehlercode:=-1;
    Antwort:='';
  end;
  FCmd.NoCarrier:=true;
  FCmd.Ergebnis:=false;
end;


{ TREClntKommandoObj_SetExtensionmode }

{-----------------------------------------------------------------------------------}
procedure TREClntCmdObj_SetExtensionmode.SetData (ACmd: TREClntCmd_SetExtensionmode);
{-----------------------------------------------------------------------------------}
{ Daten-Record belegen für Kommando 'SetExtensionmode' }
begin
  inherited SetData;
  FCmd:=ACmd;

  // Ergebnis vorbelegen:
  FCmd.Ergebnis:=0;
end;


{ TREClntKommandoObj_Disconnect }

{-----------------------------------------------------------------------}
procedure TREClntCmdObj_Disconnect.SetData (ACmd: TREClntCmd_Disconnect);
{-----------------------------------------------------------------------}
{ Daten-Record belegen für Kommando 'Disconnect' }
begin
  inherited SetData;
  FCmd:=ACmd;

  // Ergebnis vorbelegen:
  FCmd.Ergebnis:=false;
end;


{ TREClntKommandoListe }

{--------------------------------------}
constructor TREClntKommandoListe.Create;
{--------------------------------------}
begin
  inherited Create;
  FCOMNr:=0;  // Default für COM-Nummer
end;

{--------------------------------------}
destructor TREClntKommandoListe.Destroy;
{--------------------------------------}
{ Liste freigeben }
begin
  Clear;
  inherited Destroy;
end;

{-----------------------------------}
procedure TREClntKommandoListe.Clear;
{-----------------------------------}
{ Liste leeren, enthaltene Objekte freigeben }
var
  List: TList;
  i: integer;

begin
  List:=LockList;
  try
    for i:=0 to List.Count - 1 do
      if (Assigned(List[i])) then
        TObject (List [i]).Free;
  finally
    UnlockList;
  end;

  inherited;
end;

{-----------------------------------------------------}
procedure TREClntKommandoListe.Delete(iIndex: integer);
{-----------------------------------------------------}
{ Listeneintrag löschen, enthaltenes Objekt freigeben }
var
  List: TList;

begin
  List:=LockList;
  try
    if (Assigned(List[iIndex])) then
      TObject (List [iIndex]).Free;
  finally
    UnlockList;
  end;

  inherited;
end;

{---------------------------------------------------------}
procedure TREClntKommandoListe.SetKommandoObj_SendCommand (
  ACmd: TREClntCmd_SendCommand);
{---------------------------------------------------------}
{ Kommando 'SendCommand' am Ende der Liste einfügen }
var
  List: TList;
  KommandoObj: TREClntCmdObj_SendCommand;

begin
  List:=LockList;
  try
    KommandoObj:=TREClntCmdObj_SendCommand.Create;
    KommandoObj.SetData (ACmd);
    List.Add (KommandoObj);
  finally
    UnlockList;
  end;
end;

{--------------------------------------------------------------}
procedure TREClntKommandoListe.SetKommandoObj_SetExtensionmode (
  ACmd: TREClntCmd_SetExtensionmode);
{--------------------------------------------------------------}
{ Kommando 'SetExtensionmode' am Ende der Liste einfügen }
var
  List: TList;
  KommandoObj: TREClntCmdObj_SetExtensionmode;

begin
  List:=LockList;
  try
    KommandoObj:=TREClntCmdObj_SetExtensionmode.Create;
    KommandoObj.SetData (ACmd);
    List.Add (KommandoObj);
  finally
    UnlockList;
  end;
end;

{--------------------------------------------------------}
procedure TREClntKommandoListe.SetKommandoObj_Disconnect (
  ACmd: TREClntCmd_Disconnect);
{--------------------------------------------------------}
{ Kommando ''Disconnect am Ende der Liste einfügen }
var
  List: TList;
  KommandoObj: TREClntCmdObj_Disconnect;

begin
  List:=LockList;
  try
    KommandoObj:=TREClntCmdObj_Disconnect.Create;
    KommandoObj.SetData (ACmd);
    List.Add (KommandoObj);
  finally
    UnlockList;
  end;
end;


{-----------------------------------------------------------------------}
function TREClntKommandoListe.SetKommandoObj_SendCommand_Completed (
  ARueckgabe: TRueckgabe; ANoCarrier: boolean; bErgebnis: boolean): boolean;
{-----------------------------------------------------------------------}
{ Ergebnis des Kommando 'SendCommand' in die Liste eintragen und Kommando als
  erledigt kennzeichnen }
var
  List: TList;
  KommandoObj: TREClntCmdObj;

begin
  List:=LockList;
  try
    if List.Count > 0 then begin
      KommandoObj:=TREClntCmdObj (List [0]);  // nur 1 Kommando-Objekt enthalten
      TREClntCmdObj_SendCommand (KommandoObj).FCmd.Rueckgabe:=ARueckgabe;
      TREClntCmdObj_SendCommand (KommandoObj).FCmd.NoCarrier:=ANoCarrier;
      TREClntCmdObj_SendCommand (KommandoObj).FCmd.Ergebnis:=bErgebnis;
      TREClntCmdObj (KommandoObj).FCompleted:=true;  // als 'erledigt' kennzeichnen
      Result:=true;
    end else
      Result:=false;
  finally
    UnlockList;
  end;
end;

{-----------------------------------------------------------------------}
function TREClntKommandoListe.SetKommandoObj_SetExtensionmode_Completed (
  iErgebnis: byte): boolean;
{-----------------------------------------------------------------------}
{ Ergebnis des Kommando 'SetExtensionmode' in die Liste eintragen und Kommando als
  erledigt kennzeichnen }
var
  List: TList;
  KommandoObj: TREClntCmdObj;

begin
  List:=LockList;
  try
    if List.Count > 0 then begin
      KommandoObj:=TREClntCmdObj (List [0]);  // nur 1 Kommando-Objekt enthalten
      TREClntCmdObj_SetExtensionmode (KommandoObj).FCmd.Ergebnis:=iErgebnis;
      TREClntCmdObj (KommandoObj).FCompleted:=true;  // als 'erledigt' kennzeichnen
      Result:=true;
    end else
      Result:=false;
  finally
    UnlockList;
  end;
end;

{--------------------------------------------------------------}
function TREClntKommandoListe.SetKommandoObj_Completed: boolean;
{--------------------------------------------------------------}
{ Kommando als erledigt kennzeichnen }
var
  List: TList;
  KommandoObj: TREClntCmdObj;

begin
  List:=LockList;
  try
    if List.Count > 0 then begin
      KommandoObj:=TREClntCmdObj (List [0]);  // nur 1 Kommando-Objekt enthalten
      TREClntCmdObj (KommandoObj).FCompleted:=true;  // als 'erledigt' kennzeichnen
      Result:=true;
    end else
      Result:=false;
  finally
    UnlockList;
  end;
end;

{--------------------------------------------------------------------------------}
function TREClntKommandoListe.GetKommandoObj (bCompleted: boolean): TREClntCmdObj;
{--------------------------------------------------------------------------------}
{ Kommando-Objekt aus Liste lesen;
  Übergabe: Flag 'bCompleted' (True: Nur abgeschlossenes Kommando zurückgeben;
                               False: Nur nicht-abgeschlossenes Kommando zurückgeben) }
var
  List: TList;
  KommandoObj: TREClntCmdObj;

begin
  List:=LockList;
  try
    if List.Count > 0 then begin
      KommandoObj:=TREClntCmdObj (List [0]);  // nur 1 Kommando-Objekt enthalten
      if KommandoObj.Completed = bCompleted then
        Result:=KommandoObj
      else
        Result:=nil;
    end else
      Result:=nil;
  finally
    UnlockList;
  end;
end;

{-----------------------------------------------}
procedure TREClntKommandoListe.DeleteKommandoObj;
{-----------------------------------------------}
{ Kommando-Objekt aus Liste löschen }
var
  List: TList;
begin
  List:=LockList;
  try
    if List.Count > 0 then begin
      List.Delete (0);  // nur 1 Kommando-Objekt enthalten
    end;
  finally
    UnlockList;
  end;
end;

end.
