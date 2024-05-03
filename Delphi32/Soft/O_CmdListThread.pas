//------------------------------------------------------------------------------
// Basisobject für eigene Threads
//
// 05.01.2009  GD  Neu
//
// Copyright (C) Fa. Geert Dade, 2009
//------------------------------------------------------------------------------
unit O_CmdListThread;

interface

uses
  Windows, SysUtils, Classes, O_GDThread;

type
  TCustomCommandClass = class;
  TCustomCommandList = class;
  TCustomCommandClasses = class of TCustomCommandClass;

  TCustomCommandClass = class(TObject)
    Command : string;
  end;

  TCustomCommandList = class(TObject)
    constructor Create(pClassType : TCustomCommandClasses); virtual;
    destructor Destroy; override;
  private
    FClassType : TCustomCommandClasses;
    FIdList    : TList;
    FCmdList   : TList;
    FIdCounter : integer;
    FDelete    : boolean;
  protected
    function GetCount: integer;
    function GetCmdIndex(iIndex: integer): integer;
    function GetCmdObject(iIndex: integer): TCustomCommandClass;
    property DeleteCommands: boolean read FDelete write FDelete;
  public
    function AddCommand(pCmd: TCustomCommandClass): integer; overload;
    function AddCommand(
      pCmd: TCustomCommandClass; iCmdId: integer): integer; overload;
    function HasCommand(iCmdId: integer): boolean;
    function GetCommand(iCmdId: integer): TCustomCommandClass;
    function Clear: boolean;
    property MyClassType : TCustomCommandClasses write FClassType;
    property Count: integer read GetCount;
    property CmdIndex [iIndex: integer]: integer read GetCmdIndex;
    property CmdObject [iIndex: integer]: TCustomCommandClass
      read GetCmdObject;
  end;

  TCommandListThread = class(TGDThread)
    constructor Create(bCreateSuspended: boolean); override;
    destructor Destroy; override;
  private
    FCmdList    : TCustomCommandList;
    FAnswerList : TCustomCommandList;
    FClassType  : TCustomCommandClasses;
  protected
    procedure InitializeThisThread(bState: boolean); override;
    function MyExecuteAction: boolean; override;
    function HandleCmdList: boolean; virtual;
    function HandleThisCmd(
      iCmdId: integer; pCmd: TCustomCommandClass): boolean; virtual;
    function SetCmdClassType(
      pClassType : TCustomCommandClasses): boolean; virtual;
    function SetDeleteCmdObjects(bDelete: boolean): boolean; virtual;
  public
    function AddCommand(pCmd: TCustomCommandClass): integer;
    function HasCommand(iCmdId: integer): boolean;
    function GetCommand(iCmdId: integer): TCustomCommandClass;
    function AddAnswer(pCmd: TCustomCommandClass; iCmdId: integer): integer;
    function HasAnswer(iCmdId: integer): boolean;
    function GetAnswer(iCmdId: integer): TCustomCommandClass;
  end;

implementation

uses Variants;

//--------------------------- TCustomCommandList -------------------------------

// Konstruktor
//-----------------------------------------
constructor TCustomCommandList.Create(pClassType : TCustomCommandClasses);
//-----------------------------------------
begin
  inherited Create;

  FIdCounter := 1;            // Kommando-ID
  FIdList := TList.Create;    // Liste der IDs
  FCmdList := TList.Create;   // Liste der Kommandos
  FDelete := True;            // Kommandos nach Lesen löschen
end;

// Destruktor
//-----------------------------------------
destructor TCustomCommandList.Destroy;
//-----------------------------------------
var
  i : integer;
begin
  // Eigenen Listen freigeben
  FreeAndNil(FIdList);
  // Ggf. noch gespeicherte Kommandoobjekte freigeben
  for i := FCmdList.Count-1 downto 0 do
  try
    TObject(FCmdList[i]).Free;
  except
  // egal
  end;
  FreeAndNil(FCmdList);

  inherited;
end;

// Interne Listen leeren
// Rückgabe: Erfolg ja/nein
//-----------------------------------------
function TCustomCommandList.Clear: boolean;
//-----------------------------------------
var
  i : integer;
begin
  try
    // Eigenen Listen leeren
    FIdList.Clear;
    // Ggf. noch gespeicherte Kommandoobjekte freigeben
    for i := FCmdList.Count-1 downto 0 do
    try
      TObject(FCmdList[i]).Free;
    except
    // egal
    end;
    FCmdList.Clear;
    Result := True;
  except
    Result := False;
  end;
end;

// Anzahl der gespeicherten Elemente zurückgeben
// Ergebnis: Anzahl der Elemente
//-----------------------------------------
function TCustomCommandList.GetCount: integer;
//-----------------------------------------
begin
  try
    if (FCmdList.Count = FIdList.Count)
    then Result := FCmdList.Count
    else Result := -1;
  except
    Result := -1;
  end;
end;

// Kommando-Index eines Eintrags zurückgeben
// Parameter: Listenindex
// Rückgabe: Kommandoindex
//-----------------------------------------
function TCustomCommandList.GetCmdIndex(iIndex: integer): integer;
//-----------------------------------------
begin
  try
    if (Count > iIndex)
    then Result := Integer(FIdList[iIndex])
    else Result := -1
  except
    Result := -1;
  end;
end;

// Kommando-Objekt eines Eintrags zurückgeben
// Parameter: Listenindex
// Rückgabe: Kommandoobjekt
//-----------------------------------------
function TCustomCommandList.GetCmdObject(iIndex: integer): TCustomCommandClass;
//-----------------------------------------
begin
  try
    if (Count > iIndex)
    then Result := TCustomCommandClass(FCmdList[iIndex])
    else Result := nil
  except
    Result := nil;
  end;
end;

//-----------------------------------------
function TCustomCommandList.AddCommand(pCmd: TCustomCommandClass): integer;
//-----------------------------------------
begin
  try
    if (FCmdList.Count = FIdList.Count) and
      (pCmd.ClassName = FClassType.ClassName) then
    begin
      FCmdList.Add(pCmd);
      FIdList.Add(TObject(FIdCounter));
      Result := FIdCounter;
      Inc(FIdCounter);
    end
    else begin
      Result := -1;
      pCmd.Free;
    end;
  except
    Result := -1;
  end;
end;

//-----------------------------------------
function TCustomCommandList.AddCommand(
  pCmd: TCustomCommandClass; iCmdId: integer): integer;
//-----------------------------------------
begin
  try
    if (FCmdList.Count = FIdList.Count) and
      (pCmd.ClassName = FClassType.ClassName) then
    begin
      FCmdList.Add(pCmd);
      FIdList.Add(TObject(iCmdId));
      Result := iCmdId;
    end
    else begin
      Result := -1;
      pCmd.Free;
    end;
  except
    Result := -1;
  end;
end;

//-----------------------------------------
function TCustomCommandList.HasCommand(iCmdId: integer): boolean;
//-----------------------------------------
begin
  try
    Result := (FIdList.IndexOf(TObject(iCmdId)) >= 0);
  except
    Result := False;
  end;
end;

//-----------------------------------------
function TCustomCommandList.GetCommand(iCmdId: integer): TCustomCommandClass;
//-----------------------------------------
var
  iIx : integer;
begin
  try
    iIx := FIdList.IndexOf(TObject(iCmdId));
    if (iIx >= 0) then begin
      Result := TCustomCommandClass(FCmdList[iIx]);
      if (FDelete) then begin
        FIdList.Delete(iIx);
        FCmdList.Delete(iIx);
      end;
    end
    else Result := nil;
  except
    Result := nil;
  end;
end;

//--------------------------- TCommandListThread -------------------------------

// Konstruktor
//-----------------------------------------
constructor TCommandListThread.Create(bCreateSuspended: boolean);
//-----------------------------------------
begin
  inherited;
end;

// Destruktor
//-----------------------------------------
destructor TCommandListThread.Destroy;
//-----------------------------------------
begin
  inherited;
end;

// Initialisierung/Freigeben bei Start/Ende der Ausführung
// Parameter: T=Initialisieren / F=Freigeben
//-----------------------------------------
procedure TCommandListThread.InitializeThisThread(bState: boolean);
//-----------------------------------------
begin
  inherited;

  try
    // Initialisieren
    if (bState) then begin
      FClassType := TCustomCommandClass;
      FCmdList := TCustomCommandList.Create(FClassType);
      FCmdList.DeleteCommands := True;
      FAnswerList := TCustomCommandList.Create(FClassType);
      FAnswerList.DeleteCommands := True;
    end
    // Freigeben
    else begin
    end;
  except
    on E:Exception do begin
      if not(E is EAbort) then
        HandleError('TCommandListThread.InitialzeThisThread: ' + E.Message);
    end;
  end;
end;

// Klassentyp für Kommando-Übergabe-Objekt setzen
// Parameter: Klassentyp
// Rückgabe; Erfolg ja/nein
//-----------------------------------------
function TCommandListThread.SetCmdClassType(
  pClassType : TCustomCommandClasses): boolean;
//-----------------------------------------
begin
  try
    if (FClassType <> pClassType) then begin
      FClassType := pClassType;
      FCmdList.Clear;
      FCmdList.MyClassType := FClassType;
      FAnswerList.Clear;
      FAnswerList.MyClassType := FClassType;
      Result := True;
    end
    else Result := True;
  except
    on E:Exception do begin
      if not(E is EAbort) then HandleError(
        'TCommandListThread.SetCmdClassType: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Lösch-Flag in Commandliste setzen
// Parameter: Wert des Löschflags
// Rückgabe; Erfolg ja/nein
//-----------------------------------------
function TCommandListThread.SetDeleteCmdObjects(bDelete: boolean): boolean; 
//-----------------------------------------
begin
  try
    FCmdList.DeleteCommands := bDelete;
    Result := True;
  except
    on E:Exception do begin
      if not(E is EAbort) then HandleError(
        'TCommandListThread.SetDeleteCmdObjects: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Zyklisch ausgeführte Aktion im Thread
// Rückgabe: Erfolg ja/nein (Misserfolg führt zu Abbruch!)
//-----------------------------------------
function TCommandListThread.MyExecuteAction: boolean;
//-----------------------------------------
begin
  // KEIN INHERITED - Basismethode ist ABSTRACT

  try
//    MyEnterCriticalSection;
    try
      if (not HandleCmdList) then HandleError('Error in "HandleCmdList"');
      Result := True;
    finally
//      MyLeaveCriticalSection;
    end;
  except
    on E:Exception do begin
      if not(E is EAbort) then HandleError(
        'TCommandListThread.MyExecuteAction: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Kommandoliste abarbeiten
// Rückgabe: Erfolg ja/nein
//-----------------------------------------
function TCommandListThread.HandleCmdList: boolean;
//-----------------------------------------
var
  iCmdIndex  : integer;
  pCmdObject : TCustomCommandClass;
  sIx, sCmd  : string;
begin
  try
    while (FCmdList.Count > 0) do begin
      iCmdIndex := FCmdList.CmdIndex[0];
      pCmdObject := FCmdList.GetCommand(iCmdIndex);
      if (Assigned(pCmdObject)) and (pCmdObject is FClassType) then begin
        sIx := IntToStr(iCmdIndex);
        sCmd := pCmdObject.Command;
        if (not HandleThisCmd(iCmdIndex, pCmdObject)) then HandleError(
          'Error on TCommandListThread.HandleCmdList: ' + sIx +
          ' - >' + sCmd + '<');
      end;
    end;
    Result := True;
  except
    on E:Exception do begin
      if not(E is EAbort) then HandleError(
        'TCommandListThread.HandleCmdList: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Ein Kommando (aus Liste) behandeln
// Parameter: Kommando-Id, Kommando-Objekt
// Rückgabe: Erfolg ja/nein
//-----------------------------------------
function TCommandListThread.HandleThisCmd(
      iCmdId: integer; pCmd: TCustomCommandClass): boolean;
//-----------------------------------------
begin
  try
    Result := True;
  except
    on E:Exception do begin
      if not(E is EAbort) then HandleError(
        'TCommandListThread.HandleThisCmd: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Ein Kommando (in Liste) einfügen
// Parameter: Kommando-Objekt
// Rückgabe: Kommando-Id oder <= 0
//-----------------------------------------
function TCommandListThread.AddCommand(pCmd: TCustomCommandClass): integer;
//-----------------------------------------
begin
  MyEnterCriticalSection;
  try
    try
      Result := FCmdList.AddCommand(pCmd);
    except
      on E:Exception do begin
        if not(E is EAbort) then HandleError(
          'TCommandListThread.AddCommand: ' + E.Message);
        Result := -1;
      end;
    end;
  finally
    MyLeaveCriticalSection;
  end;
end;

// Ein Kommando (in Liste) enthalten?
// Parameter: Kommando-ID
// Rückgabe: Kommando-Id vorhanden ja/nein
//-----------------------------------------
function TCommandListThread.HasCommand(iCmdId: integer): boolean;
//-----------------------------------------
begin
  MyEnterCriticalSection;
  try
    try
      Result := FCmdList.HasCommand(iCmdId);
    except
      on E:Exception do begin
        if not(E is EAbort) then HandleError(
          'TCommandListThread.HasCommand: ' + E.Message);
        Result := False;
      end;
    end;
  finally
    MyLeaveCriticalSection;
  end;
end;

// Ein Kommando (aus Liste) zurückgeben und löschen
// Parameter: Kommando-ID
// Rückgabe: Kommando-Objekt oder nil
//-----------------------------------------
function TCommandListThread.GetCommand(iCmdId: integer): TCustomCommandClass;
//-----------------------------------------
begin
  MyEnterCriticalSection;
  try
    try
      Result := FCmdList.GetCommand(iCmdId);
    except
      on E:Exception do begin
        if not(E is EAbort) then HandleError(
          'TCommandListThread.GetCommand: ' + E.Message);
        Result := nil;
      end;
    end;
  finally
    MyLeaveCriticalSection;
  end;
end;

// Eine Kommando-Antwort (in Liste) einfügen
// Parameter: Kommando-Objekt, Kommando-ID
// Rückgabe: Kommando-Id oder <= 0
//-----------------------------------------
function TCommandListThread.AddAnswer(
  pCmd: TCustomCommandClass; iCmdId: integer): integer;
//-----------------------------------------
begin
  MyEnterCriticalSection;
  try
    try
      Result := FAnswerList.AddCommand(pCmd, iCmdId);
    except
      on E:Exception do begin
        if not(E is EAbort) then HandleError(
          'TCommandListThread.AddAnswer: ' + E.Message);
        Result := -1;
      end;
    end;
  finally
    MyLeaveCriticalSection;
  end;
end;

// Eine Kommando-Antwort (in Liste) enthalten?
// Parameter: Kommando-ID
// Rückgabe: Kommando-Id vorhanden ja/nein
//-----------------------------------------
function TCommandListThread.HasAnswer(iCmdId: integer): boolean;
//-----------------------------------------
begin
  MyEnterCriticalSection;
  try
    try
      Result := FAnswerList.HasCommand(iCmdId);
    except
      on E:Exception do begin
        if not(E is EAbort) then HandleError(
          'TCommandListThread.HasAnswer: ' + E.Message);
        Result := False;
      end;
    end;
  finally
    MyLeaveCriticalSection;
  end;
end;

// Eine Kommando-Antwort (aus Liste) zurückgeben und löschen
// Parameter: Kommando-ID
// Rückgabe: Kommando-Objekt oder nil
//-----------------------------------------
function TCommandListThread.GetAnswer(iCmdId: integer): TCustomCommandClass;
//-----------------------------------------
begin
  MyEnterCriticalSection;
  try
    try
      Result := FAnswerList.GetCommand(iCmdId);
    except
      on E:Exception do begin
        if not(E is EAbort) then HandleError(
          'TCommandListThread.GetAnswer: ' + E.Message);
        Result := nil;
      end;
    end;
  finally
    MyLeaveCriticalSection;
  end;
end;

end.
