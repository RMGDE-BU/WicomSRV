//------------------------------------------------------------------------------
// Basisobject für eigene Threads
//
// 05.01.2009  GD  Neu
//
// Copyright (C) Fa. Geert Dade, 2009
//------------------------------------------------------------------------------
unit O_GDThread;

interface

uses
  Windows, SysUtils, Classes, Forms;

type
  TGDThread = class(TThread)
    constructor Create(bCreateSuspended: boolean); virtual;
    destructor Destroy; override;
  private
    FLock             : TRTLCriticalSection;
    FLastStatus       : string;
    FLastError        : string;
    FExecuteException : integer;
    FTag              : integer;
    FTerminateOnFault : boolean;
  protected
    procedure InitializeThisThread(bState: boolean); virtual;
    procedure Execute; override;
    function MyExecuteAction: boolean; virtual; abstract;
    procedure HandleStatus(
      const sStatusText: string; iState: integer = 0); virtual;
    property LastStatus: string read FLastStatus write FLastStatus;
    procedure HandleError(
      const sErrorText: string; iState: integer = 0); virtual;
    property LastError: string read FLastError write FLastError;
    property ExecuteException: integer
      read FExecuteException write FExecuteException;
    procedure MyEnterCriticalSection;
    procedure MyLeaveCriticalSection;
    property TerminateOnFault: boolean write FTerminateOnFault;
  public
    property Terminated;
    property Tag: integer read FTag write FTag;
  end;

implementation

//------------------------------- TGDThread ------------------------------------

// Konstruktor
//-----------------------------------------
constructor TGDThread.Create(bCreateSuspended: boolean);
//-----------------------------------------
begin
  FLastError := '';
  FTag := 0;
  FTerminateOnFault := True;

  inherited Create(bCreateSuspended);

  FreeOnTerminate := True;  // Thread soll sich beim Beenden selbst freigeben
  Priority := tpNormal;     // Thread soll mit normaler Priorität ausgeführt werden

  InitializeCriticalSection(FLock);
  InitializeThisThread(True);
end;

// Destruktor
//-----------------------------------------
destructor TGDThread.Destroy;
//-----------------------------------------
begin
  InitializeThisThread(False);

  inherited;

  // CriticalSection wird erst NACH Destroy deleted !
  DeleteCriticalSection(FLock);
end;

// Threadausführung
//-----------------------------------------
procedure TGDThread.Execute;
//-----------------------------------------
begin
  while (not Terminated) and (not Application.Terminated) do
  try
    if (not MyExecuteAction) and (FTerminateOnFault) then begin
      Terminate;
      Break;  // Bei Fehler Thread beenden
    end;
    Sleep(1);
  except
    on E:Exception do begin
      if not(E is EAbort) then
       HandleError('TGDThread.Execute: ' + E.Message, FExecuteException);
    end;
  end;
end;

// Initialisierung/Freigeben bei Start/Ende der Ausführung
// Parameter: T=Initialisieren / F=Freigeben
//-----------------------------------------
procedure TGDThread.InitializeThisThread(bState: boolean);
//-----------------------------------------
begin
  try
    // Initialisieren
    if (bState) then begin
      FExecuteException := 0;  // Exception-Status bei Exception in Execute
    end
    // Freigeben
    else begin
    end;
  except
    on E:Exception do begin
      if not(E is EAbort) then
        HandleError('TGDThread.InitialzeThisThread: ' + E.Message);
    end;
  end;
end;

(* ABSTRACT!!!
// Zyklisch ausgeführte Aktion im Thread
// Rückgabe: Erfolg ja/nein (Misserfolg führt zu Abbruch!)
//-----------------------------------------
function TGDThread.MyExecuteAction: boolean;
//-----------------------------------------
begin
end;
*)

// Allgemeine Statusbehandlung
// Parameter: Statustext, optionaler Status
//-----------------------------------------
procedure TGDThread.HandleStatus(
  const sStatusText: string; iState: integer = 0);
//-----------------------------------------
begin
  FLastStatus := sStatusText;
end;

// Allgemeine Fehlerbehandlung
// Parameter: Fehlertext, optionaler Fehlerstatus
//-----------------------------------------
procedure TGDThread.HandleError(const sErrorText: string; iState: integer = 0);
//-----------------------------------------
begin
  FLastError := sErrorText;

  if (Pos('InitialzeThisThread', sErrorText) > 0) then Self.Terminate;
end;

// CriticalSection verriegeln - folgenden Abschnitt threadsicher behandeln
//-----------------------------------------
procedure TGDThread.MyEnterCriticalSection;
//-----------------------------------------
begin
  EnterCriticalSection(FLock);
end;

// CriticalSection freigeben - Threadsichheit aufheben
//-----------------------------------------
procedure TGDThread.MyLeaveCriticalSection;
//-----------------------------------------
begin
  LeaveCriticalSection(FLock);
end;

end.
