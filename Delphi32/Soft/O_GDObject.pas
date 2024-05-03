//------------------------------------------------------------------------------
// Allgemines Basisobject
//
// 05.01.2009  GD  Neu
//
// Copyright (C) Fa. Geert Dade, 2009
//------------------------------------------------------------------------------
unit O_GDObject;

interface

uses
  SysUtils;

type
  TGDObject = class(TObject)
    constructor Create; virtual;
    destructor Destroy; override;
  private
    FLastError     : string;
    FLastStatus    : string;
    FInitException : integer;
    FTag           : integer;
  protected
    procedure MyInitializeObject(bState: boolean); virtual;
    procedure HandleError(
      const sErrorText: string; iState: integer = 0); virtual;
    procedure HandleStatus(
      const sStatusText: string; iState: integer = 0); virtual;
    property InitException: integer
      read FInitException write FInitException;
    property Tag: integer read FTag write FTag;
  public
    property LastError: string read FLastError write FLastError;
    property LastStatus: string read FLastStatus write FLastStatus;
  end;

implementation

//-------------------------------- TGDObject -----------------------------------

// Konstruktor
//-----------------------------------------
constructor TGDObject.Create;
//-----------------------------------------
begin
  FLastError := '';
  FLastStatus := '';
  FTag := 0;

  inherited;

  MyInitializeObject(True);
end;

// Destruktor
//-----------------------------------------
destructor TGDObject.Destroy;
//-----------------------------------------
begin
  MyInitializeObject(False);

  inherited;
end;

// Initialisierung/Freigeben bei Start/Ende der Ausführung
// Parameter: T=Initialisieren / F=Freigeben
//-----------------------------------------
procedure TGDObject.MyInitializeObject(bState: boolean);
//-----------------------------------------
begin
  try
    FInitException := -1;
    if (bState) then begin

    end
    else begin
    end;
  except
    on E:Exception do begin
      if (not (E is EAbort)) then HandleError('TGDObject.MyInitializeObject: ' +
        E.Message, FInitException);
    end;
  end;
end;

// Allgemeine Fehlerbehandlung
// Parameter: Fehlertext, optionaler Fehlerstatus
//-----------------------------------------
procedure TGDObject.HandleError(
  const sErrorText: string; iState: integer = 0); 
//-----------------------------------------
begin
  FLastError := sErrorText;
end;

// Allgemeine Statusbehandlung
// Parameter: Statustext, optionaler Statusstatus (dolles Wort !)
//-----------------------------------------
procedure TGDObject.HandleStatus(
  const sStatusText: string; iState: integer = 0);
//-----------------------------------------
begin
  FLastStatus := sStatusText;
end;

end.
