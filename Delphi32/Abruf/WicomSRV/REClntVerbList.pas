{******************************************************************************}
{* Unit: Liste für TCP/IP-Rufentgegennahme-Verbindungen                       *}
{* 08.01.2018 WW                                                              *}
{* 03.06.2020 WW Umstellung auf Indy-10 TCP-Connection (Vs. 10.0.52)          *}
{* 14.03.2024 WW Verbindungs-Record mit Index des Listeneintrags              *}
{******************************************************************************}
unit REClntVerbList;

interface

uses
  Windows, Classes, SysUtils, REClntThr, REClntCmdList, IdTCPConnection;

type

  { Bearbeitungsstatus des Anrufs }

  TREClntStatus = (
    rcs_New,   // Neuer Anruf
    rcs_Busy,  // Anruf in Bearbeitung
    rcs_Done   // Anruf erledigt
  );

  { Record für TCP/IP-Rufentgegennahme-Verbindung }

  TREClntVerbData = record
    Thread: TREClntThread;  // der Rufentgegennahme-Thread selbst
    ThreadRunning: boolean;  // Flag für externe Überwachung "Thread aktiv/inaktiv"
    KommandoListe: TREClntKommandoListe;  // Kommandoliste für Thread
    Status: TREClntStatus;  // Bearbeitungsstatus des Anrufs
    ListIndex: integer;  // Index des Eintrags in der Verindungen-Liste; 14.03.2024, WW
  end;

  { Objekt für TCP/IP-Rufentgegennahme-Verbindung }

  TREClntVerbDataObj = class (TObject)
    Data: TREClntVerbData;
  public
    procedure SetData (AREClntVerbData: TREClntVerbData);
  end;

  { Liste für TCP/IP-Rufentgegennahme-Verbindungen }

  TREClntVerbList = class(TThreadList)
  public
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure AddNeueVerbindung (AClientConnection: TIdTCPConnection;
      ACallingFormHandle: HWND);
    function GetNeueVerbindung (iCOMNr: integer): TREClntVerbDataObj;
    function VerbindungAbschliessen (iIndex: integer): boolean;
    procedure Aufraeumen;
//    procedure CloseThreads;
  end;

implementation

{ TREClntVerbDataObj }

{----------------------------------------------------------------------}
procedure TREClntVerbDataObj.SetData (AREClntVerbData: TREClntVerbData);
{----------------------------------------------------------------------}
{ TCP/IP-Verbindungsdaten-Record für Anruf belegen }
begin
  Data:=AREClntVerbData;
end;


{ TREClntVerbList }

{---------------------------------}
Destructor TREClntVerbList.Destroy;
{---------------------------------}
{ Liste freigeben }
begin
  Clear;
  inherited Destroy;
end;

{------------------------------}
procedure TREClntVerbList.Clear;
{------------------------------}
{ Liste leeren, enthaltene Objekte freigeben }
var
  List: TList;
  i: integer;
  REClntVerbDataObj: TREClntVerbDataObj;

begin
  List:=LockList;
  try
    for i:=0 to List.Count - 1 do begin
      if Assigned (List [i]) then begin
        REClntVerbDataObj:=TREClntVerbDataObj (List [i]);
        if Assigned (REClntVerbDataObj) then
           REClntVerbDataObj.Data.KommandoListe.Free;

        TREClntVerbDataObj (List [i]).Free;
      end;
    end;
  finally
    UnlockList;
  end;

  inherited;
end;

{-------------------------------------------------------------------------------}
procedure TREClntVerbList.AddNeueVerbindung (AClientConnection: TIdTCPConnection;
  ACallingFormHandle: HWND);
{-------------------------------------------------------------------------------}
{ Neue Rufentgegennahme-Verbindung der Liste hinzufügen;
  Übergabe: Client-Connection
            Handle des aufrufenden Fensters }
var
  List: TList;
  REClntVerbData: TREClntVerbData;
  REClntVerbDataObj: TREClntVerbDataObj;
  iThreadIndex: integer;

begin
  List:=LockList;
  try
    with REClntVerbData do begin
      KommandoListe:=TREClntKommandoListe.Create;  // Kommandoliste für Rufentgegennahme-Thread erzeugen
      KommandoListe.Duplicates:=dupAccept;  // mehrfache, gleiche Einträge erlaubt
      ListIndex:=List.Count;  // 14.03.2024, WW
                                               
      iThreadIndex:=ListIndex + 1;  // Thread-Index 1-basiert
      Thread:=TREClntThread.CreateIt (AClientConnection, iThreadIndex, KommandoListe,
                                      ACallingFormHandle);  // Rufentgegennahme-Thread erzeugen
      ThreadRunning:=true;  // Flag setzen: Thread läuft
      Status:=rcs_New;  // Bearbeitungsstatus: Neuer Anruf
    end;

    REClntVerbDataObj:=TREClntVerbDataObj.Create;
    REClntVerbDataObj.SetData (REClntVerbData);
    List.Add (REClntVerbDataObj);
  finally
    UnlockList;
  end;
end;

{-------------------------------------------------------------------------------}
function TREClntVerbList.GetNeueVerbindung (iCOMNr: integer): TREClntVerbDataObj;
{-------------------------------------------------------------------------------}
{ Verbindungs-Objekt eines neuen Anrufs aus Liste lesen;
  Übergabe: COM-Nr. der Abruflinie, welche die neue Verbindung übernimmt
  Ergebnis: Zeiger auf Verbindungs-Objekt (nil, wenn kein neuer Anruf vorliegt) }
var
  List: TList;
  VerbObj: TREClntVerbDataObj;
  i: integer;

begin
  List:=LockList;
  try
    Result:=nil;
    for i:=0 to List.Count - 1 do begin
      VerbObj:=TREClntVerbDataObj (List [i]);
      if VerbObj.Data.Status = rcs_New then begin
        VerbObj.Data.Status:=rcs_Busy;  // jetzt als 'in Bearbeitung' markieren
        VerbObj.Data.KommandoListe.COMNr:=iCOMNr;  // COM-Nr. zuweisen
        Result:=VerbObj;
        Break;  // neue Verbindung gefunden
      end;
    end;
  finally
    UnlockList;
  end;
end;

{-------------------------------------------------------------------------}
function TREClntVerbList.VerbindungAbschliessen (iIndex: integer): boolean;
{-------------------------------------------------------------------------}
{ Verbindung als abgeschlossen markieren;
  Übergabe: Listenindex der Verbindung
  Ergebnis: true, wenn Verbindung zu Index gefunden }
var
  List: TList;
  VerbObj: TREClntVerbDataObj;

begin
  List:=LockList;
  try
    if (iIndex >= 0) AND (iIndex < List.Count) then begin
      VerbObj:=TREClntVerbDataObj (List [iIndex]);
      VerbObj.Data.ThreadRunning:=false;
      VerbObj.Data.Status:=rcs_Done;  // als 'erledigt' markieren
      Result:=true;
    end else
      Result:=false;  // Verbindung für Index nicht vorhanden
  finally
    UnlockList;
  end;
end;

{-----------------------------------}
procedure TREClntVerbList.Aufraeumen;
{-----------------------------------}
{ Liste leeren, wenn alle Anrufe abgewickelt wurden }
var
  List: TList;
  VerbObj: TREClntVerbDataObj;
  i: integer;
  bAllFinished: boolean;

begin
  List:=LockList;
  try
    if List.Count > 0 then begin
      bAllFinished:=true;
      for i:=0 to List.Count - 1 do begin
        VerbObj:=TREClntVerbDataObj (List [i]);
        if VerbObj.Data.Status <> rcs_Done then begin
          bAllFinished:=false;  // Anruf noch nicht abgewickelt
          Break;
        end;
      end;

      if bAllFinished then
        Clear;  // Liste leeren
    end;
  finally
    UnlockList;
  end;
end;

(* nicht mehr verwendet
{-------------------------------------}
procedure TREClntVerbList.CloseThreads;
{-------------------------------------}
{ Rufentgegennahme-Threads beenden }
var
  List: TList;
  VerbObj: TREClntVerbDataObj;
  i: integer;

begin
  List:=LockList;
  try
    { Beenden aller noch laufenden Rufentgegennahme-Threads einleiten: }
    for i:=0 to List.Count - 1 do begin
      VerbObj:=TREClntVerbDataObj (List [i]);
      if (VerbObj.Data.Thread <> nil) AND (VerbObj.Data.ThreadRunning = true) then begin
        VerbObj.Data.Thread.Resume;    // Thread fortsetzen, falls er unterbrochen ist (sonst reagiert er nicht)
        VerbObj.Data.Thread.Terminate; // Thread-Beenden einleiten
      end;
    end;

    { auf das Ende jedes Rufentgegennahme-Threads warten: }
    for i:=0 to List.Count - 1 do begin
      VerbObj:=TREClntVerbDataObj (List [i]);
      if (VerbObj.Data.Thread <> nil) AND (VerbObj.Data.ThreadRunning = true) then begin
        try
          VerbObj.Data.Thread.WaitFor;   // warten bis Thread beendet ist
        except
          // Ungültiges Handle unterdrücken
        end;
      end;
    end;
  finally
    UnlockList;
  end;
end; *)

end.

