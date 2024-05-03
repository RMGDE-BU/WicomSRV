{******************************************************************************}
{* Unit: Liste für GPRS-Verbindungen                                          *}
{* 03.03.2006 WW                                                              *}
{******************************************************************************}
unit GPRSVerbList;

interface

uses
  Classes, SysUtils, ComCtrls, ExtCtrls, WChars, Novell, WSysCon;

type
  TAnzeigeFelder = (af_All,
                    af_Aktiv, af_IPAdresse, af_Port,
                    af_AnzRecTelegr, af_AnzRecBloecke, af_AnzRecBytes,
                    af_AnzSendTelegr, af_AnzSendBytes,
                    af_AnzVerbOpen, af_AnzVerbClose,
                    af_Kennung, af_GerTypName,
                    af_LetzteAktion);

  TAnzeigeFelderSet = set of TAnzeigeFelder;

  { Record für IP-Verbindungsdaten }

  TGPRSVerbData = record
    IP_Adresse: string;  // Schlüsselfeld
    Port: integer;  // ab 22.01.2009 kein Schlüsselfeld mehr; WW
    Aktiv: boolean;
    AnzRecTelegramme: integer;
    AnzRecBloecke: integer;
    AnzRecBytes: integer;
    AnzSendTelegramme: integer;
    AnzSendBytes: integer;
    AnzVerbOpen: integer;
    AnzVerbClose: integer;
    Kennung: string;
    GerTypName: string;
    GerTypNr: integer;
    LetzteAktion: TDateTime;
    Pull_Aktiv: boolean;  // Flag, ob GPRS-Pull-Betrieb läuft
    ZeitSync_PCTime: TDateTime;  // Zeitsynchronisation: PC-Zeit
    ZeitSync_DiffSec: integer;  // Zeitsynchronisation: Abweichung PC-/Gerätezeit in s
  end;

  { Objekt für IP-Verbindungsdaten }

  TGPRSVerbDataObj = class (TObject)
    Data: TGPRSVerbData;
  public
    procedure SetData (AGPRSVerbData: TGPRSVerbData);
  end;

  { Liste für IP-Verbindungen }

  TGPRSVerbList = class(TThreadList)
  private
    FAnzeigeFelder: TAnzeigeFelderSet;
    FVerbData_Changed: boolean;
  public
    constructor Create (AAnzeigeFelder: TAnzeigeFelderSet);
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure GetList (L: TList);
    function FindVerbindung (aIP_Adresse: string): integer;
    function GetAktiveVerbindungByKennung (aKennung: string;
      var IP_Adresse: string): integer;
    function GetAktiv (aIP_Adresse: string): boolean;
    function GetGeraetetypNr (aIP_Adresse: string): integer;
    function GetKennung (aIP_Adresse: string): string;
    function GetPull_Aktiv (aIP_Adresse: string): boolean;
    procedure SetPull_Aktiv (aIP_Adresse: string; Value: boolean);
    function GetZeitSync_PCTime (aIP_Adresse: string; var PCTime: TDateTime): boolean;
    procedure SetZeitSync_PCTime (aIP_Adresse: string; Value: TDateTime);
    function GetZeitSync_DiffSec (aIP_Adresse: string; var DiffSec: integer): boolean;
    procedure SetZeitSync_DiffSec (aIP_Adresse: string; Value: integer);
    procedure IncrementVerbCount (isVerbOpen: boolean; aIP_Adresse: string; aPort: integer);
    function IncrementRecCounts (aIP_Adresse: string; incRecTelegramme: integer;
                                 incRecBloecke: integer; incRecBytes: integer): boolean;
    function IncrementSendCounts (aIP_Adresse: string; incSendTelegramme: integer;
                                  incSendBytes: integer): boolean;
    function UpdateMRG (aIP_Adresse: string; aKennung: string;
                        aGerTypName: string): boolean;
    function UpdateGeraetetyp (aIP_Adresse: string; aGerTypName: string;
                               aGerTypNr: integer): boolean;
    function UpdateKennung (aIP_Adresse: string; aKennung: string): boolean;
    procedure VerbindungenAnzeigen (lvVerbindungen: TListView;
                                    pVerbAktiv, pVerbInaktiv: TPanel);
    function WriteStatistikFile (dtServerStart: TDateTime; sServerLaufzeit: string): boolean;
    function WriteClientInfoFile (sFilename: string): boolean;
    property VerbData_Changed: boolean read FVerbData_Changed write FVerbData_Changed;
  end;

implementation

{ TGPRSVerbDataObj }

{----------------------------------------------------------------}
procedure TGPRSVerbDataObj.SetData (AGPRSVerbData: TGPRSVerbData);
{----------------------------------------------------------------}
{ IP-Verbindungsdaten-Record belegen }
begin
  Data:=AGPRSVerbData;
end;


{ TGPRSVerbList }

{-------------------------------------------------------------------}
constructor TGPRSVerbList.Create (AAnzeigeFelder: TAnzeigeFelderSet);
{-------------------------------------------------------------------}
begin
  inherited Create;
  FAnzeigeFelder:=AAnzeigeFelder;
  FVerbData_Changed:=false;
end;

{-------------------------------}
Destructor TGPRSVerbList.Destroy;
{-------------------------------}
{ Liste freigeben }
begin
  Clear;
  inherited Destroy;
end;

{----------------------------}
procedure TGPRSVerbList.Clear;
{----------------------------}
{ Liste leeren }
var
  List: TList;
  i: integer;

begin
  List:=LockList;
  try
    for i:=0 to List.Count - 1 do
      if Assigned (List [i]) then
        TGPRSVerbDataObj (List [i]).Free;
  finally
    UnlockList;
  end;

  inherited;
  FVerbData_Changed:=true;  // Flag setzen: Verbindungsdaten haben sich geändert
end;

{-----------------------------------------}
procedure TGPRSVerbList.GetList (L: TList);
{-----------------------------------------}
{ liefert Kopie der Liste }
var
  List: TList;
  i: integer;
  GPRSVerbData: TGPRSVerbData;
  GPRSVerbDataObj: TGPRSVerbDataObj;

begin
  if not Assigned (L) then exit;

  List:=LockList;
  try
    for i:=0 to List.Count - 1 do begin
      GPRSVerbData:=TGPRSVerbDataObj (List [i]).Data;
      GPRSVerbDataObj:=TGPRSVerbDataObj.Create;
      GPRSVerbDataObj.SetData (GPRSVerbData);
      L.Add (GPRSVerbDataObj);
    end;
  finally
    UnlockList;
  end;
end;

{-------------------------------------------------------------------}
function TGPRSVerbList.FindVerbindung (aIP_Adresse: string): integer;
{-------------------------------------------------------------------}
{ Verbindung in Liste über IP-Adresse suchen;
  Übergabe: IP-Adresse
  Ergebnis: Listenindex des gefundenen Eintrags bzw. -1, wenn nicht gefunden }
var
  List: TList;
  i: integer;

begin
  Result:=-1;
  List:=LockList;
  try
    for i:=0 to List.Count - 1 do begin
      if (TGPRSVerbDataObj (List [i]).Data.IP_Adresse = aIP_Adresse) then begin
        Result:=i;
        Break;
      end;
    end;
  finally
    UnlockList;
  end;
end;

{--------------------------------------------------------------------}
function TGPRSVerbList.GetAktiveVerbindungByKennung (aKennung: string;
  var IP_Adresse: string): integer;
{--------------------------------------------------------------------}
{ Aktive Verbindung für Kennung in Liste suchen;
  Übergabe: Kennung
  Rückgabe: IP-Adresse der aktiven Verbindung (leer, wenn keine Verbindung zu
            Kennung besteht)
  Ergebnis:  0 = Aktive Verbindung vorhanden, Kennung eindeutig
            -1 = Aktive Verbindung vorhanden, Kennung nicht eindeutig
            -2 = Keine aktive Verbindung vorhanden }
var
  List: TList;
  i: integer;

begin
  Result:=-2;  // keine aktive Verbindung zu Kennung vorhanden
  IP_Adresse:='';

  List:=LockList;
  try
    // ws müssen alle Listeneinträge durchsucht werden, um erkennen zu können, ob
    // die Kennung mehrfach vorhanden ist:
    for i:=0 to List.Count - 1 do begin
      if (TGPRSVerbDataObj (List [i]).Data.Kennung = aKennung) AND
         TGPRSVerbDataObj (List [i]).Data.Aktiv then begin
        if length (IP_Adresse) > 0 then
          Result:=-1  // Kennung mehrfach vorhanden
        else
          Result:=0;  // Kennung eindeutig
        IP_Adresse:=TGPRSVerbDataObj (List [i]).Data.IP_Adresse;
      end;
    end;
  finally
    UnlockList;
  end;
end;

{-------------------------------------------------------------}
function TGPRSVerbList.GetAktiv (aIP_Adresse: string): boolean;
{-------------------------------------------------------------}
{ liest "Aktiv"-Flag einer Verbindung;
  Übergabe: IP-Adresse der GPRS-Verbindung
  Ergebnis: true, wenn "Aktiv"-Flag gesetzt ist }
var
  List: TList;
  i: integer;

begin
  Result:=false;
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin
    List:=LockList;
    try
      Result:=TGPRSVerbDataObj (List [i]).Data.Aktiv;
    finally
      UnlockList;
    end;
  end;
end;

{--------------------------------------------------------------------}
function TGPRSVerbList.GetGeraetetypNr (aIP_Adresse: string): integer;
{--------------------------------------------------------------------}
{ liefert Gerätetypnummer einer Verbindung;
  Übergabe: IP-Adresse der GPRS-Verbindung
  Ergebnis: Gerätetypnummer (-1: unbekannter Gerätetyp) }
var
  List: TList;
  i: integer;

begin
  Result:=-1;
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin
    List:=LockList;
    try
      Result:=TGPRSVerbDataObj (List [i]).Data.GerTypNr;
    finally
      UnlockList;
    end;
  end;
end;

{--------------------------------------------------------------}
function TGPRSVerbList.GetKennung (aIP_Adresse: string): string;
{--------------------------------------------------------------}
{ liefert Kennung einer Verbindung;
  Übergabe: IP-Adresse der GPRS-Verbindung
  Ergebnis: Kennung}
var
  List: TList;
  i: integer;

begin
  Result:='';
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin
    List:=LockList;
    try
      Result:=TGPRSVerbDataObj (List [i]).Data.Kennung;
    finally
      UnlockList;
    end;
  end;
end;

{------------------------------------------------------------------}
function TGPRSVerbList.GetPull_Aktiv (aIP_Adresse: string): boolean;
{------------------------------------------------------------------}
{ liest "Pull-Aktiv"-Flag einer Verbindung;
  Übergabe: IP-Adresse der GPRS-Verbindung
  Ergebnis: true, wenn "Pull-Aktiv"-Flag gesetzt ist }
var
  List: TList;
  i: integer;

begin
  Result:=false;
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin
    List:=LockList;
    try
      Result:=TGPRSVerbDataObj (List [i]).Data.Pull_Aktiv;
    finally
      UnlockList;
    end;
  end;
end;

{--------------------------------------------------------------------------}
procedure TGPRSVerbList.SetPull_Aktiv (aIP_Adresse: string; Value: boolean);
{--------------------------------------------------------------------------}
{ setzt "Pull-Aktiv"-Flag einer Verbindung;
  Übergabe: IP-Adresse der GPRS-Verbindung
            "Pull-Aktiv"-Flag (ein/aus) }
var
  List: TList;
  i: integer;

begin
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin
    List:=LockList;
    try
      TGPRSVerbDataObj (List [i]).Data.Pull_Aktiv:=Value;
    finally
      UnlockList;
    end;
    FVerbData_Changed:=true;  // Flag setzen: Verbindungsdaten haben sich geändert
  end;
end;

{-------------------------------------------------------------}
function TGPRSVerbList.GetZeitSync_PCTime (aIP_Adresse: string;
  var PCTime: TDateTime): boolean;
{-------------------------------------------------------------}
{ liest die bei Zeitsynchronisation gemerkte PC-Zeit einer Verbindung;
  Übergabe: IP-Adresse der GPRS-Verbindung
  Rückgabe: PC-Zeit
  Ergebnis: true, wenn Abweichung gelesen werden konnte }
var
  List: TList;
  i: integer;

begin
  Result:=false;
  PCTime:=0;
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin
    List:=LockList;
    try
      PCTime:=TGPRSVerbDataObj (List [i]).Data.ZeitSync_PCTime;
      Result:=true;
    finally
      UnlockList;
    end;
  end;
end;

{---------------------------------------------------------------------------------}
procedure TGPRSVerbList.SetZeitSync_PCTime (aIP_Adresse: string; Value: TDateTime);
{---------------------------------------------------------------------------------}
{ setzt die bei Zeitsynchronisation ermittelte PC-Zeit einer Verbindung;
  Übergabe: IP-Adresse der GPRS-Verbindung
            PC-Zeit }
var
  List: TList;
  i: integer;

begin
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin
    List:=LockList;
    try
      TGPRSVerbDataObj (List [i]).Data.ZeitSync_PCTime:=Value;
    finally
      UnlockList;
    end;
  end;
end;

{--------------------------------------------------------------}
function TGPRSVerbList.GetZeitSync_DiffSec (aIP_Adresse: string;
  var DiffSec: integer): boolean;
{--------------------------------------------------------------}
{ liest die bei Zeitsynchronisation ermittelte Abweichung PC-/Gerätezeit einer Verbindung;
  Übergabe: IP-Adresse der GPRS-Verbindung
  Rückgabe: Abweichung
  Ergebnis: true, wenn Abweichung gelesen werden konnte }
var
  List: TList;
  i: integer;

begin
  Result:=false;
  DiffSec:=-1;
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin
    List:=LockList;
    try
      DiffSec:=TGPRSVerbDataObj (List [i]).Data.ZeitSync_DiffSec;
      Result:=true;
    finally
      UnlockList;
    end;
  end;
end;

{--------------------------------------------------------------------------------}
procedure TGPRSVerbList.SetZeitSync_DiffSec (aIP_Adresse: string; Value: integer);
{--------------------------------------------------------------------------------}
{ setzt die bei Zeitsynchronisation ermittelte Abweichung PC-/Gerätezeit einer Verbindung;
  Übergabe: IP-Adresse der GPRS-Verbindung
            Abweichung }
var
  List: TList;
  i: integer;

begin
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin
    List:=LockList;
    try
      TGPRSVerbDataObj (List [i]).Data.ZeitSync_DiffSec:=Value;
    finally
      UnlockList;
    end;
  end;
end;

{----------------------------------------------------------------------------------}
function TGPRSVerbList.IncrementRecCounts (aIP_Adresse: string;
  incRecTelegramme: integer; incRecBloecke: integer; incRecBytes: integer): boolean;
{----------------------------------------------------------------------------------}
{ Empfangsdaten-Zähler einer Verbindung inkrementieren;
  Übergabe: IP-Adresse der GPRS-Verbindung
            Inkrement für Anzahl empfangener Telegramme
            Inkrement für Anzahl empfangener Blöcke
            Inkrement für Anzahl empfangener Bytes
  Ergebnis: true, wenn GPRS-Verbindung in Liste vorhanden ist }
var
  List: TList;
  i: integer;

begin
  Result:=false;
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin
    List:=LockList;
    try
      inc (TGPRSVerbDataObj (List [i]).Data.AnzRecTelegramme, incRecTelegramme);
      inc (TGPRSVerbDataObj (List [i]).Data.AnzRecBloecke, incRecBloecke);
      inc (TGPRSVerbDataObj (List [i]).Data.AnzRecBytes, incRecBytes);
      TGPRSVerbDataObj (List [i]).Data.LetzteAktion:=Now;
    finally
      UnlockList;
    end;
    FVerbData_Changed:=true;  // Flag setzen: Verbindungsdaten haben sich geändert
    Result:=true;
  end;
end;

{--------------------------------------------------------------}
function TGPRSVerbList.IncrementSendCounts (aIP_Adresse: string;
  incSendTelegramme: integer; incSendBytes: integer): boolean;
{--------------------------------------------------------------}
{ Sendedaten-Zähler einer Verbindung inkrementieren;
  Übergabe: IP-Adresse der GPRS-Verbindung
            Inkrement für Anzahl gesendeter Telegramme
            Inkrement für Anzahl gesendeter Bytes
  Ergebnis: true, wenn GPRS-Verbindung in Liste vorhanden ist }
var
  List: TList;
  i: integer;

begin
  Result:=false;
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin
    List:=LockList;
    try
      inc (TGPRSVerbDataObj (List [i]).Data.AnzSendTelegramme, incSendTelegramme);
      inc (TGPRSVerbDataObj (List [i]).Data.AnzSendBytes, incSendBytes);
      TGPRSVerbDataObj (List [i]).Data.LetzteAktion:=Now;
    finally
      UnlockList;
    end;
    FVerbData_Changed:=true;  // Flag setzen: Verbindungsdaten haben sich geändert
    Result:=true;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TGPRSVerbList.IncrementVerbCount (isVerbOpen: boolean;
                                            aIP_Adresse: string; aPort: integer);
{-------------------------------------------------------------------------------}
{ Verbindungs-Zähler inkrementieren; wenn keine Verbindung besteht, neu eintragen.
  Übergabe: Flag isverbOpen (true: Anzahl 'Verbindung geöffnet' wird inkrementiert
                             false: Anzahl 'Verbindung geschlossen' wird inkrementiert)
            IP-Adresse
            Port }
var
  List: TList;
  i: integer;
  GPRSVerbData: TGPRSVerbData;
  GPRSVerbDataObj: TGPRSVerbDataObj;

begin
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin  // gefunden
    List:=LockList;
    try
      if isVerbOpen then begin
        inc (TGPRSVerbDataObj (List [i]).Data.AnzVerbOpen);
        TGPRSVerbDataObj (List [i]).Data.Port:=aPort;  // Port aktualisieren
        TGPRSVerbDataObj (List [i]).Data.Kennung:='';  // noch nicht bekannt, kann sich geändert haben
        TGPRSVerbDataObj (List [i]).Data.GerTypName:='';  // noch nicht bekannt, kann sich geändert haben
        TGPRSVerbDataObj (List [i]).Data.GerTypNr:=-1;  // noch nicht bekannt, kann sich geändert haben
        TGPRSVerbDataObj (List [i]).Data.Pull_Aktiv:=false;
        TGPRSVerbDataObj (List [i]).Data.ZeitSync_PCTime:=0;
        TGPRSVerbDataObj (List [i]).Data.ZeitSync_DiffSec:=-1;
      end else
        inc (TGPRSVerbDataObj (List [i]).Data.AnzVerbClose);

      TGPRSVerbDataObj (List [i]).Data.Aktiv:=isVerbOpen;
      TGPRSVerbDataObj (List [i]).Data.LetzteAktion:=Now;
    finally
      UnlockList;
    end;
    FVerbData_Changed:=true;  // Flag setzen: Verbindungsdaten haben sich geändert
  end
  else begin  // nicht gefunden
    if isVerbOpen then begin
      // neue Verbindung in Liste eintragen
      with GPRSVerbData do begin
        IP_Adresse:=aIP_Adresse;
        Port:=aPort;
        Aktiv:=true;
        AnzRecTelegramme:=0;  // Startwert
        AnzRecBloecke:=0;  // Startwert
        AnzRecBytes:=0;  // Startwert
        AnzSendTelegramme:=0;  // Startwert
        AnzSendBytes:=0;  // Startwert
        AnzVerbOpen:=1;  // Startwert
        AnzVerbClose:=0;  // Startwert
        Kennung:='';  // noch nicht bekannt
        GerTypName:='';  // noch nicht bekannt
        GerTypNr:=-1;  // noch nicht bekannt
        LetzteAktion:=Now;
        Pull_Aktiv:=false;
        ZeitSync_PCTime:=0;
        ZeitSync_DiffSec:=-1;
      end;

      List:=LockList;
      try
        GPRSVerbDataObj:=TGPRSVerbDataObj.Create;
        GPRSVerbDataObj.SetData (GPRSVerbData);
        List.Add (GPRSVerbDataObj);
      finally
        UnlockList;
      end;
      FVerbData_Changed:=true;  // Flag setzen: Verbindungsdaten haben sich geändert
    end;
  end;
end;

{----------------------------------------------------------------------}
function TGPRSVerbList.UpdateMRG (aIP_Adresse: string; aKennung: string;
  aGerTypName: string): boolean;
{----------------------------------------------------------------------}
{ Gerätedaten einer GPRS-Verbindung aktualisieren;
  Übergabe: IP-Adresse der GPRS-Verbindung
            Kennung und Gerätetyp des Geräts
  Ergebnis: true, wenn GPRS-Verbindung in Liste vorhanden ist und in den Verbindungsdaten
            die übergebene Kennung und der Gerätetypname nicht enthalten sind }
var
  List: TList;
  i: integer;

begin
  Result:=false;
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin
    List:=LockList;
    try
      if (TGPRSVerbDataObj (List [i]).Data.Kennung <> aKennung) AND
         (TGPRSVerbDataObj (List [i]).Data.GerTypName <> aGerTypName) then begin
        TGPRSVerbDataObj (List [i]).Data.Kennung:=aKennung;
        TGPRSVerbDataObj (List [i]).Data.GerTypName:=aGerTypName;
        FVerbData_Changed:=true;  // Flag setzen: Verbindungsdaten haben sich geändert
        Result:=true;
      end;
    finally
      UnlockList;
    end;
  end;
end;

{--------------------------------------------------------------------------------}
function TGPRSVerbList.UpdateGeraetetyp (aIP_Adresse: string; aGerTypName: string;
  aGerTypNr: integer): boolean;
{--------------------------------------------------------------------------------}
{ Gerätetyp einer GPRS-Verbindung aktualisieren;
  Übergabe: IP-Adresse der GPRS-Verbindung
            Gerätetypname
            Gerätetypnummer
  Ergebnis: true, wenn GPRS-Verbindung in Liste vorhanden ist }
var
  List: TList;
  i: integer;

begin
  Result:=false;
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin
    List:=LockList;
    try
      TGPRSVerbDataObj (List [i]).Data.GerTypName:=aGerTypName;
      TGPRSVerbDataObj (List [i]).Data.GerTypNr:=aGerTypNr;
      FVerbData_Changed:=true;  // Flag setzen: Verbindungsdaten haben sich geändert
      Result:=true;
    finally
      UnlockList;
    end;
  end;
end;

{------------------------------------------------------------------------------------}
function TGPRSVerbList.UpdateKennung (aIP_Adresse: string; aKennung: string): boolean;
{------------------------------------------------------------------------------------}
{ Kennung einer GPRS-Verbindung aktualisieren;
  Übergabe: IP-Adresse der GPRS-Verbindung
            Kennung
  Ergebnis: true, wenn GPRS-Verbindung in Liste vorhanden ist und in den Verbindungsdaten
            die übergebene Kennung nicht enthalten ist }
var
  List: TList;
  i: integer;

begin
  Result:=false;
  i:=FindVerbindung (aIP_Adresse);
  if i > -1 then begin
    List:=LockList;
    try
      if TGPRSVerbDataObj (List [i]).Data.Kennung <> aKennung then begin
        TGPRSVerbDataObj (List [i]).Data.Kennung:=aKennung;
        FVerbData_Changed:=true;  // Flag setzen: Verbindungsdaten haben sich geändert
        Result:=true;
      end;
    finally
      UnlockList;
    end;
  end;
end;

{---------------------------- Ausgabe -----------------------------------------}

{------------------------------------------------------------------------------}
procedure TGPRSVerbList.VerbindungenAnzeigen (lvVerbindungen: TListView;
                                              pVerbAktiv, pVerbInaktiv: TPanel);
{------------------------------------------------------------------------------}
{ Listeninhalt in ListView laden;
  Übergabe: ListView für Verbindungen }
const
  { Image-Indizes für IP-Verbindungsstatus }
  stip_inaktiv = 0;
  stip_aktiv   = 1;

var
  List: TList;
  i: integer;
  pListItem: TListItem;
  GPRSVerbData: TGPRSVerbData;
  GPRSVerbDataObj: TGPRSVerbDataObj;
  Count_aktiv: integer;
  Count_inaktiv: integer;

begin
  Count_aktiv:=0;
  Count_inaktiv:=0;

  if Assigned (lvVerbindungen) then begin
    lvVerbindungen.Items.BeginUpdate;
    try
      { Verbindungen-ListView: Objekte freigeben, Liste leeren }
      for i:=0 to lvVerbindungen.Items.Count - 1 do
        TGPRSVerbDataObj (lvVerbindungen.Items[i].Data).Free;
      lvVerbindungen.Items.Clear;

      List:=LockList;
      try
        { Verbindungen-Liste füllen: }
        for i:=0 to List.Count - 1 do  begin
          // Hier KEIN Application.ProcessMessages aufrufen ! Es werden sonst die
          // ServerSocket-Ereignis-Routinen zwischendurch aufgerufen und die
          // Listview nicht richtig aufgebaut.

          GPRSVerbData:=TGPRSVerbDataObj (List [i]).Data;
          GPRSVerbDataObj:=TGPRSVerbDataObj.Create;
          GPRSVerbDataObj.SetData (GPRSVerbData);

          pListItem:=lvVerbindungen.Items.Add;    { 1. Spalte: Aktiv (immer anzeigen) }
          pListItem.Data:=GPRSVerbDataObj;
          if TGPRSVerbDataObj (List [i]).Data.Aktiv then begin
            pListItem.StateIndex:=stip_aktiv;
            inc (Count_aktiv);
          end
          else begin
            pListItem.StateIndex:=stip_inaktiv;
            inc (Count_inaktiv);
          end;

          if (af_IPAdresse in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            pListItem.SubItems.Add (TGPRSVerbDataObj (List [i]).Data.IP_Adresse);  { 2. Spalte: IP-Adresse }
          if (af_Port in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            pListItem.SubItems.Add (IntToStr (TGPRSVerbDataObj (List [i]).Data.Port));  { 3. Spalte: Port }
          if (af_GerTypName in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            pListItem.SubItems.Add (TGPRSVerbDataObj (List [i]).Data.GerTypName);  { 4. Spalte: Gerätetyp }
          if (af_Kennung in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            pListItem.SubItems.Add (TGPRSVerbDataObj (List [i]).Data.Kennung);  { 5. Spalte: Kennung }
          if (af_AnzRecTelegr in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            pListItem.SubItems.Add (IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzRecTelegramme));  { 6. Spalte: Telegramme empfangen }
          if (af_AnzRecBloecke in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            pListItem.SubItems.Add (IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzRecBloecke));  { 7. Spalte: Blöcke empfangen }
          if (af_AnzRecBytes in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            pListItem.SubItems.Add (IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzRecBytes));  { 8. Spalte: Bytes empfangen }
          if (af_AnzSendTelegr in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            pListItem.SubItems.Add (IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzSendTelegramme));  { 9. Spalte: Telegramme gesendet }
          if (af_AnzSendBytes in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            pListItem.SubItems.Add (IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzSendBytes));  { 10. Spalte: Bytes gesendet }
          if (af_AnzVerbOpen in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            pListItem.SubItems.Add (IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzVerbOpen));  { 11. Spalte: Verbindungen geöffnet }
          if (af_AnzVerbClose in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            pListItem.SubItems.Add (IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzVerbClose));  { 12. Spalte: Verbindungen geschlossen }
          if (af_LetzteAktion in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            pListItem.SubItems.Add (FormatDateTime (SFormatDate + ' ' + S_FormatTime,
              TGPRSVerbDataObj (List [i]).Data.LetzteAktion));  { 13. Spalte: Datum/Zeit der letzten Aktion }
        end; { for }
      finally
        UnlockList;
      end;

      if lvVerbindungen.Items.Count > 0 then
        lvVerbindungen.Items [0].Selected:=true;
    finally
      lvVerbindungen.Items.EndUpdate;
    end;
  end;

  if Assigned (pVerbAktiv) then
    pVerbAktiv.Caption:=' ' + IntToStr (Count_aktiv) + ' aktiv';
  if Assigned (pVerbInaktiv) then
    pVerbInaktiv.Caption:=' ' + IntToStr (Count_inaktiv) + ' inaktiv';

end;

{---------------------------------------------------------------------------}
function TGPRSVerbList.WriteStatistikFile (dtServerStart: TDateTime;
                                           sServerLaufzeit: string): boolean;
{---------------------------------------------------------------------------}
{ Listeninhalt in Statistik-Datei schreiben (ASCII);
  Übergabe: Zeitpunkt, zu dem Server gestartet wurde
            String mit Server-Laufzeit
  Ergebnis: true, wenn Datei erfolgreich geschrieben wurde }
const
  CTrennzeichen = #9;  // Tabulator

var
  List: TList;
  FName: string;
  TFS: TTextFileStreamExt;
  isOpened: boolean;
  S: string;
  i: integer;

begin
  Result:=false;
  FName:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_Statistik.log';
  if FileExists (FName) then
    TFS:=TTextFileStreamExt.Create (FName, fmOpenReadWrite OR fmShareDenyWrite, isOpened)
  else
    TFS:=TTextFileStreamExt.Create (FName, fmCreate, isOpened);
  try
    if isOpened then begin
      if TFS.Size = 0 then begin // File neu angelegt
        // erst mal Spalten-Überschriften schreiben:
        S:='Server-Startzeit' + CTrennzeichen +
           'Server-Laufzeit';
        if (af_IPAdresse in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
          S:=S + CTrennzeichen + 'IP-Adresse';
        if (af_Port in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
          S:=S + CTrennzeichen + 'Port';
        if (af_Kennung in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
          S:=S + CTrennzeichen + 'Kennung';
        if (af_GerTypName in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
          S:=S + CTrennzeichen + 'Gerätetyp';
        if (af_AnzRecTelegr in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
          S:=S + CTrennzeichen + 'Telegramme empfangen';
        if (af_AnzRecBloecke in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
          S:=S + CTrennzeichen + 'Blöcke empfangen';
        if (af_AnzRecBytes in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
          S:=S + CTrennzeichen + 'Bytes empfangen';
        if (af_AnzSendTelegr in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
          S:=S + CTrennzeichen + 'Telegramme gesendet';
        if (af_AnzSendBytes in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
          S:=S + CTrennzeichen + 'Bytes gesendet';
        if (af_AnzVerbOpen in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
          S:=S + CTrennzeichen + 'Verbindung geöffnet';
        if (af_AnzVerbClose in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
          S:=S + CTrennzeichen + 'Verbindung geschlossen';
        if (af_LetzteAktion in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
          S:=S + CTrennzeichen + 'Letzte Aktion';
       TFS.WriteLn (S);
      end;
      TFS.Seek (0, soFromEnd);

      List:=LockList;
      try
        for i:=0 to List.Count - 1 do begin
          S:=FormatDateTime ('dd.mm.yyyy hh:nn:ss', dtServerStart) + CTrennzeichen +
             sServerLaufzeit;
          if (af_IPAdresse in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            S:=S + CTrennzeichen + TGPRSVerbDataObj (List [i]).Data.IP_Adresse;
          if (af_Port in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            S:=S + CTrennzeichen + IntToStr (TGPRSVerbDataObj (List [i]).Data.Port);
          if (af_Kennung in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            S:=S + CTrennzeichen + TGPRSVerbDataObj (List [i]).Data.Kennung;
          if (af_GerTypName in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            S:=S + CTrennzeichen + TGPRSVerbDataObj (List [i]).Data.GerTypName;
          if (af_AnzRecTelegr in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            S:=S + CTrennzeichen + IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzRecTelegramme);
          if (af_AnzRecBloecke in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            S:=S + CTrennzeichen + IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzRecBloecke);
          if (af_AnzRecBytes in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            S:=S + CTrennzeichen + IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzRecBytes);
          if (af_AnzSendTelegr in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            S:=S + CTrennzeichen + IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzSendTelegramme);
          if (af_AnzSendBytes in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            S:=S + CTrennzeichen + IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzSendBytes);
          if (af_AnzVerbOpen in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            S:=S + CTrennzeichen + IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzVerbOpen);
          if (af_AnzVerbClose in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            S:=S + CTrennzeichen + IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzVerbClose);
          if (af_LetzteAktion in FAnzeigeFelder) OR (af_All in FAnzeigeFelder) then
            S:=S + CTrennzeichen + FormatDateTime ('dd.mm.yyyy hh:nn:ss', TGPRSVerbDataObj (List [i]).Data.LetzteAktion);
          TFS.WriteLn (S);
        end;
      finally
        UnlockList;
      end;
      Result:=true;
    end;
  finally
    TFS.Free;
  end;
end;

{----------------------------------------------------------------------}
function TGPRSVerbList.WriteClientInfoFile (sFilename: string): boolean;
{----------------------------------------------------------------------}
{ Verbindungsinfo-Übergabedatei für Anzeige-Client schreiben;
  Übergabe: Dateiname der incl. Pfad
  Ergebnis: true, wenn Datei erfolgreich geschrieben wurde }
const
  CTrennzeichen = US;

var
  List: TList;
  TFS: TTextFileStreamExt;
  isOpened: boolean;
  i: integer;
  S: string;
  sAktiv: string;

begin
  Result:=false;
  TFS:=TTextFileStreamExt.Create (sFilename, fmCreate, isOpened);
  try
    if isOpened then begin
      List:=LockList;
      try
        for i:=0 to List.Count - 1 do begin
          if TGPRSVerbDataObj (List [i]).Data.Aktiv then
            sAktiv:='J'
          else
            sAktiv:='N';
          S:=TGPRSVerbDataObj (List [i]).Data.IP_Adresse + CTrennzeichen +
             IntToStr (TGPRSVerbDataObj (List [i]).Data.Port) + CTrennzeichen +
             sAktiv + CTrennzeichen +
             IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzRecTelegramme) + CTrennzeichen +
             IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzRecBloecke) + CTrennzeichen +
             IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzRecBytes) + CTrennzeichen +
             IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzVerbOpen) + CTrennzeichen +
             IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzVerbClose) + CTrennzeichen +
             TGPRSVerbDataObj (List [i]).Data.Kennung + CTrennzeichen +
             TGPRSVerbDataObj (List [i]).Data.GerTypName + CTrennzeichen +
             IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzSendTelegramme) + CTrennzeichen +
             IntToStr(TGPRSVerbDataObj (List [i]).Data.AnzSendBytes);
          TFS.WriteLn (S);
        end;
      finally
        UnlockList;
      end;
      Result:=true;
    end;
  finally
    TFS.Free;
  end;
end;

end.

