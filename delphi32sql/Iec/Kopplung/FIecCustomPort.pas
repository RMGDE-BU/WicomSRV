{******************************************************************************}
{* Unit: Monitor mit Basis-Kommunikationsroutinen für IEC-Kopplung (32-Bit)   *}
{* 26.03.2003  WW                                                             *}
{******************************************************************************}
unit FIecCustomPort;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ScktComp, ExtCtrls, Buttons, ComCtrls, IecConst, IecLog, O_IecRemoteMonitor;

type
  { Callback-Prozedurtypen zur Anzeige in Fremdmodulen }

  TCBStringProc = procedure (S: string) of object;

  { Formular mit Basis-IEC-Kommunikationshandling und Monitor }

  TFormCustomPortIec = class(TForm)
    TimerUebertragung: TTimer;
    memoMonitor: TMemo;
    pTop: TPanel;
    pButton: TPanel;
    sbtnClear: TSpeedButton;
    pServer: TPanel;
    pClient: TPanel;
    lClient: TLabel;
    lServer: TLabel;
    pt0: TPanel;
    procedure sbtnClearClick(Sender: TObject);
    procedure TimerUebertragungTimer(Sender: TObject);
  private
    { Private-Deklarationen }
    FStopped: boolean;
  protected
    FNetProgDir: string;
    IECLogFile: TIecLogFile;
    FIniDate: TDateTime;
    FRemoteMonitorObj: TRemoteMonitorObj;
    FRemoteMonitorControlList: TList;
    procedure ShowSendedaten (S: string);
    procedure ShowEmpfangsdaten (S: string);
    procedure ShowText (S: string);
    procedure UpdateMonitor (S: string); virtual;
    procedure Port_Action; virtual;
    function GetConnected: boolean; virtual;
  public
    { Public-Deklarationen }
    constructor Create (AOwner: TComponent; ANetProgDir: string;
      AIecLogfile: TIecLogFile;
      ARemoteMonitorObj: TRemoteMonitorObj); reintroduce; virtual;
    destructor Destroy; override;
    procedure Set_IecLogfile (AIecLogfile: TIecLogFile); virtual;
    procedure SendToRemoteClient (sNorm: string);
    property Stopped: boolean read FStopped write FStopped;
    property Connected: boolean read GetConnected;
  end;

var
  FormCustomPortIec: TFormCustomPortIec;

implementation

{$R *.DFM}

{-----------------------------------------------------------------------------}
constructor TFormCustomPortIec.Create (AOwner: TComponent; ANetProgDir: string;
  AIecLogfile: TIecLogFile; ARemoteMonitorObj: TRemoteMonitorObj);
{-----------------------------------------------------------------------------}
begin
  inherited Create (AOwner);
  FNetProgDir:=ANetProgDir;
  IECLogFile:=AIecLogfile;
  FRemoteMonitorObj:=ARemoteMonitorObj;

  FRemoteMonitorControlList:=TList.Create;

  FIniDate:=-1;  // Default: Zeitstempel der INI-Datei unbekannt
  FStopped:=false;  // Default: Übertragungs-Timer nicht gestoppt
end;

{------------------------------------}
destructor TFormCustomPortIec.Destroy;
{------------------------------------}
begin
  FRemoteMonitorControlList.Free;
  inherited Destroy;
end;

{---------------------------------------------------------------------}
procedure TFormCustomPortIec.Set_IecLogfile (AIecLogfile: TIecLogFile);  // 10.03.2015, WW
{---------------------------------------------------------------------}
begin
  IECLogFile:=AIecLogfile;
end;

{------------------------------------------------------}
procedure TFormCustomPortIec.ShowSendedaten (S: string);
{------------------------------------------------------}
{ Sendedaten anzeigen }
begin
  UpdateMonitor ('TX: ' + HexString (S));
end;

{---------------------------------------------------------}
procedure TFormCustomPortIec.ShowEmpfangsdaten (S: string);
{---------------------------------------------------------}
{ Empfangsdaten anzeigen }
begin
  UpdateMonitor ('RX: ' + HexString (S));
end;

{------------------------------------------------}
procedure TFormCustomPortIec.ShowText (S: string);
{------------------------------------------------}
{ Text anzeigen }
begin
  UpdateMonitor (S);
end;

{----------------------------------------------------}
procedure TFormCustomPortIec.UpdateMonitor(S: string);
{----------------------------------------------------}
begin
  memoMonitor.Lines.BeginUpdate;
  try
    while memoMonitor.Lines.Count > 1000 do  { max. 1000 Zeilen darstellen }
      memoMonitor.Lines.Delete(0);
    memoMonitor.Lines.Add(S);
  finally
    memoMonitor.Lines.EndUpdate;
  end;
  // ans Ende scrollen:
  memoMonitor.Perform (EM_LineScroll, 0, memoMonitor.Lines.Count-1);
end;

{-----------------------------------------------------------}
procedure TFormCustomPortIec.sbtnClearClick(Sender: TObject);
{-----------------------------------------------------------}
{ Memo-Einträge löschen }
begin
  memoMonitor.Lines.BeginUpdate;
  try
    memoMonitor.Lines.Clear;
  finally
    memoMonitor.Lines.EndUpdate;
  end;
end;

{---------------------------------------}
procedure TFormCustomPortIec.Port_Action;
{---------------------------------------}
begin
// Default: es passiert nix
end;

{------------------------------------------------}
function TFormCustomPortIec.GetConnected: boolean;
{------------------------------------------------}
begin
  Result:=false;  // Default: keine Verbindung
end;


{-------------------------- Timerroutinen -------------------------------------}

{-------------------------------------------------------------------}
procedure TFormCustomPortIec.TimerUebertragungTimer(Sender: TObject);
{-------------------------------------------------------------------}
{ Port-Kommunikation }
begin
  TimerUebertragung.Enabled:=false;
  try
    Port_Action;
    Application.ProcessMessages;
  finally
    TimerUebertragung.Enabled:=not FStopped;  // 10.03.2015, WW
  end;
end;

{-------------------------- Remote-Monitor ------------------------------------}

{--------------------------------------------------------------}
procedure TFormCustomPortIec.SendToRemoteClient (sNorm: string);
{--------------------------------------------------------------}
{ Sendet Telegramm-Daten der Remote-Monitor-Kontrollelemente an alle verbundenen
  Remote-Monitor-Clients }
begin
  if Assigned (FRemoteMonitorObj) then
    FRemoteMonitorObj.SendPortData (sNorm, '', FRemoteMonitorControlList);  // ohne Monitor-Daten
end;

end.

