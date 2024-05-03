{******************************************************************************}
{* Unit: Hauptservice-Formular für MRG/DSfG-Abrufserver                       *}
{* 19.02.2003  WW                                                             *}
{* 28.05.2013  WW  Default: Keine Interaktion zwischen Dienst und Windows-    *}
{*                 Desktop                                                    *}
{* 03.07.2013  WW  max. 10 Versuche für das Einfügen des Taskbar-Icons        *}
{* 03.12.2013  WW  Neuer Dienstname (nicht Gas-X-Version) und Dienst-         *}
{*                 Anzeigename                                                *}
{* 14.07.2020  WW  mit Exception-Handling beim Freigeben des Abrufserver-     *}
{*                 Formulars                                                  *}
{******************************************************************************}
unit SMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, FServer,
  PathIni;

type

  { Abruf-Service }

  TWIPServerService = class(TService)
    procedure ServiceExecute(Sender: TService);
    procedure ServiceCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function GetServiceController: TServiceController; override;
  end;

var
  WIPServerService: TWIPServerService;

implementation

{$R *.DFM}

{----------------------------------------------------}
procedure ServiceController(CtrlCode: DWord); stdcall;
{----------------------------------------------------}
begin
  WIPServerService.Controller(CtrlCode);
end;

{ TIPServerService }

{--------------------------------------------------------}
procedure TWIPServerService.ServiceCreate(Sender: TObject);
{--------------------------------------------------------}
begin
{$IFDEF GAS-X}
  // Dienstname (unverändert) und Dienst-Anzeigename für Gas-X-Version; 03.12.2013, WW
  Name:='IPServerService';
  DisplayName:=DisplayName + ' ' + SGasX;
{$ENDIF}
end;

{-----------------------------------------------------------------}
function TWIPServerService.GetServiceController: TServiceController;
{-----------------------------------------------------------------}
begin
  Result:=ServiceController;
end;

{----------------------------------------------------------}
procedure TWIPServerService.ServiceExecute(Sender: TService);
{----------------------------------------------------------}
var
  iTaskBarIconAdd: integer;

begin
  FormAbrufServer:=TFormAbrufServer.Create (Self);
  try
    iTaskBarIconAdd:=1;
    while not Terminated do begin
      Sleep (1);
      { versuchen, Dienst-Icon in Taskbar einzufügen: }
      if iTaskBarIconAdd in [1..10] then begin  // max. 10 Versuche (es klappt nicht immer auf Anhieb)
        if FormAbrufServer.TaskBarAddIcon then
          iTaskBarIconAdd:=0  // OK, Taskbar-Icon erfolgreich eingefügt
        else
          inc (iTaskBarIconAdd);
      end;

      if Assigned (ServiceThread) then
        ServiceThread.ProcessRequests(False);
    end;
  finally
    FormAbrufServer.TaskBarRemoveIcon;    { Dienst-Icon aus Taskbar entfernen }
    try
      FormAbrufServer.Free;
    except  // 14.07.2020, WW
      //
    end;
  end;
end;

initialization
  InitLanguage;  // 28.11.2007, WW
  
end.
