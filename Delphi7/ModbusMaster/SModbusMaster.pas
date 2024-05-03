{******************************************************************************}
{* Unit: Service-Formular für Modbus-Master                                   *}
{* 11.03.2010  WW                                                             *}
{******************************************************************************}
unit SModbusMaster;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, PathIni,
  FMainModbusMaster;

type
  { Modbus-Master-Service }

  TMBMService = class(TService)
    procedure ServiceExecute(Sender: TService);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function GetServiceController: TServiceController; override;
  end;

var
  MBMService: TMBMService;

implementation

{$R *.DFM}

{----------------------------------------------------}
procedure ServiceController(CtrlCode: DWord); stdcall;
{----------------------------------------------------}
begin
  MBMService.Controller(CtrlCode);
end;

{ TMBMService }

{------------------------------------------------------------}
function TMBMService.GetServiceController: TServiceController;
{------------------------------------------------------------}
begin
  Result:=ServiceController;
end;

{-----------------------------------------------------}
procedure TMBMService.ServiceExecute(Sender: TService);
{-----------------------------------------------------}
var
  TaskBarIconAdded: boolean;
begin
  FormMainModbusMaster:=TFormMainModbusMaster.Create (Self);
  try
    TaskBarIconAdded:=false;
    while not Terminated do begin
      Sleep (1);
      { versuchen, Dienst-Icon in Taskbar einzufügen: }
      if not TaskBarIconAdded then
        TaskBarIconAdded:=FormMainModbusMaster.TaskBarAddIcon;

      ServiceThread.ProcessRequests(False);
    end;
  finally
    FormMainModbusMaster.TaskBarRemoveIcon;    { Dienst-Icon aus Taskbar entfernen }
    FormMainModbusMaster.Free;
  end;
end;

initialization
  InitLanguage;

end.
