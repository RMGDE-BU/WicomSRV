{******************************************************************************}
{* Unit: COM-Monitor für EC 900-Kommunikationsprüfung                         *}
{* 29.10.2009  WW                                                             *}
{******************************************************************************}
unit F_COMMonitor;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, Forms, Buttons,
  ExtCtrls, Dialogs;

type

  { Formular mit COM-Monitor }

  TFormCOMMonitor = class(TForm)
    memoMonitor: TMemo;
    pTop: TPanel;
    sbtnSave: TSpeedButton;
    SaveDialog: TSaveDialog;
    procedure sbtnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    LogfilePrefix: string;
  end;

implementation

{$R *.DFM}

{----------------------------------------------------}
procedure TFormCOMMonitor.FormCreate(Sender: TObject);
{----------------------------------------------------}
begin
  LogfilePrefix:='COMMonitor';
end;

{-------------------------------------------------------}
procedure TFormCOMMonitor.sbtnSaveClick(Sender: TObject);
{-------------------------------------------------------}
{ Monitordaten in Datei speichern }
begin
  SaveDialog.InitialDir:=ExtractFilePath (ParamStr (0));
  SaveDialog.FileName:=LogfilePrefix + '_' +
    FormatDateTime ('yyyymmddhhnnss', Now) + '.log';
  if SaveDialog.Execute then
    memoMonitor.Lines.SaveToFile (SaveDialog.FileName);
end;

end.

