{------------------------------------------------------------------------------}
{ Default-Unit für Unterfenster (VCD-System)                                   }
{                                                                              }
{ 24.01.2003  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2003                                          }
{------------------------------------------------------------------------------}
unit FSubDef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TFormSubWindowDefault = class(TForm)
    pnClient: TPanel;
    pnRight: TPanel;
    pnRight1: TPanel;
    pnRightRest: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    FLastError : string;
  protected
    { Protected-Deklarationen }
    procedure InitControls(bState: boolean); virtual;
    procedure MyErrorHandling(const sError: string); virtual;
    property LastError: string read FLastError write FLastError;
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.DFM}

{-------------------------------------------------------}
procedure TFormSubWindowDefault.FormCreate(Sender: TObject);
{-------------------------------------------------------}
var
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    InitControls(True);
  finally
    Screen.Cursor := oldCursor;
  end;
end;

{-------------------------------------------------------}
procedure TFormSubWindowDefault.FormDestroy(Sender: TObject);
{-------------------------------------------------------}
var
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Screen.Cursor := crHourGlass;
    InitControls(False);
  finally
    Screen.Cursor := oldCursor;
  end;
end;

{ Initialisieren bzw. Freigeben der Controls            }
{ Parameter: T=Initialisieren, T=Freigeben              }
{-------------------------------------------------------}
procedure TFormSubWindowDefault.InitControls(bState: boolean);
{-------------------------------------------------------}
begin
  if (bState) then begin
  end
  else begin
  end;
end;

{ Internes Fehlerhandling                               }
{ Parameter: Fehlertext                                 }
{-------------------------------------------------------}
procedure TFormSubWindowDefault.MyErrorHandling(const sError: string);
{-------------------------------------------------------}
begin
  FLastError := sError;
end;

end.
