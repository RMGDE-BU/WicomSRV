{------------------------------------------------------------------------------}
{ StationsAuswahl-Fenster                                                      }
{                                                                              }
{ 27.08.2001  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001                                          }
{------------------------------------------------------------------------------}
unit FStationsAuswahl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls,
  DSfGStationen, DStaDll, PathIni, DSG_Utils;

type
  TFormStationsAuswahl = class(TForm)
    pnTree: TPanel;
    Splitter1: TSplitter;
    pnButtons: TPanel;
    pnClient: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
    FDSfGStationen : TDSfGStationen;
  protected
    { Protected-Deklarationen }
    procedure InitComponents(bState: boolean); virtual;
    procedure InitDSfGStationen(bState: boolean); virtual;
  public
    { Public-Deklarationen }
    property StaTree: TDSfGStationen read FDSfGStationen;
  end;

implementation

{$R *.DFM}

{------------------------------------------------------}
procedure TFormStationsAuswahl.FormCreate(Sender: TObject);
{------------------------------------------------------}
begin
  InitComponents(True);
end;

{------------------------------------------------------}
procedure TFormStationsAuswahl.FormDestroy(Sender: TObject);
{------------------------------------------------------}
begin
  InitComponents(False);
end;

{------------------------------------------------------}
procedure TFormStationsAuswahl.FormClose(Sender: TObject;
  var Action: TCloseAction);
{------------------------------------------------------}
begin
  Action := caFree;
end;

{ Initialisieren/Freigeben von Komponenten             }
{ Parameter: T=Initialisieren; F=Freigeben             }
{------------------------------------------------------}
procedure TFormStationsAuswahl.InitComponents(bState: boolean);
{------------------------------------------------------}
begin
  if (bState) then begin
    InitDSfGStationen(True);
  end
  else begin
    InitDSfGStationen(False);
  end;
end;

{ Initialisieren/Freigeben des Stammdatenbaums         }
{ Parameter: T=Initialisieren; F=Freigeben             }
{------------------------------------------------------}
procedure TFormStationsAuswahl.InitDSfGStationen(bState: boolean);
{------------------------------------------------------}
begin
  if (bState) then begin
    if (not Assigned(FDSfGStationen)) and
       (Assigned(ClientStammdaten)) and (Assigned(PathServer)) then
    begin
      FDSfGStationen := TDSfGStationen.Create(pnTree, drlDSfG, dslStation);
      FDSfGStationen.Parent := pnTree;
      FDSfGStationen.Align := alClient;
    end;
  end
  else begin
    FDSfGStationen.Free;
    FDSfGStationen := nil;
  end;
end;

end.
