{------------------------------------------------------------------------------}
{ Versionsausgabe der Abruf-Dienste                                            }
{                                                                              }
{ 09.10.2004  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2004                                          }
{------------------------------------------------------------------------------}
unit FExternProgInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FDllInfo, StdCtrls, ExtCtrls, Buttons;

type
  TFormExternProgInfo = class(TFormDllVersions)
  private
    { Private-Deklarationen }
    procedure SetImageFileName(sFileName: TFileName);
    procedure SetListCaption(sCaption: string);
  public
    { Public-Deklarationen }
    property ImageFileName: TFileName write SetImageFileName;
    property ListCaption: string write SetListCaption;
  end;

implementation

{$R *.DFM}

{----------------------------------------------------}
procedure TFormExternProgInfo.SetImageFileName(sFileName: TFileName);
{----------------------------------------------------}
begin
  if (FileExists(sFileName)) then Image.Picture.LoadFromFile(sFileName);
end;

{----------------------------------------------------}
procedure TFormExternProgInfo.SetListCaption(sCaption: string);
{----------------------------------------------------}
begin
  pnCaption.Caption := sCaption;
end;

end.
