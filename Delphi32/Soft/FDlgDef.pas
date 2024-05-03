{------------------------------------------------------------------------------}
{ Default-Unit für Dialoge (B&B-System)                                        }
{                                                                              }
{ 24.01.2003  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2003                                          }
{------------------------------------------------------------------------------}
unit FDlgDef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TFormDialogDefault = class(TForm)
    pnClient: TPanel;
    pnBottom: TPanel;
    pnBottomBtnOk: TPanel;
    bbtnOk: TBitBtn;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.DFM}

end.
