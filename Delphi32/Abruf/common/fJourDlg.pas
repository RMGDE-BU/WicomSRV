{------------------------------------------------------------------------------}
{ 29.01.1999 GD; Dialog-Formular-Unit für das Drucken der Journal-Tabelle      }
{                                                                              }
{  -> wird nicht zur Laufzeit erstellt - in automatische Erstellung aufnehmen  }
{                                                                              }
{  (C) Karl Wieser GmbH 1999                                                   }
{------------------------------------------------------------------------------}
unit fJourDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TFormPrintJournalDialog = class(TForm)
    pnBottom: TPanel;
    bbtnOk: TBitBtn;
    bbtnCancel: TBitBtn;
    pnClient: TPanel;
    pnInfo1: TPanel;
    pnDatentypen: TPanel;
    pnDatenZeitbereiche: TPanel;
    pnZeitSync: TPanel;
    pnFehlerWarnungen: TPanel;
    pnZusaetzlich: TPanel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    chbDatenTypen: TCheckBox;
    chbdatenZeitbereiche: TCheckBox;
    chbZeitSync: TCheckBox;
    chbFehlerWarnungen: TCheckBox;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormPrintJournalDialog: TFormPrintJournalDialog;

implementation

{$R *.DFM}

end.
