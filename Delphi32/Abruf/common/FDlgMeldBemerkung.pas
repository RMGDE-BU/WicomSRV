{******************************************************************************}
{* Unit: Eingabe-Dialog f�r Bemerkungen zu einer Meldung                      *}
{*       -> getrennte Bemerkungen f�r Kommt- und Geht-Meldung                 *}
{* Version: 10.07.2001  WW                                                    *}
{* 26.10.2007  WW  resourcestrings                                            *}
{******************************************************************************}
unit FDlgMeldBemerkung;

interface

uses
  Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, WSysCon;

type
  TFormDlgMeldBemerkung = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    bbtnSpeichern: TBitBtn;
    bbtnCancel: TBitBtn;
    gbBemerkung: TGroupBox;
    lStationStatic: TLabel;
    eKommtBemerkung: TEdit;
    lKommtBemerkung: TLabel;
    eGehtBemerkung: TEdit;
    lGehtBemerkung: TLabel;
    lStation1: TLabel;
    lStation2: TLabel;
    lMeldungStatic: TLabel;
    lMeldung: TLabel;
    procedure bbtnCancelClick(Sender: TObject);
    procedure bbtnSpeichernClick(Sender: TObject);
    procedure eBemerkungChange(Sender: TObject);
  private
    { Private-Deklarationen }
    BemerkungAlt_K: string;
    BemerkungAlt_G: string;
  public
    { Public-Deklarationen }
    BemerkungNeu_K: string;
    BemerkungNeu_G: string;
    constructor Create(anOwner: TComponent;
                       Stationtext1: string; Stationtext2: string;
                       Meldungtext: string;
                       forKommt: boolean; forGeht: boolean;
                       aBemerkung_K: string; aBemerkung_G: string); reintroduce;
  end;

implementation

{$R *.DFM}

resourcestring
  S_BemerkungGeaendert = 'Bemerkung wurde ge�ndert !'#13'Abbrechen ohne abzuspeichern ?';


{-----------------------------------------------------------------------------------}
constructor TFormDlgMeldBemerkung.Create(anOwner: TComponent;
                                         Stationtext1: string; Stationtext2: string;
                                         Meldungtext: string;
                                         forKommt: boolean; forGeht: boolean;
                                         aBemerkung_K: string; aBemerkung_G: string);
{-----------------------------------------------------------------------------------}
begin
  inherited Create(anOwner);
  lStation1.Caption:=Stationtext1;
  lStation2.Caption:=Stationtext2;
  lMeldung.Caption:=Meldungtext;

  BemerkungAlt_K:=aBemerkung_K;
  BemerkungAlt_G:=aBemerkung_G;
  BemerkungNeu_K:='';                              { Vorbelegung f�r R�ckgabe }
  BemerkungNeu_G:='';                              { Vorbelegung f�r R�ckgabe }

  eKommtBemerkung.MaxLength:=szLen_Bemerkung;
  eGehtBemerkung.MaxLength:=szLen_Bemerkung;

  eKommtBemerkung.Text:=BemerkungAlt_K;
  eGehtBemerkung.Text:=BemerkungAlt_G;

  lKommtBemerkung.Enabled:=forKommt;
  eKommtBemerkung.Enabled:=forKommt;
  lGehtBemerkung.Enabled:=forGeht;
  eGehtBemerkung.Enabled:=forGeht;
  bbtnSpeichern.Enabled:=false;
end;

{------------------------------------------------------------------}
procedure TFormDlgMeldBemerkung.bbtnSpeichernClick(Sender: TObject);
{------------------------------------------------------------------}
begin
  { R�ckgabe belegen, NTY-K�rzel steckt im vorletzten Zeichen des Listentextes: }
  BemerkungNeu_K:=eKommtBemerkung.Text;
  BemerkungNeu_G:=eGehtBemerkung.Text;
end;

{---------------------------------------------------------------}
procedure TFormDlgMeldBemerkung.bbtnCancelClick(Sender: TObject);
{---------------------------------------------------------------}
begin
  if bbtnSpeichern.Enabled then begin
    if MessageDlg(S_BemerkungGeaendert, mtConfirmation,[mbYes,mbNo],0) <> mrYes then
      ModalResult:=mrNone;
  end;
end;

{----------------------------------------------------------------}
procedure TFormDlgMeldBemerkung.eBemerkungChange(Sender: TObject);
{----------------------------------------------------------------}
begin
  bbtnSpeichern.Enabled:=(eKommtBemerkung.Text <> BemerkungAlt_K) OR
                         (eGehtBemerkung.Text <> BemerkungAlt_G);
end;

end.
