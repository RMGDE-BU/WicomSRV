{******************************************************************************}
{* Unit: Eingabe-Dialog zum Abschließen einer Meldung (manuell generierte     *}
{*       Geht-Meldung)                                                        *}
{*                                                                            *}
{* 31.03.2015  WW  Neu                                                        *}
{* Copyright (C) RMG Messtechnik GmbH 2015                                    *}
{******************************************************************************}
unit FDlgMeldAbschluss;

interface

uses
  Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, WSysCon,
  Graphics;

type
  TFormDlgMeldAbschluss = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    bbtnSpeichern: TBitBtn;
    bbtnCancel: TBitBtn;
    gbBemerkung: TGroupBox;
    lStationStatic: TLabel;
    eGehtBemerkung: TEdit;
    lGehtBemerkung: TLabel;
    lStation1: TLabel;
    lStation2: TLabel;
    lMeldungStatic: TLabel;
    lMeldung: TLabel;
    lWarnung: TLabel;
    Image1: TImage;
    procedure bbtnCancelClick(Sender: TObject);
    procedure bbtnSpeichernClick(Sender: TObject);
  private
    { Private-Deklarationen }
    BemerkungAlt_G: string;
  public
    { Public-Deklarationen }
    BemerkungNeu_G: string;
    constructor Create(anOwner: TComponent;
                       Stationtext1: string; Stationtext2: string;
                       Meldungtext: string;
                       aBemerkung_G: string); reintroduce;
  end;

implementation

{$R *.DFM}

resourcestring
  S_BemerkungGeaendert = 'Bemerkung wurde geändert !'#13'Abbrechen ohne abzuspeichern ?';
  S_WarnungAbschlussMeldung = 'Achtung: Die vorliegende offene Kommt-Meldung wird manuell abgeschlossen !';


{----------------------------------------------------------------------------------}
constructor TFormDlgMeldAbschluss.Create(anOwner: TComponent;
                                         Stationtext1: string; Stationtext2: string;
                                         Meldungtext: string;
                                         aBemerkung_G: string);
{----------------------------------------------------------------------------------}
begin
  inherited Create(anOwner);
  lStation1.Caption:=Stationtext1;
  lStation2.Caption:=Stationtext2;
  lMeldung.Caption:=Meldungtext;

  BemerkungAlt_G:=aBemerkung_G;
  BemerkungNeu_G:='';                              { Vorbelegung für Rückgabe }

  eGehtBemerkung.MaxLength:=szLen_Bemerkung;
  eGehtBemerkung.Text:=BemerkungAlt_G;

  lWarnung.Caption:=S_WarnungAbschlussMeldung;
end;

{------------------------------------------------------------------}
procedure TFormDlgMeldAbschluss.bbtnSpeichernClick(Sender: TObject);
{------------------------------------------------------------------}
begin
  { Rückgabe belegen: }
  BemerkungNeu_G:=eGehtBemerkung.Text;
end;

{---------------------------------------------------------------}
procedure TFormDlgMeldAbschluss.bbtnCancelClick(Sender: TObject);
{---------------------------------------------------------------}
begin
  if eGehtBemerkung.Text <> BemerkungAlt_G then begin
    if MessageDlg(S_BemerkungGeaendert, mtConfirmation,[mbYes,mbNo],0) <> mrYes then
      ModalResult:=mrNone;
  end;
end;

end.
