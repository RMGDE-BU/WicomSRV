{------------------------------------------------------------------------------}
{ Auswahl für (WICOM-) Abruf                                                   }
{                                                                              }
{ -> Benötigt DSta.Dll                                                         }
{ -> Benötigt PathServer mit WStammDir und WNetProgDir                         }
{                                                                              }
{ 24.04.2002  GD  Neu                                                          }
{ 05.12.2008  GD  Bugfix beim Runterblättern der Zeit                          }
{ 20.07.2010  WN  Parameter-Abruf (Export) von DSfG-Stationen                  }
{                                                                              }
{ (C) Karl Wieser GmbH 2002, RMG Messtechnik GmbH 2008, 2010                   }
{------------------------------------------------------------------------------}
unit FAbrufAuswahl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Buttons, ExtCtrls, ComCtrls, StdCtrls,
  FWicomStaAusw, WSysCon, DSG_Utils, GD_Utils, DMomLists;

type
  TFormAbrufAuswahl = class(TFormWicomStationsAuswahl)
    pnRight: TPanel;
    gbDatenTypen: TGroupBox;
    chbMesswerte: TCheckBox;
    chbMeldungen: TCheckBox;
    chbParameter: TCheckBox;
    chbPruefsaetze: TCheckBox;
    rgrpAbrufArt: TRadioGroup;
    gbZeitBereich: TGroupBox;
    dtpDatumVon: TDateTimePicker;
    dtpZeitVon: TDateTimePicker;
    chbDateTimeVon: TCheckBox;
    chbDateTimeBis: TCheckBox;
    dtpDatumBis: TDateTimePicker;
    dtpZeitBis: TDateTimePicker;
    Bevel1: TBevel;
    pnRightRest: TPanel;
    pnRight1: TPanel;
    bbtnOk: TBitBtn;
    bbtnCancel: TBitBtn;
    procedure FormResize(Sender: TObject);
    procedure chbDateTimeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgrpAbrufArtClick(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure DeleteInstanzenWithoutArchive;
  protected
    { Protected-Deklarationen }
    procedure BuildSourceTree; override;
    procedure AddSelected; override;
    procedure AddAll; override;
    procedure RemoveSelected; override;
    function CheckAuftraege: string; virtual;
  public
    { Public-Deklarationen }
    function GetDatenTypen(cGerArt: char): integer;
    function GetAbrufArt: string;
    function GetZeitbereichVon: TDateTime;
    function GetZeitbereichBis: TDateTime;
  end;

implementation

{$R *.DFM}

const
  C_RGIndex_ManuAbruf = 0;
  C_RGIndex_AutoAbruf = 1;

{----------------------------------------------------}
procedure TFormAbrufAuswahl.FormCreate(Sender: TObject);
{----------------------------------------------------}
begin
  inherited;

  pnRightRest.Color := C_ColorWieser;
  dtpDatumVon.DateTime := Trunc(Now);
  dtpZeitVon.DateTime := Trunc(Now);
  dtpDatumBis.DateTime := Trunc(Now);
  dtpZeitBis.DateTime := Now;
  chbDateTimeClick(nil);
end;

{----------------------------------------------------}
procedure TFormAbrufAuswahl.FormResize(Sender: TObject);
{----------------------------------------------------}
begin
  if (not (csDestroying in Self.ComponentState)) then begin
    pnSourceTree.Width :=
      (ClientWidth - pnRight.Width -(pnMoveButtons.Width div 2)) div 2;
    pnButtons.Top := (pnSinkTree.Height - pnButtons.Height) div 2;
  end;
end;

{----------------------------------------------------}
procedure TFormAbrufAuswahl.BuildSourceTree;
{----------------------------------------------------}
begin
  inherited;

  DeleteInstanzenWithoutArchive;
end;

{ Ausgewähltes Item in Auswahl aufnehmen              }
{----------------------------------------------------}
procedure TFormAbrufAuswahl.AddSelected;
{----------------------------------------------------}
var
  i : integer;
begin
  inherited;

  if (rgrpAbrufArt.ItemIndex = C_RGIndex_AutoAbruf) then
    for i := TreeSink.Items.Count-1 downto 0 do
      if (TreeSink.Items[i].Level > C_Level_Stationen) then
        TreeSink.DeleteNode(TreeSink.Items[i]);
end;

{ Alle Items in Auswahl aufnehmen                    }
{----------------------------------------------------}
procedure TFormAbrufAuswahl.AddAll;
{----------------------------------------------------}
var
  i : integer;
begin
  inherited;

  if (rgrpAbrufArt.ItemIndex = C_RGIndex_AutoAbruf) then
    for i := TreeSink.Items.Count-1 downto 0 do
      if (TreeSink.Items[i].Level > C_Level_Stationen) then
        TreeSink.DeleteNode(TreeSink.Items[i]);
end;

{ Ausgewähltes Item aus Auswahl entfernen            }
{----------------------------------------------------}
procedure TFormAbrufAuswahl.RemoveSelected;
{----------------------------------------------------}
var
  i : integer;
begin
  inherited;

  if (rgrpAbrufArt.ItemIndex = C_RGIndex_AutoAbruf) then
    for i := TreeSink.Items.Count-1 downto 0 do
      if (TreeSink.Items[i].Level > C_Level_Stationen) then
        TreeSink.DeleteNode(TreeSink.Items[i]);
end;

{----------------------------------------------------}
procedure TFormAbrufAuswahl.rgrpAbrufArtClick(Sender: TObject);
{----------------------------------------------------}
var
  i : integer;
begin
  EnableMyControls(gbZeitBereich, (rgrpAbrufArt.ItemIndex = C_RGIndex_ManuAbruf),
    False, False);

  if (rgrpAbrufArt.ItemIndex = C_RGIndex_AutoAbruf) then begin
    for i := TreeSink.Items.Count-1 downto 0 do
      if (TreeSink.Items[i].Level > C_Level_Stationen) then
        TreeSink.DeleteNode(TreeSink.Items[i])
  end
  else if (rgrpAbrufArt.ItemIndex = C_RGIndex_ManuAbruf) then begin
    for i := TreeSink.Items.Count-1 downto 0 do
      if (PStaRecord(TreeSink.Items[i].Data)^.GTyp = C_GerArtDSfG) then
        TreeSink.DeleteNode(TreeSink.Items[i]);
  end;
end;

{----------------------------------------------------}
procedure TFormAbrufAuswahl.chbDateTimeClick(Sender: TObject);
{----------------------------------------------------}
begin
  dtpDatumVon.Enabled := chbDateTimeVon.Checked;
  dtpZeitVon.Enabled := chbDateTimeVon.Checked;
  dtpDatumBis.Enabled := chbDateTimeBis.Checked;
  dtpZeitBis.Enabled := chbDateTimeBis.Checked;
end;

{ Gibt gewählte Datentypen für Geräteart zurück      }
{ Parameter: Geräteart                               }
{ Rückgabe: Datentypen für Geräteart                 }
{----------------------------------------------------}
function TFormAbrufAuswahl.GetDatenTypen(cGerArt: char): integer;
{----------------------------------------------------}
begin
  Result := 0;
  if (cGerArt = c_GerArtMrg) then begin
    if (chbMesswerte.Checked) then Result := Result + C_IsMesswerte;
    if (chbMeldungen.Checked) then Result := Result + C_IsMeldungen;
    if (chbParameter.Checked) then Result := Result + C_IsParameter;
    if (chbPruefsaetze.Checked) then Result := Result + C_IsPruefsaetze;
  end
  else if (cGerArt = C_GerArtDSfG) then begin
    if (chbMesswerte.Checked) then Result := Result + C_IsArchive;
    if (chbMeldungen.Checked) then Result := Result + C_IsLogbuecher;
    if (chbParameter.Checked) then Result := Result + C_IsParameter;  // 20.07.2010  WN
  end;
end;

{ Gibt gewählte Abrufart zurück                      }
{ Rückgabe: Abrufart                                 }
{----------------------------------------------------}
function TFormAbrufAuswahl.GetAbrufArt: string;
{----------------------------------------------------}
begin
  case rgrpAbrufArt.ItemIndex of
    C_RGIndex_ManuAbruf : Result := C_AbrArtManu;
    C_RGIndex_AutoAbruf : Result := C_AbrArtAuto;
    else Result := C_AbrArtManu;
  end;
end;

{ Gibt gewählten Abrufzeitpunkt - ab - zurück        }
{ Rückgabe: Abruf ab ...                             }
{----------------------------------------------------}
function TFormAbrufAuswahl.GetZeitbereichVon: TDateTime;
{----------------------------------------------------}
begin
  if (chbDateTimeVon.Checked)
  then Result := Trunc(dtpDatumVon.DateTime) + Frac(dtpZeitVon.DateTime)
  else Result := Now - 10*365;
end;

{ Gibt gewählten Abrufzeitpunkt - bis - zurück       }
{ Rückgabe: Abruf bis ...                            }
{----------------------------------------------------}
function TFormAbrufAuswahl.GetZeitbereichBis: TDateTime;
{----------------------------------------------------}
begin
  if (chbDateTimeBis.Checked)
  then Result := Trunc(dtpDatumBis.DateTime) + Frac(dtpZeitBis.DateTime)
  else Result := Now + 1*365;
end;

{----------------------------------------------------}
procedure TFormAbrufAuswahl.DeleteInstanzenWithoutArchive;
{----------------------------------------------------}
//var
//  i : integer;
begin
  // 'Delete' führt dazu, dass übergeordnete Registrierinstanzen
  // nicht mehr gefunden werdn
{
  for i := TreeSource.Items.Count-1 downto 0 do
    if (TreeSource.Items[i].Level = C_Level_Instanzen) and
       (not TreeSource.Items[i].HasChildren)
    then TreeSource.DeleteNode(TreeSource.Items[i]);
}
end;

{ Prüft, ob definierte Aufträgen i.O. sind           }
{ Rückgabe: ''=OK, sonst Fehlertext für Message      }
{ 03.11.2005 WW: Prüfung auf leere Datentypen bei Manu- und Autoabruf }
{----------------------------------------------------}
function TFormAbrufAuswahl.CheckAuftraege: string;
{----------------------------------------------------}

  function AppendResult(var sResult: string; sAppend: string): string;
  begin
    if (sResult <> '') then Result := sResult + #13#10;
    Result := Result + sAppend;
    sResult := Result;
  end;

resourcestring
  S_NoDatentypenMrg = 'Keine Datentypen für MRG definiert !';
  S_NoDatentypenDSfG = 'Keine Datentypen für DSfG definiert !';
  S_NoStationen = 'Keine Stationen oder Stationen ohne Datenarchive definiert !';
var
  iStaCount : integer;
begin
  Result := '';    // Default - kein Fehler
  iStaCount := 0;  // Anzahl der abzurufenden Stationen

  // Prüfung für MRG-Stationen
  with GetMStationen do
  try
    if (Count > 0) then begin
      iStaCount := iStaCount + Count;
      if (GetDatenTypen(C_GerArtMrg) = 0) then
        AppendResult(Result, S_NoDatentypenMrg);
    end;
  finally
    Free;
  end;

  if (GetAbrufArt = C_AbrArtAuto) then begin
    // Prüfung für DSfG-Stationen
    with GetDStationen do
    try
      if (Count > 0) then begin
        iStaCount := iStaCount + Count;
        if (GetDatenTypen(C_GerArtDSfG) = 0) then
          AppendResult(Result, S_NoDatentypenDSfG);
      end;
    finally
      Free;
    end;
  end
  else begin  // Manu-Abruf
    // Prüfung für DSfG-Archive
    with GetDKanaele do
    try
      if (Count > 0) then begin
        if (GetDatenTypen(C_GerArtDSfG) AND C_IsArchive) <> 0 then  // 03.11.2005, WW
          iStaCount := iStaCount + Count;
      end;
    finally
      Free;
    end;

    // Prüfung für DSfG-Logbücher
    with GetDLogBuecher do
    try
      if (Count > 0) then begin
        if (GetDatenTypen(C_GerArtDSfG) AND C_IsLogbuecher) <> 0 then  // 03.11.2005, WW
          iStaCount := iStaCount + Count;
      end;
    finally
      Free;
    end;

    // 20.07.2010  WN
    // Prüfung für DSfG-Stationen (Parameter-Abruf / -Export)
    with GetDStationen do
    try
      if (Count > 0) then begin
        if (GetDatenTypen(C_GerArtDSfG) AND C_IsParameter) <> 0 then
          iStaCount := iStaCount + Count;
      end;
    finally
      Free;
    end;
  end;

  // Prüfung, ob Stationen definiert sind
  if (iStaCount = 0) then begin
    Result := '';
    AppendResult(Result, S_NoStationen);
  end;
end;

end.
