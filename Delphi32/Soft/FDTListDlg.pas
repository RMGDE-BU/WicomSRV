//------------------------------------------------------------------------------
// Listenauswahldialog mit Datum-Zeit-Angabe
//
// 30.07.2008  GD  Neu
//
// Copyright (C) RMG Messtechnik GmbH 2008
//------------------------------------------------------------------------------
unit FDTListDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FListAuswahlDialog, Buttons, StdCtrls, ExtCtrls, ComCtrls;

type
  TFormDTListenDialog = class(TFormListenDialog)
    pnDT: TPanel;
    Label1: TLabel;
    dtpVonDatum: TDateTimePicker;
    dtpVonZeit: TDateTimePicker;
    Label2: TLabel;
    dtpBisDatum: TDateTimePicker;
    dtpBisZeit: TDateTimePicker;
  private
    { Private-Deklarationen }
    function GetDatumZeit(iMode: integer): TDateTime;
    procedure SetDatumZeit(iMode: integer; dtTimeStamp: TDateTime);
  public
    { Public-Deklarationen }
    property DatumZeitVon: TDateTime index 1
      read GetDatumZeit write SetDatumZeit;
    property DatumZeitBis: TDateTime index 2
      read GetDatumZeit write SetDatumZeit;
  end;

implementation

{$R *.dfm}

// Gibt die eingestellte Zeitinformation zurück
// Parameter: Modus - 1=ZeitVon, 2=ZeiBis
// Rückgabe: Zeitinformation
//---------------------------------------------------------
function TFormDTListenDialog.GetDatumZeit(iMode: integer): TDateTime;
//---------------------------------------------------------
begin
  if (iMode = 1)
  then Result := Trunc(dtpVonDatum.DateTime) + Frac(dtpVonZeit.DateTime)
  else Result := Trunc(dtpBisDatum.DateTime) + Frac(dtpBisZeit.DateTime);
end;

// Setzt die  Zeitinformation
// Parameter: Modus - 1=ZeitVon, 2=ZeiBis; Zeitinformation
//---------------------------------------------------------
procedure TFormDTListenDialog.SetDatumZeit(
  iMode: integer; dtTimeStamp: TDateTime);
//---------------------------------------------------------
begin
  if (iMode = 1) then begin
    dtpVonDatum.DateTime := dtTimeStamp;
    dtpVonZeit.DateTime := dtTimeStamp;
  end
  else begin
    dtpBisDatum.DateTime := dtTimeStamp;
    dtpBisZeit.DateTime := dtTimeStamp;
  end;
end;

end.
