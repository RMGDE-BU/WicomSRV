{******************************************************************************}
{* Unit: Dialogfenster für Modemkonfiguration                                 *}
{* 24.01.2013 WW  Neu                                                         *}
{* 06.05.2013 WW  erweitert um PIN-Eingabe für GSM-Modem                      *}
{*                                                                            *}
{* Copyright © RMG Messtechnik GmbH 2013                                      *}
{******************************************************************************}
unit F_ModemKonfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Serial, ModemIni, SrvCfgIni;

type
  TFormModemKonfig = class(TForm)
    pnClient: TPanel;
    pnBottom: TPanel;
    bbtnAbbruch: TBitBtn;
    bbtnSpeichern: TBitBtn;
    Label5: TLabel;
    cbModemTyp: TComboBox;
    Label3: TLabel;
    cbMaxBaudrate: TComboBox;
    Label4: TLabel;
    eInit: TEdit;
    rgrpWahlverfahren: TRadioGroup;
    Label15: TLabel;
    cbCOM: TComboBox;
    sbtnCheckCOMs: TSpeedButton;
    lPIN: TLabel;
    ePIN: TEdit;
    procedure FormShow(Sender: TObject);
    procedure bbtnAbbruchClick(Sender: TObject);
    procedure cbModemTypChange(Sender: TObject);
    procedure bbtnSpeichernClick(Sender: TObject);
    procedure eInitChange(Sender: TObject);
    procedure cbMaxBaudrateChange(Sender: TObject);
    procedure rgrpWahlverfahrenClick(Sender: TObject);
    procedure cbCOMChange(Sender: TObject);
    procedure sbtnCheckCOMsClick(Sender: TObject);
    procedure ePINChange(Sender: TObject);
  private
    { Private-Deklarationen }
    FModemIni: TModemIni;
    FSerial: TSerial;
    FSerialCreated: boolean;
    FCOMNr: integer;
    FModemName: string;
    FModemWahlverfahren: string;
    FPIN: string;
    procedure SetCOMCombobox (ACOMNr: integer);
    procedure SetModemInitString;
    procedure SetMaxBaudrate;
    procedure SetPINData (APIN: string);
    function CheckModemData: boolean;
  public
    { Public-Deklarationen }
    constructor Create (anOwner: TComponent; AModemIniPfad: string;
      ASerial: TSerial); reintroduce;
    destructor Destroy; override;
    procedure SetData (ACOMNr: integer; AModemName, AModemWahlverfahren,
      APIN: string);
    property COMNr: integer read FCOMNr;
    property Modemname: string read FModemName;
    property ModemWahlverfahren: string read FModemWahlverfahren;
    property PIN: string read FPIN;  // 06.05.2013, WW
  end;

implementation

{$R *.DFM}

resourcestring
  SCOMNichtAusgewaehlt = 'Serielle Schnittstelle ist nicht ausgewählt !';
  SModemtypAuswaehlen = 'Bitte Modemtyp auswählen !';
  SModemtypKeinDSfG   = 'Mit dem ausgewählten Modemtyp können keine DSfG-Abrufe durchgeführt werden. ' + #13 +
                        'Wollen Sie die Einstellungen trotzdem speichern ?';
  SPINFehlt  = 'Es ist keine PIN eingeben. ' + #13 + 'Wollen Sie die Einstellungen trotzdem speichern ?';
  SEinstGeaendert     = 'Einstellungen wurden geändert !' + #13 +
                        'Wollen Sie die Änderungen speichern ?';


{------------------------------------------------------------------------------}
constructor TFormModemKonfig.Create (anOwner: TComponent; AModemIniPfad: string;
  ASerial: TSerial);
{------------------------------------------------------------------------------}
begin
  inherited Create (anOwner);
  FModemIni:=TModemIni.Create (AModemIniPfad);
  FSerial:=ASerial;
  if not Assigned (FSerial) then begin
    FSerial:=TSerial.Create (nil);
    FSerialCreated:=true;
  end else
    FSerialCreated:=false;

  FCOMNr:=-1;
  FModemName:='';
  FModemWahlverfahren:='';
  FPIN:='';
end;

{----------------------------------}
destructor TFormModemKonfig.Destroy;
{----------------------------------}
begin
  if FSerialCreated then
    FSerial.Free;
  FModemIni.Free;
  inherited Destroy;
end;

{---------------------------------------------------}
procedure TFormModemKonfig.FormShow(Sender: TObject);
{---------------------------------------------------}
begin
  bbtnSpeichern.Enabled:=false;
end;

{--------------------------------------------------------------}
procedure TFormModemKonfig.SetData (ACOMNr: integer; AModemName,
  AModemWahlverfahren, APIN: string);
{--------------------------------------------------------------}
{ Setzt Anzeigewerte }
begin
  cbModemtyp.Clear;
  FModemIni.GetModemList ('A', cbModemtyp.Items);

  cbModemtyp.ItemIndex:=cbModemtyp.Items.IndexOf (AModemname);

  if LowerCase (AModemWahlverfahren) = CTonwahl then
    rgrpWahlverfahren.ItemIndex:=0
  else
    rgrpWahlverfahren.ItemIndex:=1;

  SetCOMCombobox (ACOMNr);
  SetModemInitString;
  SetMaxBaudrate;
  SetPINData (APIN);
end;

{----------------------------------------------------------}
procedure TFormModemKonfig.SetCOMCombobox (ACOMNr: integer);
{----------------------------------------------------------}
{ COM-Comboboxliste füllen und Eintrag für COM-Nummer setzen }
var
  i: integer;

begin
  { Liste neu laden: }
  cbCOM.Items.Clear;
  for i:=0 to FSerial.Ports.Count - 1 do
    cbCOM.Items.Add (FSerial.Ports [i]);

  { Eintrag für COM-Nummer auswählen: }
  if (cbCOM.Items.Count > 0) then  // COMs sind vorhanden
    if ACOMNr > 0 then
      cbCOM.ItemIndex:=cbCOM.Items.IndexOf (Format ('COM%d', [ACOMNr]));
end;

{--------------------------------------------}
procedure TFormModemKonfig.SetModemInitString;
{--------------------------------------------}
{ Setzt den Initialisierungs-String für das Modem }
begin
  eInit.Text:=FModemIni.GetDSfGInitString (cbModemTyp.Items [cbModemtyp.ItemIndex]);
end;

{----------------------------------------}
procedure TFormModemKonfig.SetMaxBaudrate;
{----------------------------------------}
{ Setzt max. Baudrate für das Modem }
var
  br: integer;
  i: integer;
begin
  br:=FModemIni.GetMaxBaud (cbModemTyp.Items [cbModemtyp.ItemIndex]);

  i:=cbMaxBaudrate.Items.IndexOf (IntToStr (br));
  if i > -1 then
    cbMaxBaudrate.ItemIndex:= i
  else
    cbMaxBaudrate.ItemIndex:= 8;     { = 57600 Baud }
end;

{ Setzt PIN-Anzeige für GSM-Modem  }
{------------------------------------}
procedure TFormModemKonfig.SetPINData (APIN: string);
{------------------------------------}
begin
  ePIN.Text:=APIN;  // PIN

  if FModemIni.GetGSM (cbModemTyp.Items [cbModemtyp.ItemIndex]) then begin
    lPIN.Visible:=true;
    ePIN.Visible:=true;
  end
  else begin
    lPIN.Visible:=false;
    ePIN.Visible:=false;
  end;
end;

{------------------------------------------------}
function TFormModemKonfig.CheckModemData: boolean;
{------------------------------------------------}
{ Eingaben prüfen. Wenn OK, werden Modem-INI-Einstellungen gespeichert. }
begin
  Result:=false;
  if cbCOM.ItemIndex < 0 then begin
    MessageDlg (SCOMNichtAusgewaehlt, mtError, [mbOk], 0);
    exit;
  end;

  if cbModemTyp.ItemIndex < 0 then begin
    MessageDlg (SModemtypAuswaehlen, mtInformation, [mbOK], 0);
    cbModemTyp.SetFocus;
    exit;
  end;

  if length (eInit.Text) = 0 then begin
    if MessageDlg (SModemtypKeinDSfG, mtWarning, [mbYes,mbNo], 0) <> mrYes then
      exit;
  end;

  if FModemIni.GetGSM (cbModemTyp.Items [cbModemtyp.ItemIndex]) AND
     (length (ePIN.Text) = 0) then begin
    if MessageDlg (SPINFehlt, mtWarning, [mbYes,mbNo], 0) <> mrYes then
      exit;
  end;

  FCOMNr:=StrToInt (Copy (cbCOM.Text, 4, length (cbCOM.Text)));
  FModemName:=cbModemTyp.Items [cbModemtyp.ItemIndex];
  if rgrpWahlverfahren.ItemIndex = 0 then
    FModemWahlverfahren:=CTonwahl
  else
    FModemWahlverfahren:=CPulswahl;
  FPIN:=ePIN.Text;

  FModemIni.SetMaxBaud (cbModemTyp.Items [cbModemtyp.ItemIndex],
                        StrToInt (cbMaxBaudrate.Items [cbMaxBaudrate.ItemIndex]));
  FModemIni.SetDSfGInitString (cbModemTyp.Items [cbModemtyp.ItemIndex], eInit.Text);
  Result:=true;
end;

{-------------------------------------------------------------}
procedure TFormModemKonfig.bbtnSpeichernClick(Sender: TObject);
{-------------------------------------------------------------}
begin
  if not CheckModemData then
    ModalResult:=mrNone;
end;

{-----------------------------------------------------------}
procedure TFormModemKonfig.bbtnAbbruchClick(Sender: TObject);
{-----------------------------------------------------------}
begin
  if bbtnSpeichern.Enabled then begin
    if MessageDlg (SEinstGeaendert, mtConfirmation, [mbYes,mbNo], 0) = mrYes then
     if not CheckModemData then
       ModalResult:=mrNone;
  end;
end;

{------------------------------------------------------}
procedure TFormModemKonfig.cbCOMChange(Sender: TObject);
{------------------------------------------------------}
begin
  bbtnSpeichern.Enabled:= True;
end;

{-----------------------------------------------------------}
procedure TFormModemKonfig.cbModemTypChange(Sender: TObject);
{-----------------------------------------------------------}
begin
  SetModemInitString;
  SetMaxBaudrate;
  SetPINData (ePIN.Text);
  bbtnSpeichern.Enabled:= True;
end;

{------------------------------------------------------}
procedure TFormModemKonfig.eInitChange(Sender: TObject);
{------------------------------------------------------}
begin
  bbtnSpeichern.Enabled:= True;
end;

{--------------------------------------------------------------}
procedure TFormModemKonfig.cbMaxBaudrateChange(Sender: TObject);
{--------------------------------------------------------------}
begin
  bbtnSpeichern.Enabled:= True;
end;

{-----------------------------------------------------------------}
procedure TFormModemKonfig.rgrpWahlverfahrenClick(Sender: TObject);
{-----------------------------------------------------------------}
begin
  bbtnSpeichern.Enabled:= True;
end;

{-----------------------------------------------------}
procedure TFormModemKonfig.ePINChange(Sender: TObject);
{-----------------------------------------------------}
begin
  bbtnSpeichern.Enabled:= True;
end;

{-------------------------------------------------------------}
procedure TFormModemKonfig.sbtnCheckCOMsClick(Sender: TObject);
{-------------------------------------------------------------}
var
  Save_Cursor: TCursor;
  S: string;
  iCOMNr: integer;

begin
  Save_Cursor:=Screen.Cursor;
  Screen.Cursor:=crHourglass;
  try
    S:=cbCOM.Text;
    if length (S) > 0 then
      iCOMNr:=StrToInt (Copy (cbCOM.Text, 4, length (cbCOM.Text)))
    else
      iCOMNr:=0;

    FSerial.AktuPorts;
    SetCOMCombobox (iCOMNr);
  finally
    Screen.Cursor:=Save_Cursor;
  end;
end;

end.
