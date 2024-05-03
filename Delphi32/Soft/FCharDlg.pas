{------------------------------------------------------------------------------}
{ Dialog für Auswahl eines Trennzeichens                                       }
{                                                                              }
{ 13.08.2001  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001                                          }
{------------------------------------------------------------------------------}
unit FCharDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TFormCharDialog = class(TForm)
    pnBottom: TPanel;
    pnClient: TPanel;
    bbtnOk: TBitBtn;
    bbtnCancel: TBitBtn;
    rgrpSeparatorChar: TRadioGroup;
    eAnyChar: TEdit;
    gboxFilename: TGroupBox;
    eFileName: TEdit;
    btnFileDlg: TButton;
    OpenDialog: TOpenDialog;
    procedure rbtnAnyCharClick(Sender: TObject);
    procedure bbtnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnFileDlgClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FFileNameMode: boolean;
    procedure SetChoosenChar(sChar: char);
    function GetChoosenChar: char;
    procedure SetFilename(sFilename: TFileName);
    function GetFilename: TFileName;
    procedure SetFilenameMode(bState: boolean);
  public
    { Public-Deklarationen }
    property ChoosenChar: char read GetChoosenChar write SetChoosenChar;
    property Filename: TFileName read GetFilename write SetFilename;
    property FilenameMode: boolean read FFileNameMode write SetFilenameMode;
  end;

implementation

{$R *.DFM}

const
  C_RGIndex_Tab       = 0;
  C_RGIndex_Semikolon = 1;
  C_RGIndex_Komma     = 2;
  C_RGIndex_Space     = 3;
  C_RGIndex_AnyChar  = 4;

{------------------------------ TFormCharDialog -------------------------------}

{------------------------------------------------------}
procedure TFormCharDialog.FormCreate(Sender: TObject);
{------------------------------------------------------}
begin
  FFileNameMode := False;
  SetFilenameMode(FFileNameMode);
end;

{------------------------------------------------------}
procedure TFormCharDialog.rbtnAnyCharClick(Sender: TObject);
{------------------------------------------------------}
begin
  eAnyChar.Enabled := (rgrpSeparatorChar.ItemIndex = C_RGIndex_AnyChar);
end;

{------------------------------------------------------}
procedure TFormCharDialog.SetChoosenChar(sChar: char);
{------------------------------------------------------}
begin
  case sChar of
    #9 : rgrpSeparatorChar.ItemIndex := C_RGIndex_Tab;
    ';' : rgrpSeparatorChar.ItemIndex := C_RGIndex_Semikolon;
    ',' : rgrpSeparatorChar.ItemIndex := C_RGIndex_Komma;
    ' ' : rgrpSeparatorChar.ItemIndex := C_RGIndex_Space;
    else begin
      rgrpSeparatorChar.ItemIndex := C_RGIndex_AnyChar;
      eAnyChar.Text := sChar;
    end;
  end;
end;

{------------------------------------------------------}
function TFormCharDialog.GetChoosenChar: char;
{------------------------------------------------------}
begin
  case rgrpSeparatorChar.ItemIndex of
    C_RGIndex_Tab : Result := #9;
    C_RGIndex_Semikolon : Result := ';';
    C_RGIndex_Komma : Result := ',';
    C_RGIndex_Space : Result := ' ';
    C_RGIndex_AnyChar : if (Length(eAnyChar.Text) = 1)
                        then Result := eAnyChar.Text[1]
                        else Result := #9;
    else Result := #9;
  end;
end;

{------------------------------------------------------}
procedure TFormCharDialog.SetFilename(sFilename: TFileName);
{------------------------------------------------------}
begin
  eFileName.Text := sFilename;
end;

{------------------------------------------------------}
function TFormCharDialog.GetFilename: TFileName;
{------------------------------------------------------}
begin
  Result := eFileName.Text;
end;

{------------------------------------------------------}
procedure TFormCharDialog.bbtnOkClick(Sender: TObject);
{------------------------------------------------------}
resourcestring
  C_Msg_NoChar = 'Sie müssen ein Zeichen eingeben !';
  C_Msg_NoFile = 'Sie müssen einen Dateinamen eingeben !';
begin
  if (rgrpSeparatorChar.ItemIndex = C_RGIndex_AnyChar) and
     (Length(eAnyChar.Text) <> 1) then
  begin
    MessageDlg(C_Msg_NoChar, mtError, [mbOk], 0);
    eAnyChar.SetFocus;
    ModalResult := mrNone;
    Exit;
  end
  else ModalResult := mrOk;

  if (FFileNameMode) and (eFileName.Text = '') then begin
    MessageDlg(C_Msg_NoFile, mtError, [mbOk], 0);
    eFileName.SetFocus;
    ModalResult := mrNone;
    Exit;
  end
  else ModalResult := mrOk;
end;

{------------------------------------------------------}
procedure TFormCharDialog.SetFilenameMode(bState: boolean);
{------------------------------------------------------}
begin
  if (bState) then begin
    Self.ClientHeight :=
      pnBottom.Height + gboxFilename.Top + gboxFilename.Height + 8;
  end
  else begin
    Self.ClientHeight :=
      pnBottom.Height + rgrpSeparatorChar.Top + rgrpSeparatorChar.Height + 8;
  end;
  FFileNameMode := bState;
end;

{------------------------------------------------------}
procedure TFormCharDialog.btnFileDlgClick(Sender: TObject);
{------------------------------------------------------}
begin
  with OpenDialog do begin
    if (eFileName.Text <> '') then
      InitialDir := ExtractFilePath(eFileName.Text);
    if (Execute) then eFileName.Text := FileName;
  end;
end;

end.
