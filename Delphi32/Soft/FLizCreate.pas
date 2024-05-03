{------------------------------------------------------------------------------}
{ Lizenzdialog für die Lizensierung eines Programmpakets für einen Rechner     }
{                                                                              }
{ 12.09.2003  GD  Neu als Dialog mit Mail-Funktion                             }
{ 09.10.2004  GD  Namensänderung, erweitert um telefonische Registrierung      }
{ 19.11.2007  WW  resourcestrings                                              }
{ 10.02.2017  WW  Neue E-Mailadresse software@rmg.com                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2003, 2017                                    }
{------------------------------------------------------------------------------}
unit FLizCreate;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComObj, FileCtrl, Variants,
  Lizenz32, Buttons;

const
  C_LizenzFileName = 'WK22Lizenz.LIZ';
  C_ReceiveAdress  = 'software@rmg.com';  // 10.02.2017, WW

type
  TFormCreateLizenzFile = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    bbtnOnlineLiz: TBitBtn;
    bbtnFileLiz: TBitBtn;
    OpenDialog: TOpenDialog;
    btnEncode: TButton;
    Label5: TLabel;
    bbtnTelLiz: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure bbtnOnlineLizClick(Sender: TObject);
    procedure bbtnFileLizClick(Sender: TObject);
    procedure btnEncodeClick(Sender: TObject);
    procedure bbtnTelLizClick(Sender: TObject); // 09.10.2004
  private
    { Private-Deklarationen }
    function CreateLizenzList: TStrings;
    procedure EncodeLizenzFile;
    procedure SaveLizenzToFile;
    procedure SendLizenzByMail;
  public
    { Public-Deklarationen }
  end;

var
  FormCreateLizenzFile: TFormCreateLizenzFile;

implementation

{$R *.DFM}

resourcestring
  S_LizenzText = 'Lizenzanforderung WICO22:'#13;
  S_Mailed = 'Lizenzinformationen wurden per mail an die RMG Messtechnik GmbH übermittelt.';
  S_Error = 'Lizenzinformationen konnten nicht übermittelt werden !'#13#10 +
    'Bitte speichern Sie die Lizenzinformation als Datei und'#13#10 +
    'übermitteln Sie diese an die RMG Messtechnik GmbH.';
  S_Saved = 'Lizenzinformationen wurden in Datei "%s" gespeichert.';

const
  C_Offset = 25;
  C_Zyklus = 3;
  C_Step   = 2;

  C_MailError_OK          = 0;
  C_MailError_NoRecipient = -1;
  C_MailError_NoOutlook   = -2;
  C_MailError_OtherError  = -3;

  C_RGIndex_InternetMail = 0;
  C_RGIndex_Outlook      = 1;

var
  vOutlook : Variant;

{------------------------------------------------------------------------}
function SendOutlookMail(
  sSubject, sText: string; pSlTo, pSlCc, pSlBcc, pSlAttach: TStrings): Smallint;
{------------------------------------------------------------------------}
const
  C_Ole_ApplName = 'Outlook.Application';
var
  vMailItem, vRecipient : Variant;
  i                     : integer;
begin
  if (Assigned(pSlTo)) and (pSlTo.Count > 0) then
  try

    // Ggf. OLE-Objekt initialisieren
    try
      if (VarType(vOutLook) = varEmpty) then
        vOutlook := CreateOleObject(C_Ole_ApplName);
      vMailItem := vOutlook.CreateItem(0);
    except
      Result := C_MailError_NoOutlook;
      Exit;
    end;

    // Betreff und Mailtext
    vMailItem.Subject := sSubject;
    vMailItem.Body := sText;

    // Adressaten als Liste
    if (Assigned(pSlTo)) then
      for i := 0 to pSlTo.Count-1 do
        vRecipient := vMailItem.Recipients.Add(pSlTo[i]);

    // CC's als Liste
    if (Assigned(pSlCc)) then
      for i := 0 to pSlCc.Count-1 do begin
        vRecipient := vMailItem.Recipients.Add(pSlCc[i]);
        vRecipient.Type := 2;  // CC
      end;

    // BCC's als Liste
    if (Assigned(pSlBcc)) then
      for i := 0 to pSlBcc.Count-1 do begin
        vRecipient := vMailItem.Recipients.Add(pSlBcc[i]);
        vRecipient.Type := 3;  // BCC
      end;

    // Anzuhängende Dateien als Liste
    if (Assigned(pSlAttach)) then
      for i := 0 to pSlAttach.Count-1 do begin
        if (FileExists(pSlAttach[i])) then
          vMailItem.Attachments.Add(pSlAttach[i]);
      end;

    vMailItem.Display;   // Mail anzeigen
//    vMailItem.Send;      // Mail senden
    Result := C_MailError_OK;  // OK
  except
    Result := C_MailError_OtherError;
  end
  else Result := C_MailError_NoRecipient;   // Fehler: kein Adressat
end;

procedure TFormCreateLizenzFile.FormCreate(Sender: TObject);
begin
  btnEncode.Visible := (ParamStr(1) = 'WENCODE');
end;

procedure TFormCreateLizenzFile.EncodeLizenzFile;
var
  s, sLine      : string;
  i, iAmplitude : integer;
  iOfs, iZyk, iStp : integer;
begin
  if (OpenDialog.Execute) then begin

    with TStringlist.Create do
    try
      LoadFromFile(OpenDialog.FileName);
      if (Count = 1) then sLine := Strings[0];
    finally
      Free;
    end;

    if (Length(sLine) > 4) then begin
      iOfs := Ord(sLine[1]);
      iZyk := Ord(sLine[2]);
      iStp := Ord(sLine[3]);
    end
    else Exit;

    // Entschlüsseln
    iAmplitude := iOfs;
    for i := 4 to Length(sLine) do begin
      if (Odd(i div iZyk))
      then Dec(iAmplitude, iStp)
      else Inc(iAmplitude, iStp);
      sLine[i] := Chr(Ord(sLine[i]) - (iAmplitude));
    end;

    // Dummy-Zeichen entfernen
    s := '';
    for i := 4 to Length(sLine) do if (not Odd(i)) then s := s + sLine[i];

    ShowMessage('Lizenzstring: >' + s + '<');

  end;
end;

function TFormCreateLizenzFile.CreateLizenzList: TStrings;
var
  s, sLine      : string;
  i, iAmplitude : integer;
  iOfs, iZyk, iStp : integer;
begin
  Result := TStringList.Create;

  Randomize;
  s := MyGetComputerName;

  iOfs := C_Offset + Random(C_Offset);
  iZyk := C_Zyklus + Random(C_Zyklus);
  iStp := C_Step + Random(C_Step);

  // Mit Dummyzeichen füllen
  sLine := Chr(iOfs) + Chr(iZyk) + Chr(iStp);  // Kennwerte
  for i := 1 to Length(s) do sLine := sLine + s[i] + Chr(32 + Random(32));

  // Verschlüsseln
  iAmplitude := iOfs;
  for i := 4 to Length(sLine) do begin
    if (Odd(i div iZyk))
    then Dec(iAmplitude, iStp)
    else Inc(iAmplitude, iStp);
    sLine[i] := Chr(Ord(sLine[i]) + (iAmplitude));
  end;

  Result.Add(sLine);
end;

procedure TFormCreateLizenzFile.SaveLizenzToFile;
var
  sFile : string;
begin
  sFile := ExtractFilePath(ParamStr(0));
  if (SelectDirectory(sFile, [sdAllowCreate, sdPerformCreate, sdPrompt], 0)) then
    with CreateLizenzList do
    try
      sFile := IncludeTrailingBackslash(sFile) + C_LizenzFileName;
      SaveToFile(sFile);
      MessageDlg(Format(S_Saved, [sFile]), mtInformation, [mbOk], 0);
    finally
      Free;
    end;
end;

procedure TFormCreateLizenzFile.SendLizenzByMail;
var
  sFile                           : TFileName;
  pSlTo, pSlCc, pSlBcc, pSlAttach : TStrings;
begin
  pSlTo := TStringList.Create;
  pSlCc  := TStringList.Create;
  pSlBcc  := TStringList.Create;
  pSlAttach  := TStringList.Create;
  with CreateLizenzList do
  try
    sFile := ExtractFilePath(ParamStr(0)) + C_LizenzFileName;
    SaveToFile(sFile);
    pSlTo.Add(C_ReceiveAdress);
    pSlAttach.Add(sFile);
    try
      if (SendOutlookMail('Lizenzanforderung WICO22', S_LizenzText, pSlTo,
        pSlCc, pSlBcc, pSlAttach) = C_MailError_OK)
      then MessageDlg(S_Mailed, mtInformation, [mbOk], 0)
      else MessageDlg(S_Error, mtError, [mbOk], 0);
    finally
      DeleteFile(sFile);
    end;
  finally
    Free;
    pSlTo.Free;
    pSlCc.Free;
    pSlBcc.Free;
    pSlAttach.Free;
  end;
end;

procedure TFormCreateLizenzFile.bbtnOnlineLizClick(Sender: TObject);
begin
  SendLizenzByMail;
end;

procedure TFormCreateLizenzFile.bbtnFileLizClick(Sender: TObject);
begin
  SaveLizenzToFile;
end;

procedure TFormCreateLizenzFile.btnEncodeClick(Sender: TObject);
begin
  EncodeLizenzFile;
end;

procedure TFormCreateLizenzFile.bbtnTelLizClick(Sender: TObject);
begin
  CheckComputerName;
end;

end.
