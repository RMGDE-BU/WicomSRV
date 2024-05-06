{------------------------------------------------------------------------------}
{ unit WDialogs                                                                }
{ 08.02.1996 D. Achter                                                         }
{ Info-Dialog                                                                  }
{ 16.04.1999 GD; TInfoDialog erweitert für Lizenz32-Anzeige                    }
{ 24.11.2004 GD; Erweitert um Seriennummer                                     }
{ 20.01.2006 WW; mit resourcestrings                                           }
{ 03.02.2006 SM/GD/WW; Totalumbau RMG Messtechnik                              }
{ 07.09.2006 GD; Hausnummer auf "5" geändert                                   }
{ 04.10.2006 WW; Internet- und E-Mail-Adresse geändert                         }
{ 16.07.2007 GD; Lizenzbutton invisible anstatt disabled                       }
{ 06.11.2007 WW; RMG Corporate Design                                          }
{ 25.01.2008 WW; optional Version aus Dateiressource lesen (property           }
{                'ReadResource')                                               }
{ 23.06.2009 GD  Resource-Leck beseitigt                                       }
{ 10.02.2010 WW; optionale Lizenzoptionen (properties 'LicOptCaption',         }
{                'LicOptItems'                                                 }
{ 23.04.2010 WN; Honeywell Corporate Design, Ableitung von Startwin            }
{ 13.01.2012 GD; Lizenzinfo wird immer gelesen                                 }
{ 09.12.2013 WW; Versionsanzeige: Nebenversion und Ausgabe durch Punkt getrennt}
{ 29.08.2017 WW; Aufruf Info-Dialog mit optionaler Übergabe des Fenster-Titels }
{ 11.07.2018 WW; DEFAULT_CHARSET statt ANSI_CHARSET                            }
{ 09.02.2023 WW; Tastatursteuerung: Schließen mit ESC, Aufruf Internet-Adresse }
{                und schließen mit RETURN                                      }
{------------------------------------------------------------------------------}
unit WDialogs;

interface                         

uses
  Startwin, SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, 
  Clipbrd, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, jpeg, shellapi, Lizenz32,
  WGraphics, ComCtrls;

type
  { Infodialog-Fenster }

  TInfoDialog = class(TForm)
    pnStartwin: TPanel;
    pnLizenz: TPanel;
    lSerienNummer: TLabel;
    lLizenzInfo: TLabel;
    lLicOptItems: TLabel;
    lLicOptCaption: TLabel;
    lLaufzeit: TLabel;
    lExeName: TLabel;
    lAnzahlLizenzen: TLabel;
    lAktiveLizenzen: TLabel;
    lSernr: TLabel;
    lLaufz: TLabel;
    lAktLiz: TLabel;
    lRegLiz: TLabel;
    lExe: TLabel;
    bevLicOptItems: TBevel;
    bvLizenz: TBevel;
    udLicOptItems: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure udLicOptItemsClick(Sender: TObject; Button: TUDBtnType);
    procedure FormDestroy(Sender: TObject);
    procedure pnLizenzClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private-Deklarationen }
    FStartWin: TStartFenster;
    FFirstShow  : boolean;
    FLizFileName : string;
    FLizOptionen : TStrings;
    procedure ReadLizenzInfo;
    procedure ShowLizenzOptionen;
    procedure SendEMail(var sBetreff: String; var sText: String);
  public
    { Public-Deklarationen }
    property LizFileName: string write FLizFileName;
    procedure SetLizOptionen (ACaption: string; AItems: TStrings);  // 10.02.2010, WW
  end;

  { Infodialog-Komponente }

  TDialogInfo = class (TComponent)
  private
    FProgrammName  : string;
    FVersion       : string;
    FLizenz32      : boolean;
    FReadResource  : boolean;
    FLicOptCaption : string;
    FLicOptItems   : TStrings;
    procedure SetLicOptItems (const Value: TStrings);
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(sLizFile: string = ''; sCaption: string = ''): Boolean;
  published
    property ProgrammName: string read FProgrammName write FProgrammName;
    property Version: string read FVersion write FVersion;
    property Lizenz32: boolean read FLizenz32 write FLizenz32;
    property ReadResource: boolean read FReadResource write FReadResource;  // 25.01.2008, WW
    property LicOptCaption: string read FLicOptCaption write FLicOptCaption;  // 10.02.2010, WW
    property LicOptItems: TStrings read FLicOptItems write SetLicOptItems;  // 10.02.2010, WW
  end;

procedure Register;

implementation

{$R *.DFM}

resourcestring
  SFormatDate     = 'dd/mm/yyyy';
  SUnbegrenzt     = 'unbegrenzt';
  SBis            = 'bis';
  SMailBetreff    = 'Anfrage zu Software';
  SMailBetreffLiz = 'Anfrage zu Software mit Seriennummer: %s';
  SMailTextLiz    = 'Programm: %s, Version %s vom %s';


{-------------------------------------------------------}
function Get_FileVersionInfo (sFilename: string): string;
{-------------------------------------------------------}
{ Gibt Versionsinfo zu einer Datei zurück;
  Übergabe: Dateiname
  Ergebnis: Version im Format n.nn }
var
  p: Pointer;
  i, j : cardinal;
  pFileInfo: ^VS_FIXEDFILEINFO;
  FileInfo: VS_FIXEDFILEINFO;

begin
  if FileExists (sFilename) then begin
    FillChar(FileInfo, SizeOf(FileInfo), 0);
    j := GetFileVersionInfoSize(PChar(sFileName), i);
    if (j > 0) then begin
      GetMem(p, j);
      try
        if (GetFileVersionInfo(PChar(sFileName), 0, j, p)) then begin
          i := SizeOf(VS_FIXEDFILEINFO);
          if (VerQueryValue(p, '\', Pointer(pFileInfo), i)) then
            FileInfo := pFileInfo^;
        end;
      finally
        Freemem(p, j);
      end;
    end;
    Result:=IntToStr (HiWord(FileInfo.dwFileVersionMS)) + '.' +
            IntToStr (LoWord(FileInfo.dwFileVersionMS)) + '.' +  // 09.12.2013, WW
            IntToStr (HiWord(FileInfo.dwFileVersionLS));
  end else
    Result:='';
end;

{----------------------------------- TDialogInfo ------------------------------}

{--------------------------------------------------}
constructor TDialogInfo.Create (AOwner: TComponent);
{--------------------------------------------------}
begin
  inherited Create (AOwner);
  FLicOptItems := TStringList.Create;
end;

{-----------------------------}
destructor TDialogInfo.Destroy;
{-----------------------------}
begin
  FLicOptItems.Free;
  inherited Destroy;
end;

{----------------------------------------------------------}
procedure TDialogInfo.SetLicOptItems(const Value: TStrings);
{----------------------------------------------------------}
begin
  if Assigned(FLicOptItems) then
    FLicOptItems.Assign(Value)
  else
    FLicOptItems := Value;
end;

{ Info-Dialog aufrufen                                           }
{ Optionale Übergaben: Name der Lizenz-Datei (leer = Standard)   }
{                      Titel des Info-Dialogs (leer = Standard)  }
{-----------------------------------------------------------}
function TDialogInfo.Execute(sLizFile: string = '';
  sCaption: string = ''): Boolean;
{-----------------------------------------------------------}
var
  InfoDialog: TInfoDialog;
  sVersion  : String;
begin
  InfoDialog := TInfoDialog.Create (Application);
  try
    with InfoDialog do begin
      if sCaption <> '' then
        InfoDialog.Caption := sCaption;  // 29.08.2017, WW
      LizFileName := sLizFile;
      FStartWin.ProgName := FProgrammName;
      if FReadResource then  // Version aus Dateiressource lesen; 25.01.2008, WW
        sVersion := Get_FileVersionInfo (Application.ExeName) + FVersion
      else
        sVersion := FVersion;  // übergebene Version
      pnLizenz.Visible := FLizenz32;

      SetLizOptionen (FLicOptCaption, FLicOptItems);  // 10.02.2010, WW

      FStartWin.Version := sVersion;
    end;
    Result := InfoDialog.ShowModal = IDOK;
  finally
    InfoDialog.Free;
  end;
end;

{----------------------------------- TInfoDialog ------------------------------}

{------------------------------------------------}
procedure TInfoDialog.FormCreate(Sender: TObject);
{------------------------------------------------}
const
  csCopyRight = 'Copyright © 2000 - %s, RMG Messtechnik GmbH';
var
  dtExeFile: TDateTime;
begin
  dtExeFile:=FileDateToDateTime (FileAge (Application.ExeName));

  FStartWin := TStartFenster.Create(Self);
  with FStartWin do
  begin
    Parent := pnStartwin;
    Align  := alClient;
    ExeDate   := FormatDateTime (SFormatDate, dtExeFile);
    CopyRight := Format (csCopyRight, [FormatDateTime ('yyyy', dtExeFile)]);
    OnImageClick := pnLizenzClick;
    BevelOn  := False;
    SendMail := SendEMail;
    Visible  := True;
  end;
  FLizFileName := '';
  FFirstShow := True;
  FLizOptionen := nil;
end;

{----------------------------------------------}
procedure TInfoDialog.FormDestroy(Sender: TObject);
{----------------------------------------------}
begin
  FreeAndNil(FStartWin);
end;

{----------------------------------------------}
procedure TInfoDialog.FormShow(Sender: TObject);
{----------------------------------------------}
begin
  if (FFirstShow) then begin
    FFirstShow := False;

    ReadLizenzInfo;  // 13.01.2012 Lizenzinfo wird immer gelesen
    if pnLizenz.Visible then
    begin
      pnLizenz.Height := 130;
    end else
    begin
      pnLizenz.Height := 0;
    end;

    ClientHeight := FStartWin.GesamtHeight + pnLizenz.Height;
    FStartWin.TimerLiveImg := True;
  end;
end;

{-----------------------------------}
procedure TInfoDialog.ReadLizenzInfo;
{-----------------------------------}
var
  Lizenz      : TWLizenz32;
  rec         : TLizenzRec;
  i, iPos     : integer;
begin
  Lizenz:= TWLizenz32.Create(FLizFileName);
  try
    rec:= Lizenz.GetLizenzRecord(Application.exename);
    if rec.LizenzCount > 0 { Gilt als TRUE-Rückgabe } then begin
      lExeName.Caption:= 'Wico22 - ' + rec.ExeName;
      if (rec.LizenzCount >= 100) then lAnzahlLizenzen.Caption:= SUnbegrenzt
        else lAnzahlLizenzen.Caption:= IntToStr(rec.LizenzCount);
      lAktiveLizenzen.Caption:= IntToStr(rec.CurrentCount);
      if (trunc(rec.LastLizenzDate) <> 0)
        then lLaufzeit.Caption:= SBis + ' ' + DateToStr(rec.LastLizenzDate)
        else lLaufzeit.Caption:= SUnbegrenzt;
    end
    else lExeName.Caption:= 'Wico22 - ' + 
      ChangeFileExt(ExtractFileName(ParamStr(0)), ''); // 13.01.2012

    for i := 0 to Lizenz.ExeList.Count-1 do begin
      iPos := Pos('SERIENNUMMER:', Lizenz.ExeList[i]);
      if (iPos = 1) then begin
        lSerienNummer.Caption :=
          Trim(Copy(Lizenz.ExeList[i], Length('SERIENNUMMER:')+1, 20));
        Break;
      end;
    end;
  finally
    Lizenz.Free;
  end;
end;

{------------------------------------------------------------------------}
procedure TInfoDialog.SetLizOptionen (ACaption: string; AItems: TStrings);  // 10.02.2010, WW
{------------------------------------------------------------------------}
const
  CMaxVisibleLizOptionen = 6;  // max. Anzahl sichtbarer Lizenzoptionseinträge

begin
  lLicOptCaption.Caption:=ACaption;
  FLizOptionen:=AItems;

  if FLizOptionen.Count > 0 then begin  // es sollen Lizenzoptionen angezeigt werden
    // Lizenzoptionen anzeigen:
    bevLicOptItems.Visible:=true;
    lLicOptItems.Visible:=true;
    udLicOptItems.Visible:=true;

    udLicOptItems.Min:=0;
    if FLizOptionen.Count > CMaxVisibleLizOptionen then
      udLicOptItems.Max:=FLizOptionen.Count - CMaxVisibleLizOptionen
    else
      udLicOptItems.Max:=udLicOptItems.Min;
    udLicOptItems.Position:=udLicOptItems.Min;

    ShowLizenzOptionen;
  end
  else begin
    // Autosize für Lizenzlabels aktivieren, um volle Fensterbreite nutzen zu können:
    lExeName.AutoSize:=true;
    lAnzahlLizenzen.AutoSize:=true;
    lAktiveLizenzen.AutoSize:=true;
    lLaufzeit.AutoSize:=true;
    lSerienNummer.AutoSize:=true;

    udLicOptItems.Min:=0;
    udLicOptItems.Max:=0;
    udLicOptItems.Position:=udLicOptItems.Min;
  end;
end;

{---------------------------------------}
procedure TInfoDialog.ShowLizenzOptionen;  // 10.02.2010, WW
{---------------------------------------}
{ Lizenzoptionen anzeigen }
var
  i: integer;
  S: string;

begin
  if Assigned (FLizOptionen) then begin
    S:='';
    for i:=udLicOptItems.Position to FLizOptionen.Count - 1 do begin
      if i >= 0 then
        S:=S + FLizOptionen [i] + #13#10;
    end;
    lLicOptItems.WordWrap := False;
    lLicOptItems.Caption:=S;
  end;
end;

{----------------------------------------------------------------------------}
procedure TInfoDialog.udLicOptItemsClick(Sender: TObject; Button: TUDBtnType);
{----------------------------------------------------------------------------}
begin
  ShowLizenzOptionen;
end;

{-------------------------------------------------}
procedure TInfoDialog.pnLizenzClick(Sender: TObject);
{-------------------------------------------------}
begin
  ModalResult := mrOk;
end;

{-------------------------------------------------}
procedure TInfoDialog.SendEMail(var sBetreff: String; var sText: String);
{-------------------------------------------------}
begin
  if lLizenzInfo.Visible then begin
    sBetreff:=Format (SMailBetreffLiz, [lSerienNummer.Caption]);
    sText:=Format (SMailTextLiz, [lExeName.Caption, FStartWin.Version,
                                  FStartWin.ExeDate]);
  end
  else begin
    sBetreff:=SMailBetreff;
    sText:='';
  end;
end;

{-------------------------------------------------}
procedure TInfoDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{-------------------------------------------------}
begin
  if Key = VK_ESCAPE then
    ModalResult := mrOk
  else if Key = VK_RETURN then begin
    FStartWin.lHttpClick(Sender);
    ModalResult := mrOk;
  end;
end;

{--------------------------------- Registrierung ------------------------------}

{-------------------------------------------------}
procedure Register;
{-------------------------------------------------}
begin
  RegisterComponents( 'Wieser', [TDialogInfo]);
end;

end.
