unit Prndialo;

interface

uses sysutils, WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
     inifiles, StdCtrls, Dialogs, ExtCtrls, printers;

const DruckIni='LPrint.ini';

      section_Allgemein    = 'Standard_Font';
      ident_Dialog         = 'Dialog';
      ident_FName          = 'Font_Name';
      ident_FSize          = 'Font_Size';
      ident_Logo           = 'Logo';

      section_name1        = 'Name_1';
      ident_text1          = 'Text';
      ident_FName1         = 'Font_Name';
      ident_FSize1         = 'Font_Size';
      ident_FColor1        = 'Font_Color';
      ident_FBold1         = 'Bold';
      ident_FKursiv1       = 'Kursiv';

      section_name2        = 'Name_2';
      ident_text2          = 'Text';
      ident_FName2         = 'Font_Name';
      ident_FSize2         = 'Font_Size';
      ident_FColor2        = 'Font_Color';
      ident_FBold2         = 'Bold';
      ident_FKursiv2       = 'Kursiv';

      section_name3        = 'Name_3';
      ident_text3          = 'Text';
      ident_FName3         = 'Font_Name';
      ident_FSize3         = 'Font_Size';
      ident_FColor3        = 'Font_Color';
      ident_FBold3         = 'Bold';
      ident_FKursiv3       = 'Kursiv';

      section_name4        = 'Name_4';
      ident_text4          = 'Text';
      ident_FName4         = 'Font_Name';
      ident_FSize4         = 'Font_Size';
      ident_FColor4        = 'Font_Color';
      ident_FBold4         = 'Bold';
      ident_FKursiv4       = 'Kursiv';


type
  TPrinterDialog = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    FontDialog: TFontDialog;
    Panel1: TPanel;
    LName: TLabel;
    BtnFont: TBitBtn;
    CBnoDialog: TCheckBox;
    PanelBasis: TPanel;
    RBlogo: TRadioButton;
    RBtext: TRadioButton;
    PanelKopf: TPanel;
    editZ1: TEdit;
    Sbf1: TSpeedButton;
    EditZ2: TEdit;
    SBf2: TSpeedButton;
    FD1: TFontDialog;
    FD2: TFontDialog;
    EditZ3: TEdit;
    SBf3: TSpeedButton;
    FD3: TFontDialog;
    EditZ4: TEdit;
    FD4: TFontDialog;
    SBf4: TSpeedButton;
    PanelLogo: TPanel;
    PaintBoxLogo: TPaintBox;
    procedure BtnFontClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Sbf1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SBf2Click(Sender: TObject);
    procedure SBf3Click(Sender: TObject);
    procedure SBf4Click(Sender: TObject);
    procedure PaintBoxLogoPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RBtextClick(Sender: TObject);
    procedure RBlogoClick(Sender: TObject);
  private
    { Private declarations }
    StartFlag : boolean;
  public
    { Public declarations }
    iniDatei : string;
    Bitmap  : TBitmap;
  end;

var
  PrinterDialog: TPrinterDialog;

implementation

{$R *.DFM}

{----------------------------------------------------}
procedure TPrinterDialog.BtnFontClick(Sender: TObject);
{----------------------------------------------------}
begin
  FontDialog.execute;
  LName.Caption:=FontDialog.font.Name+'  G: '+Inttostr(FontDialog.font.size);
end;


{----------------------------------------------------}
procedure TPrinterDialog.FormCreate(Sender: TObject);
{----------------------------------------------------}
var Datei: string;
begin
 Inidatei:=application.ExeName;
 Inidatei:=ExtractFilePath(Inidatei)+DruckIni;
 StartFlag:=true;
 datei:=application.ExeName;
 datei:=ExtractFilePath(datei)+'logo.bmp';
 if fileexists(datei) then begin
     Bitmap:=TBitmap.Create;
     Bitmap.LoadFromFile(datei);
 end
 else BitMap:=nil
end;

{----------------------------------------------------}
procedure TPrinterDialog.FormActivate(Sender: TObject);
{----------------------------------------------------}
var  iniFile  : Tinifile;
begin
  if StartFlag then begin
    Screen.cursor:=crArrow;
    iniFile:=TiniFile.Create(IniDatei);
    FontDialog.font.Name:=IniFile.ReadString(section_Allgemein, Ident_FName, 'System');
    FontDialog.font.Size:=IniFile.ReadInteger(section_Allgemein,Ident_FSize, 10);
    LName.Caption:=FontDialog.font.Name+'  G: '+Inttostr(FontDialog.font.Size);
    RBlogo.checked:=IniFile.ReadBool(section_Allgemein,Ident_logo,false);
    CBnoDialog.checked:=IniFile.ReadBool(section_Allgemein,Ident_Dialog,false);

    editZ1.text:=IniFile.ReadString(section_name1, ident_text1, 'Zeile1');
    fd1.font.Name:=IniFile.ReadString(section_name1, Ident_FName1, 'System');
    fd1.font.Size:=IniFile.ReadInteger(section_name1,Ident_FSize1, 10);
    fd1.Font.Color:=IniFile.ReadInteger(section_name1,Ident_FColor1, 255);
    if IniFile.ReadBool(section_name1, ident_FBold1, true)
      then fd1.font.style:=fd1.font.style+[fsBold];
    if IniFile.ReadBool(section_name1, ident_FKursiv1, true)
      then fd1.font.style:=fd1.font.style+[fsItalic];

    editZ2.text:=IniFile.ReadString(section_name2, ident_text2, 'Zeile2');
    fd2.font.Name:=IniFile.ReadString(section_name2, Ident_FName2, 'System');
    fd2.font.Size:=IniFile.ReadInteger(section_name2,Ident_FSize2, 20);
    fd2.Font.Color:=IniFile.ReadInteger(section_name2,Ident_FColor2, 255);
    if IniFile.ReadBool(section_name2, ident_FBold2, true)
      then fd2.font.style:=fd2.font.style+[fsBold];
    if IniFile.ReadBool(section_name2, ident_FKursiv2, true)
      then fd2.font.style:=fd2.font.style+[fsItalic];

    editZ3.text:=IniFile.ReadString(section_name3, ident_text3, 'Zeile3');
    fd3.font.Name:=IniFile.ReadString(section_name3, Ident_FName3, 'System');
    fd3.font.Size:=IniFile.ReadInteger(section_name3,Ident_FSize3, 30);
    fd3.Font.Color:=IniFile.ReadInteger(section_name3,Ident_FColor3, 355);
    if IniFile.ReadBool(section_name3, ident_FBold3, true)
      then fd3.font.style:=fd3.font.style+[fsBold];
    if IniFile.ReadBool(section_name3, ident_FKursiv3, true)
      then fd3.font.style:=fd3.font.style+[fsItalic];

    editZ4.text:=IniFile.ReadString(section_name4, ident_text4, 'Zeile4');
    fd4.font.Name:=IniFile.ReadString(section_name4, Ident_FName4, 'System');
    fd4.font.Size:=IniFile.ReadInteger(section_name4,Ident_FSize4, 40);
    fd4.Font.Color:=IniFile.ReadInteger(section_name4,Ident_FColor4, 455);
    if IniFile.ReadBool(section_name4, ident_FBold4, true)
      then fd4.font.style:=fd4.font.style+[fsBold];
    if IniFile.ReadBool(section_name4, ident_FKursiv4, true)
      then fd4.font.style:=fd4.font.style+[fsItalic];

    inifile.free;
    if RBlogo.checked then begin
      RBlogoClick(nil)
    end
    else begin
      RBtextClick(nil)
    end;
  end;
  StartFlag:=false;
end;

{----------------------------------------------------}
procedure TPrinterDialog.Sbf1Click(Sender: TObject);
{----------------------------------------------------}
begin
  if FD1.execute then begin
    editZ1.font:=fd1.font;
    editZ1.update
  end
end;

{----------------------------------------------------}
procedure TPrinterDialog.SBf2Click(Sender: TObject);
{----------------------------------------------------}
begin
  if FD2.execute then begin
    editZ2.font:=fd2.font;
    editZ2.update
  end
end;

{----------------------------------------------------}
procedure TPrinterDialog.SBf3Click(Sender: TObject);
{----------------------------------------------------}
begin
  if FD3.execute then begin
    editZ3.font:=fd3.font;
    editZ3.update
  end
end;

{----------------------------------------------------}
procedure TPrinterDialog.SBf4Click(Sender: TObject);
{----------------------------------------------------}
begin
  if FD4.execute then begin
    editZ4.font:=fd4.font;
    editZ4.update
  end
end;

{----------------------------------------------------}
procedure TPrinterDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
{----------------------------------------------------}
begin
  StartFlag:=true;
end;

{----------------------------------------------------}
procedure TPrinterDialog.OKBtnClick(Sender: TObject);
{----------------------------------------------------}
var  iniFile  : Tinifile;
begin
 iniFile:=TiniFile.Create(IniDatei);
 IniFile.WriteString(section_Allgemein, Ident_FName, FontDialog.font.Name);
 IniFile.WriteInteger(section_Allgemein,Ident_FSize, FontDialog.font.Size);
 IniFile.WriteBool(section_Allgemein,Ident_logo,RBlogo.checked);
 IniFile.WriteBool(section_Allgemein,ident_Dialog,CBnoDialog.checked);

 IniFile.WriteString(section_name1, ident_text1, editZ1.text);
 IniFile.WriteString(section_name1, ident_FName1, fd1.font.name);
 IniFile.WriteInteger(section_name1, ident_FSize1, fd1.font.size);
 IniFile.WriteInteger(section_name1, ident_FColor1, fd1.font.color);
 IniFile.WriteBool(section_name1, ident_FBold1, fsBold in fd1.font.style);
 IniFile.WriteBool(section_name1, ident_FKursiv1, fsItalic in fd1.font.style);

 IniFile.WriteString(section_name2, ident_text2, editZ2.text);
 IniFile.WriteString(section_name2, ident_FName2, fd2.font.name);
 IniFile.WriteInteger(section_name2, ident_FSize2, fd2.font.size);
 IniFile.WriteInteger(section_name2, ident_FColor2, fd2.font.color);
 IniFile.WriteBool(section_name2, ident_FBold2, fsBold in fd2.font.style);
 IniFile.WriteBool(section_name2, ident_FKursiv2, fsItalic in fd2.font.style);

 IniFile.WriteString(section_name3, ident_text3, editZ3.text);
 IniFile.WriteString(section_name3, ident_FName3, fd3.font.name);
 IniFile.WriteInteger(section_name3, ident_FSize3, fd3.font.size);
 IniFile.WriteInteger(section_name3, ident_FColor3, fd3.font.color);
 IniFile.WriteBool(section_name3, ident_FBold3, fsBold in fd3.font.style);
 IniFile.WriteBool(section_name3, ident_FKursiv3, fsItalic in fd3.font.style);

 IniFile.WriteString(section_name4, ident_text4, editZ4.text);
 IniFile.WriteString(section_name4, ident_FName4, fd4.font.name);
 IniFile.WriteInteger(section_name4, ident_FSize4, fd4.font.size);
 IniFile.WriteInteger(section_name4, ident_FColor4, fd4.font.color);
 IniFile.WriteBool(section_name4, ident_FBold4, fsBold in fd4.font.style);
 IniFile.WriteBool(section_name4, ident_FKursiv4, fsItalic in fd4.font.style);

 inifile.free;
end;

{----------------------------------------------------}
procedure TPrinterDialog.PaintBoxLogoPaint(Sender: TObject);
{----------------------------------------------------}
var bereich : TRect;
begin
  with bereich do begin
       left:=0;
       top:=0;
       right:=panelLogo.width;
       bottom:=panelLogo.height;
  end;
  PaintBoxLogo.canvas.StretchDraw(bereich,Bitmap);
end;

{----------------------------------------------------}
procedure TPrinterDialog.FormDestroy(Sender: TObject);
{----------------------------------------------------}
begin
  bitmap.free
end;

{----------------------------------------------------}
procedure TPrinterDialog.RBtextClick(Sender: TObject);
{----------------------------------------------------}
begin
  PanelLogo.visible:=false;
  PanelKopf.visible:=true;
  EditZ1.font:=fd1.font;
  EditZ2.font:=fd2.font;
  EditZ3.font:=fd3.font;
  EditZ4.font:=fd4.font;
  PanelKopf.update
end;

{----------------------------------------------------}
procedure TPrinterDialog.RBlogoClick(Sender: TObject);
{----------------------------------------------------}
begin
  if assigned(bitmap) then
    if bitmap.height>0 then
      PanelLogo.width:=PanelLogo.height*(bitmap.width div bitmap.height);
  PanelLogo.visible:=true;
  PanelKopf.visible:=false;
  PanelLogo.update;
  {paintBoxLogo.Invalidate }
end;

end.
