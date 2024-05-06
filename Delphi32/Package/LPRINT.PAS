unit Lprint;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, printers, PrnDialo, inifiles;

const
  maxTab = 50;
  maxChar= 500;

type

  TFormat = (Hoch,Quer);
  TJaNein = (Ja,Nein);

  TTabulator = Record
     Breite : Byte;
     Ausrichtung : Char;
  end;

  TStrObject= Class(TObject)
    Text: array[0..maxChar] of char;
  end;

  TListPrint = class(TComponent)
  private
    { Private-Deklarationen }
    indexText: integer;
    XRand,
    YKopf    : word;
    FCaption : string;
    YposAlt,
    ypos,
    page     : integer;
    FGrafikDruck : TJaNein;
    FKopfDruck   : TJaNein;
    FK1Zeile : string;
    FK2Zeile : string;
    FK3Zeile : string;
    FFont  : TFont;
    FK1Font: TFont;
    FK2Font: TFont;
    FK3Font: TFont;
    FSNummerierung: TJaNein;
    FDialogFenster: TJaNein;
    FFormat       : TFormat;
    FAnzUbs ,
    FUntererRand,
    FLinkerRand   : integer;
    TabListe      : array[1..maxTab] of TTabulator;
    StringListe   : TStringList;
    UbsFont,
    AktFont       : TFont;
    TabModus      : boolean;
    IniDatei      : string;
    pageNr        : integer;
    procedure SetFont(Value: TFont);
    procedure SetK1Font(Value: TFont);
    procedure SetK2Font(Value: TFont);
    procedure SetK3Font(Value: TFont);
    procedure ZeileDrucken(ptext: pchar);
    procedure kopf;
    procedure NeuSeite;
    procedure LadeFont(nach,von: TFont);
 protected
    { Protected-Deklarationen }
  public
    { Public-Deklarationen }
    xmst,
    ymst     : real;
    Constructor create(AOwner:TComponent); override;
    destructor Destroy; override;
    function Drucken:boolean;
    procedure MakeYpos;
    procedure setYpos;
    procedure AddText(text1:string; text2: pchar);
    procedure AddTabulator(Tabulator:string);
    procedure AddSchriftHoehe(Hoehe:byte);
    procedure AddFontStyle(Style: String);
    procedure AddNewPage;
    procedure AddLine(Breite:byte);
    procedure AddYpos(dy:integer);
    procedure AddYposNew(y:integer);
    procedure AddXText(xPosition:byte; style, text:string);
    procedure SetTabModusZeichen;
    procedure SetTabModusMillimeter;
    procedure SetPageOffset(offset:integer);
    function  getPageOffset:integer;
  published
    { Published-Deklarationen }
    property GrafikDruck: TJaNein read FGrafikDruck write FGrafikDruck default Nein;
    property Font: TFont read FFont write SetFont;
    property KopfDruck: TJaNein read FKopfDruck write FKopfDruck default Ja;
    property Kopfzeile1Font: TFont read FK1Font write SetK1Font;
    property Kopfzeile2Font: TFont read FK2Font write SetK2Font;
    property Kopfzeile3Font: TFont read FK3Font write SetK3Font;
    property Caption: String read FCaption write FCaption;
    property Kopfzeile1: String read FK1Zeile write FK1Zeile;
    property Kopfzeile2: String read FK2Zeile write FK2Zeile;
    property Kopfzeile3: String read FK3Zeile write FK3Zeile;
    property DialogFenster: TJaNein read FDialogFenster write FDialogFenster default Ja;
    property SeitenNummerierung: TJaNein read FSNummerierung write FSNummerierung default Ja;
    property Format: TFormat read FFormat write FFormat default Hoch;
    property LinkerRand: Integer  read FLinkerRand  write FLinkerRand default 100;
    property UntererRand: Integer  read FUntererRand  write FUntererRand default 15;
    property AnzUeberschriften: Integer  read FAnzUbs  write FAnzUbs;
  end;

procedure Register;

implementation

const yBasisRand = 5;
      xBasisRand = 7;
      pageHeightBasis = 6778;
      pageWidthBasis = 4672;

      PageOffset : integer = 0;

procedure TListPrint.SetFont(Value: TFont);
begin
 FFont.Assign(Value)
end;

procedure TListPrint.SetK1Font(Value: TFont);
begin
 FK1Font.Assign(Value)
end;

procedure TListPrint.SetK2Font(Value: TFont);
begin
 FK2Font.Assign(Value)
end;

procedure TListPrint.SetK3Font(Value: TFont);
begin
 FK3Font.Assign(Value)
end;

procedure TListPrint.AddText(text1:string; text2: pchar);
var Strobj: TStrObject;
begin
    strobj:=TStrObject.create;
    strPCopy(strobj.text,text1);
    if assigned(text2){ and (strlen(text2)<maxChar) }then strCat(StrEnd(strobj.text),text2);
    StringListe.addObject('S', strObj);
end;

Procedure TListPrint.SetYpos;
begin
  StringListe.add('ZS')
end;

Procedure TListPrint.MakeYpos;
begin
  StringListe.add('ZM')
end;

procedure TListPrint.AddTabulator(Tabulator:string);
begin
  StringListe.add('T'+Tabulator);
end;

procedure TListPrint.AddSchriftHoehe(Hoehe:byte);
begin
  StringListe.add('H'+IntToStr(Hoehe));
end;

procedure TListPrint.AddFontStyle(Style: String);
begin
  StringListe.add('F'+Style);
end;

procedure TListPrint.AddNewPage;
begin
  StringListe.add('P');
end;

procedure TListPrint.AddLine(Breite:byte);
begin
  StringListe.add('L'+IntToStr(breite));
end;

procedure TListPrint.AddYpos(dy:integer);
begin
  StringListe.add('Y'+IntToStr(dy));
end;

procedure TListPrint.AddYposNew(y:integer);
begin
  StringListe.add('Z'+IntToStr(y));
end;

procedure TListPrint.AddXText(xPosition:byte; style, text:string);
begin
  StringListe.add('X'+IntToStr(xposition)+#9+style+#9+Text);
end;

procedure TListPrint.SetTabModusZeichen;
begin
  TabModus:=false
end;

procedure TListPrint.SetTabModusMillimeter;
begin
  TabModus:=true
end;

procedure TListPrint.SetPageOffset(offset:integer);
begin
  PageOffset:=offset;
end;

function  TListPrint.getPageOffset:integer;
begin
  result:=pageNr
end;

{-----------------------------------------------------}
function TListPrint.Drucken:boolean;
{-----------------------------------------------------}

var i,p,x,
    TextHoehe,
    breite,
    index,
    Code     : integer;
    TabNummer: byte;
    SText,
    text     : string;
    str      : array[0..256+maxChar] of char;
    strObj   : TStrObject;
    iniFile  : Tinifile;
    PrnDial  : boolean;

begin
 result:=false;
 Inidatei:=application.ExeName;
 Inidatei:=ExtractFilePath(Inidatei)+DruckIni;
 iniFile:=TiniFile.Create(IniDatei);
 PrnDial:=inifile.ReadBool(section_Allgemein, ident_Dialog,true);
 inifile.free;


 if not PrnDial then begin
   if printerDialog.ShowModal <> IDOK then exit
 end;

 iniFile:=TiniFile.Create(IniDatei);
 FFont.Name:=IniFile.readString(section_Allgemein, Ident_FName, FFont.Name);
 FFont.Size:=IniFile.readInteger(section_Allgemein,Ident_FSize,FFont.size);
 inifile.free;


 AktFont:=TFont.create;
 UbsFont:=TFont.create;
 LadeFont(AktFont,Ffont);

 if DialogFenster=Ja
   then if messagedlg(FCaption, mtConfirmation, mbOKCancel, 0) = IDCancel
     then begin
       StringListe.Clear;
       exit;
     end;
 {PrintDlg:=TPrintDlg.Create (Application); } 

 try
   if FFormat=hoch
     then printer.Orientation:=poPortrait
     else printer.Orientation:=PoLandscape;
   if FFormat=hoch
     then begin
       xmst:=printer.pagewidth/(210-xBasisRand);
       ymst:=printer.pageheight/(297-yBasisRand)
     end
     else begin
       xmst:=printer.pagewidth/(297-yBasisRand);
       ymst:=printer.pageHeight/(210-xBasisRand);
     end;

   printer.BeginDoc;
   page:=1;
   kopf;
   for i:=1 to maxTab do with TabListe[i] do begin
     Breite:=20;
     Ausrichtung:='L'
   end;

   indexText:=0;
   for index:=0 to StringListe.Count-1 do begin
     text:=copy(Stringliste.strings[index],2,length(Stringliste.strings[index]));

     if (upcase(Stringliste.strings[index][1])='S') then begin
       StrPCopy(str,text);
       StrObj:=TStrObject(Stringliste.objects[index]);
       strCat(str,strObj.text);
       ZeileDrucken(str);
       if indexText<FAnzUbs then LadeFont(UbsFont,AktFont);
       inc(indexText);
       TextHoehe:=printer.canvas.TextHeight(Stringliste.strings[index]);
       yPos:=ypos+texthoehe;
       if ypos>printer.PageHeight-Ymst*FUntererRand then NeuSeite
     end

     else if upcase(Stringliste.strings[index][1])='T' then begin
       { Neue Tabulatoren setzen   T(L;R,M)n..n#9(L,R,M)n..n }
       TabNummer:=1;
       text:=copy(Stringliste.strings[index],2,255);
       repeat
         TabListe[TabNummer].Ausrichtung:=upcase(text[1]);
         text:=copy(text,2,255);
         p:=pos(';',text);
         if p>0 then SText:=copy(text,1,p-1)
                else Stext:=copy(text,1,255);
         val(SText,breite,code);
         if code=0 then TabListe[TabNummer].Breite:=breite;
         text:=copy(text,p+1,255);
         inc(TabNummer)
       until p=0
     end

     else if upcase(Stringliste.strings[index][1])='H'
       then AktFont.size:=StrToInt(copy(Stringliste.strings[index],2,255))

     else if upcase(Stringliste.strings[index][1])='F' then begin
       AktFont.style:=[];
       if pos('B',Stringliste.strings[index])>0
         then AktFont.style:= AktFont.style+[fsBold];
       if pos('I',Stringliste.strings[index])>0
         then AktFont.style:= AktFont.style+[fsItalic];
       if pos('U',Stringliste.strings[index])>0
         then AktFont.style:= AktFont.style+[fsUnderline];
       if pos('S',Stringliste.strings[index])>0
         then AktFont.style:= AktFont.style+[fsStrikeOut];
     end

     else if upcase(Stringliste.strings[index][1])='P' then NeuSeite

     else if upcase(Stringliste.strings[index][1])='L' then begin
       printer.Canvas.pen.width:=StrToInt(copy(Stringliste.strings[index],2,99));
       ypos:=ypos+round(ymst*1);
       printer.canvas.moveTo(xRand,ypos);
       printer.canvas.LineTo(printer.pageWidth,ypos);
       ypos:=ypos+round(ymst*1);
     end

     else if upcase(Stringliste.strings[index][1])='Y'
        then begin
          ypos:=ypos+round(ymst*StrToInt(copy(Stringliste.strings[index],2,99)));
          inc(IndexText)
        end

     else if upcase(Stringliste.strings[index][1])='Z'
        then begin
          if upcase(Stringliste.strings[index][2])='M'
            then YposAlt:=ypos;
          if upcase(Stringliste.strings[index][2])='S'
            then Ypos:=YposAlt;
          inc(IndexText)
        end

     else if upcase(Stringliste.strings[index][1])='X' then begin
       p:=pos(#9,Stringliste.strings[index]);
       text:=copy(Stringliste.strings[index],2,p-2);
       x:=xrand+round(xmst*strToInt(text));
       text:=copy(Stringliste.strings[index],p+1,255);
       p:=pos(#9,text);
       stext:=copy(text,1,p-1);
       printer.canvas.font.style:=[];
       if pos('B',stext)>0
         then printer.canvas.font.style:= printer.canvas.font.style+[fsBold];
       if pos('I',stext)>0
         then printer.canvas.font.style:= printer.canvas.font.style+[fsItalic];
       if pos('U',stext)>0
         then printer.canvas.font.style:= printer.canvas.font.style+[fsUnderline];
       if pos('S',stext)>0
         then printer.canvas.font.style:= printer.canvas.font.style+[fsStrikeOut];
       text:=copy(text,p+1,255);
       printer.canvas.textout(x,ypos,text);
       printer.canvas.font.style:=[];
     end

   end;


   PageNr:=printer.pageNumber;
 finally
   if FGrafikDruck = Nein
     then Printer.EndDoc;
   StringListe.Clear;
   aktFont.free;
   ubsFont.free;
   result:=true;
 end
end;

{-----------------------------------------------}
procedure TListPrint.ZeileDrucken(ptext: pchar);
{-----------------------------------------------}
var
    TabNummer: byte;
    ZeichenBreite : real;
    p,x,
    xpos      : integer;
    TabPos,
    TabPosEnd : pchar;
    SText     : string;
    c        : char;
begin
    TabNummer:=1;
    TabPos:=StrPos(ptext,#9);
    xpos:=XRand;

    while assigned(TabPos) do begin
      printer.canvas.font.Color:=clBlack;
      LadeFont(printer.Canvas.Font,AktFont);
      ZeichenBreite:=printer.canvas.textWidth('1234567890')/10;
      inc(TabPos);
      if TabPos[0]<>#9 then begin
        TabPosEnd:=StrPos(tabpos+1,#9);
        if assigned(TabPosEnd) then SText:=copy(StrPas(TabPos),1,TabPosEnd-TabPos)
                               else SText:=StrPas(tabpos);
        p:=pos(#27,SText);
        while p>0 do begin
          inc(p);
          c:=upcase(SText[p]);
          case c of
            'B': printer.canvas.font.style:=printer.canvas.font.style+[fsBold];
            'I': printer.canvas.font.style:=printer.canvas.font.style+[fsItalic];
            'U': printer.canvas.font.style:=printer.canvas.font.style+[fsUnderline];
            'S': printer.canvas.font.style:=printer.canvas.font.style+[fsStrikeout];
            'C': begin
                   inc(p);
                   c:=upcase(SText[p]);
                   case c of
                     'B': printer.canvas.font.Color:=clBlue;
                     'R': printer.canvas.font.Color:=clred;
                     'G': printer.canvas.font.Color:=clGreen;
                   end
                 end
           end;
           SText:=copy(SText,p+1,255);
           p:=pos(#27,SText);
        end;
        if TabModus then begin { Tabulator wird in mm angegeben }
        if TabListe[TabNummer].Ausrichtung='L'
          then x:=xPos
          else if TabListe[TabNummer].Ausrichtung='R'
            then x:=xpos+round(xmst*TabListe[TabNummer].Breite)-printer.canvas.textWidth(SText)
              else if TabListe[TabNummer].Ausrichtung='M'
                 then begin
                    x:=xpos+(round(xmst*TabListe[TabNummer].Breite)-printer.canvas.textWidth(SText)) div 2;
                 end
                 else x:=xpos;
        end
        else begin { Tabulator wird in Zeichen angegeben }
        if TabListe[TabNummer].Ausrichtung='L'
          then x:=xPos
          else if TabListe[TabNummer].Ausrichtung='R'
            then x:=xpos+round(ZeichenBreite*TabListe[TabNummer].Breite)-printer.canvas.textWidth(SText)
              else if TabListe[TabNummer].Ausrichtung='M'
                 then begin
                    x:=xpos+(round(ZeichenBreite*TabListe[TabNummer].Breite)-printer.canvas.textWidth(SText)) div 2;
                 end
                 else x:=xpos;
        end;
        printer.canvas.textOut(x,ypos,SText);
        ptext:=TabPos;
        TabPos:=strPos(ptext,#9);
      end; { Spalte durcken }
      if TabModus
        then Xpos:=Xpos+round(xmst*TabListe[TabNummer].Breite)
        else Xpos:=Xpos+round(ZeichenBreite*TabListe[TabNummer].Breite);
      inc(TabNummer);
    end;  { Ende der Zeile }
end;


{--------------------------------}
procedure TListPrint.NeuSeite;
{--------------------------------}
var index,
    indexUbs,
    textHoehe: Integer;
    Text     : string;
    HFont    : TFont;
    str      : array[0..256+maxChar] of char;
    strObj   : TStrObject;

begin
  HFont:=TFont.create;
  Printer.NewPage;
  index:=0;
  indexUbs:=0;
  kopf;
  LadeFont(HFont,AktFont);
  LadeFont(AktFont,UbsFont);
  while indexUBS<FAnzUbs do begin
    text:=copy(Stringliste.strings[index],2,length(Stringliste.strings[index]));
     if upcase(Stringliste.strings[index][1])='S' then begin
       StrPCopy(str,text);
       StrObj:=TStrObject(Stringliste.objects[index]);
       strCat(str,strObj.text);
       ZeileDrucken(str);
       TextHoehe:=printer.canvas.TextHeight(Stringliste.strings[index]);
       yPos:=ypos+texthoehe;
       inc(indexUbs);
     end
     else if upcase(Stringliste.strings[index][1])='Y'
        then begin
          ypos:=ypos+round(ymst*StrToInt(copy(Stringliste.strings[index],2,99)));
          inc(indexUbs);
        end;
     inc(index);
  end;
  LadeFont(AktFont,HFont);
  HFont.free;
end;

{--------------------------------}
procedure TListPrint.kopf;
{--------------------------------}
var x1,x2,x3,
    x,y,y3,
    Yrand,
    XBMFeld,
    XDFeld  :integer;
    textHoehe,
    kh1,kh2,
    kh3     : byte;
    bereich : TRect;
    s: string;
    StyleOld: TFontStyles;
    SizeOld : Integer;
    NameOld : TFontName;
    PrnLogo  : boolean;
    inifile  : Tinifile;

begin
if kopfDruck=Ja then begin
  iniFile:=TiniFile.Create(IniDatei);
  PrnLogo:=IniFile.readBool(section_Allgemein, Ident_logo, false);
  inifile.free;

  with AktFont do begin
    StyleOld:=Style;
    SizeOld:=Size;
    NameOld:=Name;
  end;
  if Format=Hoch
    then begin
      yKopf:=round(ymst*(30-yBasisrand));  { Kopfhöhe = 25 mm}
      XRand:=round(xmst*(FLinkerRand-XBasisRand))+round(xmst*10);
      YRand:=round(ymst*3);
    end
    else begin
      yKopf:=round(ymst*(20+FLinkerRand-yBasisrand));
      YRand:=round(Ymst*(FLinkerRand-YBasisRand));
      XRand:=round(xmst*8);
    end;
  XDFeld:=round(xmst*40);
  XBMFeld:=round(xmst*40);
  x2:=printer.pagewidth-XDFeld;
  x1:=XRand+XBMFeld;
  with printer.canvas do begin
    pen.color:=clBlack;
    pen.width:=3;
    MoveTo(XRand,ykopf);
    Lineto(printer.pagewidth,ykopf);
    MoveTo(x2,ykopf);
    Lineto(x2,YRand);
    LadeFont(printer.canvas.font,FFont);
    printer.canvas.font.size:=8;
    TextHoehe:=textHeight('11');
    s:='gedruckt am';
    x:=TextWidth(s)+round(2*xmst);
    x3:=x2+round(1*xmst);
    Textout(x3,YRand,s);
    TextOut(x3+x,Yrand,DateToStr(date));
    Textout(x3,Yrand+TextHoehe,'um');
    TextOut(x3+x,YRand+TextHoehe,TimeToStr(Time));
    if FSNummerierung=Ja
      then Textout(x3,ykopf-TextHoehe,'Seite: '+intToStr(printer.pageNumber+PageOffset));
    MoveTo(x1,ykopf);
    Lineto(x1,YRand);
    inc(page);

    LadeFont(printer.canvas.font,FK1Font);
    x:=textWidth(FK1Zeile);
    x:=((x2-x1-x) div 2)+x1;
    kh1:=printer.canvas.TextHeight(FK1Zeile);
    y:=YRand;
    printer.canvas.textOut(x,y,FK1Zeile);

    LadeFont(printer.canvas.font,FK3Font);
    x:=textWidth(FK3Zeile);
    x:=((x2-x1-x) div 2)+x1;
    kh3:=printer.canvas.TextHeight(FK3Zeile);
    y3:=yKopf-kh3-(kh3 div 2);
    printer.canvas.textOut(x,y3,FK3Zeile);

    LadeFont(printer.canvas.font,FK2Font);
    x:=textWidth(FK2Zeile);
    x:=((x2-x1-x) div 2)+x1;
    kh2:=printer.canvas.TextHeight(FK2Zeile);
    y:=((yrand+kh1+y3) div 2);
    y:=y-(kh2 div 2);
    printer.canvas.textOut(x,y,FK2Zeile);

    Font.Style:=[];

    if PrnLogo then begin
      with bereich do begin
        left:=xrand;
        top:=YRand;
        right:=XRand+round(xmst*30);;
        bottom:=ykopf;
      end;
      if assigned(printerDialog.bitmap)
        then begin
          if true then
            printer.canvas.StretchDraw(bereich,printerDialog.bitmap)
          else with printerDialog do begin
            x:=xrand+((x1-xrand-bitmap.width) div 2);
            y:=YRand+((ykopf-yrand-bitmap.height) div 2);
            printer.canvas.Draw(x,y,bitmap)
          end
        end
        else showMessage('Kein Bitmap');


    end
      else begin
        iniFile:=TiniFile.Create(IniDatei);
        font.size:=Inifile.readInteger(section_name1, Ident_Fsize1, 10);
        font.color:=Inifile.readInteger(section_name1, Ident_Fcolor1, 255);
        if IniFile.ReadBool(section_name1, ident_FBold1, true)
             then font.style:=font.style+[fsBold];
        if IniFile.ReadBool(section_name1, ident_FKursiv1, true)
             then font.style:=font.style+[fsItalic];
        textout(xRand,Yrand, IniFile.readString(section_name1, Ident_text1, 'Zeile1'));
        y:=yRand+textHeight('A');

        font.size:=Inifile.readInteger(section_name2, Ident_Fsize2, 20);
        font.color:=Inifile.readInteger(section_name2, Ident_Fcolor2, 255);
        if IniFile.ReadBool(section_name2, ident_FBold2, true)
             then font.style:=font.style+[fsBold];
        if IniFile.ReadBool(section_name2, ident_FKursiv2, true)
             then font.style:=font.style+[fsItalic];
        textout(xRand,y, IniFile.readString(section_name2, Ident_text2, 'Zeile2'));
        y:=y+textHeight('A');

        font.size:=Inifile.readInteger(section_name3, Ident_Fsize3, 30);
        font.color:=Inifile.readInteger(section_name3, Ident_Fcolor3, 355);
        if IniFile.ReadBool(section_name3, ident_FBold3, true)
             then font.style:=font.style+[fsBold];
        if IniFile.ReadBool(section_name3, ident_FKursiv3, true)
             then font.style:=font.style+[fsItalic];
        textout(xRand,y, IniFile.readString(section_name3, Ident_text3, 'Zeile3'));
        y:=y+textHeight('A');

        font.size:=Inifile.readInteger(section_name4, Ident_Fsize4, 40);
        font.color:=Inifile.readInteger(section_name4, Ident_Fcolor4, 455);
        if IniFile.ReadBool(section_name4, ident_FBold4, true)
             then font.style:=font.style+[fsBold];
        if IniFile.ReadBool(section_name4, ident_FKursiv4, true)
             then font.style:=font.style+[fsItalic];
        textout(xRand,y, IniFile.readString(section_name4, Ident_text4, 'Zeile4'));

        inifile.free;
      end;

  end;
  Ypos:=ykopf+2;
  YposAlt:=Ypos;
  with AktFont do begin
    Style:=StyleOld;
    Size:=SizeOld;
    Name:=NameOld;
  end
end
else begin { Es wird kein Kopf gedruckt ! }
  if Format=Hoch
    then begin
      yKopf:=0;
      XRand:=round(xmst*(FLinkerRand-XBasisRand));
    end
    else begin
      yKopf:=0;
      XRand:=0;
    end;
  ypos:=ykopf+2;
end
end;

{-----------------------------------------------------}
procedure TListPrint.LadeFont(nach, von: TFont);
{-----------------------------------------------------}
begin
  nach.Name:=von.Name;
  nach.style:=von.style;
  nach.size:=von.size
end;

Constructor TListPrint.create(AOwner:TComponent);
var I: integer;
begin
  inherited Create(AOwner);
  TabModus:=true;  {Tabulator-Breite wird in mm angegeben }
  FKopfDruck:=Ja;
  FFont:=TFont.Create;
  FK1Font:=TFont.Create;
  FK2Font:=TFont.Create;
  FK3Font:=TFont.Create;
  FSNummerierung:=Ja;
  FDialogFenster:=Ja;
  FGrafikDruck:=Nein;
  FFormat:=Hoch;
  FLinkerRand:=25-XBasisRand;
  FUntererRand:=15;
  StringListe:=TStringList.create;

  for I:=1 to maxTab do with TabListe[i] do begin
    Breite:=20;
    Ausrichtung:='L'
  end
end;

destructor TListPrint.destroy;
begin
  FFont.free;
  FK1Font.free;
  FK2Font.free;
  FK3Font.free;
  inherited Destroy
end;

procedure Register;
begin
  RegisterComponents('Wieser', [TListPrint]);
end;

end.
