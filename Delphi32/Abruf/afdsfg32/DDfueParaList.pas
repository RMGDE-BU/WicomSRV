{******************************************************************************}
{* Unit: gemeinsame Liste für "DSfG-DFÜ-Parameter" (Wieser-Parameter,         *}
{*       Standard-Parameter wie Identifikation, Kennung etc., NTY-Masken)     *}
{* 11.04.2001  WW                                                             *}
{* 05.01.2005  GD  Erweitert um "LoadFromRohstringXML"                        *}
{* 06.11.2014  WW  Erweitert um Feld "Name"                                   *}
{******************************************************************************}
unit DDfueParaList;

interface

uses
  Classes, Forms, SysUtils,
  WChars, DSfGXmlDecodeObj, WXmlConst, WStrUtils;

type
  { Objekt für TDfueParaList }

  TDfueParaListObj = class(TObject)
    Befehl: string [3];          { Schnittstellenbefehl: z.B. YWB, YWT, A, E, I... }
    ParaAdr: string [4];         { "Parameteradresse": z.B. Parameternr. bei Befehl YWB, DNO bei Befehl A usw.
                                   erweitert von 3 auf 4 für Parameternummern aus YWa-Befehl; 20.07.2011, WW }
    Wert: string [40];
    Name: string [40];  // 06.11.2014, WW
  public
    procedure SetData (ABefehl, AParaAdr, AWert, AName: string);
  end;

  { Datenelementeliste }

  TDfueParaList = class(TList)
  private
    procedure Eintragen (Befehl, ParaAdr, Wert, Name: string);
  public
    destructor Destroy; override;
    procedure LoadFromRohstring_Allg (Rohstring: string; Befehl: string);
    procedure LoadFromRohstring_A (Rohstring: string; Befehl: string);
    procedure LoadFromRohstring_B (Rohstring: string; Befehl: string);
    procedure LoadFromRohstring_kleinA (Rohstring: string; Befehl: string);
    procedure LoadFromRohstring_kleinB (Rohstring: string; Befehl: string);
    function LoadFromRohstringXML (sXmlAntwort: string): boolean;
    function GetDfueParaListObj (Befehl: string; ParaAdr: string): TDfueParaListObj;
    procedure SortByBefehl_ParaAdr;
  end;

implementation


{ TDfueParaListObj }

{---------------------------------------------------------------------------}
procedure TDfueParaListObj.SetData (ABefehl, AParaAdr, AWert, AName: string);
{---------------------------------------------------------------------------}
begin
  Befehl:=ABefehl;
  ParaAdr:=AParaAdr;
  Wert:=AWert;
  Name:=AName;
end;


{ TDfueParaList }

{-------------------------------}
Destructor TDfueParaList.Destroy;
{-------------------------------}
var
  i: integer;
Begin
  for i:=0 to Count-1 do
    TDfueParaListObj (Items [i]).Free;
  inherited Destroy;
end;

{--------------------------------------------------------------------------------------------}
Function TDfueParaList.GetDfueParaListObj (Befehl: string; ParaAdr: string): TDfueParaListObj;
{--------------------------------------------------------------------------------------------}
{ Listenobjekt mit Befehl und ParaAdr ermitteln;
  Ergebnis: nil, wenn nicht gefunden, sonst Listenobjekt }
var
  i: integer;
begin
  Result:=nil;
  for i:=0 to Count - 1 do begin
    if (Befehl = TDfueParaListObj (Items[i]).Befehl) AND (ParaAdr = TDfueParaListObj (Items[i]).ParaAdr) then begin
      Result:=TDfueParaListObj (Items[i]);
      Break;
    end;
  end;
end;

{----------------------------------------------------------------------}
procedure TDfueParaList.Eintragen (Befehl, ParaAdr, Wert, Name: string);
{----------------------------------------------------------------------}
{ Daten in Liste eintragen;
  -> neuer Eintrag, wenn noch keiner für Befehl und ParaAdr vorhanden
  -> Wert updaten, wenn Eintrag für Befehl und ParaAdr bereits vorhanden }
var
  DfueParaListObj: TDfueParaListObj;
begin
  DfueParaListObj:=GetDfueParaListObj (Befehl, ParaAdr);
  if DfueParaListObj <> nil then
    DfueParaListObj.Wert:=Wert      { Wert updaten in vorhandenem Listenobjekt }
  else begin
    DfueParaListObj:=TDfueParaListObj.Create;
    DfueParaListObj.SetData (Befehl, ParaAdr, Wert, Name);
    Add (DfueParaListObj);               { in Datenelementeliste neu eintragen }
  end;
end;

{---------------------------------------------------------------------------------}
procedure TDfueParaList.LoadFromRohstring_Allg (Rohstring: string; Befehl: string);
{---------------------------------------------------------------------------------}
{ Konvertiert die im Rohstring übergebene und vorab bereits auf Richtigkeit
  geprüfte Antwort aller DSfG-DFÜ-Befehle (außer A (NTY-Masken), B (Wieser-Parameter)
  in die DFÜ-Parameterliste.
  Übergabe: Rohstring
            zur Antwort gehörender Befehl }
var
  i: integer;
  Satz: string;
  zeich: char;
  Nr: string;
  ParaAdr: integer;

begin
  i:=3;               { erste zwei Zeichen (STX, Befehlszeichen) überspringen }
  Satz:='';
  ParaAdr:=0;
  while i <= length (Rohstring) do begin
    zeich:=Rohstring[i];
    inc(i);
    if (zeich = US) OR (zeich = ETX) then begin
      if Satz <> '' then begin
        inc (ParaAdr);
        { "künstliche" Parameter-Adresse -> lfd. Zähler für in der Antwort enthaltene Werte: }
        Nr:=IntToStr (ParaAdr);
        Eintragen (Befehl, Nr, Satz, '');          { in Parameterliste schreiben }
      end;
      Satz:='';
      Application.ProcessMessages;
    end
    else
      Satz:=Satz+zeich;
  end;
end;

{------------------------------------------------------------------------------}
procedure TDfueParaList.LoadFromRohstring_A (Rohstring: string; Befehl: string);
{------------------------------------------------------------------------------}
{ Konvertiert die im Rohstring übergebene und vorab bereits auf Richtigkeit
  geprüfte Antwort des A-Befehls (NTY-Masken) in die DFÜ-Parameterliste.
  Übergabe: Rohstring
            zur Antwort gehörender Befehl }
var
  i: integer;
  Satz: string;
  zeich: char;
  NTY: string;
  DNO: string;

begin
  i:=3;                            { erste zwei Zeichen (STX, A) überspringen }
  Satz:='';
  while i <= length (Rohstring) do begin
    zeich:=Rohstring[i];
    inc(i);
    if (zeich = US) OR (zeich = ETX) then begin
      if Satz <> '' then begin
        NTY:=Copy(Satz, 1, length(Satz)-2);                  { NTY -> Wert }
        DNO:=Satz[length(Satz)];                             { DNO -> ParaAdr }
        Eintragen (Befehl, DNO, NTY, '');           { in Parameterliste schreiben }
      end;
      Satz:='';
      Application.ProcessMessages;
    end
    else
      Satz:=Satz+zeich;
  end;
end;

{------------------------------------------------------------------------------}
procedure TDfueParaList.LoadFromRohstring_B (Rohstring: string; Befehl: string);
{------------------------------------------------------------------------------}
{ Konvertiert die im Rohstring übergebene und vorab bereits auf Richtigkeit
  geprüfte Antwort eines Wieser-B-Befehls in die DFÜ-Parameterliste.
  Übergabe: Rohstring
            zur Antwort gehörender Befehl }
var
  i: integer;
  Satz: string;
  zeich: char;
  Nr: string;
  Wert: string;

begin
  i:=3;                            { erste zwei Zeichen (STX, B) überspringen }
  Satz:='';
  while i <= length (Rohstring) do begin
    zeich:=Rohstring[i];
    inc(i);
    if (zeich = US) OR (zeich = ETX) then begin
      if Satz <> '' then begin
        Nr:=Copy(Satz, 1, 3);                  { Parameternummer -> ParaAdr }
        Wert:=Copy(Satz, 4, length(Satz));
        Eintragen (Befehl, Nr, Wert, '');          { in Parameterliste schreiben }
      end;
      Satz:='';
      Application.ProcessMessages;
    end
    else
      Satz:=Satz+zeich;
  end;
end;

{-----------------------------------------------------------------------------------}
procedure TDfueParaList.LoadFromRohstring_kleinA (Rohstring: string; Befehl: string);
{-----------------------------------------------------------------------------------}
{ Konvertiert die im Rohstring übergebene und vorab bereits auf Richtigkeit
  geprüfte Antwort eines Wieser-a-Befehls in die DFÜ-Parameterliste.
  Übergabe: Rohstring
            zur Antwort gehörender Befehl }
var
  i, j: integer;
  Satz: string;
  zeich: char;
  Nr: string;
  Wert: string;
  S: string;
  Name: string;

begin
  i:=3;                            { erste zwei Zeichen (STX, a) überspringen }
  Satz:='';
  while i <= length (Rohstring) do begin
    zeich:=Rohstring[i];
    inc(i);
    if (zeich = US) OR (zeich = ETX) then begin
      if Satz <> '' then begin
        { Default-Parameterdaten (nicht vorhanden): }
        Nr:='';
        Wert:='';
        Name:='';

        j:=0;
        while length (Satz) > 0 do begin
          inc (j);
          S:=F_Zerlegen (Satz, RS);

          case j of
            1: Nr:=S;
            2: Name:=S;  // 06.11.2014, WW
            5: Wert:=S;
          end;  { case j }
        end;  { while length (Satz) > 0 }

        if length (Nr) > 0 then
          Eintragen (Befehl, Nr, Wert, Name);  { in Parameterliste schreiben; Parameternummer -> ParaAdr }
      end;
      Satz:='';
      Application.ProcessMessages;
    end
    else
      Satz:=Satz+zeich;
  end;
end;

{-----------------------------------------------------------------------------------}
procedure TDfueParaList.LoadFromRohstring_kleinB (Rohstring: string; Befehl: string);
{-----------------------------------------------------------------------------------}
{ Konvertiert die im Rohstring übergebene und vorab bereits auf Richtigkeit
  geprüfte Antwort eines Wieser-b-Befehls in die DFÜ-Parameterliste.
  Übergabe: Rohstring
            zur Antwort gehörender Befehl }
var
  i, j: integer;
  Satz: string;
  zeich: char;
  Nr: string;
  Wert: string;
  S: string;
  Name: string;

begin
  i:=3;                            { erste zwei Zeichen (STX, b) überspringen }
  Satz:='';
  while i <= length (Rohstring) do begin
    zeich:=Rohstring[i];
    inc(i);
    if (zeich = US) OR (zeich = ETX) then begin
      if Satz <> '' then begin
        { Default-Parameterdaten (nicht vorhanden): }
        Nr:='';
        Wert:='';
        Name:='';

        j:=0;
        while length (Satz) > 0 do begin
          inc (j);
          S:=F_Zerlegen (Satz, RS);

          case j of
            1: Nr:=S;
            2: Name:=S;  // 06.11.2014, WW
            5: Wert:=S;
          end;  { case j }
        end;  { while length (Satz) > 0 }

        if length (Nr) > 0 then
          Eintragen (Befehl, Nr, Wert, Name);  { in Parameterliste schreiben; Parameternummer -> ParaAdr }
      end;
      Satz:='';
      Application.ProcessMessages;
    end
    else
      Satz:=Satz+zeich;
  end;
end;

{--------------------------------------------------------------}
function Befehl_ParaAdrCompare (Item1, Item2: Pointer): Integer;
{--------------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TDfueParaListObj-Objekten nach:
  1. dem Befehl
  2. der Parameter-Adresse }
begin
  Result := CompareStr (TDfueParaListObj (Item1).Befehl,
                        TDfueParaListObj (Item2).Befehl);    { 1. Sortierkriterium: Befehl }
  if Result = 0 then
    Result := CompareStr (TDfueParaListObj (Item1).ParaAdr,
                          TDfueParaListObj (Item2).ParaAdr); { 2. Sortierkriterium: Parameter-Adresse }
end;

{-------------------------------------------}
procedure TDfueParaList.SortByBefehl_ParaAdr;
{-------------------------------------------}
begin
  Sort (Befehl_ParaAdrCompare);     { Liste nach Befehl und ParaAdr sortieren }
end;

{ Lädt Listeninhalt aus einem XML-codierten string             }
{ Parameter: XML-codierter string                              }
{ Rückgabe: Erfolg ja/nein                                     }
{--------------------------------------------------------------}
function TDfueParaList.LoadFromRohstringXML (sXmlAntwort: string): boolean;
{--------------------------------------------------------------}
var
  i                         : integer;
  sIdent, sBefehl, sParaAdr : string;
begin
  try
    with TDSfGXmlDecodeObject.Create() do
    try
      with ConvertXmlToDfuParamList(sXmlAntwort) do  // XML in Zwischenobject
      try
        // Alle allgemeinen Parameter einfügen
        for i := 0 to Count-1 do begin
          sBefehl := '';       // Vorbelegung
          sParaAdr := '1';     // Vorbelegung
          sIdent := Strings[i];

          // Idents entschlüsseln
          if (Pos(sIdent, C_DSfGTeilnehmerStart) = 1) then
            sBefehl := 'YWT'
          else if (Pos(sIdent, C_DSfGIdentStart) = 1) then
            sBefehl := 'K'
          else if (Pos(sIdent, C_DSfGPasswordStart) = 1) then
            sBefehl := 'I'
          else if (Pos(sIdent, C_DSfGInstAdrStart) = 1) then
            sBefehl := 'E'
          else if (Pos(sIdent, C_DSfGTelNrZentraleStart) = 1) then
            sBefehl := 'R'
          else if (Pos(sIdent, C_DSfGAktDatumStart) = 1) then
            sBefehl := 'U'
          else if (Pos(sIdent, C_DSfGAktZeitStart) = 1) then begin
            sBefehl := 'U';
            sParaAdr := '2';
          end
          else if (Pos(sIdent, C_DSfGHerstellerStart) = 1) then
            sBefehl := 'V'
          else if (Pos(sIdent, C_DSfGProgNameStart) = 1) then begin
            sBefehl := 'V';
            sParaAdr := '2';
          end
          else if (Pos(sIdent, C_DSfGProgVersionStart) = 1) then begin
            sBefehl := 'V';
            sParaAdr := '3';
          end
          else if (Pos(sIdent, C_DSfGDatumLpcStart) = 1) then begin
            sBefehl := 'V';
            sParaAdr := '4';
          end
          else if (Pos(sIdent, C_DSfGZeitLpcStart) = 1) then begin
            sBefehl := 'V';
            sParaAdr := '5';
          end
          else if (Pos(sIdent, C_DSfGInVerbNoLoginStart) = 1) then
            sBefehl := 'D'
          else if (Pos(sIdent, C_DSfGInVerbLoginErrStart) = 1) then begin
            sBefehl := 'D';
            sParaAdr := '2';
          end
          else if (Pos(sIdent, C_DSfGOutVerbErrorStart) = 1) then begin
            sBefehl := 'D';
            sParaAdr := '3';
          end
          else if (Pos(sIdent, C_DSfGOutVerbNoLoginStart) = 1) then begin
            sBefehl := 'D';
            sParaAdr := '4';
          end
          else if (Pos(sIdent, C_DSfGOutVerbLoginErrStart) = 1) then begin
            sBefehl := 'D';
            sParaAdr := '5';
          end;

          // Falls ein Befehl zugeordnet werden konnte, wird eingetragen
          if (sBefehl <> '') then
            Self.Eintragen(sBefehl, sParaAdr, GetMemberVal(i), '');
        end;

        // Alle Wieser-Parameter einfügen
        sBefehl := 'YWB';       // Vorbelegung
        for i := 0 to ParamCount-1 do begin
          sParaAdr := Trim(GetParamNr(i));
          sIdent := GetParamVal(i);
          if (sParaAdr <> '') then
            Self.Eintragen(sBefehl, sParaAdr, sIdent, '');
        end;

        // Alle NTY-Parameter einfügen
        sBefehl := 'A';       // Vorbelegung
        for i := 0 to NtyCount-1 do begin
          sParaAdr := Trim(GetNtyDno(i));
          sIdent := GetNtyVal(i);
          if (sParaAdr <> '') then
            Self.Eintragen(sBefehl, sParaAdr, sIdent, '');
        end;
      finally
        Free;
      end;

      Result := True;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

end.

