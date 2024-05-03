{******************************************************************************}
{* Unit: DSfG-Abrufliste f�r Archive, Logb�cher, Instanzwerte                 *}
{* 30.01.2003 WW                                                              *}
{* 15.03.2012 WW  mit �ffentlichem Schl�ssel X, Y f�r digitale Signatur       *}
{******************************************************************************}
unit DAbrufList;

interface

uses
  contnrs, WStrUtils, WSysCon, AbrufCmd, WChars, DD_Allg;

type
  { Record f�r DSfG-Abrufdaten }

  TDSfGAbrufData = record
    EAdr: string;      { Busadresse der Instanz, deren Daten gelesen werden sollen }
    PublicKey_x: string;  { �ffentlicher Schl�ssel X f�r digitale Signatur }
    PublicKey_y: string;  { �ffentlicher Schl�ssel Y f�r digitale Signatur }
    DEL: string;       { ein oder mehrere (durch <GS> getrennte) Einzel-Datenelemente oder von-Del bei Bereich }
    DEL_bis: string;   { nur f�r DEL-Bereichsabfrage: bis-Del (sonst leer) }
  end;

  { Objekt f�r TDSfGAbrufList }

  TDSfGAbrufListObj = class(TObject)
    Data: TDSfGAbrufData;
  public
    procedure SetData (ADSfGAbrufData: TDSfGAbrufData);
  end;

  { Abrufliste f�r DSfG-Archive, -Logb�cher, -Instanzwerte }

  TDSfGAbrufList = class(TObjectList)
  public
    procedure LoadFromXMLAdressliste (AdrList: string; DEL_einzeln: boolean);
    function CheckForVirtualDEA (iIndex: integer): string;
  end;


implementation

uses SysUtils;

{ TDSfGAbrufListObj }

{-------------------------------------------------------------------}
procedure TDSfGAbrufListObj.SetData (ADSfGAbrufData: TDSfGAbrufData);
{-------------------------------------------------------------------}
begin
  Data:=ADSfGAbrufData;
end;

{ TDSfGAbrufList }

{--------------------------------------------------------------------------------------}
procedure TDSfGAbrufList.LoadFromXMLAdressliste (AdrList: string; DEL_einzeln: boolean);
{--------------------------------------------------------------------------------------}
{ AdrList-String aus XML-Clientanforderung in DSfG-Abrufliste konvertieren;
  �bergabe: Adresslisten-String
            Flag DEL_einzeln (wenn true, werden alle Datenelement-Adressen einzeln
                              in die Liste geschrieben, ansonsten je Busadresse jeweils
                              ein Listen-Eintrag mit allen Datenelementen) }
var
  S: string;
  InstDataStr: string;
  InstDataTeilStr: string;
  AdrStr: string;
  BusAdr: string;
  i: integer;
  DSfGAbrufData: TDSfGAbrufData;
  DSfGAbrufListObj: TDSfGAbrufListObj;
  DELBuf: string;
  sPubKeyX: string;
  sPubKeyY: string;

begin
  S:=AdrList;
  while length (S) > 0 do begin
    InstDataStr:=F_Zerlegen (S, C_CmdSeparator);  { Adressdaten (Busadresse und DEL-Adressen) }
    i:=0;
    while length (InstDataStr) > 0 do begin
      inc (i);
      AdrStr:=F_Zerlegen (InstDataStr, C_CmdSeparatorAdr);  { Busadresse oder DEL-Adresse }

      if DEL_einzeln then begin    { DEL einzeln in die Abrufliste eintragen (f�r Archiv/Logbuch-Abfrage) }
        if i = 1 then
          BusAdr:=AdrStr  { Busadresse steht an erster Stelle }
        else if i = 2 then
          sPubKeyX:=AdrStr  { �ffentlicher Schl�ssel X steht an zweiter Stelle }
        else if i = 3 then
          sPubKeyY:=AdrStr  { �ffentlicher Schl�ssel Y steht an dritter Stelle }
        else begin
          with DSfGAbrufData do begin  { ...anschlie�end folgen die einzelnen Busadressen }
            EAdr:=BusAdr;
            PublicKey_x:=sPubKeyX;  // 15.03.2012, WW
            PublicKey_y:=sPubKeyY;  // 15.03.2012, WW
            DEL:=AdrStr;
            DEL_bis:='';    { vorerst keine Bereiche }
          end;
          DSfGAbrufListObj:=TDSfGAbrufListObj.Create;
          DSfGAbrufListObj.SetData (DSfGAbrufData);
          Add (DSfGAbrufListObj);
        end;
      end
      else begin   { DEL-Gruppe in die Abrufliste eintragen (f�r Abfrage mehrerer DEL mit einem Befehl) }
        with DSfGAbrufData do begin
          EAdr:=AdrStr;  { Busadresse steht an erster Stelle }
          { �ffentlicher Schl�ssel X steht an zweiter Stelle; 15.03.2012, WW }
          PublicKey_x:=F_Zerlegen (InstDataStr, C_CmdSeparatorAdr);
          { �ffentlicher Schl�ssel Y steht an zweiter Stelle; 15.03.2012, WW }
          PublicKey_y:=F_Zerlegen (InstDataStr, C_CmdSeparatorAdr);

          DELBuf:='';
          InstDataTeilStr:=F_Zerlegen (InstDataStr, C_CmdSeparatorAdr);
          while length (InstDataTeilStr) > 0 do begin
            if Pos (C_CmdSeparatorVonBis, InstDataTeilStr) > 0 then begin
              { Teileintrag enth�lt einen von-bis-Datenelementbereich }
              DEL:=F_Zerlegen (InstDataTeilStr, C_CmdSeparatorVonBis);  { von }
              DEL_bis:=InstDataTeilStr;  { bis }

              DSfGAbrufListObj:=TDSfGAbrufListObj.Create;
              DSfGAbrufListObj.SetData (DSfGAbrufData);
              Add (DSfGAbrufListObj);
            end else
              { Einzel-Datenelemente aneinanderreihen, durch <GS> getrennt (f�r DSfG-Befehl): }
              DELBuf:=DELBuf + InstDataTeilStr + GS;

            { aufgesammelte Einzel-Datenelemente in Liste abspeichern, wenn
              max. Telegramml�nge �berschritten ist oder InstDataStr komplett abgearbeitet ist: }
            if length (DELBuf) > 0 then begin
              if (length (DELBuf) > CMaxDSfGTelegrammLaenge) OR (length (InstDataStr) = 0) then begin
                if DELBuf [length (DELBuf)] = GS then
                  DELBuf:=Copy (DELBuf, 1, length (DELBuf)-1);   { evtl. vorhandenen <GS> am Ende wegschneiden }
                DEL:=DELBuf;
                DEL_bis:='';    { Einzel-Datenelemente, kein Bereich }

                DSfGAbrufListObj:=TDSfGAbrufListObj.Create;
                DSfGAbrufListObj.SetData (DSfGAbrufData);
                Add (DSfGAbrufListObj);

                DELBuf:='';   { f�r n�chstes Telegramm wieder mit leer vorbelegen }
              end;
            end;

            InstDataTeilStr:=F_Zerlegen (InstDataStr, C_CmdSeparatorAdr);
          end;  { while }
        end;  { with }
        Break;
      end;
    end;  { while length (InstDatastr) > 0 }
  end;  { while length (S) > 0 }
end;

{-------------------------------------------------------------------}
function TDSfGAbrufList.CheckForVirtualDEA (iIndex: integer): string;
{-------------------------------------------------------------------}
{ Pr�fen, ob virtuelle DE-Adresse enthalten ist (Ger�tezustand). Wenn ja,
  Abrufliste durch Befehl zum Lesen der einzelnen Ger�tezustands-DE ersetzen;
  Ergebnis: Virtuelle DE-Adresse, wenn enthalten (sonst leer) }
var
  DSfGAbrufData: TDSfGAbrufData;
  sDELReplace: string;

begin
  Result:='';
  if (Count > 0) AND (iIndex < Count) then begin
    DSfGAbrufData:=TDSfGAbrufListObj (Items [iIndex]).Data;

    if Pos (CD_ALLG_MRG900_GerZustand_aktuell, DSfGAbrufData.DEL) > 0 then begin
      sDELReplace:='';
      if Pos (CD_ALLG_Eichschalter, DSfGAbrufData.DEL) = 0 then
        sDELReplace:=sDELReplace + CD_ALLG_Eichschalter + GS;  // DE Eichschalter Zustand
      if Pos (CD_WIESER_Ausg1_Zustand, DSfGAbrufData.DEL) = 0 then
        sDELReplace:=sDELReplace + CD_WIESER_Ausg1_Zustand + GS;  // DE Signalausgang 1 Zustand
      if Pos (CD_WIESER_Ausg2_Zustand, DSfGAbrufData.DEL) = 0 then
        sDELReplace:=sDELReplace + CD_WIESER_Ausg2_Zustand + GS;  // DE Signalausgang 2 Zustand

      if length (sDELReplace) > 0 then begin
        sDELReplace:=Copy (sDELReplace, 1, length (sDELReplace)-1);  // GS am Ende wegschneiden
        DSfGAbrufData.DEL:=StringReplace (DSfGAbrufData.DEL,
          CD_ALLG_MRG900_GerZustand_aktuell, sDELReplace, [rfReplaceAll]);
      end
      else begin
        DSfGAbrufData.DEL:=StringReplace (DSfGAbrufData.DEL,
          CD_ALLG_MRG900_GerZustand_aktuell + GS, '', [rfReplaceAll]);
      end;

      TDSfGAbrufListObj (Items [iIndex]).Data:=DSfGAbrufData;
      Result:=CD_ALLG_MRG900_GerZustand_aktuell;
    end;
  end;
end;

end.
