{------------------------------------------------------------------------------}
{ Objekt für DSfGCard-Zugriff über COM (TCustomDSfGCardCOMObject)              }
{ Objekt für DSfGCard-Parameter-Zugriff über COM (TDSfGCardCOMParamObject)     }
{                                                                              }
{ 22.11.2000  GD    Neu                                                        }
{ 05.12.2000  GD    Vs 1.01   diverse Korrekturen                              }
{ 30.03.2001  GD    Abbruchkriterium bei SendReceiveTelegramm                  }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, 2001                                    }
{------------------------------------------------------------------------------}
unit DCardCOM;

interface

uses
  Windows, SysUtils, Classes, Forms, ComObj, GD_Utils, DSG_Utils, Variants;

type
  TCustomDSfGCardCOMObject = class(TObject)
    constructor Create(
      iBdRate: integer; bShowMessages: boolean = False); virtual;
    destructor Destroy; override;
  private
    FCOMObject : OleVariant;
    FBdRate    : integer;
    FOpened    : boolean;
    FTimeOut   : integer;
    procedure SetBaudRate(Value: integer);
    procedure SetTimeOut(Value: integer);
    function GetCardAdr: char;
    procedure SetCardAdr(Value: char);
    function GetMonitorMode: boolean;
    procedure SetMonitorMode(Value: boolean);
  protected
  public
    function GetTeilnehmer: string;
    function CardIsTeilnehmer: boolean;
    procedure SendTelegramm(sTelegramm: string);
    procedure SetMonitorFileName(sFileName: TFileName);
    function ReceiveTelegramm: string;
    function SendReceiveTelegramm(
      sTelegramm: string; bFolgetelegramme: boolean = True): string;
    function GetDatenElementAbfrage(sTelegramm: string): string;
    property BaudRate: integer read FBdRate write SetBaudRate;
    property CardAddress: char read GetCardAdr write SetCardAdr;
    property TimeOut: integer read FTimeOut write SetTimeOut;
    property Opened: boolean read FOpened;
    property MonitorMode: boolean read GetMonitorMode write SetMonitorMode;
  end;

  TDSfGCardCOMParamObject = class(TCustomDSfGCardCOMObject)
  private
  protected
  public
    function GetAllParameters(cIAdr: char): string;  // a-z
    function GetInstParameters(cIAdr, cITyp: char): string; // Instanzspez. (b,c,d)
    function GetAllgParameters(cIAdr: char): string; // a
    function GetBereichsParameters(
      cIAdr: char; sDEAVon, sDEABis: string): string;
    function GetMultipleParameters(
      cIAdr: char; sDEAString: string; iZAE: integer): string;
    function SendFreezeReceiveAnswers(cIAdr: char): TStrings;
  end;

  TDSfGCardCOMMonitorObject = class(TCustomDSfGCardCOMObject)
  private
  protected
  public
    function InitMonitorMode(aState: boolean): boolean;
    function ReceiveMonitorData: string;
  end;

implementation

const
  C_COMObject = 'DSfGCSvr.DSfGCardInterface';

{--------------------------- TCustomDSfGCardCOMObject -------------------------}

{ Konstruktor                                                }
{------------------------------------------------------------}
constructor TCustomDSfGCardCOMObject.Create(
  iBdRate: integer; bShowMessages: boolean = False);
{------------------------------------------------------------}
begin
  inherited Create;
  try
    FCOMObject := CreateOleObject(C_COMObject);
    Sleep(5000); // Damit Initialisierung durchlaufen wird
    FCOMObject.TimeOut := 60000; // COMSERVER ist nicht der schnellste ...
    FTimeOut := 60000;
    FCOMObject.BaudRate := iBdRate;
    FBdRate := iBdRate;

    CardIsTeilnehmer;
    FOpened := FCOMObject.Opened;
  except
    FOpened := False;
  end;
end;

{ Destruktor                                                 }
{------------------------------------------------------------}
destructor TCustomDSfGCardCOMObject.Destroy;
{------------------------------------------------------------}
begin
  inherited Destroy;
end;

{ Stellt die Baudrate für die Karte ein                      }
{ Parameter: BaudRate                                        }
{------------------------------------------------------------}
procedure TCustomDSfGCardCOMObject.SetBaudRate(Value: integer);
{------------------------------------------------------------}
begin
  if (Value <> FBdRate) then begin
    FCOMObject.BaudRate := Value;
    FBdRate := Value;
  end;
end;

{ Stellt den Timeout für die Karte ein                       }
{ Parameter: TimeOut                                         }
{------------------------------------------------------------}
procedure TCustomDSfGCardCOMObject.SetTimeOut(Value: integer);
{------------------------------------------------------------}
begin
  if (Value <> FTimeOut) then begin
    FCOMObject.TimeOut := Value;
    FTimeOut := Value;
  end;
end;

{ Gibt die Instanzadresse für die Karte zurück               }
{ Rückgabe: Instanzadresse                                   }
{------------------------------------------------------------}
function TCustomDSfGCardCOMObject.GetCardAdr: char;
{------------------------------------------------------------}
begin
  Result := VarToStr(FCOMObject.Adresse)[1];
end;

{ Stellt die Instanzadresse für die Karte ein                }
{ Parameter: Instanzadresse                                  }
{------------------------------------------------------------}
procedure TCustomDSfGCardCOMObject.SetCardAdr(Value: char);
{------------------------------------------------------------}
begin
  if (Value in ['A'..'_']) and (Pos(Value, FCOMObject.GetTeilnehmer) = 0) then
    FCOMObject.Adresse := Value;
end;

{ Gibt den Zustand des Monitormodus für die Karte zurück     }
{ Rückgabe: Monitormodus                                   }
{------------------------------------------------------------}
function TCustomDSfGCardCOMObject.GetMonitorMode: boolean;
{------------------------------------------------------------}
begin
  Result := FCOMObject.MonitorMode;
end;

{ Setzt den Zustand des Monitormodus für die Karte           }
{ Parameter: Monitormodus                                    }
{------------------------------------------------------------}
procedure TCustomDSfGCardCOMObject.SetMonitorMode(Value: boolean);
{------------------------------------------------------------}
begin
  FCOMObject.MonitorMode := Value;
end;

{ Liest die Telnehmerliste aus       }
{ Rückgabe: Teilnehmerliste          }
{------------------------------------}
function TCustomDSfGCardCOMObject.GetTeilnehmer: string;
{------------------------------------}
begin
  Result := FCOMObject.GetTeilnehmer;
end;

{ Ist die Karte in Teilnehmerliste ? }
{ Rückgabe: Ja/Nein                  }
{------------------------------------}
function TCustomDSfGCardCOMObject.CardIsTeilnehmer: boolean;
{------------------------------------}
begin
  Result := FCOMObject.IsTeilnehmer;
end;

{ Versendet ein Telegramm            }
{ Parameter: DSfG-Telegramm          }
{------------------------------------}
procedure TCustomDSfGCardCOMObject.SendTelegramm(sTelegramm: string);
{------------------------------------}
begin
  FCOMObject.SendTelegramm(sTelegramm);
end;

{ Setzt Namen für Monitor-Savefile   }
{ Parameter: Dateiname               }
{------------------------------------}
procedure TCustomDSfGCardCOMObject.SetMonitorFileName(sFileName: TFileName);
{------------------------------------}
begin
  FCOMObject.MonitorFileName := sFileName;
end;

{ Fragt ein Telegramm ab             }
{ Rückgabe: DSfG-Telegramm           }
{------------------------------------}
function TCustomDSfGCardCOMObject.ReceiveTelegramm: string;
{------------------------------------}
begin
  Result := FCOMObject.ReceiveTelegramm;
  if (Pos(Chr(stx), Result) = 0) or (Pos(Chr(etx), Result) = 0) then Result := '';
end;

{ Fragt ein Telegramm ab             }
{ Parameter: Sendetelegramm          }
{ Rückgabe: DSfG-Telegramm oder ''   }
{------------------------------------}
function TCustomDSfGCardCOMObject.SendReceiveTelegramm(
  sTelegramm: string; bFolgetelegramme: boolean = True): string;
{------------------------------------}
const
  C_RepeatCount : byte = 0;  // Wiederholung zur Fehlervermeidung beim DPA2200
var
  s : string;
  iStop : Cardinal;
  i, j : integer;
  pSl  : TStrings;
  cAdr : char;
begin
  try
    Result:= '';
    s := sTelegramm;
    cAdr := GetDSfGInstAdr(s);
    FCOMObject.StoreTelegrams := True;
    try
      Sleep(100);
      FCOMObject.SendTelegramm(s);

      iStop := GetTickCount + DWord(FTimeOut);

      while ((pos(chr(stx), s) = 0) or
             ((pos(chr(stx), s) > 0) and
              ((GetDSfGAbsAdr(s) <> cAdr) or (GetDSfGNTY(s) = 'Z'))))
        and (GetTickCount < iStop)
      do begin
        s := FCOMObject.ReceiveTelegramm;
        Sleep(10);
        Application.ProcessMessages;
      end;

      // Prüfung auf Folgetelegramme (bei Blockung)
      pSl := TStringList.Create;
      try
        while (pos(chr(stx), s) > 0) and (GetDSfGAbsAdr(s) = cAdr) and
           (pos(chr(etx), s) = 0) do
        begin

          pSl.Add(s);
          iStop := GetTickCount + DWord(FTimeOut);

          while (s <> '') and ((pos(chr(stx), s) = 0) or
                 ((pos(chr(stx), s) > 0) and
                  ((GetDSfGAbsAdr(s) <> cAdr) or (GetDSfGNTY(s) = 'Z'))))
            and (GetTickCount < iStop)
          do begin
            s:= FCOMObject.ReceiveTelegramm;
            sleep(10);
            Application.ProcessMessages;
          end;

        end;
        FolgeTelegrammeSuchen(pSl);
        if (pSl.Count > 0) then s := pSl[0];
      finally
        pSl.Free;
      end;

      if (Pos(Chr(stx), s) = 0) or (GetDSfGAbsAdr(s) <> cAdr)
        then Result:= ''
        else Result := s;

    finally
      FCOMObject.StoreTelegrams := False;
    end;

    // Prüfung auf Folgetelegramme (bei 'U' und DEB = 'V', 'O', 'Z')  // 19.09.2000
        // Prüfung auf Folgetelegramme (bei 'U' und DEB = 'V', 'O', 'Z')
    if (bFolgetelegramme) and (
        (GetDSfGDEB(sTelegramm) = 'V') or (GetDSfGDEB(sTelegramm) = 'O')  or
        (GetDSfGDEB(sTelegramm) = 'Z'))
    then begin

      while (GetDSfGNTY(s) = 'U') do begin

        if (not Opened) then Exit;

    // Bei Bereichsabfragen
        if (GetDSfGDEB(sTelegramm) = 'V') then
          sTelegramm := ModifyDSfGParamTelegramm(sTelegramm, s)
      // Bei Ordnungsnummernabfragen
        else if (GetDSfGDEB(sTelegramm) = 'O') then
          sTelegramm := ModifyDSfGONrTelegramm(sTelegramm, s)
      // Bei Zeitbereichsabfragen
        else if (GetDSfGDEB(sTelegramm) = 'Z') then
          sTelegramm := ModifyDSfGZeitBereichsTelegramm(sTelegramm, s)
      // sonst abbrechen
        else sTelegramm := '';

        if (sTelegramm <> '') then begin
          s := SendReceiveTelegramm(sTelegramm, False);

          if (Length(s) > 8) then begin
      // Telegramme zusammensetzen
            // Datenteil bisheriges Ergebnis
            pSl := GetDatenListe(Result);
            try
              j := pSl.Count;
              Result := '';
              for i := 0 to pSl.Count-1 do Result := Result + pSl[i] + Chr(gs);
            finally
              pSl.Free;
            end;
            // Datenteil neu hinzugekommenes Ergebnis
            pSl := GetDatenListe(s);
            if (Assigned(pSl)) then
            try
              if (pSl.Count > 1) then begin
                j := j + pSl.Count;
                for i := 0 to pSl.Count-2 do Result := Result + pSl[i] + Chr(gs);
                Result := Result + pSl[pSl.Count-1] + Chr(fs) + Chr(etx);
              end;
            finally
              pSl.Free;
            end;
            // Deklarationsteil erstellen
            Result := IntToStr(j) + Chr(us) + Result;  // ZAE
            for i := 9 downto 1 do Result := GetStringPart(s, i) + Chr(us) + Result;
          end;

        end;

      end;

    end;
  finally
    if (C_RepeatCount = 0) and (Result = '') then begin
      Inc(C_RepeatCount);
      Result := SendReceiveTelegramm(sTelegramm, bFolgetelegramme);
    end
    else C_RepeatCount := 0;
  end;
end;

{ Gibt Telegramm mit DatenE. zurück  }
{ Parameter: Telegramm oder ''       }
{ Rückgabe: DSfG-Telegramm mit DEA   }
{------------------------------------}
function TCustomDSfGCardCOMObject.GetDatenElementAbfrage(sTelegramm: string): string;
{------------------------------------}
var
  s, sDeklarationsTeil, sDatenTeil : string;
  i, iDECount                      : integer;
  pSl                              : TStrings;
  c                                : char;

  // NOTBEHELF !!!
  { Datenelementliste der DSfGCard                  }
  { Parameter: Datenelementadresse                  }
  { Rückgabe: Wert zur DEA oder ''                  }
  {-------------------------------------------------}
  function CC_GetDEWert(sDEA: string): string;
  {-------------------------------------------------}
//  var
//    c : char;
//    s : string;
  begin
    Result := '';  // Default
    if (sDEA = 'aaa') then Result := 'M'       // Instanztyp
    else if (sDEA = 'aab') then Result := FCOMObject.Adresse  // Busadresse
//    else if (sDEA = 'aac') then begin          // Vs. der DSfG-Firmware
//      if (FW_getFWVersion(s)) then Result := s;
//    end
    else if (sDEA = 'aba') then Result := 'Karl Wieser GmbH'  // Hersteller
    else if (sDEA = 'abb') then Result := 'Wieser DSfGCard';  // Gerätetyp
  end;

begin
  Result := '';  // Vorbelegung

  if (sTelegramm = '') then
    sTelegramm := GetBereichsabfrageDSfGTelegramm('_', CardAddress, 'a', 'z');

  iDECount := 0;
  sDatenTeil := GetDSfGdatenTeil(sTelegramm);
  if (Length(sDatenTeil) > 0) then begin

    pSl := TStringList.Create;
    try
  // Deklarationsteil erstellen
      sDeklarationsTeil := Chr(stx) + GetDSfGAbsAdr(sTelegramm) + Chr(us)
                           + '255' + Chr(us)
                           + '1' + Chr(us)
                           + '1' + Chr(us)
                           + '1' + Chr(us)
                           + CardAddress + Chr(us)                { DNO }
                           + 'R' + Chr(us)                        { NTY }
                           + 'N' + Chr(us)                        { DFO }
                           + GetDSfGDEB(sTelegramm) + Chr(us);    { DEB }

  // Bereichsabfrage
      if (GetDSfGDEB(sTelegramm) = 'V') then begin
        iDECount := 5;
        pSl.Add('aaa' + Chr(us) + CC_GetDEWert('aaa'));
        pSl.Add('aab' + Chr(us) + CC_GetDEWert('aab'));
        pSl.Add('aac' + Chr(us) + CC_GetDEWert('aac'));
        pSl.Add('aba' + Chr(us) + CC_GetDEWert('aba'));
        pSl.Add('abb' + Chr(us) + CC_GetDEWert('abb'));
      end
  // Einzelabfrage
      else if (GetDSfGDEB(sTelegramm) = 'M') then begin
        i := Pos(Chr(gs), sDatenTeil);
        if (i = 0) then i := Pos(Chr(fs), sDatenTeil);
        while (i > 0) do begin
          Inc(iDECount);
          s := Copy(sDatenTeil, 1, i-1);
          if (Length(s) = 0) or (Length(s) > 5)
          then Exit
          else begin
            if (CC_GetDEWert(s) <> '')
              then pSl.Add(s + Chr(us) + CC_GetDEWert(s)) else pSl.Add(s);
          end;
          Delete(sDatenTeil, 1, Length(s)+1);
          i := Pos(Chr(gs), sDatenTeil);
          if (i = 0) then i := Pos(Chr(fs), sDatenTeil);
        end;
      end;

      sDeklarationsTeil := sDeklarationsTeil + IntToStr(iDECount) + Chr(us);

  // Datenteil erstellen
      sDatenTeil := '';
      if (pSl.Count = 0)
      then Exit
      else for i := 0 to pSl.Count-1 do begin
        if (i = pSl.Count-1) then c := Chr(fs) else c := Chr(gs);
        sDatenTeil := sDatenTeil + pSl[i] + c;
      end;
      sDatenTeil := sDatenTeil+ Chr(etx);

  // Wenn die Funktion hier ankommt, ist alles i.O.
      Result := sDeklarationsTeil + sDatenTeil;
    finally
      pSl.Free;
    end;
  end;
end;

{--------------------------- TDSfGCardCOMParamObject --------------------------}

{ Liest alle Parameter aus           }
{ Parameter: Instanzadresse          }
{ Rückgabe: Parameterstring          }
{------------------------------------}
function TDSfGCardCOMParamObject.GetAllParameters(cIAdr: char): string;
{------------------------------------}
var
  sDSfGCommand, sDSfGAnswer : string;
begin
  sDSfGCommand := GetBereichsabfrageDSfGTelegramm(CardAddress, cIAdr, 'a', 'z');
  sDSfGAnswer := SendReceiveTelegramm(sDSfGCommand);
  Result := sDSfGAnswer;
end;

{ Liest allgemeine Parameter aus     }
{ Parameter: Instanzadresse          }
{ Rückgabe: Parameterstring          }
{------------------------------------}
function TDSfGCardCOMParamObject.GetAllgParameters(cIAdr: char): string;
{------------------------------------}
var
  sDSfGCommand : string;
begin
  sDSfGCommand := GetBereichsabfrageDSfGTelegramm(
    CardAddress, cIAdr, 'a', 'b');
  Result := SendReceiveTelegramm(sDSfGCommand);
end;

{ Liest Parameter eines Bereichs aus }
{ Parameter: InstAdr, DEA von, bis   }
{ Rückgabe: Parameterstring          }
{------------------------------------}
function TDSfGCardCOMParamObject.GetBereichsParameters(
  cIAdr: char; sDEAVon, sDEABis: string): string;
{------------------------------------}
var
  sDSfGCommand : string;
begin
  sDSfGCommand := GetBereichsabfrageDSfGTelegramm(
    CardAddress, cIAdr, sDEAVon, sDEABis);
  Result := SendReceiveTelegramm(sDSfGCommand);
end;

{ Liest Parameter eines Bereichs aus }
{ Parameter: InstAdr, DEAs, Anzahl   }
{ Rückgabe: Parameterstring          }
{------------------------------------}
function TDSfGCardCOMParamObject.GetMultipleParameters(
  cIAdr: char; sDEAString: string; iZAE: integer): string;
{------------------------------------}
var
  sDSfGCommand : string;
begin
  sDSfGCommand := DSG_Utils.GetMultiDEDSfGTelegramm(
    CardAddress, cIAdr, sDEAString, iZAE);
  Result := SendReceiveTelegramm(sDSfGCommand);
end;

{ Liest instanzspezifische Param. aus}
{ Parameter: Instanzadresse, -typ    }
{ Rückgabe: Parameterstring          }
{------------------------------------}
function TDSfGCardCOMParamObject.GetInstParameters(cIAdr, cITyp: char): string;
{------------------------------------}
var
  sDSfGCommand : string;
  cInstDelVon  : string;
  cInstDelBis  : string;
begin
  case cITyp of
    'U' : begin
            cInstDelVon := 'b';
            cInstDelbis := 'big';
          end;
    'R' : begin
            cInstDelVon := 'c';
            cInstDelbis := 'z';
          end;
    'G' : begin
            cInstDelVon := 'd';
            cInstDelbis := 'z';
          end;
    'D' : begin
            cInstDelVon := 'e';
            cInstDelbis := 'z';
          end;
    'S' : begin
            cInstDelVon := 'f';
            cInstDelbis := 'z';
          end;
    'W' : begin
            cInstDelVon := 'w';
            cInstDelbis := 'z';
          end;
    else begin
      cInstDelVon := 'a';
      cInstDelbis := 'z';
    end;
  end;

  sDSfGCommand := GetBereichsabfrageDSfGTelegramm(
    CardAddress, cIAdr, cInstDelVon, cInstDelbis);
  Result := SendReceiveTelegramm(sDSfGCommand);
end;

{ Sendet ein Freeze-Telegramm ab     }
{ Holt während Totzeit Antworten     }
{ Rückgabe: Liste mit Antworten      }
{------------------------------------}
function TDSfGCardCOMParamObject.SendFreezeReceiveAnswers(cIAdr: char): TStrings;
{------------------------------------}
var
  s    : string;
  i    : DWord;
begin
  Result:= TStringList.Create;
  s := GetFreezeTelegramm(Self.CardAddress);
  FCOMObject.SendTelegramm(s);
  FCOMObject.RundsendeMode := True;
  FCOMObject.StoreTelegrams := True;
  try
    FCOMObject.SendTelegramm(s);
    FCOMObject.RundsendeMode := True;

    i := GetTickCount + DWord(FTimeOut);

    while (GetTickCount < i) do begin
      s := FCOMObject.ReceiveTelegramm;
      if (Pos(Chr(stx), s) > 0) and (Pos(Chr(etx), s) > 0) and
        (GetDSfGNTY(s) = 'I') then Result.Add(s);
      Sleep(10);
      Application.ProcessMessages;
    end;

  finally
    FCOMObject.StoreTelegrams := False;
    FCOMObject.RundsendeMode := False;
  end;
end;

{------------------------- TDSfGCardCOMMonitorObject --------------------------}

{ Schaltet den Monitor-Mode ein/aus  }
{ Parameter: Initialis./Freigeben    }
{ Rückgabe: Erfolg Ja/Nein           }
{------------------------------------}
function TDSfGCardCOMMonitorObject.InitMonitorMode(aState: boolean): boolean;
{------------------------------------}
begin
  FCOMObject.MonitorMode := aState;
  Result := (FCOMObject.MonitorMode = aState);
end;

{ Holt Monitordaten }
{ Parameter: Initialis./Freigeben    }
{ Rückgabe: Erfolg Ja/Nein           }
{------------------------------------}
function TDSfGCardCOMMonitorObject.ReceiveMonitorData: string;
{------------------------------------}
begin
  Result := FCOMObject.ReceiveMonitorData;
end;

end.
