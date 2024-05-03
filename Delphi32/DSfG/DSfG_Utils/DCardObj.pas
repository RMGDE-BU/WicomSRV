{------------------------------------------------------------------------------}
{ Objekt für allgemeine DSfGCard-Abfragen (TCustomDSfGCardObject)              }
{ Objekt für DSfGCard-Parameter-Abfragen (TDSfGCardParamObject)                }
{ Objekt für DSfGCard-Zugriff über COM (TCustomDSfGCardCOMObject)              }
{ Objekt für DSfGCard-Parameter-Zugriff über COM (TDSfGCardCOMParamObject)     }
{                                                                              }
{ 04.07.2000  GD    Neu                                                        }
{ 24.07.2000  GD    Erweiterung um Zugangscode2 für Parametrierung             }
{                   Erweiterung um Einstellung der Baudrate                    }
{                   Erweiterung um Zugriff auf eigene Busadresse               }
{                   Veröffentlichen von Sende-/Empfangsbefehlen                }
{ 17.08.2000  GD    Objekt für DSfGCard-Zugriff über COM                       }
{ 19.09.2000  GD    Handling unvollstädiger Antworten bei COM                  }
{ 31.10.2000  GD    Erweiterung um Bereichsabfrage                             }
{ 22.11.2000  GD    COM-Zugriffe in DCardCOM ausgegliedert                     }
{ 27.11.2000  GD    Neues Objekt für Monitormodus abgeleitet                   }
{ 14.12.2000  GD    Neues Objekt für Abrufe; Abr. v. InstParams geändert       }
{ 18.01.2001  GD    Neues Objekt: Parametrierung umfasst jetzt auch Abrufe     }
{ 30.03.2001  GD    Abbruchkriterium bei SendReceiveTelegramm                  }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, 2001                                    }
{------------------------------------------------------------------------------}
unit DCardObj;

interface

uses
  Windows, SysUtils, CC_Basis, CC_Call, CC_FW, CC_DSfG, GD_Utils, DSG_Utils,
  Classes, Dialogs, ComObj, Forms;

type
  TCustomDSfGCardObject = class(TObject)
    constructor Create(iBdRate: integer); virtual;
    destructor Destroy; override;
  private
    FBdRate : integer;
    FOpened : boolean;
    FTimeOut: integer;
    procedure InitDSfGCard(bState: boolean);
    procedure SetBaudRate(Value: integer);  // 24.07.2000
    function GetCardAdr: char;              // 08.08.2000
    procedure SetCardAdr(Value: char);      // 08.08.2000
  protected
  public
    function GetTeilnehmer: string;
    function CardIsTeilnehmer: boolean;
    function SendTelegramm(sTelegramm: string): boolean;        // 16.08.2000
    function ReceiveTelegramm: string;                          // 16.08.2000
    function SendReceiveTelegramm(sTelegramm: string): string;  // 16.08.2000
    function GetDatenElementAbfrage(sTelegramm: string): string;
    property BaudRate: integer read FBdRate write SetBaudRate;
    property CardAddress: char read GetCardAdr write SetCardAdr;
    property TimeOut: integer read FTimeOut write FTimeOut;
    property Opened: boolean read FOpened;
  end;

  TDSfGCardBusDatenObject = class(TCustomDSfGCardObject)
  private
  protected
  public
    function ReadParameter(cIAdr: char; sDEAdr: string): string;
    function GetAllParameters(cIAdr: char): string;  // a-z
    function GetInstParameters(cIAdr: char; cITyp: char = ' '): string; // Instanzspez. (b,c,d)
    function GetAllgParameters(cIAdr: char; cITyp: char = ' '): string; // a
    function GetBereichsParameters(cIAdr: char; sDEAVon, sDEABis: string): string;
  end;

  TDSfGCardAbfrageObject = class(TDSfGCardBusDatenObject)  // 14.12.2000
  private
  protected
  public
    function GetZeitbereichsDaten(
      cIAdr: char; sDEA: string; dtVon, dtBis: TDateTime): string;
    function GetOrdnungsNummerBereichsDaten(
      cIAdr: char; sDEA: string; iONrVon, iONrBis: integer): string;
  end;

  TDSfGCardParamObject = class(TDSfGCardAbfrageObject)  // 18.01.2001
  private
  protected
  public
    function WriteParameter(
      cIAdr: char; sZC1, sZC2, sDEAdr, sValue: string): string;
  end;

  TDSfGCardMonitorObject = class(TCustomDSfGCardObject)   // 27.11.2000
  private
  protected
  public
    function InitMonitorMode(aState: boolean): boolean;
    function ReceiveMonitorData: string;
  end;

implementation

{------------------------------- Allgemeine Funktionen ------------------------}

{---------------------------- TCustomDSfGCardObject ---------------------------}

{ Konstruktor                                                }
{------------------------------------------------------------}
constructor TCustomDSfGCardObject.Create(iBdRate: integer);
{------------------------------------------------------------}
begin
  inherited Create;
  FBdRate := iBdRate;
  FTimeOut := 30000;
  InitDSfGCard(True);
end;

{ Destruktor                                                 }
{------------------------------------------------------------}
destructor TCustomDSfGCardObject.Destroy;
{------------------------------------------------------------}
begin
  InitDSfGCard(False);
  inherited Destroy;
end;

{ Initialisiert die DSfG-Konversation zur Firmware der Karte }
{ Parameter: Initialisieren oder schließen                   }
{------------------------------------------------------------}
procedure TCustomDSfGCardObject.InitDSfGCard(bState: boolean);
{------------------------------------------------------------}
begin
  { Öffnen des PnP-Treibers }
  CC_SendTimeOut := not (bState);
  if (bState) then FOpened := (CC_Open(FBdRate))
    else FOpened := ((FOpened) and (not CC_Close));
end;

{ Stellt die Baudrate für die Karte ein                      }
{ Parameter: BaudRate                                        }
{------------------------------------------------------------}
procedure TCustomDSfGCardObject.SetBaudRate(Value: integer);
{------------------------------------------------------------}
begin
  if (Value <> FBdRate) then begin
    if (not CC_setBdRate(Value))
    then MessageDlg('Konnte Baudrate nicht ändern !', mtError, [mbOk], 0)
    else FBdRate := Value;
  end;
end;

{ Gibt die Instanzadresse für die Karte zurück               }
{ Rückgabe: Instanzadresse                                   }
{------------------------------------------------------------}
function TCustomDSfGCardObject.GetCardAdr: char;
{------------------------------------------------------------}
begin
  FW_getAdresse(Result);
end;

{ Stellt die Instanzadresse für die Karte ein                }
{ Parameter: Instanzadresse                                  }
{------------------------------------------------------------}
procedure TCustomDSfGCardObject.SetCardAdr(Value: char);
{------------------------------------------------------------}
begin
  if (Value in ['A'..'_']) and (Pos(Value, GetTeilnehmer) = 0) then begin
    if (not FW_setAdresse(Value))
    then MessageDlg('Konnte Busadresse nicht ändern !', mtError, [mbOk], 0);
  end;
end;

{ Liest die Telnehmerliste aus       }
{ Rückgabe: Teilnehmerliste          }
{------------------------------------}
function TCustomDSfGCardObject.GetTeilnehmer: string;
{------------------------------------}
begin
  if (not FW_getTeilnehmerListe(Result)) then Result := '';
end;

{ Ist die Karte in Teilnehmerliste ? }
{ Rückgabe: Ja/Nein                  }
{------------------------------------}
function TCustomDSfGCardObject.CardIsTeilnehmer: boolean;
{------------------------------------}
var
  s : string;
  c : char;
begin
  s := '';
  c := ' ';
  if (FOpened) then begin
    if (not FW_getTeilnehmerListe(s))
    then raise Exception.Create('Fehler beim Abruf der Teilnehmerliste');
    if (not FW_getAdresse(c))
    then raise Exception.Create('Fehler beim Abruf der Busadresse der DSfGCard');
  end;
  Result := (Pos(c, s) > 0);
end;

{ Versendet ein Telegramm            }
{ Parameter: DSfG-Telegramm          }
{------------------------------------}
function TCustomDSfGCardObject.SendTelegramm(sTelegramm: string): boolean;
{------------------------------------}
begin
  Result := CC_sendTelegramm(sTelegramm);
end;

{ Fragt ein Telegramm ab             }
{ Rückgabe: DSfG-Telegramm oder ''   }
{------------------------------------}
function TCustomDSfGCardObject.ReceiveTelegramm: string;
{------------------------------------}
begin
  Result := CC_receiveTelegramm;
  if (Pos(Chr(stx), Result) = 0) or (Pos(Chr(etx), Result) = 0) then Result := '';
end;

{ Fragt ein Telegramm ab             }
{ Parameter: Sendetelegramm          }
{ Rückgabe: DSfG-Telegramm oder ''   }
{------------------------------------}
function TCustomDSfGCardObject.SendReceiveTelegramm(sTelegramm: string): string;
{------------------------------------}
var
  s   : string;
  i   : integer;
  pSl : TStrings;
begin
  s := CC_SendAndReceiveTelegramm(sTelegramm, FTimeOut);
  if (Pos(Chr(stx), s) = 0) or (Pos(Chr(etx), s) = 0)
    then Result := ''
    else Result := s;

  // Prüfung auf Folgetelegramme (bei 'U' und DEB = 'V', 'O', 'Z')  // 19.09.2000
  if (GetDSfGDEB(sTelegramm) = 'V') or (GetDSfGDEB(sTelegramm) = 'O')  or
     (GetDSfGDEB(sTelegramm) = 'Z') then

    while (GetDSfGNTY(s) = 'U') do begin
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
      else sTelegramm := '';   // 30.03.2001

      if (sTelegramm <> '') then begin
        s := SendReceiveTelegramm(sTelegramm);

    // Telegramme zusammensetzen
        pSl := GetDatenListe(s);
        if (Assigned(pSl)) then
        try
          if (pSl.Count > 1) then begin
            Result := Copy(Result, 1, Pos(Chr(fs), Result)-1) + Chr(gs);
            for i := 1 to pSl.Count-2 do
              Result := Result + pSl[i] + Chr(gs);
            Result := Result + pSl[pSl.Count-1] + Chr(fs) + Chr(etx);
          end;
        finally
          pSl.Free;
        end;
      end;
    end;
end;

{ Gibt Telegramm mit DatenE. zurück  }
{ Parameter: Anforderungs-Telegramm  }
{ Rückgabe: DSfG-Telegramm           }
{------------------------------------}
function TCustomDSfGCardObject.GetDatenElementAbfrage(sTelegramm: string): string;
{------------------------------------}
var
  s, sDeklarationsTeil, sDatenTeil : string;
  i, iDECount                      : integer;
  pSl                              : TStrings;
  c                                : char;
begin
  Result := '';  // Vorbelegung
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

  // Datententeil erstellen
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

{-------------------------- TDSfGCardBusDatenObject ---------------------------}

{ Liest einen Parameter aus          }
{ Parameter: Instanzadresse,         }
{            Datenelementadresse     }
{ Rückgabe: Parameter                }
{------------------------------------}
function TDSfGCardBusDatenObject.ReadParameter(cIAdr: char; sDEAdr: string): string;
{------------------------------------}
var
  sDSfGCommand : string;
begin
  sDSfGCommand := GetOneDEDSfGTelegramm(CardAddress, cIAdr, sDEAdr);
  Result := CC_SendAndReceiveTelegramm(sDSfGCommand, TimeOut);
end;

{ Liest alle Parameter aus           }
{ Parameter: Instanzadresse          }
{ Rückgabe: Parameterstring          }
{------------------------------------}
function TDSfGCardBusDatenObject.GetAllParameters(cIAdr: char): string;
{------------------------------------}
var
  sDSfGCommand : string;
begin
  sDSfGCommand := GetBereichsabfrageDSfGTelegramm(CardAddress, cIAdr, 'a', 'z');
  Result := SendReceiveTelegramm(sDSfGCommand);
end;

{ Liest allgemeine Parameter aus     }
{ Parameter: Instanzadresse          }
{ Rückgabe: Parameterstring          }
{------------------------------------}
function TDSfGCardBusDatenObject.GetAllgParameters(
  cIAdr: char; cITyp: char = ' '): string;
{------------------------------------}
var
  sDSfGCommand : string;
  cInstDelBis  : string;
begin
  // Instanztyp ermitteln
  if (cITyp = ' ') then begin
    sDSfGCommand := ReadParameter(cIAdr, C_DEA_InstTyp);
    with GetDatenListe(sDSfGCommand) do
    try
      try
        if (Count = 1) then begin
          sDSfGCommand := GetStringPart(Strings[0], 2);
          if (sDSfGCommand <> '') then cITyp := sDSfGCommand[1];
        end;
      except       // Fehler, falls kein gültiges Rücktelegramm --> Abbrechen
        Result := '';
        Exit;
      end;
    finally
      Free;
    end;
  end;

  // Obere Datenelementgrenzen ermitteln
  case cITyp of
    'U' :   cInstDelbis := 'b';
    'R' :   cInstDelbis := 'c';
    'G' :   cInstDelbis := 'e';
    'D' :   cInstDelbis := 'e';
    'S' :   cInstDelbis := 'f';
    'W' :   cInstDelbis := 'w';
    else begin
      cInstDelbis := 'b';
    end;
  end;

  // a-Parameter abfragen
  sDSfGCommand := GetBereichsabfrageDSfGTelegramm(
    CardAddress, cIAdr, 'a', cInstDelbis);
  Result := SendReceiveTelegramm(sDSfGCommand);
end;

{ Liest Parameter eines Bereichs aus }
{ Parameter: InstAdr, DEA von, bis   }
{ Rückgabe: Parameterstring          }
{------------------------------------}
function TDSfGCardBusDatenObject.GetBereichsParameters(
  cIAdr: char; sDEAVon, sDEABis: string): string;
{------------------------------------}
var
  sDSfGCommand : string;
begin
  sDSfGCommand := GetBereichsabfrageDSfGTelegramm(
    CardAddress, cIAdr, sDEAVon, sDEABis);
  Result := SendReceiveTelegramm(sDSfGCommand);
end;

{ Liest instanzspezifische Param. aus}
{ Parameter: Instanzadresse, -typ    }
{ Rückgabe: Parameterstring          }
{------------------------------------}
function TDSfGCardBusDatenObject.GetInstParameters(
  cIAdr: char; cITyp: char = ' '): string;
{------------------------------------}
var
  sDSfGCommand : string;
  cInstDelVon  : string;
  cInstDelBis  : string;
begin
  // Instanztyp ermitteln
  if (cITyp = ' ') then begin
    sDSfGCommand := ReadParameter(cIAdr, C_DEA_InstTyp);
    with GetDatenListe(sDSfGCommand) do
    try
      try
        if (Count = 1) then begin
          sDSfGCommand := GetStringPart(Strings[0], 2);
          if (sDSfGCommand <> '') then cITyp := sDSfGCommand[1];
        end;
      except       // Fehler, falls kein gültiges Rücktelegramm --> Abbrechen
        Result := '';
        Exit;
      end;
    finally
      Free;
    end;
  end;

  // Datenelementgrenzen ermitteln
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

  // Parameter holen
  sDSfGCommand := GetBereichsabfrageDSfGTelegramm(
    CardAddress, cIAdr, cInstDelVon, cInstDelbis);
  Result := SendReceiveTelegramm(sDSfGCommand);
end;

{---------------------------- TDSfGCardParamObject ---------------------------}

{ Schreibt einen Parameter           }
{ Parameter: Instanzadresse,         }
{            Datenelementadresse,    }
{            Parameterwert,          }
{            Zugangscode2            }
{------------------------------------}
function TDSfGCardParamObject.WriteParameter(       // 24.07.2000
  cIAdr: char; sZC1, sZC2, sDEAdr, sValue: string): string;
{------------------------------------}
var
  sDSfGCommand : string;
begin
  sDSfGCommand :=
    GetEinstDSfGTelegramm(CardAddress, cIAdr, sZC1, sZC2, sDEAdr, sValue);
  Result := SendReceiveTelegramm(sDSfGCommand);
end;

{--------------------------- TDSfGCardAbfrageObject ---------------------------}

{ Schreibt einen Parameter           }
{ Parameter: Instanzadresse,         }
{            Datenelementadresse,    }
{            Parameterwert,          }
{            Zugangscode2            }
{------------------------------------}
function TDSfGCardAbfrageObject.GetZeitbereichsDaten(
  cIAdr: char; sDEA: string; dtVon, dtBis: TDateTime): string;
{------------------------------------}
var
  sDSfGCommand : string;
  sZVon, sZBis : string;
begin
  sZVon:= IntToHex(DateTimeToUnix(dtVon), 8);
  sZBis:= IntToHex(DateTimeToUnix(dtBis), 8);

  sDSfGCommand := GetDatenabfrageZeitbereichTelegramm(
    CardAddress, cIAdr, sDEA, sZVon, sZBis);
  Result := SendReceiveTelegramm(sDSfGCommand);
end;

{ Schreibt einen Parameter           }
{ Parameter: Instanzadresse,         }
{            Datenelementadresse,    }
{            Parameterwert,          }
{            Zugangscode2            }
{------------------------------------}
function TDSfGCardAbfrageObject.GetOrdnungsNummerBereichsDaten(
  cIAdr: char; sDEA: string; iONrVon, iONrBis: integer): string;
{------------------------------------}
var
  sDSfGCommand : string;
begin
  sDSfGCommand := GetDatenabfrageONrTelegramm(
    CardAddress, cIAdr, sDEA, iONrVon, iONrBis);
  Result := SendReceiveTelegramm(sDSfGCommand);
end;

{--------------------------- TDSfGCardMonitorObject ---------------------------}

{ Schaltet den Monitor-Mode ein/aus  }
{ Parameter: Initialis./Freigeben    }
{ Rückgabe: Erfolg Ja/Nein           }
{------------------------------------}
function TDSfGCardMonitorObject.InitMonitorMode(aState: boolean): boolean;
{------------------------------------}
begin
  Result := FW_setMonitorStatus(Integer(aState));
end;

{ Holt Monitordaten                  }
{ Rückgabe: Monitordaten             }
{------------------------------------}
function TDSfGCardMonitorObject.ReceiveMonitorData: string;
{------------------------------------}
begin
  if (not CC_ReceiveFromFW(Result)) then
    ShowMessage('Fehler beim Holen von MonitorDaten');
end;

end.
