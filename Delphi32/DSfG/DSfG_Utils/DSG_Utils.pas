{------------------------------------------------------------------------------}
{ Funktionen für DSfG-Abfragen                                                 }
{                                                                              }
{ 04.07.2000  GD    Neu                                                        }
{ 29.03.2001  GD    DSfG-Instanztypen in WSysCon                               }
{ 12.04.2002  GD    Analyse eines Telegramm-HDCLs                              }
{ 21.06.2002  GD    Resourcezugriffe für DSfGStationen hierher verschoben      }
{ 08.08.2002  GD    Typdefinitionen für DSfGStationen hierher verschoben       }
{ 18.02.2003  GD    In GetDatenliste Prüfung des HDCL verbessert               }
{ 02.10.2003  GD    Typdefinitionen für DSfGStationen erweitert                }
{ 06.10.2003  GD    Blockung bei GetDatenTeil                                  }
{ 29.03.2004  GD    Vorhergehendes STX bei GetDatenTeil ausgefiltert           }
{ 03.12.2004  GD    BCC auch über einzelne Blöcke bildbar                      }
{ 25.10.2006  GD    Prüfung für Typ 6,7 auf RMG ausgeweitet                    }
{ 15.11.2007  GD    Symbol für Blendenrechner                    	             }
{ 18.08.2008  WW    zusätzliche Telegramme                             	       }
{ 26.01.2010  GD    Auswahlbaum auf WICAL erweitert                            }
{ 20.09.2011  WN    BildeArchivgruppenElementeListe                            }
{                   BildeArchivkanalElementeListe                              }
{                   GetDatenElementListe (für Abruf emulierte DSfG-Station)    }
{                   DatenelementInBereich                                      }
{                   GetArchivAbrufInfosFromDEA                                 }
{                   DFÜ-DEA                                                    }
{ 14.11.2011  WN    Fkt. "GetEinstDSfGTelegramm" für mehrere Parameter         }
{ 26.01.2016  WW    mit Symbol für erweiterte DSfG-DFÜ und elektron. Gaszähler }
{ 24.08.2016  WW    GetGerTypNr für ERZ 2000 erweitert                         }
{ 05.03.2024  WW    mit Symbol für Odorierung und Gasbegleitstoffmessung       }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, RMG Messtechnik GmbH 2010, 2024         }
{------------------------------------------------------------------------------}
unit DSG_Utils;

interface

uses
  SysUtils, Classes, Contnrs,
  GD_Utils, WSysCon, ErrConst, WStrUtils;

const

// Datenelement-Adressen

  C_DEA_InstTyp        = 'aaa';
  C_DEA_InstAdr        = 'aab';
  C_DEA_DSfGFwVs       = 'aac';
  C_DEA_CRCStWert      = 'aad';

  C_DEA_Hersteller     = 'aba';
  C_DEA_GerTyp         = 'abb';
  C_DEA_FabrikNr       = 'abc';
  C_DEA_Baujahr        = 'abd';
  C_DEA_GerSwVs        = 'abe';
  C_DEA_Inbetriebnahme = 'abf';

  C_DEA_DatumZeit      = 'aca';
  C_DEA_Zeitzone       = 'acb';
  C_DEA_LetztVerstZz   = 'acc';

  C_DEA_Zugangscode1   = 'add';
  C_DEA_Zugangscode2   = 'ade';

  // 20.09.2011  WN
  // DFÜ-DEA für Kennung, DFÜ-Adr.
  C_DEA_DFUE_Id_Kennung = 'eaa';
  C_DEA_DFUE_Adresse_1  = 'ebaa';

// >>>> Anfang 21.06.2002 <<<<

  C_Res_DSfGStation_Aktiv             = 'DSfGStation_aktiv';
  C_Res_DSfGStation_Inaktiv           = 'DSfGStation_inaktiv';
  C_Res_DSfGStation_Umwerter          = 'DSfGStation_Umwerter';
  C_Res_DSfGStation_Registrierung     = 'DSfGStation_Registrierung';
  C_Res_DSFGSTATION_STEUERUNG         = 'DSFGSTATION_STEUERUNG';  // 19.03.2002
  C_Res_DSfGStation_Wieser            = 'DSfGStation_Wieser';
  C_Res_DSfGStation_DFU               = 'DSfGStation_DFU';
  C_Res_DSfGStation_Gasbeschaffenheit = 'DSfGStation_Gasbeschaffenheit';
  C_Res_DSfGStation_unbestimmt        = 'DSfGStation_unbestimmt';
  C_Res_DSfGStation_Ordner            = 'DSfGStation_Ordner';
  C_Res_DSfGStation_Parameter         = 'DSfGStation_Parameter';
  C_Res_DSfGStation_MRG2100           = 'DSfGStation_MRG2100';
  C_Res_DSfGStation_MRG2200           = 'DSfGStation_MRG2200';
  C_Res_DSfGStation_AnwOrdner         = 'DSfGStation_AnwOrdner';
  C_Res_DSfGStation_AnwParameter      = 'DSfGStation_AnwParameter';
  C_Res_MRGStation_Aktiv              = 'MRGStation_aktiv';
  C_Res_MRGStation_Inaktiv            = 'MRGStation_inaktiv';
  C_Res_DSfGStation_REVISION          = 'DSfGStation_REVISION';    // 02.10.2003
  C_Res_DSfGStation_KORRGASBESCH      = 'DSfGStation_KORRGASBESCH';// 02.10.2003
  C_Res_DSfGStation_Blende            = 'DSfGStation_BLENDE';      // 13.11.2003
  C_Res_Gruppe_Aktiv                  = 'Gruppe_aktiv';
  C_Res_Gruppe_Inaktiv                = 'Gruppe_inaktiv';
  C_Res_WicalMessstelle_Aktiv         = 'WicalMst_aktiv';
  C_Res_WicalMessstelle_Inaktiv       = 'WicalMst_inaktiv';
  C_Res_DSfGStation_DFU_erweitert     = 'DSfGStation_DFU_ERWEITERT';  // 26.01.2016, WW
  C_Res_DSfGStation_Gaszaehler        = 'DSfGStation_Gaszaehler';  // 04.02.2016, WW
  C_Res_DSfGStation_Odorierung        = 'DSfGStation_Odorierung';  // 05.03.2024, WW
  C_Res_DSfGStation_Begleit           = 'DSfGStation_Begleit';  // 05.03.2024, WW

  C_Level_AbrufTyp      = 0;
  C_Level_Stationen     = 1;
  C_Level_Instanzen     = 2;
  C_Level_Archive       = 3;
  C_Level_Kanaele       = 4;
  C_Level_Logbuecher    = 3;

  C_DEALevel_InstanzTyp = 0;
  C_DEALevel_OberOrdner = 1;

  C_ImageIndex_Aktiv    = 0;
  C_ImageIndex_Inaktiv  = 1;
  C_ImageIndex_Umwerter = 2;
  C_ImageIndex_Registrierung = 3;
  C_ImageIndex_Wieser   = 4;
  C_ImageIndex_DFU      = 5;
  C_ImageIndex_Gasbeschaffenheit = 6;
  C_ImageIndex_unbestimmt = 7;
  C_ImageIndex_Ordner = 8;
  C_ImageIndex_Parameter = 9;
  C_ImageIndex_MRG2100  = 10;
  C_ImageIndex_MRG2200  = 11;
  C_ImageIndex_AnwOrdner = 12;
  C_ImageIndex_AnwParameter = 13;
  C_ImageIndex_M_Aktiv  = 14;
  C_ImageIndex_M_Inaktiv = 15;
  C_ImageIndex_Steuerung = 16;  // 19.03.2002
  C_ImageIndex_Archive  = 8;
  C_ImageIndex_Logbuecher = 9;
  C_ImageIndex_Kanaele  = 9;
  C_ImageIndex_G_Aktiv  = 19;     // 16.07.2001
  C_ImageIndex_G_Inaktiv = 20;    // 16.07.2001
  C_ImageIndex_REVISION = 17;   // 02.10.2003
  C_ImageIndex_KORRGASBESCH = 18;   // 02.10.2003
  C_ImageIndex_Blendenrechner = 21;   // 13.11.2007
  C_ImageIndex_W_Aktiv  = 22;
  C_ImageIndex_W_Inaktiv = 23;
  C_ImageIndex_DFU_erweitert = 24;  // 26.01.2016, WW
  C_ImageIndex_Gaszaehler = 25;  // 04.02.2016, WW
  C_ImageIndex_Odor = 26;  // 05.03.2024, WW
  C_ImageIndex_Begleit = 27;  // 05.03.2024, WW

// >>>> Ende 21.06.2002 <<<<

// >>>> Anfang 20.09.2011  WN <<<<

  C_DEA_AGr_Offset = 96;   // a..y: Archivgruppe 1 ..25
  C_DEA_AKa_Offset = 101;  // f..y: Archivkanal  1 ..20
  // Array der unterstützten Datenelementadressen "allgemeine Beschreibung"
  C_Array_DEA_Allgemein: array [1..10] of String = (
     (C_DEA_InstTyp), (C_DEA_InstAdr), (C_DEA_DSfGFwVs),
     (C_DEA_Hersteller), (C_DEA_GerTyp), (C_DEA_FabrikNr), (C_DEA_Baujahr),
     (C_DEA_GerSwVs), (C_DEA_Inbetriebnahme), (C_DEA_DatumZeit));

// >>>> Ende 20.09.2011  WN <<<<

type
  TDSfGRootLevel = (drlDSfG, drlStation, drlInstanzen, drlDEA);
  TDSfGSichtLevel = (dslNone, dslStation, dslInstanzen, dslArchLogb,
    dslArchive, dslLogbuecher, dslKanaele, dslDEA, dslAll);  // 15.10.2001

  { Objekt für DSfG-Datenelementliste mit Telegramm-Rohdaten }
  TDSfGTelegrDEObj = class (TObject)
  public
    Daten: TDSfGRohRec;
    constructor Create (ADaten: TDSfGRohRec);
  end;

  { DSfG-Datenelementliste mit Telegramm-Rohdaten }
  TDSfGTelegrDEList = class (TObjectList)
  end;


{ Bildungs-Funktionen }
function GetDeaLogbuch(cQuellAdr: char): string;
function GetDeaArchivgruppe(iAgNr: byte): string;
function GetDeaArchivkanal(iAgNr, iAkNr: byte): string;

{ Parameter-Funktionen }
function GetDSfGDatenelement(sTelegramm: string; iIndex: integer): string;
//function GetDEABezeichnung(sDEA: string): string;
function GetDSfGAbsAdr(sTelegramm: string): char;
function GetDSfGInstAdr(sTelegramm: string): char;
function GetDSfGNTY(sTelegramm: string): char;
function GetDSfGDFO(sTelegramm: string): char;
function GetDSfGDEB(sTelegramm: string): char;
function GetDSfGDatenTeil(sTelegramm: string): string;
function GetDSfGDeclTeil(sTelegramm: string): string;

{ DSfG-Telegramme erstellen }
function GetBCC (Start: byte; s: string): byte;
function GetBCC_Chars (ABCC: byte): string;
{ Antworttelegramme }
function GetUnknownDSfGTelegramm(sTelegramm: string; cAdr: char): string;
function GetAntwortTelegramm_Einstellung(
  cAdr, cInst: char; sDEA_ZC1, sDEA_ZC2, sDEA_Einst, sWert: string): string;
function GetAntwortTelegramm_Anfrage(cAdr, cInst: char; cDEB_Anfrage: char;
  DSfGTelegrDEList: TDSfGTelegrDEList;
  bIgnoreEmptyArchiveData: boolean = false): string;
function GetAntwortTelegramm_AA_Vorgang_nicht_plausibel(
  cAdr, cInst: char): string;
function GetAntwortTelegramm_AA_Fehlende_Zugangsberechtigung(
  cAdr, cInst: char): string;
function GetAntwortTelegramm_AA_Vorgang_Teilnehmer_unbekannt(
  cAdr, cInst: char): string;
function GetAntwortTelegramm_AA_Vorgang_nicht_behandelt(
  cAdr, cInst: char): string;
{ Aufmerksamkeitstelegramme }
function GetFreezeTelegramm(cAdr: char): string;
function GetAttentionTelegramm_MesswertNeuGebildet(cAdr: char): string;
function GetAttentionTelegramm_EndeAbrechnungsperiode(cAdr: char): string;
function GetAttentionTelegramm_Alarm(cAdr: char): string;
function GetAttentionTelegramm_Warnung(cAdr: char): string;
function GetAttentionTelegramm_Hinweis(cAdr: char): string;
{ Einstelltelegramm }
function GetEinstDSfGTelegramm(
  cAdr, cInst: char; sZC1, sZC2, sDEA, sWert: string): string;  overload;
function GetEinstDSfGTelegramm(cAdrDFU, cAdrInst: Char; sZC1, sZC2: String;
                               p: TStrings): String;  overload;  // 14.11.2011  WN
{ Anfragetelegramme }
function GetOneDEDSfGTelegramm(cAdr, cInst: char; sDEL: string): string;
function GetMultiDEDSfGTelegramm(cAdr, cInst: char; sDEL: string; iZAE: integer): string;
function GetBereichsabfrageDSfGTelegramm(
  cAdr, cInst: char; sDEAVon, sDEABis: string): string;
function GetDatenabfrageONrTelegramm(
  cAdr, cInst: char; sDEA: string; iONrVon, iONrBis: integer): string;
function GetDatenabfrageZeitbereichTelegramm(
  cAdr, cInst: char; sDEA: string; sZeitVon, sZeitBis: string): string;
{ Folgetelegramme }
function ModifyDSfGParamTelegramm(sDSfGCommand, sDSfGAnswer: string): string;
function ModifyDSfGONrTelegramm(sDSfGCommand, sDSfGAnswer: string): string;
function ModifyDSfGZeitBereichsTelegramm(
  sDSfGCommand, sDSfGAnswer: string): string;

{ DSfG-Telegramme aufteilen }
function GetHDCL(sTelegramm: string): string;
function GetDatenTeil(sTelegramm: string): string;
function GetDatenListe(sTelegramm: string): TStrings;
function GetWertFromTelegramm(sTelegramm, sDEA: string): string; overload;
function GetWertFromTelegramm(sTelegramm: string; iIndex: integer): string; overload;

// DSfG-Telegramme prüfen
function CheckDSfGHeader(sTelegramm: string; var iZAE: integer): integer;

{ DSfG-Telegramme zusammenführen }
procedure FolgeTelegrammeSuchen(TelegrammList: TStrings);

{ Gerätetypnummer ermitteln }
function GetGerTypNr(sDEAHersteller, sDEAGeraeteTyp: string): byte;

// 20.09.2011  WN
procedure BildeArchivgruppenElementeListe(pSl: TStrings);
procedure BildeArchivkanalElementeListe(pSl: TStrings);
procedure GetDatenElementListe(sDeaVon, sDeaBis: String; pSl: TStrings);
function DatenelementInBereich(sDea, sDeaVon, sDeaBis: String): Boolean;
function GetArchivGruppeFromDEA(sDea: String): Integer;
function GetArchivKanalFromDEA(sDea: String): Integer;
function GetArchivAbrufInfosFromDEA(sDea: String; var iAGr: Integer;
                                    var iAKanal: Integer): Boolean;

implementation

uses TypInfo;

const
  C_AktTID: integer = 0;   // globale Datenaustausch-Referenz (TelegrammId)

{---------------------------------------------------------}
procedure IncAktTID;
{---------------------------------------------------------}
{ erhöht C_AktTID }
begin
  Inc(C_AktTID);
  if (C_AktTID > 9999) then C_AktTID := 1;
end;

{---------------------------------------------------------}
function GetBCC (Start: byte; s: string): byte;
{---------------------------------------------------------}
{ BCC berechnen/updaten;
  Übergabe: BCC-Startwert
            Daten-String
  Ergebnis: neues BCC }
var
  i: integer;
begin
  Result:=Start;
  for i:=1 to length(s) do begin
    if (s[i] <> #$02) then Result:=Result XOR Byte (s[i]);
    if (s[i] in [#$03, #$17]) then Break;   // 03.11.2004
  end;
end;

{-----------------------------------------------------}
function GetBCC_Chars (ABCC: byte): string;
{-----------------------------------------------------}
{ die beiden BCC-Zeichen aus BCC bilden (siehe "Technische Spezifikation für
  DSfG-Realisierungen, Punkt 3.2.4");
  Ergebnis: BCC-Zeichen als String }
const
  CNibbleAdd = $20;
var
  LoNibble: byte;
  HiNibble: byte;
begin
  LoNibble:=ABCC AND $0F;                            { die vier niederen Bits }
  HiNibble:=(ABCC AND $F0) SHR 4;       { die vier oberen Bits rechtsschieben }
  LoNibble:=LoNibble + CNibbleAdd;
  HiNibble:=HiNibble + CNibbleAdd;
  Result:=char(HiNibble) + char (LoNibble);
end;

(*
{ Gibt Datenelementbezeichn. zurück  }
{ Parameter: Datenelement-Adresse    }
{ Rückgabe: Datenelementbezeichnung  }
{------------------------------------}
function GetDEABezeichnung(sDEA: string): string;
{------------------------------------}
begin
  Result := GetDsfgDeaBezeichnung(sDEA);
end;
*)

{ Gibt 4-stellige Grund-DEA für Logbuch zurück           }
{ Parameter: Quelladresse des Logbuchs                   }
{ Rückgabe: Datenelementadresse oder ''                  }
{--------------------------------------------------------}
function GetDeaLogbuch(cQuellAdr: char): string;
{--------------------------------------------------------}
begin
  Result := 'cb';
  Result := Result + Chr(Ord('a') + ((Ord(cQuellAdr) - Ord('A')) div 10));
  Result := Result + Chr(Ord('a') + ((Ord(cQuellAdr) - Ord('A')) mod 10));
end;

{ Gibt 3-stellige Grund-DEA für ArchivGruppe zurück      }
{ Parameter: Archivgruppe                                }
{ Rückgabe: Datenelementadresse oder ''                  }
{--------------------------------------------------------}
function GetDeaArchivgruppe(iAgNr: byte): string;
{--------------------------------------------------------}
begin
  Result := 'ca' + Chr(Ord('a') + iAgNr - 1);
end;

{ Gibt 4-stellige Grund-DEA für Archivkanal zurück       }
{ Parameter: Archivgruppe, -kanal                        }
{ Rückgabe: Datenelementadresse oder ''                  }
{--------------------------------------------------------}
function GetDeaArchivkanal(iAgNr, iAkNr: byte): string;
{--------------------------------------------------------}
begin
  Result := GetDeaArchivgruppe(iAgNr) + Chr(Ord('f') + iAkNr - 1);
end;

{ Gibt aus einem Telegramm das iIndex-te Element des     }
{ Deklarationsteils zurück                               }
{ Parameter: Telegramm, Index des Datenelementes (ab 0)  }
{ Rückgabe: Datenelement oder ''                         }
{--------------------------------------------------------}
function GetDSfGDatenelement(sTelegramm: string; iIndex: integer): string;
{--------------------------------------------------------}
var
  i, j : integer;
  s    : string;
begin
  Result := '';

  i := Pos(Chr(stx), sTelegramm);
  if (i > 0) then begin
    j := 0;
    while (i < Length(sTelegramm)) and (j < iIndex) do begin
       Inc(i);
       if (sTelegramm[i] = chr(us)) then begin
         Inc(j);
       end;
    end;

    if (i < Length(sTelegramm)) then begin
      s := Copy(sTelegramm, i+1, Length(sTelegramm)-i);
      i := Pos(Chr(us), s);
      if (i > 0) then Result := Copy(s, 1, i-1);
    end;
  end;
end;

{ Gibt aus einem Telegramm die Absenderadresse zurück    }
{ Parameter: Telegramm                                   }
{ Rückgabe: Adresse oder ' '                             }
{--------------------------------------------------------}
function GetDSfGAbsAdr(sTelegramm: string): char;
{--------------------------------------------------------}
var
  s    : string;
begin
  s := GetDSfGDatenelement(sTelegramm, 5);
  if (Length(s) = 1) and (s[1] in ['0','A'..'_']) then Result := s[1]
    else Result := ' ';
end;

{ Gibt aus einem Telegramm die Empfängeradresse zurück   }
{ Parameter: Telegramm                                   }
{ Rückgabe: Adresse oder ' '                             }
{--------------------------------------------------------}
function GetDSfGInstAdr(sTelegramm: string): char;
{--------------------------------------------------------}
var
  s : string;
begin
  s := GetDSfGDatenelement(sTelegramm, 0);
  if (Length(s) = 1) and (s[1] in ['0', 'A'..'_']) then Result := s[1]
    else Result := ' ';
end;

{ Gibt aus einem Telegramm die Telegrammart (NTY) zurück }
{ Parameter: Telegramm                                   }
{ Rückgabe: Adresse oder ' '                             }
{--------------------------------------------------------}
function GetDSfGNTY(sTelegramm: string): char;
{--------------------------------------------------------}
var
  s : string;
begin
  s := GetDSfGDatenelement(sTelegramm, 6);
  if (Length(s) = 1) and (s[1] in ['A'..'Z']) then Result := s[1]
    else Result := ' ';
end;

{ Gibt zurück, ob das Telegramm eine Antwort erwartet    }
{ Parameter: Telegramm                                   }
{ Rückgabe: 'J'/'N'                                      }
{--------------------------------------------------------}
function GetDSfGDFO(sTelegramm: string): char;
{--------------------------------------------------------}
var
  s : string;
begin
  s := GetDSfGDatenelement(sTelegramm, 7);
  if (Length(s) = 1) and (s[1] in ['J','N']) then Result := s[1]
    else Result := 'N';
end;

{ Gibt die TelegrammBedeutung (DEB) zurück               }
{ Parameter: Telegramm                                   }
{ Rückgabe: DEB oder ' '                                 }
{--------------------------------------------------------}
function GetDSfGDEB(sTelegramm: string): char;
{--------------------------------------------------------}
var
  s : string;
begin
  s := GetDSfGDatenelement(sTelegramm, 8);
  if (Length(s) = 1) and (s[1] in ['A'..'Z']) then Result := s[1]
    else Result := ' ';
end;

{ Gibt den Datenteil eines Telegramms zurück (DID=255)   }
{ Parameter: Telegramm                                   }
{ Rückgabe: Datenteil des Telegramms incl. <fs>          }
{--------------------------------------------------------}
function GetDSfGDatenTeil(sTelegramm: string): string;
{--------------------------------------------------------}
var
  i, iDID : integer;
  s       : string;
begin
  Result := ''; // Default
  s := sTelegramm;

  // Prüfung auf DID = 255
  if (StrToIntDef(GetDSfGDatenelement(s, 1), 0) <> 255) then Exit;

  // bei DID = 255 sind die ersten 10 Positionen vorgegeben -> löschen
  i := 0;
  iDID := StrToIntDef(GetStringPart(s, 2), 255);
  while (Pos(Chr(us), s) > 0) and (i < 10) do begin
    Inc(i);
    Delete(s, 1, Pos(Chr(us), s));
  end;
  if (i < 10) then Exit;

  // Test, ob im HDCL noch weitere Felder vorhanden sind (keine Auswertung):
  if (iDID AND C_PAS) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { PAS }
  if (iDID AND C_DTY) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { DTY }
  if (iDID AND C_ABS) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { ABS }
  if (iDID AND C_EMF) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { EMF }
  if (iDID AND C_TDA) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { TDA }
  if (iDID AND C_TTI) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { TTI }
  if (iDID AND C_PTB) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { PTB }

  // Telegrammende löschen
  i := Pos(Chr(fs), s);
  if (i > 0) then Delete(s, i+1, Length(s)-i) else Exit;

  // Wenn wir hier ankommen, ist alles i.O.
  Result := s;
end;

{ Gibt den Dekl.-teil eines Telegramms zurück (DID=255)  }
{ Parameter: Telegramm                                   }
{ Rückgabe: Deklarationsteil des Telegramms              }
{--------------------------------------------------------}
function GetDSfGDeclTeil(sTelegramm: string): string;
{--------------------------------------------------------}
var
  i, iDID : integer;
begin
  Result := ''; // Default

  // DID speichern
  iDID := StrToIntDef(GetStringPart(sTelegramm, 2), 255);

  // bei DID = 255 sind die ersten 10 Positionen vorgegeben
  for i := 1 to 10 do Result := Result + GetStringPart(sTelegramm, i) + Chr(us);
  i := 10;

  // Test, ob im HDCL noch weitere Felder vorhanden sind
  if (iDID AND C_PAS) <> 0 then begin { PAS }
    Inc(i);
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);
  end;
  if (iDID AND C_DTY) <> 0 then begin    { DTY }
    Inc(i);
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);
  end;    { PAS }
  if (iDID AND C_ABS) <> 0 then begin    { ABS }
    Inc(i);
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);
  end;    { PAS }
  if (iDID AND C_EMF) <> 0 then begin    { EMF }
    Inc(i);
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);
  end;    { PAS }
  if (iDID AND C_TDA) <> 0 then begin    { TDA }
    Inc(i);
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);
  end;    { PAS }
  if (iDID AND C_TTI) <> 0 then begin    { TTI }
    Inc(i);
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);
  end;    { PAS }
  if (iDID AND C_PTB) <> 0 then begin    { PTB }
    Inc(i);
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);
  end;    { PAS }

end;

{ Gültigkeit des DSfG-Headers für Archive  }
{ Parameter: Telegeramm, Anzahl der Daten  }
{ Rückgabe: Felhercode (0=OK)              }
{------------------------------------------}
function CheckDSfGHeader(sTelegramm: string; var iZAE: integer): integer;
{------------------------------------------}

  function NextDeclPart(var s: string): string;
  begin
    Result := GetStringPart(s, 1);
    System.Delete(s, 1, Length(Result) + 1);
  end;

var
  iCode    : integer;
  s, sTeil : string;
  iDID     : integer;
begin
  s := sTelegramm;
  sTeil := NextDeclPart(s);     { alles bis zum ersten US (keine Auswertung) }

  { HDCL auswerten: }
  sTeil := NextDeclPart(s);                                             { DID }
  val(sTeil, iDID, iCode);
  if (iCode <> 0) then begin
    Result := DSFGKONVERR_HEADER;
    exit;
  end;
  if (iDID < C_DID_DataSimple) then begin
    Result := DSFGKONVERR_HEADER;
    exit;
  end;

  sTeil := NextDeclPart(s);                                            { TID }
  sTeil := NextDeclPart(s);                                            { BLO }
  sTeil := NextDeclPart(s);                                            { BNO }
  sTeil := NextDeclPart(s);                                            { DNO }
  sTeil := NextDeclPart(s);                                            { NTY }
  if (sTeil[1] <> 'R') AND (sTeil[1] <> 'U') then begin
    Result:=DSFGKONVERR_HEADER;
    exit;
  end;
  sTeil := NextDeclPart(s);                                            { DFO }
  sTeil := NextDeclPart(s);                                            { DEB }
  if (sTeil[1] <> 'O') AND (sTeil[1] <> 'Z') then begin
    Result:=DSFGKONVERR_HEADER;
    exit;
  end;
  sTeil := NextDeclPart(s);                                            { ZAE }
  val(sTeil, iZAE, iCode);                       { Anzahl der Datenelemente im File }
  if (iCode <> 0) then begin
    Result := DSFGKONVERR_HEADER;
    exit;
  end;
  if (iZAE = 0) then begin
    Result := DSFGKONVERR_NODATA;
    exit;
  end
  else if (iZAE < 0) then begin
    Result := DSFGKONVERR_HEADER;
    exit;
  end;

  { Test, ob im HDCL noch weitere Felder vorhanden sind (keine Auswertung): }
  if (iDID AND C_PAS) <> 0 then
    sTeil := NextDeclPart(s);                                          { PAS }
  if (iDID AND C_DTY) <> 0 then
    sTeil := NextDeclPart(s);                                          { DTY }
  if (iDID AND C_ABS) <> 0 then
    sTeil := NextDeclPart(s);                                          { ABS }
  if (iDID AND C_EMF) <> 0 then
    sTeil := NextDeclPart(s);                                          { EMF }
  if (iDID AND C_TDA) <> 0 then
    sTeil := NextDeclPart(s);                                          { TDA }
  if (iDID AND C_TTI) <> 0 then
    sTeil := NextDeclPart(s);                                          { TTI }
  if (iDID AND C_PTB) <> 0 then
    sTeil := NextDeclPart(s);                                          { PTB }

  Result := DSFGKONVERR_OK;
end;

{ Gibt DSfG-Antworttelegramm 'unbekannt'   }
{ zurück                                   }
{ Parameter: Anforderungstelegramm, IAdr   }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetUnknownDSfGTelegramm(sTelegramm: string; cAdr: char): string;
{------------------------------------------}
begin
  IncAktTID;

  Result := Chr(stx) + GetDSfGAbsAdr(sTelegramm) + Chr(us)
                   + '255' + Chr(us)
                   + IntToStr(C_AktTID) + Chr(us)      { TID }
                   + '1' + Chr(us)
                   + '1' + Chr(us)
                   + cAdr + Chr(us)  { DNO }
                   + 'R' + Chr(us)                        { NTY }
                   + 'N' + Chr(us)                        { DFO }
                   + GetDSfGDEB(sTelegramm) + Chr(us)     { DEB }
                   + '1' + Chr(us)
                   + '?' + Chr(fs) + Chr(etx);
end;

{ Gibt DSfG-Antworttelegramm auf Einstell- }
{ telegramm zurück                         }
{ Parameter: CardAdr, InstAdr, DEAdr Zu-   }
{ gangscode 1 und 2, Adresse und Wert des  }
{ einzustellenden Datenelements            }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetAntwortTelegramm_Einstellung(
  cAdr, cInst: char; sDEA_ZC1, sDEA_ZC2, sDEA_Einst, sWert: string): string;
{------------------------------------------}
begin
  IncAktTID;

  Result := Chr(stx) + cInst + Chr(us)
                   + IntToStr(255) + Chr(us)           { DID }
                   + IntToStr(C_AktTID) + Chr(us)      { TID }
                   + '1' + Chr(us)                     { BLO }
                   + '1' + Chr(us)                     { BNR }
                   + cAdr + Chr(us)                    { DNO }
                   + 'R' + Chr(us)                     { NTY }
                   + 'N' + Chr(us)                     { DFO }
                   + 'M' + chr(us)                     { DEB }
                   + IntToStr(3) + Chr(us)             { ZAE }
                   { Datenteil }
                   + sDEA_ZC1 + Chr(gs)                { DEA Zugangscode 1 }
                   + sDEA_ZC2 + Chr(gs)                { DEA Zugangscode 2 }
                   + sDEA_Einst + Chr(us)              { DE-Adresse }
                   + sWert + Chr(fs) + Chr(etx);       { Akt. Wert und Abschluß }
end;


{ Gibt DSfG-Antworttelegramm auf Anfrage-  }
{ telegramm zurück                         }
{ Parameter: CardAdr, InstAdr; DEB aus     }
{            Anfrage, DE-Liste             }
{            Flag 'Leere Archivdaten-Records in DE-Liste igorieren' (optional) }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetAntwortTelegramm_Anfrage(
  cAdr, cInst: char; cDEB_Anfrage: char;
  DSfGTelegrDEList: TDSfGTelegrDEList;
  bIgnoreEmptyArchiveData: boolean = false): string;
{------------------------------------------}
const
  CMaxTelegrLen = 7500;
  { Die in WSysCon.pas definierte, aufgrund von Geräteproblemen deutlich kleiner
    gewählte max. DSfG-Telegrammlänge muß nicht verwendet werden. Funktion wird
    bislang nur zusammen mit PGC-Simulationsprogramm und aktuellem DLA verwendet.
    Der aktuelle DLA verträgt die max. mögliche Telegrammlänge. 19.09.2008, WW }

var
  cDEB: char;
  cNTY: char;
  iZAE: integer;
  sDaten: string;
  i: integer;
  S: string;
  bAdd: boolean;

begin
  IncAktTID;

  { Datenteil zusammensetzen: }
  sDaten:='';
  iZAE:=0;
  cNTY:='R';
  if Assigned (DSfGTelegrDEList) then begin
    for i:=0 to DSfGTelegrDEList.Count - 1 do begin
      { max. Telegrammlänge erreicht ? }
      if length (sDaten) >= CMaxTelegrLen then begin
        cNTY:='U';  // Antwort unvollständig
        Break;
      end;

      bAdd:=true;  // Default: DE-Listenrecord in Telegramm einfügen
      if (cDEB_Anfrage = 'O') OR (cDEB_Anfrage = 'Z') then begin  { Anfrage über Ordnungsnummer oder Zeit }
        if bIgnoreEmptyArchiveData then begin
          if (length (TDSfGTelegrDEObj (DSfGTelegrDEList[i]).Daten.Wert) = 0) OR  { Wert leer oder... }
             (StrToIntDef (TDSfGTelegrDEObj (DSfGTelegrDEList[i]).Daten.Status, 0) < 0) then       { Status -1: fehlend }
            bAdd:=false;  // 01.12.2011, WW
        end;
      end;

      if bAdd then begin
        inc (iZAE);
        if iZAE = 1 then
          sDaten:=sDaten + Chr(us)
        else
          sDaten:=sDaten + Chr(gs);
        sDaten:=sDaten
                + TDSfGTelegrDEObj (DSfGTelegrDEList[i]).Daten.Adresse;  { DE-Adresse }

        if (cDEB_Anfrage = 'O') OR (cDEB_Anfrage = 'Z') then begin  { Anfrage über Ordnungsnummer oder Zeit }
          sDaten:=sDaten + Chr(us)
                  + TDSfGTelegrDEObj (DSfGTelegrDEList[i]).Daten.Wert + Chr(us)   { Wert }
                  + TDSfGTelegrDEObj (DSfGTelegrDEList[i]).Daten.UTime + Chr(us)  { Zeitstempel }
                  + TDSfGTelegrDEObj (DSfGTelegrDEList[i]).Daten.OrdNr + Chr(us)  { Ordnungsnummer }
                  + TDSfGTelegrDEObj (DSfGTelegrDEList[i]).Daten.Status;          { Status }

          S:=TDSfGTelegrDEObj (DSfGTelegrDEList[i]).Daten.CRC;  { CRC }
          if length (S) > 0 then  { nur, wenn vorhanden }
            sDaten:=sDaten + Chr(us) + S;
        end
        else begin  { Anfrage einer Menge von Datenelementen }
          S:=TDSfGTelegrDEObj (DSfGTelegrDEList[i]).Daten.Wert;  { Wert }
          if length (S) > 0 then  { nur, wenn vorhanden }
            sDaten:=sDaten + Chr(us) + S;
        end;
      end;  { if bAdd }
    end;  { for i }
  end; { if Assigned (DSfGTelegrDEList) }

  if (cDEB_Anfrage = 'O') OR (cDEB_Anfrage = 'Z') then  { Anfrage über Ordnungsnummer oder Zeit }
    cDEB:=cDEB_Anfrage
  else
    cDEB:='M';

  Result := Chr(stx) + cInst + Chr(us)
                   + IntToStr(255) + Chr(us)           { DID }
                   + IntToStr(C_AktTID) + Chr(us)      { TID }
                   + '1' + Chr(us)                     { BLO }
                   + '1' + Chr(us)                     { BNR }
                   + cAdr + Chr(us)                    { DNO }
                   + cNTY + Chr(us)                    { NTY }
                   + 'N' + Chr(us)                     { DFO }
                   + cDEB + chr(us)                    { DEB }
                   + IntToStr(iZAE)                    { ZAE }
                   + sDaten                            { Datenteil }
                   + Chr(fs) + Chr(etx);               { Abschluß }
end;

{ Gibt DSfG-Telegramm 'Außerplanmäßige     }
{ Antwort' zurück                          }
{ Parameter: CardAdr, InstAdr; Kurzzeichen }
{            der außerplanmäßigen Antwort  }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetAntwortTelegramm_Ausserplanmaessig(cAdr, cInst, cAA: char): string;
{------------------------------------------}
begin
  IncAktTID;

  Result := Chr(stx) + cInst + Chr(us)
                   + '255' + Chr(us)                   { DID }
                   + IntToStr(C_AktTID) + Chr(us)      { TID }
                   + '1' + Chr(us)                     { BLO }
                   + '1' + Chr(us)                     { BNR }
                   + cAdr + Chr(us)                    { DNO }
                   + 'R' + Chr(us)                     { NTY }
                   + 'N' + Chr(us)                     { DFO }
                   + 'A' + Chr(us)                     { DEB }
                   + '1' + Chr(us)                     { ZAE }
                   + cAA                               { Datenteil }
                   + Chr(fs) + Chr(etx);               { Abschluß }
end;

{ Gibt DSfG-Telegramm 'Außerplanmäßige Ant-}
{ wort: Vorgang nicht plausibel' zurück    }
{ Parameter: CardAdr, InstAdr              }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetAntwortTelegramm_AA_Vorgang_nicht_plausibel(
  cAdr, cInst: char): string;
{------------------------------------------}
begin
  Result:=GetAntwortTelegramm_Ausserplanmaessig(cAdr, cInst, '?');
end;

{ Gibt DSfG-Telegramm 'Außerplanmäßige Ant-}
{ wort: Fehlende Zugangsberecht.' zurück   }
{ Parameter: CardAdr, InstAdr              }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetAntwortTelegramm_AA_Fehlende_Zugangsberechtigung(
  cAdr, cInst: char): string;
{------------------------------------------}
begin
  Result:=GetAntwortTelegramm_Ausserplanmaessig(cAdr, cInst, '!');
end;

{ Gibt DSfG-Telegramm 'Außerplanmäßige Ant-}
{ wort: Vorgang bzw. Teilnehmer unbekannt' }
{ zurück                                   }
{ Parameter: CardAdr, InstAdr              }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetAntwortTelegramm_AA_Vorgang_Teilnehmer_unbekannt(
  cAdr, cInst: char): string;
{------------------------------------------}
begin
  Result:=GetAntwortTelegramm_Ausserplanmaessig(cAdr, cInst, '#');
end;

{ Gibt DSfG-Telegramm 'Außerplanmäßige Ant-}
{ wort: Vorgang kann z.Z. nicht behandelt  }
{ werden' zurück                           }
{ Parameter: CardAdr, InstAdr              }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetAntwortTelegramm_AA_Vorgang_nicht_behandelt(
  cAdr, cInst: char): string;
{------------------------------------------}
begin
  Result:=GetAntwortTelegramm_Ausserplanmaessig(cAdr, cInst, ':');
end;

{ Gibt ein DSfG-Aufmerksamkeitstelegramm   }
{ zurück                                   }
{ Parameter: CardAdr, NTY                  }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetAttentionTelegramm(cAdr, cNTY: char): string;
{------------------------------------------}
var
  dtNow: TDateTime;
begin
  IncAktTID;

  dtNow:=Now;
  Result := Chr(stx) + '@' + Chr(us)
                   + '12543' + Chr(us)
                   + IntToStr(C_AktTID) + Chr(us)      { TID }
                   + '1' + Chr(us)
                   + '1' + Chr(us)
                   + cAdr + Chr(us)   { DNO }
                   + cNTY + Chr(us)   { NTY }
                   + 'N' + Chr(us)    { DFO }
                   + 'M' + Chr(us)    { DEB }
                   + '0' + Chr(us)    { ZAE }
                   + FormatDateTime('yymmdd', dtNow) + Chr(us)    { TTA }
                   + FormatDateTime('hhnnss ', dtNow)    { TTI }
                   + Chr(fs) + Chr(etx);
end;

{ Gibt ein DSfG-Freeze-Telegramm zurück    }
{ Parameter: CardAdr                       }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetFreezeTelegramm(cAdr: char): string;
{------------------------------------------}
begin
  Result:=GetAttentionTelegramm(cAdr, 'F');
end;

{ Gibt ein DSfG-Aufmerksamkeitstelegramm   }
{ 'Messwert neu gebildet' zurück           }
{ Parameter: CardAdr                       }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetAttentionTelegramm_MesswertNeuGebildet(cAdr: char): string;
{------------------------------------------}
begin
  Result:=GetAttentionTelegramm(cAdr, 'M');
end;

{ Gibt ein DSfG-Aufmerksamkeitstelegramm 'Ende einer Abrechnungsperiode' zurück }
{ Parameter: CardAdr                       }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetAttentionTelegramm_EndeAbrechnungsperiode(cAdr: char): string;
{------------------------------------------}
begin
  Result:=GetAttentionTelegramm(cAdr, 'I');
end;

{ Gibt ein DSfG-Aufmerksamkeitstelegramm   }
{ 'Alarm' zurück                           }
{ Parameter: CardAdr                       }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetAttentionTelegramm_Alarm(cAdr: char): string;
{------------------------------------------}
begin
  Result:=GetAttentionTelegramm(cAdr, 'L');
end;

{ Gibt ein DSfG-Aufmerksamkeitstelegramm   }
{ 'Warnung' zurück                         }
{ Parameter: CardAdr                       }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetAttentionTelegramm_Warnung(cAdr: char): string;
{------------------------------------------}
begin
  Result:=GetAttentionTelegramm(cAdr, 'W');
end;

{ Gibt ein DSfG-Aufmerksamkeitstelegramm   }
{ 'Hinweis' zurück                         }
{ Parameter: CardAdr                       }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetAttentionTelegramm_Hinweis(cAdr: char): string;
{------------------------------------------}
begin
  Result:=GetAttentionTelegramm(cAdr, 'H');
end;

{ Gibt ein DSfG-Telegr. für Einst.  zurück }
{ Parameter: CardAdr, InstAdr; Zugangscodes}
{            Datenel., neuer Wert          }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetEinstDSfGTelegramm(
  cAdr, cInst: char; sZC1, sZC2, sDEA, sWert: string): string;
{------------------------------------------}
var
  sZ1, sZ2 : string;
begin
  if (sZC1 = '')
    then sZ1 := C_DEA_Zugangscode1 + Chr(gs)
    else sZ1 := C_DEA_Zugangscode1 + Chr(us) + sZC1 + Chr(gs);
  if (sZC2 = '')
    then sZ2 := C_DEA_Zugangscode2 + Chr(gs)
    else sZ2 := C_DEA_Zugangscode2 + Chr(us) + sZC2 + Chr(gs);

  IncAktTID;

  Result := Chr(stx) + cInst + Chr(us)
                   + IntToStr(255) + Chr(us)           { DID }
                   + IntToStr(C_AktTID) + Chr(us)      { TID }
                   + '1' + Chr(us)                     { BLO }
                   + '1' + Chr(us)                     { BNR }
                   + cAdr + Chr(us)                    { DNO }
                   + 'E' + Chr(us)                     { NTY }
                   + 'J' + Chr(us)                     { DFO }
                   + 'M' + chr(us)                     { DEB }
                   + IntToStr(3) + Chr(us)             { ZAE }
                   { Datenteil }
                   + sZ1                               { Zugangscode 1 }
                   + sZ2                               { Zugangscode 2 }
                   + sDEA + Chr(us)                    { DE-Adresse }
                   + sWert + Chr(fs) + Chr(etx);       { Neuer Wert und Abschluß }
end;

// 14.11.2011  WN
// -----------------------------------------------------------------------------
// gibt ein DSfG-Einstell-Telegramm mit mehreren Parametern zurück
//  ... add<us>9999<gs>ade<us>9999<gs>par1<us>wert1...<gs>parN<us>wertN
// Parameter: CardAdr, InstAdr, Zugangscodes
//            String-Liste mit zu parametrierenden Datenelementen 'DEA|Wert'
// Ergebnis: DSfG-Kommando
// -----------------------------------------------------------------------------
function GetEinstDSfGTelegramm(cAdrDFU, cAdrInst: Char; sZC1, sZC2: String;
                               p: TStrings): String;
// -----------------------------------------------------------------------------
const
  C_Trennzeichen = #124;  // '|'
var
  i, iCnt: Integer;
  s, sWert, sDEA: String;
  sPara, sCode1, sCode2: String;
  sTelStart, sTelEnde: String;
begin
  Result := '';
  iCnt   := 0;  // ZAE-Zähler (Anzahl Datenelemente) (Code 1 + Code 2)
  sPara  := '';
  // Datenelemente für Parametrierung ermitteln
  if Assigned(p) and (p.Count > 0) then
  begin
    for i:=0 to p.Count-1 do
    begin
      s     := p.Strings[i];
      sDEA  := F_Zerlegen(s, C_Trennzeichen);
      sWert := F_Zerlegen(s, C_Trennzeichen);
      if (sDEA <> '') and (sWert <> '') then
      begin
        sPara  := sPara + Chr(gs) + sDEA + Chr(us) + sWert;
        Inc(iCnt);  // ZAE-Zähler (Anzahl Datenelemente)
      end;
    end;
  end;

  // Telegramm zusammenstellen
  if (iCnt > 0) and (sPara <> '') then  // mind. 1 Datenelement parametrieren
  begin
    iCnt   := iCnt + 2; // ZAE-Zähler (Anzahl Datenelemente + Code 1 + Code 2)
    sCode1 := C_DEA_Zugangscode1 + Chr(us) + sZC1 + Chr(gs);
    sCode2 := C_DEA_Zugangscode2 + Chr(us) + sZC2;
    sTelStart := Chr(stx) + cAdrInst + Chr(us)
                   + IntToStr(255) + Chr(us)         { DID }
                   + IntToStr(C_AktTID) + Chr(us)    { TID }
                   + '1' + Chr(us)                   { BLO }
                   + '1' + Chr(us)                   { BNR }
                   + cAdrDFU + Chr(us)               { DNO }
                   + 'E' + Chr(us)                   { NTY }
                   + 'J' + Chr(us)                   { DFO }
                   + 'M' + chr(us)                   { DEB }
                   + IntToStr(iCnt) + Chr(us)        { ZAE }
                   + sCode1                          { Zugangscode 1 }
                   + sCode2;                         { Zugangscode 2 }
    sTelEnde := Chr(fs) + Chr(etx);                  { Abschluß }
    Result := sTelStart + sPara + sTelEnde;
  end;
end;

{ Gibt ein DSfG-Telegr. für Abfrage 1 DE   }
{ zurück                                   }
{ Parameter: CardAdr, InstAdr; Datenel.    }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetOneDEDSfGTelegramm(cAdr, cInst: char; sDEL: string): string;
{------------------------------------------}
begin
  IncAktTID;

  Result := Chr(stx) + cInst + Chr(us)
                   + '255' + Chr(us)                   { DID }
                   + IntToStr(C_AktTID) + Chr(us)      { TID }
                   + '1' + Chr(us)                     { BLO }
                   + '1' + Chr(us)                     { BNR }
                   + cAdr + Chr(us)                    { DNO }
                   + 'A' + Chr(us)                     { NTY }
                   + 'J' + Chr(us)                     { DFO }
                   + 'M' + Chr(us)                     { DEB }
                   + '1' + Chr(us)                     { ZAE }
                   { Datenteil }
                   + sDEL + Chr(fs) + Chr(etx);        { DEA und Abschluß }
end;

{ Gibt ein DSfG-Telegr. für Abfrage        }
{ mehrerer DE zurück                       }
{ Parameter: CardAdr, InstAdr, DEAs, Anzahl}
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetMultiDEDSfGTelegramm(cAdr, cInst: char; sDEL: string; iZAE: integer): string;
{------------------------------------------}
begin
  IncAktTID;

  Result := Chr(stx) + cInst + Chr(us)
                   + '255' + Chr(us)                   { DID }
                   + IntToStr(C_AktTID) + Chr(us)      { TID }
                   + '1' + Chr(us)                     { BLO }
                   + '1' + Chr(us)                     { BNR }
                   + cAdr + Chr(us)                    { DNO }
                   + 'A' + Chr(us)                     { NTY }
                   + 'J' + Chr(us)                     { DFO }
                   + 'M' + Chr(us)                     { DEB }
                   + IntToStr(iZAE) + Chr(us)          { ZAE }
                   { Datenteil }
                   + sDEL + Chr(fs) + Chr(etx);        { DEAs und Abschluß }
end;

{ Gibt ein DSfG-Telegr. für Abfrage eines  }
{ DE-Bereichs zurück                       }
{ Parameter: CardAdr, InstAdr; Datenel.    }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetBereichsabfrageDSfGTelegramm(
  cAdr, cInst: char; sDEAVon, sDEABis: string): string;
{------------------------------------------}
begin
  IncAktTID;

  Result := Chr(stx) + cInst + Chr(us)
                   + IntToStr(255) + Chr(us)           { DID }
                   + IntToStr(C_AktTID) + Chr(us)      { TID }
                   + '1' + Chr(us)                     { BLO }
                   + '1' + Chr(us)                     { BNR }
                   + cAdr + Chr(us)                    { DNO }
                   + 'A' + Chr(us)                     { NTY }
                   + 'J' + Chr(us)                     { DFO }
                   + 'V' + chr(us)                     { DEB }
                   + IntToStr(2) + Chr(us)             { ZAE }
                   { Datenteil }
                   + sDEAVon + Chr(gs)                 { DEA Von }
                   + sDEABis + Chr(fs) + Chr(etx);     { DEA Bis und Abschluß }
end;

{ Gibt ein DSfG-Telegramm für eine Daten-  }
{ bereichsabfrage nach ONr. zurück         }
{ Parameter: CardAdr, InstAdr; DEA; ONr.   }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetDatenabfrageONrTelegramm(
  cAdr, cInst: char; sDEA: string; iONrVon, iONrBis: integer): string;
{------------------------------------------}
begin
  IncAktTID;

  Result := Chr(stx) + cInst + Chr(us)
                   + IntToStr(255) + Chr(us)           { DID }
                   + IntToStr(C_AktTID) + Chr(us)      { TID }
                   + '1' + Chr(us)                     { BLO }
                   + '1' + Chr(us)                     { BNR }
                   + cAdr + Chr(us)                    { DNO }
                   + 'A' + Chr(us)                     { NTY }
                   + 'J' + Chr(us)                     { DFO }
                   + 'O' + chr(us)                     { DEB }
                   + IntToStr(2) + Chr(us)             { ZAE }
                   { Datenteil }
                   + sDEA + Chr(us) + Chr(us) + Chr(us)
                   + IntToStr(iONrVon) + Chr(gs)       { ONr Von }
                   + sDEA + Chr(us) + Chr(us) + Chr(us)
                   + IntToStr(iONrBis)                 { ONr Bis }
                   + Chr(fs) + Chr(etx);               { Abschluß }
end;

{ Gibt ein DSfG-Telegramm für eine Daten-  }
{ bereichsabfrage nach der Zeit zurück     }
{ Parameter: CardAdr, InstAdr; DEA;        }
{            Zeiten als Unix-Zeit          }
{ Rückgabe: DSfG-Kommando                  }
{------------------------------------------}
function GetDatenabfrageZeitbereichTelegramm(
  cAdr, cInst: char; sDEA: string; sZeitVon, sZeitBis: string): string;
{------------------------------------------}
begin
  IncAktTID;

  Result := Chr(stx) + cInst + Chr(us)
                   + IntToStr(255) + Chr(us)           { DID }
                   + IntToStr(C_AktTID) + Chr(us)      { TID }
                   + '1' + Chr(us)                     { BLO }
                   + '1' + Chr(us)                     { BNR }
                   + cAdr + Chr(us)                    { DNO }
                   + 'A' + Chr(us)                     { NTY }
                   + 'J' + Chr(us)                     { DFO }
                   + 'Z' + chr(us)                     { DEB }
                   + IntToStr(2) + Chr(us)             { ZAE }
                   { Datenteil }
                   + sDEA + Chr(us) + Chr(us)
                   + sZeitVon + Chr(gs)                { Zeit Von }
                   + sDEA + Chr(us) + Chr(us)
                   + sZeitBis                          { Zeit Bis }
                   + Chr(fs) + Chr(etx);               { Abschluß }
end;

{ Erzeugt ein Folgetelegramm bei unvollst. }
{ Antwort auf eine Parameterabfrage        }
{ Parameter: Gesendetes/Empf. Telegramm    }
{ Rückgabe: Folgetelegramm                 }
{------------------------------------------}
function ModifyDSfGParamTelegramm(sDSfGCommand, sDSfGAnswer: string): string;
{------------------------------------------}
var
  pSl              : TStrings;
  sDEAVon, sDEABis : string;
begin
  Result := '';

  pSl := GetDatenListe(sDSfGAnswer);
  if (Assigned(pSl)) then
  try
    if (pSl.Count > 0) then begin
      sDEAVon := pSl[pSl.Count-1];
      if (Pos(Chr(us), sDEAVon) > 0) then
        sDEAVon := Copy(sDEAVon, 1, Pos(Chr(us), sDEAVon)-1)
      else if (Pos(Chr(gs), sDEAVon) > 0) then
        sDEAVon := Copy(sDEAVon, 1, Pos(Chr(gs), sDEAVon)-1)
      else if (Pos(Chr(fs), sDEAVon) > 0) then
        sDEAVon := Copy(sDEAVon, 1, Pos(Chr(fs), sDEAVon)-1);
    end
    else Exit;
  finally
    pSl.Free;
  end;

  pSl := GetDatenListe(sDSfGCommand);
  if (Assigned(pSl)) then
  try
    if (pSl.Count > 0) then begin
      sDEABis := pSl[pSl.Count-1];
      if (Pos(Chr(us), sDEABis) > 0) then
        sDEABis := Copy(sDEABis, 1, Pos(Chr(us), sDEABis)-1);
    end
    else Exit;
  finally
    pSl.Free;
  end;

  Result := GetBereichsabfrageDSfGTelegramm(GetDSfGAbsAdr(sDSfGCommand),
    GetDSfGInstAdr(sDSfGCommand), sDEAVon, sDEABis);
end;

{ Erzeugt ein Folgetelegramm bei unvollst. }
{ Antwort auf eine Ordnungsnummernabfrage  }
{ Parameter: Gesendetes/Empf. Telegramm    }
{ Rückgabe: Folgetelegramm                 }
{------------------------------------------}
function ModifyDSfGONrTelegramm(sDSfGCommand, sDSfGAnswer: string): string;
{------------------------------------------}
var
  pSl  : TStrings;
  sDEA : string;
  iONrVon, iONrBis : integer;
begin
  Result := '';
  iONrVon := 0;
  iONrBis := 0;

  pSl := GetDatenListe(sDSfGAnswer);
  if (Assigned(pSl)) then
  try
    if (pSl.Count > 0) then begin
      sDEA := GetStringPart(pSl[0], 1);
      iONrVon := StrToIntDef(GetStringPart(pSl[pSl.Count-1], 4), -1) + 1;
      if (iONrVon <= 0) then Exit;
    end
    else Exit;
  finally
    pSl.Free;
  end;

  pSl := GetDatenListe(sDSfGCommand);
  if (Assigned(pSl)) then
  try
    if (pSl.Count > 0) then begin
      iONrBis := StrToIntDef(GetStringPart(pSl[pSl.Count-1], 4), -1);
      if (iONrBis < 0) then Exit;
    end
    else Exit;
  finally
    pSl.Free;
  end;

  if (iONrVon <= iONrBis) then
    Result := GetDatenabfrageONrTelegramm(GetDSfGAbsAdr(sDSfGCommand),
      GetDSfGInstAdr(sDSfGCommand), sDEA, iONrVon, iONrBis);
end;

{ Erzeugt ein Folgetelegramm bei unvollst. }
{ Antwort auf eine Zeitbereichsabfrage     }
{ Parameter: Gesendetes/Empf. Telegramm    }
{ Rückgabe: Folgetelegramm                 }
{------------------------------------------}
function ModifyDSfGZeitBereichsTelegramm(
  sDSfGCommand, sDSfGAnswer: string): string;
{------------------------------------------}
var
  pSl  : TStrings;
  sDEA : string;
  iZeitVon, iZeitBis : Longword;
begin
  Result := '';
  iZeitVon := 0;
  iZeitBis := 0;

  pSl := GetDatenListe(sDSfGAnswer);
  if (Assigned(pSl)) then
  try
    if (pSl.Count > 0) then begin
      sDEA := GetStringPart(pSl[0], 1);
      iZeitVon := StrToIntDef('$' + GetStringPart(pSl[pSl.Count-1], 3), -1);
      iZeitVon := iZeitVon + 1;
      if (iZeitVon <= 0) then Exit;
    end
    else Exit;
  finally
    pSl.Free;
  end;

  pSl := GetDatenListe(sDSfGCommand);
  if (Assigned(pSl)) then
  try
    if (pSl.Count > 0)
    then iZeitBis := StrToIntDef('$' + GetStringPart(pSl[pSl.Count-1], 3), -1)
    else Exit;
  finally
    pSl.Free;
  end;

  if (iZeitVon <= iZeitBis) then
    Result := GetDatenabfrageZeitbereichTelegramm(GetDSfGAbsAdr(sDSfGCommand),
      GetDSfGInstAdr(sDSfGCommand), sDEA, IntToHex(iZeitVon, 8),
      IntToHex(iZeitBis, 8));
end;

{ Gibt den HDCL eines Telegramms zurück    }
{ Parameter: DSfG-Telegramm                }
{ Rückgabe: Header oder nix                }
{------------------------------------------}
function GetHDCL(sTelegramm: string): string;
{------------------------------------------}
var
  i, iDID : integer;
begin
  Result := '';

  iDID := StrToIntDef(GetStringPart(sTelegramm, 2), 255);
  i := 1;
  Result := Result + GetStringPart(sTelegramm, i) + Chr(us);      { TRN }
  Inc(i);
  Result := Result + GetStringPart(sTelegramm, i) + Chr(us);      { DID }
  Inc(i);
  if (iDID AND C_TID) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { TID }
    Inc(i);
  end;
  if (iDID AND C_BLO) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { BLO }
    Inc(i);
  end;
  if (iDID AND C_BNR) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { BNR }
    Inc(i);
  end;
  if (iDID AND C_DNO) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { DNO }
    Inc(i);
  end;
  if (iDID AND C_NTY) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { NTY }
    Inc(i);
  end;
  if (iDID AND C_DFO) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { DFO }
    Inc(i);
  end;
  if (iDID AND C_DEB) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { DEB }
    Inc(i);
  end;
  if (iDID AND C_ZAE) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { ZAE }
    Inc(i);
  end;
  if (iDID AND C_PAS) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { PAS }
    Inc(i);
  end;
  if (iDID AND C_DTY) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { DTY }
    Inc(i);
  end;
  if (iDID AND C_ABS) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { ABS }
    Inc(i);
  end;
  if (iDID AND C_EMF) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { EMF }
    Inc(i);
  end;
  if (iDID AND C_TDA) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { TDA }
    Inc(i);
  end;
  if (iDID AND C_TTI) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { TTI }
    Inc(i);
  end;
  if (iDID AND C_PTB) <> 0 then begin
    Result := Result + GetStringPart(sTelegramm, i) + Chr(us);    { PTB }
  end;
end;

{ Gibt Datenteil (ohne us,ohne etx) zurück }
{ Parameter: DSfG-Telegramm                }
{ Rückgabe: Datenteil                      }
{------------------------------------------}
function GetDatenTeil(sTelegramm: string): string;
{------------------------------------------}
var
  s       : string;
  i, iDID : integer;
begin
  if ((Pos(Chr(stx), sTelegramm) > 0) and (Pos(Chr(us), sTelegramm) > 0) and
     ((Pos(Chr(etx), sTelegramm) > 0) or (Pos(Chr(etb), sTelegramm) > 0)))
  then begin
    s := sTelegramm;
    i := 2;  // Index für Daten hinter STX (i.d. Regel nur 1 STX => i = 2)
    s := GetStringPart(s, i, Chr(stx));       // (i-1). Teil nach stx
    // 29.03.2004  -  weitere STX herausfiltern, falls Teil des vorigen BCC
    while ((Pos(Chr(etx), s) = 0) and (Pos(Chr(etb), s) = 0)) do begin
      Inc(i);
      s := GetStringPart(sTelegramm, i, Chr(stx));  // (i-1). Teil nach stx
      if (Length(s) < 2) then s := '';
    end;
    if (Pos(Chr(etx), sTelegramm) > 0)
    then s := GetStringPart(s, 1, Chr(etx))   // Teil vor etx
    else s := GetStringPart(s, 1, Chr(etb));  // Teil vor etb
    iDID := StrToIntDef(GetStringPart(s, 2), 255);


    // Test, ob im HDCL noch weitere Felder vorhanden sind (keine Auswertung):
    Delete(s, 1, Pos(Chr(us), s));    { TRN }
    Delete(s, 1, Pos(Chr(us), s));    { DID }
    if (iDID AND C_TID) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { TID }
    if (iDID AND C_BLO) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { BLO }
    if (iDID AND C_BNR) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { BNR }
    if (iDID AND C_DNO) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { DNO }
    if (iDID AND C_NTY) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { NTY }
    if (iDID AND C_DFO) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { DFO }
    if (iDID AND C_DEB) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { DEB }
    if (iDID AND C_ZAE) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { ZAE }
    if (iDID AND C_PAS) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { PAS }
    if (iDID AND C_DTY) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { DTY }
    if (iDID AND C_ABS) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { ABS }
    if (iDID AND C_EMF) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { EMF }
    if (iDID AND C_TDA) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { TDA }
    if (iDID AND C_TTI) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { TTI }
    if (iDID AND C_PTB) <> 0 then Delete(s, 1, Pos(Chr(us), s));    { PTB }

    if (Pos(Chr(etb), sTelegramm) > 0) then Result := s   // 06.10.2003
    else if (s[Length(s)] = Chr(fs)) then Result := Copy(s, 1, Length(s)-1);
  end
  else Result := '';  // kein gültiges DSfG-Telegramm
end;

{ Gibt eine Liste mit DE u. Werten zurück  }
{ Parameter: DSfG-Telegramm                }
{ Rückgabe: Datenliste oder nil            }
{------------------------------------------}
function GetDatenListe(sTelegramm: string): TStrings;
{------------------------------------------}
var
  s    : string;
begin
  if ((Pos(Chr(stx), sTelegramm) > 0) and (Pos(Chr(etx), sTelegramm) > 0))
  then begin
    s := GetDatenTeil(sTelegramm);
    Result := TStringList.Create;

    // Alle Datenteile bis auf den letzten in Liste eintragen
    while (Pos(Chr(gs), s) > 0) do begin
      Result.Add(Copy(s, 1, Pos(Chr(gs), s)-1));
      Delete(s, 1, Pos(Chr(gs), s));
    end;
    // Letzten Datenteil in Liste eintragen
    if (Length(s) > 0) then Result.Add(s);
  end
  else Result := nil;  // kein gültiges DSfG-Telegramm
end;

{ Gibt Wert zu einem Datenelement zurück   }
{ Parameter: DSfG-Telegramm, Datenelement  }
{ Rückgabe: Wert oder ''                   }
{------------------------------------------}
function GetWertFromTelegramm(sTelegramm, sDEA: string): string;
{------------------------------------------}
var
  pSl : TStrings;
  i   : integer;
begin
  Result := '';  // Default

  pSl := GetDatenListe(sTelegramm);
  if (Assigned(pSl)) then
  try
    if (pSl.Count > 0) and (pSl[0] <> '0') then
      for i := 0 to pSl.Count-1 do
        if (GetStringPart(pSl[i], 1) = sDea) then begin
          Result := GetStringPart(pSl[i], 2);
          Break;
        end;
  finally
    pSl.Free;
  end;
end;

{ Gibt Wert zu einem Datenelement zurück   }
{ Parameter: DSfG-Telegramm, Index         }
{ Rückgabe: Wert oder ''                   }
{------------------------------------------}
function GetWertFromTelegramm(sTelegramm: string; iIndex: integer): string;
{------------------------------------------}
var
  pSl : TStrings;
begin
  Result := '';  // Default

  pSl := GetDatenListe(sTelegramm);
  if (Assigned(pSl)) then
  try
    if (pSl.Count > iIndex) and (pSl[0] <> '0') then
      Result := GetStringPart(pSl[iIndex], 2);
  finally
    pSl.Free;
  end;
end;

{ Folgetelegramme aus einer Liste den ursprünglichen Telegrammen zuordnen    }
{----------------------------------------------------------------------------}
procedure FolgeTelegrammeSuchen(TelegrammList: TStrings);
{----------------------------------------------------------------------------}
type
  FTRec = record
    sTID : string;
    sDNO : string;
  end;
  var
    iLoop1, iLoop2 : integer;
    iBNOCount : integer;
    s     : string;
    TRec  : FTRec;

  function GetBlockAnzahl(sTelegramm: string): integer;
  var
    i, j : integer;
  begin
    Result := 0;
    for i := 0 to 2 do begin
      j := Pos(Chr(us), sTelegramm);
      if (j = 0) then Exit else Delete(sTelegramm, 1, j);
    end;
    if (j > 0) then Result := StrToIntDef(sTelegramm[1], 0);
  end;

  function GetTID_DNO(sTelegramm: string): FTRec;
  var
    i, j : integer;
  begin
    Result.sTID := '';
    Result.sDNO := '';
    for i := 0 to 1 do begin
      j := Pos(Chr(us), sTelegramm);
      if (j = 0) then Exit else Delete(sTelegramm, 1, j);
    end;
    j := Pos(Chr(us), sTelegramm);
    if (j > 0) then Result.sTID := Copy(sTelegramm, 1, j-1);
    for i := 0 to 2 do begin
      j := Pos(Chr(us), sTelegramm);
      if (j = 0) then Exit else Delete(sTelegramm, 1, j);
    end;
    if (j > 0) then Result.sDNO := sTelegramm[1];
  end;

begin
  for iLoop1 := 0 to TelegrammList.Count-1 do begin
    if (GetBlockAnzahl(TelegrammList[iLoop1]) > 1) then begin
       iBNOCount := 2;
       TRec := GetTID_DNO(TelegrammList[iLoop1]);
       s := chr(stx) + chr(us) + '15' + chr(us) + TRec.sTID + chr(us) +
         IntToStr(GetBlockAnzahl(TelegrammList[iLoop1])) + chr(us) +
         IntToStr(iBNOCount) + chr(us) + TRec.sDNO + char(us);
       for iLoop2 := iLoop1+1 to TelegrammList.Count-1 do begin
          if (Pos(s, TelegrammList[iLoop2]) > 0) then begin
            TelegrammList[iLoop1] :=
              Copy(TelegrammList[iLoop1], 1, Length(TelegrammList[iLoop1])-1) +
              Copy(TelegrammList[iLoop2], Length(s)+1, Length(TelegrammList[iLoop2]));
            TelegrammList[iLoop2] := 'Folgetelegramm';
            Inc(iBNOCount);
            if (iBNOCount > GetBlockAnzahl(TelegrammList[iLoop1])) then Break;
            s := chr(stx) + chr(us) + '15' + chr(us) + TRec.sTID + chr(us) +
              IntToStr(GetBlockAnzahl(TelegrammList[iLoop1])) + chr(us) +
              IntToStr(iBNOCount) + chr(us) + TRec.sDNO + chr(us);
          end;
       end;
    end;
  end;
end;

{ Gerätetypnummer ermitteln                                                  }
{ Parameter: Hersteller (aba); Gerätetyp (abb)                               }
{ Rückgabe: Gerätetypnummer                                                  }
{----------------------------------------------------------------------------}
function GetGerTypNr(sDEAHersteller, sDEAGeraeteTyp: string): byte;
{----------------------------------------------------------------------------}
begin
  Result := C_GTypNr_Normal;  // Default

  if ((Pos('wies', LowerCase(sDEAHersteller)) > 0) or
     (Pos('rmg', LowerCase(sDEAHersteller)) = 1)) then begin
     if (Pos('220', sDEAGeraeteTyp) > 0) then
       Result := C_GTypNr_MRG2200
     else if (Pos('2000', sDEAGeraeteTyp) > 0) then   // alle ERZ 2000; 24.08.2016, WW
       Result := 8;  // DEL-Gruppe in dellist.db
  end;
  if (Pos('wies', LowerCase(sDEAHersteller)) > 0) and
     (Pos('2100D', sDEAGeraeteTyp) > 0) then
  begin
    Result := C_GTypNr_MRG2100D;
  end;
end;

// 20.09.2011  WN
// -----------------------------------------------------------------------------
// alle unterstützten Datenelemente der Registrier-Instanz bezügl. Archivgruppe
// erzeugen: Kennung, Zahl Kanäle, Füllstand O.-Nr. von..bis für AGr.1..25
// Rückgabe: Liste mit allen Datenelementen für Archivgruppen
// -----------------------------------------------------------------------------
procedure BildeArchivgruppenElementeListe(pSl: TStrings);
// -----------------------------------------------------------------------------
var
  iAGr: Integer;
  sAGr: String;
begin
  if Assigned(pSl) then
  begin
    for iAGr:=1 to 25 do
    begin
      sAGr := Char(iAGr + C_DEA_AGr_Offset);  // a..y: Archivgruppe 1 ..25
      pSl.Add('ca' + sAGr + 'a');  // DEA für Kennung der Archivgruppe
      pSl.Add('ca' + sAGr + 'b');  // DEA für Zahl Kanäle in der Archivgruppe
      pSl.Add('ca' + sAGr + 'c');  // DEA für Füllstand von Ordnungsnummer
      pSl.Add('ca' + sAGr + 'd');  // DEA für Füllstand bis Ordnungsnummer
    end;
  end;
end;

// 20.09.2011  WN
// -----------------------------------------------------------------------------
// alle unterstützten Datenelemente der Registrier-Instanz bezügl. Archivkanäle
// erzeugen: Archivtyp, EADR der Quell-Instanz, DEL-Adresse der Quell-Instanz,
//           phys. Einheit der Quelle für AGr.1..25, Kanal 1..20
// Rückgabe: Liste mit allen Datenelementen für Archivkanäle
// -----------------------------------------------------------------------------
procedure BildeArchivkanalElementeListe(pSl: TStrings);
// -----------------------------------------------------------------------------
var
  iAGr, iAKanal: Integer;
  sAGr, sAKanal: String;
begin
  if Assigned(pSl) then
  begin
    for iAGr:=1 to 25 do
    begin
      sAGr := Char(iAGr + C_DEA_AGr_Offset);     // a..y: Archivgruppe 1 ..25
      for iAKanal:=1 to 20 do
      begin
        sAKanal := Char(iAKanal + C_DEA_AKa_Offset);  // f..y: Archivkanal  1 ..20
        pSl.Add('ca' + sAGr + sAKanal + 'a');  // DEA für Archivtyp
        pSl.Add('ca' + sAGr + sAKanal + 'b');  // DEA für EADR der Quell-Instanz
        pSl.Add('ca' + sAGr + sAKanal + 'c');  // DEA für DEL-Adresse der Quell-Instanz
        pSl.Add('ca' + sAGr + sAKanal + 'f');  // DEA für phys. Einheit der Quelle
        pSl.Add('ca' + sAGr + sAKanal + 'g');  // DEA für Kanalart
      end;
    end;
  end;
end;

// 20.09.2011  WN
// -----------------------------------------------------------------------------
// Datenelemente in einem Bereich von ... bis ermitteln und in eine Liste schreiben
// nur die Datenelemente verwenden, für die ein Wert in der DB vorhanden ist
// bisher: beschränkt auf a- und c-Elemente
// Parameter: Datenelement von
//            Datenelement von
// Rückgabe: Liste mit allen Datenelementen innerhalb des Bereiches
// -----------------------------------------------------------------------------
procedure GetDatenElementListe(sDeaVon, sDeaBis: String; pSl: TStrings);
// -----------------------------------------------------------------------------
var
  i: Integer;
  s: String;
  pSlDEA: TStrings;
begin
  if Assigned(pSl) then
  begin
    pSl.Clear;
    // alle möglichen Datenelemente ermitteln und in Zwischen-Liste schreiben
    pSlDEA := TStringList.Create;
    try
      // Allgemeine Datenelemente:
      for i:=Low(C_Array_DEA_Allgemein) to High(C_Array_DEA_Allgemein) do
      begin
        pSlDEA.Add(C_Array_DEA_Allgemein[i]);
      end;
      // Archivgruppen-Datenelemente der Registrier-Instanz
      BildeArchivgruppenElementeListe(pSlDEA);
      // Archivkanal-Datenelemente der Registrier-Instanz
      BildeArchivkanalElementeListe(pSlDEA);

      // Zwischen-Liste mit allen Elementen durchgehen und prüfen, ob DEA im Bereich liegt
      for i:=0 to pSlDEA.Count-1 do
      begin
        // Datenelement der Liste hinzufügen ?
        s := pSlDEA.Strings[i];
        if DatenelementInBereich(s, sDeaVon, sDeaBis) then pSl.Add(s);
      end;
    finally
      FreeAndNil(pSlDEA);
    end;
  end;
end;

// 20.09.2011  WN
// -----------------------------------------------------------------------------
// Datenelement in einem Bereich DEA von ... DEA bis
// Parameter: zu prüfendes Datenelement
//            Datenelement von
//            Datenelement von
// Rückgabe: (T), wenn DEA im Bereich DEA von ... DEA bis
// -----------------------------------------------------------------------------
function DatenelementInBereich(sDea, sDeaVon, sDeaBis: String): Boolean;
// -----------------------------------------------------------------------------
var
  iChar: Integer;
  iOrdVon, iOrdBis, iOrd: Integer;
begin
  Result := False;

  // Auswertung Bereich von
  for iChar:=1 to 5 do
  begin
    if (Length(sDeaVon) > (iChar-1))
    then iOrdVon := Ord(sDeaVon[iChar])
    else iOrdVon := 0;

    if (Length(sDea) > (iChar-1))
    then iOrd := Ord(sDea[iChar])
    else iOrd := 0;

    if (iOrdVon < iOrd) then
    begin
      Result := True;
      Break;
    end else if (iOrdVon > iOrd) then
    begin
      Result := False;
      Break;
    end else if (iOrdVon = iOrd) and ((iChar = 5) or (iOrd = 0)) then
    begin
      Result := True;
      Break;
    end;
  end;

  if not Result then Exit;
  Result := False;
  
  // Auswertung Bereich bis
  for iChar:=1 to 5 do
  begin
    if (Length(sDeaBis) > (iChar-1))
    then iOrdBis := Ord(sDeaBis[iChar])
    else iOrdBis := 0;

    if (Length(sDea) > (iChar-1))
    then iOrd := Ord(sDea[iChar])
    else iOrd := 0;

    if (iOrdBis > iOrd) then
    begin
      Result := True;
      Break;
    end else if (iOrdBis < iOrd) then
    begin
      Result := False;
      Break;
    end else if (iOrdBis = iOrd) and ((iChar = 5) or (iOrd = 0)) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

// 20.09.2011  WN
// -----------------------------------------------------------------------------
// Ermittlung der Archivgruppe aus dem Datenelement
// Parameter: Datenelement der Archivanfrage (ca..)
// Ergebnis: Nummer der Archivgruppe
// -----------------------------------------------------------------------------
function GetArchivGruppeFromDEA(sDea: String): Integer;
// -----------------------------------------------------------------------------
begin
  Result := 0;
  // Archiv-Datenelement ?
  if (Length(sDea) > 2) and (sDea[1] = 'c') and (sDea[2] = 'a') then
    Result := Ord(sDea[3]) - C_DEA_AGr_Offset;
end;

// 20.09.2011  WN
// -----------------------------------------------------------------------------
// Ermittlung des Archivkanals aus dem Datenelement
// Parameter: Datenelement der Archivanfrage (ca..)
// Ergebnis: Nummer des Archivkanals
// -----------------------------------------------------------------------------
function GetArchivKanalFromDEA(sDea: String): Integer;
// -----------------------------------------------------------------------------
begin
  Result := 0;
  // Archiv-Datenelement ?
  if (Length(sDea) > 3) and (sDea[1] = 'c') and (sDea[2] = 'a') then
    Result := Ord(sDea[4]) - C_DEA_AKa_Offset;
end;

// 20.09.2011  WN
// -----------------------------------------------------------------------------
// aus Datenelement für Archivabruf werden Archivgruppe und Archivkanal ermittelt
// Parameter: Datenelement der Archivanfrage (ca..d) ("Archivdaten des Kanals")
// Rückgabe: Archivgruppe und Archivkanal zum Datenelement
// Ergebnis: (T) bei Erfolg
// -----------------------------------------------------------------------------
function GetArchivAbrufInfosFromDEA(sDea: String; var iAGr: Integer;
                                    var iAKanal: Integer): Boolean;
// -----------------------------------------------------------------------------
begin
  iAGr    := GetArchivGruppeFromDEA(sDea);
  iAKanal := GetArchivKanalFromDEA(sDea);
  // Kanal 21 nicht zählen (Faktorkanal)
  Result  := (iAGr > 0) and (iAGr < 26) and (iAKanal > 0) and (iAKanal < 21);
  // Archiv-Datenelement ?
  Result := Result and ((Length(sDea) = 5) and (sDea[5] = 'd'));
end;

{ TDSfGTelegrDEObj }

{--------------------------------------------------------}
constructor TDSfGTelegrDEObj.Create (ADaten: TDSfGRohRec);
{--------------------------------------------------------}
begin
  inherited Create;
  Daten:=ADaten;
end;

end.
