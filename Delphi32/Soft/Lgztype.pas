{******************************************************************************}
{* Unit: Typen und Konstanten f�r MRG-Langzeitdaten                           *}
{* Version: 13.05.2002  WW                                                    *}
{******************************************************************************}
unit LGZType;

interface

uses
  T_Tools;

{$H-}

const
  C_MaxKanalZahl = 28;  { max. m�gliche Anzahl der Kan�le, die im Programm gehandelt werden kann }
  C_KennungLen   = 14;

  no_parms = 65535;     { f�r alte LGZ-Meldungen: "keine Parameter�nderungen" }

  { Dateinamen }

  CLangZeitVerz  = 'LGZDVERZ.DAT';  { Verzeichnisdatei }
  CLangZeitMess  = 'LGZD';          { Stundens�tze }
  CLangZeitTag   = 'LGZT';          { Tagess�tze }
  CLangZeitTag14 = 'LGZZ';          { Tagess�tze, 14-stellig (bislang nicht verwendet) }
  CLangZeitPrf   = 'LGZQ';          { Pr�fungss�tze }
  CLangZeitMeld  = 'LGZM';          { Meldungen (alte Struktur, nur noch f�r GVT) }
  CLangZeitPara  = 'LGZP';          { Parameter�nderungen (alte Struktur, nur noch f�r GVT) }

type

  { Langzeitverzeichnis }

  TypsVerzTyp = packed record
    Kennung : string [C_KennungLen];
    nr      : Word;  { Nummer der Langzeitdaten-Datei }
    status  : Byte;  { z.Zt. unbelegt =0 }
    index   : word;  { ben�tigt Auswerteprogramm - MRG-Typ lt. G_Liste}
  end;

  { Langzeitdaten Stundens�tze }

  KanalRec = packed record
    KanalStatus : Byte; { bit 0-3 : �berlauf etc.
			  bit 4-5 : ohne Bedeutung
			  bit 6   : Wert korrigiert?
			  bit 7   : Wert fehlend? }
    Wert        : Word; { normierter Wert 0..9999 }
  end;

  KanalFeldTyp = array [1..C_maxKanalZahl] of KanalRec;

  LGZSRec = packed record
    SatzStatus : Byte;  { bit 0-3 : Uhr gestellt, Netz ein, Systemreset etc.
			  bit 4-6 : ohne Bedeutung
			  bit 7   : Satz normal/hinzugef�gt }
    Datum      : DateRec;
    Zeit       : TimeRec;
    Kanal      : KanalFeldTyp;
  end;

  { Rohdaten Stundens�tze (entstehen aus Konvertierung), neu ab 19.02.2003 }

  KanalOrigRec = packed record      { f�r Original-Kanalwerte aus Ger�t }
    KanalStatus : Byte;    { Bit-Bedeutung wie in KanalRec }
    Wert        : Double;  { Original-Rohwert }
  end;

  KanalOrigFeldTyp = array [1..C_maxKanalZahl] of KanalOrigRec;

  RohSRec = packed record
    SatzStatus : Byte;    { Bit-Bedeutung wie in LGZSRec }
    DatumZeit  : TDateTime;
    Kanal      : KanalFeldTyp;
    KanalOrig  : KanalOrigFeldTyp;
  end;

  { Objekt f�r RohSRec }
  TRohSRecObj = class(TObject)
  public
    RohSRec: RohSRec;
    constructor Create (ARohSRec: RohSRec);
  end;

  { Langzeitdaten Tagess�tze }

  Zaehler_rec = packed record
    zaehlerstatus : byte;    { Bit 7=128 => Fehlend, sonst 0 }
    wert          : longint;
  end;

  Zaehler_typ = array[1..C_maxkanalzahl] of zaehler_rec;

  LGZTRec = packed record
    satzstatus : byte;
    Datum      : DateRec;
    Stunden    : byte;
    E_Zaehler  : Zaehler_typ;
    K_Zaehler  : Zaehler_typ;
  end;

  { Rohdaten Tagess�tze (entstehen aus Konvertierung), neu ab 08.06.2004 }

  RohTRec = packed record
    satzstatus : byte;
    DatumZeit  : TDateTime;
    E_Zaehler  : Zaehler_typ;
    K_Zaehler  : Zaehler_typ;
  end;

  { Objekt f�r RohTRec }

  TRohTRecObj = class(TObject)
  public
    RohTRec: RohTRec;
    constructor Create (ARohTRec: RohTRec);
  end;

(*  { Langzeitdaten Tagess�tze 14-stellig }

  Zaehler14_rec = packed record
    Zaehlerstatus : byte;     { Bit 7=128 => Fehlend, sonst 0 }
    Wert          : comp;
  end;

  Zaehler14_typ = array[1..C_maxkanalzahl] of zaehler14_rec;

  LGZT14Rec = packed record
	        Satzstatus  : longint;
                UmwStatus   : longint;
	        Datum       : DateRec;
	        Zeit        : TimeRec;
	        Zaehler     : Zaehler14_typ;
	      end;

  Anm. WW: Diese Struktur ist anders als in der DOS-DF� ! Warum ?
           Da das unter diesen Umst�nden eh nicht funktionieren kann: im MRG-Abrufmodul
           alles auskommentiert, was mit "Z�hler14" zu tun hat. *)

  { Langzeitdaten Pr�fungss�tze f�r MRG 4013Q }

  PruefRec = packed record
    kanal     : char;
    von_datum : DateRec;
    von_zeit  : TimeRec;
    bis_datum : DateRec;
    bis_zeit  : TimeRec;
    wert      : array [1..4] of string[5];
  end;

  { Langzeitdaten Meldungen }

  MeldRec = packed record
    Kennung    : string [C_KennungLen];
    nummer     : word;
    K_datum    : daterec;    { "Kommt" Datum }
    K_zeit     : timerec;    { "Kommt" Zeit }
    G_datum    : daterec;    { "Geht" Datum }
    G_zeit     : timerec;    { "Geht" Zeit }
    status_neu : boolean;    { neu hinzugef�gt=TRUE, sonst FALSE }
  end;

  { Langzeitdaten Parameter�nderungen }

  ParmRec = packed record
    nummer : word;
    datum  : daterec;
    zeit   : timerec;
    p_alt  : string [64];
    P_neu  : string [64];
  end;

implementation

{ TRohSRecObj }

{-------------------------------------------------}
constructor TRohSRecObj.Create (ARohSRec: RohSRec);
{-------------------------------------------------}
begin
  inherited Create;
  RohSRec:=ARohSRec;
end;

{ TRohTRecObj }

{-------------------------------------------------}
constructor TRohTRecObj.Create (ARohTRec: RohTRec);
{-------------------------------------------------}
begin
  inherited Create;
  RohTRec:=ARohTRec;
end;

end.
