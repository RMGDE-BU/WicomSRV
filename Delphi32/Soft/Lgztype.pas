{******************************************************************************}
{* Unit: Typen und Konstanten für MRG-Langzeitdaten                           *}
{* Version: 13.05.2002  WW                                                    *}
{******************************************************************************}
unit LGZType;

interface

uses
  T_Tools;

{$H-}

const
  C_MaxKanalZahl = 28;  { max. mögliche Anzahl der Kanäle, die im Programm gehandelt werden kann }
  C_KennungLen   = 14;

  no_parms = 65535;     { für alte LGZ-Meldungen: "keine Parameteränderungen" }

  { Dateinamen }

  CLangZeitVerz  = 'LGZDVERZ.DAT';  { Verzeichnisdatei }
  CLangZeitMess  = 'LGZD';          { Stundensätze }
  CLangZeitTag   = 'LGZT';          { Tagessätze }
  CLangZeitTag14 = 'LGZZ';          { Tagessätze, 14-stellig (bislang nicht verwendet) }
  CLangZeitPrf   = 'LGZQ';          { Prüfungssätze }
  CLangZeitMeld  = 'LGZM';          { Meldungen (alte Struktur, nur noch für GVT) }
  CLangZeitPara  = 'LGZP';          { Parameteränderungen (alte Struktur, nur noch für GVT) }

type

  { Langzeitverzeichnis }

  TypsVerzTyp = packed record
    Kennung : string [C_KennungLen];
    nr      : Word;  { Nummer der Langzeitdaten-Datei }
    status  : Byte;  { z.Zt. unbelegt =0 }
    index   : word;  { benötigt Auswerteprogramm - MRG-Typ lt. G_Liste}
  end;

  { Langzeitdaten Stundensätze }

  KanalRec = packed record
    KanalStatus : Byte; { bit 0-3 : Überlauf etc.
			  bit 4-5 : ohne Bedeutung
			  bit 6   : Wert korrigiert?
			  bit 7   : Wert fehlend? }
    Wert        : Word; { normierter Wert 0..9999 }
  end;

  KanalFeldTyp = array [1..C_maxKanalZahl] of KanalRec;

  LGZSRec = packed record
    SatzStatus : Byte;  { bit 0-3 : Uhr gestellt, Netz ein, Systemreset etc.
			  bit 4-6 : ohne Bedeutung
			  bit 7   : Satz normal/hinzugefügt }
    Datum      : DateRec;
    Zeit       : TimeRec;
    Kanal      : KanalFeldTyp;
  end;

  { Rohdaten Stundensätze (entstehen aus Konvertierung), neu ab 19.02.2003 }

  KanalOrigRec = packed record      { für Original-Kanalwerte aus Gerät }
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

  { Objekt für RohSRec }
  TRohSRecObj = class(TObject)
  public
    RohSRec: RohSRec;
    constructor Create (ARohSRec: RohSRec);
  end;

  { Langzeitdaten Tagessätze }

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

  { Rohdaten Tagessätze (entstehen aus Konvertierung), neu ab 08.06.2004 }

  RohTRec = packed record
    satzstatus : byte;
    DatumZeit  : TDateTime;
    E_Zaehler  : Zaehler_typ;
    K_Zaehler  : Zaehler_typ;
  end;

  { Objekt für RohTRec }

  TRohTRecObj = class(TObject)
  public
    RohTRec: RohTRec;
    constructor Create (ARohTRec: RohTRec);
  end;

(*  { Langzeitdaten Tagessätze 14-stellig }

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

  Anm. WW: Diese Struktur ist anders als in der DOS-DFÜ ! Warum ?
           Da das unter diesen Umständen eh nicht funktionieren kann: im MRG-Abrufmodul
           alles auskommentiert, was mit "Zähler14" zu tun hat. *)

  { Langzeitdaten Prüfungssätze für MRG 4013Q }

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
    status_neu : boolean;    { neu hinzugefügt=TRUE, sonst FALSE }
  end;

  { Langzeitdaten Parameteränderungen }

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
