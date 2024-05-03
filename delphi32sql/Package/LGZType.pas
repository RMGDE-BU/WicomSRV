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
  { max. mögliche Anzahl der MRG-Kanäle, welche die Programme verarbeiten können: }
{$IFDEF GAS-X}
  C_MaxKanalZahl = 120;  { für WicomSrv mit Gas-X-Client wegen Gerätetypen "KE" }
{$ELSE}
  C_MaxKanalZahl =  28;  { Standard für WK22-System }
{$ENDIF}

  C_KennungLen   = 14;

type

  { Langzeitdaten Stundensätze }

  KanalRec = packed record
    KanalStatus : Byte; { bit 0-3 : Überlauf etc.
			  bit 4-5 : ohne Bedeutung
			  bit 6   : Wert korrigiert?
			  bit 7   : Wert fehlend? }
    Wert        : Longint; { normierter Wert; Analogwertebereich: 0..9999
                             ab 12.01.2008: Longint statt Word für erweiterten
                             Impulswertebereich }
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
    OrdNr       : Longint;  { Ordnungsnummer, neu ab 18.05.2018, WW }
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

  { Objekt für LGZTRec }

  TLGZTRecObj = class(TObject)
  public
    LGZTRec: LGZTRec;
    constructor Create (ALGZTRec: LGZTRec);
  end;

  { Rohdaten Tagessätze (entstehen aus Konvertierung), neu ab 08.06.2004 }

  ZaehlerOrig_rec = packed record
    zaehlerstatus : byte;    { Bit 7=128 => Fehlend, sonst 0 }
    wert          : double;  // 20.10.2010, WW
  end;

  ZaehlerOrig_typ = array[1..C_maxkanalzahl] of ZaehlerOrig_rec;

  RohTRec = packed record
    satzstatus : byte;
    DatumZeit  : TDateTime;
    E_Zaehler  : ZaehlerOrig_typ;
    K_Zaehler  : ZaehlerOrig_typ;
  end;

  { Objekt für RohTRec }

  TRohTRecObj = class(TObject)
  public
    RohTRec: RohTRec;
    constructor Create (ARohTRec: RohTRec);
  end;

  { erweitertes Objekt für RohTRec und Rückstellnummer }

  TRohTRecExtObj = class(TObject)
  public
    RohTRec: RohTRec;
    RueckstellNr: integer;
    constructor Create (ARohTRec: RohTRec; ARueckstellNr: integer);
  end;

  { Langzeitdaten Prüfungssätze für MRG 4013Q }

  PruefRec = packed record
    kanal     : char;
    von_datum : DateRec;
    von_zeit  : TimeRec;
    bis_datum : DateRec;
    bis_zeit  : TimeRec;
    wert      : array [1..4] of string[5];
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

{ TRohTRecExtObj }

{----------------------------------------------------------------------------}
constructor TRohTRecExtObj.Create (ARohTRec: RohTRec; ARueckstellNr: integer);
{----------------------------------------------------------------------------}
begin
  inherited Create;
  RohTRec:=ARohTRec;
  RueckstellNr:=ARueckstellNr;
end;

{ TLGZTRecObj }

{-------------------------------------------------}
constructor TLGZTRecObj.Create (ALGZTRec: LGZTRec);
{-------------------------------------------------}
begin
  inherited Create;
  LGZTRec:=ALGZTRec;
end;

end.
