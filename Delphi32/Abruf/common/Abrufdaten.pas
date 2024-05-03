{------------------------------------------------------------------------------}
{ Bereitstellung von Abrufdaten                                                }
{                                                                              }
{ 18.10.2001  GD  Neu                                                          }
{ 12.08.2002  GD  von String- auf Zeigerlisten umgestellt (Speicherbedarf)     }
{ 10.07.2003  GD  Default-Format auf 4 Nachkommastellen eingestellt            }
{ 03.09.2003  GD  Formatierten EXCEL-Export integriert                         }
{ 07.10.2003  GD  Bugfix: Integer werden jetzt als echte Zeiger gespeichert    }
{ 07.03.2005  GD  Zugriff auf Statusliste jetzt public                         }
{ 26.05.2006  GD  Erweitert um den Eintrag von Kanalnummern                    }
{ 11.09.2007  GD  Gültigkeitsmerkmale erweitert                                }
{ 15.02.2011  WN  zus. Kanal-Infos anzeigen: ZPkt, Obis und Einheit            }
{ 07.06.2011  WN  Excel-Export mit Anzeige Uhrzeit 0:00 Uhr                    }
{ 19.12.2011  GD  Neuer Spaltentyp "Zaehlwert"                                 }
{ 07.12.2012  GD  Zusätzliche Statistik-Funktionen                             }
{ 19.03.2014  WW  mit Property TAbrufDaten.Statuszahl                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH, RMG Messtechnik GmbH 2001, 2014              }
{------------------------------------------------------------------------------}
unit Abrufdaten;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, ComObj, Dialogs,
  COM_Utils, WSysCon, GD_Utils;

resourcestring
  S_Format_Date    = 'dd/mm/yyyy';
  S_Format_Time    = 'hh:nn:ss';
  S_Text_DatumZeit = 'Datum/Zeit';
  S_ValueNoValue   = ' - ';
  S_ValueNotValid  = 'ungültig';

const
  C_SpaltenTyp_ReferenzNr   = 1;
  C_SpaltenTyp_DatumZeit    = 2;
  C_SpaltenTyp_ZeitZone     = 3;
  C_SpaltenTyp_OrdnungsNr   = 4;
  C_SpaltenTyp_Messwert     = 5;
  C_SpaltenTyp_Zaehlerstand = 6;
  C_SpaltenTyp_Status       = 7;
  C_SpaltenTyp_Text         = 8;
  C_SpaltenTyp_Zaehlwert    = 9;  // 19.12.2011

  C_Value_NoValue  = -9999;  // kein Wert
  C_Value_NotValid = -9998;  // ungültiger Wert

type
  TStatusListe = class(TList)
  private
  protected
  public
    procedure Delete(iIndex: integer);
    procedure Clear; override;
    function GetCol(iIndex: integer): TStrings;
  end;

  TKanalListe = class(TList)
    constructor Create(iTyp: byte = 0; sMask: string = '');
  private
    FSpaltenTyp  : byte;
    FKontroll    : boolean;
    FFormatMask  : string;
    FValNotValid : integer;
    FValNoValue  : integer;
    FKanalId     : integer;
    FUnit        : String;  // 15.02.2011  WN
    FObis        : String;  // 15.02.2011  WN
    FZpkt        : String;  // 15.02.2011  WN
  protected
    function GetDefaultMask: string;
    function GetString(iIndex: integer): string; virtual;
    procedure SetString(iIndex: integer; sValue: string); virtual;
    function GetDateTime(iIndex: integer): TDateTime;
    procedure SetDateTime(iIndex: integer; dtValue: TDateTime);
    function GetFloat(iIndex: integer): double;
    procedure SetFloat(iIndex: integer; fValue: double);
    function GetInteger(iIndex: integer): Int64;
    procedure SetInteger(iIndex: integer; iValue: Int64);
  public
    procedure Delete(iIndex: integer); virtual;
    procedure Clear; override;
    function IndexOf(const sValue: string): integer;
    function AddString(const sValue: string): integer; virtual;
    function AddInteger(const iValue: Int64): integer; virtual;
    function InsertString(
      iIndex: integer; const sValue: string): integer; virtual;
    function GetStatistic(var sSumme, sAverage, sMin, sMax: string): boolean; // 07.12.2012
    procedure Assign(pList: TKanalListe);
    property SpaltenTyp: byte read FSpaltenTyp write FSpaltenTyp default 0;
    property Strings [iIndex: integer]: string read GetString write SetString;
    property AsDateTime [iIndex: integer]: TDateTime
      read GetDateTime write SetDateTime;  // 10.07.2003
    property AsInteger [iIndex: integer]: Int64
      read GetInteger write SetInteger;      // 10.07.2003
    property AsFloat [iIndex: integer]: double
      read GetFloat write SetFloat;           // 10.07.2003
    property Kontroll: boolean read FKontroll write FKontroll;
    property Mask: string read FFormatMask;
    property ValNotValid: integer read FValNotValid write FValNotValid;
    property ValNoValue: integer read FValNoValue write FValNoValue;
    property KanalId: integer read FKanalId write FKanalId;
    property KanalUnit: String read FUnit write FUnit;  // 15.02.2011  WN
    property KanalObis: String read FObis write FObis;  // 15.02.2011  WN
    property KanalZpkt: String read FZpkt write FZpkt;  // 15.02.2011  WN
  end;

  TAbrufDaten = class(TStringList)
    constructor Create(sDatabaseName: string; iGerId: integer);
    destructor Destroy; override;
  private
    FDatabaseName   : string;
    FGeraeteId      : integer;
    FMWFormat       : string;
    FZSFormat       : string;
    FDtVon          : TDateTime;
    FDtBis          : TDateTime;
    FDtOldVon       : TDateTime;
    FDtOldBis       : TDateTime;
    FMaxRecords     : integer;
    FStundenWerte   : boolean;
    FIndexDatumZeit : byte;
    FStatusListe    : TStatusListe;
    FExportCaption  : string;
    FStatistics     : boolean;
  protected
    FStatusZahl     : boolean;    // 24.09.2004
    function AddCol(sName: string; iTyp: byte = 0; sMask: string = ''): integer;
    function GetCol(sName: string): TKanalListe; overload;
    procedure AutoDelete; virtual;
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property IndexDatumZeit: byte read FIndexDatumZeit write FIndexDatumZeit;
  public
    procedure Delete(iIndex: integer); override;
    procedure Clear; override;
    procedure CreateStatistic; // 07.12.2012
    function GetCol(iIndex: integer): TKanalListe; overload;
    function ExportToExcel(sBookName: string = ''): boolean;
    procedure LoadData(dtVon: TDateTime = 0; dtBis: TDateTime = 0); virtual;
    property OldVon: TDateTime read FDtOldVon write FDtOldVon;
    property OldBis: TDateTime read FDtOldBis write FDtOldBis;
    property GerId: integer read FGeraeteId write FGeraeteId;
    property MWFormat: string read FMWFormat write FMWFormat;
    property ZSFormat: string read FZSFormat write FZSFormat;
    property DatumVon: TDateTime read FDtVon write FDtVon;
    property DatumBis: TDateTime read FDtBis write FDtBis;
    property MaxRecords: integer read FMaxRecords write FMaxRecords;
    property StundenWerte: boolean read FStundenWerte write FStundenWerte;
    property ExportCaption: string write FExportCaption;
    property StatusListe: TStatusListe read FStatusListe;
    property Statistics: boolean read FStatistics write FStatistics;
    property StatusZahl: boolean write FStatusZahl;  // 19.03.2014, WW
  end;

implementation

resourcestring
  C_Text_Referenz = 'Referenznummer';
  C_Text_OrdNr = 'Ordnungsnr.';
  C_Text_ZeitZone = 'Zeitzone';
  S_AbrufDaten_Export1 = 'Export in Excel';
  S_AbrufDaten_Export2 = 'Exportiere %d Einträge';
  S_AbrufDaten_Export3 = 'Export in Excel - %d Einträge in %d Spalten';
  S_AbrufDaten_Export4 = 'Export. %d von %d in Sp. %d von %d';
  S_AbrufDaten_Export5 = 'Formatiere Spalte %d für Ausgabetext';
  S_Summe              = 'Summe';
  S_Mittelwert         = 'Mittelwert';
  S_Minimum            = 'Minimum';
  S_Maximum            = 'Maximum';

const
  C_Index_Referenz   = 0;
  C_Index_DatumZeit  = 1;
  C_Index_Zeitzone   = 2;
  C_Index_OrdnungsNr = 3;

{------------------------------ TStatusListe ----------------------------------}

{------------------------------------------------------}
procedure TStatusListe.Delete(iIndex: integer);
{------------------------------------------------------}
begin
  if (Assigned(Items[iIndex])) then TObject(Items[iIndex]).Free;

  inherited;
end;

{------------------------------------------------------}
procedure TStatusListe.Clear;
{------------------------------------------------------}
var
  i : integer;
begin
  for i := Count-1 downto 0 do
    if (Assigned(Items[i])) then TObject(Items[i]).Free;

  inherited;
end;

{------------------------------------------------------}
function TStatusListe.GetCol(iIndex: integer): TStrings;
{------------------------------------------------------}
begin
  if (Assigned(Items[iIndex]))
  then Result := TStrings(Items[iIndex])
  else Result := nil;
end;

{---------------------------------- TKanalListe -------------------------------}

{------------------------------------------------------}
constructor TKanalListe.Create(iTyp: byte = 0; sMask: string = '');
{------------------------------------------------------}
begin
  inherited Create;

  FSpaltenTyp := iTyp;
  FValNotValid := 0;  // Kein Wert eingestellt
  FValNoValue := 0;   // Kein Wert eingestellt
  if (sMask = '') then FFormatMask := GetDefaultMask else FFormatMask := sMask;
  FUnit := '';  // 15.02.2011  WN
  FObis := '';  // 15.02.2011  WN
  FZpkt := '';  // 15.02.2011  WN
end;

{----------------------------------------------------}
procedure TKanalListe.Delete(iIndex: integer);
{----------------------------------------------------}
begin
  case FSpaltenTyp of
    C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_OrdnungsNr,
    C_SpaltenTyp_Zaehlerstand, C_SpaltenTyp_Zaehlwert:
       Dispose(Items[iIndex]);
    C_SpaltenTyp_Messwert, C_SpaltenTyp_DatumZeit : Dispose(Items[iIndex]);
    else begin
      FreeMem(PChar(Items[iIndex]));
    end;
  end;

  inherited Delete(iIndex);
end;

{----------------------------------------------------}
procedure TKanalListe.Clear;
{----------------------------------------------------}
var
  i : integer;
begin
  for i := 0 to Count-1 do
    case FSpaltenTyp of
      C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_OrdnungsNr,
      C_SpaltenTyp_Zaehlerstand, C_SpaltenTyp_Zaehlwert:
         Dispose(Items[i]);
      C_SpaltenTyp_Messwert, C_SpaltenTyp_DatumZeit : Dispose(Items[i]);
      else begin
        FreeMem(PChar(Items[i]));
      end;
    end;

  inherited;
end;

{------------------------------------------------------}
function TKanalListe.AddString(const sValue: string): integer;
{------------------------------------------------------}
begin
  Result := Add(nil);
  Strings[Result] := sValue;
end;

{------------------------------------------------------}
function TKanalListe.AddInteger(const iValue: Int64): integer;
{------------------------------------------------------}
begin
  Result := Add(nil);
  AsInteger[Result] := iValue;
end;

{------------------------------------------------------}
function TKanalListe.InsertString(
  iIndex: integer; const sValue: string): integer;
{------------------------------------------------------}
begin
  Insert(iIndex, nil);
  Result := iIndex;
  Strings[Result] := sValue;
end;

{------------------------------------------------------}
function TKanalListe.GetDefaultMask: string;
{------------------------------------------------------}
begin
  case FSpaltenTyp of
    C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_OrdnungsNr : Result := '#,##0';
    C_SpaltenTyp_Zaehlerstand, C_SpaltenTyp_Zaehlwert : Result := '#,##0';
    C_SpaltenTyp_Messwert     : Result := '#,##0.0000';
    C_SpaltenTyp_DatumZeit    : Result := C_FormatDateTime;
    else Result := '%s';
  end;
end;

{------------------------------------------------------}
function TKanalListe.IndexOf(const sValue: string): integer;
{------------------------------------------------------}
begin
  for Result := 0 to Count - 1 do
    if (AnsiCompareText(Strings[Result], sValue) = 0) then Exit;
  Result := -1;
end;

{------------------------------------------------------}
function TKanalListe.GetString(iIndex: integer): string;
{------------------------------------------------------}
var
  i : Int64;
  f : double;
begin
  if (Assigned(Items[iIndex])) then begin
    case FSpaltenTyp of
      C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_OrdnungsNr,
      C_SpaltenTyp_Zaehlerstand, C_SpaltenTyp_Zaehlwert :
        begin
          i := PInt64(Items[iIndex])^;
          if (FValNoValue <> 0) and (i = FValNoValue)  then  // 11.09.2007
            Result := S_ValueNoValue
          else if (FValNotValid <> 0) and (i = FValNotValid) then
            Result := S_ValueNotValid
          else Result := FormatFloat(FFormatMask, i);
        end;
      C_SpaltenTyp_Messwert :
        begin
          f := PDouble(Items[iIndex])^;
          if (FValNoValue <> 0) and (f = FValNoValue) then  // 11.09.2007
            Result := S_ValueNoValue
          else if (FValNotValid <> 0) and (f = FValNotValid) then
            Result := S_ValueNotValid
          else Result := FormatFloat(FFormatMask, f);
        end;
      C_SpaltenTyp_DatumZeit :
        if (Round(PDouble(Items[iIndex])^) in [101..104]) then begin  // Statistik
          case Round(PDouble(Items[iIndex])^) of
            101 : Result := S_Summe;
            102 : Result := S_Mittelwert;
            103 : Result := S_Minimum;
            104 : Result := S_Maximum;
          end;
        end
        else Result := FormatDateTime(FFormatMask, PDouble(Items[iIndex])^);
      C_SpaltenTyp_ZeitZone :
        Result := Format(FFormatMask, [PChar(Items[iIndex])]);
      C_SpaltenTyp_Status :
        Result := Format(FFormatMask, [PChar(Items[iIndex])]);
      C_SpaltenTyp_Text :
        Result := Format(FFormatMask, [PChar(Items[iIndex])]);
      else Result := Format(FFormatMask, [PChar(Items[iIndex])]);
    end;
  end
  else begin
   case FSpaltenTyp of
      C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_OrdnungsNr,
      C_SpaltenTyp_Zaehlerstand, C_SpaltenTyp_Zaehlwert :
        Result := FormatFloat(FFormatMask, 0);
{      C_SpaltenTyp_Messwert : Result := FormatFloat(FFormatMask, 0);
      C_SpaltenTyp_DatumZeit : Result := FormatDateTime(FFormatMask, 0);

      C_SpaltenTyp_ZeitZone : Result := '';
      C_SpaltenTyp_Status : Result := '';
      C_SpaltenTyp_Text : Result := '';
}      else Result := '';
    end;
  end;
end;

{------------------------------------------------------}
procedure TKanalListe.SetString(iIndex: integer; sValue: string);
{------------------------------------------------------}
var
  pInt : PInt64;
  pd   : PDouble;
  pc   : PChar;
  s    : string;  // 11.09.2007
begin
  case FSpaltenTyp of
    C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_OrdnungsNr,
    C_SpaltenTyp_Zaehlerstand, C_SpaltenTyp_Zaehlwert:
      begin                // 07.10.2003
        New(pInt);
        s := Trim(sValue);
        if (s = '') then s := IntToStr(FValNoValue);  // 11.09.2007
        pInt^ := StrToInt64Def(StringReplace(
          s, ThousandSeparator, '', [rfReplaceAll]), FValNotValid);
        Items[iIndex] := pInt;
      end;
    C_SpaltenTyp_Messwert :
      begin
        s := Trim(sValue);
        if (s = '') then s := IntToStr(FValNoValue);  // 11.09.2007
        if (Assigned(Items[iIndex])) then Dispose(Items[iIndex]);
        if (Trim(sValue) = '')
        then Items[iIndex] := nil
        else begin
          New(pd);
          pd^ := WStrToFloatDef(s, FValNotValid);
          Items[iIndex] := pd;
        end;
      end;
    C_SpaltenTyp_DatumZeit :
      begin
        if (Assigned(Items[iIndex])) then Dispose(Items[iIndex]);
        if (Trim(sValue) = '')
        then Items[iIndex] := nil
        else begin
          New(pd);
          try
            if (sValue = '101') or (sValue = '102') or (sValue = '103') or
             (sValue = '104')  // Statistik-Konstanten
            then pd^ := WStrToFloatDef(sValue, FValNotValid)
            else pd^ := StrToDateTime(sValue);
          except
            pd^ := 0;
          end;
          Items[iIndex] := pd;
        end;
      end;
    else begin
      if (Assigned(Items[iIndex])) then FreeMem(PChar(Items[iIndex]));
      GetMem(pc, Length(sValue)+1);
      StrPCopy(pc, sValue);
      Items[iIndex] := pc;
    end;
  end;
end;

{------------------------------------------------------}
function TKanalListe.GetDateTime(iIndex: integer): TDateTime;
{------------------------------------------------------}
begin
  if (Assigned(Items[iIndex])) then begin
    case FSpaltenTyp of
      C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_OrdnungsNr,
      C_SpaltenTyp_Zaehlerstand, C_SpaltenTyp_Zaehlwert :
        Result := PInt64(Items[iIndex])^;
      C_SpaltenTyp_Messwert :  Result := PDouble(Items[iIndex])^;
      C_SpaltenTyp_DatumZeit : Result := PDouble(Items[iIndex])^;

      C_SpaltenTyp_ZeitZone :  Result := 0;
      C_SpaltenTyp_Status :    Result := 0;
      C_SpaltenTyp_Text :      Result := 0;
      else Result := 0;
    end;
  end
  else Result := 0;
end;

{------------------------------------------------------}
procedure TKanalListe.SetDateTime(iIndex: integer; dtValue: TDateTime);
{------------------------------------------------------}
var
  pInt : PInt64;
  pd   : PDouble;
  pc   : PChar;
  sV   : string;
begin
  case FSpaltenTyp of
    C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_OrdnungsNr,
    C_SpaltenTyp_Zaehlerstand, C_SpaltenTyp_Zaehlwert :
      begin
        if (Assigned(Items[iIndex])) then Dispose(Items[iIndex]);
        New(pInt);
        pInt^ := Round(dtValue);
        Items[iIndex] := pInt;
      end;
    C_SpaltenTyp_Messwert :
      begin
        if (Assigned(Items[iIndex])) then Dispose(Items[iIndex]);
        New(pd);
        pd^ := dtValue;
        Items[iIndex] := pd;
      end;
    C_SpaltenTyp_DatumZeit :
      begin
        if (Assigned(Items[iIndex])) then Dispose(Items[iIndex]);
        New(pd);
        pd^ := dtValue;
        Items[iIndex] := pd;
      end;
    else begin
      if (Assigned(Items[iIndex])) then FreeMem(PChar(Items[iIndex]));

      try
        sV := FormatDateTime(FFormatMask, dtValue);
      except
        sV := DateTimeToStr(dtValue);
      end;
      GetMem(pc, Length(sV)+1);
      StrPCopy(pc, sV);
      Items[iIndex] := pc;
    end;
  end;
end;

{------------------------------------------------------}
function TKanalListe.GetFloat(iIndex: integer): double;
{------------------------------------------------------}
begin
  if (Assigned(Items[iIndex])) then begin
    case FSpaltenTyp of
      C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_OrdnungsNr,
      C_SpaltenTyp_Zaehlerstand, C_SpaltenTyp_Zaehlwert :
        Result := PInt64(Items[iIndex])^;
      C_SpaltenTyp_Messwert :  Result := PDouble(Items[iIndex])^;
      C_SpaltenTyp_DatumZeit : Result := PDouble(Items[iIndex])^;

      C_SpaltenTyp_ZeitZone :  Result := 0;
      C_SpaltenTyp_Status :    Result := 0;
      C_SpaltenTyp_Text :
        begin
          Result := 0;
          if (Assigned(Items[iIndex])) then
            Result := WStrToFloatDef(PChar(Items[iIndex]), 0);
        end;
      else begin
        Result := 0;
        if (Assigned(Items[iIndex])) then
          Result := WStrToFloatDef(PChar(Items[iIndex]), 0);
      end;
    end;
  end
  else Result := 0;
end;

{------------------------------------------------------}
procedure TKanalListe.SetFloat(iIndex: integer; fValue: double);
{------------------------------------------------------}
var
  pInt : PInt64;
  pd   : PDouble;
  pc   : PChar;
  sV   : string;
begin
  case FSpaltenTyp of
    C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_OrdnungsNr,
    C_SpaltenTyp_Zaehlerstand, C_SpaltenTyp_Zaehlwert :
      begin
        if (Assigned(Items[iIndex])) then Dispose(Items[iIndex]);
        New(pInt);
        pInt^ := Round(fValue);
        Items[iIndex] := pInt;
      end;
    C_SpaltenTyp_Messwert :
      begin
        if (Assigned(Items[iIndex])) then Dispose(Items[iIndex]);
        New(pd);
        pd^ := fValue;
        Items[iIndex] := pd;
      end;
    C_SpaltenTyp_DatumZeit :
      begin
        if (Assigned(Items[iIndex])) then Dispose(Items[iIndex]);
        New(pd);
        pd^ := fValue;
        Items[iIndex] := pd;
      end;
    else begin
      if (Assigned(Items[iIndex])) then FreeMem(PChar(Items[iIndex]));
      try
        sV := FormatFloat(FFormatMask, fValue);
      except
        sV := FloatToStr(fValue);
      end;
      GetMem(pc, Length(sV)+1);
      StrPCopy(pc, sV);
      Items[iIndex] := pc;
    end;
  end;
end;

{------------------------------------------------------}
function TKanalListe.GetInteger(iIndex: integer): Int64;
{------------------------------------------------------}
var
  iCode : integer;
begin
  if (Assigned(Items[iIndex])) then begin
    case FSpaltenTyp of
      C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_OrdnungsNr,
      C_SpaltenTyp_Zaehlerstand, C_SpaltenTyp_Zaehlwert :
        Result := PInt64(Items[iIndex])^;
      C_SpaltenTyp_Messwert :  Result := Round(PDouble(Items[iIndex])^);
      C_SpaltenTyp_DatumZeit : Result := Round(PDouble(Items[iIndex])^);

      C_SpaltenTyp_ZeitZone :  Result := 0;
      C_SpaltenTyp_Status :    Result := 0;
      C_SpaltenTyp_Text :
        begin
          Result := 0;
          if (Assigned(Items[iIndex])) then
            Val(PChar(Items[iIndex]), Result, iCode);
        end;
      else begin
        Result := 0;
        if (Assigned(Items[iIndex])) then
          Val(PChar(Items[iIndex]), Result, iCode);
      end;

    end;
  end
  else Result := 0;
end;

{------------------------------------------------------}
procedure TKanalListe.SetInteger(iIndex: integer; iValue: Int64);
{------------------------------------------------------}
var
  pInt : PInt64;
  pd   : PDouble;
  pc   : PChar;
  sV   : string;
begin
  case FSpaltenTyp of
    C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_OrdnungsNr,
    C_SpaltenTyp_Zaehlerstand, C_SpaltenTyp_Zaehlwert :
      begin
        if (Assigned(Items[iIndex])) then Dispose(Items[iIndex]);
        New(pInt);
        pInt^ := iValue;
        Items[iIndex] := pInt;
      end;
    C_SpaltenTyp_Messwert :
      begin
        if (Assigned(Items[iIndex])) then Dispose(Items[iIndex]);
        New(pd);
        pd^ := iValue;
        Items[iIndex] := pd;
      end;
    C_SpaltenTyp_DatumZeit :
      begin
        if (Assigned(Items[iIndex])) then Dispose(Items[iIndex]);
        New(pd);
        pd^ := iValue;
        Items[iIndex] := pd;
      end;
    else begin
      if (Assigned(Items[iIndex])) then FreeMem(PChar(Items[iIndex]));
      try
        sV := FormatFloat(FFormatMask, iValue);
      except
        sV := IntToStr(iValue);
      end;
      GetMem(pc, Length(sV)+1);
      StrPCopy(pc, sV);
      Items[iIndex] := pc;
    end;
  end;
end;

{------------------------------------------------------}
procedure TKanalListe.Assign(pList: TKanalListe);
{------------------------------------------------------}
var
  i : integer;
begin
  if (Assigned(pList)) then begin
    FSpaltenTyp := pList.SpaltenTyp;
    FKontroll   := pList.Kontroll;
    FFormatMask := pList.FFormatMask;
    FUnit       := pList.FUnit;  // 15.02.2011  WN
    FObis       := pList.FObis;  // 15.02.2011  WN
    FZpkt       := pList.FZpkt;  // 15.02.2011  WN

    for i := 0 to pList.Count-1 do AddString(pList.Strings[i]);
  end;
end;

function TKanalListe.GetStatistic(
  var sSumme, sAverage, sMin, sMax: string): boolean;
var
  i : integer;
  fFirst, fLast, fNow, fSumme, fMin, fMax : double;
begin
  sSumme := '';
  sAverage := '';
  sMin := '';
  sMax := '';
  try
    if (Count > 1) and (SpaltenTyp in
      [C_SpaltenTyp_Zaehlerstand, C_SpaltenTyp_Zaehlwert, C_SpaltenTyp_Messwert])
    then begin
      fLast := 0;
      fMin := fLast;
      fMax := fLast;
      fSumme := fLast;
      fFirst := fLast;
      for i := 0 to Count-1 do begin
        if (i = 0) then begin
          fLast := AsFloat[0];
          fMin := fLast;
          fMax := fLast;
          fSumme := fLast;
          fFirst := fLast;
        end
        else begin
          fNow := AsFloat[i];
          if (SpaltenTyp = C_SpaltenTyp_Zaehlerstand) then begin
            fSumme := fNow - fLast;
            if (fSumme >= 0) then begin
              if (i = 1) then begin
                fMin := fSumme;
                fMax := fSumme;
              end
              else begin
                if (fSumme < fMin) then fMin := fSumme;
                if (fSumme > fMax) then fMax := fSumme;
              end;
              if (i = Count-1) then fSumme := fLast - fFirst;
            end;
          end
          else begin
            fSumme := fSumme + fNow;
            if (fNow < fMin) then fMin := fNow;
            if (fNow > fMax) then fMax := fNow;
            if (i = Count-1) then fSumme := fSumme / Count;
           end;
          fLast := fNow;
        end;
      end;

      if (SpaltenTyp = C_SpaltenTyp_Zaehlerstand)
      then sSumme := FormatFloat(FFormatMask, fSumme)
      else sAverage := FormatFloat(FFormatMask, fSumme);
      sMin := FormatFloat(FFormatMask, fMin);
      sMax := FormatFloat(FFormatMask, fMax);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

{-------------------------------- TDSfGAbrufDaten -----------------------------}

{------------------------------------------------------}
constructor TAbrufDaten.Create(sDatabaseName: string; iGerId: integer);
{------------------------------------------------------}
begin
  inherited Create;

  FDatabaseName := sDatabaseName;
  FGeraeteId := iGerId;
  MWFormat := '#,##0.0000';  // Vorgabe für Messwert-Format
  ZSFormat := '#,##0';  // Vorgabe für Zählerstand-Format
  FExportCaption := '';
  FDtVon := 0;
  FDtBis := 0;
  FDtOldVon := 0;
  FDtOldBis := 0;
  FMaxRecords := 100;
  FStundenWerte := False;
  FStatistics := False;
  FStatusZahl := False;  // Status als Text
  FIndexDatumZeit := C_Index_DatumZeit;
  FStatusListe := TStatusListe.Create;
end;

{------------------------------------------------------}
destructor TAbrufDaten.Destroy;
{------------------------------------------------------}
begin
  Clear;
  FStatusListe.Free;

  inherited;
end;

{------------------------------------------------------}
procedure TAbrufDaten.Delete(iIndex: integer);
{------------------------------------------------------}
begin
  if (Assigned(Objects[iIndex])) then Objects[iIndex].Free;

  inherited;
end;

{------------------------------------------------------}
procedure TAbrufDaten.Clear;
{------------------------------------------------------}
var
  i : integer;
begin
  FDtVon    := 0;
  FDtBis    := 0;
  FDtOldVon := 0;
  FDtOldBis := 0;

  for i := Count-1 downto 0 do if (Assigned(Objects[i])) then Objects[i].Free;

  inherited;
end;

procedure TAbrufDaten.CreateStatistic;
var
  i, j : integer;
  sSumme, sAverage, sMin, sMax : string;
begin
  // Alle Spalten auffüllen
  for i := 1 to Count-1 do
    for j := GetCol(i).Count to GetCol(0).Count-1 do
      GetCol(i).AddString('');
  // Statistikzeilen in Spalte 1
  GetCol(0).AddString('101');
  GetCol(0).AddString('102');
  GetCol(0).AddString('103');
  GetCol(0).AddString('104');
  // Statistikzeilen in allen anderen Spalten
  for i := 1 to Count-1 do begin
    GetCol(i).GetStatistic(sSumme, sAverage, sMin, sMax);
    GetCol(i).AddString(sSumme);
    GetCol(i).AddString(sAverage);
    GetCol(i).AddString(sMin);
    GetCol(i).AddString(sMax);
  end;
end;

{ Exportiert die Abrufdaten in akt. Excel-Arbeitsmappe }
{ Parameter: Optionaler Name für Excel-Mappe           }
{ Rückgabe: Erfolg ja/nein                             }
{------------------------------------------------------}
function TAbrufDaten.ExportToExcel(sBookName: string = ''): boolean;
{------------------------------------------------------}
var
  vExcel, vWorkBook, vWorkSheet : Variant;
  i, j, iRow, iR, iC, iDelay    : integer;
  s, sName, sCell, sRange, sMask, sDateTime: string;
  oldCursor                     : TCursor;
  pForm                         : TComInfoWindow;
  bStatistic                    : boolean;
  fStart, fEnd, fSumme, fMin, fMax, fLast, fNow : double;
begin
  Result := False;
  bStatistic := FStatistics;
  try
    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;

    pForm := TComInfoWindow.CreateInfoWindow(S_AbrufDaten_Export1);
    try

      pForm.Show;
      if (Count > 0) and (GetCol(0).Count > 0) then begin
        if (Assigned(pForm)) then begin
          pForm.SetInfoText(Format(S_AbrufDaten_Export2, [GetCol(0).Count]));
          pForm.SetInfoCaption(
            Format(S_AbrufDaten_Export3, [GetCol(0).Count, Count]));
        end;

        iRow := 1;
        iC := 1;
        iDelay := 0;

        try
          try
            if (sBookName = '') then
            try
              vExcel := GetActiveOleObject('Excel.Application');
              vWorkBook := vExcel.WorkBooks[vExcel.WorkBooks.Count];
              vWorkSheet := vWorkBook.Worksheets.Add;
            except
              vExcel := CreateOleObject('Excel.Application');
              vWorkBook := vExcel.WorkBooks.Add;
              vExcel.DisplayAlerts := False;
              for i := vWorkBook.Worksheets.Count downto 2 do
                vWorkBook.Worksheets[i].Delete;
              vWorkSheet := vWorkBook.WorkSheets[1];
            end
            else begin
              vExcel := CreateOleObject('Excel.Application');
              vWorkBook := vExcel.WorkBooks.Add;
              vExcel.DisplayAlerts := False;
              vWorkBook.SaveAs(FileName := sBookName);
              for i := vWorkBook.Worksheets.Count downto 2 do
                vWorkBook.Worksheets[i].Delete;
              vWorkSheet := vWorkBook.WorkSheets[1];
            end;
          except
            Exit;
          end;
          vExcel.DisplayAlerts := True;

          // Ggf. Überschrift behandeln
          if (FExportCaption <> '') then begin
            // Als Überschrift einfügen
            vWorkSheet.Cells[1, 1].Value := FExportCaption;
            Inc(iROw);

            // Überprüfen, ob Name bereits existiert
            s := '';  // Flag, das Name eindeutig ist
            j := 1;
            sName := FilterCharsFromString(FExportCaption);
            sName := StringReplace(sName, '[', '', [rfReplaceAll]);
            sName := StringReplace(sName, ']', '', [rfReplaceAll]);
            while (s = '') do begin
              s := sName;
              if (j > 1) then s := '(' + IntToStr(j) + ') ' + s;
              if (Length(s) > 31) then s := Copy(s, 1, 28) + '...';
              for i := 1 to vWorkBook.WorkSheets.Count do
                if (s = vWorkBook.WorkSheets[i].Name) then begin
                  Inc(j);
                  s := '';
                  Break;
                end;
            end;
            vWorkSheet.Name := s;
          end;

          // Spaltenüberschriften
          sCell := 'A' + IntToStr(iRow) + ':';
          if (Count < 27)
          then sCell := sCell + Chr(Ord('A') - 1 + Count) + IntToStr(iRow)
          else sCell := sCell + Chr(Ord('A') - 1 + ((Count-1) div 26)) +
              Chr(Ord('A') + ((Count-1) mod 26)) + IntToStr(iRow);
          vWorkSheet.Range[sCell].NumberFormat := '@';  // Reihe als TEXT
          for i := 0 to Count-1 do
            vWorkSheet.Cells[iRow, iC+i].Value := Strings[i];
          Inc(iRow);

          // Kanäle seriell abarbeiten
          for i := 0 to Count-1 do with GetCol(i) do begin
            iR := iRow;
            iC := i+1;
            // Spalte im richtigen Format formatieren
            case SpaltenTyp of  // Formatmaske bestimmen
              C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_Zaehlerstand,
              C_SpaltenTyp_Zaehlwert, C_SpaltenTyp_OrdnungsNr,
              C_SpaltenTyp_Messwert :
                begin
                   sMask := StringReplace(Mask, '.', 'X', [rfReplaceAll]);
                   sMask := StringReplace(sMask, ',', 'Y', [rfReplaceAll]);
                   sMask := StringReplace(
                     sMask, 'X', DecimalSeparator, [rfReplaceAll]);
                   sMask := StringReplace(
                     sMask, 'Y', ThousandSeparator, [rfReplaceAll]);
                end;
              C_SpaltenTyp_DatumZeit :
                begin
                  sMask := '';
//                  sMask := StringReplace(Mask, '.', '/', [rfReplaceAll]);
//                  sMask := StringReplace(sMask, 'n', 'm', [rfReplaceAll]);
                end;
              else sMask := '@';
            end;
            if (sMask <> '') then begin
              if (iC < 27) then begin
                sCell := Chr(Ord('A') - 1 + iC) + IntToStr(iR) + ':';
                sCell := sCell + Chr(Ord('A') - 1 + iC) +
                  IntToStr(iR + Count-1 + i);
              end
              else begin
                sCell := Chr(Ord('A') - 1 + ((iC-1) div 26)) +
                  Chr(Ord('A') + ((iC-1) mod 26)) + IntToStr(iR) + ':';
                sCell := sCell + Chr(Ord('A') - 1 + ((iC-1) div 26)) +
                  Chr(Ord('A') + ((iC-1) mod 26)) + IntToStr(iR + Count-1 + i);
              end;
              vWorkSheet.Range[sCell].NumberFormat := sMask;
            end;

            fmin := -9999;
            fmax := -9999;
            fSumme := 0;
            fStart := 0;
            fEnd := 0;
            fLast := 0;
            for j := 0 to Count-1 do begin
              if (bStatistic) then begin
                if (SpaltenTyp = C_SpaltenTyp_Zaehlerstand) then begin
                  if (j = 0) then begin
                    fStart := AsFloat[j];
                    fLast := fStart;
                  end
                  else begin
                    fNow := AsFloat[j];
                    if (j = Count-1) then fEnd := fNow;
                    fSumme := fNow - fLast;
                    if (fSumme >= 0) then begin
                      if (fMin = -9999) or (fMin > fSumme) then fMin := fSumme;
                      if (fMax = -9999) or (fMax < fSumme) then fMax := fSumme;
                    end;
                    fLast := fNow;
                  end;
                end
                else if (SpaltenTyp in
                  [C_SpaltenTyp_Zaehlwert, C_SpaltenTyp_Messwert]) then
                begin
                  fNow := AsFloat[j];
                  if (j = 0) then fSumme := fNow
                  else fSumme := fSumme + fNow;
                  if (fMin = -9999) or (fMin > fNow) then fMin := fNow;
                  if (fMax = -9999) or (fMax < fNow) then fMax := fNow;
                end
              end;

              if (Assigned(pForm)) then pForm.SetInfoText(
                Format(S_AbrufDaten_Export4, [j+1, Count, i+1, Self.Count]));

              // Wert eintragen
              sCell := Chr(Ord('A') - 1 + iC) + IntToStr(iR);
              case SpaltenTyp of
                C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_Zaehlerstand,
                C_SpaltenTyp_Zaehlwert, C_SpaltenTyp_OrdnungsNr,
                C_SpaltenTyp_Messwert :
                  vWorkSheet.Cells[iR, iC].Value := AsFloat[j];
                // 07.06.2011  WN
                C_SpaltenTyp_DatumZeit : begin
                  sDateTime := FormatDateTime(C_FormatDateTime, AsDateTime[j]);
                  sDateTime := StringReplace(sDateTime, '/', '.', [rfReplaceAll]);
                  vWorkSheet.Cells[iR, iC].Value := sDateTime;
                end;
                else vWorkSheet.Cells[iR, iC].Value := Strings[j];
              end;

              if (bStatistic) then begin
                // Letzte Zeile ermiteln und Statistik anhängen
                if (i = 0) then begin
                  vWorkSheet.Cells[iRow + GetCol(0).Count + 1, iC].Value :=
                    S_Summe;
                  vWorkSheet.Cells[iRow + GetCol(0).Count + 2, iC].Value :=
                    S_Mittelwert;
                  vWorkSheet.Cells[iRow + GetCol(0).Count + 3, iC].Value :=
                    S_Minimum;
                  vWorkSheet.Cells[iRow + GetCol(0).Count + 4, iC].Value :=
                    S_Maximum;
                end
                else begin
                // ZS-Differenz
                  if (SpaltenTyp = C_SpaltenTyp_Zaehlerstand) then
                    vWorkSheet.Cells[iRow + GetCol(0).Count + 1, iC].Value :=
                      fEnd - fStart;
                  // Mittelwert
                  if (SpaltenTyp in
                      [C_SpaltenTyp_Zaehlwert, C_SpaltenTyp_Messwert]) and
                     (GetCol(i).Count > 0)
                  then vWorkSheet.Cells[iRow + GetCol(0).Count + 2, iC].Value :=
                      fSumme / GetCol(i).Count;
                  // Minimum
                  if (SpaltenTyp in [C_SpaltenTyp_Zaehlerstand,
                    C_SpaltenTyp_Zaehlwert, C_SpaltenTyp_Messwert])
                  then vWorkSheet.Cells[iRow + GetCol(0).Count + 3, iC].Value := fMin;
                  // Maximum
                  if (SpaltenTyp in [C_SpaltenTyp_Zaehlerstand,
                    C_SpaltenTyp_Zaehlwert, C_SpaltenTyp_Messwert])
                  then vWorkSheet.Cells[iRow + GetCol(0).Count + 4, iC].Value := fMax;
                end;
              end;

	            Inc(iDelay);
              if ((iDelay mod 5) = 0) then Sleep(1);
              if ((iDelay mod 50) = 0) then Application.ProcessMessages;
              if (Assigned(pForm)) and (pForm.Stoped) then Break;

              Inc(iR);
            end;

          end;

          // Excel anzeigen und Spaltenbreiten anpassen
          sRange := 'A' + IntToStr(iRow) + ':';
          if (Count < 27)
          then sRange := sRange + Chr(Ord('A') - 1 + Count) + IntToStr(GetCol(0).Count)
          else sRange := sRange + Chr(Ord('A') - 1 + ((iC-1) div 26)) +
              Chr(Ord('A') + ((iC-1) mod 26)) + IntToStr(GetCol(0).Count);
          vWorkSheet.Range[sRange].Columns.AutoFit;

          if (sBookName <> '') then begin
            vExcel.DisplayAlerts := False;
            vWorkBook.SaveAs(FileName := sBookName);
            vExcel.Quit;
          end
          else vExcel.Visible := True;
          Result := True;
        except
        // Fehlermeldung unterdrücken; Result ist bereits 'False'
        end;

      end;

    finally
      if (Assigned(pForm)) then pForm.Free;
      Screen.Cursor := oldCursor;
    end;
  except
    on E:Exception do
      MessageDlg(S_ExportErr + #13#10 + E.Message, mtError, [mbOk], 0);
  end;
end;

{ Fügt neuen Eintrag (= Spalte) hinzu                  }
{ Parameter: Name der Spalte, Kanaltyp, Anzeigemaske   }
{ Rückgabe: Index der Spalte                           }
{------------------------------------------------------}
function TAbrufDaten.AddCol(
  sName: string; iTyp: byte = 0; sMask: string = ''): integer;
{------------------------------------------------------}
begin
  if (sMask = '') then begin
    if (iTyp in [C_SpaltenTyp_Messwert])
    then sMask := MWFormat
    else if (iTyp in [C_SpaltenTyp_ReferenzNr, C_SpaltenTyp_OrdnungsNr,
                      C_SpaltenTyp_Zaehlerstand, C_SpaltenTyp_Zaehlwert])
    then sMask := ZSFormat;
  end;
  Result := AddObject(sName, TKanalListe.Create(iTyp, sMask));
end;

{ Gibt Stringliste (= Spalte) zum Namen zurück         }
{ Parameter: Name der Spalte                           }
{ Rückgabe: Spalte oder nil                            }
{------------------------------------------------------}
function TAbrufDaten.GetCol(sName: string): TKanalListe;
{------------------------------------------------------}
var
  i, iIndex : integer;
begin
  Result := nil;  // Default

  iIndex := IndexOf(sName);
  if (iIndex < 0) then
    for i := 0 to Count-1 do
      if (UpperCase(sName) = UpperCase(Strings[i])) then begin
        iIndex := i;
        Break;
      end;

  if (iIndex >= 0) and (Assigned(Objects[iIndex])) then
    Result := TKanalListe(Objects[iIndex]);
end;

{ Gibt Stringliste (= Spalte) zum Index zurück         }
{ Parameter: Index der Spalte                          }
{ Rückgabe: Spalte oder nil                            }
{------------------------------------------------------}
function TAbrufDaten.GetCol(iIndex: integer): TKanalListe;
{------------------------------------------------------}
begin
  if (iIndex >= 0) and (Assigned(Objects[iIndex]))
  then Result := TKanalListe(Objects[iIndex])
  else Result := nil;
end;

{ Lädt Daten für den Archivgruppen-Index               }
{------------------------------------------------------}
procedure TAbrufDaten.LoadData(dtVon: TDateTime = 0; dtBis: TDateTime = 0);
{------------------------------------------------------}
begin
  AutoDelete;
end;

{ Entfernt überzählige Daten, passt Parameter an       }
{------------------------------------------------------}
procedure TAbrufDaten.AutoDelete;
{------------------------------------------------------}
var
  i, j : integer;
begin
  if (FMaxRecords > 0) then  // <= 0 heißt: keine Begrenzung
    if (Count > 0) and (GetCol(0).Count > FMaxRecords) then begin
      while (GetCol(0).Count > FMaxRecords) and
            (GetCol(FIndexDatumZeit).AsDateTime[0] < FDtVon)
      do for i := 0 to Count-1 do GetCol(i).Delete(0);

      while (GetCol(0).Count > FMaxRecords) and
            (GetCol(FIndexDatumZeit).AsDateTime[GetCol(0).Count-1] > FDtBis)
      do begin
        j := GetCol(0).Count-1;
        for i := 0 to Count-1 do GetCol(i).Delete(j);
      end;
    end;
end;

end.
