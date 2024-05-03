{******************************************************************************}
{* Unit: Datensatzliste für MRG-Kommunikation mit ACK01-Protokoll             *}
{* 09.01.2000 WW                                                              *}
{* 23.06.2003 WW  Erweiterungen für Abruf von KE-Anlagen                      *}
{******************************************************************************}
unit MDatensatzList;

interface

uses
  Classes, Forms, SysUtils, WChars;

type
  { ACK01-Kommunikationsmodi }

  TACK01CommModus = (
    cm_MRG,                       { Kommunikation mit MRG }
    cm_KE);                       { Kommunikation mit KE-Anlage }


  { Objekt für TDatensatzList }

  TDatensatzListObj = class(TObject)
    Anz: integer;         { Gesamtanzahl an Datenblöcken }
    Num: integer;         { Nummer des Datensatzes }
    Wdh: integer;         { Wiederholungszähler }
    Data: string;         { Daten }
    MRGBefehl: boolean;   { Flag: true, wenn Data einen MRG-Befehlsblock enthält }
  public
    procedure SetData (AnAnz: integer; ANum: integer; AWdh: integer; AData: string;
                       AMRGBefehl: boolean);
    function GetProtokoll (ACK01CommModus: TACK01CommModus): string;
  end;

  { Datensatzliste }

  TDatensatzList = class(TList)
  private
    ACK01CommModus: TACK01CommModus;
    BlockLaenge: integer;
    procedure ClearAll;
  public
    BlockCount: integer;
    constructor Create (AACK01CommModus: TACK01CommModus; ABlockLaenge: integer);
    destructor Destroy; override;
    procedure InsertDaten (Daten: string; isMRGBefehl: boolean);
    function GetDatensatz (var Datensatz: string; var Wdh: integer): boolean;
  end;

implementation

{ TDatensatzListObj }

{--------------------------------------------------------------------------------}
procedure TDatensatzListObj.SetData (AnAnz: integer; ANum: integer; AWdh: integer;
                                     AData: string; AMRGBefehl: boolean);
{--------------------------------------------------------------------------------}
begin
  Anz:=AnAnz;
  Num:=ANum;
  Wdh:=AWdh;
  Data:=AData;
  MRGBefehl:=AMRGBefehl;
end;

{--------------------------------------------------------------------------------}
function TDatensatzListObj.GetProtokoll (ACK01CommModus: TACK01CommModus): string;
{--------------------------------------------------------------------------------}
{ liefert als Ergebnis Protokoll für Datensatz;
  Übergabe: ACK01-Kommunikationsmodus }
var
  BCC: byte;
  i : integer;
  S: string;

begin
  if MRGBefehl then begin
    if ACK01CommModus = cm_MRG then
      S:='*'     { MRG-Kommunikation: mit vorangestelltem * }
    else
      S:='';
    S:=S +
       SOH +                               { Protokoll beginnt mit SOH }
       Format ('%.3d', [Num]) +            { Nummer des Datenblocks, 3-stellig }
       Format ('%.4d', [length (Data)]) +  { Länge des Datenblocks, 4-stellig }
       Format ('%.1d', [Wdh]) +            { Wiederholungszähler, 1-stellig }
       STX +                               { Datensatz beginnt mit STX }
       Data;                               { zu sendende Daten }

    if ACK01CommModus = cm_MRG then begin  { MRG-Kommunikation }
      if Num < Anz then                    { Datensatz endet mit ETB bzw. ETX (letzter Satz) }
        S:=S + ETB
      else
        S:=S + ETX;
    end else                               { KE-Kommunikation }
      S:=S + ETX;

    BCC:=0;
    if ACK01CommModus = cm_MRG then begin   { MRG-Kommunikation }
      for i:=3 to length (S) do             { ab Num bis Endezeichen (ETX, ETB) }
        BCC:=BCC XOR byte (S [i]);
    end
    else begin                              { KE-Kommunikation }
      for i:=1 to length (S) do             { ab SOH bis Endezeichen (ETX, ETB) }
        BCC:=BCC + (byte (S [i]) MOD 2);
    end;
    S:=S +
       char (((BCC AND $F0) SHR 4) OR $20) +   { BCC1 enthält XOR der oberen 4 Bits }
       char ((BCC AND $0F) OR $20) +           { BCC2 enthält XOR der unteren 4 Bits }
       CR;
    Result:=S;
  end else
    Result:=Data;
end;


{ TDatensatzList }

{------------------------------------------------------------------}
constructor TDatensatzList.Create (AACK01CommModus: TACK01CommModus;
                                   ABlockLaenge: integer);
{------------------------------------------------------------------}
{ Create: setzen des ACK01-Kommunikationsmodus und der Datenblock-Länge, initialisieren
          des Block-Zählers }
Begin
  inherited Create;
  ACK01CommModus:=AACK01CommModus;
  BlockLaenge:=ABlockLaenge;
  BlockCount:=0;
End;

{--------------------------------}
Destructor TDatensatzList.Destroy;
{--------------------------------}
Begin
  ClearAll;
  inherited Destroy;
end;

{--------------------------------}
procedure TDatensatzList.ClearAll;
{--------------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    TDatensatzListObj (Items [i]).Free;
  Clear;
  BlockCount:=0;
end;

{-------------------------------------------------------------------------}
Procedure TDatensatzList.InsertDaten (Daten: string; isMRGBefehl: boolean);
{-------------------------------------------------------------------------}
{ InsertDaten: Zerlegt Befehls-Datenstring in Datenblöcke und fügt diese in Liste ein }
Var
  DataBlock: string;
  AnzBlocks: integer;
  i: integer;
  DatensatzListObj: TDatensatzListObj;
  Num_von: integer;
  Num_bis: integer;

Begin
  ClearAll;
  if isMRGBefehl then
    Daten:=Copy (Daten, 2, length (Daten) - 2);            { ohne STX und ETX }

  { Anzahl der Datenblöcke ergibt sich aus der Größe der Daten und der maximalen
    Blocklänge: }
  AnzBlocks:=length (Daten) DIV BlockLaenge;
  if (length (Daten) MOD BlockLaenge) <> 0 then
    inc (AnzBlocks);

  if ACK01CommModus = cm_MRG then begin  { MRG-Kommunikation }
    Num_von:=1;             { Num beginnt bei 1 }
    Num_bis:=AnzBlocks;
  end
  else begin                             { KE-Kommunikation }
    Num_von:=0;             { Num beginnt bei 0 }
    Num_bis:=AnzBlocks - 1;
  end;

  for i:=Num_von to Num_bis do begin
    DataBlock:=Copy (Daten, 1+(i-1)*BlockLaenge, BlockLaenge);
    DatensatzListObj:=TDatensatzListObj.Create;
    DatensatzListObj.SetData (AnzBlocks, i, 0, DataBlock, isMRGBefehl);
    { Anm.: Für KE-Kommunikation müßte lt. Quellcode des für die Entwicklung
            zugrundegelegten RMG-Abrufprogrammes 'GetDbk' das i (Num im Datenblock)
            bei jedem an die Station gesendeten Befehl hochgezählt werden.
            Es funktioniert aber bislang auch so... }

    Add (DatensatzListObj);
  end;
end;

{--------------------------------------------------------------------------------------}
function TDatensatzList.GetDatensatz (var Datensatz: string; var Wdh: integer): boolean;
{--------------------------------------------------------------------------------------}
{ liefert über BlockCount den nächsten Datenblock;
  Rückgabe: Datenblock (Sendebefehl)
            Anzahl der Wiederholungen für Datenblock
  Ergebnis: true, wenn noch nicht alle Datenblöcke geliefert wurden }
begin
  if (Count >= 0) AND (BlockCount < Count) then begin
    Datensatz:=TDatensatzListObj (Items [BlockCount]).GetProtokoll (ACK01CommModus);
    Wdh:=TDatensatzListObj (Items [BlockCount]).Wdh;

    inc (TDatensatzListObj (Items [BlockCount]).Wdh);  { Wdh inkrementieren, falls Block
                                                         wiederholt werden muß }
    Result:=true;
  end
  else begin
    Datensatz:='';
    Wdh:=0;
    Result:=false;
  end;
end;


end.

