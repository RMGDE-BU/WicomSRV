{******************************************************************************}
{*                                                                            *}
{* ACHTUNG: Spezielle "Importunit" OHNE Zugriff auf ERRTXT32.DLL !            *}
{*                                                                            *}
{* Version: 24.08.2018  WW  Direktes Einbinden der Errtxt.pas-Funktionen für  *}
{*                          WicomSrv (Funktionsaufruf GetGasXStatus aus       *}
{*                          Errtxt32.dll liefert in Gas-X-Version bei OGE mit *}
{*                          sehr vielen parallelen IP-Linien Exceptions "Un-  *}
{*                          gültige Zeigeroperation", "Zugriffsverletzung bei *}
{*                          Adresse..." -> DLL-Zugriff offensichtlich nicht   *}
{*                          Thread-fähig, problematisch in Multi-Threading-   *}
{*                          Programmen mit sehr vielen Threads)               *}
{******************************************************************************}
Unit ErrPrc32;

INTERFACE

Uses
  Errtxt;


function GetStatusText (Status: integer): shortstring;
function GetErrorText (Status, Error: integer): shortstring;
function GetFupErrorcode(FupMeldung: shortstring): integer;
// function GetKlasseText (Klasse: integer): shortstring;
// function GetErrorKlasse (Status, Error: integer): integer;
function GetDSfGKonvErrorText (Status: integer): shortstring;
function GetGasXStatus(Status, Error: integer; Kommando: char): integer;
function GetErrorcode_basic (Status, Error: integer): integer;

IMPLEMENTATION

{------------------------------------------------------------------------------}
function GetStatusText (Status: integer): shortstring;
{------------------------------------------------------------------------------}
begin
  Result := Errtxt.GetStatusText (Status);
end;

{------------------------------------------------------------------------------}
function GetErrorText (Status, Error: integer): shortstring;
{------------------------------------------------------------------------------}
begin
  Result := Errtxt.GetErrorText (Status, Error);
end;

{------------------------------------------------------------------------------}
function GetFupErrorcode(FupMeldung: shortstring): integer;
{------------------------------------------------------------------------------}
begin
  Result := Errtxt.GetFupErrorcode (FupMeldung);
end;

{------------------------------------------------------------------------------}
function GetKlasseText (Klasse: integer): shortstring;
{------------------------------------------------------------------------------}
begin
  Result := Errtxt.GetKlasseText (Klasse);
end;

{------------------------------------------------------------------------------}
function GetErrorKlasse (Status, Error: integer): integer;
{------------------------------------------------------------------------------}
begin
  Result := Errtxt.GetErrorKlasse (Status, Error);
end;

{------------------------------------------------------------------------------}
function GetDSfGKonvErrorText (Status: integer): shortstring;
{------------------------------------------------------------------------------}
begin
  Result := Errtxt.GetDSfGKonvErrorText (Status);
end;

{------------------------------------------------------------------------------}
function GetGasXStatus(Status, Error: integer; Kommando: char): integer;
{------------------------------------------------------------------------------}
begin
  Result := Errtxt.GetGasXStatus(Status, Error, Kommando);
end;

{------------------------------------------------------------------------------}
function GetErrorcode_basic (Status, Error: integer): integer;
{------------------------------------------------------------------------------}
begin
  Result := Errtxt.GetErrorcode_basic (Status, Error);
end;

End.
