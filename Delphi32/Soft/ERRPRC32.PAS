{******************************************************************************}
{* Unit: Importunit für ERRTXT32.DLL (Fehlertexte)                            *}
{* Version: 20.11.98  WW                                                      *}
{*          07.02.00  WW  Funktionsergebnisse als ShortString statt String    *}
{*          19.02.03  WW  erweitert um GetFupErrorcode und GetGasXStatus      *}
{*          07.05.03  Umgestellt auf dynamische Aufrufe (für Optimierung)     *}
{*          16.08.11  WW  erweitert um GetErrorcode_basic                     *}
{******************************************************************************}
Unit ErrPrc32;

INTERFACE

Uses
  Windows, DllUtil;

const
  ErrLibHandle: THandle = HINSTANCE_ERROR;

function GetStatusText (Status: integer): shortstring;
function GetErrorText (Status, Error: integer): shortstring;
function GetFupErrorcode(FupMeldung: shortstring): integer;
function GetKlasseText (Klasse: integer): shortstring;
function GetErrorKlasse (Status, Error: integer): integer;
function GetDSfGKonvErrorText (Status: integer): shortstring;
function GetGasXStatus(Status, Error: integer; Kommando: char): integer;
function GetErrorcode_basic (Status, Error: integer): integer;
function InitLibraryErrTxt32 (MsgOut: boolean = true): Boolean;
function DoneLibraryErrTxt32: Boolean;

IMPLEMENTATION

type
  TDLLIntStr = function(i: integer): shortstring;
  TDLLStrInt = function(s: shortstring): integer;
  TDLLIntIntStr = function(i1, i2: integer): shortstring;
  TDLLIntIntInt = function(i1, i2: integer): integer;
  TDLLIntIntCharInt = function(i1, i2: integer; c: char): integer;

function GetStatusText (Status: integer): shortstring;
var
  p : TDLLIntStr;
begin
  Result := '';  // Default

  try
    if (ErrLibHandle <> 0) and (ErrLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (ErrLibHandle, MakeIntResource(1));
      if (@p <> nil) then Result := p(Status);
    end;
  except
  // Result ist ''
  end;
end;

function GetErrorText (Status, Error: integer): shortstring;
var
  p : TDLLIntIntStr;
begin
  Result := '';  // Default

  try
    if (ErrLibHandle <> 0) and (ErrLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (ErrLibHandle, MakeIntResource(2));
      if (@p <> nil) then Result := p(Status, Error);
    end;
  except
  // Result ist ''
  end;
end;

function GetFupErrorcode(FupMeldung: shortstring): integer;
var
  p : TDLLStrInt;
begin
  Result := -1;  // Default

  try
    if (ErrLibHandle <> 0) and (ErrLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (ErrLibHandle, MakeIntResource(3));
      if (@p <> nil) then Result := p(FupMeldung);
    end;
  except
  // Result ist -1
  end;
end;

function GetKlasseText (Klasse: integer): shortstring;
var
  p : TDLLIntStr;
begin
  Result := '';  // Default

  try
    if (ErrLibHandle <> 0) and (ErrLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (ErrLibHandle, MakeIntResource(4));
      if (@p <> nil) then Result := p(Klasse);
    end;
  except
  // Result ist ''
  end;
end;

function GetErrorKlasse (Status, Error: integer): integer;
var
  p : TDLLIntIntInt;
begin
  Result := -1;  // Default

  try
    if (ErrLibHandle <> 0) and (ErrLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (ErrLibHandle, MakeIntResource(5));
      if (@p <> nil) then Result := p(Status, Error);
    end;
  except
  // Result ist ''
  end;
end;

function GetDSfGKonvErrorText (Status: integer): shortstring;
var
  p : TDLLIntStr;
begin
  Result := '';  // Default

  try
    if (ErrLibHandle <> 0) and (ErrLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (ErrLibHandle, MakeIntResource(6));
      if (@p <> nil) then Result := p(Status);
    end;
  except
  // Result ist ''
  end;
end;

function GetGasXStatus(Status, Error: integer; Kommando: char): integer;
var
  p : TDLLIntIntCharInt;
begin
  Result := -1;  // Default

  try
    if (ErrLibHandle <> 0) and (ErrLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (ErrLibHandle, MakeIntResource(7));
      if (@p <> nil) then Result := p(Status, Error, Kommando);
    end;
  except
  // Result ist -1
  end;
end;

function GetErrorcode_basic (Status, Error: integer): integer;  // 16.08.2011, WW
var
  p : TDLLIntIntInt;
begin
  Result := -1;  // Default

  try
    if (ErrLibHandle <> 0) and (ErrLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (ErrLibHandle, MakeIntResource(8));
      if (@p <> nil) then Result := p(Status, Error);
    end;
  except
  // Result ist -1
  end;
end;

procedure TagFunctions;
begin
{  @GetStatusText        := GetProcAddress (ErrLibHandle, MakeIntResource (1));
  @GetErrorText         := GetProcAddress (ErrLibHandle, MakeIntResource (2));
  @GetDFUActionText     := GetProcAddress (ErrLibHandle, MakeIntResource (3));
  @GetKlasseText        := GetProcAddress (ErrLibHandle, MakeIntResource (4));
  @GetErrorKlasse       := GetProcAddress (ErrLibHandle, MakeIntResource (5));
  @GetDSfGKonvErrorText := GetProcAddress (ErrLibHandle, MakeIntResource (6));
}end;

function InitLibraryErrTxt32 (MsgOut: boolean = true): Boolean;
const
  CDLLName_ErrTxt32 = 'ERRTXT32.DLL';
begin
  Result := (InitDll (GetDLLPath (CDLLName_ErrTxt32), ErrLibHandle, MsgOut));
end;

function DoneLibraryErrTxt32: Boolean;
begin
  Result := DoneDll (ErrLibHandle);
end;

End.
