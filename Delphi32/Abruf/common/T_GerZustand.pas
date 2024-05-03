{******************************************************************************}
{* Unit: Funktionen zum Erstellen einer Gerätezustand-Bitleiste               *}
{* 27.02.2014 WW                                                              *}
{* 04.04.2024 WW  Erweitert für TME400 und RSM200                             *}
{*                                                                            *}
{* Copyright © RMG Messtechnik GmbH 2014, 2024                                *}
{******************************************************************************}
unit T_GerZustand;

interface

uses
  SysUtils, Forms, Classes, MP_Elster, MP_Allg, DD_Allg, MP_RMG;

function SetGeraeteZustand_EK260 (slMeldungNr: TStrings; AllgNum: string;
  var iGerZustand: Int64): boolean;

function SetGeraeteZustand_EK280 (slMeldungNr: TStrings; AllgNum: string;
  var iGerZustand: Int64): boolean;

function SetGeraeteZustand_DL240 (slMeldungNr: TStrings; AllgNum: string;
  var iGerZustand: Int64): boolean;

function SetGeraeteZustand_DL230 (slMeldungNr: TStrings; AllgNum: string;
  var iGerZustand: Int64): boolean;

function SetGeraeteZustand_DL220 (slMeldungNr: TStrings; AllgNum: string;
  var iGerZustand: Int64): boolean;

function SetGeraeteZustand_DL210 (slMeldungNr: TStrings; AllgNum: string;
  var iGerZustand: Int64): boolean;

function SetGeraeteZustand_MRG900 (sParaWert: string; sParaAdr: string;
  var iGerZustand: Int64): boolean;

function SetGeraeteZustand_TME400 (sParaWert: string; sAllgNum: string;
  var iGerZustand: Int64): boolean;

function SetGeraeteZustand_RSM200 (sParaWert: string; sAllgNum: string;
  var iGerZustand: Int64): boolean;

implementation

{-----------------------------------------------------------------------}
function SetGeraeteZustand_EK260 (slMeldungNr: TStrings; AllgNum: string;
  var iGerZustand: Int64): boolean;
{-----------------------------------------------------------------------}
{ Setzt Bits im EK 260-Gerätezustand;
  Übergabe: Liste mit Status-Meldungsnummern
            Allgemeine Parameternummer, zu der die Status-Meldungsnummern gehören
  Übergabe/Rückgabe: Gerätezustand }
var
  i: integer;
  iMeldNr: integer;
  iBuf: Int64;

begin
  Result:=false;
  iBuf:=$01;  // zum Bit shiften

  for i:=0 to slMeldungNr.Count - 1 do begin
    Application.ProcessMessages;
    iMeldNr:=StrToIntDef (slMeldungNr [i], -1);
    if iMeldNr < 0 then exit;  // Gültige Meldungsnummern ab 0

    // Aktueller Systemstatus, System-Statusregister
    if (AllgNum = CP_ELS_AktSystemStatus) OR
       (AllgNum = CP_ELS_RegSystemStatus) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf);         // Bit 0
         3: iGerZustand:=iGerZustand OR (iBuf SHL 9);   // Bit 9
         7: iGerZustand:=iGerZustand OR (iBuf SHL 20);  // Bit 20
         8: iGerZustand:=iGerZustand OR (iBuf SHL 21);  // Bit 21
         9: iGerZustand:=iGerZustand OR (iBuf SHL 24);  // Bit 24
        10: iGerZustand:=iGerZustand OR (iBuf SHL 25);  // Bit 25
        11: iGerZustand:=iGerZustand OR (iBuf SHL 26);  // Bit 26
        12: iGerZustand:=iGerZustand OR (iBuf SHL 28);  // Bit 28
        13: iGerZustand:=iGerZustand OR (iBuf SHL 29);  // Bit 29
        14: iGerZustand:=iGerZustand OR (iBuf SHL 32);  // Bit 32
        15: iGerZustand:=iGerZustand OR (iBuf SHL 37);  // Bit 37
        16: iGerZustand:=iGerZustand OR (iBuf SHL 39);  // Bit 39
      end;
    end

    // Aktueller Status 1, Statusregister 1
    else if (AllgNum = CP_ELS_AktStatusK1) OR
            (AllgNum = CP_ELS_RegStatusK1) then begin
      case iMeldNr of
         2: iGerZustand:=iGerZustand OR (iBuf SHL 6);   // Bit 6
         4: iGerZustand:=iGerZustand OR (iBuf SHL 10);  // Bit 10
         6: iGerZustand:=iGerZustand OR (iBuf SHL 15);  // Bit 15
        11: iGerZustand:=iGerZustand OR (iBuf SHL 27);  // Bit 27
        14: iGerZustand:=iGerZustand OR (iBuf SHL 33);  // Bit 33
        15: iGerZustand:=iGerZustand OR (iBuf SHL 38);  // Bit 38
        16: iGerZustand:=iGerZustand OR (iBuf SHL 40);  // Bit 40
      end;
    end

    // Aktueller Status 2, Statusregister 2
    else if (AllgNum = CP_ELS_AktStatusK2) OR
            (AllgNum = CP_ELS_RegStatusK2) then begin
      case iMeldNr of
         4: iGerZustand:=iGerZustand OR (iBuf SHL 11);  // Bit 11
         5: iGerZustand:=iGerZustand OR (iBuf SHL 14);  // Bit 14
         6: iGerZustand:=iGerZustand OR (iBuf SHL 16);  // Bit 16
         8: iGerZustand:=iGerZustand OR (iBuf SHL 22);  // Bit 22
        13: iGerZustand:=iGerZustand OR (iBuf SHL 30);  // Bit 30
        14: iGerZustand:=iGerZustand OR (iBuf SHL 34);  // Bit 34
        16: iGerZustand:=iGerZustand OR (iBuf SHL 41);  // Bit 41
      end;
    end

    // Aktueller Status 3, Statusregister 3
    else if (AllgNum = CP_ELS_AktStatusK3) OR
            (AllgNum = CP_ELS_RegStatusK3) then begin
      case iMeldNr of
         4: iGerZustand:=iGerZustand OR (iBuf SHL 12);  // Bit 12
         8: iGerZustand:=iGerZustand OR (iBuf SHL 23);  // Bit 23
        13: iGerZustand:=iGerZustand OR (iBuf SHL 31);  // Bit 31
        14: iGerZustand:=iGerZustand OR (iBuf SHL 35);  // Bit 35
      end;
    end

    // Aktueller Status 4, Statusregister 4
    else if (AllgNum = CP_ELS_AktStatusK4) OR
            (AllgNum = CP_ELS_RegStatusK4) then begin
      case iMeldNr of
         4: iGerZustand:=iGerZustand OR (iBuf SHL 13);  // Bit 13
         6: iGerZustand:=iGerZustand OR (iBuf SHL 17);  // Bit 17
        14: iGerZustand:=iGerZustand OR (iBuf SHL 36);  // Bit 36
      end;
    end

    // Aktueller Status 5, Statusregister 5
    else if (AllgNum = CP_ELS_AktStatusK5) OR
            (AllgNum = CP_ELS_RegStatusK5) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf SHL 1);   // Bit 1
         2: iGerZustand:=iGerZustand OR (iBuf SHL 7);   // Bit 7
      end;
    end

    // Aktueller Status 6, Statusregister 6
    else if (AllgNum = CP_ELS_AktStatusK6) OR
            (AllgNum = CP_ELS_RegStatusK6) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf SHL 2);   // Bit 2
         2: iGerZustand:=iGerZustand OR (iBuf SHL 8);   // Bit 8
         6: iGerZustand:=iGerZustand OR (iBuf SHL 18);  // Bit 18
      end;
    end

    // Aktueller Status 7, Statusregister 7
    else if (AllgNum = CP_ELS_AktStatusK7) OR
            (AllgNum = CP_ELS_RegStatusK7) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf SHL 3);   // Bit 3
         6: iGerZustand:=iGerZustand OR (iBuf SHL 19);  // Bit 19
      end;
    end

    // Aktueller Status 8, Statusregister 8
    else if (AllgNum = CP_ELS_AktStatusK8) OR
            (AllgNum = CP_ELS_RegStatusK8) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf SHL 4);   // Bit 4
      end;
    end

    // Aktueller Status 9, Statusregister 9
    else if (AllgNum = CP_ELS_AktStatusK9) OR
            (AllgNum = CP_ELS_RegStatusK9) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf SHL 5);   // Bit 5
      end;
    end;
  end;  { for i }
  Result:=true;
end;

{-----------------------------------------------------------------------}
function SetGeraeteZustand_EK280 (slMeldungNr: TStrings; AllgNum: string;
  var iGerZustand: Int64): boolean;
{-----------------------------------------------------------------------}
{ Setzt Bits im EK 280-Gerätezustand;
  Übergabe: Liste mit Status-Meldungsnummern
            Allgemeine Parameternummer, zu der die Status-Meldungsnummern gehören
  Übergabe/Rückgabe: Gerätezustand }
var
  i: integer;
  iMeldNr: integer;
  iBuf: Int64;

begin
  Result:=false;
  iBuf:=$01;  // zum Bit shiften

  for i:=0 to slMeldungNr.Count - 1 do begin
    Application.ProcessMessages;
    iMeldNr:=StrToIntDef (slMeldungNr [i], -1);
    if iMeldNr < 0 then exit;  // Gültige Meldungsnummern ab 0

    // Aktueller Systemstatus, System-Statusregister
    if (AllgNum = CP_ELS_AktSystemStatus) OR
       (AllgNum = CP_ELS_RegSystemStatus) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf);         // Bit 0
         3: iGerZustand:=iGerZustand OR (iBuf SHL 11);  // Bit 11
         7: iGerZustand:=iGerZustand OR (iBuf SHL 23);  // Bit 23
         8: iGerZustand:=iGerZustand OR (iBuf SHL 25);  // Bit 25
         9: iGerZustand:=iGerZustand OR (iBuf SHL 33);  // Bit 33
        10: iGerZustand:=iGerZustand OR (iBuf SHL 35);  // Bit 35
        11: iGerZustand:=iGerZustand OR (iBuf SHL 36);  // Bit 36
        12: iGerZustand:=iGerZustand OR (iBuf SHL 39);  // Bit 39
        13: iGerZustand:=iGerZustand OR (iBuf SHL 40);  // Bit 40
        14: iGerZustand:=iGerZustand OR (iBuf SHL 46);  // Bit 46
        15: iGerZustand:=iGerZustand OR (iBuf SHL 54);  // Bit 54
        16: iGerZustand:=iGerZustand OR (iBuf SHL 56);  // Bit 56
      end;
    end

    // Aktueller Systemstatus 2, System-Statusregister 2
    else if (AllgNum = CP_ELS_AktSystemStatus2) OR
            (AllgNum = CP_ELS_RegSystemStatus2) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf SHL 1);   // Bit 1
         7: iGerZustand:=iGerZustand OR (iBuf SHL 24);  // Bit 24
        11: iGerZustand:=iGerZustand OR (iBuf SHL 37);  // Bit 37
      end;
    end

    // Aktueller Status 1, Statusregister 1
    else if (AllgNum = CP_ELS_AktStatusK1) OR
            (AllgNum = CP_ELS_RegStatusK1) then begin
      case iMeldNr of
         2: iGerZustand:=iGerZustand OR (iBuf SHL 8);   // Bit 8
         4: iGerZustand:=iGerZustand OR (iBuf SHL 12);  // Bit 12
         6: iGerZustand:=iGerZustand OR (iBuf SHL 17);  // Bit 17
        11: iGerZustand:=iGerZustand OR (iBuf SHL 38);  // Bit 38
        14: iGerZustand:=iGerZustand OR (iBuf SHL 47);  // Bit 47
        15: iGerZustand:=iGerZustand OR (iBuf SHL 55);  // Bit 55
        16: iGerZustand:=iGerZustand OR (iBuf SHL 57);  // Bit 57
      end;
    end

    // Aktueller Status 2, Statusregister 2
    else if (AllgNum = CP_ELS_AktStatusK2) OR
            (AllgNum = CP_ELS_RegStatusK2) then begin
      case iMeldNr of
         4: iGerZustand:=iGerZustand OR (iBuf SHL 13);  // Bit 13
         5: iGerZustand:=iGerZustand OR (iBuf SHL 16);  // Bit 16
         6: iGerZustand:=iGerZustand OR (iBuf SHL 18);  // Bit 18
         8: iGerZustand:=iGerZustand OR (iBuf SHL 26);  // Bit 26
        13: iGerZustand:=iGerZustand OR (iBuf SHL 41);  // Bit 41
        14: iGerZustand:=iGerZustand OR (iBuf SHL 48);  // Bit 48
        16: iGerZustand:=iGerZustand OR (iBuf SHL 58);  // Bit 58
      end;
    end

    // Aktueller Status 3, Statusregister 3
    else if (AllgNum = CP_ELS_AktStatusK3) OR
            (AllgNum = CP_ELS_RegStatusK3) then begin
      case iMeldNr of
         4: iGerZustand:=iGerZustand OR (iBuf SHL 14);  // Bit 14
         8: iGerZustand:=iGerZustand OR (iBuf SHL 27);  // Bit 27
         9: iGerZustand:=iGerZustand OR (iBuf SHL 34);  // Bit 34
        13: iGerZustand:=iGerZustand OR (iBuf SHL 42);  // Bit 42
        14: iGerZustand:=iGerZustand OR (iBuf SHL 49);  // Bit 49
        16: iGerZustand:=iGerZustand OR (iBuf SHL 59);  // Bit 59
      end;
    end

    // Aktueller Status 4, Statusregister 4
    else if (AllgNum = CP_ELS_AktStatusK4) OR
            (AllgNum = CP_ELS_RegStatusK4) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf SHL 2);   // Bit 2
         4: iGerZustand:=iGerZustand OR (iBuf SHL 15);  // Bit 15
         6: iGerZustand:=iGerZustand OR (iBuf SHL 19);  // Bit 19
         8: iGerZustand:=iGerZustand OR (iBuf SHL 28);  // Bit 28
        13: iGerZustand:=iGerZustand OR (iBuf SHL 43);  // Bit 43
        14: iGerZustand:=iGerZustand OR (iBuf SHL 50);  // Bit 50
        16: iGerZustand:=iGerZustand OR (iBuf SHL 60);  // Bit 60
      end;
    end

    // Aktueller Status 5, Statusregister 5
    else if (AllgNum = CP_ELS_AktStatusK5) OR
            (AllgNum = CP_ELS_RegStatusK5) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf SHL 3);   // Bit 3
         2: iGerZustand:=iGerZustand OR (iBuf SHL 9);   // Bit 9
        14: iGerZustand:=iGerZustand OR (iBuf SHL 51);  // Bit 51
        16: iGerZustand:=iGerZustand OR (iBuf SHL 61);  // Bit 61
      end;
    end

    // Aktueller Status 6, Statusregister 6
    else if (AllgNum = CP_ELS_AktStatusK6) OR
            (AllgNum = CP_ELS_RegStatusK6) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf SHL 4);   // Bit 4
         2: iGerZustand:=iGerZustand OR (iBuf SHL 10);  // Bit 10
         6: iGerZustand:=iGerZustand OR (iBuf SHL 20);  // Bit 20
         8: iGerZustand:=iGerZustand OR (iBuf SHL 29);  // Bit 29
        14: iGerZustand:=iGerZustand OR (iBuf SHL 52);  // Bit 52
        16: iGerZustand:=iGerZustand OR (iBuf SHL 62);  // Bit 62
      end;
    end

    // Aktueller Status 7, Statusregister 7
    else if (AllgNum = CP_ELS_AktStatusK7) OR
            (AllgNum = CP_ELS_RegStatusK7) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf SHL 5);   // Bit 5
         6: iGerZustand:=iGerZustand OR (iBuf SHL 21);  // Bit 21
         8: iGerZustand:=iGerZustand OR (iBuf SHL 30);  // Bit 30
        14: iGerZustand:=iGerZustand OR (iBuf SHL 53);  // Bit 53
      end;
    end

    // Aktueller Status 8, Statusregister 8
    else if (AllgNum = CP_ELS_AktStatusK8) OR
            (AllgNum = CP_ELS_RegStatusK8) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf SHL 6);  // Bit 6
      end;
    end

    // Aktueller Status 9, Statusregister 9
    else if (AllgNum = CP_ELS_AktStatusK9) OR
            (AllgNum = CP_ELS_RegStatusK9) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf SHL 7);   // Bit 7
         6: iGerZustand:=iGerZustand OR (iBuf SHL 22);  // Bit 22
         8: iGerZustand:=iGerZustand OR (iBuf SHL 31);  // Bit 31
        13: iGerZustand:=iGerZustand OR (iBuf SHL 44);  // Bit 44
      end;
    end

    // Aktueller Status 10, Statusregister 10
    else if (AllgNum = CP_ELS_AktStatusK10) OR
            (AllgNum = CP_ELS_RegStatusK10) then begin
      case iMeldNr of
         8: iGerZustand:=iGerZustand OR (iBuf SHL 32);  // Bit 32
        13: iGerZustand:=iGerZustand OR (iBuf SHL 45);  // Bit 45
      end;
    end;
  end;  { for i }
  Result:=true;
end;

{-----------------------------------------------------------------------}
function SetGeraeteZustand_DL240 (slMeldungNr: TStrings; AllgNum: string;
  var iGerZustand: Int64): boolean;
{-----------------------------------------------------------------------}
{ Setzt Bits im DL 240-Gerätezustand;
  Übergabe: Liste mit Status-Meldungsnummern
            Allgemeine Parameternummer, zu der die Status-Meldungsnummern gehören
  Übergabe/Rückgabe: Gerätezustand }
var
  i: integer;
  iMeldNr: integer;
  iBuf: Int64;

begin
  Result:=false;
  iBuf:=$01;  // zum Bit shiften

  for i:=0 to slMeldungNr.Count - 1 do begin
    Application.ProcessMessages;
    iMeldNr:=StrToIntDef (slMeldungNr [i], -1);
    if iMeldNr < 0 then exit;  // Gültige Meldungsnummern ab 0

    // Aktueller Systemstatus, System-Statusregister
    if (AllgNum = CP_ELS_AktSystemStatus) OR
       (AllgNum = CP_ELS_RegSystemStatus) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf);         // Bit 0
         3: iGerZustand:=iGerZustand OR (iBuf SHL 1);   // Bit 1
         4: iGerZustand:=iGerZustand OR (iBuf SHL 2);   // Bit 2
         5: iGerZustand:=iGerZustand OR (iBuf SHL 5);   // Bit 5
         6: iGerZustand:=iGerZustand OR (iBuf SHL 10);  // Bit 10
         7: iGerZustand:=iGerZustand OR (iBuf SHL 15);  // Bit 15
         8: iGerZustand:=iGerZustand OR (iBuf SHL 17);  // Bit 17
         9: iGerZustand:=iGerZustand OR (iBuf SHL 22);  // Bit 22
        10: iGerZustand:=iGerZustand OR (iBuf SHL 23);  // Bit 23
        11: iGerZustand:=iGerZustand OR (iBuf SHL 24);  // Bit 24
        13: iGerZustand:=iGerZustand OR (iBuf SHL 29);  // Bit 29
        15: iGerZustand:=iGerZustand OR (iBuf SHL 38);  // Bit 38
        16: iGerZustand:=iGerZustand OR (iBuf SHL 39);  // Bit 39
      end;
    end

    // Aktueller Status 1, Statusregister 1
    else if (AllgNum = CP_ELS_AktStatusK1) OR
            (AllgNum = CP_ELS_RegStatusK1) then begin
      case iMeldNr of
         4: iGerZustand:=iGerZustand OR (iBuf SHL 3);   // Bit 3
         5: iGerZustand:=iGerZustand OR (iBuf SHL 6);   // Bit 6
         6: iGerZustand:=iGerZustand OR (iBuf SHL 11);  // Bit 11
         7: iGerZustand:=iGerZustand OR (iBuf SHL 16);  // Bit 16
         8: iGerZustand:=iGerZustand OR (iBuf SHL 18);  // Bit 18
        12: iGerZustand:=iGerZustand OR (iBuf SHL 25);  // Bit 25
        13: iGerZustand:=iGerZustand OR (iBuf SHL 30);  // Bit 30
        14: iGerZustand:=iGerZustand OR (iBuf SHL 34);  // Bit 34
        16: iGerZustand:=iGerZustand OR (iBuf SHL 40);  // Bit 40
      end;
    end

    // Aktueller Status 2, Statusregister 2
    else if (AllgNum = CP_ELS_AktStatusK2) OR
            (AllgNum = CP_ELS_RegStatusK2) then begin
      case iMeldNr of
         4: iGerZustand:=iGerZustand OR (iBuf SHL 4);   // Bit 4
         5: iGerZustand:=iGerZustand OR (iBuf SHL 7);   // Bit 7
         6: iGerZustand:=iGerZustand OR (iBuf SHL 12);  // Bit 12
         8: iGerZustand:=iGerZustand OR (iBuf SHL 19);  // Bit 19
        12: iGerZustand:=iGerZustand OR (iBuf SHL 26);  // Bit 26
        13: iGerZustand:=iGerZustand OR (iBuf SHL 31);  // Bit 31
        14: iGerZustand:=iGerZustand OR (iBuf SHL 35);  // Bit 35
        16: iGerZustand:=iGerZustand OR (iBuf SHL 41);  // Bit 41
      end;
    end

    // Aktueller Status 3, Statusregister 3
    else if (AllgNum = CP_ELS_AktStatusK3) OR
            (AllgNum = CP_ELS_RegStatusK3) then begin
      case iMeldNr of
         5: iGerZustand:=iGerZustand OR (iBuf SHL 8);   // Bit 8
         6: iGerZustand:=iGerZustand OR (iBuf SHL 13);  // Bit 13
         8: iGerZustand:=iGerZustand OR (iBuf SHL 20);  // Bit 20
        12: iGerZustand:=iGerZustand OR (iBuf SHL 27);  // Bit 27
        13: iGerZustand:=iGerZustand OR (iBuf SHL 32);  // Bit 32
        14: iGerZustand:=iGerZustand OR (iBuf SHL 36);  // Bit 36
        16: iGerZustand:=iGerZustand OR (iBuf SHL 42);  // Bit 42
      end;
    end

    // Aktueller Status 4, Statusregister 4
    else if (AllgNum = CP_ELS_AktStatusK4) OR
            (AllgNum = CP_ELS_RegStatusK4) then begin
      case iMeldNr of
         5: iGerZustand:=iGerZustand OR (iBuf SHL 9);   // Bit 9
         6: iGerZustand:=iGerZustand OR (iBuf SHL 14);  // Bit 14
         8: iGerZustand:=iGerZustand OR (iBuf SHL 21);  // Bit 21
        12: iGerZustand:=iGerZustand OR (iBuf SHL 28);  // Bit 28
        13: iGerZustand:=iGerZustand OR (iBuf SHL 33);  // Bit 33
        14: iGerZustand:=iGerZustand OR (iBuf SHL 37);  // Bit 37
      end;
    end;
  end;  { for i }
  Result:=true;
end;

{-----------------------------------------------------------------------}
function SetGeraeteZustand_DL230 (slMeldungNr: TStrings; AllgNum: string;
  var iGerZustand: Int64): boolean;
{-----------------------------------------------------------------------}
{ Setzt Bits im DL 230-Gerätezustand;
  Übergabe: Liste mit Status-Meldungsnummern
            Allgemeine Parameternummer, zu der die Status-Meldungsnummern gehören
  Übergabe/Rückgabe: Gerätezustand }
var
  i: integer;
  iMeldNr: integer;
  iBuf: Int64;

begin
  Result:=false;
  iBuf:=$01;  // zum Bit shiften

  for i:=0 to slMeldungNr.Count - 1 do begin
    Application.ProcessMessages;
    iMeldNr:=StrToIntDef (slMeldungNr [i], -1);
    if iMeldNr < 0 then exit;  // Gültige Meldungsnummern ab 0

    // Aktueller Systemstatus, System-Statusregister
    if (AllgNum = CP_ELS_AktSystemStatus) OR
       (AllgNum = CP_ELS_RegSystemStatus) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf);         // Bit 0
         3: iGerZustand:=iGerZustand OR (iBuf SHL 4);   // Bit 4
         7: iGerZustand:=iGerZustand OR (iBuf SHL 18);  // Bit 18
         8: iGerZustand:=iGerZustand OR (iBuf SHL 20);  // Bit 20
         9: iGerZustand:=iGerZustand OR (iBuf SHL 25);  // Bit 25
        11: iGerZustand:=iGerZustand OR (iBuf SHL 27);  // Bit 27
        12: iGerZustand:=iGerZustand OR (iBuf SHL 31);  // Bit 31
        13: iGerZustand:=iGerZustand OR (iBuf SHL 36);  // Bit 36
        15: iGerZustand:=iGerZustand OR (iBuf SHL 48);  // Bit 48
        16: iGerZustand:=iGerZustand OR (iBuf SHL 50);  // Bit 50
      end;
    end

    // Aktueller Systemstatus 2, System-Statusregister 2
    else if (AllgNum = CP_ELS_AktSystemStatus2) OR
            (AllgNum = CP_ELS_RegSystemStatus2) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf SHL 1);   // Bit 1
         3: iGerZustand:=iGerZustand OR (iBuf SHL 5);   // Bit 5
         7: iGerZustand:=iGerZustand OR (iBuf SHL 19);  // Bit 19
        11: iGerZustand:=iGerZustand OR (iBuf SHL 28);  // Bit 28
        15: iGerZustand:=iGerZustand OR (iBuf SHL 49);  // Bit 49
        16: iGerZustand:=iGerZustand OR (iBuf SHL 51);  // Bit 51
      end;
    end

    // Aktueller Status 1, Statusregister 1
    else if (AllgNum = CP_ELS_AktStatusK1) OR
            (AllgNum = CP_ELS_RegStatusK1) then begin
      case iMeldNr of
         2: iGerZustand:=iGerZustand OR (iBuf SHL 2);   // Bit 2
         4: iGerZustand:=iGerZustand OR (iBuf SHL 6);   // Bit 6
         5: iGerZustand:=iGerZustand OR (iBuf SHL 10);  // Bit 10
         6: iGerZustand:=iGerZustand OR (iBuf SHL 14);  // Bit 14
         8: iGerZustand:=iGerZustand OR (iBuf SHL 21);  // Bit 21
         9: iGerZustand:=iGerZustand OR (iBuf SHL 26);  // Bit 26
        11: iGerZustand:=iGerZustand OR (iBuf SHL 29);  // Bit 29
        12: iGerZustand:=iGerZustand OR (iBuf SHL 32);  // Bit 32
        13: iGerZustand:=iGerZustand OR (iBuf SHL 37);  // Bit 37
        14: iGerZustand:=iGerZustand OR (iBuf SHL 41);  // Bit 41
        16: iGerZustand:=iGerZustand OR (iBuf SHL 52);  // Bit 52
      end;
    end

    // Aktueller Status 2, Statusregister 2
    else if (AllgNum = CP_ELS_AktStatusK2) OR
            (AllgNum = CP_ELS_RegStatusK2) then begin
      case iMeldNr of
         2: iGerZustand:=iGerZustand OR (iBuf SHL 3);   // Bit 3
         4: iGerZustand:=iGerZustand OR (iBuf SHL 7);   // Bit 7
         5: iGerZustand:=iGerZustand OR (iBuf SHL 11);  // Bit 11
         6: iGerZustand:=iGerZustand OR (iBuf SHL 15);  // Bit 15
         8: iGerZustand:=iGerZustand OR (iBuf SHL 22);  // Bit 22
        11: iGerZustand:=iGerZustand OR (iBuf SHL 30);  // Bit 30
        12: iGerZustand:=iGerZustand OR (iBuf SHL 33);  // Bit 33
        13: iGerZustand:=iGerZustand OR (iBuf SHL 38);  // Bit 38
        14: iGerZustand:=iGerZustand OR (iBuf SHL 42);  // Bit 42
        16: iGerZustand:=iGerZustand OR (iBuf SHL 53);  // Bit 53
      end;
    end

    // Aktueller Status 3, Statusregister 3
    else if (AllgNum = CP_ELS_AktStatusK3) OR
            (AllgNum = CP_ELS_RegStatusK3) then begin
      case iMeldNr of
         4: iGerZustand:=iGerZustand OR (iBuf SHL 8);   // Bit 8
         5: iGerZustand:=iGerZustand OR (iBuf SHL 12);  // Bit 12
         6: iGerZustand:=iGerZustand OR (iBuf SHL 16);  // Bit 16
         8: iGerZustand:=iGerZustand OR (iBuf SHL 23);  // Bit 23
        12: iGerZustand:=iGerZustand OR (iBuf SHL 34);  // Bit 34
        13: iGerZustand:=iGerZustand OR (iBuf SHL 39);  // Bit 39
        14: iGerZustand:=iGerZustand OR (iBuf SHL 43);  // Bit 43
        16: iGerZustand:=iGerZustand OR (iBuf SHL 54);  // Bit 54
      end;
    end

    // Aktueller Status 4, Statusregister 4
    else if (AllgNum = CP_ELS_AktStatusK4) OR
            (AllgNum = CP_ELS_RegStatusK4) then begin
      case iMeldNr of
         4: iGerZustand:=iGerZustand OR (iBuf SHL 9);   // Bit 9
         5: iGerZustand:=iGerZustand OR (iBuf SHL 13);  // Bit 13
         6: iGerZustand:=iGerZustand OR (iBuf SHL 17);  // Bit 17
         8: iGerZustand:=iGerZustand OR (iBuf SHL 24);  // Bit 24
        12: iGerZustand:=iGerZustand OR (iBuf SHL 35);  // Bit 35
        13: iGerZustand:=iGerZustand OR (iBuf SHL 40);  // Bit 40
        14: iGerZustand:=iGerZustand OR (iBuf SHL 44);  // Bit 44
        16: iGerZustand:=iGerZustand OR (iBuf SHL 55);  // Bit 55
      end;
    end

    // Aktueller Status 5, Statusregister 5
    else if (AllgNum = CP_ELS_AktStatusK5) OR
            (AllgNum = CP_ELS_RegStatusK5) then begin
      case iMeldNr of
        14: iGerZustand:=iGerZustand OR (iBuf SHL 45);  // Bit 45
        16: iGerZustand:=iGerZustand OR (iBuf SHL 56);  // Bit 56
      end;
    end

    // Aktueller Status 6, Statusregister 6
    else if (AllgNum = CP_ELS_AktStatusK6) OR
            (AllgNum = CP_ELS_RegStatusK6) then begin
      case iMeldNr of
        14: iGerZustand:=iGerZustand OR (iBuf SHL 46);  // Bit 46
        16: iGerZustand:=iGerZustand OR (iBuf SHL 57);  // Bit 57
      end;
    end

    // Aktueller Status 7, Statusregister 7
    else if (AllgNum = CP_ELS_AktStatusK7) OR
            (AllgNum = CP_ELS_RegStatusK7) then begin
      case iMeldNr of
        14: iGerZustand:=iGerZustand OR (iBuf SHL 47);  // Bit 47
        16: iGerZustand:=iGerZustand OR (iBuf SHL 58);  // Bit 58
      end;
    end

    // Aktueller Status 8, Statusregister 8
    else if (AllgNum = CP_ELS_AktStatusK8) OR
            (AllgNum = CP_ELS_RegStatusK8) then begin
      case iMeldNr of
        16: iGerZustand:=iGerZustand OR (iBuf SHL 59);  // Bit 59
      end;
    end;
  end;  { for i }
  Result:=true;
end;

{-----------------------------------------------------------------------}
function SetGeraeteZustand_DL220 (slMeldungNr: TStrings; AllgNum: string;
  var iGerZustand: Int64): boolean;
{-----------------------------------------------------------------------}
{ Setzt Bits im DL 220-Gerätezustand;
  Übergabe: Liste mit Status-Meldungsnummern
            Allgemeine Parameternummer, zu der die Status-Meldungsnummern gehören
  Übergabe/Rückgabe: Gerätezustand }
var
  i: integer;
  iMeldNr: integer;
  iBuf: Int64;

begin
  Result:=false;
  iBuf:=$01;  // zum Bit shiften

  for i:=0 to slMeldungNr.Count - 1 do begin
    Application.ProcessMessages;
    iMeldNr:=StrToIntDef (slMeldungNr [i], -1);
    if iMeldNr < 0 then exit;  // Gültige Meldungsnummern ab 0

    // Aktueller Systemstatus, System-Statusregister
    if (AllgNum = CP_ELS_AktSystemStatus) OR
       (AllgNum = CP_ELS_RegSystemStatus) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf);         // Bit 0
         3: iGerZustand:=iGerZustand OR (iBuf SHL 1);   // Bit 1
         8: iGerZustand:=iGerZustand OR (iBuf SHL 8);   // Bit 8
         9: iGerZustand:=iGerZustand OR (iBuf SHL 11);  // Bit 11
        11: iGerZustand:=iGerZustand OR (iBuf SHL 13);  // Bit 13
        12: iGerZustand:=iGerZustand OR (iBuf SHL 14);  // Bit 14
        13: iGerZustand:=iGerZustand OR (iBuf SHL 17);  // Bit 17
        15: iGerZustand:=iGerZustand OR (iBuf SHL 24);  // Bit 24
        16: iGerZustand:=iGerZustand OR (iBuf SHL 25);  // Bit 25
      end;
    end

    // Aktueller Status 1, Statusregister 1
    else if (AllgNum = CP_ELS_AktStatusK1) OR
            (AllgNum = CP_ELS_RegStatusK1) then begin
      case iMeldNr of
         4: iGerZustand:=iGerZustand OR (iBuf SHL 2);   // Bit 2
         5: iGerZustand:=iGerZustand OR (iBuf SHL 4);   // Bit 4
         6: iGerZustand:=iGerZustand OR (iBuf SHL 6);   // Bit 6
         8: iGerZustand:=iGerZustand OR (iBuf SHL 9);   // Bit 9
        12: iGerZustand:=iGerZustand OR (iBuf SHL 15);  // Bit 15
        13: iGerZustand:=iGerZustand OR (iBuf SHL 18);  // Bit 18
        14: iGerZustand:=iGerZustand OR (iBuf SHL 20);  // Bit 20
        16: iGerZustand:=iGerZustand OR (iBuf SHL 26);  // Bit 26
      end;
    end

    // Aktueller Status 2, Statusregister 2
    else if (AllgNum = CP_ELS_AktStatusK2) OR
            (AllgNum = CP_ELS_RegStatusK2) then begin
      case iMeldNr of
         4: iGerZustand:=iGerZustand OR (iBuf SHL 3);   // Bit 3
         5: iGerZustand:=iGerZustand OR (iBuf SHL 5);   // Bit 5
         6: iGerZustand:=iGerZustand OR (iBuf SHL 7);   // Bit 7
         8: iGerZustand:=iGerZustand OR (iBuf SHL 10);  // Bit 10
        12: iGerZustand:=iGerZustand OR (iBuf SHL 16);  // Bit 16
        13: iGerZustand:=iGerZustand OR (iBuf SHL 19);  // Bit 19
        14: iGerZustand:=iGerZustand OR (iBuf SHL 21);  // Bit 21
        16: iGerZustand:=iGerZustand OR (iBuf SHL 27);  // Bit 27
      end;
    end

    // Aktueller Status 3, Statusregister 3
    else if (AllgNum = CP_ELS_AktStatusK3) OR
            (AllgNum = CP_ELS_RegStatusK3) then begin
      case iMeldNr of
        14: iGerZustand:=iGerZustand OR (iBuf SHL 22);  // Bit 22
      end;
    end

    // Aktueller Status 4, Statusregister 4
    else if (AllgNum = CP_ELS_AktStatusK4) OR
            (AllgNum = CP_ELS_RegStatusK4) then begin
      case iMeldNr of
         9: iGerZustand:=iGerZustand OR (iBuf SHL 12);  // Bit 12
        14: iGerZustand:=iGerZustand OR (iBuf SHL 23);  // Bit 23
      end;
    end;
  end;  { for i }
  Result:=true;
end;

{-----------------------------------------------------------------------}
function SetGeraeteZustand_DL210 (slMeldungNr: TStrings; AllgNum: string;
  var iGerZustand: Int64): boolean;
{-----------------------------------------------------------------------}
{ Setzt Bits im DL 210-Gerätezustand;
  Übergabe: Liste mit Status-Meldungsnummern
            Allgemeine Parameternummer, zu der die Status-Meldungsnummern gehören
  Übergabe/Rückgabe: Gerätezustand }
var
  i: integer;
  iMeldNr: integer;
  iBuf: Int64;

begin
  Result:=false;
  iBuf:=$01;  // zum Bit shiften

  for i:=0 to slMeldungNr.Count - 1 do begin
    Application.ProcessMessages;
    iMeldNr:=StrToIntDef (slMeldungNr [i], -1);
    if iMeldNr < 0 then exit;  // Gültige Meldungsnummern ab 0

    // Aktueller Systemstatus, System-Statusregister
    if (AllgNum = CP_ELS_AktSystemStatus) OR
       (AllgNum = CP_ELS_RegSystemStatus) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf);         // Bit 0
         3: iGerZustand:=iGerZustand OR (iBuf SHL 3);   // Bit 3
         8: iGerZustand:=iGerZustand OR (iBuf SHL 7);   // Bit 7
         9: iGerZustand:=iGerZustand OR (iBuf SHL 10);  // Bit 10
        11: iGerZustand:=iGerZustand OR (iBuf SHL 12);  // Bit 12
        12: iGerZustand:=iGerZustand OR (iBuf SHL 14);  // Bit 14
        13: iGerZustand:=iGerZustand OR (iBuf SHL 17);  // Bit 17
        15: iGerZustand:=iGerZustand OR (iBuf SHL 24);  // Bit 24
        16: iGerZustand:=iGerZustand OR (iBuf SHL 25);  // Bit 25
      end;
    end

    // Aktueller Status 1, Statusregister 1
    else if (AllgNum = CP_ELS_AktStatusK1) OR
            (AllgNum = CP_ELS_RegStatusK1) then begin
      case iMeldNr of
         1: iGerZustand:=iGerZustand OR (iBuf SHL 1);   // Bit 1
         2: iGerZustand:=iGerZustand OR (iBuf SHL 2);   // Bit 2
         5: iGerZustand:=iGerZustand OR (iBuf SHL 4);   // Bit 4
         6: iGerZustand:=iGerZustand OR (iBuf SHL 5);   // Bit 5
         8: iGerZustand:=iGerZustand OR (iBuf SHL 8);   // Bit 8
        11: iGerZustand:=iGerZustand OR (iBuf SHL 13);  // Bit 13
        12: iGerZustand:=iGerZustand OR (iBuf SHL 15);  // Bit 15
        13: iGerZustand:=iGerZustand OR (iBuf SHL 18);  // Bit 18
        14: iGerZustand:=iGerZustand OR (iBuf SHL 20);  // Bit 20
        16: iGerZustand:=iGerZustand OR (iBuf SHL 26);  // Bit 26
      end;
    end

    // Aktueller Status 2, Statusregister 2
    else if (AllgNum = CP_ELS_AktStatusK2) OR
            (AllgNum = CP_ELS_RegStatusK2) then begin
      case iMeldNr of
         6: iGerZustand:=iGerZustand OR (iBuf SHL 6);   // Bit 6
         8: iGerZustand:=iGerZustand OR (iBuf SHL 9);   // Bit 9
        12: iGerZustand:=iGerZustand OR (iBuf SHL 16);  // Bit 16
        13: iGerZustand:=iGerZustand OR (iBuf SHL 19);  // Bit 19
        14: iGerZustand:=iGerZustand OR (iBuf SHL 21);  // Bit 21
        16: iGerZustand:=iGerZustand OR (iBuf SHL 27);  // Bit 27
      end;
    end

    // Aktueller Status 3, Statusregister 3
    else if (AllgNum = CP_ELS_AktStatusK3) OR
            (AllgNum = CP_ELS_RegStatusK3) then begin
      case iMeldNr of
        14: iGerZustand:=iGerZustand OR (iBuf SHL 22);  // Bit 22
        16: iGerZustand:=iGerZustand OR (iBuf SHL 28);  // Bit 28
      end;
    end

    // Aktueller Status 4, Statusregister 4
    else if (AllgNum = CP_ELS_AktStatusK4) OR
            (AllgNum = CP_ELS_RegStatusK4) then begin
      case iMeldNr of
         9: iGerZustand:=iGerZustand OR (iBuf SHL 11);  // Bit 11
        14: iGerZustand:=iGerZustand OR (iBuf SHL 23);  // Bit 23
        16: iGerZustand:=iGerZustand OR (iBuf SHL 29);  // Bit 29
      end;
    end;
  end;  { for i }
  Result:=true;
end;

{---------------------------------------------------------------------}
function SetGeraeteZustand_MRG900 (sParaWert: string; sParaAdr: string;
  var iGerZustand: Int64): boolean;
{---------------------------------------------------------------------}
{ Setzt Bits im MRG 905/910-Gerätezustand;
  Übergabe: Parameterwert
            Parameter-Adresse, zu dem der Parameterwert gehört (MRG: Allgemeine
              Parameternummer; DSfG: Datenelementadresse)
  Übergabe/Rückgabe: Gerätezustand }
begin
  // Eichschalter Zustand
  if (sParaAdr = CP_ALLG_Eichschalter) OR
     (sParaAdr = CD_ALLG_Eichschalter) then begin
    if sParaWert = '1' then  // Eichschalter auf
      iGerZustand:=iGerZustand OR $01;  // Bit 0
  end
  // Signalausgang 1 Zustand
  else if (sParaAdr = CP_ALLG_Ausg1_Zustand) OR
          (sParaAdr = CD_WIESER_Ausg1_Zustand) then begin
    if sParaWert = '0' then  // Ausgang zu
      iGerZustand:=iGerZustand OR $02;  // Bit 1
  end
  // Signalausgang 2 Zustand
  else if (sParaAdr = CP_ALLG_Ausg2_Zustand) OR
          (sParaAdr = CD_WIESER_Ausg2_Zustand) then begin
    if sParaWert = '0' then  // Ausgang zu
      iGerZustand:=iGerZustand OR $04;  // Bit 2
  end;
  Result:=true;
end;

// 04.04.2024, WW
{---------------------------------------------------------------------}
function SetGeraeteZustand_TME400 (sParaWert: string; sAllgNum: string;
  var iGerZustand: Int64): boolean;
{---------------------------------------------------------------------}
{ Setzt Bits im TME400-Gerätezustand;
  Übergabe: Parameterwert
            Allgemeine Parameternummer, zu der der Parameterwert gehört
  Übergabe/Rückgabe: Gerätezustand }
var
  iParaWert: Int64;
  iMaske: cardinal;
  iBit: integer;
  iBuf: Int64;

begin
  Result:=false;
  iBuf:=$01;  // zum Bit shiften

  // Fehlerregister
  if (sAllgNum = CP_RMG_TME400_Fehlerregister) then begin
    iParaWert:=StrToInt64Def ('$' + sParaWert, -1);
    if iParaWert < 0 then exit;  // Gültige Bitleiste ab 0

    iMaske:=1;
    for iBit:=0 to 31 do begin
      Application.ProcessMessages;
      if (iParaWert AND iMaske) <> 0 then begin  // Bit gesetzt im Fehlerregister ?
        case iBit of
           1: iGerZustand:=iGerZustand OR (iBuf);         // Bit 0
           2: iGerZustand:=iGerZustand OR (iBuf SHL 1);   // Bit 1
           3: iGerZustand:=iGerZustand OR (iBuf SHL 2);   // Bit 2
           4: iGerZustand:=iGerZustand OR (iBuf SHL 3);   // Bit 3
           5: iGerZustand:=iGerZustand OR (iBuf SHL 4);   // Bit 4
           6: iGerZustand:=iGerZustand OR (iBuf SHL 5);   // Bit 5
           7: iGerZustand:=iGerZustand OR (iBuf SHL 6);   // Bit 6
           8: iGerZustand:=iGerZustand OR (iBuf SHL 7);   // Bit 7
           9: iGerZustand:=iGerZustand OR (iBuf SHL 8);   // Bit 8
          10: iGerZustand:=iGerZustand OR (iBuf SHL 9);   // Bit 9
          11: iGerZustand:=iGerZustand OR (iBuf SHL 10);  // Bit 10
          12: iGerZustand:=iGerZustand OR (iBuf SHL 11);  // Bit 11
          16: iGerZustand:=iGerZustand OR (iBuf SHL 12);  // Bit 12
        end;
      end;

      iMaske:=iMaske SHL 1;
    end;  { for iBit }
  end

  // Warnungregister
  else if (sAllgNum = CP_RMG_TME400_Warnungregister) then begin
    iParaWert:=StrToInt64Def ('$' + sParaWert, -1);
    if iParaWert < 0 then exit;  // Gültige Bitleiste ab 0

    iMaske:=1;
    for iBit:=0 to 31 do begin
      Application.ProcessMessages;
      if (iParaWert AND iMaske) <> 0 then begin  // Bit gesetzt im Warnungregister ?
        case iBit of
           1: iGerZustand:=iGerZustand OR (iBuf SHL 13);  // Bit 13
        end;
      end;

      iMaske:=iMaske SHL 1;
    end;  { for iBit }
  end

  // Hinweisregister
  else if (sAllgNum = CP_RMG_TME400_Hinweisregister) then begin
    iParaWert:=StrToInt64Def ('$' + sParaWert, -1);
    if iParaWert < 0 then exit;  // Gültige Bitleiste ab 0

    iMaske:=1;
    for iBit:=0 to 31 do begin
      Application.ProcessMessages;
      if (iParaWert AND iMaske) <> 0 then begin  // Bit gesetzt im Hinweisregister ?
        case iBit of
           1: iGerZustand:=iGerZustand OR (iBuf SHL 14);   // Bit 14
           2: iGerZustand:=iGerZustand OR (iBuf SHL 15);   // Bit 15
           3: iGerZustand:=iGerZustand OR (iBuf SHL 16);   // Bit 16
        end;
      end;

      iMaske:=iMaske SHL 1;
    end;  { for iBit }
  end;
  Result:=true;
end;

// 04.04.2024, WW
{---------------------------------------------------------------------}
function SetGeraeteZustand_RSM200 (sParaWert: string; sAllgNum: string;
  var iGerZustand: Int64): boolean;
{---------------------------------------------------------------------}
{ Setzt Bits im RSM200-Gerätezustand;
  Übergabe: Parameterwert
            Allgemeine Parameternummer, zu der der Parameterwert gehört
  Übergabe/Rückgabe: Gerätezustand }
var
  iParaWert: Int64;
  iMaske: cardinal;
  iBit: integer;
  iBuf: Int64;

begin
  Result:=false;
  iBuf:=$01;  // zum Bit shiften

  // Fehlerregister
  if (sAllgNum = CP_RMG_RSM200_Fehlerregister) then begin
    iParaWert:=StrToInt64Def ('$' + sParaWert, -1);
    if iParaWert < 0 then exit;  // Gültige Bitleiste ab 0

    iMaske:=1;
    for iBit:=0 to 31 do begin
      Application.ProcessMessages;
      if (iParaWert AND iMaske) <> 0 then begin  // Bit gesetzt im Fehlerregister ?
        case iBit of
           1: iGerZustand:=iGerZustand OR (iBuf);         // Bit 0
           2: iGerZustand:=iGerZustand OR (iBuf SHL 1);   // Bit 1
           3: iGerZustand:=iGerZustand OR (iBuf SHL 2);   // Bit 2
           4: iGerZustand:=iGerZustand OR (iBuf SHL 3);   // Bit 3
           5: iGerZustand:=iGerZustand OR (iBuf SHL 4);   // Bit 4
           6: iGerZustand:=iGerZustand OR (iBuf SHL 5);   // Bit 5
          10: iGerZustand:=iGerZustand OR (iBuf SHL 6);   // Bit 6
          11: iGerZustand:=iGerZustand OR (iBuf SHL 7);   // Bit 7
          12: iGerZustand:=iGerZustand OR (iBuf SHL 8);   // Bit 8
          17: iGerZustand:=iGerZustand OR (iBuf SHL 9);   // Bit 9
          18: iGerZustand:=iGerZustand OR (iBuf SHL 10);  // Bit 10
          19: iGerZustand:=iGerZustand OR (iBuf SHL 11);  // Bit 11
          20: iGerZustand:=iGerZustand OR (iBuf SHL 12);  // Bit 12
          22: iGerZustand:=iGerZustand OR (iBuf SHL 13);  // Bit 13
          23: iGerZustand:=iGerZustand OR (iBuf SHL 14);  // Bit 14
          24: iGerZustand:=iGerZustand OR (iBuf SHL 15);  // Bit 15
          25: iGerZustand:=iGerZustand OR (iBuf SHL 16);  // Bit 16
          26: iGerZustand:=iGerZustand OR (iBuf SHL 17);  // Bit 17
          27: iGerZustand:=iGerZustand OR (iBuf SHL 18);  // Bit 18
          28: iGerZustand:=iGerZustand OR (iBuf SHL 19);  // Bit 19
        end;
      end;

      iMaske:=iMaske SHL 1;
    end;  { for iBit }
  end

  // Warnungregister
  else if (sAllgNum = CP_RMG_RSM200_Warnungregister) then begin
    iParaWert:=StrToInt64Def ('$' + sParaWert, -1);
    if iParaWert < 0 then exit;  // Gültige Bitleiste ab 0

    iMaske:=1;
    for iBit:=0 to 31 do begin
      Application.ProcessMessages;
      if (iParaWert AND iMaske) <> 0 then begin  // Bit gesetzt im Warnungregister ?
        case iBit of
           1: iGerZustand:=iGerZustand OR (iBuf SHL 20);  // Bit 20
           2: iGerZustand:=iGerZustand OR (iBuf SHL 21);  // Bit 21
           3: iGerZustand:=iGerZustand OR (iBuf SHL 22);  // Bit 22
           4: iGerZustand:=iGerZustand OR (iBuf SHL 23);  // Bit 23
          20: iGerZustand:=iGerZustand OR (iBuf SHL 24);  // Bit 24
          21: iGerZustand:=iGerZustand OR (iBuf SHL 25);  // Bit 25
          22: iGerZustand:=iGerZustand OR (iBuf SHL 26);  // Bit 26
          23: iGerZustand:=iGerZustand OR (iBuf SHL 27);  // Bit 27
        end;
      end;

      iMaske:=iMaske SHL 1;
    end;  { for iBit }
  end

  // Hinweisregister
  else if (sAllgNum = CP_RMG_RSM200_Hinweisregister) then begin
    iParaWert:=StrToInt64Def ('$' + sParaWert, -1);
    if iParaWert < 0 then exit;  // Gültige Bitleiste ab 0

    iMaske:=1;
    for iBit:=0 to 31 do begin
      Application.ProcessMessages;
      if (iParaWert AND iMaske) <> 0 then begin  // Bit gesetzt im Hinweisregister ?
        case iBit of
           1: iGerZustand:=iGerZustand OR (iBuf SHL 28);   // Bit 28
           2: iGerZustand:=iGerZustand OR (iBuf SHL 29);   // Bit 29
           3: iGerZustand:=iGerZustand OR (iBuf SHL 30);   // Bit 30
           4: iGerZustand:=iGerZustand OR (iBuf SHL 31);   // Bit 31
           5: iGerZustand:=iGerZustand OR (iBuf SHL 32);   // Bit 32
           6: iGerZustand:=iGerZustand OR (iBuf SHL 33);   // Bit 33
           7: iGerZustand:=iGerZustand OR (iBuf SHL 34);   // Bit 34
           8: iGerZustand:=iGerZustand OR (iBuf SHL 35);   // Bit 35
           9: iGerZustand:=iGerZustand OR (iBuf SHL 36);   // Bit 36
          10: iGerZustand:=iGerZustand OR (iBuf SHL 37);   // Bit 37
          11: iGerZustand:=iGerZustand OR (iBuf SHL 38);   // Bit 38
          12: iGerZustand:=iGerZustand OR (iBuf SHL 39);   // Bit 39
          13: iGerZustand:=iGerZustand OR (iBuf SHL 40);   // Bit 40
          14: iGerZustand:=iGerZustand OR (iBuf SHL 41);   // Bit 41
          15: iGerZustand:=iGerZustand OR (iBuf SHL 42);   // Bit 42
          16: iGerZustand:=iGerZustand OR (iBuf SHL 43);   // Bit 43
        end;
      end;

      iMaske:=iMaske SHL 1;
    end;  { for iBit }
  end;
  Result:=true;
end;

end.

