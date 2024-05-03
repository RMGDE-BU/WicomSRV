{******************************************************************************}
{* Version: 16.01.98  WW                                                      *}
{******************************************************************************}
Unit LEDConst;

INTERFACE

uses
  SysUtils, Classes;

Const
  Neustart_noetig: boolean = false;   { wenn wichtige Verzeichnisse geändert werden }

  C_StdDirectory = 'C:\';

  ma_Grafik  = 'G';
  ma_Fenster = 'F';

  md_Arbeit   = 'W';
  md_Leistung = 'P';

  { Hilfe-Kontext-IDs: }
  HID_Inhalt            = 0;
  HID_LAKSPasswort      = 3;
  HID_Momentanwerte     = 5;
  HID_Verzeichnisse     = 6;
  HID_MomentanwerteEdit = 8;


type
  string1  = string[1];

var
  { Verzeichnisse }
  V_StammDir: TFilename;
  V_DArchivDir: TFileName;
  V_LArchivDir: TFileName;
  V_ComserveDir: TFileName;

IMPLEMENTATION

End.
