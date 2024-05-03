{------------------------------------------------------------------------------}
{ 14.04.1999 GD; Lizenzabfrage f�r verteiltes System                           }
{                                                                              }
{                                                                              }
{ Anwendung: Nach letzten 'CreateForm' einf�gen (im Beispiel mit Instanztest)  }
{  if not TerminateSecondInstance(Application.MainForm.ClassName,              }
{                          Application.MainForm.Caption,                       }
{                          True)                                               }
{  then begin                                                                  }
{    if not WLogin32 then Application.Terminate else begin                     }
{      try                                                                     }
{        Application.Run;                                                      }
{      finally                                                                 }
{        WLogout32;                                                            }
{      end;                                                                    }
{    end;                                                                      }
{  end;                                                                        }
{                                                                              }
{ 16.06.2000 WW; Lizenzabfrage f�r MRG- und DSfG-Stationen                     }
{                -> Feld "Filename" = "MRG-Stationen" bzw. "DSfG-Stationen"    }
{                -> es wird nur das Feld "Anzahl Lizenzen" ausgewertet         }
{                                                                              }
{ 18.07.2001 WW; Lizenzabfrage f�r Programm-Funktionen                         }
{ 22.02.2002 GD; Aktuelle User vorgestellt                         	           }
{ 29.04.2002 GD  Z�hler geht bei 'unbegrenzt' nicht mehr unter '0', 	         }
{                Parameter f�r WISERV                                          }
{ 04.11.2002 GD  Speicher f�r Records mit '0' vorbelegt                        }
{ 17.02.2003 GD  Verschl�sselung der strings eingef�hrt                        }
{ 11.03.2003 GD  Pr�fung auf Lizenz Exe/Laufzeit                               }
{ 01.09.2003 GD  Pr�fung in LogIn32, ob Lizenzeintrag verschl�sselt ist        }
{ 10.09.2003 WW  erweiterte Stationslizenzierung f�r MRG und DSfG              }
{ 31.08.2004 GD  Optionale interaktive Schl�sseleingabe f�r PC-Name, Bugfix    }
{ 19.01.2005 GD  Lizenzhandling bei mehrfach gestarteten Applikationen         }
{ 23.03.2005 WW  Konstanten f�r Lizenz 'Export ASCII', 'Export DB'             }
{ 26.07.2005 GD  Absicherung bei WLogOut32, befristete Lizenz l�schen          }
{ 22.12.2005 GD  Lizenzeintrag f�r Mailweiterleitung                           }
{ 13.02.2008 GD  Bug in Teilnehmerliste beseitigt                              }
{ 03.12.2008 NW  Lizenzeintrag f�r Importformate DWD und ASCII                 }
{ 12.02.2009 NW  Lizenzeintrag f�r Importformat EWB                            }
{ 25.03.2009 GD  Laufzeitbegrenzung einzelner Funktionen                       }
{ 04.09.2009 NW  Lizenzeintrag f�r Export MSCONS                               }
{ 29.10.2009 GD  Erweiterte Lizenzeintr�ge f�r WISERV                          }
{ 21.12.2009 WW  Lizenzeintrag f�r Importformat 'Tritschler MoTe'              }
{ 05.02.2010 WW  DSfG-B-Stationslizenzierung                                   }
{ 09.09.2010 WN  Lizenzeintrag f�r Importformat 'VB-MINI Auswertesoftware'     }
{ 07.03.2012 WW  Lizenzierung Firmware-Update                                  }
{ 30.10.2012 WW  Lizenzeintrag f�r Importformat MSCONS                         }
{ 20.02.2013 WN  Lizenzeintrag f�r Automatik-Report (Ascii)                    }
{ 27.02.2013 WW  Lizenzeintrag Fern-Parametrierung f�r DF�-Installer           }
{ 29.11.2013 WW  Lizenzeintrag Wico22-Netzwerk-Kommunikation f�r Wiserv        }
{ 07.04.2014 WW  Lizenzierung Signatur                                         }
{ 04.06.2014 WW  Pr�fung in GetExeLaufzeit optional ohne 'unbegrenzt'          }
{ 08.01.2015 WW  Property IsOk                                                 }
{ 27.03.2015 WW  ReadProgrammfunktionFromLizenzFile mit optionaler �bergabe des}
{                Exe-Namens                                                    }
{ 05.08.2016 WW  GetSeriennnummer                                              }
{ 29.03.2017 WW  An- und Abmelden zus�tzlich mit Benutzername (GetTeilnehmer ) }
{ 30.01.2018 WW  Pr�fung Programm-Funktions-Anzahl                             }
{ 28.03.2018 WW  Lizenzeintrag f�r Importformat ABL                            }
{ 26.04.2018 WW  Lizenzeintrag f�r SMS-Alarm                                   }
{ 01.08.2018 WW  CheckProgrammFktLizenzCount mit optionaler �bergabe des Exe-  }
{                Namens                                                        }
{ 02.03.2020 WW  GetLizenzFileValid                                            }
{ 28.09.2020 WW  GetFileDateTime, FileDateTimeChanged                          }
{ 28.01.2021 WW  Lizenzeintrag f�r Importformat Primus/Prilog FTP              }
{ 09.07.2021 WW  �berarbeitung zur Absicherung von gleichzeitigen Schreib-     }
{                zugriffen auf die Lizenzdatei: �bergabe-Flag "ReadWrite" im   }
{                Konstruktor der Lizenz-Klasse; Speichern der Lizenzdatei: ohne}
{                erneutes Datei-�ffnen, Zwischenspeichern mit AddIntoLizenzFile}
{                und Tempor�rdatei eliminiert; Property LastError mit Fehler-  }
{                codes beim An-/Abmelden und Speichern; L�schen des Lizenz-    }
{                eintrags beim Anmelden mit abgelaufener Lizenz rausgenommen   }
{                (DeleteLizenzEntry)                                           }
{ 01.03.2022 WW  GetStationsLizenzText                                         }
{ 31.01.2023 WW  Lizenzliste mit Lizenz-Basismethoden, LoadFromLizenzlist      }
{ 09.02.2023 WW  Lizenzeintrag f�r Modbus-Alarm                                }
{                                                                              }
{ (C) Karl Wieser GmbH 1999, RMG Messtechnik GmbH 2023                         }
{------------------------------------------------------------------------------}
unit Lizenz32;

interface

uses
  Windows, Forms, Controls, SysUtils, Classes, PathIni, T_Zeit, novell, Dialogs,
  InstTest, DB_Attn;

const
  C_Lizenz_Filename           = 'WNET_LIZ.DAT';
  C_Lizenz_Encoded            = False;  // Vorgabe: keine Verschl�sselung, 17.02.2003/01.09.2003 variabel
  C_Lizenz_MyEncoded : boolean = C_Lizenz_Encoded;  // 01.09.2003 variabel

  C_Lizenz_NoError            =  0;
  C_Lizenz_NoFile             = -1;
  C_Lizenz_MaxError           = -2;
  C_Lizenz_TimeError          = -3;
  C_Lizenz_NameError          = -4;
  C_Lizenz_InsertError        = -5;
  C_Lizenz_DeleteError        = -6;
  C_Lizenz_AnzahlKleinerNull  = -7;
  C_Lizenz_VersionError       = -8;

  { Filename-Eintr�ge im Lizenzrecord f�r Stationslizenzierung }
  FN_MRG_Stationen  = 'MRG-Stationen';
  FN_DSFG_Stationen = 'DSfG-Stationen';
  FN_DSFG_B         = 'DSFG-B-';  // 05.02.2010, WW
  FN_MRG_WIESER_ALLE_MRG_FREI     = 'MRG-WIESER_ALLE_MRG_FREI';
  FN_MRG_WIESER_MRG800_910_FREI   = 'MRG-WIESER_MRG800_910_FREI';

  //-> f�r eine globale Funktion-Freischaltung mu� im Lizenzfile-Record das Feld
  //   "Filename" mit 'FKT-' und der Funktion versehen sein
  FNExt_Funktion = 'FKT';

  { Filename-Erweiterungen im Lizenzrecord f�r Programmfunktions-Lizenzierung }
  { Global im WK22-System: }
  FNExt_RufentgegennahmeMRG    = '-Rufentgegennahme_MRG';
  FNExt_RufentgegennahmeDSfG   = '-Rufentgegennahme_DSfG';
  FNExt_SMS_Datenempfang       = '-SMS_Datenempfang';
  FNExt_GPRS                   = '-GPRS';  // 19.03.2009, WW
  FNExt_ParametrierungMRG      = '-Parametrierung_MRG';
  FNExt_ParametrierungDSfG     = '-Parametrierung_DSFG';
  FNExt_ParametrierungDSfGDfue = '-Parametrierung_DSFGDFUE';
  FNExt_ExportASCII            = '-Export_ASCII';    // 23.03.2005, WW
  FNExt_ExportDB               = '-Export_DB';       // 23.03.2005, WW
  FNExt_ExportMSCONS           = '-Export_MSCONS';   // 04.09.2009, WN
  FNExt_Signatur               = '-Signatur';  // 07.04.2014, WW
  FNExt_ModbusAlarm            = '-ModbusAlarm';  // 09.02.2023, WW

  { einem einzelnen Programm zugeordnet: }
  C_WISERVParam_Archivabfrage = '-Archivabfrage';  // f�r WISERV - Archivabfrage
  C_WISERVParam_CommWICOM = '-CommWICOM';  // f�r WISERV - Kommunikation WICO22
  C_WISERVParam_CommDfu   = '-CommDFU';    // f�r WISERV - Kommunikation DF�
  C_WISERVParam_CommCard  = '-CommCard';   // f�r WISERV - Kommunikation DPA
  C_WISERVParam_CommV24   = '-CommV24';    // f�r WISERV - Kommunikation seriell
  C_WISERVParam_CommLAN   = '-CommLAN';    // f�r WISERV - Kommunikation WICO22, nur Netzwerk
  C_WAlarm_MailAlarm      = '-MailAlarm';  // f�r WAlarm - Mailweiterleitung
  C_WAlarm_SMSAlarm       = '-SMSAlarm';   // f�r WAlarm - SMS-Weiterleitung; 26.04.2018, WW

  FNExt_Rufentgegennahme = '-Rufentgegennahme';   { f�r Abrufmodule }
  FNExt_Parametrierung   = '-Parametrierung';     { f�r WISERV, MOM32 }
  FNExt_CommDFU          = C_WISERVParam_CommDfu; { f�r DF�-Installer }

  FNExt_MRG  = '-MRG';                              { f�r Leitwarten-Kopplung }
  FNExt_DSfG = '-DSFG';                             {         "               }
  FNExt_KZW  = '-KZW';                              {         "               }
  FNExt_101  = '-101';                              {         "               }
  FNExt_104  = '-104';                              {         "               }

  FNExt_FormatLPEX      = '-LPEX';                  { f�r Importprogramm }
  FNExt_FormatElsterAGR = '-ELSTERAGR';             {         "          }
  FNExt_FormatRuhrgas   = '-RUHRGAS';               {         "          }
  FNExt_FormatDWD       = '-DWD';                   {         "          }      // 03.12.2008 NW
  FNExt_FormatASCII     = '-ASCII';                 {         "          }      // 03.12.2008 NW
  FNExt_FormatEWB       = '-EWB';                   {         "          }      // 12.02.2009 NW
  FNExt_FormatVeriboxCSV  = '-VERIBOXCSV';          {         "          }      // 07.07.2009 WW
  FNExt_FormatFTLMoTe     = '-FTLMOTE';             {         "          }      // 21.12.2009 WW
  FNExt_FormatVeriboxMini = '-VERIBOXMINI';         {         "          }      // 09.09.2010 WN
  FNExt_FormatMSCONS    = '-MSCONS';                {         "          }      // 30.10.2012 WW
  FNExt_FormatGoerlitzABL = '-GOERLITZABL';         {         "          }      // 28.03.2018 WW
  FNExt_FormatPrimusPrilogFTP = '-PRIMUSPRILOGFTP'; {         "          }      // 28.01.2021 WW

  FNExt_Archive  = '-ARCHIVE';                      { f�r GM-Monitor (SupMon) } // 30.01.2018, WW
  FNExt_Displays = '-DISPLAYS';                     {         "               }

  FNExt_Datapoints = '-DATAPOINTS';                 { f�r DPCalc } // 30.01.2018, WW

  // Datentypen
  FNExt_Datentyp_GBHRev1 = '-Datentyp_1024';                 { GBHRevision1 }
  FNExt_Datentyp_Params  = '-Datentyp_0008';                 { Parameter }
  FNExt_Datentyp_RufFTL  = '-Datentyp_1000';                 { Pr�falarmFTL }
  FNExt_Datentyp_FwUpdate = '-Datentyp_2000';                { Firmware-Update }
  FNExt_Datentyp_Report   = '-Datentyp_8000';                { Auto-Report }    // 20.02.2013  WN

  C_Lizenz_MrgTyp     = 'MRG-Typ: ';
  C_Lizenz_PC_ID      = 'PC-ID: ';

  C_Lizenz_PC_ID_beliebig = '<BELIEBIG>';   { Eintrag schaltet jeden PC frei }

  C_MaxLength_ExeName = 80;  // 04.11.2002 -> ACHTUNG: mit vier F�llzeichen Code

  C_Encode_Offset  = 50;
  C_Encode_Zyklus  = 5;
  C_Encode_Step    = 3;

  CMsgLicExe         = 'Das Programm ist nicht lizenziert !';
  CMsgLicExeLaufzeit = 'Die Programmlizenz ist abgelaufen !';
  CMsgLicPC          = 'Das Programm ist f�r diesen Rechner nicht lizenziert !';

resourcestring
  SMsgLicExe         = CMsgLicExe;
  SMsgLicExeLaufzeit = CMsgLicExeLaufzeit;
  SMsgLicPC          = CMsgLicPC;
  SMsgGetLicPc       = 'Bitte geben Sie den Freischaltcode zu folgendem ' +
                       'System ein:'#13#10#13#10'%s'#13#10#13#10 +
                       '(zu beziehen �ber die RMG Messtechnik GmbH)';
  SMsgLizFuer        = 'Lizenzierung f�r ';
  SMsgLizWriteError  = 'Lizenzdatei konnte nicht geschrieben werden.';

type
  PLizenzRec = ^TLizenzRec;
  TLizenzRec = packed record
{$H-}
    ExeName       : string[C_MaxLength_ExeName];  { Name des Exe-Files }
{$H+}
    LastExeDate   : TDateTime;   { max. Datum des Exe-Files }
    LizenzCount   : SmallInt;    { Anzahl der Lizenzen f�r dieses Exe (>100 = unbegrenzt; -1 = Stationen unbegrenzt }
    LastLizenzDate: TDateTime;   { Evtl. Zeitbegrenzung - 0: unbegrenzt }
    CurrentCount  : SmallInt;    { Gegenw�rtig aktive Clients dieses Exes }
  end;

  TLizenzList = class(TStringList)
    destructor Destroy; override;
  private
    function GetLizenzRecord(anExe: string): TLizenzRec;
  public
    procedure Delete(iIndex: integer); override;
    procedure Clear; override;
    procedure LoadFromLizenzlist (LL_Src: TLizenzList);
    function GetExeLaufzeit(sExe: string; bAllow_Unbegrenzt: boolean = true): boolean;
    function ReadGlobaleFunktionFromLizenzFile(FktName: string): boolean;
    function GetLizenzMrgTyp(iTyp: integer): integer;
  end;

  TWLizenz32 = class(TObject)
    constructor Create(aFile: string = ''; bEncode: boolean = C_Lizenz_Encoded;
      bKeepFileOpenforReadWrite: boolean = false);
    destructor Destroy; override;
  private
    FLizenzFileName : TFileName;   { Name der Lizenzdatei }
    FLizenzFileStream : TFileStreamExt;  { Lizenzdatei-Stream; 09.07.2021, WW }
    FExeList       : TLizenzList;  { Stringlist mit den Eintr�gen in der Lizenzdatei }
    FMembers       : TStrings;     { Stringliste mit angemeldeten Computern }
    FIsOk          : boolean;      { Flag, ob die Datei vorhanden ist }
    FEncoded       : boolean;      { Flag, ob strings verschl�sselt sind - 17.02.2003}
    FdtLizenzFile  : TDateTime;    { Zeitstempel der Lizenzdatei }
    FLastError     : integer;      { Code des letzten aufgetretenen Fehlers; 09.07.2021, WW }
    function GetWieserLizenzFile: string;
    procedure ReadTeilnehmer;
    procedure ReadExeEntries;
    function IsTeilnehmer(sText: string): boolean;
    function AktuTeilnehmer(sText: string; bInsert: boolean = True): boolean;
    function AktuLizenzRecord_CurrentCount(iIndex: integer;
      iOffset: integer = 1): boolean;  // 09.07.2021, WW
    function GetSeriennummer: string;
    function GetFileDateTime: TDateTime;
  public
    procedure ShowLizenzDialog(anError: SmallInt);
    procedure AddLizenzEntry(aName: string; aLCount, aCCount: SmallInt;
                             anExeDate, aLIzenzDate: TDateTime);
    function WriteToLizenzFile(aFile: string; aDatum: TDateTime): SmallInt;
    function DeleteFromLizenzFile(aFile: string): SmallInt;
    function GetLizenzRecord(anExe: string): TLizenzRec;
    function ReadStationenFromLizenzFile(afn: string; vorhandene_Stationen: integer): SmallInt;
    procedure GetStationsLizenzText(afn: string; anError: SmallInt;
      var aCaption: string; var aText: string);  // 01.03.2022, WW
    procedure ShowStationsLizenzDialog(afn: string; anError: SmallInt);
    function CreateListDSfG_B_Geraetetypen_frei: TStrings;
    function ReadDSfG_B_Geraetetyp_frei (AGerTypName: string): boolean;  // 05.02.2010, WW
    function ReadMrgTypGrp_Wieser_frei: boolean;
    function ReadMrgTypGrp_Wieser800_910_frei: boolean;
    function ReadProgrammfunktionFromLizenzFile(FktName: string;
      ExeName: string = ''): boolean;
    function ReadGlobaleFunktionFromLizenzFile(FktName: string): boolean;
    function GetLizenzMrgTyp(iTyp: integer): integer;  // 17.02.2003
    function GetLizenzPC(sPCID: string): boolean;      // 17.02.2003
    function GetLizenzExe(sExe: string): boolean;      // 11.02.2003
    function GetExeLaufzeit(sExe: string; bAllow_Unbegrenzt: boolean = true): boolean;    // 11.02.2003
    function SaveLizenzdatei (bKillLizenz: boolean = false): boolean;
    function GetTeilnehmer(sText: string): string;
    function CheckProgrammFktLizenzCount(sFktName: string; iCountVorhanden: integer;
      var iCountLizenz: integer; ExeName: string = ''): string;
    function GetLizenzFileValid: boolean;
    function FileDateTimeChanged: boolean;  // 28.09.2020, WW

    property ExeList: TLizenzList read FExeList;
    property MemberList: TStrings read FMembers;
    property Encoded: boolean read FEncoded write FEncoded;
    property IsOk: boolean read FIsOk;
    property Seriennummer: string read GetSeriennummer;
  end;

function WLogin32(sFile: string = ''): boolean;
procedure WLogOut32(sFile: string = '');
function MyGetComputerName: string;
function CheckComputerName(sFile: string = ''): boolean;  // 31.08.2004

function CharStrToIntStr(sString: string): string;
function IntStrToCharStr(sString: string): string;

function EncodeLizenzString(sString: string; iOffset: byte = C_Encode_Offset;
  iZyklus: byte = C_Encode_Zyklus; iStep: byte = C_Encode_Step): string;
function DecodeLizenzString(sString: string; iOffset: byte = C_Encode_Offset;
  iZyklus: byte = C_Encode_Zyklus; iStep: byte = C_Encode_Step): string;

implementation

resourcestring
  SDateFormat = 'dd.mm.yyyy';

  SMsgLizAnmeldFuer = 'Lizenzanmeldung f�r ';
  SMsgLizProgFehlt  = 'Programm ist nicht lizenziert (Lizenzdatei fehlt) !';
  SMsgLizMaxAnz     = 'Maximale Anzahl der Lizenzen (%s) ist �berschritten !';
  SMsgLizProgVers   = 'Nicht lizenzierte Programmversion !' + #13 +
                      'Lizenz gilt nur f�r Versionen bis %s.';
  SMsgLizAbgel      = 'Lizenz ist am %s abgelaufen !';
  SMsgLizProg       = 'Programm ist nicht lizenziert !';
  SMsgLizProgAnmeld = 'Fehler beim Anmelden des Programms !';
  SMsgLizProgAbmeld = 'Fehler beim Abmelden des Programms !';
  SMsgLizAnzNeg     = 'Anzahl der angemeldeten Programminstanzen ist kleiner als Null !';
  SMsgLizUnbekannt  = 'Unbekannter Lizenzfehler !';

  SMsgLizKeineAbrufBerecht = 'Es bestehen keine Abrufberechtigungen (Lizenzdatei fehlt) !';
  SMsgLizMaxAbrufBerecht   = 'Maximale Anzahl der Abrufberechtigungen (%s) ist �berschritten !';
  SMsgLizAbrufBerechtFehlt = 'Es bestehen keine Abrufberechtigungen !';

  SMsgLizKeineFreischaltung = 'Es liegt keine Freischaltung vor (Lizenzdatei fehlt) !';
  SMsgLizMaxAnzahl          = 'Maximale Anzahl (%s) ist �berschritten !';
  SMsgLizFreischaltungFehlt = 'Es liegt keine Freischaltung vor !';

  sMsgErrorCode = 'Fehlercode: %d';  // 09.07.2021, WW


{----------------------------------------------------}
function MyGetComputerName: string;
{----------------------------------------------------}
var
  p : PChar;
  i : DWord;
begin
  GetMem(p, 100);
  try
    p[0] := #0;
    i := 99;
    GetComputerName(p, i);
    Result := StrPas(p);
  finally
    FreeMem(p, 100);
  end;
end;

{ Verschl�sselt/Entschl�sselt einen string           }
{ Parameter: Faktor, Text, Verschl�sselungsparameter }
{ R�ckgabe: Resultierender String                    }
{----------------------------------------------------}
function CodeLizenzString(iFaktor: integer; sString: string;
  iOffset: byte = C_Encode_Offset; iZyklus: byte = C_Encode_Zyklus;
  iStep: byte = C_Encode_Step): string;
{----------------------------------------------------}
var
  i, iAmplitude : byte;
  s             : string;
begin
  s := sString;

  // Zus�tzlich Dummyzeichen
  if (iFaktor = 1) then begin  // Verschl�sseln
    s := Chr(iOffset + Random(iOffset)) + Chr(iOffset + Random(iOffset)) +
      Copy(s, 1, 5) + Chr(iOffset + Random(iOffset)) + Copy(s, 6, Length(s)-5) +
      Chr(iOffset + Random(iOffset));
  end;

  // Verschl�sseln
  iAmplitude := iOffset;
  for i := 1 to Length(s) do begin
    if (Odd(i div iZyklus))
    then Dec(iAmplitude, iStep)
    else Inc(iAmplitude, iStep);
    s[i] := Chr(Ord(s[i]) + (iFaktor*iAmplitude));
  end;

  // Zus�tzlich Dummyzeichen entfernen
  if (iFaktor = -1) then begin  // Entschl�sseln
    // Ersten beiden Dummy-Zeichem entfernen
    s := Copy(s, 3, Length(s)-2);
    // Letztes Dummy-Zeichem entfernen
    s := Copy(s, 1, Length(s)-1);
    // Wenn verbleibende L�nge von s < 6, dann ist der Text <= 5
    if (Length(s) < 6)
    then s := Copy(s, 1, Length(s)-1)
    else s := Copy(s, 1, 5) + Copy(s, 7, Length(s)-6);
  end;

  Result := s;
end;

{ Gibt verschl�sselten string zur�ck                 }
{ Parameter: Klartext, Verschl�sselungsparameter     }
{ R�ckgabe: Mit Parametern verschl�sselter String    }
{----------------------------------------------------}
function EncodeLizenzString(sString: string; iOffset: byte = C_Encode_Offset;
  iZyklus: byte = C_Encode_Zyklus; iStep: byte = C_Encode_Step): string;
{----------------------------------------------------}
begin
  Result := CodeLizenzString(1, sString, iOffset, iZyklus, iStep);
end;

{ Gibt entschl�sselten string zur�ck                 }
{ Parameter: Cod. Text, Verschl�sselungsparameter    }
{ R�ckgabe: Entschl�sselter String                   }
{----------------------------------------------------}
function DecodeLizenzString(sString: string; iOffset: byte = C_Encode_Offset;
  iZyklus: byte = C_Encode_Zyklus; iStep: byte = C_Encode_Step): string;
{----------------------------------------------------}
begin
  Result := CodeLizenzString(-1, sString, iOffset, iZyklus, iStep);
end;
{----------------------------------------------------}
function CharStrToIntStr(sString: string): string;
{----------------------------------------------------}
var
  sHexChar    : string;
  i, j : integer;
begin
  Result := '';
  try
    for i := Length(sString) downto 1 do begin
      sHexChar := IntToHex(Ord(sString[i]), 2);
      for j := Length(sHexChar) downto 1 do begin
        if (sHexChar[j] in ['A'..'F']) then
          sHexChar[j] := IntToStr(Ord(sHexChar[j]) - Ord('A'))[1]
        else if (sHexChar[j] in ['0'..'9']) then
          sHexChar[j] := Chr(Ord('A') + StrToInt(sHexChar[j]));
        Result := Result + sHexChar[j];
      end;
    end;
  except
  end;
end;

{----------------------------------------------------}
function IntStrToCharStr(sString: string): string;
{----------------------------------------------------}
var
  s, sHexChar : string;
  i, j        : integer;
begin
  sString := UpperCase(sString);
  Result := '';
  try
    i := Length(sString) - 1;
    while (i >= 1) do begin
      sHexChar := Copy(sString, i, 2);
      s := '';
      for j := Length(sHexChar) downto 1 do begin
        if (sHexChar[j] in ['A'..'Z']) then
          sHexChar[j] := IntToStr(Ord(sHexChar[j]) - Ord('A'))[1]
        else if (sHexChar[j] in ['0'..'9']) then
          sHexChar[j] := Chr(Ord('A') + StrToInt(sHexChar[j]));
        s := s + sHexChar[j];
      end;
      Result := Result + Chr(StrToInt('$' + s));
      Dec(i, 2);
    end;
  except
  end;
end;

{----------------------------------------------------}
function CheckComputerName(sFile: string = ''): boolean;  // 31.08.2004
{----------------------------------------------------}
var
  s, sCoded, sPcName : string;
  i : integer;

begin
  sPcName := MyGetComputerName;
  with TWLizenz32.Create(sFile, C_Lizenz_MyEncoded, true) do  // mit Schreibzugriff; 09.07.2021, WW
  try
    Result := GetLizenzPC(sPcName);
    sCoded := CharStrToIntStr(sPcName);
    if (not Result) and (MessageDlg(
        Format(SMsgGetLicPc, [sCoded]), mtInformation,
        [mbOk, mbCancel], 0) = mrOk) then
    begin
      s := sCoded;
      if (InputQuery(SMsgLicPC, 'Code: >' + sCoded + '<:', s)) then begin
        i := 0;
        while (i < FExeList.Count) do begin
          if (Pos(C_Lizenz_PC_ID, FExeList[i]) = 1) then Break;
          Inc(i);
        end;

        sCoded := IntStrToCharStr(s);
        sCoded := DecodeLizenzString(sCoded);
        if (i = FExeList.Count) then
          AddLizenzEntry(C_Lizenz_PC_ID + sCoded, 0, 0, 0, 0)
        else begin
          FExeList[i] := C_Lizenz_PC_ID + sCoded;
          PLizenzRec(FExeList.Objects[i])^.ExeName := FExeList[i];
        end;

        Result := GetLizenzPC(sPcName);
        if (Result) then
          if not SaveLizenzdatei then      
            MessageDlg(SMsgLizWriteError, mtError, [mbOk], 0);  // 09.07.2021, WW
      end;
    end;
  finally
    Free;
  end;
  if (not Result) then MessageDlg(SMsgLicPC, mtInformation, [mbOk], 0);
end;


{------------------------------ Login, Logout ---------------------------------}

{ Meldet den Client im NetProgDir an                 }
{ R�ckgabe: True - OK; FALSE - Lizenz �berschritten  }
{----------------------------------------------------}
function WLogin32(sFile: string = ''): boolean;
{----------------------------------------------------}
var
  i, res   : integer;
  dt       : TDateTime;
  s        : string;
  Lizenz   : TWLizenz32;
begin
  i:= FileAge(Application.ExeName);
  dt:= FileDateToDateTime(i);

  s:= ExtractFileName(Application.ExeName);
  Lizenz:= TWLizenz32.Create(sFile, C_Lizenz_MyEncoded,  // 01.09.2003
                             true);  // mit Schreibzugriff; 09.07.2021, WW
  try
    res:= Lizenz.WriteToLizenzFile(s, dt);
    if (res < 0) then begin
      Lizenz.ShowLizenzDialog(res);
      result:= False;
    end
    else result:= True;
  finally
    Lizenz.Free;
  end;
end;

{ Meldet den Client im NetProgDir ab                 }
{----------------------------------------------------}
procedure WLogOut32(sFile: string = '');
{----------------------------------------------------}
var
  iRes   : integer;
  s      : string;
  Lizenz : TWLizenz32;
begin
  // Bei aktuell mehreren laufenden Instanzen: nichts machen // 19.01.2005
  if (Assigned(Application.MainForm)) and
    (AnzahlInstanzen(Application.MainForm.ClassName, '') > 1)
  then Exit;             // 26.07.2005

  s:= ExtractFileName(Application.ExeName);
  Lizenz:= TWLizenz32.Create(sFile, C_Lizenz_MyEncoded,  // 01.09.2003
                             true);  // mit Schreibzugriff; 09.07.2021, WW
  try
    iRes:= Lizenz.DeleteFromLizenzFile(s);
    if (iRes < 0) then begin
      Lizenz.ShowLizenzDialog(iRes);
    end;
  finally
    Lizenz.Free;
  end;
end;


{-------------------------------- TLizenzList ----------------------------------}

{----------------------------------------------------}
destructor TLizenzList.Destroy;
{----------------------------------------------------}
begin
  Clear;

  inherited;
end;

{----------------------------------------------------}
procedure TLizenzList.Delete(iIndex: integer);
{----------------------------------------------------}
begin
  if (Assigned(Objects[iIndex])) then Dispose(PLizenzRec(Objects[iIndex]));

  inherited;
end;

{----------------------------------------------------}
procedure TLizenzList.Clear;
{----------------------------------------------------}
var
  i : integer;
begin
  for i:= 0 to Count-1 do
    if (Assigned(Objects[i])) then
      Dispose(PLizenzRec(Objects[i]));

  inherited;
end;

{-------------------------------------------------------------}
procedure TLizenzList.LoadFromLizenzlist (LL_Src: TLizenzList);
{-------------------------------------------------------------}
{ Lizenzliste mit Inhalt von Quell-Liste f�llen (Kopie);
  �bergabe: zu kopierende Quell-Lizenzliste }
var
  i: integer;
  pRec: PLizenzRec;

begin
  Clear;  // Liste leeren
  if Assigned (LL_Src) then begin
    for i:=0 to LL_Src.Count - 1 do begin
      if Assigned (LL_Src.Objects[i]) then begin
        New(pRec);
        pRec^:=PLizenzRec(LL_Src.Objects[i])^;
        AddObject(LL_Src[i], TObject(pRec));
      end;
    end;
  end;
end;

{ Liest f�r ein Exe-File die Eintr�ge in ein Record  }
{ R�ckgabe: Record mit Lizenzinformationen           }
{----------------------------------------------------}
function TLizenzList.GetLizenzRecord(anExe: string): TLizenzRec;
{----------------------------------------------------}
var
  i     : integer;
begin
  { Default-Vorbelegung f�r result }
  result.ExeName:= 'Unbekannt';
  result.LastExeDate:= 0;
  result.LizenzCount:= 0;
  result.LastLizenzDate:= 0;
  result.CurrentCount:= 0;

  if (UpperCase(ExtractFileExt(anExe)) = '.EXE') or (Pos('\', anExe) > 0) then
    anExe := ChangeFileExt(ExtractFilename(anExe), '');
  anExe:= UpperCase(anExe);
  i:= Self.IndexOf(ANSIUpperCase(anExe));
  if (i > -1) then begin
    result.ExeName:= PLizenzRec(Self.Objects[i])^.ExeName;
    result.LastExeDate:= PLizenzRec(Self.Objects[i])^.LastExeDate;
    result.LizenzCount:= PLizenzRec(Self.Objects[i])^.LizenzCount;
    result.LastLizenzDate:= PLizenzRec(Self.Objects[i])^.LastLizenzDate;
    result.CurrentCount:= PLizenzRec(Self.Objects[i])^.CurrentCount;
  end;
end;

{ Gibt zur�ck, ob die Laufzeit eines EXEs g�ltig ist }
{ Parameter: EXE-Name                                }
{            Schalter 'Unbegrenzte Laufzeit erlaubt' }
{ R�ckgabe: Laufzeit noch g�ltig ja/nein             }
{----------------------------------------------------}
function TLizenzList.GetExeLaufzeit(sExe: string;
  bAllow_Unbegrenzt: boolean = true): boolean;
{----------------------------------------------------}
begin
  with GetLizenzRecord(sExe) do
    Result := (LizenzCount > 0) and
              ((bAllow_Unbegrenzt and (LastLizenzDate = 0)) or  // 04.06.2014, WW
               (LastLizenzDate >= Now));
end;

{ Pr�ft, ob eine globale (keinem einzelnen Programm zugeordnete) Funktion im
  Lizenzfile freigeschaltet ist }
{ Parameter: FktName = Funktion
                      (m�gliche Lizenz-Funktionen siehe Konstanten im interface-Teil) }
{ R�ckgabe: true, wenn Funktion freigeschaltet ist }
{ Hinweis:
  -> f�r eine globale Funktion-Freischaltung mu� im Lizenzfile-Record das Feld
     "Filename" mit 'FKT-' und der Funktion versehen sein
     (z.B. 'FKT-Rufentgegennahme') und im Feld "Anzahl" 1 (oder gr��er) stehen }
{-------------------------------------------------------------------------------}
function TLizenzList.ReadGlobaleFunktionFromLizenzFile(FktName: string): boolean;
{-------------------------------------------------------------------------------}
var
  sExe : string;
begin
  { Funktionsname: }
  sExe := FNExt_Funktion + FktName;
  // Pr�fung: Lizenzcount > 0 und ggf. Laufzeitbegrenzung  // 25.03.2009
  Result := GetExeLaufzeit(sExe);
end;

{ Gibt zur�ck, ob ein MRG-Typ lizensiert ist         }
{ Parameter: MRG-Typ (integer)                       }
{ R�ckgabe: Lizensiert (Anzahl: 0=F, -1/>0=T)        }
{----------------------------------------------------}
function TLizenzList.GetLizenzMrgTyp(iTyp: integer): integer;
{----------------------------------------------------}
var
  i : integer;
  s : string;
begin
  Result := 0;  // Default
  s := C_Lizenz_MrgTyp + IntToStr(iTyp);
  i := Self.IndexOf(ANSIUpperCase(s));
  if (i >= 0) then
    Result := PLizenzRec(Self.Objects[i])^.LizenzCount;
end;


{-------------------------------- TWLizenz32 ----------------------------------}

{ Konstruktor:                                                        }
{ �bergaben: Name der Lizenzdatei (leer = Lizenzdatei im Net-ProgDir, }
{              ggf. Exe-Dir)                                          }
{            Verschl�sselungsmodus ja/nein                            }
{            Flag "ReadWrite" (wenn true, wird Datei offen gehalten   }
{              f�r nachfolgende Schreibzugriffe)                      }
{---------------------------------------------------------------------}
constructor TWLizenz32.Create(aFile: string = '';
  bEncode: boolean = C_Lizenz_Encoded;
  bKeepFileOpenforReadWrite: boolean = false);
{---------------------------------------------------------------------}
var
  wFileMode: word;

begin
  try
    FLastError:=0;  // 09.07.2021, WW

    FExeList:= TLizenzList.Create;
    FMembers := TStringList.Create;
    FEncoded := bEncode;
    if (FEncoded) then Randomize;

    if (aFile = '') then
    try
      FLizenzFileName:= GetWieserLizenzFile
    except
      FLizenzFileName := ExtractFilePath(ParamStr(0)) + C_Lizenz_Filename;
    end
    else FLizenzFileName:= aFile; { Lizenzdatei aus Parameter }

    // Zeitstempel der INI-Datei lesen; 28.09.2020, WW
    FdtLizenzFile:=GetFileDateTime;

    // Pr�fung, ob Datei vorhanden ist
    FLizenzFileStream := nil;
    if (FileExists(FLizenzFileName)) then begin
      if bKeepFileOpenforReadWrite then  // 09.07.2021, WW
        wFileMode := fmOpenReadWrite OR fmShareDenyWrite  // Datei zum Lesen und Schreiben �ffnen
      else
        wFileMode := fmOpenRead OR fmShareDenyWrite;  // Datei nur zum Lesen �ffnen
      FLizenzFileStream := TFileStreamExt.Create(FLizenzFileName, wFileMode, FIsOk);
    end
    else FIsOk := False;

    try
      if (FIsOk) and (Assigned(FLizenzFileStream)) then begin
        ReadTeilnehmer;
        ReadExeEntries;
      end;

      // Pr�fung, ob Verschl�sselungs-Modus stimmt ...  // 01.09.2002
      if (FIsOk) and (FExeList.Count > 0) and (FExeList[0] <> '') and
         (not (FExeList[0][1] in ['A'..'Z'])) then   // 31.08.2004
      begin
        // Noch einmal einlesen
        FEncoded := not FEncoded;  // Verschl�sselungs-Modus wechseln

        if (FIsOk) and (Assigned(FLizenzFileStream)) then begin
          // Position in Datei r�cksetzen; 09.07.2021, WW
          FLizenzFileStream.Seek(0, soFromBeginning);

          ReadTeilnehmer;
          ReadExeEntries;
        end;
      end;
    finally
      // Wenn nur gelesen werden soll: Lizenzdatei gleich wieder schliessen und freigeben
      if not bKeepFileOpenforReadWrite then  // 09.07.2021, WW
        if (Assigned(FLizenzFileStream)) then
          FreeAndNil(FLizenzFileStream);
    end;

    if (FIsOk) then C_Lizenz_MyEncoded := FEncoded;

  finally
    inherited Create;
  end;
end;

{----------------------------------------------------}
destructor TWLizenz32.Destroy;
{----------------------------------------------------}
begin
  FLizenzFileStream.Free;  // Lizenzdatei schliessen und freigeben; 09.07.2021, WW

  { Objekte in der ExeList und ExeList selber freigeben }
  FExeList.Free;  
  FMembers.Free;

  inherited Destroy;
end;

{ Gibt string zur Identifizierung des Teilnehmers z. }
{ R�ckgabe: ID-String                                }
{----------------------------------------------------}
function TWLizenz32.GetTeilnehmer(sText: string): string;
{----------------------------------------------------}
var
  pc : PChar;
  i  : Cardinal;
  sUserName: string;

begin
  Result := '';
  i := MAX_COMPUTERNAME_LENGTH+1;
  GetMem(pc, i);
  try
    GetComputerName(pc, i);
    Result := pc;
  finally
    FreeMem(pc, i);
  end;
  if (Trim(Result) <> '') then Result := sText + ' - ' + Trim(Result);

  // Zus�tzlich mit Benutzername f�r Mehrfach-Benutzerbetrieb auf gleicher
  // Maschine; 29.03.2017, WW
  i := 100;
  GetMem(pc, i+1);
  try
    GetUserName(pc, i);
    sUserName := pc;
  finally
    FreeMem(pc, i+1);
  end;
  Result := Result + ' - ' + Trim(sUserName);
end;

{ Liest z.Zt. registrierte Computer-Teilnehmer in Liste ein }
{-----------------------------------------------------------}
procedure TWLizenz32.ReadTeilnehmer;
{-----------------------------------------------------------}
var
  s : string;
  i : integer;
  b : boolean;
  c : char;
begin
  FMembers.Clear;
  if Assigned (FLizenzFileStream) then begin  // 09.07.2021, WW
    s := '';
    b := True;

    while (b) do begin
      i := FLizenzFileStream.Read(c, 1);     // Einlesen des n�chsten Zeichens
      if (i > 0) and (c <> #31) then begin   // Pr�fung, ob Zeichen gelesen wurde
        s := s + c;
      end
      else b := False;
    end;

    if (FLizenzFileStream.Position = FLizenzFileStream.Size)  // Kein <us> => keine Eintr�ge
    then FLizenzFileStream.Seek(0, soFromBeginning)  // Position r�cksetzen
    else FMembers.CommaText := s;          // Sonst Eintr�ge speichern

    if (FEncoded) then
      for i := 0 to FMembers.Count-1 do
        FMembers[i] := DecodeLizenzString(FMembers[i]);
  end;
end;

{ Liest z.Zt. registrierte Anwendungen in Liste ein  }
{----------------------------------------------------}
procedure TWLizenz32.ReadExeEntries;
{----------------------------------------------------}

  procedure ReadLizenzRec(bInsert: boolean);
  var
    pRec    : PLizenzRec;
    s       : string[C_MaxLength_ExeName];

  begin
    New(pRec);
    FLizenzFileStream.ReadBuffer(pRec^, SizeOf(TLizenzRec)); // Einlesen des n�chsten Records
    s := pRec^.ExeName;
    if (FEncoded) then s := DecodeLizenzString(s);
    pRec^.ExeName := s;
    if (bInsert)
    then FExeList.InsertObject(0, pRec^.ExeName, TObject(pRec))
    else FExeList.AddObject(pRec^.ExeName, TObject(pRec));
  end;

var
  i, iPos : longint;

begin
  // 1. Versuch: Von vorne lesen
  FExeList.Clear;
  if Assigned(FLizenzFileStream) then begin  // 09.07.2021, WW
    try
      while (FLizenzFileStream.Position < FLizenzFileStream.Size) do begin
        ReadLizenzRec(False);
      end;
    except
      // 2. Versuch: Von hinten lesen
      FExeList.Clear;
      i := 1;
      iPos := FLizenzFileStream.Seek(-i * SizeOf(TLizenzRec)+0, soFromEnd);
      try
        while (iPos >= 0) do begin
          ReadLizenzRec(True);
          if (iPos = 0) then FMembers.Clear;  // 13.02.2008
          Inc(i);
          if (iPos >= SizeOf(TLizenzRec))
          then iPos := FLizenzFileStream.Seek(-i * SizeOf(TLizenzRec), soFromEnd)
          else iPos := -1;
        end;
      except
      // Rest vergessen
      end;
    end;
  end;
end;

{ Gibt Lizenzfilepath aus Wieser.Ini zur�ck          }
{----------------------------------------------------}
function TWLizenz32.GetWieserLizenzFile: string;
{----------------------------------------------------}
var
  pi       : TProgramIni;
  ps       : TPathserver;
begin
  pi:= TProgramIni.Create;
  ps:= TPathserver.Create(pi.WieserIniFile, [WNetProgDir]);
  try
    ps.Check;
    result:= ps.Pathname[WNetProgDir] + C_Lizenz_Filename;
  finally
    pi.Free;
    ps.Free;
  end;
end;

{ Liest f�r ein Exe-File die Eintr�ge in ein Record  }
{ R�ckgabe: Record mit Lizenzinformationen           }
{----------------------------------------------------}
function TWLizenz32.GetLizenzRecord(anExe: string): TLizenzRec;
{----------------------------------------------------}
begin
  result := FExeList.GetLizenzRecord(anExe);  // 31.01.2023, WW
end;

{ Aktualisiert den Instanzz�hler eines Eintrags in der Exe-Liste }
{ Parameter: Index des Eintrags in Exe-Liste                     }
{            Offset                                              }
{ Ergebnis: True, wenn Instanzz�hler aktualisiert wurde          }
{----------------------------------------------------------------}
function TWLizenz32.AktuLizenzRecord_CurrentCount(iIndex: integer;
  iOffset: integer = 1): boolean;
{----------------------------------------------------------------}
var
  iCurrentCount: integer;

begin
  Result := false;
  if (iIndex > -1) AND (iIndex < FExeList.Count) then begin
    // Instanzz�hler nicht bei fehlender Programmlizenz aktualisieren
    if (PLizenzRec(FExeList.Objects[iIndex])^.LizenzCount = 0) then begin
      FLastError := -2;  // 09.07.2021, WW
      exit;
    end;

    iCurrentCount := PLizenzRec(FExeList.Objects[iIndex])^.CurrentCount + iOffset;
    if (iCurrentCount < 0) AND
       (PLizenzRec(FExeList.Objects[iIndex])^.LizenzCount > 100) then
      iCurrentCount := 0;  // 29.04.2002

    PLizenzRec(FExeList.Objects[iIndex])^.CurrentCount := iCurrentCount;
    Result := true;
    FLastError := 0;  // Aktualisiert, OK; 09.07.2021, WW
  end else
    FLastError := -4;  // 09.07.2021, WW
end;

{ Tr�gt einen Satz in das Lizenzfile ein             }
{ Parameter: Eingaben f�r das LizenzFile             }
{----------------------------------------------------}
procedure TWLizenz32.AddLizenzEntry(aName: string; aLCount, aCCount: SmallInt;
                                    anExeDate, aLizenzDate: TDateTime);
{----------------------------------------------------}
var
  pRec    : PLizenzRec;
begin
  if (FIsOk) and (Assigned(FLizenzFileStream)) then begin
    New(pRec);
    try
      if (FEncoded)
      then pRec^.ExeName := EncodeLizenzString(ANSIUpperCase(aName)) // 01.09.2003
      else pRec^.ExeName:= ANSIUpperCase(aName);
      pRec^.LastExeDate:= anExeDate;
      pRec^.LizenzCount:= aLCount;
      pRec^.LastLizenzDate:= aLizenzDate;
      pRec^.CurrentCount:= aCCount;

      FLizenzFileStream.Seek(0, soFromEnd);  // Position auf Ende des Stream setzen
      FLizenzFileStream.WriteBuffer(pRec^, SizeOf(TLizenzRec));
    finally
      pRec^.ExeName:= ANSIUpperCase(aName);
      FExeList.AddObject(pRec^.ExeName, TObject(pRec));
    end;
  end;
end;

(* Funktion auskommentiert und nicht mehr verwendet; 09.07.2021, WW
   -> Funktionierte mit den 02/2003 eingef�hrten verschl�sselten Lizenzdateien
      ohnehin nicht (aus Datei gelesene Exe-Namen werden nicht mit
      "DecodeLizenzString" decodiert, wurde wohl an dieser Stelle einfach vergessen)
   -> Auch bei unverschl�sselten Lizenzdateien sollte ein Verbleiben des Exe-
      Eintrags keine "Lizenz-Manipulation" erm�glichen, da das Ablaufdatum nicht
      im Klartext lesbar und somit nicht so einfach gezielt in der Datei �nderbar
      ist.
   -> Achtung, Problem bei Mehrfachzugriff auf Lizenzdatei: In der Funktion
      verwendete Exe-Liste und aktueller Inhalt der Lizenzdatei k�nnen
      bei gleichzeitgen Schreibzugriffen durch andere Programme inkonsistent sein !
      Funktion m��te bei Reaktivierung dahingehend noch zus�tzlich abgesichert werden.

{ L�scht einen Eintrag aus dem Lizenzfile             }
{ Parameter: Name des Lizenzeintrags                 }
{ R�ckgabe: Erfolg ja/nein                           }
{----------------------------------------------------}
function TWLizenz32.DeleteLizenzEntry(aName: string): boolean;
{----------------------------------------------------}
var
  p, pRec : PLizenzRec;
  pStream : TFileStreamExt;
  i, j, k : integer;
begin
  // Pr�fung, ob Datei vorhanden ist
  pStream := nil;
  if (FileExists(FLizenzFileName)) then begin
    pStream := TFileStreamExt.Create(FLizenzFileName, fmOpenReadWrite OR fmShareDenyWrite, FIsOk);
  end
  else FIsOk := False;

  New(pRec);
  New(p);
  try
    if (FIsOk) and (Assigned(pStream)) then begin
      k := 0;
      for i := FExeList.Count-1 downto 0 do begin
        pStream.Seek(-(FExeList.Count-i)*SizeOf(TLizenzRec), soFromEnd);
        pStream.ReadBuffer(pRec^, SizeOf(TLizenzRec));
        if (pRec^.ExeName = ANSIUpperCase(aName)) then begin
          Inc(k);
          for j := i+1 to FExeList.Count-k do begin
            pStream.Seek(-(FExeList.Count-j)*SizeOf(TLizenzRec), soFromEnd);
            pStream.ReadBuffer(p^, SizeOf(TLizenzRec));
            pStream.Seek(-2*SizeOf(TLizenzRec), soFromCurrent);
            pStream.WriteBuffer(p^, SizeOf(TLizenzRec));
          end;
          pStream.Size := pStream.Size - SizeOf(TLizenzRec);
        end;
      end;
      Result := True;
    end
    else Result := False;
  finally
    Dispose(p);
    Dispose(pRec);
    if (Assigned(pStream)) then pStream.Free;
  end;
end; *)

{ Tr�gt ein Exe in das Lizenzfile ein                }
{ Parameter: Name, Datum des Exe-Files               }
{ R�ckgabe > 0 => Fehler                             }
{----------------------------------------------------}
function TWLizenz32.WriteToLizenzFile(aFile: string; aDatum: TDateTime): SmallInt;
{----------------------------------------------------}
var
  i : integer;
  s : string;
begin
  if not FIsOk then begin
    result := C_Lizenz_NoFile;
    exit;
  end;
  s := UpperCase(ChangeFileExt(aFile, ''));
  i := FExeList.IndexOf(s);
  if (i >= 0) then begin
    // Pr�fen, ob das Programm mit dem Benutzer bereits eingetragen ist
    if (IsTeilnehmer(s)) then begin
      Result := C_Lizenz_NoError;
      Exit;
    end;

    // Pr�fung letztes Exe-Datum des Programms
    if (PLizenzRec(FExeList.Objects[i])^.LastExeDate < trunc(aDatum)) and
       (trunc(PLizenzRec(FExeList.Objects[i])^.LastExeDate) <> 0)
    then begin
      result:= C_Lizenz_VersionError;
    end
    // Pr�fung Ablaufdatum des Programms
    else if (PLizenzRec(FExeList.Objects[i])^.LastLizenzDate <= trunc(now)) and
       (trunc(PLizenzRec(FExeList.Objects[i])^.LastLizenzDate) <> 0)
    then begin
      result:= C_Lizenz_TimeError;
      // Auskommentiert, nicht mehr verwendet; 09.07.2021, WW
      // DeleteLizenzEntry(ChangeFileExt(aFile, ''));  // 26.07.2005
    end
    // Pr�fung Instanzz�hler des Programms
    else if (PLizenzRec(FExeList.Objects[i])^.LizenzCount <=
        PLizenzRec(FExeList.Objects[i])^.CurrentCount) and
            (PLizenzRec(FExeList.Objects[i])^.LizenzCount < 100)
    then begin
      result:= C_Lizenz_MaxError;
    end
    else begin  // Pr�fungen OK
      // Teilnehmer in Teilnehmerliste eintragen
      if (AktuTeilnehmer(s)) then begin
        // Instanzz�hler im Eintrag der Exe-Liste inkrementieren
        if (not AktuLizenzRecord_CurrentCount(i))  // 09.07.2021, WW
        then result:= C_Lizenz_InsertError
        else result:= C_Lizenz_NoError;
        // Teilnehmer- und Exe-Liste in Lizenzdatei speichern
        if (not SaveLizenzdatei)
        then result:= C_Lizenz_InsertError;  // 09.07.2021, WW
      end
      else result:= C_Lizenz_NoError;
    end
  end
  else result:= C_Lizenz_NameError;
end;

(* Funktion auskommentiert und nicht mehr verwendet; 09.07.2021, WW

{ Tr�gt ein Exe in das Lizenzfile ein                }
{ Parameter: Name des Exe-Files, Offset              }
{ R�ckgabe: FALSE - Fehler                           }
{----------------------------------------------------}
function TWLizenz32.AddIntoLizenzFile(
  aFile: string; iOffset: integer = 1): boolean;
{----------------------------------------------------}
var
  pRec     : TLizenzRec;
  pStream  : TFileStreamExt;
  sFile    : string;
  sRecFile : string;
begin
  Result := False;

  // Pr�fung, ob Datei vorhanden ist
//  pStream := nil;
  if (FileExists(FLizenzFileName)) then begin
    pStream := TFileStreamExt.Create(FLizenzFileName, fmOpenReadWrite OR fmShareDenyWrite, FIsOk);
  end
  else begin
    FIsOk := False;
    FLastError:=-1;  // 09.07.2021, WW
    exit;
  end;

  if (FIsOk) and (Assigned(pStream)) then
    try
      sFile := UpperCase(ChangeFileExt(ExtractFileName(aFile), ''));
      ReadTeilnehmer(pStream);  // Position hinter Teilnehmer-Eintr�ge setzen, es folgen die Exe-Eintr�ge

      pRec := GetLizenzRecord(aFile);
      if (pRec.LizenzCount = 0) then begin
        FLastError:=-2;  // 09.07.2021, WW
        Exit;
      end;

      // Exe-Eintrag suchen
      FLastError:=-3;  // 09.07.2021, WW
      while (pStream.Position < pStream.Size) do begin
        pStream.ReadBuffer(pRec, SizeOf(TLizenzRec)); // Einlesen des n�chsten Records

        sRecFile := pRec.ExeName;  // 01.09.2003
        if (FEncoded) then sRecFile := DecodeLizenzString(sRecFile);

        if (UpperCase(sRecFile) = sFile) then begin
          pStream.Seek(-SizeOf(TLizenzRec), soFromCurrent);  // Position im Stream um Record zur�ck

          pRec.CurrentCount:= pRec.CurrentCount + iOffset;
          if (pRec.CurrentCount < 0) and (pRec.LizenzCount > 100) then
            pRec.CurrentCount := 0;  // 29.04.2002

          pStream.WriteBuffer(pRec, SizeOf(TLizenzRec)); // Record Schreiben
          Result := True;
          FLastError:=0;  // Gefunden, OK; 09.07.2021, WW
          Break;
        end;
      end;
    finally
      pStream.Free;
    end
  else
    FLastError:=-4;  // 09.07.2021, WW
end;  *)

{ L�scht ein Exe aus dem Lizenzfile                  }
{ Parameter: Name, Datum des Exe-Files               }
{ R�ckgabe > 0 => Fehler                             }
{----------------------------------------------------}
function TWLizenz32.DeleteFromLizenzFile(aFile: string): SmallInt;
{----------------------------------------------------}
var
  i     : integer;
  sFile : string;
begin
  if not FIsOk then begin
    Result := C_Lizenz_NoFile;
    Exit;
  end;

  sFile := UpperCase(ChangeFileExt(ExtractFileName(aFile), ''));
  i:= FExeList.IndexOf(sFile);
  if (i > -1) then begin
    if (PLizenzRec(FExeList.Objects[i])^.CurrentCount <= 0) and
       (PLizenzRec(FExeList.Objects[i])^.LizenzCount < 100)
    then begin
      Result := C_Lizenz_AnzahlKleinerNull;
      SaveLizenzDatei(true); { Lizenzdatei unbrauchbar machen } 
    end
    else begin
      // Teilnehmer aus Teilnehmerliste l�schen
      AktuTeilnehmer(sFile, False);
      // Instanzz�hler im Eintrag der Exe-Liste dekrementieren
      if (not AktuLizenzRecord_CurrentCount(i, -1))  // 09.07.2021, WW
      then Result := C_Lizenz_DeleteError
      else Result := C_Lizenz_NoError;
      // Teilnehmer- und Exe-Liste in Lizenzdatei speichern
      if (not SaveLizenzdatei)
      then result:= C_Lizenz_DeleteError;  // 09.07.2021, WW
    end;
  end
  else Result := C_Lizenz_NameError;
end;

{ Gibt eine Fehlermeldung aus                        }
{----------------------------------------------------}
procedure TWLizenz32.ShowLizenzDialog(anError: SmallInt);
{----------------------------------------------------}
var
  s, s1, s2   : string;
  rec         : TLizenzRec;
begin
  s2:= SMsgLizAnmeldFuer + Application.ExeName;
  case anError of
    C_Lizenz_NoFile      : s1:= SMsgLizProgFehlt;
    C_Lizenz_MaxError    : begin
                             rec:= GetLizenzRecord(application.ExeName);
                             s:= IntToStr(rec.LizenzCount);
                             s1:= Format (SMsgLizMaxAnz, [s]);
                           end;
    C_Lizenz_VersionError: begin
                             rec:= GetLizenzRecord(application.ExeName);
                             s:= FormatDateTime(SDateFormat, rec.LastExeDate);
                             s1:= Format (SMsgLizProgVers, [s]);
                           end;
    C_Lizenz_TimeError    : begin
                             rec:= GetLizenzRecord(application.ExeName);
                             s:= FormatDateTime(SDateFormat, rec.LastLizenzDate);
                             s1:= Format (SMsgLizAbgel, [s]);
                           end;
    C_Lizenz_NameError   : s1:= SMsgLizProg;
    C_Lizenz_InsertError : s1:= SMsgLizProgAnmeld + #13#10#13#10 +
                                Format(SMsgErrorCode, [FLastError]);
    C_Lizenz_DeleteError : s1:= SMsgLizProgAbmeld + #13#10#13#10 +
                                Format(SMsgErrorCode, [FLastError]);
    C_Lizenz_AnzahlKleinerNull : s1:= SMsgLizAnzNeg;
  else
    s1:= SMsgLizUnbekannt;
  end;
  MessageBox(0, PChar(s1), PChar(s2), MB_OK + MB_ICONSTOP);
end;

{ Pr�ft. ob Teilnehmer in Datei bereits vorh. ist    }
{ Parameter: Kennstring                              }
{----------------------------------------------------}
function TWLizenz32.IsTeilnehmer(sText: string): boolean;
{----------------------------------------------------}
var
  s : string;
begin
  s := Trim(GetTeilnehmer(sText));
  Result := (FMembers.IndexOf(s) >= 0);
end;

{ Aktualisiert Teilnehmer in Teilnehmerliste         }
{ Parameter: Flag (T=Insert, F=Delete)               }
{ Ergebnis: True, wenn Teilnehmer aktualisiert wurde }
{           oder Teilnehmer leer ist                 }
{----------------------------------------------------}
function TWLizenz32.AktuTeilnehmer(sText: string;
  bInsert: boolean = True): boolean;
{----------------------------------------------------}
var
  s : string;
  i: integer;
begin
  s := Trim(GetTeilnehmer(sText));
  if (s <> '') then begin
    if (bInsert) then begin
      if (FMembers.IndexOf(s) >= 0)
      then Result := False  // Teilnehmer schon vorhanden, kein Speichern n�tig
      else begin
        FMembers.Add(s);  // Teilnehmer eintragen und speichern
        Result := True;
      end;
    end
    else begin  // Delete
      i := FMembers.IndexOf(s);
      if (i < 0)
      then Result := False  // Teilnehmer nicht vorhanden, kein Speichern n�tig
      else begin
        FMembers.Delete(i);  // Teilnehmer austragen und speichern
        Result := True;
      end;
    end;
  end
  else Result := True;
end;

{ Schreibt Teilnehmer und EXE-Liste in Datei und schliesst die Datei         }
{ Parameter: Kill-Flag, um die Lizenzdatei unbrauchbar zu machen (Teilnehmer }
{            und Exe-Liste werden nicht gespeichert)                         }
{----------------------------------------------------}
function TWLizenz32.SaveLizenzdatei (bKillLizenz: boolean = false): boolean;
{----------------------------------------------------}
var
  i         : integer;
  s         : string;
  c         : char;
begin
  // Teilnehmer und Programme in Lizenzdatei schreiben
  if (FIsOk) and (Assigned(FLizenzFileStream)) then begin
    FLizenzFileStream.Size := 0;  // Datei leeren; 09.07.2021, WW

    if (not bKillLizenz) then begin  // 09.07.2021, WW
      // Teilnehmerliste: Ggf. "Kein Eintrag", um Fehler in Alt-Programmen zu vermeiden
      if (FMembers.Count = 0) then FMembers.Add('<no entry>');
      // Teilnehmerliste eintragen
      if (FEncoded) then
        for i := 0 to FMembers.Count-1 do
          FMembers[i] := EncodeLizenzString(FMembers[i]);
      s := FMembers.CommaText;
      if s <> '' then
        FLizenzFileStream.WriteBuffer(s[1], length(s));  // Schneller in einem Block schreiben
                                                         // statt zeichenweise; 26.07.2021, WW
      // Trennzeichen eintragen; 13.02.2008 Wird immer geschrieben
      c := #31;
      FLizenzFileStream.WriteBuffer(c, 1);

      // Exe-Liste eintragen
      for i := 0 to FExeList.Count-1 do begin
        if (FEncoded) then PLizenzRec(FExeList.Objects[i])^.ExeName :=
          EncodeLizenzString(PLizenzRec(FExeList.Objects[i])^.ExeName);
        FLizenzFileStream.WriteBuffer(
          PLizenzRec(FExeList.Objects[i])^, SizeOf(TLizenzRec));
      end;
    end;

    // Lizenzdatei schliessen und freigeben; 09.07.2021, WW
    FreeAndNil(FLizenzFileStream);

    Result:=true;
    FLastError:=0;
  end
  else begin  // 09.07.2021, WW
    Result:=false;
    FLastError:=-11;
  end;
end;


{-------------------------- Stationslizenz ------------------------------------}

{ Pr�ft Anzahl der lizenzierten Stationen im Lizenzfile }
{ Parameter: afn = "Filename" f�r MRG-, DSfG-Stationen etc.), Anzahl der bereits angelegten Stationen }
{ R�ckgabe > 0 => Fehler                             }
{----------------------------------------------------------------------------------------------------}
function TWLizenz32.ReadStationenFromLizenzFile(afn: string; vorhandene_Stationen: integer): SmallInt;
{----------------------------------------------------------------------------------------------------}
var
  i: integer;
begin
  if not FIsOk then begin
    result:= C_Lizenz_NoFile;
    exit;
  end;
  i:= FExeList.IndexOf(ANSIUpperCase(afn));
  if (i > -1) then begin
    if (PLizenzRec(FExeList.Objects[i])^.LizenzCount <= vorhandene_Stationen) and
       (PLizenzRec(FExeList.Objects[i])^.LizenzCount > -1) then
      result:= C_Lizenz_MaxError
    else
      result:= C_Lizenz_NoError;
  end else
    result:= C_Lizenz_NameError;
end;

{ Liefert einen Fehlermeldungstext mit �berschrift zur Stationslizenzierung }
{------------------------------------------------------------------------}
procedure TWLizenz32.GetStationsLizenzText(afn: string; anError: SmallInt;
  var aCaption: string; var aText: string);
{------------------------------------------------------------------------}
var
  s: string;
  rec: TLizenzRec;

begin
  aCaption:=SMsgLizFuer + afn;
  case anError of
    C_Lizenz_NoFile      : aText:= SMsgLizKeineAbrufBerecht;
    C_Lizenz_MaxError    : begin
                             rec:= GetLizenzRecord(afn);
                             s:= IntToStr(rec.LizenzCount);
                             aText:= Format (SMsgLizMaxAbrufBerecht, [s]);
                           end;
    C_Lizenz_NameError   : aText:= SMsgLizAbrufBerechtFehlt;
  else
    aText:= SMsgLizUnbekannt;
  end;
end;

{ Gibt eine Fehlermeldung zur Stationslizenzierung in einer Messagebox aus }
{----------------------------------------------------------------------------}
procedure TWLizenz32.ShowStationsLizenzDialog(afn: string; anError: SmallInt);
{----------------------------------------------------------------------------}
var
  aCaption, aText: string;
begin
  GetStationsLizenzText(afn, anError, aCaption, aText);  // 01.03.2022, WW
  MessageBox(0, PChar(aText), PChar(aCaption), MB_OK + MB_ICONSTOP);
end;

{ Erzeugt eine Liste mit allen Ger�tetypen, f�r die unbegrenzte
{ DSfG-B-Stationslizenzen vorliegen }
{ R�ckgabe: Stringliste }
{ Hinweis:
  -> f�r eine Freischaltung mu� im Lizenzfile-Feld "Anzahl" 1 (oder gr��er) stehen }
{----------------------------------------------------------------}
function TWLizenz32.CreateListDSfG_B_Geraetetypen_frei: TStrings;
{----------------------------------------------------------------}
var
  i: integer;
  sExeName: string;
  sGeraetetyp: string;
  Sl: TStringList;

begin
  Result:=TStringList.Create;
  if not FIsOk then exit;

  Sl:=TStringList.Create;
  try
    Sl.Sorted:=true;  // Liste sortiert
    for i := 0 to FExeList.Count-1 do begin
      sExeName:=PLizenzRec(FExeList.Objects[i])^.ExeName;
      if Pos (FN_DSFG_B, sExeName) = 1 then begin
        { LizenzCount >= 1 -> Funktion freigeschaltet }
        if PLizenzRec(FExeList.Objects[i])^.LizenzCount >= 1 then begin
          sGeraetetyp:=Copy (sExeName, length (FN_DSFG_B) + 1, length (sExeName));
          Sl.Add (sGeraetetyp);
        end;
      end;
    end;
    Result.AddStrings (TStrings (Sl));
  finally
    Sl.Free;
  end;
end;

{ Pr�ft, ob f�r �bergebenen Ger�tetyp unbegrenzte DSfG-B-Stationslizenzen }
{ freigeschaltet sind }
{ �bergabe: Ger�tetypname (wie in DSfGDef.db !)
{ R�ckgabe: true, wenn freigeschaltet }
{ Hinweis:
  -> f�r eine Freischaltung mu� im Lizenzfile-Feld "Anzahl" 1 (oder gr��er) stehen }
{----------------------------------------------------------------}
function TWLizenz32.ReadDSfG_B_Geraetetyp_frei (AGerTypName: string): boolean;
{----------------------------------------------------------------}
var
  i: integer;
begin
  Result:=false;
  if not FIsOk then exit;
  i:= FExeList.IndexOf(FN_DSFG_B + AGerTypName);
  if i > -1 then
    { LizenzCount >= 1 -> Funktion freigeschaltet }
    Result:=PLizenzRec(FExeList.Objects[i])^.LizenzCount >= 1;
end;

{ Pr�ft, ob MRG vom Typ Wieser freigeschaltet sind oder nicht }
{ R�ckgabe: true, wenn freigeschaltet }
{ Hinweis:
  -> f�r eine Freischaltung mu� im Lizenzfile-Feld "Anzahl" 1 (oder gr��er) stehen }
{-----------------------------------------------------}
function TWLizenz32.ReadMrgTypGrp_Wieser_frei: boolean;
{-----------------------------------------------------}
var
  i: integer;
begin
  Result:=false;
  if not FIsOk then exit;
  i:= FExeList.IndexOf(ANSIUpperCase(FN_MRG_WIESER_ALLE_MRG_FREI));
  if i > -1 then
    { LizenzCount >= 1 -> Funktion freigeschaltet }
    Result:=PLizenzRec(FExeList.Objects[i])^.LizenzCount >= 1;
end;

{ Pr�ft, ob MRG vom Typ Wieser 800/910 freigeschaltet sind oder nicht }
{ R�ckgabe: true, wenn freigeschaltet }
{ Hinweis:
  -> f�r eine Freischaltung mu� im Lizenzfile-Feld "Anzahl" 1 (oder gr��er) stehen }
{------------------------------------------------------------}
function TWLizenz32.ReadMrgTypGrp_Wieser800_910_frei: boolean;
{------------------------------------------------------------}
var
  i: integer;
begin
  Result:=false;
  if not FIsOk then exit;
  i:= FExeList.IndexOf(ANSIUpperCase(FN_MRG_WIESER_MRG800_910_FREI));
  if i > -1 then
    { LizenzCount >= 1 -> Funktion freigeschaltet }
    Result:=PLizenzRec(FExeList.Objects[i])^.LizenzCount >= 1;
end;


{--------------------- Lizenz f�r Programm-Funktionen--------------------------}

{ Pr�ft, ob eine einem Programm zugeordnete Funktion im Lizenzfile freigeschaltet ist }
{ Parameter: FktName = Funktion im aufrufenden Programm
                      (m�gliche Lizenz-Funktionen siehe Konstanten im interface-Teil) }
{            optional: Exe-Name (leer = aufrufendes Exe)                       }
{ R�ckgabe: true, wenn Funktion freigeschaltet ist }
{ Hinweis:
  -> f�r eine Programmfunktion-Freischaltung mu� im Lizenzfile-Record das Feld
     "Filename" mit dem Programm und seiner Funktion versehen sein
     (z.B. 'Afdsfg32-Rufentgegennahme') und im Feld "Anzahl" 1 (oder gr��er) stehen }
{-------------------------------------------------------------------------------}
function TWLizenz32.ReadProgrammfunktionFromLizenzFile(FktName: string;
  ExeName: string = ''): boolean;
{-------------------------------------------------------------------------------}
var
  sExe : string;
begin
  { Programmname ohne Extension: }
  if ExeName = '' then
    sExe := ExtractFilename(Application.ExeName)
  else
    sExe := ExtractFilename(ExeName);  // 27.03.2015, WW
  sExe := ChangeFileExt(sExe, '');
  { Funktionsname anh�ngen: }
  sExe := sExe + FktName;

  // Pr�fung: Lizenzcount > 0 und ggf. Laufzeitbgrenzung  // 25.03.2009
  Result := FExeList.GetExeLaufzeit(sExe);
end;

{ Pr�ft, ob eine globale (keinem einzelnen Programm zugeordnete) Funktion im
  Lizenzfile freigeschaltet ist }
{ Parameter: FktName = Funktion
                      (m�gliche Lizenz-Funktionen siehe Konstanten im interface-Teil) }
{ R�ckgabe: true, wenn Funktion freigeschaltet ist }
{-------------------------------------------------------------------------------}
function TWLizenz32.ReadGlobaleFunktionFromLizenzFile(FktName: string): boolean;
{-------------------------------------------------------------------------------}
begin
  Result := FExeList.ReadGlobaleFunktionFromLizenzFile(FktName);  // 31.01.2023, WW
end;

{ Gibt zur�ck, ob ein MRG-Typ lizensiert ist         }
{ Parameter: MRG-Typ (integer)                       }
{ R�ckgabe: Lizensiert (Anzahl: 0=F, -1/>0=T)        }
{----------------------------------------------------}
function TWLizenz32.GetLizenzMrgTyp(iTyp: integer): integer;  // 17.02.2003
{----------------------------------------------------}
begin
  Result := FExeList.GetLizenzMrgTyp(iTyp);  // 31.01.2023, WW
end;

{ Gibt zur�ck, ob eine PC-ID lizensiert ist          }
{ Parameter: PC-ID (ein Kennungsstring)              }
{ R�ckgabe: Lizensiert ja/nein                       }
{----------------------------------------------------}
function TWLizenz32.GetLizenzPC(sPCID: string): boolean;      // 17.02.2003
{----------------------------------------------------}
var
  i : integer;
  s : string;
begin
  if (sPCID = '') then sPCID := MyGetComputerName;
  s := C_Lizenz_PC_ID + sPCID;
  i := FExeList.IndexOf(ANSIUpperCase(s));
  Result := (i >= 0);

  { Pr�fung auf Eintrag in ExeList, der jeden PC freischaltet: 01.09.2003, WW }
  if not Result then begin
    s:=C_Lizenz_PC_ID + C_Lizenz_PC_ID_beliebig;
    i := FExeList.IndexOf(ANSIUpperCase(s));
    Result := (i >= 0);
  end;
end;

{ Gibt zur�ck, ob eine EXE lizensiert ist            }
{ Parameter: EXE-Name                                }
{ R�ckgabe: Lizensiert ja/nein                       }
{----------------------------------------------------}
function TWLizenz32.GetLizenzExe(sExe: string): boolean;      // 11.02.2003
{----------------------------------------------------}
begin
  with GetLizenzRecord(sExe) do
    Result := (LizenzCount > 0);
end;

{ Gibt zur�ck, ob die Laufzeit eines EXEs g�ltig ist }
{ Parameter: EXE-Name                                }
{            Schalter 'Unbegrenzte Laufzeit erlaubt' }
{ R�ckgabe: Laufzeit noch g�ltig ja/nein             }
{----------------------------------------------------}
function TWLizenz32.GetExeLaufzeit(sExe: string;
  bAllow_Unbegrenzt: boolean = true): boolean;    // 11.02.2003
{----------------------------------------------------}
begin
  Result := FExeList.GetExeLaufzeit(sExe, bAllow_Unbegrenzt);  // 31.01.2023, WW
end;

{ Gibt die Seriennummer zur�ck, sofern vorhanden     }
{----------------------------------------------------}
function TWLizenz32.GetSeriennummer: string;  // 05.08.2016, WW
{----------------------------------------------------}
var
  i: integer;
  iPos: integer;

begin
  Result:='';

  for i:=0 to FExeList.Count-1 do begin
    iPos:=Pos ('SERIENNUMMER:', FExeList[i]);
    if (iPos = 1) then begin
      Result:=Trim (Copy (FExeList[i], length ('SERIENNUMMER:')+1, 20));
      Break;
    end;
  end;
end;

{ 30.01.2018, WW }
{ Pr�ft f�r ein Programm eine freigeschaltete Funktions-Anzahl im Lizenzfile   }
{ Parameter: Name der Funktion, f�r die die lizenzierte Anzahl gepr�ft werden  }
{            soll                                                              }
{            Vorhandene Anzahl (Ist)                                           }
{            optional: Exe-Name (leer = aufrufendes Exe)                       }
{ R�ckgabe:  Freigeschaltete Anzahl                                            }
{ Ergebnis:  Lizenzfehler-Text (leer = OK)                                     }
{------------------------------------------------------------------------------}
function TWLizenz32.CheckProgrammFktLizenzCount(sFktName: string;
  iCountVorhanden: integer; var iCountLizenz: integer;
  ExeName: string = ''): string;
{------------------------------------------------------------------------------}
var
  sExe: string;
  iRes: SmallInt;
  s: string;
  i: integer;

begin
  iCountLizenz := 0;  // Vorbelegung R�ckgabe

  { Programmname ohne Extension: }
  if ExeName = '' then
    sExe := ExtractFilename(Application.ExeName)
  else
    sExe := ExtractFilename(ExeName);  // 01.08.2018, WW
  sExe := ChangeFileExt(sExe, '');
  { Name der Funktion anh�ngen: }
  sExe := sExe + sFktName;

  if not FIsOk then
    iRes:= C_Lizenz_NoFile
  else begin
    i:= FExeList.IndexOf(ANSIUpperCase(sExe));
    if (i > -1) then begin
      iCountLizenz := PLizenzRec(FExeList.Objects[i])^.LizenzCount;
      if (PLizenzRec(FExeList.Objects[i])^.LizenzCount < iCountVorhanden) and
         (PLizenzRec(FExeList.Objects[i])^.LizenzCount > -1) then
        iRes:= C_Lizenz_MaxError
      else
        iRes:= C_Lizenz_NoError;
    end else
      iRes:= C_Lizenz_NameError;
  end;

  if (iRes < 0) then begin
    case iRes of
      C_Lizenz_NoFile      : Result:= SMsgLizKeineFreischaltung;
      C_Lizenz_MaxError    : begin
                               s:= IntToStr(iCountLizenz);
                               Result:= Format (SMsgLizMaxAnzahl, [s]);
                             end;
      C_Lizenz_NameError   : Result:= SMsgLizFreischaltungFehlt;
    else
      Result:= SMsgLizUnbekannt;
    end;
  end else
    Result:='';
end;

{ Pr�ft auf plausiblen Inhalt der Lizenzdatei }
{ Ergebnis: True, wenn plausibel              }
{----------------------------------------------------}
function TWLizenz32.GetLizenzFileValid: boolean;  // 02.03.2020, WW
{----------------------------------------------------}
var
  i, j: integer;
  b: boolean;
  sName: string;
  iOrd: integer;

begin
  if FExeList.Count > 0 then begin
    b := true;
    for i:= 0 to FExeList.Count-1 do begin
      try
        // Exename-Feld auf plausible Zeichen pr�fen (Space bis z)
        sName := PLizenzRec(FExeList.Objects[i])^.ExeName;
        for j:=1 to length (sName) do begin
          iOrd := Ord(sName[j]);
          if (iOrd < 32) OR  (iOrd > 122) then begin
            b := false;
            Break;
          end;
        end;
        if not b then
          Break;

        // Datum-Felder auf plausibles Datum pr�fen:
        DateTimeToStr(PLizenzRec(FExeList.Objects[i])^.LastExeDate);
        DateTimeToStr(PLizenzRec(FExeList.Objects[i])^.LastLizenzDate);
      except
        b := false;
        Break;
      end;
    end;  // for i
  end else
    b := false;

  Result := b;
end;


{--------------------- Zeitstempel der Lizenzdatei ----------------------------}

{---------------------------------------------}
function TWLizenz32.GetFileDateTime: TDateTime;
{---------------------------------------------}
{ Liefert Zeitstempel der Lizenzdatei }
begin
  Result:=GetTriggerTime (FLizenzFileName, false);
end;

{-----------------------------------------------}
function TWLizenz32.FileDateTimeChanged: boolean;
{-----------------------------------------------}
{ Liefert true, wenn sich seit dem letzten Funktionsaufruf bzw. Create der
  Zeitstempel der Lizenzdatei ge�ndert hat }
var
  dtBuf: TDateTime;

begin
  dtBuf:=GetFileDateTime;
  if dtBuf <> FdtLizenzFile then begin   // Datei-Datum hat sich ge�ndert
    FdtLizenzFile:=dtBuf;
    Result:=true;
  end else
    Result:=false;
end;

end.

