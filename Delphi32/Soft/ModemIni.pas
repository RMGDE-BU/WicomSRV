{------------------------------------------------------------------------------}
{ 23.06.1999 GD; Zugriff auf Modem.Ini (Modemeinstellungen für Laks)           }
{ 17.11.1999 WW; Modemeinstellungen für DSfG                                   }
{ 09.03.2004 WW; Modemeinstellungen für MRG                                    }
{ 03.11.2004 WW; Erweiterungen für GSM-Modem                                   }
{------------------------------------------------------------------------------}
unit ModemIni;

interface

uses
  SysUtils, Classes, IniFiles;


Const
  CModemInitDefault = '';    { Default für MRG, DSfG, LAKS: keine Initialisierung vorhanden }
  CModemMaxBaud     = 57600; { Default für max. Baudrate }

type

  { Objekt für MODEM.INI }

  TModemIni = class (TIniFile)
  public
    constructor Create(Path: TFilename);
    procedure GetModemList(cGerTyp: char; sl: TStrings);
    function GetMRGInitString(aModem: string): string;
    procedure SetMRGInitString(aModem, Value: string);
    function GetMRG800PTBInitString(aModem: string): string;
    procedure SetMRG800PTBInitString(aModem, Value: string);
    function GetLaksInitString(aModem: string): string;
    procedure SetLaksInitString(aModem, Value: string);
    function GetDSfGInitString(aModem: string): string;
    procedure SetDSfGInitString(aModem, Value: string);
    function GetMaxBaud(aModem: string): integer;
    procedure SetMaxBaud(aModem: string; Value: integer);
    function GetGSM(aModem: string): boolean;
    function GetDPS(aModem: string): string;
  end;

implementation

const
  CModemIni = 'MODEM.INI';

  { Idents }
  CIdentMRGInit       = 'MRGInit';
  CIdentMRG800PTBInit = 'MRG800PTBInit';
  CIdentDSfGInit      = 'DSfGInit';
  CIdentLaksInit      = 'LAKSInit';
  CIdentMaxBaud       = 'MaxBaud';
  CIdentGSM           = 'GSM';
  CIdentDPS           = 'DPS';  // Datenbits, Parität, Stopbits


{ TModemIni }

{--------------------------------------------}
constructor TModemIni.Create(Path: TFilename);
{--------------------------------------------}
begin
  inherited Create(Path + CModemIni);
end;

{ Gibt eine Liste aller in der Modem.Ini vor-   }
{ handenen Modems [Sections] zurück             }
{ Parameter: Gerätetyp-Zeichen (M = MRG, D = DSfG, L = LAKS, A = alle Gerätetypen }
{            Übergabestringlist (TStrings)      }
{------------------------------------------------------------}
procedure TModemIni.GetModemList(cGerTyp: char; sl: TStrings);
{------------------------------------------------------------}
var
  SectionList: TStringList;
  i: integer;
begin
  sl.Clear;
  SectionList:=TStringList.Create;
  try
    ReadSections(SectionList);
    for i:=0 to SectionList.Count-1 do begin
      case cGerTyp of
        'A': sl.Add (SectionList[i]);
        'M': if length (GetMRGInitString (SectionList[i])) > 0 then   // nur, wenn MRG-Initstring vorhanden
               sl.Add (SectionList[i]);
        'D': if length (GetDSfGInitString (SectionList[i])) > 0 then  // nur, wenn DSfG-Initstring vorhanden
               sl.Add (SectionList[i]);
        'L': if length (GetLAKSInitString (SectionList[i])) > 0 then  // nur, wenn LAKS-Initstring vorhanden
               sl.Add (SectionList[i]);
      else
        sl.Add (SectionList[i]);
      end;
    end;
  finally
    SectionList.Free;
  end;
end;

{ Gibt den MRGInitString eines Modems zurück    }
{ Parameter: Modemname (Section)                }
{ Rückgabe: Initialisierungsstring              }
{----------------------------------------------------------}
function TModemIni.GetMRGInitString(aModem: string): string;
{----------------------------------------------------------}
begin
  if length(aModem) > 0 then
    result:= ReadString(aModem, CIdentMRGInit, CModemInitDefault)
  else
    result:=CModemInitDefault;
end;

{ Setzt den MRGInitString für ein Modem         }
{ Parameter: Modemname (Section), Wert          }
{----------------------------------------------------------}
procedure TModemIni.SetMRGInitString(aModem, Value: string);
{----------------------------------------------------------}
begin
  if length(aModem) > 0 then
    WriteString(aModem, CIdentMRGInit, Value);
end;

{ Gibt den MRG800PTBInitString eines Modems zurück }
{ Parameter: Modemname (Section)                   }
{ Rückgabe: Initialisierungsstring                 }
{----------------------------------------------------------------}
function TModemIni.GetMRG800PTBInitString(aModem: string): string;
{----------------------------------------------------------------}
begin
  if length(aModem) > 0 then
    result:= ReadString(aModem, CIdentMRG800PTBInit, CModemInitDefault)
  else
    result:=CModemInitDefault;
end;

{ Setzt den MRG800PTBInitString für ein Modem   }
{ Parameter: Modemname (Section), Wert          }
{----------------------------------------------------------------}
procedure TModemIni.SetMRG800PTBInitString(aModem, Value: string);
{----------------------------------------------------------------}
begin
  if length(aModem) > 0 then
    WriteString(aModem, CIdentMRG800PTBInit, Value);
end;

{ Gibt den LaksInitString eines Modems zurück   }
{ Parameter: Modemname (Section)                }
{ Rückgabe: Initialisierungsstring              }
{-----------------------------------------------------------}
function TModemIni.GetLaksInitString(aModem: string): string;
{-----------------------------------------------------------}
begin
  if length(aModem) > 0 then
    result:= ReadString(aModem, CIdentLaksInit, CModemInitDefault)
  else
    result:=CModemInitDefault;
end;

{ Setzt den LaksInitString für ein Modem        }
{ Parameter: Modemname (Section), Wert          }
{-----------------------------------------------------------}
procedure TModemIni.SetLaksInitString(aModem, Value: string);
{-----------------------------------------------------------}
begin
  if length(aModem) > 0 then
    WriteString(aModem, CIdentLaksInit, Value);
end;

{ Gibt den DSfGInitString eines Modems zurück   }
{ Parameter: Modemname (Section)                }
{ Rückgabe: Initialisierungsstring              }
{-----------------------------------------------------------}
function TModemIni.GetDSfGInitString(aModem: string): string;
{-----------------------------------------------------------}
begin
  if length(aModem) > 0 then
    result:= ReadString(aModem, CIdentDSfGInit, CModemInitDefault)
  else
    result:=CModemInitDefault;
end;

{ Setzt den DSfG-InitString für ein Modem       }
{ Parameter: Modemname (Section), Wert          }
{-----------------------------------------------------------}
procedure TModemIni.SetDSfGInitString(aModem, Value: string);
{-----------------------------------------------------------}
begin
  if length(aModem) > 0 then
    WriteString(aModem, CIdentDSfGInit, Value);
end;

{ Gibt die max. Baudrate, mit der das Modem angesprochen werden kann zurück }
{ Parameter: Modemname (Section)                                            }
{ Rückgabe: max. Baudrate                                                   }
{-----------------------------------------------------}
function TModemIni.GetMaxBaud(aModem: string): integer;
{-----------------------------------------------------}
begin
  if length(aModem) > 0 then
    result:= ReadInteger(aModem, CIdentMaxBaud, CModemMaxBaud)
  else
    result:=CModemMaxBaud;
end;

{ Setzt die max. Baudrate, mit der das Modem angesprochen werden kann }
{ Parameter: Modemname (Section), Wert                                }
{-------------------------------------------------------------}
procedure TModemIni.SetMaxBaud(aModem: string; Value: integer);
{-------------------------------------------------------------}
begin
  if length(aModem) > 0 then
    WriteInteger(aModem, CIdentMaxBaud, Value);
end;

{ Gibt das Flag "GSM" zurück (Kennzeichen, ob Modem ein GSM-Modem ist) }
{ Parameter: Modemname (Section)                }
{ Rückgabe: true, wenn Modem ein GSM-Modem ist  }
{-------------------------------------------------}
function TModemIni.GetGSM(aModem: string): boolean;
{-------------------------------------------------}
begin
  if length(aModem) > 0 then
    result:= ReadBool(aModem, CIdentGSM, false)
  else
    result:=false;
end;

{ Gibt Einstellung der Schnittstellenparameter Datenbits, Parität, Stopbits zurück }
{ Parameter: Modemname (Section)                }
{ Rückgabe: Schnittstellenparameter  }
{------------------------------------------------}
function TModemIni.GetDPS(aModem: string): string;
{------------------------------------------------}
begin
  if length(aModem) > 0 then
    result:= ReadString(aModem, CIdentDPS, '')
  else
    result:='';
end;

end.
