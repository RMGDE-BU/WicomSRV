{------------------------------------------------------------------------------}
{ 25.02.1999 GD; Unit für das Verwalten der Netzdateien bei Datenbankänderungen}
{ 18.11.2002 GD; Änderung bei Namensfindung                                    }
{ 10.05.2006 WW; GetTriggerTime auf System-Zeit (TDateTime) umgestellt         }
{                                                                              }
{  (C) Karl Wieser GmbH 1999, 2002                                             }
{------------------------------------------------------------------------------}
unit DB_Attn;

interface

uses
  Classes, SysUtils, DateUtils, novell, T_Zeit;

const
  C_TriggerExt     = 'ctr';

  C_RufTriggerFile = 'WRUFTRIG.CTR';
  C_MrgRufReaktTriggerFile  = 'MRR';      { wird erweitert um MrgId: z.B. MRR0148.CTR }
  C_DSfGRufReaktTriggerFile = 'DRR';      { wird erweitert um StationId: z.B. DRR0148.CTR }

procedure WriteNewTime(Datei: string);
function GetTriggerTime(Datei: string; bTriggerExt: boolean = true): TDateTime;
procedure DeleteTriggerFile (Datei: string);

implementation

{ ermittelt TriggerFileName                     }
{ Parameter: Pfad zur Datei                     }
{-----------------------------------------------}
function TriggerFileName (Datei: string): string;
{-----------------------------------------------}
begin
  if (Datei <> '') and (Datei[Length(Datei)] <> '\')  // 18.11.2002
  then Result:= ChangeFileExt(Datei, '.' + C_TriggerExt)
  else Result:= '';
end;

{ Schreibt die aktuelle Zeit als Filezeit                        }
{ Parameter: Pfad zur Datei                                      }
{----------------------------------------------------------------}
procedure WriteNewTime(Datei: string);
{----------------------------------------------------------------}
var
  s  : string;
  dummy: boolean;
begin
  if length(Datei) > 0 then begin
    s:= TriggerFileName (Datei);
    if s <> '' then
      with TFileStreamExt.Create (s, fmCreate, dummy) do Free;
  end;
end;

{ Liest die Filezeit als TDateTime                               }
{ Parameter: Pfad zur Datei                                      }
{            Flag 'bTriggerExt':                                 }
{              true: es wird Filezeit der zur Datei gehörenden   }
{                    Triggerdatei gelesen                        }
{              false: es wird Filezeit der Datei gelesen         }
{ Rückgabe: Filezeit der letzten Änderung                        }
{ -> 10.05.2006 umgestellt von integer (DOS-Zeit) auf TDateTime) }
{    (System-Zeit)                                               }
{----------------------------------------------------------------}
function GetTriggerTime(Datei: string; bTriggerExt: boolean = true): TDateTime;
{----------------------------------------------------------------}
var
  s: string;
  dt: TDateTime;
  year, month, day: word;
  hour, min, sec, msec: word;

begin
  result:= -1; { default }
  if length(Datei) > 0 then begin
    if bTriggerExt then
      s:= TriggerFileName (Datei)
    else
      s:= Datei;  // 21.06.2012, WW
    if s <> '' then begin
      dt:=WFileDateTime_System(s);
      // durch Decodieren und Encodieren Referenzwert bilden für nachfolgende
      // Vergleiche ohne Rundungsfehler:
      DecodeDate (dt, year, month, day);
      DecodeTime (dt, hour, min, sec, msec);
      Result:=EncodeDateTime (year, month, day, hour, min, sec, msec);
    end;
  end;
end;

{ löscht TriggerFile                                             }
{ Parameter: Pfad zur Datei                                      }
{------------------------------------------}
procedure DeleteTriggerFile (Datei: string);
{------------------------------------------}
var
  s: string;
begin
  s:= TriggerFileName (Datei);
  if s <> '' then DeleteFile (s);
end;

end.
