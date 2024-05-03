{******************************************************************************}
{* Unit: Info-Ausgaben des DSfG-Abrufs (Statuszeile, Zustandstabelle)         *}
{* 17.11.1999 WW                                                              *}
{******************************************************************************}
unit AbrfInfo;

interface

uses
  SysUtils, Forms, PathIni, ZustndDb, WSysCon, DZustand;


procedure DeleteZustaende (ComNr: integer);
procedure ZustandMessage(ComNr: integer; StationId: integer; ZustandCode: integer;
                         ZusatzInfo: string; WriteToZustandTable: boolean);
procedure KennungMessage(Msg: string);
procedure StationMessage(Msg: string);

implementation

uses FMain;

{-----------------------------------------}
procedure DeleteZustaende (ComNr: integer);
{-----------------------------------------}
{ alle Einträge eines Abruf aus Zustandstabelle löschen
  Übergabe: ComNr }
var
  ZustandDb: TZustandDb;
begin
  ZustandDb:=TZustandDb.Create (PathServer.PathName [WStammDir]);
  try
    ZustandDb.DeleteAbruf (ComNr);
  finally
    ZustandDb.Free;
  end;
end;

{---------------------------------------------------------------------------}
procedure WriteZustand (ComNr: integer; StationId: integer; Zustand: string);
{---------------------------------------------------------------------------}
{ Aktuellen Abrufzustand (Aktion) in Zustandstabelle eintragen;
  Übergabe: ComNr
            StationId
            Zustand }
var
  ZustandDb: TZustandDb;
begin
  ZustandDb:=TZustandDb.Create (PathServer.PathName [WStammDir]);
  try
    ZustandDb.Append (ComNr, C_GerArtDSfG, StationId, Zustand);
  finally
    ZustandDb.Free;
  end;
end;

{--------------------------------------------------------------------------------}
procedure ZustandMessage(ComNr: integer; StationId: integer; ZustandCode: integer;
                         ZusatzInfo: string; WriteToZustandTable: boolean);
{--------------------------------------------------------------------------------}
{ Ausgabe des aktuellen Abrufzustands (Aktion) in Statuszeile und Zustandtabelle
  Übergabe: ComNr  (für Zustandstabelle)
            StationId
            ZustandCode
            ZusatzInfo-String
            WriteToZustandTable: wenn true, dann Zustände in Zustandtabelle schreiben }
var
  S: string;
begin
  if (ZustandCode >= Low (ZustandStr)) AND (ZustandCode <= High (ZustandStr)) then begin
    S:=ZustandStr [ZustandCode] + ZusatzInfo;
    FormMainDSfGAbruf.Statusbar.Panels [1].Text:=S;                   { Ausgabe in Statuszeile }
    Application.ProcessMessages;
    { Protokollierung in Tabelle, wenn Zustandsprotokollierung aktiv und echter Abrufzustand: }
    if (ZustandCode > 0) AND WriteToZustandTable then
      WriteZustand (ComNr, StationId, S);
  end;
end;

{-------------------------------------}
procedure KennungMessage (Msg: string);
{-------------------------------------}
{ Ausgabe der Kennung der momentan abgerufenen Station in Statuszeile
  Übergabe: Msg-String }
begin
  FormMainDSfGAbruf.Statusbar.Panels [2].Text:=Msg;
  Application.ProcessMessages;
end;

{-------------------------------------}
procedure StationMessage (Msg: string);
{-------------------------------------}
{ Ausgabe des Namens der momentan abgerufenen Station in Statuszeile
  Übergabe: Msg-String }
begin
  FormMainDSfGAbruf.Statusbar.Panels [3].Text:=Msg;
  Application.ProcessMessages;
end;

end.

