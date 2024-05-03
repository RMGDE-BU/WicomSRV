{******************************************************************************}
{* Unit: Info-Ausgaben des MRG-Abrufs (Statuszeile, Zustandstabelle)          *}
{* 10.12.1998 WW                                                              *}
{******************************************************************************}
unit AbrfInfo;

interface

uses
  SysUtils, Forms,  MDBSta, PathIni, ZustndDb, MZustand, WSysCon;

procedure DeleteZustaende (ComNr: integer);
procedure ZustandMessage(ComNr: integer; MrgId: TMrgId; ZustandCode: integer;
                         ZusatzInfo: string; WriteToZustandTable: boolean);
procedure KennungMessage(Msg: string);
procedure StationMessage(Msg: string);

implementation

uses FMain;

{-----------------------------------------}
procedure DeleteZustaende (ComNr: integer);
{-----------------------------------------}
{ alle Eintr�ge eines Abruf aus Zustandstabelle l�schen
  �bergabe: ComNr }
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

{----------------------------------------------------------------------}
procedure WriteZustand (ComNr: integer; MrgId: TMrgId; Zustand: string);
{----------------------------------------------------------------------}
{ Aktuellen Abrufzustand (Aktion) in Zustandstabelle eintragen;
  �bergabe: ComNr
            MrgId
            Zustand }
var
  ZustandDb: TZustandDb;
begin
  ZustandDb:=TZustandDb.Create (PathServer.PathName [WStammDir]);
  try
    ZustandDb.Append (ComNr, C_GerArtMrg, MrgId, Zustand);
  finally
    ZustandDb.Free;
  end;
end;

{----------------------------------------------------------------------------}
procedure ZustandMessage (ComNr: integer; MrgId: TMrgId; ZustandCode: integer;
                         ZusatzInfo: string; WriteToZustandTable: boolean);
{----------------------------------------------------------------------------}
{ Ausgabe des aktuellen Abrufzustands (Aktion) in Statuszeile und Zustandtabelle
  �bergabe: ComNr  (f�r Zustandstabelle)
            MrgId
            ZustandCode
            ZusatzInfo-String
            WriteToZustandTable: wenn true, dann Zust�nde in Zustandtabelle schreiben }
var
  S: string;
begin
  if (ZustandCode >= Low (ZustandStr)) AND (ZustandCode <= High (ZustandStr)) then begin
    S:=ZustandStr [ZustandCode] + ZusatzInfo;
    FormMainMRGAbruf.Statusbar.Panels [1].Text:=S;                   { Ausgabe in Statuszeile }
    Application.ProcessMessages;
    { Protokollierung in Tabelle, wenn Zustandsprotokollierung aktiv und echter Abrufzustand: }
    if (ZustandCode > 0) AND WriteToZustandTable then
      WriteZustand (ComNr, MrgId, S);
  end;
end;

{-------------------------------------}
procedure KennungMessage (Msg: string);
{-------------------------------------}
{ Ausgabe der Kennung der momentan abgerufenen Station in Statuszeile
  �bergabe: Msg-String }
begin
  FormMainMRGAbruf.Statusbar.Panels [2].Text:=Msg;
  Application.ProcessMessages;
end;

{-------------------------------------}
procedure StationMessage (Msg: string);
{-------------------------------------}
{ Ausgabe des Namens der momentan abgerufenen Station in Statuszeile
  �bergabe: Msg-String }
begin
  FormMainMRGAbruf.Statusbar.Panels [3].Text:=Msg;
  Application.ProcessMessages;
end;

end.

