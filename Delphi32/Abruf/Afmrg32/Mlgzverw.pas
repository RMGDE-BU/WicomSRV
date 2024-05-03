{*******************************************************************************************}
{* Unit: Routinen für Langzeitdaten-Verwaltung                                             *}
{* 10.11.99 WW                                                                             *}
{*******************************************************************************************}
unit MLGZVerw;

interface

uses
  WStrUtils, T_Tools, LGZType, novell, MDbSta, MFileNam, PathIni;

function GetLGZ_Nr (kennung: string; var LGZ_Nr: word): shortint;
procedure SetLGZ_Kennung (Kennung_Alt: string; Kennung_Neu: string);
function Verz_Eintrag (MrgId: TMrgId): integer;


implementation

{---------------------------------------------------------------}
function GetLGZ_Nr (kennung: string; var LGZ_Nr: word): shortint;
{---------------------------------------------------------------}
{ liest LGZ-Nr. aus LGZ-Verzeichnisdatei
  Ergebnis = 0: fehlerfrei
            -1: LGZ-Verzeichnisdatei nicht gefunden
            -2: Kennung nicht gefunden, LGZ_Nr = '' }

var
  VerzDatei    : file of TypsVerzTyp;
  VerzEintrag  : TypsVerzTyp;
  gefunden     : boolean;
  ResetErg     : longint;

begin
  Result:=-1;
  LGZ_Nr:=0;
  AssignFile (VerzDatei, GetLGZDVerzDateiName);
  ResetErg := NetReset(VerzDatei, SizeOf(TypsVerzTyp), ForRead + DenyWrite);
  if ResetErg > 0 then begin
    try
      gefunden:=false;
      Result:=-2;
      kennung:=FilterKennung(kennung);
      while not eof(VerzDatei) and not gefunden do begin
        read(VerzDatei, VerzEintrag);
        if kennung=FilterKennung (VerzEintrag.Kennung) then begin
          LGZ_Nr:=VerzEintrag.Nr;
          gefunden:=true;
          Result:=0;
        end;
      end;
    finally
      CloseFile(VerzDatei);
    end;
  end; { if ResetErg > 0 }
end;

{------------------------------------------------------------------}
procedure SetLGZ_Kennung (Kennung_Alt: string; Kennung_Neu: string);
{------------------------------------------------------------------}
{ ändert die Kennung in der LGZ-Verzeichnisdatei }
var
  VerzDatei    : file of TypsVerzTyp;
  VerzEintrag  : TypsVerzTyp;
  gefunden     : boolean;
  ResetErg     : longint;
  k            : string;
begin
  AssignFile (VerzDatei, GetLGZDVerzDateiName);
  ResetErg := NetReset(VerzDatei, SizeOf(TypsVerzTyp), ForAll + DenyAll);
  if ResetErg > 0 then begin
    try
      gefunden:=false;
      k:=FilterKennung(Kennung_Alt);
      while not eof(VerzDatei) and not gefunden do begin
        read(VerzDatei, VerzEintrag);
        if k=FilterKennung (VerzEintrag.Kennung) then begin
          Seek (VerzDatei, FilePos (VerzDatei)-1);
          VerzEintrag.Kennung:=Kennung_Neu;
          write(VerzDatei, VerzEintrag);              { neue Kennung eintragen }
          gefunden:=true;
        end;
      end;
    finally
      CloseFile(VerzDatei);
    end;
  end; { if ResetErg > 0 }
end;

{---------------------------------------------}
function Verz_Eintrag (MrgId: TMrgId): integer;
{---------------------------------------------}
{ liefert für MrgId die LangzeitId aus LGZDVERZ.DAT (lesen bzw. neu anlegen) }
type
  PMerk = ^TMerk;
  TMerk = array [1..9999] of Boolean;
var
  Typsverz: File of TypsVerzTyp;      { LGZ-Verzeichnisdatei }
  TypsVerzKomp: TypsVerzTyp;
  Gefunden: Boolean;
  merkfeld: PMerk;
  ResetErg: longint;
  StaData: TStaData;
  k:string;
  Stammdaten: TMRGStammdaten;

begin
  Result:=0;

  { Langzeitverzeichnisdatei: Kennung prüfen, evtl. neue anhängen }
  AssignFile (TypsVerz, GetLGZDVerzDateiName);
  { Langzeitverzeichnisdatei }
  ResetErg := NetReset (TypsVerz, SizeOf(TypsVerzTyp), ForAll + DenyAll);
  if ResetErg < 0 then begin
    if ResetErg = ERR_FILE_NOT_FOUND then
      Rewrite(TypsVerz)          { Langzeitdaten-Verzeichnisdatei neu anlegen }
    else
      exit;
  end;

  try
    Stammdaten:=TMRGStammdaten.Create (PathServer.PathName [WStammDir]);
    try
      if Stammdaten.InitTabellen then begin
        if Stammdaten.GetStaData (MrgId, StaData) then begin
          New (MerkFeld);
          try
            FillChar (MerkFeld^, sizeof (MerkFeld^), 0);
            Gefunden := False;
            k:=FilterKennung(StaData.Kennung);
            While not (eof(TypsVerz)) and not(gefunden) do
            begin
              Read (TypsVerz,TypsVerzKomp);
              merkfeld^[typsverzKomp.nr] := true;             { Nummer belegt }
              TypsVerzKomp.Kennung:=FilterKennung(TypsVerzKomp.Kennung);
              if TypsVerzKomp.Kennung = k then
                gefunden := True;
            end;
            if Not Gefunden then begin                { neue Kennung anhängen }
              TypsVerzKomp.Kennung := F_rightPad (StaData.Kennung, ' ', 14);
              TypsVerzKomp.nr:=1;
              while merkfeld^[TypsVerzKomp.nr] do
                inc(TypsVerzKomp.nr);             { erste freie Nummer suchen }
              TypsVerzKomp.Status := 0;
              TypsVerzKomp.Index := StaData.MrgTyp;
              Write (TypsVerz,TypsVerzKomp);
            end;
            Result:=TypsVerzKomp.nr;
          finally
            Dispose (MerkFeld);
          end;
        end;
      end;
    finally
      Stammdaten.Free;
    end;
  finally
    CloseFile (TypsVerz);
  end;
end;

end.
