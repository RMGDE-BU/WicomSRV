{**********************************************************************************************}
{* Unit: Datum/Zeit der letzten LGZ-Daten ermitteln (Stundenwerte, Tagessätze, Prüfungssätze) *}
{* 21.08.2002 WW                                                                              *}
{**********************************************************************************************}
unit MLetztLGZ;

interface

uses
  Classes, SysUtils, WStrUtils, T_Tools, T_Zeit, novell, MFileNam, WSysCon, LGZType,
  MLGZVerw, LGZUtil;

  
function HoleLetztdatum_Mess (AKennung: string; Kanalzahl: word;
                              var LetztDatum: DateRec; var LetztZeit: TimeRec): integer;
function HoleLetztDatum_Tags (AKennung: string;  Kanalzahl: word;
                              var LetztDatum: DateRec): integer;
function HoleLetztDatum_Prfs (AKennung: string; var LetztDatum: DateRec): integer;

implementation

{--------------------------------------------------------------------------------------}
function HoleLetztdatum_Mess (AKennung: string; Kanalzahl: word;
                              var LetztDatum: DateRec; var LetztZeit: TimeRec): integer;
{--------------------------------------------------------------------------------------}
{ liefert Datum/Zeit des letzten LGZ-Meßwertsatzes
  Übergabe: Kennung
  Rückgabe: LetztDatum
            LetztZeit
  Ergebnis: 0, falls LGZ-Meßwertsatz vorhanden
            -1, sonst }
var
  D_mess  : TFileStreamExt;
  lgzmess : lgzsrec;
  Kennung: string [szlen_Kennung];
  LGZNr: word;
  Ok: boolean;
  LGZSRecSize: integer;

begin
  Result := -1;
  FillChar(LetztDatum, SizeOf(DateRec), 0);
  FillChar(LetztZeit, SizeOf(TimeRec), 0);
  Kennung := F_RightPad (AKennung, ' ', 14);
  LGZSRecSize:=F_SizeOfLGZSRec (Kanalzahl);

  if GetLGZ_Nr(Kennung, LGZNr) <> 0 then exit;
  if FileExists (GetLGZDateiName(LGZNr, dd_mess)) then begin
    D_mess:=TFileStreamExt.Create (GetLGZDateiName(LGZNr, dd_mess),
                                   fmOpenRead OR fmShareDenyWrite, Ok);
    try
      if Ok then begin
        if D_mess.Size > 0 then begin
          D_mess.Seek(-LGZSRecSize, soFromEnd);
          D_mess.ReadBuffer(lgzmess, LGZSRecSize);

          LetztDatum := lgzmess.datum;
          LetztZeit := lgzmess.zeit;
          Result :=0;
        end;
      end;
    finally
      D_mess.Free;
    end;
  end;
end;

{---------------------------------------------------------------}
function HoleLetztDatum_Tags (AKennung: string;  Kanalzahl: word;
                              var LetztDatum: DateRec): integer;
{---------------------------------------------------------------}
{ liefert Datum des letzten LGZ-Tagessatz
  Übergabe: Kennung
  Rückgabe: LetztDatum
  Ergebnis: 0, falls LGZ-Tagessatz vorhanden
            -1, sonst }
Var
   D_tag  : TFileStreamExt;
   lgztag : lgztrec;
   Kennung: string[szlen_Kennung];
   LGZNr: word;
   Ok: boolean;
   LGZTRecSize: integer;

begin
  Result := -1;
  FillChar(LetztDatum, SizeOf(DateRec), 0);
  Kennung := F_rightPad (AKennung, ' ', 14);
  LGZTRecSize:=F_SizeOfLGZTRec (Kanalzahl);

  if GetLGZ_Nr(Kennung, LGZNr) <> 0 then exit;
  if FileExists (GetLGZDateiName(LGZNr, dd_tags)) then begin
    D_tag:=TFileStreamExt.Create (GetLGZDateiName(LGZNr, dd_tags),
                                   fmOpenRead OR fmShareDenyWrite, Ok);
    try
      if Ok then begin
        if D_tag.Size > 0 then begin
          D_tag.Seek(-LGZTRecSize, soFromEnd);
          D_tag.ReadBuffer(lgztag, LGZTRecSize);

          LetztDatum := lgztag.datum;
          Result :=0;
        end;
      end;
    finally
      D_tag.Free;
    end;
  end;
end;

{--------------------------------------------------------------------------------}
function HoleLetztDatum_Prfs (AKennung: string; var LetztDatum: DateRec): integer;
{--------------------------------------------------------------------------------}
{ liefert Datum des letzten LGZ-Prüfungssatz
  Übergabe: Kennung
  Rückgabe: LetztDatum
  Ergebnis: 0, falls LGZ-Prüfungssatz vorhanden
            -1, sonst }
Var
   D_Prf  : File of PruefRec;
   lgzprf : PruefRec;
   Kennung: string[szlen_Kennung];
   LGZNr: word;
   ResetErg: longint;

begin
  Result := -1;
  FillChar(LetztDatum, SizeOf(DateRec), 0);
  Kennung := F_rightPad (AKennung, ' ', 14);
  if GetLGZ_Nr(Kennung, LGZNr) <> 0 then exit;
  AssignFile(D_Prf, GetLGZDateiName(LGZNr, dd_pruef));
  ResetErg := NetReset(D_Prf, SizeOf(PruefRec), ForRead + DenyNone);
  if ResetErg >= 0 then begin
    if ResetErg = 0 then begin
      CloseFile(D_Prf);
      exit;
    end;
    Seek(D_Prf,filesize(D_Prf)-1);
    read(D_Prf,lgzprf);
    CloseFile(D_Prf);
    LetztDatum := lgzprf.Von_Datum;
    Result := 0;
  end;
end;

end.
