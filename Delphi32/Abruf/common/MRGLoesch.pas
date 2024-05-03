{***************************************************************************}
{* Unit zum Löschen von Langzeitdaten (Kennungsauswahl über Verz.datei)    *}
{* Version      17.03.94      WW                                           *}
{* Version      05.11.01      GD    Eingebaut in neues Sichtprogramm       *}
{*                                  Umbenannt (bisher LOESCH)              *}
{*              26.10.07      WW    resourcestrings                        *}
{***************************************************************************}
UNIT MRGLoesch;

INTERFACE


uses Forms, novell, MrgLoeschFktn, T_tools, PathIni, SysUtils, Dialogs, Sysconst,
     WStrUtils, WSysCon, WTables, LgzType;


function LGZ_Loeschen(Art: boolean;Option: char;LoeschDatum: string; Kennung: string): boolean;


IMPLEMENTATION

resourcestring
  S_KeineLGZDaten = 'Keine Langzeitdaten vorhanden !';
  S_AlleStationen = 'alle Stationen';
  S_KennungXYZ    = 'Kennung ´%s´';
  S_LoeschenVon   = '"Langzeitdaten löschen von" für %s abgeschlossen.';
  S_LoeschenBis   = '"Langzeitdaten löschen bis" für %s abgeschlossen.';


{-----------------------------------------------------------------------------}
function Tagesende(Kennung: string): integer;
{-----------------------------------------------------------------------------}
var
  Q: TQueryExt;
  PfadStammDB: string;
  StammdatPfad: string;
  Akt: string;
begin
  PfadStammDB:= PathServer.PathName[WStammDir];
  q:= TQueryExt.Create(nil);
  try
   StammdatPfad:= PfadStammDB;
    Q.Close;
    Q.Sql.Clear;
    Q.databasename:= Stammdatpfad;
    Akt:= '0';
    with q do begin
      sql.Add('SELECT '+C_Sta_Tagesende+' FROM '+CDBSta+'');
      sql.Add('WHERE ('+C_Sta_Kennung+' = :Kennung) AND ('+C_Sta_Aktiv+' = 0)');
      Parambyname('Kennung').Asstring:=Kennung;
    end;
    Q.Open;
    result:= Q.Fieldbyname(C_Sta_TagesEnde).asinteger;
    Q.Close;
  finally
    Q.Free;
  end;
end;


function LGZ_Loeschen(Art: boolean;Option: char;LoeschDatum: string; Kennung: string): boolean;
{Je nach Option wird }
{ Option: 'A' - Langzeitdaten l”schen "ab";
          'B' - Langzeitdaten l”schen "bis" }

Var
   Datum          : datzeit_rec;
   weitersuchen   : boolean;
   Loesche        : boolean;
   Einzel         : boolean;
   D_Lgzdverzfile : file of TypsverzTyp;
   lgzd           : TypsverzTyp;
   StationStr     : string;
   Lgzddatei      : string;
   Kenn           : string;
   Lgznum         : string;
   FilterKennung  : string;
   aTagesende     : integer;

begin
  LGZ_Loeschen:= true;
  lgzddatei:= PathServer.PathName[LangzeitDir] + 'Lgzdverz.Dat';

  {-------------------------------------------------------------}

  if not FileExists(lgzddatei) then begin
    MessageDlg(S_KeineLGZDaten, mtInformation, [mbOk], 0);
    exit;
  end;
  Einzel:= not Art;

  assignfile(D_Lgzdverzfile,lgzddatei);
  reset(D_Lgzdverzfile);
  weitersuchen:=true;

  while not eof(D_Lgzdverzfile) AND weitersuchen do begin
    Application.ProcessMessages;
    Read(D_Lgzdverzfile, lgzd);
    Loesche:=true;
    FilterKennung:= F_LeftRightTrunc (lgzd.Kennung,' ');
    if Einzel then begin
      if (comparestr(Kennung,FilterKennung) = 0) then
        weitersuchen:=false
      else
        Loesche:=false;
    end;
    if Loesche then begin
      Kenn:= FilterKennung;
      aTagesende:= Tagesende(Kenn);
      Lgznum:= Format('%.4d',[lgzd.nr]);
      datum.Datum.Year:= strtoint(copy(LoeschDatum,7,4));
      datum.Datum.month:= strtoint(copy(LoeschDatum,4,2));
      datum.Datum.day:= strtoint(copy(LoeschDatum,1,2));
      datum.Uhrzeit.Hour:= aTagesende;
      datum.Uhrzeit.Min:= 0;
      datum.Uhrzeit.Sec:= 0;
      datum.Uhrzeit.Hsec:= 0;

      if Option = 'A' then LoeschenAb(datum,Lgznum,Kenn)
      else if Option = 'B' then LoeschenBis(datum,Lgznum,Kenn);
    end;
  end;
  close(D_Lgzdverzfile);

  if Einzel then
    Stationstr:=Format (S_KennungXYZ, [Kenn]);
  else
    StationStr:=S_AlleStationen;

  if Option = 'A' then
    MessageDlg(Format (S_LoeschenVon, [StationStr]), mtInformation, [mbOk], 0)
  else
    MessageDlg(Format (S_LoeschenBis, [StationStr]), mtInformation, [mbOk], 0);
end;

END.
