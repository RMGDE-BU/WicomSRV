{------------------------------------------------------------------------------}
{ 25.01.1999 GD; Dialog-Formular-Unit für die Detail-Journal-Tabelle           }
{                                                                              }
{  -> wird zur nicht Laufzeit erstellt - in automatische Erstellung aufnehmen  }
{                                                                              }
{   - Tabelle: WJournal.db                                                     }
{                                                                              }
{ 19.04.1999  GD  Anzeige um Ruf erweitert                                     }
{ 19.04.1999  GD  Anzeige um Laks erweitert                                    }
{ 04.04.2000  WW  Anzeige um DSfG erweitert                                    }
{ 14.02.2001  GD  Query auf TQueryExt umgestellt ('ShowJournal')               }
{ 21.02.2001 WW; Ausgabetexte für Zeitbereiche aussagekräftiger gemacht        }
{                                                                              }
{  (C) Karl Wieser GmbH 1999, 2000, 2001                                       }
{------------------------------------------------------------------------------}
unit fDetJour;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PathIni, DbTables, JournlDb, ExtCtrls, StdCtrls, My_Utils, WSysCon, ZBDatDb,
  Buttons, ZSyncDb, ErrConst, ErrPrc32, DBCtrls, Db, WTables, TelegrDB;

type
  TFormDetailJournal = class(TForm)
    pnBottom: TPanel;
    pnJournalDaten: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    pnGeraeteArt: TPanel;
    pnStationsname: TPanel;
    pnKennung: TPanel;
    pnAbrufart: TPanel;
    pnComPort: TPanel;
    bbtnClose: TBitBtn;
    Label5: TLabel;
    Bevel2: TBevel;
    qryJournal: TQuery;
    sbtnFirst: TSpeedButton;
    sbtnLast: TSpeedButton;
    sbtnPrior: TSpeedButton;
    sbtnNext: TSpeedButton;
    gbVerbindungsZeiten: TGroupBox;
    lbVerbindungszeiten: TListBox;
    pnClient: TPanel;
    pnFehler: TPanel;
    lKeinFehler: TLabel;
    gbWarnungen: TGroupBox;
    lbWarnungen: TListBox;
    gbFehler: TGroupBox;
    pnAktionen: TPanel;
    gbDatenTypen: TGroupBox;
    lbDatenTypen: TListBox;
    gbZeitSync: TGroupBox;
    lbZeitSync: TListBox;
    lbFehler: TListBox;
    gbTelegramme: TGroupBox;
    lbTelegramme: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbtnFirstClick(Sender: TObject);
    procedure sbtnPriorClick(Sender: TObject);
    procedure sbtnNextClick(Sender: TObject);
    procedure sbtnLastClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private-Deklarationen }
    JournalId        : integer;
    Journal          : TJournalDB;
    Stationsliste    : TStringList;
    procedure SetAnzeige(aQuery: TQuery);
    function SetDatenBereichInfo(DatenTyp: integer): boolean;
    procedure SetZeitSync;
    procedure SetDSfGTelegramme (aQuery: TQuery);
    procedure SetFehlerUndWarnungen;
    procedure ClearAnzeige;
    procedure Dialogelemente_plazieren;
  public
    { Public-Deklarationen }
    procedure SetJournal(aQuery: TQuery);
    procedure ShowJournal(aJournalId: integer);
  end;

var
  FormDetailJournal: TFormDetailJournal;

implementation

{$R *.DFM}

const
  { Positionierung der Fehler- und Warnungsboxes }
  C_GbFehlerTop              = 16;
  C_GbWarnungenTop           = 96;
  C_GbFehlerWarnungenHeight  = 70;


{----------------------------------------------------}
procedure TFormDetailJournal.FormCreate(Sender: TObject);
{----------------------------------------------------}
begin
  { Voreinstellungen }
  Journal:= TJournalDB.Create(Pathserver.Pathname[WStammDir]);
  { Stationsliste anlegen }
  Stationsliste:= TStringList.Create;
end;

{----------------------------------------------------}
procedure TFormDetailJournal.FormActivate(Sender: TObject);
{----------------------------------------------------}
begin
  { Stationsliste füllen }
  GetStationsnameListe(Stationsliste, C_AuswahlAlle);
end;

{----------------------------------------------------}
procedure TFormDetailJournal.FormDestroy(Sender: TObject);
{----------------------------------------------------}
var
  i       : integer;
begin
  { Stationsliste freigeben }
  if Stationsliste.Count > 0 then begin
    try
      for i:= 0 to Stationsliste.Count-1 do
        TIdRec(Stationsliste.Objects[i]).free;
    except
      // tue nix
    end;
  end;
  Stationsliste.Free;
  { Journalobjekt freigeben }
  Journal.Free;
end;

{----------------------------------------------------}
procedure TFormDetailJournal.ClearAnzeige;
{----------------------------------------------------}
begin
  lbDatenTypen.Clear;
  lbTelegramme.Clear;
  lbZeitSync.Clear;
  lbWarnungen.Clear;
  lbFehler.Clear;
end;

{----------------------------------------------------}
procedure TFormDetailJournal.ShowJournal(aJournalId: integer);
{----------------------------------------------------}
var
  q: TQueryExt;  // 14.02.2001
  Save_Cursor: TCursor;
begin
  Save_Cursor:= Screen.Cursor;
  Screen.Cursor:= crHourglass;
  try
    LockWindowUpdate (WindowHandle);
    try
      JournalId:= aJournalId;
      ClearAnzeige;
      q:= TQueryExt.Create(nil);
      if Journal.GetRecord(q, JournalId) then setAnzeige(q);
      q.free;
    finally
      LockWindowUpdate (0);
    end;
  finally
    Screen.Cursor:= Save_Cursor;
  end;
end;

{----------------------------------------------------}
procedure TFormDetailJournal.SetJournal(aQuery: TQuery);
{----------------------------------------------------}
begin
  qryJournal:= aQuery;
end;

{----------------------------------------------------}
procedure TFormDetailJournal.SetAnzeige(aQuery: TQuery);
{----------------------------------------------------}
var
  s              : string;
  StaIndex       : integer;
  BereichInfo_vorhanden: boolean;
  ComNr          : integer;

begin
  { Stationsliste füllen }
  if (StationsListe.Count = 0)
    then GetStationsnameListe(Stationsliste, C_AuswahlAlle);

  with aQuery do begin
  { Index der Station in der Stationsliste }
    StaIndex:= StationsListe.IndexOf(qryJournal.FieldByName(C_WJournal_GeraeteArt).asString
                        + qryJournal.FieldByName(C_WJournal_GeraeteId).asString);

  { Eintragen der Kennung }
    if FieldByName(C_WJournal_Kennung).isNull then begin
      if StaIndex > -1
        then s:= TIdRec(StationsListe.Objects[StaIndex]).Kennung else s:= '';
    end
    else s:= FieldByName(C_WJournal_Kennung).asString;
    pnKennung.Caption:= s;

  { Eintragen des Stationsnamens }
    if StaIndex > -1
      then s:= TIdRec(StationsListe.Objects[StaIndex]).Name else s:= '';
    if s = '' then pnStationsname.Caption:= 'Unbekannte Station'
      else pnStationsname.Caption:= s;

  { Eintragen der Geräteart }
    s:= FieldByName(C_WJournal_GeraeteArt).asString;
    if s = C_GerArtMrg then s:= C_AnzeigeMrg
    else if s = C_GerArtLaks then s:= C_AnzeigeLaks
    else if s = C_GerArtGruppe then s:= C_AnzeigeGruppe
    else if s = C_GerArtDSfG then s:= C_AnzeigeDSfG;
    pnGeraeteArt.Caption:= s;

  { Eintragen der Abrufart }
    s:= FieldByName(C_WJournal_AbrufArt).asString;
    if s = C_AbrArtAuto then s:= C_AnzeigeAuto
    else if s = C_AbrArtManu then s:= C_AnzeigeManu
    else if s = C_AbrArtRuf then s:= C_AnzeigeRuf
    else if (s = C_AbrArtMomStart) or (s = C_AbrArtMomStop) then s:= C_AnzeigeMom
    else if s = C_AbrArtRufReakt then s:= C_AnzeigeRufReakt
    else if s = C_AbrArtRueckRuf then s:= C_AnzeigeRueckRuf
    else if s = C_AbrArtKonfLesen then s:= C_AnzeigeKonfLesen
    else if s = C_AbrArtMomDfueStart then s:= C_AnzeigeMomDfue
    else s:= 'Unbekannt';
    pnAbrufArt.Caption:= s;

  { Eintragen der Schnittstelle }
    ComNr:=FieldByName(C_WJournal_ComNr).asInteger;
    if ComNr = CComTCP_IP then
      pnComPort.Caption:= C_AnzeigeTCP_IP
    else
      pnComPort.Caption:= 'COM' + IntToStr (ComNr);

  { Listbox für Zeiten löschen }
    lbVerbindungszeiten.Clear;
  { Eintragen der Verbindungsaufbauzeit }
    lbVerbindungszeiten.Items.Add('');
    lbVerbindungszeiten.Items.Add('Verbindungsaufbau');
    if trunc(FieldByName(C_WJournal_DZVerbAufbau).asDateTime) = 0
      then lbVerbindungszeiten.Items.Add(#9 + 'nicht erfolgt')
      else lbVerbindungszeiten.Items.Add(FormatDateTime(#9 + 'dd.mm.yyyy  hh:nn:ss',
                               FieldByName(C_WJournal_DZVerbAufbau).asDateTime));
  { Eintragen der 'Verbindung steht'-Zeit }
    lbVerbindungszeiten.Items.Add('Verbindung steht');
    if trunc(FieldByName(C_WJournal_DZVerbSteht).asDateTime) = 0
      then lbVerbindungszeiten.Items.Add(#9 + 'nicht erfolgt')
      else lbVerbindungszeiten.Items.Add(FormatDateTime(#9 + 'dd.mm.yyyy  hh:nn:ss',
                               FieldByName(C_WJournal_DZVerbSteht).asDateTime));
  { Eintragen der Loginzeit }
    lbVerbindungszeiten.Items.Add('Login');
    if trunc(FieldByName(C_WJournal_DZLoggedIn).asDateTime) = 0
      then lbVerbindungszeiten.Items.Add(#9 + 'nicht erfolgt')
      else lbVerbindungszeiten.Items.Add(FormatDateTime(#9 + 'dd.mm.yyyy  hh:nn:ss',
                               FieldByName(C_WJournal_DZLoggedIn).asDateTime));
  { Eintragen der Verbindungsabbauzeit }
    lbVerbindungszeiten.Items.Add('Verbindungsabbau');
    if trunc(FieldByName(C_WJournal_DZVerbEnde).asDateTime) = 0
      then lbVerbindungszeiten.Items.Add(#9 + 'nicht erfolgt')
      else lbVerbindungszeiten.Items.Add(FormatDateTime(#9 + 'dd.mm.yyyy  hh:nn:ss',
                               FieldByName(C_WJournal_DZVerbEnde).asDateTime));
  { Eintragen der Verbindungsdauer }
    if (not FieldByName(C_WJournal_DZVerbEnde).isNull) and
       (not FieldByName(C_WJournal_DZVerbSteht).isNull) then begin
      s:= (GetTimeDiffStr(FieldByName(C_WJournal_DZVerbEnde).asDateTime,
                FieldByName(C_WJournal_DZVerbSteht).asDateTime));
      lbVerbindungszeiten.Items.Add('------------------------------------------------');
      lbVerbindungszeiten.Items.Add('Verbindungsdauer:');
      lbVerbindungszeiten.Items.Add(#9 + s);
    end;

  { Eintragen der Datentypen: Bei DSfG-Abrufen existieren nur für die letzten Abrufe
    Journal-Datenbereichsinfos (Rundpuffer). Bei fehlenden Archivkanal-/Logbuch-
    Datenbereichsinfos wird ein Hinweis angezeigt. }
    gbDatentypen.Visible:=false;
    BereichInfo_vorhanden:=true;
    if (FieldByName(C_WJournal_Datentypen).asInteger and C_IsMesswerte) > 0 then
      SetDatenBereichInfo(C_IsMesswerte);
    if (FieldByName(C_WJournal_Datentypen).asInteger and C_IsMeldungen) > 0 then
      SetDatenBereichInfo(C_IsMeldungen);
    if (FieldByName(C_WJournal_Datentypen).asInteger and C_IsPruefsaetze) > 0 then
      SetDatenBereichInfo(C_IsPruefsaetze);
    if (FieldByName(C_WJournal_Datentypen).asInteger and C_IsParameter) > 0 then
      SetDatenBereichInfo(C_IsParameter);
    if (FieldByName(C_WJournal_Datentypen).asInteger and C_IsEBandaenderungen) > 0 then
      SetDatenBereichInfo(C_IsEBandaenderungen);
    if (FieldByName(C_WJournal_Datentypen).asInteger and C_IsArchive) > 0 then
      if not SetDatenBereichInfo(C_IsArchive) then
        BereichInfo_vorhanden:=false;
    if (FieldByName(C_WJournal_Datentypen).asInteger and C_IsLogbuecher) > 0 then
      if not SetDatenBereichInfo(C_IsLogbuecher) then
        BereichInfo_vorhanden:=false;
    if (FieldByName(C_WJournal_Datentypen).asInteger and C_IsDatenelemente) > 0 then
      SetDatenBereichInfo (C_IsDatenelemente);
    if (FieldByName(C_WJournal_Datentypen).asInteger and C_IsBinaerdatei) > 0 then
      SetDatenBereichInfo(C_IsBinaerdatei);

    if not BereichInfo_vorhanden then begin
      lbDatenTypen.Items.Add('');
      lbDatenTypen.Items.Add(#9 + '-> Detailinformationen nicht verfügbar');
    end;

  { Eintragen der Zeitsynchronisation }
    SetZeitSync;

  { Eintragen der DSfG-Telegramme }
    SetDSfGTelegramme (aQuery);

  { Eintragen der Fehler und Warnungen }
    SetFehlerUndWarnungen;

  { Größe und Koordinaten der Dialogelemente anpassen }
    Dialogelemente_plazieren;

  { Formular-Überschrift }
    if trunc(FieldByName(C_WJournal_DZVerbAufbau).asDateTime) = 0
      then s:= 'Verbindungsaufbau nicht erfolgt'
      else s:= FormatDateTime('dd.mm.yyyy  hh:nn:ss',
                               FieldByName(C_WJournal_DZVerbAufbau).asDateTime);
    Caption:= 'Journal für Station ''' + pnStationsname.Caption + ''' vom ' + s;
  end;
end;

{ Gibt für einen Datentyp die Soll-/Ist-Infos aus    }
{ Parameter: Datentyp                                }
{ Result: true, wenn Soll/Ist-Infos vorhanden sind   }
{----------------------------------------------------}
function TFormDetailJournal.SetDatenBereichInfo(DatenTyp: integer): boolean;
{----------------------------------------------------}
var
  tzb: TZeitbereich;
  MRGDatenZeitBereich: TMJZBDatenDB;
  DSfGDatenZeitBereich: TDJZBDatenDB;
  s: string;
  q: TQuery;
  InstanzId: integer;
  ArchivNr: integer;

begin
  Result:=true;
  gbDatentypen.Visible:=true;
  { Leerzeile einfügen }
  if lbDatenTypen.Items.Count > 0 then
    lbDatenTypen.Items.Add('---------------------------------------------------------------------------------' +
                           '---------------------------------------------------------------------------------');
  case DatenTyp of
    C_IsMesswerte: begin  { Meßwerte }
         lbDatenTypen.Items.Add('Meßwerte');
         { Zeitbereiche der MRG-Meßwerte mitanzeigen }
         if (pnGeraeteArt.Caption = C_AnzeigeMRG) then begin
           MRGDatenZeitBereich:= TMJZBDatenDB.Create(PathServer.Pathname[WStammDir]);
           try
             tzb:=MRGDatenZeitBereich.ReadZbDaten(JournalId);
           finally
             MRGDatenZeitBereich.Free;
           end;

           if not ((trunc(tzb.SollVon) = 0) AND (trunc(tzb.SollBis) = 0) AND
                   (trunc(tzb.IstVon) = 0) AND (trunc(tzb.IstBis) = 0)) then begin
             s:=#9 + 'Soll:' + #9;
             if tzb.SollVon > tzb.SollBis then
               s:= s + 'Keine neuen Daten'    { ... zu erwarten }
             else begin
               if trunc(tzb.SollVon) = 0 then
                 s:= s + 'Keine Angabe' + #9#9#9
               else
                 s:= s + FormatDateTime('dd.mm.yyyy  hh:nn:ss', tzb.SollVon) + #9;
               if trunc(tzb.SollBis) = 0 then
                 s:= s + '-  Keine Angabe'
               else
                 s:= s + FormatDateTime('"-  "dd.mm.yyyy  hh:nn:ss', tzb.SollBis);
             end;
             lbDatenTypen.Items.Add(s);
             s:=#9 + 'Ist:' + #9#9;
             if (trunc(tzb.IstVon) = 0) AND (trunc(tzb.IstBis) = 0) then
               s:= s + 'Keine neuen Daten'    { ... in LGZ-Archiv konvertiert }
             else begin
               if trunc(tzb.IstVon) = 0 then
                 s:= s + '-  Keine Angabe'
               else
                 s:= s + FormatDateTime('dd.mm.yyyy  hh:nn:ss', tzb.IstVon) + #9;
               if trunc(tzb.IstBis) = 0 then
                 s:= s + '-  Keine Angabe'
               else
                 s:= s + FormatDateTime('"-  "dd.mm.yyyy  hh:nn:ss', tzb.IstBis);
             end;
             lbDatenTypen.Items.Add(s);
           end;
         end;
       end;
    C_IsMeldungen: lbDatenTypen.Items.Add('Meldungen');                       { Meldungen }
    C_IsPruefsaetze: lbDatenTypen.Items.Add('Prüfungssätze');               { Prüfungssätze }
    C_IsParameter: lbDatenTypen.Items.Add('Parameter');                       { Parameter }
    C_IsEBandAenderungen: lbDatenTypen.Items.Add('Energiebandänderungen'); { Energiebandänderungen }
    C_IsArchive: begin
         lbDatenTypen.Items.Add('Archive');                          { Archive }
         q:= TQuery.Create(nil);
         try
           DSfGDatenZeitBereich:= TDJZBDatenDB.Create(PathServer.Pathname[WStammDir]);
           try
             DSfGDatenZeitBereich.ReadZbDaten_Archive (q, JournalId);
           finally
             DSfGDatenZeitBereich.Free;
           end;
           InstanzId:=0;
           ArchivNr:=0;
           if q.RecordCount = 0 then
             Result:=false;

           q.First;
           while not q.Eof do begin
             if q.FieldByName(C_DJZBDaten_InstanzId).asInteger <> InstanzId then begin
               lbDatenTypen.Items.Add(#9 + q.FieldByName(C_DTF_Instanz_Instanzname).asString);
               InstanzId:=q.FieldByName(C_DJZBDaten_InstanzId).asInteger;
             end;
             if q.FieldByName(C_DJZBDaten_ArchLogbNr).asInteger <> ArchivNr then begin
               lbDatenTypen.Items.Add(#9#9 + q.FieldByName(C_DTF_Archive_Name).asString);
               ArchivNr:=q.FieldByName(C_DJZBDaten_ArchLogbNr).asInteger;
             end;
             s:=#9#9#9 + q.FieldByName('Kanalname').asString +
                ' (K' + q.FieldByName('KanalNr').asString + '):  ';
             if not q.FieldByName(C_DJZBDaten_Fehler).IsNull then
               s:=s + GetDSfGKonvErrorText (q.FieldByName(C_DJZBDaten_Fehler).asInteger);

             { keine Soll-Daten vorhanden: Kanal wurde nicht abgerufen }
             if q.FieldByName(C_DJZBDaten_DZ_SollBis).IsNull AND
                q.FieldByName(C_DJZBDaten_OrdNr_SollBis).IsNull then
               s:=s + 'Keine Daten verfügbar';
             lbDatenTypen.Items.Add(s);

             { prüfen, ob Abruf über Zeitbereich oder Ordnungsnummer erfolgte: }
             if not q.FieldByName(C_DJZBDaten_DZ_SollBis).IsNull then begin   { über Zeitbereich }
               s:=#9#9#9#9 + 'Soll (Zeit):' + #9;
               if q.FieldByName(C_DJZBDaten_DZ_SollVon).IsNull then
                 s:= s + 'Keine Angabe' + #9#9#9
               else
                 s:= s + FormatDateTime('dd.mm.yyyy  hh:nn:ss',
                                        q.FieldByName(C_DJZBDaten_DZ_SollVon).AsDateTime) + #9;
               if q.FieldByName(C_DJZBDaten_DZ_SollBis).IsNull then
                 s:= s + '-  Keine Angabe'
               else
                 s:= s + FormatDateTime('"-  "dd.mm.yyyy  hh:nn:ss',
                                        q.FieldByName(C_DJZBDaten_DZ_SollBis).AsDateTime);
               lbDatenTypen.Items.Add(s);

               s:=#9#9#9#9 + 'Ist (Zeit):' + #9;
               if q.FieldByName(C_DJZBDaten_DZ_IstVon).IsNull AND
                  q.FieldByName(C_DJZBDaten_DZ_IstBis).IsNull then
                 s:= s + 'Keine neuen Daten'
               else begin
                 if q.FieldByName(C_DJZBDaten_DZ_IstVon).IsNull then
                   s:= s + 'Keine Angabe' + #9#9#9
                 else
                   s:= s + FormatDateTime('dd.mm.yyyy  hh:nn:ss',
                                          q.FieldByName(C_DJZBDaten_DZ_IstVon).AsDateTime) + #9;
                 if q.FieldByName(C_DJZBDaten_DZ_IstBis).IsNull then
                   s:= s + '-  Keine Angabe'
                 else
                   s:= s + FormatDateTime('"-  "dd.mm.yyyy  hh:nn:ss',
                                          q.FieldByName(C_DJZBDaten_DZ_IstBis).AsDateTime);
               end;
               lbDatenTypen.Items.Add(s);
             end
             else if not q.FieldByName(C_DJZBDaten_OrdNr_SollBis).IsNull then begin   { über Ordnungsnummer }
               s:=#9#9#9#9 + 'Soll (Ord.Nr.):' + #9;
               if q.FieldByName(C_DJZBDaten_OrdNr_SollVon).IsNull then
                 s:= s + 'Keine Angabe  '
               else
                 s:= s + q.FieldByName(C_DJZBDaten_OrdNr_SollVon).AsString + '  ';
               if q.FieldByName(C_DJZBDaten_OrdNr_SollBis).IsNull then
                 s:= s + '-  Keine Angabe'
               else
                 s:= s + '-  ' + q.FieldByName(C_DJZBDaten_OrdNr_SollBis).AsString;
               lbDatenTypen.Items.Add(s);
               s:=#9#9#9#9 + 'Ist (Ord.Nr.):' + #9;
               if q.FieldByName(C_DJZBDaten_OrdNr_IstVon).IsNull AND
                  q.FieldByName(C_DJZBDaten_OrdNr_IstBis).IsNull then
                 s:= s + 'Keine neuen Daten'
               else begin
                 if q.FieldByName(C_DJZBDaten_OrdNr_IstVon).IsNull then
                   s:= s + 'Keine Angabe  '
                 else
                   s:= s + q.FieldByName(C_DJZBDaten_OrdNr_IstVon).AsString + '  ';
                 if q.FieldByName(C_DJZBDaten_OrdNr_IstBis).IsNull then
                   s:= s + '-  Keine Angabe'
                 else
                   s:= s + '-  ' + q.FieldByName(C_DJZBDaten_OrdNr_IstBis).AsString;
               end;
               lbDatenTypen.Items.Add(s);
             end;

             q.Next;
           end;
         finally
           q.Free;
         end;
       end;
     C_IsLogbuecher: begin
         lbDatenTypen.Items.Add('Logbücher');                          { Logbücher }
         q:= TQuery.Create(nil);
         try
           DSfGDatenZeitBereich:= TDJZBDatenDB.Create(PathServer.Pathname[WStammDir]);
           try
             DSfGDatenZeitBereich.ReadZbDaten_Logbuecher (q, JournalId);
           finally
             DSfGDatenZeitBereich.Free;
           end;
           InstanzId:=0;
           if q.RecordCount = 0 then
             Result:=false;

           q.First;
           while not q.Eof do begin
             if q.FieldByName(C_DJZBDaten_InstanzId).asInteger <> InstanzId then begin
               lbDatenTypen.Items.Add(#9 + q.FieldByName(C_DTF_Instanz_Instanzname).asString);
               InstanzId:=q.FieldByName(C_DJZBDaten_InstanzId).asInteger;
             end;
             s:=#9#9 + q.FieldByName(C_DTF_Logbuch_Name).asString + ':  ';
             if not q.FieldByName(C_DJZBDaten_Fehler).IsNull then
               s:=s + GetDSfGKonvErrorText (q.FieldByName(C_DJZBDaten_Fehler).asInteger);

             { keine Soll-Datenangabe vorhanden: Kanal wurde nicht abgerufen }
             if q.FieldByName(C_DJZBDaten_DZ_SollBis).IsNull AND
                q.FieldByName(C_DJZBDaten_OrdNr_SollBis).IsNull then
               s:=s + 'Keine Daten verfügbar';
             lbDatenTypen.Items.Add(s);

             { prüfen, ob Abruf über Zeit oder Ordnungsnummer erfolgte: }
             if not q.FieldByName(C_DJZBDaten_DZ_SollBis).IsNull then begin    { über Zeitbereich }
               s:=#9#9#9#9 + 'Soll (Zeit):' + #9;
               if q.FieldByName(C_DJZBDaten_DZ_SollVon).IsNull then
                 s:= s + 'Keine Angabe' + #9#9#9
               else
                 s:= s + FormatDateTime('dd.mm.yyyy  hh:nn:ss',
                                        q.FieldByName(C_DJZBDaten_DZ_SollVon).AsDateTime) + #9;
               if q.FieldByName(C_DJZBDaten_DZ_SollBis).IsNull then
                 s:= s + '-  Keine Angabe'
               else
                 s:= s + FormatDateTime('"-  "dd.mm.yyyy  hh:nn:ss',
                                        q.FieldByName(C_DJZBDaten_DZ_SollBis).AsDateTime);
               lbDatenTypen.Items.Add(s);
               s:=#9#9#9#9 + 'Ist (Zeit):' + #9;
               if q.FieldByName(C_DJZBDaten_DZ_IstVon).IsNull AND
                  q.FieldByName(C_DJZBDaten_DZ_IstBis).IsNull then
                 s:= s + 'Keine neuen Daten'
               else begin
                 if q.FieldByName(C_DJZBDaten_DZ_IstVon).IsNull then
                   s:= s + 'Keine Angabe' + #9#9#9
                 else
                   s:= s + FormatDateTime('dd.mm.yyyy  hh:nn:ss',
                                          q.FieldByName(C_DJZBDaten_DZ_IstVon).AsDateTime) + #9;
                 if q.FieldByName(C_DJZBDaten_DZ_IstBis).IsNull then
                   s:= s + '-  Keine Angabe'
                 else
                   s:= s + FormatDateTime('"-  "dd.mm.yyyy  hh:nn:ss',
                                          q.FieldByName(C_DJZBDaten_DZ_IstBis).AsDateTime);
               end;
               lbDatenTypen.Items.Add(s);
             end
             else if not q.FieldByName(C_DJZBDaten_OrdNr_SollBis).IsNull then begin  { über Ordnungsnummer }
               s:=#9#9#9#9 + 'Soll (Ord.Nr.):' + #9;
               if q.FieldByName(C_DJZBDaten_OrdNr_SollVon).IsNull then
                 s:= s + 'Keine Angabe  '
               else
                 s:= s + q.FieldByName(C_DJZBDaten_OrdNr_SollVon).AsString + '  ';
               if q.FieldByName(C_DJZBDaten_OrdNr_SollBis).IsNull then
                 s:= s + '-  Keine Angabe'
               else
                 s:= s + '-  ' + q.FieldByName(C_DJZBDaten_OrdNr_SollBis).AsString;
               lbDatenTypen.Items.Add(s);
               s:=#9#9#9#9 + 'Ist (Ord.Nr.):' + #9;
               if q.FieldByName(C_DJZBDaten_OrdNr_IstVon).IsNull AND
                  q.FieldByName(C_DJZBDaten_OrdNr_IstBis).IsNull then
                 s:= s + 'Keine neuen Daten'
               else begin
                 if q.FieldByName(C_DJZBDaten_OrdNr_IstVon).IsNull then
                   s:= s + 'Keine Angabe  '
                 else
                   s:= s + q.FieldByName(C_DJZBDaten_OrdNr_IstVon).AsString + '  ';
                 if q.FieldByName(C_DJZBDaten_OrdNr_IstBis).IsNull then
                   s:= s + '-  Keine Angabe'
                 else
                   s:= s + '-  ' + q.FieldByName(C_DJZBDaten_OrdNr_IstBis).AsString;
               end;
               lbDatenTypen.Items.Add(s);
             end;

             q.Next;
           end;
         finally
           q.Free;
         end;
       end;
     C_IsDatenelemente: begin
         lbDatenTypen.Items.Add('Datenelemente');             { Datenelemente }
         q:= TQuery.Create(nil);
         try
           DSfGDatenZeitBereich:= TDJZBDatenDB.Create(PathServer.Pathname[WStammDir]);
           try
             DSfGDatenZeitBereich.ReadZbDaten_Datenelemente (q, JournalId);
           finally
             DSfGDatenZeitBereich.Free;
           end;
           if q.RecordCount = 0 then
             Result:=false;

           q.First;
           while not q.Eof do begin
             s:=#9 + q.FieldByName(C_DTF_Instanz_Instanzname).asString + ':  ';
             if not q.FieldByName(C_DJZBDaten_Fehler).IsNull then
                s:=s + GetDSfGKonvErrorText (q.FieldByName(C_DJZBDaten_Fehler).asInteger);
             lbDatenTypen.Items.Add(s);
             q.Next;
           end;
         finally
           q.Free;
         end;
       end;
    C_IsBinaerdatei: lbDatenTypen.Items.Add('Binärdatei');       { Binärdatei }
  end;
end;

{ Gibt die Info über die Zeitsynchronisation aus     }
{----------------------------------------------------}
procedure TFormDetailJournal.SetZeitSync;
{----------------------------------------------------}
var
  Sync        : TWJZSyncDB;
  tPc, tMrg   : TDateTime;
  sPc, sMrg   : string;
  b           : boolean;
  diff        : integer;
begin
  Sync:= TWJZSyncDB.Create(PathServer.Pathname[WStammDir]);
  b:= Sync.ReadGeraet_PCZeit(JournalId, tMrg, tPc);
  Sync.Free;
  { es sollte nicht synchronisiert werden }
  if not b then begin
  { gbZeitSync hiden und gbDatenTypen entsprechend vergrößern }
    gbZeitSync.Visible:= False;
    gbDatenTypen.Height:= gbZeitSync.Height + gbZeitSync.Top - gbDatenTypen.Top;
  end
  { es sollte synchroniert werden }
  else begin
    gbZeitSync.Visible:= True;
    gbDatenTypen.Height:= gbZeitSync.Top - gbDatenTypen.Top - 7;
  { Ruf wurde vorher aus anderen Grünen abgebrochen }
    if (trunc(tPc) = 0) and (trunc(tMrg) = 0) then begin
      lbZeitSync.Items.Add('');
      lbZeitSync.Items.Add('Es konnte nicht synchronisiert werden');
    end
    else begin
      if trunc(tPc) > 0 then sPc:= FormatDateTime('dd.mm.yyyy  hh:nn:ss', tPc)
        else sPc:= 'Keine Angaben';
      lbZeitSync.Items.Add('PC-Zeit:' + #9 + sPc);
      if trunc(tMrg) > 0 then sMrg:= FormatDateTime('dd.mm.yyyy  hh:nn:ss', tMrg)
        else sMrg:= 'Keine Angaben';
      lbZeitSync.Items.Add('MRG-Zeit:' + #9 + sMRG);
      diff:=GetTimeDiffInSec(tPc, tMrg);
      if tPc < tMrg then
        diff:=diff * (-1);
      lbZeitSync.Items.Add('Differenz: ' + #9 + IntToStr(diff) + ' s');
    end;
  end;
end;

{ Gibt die Info über die DSfG-Telegramme aus                               }
{-----------------------------------------------------------------------}
procedure TFormDetailJournal.SetDSfGTelegramme (aQuery: TQuery);
{-----------------------------------------------------------------------}
var
  DJTelegrDB: TDJTelegrDB;
  q: TQuery;
  DatumZeit: string;
  Zeitzone: string;
  TelegrBez: string;
  Busadresse: string;
  Instanzname: string;

begin
  gbTelegramme.Visible:=false;
  { nur DSfG und nicht bei DSfG-DFÜ-Momentanwertdarstellung/Parametrierung (da gibts
    keine spontan gesendeten Telegramme, weil der Bus nicht transparent geschaltet wird) }
  if (aQuery.FieldByName(C_WJournal_GeraeteArt).asString = C_GerArtDSfG) AND
     (aQuery.FieldByName(C_WJournal_AbrufArt).asString <> C_AbrArtMomDfueStart) then begin
    DJTelegrDB:= TDJTelegrDB.Create(Pathserver.Pathname[WStammDir]);
    try
      q:= TQuery.Create(nil);
      try
        { Telegramme in einem Query holen }
        if DJTelegrDB.GetTelegramme (q, JournalId,
                                     aQuery.FieldByName(C_WJournal_GeraeteId).asInteger) then begin
          { Telegramme in die Telegramm-Box eintragen }
          gbTelegramme.Visible:=true;
          q.First;
          while not q.Eof do begin
            if not q.FieldByName(C_DJTelegr_DatumZeit).isNull then begin
              Zeitzone:=q.FieldByName(C_DJTelegr_Zeitzone).asString;
              if length (Zeitzone) = 0 then
                Zeitzone:=' ';
              DatumZeit:=FormatDateTime('dd.mm.yyyy  hh:nn:ss',
                                        q.FieldByName(C_DJTelegr_DatumZeit).asDateTime) + '  ' +
                         Zeitzone + #9;
            end else
              DatumZeit:='';
            TelegrBez:=q.FieldByName(C_Tf_DNTY_Bezeichnung).asString;
            if length (TelegrBez) = 0 then
              TelegrBez:='unbekannter Nachrichtentyp ' +
                         q.FieldByName(C_DJTelegr_Nachrichtentyp).asString;
            if length (q.FieldByName(C_DTF_Instanz_Instanzname).asString) > 0 then
              Instanzname:=q.FieldByName(C_DTF_Instanz_Instanzname).asString
            else
              Instanzname:='unbekannte Instanz';
            Busadresse:=q.FieldByName(C_DTF_Instanz_Busadresse).asString;
            if length (Busadresse) = 0 then
              Busadresse:='unbekannt';
            Instanzname:=Instanzname + ' (Adresse ' + Busadresse + ')';

            lbTelegramme.Items.Add(DatumZeit + Instanzname);
            lbTelegramme.Items.Add(#9'-> ' + TelegrBez);
            q.Next;
          end;
        end;
      finally
        q.Free;
      end;
    finally
      DJTelegrDB.Free;
    end;
  end;
end;

{ Gibt die Infos über Fehler und Warnungen aus       }
{----------------------------------------------------}
procedure TFormDetailJournal.SetFehlerUndWarnungen;
{----------------------------------------------------}
const
  pnFehlerHeight_OK = 92;
var
  FehlerDB    : TJFehlerDB;
  fz, wz      : integer;
  n1, n2      : integer;
  q           : TQuery;
  ss, se      : shortstring;
  lb          : TListbox;
begin
  lb:=nil;
  FehlerDB:= TJFehlerDB.Create(Pathserver.Pathname[WStammDir]);
  try
    { Prüfen, ob es Fehler oder Warnungen gibt }
    if FehlerDB.IdRecordCount(JournalId) = 0 then begin
      { alles Ok }
      pnFehler.Height:=pnFehlerHeight_OK;                { Fehler-Panel klein }
      lKeinFehler.Visible:= True;
      gbFehler.Visible:= False;
      gbWarnungen.Visible:= False;
    end
    else begin
      pnFehler.Height:=pnFehlerHeight_OK * 2;             { Fehler-Panel groß }
    { Fehler und Warnungen in einem Query holen }
      q:= TQuery.Create(nil);
      try
        fz:=0;
        wz:=0;
        FehlerDB.IdReadFehlerUndWarnungen(q, JournalId);
      { Fehler und Warnungen aus dem Query in die entsprechenden Boxen eintragen }
        q.First;
        while not q.Eof do begin
          n1:= q.FieldByName(C_WJFehler_Gruppe).asInteger;
          n2:= q.FieldByName(C_WJFehler_Fehler).asInteger;

          ss:= GetStatusText(n1);
          se:= GetErrorText(n1, n2);
      { Fehler }
          if q.FieldByName(C_WJFehler_Klasse).asInteger = FK_FEHLER then begin
            lb:= lbFehler;
            inc (fz);
          end
      { Warnung }
          else if q.FieldByName(C_WJFehler_Klasse).asInteger = FK_WARNUNG then begin
            lb:= lbWarnungen;
            inc (wz);
          end
      { sonst weiter }
          else begin
            q.Next;
            Continue;
          end;
      { Einzutragenden Text zusammenstellen und eintragen }
          lb.Items.Add(FormatDateTime('dd.mm.yyyy  hh:nn:ss', q.FieldByName(C_WJFehler_DatumZeit).asDateTime)
                       + '     ' + ss);
          if se <> '' then
            lb.Items.Add(#9'-> ' + se);
          q.Next;
        end;
      finally
        q.Free;
      end;

      lKeinFehler.Visible:= False;
      if (fz = 0) AND (wz > 0) then begin
    { Es gibt nur Warnungen }
        gbFehler.Visible:= False;
        gbWarnungen.Visible:= True;
        gbWarnungen.Top:= C_gbFehlerTop;
        gbWarnungen.Height:= C_gbWarnungenTop - C_gbFehlerTop + C_gbFehlerWarnungenHeight;
      end
      else if (fz > 0) AND (wz = 0) then begin
    { Es gibt nur Fehler }
        gbWarnungen.Visible:= False;
        gbFehler.Visible:= True;
        gbFehler.Top:= C_gbFehlerTop;
        gbFehler.Height:= C_gbWarnungenTop - C_gbFehlerTop + C_gbFehlerWarnungenHeight;
      end
      else begin
      { Es gibt beides }
        gbFehler.Visible:= True;
        gbWarnungen.Visible:= True;
        gbFehler.Top:= C_GbFehlerTop;
        gbFehler.Height:= C_GbFehlerWarnungenHeight;
        gbWarnungen.Top:= C_GbWarnungenTop;
        gbWarnungen.Height:= C_gbFehlerWarnungenHeight;
      end;
    end;
  finally
    FehlerDB.Free;
  end;
end;

{----------------------------------------------------}
procedure TFormDetailJournal.Dialogelemente_plazieren;
{----------------------------------------------------}
{ Reihenfolge der Plazierung: 1. gbZeitSync
                              2. gbTelegramme
                              3. gbDatentypen }
const
  CGroupboxAbstand = 7;
var
  yKoord: integer;

begin
  { Vorbelegung für die untere Kante der als nächstes zu plazierenden Groupbox }
  yKoord:=pnAktionen.Top + pnAktionen.Height - 16;

  { ZeitSync-Groupbox: Platz und Höhe wie designed }
  if gbZeitSync.Visible then begin
    gbZeitSync.Top:=yKoord - gbZeitSync.Height;
    yKoord:=yKoord - gbZeitSync.Height - CGroupboxAbstand;
  end;

  { Telegramme-Groupbox: }
  if gbTelegramme.Visible then begin
    gbTelegramme.Top:=yKoord - gbTelegramme.Height;
    yKoord:=yKoord - gbTelegramme.Height - CGroupboxAbstand;
  end;

  { Datentyp-Groupbox }
  gbDatenTypen.Height:=yKoord - gbDatentypen.Top;
end;

{----------------------------------------------------}
procedure TFormDetailJournal.sbtnFirstClick(Sender: TObject);
{----------------------------------------------------}
begin
  qryJournal.First;
  JournalId:= qryJournal.FieldByName(C_WJournal_JournalId).asInteger;
  ShowJournal(JournalId);
end;

{----------------------------------------------------}
procedure TFormDetailJournal.sbtnPriorClick(Sender: TObject);
{----------------------------------------------------}
begin
  qryJournal.Prior;
  if not qryJournal.Bof then begin
    JournalId:= qryJournal.FieldByName(C_WJournal_JournalId).asInteger;
    ShowJournal(JournalId);
  end;
end;

{----------------------------------------------------}
procedure TFormDetailJournal.sbtnNextClick(Sender: TObject);
{----------------------------------------------------}
begin
  qryJournal.Next;
  if not qryJournal.Eof then begin
    JournalId:= qryJournal.FieldByName(C_WJournal_JournalId).asInteger;
    ShowJournal(JournalId);
  end;
end;

{----------------------------------------------------}
procedure TFormDetailJournal.sbtnLastClick(Sender: TObject);
{----------------------------------------------------}
begin
  qryJournal.Last;
  JournalId:= qryJournal.FieldByName(C_WJournal_JournalId).asInteger;
  ShowJournal(JournalId);
end;

end.
