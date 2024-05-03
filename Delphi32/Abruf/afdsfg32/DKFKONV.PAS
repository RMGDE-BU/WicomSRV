{******************************************************************************}
{* Unit: Konvertieren von DSfG-Konfigurationsdaten in Stammdatentabellen      *}
{* 02.02.2000 WW                                                              *}
{******************************************************************************}
unit DKfKonv;

interface

uses
  Forms, SysUtils, DbTables, ErrConst, PathIni, DListen, DDbSta, WSysCon, DDELList,
  DJournal, TbDSfGAr, WChars;


procedure Konvertiere_Konfiguration (StationId: integer; JournalId: integer;
                                     Teilnehmer: string; EAdr_LoginDfue: string;
                                     EAdr_nichtloeschen: string;
                                     InstanzListe: TInstanzDataList;
                                     InstDfuListe: TInstDfuDataList;
                                     KonfigRohdataListe: TKonfigRohdataList);


implementation

{--------------------------------------------------------------------------}
procedure Konvertiere_RohdatenListe (KonfigRohdataListe: TKonfigRohdataList;
                                     Teilnehmer: string; JournalId: integer;
                                     InstanzListe: TInstanzDataList;
                                     ArchiveListe: TArchiveDataList;
                                     AKanaeleListe: TAKanaeleDataList;
                                     LogbuchListe: TLogbuchDataList);
{--------------------------------------------------------------------------}
{ in Rohdatenliste gespeicherte Rohdaten-Strings in Konfigurationslisten konvertieren;
  Übergabe: Rohdatenliste
            DSfG-Teilnehmerliste }
Const
  CMaxArchivgruppenzahl = 26; { a..z }
  CMaxArchivkanalzahl   = 21; { f..z }

  CWerteart = '';

var
  DELList: TDELList;
  Wert: string;
  ArchivGrpNr: integer;
  KanalNr: integer;
  ArchiveData: PArchiveData;
  AKanaeleData: PAKanaeleData;
  LogbuchData: PLogbuchData;
  InstanzId: integer;
  Name: string;
  Kanaltyp: string;
  EAdrQuelle: string;
  Automatik: boolean;
  DELQuelle: string;
  i, k, l: integer;
  FuellstandVon: integer;
  QuellEAdr: char;
  Kanalzahl: integer;
  AktiveKanaele: integer;
  EAdr: char;
  Rohstring: string;
  Stand: integer;
  Adressen: string;
  isFahrwegkanal: boolean;
  S_45: string [2];
  S_5: string [1];
  isAutomatikFalseKanal: boolean;
  DelKanaltypDb: TTable;
  KanaltypDbOpened: boolean;
  Erg: integer;

begin
  DelKanaltypDb:=TTable.Create(nil);
  try
    DelKanaltypDb.DatabaseName:=PathServer.PathName [WStammDir];
    DelKanaltypDb.TableName:=C_Tb_DelKanaltyp;
    if DelKanaltypDb.Exists then begin
      DelKanaltypDb.Open;
      KanaltypDbOpened:=true;
    end else
      KanaltypDbOpened:=false;

    i:=0;
    while i <= KonfigRohdataListe.Count-1 do begin
      DELList:=TDELList.Create;
      try
        { zuerst alle Rohdatenstrings der gleichen EAdr in DELList konvertieren: }
        EAdr:=NUL;
        while i <= KonfigRohdataListe.Count-1 do begin
          Application.ProcessMessages;
          if KonfigRohdataListe[i] <> '' then begin
            EAdr:=KonfigRohdataListe[i][1];
            Rohstring:=TKonfigRohdataListObj (KonfigRohdataListe.Objects[i]).RegInstData;

            { Antwortstring in Datenelementeliste (Zwischenpuffer) konvertieren: }
            Erg:=DELList.LoadFromRohstring (Rohstring, EAdr);
            if Erg <> DSFGKONVERR_OK then begin
              if (Erg >= C_MinCode_DSfGKonvErr_AA) then   { außerplanmäßige Antwort }
                WriteJournalFehler (JournalId, ST_KONVERROR, SKERR_AUSSERPLANMAESSIGEANTW_DE)  // 20.07.2004, WW
              else
                WriteJournalFehler (JournalId, ST_KONVERROR, SKERR_INSTWERTKONV);
              inc (i);
              Continue;
            end;

            if i < KonfigRohdataListe.Count-1 then begin        { wenn nicht letzter Listeneintrag }
              if KonfigRohdataListe[i+1] <> KonfigRohdataListe[i] then begin  { wenn nächster Eintrag mit anderer EAdr }
                inc (i);
                Break;                                    { raus aus Schleife }
              end;
            end;
          end;
          inc (i);
        end;

        if EAdr <> NUL then begin
          { Konfigurationsdaten aus Datenelementeliste lesen und in Konfigurationslisten eintragen: }
          InstanzId:=Ord (EAdr) * (-1); { zur Verknüpfung aller Listeneinträge einer Instanz, die Id wird
                                          erst beim Schreiben in die Instanz-Tabelle ermittelt !
                                          Negativer Nummernbereich, damit keine Überschneidung mit den
                                          echten InstanzIds passieren kann !}

          Stand:=1;           { Vorbelegung für Stand der zugehörigen Instanz }
          { Schleife über alle möglichen Archivgruppen (caa..caz): }
          ArchivGrpNr:=1;
          while ArchivGrpNr <= CMaxArchivgruppenzahl do begin
            Application.ProcessMessages;
            { Kanalzahl ermitteln. Bei Wieser-Geräten bis 21.6.99 liefert das
              Datenelement die max. Kanalzahl, ab dann und bei Fremdgeräten die
              Zahl der aktivierten Kanäle. }
            if DELList.GetWert (EAdr, 'ca' + Chr(96+ArchivGrpNr) + 'b', Wert) then begin
              if Wert <> '' then
                Kanalzahl:=StrToInt (Wert)
              else
                Kanalzahl:=0;
            end else
              Kanalzahl:=0;

            if Kanalzahl > 0 then begin
              New (ArchiveData);
              { Kennung der Archivgruppe -> Archivgruppenname für Stammdaten (ab 10.10.2000): }
              if DELList.GetWert (EAdr, 'ca' + Chr(96+ArchivGrpNr) + 'a', Wert) then
                Name:=Format ('%.2d ',[ArchivGrpNr]) + Wert
              else
                Name:='Archivgruppe ' + Format ('%.2d',[ArchivGrpNr]);
              Automatik:=true;                     { alle Archivgruppen aktiv }
              SetArchiveDataRec (ArchiveData,
                                 InstanzId,
                                 ArchivGrpNr,
                                 Name,
                                 Automatik);
              ArchiveListe.Add (ArchiveData);

              { Schleife über alle möglichen Archivkanäle (ca?f..ca?z) und Kanalzahl
                (Algorithmus funktioniert, wenn Kanalzahl alle vorhandenen oder nur die aktiven angibt) }
              KanalNr:=1;
              AktiveKanaele:=0;
              while (KanalNr <= CMaxArchivkanalzahl) AND (AktiveKanaele < Kanalzahl) do begin
                Application.ProcessMessages;
                { Archivtyp: }
                if DELList.GetWert (EAdr, 'ca' + Chr(96+ArchivGrpNr) + Chr(101+KanalNr) + 'a', Wert) then
                  Name:=Wert
                else
                  Name:='Archivkanal ' + Format ('%.2d',[KanalNr]);
                isFahrwegkanal:=UpperCase (Copy(Name,1,7)) = 'FAHRWEG';  { Fahrwegkanal identifizieren }

                { EADR der Quellinstanz: }
                if DELList.GetWert (EAdr, 'ca' + Chr(96+ArchivGrpNr) + Chr(101+KanalNr) + 'b', Wert) then
                  EAdrQuelle:=Wert
                else
                  EAdrQuelle:='';

                { Kanal in Tabelle eintragen, wenn in der Quell-EADR was drin steht
                  oder wenn es ein Fahrweg-Kanal ist: }
                if (EAdrQuelle <> '') OR isFahrwegkanal then begin
                  { Quell-DEL: }
                  if DELList.GetWert (EAdr, 'ca' + Chr(96+ArchivGrpNr) + Chr(101+KanalNr) + 'c', Wert) then
                    DELQuelle:=Wert
                  else
                    DELQuelle:='';

                  { Kanaltyp aus DelKanaltyp.db ermitteln: }
                  KanalTyp:='';
                  if KanaltypDbOpened then
                    if DelKanaltypDb.FindKey ([DELQuelle]) then
                      KanalTyp:=DelKanaltypDb.FieldByName (C_Tf_DelKanaltyp_Kanaltyp).AsString;

                  { Stand der zugehörigen Instanz: }
                  if Kanaltyp = kt_ST then
                    Stand:=2;

                  { Kanal aktiv/inaktiv:
                    -> MRG 2201: Kanäle aus Vollanalyse-AGs und LGZ-Speicher-AGs speichern
                       sehr große Datenmengen, so daß es sinnvoll ist, diese Kanäle für die
                       Automatik standardmäßig zu deaktivieren (isAutomatikFalseKanal) }
                  isAutomatikFalseKanal:=false;
                  if Copy (DELQuelle,1,2) = 'wn' then begin
                    S_45:=Copy (DELQuelle,4,2);  { 4. und 5. Zeichen DEA }
                    S_5:=Copy (DELQuelle,5,1);   { 5. Zeichen DEA }
                    isAutomatikFalseKanal:=
                      { Statuskanäle in AG 3,4,15,16 des MRG2201 immer inaktiv }
                      (S_45 = 'ya') OR (S_45 = 'yh') OR (S_45 = 'yi') OR
                      { Datenkanäle in AG 3,4,15,16 des MRG2201 immer inaktiv }
                      ((S_5 = 'e') AND (S_45 <> 'ye')) OR
                      (S_5 = 'i');
                  end;

                  if isAutomatikFalseKanal then
                    Automatik:=false
                  else
                    Automatik:=((EAdrQuelle <> '0') AND (DELQuelle <> '')) OR
                               isFahrwegkanal;

                  New (AKanaeleData);
                  SetAKanaeleDataRec (AKanaeleData,
                                      KanalNr,
                                      InstanzId,
                                      ArchivGrpNr,
                                      Name,
                                      Kanaltyp,
                                      CWerteart,
                                      EAdrQuelle,
                                      Automatik,
                                      DELQuelle);
                  AKanaeleListe.Add (AKanaeleData);
                  inc (AktiveKanaele);
                end;  { if EAdrQuelle <> '' }
                inc (KanalNr);
              end;  { while Archivkanal }
            end;
            inc (ArchivGrpNr);
          end; { while ArchivGrp }

          { Schleife über alle möglichen Logbücher (Prüfung über DEL "Füllstand von Ordnungsnummer"): }
          if Teilnehmer <> '' then
            Adressen:=Teilnehmer
          else
            Adressen:=CMaxDSfGTeilnehmer;

          for l:=1 to length (Adressen) do begin       { Logbücher aller Adressen der Teilnehmerliste }
            Application.ProcessMessages;
            QuellEAdr:=Adressen[l];
            if (QuellEAdr >= Low (CLogbuchDEL)) AND (QuellEAdr <= High (CLogbuchDEL)) then begin
              if DELList.GetWert (EAdr, CLogbuchDEL [QuellEAdr]+'a', Wert) then begin
                if Wert <> '' then begin
                  FuellstandVon:=StrToInt (Wert);
                  if FuellstandVon >= 0 then begin
                    New (LogbuchData);
                    Name:='Logbuch für Adresse ' + QuellEAdr;
                    Automatik:=true;                     { alle Logbücher aktiv }
                    SetLogbuchDataRec (LogbuchData,
                                       InstanzId,
                                       -1,    { die LogbuchNr wird erst beim Schreiben in die Logbuch-Tabelle ermittelt ! }
                                       Name,
                                       QuellEAdr,
                                       Automatik);
                    LogbuchListe.Add (LogbuchData);
                  end;
                end;
              end;
            end;
          end;  { for l }

          { Stand aufgrund der Kanalkonfiguration in InstanzListe eintragen: }
          for k:=0 to InstanzListe.Count-1 do begin
            if PInstanzData (InstanzListe.Items[k])^.Busadresse = EAdr then begin
              PInstanzData (InstanzListe.Items[k])^.Stand:=Stand;
              Break;
            end;
          end;
        end;  { if EAdr <> NUL }
      finally
        DELList.Free;
      end;
    end; { while i }
  finally
    DelKanaltypDb.Close;
    DelKanaltypDb.Free;
  end;
end;

{------------------------------------------------------------------------------}
procedure AddKonfiguration_InstDfu (EAdr: char; InstDfuListe: TInstDfuDataList);
{------------------------------------------------------------------------------}
{ Konfigurationsdaten von DFÜ-Instanzen (nicht Login-Instanz), soweit bekannt, in DfuInstListe eintragen;
  Übergabe: Busadresse der Registrierinstanz
  Übergabe/Rückgabe: InstDfuListe }
Const
  { Vorbelegungen }
  Kennung: string = '';
  Rufnummer: string = '';
  Adresse: array [1..4] of string = ('', '', '', '');
  Passwort: array [1..4] of string = ('', '', '', '');
  LogPort: integer = -1;

var
  InstDfuData: PInstDfuData;
  InstanzId: integer;

begin
  InstanzId:=Ord (EAdr) * (-1);                       { zur Verknüpfung aller Listeneinträge einer Instanz, die Id wird
                                                        erst beim Schreiben in die Instanz-Tabelle ermittelt ! }
  Adresse[1]:=EAdr;
  LogPort:=0;                                        { frei wählbarer LogPort }
  New (InstDfuData);
  SetInstDfuDataRec (InstDfuData,
                     InstanzId,
                     Kennung,
                     Rufnummer,
                     Adresse,
                     Passwort,
                     LogPort);
  InstDfuListe.Add (InstDfuData);
end;

{------------------------------------------------------------------------------}
procedure Konvertiere_Konfiguration (StationId: integer; JournalId: integer;
                                     Teilnehmer: string; EAdr_LoginDfue: string;
                                     EAdr_nichtloeschen: string;
                                     InstanzListe: TInstanzDataList;
                                     InstDfuListe: TInstDfuDataList;
                                     KonfigRohdataListe: TKonfigRohdataList);
{------------------------------------------------------------------------------}
{ in Listen enthaltene Gerätekonfigurationen in Stammdaten-Tabellen eintragen
  Übergabe: StationId
            JournalId
            DSfG-Teilnehmerliste
            Busadresse der Login-DFÜ-Instanz
            Busadressen, die nicht aus Stammdatentabellen gelöscht werden dürfen
            InstanzListe
            KonfigRohdataListe }
var
  ArchiveListe: TArchiveDataList;
  AKanaeleListe: TAKanaeleDataList;
  LogbuchListe: TLogbuchDataList;
  i: integer;
  InstanzData: PInstanzData;
  InstanzId_alt: integer;
  InstanzId_neu: integer;
  Busadresse: string;
  DatenLoeschenOk: boolean;
  LoginInstanzname: string;
  Stammdaten: TDSfGStammdaten;

begin
  DatenLoeschenOk:=true;
  ArchiveListe:=TArchiveDataList.Create;
  try
    AKanaeleListe:=TAKanaeleDataList.Create;
    try
      LogbuchListe:=TLogbuchDataList.Create;
      try
        { in KonfigRohdataListe gespeicherte Rohdaten-Strings in Konfigurationslisten konvertieren: }
        Konvertiere_RohdatenListe (KonfigRohdataListe, Teilnehmer, JournalId,
                                   InstanzListe, ArchiveListe, AKanaeleListe, LogbuchListe);

        { DFÜ-Instanzen (nicht Login-DFÜ, die ist schon in der Liste drin):
          Eintrag mit EAdr und LogPort (mehr ist nicht bekannt) }
        for i:=0 to InstanzListe.Count-1 do begin
          InstanzData:=InstanzListe.Items[i];
          if (InstanzData^.Instanztyp = C_D_Instanztyp_Dfu) AND (InstanzData^.Busadresse <> EAdr_LoginDfue) then
            AddKonfiguration_InstDfu (InstanzData^.Busadresse[1], InstDfuListe);
        end;

        Stammdaten:=TDSfGStammdaten.Create (PathServer.PathName [WStammDir]);
        try
          if Stammdaten.InitTabellen then begin
            { Instanzliste in Tabelle schreiben, InstanzIds zurückgeben: }
            if not Stammdaten.UpdateInstanzDataByList (InstanzListe, StationId, EAdr_LoginDfue, EAdr_nichtloeschen,
                                                       LoginInstanzname) then
              DatenLoeschenOk:=false;

            { geänderten Login-Instanznamen in Station.db mitändern: }
            if LoginInstanzname <> '' then
              Stammdaten.UpdateLoginInstanzname (StationId, LoginInstanzname);

            { InstanzIds in den untergeordneten Listen updaten (bisherige Verknüpfung über neg. Ord (Busadresse) !): }
            for i:=0 to InstanzListe.Count-1 do begin
              Busadresse:=PInstanzData (InstanzListe.Items[i])^.Busadresse;
              if Busadresse <> '' then begin
                InstanzId_alt:=Ord (Busadresse[1]) * (-1);
                InstanzId_neu:=PInstanzData (InstanzListe.Items[i])^.InstanzId;

                InstDfuListe.UpdateInstanzId (InstanzId_alt, InstanzId_neu);
                ArchiveListe.UpdateInstanzId (InstanzId_alt, InstanzId_neu);
                AKanaeleListe.UpdateInstanzId (InstanzId_alt, InstanzId_neu);
                LogbuchListe.UpdateInstanzId (InstanzId_alt, InstanzId_neu);
              end;
            end;

            Stammdaten.UpdateInstDfuDataByList (InstDfuListe);                            { InstDfuliste in Tabelle schreiben }
            if not Stammdaten.UpdateArchiveDataByList (ArchiveListe, InstanzListe) then   { Archiveliste in Tabelle schreiben }
              DatenLoeschenOk:=false;
            if not Stammdaten.UpdateAKanaeleDataByList (AKanaeleListe, ArchiveListe) then { AKanaeleliste in Tabelle schreiben }
              DatenLoeschenOk:=false;
            if not Stammdaten.UpdateLogbuchDataByList (LogbuchListe, InstanzListe) then   { Logbuchliste in Tabelle schreiben }
              DatenLoeschenOk:=false;
          end;  { if InitTabellen }
        finally
          Stammdaten.Free;
        end;
      finally
        LogbuchListe.Free;
      end;
    finally
      AKanaeleListe.Free;
    end;
  finally
    ArchiveListe.Free;
  end;

  if not DatenLoeschenOk then
    WriteJournalFehler (JournalId, ST_DATAERROR, DERR_DELETE);
end;

end.


