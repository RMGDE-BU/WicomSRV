{******************************************************************************}
{* Unit: Thread für Import empfangener IEC-Datentelegramme                    *}
{* 12.02.2009  WW                                                             *}
{******************************************************************************}
unit IecImportThr;

interface

uses
  Windows, Messages, Classes, StdCtrls, SysUtils, DateUtils, PathIni, WChars,
  WErrMsg, WStream, WStrUtils, WSysCon, T_BinMask, WSysDat, JournlDb, DALKonv,
  DListen, DDbSta, DSfGKonvManager, WMessageSend, UnixDT, IecConst, IecKonfDb,
  Iec870Util, IecLog, IecImportTelegrList, IecImportDirList, WComm, Novell,
  LogFile;

type

  { IEC-Datenimport-Thread }

  TIecImportThread = class(TThread)
  private
    IecImportTelegrammListe: TIecImportTelegrList;
    InfoObj_Bytes: byte;
    FZwFileLoeschen: boolean;
    FDatenImport: boolean;
    IecLogFile: TIecLogFile;
    WieserIniFilename: string;
    LastJournalReorganizeDate: TDateTime;
    IECKonfigDb: TIECKonfigDb;
    DSfGStammdaten: TDSfGStammdaten;
    FImportTermin: TDateTime;
    FImportDirList: TImportDirList;
    procedure Decode_Import_TelegrammDaten;
    procedure AbrufJournal_Reorganisieren;
    function WriteZwischenFile_DSfGArchiv (DSfGSatzData: TDSfGSatzData;
                                           var Filename: string): boolean;
    function WriteAusgabeFile_DSfGArchiv_Stunde (Pfad: string;
                                                 DSfGSatzData: TDSfGSatzData;
                                                 StationId: integer;
                                                 Busadresse, DEL: string): boolean;
    procedure SetImportTermin (Value: TDateTime);
    procedure SetZwFileLoeschen (Value: boolean);
    procedure SetDatenImport (Value: boolean);
  protected
    procedure Execute; override;
  public
    constructor CreateIt (AIecImportTelegrammListe: TIecImportTelegrList;
                          AInfoObj_Bytes: byte;
                          AIecLogFile: TIecLogFile;
                          AWieserIniFilename: string;
                          AImportDirList: TImportDirList);
    property ImportTermin: TDateTime write SetImportTermin;
    property ZwFileLoeschen: boolean write SetZwFileLoeschen;
    property DatenImport: boolean write SetDatenImport;
  end;

implementation


{ TIecImportThread }

{------------------------------------------------------------------------------------}
constructor TIecImportThread.CreateIt (AIecImportTelegrammListe: TIecImportTelegrList;
                                       AInfoObj_Bytes: byte;
                                       AIecLogFile: TIecLogFile;
                                       AWieserIniFilename: string;
                                       AImportDirList: TImportDirList);
{------------------------------------------------------------------------------------}
{ Konstruktor für IEC-Datenimport-Thread;
  Übergaben: Zeiger auf Liste mit Iec-Telegrammen
             Anzahl Bytes, aus der die Info-Objektadresse zusammengesetzt ist
             Zeiger auf IEC-Logfile (nil: kein Logging)
             vollständiger Dateiname der WIESER.INI
             Import-Ausgabe-VerzeichnisListe }
begin
  inherited Create(true); // Thread createn und gleich wieder anhalten
  FreeOnTerminate:=true;  // Thread soll sich beim Beenden selbst freigeben
  Priority:=tpNormal;     // Thread soll mit normaler Priorität ausgeführt werden

  IecImportTelegrammListe:=AIecImportTelegrammListe;
  InfoObj_Bytes:=AInfoObj_Bytes;
  IecLogFile:=AIecLogFile;
  WieserIniFilename:=AWieserIniFilename;
  FImportDirList:=AImportDirList;
  FImportTermin:=EncodeDate (9999, 1, 1);  // Vorbelegung: ganz weit in der Zukunft

  LastJournalReorganizeDate:=0;   { Zeitpunkt der letzten Journal-Reorganisation initialisieren }
  Suspended:=false;         // Thread jetzt fortsetzen
end;

{---------------------------------}
procedure TIecImportThread.Execute;
{---------------------------------}
{ Thread-Ausführung }
var
  IecImportTelegrData: TIecImportTelegrData;
  S: string;

begin
  try
    // PathServer initialisieren:
    PathServer:=TPathServer.Create(WieserIniFilename,
                                   [WNetProgDir, WNetWorkDir, WWorkDir, WStammDir,
                                    WStammDb, AutoDb, ManuDb],
                                   false, false); // keine eigene Session, Zugriff nicht prüfen
    try
      PathServer.Check;

      IECKonfigDb:=TIECKonfigDb.Create (PathServer.Database [WStammDb]);
      try
        DSfGStammdaten:=TDSfGStammdaten.Create (PathServer.Database [WStammDb], nil, nil);
        try
          if DSfGStammdaten.InitTabellen then begin
            while true do begin
              if (IecImportTelegrammListe.GetCount > 0) AND
                 (Now >= FImportTermin) then begin
                // Datentelegramme auswerten und Daten importieren:
                Decode_Import_TelegrammDaten;

                // Journal reorganisieren:
                AbrufJournal_Reorganisieren;
              end
              else begin
                if Terminated then begin
                  // Thread erst beenden, wenn keine Telegramme mehr vorliegen
                  if IecImportTelegrammListe.GetCount > 0 then
                    FImportTermin:=0  // es soll jetzt sofort importiert werden
                  else
                    Break;
                end else
                  Sleep (1);  { Prozessorauslastung niedrig halten }
              end;
            end;  { while true }
          end
          else begin
            if Assigned (IECLogfile) then begin
              S:='TIecImportThread.Execute: FEHLER DSfGStammdaten.InitTabellen';
              IECLogFile.Write (S, '', '', lt_Error);
            end;
          end;
        finally
          DSfGStammdaten.Free;
        end;
      finally
        IECKonfigDb.Free;
      end;
    finally
      PathServer.Free;
    end;
  except
    on E: Exception do begin
      if Assigned (IECLogfile) then begin
        S:='!!! ' + ExceptionErrorMsgStr (E) + ' !!!';
        IECLogFile.Write (S, '', IecImportTelegrData.Telegramm, lt_Error);
      end;
    end;
  end;
end;

{------------------------------------------------------------}
procedure TIecImportThread.SetImportTermin (Value: TDateTime);
{------------------------------------------------------------}
begin
  FImportTermin:=Value;
end;

{------------------------------------------------------------}
procedure TIecImportThread.SetZwFileLoeschen (Value: boolean);
{------------------------------------------------------------}
{ Schalter 'ZwFileLoeschen' (true: bei der Konvertierung entstehende temporäre
  Zwischendateien werden gelöscht) }
begin
  FZwFileLoeschen:=Value;
end;

{---------------------------------------------------------}
procedure TIecImportThread.SetDatenImport (Value: boolean);
{---------------------------------------------------------}
{ Schalter 'DatenImport' (true: Telegramme werden in DB importiert) }
begin
  FDatenImport:=Value;
end;

{------------------------------------------------------}
procedure TIecImportThread.Decode_Import_TelegrammDaten;
{------------------------------------------------------}
{ alle IEC-Datentelegramme decodieren, Daten importieren }
var
  S: string;
  i: integer;
  Len: integer;
  LenDatenfeld: integer;
  Len_Soll: integer;
  iPos: integer;
  sObjekt: string;
  iInfoObjAdr: integer;
  sDaten: string;
  iWert: integer;
  msec, min, hour, day, month, year: word;
  IV_Zeit, SU: boolean;
  IV, CA, CY: boolean;
  SeqNr: byte;
  NT, SB, BL, OV: boolean;
  b: byte;
  siWert: single;
  sLogKopf: string;
  sLogText: string;
  B1, B2, B3: byte;
  bTest: boolean;
  bNeg: boolean;
  iUrsache: byte;
  iErg: integer;
  sGeraeteArt, sMRG_Kennung: string;
  iDSfG_StationId, iDSfG_InstanzId, iDSfG_ArchivNr, iKanalNr: integer;
  sKanaltyp: string;
  KonfigIEC: TKonfigIEC;
  Filename: string;
  DSfGSatzData: TDSfGSatzData;
  DSfGDataListObj: TDSfGDataListObj;
  ArLbDatenListe: TDSfGDataList;
  DEL: string;
  InstanzData: TInstanzData;
  DSfGStationDataList: TDSfGStationDataList;
  KonvManagerDSfG: TKonvManagerDSfG;
  sUnixDatumZeit: string;
  IecImportTelegrData: TIecImportTelegrData;
  AusgabeDirName: string;
  lt: TLogType;

begin
  DSfGStationDataList:=TDSfGStationDataList.Create;
  try
    { alle vorhandenen IEC-Telegramme abarbeiten: }
    while IecImportTelegrammListe.GetTelegramm (IecImportTelegrData) do begin
      { Feld 'Übertragungsursache' decodieren (nur für Log): }
      bTest:=(IecImportTelegrData.DatenEinheitIdent.UebertrUrsache AND $80) <> 0;
      bNeg:=(IecImportTelegrData.DatenEinheitIdent.UebertrUrsache AND $40) <> 0;
      iUrsache:=IecImportTelegrData.DatenEinheitIdent.UebertrUrsache AND $3F;

      sLogKopf:='Typkennung: ' + IntToStr (IecImportTelegrData.DatenEinheitIdent.Typkennung) + '  ' +
                'SQ: ' + IntToStr (integer (IecImportTelegrData.DatenEinheitIdent.SQ_Bit)) + '  ' +
                'Objekte: ' + IntToStr (IecImportTelegrData.DatenEinheitIdent.AnzahlObjekte) + '  ' +
                'Ursache: ' + IntToStr (iUrsache) +
                ' (Test: ' + IntToStr (integer (bTest)) +
                ' Neg.: ' + IntToStr (integer (bNeg)) + ')  ' +
                'ASDU: ' + IntToStr (IecImportTelegrData.DatenEinheitIdent.Stationsnummer);

      { Typkennungs-abhängige Länge des Datenfelds ermitteln: }
      LenDatenfeld:=Get_IEC870_LaengeDatenfeld (IecImportTelegrData.DatenEinheitIdent.Typkennung);
      if LenDatenfeld = 0 then begin
        if Assigned (IECLogfile) then
          IECLogFile.Write (sLogKopf, 'FEHLER Get_IEC870_LaengeDatenfeld' + CR + LF,
                            IecImportTelegrData.Telegramm, lt_Error);
        exit;
      end;

      { Längenprüfung für Datenfelder: }
      if IecImportTelegrData.DatenEinheitIdent.SQ_Bit then begin  // nur erstes Datenelement mit Info-Objektadresse
        if IecImportTelegrData.DatenEinheitIdent.AnzahlObjekte > 0 then
          Len_Soll:=InfoObj_Bytes + (LenDatenfeld * (IecImportTelegrData.DatenEinheitIdent.AnzahlObjekte))
        else
          Len_Soll:=0;
      end else  // alle Datenelemente mit Info-Objektadresse
        Len_Soll:=(LenDatenfeld + InfoObj_Bytes) * IecImportTelegrData.DatenEinheitIdent.AnzahlObjekte;
      if length (IecImportTelegrData.DatenObjekte) <> Len_Soll then begin
        if Assigned (IECLogfile) then
          IECLogFile.Write (sLogKopf, 'FEHLER Soll/Ist-Länge Datenfelder' + CR + LF,
                            IecImportTelegrData.Telegramm, lt_Error);
        exit;
      end;

      // alle Datenfelder abarbeiten:
      sLogText:='';
      iInfoObjAdr:=-1;  // Vorbelegung: unbelegt
      iPos:=1;
      for i:=1 to IecImportTelegrData.DatenEinheitIdent.AnzahlObjekte do begin
        // Datenfeldlänge bestimmen:
        if IecImportTelegrData.DatenEinheitIdent.SQ_Bit AND (i > 1) then
          Len:=LenDatenfeld  // ohne Info-Objektadresse
        else
          Len:=LenDatenfeld + InfoObj_Bytes;  // mit Info-Objektadresse
        sObjekt:=Copy (IecImportTelegrData.DatenObjekte, iPos, Len);
        inc (iPos, Len);  // Positionierung für nächstes Datenfeld

        // Info-Objektadresse:
        if IecImportTelegrData.DatenEinheitIdent.SQ_Bit AND (i > 1) then begin
          if iInfoObjAdr > -1 then begin
            inc (iInfoObjAdr);
            sDaten:=sObjekt;
          end
          else begin
            if Assigned (IECLogfile) then
              IECLogFile.Write (sLogKopf, 'FEHLER Info-Objektadresse nicht belegt' +
                                CR + LF, IecImportTelegrData.Telegramm, lt_Error);
            exit;
          end;
        end
        else begin
          S:=Copy (sObjekt, 1, InfoObj_Bytes);
          iInfoObjAdr:=byte (S[1]);
          if length (S) > 1 then
            iInfoObjAdr:=iInfoObjAdr + (byte (S[2]) * 256);
          if length (S) > 2 then
            iInfoObjAdr:=iInfoObjAdr + (byte (S[3]) * 256 * 256);
          sDaten:=Copy (sObjekt, InfoObj_Bytes + 1, length (sObjekt));
        end;

        { Record zusammenstellen für Suche nach Zuordnung IEC -> WICO22 in
          IEC-Konfigurations-DB: }
        with KonfigIEC do begin
          KonfigIEC.LinienNr:=IecImportTelegrData.LinienNr;
          KonfigIEC.StationsNr:=IecImportTelegrData.DatenEinheitIdent.Stationsnummer;

          { Infoobjekt-Adresse in Byte-Darstellung wandeln: }
          IEC870_InfoObj_IntToBytes (iInfoObjAdr, B1, B2, B3);
          if InfoObj_Bytes > 2 then begin { 3 Byte InfoObj-Adresse }
            InfoObj_high:=B3;
            InfoObj_medium:=B2;
            S:=Format ('%u-%u-%u', [B3, B2, B1]);
          end
          else if InfoObj_Bytes > 1 then begin  { 2 Byte InfoObj-Adresse }
            InfoObj_high:=B2;
            InfoObj_medium:=0;  // unbenutzt
            S:=Format ('%u-%u', [B2, B1]);
          end
          else begin  { 1 Byte InfoObj-Adresse }
            InfoObj_high:=0;    // unbenutzt
            InfoObj_medium:=0;  // unbenutzt
            S:=Format ('%u', [B1]);
          end;
          InfoObj_low:=B1;
        end;  // with KonfigIEC

        sLogText:=sLogText + CR + LF +
                  '-> InfoObj: ' + IntToStr (iInfoObjAdr) + ' (' + S + ')';

        { Typkennungen unterscheiden: }
        iWert:=-1;
        siWert:=-1;
        IV:=true;
        case IecImportTelegrData.DatenEinheitIdent.Typkennung of
          C_TK_Messwert_normiert_CP56Time2a:
            begin
              // NVA, normierter Wert:
              S:=Copy (sDaten, 1, 2);
              iWert:=Bin2Smallint (S);
              // QDS, Qualitätskennung:
              b:=Bin2Byte (Copy (sDaten, 3, 1));
              IV:=(b AND $80) <> 0;
              NT:=(b AND $40) <> 0;
              SB:=(b AND $20) <> 0;
              BL:=(b AND $10) <> 0;
              OV:=(b AND $01) <> 0;
              // Zeitmarke CP56Time2a:
              S:=Copy (sDaten, 4, 7);
              if not Decode_IEC870_ZeitString_CP56Time2a (S, msec, min, hour, day,
                month, year, IV_Zeit, SU) then begin
                sLogText:=sLogText + '  FEHLER Decode_IEC870_ZeitString_CP56Time2a';
                Continue;
              end else
                sLogText:=sLogText +
                          '  Wert: ' + IntToStr (iWert) +
                          '  IV: ' + IntToStr (integer (IV)) +
                          '  NT: ' + IntToStr (integer (NT)) +
                          '  SB: ' + IntToStr (integer (SB)) +
                          '  BL: ' + IntToStr (integer (BL)) +
                          '  OV: ' + IntToStr (integer (OV)) +
                          '  DZ: ' + Format ('%.2d', [day]) + '.' +
                                     Format ('%.2d', [month]) + '.' +
                                     Format ('%.4d', [year]) + ' ' +
                                     Format ('%.2d', [hour]) + ':' +
                                     Format ('%.2d', [min]) + ':' +
                                     Format ('%.2d', [msec DIV 1000]) + ',' +
                                     Format ('%.3d', [msec MOD 1000]) +
                          '  IV: ' + IntToStr (integer (IV_Zeit)) +
                          '  SU: ' + IntToStr (integer (SU));
            end;

          C_TK_Messwert_Gleitkomma_CP56Time2a:
            begin
              // verkürzte Gleitkommazahl:
              S:=Copy (sDaten, 1, 4);
              siWert:=Bin2Single (S);
              // QDS, Qualitätskennung:
              b:=Bin2Byte (Copy (sDaten, 5, 1));
              IV:=(b AND $80) <> 0;
              NT:=(b AND $40) <> 0;
              SB:=(b AND $20) <> 0;
              BL:=(b AND $10) <> 0;
              OV:=(b AND $01) <> 0;
              // Zeitmarke CP56Time2a:
              S:=Copy (sDaten, 6, 7);
              if not Decode_IEC870_ZeitString_CP56Time2a (S, msec, min, hour, day,
                month, year, IV_Zeit, SU) then begin
                sLogText:=sLogText + '  FEHLER Decode_IEC870_ZeitString_CP56Time2a';
                Continue;
              end else
                sLogText:=sLogText +
                          '  Wert: ' + FloatToStr (siWert) +
                          '  IV: ' + IntToStr (integer (IV)) +
                          '  NT: ' + IntToStr (integer (NT)) +
                          '  SB: ' + IntToStr (integer (SB)) +
                          '  BL: ' + IntToStr (integer (BL)) +
                          '  OV: ' + IntToStr (integer (OV)) +
                          '  DZ: ' + Format ('%.2d', [day]) + '.' +
                                     Format ('%.2d', [month]) + '.' +
                                     Format ('%.4d', [year]) + ' ' +
                                     Format ('%.2d', [hour]) + ':' +
                                     Format ('%.2d', [min]) + ':' +
                                     Format ('%.2d', [msec DIV 1000]) + ',' +
                                     Format ('%.3d', [msec MOD 1000]) +
                          '  IV: ' + IntToStr (integer (IV_Zeit)) +
                          '  SU: ' + IntToStr (integer (SU));
            end;

          C_TK_Zaehlwert_CP56Time2a:
            begin
              // BCR, dualer Zählerstand:
              S:=Copy (sDaten, 1, 4);
              iWert:=Bin2Integer (S);
              // IV, CA, CY, Sequenznummer:
              b:=Bin2Byte (Copy (sDaten, 5, 1));
              IV:=(b AND $80) <> 0;
              CA:=(b AND $40) <> 0;
              CY:=(b AND $20) <> 0;
              SeqNr:=(b AND $1F);
              // Zeitmarke CP56Time2a:
              S:=Copy (sDaten, 6, 7);
              if not Decode_IEC870_ZeitString_CP56Time2a (S, msec, min, hour, day,
                month, year, IV_Zeit, SU) then begin
                sLogText:=sLogText + '  FEHLER Decode_IEC870_ZeitString_CP56Time2a';
                Continue;
              end else
                sLogText:=sLogText +
                          '  Wert: ' + IntToStr (iWert) +
                          '  IV: ' + IntToStr (integer (IV)) +
                          '  CA: ' + IntToStr (integer (CA)) +
                          '  CY: ' + IntToStr (integer (CY)) +
                          '  Seq: ' + IntToStr (SeqNr) +
                          '  DZ: ' + Format ('%.2d', [day]) + '.' +
                                     Format ('%.2d', [month]) + '.' +
                                     Format ('%.4d', [year]) + ' ' +
                                     Format ('%.2d', [hour]) + ':' +
                                     Format ('%.2d', [min]) + ':' +
                                     Format ('%.2d', [msec DIV 1000]) + ',' +
                                     Format ('%.3d', [msec MOD 1000]) +
                          '  IV: ' + IntToStr (integer (IV_Zeit)) +
                          '  SU: ' + IntToStr (integer (SU));
            end;

          C_TK_Initialisierungsende:
            begin
              // COI:
              S:=Copy (sDaten, 1, 1);
              b:=Bin2Byte (S);
                sLogText:=sLogText + '  COI: ' + IntToStr (b);
              Continue;  // enthält keine zu importierenden Daten
            end;
        else
          if IV then;  // nur zum Umgehen der Delphi-Compilerwarnung
          sLogText:=sLogText + '  Nicht unterstützte Typkennung';
          Continue;
        end;  { case aDatenEinheitIdent.Typkennung }

        if FDatenImport then begin
          { Zuordnung IEC -> WICO22 in IEC-Konfigurations-DB suchen: }
          iErg:=IECKonfigDb.GetIECData (KonfigIEC, InfoObj_Bytes,
            sGeraeteArt, sMRG_Kennung, iDSfG_StationId, iDSfG_InstanzId, iDSfG_ArchivNr,
            iKanalNr, sKanaltyp);
          if iErg < 0 then begin
            case iErg of
              -1: begin  // kein Konfig-Datensatz vorhanden
                    sLogText:=sLogText + '  FEHLER Get_KonfigDataIEC: IEC-Konfigdatensatz nicht vorhanden';
                    Continue;
                  end;

              -2: begin  // kein eindeutiger Konfig-Datensatz vorhanden
                    sLogText:=sLogText + '  FEHLER Get_KonfigDataIEC: IEC-Konfigdatensatz nicht eindeutig';
                    Continue;
                  end;
            else  // undefinierter Fehler
              sLogText:=sLogText + '  FEHLER Get_KonfigDataIEC: Undefinierter Fehler';
              Continue;
            end;
          end;

          { Konvertierung in DSfG-Archiv: }
          if (sGeraeteArt = C_GerArtDSfG) then begin
            if DSfG_freigeschaltet then begin
              if IV then begin  // Wert ungültig
                sLogText:=sLogText + '  Kein Datenimport: Wert ungültig (IV)';
                Continue;
              end
              else if IV_Zeit then begin  // Zeitstempel ungültig
                sLogText:=sLogText + '  Kein Datenimport: Zeitstempel ungültig (IV_Zeit)';
                Continue;
              end
              else begin  // gültige Daten
                // in TDSfGSatzData-Struktur packen:
                with DSfGSatzData do begin
                  OrdnungsNr:=0;  // existiert nicht
                  try
                    DatumZeit:=EncodeDateTime (year, month, day, hour, min,
                                               msec DIV 1000, msec MOD 1000);
                  except
                    sLogText:=sLogText + '  FEHLER EncodeDateTime';
                    Continue;
                  end;
                  DateTimeToUnixTimeStr (DatumZeit, sUnixDatumZeit);  // in Unix-Zeit wandeln
                  UnixDatumZeit:=sUnixDatumZeit;
                  Status:='';  // existiert nicht
                  CRC:='';  // existiert nicht
                  if SU then
                    Zeitzone:=CMESZ
                  else
                    Zeitzone:=CMEZ;

                  case IecImportTelegrData.DatenEinheitIdent.Typkennung of
                    C_TK_Messwert_normiert_CP56Time2a,
                    C_TK_Zaehlwert_CP56Time2a:
                      begin
                        WertAsDouble:=iWert;
                        WertAsString:=IntToStr (iWert);
                      end;

                    C_TK_Messwert_Gleitkomma_CP56Time2a:
                      begin
                        WertAsDouble:=siWert;
                        S:=FloatToStr (siWert);
                        StrSubst(S, ',', '.');  { Ausgabe fest mit Dezimal-Punkt }
                        WertAsString:=S;
                      end;
                  else
                    sLogText:=sLogText + '  Nicht unterstützte Typkennung für Geräteart ' +
                              sGeraeteArt;
                    Continue;
                  end;  { case aDatenEinheitIdent.Typkennung }
                end;  { with DSfGSatzData }

                { DSfG-Stammdaten der Instanz lesen: }
                if not DSfGStammdaten.GetInstanzData (iDSfG_InstanzId, InstanzData) then begin
                  sLogText:=sLogText + '  FEHLER GetInstanzData (InstanzId: ' + IntToStr (iDSfG_InstanzId) + ')';
                  Continue;  // Instanzdaten nicht gefunden
                end;
                DEL:='ca' + Chr (iDSfG_ArchivNr + 96)  + Chr (iKanalNr + 101) + 'd';

                // Prüfung, ob Daten der Typkennung in Auto-DB oder Datei konvertiert werden sollen
                AusgabeDirName:=FImportDirList.GetDirName (IecImportTelegrData.DatenEinheitIdent.Typkennung);

                if length (AusgabeDirName) = 0 then begin  // kein Ausgabe-Verzeichnis definiert: Konvertierung in Auto-DB
                  // Zwischendatei mit TDSfGSatzData-Struktur schreiben: }
                  if not WriteZwischenFile_DSfGArchiv (DSfGSatzData, Filename) then begin
                    sLogText:=sLogText + '  Zwischendatei konnte nicht erzeugt werden';
                    Continue;  // Fehler beim Schreiben der Zwischendatei
                  end;

                  { Archiv-/Logbuch-Datenliste für Station-Id: }
                  ArLbDatenListe:=DSfGStationDataList.Get_DSfGDataList (iDSfG_StationId);
                  { Name der Zwischendatei mit Busadresse und DE-Adresse in
                    Archiv/Logbuch-DatenListe eintragen: }
                  DSfGDataListObj:=TDSfGDataListObj.Create;
                  DSfGDataListObj.SetData (InstanzData.Busadresse, DEL);
                  ArLbDatenListe.AddObject (FileName, DSfGDataListObj);
                end
                else begin  // Konvertierung in Datei für externe Weiterverarbeitung
                  if not WriteAusgabeFile_DSfGArchiv_Stunde (AusgabeDirName,
                                                             DSfGSatzData,
                                                             iDSfG_StationId,
                                                             InstanzData.Busadresse,
                                                             DEL) then begin
                    sLogText:=sLogText + '  Ausgabedatei "DSfG-Archiv, Stunde" konnte nicht geschrieben werden';
                    Continue;  // Fehler beim Schreiben der Zwischendatei
                  end;
                end;
              end;
            end;  { if DSfG_freigeschaltet }
          end

          else if (sGeraeteArt = C_GerArtMrg) AND MRG_freigeschaltet then begin
            if MRG_freigeschaltet then begin
              sLogText:=sLogText + '  Nicht unterstützte Geräteart (' + sGeraeteArt + ')';
            end;  { if MRG_freigeschaltet }
          end

          else begin
            sLogText:=sLogText + '  FEHLER Get_KonfigDataIEC: Undefinierte Geräteart (' +
                      sGeraeteArt + ')';
          end;
        end;  { if DatenImport }
      end;  // for i

      if Assigned (IECLogfile) then begin
        if Pos('FEHLER', sLogText) > 0 then
          lt:=lt_Error
        else
          lt:=lt_Info;
        IECLogFile.Write (sLogKopf, sLogText + CR + LF, IecImportTelegrData.Telegramm, lt);
      end;
    end;  { while IecImportTelegrammListe.GetTelegramm }

    { Daten für alle DSfG-Stationen in Zieldaten (Archiv-/Logbuch-DB) konvertieren: }
    for i:=0 to DSfGStationDataList.Count - 1 do begin
      KonvManagerDSfG:=TKonvManagerDSfG.Create (CComIEC,  // Pseudo-Schnittstellennummer für IEC-Datenempfang
        CWMsg_CommType_IEC,  // Kommunikationsart 'IEC' für Wieser-Benachrichtigung
        PathServer.Database [WStammDb], PathServer.Database [AutoDb],
        PathServer.Database [ManuDb], FZwFileLoeschen);
      try
        KonvManagerDSfG.KonvertByStationId (
          TDSfGStationDataListObj (DSfGStationDataList [i]).DSfGDataList,
          TDSfGStationDataListObj (DSfGStationDataList [i]).StationId,
          C_IsArchive, 0, 0);
      finally
        KonvManagerDSfG.Free;
      end;
    end;
  finally
    DSfGStationDataList.Free;
  end;
end;

{-------------------------------------------------------------------------------------}
function TIecImportThread.WriteZwischenFile_DSfGArchiv (DSfGSatzData: TDSfGSatzData;
                                                        var Filename: string): boolean;
{-------------------------------------------------------------------------------------}
{ Zwischendatei mit TDSfGSatzData-Struktur schreiben (für Konvertierung in DB);
  Übergabe: DSfGSatzData-Record
  Rückgabe: Name der Zwischendatei
  Ergebnis: true, wenn Zwischenfile erfolgreich geschrieben werden konnte }
var
  sPref: string;
  pFileName: array[0..255] of char;
  FS: TFileOfRecStream;

begin
  Filename:='';  // Vorbelegung für Rückgabe

  sPref:=prefix_DSfG_Ar;     { Prefix für DSfG-Archivdaten }
  if GetTempFileName (pchar (PathServer [WWorkDir]), pchar (sPref), 0,
                      pFileName) = 0 then begin
    Result:=false;
    exit;  // Fehler beim TempFile erzeugen
  end;

  Filename:=string (pFileName);
  FS:=TFileOfRecStream.Create (FileName, fmOpenReadWrite OR fmShareDenyWrite,
                               SizeOf (TDSfGSatzData));
  try
    FS.WriteRec (DSfGSatzData);
  finally
    FS.Free;
  end;
  Result:=true;
end;

{-----------------------------------------------------------------------------------}
function TIecImportThread.WriteAusgabeFile_DSfGArchiv_Stunde (Pfad: string;
  DSfGSatzData: TDSfGSatzData; StationId: integer; Busadresse, DEL: string): boolean;
{-----------------------------------------------------------------------------------}
{ Ausgabedatei mit TDSfGSatzData-Struktur schreiben (für externe Mittelwertbildung);
  Übergabe: Pfad für Ausgabedatei
            DSfGSatzData-Record
            DSfG-StationId
            Busadresse
            DE-Adresse
  Ergebnis: true, wenn Ausgabedatei erfolgreich geschrieben werden konnte }
var
  Filename: string;
  FS: TFileOfRecStreamExt;
  isOK: boolean;

begin
  { Daten einer Stunde werden in einem File zusammengefaßt: }
  FileName:=Pfad + prefix_DSfG_Ar + IntToStr (StationId) + '_' + Busadresse + DEL +
            FormatDateTime ('yyyymmddhh', DSfGSatzData.DatumZeit) + '.dat';             

  if not FileExists (FileName) then
    FS:=TFileOfRecStreamExt.Create (FileName, fmCreate, SizeOf (TDSfGSatzData), isOK)
  else
    FS:=TFileOfRecStreamExt.Create (FileName, fmOpenReadWrite OR fmShareDenyWrite,
                                    SizeOf (TDSfGSatzData), isOK);
  try
    if isOK then begin
      FS.SeekRec (0, soFromEnd);
      FS.WriteRec (DSfGSatzData);   { DSfG-Satzdata am Ende anhängen }
      Result:=true;
    end else
      Result:=false;
  finally
    FS.Free;
  end;
end;

{-----------------------------------------------------}
procedure TIecImportThread.AbrufJournal_Reorganisieren;
{-----------------------------------------------------}
{ Funktion reorganisiert nur 1 mal am Tag }
var
  SystemEinstellungen: TSystemEinstellungen;
  JournalDB: TJournalDB;

begin
  if Date > LastJournalReorganizeDate then begin
    LastJournalReorganizeDate:=Date;

    SystemEinstellungen:=TSystemEinstellungen.Create (PathServer [WNetProgDir]);
    try
      JournalDB:=TJournalDB.Create (PathServer.Database [WStammDb]);
      try
        JournalDB.Reorganize (SystemEinstellungen.GroesseJournalPuffer);
      finally
        JournalDB.Free;
      end;
    finally
      SystemEinstellungen.Free;
    end;
  end;
end;

end.

