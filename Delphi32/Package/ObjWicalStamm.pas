{$B-}
unit ObjWicalStamm;

interface
uses  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, Math,
      contnrs, Db, DbTables, SysUtils, comCtrls, WTables, unitKonstante;

const
   Length_MstNr   = 20;
   Length_MstName = 40;
   Length_MstKennung = 40;
   Length_KanName = 40;
   Length_KanInfo = 40;
   Length_Einheit = 6;
   Length_Operand = 10;
   Length_Kennung = 14;
   length_ClassName   = 60;
   length_RegressName = 60;
   length_TGLName     = 60;
   length_PrognoseName= 60;
   length_AGName      = 60;
   length_UBS_NetzPro = 40;
   length_Schrift     = 30;
   Length_GrafDef_SF1 = 20;
   Length_ExportDir   = 80;
   Length_MG_Kennzeichen = 5;
   Length_MG_Bezeichnung = 40;
   Length_MG_Einheit     = Length_Einheit;
   Length_Adr_Name       = 30;
   Length_Adr_PLZ        = 5;
   Length_Adr_Ort        = 25;
   Length_Adr_Strasse    = 30;
   Length_InfoCode_Text  = 40;

   c_Tag_NameGeaendert   = 1;
   c_Tag_DateRangeVisble = 2;

   // Messtellen-Gruppen
   C_ImageGroupDefault = 'ImageDefaultGroup';
   c_MstGroupStateOff  = 1;  // Die Gruppe ist deaktiviert

   // Messstellengruppen - Protokolldefinitionen
   // Feld "AWPKonfig" in Tagelle "AWGProtDef"
   c_AWPDKonfig_HochQuer     = $01;    // Netz- und Listenprotokoll
   c_AWPDKonfig_PrintMstNr   = $02;    // Neztptotokoll
   c_AWPDKonfig_PrintMstName = $04;    // Neztptotokoll
   c_AWPDKonfig_PrintLPF     = $08;    // Netzprotokoll: Statistik ohne Leistungspreis freie Tage
   c_AWPDKonfig_SummKanDecode= $10;    // Summenkanal in Summanden auflösen
   c_AWPDKonfig_SummKanPrint = $20;    // Summenaknal zusätzlich zu den Summanden drucken bzw. exportieren
   c_AWPDKonfig_PrintMinima  = $40;    // Netzprotokoll: Statistik Mit Minia drucken
   c_AWPDKonfig_ListProtMDE  = $80;    // Das Listenprotokoll soll als MDE-Prokoll gedruck werden

   // Prognoseprogramm
   // Prognose-Attribute
   c_Attribut_Zeitbereich    = 1;     // Zeitbereich für die Regressionsgeraden und Tgesganglinien
   c_Attribut_WTModus        = 2;     // Wochentag-Modus
   c_Attribut_MiTemp         = 3;     // Mittlere Luftemperatur
   c_Attribut_ZBvon          = 4;     // Benutzer Prognosezeitbereich von
   c_Attribut_ZBbis          = 5;     // Benutzer Prognosezeitbereich nis
   c_Attribut_AKV            = 6;     // Abschaltkundenverwaltung aktiv
   c_Attribut_SPVaktiv       = 7;     // Speicherverwaltung ein/aus
   c_Attribut_SPVModInh      = 8;     // Modus und Ihnhalt

   // Attribut-Besitzer
   c_PAttOwner_PROG = 0;
   c_PAttOwner_REG  = 1;
   c_PAttOwner_TGL  = 2;


   // Grafikdefinition: Tabelle: GrafikDef; Feld: FormatDef
   // Standard - Grafik
   c_GrafikDef_FormDef_LinkeAchse    = $001;
   c_GrafikDef_FormDef_RechteAchse   = $002;
   c_GrafikDef_FormDef_3DDarstellung = $004;
   c_GrafikDef_FormDef_Markiert      = $008;
   c_GrafikDef_FormDef_Gestuft       = $010;
   c_GrafikDef_FormDef_Raender       = $020;
   c_GrafikDef_FormDef_A3            = $040;
   // Geordnete Ganglinie
   c_GrafikDef_FDGG_WertUD           = $001;
   c_GrafikDef_FDGG_3DD              = $002;
   c_GrafikDef_FDGG_Raender          = $004;


   //c_WCAWGListPro: Statusinfo für Spaltendefinition
   c_ListPro_MaxMin     = 1;

   // Auswertegruppen Flag
   c_AWG_Flag_Dienst   = 1;

   // Stammtabellen
   c_WCStammMst       = 'AwMst';                 // 01
   c_WCStammKan       = 'AwKan';                 // 02
   c_WCStammKanFor    = 'AwKanFor';              // 03
   c_WCStammKanMrg    = 'AwMrgKan';              // 04
   c_WCStammKanDSfG   = 'AwDSfGKan';             // 05
   c_WCStammUmKan     = 'AwUmKan';               // 06

   c_WCStammKanText   = 'AwKanText';             // 07

   c_WCStammMstGruppen= 'MstGruppen';            // 08

   c_WCStammSkalierung= 'Skalierung';            // 09

   c_WCFeierTagFest   = 'FeierTagFest';          // 10
   c_WCFeierTagBeweg  = 'FeierTagBeweg';         // 11

   c_WCAWGruppenAuto  = 'AWGruppenAuto';         // 12
   c_WCAWGruppen      = 'AWGruppen';             // 13
   c_WCAWGObject      = 'AWGObject';             // 14
   c_WCAWGFunktion    = 'AWGFunktion';           // 15
   c_WCAWGListPro     = 'AWGListPro';            // 16
   c_WCAWGProtDef     = 'AWGProtDef';            // 17
   c_WCAWGNetzProCol  = 'AWGNetzProCol';         // 18

   c_WCGrafikDef      = 'GrafikDef';             // 19

   c_WCBrennwertTabelle  = 'BrennwertTabelle';   // 20
   c_WCDruckstufen       = 'Druckstufen';        // 21
   c_WCLuftdruckBezirke  = 'LuftdruckBezirke';   // 22

   c_WCExportMarken      = 'exportmarken';       // 23
   c_WCClasses           = 'classes';            // 24

   c_P_Prognose             = 'p_prognose';             //25
   c_P_PrognoseAttribute    = 'p_prognoseattribute';    //26

   c_P_Regressionsgerade    = 'p_regressionsgerade';    //27
   c_P_TGL                  = 'p_tgl';                  //28
   c_P_AG                   = 'p_abschaltgruppen';      //29
   c_P_WetterProgDat        = 'p_wetterprogdat';        //30
   c_P_WetterProgAtt        = 'p_Wetterprogatt';        //31

   c_KonvertJournal         = 'konvertjournal';         //50
   c_Sonderzeichen          = 'sonderzeichen';          //51

   c_WCMessgroessen         = 'messgroessen';           //52
   c_WCMdeInfocode          = 'mde_infocode';           //53
   c_WCMdeInfocodeJournal   = 'mde_infocodejrn';        //54

   // Aw_Mst
   c_AwMst_MstLfdNr            = 'MstLfdNr';
   c_AwMst_MstNummer           = 'MstNummer';
   c_AwMst_MstName             = 'MstName';
   c_AwMst_MstGruppe           = 'AwGruppe';
   c_AwMst_MstKennung          = 'MstKennung';
   c_AwMst_MstTyp              = 'MstTyp';
   c_AwMst_MstLinkToClass      = 'mstlinktoclass';
   c_AwMst_OrdnungNr           = 'ordnungsnr';
   c_AwMst_MstStatus           = 'mststatus';
   c_AwMst_StationsName        = 'stationsname';
   c_AwMst_PLZ                 = 'plz';
   c_AwMst_Ort                 = 'ort';
   c_AwMst_Strasse             = 'strasse';
   // alte Felder, Definitionen müssen wg. Stammdatenänderungsprogramm bleiben
   c_AwMst_MstKeineZeitUmst    = 'KeineZeitUmst';
   c_AwMst_MstNoSZ             = 'mstnosz';


   // AwKan
   C_Awkan_AwkLfdNr       = 'AwkLfdNr';
   C_Awkan_MstLfdNr       = 'MstLfdNr';
   C_Awkan_Name           = 'Name';
   C_Awkan_Formel         = 'Formel';
   C_Awkan_Einheit        = 'Einheit';
   C_Awkan_Typ            = 'Typ';
   C_Awkan_Nachkomma      = 'Nachkomma';
   C_Awkan_Faktor         = 'Faktor';
   C_Awkan_Konstante      = 'Konstante';
   C_Awkan_AuswerteArt    = 'AuswerteArt';
   C_Awkan_OrdnungsNr     = 'OrdnungsNr';
   C_Awkan_Spaltenbreite  = 'Spaltenbreite';
   C_Awkan_FormatIndex    = 'FormatIndex';
   C_Awkan_KanalLink      = 'KanalLink';
   C_Awkan_KanalInfo      = 'KanalInfo';
   c_Awkan_Messgroesse    = 'mgroesse';
   c_Awkan_Schiene        = 'schiene';

   // AwKanFor
   c_AwKanFor_ForLfdNr = 'ForLfdNr';
   C_AwKanFor_AwkLfdNr = 'AwkLfdNr';
   C_AwKanFor_FormelNr = 'FormelNr';
   C_AwKanFor_KanalNr  = 'KanalNr';
   C_AwKanFor_Operand  = 'Operand';
   // AwMRGKan
   c_AwMrgKan_AwkLfdNr   = 'AwkLfdNr';
   c_AwMrgKan_Mrgkennung = 'Mrgkennung';
   c_AwMrgKan_MrgKanalNr = 'MrgKanalNr';
   c_AwMrgKan_TarifZone  = 'TarifZone';
   // AwDSfGKan
   c_AwDSfGKan_AwkLfdNr  = 'AwkLfdNr';
   c_AwDSfGKan_StationId = 'StationId';
   c_AwDSfGKan_InstanzId = 'InstanzId';
   c_AwDSfGKan_ArchivId  = 'ArchivId';
   c_AwDSfGKan_KanalId   = 'KanalId';
   c_AwDSfGKan_HZKanId   = 'HzKanId';  // ID des zu einnem Störklanal gehörenden Hautpzählers-Kanals
   // AwUmKan
   c_AwUmKan_AwkLfdNr    = 'AwkLfdNr';
   c_AwUmKan_AwkVb       = 'AwkVb';
   c_AwUmKan_AwkDruck    = 'AwkDruck';
   c_AwUmKan_AwkTemp     = 'AwkTemp';
   c_AwUmKan_AwkKzahl    = 'AwkKzahl';
   c_AwUmKan_Druck       = 'Druck';
   c_AwUmKan_Temp        = 'Temp';
   c_AwUmKan_Kzahl       = 'Kzahl';
   c_AwUmKan_Luftdruck   = 'Luftdruck';
   c_AwUmKan_Speicher    = 'Speicher';
   c_AwUmKan_SpeicherMin = 'SpeicherMin';

   c_AwKanText_ID        = 'AwkLfdNr';
   c_AwKanText_FktID     = 'AwkFktID';
   c_AwKanText_Text      = 'AwkText';

   c_MstGruppen_ID        = 'GruppenID';
   c_MstGruppen_Name      = 'Gruppenname';
   c_MstGruppen_MpLaenge  = 'MpLaenge';
   c_MstGruppen_EndeAwTag = 'EndeAwTag';
   c_MstGruppen_EndeAwJahr= 'EndeAwJahr';
   c_MstGruppen_ICO_Name  = 'Ico_Name';
   c_MstGruppen_OrdNr     = 'OrdNr';
   c_MstGruppen_State     = 'State';

   c_Skal_ID          = 'SkalID';
   c_Skal_Name        = 'SkalName';
   c_Skal_MpUnten     = 'SkalMpUnten';
   c_Skal_MpOben      = 'SkalMpOben';
   c_Skal_TagUnten    = 'SkalTagUnten';
   c_Skal_TagOben     = 'SkalTagOben';
   c_Skal_MonatUnten  = 'SkalMonatUnten';
   c_Skal_MonatOben   = 'SkalMonatOben';
   c_Skal_Kategorie   = 'SkalKategorie';
   c_Skal_Increment   = 'SkalIncrement';

   // Feiertage fest
   c_FtFest_Monat        = 'Monat';
   c_FtFest_Tag          = 'Tag';
   c_FtFest_FName        = 'FName';
   // Feiertage beweglich
   c_FtBeweg_Datum       = 'Datum';
   c_FtBeweg_FName       = 'FName';

   c_AWGAuto_ID          = 'AWGAutoID';

   c_AWG_ID              = 'AWGID';
   c_AWG_Name            = 'AWGName';
   c_AWG_Typ             = 'AWGTyp';
   c_AWG_ClassLink       = 'awgclasslink';
   c_AWG_ExportDir       = 'awgexportdir';
   c_AWG_MonatsModus     = 'awgmonatsmodus';
   c_AWG_WTModus         = 'awgwtmodus';
   c_AWG_AutoMinOffset   = 'awgautominoffset';
   c_AWG_AutoStunde      = 'awgautostunde';
   c_AWG_AutoModus       = 'awgautomodus';
   c_AWG_TagEnde         = 'awgtagende';
   c_AWG_StartDatum      = 'awgstartdatum';
   c_AWG_CheckHour       = 'awgcheckhour';
   c_AWG_Flags           = 'awgflags';
   c_AWG_JahrEnde        = 'awgjahrende';
   c_AWG_Zeitbereich     = 'awgzeitbereich';

   c_AWGObj_ID           = 'AWObjGID';
   c_AWGObj_Typ          = 'ObjTyp';
   c_AWGObj_ObjID        = 'AWGObjID';
   c_AWGObj_OrdnungsNr   = 'OrdnungsNr';

   c_AWGFkt_ID           = 'AWFktGID';
   c_AWGFkt_FkrGR        = 'FktGruppe';
   c_AWGFkt_Funktion     = 'Funktion';

   // c_WCAWGListPro: Spaltendefinitionen für Listenprotokoll
   c_AWGLP_ID            = 'AWLpGID';
   c_AWGLP_ProtTyp       = 'ProtTyp';
   c_AWGLP_SPID          = 'SPID';
   c_AWGLP_SPText        = 'SPText';
   c_AWGLP_SPBreite      = 'SPBreite';
   c_AWGLP_SPBreiteMaxMin= 'SPBreiteMaxMin';
   c_AWGLP_SPSchrift     = 'SPSchrift';
   c_AWGLP_SPSize        = 'SPSize';
   c_AWGLP_SPStyle       = 'SPStyle';
   c_AWGLP_SPColor       = 'SPColor';
   c_AWGLP_SPMaxMin      = 'MaxMin';     // Spalte wurde gelöscht. Definition muss aus formalen Gründen erhalten bleiben
   c_AWGLP_SPAktiv       = 'SPAktiv';    // Spalte wurde gelöscht. Definition muss aus formalen Gründen erhalten bleiben
   c_AWGLP_StatusInfo    = 'statusinfo';
   // c_WCAWGProtDef: Protokolldefinitionen, globale  Einstellungen
   c_AWGPD_ID            = 'AWPdID';
   c_AWGPD_ProtTyp       = 'AWPdProtTyp';
   c_AWGPD_FontName      = 'AWPFontName';
   c_AWGPD_FontSize      = 'AWPFontSize';
   c_AWGPD_AnzUbs        = 'AWPdAnzUbs';
   c_AWGPD_Ubs1          = 'AWPUbs1';
   c_AWGPD_Ubs2          = 'AWPUbs2';
   c_AWGPD_Konfig        = 'AWPKonfig';
   // c_WCAWGNetzProCol: Spaltendefinitionen für das Netzprotokoll
   c_AWGNP_ID            = 'npcolid';    // Spalten-ID
   c_AWGNP_GrID          = 'grid';       // Gruppen-ID
   c_AWGNP_KanID         = 'kanid';      // Kanal-ID
   c_AWGNP_UBS1          = 'ubs1';
   c_AWGNP_UBS2          = 'ubs2';
   c_AWGNP_UBS3          = 'ubs3';
   c_AWGNP_Breite        = 'breite';
   c_AWGNP_Schrift       = 'fschrift';
   c_AWGNP_Size          = 'fsize';
   c_AWGNP_Style         = 'fstyle';
   c_AWGNP_Color         = 'fcolor';
   c_AWGNP_Ausrichtung   = 'ausrichtung';


   // c_WCGrafikDef
   c_GrafikDef_ID          = 'GrafikDefID';
   c_GrafikDef_FormatDef   = 'FormatDef';
   c_GrafikDef_FormatIndex = 'FormatIndex';
   c_GrafikDef_DatFormDef  = 'DatFormDef';
   c_GrafikDef_Balken      = 'Balken';
   c_GrafikDef_VolKanal    = 'VolKanal';
   c_GrafikDef_AnaKanal    = 'AnaKanal';
   c_GrafikDef_GrafikTyp   = 'grafiktyp';
   c_GrafikDef_SF1         = 'sf1';          // Stringfeld 1
   c_GrafikDef_LineSize    = 'linesize';

   // c_WCBrennwertTabelle
   c_BW_ID   = 'BWID';
   c_BW_KZ   = 'BWKennziffer';
   c_BW_Name = 'BWName';
   c_BW_Wert = 'BWWert';

   // c_WCDruckstufen
   c_DS_ID    = 'DSID';
   c_DS_Name  = 'DSName';
   c_DS_Wert  = 'DSWert';

   // c_WCLuftdruckBezirke
   c_LB_ID    = 'LBID';
   c_LB_Name  = 'LBName';
   c_LB_Wert  = 'LBWert';

   // Exportmarken
   c_em_KanalID   = 'kanalid';
   c_em_ExportTyp = 'exporttyp';
   c_em_LastDate  = 'lastdate';

   // Messstellen-Klassen
   c_ClassID      = 'classid';
   c_className    = 'classname';
   c_classParentID= 'classparentid';
   c_classIndex   = 'classindex';
   c_classLevel   = 'classlevel';
   c_classTyp     = 'classtyp';

   // Regressionsgerade
   C_PReg_ID       = 'regid';
   C_PReg_ProgID   = 'progid';
   C_PReg_Name     = 'regname';
   C_PReg_tm       = 'tm';
   C_PReg_Vm       = 'vm';
   C_PReg_Steigung = 'steigung';
   c_PReg_AnzTage  = 'anztage';
   c_PReg_Klasse   = 'klasse';
   c_PReg_Standard = 'standard';  // Nr. entsprechend Wochentagmodus

   // Prognose-Attribute            // Die Attribudte-Tabelle gilt auch für die Prognose und TGL
   c_PAtt_ID       = 'attid';
   c_PAtt_Owner    = 'owner';      // Prognose =0, REG =1, TGL =2
   c_PAtt_OwnerId  = 'ownerid';    // REG, TGL oder Prognose ID
   c_PAtt_Attribut = 'attribut';
   c_PAtt_WertVon  = 'wertvon';
   c_PAtt_WertBis  = 'wertbis';

   // Prognose
   c_P_ID             = 'progid';
   c_P_Name           = 'progname';
   c_P_VerbrauchId    = 'verbrauchid';
   c_P_TemperaturId   = 'temperaturid';
   c_P_SpeicherId     = 'speicherid';
   c_P_Vmax           = 'vmax';        // Achsenskalierung Tagesmenge
   c_P_Vmin           = 'vmin';
   c_P_Tmax           = 'tmax';        // Achsenskalierung Lufttemperatur
   c_P_Tmin           = 'tmix';
   c_P_Tagmax         = 'tagmax';

   // Tagesganglinie
   c_PTGL_ID          = 'tglid';
   c_PTGL_ProgID      = 'tglprogid';
   c_PTGL_Name        = 'tglname';
   c_PTGL_AnzTage     = 'anztage';
   c_PTGL_Klasse      = 'klasse';
   c_PTGL_Standard    = 'standard';  // Nr. entsprechend Wochentagmodus
   c_PTGL_W00         = 'wert00';   // Wert0 ist der erste Wertdes Tages (z.B. 7-Uhr-Wert
   c_PTGL_W01         = 'wert01';
   c_PTGL_W02         = 'wert02';
   c_PTGL_W03         = 'wert03';
   c_PTGL_W04         = 'wert04';
   c_PTGL_W05         = 'wert05';
   c_PTGL_W06         = 'wert06';
   c_PTGL_W07         = 'wert07';
   c_PTGL_W08         = 'wert08';
   c_PTGL_W09         = 'wert09';
   c_PTGL_W10         = 'wert10';
   c_PTGL_W11         = 'wert11';
   c_PTGL_W12         = 'wert12';
   c_PTGL_W13         = 'wert13';
   c_PTGL_W14         = 'wert14';
   c_PTGL_W15         = 'wert15';
   c_PTGL_W16         = 'wert16';
   c_PTGL_W17         = 'wert17';
   c_PTGL_W18         = 'wert18';
   c_PTGL_W19         = 'wert19';
   c_PTGL_W20         = 'wert20';
   c_PTGL_W21         = 'wert21';
   c_PTGL_W22         = 'wert22';
   c_PTGL_W23         = 'wert23';

   c_pag_id           = 'agid';
   c_pag_progid       = 'progid';
   c_pag_name         = 'agname';
   c_pag_modus        = 'agmodus';
   c_pag_status       = 'agStatus';
   c_pag_tl1          = 'tl1';
   c_pag_tl2          = 'tl2';
   c_pag_tl3          = 'tl3';
   c_pag_tl4          = 'tl4';
   c_pag_tl5          = 'tl5';
   c_pag_tl6          = 'tl6';
   c_pag_tl7          = 'tl7';
   c_pag_tl8          = 'tl8';

   // Wetterprognose - Datumtabelle
   c_pwd_id           = 'wdid';
   c_pwd_datum        = 'datum';

   // Wetterprognose - Attributentabelle
   c_pwa_id           = 'waid';
   c_pwa_datid        = 'datid';
   c_pwa_att          = 'att';
   c_pwa_wert         = 'wert';


   // Konvertjournal
   c_KJ_ID            = 'kjid';
   c_KJ_DatumAktion   = 'datumaktion';
   c_KJ_MstID         = 'mstID';
   c_KJ_MstName       = 'mstname';
   c_KJ_KanalID       = 'kanalID';
   c_KJ_KanalName     = 'kanalname';
   c_KJ_DatumAlt      = 'datumalt';  // Daten vorhanden bis: vor der Konvertierung
   c_KJ_DatumNeu      = 'datumneu';  // Daten vorhanden bis: nach der Konvertierung

   // Sonderzeichen-Tabelle
   c_Sonder_nr      = 'nr';
   c_sonder_zeichen = 'zeichen';

   // Messgroessen
   c_MG_ID           = 'mgid';
   c_MG_Kennzeichen  = 'kennzeichen';
   c_MG_Bezeichnung  = 'bezeichnung';
   c_MG_Typ          = 'typ';
   c_MG_Einheit      = 'einheit';
   c_MG_Kommastellen = 'kommastellen_mde';
   c_MG_MGID_MDE     = 'mgid_mde';

   // MDE_InfoCode
   c_MDEic_nr        = 'infocodenr';
   c_MDEic_txt       = 'infocodetext';

   // MDE_InfoCodeJournal
   c_MDEjrn_ID        = 'icid';
   c_MDEjrn_DT        = 'icdt';
   c_MDEjrn_MstdId    = 'mstid';
   c_MDEjrn_Schiene   = 'schiene';
   c_MDEjrn_Kennung   = 'kennung';
   c_MDEjrn_IcNr      = 'infocodenr';

type


  TWicalTabellen = set of (wical_Mst,Wical_Kan,wical_KanFor,wical_KanUmw,
                           wical_KanMrg,wical_KanDSfG);

  TObjTyp = (OT_Mst,OT_Kanal,OT_Umw,OT_AwGruppe,OT_class,OT_AktionsGruppe,
             OT_prognose,OT_Regress,OT_RegKlasse,OT_TGL,OT_TGLKlasse,ot_WetterPrognose,
             OT_MessGr,OT_MDEJournal);

  TWicalObj = class(Tobject)
    ObjTyp : TObjTyp;
    tag    : byte;
    public
      constructor Create(ot:TObjTyp);
  end;

  TClassObject = class(TWicalObj)
    ClassId        : integer;
    ClassName      : string[length_ClassName];
    ClassParentId  : integer;
    ClassLevel     : integer;
    ClassIndex     : integer;
    procedure copy(source:TClassObject);
  end;


  RecMstAdresse = record
    Name   : string[Length_Adr_Name];
    Strasse: string[Length_Adr_Strasse];
    PLZ    : string[Length_Adr_PLZ];
    Ort    : string[Length_Adr_Ort];
  end;

  TWicalStammSatz = class(TWicalObj)
    Mstid         : integer;
    MstTyp        : byte;
    MstNr         : string[Length_MstNr];
    MstName       : string[Length_MstName];
    MstKennung    : string[Length_MstKennung];
    MstNoSZ       : boolean; // Wenn true, sind die Quelldaten ohne Zeitumstellung
    MstInaktiv    : boolean;
    MstMDE        : boolean;
    MstGruppe     : integer;
    MstKlasse     : integer;
    MstOrdnungsNr : integer;
    MstAdresse    : RecMstAdresse;
    procedure copy(source:TWicalStammSatz);
  end;

  TWStamm = TWicalStammSatz;

  TWCKanalTyp = (UmwerterKanal,FormelKanal,MrgKanal,DSfGKanal);
  TWCKanalDaten = Record
    case TWCKanalTyp of
      UmwerterKanal: (            // Wenn Energiekanal:
        VbLfdNr      : integer;   // Vn
        DrLfdNr      : integer;   // ID der Brennerttabelle (-1 = Faktor ist Brennwert)
        TeLfdNr      : integer;
        KzahlLfdNr   : integer;
        ConDruck     : double;
        ConTemp      : double;
        ConLuftdr    : double;
        ConKzahl     : double;
        ConSpeicher  : double;
        ConSpeicherMin : double;
      );
      FormelKanal: (
        ForLfdNr     : integer;
        AwkLfdNr     : integer;
        FormelNr     : byte;
        OperandKanalNr   : integer; // Summandenkanal usw.
        Operand      : string[Length_Operand];
      );
      MrgKanal: (
         MrgKennung : string[length_Kennung];
         MrgKanalNr : integer;
      );
      DSfGKanal: (
         StationID : integer;
         InstanzID : integer;
         ArchivID  : integer;
         KanalID   : integer;
         HzKanID   : integer;
      )
  end;

  TWicalKanalSatz = class(TWicalObj)
    KanID        : integer;
    Mstid        : integer;
    KanName      : string[Length_KanName];
    Formel       : byte;
    Einheit      : string[Length_Einheit];
    KanTyp       : char;
    Nachkomma    : byte;
    Faktor       : double;
    Konstante    : double;
    AuswerteArt  : integer;
    OrdnungsNr   : integer;
    Spaltenbreite: integer;
    KanalDaten   : TWCKanalDaten;
    FormatIndex  : integer;
    KanalLink    : integer;
    KanalInfo    : string[Length_KanInfo];
    TempStr      : string[Length_KanInfo];  // Wird für temporäre Informationen verwendet
    MessGroesse  : integer;
    SchienenNr   : integer;
    procedure copy(source:TWicalKanalSatz);
  end;

  TWicalUmwSatz = class(TWicalObj)
    AwkVb    : integer;
    AwkDruck : integer;
    AwkTemp  : integer;
    AwkKZahl : integer;
    Druck    : single;
    Temp     : single;
    KZahl    : single;
    Luftdruck: single;
    Speicher : single;
    SpeicherMin : single;
  end;

  TMstGruppe = class(TObject)
    Nummer : integer;
    Name : string;
    Mp   : integer;
    AwTag : integer;
    AwJahr  : integer;
    IconName  : string;
    OrdNummer : integer;
    State     : integer;
  end;

  TAuswerteGruppe = class(TWicalObj)
    GrId        : integer;
    GrName      : string;
    GrTyp       : Integer;
    ClassLink   : integer;
    ExportVerz  : string;
    MonatsModus : integer;
    WTModus     : integer;
    MinOffset   : integer;
    Stunde      : integer;
    AutoModus   : integer;
    TagEnde     : integer;
    JahrEnde    : integer;
    StartDatum  : TDateTime;
    CheckHour   : integer;
    Flags       : integer;
    Zeitbereich : integer;
    procedure Copy(source:TAuswerteGruppe);
  end;

  TGruppenOjekt = class(TObject)
    GrId   : integer;
    ObjTyp : integer;
    ObjID  : integer;
  end;

  TGruppenFunktion = class(TObject)
    GrId   : integer;
    FktGr  : integer;
    Fkt    : integer;
  end;

  RecProtDef = Record
    GrId           : integer;
    ProtTyp        : integer;
    FontName       : string[20];
    FontSize       : byte;
    AnzUbs         : byte;
    Ubs1,Ubs2      : string[60];
    Hochformat,
    PrintMstNr,
    PrintMstName,
    Print_Minima,
    Print_LPF,           // Leistungspreisfreie Tage
    SummKanDecode,       // Summenkanäle in Summanden auflösen
    SummKanPrint,
    ListProtMDE     : boolean
  end;

  RecSkalierung = Record
    SkalId   : integer;
    SkalName : string;
    MpUnten,
    MpOben,
    TagUnten,
    TagOben,
    MonatUnten,
    MonatOben  : double;
    Kategorie  : integer;
    Increment  : double;
  end;

  RecBrennwert = Record
    BWid   : integer;
    BWkz   : integer;
    BWname : string;
    BW     : double;
  end;

  RecGrafikDef = Record
     GruppenID,
     FormatIndexVol,
     FormatIndexAna,
     Format,
     DatumFormat,
     Balken,
     VolKanal,
     AnaKanal,
     LineSize   : integer;
     TextFeld1  : string[Length_GrafDef_SF1];
  end;

  RecAttribute = Record
    WtModus   : integer;
    IsDateFrom,
    IsDateTo  : boolean;
    DFrom,DTo :TDateTime;
    IsMiTemp  : boolean;
    MiTempVon,
    MiTempBis : integer;
  end;

  RecRegressionsgerade = Record
    Id       : integer;
    ProgId   : integer;
    RegName  : string[length_RegressName];
    tm       : double;  // mittlere Temperatur
    vm       : double;  // mittlerere Verbrauch
    steigung : double;
    Klasse   : Integer;
    Standard : Integer;
    AnzTage  : integer;
    Attribute: RecAttribute;
  end;

  TObjRegressGerade = class(TWicalObj)
    REG : RecRegressionsgerade;
    function Abgabe(t:double):double;
  end;

  RecPrognose = Record
    ProgId   : integer;
    ProgName : string[length_PrognoseName];
    VKanalId : Integer;
    TKanalId : Integer;
    SKanalID : integer;
    Vmax     : double;
    Vmin     : double;
    Tmax     : double;
    Tmin     : double;
    TagMax   : double;
    DateFrom : integer;  // Benutzter Zeitbereich für die historischen Daten
    DateTo   : integer;
  end;

  TPrognoseObject = class(TWicalObj)
    Prognose : RecPrognose;
  end;

  TStdWerte  = array[0..23] of double;

  RecTagesGanglinie = Record
    MiTemp   : double;
    Id       : integer;
    ProgId   : integer;
    TGLName  :  string[length_TGLName];
    Klasse   : Integer;
    Standard : Integer;
    AnzTage  : integer;
    Swert    : TStdWerte;
    StandAbw : TStdWerte;
    SumSAbw  : double;
    Attribute: RecAttribute;
  end;

  TObjTagesganglinie = class(TWicalObj)
    TGL : RecTagesGanglinie;
    constructor Create(ot:TObjTyp);
  end;

  TObjKlasse = class(TWicalObj)
    WtModus : integer;
  end;



  // --------------------- MDE
  RecMdeInfoCodeJournal = record
    id      : integer;
    DateTime: TDateTime;
    MstId   : integer;
    Mstname : string[Length_MstName];
    schiene : integer;
    infoNr  : integer;
    infoText: string[Length_InfoCode_Text];
  end;

  TMdeInfoCodeJournal = class(TWicalObj)
    InfoCodeJournal : RecMdeInfoCodeJournal;
  end;

  TMDEInfoCodeTable = class(Tobject)
    MdeJQuery  : TQueryExt;
    MdeJTable  : TTableExt;
    MdeItQuery : TQueryExt;
    constructor Create;
    destructor Destroy; override;
    function OpenMDEInfoCode(DTvon:TDateTime=0;DTbis:TDateTime=0):boolean;
    function GetMdeInfoJournal(var JournalSatz:RecMdeInfoCodeJournal):boolean;
    function GetMdeInfoText(var JournalSatz:RecMdeInfoCodeJournal):boolean;
    procedure DeleteJournalRecord(id:integer);
  end;

  RecMessgroesse = record
    mgid        : integer;
    kennzeichen : string[length_mg_kennzeichen];
    Bezeichnung : string[length_mg_Bezeichnung];
    Typ         : char;
    Einheit     : string[length_mg_einheit];
    KommaStellen: integer;
    CharId      : char;
  end;
  TobjMessgroesse = class(TWicalObj)
    MG : RecMessgroesse;
  end;

  TMGTable = class(Tobject)
    MGQuery : TQueryExt;
    MGTable : TTableExt;
    constructor Create;
    destructor Destroy; override;
    procedure WriteMGListe(id:integer;Kz,Bz,Typ,Eh:string;Komma:integer;MgId:char);
    procedure OpenMGListe;
    function GetMGSatz(var Messgroesse : RecMessgroesse):boolean;overload;
    function GetMGSatz(index: integer; var Messgroesse : RecMessgroesse):boolean;overload;
 end;

/// Wetterprognose
  TWerteFeld = Record
    Valid : Boolean;
    Wert  : Double;
  end;

  RecWetterPrognose = Record
    DatId : integer;
    Datum : TDateTime;
    LTMin : TWerteFeld;
    LTMax : TWerteFeld;
    LTMit : TWerteFeld;
    WindR : TWerteFeld;
    WindG : TWerteFeld;
    WLage : TWerteFeld;
  end;

  TWetterPrognose = class(TWicalObj)
    WP : RecWetterPrognose;
  end;

  TWPTable = class(Tobject)
      WPQuery : TQueryExt;
      WPTable : TTableExt;
      ATQuery : TQueryExt;
      ATTable : TTableExt;
      constructor Create;
      destructor Destroy; override;
      function OpenWPDatum:boolean;
      function ExistsWPDatum(Datum:TDateTime):integer;
      function InsertNewWP(Datum:TDateTime):integer;
      procedure WriteAT(DatId,Att:integer; Wert:double);
      function ReadAT(DatId,Att:integer; var Wertf:TWerteFeld):boolean;
      function ReadAlleAT(DatId:integer; var WP:RecWetterPrognose):boolean;
      function GetNextWP(var Datum:TDateTime):integer;
  end;

///// Konvertierung

 TObjFormelKanal = class(Tobject)
   KanalId        : integer;
   Formel         : integer;
   OperantenListe : TobjectList;
 end;

  RecKonvertJournal = Record
     JournalId    : integer;
     AktionsDatum : TDateTime;
     MstId        : integer;
     MstNamen     : string[Length_MstName];
     KanalId      : integer;
     KanalName    : string[Length_KanName];
     DatenEndeAlt : TDateTime;
     DatenEndeNeu : TDateTime;
  end;

  TKonvertJournalTable = class(Tobject)
      KJQuery : TQueryExt;
      KJTable : TTableExt;
    public
      constructor Create;
      destructor Destroy; override;
      function InsertNewRecord(var MyKonvertJournal:RecKonvertJournal):boolean;
      procedure JournalBegrenzen(max:integer);
      function OpenJournal:integer;
      function GetEintrag(var MyKonvertJournal:RecKonvertJournal):boolean;
  end;
/////
  RecAbschaltgruppe = Record
    agid    : integer;
    progid  : integer;
    agname  : string [length_AGName];
    modus   : integer;
    aktiv   : boolean;
    Einaus  : boolean;
    tl      : array[1..8] of double;
  end;

  TAbschaltgruppe = class(Tobject)
    Abschaltgruppe : RecAbschaltgruppe;
  end;

  TAGTable = class(Tobject)
      AGTable : TTableExt;
      AGQuery : TQuery;
      constructor Create;
      destructor Destroy; override;
      function OpenAGTable(ProgId:integer):boolean;
      function InsertNewRecord(var Abschaltgruppe:RecAbschaltgruppe):boolean;
      function WriteAbschaltgruppe(var Abschaltgruppe:RecAbschaltgruppe):boolean;
      procedure ReadAG(var Abschaltgruppe:RecAbschaltgruppe);
      function GetNextAG(var Abschaltgruppe:RecAbschaltgruppe):boolean;
  end;
/////
  TPrognoseTable = class(Tobject)
      ProgAttTable : TTableExt;
      ProgAttQuery : TQuery;
      Atyp         : integer;
    public
      constructor Create;
      destructor Destroy; override;
      function InsertAttribut(Id,Attribut:integer; WertVon,WertBis:integer):boolean;
      function GetAttribute(Id,Attribut:integer):boolean;overload;
      function GetAttribute(Id,Attribut:integer; var Wert1:integer):boolean;overload;
      function GetAttribute(Id,Attribut:integer; var Wert1,Wert2:integer):boolean;overload;
      procedure GetAttribute(Id:integer; var Attribute:RecAttribute);overload;
  end;

  TProgRegionTable = class(TPrognoseTable)
      ProgTable : TTableExt;
      ProgQuery : TQuery;
    public
      constructor Create;
      destructor Destroy; override;
      function OpenPrognoseTable:boolean;
      function InsertNewPrognose(var Prognose:RecPrognose):boolean;
      function GetNextPrognose(var Prognose:RecPrognose):boolean;
      function WritePrognose(var Prognose:RecPrognose):boolean;
      function WritePrognoseName(ProgId:integer;ProgName:string):boolean;
      function ReadPrognose(var Prognose:RecPrognose):boolean;
      Procedure LoeschePrognose(ProgId:integer);
    private
      procedure ReadSelectPrognose(var Prognose:RecPrognose);
  end;

  TPTglTable = class(TPrognoseTable)
    public
      TGLTable : TTableExt;
      TGLQuery : TQuery;
      constructor Create;
      destructor Destroy; override;
      function InsertNewTGL(var TGL:RecTagesGanglinie):boolean;
      function OpenTGLTable(ProgId:integer):boolean;
      function OpenTGLTableSortWT(ProgId:integer):boolean;
      function GetNextTGL(var TGL:RecTagesGanglinie):boolean;
      function ExistsTGLWt(ProgId,Klasse,Wt:integer):integer;
      function ExistsTGLName(ProgId,Klasse:integer;TGLName:string):integer;
      procedure LoescheTGL(TGLId:integer);
      procedure LoescheAlleTGL(progId,Klasse:integer);
      function WriteTGLName(TGLId:integer;TGLName:string):boolean;
    private
      procedure ReadSelectTGL(var TGL:RecTagesGanglinie);
  end;

  TPRegressTable = class(TPrognoseTable)
      RegTable : TTableExt;
      RegQuery : TQuery;
    public
      constructor Create;
      destructor Destroy; override;
      function OpenRegTable(Progid:integer):boolean;
      function OpenRegTableSortWT(ProgId:integer):boolean;
      function GetNextRegress(var Reggerade:RecRegressionsgerade):boolean;
      function InsertNewRegress(var Reggerade:RecRegressionsgerade):boolean;
      procedure LoescheReg(RegId:integer);
      procedure LoescheAlleReg(ProgId,Klasse:integer);
      function ExistsRegName(ProgId,Klasse:integer;RegName:string):Integer;
      function ExistsRegWt(ProgId,Klasse,Wt:integer):integer;
      function WriteRegName(RegId:integer;RegName:string):boolean;
      procedure CloseRegAttTable;
    private
      procedure ReadSelectRegress(var Reggerade:RecRegressionsgerade);
  end;

  TObjClassesTable = class(Tobject)
      ClassTable : TTableExt;
      ClassQuery : TQuery;
      ClassTyp   : integer;
    public
      constructor Create(CT:integer);
      destructor Destroy; override;
      function InsertNewClass(const KlassenName:string;classParentID,classLevel,classIndex:integer):integer;
      procedure OpenClassTable(classLevel:integer);
      function GetClassName(ClassId:integer):string;
      function GetParentClassID(ClassId:integer):integer;
      procedure GetClass(ClassObject:TClassObject);
      function OpenClass(ClassId:integer):boolean;
      procedure WriteClassName(const KlassenName:String);
      procedure WriteClassIndex(ClInd:integer);
      procedure WriteClass(ParentId,ClLevel,ClIndex:integer);
      procedure DeleteClass(ClassId:integer);
      function ClassTableEof:boolean;
  end;

  TObjBrennwertTabelle =  class(Tobject)
      BWQuery : TQuery;
    public
      constructor Create;
      destructor Destroy; override;
      function GetBWDaten(BwID:integer; var BWdaten:RecBrennwert):boolean;
  end;

  TObjDruckstufenTabelle =  class(Tobject)
      DSQuery : TQuery;
    public
      constructor Create;
      destructor Destroy; override;
      function GetDruckDaten(Druckstufe:integer; var XName:string; var Druck:double):boolean;
  end;

  TObjLuftdruckTabelle =  class(Tobject)
      LBQuery : TQuery;
    public
      constructor Create;
      destructor Destroy; override;
      function GetLuftdruckDaten(Bezirk:integer;var Xname:string; var Luftdruck:double):boolean;
  end;

  TObjMstGroup = class(Tobject)
    private
      GrQuery : TQuery;
    public
      constructor Create;
      destructor Destroy; override;
      function GetMaxOrdnr:integer;
      function GetGrIdMinOrdnr:integer;
      function DeleteMstGruppe(ID : integer):boolean;
      function WriteDaten(MstGruppe:TMstGruppe):boolean;
      function WriteIconName(MstGruppe:TMstGruppe):boolean;
      function WriteOrdnungsNr(MstGruppe:TMstGruppe):boolean;
      function WriteState(MstGruppe:TMstGruppe):boolean;
      function InsertRecord(MstGruppe:TMstGruppe):boolean;
      function OpenMstGruppen(RL:boolean):TQuery;
      function OpenMstGruppenSort:TQuery;
      function GetGroupName(GrID:integer):string;
      function GetImageName(GrID:integer):string;
  end;

  TRecNetzproSpalte = Record
    SpaltenId : integer;
    GruppenId : integer;
    KanalId   : integer;
    Ubs     : array[1..3] of string[length_UBS_NetzPro];
    Breite  : integer;
    Schrift : string[length_Schrift];
    Size    : integer;
    Style   : integer;
    Color   : integer;
    Ausrichtung  : integer;
  end;

  TObjNetzproSpalte = class(Tobject)
    Spalte : TRecNetzproSpalte;
  end;

  TObjAwGruppeTable = class(Tobject)
    private
      WorkQuery  : TQueryExt;
      GroupQuery : TQuery;
      AutoQuery  : TQuery;
    public
      constructor Create;
      destructor Destroy; override;
      function ObjectListeLaden(GrId:integer; ObjListe : TObjectList):boolean;
      function OpenAutoTabel(RL:boolean):TQuery;
      procedure CloseAutoTabel;
      procedure ClearAutoTabel;
      Procedure InsertAutoTabel(GrId:Integer);
      function FunktionListeLaden(GrId:integer; ObjListe : TObjectList):boolean;
      function GetGroup(AuswerteGruppe:TAuswerteGruppe):boolean;
      function GruppenListeLaden(AwGruppenListe : TObjectList;GrTyp:integer;ClassLink:integer=0):boolean;
      function ExistsGroupNameNo(GrName:String):boolean;
      procedure DeleteObject(GrID,ObjTyp,ObjID:integer);
      procedure DeleteObjectsAndFunctions(GrID:integer);
      procedure DeleteMst(MstId:integer);
      procedure DeleteKan(KanalId:integer);
      procedure DeleteGroup(GrID:integer);
      procedure DeleteGroupProtDef(GrID:integer); overload;
      procedure DeleteGroupProtDef(ProtDef:RecProtDef); overload;
      procedure DeleteGroupListPro(GRID:integer);
      function NewGroupName(GrId:integer; GrName:string):integer;
      function NewClassLink(GrId:integer; CL:integer):boolean;
      function NewExportVerz(GrId:integer; ExportVerz:string):boolean;
      function WriteAWG(AWG:TAuswerteGruppe):boolean;
      function InsertNewGroup(GrName:String;Typ,Klasse:integer):integer;
      function InsertNewObject(GrId,ObjTyp,ObjID,OrnungsNr:Integer):boolean;
      function InsertNewFunction(GrId,FktGruppe,Funktion:Integer):boolean;
      function InsertNewLPSpalte(GrId,PTyp,SpId,Breite,BreiteMM,Size,Style,Color:Integer;
               SpText,Schrift:string; MaxMin:boolean):boolean;
      function GetSpaltenliste(GrId,PTyp:integer):TQuery;
      function GetSpaltenDaten(GrId,PTyp,SpId:integer):TQuery;overload;
      function InsertNewProtDef(ProtDef:RecProtDef):boolean;
      function GetProtDef(var ProtDef:RecProtDef):boolean;

      // Netzprokoll-Spaltendefinitionen
      function GetColProNetz(var SpaltenDef : TRecNetzproSpalte):boolean;
      procedure DeleteColProNetz(GRID:integer);
      function InsertNewNetzProCol(Spalte:TRecNetzproSpalte):boolean;
      // Grafik-Definitionen
      function InsertNewGrafikDef(GrId,FormatIndex,Format,DatFormDef,Balken,
                                  VolKanal,AnaKanal,GrafikTyp,LineSize:integer;
                                  TextFeld1:string=''):boolean;
      function GetGrafikDef(Var GrafikDef:RecGrafikDef;GrafikTyp:integer):boolean;
      procedure DeleteGafikDef(GrID:integer;GrafikTyp:integer);
      procedure DeleteGafikDefAlle(GrID:integer);

  end;

  TWicalStamm = class(Tobject)
    private
      Query          : TQuery;
      MstQuery       : TQuery;
      KanQuery       : TQuery;
      KanForQuery    : TQuery;
      KanUmwQuery    : TQuery;
      KanMrgQuery    : TQuery;
      KanDSfGQuery   : TQuery;
      LocalQuery     : TQuery;
      LocMstQuery    : TQuery;
      LocKanQuery    : TQuery;
      LocForQuery    : TQuery;
      SkalQuery      : TQuery;
      MstLfdNr       : integer;
    public
     {Public-Deklarationen }
      GruppenListe  : TObjectList;
      TabellenListe :  TWicalTabellen;
      MstStammSatz  : TWicalStammSatz;
      KanalStammSatz: TWicalKanalSatz;
      constructor Create;
      destructor Destroy; override;

      function GetImageNrClass(ClassObject:TClassObject):integer;
      function GetImageNrKanal(Node:TTreeNode):integer; overload;
      function GetImageNrKanal(Formel:integer):integer; overload;
      ////////////////////////////////////////////////////////////
      function GetAnzMpProTag(MstId:integer):integer;
      function GetMpLaenge(MstId:integer):integer;
      function GetTagEnde(MstId:integer):integer;
      function GetJahrEnde(MstId:integer):integer;
      ///////////////////////////////////////////////////////////
      function OpenKanTables:TQuery; overload;
      function OpenKanTables(MstId:integer):TQuery; overload;
      function OpenKanTablesFormel(KanalListe:TObjectList; Formel:integer):Integer;
      procedure OpenKanTablesOrderBy;
      procedure OpenTablesNew(TL:TWicalTabellen; RL:boolean);
      procedure OpenTables(TL:TWicalTabellen; RL:boolean);
      procedure OpenTablesSort(TL:TWicalTabellen; SortFeld1,SortFeld2,SortFeld3:string);
      procedure CloseTables;

      function ExistsMst(MstID:integer):boolean;
      function ExistsKanal(KanalID:integer):boolean;

      procedure InsertMst(var Mst:TWicalStammSatz);
      procedure WriteMst(Mst:TWicalStammSatz);
      procedure DeleteMst(Mst:TWicalStammSatz);

      function SucheMst(MstId:integer):boolean;
      function SucheAwKanal(KanId:integer):boolean;
      procedure WriteAwKanal(Kanal:TWicalKanalSatz);
      procedure WriteStr(const FeldName, Wert:string);
      procedure WriteInteger(const FeldName:string; Wert:variant);
      procedure WriteFloat(const FeldName:string; Wert:variant);
      procedure QueryPost;

      procedure InsertKanal(var Kanal:TWicalKanalSatz);
      procedure InsertMRGKanal(Kanal:TWicalKanalSatz);
      procedure InsertDSfGKanal(Kanal:TWicalKanalSatz);
      procedure InsertFormelKanal(Kanal:TWicalKanalSatz);
      procedure InsertUmwerterKanal(Kanal:TWicalKanalSatz);

      procedure DeleteKanal(Kanal:TWicalKanalSatz);
      procedure DeleteMrgKanal(KanalId:integer);
      procedure DeleteDSfGKanal(KanalId:integer);
      procedure DeleteFormelKanal(KanalId:integer);
      procedure DeleteEinFormelKanal(ForLfdNr:integer);
      procedure DeleteAlleFormelKanaele(KanalId:integer);
      procedure DeleteUmwerterKanal(KanalId:integer);

      function GetMrgKanalDaten(var Kanal:TWicalKanalSatz):boolean;
      function SelectMRGKanaele(const Kennung:string):integer;

      function GetMRG910KanalListe(var AwkId:array of integer):integer;
      function GetDSfGKanalDaten(var Kanal:TWicalKanalSatz):boolean;

      function FindMrgFirstKanal(var Kanal:TWicalKanalSatz):integer;
      function FindMrgNextKanal(var Kanal:TWicalKanalSatz):boolean;

      function FindDSfGFirstKanal(var Kanal:TWicalKanalSatz):boolean;
      function FindDSfGNextKanal(var Kanal:TWicalKanalSatz):boolean;
      function WriteDSfGKanaele(Kanal:TWicalKanalSatz):boolean;
      function GetSzKanalID(HzKanalId:integer):integer;

      function GetUmwerterKanalDaten(Kanal:TWicalKanalSatz):boolean;

      function SelectFormelKanaele(KanalId:integer; Formel:byte):boolean;
      function GetFormelKanalDaten(var Kanal:TWicalKanalSatz):boolean;
      function GetFormelKanalQuery:TQuery;

      function GetMaxKanalOrdNr(MstId:integer):integer;
      function GetKanalListe(MstId:Integer;KanalListe:TObjectList):boolean;
      function GetLocKanalListe(KanalSelektIndex:integer;KanalListe:TObjectList):boolean; overload;
      function GetLocKanalListe(KanalListe:TObjectList):boolean; overload;
      function GetLocKanalDaten(Kanal:TWicalKanalSatz):boolean; overload;
      function GetLocKanalDaten(KanalId:integer):boolean; overload;
      function GetKanalDaten(KanalId:integer; Kanal:TWicalKanalSatz):boolean;
      function GetFirstKanalDaten(MstId:integer; var  Kanal:TWicalKanalSatz):boolean;
      function GetNextKanalDaten(var Kanal:TWicalKanalSatz):boolean;
      function LeseKanalDaten(var Kanal:TWicalKanalSatz):boolean; overload;
      procedure LeseKanalDaten(KQuery:TQuery; var Kanal:TWicalKanalSatz); overload;
      function IstMstAktiv(MstId:integer):boolean;

      procedure LeseMstDaten(MstQuery:TQuery; var MstSatz:TWicalStammSatz);
      function GetMstName(MstId:integer):String;
      function GetMstKennung(MstId:integer):String;
      function GetMstNummer(MstId:integer):String;
      function GetNextMstDaten(var StammSatz:TWicalStammSatz):boolean;
      function GetMstDaten(MstId:integer; var StammSatz:TWicalStammSatz):boolean; overload;
      function GetMstDaten(MstId:integer):boolean; overload;

      function GetMstNummerName(MstId:integer):string;
      function GetMstDatenAwGruppe(MstId:integer):Integer;

      function GetMstListeKlasse(Klasse:integer;MstListe:TObjectList):boolean;

      function GetNameKanalTyp(KanTyp:integer):string;

      function GetMstId(KanalId:integer):integer;

      function ValidTypSumme(KanalTyp:integer):boolean;
      function ValidTypVnBerechnen(Kanal:TWicalKanalSatz):boolean;
      function ValidTypManuellerMDEkanal(Kanal:TWicalKanalSatz):boolean;
      function ValidTypEnergieBerechnen(Kanal:TWicalKanalSatz):boolean;
      function ValidTypBrennwertBerechnen(Kanal:TWicalKanalSatz):boolean;
      function ValidTypZZahlBerechnen(Kanal:TWicalKanalSatz):boolean;

      function OpenTableFormel(KanalID:integer):integer;
      function GetFormelKanal(Summand:TWicalKanalSatz):boolean;

      Procedure GetEinheitenListe(EinhListe:TStringList);

      // Skalierung
      function OpenSkalQueryOrderByName(Kategorie:integer):TQuery; overload;
      function OpenSkalQueryOrderByName:TQuery; overload;
      function OpenSkalQuery:TQuery;
      function GetSkalierungQuery(SkalId:Integer):TQuery;
      function GetSkalierung(FormatIndex:Integer; var Skalierung:RecSkalierung):boolean;
      function InsertSkalierung(Skalierung:RecSkalierung):integer;
      function WriteSkalierung(Skalierung:RecSkalierung):boolean;
      procedure DeleteSkalierung(SkalId:integer);

      // Kanal-Text-Tabelle
      procedure DeleteKanalTextAlles(kanalID:Integer);
      procedure DeleteKanalTextKonfig(kanalID:Integer);
      procedure DeleteKanalText(kanalID,FktID:Integer);
      procedure InsertKanalText(kanalID,FktID:Integer;KText:String);
      function GetKanalText(kanalID,FktID:integer):string;
      function GetKanalTextToInteger(kanalID,FktID:integer;var Resultat:integer):boolean;
      function GetKanalTextName(kanal:TWicalKanalSatz;FktID:integer):string;
      function GetKanalTextToDouble(kanalId,FktID:integer; var Wert:double):boolean;

      // Exportmarken
      procedure WriteExportMarke(kanalID,FktID:Integer; Datum:TDateTime);
      function GetExportMarke(kanalID,FktID:integer):TDateTime;
      procedure DeleteExportMarke(kanalID,FktID:Integer);

  end;

var WicalStamm : TWicalStamm;

Function IsStoermengenKanal(Kanal:TWicalKanalSatz):boolean;
Function GetManuellFunktion(AwArt:integer):integer;
Function IsSummenFormelKanal(Kanal:TWicalKanalSatz):boolean;
Function IsExcelFormelKanal(Kanal:TWicalKanalSatz):boolean;
Function IsGERGFormelKanal(Kanal:TWicalKanalSatz):boolean;
Function IsKonstanteKanal(Kanal:TWicalKanalSatz):boolean;
Function IsMdeKanal(Kanal:TWicalKanalSatz):boolean;
Function IsMdeMst(Kanal:TWicalKanalSatz):boolean;
Function IsNotMdeKanal(KanalNode:TTreeNode):boolean;
Function IsImportKanal(Kanal:TWicalKanalSatz):boolean;
procedure SchreibeProgAtt(ProgId,Attribut,von,bis:integer);
procedure LeseProgAtt(ProgId,Attribut: integer; var von,bis:integer);
function IsProgAtt(ProgId,Attribut: integer):boolean;

implementation
uses pathini, Myini, DbAutoInc, StrUtils, util;

//================================================================= =============
constructor TObjTagesganglinie.Create(ot:TObjTyp);
var index : integer;
begin
  inherited Create(ot);
  for index:=0 to 23 do
    TGL.swert[index]:=0;
end;

//================================================================= =============
function TObjRegressGerade.Abgabe(t:double):double;
begin
  result:=-REG.steigung*(t-Reg.tm)+Reg.vm;
end;

//================================================================= =============
procedure TClassObject.copy(source:TClassObject);
begin
  ClassId:=source.ClassId;
  ClassName:=source.ClassName;
  ClassParentId:=source.ClassParentId;
  ClassLevel:=source.ClassLevel;
  ClassIndex:=source.ClassIndex;
end;

//================================================================= =============
procedure TWicalStammSatz.copy(source:TWicalStammSatz);
begin
  Mstid:=source.Mstid;
  MstNr:=source.MstNr;
  MstName:=source.MstName;
  MstKennung:=source.MstKennung;
  MstTyp:=source.MstTyp;
  MstNoSZ:=source.MstNoSZ;
  MstInaktiv:=source.MstInaktiv;
  MstMDE:=source.MstMDE;
  MstGruppe:=source.MstGruppe;
  MstKlasse:=source.MstKlasse;
  MstOrdnungsNr:=source.MstOrdnungsNr;
  MstAdresse:=source.MstAdresse;
end;

//================================================================= =============
procedure TWicalKanalSatz.copy(source:TWicalKanalSatz);
begin
  KanId:=source.KanID;
  Mstid:=Source.Mstid;
  KanName:=Source.KanName;
  Formel:=Source.Formel;
  Einheit:=Source.Einheit;
  self.KanTyp:=Source.KanTyp;
  self.Nachkomma:=Source.Nachkomma;
  self.Faktor:=Source.Faktor;
  self.Konstante:=Source.Konstante;
  self.AuswerteArt:=Source.AuswerteArt;
  self.OrdnungsNr:=Source.OrdnungsNr;
  self.Spaltenbreite:=Source.Spaltenbreite;
  self.FormatIndex:=Source.FormatIndex;
  self.KanalLink:=Source.KanalLink;
  self.KanalInfo:=Source.KanalInfo;
  self.KanalDaten:=Source.KanalDaten;
  self.MessGroesse:=source.MessGroesse;
  self.SchienenNr:=source.SchienenNr;
end;



//==============================================================================
//------------------------------------------------------------------------------
procedure TAuswerteGruppe.Copy(source:TAuswerteGruppe);
begin
  ObjTyp:=source.ObjTyp;
  GrId:=Source.GrId;
  GrName:=Source.GrName;
  GrTyp:=Source.GrTyp;
  ClassLink:=Source.ClassLink;
  ExportVerz:=Source.ExportVerz;
  MonatsModus:=source.MonatsModus;
  WTModus:=source.WTModus;
  MinOffset:=source.MinOffset;
  Stunde:=source.Stunde;
  AutoModus:=source.AutoModus;
  TagEnde:=source.TagEnde;
  StartDatum:=source.StartDatum;
  CheckHour:=source.CheckHour;
  Flags:=source.Flags;
  JahrEnde:=source.JahrEnde;
  ZeitBereich:=source.Zeitbereich;
end;

//==============================================================================
//------------------------------------------------------------------------------
constructor TMDEInfoCodeTable.Create;
begin
  inherited Create;
  MdeJQuery:=TQueryExt.Create(nil);
  MdeJQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  MdeItQuery:=TQueryExt.Create(nil);
  MdeItQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  MdeJTable:=TTableExt.Create(nil);
  MdeJTable.DataBaseName:=PathServer.PathName[WWicalStammDb];
  MdeJTable.TableName:=c_WCMdeInfocodeJournal;
end;

//------------------------------------------------------------------------------
destructor TMDEInfoCodeTable.Destroy;
begin
  MdeJQuery.Close;
  MdeJQuery.Free;
  MdeItQuery.Close;
  MdeItQuery.Free;
  MdeJTable.Close;
  MdeJTable.Free;
end;

//------------------------------------------------------------------------------
function TMDEInfoCodeTable.OpenMDEInfoCode(DTvon:TDateTime=0;DTbis:TDateTime=0):boolean;
begin
  with MdeJQuery do begin
    sql.text:='select * from '+c_WCMdeInfocodeJournal+','+c_WCStammMst;
    sql.add('where '+c_MDEjrn_MstdId+' = '+c_AwMst_MstLfdNr);
    sql.add('and '+  c_MDEjrn_DT+    ' >= :Von');
    if not isZero(DTbis,1/24) then
      sql.add('and '+  c_MDEjrn_DT+    ' <= :Bis');
    parambyName('Von').AsDateTime:=DTvon;
    if not isZero(DTbis,1/24) then
      parambyName('Bis').AsDateTime:=DTbis+1;
    open;
    result:= not eof;
  end
end;

//------------------------------------------------------------------------------
function TMDEInfoCodeTable.GetMdeInfoJournal(var JournalSatz:RecMdeInfoCodeJournal):boolean;
begin
  with MdeJQuery,JournalSatz do begin
    if eof then
      result:=false
    else begin
      id:=FieldByName(c_MDEjrn_ID).AsInteger;
      DateTime:=FieldByName(c_MDEjrn_DT).AsDateTime;
      MstId:=FieldByName(c_MDEjrn_MstdId).AsInteger;
      Mstname:=FieldByName(c_AwMst_MstName).AsString;
      Schiene:=FieldByName(c_MDEjrn_Schiene).AsInteger;
      infoNr:=FieldByName(c_MDEjrn_IcNr).AsInteger;
      next;
      result:=true
    end
  end
end;

//------------------------------------------------------------------------------
function TMDEInfoCodeTable.GetMdeInfoText(var JournalSatz:RecMdeInfoCodeJournal):boolean;
begin
  with MdeItQuery do begin
    sql.text:='select * from '+c_WCMdeInfocode;
    sql.add('where '+c_MDEic_nr+' = '+inttostr(JournalSatz.infoNr));
    open;
    if eof then
      result:=false
    else begin
      JournalSatz.infoText:=FieldByName(c_MDEic_txt).asString;
      result:=true
    end
  end
end;

//------------------------------------------------------------------------------
procedure TMDEInfoCodeTable.DeleteJournalRecord(id:integer);
begin
  with MdeJQuery do begin
    sql.text:='delete from '+c_WCMdeInfocodeJournal;
    sql.add('where '+c_MDEjrn_ID+' = '+inttostr(id));
    ExecSql;
  end
end;


//==============================================================================
//------------------------------------------------------------------------------
constructor TMGTable.Create;
begin
  inherited Create;
  MGQuery:=TQueryExt.Create(nil);
  MGQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  MGTable:=TTableExt.Create(nil);
  MGTable.DataBaseName:=PathServer.PathName[WWicalStammDb];
  MGTable.TableName:=c_WCMessgroessen;
end;

//------------------------------------------------------------------------------
destructor TMGTable.Destroy;
begin
  MGQuery.Close;
  MGQuery.Free;
  MGTable.Close;
  MGTable.Free;
end;

//------------------------------------------------------------------------------
procedure TMGTable.WriteMGListe;
begin
  if (MGTable.OpenExclusive) then with MGTable do begin
    Append;
    FieldByName(c_MG_ID).asInteger:=id;
    FieldByName(c_MG_Kennzeichen).asString:=Kz;
    FieldByName(c_MG_Bezeichnung).asString:=Bz;
    FieldByName(c_MG_Typ).asString:=Typ;
    FieldByName(c_MG_Einheit).asString:=Eh;
    FieldByName(c_MG_Kommastellen).asInteger:=Komma;
    FieldByName(c_MG_MGID_MDE).asString:=MgId;
    post;
    close;
  end
end;

//------------------------------------------------------------------------------
procedure TMGTable.OpenMGListe;
begin
  with MGQuery do begin
    Sql.Text:='select * from '+c_WCMessgroessen;
    open;
  end
end;

//------------------------------------------------------------------------------
function TMGTable.GetMGSatz(var Messgroesse : RecMessgroesse):boolean;
begin
  with MGQuery do begin
    if eof then
      result:=false
    else with Messgroesse do begin
      mgid:=fieldByName(c_MG_ID).asInteger;
      kennzeichen:=fieldByName(c_MG_Kennzeichen).asString;
      Bezeichnung:=fieldByName(c_MG_Bezeichnung).asString;
      Typ:=fieldByName(c_MG_Typ).asString[1];
      Einheit:=fieldByName(c_MG_Einheit).asString;
      Kommastellen:=fieldByName(c_MG_Kommastellen).asInteger;
      CharId:=fieldByName(c_MG_MGID_MDE).asString[1];
      next;
      result:=true;
    end
  end
end;

//------------------------------------------------------------------------------
function TMGTable.GetMGSatz(index: integer; var Messgroesse : RecMessgroesse):boolean;
begin
  with MGQuery do begin
    Sql.Text:='select * from '+c_WCMessgroessen;
    sql.add('where '+c_MG_ID+' = '+inttostr(index));
    open;
    result := not eof;
    if result then with Messgroesse do begin
      mgid:=fieldByName(c_MG_ID).asInteger;
      kennzeichen:=fieldByName(c_MG_Kennzeichen).asString;
      Bezeichnung:=fieldByName(c_MG_Bezeichnung).asString;
      Typ:=fieldByName(c_MG_Typ).asString[1];
      Einheit:=fieldByName(c_MG_Einheit).asString;
    end
  end
end;


//==============================================================================
//------------------------------------------------------------------------------
constructor TWPTable.Create;
begin
  inherited Create;
  WPQuery:=TQueryExt.Create(nil);
  WPQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  WPTable:=TTableExt.Create(nil);
  WPTable.DataBaseName:=PathServer.PathName[WWicalStammDb];
  WPTable.TableName:=c_P_WetterProgDat;
  ATQuery:=TQueryExt.Create(nil);
  ATQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  ATTable:=TTableExt.Create(nil);
  ATTable.DataBaseName:=PathServer.PathName[WWicalStammDb];
  ATTable.TableName:=c_P_WetterProgAtt;
end;

//------------------------------------------------------------------------------
destructor TWPTable.Destroy;
begin
  WPQuery.Close;
  WPQuery.Free;
  WPTable.Close;
  WPTable.Free;
  ATQuery.Close;
  ATQuery.Free;
  ATTable.Close;
  ATTable.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function TWPTable.OpenWPDatum:boolean;
begin
  with WPQuery do begin
    sql.Text:='select * from '+c_P_WetterProgDat;
    sql.Add('order by '+c_pwd_datum);
    open;
    result:=not eof;
  end
end;

//------------------------------------------------------------------------------
function TWPTable.ExistsWPDatum(Datum:TDateTime):integer;
begin
  with WPQuery do begin
    sql.Text:='select * from '+c_P_WetterProgDat;
    sql.add('where '+c_pwd_datum+' = :'+c_pwd_datum);
    paramByName(c_pwd_datum).AsDateTime:=Datum;
    open;
    if eof then
      result:=-1
    else
      result:=fieldByName(c_pwd_id).AsInteger
  end
end;

//------------------------------------------------------------------------------
function TWPTable.InsertNewWP(Datum:TDateTime):integer;
var DbAutoInc : TDbAutoInc;
begin
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  result:=DbAutoInc.NextIndex[c_P_WetterProgDat];
  if result > 0 then begin
    if (WPTable.OpenExclusive) then with WPTable do begin
      Append;
      fieldByName(c_pwd_id).AsInteger:=result;
      fieldByName(c_pwd_datum).AsDateTime:=datum;
      post;
      close;
    end
  end
end;

//------------------------------------------------------------------------------
procedure TWPTable.WriteAT(DatId,Att:integer; Wert:double);
var DbAutoInc : TDbAutoInc;
    AtId : integer;
begin
  with ATQuery do begin
    RequestLive:=true;
    sql.Text:='select *  from '+c_P_WetterProgAtt;
    sql.Add('where '+c_pwa_datid+' = '+inttostr(DatId));
    sql.Add('and '+c_pwa_att+' = '+inttostr(Att));
    if open and not eof then begin
      // Wert überschreiben
      edit;
      FieldByName(c_pwa_wert).AsFloat:=Wert;
      post;
      close;
    end
    else begin
      // Neues Attribut eintragen
      ATQuery.Close;
      DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
      AtId:=DbAutoInc.NextIndex[c_P_WetterProgAtt];
      if AtId > 0 then begin
        if (AtTable.OpenExclusive) then with AtTable do begin
          Append;
          FieldByName(c_pwa_id).AsInteger:=AtId;
          FieldByName(c_pwa_datid).AsInteger:=DatId;
          FieldByName(c_pwa_att).AsInteger:=Att;
          FieldByName(c_pwa_wert).AsFloat:=Wert;
          post;
          close;
        end
      end;
    end
  end
end;

//------------------------------------------------------------------------------
function TWPTable.ReadAT(DatId,Att:integer; var Wertf:TWerteFeld):boolean;
begin
  with ATQuery do begin
    sql.Text:='select *  from '+c_P_WetterProgAtt;
    sql.Add('where '+c_pwa_datid+' = '+inttostr(DatId));
    sql.Add('and '+c_pwa_att+' = '+inttostr(Att));
    open;
    result:=not eof;
    wertf.Valid:=result;
    if result then
      Wertf.Wert:=FieldByName(c_pwa_wert).AsFloat;
  end;
end;

//------------------------------------------------------------------------------
function TWPTable.ReadAlleAT(DatId:integer; var WP:RecWetterPrognose):boolean;
begin
  WP.LTMin.Valid:=ReadAT(DatId,c_WP_LTMin,WP.LTMin);
  WP.LTMax.Valid:=ReadAT(DatId,c_WP_LTMax,WP.LTMax);
  WP.LTMit.Valid:=ReadAT(DatId,c_WP_LTMit,WP.LTMit);
  WP.WindR.Valid:=ReadAT(DatId,c_WP_WindR,WP.WindR);
  WP.WindG.Valid:=ReadAT(DatId,c_WP_WindG,WP.WindG);
  WP.WLage.Valid:=ReadAT(DatId,c_WP_WLage,WP.WLage);
  result:=WP.LTMin.Valid or WP.LTMax.Valid or WP.LTMit.Valid or
          WP.WindR.Valid or WP.WindG.Valid or WP.WLage.Valid
end;

//------------------------------------------------------------------------------
function TWPTable.GetNextWP(var Datum:TDateTime):integer;
begin
  if not WPQuery.eof then begin
    result:=WPQuery.FieldByName(c_pwd_id).asInteger;
    Datum:=WPQuery.FieldByName(c_pwd_datum).asDateTime;
    WPQuery.next;
  end
  else
    result:=-1
end;

//==============================================================================
//------------------------------------------------------------------------------
constructor TKonvertJournalTable.Create;
begin
  inherited Create;
  KJQuery:=TQueryExt.Create(nil);
  KJQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  KJTable:=TTableExt.Create(nil);
  KJTable.DataBaseName:=PathServer.PathName[WWicalStammDb];
  KJTable.TableName:=c_KonvertJournal;
end;

//------------------------------------------------------------------------------
destructor TKonvertJournalTable.Destroy;
begin
  KJQuery.Close;
  KJQuery.Free;
  KJTable.Close;
  KJTable.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function TKonvertJournalTable.InsertNewRecord(var MyKonvertJournal:RecKonvertJournal):boolean;
var DbAutoInc : TDbAutoInc;
begin
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  MyKonvertJournal.JournalId:=DbAutoInc.NextIndex[c_KonvertJournal];
  if MyKonvertJournal.JournalId > 0 then begin
    if (KJTable.OpenExclusive) then with KJTable,MyKonvertJournal do begin
      Append;
      FieldByName(c_KJ_ID).asInteger:=JournalId;
      FieldByName(c_KJ_DatumAktion).asDateTime:=AktionsDatum;
      FieldByName(c_KJ_MstID).asInteger:=MstId;
      FieldByName(c_KJ_MstName).asString:=MstNamen;
      FieldByName(c_KJ_KanalID).asInteger:=KanalId;
      FieldByName(c_KJ_KanalName).asString:=KanalName;
      FieldByName(c_KJ_DatumAlt).asDateTime:=DatenEndeAlt;
      FieldByName(c_KJ_DatumNeu).asDateTime:=DatenEndeNeu;
      post;
      close;
      result:=true
    end
    else begin
      ShowMessage('Einfügen eines neuen Journaleintrags fehlgeschlagen!');
      result:=false;
    end
  end
  else begin
    ShowMessage('Einfügen eines neuen Journaleintrags fehlgeschlagen!');
    result:=false;
  end;
  DbAutoInc.Free;
end;

//------------------------------------------------------------------------------
procedure TKonvertJournalTable.JournalBegrenzen(max:integer);
var FirstId : integer;
begin
   with KJQuery do begin
     sql.Text:='select '+c_KJ_ID+' from '+c_KonvertJournal;
     open;
     if not eof then begin
       FirstId:=FieldByName(c_KJ_ID).AsInteger;
       if KJQuery.RecordCount > max then begin
         FirstId:=FirstId+(KJQuery.RecordCount-max)+(max div 10);
         sql.Text:='delete  from '+c_KonvertJournal;
         sql.add('where '+c_KJ_ID+' < '+inttostr(FirstId));
         ExecSql;
       end
     end;
     close
  end;
end;

//------------------------------------------------------------------------------
function TKonvertJournalTable.OpenJournal:integer;
begin
  with KJQuery do begin
    sql.Text:='select * from '+c_KonvertJournal;
    sql.Add('order by '+c_KJ_DatumAktion);
    try
      open;
      if not eof then
        result:=recordCount
      else
        result:=0;
    except
      result:=-1
    end
  end
end;

//------------------------------------------------------------------------------
function TKonvertJournalTable.GetEintrag(var MyKonvertJournal:RecKonvertJournal):boolean;
begin
  with KJQuery do begin
    if not eof then with MyKonvertJournal do begin
      JournalId:=FieldByName(c_KJ_ID).asInteger;
      AktionsDatum:=FieldByName(c_KJ_DatumAktion).asDateTime;
      MstNamen:=FieldByName(c_KJ_MstName).asString;
      KanalName:=FieldByName(c_KJ_KanalName).asString;
      KanalId:=FieldByName(c_KJ_KanalID).asInteger;
      DatenEndeAlt:=FieldByName(c_KJ_DatumAlt).asDateTime;
      DatenEndeNeu:=FieldByName(c_KJ_DatumNeu).asDateTime;
      next;
      result:=true;
    end
    else
      result:=false;
  end
end;

//==============================================================================
//------------------------------------------------------------------------------
constructor TAGTable.Create;
begin
  inherited Create;
  AGQuery:=TQuery.Create(nil);
  AGQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  AGTable:=TTableExt.Create(nil);
  AGTable.DataBaseName:=PathServer.PathName[WWicalStammDb];
  AGTable.TableName:=c_P_AG;
end;

//------------------------------------------------------------------------------
destructor TAGTable.Destroy;
begin
  AGQuery.Close;
  AGQuery.Free;
  AGTable.Close;
  AGTable.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function TAGTable.OpenAGTable(ProgId:integer):boolean;
begin
  with AGQuery do begin
    sql.text:='select * from '+c_P_AG;
    sql.Add('where '+c_pag_progid+' = '+intToStr(ProgId));
//    sql.Add('order by '+c_pag_name);
    open;
    result:= not eof
  end
end;

//------------------------------------------------------------------------------
function TAGTable.InsertNewRecord(var Abschaltgruppe:RecAbschaltgruppe):boolean;
var DbAutoInc : TDbAutoInc;
    IStatus   : integer;
begin
  result:=false;
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  Abschaltgruppe.agid:=DbAutoInc.NextIndex[c_P_AG];
  if Abschaltgruppe.agid > 0 then begin
    if (AgTable.OpenExclusive) then with AGTable,Abschaltgruppe do begin
      Append;
      FieldByName(c_pag_id).AsInteger:=agid;
      FieldByName(c_pag_progid).AsInteger:=progid;
      FieldByName(c_pag_name).AsString:=agname;
      FieldByName(c_pag_modus).AsInteger:=modus;
      if aktiv then
        iStatus:=1
      else
        iStatus:=0;
      if EinAus then
        iStatus:=iStatus or 2;
      FieldByName(c_pag_status).AsInteger:=iStatus;
      FieldByName(c_pag_tl1).AsFloat:=tl[1];
      FieldByName(c_pag_tl2).AsFloat:=tl[2];
      FieldByName(c_pag_tl3).AsFloat:=tl[3];
      FieldByName(c_pag_tl4).AsFloat:=tl[4];
      FieldByName(c_pag_tl5).AsFloat:=tl[5];
      FieldByName(c_pag_tl6).AsFloat:=tl[6];
      FieldByName(c_pag_tl7).AsFloat:=tl[7];
      FieldByName(c_pag_tl8).AsFloat:=tl[8];
      post;
      close;
      result:=true
    end
    else begin
      ShowMessage('Einfügen eines neuen Archivgruppeneintrags fehlgeschlagen!');
      result:=false;
    end
  end
end;

//------------------------------------------------------------------------------
function TAGTable.WriteAbschaltgruppe(var Abschaltgruppe:RecAbschaltgruppe):boolean;
var iStatus : integer;
begin
  with AGQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_P_AG;
    sql.add('where '+c_pag_id+' = '+inttostr(Abschaltgruppe.agid));
    open;
    edit;
    if eof then
      result:=false
    else with Abschaltgruppe do begin
      FieldByName(c_pag_progid).AsInteger:=progid;
      FieldByName(c_pag_name).AsString:=agname;
      FieldByName(c_pag_modus).AsInteger:=modus;
      if aktiv then
        iStatus:=1
      else
        iStatus:=0;
      if EinAus then
        iStatus:=iStatus or 2;
      FieldByName(c_pag_status).AsInteger:=iStatus;
      FieldByName(c_pag_tl1).AsFloat:=tl[1];
      FieldByName(c_pag_tl2).AsFloat:=tl[2];
      FieldByName(c_pag_tl3).AsFloat:=tl[3];
      FieldByName(c_pag_tl4).AsFloat:=tl[4];
      FieldByName(c_pag_tl5).AsFloat:=tl[5];
      FieldByName(c_pag_tl6).AsFloat:=tl[6];
      FieldByName(c_pag_tl7).AsFloat:=tl[7];
      FieldByName(c_pag_tl8).AsFloat:=tl[8];
      post;
      close;
      result:=true;
    end
  end
end;

//------------------------------------------------------------------------------
procedure TAGTable.ReadAG(var Abschaltgruppe:RecAbschaltgruppe);
var iStatus : integer;
begin
  with AGQuery,Abschaltgruppe do begin
    agid:=FieldByName(c_pag_id).AsInteger;
    progid:=FieldByName(c_pag_progid).AsInteger;
    agname:=FieldByName(c_pag_name).AsString;
    modus:=FieldByName(c_pag_modus).AsInteger;
    iStatus:=FieldByName(c_pag_Status).AsInteger;
    Aktiv:=iStatus and 1 = 1;
    EinAus:=iStatus and 2 = 2;
    tl[1]:=FieldByName(c_pag_tl1).AsFloat;
    tl[2]:=FieldByName(c_pag_tl2).AsFloat;
    tl[3]:=FieldByName(c_pag_tl3).AsFloat;
    tl[4]:=FieldByName(c_pag_tl4).AsFloat;
    tl[5]:=FieldByName(c_pag_tl5).AsFloat;
    tl[6]:=FieldByName(c_pag_tl6).AsFloat;
    tl[7]:=FieldByName(c_pag_tl7).AsFloat;
    tl[8]:=FieldByName(c_pag_tl8).AsFloat;
  end
end;

//------------------------------------------------------------------------------
function TAGTable.GetNextAG(var Abschaltgruppe:RecAbschaltgruppe):boolean;
begin
  result:= not AGQuery.eof;
  if result then begin
    ReadAG(Abschaltgruppe);
    AGQuery.next;
  end
end;

//==============================================================================
//------------------------------------------------------------------------------
constructor TPrognoseTable.Create;
begin
  inherited Create;
  ProgAttQuery:=TQuery.Create(nil);
  ProgAttQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  ProgAttTable:=TTableExt.Create(nil);
  ProgAttTable.DataBaseName:=PathServer.PathName[WWicalStammDb];
  ProgAttTable.TableName:=c_P_PrognoseAttribute;
end;

//------------------------------------------------------------------------------
destructor TPrognoseTable.Destroy;
begin
  ProgAttQuery.Close;
  ProgAttQuery.Free;
  ProgAttTable.Close;
  ProgAttTable.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function TPrognoseTable.InsertAttribut(Id,Attribut:integer; WertVon,WertBis:integer):boolean;
var DbAutoInc : TDbAutoInc;
    LastId    : integer;
begin
  with ProgAttQuery do begin
    requestLive:=true;
    sql.Text:='delete from '+c_P_PrognoseAttribute;
    sql.add('where '+c_PAtt_Owner+' = '+inttostr(Atyp));
    sql.add('and '+c_PAtt_OwnerId+' = '+inttostr(Id));
    sql.add('and '+c_PAtt_Attribut+' = '+inttostr(Attribut));
    ExecSql;
  end;
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  LastId:=DbAutoInc.NextIndex[c_P_PrognoseAttribute];
  if LastID > 0 then begin
    if (ProgAttTable.OpenExclusive) then with ProgAttTable do begin
      Append;
      FieldByName(c_PAtt_ID).asInteger:=LastID;
      FieldByName(c_PAtt_Owner).asInteger:=Atyp;
      FieldByName(c_PAtt_OwnerID).asInteger:=Id;
      FieldByName(c_PAtt_Attribut).asInteger:=Attribut;
      FieldByName(c_PAtt_WertVon).asInteger:=WertVon;
      FieldByName(c_PAtt_WertBis).asInteger:=WertBis;
      post;
      close;
      result:=true
    end
    else begin
      ShowMessage('Einfügen eines neuen Attributs fehlgeschlagen!');
      result:=false;
    end
  end
  else begin
    ShowMessage('Einfügen einer neuen Attributs fehlgeschlagen!');
    result:=false;
  end;
  DbAutoInc.Free;
end;


//------------------------------------------------------------------------------
function TPrognoseTable.GetAttribute(Id,Attribut:integer):boolean;
var x,y : integer;
begin
  result:=GetAttribute(Id,Attribut,x,y);
  result:= x > 0
end;

//------------------------------------------------------------------------------
function TPrognoseTable.GetAttribute(Id,Attribut:integer; var Wert1:integer):boolean;
var x : integer;
begin
  result:=GetAttribute(Id,Attribut,wert1,x)
end;

//------------------------------------------------------------------------------
function TPrognoseTable.GetAttribute(Id,Attribut:integer; var Wert1,Wert2:integer):boolean;
begin
  with ProgAttQuery do begin
    requestLive:=true;
    sql.text:='select * from '+c_P_PrognoseAttribute;
    sql.add('where '+c_PAtt_OwnerID+' = '+intToStr(Id));
    sql.add('and '+c_PAtt_Owner+' = '+inttoStr(Atyp));
    sql.add('and '+c_PAtt_Attribut+' = '+inttoStr(Attribut));
    open;
    if eof then begin
      wert1:=0;
      wert2:=0;
      result:=false
    end
    else begin
      Wert1:=FieldByName(c_PAtt_WertVon).asInteger;
      Wert2:=FieldByName(c_PAtt_WertBis).asInteger;
      result:=true;
    end
  end
end;

//------------------------------------------------------------------------------
procedure TPrognoseTable.GetAttribute(Id:integer; var Attribute:RecAttribute);
var wert1,wert2 : integer;
begin
  if GetAttribute(id,c_Attribut_WTModus,wert1) then
    Attribute.WtModus:=Wert1
  else
    Attribute.WtModus:=0;
  if GetAttribute(id,c_Attribut_Zeitbereich,wert1,wert2) then begin
   Attribute.DFrom:=Wert1;
   Attribute.DTo:=Wert2;
  end
  else begin
   Attribute.DFrom:=0;
   Attribute.DTo:=0;
  end;
  if GetAttribute(id,c_Attribut_MiTemp,wert1,wert2) then begin
    Attribute.IsMiTemp:=true;
    Attribute.MiTempVon:=Wert1;
    Attribute.MiTempBis:=Wert2;
  end
  else begin
    Attribute.IsMiTemp:=false;
    Attribute.MiTempVon:=0;
    Attribute.MiTempBis:=0;
  end
end;

//==============================================================================
//------------------------------------------------------------------------------
constructor TProgRegionTable.Create;
begin
  inherited Create;
  Atyp:=c_PAttOwner_PROG;
  ProgQuery:=TQuery.Create(nil);
  ProgQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  ProgTable:=TTableExt.Create(nil);
  ProgTable.DataBaseName:=PathServer.PathName[WWicalStammDb];
  ProgTable.TableName:=c_P_Prognose;
end;

//------------------------------------------------------------------------------
destructor TProgRegionTable.Destroy;
begin
  ProgQuery.Close;
  ProgQuery.Free;
  ProgTable.Close;
  ProgTable.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function TProgRegionTable.OpenPrognoseTable:boolean;
begin
  with ProgQuery do begin
    sql.text:='select * from '+c_P_Prognose;
    sql.Add('order by '+c_P_Name);
    open;
    result:= not eof
  end
end;

//------------------------------------------------------------------------------
procedure TProgRegionTable.ReadSelectPrognose(var Prognose:RecPrognose);
begin
  with Prognose,ProgQuery do begin
    ProgId:=FieldByName(c_P_ID).asInteger;
    Prognose.ProgName:=FieldByName(c_P_Name).asString;
    Prognose.VKanalId:=FieldByName(c_P_VerbrauchId).asInteger;
    Prognose.TKanalId:=FieldByName(c_P_TemperaturId).asInteger;
    Prognose.SKanalId:=FieldByName(c_P_SpeicherId).asInteger;
    Prognose.Vmax:=FieldByName(c_P_Vmax).asFloat;
    Prognose.Vmin:=FieldByName(c_P_Vmin).asFloat;
    Prognose.Tmax:=FieldByName(c_P_Tmax).asFloat;
    Prognose.Tmin:=FieldByName(c_P_Tmin).asFloat;
    Prognose.TagMax:=FieldByName(c_P_TagMax).asFloat;
  end;
end;

//------------------------------------------------------------------------------
function TProgRegionTable.GetNextPrognose(var Prognose:RecPrognose):boolean;
begin
  result:= not ProgQuery.eof;
  if result then begin
    ReadSelectPrognose(Prognose);
    ProgQuery.next;
  end
end;

//------------------------------------------------------------------------------
function TProgRegionTable.InsertNewPrognose(var Prognose:RecPrognose):boolean;
var DbAutoInc : TDbAutoInc;
begin
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  Prognose.ProgId:=DbAutoInc.NextIndex[c_P_Prognose];
  if Prognose.ProgId > 0 then begin
    if (ProgTable.OpenExclusive) then with ProgTable,Prognose do begin
      Append;
      FieldByName(c_P_ID).asInteger:=ProgId;
      FieldByName(c_P_Name).asString:=ProgName;
      FieldByName(c_P_VerbrauchId).asInteger:=VKanalId;
      FieldByName(c_P_TemperaturId).asInteger:=TKanalId;
      FieldByName(c_P_SpeicherId).asInteger:=SKanalId;
      FieldByName(c_P_Vmax).asFloat:=Vmax;
      FieldByName(c_P_Vmin).asFloat:=Vmin;
      FieldByName(c_P_Tmax).asFloat:=Tmax;
      FieldByName(c_P_Tmin).asFloat:=Tmin;
      FieldByName(c_P_Tagmax).asFloat:=TagMax;
      post;
      close;
      result:=true
    end
    else begin
      ShowMessage('Einfügen einer neuen Prognose fehlgeschlagen!');
      result:=false;
    end
  end
  else begin
    ShowMessage('Einfügen einer neuen Prognose fehlgeschlagen!');
    result:=false;
  end;
  DbAutoInc.Free;
end;

//------------------------------------------------------------------------------
function TProgRegionTable.WritePrognose(var Prognose:RecPrognose):boolean;
begin
  with ProgQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_P_Prognose;
    sql.add('where '+c_P_ID+' = '+inttostr(Prognose.ProgId));
    open;
    edit;
    if eof then
      result:=false
    else with Prognose do begin
      FieldByName(c_P_Name).asString:=ProgName;
      FieldByName(c_P_VerbrauchId).asInteger:=VKanalId;
      FieldByName(c_P_TemperaturId).asInteger:=TKanalId;
      FieldByName(c_P_SpeicherId).asInteger:=SKanalId;
      FieldByName(c_P_Vmax).asFloat:=Vmax;
      FieldByName(c_P_Vmin).asFloat:=Vmin;
      FieldByName(c_P_Tmax).asFloat:=Tmax;
      FieldByName(c_P_Tmin).asFloat:=Tmin;
      FieldByName(c_P_Tagmax).asFloat:=TagMax;
      post;
      close;
      result:=true;
    end
  end
end;

//------------------------------------------------------------------------------
function TProgRegionTable.WritePrognoseName(ProgId:integer;ProgName:string):boolean;
begin
  with ProgQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_P_Prognose;
    sql.add('where '+c_P_ID+' = '+inttostr(ProgId));
    open;
    edit;
    if eof then
      result:=false
    else begin
      FieldByName(c_P_Name).asString:=ProgName;
      post;
      close;
      result:=true;
    end
  end
end;


//------------------------------------------------------------------------------
function TProgRegionTable.ReadPrognose(var Prognose:RecPrognose):boolean;
begin
  with ProgQuery do begin
    sql.Text:='select * from '+c_P_Prognose;
    sql.add('where '+c_P_ID+' = '+inttostr(Prognose.ProgId));
    open;
    if eof then
      result:=false
    else begin
      ReadSelectPrognose(Prognose);
      close;
      result:=true;
    end
  end
end;

//------------------------------------------------------------------------------
Procedure TProgRegionTable.LoeschePrognose(ProgId:integer);
begin
  with ProgQuery do begin

    sql.Text:='select * from '+c_P_Regressionsgerade;
    sql.add('where '+C_PReg_ProgID+' = '+inttostr(ProgId));
    open;
    while not eof do begin
      // Attribute zu den Reg-Geraden löschen
      ProgAttQuery.sql.Text:='delete from '+c_P_PrognoseAttribute;
      ProgAttQuery.sql.add('where '+c_PAtt_OwnerID+' = '+inttostr(FieldByName(C_PReg_ID).asInteger));
      ProgAttQuery.sql.add('and '+c_PAtt_Owner+' = '+inttostr(c_PAttOwner_REG));
      ProgAttQuery.execsql;
      next;
    end;
    // Die zur Prognose gehörenden Reg-Geraden löschen
    sql.Text:='delete from '+c_P_Regressionsgerade;
    sql.add('where '+C_PReg_ProgID+' = '+inttostr(ProgId));
    execsql;

    sql.Text:='select * from '+c_P_TGL;
    sql.add('where '+C_PTGL_ProgID+' = '+inttostr(ProgId));
    open;
    while not eof do begin
      // Attribute zu den TGL-Geraden löschen
      ProgAttQuery.sql.Text:='delete from '+c_P_PrognoseAttribute;
      ProgAttQuery.sql.add('where '+c_PAtt_OwnerID+' = '+inttostr(FieldByName(c_PTGL_ID).asInteger));
      ProgAttQuery.sql.add('and '+c_PAtt_Owner+' = '+inttostr(c_PAttOwner_TGL));
      ProgAttQuery.execsql;
      next;
    end;
    // Die zur Prognose gehörenden TGL-Geraden löschen
    sql.Text:='delete from '+c_P_TGL;
    sql.add('where '+C_PTGL_ProgID+' = '+inttostr(ProgId));
    execsql;
    
    // Attribute der Prognose löschen
    ProgAttQuery.sql.Text:='delete from '+c_P_PrognoseAttribute;
    ProgAttQuery.sql.add('where '+c_PAtt_OwnerID+' = '+inttostr(ProgId));
    ProgAttQuery.sql.add('and '+c_PAtt_Owner+' = '+inttostr(c_PAttOwner_PROG));
    ProgAttQuery.execsql;
    // Prognose selbst löschen
    ProgAttQuery.sql.Text:='delete from '+c_P_Prognose;
    ProgAttQuery.sql.add('where '+c_P_ID+' = '+inttostr(ProgId));
    ProgAttQuery.execsql;

  end;
end;

//==============================================================================
//------------------------------------------------------------------------------
constructor TPTglTable.Create;
begin
  inherited Create;
  Atyp:=c_PAttOwner_TGL;
  TGLQuery:=TQuery.Create(nil);
  TGLQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  TGLTable:=TTableExt.Create(nil);
  TGLTable.DataBaseName:=PathServer.PathName[WWicalStammDb];
  TGLTable.TableName:=c_P_TGL;
end;

//------------------------------------------------------------------------------
destructor TPTglTable.Destroy;
begin
  TGLQuery.Close;
  TGLQuery.Free;
  TGLTable.Close;
  TGLTable.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function TPTglTable.InsertNewTGL(var TGL:RecTagesGanglinie):boolean;
var DbAutoInc : TDbAutoInc;
begin
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  TGL.Id:=DbAutoInc.NextIndex[c_P_TGL];
  if TGL.Id > 0 then begin
    if (TGLTable.OpenExclusive) then with TGLTable,TGL do begin
      Append;
      FieldByName(c_PTGL_ID).asInteger:=TGL.Id;
      FieldByName(C_PTGL_ProgID).asInteger:=ProgID;
      FieldByName(C_PTGL_Name).asString:=TGLName;
      FieldByName(C_PTGL_Klasse).asInteger:=Klasse;
      FieldByName(c_PTGL_Standard).asInteger:=Standard;
      FieldByName(c_PTGL_AnzTage).asinteger:=anztage;
      FieldByName(c_PTGL_W00).asFloat:=SWert[0];
      FieldByName(c_PTGL_W01).asFloat:=SWert[1];
      FieldByName(c_PTGL_W02).asFloat:=SWert[2];
      FieldByName(c_PTGL_W03).asFloat:=SWert[3];
      FieldByName(c_PTGL_W04).asFloat:=SWert[4];
      FieldByName(c_PTGL_W05).asFloat:=SWert[5];
      FieldByName(c_PTGL_W06).asFloat:=SWert[6];
      FieldByName(c_PTGL_W07).asFloat:=SWert[7];
      FieldByName(c_PTGL_W08).asFloat:=SWert[8];
      FieldByName(c_PTGL_W09).asFloat:=SWert[9];
      FieldByName(c_PTGL_W10).asFloat:=SWert[10];
      FieldByName(c_PTGL_W11).asFloat:=SWert[11];
      FieldByName(c_PTGL_W12).asFloat:=SWert[12];
      FieldByName(c_PTGL_W13).asFloat:=SWert[13];
      FieldByName(c_PTGL_W14).asFloat:=SWert[14];
      FieldByName(c_PTGL_W15).asFloat:=SWert[15];
      FieldByName(c_PTGL_W16).asFloat:=SWert[16];
      FieldByName(c_PTGL_W17).asFloat:=SWert[17];
      FieldByName(c_PTGL_W18).asFloat:=SWert[18];
      FieldByName(c_PTGL_W19).asFloat:=SWert[19];
      FieldByName(c_PTGL_W20).asFloat:=SWert[20];
      FieldByName(c_PTGL_W21).asFloat:=SWert[21];
      FieldByName(c_PTGL_W22).asFloat:=SWert[22];
      FieldByName(c_PTGL_W23).asFloat:=SWert[23];
      post;
      close;
      result:=true
    end
    else begin
      ShowMessage('Einfügen einer neuen Tagesganglinie fehlgeschlagen!');
      result:=false;
    end
  end
  else begin
    ShowMessage('Einfügen einer neuen Tagesganglinie fehlgeschlagen!');
    result:=false;
  end;
  DbAutoInc.Free;
end;

//------------------------------------------------------------------------------
function TPTglTable.OpenTGLTable(ProgId:integer):boolean;
begin
  with TGLQuery do begin
    sql.text:='select * from '+c_P_TGL;
    sql.Add('where '+C_PTGL_ProgID+' = '+inttostr(ProgId));
    sql.Add('order by '+C_PTGL_Name);
    open;
    result:= not eof
  end
end;

//------------------------------------------------------------------------------
function TPTglTable.OpenTGLTableSortWT(ProgId:integer):boolean;
begin
  with TGLQuery do begin
    sql.text:='select * from '+c_P_TGL+','+c_P_PrognoseAttribute;
    sql.Add('where '+c_P_TGL+'.'+c_PTGL_ID+' = '+c_P_PrognoseAttribute+'.'+c_PAtt_OwnerID);
    sql.Add('and '+C_PTGL_ProgID+' = '+inttostr(ProgId));
    sql.Add('and '+c_PAtt_Owner+' = '+inttostr(c_PAttOwner_TGL));
    sql.Add('and '+c_PAtt_Attribut+' = '+inttostr(c_Attribut_WTModus));
    sql.Add('order by '+c_PAtt_WertVon);
    open;
    result:= not eof
  end
end;

//------------------------------------------------------------------------------
function TPTglTable.GetNextTGL(var TGL:RecTagesGanglinie):boolean;
begin
  result:= not TGLQuery.eof;
  if result then begin
    ReadSelectTGL(TGL);
    TGLQuery.next;
  end
end;

//------------------------------------------------------------------------------
function TPTglTable.ExistsTGLWt(ProgId,Klasse,Wt:integer):integer;
begin
  with TGLQuery do begin
    OpenTGLTableSortWT(ProgId);
    while not eof do begin
      if (Wt = FieldByName(c_PAtt_WertVon).asInteger) and
         (Klasse =  FieldByName(C_PTGL_Klasse).asInteger) then begin
        result:=FieldByName(c_PTGL_ID).asInteger;
        exit
      end;
      next
    end;
    result:=-1;
    close
  end
end;

//------------------------------------------------------------------------------
function TPTglTable.ExistsTGLName(ProgId,Klasse:integer;TGLName:string):integer;
begin
  with TGLQuery do begin
    sql.Text:='select * from '+c_P_TGL;
    sql.Add('where '+c_PTGL_ProgID+' = '+inttostr(ProgId));
    open;
    while not eof do begin
      if (TGLName = FieldByName(C_PTGL_Name).asString) and
         (Klasse =  FieldByName(C_PTGL_Klasse).asInteger) then begin
        result:=FieldByName(c_PTGL_ID).asInteger;
        exit
      end;
      next
    end;
    result:=-1;
    close
  end
end;

//------------------------------------------------------------------------------
procedure TPTglTable.LoescheTGL(TGLId:integer);
begin
  with TGLQuery do begin
    RequestLive:=true;
    sql.Text:='delete from '+c_P_TGL;
    sql.add('where '+c_PTGL_ID+' = '+inttostr(TGLId));
    execsql;
  end;
  with ProgAttQuery do begin
    RequestLive:=true;
    sql.Text:='delete from '+c_P_PrognoseAttribute;
    sql.add('where '+c_PAtt_Owner+' = '+inttostr(ATyp));
    sql.add('and '+c_PAtt_OwnerID+' = '+inttostr(TGLId));
    execsql;
  end
end;

//------------------------------------------------------------------------------
procedure TPTglTable.LoescheAlleTGL(progId,Klasse:integer);
begin
  with TglQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_P_TGL;
    sql.add('where '+c_PTgl_Klasse+' = '+inttostr(Klasse));
    sql.add('and '+c_PTGL_ProgID+' = '+inttostr(progid));
    open;
    while not eof do begin
      with ProgAttQuery do begin
        RequestLive:=true;
        sql.Text:='delete from '+c_P_PrognoseAttribute;
        sql.add('where '+c_PAtt_Owner+' = '+inttostr(ATyp));
        sql.add('and '+c_PAtt_OwnerID+' = '+inttostr(TglQuery.fieldbyName(C_PTgl_ID).AsInteger));
        execsql;
      end;
      next
    end;
    sql.Text:='delete from '+c_P_TGL;
    sql.add('where '+c_PTgl_Klasse+' = '+inttostr(Klasse));
    sql.add('and '+c_PTGL_ProgID+' = '+inttostr(progid));
    execsql;
  end
end;

//------------------------------------------------------------------------------
function TPTglTable.WriteTGLName(TGLId:integer;TGLName:string):boolean;
begin
  with TGLQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_P_TGL;
    sql.add('where '+C_PTGL_ID+' = '+inttostr(TGLId));
    open;
    edit;
    if eof then
      result:=false
    else begin
      FieldByName(C_PTGL_Name).asString:=TGLName;
      post;
      close;
      result:=true;
    end
  end
end;


//------------------------------------------------------------------------------
procedure TPTglTable.ReadSelectTGL(var TGL:RecTagesGanglinie);
begin
  with TGL,TGLQuery do begin
      TGL.Id:=FieldByName(c_PTGL_ID).asInteger;
      ProgID:=FieldByName(C_PTGL_ProgID).asInteger;
      TGLName:=FieldByName(C_PTGL_Name).asString;
      Klasse:=FieldByName(C_PTGL_Klasse).asInteger;
      Standard:=FieldByName(c_PTGL_Standard).asInteger;
      anztage:=FieldByName(c_PTGL_AnzTage).asinteger;
      SWert[0]:=FieldByName(c_PTGL_W00).asFloat;
      SWert[1]:=FieldByName(c_PTGL_W01).asFloat;
      SWert[2]:=FieldByName(c_PTGL_W02).asFloat;
      SWert[3]:=FieldByName(c_PTGL_W03).asFloat;
      SWert[4]:=FieldByName(c_PTGL_W04).asFloat;
      SWert[5]:=FieldByName(c_PTGL_W05).asFloat;
      SWert[6]:=FieldByName(c_PTGL_W06).asFloat;
      SWert[7]:=FieldByName(c_PTGL_W07).asFloat;
      SWert[8]:=FieldByName(c_PTGL_W08).asFloat;
      SWert[9]:=FieldByName(c_PTGL_W09).asFloat;
      SWert[10]:=FieldByName(c_PTGL_W10).asFloat;
      SWert[11]:=FieldByName(c_PTGL_W11).asFloat;
      SWert[12]:=FieldByName(c_PTGL_W12).asFloat;
      SWert[13]:=FieldByName(c_PTGL_W13).asFloat;
      SWert[14]:=FieldByName(c_PTGL_W14).asFloat;
      SWert[15]:=FieldByName(c_PTGL_W15).asFloat;
      SWert[16]:=FieldByName(c_PTGL_W16).asFloat;
      SWert[17]:=FieldByName(c_PTGL_W17).asFloat;
      SWert[18]:=FieldByName(c_PTGL_W18).asFloat;
      SWert[19]:=FieldByName(c_PTGL_W19).asFloat;
      SWert[20]:=FieldByName(c_PTGL_W20).asFloat;
      SWert[21]:=FieldByName(c_PTGL_W21).asFloat;
      SWert[22]:=FieldByName(c_PTGL_W22).asFloat;
      SWert[23]:=FieldByName(c_PTGL_W23).asFloat;
  end
end;

//==============================================================================
//------------------------------------------------------------------------------
constructor TPRegressTable.Create;
begin
  inherited Create;
  Atyp:=c_PAttOwner_REG;
  RegQuery:=TQuery.Create(nil);
  RegQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  RegTable:=TTableExt.Create(nil);
  RegTable.DataBaseName:=PathServer.PathName[WWicalStammDb];
  RegTable.TableName:=c_P_Regressionsgerade;
end;

//------------------------------------------------------------------------------
destructor TPRegressTable.Destroy;
begin
  RegQuery.Close;
  RegQuery.Free;
  RegTable.Close;
  RegTable.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function TPRegressTable.OpenRegTable(ProgId:integer):boolean;
begin
  with RegQuery do begin
    sql.text:='select * from '+c_P_Regressionsgerade;
    sql.Add('where '+C_PReg_ProgID+' = '+inttostr(ProgId));
    sql.Add('order by '+C_PReg_Name);
    open;
    result:= not eof
  end
end;

//------------------------------------------------------------------------------
function TPRegressTable.OpenRegTableSortWT(ProgId:integer):boolean;
begin
  with RegQuery do begin
    sql.text:='select * from '+c_P_Regressionsgerade+','+c_P_PrognoseAttribute;
    sql.Add('where '+c_P_Regressionsgerade+'.'+C_PReg_ID+' = '+c_P_PrognoseAttribute+'.'+c_PAtt_OwnerID);
    sql.Add('and '+C_PReg_ProgID+' = '+inttostr(ProgId));
    sql.Add('and '+c_PAtt_Owner+' = '+inttostr(c_PAttOwner_REG));
    sql.Add('and '+c_PAtt_Attribut+' = '+inttostr(c_Attribut_WTModus));
    sql.Add('order by '+c_PAtt_WertVon);
    open;
    result:= not eof
  end
end;

//------------------------------------------------------------------------------
procedure TPRegressTable.ReadSelectRegress(var Reggerade:RecRegressionsgerade);
begin
  with Reggerade,RegQuery do begin
    id:=FieldByName(C_PReg_ID).asInteger;
    ProgId:=FieldByName(C_PReg_ProgID).asInteger;
    RegName:=FieldByName(C_PReg_Name).asString;
    tm:=FieldByName(C_PReg_tm).asFloat;
    vm:=FieldByName(C_PReg_vm).asFloat;
    steigung:=FieldByName(C_PReg_steigung).asFloat;
    Klasse:=FieldByName(c_PReg_Klasse).asInteger;
    Standard:=FieldByName(c_PReg_Standard).asInteger;
    AnzTage:=FieldByName(c_PReg_AnzTage).asInteger;
  end;
end;

//------------------------------------------------------------------------------
function TPRegressTable.GetNextRegress(var Reggerade:RecRegressionsgerade):boolean;
begin
  result:= not RegQuery.eof;
  if result then begin
    ReadSelectRegress(RegGerade);
    RegQuery.next;
  end
end;

//------------------------------------------------------------------------------
function TPRegressTable.InsertNewRegress(var Reggerade:RecRegressionsgerade):boolean;
var DbAutoInc : TDbAutoInc;
begin
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  Reggerade.Id:=DbAutoInc.NextIndex[c_P_Regressionsgerade];
  if Reggerade.Id > 0 then begin
    if (RegTable.OpenExclusive) then with RegTable,Reggerade do begin
      Append;
      FieldByName(C_PReg_ID).asInteger:=Reggerade.Id;
      FieldByName(C_PReg_ProgID).asInteger:=ProgID;
      FieldByName(C_PReg_Name).asString:=RegName;
      FieldByName(C_PReg_tm).asFloat:=tm;
      FieldByName(C_PReg_vm).asFloat:=vm;
      FieldByName(C_PReg_steigung).asFloat:=steigung;
      FieldByName(c_PReg_Klasse).asInteger:=Klasse;
      FieldByName(c_PReg_Standard).asInteger:=Standard;
      FieldByName(c_PReg_AnzTage).asinteger:=anztage;
      post;
      close;
      result:=true
    end
    else begin
      ShowMessage('Einfügen einer neuen Regressionsgeraden fehlgeschlagen!');
      result:=false;
    end
  end
  else begin
    ShowMessage('Einfügen einer neuen Regressionsgeraden fehlgeschlagen!');
    result:=false;
  end;
  DbAutoInc.Free;
end;

//------------------------------------------------------------------------------
procedure TPRegressTable.LoescheReg(RegId:integer);
begin
  with RegQuery do begin
    RequestLive:=true;
    sql.Text:='delete from '+c_P_Regressionsgerade;
    sql.add('where '+C_PReg_ID+' = '+inttostr(RegId));
    execsql;
  end;
  with ProgAttQuery do begin
    RequestLive:=true;
    sql.Text:='delete from '+c_P_PrognoseAttribute;
    sql.add('where '+c_PAtt_Owner+' = '+inttostr(ATyp));
    sql.add('and '+c_PAtt_OwnerID+' = '+inttostr(RegId));
    execsql;
  end
end;

//------------------------------------------------------------------------------
procedure TPRegressTable.LoescheAlleReg(ProgId,Klasse:integer);
begin
  with RegQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_P_Regressionsgerade;
    sql.add('where '+c_PReg_Klasse+' = '+inttostr(Klasse));
    sql.add('and '+C_PReg_ProgID+' = '+inttostr(ProgId));
    open;
    while not eof do begin
      with ProgAttQuery do begin
        RequestLive:=true;
        sql.Text:='delete from '+c_P_PrognoseAttribute;
        sql.add('where '+c_PAtt_Owner+' = '+inttostr(ATyp));
        sql.add('and '+c_PAtt_OwnerID+' = '+inttostr(RegQuery.fieldbyName(C_PReg_ID).AsInteger));
        execsql;
      end;
      next
    end;
    sql.Text:='delete from '+c_P_Regressionsgerade;
    sql.add('where '+c_PReg_Klasse+' = '+inttostr(Klasse));
    sql.add('and '+C_PReg_ProgID+' = '+inttostr(ProgId));
    execsql;
  end;
end;

//------------------------------------------------------------------------------
function TPRegressTable.ExistsRegName(ProgId,Klasse:integer;RegName:string):integer;
begin
  with RegQuery do begin
    result:=-1;
    sql.Text:='select * from '+c_P_Regressionsgerade;
    sql.Add('where '+C_PReg_ProgID+' = '+inttostr(ProgId));
    open;
    while not eof do begin
      if (RegName = FieldByName(C_PReg_Name).asString) and
         (Klasse =  FieldByName(C_PREG_Klasse).asInteger) then begin
        result:=FieldByName(C_PReg_ID).AsInteger;
        exit
      end;
      next
    end;
    close
  end
end;

//------------------------------------------------------------------------------
function TPRegressTable.ExistsRegWt(ProgId,Klasse,Wt:integer):integer;
begin
  with RegQuery do begin
    OpenRegTableSortWT(ProgId);
    while not eof do begin
      if (Wt = FieldByName(c_PAtt_WertVon).asInteger) and
         (Klasse =  FieldByName(C_PReg_Klasse).asInteger) then begin
        result:=FieldByName(c_PReg_ID).asInteger;
        exit
      end;
      next
    end;
    result:=-1;
    close
  end
end;

//------------------------------------------------------------------------------
function TPRegressTable.WriteRegName(RegId:integer;RegName:string):boolean;
begin
  with RegQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_P_Regressionsgerade;
    sql.add('where '+C_PReg_ID+' = '+inttostr(RegId));
    open;
    edit;
    if eof then
      result:=false
    else begin
      FieldByName(C_PReg_Name).asString:=RegName;
      post;
      close;
      result:=true;
    end
  end
end;

//------------------------------------------------------------------------------
procedure TPRegressTable.CloseRegAttTable;
begin
  ProgAttQuery.close
end;

//==============================================================================
//------------------------------------------------------------------------------
constructor TObjClassesTable.Create(CT:integer);
begin
  inherited Create;
  ClassTyp:=CT;
  ClassQuery:=TQuery.Create(nil);
  ClassQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  ClassTable:=TTableExt.Create(nil);
  ClassTable.DataBaseName:=PathServer.PathName[WWicalStammDb];
  ClassTable.TableName:=c_WCClasses;
end;

//------------------------------------------------------------------------------
destructor TObjClassesTable.Destroy;
begin
  ClassQuery.Close;
  ClassTable.Close;
  ClassQuery.Free;
  ClassTable.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function TObjClassesTable.InsertNewClass(const KlassenName:string;classParentID,classLevel,classIndex:integer):integer;
var DbAutoInc : TDbAutoInc;
    LastId    : integer;
begin
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  LastId:=DbAutoInc.NextIndex[c_WCClasses];
  if LastID > 0 then begin
    if (ClassTable.OpenExclusive) then with ClassTable do begin
      Append;
      FieldByName(C_ClassId).asInteger:=LastID;
      FieldByName(C_ClassName).asString:=KlassenName;
      FieldByName(c_classParentID).asInteger:=classParentID;
      FieldByName(C_ClassLevel).asInteger:=classLevel;
      FieldByName(C_ClassIndex).asInteger:=classIndex;
      FieldByName(C_ClassTyp).asInteger:=classTyp;
      post;
      result:=LastID
    end
    else begin
      ShowMessage('Einfügen einer neuen Klasse fehlgeschlagen!');
      result:=-1;
    end
  end
  else begin
    ShowMessage('Einfügen einer neuen Klasse fehlgeschlagen!');
    result:=-1;
  end;
  DbAutoInc.Free;
end;

//------------------------------------------------------------------------------
procedure TObjClassesTable.OpenClassTable(ClassLevel:integer);
begin
  with classQuery do begin
    sql.text:='select * from '+c_WCClasses;
    sql.add('where '+c_classLevel+' = '+inttostr(ClassLevel));
    sql.add('and '+c_classTyp+' = '+inttostr(ClassTyp));
    sql.Add('order by '+c_classIndex);
    open;
  end
end;

//------------------------------------------------------------------------------
procedure TObjClassesTable.GetClass(ClassObject:TClassObject);
begin
  with classQuery do begin
    ClassObject.ClassId:=fieldByName(c_classId).AsInteger;
    ClassObject.ClassName:=fieldByName(c_className).asString;
    ClassObject.ClassParentId:=fieldByName(c_classParentId).AsInteger;
    ClassObject.ClassLevel:=fieldByName(c_classLevel).AsInteger;
    ClassObject.ClassIndex:=fieldByName(c_classIndex).AsInteger;
    next;
  end
end;

//------------------------------------------------------------------------------
function TObjClassesTable.OpenClass(ClassId:integer):boolean;
begin
  with classQuery do begin
    requestLive:=true;
    sql.text:='select * from '+c_WCClasses;
    sql.add('where '+c_classId+' = '+inttostr(ClassId));
    sql.add('and '+c_classTyp+' = '+inttostr(ClassTyp));
    open;
    edit;
    result:= not eof
  end
end;

//------------------------------------------------------------------------------
function TObjClassesTable.GetClassName(ClassId:integer):string;
begin
  with classQuery do begin
    requestLive:=false;
    sql.text:='select * from '+c_WCClasses;
    sql.add('where '+c_classId+' = '+inttostr(ClassId));
    sql.add('and '+c_classTyp+' = '+inttostr(ClassTyp));
    open;
    if not eof then
      result:=fieldByName(c_className).asString
    else
      result:=''
  end
end;

//------------------------------------------------------------------------------
function TObjClassesTable.GetParentClassID(ClassId:integer):integer;
begin
  with classQuery do begin
    requestLive:=false;
    sql.text:='select * from '+c_WCClasses;
    sql.add('where '+c_classId+' = '+inttostr(ClassId));
    sql.add('and '+c_classTyp+' = '+inttostr(ClassTyp));
    open;
    if not eof then
      result:=fieldByName(c_classParentID).asInteger
    else
      result:=-1
  end
end;

//------------------------------------------------------------------------------
procedure TObjClassesTable.WriteClassName(const KlassenName:string);
begin
  with classQuery do begin
    fieldByName(c_className).asString:=KlassenName;
    post
  end
end;

//------------------------------------------------------------------------------
procedure TObjClassesTable.WriteClassIndex(ClInd:integer);
begin
  with classQuery do begin
    fieldByName(c_classIndex).asInteger:=ClInd;
    post
  end
end;

//------------------------------------------------------------------------------
procedure TObjClassesTable.WriteClass(ParentId,ClLevel,ClIndex:integer);
begin
  with classQuery do begin
    fieldByName(c_classParentID).asInteger:=ParentId;
    fieldByName(c_classLevel).asInteger:=ClLevel;
    fieldByName(c_classIndex).asInteger:=ClIndex;
    post
  end
end;

//------------------------------------------------------------------------------
procedure TObjClassesTable.DeleteClass(ClassId:integer);
begin
  with classQuery do begin
    requestLive:=true;
    sql.text:='delete from '+c_WCClasses;
    sql.add('where '+c_classId+' = '+inttostr(ClassId));
    ExecSql;
  end
end;

//------------------------------------------------------------------------------
function TObjClassesTable.ClassTableEof:boolean;
begin
  result:=classQuery.eof
end;

//==============================================================================
//------------------------------------------------------------------------------
constructor TObjBrennwertTabelle.Create;
begin
  inherited Create;
  BWQuery:=TQuery.Create(nil);
  BWQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
end;

//------------------------------------------------------------------------------
destructor TObjBrennwertTabelle.Destroy;
begin
  BWQuery.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function TObjBrennwertTabelle.GetBWDaten(BwId:integer; var BWdaten:RecBrennwert):boolean;
begin
  with BWQuery do begin
  try
    sql.text:='select * from '+c_WCBrennwertTabelle;
    sql.add('where '+c_BW_ID+'=:'+c_BW_ID);
    parambyName(c_BW_ID).Asinteger:=BwId;
    open;
    if not eof then begin
      BWdaten.BWid:=fields[0].asInteger;
      BWdaten.BWkz:=abs(fields[1].asInteger);
      BWdaten.BWname:=fields[2].asString;
      BWdaten.BW:=fields[3].asFloat;
      result:=true
    end
    else
      result:=false;
  except
    result:=false;
  end
  end
end;

//==============================================================================
//------------------------------------------------------------------------------
constructor TObjDruckstufenTabelle.Create;
begin
  inherited Create;
  DSQuery:=TQuery.Create(nil);
  DSQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
end;

//------------------------------------------------------------------------------
destructor TObjDruckstufenTabelle.Destroy;
begin
  DSQuery.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function TObjDruckstufenTabelle.GetDruckDaten(Druckstufe:integer; var XName:string; var Druck:double):boolean;
begin
  with DSQuery do begin
  try
    sql.text:='select * from '+c_WCDruckstufen;
    sql.add('where '+c_DS_ID+'= '+inttostr(Druckstufe));
    open;
    if not eof then begin
     XName:=fields[1].asString;
     Druck:=abs(fields[2].asInteger);
     result:=true
    end
    else
      result:=false;
  except
    result:=false;
  end
  end
end;

//==============================================================================
//------------------------------------------------------------------------------
constructor TObjLuftdruckTabelle.Create;
begin
  inherited Create;
  LBQuery:=TQuery.Create(nil);
  LBQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
end;

//------------------------------------------------------------------------------
destructor TObjLuftdruckTabelle.Destroy;
begin
  LBQuery.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function TObjLuftdruckTabelle.GetLuftdruckDaten(Bezirk:integer;var XName:string; var Luftdruck:double):boolean;
begin
  with LBQuery do begin
  try
    sql.text:='select * from '+c_WCLuftdruckBezirke;
    sql.add('where '+c_LB_ID+'= '+inttostr(Bezirk));
    open;
    if not eof then begin
     XName:=fields[1].asString;
     Luftdruck:=abs(fields[2].asFloat);
     result:=true
    end
    else
      result:=false;
  except
    result:=false;
  end
  end
end;

//==============================================================================
//------------------------------------------------------------
constructor TObjMstGroup.Create;
begin
  inherited Create;
  GrQuery:=TQuery.Create(nil);
  GrQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
end;

//------------------------------------------------------------
function  TObjMstGroup.OpenMstGruppen(RL:boolean):TQuery;
begin
  GrQuery.sql.text:='select * from '+c_WCStammMstGruppen;
  try
  GrQuery.RequestLive:=RL;
  GrQuery.open;
  result:=GrQuery;
  except
    result:=nil;
  end;
end;

//------------------------------------------------------------
function  TObjMstGroup.OpenMstGruppenSort:TQuery;
begin
  GrQuery.sql.text:='select * from '+c_WCStammMstGruppen;
  GrQuery.sql.add('order by '+c_MstGruppen_OrdNr);
  try
  GrQuery.open;
  result:=GrQuery;
  except
    result:=nil;
  end;
end;

//------------------------------------------------------------------------------
// Liefert die höchste vergebene Ordnungsnummer
function  TObjMstGroup.GetMaxOrdnr:integer;
begin
  with GrQuery do begin
   try
   sql.text:='select Max('+c_MstGruppen_OrdNr+') from '+c_WCStammMstGruppen;
   open;
   if not eof then
     result:=fields[0].AsInteger
   else
     result:=0
   except
     result:=0;
   end;
   close;
  end
end;

//------------------------------------------------------------------------------
// Liefert die Gruppen-ID mit der kleinsten Ordnungsnummer
function  TObjMstGroup.GetGrIDMinOrdnr:integer;
begin
  with GrQuery do begin
   try
   sql.text:='select '+c_MstGruppen_ID+','+c_MstGruppen_OrdNr+' from '+c_WCStammMstGruppen;
   sql.Add('order by '+c_MstGruppen_OrdNr);
   open;
   if not eof then
     result:=fieldByName(c_MstGruppen_ID).AsInteger
   else
     result:=0
   except
     result:=0;
   end;
   close;
  end
end;

//------------------------------------------------------------
function TObjMstGroup.DeleteMstGruppe(ID : integer):boolean;
begin
  with GrQuery do begin
    try
    RequestLive:=true;
    sql.clear;
    sql.add('delete from '+c_WCStammMstGruppen);
    sql.add('where '+c_MstGruppen_ID+' =:'+c_MstGruppen_ID);
    paramByName(c_MstGruppen_ID).asInteger:=ID;
    ExecSql;
    RequestLive:=false;
    result:=true;
    except
      result:=false
    end
  end
end;

//------------------------------------------------------------
function TObjMstGroup.WriteDaten(MstGruppe:TMstGruppe):boolean;
begin
  with GrQuery do begin
    try
    RequestLive:=true;
    sql.text:='select * from '+c_WCStammMstGruppen;
    sql.add('where '+c_MstGruppen_ID+' = :'+c_MstGruppen_ID);
    paramByName(c_MstGruppen_ID).asInteger:=MstGruppe.Nummer;
    open;
    if not eof then begin
      edit;
      fieldByName(c_MstGruppen_Name).AsString:=MstGruppe.Name;
      fieldByName(c_MstGruppen_MpLaenge).AsInteger:=MstGruppe.Mp;
      fieldByName(c_MstGruppen_EndeAwTag).AsInteger:=MstGruppe.AwTag;
      fieldByName(c_MstGruppen_EndeAwJahr).AsInteger:=MstGruppe.AwJahr;
      post;
      result:=true
    end
    else
      result:=false
    except
      result:=false
    end
  end
end;

//------------------------------------------------------------
function  TObjMstGroup.WriteIconName(MstGruppe:TMstGruppe):boolean;
begin
  with GrQuery do begin
    try
    RequestLive:=true;
    sql.text:='select * from '+c_WCStammMstGruppen;
    sql.add('where '+c_MstGruppen_ID+' = :'+c_MstGruppen_ID);
    paramByName(c_MstGruppen_ID).asInteger:=MstGruppe.Nummer;
    open;
    if not eof then begin
      edit;
      fieldByName(c_MstGruppen_ICO_Name).AsString:=MstGruppe.IconName;
      post;
      result:=true
    end
    else
      result:=false
    except
      result:=false
    end
  end
end;

//------------------------------------------------------------
function  TObjMstGroup.WriteOrdnungsNr(MstGruppe:TMstGruppe):boolean;
begin
  with GrQuery do begin
    try
    RequestLive:=true;
    sql.text:='select * from '+c_WCStammMstGruppen;
    sql.add('where '+c_MstGruppen_ID+' = :'+c_MstGruppen_ID);
    paramByName(c_MstGruppen_ID).asInteger:=MstGruppe.Nummer;
    open;
    if not eof then begin
      edit;
      fieldByName(c_MstGruppen_OrdNr).AsInteger:=MstGruppe.OrdNummer;
      post;
      result:=true
    end
    else
      result:=false
    except
      result:=false
    end
  end
end;

//------------------------------------------------------------
function  TObjMstGroup.WriteState(MstGruppe:TMstGruppe):boolean;
begin
  with GrQuery do begin
    try
    RequestLive:=true;
    sql.text:='select * from '+c_WCStammMstGruppen;
    sql.add('where '+c_MstGruppen_ID+' = :'+c_MstGruppen_ID);
    paramByName(c_MstGruppen_ID).asInteger:=MstGruppe.Nummer;
    open;
    if not eof then begin
      edit;
      fieldByName(c_MstGruppen_State).AsInteger:=MstGruppe.State;
      post;
      result:=true
    end
    else
      result:=false
    except
      result:=false
    end
  end
end;

//------------------------------------------------------------------------------
function  TObjMstGroup.InsertRecord(MstGruppe:TMstGruppe):boolean;
var LastId : integer;
    DbAutoInc : TDbAutoInc;
begin
  result:=false;
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  with GrQuery do begin
    RequestLive:=true;
    LastId:=DbAutoInc.NextIndex[c_WCStammMstGruppen];
    if LastID > 0 then begin
      try
      sql.text:='select * from '+c_WCStammMstGruppen;
      open;
      with MstGruppe do
        AppendRecord([LastId,Name,Mp,AWTag,AWJahr,IconName,OrdNummer])
      except
        result:=false
      end
    end
    else begin
      ShowMessage('Gruppdenanlegen fehlgeschlagen!');
      result:=false
    end
  end;
  DbAutoInc.Free;
end;

//------------------------------------------------------------
function  TObjMstGroup.GetGroupName(GrId:integer):string;
begin
  try
  GrQuery.sql.text:='select * from '+c_WCStammMstGruppen;
  GrQuery.sql.add('where '+c_MstGruppen_ID+' = :'+c_MstGruppen_ID);
  GrQuery.ParamByName(c_MstGruppen_ID).AsInteger:=GrID;
  GrQuery.open;
  if not GrQuery.eof then
    result:=GrQuery.fieldByName(c_MstGruppen_Name).AsString
  else
    result:='';
  except
    result:='';
  end;
end;

//------------------------------------------------------------
function  TObjMstGroup.GetImageName(GrId:integer):string;
begin
  try
  GrQuery.sql.text:='select * from '+c_WCStammMstGruppen;
  GrQuery.sql.add('where '+c_MstGruppen_ID+' = :'+c_MstGruppen_ID);
  GrQuery.ParamByName(c_MstGruppen_ID).AsInteger:=GrID;
  GrQuery.open;
  if not GrQuery.eof then
    result:=GrQuery.fieldByName(c_MstGruppen_ICO_Name).AsString
  else
    result:='';
  except
    result:='';
  end;
end;

//------------------------------------------------------------------------------
destructor TObjMstGroup.Destroy;
begin
  GrQuery.Free;
  inherited Destroy;
end;


//==============================================================================
//------------------------------------------------------------------------------
constructor TObjAwGruppeTable.Create;
begin
  inherited Create;
  WorkQuery:=TQueryExt.create(nil);
  WorkQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  GroupQuery:=TQuery.create(nil);
  GroupQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  AutoQuery:=TQuery.create(nil);
  AutoQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
end;


//------------------------------------------------------------------------------
destructor TObjAwGruppeTable.Destroy;
begin
  WorkQuery.Free;
  GroupQuery.free;
  autoQuery.free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.OpenAutoTabel(RL:boolean):TQuery;
begin
  with AutoQuery do begin
    RequestLive:=RL;
    sql.text:='select * from '+c_WCAWGruppenAuto;
    open;
    if not eof then
      result:=AutoQuery
    else
      result:=nil
  end
end;

//------------------------------------------------------------------------------
Procedure TObjAwGruppeTable.CloseAutoTabel;
begin
  AutoQuery.close;
end;

//------------------------------------------------------------------------------
Procedure TObjAwGruppeTable.ClearAutoTabel;
begin
  with AutoQuery do begin
    sql.text:='delete from '+c_WCAWGruppenAuto;
    ExecSql;
  end
end;

//------------------------------------------------------------------------------
Procedure TObjAwGruppeTable.InsertAutoTabel(GrId:Integer);
begin
  with AutoQuery do begin
    RequestLive:=true;
    AppendRecord([GrId]);
  end
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.FunktionListeLaden(GrId:integer; ObjListe : TObjectList):boolean;
var GruppenFunktion : TGruppenFunktion;
begin
  ObjListe.clear;
  with GroupQuery do begin
    sql.text:='select * from '+c_WCAWGFunktion;
    sql.add('where '+c_AWGFkt_ID+' = :'+c_AWGFkt_ID);
    parambyName(c_AWGFkt_ID).AsInteger:=GrID;
    try;
    open;
    while not eof do begin
      GruppenFunktion:=TGruppenFunktion.create;
      GruppenFunktion.GrId:=FieldByName(c_AWGFkt_ID).AsInteger;
      GruppenFunktion.FktGr:=FieldByName(c_AWGFkt_FkrGR).AsInteger;
      GruppenFunktion.Fkt:=FieldByName(c_AWGFkt_Funktion).AsInteger;
      ObjListe.Add(GruppenFunktion);
      next
    end;
    result:=true;
    except
      result:=false;
    end
  end;
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.ObjectListeLaden(GrId:integer; ObjListe : TObjectList):boolean;
var GruppenOjekt : TGruppenOjekt;
begin
  ObjListe.clear;
  with GroupQuery do begin
    sql.text:='select * from '+c_WCAWGObject;
    sql.add('where '+c_AWGObj_ID+' = :'+c_AWGObj_ID);
    sql.add('order by '+c_AWGObj_OrdnungsNr);
    parambyName(c_AWGObj_ID).AsInteger:=GrID;
    try;
    open;
    while not eof do begin
      GruppenOjekt:=TGruppenOjekt.create;
      GruppenOjekt.GrId:=FieldByName(c_AWGObj_ID).AsInteger;
      GruppenOjekt.ObjTyp:=FieldByName(c_AWGObj_Typ).AsInteger;
      GruppenOjekt.ObjID:=FieldByName(c_AWGObj_ObjID).AsInteger;
      ObjListe.Add(GruppenOjekt);
      next
    end;
    result:=true;
    except
      result:=false;
    end
  end;
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.GetGroup(AuswerteGruppe:TAuswerteGruppe):boolean;
begin
  with GroupQuery do begin
    sql.text:='select * from '+c_WCAWGruppen;
    sql.add('where '+c_AWG_ID+' = :'+c_AWG_ID);
    paramByName(c_AWG_ID).AsInteger:=AuswerteGruppe.GrId;
    open;
    if eof then
      result:=false
    else begin
      AuswerteGruppe.GrTyp:=GroupQuery.fieldByName(c_AWG_Typ).AsInteger;
      AuswerteGruppe.GrName:=GroupQuery.fieldByName(c_AWG_Name).AsString;
      AuswerteGruppe.ClassLink:=GroupQuery.fieldByName(c_AWG_ClassLink).AsInteger;
      AuswerteGruppe.ExportVerz:=GroupQuery.fieldByName(c_AWG_ExportDir).AsString;
      AuswerteGruppe.MonatsModus:=GroupQuery.fieldByName(c_AWG_MonatsModus).AsInteger;
      AuswerteGruppe.WTModus:=GroupQuery.fieldByName(c_AWG_WTModus).AsInteger;
      AuswerteGruppe.MinOffset:=GroupQuery.fieldByName(c_AWG_AutoMinOffset).AsInteger;
      AuswerteGruppe.Stunde:=GroupQuery.fieldByName(c_AWG_AutoStunde).AsInteger;
      AuswerteGruppe.AutoModus:=GroupQuery.fieldByName(c_AWG_AutoModus).AsInteger;
      AuswerteGruppe.TagEnde:=GroupQuery.fieldByName(c_AWG_TagEnde).AsInteger;
      AuswerteGruppe.StartDatum:=GroupQuery.fieldByName(c_AWG_StartDatum).AsDateTime;
      AuswerteGruppe.CheckHour:=GroupQuery.fieldByName(c_AWG_CheckHour).AsInteger;
      AuswerteGruppe.Flags:=GroupQuery.fieldByName(c_AWG_Flags).AsInteger;
      AuswerteGruppe.JahrEnde:=GroupQuery.fieldByName(c_AWG_JahrEnde).AsInteger;
      AuswerteGruppe.ZeitBereich:=GroupQuery.fieldByName(c_AWG_Zeitbereich).AsInteger;
      result:=true;
    end
  end
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.GruppenListeLaden(AwGruppenListe : TObjectList;GrTyp:integer;ClassLink:integer=0):boolean;
var AuswerteGruppe : TAuswerteGruppe;
begin
  AwGruppenListe.Clear;
  with GroupQuery do begin
    sql.text:='select * from '+c_WCAWGruppen;
    sql.add('order by '+c_AWG_Name);
    try
    open;
    while not eof do begin
      if (GrTyp = c_ObjTyp_undef) or
         ((0 = GroupQuery.FieldByName(c_AWG_Typ).AsInteger) and (GrTyp = c_ObjTyp_Gruppe)) or // wegen alten Tabellen, in denen noch kein Gruppentyp existierete
         (GrTyp = GroupQuery.FieldByName(c_AWG_Typ).AsInteger) then begin
        if (ClassLink = 0) or
           (GroupQuery.FieldByName(c_AWG_ClassLink).AsInteger = ClassLink) then begin
          if GrTyp = c_ObjTyp_Gruppe then
            AuswerteGruppe:=TAuswerteGruppe.create(OT_AwGruppe)
          else
            AuswerteGruppe:=TAuswerteGruppe.create(OT_AktionsGruppe);
          AuswerteGruppe.GrId:=GroupQuery.FieldByName(c_AWG_ID).AsInteger;
          AuswerteGruppe.GrName:=GroupQuery.FieldByName(c_AWG_Name).AsString;
          AuswerteGruppe.GrTyp:=GroupQuery.FieldByName(c_AWG_Typ).AsInteger;
          AuswerteGruppe.ClassLink:=GroupQuery.FieldByName(c_AWG_ClassLink).AsInteger;
          AuswerteGruppe.ExportVerz:=GroupQuery.FieldByName(c_AWG_ExportDir).AsString;
          AuswerteGruppe.MonatsModus:=GroupQuery.FieldByName(c_AWG_MonatsModus).AsInteger;
          AuswerteGruppe.WTModus:=GroupQuery.FieldByName(c_AWG_WTModus).AsInteger;
          AuswerteGruppe.MinOffset:=GroupQuery.fieldByName(c_AWG_AutoMinOffset).AsInteger;
          AuswerteGruppe.Stunde:=GroupQuery.fieldByName(c_AWG_AutoStunde).AsInteger;
          AuswerteGruppe.AutoModus:=GroupQuery.fieldByName(c_AWG_AutoModus).AsInteger;
          AuswerteGruppe.TagEnde:=GroupQuery.fieldByName(c_AWG_TagEnde).AsInteger;
          AuswerteGruppe.StartDatum:=GroupQuery.fieldByName(c_AWG_StartDatum).AsDateTime;
          AuswerteGruppe.CheckHour:=GroupQuery.fieldByName(c_AWG_CheckHour).AsInteger;
          AuswerteGruppe.Flags:=GroupQuery.fieldByName(c_AWG_Flags).AsInteger;
          AuswerteGruppe.JahrEnde:=GroupQuery.fieldByName(c_AWG_JahrEnde).AsInteger;
          AuswerteGruppe.Zeitbereich:=GroupQuery.fieldByName(c_AWG_Zeitbereich).AsInteger;
          AwGruppenListe.Add(AuswerteGruppe);
        end
      end;
      next
    end;
    result:=true;
    except
      showMessage('Fehlerhafte Datenbank: '+c_WCAWGruppen);
      result:=false;
    end;
    close;
  end;
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.ExistsGroupNameNo(GrName:String):boolean;
begin
  with GroupQuery do begin
    sql.text:='select * from '+c_WCAWGruppen;
    Sql.Add('where '+c_AWG_Name+' = :'+c_AWG_Name);
    parambyName(c_AWG_Name).AsString:=GrName;
    try
    open;
    result:=eof;
    except
      result:=false;
    end
  end;
end;

//------------------------------------------------------------------------------
procedure TObjAwGruppeTable.DeleteObject(GrID,ObjTyp,ObjID:integer);
begin
  with GroupQuery do begin
    sql.Text:='delete from '+c_WCAWGObject;
    sql.add('where '+c_AWGObj_ID+' = :'+c_AWGObj_ID);
    sql.add('and '+c_AWGObj_Typ+' = :'+c_AWGObj_Typ);
    sql.add('and '+c_AWGObj_ObjID+' = :'+c_AWGObj_ObjID);
    parambyName(c_AWGObj_ID).AsInteger:=GrID;
    parambyName(c_AWGObj_Typ).AsInteger:=ObjTyp;
    parambyName(c_AWGObj_ObjID).AsInteger:=ObjID;
    ExecSql;
  end
end;

//------------------------------------------------------------------------------
procedure TObjAwGruppeTable.DeleteObjectsAndFunctions(GrID:integer);
// Gruppen-Objekte, Funktionen usw. löschen
begin
  with GroupQuery do begin
    sql.Text:='delete from '+c_WCAWGObject;
    sql.add('where '+c_AWGObj_ID+' = :'+c_AWGObj_ID);
    parambyName(c_AWGObj_ID).AsInteger:=GrID;
    ExecSql;
    sql.Text:='delete from '+c_WCAWGFunktion;
    sql.add('where '+c_AWGFkt_ID+' = :'+c_AWGFkt_ID);
    parambyName(c_AWGFkt_ID).AsInteger:=GrID;
    ExecSql;
  end
end;

//------------------------------------------------------------------------------
procedure TObjAwGruppeTable.DeleteMst(MstId:integer);
begin
  with GroupQuery do begin
    sql.Text:='delete from '+c_WCAWGObject;
    sql.add('where '+c_AWGObj_Typ+' = :'+c_AWGObj_Typ);
    sql.add('and '+c_AWGObj_ObjID+' = :'+c_AWGObj_ObjID);
    parambyName(c_AWGObj_Typ).AsInteger:=c_ObjTyp_Mst;
    parambyName(c_AWGObj_ObjID).AsInteger:=MstId;
    ExecSql;
  end
end;

//------------------------------------------------------------------------------
procedure TObjAwGruppeTable.DeleteKan(KanalId:integer);
begin
  with GroupQuery do begin
    sql.Text:='delete from '+c_WCAWGObject;
    sql.add('where '+c_AWGObj_Typ+' = :'+c_AWGObj_Typ);
    sql.add('and '+c_AWGObj_ObjID+' = :'+c_AWGObj_ObjID);
    parambyName(c_AWGObj_Typ).AsInteger:=c_ObjTyp_Kanal;
    parambyName(c_AWGObj_ObjID).AsInteger:=KanalId;
    ExecSql;
    // Netzprotokoll-Spaltendefinitionsliste
    sql.Text:='delete from '+c_WCAWGNetzProCol;
    sql.add('where '+c_AWGNP_KanID+' = '+inttostr(KanalId));
    ExecSql;
  end
end;

//------------------------------------------------------------------------------
procedure TObjAwGruppeTable.DeleteGroup(GrID:integer);
begin
  with GroupQuery do begin
    sql.Text:='delete from '+c_WCAWGruppen;
    sql.add('where '+c_AWG_ID+' = :'+c_AWG_ID);
    parambyName(c_AWG_ID).AsInteger:=GrID;
    ExecSql;
  end;
  DeleteGroupProtDef(GrID);
  DeleteGafikDefAlle(GrID);
end;

//------------------------------------------------------------------------------
procedure TObjAwGruppeTable.DeleteGroupProtDef(GRID:integer);
begin
  with GroupQuery do begin
    sql.Text:='delete from '+c_WCAWGProtDef;
    sql.add('where '+c_AWGPD_ID+' = :'+c_AWGPD_ID);
    parambyName(c_AWGPD_ID).AsInteger:=GRID;
    ExecSql;
  end
end;

//------------------------------------------------------------------------------
procedure TObjAwGruppeTable.DeleteGroupProtDef(ProtDef:RecProtDef);
begin
  with GroupQuery do begin
    sql.Text:='delete from '+c_WCAWGProtDef;
    sql.add('where '+c_AWGPD_ID+' = :'+c_AWGPD_ID);
    sql.add('and '+c_AWGPD_ProtTyp+' = :'+c_AWGPD_ProtTyp);
    parambyName(c_AWGPD_ID).AsInteger:=ProtDef.GrId;
    parambyName(c_AWGPD_ProtTyp).AsInteger:=ProtDef.ProtTyp;
    ExecSql;
  end
end;

//------------------------------------------------------------------------------
procedure TObjAwGruppeTable.DeleteGroupListPro(GRID:integer);
begin
  with GroupQuery do begin
    sql.Text:='delete from '+c_WCAWGListPro;
    sql.add('where '+c_AWGLP_ID+' = :'+c_AWGLP_ID);
    parambyName(c_AWGLP_ID).AsInteger:=GrID;
    ExecSql;
  end
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.NewGroupName(GrId:integer; GrName:string):integer;
begin
  with GroupQuery do begin
    try
    RequestLive:=true;
    sql.Text:='select * from '+c_WCAWGruppen;
    sql.add('where '+c_AWG_ID+' = :'+c_AWG_ID);
    parambyName(c_AWG_ID).AsInteger:=GrID;
    open;
    if not eof then begin
      edit;
      FieldByName(c_AWG_Name).AsString:=GrName;
      post;
      close;
      result:=GrId
    end
    else
      result:=-1
    except
      result:=-1
    end
  end
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.NewClassLink(GrId:integer; CL:integer):boolean;
begin
  with GroupQuery do begin
    try
    RequestLive:=true;
    sql.Text:='select * from '+c_WCAWGruppen;
    sql.add('where '+c_AWG_ID+' = :'+c_AWG_ID);
    parambyName(c_AWG_ID).AsInteger:=GrID;
    open;
    if not eof then begin
      edit;
      FieldByName(c_AWG_ClassLink).AsInteger:=cl;
      post;
      close;
      result:=true
    end
    else
      result:=false
    except
      result:=false
    end
  end
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.NewExportVerz(GrId:integer; ExportVerz:string):boolean;
begin
  with GroupQuery do begin
    try
    RequestLive:=true;
    sql.Text:='select * from '+c_WCAWGruppen;
    sql.add('where '+c_AWG_ID+' = :'+c_AWG_ID);
    parambyName(c_AWG_ID).AsInteger:=GrID;
    open;
    if not eof then begin
      edit;
      FieldByName(c_AWG_ExportDir).AsString:=ExportVerz;
      post;
      close;
      result:=true
    end
    else
      result:=false
    except
      result:=false
    end
  end
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.WriteAWG(AWG:TAuswerteGruppe):boolean;
begin
  with GroupQuery do begin
    try
    RequestLive:=true;
    sql.Text:='select * from '+c_WCAWGruppen;
    sql.add('where '+c_AWG_ID+' = :'+c_AWG_ID);
    parambyName(c_AWG_ID).AsInteger:=AWG.GrID;
    open;
    if not eof then begin
      edit;
      FieldByName(c_AWG_ExportDir).AsString:=AWG.ExportVerz;
      FieldByName(c_AWG_MonatsModus).AsInteger:=AWG.MonatsModus;
      FieldByName(c_AWG_WTModus).AsInteger:=AWG.WTModus;
      FieldByName(c_AWG_AutoMinOffset).AsInteger:=AWG.MinOffset;
      FieldByName(c_AWG_AutoStunde).AsInteger:=AWG.Stunde;
      FieldByName(c_AWG_AutoModus).AsInteger:=AWG.AutoModus;
      FieldByName(c_AWG_TagEnde).AsInteger:=AWG.TagEnde;
      FieldByName(c_AWG_StartDatum).AsDateTime:=AWG.StartDatum;
      FieldByName(c_AWG_CheckHour).AsInteger:=AWG.CheckHour;
      FieldByName(c_AWG_Flags).AsInteger:=AWG.Flags;
      FieldByName(c_AWG_JahrEnde).AsInteger:=AWG.JahrEnde;
      FieldByName(c_AWG_Zeitbereich).AsInteger:=AWG.Zeitbereich;
      post;
      close;
      result:=true
    end
    else
      result:=false
    except
      result:=false
    end
  end
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.InsertNewGroup(GrName:String;Typ,Klasse:integer):integer;
var DbAutoInc : TDbAutoInc;
    GrId      : integer;
begin
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  GrId:=DbAutoInc.NextIndex[c_WCAWGruppen];
  with GroupQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_WCAWGruppen;
    open;
    AppendRecord([GrId,GrName,typ,Klasse,'']);
    result:=GrId
  end;
  DbAutoInc.Free;
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.InsertNewObject(GrId,ObjTyp,ObjID,OrnungsNr:Integer):boolean;
begin
  try
  with GroupQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_WCAWGObject;
    open;
    AppendRecord([GrId,ObjTyp,ObjID,OrnungsNr]);
    result:=true
  end
  except
    result:=false
  end;
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.InsertNewFunction(GrId,FktGruppe,Funktion:Integer):boolean;
begin
  try
  with GroupQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_WCAWGFunktion;
    open;
    AppendRecord([GrId,FktGruppe,Funktion]);
    result:=true
  end
  except
    result:=false
  end;
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.InsertNewProtDef(ProtDef:RecProtDef):boolean;
var konfig : integer;
begin
  konfig:=0;
  if Protdef.Hochformat then
    konfig:=konfig or c_AWPDKonfig_HochQuer;
  if Protdef.PrintMstNr then
    konfig:=konfig or c_AWPDKonfig_PrintMstNr;
  if Protdef.PrintMstName then
    konfig:=konfig or c_AWPDKonfig_PrintMstName;
  if Protdef.Print_LPF then
    konfig:=konfig or c_AWPDKonfig_PrintLPF;
  if Protdef.SummKanDecode then
    konfig:=konfig or c_AWPDKonfig_SummKanDecode;
  if Protdef.SummKanPrint then
    konfig:=konfig or c_AWPDKonfig_SummKanPrint;
  if Protdef.Print_Minima then
    konfig:=konfig or c_AWPDKonfig_PrintMinima;
  if Protdef.ListProtMDE then
    konfig:=konfig or c_AWPDKonfig_ListProtMDE;
  with GroupQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_WCAWGProtDef;
    open;
    with Protdef do
      AppendRecord([GrId,ProtTyp,FontName,FontSize,
                    AnzUbs,Ubs1,Ubs2,Konfig]);
    RequestLive:=false;
    result:=true
  end
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.GetProtDef(var ProtDef:RecProtDef):boolean;
begin
  try
  with GroupQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_WCAWGProtDef;
    sql.add('where '+c_AWGPD_ID+' = :'+c_AWGPD_ID);
    sql.add('and '+c_AWGPD_ProtTyp+' = :'+c_AWGPD_ProtTyp);
    parambyName(c_AWGPD_ID).AsInteger:=ProtDef.GrId;
    parambyName(c_AWGPD_ProtTyp).AsInteger:=ProtDef.ProtTyp;
    open;
    if eof then begin
      // Noch keine Daten für diese Gruppen-ID gespeichert: Default-Gruppe laden
      sql.Text:='select * from '+c_WCAWGProtDef;
      sql.add('where '+c_AWGPD_ID+' = :'+c_AWGPD_ID);
      sql.add('and '+c_AWGPD_ProtTyp+' = :'+c_AWGPD_ProtTyp);
      parambyName(c_AWGPD_ID).AsInteger:=0;
      parambyName(c_AWGPD_ProtTyp).AsInteger:=ProtDef.ProtTyp;
      open;
    end;

    if not eof then begin
      ProtDef.ProtTyp:=FieldByName(c_AWGPD_ProtTyp).AsInteger;
      ProtDef.Hochformat:=FieldValues[c_AWGPD_Konfig] and c_AWPDKonfig_HochQuer;
      ProtDef.FontName:=FieldByName(c_AWGPD_FontName).AsString;
      ProtDef.FontSize:=FieldByName(c_AWGPD_FontSize).AsInteger;
      ProtDef.AnzUbs:=FieldValues[c_AWGPD_AnzUbs];
      ProtDef.PrintMstNr:=FieldValues[c_AWGPD_Konfig] and c_AWPDKonfig_PrintMstNr;
      ProtDef.PrintMstName:=FieldValues[c_AWGPD_Konfig] and c_AWPDKonfig_PrintMstName;
      ProtDef.Ubs1:=FieldByName(c_AWGPD_Ubs1).AsString;
      ProtDef.Ubs2:=FieldByName(c_AWGPD_Ubs2).AsString;
      ProtDef.Print_LPF:=FieldValues[c_AWGPD_Konfig] and c_AWPDKonfig_PrintLPF;
      ProtDef.SummKanDecode:=FieldValues[c_AWGPD_Konfig] and c_AWPDKonfig_SummKanDecode;
      ProtDef.SummKanPrint:=FieldValues[c_AWGPD_Konfig] and c_AWPDKonfig_SummKanPrint;
      ProtDef.Print_Minima:=FieldValues[c_AWGPD_Konfig] and c_AWPDKonfig_PrintMinima;
      ProtDef.ListProtMDE:=FieldValues[c_AWGPD_Konfig] and c_AWPDKonfig_ListProtMDE;
      result:=true
    end
    else begin
      ProtDef.AnzUbs:=1;
      result:=false // Default-Gruppe auch nicht gefunden
    end
  end
  except
    result:=false
  end;
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.InsertNewLPSpalte(GrId,PTyp,SpId,Breite,BreiteMM,Size,Style,Color:Integer;
                           SpText,Schrift:string;MaxMin:boolean):boolean;
var Statusinfo : integer;
begin
  try
  with GroupQuery do begin
    if maxMin then
      StatusInfo:=c_ListPro_MaxMin
    else
      Statusinfo:=0;
    RequestLive:=true;
    sql.Text:='select * from '+c_WCAWGListPro;
    open;
    AppendRecord([GrId,PTyp,SpID,SpText,Breite,BreiteMM,Schrift,Size,Style,Color,StatusInfo]);
    result:=true
  end
  except
    result:=false
  end;
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.GetColProNetz(var SpaltenDef : TRecNetzproSpalte):boolean;
begin
  with GroupQuery do begin
    sql.Text:='select * from '+c_WCAWGNetzProCol;
    sql.Add('where '+c_AWGNP_GrID+' = '+inttostr(SpaltenDef.GruppenId));
    sql.Add('and '+c_AWGNP_KanID+' = '+inttostr(SpaltenDef.KanalId));
    open;
    result:=not eof;
    if result then with SpaltenDef do begin
      SpaltenId:=FieldByName(c_AWGNP_ID).Asinteger;
      GruppenId:=FieldByName(c_AWGNP_GrID).Asinteger;
      KanalId:=FieldByName(c_AWGNP_KanID).Asinteger;
      UBS[1]:=FieldByName(c_AWGNP_UBS1).AsString;
      UBS[2]:=FieldByName(c_AWGNP_UBS2).AsString;
      UBS[3]:=FieldByName(c_AWGNP_UBS3).AsString;
      Breite:=FieldByName(c_AWGNP_Breite).Asinteger;
      Schrift:=FieldByName(c_AWGNP_Schrift).AsString;
      Size:=FieldByName(c_AWGNP_Size).Asinteger;
      Color:=FieldByName(c_AWGNP_Color).Asinteger;
      Style:=FieldByName(c_AWGNP_Style).Asinteger;
    end;
  end
end;

//------------------------------------------------------------------------------
procedure TObjAwGruppeTable.DeleteColProNetz(GrID:integer);
begin
  with WorkQuery do begin
    RequestLive:=true;
    sql.Text:='delete from '+c_WCAWGNetzProCol;
    sql.Add('where '+c_AWGNP_GrID+' = '+inttostr(GrID));
    ExecSql;
  end
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.InsertNewNetzProCol(Spalte:TRecNetzproSpalte):boolean;
var DbAutoInc : TDbAutoInc;
begin
  result:=false;
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  Spalte.SpaltenId:=DbAutoInc.NextIndex[c_WCAWGNetzProCol];
  with WorkQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_WCAWGNetzProCol;
    open;
    with Spalte do
      AppendRecord([SpaltenId,GruppenId,KanalId,Ubs[1],Ubs[2],Ubs[3],Breite,Schrift,Size,Style,Color,Ausrichtung]);
    close
  end
end;


//------------------------------------------------------------------------------
function TObjAwGruppeTable.GetSpaltenliste(GrId,PTyp:integer):TQuery;
begin
  try
  with GroupQuery do begin
    sql.Text:='select * from '+c_WCAWGListPro;
    sql.add('where '+c_AWGLP_ID+' = :'+c_AWGLP_ID);
    sql.add('and '+c_AWGLP_ProtTyp+' = :'+c_AWGLP_ProtTyp);
    parambyName(c_AWGLP_ID).AsInteger:=GrId;
    parambyName(c_AWGLP_ProtTyp).AsInteger:=PTyp;
    open;
    if eof then
      result:=nil
    else
      result:=GroupQuery
  end
  except
    result:=nil
  end;
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.GetSpaltenDaten(GrId,PTyp,SpId:integer):TQuery;
begin
  try
  with GroupQuery do begin
    sql.Text:='select * from '+c_WCAWGListPro;
    sql.add('where '+c_AWGLP_ID+' = :'+c_AWGLP_ID);
    sql.add('and '+c_AWGLP_ProtTyp+' = :'+c_AWGLP_ProtTyp);
    sql.add('and '+c_AWGLP_SPID+' = :'+c_AWGLP_SPID);
    parambyName(c_AWGLP_ID).AsInteger:=GrId;
    parambyName(c_AWGLP_ProtTyp).AsInteger:=PTyp;
    parambyName(c_AWGLP_SPID).AsInteger:=SpId;
    open;
    if eof then
      result:=nil
    else
      result:=GroupQuery
  end
  except
    result:=nil
  end;
end;

// Grafik-Definitionen
//------------------------------------------------------------------------------
function TObjAwGruppeTable.InsertNewGrafikDef(GrId,FormatIndex,Format,DatFormDef,
                                              Balken,VolKanal,AnaKanal,GrafikTyp,LineSize:integer;
                                              TextFeld1:string=''):boolean;
begin
  try
  with GroupQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_WCGrafikDef;
    open;
    AppendRecord([GrId,FormatIndex,Format,DatFormDef,Balken,VolKanal,AnaKanal,GrafikTyp,TextFeld1,LineSize]);
    result:=true
  end
  except
    result:=false
  end;
end;

//------------------------------------------------------------------------------
function TObjAwGruppeTable.GetGrafikDef(Var GrafikDef:RecGrafikDef;GrafikTyp:integer):boolean;
begin
  try
  with GroupQuery do begin
    sql.Text:='select * from '+c_WCGrafikDef;
    sql.add('where '+c_GrafikDef_ID+' = '+IntToStr(GrafikDef.GruppenID));
    sql.add('and '+c_GrafikDef_GrafikTyp+' = '+IntToStr(GrafikTyp));
    open;
    if eof then
      result:=false
    else with GrafikDef do begin
      formatIndexVol:=fields[1].value;
      format:=fields[2].value;
      DatumFormat:=fields[3].value;
      Balken:=fields[4].value;
      VolKanal:=fields[5].value;
      AnaKanal:=fields[6].value;
      // field[7] ist der Grafiktyp, der nicht in den Record "GrafikDef" übernommen wird!
      try
      TextFeld1:=fields[8].Value;
      except
        TextFeld1:='';
      end;
      LineSize:=fields[9].Value;
      result:=true;
    end
  end
  except
    result:=false
  end;
end;

//------------------------------------------------------------------------------
procedure TObjAwGruppeTable.DeleteGafikDef(GrID:integer;GrafikTyp:Integer);
begin
  with GroupQuery do begin
    RequestLive:=true;
    sql.Text:='delete from '+c_WCGrafikDef;
    sql.add('where '+c_GrafikDef_ID+' = '+IntToStr(GrID));
    sql.add('and '+c_GrafikDef_GrafikTyp+' = '+IntToStr(GrafikTyp));
    ExecSql;
    RequestLive:=false;
  end;
end;


//------------------------------------------------------------------------------
procedure TObjAwGruppeTable.DeleteGafikDefAlle(GrID:Integer);
begin
  with GroupQuery do begin
    RequestLive:=true;
    sql.Text:='delete from '+c_WCGrafikDef;
    sql.add('where '+c_GrafikDef_ID+' = '+IntToStr(GrID));
    ExecSql;
    RequestLive:=false;
  end;
end;


//==============================================================================
constructor TWicalObj.Create(ot:TObjTyp);
begin
  tag:=0;
  ObjTyp:=ot;
end;

//==============================================================================
//------------------------------------------------------------------------------
constructor TWicalStamm.Create;
var MstGruppe : TMstGruppe;
begin
  inherited Create;
  MstQuery:=TQuery.create(nil);
  MstQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  KanQuery:=TQuery.create(nil);
  KanQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  KanForQuery:=TQuery.create(nil);
  KanForQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  KanUmwQuery:=TQuery.create(nil);
  KanUmwQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  KanMrgQuery:=TQuery.create(nil);
  KanMrgQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  KanDSfGQuery:=TQuery.create(nil);
  KanDSfGQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  LocalQuery:=TQuery.create(nil);
  LocalQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  LocMstQuery:=TQuery.create(nil);
  LocMstQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  LocKanQuery:=TQuery.create(nil);
  LocKanQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  LocForQuery:=TQuery.create(nil);
  LocForQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];
  SkalQuery:=TQuery.create(nil);
  SkalQuery.DataBaseName:=PathServer.PathName[WWicalStammDb];

  TabellenListe:=[];

  // Gruppenliste einlesen
  GruppenListe:=TObjectList.create;
  with LocalQuery do begin
    sql.clear;
    sql.add('select * from '+c_WCStammMstGruppen);
    open;
    while not eof do begin
      MstGruppe:=TMstGruppe.create;
      with MstGruppe do begin
        Nummer:=fields[0].AsInteger;
        Name:=fields[1].AsString;
        Mp:=fields[2].AsInteger;
        AwTag:=fields[3].AsInteger;
        AwJahr:=fields[4].AsInteger;
        IconName:=fields[5].AsString;
      end;
      GruppenListe.Add(MstGruppe);
      next
    end;
    close;
  end;
  MstStammSatz:=TWicalStammSatz.Create(ot_mst);
  KanalStammSatz:=TWicalKanalSatz.Create(ot_kanal);
end;

//------------------------------------------------------------------------------
destructor TWicalStamm.Destroy;
begin
  MstQuery.free;
  KanQuery.free;
  KanForQuery.free;
  KanUmwQuery.free;
  KanMrgQuery.Free;
  KanDSfGQuery.Free;
  LocalQuery.Free;
  LocMstQuery.Free;
  LocKanQuery.Free;
  LocForQuery.Free;
  SkalQuery.Free;

  GruppenListe.free;
  MstStammSatz.free;
  KanalStammSatz.free;
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetImageNrClass(ClassObject:TClassObject):integer;
begin
  result:=C_ImageIndexAlleKlassen
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetImageNrKanal(Node:TTreeNode):integer;

  procedure SetKanalImage(Node:TTreeNode;ImageNr:integer);
  begin
    Node.ImageIndex:=GetImageNrKanal(ImageNr);
    Node.SelectedIndex:=Node.ImageIndex
  end;

begin
  result:=0;
  if TWicalKanalSatz(node.Data).formel = c_DsfGKanal then begin
    if TWicalKanalSatz(node.Data).AuswerteArt and c_AwArt_SZ =  0 then begin
      // Hauptzähler
      if TWicalKanalSatz(node.Data).KanTyp <> 'Z' then
        SetKanalImage(Node,c_DSfGKanal)
      else
        SetKanalImage(Node,c_DSfGZstHZ)
    end
    else begin
      // Störzähler
      if TWicalKanalSatz(node.Data).KanTyp <> 'Z' then
        SetKanalImage(Node,c_DSfGVolSZ)
      else
        SetKanalImage(Node,c_DSfGZstSZ)
    end
  end
  else
    if TWicalKanalSatz(node.Data).formel = c_MRGKanal then begin

      if TWicalKanalSatz(node.Data).AuswerteArt and c_AwArt_SZ =  0 then begin
        // Eingangszähle
        if TWicalKanalSatz(node.Data).KanTyp <> 'Z' then
          SetKanalImage(Node,c_MRGKanal)
        else
          SetKanalImage(Node,c_MRGKanalEZ)
      end
      else
        // Kontrollzähler
        SetKanalImage(Node,c_MRGKanalKZ)
    end
    else
      SetKanalImage(Node,TWicalKanalSatz(node.Data).formel)
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetImageNrKanal(Formel:integer):integer;
begin
  case Formel of
    c_MRGKanal        : result:=0;
    c_MRGKanalEZ      : result:=c_ImageIndexMRGZstEZ;
    c_MRGKanalKZ      : result:=c_ImageIndexMRGZstKZ;
    c_MRGKanalTZ      : result:=c_ImageIndexTageszaehler;   //Originaldaten: Tageszähler aus den Messpweriodenwerten ermittelt

    c_FormelSumme     : result:=1;
    c_FormelManuell   : result:=2;
    c_FormelDifferenz : result:=3;
    c_FormelMittelwert: result:=5;
    c_SpeicherInhalt  : result:=6;
    c_DSfGKanal       : result:=7;  // Hauptzäler, Volumen
    c_DSfGVolSZ       : result:=c_ImageIndexDSfGVolSZ;
    c_DSfGZstHZ       : result:=c_ImageIndexDSfGZstHZ;
    c_DSfGZstSZ       : result:=c_ImageIndexDSfGZstSZ;

    c_TWKanal         : result:=-1;
    c_LaksKanal       : result:=-1;
    c_FormelProdukt   : result:=-1;
    c_PVPKanal        : result:=-1;
    c_MRG910Kanal     : result:=16;
    c_EnergieKanal    : result:=C_ImageIndexEnergiekanal;
    c_VnKanal         : result:=C_ImageIndexVnkanal;
    c_ZZKanal         : result:= C_ImageIndexZZahl;
    c_ProzentKanal    : result:=C_ImageIndexProzKanal;
    //97...
    c_AlleKlassen     : result:=C_ImageIndexAlleKlassen;
    c_AlleKanaele     : result:=C_ImageIndexAlleKanaele;
    c_RohdatenKanaele : result:=C_ImageIndexRohdaten;
    c_AlleAWGKlassen  : result:=c_ImageIndexAlleAWGKlassen;
    else result:=0
  end;
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetAnzMpProTag(MstId:integer):integer;
begin
  result:=GetMpLaenge(MstId);
  if result > 0 then
    result:=24*60 div result
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetMpLaenge(MstId:integer):integer;
var MstGruppe : TMstGruppe;
    AwGrNr,
    i        : integer;
begin
  AwGrNr:=GetMstDatenAwGruppe(MstId);
  if AwGrNr >= 0 then begin
    for i:=0 to GruppenListe.Count-1 do begin
      MstGruppe:=TMstGruppe(GruppenListe.Items[i]);
      if MstGruppe.Nummer = AwGrNr then begin
        result:=MstGruppe.Mp;
        exit
      end
    end;
    result:=-2
  end
  else
    result:=-1
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetTagEnde(MstId:integer):integer;
var MstGruppe : TMstGruppe;
    AwGrNr,
    i        : integer;
begin
  if MrgDialogIni.AWTagende > -1 then
    result:=MrgDialogIni.AWTagende
  else begin
    // Bei einer Einstellung = -1 wird die gruppenindividuelle Einstellung verwendet
    AwGrNr:=GetMstDatenAwGruppe(MstId);
    if AwGrNr >= 0 then begin
      for i:=0 to GruppenListe.Count-1 do begin
        MstGruppe:=TMstGruppe(GruppenListe.Items[i]);
        if MstGruppe.Nummer = AwGrNr then begin
          result:=MstGruppe.AwTag;
          exit
        end
      end;
      result:=-2
    end
    else
      result:=-1
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetJahrEnde(MstId:integer):integer;
var MstGruppe : TMstGruppe;
    AwGrNr,
    i        : integer;
begin
  if MrgDialogIni.AWJahresende > 0 then
    result:=MrgDialogIni.AWJahresende
  else begin
    // Bei einer Einstellung = 0 wird die gruppenindividuelle Einstellung verwendet
    AwGrNr:=GetMstDatenAwGruppe(MstId);
    if AwGrNr >= 0 then begin
      for i:=0 to GruppenListe.Count-1 do begin
        MstGruppe:=TMstGruppe(GruppenListe.Items[i]);
        if MstGruppe.Nummer = AwGrNr then begin
          result:=MstGruppe.AwJahr;
          exit
        end
      end;
      result:=-2
    end
    else
      result:=-1
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.OpenKanTables:TQuery;
begin
  CloseTables;
  with KanQuery do begin
    sql.text:='select * from '+c_WCStammKan;
    open;
    result:=KanQuery
  end;
  TabellenListe:=TabellenListe+[Wical_Kan];
end;

//------------------------------------------------------------------------------
function TWicalStamm.OpenKanTables(MstId:integer):TQuery;
begin
  CloseTables;
  with KanQuery do begin
    sql.text:='select * from '+c_WCStammKan;
    sql.add('where '+C_Awkan_MstLfdNr+' = '+inttostr(MstId));
    sql.add('order by '+C_Awkan_OrdnungsNr);
    open;
    result:=KanQuery
  end;
  TabellenListe:=TabellenListe+[Wical_Kan];
end;

//------------------------------------------------------------------------------
function TWicalStamm.OpenKanTablesFormel(KanalListe:TObjectList; Formel:integer):integer;
var ObjFormelKanal  : TObjFormelKanal;
begin
  CloseTables;
  with KanQuery do begin
    sql.text:='select * from '+c_WCStammKan;
    sql.add('where '+C_Awkan_Formel+' = '+inttostr(Formel));
    open;
    if eof then
      result:=KanalListe.Count
    else begin
      while not eof do begin
        ObjFormelKanal:=TObjFormelKanal.Create;
        ObjFormelKanal.KanalId:=FieldByName(C_Awkan_AwkLfdNr).AsInteger;
        ObjFormelKanal.Formel:=FieldByName(C_Awkan_Formel).AsInteger;
        ObjFormelKanal.OperantenListe:=TobjectList.Create;
        KanalListe.Add(ObjFormelKanal);
        next
      end;
      result:=KanalListe.Count
    end;
    close
  end
end;

//------------------------------------------------------------------------------
procedure TWicalStamm.OpenKanTablesOrderBy;
begin
  CloseTables;
  with KanQuery do begin
    sql.clear;
    sql.add('select * from '+c_WCStammKan);
    sql.add('order by '+C_Awkan_OrdnungsNr);
    RequestLive:=false;
    open;
    TabellenListe:=[wical_Kan];
  end;
end;

//------------------------------------------------------------------------------
procedure TWicalStamm.OpenTablesNew(TL:TWicalTabellen; RL:boolean);
begin
  CloseTables;
  OpenTables(TL,RL);
end;

//------------------------------------------------------------
procedure TWicalStamm.OpenTables(TL:TWicalTabellen; RL:boolean);
begin
  if wical_Mst in TL then
    with MstQuery do begin
      sql.clear;
      sql.add('select * from '+c_WCStammMst);
      if not rl then
        sql.add('order by '+c_AwMst_MstName);
      RequestLive:=RL;
      open;
      TabellenListe:=TabellenListe+[wical_Mst];
    end;
  if wical_Kan in TL then
    with KanQuery do begin
      sql.clear;
      sql.add('select * from '+c_WCStammKan);
      if not rl then
        sql.add('order by '+C_Awkan_OrdnungsNr);
      RequestLive:=RL;
      open;
      TabellenListe:=TabellenListe+[wical_Kan];
    end;
  if wical_KanFor in TL then
    with KanForQuery do begin
      sql.clear;
      sql.add('select * from '+c_WCStammKanFor);
      RequestLive:=RL;
      open;
      TabellenListe:=TabellenListe+[wical_KanFor];
    end;
  if wical_KanUmw in TL then
    with KanUmwQuery do begin
      sql.clear;
      sql.add('select * from '+c_WCStammUmKan);
      RequestLive:=RL;
      open;
      TabellenListe:=TabellenListe+[wical_KanUmw];
    end;
  if wical_KanMrg in TL then
    with KanMrgQuery do begin
      sql.clear;
      sql.add('select * from '+c_WCStammKanMrg);
      RequestLive:=RL;
      open;
      TabellenListe:=TabellenListe+[wical_KanMrg];
    end;
  if wical_KanDSfG in TL then
    with KanDSfGQuery do begin
      sql.clear;
      sql.add('select * from '+c_WCStammKanDSfG);
      RequestLive:=RL;
      open;
      TabellenListe:=TabellenListe+[wical_KanDSfG];
    end;
end;

//------------------------------------------------------------
procedure TWicalStamm.OpenTablesSort(TL:TWicalTabellen; SortFeld1,SortFeld2,SortFeld3 : string);
begin
  if wical_Mst in TL then
    with MstQuery do begin
      sql.clear;
      sql.add('select * from '+c_WCStammMst);
      if SortFeld3 = '' then
        sql.add('order by '+SortFeld1+','+SortFeld2)
      else
        sql.add('order by '+SortFeld1+','+SortFeld2+','+SortFeld3);
      open;
      TabellenListe:=TabellenListe+[wical_Mst];
    end;
  if wical_Kan in TL then
    with KanQuery do begin
      sql.clear;
      sql.add('select * from '+c_WCStammKan);
      sql.add('order by '+C_Awkan_OrdnungsNr);
      open;
      TabellenListe:=TabellenListe+[wical_Kan];
    end;
  if wical_KanFor in TL then
    with KanForQuery do begin
      sql.clear;
      sql.add('select * from '+c_WCStammKanFor);
      open;
      TabellenListe:=TabellenListe+[wical_KanFor];
    end;
  if wical_KanUmw in TL then
    with KanUmwQuery do begin
      sql.clear;
      sql.add('select * from '+c_WCStammUmKan);
      open;
      TabellenListe:=TabellenListe+[wical_KanUmw];
    end;
  if wical_KanMrg in TL then
    with KanMrgQuery do begin
      sql.clear;
      sql.add('select * from '+c_WCStammKanMrg);
      open;
      TabellenListe:=TabellenListe+[wical_KanMrg];
    end;
  if wical_KanDSfG in TL then
    with KanDSfGQuery do begin
      sql.clear;
      sql.add('select * from '+c_WCStammKanDSfG);
      open;
      TabellenListe:=TabellenListe+[wical_KanDSfG];
    end;
end;

//------------------------------------------------------------------------------
procedure TWicalStamm.CloseTables;
begin
  if wical_Mst in TabellenListe then MstQuery.Close;
  if wical_Kan in TabellenListe then KanQuery.Close;
  if wical_KanFor in TabellenListe then KanForQuery.Close;
  if wical_KanUmw in TabellenListe then KanUmwQuery.Close;
  if wical_KanMrg in TabellenListe then KanMrgQuery.Close;
  if wical_KanDSfG in TabellenListe then KanDSfGQuery.Close;
  TabellenListe:=[];
end;

//-------------------------------------------------------
function TWicalStamm.ExistsMst(MstID:integer):boolean;
begin
  with LocalQuery do begin
    sql.Text:='select '+c_AwMst_MstLfdNr+' from '+c_WCStammMst;
    sql.add('where '+c_AwMst_MstLfdNr+' = '+intToStr(MstId));
    open;
    result:=not eof;
    close
  end
end;

//-------------------------------------------------------
function TWicalStamm.ExistsKanal(KanalID:integer):boolean;
begin
  with LocalQuery do begin
    sql.Text:='select '+C_Awkan_AwkLfdNr+' from '+c_WCStammKan;
    sql.add('where '+C_Awkan_AwkLfdNr+' = '+intToStr(KanalId));
    open;
    result:=not eof
  end
end;

//-------------------------------------------------------
procedure TWicalStamm.InsertMst(var Mst:TWicalStammSatz);
var DbAutoInc : TDbAutoInc;
    Status : integer;
begin
  Status:=0;
  if Mst.MstNoSZ then
    Status:=Status or c_Mst_Status_NoSWZ;
  if Mst.MstInaktiv then
    Status:=Status or c_Mst_Status_Inaktiv;
  if Mst.MstMDE then
    Status:=Status or c_Mst_Status_MDE;
  OpenTablesNew([Wical_Mst],true);
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  with MstQuery do begin
    Mst.MstId:=DbAutoInc.NextIndex[c_WCStammMst];
    with Mst do
      insertRecord([MstId,MstNr,MstName,MstGruppe,MstKennung,MstTyp,MstKlasse,MstOrdnungsNr,Status,
                    MstAdresse.Name,MstAdresse.PLZ,MstAdresse.Ort,MstAdresse.Strasse]);
  end;
  DbAutoInc.Free;
  CloseTables;
end;

//-------------------------------------------------------
function TWicalStamm.SucheMst(MstId:integer):boolean;
begin
  Query:=MstQuery;
  with Query do begin
    first;
    while not eof do begin
      if fieldByName(c_AwMst_MstLfdNr).AsInteger=MstId then begin
        edit;
        result:=true;
        exit;
      end;
      next
    end;
    result:=false;
  end;
end;

//-------------------------------------------------------
procedure TWicalStamm.WriteMst(Mst:TWicalStammSatz);
var Status : integer;
begin
  CloseTables;
  with MstQuery do begin
    RequestLive:=true;
    sql.clear;
    sql.add('select * from '+c_WCStammMst);
    sql.add('where MstLfdNr ='+inttoStr(Mst.Mstid));
    open;
    Edit;
    Status:=0;
    if Mst.MstNoSZ then
      Status:=Status or c_Mst_Status_NoSWZ;
    if Mst.MstInaktiv then
      Status:=Status or c_Mst_Status_Inaktiv;
    if Mst.MstMDE then
      Status:=Status or c_Mst_Status_MDE;
    if not eof then begin
      fields[1].AsString:=Mst.MstNr;
      fields[2].AsString:=Mst.MstName;
      fields[3].AsInteger:=Mst.MstGruppe;
      fields[4].AsString:=Mst.MstKennung;
      fields[5].AsInteger:=Mst.MstTyp;
      fields[6].AsInteger:=Mst.MstKlasse;
      fields[7].AsInteger:=Mst.MstOrdnungsNr;
      fields[8].AsInteger:=Status;
      fields[9].AsString:=Mst.MstAdresse.Name;
      fields[10].AsString:=Mst.MstAdresse.PLZ;
      fields[11].AsString:=Mst.MstAdresse.Ort;
      fields[12].AsString:=Mst.MstAdresse.Strasse;
    end;
    post;
    close;
    RequestLive:=false;
  end;
end;

//-------------------------------------------------------
procedure TWicalStamm.DeleteMst(Mst:TWicalStammSatz);
begin
  CloseTables;
  with MstQuery do begin
    RequestLive:=true;
    sql.clear;
    sql.add('delete from '+c_WCStammMst);
    sql.add('where MstLfdNr ='+inttoStr(Mst.Mstid));
    ExecSql;
    RequestLive:=false;
  end;
end;

//-------------------------------------------------------
function TWicalStamm.SucheAwKanal(KanId:integer):boolean;
begin
  Query:=KanQuery;
  with Query do begin
    first;
    while not eof do begin
      if (fieldByName(C_Awkan_AwkLfdNr).AsInteger=KanId) then begin
        edit;
        result:=true;
        exit;
      end;
      next
    end;
    result:=false;
  end;
end;

//-------------------------------------------------------
procedure TWicalStamm.WriteAwKanal(Kanal:TWicalKanalSatz);
begin
  WriteStr(C_Awkan_Name,Kanal.KanName);
  WriteInteger(C_Awkan_Formel,Kanal.Formel);
  WriteStr(C_Awkan_Einheit,Kanal.Einheit);
  WriteStr(C_Awkan_Typ,Kanal.KanTyp);
  WriteFloat(C_Awkan_Nachkomma,Kanal.NachKomma);
  WriteFloat(C_Awkan_Faktor,Kanal.Faktor);
  WriteFloat(C_Awkan_Konstante,Kanal.Konstante);
  WriteInteger(C_Awkan_AuswerteArt,Kanal.AuswerteArt);
  WriteInteger(C_Awkan_OrdnungsNr,Kanal.OrdnungsNr);
  WriteInteger(C_Awkan_Spaltenbreite,Kanal.Spaltenbreite);
  WriteInteger(C_Awkan_FormatIndex,Kanal.FormatIndex);
  WriteInteger(C_Awkan_KanalLink,Kanal.KanalLink);
  WriteStr(C_Awkan_KanalInfo,Kanal.KanalInfo);
  WriteInteger(c_Awkan_Messgroesse,Kanal.MessGroesse);
  WriteInteger(c_Awkan_Schiene,Kanal.SchienenNr);
  QueryPost;
end;

//-------------------------------------------------------
procedure TWicalStamm.WriteStr(const FeldName, Wert:string);
begin
  query.FieldByName(FeldName).AsString:=Wert;
end;

//-------------------------------------------------------
procedure TWicalStamm.WriteInteger(const FeldName:string; Wert:variant);
begin
  query.FieldByName(FeldName).AsInteger:=Wert
end;

//-------------------------------------------------------
procedure TWicalStamm.WriteFloat(const FeldName:string; Wert:variant);
begin
  query.FieldByName(FeldName).AsFloat:=Wert
end;

//-------------------------------------------------------
procedure TWicalStamm.QueryPost;
begin
  query.post
end;

//-------------------------------------------------------
procedure TWicalStamm.InsertKanal(var Kanal:TWicalKanalSatz);
var DbAutoInc : TDbAutoInc;
begin
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  with KanQuery do begin
    Kanal.KanId:=DbAutoInc.NextIndex[c_WCStammKan];
    with Kanal do
      insertRecord([KanId,MstId,KanName,Formel,Einheit,KanTyp,Nachkomma,Faktor,
                    Konstante,AuswerteArt,ordnungsNr,Spaltenbreite,FormatIndex,KanalLink,KanalInfo,
                    MessGroesse,SchienenNr]);
  end;
  if (kanal.Formel = c_MRGKanal) or (kanal.Formel = c_MRG910Kanal) then
    InsertMRGKanal(Kanal)
  else if kanal.Formel = c_DSfGKanal then
    InsertDSfGKanal(Kanal)
  else if (kanal.Formel = c_EnergieKanal) and (Kanal.KanalDaten.VbLfdNr > 0) then
    InsertUmwerterKanal(Kanal)
  else if (kanal.Formel = c_VnKanal) and (Kanal.KanalDaten.VbLfdNr > 0) then
    InsertUmwerterKanal(Kanal)
  else if (kanal.Formel = c_ProzentKanal) and (Kanal.KanalDaten.VbLfdNr > 0) then
    InsertUmwerterKanal(Kanal);
  DbAutoInc.Free;
end;

//-------------------------------------------------------
procedure TWicalStamm.InsertFormelKanal(Kanal:TWicalKanalSatz);
var LastId : integer;
    DbAutoInc : TDbAutoInc;
begin
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  with KanForQuery do begin
    LastId:=DbAutoInc.NextIndex[c_WCStammKanFor];
    insertRecord([LastId,
                  Kanal.KanalDaten.AwkLfdNr,
                  Kanal.KanalDaten.FormelNr,
                  Kanal.KanalDaten.OperandKanalNr,
                  Kanal.KanalDaten.Operand]);
  end;
  DbAutoInc.Free;
end;


//-------------------------------------------------------
procedure TWicalStamm.InsertMRGKanal(Kanal:TWicalKanalSatz);
begin
  with KanMrgQuery do begin
    Last;
    insertRecord([Kanal.KanId,
                  Kanal.KanalDaten.MrgKennung,
                  Kanal.KanalDaten.MrgKanalNr]);
  end;
end;

//-------------------------------------------------------
procedure TWicalStamm.InsertDSfGKanal(Kanal:TWicalKanalSatz);
begin
  with KanDSfGQuery do begin
    Last;
    insertRecord([Kanal.KanId,
                  Kanal.KanalDaten.StationID,
                  Kanal.KanalDaten.InstanzID,
                  Kanal.KanalDaten.ArchivID,
                  Kanal.KanalDaten.KanalID,
                  Kanal.KanalDaten.HzKanID]);
  end;
end;

//-------------------------------------------------------
procedure TWicalStamm.InsertUmwerterKanal(Kanal:TWicalKanalSatz);
begin
  with KanUmwQuery do begin
    Last;
    insertRecord([Kanal.KanId,
                  Kanal.KanalDaten.VbLfdNr,
                  Kanal.KanalDaten.DrLfdNr,
                  Kanal.KanalDaten.TeLfdNr,
                  Kanal.KanalDaten.KzahlLfdNr,
                  Kanal.KanalDaten.ConDruck,
                  Kanal.KanalDaten.ConTemp,
                  Kanal.KanalDaten.ConLuftdr,
                  Kanal.KanalDaten.ConSpeicher,
                  Kanal.KanalDaten.ConSpeicherMin,
                  Kanal.KanalDaten.ConSpeicherMin]);
  end;
end;

//-------------------------------------------------------
procedure TWicalStamm.DeleteKanal(Kanal:TWicalKanalSatz);
begin
  CloseTables;
  with KanQuery do begin
    RequestLive:=true;
    sql.text:='delete from '+c_WCStammKan;
    sql.add('where '+C_Awkan_AwkLfdNr+'='+inttoStr(kanal.KanId));
    ExecSql;

    sql.text:='select * from '+c_WCStammKan;
    sql.add('where '+C_Awkan_KanalLink+' ='+inttoStr(kanal.KanId));
    open;
    edit;
    while not eof do begin
     fieldByName(C_Awkan_KanalLink).AsInteger:=0;
     next
    end;
    RequestLive:=false;
  end;

  if (Kanal.Formel = c_MrgKanal) or (Kanal.Formel = c_Mrg910Kanal) then
    DeleteMrgKanal(kanal.KanId)
  else if Kanal.Formel = c_DSfGKanal then
    DeleteDSfGKanal(kanal.KanId)
  else if Kanal.Formel in [c_FormelSumme,c_FormelDifferenz,c_FormelMittelwert] then
    DeleteFormelKanal(kanal.KanId)
  else if Kanal.Formel in [c_EnergieKanal,c_VnKanal,c_ZZKanal] then
   DeleteUmwerterKanal(kanal.KanId);

  // Kanal aus Formel-Kanlatabelle löschen
  with KanQuery do begin
    RequestLive:=true;
    sql.clear;
    sql.add('delete from '+c_WCStammKanFor);
    sql.add('where '+C_AwKanFor_KanalNr+' ='+inttoStr(kanal.KanId));
    ExecSql;
    RequestLive:=false;
  end;
  // Referenz-Kanäle in "AwUmKan" auf 0
  with KanQuery do begin
    RequestLive:=true;
    sql.text:='select * from '+c_WCStammUmKan;
    sql.add('where '+c_AwUmKan_AwkVb+' ='+inttoStr(kanal.KanId));
    open;
    edit;
    while not eof do begin
     fieldByName(c_AwUmKan_AwkVb).AsInteger:=0;
     next
    end;
    sql.text:='select * from '+c_WCStammUmKan;
    sql.add('where '+c_AwUmKan_AwkDruck+' ='+inttoStr(kanal.KanId));
    open;
    edit;
    while not eof do begin
     fieldByName(c_AwUmKan_AwkDruck).AsInteger:=0;
     next
    end;
    sql.text:='select * from '+c_WCStammUmKan;
    sql.add('where '+c_AwUmKan_AwkTemp+' ='+inttoStr(kanal.KanId));
    open;
    edit;
    while not eof do begin
     fieldByName(c_AwUmKan_AwkTemp).AsInteger:=0;
     next
    end;
    sql.text:='select * from '+c_WCStammUmKan;
    sql.add('where '+c_AwUmKan_AwkKzahl+' ='+inttoStr(kanal.KanId));
    open;
    edit;
    while not eof do begin
     fieldByName(c_AwUmKan_AwkKzahl).AsInteger:=0;
     next
    end;
    RequestLive:=false;
  end;

end;

//-------------------------------------------------------
procedure TWicalStamm.DeleteMrgKanal(KanalId:integer);
begin
  with KanMrgQuery do begin
    RequestLive:=true;
    sql.clear;
    sql.add('delete from '+c_WCStammKanMrg);
    sql.add('where '+c_AwMrgKan_AwkLfdNr+' ='+inttoStr(KanalId));
    ExecSql;
    RequestLive:=false;
  end;
end;

//-------------------------------------------------------
procedure TWicalStamm.DeleteDSfGKanal(KanalId:integer);
begin
  with KanDSfGQuery do begin
    RequestLive:=true;
    sql.clear;
    sql.add('delete from '+c_WCStammKanDSfG);
    sql.add('where '+c_AwMrgKan_AwkLfdNr+' ='+inttoStr(KanalId));
    ExecSql;
    RequestLive:=false;
  end;
end;

//-------------------------------------------------------
procedure TWicalStamm.DeleteFormelKanal(KanalId:integer);
begin
  with KanMrgQuery do begin
    RequestLive:=true;
    sql.clear;
    sql.add('delete from '+c_WCStammKanFor);
    sql.add('where '+C_AwKanFor_AwkLfdNr+' ='+inttoStr(KanalId));
    ExecSql;
    RequestLive:=false;
  end;
end;

//-------------------------------------------------------
// Der Kanal "ForLfdNr" wird aus der Tabelle gelöscht
procedure TWicalStamm.DeleteEinFormelKanal(ForLfdNr:integer);
begin
  with KanMrgQuery do begin
    RequestLive:=true;
    sql.clear;
    sql.add('delete from '+c_WCStammKanFor);
    sql.add('where '+c_AwKanFor_ForLfdNr+' ='+inttoStr(ForLfdNr));
    ExecSql;
    RequestLive:=false;
  end;
end;

//-------------------------------------------------------
procedure TWicalStamm.DeleteAlleFormelKanaele(KanalId:integer);
begin
  with KanForQuery do begin
    first;
    edit;
    while not eof do begin
      if FieldByName(C_Awkan_AwkLfdNr).AsInteger = KanalId then begin
        delete;
        first
      end
      else
        next
    end;
  end;
end;

//-------------------------------------------------------
procedure TWicalStamm.DeleteUmwerterKanal(KanalId:integer);
begin
  with KanUmwQuery do begin
    sql.clear;
    sql.add('delete from '+c_WCStammUmKan);
    sql.add('where '+c_AwUmKan_AwkLfdNr+' ='+inttoStr(KanalId));
    ExecSql;
  end;
end;


//------------------------------------------------------------------------------
function TWicalStamm.GetMrgKanalDaten(var Kanal:TWicalKanalSatz):boolean;
begin
  with KanMrgQuery do begin
    sql.clear;
    sql.add('select * from '+c_WCStammKanMRG);
    sql.add('where  '+c_AwMrgKan_AwkLfdNr+' = '+Inttostr(Kanal.KanId));
    open;
    if eof then begin
      close;
      result:=false
    end
    else with kanal do begin
      KanalDaten.MrgKennung:=fieldByName(c_AwMrgKan_MrgKennung).AsString;
      KanalDaten.MrgKanalNr:=fieldByName(c_AwMrgKan_MrgKanalNr).AsInteger;
      result:=true;
      close;
    end
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.SelectMRGKanaele(const Kennung:string):integer;
begin
  with KanMrgQuery do begin
    sql.text:='select * from '+c_WCStammKanMRG;
    Sql.Add('where '+c_AwMrgKan_Mrgkennung+' = :kennung');
    paramByName('Kennung').AsString:=kennung;
    open;
    if eof then
      result:=0
    else
      result:=RecordCount
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetMRG910KanalListe(var AwkId:array of integer):integer;
var i : integer;
begin
  i:=0;
  while not KanMrgQuery.eof do begin
    AwkId[i]:=KanMrgQuery.FieldValues[c_AwMrgKan_AwkLfdNr];
    // Prüfen, op MRG910-Kanal
    KanQuery.sql.text:='select * from '+c_WCStammKan;
    KanQuery.sql.add('where '+C_Awkan_AwkLfdNr+' = '+inttostr(AwkId[i]));
    KanQuery.sql.add('and '+C_Awkan_Formel+' = '+inttostr(c_MRG910Kanal));
    KanQuery.open;
    if not KanQuery.eof then
      inc(i);
    KanMrgQuery.Next
  end;
  result:=i;
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetDSfGKanalDaten(var Kanal:TWicalKanalSatz):boolean;
begin
  with KanDSfGQuery do begin
    sql.clear;
    sql.add('select * from '+c_WCStammKanDSfG);
    sql.add('where  '+c_AwDSfGKan_AwkLfdNr+' = '+intToStr(Kanal.KanId));
    open;
    if eof then begin
      close;
      result:=false
    end
    else with kanal do begin
      KanalDaten.StationID:=fieldByName(c_AwDSfGKan_StationId).AsInteger;
      KanalDaten.InstanzID:=fieldByName(c_AwDSfGKan_InstanzId).AsInteger;
      KanalDaten.ArchivID:=fieldByName(c_AwDSfGKan_ArchivID).AsInteger;
      KanalDaten.KanalID:=fieldByName(c_AwDSfGKan_KanalID).AsInteger;
      KanalDaten.HzKanID:=fieldByName(c_AwDSfGKan_HZKanId).AsInteger;
      result:=true;
      close;
    end
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.FindMrgFirstKanal(var Kanal:TWicalKanalSatz):integer;
begin
  with KanMrgQuery do begin
    sql.text:='select * from '+c_WCStammKanMRG;
    Sql.Add('where '+c_AwMrgKan_Mrgkennung+' = :kennung');
    paramByName('Kennung').AsString:=Kanal.KanalDaten.MrgKennung;
    open;
    if eof then
      result:=0
    else begin
      result:=RecordCount;
      Kanal.KanID:=fieldByName(c_AwMRGKan_AwkLfdNr).AsInteger;
      Kanal.KanalDaten.MrgKanalNr:=fieldByName(c_AwMrgKan_MrgKanalNr).AsInteger;
      if not GetLocKanalDaten(Kanal) then
        result:=0
    end;
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.FindMrgNextKanal(var Kanal:TWicalKanalSatz):boolean;
begin
  with KanMrgQuery do begin
    next;
    if eof then
      result:=false
    else begin
      Kanal.KanID:=fieldByName(c_AwDSfGKan_AwkLfdNr).AsInteger;
      Kanal.KanalDaten.MrgKanalNr:=fieldByName(c_AwMrgKan_MrgKanalNr).AsInteger;
      result:=GetLocKanalDaten(Kanal)
    end
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.FindDSfGFirstKanal(var Kanal:TWicalKanalSatz):boolean;
begin
  with KanQuery do begin
    sql.text:='select * from '+c_WCStammKanDSfG;
    sql.add('where  '+c_AwDSfGKan_StationId+' = '+intToStr(Kanal.KanalDaten.StationID));
    sql.add('and  '+c_AwDSfGKan_InstanzId+' = '+intToStr(Kanal.KanalDaten.InstanzID));
    sql.add('and  '+c_AwDSfGKan_ArchivId+' = '+intToStr(Kanal.KanalDaten.ArchivID));
    sql.add('and  '+c_AwDSfGKan_KanalId+' = '+intToStr(Kanal.KanalDaten.KanalID));
    open;
    if eof then
      result:=false
    else begin
      Kanal.KanID:=fieldByName(c_AwDSfGKan_AwkLfdNr).AsInteger;
      if GetLocKanalDaten(Kanal) then
        result:=true
      else
        result:=false
    end;
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.FindDSfGNextKanal(var Kanal:TWicalKanalSatz):boolean;
begin
  with KanQuery do begin
    next;
    if eof then
      result:=false
    else begin
      Kanal.KanID:=fieldByName(c_AwDSfGKan_AwkLfdNr).AsInteger;
      if GetLocKanalDaten(Kanal) then
        result:=true
      else
        result:=false
    end
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.WriteDSfGKanaele(Kanal:TWicalKanalSatz):boolean;
begin
  with KanForQuery do begin
    try
    sql.clear;
    sql.add('select * from '+c_WCStammKanDSfG);
    sql.add('where  '+c_AwDSfGKan_AwkLfdNr+' = '+Inttostr(Kanal.KanID));
    RequestLive:=true;
    open;
    edit;
    if not eof then with kanal.KanalDaten do begin
      fieldByName(c_AwDSfGKan_StationId).AsInteger:=StationID;
      fieldByName(c_AwDSfGKan_InstanzId).AsInteger:=InstanzID;
      fieldByName(c_AwDSfGKan_ArchivID).AsInteger:=ArchivID;
      fieldByName(c_AwDSfGKan_KanalID).AsInteger:=KanalID;
      fieldByName(c_AwDSfGKan_HZKanId).AsInteger:=HzKanID;
      post;
      close;
      result:=true
    end
    else
      result:=false
    except
      result:=false
    end
  end;
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetSzKanalID(HzKanalId:integer):integer;
begin
  with KanForQuery do begin
    sql.text:='select * from '+c_WCStammKanDSfG;
    sql.add('where  '+c_AwDSfGKan_HZKanId+' = '+Inttostr(HzKanalId));
    open;
    if not eof then
      result:=fieldByName(c_AwDSfGKan_AwkLfdNr).asInteger
    else
      result:=0
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetUmwerterKanalDaten(Kanal:TWicalKanalSatz):boolean;
begin
  with KanUmwQuery do begin
    sql.clear;
    sql.add('select * from '+c_WCStammUmKan);
    sql.add('where  '+c_AwUmKan_AwkLfdNr+' = '+Inttostr(Kanal.KanID));
    open;
    if eof then begin
      close;
      result:=false
    end
    else begin
      with kanal.KanalDaten do begin
        VbLfdNr:=fieldByName(c_AwUmKan_AwkVb).AsInteger;
        DrLfdNr:=fieldByName(c_AwUmKan_AwkDruck).AsInteger;
        TeLfdNr:=fieldByName(c_AwUmKan_AwkTemp).AsInteger;
        KzahlLfdNr:=fieldByName(c_AwUmKan_AwkKzahl).AsInteger;
        ConDruck:=fieldByName(c_AwUmKan_Druck).AsFloat;
        ConTemp:=fieldByName(c_AwUmKan_Temp).AsFloat;
        ConLuftdr:=fieldByName(c_AwUmKan_Luftdruck).AsFloat;
        ConKzahl:=fieldByName(c_AwUmKan_Kzahl).AsFloat;
        ConSpeicher:=fieldByName(c_AwUmKan_Speicher).AsFloat;
        ConSpeicherMin:=fieldByName(c_AwUmKan_SpeicherMin).AsFloat;
      end;
      result:=true;
      close;
    end;
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.SelectFormelKanaele(KanalId:integer; Formel:byte):boolean;
begin
  with KanForQuery do begin
    sql.clear;
    sql.add('select * from '+c_WCStammKanFor);
    sql.add('where  '+C_AwKanFor_AwkLfdNr+' = '+Inttostr(KanalId));
    sql.add('and  FormelNr = '+Inttostr(formel));
    open;
    SelectformelKanaele:=not eof;
  end;
  TabellenListe:=[wical_KanFor];
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetFormelKanalDaten(var Kanal:TWicalKanalSatz):boolean;
begin
  if KanForQuery.eof then result:=false
  else begin
    Kanal:=TWicalKanalSatz.create(OT_Kanal);
    with kanal,KanForQuery do begin
      KanalDaten.ForLfdNr:=fieldByName(c_AwKanFor_ForLfdNr).AsInteger;
      KanalDaten.AwkLfdNr:=fieldByName(c_AwKanFor_AwkLfdNr).AsInteger;
      KanalDaten.FormelNr:=fieldByName(c_AwKanFor_FormelNr).AsInteger;
      KanalDaten.OperandKanalNr:=fieldByName(c_AwKanFor_KanalNr).AsInteger;
      KanalDaten.Operand:=fieldByName(c_AwKanFor_Operand).AsString;
    end;
    result:=true;
    KanForQuery.Next;
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetFormelKanalQuery:TQuery;
begin
  result:=KanForQuery;
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetMaxKanalOrdNr(MstId:integer):integer;
begin
  with LocalQuery do begin
    sql.text:='select * from '+c_WCStammKan;
    sql.Add('where '+C_Awkan_MstLfdNr+' = '+inttostr(MstId));
    try
    open;
    if eof then
      result:=0
    else begin
      result:=-1;
      while not eof do begin
        if result < FieldbyName(C_Awkan_OrdnungsNr).AsInteger then
          result:=FieldbyName(C_Awkan_OrdnungsNr).AsInteger;
        next
      end
    end
    except
      result:=-1
    end
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetKanalListe(MstId:Integer; KanalListe:TObjectList):boolean;
var Kanal:TWicalKanalSatz;
begin
  Kanal:=TWicalKanalSatz.create(OT_kanal);
  if GetFirstKanalDaten(MstId,Kanal) then begin
    repeat
      KanalListe.Add(Kanal);
      Kanal:=TWicalKanalSatz.create(OT_kanal);
    until not GetNextKanalDaten(Kanal);
    Kanal.Free;
    result:=true;
  end
  else begin
    Kanal.free;
    result:=false;
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetLocKanalListe(KanalSelektIndex:integer;KanalListe:TObjectList):boolean;
var Kanal : TWicalKanalSatz;
    Formel : integer;
begin
  case KanalSelektIndex of
    1: Formel:=c_MRGKanal;
  end;
  with LocKanQuery do begin
    sql.text:='select * from '+c_WCStammKan;
    if KanalSelektIndex > 0 then
      sql.Add('where '+C_Awkan_Formel+' = '+inttostr(Formel));
    open;
    while not eof do begin
      Kanal:=TWicalKanalSatz.create(OT_kanal);
      LeseKanalDaten(LocKanQuery,Kanal);
      if ((KanalSelektIndex = 0) and not IsStoermengenKanal(Kanal)) or  // Basiskanäle ohne Störkanäle
         (KanalSelektIndex = 0) then
        KanalListe.Add(Kanal);
      next
    end
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetLocKanalListe(KanalListe:TObjectList):boolean;
var Kanal:TWicalKanalSatz;
begin
  with LocKanQuery do begin
    sql.text:='select * from '+c_WCStammKan;
    sql.Add('order by '+C_Awkan_MstLfdNr);
    open;
    while not eof do begin
      Kanal:=TWicalKanalSatz.create(OT_kanal);
      LeseKanalDaten(LocKanQuery,Kanal);
      KanalListe.Add(Kanal);
      next
    end;
    close;
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetLocKanalDaten(Kanal:TWicalKanalSatz):boolean;
begin
  with LocKanQuery do begin
    sql.text:='select * from '+c_WCStammKan;
    sql.Add('where '+C_Awkan_AwkLfdNr+' = '+inttostr(Kanal.KanID));
    try
    open;
    if not eof then begin
      LeseKanalDaten(LocKanQuery,Kanal);
      result:=true
    end
    else
      result:=false;
    except
      result:=false
    end
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetLocKanalDaten(KanalId:integer):boolean;
begin
  with LocKanQuery do begin
    sql.text:='select * from '+c_WCStammKan;
    sql.Add('where '+C_Awkan_AwkLfdNr+' = '+inttostr(KanalID));
    try
    open;
    if not eof then begin
      LeseKanalDaten(LocKanQuery,KanalStammSatz);
      result:=true
    end
    else
      result:=false;
    except
      result:=false
    end
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetKanalDaten(KanalId:integer; Kanal:TWicalKanalSatz):boolean;
begin
  KanQuery.first;
  result:=false;
  while not KanQuery.eof do begin
    if KanalId = KanQuery.fieldByName(C_Awkan_AWKLfdNr).AsInteger then begin
      LeseKanalDaten(kanQuery,Kanal);
      result:=true;
      exit;
    end;
    KanQuery.next
  end;
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetFirstKanalDaten(MstId:integer; var Kanal:TWicalKanalSatz):boolean;
begin
  MstLfdNr:=MstId;
  KanQuery.first;
  result:=GetNextKanalDaten(Kanal);
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetNextKanalDaten(var Kanal:TWicalKanalSatz):boolean;
begin
  if KanQuery.Eof then result:=false
  else begin
    while not KanQuery.eof do begin
      if KanQuery.fieldByName(C_Awkan_MstLfdNr).AsInteger = MstLfdNr then begin
        kanal:=TWicalKanalSatz.create(OT_Kanal);
        LeseKanalDaten(KanQuery,Kanal);
        KanQuery.next;
        result:=true;
        exit;
      end;
      KanQuery.next;
    end;
    result:=false;
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetMstName(MstId:integer):String;
begin
  result:='';
  MstQuery.First;
  while not MstQuery.eof do begin
    if MstId = MstQuery.fieldByName(c_AwMst_MstLfdNr).AsInteger then begin
      result:=MstQuery.FieldByName(c_AwMst_MstName).AsString;
      exit;
    end;
    MstQuery.next
  end;
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetMstKennung(MstId:integer):String;
begin
  result:='';
  MstQuery.First;
  while not MstQuery.eof do begin
    if MstId = MstQuery.fieldByName(c_AwMst_MstLfdNr).AsInteger then begin
      result:=MstQuery.FieldByName(c_AwMst_MstKennung).AsString;
      exit;
    end;
    MstQuery.next
  end;
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetMstNummer(MstId:integer):String;
begin
  result:='';
  MstQuery.First;
  while not MstQuery.eof do begin
    if MstId = MstQuery.fieldByName(c_AwMst_MstLfdNr).AsInteger then begin
      result:=MstQuery.FieldByName(c_AwMst_MstNummer).AsString;
      exit;
    end;
    MstQuery.next
  end;
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetNextMstDaten(var StammSatz:TWicalStammSatz):boolean;
var Status : integer;
begin
  if MstQuery.Eof then result:=false
  else begin
    LeseMstDaten(MstQuery,StammSatz);
    MstQuery.next;
    result:=true;
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetMstNummerName(MstId:integer):string;
begin
  with LocMstQuery do begin
    sql.clear;
    sql.Add('select * from '+c_WCStammMst);
    sql.add('where '+c_AwMst_MstLfdNr+' = '+intToStr(MstId));
    try
    open;
    if not eof then begin
      if MrgDialogIni.SortierModus = 0 then begin
        result:=FieldByName(c_AwMst_MstName).AsString;
        result:=result+' ['+FieldByName(c_AwMst_MstNummer).AsString+']';
      end
      else begin
        result:=FieldByName(c_AwMst_MstNummer).AsString;
        result:=result+' ['+FieldByName(c_AwMst_MstName).AsString+']';
      end
    end
    else
      result:='';
    except
      on exception do
        result:=''
    end;
  end;
end;


//------------------------------------------------------------------------------
function TWicalStamm.GetMstDaten(MstId:integer; var StammSatz:TWicalStammSatz):boolean;
begin
  if GetMstDaten(MstId) then begin
    StammSatz.copy(MstStammSatz);
    result:=true;
  end
  else
   result:=false;
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetMstDaten(MstId:integer):boolean;
begin
  with LocMstQuery do begin
    sql.clear;
    sql.Add('select * from '+c_WCStammMst);
    sql.add('where '+c_AwMst_MstLfdNr+' = '+intToStr(MstId));
    try
    open;
    if not eof then begin
      LeseMstDaten(LocMstQuery,MstStammSatz);
      result:=true;
    end
    else
      result:=false;
    except
      result:=false
    end
  end
end;

//------------------------------------------------------------------------------
procedure TWicalStamm.LeseMstDaten(MstQuery:TQuery; var MstSatz:TWicalStammSatz);
var Status : integer;
begin
  with MstSatz,MstQuery do begin
    MstId:=FieldByName(c_AwMst_MstLfdNr).AsInteger;
    MstNr:=FieldByName(c_AwMst_MstNummer).AsString;
    MstName:=FieldByName(c_AwMst_MstName).AsString;
    MstGruppe:=FieldByName(c_AwMst_MstGruppe).AsInteger;
    MstKennung:=FieldByName(c_AwMst_MstKennung).AsString;
    MstTyp:=FieldByName(c_AwMst_MstTyp).AsInteger;
    MstKlasse:=FieldByName(c_AwMst_MstLinkToClass).AsInteger;
    MstOrdnungsNr:=FieldByName(c_AwMst_OrdnungNr).AsInteger;
    Status:=FieldByName(c_AwMst_MstStatus).AsInteger;
    MstNoSZ:=Status and c_Mst_Status_NoSWZ > 0;
    MstInaktiv:=Status and c_Mst_Status_Inaktiv > 0;
    MstMDE:=Status and c_Mst_Status_MDE > 0;
    with MstAdresse do begin
      Name:=FieldByName(c_AwMst_StationsName).AsString;
      PLZ:=FieldByName(c_AwMst_PLZ).AsString;
      Ort:=FieldByName(c_AwMst_Ort).AsString;
      Strasse:=FieldByName(c_AwMst_Strasse).AsString;
    end;
  end
end;

//------------------------------------------------------------------------------
// Liste aller Messtellen, die zu einer Mst-Klasse gehören
function TWicalStamm.GetMstListeKlasse(Klasse:integer;MstListe:TObjectList):boolean;
var MstSatz : TWicalStammSatz;
begin
  MstListe.Clear;
  with LocMstQuery do begin
    sql.clear;
    sql.Add('select * from '+c_WCStammMst);
    sql.add('where '+c_AwMst_MstLinkToClass+' = '+intToStr(Klasse));
    try
    open;
    while not eof do begin
      MstSatz:=TWicalStammSatz.Create(OT_Mst);
      LeseMstDaten(LocMstQuery,MstSatz);
      MstListe.Add(MstSatz);
    end;
    except
      result:=false
    end
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetMstDatenAwGruppe(MstId:integer):Integer;
begin
  with LocMstQuery do begin
    sql.clear;
    sql.Add('select * from '+c_WCStammMst);
    sql.add('where '+c_AwMst_MstLfdNr+' = '+intToStr(MstId));
    try
    open;
    if not eof then
      result:=FieldByName(c_AwMst_MstGruppe).AsInteger
    else
      result:=-1;
    except
      on exception do
        result:=-1
    end
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.LeseKanalDaten(var Kanal:TWicalKanalSatz):boolean;
begin
  // KanQuery muss vorher geöffnet werden
  if KanQuery.Eof then
    result:=false
  else begin
    LeseKanalDaten(KanQuery,Kanal);
    KanQuery.Next;
    result:=true
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.IstMstAktiv(MstId:integer):boolean;
begin
  with LocMstQuery do begin
    sql.clear;
    sql.Add('select * from '+c_WCStammMst);
    sql.add('where '+c_AwMst_MstLfdNr+' = '+intToStr(MstId));
    try
    open;
    if not eof then with MstStammSatz do begin
      result:=c_Mst_Status_Inaktiv > 0;
    end
    else
      result:=false;
    except
      on exception do
        result:=false
    end
  end
end;


//------------------------------------------------------------------------------
procedure TWicalStamm.LeseKanalDaten(KQuery:TQuery; var Kanal:TWicalKanalSatz);
begin
  with Kanal,KQuery do begin
    KanID:=fieldByName(C_Awkan_AWKLfdNr).AsInteger;
    MstID:=fieldByName(C_Awkan_MstLfdNr).AsInteger;
    KanName:=fieldByName(C_Awkan_Name).AsString;
    Formel:=fieldByName(C_Awkan_Formel).AsInteger;
    Einheit:=fieldByName(C_Awkan_Einheit).AsString;
    KanTyp:=fieldByName(C_Awkan_Typ).AsString[1];
    Nachkomma:=fieldByName(C_Awkan_Nachkomma).AsInteger;
    Faktor:=fieldByName(C_Awkan_Faktor).AsFloat;
    Konstante:=fieldByName(C_Awkan_Konstante).AsFloat;
    AuswerteArt:=fieldByName(C_Awkan_AuswerteArt).AsInteger;
    OrdnungsNr:=fieldByName(C_Awkan_OrdnungsNr).AsInteger;
    Spaltenbreite:=fieldByName(C_Awkan_Spaltenbreite).AsInteger;
    FormatIndex:=fieldByName(C_Awkan_FormatIndex).AsInteger;
    KanalLink:=fieldByName(C_Awkan_KanalLink).AsInteger;
    KanalInfo:=fieldByName(C_Awkan_KanalInfo).AsString;
    MessGroesse:=fieldByName(c_Awkan_Messgroesse).AsInteger;
    SchienenNr:=fieldByName(c_Awkan_Schiene).AsInteger;
  end
end;


//------------------------------------------------------------
function TWicalStamm.GetNameKanalTyp(KanTyp:integer):string;
begin
  case KanTyp of
    c_MRGKanal        : result:='MRG';
    c_FormelSumme     : result:='SUM';
    c_FormelManuell   : result:='MAN';
    c_FormelDifferenz : result:='DIF';
    c_TWKanal         : result:='TW';
//    c_FormelUmwerter  : result:='UMW';
    c_FormelMittelwert:result:='MW';
    c_SpeicherInhalt  : result:='Speicher';
    c_DSfGKanal       : result:='DSfG';
    c_LaksKanal       : result:='LAKS';
    c_FormelProdukt   : result:='Produkt';
    c_PVPKanal        : result:='PVP';
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetMstId(KanalId:integer):integer;
begin
  with LocalQuery do begin
    sql.clear;
    sql.Add('select '+C_Awkan_AwkLfdNr+','+C_Awkan_MstLfdNr+' from '+c_WCStammKan);
    sql.add('where '+C_Awkan_AwkLfdNr+' = '+intToStr(KanalId));
    open;
    if eof then
      result:=0
    else
      result:=fieldByName(C_Awkan_MstLfdNr).AsInteger;
    close
  end;
end;


//------------------------------------------------------------------------------
// Ist der Kanatyp als Summenkanal zulässig?
function TWicalStamm.ValidTypSumme(KanalTyp:integer):boolean;
begin
  result:= (KanalTyp in c_SetBasisKanaele) or
           (KanalTyp in [c_FormelSumme,c_VnKanal,c_EnergieKanal])
end;

//------------------------------------------------------------------------------
function TWicalStamm.ValidTypManuellerMDEkanal(Kanal:TWicalKanalSatz):boolean;
begin
  result:=(Kanal.Formel = c_FormelManuell) and
          (GetManuellFunktion(Kanal.AuswerteArt) = c_AwArt_MDE)
end;

//------------------------------------------------------------------------------
function TWicalStamm.ValidTypVnBerechnen(Kanal:TWicalKanalSatz):boolean;
begin
  result:= (Kanal.Formel in c_SetBasisKanaele) and (Kanal.KanTyp ='V')
end;

//------------------------------------------------------------------------------
function TWicalStamm.ValidTypEnergieBerechnen(Kanal:TWicalKanalSatz):boolean;
begin
  result:= ((Kanal.Formel in c_SetBasisKanaele) or (Kanal.Formel = c_VnKanal)) and
           (Kanal.KanTyp = 'V')
end;

//------------------------------------------------------------------------------
function TWicalStamm.ValidTypBrennwertBerechnen(Kanal:TWicalKanalSatz):boolean;
begin
  result:= (Kanal.Formel in c_SetBasisKanaele) and
           (Kanal.KanTyp = 'A')
end;

//------------------------------------------------------------------------------
function TWicalStamm.ValidTypZZahlBerechnen(Kanal:TWicalKanalSatz):boolean;
begin
  result:=(Kanal.KanTyp <> 'Z')
//  result:=(Kanal.Formel in c_SetBasisKanaele) and (Kanal.KanTyp ='V')
end;

//------------------------------------------------------------------------------
function TWicalStamm.OpenTableFormel(KanalID:integer):integer;
begin
  with LocForQuery do begin
    try
    sql.Text:='select * from '+c_WCStammKanFor;
    sql.add('where  '+C_AwKanFor_AwkLfdNr+' = '+intTostr(KanalID));
    open;
    if eof then
      result:=0
    else
    result:=recordCount
    except
      result:=0
    end
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetFormelKanal(Summand:TWicalKanalSatz):boolean;
begin
  if LocForQuery.Eof then
    result:=false
  else with LocForQuery do begin
    summand.KanID:=fieldByName(C_AwKanFor_AwkLfdNr).AsInteger;
    summand.KanalDaten.Operand:=fieldByName(C_AwKanFor_Operand).AsString;
    summand.KanalDaten.FormelNr:=fieldByName(C_AwKanFor_FormelNr).AsInteger;
    summand.KanalDaten.OperandKanalNr:=fieldByName(C_AwKanFor_KanalNr).AsInteger;
    next;
    result:=true
  end
end;

//------------------------------------------------------------------------------
procedure TWicalStamm.GetEinheitenListe(EinhListe:TStringList);
begin
  with LocalQuery do begin
    sql.text:='select '+C_Awkan_Einheit+' from '+c_WCStammKan;
    sql.add('order by '+C_Awkan_Einheit);
    open;
    while not eof do begin
      EinhListe.Add(fields[0].AsString);
      next
    end
  end;
end;


//------------------------------------------------------------------------------
//  Skalierung Standardprotokolle

//------------------------------------------------------------------------------
function TWicalStamm.OpenSkalQueryOrderByName(Kategorie:integer):TQuery;
begin
  with SkalQuery do begin
    requestLive:=false;
    sql.clear;
    sql.text:='select * from '+c_WCStammSkalierung;
    sql.Add('where '+c_Skal_Kategorie+' = :'+c_Skal_Kategorie);
    sql.add('order by '+c_Skal_Name);
    paramByName(c_Skal_Kategorie).AsInteger:=Kategorie;
    open;
    result:=SkalQuery
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.OpenSkalQueryOrderByName:TQuery;
begin
  with SkalQuery do begin
    requestLive:=false;
    sql.clear;
    sql.text:='select * from '+c_WCStammSkalierung;
    sql.add('order by '+c_Skal_Name);
    open;
    result:=SkalQuery
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.OpenSkalQuery:TQuery;
begin
  with SkalQuery do begin
    requestLive:=true;
    sql.clear;
    sql.text:='select * from '+c_WCStammSkalierung;
    open;
    edit;
    result:=SkalQuery
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetSkalierungQuery(SkalId:Integer):TQuery;
begin
  with SkalQuery do begin
    sql.clear;
    sql.text:='select * from '+c_WCStammSkalierung;
    sql.add('where '+c_Skal_ID+' = '+inttostr(SkalId));
    open;
    if eof then
      result:=nil
    else
      result:=SkalQuery
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetSkalierung(FormatIndex:Integer; var Skalierung:RecSkalierung):boolean;
var FormatQuery : TQuery;
begin
  FormatQuery:=GetSkalierungQuery(FormatIndex);
  if assigned(FormatQuery) then with FormatQuery do begin
    Skalierung.SkalId:=fieldByName(c_Skal_ID).AsInteger;
    Skalierung.SkalName:=fields[1].value;
    Skalierung.MPunten:=fields[2].value;
    Skalierung.MPoben:=fields[3].value;
    Skalierung.Tagunten:=fields[4].value;
    Skalierung.Tagoben:=fields[5].value;
    Skalierung.Monatunten:=fields[6].value;
    Skalierung.Monatoben:=fields[7].value;
    Skalierung.Kategorie:=fieldByName(c_Skal_Kategorie).AsInteger;
    Skalierung.Increment:=fieldByName(c_Skal_Increment).AsFloat;
    result:=true;
    close;
  end
  else
    result:=false
end;

//------------------------------------------------------------------------------
procedure TWicalStamm.DeleteSkalierung(SkalId:integer);
begin
  with SkalQuery do begin
    RequestLive:=true;
    sql.Text:='delete from '+c_WCStammSkalierung;
    sql.add('where '+c_Skal_ID+' = '+inttostr(SkalID));
    ExecSql;
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.InsertSkalierung(Skalierung:RecSkalierung):integer;
var DbAutoInc : TDbAutoInc;
begin
  result:=0;
  DbAutoInc:=TDbAutoInc.Create(PathServer.PathName[WWicalStammDb]);
  with SkalQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_WCStammSkalierung;
    open;
    edit;
    Skalierung.SkalId:=DbAutoInc.NextIndex[c_WCStammSkalierung];
    insertRecord([Skalierung.SkalId,
                  Skalierung.SkalName,
                  Skalierung.MpUnten,
                  Skalierung.MpOben,
                  Skalierung.TagUnten,
                  Skalierung.TagOben,
                  Skalierung.MonatUnten,
                  Skalierung.MonatOben,
                  Skalierung.Kategorie,
                  Skalierung.Increment]);
    close;
  end;
  DbAutoInc.Free;
end;

//------------------------------------------------------------------------------
function TWicalStamm.WriteSkalierung(Skalierung:RecSkalierung):boolean;
begin
  result:=false;
  with SkalQuery do begin
    result:=false;
    RequestLive:=true;
    sql.text:='select * from '+c_WCStammSkalierung;
    sql.add('where '+c_Skal_ID+' = '+inttostr(Skalierung.SkalId));
    open;
    if not eof then begin
      edit;
      fields[1].value:=Skalierung.SkalName;
      fields[2].value:=Skalierung.MpUnten;
      fields[3].value:=Skalierung.MpOben;
      fields[4].value:=Skalierung.TagUnten;
      fields[5].value:=Skalierung.TagOben;
      fields[6].value:=Skalierung.MonatUnten;;
      fields[7].value:=Skalierung.MonatOben;
      fields[8].value:=Skalierung.Kategorie;
      fields[9].value:=Skalierung.Increment;
      post;
      result:=true
    end
  end
end;


//------------------------------------------------------------------------------
// Alle Einträge für den Kanal "KanUD" aus der Kanal-Text-Tabelle löschen
procedure TWicalStamm.DeleteKanalTextAlles(kanalID:Integer);
begin
  with LocalQuery do begin
    sql.clear;
    sql.text:='delete from '+c_WCStammKanText;
    sql.add('where '+c_AwKanText_ID+' ='+inttoStr(KanalId));
    ExecSql;
  end
end;

procedure TWicalStamm.DeleteKanalTextKonfig(kanalID:Integer);
begin
  with LocalQuery do begin
    sql.clear;
    sql.text:='delete from '+c_WCStammKanText;
    sql.add('where '+c_AwKanText_ID+' ='+inttoStr(KanalId));
    sql.add('and '+c_AwKanText_FktID+' < '+inttoStr(c_KanalText_FktID_ImportIniDateiSection));
    ExecSql;
  end
end;

procedure TWicalStamm.DeleteKanalText(kanalID,FktId:Integer);
begin
  with LocalQuery do begin
    sql.clear;
    sql.text:='delete from '+c_WCStammKanText;
    sql.add('where '+c_AwKanText_ID+' ='+inttoStr(KanalId));
    sql.add('and '+c_AwKanText_FktID+' = '+inttoStr(FktID));
    ExecSql;
  end
end;

//------------------------------------------------------------------------------
procedure TWicalStamm.InsertKanalText(kanalID,FktID:Integer; KText:String);
begin
  with LocalQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_WCStammKanText;
    open;
    AppendRecord([kanalID,FktID,KText]);
    RequestLive:=false;
    close;
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetKanalText(kanalID,FktID:integer):string;
begin
  with LocalQuery do begin
    sql.text:='select * from '+c_WCStammKanText;
    sql.add('where '+c_AwKanText_ID+' ='+inttoStr(KanalId));
    sql.add('and '+c_AwKanText_FktID+' ='+inttoStr(FktID));
    open;
    if not eof then
      result:=FieldByName(c_AwKanText_Text).AsString
    else
      result:='';
    close
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetKanalTextToInteger(kanalID,FktID:integer;var Resultat:integer):boolean;
begin
  with LocalQuery do begin
    sql.text:='select * from '+c_WCStammKanText;
    sql.add('where '+c_AwKanText_ID+' ='+inttoStr(KanalId));
    sql.add('and '+c_AwKanText_FktID+' ='+inttoStr(FktID));
    open;
    if not eof then begin
      try
      Resultat:=StrtoInt(FieldByName(c_AwKanText_Text).AsString);
      result:=true;
      except
        Resultat:=0;
        result:=false
      end
    end
    else
      result:=false;
    close
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetKanalTextName(kanal:TWicalKanalSatz;FktID:integer):string;
begin
  with LocalQuery do begin
    sql.text:='select * from '+c_WCStammKanText;
    sql.add('where '+c_AwKanText_ID+' ='+inttoStr(Kanal.KanID));
    sql.add('and '+c_AwKanText_FktID+' ='+inttoStr(FktID));
    open;
    if not eof then
      result:=FieldByName(c_AwKanText_Text).AsString
    else
      result:=kanal.KanName;
    close
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetKanalTextToDouble(kanalId,FktID:integer; var Wert:double):boolean;
var StrWert : string;
begin
  StrWert:=GetKanalText(kanalId,FktID);
  if StrWert='' then
    result:=false
  else
    if IsValid(StrWert) then begin
      wert:=StringToDouble(StrWert);
      result:=true;
    end
    else
      result:=false
end;

//------------------------------------------------------------------------------
procedure TWicalStamm.WriteExportMarke(kanalID,FktID:Integer; Datum:TDateTime);
begin
  DeleteExportMarke(kanalID,FktID);
  with LocalQuery do begin
    RequestLive:=true;
    sql.Text:='select * from '+c_WCExportMarken;
    open;
    AppendRecord([kanalID,FktID,Datum]);
    RequestLive:=false;
    close;
  end
end;

//------------------------------------------------------------------------------
function TWicalStamm.GetExportMarke(kanalID,FktID:integer):TDateTime;
begin
  with LocalQuery do begin
    sql.text:='select * from '+c_WCExportMarken;
    sql.add('where '+c_em_KanalID+' ='+inttoStr(KanalId));
    sql.add('and '+c_em_ExportTyp+' ='+inttoStr(FktID));
    open;
    if not eof then
      result:=FieldByName(c_em_LastDate).AsDateTime
    else
      result:=0;
    close
  end
end;

//------------------------------------------------------------------------------
procedure TWicalStamm.DeleteExportMarke(kanalID,FktID:Integer);
begin
  with LocalQuery do begin
    sql.clear;
    sql.text:='delete from '+c_WCExportMarken;
    sql.add('where '+c_em_KanalID+' ='+inttoStr(KanalId));
    sql.add('and '+c_em_ExportTyp+' ='+inttoStr(FktID));
    ExecSql;
  end
end;



//==============================================================================
//------------------------------------------------------------------------------
Function IsStoermengenKanal(Kanal:TWicalKanalSatz):boolean;
begin
  result:=(Kanal.Formel = c_DSfGKanal) and (Kanal.AuswerteArt and c_AwArt_SZ <> 0);
end;

//------------------------------------------------------------------------------
Function GetManuellFunktion(AwArt:integer):integer;
begin
  result:=(AwArt and $0000F000) shr 12;
end;

//------------------------------------------------------------------------------
// Summe oder Mittelwert
Function IsSummenFormelKanal(Kanal:TWicalKanalSatz):boolean;
begin
  result:=(Kanal.Formel = c_FormelSumme) and
          ((Kanal.AuswerteArt and c_AwArt_HZ <> 0) or (Kanal.AuswerteArt and c_AwArt_SZ <> 0));
end;

//------------------------------------------------------------------------------
// Excel-Formel
Function IsExcelFormelKanal(Kanal:TWicalKanalSatz):boolean;
begin
  result:=(Kanal.Formel = c_FormelSumme) and (Kanal.AuswerteArt and c_AwArt_MPArchiv <> 0);
end;

//------------------------------------------------------------------------------
// GERG
Function IsGERGFormelKanal(Kanal:TWicalKanalSatz):boolean;
begin
  result:=(Kanal.Formel = c_FormelSumme) and (Kanal.AuswerteArt and c_AwArt_FormelGERG <> 0);
end;

//------------------------------------------------------------------------------
Function IsKonstanteKanal(Kanal:TWicalKanalSatz):boolean;
begin
  result:=(Kanal.Formel = c_FormelManuell) and
          (GetManuellFunktion(Kanal.AuswerteArt) and  c_AwArt_KorrWert = 0)
end;

//------------------------------------------------------------------------------
Function IsMdeKanal(Kanal:TWicalKanalSatz):boolean;
begin
  result:=(Kanal.Formel = c_FormelManuell) and (GetManuellFunktion(Kanal.AuswerteArt) = c_AwArt_MDE);
end;

//------------------------------------------------------------------------------
Function IsMdeMst(Kanal:TWicalKanalSatz):boolean;
begin
  if WicalStamm.GetMstDaten(Kanal.Mstid) then
    result:=WicalStamm.MstStammSatz.MstMDE
  else
    result:=false
end;

//------------------------------------------------------------------------------
Function IsNotMdeKanal(KanalNode:TTreeNode):boolean;
begin
  result:=(KanalNode <> nil) and
          (TWicalObj(KanalNode.data).ObjTyp = OT_Kanal) and
           not IsMdeKanal(KanalNode.data)
end;

//------------------------------------------------------------------------------
Function IsImportKanal(Kanal:TWicalKanalSatz):boolean;
begin
  result:=(Kanal.Formel = c_FormelManuell) and (GetManuellFunktion(Kanal.AuswerteArt) = c_AwArt_Import);
end;

//------------------------------------------------------------------------------
procedure SchreibeProgAtt(ProgId,Attribut,von,bis:integer);
var ProgRegionTable : TProgRegionTable;
begin
  ProgRegionTable:=TProgRegionTable.create;
  try
    ProgRegionTable.InsertAttribut(ProgId,Attribut,von,bis)
  except
    FreeAndNil(ProgRegionTable);
  end;
end;

//------------------------------------------------------------------------------
procedure LeseProgAtt(ProgId,Attribut: integer; var von,bis:integer);
var ProgRegionTable : TProgRegionTable;
begin
  ProgRegionTable:=TProgRegionTable.create;
  try
    ProgRegionTable.GetAttribute(ProgId,Attribut,von,bis)
  except
    FreeAndNil(ProgRegionTable);
  end;
end;

//------------------------------------------------------------------------------
function IsProgAtt(ProgId,Attribut: integer):boolean;
var ProgRegionTable : TProgRegionTable;
begin
  ProgRegionTable:=TProgRegionTable.create;
  try
    result:=ProgRegionTable.GetAttribute(ProgId,Attribut)
  except
    FreeAndNil(ProgRegionTable);
  end;
end;

end.


