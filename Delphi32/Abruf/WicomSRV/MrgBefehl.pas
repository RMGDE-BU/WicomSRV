{******************************************************************************}
{* Unit: MRG-Befehle bilden                                                   *}
{* 19.12.2000 WW  Neu                                                         *}
{* 06.08.2021 WW  Ressourcedaten aus Liste statt Datei lesen                  *}
{******************************************************************************}
unit MrgBefehl;

interface

uses
  Windows, Classes, SysUtils, DateUtils, WChars, WStrUtils, T_BinMask, UnixDT,
  MResMrg, WSyscon, ModbusUtil;

type
  { Record mit Informationen f�r MRG-Datenabruf-Befehl }
  TAbrufRec = record
    KanalMaske : string;
    AlleDaten : Boolean;
    vonJahr : Word;
    vonMonat : Word;
    vonTag : Word;
    vonStunde : Word;
    vonMinute : Word;
    vonSekunde : Word;
    bisJahr : Word;
    bisMonat : Word;
    bisTag : Word;
    bisStunde : Word;
    bisMinute : Word;
    bisSekunde : Word;
  end;

  { Archivtypen f�r Ger�te mit Modbusabruf }
  TArchivtyp = (at_Parameterarchiv_eichamtlich,  // -> Meldungen (Parameter�nderungen)
                at_Parameterarchiv_nichteichamtlich,  // -> Meldungen (Parameter�nderungen)
                at_Ereignisarchiv,  // -> Meldungen
                at_Periodenarchiv,  // -> Messwerte
                at_Parameterarchiv_Gas  // -> Meldungen (Parameter�nderungen)
               );

const
  { Kennzahlen f�r den Messwertabruf von Ger�ten des Typs Datacon FWU (Lastprofile): }
  FWU_MessAbruf_Kennzahlen: array [1..16] of string = (
    '7-1z:V.b.p',           { Vb Z, Tarifger�t 1 }
    '7-1m:V.b.p',           { Vb MU, Tarifger�t 1 }
    '7-1m:V.n.p',           { Vn MU, Tarifger�t 1 }
    '7-1j:V.b.p',           { Vb Z Impuls, FWU MS 1 }
    '7-1i:V.b.p',           { Vb MU Impuls, FWU MS 1 }
    '7-1i:V.n.p',           { Vn MU Impuls, FWU MS 1 }
    '7-1X:T.0.p',           { Temperatur, FWU MS 1 }
    '7-1X:P.0.p',           { Druck, FWU MS 1 }
    '7-2z:V.b.p',           { Vb Z, Tarifger�t 2 }
    '7-2m:V.b.p',           { Vb MU, Tarifger�t 2 }
    '7-2m:V.n.p',           { Vn MU, Tarifger�t 2 }
    '7-2j:V.b.p',           { Vb Z Impuls, FWU MS 2 }
    '7-2i:V.b.p',           { Vb MU Impuls, FWU MS 2 }
    '7-2i:V.n.p',           { Vn MU Impuls, FWU MS 2 }
    '7-2X:T.0.p',           { Temperatur, FWU MS 2 }
    '7-2X:P.0.p');          { Druck, FWU MS 2 }

  { Kennzahlen f�r den Tagessatzabruf von Ger�ten des Typs Datacon FWU (Tages- bzw. Revisionswerte): }
  FWU_TagAbruf_Kennzahlen: array [1..10] of string = (
    '7-1M:V.b.t',           { Vb, MU1 }
    '7-1M:V.n.t',           { Vn, MU1 }
    '7-1z:V.b.t',           { Vb Z, Tarifger�t 1 }
    '7-1m:V.b.t',           { Vb MU, Tarifger�t 1 }
    '7-1m:V.n.t',           { Vn MU, Tarifger�t 1 }
    '7-2M:V.b.t',           { Vb, MU2 }
    '7-2M:V.n.t',           { Vn, MU2 }
    '7-2z:V.b.t',           { Vb Z, Tarifger�t 2 }
    '7-2m:V.b.t',           { Vb MU, Tarifger�t 2 }
    '7-2m:V.n.t');          { Vn MU, Tarifger�t 2 }

  { Kanaldefinitionen f�r Modbus-Register: }
  C_MBKanalDef_DZ         = 'DZ';       // Datum-/Zeitinformation
  C_MBKanalDef_ONr        = 'ONr';      // Ordnungsnummer
  C_MBKanalDef_SatzStatus = 'SStatus';  // Satz-Status
  C_MBKanalDef_Faktor     = 'Faktor';   // Faktor f�r Werte-Verrechnung
  C_MBKanalDef_TypNr      = 'TypNr';    // Typnummer
  C_MBKanalDef_IdentNr    = 'IdentNr';  // Identifizierungsnummer
  C_MBKanalDef_WertAlt    = 'WertAlt';  // Alter Parameterwert
  C_MBKanalDef_WertNeu    = 'WertNeu';  // Neuer Parameterwert


  { Primus: -------------------------------------------------------------------}
  { Modbus-Register f�r den Messwertabruf von Ger�ten des Typs Primus (Datenarchiv): }
  Primus_MBRegisterDef_Mess: array [1..24] of TMBRegisterDef = (
    (StartAdresse: 37001; Typ: 'E'; AnzahlBytes: 8; FktCode: 3; KanalDef: '1'),   // Normvolumen
    (StartAdresse: 37005; Typ: 'E'; AnzahlBytes: 8; FktCode: 3; KanalDef: '3'),   // Originalvolumen
    (StartAdresse: 37009; Typ: 'E'; AnzahlBytes: 8; FktCode: 3; KanalDef: '2'),   // Betriebsvolumen
    (StartAdresse: 37013; Typ: 'F'; AnzahlBytes: 4; FktCode: 3; KanalDef: '8'),   // Gasdruck
    (StartAdresse: 37015; Typ: 'F'; AnzahlBytes: 4; FktCode: 3; KanalDef: '9'),   // Gastemperatur
    (StartAdresse: 37017; Typ: 'F'; AnzahlBytes: 4; FktCode: 3; KanalDef: '10'),  // Zustandszahl
    (StartAdresse: 37019; Typ: 'F'; AnzahlBytes: 4; FktCode: 3; KanalDef: '11'),  // K-Zahl
    (StartAdresse: 37021; Typ: 'F'; AnzahlBytes: 4; FktCode: 3; KanalDef: '12'),  // Brennwert
    (StartAdresse: 37023; Typ: 'E'; AnzahlBytes: 8; FktCode: 3; KanalDef: '5'),   // Normvolumen St�rung
    (StartAdresse: 37027; Typ: 'E'; AnzahlBytes: 8; FktCode: 3; KanalDef: '6'),   // Betriebsvolumen St�rung
    (StartAdresse: 37031; Typ: 'E'; AnzahlBytes: 8; FktCode: 3; KanalDef: '4'),   // Energie
    (StartAdresse: 37035; Typ: 'E'; AnzahlBytes: 8; FktCode: 3; KanalDef: '7'),   // Energie St�rung
    (StartAdresse: 37039; Typ: 'E'; AnzahlBytes: 8; FktCode: 3; KanalDef: ''),    // Vb Stundenmenge, wird nicht archiviert
    (StartAdresse: 37043; Typ: 'E'; AnzahlBytes: 8; FktCode: 3; KanalDef: ''),    // Vn Stundenmenge, wird nicht archiviert
    (StartAdresse: 37047; Typ: 'F'; AnzahlBytes: 4; FktCode: 3; KanalDef: '15'),  // Innentemperatur
    (StartAdresse: 37049; Typ: 'F'; AnzahlBytes: 4; FktCode: 3; KanalDef: '16'),  // Batteriespannung Umwerter
    (StartAdresse: 37051; Typ: 'F'; AnzahlBytes: 4; FktCode: 3; KanalDef: '17'),  // Batteriekapazit�t Umwerter
    (StartAdresse: 37053; Typ: 'F'; AnzahlBytes: 4; FktCode: 3; KanalDef: '18'),  // Batteriespannung Modem
    (StartAdresse: 37055; Typ: 'F'; AnzahlBytes: 4; FktCode: 3; KanalDef: '19'),  // Batteriekapazit�t Modem
    (StartAdresse: 37057; Typ: 'F'; AnzahlBytes: 4; FktCode: 3; KanalDef: '20'),  // GSM-Signalst�rke
    (StartAdresse: 37059; Typ: 'F'; AnzahlBytes: 4; FktCode: 3; KanalDef: '13'),  // Durchfluss
    (StartAdresse: 37061; Typ: 'F'; AnzahlBytes: 4; FktCode: 3; KanalDef: '14'),  // Normdurchfluss
    (StartAdresse: 37063; Typ: 'LL'; AnzahlBytes: 8; FktCode: 3; KanalDef: C_MBKanalDef_SatzStatus),  // Ger�te-Status
    (StartAdresse: 37067; Typ: 'DT2'; AnzahlBytes: 6; FktCode: 3; KanalDef: C_MBKanalDef_DZ));  // Zeitstempel

  C_Primus_MBRegisterSize_Mess  =  69;  { Register-Gr��e eines Datensatzes des Datenarchivs }
  C_Primus_MBRegisterCount_Mess = 360;  { Gesamte Anzahl der Datens�tze des Datenarchivs }

  { Modbus-Register f�r den Meldungsabruf von Ger�ten des Typs Primus (Statusarchiv):
    -> Anmerkung: Vn, Vo und E werden f�r Meldungen nicht ben�tigt, werden aber
       trotzdem mitgelesen, um mit einem Modbus-Request mehrere Archivdatens�tze
       zusammen auslesen zu k�nnen (Zeitgewinn beim Lesen !)}
  Primus_MBRegisterDef_Meld: array [1..5] of TMBRegisterDef = (
    (StartAdresse: 1001; Typ: 'E'; AnzahlBytes: 8; FktCode: 3; KanalDef: ''),  // Normvolumen, f�r Meldungen nicht ben�tigt
    (StartAdresse: 1005; Typ: 'E'; AnzahlBytes: 8; FktCode: 3; KanalDef: ''),  // Volumen original, f�r Meldungen nicht ben�tigt
    (StartAdresse: 1009; Typ: 'E'; AnzahlBytes: 8; FktCode: 3; KanalDef: ''),  // Energie, f�r Meldungen nicht ben�tigt
    (StartAdresse: 1013; Typ: 'LL'; AnzahlBytes: 8; FktCode: 3; KanalDef: C_MBKanalDef_SatzStatus),  // Ger�te-Status
    (StartAdresse: 1017; Typ: 'DT2'; AnzahlBytes: 6; FktCode: 3; KanalDef: C_MBKanalDef_DZ));  // Zeitstempel

  C_Primus_MBRegisterSize_Meld  =  19;  { Register-Gr��e eines Datensatzes des Statusarchivs }
  C_Primus_MBRegisterCount_Meld = 200;  { Gesamte Anzahl der Datens�tze des Statusarchivs }

  { Modbus-Register f�r den Parameterabruf von Ger�ten des Typs Primus:
    -> Neue Zuordnung; 22.07.2019, WW
    -> Registerdefinitionen aus Ressourcendatei ParamMrg.dat; 11.02.2022, WW }

  { Prilog: -------------------------------------------------------------------}
  { -> Registerdefinitionen f�r Archive ausgelagert in Ressourcendatei MBAbruf.dat; 11.02.2022, WW 
    -> Registerdefinitionen f�r Parameter aus Ressourcendatei ParamMrg.dat; 11.02.2022, WW }

  { TME400, RSM200: -----------------------------------------------------------}
  { Modbus-Register f�r den Archivheaderabruf von Ger�ten des Typs TME400,
    RSM200: }
  TME400_MBRegisterDef_Archivheader: array [1..16] of TMBRegisterDef = (
    // Header 0
    (StartAdresse:  0; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // Ordnungsnummer
    (StartAdresse:  1; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // Index �ltester Eintrag
    (StartAdresse:  2; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // Index neuester Eintrag
    (StartAdresse:  3; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // CRC16
    // Header 1
    (StartAdresse:  4; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // Ordnungsnummer
    (StartAdresse:  5; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // Index �ltester Eintrag
    (StartAdresse:  6; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // Index neuester Eintrag
    (StartAdresse:  7; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // CRC16
    // Header 2
    (StartAdresse:  8; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // Ordnungsnummer
    (StartAdresse:  9; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // Index �ltester Eintrag
    (StartAdresse: 10; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // Index neuester Eintrag
    (StartAdresse: 11; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // CRC16
    // Header 3
    (StartAdresse: 12; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // Ordnungsnummer
    (StartAdresse: 13; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // Index �ltester Eintrag
    (StartAdresse: 14; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),   // Index neuester Eintrag
    (StartAdresse: 15; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''));  // CRC16

  { TME400: -------------------------------------------------------------------}
  { Modbus-Register f�r den Messwertabruf von Ger�ten des Typs TME400-VCF (Periodenarchiv): }
  TME400_VCF_MBRegisterDef_Mess: array [1..12] of TMBRegisterDef = (
    (StartAdresse:  0; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_ONr),  // Ordnungsnummer
    (StartAdresse:  1; Typ: 'U'; AnzahlBytes: 4; FktCode: 20; KanalDef: C_MBKanalDef_DZ),   // Zeitstempel (Unixzeit)
    (StartAdresse:  3; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: '1'),   // Normvolumen
    (StartAdresse:  5; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: '2'),   // Betriebsvolumen
    (StartAdresse:  7; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: '3'),   // Normvolumen St�rung
    (StartAdresse:  9; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: '4'),   // Betriebsvolumen St�rung
    (StartAdresse: 11; Typ: 'I'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_Faktor),  // Exponent (zur Basis 10)
    (StartAdresse: 12; Typ: 'F'; AnzahlBytes: 4; FktCode: 20; KanalDef: '5'),   // Druck
    (StartAdresse: 14; Typ: 'F'; AnzahlBytes: 4; FktCode: 20; KanalDef: '6'),   // Gastemperatur
    (StartAdresse: 16; Typ: 'F'; AnzahlBytes: 4; FktCode: 20; KanalDef: '7'),   // Kompressibilit�t
    (StartAdresse: 18; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_SatzStatus),  // Status
    (StartAdresse: 19; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''));  // CRC16, wird nicht ausgewertet

  { Modbus-Register f�r den Messwertabruf von Ger�ten des Typs TME400-VMF (Periodenarchiv): }
  TME400_VMF_MBRegisterDef_Mess: array [1..12] of TMBRegisterDef = (
    (StartAdresse:  0; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_ONr),  // Ordnungsnummer
    (StartAdresse:  1; Typ: 'U'; AnzahlBytes: 4; FktCode: 20; KanalDef: C_MBKanalDef_DZ),   // Zeitstempel (Unixzeit)
    (StartAdresse:  3; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: ''),    // Normvolumen, wird nicht archiviert
    (StartAdresse:  5; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: '1'),   // Betriebsvolumen
    (StartAdresse:  7; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: ''),    // Normvolumen St�rung, wird nicht archiviert
    (StartAdresse:  9; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: '2'),   // Betriebsvolumen St�rung
    (StartAdresse: 11; Typ: 'I'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_Faktor),  // Exponent (zur Basis 10)
    (StartAdresse: 12; Typ: 'F'; AnzahlBytes: 4; FktCode: 20; KanalDef: ''),    // Druck, wird nicht archiviert
    (StartAdresse: 14; Typ: 'F'; AnzahlBytes: 4; FktCode: 20; KanalDef: ''),    // Gastemperatur, wird nicht archiviert
    (StartAdresse: 16; Typ: 'F'; AnzahlBytes: 4; FktCode: 20; KanalDef: ''),    // Kompressibilit�t, wird nicht archiviert
    (StartAdresse: 18; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_SatzStatus),  // Status
    (StartAdresse: 19; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''));   // CRC16, wird nicht ausgewertet

  C_TME400_MBRegisterSize_Mess  =   20;  { Register-Gr��e eines Datensatzes des Periodenarchivs }
  C_TME400_MBRegisterCount_Mess = 9000;  { Gesamte Anzahl der Datens�tze des Periodenarchivs }

  { Modbus-Register f�r den Ereignisarchivabruf von Ger�ten des Typs TME400 (Meldungen): }
  TME400_MBRegisterDef_Ereignis: array [1..5] of TMBRegisterDef = (
    (StartAdresse: 0; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_ONr),  // Ordnungsnummer
    (StartAdresse: 1; Typ: 'U'; AnzahlBytes: 4; FktCode: 20; KanalDef: C_MBKanalDef_DZ),   // Zeitstempel (Unixzeit)
    (StartAdresse: 3; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_TypNr),  // Ereignistyp
    (StartAdresse: 4; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_IdentNr),  // Ereignisnummer
    (StartAdresse: 5; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''));  // CRC16, wird nicht ausgewertet

  C_TME400_MBRegisterSize_Ereignis  =   6;  { Register-Gr��e eines Datensatzes des Ereignisarchivs }
  C_TME400_MBRegisterCount_Ereignis = 200;  { Gesamte Anzahl der Datens�tze des Ereignisarchivs }

  { Modbus-Register f�r den Abruf von Parameter�nderungen von Ger�ten des Typs TME400 (Meldungen): }
  TME400_MBRegisterDef_ParaAend: array [1..6] of TMBRegisterDef = (
    (StartAdresse:  0; Typ: 'W'; AnzahlBytes:  2; FktCode: 20; KanalDef: C_MBKanalDef_ONr),  // Ordnungsnummer
    (StartAdresse:  1; Typ: 'U'; AnzahlBytes:  4; FktCode: 20; KanalDef: C_MBKanalDef_DZ),   // Zeitstempel (Unixzeit)
    (StartAdresse:  3; Typ: 'W'; AnzahlBytes:  2; FktCode: 20; KanalDef: C_MBKanalDef_IdentNr),  // Koordinate
    (StartAdresse:  4; Typ: 'N'; AnzahlBytes: 12; FktCode: 20; KanalDef: C_MBKanalDef_WertAlt),  // Alter Parameterwert
    (StartAdresse: 10; Typ: 'N'; AnzahlBytes: 12; FktCode: 20; KanalDef: C_MBKanalDef_WertNeu),  // Neuer Parameterwert
    (StartAdresse: 16; Typ: 'W'; AnzahlBytes:  2; FktCode: 20; KanalDef: ''));  // CRC16, wird nicht ausgewertet

  C_TME400_MBRegisterSize_ParaAend  =  17;  { Register-Gr��e eines Datensatzes des Parameterarchivs }
  C_TME400_MBRegisterCount_ParaAend = 300;  { Gesamte Anzahl der Datens�tze des Parameterarchivs }

  { Modbus-Register f�r den Parameterabruf von Ger�ten des Typs TME400: }
  TME400_MBRegisterDef_Para_Uhrzeit: TMBRegisterDef =
    (StartAdresse: 714; Typ: 'N'; AnzahlBytes: 6);  { Ger�te-Uhrzeit }
  TME400_MBRegisterDef_Para_Codewort: TMBRegisterDef =
    (StartAdresse: 777; Typ: C_MBWertTyp_W; AnzahlBytes: 2);  { Ger�te-Codewort Freigabe }

  { RSM200: -------------------------------------------------------------------}
  { Modbus-Register f�r den Messwertabruf von Ger�ten des Typs RSM200-VCF (Periodenarchiv): }
  RSM200_VCF_MBRegisterDef_Mess: array [1..13] of TMBRegisterDef = (
    (StartAdresse:  0; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_ONr),  // Ordnungsnummer
    (StartAdresse:  1; Typ: 'U'; AnzahlBytes: 4; FktCode: 20; KanalDef: C_MBKanalDef_DZ),   // Zeitstempel (Unixzeit)
    (StartAdresse:  3; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: '1'),   // Normvolumen
    (StartAdresse:  5; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: '2'),   // Betriebsvolumen
    (StartAdresse:  7; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: '3'),   // Normvolumen St�rung
    (StartAdresse:  9; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: '4'),   // Betriebsvolumen St�rung
    (StartAdresse: 11; Typ: 'I'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_Faktor),  // Exponent (zur Basis 10)
    (StartAdresse: 12; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),    // Z�hlwerksstellen (Anzahl der Vor- und Nachkommastellen), wird nicht archiviert
    (StartAdresse: 13; Typ: 'F'; AnzahlBytes: 4; FktCode: 20; KanalDef: '5'),   // Druck
    (StartAdresse: 15; Typ: 'F'; AnzahlBytes: 4; FktCode: 20; KanalDef: '6'),   // Gastemperatur
    (StartAdresse: 17; Typ: 'F'; AnzahlBytes: 4; FktCode: 20; KanalDef: '7'),   // Kompressibilit�t
    (StartAdresse: 19; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_SatzStatus),  // Status
    (StartAdresse: 20; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''));   // CRC16, wird nicht ausgewertet

  { Modbus-Register f�r den Messwertabruf von Ger�ten des Typs RSM200-VMF (Periodenarchiv): }
  RSM200_VMF_MBRegisterDef_Mess: array [1..13] of TMBRegisterDef = (
    (StartAdresse:  0; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_ONr),  // Ordnungsnummer
    (StartAdresse:  1; Typ: 'U'; AnzahlBytes: 4; FktCode: 20; KanalDef: C_MBKanalDef_DZ),   // Zeitstempel (Unixzeit)
    (StartAdresse:  3; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: ''),    // Normvolumen, wird nicht archiviert
    (StartAdresse:  5; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: '1'),   // Betriebsvolumen
    (StartAdresse:  7; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: ''),    // Normvolumen St�rung, wird nicht archiviert
    (StartAdresse:  9; Typ: 'D'; AnzahlBytes: 4; FktCode: 20; KanalDef: '2'),   // Betriebsvolumen St�rung
    (StartAdresse: 11; Typ: 'I'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_Faktor),  // Exponent (zur Basis 10)
    (StartAdresse: 12; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''),    // Z�hlwerksstellen (Anzahl der Vor- und Nachkommastellen), wird nicht archiviert
    (StartAdresse: 13; Typ: 'F'; AnzahlBytes: 4; FktCode: 20; KanalDef: ''),    // Druck, wird nicht archiviert
    (StartAdresse: 15; Typ: 'F'; AnzahlBytes: 4; FktCode: 20; KanalDef: ''),    // Gastemperatur, wird nicht archiviert
    (StartAdresse: 17; Typ: 'F'; AnzahlBytes: 4; FktCode: 20; KanalDef: ''),    // Kompressibilit�t, wird nicht archiviert
    (StartAdresse: 19; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_SatzStatus),  // Status
    (StartAdresse: 20; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''));   // CRC16, wird nicht ausgewertet

  C_RSM200_MBRegisterSize_Mess  =   21;  { Register-Gr��e eines Datensatzes des Periodenarchivs }
  C_RSM200_MBRegisterCount_Mess = 8800;  { Gesamte Anzahl der Datens�tze des Periodenarchivs }

  { Modbus-Register f�r den Ereignisarchivabruf von Ger�ten des Typs RSM200 (Meldungen): }
  RSM200_MBRegisterDef_Ereignis: array [1..6] of TMBRegisterDef = (
    (StartAdresse:  0; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_ONr),  // Ordnungsnummer
    (StartAdresse:  1; Typ: 'U'; AnzahlBytes: 4; FktCode: 20; KanalDef: C_MBKanalDef_DZ),   // Zeitstempel (Unixzeit)
    (StartAdresse:  3; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_TypNr),  // Ereignistyp
    (StartAdresse:  4; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: C_MBKanalDef_IdentNr),  // Ereignisnummer
    (StartAdresse:  5; Typ: 'N'; AnzahlBytes: 6; FktCode: 20; KanalDef: ''),  // Ereignisinfo (Bin�rinformation), wird nicht archiviert
    (StartAdresse:  8; Typ: 'W'; AnzahlBytes: 2; FktCode: 20; KanalDef: ''));  // CRC16, wird nicht ausgewertet

  C_RSM200_MBRegisterSize_Ereignis  =   9;  { Register-Gr��e eines Datensatzes des Ereignisarchivs }
  C_RSM200_MBRegisterCount_Ereignis = 200;  { Gesamte Anzahl der Datens�tze des Ereignisarchivs }

  { Modbus-Register f�r den Abruf von Parameter�nderungen von Ger�ten des Typs RSM200 (Meldungen): }
  RSM200_MBRegisterDef_ParaAend: array [1..6] of TMBRegisterDef = (
    (StartAdresse:  0; Typ: 'W'; AnzahlBytes:  2; FktCode: 20; KanalDef: C_MBKanalDef_ONr),  // Ordnungsnummer
    (StartAdresse:  1; Typ: 'U'; AnzahlBytes:  4; FktCode: 20; KanalDef: C_MBKanalDef_DZ),   // Zeitstempel (Unixzeit)
    (StartAdresse:  3; Typ: 'W'; AnzahlBytes:  2; FktCode: 20; KanalDef: C_MBKanalDef_IdentNr),  // Koordinate
    (StartAdresse:  4; Typ: 'S'; AnzahlBytes: 12; FktCode: 20; KanalDef: C_MBKanalDef_WertAlt),  // Alter Parameterwert (Bin�r, typabh�ngig zu interpretieren)
    (StartAdresse: 10; Typ: 'S'; AnzahlBytes: 12; FktCode: 20; KanalDef: C_MBKanalDef_WertNeu),  // Neuer Parameterwert (Bin�r, typabh�ngig zu interpretieren)
    (StartAdresse: 16; Typ: 'W'; AnzahlBytes:  2; FktCode: 20; KanalDef: ''));  // CRC16, wird nicht ausgewertet

  C_RSM200_MBRegisterSize_ParaAend  =  17;  { Register-Gr��e eines Datensatzes des Parameterarchivs }
  C_RSM200_MBRegisterCount_ParaAend = 300;  { Gesamte Anzahl der Datens�tze des Parameterarchivs }

  { Modbus-Register f�r den Parameterabruf von Ger�ten des Typs RSM200: }
  RSM200_MBRegisterDef_Para_Uhrzeit: TMBRegisterDef =
    (StartAdresse: 714; Typ: 'N'; AnzahlBytes: 6);  { Ger�te-Uhrzeit }
  RSM200_MBRegisterDef_Para_ZeitDatumBestaetigen: TMBRegisterDef =
    (StartAdresse: 776; Typ: 'W'; AnzahlBytes: 2);  { Ger�te-Zeit/Datum best�tigen }
  RSM200_MBRegisterDef_Para_Codewort: TMBRegisterDef =
    (StartAdresse: 777; Typ: C_MBWertTyp_W; AnzahlBytes: 2);  { Ger�te-Codewort Freigabe }

  { FLOWSIC500: ---------------------------------------------------------------}
  { Modbus-Register f�r den Parameter-/Registerabruf von Ger�ten des Typs FLOWSIC500: }
  FLOWSIC500_MBRegisterDef_Para_UserID_Login: TMBRegisterDef =
    (StartAdresse: 3260; Typ: C_MBWertTyp_W; AnzahlBytes: 2);  { User-ID f�r Login }
  FLOWSIC500_MBRegisterDef_Para_Passwort_Login: TMBRegisterDef =
    (StartAdresse: 3261; Typ: C_MBWertTyp_D; AnzahlBytes: 4);  { Passwort f�r Login }
  FLOWSIC500_MBRegisterDef_Para_Logout: TMBRegisterDef =
    (StartAdresse: 3280; Typ: C_MBWertTyp_W; AnzahlBytes: 2);  { Logout }

  FLOWSIC500_MBRegisterDef_Cmd_SetMaintenanceMode: TMBRegisterDef =
    (StartAdresse: 3281; Typ: C_MBWertTyp_W; AnzahlBytes: 2);  { Konfigurationsmodus einschalten }
  FLOWSIC500_MBRegisterDef_Cmd_ResetMaintenanceMode: TMBRegisterDef =
    (StartAdresse: 3282; Typ: C_MBWertTyp_W; AnzahlBytes: 2);  { Konfigurationsmodus ausschalten }

  FLOWSIC500_MBRegisterDef_Para_SyncReferenzZeit: TMBRegisterDef =
    (StartAdresse: 4348; Typ: C_MBWertTyp_U; AnzahlBytes: 4);  { Referenzzeit f�r Synchronisation (UTC) }

  FLOWSIC500_MBRegisterDef_Para_DownloadAdresse: TMBRegisterDef =
    (StartAdresse: 6005; Typ: C_MBWertTyp_W; AnzahlBytes: 2);  { Download-Adresse f�r Archiv- und Logbuchdaten }
  FLOWSIC500_MBRegisterDef_Para_DownloadPuffer: TMBRegisterDef =
    (StartAdresse: 6006; Typ: C_MBWertTyp_Sle; AnzahlBytes: 250);  { Download-Puffer f�r Archiv- und Logbuchdaten }


procedure GetMRGKommando_Archiv (AbrufRec: TAbrufRec;
                                 KommandoTyp: string;
                                 Abrufkommando_Def: string;
                                 Abrufkommando_alle_Daten_Def: string;
                                 MrgTyp: integer;
                                 MrgKanalBitKonfigList: TMrgKanalBitKonfigList;
                                 var MRGKommando: string);
function GetMRGKommando_B (ParaNr: string): string;
function GetMRGKommando_C (ParaNr: string; NeuerWert: string): string;
function GetMRGKommando_k: string;

function GetLIS200Kommando_Archiv (AbrufRec: TAbrufRec; ArchivNr: integer): string;
function GetLIS200Kommando_Lesen (DatenAdresse: string): string;
function GetLIS200Kommando_Schreiben (DatenAdresse: string; NeuerWert: string): string;

function GetKE_Kommando (Kommandostr: string): string;
function GetKE_Kommando_Zeitbereich_von (AbrufRec: TAbrufRec): string;
function GetKE_Kommando_Zeitbereich_bis (AbrufRec: TAbrufRec): string;

function GetFWU_Kommando_Archiv (AbrufRec: TAbrufRec; KennzifferStr: string): string;

function GetTritschler_IECKommando_Standardabfrage (SSU_GerAuswNr: byte;
  Passwort: string): string;
function GetTritschler_IECKommando_05_AktUmwerterDaten (SSU_GerAuswNr: byte;
  Passwort: string): string;
function GetTritschler_IECKommando_20_ZeitSync (sNewUTC: string;
  SSU_GerAuswNr: byte; Passwort: string): string;
function GetTritschler_IECKommando_26_Logbuecher (AbrufRec: TAbrufRec;
  SSU_GerAuswNr: byte; Passwort: string): string;
function GetTritschler_IECKommando_27_Logbuecher_nicht_eichtechnisch (
  AbrufRec: TAbrufRec; SSU_GerAuswNr: byte; Passwort: string): string;
function GetTritschler_IECKommando_28_Logbuecher_eichtechnisch (
  AbrufRec: TAbrufRec; SSU_GerAuswNr: byte; Passwort: string): string;
function GetTritschler_IECKommando_29_Zaehlerstandsprofil (AbrufRec: TAbrufRec;
  SSU_GerAuswNr: byte; Passwort: string): string;
function GetTritschler_IECKommando_40_Quittung_GPRS: string;
function GetTritschler_IECKommando_42_ZeitSync_GPRS: string;
function GetTritschler_IECKommando_99_Komplettauslesung (AbrufRec: TAbrufRec;
  SSU_GerAuswNr: byte; Passwort: string): string;

function GetTTG_IECKommando_Lastprofil (AbrufRec: TAbrufRec; KanalNr: integer;
  SSU_GerAuswNr: byte; Passwort: string): string;

function GetTTG_FTLKommando_Geraeteinfo (KanalNr: integer): string;
function GetTTG_FTLKommando_Zaehlerstaende (KanalNr: integer): string;
function GetTTG_FTLKommando_Messperiodenwerte (KanalNr: integer): string;

function GetTritschler_FTLinternKommando (iBlockNr: word; sDaten: string;
  cRW: char): string;
function GetTritschler_FTLinternKommando_WritePasswort (sPasswort: string;
  iPasswortNr: word): string;

function GetSSUKommando_Login (Passwort: string): string;
function GetSSUKommando_Quittung (Modus: char): string;

function GetCorusSAMKommando_DatenspeicherLesen (AbrufRec: TAbrufRec;
  DatenspeicherID: byte): string;
function GetCorusSAMKommando_ParameterLesen (ParameterID: byte): string;
function GetCorusSAMKommando_ParameterGruppeLesen (slParameterID: TStrings): string;
function GetCorusSAMKommando_ParameterSchreiben (ParameterID: byte; NeuerWert: string): string;

function GetActaris_IEC1107Kommando_Archiv (AbrufRec: TAbrufRec;
  Kennzahl: string): string;

function GetKamstrup_IEC1107Kommando_Archiv (AbrufRec: TAbrufRec;
  Kennzahl: string): string;
function GetKamstrup_IEC1107Kommando_Logbuch (Kennzahl: string): string;

function GetIEC1107Kommando_Lesen (Kennzahl: string): string;
function GetIEC1107Kommando_Schreiben (Kennzahl, NeuerWert, Passwort: string): string;

function GetVeriboxMini_SMSKommando_Daten (vonZeit: TDateTime): string;

function GetDS100_LeseBefehl (Kommando: string): string;
function GetDS100_SchreibBefehl (Kommando: string): string;

function GetKanalDef_MBRegister_Archiv (AMrgTyp: integer; AArchivtyp: TArchivtyp;
  iStartAdresse: word; MBAbrufData: TMBAbrufData): string;
function GetStartAdresse_MBRegister_Archiv (AMrgTyp: integer; AArchivtyp: TArchivtyp;
  sKanalDef: string; iRecOffset: word; MBAbrufData: TMBAbrufData): word;
function GetMB_ArchivRecSize (AMrgTyp: integer; AArchivtyp: TArchivtyp;
  MBAbrufData: TMBAbrufData): word;
function GetMB_MaxArchivRecCount (AMrgTyp: integer; AArchivtyp: TArchivtyp;
  MBAbrufData: TMBAbrufData): word;
function GetMB_RequestFileNr_Archivheader (AMrgTyp: integer; AArchivtyp: TArchivtyp): word;
function GetMB_RequestFileNr_Archiv (AMrgTyp: integer; AArchivtyp: TArchivtyp): word;

implementation

{--------------------- Befehlsroutinen f�r Wieser-Ger�te ----------------------}

{----------------------------------------------------------------------------}
procedure CorrectAbrufRecY2000 (KommandoTyp: string; var AbrufRec: TAbrufRec);
{----------------------------------------------------------------------------}
{ Korrektur des Abrufzeitraums bei Jahrtausendwechsel;
  �bergabe: Kommandotyp-Zeichen
            AbrufRec
  R�ckgabe: korrigierter AbrufRec }
begin
  { Meldungen: }
  if KommandoTyp = 'M' then begin
    { bei Jahrtausendwechsel: alle Meldungen }
    if (AbrufRec.vonJahr < 2000) AND (AbrufRec.bisJahr >= 2000) then
      AbrufRec.AlleDaten := True;
  end;

  { Pr�fungss�tze: }
  if KommandoTyp = 'X' then begin
    { bei Jahrtausendwechsel: alle Pr�fungss�tze }
    if (AbrufRec.vonJahr < 2000) AND (AbrufRec.bisJahr >= 2000) then
      AbrufRec.AlleDaten := True;
  end;
end;

{---------------------------------------------------------------------}
function GetMaskString (Const AbrufRec: TAbrufRec; Const Maske: string;
  Const MrgTyp: integer; Const Kommandotyp: string;
  MrgKanalBitKonfigList: TMrgKanalBitKonfigList): string;
{---------------------------------------------------------------------}
Var
  W: Word;
  S: string;
  dt: TDateTime;
  C: cardinal;
  BitMaske: cardinal;
  i: integer;
  KanalNr: byte;
  MrgKanalBitMaskList: TMrgKanalBitMaskList;

Begin
  S:='';
  W := 0;
  If Length (Maske) > 0 Then Begin
    Case Maske [1] of
      'J', 'M', 'T', 'h', 'm', 's':
        Begin
          If Length (Maske) > 1 Then Begin
            Case Maske [2] of
              'v' : Begin
                      Case Maske [1] of
                        'J': W:=AbrufRec.vonJahr MOD 100;
                        'M': W:=AbrufRec.vonMonat;
                        'T': W:=AbrufRec.vonTag;
                        'h': W:=AbrufRec.vonStunde;
                        'm': W:=AbrufRec.vonMinute;
                        's': W:=AbrufRec.vonSekunde;
                      End;
                      S:=Format('%.2d', [W]);
                    End;
              'b' : Begin
                      Case Maske [1] of
                        'J': W:=AbrufRec.bisJahr MOD 100;
                        'M': W:=AbrufRec.bisMonat;
                        'T': W:=AbrufRec.bisTag;
                        'h': W:=AbrufRec.bisStunde;
                        'm': W:=AbrufRec.bisMinute;
                        's': W:=AbrufRec.bisSekunde;
                      End;
                      S:=Format('%.2d', [W]);
                    End;
            End;
          End;
        End;

      'K': Begin
             if Copy (Maske, 1, 2) = 'KB' then begin  // Kanalmaske �ber Bitpositon; 12.06.2009 WW
               MrgKanalBitMaskList:=TMrgKanalBitMaskList.Create;
               try
                 C:=0;
                 // Kanalbitmaskenliste aus Resourcenliste laden:
                 if MrgKanalBitMaskList.LoadFromKonfigList_ByMrgKommandoTyp (
                      MrgTyp, Kommandotyp, MrgKanalBitKonfigList) then begin  // 06.08.2021, WW
                   BitMaske:=1;
                   for i:=0 to MrgKanalBitMaskList.Count - 1 do begin
                     KanalNr:=TMrgKanalBitMaskDataObj (MrgKanalBitMaskList [i]).Kanal;
                     if KanalNr > 0 then
                       if length (AbrufRec.KanalMaske) >= KanalNr then
                         if AbrufRec.KanalMaske [KanalNr] = '1' then
                           C:=C OR (BitMaske SHL i);
                   end;
                 end;
                 S:=IntToHex (C, 1);  // Hex, nur soviele Stellen wie n�tig
               finally
                 MrgKanalBitMaskList.Free;
               end;
             end else
               S:=AbrufRec.KanalMaske;
           End;

      'U':  // Zeitstempel im Unix-Format
        Begin
          If Length (Maske) > 1 Then Begin
            Case Maske [2] of
              'v' : Begin
                      try
                        with AbrufRec do
                          dt:=EncodeDateTime (vonJahr, vonMonat, vonTag,
                                              vonStunde, vonMinute, vonSekunde, 0);
                        DateTimeToUnixTimeStr (dt, S);
                      except
                        S:='';
                      end;
                    End;
              'b' : Begin
                      try
                        with AbrufRec do
                          dt:=EncodeDateTime (bisJahr, bisMonat, bisTag,
                                              bisStunde, bisMinute, bisSekunde, 0);
                        DateTimeToUnixTimeStr (dt, S);
                      except
                        S:='';
                      end;
                    End;
            End;
          End;
        End;
    End;
  End;
  Result:=S;
End;

{-----------------------------------------------------------------------------}
function BuildMRGCommand (const AbrufRec: TAbrufRec; const KommandoDef: string;
  Const MrgTyp: integer; Const Kommandotyp: string;
  MrgKanalBitKonfigList: TMrgKanalBitKonfigList): string;
{-----------------------------------------------------------------------------}
Const
  m_copy = 0;
  m_mask = 1;
  m_hex  = 2;

Var
  i : Byte;
  Modus : Word;
  Buffer : String;
  MRGKommando: string;

Begin
  MRGKommando:='';
  Modus := m_copy;
  For i := 1 to Length (KommandoDef) Do Begin
    Case Modus of
      m_copy :
        Begin
          If KommandoDef [i] = '<' Then Begin
            Modus := m_mask;
            Buffer := '';
          End
          Else If KommandoDef [i] = '$' Then Begin
            Modus := m_hex;
            Buffer := '$';
          End Else
            MRGKommando:=MRGKommando + Copy (KommandoDef, i, 1);
        End;
      m_mask :
        Begin
          If KommandoDef [i] = '>' Then Begin
            Modus := m_copy;
            MRGKommando:=MRGKommando +
              GetMaskString (AbrufRec, Buffer, MrgTyp, Kommandotyp, MrgKanalBitKonfigList);
          End Else
            Buffer := Buffer + Copy (KommandoDef, i, 1);
        End;
      m_hex :  // Zeichen in Hex-Darstellung
        Begin
          Buffer := Buffer + Copy (KommandoDef, i, 1);
          If length (Buffer) >= 3 Then Begin
            Modus := m_copy;
            try
              MRGKommando:=MRGKommando + Chr (StrToInt (Buffer));
            except
              //
            end;
          End;
        End;
    End;
  End;
  if length (MRGKommando) > 0 then
    MRGKommando:=STX + MRGKommando + ETX;
  Result:=MRGKommando;
End;

{-----------------------------------------------------------------------------}
procedure GetMRGKommando_Archiv (AbrufRec: TAbrufRec;
                                 KommandoTyp: string;
                                 Abrufkommando_Def: string;
                                 Abrufkommando_alle_Daten_Def: string;
                                 MrgTyp: integer;
                                 MrgKanalBitKonfigList: TMrgKanalBitKonfigList;
                                 var MRGKommando: string);
{-----------------------------------------------------------------------------}
{ ger�tespezifischen MRG-Datenabruf-Befehl f�r Meldungen, Me�werte, Tagess�tze
  oder Pr�fungss�tze bilden;
  �bergabe: AbrufRec
            Kommandotyp-Zeichen
            Abrufkommando-Definitionsmaske (aus MrgAbruf-Ressource)
            Abrufkommando-Definitionsmaske f�r "alle Daten" (aus MrgAbruf-Ressource)
            MRG-Typnummer
            MrgKanalBit-Ressourcedatenliste
  R�ckgabe: MRG-Kommando }
begin
  CorrectAbrufRecY2000 (KommandoTyp, AbrufRec);
  if AbrufRec.AlleDaten then
    MRGKommando:=BuildMRGCommand (AbrufRec, Abrufkommando_alle_Daten_Def,
                   MrgTyp, Kommandotyp, MrgKanalBitKonfigList)
  else
    MRGKommando:=BuildMRGCommand (AbrufRec, Abrufkommando_Def,
                   MrgTyp, Kommandotyp, MrgKanalBitKonfigList);
end;

{-------------------------------------------------}
function GetMRGKommando_B (ParaNr: string): string;
{-------------------------------------------------}
{ MRG-Abrufbefehl f�r Parameter bilden;
  �bergabe: ParaNr (3-stellige MRG-Parameternummer oder
                    Leer-String f�r alle Parameter)
  Ergebnis: B-Befehlstring }
begin
  Result:=STX+'B'+ParaNr+ETX;
end;

{--------------------------------------------------------------------}
function GetMRGKommando_C (ParaNr: string; NeuerWert: string): string;
{--------------------------------------------------------------------}
{ MRG-Befehl zum Einstellen eines Parameters bilden;
  �bergabe: ParaNr (3-stellige MRG-Parameternummer)
            neuer Parameterwert
  Ergebnis: C-Befehlstring }
begin
  Result:=STX+'C'+ParaNr+NeuerWert+ETX;
end;

{--------------------------------}
function GetMRGKommando_k: string;
{--------------------------------}
{ MRG-Abrufbefehl f�r Kennung bilden (bei MRG 910 mit Anruf-Funktion);
  -> Damit kann die Kennung ohne Kenntnis des im Ger�t eingestellten Passworts
     abgerufen werden !
  Ergebnis: k-Befehlstring }
begin
  Result:=STX+'k'+ETX;
end;


{-------------- Befehlsroutinen f�r Elster-Ger�te (LIS-200) -------------------}

{---------------------------------------------------------------------------------}
function GetLIS200Kommando_Archiv (AbrufRec: TAbrufRec; ArchivNr: integer): string;
{---------------------------------------------------------------------------------}
{ LIS-200-Archiv-Abrufbefehl bilden;
  -> mit Blockgr��e 10 (bisher 1); 12.02.2014, WW
  �bergabe: AbrufRec
            Archivnummer
  Ergebnis: Kommando-String }
begin
  with AbrufRec do begin
    if AlleDaten then
      Result:=Format ('%sR3%s%d:V.0(;;;10)%s', [SOH, STX, ArchivNr, ETX])
    else
      Result:=Format ('%sR3%s%d:V.0(3;%.4d-%.2d-%.2d,%.2d:%.2d:%.2d;%.4d-%.2d-%.2d,%.2d:%.2d:%.2d;10)%s',
                      [SOH, STX, ArchivNr,
                       vonJahr, vonMonat, vonTag,
                       vonStunde,vonMinute, vonSekunde,
                       bisJahr, bisMonat, bisTag,
                       bisStunde, bisMinute, bisSekunde,
                       ETX]);
  end;
end;

{--------------------------------------------------------------}
function GetLIS200Kommando_Lesen (DatenAdresse: string): string;
{--------------------------------------------------------------}
{ LIS-200-Daten-Lesebefehl bilden;
  �bergabe: Daten-Adresse (z.B. 3:180 f�r Stationsnummer)
  Ergebnis: Kommando-String }
begin
  Result:=SOH+'R1'+STX+DatenAdresse+'.0(1)'+ETX;
end;

{-------------------------------------------------------------------------------------}
function GetLIS200Kommando_Schreiben (DatenAdresse: string; NeuerWert: string): string;
{-------------------------------------------------------------------------------------}
{ LIS-200-Daten-Schreibbefehl bilden (Parametrierung);
  �bergabe: Daten-Adresse (z.B. 3:180 f�r Stationsnummer)
            neuer Wert
  Ergebnis: Kommando-String }
begin
  Result:=SOH+'W1'+STX+DatenAdresse+'.0('+NeuerWert+')'+ETX;
end;


{--------------------- Befehlsroutinen f�r KE-Anlagen -------------------------}

{----------------------------------------------------}
function GetKE_Kommando (Kommandostr: string): string;
{----------------------------------------------------}
{ KE-Abrufbefehl bilden;
  �bergabe: Kommando-String (Kommandobuchstabe und Nummer)
  Ergebnis: KE-Befehlstring }
begin
  Result:=STX+KommandoStr+ETX;
end;

{--------------------------------------------------------------------}
function GetKE_Kommando_Zeitbereich_von (AbrufRec: TAbrufRec): string;
{--------------------------------------------------------------------}
{ KE-Befehl zum Setzen des von-Auslesezeitraums bilden;
  �bergabe: AbrufRec
  Ergebnis: Kommando-String }
var
  Jahr: integer;
  Monat: integer;
  Tag: integer;
  Stunde: integer;
  Minute: integer;
  Sekunde: integer;
  JahrBuf: integer;

begin
  with AbrufRec do begin
    if AlleDaten then begin
      { Ersatz-von-Zeitpunkt f�r "alle Daten" bilden: ab 1.1.2000 }
      Jahr:=2000;
      Monat:=1;
      Tag:=1;
      Stunde:=0;
      Minute:=0;
      Sekunde:=0;
    end
    else begin
      Jahr:=vonJahr;
      Monat:=vonMonat;
      Tag:=vonTag;
      Stunde:=vonStunde;
      Minute:=vonMinute;
      Sekunde:=vonSekunde;
    end;
  end;

  { Jahr in C-Format wandeln (z.B. 2003 -> 103): }
  JahrBuf:=Jahr MOD 100;
  if Jahr >= 2000 then
    JahrBuf:=JahrBuf + 100;

  Result:=Format ('%sP201=%.3d/%.2d/%.2d/%.2d/%.2d/%.2d%s',
                  [STX, JahrBuf, Monat, Tag, Stunde, Minute, Sekunde, ETX]);
end;

{--------------------------------------------------------------------}
function GetKE_Kommando_Zeitbereich_bis (AbrufRec: TAbrufRec): string;
{--------------------------------------------------------------------}
{ KE-Befehl zum Setzen des bis-Auslesezeitraums bilden;
  �bergabe: AbrufRec
  Ergebnis: Kommando-String }
var
  Jahr: word;
  Monat: word;
  Tag: word;
  Stunde: word;
  Minute: word;
  Sekunde: word;
  dummy: word;
  JahrBuf: integer;

begin
  with AbrufRec do begin
    if AlleDaten then begin
      { Ersatz-bis-Zeitpunkt f�r "alle Daten" bilden: aktuelle PC-Zeit }
      DecodeDate (Date,  Jahr, Monat, Tag);
      DecodeTime (Time, Stunde, Minute, Sekunde, dummy);
    end
    else begin
      Jahr:=bisJahr;
      Monat:=bisMonat;
      Tag:=bisTag;
      Stunde:=bisStunde;
      Minute:=bisMinute;
      Sekunde:=bisSekunde;
    end;
  end;

  { Jahr in C-Format wandeln (z.B. 2003 -> 103): }
  JahrBuf:=Jahr MOD 100;
  if Jahr >= 2000 then
    JahrBuf:=JahrBuf + 100;

  Result:=Format ('%sP202=%.3d/%.2d/%.2d/%.2d/%.2d/%.2d%s',
                  [STX, JahrBuf, Monat, Tag, Stunde, Minute, Sekunde, ETX]);
end;


{------------------ Befehlsroutinen f�r Datacon-Ger�te (FWU) ------------------}

{-----------------------------------------------------------------------------------}
function GetFWU_Kommando_Archiv (AbrufRec: TAbrufRec; KennzifferStr: string): string;
{-----------------------------------------------------------------------------------}
{ FWU-Datenlesebefehl bilden;
  �bergabe: AbrufRec
            Kennziffer (z.B. 7-1m:V.b.p f�r Werte-Profil Vb MU, Tarifger�t 1)
  Ergebnis: Kommando-String }
var
  JahrBuf_von: integer;
  MonatBuf_von: integer;
  TagBuf_von: integer;
  JahrBuf_bis: word;
  MonatBuf_bis: word;
  TagBuf_bis: word;

begin
  with AbrufRec do begin
    if AlleDaten then begin
      { Ersatz-von-Zeitpunkt f�r "alle Daten" bilden: ab 1.1.2000 }
      JahrBuf_von:=2000;
      MonatBuf_von:=1;
      TagBuf_von:=1;
      { Ersatz-bis-Zeitpunkt f�r "alle Daten" bilden: aktuelle PC-Zeit }
      DecodeDate (Date, JahrBuf_bis, MonatBuf_bis, TagBuf_bis);
    end
    else begin
      JahrBuf_von:=vonJahr;
      MonatBuf_von:=vonMonat;
      TagBuf_von:=vonTag;
      JahrBuf_bis:=bisJahr;
      MonatBuf_bis:=bisMonat;
      TagBuf_bis:=bisTag;
    end;
  end;

  { von/bis-Jahr zweistellig }
  JahrBuf_von:=JahrBuf_von MOD 100;
  JahrBuf_bis:=JahrBuf_bis MOD 100;
  Result:=Format ('/?%s;%.2d%.2d%.2d;%.2d%.2d%.2d!%s',
                  [KennzifferStr,
                   TagBuf_von, MonatBuf_von, JahrBuf_von,
                   TagBuf_bis, MonatBuf_bis, JahrBuf_bis,
                   CR + LF]);
end;


{-------- Befehlsroutinen f�r Tritschler-Ger�te (VC2, TTG, TDS, SSU) ----------}

{------------------------------------------------------------}
function GetTritschler_Sicherung_BC (sBefehl: string): string;
{------------------------------------------------------------}
{ liefert Tritschler-Sicherungszeichen, Hex-codiert (High-Nibble Low-Nibble)
  �bergabe: Befehl, �ber den die Sicherung gebildet werden soll }
var
  i: integer;
  b: byte;

begin
  b:=0;
  for i:=1 to length (sBefehl) do
    b:=b XOR Byte (sBefehl [i]);
  Result:=IntToHex (b, 2);
end;

{----------------------------------------------------------------------}
function GetTritschler_IECKommando_Standardabfrage (SSU_GerAuswNr: byte;
  Passwort: string): string;
{----------------------------------------------------------------------}
{ liefert Tritschler-IEC-Kommando f�r Standardabfrage (alle Ger�tetypen);
  �bergabe: Ger�teauswahlnummer f�r Multiplexer SSU
            Zugangs-Passwort f�r Multiplexer SSU }
var
  S: string;

begin
  if SSU_GerAuswNr > 0 then  // Befehl an Ger�t �ber Multiplexer
    S:=Format ('P%sG%u', [Passwort, SSU_GerAuswNr])
  else
    S:='';
  Result:=Format ('/?%s!%s', [S, CR + LF]);
end;

{--------------------------------------------------------------------------}
function GetTritschler_IECKommando_05_AktUmwerterDaten (SSU_GerAuswNr: byte;
  Passwort: string): string;
{--------------------------------------------------------------------------}
{ liefert Tritschler-IEC-Kommando zur Auslesung aktueller Umwerter-Daten (VC2);
  �bergabe: Ger�teauswahlnummer f�r Multiplexer SSU
            Zugangs-Passwort f�r Multiplexer SSU }
var
  S: string;
begin
  if SSU_GerAuswNr > 0 then  // Befehl an Ger�t �ber Multiplexer
    S:=Format ('P%sG%u', [Passwort, SSU_GerAuswNr])
  else
    S:='';
  Result:=Format ('/?%s05!%s', [S, CR + LF]);
end;

{--------------------------------------------------------------}
function GetTritschler_IECKommando_20_ZeitSync (sNewUTC: string;
  SSU_GerAuswNr: byte; Passwort: string): string;
{--------------------------------------------------------------}
{ liefert Tritschler-IEC-Kommando zur Synchronisierung der Ger�tezeit (TDS);
  �bergabe: neue Ger�tezeit als UTC-String
            Ger�teauswahlnummer f�r Multiplexer SSU
            Zugangs-Passwort f�r Multiplexer SSU }
var
  S: string;
  S_Sicherung: string;

begin
  if SSU_GerAuswNr > 0 then  // Befehl an Ger�t �ber Multiplexer
    S:=Format ('P%sG%u', [Passwort, SSU_GerAuswNr])
  else
    S:='';

  S:=Format ('/?%s20%sS', [S, sNewUTC]);

  // 2 Sicherungszeichen anh�ngen: gebildet �ber '/' bis 'S' ohne Mux-Passwort
  // und Ger�teauswahl-Info !
  S_Sicherung:=Format ('/?20%sS', [sNewUTC]);

  Result:=S + GetTritschler_Sicherung_BC (S_Sicherung) + '!' + CR + LF;
end;

{--------------------------------------------------------------------}
function GetTritschler_IECKommando_26_Logbuecher (AbrufRec: TAbrufRec;
  SSU_GerAuswNr: byte; Passwort: string): string;
{--------------------------------------------------------------------}
{ liefert Tritschler-IEC-Kommando zur Auslesung des eichpflichtigen und nicht-
  eichpflichtigen Logbuchs (TDS);
  �bergabe: AbrufRec
            Ger�teauswahlnummer f�r Multiplexer SSU
            Zugangs-Passwort f�r Multiplexer SSU }
var
  JahrBuf_von: integer;
  JahrBuf_bis: word;
  S: string;

begin
  if SSU_GerAuswNr > 0 then  // Befehl an Ger�t �ber Multiplexer
    S:=Format ('P%sG%u', [Passwort, SSU_GerAuswNr])
  else
    S:='';

  with AbrufRec do begin
    if AlleDaten then
      Result:=Format ('/?%s26!%s', [S, CR + LF])
    else begin
      { von/bis-Jahr zweistellig }
      JahrBuf_von:=vonJahr MOD 100;
      JahrBuf_bis:=bisJahr MOD 100;
      Result:=Format ('/?%s26%.2d%.2d%.2d%.2d%.2d%.2d!%s',
                      [S, bisTag, bisMonat, JahrBuf_bis, // erst Bis-Zeitpunkt !
                       vonTag, vonMonat, JahrBuf_von,    // ...dann von-Zeitpunkt
                       CR + LF]);
    end;
  end;
end;

{--------------------------------------------------------------------}
function GetTritschler_IECKommando_27_Logbuecher_nicht_eichtechnisch (
  AbrufRec: TAbrufRec; SSU_GerAuswNr: byte; Passwort: string): string;
{--------------------------------------------------------------------}
{ liefert Tritschler-IEC-Kommando zur Auslesung des nicht-eichpflichtigen
  Logbuchs (VC3, VCC);
  �bergabe: AbrufRec
            Ger�teauswahlnummer f�r Multiplexer SSU
            Zugangs-Passwort f�r Multiplexer SSU }
var
  JahrBuf_von: integer;
  JahrBuf_bis: word;
  S: string;

begin
  if SSU_GerAuswNr > 0 then  // Befehl an Ger�t �ber Multiplexer
    S:=Format ('P%sG%u', [Passwort, SSU_GerAuswNr])
  else
    S:='';

  with AbrufRec do begin
    if AlleDaten then
      Result:=Format ('/?%s27!%s', [S, CR + LF])
    else begin
      { von/bis-Jahr zweistellig }
      JahrBuf_von:=vonJahr MOD 100;
      JahrBuf_bis:=bisJahr MOD 100;
      Result:=Format ('/?%s27%.2d%.2d%.2d%.2d%.2d%.2d!%s',
                      [S, bisTag, bisMonat, JahrBuf_bis, // erst Bis-Zeitpunkt !
                       vonTag, vonMonat, JahrBuf_von,    // ...dann von-Zeitpunkt
                       CR + LF]);
    end;
  end;
end;

{--------------------------------------------------------------------}
function GetTritschler_IECKommando_28_Logbuecher_eichtechnisch (
  AbrufRec: TAbrufRec; SSU_GerAuswNr: byte; Passwort: string): string;
{--------------------------------------------------------------------}
{ liefert Tritschler-IEC-Kommando zur Auslesung des eichpflichtigen
  Logbuchs (VC3, VCC);
  �bergabe: AbrufRec
            Ger�teauswahlnummer f�r Multiplexer SSU
            Zugangs-Passwort f�r Multiplexer SSU }
var
  JahrBuf_von: integer;
  JahrBuf_bis: word;
  S: string;

begin
  if SSU_GerAuswNr > 0 then  // Befehl an Ger�t �ber Multiplexer
    S:=Format ('P%sG%u', [Passwort, SSU_GerAuswNr])
  else
    S:='';

  with AbrufRec do begin
    if AlleDaten then
      Result:=Format ('/?%s28!%s', [S, CR + LF])
    else begin
      { von/bis-Jahr zweistellig }
      JahrBuf_von:=vonJahr MOD 100;
      JahrBuf_bis:=bisJahr MOD 100;
      Result:=Format ('/?%s28%.2d%.2d%.2d%.2d%.2d%.2d!%s',
                      [S, bisTag, bisMonat, JahrBuf_bis, // erst Bis-Zeitpunkt !
                       vonTag, vonMonat, JahrBuf_von,    // ...dann von-Zeitpunkt
                       CR + LF]);
    end;
  end;
end;

{-----------------------------------------------------------------------------}
function GetTritschler_IECKommando_29_Zaehlerstandsprofil (AbrufRec: TAbrufRec;
  SSU_GerAuswNr: byte; Passwort: string): string;
{-----------------------------------------------------------------------------}
{ liefert Tritschler-IEC-Kommando zur Auslesung des Z�hlerstandsprofils (TDS);
  �bergabe: AbrufRec
            Ger�teauswahlnummer f�r Multiplexer SSU
            Zugangs-Passwort f�r Multiplexer SSU }
var
  JahrBuf_von: integer;
  MonatBuf_von: integer;
  TagBuf_von: integer;
  JahrBuf_bis: word;
  MonatBuf_bis: word;
  TagBuf_bis: word;
  S: string;

begin
  with AbrufRec do begin
    if AlleDaten then begin
      { Ersatz-von-Zeitpunkt f�r "alle Daten" bilden: ab 1.1.2000 }
      JahrBuf_von:=2000;
      MonatBuf_von:=1;
      TagBuf_von:=1;
      { Ersatz-bis-Zeitpunkt f�r "alle Daten" bilden: aktuelle PC-Zeit }
      DecodeDate (Date, JahrBuf_bis, MonatBuf_bis, TagBuf_bis);
    end
    else begin
      JahrBuf_von:=vonJahr;
      MonatBuf_von:=vonMonat;
      TagBuf_von:=vonTag;
      JahrBuf_bis:=bisJahr;
      MonatBuf_bis:=bisMonat;
      TagBuf_bis:=bisTag;
    end;
  end;
  { von/bis-Jahr zweistellig }
  JahrBuf_von:=JahrBuf_von MOD 100;
  JahrBuf_bis:=JahrBuf_bis MOD 100;

  if SSU_GerAuswNr > 0 then  // Befehl an Ger�t �ber Multiplexer
    S:=Format ('P%sG%u', [Passwort, SSU_GerAuswNr])
  else
    S:='';
  Result:=Format ('/?%s29%.2d%.2d%.2d%.2d%.2d%.2d!%s',
                  [S, TagBuf_bis, MonatBuf_bis, JahrBuf_bis, // erst Bis-Zeitpunkt !
                   TagBuf_von, MonatBuf_von, JahrBuf_von,    // ...dann von-Zeitpunkt
                   CR + LF]);
end;

{----------------------------------------------------------}
function GetTritschler_IECKommando_40_Quittung_GPRS: string;
{----------------------------------------------------------}
{ liefert Tritschler-IEC-Kommando zur Quittierung eines per GPRS �bertragenen
  Datentelegramms (TDS) }
var
  S: string;

begin
  S:='0';  // OK
  Result:=Format ('/?40%s!%s', [S, CR + LF]);
end;

{----------------------------------------------------------}
function GetTritschler_IECKommando_42_ZeitSync_GPRS: string;
{----------------------------------------------------------}
{ liefert Tritschler-IEC-Kommando zur Synchronisierung (Feinjustierung) der
  Ger�tezeit �ber GPRS (TDS) }
var
  S: string;

begin
  // Info lt. Tritschler-Doc: Die Stundenangabe hh wird vom Ger�t nicht
  // ausgewertet (nur Feinjustierung !)
  S:=FormatDateTime ('hhnnsszzz', Time);
  S:=Copy (S, 1, length (S) - 1);  // nur Hundertstel-Sekunden
  S:=Format ('/?42%sS', [S]);

  // 2 Sicherungszeichen anh�ngen
  Result:=S + GetTritschler_Sicherung_BC (S) + '!' + CR + LF;
end;

{---------------------------------------------------------------------------}
function GetTritschler_IECKommando_99_Komplettauslesung (AbrufRec: TAbrufRec;
  SSU_GerAuswNr: byte; Passwort: string): string;
{---------------------------------------------------------------------------}
{ liefert Tritschler-IEC-Kommando zur Komplettauslesung aller Informationen (VC2);
  �bergabe: AbrufRec
            Ger�teauswahlnummer f�r Multiplexer SSU
            Zugangs-Passwort f�r Multiplexer SSU }
var
  JahrBuf_von: integer;
  MonatBuf_von: integer;
  TagBuf_von: integer;
  JahrBuf_bis: word;
  MonatBuf_bis: word;
  TagBuf_bis: word;
  S: string;

begin
  with AbrufRec do begin
    if AlleDaten then begin
      { Ersatz-von-Zeitpunkt f�r "alle Daten" bilden: ab 1.1.2000 }
      JahrBuf_von:=2000;
      MonatBuf_von:=1;
      TagBuf_von:=1;
      { Ersatz-bis-Zeitpunkt f�r "alle Daten" bilden: aktuelle PC-Zeit }
      DecodeDate (Date, JahrBuf_bis, MonatBuf_bis, TagBuf_bis);
    end
    else begin
      JahrBuf_von:=vonJahr;
      MonatBuf_von:=vonMonat;
      TagBuf_von:=vonTag;
      JahrBuf_bis:=bisJahr;
      MonatBuf_bis:=bisMonat;
      TagBuf_bis:=bisTag;
    end;
  end;
  { von/bis-Jahr zweistellig }
  JahrBuf_von:=JahrBuf_von MOD 100;
  JahrBuf_bis:=JahrBuf_bis MOD 100;

  if SSU_GerAuswNr > 0 then  // Befehl an Ger�t �ber Multiplexer
    S:=Format ('P%sG%u', [Passwort, SSU_GerAuswNr])
  else
    S:='';
  Result:=Format ('/?%s99%.2d%.2d%.2d%.2d%.2d%.2d!%s',
                  [S, TagBuf_bis, MonatBuf_bis, JahrBuf_bis, // erst Bis-Zeitpunkt !
                   TagBuf_von, MonatBuf_von, JahrBuf_von,    // ...dann von-Zeitpunkt
                   CR + LF]);
end;

{----------------------------------------------------------------------------}
function GetTTG_IECKommando_Lastprofil (AbrufRec: TAbrufRec; KanalNr: integer;
  SSU_GerAuswNr: byte; Passwort: string): string;
{----------------------------------------------------------------------------}
{ liefert IEC-Kommando f�r TTG zur Auslesung des Lastprofils eines Kanals;
  �bergabe: AbrufRec
            Kanalnummer
            Ger�teauswahlnummer f�r Multiplexer SSU
            Zugangs-Passwort f�r Multiplexer SSU }
var
  JahrBuf_von: integer;
  MonatBuf_von: integer;
  TagBuf_von: integer;
  JahrBuf_bis: word;
  MonatBuf_bis: word;
  TagBuf_bis: word;
  S: string;

begin
  with AbrufRec do begin
    if AlleDaten then begin
      { Ersatz-von-Zeitpunkt f�r "alle Daten" bilden: ab 1.1.2000 }
      JahrBuf_von:=2000;
      MonatBuf_von:=1;
      TagBuf_von:=1;
      { Ersatz-bis-Zeitpunkt f�r "alle Daten" bilden: aktuelle PC-Zeit }
      DecodeDate (Date, JahrBuf_bis, MonatBuf_bis, TagBuf_bis);
    end
    else begin
      JahrBuf_von:=vonJahr;
      MonatBuf_von:=vonMonat;
      TagBuf_von:=vonTag;
      JahrBuf_bis:=bisJahr;
      MonatBuf_bis:=bisMonat;
      TagBuf_bis:=bisTag;
    end;
  end;
  { von/bis-Jahr zweistellig }
  JahrBuf_von:=JahrBuf_von MOD 100;
  JahrBuf_bis:=JahrBuf_bis MOD 100;

  if SSU_GerAuswNr > 0 then  // Befehl an TTG �ber Multiplexer
    S:=Format ('P%sG%u', [Passwort, SSU_GerAuswNr])
  else
    S:='';
  Result:=Format ('/?%s%.2d%.2d%.2d%.2d%.2d%.2d%.2d!%s',
                  [S, KanalNr + 1,        // Kanal 1: /02?...  Kanal 2: /03?...
                   TagBuf_bis, MonatBuf_bis, JahrBuf_bis,    // erst Bis-Zeitpunkt !
                   TagBuf_von, MonatBuf_von, JahrBuf_von,    // ...dann von-Zeitpunkt
                   CR + LF]);
end;

{-----------------------------------------------------------------}
function GetTTG_FTLKommando_Geraeteinfo (KanalNr: integer): string;
{-----------------------------------------------------------------}
{ liefert Kommando zum Lesen der Ger�teinformation f�r TTG (FTL-Protokoll);
  �bergabe: Kanalnummer }
begin
  case KanalNr of
    1: Result:='F0000' + CR;
    2: Result:='F0010' + CR;
  else
    Result:='';
  end;
end;

{--------------------------------------------------------------------}
function GetTTG_FTLKommando_Zaehlerstaende (KanalNr: integer): string;
{--------------------------------------------------------------------}
{ liefert Kommando zum Lesen der Z�hlerst�nde f�r TTG (FTL-Protokoll);
  �bergabe: Kanalnummer }
begin
  case KanalNr of
    1: Result:='F0001' + CR;
    2: Result:='F0011' + CR;
  else
    Result:='';
  end;
end;

{-----------------------------------------------------------------------}
function GetTTG_FTLKommando_Messperiodenwerte (KanalNr: integer): string;
{-----------------------------------------------------------------------}
{ liefert Kommando zum Lesen der Messperiodenwerte f�r TTG (FTL-Protokoll);
  �bergabe: Kanalnummer }
begin
  case KanalNr of
    1: Result:='F0002' + CR;
    2: Result:='F0012' + CR;
  else
    Result:='';
  end;
end;


{-----------------------------------------------------------------------}
function GetTritschler_FTLinternKommando (iBlockNr: word; sDaten: string;
  cRW: char): string;
{-----------------------------------------------------------------------}
{ liefert Kommando f�r FTL-internes Protokoll (ohne Pr�fsumme, aber mit
  abschlie�endem CR);
  �bergabe: Blocknummer (0 - 999)
            Daten-String
            RW-Kennung ('L' = Lesen, 'S' = Schreiben) }
begin
  Result:=Format ('%.3d', [iBlockNr]) + sDaten + 'N' + cRW + CR;
end;

{------------------------------------------------------------------------}
function GetTritschler_FTLinternKommando_WritePasswort (sPasswort: string;
  iPasswortNr: word): string;
{------------------------------------------------------------------------}
{ liefert Kommando zum �bertragen des (Parametrier-)Passworts f�r FTL-internes
  Protokoll (ohne Pr�fsumme, aber mit abschlie�endem CR);
  �bergabe: Passwort
            WICO-Passwort-Nummer (2 = Hersteller, 3 = Lieferant [+ Kunde], 4 = Kunde) }
var
  sDaten: string;

begin
  // Die 3 m�glichen Passworte sind jeweils max. 4 Zeichen lang:
  case iPasswortNr of
    2: begin  // Hersteller-Passwort
         sDaten:=Copy (sPasswort, 1, 4);
       end;

    3: begin  // Lieferanten-Passwort (optional erweitert um Kunden-Passwort)
         sDaten:=F_LeftPad (sDaten, ' ', 4);
         sDaten:=sDaten + Copy (sPasswort, 1, 8);
       end;

    4: begin  // Kunden-Passwort
         sDaten:=F_LeftPad (sDaten, ' ', 8);
         sDaten:=sDaten + Copy (sPasswort, 1, 4);
       end;
  else
    sDaten:='';
  end;
  sDaten:=F_RightPad (sDaten, ' ', 12);

  Result:=GetTritschler_FTLinternKommando (002, sDaten, 'S');          
end;


{-------------------------------------------------------}
function GetSSUKommando_Login (Passwort: string): string;
{-------------------------------------------------------}
{ liefert Login-Kommando f�r SSU (Multiplexer, IEC-Protokoll);
  �bergabe: Zugangs-Passwort f�r Multiplexer SSU }
begin
  Result:=Format ('/?P%s!%s', [Passwort, CR + LF]);
end;

{-----------------------------------------------------}
function GetSSUKommando_Quittung (Modus: char): string;
{-----------------------------------------------------}
{ liefert Quittungs-Kommando f�r SSU (Multiplexer, IEC-Protokoll);
  �bergabe: Auswahlmodus: 0 = Standardanfrage (SSU-Status bzw. Alarmursache)
                          1 = Programmiermodus }
begin
  Result:=ACK + '0 ' + Modus + CR + LF;
end;


{----------- Befehlsroutinen f�r Actaris-Ger�te (Corus, Sparklog) -------------}

{-------------------------------------------------------------------}
function GetCorusSAMKommando_DatenspeicherLesen (AbrufRec: TAbrufRec;
  DatenspeicherID: byte): string;
{-------------------------------------------------------------------}
{ Corus-Datenspeicherlesebefehl bilden (SAM-Protokoll);
  �bergabe: AbrufRec
            Datenspeicher-ID (z.B. 0 = Hauptdatenspeicher (Interval log)
                                   3 = Monatsspeicher
                                   4 = Ereignis-Logbuch
                                   5 = Parameter-Logbuch
                                   6 = Eichtechnisches Logbuch)
  Ergebnis: Kommando-String }
var
  iSize: byte;
  iDB: byte;
  iOptions: cardinal;
  dtVon: TDateTime;
  dtBis: TDateTime;
  sVon: string;
  sBis: string;

begin
  iSize:=13;
  iDB:=DatenspeicherID AND $0F;  // Bit 0-3: Datenspeicher-ID
  iDB:=iDB OR $10;  // Bit 4: Anzahl der Datens�tze in erster Antwort mitschicken
  case DatenspeicherID of
    0: iOptions:=$000893F1;  // Hauptdatenspeicher: mit gesetzten Datamask-Bits f�r
                             // Date_End, Vb, Vn, Vb st�r, Vn ges, Status, Druck, Temperatur, Vb kontr
    3: iOptions:=$00F00211;  // Monatsspeicher: mit gesetzten Datamask-Bits f�r
                             // Date_End, Status, Vb, Vn, Vb st�r, Vn ges
    4: iOptions:=$00000019;  // Ereignis-Logbuch: mit gesetzten Datamask-Bits f�r
                             // Datum und Ereigniscode
    5, 6: iOptions:=$00000319;  // Parameter- und eichtechnisches Logbuch: mit
                                // gesetzten Datamask-Bits f�r Datum, Parametercode,
                                //alter und neuer Wert
  else
    Result:='';
    exit;
  end;

  with AbrufRec do begin
    if AlleDaten then begin
      sVon:=#0#0#0#0;
      sBis:=#0#0#0#0;
    end
    else begin
      try
        dtVon:=EncodeDateTime (vonJahr, vonMonat, vonTag, vonStunde, vonMinute, vonSekunde, 0);
        sVon:=Date2Bin_Corus (dtVon);
      except
        sVon:=#0#0#0#0;
      end;

      try
        dtBis:=EncodeDateTime (bisJahr, bisMonat, bisTag, bisStunde, bisMinute, bisSekunde, 0);
        sBis:=Date2Bin_Corus (dtBis);
      except
        sBis:=#0#0#0#0;
      end;
    end;

    Result:=SOH+ #$BE + Chr(iSize) + Chr(iDB) + Longword2Bin (iOptions) + sBis + sVon + ETX;
  end;
end;

{----------------------------------------------------------------------}
function GetCorusSAMKommando_ParameterLesen (ParameterID: byte): string;
{----------------------------------------------------------------------}
{ Corus-Befehl zum Lesen von einem einzelnen Parameter bilden (SAM-Protokoll);
  �bergabe: Parameter-ID
  Ergebnis: Kommando-String }
var
  iSize: byte;

begin
  iSize:=1;
  Result:=SOH+ #$BF + Chr(iSize) + Chr(ParameterID) + ETX;
end;

{----------------------------------------------------------------------------------}
function GetCorusSAMKommando_ParameterGruppeLesen (slParameterID: TStrings): string;
{----------------------------------------------------------------------------------}
{ Corus-Befehl zum Lesen von mehreren Parametern bilden (SAM-Protokoll);
  �bergabe: Stringliste mit Parameternummer-IDs
  Ergebnis: Kommando-String }
var
  iSize: byte;
  sData: string;
  i: integer;
  iParameterID: integer;

begin
  iSize:=0;
  sData:='';
  for i:=0 to slParameterID.Count - 1 do begin
    iParameterID:=StrToIntDef (slParameterID [i], -1);
    if (iParameterID >= 0) AND (iParameterID <= MaxByte) then begin  // Erlaubte Parameter-IDs im Bereich 0..255
      if iSize < MaxByte then begin
        inc (iSize);
        sData:=sData + Chr(iParameterID);
      end;
    end;
  end;

  Result:=SOH+ #$BF + Chr(iSize) + sData + ETX;
end;

{-----------------------------------------------------------------}
function GetCorusSAMKommando_ParameterSchreiben (ParameterID: byte;
  NeuerWert: string): string;
{-----------------------------------------------------------------}
{ Corus-Parameterschreibbefehl bilden (SAM-Protokoll);
  �bergabe: Parameter-ID
            neuer Wert
  Ergebnis: Kommando-String }
var
  iSize: byte;

begin
  iSize:=1 + length (NeuerWert);
  Result:=SOH+ #$FF + Chr(iSize) + Chr(ParameterID) + NeuerWert + ETX;
end;

{--------------------------------------------------------------}
function GetActaris_IEC1107Kommando_Archiv (AbrufRec: TAbrufRec;
  Kennzahl: string): string;
{--------------------------------------------------------------}
{ Actaris IEC1107-Lesebefehl f�r Archive bilden (Lastgang, Logb�cher);
  �bergabe: AbrufRec
            EDIS-Kennzahl (z.B. P.01 f�r Lastgang)
  Ergebnis: Kommando-String }
begin
  with AbrufRec do begin
    if AlleDaten then
      Result:=Format ('%sR5%s%s(;)%s', [SOH, STX, Kennzahl, ETX])
    else
      Result:=Format ('%sR5%s%s(%.2d%.2d%.2d%.2d%.2d%.2d;%.2d%.2d%.2d%.2d%.2d%.2d)%s',
                      [SOH, STX, Kennzahl,
                       vonJahr MOD 100, vonMonat, vonTag,  // von-Jahr 2-stellig
                       vonStunde, vonMinute, vonSekunde,
                       bisJahr MOD 100, bisMonat, bisTag,  // bis-Jahr 2-stellig
                       bisStunde, bisMinute, bisSekunde,
                       ETX]);
  end;
end;


{------------- Befehlsroutinen f�r Kamstrup-Ger�te (UNIGAS 300) ---------------}

{---------------------------------------------------------------}
function GetKamstrup_IEC1107Kommando_Archiv (AbrufRec: TAbrufRec;
  Kennzahl: string): string;
{---------------------------------------------------------------}
{ Kamstrup-IEC1107-Lesebefehl f�r Archive bilden;
  �bergabe: AbrufRec
            EDIS-Kennzahl (z.B. P.01 f�r Messperiodenarchiv)
  Ergebnis: Kommando-String }
begin
  with AbrufRec do begin
    if AlleDaten then
      Result:=Format ('%sR6%s%s(;;99)%s', [SOH, STX, Kennzahl, ETX])
    else
      Result:=Format ('%sR6%s%s(%.2d%.2d%.2d%.2d%.2d%.2d;%.2d%.2d%.2d%.2d%.2d%.2d;99)%s',
                      [SOH, STX, Kennzahl,
                       vonJahr MOD 100, vonMonat, vonTag,  // von-Jahr 2-stellig
                       vonStunde, vonMinute, vonSekunde,
                       bisJahr MOD 100, bisMonat, bisTag,  // bis-Jahr 2-stellig
                       bisStunde, bisMinute, bisSekunde,
                       ETX]);
  end;
end;

{----------------------------------------------------------------------}
function GetKamstrup_IEC1107Kommando_Logbuch (Kennzahl: string): string;
{----------------------------------------------------------------------}
{ Kamstrup-IEC1107-Lesebefehl f�r Logbuch bilden;
  �bergabe: AbrufRec
            EDIS-Kennzahl (z.B. P.01 f�r Messperiodenarchiv)
  Ergebnis: Kommando-String }
begin
  Result:=Format ('%sR6%s%s(;;99)%s', [SOH, STX, Kennzahl, ETX])
end;

{--------------------- Standard IEC 1107-Befehlsroutinen ----------------------}

{-----------------------------------------------------------}
function GetIEC1107Kommando_Lesen (Kennzahl: string): string;
{-----------------------------------------------------------}
{ Standard IEC 1107-Daten-Lesebefehl bilden (z.B Actaris Sparklog, Kamstrup
  UNIGAS 300);
  �bergabe: EDIS-Kennzahl (z.B. 0:0.0.0 f�r Eigentumsnummer)
  Ergebnis: Kommando-String }
begin
  Result:=SOH+'R5'+STX+Kennzahl+'()'+ETX;
end;

{-----------------------------------------------}
function GetIEC1107Kommando_Schreiben (
  Kennzahl, NeuerWert, Passwort: string): string;
{-----------------------------------------------}
{ Standard IEC 1107-Daten-Schreibbefehl f�r Parametrierung bilden (z.B Actaris
  Sparklog, Kamstrup UNIGAS 300);
  �bergabe: EDIS-Kennzahl (z.B. 0:0.0.0 f�r Eigentumsnummer)
            neuer Wert
            Passwort
  Ergebnis: Kommando-String }
begin
  Result:=SOH+'W5'+STX+Kennzahl+'('+NeuerWert+')('+Passwort+')'+ETX;
end;


{------------- Befehlsroutinen f�r Veraut-Ger�te (Veribox Mini) ---------------}

{---------------------------------------------------------------------}
function GetVeriboxMini_SMSKommando_Daten (vonZeit: TDateTime): string;
{---------------------------------------------------------------------}
{ Veribox SMS-Befehl zum Anfordern von Messdaten;
  �bergabe: von-Zeitpunkt
  Ergebnis: Kommando-String }
var
  iSeconds: Int64;
  sSeconds: string;

begin
  try
    { Sekunden zwischen 1.1.2000 00:00:00 und von-Zeitpunkt: }
    iseconds:=GetUnixSekundenFromDateTime (vonZeit) -
              GetUnixSekundenFromDateTime (EncodeDateTime (2000, 1, 1, 0, 0, 0, 0));
    sSeconds:=IntToStr (iSeconds);
  except
    sSeconds:='0';
  end;

  Result:='10000 ' + sSeconds;
end;


{--------------------- Befehlsroutinen f�r Elster DS-100 ----------------------}

{------------------------------------------------------}
function GetDS100_LeseBefehl (Kommando: string): string;
{------------------------------------------------------}
{ Liefert DS100-Lesebefehl zum �bergebenen Kommando-Zeichen;
  �bergabe: Kommandozeichen
  Ergebnis: Befehl }
begin
  Result:=Format ('?%s', [Kommando]);
end;

{---------------------------------------------------------}
function GetDS100_SchreibBefehl (Kommando: string): string;
{---------------------------------------------------------}
{ Liefert DS100-Schreibbefehl zum �bergebenen Kommando-Zeichen;
  �bergabe: Kommandozeichen
  Ergebnis: Befehl }
begin
  Result:=Format ('!%s', [Kommando]);
end;


{------------------------------- Modbus ---------------------------------------}

{-------------------------------------------------------------------------------}
function GetKanalDef_MBRegister_Archiv (AMrgTyp: integer; AArchivtyp: TArchivtyp;
  iStartAdresse: word; MBAbrufData: TMBAbrufData): string;
{-------------------------------------------------------------------------------}
{ Liefert Ger�tetyp-abh�ngig die Modbus-Kanaldefinition zu der Startadresse eines
  Werts eines Archiv-Datensatzes;
  �bergaben: Ger�tetyp
             Archivtyp
             Startadresse
             MBAbrufData-Record mit Modbusregister-Konfiguration des Archivs
  Ergebnis: Kanal-Definition (leer, wenn nicht gefunden) }
var
  i: integer;
  MBRegisterDef: TMBRegisterDef;

begin
  Result:='';  // Vorbelegung: KanalDef unbekannt

  case AArchivtyp of
    at_Periodenarchiv:
      begin
        case AMrgTyp of
          mrgtyp_Primus:
            begin
              for i:=Low (Primus_MBRegisterDef_Mess) to
                     High (Primus_MBRegisterDef_Mess) do begin
                // Startadresse aller Datenarchiv-Datens�tze des Primus:
                if (iStartAdresse >= Primus_MBRegisterDef_Mess [i].StartAdresse) then begin
                  if ((iStartAdresse - Primus_MBRegisterDef_Mess [i].StartAdresse) MOD
                      C_Primus_MBRegisterSize_Mess) = 0 then begin
                    Result:=Primus_MBRegisterDef_Mess [i].KanalDef;
                    Break;
                  end;
                end;
              end;
            end;

          mrgtyp_Prilog:  // 11.02.2022, WW
            begin
              if Assigned (MBAbrufData.MBRegisterDefList) then begin
                for i:=0 to MBAbrufData.MBRegisterDefList.Count - 1 do begin
                  MBRegisterDef:=TMBRegisterDefObj (MBAbrufData.MBRegisterDefList [i]).Data;
                  // Startadresse aller Archiv-Datens�tze aus MBAbruf-Konfiguration:
                  if (iStartAdresse >= MBRegisterDef.StartAdresse) then begin
                    if ((iStartAdresse - MBRegisterDef.StartAdresse) MOD
                        MBAbrufData.RecSize) = 0 then begin
                      Result:=MBRegisterDef.KanalDef;
                      Break;
                    end;
                  end;
                end;
              end;
            end;

          mrgtyp_TME400_VCF:
            begin
              for i:=Low (TME400_VCF_MBRegisterDef_Mess) to
                     High (TME400_VCF_MBRegisterDef_Mess) do begin
                // Startadresse aller Periodenarchiv-Datens�tze des TME400-VCF:
                if (iStartAdresse >= TME400_VCF_MBRegisterDef_Mess [i].StartAdresse) then begin
                  if ((iStartAdresse - TME400_VCF_MBRegisterDef_Mess [i].StartAdresse) MOD
                      C_TME400_MBRegisterSize_Mess) = 0 then begin
                    Result:=TME400_VCF_MBRegisterDef_Mess [i].KanalDef;
                    Break;
                  end;
                end;
              end;
            end;

          mrgtyp_TME400_VMF:
            begin
              for i:=Low (TME400_VMF_MBRegisterDef_Mess) to
                     High (TME400_VMF_MBRegisterDef_Mess) do begin
                // Startadresse aller Periodenarchiv-Datens�tze des TME400-VMF:
                if (iStartAdresse >= TME400_VMF_MBRegisterDef_Mess [i].StartAdresse) then begin
                  if ((iStartAdresse - TME400_VMF_MBRegisterDef_Mess [i].StartAdresse) MOD
                      C_TME400_MBRegisterSize_Mess) = 0 then begin
                    Result:=TME400_VMF_MBRegisterDef_Mess [i].KanalDef;
                    Break;
                  end;
                end;
              end;
            end;

          mrgtyp_RSM200_VCF:
            begin
              for i:=Low (RSM200_VCF_MBRegisterDef_Mess) to
                     High (RSM200_VCF_MBRegisterDef_Mess) do begin
                // Startadresse aller Periodenarchiv-Datens�tze des RSM200-VCF:
                if (iStartAdresse >= RSM200_VCF_MBRegisterDef_Mess [i].StartAdresse) then begin
                  if ((iStartAdresse - RSM200_VCF_MBRegisterDef_Mess [i].StartAdresse) MOD
                      C_RSM200_MBRegisterSize_Mess) = 0 then begin
                    Result:=RSM200_VCF_MBRegisterDef_Mess [i].KanalDef;
                    Break;
                  end;
                end;
              end;
            end;

          mrgtyp_RSM200_VMF:
            begin
              for i:=Low (RSM200_VMF_MBRegisterDef_Mess) to
                     High (RSM200_VMF_MBRegisterDef_Mess) do begin
                // Startadresse aller Periodenarchiv-Datens�tze des RSM200-VMF:
                if (iStartAdresse >= RSM200_VMF_MBRegisterDef_Mess [i].StartAdresse) then begin
                  if ((iStartAdresse - RSM200_VMF_MBRegisterDef_Mess [i].StartAdresse) MOD
                      C_RSM200_MBRegisterSize_Mess) = 0 then begin
                    Result:=RSM200_VMF_MBRegisterDef_Mess [i].KanalDef;
                    Break;
                  end;
                end;
              end;
            end;

        end;
      end;  // at_Periodenarchiv

    at_Ereignisarchiv:
      begin
        case AMrgTyp of
          mrgtyp_Primus:
            begin
              for i:=Low (Primus_MBRegisterDef_Meld) to
                     High (Primus_MBRegisterDef_Meld) do begin
                // Startadresse aller Statusarchiv-Datens�tze des Primus:
                if (iStartAdresse >= Primus_MBRegisterDef_Meld [i].StartAdresse) then begin
                  if ((iStartAdresse - Primus_MBRegisterDef_Meld [i].StartAdresse) MOD
                      C_Primus_MBRegisterSize_Meld) = 0 then begin
                    Result:=Primus_MBRegisterDef_Meld [i].KanalDef;
                    Break;
                  end;
                end;
              end;
            end;

          mrgtyp_Prilog:  // 11.02.2022, WW
            begin
              if Assigned (MBAbrufData.MBRegisterDefList) then begin
                for i:=0 to MBAbrufData.MBRegisterDefList.Count - 1 do begin
                  MBRegisterDef:=TMBRegisterDefObj (MBAbrufData.MBRegisterDefList [i]).Data;
                  // Startadresse aller Archiv-Datens�tze aus MBAbruf-Konfiguration:
                  if (iStartAdresse >= MBRegisterDef.StartAdresse) then begin
                    if ((iStartAdresse - MBRegisterDef.StartAdresse) MOD
                        MBAbrufData.RecSize) = 0 then begin
                      Result:=MBRegisterDef.KanalDef;
                      Break;
                    end;
                  end;
                end;
              end;
            end;

          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF:
            begin
              for i:=Low (TME400_MBRegisterDef_Ereignis) to
                     High (TME400_MBRegisterDef_Ereignis) do begin
                // Startadresse aller Ereignisarchiv-Datens�tze des TME400:
                if (iStartAdresse >= TME400_MBRegisterDef_Ereignis [i].StartAdresse) then begin
                  if ((iStartAdresse - TME400_MBRegisterDef_Ereignis [i].StartAdresse) MOD
                      C_TME400_MBRegisterSize_Ereignis) = 0 then begin
                    Result:=TME400_MBRegisterDef_Ereignis [i].KanalDef;
                    Break;
                  end;
                end;
              end;
            end;

          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF:
            begin
              for i:=Low (RSM200_MBRegisterDef_Ereignis) to
                     High (RSM200_MBRegisterDef_Ereignis) do begin
                // Startadresse aller Ereignisarchiv-Datens�tze des RSM200:
                if (iStartAdresse >= RSM200_MBRegisterDef_Ereignis [i].StartAdresse) then begin
                  if ((iStartAdresse - RSM200_MBRegisterDef_Ereignis [i].StartAdresse) MOD
                      C_RSM200_MBRegisterSize_Ereignis) = 0 then begin
                    Result:=RSM200_MBRegisterDef_Ereignis [i].KanalDef;
                    Break;
                  end;
                end;
              end;
            end;

        end;
      end;  // at_Ereignisarchiv

    at_Parameterarchiv_eichamtlich,
    at_Parameterarchiv_nichteichamtlich:
      begin
        case AMrgTyp of
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF:
            begin
              for i:=Low (TME400_MBRegisterDef_ParaAend) to
                     High (TME400_MBRegisterDef_ParaAend) do begin
                // Startadresse aller Parameterarchiv-Datens�tze des TME400:
                if (iStartAdresse >= TME400_MBRegisterDef_ParaAend [i].StartAdresse) then begin
                  if ((iStartAdresse - TME400_MBRegisterDef_ParaAend [i].StartAdresse) MOD
                      C_TME400_MBRegisterSize_ParaAend) = 0 then begin
                    Result:=TME400_MBRegisterDef_ParaAend [i].KanalDef;
                    Break;
                  end;
                end;
              end;
            end;
                
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF:
            begin
              for i:=Low (RSM200_MBRegisterDef_ParaAend) to
                     High (RSM200_MBRegisterDef_ParaAend) do begin
                // Startadresse aller Parameterarchiv-Datens�tze des RSM200:
                if (iStartAdresse >= RSM200_MBRegisterDef_ParaAend [i].StartAdresse) then begin
                  if ((iStartAdresse - RSM200_MBRegisterDef_ParaAend [i].StartAdresse) MOD
                      C_RSM200_MBRegisterSize_ParaAend) = 0 then begin
                    Result:=RSM200_MBRegisterDef_ParaAend [i].KanalDef;
                    Break;
                  end;
                end;
              end;
            end;
                
        end;
      end;  // at_Parameterarchiv_eichamtlich, at_Parameterarchiv_nichteichamtlich

  end;
end;

{-----------------------------------------------------------------------------------}
function GetStartAdresse_MBRegister_Archiv (AMrgTyp: integer; AArchivtyp: TArchivtyp;
  sKanalDef: string; iRecOffset: word; MBAbrufData: TMBAbrufData): word;
{-----------------------------------------------------------------------------------}
{ Liefert Ger�tetyp-abh�ngig die Modbus-Startadresse zu der Kanaldefinition eines
  Werts in einem Archiv-Datensatz;
  �bergaben: Ger�tetyp
             Archivtyp
             Kanal-Definition
             Datensatz-Offset (0 = j�ngster Datensatz, 1 = 2.-j�ngster usw.)
             MBAbrufData-Record mit Modbusregister-Konfiguration des Archivs
  Ergebnis: Startadresse (0, wenn nicht gefunden) }
var
  i: integer;
  MBRegisterDef: TMBRegisterDef;

begin
  Result:=0;  // Vorbelegung: Startadresse unbekannt

  case AArchivtyp of
    at_Periodenarchiv:
      begin
        case AMrgTyp of
          mrgtyp_Primus:
            begin
              for i:=Low (Primus_MBRegisterDef_Mess) to
                     High (Primus_MBRegisterDef_Mess) do begin
                if Primus_MBRegisterDef_Mess [i].KanalDef = sKanalDef then begin
                  // Startadresse abh�ngig vom Datensatz-Offset:
                  Result:=Primus_MBRegisterDef_Mess [i].StartAdresse +
                    (C_Primus_MBRegisterSize_Mess * iRecOffset);
                  Break;
                end;
              end;
            end;

          mrgtyp_Prilog:  // 11.02.2022, WW
            begin
              if Assigned (MBAbrufData.MBRegisterDefList) then begin
                for i:=0 to MBAbrufData.MBRegisterDefList.Count - 1 do begin
                  MBRegisterDef:=TMBRegisterDefObj (MBAbrufData.MBRegisterDefList [i]).Data;
                  if MBRegisterDef.KanalDef = sKanalDef then begin
                    // Startadresse abh�ngig vom Datensatz-Offset:
                    Result:=MBRegisterDef.StartAdresse + (MBAbrufData.RecSize * iRecOffset);
                    Break;
                  end;
                end;
              end;
            end;

          mrgtyp_TME400_VCF:
            begin
              for i:=Low (TME400_VCF_MBRegisterDef_Mess) to
                     High (TME400_VCF_MBRegisterDef_Mess) do begin
                if TME400_VCF_MBRegisterDef_Mess [i].KanalDef = sKanalDef then begin
                  // Startadresse abh�ngig vom Datensatz-Offset:
                  Result:=TME400_VCF_MBRegisterDef_Mess [i].StartAdresse + iRecOffset;
                  Break;
                end;
              end;
            end;

          mrgtyp_TME400_VMF:
            begin
              for i:=Low (TME400_VMF_MBRegisterDef_Mess) to
                     High (TME400_VMF_MBRegisterDef_Mess) do begin
                if TME400_VMF_MBRegisterDef_Mess [i].KanalDef = sKanalDef then begin
                  // Startadresse abh�ngig vom Datensatz-Offset:
                  Result:=TME400_VMF_MBRegisterDef_Mess [i].StartAdresse + iRecOffset;
                  Break;
                end;
              end;
            end;

          mrgtyp_RSM200_VCF:
            begin
              for i:=Low (RSM200_VCF_MBRegisterDef_Mess) to
                     High (RSM200_VCF_MBRegisterDef_Mess) do begin
                if RSM200_VCF_MBRegisterDef_Mess [i].KanalDef = sKanalDef then begin
                  // Startadresse abh�ngig vom Datensatz-Offset:
                  Result:=RSM200_VCF_MBRegisterDef_Mess [i].StartAdresse + iRecOffset;
                  Break;
                end;
              end;
            end;

          mrgtyp_RSM200_VMF:
            begin
              for i:=Low (RSM200_VMF_MBRegisterDef_Mess) to
                     High (RSM200_VMF_MBRegisterDef_Mess) do begin
                if RSM200_VMF_MBRegisterDef_Mess [i].KanalDef = sKanalDef then begin
                  // Startadresse abh�ngig vom Datensatz-Offset:
                  Result:=RSM200_VMF_MBRegisterDef_Mess [i].StartAdresse + iRecOffset;
                  Break;
                end;
              end;
            end;

        end;
      end;  // at_Periodenarchiv

    at_Ereignisarchiv:
      begin
        case AMrgTyp of
          mrgtyp_Primus:
            begin
              for i:=Low (Primus_MBRegisterDef_Meld) to
                     High (Primus_MBRegisterDef_Meld) do begin
                if Primus_MBRegisterDef_Meld [i].KanalDef = sKanalDef then begin
                  // Startadresse abh�ngig vom Datensatz-Offset:
                  Result:=Primus_MBRegisterDef_Meld [i].StartAdresse +
                    (C_Primus_MBRegisterSize_Meld * iRecOffset);
                  Break;
                end;
              end;
            end;

          mrgtyp_Prilog:  // 11.02.2022, WW
            begin
              if Assigned (MBAbrufData.MBRegisterDefList) then begin
                for i:=0 to MBAbrufData.MBRegisterDefList.Count - 1 do begin
                  MBRegisterDef:=TMBRegisterDefObj (MBAbrufData.MBRegisterDefList [i]).Data;
                  if MBRegisterDef.KanalDef = sKanalDef then begin
                    // Startadresse abh�ngig vom Datensatz-Offset:
                    Result:=MBRegisterDef.StartAdresse + (MBAbrufData.RecSize * iRecOffset);
                    Break;
                  end;
                end;
              end;
            end;

          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF:
            begin
              for i:=Low (TME400_MBRegisterDef_Ereignis) to
                     High (TME400_MBRegisterDef_Ereignis) do begin
                if TME400_MBRegisterDef_Ereignis [i].KanalDef = sKanalDef then begin
                  // Startadresse abh�ngig vom Datensatz-Offset:
                  Result:=TME400_MBRegisterDef_Ereignis [i].StartAdresse + iRecOffset;
                  Break;
                end;
              end;
            end;

          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF:
            begin
              for i:=Low (RSM200_MBRegisterDef_Ereignis) to
                     High (RSM200_MBRegisterDef_Ereignis) do begin
                if RSM200_MBRegisterDef_Ereignis [i].KanalDef = sKanalDef then begin
                  // Startadresse abh�ngig vom Datensatz-Offset:
                  Result:=RSM200_MBRegisterDef_Ereignis [i].StartAdresse + iRecOffset;
                  Break;
                end;
              end;
            end;

        end;
      end;  // at_Ereignisarchiv

    at_Parameterarchiv_eichamtlich,
    at_Parameterarchiv_nichteichamtlich:
      begin
        case AMrgTyp of
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF:
            begin
              for i:=Low (TME400_MBRegisterDef_ParaAend) to
                     High (TME400_MBRegisterDef_ParaAend) do begin
                if TME400_MBRegisterDef_ParaAend [i].KanalDef = sKanalDef then begin
                  // Startadresse abh�ngig vom Datensatz-Offset:
                  Result:=TME400_MBRegisterDef_ParaAend [i].StartAdresse + iRecOffset;
                  Break;
                end;
              end;
            end;

          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF:
            begin
              for i:=Low (RSM200_MBRegisterDef_ParaAend) to
                     High (RSM200_MBRegisterDef_ParaAend) do begin
                if RSM200_MBRegisterDef_ParaAend [i].KanalDef = sKanalDef then begin
                  // Startadresse abh�ngig vom Datensatz-Offset:
                  Result:=RSM200_MBRegisterDef_ParaAend [i].StartAdresse + iRecOffset;
                  Break;
                end;
              end;
            end;

        end;
      end;  // at_Parameterarchiv_eichamtlich, at_Parameterarchiv_nichteichamtlich:

  end;
end;

{---------------------------------------------------------------------}
function GetMB_ArchivRecSize (AMrgTyp: integer; AArchivtyp: TArchivtyp;
  MBAbrufData: TMBAbrufData): word;
{---------------------------------------------------------------------}
{ Liefert Ger�tetyp-abh�ngig die Register-Gr��e eines Modbus-Archivdatensatzes;
  �bergaben: Ger�tetyp
             Archivtyp
             MBAbrufData-Record mit Modbusregister-Konfiguration des Archivs
  Ergebnis: Register-Gr��e des Modbus-Archivdatensatzes (0, wenn unbekannt) }
begin
  Result:=0;  // Vorbelegung: Register-Gr��e unbekannt

  case AArchivtyp of
    at_Periodenarchiv:
      begin
        case AMrgTyp of
          mrgtyp_Primus: Result:=C_Primus_MBRegisterSize_Mess;
          mrgtyp_Prilog:  // 11.02.2022, WW
            Result:=MBAbrufData.RecSize;
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF: Result:=C_TME400_MBRegisterSize_Mess;
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF: Result:=C_RSM200_MBRegisterSize_Mess;
        end;
      end;

    at_Ereignisarchiv:
      begin
        case AMrgTyp of
          mrgtyp_Primus: Result:=C_Primus_MBRegisterSize_Meld;
          mrgtyp_Prilog:  // 11.02.2022, WW
            Result:=MBAbrufData.RecSize;
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF: Result:=C_TME400_MBRegisterSize_Ereignis;
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF: Result:=C_RSM200_MBRegisterSize_Ereignis;
        end;
      end;

    at_Parameterarchiv_eichamtlich,
    at_Parameterarchiv_nichteichamtlich:
      begin
        case AMrgTyp of
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF: Result:=C_TME400_MBRegisterSize_ParaAend;
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF: Result:=C_RSM200_MBRegisterSize_ParaAend;
        end;
      end;
  end;
end;

{-------------------------------------------------------------------------}
function GetMB_MaxArchivRecCount (AMrgTyp: integer; AArchivtyp: TArchivtyp;
  MBAbrufData: TMBAbrufData): word;
{-------------------------------------------------------------------------}
{ Liefert Ger�tetyp-abh�ngig die maximal m�gliche Anzahl an adressierbaren
  Datens�tzen eines Modbus-Archivs;
  �bergaben: Ger�tetyp
             Archivtyp
             MBAbrufData-Record mit Modbusregister-Konfiguration des Archivs
  Ergebnis: Anzahl der Archiv-Datens�tze (0, wenn unbekannt) }
begin
  Result:=0;  // Vorbelegung: Anzahl Archiv-Datens�tze unbekannt

  case AArchivtyp of
    at_Periodenarchiv:
      begin
        case AMrgTyp of
          mrgtyp_Primus: Result:=C_Primus_MBRegisterCount_Mess;
          mrgtyp_Prilog:  // 11.02.2022, WW
            Result:=MBAbrufData.RecCount;
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF: Result:=C_TME400_MBRegisterCount_Mess;
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF: Result:=C_RSM200_MBRegisterCount_Mess;
        end;
      end;

    at_Ereignisarchiv:
      begin
        case AMrgTyp of
          mrgtyp_Primus: Result:=C_Primus_MBRegisterCount_Meld;
          mrgtyp_Prilog:  // 11.02.2022, WW
            Result:=MBAbrufData.RecCount;
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF: Result:=C_TME400_MBRegisterCount_Ereignis;
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF: Result:=C_RSM200_MBRegisterCount_Ereignis;
        end;
      end;

    at_Parameterarchiv_eichamtlich,
    at_Parameterarchiv_nichteichamtlich:
      begin
        case AMrgTyp of
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF: Result:=C_TME400_MBRegisterCount_ParaAend;
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF: Result:=C_RSM200_MBRegisterCount_ParaAend;
        end;
      end;
  end;
end;

{----------------------------------------------------------}
function GetMB_RequestFileNr_Archivheader (AMrgTyp: integer;
  AArchivtyp: TArchivtyp): word;
{----------------------------------------------------------}
{ Liefert die Filenummer f�r einen Modbus-Request zum Lesen der Archivheader
  eines Archivs;
  �bergaben: Ger�tetyp
             Archivtyp
  Ergebnis: Filenummer (0, wenn unbekannt) }
begin
  Result:=0;  // Vorbelegung: Filenummer unbekannt

  case AArchivtyp of
    at_Periodenarchiv:
      begin
        case AMrgTyp of
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF,
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF: Result:=7;
        end;
      end;

    at_Ereignisarchiv:
      begin
        case AMrgTyp of
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF,
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF: Result:=5;
        end;
      end;

    at_Parameterarchiv_eichamtlich:
      begin
        case AMrgTyp of
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF,
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF: Result:=1;
        end;
      end;

    at_Parameterarchiv_nichteichamtlich:
      begin
        case AMrgTyp of
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF,
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF: Result:=3;
        end;
      end;
  end;
end;                                        

{----------------------------------------------------}
function GetMB_RequestFileNr_Archiv (AMrgTyp: integer;
  AArchivtyp: TArchivtyp): word;
{----------------------------------------------------}
{ Liefert die Filenummer f�r einen Modbus-Request zum Lesen der Datens�tze
  eines Archivs;
  �bergaben: Ger�tetyp
             Archivtyp
  Ergebnis: Filenummer (0, wenn unbekannt) }
begin
  Result:=0;  // Vorbelegung: Filenummer unbekannt

  case AArchivtyp of
    at_Periodenarchiv:
      begin
        case AMrgTyp of
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF,
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF: Result:=8;
        end;
      end;

    at_Ereignisarchiv:
      begin
        case AMrgTyp of
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF,
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF: Result:=6;
        end;
      end;

    at_Parameterarchiv_eichamtlich:
      begin
        case AMrgTyp of
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF,
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF: Result:=2;
        end;
      end;

    at_Parameterarchiv_nichteichamtlich:
      begin
        case AMrgTyp of
          mrgtyp_TME400_VCF,
          mrgtyp_TME400_VMF,
          mrgtyp_RSM200_VCF,
          mrgtyp_RSM200_VMF: Result:=4;
        end;
      end;
  end;
end;

end.
