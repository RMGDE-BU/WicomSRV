{******************************************************************************}
{* Unit: Allgemeine Parameternummern für RMG-Geräte                           *}
{* 09.06.2009 WW  EC 900                                                      *}
{* 08.02.2019 WW  Primus                                                      *}
{* 22.07.2019 WW  Primus, neue Zuordnung                                      *}
{* 18.02.2021 WW  TME400                                                      *}
{* 09.01.2024 WW  RSM200                                                      *}
{* 04.04.2024 WW  Virtuelle Gerätezustand-Parameter TME400 und RSM200         *}
{******************************************************************************}
Unit MP_RMG;

interface

Const
  { EC 900: }

  { Gerätezeit: }
  CP_RMG_EC900_Systemzeit = '190200004';
  CP_RMG_EC900_TimeZone   = '190200006';
  CP_RMG_EC900_DiffToUTC  = '190200074';
  CP_RMG_EC900_DiffToUTC2 = '190200153';

  { Setzeingang Datum- und Zeitsychronisation: }
  CP_RMG_EC900_SetzEing_DZSync = '190201236';

  { Zählerfaktor Messkanal: }
  CP_RMG_EC900_Zaehlerfaktor_Mess = '190201368';

  { Meßbereiche Druck: }
  CP_RMG_EC900_AlarmGWunten_P = '190201164';
  CP_RMG_EC900_AlarmGWoben_P  = '190201165';

  { Meßbereiche Temperatur: }
  CP_RMG_EC900_AlarmGWunten_T = '190201180';
  CP_RMG_EC900_AlarmGWoben_T  = '190201181';


  { Primus/Prilog 400: }

  { Fixe Parameter (Gerätetyp- und Geräteversions-unabhängig): }
  CP_RMG_PrimusPrilog_FWVersion_App  = '190300002';
  CP_RMG_PrimusPrilog_FWVersion_Eich = '190300004';
  CP_RMG_PrimusPrilog_ModbuslisteID  = '190300007';

  { Variable Parameter (Gerätetyp- oder Geräteversions-abhängig):
    -> Neue Zuordnung; 22.07.2019, WW }
  CP_RMG_PrimusPrilog_Systemzeit   = '190301001';
  CP_RMG_PrimusPrilog_Kennung      = '190301056';
  CP_RMG_PrimusPrilog_Tagesende    = '190301059';
  CP_RMG_PrimusPrilog_Passwort     = '190301062';
  CP_RMG_PrimusPrilog_Sommerzeit   = '190301094';
  CP_RMG_PrimusPrilog_ArchivZeiger = '190301131';
  CP_RMG_PrimusPrilog_Binaereingang_4 = '190301139';
  CP_RMG_PrimusPrilog_Impulseingang_4 = '190301146';  // nur bei Modbusliste-ID 17 (Energie Steiermark)


  { TME400: }
  CP_RMG_TME400_Kennung  = '190407006';  // Messstelle
  CP_RMG_TME400_Unixzeit = '190409003';  // Unix-Gerätezeit

  CP_RMG_TME400_Fehlerregister  = '190411003';  // Fehlerregister
  CP_RMG_TME400_Statusregister  = '190411004';  // Statusregister
  CP_RMG_TME400_Warnungregister = '190411018';  // Warnungregister
  CP_RMG_TME400_Hinweisregister = '190411019';  // Hinweisregister


  { RSM200: }
  CP_RMG_RSM200_Kennung  = '190507006';  // Messstelle
  CP_RMG_RSM200_Unixzeit = '190521003';  // Unix-Gerätezeit

  CP_RMG_RSM200_Fehlerregister  = '190523001';  // Fehlerregister
  CP_RMG_RSM200_Statusregister  = '190523002';  // Statusregister
  CP_RMG_RSM200_Warnungregister = '190523013';  // Warnungregister
  CP_RMG_RSM200_Hinweisregister = '190523014';  // Hinweisregister

  CP_RMG_RSM200_Vn    = '190501001';  // Normvolumen
  CP_RMG_RSM200_Vb    = '190501002';  // Betriebsvolumen
  CP_RMG_RSM200_VnErr = '190501003';  // Normvolumen Error
  CP_RMG_RSM200_VbErr = '190501004';  // Betriebsvolumen Error
  CP_RMG_RSM200_VbTot = '190501005';  // Betriebsvolumen Total
  
  CP_RMG_RSM200_ZaehlerAufloesExp = '190501006';  // Zählerauflösung Exponent


  { Gerätezustände (Virtuelle Parameter): }

  { TME400: }
  CP_RMG_TME400_GerZustand_aktuell = '190001001';

  { RSM200: }
  CP_RMG_RSM200_GerZustand_aktuell = '190002001';


function IsRSM200_VolumeParaNrAllg (sParaNrAllg: string): boolean;
function IsRSM200_VolumeFactorParaNrAllg (sParaNrAllg: string): boolean;

implementation

{----------------------------------------------------------------}
function IsRSM200_VolumeParaNrAllg (sParaNrAllg: string): boolean;
{----------------------------------------------------------------}
{ Prüft auf RSM200 Zähler-Parameter (Vb/Vn);
  Übergabe: Allgemeine MRG-Parameternummer
  Ergebnis: True, wenn RSM200 Zähler-Parameter }
begin
  Result := (sParaNrAllg = CP_RMG_RSM200_Vn) OR
            (sParaNrAllg = CP_RMG_RSM200_Vb) OR
            (sParaNrAllg = CP_RMG_RSM200_VnErr) OR
            (sParaNrAllg = CP_RMG_RSM200_VbErr) OR
            (sParaNrAllg = CP_RMG_RSM200_VbTot);
end;

{----------------------------------------------------------------------}
function IsRSM200_VolumeFactorParaNrAllg (sParaNrAllg: string): boolean;
{----------------------------------------------------------------------}
{ Prüft auf RSM200 Zählerfaktor-Parameter (Zählerauflösung Exponent);
  Übergabe: Allgemeine MRG-Parameternummer
  Ergebnis: True, wenn RSM200 Zählerfaktor-Parameter }
begin
  Result := (sParaNrAllg = CP_RMG_RSM200_ZaehlerAufloesExp);
end;

end.
