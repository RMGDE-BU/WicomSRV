{******************************************************************************}
{* Unit: Datenelementeadressen - allgemeiner Teil                             *}
{* 27.12.1999 WW                                                              *}
{******************************************************************************}
Unit DD_Allg;

interface

Const
  { DSfG }

  CD_ALLG_eigInstanztyp = 'aaa';


  { Typenschild }

  CD_ALLG_Hersteller     = 'aba';
  CD_ALLG_Geraetetyp     = 'abb';
  CD_ALLG_FabrikNr       = 'abc';
  CD_ALLG_Baujahr        = 'abd';
  CD_ALLG_SWVersion      = 'abe';
  CD_ALLG_Inbetriebnahme = 'abf';

  CD_ERZ2000_SubType     = 'zebas';

  { Zeitangaben }

  CD_ALLG_DatumUhrzeit = 'aca';
  CD_ALLG_Zeitzone     = 'acb';
  CD_ALLG_LetztVerstZz = 'acc';


  { Benutzerdaten }

  CD_ALLG_Zugangscode1 = 'add';
  CD_ALLG_Zugangscode2 = 'ade';
  CD_ALLG_Eichschalter = 'adf';

  { Betriebsarten }
  CD_ALLG_ArchivZeilenweiseAuslesbar = 'agcb';


  { Hersteller-spezifisch: Wieser }

  CD_WIESER_Ausg1_Zustand = 'wfacb';
  CD_WIESER_Ausg2_Zustand = 'wfadb';
  

  { Virtuelle DEAs für Gerätezustand }
  CD_ALLG_MRG900_GerZustand_aktuell = 'WAAAA';  // MRG 905/910

implementation

end.
