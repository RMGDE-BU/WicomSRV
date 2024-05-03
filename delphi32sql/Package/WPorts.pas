{******************************************************************************}
{* Unit: Standard-IP-Ports für Wico22-Programme                               *}
{* 27.11.2006  WW                                                             *}
{******************************************************************************}
unit WPorts;

interface

const
  CIPPortWMsg_ab_Default  = 9000;  { Standardeinstellung für kleinsten IP-Serverport
                                     für Wieser-Benachrichtigung }

  { Standardeinstellungen für IP-Serverports in Wico-Programmen/Diensten: }
  CIPPortWicomSrv_Default      = 9999;  { IP-Serverport im WicomSrv }
  CIPPortGPRSSrv_Default       = 9998;  { IP-Serverport im GPRS-Server }
  CIPPort_mGDM_Default         = 9997;  { IP-Serverport für mobile Gasdichtemessung (ERG) }
  CIPPortWicomSrv_GPRS_Default = 9996;  { IP-Serverport im WicomSrv für GPRS }
  CIPPortSignaturSrv_Default   = 9995;  { IP-Serverport im Signaturserver }
  CIPPortIEC_RM_Default        = 9994;  { IP-Serverport in IEC-Kopplung für Remote-Monitoring }    
  CIPPortWicomSrv_RufDSfG_Default = 9993;  { IP-Serverport im WicomSrv für DSfG-Rufentgegennahme }

  CIPPortIEC870_104_Default    = 2404;  { IP-Serverport in IEC-Kopplung (IEC 870-5-104) }

implementation

end.
