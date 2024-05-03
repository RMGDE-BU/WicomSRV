
{*******************************************************}
{                                                       }
{       Delphi Runtime Library                          }
{                                                       }
{       Partial translation of DBT.H                    }
{                                                       }
{       version : 4.00                                  }
{       Date : 24 May 1993                              }
{       Copyright (c) 1993-1996 Microsoft Corporation   }
{                                                       }
{       Equates for WM_DEVICECHANGE                     }
{                                                       }
{       Copyright (p) 1998 ZifNab (T. Deprez            }
{                                                       }
{*******************************************************}

{*******************************************************}
{ [History :]                                           }
{   18-01-1988 first release                            }
{   15-04-2005 WW, weitere Event- und Device-Konstanten }
{                                                       }
{ [FUTURE :]                                            }
{   - further translation of DBT.H                      }
{                                                       }
{*******************************************************}

unit DBTMsg;

interface

uses
  windows;

const
  // Events of WM_DEVICECHANGE (wParam)
  DBT_DEVICEARRIVAL           = $8000;  // system detected a new device
  DBT_DEVICEQUERYREMOVE       = $8001;  // wants to remove, may fail (usually dialog is shown)
  DBT_DEVICEQUERYREMOVEFAILED = $8002;  // removal aborted
  DBT_DEVICEREMOVEPENDING     = $8003;  // about to remove, still avail
  DBT_DEVICEREMOVECOMPLETE    = $8004;  // device is gone
  DBT_DEVICETYPESPECIFIC      = $8005;  // type specific event
  DBT_CUSTOMEVENT             = $8006;  // user-defined event

  DBT_DEVNODES_CHANGED        = $0007;  // a device has been added to or removed from the system
  DBT_QUERYCHANGECONFIG       = $0017;  // Windows requests permission to change the current configuration (dock or undock)
  DBT_CONFIGCHANGED           = $0018;  // the current configuration has changed, due to a dock or undock
  DBT_CONFIGCHANGECANCELED    = $0019;  // a request to change the current configuration has been canceled

  DBT_USERDEFINED	      = $FFFF;  // user-defined system message

  // type of device in DEV_BROADCAST_HDR
  DBT_DEVTYP_OEM             = $00000000;  // OEM- or IHV-defined device
  DBT_DEVTYP_DEVNODE         = $00000001;  // Devnode number (hub)
  DBT_DEVTYP_VOLUME          = $00000002;  // Logical volume
  DBT_DEVTYP_PORT            = $00000003;  // Port (serial or parallel)
  DBT_DEVTYP_NET             = $00000004;  // Network resource
  DBT_DEVTYP_DEVICEINTERFACE = $00000005;  // device interface class
  DBT_DEVTYP_HANDLE          = $00000006;  // file system handle

  // media types in DBT_DEVTYP_VOLUME
  DBTF_MEDIA = $0001;  // change affects media in drive
  DBTF_NET   = $0002;  // logical volume is network volume

type
  // window structures

  TWMDeviceChange = record
    Msg : Cardinal;
    Event : UINT;
    dwData : Pointer;
    Result : LongInt;
  end;

  // In a DBT_DEVICECHANGE or DBT_DEVICEREMOVECOMPLETE event, dwData contains an
  // address of a DEV_BROADCAST_HDR structure identifying the device inserted.

  PDEV_BROADCAST_HDR = ^TDEV_BROADCAST_HDR;
  TDEV_BROADCAST_HDR = packed record
    dbch_size : DWORD;
    dbch_devicetype : DWORD;
    dbch_reserved : DWORD;
  end;

  // When the device is of type volume, then we can get some device specific
  // information, namely specific information about a logical volume.

  PDEV_BROADCAST_VOLUME = ^TDEV_BROADCAST_VOLUME;
  TDEV_BROADCAST_VOLUME = packed record
    dbcv_size : DWORD;
    dbcv_devicetype : DWORD;
    dbcv_reserved : DWORD;
    dbcv_unitmask : DWORD;
    dbcv_flags : WORD;
  end;

implementation

end.
