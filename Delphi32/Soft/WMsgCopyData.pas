// -----------------------------------------------------------------------------
// Unit: Kennzeichner für WM_COPYDATA-Strukturen
// 12.05.2017  WW  Neu
// 28.02.2023  WW  Kennzeichner für Modbus-Server: Reload data file, Exe initia-
//                 lized; Funktionen zum Versenden
//
// Copyright © RMG Messtechnik GmbH 2011, 2023
// -----------------------------------------------------------------------------
unit WMsgCopyData;

interface

uses
  Windows, Messages, WErrMsg;

const
  // dwData-Kennzeichner
  C_dwData_Exe_Initialized = 100;  // Exe ist gestartet und vollständig initialisiert

  C_dwData_MBS_ReloadDataFile = 10001;  // Register-Definitionsdatei im WICO22 Modbus-Server neu laden

  C_dwData_StationEvent       = 11001;  // Stationsereignis

  C_dwData_GMMOnlineData      = 70001;  // GMM-Onlinedaten


function SendData_WindowsMessage (AHandle_Sender: HWND; AClassName_Receiver: string;
  ADataId: cardinal; ADataSize: cardinal; var AData;
  ATimeout: cardinal; var iMsgResult: cardinal; var sError: string): boolean;

implementation

// Daten versenden per Windows-Message über Handle des Empfängerfensters;
// Übergabe: Handle des Senders
//           Handle des Empfängerfensters
//           Kennzeichner zur Identifierzung der Daten auf Empfängerseite
//           Datengröße
//           Daten
//           Timeout für erfolgreiches Versenden in ms
// Rückgabe: Ergebniscode aus Antwort des Empfängers
// Ergebnis: true, wenn Versenden erfolgreich
// -----------------------------------------------------------------------------
function SendData_WindowsMessage_Handle (AHandle_Sender, AHandle_Receiver: HWND;
  ADataId: cardinal; ADataSize: cardinal; var AData;
  ATimeout: cardinal; var iMsgResult: cardinal): boolean;
// -----------------------------------------------------------------------------
var
  cds: TCopyDataStruct;

begin
  cds.dwData := ADataId;
  cds.cbData := ADataSize;
  cds.lpData := @AData;

  iMsgResult := 0;
  if SendMessageTimeout (AHandle_Receiver, WM_COPYDATA, AHandle_Sender,
                         Integer(@cds), SMTO_ABORTIFHUNG, ATimeout, iMsgResult) = 0 then
    Result := false  // Fehler SendMessageTimeout
  else
    Result := true;
end;

// Daten versenden per Windows-Message über Klassenname des Empfängerfensters;
// Übergabe: Handle des Senders
//           Klassenname des Empfängerfensters
//           Kennzeichner zur Identifierzung der Daten auf Empfängerseite
//           Datengröße
//           Daten
//           Timeout für erfolgreiches Versenden in ms
// Rückgabe: Ergebniscode aus Antwort des Empfängers
//           Fehlermeldung
// Ergebnis: true, wenn Versenden erfolgreich
// -----------------------------------------------------------------------------
function SendData_WindowsMessage (AHandle_Sender: HWND; AClassName_Receiver: string;
  ADataId: cardinal; ADataSize: cardinal; var AData;
  ATimeout: cardinal; var iMsgResult: cardinal; var sError: string): boolean;
// -----------------------------------------------------------------------------
var
  Handle_Receiver: HWND;

begin
  iMsgResult := 0;  // Default
  // Empfängerfenster wird über Klassenname gesucht:
  Handle_Receiver := FindWindow (pchar (AClassName_Receiver), nil);
  if (Handle_Receiver > 0) then begin
    if SendData_WindowsMessage_Handle (AHandle_Sender, Handle_Receiver, ADataId,
                                       ADataSize, AData, ATimeout, iMsgResult) then begin
      // Versenden der Message OK
      sError := '';
      Result := true;
    end
    else begin
      // Fehler beim Versenden der Message
      sError := 'Error sending message to ''' + AClassName_Receiver + ''': ' +
                 LastErrorStr;  // LastError mitausgeben
      Result := false;
    end;
  end
  else begin
    // Fehler FindWindow
    sError := 'Window ''' + AClassName_Receiver + ''' not found';
    Result := false;
  end;
end;

end.
