//------------------------------------------------------------------------------
// Kapselung von FTP-Funktionen
//
// 31.05.2010  GD  Neu - dezidierte Veröffentlichung IdFTP-Funktionen
// 04.09.2010  GD  Erweitert um Kopieren von Dateien
//
// Copyright (C) Fa. Geert Dade 2010
//------------------------------------------------------------------------------
unit GD_FTP;

interface

uses
  SysUtils, Classes, Forms,
  IdFTP, IdFTPList,
  O_GDObject;

type
  TGDIdFTP = class(TGDObject)
  private
    FIdFTP          : TIdFTP;
    FConnectTimeOut : integer;
    FConnected      : boolean;
  protected
    procedure MyInitializeObject(bState: boolean); override;
    function GetListing: TIdFTPListItems; virtual;
  public
    function InitFTP(
      const sHost, sUser, sPassword: string; iPort: integer): boolean;
    function ConnectFTP: boolean;
    function DisconnectFTP: boolean;

    function ChangeDir(const sDirectory: string): boolean; virtual;
    function List(sMask: string = '*'): boolean; overload; virtual;
    function List(pSl: TStrings; sMask: string = '*'): boolean; overload; virtual;
    function Put(const sFileName: string): boolean; virtual;

    property FTP: TIdFTP read FIdFTP;
    property ConnectTimeOut: integer write FConnectTimeOut;
    property Connected: boolean read FConnected;
    property Listing: TIdFTPListItems read GetListing;
  end;

implementation

const
  C_FTP_TimeOut_Default = 60000;  // Timeout in ms;

//---------------------------------- TGDIdFTP ----------------------------------

// Initialisieren bzw. Freigeben des Objekts
//---------------------------------------------
procedure TGDIdFTP.MyInitializeObject(bState: boolean);
//---------------------------------------------
begin
  inherited;

  if (LastError = '') then
  try
    if (bState) then begin
      FIdFTP := TIdFTP.Create(nil);  // FTP-Objekt initialisiern
      FConnectTimeOut := C_FTP_TimeOut_Default; // Verbindungstimeout auf -1 (Default)
      FConnected := False;           // FTP nicht connected
    end
    else begin
      DisconnectFTP;                 // Ggf. disconnecten
      FConnected := False;           // nicht connected
      FreeAndNil(FIdFTP);            // FTP-Objekt freigeben
    end;
  except
    on E:Exception do begin
      HandleError('TGDIdFTP.MyInitializeObject: ' + E.Message);
    end;
  end;
end;

// FTP-Verbindung mit Basisparametern initialisieren
// Parameter: Host, Benutzer, Passwort, Portnummer
// Rückgabe: Erfolg ja / nein
//---------------------------------------------
function TGDIdFTP.InitFTP(
  const sHost, sUser, sPassword: string; iPort: integer): boolean;
//---------------------------------------------
begin
  try
    FIdFTP.Host := sHost;
    FIdFTP.Username := sUser;
    FIdFTP.Password := sPassword;
    FIdFTP.Port := iPort;
    Result := True;
  except
    on E:Exception do begin
      Result := False;
      HandleError('TGDIdFTP.InitFTP: ' + E.Message);
    end;
  end;
end;

// FTP-Verbindung öffnen
// Rückgabe: Erfolg ja / nein
//---------------------------------------------
function TGDIdFTP.ConnectFTP: boolean;
//---------------------------------------------
begin
  try
    DisconnectFTP;

    FIdFTP.Connect(True, FConnectTimeOut);
    Sleep(100);
    Application.ProcessMessages;
    Result := True;
  except
    on E:Exception do begin
      Result := False;
      HandleError('TGDIdFTP.ConnectFTP: ' + E.Message);
    end;
  end;

  FConnected := Result;
end;

// FTP-Verbindung schließen
// Rückgabe: Erfolg ja / nein
//---------------------------------------------
function TGDIdFTP.DisconnectFTP: boolean;
//---------------------------------------------
begin
  try
    if (FConnected) then begin
      FIdFTP.Disconnect;
      Sleep(100);
      Application.ProcessMessages;
    end;
    Result := True;
  except
    on E:Exception do begin
      Result := False;
      HandleError('TGDIdFTP.DisconnectFTP: ' + E.Message);
    end;
  end;

  FConnected := False;
end;

// FTP-Verzeichnis wechseln
// Parameter: Verzeichnisangabe
// Rückgabe: Erfolg ja / nein
//---------------------------------------------
function TGDIdFTP.ChangeDir(const sDirectory: string): boolean; 
//---------------------------------------------
begin
  try
    FIdFTP.ChangeDir(sDirectory);
    Result := True;
  except
    on E:Exception do begin
      Result := False;
      HandleError('TGDIdFTP.ChangeDir: ' + E.Message);
    end;
  end;
end;

// Verzeichnisinhalt in interne Directory-Liste laden
// Parameter: Dateimaske (optional)
// Rückgabe: Erfolg ja / nein
//---------------------------------------------
function TGDIdFTP.List(sMask: string = '*'): boolean;
//---------------------------------------------
var
  pSl : TStrings;
begin
  try
    pSl := TStringList.Create;
    try
      FIdFTP.List(pSl, sMask, True);
      Result := True;
    finally
      pSl.Free;
    end;
  except
    on E:Exception do begin
      Result := False;
      HandleError('TGDIdFTP.List: ' + E.Message);
    end;
  end;
end;

// Verzeichnisinhalt in interne Directory-Liste laden
// Parameter: Übergabeliste für Ergebnis (nur String-Information), Dateimaske
// Rückgabe: Erfolg ja / nein
//---------------------------------------------
function TGDIdFTP.List(pSl: TStrings; sMask: string = '*'): boolean;
//---------------------------------------------
begin
  try
    FIdFTP.List(pSl, sMask, True);
    Result := True;
  except
    on E:Exception do begin
      Result := False;
      HandleError('TGDIdFTP.List: ' + E.Message);
    end;
  end;
end;

// Datei in aktuelles FTP-Verzeichnnis kopieren
// Parameter: Dateiname mit Pfadangabe der zu kopiereneden Datei
// Rückgabe: Erfolg ja / nein
//---------------------------------------------
function TGDIdFTP.Put(const sFileName: string): boolean;
//---------------------------------------------
begin
  try
    FIdFTP.Put(sFileName, ExtractFileName(sFileName), False); // Überschreiben
    Result := True;
  except
    on E:Exception do begin
      Result := False;
      HandleError('TGDIdFTP.CopyFile: ' + E.Message);
    end;
  end;
end;

// Gibt aktuelle Verzeichnisliste (über LIST angefordert) zurück
// Rückgabe: Verzeichnisliste oder nil
//---------------------------------------------
function TGDIdFTP.GetListing: TIdFTPListItems;
//---------------------------------------------
begin
  try
    Result := FIdFTP.DirectoryListing;
  except
    on E:Exception do begin
      Result := nil;
      HandleError('TGDIdFTP.GetListing: ' + E.Message);
    end;
  end;
end;

end.
