//------------------------------------------------------------------------------
// Mail-Objekt Prototyp von SM grundüberholt
//
// 2001        SM  Prototyp
// 16.01.2008  GD  Produktiv für INetData
// 24.11.2008  GD  Restmüll entsorgt
// 05.12.2008  GD  Um Outlook erweitert
// 09.12.2008  GD  Um "SMTP Relay" erweitert
// 10.02.2010  GD  Um Übergabe von Hostname bei "SMTP Relay" erweitert
// 20.09.2010  GD  File-Liste kann auch "nil" sein
// 07.01.2016  WN  Umstellung auf Indy 10, mit TLS-Unterstützung
// 04.02.2016  WN  Versand über unterschiedliche Mail-Einstellungen
// 26.04.2018  WW  TWMail-Objekt mit definierbarem Logdateinamen (statt FEHLER.TXT)
// 21.04.2023  WW  Exception-Behandlung in SendOutlookMail; Disconnect in
//                 TWMail.Destroy entfernt
//
// Copyright (C) RMG Messtechnik GmbH 2008, 2023
//------------------------------------------------------------------------------
unit wmail;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, ComObj, Variants,
  IdComponent, IdTCPConnection, IdTCPClient, IdMessageClient, IdSMTP, IdGlobal,
  IdBaseComponent, IdMessage, IdDNSResolver,
  GD_Utils, IdAttachmentFile, IdSSLOpenSSL, IdExplicitTLSClientServerBase;

type
//  TMySendMailMode = (smmNone, smmSmtpHost, smmSmtpRelay, smmOutlook);
//  -> Auskommentiert, da offenbar nie verwendet. Zukünftig sinnnvoller in
//     Ini_Mail.pas deklarieren; 11.05.2023, WW

  TWMail = class(TObject)
    constructor Create(const sHost, sUserId, sPassword: string; iPort: integer;
      sLogFilename: string = '');
    destructor Destroy; override;
  private
    FIdSMTP   : TIdSMTP;
    FHostList : TStringList;
    FSSLHandler: TIdSSLIOHandlerSocketOpenSSL;
    FLogFilename : string;  // 26.04.2018, WW
    procedure SetAuthentication(pAuthMode: TIdSMTPAuthenticationType);
    procedure SetHostName(const sHostName: string);
    function GetHostName: string;
    function SendMessage(pMsg: TIdMessage; iVersion: Integer): Integer;  // 04.02.2016  WN
  public
    function ResolveHostNames(const sIpAdress, sDomain: string): integer;
    function SendMail(const sVon, sVonAdr, sBetreff, sAn, sCC, sBCC: string;
      pTextList, pFileList: TStrings): Integer;
    property HostList : TStringList read FHostList;
    property AuthenticationMode: TIdSMTPAuthenticationType write SetAuthentication;
    property HostName: string read GetHostName write SetHostName;
  end;

function SendSmtpRelayMail(const sDnsServerIp, sVon, sVonAdr, sBetreff, sAn,
  sCC, sBCC: string; pTextList, pFileList: TStrings; var sHost: string): Smallint;
function SendOutlookMail(sSubject, sText: string;
  pSlTo, pSlCc, pSlBcc, pSlAttach: TStrings): Smallint;

implementation

var
  vOutlook : Variant;

//--------------------- Versenden per MS Exchange Server  ----------------------

// Parameter sDnsServerIP: DNS-Adresse, Hostname oder IP-Adresse
//----------------------------------------------
function SendSmtpRelayMail(const sDnsServerIp, sVon, sVonAdr, sBetreff, sAn,
  sCC, sBCC: string; pTextList, pFileList: TStrings; var sHost: string): Smallint;
//----------------------------------------------
var
  sDomain, sDebug : string;
  i       : integer;
begin
  try
    with TWMail.Create('', '', '', 25) do
    try
      Result := 0;
      // Hostnamen herausfinden
      sDebug := 'Find out hostname';
      sDomain := Trim(GetStringPart(sVonAdr, 2, '@'));
      if (sHost <> '') then begin   // Hostname wurde übergeben // 10.02.2010
        AuthenticationMode := atNone;
        HostName := sHost;
        sDebug := 'A) Hostname: ' + HostName + '; Try to send mail';
        Result := SendMail(
          sVon, sVonAdr, sBetreff, sAn, sCC, sBCC, pTextList, pFileList);
      end;

      if (Result <= 0) then begin
        sDebug := 'Resolve hostname';
        if (ResolveHostNames(sDnsServerIp, sDomain) > 0) then begin
          AuthenticationMode := atNone;
          for i := 0 to HostList.Count-1 do begin
            HostName := HostList[i];
            sDebug := 'B) Hostname: ' + HostName + ' (' + IntToStr(i) + '); Try to send mail';
            Result := SendMail(
              sVon, sVonAdr, sBetreff, sAn, sCC, sBCC, pTextList, pFileList);
            if (Result > 0) then begin
              sHost := HostName;  // Hostname zurückgeben // 10.02.2010
              Break;
            end;
          end;
        end;
      end;
    finally
      Free;
    end;
  except
    on E:Exception do begin
      Result := -1;
      WriteErrorLog('SendSmtpRelayMail (' + sDebug + '): ' + E.Message);
    end;
  end;
end;


//---------------------------- Versenden per Outlook ---------------------------

{ Versenden einer E-Mail über das lokale Outlook                         }
{ Parameter: Betreff, Text, Liste der Empfänger, Liste der CC's          }
{            Liste der BCC's, Liste der anzuhängenden Dateien            }
{ Rückgabe: Error-Code (0 = OK, -1 = Kein Adressat, -2 = Kein Outlook,   }
{                      -3 = Sonstiger Fehler)                            }
{------------------------------------------------------------------------}
function SendOutlookMail(sSubject, sText: string;
  pSlTo, pSlCc, pSlBcc, pSlAttach: TStrings): Smallint;
{------------------------------------------------------------------------}
const
  C_Err = 'SendOutlookMail: ';
  C_Ole_ApplName = 'Outlook.Application';
  
var
  vMailItem, vRecipient : Variant;
  i                     : integer;
begin
  if (Assigned(pSlTo)) and (pSlTo.Count > 0) then
  try
    // Ggf. OLE-Objekt initialisieren
    try
      if (VarType(vOutLook) = varEmpty) then
        vOutlook := CreateOleObject(C_Ole_ApplName);
      vMailItem := vOutlook.CreateItem(0);
    except
      on E:Exception do begin
        Result := -2;  // Fehler: Kein Outlook
        WriteErrorLog(C_Err + E.Message + ' (' + C_Ole_ApplName + ')');
        exit;
      end;
    end;

    // Betreff und Mailtext
    vMailItem.Subject := sSubject;
    vMailItem.Body := sText;

    // Adressaten als Liste
    if (Assigned(pSlTo)) then
      for i := 0 to pSlTo.Count-1 do
        vRecipient := vMailItem.Recipients.Add(pSlTo[i]);

    // CC's als Liste
    if (Assigned(pSlCc)) then
      for i := 0 to pSlCc.Count-1 do begin
        vRecipient := vMailItem.Recipients.Add(pSlCc[i]);
        vRecipient.Type := 2;  // CC
      end;

    // BCC's als Liste
    if (Assigned(pSlBcc)) then
      for i := 0 to pSlBcc.Count-1 do begin
        vRecipient := vMailItem.Recipients.Add(pSlBcc[i]);
        vRecipient.Type := 3;  // BCC
      end;

    // Anzuhängende Dateien als Liste
    if (Assigned(pSlAttach)) then
      for i := 0 to pSlAttach.Count-1 do begin
        if (FileExists(pSlAttach[i])) then
          vMailItem.Attachments.Add(pSlAttach[i]);
      end;
                                                                         
//    vMailItem.Display;   // Mail anzeigen
    vMailItem.Send;      // Mail senden
    Result := 0;  // OK
  except
    on E:Exception do begin
      Result := -3;  // Sonstiger Fehler
      WriteErrorLog(C_Err + E.Message);
    end;
  end
  else Result := -1;  // Fehler: kein Adressat
end;


//---------------------------------- TWMail ------------------------------------

{ Host:   hostname z.B. smtp.puretec.de oder ip Adr.}
{ Port:   Standard 25 }
{ USerId: User im Mailserver }
{------------------------------------------------------------------------}
constructor TWMail.Create(const sHost, sUserId, sPassword: string;
  iPort: integer; sLogFilename: string = '');
{------------------------------------------------------------------------}
begin
  FLogFilename := sLogFilename;  // 26.04.2018, WW

  // 07.01.2016  WN
  FSSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create;
  FSSLHandler.MaxLineAction := maException;
  FSSLHandler.SSLOptions.Method := sslvTLSv1;
{
  FSSLHandler.SSLOptions.Mode := sslmUnassigned;
  FSSLHandler.SSLOptions.VerifyMode := [];
  FSSLHandler.SSLOptions.VerifyDepth := 0;
}

  // Indy-SMTPObject erzeugen
  FIdSMTP := TIdSMTP.Create;
  FIdSMTP.Host := sHost;
  FIdSMTP.Port := iPort;
  FIdSMTP.Username := sUserId;
  FIdSMTP.Password := sPassword;
  FIdSMTP.IOHandler := FSSLHandler;
  FIdSMTP.AuthType := atDefault;
  FIdSMTP.UseTLS := utUseRequireTLS;
  FIdSMTP.ReadTimeout := 0;

  FHostList := TStringList.Create; // Liste der Hostnamen
end;

{------------------------------------------------------------------------}
destructor TWMail.Destroy;
{------------------------------------------------------------------------}
begin
// Auskommentiert 21.04.2023, WW: Disconnect liefert hier ggf. eine Exception.
// Der Aufruf ist hier auch gar nicht erforderlich, da Disconnect bereits in
// einem try..finally-Block beim Connect-Aufruf gemacht wird. ProcessMessages
// braucht's dann auch nicht (wofür war es überhaupt ?)
//  if (FIdSMTP.Connected) then FIdSMTP.Disconnect;
//  Application.ProcessMessages;

  FreeAndNil(FIdSMTP);
  FreeAndNil(FHostList);
  FreeAndNil(FSSLHandler);
end;

// Setzen des AuthenticationTypes
// Parameter: AuthenticationTypes
//----------------------------------------------
procedure TWMail.SetAuthentication(pAuthMode: TIdSMTPAuthenticationType);
//----------------------------------------------
begin
  FIdSMTP.AuthType := pAuthMode;
end;

// Setzen des Hostnames
// Parameter: Hostname
//----------------------------------------------
procedure TWMail.SetHostName(const sHostName: string);
//----------------------------------------------
begin
  FIdSMTP.Host := sHostName;
end;

// Setzen des Hostnames
// Parameter: Hostname
//----------------------------------------------
function TWMail.GetHostName: string;
//----------------------------------------------
begin
  Result := FIdSMTP.Host;
end;

// Hostnames über IdResolver ermitteln
// Parameter: IP-Adresse des DNS Servers, Domäne
// Rückgabe: Anzahl der ermittelten Hostnames (-1 = Fehler)
//----------------------------------------------
function TWMail.ResolveHostNames(const sIpAdress, sDomain: string): integer;
//----------------------------------------------
var
  i, iPos : integer;
  s       : string;
  pRec    : TMXRecord;
begin
  try
    // Liste aller Hostnamen
    FHostList.Clear;

    with TIdDNSResolver.Create(nil) do
    try
      QueryResult.Clear;
//      QueryRecords := [qtMX];
      Host := sIpAdress;
//      ReceiveTimeout := 5000;
      Resolve(sDomain);

      if (QueryResult.Count > 0) then begin
        for i := 0 to QueryResult.Count - 1 do begin
          pRec := TMXRecord(QueryResult.Items[i]);
          s := IntToStr(pRec.Preference);
          if (Length(s) < 2) then s := '0' + s;
          FHostList.Append(s + '=' + pRec.ExchangeServer);
        end;

        // sort in order of priority and then remove extra data
        FHostList.Sorted := False;
        for i := 0 to FHostList.Count-1 do begin
          iPos := Pos('=', FHostList[i]);
          if (iPos > 0) then
            FHostList[i] := Copy(FHostList[i], iPos+1, Length(FHostList[i]));
         end;
        FHostList.Sorted := True;
        // Ignore duplicate servers
        FHostList.Duplicates := dupIgnore;
      end;
      Result := FHostList.Count;
    finally
      Free;
    end;
  except
    Result := -1;
  end;
end;

{ VonName     : Name des Absenders beliebig }
{ VonAdr      : gültige AbsenderAdresse z.B. Manfred.Schwarzmueller@Karlwieser.de}
{ Betreff     : Betreffzeile der Mail (Subject)}
{ An          : gültige Empfängeradresse (siehe VOnAdr)}
{ CC          : gültige Mailadr. Kopie an }
{ BCC         : gültige Mailadr. blinde Kopie an }
{------------------------------------------------------------------------}
function TWMail.SendMail(const sVon, sVonAdr, sBetreff, sAn, sCC, sBCC: string;
  pTextList, pFileList: TStrings): Integer;
{------------------------------------------------------------------------}
var
  i, iVersion : integer;
  pMsg   : TIdMessage;
  sDebug : string;
begin
  try
    sDebug := 'Start "SendMail" - create "TIdMessage"';
    pMsg := TIdMessage.Create;
    try
//      pMsg.AttachmentEncoding := 'MIME';
//      pMsg.ContentType := 'text/plain';
      pMsg.Encoding := meMIME;
      pMsg.Body.Assign(pTextList);
      pMsg.From.Text := '"' + sVon + '"';
      pMsg.From.Address := sVonAdr;
      pMsg.ReplyTo.EMailAddresses := sVon;
      pMsg.Recipients.EMailAddresses := sAn; { To: header }
      pMsg.Subject := sBetreff; { Subject: header }
      pMsg.Priority := TIdMessagePriority(2); { Priority: Normal (0..4) }
      pMsg.CCList.EMailAddresses := sCC; {CC}
      pMsg.BccList.EMailAddresses := sBCC; {BBC}
      pMsg.ReceiptRecipient.Text := ''; {indicate that there is no receipt recipiant}
      // Dateianhänge
      if (Assigned(pFileList)) then  // 20.09.2010
        for i := 0 to pFileList.Count-1 do
          TIdAttachmentFile.Create(pMsg.MessageParts, pFileList[i]);

      // 04.02.2016  WN - ohne Authentifizierung
      if (FIdSMTP.Username = '') then FIdSMTP.AuthType := atNone;

      {now we send the message}
      for iVersion:=1 to 5 do
      begin
        Result := SendMessage(pMsg, iVersion);  // 04.02.2016  WN
        if (Result <> -1) then Break;
      end;
    finally
      FreeAndNil(pMsg);
    end;
  except
    on E:Exception do begin
      Result := -1;
      WriteErrorLog('TWMail.SendMail (' + sDebug + '): ' + E.Message, FLogFilename);
    end;
  end;
end;

// 04.02.2016  WN
// -----------------------------------------------------------------------------
// Parameter: pMsg = die zu versendende Nachricht
//            iVersion = Konfigurationsversion
// Ergebnis: Error-Result = -1 , OK-Result = 1
// -----------------------------------------------------------------------------
function TWMail.SendMessage(pMsg: TIdMessage; iVersion: Integer): Integer;
// -----------------------------------------------------------------------------
var
  sVersion, sDebug : string;
  bConnected: Boolean;
begin
  Result := 1;  // OK ...

  if (iVersion = 1) then
  begin
    sVersion := ' Version: SSLOption = sslvTLSv1, UseTLS = utUseRequireTLS';
    FSSLHandler.SSLOptions.Method := sslvTLSv1;
    FIdSMTP.UseTLS := utUseRequireTLS;
  end else if (iVersion = 2) then
  begin
    sVersion := ' Version: SSLOption = sslvTLSv1, UseTLS = utNoTLSSupport';
    FSSLHandler.SSLOptions.Method := sslvTLSv1;
    FIdSMTP.UseTLS := utNoTLSSupport;
  end else if (iVersion = 3) then
  begin
    sVersion := ' Version: SSLOption = sslvSSLv2, UseTLS = utNoTLSSupport';
    FSSLHandler.SSLOptions.Method := sslvSSLv2;
    FIdSMTP.UseTLS := utNoTLSSupport;
  end else if (iVersion = 4) then
  begin
    sVersion := ' Version: SSLOption = sslvSSLv23, UseTLS = utNoTLSSupport';
    FSSLHandler.SSLOptions.Method := sslvSSLv23;
    FIdSMTP.UseTLS := utNoTLSSupport;
  end else
  begin
    sVersion := ' Version: SSLOption = sslvSSLv3, UseTLS = utNoTLSSupport';
    FSSLHandler.SSLOptions.Method := sslvSSLv3;
    FIdSMTP.UseTLS := utNoTLSSupport;
  end;

  bConnected := false;
  sDebug := 'Try to connect' + sVersion;
  try
    FIdSMTP.Connect;
    bConnected := true;
  except
    on E:Exception do begin
      Result := -1;  // Error
      WriteErrorLog('TWMail.SendMessage (' + sDebug + '): ' + E.Message, FLogFilename);
    end;
  end;

  // wenn connected, dann versenden
  if (bConnected) then
  try
    try
      if (FIdSMTP.AuthType <> atNone) then
      begin
        sDebug := 'Try to authenticate' + sVersion;
        FIdSMTP.Authenticate;
        sDebug := 'Authentication is done' + sVersion;
      end;
      sDebug := 'Try to send:' + sVersion;
      FIdSMTP.Send(pMsg);
      sDebug := 'Send is done' + sVersion;
    except
      on E:Exception do begin
        Result := -1;  // Error
        WriteErrorLog('TWMail.SendMessage (' + sDebug + '): ' + E.Message, FLogFilename);
      end;
    end;
  finally
    if (FIdSMTP.Connected) then  // 21.04.2023, WW
      FIdSMTP.Disconnect;
  end;
end;

end.
