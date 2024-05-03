// -----------------------------------------------------------------------------
// Unit: allgemeine Ini-Datei für das Versenden von E-Mails
//       E-Mail-Einstellungen je Programm für den Versand der E-Mail
//
// 24.11.2008  NW  Vs. 1.0  Neu
//
// Copyright © RMG Messtechnik GmbH 2008
// -----------------------------------------------------------------------------
unit Ini_Mail;

interface

uses
  Classes, IniFiles, SysUtils;

type
  // Objekt für Mail.INI
  TMailIni = class (TIniFile)
  private
    FSection: String;   // Name des Programms ist Section
  protected
    function Get_DNS: String;     // DNS-Server-IP
    function Get_Host: String;    // Hostname
    function Get_Port: Integer;   // Port-Nummer
    function Get_User: String;    // User-Id
    function Get_PW: String;      // Password
    function Get_From: String;    // Absender
    function Get_To: String;      // Empfänger
    function Get_Cc: String;      // Kopie
    function Get_Bcc: String;     // Blindkopie
  public
    constructor Create(Path: TFilename);
    property Section: String read FSection write FSection;  // Section
    property DNSServerIP: String read Get_DNS; // DNS-Server-IP
    property Hostname: String read Get_Host; // Hostname
    property PortNo: Integer read Get_Port;  // Port-Nummer
    property UserID: String read Get_User;   // User-Id
    property Password: String read Get_PW;   // Password
    property AdrFrom: String read Get_From;  // Absender
    property AdrTo: String read Get_To;      // Empfänger
    property AdrCc: String read Get_Cc;      // Kopie
    property AdrBcc: String read Get_Bcc;    // Blindkopie
  end;

implementation

const
  CMailIni = 'MAIL.INI';

  C_Ident_DNS     = 'DNSSERVERIP';
  C_Ident_Host    = 'HOSTNAME';
  C_Ident_Port    = 'PORTNO';
  C_Ident_User    = 'USERID';
  C_Ident_PW      = 'PASSWORD';
  C_Ident_From    = 'ADRFROM';
  C_Ident_To      = 'ADRTO';
  C_Ident_Cc      = 'ADRCC';
  C_Ident_Bcc     = 'ADRBCC';

  C_Default_Port  = -1;


{******************************************************************************}
{*                                  TMailIni                                  *}
{******************************************************************************}

{-----------------------------------------------}
constructor TMailIni.Create(Path: TFilename);
{-----------------------------------------------}
begin
  inherited Create(Path + CMailIni);
end;

// -----------------------------------------------------------------------------
function TMailIni.Get_DNS: String;  // DNS-Server-IP
// -----------------------------------------------------------------------------
begin
  Result:=ReadString(FSection, C_Ident_DNS, '');
end;

// -----------------------------------------------------------------------------
function TMailIni.Get_Host: String;  // Hostname
// -----------------------------------------------------------------------------
begin
  Result:=ReadString(FSection, C_Ident_Host, '');
end;

// -----------------------------------------------------------------------------
function TMailIni.Get_Port: Integer;  // Port-Nummer
// -----------------------------------------------------------------------------
begin
  Result:=ReadInteger(FSection, C_Ident_Port, C_Default_Port);
end;

// -----------------------------------------------------------------------------
function TMailIni.Get_User: String;  // User-Id
// -----------------------------------------------------------------------------
begin
  Result:=ReadString(FSection, C_Ident_User, '');
end;

// -----------------------------------------------------------------------------
function TMailIni.Get_PW: String;  // Password
// -----------------------------------------------------------------------------
begin
  Result:=ReadString(FSection, C_Ident_PW, '');
end;

// -----------------------------------------------------------------------------
function TMailIni.Get_From: String;  // Absender
// -----------------------------------------------------------------------------
begin
  Result:=ReadString(FSection, C_Ident_From, '');
end;

// -----------------------------------------------------------------------------
function TMailIni.Get_To: String;  // Empfänger
// -----------------------------------------------------------------------------
begin
  Result:=ReadString(FSection, C_Ident_To, '');
end;

// -----------------------------------------------------------------------------
function TMailIni.Get_Cc: String;  // Kopie
// -----------------------------------------------------------------------------
begin
  Result:=ReadString(FSection, C_Ident_Cc, '');
end;

// -----------------------------------------------------------------------------
function TMailIni.Get_Bcc: String;  // Blindkopie
// -----------------------------------------------------------------------------
begin
  Result:=ReadString(FSection, C_Ident_Bcc, '');
end;

end.
