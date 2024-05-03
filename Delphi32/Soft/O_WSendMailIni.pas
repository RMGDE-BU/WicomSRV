//------------------------------------------------------------------------------
// Zugriff auf INI-Datei des WSendMail-Moduls
//
// 21.06.2023  WW  Neu
//
// Copyright (C) RMG Messtechnik GmbH 2023
//------------------------------------------------------------------------------
unit O_WSendMailIni;

interface

uses
  Classes, SysUtils, IniFiles;

type
  TMailModes = (mm_SMTP,  mm_Outlook);

  TMailSecurity = (ms_None, ms_Auto, ms_SslOnConnect,
                   ms_StartTls, ms_StartTlsWhenAvailable);

  TMailAuths = (ma_None, ma_Password{!!, ma_OAuth2});


  TWSendMailIni = class (TIniFile)
  private
    function GetMode: integer;
    procedure SetMode (Value: integer);
    function GetServer: string;
    procedure SetServer (Value: string);
    function GetPort: integer;
    procedure SetPort (Value: integer);
    function GetSecurity: integer;
    procedure SetSecurity (Value: integer);
    function GetServerCertificateValidation: boolean;
    procedure SetServerCertificateValidation (Value: boolean);
    function GetAuthentication: integer;
    procedure SetAuthentication (Value: integer);
    function GetUser: string;
    procedure SetUser (Value: string);
    function GetPassword: string;
    procedure SetPassword (Value: string);
    function GetPasswordEncrypted: string;
  public
    constructor Create(Path: string);
    property Mode: integer read GetMode write SetMode;
    property Server: string read GetServer write SetServer;
    property Port: integer read GetPort write SetPort;
    property Security: integer read GetSecurity write SetSecurity;
    property ServerCertificateValidation: boolean read GetServerCertificateValidation
      write SetServerCertificateValidation;
    property Authentication: integer read GetAuthentication write SetAuthentication;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
    property PasswordEncrypted: string read GetPasswordEncrypted;
  end;

implementation

const
  C_IniFileName = 'WSendMail.ini';

  C_Section_Mail = 'Mail';

  C_Ident_Mode = 'Mode';
  C_Ident_Server = 'Server';
  C_Ident_Port = 'Port';
  C_Ident_Security = 'Security';
  C_Ident_ServerCertificateValidation = 'ServerCertificateValidation';
  C_Ident_Authentication = 'Authentication';
  C_Ident_User = 'User';
  C_Ident_Password = 'Password';
  C_Ident_PasswordEncrypted = 'PasswordEncrypted';


//----------------------------------
constructor TWSendMailIni.Create(Path: string);
//----------------------------------
begin
  inherited Create (Path + C_IniFileName);
end;

//----------------------------------
function TWSendMailIni.GetMode: integer;
//----------------------------------
begin
  Result := ReadInteger(C_Section_Mail, C_Ident_Mode, integer (mm_SMTP));
end;

//----------------------------------
procedure TWSendMailIni.SetMode (Value: integer);
//----------------------------------
begin
  WriteInteger(C_Section_Mail, C_Ident_Mode, Value);
end;

//----------------------------------
function TWSendMailIni.GetServer: string;
//----------------------------------
begin
  Result := ReadString(C_Section_Mail, C_Ident_Server, '');
end;

//----------------------------------
procedure TWSendMailIni.SetServer (Value: string);
//----------------------------------
begin
  WriteString(C_Section_Mail, C_Ident_Server, Value);
end;

//----------------------------------
function TWSendMailIni.GetPort: integer;
//----------------------------------
begin
  Result := ReadInteger(C_Section_Mail, C_Ident_Port, 587);
end;

//----------------------------------
procedure TWSendMailIni.SetPort (Value: integer);
//----------------------------------
begin
  WriteInteger(C_Section_Mail, C_Ident_Port, Value);
end;

//----------------------------------
function TWSendMailIni.GetSecurity: integer;
//----------------------------------
begin
  Result := ReadInteger(C_Section_Mail, C_Ident_Security, integer (ms_None));
end;

//----------------------------------
procedure TWSendMailIni.SetSecurity (Value: integer);
//----------------------------------
begin
  WriteInteger(C_Section_Mail, C_Ident_Security, Value);
end;

//----------------------------------
function TWSendMailIni.GetServerCertificateValidation: boolean;
//----------------------------------
begin
  Result := ReadBool(C_Section_Mail, C_Ident_ServerCertificateValidation, true);
end;

//----------------------------------
procedure TWSendMailIni.SetServerCertificateValidation (Value: boolean);
//----------------------------------
begin
  WriteBool(C_Section_Mail, C_Ident_ServerCertificateValidation, Value);
end;

//----------------------------------
function TWSendMailIni.GetAuthentication: integer;
//----------------------------------
begin
  Result := ReadInteger(C_Section_Mail, C_Ident_Authentication, integer (ma_None));
end;

//----------------------------------
procedure TWSendMailIni.SetAuthentication (Value: integer);
//----------------------------------
begin
  WriteInteger(C_Section_Mail, C_Ident_Authentication, Value);
end;

//----------------------------------
function TWSendMailIni.GetUser: string;
//----------------------------------
begin
  Result := ReadString(C_Section_Mail, C_Ident_User, '');
end;

//----------------------------------
procedure TWSendMailIni.SetUser (Value: string);
//----------------------------------
begin
  WriteString(C_Section_Mail, C_Ident_User, Value);
end;

//----------------------------------
function TWSendMailIni.GetPassword: string;
//----------------------------------
begin
  Result := ReadString(C_Section_Mail, C_Ident_Password, '');
end;

//----------------------------------
procedure TWSendMailIni.SetPassword (Value: string);
//----------------------------------
begin
  WriteString(C_Section_Mail, C_Ident_Password, Value);
end;

//----------------------------------
function TWSendMailIni.GetPasswordEncrypted: string;
//----------------------------------
begin
  Result := ReadString(C_Section_Mail, C_Ident_PasswordEncrypted, '');
end;

end.
