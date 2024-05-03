{------------------------------------------------------------------------------}
{ Kapselung der ID-Mail-Komponenten (INDY) von Don Siders                      }
{                                                                              }
{ 21.12.2005  GD  Bereinigtes und an die Grundlagen sinnvollen Programmier-    }
{                 stils angepasstes Object aus Prototyp von SM erstellt        }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2005                                          }
{------------------------------------------------------------------------------}
unit WIdMailObject;

interface

uses
  SysUtils, Classes,
  IdSMTP, IdMessage;

type
  TWIdMailObject = class(TObject)
    constructor Create(sHost, sFrom, sRec, sCc, sBcc: string); virtual;
    destructor Destroy; override;
  private
    FIdSMTP    : TIdSMTP;
    FIdMessage : TIdMessage;
    FHost      : string;
  protected
  public
    function SendMail(sSubject: string; sBody: TStrings): string;
  end;

function WSendMail(
  sHost, sFrom, sRec, sCc, sBcc, sSubject, sBody: string): boolean;

implementation

resourcestring
  S_WIdMailObject_Result_NoError = 'OK';

{------------------------- Allgemeine Funktionen ------------------------------}

{ Versenden eine Mail über INDY-Komponenten      }
{ Parameter: Name des Mail-Servers, 'Von', 'An'  }
{   (Kommaliste), 'CC' (Kommaliste), 'BCC'       }
{   (Kommaliste), 'Betreff', Inhalt (Kommaliste) }
{ Rückgabe: Erfolg ja / nein                     }
{------------------------------------------------}
function WSendMail(
  sHost, sFrom, sRec, sCc, sBcc, sSubject, sBody: string): boolean;
{------------------------------------------------}
var
  pSlBody : TStrings;
begin
  try
    with TWIdMailObject.Create(sHost, sFrom, sRec, sCc, sBcc) do
    try
      pSlBody := TStringList.Create;
      try
        pSlBody.CommaText := sBody;

        Result := (SendMail(sSubject, pSlBody) = S_WIdMailObject_Result_NoError);
      finally
        pSlBody.Free;
      end;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

{----------------------------- TWIdMailObject ----------------------------------}

{ Konstruktor des Mail-Objekts                   }
{ Parameter: Name des Mail-Servers; 'Von'; 'An', }
{   'CC', 'BCC' als Kommaliste                   }
{------------------------------------------------}
constructor TWIdMailObject.Create(sHost, sFrom, sRec, sCc, sBcc: string);
{------------------------------------------------}
begin
  inherited Create;

  FIdSMTP := TIdSMTP.Create(nil);
  FIdMessage := TIdMessage.Create(nil);

  FHost := sHost;

  with FIdMessage do begin
    From.Text := sFrom;
    Recipients.EMailAddresses := sRec;
    CCList.EMailAddresses := sCC;
    BccList.EMailAddresses := sBcc;
  end;
end;

{ Destruktor des Mail-Objekts                   }
{------------------------------------------------}
destructor TWIdMailObject.Destroy;
{------------------------------------------------}
begin
  FreeAndNil(FIdMessage);
  FreeAndNil(FIdSMTP);

  inherited Destroy;
end;

{ Versenden einer Mail                           }
{ Parameter: 'Betreff', Inhalt                   }
{ Rückgabe: Ergebnis als Text: 'OK' = i.O.       }
{------------------------------------------------}
function TWIdMailObject.SendMail(sSubject: string; sBody: TStrings): string;
{------------------------------------------------}
begin
  if (sSubject <> '') or (sBody.Count > 0) then begin

    FIdMEssage.Subject := sSubject;
    if (sBody.Count > 0) then FIdMEssage.Body := sBody;

    with FIdSMTP do begin
      Host := Self.FHost;
      try
        Connect;
        Send(FIdMessage);
        Disconnect;
        Result := S_WIdMailObject_Result_NoError;
      except
        on E:Exception do begin
          if (Connected) then
          try
            Disconnect;
          except
            on E:Exception do Result := 'Not disconnected: ' + E.Message + '; ';
          end;
          Result := Result + 'Error: ' + E.Message;
        end;
      end;
    end;
  end
  else Result := 'No content';
end;

end.
