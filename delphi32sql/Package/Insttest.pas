{------------------------------------------------------------------------------}
{ 08.03.1999 GD; Unit zum Überprüfen evtl. schon vorhandener Instanzen         }
{  -> VORSICHT: Es wird nur überprüft, ob der Classname und die Caption des    }
{               Hauptfensters in den Klassen ENTHALTEN sind                    }
{  -> 1. Instanz in den Vordergrund holen geht nur bei Nicht-Minimiert         }
{                                                                              }
{ 06.05.1999 GD; Änderungen                                                    }
{                                                                              }
{ Anwendung: Nach letzten 'CreateForm' einfügen (im Beispiel mit Lizenztest)   }
{  if not TerminateSecondInstance(Application.MainForm.ClassName,              }
{                                 Application.MainForm.Caption,                }
{                                 True)                                        }
{  then begin                                                                  }
{    if not WLogin32 then Application.Terminate else begin                     }
{      try                                                                     }
{        Application.Run;                                                      }
{      finally                                                                 }
{        WLogout32;                                                            }
{      end;                                                                    }
{    end;                                                                      }
{  end;                                                                        }
{                                                                              }
{ Alternativ vor erstem 'Create' (Hauptformular im Quelltext deklariert)       }
{ (Ist schneller)                                                              }
{                                                                              }
{  var                                                                         }
{    FormAkaMain: TFormAkaMain;                                                }
{                                                                              }
{  begin                                                                       }
{    Application.Initialize;                                                   }
{    if (ShowInstance(TFormAkaMain.Classname, ''))                             }
{    then Application.Terminate                                                }
{    else begin                                                                }
{      Application.CreateForm(TFormAkaMain, FormAkaMain);                      }
{      Application.Run;                                                        }
{    end;                                                                      }
{  end.                                                                        }
{                                                                              }
{ 20.03.2001 WW; 1. Instanz in den Vordergrund holen geht jetzt immer          }
{ 03.02.2002 GD; ShowInstance                                                  }
{ 13.10.2003 WW; AnzahlInstanzen: Erkennen von MainformCaptions mit MDIForm-   }
{                Anhängen verbessert                                           }
{ 03.06.2005 GD; AnzahlInstanzen: Opional mit Teil-Capton                      }
{ 30.11.2007 WW; GetWindowText/GetClassName-Buffer vergrößert                  }
{ 07.08.2018 WW; AnzahlInstanzen: Verbesserung Klassennamen-Vergleich auf      }
{                gleiche Länge                                                 }
{------------------------------------------------------------------------------}
unit InstTest;

interface

uses
  Windows, SysUtils, Forms, WStrUtils;

function AnzahlInstanzen (aClassName, aCaption: string;
  bCheckCaptionPart: boolean = False): integer;
function ShowInstance (aClassName, aCaption: string): boolean;
function TerminateSecondInstance (aClassName, aCaption: string;
                                  ShowFirst: boolean): boolean;

implementation

{ Beendet 2. Instanz mit übergebener Klasse, Caption und bringt die       }
{ bereits laufende Instanz in den Vordergrund                             }
{ Parameter: Fenster-ClassName, -Caption; ShowFirst-1.Instanz aktivieren  }
{-------------------------------------------------------------}
function TerminateSecondInstance (aClassName, aCaption: string;
                                  ShowFirst: boolean): boolean;
{-------------------------------------------------------------}
var
  FWnd: HWnd;
  H: HWnd;
  Buf: array [Byte] of Char;

begin
  if AnzahlInstanzen (aClassName, aCaption) > 1 then begin
    result:= True;
    { Handle der bereits laufenden Instanz finden: }
    H:= 0;

    { Fängt bei dem ersten Fenster mit 'Application.Title' an zu suchen }
    FWnd:= FindWindow(nil, pchar (Application.Title));
    while FWnd <> 0 do begin
      GetWindowText(FWnd, Buf, sizeof(Buf));
      { Caption vergleichen }
      if StrPos(Buf, pchar (Application.Title)) <> nil then begin
        { Klassenname vergleichen }
        GetClassName(FWnd, Buf, sizeof(Buf));
        if StrPos(Buf, 'TApplication') <> nil then begin
          { Application-Handle für Maximieren speichern }
          if FWnd <> Application.Handle then begin
            H:=FWnd;
            Break;
          end;
        end;
      end;

      { Handle des nächsten Fensters }
      FWnd:= GetNextWindow(FWnd, GW_HWNDNEXT);
    end;

    Application.Terminate;                               { 2. Instanz beenden }

    { laufende Instanz nach vorne bringen (funktioniert nur vernünftig mit dem
      Application.Handle !) }
    if (H > 0) AND ShowFirst then begin
      ShowWindow (H, sw_ShowDefault);
      SetForegroundWindow (H);
    end;
  end else
    result:= False;
end;

{ Bringt ein bereits laufende Instanz in den Vordergrund      }
{ Parameter: Fenster-ClassName, -Caption                      }
{ Rückgabe: Instanz vorhanden: ja/nein                        }
{-------------------------------------------------------------}
function ShowInstance (aClassName, aCaption: string): boolean;
{-------------------------------------------------------------}
var
  FWnd: HWnd;
  H: HWnd;
  Buf: array [Byte] of Char;

begin
  if (AnzahlInstanzen (aClassName, aCaption) > 0) then begin
    result:= True;
    { Handle der bereits laufenden Instanz finden: }
    H:= 0;

    { Fängt bei dem ersten Fenster mit 'Application.Title' an zu suchen }
    FWnd:= FindWindow(nil, pchar (Application.Title));
    while FWnd <> 0 do begin
      GetWindowText(FWnd, Buf, sizeof(Buf));
      { Caption vergleichen }
      if StrPos(Buf, pchar (Application.Title)) <> nil then begin
        { Klassenname vergleichen }
        GetClassName(FWnd, Buf, sizeof(Buf));
        if StrPos(Buf, 'TApplication') <> nil then begin
          { Application-Handle für Maximieren speichern }
          if FWnd <> Application.Handle then begin
            H:=FWnd;
            Break;
          end;
        end;
      end;

      { Handle des nächsten Fensters }
      FWnd:= GetNextWindow(FWnd, GW_HWNDNEXT);
    end;

    { laufende Instanz nach vorne bringen (funktioniert nur vernünftig mit dem
      Application.Handle !) }
    if (H > 0) then begin
      ShowWindow (H, sw_ShowDefault);
      SetForegroundWindow (H);
    end;
  end
  else Result := False;
end;

{ Gibt die Anzahl der laufenden Instanzen mit übergebener Klasse und Caption }
{ zurück.                                                                    }
{ Parameter: aClassName, aCaption                                            }
{ Ergebnis: Anzahl der Instanzen                                             }
{---------------------------------------------------------------}
function AnzahlInstanzen (aClassName, aCaption: string;
  bCheckCaptionPart: boolean = False): integer;
{---------------------------------------------------------------}

  {-----------------------------------------------------------------}
  function ExtractMainFormCaption (AMainFormCaption: string): string;    // 13.10.2003, WW
  {-----------------------------------------------------------------}
  { MDI-Clientfenster als Vollbild ändern die Mainform-Caption in:
    <MainFormCaption> - [MDIForm.Caption]
    Ergebnis: reine MainFormCaption ohne MDIForm-Anhang }
  var
    SSave, SBuf: string;
  begin
    SSave:=AMainFormCaption;
    SBuf:=F_Zerlegen (SSave, '[');
    if Copy (SBuf, length (SBuf)-2, 3) = ' - ' then
      Result:=Copy (SBuf, 1, length (SBuf)-3)
    else
      Result:=AMainFormCaption;
  end;

var
  FWnd    : HWnd;
  s1      : string;
  sc, spc : string;
  Buf: array [Byte] of Char;

begin
  result:= 0; { default }
  s1:= aClassName;
  sc:=ExtractMainformCaption (aCaption);  { reine MainFormCaption der übergebenen Caption }

  { Fängt bei dem ersten Fenster mit 'aClassName' an zu suchen }
  FWnd := FindWindow(PChar(s1), nil);
  while (FWnd <> 0) do begin
    { Klassenname vergleichen }
    GetClassName(FWnd, Buf, sizeof(Buf));
    if (StrPos(Buf, PChar(s1)) <> nil) then begin
      { Verbesserung: Klassennamen-Vergleich auf gleiche Länge (Teilstring möglich !); 07.08.2018, WW }
      if StrLen(Buf) = StrLen(PChar(s1)) then begin
        { Caption vergleichen, wenn für ACaption kein Leeerstring übergeben wurde }
        if (Length (aCaption) > 0) then begin
          GetWindowText(FWnd, Buf, sizeof(Buf));
          if (bCheckCaptionPart) then begin  // 03.06.2005
            if (Pos(aCaption, string(Buf)) > 0) then Result := Result + 1;
          end
          else begin
            spc:=ExtractMainformCaption (string (Buf)); { reine MainFormCaption der gefundenen Caption }
            if (spc = sc) then Result := Result + 1;
          end;
        end
        else Result:= Result + 1;
      end;
    end;
    { Handle des nächsten Fensters }
    FWnd:= GetNextWindow(FWnd, GW_HWNDNEXT);
  end;
end;

end.
