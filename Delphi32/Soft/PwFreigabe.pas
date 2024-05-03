{------------------------------------------------------------------------------}
{ Steuerung der Freigabe von Datenelementen über Passwortlevel                 }
{                                                                              }
{ 20.06.2003  GD  Neu                                                          }
{ 21.07.2005  GD  Erweitert um "ComponentEnabled"                              }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2003, 2005                                    }
{------------------------------------------------------------------------------}
unit PwFreigabe;

interface

uses
  SysUtils, Classes, Controls, Forms, ActnList, Menus,
  GD_Utils;

type
  TCtrlFreigabe = class(TMyTablist)
  private
  protected
  public
    procedure InsertComponent(
      pForm: TForm; pComponent: TComponent; iPwLevel: byte);
    function ComponentEnabled(
      pForm: TForm; pComponent: TComponent; iPwLevel: byte): boolean;
    procedure DeleteForm(pForm: TForm);
    procedure SetPasswordLevel(iPwLevel: byte);
  end;

var
  CtrlFreigabe : TCtrlFreigabe;

implementation

{ Fügt ein Passwort-gesteuertes Control in Liste ein   }
{ Parameter: Übergeordnetes Formular, Ctrl, PW-Level   }
{------------------------------------------------------}
procedure TCtrlFreigabe.InsertComponent(
  pForm: TForm; pComponent: TComponent; iPwLevel: byte);
{------------------------------------------------------}
var
  iIndex : integer;
begin
  iIndex := IndexOf(IntToStr(Integer(pForm)));  // Formular-Zeiger als string
  if (iIndex < 0) then
    iIndex := Self.AddObject(IntToStr(Integer(pForm)), TStringList.Create);
  // Passwortlevel als String, Control als Object
  Col[iIndex].AddObject(IntToStr(iPwLevel), pComponent);
end;

{ Entfernt alle Controls zu einem Formular aus Liste   }
{ Parameter: Formular, dessen Ctrls freizugeben sind   }
{------------------------------------------------------}
procedure TCtrlFreigabe.DeleteForm(pForm: TForm);
{------------------------------------------------------}
var
  iIndex : integer;
begin
  iIndex := IndexOf(IntToStr(Integer(pForm)));
  if (iIndex >= 0) then Delete(iIndex);
end;

{ En-/Disablen aller gespeicherten Controls gem. Level }
{ Parameter: Aktueller Passwort-Level                  }
{------------------------------------------------------}
procedure TCtrlFreigabe.SetPasswordLevel(iPwLevel: byte);
{------------------------------------------------------}
var
  i, j, iForm : integer;
begin
  for i := Count-1 downto 0 do begin  // Schleife über alle Formulare
    iForm := StrToInt(Strings[i]);
    with Col[i] do
      for j := 0 to Count-1 do  // Alle Ctrls des betreffenden Formulars
        if (Objects[j] is TForm) then begin
          if (Integer(Objects[j]) = iForm) and
             (StrToInt(Strings[j]) > iPwLevel) then
          begin
            TForm(Objects[j]).Free;
            Break;
          end
          else begin
            TControl(Objects[j]).Enabled := (StrToInt(Strings[j]) <= iPwLevel);
          end;
        end
        else if (Objects[j] is TControl) then
          TControl(Objects[j]).Enabled := (StrToInt(Strings[j]) <= iPwLevel)
        else if (Objects[j] is TAction) then
          TAction(Objects[j]).Enabled := (StrToInt(Strings[j]) <= iPwLevel)
        else if (Objects[j] is TMenuItem) then
          TMenuItem(Objects[j]).Enabled := (StrToInt(Strings[j]) <= iPwLevel);

  end;
  if (Count > 0) and (Assigned(Application.MainForm)) then
    Application.MainForm.Invalidate;
end;

{ Fragt den enabled-state einer Komponente ab          }
{ Parameter: Fenster, Komponente                       }
{ Rückgabe: enabled-state                              }
{------------------------------------------------------}
function TCtrlFreigabe.ComponentEnabled(
  pForm: TForm; pComponent: TComponent; iPwLevel: byte): boolean;  // 21.07.2005
{------------------------------------------------------}
var
  i, iIndex : integer;
begin
  Result := True;  // Kein Eintrag - nicht gesperrt
  iIndex := IndexOf(IntToStr(Integer(pForm)));
  if (iIndex >= 0) then
    with Col[iIndex] do
      for i := 0 to Count-1 do
        if (Integer(pComponent) = Integer(Objects[i])) then begin
          Result := (StrToInt(Strings[i]) <= iPwLevel);
          Break;
        end;
end;

end.
