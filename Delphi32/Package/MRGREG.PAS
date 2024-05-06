{ 13.12.2005 WW; ohne Property-Editoren bei Delphi 7, um Programme ohne Lauf-  }
{                zeitpackages ausführen zu können                              }
unit Mrgreg;

interface

procedure Register;

implementation

uses
  Classes, Mrgstd,
{$IFDEF VER150}
  ScktComp;
{$ELSE}
  DsgnIntf, SysUtils, DBTables;

type
  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDatabaseNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;


{ TDBStringProperty }

function TDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{ TDatabaseNameProperty }

procedure TDatabaseNameProperty.GetValueList(List: TStrings);
begin
  Session.GetDatabaseNames(List);
end;
{$ENDIF}

procedure Register;
begin
  RegisterComponents ('Wieser', [TMSSQL]);
  RegisterComponents ('Wieser', [TMSListBox]);
  RegisterComponents ('Wieser', [TMSStammdaten]);
{$IFNDEF VER150}
  RegisterPropertyEditor (TypeInfo (TFileName), TMSSQL,
                          'MrgStamm', TDatabaseNameProperty);
  RegisterPropertyEditor (TypeInfo (TFileName), TMSSQL,
                          'MrgSys', TDatabaseNameProperty);
  RegisterPropertyEditor (TypeInfo (TFileName), TMSListBox,
                          'DatabaseName', TDatabaseNameProperty);
  RegisterPropertyEditor (TypeInfo (TFileName), TMSListBox,
                          'SysDatabase', TDatabaseNameProperty);
  RegisterPropertyEditor (TypeInfo (TFileName), TMSStammdaten,
                          'DatabaseName', TDatabaseNameProperty);
  RegisterPropertyEditor (TypeInfo (TFileName), TMSStammdaten,
                          'SysDatabase', TDatabaseNameProperty);
{$ENDIF}

{$IFDEF VER150}
  // Komponenten entsprechend zu Delphi5 "von Hand" registrieren
  RegisterComponents ('Internet', [TServerSocket]);
  RegisterComponents ('Internet', [TClientSocket]);
{$ELSE}
  // MSStammdatenPlus-Komponente wird nicht mehr verwendet
  RegisterComponents ('Wieser', [TMSStammdatenPlus]);
  RegisterPropertyEditor (TypeInfo (TFileName), TMSStammdatenPlus,
                          'DatabaseName', TDatabaseNameProperty);
  RegisterPropertyEditor (TypeInfo (TFileName), TMSStammdatenPlus,
                          'SysDatabase', TDatabaseNameProperty);
{$ENDIF}

end;

end.

