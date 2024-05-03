{******************************************************************************}
{* Unit: Zugriff auf Fehlertabellen der Wieser DSfG-DFÜ                       *}
{* Version: 08.05.2001 WW                                                     *}
{******************************************************************************}
unit DDbDfueErr;

interface

uses
  Classes, SysUtils, WTables, WSysCon;


function GetWieserDSfGDfueFehlertexte_Anstehend (Pfad: string; Fehler_Anstehend: string;
                                                 Fehlertextliste: TStringList): boolean;
function GetWieserDSfGDfueFehlertexte_DSfG (Pfad: string; Fehler_DSfG: integer;
                                            Fehlertextliste: TStringList): boolean;

implementation

{--------------------------------------------------------------------------------------}
function GetWieserDSfGDfueFehlertexte_Anstehend (Pfad: string; Fehler_Anstehend: string;
                                                 Fehlertextliste: TStringList): boolean;
{--------------------------------------------------------------------------------------}
{ gibt Fehlertextliste für die anstehenden Fehler einer Wieser-DSfG-DFÜ
  zurück;
  Übergabe: Fehler_anstehend { Teilstring aus Antwort auf F-Befehl)
  Rückgabe: Fehlertextliste
  Ergebnis: true, wenn Fehlertexte ermittelt werden konnten }
var
  ErrTable: TTableExt;
  i: integer;
  chFehler: string;

begin
  Result:=false;
  ErrTable:=TTableExt.Create (nil);
  try
    ErrTable.DatabaseName:=Pfad;
    ErrTable.TableName:=C_Tb_DErrAnst;
    if ErrTable.Exists then begin
      if ErrTable.OpenShared then begin
        try
          for i:=1 to length (Fehler_Anstehend) do begin
            chFehler:=Fehler_Anstehend [i];
            if ErrTable.FindKey ([chFehler]) then
              Fehlertextliste.Add (ErrTable.FieldByName (C_Tf_DErrAnst_Fehlertext).AsString)
            else
              Fehlertextliste.Add ('unbekannter Fehler ' + chFehler);
          end;
        finally
          ErrTable.Close;
        end;
        Result:=true;
      end;
    end;
  finally
    ErrTable.Free;
  end;
end;

{---------------------------------------------------------------------------------}
function GetWieserDSfGDfueFehlertexte_DSfG (Pfad: string; Fehler_DSfG: integer;
                                            Fehlertextliste: TStringList): boolean;
{---------------------------------------------------------------------------------}
{ gibt Fehlertextliste für die DSfG-Fehler einer Wieser-DSfG-DFÜ zurück;
  Übergabe: Fehler_DSfG { Teilstring aus Antwort auf F-Befehl)
  Rückgabe: Fehlertextliste
  Ergebnis: true, wenn Fehlertexte ermittelt werden konnten }
var
  ErrTable: TTableExt;
  i: integer;
  DSfGErrorCode: integer;

begin
  Result:=false;
  ErrTable:=TTableExt.Create (nil);
  try
    ErrTable.DatabaseName:=Pfad;
    ErrTable.TableName:=C_Tb_DErrDSfG;
    if ErrTable.Exists then begin
      if ErrTable.OpenShared then begin
        try
          DSfGErrorCode:=1;
          for i:=1 to 16 do begin
            if (Fehler_DSfG AND DSfGErrorCode) <> 0 then begin
              if ErrTable.FindKey ([DSfGErrorCode]) then
                Fehlertextliste.Add (ErrTable.FieldByName (C_Tf_DErrDSfG_Fehlertext).AsString)
              else
                Fehlertextliste.Add ('unbekannter Fehler ' + IntToStr (DSfGErrorCode));
            end;
            DSfGErrorCode:=DSfGErrorCode SHL 1;
          end;
        finally
          ErrTable.Close;
        end;
        Result:=true;
      end;
    end;
  finally
    ErrTable.Free;
  end;
end;

end.
