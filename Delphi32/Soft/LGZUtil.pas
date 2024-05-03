{******************************************************************************}
{* Unit: Hilfsroutinen für Langzeitdaten                                      *}
{* Version: 13.05.2002                                                        *}
{******************************************************************************}
unit LGZUtil;

interface

uses
  SysUtils, novell, T_Tools, LGZType;

function CheckLGZKanalzahl (AKanalzahl: byte; LGZPath: string): boolean;
function F_SizeOfLGZSRec (Kanalzahl: word): integer;
function F_SizeOfLGZTRec (Kanalzahl: word): integer;

implementation

{----------------------------------------------------------------------}
function CheckLGZKanalzahl (AKanalzahl: byte; LGZPath: string): boolean;
{----------------------------------------------------------------------}
{ Prüfung, ob die übergebene Kanalzahl mit der vorhandener Messwert-Langzeitdaten
  übereinstimmt;
  Übergabe: Kanalzahl für Prüfung
            Pfad der Langzeitdaten
  Ergebnis: true, wenn Langzeitdaten die übergebene Kanalzahl haben }
var
  LGZSRecSize: integer;
  SR: TSearchRec;
  Stop: boolean;

  {--------------}
  procedure Check;
  {--------------}
  var
    F: TFileStreamExt;
    Ok: boolean;
  begin
    F:=TFileStreamExt.Create (LGZPath + SR.Name, fmOpenRead OR fmShareDenyWrite, Ok);
    try
      if Ok then begin
        if F.Size > 0 then begin
          Stop:=true;
          if F.Size MOD LGZSRecSize <> 0 then
            Result:=false;
        end;
      end;
    finally
      F.Free;
    end;
  end;

begin
  Result:=true;
  { Recordgröße mit übergebener Kanlzahl: }
  LGZSRecSize:=1 + SizeOf(DateRec) + SizeOf(TimeRec) + AKanalZahl*SizeOf(KanalRec);
  { nicht nach LGZD*.DAT suchen, sonst findet er evtl. LGZDVERZ.DAT ! }
  if FindFirst (LGZPath + 'LGZD0*.DAT', faAnyFile, SR) = 0 then begin
    Stop:=false;
    Check;
    while not Stop AND (FindNext (SR) = 0) do
      Check;
  end;
  FindClose (SR);
end;

{--------------------------------------------------}
function F_SizeOfLGZSRec (Kanalzahl: word): integer;
{--------------------------------------------------}
{ berechnet die Größe des LGZ-Messwert-Records für die übergebene Kanalzahl }
begin
  Result:=1 + SizeOf(DateRec) + SizeOf(TimeRec) + KanalZahl*SizeOf(KanalRec);
end;

{--------------------------------------------------}
function F_SizeOfLGZTRec (Kanalzahl: word): integer;
{--------------------------------------------------}
{ berechnet die Größe des LGZ-Tagsatz-Records für die übergebene Kanalzahl }
begin
  Result:=2 + SizeOf(DateRec) + 2*KanalZahl*SizeOf(Zaehler_Rec);
end;

end.
