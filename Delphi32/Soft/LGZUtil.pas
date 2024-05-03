{******************************************************************************}
{* Unit: Hilfsroutinen f�r Langzeitdaten                                      *}
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
{ Pr�fung, ob die �bergebene Kanalzahl mit der vorhandener Messwert-Langzeitdaten
  �bereinstimmt;
  �bergabe: Kanalzahl f�r Pr�fung
            Pfad der Langzeitdaten
  Ergebnis: true, wenn Langzeitdaten die �bergebene Kanalzahl haben }
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
  { Recordgr��e mit �bergebener Kanlzahl: }
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
{ berechnet die Gr��e des LGZ-Messwert-Records f�r die �bergebene Kanalzahl }
begin
  Result:=1 + SizeOf(DateRec) + SizeOf(TimeRec) + KanalZahl*SizeOf(KanalRec);
end;

{--------------------------------------------------}
function F_SizeOfLGZTRec (Kanalzahl: word): integer;
{--------------------------------------------------}
{ berechnet die Gr��e des LGZ-Tagsatz-Records f�r die �bergebene Kanalzahl }
begin
  Result:=2 + SizeOf(DateRec) + 2*KanalZahl*SizeOf(Zaehler_Rec);
end;

end.
