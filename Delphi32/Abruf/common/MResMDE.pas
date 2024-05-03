{******************************************************************************}
{* Unit: MDE-Konfigurationsdaten aus ASCII-Dateien lesen                      *}
{* 11.05.2005 WW                                                              *}
{******************************************************************************}
unit MResMDE;

interface

uses
  Forms, SysUtils, WStrUtils, WChars, WResConst, WStream;

const
  { Spaltennummern und -bedeutung für MrgTypMDE.dat }
  CMrgTypMDECol_Wieser   = 2;  { Spalte 2: Gerätetypnummern Wieser-System }
  CMrgTypMDECol_RWE_GasX = 3;  { Spalte 3: Gerätetypnummern MDE/Gas-X-System der RWE }


function GetMrgTypMDE_Zentrale (Pfad: string;
                                AMrgTyp_MDEPalm: integer; ACol_MDEZentrale: byte;
                                var MrgTyp_MDEZentrale: integer): boolean;
function GetMrgTypMDE_Palm (Pfad: string;
                            AMrgTyp_MDEZentrale: integer; ACol_MDEZentrale: byte;
                            var MrgTyp_MDEPalm: integer): boolean;

implementation

const
  { Dateinamen }
  CResMrgTypMDE = 'MrgTypMDE.dat';


{-------------------------------------------------------------------------------}
function GetMrgTypMDE_Zentrale (Pfad: string;
                                AMrgTyp_MDEPalm: integer; ACol_MDEZentrale: byte;
                                var MrgTyp_MDEZentrale: integer): boolean;
{-------------------------------------------------------------------------------}
{ liefert zur übergebenen, vom Wieser-MDE/Palm-System stammenden MRG-Typnummer die
  entsprechende MRG-Typnummer der MDE-Zentrale;
  Übergabe: Pfad zur Ressourcendatei
            MRG-Typnummer MDE-Palm
            der MDE-Zentrale zugeordnete Spaltennummer in der Datei (Konstanten
            CMrgTypMDECol_...)
  Rückgabe: MRG-Typnummer MDE-Zentrale (-1, wenn nicht zugeordnet)
  Ergebnis: true, wenn Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;

begin
  Result:=false;
  // Vorbelegung für Fall, daß keine MDE-Zentrale-Gerätetypnummer zugeordnet ist:
  MrgTyp_MDEZentrale:=-1;

  FName:=Pfad+CResMrgTypMDE;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          { Feld 1: MRG-Typ Wieser MDE/Palm }
          FieldStr:=ExtractString (S, NUL, CResTrenner, 0);
          if StrToIntDef (FieldStr, -1) = AMrgTyp_MDEPalm then begin
            try
              { Feld 'ACol_MDEZentrale': MRG-Typ der MDE-Zentrale }
              FieldStr:=ExtractString (S, CResTrenner, CResTrenner, ACol_MDEZentrale - 2);
              MrgTyp_MDEZentrale:=StrToInt (FieldStr);
              Break;
            except
              Result:=false;
              Break;
            end;
          end;
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;


{-------------------------------------------------------------------------------}
function GetMrgTypMDE_Palm (Pfad: string;
                            AMrgTyp_MDEZentrale: integer; ACol_MDEZentrale: byte;
                            var MrgTyp_MDEPalm: integer): boolean;
{-------------------------------------------------------------------------------}
{ liefert zur übergebenen, von einer MDE-Zentrale stammenden MRG-Typnummer die
  entsprechende MRG-Typnummer des Wieser-MDE/Palm-Systems;
  Übergabe: Pfad zur Ressourcendatei
            MRG-Typnummer MDE-Zentrale
            der MDE-Zentrale zugeordnete Spaltennummer in der Datei (Konstanten
            CMrgTypMDECol_...)
  Rückgabe: MRG-Typnummer MDE-Palm (-1, wenn nicht zugeordnet)
  Ergebnis: true, wenn Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;

begin
  Result:=false;
  // Vorbelegung für Fall, daß keine MDE/Palm-Gerätetypnummer zugeordnet ist:
  MrgTyp_MDEPalm:=-1;

  FName:=Pfad+CResMrgTypMDE;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          { Feld 'ACol_MDEZentrale': MRG-Typ der MDE-Zentrale }
          FieldStr:=ExtractString (S, CResTrenner, CResTrenner, ACol_MDEZentrale - 2);
          if StrToIntDef (FieldStr, -1) = AMrgTyp_MDEZentrale then begin
            try
             { Feld 1: MRG-Typ Wieser MDE/Palm }
              FieldStr:=ExtractString (S, NUL, CResTrenner, 0);
              MrgTyp_MDEPalm:=StrToInt (FieldStr);
              Break;
            except
              Result:=false;
              Break;
            end;
          end;
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

end.
