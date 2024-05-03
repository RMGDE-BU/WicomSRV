{******************************************************************************}
{* Unit: Liste der Ausgabe-Verzeichnisse zu GPRS-Serverdaten                  *}
{* 04.09.2002 WW                                                              *}
{******************************************************************************}
unit AusgabeDirList;

interface

uses
  Classes, Contnrs;

const
  { Datenformat-Konstanten }
  C_DF_KZW_IEC = 0;  // Kurzzeitwerte f�r IEC-Kopplung
  C_DF_KZW_Deb = 1;  // Kurzzeitwerte f�r Debugging (Kanalwerte werden fortgeschrieben)
  C_DF_KZW_Asc = 2;  // Kurzzeitwerte im Standard-ASCII-Format (Kanalwerte werden fortgeschrieben)

type
  { Struktur f�r Ausgabeverzeichnis-Daten }

  TAusgabeDirData = record
    DirName: string;      { Ordnername }
    Bezeichnung: string;  { Bezeichnung f�r Ausgabeverzeichnis }
    ClientVerbindungen: boolean;  { Ausgabe der Client-Verbindungsdatei ja/nein }
    Datenformat: integer; { Format der Daten in der Ausgabedatei }
  end;

  { Objekt f�r Ausgabeverzeichnis-Daten }

  TAusgabeDirDataObj = class(TObject)
  public
    Daten: TAusgabeDirData;
    constructor Create (ADaten: TAusgabeDirData);
  end;

  { Objektliste f�r definierte Ausgabeverzeichnisse }

  TAusgabeDirList = class(TObjectList)
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation


{ TAusgabeDirDataObj }

{--------------------------------------------------------------}
constructor TAusgabeDirDataObj.Create (ADaten: TAusgabeDirData);
{--------------------------------------------------------------}
begin
  inherited Create;
  Daten:=ADaten;
end;

end.
