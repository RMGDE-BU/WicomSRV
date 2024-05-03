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
  C_DF_KZW_IEC = 0;  // Kurzzeitwerte für IEC-Kopplung
  C_DF_KZW_Deb = 1;  // Kurzzeitwerte für Debugging (Kanalwerte werden fortgeschrieben)
  C_DF_KZW_Asc = 2;  // Kurzzeitwerte im Standard-ASCII-Format (Kanalwerte werden fortgeschrieben)

type
  { Struktur für Ausgabeverzeichnis-Daten }

  TAusgabeDirData = record
    DirName: string;      { Ordnername }
    Bezeichnung: string;  { Bezeichnung für Ausgabeverzeichnis }
    ClientVerbindungen: boolean;  { Ausgabe der Client-Verbindungsdatei ja/nein }
    Datenformat: integer; { Format der Daten in der Ausgabedatei }
  end;

  { Objekt für Ausgabeverzeichnis-Daten }

  TAusgabeDirDataObj = class(TObject)
  public
    Daten: TAusgabeDirData;
    constructor Create (ADaten: TAusgabeDirData);
  end;

  { Objektliste für definierte Ausgabeverzeichnisse }

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
