{******************************************************************************}
{* Unit: Konvertierung von GPRS-Kurzzeit-Telegrammdaten                       *}
{* 18.03.2009 WW                                                              *}
{* 21.04.2014 WW mit Log-Pfad                                                 *}
{******************************************************************************}
unit DGPRS_KZWKonv;

INTERFACE

Uses
  SysUtils, DListen, WStream, WChars, DALKonv, ErrConst, ErrPrc32, LogFile,
  DKurzzeitWerte, AusgabeDirList;


function KonvertDSfG_KZW (ArLbDatenListe: TDSfGDataList; GPRSKennung: string;
                          GPRSTelegrammRohdaten: string;
                          GPRSTelegrammNr: integer;
                          AusgabeDirList: TAusgabeDirList;
                          sLogPath: string): boolean;

procedure WriteGPRSKonvErrorLog (Fehlergruppe, Fehlercode: integer;
  sInfo, sTelegramm, sLogPath: string);
procedure WriteGPRSKonvDataLog (sKonvData, sTelegramm, sLogPath: string);

IMPLEMENTATION

{-----------------------------------------------------------------}
procedure WriteGPRSKonvErrorLog (Fehlergruppe, Fehlercode: integer;
  sInfo, sTelegramm, sLogPath: string);
{-----------------------------------------------------------------}
{ Eintrag in Konvertierungs-Fehler-Logdatei schreiben;
  Übergabe: Logdatei-Eintrag }
var
  FName: string;
  S: string;
begin
  FName:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_GPRSKonvError';

  if (Fehlergruppe > -1) AND (Fehlercode > -1) then
    S:=GetStatusText (Fehlergruppe) + ': ' + GetErrorText (Fehlergruppe, Fehlercode)
  else
    S:='';
  S:=sTelegramm + CR + LF + S + CR + LF + sInfo;
  WriteDebugLog (sLogPath, FName, '***' + CR + LF + S + CR + LF,
    true, lt_Error);
end;

{-----------------------------------------------------------------------}
procedure WriteGPRSKonvDataLog (sKonvData, sTelegramm, sLogPath: string);
{-----------------------------------------------------------------------}
{ Eintrag in Konvertierungs-Daten-Logdatei schreiben;
  Übergabe: Logdatei-Eintrag }
var
  FName: string;
  S: string;
begin
  FName:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_GPRSKonvData';
  S:=sTelegramm + CR + LF + sKonvData;
  WriteDebugLog (sLogPath, FName, '***' + CR + LF + S + CR + LF);
end;

{------------------------------------------------------------------}
function KonvertDSfG_KZW (ArLbDatenListe: TDSfGDataList;
                          GPRSKennung: string;
                          GPRSTelegrammRohdaten: string;
                          GPRSTelegrammNr: integer;
                          AusgabeDirList: TAusgabeDirList;
                          sLogPath: string): boolean;
{------------------------------------------------------------------}
{ GPRS-Kurzzeit-Telegrammdaten (File mit TDSfGData-Struktur) in Ascii-File (Format für
  IEC-Leitwartenkopplung) konvertieren;
  Übergabe: Archiv/Logbuch-Datenliste
            Kennung aus GPRS-Datentelegramm
            Datentyp
            Verzeichnisliste für Ausgabe der Kurzzeitwert-Dateien
            Pfad für Log-Dateien
  Ergebnis: true, wenn ok }
var
  i, l: integer;
  DEL: string;
  EAdr: string;
  SatzData: TDSfGSatzData;
  FS_SatzData: TFileOfRecStream;
  FName_SatzData: string;
  DKZWData: TDKZWData;
  DSfGKurzzeitwert: TDSfGKurzzeitwert;
  DirName: string;
  bOK: boolean;

begin
  Result:=true;

  for l:=0 to ArLbDatenListe.Count-1 do begin  // für alle ArLb-Zwischendateien
    EAdr:=TDSfGDataListObj (ArLbDatenListe.Objects[l]).EAdr;
    DEL:=TDSfGDataListObj (ArLbDatenListe.Objects[l]).DEL;  // DE-Adresse des Archivkanals

    { strukturierte Quell-Datei mit Kurzzeitwert eines Kanals öffnen: }
    FName_SatzData:=ArLbDatenListe[l];
    if FileExists(FName_SatzData) then begin
      FS_SatzData:=TFileOfRecStream.Create (FName_SatzData, fmOpenRead OR fmShareDenyWrite, SizeOf (SatzData));
      try
        if FS_SatzData.RecCount > 0 then begin  // es ist nur ein Datensatz enthalten
          FS_SatzData.ReadRec (SatzData);  // SatzData lesen
          // Record für Kurzzeitwert-Daten zusammensetzen:
          DKZWData.TelegrNr:=GPRSTelegrammNr;
          DKZWData.Kennung:=GPRSKennung;
          DKZWData.EAdr:=EAdr;
          DKZWData.DEL:=DEL;
          DKZWData.DatumZeit:=SatzData.DatumZeit;
          DKZWData.Wert:=SatzData.WertAsDouble;

          // Kurzzeitwert in alle Ausgabe-Verzeichnisse schreiben:
          for i:=0 to (AusgabeDirList.Count-1) do begin
            DirName:=TAusgabeDirDataObj (AusgabeDirList [i]).Daten.DirName;
            DSfGKurzzeitwert:=TDSfGKurzzeitwert.Create (DirName);
            try
              bOK:=true;
              case TAusgabeDirDataObj (AusgabeDirList [i]).Daten.Datenformat of
                C_DF_KZW_IEC:
                  begin
                    // Kurzzeitwert in Ascii-Datei zur Weiterverarbeitung durch
                    // IEC-Kopplung schreiben; 26.07.2007, WW
                    bOK:=DSfGKurzzeitwert.WriteFile_IECKopplung (DKZWData);
                  end;

                C_DF_KZW_Deb:
                  begin
                    // Kurzzeitwert in ASCII-Datei schreiben (Debug-Format); 03.06.2008, WW
                    bOK:=DSfGKurzzeitwert.WriteFile_Debug (DKZWData);
                  end;

                C_DF_KZW_Asc:
                  begin
                    // Kurzzeitwert in ASCII-Datei schreiben (Standard-ASCII-Format); 30.07.2008, WW
                    bOK:=DSfGKurzzeitwert.WriteFile_Ascii (DKZWData);
                  end;
              end;  { case }

              if not bOK then begin
                WriteGPRSKonvErrorLog (ST_FILEERROR, FILEERR_COULDNOTWRITE,
                                       DirName + CR + LF + GPRSKennung,
                                       GPRSTelegrammRohdaten, sLogPath);
                Result:=false;
              end;
            finally
              DSfGKurzzeitwert.Free;
            end;
          end;
        end;  { if FS_SatzData.RecCount > 0 }
      finally
        FS_SatzData.Free;
      end;
    end;  // if FileExists(FName_SatzData)
  end;  { for l }
end;

end.


