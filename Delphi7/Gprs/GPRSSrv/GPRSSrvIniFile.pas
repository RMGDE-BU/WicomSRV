{******************************************************************************}
{* Unit: Zugriff auf INI-Datei des GPRS-Servers                               *}
{* Version: 02.11.2006  WW                                                    *}
{******************************************************************************}
unit GPRSSrvIniFile;

interface

uses
  Classes, SysUtils, IniFiles, WPorts, WSysCon, AusgabeDirList;

type
  { Objekt für GPRSSRV.INI }

  TGPRSSrvIni = class (TIniFile)
  protected
    function GetServerPort: integer;
    function GetAusgabeKurzzeitwerte: boolean;
    function GetAnzeigeRohdaten: boolean;
    function GetDebugDatenProtokoll: boolean;
    function GetDebugFehlerProtokoll: boolean;
    function GetDebugStatistikProtokoll: boolean;
    function GetDebugRohdaten: boolean;
    function GetLetztTelegrammNr: integer;
    procedure SetLetztTelegrammNr (Value: integer);
  public
    constructor Create (Path: string);
    function GetAusgabeDirList (L: TAusgabeDirList; DefaultDir: string): integer;  { 26.07.2007, WW }
    property ServerPort: integer read GetServerPort;
    property AusgabeKurzzeitwerte: boolean read GetAusgabeKurzzeitwerte;  { 18.07.2007, WW }
    property AnzeigeRohdaten: boolean read GetAnzeigeRohdaten;
    property DebugDatenProtokoll: boolean read GetDebugDatenProtokoll;
    property DebugFehlerProtokoll: boolean read GetDebugFehlerProtokoll;
    property DebugStatistikProtokoll: boolean read GetDebugStatistikProtokoll;
    property DebugRohdaten: boolean read GetDebugRohdaten;
    property LetztTelegrammNr: integer read GetLetztTelegrammNr write SetLetztTelegrammNr;
  end;

implementation

const
  CGPRSSrvIni = 'GPRSSRV.INI';

  { Sections }
  CSectionServer        = 'Server';
  CSectionAusgabe       = 'Ausgabe';
  CSectionAusgabeDir_   = 'AusgabeDir_';
  CSectionAnzeige       = 'Anzeige';
  CSectionDebug         = 'Debug';
  CSectionTelegramme    = 'Telegramme';

  { Idents }
  CIdentPort                = 'Port';
  CIdentKurzzeitwerte       = 'Kurzzeitwerte';
  CIdentDirName             = 'DirName';
  CIdentBezeichnung         = 'Bezeichnung';
  CIdentClientVerbindungen  = 'ClientVerbindungen';
  CIdentDatenformat         = 'Datenformat';
  CIdentDatenProtokoll      = 'DatenProtokoll';
  CIdentFehlerProtokoll     = 'FehlerProtokoll';
  CIdentStatistikProtokoll  = 'StatistikProtokoll';
  CIdentRohdaten            = 'Rohdaten';
  CIdentLetztTelegrammNr    = 'LetztTelegrammNr';

  { Einstellwerte }
  CDefaultDir = '<DEFAULTDIR>';  // Platzhalter für Standard-Verzeichnis
  CDefaultBezeichnung        = '';
  CDefaultClientVerbindungen = false;


{ TGPRSSrvIni }

{--------------------------------------------}
constructor TGPRSSrvIni.Create (Path: string);
{--------------------------------------------}
begin
  inherited Create (Path + CGPRSSrvIni);
end;

{------------------------------------------}
function TGPRSSrvIni.GetServerPort: integer;
{------------------------------------------}
begin
  Result := ReadInteger (CSectionServer, CIdentPort, CIPPortGPRSSrv_Default);
end;

{----------------------------------------------------}
function TGPRSSrvIni.GetAusgabeKurzzeitwerte: boolean;
{----------------------------------------------------}
begin
  Result := ReadBool (CSectionAusgabe, CIdentKurzzeitwerte, true);
end;

{---------------------------------------------------------------------------------------}
function TGPRSSrvIni.GetAusgabeDirList (L: TAusgabeDirList; DefaultDir: string): integer;
{---------------------------------------------------------------------------------------}
{ liest Ausgabeverzeichnis-Einstellungen aus und gibt sie in L zurück;
  Ergebnis:  0 = Einstellungen OK
            -1 = Verzeichnisname fehlt (leer)
            -2 = gleiche Verzeichnisname(n) mehrfach vorhanden }
var
  SectionList: TStringList;
  i, j: integer;
  Section: string;
  AusgabeDirData: TAusgabeDirData;
  AusgabeDirDataObj: TAusgabeDirDataObj;

begin
  Result:=0;  // OK
  if L = nil then exit;
  L.Clear;

  SectionList:=TStringList.Create;
  try
    ReadSections(SectionList);
    for i:=0 to SectionList.Count-1 do begin
      Section:=SectionList[i];
      if Pos (CSectionAusgabeDir_, Section) = 1 then begin   { nur Sections, die mit AusgabeDir_... beginnen }
        with AusgabeDirData do begin
          DirName:=ReadString (Section, CIdentDirName, '');
          Bezeichnung:=ReadString (Section, CIdentBezeichnung, CDefaultBezeichnung);
          ClientVerbindungen:=ReadBool (Section, CIdentClientVerbindungen, CDefaultClientVerbindungen);
          Datenformat:=ReadInteger (Section, CIdentDatenformat, C_DF_KZW_IEC);  // Standard: Kurzzeitwerte für IEC-Kopplung

          { Prüfung auf leeren Verzeichnisnamen: }
          if length (DirName) = 0 then begin
            Result:=-1;
            exit;
          end;
          { Verzeichnisnamen-Eintrag mit Default-Platzhalter ersetzen durch
            übergebenes Default-Verzeichnis: }
          if Uppercase (DirName) = CDefaultDir then
            DirName:=DefaultDir;
          { vollständigen Verzeichnisnamen bilden: }
          DirName:=IncludeTrailingBackslash (ExpandUNCFileName (DirName));
          { Prüfung auf mehrfach vorhandene, gleiche Verzeichnisnamen: }
          for j:=0 to (L.Count-1) do begin
            if TAusgabeDirDataObj (L [j]).Daten.DirName = DirName then begin
              Result:=-2;
              exit;
            end;
          end;
        end;

        { Ausgabeverzeichnis-Einstellung in Liste laden: }
        AusgabeDirDataObj:=TAusgabeDirDataObj.Create (AusgabeDirData);
        L.Add (AusgabeDirDataObj);
      end;
    end;  { for i }
  finally
    SectionList.Free;
  end;

  { keine Ausgabeverzeichnis-Einstellungen vorhanden: Default-Verzeichnis in
    Liste laden }
  if L.Count = 0 then begin
    with AusgabeDirData do begin
      DirName:=IncludeTrailingBackslash (ExpandUNCFileName (DefaultDir));
      Bezeichnung:=CDefaultBezeichnung;
      ClientVerbindungen:=CDefaultClientVerbindungen;
      Datenformat:=C_DF_KZW_IEC;  // Standard: Kurzzeitwerte für IEC-Kopplung
    end;
    AusgabeDirDataObj:=TAusgabeDirDataObj.Create (AusgabeDirData);
    L.Add (AusgabeDirDataObj);
  end;
end;

{-----------------------------------------------}
function TGPRSSrvIni.GetAnzeigeRohdaten: boolean;
{-----------------------------------------------}
begin
  Result:=ReadBool (CSectionAnzeige, CIdentRohdaten, false);
end;

{---------------------------------------------------}
function TGPRSSrvIni.GetDebugDatenProtokoll: boolean;
{---------------------------------------------------}
begin
  Result:=ReadBool (CSectionDebug, CIdentDatenProtokoll, false);
end;

{----------------------------------------------------}
function TGPRSSrvIni.GetDebugFehlerProtokoll: boolean;
{----------------------------------------------------}
begin
  Result:=ReadBool (CSectionDebug, CIdentFehlerProtokoll, false);
end;

{-------------------------------------------------------}
function TGPRSSrvIni.GetDebugStatistikProtokoll: boolean;
{-------------------------------------------------------}
begin
  Result:=ReadBool (CSectionDebug, CIdentStatistikProtokoll, false);
end;

{---------------------------------------------}
function TGPRSSrvIni.GetDebugRohdaten: boolean;
{---------------------------------------------}
begin
  Result:=ReadBool (CSectionDebug, CIdentRohdaten, false);
end;

{------------------------------------------------}
function TGPRSSrvIni.GetLetztTelegrammNr: integer;
{------------------------------------------------}
begin
  Result:=ReadInteger (CSectionTelegramme, CIdentLetztTelegrammNr, 0);
end;

{---------------------------------------------------------}
procedure TGPRSSrvIni.SetLetztTelegrammNr (Value: integer);
{---------------------------------------------------------}
begin
  WriteInteger (CSectionTelegramme, CIdentLetztTelegrammNr, Value);
end;

end.
