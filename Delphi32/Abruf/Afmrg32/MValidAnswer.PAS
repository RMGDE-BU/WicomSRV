{******************************************************************************}
{* Unit: Auswerten von Schnittstellen-Antworten auf MRG- und FUP-Befehle      *}
{* 15.12.2000 WW                                                              *}
{* 28.06.2018 WW  Anpassung Auswertung Tritschler ZeitSync-Antwort            *}
{* 30.04.2019 WW  Tritschler Protokoll FTL-intern                             *}
{* 03.11.2020 WW  Anpassung ValidFTL_internAntwort für VC2 (Meldeliste, Melde-*}
{*                statistik löschen)                                          *}
{******************************************************************************}
unit MValidAnswer;

INTERFACE

uses
  Classes, SysUtils, WStrUtils, T_Tools, T_Zeit, ErrConst, WChars;

const
  { Antwortcodes auf KE-Befehle }

  kec_Daten_folgen          = 101;
  kec_Warten_auf_Daten      = 102;
  kec_Befehl_falsche_Syntax = 103;
  kec_Befehl_nicht_erlaubt  = 104;
  kec_Keine_Daten_vorhanden = 106;


function ValidMRGAntwort (Kommando_Soll: string;
                          ModemAbrufgruppe: integer;
                          var Antwort: string;
                          var Fehlergruppe: integer;
                          var Fehlercode: integer): boolean;

function ValidMRGAntwortfile (Kommando_Soll: string;
                              ModemAbrufgruppe: integer;
                              AntwortfileName: TFilename;
                              var Fehlergruppe: integer;
                              var Fehlercode: integer;
                              doDeleteRohfile: boolean): boolean;

function ValidDSfGUmschaltAntwort (Antwort: string; SlaveAdr_Soll: Char): boolean;

function ValidSlaveKennung (Antwort: string; Kennung_Soll: string;
                            var Kennung_Ist: string): boolean;

function GetRufListe (Antwort: string): string;

function ValidElster_IEC1107DatenTelegrammAntwort (Antwort: string;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;

function ValidIEC1107DatenTelegrammAntwort (Antwort: string;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;

function ValidKEAntwort (var Antwort: string;
                         var AntwCode: integer;
                         var Fehlergruppe: integer;
                         var Fehlercode: integer): boolean;

function ValidFTLAntwort (FunktionNr_Soll: string;
                          Antwort: string;
                          var Fehlergruppe: integer;
                          var Fehlercode: integer): boolean;
                          
function ValidFTL_internAntwort (sBlockNr_Befehl: string; sAntwort: string;
                                 var Fehlergruppe: integer;
                                 var Fehlercode: integer): boolean;

function ValidTritschler_IECAntwort_20_ZeitSync (Antwort: string;
                                                 var Fehlergruppe: integer;
                                                 var Fehlercode: integer;
                                                 var dtGeraeteZeit: TDateTime): boolean;

function ValidCorusSAMAntwort (var Antwort: string;
                               var Fehlergruppe: integer;
                               var Fehlercode: integer;
                               var ETX_Pos: integer): boolean;

IMPLEMENTATION

uses DateUtils;

{----------------------------------------------------------}
function ValidMRGAntwort (Kommando_Soll: string;
                          ModemAbrufgruppe: integer;
                          var Antwort: string;
                          var Fehlergruppe: integer;
                          var Fehlercode: integer): boolean;
{----------------------------------------------------------}
{ überprüft eine MRG-Antwort auf Richtigkeit;
  Übergabe: in Antwort erwarteter Kommando-String
            Modem-Abrufgruppe
  Übergabe/Rückgabe: Antwort auf MRG-Befehl;
  Rückgabe: Fehlergruppe, Fehlercode für Journal
  Ergebnis: true, wenn Antwort ok }
var
  P: integer;
  Kommando_Antw: string;

begin
  Result:=false;
  { Antwort-String: alles vor dem STX ist Müll und muß weg }
  P:=Pos (STX, Antwort);
  Antwort:=Copy (Antwort, P, length(Antwort));

  Fehlergruppe:=COM_MRGERROR;  // Fehlergruppe: MRG-Fehler
  Fehlercode:=-1;  // Fehlercode: unbelegt
  if length (Antwort) >= 3 then begin
    if ModemAbrufgruppe = 12 then begin  // EC 900
      case Antwort[2] of                { Befehlszeichen oder MRG-Fehlerzeichen }
        '?': Fehlercode:=MRGERR_KOMMANDOUNPLAUSIBEL;
        '!': Fehlercode:=MRGERR_MOMENTANNICHTMOEGLICH;
        '>': Fehlercode:=MRGERR_KOMMANDOUNBEKANNT;
        '=': Fehlercode:=MRGERR_KEINEBERECHTIGUNG;
      end;
    end
    else begin  // übrige Geräte (MRG, EC 694)
      case Antwort[2] of                { Befehlszeichen oder MRG-Fehlerzeichen }
        '?': Fehlercode:=MRGERR_KOMMANDOUNPLAUSIBEL;
        '!': Fehlercode:=MRGERR_KEINEBERECHTIGUNG;
        ':': Fehlercode:=MRGERR_FALSCHESFORMAT;
        '<': Fehlercode:=MRGERR_REVISION;
        '>': Fehlercode:=MRGERR_KOMMANDOUNBEKANNT;
        '=': Fehlercode:=MRGERR_AENDERUNGNICHTZULAESSIG;
      end;
    end;

    if Fehlercode = -1 then begin
      { Vergleich des in der Antwort enthaltenen Befehls mit dem Soll-Kommando; 08.07.2005, WW }
      if length (Kommando_Soll) > 0 then begin
        Kommando_Antw:=Copy (Antwort, 2, length (Kommando_Soll));  // Kommando folgt nach STX
        if Kommando_Antw <> Kommando_Soll then begin
          Fehlercode:=MRGERR_ANTWORTUNERWARTET;
          exit;
        end;
      end
      else begin
        if Antwort [2] = 'Y' then begin  // Antwort nachdem Batterie-Geräte aufgewacht ist; 10.08.2011, WW
          Fehlercode:=MRGERR_ANTWORTUNERWARTET;
          exit;
        end;
      end;

      { Antwort ist OK: }
      Fehlergruppe:=0;
      Fehlercode:=0;
      Result:=true;
    end;
  end else
    Fehlercode:=MRGERR_ANTWORTUNVOLLSTAENDIG;
end;

{---------------------------------------------------------------}
function ValidMRGAntwortfile (Kommando_Soll: string;
                              ModemAbrufgruppe: integer;
                              AntwortfileName: TFilename;
                              var Fehlergruppe: integer;
                              var Fehlercode: integer;
                              doDeleteRohfile: boolean): boolean;
{---------------------------------------------------------------}
{ überprüft eine MRG-File-Antwort auf Richtigkeit;
  Übergabe: in Antwort erwarteter Kommando-String
            Modem-Abrufgruppe
            Name der Datei, die Antwort auf MRG-Befehl enthält;
            Flag doDeleteRohfile (wenn true, wird Rohfile bei fehlerhafter Antwort gelöscht)
  Rückgabe: Fehlergruppe, Fehlercode für Journal
  Ergebnis: true, wenn Antwort ok }
var
  FS: TFileStream;
  zeichen: char;
  Antwort: string;
  Kommando_Antw: string;
  Anz, i: integer;

begin
  Result:=false;
  try
    FS:=TFileStream.Create (AntwortfileName, fmOpenRead OR fmShareDenyWrite);
    try
      Fehlergruppe:=COM_MRGERROR;  // Fehlergruppe: MRG-Fehler
      Fehlercode:=-1;  // Fehlercode: unbelegt
      if FS.Size >= 3 then begin
        FS.Read (zeichen, 1);   { STX }
        Antwort:=zeichen;
        FS.Read (zeichen, 1);   { Befehlszeichen oder MRG-Fehlerzeichen }
        Antwort:=Antwort + zeichen;

        if ModemAbrufgruppe = 12 then begin  // EC 900
          case Antwort[2] of                { Befehlszeichen oder MRG-Fehlerzeichen }
            '?': Fehlercode:=MRGERR_KOMMANDOUNPLAUSIBEL;
            '!': Fehlercode:=MRGERR_MOMENTANNICHTMOEGLICH;
            '>': Fehlercode:=MRGERR_KOMMANDOUNBEKANNT;
            '=': Fehlercode:=MRGERR_KEINEBERECHTIGUNG;
          end;
        end
        else begin  // übrige Geräte (MRG, EC 694)
          case Antwort[2] of
            '?': Fehlercode:=MRGERR_KOMMANDOUNPLAUSIBEL;
            '!': Fehlercode:=MRGERR_KEINEBERECHTIGUNG;
            ':': Fehlercode:=MRGERR_FALSCHESFORMAT;
            '<': Fehlercode:=MRGERR_REVISION;
            '>': Fehlercode:=MRGERR_KOMMANDOUNBEKANNT;
            '=': Fehlercode:=MRGERR_AENDERUNGNICHTZULAESSIG;
          end;
        end;

        if Fehlercode = -1 then begin
          { Vergleich des in der Antwort enthaltenen Befehls mit dem Soll-Kommando; 08.07.2005, WW }
          if length (Kommando_Soll) > 0 then begin
            { Anzahl der zusätzlich auszulesenden Kommando-Zeichen ermitteln (Befehlszeichen
              wurde schon gelesen): }
            Anz:=length (Kommando_Soll) - 1;
            if Anz > 0 then begin
              if FS.Size < (2 + Anz) then begin
                Fehlercode:=MRGERR_ANTWORTUNVOLLSTAENDIG;
                exit;
              end;
             { weitere Befehlszeichen lesen }
             for i:=1 to Anz do begin
                FS.Read (zeichen, 1);
                Antwort:=Antwort + zeichen;
              end;
            end;

            Kommando_Antw:=Copy (Antwort, 2, length (Kommando_Soll));  // Kommando folgt nach STX
            if Kommando_Antw <> Kommando_Soll then begin
              Fehlercode:=MRGERR_ANTWORTUNERWARTET;
              exit;
            end;
          end;

          { im File enthaltene Antwort ist OK: }
          Fehlergruppe:=0;
          Fehlercode:=0;
          Result:=true;
        end;
      end else
        Fehlercode:=MRGERR_ANTWORTUNVOLLSTAENDIG;
    finally
      FS.Free;
    end;

    if not Result AND doDeleteRohfile then
      DeleteFile (AntwortfileName);
  except
    Fehlergruppe:=ST_FILEERROR;
    Fehlercode:=FILEERR_COULDNOTOPEN;
  end;
end;

{--------------------------------------------------------------------------------}
function ValidDSfGUmschaltAntwort (Antwort: string; SlaveAdr_Soll: Char): boolean;
{--------------------------------------------------------------------------------}
{ überprüft Antwort auf DSfG-Umschaltbefehl;
  Übergabe: Antwort
            Soll-SlaveAdr
  Ergebnis: true, wenn auf richtigen DSfG-Slave umgeschaltet wurde }
var
  Soll, Ist: string [2];

begin
  Soll := '}' + SlaveAdr_Soll;
  Ist := Copy(Antwort, Length(Antwort)-2, 2);
  Result := Ist = Soll;
end;

{----------------------------------------------------------------}
function ValidSlaveKennung (Antwort: string; Kennung_Soll: string;
                            var Kennung_Ist: string): boolean;
{----------------------------------------------------------------}
{ ermittelt Kennung (eines Slave) aus Rohantwort und vergleicht sie mit Soll-Kennung;
  Übergabe: Antwort
            Soll-Kennung
  Rückgabe: Ist-Kennung
  Ergebnis: true, wenn Soll- und Ist-Kennung übereinstimmen }
var
  S: string;
begin
  Result := False;
  Kennung_Ist:='';
  S:=ExtractString (Antwort, STX, ETX, 0);
  Kennung_Ist:=Copy (S, 5, length(S) - 4);
  if FilterKennung(Kennung_Ist) = FilterKennung(Kennung_Soll) then
    Result:=true;
end;

{---------------------------------------------}
function GetRufListe (Antwort: string): string;
{---------------------------------------------}
var
  P: integer;
begin
  P:=Pos ('#', Antwort);
  if P > 0 then
    Result:=Copy (Antwort, P + 1, 14)
  else
    Result:='';
end;

{-----------------------------------------------------------------}
function ValidElster_IEC1107DatenTelegrammAntwort (Antwort: string;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{-----------------------------------------------------------------}
{ überprüft eine IEC 1107-Datentelegramm-Antwort von Elster-Geräten auf Richtigkeit;
  Übergabe: Antwort
  Rückgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn Antwort ok }
var
  AntwBuf: string;

begin
  Result:=false;
  Fehlergruppe:=COM_MRGERROR;
  if length (Antwort) >= 3 then begin
    AntwBuf:=ExtractString (Antwort, STX, ETX, 0);    { Datenteil der Antwort }
    { erste zwei Zeichen auf # prüfen: }
    if Pos ('#', Copy (AntwBuf, 1, 2)) > 0 then begin   { Antwort enthält Fehler-Telegramm }
//      IEC1107_Fehlercode:=ExtractString (AntwBuf, '#', ')', 0);
      Fehlercode:=MRGERR_FEHLERTELEGRAMM;
    end
    else begin
      { Antwort ist OK: }
      Fehlergruppe:=0;
      Fehlercode:=0;
      Result:=true;
    end;
  end else
    Fehlercode:=MRGERR_ANTWORTUNVOLLSTAENDIG;
end;

{-------------------------------------------------------------}
function ValidIEC1107DatenTelegrammAntwort (Antwort: string;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{-------------------------------------------------------------}
{ überprüft eine Standard IEC 1107-Datentelegramm-Antwort auf Richtigkeit (z.B.
  Actaris Sparklog, Kamstrup UNIGAS 300);
  Übergabe: Antwort
  Rückgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn Antwort ok }
var
  AntwBuf: string;

begin
  Result:=false;
  Fehlergruppe:=COM_MRGERROR;
  if length (Antwort) >= 3 then begin
    AntwBuf:=ExtractString (Antwort, STX, ETX, 0);    { Datenteil der Antwort }
    AntwBuf:=ExtractString (AntwBuf, '(', ')', 0);    { Wert steht in Klammern }
    { auf ERROR prüfen: }
    if Pos ('ERROR', AntwBuf) > 0 then  { Antwort enthält Fehler-Telegramm }
      Fehlercode:=MRGERR_FEHLERTELEGRAMM
    else begin
      { Antwort ist OK: }
      Fehlergruppe:=0;
      Fehlercode:=0;
      Result:=true;
    end;
  end else
    Fehlercode:=MRGERR_ANTWORTUNVOLLSTAENDIG;
end;

{----------------------------------------------------------}
function ValidKEAntwort (var Antwort: string;
                         var AntwCode: integer;
                         var Fehlergruppe: integer;
                         var Fehlercode: integer): boolean;
{----------------------------------------------------------}
{ überprüft eine KE-Antwort auf Richtigkeit;
  Übergabe/Rückgabe: Antwort auf KE-Befehl;
  Rückgabe: Antwortcode (Qnnn)
            Fehlergruppe, Fehlercode für Journal
  Ergebnis: true, wenn Antwort ok }
var
  P: integer;

begin
  Result:=false;
  { Antwort-String: alles vor dem STX ist Müll und muß weg }
  P:=Pos (STX, Antwort);
  Antwort:=Copy (Antwort, P, length(Antwort));

  AntwCode:=-1;
  if length (Antwort) >= 5 then begin
    try
      AntwCode:=StrToInt (Copy (Antwort, 3, 3));
    except
    end;

    case AntwCode of     { Antwort-Codes Qnnn auswerten, welche echte Fehler darstellen }
      kec_Befehl_falsche_Syntax: begin
                                   Fehlergruppe:=COM_KE_ERROR;
                                   Fehlercode:=KEERR_BEFEHLSYNTAX;
                                 end;
      kec_Befehl_nicht_erlaubt:  begin
                                   Fehlergruppe:=COM_KE_ERROR;
                                   Fehlercode:=KEERR_BEFEHL_NICHT_ERLAUBT;
                                 end;
    else
      { Antwort ist OK: }
      Fehlergruppe:=0;
      Fehlercode:=0;
      Result:=true;
    end;
  end
  else begin
    Fehlergruppe:=COM_KE_ERROR;
    Fehlercode:=KEERR_ANTWORT_UNVOLLSTAENDIG;
  end;
end;

{----------------------------------------------------------}
function ValidFTLAntwort (FunktionNr_Soll: string;
                          Antwort: string;
                          var Fehlergruppe: integer;
                          var Fehlercode: integer): boolean;
{----------------------------------------------------------}
{ überprüft eine FTL-Antwort auf Richtigkeit;
  Übergabe: in Antwort erwartete Funktionsnummer (leer: keine Soll/Ist-Prüfung)
            Antwort
  Rückgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn Antwort OK (positive Quittung oder Datenblock mit erwarteter
                                   Funktionsnummer) }
var
  AntwBuf: string;
  FunktionNr_Antw: string;

begin
  Result:=false;
  Fehlergruppe:=COM_FTL_ERROR;

  AntwBuf:=Copy (Antwort, 1, 3);
  { auf negative Quittung '?nn' prüfen: }
  if Pos ('?', AntwBuf) = 1 then begin  { negative Quittung '?nn' }
    if AntwBuf = '?01' then
      Fehlercode:=FTLERR_EMPFANGSPUFFERUEBERLAUF
    else if AntwBuf = '?02' then
      Fehlercode:=FTLERR_NICHTEMPFANGSBEREIT_TIMEOUT
    else if AntwBuf = '?04' then
      Fehlercode:=FTLERR_STARTSTOPBIT
    else if AntwBuf = '?06' then
      Fehlercode:=FTLERR_BLOCKSICHERUNG
    else if AntwBuf = '?08' then
      Fehlercode:=FTLERR_KOMMANDONICHTAUSFUEHRBAR_FALSCHEFKTNR
    else if AntwBuf = '?09' then
      Fehlercode:=FTLERR_KEINEDATENVORHANDEN
    else if AntwBuf = '?16' then
      Fehlercode:=FTLERR_UNBEKZEICHEN_KOMMANDO
    else if AntwBuf = '?17' then
      Fehlercode:=FTLERR_PARAMETERUNZULAESSIG
    else
      Fehlercode:=FTLERR_QUITTUNGUNDEFINIERT;
  end
  else begin
    if Pos ('*', AntwBuf) <> 1 then begin  { keine positive Quittung '*' oder '*00' -> Daten }
      if length (FunktionNr_Soll) > 0 then begin
        { Soll/Ist-Vergleich der Funktionsnummer, wenn K85-Block (Funktionsnummerneintrag)
          in Antwortdaten enthalten ist: }
        if Copy (Antwort, 1, 3) = 'K85' then begin  // wenn K85-Block vorhanden ist, steht er immer am Anfang
          FunktionNr_Antw:=Copy (Antwort, 5, 4);
          if FunktionNr_Antw <> FunktionNr_Soll then begin
            Fehlercode:=FTLERR_ANTWORTUNERWARTET;
            exit;
          end;
        end;
      end;
    end;

    { OK ! }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Result:=true;
  end;
end;

{-------------------------------------------------------------------------}
function ValidFTL_internAntwort (sBlockNr_Befehl: string; sAntwort: string;
                                 var Fehlergruppe: integer;
                                 var Fehlercode: integer): boolean;
{-------------------------------------------------------------------------}
{ überprüft eine Antwort mit internem FTL-Protokoll auf Richtigkeit;
  Übergabe: Blocknummer im zugehörigen Befehl
            Antwort
  Rückgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn Antwort OK ("positive" Blocknummer) }
var
  sBlockNr: string;
  bIsOK: boolean;

begin
  sBlockNr:=Copy (sAntwort, 1, 3);
  { auf "positive" Blocknummer prüfen: }
  if sBlockNr_Befehl = '002' then  // Befehl für Codewort-Freigabe
    bIsOK:=sBlockNr = '820'
  else  // übrige Befehle
    bIsOK:=
      (sBlockNr = '800') OR  // OK bei Lese-Befehl und manchen Schreib-Befehlen (z.B. Zählerstände setzen)
      (sBlockNr = '809') OR  // OK bei manchen Schreib-Befehlen (z.B. Gasanalysedaten setzen)
      // OK bei z.B. Blocknummern 007, 008 (VC2 Meldeliste, Meldestatistik löschen)
      ((sBlockNr = sBlockNr_Befehl) AND (Copy (sAntwort, 4, 2) = 'OK'));  // 03.11.2020, WW

  if bIsOK then begin
    Fehlergruppe:=0;
    Fehlercode:=0;
    Result:=true;
  end
  else begin
    Result:=false;
    Fehlergruppe:=COM_FTLINTERN_ERROR;
    { auf "negative" Blocknummer prüfen: }
    if sBlockNr = '802' then
      Fehlercode:=FTLINTERNERR_BLOCKZUKURZ
    else if sBlockNr = '803' then
      Fehlercode:=FTLINTERNERR_BLOCKNRKEINEZAHL
    else if sBlockNr = '804' then
      Fehlercode:=FTLINTERNERR_BLOCKFOLGEMARKEUNGUELTIG
    else if sBlockNr = '805' then
      Fehlercode:=FTLINTERNERR_CRCKEINEZAHL
    else if sBlockNr = '806' then
      Fehlercode:=FTLINTERNERR_CRCFALSCH
    else if sBlockNr = '807' then
      Fehlercode:=FTLINTERNERR_TESTERGEBNISSCHLECHT
    else if sBlockNr = '810' then
      Fehlercode:=FTLINTERNERR_BLOCKNRUNBEKANNT
    else if sBlockNr = '811' then
      Fehlercode:=FTLINTERNERR_PARAMETERNICHTFLOAT
    else if sBlockNr = '812' then
      Fehlercode:=FTLINTERNERR_PARAMETERNICHTINTEGER
    else if sBlockNr = '813' then
      Fehlercode:=FTLINTERNERR_PARAMETERNICHTLONGINTEGER
    else if sBlockNr = '814' then
      Fehlercode:=FTLINTERNERR_PARAMETERNICHTCHAR
    else if sBlockNr = '815' then
      Fehlercode:=FTLINTERNERR_PARAMETERAUSSERHALBGRENZWERTE
    else if sBlockNr = '816' then
      Fehlercode:=FTLINTERNERR_BLOCKGESPERRT
    else if sBlockNr = '817' then
      Fehlercode:=FTLINTERNERR_BLOCKKANNNICHTGESCHRIEBENWERDEN
    else if sBlockNr = '819' then
      Fehlercode:=FTLINTERNERR_FALSCHEBLOCKLAENGE
    else
      Fehlercode:=FTLINTERNERR_BLOCKNRUNDEFINIERT;
  end;
end;

{---------------------------------------------------------------------------------}
function ValidTritschler_IECAntwort_20_ZeitSync (Antwort: string;
                                                 var Fehlergruppe: integer;
                                                 var Fehlercode: integer;
                                                 var dtGeraeteZeit: TDateTime): boolean;
{---------------------------------------------------------------------------------}
{ wertet den Synchronisations-Status in einer Antwort auf einen Tritschler
  IEC-Zeitsynchronisations-Befehl aus und liefert ZeitSync-Fehlergruppe/-code;
  Übergabe: Antwort
  Rückgabe: Fehlergruppe
            Fehlercode
            Gerätezeit vor der Synchronisation bzw. aktuelle Gerätezeit, wenn
              keine Synchronisation erfolgte (0, wenn Info nicht vorhanden)
  Ergebnis: true, wenn Zeitsynchronisation durchgeführt wurde }
var
  sAntwort: string;
  Kennziffer_StatusSync: string;
  Kennziffer_ZeitNachSync: string;
  sStatusSync: string;
  sSecSync: string;
  sZeitNachSync: string;
  S, S1: string;
  iSecSync: integer;
  iCode: integer;
  dtBuf: TDateTime;
  dtGerZeitBuf: TDateTime;

begin
  Result:=false;
  Fehlergruppe:=EST_ZEITSYNCERROR;
  dtGeraeteZeit:=0;  // Vorbelegung: Gerätezeit nicht vorhanden

  sAntwort:=ExtractString (Antwort, STX, ETX, 0);  // Rohdatenteil zwischen STX und ETX
  Kennziffer_StatusSync:='0.3';   { Kennziffer für Status der Synchronisation }
  Kennziffer_ZeitNachSync:='30.';   { Kennziffer für UTC-Gerätezeit nach der Synchronisation }

  sStatusSync:='';
  sSecSync:='';
  sZeitNachSync:='';
  while length (sAntwort) > 0 do begin
    S:=F_Zerlegen (sAntwort, LF);
    if length (S) > 0 then begin
      if Copy (S, 1, length (Kennziffer_StatusSync)) =
         Kennziffer_StatusSync then begin  { Status der Synchronisation gefunden }
        S:=ExtractString (S, '(', ')', 0);  { Status der Synchronisation steht zwischen den runden Klammern }
        // optional zusätzlich mit Verstellung in Sekunden (nach dem Semikolon); 28.06.2018, WW
        sStatusSync:=ExtractString (S, NUL, ';', 0);
        sSecSync:=ExtractString (S, ';', ';', 0);
      end

      else if Copy (S, 1, length (Kennziffer_ZeitNachSync)) =
         Kennziffer_ZeitNachSync then begin  { UTC-Gerätezeit nach der Synchronisation gefunden }
        sZeitNachSync:=ExtractString (S, '(', ')', 0);  { Zeit steht zwischen den runden Klammern }
      end;
    end;
  end;  { while length (sAntwort) }
  
  if sStatusSync = '00' then begin
    Fehlercode:=ZSYNCERR_SUCCESS;
    Result:=true;
  end
  else if sStatusSync = '01' then
    Fehlercode:=ZSYNCERR_VERZOEGERT
  else if sStatusSync = '02' then
    Fehlercode:=ZSYNCERR_NICHTDURCHGEFUEHRT
  else if sStatusSync = '03' then
    Fehlercode:=ZSYNCERR_SYNTAXFEHLER
  else if sStatusSync = '04' then
    Fehlercode:=ZSYNCERR_GERAETBUSY
  else if sStatusSync = '05' then
    Fehlercode:=ZSYNCERR_BEREITSSYNCHRON
  else if sStatusSync = '06' then
    Fehlercode:=ZSYNCERR_SICHERUNGFALSCH
  else if sStatusSync = '07' then
    Fehlercode:=ZSYNCERR_VERSTELLWEITEGEKAPPT
  else if sStatusSync = '08' then
    Fehlercode:=ZSYNCERR_WIEDERHOLSPERRE
  else if sStatusSync = '99' then
    Fehlercode:=ZSYNCERR_SONSTIG
  else
    Fehlercode:=ZSYNCERR_UNDEFINIERT;

  // UTC-Gerätezeit auswerten; 28.06.2018, WW
  // -> Gerätezeit VOR der Synchronisation rückrechnen, wenn Information über
  //    Verstell-Sekunden vorhanden und gültig ist
  if  (sZeitNachSync <> '') then begin
    // Gerätezeit nach der Synchronisation:
    S1:=Copy (sZeitNachSync, 1, 6);  { Datum }
    if EncodeDateStr (S1, 'YYMMDD', dtBuf) then begin
      dtGerZeitBuf:=dtBuf;

      S1:=Copy (sZeitNachSync, 7, 6);  { Zeit }
      if EncodeTimeStr (S1, 'HHMMSS', dtBuf) then begin
        dtGerZeitBuf:=dtGerZeitBuf + dtBuf;

        S1:=Copy (sZeitNachSync, 13, 3);  { UTC-Zeitzonen-Differenz }
        if UTCTimezoneStrToDateTime (S1, dtBuf) then begin
          // Aktuelle Gerätezeit nach bzw. ohne Synchronisation:
          dtGeraeteZeit:=dtGerZeitBuf + dtBuf;  // Rückgabe

          // Wenn Verstell-Sekunden vorhanden sind: Gerätezeit VOR der
          // Synchronisation rückrechnen
          if (sSecSync <> '') then begin
            Val (sSecSync, iSecSync, iCode);  // Verstell-Sekunden
            if iCode = 0 then
              dtGeraeteZeit:=IncSecond (dtGeraeteZeit, iSecSync);  // Rückgabe
          end;
        end;
      end;
    end;
  end;
end;

{---------------------------------------------------------------}
function ValidCorusSAMAntwort (var Antwort: string;
                               var Fehlergruppe: integer;
                               var Fehlercode: integer;
                               var ETX_Pos: integer): boolean;
{---------------------------------------------------------------}
{ überprüft eine Actaris Corus-Antwort auf einen Lesebefehl auf Richtigkeit;
  Übergabe: Antwort
  Rückgabe: Fehlergruppe
            Fehlercode
            Position des Endezeichens ETX in Antwort
  Ergebnis: true, wenn Antwort ok }
var
  iSize_Soll: byte;
  iPosETX_Soll: integer;
  bSizeOK: boolean;
  P: integer;

begin
  Result:=false;
  ETX_Pos:=-1;
  Fehlergruppe:=COM_MRGERROR;

  if Antwort = NAK then begin
    { NACK, Gerät kann nicht mehr als 255 Datenbytes in der Antwort zurückliefern }
    Fehlercode:=MRGERR_DATASIZE_EXCEEDED_NACK;
  end
  else begin
    { Antwort-String: alles vor dem SOH ist Müll und muß weg }
    P:=Pos (SOH, Antwort);
    Antwort:=Copy (Antwort, P, length(Antwort));
    if length (Antwort) > 1 then begin  { das Size-Byte muß mindestens enthalten sein }
      iSize_Soll:=Ord(Antwort[2]);
      iPosETX_Soll:=2 + iSize_Soll + 1;  // SOH, Size-Byte, Datenteil, ETX

      bSizeOK:=false;
      if length (Antwort) >= iPosETX_Soll then
        if (Antwort [iPosETX_Soll] = ETX) then
          bSizeOK:=true;

      if not bSizeOK then
        Fehlercode:=MRGERR_DATASIZE_SOLL_IST  { Soll-Datengröße ungleich Ist-Datengröße }
      else begin
        { Antwort ist OK: }
        Fehlergruppe:=0;
        Fehlercode:=0;
        ETX_Pos:=iPosETX_Soll;  // Position des Endezeichens ETX
        Result:=true;
      end;
    end else
      Fehlercode:=MRGERR_ANTWORTUNVOLLSTAENDIG;
  end;
end;

End.

