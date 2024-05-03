{------------------------------------------------------------------------------}
{ Objekt für DSfG-Busdaten-Überprüfung einer DSfG-Station                      }
{                                                                              }
{ 26.07.2000  GD    Neu                                                        }
{ 17.08.2001  GD    Umstellung auf Konstanten in WSysCon                       }
{ 17.01.2003  GD    Kein DEA definiert ist kein Fehler mehr                    }
{ 09.12.2004  GD    Erweitert um Excel-Zusammenfassung                         }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, 2004                                    }
{------------------------------------------------------------------------------}
unit DSGBusAnalyse;

interface

uses
  SysUtils, Classes, Dialogs, ComObj, Forms, Controls,
  DSfGBusDaten, DSG_Utils, GD_Utils, WSysCon, COM_Utils, DelListCom;

type
  TQRZuordnungsTyp = (qrzArchive, qrzLogbuecher, qrzArchivGrp, qrzArchivLogb);

  TAdrLogicErrorType = (tetPrgListError, tetInstMissingAdr, tetInstMissingDEA,
                        tetInstDoubleAdr, tetInstDoubleLogb, tetInstLogbDiffCRC,
                        tetNullCRC, tetQuellCRCNull);

  TDSfGBusAnalyse = class(TDSfGStation)
    constructor Create; override;
    destructor Destroy; override;
  private
    FTimeDiffList : TStrings;
    FArchivList   : TStrings;
  protected
  public
    procedure RegisterArchivStatus(
      cRAdr, cQAdr: char; sDEA, sUnixDatum: string; iCount: integer);
    function GetRegArchivData(cRAdr: char; sDEA: string;
      var cQAdr: char; var iCount: integer; var sUnixTime: string): boolean;
    function GetRegArchivState(cRAdr: char; sDEA: string; var cQAdr: char;
      var sONrVon, sOnrBis: string): boolean;
    function GetUmwSenkenDataList: TStringList;
    function CheckUmwerterSenken: TStringList;
    function GetRegArchivDataList(cRAdr: char): TStringList;
    procedure DeleteRegArchivData(cRAdr: char);
    procedure RegisterTimeDiff(cInstAdr: char);
    function GetTimeDiff(cInstAdr: char): integer;
    function GetRegisterAdressen(State: TQRZuordnungsTyp): TStringList;
    function GetRegisterQuellAdressen(
      cAdr: char; pSl: TStrings = nil; bFirmSpez: boolean = False): string;
    function CheckRegisterAdressen: TStringList;
    function GetRegArchiveONrVonBis: TStringList;
    function GetRegMaxONrVonBis(cRAdr: char): integer;
    function IsRegArchivData(cRAdr: char): boolean;
    procedure CreateExcelSummary;
  end;

function GetAktiveArchivGruppen(
  cInstAdr: char; pSl: TStringList; iState: byte = 1): TStrings;

implementation

{---------------------------- Allgemeine Funktion -----------------------------}

{ Aktive Archivgruppen ermitteln     }
{ Parameter: RInst., Liste mit Adr.  }
{            Status: 1-ONr, 2-Kanäle }
{ Rückgabe: Liste mit Archivgruppen  }
{------------------------------------}
function GetAktiveArchivGruppen(
  cInstAdr: char; pSl: TStringList; iState: byte = 1): TStrings;
{------------------------------------}
var
  c : char;
  i : integer;
  s : string;
begin
  pSl.Sorted := True;
  Result := TStringList.Create;
  c := '0';  // Archivgruppe
  for i := 0 to pSl.Count-1 do begin
    s := GetStringPart(pSl[i], 2); // DEA
    if (pSl[i][1] = cInstAdr) then begin
      if (s[2] = 'a') and (s[3] <> c) then begin
        c := s[3];
        if (iState = 1) then begin
          Result.Add(Copy(s, 1, 3) + 'c');  // ONr. von
          Result.Add(Copy(s, 1, 3) + 'd');  // ONr. bis
        end
        else if (iState = 2) then Result.Add(Copy(s, 1, 4) + 'd');   // 1. aktiver Kanalwert
      end
      else if (s[2] = 'b') then begin
        if (iState = 1) then begin
          Result.Add(Copy(s, 1, 4) + 'a');  // ONr. von
          Result.Add(Copy(s, 1, 4) + 'b');  // ONr. bis
        end
        else if (iState = 2) then Result.Add(Copy(s, 1, 4) + 'd');   // 1. aktiver Eintrag
      end
      else Continue;
    end;
  end;
end;

{------------------------------ TDSfGBusAnalyse -------------------------------}

{----------------------------------------------------}
constructor TDSfGBusAnalyse.Create;
{----------------------------------------------------}
begin
  inherited Create;
  FTimeDiffList := TStringList.Create;
  FArchivList := TStringList.Create;
end;

{----------------------------------------------------}
destructor TDSfGBusAnalyse.Destroy;
{----------------------------------------------------}
begin
  if (Assigned(FTimeDiffList)) then FTimeDiffList.Free;
  if (Assigned(FArchivList)) then FArchivList.Free;
  inherited Destroy;
end;

{ Liste mit Umwertern als Senken (Quelle PGC) zurück }
{ Rückgabe: TStrings mit <UmwInst>+<QInst>+<CRC>   }
{----------------------------------------------------}
function TDSfGBusAnalyse.GetUmwSenkenDataList: TStringList;
{----------------------------------------------------}
var
  i              : integer;
  s, sAdd, sWert : string;
  c3             : char;
begin
  Result := TStringList.Create;

  s := GetTeilnehmer;

  for i := 1 to Length(s) do
    if (s[i] in ['A'..'_']) then
      if (GetInstTyp(s[i]) = C_D_Instanztyp_Umw) then begin
        for c3 := 'a' to 'z' do begin
          sWert := Trim(GetDatenelement(s[i], 'bc'+c3+'aa'));
          if (Length(sWert) = 1) and (sWert[1] in ['A'..'_']) then begin
            sAdd := s[i] +
              sWert +  // Adresse
              GetDatenelement(s[i], 'bc'+c3+'ag');   // CRC
            if (Result.IndexOf(sAdd) < 0) then Result.Add(sAdd);
          end;
        end;
      end;

end;

{ Gibt die Senken-Quellen-Zuordnung f. Umw. zurück   }
{ Rückgabe: TStrings mit Fehlermeldungen             }
{   <UAdr>us<QAdr>us<UCRC>us<QCRC>us<ErrorCode>      }
{----------------------------------------------------}
function TDSfGBusAnalyse.CheckUmwerterSenken: TStringList;
{----------------------------------------------------}
var
  i : integer;
  s : string;
  cQAdr : char;
  sQCRC, sUCRC  : string;
  iQCRC, iUCRC  : integer;
begin
  Result := TStringList.Create;

  with GetUmwSenkenDataList do
  try
    for i := 0 to Count-1 do begin
      cQAdr := Strings[i][2];
      sUCRC := Copy(Strings[i], 3, Length(Strings[i])-2);
      sQCRC := GetDatenelement(cQAdr, C_DEA_CRCStWert);
      iUCRC := StrToIntDef(sUCRC, 0);
      iQCRC := StrToIntDef(sQCRC, 0);
      s := Strings[i][1] + Chr(us) + cQAdr + Chr(us) + sUCRC + Chr(us) + sQCRC;

      // Quelle vorhanden ?
      if (Pos(cQAdr, Self.GetTeilnehmer) = 0) then
        s := s + Chr(us) + IntToStr(Integer(tetInstMissingAdr))
      // CRCs gleich ?
      else if (iUCRC <> iQCRC) then
        s := s + Chr(us) + IntToStr(Integer(tetInstLogbDiffCRC))
      // CRCs ungleich 0
      else if (iUCRC = 0) or (iQCRC = 0) then
        s := s + Chr(us) + IntToStr(Integer(tetNullCRC))
      else if (StrToIntDef(GetDatenelement(s[1], C_DEA_CRCStWert), 0) = 0) then
        s := s + Chr(us) + IntToStr(Integer(tetQuellCRCNull))
      // Kein Fehler
      else Continue; 

      Result.Add(s);
    end;
  finally
    Free;
  end;
end;

{ Gibt die Registrierungs-Quellen-Zuordnung zurück   }
{ Parameter: Archive oder Logbücher                  }
{ Rückgabe: TStrings mit <RegInst>+<QInst>+<DEA>     }
{----------------------------------------------------}
function TDSfGBusAnalyse.GetRegisterAdressen(
  State: TQRZuordnungsTyp): TStringList;
{----------------------------------------------------}
var
  cQuellInst, c3, c4, cRegInstAdr : char;
  pInstanz : TDSfGInstanz;
  s, sDea   : string;
begin
  Result := TStringList.Create;
//  Result.Sorted := True;
  for cRegInstAdr := 'A' to '_' do
    if (Self.HasDatenelement(cRegInstAdr, 'aaa')) then begin

      pInstanz := GetInstanz(cRegInstAdr);

      if (State = qrzArchive) then begin  // DEA: DEL-Adresse in Quellinstanz
        for c3 := 'a' to 'z' do
          if (pInstanz.HasDatenelement('ca' + c3 + 'a')) then
            for c4 := 'f' to 'z' do
              if (pInstanz.HasDatenelement('ca'+ c3 + c4 + 'b')) then begin
                s := pInstanz.GetWert('ca'+ c3 + c4 + 'b');
                if (Length(s) > 0) and (s[1] in ['A'..'_']) then begin
                  sDea := Trim(pInstanz.GetWert('ca'+ c3 + c4 + 'c'));
                  if (sDea <> '') then Result.Add(cRegInstAdr + s[1] + sDea);
                end;
              end;
      end

      else if (State = qrzLogbuecher) then begin  // DEA: CRC-Startwert der Quelle
        for c3 := 'a' to 'd' do
          for c4 := 'a' to 'j' do
            if (pInstanz.HasDatenelement('cb' + c3 + c4 + 'e')) then begin
              s := pInstanz.GetWert('cb'+ c3 + c4 + 'e');
              cQuellInst :=  // Quelladresse ist in DEA verschlüsselt
                Chr(Ord('A') + (Ord(c3)-Ord('a'))*10 + (Ord(c4)-Ord('a')));
//              if (Length(s) > 0) and (StrToIntDef(s, -9999) <> -9999) then
                Result.Add(cRegInstAdr + cQuellInst + s);
            end;
      end

      else if (State = qrzArchivGrp) then begin  // DEA: Archivkanal
        for c3 := 'a' to 'z' do
          if (pInstanz.HasDatenelement('ca' + c3 + 'a')) then // Kennung
            for c4 := 'f' to 'z' do
              if (pInstanz.HasDatenelement('ca'+ c3 + c4 + 'b')) then begin
                s := pInstanz.GetWert('ca'+ c3 + c4 + 'b');
                if (Length(s) > 0) and (s[1] in ['A'..'_']) then begin
                  sDea := Trim(pInstanz.GetWert('ca'+ c3 + c4 + 'c'));
                  if (sDea <> '') then Result.Add(cRegInstAdr + s[1] + ('ca'+ c3 + c4));
                end;
              end;
      end

      else if (State = qrzArchivLogb) then begin  // DEA: Logbuch-Quellinstanzadresse
        for c3 := 'a' to 'd' do
          for c4 := 'a' to 'j' do
            if (pInstanz.HasDatenelement('cb' + c3 + c4 + 'e')) then begin
              cQuellInst :=  // Quelladresse ist in DEA verschlüsselt
                Chr(Ord('A') + (Ord(c3)-Ord('a'))*10 + (Ord(c4)-Ord('a')));
              Result.Add(cRegInstAdr + cQuellInst + ('cb' + c3 + c4));
            end;
      end;

    end;
end;

{ Prüft die Registrierungs-Quellen-Zuordnung zurück  }
{ Parameter: RegInstAdr oder ' ' (alle)              }
{ Rückgabe: string mit Quelladressen                 }
{----------------------------------------------------}
function TDSfGBusAnalyse.GetRegisterQuellAdressen(
  cAdr: char; pSl: TStrings = nil; bFirmSpez: boolean = False): string;
{----------------------------------------------------}
var
  i : integer;
begin
  Result := '';

  if (Assigned(pSl)) then with pSl do begin
    for i := 0 to Count-1 do
    // Format: <RegInst>+<QInst>+<DEA>
      if ((cAdr = ' ') or (cAdr= Strings[i][1])) and
         ((bFirmSpez) or
          (Self.GetInstTyp(Strings[i][2]) in
            [C_D_Instanztyp_Gas, C_D_Instanztyp_Umw]))
      then
        if (Pos(Strings[i][2], Result) = 0) then
          Result := Result + Strings[i][2];
  end

  else with (GetRegisterAdressen(qrzArchivGrp)) do
  try
    for i := 0 to Count-1 do
    // Format: <RegInst>+<QInst>+<DEA>
      if ((cAdr = ' ') or (cAdr= Strings[i][1])) and
         ((bFirmSpez) or (Self.GetInstTyp(Strings[i][2]) in
           [C_D_Instanztyp_Gas, C_D_Instanztyp_Umw]))
      then
        if (Pos(Strings[i][2], Result) = 0) then
          Result := Result + Strings[i][2];
  finally
    Free;
  end;
end;

{ Gibt die Registrierungs-Quellen-Zuordnung zurück   }
{ Rückgabe: TStrings mit Fehlermeldungen             }
{   RAdr+<us+QAdr+<us>+RDEA+<us>+QDEA+<us>+ErrorCode }
{----------------------------------------------------}
function TDSfGBusAnalyse.CheckRegisterAdressen: TStringList;
{----------------------------------------------------}
var
  pSl : TStringList;

  procedure SetError(
    pSLError: TStrings; ErrorType: TAdrLogicErrorType; sDEA: string);
  var
    sRDEA, sQDEA : string;
  begin
    sRDEA := Copy(sDEA, 3, Length(sDEA)-2);
    if (sRDEA[2] = 'a')
    then sQDEA := GetDatenelement(sDEA[1], sRDEA+'c')  // Archiv -> DEA
    else sQDEA := GetDatenelement(sDEA[1], sRDEA+'e'); // Logbuch -> CRC
    pSLError.Add(sDEA[1] + Chr(us) + sDEA[2] + Chr(us) + sRDEA + Chr(us) +
       sQDEA + Chr(us) + IntToStr(Integer(ErrorType)));
  end;

  procedure CheckForErrors(pSLError, pSlData: TStrings; State: TQRZuordnungsTyp);
  var
    pSlPruef : TStringList;
    s   : string;
    i   : integer;
  begin
    pSlPruef := TStringList.Create;
    try
      for i := 0 to pSlData.Count-1 do begin
  // Test auf sinnvollen Eintrag in Zuordnungsliste (Programmfehler)
        if (Length(pSlData[i]) <> 6)
        then SetError(pSLError, tetPrgListError, pSlData[i])
        else begin
  // Quell-DEA extrahieren
          if (State = qrzArchivGrp) then begin
            s := Copy(pSlData[i], 3, Length(pSlData[i])-2) + 'c';
            s := GetDatenelement(pSlData[i][1], s);
          end
  // Quell-CRC extrahieren
          else if (State = qrzArchivLogb) then begin
            s := Copy(pSlData[i], 3, Length(pSlData[i])-2) + 'e';
            s := GetDatenelement(pSlData[i][1], s);
          end;
  // Prüfung auf vorhandene Quellinstanz
          if (not HasInstanz(pSlData[i][2]))
          then SetError(pSLError, tetInstMissingAdr, pSlData[i])
  // Prüfung auf vorhandene Datenelementadresse
          else if (State = qrzArchivGrp) and (Trim(s) <> '') and
                  (not HasDatenelement(pSlData[i][2], s)) // 17.01.2003
          then SetError(pSLError, tetInstMissingDEA, pSlData[i])
  // Prüfung auf übereinstimmendes CRC
          else if (State = qrzArchivLogb) and
            (GetDatenelement(pSlData[i][2], C_DEA_CRCStWert) <> s)
          then SetError(pSLError, tetInstLogbDiffCRC, pSlData[i])
  // Prüfung auf doppelt vorhandene Datenelementadresse/CRC
          else begin
            s := pSlData[i][2] + s;
            if (pSlPruef.IndexOf(s) >= 0)
            then begin
              if (State = qrzArchivGrp) then
                SetError(pSLError, tetInstDoubleAdr, pSlData[i])
              else if (State = qrzArchivLogb) then
                SetError(pSLError, tetInstDoubleLogb, pSlData[i]);
            end
            else pSlPruef.Add(s);
          end;
        end;
      end;
    finally
      pSlPruef.Free;
    end;
  end;

begin
  Result := TStringList.Create;
  // Prüfung der Archive
  pSl := Self.GetRegisterAdressen(qrzArchivGrp);
  try
    CheckForErrors(Result, pSl, qrzArchivGrp);
  finally
    pSl.Free;
  end;
  // Prüfung der Logbücher
  pSl := Self.GetRegisterAdressen(qrzArchivLogb);
  try
    CheckForErrors(Result, pSl, qrzArchivLogb);
  finally
    pSl.Free;
  end;
end;

{ Speichert Eckdaten einer Archivgruppe              }
{ Parameter: RegAdr, QAdr, DEA, 1. Datum, Füllst.    }
{----------------------------------------------------}
procedure TDSfGBusAnalyse.RegisterArchivStatus(
  cRAdr, cQAdr: char; sDEA, sUnixDatum: string; iCount: integer);
{----------------------------------------------------}
var
  i : integer;
  s : string;
begin
  // Format ist <RegAdr>+<US>+<QuellAdr>+<US>+<DEA>+<US>+<Füllst>+<US>+<Datum>
  s := cRAdr + Chr(us) + cQAdr + Chr(us) + sDEA + Chr(us);

  // Eintrag zu der Instanz suchen und überschreiben
  for i := 0 to FArchivList.Count-1 do
    if (Copy(FArchivList[i], 1, Length(s)) = s) then begin
      FArchivList[i] := s + IntToStr(iCount) + Chr(us) + sUnixDatum;
      Exit;
    end;

  // Wenn man hier ankommt, gibt es noch keinen Eintrag
  FArchivList.Add(s + IntToStr(iCount) + Chr(us) + sUnixDatum);
end;

{ Gibt Archivgruppen-Füllstände zurück               }
{ Parameter: Registrierinstanz, DEA zum Archiv       }
{ Var-Parameter: QuellInstAdr, 1. OrdNr., l. OrdNr.  }
{ Rückgabe: Daten vorhanden Ja/Nein                  }
{----------------------------------------------------}
function TDSfGBusAnalyse.GetRegArchivState(cRAdr: char; sDEA: string;
  var cQAdr: char; var sONrVon, sOnrBis: string): boolean;
{----------------------------------------------------}
var
  sDEAONrVon, sDEAONrBis : string;
  s                      : string;
  c                      : char;
begin
  Result := False;
  if (Length(sDEA) < 3) then Exit;

  if (sDEA[2] = 'a') then begin
    sDEAONrVon := Copy(sDEA, 1, 3) + 'c';
    sDEAONrBis := Copy(sDEA, 1, 3) + 'd';
  end
  else if (sDEA[2] = 'b') and (Length(sDEA) >= 4) then begin
    sDEAONrVon := Copy(sDEA, 1, 4) + 'a';
    sDEAONrBis := Copy(sDEA, 1, 4) + 'b';
  end
  else Exit;

  if (not HasDatenelement(cRAdr, sDEAONrVon)) or
     (not HasDatenelement(cRAdr, sDEAONrBis)) then Exit;
  sONrVon := GetDatenelement(cRAdr, sDEAONrVon);
  sOnrBis := GetDatenelement(cRAdr, sDEAONrBis);

  cQAdr := '0';
  if (sDEA[2] = 'a') then
    for c := 'f' to 'z' do begin
      s := Copy(sDEA, 1, 3) + c + 'b';  // Adresse der Quellinstanz
      s := GetDatenelement(cRAdr, s);
      if (s <> '') then begin
        cQAdr := s[1];
        Break;
      end;
    end
  else if (sDEA[2] = 'b') then cQAdr := Chr(Ord('A') - 1 +
    (10*(Ord(sDEA[3]) - Ord('a'))) + (Ord(sDEA[4]) - Ord('a') + 1));

  Result := True;
end;

{ Gibt Archivgruppen-Eckdaten zurück                 }
{ Parameter: Registrierinstanz, DEA zum Archiv       }
{ Var-Parameter: QuellInst, Füllstand, 1. Zeitpunkt  }
{ Rückgabe: Daten vorhanden Ja/Nein                  }
{----------------------------------------------------}
function TDSfGBusAnalyse.GetRegArchivData(cRAdr: char; sDEA: string;
  var cQAdr: char; var iCount: integer; var sUnixTime: string): boolean;
{----------------------------------------------------}
var
  i : integer;
begin
  Result := False;

  for i := 0 to FArchivList.Count-1 do
    if (GetStringPart(FArchivList[i], 1) = cRAdr) and
      (GetStringPart(FArchivList[i], 3) = sDEA) then
    begin
      cQAdr := GetStringPart(FArchivList[i], 2)[1];
      iCount := StrToInt(GetStringPart(FArchivList[i], 4));
      sUnixTime := GetStringPart(FArchivList[i], 5);
      Result := True;
      Exit;
    end
end;

{ Gibt Liste mit Archivgruppen-Eckdaten zurück       }
{ Parameter: Registrierinstanz oder ' '              }
{ Rückgabe: TStrings                                 }
{----------------------------------------------------}
function TDSfGBusAnalyse.GetRegArchivDataList(cRAdr: char): TStringList;
{----------------------------------------------------}
var
  i : integer;
begin
  Result := TStringList.Create;

  if (cRAdr = ' ')
  then Result.Assign(FArchivList)
  else for i := 0 to FArchivList.Count-1 do
    if (GetStringPart(FArchivList[i], 1) = cRAdr) then
      Result.Add(FArchivList[i]);
end;

{ Löscht Einträge aus Liste mit Archivgruppen-Daten  }
{ Parameter: Registrierinstanz oder ' ' (alle)       }
{----------------------------------------------------}
procedure TDSfGBusAnalyse.DeleteRegArchivData(cRAdr: char);
{----------------------------------------------------}
var
  i : integer;
begin
  if (cRAdr = ' ')
   then FArchivList.Clear
   else for i := FArchivList.Count-1 downto 0 do
     if (GetStringPart(FArchivList[i], 1) = cRAdr) then FArchivList.Delete(i);
end;

{ Bildet Differenz zwischen PC-Zeit und aca-Zeit     }
{ Parameter: Instanzadresse                          }
{----------------------------------------------------}
procedure TDSfGBusAnalyse.RegisterTimeDiff(cInstAdr: char);
{----------------------------------------------------}
var
  sInstUnixTime  : string;
  dtNow          : TDateTime;
  s              : string;
  i, iVorzeichen : integer;
  l, lNow, lUnix : LongWord;
begin
  dtNow := Now;
  sInstUnixTime := GetDatenelement(cInstAdr, C_DEA_DatumZeit);
  if (sInstUnixTime <> '') then begin
    Val('$' + sInstUnixTime, lUnix, i);
    if (i <> 0) then Exit;
    lNow := DateTimeToUnix(dtNow);
    if (lNow > lUnix) then begin
      l := lNow - lUnix;
      iVorzeichen := -1;
    end
    else begin
      l := lUnix - lNow;
      iVorzeichen := 1;
    end;
    if (l <= LongWord(High(Integer))) then i := iVorzeichen*Integer(l) else Exit;

  // Format ist <InstAdr>+<Differnz in sec>
    s := cInstAdr + IntToStr(i);

  // Eintrag zu der Instanz suchen und überschreiben
    for i := 0 to FTimeDiffList.Count-1 do
      if (FTimeDiffList[i][1] = cInstAdr) then begin
        FTimeDiffList[i] := s;
        Exit;
      end;

  // Wenn man hier ankommt, gibt es noch keinen Eintrag
    FTimeDiffList.Add(s);
  end;
end;

{ Zeitdifferenz zurückgeben                          }
{ Rückgabe: Differenz Instanz und PC-Zeit in sec     }
{----------------------------------------------------}
function TDSfGBusAnalyse.GetTimeDiff(cInstAdr: char): integer;
{----------------------------------------------------}
var
  i : integer;
begin
  Result := -9999;  // default;

  for i := 0 to FTimeDiffList.Count-1 do
    if (FTimeDiffList[i][1] = cInstAdr) then begin
      Result := StrToIntDef(
        Copy(FTimeDiffList[i], 2, Length(FTimeDiffList[i])-1), -9999);
      Break;
    end;
end;

{ Gibt die Füllst. der Archive in den RegInst. zur.  }
{ Rückgabe: TStrings (IAdr<us>AvDEA<us>OVon<us>OBis) }
{----------------------------------------------------}
function TDSfGBusAnalyse.GetRegArchiveONrVonBis: TStringList;
{----------------------------------------------------}
var
  pSl         : TStrings;
  cRegInstAdr : char;
  sDEA        : string;
  i           : integer;
  sONrVon, sONrBis : string;
begin
  Result := TStringList.Create;
  // Prüfung der Archive
  pSl := Self.GetRegisterAdressen(qrzArchivGrp); // <RegInst>+<QInst>+<DEA>
  try
    for i := 0 to pSl.Count-1 do
      if (Length(pSl[i]) <> 6)
      then Continue
      else begin
        cRegInstAdr := pSl[i][1];
        sDEA := Copy(pSl[i], 3, 4) + 'd';
        sONrVon := Self.GetDatenelement(cRegInstAdr, Copy(sDEA, 1, 3) + 'c');
        sONrBis := Self.GetDatenelement(cRegInstAdr, Copy(sDEA, 1, 3) + 'd');
        if (sONrVon = '') or (sONrVon = '')
        then Continue
        else Result.Add(cRegInstAdr + Chr(us) + sDEA + Chr(us) + sONrVon +
          Chr(us) + sONrBis);
      end;
  finally
    pSl.Free;
  end;
  // Prüfung der Logbücher
  pSl := Self.GetRegisterAdressen(qrzArchivLogb); // <RegInst>+<QInst>+<DEA>
  try
    for i := 0 to pSl.Count-1 do
      if (Length(pSl[i]) <> 6)
      then Continue
      else begin
        cRegInstAdr := pSl[i][1];
        sDEA := Copy(pSl[i], 3, 4) + 'd';
        sONrVon := Self.GetDatenelement(cRegInstAdr, Copy(sDEA, 1, 4) + 'a');
        sONrBis := Self.GetDatenelement(cRegInstAdr, Copy(sDEA, 1, 4) + 'b');
        if (sONrVon = '') or (sONrVon = '')
        then Continue
        else Result.Add(cRegInstAdr + Chr(us) + sDEA + Chr(us) + sONrVon +
          Chr(us) + sONrBis);
      end;
  finally
    pSl.Free;
  end;
end;

{ Gibt die max. ONr.-Diff in Registrierinstanz aus   }
{ Parameter: Adresse der Registrierinstanz           }
{ Rückgabe: Max. ONr.-Diff                           }
{----------------------------------------------------}
function TDSfGBusAnalyse.GetRegMaxONrVonBis(cRAdr: char): integer;
{----------------------------------------------------}
var
  i, iCount : integer;
  cAg       : char;
begin
  with GetRegArchiveONrVonBis do
  try
    Sorted := True;
    cAg := '0';
    Result := 0;

  // Format ist IAdr<us>AvDEA<us>OVon<us>OBis
    for i := 0 to Count-1 do
      if (GetStringPart(Strings[i], 2)[3] = cAG) or (Strings[i][1] <> cRAdr)
      then Continue
      else begin
        cAg := GetStringPart(Strings[i], 2)[3];
        iCount := StrToIntDef(GetStringPart(Strings[i], 4), 0) -
          StrToIntDef(GetStringPart(Strings[i], 3), 0);
        if (iCount > Result) then Result := iCount;
      end;
  finally
    Free;
  end;
end;

{ Gibt es Archiv-Eck-Daten zu der Registrierinstanz? }
{ Parameter: Adresse der Registrierinstanz           }
{ Rückgabe: Daten vorhanden Ja/Nein                  }
{----------------------------------------------------}
function TDSfGBusAnalyse.IsRegArchivData(cRAdr: char): boolean;
{----------------------------------------------------}
begin
  with GetRegArchivDataList(cRAdr) do
  try
    Result := (Count > 0);
  finally
    Free;
  end;
end;

{ Erzeugt eine zusammenfassende Excel-Ausgabe        }
{----------------------------------------------------}
procedure TDSfGBusAnalyse.CreateExcelSummary;
{----------------------------------------------------}

  function GetInstTypName(sInstTyp: string): string;
  begin
    if (sInstTyp <> '') then begin
      case sInstTyp[1] of
        C_D_Instanztyp_DFU    : Result := C_D_Instanzname_DFU;
        C_D_Instanztyp_Gas    : Result := C_D_Instanzname_Gas;
        C_D_Instanztyp_Rev    : Result := C_D_Instanzname_Rev;
        C_D_Instanztyp_Odor   : Result := C_D_Instanzname_Odor;
        C_D_Instanztyp_KGM    : Result := C_D_Instanzname_KGM;
        C_D_Instanztyp_Prot   : Result := C_D_Instanzname_Prot;
        C_D_Instanztyp_Reg    : Result := C_D_Instanzname_Reg;
        C_D_Instanztyp_Strg   : Result := C_D_Instanzname_Strg;
        C_D_Instanztyp_Umw    : Result := C_D_Instanzname_Umw;
        C_D_Instanztyp_Wieser : Result := C_D_Instanzname_Wieser;
        else Result := C_D_Instanzname_unbest;
      end;
    end
    else Result := C_D_Instanzname_unbest;
  end;

var
//  vExcel, vWorkBook     : Variant;
  i                     : integer;
  sDea, sWert, sName    : string;
  c                     : char;
  bFirst                : boolean;
  pInstanz              : TDSfGInstanz;
  pDatenListe, pSlExcel : TStrings;
begin
  try
    bFirst := True;
    pSlExcel := TStringList.Create;
    Screen.Cursor := crHourGlass;
    try
      for c := 'A' to '_' do begin
        if (Pos(c, GetTeilnehmer) > 0) then begin
          pInstanz := GetInstanz(c);
          if (Assigned(pInstanz)) then begin
            pSlExcel.Clear;
            pSlExcel.Add(
              GetInstTypName(pInstanz.InstTyp) + ' [' + pInstanz.InstAdr + ']');
            pSlExcel.Add(#9 +
              GetInstTypName(pInstanz.InstTyp) + ' [' + pInstanz.InstAdr + ']');
            pSlExcel.Add('');
            pSlExcel.Add('DE-Adresse'#9'Bezeichnung'#9'Wert');
            pDatenListe := pInstanz.DatenListe;
            for i := 0 to pDatenListe.Count - 1 do begin
              sDea := pDatenListe[i];
              if (sDea = '_aca')
              then sName := 'Zeitpunkt der Abfrage'
              else sName := GetDeaBezeichnung(sDea, 0);
              sWert := PStr40(pDatenListe.Objects[i])^;
              if (GetDeaDefRecordByDea(sDea, 0).DEATyp = '7') and
                 (Length(sWert) > 0) and (StrToIntDef('$'+sWert, -1) >= 0)
              then sWert := DateTimeToStr(UnixToDateTime(StrToInt('$' + sWert)));

              pSlExcel.Add(sDea + #9 + sName + #9 + sWert);
            end;
            InsertToExcel(pSlExcel, #9, 1, 1, bFirst, True, True);
            bFirst := False;
          end;
        end;
      end;
    finally
      Screen.Cursor := crDefault;
      pSlExcel.Free;
    end;
  except
    on E:Exception do MessageDlg(E.Message, mtError, [mbOk], 0);
  end;
end;

end.
