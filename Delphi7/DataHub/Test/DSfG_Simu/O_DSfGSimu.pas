//------------------------------------------------------------------------------
// Klasse für simuliertes DSfG-Archiv/Logbuch
//
// 19.04.2013  GD  Neu
// 14.01.2015  WW  optional mit Random-Intervall; Bugfix Logbuch-Füllstand
//                 liefern für DEA
// 21.04.2015  WW  Log-Pfad optional übergeben
//
// Copyright (C) RMG Messtechnik GmbH 2013, 2015
//------------------------------------------------------------------------------
unit O_DSfGSimu;

interface

uses
  Windows, Messages, SysUtils, Classes, GD_Utils, Novell;

const
  C_DSfGSimuFileArchive = 'DSfGArchSimu.DAT';
  C_DSfGSimuFileLogbook = 'DSfGLogbSimu.DAT';

type
  TSimuFile = class(TObject)
    constructor Create(const sFileName: TFileName;
      const sLogInfo_ThreadIdent: string = ''; const sLogPath: string = '');
    destructor Destroy; override;
  private
    FFileName  : TFileName;
    FLUnixTime : TList;
    FLOrdinal  : TList;
    FPosition  : integer;
    FFileAge   : integer;
    FIndex     : integer;
    FLogInfo_ThreadIdent: string;  // für Multi-Threading-Anwendungen
    FLogPath: string;
  protected
  public
    function LoadFromFile: boolean;
    function SaveToFile: boolean;
    function GetLastTimeAndONo(var iUnixTime, iONo: Longword): boolean;
    function AddRecordsFrom(dtFrom: TDateTime; iInterval: integer;
      bRandomInterval: boolean = false): integer;
    function AddRecordsTo(dtTo: TDateTime; iInterval: integer;
      bRandomInterval: boolean = false): integer;
    function AddRecordNow: boolean;
    function GetRecordRangeUnixTime(iFrom, iTo: Longword): string;
    function GetRecordRangeONo(iFrom, iTo: Longword): string; overload;
    function GetRecordRangeONo(const sDea: string): integer; overload;
  end;

implementation

uses DateUtils;

const
  C_Max_Record_Count = 100;  // Max. Anzahl an Datensätzen im DSfG-Telegramm

//---------------------------------- TSimuFile ---------------------------------

//------------------------------------------------
constructor TSimuFile.Create(const sFileName: TFileName;
  const sLogInfo_ThreadIdent: string = ''; const sLogPath: string = '');
//------------------------------------------------
begin
  inherited Create;

  Randomize;
  FFileName := sFileName;
  FLogInfo_ThreadIdent := sLogInfo_ThreadIdent;  // 14.01.2015, WW
  if FLogInfo_ThreadIdent <> '' then
    FLogInfo_ThreadIdent:=FLogInfo_ThreadIdent + #9;
  FLogPath:=sLogPath;  // 21.04.2015, WW

  FPosition := 0;    // Letzte Leseposition in FileStream
  FFileAge := 0;     // Letzter gelesener Dateistempel
  FIndex := 0;       // Nächster Index in Datei (noch nicht geschrieben)

  FLUnixTime := TList.Create;
  FLOrdinal := TList.Create;

  LoadFromFile;
end;

//------------------------------------------------
destructor TSimuFile.Destroy;
//------------------------------------------------
begin
  FreeAndNil(FLUnixTime);
  FreeAndNil(FLOrdinal);

  inherited;
end;

//------------------------------------------------
function TSimuFile.LoadFromFile: boolean;
//------------------------------------------------
var
  iCnt : integer;
  s       : string;
  iUnix, iONo : LongWord;
  bOK     : boolean;
begin
  Result := False;
  try
    iCnt := 0;
    if (FileExists(FFileName)) and (FFileAge <> FileAge(FFileName)) then begin
      WriteErrorLog(FLogInfo_ThreadIdent + 'Load file',
        ChangeFileExt(FFileName, '.LOG'), elt_Info, FLogPath);

      FFileAge := FileAge(FFileName);
      with TTextFileStreamExt.Create(
        FFileName, fmOpenRead or fmShareDenyWrite, bOK) do
      begin
        if (bOK) then
        try
          if (Self.FPosition > 0) then Seek(Self.FPosition, soFromBeginning);
          while (Position < Size) do begin
            ReadLn(s);
            iUnix := StrToIntDef('$' + GetStringPart(s, 1, #9), 0);
            iONo := StrToIntDef(GetStringPart(s, 2, #9), 0);
            if (iUnix > 0) and (iONo > 0) then begin
              FLUnixTime.Add(TObject(iUnix));
              FLOrdinal.Add(TObject(iONo));
              Inc(iCnt);
            end;
          end;
          Self.FPosition := Position;
          Self.FIndex := Self.FIndex + iCnt;
          Result := True;

          WriteErrorLog(FLogInfo_ThreadIdent +
            IntToStr(iCnt) + ' records loaded; total: ' +
            IntToStr(FLUnixTime.Count), ChangeFileExt(FFileName, '.LOG'),
            elt_Info, FLogPath);
        finally
          Free;
        end
        else Result := False;
      end;
    end
    else Result := True;
  except
    on E:Exception do
      WriteErrorLog(FLogInfo_ThreadIdent + 'Load file; Exception: ' + E.Message,
        ChangeFileExt(ParamStr(0), '.ERR'), elt_Error, FLogPath);
  end;
end;

//------------------------------------------------
function TSimuFile.SaveToFile: boolean;
//------------------------------------------------
var
  i   : integer;
  bOK : boolean;
  pStream : TTextFileStreamExt;
  iCnt : integer;
begin
  Result := False;
  try
    iCnt := 0;
    if (FIndex < FLUnixTime.Count) then begin
      WriteErrorLog(FLogInfo_ThreadIdent + 'Save file',
        ChangeFileExt(FFileName, '.LOG'), elt_Info, FLogPath);  // 14.01.2015, WW

      if (FileExists(FFileName)) and (FIndex > 0)
      then pStream := TTextFileStreamExt.Create(
        FFileName, fmOpenReadWrite or fmShareDenyWrite, bOK)
      else pStream := TTextFileStreamExt.Create(FFileName, fmCreate, bOK);
      if (bOK) then
      try
        if (Self.FPosition > 0) then
          pStream.Seek(Self.FPosition, soFromBeginning);
        for i := FIndex to FLUnixTime.Count-1 do begin
          pStream.WriteLn(IntToHex(Integer(FLUnixTime[i]), 8) + #9 +
            IntToStr(Integer(FLOrdinal[i]))
            // nur zum Testen: + #9 + DateTimeToStr (UnixToDateTime(Integer(FLUnixTime[i])))
            );
          Inc(iCnt); 
        end;
        Self.FIndex := FLUnixTime.Count;
        Self.FPosition := pStream.Position;

        WriteErrorLog(FLogInfo_ThreadIdent +
          IntToStr(iCnt) + ' records saved; total: ' +
          IntToStr(FLUnixTime.Count), ChangeFileExt(FFileName, '.LOG'),
          elt_Info, FLogPath);  // 14.01.2015, WW
      finally
        pStream.Free;
      end;
      Result := (bOK);
    end
    else Result := True;
  except
    on E:Exception do
      WriteErrorLog(FLogInfo_ThreadIdent + 'Save file; Exception: ' + E.Message,
        ChangeFileExt(ParamStr(0), '.ERR'), elt_Error, FLogPath);
  end;
end;

//------------------------------------------------
function TSimuFile.GetLastTimeAndONo(var iUnixTime, iONo: Longword): boolean;
//------------------------------------------------
// Liefert Zeitstempel und Ordnungsnummer des letzten Simulations-Archivdatensatzes
var
  iCount : integer;
begin
  Result := False;
  try
    iCount := FLUnixTime.Count;
    if (iCount > 0) then begin
      iUnixTime := Integer(FLUnixTime[iCount-1]);
      iONo := Integer(FLOrdinal[iCount-1]);
      Result := (iUnixTime > 0) and (iONo > 0);
    end
    else begin
      iUnixTime := 0;
      iONo := 0;
      Result := True;
    end;
  except
    on E:Exception do
      WriteErrorLog(FLogInfo_ThreadIdent +
        'Last Time and ordinal; Exception: ' + E.Message,
        ChangeFileExt(ParamStr(0), '.ERR'), elt_Error, FLogPath);
  end;
end;

//------------------------------------------------
function TSimuFile.AddRecordsFrom(dtFrom: TDateTime; iInterval: integer;
  bRandomInterval: boolean = false): integer;
//------------------------------------------------
// Fügt Simulations-Archivdatensätze ein (Zeitbereich: von - aktuelle PC-Zeit)
// im übergebenem Intervall in s (optional: als Random-Intervall)
var
  dt, dtHelp   : TDateTime;
  iUnix, iONo  : LongWord;
  y, m, d, h, n, s, ms : word;
begin
  Result := 0;
  try
    if (FLUnixTime.Count > 0) then begin
      // Daten ergänzen, die vor den bisher gespeicherten liegen
      iUnix := Integer(FLUnixTime[0]);
      iONo := Integer(FLOrdinal[0]);
      // Auf Stunden-/Tagesanfang beziehen
      dt := UnixToDateTime(iUnix);
      DecodeDate(dt, y, m, d);
      DecodeTime(dt, h, n, s, ms);
      dtHelp := EncodeDate(y, m, d);
      if (iInterval <= 3600) then dtHelp := dtHelp + EncodeTime(h, 0, 0, 0);
      while (dtHelp < (dt - EncodeTime(0, 0, 1, 0))) do
        dtHelp := IncSecond(dtHelp, iInterval);
      dt := IncSecond(dtHelp, -iInterval);
      Dec(iONo);

      // Daten am Anfang einfügen
      while (dt >= dtFrom) do begin
        if (iONo <= 0) then iONo := 999999;
        FLUnixTime.Insert(0, TObject(DateTimeToUnix(dt)));
        FLOrdinal.Insert(0, TObject(iONo));
        dt := IncSecond(dt, -iInterval);
        Dec(iONo);
        Inc(Result);
      end;
    end
    else begin
      // Noch keine Daten vorhanden: 1. Datensatz einfügen
      FLUnixTime.Add(TObject(DateTimeToUnix(dtFrom)));
      iONo := Random(9999) + 1;
      FLOrdinal.Add(TObject(iONo));
    end;

    // Daten am Ende einfügen
    Result := Result + AddRecordsTo(Now, iInterval, bRandomInterval);
  except
    on E:Exception do
      WriteErrorLog(FLogInfo_ThreadIdent +
        'Add records from; Exception: ' + E.Message,
        ChangeFileExt(ParamStr(0), '.ERR'), elt_Error, FLogPath);
  end;
end;

//------------------------------------------------
function TSimuFile.AddRecordsTo(dtTo: TDateTime; iInterval: integer;
  bRandomInterval: boolean = false): integer;
//------------------------------------------------
// Ergänzt Simulations-Archivdatensätze bis zum übergebenem Zeitpunkt
// im übergebenem Intervall in s (optional: als Random-Intervall)
var
  dt, dtHelp   : TDateTime;
  iUnix, iONo  : LongWord;
  y, m, d, h, n, s, ms : word;
  iInc: Int64;
begin
  Result := -1;
  try
    if (GetLastTimeAndONo(iUnix, iONo)) then begin
      Result := 0;
      if (iUnix > 0) then begin
        dt := UnixToDateTime(iUnix);
        DecodeDate(dt, y, m, d);
        DecodeTime(dt, h, n, s, ms);

        dtHelp := EncodeDate(y, m, d);
        if (iInterval <= 3600) then dtHelp := dtHelp + EncodeTime(h, 0, 0, 0);
        if bRandomInterval then begin  // 14.01.2015, WW
          iInc := Random(iInterval);
          if iInc = 0 then
            iInc := 1;                  
          dtHelp := IncSecond(dt, iInc);
        end
        else begin
          while (dtHelp < (dt + EncodeTime(0, 0, 1, 0))) do
            dtHelp := IncSecond(dtHelp, iInterval);
        end;

        dt := dtHelp;
        iONo := iONo + 1;
      end
      else begin
        DecodeDate(dtTo, y, m, d);
        DecodeTime(dtTo, h, n, s, ms);
        dtHelp := EncodeDate(y, m, d);
        if (iInterval <= 3600) then dtHelp := dtHelp + EncodeTime(h, 0, 0, 0);
        dt := dtHelp;
        iONo := Random(9999) + 1;
      end;

      while (dt <= dtTo) do begin
        FLUnixTime.Add(TObject(DateTimeToUnix(dt)));
        FLOrdinal.Add(TObject(iONo));
        if bRandomInterval then begin  // 14.01.2015, WW
          iInc := Random(iInterval);
          if iInc = 0 then
            iInc := 1;
          dt := IncSecond(dt, iInc);
        end else
          dt := IncSecond(dt, iInterval);

        Inc(Result);
        Inc(iONo);
      end;
    end;
  except
    on E:Exception do
      WriteErrorLog(FLogInfo_ThreadIdent +
        'Add records to; Exception: ' + E.Message,
        ChangeFileExt(ParamStr(0), '.ERR'), elt_Error, FLogPath);
  end;
end;

//------------------------------------------------
function TSimuFile.AddRecordNow: boolean;
//------------------------------------------------
// Fügt Simulations-Archivdatensatz mit Zeitstempel der aktuellen PC-Zeit ein
var
  iUnix, iONo  : LongWord;
begin
  Result := False;
  try
    if (GetLastTimeAndONo(iUnix, iONo)) then begin
      FLUnixTime.Add(TObject(DateTimeToUnix(Now)));
      FLOrdinal.Add(TObject(iONo+1));
      Result := True;
    end;
  except
    on E:Exception do
      WriteErrorLog(FLogInfo_ThreadIdent +
        'Add record now; Exception: ' + E.Message,
        ChangeFileExt(ParamStr(0), '.ERR'), elt_Error, FLogPath);
  end;
end;

//------------------------------------------------
function TSimuFile.GetRecordRangeUnixTime(iFrom, iTo: Longword): string;
//------------------------------------------------
// Liefert Simulations-Archivdatensätze für den übergebenen von/bis-Zeitbereich
var
  i, iIx : integer;
begin
  LoadFromFile;

  try
    // Eintrag für Von-Zeit vorhanden?
    iIx := FLUnixTime.IndexOf(TObject(iFrom));

    WriteErrorLog(FLogInfo_ThreadIdent +
      'Time range request; From: ' + IntToHex(iFrom, 8) +
      '; Index: ' + IntToStr(iIx), ChangeFileExt(FFileName, '.LOG'),
      elt_Info, FLogPath);
    // Wenn nein, Liste durchsuchen
    if (iIx < 0) then begin
      for i := 0 to FLUnixTime.Count-1 do begin
         if (Longword(FLUnixTime[i]) >= iFrom) then begin
           iIx := i;
           Break;
         end;
      end;
    end;

    if (iIx >= 0) then begin
      with TStringList.Create do
      try
        for i := iIx to FLUnixTime.Count-1 do begin
          if (Count < C_Max_Record_Count) and
            ((iTo = 0) or (Longword(FLUnixTime[i]) <= iTo))
          then Add(IntToHex(Integer(FLUnixTime[i]), 8) + #9 +
            IntToStr(Integer(FLOrdinal[i])))
          else Break;
        end;
        Result := CommaText;

        WriteErrorLog(FLogInfo_ThreadIdent +
          'Time range request; Total: ' + IntToStr(Count),
          ChangeFileExt(FFileName, '.LOG'), elt_Info, FLogPath);
      finally
        Free;
      end;
    end
  else WriteErrorLog(FLogInfo_ThreadIdent +
    'Time range request; Not found: ' + IntToHex(iFrom, 8) +
    ' -> ' + IntToHex(iTo, 8), ChangeFileExt(FFileName, '.LOG'),
    elt_Info, FLogPath);
  except
    on E:Exception do
      WriteErrorLog(FLogInfo_ThreadIdent +
        'Time range request; Exception: ' + E.Message,
        ChangeFileExt(ParamStr(0), '.ERR'), elt_Error, FLogPath);
  end;
end;

//------------------------------------------------
function TSimuFile.GetRecordRangeONo(iFrom, iTo: Longword): string;
//------------------------------------------------
// Liefert Simulations-Archivdatensätze für den übergebenen von/bis-
// Ordnungsnummernbereich
var
  i, iIx : integer;
begin
  LoadFromFile;

  try
    // Eintrag für Von-Ordnungsnummer vorhanden?
    iIx := FLOrdinal.IndexOf(TObject(iFrom));

    WriteErrorLog(FLogInfo_ThreadIdent +
      'Ordinal range request; From: ' + IntToStr(iFrom) +
      '; Index: ' + IntToStr(iIx), ChangeFileExt(FFileName, '.LOG'),
      elt_Info, FLogPath);
    // Wenn nein, Liste durchsuchen
    if (iIx < 0) then begin
      for i := 0 to FLOrdinal.Count-1 do begin
         if (Longword(FLOrdinal[i]) >= iFrom) then begin
           iIx := i;
           Break;
         end;
      end;
    end;

    if (iIx >= 0) then begin
      with TStringList.Create do
      try
        for i := iIx to FLOrdinal.Count-1 do begin
          if (Count < C_Max_Record_Count) and
            ((iTo = 0) or (Longword(FLOrdinal[i]) <= iTo))
          then Add(IntToHex(Integer(FLUnixTime[i]), 8) + #9 +
            IntToStr(Integer(FLOrdinal[i])))
          else Break;
        end;
        Result := CommaText;

        WriteErrorLog(FLogInfo_ThreadIdent +
          'Ordinal range request; Total: ' + IntToStr(Count),
          ChangeFileExt(FFileName, '.LOG'), elt_Info, FLogPath);
      finally
        Free;
      end;
    end
  else WriteErrorLog(FLogInfo_ThreadIdent +
    'Ordinal range request; Not found: ' + IntToStr(iFrom) +
    ' -> ' + IntToStr(iTo), ChangeFileExt(FFileName, '.LOG'), elt_Info, FLogPath);
  except
    on E:Exception do
      WriteErrorLog(FLogInfo_ThreadIdent +
        'Ordinal range request; Exception: ' + E.Message,
        ChangeFileExt(ParamStr(0), '.ERR'), elt_Error, FLogPath);
  end;
end;

//------------------------------------------------
function TSimuFile.GetRecordRangeONo(const sDea: string): integer;
//------------------------------------------------
// Liefert eine Füllstandsangabe (von oder bis) des Simulationsarchivs über
// Datenelementadresse
begin
  LoadFromFile;  // 14.01.2015, WW

  if (sDea[1] = 'c') and (sDea[2] in ['a','b']) then begin
    if (Length(sDea) = 4) and (sDea[2] = 'a') and (sDea[3] in ['a'..'z']) then begin
      case sDea[4] of
        'c': begin  // DE-Adresse für 'Archiv-Füllstand von'
               if (FLOrdinal.Count > 0) then
                 Result := Integer(FLOrdinal[0])
               else
                 Result:=0;
             end;

        'd': begin  // DE-Adresse für 'Archiv-Füllstand bis'
               if (FLOrdinal.Count > 0) then
                 Result := Integer(FLOrdinal[FLOrdinal.Count-1])
               else
                 Result:=0;
             end;
        else Result := -1;
      end;
    end

    else if (Length(sDea) = 5) and (sDea[2] = 'b') and (sDea[3] in ['a'..'d']) and
            (sDea[4] in ['a'..'j']) then begin  // Bugfix Logbuch-DEA; 14.01.2015, WW
      case sDea[5] of
        'a': begin  // DE-Adresse für 'Logbuch-Füllstand von'
               if (FLOrdinal.Count > 0) then
                 Result := Integer(FLOrdinal[0])
               else
                 Result:=0;
             end;

        'b': begin  // DE-Adresse für 'Logbuch-Füllstand bis'
               if (FLOrdinal.Count > 0) then
                 Result := Integer(FLOrdinal[FLOrdinal.Count-1])
               else
                 Result:=0;
             end;
        else Result := -1;
      end;
    end
    else Result := -1
  end
  else Result := -1;
end;

end.
