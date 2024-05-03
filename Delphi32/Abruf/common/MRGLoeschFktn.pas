{******************************************************************************}
{* Unit:  Unterprogramme zum Löschen von Langzeitdaten unter TVision          *}
{* Version: 17.3.1994  WW                                                     *}
{* 19.3.2001  WW  Fehler beim "Löschen von" für Stundensätze und Tagessätze   *}
{*                behoben (Lesen über Dateiende hinaus)                       *}
{* 08.11.2001  GD  Konstanten auf MSysCon ausgelagert, umbenannt (UPLOESCH)   *}
{******************************************************************************}
UNIT MRGLoeschFktn;

INTERFACE

uses
  Forms, Classes, T_Tools, T_Zeit, novell, WSysCon, Langzeit, PathIni, SysUtils,
  Dialogs, LGZType;


TYPE
  DATZEIT_REC = record
      UHRZEIT     : TIMEREC;
      DATUM       : DATEREC;
    END; { DATZEIT_REC }

{--------------------------------------------------------------------------}

procedure LoeschenBis(loeschdaten: datzeit_rec; lgznr: string; kennung: string);
{ Langzeitdaten (Tages-/Stunden-/Prüfungss„tzes„tze) l”schen bis ... }


procedure LoeschenAb(loeschdaten: datzeit_rec; lgznr: string; kennung: string);
{ Langzeitdaten (Tages-/Stunden-/Prüfungss„tze) l”schen ab ... }


IMPLEMENTATION

Var
      PrufLoeschfile  : PruefRec;
      D_PrufLoeschfile: file of PruefRec;
      LgzSRecSize   : longint;
      LgzTRecSize   : longint;
      Tagfile_zeit    : timerec;
      ok              : boolean;
      PfadLgzDB       : string;
      MaxKanalzahl    : integer;

{--------------------------------------------------------------------------}
Procedure DatenRecordGroesse;
begin
  LgzSRecSize:=1 + SizeOf(DateRec) + SizeOf(TimeRec) +
                 MaxKanalZahl*SizeOf(KanalRec);
{$IFDEF ZAEHL14}
  LgzTRecSize:=3 + SizeOf(DateRec) + 2*SizeOf(Zaehler_Rec)*MaxKanalZahl;
{$ELSE}
  LgzTRecSize:=2 + SizeOf(DateRec) + 2*SizeOf(Zaehler_Rec)*MaxKanalZahl;
{$ENDIF}
end;

{--------------------------------------------------------------------------}

procedure VarTruncate(Posfile: longint; lgzdname: string);
var
  F: file of char;
  size: longint;
begin
  assignfile(F,lgzdname);
  reset(F);
  seek(F,Posfile);
  truncate(F);
  size:= filesize(F);
  close(F);
  if size=0 then erase(F);
end;

{--------------------------------------------------------------------------}

procedure Findedatum_Std(var position: longint; loeschdaten: datzeit_rec;
                         Kennung: string;Lgznr: string);
{ findet und markiert Eintrag mit gesuchtem Datum in Stundensatzdatei }

Var  gefunden: boolean;
     lgzdname: string;
     StdRec: lgzsrec;
     SizeofStream: longint;
     Diff: longint;
     Byteposition: longint;
     StdLoeschfile   : TFileStreamExt;

begin
  PfadLgzDB:= PathServer.PathName[LangzeitDir];
  MaxKanalZahl := PathServer.MaxKanaele; {ProgramIni.KanalZahl;}
  lgzdname:= PfadLgzDB + 'LGZD' + Lgznr + '.Dat';
  DatenRecordGroesse;
  position:=-1;
  gefunden:=false; {kennung--langzeitdatnr? abfangen}

  StdLoeschfile:= TFileStreamExt.Create (lgzdname,fmOpenReadWrite OR fmShareDenyWrite, Ok);
  try
    if ok then begin
      SizeofStream:= StdLoeschfile.size;
      if SizeOfStream > 0 then begin
        repeat
          inc(position);
          Byteposition:= Position * LgzSRecSize;
          Diff:= SizeofStream - Byteposition;
          StdLoeschfile.seek(Byteposition,soFromBeginning);
          StdLoeschfile.ReadBuffer(StdRec, LgzSRecSize);

          if (CmpDate(StdRec.datum,loeschdaten.datum) +
              CmpTime(StdRec.zeit,loeschdaten.uhrzeit)) > 0 then begin
            gefunden:=true;
          end;
        until (Diff <= LgzSRecSize) or gefunden;
      end;
      if not gefunden then inc(position);       { position zeigt auf Dateiende }
    end;
  finally
    StdLoeschfile.free;
  end;
end;


{--------------------------------------------------------------------------}

procedure Findedatum_Tag(var position: longint; loeschdaten: datzeit_rec;
                         Kennung: string;Lgznr: string);
{ findet und markiert Eintrag mit gesuchtem Datum in Tagessatzdatei }

Var  gefunden: boolean;
     lgzdname: string;
     TagRec: lgztrec;
     SizeofStream: longint;
     Diff: longint;
     Byteposition: longint;
     TagLoeschfile   : TFileStreamExt;

begin
  PfadLgzDB:= PathServer.PathName[LangzeitDir];
  MaxKanalZahl := PathServer.MaxKanaele;
  lgzdname:= PfadLgzDB + 'LGZT' + Lgznr + '.Dat';
  DatenRecordGroesse;
  Tagfile_zeit.min:=0;
  Tagfile_zeit.sec:=0;
  Tagfile_zeit.hsec:=0;

  position:=-1;
  gefunden:=false;
  TagLoeschfile:= TFileStreamExt.Create (lgzdname,fmOpenReadWrite OR fmShareDenyWrite, Ok);
  try
    if ok then begin
      SizeofStream:= TagLoeschfile.size;
      if SizeOfStream > 0 then begin
        repeat
          inc(position);
          Byteposition:= Position * LgzTRecSize;
          Diff:= SizeofStream - Byteposition;
          TagLoeschfile.seek(Byteposition,soFromBeginning);
          TagLoeschfile.ReadBuffer(TagRec, LgzTRecSize);
          Tagfile_zeit.hour:=TagRec.stunden;
          if (CmpDate(TagRec.datum,loeschdaten.datum) +
              CmpTime(Tagfile_zeit,loeschdaten.uhrzeit)) > 0 then begin
            gefunden:=true;
          end;
        until (Diff <= LgzTRecSize) or gefunden;
      end;
      if not gefunden then inc(position);        { position zeigt auf Dateiende }
    end;
  finally
    TagLoeschfile.free;
  end;
end;


{--------------------------------------------------------------------------}
procedure Findedatum_Pruf(var position: longint; loeschdaten: datzeit_rec;
                         Kennung: string;Lgznr: string);
{ findet und markiert Eintrag mit gesuchtem Datum in Prüfungssatzdatei }

Var  gefunden: boolean;
     lgzdname: string;

begin
  PfadLgzDB:= Pathserver.PathName[LangzeitDir];
  lgzdname:= PfadLgzDB + 'LGZQ' + LgzNr + '.Dat';

  position:=-1;
  gefunden:=false;
  assign(D_PrufLoeschfile,lgzdname);
  if Netreset(D_PrufLoeschfile, SizeOf(PruefRec), ForAll+DenyAll) < 0 then;

  repeat
    read(D_PrufLoeschfile,PrufLoeschfile);
    inc(position);

    if (CmpDate(PrufLoeschfile.von_datum,loeschdaten.datum) +
        CmpTime(PrufLoeschfile.von_zeit,loeschdaten.uhrzeit)) > 0   then
    begin
      gefunden:=true;
    end;

  until eof(D_PrufLoeschfile) or gefunden;

  if not gefunden then inc(position);       (* position zeigt auf Dateiende *)
  close(D_PrufLoeschfile);
end;


{---------------------------------------------------------------------------}


function Umspeichern_Std(lpos: longint; lgzdname: string): boolean;
{ Stundensatzdatei ab lpos an den Anfang umspeichern
  Ergebnis: true, wenn Datei bearbeitet wurde }

Var  schreibpos : longint;
     posfile    : longint;
     StdRec     : lgzsrec;
     SizeofStream: longint;
     Streamposition: longint;
     Byteposition: longint;
     Streamende: longint;
     StdLoeschfile   : TFileStreamExt;

begin
  Umspeichern_Std:=false;
  PosFile:=0;
  if lpos>0 then
  begin
    schreibpos:=0;
    StdLoeschfile:= TFileStreamExt.Create (lgzdname,fmOpenReadWrite OR fmShareDenyWrite, Ok);
    try
      Streamposition:= 0;
      if ok then begin
        SizeofStream:= StdLoeschfile.size;
        Streamende:= (SizeofStream div LgzSRecSize) - lpos;
        while ( Streamposition < Streamende ) do
        begin
          Application.ProcessMessages;
          Byteposition:= lpos * LgzSRecSize;
          StdLoeschfile.seek(Byteposition,soFromBeginning);
          StdLoeschfile.ReadBuffer(StdRec, LgzSRecSize);
          inc(lpos);
          Byteposition:= schreibpos * LgzSRecSize;
          StdLoeschfile.seek(Byteposition,soFromBeginning);
          StdLoeschfile.WriteBuffer(StdRec,LgzSRecSize);
          inc(schreibpos);
          Byteposition:= lpos * LgzSRecSize;
          StdLoeschfile.seek(Byteposition,soFromBeginning);
          inc(Streamposition);
        end;
        Byteposition:= schreibpos * LgzSRecSize;
        StdLoeschfile.seek(Byteposition,soFromBeginning);
        {Ermitteln der Anzahl der Bytes bis zur aktuellen Fileposition}
        Posfile:= StdLoeschfile.Position;
      end;
    finally
      StdLoeschfile.free;
    end;

    if Ok then begin
      VarTruncate(Posfile,lgzdname);
      Umspeichern_Std:=true;
    end;
  end;
end;

{---------------------------------------------------------------------------}

function Umspeichern_Tag(lpos: longint; lgzdname: string): boolean;
{ Tagessatzdatei ab lpos an den Anfang umspeichern
  Ergebnis: true, wenn Datei bearbeitet wurde }

Var schreibpos : longint;
    posfile    : longint;
    TagRec     : lgztrec;
    SizeofStream: longint;
    Streamposition: longint;
    Byteposition: longint;
    Streamende: longint;
    TagLoeschfile   : TFileStreamExt;

begin
  Umspeichern_Tag:=false;
  if lpos>0 then
  begin
    PosFile:=0;
    schreibpos:=0;
    TagLoeschfile:= TFileStreamExt.Create (lgzdname,fmOpenReadWrite OR fmShareDenyWrite, Ok);
    try
      Streamposition:= 0;
      if ok then begin
        SizeofStream:= TagLoeschfile.size;
        Streamende:= (SizeofStream div LgzTRecSize) - lpos;
        while ( Streamposition < Streamende ) do
        begin
          Application.ProcessMessages;
          Byteposition:= lpos * LgzTRecSize;
          TagLoeschfile.seek(Byteposition,soFromBeginning);
          TagLoeschfile.ReadBuffer(TagRec, LgzTRecSize);
          inc(lpos);
          Byteposition:= schreibpos * LgzTRecSize;
          TagLoeschfile.seek(Byteposition,soFromBeginning);
          TagLoeschfile.WriteBuffer(TagRec,LgzTRecSize);
          inc(schreibpos);
          Byteposition:= lpos * LgzTRecSize;
          TagLoeschfile.seek(Byteposition,soFromBeginning);
          inc(Streamposition);
        end;
        Byteposition:= schreibpos * LgzTRecSize;
        TagLoeschfile.seek(Byteposition,soFromBeginning);
        Posfile:= TagLoeschfile.Position;
      end;
    finally
      TagLoeschfile.free;
    end;

    if Ok then begin
      VarTruncate(Posfile,lgzdname);
      Umspeichern_Tag:=true;
    end;
  end;
end;

{---------------------------------------------------------------------------}
function Umspeichern_Pruf(lpos: longint; lgzdname: string): boolean;
{ Prüfungssatzdatei ab lpos an den Anfang umspeichern
  Ergebnis: true, wenn Datei bearbeitet wurde }

Var  schreibpos : longint;
     size       : longint;

begin
  Umspeichern_Pruf:=false;
  if lpos>0 then
  begin
    schreibpos:=0;
    assign(D_PrufLoeschfile,lgzdname);
    if netreset(D_PrufLoeschfile, SizeOf(PruefRec), ForAll+DenyAll) < 0 then;
    seek(D_PrufLoeschfile,lpos);

    while not eof(D_PrufLoeschfile) do
    begin
      Application.ProcessMessages;
      read(D_PrufLoeschfile,PrufLoeschfile);
      inc(lpos);
      seek(D_PrufLoeschfile,schreibpos);
      write(D_PrufLoeschfile,PrufLoeschfile);
      inc(schreibpos);
      seek(D_PrufLoeschfile,lpos);
    end;

    seek(D_PrufLoeschfile,schreibpos);
    truncate(D_PrufLoeschfile);
    size:=filesize(D_PrufLoeschfile);
    close(D_PrufLoeschfile);
    if size=0 then erase(D_PrufLoeschfile);
    Umspeichern_Pruf:=true;
  end;
end;


{--------------------------------------------------------------------------}


function Kuerzen_Std(loeschdaten: datzeit_rec;Kennung: string;Lgznr: string): boolean;
{ findet und kürzt ab einschließlich Eintrag mit gesuchtem Datum in Stundensatzdatei
  Ergebnis: true, wenn Datei bearbeitet wurde }

Var  gefunden : boolean;
     lgzdname : string;
     position : longint;
     posfile  : longint;
     StdRec   : lgzsrec;
     SizeofStream: longint;
     Diff       : longint;
     Byteposition: longint;
     StdLoeschfile   : TFileStreamExt;

begin
  Kuerzen_Std:=false;
  PfadLgzDB:= PathServer.PathName[LangzeitDir];
  MaxKanalZahl := PathServer.MaxKanaele;
  lgzdname:= PfadLgzDB + 'LGZD' + Lgznr + '.Dat';
  DatenRecordGroesse;
  position:=-1;
  PosFile:=0;
  gefunden:=false;
  StdLoeschfile:= TFileStreamExt.Create (lgzdname,fmOpenReadWrite OR fmShareDenyWrite, Ok);
  try
    if ok then begin
      SizeofStream:= StdLoeschfile.size;
      if SizeOfStream > 0 then begin
        repeat
          inc(position);
          Byteposition:= position * LgzSRecSize;
          Diff:= SizeofStream - Byteposition;
          StdLoeschfile.seek(Byteposition,soFromBeginning);
          StdLoeschfile.ReadBuffer(StdRec, LgzSRecSize);

          if (CmpDate(StdRec.datum,loeschdaten.datum) +
            CmpTime(StdRec.zeit,loeschdaten.uhrzeit)) > 0   then
            gefunden:=true;
        until (Diff <= LgzSRecSize) or gefunden;

        if gefunden then
        begin
          Byteposition:= position * LgzSRecSize;
          StdLoeschfile.seek(Byteposition,soFromBeginning);
          Posfile:= StdLoeschfile.Position;
        end;
      end;
    end;
  finally
    StdLoeschfile.free;
  end;                                                           

  if ok and gefunden then begin
    VarTruncate(Posfile,lgzdname);
    Kuerzen_Std:=true;
  end;
end;

{--------------------------------------------------------------------------}

function Kuerzen_Tag(loeschdaten: datzeit_rec;Kennung: string;Lgznr: string): boolean;
{ findet und kürzt ab einschließlich Eintrag mit gesuchtem Datum in Tagessatzdatei
  Ergebnis: true, wenn Datei bearbeitet wurde }

Var  gefunden : boolean;
     lgzdname : string;
     position : longint;
     posfile  : longint;
     TagRec   : lgztrec;
     SizeofStream: longint;
     Diff       : longint;
     Byteposition: longint;
     TagLoeschfile   : TFileStreamExt;

begin
  Kuerzen_Tag:=false;
  PfadLgzDB:= PathServer.PathName[LangzeitDir];
  MaxKanalZahl := PathServer.MaxKanaele;
  lgzdname:= PfadLgzDB + 'LGZT' + Lgznr + '.Dat';
  DatenRecordGroesse;
  position:=-1;
  PosFile:=0;
  gefunden:=false;
  TagLoeschfile:= TFileStreamExt.Create (lgzdname,fmOpenReadWrite OR fmShareDenyWrite, Ok);
  try
    if ok then begin
      SizeofStream:= TagLoeschfile.size;
      if SizeOfStream > 0 then begin
        repeat
          inc(position);
          Byteposition:= position * LgzTRecSize;
          Diff:= SizeofStream - Byteposition;
          TagLoeschfile.seek(Byteposition,soFromBeginning);
          TagLoeschfile.ReadBuffer(TagRec, LgzTRecSize);

          if CmpDate(TagRec.datum,loeschdaten.datum) > 0  then
             gefunden:=true;
        until (Diff <= LgzTRecSize) or gefunden;

        if gefunden then
        begin
          Byteposition:= position * LgzTRecSize;
          TagLoeschfile.seek(Byteposition,soFromBeginning);
          Posfile:= TagLoeschfile.Position;
        end;
      end;
    end;
  finally
    TagLoeschfile.free;
  end;

  if ok and gefunden then begin
    VarTruncate(Posfile,lgzdname);
    Kuerzen_Tag:=true;
  end;
end;

{--------------------------------------------------------------------------}
function Kuerzen_Pruf(loeschdaten: datzeit_rec;Kennung: string;Lgznr: string): boolean;
{ findet und kürzt ab einschließlich Eintrag mit gesuchtem Datum in Prüfungssatzdatei
  Ergebnis: true, wenn Datei bearbeitet wurde }

Var  gefunden : boolean;
     lgzdname : string;
     position : longint;
     size     : longint;

begin
  Kuerzen_Pruf:=false;
  PfadLgzDB:= PathServer.PathName[LangzeitDir];
  lgzdname:= PfadLgzDB + 'LGZQ' + Lgznr + '.Dat';

  position:=-1;
  gefunden:=false;
  assign(D_PrufLoeschfile,lgzdname);
  if Netreset(D_PrufLoeschfile, SizeOf(PruefRec), ForAll+DenyAll) < 0 then;
  seek(D_PrufLoeschfile,0);
  repeat
    read(D_PrufLoeschfile,PrufLoeschfile);
    inc(position);

    if (CmpDate(PrufLoeschfile.von_datum,loeschdaten.datum) +
        CmpTime(PrufLoeschfile.von_zeit,loeschdaten.uhrzeit)) >= 0   then
        gefunden:=true;

  until eof(D_PrufLoeschfile) or gefunden;

  if gefunden then
  begin
    seek(D_PrufLoeschfile,position);
    truncate(D_PrufLoeschfile);
    Kuerzen_Pruf:=true;
  end;
  size:= filesize(D_PrufLoeschfile);
  close(D_PrufLoeschfile);
  if size=0 then erase(D_PrufLoeschfile);
end;


{------------------------------------------------------------------------------}


procedure LoeschenBis(loeschdaten: datzeit_rec; lgznr: string; kennung: string);

Var    lgzddatei : string;
       lgztdatei : string;
       lgzqdatei : string;
       pos       : longint;

begin
  DatenRecordGroesse;
  PfadLgzDB:= PathServer.PathName[LangzeitDir];
  lgzddatei:= PfadLgzDB + 'LGZD' + Lgznr + '.Dat';
  lgztdatei:= PfadLgzDB + 'LGZT' + Lgznr + '.Dat';
  lgzqdatei:= PfadLgzDB + 'LGZQ' + Lgznr + '.Dat';

  if FileExists(lgzddatei) then begin
    Findedatum_Std(pos,loeschdaten,Kennung,Lgznr);
    Umspeichern_Std(pos,lgzddatei);
  end;

  if FileExists(lgztdatei) then begin
    Findedatum_Tag(pos,loeschdaten,Kennung,Lgznr);
    Umspeichern_Tag(pos,lgztdatei);
  end;

  if FileExists(lgzqdatei) then begin
    Findedatum_Pruf(pos,loeschdaten,Kennung,Lgznr);
    Umspeichern_Pruf(pos,lgzqdatei);
  end;
end;


procedure LoeschenAb(loeschdaten: datzeit_rec; lgznr: string; kennung: string);

Var    lgzddatei : string;
       lgztdatei : string;
       lgzqdatei : string;

begin
  DatenRecordGroesse;
  PfadLgzDB:= PathServer.PathName[LangzeitDir];
  lgzddatei:= PfadLgzDB + 'LGZD' + Lgznr + '.Dat';
  lgztdatei:= PfadLgzDB + 'LGZT' + Lgznr + '.Dat';
  lgzqdatei:= PfadLgzDB + 'LGZQ' + Lgznr + '.Dat';

  if FileExists(lgzddatei) then
    Kuerzen_Std(loeschdaten,Kennung,Lgznr);

  if FileExists(lgztdatei) then
    Kuerzen_Tag(loeschdaten,Kennung,Lgznr);

  if FileExists(lgzqdatei) then
    Kuerzen_Pruf(loeschdaten,Kennung,Lgznr);
end;

END.