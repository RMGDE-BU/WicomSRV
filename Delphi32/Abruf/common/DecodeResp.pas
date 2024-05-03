{------------------------------------------------------------------------------}
{ Object zum Decodieren von DSfG-Daten in XML-Strings                          }
{                                                                              }
{ 03.11.2003  GD  Neu                                                          }
{ 23.12.2004  GD  Erweiterung für Transparentkommandos                         }
{ 26.02.2008  GD  Ausfiltern nicht sichtbarer Zeichen                          }
{ 23.09.2015  WW  Beschleunigte Dateizugriffe mit TFileOfCharStream            }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2003, RMG Messtechnik GmbH 2015               }
{------------------------------------------------------------------------------}
unit DecodeResp;

interface

uses
  Windows, SysUtils, Classes,
  GD_Utils, T_Zeit, WStream, WXmlConst, WSysCon;

type
  TDecodeXMLResponse = class(TObject)
  protected
    procedure DelayLoop(
      var iDelayCount: longword; const iDelayNumber: longword);
    function XMLStringToDateTime(sString: string): TDateTime;
  public
    function CutDataString(
      const sData: string; sStart, sEnd: string; bTrim: boolean = True): string;
    function GetNextDataBlock(
      var sData: string; sStart, sEnd: string; sAltEnd: string = '';
      bTrim: boolean = true): string;
    function GetDataPart(sString: string; iIndex: integer;
      sSeparator: string; bTrim: boolean = True): string;
    function IsCorrectXML(sXMLDataString: string): boolean;
    function IsCompleteXML(sXMLDataString: string): boolean;
    function SetStreamPos(
      pStream: TFileOfCharStream; pSearchArray: array of string): string;
    function ReadStreamUntil(pStream: TFileOfCharStream; sUntil: string;
      iMaxLength: integer = 0): string;
    function ExtractTransparentResponseFromXML(sXMLDataString: string): string;
    function ConvertXmlToVerbInfoData(sXmlData: string;
      var VerbInfoData: TVerbInfoData): boolean;
  end;

function CutOneXMLResponse (var XMLResponses: string): string;
  
implementation

const
  C_XMLTerminatorStr = '</methodResponse>';   { Abschluß eines XML-Strings }

{------------------------------------------------------------}
function CutOneXMLResponse (var XMLResponses: string): string;
{------------------------------------------------------------}
{ liefert die erste in XMLResponses enthaltene XML-Antwort (Ergebnis) und
  schneidet diese aus XMLResponses aus (Rückgabe) }
var
  P: integer;
begin
  P:=Pos (C_XMLTerminatorStr, XMLResponses);
  if P = 0 then
    P:=length (XMLResponses);
  { eine Teil-Response von evtl. mehreren enthaltenen rauskopieren: }
  Result:=Copy (XMLResponses, 1, P + 16);
  Delete (XMLResponses, 1, P + 16);   { aus Gesamt-Antwort löschen }
end;


{ TDecodeXMLResponse }

{-------------------------------------------------}
procedure TDecodeXMLResponse.DelayLoop(
  var iDelayCount: longword; const iDelayNumber: longword);
{-------------------------------------------------}
begin
  Inc(iDelayCount);
  if (iDelayCount >= iDelayNumber) then begin
    Delay(1);
    iDelayCount := 0;
  end;
end;

{ Übergebener XML-String syntaktisch korrekt ?    }
{ Parameter: XML-String ('' = Def.-String verw.)  }
{ Rückgabe: Korrekt ja/nein                       }
{-------------------------------------------------}
function TDecodeXMLResponse.IsCorrectXML(sXMLDataString: string): boolean;
{-------------------------------------------------}
begin
  // Dummy ...
  Result:=Pos('<methodResponse>', sXMLDataString) > 0;
end;

{ Übergebener XML-String abgeschlossen ?          }
{ Parameter: XML-String ('' = Def.-String verw.)  }
{ Rückgabe: Vollständig ja/nein                   }
{-------------------------------------------------}
function TDecodeXMLResponse.IsCompleteXML(sXMLDataString: string): boolean;
{-------------------------------------------------}
begin
  { mit </methodResponse> ist der XML-String abgeschlossen }
  Result:=Pos(C_XMLTerminatorStr, sXMLDataString) > 0;
end;

{ Gibt begrenzten string zurück                   }
{ Parameter: Datenstring, Start-/Ende-string      }
{ Rückgabe: string oder ''                        }
{-------------------------------------------------}
function TDecodeXMLResponse.CutDataString(
  const sData: string; sStart, sEnd: string; bTrim: boolean = True): string;
{-------------------------------------------------}
var
  s            : string;
  iStart, iEnd : integer;
begin
  Result := '';

  // Begrenzung der Daten ermitteln
  iStart := Pos(sStart, sData) + Length(sStart);
  if (iStart > 0) then begin
    s := Copy(sData, iStart, Length(sData)-iStart+1);
    iEnd := Pos(sEnd, s);
    // Falls Begrenzung fehlt => raus !
    if (iEnd > 0) then Result := Copy(s, 1, (iEnd-1));
    if (bTrim) then Result := Trim(Result);
  end;
end;

{ Gibt jeweils ersten Datenblock zurück           }
{ Parameter: Datenstring (Datenblock wird gel.)   }
{            Start-/Ende-string, altern. Ende-str.}
{ Rückgabe: 1. Datenblock                         }
{-------------------------------------------------}
function TDecodeXMLResponse.GetNextDataBlock(
  var sData: string; sStart, sEnd: string; sAltEnd: string = '';
  bTrim: boolean = true): string;
{-------------------------------------------------}
var
  iStart, iEnd : integer;
  sEndString   : string;
begin
  // Begrenzung der Daten ermitteln
  iStart := Pos(sStart, sData) + Length(sStart);
  if (iStart > Length(sStart)) then begin
    iEnd := Pos(sEnd, Copy(sData, iStart, Length(sData)));
    if (iEnd > 0) then iEnd := iEnd + iStart - 1;
    if (iEnd = 0) and (sAltEnd <> '') then begin
      iEnd := Pos(sAltEnd, sData);
      sEndString := sAltEnd;
    end
    else sEndString := sEnd;
    // Falls Begrenzung fehlt => raus !
    if (iStart > Length(sStart)) and (iEnd > 0) then begin
      Result := Copy(sData, iStart, (iEnd-iStart));      
      if bTrim then  // 24.10.2007; WW
        Result := Trim(Result);
      System.Delete(sData, 1, iEnd + (Length(sEndString)-1));
      sData := StringReplace(sData, #13#10, '', []);  // Nächsten ZU löschen
    end
    else Result := '';
  end
  else Result := '';
end;

{ Gibt Teilstring zurück                     }
{ Parameter: String, Index des Elementes,    }
{            Teilstring-Trenner              }
{ Rückgabe: Element oder ''                  }
{--------------------------------------------}
function TDecodeXMLResponse.GetDataPart(sString: string; iIndex: integer;
  sSeparator: string; bTrim: boolean = True): string;
{--------------------------------------------}
var
  iPos  : integer;
  iLoop : integer;
  s     : string;
begin
  Result := '';
  if (sString <> '') and (sSeparator <> '') then begin
    s := sString;
    for iLoop := 1 to iIndex-1 do begin
      iPos := Pos(sSeparator, s);
      if (iPos = 0) then Exit
      else Delete(s, 1, iPos + (Length(sSeparator)-1));
    end;
    iPos := Pos(sSeparator, s);
    if (iPos > 0) then Result := Copy(s, 1, iPos-1) else Result := s;
    if (bTrim) then Result := Trim(Result);
  end;
end;

{ Setzt Position in Char-Daten-Stream        }
{ Parameter: Char-Daten-Stream, Suchbegriffe }
{ Rückgabe: Gefundener string oder ''        }
{--------------------------------------------}
function TDecodeXMLResponse.SetStreamPos(
  pStream: TFileOfCharStream;  // 23.09.2015, WW
  pSearchArray: array of string): string;
{--------------------------------------------}
var
  p, pArray : PChar;
  i, iPos   : integer;
  c         : char;
begin
  Result := '';  // Gefundener String
  p := nil;
  GetMem(pArray, 101);
  try
    for i := 50 to 99 do pArray[i] := ' ';
    pArray[100] := #0;

    while (pStream.Position < pStream.Size) do begin  // Schleife durch den Stream
      i := 50;
      // Array überlappend füllen
      while (i < 100) do begin
        pStream.Read(c);
        if (Ord(c) >= Ord(' ')) then begin  // 26.02.2008 nur sichtbare Zeichen
          pArray[i-50] := pArray[i];
          pArray[i] := c;
          Inc(i);
        end;
        if (pStream.Position = pStream.Size) then Exit;
      end;

      // Prüfen, ob ein Suchbegriff enthalten ist
      for i := Low(pSearchArray) to High(pSearchArray) do begin
        p := StrPos(pArray, PChar(pSearchArray[i]));
        if (p <> nil) then begin
          Result := pSearchArray[i];
          Break;
        end;
      end;

      // Falls Erfolg: Stream positionieren und raus
      if (Assigned(p)) then begin
         iPos := (p-pArray)-100; // Offset: (<Länge pArray>-<Längen-Differenz>)
        pStream.Seek(iPos, soFromCurrent);
        Break;
      end;
    end;
  finally
    FreeMem(pArray, 101);
  end;
end;

{ Sucht Teilstring in Char-Daten-Stream      }
{ Parameter: Char-Daten-Stream, Suchbegriff, }
{   max. Länge (max. 99999)                  }
{ Rückgabe: Gefundener string oder ''        }
{--------------------------------------------}
function TDecodeXMLResponse.ReadStreamUntil(
  pStream: TFileOfCharStream;  // 23.09.2015, WW
  sUntil: string; iMaxLength: integer = 0): string;
{--------------------------------------------}
var
  i, iMax : integer;
  c       : char;
  bFound  : boolean;
begin
  Result := '';     // Gefundener String
  bFound := False;  // Erfolg ?
  if (iMaxLength = 0) then iMax := 99999 else iMax := iMaxLength;

  for i := 1 to iMax do begin
    pStream.Read(c);
    Result := Result + c;
    if (Pos(sUntil, Result) > 0) then begin
      bFound := True;
      Break;
    end;
  end;

  if (not bFound) then Result := '';
end;

{ Wandelt XML-Zeitstempel-String in DateTime-Format     }
{ Parameter: XML-Zeitstempel als String                 }
{ Rückgabe: Zeitstempel als DateTime (0, wenn ungültig) }
{--------------------------------------------}
function TDecodeXMLResponse.XMLStringToDateTime(sString: string): TDateTime;
{--------------------------------------------}
var
  dtd, dtt : TDateTime;
begin
  Result := 0;
  if length (sString) > 0 then begin
    if (EncodeDateStr(Copy(sString, 1, 8), 'YYYYMMDD', dtd)) and
       (EncodeTimeStr(Copy(sString, 9, 6), 'HHMMSS', dtt)) then
      Result := Trunc(dtd) + Frac(dtt);
  end;
end;

{ Gibt Antwort auf Transparent-Kommando aus XML zurück   }
{ Parameter: XML-codierter string            }
{ Rückgabe: Gefundener string oder ''        }
{--------------------------------------------}
function TDecodeXMLResponse.ExtractTransparentResponseFromXML(  // 23.12.2004
  sXMLDataString: string): string;
{--------------------------------------------}
var
  s : string;
begin
  s := CutDataString(sXMLDataString, C_WieserStart, C_WieserEnd);
  s := CutDataString(s, C_TransparentAntwStart, C_TransparentAntwEnd, False);
  Result := s;
end;

{ Gibt Verbindungsinformationen aus XML-Responselog zurück }
{ Parameter: XML-Datenstring                      }
{ Rückgabe: Verbindungsinfo-Record                }
{ Ergebnis: true, wenn XML-Verbindungsinfo vorhanden }
{-------------------------------------------------}
function TDecodeXMLResponse.ConvertXmlToVerbInfoData(  // 20.12.2013
  sXmlData: string; var VerbInfoData: TVerbInfoData): boolean;
{-------------------------------------------------}
var
  s, sText : string;
begin
  Result:=false;
  with VerbInfoData do begin  // Vorbelegung
    DZ_VerbindungSteht:=0;
    DZ_Login:=0;
  end;

  s := CutDataString(sXmlData, C_ConnInfoBlockStart, C_ConnInfoBlockEnd);
  if (s <> '') then begin
    sText := CutDataString(s, C_DZ_ConnectStart, C_DZ_ConnectEnd);
    VerbInfoData.DZ_VerbindungSteht := XMLStringToDateTime(sText);
    sText := CutDataString(s, C_DZ_LoginStart, C_DZ_LoginEnd);
    VerbInfoData.DZ_Login := XMLStringToDateTime(sText);
    Result:=true;
  end;
end;

end.
