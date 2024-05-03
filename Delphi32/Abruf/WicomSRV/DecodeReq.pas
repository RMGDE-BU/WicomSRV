{******************************************************************************}
{ 10.12.02  SM  XML Parser , coder Klassen                                     }
{ 18.07.03  WW  XML-String zusammensetzen beschleunigt                         }
{ 13.01.11  GD  t-Befehl für GasX					                                     }
{ 24.02.11  GD  XML-Ersatzzeichen in DSfG-Parametertexten                      }
{ 24.03.11  GD  Weitere XML-Ersatzzeichen in DSfG-Parametertexten              }
{ 15.03.12  WW  mit öffentlichem Schlüssel X, Y für digitale Signatur in XML-  }
{               Resonse                                                        }
{ 23.03.12  WW  mit IFNDEF 'NO_XML_SIGVERIFYSTATE'                             }
{ 07.04.14  WW  Prüfung auf XML-Tag 'digitalsignature' in XML-Request; XML-Tag }
{               'sigverifystate' in XML-Responses auch für Gas-X-Version;      }
{               Rohdatenliste in XML-Responses AR, LB, DE (rawdata_list)       }
{ 11.07.16  WW  Anpassung für zeilenweise abgefragte Daten einer DSfG-Archiv-  }
{               gruppe                                                         }
{ 31.01.17  WW  GET_XML_VerbAufbauDSfG_Response erweitert um 'extensionmode'   }
{ 13.03.18  WW  Bugfix C-Response bei DSfG-DFÜ-Parametrierung: Parametrier-    }
{               Ergebnisdaten bei Verstellung eines Normparameters fehlten;    }
{               Gas-X-Version: mit erweiterten Ergebnis-Rückgaben bei allen    }
{               Responses, Busadresse der Login-DFÜ-Instanz zurückgeben in v-  }
{               Response DSfG                                                  }
{ 18.07.18  WW  Bugfix fehlender valchange-Abschluß / in Parametrieren-Response}
{ 24.02.20  WW  E-Response für DSfG-Messwerte mit optionalem COM-Tracelog;     }
{               nur Gas-X-Version: v-Response DSfG erweitert um Erweiterungs-  }
{               grad der Login-DFÜ, Z-Response erweitert um Responsedata mit   }
{               Server-Zeit und Geräte-Zeit vor der Synchronisierung           }
{ 03.06.20  WW  mit optionaler Basis-Authentifizierung im Header; XML-Ersatz-  }
{               zeichen in Responses für Parameter MRG/DSfG-DFÜ und Para-      }
{               metrierung MRG/DSfG/DSfG-DFÜ; XML-Ersatzzeichen decodieren in  }
{               Request-Teil und codieren in Response-Teil                     }
{ 28.09.20  WW  E-Response für MRG-Messwerte <voh> und M-Response für MRG-     }
{               Meldungen <msg> mit optionalem XML-Tag 'ordinal' (nur Gas-X-   }
{               Version)                                                       }
{ 18.02.21  WW  Anpassung Ordnungsnummer-Prüfung 'fehlend' an geänderten       }
{               Default-Wert -1 (wegen TME400)                                 }
{ 03.01.22  WW  Optionales COM-Tracelog erweitert für alle GAS-X-relevanten    }
{               Responses                                                      }
{ 24.08.23  WW  Responsedata für MRG-Messwerte für optionale, erweiterte Kanäle}
{******************************************************************************}
unit DecodeReq;

INTERFACE

uses
  Forms, SysUtils, Classes, EncdDecd,
  GD_Utils, T_Tools, T_Zeit, WStrUtils, WChars, RespConst, LGZType, MObjMeld, MObjPara,
  DDELList, DListen, DALKonv, AbrufCmd, WSysCon, WXmlConst, WStream, MLGZKonvList,
  AbrufConst, DDfueParaList, UnixDT, ErrConst, ErrPrc32;

  { Format für Antwort an Client:

    HTTP/1.1 200 OK
    Server: HOST XML-RPC 1.0
    Connection: close
    Content-Type: text/xml
    Content-Length: LAENGE
    <?xml version="1.0" encoding="ISO-8859-1"?>
    <methodResponse>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>result</name>
                <value>OK</value>
              </member>
              <member>
                <name>wieser_answer</name>          -> für GAS-X: hps_answer
                <value>RESPONSE</value>
              </member>
              <member>
                <name>wieser_data</name>            -> für GAS-X: hps_data
                <value>RESPONSEDATA</value>
              </member>
              <member>
                <name>wieser_log</name>             -> nur für Wieser-System
                <value>RESPONSELOG</value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodResponse>

  }

const
  ext_Req_Log = '_req.log';  // Filenamen-Erweiterung für Request-Log-Dateien
  ext_Req_Xml = '_req.xml';  // Filenamen-Erweiterung für Request-XML-Dateien
  ext_Res_Xml = '_res.xml';  // Filenamen-Erweiterung für Response-XML-Dateien

type

  TEnCodeXMLRESP = class(TObject)
  private
    VersionTyp: TVersionTyp;  // Gas-X-Version oder Wieser-Version
    Head_FIX:String;          // Vorbelegung für SOAP Header
    XML_FIX_START:String;     // Vorbelegung für XML Anfang
    XML_FIX_END:String;       // Vorbelegung für XML Ende
    XmlResponse:String;       // XMLRPC-Rahmen der Antwort (ohne SOAP-Header)
    SOAPResponse:String;      // komplette Antwort für Rückgabe an Client (SOAP-Header und XMLRPC-Rahmen)

    DebXML:Boolean;      // Debug xml - File zu Fremdsystem
    DebXML_DIR:string;   // Debug File Pfad
    bISO646: boolean;  // Zeichen-Konvertierung ASCII -> ISO 646 ein/aus
    Function Build_SOAPResponse: string;
    Function GET_Member_Result(Resultstr:string):string;
    Function GET_Member_hpsanswer(Cmd_Ret:string):string;
    Function GET_Member_hpsdata (DataStr: string): string;
    Function GET_Member_hpsdata_MRG(AMRGKennung: string; AMRGTyp: integer): string;
    Function GET_Member_wieserlog (LogStr: string): string;
    Procedure Write_XMLResponse;
    Function GET_Responsedata_MWTA (MwFileNameList: TStringList;
                                    TaFileNameList: TStringList;
                                    MaxMessKanal, MaxTagKanal: integer;
                                    MinMessKanalExt, MaxMessKanalExt: integer;
                                    Tagesende: integer;
                                    MRGKennung:string; MRGTyp:integer;
                                    ResponseTraceLogList: TDSfGDataList;
                                    von, bis: TDateTime;
                                    LGZnorm_Daten: boolean;
                                    VohList_Orig: boolean;
                                    Fehlergruppe, Fehlercode: integer;
                                    sRetFreierText: string;
                                    var Datencode: integer):string;
    Function GET_Responsedata_ME (MeldungsListe: TMeldungsListe;
                                  MRGKennung:string; MRGTyp:integer;
                                  ResponseTraceLogList: TDSfGDataList;
                                  von, bis: TDateTime;
                                  Fehlergruppe, Fehlercode: integer;
                                  sRetFreierText: string;
                                  var Datencode: integer):string;
    function Get_Responselog_VerbInfoData (VerbInfoData: TVerbInfoData): string;
    function Get_Responsedata_Rohdaten (sEAdr: string;
      ResponseRohdatenList: TDSfGDataList; bMitDEAdr: boolean): string;
    function Get_Responsedata_TraceLog (sEAdr: string;
      ResponseTraceLogList: TDSfGDataList): string;
    function Get_ResponseTraceLog_BusadressenUnerledigt (
      ResponseTraceLogList: TDSfGDataList): string;
    function Get_Responsedata_ReturnExt (iFehlergruppe, iFehlercode: integer;
      sFreierText: string): string;
  public
    Constructor Create (ADebXML: boolean; ADebXML_DIR: string; AISO646: boolean);
    Function GET_XML_Response(ResultStr,Cmd_Ret:String;
                              ResponseTraceLogList: TDSfGDataList;
                              Fehlergruppe, Fehlercode: integer;
                              sRetFreierText: string): string;
    Function GET_XML_VI_Response (ResultStr,Cmd_Ret: string;
                                  VerbInfoData: TVerbInfoData;
                                  Fehlergruppe, Fehlercode: integer;
                                  sRetFreierText: string): string;
    Function GET_XML_VerbAufbauMRG_Response(resultStr,Cmd_Ret: string;
                                            VerbAutoDetectData: TVerbAutoDetectData;
                                            VerbInfoData: TVerbInfoData;
                                            MRGKennung: string; MRGTyp:integer;
                                            ResponseTraceLogList: TDSfGDataList;
                                            Fehlergruppe, Fehlercode: integer;
                                            sRetFreierText: string): string;
    Function GET_XML_VerbAufbauDSfG_Response (resultStr,Cmd_Ret: string;
                                              AufmTelegrammList: TAufmTelegrammList;
                                              DSfGDfueKonfigData: TDSfGDfueKonfigData;
                                              ZeitSyncInfoData: TZeitSyncInfoData;
                                              ZS_Fehlergruppe, ZS_Fehlercode: integer;
                                              ResponseLogList: TResponseLogList;
                                              ResponseTraceLogList: TDSfGDataList;
                                              FwUpdateInfoData: TFwUpdateInfoData;
                                              VerbInfoData: TVerbInfoData;
                                              Fehlergruppe, Fehlercode: integer;
                                              sRetFreierText: string): string;
    Function GET_XML_MWTA_Response(resultStr,Cmd_Ret: string;
                                   MwFileNameList: TStringList;
                                   TaFileNameList: TStringList;
                                   MaxMessKanal, MaxTagKanal: integer;
                                   MinMessKanalExt, MaxMessKanalExt: integer;
                                   Tagesende: integer;
                                   MRGKennung:string; MRGTyp:integer;
                                   ResponseTraceLogList: TDSfGDataList;
                                   von, bis: TDateTime;
                                   LGZnorm_Daten: boolean;
                                   Fehlergruppe, Fehlercode: integer;
                                   sRetFreierText: string): string;
    Function GET_XML_ME_Response(resultStr,Cmd_Ret:string;MeldungsListe: TMeldungsListe;
                                 MRGKennung:string; MRGTyp:integer;
                                 ResponseTraceLogList: TDSfGDataList;
                                 von, bis: TDateTime;
                                 Fehlergruppe, Fehlercode: integer;
                                 sRetFreierText: string):string;
    Function GET_XML_PA_Response(resultStr,Cmd_Ret:string;ParameterListe: TParameterListe;
                                 MRGKennung:string; MRGTyp:integer;
                                 ResponseTraceLogList: TDSfGDataList;
                                 Fehlergruppe, Fehlercode: integer;
                                 sRetFreierText: string):string;
    Function GET_XML_PR_Response(resultStr,Cmd_Ret,PrFileName:String;
                                 MRGKennung:string; MRGTyp:integer;
                                 von, bis: TDateTime;
                                 Fehlergruppe, Fehlercode: integer;
                                 sRetFreierText: string):string;
    Function GET_XML_AR_Response(resultStr,Cmd_Ret:string;
                                 DSfGArchivDataList: TDSfGDataList;
                                 ResponseLogList: TResponseLogList;
                                 ResponseRohdatenList: TDSfGDataList;
                                 ResponseTraceLogList: TDSfGDataList;
                                 von, bis: TDateTime;
                                 mit_Zeitzone: boolean;
                                 Rufcode: integer;
                                 Fehlergruppe, Fehlercode: integer;
                                 sRetFreierText: string):string;
    Function GET_XML_LB_Response(resultStr,Cmd_Ret:string;
                                 DSfGLogbuchDataList: TDSfGDataList;
                                 ResponseLogList: TResponseLogList;
                                 ResponseRohdatenList: TDSfGDataList;
                                 ResponseTraceLogList: TDSfGDataList;
                                 von, bis: TDateTime;
                                 mit_Zeitzone: boolean;
                                 Rufcode: integer;
                                 Fehlergruppe, Fehlercode: integer;
                                 sRetFreierText: string):string;
    Function GET_XML_DE_Response(resultStr,Cmd_Ret:string;
                                 DELList: TDELList;
                                 ResponseLogList: TResponseLogList;
                                 ResponseRohdatenList: TDSfGDataList;
                                 ResponseTraceLogList: TDSfGDataList;
                                 Fehlergruppe, Fehlercode: integer;
                                 sRetFreierText: string):string;
    Function GET_XML_DSfG_Busanalyse_Response(resultStr,Cmd_Ret:string;
                                              DELList: TDELList;
                                              Fehlergruppe, Fehlercode: integer;
                                              sRetFreierText: string):string;
    Function GET_XML_DSfGDfue_Mom_Response(resultStr,Cmd_Ret:string;
                                           DSfGDfueParaList: TDfueParaList;
                                           DSfGDfue_EAdr: string;
                                           ResponseTraceLogList: TDSfGDataList;
                                           Fehlergruppe, Fehlercode: integer;
                                           sRetFreierText: string):string;
    Function GET_XML_ZeitSync_Response(resultStr,Cmd_Ret: string;
                                       ZeitSyncInfoData: TZeitSyncInfoData;
                                       MRGKennung:string; MRGTyp:integer;
                                       ResponseTraceLogList: TDSfGDataList;
                                       Fehlergruppe, Fehlercode: integer;
                                       sRetFreierText: string):string;
    Function GET_XML_Parametrieren_Response (resultStr,Cmd_Ret:string;
                                             ParaEinstellResultData: TParaEinstellResultData;
                                             MRGKennung:string; MRGTyp:integer;
                                             DSfGDfue_EAdr: string;
                                             ResponseTraceLogList: TDSfGDataList;
                                             Fehlergruppe, Fehlercode: integer;
                                             sRetFreierText: string):string;
    Function GET_XML_Transparent_Response (resultStr,Cmd_Ret:string;
                                           TransparentAntw: string;
                                           Fehlergruppe, Fehlercode: integer;
                                           sRetFreierText: string):string;
    Function GET_XML_Ruf_ME_VI_Response (resultStr,Cmd_Ret: string;
                                         Rufcode: integer;
                                         Meldungsliste: TMeldungsliste;
                                         VerbInfoData: TVerbInfoData;
                                         MRGKennung:string; MRGTyp:integer;
                                         Fehlergruppe, Fehlercode: integer;
                                         sRetFreierText: string):string;
    Function GET_XML_SMS_MWTA_Response(resultStr,Cmd_Ret: string;
                                       Rufcode: integer;
                                       MwFileName: string;
                                       TaFileName: string;
                                       MaxMessKanal, MaxTagKanal: integer;
                                       Tagesende: integer;
                                       MRGKennung:string; MRGTyp:integer;
                                       Fehlergruppe, Fehlercode: integer;
                                       sRetFreierText: string):string;
    Function GET_XML_Rufliste_Response (resultStr,Cmd_Ret:string; Rufliste:string;
                                        MRGKennung:string; MRGTyp:integer;
                                        Fehlergruppe, Fehlercode: integer;
                                        sRetFreierText: string):string;
    Function GET_XML_RufannahmeDSfG_Response (resultStr,Cmd_Ret: string;
                                              AufmTelegrammList: TAufmTelegrammList;
                                              RDeakt_RufNrZentrale_Alt: string;
                                              RDeakt_Fehlergruppe,
                                              RDeakt_Fehlercode: integer;
                                              DSfGDfue_EAdr: string;
                                              VerbInfoData: TVerbInfoData;
                                              Fehlergruppe, Fehlercode: integer;
                                              sRetFreierText: string): string;
    function GET_XML_ZeitAbruf_Response (resultStr,Cmd_Ret: string;
      sKennung, sUnixTime, sTimeBias, sTimeInfo: string;
      ResponseTraceLogList: TDSfGDataList;
      Fehlergruppe, Fehlercode: integer;
      sRetFreierText: string): string;  // 17.08.2010, 13.01.2011
  end;


  TDecodeXMLREQ = class(TObject)
  private
    REQ: String;
    DebXML:Boolean;      // Debug xml - File zu Fremdsystem
    DebXML_DIR:string;   // Debug File Pfad

    Procedure Write_XMLRequest;
  public
    Constructor Create(ADebXML:Boolean;ADebXML_DIR:string);
    procedure SetRequest (ARequest: string);
    function isCompleteXML (ARequest: string): boolean;
    Function isCorrectXML (sBasicAuthentication: string; var sErrInfo: string): integer;
    Function GetCmdRequest:String;
    Function CreateRequestData_MRG_StaKanalKonvDataList: TStaKanalKonvDataList;
  end;

implementation

Const
  ERR_LENGTH        = -1;    // Fehler: xml-Telegrammlänge Soll/Ist-Vergleich
  ERR_LENGTH_HEADER = -2;    // Fehler: xml-Telegramm, Soll-Länge Header
  ERR_BASICAUTHENTICATION = -3;  // Fehler: xml-Telegramm, Basis-Authentifizierung Header

  C_ListStrLen = 1024;    // max. String-Länge zum Zwischenpuffern in temporärer Stringliste


{ TEnCodeXMLRESP }

{------------------------------------------------------------------------------------------}
Constructor TEnCodeXMLRESP.Create (ADebXML: boolean; ADebXML_DIR: string; AISO646: boolean);
{------------------------------------------------------------------------------------------}
begin
  inherited Create;
  DebXML:=ADebXML;
  DebXML_DIR:=ADebXML_DIR;
  bISO646:=AISO646;

{$IFDEF GAS-X}
  VersionTyp:=vs_GasX;
{$ELSE}
  VersionTyp:=vs_Wieser;
{$ENDIF}

  Head_FIX:='HTTP/1.1 200 OK Server: HOST XML-RPC 1.0 Connection: close Content-Type: text/xml Content-Length: ';
  XML_FIX_START:='<?xml version="1.0" encoding="ISO-8859-1"?><methodResponse><params><param><value><struct>';
  XML_FIX_END:='</struct></value></param></params></methodResponse>';
  XMLResponse:='';
  SOAPResponse:='';
end;

{-------------------------------------------------}
Function TEnCodeXMLRESP.Build_SOAPResponse: string;
{-------------------------------------------------}
Var
  Laenge:Integer;
begin
  Laenge:=Length(XMLResponse);
  Result:=Head_FIX+IntToStr(Laenge)+XMLResponse;
end;

{-----------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_Member_Result(Resultstr:String):string;
{-----------------------------------------------------------------}
begin
  Result:='<member><name>result</name><value>'+ResultStr+'</value></member>';
end;

{------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_Member_hpsanswer(Cmd_Ret:string):string;
{------------------------------------------------------------------}
Var
  S: string;
begin
  case VersionTyp of
    vs_GasX: S:='hps_answer';
  else
    S:='wieser_answer';
  end;
  Result:='<member><name>' + S + '</name><value>' +
          EncodeXml (cmd_Ret) +  // XML-Ersatzzeichen codieren; 03.06.2020, WW
          '</value></member>';
end;

{-------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_Member_hpsdata (DataStr: string): string;
{-------------------------------------------------------------------}
var
  S: string;
begin
  case VersionTyp of
    vs_GasX: S:='hps_data';
  else
    S:='wieser_data';
  end;
  Result:='<member><name>' + S + '</name><value>'+CR+LF+ DataStr +CR+LF+'</value></member>';
end;

{--------------------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_Member_hpsdata_MRG(AMRGKennung: string; AMRGTyp: integer): string;
{--------------------------------------------------------------------------------------------}
var
  S: string;
begin
  case VersionTyp of
    vs_GasX: S:='hps_data';
  else
    S:='wieser_data';
  end;
  Result:='<member><name>' + S + '</name><value>'+CR+LF+
          '&lt;wieser&gt;'+CR+LF+'&lt;mrg ident=&quot;'+AMRGKennung+'&quot; ' +
          'type=&quot;'+IntToStr(AMRGTyp)+'&quot;&gt;';
end;

{--------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_Member_wieserlog (LogStr: string): string;
{--------------------------------------------------------------------}
var
  S: string;
begin
  case VersionTyp of
    vs_GasX: S:='';   // für Gas-X keine Response-Log-Ausgabe
  else
    S:='<member><name>wieser_log</name><value>'+CR+LF+ LogStr +CR+LF+'</value></member>';
  end;
  Result:=S;
end;

{---------------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_Responsedata_MWTA (MwFileNameList: TStringList;
                                               TaFileNameList: TStringList;
                                               MaxMessKanal, MaxTagKanal: integer;
                                               MinMessKanalExt, MaxMessKanalExt: integer;
                                               Tagesende: integer;
                                               MRGKennung:string; MRGTyp:integer;
                                               ResponseTraceLogList: TDSfGDataList;
                                               von, bis: TDateTime;
                                               LGZnorm_Daten: boolean;
                                               VohList_Orig: boolean;
                                               Fehlergruppe, Fehlercode: integer;
                                               sRetFreierText: string;
                                               var Datencode: integer):string;
{---------------------------------------------------------------------------------------}
{ setzt Responsedata für MRG-Messwerte/Tagessätze zusammen und liefert den
  Datencode zurück;
  -> Der Übergabe-Schalter 'LGZnorm_Daten' bestimmt, ob die in den MW-Dateien enthaltenen
     Original- (RohSRec.KanalOrig) oder die normierten LGZ-Messwerte (RohSRec.Kanal)
     zurückgegeben werden (Ausnahme: wenn VersionTyp = vs_GasX werden immer die MW-Originalwerte geliefert)
  -> Bei gesetztem Übergabe-Schalter 'VohList_Orig' werden zusätzlich die Originalwerte
     unter dem gleichnamigen Tag zurückgegeben (-> für SMS-Daten mit berechneten
     Stundenmengen in RohSRec.Kanal, LGZnorm_Daten = true !) }
Var
  FS: TFileOfRecStream;
  FRecCount: integer;
  mrglist,mvlist,DTStr,tmp: string;
  dt:TDateTime;
  OK: boolean;
  ListBuf: TStringList;   { Zwischenpuffer um lange Strings (vohlist, mvlist) zu
                            vermeiden (ab 1 MB XML-Ausgabe wird es sonst sehr langsam !) }


  {----------------------------------------------------------------------}
  function Write_Vohs_Kanaele (iKanalVon, iKanalBis: integer; mw: RohSRec;
    bReadFromKanalOrig: boolean): string;
  {----------------------------------------------------------------------}
  { XML-Voh-Elemente zusammenstellen }
  var
    voh: string;
    i: integer;
    s_wert: string;
    s_kanalstatus: string;
    s_ordnr: string;
    s_xml_ordnr: string;

  begin
    Result:='';
    for i:=iKanalVon to iKanalBis do begin
      if (i >= 1) AND (i <= C_maxKanalZahl) then begin  // 24.08.2023, WW
        s_xml_ordnr:='';  // Default: Attribut mit Ordnungsnummer nicht in XML-Response ausgeben
        if bReadFromKanalOrig then begin
          // Ausgabe der Original-Kanaldaten
          s_wert:=FloatToStr(mw.kanalorig[i].Wert);
          StrSubst(s_wert, ',', '.');  { Ausgabe fest mit Dezimal-Punkt, 07.07.2003  WW }
          s_kanalstatus:=IntToStr(mw.kanalorig[i].KanalStatus);

          // Bei Gas-X-Version Ordnungsnummer ausgeben; 28.09.2020, WW:
          if (VersionTyp = vs_GasX) then begin
            // Ordnungsnummer nur ausgeben, wenn vom Gerät unterstützt
            // (d.h. ungleich Defaultwert -1), ansonsten entfällt Attribut
            if mw.kanalorig[i].OrdNr <> -1 then begin
              s_ordnr:=IntToStr(mw.kanalorig[i].OrdNr);
              s_xml_ordnr:=' ordinal=&quot;'+s_ordnr+'&quot;';
            end;
          end;
        end
        else begin
          // Ausgabe der LGZ-Daten
          s_wert:=IntToStr (mw.kanal[i].Wert);
          s_kanalstatus:=IntToStr(mw.kanal[i].KanalStatus);
        end;

        voh:='&lt;voh cid=&quot;'+IntToStr(i)+'&quot;'+
             s_xml_ordnr +
             ' state=&quot;'+s_kanalstatus+'&quot;'+
             '&gt;'+s_wert+'&lt;/voh&gt;'+CR+LF;
        Result:=Result+voh;
      end;
    end;  // for i
  end;

  {---------------------------------------------------------------------}
  procedure Write_VohList (ListTag: string; bReadFromKanalOrig: boolean);
  {---------------------------------------------------------------------}
  { XML-VohList zusammenstellen }
  var
    j: integer;
    MwFileName: string;
    mw: RohSRec;
    vohlist: string;
    s_vohs: string;

  begin
    vohlist:=CR+LF+'&lt;' + ListTag + '&gt;'+CR+LF;
    for j:=0 to MwFileNameList.Count - 1 do begin
      { strukturierte Datei mit Messwerten öffnen: }
      MwFileName:=MwFileNameList[j];
      if FileExists(MWFileName) then begin
        try
          FS:=TFileOfRecStream.Create (MwFilename, fmOpenRead OR fmShareDenyWrite, SizeOf (mw));
          try
            FRecCount:=FS.RecCount;
            if FRecCount > 0 then begin
              if (Datencode AND dc_Messwerte) = 0 then
                Datencode:=Datencode + dc_Messwerte;
              while FS.RecPosition < FRecCount do begin
                Application.ProcessMessages;
                FS.ReadRec (mw);
                try
                  DTStr:=FormatDatetime('yyyymmddhhnnss',mw.DatumZeit);
                  OK:=true;
                  if VersionTyp = vs_GasX then begin
                    { wenn Zeitfilter aktiviert:
                      Prüfung ob Datensatz-Zeitstempel innerhalb des
                      zurückzugebenden Zeitbereichs liegt }
                    if (von > -1) AND (bis > -1) then begin
                      if (CmpDateTime (mw.DatumZeit, von) < 0) OR
                         (CmpDateTime (mw.DatumZeit, bis) > 0) then
                        OK:=false;
                    end;
                  end;
                except
                  OK:=false;  { sicherheitshalber abfangen, man weiß ja nie... }
                end;

                if OK then begin
                  vohlist:=vohlist+'&lt;vohset ts=&quot;'+DTStr+'&quot; state=&quot;'+inttostr(mw.SatzStatus)+'&quot;&gt;'+CR+LF;
                  { voh's für Kanäle 1 bis MaxMessKanal bilden (Standard): }
                  s_vohs:=Write_Vohs_Kanaele(1, MaxMessKanal, mw, bReadFromKanalOrig);
                  { Ggf. zusätzlich voh's für erweiterte Kanäle bilden; 24.08.2023, WW}
                  if (MinMessKanalExt > 0) AND (MaxMessKanalExt > 0) then
                    s_vohs:=s_vohs +
                      Write_Vohs_Kanaele(MinMessKanalExt, MaxMessKanalExt, mw, bReadFromKanalOrig);
                  vohList:=vohList+s_vohs+'&lt;/vohset&gt;'+CR+LF;
                end;

                if length (vohlist) > C_ListStrLen then begin
                  ListBuf.Add (vohlist);       { vohlist zwischenpuffern }
                  vohlist:='';
                end;
              end;  { while FS.RecPosition < FRecCount }
            end;  { if FRecCount > 0 }
          finally
            FS.Free;
          end;
        except
        end;
      end;
    end; // for MwFileNameList.Count
    vohList:=vohList+'&lt;/' + ListTag + '&gt;';
    ListBuf.Add (vohlist);
  end;

var
  bReadFromKanalOrig: boolean;
  i,j:integer;
  TaFileName: string;
  ta : RohTRec;
  Tagesende_Rec: TimeRec;
  TimeBuf: TimeRec;
  s_wert_E: string;
  s_wert_K: string;
  sReturnExt: string;
  sTraceLog: string;

begin
  with Tagesende_Rec do begin  // für Gastag-Rückrechnung bei Zählerständen
    hour:=Tagesende;
    min:=0;
    sec:=0;
    hsec:=0;
  end;

  ListBuf:=TStringList.Create;
  try
    ListBuf.Sorted:=false;

    mrglist:='';
    Datencode:=dc_KeineDaten;
    if MwFileNameList <> nil then begin
      if MwFileNameList.Count > 0 then begin
        // Standard: Tag 'vohlist' immer schreiben
        bReadFromKanalOrig:=(VersionTyp = vs_GasX) OR not LGZnorm_Daten;  // true: Messwerte aus RohSRec.KanalOrig zurückgeben
        Write_VohList ('vohlist', bReadFromKanalOrig);
        // Tag 'vohlist_orig' nur für Wieser-Version:
        if (VersionTyp = vs_Wieser) AND VohList_Orig then
          Write_VohList ('vohlist_orig', true);
      end;  // if MwFileNameList.Count > 0
    end; // if MwFileNameList <> nil

    mvlist:='';
    { keine Tagessatz-Ausgabe bei MRG-Typen 'KE' (die stündlichen Zählerstände stehen
      schon im Stundenwertfile)
      -> Da die KE-Typen keine Gas-X-spezifischen Gerätetypnummern tragen, kann hier
         auf die Umwandlung in die Wieser-spezifische Gerätetypnummer verzichtet werden. }
    if (TaFileNameList <> nil) AND
       (MRGTyp <> mrgtyp_KE_Ruhrgas) AND (MRGTyp <> mrgtyp_KE_PPN) then begin
      if TaFileNameList.Count > 0 then begin
        mvList:=CR+LF+'&lt;mvlist&gt;'+CR+LF;
        for j:=0 to TaFileNameList.Count - 1 do begin
          { strukturierte Datei mit Tagessätzen öffnen: }
          TaFileName:=TaFileNameList[j];
          if FileExists(TaFileName) then begin
            try
              FS:=TFileOfRecStream.Create (TaFilename, fmOpenRead OR fmShareDenyWrite, SizeOf (ta));
              try
                FRecCount:=FS.RecCount;
                if FRecCount > 0 then begin
                  if (Datencode AND dc_Tagessaetze) = 0 then
                    Datencode:=Datencode + dc_Tagessaetze;
                  while FS.RecPosition < FRecCount do begin
                    Application.ProcessMessages;
                    FS.ReadRec (ta);
                    try
                      dt:=ta.DatumZeit;
                      DecodeTime (dt, word (TimeBuf.hour), word (TimeBuf.min),
                                      word (TimeBuf.sec), word (TimeBuf.hsec));
                      { Umrechnung Gastag -> physikalischer Tag:
                        -> nicht umrechnen, wenn Tagesende < 0 übergeben wurde; 11.11.2004 WW }
                      if (Tagesende > -1) AND (CmpTime (TimeBuf, Tagesende_Rec) <= 0) then
                        dt:=dt + 1;
                      DTStr:=FormatDatetime('yyyymmddhhnnss',dt);
                      OK:=true;
                      if VersionTyp = vs_GasX then begin
                        { wenn Zeitfilter aktiviert:
                          Prüfung ob physikalischer Datensatz-Zeitstempel innerhalb des
                          zurückzugebenden Zeitbereichs liegt }
                        if (von > -1) AND (bis > -1) then begin
                          if (CmpDateTime (dt, von) < 0) OR
                             (CmpDateTime (dt, bis) > 0) then
                            OK:=false;
                        end;
                      end;
                    except
                      OK:=false;  { sicherheitshalber abfangen, man weiß ja nie... }
                    end;

                    if OK then begin
                      mvlist:=mvlist+'&lt;mvset ts=&quot;'+DTStr+'&quot; state=&quot;'+
                              inttostr(ta.SatzStatus)+'&quot;&gt;'+CR+LF;
                      for i:=1 to MaxTagKanal do begin
                        if (i <= C_maxKanalZahl) then begin  // 24.08.2023, WW
                          s_wert_E:=floattostr(ta.E_Zaehler[i].Wert);  // Rundung raus; 27.03.2013, WW
                          StrSubst(s_wert_E, ',', '.');  { Ausgabe fest mit Dezimal-Punkt }
                          s_wert_K:=floattostr(ta.K_Zaehler[i].Wert);  // Rundung raus; 27.03.2013, WW
                          StrSubst(s_wert_K, ',', '.');  { Ausgabe fest mit Dezimal-Punkt }

                          tmp:='&lt;mv cid=&quot;'+inttostr(i)+'&quot; '+
                               'ic=&quot;'+s_wert_E+'&quot; '+
                               'cc=&quot;'+s_wert_K+'&quot;';
                          // Zählerstati nur für Wieser-System:
                          if VersionTyp = vs_Wieser then
                            tmp:=tmp + ' icstate=&quot;'+inttostr(ta.E_Zaehler[i].zaehlerstatus)+'&quot; '+
                                       'ccstate=&quot;'+inttostr(ta.K_Zaehler[i].zaehlerstatus)+'&quot;';
                          tmp:=tmp + '/&gt;'+CR+LF;
                          mvList:=mvList+tmp;
                        end;
                      end;  // for i
                      mvlist:=mvlist+'&lt;/mvset&gt;'+CR+LF;
                    end;

                    if length (mvlist) > C_ListStrLen then begin
                      ListBuf.Add (mvlist);          { mvlist zwischenpuffern }
                      mvlist:='';
                    end;
                  end;  { while FS.RecPosition < FRecCount }
                end;  { if FRecCount > 0 }
              finally
                FS.Free;
              end;
            except
            end;
          end;
        end; // for TaFileNameList.Count
        mvList:=mvList+'&lt;/mvlist&gt;'+CR+LF;
        ListBuf.Add (mvlist);

      end;  // if TaFileNameList.Count > 0
    end; // if TaFileNameList <> nil

    { TraceLog: 03.01.2022, WW }
    sTraceLog:=Get_Responsedata_TraceLog ('', ResponseTraceLogList);
    if length (sTraceLog) > 0 then
      ListBuf.Add (sTraceLog);

    if (ListBuf.Count > 0) then begin
      mrglist:=GET_Member_hpsdata_MRG(MRGKennung, MRGTyp);
      for i:=0 to ListBuf.Count-1 do
        mrglist:=mrglist + ListBuf[i];
      mrglist:=mrglist+'&lt;/mrg&gt;'+CR+LF+'&lt;/wieser&gt;';
    end;

    sReturnExt:='';
    if VersionTyp = vs_GasX then begin
      // Responsedata für Gas-X-System:

      // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
      sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
    end;

    if mrglist <> '' then begin
      if sReturnExt <> '' then
        mrglist:=mrglist+CR+LF+sReturnExt;

      mrglist:=mrglist+CR+LF+'</value></member>';
    end
    else begin
      if sReturnExt <> '' then
        mrglist:=GET_Member_hpsdata(sReturnExt);
    end;
  finally
    ListBuf.Free;
  end;

  Result:=mrglist;
end;

{-----------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_Responsedata_ME (MeldungsListe: TMeldungsListe;
                                             MRGKennung:string; MRGTyp:integer;
                                             ResponseTraceLogList: TDSfGDataList;
                                             von, bis: TDateTime;
                                             Fehlergruppe, Fehlercode: integer;
                                             sRetFreierText: string;
                                             var Datencode: integer):string;
{-----------------------------------------------------------------------------}
{ setzt Responsedata für MRG-Meldungen zusammen und liefert den Datencode zurück }
Var
  msgList,mrglist: string;
  i:integer;
  Meldung: TMeldung;
  ParaChange: TParaChange;
  dt:TDateTime;
  DatumZeitStr:String;
  OK: boolean;
  sReturnExt: string;
  s_ordnr: string;
  s_xml_ordnr: string;
  sTraceLog: string;

begin
  mrglist:='';
  msglist:='';
  Datencode:=dc_KeineDaten;
  if MeldungsListe<>Nil then begin
    if Meldungsliste.Count>0 then begin
      Datencode:=dc_Meldungen;
      msglist:=CR+LF+'&lt;msglist&gt;'+CR+LF;
      for i:=0 to MeldungsListe.count -1 do begin
        Application.ProcessMessages;
        Meldung := TMeldung(MeldungsListe[i]);
        try
          with Meldung do begin
            dt:=EncodeDate(Jahr,Monat,Tag);
            dt:=dt + EncodeTime(Stunde,Minute,Sekunde,0);
            DatumZeitStr:=FormatDatetime('yyyymmddhhnnss',dt);
          end;
          OK:=true;
          if VersionTyp = vs_GasX then begin
            { wenn Zeitfilter aktiviert:
              Prüfung ob Datensatz-Zeitstempel innerhalb des
              zurückzugebenden Zeitbereichs liegt }
            if (von > -1) AND (bis > -1) then begin
             if (CmpDateTime (dt, von) < 0) OR (CmpDateTime (dt, bis) > 0) then
               OK:=false;
            end;
          end;
        except
          OK:=false;   { sicherheitshalber abfangen, man weiß ja nie... }
        end;

        if OK then begin
          s_xml_ordnr:='';  // Default: Attribut mit Ordnungsnummer nicht in XML-Response ausgeben

          // Bei Gas-X-Version Ordnungsnummer ausgeben; 28.09.2020, WW:
          if (VersionTyp = vs_GasX) then begin
            // Ordnungsnummer nur ausgeben, wenn vom Gerät unterstützt
            // (d.h. ungleich Defaultwert -1), ansonsten entfällt Attribut
            if Meldung.OrdNr <> -1 then begin
              s_ordnr:=IntToStr(Meldung.OrdNr);
              s_xml_ordnr:=' ordinal=&quot;'+s_ordnr+'&quot;';
            end;
          end;

          msgList:=msglist+'&lt;msg ts=&quot;'+DatumZeitStr+'&quot;'+
                   s_xml_ordnr +
                   ' mcode=&quot;'+Meldung.NrAllg+'&quot;';
          // MRG-Meldungsnummer und Parameteränderungen nur für Wieser-System:
          if VersionTyp = vs_Wieser then begin
            msglist:=msglist + ' mrgmcode=&quot;'+Meldung.Vz+Meldung.NrMrg+'&quot;';
            { wenn Parameteränderung: }
            ParaChange:=Meldung.GetParaChange;
            if ParaChange <> nil then
              msglist:=msglist+' pch_parid=&quot;'+ParaChange.NrAllg+'&quot; '+
                               'pch_mrgparid=&quot;'+ParaChange.NrMrg+'&quot; '+
                               'pch_oldval=&quot;'+ParaChange.OldValue+'&quot; '+
                               'pch_newval=&quot;'+ParaChange.NewValue+'&quot;';
          end;
          msgList:=msglist+'/&gt;'+CR+LF;
        end;
      end; // for Meldungsliste.count
      msglist:=msglist+'&lt;/msglist&gt;'+CR+LF;
    end;  // if Meldungsliste.Count>0
  end; // if MeldungsListe<>Nil

  { TraceLog: 03.01.2022, WW }
  sTraceLog:=Get_Responsedata_TraceLog ('', ResponseTraceLogList);
  if length (sTraceLog) > 0 then
    msgList:=msgList + sTraceLog;

  if length (msgList) > 0 then begin  // 03.01.2022, WW
    mrglist:=GET_Member_hpsdata_MRG(MRGKennung, MRGTyp);
    mrglist:=mrglist+msgList;
    mrglist:=mrglist+'&lt;/mrg&gt;'+CR+LF+'&lt;/wieser&gt;';
  end;

  sReturnExt:='';
  if VersionTyp = vs_GasX then begin
    // Responsedata für Gas-X-System:

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
  end;

  if mrglist <> '' then begin
    if sReturnExt <> '' then
      mrglist:=mrglist+CR+LF+sReturnExt;

    mrglist:=mrglist+CR+LF+'</value></member>';
  end
  else begin
    if sReturnExt <> '' then
      mrglist:=GET_Member_hpsdata(sReturnExt);
  end;

  Result:=mrglist;
end;

{-----------------------------------------------------------------------------------------}
function TEnCodeXMLRESP.Get_Responselog_VerbInfoData (VerbInfoData: TVerbInfoData): string;
{-----------------------------------------------------------------------------------------}
{ Bildet Responselog für Verbindungsinformationen }
var
  LogStr: string;

begin
  LogStr:=C_ConnInfoBlockStart + CR + LF;
  with VerbInfoData do begin
  if DZ_VerbindungSteht > 0 then
    LogStr:=LogStr +
            C_DZ_ConnectStart + FormatDateTime ('yyyymmddhhnnss', DZ_VerbindungSteht) +
            C_DZ_ConnectEnd + CR + LF;
  if DZ_Login > 0 then
    LogStr:=LogStr +
            C_DZ_LoginStart + FormatDateTime ('yyyymmddhhnnss', DZ_Login) +
            C_DZ_LoginEnd + CR + LF;
  end;
  LogStr:=LogStr + C_ConnInfoBlockEnd;
  Result:=LogStr;
end;

{-----------------------------------------------------------------}
function TEnCodeXMLRESP.Get_Responsedata_Rohdaten (sEAdr: string;
  ResponseRohdatenList: TDSfGDataList; bMitDEAdr: boolean): string;
{-----------------------------------------------------------------}
{ Bildet Responsedata für Rohdaten }
var
  RohStr: string;
  i: integer;
  sData: string;
  sFilename: string;
  ResponseRohdatenObj: TResponseRohdatenObj;

begin
  RohStr:='';
  if Assigned (ResponseRohdatenList) then begin
    for i:=0 to ResponseRohdatenList.Count-1 do begin
      ResponseRohdatenObj:=TResponseRohdatenObj (ResponseRohdatenList.Objects [i]);
      if ResponseRohdatenObj.EAdr = sEAdr then begin  // Filter auf Busadresse
        sFilename:=ResponseRohdatenList[i];  // Name der Datei mit kodierten Rohdaten
        if FileExists (sFilename) then begin
          sData:=StringFromFile (sFilename);
          RohStr:=RohStr + C_RawDataStart;
          if bMitDEAdr then  // wenn DE-Adresse ausgegeben werden soll
            RohStr:=RohStr + ' ' + C_DSfGKennungDEA +
              C_quot_Subst + ResponseRohdatenObj.DEL + C_quot_Subst;
          RohStr:=RohStr + ' ' + C_RawDataEncode +
            C_quot_Subst + IntToStr (ResponseRohdatenObj.Encode) + C_quot_Subst +
            C_gt_Subst + sData + C_RawDataEnd + CR + LF;
        end;
      end;
    end;  { for i }
    if length (RohStr) > 0 then
      RohStr:=C_RawDataListStart + CR + LF + RohStr +
              C_RawDataListEnd + CR + LF;
  end;  // if Assigned (ResponseRohdatenList)
  Result:=RohStr;
end;

{---------------------------------------------------------------}
function TEnCodeXMLRESP.Get_Responsedata_TraceLog (sEAdr: string;
  ResponseTraceLogList: TDSfGDataList): string;
{---------------------------------------------------------------}
{ Bildet Responsedata für Tracelog (Base64-codiert)
  -> EAdr: DSfG-Busadresse (oder leer, wenn Tracelog nicht DSfG-Instanz-spezifisch) }
var
  TraceLogStr: string;
  i: integer;
  DSfGDataListObj: TDSfGDataListObj;

begin
  TraceLogStr:='';
  if Assigned (ResponseTraceLogList) then begin
    for i:=0 to ResponseTraceLogList.Count-1 do begin
      DSfGDataListObj:=TDSfGDataListObj (ResponseTraceLogList.Objects [i]);
      if DSfGDataListObj.EAdr = sEAdr then begin  // Filter auf DSfG-Busadresse (auch leer, z.B. bei MRG)
        TraceLogStr:=TraceLogStr + ResponseTraceLogList[i];
        DSfGDataListObj.DEL:='Done';  // Feld DEL als Merker 'erledigt'
      end;
    end;  { for i }

    if length (TraceLogStr) > 0 then
      TraceLogStr:=EncodeString (TraceLogStr);  // Base64-codieren; 03.01.2022, WW

    TraceLogStr:=C_TraceLogStart + TraceLogStr + C_TraceLogEnd + CR + LF;
  end;  // if Assigned (ResponseTraceLogList)
  Result:=TraceLogStr;
end;

{------------------------------------------------------------------}
function TEnCodeXMLRESP.Get_ResponseTraceLog_BusadressenUnerledigt (
  ResponseTraceLogList: TDSfGDataList): string;
{------------------------------------------------------------------}
{ Liefert DSfG-Busadressen der Instanzen, deren Tracelog noch nicht ausgegeben
  wurde;
  Ergebnis: DSfG-Busadressen (z.B. ACI) oder leer }
var
  i: integer;
  DSfGDataListObj: TDSfGDataListObj;

begin
  Result:='';
  if Assigned (ResponseTraceLogList) then begin
    for i:=0 to ResponseTraceLogList.Count-1 do begin
      DSfGDataListObj:=TDSfGDataListObj (ResponseTraceLogList.Objects [i]);
      if DSfGDataListObj.EAdr <> '' then  // DSfG-Busadressen dürfen nicht leer sein
        if DSfGDataListObj.DEL = '' then  // nicht erledigt (Merker in Feld DEL)
          if Pos (DSfGDataListObj.EAdr, Result) = 0 then
            Result:=Result + DSfGDataListObj.EAdr;
    end;  { for i }
  end;  // if Assigned (ResponseTraceLogList)
end;


{------------------------------------------------------------------}
function TEnCodeXMLRESP.Get_Responsedata_ReturnExt (
  iFehlergruppe, iFehlercode: integer; sFreierText: string): string;
{------------------------------------------------------------------}
{ Bildet Responselog für erweiterte Ergebnis-Rückgaben (nur für Gas-X);
  Anmerkung: Die in der erweiterten Ergebnis-Rückgabe enthaltene Statusgruppe
             und -code werden hier unabhängig vom Gas-X-Status gebildet, d.h. es
             könnte lt. Fehlertexte-DLL (bislang nur theoretisch) vorkommen, daß
             ein Gas-X-Status = 0 ("OK"), jedoch erweiterte Statusgruppe und
             -code <> 0 zurückgegeben werden }
var
  iGroup: integer;
  iCode: integer;
  sAddInfo: string;
  iStatus_Prio1: integer;
  iStatus_Prio2: integer;

begin
  // Vorbelegung: Fehlergruppe wie übergeben, keine Zusatzinfo
  iGroup:=iFehlergruppe;
  sAddInfo:='';

  // Sonderfälle: Fehlergruppe/-code-Kombinationen glattbügeln, welche einen
  // OK-Zustand darstellen:
  if (iFehlergruppe = EST_ZEITSYNCERROR) AND
     (iFehlercode = ZSYNCERR_SUCCESS) then begin
    iFehlergruppe:=0;
    iFehlercode:=0;
  end;

  if iFehlergruppe > 0 then begin  // Fehler/Warnung aufgetreten
    // Wenn in der Fehlergruppe Statuscodes für Prioritätsstufe 1 (1..999) UND
    // Prioritätsstufe 2 (1000..99000 in 1000er-Schritten) enthalten sind, wird der
    // Prio1-Status als "Gruppe" und der Prio2-Status als AddInfo-Text zurückgegeben:
    iStatus_Prio1:=iFehlergruppe MOD 1000;             { z.B. 1001 ->      1 }
    iStatus_Prio2:=(iFehlergruppe DIV 1000) * 1000;    { z.B. 1001 ->   1000 }
    // Prio3-Status wird nicht verarbeitet, kommt im WicomSrv nicht vor

    if (iStatus_Prio1 <> 0) AND (iStatus_Prio2 <> 0) then begin
      iGroup:=iStatus_Prio1;
      sAddInfo:=GetStatusText (iStatus_Prio2);
    end;
  end;

  if (sAddInfo <> '') AND (sFreierText <> '') then
    sAddInfo:=sAddInfo + ': ';
  sAddInfo:=sAddInfo + sFreierText;

  { Übergebener Fehlercode wird 1:1 als "Code" zurückgegeben: }
  iCode:=iFehlercode;

  Result:=C_ReturnExtStart +
    C_ReturnExtGroup + C_quot_Subst + IntToStr(iGroup) + C_quot_Subst + ' ' +
    C_ReturnExtCode + C_quot_Subst + IntToStr(iCode) + C_quot_Subst + ' ' +
    C_ReturnExtAddInfo + C_quot_Subst + EncodeXml(sAddInfo) + C_quot_Subst + '/&gt;';
end;


{---------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_Response(ResultStr,Cmd_Ret:String;
                                         ResponseTraceLogList: TDSfGDataList;
                                         Fehlergruppe, Fehlercode: integer;
                                         sRetFreierText: string): string;
{---------------------------------------------------------------------------}
var
  hpsdata: string;
  sReturnExt: string;
  sTraceLog: string;

begin
  hpsdata:='';

  { TraceLog: 03.01.2022, WW }
  sTraceLog:=Get_Responsedata_TraceLog ('', ResponseTraceLogList);

  if VersionTyp = vs_GasX then begin
    // Responsedata für Gas-X-System:

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
    hpsdata:=GET_Member_hpsdata(sTraceLog + sReturnExt);  // mit TraceLog: 03.01.2022, WW
  end;

  XMLResponse:=XML_FIX_Start+
               Get_Member_Result(ResultStr)+
               Get_Member_hpsanswer(Cmd_Ret)+
               hpsdata;
  XMLResponse:=XMLResponse + XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{-----------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_VI_Response (ResultStr,Cmd_Ret: string;
                                             VerbInfoData: TVerbInfoData;
                                             Fehlergruppe, Fehlercode: integer;
                                             sRetFreierText: string): string;
{-----------------------------------------------------------------------------}
{ Liefert XML-Antwort mit Verbindungsinformationen im Responselog-Teil }
var
  LogStr: string;
  hpsdata: string;
  sReturnExt: string;

begin
  hpsdata:='';
  LogStr:='';

  if VersionTyp = vs_Wieser then begin
    // Responselog für Wieser-System:

    // Verbindungsinformationen: 03.12.2013, WW
    LogStr:=Get_Responselog_VerbInfoData (VerbInfoData);
  end
  else begin
    // Responsedata für Gas-X-System:

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
    hpsdata:=GET_Member_hpsdata(sReturnExt);
  end;

  XMLResponse:=XML_FIX_Start+
               Get_Member_Result(ResultStr)+
               Get_Member_hpsanswer(Cmd_Ret)+
               hpsdata;
  if length (LogStr) > 0 then
    XMLResponse:=XMLResponse + GET_Member_wieserlog (LogStr);
  XMLResponse:=XMLResponse + XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{------------------------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_VerbAufbauMRG_Response(resultStr,Cmd_Ret: string;
                                                       VerbAutoDetectData: TVerbAutoDetectData;
                                                       VerbInfoData: TVerbInfoData;
                                                       MRGKennung: string; MRGTyp:integer;
                                                       ResponseTraceLogList: TDSfGDataList;
                                                       Fehlergruppe, Fehlercode: integer;
                                                       sRetFreierText: string): string;
{------------------------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
Var
  connlist,mrglist: string;
  Datencode: integer;
  LogStr: string;
  sReturnExt: string;
  sTraceLog: string;

begin
  connlist:='';
  mrglist:='';
  LogStr:='';
  Datencode:=dc_KeineDaten;

  { TraceLog: 03.01.2022, WW }
  sTraceLog:=Get_Responsedata_TraceLog ('', ResponseTraceLogList);

  // Responsedata, Responselog nur für Wieser-System:
  if VersionTyp = vs_Wieser then begin
    // Responsedata:
    { Wenn automatisch ermittelte Verbindungsparameter vorhanden sind: }
    if (length (VerbAutoDetectData.ModemTyp) > 0) OR
       (VerbAutoDetectData.PasswortNr > -1) then begin
      Datencode:=dc_VerbAutoDetectDaten;
      connlist:=CR+LF+'&lt;connlist&gt;'+CR+LF;
      if length (VerbAutoDetectData.ModemTyp) > 0 then
        connlist:=connlist+'&lt;modemtype&gt;'+VerbAutoDetectData.ModemTyp+'&lt;/modemtype&gt;'+CR+LF;
      if VerbAutoDetectData.PasswortNr > -1 then
        connlist:=connlist+'&lt;pwno&gt;'+IntToStr (VerbAutoDetectData.PasswortNr)+'&lt;/pwno&gt;'+CR+LF;
      connlist:=connlist+'&lt;/connlist&gt;'+CR+LF;

      mrglist:=GET_Member_hpsdata_MRG(MRGKennung, MRGTyp);
      mrglist:=mrglist+connlist;
      mrglist:=mrglist+'&lt;/mrg&gt;'+CR+LF+'&lt;/wieser&gt;'+CR+LF+'</value></member>';
    end;

    // Responselog:
    // Verbindungsinformationen: 03.12.2013, WW
    LogStr:=Get_Responselog_VerbInfoData (VerbInfoData);
  end
  else begin
    // Responsedata für Gas-X-System:
    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
    mrglist:=GET_Member_hpsdata(sTraceLog + sReturnExt);  // mit TraceLog: 03.01.2022, WW
  end;

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+
               Get_Member_Result(ResultStr)+
               Get_Member_hpsanswer(Cmd_Ret)+
               mrglist;
  if length (LogStr) > 0 then
    XMLResponse:=XMLResponse + GET_Member_wieserlog (LogStr);
  XMLResponse:=XMLResponse + XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{-----------------------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_VerbAufbauDSfG_Response (resultStr,Cmd_Ret: string;
                                                         AufmTelegrammList: TAufmTelegrammList;
                                                         DSfGDfueKonfigData: TDSfGDfueKonfigData;
                                                         ZeitSyncInfoData: TZeitSyncInfoData;
                                                         ZS_Fehlergruppe, ZS_Fehlercode: integer;
                                                         ResponseLogList: TResponseLogList;
                                                         ResponseTraceLogList: TDSfGDataList;
                                                         FwUpdateInfoData: TFwUpdateInfoData;
                                                         VerbInfoData: TVerbInfoData;
                                                         Fehlergruppe, Fehlercode: integer;
                                                         sRetFreierText: string): string;
{-----------------------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
Var
  instlist,dsfglist: string;
  i:integer;
  Datencode: integer;
  EAdrMerker: string[1];
  TelegrItem: TAufmTelegrammListObj;
  DTStr: string;
  OK: boolean;
  HasAufmTelegramme: boolean;
  HasDfueKonfigData: boolean;
  HasZeitSyncData: boolean;
  DfueKonfigData_geschrieben: boolean;
  ZeitSyncData_geschrieben: boolean;
  LogStr: string;
  EAdr: string;
  rfilelist: string;
  HasFwUpdData: boolean;
  FwUpdData_geschrieben: boolean;
  sReturnExt: string;
  sTraceLog: string;

  {----------------------------------}
  procedure Insert_XML_DfueKonfigData;
  {----------------------------------}
  var
    j: integer;
  begin
    instlist:=instlist+C_DSfGDfueWertelisteStart+CR+LF;
    { Wieser: }
    if (VersionTyp = vs_Wieser) AND  // nur bei Wieser-Vesion; 24.02.2020, WW
       (length (DSfGDfueKonfigData.Wieser_Teilnehmer) > 0) then begin
      instlist:=instlist+C_WieserStart+CR+LF;
      instlist:=instlist+C_DSfGTeilnehmerStart+DSfGDfueKonfigData.Wieser_Teilnehmer+C_DSfGTeilnehmerEnd+CR+LF;
      for j:=Low (DSfGDfueKonfigData.Wieser_Adresse) to High (DSfGDfueKonfigData.Wieser_Adresse) do begin
        instlist:=instlist+C_lt_Subst+Format(C_DSfGIAdr_nStart,[j])+C_gt_Subst+
                  DSfGDfueKonfigData.Wieser_Adresse[j]+
                  C_lt_Subst+Format(C_DSfGIAdr_nEnd,[j])+C_gt_Subst+CR+LF;
      end;
      { Fabriknummer und Baujahr: 02.10.2009, WW }
      instlist:=instlist+C_DSfGFabrikNrStart+DSfGDfueKonfigData.Wieser_Fabriknummer+
                C_DSfGFabrikNrEnd+CR+LF;
      instlist:=instlist+C_DSfGBaujahrStart+DSfGDfueKonfigData.Wieser_Baujahr+
                C_DSfGBaujahrEnd+CR+LF;
      { Typ, Version und Build (für Firmware-Update): 05.03.2012, WW }
      instlist:=instlist+C_DSfGTypStart+DSfGDfueKonfigData.Wieser_NG_Typ+
                C_DSfGTypEnd+CR+LF;
      instlist:=instlist+C_DSfGVersionStart+DSfGDfueKonfigData.Wieser_NG_Version+
                C_DSfGVersionEnd+CR+LF;
      instlist:=instlist+C_DSfGBuildStart+DSfGDfueKonfigData.Wieser_NG_Build+
                C_DSfGBuildEnd+CR+LF;

      instlist:=instlist+C_WieserEnd+CR+LF;
    end;
    { Basic: }
    instlist:=instlist+C_DSfGBasicStart+CR+LF;
    if VersionTyp = vs_Wieser then begin  // nur bei Wieser-Vesion; 24.02.2020, WW
      instlist:=instlist +
        C_DSfGHerstellerStart+EncodeXml(DSfGDfueKonfigData.Hersteller)+C_DSfGHerstellerEnd+CR+LF+
        C_DSfGProgNameStart+DSfGDfueKonfigData.ProgName+C_DSfGProgNameEnd+CR+LF+
        C_DSfGProgVersionStart+DSfGDfueKonfigData.Version+C_DSfGProgVersionEnd+CR+LF;
    end;
    instlist:=instlist +
      C_DSfGExtensionmodeStart+IntToStr(DSfGDfueKonfigData.Extensionmode)+C_DSfGExtensionmodeEnd+CR+LF+  // 31.01.2017, WW
      C_DSfGBasicEnd+CR+LF;

    instlist:=instlist+C_DSfGDfueWertelisteEnd+CR+LF;
  end;

  {--------------------------------}
  procedure Insert_XML_ZeitSyncData;
  {--------------------------------}
  begin
    instlist:=instlist+C_ZeitSynclisteStart+CR+LF;
    instlist:=instlist+
              C_ErrorgroupStart+IntToStr(ZS_Fehlergruppe)+C_ErrorgroupEnd+CR+LF+
              C_ErrorcodeStart+IntToStr(ZS_Fehlercode)+C_ErrorcodeEnd+CR+LF;
    // Kennzeichnung, daß ZeitSync-Infodaten abgefragt wurden und somit zurück-
    // gegeben werden können, erfolgt anhand von 'DZ_Server':
    if ZeitSyncInfoData.DZ_Server > 0 then
      instlist:=instlist+
                C_DZ_ServerStart+FormatDateTime ('yyyymmddhhnnss', ZeitSyncInfoData.DZ_Server)+C_DZ_ServerEnd+CR+LF+
                C_DSfGDZ_InstanceStart+FormatDateTime ('yyyymmddhhnnss', ZeitSyncInfoData.DZ_Geraet)+C_DSfGDZ_InstanceEnd+CR+LF;

    instlist:=instlist+C_ZeitSynclisteEnd+CR+LF;
  end;

  {--------------------------------------}
  procedure Insert_XML_FirmwareUpdateData;
  {--------------------------------------}
  begin
    instlist:=instlist+C_FwUpdListeStart+CR+LF;
    instlist:=instlist+
              C_DSfGVersionStart+FwUpdateInfoData.Version_neu+C_DSfGVersionEnd+CR+LF+
              C_DSfGBuildStart+FwUpdateInfoData.Build_neu+C_DSfGBuildend+CR+LF;
    instlist:=instlist+C_FwUpdListeEnd+CR+LF;
  end;

begin
  dsfglist:='';
  LogStr:='';
  Datencode:=dc_KeineDaten;

  { TraceLog: 03.01.2022, WW }
  sTraceLog:=Get_Responsedata_TraceLog ('', ResponseTraceLogList);
  
  // Responsedata, Responselog für Wieser-System:
  if VersionTyp = vs_Wieser then begin
    // Responsedata:
    HasAufmTelegramme:=false;
    if AufmTelegrammList <> nil then begin
      if AufmTelegrammList.Count > 0 then begin
        HasAufmTelegramme:=true;
        Datencode:=Datencode + dc_DSfGAufmTelegr;
        AufmTelegrammList.SortByBusadresse; // Liste nach Busadresse sortieren
      end;
    end;

    HasDfueKonfigData:=false;
    HasZeitSyncData:=false;
    HasFwUpdData:=false;
    // Kennzeichnung, daß DSfG-DFÜ-Konfigdaten abgefragt wurden und somit zurück-
    // gegeben werden können, erfolgt anhand von 'EAdr_Dfue':
    if DSfGDfueKonfigData.EAdr_Dfue <> NUL then begin
      HasDfueKonfigData:=true;
      Datencode:=Datencode + dc_DSfGDfueDaten;

      // optionale ZeitSync-Rückgabedaten werden unter der Adresse der Login-DFÜ eingetragen:
      // -> Kennzeichnung, daß ZeitSync durchgeführt wurde und somit Ergebnis
      //    zurückgegeben werden kann, erfolgt anhand von 'ZeitSync-Fehlergruppe/-code':
      if (ZS_Fehlergruppe > -1) AND (ZS_Fehlercode > -1) then begin
        HasZeitSyncData:=true;
        Datencode:=Datencode + dc_ZeitSyncDaten;
      end;

      // optionale Firmwareupdate-Rückgabedaten werden unter der Adresse der Login-DFÜ eingetragen:
      // -> Kennzeichnung, daß Firmwareupdate durchgeführt wurde und somit Firmwareupdate-Daten
      //    zurückgegeben werden kann, erfolgt anhand von 'Version_neu', 'Build_neu':
      if (length (FwUpdateInfoData.Version_neu) > 0) AND
         (length (FwUpdateInfoData.Build_neu) > 0) then begin
        HasFwUpdData:=true;
        Datencode:=Datencode + dc_FwUpdateDaten;
      end;
    end;
    DfueKonfigData_geschrieben:=false;
    ZeitSyncData_geschrieben:=false;
    FwUpdData_geschrieben:=false;

    if HasAufmTelegramme OR HasDfueKonfigData OR HasZeitSyncData OR HasFwUpdData then begin
      instlist:='';

      if HasAufmTelegramme then begin
        EAdrMerker:=NUL;
        for i:=0 to AufmTelegrammList.Count-1 do begin
          Application.ProcessMessages;
          TelegrItem:=TAufmTelegrammListObj (AufmTelegrammList [i]);
          if TelegrItem.Busadresse <> EAdrMerker then begin  // neue Instanz startet
            if EAdrMerker <> NUL then      // nicht beim ersten Telegramm
              instlist:=instlist+'&lt;/atttelegr_list&gt;'+CR+LF+'&lt;/instance&gt;'+CR+LF;
            instlist:=instlist+'&lt;instance busaddr=&quot;'+TelegrItem.Busadresse+'&quot;&gt;'+CR+LF+'&lt;atttelegr_list&gt;'+CR+LF;
            EAdrMerker:=TelegrItem.Busadresse;

            { DFÜ-Konfigurationsdaten zur aktuellen Busadresse miteinfügen: }
            if HasDfueKonfigData AND (EAdrMerker = DSfGDfueKonfigData.EAdr_Dfue) then begin
              Insert_XML_DfueKonfigData;
              DfueKonfigData_geschrieben:=true;
            end;
            { ZeitSync-Daten zur aktuellen Busadresse miteinfügen: }
            if HasZeitSyncData AND (EAdrMerker = DSfGDfueKonfigData.EAdr_Dfue) then begin
              Insert_XML_ZeitSyncData;
              ZeitSyncData_geschrieben:=true;
            end;
            { Firmwareupdate-Info-Daten zur aktuellen Busadresse miteinfügen: }
            if HasFwUpdData AND (EAdrMerker = DSfGDfueKonfigData.EAdr_Dfue) then begin
              Insert_XML_FirmwareUpdateData;
              FwUpdData_geschrieben:=true;
            end;
          end;

          try
            { Datum/Zeit ist optional: }
            if TelegrItem.DatumZeit > -1 then
              DTStr:=FormatDateTime ('yyyymmddhhnnss', TelegrItem.DatumZeit)
            else
              DTStr:='';
            OK:=true;
          except
            OK:=false;  { sicherheitshalber abfangen, man weiß ja nie... }
          end;
          if OK then
            instlist:=instList+'&lt;atttelegr nty=&quot;'+TelegrItem.Nachrichtentyp+'&quot; '+
                               'ts=&quot;'+DTStr+'&quot; tz=&quot;'+TelegrItem.Zeitzone+'&quot;/&gt;'+CR+LF;
        end; // for TelegrDELLIst.count
        instlist:=instlist+'&lt;/atttelegr_list&gt;'+CR+LF+'&lt;/instance&gt;'+CR+LF;
      end;  // if HasAufmTelegramme

      // wenn DFÜ-Konfigurationsdaten oder ZeitSync-Daten noch nicht geschrieben
      // wurden, jetzt unter eigenem Instanz-Tag anhängen:
      if HasDfueKonfigData AND not DfueKonfigData_geschrieben then begin
        instlist:=instlist + C_DSfGInstanzStart+C_DSfGKennungIAdr+
                  C_quot_Subst+ DSfGDfueKonfigData.EAdr_Dfue+C_quot_Subst+C_gt_Subst+CR+LF;
        Insert_XML_DfueKonfigData;

        if HasZeitSyncData AND not ZeitSyncData_geschrieben then
          Insert_XML_ZeitSyncData;

        if HasFwUpdData AND not FwUpdData_geschrieben then
          Insert_XML_FirmwareUpdateData;

        instlist:=instlist+C_DSfGInstanzEnd+CR+LF;
      end;
      dsfglist:='&lt;dsfg&gt;'+CR+LF+instlist+'&lt;/dsfg&gt;';
    end; // if HasAufmTelegramme OR HasDfueKonfigData

    // Responselog:
    if ResponseLogList <> nil then begin
      if ResponseLogList.Count > 0 then begin
        ResponseLogList.SortByBusadresse; // Liste nach Busadresse sortieren

        instlist:='';
        rfilelist:='';
        EAdrMerker:=NUL;
        for i:=0 to ResponseLogList.Count -1 do begin
          EAdr:=TRohfileLogObj (ResponseLogList [i]).EAdr;  // Rohfile-Loglisteneintrag

          if (EAdr <> EAdrMerker) then begin  // neue Instanz startet
            if (EAdrMerker <> NUL) then begin          // nicht beim ersten Datenelement
              if length (rfilelist) > 0 then
                instlist:=instlist+CR+LF+'&lt;rfile_list&gt;'+CR+LF+rfilelist+'&lt;/rfile_list&gt;';

              // vorhergehende Instanz schließen
              instlist:=instlist+CR+LF+'&lt;/instance&gt;'+CR+LF;
            end;
            // nächste Instanz öffnen
            instlist:=instlist+'&lt;instance busaddr=&quot;'+EAdr+'&quot;&gt;';
            rfilelist:='';

            EAdrMerker:=EAdr;
          end;

          // Rohfile-Loglisteneintrag
          rfilelist:=rfilelist+'&lt;rfile '+
                     'name=&quot;'+TRohfileLogObj (ResponseLogList [i]).RohfileName+'&quot;/&gt;'+CR+LF;
        end; // for ResponseLogList.Count

        if length (rfilelist) > 0 then
          instlist:=instlist+CR+LF+'&lt;rfile_list&gt;'+CR+LF+rfilelist+'&lt;/rfile_list&gt;';

        // letzte Instanz schließen
        instlist:=instlist+CR+LF+'&lt;/instance&gt;'+CR+LF;
        LogStr:='&lt;dsfg&gt;'+CR+LF+instlist+'&lt;/dsfg&gt;';
      end;  // if ResponseLogList.Count > 0
    end;  // if ResponseLogList <> nil

    // Verbindungsinformationen: 03.12.2013, WW
    if length (LogStr) > 0 then
      LogStr:=LogStr + CR + LF;
    LogStr:=LogStr + Get_Responselog_VerbInfoData (VerbInfoData);
  end
  else begin
    // Responsedata für Gas-X-System:

    // Busadresse der Login-DFÜ-Instanz zurückgeben; 13.03.2018, WW
    if DSfGDfueKonfigData.EAdr_Dfue <> NUL then begin
      instlist:=instlist + C_DSfGInstanzStart+C_DSfGKennungIAdr+
                C_quot_Subst+ DSfGDfueKonfigData.EAdr_Dfue+C_quot_Subst+C_gt_Subst+CR+LF;
      Insert_XML_DfueKonfigData;  // 24.02.2020, WW

      if length (sTraceLog) > 0 then
        instlist:=instlist + sTraceLog;  // 03.01.2022, WW

      instlist:=instlist+C_DSfGInstanzEnd+CR+LF;
    end;
    dsfglist:='&lt;dsfg&gt;'+CR+LF+instlist+'&lt;/dsfg&gt;';

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);

    dsfglist:=dsfglist+CR+LF+sReturnExt;
  end;

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+
               Get_Member_Result(ResultStr)+
               Get_Member_hpsanswer(Cmd_Ret);
  if length (dsfglist) > 0 then
    XMLResponse:=XMLResponse + GET_Member_hpsdata (dsfglist);
  if length (LogStr) > 0 then
    XMLResponse:=XMLResponse + GET_Member_wieserlog (LogStr);
  XMLResponse:=XMLResponse + XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{--------------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_MWTA_Response(resultStr,Cmd_Ret: string;
                                              MwFileNameList: TStringList;
                                              TaFileNameList: TStringList;
                                              MaxMessKanal, MaxTagKanal: integer;
                                              MinMessKanalExt, MaxMessKanalExt: integer;
                                              Tagesende: integer;
                                              MRGKennung:string; MRGTyp:integer;
                                              ResponseTraceLogList: TDSfGDataList;
                                              von, bis: TDateTime;
                                              LGZnorm_Daten: boolean;
                                              Fehlergruppe, Fehlercode: integer;
                                              sRetFreierText: string): string;
{--------------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
Var
  mrglist: string;
  Datencode: integer;

begin
  mrglist:=GET_Responsedata_MWTA (MwFileNameList, TaFileNameList,
                                  MaxMessKanal, MaxTagKanal,
                                  MinMessKanalExt, MaxMessKanalExt,
                                  Tagesende,
                                  MRGKennung, MRGTyp,
                                  ResponseTraceLogList,
                                  von, bis,
                                  LGZnorm_Daten, false,
                                  Fehlergruppe, Fehlercode, sRetFreierText,
                                  Datencode);

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+Get_Member_Result(ResultStr)+Get_Member_hpsanswer(Cmd_Ret)+mrglist+XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{-------------------------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_ME_Response(resultStr,Cmd_Ret:string;MeldungsListe: TMeldungsListe;
                                            MRGKennung:string; MRGTyp:integer;
                                            ResponseTraceLogList: TDSfGDataList;
                                            von, bis: TDateTime;
                                            Fehlergruppe, Fehlercode: integer;
                                            sRetFreierText: string):string;
{-------------------------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
Var
  mrglist: string;
  Datencode: integer;

begin
  mrglist:=GET_Responsedata_ME (Meldungsliste, MRGKennung, MRGTyp,
                                ResponseTraceLogList,
                                von, bis,
                                Fehlergruppe, Fehlercode, sRetFreierText,
                                Datencode);

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+Get_Member_Result(ResultStr)+Get_Member_hpsanswer(Cmd_Ret)+mrglist+XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_PA_Response(resultStr,Cmd_Ret:string;
                                            ParameterListe: TParameterListe;
                                            MRGKennung:string; MRGTyp:integer;
                                            ResponseTraceLogList: TDSfGDataList;
                                            Fehlergruppe, Fehlercode: integer;
                                            sRetFreierText: string):string;
{------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
Var
  ParaList,mrglist: string;
  i:integer;
  Datencode: integer;
  ParameterItem: TParameterItem;
  sReturnExt: string;
  sTraceLog: string;

begin
  paralist:='';
  mrglist:='';
  Datencode:=dc_KeineDaten;
  if (ParameterListe<>Nil)  then begin
    if (ParameterListe.Count>0) then begin
      Datencode:=dc_Parameter;
      paralist:=CR+LF+'&lt;paralist&gt;'+CR+LF;
      for i:=0 to ParameterListe.count -1 do begin
        Application.ProcessMessages;
        ParameterItem := TParameterItem(ParameterListe[i]);
        ParaList:=ParaList+'&lt;para par-id=&quot;'+ParameterItem.AllgNum+'&quot;';
        // MRG-Parameternummer nur für Wieser-System:
        if VersionTyp = vs_Wieser then
          ParaList:=ParaList + ' mrgpar-id=&quot;'+ParameterItem.MrgNum+'&quot;';
        ParaList:=ParaList + '&gt;'+ EncodeXml(ParameterItem.Wert) +'&lt;/para&gt;'+CR+LF;  // Bugfix fehlende XML-Zeichenersetzung; 03.06.2020, WW
      end; // for ParameterLIst.count
      paralist:=paralist+'&lt;/paralist&gt;'+CR+LF;
    end;  // ParameterListe.Count > 0
  end; // ParameterListe <> Nil

  { TraceLog: 03.01.2022, WW }
  sTraceLog:=Get_Responsedata_TraceLog ('', ResponseTraceLogList);
  if length (sTraceLog) > 0 then
    ParaList:=ParaList + sTraceLog;

  if length (ParaList) > 0 then begin  // 03.01.2022, WW
    mrglist:=GET_Member_hpsdata_MRG(MRGKennung, MRGTyp);
    mrglist:=mrglist+ParaList;
    mrglist:=mrglist+'&lt;/mrg&gt;'+CR+LF+'&lt;/wieser&gt;';
  end;

  sReturnExt:='';
  if VersionTyp = vs_GasX then begin
    // Responsedata für Gas-X-System:

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
  end;

  if mrglist <> '' then begin
    if sReturnExt <> '' then
      mrglist:=mrglist+CR+LF+sReturnExt;

    mrglist:=mrglist+CR+LF+'</value></member>';
  end
  else begin
    if sReturnExt <> '' then
      mrglist:=GET_Member_hpsdata(sReturnExt);
  end;

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+Get_Member_Result(ResultStr)+Get_Member_hpsanswer(Cmd_Ret)+mrglist+XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{-------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_PR_Response(resultStr,Cmd_Ret,PrFileName:String;
                                            MRGKennung:string; MRGTyp:integer;
                                            von, bis: TDateTime;
                                            Fehlergruppe, Fehlercode: integer;
                                            sRetFreierText: string):string;
{-------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
Var
  FS: TFileOfRecStream;
  FRecCount: integer;
  pr: PruefRec;
  cklist,mrglist,tmp,von_Datum,bis_Datum: string;
  i:integer;
  dtv,dtb:TDateTime;
  Datencode: integer;
  OK: boolean;
  sReturnExt: string;

begin
  cklist:='';
  mrglist:='';
  Datencode:=dc_KeineDaten;

  if (FileExists(PrFileName)) then begin
    try
      FS:=TFileOfRecStream.Create (PrFilename, fmOpenRead OR fmShareDenyWrite, SizeOf (pr));
      try
        FRecCount:=FS.RecCount;
        if FRecCount > 0 then begin
          Datencode:=dc_Pruefsaetze;
          cklist:=CR+LF+'&lt;cklist&gt;'+CR+LF;
          while FS.RecPosition < FRecCount do begin
            Application.ProcessMessages;
            FS.ReadRec (pr);
            OK:=true;
            if RecToDateTime(pr.von_datum,pr.von_zeit,dtv) then
              von_Datum:=FormatDatetime('yyyymmddhhnnss',dtv)
            else
              OK:=false;  { sicherheitshalber abfangen, man weiß ja nie... }
            if RecToDateTime(pr.bis_datum,pr.bis_zeit,dtb) then
              bis_Datum:=FormatDatetime('yyyymmddhhnnss',dtb)
            else
              OK:=false;  { sicherheitshalber abfangen, man weiß ja nie... }

            if VersionTyp = vs_GasX then begin
              if OK then begin
                { wenn Zeitfilter aktiviert:
                  Prüfung ob Datensatz-Zeitstempel innerhalb des
                  zurückzugebenden Zeitbereichs liegt }
                if (von > -1) AND (bis > -1) then begin
                  if (CmpDateTime (dtv, von) < 0) OR (CmpDateTime (dtv, bis) > 0) then
                    OK:=false;
                end;
              end;
            end;
            if OK then begin
              cklist:=cklist+'&lt;ckset ts_start=&quot;'+von_Datum+'&quot; ts_stop=&quot;'+bis_Datum+'&quot;&gt;'+CR+LF;
              for i:=1 to 4 do begin
                tmp:='&lt;ck cid=&quot;'+inttostr(i)+'&quot;&gt;'+pr.wert[i]+'&lt;/ck&gt;'+CR+LF;
                ckList:=ckList+tmp;
              end;
              ckList:=ckList+'&lt;/ckset&gt;'+CR+LF;
            end;
          end;  { while FS.RecPosition < FRecCount }
          ckList:=ckList+'&lt;/cklist&gt;'+CR+LF;

          mrglist:=GET_Member_hpsdata_MRG(MRGKennung, MRGTyp);
          mrglist:=mrglist+cklist;
          mrglist:=mrglist+'&lt;/mrg&gt;'+CR+LF+'&lt;/wieser&gt;';
        end;  { if FRecCount > 0 }
      finally
        FS.Free;
      end;
    except
      mrglist:='';  // 13.03.2018, WW
    end;
  end;

  sReturnExt:='';
  if VersionTyp = vs_GasX then begin
    // Responsedata für Gas-X-System:

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
  end;

  if mrglist <> '' then begin
    if sReturnExt <> '' then
      mrglist:=mrglist+CR+LF+sReturnExt;

    mrglist:=mrglist+CR+LF+'</value></member>';
  end
  else begin
    if sReturnExt <> '' then
      mrglist:=GET_Member_hpsdata(sReturnExt);
  end;

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+Get_Member_Result(ResultStr)+Get_Member_hpsanswer(Cmd_Ret)+mrglist+XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{----------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_AR_Response(resultStr,Cmd_Ret:string;
                                            DSfGArchivDataList: TDSfGDataList;
                                            ResponseLogList: TResponseLogList;
                                            ResponseRohdatenList: TDSfGDataList;
                                            ResponseTraceLogList: TDSfGDataList;
                                            von, bis: TDateTime;
                                            mit_Zeitzone: boolean;
                                            Rufcode: integer;
                                            Fehlergruppe, Fehlercode: integer;
                                            sRetFreierText: string):string;
{----------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt
  -> optionaler Rufcode (nur für Anrufentgegennahme-Antwort mit SMS-Daten) wird
     übergeben und an Cmd_Ret angehängt }
Var
  instlist,dsfglist: string;
  i:integer;
  Datencode: integer;
  DSfGDataListItem: TDSfGDataListObj;
  EAdrMerker:string[1];
  ArchFilename: string;
  Arch: TDSfGSatzData;
  ListBuf: TStringList;  { Zwischenpuffer um lange Strings (instlist) zu
                           vermeiden (ab 1 MB XML-Ausgabe wird es sonst sehr langsam !) }
  FS: TFileOfRecStream;
  FRecCount: integer;
  OK: boolean;
  LogStr: string;
  EAdr: string;
  archloglist: string;
  rfilelist: string;
  sWert: string;
  rawdatalist: string;
  sReturnExt: string;
  sTraceLog: string;

begin
  ListBuf:=TStringList.Create;
  try
    ListBuf.Sorted:=false;

    dsfglist:='';
    Datencode:=dc_KeineDaten;
    { Archivdaten: }
    if (DSfGArchivDataList<>Nil)  then begin
      if (DSfGArchivDataList.Count>0) then begin
        instlist:='';
        EAdrMerker:=NUL;
        for i:=0 to DSfGArchivDataList.Count -1 do begin
          DSfGDataListItem:=TDSfGDataListObj (DSfGArchivDataList.Objects[i]);

          if (DSfGDataListItem.EAdr <> EAdrMerker) then begin  // neue Instanz startet
            if (EAdrMerker<>NUL) then begin  // nicht bei den ersten Archivdaten
              instlist:=instlist+'&lt;/archval_list&gt;'+CR+LF;

              { Rohdatenliste: 07.04.2014, WW }
              rawdatalist:=Get_Responsedata_Rohdaten (EAdrMerker,
                 ResponseRohdatenList, true);
              if length (rawdatalist) > 0 then
                instlist:=instlist + rawdatalist;

              { TraceLog: 24.02.2020, WW }
              sTraceLog:=Get_Responsedata_TraceLog (EAdrMerker, ResponseTraceLogList);
              if length (sTraceLog) > 0 then
                instlist:=instlist + sTraceLog;

              instlist:=instlist+'&lt;/instance&gt;'+CR+LF;
            end;
            instlist:=instlist+'&lt;instance busaddr=&quot;'+ DSfGDataListItem.EAdr+'&quot;&gt;'+CR+LF+'&lt;archval_list&gt;'+CR+LF;
            EAdrMerker:= DSfGDataListItem.EAdr;
          end;

          { strukturierte Datei mit Archivdaten öffnen: }
          ArchFilename:=DSfGArchivDataList[i];
          if (FileExists(ArchFileName)) then begin
            try
              FS:=TFileOfRecStream.Create (ArchFilename, fmOpenRead OR fmShareDenyWrite, SizeOf (Arch));
              try
                FRecCount:=FS.RecCount;
                if FRecCount > 0 then begin
                  Datencode:=dc_Messwerte + dc_Tagessaetze;
                  while FS.RecPosition < FRecCount do begin
                    Application.ProcessMessages;
                    FS.ReadRec (Arch);
                    OK:=true;
                    if VersionTyp = vs_GasX then begin
                      { wenn über Datum/Zeit abgerufen wurde:
                        -> Zeitfilter: Prüfung ob Datensatz-Zeitstempel innerhalb des
                                       zurückzugebenden Zeitbereichs liegt }
                      if (von > -1) AND (bis > -1) then begin
                        if (CmpDateTime (Arch.DatumZeit, von) < 0) OR
                           (CmpDateTime (Arch.DatumZeit, bis) > 0) then
                          OK:=false;
                      end;
                    end;
                    if OK then begin
                      instlist:=instList+'&lt;archval '+
                                'addr=&quot;'+Arch.DEA+'&quot; '+  // für zeilenweise abgefragte Daten einer AG; 11.07.2016, WW
                                'ordinal=&quot;'+ IntToStr(Arch.OrdnungsNr) +'&quot; '+
                                'ts=&quot;'+ Arch.UnixDatumZeit +'&quot; '+
                                'state=&quot;'+ Arch.Status +'&quot; '+
                                'checksum=&quot;'+ Arch.CRC +'&quot;';
                      if (VersionTyp = vs_Wieser) AND mit_Zeitzone then  // mit XML-Tag für Zeitzone (SMS-Daten); 01.08.2006, WW
                        instlist:=instlist + ' tz=&quot;'+ Arch.Zeitzone +'&quot;';
{$IFNDEF NO_XML_SIGVERIFYSTATE}
                      // mit Signatur-Verifizierungsstatus; 15.03.2012, WW
                      // 07.04.2014: auch für Gas-X
                      instlist:=instlist + ' sigverifystate=&quot;'+ IntToStr (Arch.SigVerifyStatus) +'&quot;';
{$ENDIF}
                      sWert:=Arch.WertAsString;
                      if bISO646 then  // Zeichen-Konvertierung ASCII -> ISO 646; 03.03.2010, WW
                        if length (sWert) > 1 then  // zur Vermeidung, daß DSfG-Busadressen konvertiert werden
                          sWert:=WAsciiToISO646_De (sWert);

                      instlist:=instlist + '&gt;'+ sWert +'&lt;/archval&gt;'+CR+LF;
                    end;

                    if length (instlist) > C_ListStrLen then begin
                      ListBuf.Add (instlist);    { instlist zwischenpuffern }
                      instlist:='';
                    end;
                  end;  { while FS.RecPosition < FRecCount }
                end;  { if FRecCount > 0 }
              finally
                FS.Free;
              end;
            except
            end;
          end;
        end; // for DSfGArchivDataList.count
        instlist:=instlist+'&lt;/archval_list&gt;'+CR+LF;

        if (EAdrMerker<>NUL) then begin
          { Rohdatenliste: 07.04.2014, WW }
          rawdatalist:=Get_Responsedata_Rohdaten (EAdrMerker,
             ResponseRohdatenList, true);
          if length (rawdatalist) > 0 then
            instlist:=instlist + rawdatalist;

          { TraceLog: 24.02.2020, WW }
          sTraceLog:=Get_Responsedata_TraceLog (EAdrMerker, ResponseTraceLogList);
          if length (sTraceLog) > 0 then
            instlist:=instlist + sTraceLog;
        end;

        instlist:=instlist+'&lt;/instance&gt;'+CR+LF;
        ListBuf.Add (instlist);

        dsfglist:='&lt;dsfg&gt;'+CR+LF;
        for i:=0 to ListBuf.Count-1 do
          dsfglist:=dsfglist + ListBuf[i];
        dsfglist:=dsfglist+'&lt;/dsfg&gt;';
      end;  // if DSfGArchivDataList.Count > 0
    end; // DSfGArchivDataList <> Nil
  finally
    ListBuf.Free;
  end;

  if VersionTyp = vs_GasX then begin
    // Responsedata für Gas-X-System:

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
    dsfglist:=dsfglist+CR+LF+sReturnExt;
  end;

  { Reponse-Log: }
  LogStr:='';
  if ResponseLogList <> nil then begin
    if ResponseLogList.Count > 0 then begin
      ResponseLogList.SortByBusadresse; // Liste nach Busadresse sortieren

      instlist:='';
      archloglist:='';
      rfilelist:='';
      EAdrMerker:=NUL;
      for i:=0 to ResponseLogList.Count -1 do begin
        if TObject (ResponseLogList [i]) is TDSfGDataKonvLogObj then  // Datenkonvertierung-Loglisteneintrag
          EAdr:=TDSfGDataKonvLogObj (ResponseLogList [i]).EAdr
        else if TObject (ResponseLogList [i]) is TRohfileLogObj then  // Rohfile-Loglisteneintrag
          EAdr:=TRohfileLogObj (ResponseLogList [i]).EAdr
        else  // unbekannter Loglisteneintrag
          Continue;

        if (EAdr <> EAdrMerker) then begin  // neue Instanz startet
          if (EAdrMerker <> NUL) then begin          // nicht beim ersten Datenelement
            if length (archloglist) > 0 then
              instlist:=instlist+CR+LF+'&lt;archlog_list&gt;'+CR+LF+archloglist+'&lt;/archlog_list&gt;';
            if length (rfilelist) > 0 then
              instlist:=instlist+CR+LF+'&lt;rfile_list&gt;'+CR+LF+rfilelist+'&lt;/rfile_list&gt;';

            // vorhergehende Instanz schließen
            instlist:=instlist+CR+LF+'&lt;/instance&gt;'+CR+LF;
          end;
          // nächste Instanz öffnen
          instlist:=instlist+'&lt;instance busaddr=&quot;'+EAdr+'&quot;&gt;';
          archloglist:='';
          rfilelist:='';

          EAdrMerker:=EAdr;
        end;

        if TObject (ResponseLogList [i]) is TDSfGDataKonvLogObj then  // Datenkonvertierung-Loglisteneintrag
          archloglist:=archloglist+'&lt;archlog '+
                       'addr=&quot;'+ TDSfGDataKonvLogObj (ResponseLogList [i]).DEL+'&quot; '+
                       'convstate=&quot;'+ IntToStr (TDSfGDataKonvLogObj (ResponseLogList [i]).KonvStatus) +
                       '&quot;/&gt;'+CR+LF
        else if TObject (ResponseLogList [i]) is TRohfileLogObj then  // Rohfile-Loglisteneintrag
          rfilelist:=rfilelist+'&lt;rfile '+
                     'name=&quot;'+TRohfileLogObj (ResponseLogList [i]).RohfileName+'&quot;/&gt;'+CR+LF;
      end; // for ResponseLogList.Count

      if length (archloglist) > 0 then
        instlist:=instlist+CR+LF+'&lt;archlog_list&gt;'+CR+LF+archloglist+'&lt;/archlog_list&gt;';
      if length (rfilelist) > 0 then
        instlist:=instlist+CR+LF+'&lt;rfile_list&gt;'+CR+LF+rfilelist+'&lt;/rfile_list&gt;';

      // letzte Instanz schließen
      instlist:=instlist+CR+LF+'&lt;/instance&gt;'+CR+LF;
      LogStr:='&lt;dsfg&gt;'+CR+LF+instlist+'&lt;/dsfg&gt;';
    end;  // if ResponseLogList.Count > 0
  end;  // if ResponseLogList <> nil

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  if Rufcode > -1 then  // für Anrufentgegennahme-Antwort: mit Rufcode
    Cmd_Ret:=Cmd_Ret + IntToStr (Rufcode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+
               Get_Member_Result(ResultStr)+
               Get_Member_hpsanswer(Cmd_Ret)+
               GET_Member_hpsdata (dsfglist)+
               GET_Member_wieserlog (LogStr)+
               XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{-----------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_LB_Response(resultStr,Cmd_Ret:string;
                                            DSfGLogbuchDataList: TDSfGDataList;
                                            ResponseLogList: TResponseLogList;
                                            ResponseRohdatenList: TDSfGDataList;
                                            ResponseTraceLogList: TDSfGDataList;
                                            von, bis: TDateTime;
                                            mit_Zeitzone: boolean;
                                            Rufcode: integer;
                                            Fehlergruppe, Fehlercode: integer;
                                            sRetFreierText: string):string;
{-----------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt
  -> optionaler Rufcode (nur für Anrufentgegennahme-Antwort mit SMS-Daten) wird
     übergeben und an Cmd_Ret angehängt }
var
  instlist,dsfglist: string;
  i:integer;
  Datencode: integer;
  DSfGDataListItem: TDSfGDataListObj;
  EAdrMerker:string[1];
  LogbFilename: string;
  Logb: TDSfGSatzData;
  FS: TFileOfRecStream;
  FRecCount: integer;
  OK: boolean;
  LogStr: string;
  EAdr: string;
  msgloglist: string;
  rfilelist: string;
  rawdatalist: string;
  sReturnExt: string;
  sTraceLog: string;

begin
  dsfglist:='';
  Datencode:=dc_KeineDaten;
  if (DSfGLogbuchDataList<>Nil)  then begin
    if (DSfGLogbuchDataList.Count>0) then begin
      instlist:='';
      EAdrMerker:=NUL;
      for i:=0 to DSfGLogbuchDataList.count -1 do begin
        DSfGDataListItem:=TDSfGDataListObj (DSfGLogbuchDataList.Objects[i]);

        if (DSfGDataListItem.EAdr <> EAdrMerker) then begin  // neue Instanz startet
          if (EAdrMerker<>NUL) then begin          // nicht beim ersten Datenelement
            instlist:=instlist+'&lt;/msglist&gt;'+CR+LF;

            { Rohdatenliste: 07.04.2014, WW }
            rawdatalist:=Get_Responsedata_Rohdaten (EAdrMerker,
               ResponseRohdatenList, true);
            if length (rawdatalist) > 0 then
              instlist:=instlist + rawdatalist;

            { TraceLog: 03.01.2022, WW }
            sTraceLog:=Get_Responsedata_TraceLog (EAdrMerker, ResponseTraceLogList);
            if length (sTraceLog) > 0 then
              instlist:=instlist + sTraceLog;

            instlist:=instlist+'&lt;/instance&gt;'+CR+LF;
          end;
          instlist:=instlist+'&lt;instance busaddr=&quot;'+ DSfGDataListItem.EAdr+'&quot;&gt;'+CR+LF+'&lt;msglist&gt;'+CR+LF;
          EAdrMerker:= DSfGDataListItem.EAdr;
        end;

        { strukturierte Datei mit Logbuchdaten öffnen: }
        LogbFilename:=DSfGLogbuchDataList[i];
        if (FileExists(LogbFileName)) then begin
          try
            FS:=TFileOfRecStream.Create (LogbFilename, fmOpenRead OR fmShareDenyWrite, SizeOf (Logb));
            try
              FRecCount:=FS.RecCount;
              if FRecCount > 0 then begin
                Datencode:=dc_Meldungen;
                while FS.RecPosition < FRecCount do begin
                  Application.ProcessMessages;
                  FS.ReadRec (Logb);
                  OK:=true;
                  if VersionTyp = vs_GasX then begin
                    { wenn über Datum/Zeit abgerufen wurde:
                      -> Zeitfilter: Prüfung ob Datensatz-Zeitstempel innerhalb des
                                     zurückzugebenden Zeitbereichs liegt }
                    if (von > -1) AND (bis > -1) then begin
                      if (CmpDateTime (Logb.DatumZeit, von) < 0) OR
                         (CmpDateTime (Logb.DatumZeit, bis) > 0) then
                        OK:=false;
                    end;
                  end;
                  if OK then begin
                    instlist:=instList+'&lt;msg '+
                              'addr=&quot;'+DSfGDataListItem.DEL+'&quot; '+
                              'mcode=&quot;'+Logb.WertAsString+'&quot; '+
                              'ordinal=&quot;'+ IntToStr(Logb.OrdnungsNr) +'&quot; '+
                              'ts=&quot;'+ Logb.UnixDatumZeit +'&quot; '+
                              'state=&quot;'+ Logb.Status +'&quot; '+
                              'checksum=&quot;'+ Logb.CRC +'&quot;';
                    if (VersionTyp = vs_Wieser) AND mit_Zeitzone then  // mit XML-Tag für Zeitzone (SMS-Daten); 01.08.2006, WW
                      instlist:=instlist + ' tz=&quot;'+ Logb.Zeitzone +'&quot;';
{$IFNDEF NO_XML_SIGVERIFYSTATE}
                    // mit Signatur-Verifizierungsstatus; 15.03.2012, WW
                    // 07.04.2014: auch für Gas-X
                    instlist:=instlist + ' sigverifystate=&quot;'+ IntToStr (Logb.SigVerifyStatus) +'&quot;';
{$ENDIF}
                    instlist:=instList+'/&gt;'+CR+LF;
                  end;
                end;
              end;  { if FRecCount > 0 }
            finally
              FS.Free;
            end;
          except
          end;
        end;
      end; // for DSfGArchivDataList.count
      instlist:=instlist+'&lt;/msglist&gt;'+CR+LF;

      if (EAdrMerker<>NUL) then begin
        { Rohdatenliste: 07.04.2014, WW }
        rawdatalist:=Get_Responsedata_Rohdaten (EAdrMerker,
           ResponseRohdatenList, true);
        if length (rawdatalist) > 0 then
          instlist:=instlist + rawdatalist;

        { TraceLog: 03.01.2022, WW }
        sTraceLog:=Get_Responsedata_TraceLog (EAdrMerker, ResponseTraceLogList);
        if length (sTraceLog) > 0 then
          instlist:=instlist + sTraceLog;
      end;

      instlist:=instlist+'&lt;/instance&gt;'+CR+LF;

      dsfglist:='&lt;dsfg&gt;'+CR+LF+instlist+'&lt;/dsfg&gt;';
    end;  // if (DSfGLogbuchDataList.Count>0)
  end; // if DSfGLogbuchDataList<>Nil

  if VersionTyp = vs_GasX then begin
    // Responsedata für Gas-X-System:

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
    dsfglist:=dsfglist+CR+LF+sReturnExt;
  end;

  { Reponse-Log: }
  LogStr:='';
  if ResponseLogList <> nil then begin
    if ResponseLogList.Count > 0 then begin
      ResponseLogList.SortByBusadresse; // Liste nach Busadresse sortieren

      instlist:='';
      msgloglist:='';
      rfilelist:='';
      EAdrMerker:=NUL;
      for i:=0 to ResponseLogList.Count -1 do begin
        if TObject (ResponseLogList [i]) is TDSfGDataKonvLogObj then  // Datenkonvertierung-Loglisteneintrag
          EAdr:=TDSfGDataKonvLogObj (ResponseLogList [i]).EAdr
        else if TObject (ResponseLogList [i]) is TRohfileLogObj then  // Rohfile-Loglisteneintrag
          EAdr:=TRohfileLogObj (ResponseLogList [i]).EAdr
        else  // unbekannter Loglisteneintrag
          Continue;

        if (EAdr <> EAdrMerker) then begin  // neue Instanz startet
          if (EAdrMerker <> NUL) then begin          // nicht beim ersten Datenelement
            if length (msgloglist) > 0 then
              instlist:=instlist+CR+LF+'&lt;msglog_list&gt;'+CR+LF+msgloglist+'&lt;/msglog_list&gt;';
            if length (rfilelist) > 0 then
              instlist:=instlist+CR+LF+'&lt;rfile_list&gt;'+CR+LF+rfilelist+'&lt;/rfile_list&gt;';

            // vorhergehende Instanz schließen
            instlist:=instlist+CR+LF+'&lt;/instance&gt;'+CR+LF;
          end;
          // nächste Instanz öffnen
          instlist:=instlist+'&lt;instance busaddr=&quot;'+EAdr+'&quot;&gt;';
          msgloglist:='';
          rfilelist:='';

          EAdrMerker:=EAdr;
        end;

        if TObject (ResponseLogList [i]) is TDSfGDataKonvLogObj then  // Datenkonvertierung-Loglisteneintrag
          msgloglist:=msgloglist+'&lt;msglog '+
                      'addr=&quot;'+ TDSfGDataKonvLogObj (ResponseLogList [i]).DEL+'&quot; '+
                      'convstate=&quot;'+ IntToStr (TDSfGDataKonvLogObj (ResponseLogList [i]).KonvStatus) +
                      '&quot;/&gt;'+CR+LF
        else if TObject (ResponseLogList [i]) is TRohfileLogObj then  // Rohfile-Loglisteneintrag
          rfilelist:=rfilelist+'&lt;rfile '+
                     'name=&quot;'+TRohfileLogObj (ResponseLogList [i]).RohfileName+'&quot;/&gt;'+CR+LF;
      end; // for ResponseLogList.Count

      if length (msgloglist) > 0 then
        instlist:=instlist+CR+LF+'&lt;msglog_list&gt;'+CR+LF+msgloglist+'&lt;/msglog_list&gt;';
      if length (rfilelist) > 0 then
        instlist:=instlist+CR+LF+'&lt;rfile_list&gt;'+CR+LF+rfilelist+'&lt;/rfile_list&gt;';

      // letzte Instanz schließen
      instlist:=instlist+CR+LF+'&lt;/instance&gt;'+CR+LF;
      LogStr:='&lt;dsfg&gt;'+CR+LF+instlist+'&lt;/dsfg&gt;';
    end;  // if ResponseLogList.Count > 0
  end;  // if ResponseLogList <> nil

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  if Rufcode > -1 then  // für Anrufentgegennahme-Antwort: mit Rufcode
    Cmd_Ret:=Cmd_Ret + IntToStr (Rufcode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+
               Get_Member_Result(ResultStr)+
               Get_Member_hpsanswer(Cmd_Ret)+
               GET_Member_hpsdata (dsfglist)+
               GET_Member_wieserlog (LogStr)+
               XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{--------------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_DE_Response(resultStr,Cmd_Ret:string;
                                            DELList: TDELList;
                                            ResponseLogList: TResponseLogList;
                                            ResponseRohdatenList: TDSfGDataList;
                                            ResponseTraceLogList: TDSfGDataList;
                                            Fehlergruppe, Fehlercode: integer;
                                            sRetFreierText: string):string;
{--------------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
Var
  instlist,dsfglist: string;
  i:integer;
  Datencode: integer;
  DELItem: TDELListObj;
  EAdrMerker:string[1];
  LogStr: string;
  EAdr: string;
  curvallog: string;
  rfilelist: string;
  sDELWert: string;
  rawdatalist: string;
  sReturnExt: string;
  sTraceLog: string;
  sUnerledigt: string;

begin
  dsfglist:='';
  instlist:='';
  Datencode:=dc_KeineDaten;
  if (DELList<>Nil)  then begin
    if (DELList.Count>0) then begin
      Datencode:=dc_Parameter;

      EAdrMerker:=NUL;
      for i:=0 to DELList.count -1 do begin
        Application.ProcessMessages;
        DELItem := TDELListObj(DELList[i]);
        if (DELItem.EAdr <> EAdrMerker) then begin  // neue Instanz startet
          if (EAdrMerker<>NUL) then begin          // nicht beim ersten Datenelement
            instlist:=instlist+'&lt;/curval_list&gt;'+CR+LF;

            { Rohdatenliste: 07.04.2014, WW }
            rawdatalist:=Get_Responsedata_Rohdaten (EAdrMerker,
               ResponseRohdatenList, false);
            if length (rawdatalist) > 0 then
              instlist:=instlist + rawdatalist;

            { TraceLog: 03.01.2022, WW }
            sTraceLog:=Get_Responsedata_TraceLog (EAdrMerker, ResponseTraceLogList);
            if length (sTraceLog) > 0 then
              instlist:=instlist + sTraceLog;

            instlist:=instlist+'&lt;/instance&gt;'+CR+LF;
          end;
          instlist:=instlist+'&lt;instance busaddr=&quot;'+ DELItem.EAdr+'&quot;&gt;'+CR+LF+'&lt;curval_list&gt;'+CR+LF;
          EAdrMerker:= DELItem.EAdr;
        end;

        sDELWert:=DELItem.Wert;
        if bISO646 then  // Zeichen-Konvertierung ASCII -> ISO 646; 03.03.2010, WW
          if length (sDELWert) > 1 then  // zur Vermeidung, daß DSfG-Busadressen konvertiert werden
            sDELWert:=WAsciiToISO646_De (sDELWert);

        // 24.02.2011 XML-Ersatzzeichen
        instlist:=instList+'&lt;curval addr=&quot;'+DELItem.DEL+'&quot;';

{$IFNDEF NO_XML_SIGVERIFYSTATE}
        // mit Signatur-Verifizierungsstatus; 04.01.2013, WW
        // 07.04.2014: auch für Gas-X
        instlist:=instlist + ' sigverifystate=&quot;'+ IntToStr (DELItem.SigVerifyStatus) +'&quot;';
{$ENDIF}

        instlist:=instList+'&gt;'+ EncodeXml(sDELWert)+'&lt;/curval&gt;'+CR+LF;
      end; // for DELLIst.count
      instlist:=instlist+'&lt;/curval_list&gt;'+CR+LF;

      if (EAdrMerker<>NUL) then begin
        { Rohdatenliste: 07.04.2014, WW }
        rawdatalist:=Get_Responsedata_Rohdaten (EAdrMerker,
           ResponseRohdatenList, false);
        if length (rawdatalist) > 0 then
          instlist:=instlist + rawdatalist;

        { TraceLog: 03.01.2022, WW }
        sTraceLog:=Get_Responsedata_TraceLog (EAdrMerker, ResponseTraceLogList);
        if length (sTraceLog) > 0 then
          instlist:=instlist + sTraceLog;
      end;

      instlist:=instlist+'&lt;/instance&gt;'+CR+LF;
    end;  // if (DELList.Count>0)
  end; // if DELList<>nil

  { TraceLogs für Instanzen, welche noch nicht ausgegeben wurden (zu denen keine
    DE vorliegen): 03.01.2022, WW }
  sUnerledigt:=Get_ResponseTraceLog_BusadressenUnerledigt (ResponseTraceLogList);
  for i:=1 to length (sUnerledigt) do begin
    EAdr:=sUnerledigt [i];
    instlist:=instlist+'&lt;instance busaddr=&quot;'+ EAdr +'&quot;&gt;'+CR+LF;
    sTraceLog:=Get_Responsedata_TraceLog (EAdr, ResponseTraceLogList);
    if length (sTraceLog) > 0 then
      instlist:=instlist + sTraceLog;
    instlist:=instlist+'&lt;/instance&gt;'+CR+LF;
  end;

  if length (instlist) > 0 then
    dsfglist:='&lt;dsfg&gt;'+CR+LF+instlist+'&lt;/dsfg&gt;';

  if VersionTyp = vs_GasX then begin
    // Responsedata für Gas-X-System:

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
    dsfglist:=dsfglist+CR+LF+sReturnExt;
  end;

  { Reponse-Log: }
  LogStr:='';
  if ResponseLogList <> nil then begin
    if ResponseLogList.Count > 0 then begin
      ResponseLogList.SortByBusadresse; // Liste nach Busadresse sortieren

      instlist:='';
      curvallog:='';
      rfilelist:='';
      EAdrMerker:=NUL;
      for i:=0 to ResponseLogList.Count -1 do begin
        if TObject (ResponseLogList [i]) is TDSfGDataKonvLogObj then  // Datenkonvertierung-Loglisteneintrag
          EAdr:=TDSfGDataKonvLogObj (ResponseLogList [i]).EAdr
        else if TObject (ResponseLogList [i]) is TRohfileLogObj then  // Rohfile-Loglisteneintrag
          EAdr:=TRohfileLogObj (ResponseLogList [i]).EAdr
        else  // unbekannter Loglisteneintrag
          Continue;

        if (EAdr <> EAdrMerker) then begin  // neue Instanz startet
          if (EAdrMerker <> NUL) then begin          // nicht beim ersten Datenelement
            if length (curvallog) > 0 then
              instlist:=instlist+CR+LF+curvallog;
            if length (rfilelist) > 0 then
              instlist:=instlist+CR+LF+'&lt;rfile_list&gt;'+CR+LF+rfilelist+'&lt;/rfile_list&gt;';

            // vorhergehende Instanz schließen
            instlist:=instlist+CR+LF+'&lt;/instance&gt;'+CR+LF;
          end;
          // nächste Instanz öffnen
          instlist:=instlist+'&lt;instance busaddr=&quot;'+EAdr+'&quot;&gt;';
          curvallog:='';
          rfilelist:='';

          EAdrMerker:=EAdr;
        end;

        if TObject (ResponseLogList [i]) is TDSfGDataKonvLogObj then begin  // Datenkonvertierung-Loglisteneintrag
          if length (curvallog) = 0 then  // nur 1 curvallog-Tag je Busadresse schreiben (keine Liste !)
            curvallog:='&lt;curvallog '+
                       'convstate=&quot;'+ IntToStr (TDSfGDataKonvLogObj (ResponseLogList [i]).KonvStatus) +
                       '&quot;/&gt;';
        end
        else if TObject (ResponseLogList [i]) is TRohfileLogObj then  // Rohfile-Loglisteneintrag
          rfilelist:=rfilelist+'&lt;rfile '+
                     'name=&quot;'+TRohfileLogObj (ResponseLogList [i]).RohfileName+'&quot;/&gt;'+CR+LF;
      end; // for ResponseLogList.Count

      if length (curvallog) > 0 then
        instlist:=instlist+CR+LF+curvallog;
      if length (rfilelist) > 0 then
        instlist:=instlist+CR+LF+'&lt;rfile_list&gt;'+CR+LF+rfilelist+'&lt;/rfile_list&gt;';

      // letzte Instanz schließen
      instlist:=instlist+CR+LF+'&lt;/instance&gt;'+CR+LF;
      LogStr:='&lt;dsfg&gt;'+CR+LF+instlist+'&lt;/dsfg&gt;';
    end;  // if ResponseLogList.Count > 0
  end;  // if ResponseLogList <> nil

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+
               Get_Member_Result(ResultStr)+
               Get_Member_hpsanswer(Cmd_Ret)+
               GET_Member_hpsdata (dsfglist)+
               GET_Member_wieserlog (LogStr)+
               XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{---------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_DSfG_BusAnalyse_Response(resultStr,Cmd_Ret:string;
                                                         DELList: TDELList;
                                                         Fehlergruppe, Fehlercode: integer;
                                                         sRetFreierText: string):string;
{---------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
Var
  instlist,dsfglist: string;
  i:integer;
  Datencode: integer;
  DELItem: TDELListObj;
  EAdrMerker:string[1];
  sDELWert: string;
  sReturnExt: string;

begin
  dsfglist:='';
  Datencode:=dc_KeineDaten;
  if (DELList<>Nil)  then begin
    if (DELList.Count>0) then begin
      Datencode:=dc_Parameter;

      instlist:='&lt;instance_list&gt;'+CR+LF;
      EAdrMerker:=NUL;
      for i:=0 to DELList.count -1 do begin
        Application.ProcessMessages;
        DELItem := TDELListObj(DELList[i]);
        if (DELItem.EAdr <> EAdrMerker) then begin  // neue Instanz startet
          if (EAdrMerker<>NUL) then begin          // nicht beim ersten Datenelement
            instlist:=instlist+'&lt;/md_list&gt;'+CR+LF+'&lt;/instance&gt;'+CR+LF;
          end;
          instlist:=instlist+'&lt;instance busaddr=&quot;'+ DELItem.EAdr+'&quot;&gt;'+CR+LF+'&lt;md_list&gt;'+CR+LF;
          EAdrMerker:= DELItem.EAdr;
        end;

        sDELWert:=DELItem.Wert;
        if bISO646 then  // Zeichen-Konvertierung ASCII -> ISO 646; 03.03.2010, WW
          if length (sDELWert) > 1 then  // zur Vermeidung, daß DSfG-Busadressen konvertiert werden
            sDELWert:=WAsciiToISO646_De (sDELWert);

        instlist:=instList+'&lt;md addr=&quot;'+DELItem.DEL+'&quot;&gt;'+
          EncodeXml(sDELWert)+'&lt;/md&gt;'+CR+LF;  // 24.03.2011
      end; // for DELLIst.count
      instlist:=instlist+'&lt;/md_list&gt;'+CR+LF+'&lt;/instance&gt;'+CR+LF+'&lt;/instance_list&gt;'+CR+LF;

      dsfglist:='&lt;dsfg&gt;'+CR+LF+instlist+'&lt;/dsfg&gt;';
    end;  // if (DELList.Count>0)
  end; // if DELList<>nil

  if VersionTyp = vs_GasX then begin
    // Responsedata für Gas-X-System:

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
    dsfglist:=dsfglist+CR+LF+sReturnExt;
  end;

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+
               Get_Member_Result(ResultStr)+
               Get_Member_hpsanswer(Cmd_Ret)+
               GET_Member_hpsdata (dsfglist)+
               XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{------------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_DSfGDfue_Mom_Response(resultStr,Cmd_Ret:string;
                                                      DSfGDfueParaList: TDfueParaList;
                                                      DSfGDfue_EAdr: string;
                                                      ResponseTraceLogList: TDSfGDataList;
                                                      Fehlergruppe, Fehlercode: integer;
                                                      sRetFreierText: string):string;
{------------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
Var
  instlist,dsfglist: string;
  wieserlist, basiclist: string;
  wparalist, ntymasklist: string;
  i:integer;
  Datencode: integer;
  ListItem: TDfueParaListObj;
  Nr: integer;
  sReturnExt: string;
  sWert: string;
  sTraceLog: string;

begin
  dsfglist:='';
  instlist:='';
  Datencode:=dc_KeineDaten;
  if DSfGDfueParaList <> nil then begin
    if DSfGDfueParaList.Count > 0 then begin
      Datencode:=dc_DSfGDfueDaten;
      DSfGDfueParaList.SortByBefehl_ParaAdr; // Liste nach Befehl, ParaAdr sortieren

      instlist:='&lt;instance busaddr=&quot;'+ DSfGDfue_EAdr +'&quot;&gt;'+CR+LF+
                '&lt;rdtval_list&gt;'+CR+LF;

      { zuerst alle Wieser-spezifischen DFÜ-Momentanwerte: }
      wieserlist:='';
      wparalist:='';
      for i:=0 to DSfGDfueParaList.Count-1 do begin
        Application.ProcessMessages;
        ListItem:=TDfueParaListObj (DSfGDfueParaList [i]);
        if Copy (ListItem.Befehl, 1, 2) = 'YW' then begin  { nur Wieser-spezifische Befehle }
          sWert:=EncodeXml(ListItem.Wert);  // Bugfix fehlende XML-Zeichenersetzung; 03.06.2020, WW
          if ListItem.Befehl = 'YWB' then   { Parameter }
            wparalist:=wparalist+'&lt;para par_id=&quot;'+ListItem.ParaAdr+'&quot;&gt;'+
              sWert+'&lt;/para&gt;'+CR+LF

          else if ListItem.Befehl = 'YWT' then  { aktive Teilnehmer }
            wieserlist:=wieserList+'&lt;activeusers&gt;'+ sWert+'&lt;/activeusers&gt;'+CR+LF;
        end;
      end;  // for

      if length (wparalist) > 0 then begin   { Wieser-Parameter sind vorhanden }
        wieserlist:=wieserlist+'&lt;para_list&gt;'+CR+LF;   { Beginn Wieser Parameterliste }
        wieserlist:=wieserlist+wparalist;
        wieserlist:=wieserlist+'&lt;/para_list&gt;'+CR+LF;  { Ende Wieser Parameterliste }
      end;

      if length (wieserlist) > 0 then begin   { Wieser-DFÜ-Momentanwerte sind vorhanden }
        instlist:=instlist+C_WieserStart+CR+LF;
        instlist:=instlist+wieserlist;
        instlist:=instlist+C_WieserEnd+CR+LF;
      end;

      { ...dann alle Norm-DFÜ-Momentanwerte: }
      basiclist:='';
      ntymasklist:='';
      for i:=0 to DSfGDfueParaList.Count-1 do begin
        Application.ProcessMessages;
        ListItem:=TDfueParaListObj (DSfGDfueParaList [i]);
        if Copy (ListItem.Befehl, 1, 2) <> 'YW' then begin  { keine Wieser-spezifischen Befehle }
          sWert:=EncodeXml(ListItem.Wert);  // Bugfix fehlende XML-Zeichenersetzung; 03.06.2020, WW
          if ListItem.Befehl = 'A' then  { NTY-Maske }
            ntymasklist:=ntymasklist+'&lt;ntymask dno=&quot;'+ListItem.ParaAdr+'&quot;&gt;'+
              sWert+'&lt;/ntymask&gt;'+CR+LF

          else if ListItem.Befehl = 'D' then begin { DFÜ-Statistik }
            { ParaAdr ist lfd. Zähler für in der D-Rohantwort enthaltene Werte: }
            Nr:=StrToInt (ListItem.ParaAdr);
            if Nr = 1 then
              basiclist:=basicList+'&lt;cin_nologin&gt;'+ sWert+'&lt;/cin_nologin&gt;'+CR+LF
            else if Nr = 2 then
              basiclist:=basicList+'&lt;cin_loginerror&gt;'+ sWert+'&lt;/cin_loginerror&gt;'+CR+LF
            else if Nr = 3 then
              basiclist:=basicList+'&lt;cout_failed&gt;'+ sWert+'&lt;/cout_failed&gt;'+CR+LF
            else if Nr = 4 then
              basiclist:=basicList+'&lt;cout_nologin&gt;'+ sWert+'&lt;/cout_nologin&gt;'+CR+LF
            else if Nr = 5 then
              basiclist:=basicList+'&lt;cout_loginerror&gt;'+ sWert+'&lt;/cout_loginerror&gt;'+CR+LF;
          end

          else if ListItem.Befehl = 'E' then  { Busadresse, über die das Login erfolgte }
            basiclist:=basicList+'&lt;busaddr&gt;'+ sWert+'&lt;/busaddr&gt;'+CR+LF

          else if ListItem.Befehl = 'I' then  { Passwort, über die das Login erfolgte }
            basiclist:=basicList+'&lt;passw&gt;'+ sWert+'&lt;/passw&gt;'+CR+LF

          else if ListItem.Befehl = 'K' then  { Kennung }
            basiclist:=basicList+'&lt;ident&gt;'+ sWert+'&lt;/ident&gt;'+CR+LF

          else if ListItem.Befehl = 'R' then  { Rufnummer der Zentrale, über die das Login erfolgte }
            basiclist:=basicList+'&lt;callno_cs&gt;'+ sWert+'&lt;/callno_cs&gt;'+CR+LF

          else if ListItem.Befehl = 'U' then begin { Zeitinfos }
            { ParaAdr ist lfd. Zähler für in der U-Rohantwort enthaltene Werte: }
            Nr:=StrToInt (ListItem.ParaAdr);
            if Nr = 1 then
              basiclist:=basicList+'&lt;currdate&gt;'+ sWert+'&lt;/currdate&gt;'+CR+LF
            else if Nr = 2 then
              basiclist:=basicList+'&lt;currtime&gt;'+ sWert+'&lt;/currtime&gt;'+CR+LF;
          end

          else if ListItem.Befehl = 'V' then begin { Versionsdaten }
            { ParaAdr ist lfd. Zähler für in der V-Rohantwort enthaltene Werte: }
            Nr:=StrToInt (ListItem.ParaAdr);
            if Nr = 1 then
              basiclist:=basicList+'&lt;manufact&gt;'+ sWert+'&lt;/manufact&gt;'+CR+LF
            else if Nr = 2 then
              basiclist:=basicList+'&lt;progname&gt;'+ sWert+'&lt;/progname&gt;'+CR+LF
            else if Nr = 3 then
              basiclist:=basicList+'&lt;progversion&gt;'+ sWert+'&lt;/progversion&gt;'+CR+LF
            else if Nr = 4 then
              basiclist:=basicList+'&lt;date_lpc&gt;'+ sWert+'&lt;/date_lpc&gt;'+CR+LF
            else if Nr = 5 then
              basiclist:=basicList+'&lt;time_lpc&gt;'+ sWert+'&lt;/time_lpc&gt;'+CR+LF;
          end;
        end;
      end;  // for

      if length (ntymasklist) > 0 then begin   { NTY-Masken sind vorhanden }
        basiclist:=basiclist+'&lt;ntymask_list&gt;'+CR+LF;  { Beginn NTY-Maskenliste }
        basiclist:=basiclist+ntymasklist;
        basiclist:=basicList+'&lt;/ntymask_list&gt;'+CR+LF; { Ende NTY-Maskenliste }
      end;

      if length (basiclist) > 0 then begin   { Norm-DFÜ-Momentanwerte sind vorhanden }
        instlist:=instlist+C_DSfGBasicStart+CR+LF;
        instlist:=instlist+basiclist;
        instlist:=instlist+C_DSfGBasicEnd+CR+LF;
      end;

      instlist:=instlist+'&lt;/rdtval_list&gt;'+CR+LF;

      { TraceLog: 03.01.2022, WW }
      sTraceLog:=Get_Responsedata_TraceLog (DSfGDfue_EAdr, ResponseTraceLogList);
      if length (sTraceLog) > 0 then
        instlist:=instlist + sTraceLog;

      instlist:=instlist+'&lt;/instance&gt;'+CR+LF;
    end;  // if DSfGDfueParaList.Count > 0
  end;  // if DSfGDfueParaList <> nil

  { TraceLog für DFÜ-Instanz, wenn noch nicht ausgegeben (es liegen keine DFÜ-
    Parameter vor): 03.01.2022, WW }
  if length (instlist) = 0 then begin
    instlist:='&lt;instance busaddr=&quot;'+ DSfGDfue_EAdr +'&quot;&gt;'+CR+LF;
    sTraceLog:=Get_Responsedata_TraceLog (DSfGDfue_EAdr, ResponseTraceLogList);
    if length (sTraceLog) > 0 then
      instlist:=instlist + sTraceLog;
    instlist:=instlist+'&lt;/instance&gt;'+CR+LF;
  end;

  if length (instlist) > 0 then
    dsfglist:='&lt;dsfg&gt;'+CR+LF+instlist+'&lt;/dsfg&gt;';
                                                   
  if VersionTyp = vs_GasX then begin
    // Responsedata für Gas-X-System:

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
    dsfglist:=dsfglist+CR+LF+sReturnExt;
  end;

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+
               Get_Member_Result(ResultStr)+
               Get_Member_hpsanswer(Cmd_Ret)+
               GET_Member_hpsdata (dsfglist)+
               XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{------------------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_ZeitSync_Response(resultStr,Cmd_Ret:string;
                                                  ZeitSyncInfoData: TZeitSyncInfoData;
                                                  MRGKennung:string; MRGTyp:integer;
                                                  ResponseTraceLogList: TDSfGDataList;
                                                  Fehlergruppe, Fehlercode: integer;
                                                  sRetFreierText: string):string;
{------------------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
var
  zslist, mrglist: string;
  Datencode: integer;
  sReturnExt: string;
  sTraceLog: string;

begin
  // Responsedata für Wieser-System
  // WW: ab 24.02.2020 auch für Gas-X-System
  Datencode:=dc_ZeitSyncDaten;
  zslist:=CR+LF+C_ZeitSynclisteStart+CR+LF;
  with ZeitSyncInfoData do begin
    // Kennzeichnung, ob ZeitSync-Infodaten abgefragt wurden und somit zurück-
    // gegeben werden können, erfolgt anhand von 'DZ_Server':
    if DZ_Server > 0 then
      zslist:=zslist+
              C_DZ_ServerStart+FormatDateTime ('yyyymmddhhnnss', DZ_Server)+C_DZ_ServerEnd+CR+LF+
              C_MRGDZ_MRGStart+FormatDateTime ('yyyymmddhhnnss', DZ_Geraet)+C_MRGDZ_MRGEnd+CR+LF;
  end;
  zsList:=zsList+C_ZeitSynclisteEnd+CR+LF;

  { TraceLog: 03.01.2022, WW }
  sTraceLog:=Get_Responsedata_TraceLog ('', ResponseTraceLogList);
  if length (sTraceLog) > 0 then
    zsList:=zsList + sTraceLog;

  mrglist:=GET_Member_hpsdata_MRG(MRGKennung, MRGTyp);
  mrglist:=mrglist+zslist;
  mrglist:=mrglist+C_MRGBlockEnd+CR+LF+C_WieserEnd;

  // Erweiterte Ergebnis-Rückgaben für Gas-X-System; 13.03.2018, WW
  if VersionTyp = vs_GasX then
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText)
  else
    sReturnExt:='';

  if sReturnExt <> '' then
    mrglist:=mrglist+CR+LF+sReturnExt;

  mrglist:=mrglist+CR+LF+'</value></member>';

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+Get_Member_Result(ResultStr)+Get_Member_hpsanswer(Cmd_Ret);
  if length (mrglist) > 0 then
    XMLResponse:=XMLResponse + mrglist;
  XMLResponse:=XMLResponse + XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{------------------------------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_Parametrieren_Response (resultStr,Cmd_Ret:string;
                                                        ParaEinstellResultData: TParaEinstellResultData;
                                                        MRGKennung:string; MRGTyp:integer;
                                                        DSfGDfue_EAdr: string;
                                                        ResponseTraceLogList: TDSfGDataList;
                                                        Fehlergruppe, Fehlercode: integer;
                                                        sRetFreierText: string):string;
{------------------------------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
var
  pelist, datalist: string;
  Datencode: integer;
  sWertAlt: string;
  sWertNeu: string;
  sReturnExt: string;
  sTraceLog: string;

begin
  Datencode:=dc_KeineDaten;
  datalist:='';     // Vorbelegung: keine XML-Parametrier-Ergebnisdaten

  if ParaEinstellResultData.ParaTyp = C_CmdParatyp_MRG then begin  { MRG-Parametrierung }
    if length (ParaEinstellResultData.ParaAdr) > 0 then begin  { Ergebnisdaten sind vorhanden }
      Datencode:=dc_ParaEinstell;
      pelist:=CR+LF+C_ParaAendListeStart+CR+LF;
      pelist:=pelist+C_lt_Subst+C_ParaAend+' '+
                     C_MrgKennungAllgParamNr +C_quot_Subst+ParaEinstellResultData.ParaAdr+C_quot_Subst+' '+
                     // Bugfix fehlende XML-Zeichenersetzung; 03.06.2020, WW
                     C_ParaAendWertAlt+C_quot_Subst+ EncodeXml(ParaEinstellResultData.WertAlt) +C_quot_Subst+' '+
                     C_ParaAendWertNeu+C_quot_Subst+ EncodeXml(ParaEinstellResultData.WertNeu) +C_quot_Subst+
                     '/'+C_gt_Subst+CR+LF;  // Bugfix fehlender Abschluß /; 18.07.2018, WW
      peList:=peList+C_ParaAendListeEnd+CR+LF;
    end else
      pelist:='';

    { TraceLog: 03.01.2022, WW }
    sTraceLog:=Get_Responsedata_TraceLog ('', ResponseTraceLogList);
    if length (sTraceLog) > 0 then
      pelist:=peList + sTraceLog;

    if length (pelist) > 0 then begin  // 03.01.2022, WW
      datalist:=GET_Member_hpsdata_MRG(MRGKennung, MRGTyp);
      datalist:=datalist+pelist;
      datalist:=datalist+C_MRGBlockEnd+CR+LF+C_WieserEnd;
    end;

    sReturnExt:='';
    if VersionTyp = vs_GasX then begin
      // Responsedata für Gas-X-System:

      // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
      sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
    end;

    if datalist <> '' then begin
      if sReturnExt <> '' then
        datalist:=datalist+CR+LF+sReturnExt;

      datalist:=datalist+CR+LF+'</value></member>';
    end
    else begin
      if sReturnExt <> '' then
        datalist:=GET_Member_hpsdata(sReturnExt);
    end;
  end
  else if ParaEinstellResultData.ParaTyp = C_CmdParatyp_DSfG then begin  { DSfG-Parametrierung }
    if length (ParaEinstellResultData.ParaAdr) > 0 then begin  { Ergebnisdaten sind vorhanden }
      Datencode:=dc_ParaEinstell;

      sWertAlt:=EncodeXml(ParaEinstellResultData.WertAlt);  // Bugfix fehlende XML-Zeichenersetzung; 03.06.2020, WW
      if bISO646 then  // Zeichen-Konvertierung ASCII -> ISO 646; 03.03.2010, WW
        if length (sWertAlt) > 1 then  // zur Vermeidung, daß DSfG-Busadressen konvertiert werden
          sWertAlt:=WAsciiToISO646_De (sWertAlt);

      sWertNeu:=EncodeXml(ParaEinstellResultData.WertNeu);  // Bugfix fehlende XML-Zeichenersetzung; 03.06.2020, WW
      if bISO646 then  // Zeichen-Konvertierung ASCII -> ISO 646; 03.03.2010, WW
        if length (sWertNeu) > 1 then  // zur Vermeidung, daß DSfG-Busadressen konvertiert werden
          sWertNeu:=WAsciiToISO646_De (sWertNeu);

      // 03.01.2022, WW
      pelist:=C_ParaAendListeStart+CR+LF+
                C_lt_Subst+C_ParaAend+' '+C_DSfGKennungDEA+C_quot_Subst+ ParaEinstellResultData.ParaAdr + C_quot_Subst+ ' ' +
                C_ParaAendWertAlt+C_quot_Subst + sWertAlt + C_quot_Subst+' ' +
                C_ParaAendWertNeu+C_quot_Subst + sWertNeu + C_quot_Subst+
                '/'+C_gt_Subst+CR+LF+  // Bugfix fehlender Abschluß /; 18.07.2018, WW
              C_ParaAendListeEnd+CR+LF;
    end else
      pelist:='';

    { TraceLog: 03.01.2022, WW }
    sTraceLog:=Get_Responsedata_TraceLog (ParaEinstellResultData.BAdr, ResponseTraceLogList);
    if length (sTraceLog) > 0 then
      pelist:=peList + sTraceLog;

    if length (pelist) > 0 then begin  // 03.01.2022, WW
      pelist:=C_DSfGBlockStart+CR+LF+
              C_DSfGInstanzStart +C_DSfGKennungIAdr+C_quot_Subst+ ParaEinstellResultData.BAdr+C_quot_Subst+C_gt_Subst+CR+LF+
              pelist +
              C_DSfGInstanzEnd+CR+LF+
              C_DSfGBlockEnd;
    end;

    if VersionTyp = vs_GasX then begin
      // Responsedata für Gas-X-System:

      // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
      sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
      if length (pelist) > 0 then  // 03.01.2022, WW
        pelist:=pelist+CR+LF;
      pelist:=pelist+sReturnExt;
    end;

    datalist:=GET_Member_hpsdata (pelist);
  end
  else if ParaEinstellResultData.ParaTyp = C_CmdParatyp_DSfGDfue then begin  { DSfG-DFÜ-Parametrierung }
    if length (ParaEinstellResultData.BAdr) > 0 then begin  { Ergebnisdaten sind vorhanden; 03.01.2022, WW }
      // Bugfix: ParaAdr darf leer sein (Normparameter !); 13.03.2018, WW
      Datencode:=dc_ParaEinstell;
      pelist:=C_DSfGDfueParaAendListeStart+CR+LF+
                C_lt_Subst+C_ParaAend+' '+C_DSfGDfueParaAend_BAdr+C_quot_Subst + ParaEinstellResultData.BAdr + C_quot_Subst+' ' +
                C_DSfGDfueParaAend_ParaAdr+C_quot_Subst + ParaEinstellResultData.ParaAdr + C_quot_Subst+' ' +
                // Bugfix fehlende XML-Zeichenersetzung; 03.06.2020, WW
                C_ParaAendWertAlt+C_quot_Subst + EncodeXml(ParaEinstellResultData.WertAlt) + C_quot_Subst+' ' +
                C_ParaAendWertNeu+C_quot_Subst + EncodeXml(ParaEinstellResultData.WertNeu) + C_quot_Subst+
                '/'+C_gt_Subst+CR+LF+  // Bugfix fehlender Abschluß /; 18.07.2018, WW
              C_DSfGDfueParaAendListeEnd+CR+LF;
    end else
      pelist:='';

    { TraceLog: 03.01.2022, WW }
    sTraceLog:=Get_Responsedata_TraceLog (DSfGDfue_EAdr, ResponseTraceLogList);
    if length (sTraceLog) > 0 then
      pelist:=peList + sTraceLog;

    if length (pelist) > 0 then begin  // 03.01.2022, WW
      pelist:=C_DSfGBlockStart+CR+LF+
              C_DSfGInstanzStart+C_DSfGKennungIAdr+C_quot_Subst+ DSfGDfue_EAdr +C_quot_Subst+C_gt_Subst+CR+LF+
              pelist +
              C_DSfGInstanzEnd+CR+LF+
              C_DSfGBlockEnd;
    end;

    if VersionTyp = vs_GasX then begin
      // Responsedata für Gas-X-System:

      // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
      sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
      if length (pelist) > 0 then  // 03.01.2022, WW
        pelist:=pelist+CR+LF;
      pelist:=pelist+sReturnExt;
    end;

    datalist:=GET_Member_hpsdata (pelist);
  end;

  // Auch wenn keine Parametrier-Ergebnisdaten vorhanden sind, müssen für Gas-X
  // erweiterte Ergebnis-Rückgaben enthalten sein:
  if datalist = '' then begin
    if VersionTyp = vs_GasX then begin
      // Responsedata für Gas-X-System:

      // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
      sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
      datalist:=GET_Member_hpsdata (sReturnExt);
    end;
  end;  

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+Get_Member_Result(ResultStr)+Get_Member_hpsanswer(Cmd_Ret)+datalist+XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{------------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_Transparent_Response (resultStr,Cmd_Ret:string;
                                                      TransparentAntw: string;
                                                      Fehlergruppe, Fehlercode: integer;
                                                      sRetFreierText: string):string;
{------------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
var
  transpdata: string;
  Datencode: integer;
  sReturnExt: string;

begin
  Datencode:=dc_Transparent;
  transpdata:=C_WieserStart+CR+LF+
              C_TransparentAntwStart + TransparentAntw + C_TransparentAntwEnd+CR+LF+
              C_WieserEnd;

  if VersionTyp = vs_GasX then begin
    // Responsedata für Gas-X-System:

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
    transpdata:=transpdata+CR+LF+sReturnExt;
  end;

  transpdata:=GET_Member_hpsdata (transpdata);

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+Get_Member_Result(ResultStr)+Get_Member_hpsanswer(Cmd_Ret)+transpdata+XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{--------------------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_Ruf_ME_VI_Response (resultStr,Cmd_Ret: string;
                                                    Rufcode: integer;
                                                    Meldungsliste: TMeldungsliste;
                                                    VerbInfoData: TVerbInfoData;
                                                    MRGKennung:string; MRGTyp:integer;
                                                    Fehlergruppe, Fehlercode: integer;
                                                    sRetFreierText: string):string;
{--------------------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
Var
  mrglist: string;
  Datencode: integer;
  LogStr: string;

begin
  LogStr:='';

  // Responsedata:
  mrglist:=GET_Responsedata_ME (Meldungsliste, MRGKennung, MRGTyp, nil, -1, -1,
                                Fehlergruppe, Fehlercode, sRetFreierText,
                                Datencode);

  // Responselog nur für Wieser-System:
  if VersionTyp = vs_Wieser then begin
    // Verbindungsinformationen: 03.12.2013, WW
    LogStr:=Get_Responselog_VerbInfoData (VerbInfoData);
  end;  { if VersionTyp }

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator + IntToStr (Rufcode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+Get_Member_Result(ResultStr)+Get_Member_hpsanswer(Cmd_Ret)+mrglist;
  if length (LogStr) > 0 then
    XMLResponse:=XMLResponse + GET_Member_wieserlog (LogStr);
  XMLResponse:=XMLResponse + XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;


{-----------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_SMS_MWTA_Response(resultStr,Cmd_Ret: string;
                                                  Rufcode: integer;
                                                  MwFileName: string;
                                                  TaFileName: string;
                                                  MaxMessKanal, MaxTagKanal: integer;
                                                  Tagesende: integer;
                                                  MRGKennung:string; MRGTyp:integer;
                                                  Fehlergruppe, Fehlercode: integer;
                                                  sRetFreierText: string):string;
{-----------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
Var
  mrglist: string;
  Datencode: integer;
  MwFileNameList: TStringList;
  TaFileNameList: TStringList;

begin
  { MW/TA-Responsedata für SMS-Daten erzeugen:
    -> Rückgabe der Original-Messwertdaten (Konvertierung in LGZ-Format nicht möglich,
       da notwendige Stammdaten-Kanal-Informationen bei Rohdaten aus SMS nicht zur
       Verfügung stehen !)
    -> Zeitfilter deaktiviert, da kein Soll-Zeitbereich existiert (von, bis = -1 übergeben }
  MwFilenameList:=TStringList.Create;
  try
    TaFilenameList:=TStringList.Create;
     try
       MwFilenameList.Add (MwFileName);
       TaFilenameList.Add (TaFileName);
       mrglist:=GET_Responsedata_MWTA (MwFileNameList, TaFileNameList,
                                       MaxMessKanal, MaxTagKanal, 0, 0,
                                       Tagesende,
                                       MRGKennung, MRGTyp, nil, -1, -1,
                                       true,  // die berechneten Stundenmengen zurückliefern (in RohSRec.Kanal)
                                       true,   // ...und zusätzlich die Originalwerte (Zählerstände)
                                       Fehlergruppe, Fehlercode, sRetFreierText,
                                       Datencode);
    finally
      TaFilenameList.Free;
    end;
  finally
    MwFilenameList.Free;
  end;

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator + IntToStr (Rufcode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+Get_Member_Result(ResultStr)+Get_Member_hpsanswer(Cmd_Ret)+mrglist+XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{-------------------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_Rufliste_Response (resultStr,Cmd_Ret:string;
                                                   Rufliste: string;
                                                   MRGKennung:string; MRGTyp:integer;
                                                   Fehlergruppe, Fehlercode: integer;
                                                   sRetFreierText: string):string;
{-------------------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
var
  S: string;
  Datencode: integer;
  sReturnExt: string;

begin
  Datencode:=dc_MRG_Rufliste;

  S:=GET_Member_hpsdata_MRG(MRGKennung, MRGTyp);
  S:=S+CR+LF+C_MRGRuflisteStart + Rufliste + C_MRGRuflisteEnd+CR+LF;
  S:=S+'&lt;/mrg&gt;'+CR+LF+'&lt;/wieser&gt;';

  sReturnExt:='';
  if VersionTyp = vs_GasX then begin
    // Responsedata für Gas-X-System:

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
  end;

  if S <> '' then begin
    if sReturnExt <> '' then
      S:=S+CR+LF+sReturnExt;

    S:=S+CR+LF+'</value></member>';
  end
  else begin
    if sReturnExt <> '' then
      S:=GET_Member_hpsdata(sReturnExt);
  end;

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+Get_Member_Result(ResultStr)+Get_Member_hpsanswer(Cmd_Ret)+S+XML_FIX_END;
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

{---------------------------------------------------------------------------------------------}
Function TEnCodeXMLRESP.GET_XML_RufannahmeDSfG_Response (resultStr,Cmd_Ret: string;
                                                         AufmTelegrammList: TAufmTelegrammList;
                                                         RDeakt_RufNrZentrale_Alt: string;
                                                         RDeakt_Fehlergruppe,
                                                         RDeakt_Fehlercode: integer;
                                                         DSfGDfue_EAdr: string;
                                                         VerbInfoData: TVerbInfoData;
                                                         Fehlergruppe, Fehlercode: integer;
                                                         sRetFreierText: string): string;
{---------------------------------------------------------------------------------------------}
{ -> Datencode wird hier ermittelt und an Cmd_Ret angehängt }
Var
  instlist,dsfglist: string;
  i:integer;
  Datencode: integer;
  EAdrMerker: string[1];
  TelegrItem: TAufmTelegrammListObj;
  DTStr: string;
  OK: boolean;
  HasAufmTelegramme: boolean;
  HasRufDeaktData: boolean;
  RufDeaktData_geschrieben: boolean;
  LogStr: string;
  sReturnExt: string;

  {--------------------------------}
  procedure Insert_XML_RufDeaktData;
  {--------------------------------}
  begin
    instlist:=instlist+C_RufDeaktListeStart+CR+LF;
    instlist:=instlist+
              C_ErrorgroupStart+IntToStr(RDeakt_Fehlergruppe)+C_ErrorgroupEnd+CR+LF+
              C_ErrorcodeStart+IntToStr(RDeakt_Fehlercode)+C_ErrorcodeEnd+CR+LF;
    // Kennzeichnung, daß alte Rufnummer der Zentrale abgefragt wurde und somit zurück-
    // gegeben werden können:
    if length (RDeakt_RufNrZentrale_Alt) > 0 then
      instlist:=instlist+
                C_DSfGTelNrZentraleStart+RDeakt_RufNrZentrale_Alt+C_DSfGTelNrZentraleEnd+CR+LF;

    instlist:=instlist+C_RufDeaktListeEnd+CR+LF;
  end;

begin
  dsfglist:='';
  LogStr:='';
  Datencode:=dc_KeineDaten;

  // Responsedata, Responselog nur für Wieser-System:
  if VersionTyp = vs_Wieser then begin
    // Responsedata:
    HasAufmTelegramme:=false;
    if AufmTelegrammList <> nil then begin
      if AufmTelegrammList.Count > 0 then begin
        HasAufmTelegramme:=true;
        Datencode:=Datencode + dc_DSfGAufmTelegr;
        AufmTelegrammList.SortByBusadresse; // Liste nach Busadresse sortieren
      end;
    end;

    HasRufDeaktData:=false;
    // optionale Rufdeaktivierungs-Rückgabedaten werden unter der Adresse der Login-DFÜ eingetragen:
    // -> Kennzeichnung, daß Rufdeaktivierung durchgeführt wurde und somit Ergebnis
    //    zurückgegeben werden kann, erfolgt anhand von 'Rufdeaktivierung-Fehlergruppe/-code':
    if (RDeakt_Fehlergruppe > -1) AND (RDeakt_Fehlercode > -1) then begin
      HasRufDeaktData:=true;
      Datencode:=Datencode + dc_RufDeaktDaten;
    end;
    RufDeaktData_geschrieben:=false;

    if HasAufmTelegramme OR HasRufDeaktData then begin
      instlist:='';

      if HasAufmTelegramme then begin
        EAdrMerker:=NUL;
        for i:=0 to AufmTelegrammList.Count-1 do begin
          Application.ProcessMessages;
          TelegrItem:=TAufmTelegrammListObj (AufmTelegrammList [i]);
          if TelegrItem.Busadresse <> EAdrMerker then begin  // neue Instanz startet
            if EAdrMerker <> NUL then      // nicht beim ersten Telegramm
              instlist:=instlist+'&lt;/atttelegr_list&gt;'+CR+LF+'&lt;/instance&gt;'+CR+LF;
            instlist:=instlist+'&lt;instance busaddr=&quot;'+TelegrItem.Busadresse+'&quot;&gt;'+CR+LF+'&lt;atttelegr_list&gt;'+CR+LF;
            EAdrMerker:=TelegrItem.Busadresse;

            { Rufdeaktivierungs-Daten zur aktuellen Busadresse miteinfügen: }
            if HasRufDeaktData AND (EAdrMerker = DSfGDfue_EAdr) then begin
              Insert_XML_RufDeaktData;
              RufDeaktData_geschrieben:=true;
            end;
          end;

          try
            { Datum/Zeit ist optional: }
            if TelegrItem.DatumZeit > -1 then
              DTStr:=FormatDateTime ('yyyymmddhhnnss', TelegrItem.DatumZeit)
            else
              DTStr:='';
            OK:=true;
          except
            OK:=false;  { sicherheitshalber abfangen, man weiß ja nie... }
          end;
          if OK then
            instlist:=instList+'&lt;atttelegr nty=&quot;'+TelegrItem.Nachrichtentyp+'&quot; '+
                               'ts=&quot;'+DTStr+'&quot; tz=&quot;'+TelegrItem.Zeitzone+'&quot;/&gt;'+CR+LF;
        end; // for TelegrDELLIst.count
        instlist:=instlist+'&lt;/atttelegr_list&gt;'+CR+LF+'&lt;/instance&gt;'+CR+LF;
      end;  // if HasAufmTelegramme

      // wenn Rufdeaktivierungs-Daten noch nicht geschrieben wurden, jetzt unter
      // eigenem Instanz-Tag anhängen:
      if HasRufDeaktData AND not RufDeaktData_geschrieben then begin
        instlist:=instlist + C_DSfGInstanzStart+C_DSfGKennungIAdr+
                  C_quot_Subst+ DSfGDfue_EAdr+C_quot_Subst+C_gt_Subst+CR+LF;
        Insert_XML_RufDeaktData;
        instlist:=instlist+C_DSfGInstanzEnd+CR+LF;
      end;
      dsfglist:='&lt;dsfg&gt;'+CR+LF+instlist+'&lt;/dsfg&gt;';
    end; // if HasAufmTelegramme OR HasRufDeaktData

    // Responselog:
    // Verbindungsinformationen: 03.12.2013, WW
    LogStr:=Get_Responselog_VerbInfoData (VerbInfoData);
  end
  else begin
    // Responsedata für Gas-X-System:

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
    dsfglist:=sReturnExt;
  end;       

  Cmd_Ret:=Cmd_Ret + IntToStr (Datencode) + C_RespSeparator;
  XMLResponse:=XML_FIX_Start+
               Get_Member_Result(ResultStr)+
               Get_Member_hpsanswer(Cmd_Ret);
  if length (dsfglist) > 0 then
    XMLResponse:=XMLResponse + GET_Member_hpsdata (dsfglist);
  if length (LogStr) > 0 then
    XMLResponse:=XMLResponse + GET_Member_wieserlog (LogStr);
  XMLResponse:=XMLResponse + XML_FIX_END;   
  SOAPResponse:=Build_SOAPResponse;
  if DebXML then
    Write_XMLResponse;
  Result:=SOAPResponse;
end;

// Schreibt XML-Antwort auf ZeitAbruf
// Parameter: Antwortstrings, Kennung (MRG-Kennung bzw. DSfG-Busadresse),
//            Zeit im UNIX-Format, MEZ/MESZ, UTC
// Rückgabe: XML-Antwort
//---------------------------------------------------
function TEnCodeXMLRESP.GET_XML_ZeitAbruf_Response (ResultStr, Cmd_Ret: string;
  sKennung, sUnixTime, sTimeBias, sTimeInfo: string;
  ResponseTraceLogList: TDSfGDataList;
  Fehlergruppe, Fehlercode: integer;
  sRetFreierText: string): string;  // 17.08.2010 // 13.01.2011
//---------------------------------------------------
var
  S, sDT, sKng : string;
  dt : TDateTime;
  iDatencode: integer;
  sReturnExt: string;
  sTraceLog: string;

begin
  iDatencode := dc_KeineDaten;

  try
    UnixSekundenToDateTime(StrToInt('$' + sUnixTime), dt);
    sDT := FormatDateTime('yyyymmddhhnnss', dt);
  except
    sDT := '';
  end;

  S := '&lt;time ts=&quot;' + sDT + '&quot; zone=&quot;' + sTimeBias +
       '&quot; tz=&quot;' + sTimeInfo + '&quot;/&gt;' + CR + LF;

  sKng := sKennung;
  if (Pos(#9, sKng) > 1) then begin  // Ist MRG
    { TraceLog: 03.01.2022, WW }
    sTraceLog:=Get_Responsedata_TraceLog ('', ResponseTraceLogList);
    if length (sTraceLog) > 0 then
      S:=S + sTraceLog;

    // Bei MRG werden Kennung und Typ durch #9 getrennt
    S := C_WieserStart + CR + LF +
        C_MRGBlockStart +
          C_MrgKennungKennung + C_quot_Subst + GetStringPart(sKng, 1, #9) +
            C_quot_Subst + ' ' +
          C_MrgKennungDatentypen + C_quot_Subst + GetStringPart(sKng, 2, #9) +
            C_quot_Subst +
        C_gt_Subst + CR + LF + S + C_MRGBlockEnd +
      CR + LF + C_WieserEnd;
  end
  else begin  // Ist DSfG
    { TraceLog: 03.01.2022, WW }
    sTraceLog:=Get_Responsedata_TraceLog (sKennung, ResponseTraceLogList);
    if length (sTraceLog) > 0 then
      S:=S + sTraceLog;

    S := C_DSfGBlockStart + CR + LF +
      C_DSfGInstanzStart + C_DSfGKennungIAdr + C_quot_Subst + sKennung +
        C_quot_Subst + C_gt_Subst + CR + LF + S +
      C_DSfGInstanzEnd + CR + LF + C_DSfGBlockEnd;
  end;

  if VersionTyp = vs_GasX then begin
    // Responsedata für Gas-X-System:

    // Erweiterte Ergebnis-Rückgaben; 13.03.2018, WW
    sReturnExt:=Get_Responsedata_ReturnExt (Fehlergruppe, Fehlercode, sRetFreierText);
    S:=S+CR+LF+sReturnExt;
  end;  

  S := GET_Member_hpsdata (S);

  Cmd_Ret := Cmd_Ret + IntToStr(iDatencode) + C_RespSeparator;
  XMLResponse := XML_FIX_Start + Get_Member_Result(ResultStr) +
    Get_Member_hpsanswer(Cmd_Ret) + S + XML_FIX_END;
  SOAPResponse := Build_SOAPResponse;
  if (DebXML) then Write_XMLResponse;
  Result := SOAPResponse;
end;

{-----------------------------------------}
Procedure TEnCodeXMLRESP.Write_XMLResponse;
{-----------------------------------------}
var
  FName: string;
  DTStr: string;
  FS: TFileStream;
  l: integer;
begin
  try
    DateTimeToString (DTStr, 'yyyymmdd_hhmmsszzz', Now);
    FName:=debXML_Dir + DTStr + ext_Res_Xml;
    FS:=TFileStream.Create (FName, fmCreate);
    try
      l:=length (XMLResponse);
      if l > 0 then
        FS.Write (XMLResponse [1], l);
    finally
      FS.Free;
    end;
  except
  end;
end;


{------------------------------------------------------------------------------}

{ TDecodeXMLREQ }

{-------------------------------------------------------------------}
Constructor TDecodeXMLREQ.Create(ADebXML:Boolean;ADebXML_DIR:string);
{-------------------------------------------------------------------}
begin
  inherited Create;
  DebXML:=ADebXML;
  DebXML_DIR:=ADebXML_DIR;
end;

{----------------------------------------------------}
procedure TDecodeXMLREQ.SetRequest (ARequest: string);
{----------------------------------------------------}
begin
  REQ:=ARequest;
  if DebXML then
    Write_XMLRequest;
end;

{---------------------------------------------------------------}
function TDecodeXMLREQ.isCompleteXML (ARequest: string): boolean;
{---------------------------------------------------------------}
begin
  Result:=Pos('</methodCall>', ARequest) > 0;  { mit </methodCall> ist der XML-String abgeschlossen }
end;

{----------------------------------------------------------------}
Function TDecodeXMLREQ.isCorrectXML (sBasicAuthentication: string;
  var sErrInfo: string): integer;
{----------------------------------------------------------------}
Var
  i:Integer;
  ctnlenstr:string;
  st:string;
  P: integer;
  sBase64: string;

{$IFNDEF NO_XMLHEADERLENGTH_CHECK}
  XMLHeader_Len, DatLEn: integer;
{$ENDIF}

begin
  sErrInfo:='';  // Vorbelegung Rückgabe;

  // Laengenprüfung Header - Daten
  P:=Pos('Content-Length: ', REQ);
  st:='';
  if P > 0 then
    st:=Copy (REQ, P + 16, length (REQ));  // ab start der Contentlength

  ctnlenstr:='';
  i:=1;
  while i <= length (st) do begin
    if (st[i]>#47) and (st[i]<#58) then  // nur '0'..'9'
      ctnlenStr:=ctnlenStr+st[i]
    else
      Break;
   inc(i);
  end;
  if (Length(ctnLenStr)<=0) then begin
    result:=ERR_LENGTH_HEADER;
    sErrInfo:='Content-Length im Header fehlt';  // 03.06.2020, WW
    exit;
  end;

{$IFNDEF NO_XMLHEADERLENGTH_CHECK}
  XMLHeader_Len:=StrToInt(ctnLenStr);
  P:=Pos('<?xml', REQ);
  st:='';
  if P > 0 then
    st:=Copy (REQ, P, length (REQ));
  DatLen:=Length(st);

  // Fehler Header Length
  if XMLHeader_Len<>DatLen then begin
   result:=ERR_LENGTH;
   sErrInfo:='Content-Length im Header: ' + ctnlenstr +
             ', Telegrammlänge: ' + IntToStr (DatLen);  // 07.04.2014, WW
   exit;
  end;
{$ENDIF}

  // Optionale Basis-Authentifizierung; 03.06.2020, WW
  if sBasicAuthentication <> '' then begin
    sBase64:=EncodeString (sBasicAuthentication);  // Base64-codieren

    // Fehler Basis-Authentifizierung
    P:=Pos('Authorization: Basic ' + sBase64, REQ);
    if P = 0 then begin
       result:=ERR_BASICAUTHENTICATION;
       sErrInfo:='Authorization: Basic im Header fehlt oder ungültig';
       exit;
     end;
  end;

  result:=0;
end;

{------------------------------------------}
Function TDecodeXMLREQ.GetCmdRequest:String;
{------------------------------------------}
Var
  st: string;
  P: integer;
  RequestDataStr: string;
  xmlst: string;
  BusAdr: string;
  DelAdr: string;
  sSystem: string;
  sData: string;
  s_timesync: string;
  s_transpcmd: string;
  s_signature: string;
  sPubKeyX: string;
  sPubKeyY: string;
  S: string;

begin
  Result:='';
  { Request: }
  P:=Pos('request="', REQ);
  if P > 0 then begin
    xmlst:=Copy (REQ, P + 9, length (REQ));
    Result:=DecodeXml (ExtractString (xmlst, NUL, '"', 0));  // XML-Ersatzzeichen decodieren; 03.06.2020, WW
  end else
    exit;

{$IFDEF GAS-X}
  sSystem:='GAS-X';
  sData:='hps_data';
{$ELSE}
  sSystem:='wieser';
  sData:='wieser_data';
{$ENDIF}
  P:=Pos('&lt;' + sSystem + '&gt;', REQ);
  if P > 0 then
    xmlst:=Copy (REQ, P, length (REQ))
  else
    exit;
  P:=Pos('&lt;' + sData + '&gt;', xmlst);
  if P > 0 then
    xmlst:=Copy (xmlst, P, length (xmlst))
  else
    exit;

  { optional: Requestdata }
  RequestDataStr:='';

  { Transparent-Befehl enthalten ? }
  P:=Pos('&lt;transparentcmd&gt;', xmlst);
  if P > 0 then begin
    s_transpcmd:=Copy (xmlst, P + 22, length (xmlst));
    P:=Pos('&lt;/transparentcmd&gt;', s_transpcmd);
    if P > 0 then
      RequestDataStr:=Copy (s_transpcmd, 1, P-1)
    else
      exit;
  end

  else begin
    { DSfG-Adressliste, DSfG-ZeitSync-Kommandodaten enthalten ? }
    P:=Pos('&lt;dsfg&gt;', xmlst);
    if P > 0 then
      xmlst:=Copy (xmlst, P + 12, length (xmlst))
    else
      exit;
    P:=Pos('&lt;/dsfg&gt;', xmlst);
    if P > 0 then
      xmlst:=Copy (xmlst, 1, P-1)
    else
      exit;

    { Requestdata: DSfG-ZeitSync-Kommandodaten }
    P:=Pos('&lt;timesync&gt;', xmlst);
    if P > 0 then
      s_timesync:=Copy (xmlst, P + 16, length(xmlst))
    else
      s_timesync:='';
    if length (s_timesync) > 0 then begin
      P:=Pos('&lt;/timesync&gt;', s_timesync);
      if P > 0 then
        s_timesync:=Copy (s_timesync, 1, P-1)
      else
        exit;

      { ZeitSync-Aktiv extrahieren: }
      P:=Pos('&lt;active&gt;', s_timesync);
      if P > 0 then
        st:=Copy (s_timesync, P + 14, length(s_timesync))
      else
        exit;
      P:=Pos('&lt;/active&gt;', st);
      if P > 0 then
        st:=Copy (st, 1, P-1)
      else
        exit;
      RequestDataStr:=RequestDataStr + st + C_CmdSeparator;

      { Zeitbasis extrahieren: }
      P:=Pos('&lt;timebase_instance&gt;', s_timesync);
      if P > 0 then
        st:=Copy (s_timesync, P + 25, length(s_timesync))
      else
        exit;
      P:=Pos('&lt;/timebase_instance&gt;', st);
      if P > 0 then
        st:=Copy (st, 1, P-1)
      else
        exit;
      RequestDataStr:=RequestDataStr + st + C_CmdSeparator;

      { min. Abweichung extrahieren: }
      P:=Pos('&lt;min_diff&gt;', s_timesync);
      if P > 0 then
        st:=Copy (s_timesync, P + 16, length(s_timesync))
      else
        exit;
      P:=Pos('&lt;/min_diff&gt;', st);
      if P > 0 then
        st:=Copy (st, 1, P-1)
      else
        exit;
      RequestDataStr:=RequestDataStr + st + C_CmdSeparator;

      { max. Abweichung extrahieren: }
      P:=Pos('&lt;max_diff&gt;', s_timesync);
      if P > 0 then
        st:=Copy (s_timesync, P + 16, length(s_timesync))
      else
        exit;
      P:=Pos('&lt;/max_diff&gt;', st);
      if P > 0 then
        st:=Copy (st, 1, P-1)
      else
        exit;
      RequestDataStr:=RequestDataStr + st + C_CmdSeparator;
    end  { if length (s_timesync) > 0 }

    else begin
      { Requestdata: DSfG-Adressliste }
      { Request-Type extrahieren: }
      P:=Pos('&lt;req_type&gt;', xmlst);
      if P > 0 then
        st:=Copy (xmlst, P + 16, length(xmlst))
      else
        exit;
      P:=Pos('&lt;/req_type&gt;', st);
      if P > 0 then
        st:=Copy (st, 1, P-1)
      else
        exit;
      RequestDataStr:=st;

      { Instanz-Adressdaten extrahieren: }
      P:=Pos('&lt;instance busaddr=&quot;', xmlst);
      while P > 0 do begin                  // Schleife über alle Busadressen
        st:=Copy (xmlst, P + 27, length(xmlst));

        P:=Pos('&lt;/instance&gt;', st);
        if P > 0 then begin
          xmlst:=Copy (st, P + 17, length (st));   // der Rest vom XML-String
          st:=Copy (st, 1, P-1);
        end else
          exit;
        BusAdr:=ExtractString (st, NUL, '&', 0);  // Busadresse
        RequestDataStr:=RequestDataStr + C_CmdSeparator + BusAdr;

        { Digitale Signatur: Öffentlicher Schlüssel X, Y; 15.03.2012, WW }
        sPubKeyX:=CPubKey_DoNotVerify;                                 
        sPubKeyY:=CPubKey_DoNotVerify;
        // -> Schalter: Wenn XML-Tag 'digitalsignature' fehlt, sollen Signaturen
        //    Kommando-gesteuert NICHT verifiziert werden (ab 07.04.2014 für
        //    Gas-X- und RMG-Version)
        s_signature:=st;
        P:=Pos('&lt;digitalsignature&gt;', s_signature);
        if P > 0 then begin
          sPubKeyX:='';  // Vorbelegung: Öffentlicher Schlüssel X in XML-Struktur nicht enthalten
          sPubKeyY:='';  // Vorbelegung: Öffentlicher Schlüssel Y in XML-Struktur nicht enthalten
          s_signature:=Copy (s_signature, P + 24, length(s_signature));
          P:=Pos('&lt;/digitalsignature&gt;', s_signature);
          if P > 0 then begin
            s_signature:=Copy (s_signature, 1, P-1);
            P:=Pos('&lt;publickeyX&gt;', s_signature);
            if P > 0 then begin
              S:=Copy (s_signature, P + 18, length(s_signature));
              P:=Pos('&lt;/publickeyX&gt;', S);
              if P > 0 then
                sPubkeyX:=Copy (S, 1, P-1);
            end;

            P:=Pos('&lt;publickeyY&gt;', s_signature);
            if P > 0 then begin
              S:=Copy (s_signature, P + 18, length(s_signature));
              P:=Pos('&lt;/publickeyY&gt;', S);
              if P > 0 then
                sPubkeyY:=Copy (S, 1, P-1);
            end;
          end;
        end;
        RequestDataStr:=RequestDataStr + C_CmdSeparatorAdr + sPubKeyX +
                        C_CmdSeparatorAdr + sPubKeyY;

        { Adressenliste }
        P:=Pos('&lt;addrlist&gt;', st);
        if P > 0 then begin
          st:=Copy (st, P + 16, length(st));
          P:=Pos('&lt;/addrlist&gt;', st);
          if P > 0 then begin
            st:=Copy (st, 1, P-1);
            P:=Pos('&lt;addr&gt;', st);
            if P > 0 then begin
              st:=Copy (st, P + 12, length(st));
              P:=Pos('&lt;/addr&gt;', st);
              if P > 0 then begin
                st:=Copy (st, 1, P-1);
                // Datenelement-Adressen:
                DELAdr:=StringReplace(st, C_CmdSeparator, C_CmdSeparatorAdr, [rfReplaceAll]);
                RequestDataStr:=RequestDataStr + C_CmdSeparatorAdr + DELAdr;
              end;
            end;
          end;
        end;
        P:=Pos('&lt;instance busaddr=&quot;', xmlst);
      end;  { while P > 0 }
    end;
  end;

  { RequestData in Separatoren für "dynamischen Kommandoteil packen und an Request anhängen: }
  if length (RequestDataStr) > 0 then
    RequestDataStr:=C_CmdSeparatorDyn + RequestDataStr + C_CmdSeparatorDyn;
  Result:=Result + RequestDataStr;
end;

{---------------------------------------------------------------------------------------}
Function TDecodeXMLREQ.CreateRequestData_MRG_StaKanalKonvDataList: TStaKanalKonvDataList;
{---------------------------------------------------------------------------------------}
{ optionalen Requestdata-Teil "Kanalliste", wenn vorhanden, decodieren und Kanalliste createn }
Var
  st:STRING ;
  P: integer;
  xmlst: string;
  sttag: string;
  KRec: TStaKanalKonvData;
  KObj: TStaKanalKonvDataObj;
  KList: TStaKanalKonvDataList;
  iBuf: integer;
  dBuf: double;
  code: integer;

begin
  Result:=nil;

  P:=Pos('&lt;wieser&gt;', REQ);
  if P > 0 then
    xmlst:=Copy (REQ, P, length (REQ))
  else
    exit;
  P:=Pos('&lt;wieser_data&gt;', xmlst);
  if P > 0 then
    xmlst:=Copy (xmlst, P, length (xmlst))
  else
    exit;
  P:=Pos('&lt;mrg&gt;', xmlst);
  if P > 0 then
    xmlst:=Copy (xmlst, P + 11, length (xmlst))
  else
    exit;
  P:=Pos('&lt;chlist&gt;', xmlst);
  if P > 0 then
    xmlst:=Copy (xmlst, P + 14, length (xmlst))
  else
    exit;
  P:=Pos('&lt;/chlist&gt;', xmlst);
  if P > 0 then
    xmlst:=Copy (xmlst, 1, P-1)
  else
    exit;

  KList:=nil;
  P:=Pos('&lt;ch ', xmlst);
  while P > 0 do begin                  // Schleife über alle Kanaleinträge
    st:=Copy (xmlst, P + 7, length(xmlst));

    P:=Pos('/ch&gt;', st);
    if P > 0 then begin
      xmlst:=Copy (st, P + 7, length (st));   // der Rest vom XML-String
      st:=Copy (st, 1, P-1);
    end else
      exit;

    { Kanalliste createn, wenn noch nicht erfolgt: }
    if not Assigned (KList) then
      KList:=TStaKanalKonvDataList.Create;
    { Vorbelegungen Kanallisten-Record: }
    with KRec do begin
      KanalNr:=0;
      OrgFaktor:=0;
      MessBereichMin:=0;
      MessBereichMax:=0;
    end;

    P:=Pos('cid=&quot;', st);         // Tag Kanalnummer: cid
    if P > 0 then begin
      sttag:=Copy (st, P + 10, length(st));
      P:=Pos('&quot;', sttag);
      if P > 0 then begin
        sttag:=Copy (sttag, 1, P-1);  //  Kanalnummer-Wert (Integer)
        Val (sttag, iBuf, Code);
        if Code = 0 then
          KRec.KanalNr:=iBuf
      end;
    end;

    P:=Pos('impfactor=&quot;', st);         // Tag Impulsfaktor: impfactor
    if P > 0 then begin
      sttag:=Copy (st, P + 16, length(st));
      P:=Pos('&quot;', sttag);
      if P > 0 then begin
        sttag:=Copy (sttag, 1, P-1);  //  Impulsfaktor-Wert (Real)
        Val (sttag, dBuf, Code);
        if Code = 0 then
          KRec.OrgFaktor:=dBuf
      end;
    end;

    P:=Pos('mr_low=&quot;', st);         // Tag untere Meßbereichsgrenze: mr_low
      if P > 0 then begin
      sttag:=Copy (st, P + 13, length(st));
      P:=Pos('&quot;', sttag);
      if P > 0 then begin
        sttag:=Copy (sttag, 1, P-1);  //   untere Meßbereichsgrenze-Wert (Real)
        Val (sttag, dBuf, Code);
        if Code = 0 then
          KRec.MessBereichMin:=dBuf
      end;
    end;

    P:=Pos('mr_high=&quot;', st);         // Tag obere Meßbereichsgrenze: mr_low
    if P > 0 then begin
      sttag:=Copy (st, P + 14, length(st));
      P:=Pos('&quot;', sttag);
      if P > 0 then begin
        sttag:=Copy (sttag, 1, P-1);  //  obere Meßbereichsgrenze-Wert (Real)
        Val (sttag, dBuf, Code);
        if Code = 0 then
          KRec.MessBereichMax:=dBuf
      end;
    end;

    { in Liste eintragen: }
    if KRec.KanalNr > 0 then begin
      KObj:=TStaKanalKonvDataObj.Create (KRec);
      KList.Add (KObj);
    end;

    P:=Pos('&lt;ch ', xmlst);
  end;  { while P > 0 }

  Result:=KList;
end;

{---------------------------------------}
Procedure TDecodeXMLREQ.Write_XMLRequest;
{---------------------------------------}
Var
  FName: string;
  DTStr:string;
  pch:Integer;
  st:string;
  FS: TFileStream;
  l: integer;

begin
  try
    DateTimeToString (DTStr, 'yyyymmdd_hhmmsszzz', Now);
    FName:=debXML_Dir + DTStr + ext_Req_Log;
    FS:=TFileStream.Create (FName, fmCreate);
    try
      l:=length (REQ);
      if l > 0 then
        FS.Write (REQ [1], l);
    finally
      FS.Free;
    end;

    pch:=Pos('<?xml',REQ);
    st:=copy(REQ,pch,length(REQ));
    FName:=debXML_Dir + DTStr + ext_Req_Xml;
    FS:=TFileStream.Create (FName, fmCreate);
    try
      l:=length (st);
      if l > 0 then
        FS.Write (st [1], l);
    finally
      FS.Free;
    end;
  except
  end;
end;

end.


