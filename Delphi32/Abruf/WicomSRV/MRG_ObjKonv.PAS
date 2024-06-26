{******************************************************************************}
{* Unit: Objekte zur MRG-Datenkonvertierung von Messwerten/Tagess�tzen und    *}
{*       Pr�fungss�tzen                                                       *}
{* 07.01.2002 WW  Neu                                                         *}
{* 06.08.2021 WW  Ressourcedaten aus Liste statt Datei lesen                  *}
{******************************************************************************}
unit MRG_ObjKonv;

INTERFACE

Uses
  Classes, SysUtils, MFileNam, MObjList, MLGZKonv, WSysCon, MResMrg, LGZType,
  MObjPara, MLGZKonvList, MResUtil;

type

  { Objekt zur Konvertierung der Daten eines MRG }

  TMrgKonvert = class (TObject)
  private
    MrgTyp: integer;
    RohdatenLoeschen: boolean;
    MessRohFileNameListe: TTextListe; { je Abruf k�nnen mehrere Messwert-Rohdateien entstanden sein }
    TagRohFileNameListe: TTextListe;  { je Abruf k�nnen mehrere Tagessatz-Rohdateien entstanden sein }
    PruefRohFileName: TFileName;
  public
    constructor Create (AMrgTyp: integer; ARohdatenLoeschen: boolean);
    destructor Destroy; override;
    procedure AddFileName (FileName: TFileName; Datenart: integer);
    function MessTagKonvert(AMrgDefData: TMrgDefData;
                            AMrgKonvData: TMrgKonvData;
                            AMBAbrufData: TMBAbrufData;
                            MrgKanalBitKonfigList: TMrgKanalBitKonfigList;
                            WZ_SZKonfigList: TWZ_SZKonfigList;
                            ATagesende: integer;
                            AKanalAktivMaske: string;
                            AStaKanalKonvDataList: TStaKanalKonvDataList;
                            AParameterListe: TParameterListe;
                            AAnalogOrig_normiert: boolean;
                            var KonvDateiname_MessList: TStringList;
                            var KonvDateiname_TagList: TStringList;
                            var iMaxMessKanal: integer;
                            var iMinMessKanalExt: integer;
                            var iMaxMessKanalExt: integer): boolean;
    procedure PruefKonvert(var KonvDateiname_Pruef: string);
  end;

IMPLEMENTATION

{ TMrgKonvert }

{----------------------------------------------------------------------------}
constructor TMrgKonvert.Create (AMrgTyp: integer; ARohdatenLoeschen: boolean);
{----------------------------------------------------------------------------}
begin
  inherited Create;
  MrgTyp:=AMrgTyp;
  RohdatenLoeschen:=ARohdatenLoeschen;

  MessRohFileNameListe:=TTextListe.Create;
  TagRohFileNameListe:=TTextListe.Create;
  PruefRohFileName:='';
end;

{-----------------------------}
destructor TMrgKonvert.Destroy;
{-----------------------------}
begin
  TagRohFileNameListe.Free;
  MessRohFileNameListe.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------}
procedure TMrgKonvert.AddFileName (FileName: TFileName; Datenart: integer);
{-------------------------------------------------------------------------}
begin
  if FileName <> '' then
    case Datenart of
      dt_Messwerte:   MessRohFileNameListe.Add (FileName);
      dt_Tagessaetze: TagRohFileNameListe.Add (FileName);
      dt_Pruefsaetze: PruefRohFileName := FileName;
    end; { case }
end;

{--------------------------------------------------------------------------------}
function TMrgKonvert.MessTagKonvert(AMrgDefData: TMrgDefData;
                                    AMrgKonvData: TMrgKonvData;
                                    AMBAbrufData: TMBAbrufData;
                                    MrgKanalBitKonfigList: TMrgKanalBitKonfigList;
                                    WZ_SZKonfigList: TWZ_SZKonfigList;
                                    ATagesende: integer;
                                    AKanalAktivMaske: string;
                                    AStaKanalKonvDataList: TStaKanalKonvDataList;
                                    AParameterListe: TParameterListe;
                                    AAnalogOrig_normiert: boolean;
                                    var KonvDateiname_MessList: TStringList;
                                    var KonvDateiname_TagList: TStringList;
                                    var iMaxMessKanal: integer;
                                    var iMinMessKanalExt: integer;
                                    var iMaxMessKanalExt: integer): boolean;
{--------------------------------------------------------------------------------}
{ Aufruf der Haupt-Routine zur Me�wert/Tagessatz-Konvertierung;
  �bergabe: MrgDefData-Record
            MrgKonvData-Record
            MBAbrufData-Record
            MrgKanalBit-Ressourcedatenliste
            WZ_SZ-Ressourcedatenliste
            Tagesende im Ger�t
            Maske aktiver Kan�le
            Schalter f�r R�ckgabe von normierten Analog-Messwerten
  R�ckgaben: Listen mit Namen der von der Konvertierung erzeugten Dateien mit
               Messwerten (RohSRec-Struktur) und Tagess�tzen (RohTRec-Struktur)
             Anzahl der aus den Rohdaten konvertierten Messwert-Kan�le (-1, wenn
               unbekannt)
             Niedrigste und h�chste Kanalnummer des konvertierten, erweiterten
               Messwert-Archivs (-1, wenn unbekannt) }
var
  MessTagKonv: TMessTagKonv;
  i: integer;
  fertig: boolean;
  k: integer;
  MrgKanalBitMaskList: TMrgKanalBitMaskList;

begin
  Result:=false;
  // Vorbelegungen: Kanalzahlen aus Konvertierung unbekannt
  iMaxMessKanal:=-1;
  iMinMessKanalExt:=-1;
  iMaxMessKanalExt:=-1;

  MrgKanalBitMaskList:=TMrgKanalBitMaskList.Create;
  try
    if Assigned (WZ_SZKonfigList) then begin  // 06.08.2021, WW
      // Kanalbitmaskenliste aus Ressourcedatenliste laden:
      MrgKanalBitMaskList.LoadFromKonfigList_ByMrgKommandoTyp (MrgTyp, 'E',
        MrgKanalBitKonfigList);  // 06.08.2021, WW

      { Alle Me�wert-Rohdateien konvertieren:
          Wenn mehrere Me�wert-Rohdateien existieren gibt es zwei Varianten:
          1. Rohdateien aus Abrufen per DSfG-Umleitung werden nacheinander einzeln konvertiert, da
             sie nur die Daten f�r einen begrenzten Zeitbereich (1 Tag) enthalten  (for i:=0 to...)
          2. Rohdateien aus Abrufen, welche je Kanal eine eigene Rohdatei erzeugt haben, werden
             alle mit einem einzigen Aufruf von 'MessTag_Konv' konvertiert. 'MessTag_Konv' mu� in
             diesem Fall das gesetzte Schleifenabbruch-Flag 'fertig' zur�ckgeben.
        Alle Tagessatz-Rohdateien konvertieren:
          Rohdateien aus Abrufen, welche je Kanal eine eigene Rohdatei erzeugt haben, werden
          alle mit einem einzigen Aufruf von 'MessTag_Konv' konvertiert. 'MessTag_Konv' mu� in
          diesem Fall das gesetzte Schleifenabbruch-Flag 'fertig' zur�ckgeben. }
      for i:=0 to MessRohFileNameListe.Count - 1 do begin
        with MessTagKonv do begin
          KonvGruppe:=AMrgKonvData.KonvGruppe;
          Tagesende:=ATagesende;
          KanalAktivMaske:=AKanalAktivMaske;
          if length (KanalAktivMaske) = 0 then  { falls keine Kanalaktiv-Maske vom Client mitgegeben wurde }
            for k:=1 to C_MaxKanalZahl do
              KanalAktivMaske:=KanalAktivMaske + '1';  { Ersatzmaske: alle Kan�le aktiv }

          if AMrgDefData.Eingangszaehler then
            AnzEingZaehler:=AMrgDefData.AnzahlZaehlerKanaele
          else
            AnzEingZaehler:=0;
          if AMrgDefData.Kontrollzaehler then
            AnzKontrZaehler:=AMrgDefData.AnzahlZaehlerKanaele
          else
            AnzKontrZaehler:=0;
          AnalogKanal_von:=AMrgDefData.AnalogKanal_von;
          AnalogKanal_bis:=AMrgDefData.AnalogKanal_bis;

          StdQuellDateiName := MessRohFileNameListe [i];
          StdQuellDateiNameListe := MessRohFileNameListe;
          StdZielDateiName:=CreateZwischenDateiNameTemp (dd_Mess);  { Messwert-Zielfile mit eindeutigem Namen wird hier bereits erzeugt }
          if TagRohFileNameListe.Count > 0 then
            TagQuellDateiName := TagRohFileNameListe [0]
          else
            TagQuellDateiName := '';
          TagQuellDateiNameListe := TagRohFileNameListe;
          TagZielDateiName:=CreateZwischenDateiNameTemp (dd_Tags);  { Tagessatz-Zielfile mit eindeutigem Namen wird hier bereits erzeugt }
          Loeschen := RohdatenLoeschen;

          ParameterListe:=AParameterListe;
          AufzMax_Analog:=AMrgDefData.AnalogKanal_AufzMax;
          AnalogOrig_normiert:=AAnalogOrig_normiert;
          KanalList:=AStaKanalKonvDataList;
          { Anmerkung: KanalList darf ohne Fehler NIL enthalten, solange keine
            Messwerte in LGZ-Daten konvertiert werden m�ssen von Ger�ten, welche
            pyhsikalische Rohwerte liefern und zu denen keine Umrechnungsfaktoren/
            Messbereichsgrenzen �ber die Parameterliste zur Verf�gung stehen
            (z.B. f�r Gas-X-Version des Abrufdienstes).
            -> betrifft MRG-Typen: EC694 (Imp, Ana), VC2 (Imp, Ana), EK260 (nur Imp),
                        TTG (IEC) }

          { Dummy-Belegung ausreichend solange keine natGas-ASCII-Daten erzeugt werden m�ssen: }
          Select:=0;
          Sta_Stationsname:='';

{$IFDEF GAS-X}
          bAktZaehler:=true;  // aktuelle Z�hlerst�nde zum Auslesezeitpunkt
                              // mitkonvertieren (VC2, Corus)
{$ELSE}
          bAktZaehler:=false;
{$ENDIF}
        end; { with MessTagKonv }
        MessTagKonv.MrgKanalBitMaskList:=MrgKanalBitMaskList;
        MessTagKonv.WZ_SZKonfigList:=WZ_SZKonfigList;
        MessTagKonv.MrgTyp:=MrgTyp;  // f�r Modbus-Daten (Primus/Prilog, TME400, FLOWSIC500, RSM200); 08.03.2019, WW
        MessTagKonv.MBAbrufData:=AMBAbrufData;  // f�r Modbus-Daten; 11.02.2022, WW

        { R�ckgaben: Listen mit Namen der Zieldateien }
        if KonvDateiname_MessList <> nil then
          KonvDateiname_MessList.Add (MessTagKonv.StdZielDateiName);
        if KonvDateiname_TagList <> nil then
          KonvDateiname_TagList.Add (MessTagKonv.TagZielDateiName);

        MessTag_Konv(MessTagKonv, fertig,
          iMaxMessKanal, iMinMessKanalExt, iMaxMessKanalExt);  { Konvertierung }
        if fertig then Break;
      end;  { for }
      Result:=true;
    end;  { if Assigned (WZ_SZKonfigList) }
  finally
    MrgKanalBitMaskList.Free;
  end;
end;

{------------------------------------------------------------------}
procedure TMrgKonvert.PruefKonvert(var KonvDateiname_Pruef: string);
{------------------------------------------------------------------}
{ Aufruf der Routine zur Pr�fsatz-Konvertierung;
  R�ckgabe: Name der von der Konvertierung erzeugten Datei mit Pr�fungss�tzen (PruefRec-Struktur) }
var
  PruefKonv: TPruefKonv;

begin
  { Vorbelegung der R�ckgabe: keine konvertierten Daten vorhanden }
  KonvDateiname_Pruef:='';

  if length (PruefRohFileName) > 0 then begin
    with PruefKonv do begin
      QuellDateiName := PruefRohFileName;
      ZielDateiName:=CreateZwischenDateiNameTemp (dd_Pruef);  { Pr�fsatz-Zielfile mit eindeutigem Namen wird hier bereits erzeugt }
      loeschen := RohdatenLoeschen;
    end;
    { R�ckgabe: Name der Zieldatei }
    KonvDateiname_Pruef:=PruefKonv.ZielDateiName;

    Pruef_konv (PruefKonv);                                   { Konvertierung }
  end;
end;

end.
