{------------------------------------------------------------------------------}
{ DSfG-Datenelemente                                                           }
{                                                                              }
{ 10.11.2000  AUTO    Neu                                                      }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000                                          }
{------------------------------------------------------------------------------}
unit DsgDel;

interface

function GetDsfgDeaBezeichnung(sValue: string): string;

implementation

{ Gibt Datenelementbezeichnung für   }
{ Datenelemente mit Anfagsbuchstaben }
{ 'a' zurück                         }
{ Parameter: Datenelement-Adresse    }
{ Rückgabe: Datenelementbezeichnung  }
{------------------------------------}
function GetDsfgDeaBezeichnungA(sValue: string): string;
{------------------------------------}
begin
  if (sValue = 'a') then Result := 'allgemeine Beschreibung'
  else if (sValue = 'aa') then Result := 'DSfG'
  else if (sValue = 'aaa') then Result := 'eigener Instanzentyp'
  else if (sValue = 'aab') then Result := 'ID-Schnittstellenkarte'
  else if (sValue = 'aac') then Result := 'Software-Version'
  else if (sValue = 'aad') then Result := 'CRC12-Startwert'
  else if (sValue = 'ab') then Result := 'Typenschild'
  else if (sValue = 'aba') then Result := 'Hersteller'
  else if (sValue = 'abb') then Result := 'Gerätetyp'
  else if (sValue = 'abc') then Result := 'Fabrik-Nr.'
  else if (sValue = 'abd') then Result := 'Baujahr'
  else if (sValue = 'abe') then Result := 'Software-Version'
  else if (sValue = 'abf') then Result := 'Inbetriebnahme'
  else if (sValue = 'ac') then Result := 'Zeitangaben'
  else if (sValue = 'aca') then Result := 'Datum, Uhrzeit'
  else if (sValue = 'acb') then Result := 'Zeitzone'
  else if (sValue = 'acc') then Result := 'letzte Verstellung der Zeitzone'
  else if (sValue = 'acd') then Result := 'Betriebsstunden'
  else if (sValue = 'ace') then Result := 'letzte Eichung'
  else if (sValue = 'acf') then Result := 'letzter Batteriewechsel'
  else if (sValue = 'ad') then Result := 'Benutzerdaten'
  else if (sValue = 'ada') then Result := 'Meßort'
  else if (sValue = 'adb') then Result := 'Urbeleg-Drucker'
  else if (sValue = 'adc') then Result := 'Größe Datenspeicher'
  else if (sValue = 'add') then Result := 'Zugangscode 1'
  else if (sValue = 'ade') then Result := 'Zugangscode 2'
  else if (sValue = 'adf') then Result := 'Eichschalter'
  else if (sValue = 'adg') then Result := 'Benutzerschalter'
  else if (sValue = 'ae') then Result := 'Ereignismeldung'
  else if (sValue = 'aea') then Result := 'letztes Ereignis'
  else if (sValue = 'aeb') then Result := 'Datum des letzten Ereignisses'
  else if (sValue = 'af') then Result := 'physik. Maßeinheiten'
  else if (sValue = 'afa') then Result := 'Einh. Drücke'
  else if (sValue = 'afb') then Result := 'Einh. Temperatur'
  else if (sValue = 'afc') then Result := 'Einh. Durchfluß'
  else if (sValue = 'afd') then Result := 'Einh. Betriebsdichte'
  else if (sValue = 'afe') then Result := 'Einh. Normdichte'
  else if (sValue = 'aff') then Result := 'Einh. Brennwert'
  else if (sValue = 'afg') then Result := 'Einh. Wärmemenge'
  else if (sValue = 'afh') then Result := 'Einh. Stoffmenge'
  else if (sValue = 'afi') then Result := 'Einh. Volumen'
  else if (sValue = 'afj') then Result := 'Einh. thermischer Mengendurchfluß'
  else Result := 'Datenelementadresse nicht gespeichert';
end;

{ Gibt Datenelementbezeichnung für   }
{ Datenelemente mit Anfagsbuchstaben }
{ 'b' zurück                         }
{ Parameter: Datenelement-Adresse    }
{ Rückgabe: Datenelementbezeichnung  }
{------------------------------------}
function GetDsfgDeaBezeichnungB(sValue: string): string;
{------------------------------------}
begin
  if (sValue = 'b') then Result := 'Umwerter Instanz'
  else begin
    if (Length(sValue) >= 2) and (sValue[2] = 'a') then begin
      if (sValue = 'ba') then Result := 'Zählwerte'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'baa') then Result := 'Hauptzählwerke Fahrrichtung 1 der Meßanlage'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'baaa') then Result := 'Hauptzählwerk Normvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'baab') then Result := 'Restmengenzählwerk Normvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'baac') then Result := 'Hauptzählwerk thermische Energie'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'baad') then Result := 'Restmengenzählwerk thermische Energie'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'baae') then Result := 'Hauptzählwerk Betriebsvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'baaf') then Result := 'Restmengenzählwerk Betriebsvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'baag') then Result := 'Hauptzählwerk Betriebsvolumen vor Kennlinienkorrektur'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'baah') then Result := 'Restmengenzählwerk Betriebsvolumen vor Kennlinienkorrektur'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'bab') then Result := 'Hauptzählwerke Fahrrichtung 2 der Meßanlage'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'baba') then Result := 'Hauptzählwerk Normvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'babb') then Result := 'Restmengenzählwerk Normvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'babc') then Result := 'Hauptzählwerk thermische Energie'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'babd') then Result := 'Restmengenzählwerk thermische Energie'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'babe') then Result := 'Hauptzählwerk Betriebsvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'babf') then Result := 'Restmengenzählwerk Betriebsvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'babg') then Result := 'Hauptzählwerk Betriebsvolumen vor Kennlinienkorrektur'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'babh') then Result := 'Restmengenzählwerk Betriebsvolumen vor Kennlinienkorrektur'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'bac') then Result := 'Störzählwerke Fahrrichtung 1 der Meßanlage'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'baca') then Result := 'Störzählwerk Normvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bacb') then Result := 'Restmengenzöhlwerk Normvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bacc') then Result := 'Störzählwerk thermische Energie'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bacd') then Result := 'Restmengenzählwerk thermische Energie'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bace') then Result := 'Störzählwerk Betriebsvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'bacf') then Result := 'Restmengenzählwerk Betriebsvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'bacg') then Result := 'Störzählwerk Betriebsvolumen vor Kennlinienkorrektur'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'bach') then Result := 'Restmengenzählwerk Betriebsvolumen vor Kennlinienkorrektur'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'bad') then Result := 'Störzählwerke Fahrrichtung 2 der Meßanlage'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bada') then Result := 'Störzählwerk Normvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'badb') then Result := 'Restmengenzählwerk Normvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'badc') then Result := 'Störzählwerk thermische Energie'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'badd') then Result := 'Restmengenzählwerk thermische Energie'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bade') then Result := 'Störzählwerk Betriebsvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'badf') then Result := 'Restmengenzählwerk Betriebsvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'badg') then Result := 'Störzählwerk Betriebsvolumen vor Kennlinienkorrektur'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'badh') then Result := 'Restmengenzählwerk Betriebsvolumen vor Kennlinienkorrektur'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'bae') then Result := 'Faktoren für die Zählwerke'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'baea') then Result := 'Zählwerksfaktor Normvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'baeb') then Result := 'Zählwerksfaktor thermische Energie'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'baec') then Result := 'Zählwerksfaktor Betriebsvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'baed') then Result := 'Zählwerksfaktor Betriebsvolumen vor Kennlinienkorrektur'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'baf') then Result := 'Faktoren fürs Dispatching'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bafa') then Result := 'Dispatchingfaktor Normvolumen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bafd') then Result := 'Dispatchingfaktor Betriebsvolumen vor Kennlinienkorrektur'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'bag') then Result := 'Zählwerke für fliegende Eichung'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'baga') then Result := 'Zählerstand Normvolumen ( Haupt )'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bagb') then Result := 'Zählerstand Normvolumen ( Rest )'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bagc') then Result := 'Zählerstand thermische Energie ( Haupt )'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bagd') then Result := 'Zählerstand thermische Energie ( Rest )'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bage') then Result := 'Zählerstand Betriebsvolumen ( Haupt, korrigiert )'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'bagf') then Result := 'Zählerstand Betriebsvolumen ( Rest, korrigiert )'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'bagh') then Result := 'Zählerstand Betriebsvolumen ( Haupt, vor Kennlinienkorrektur )'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'bagi') then Result := 'Zählerstand Betriebsvolumen ( Rest, vor Kennlinienkorrektur )'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'b') then begin
      if (sValue = 'bb') then Result := 'Durchflüsse'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'bba') then Result := 'Normvolumendurchfluß'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'bbb') then Result := 'thermische Mengendurchfluß'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'bbc') then Result := 'Betriebsvolumendurchfluß'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'bbd') then Result := 'Betriebsvolumendurchfluß vor Kennlinienkorrektur'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'bbe') then Result := 'aktuell zur Umwertung verwendeter Wert'
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'c') then begin
      if (sValue = 'bc') then Result := 'Einstellung der Meßwerteingänge'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'bca') then Result := 'Betriebsvolumendurchfluß'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bcaa') then Result := 'Kennwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Wertigkeit HF Impulse 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Wertigkeit HF Impulse 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Wertigkeit NF Impulse'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bcab') then Result := 'Grenzwerte Überwachung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'untere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'maximal erlaubter Gradient'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Vergleichsgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Fehlimpulse Schaufelüberwachung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'maximale Zeit Zähleranlauf'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'maximale Zeit Zählerauslauf'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'timeout in Sek. bei Erfassung über DSfG'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'bcb') then Result := 'Betriebsdichte'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bcba') then Result := 'Kennwerte Konfigurierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Koeffizient ko'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Koeffizient k1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Koeffizient k2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Koeffizient k T0'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Koeffizient k T1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bcbb') then Result := 'Grenzwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Ersatzwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'untere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'maximal erlaubter Gradient'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'timeout in Sek. bei Erfassung über DSfG'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'bcc') then Result := 'Normdichte'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bcca') then Result := 'Korrekturwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Koeffizient ko'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Koeffizient k1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Koeffizient k2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Koeffizient k3'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bccb') then Result := 'Grenzwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Ersatzwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'untere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'maximal erlaubter Gradient'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'timeout in Sek. bei Erfassung über DSfG'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'bcd') then Result := 'Absolutdruck'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bcda') then Result := 'Kennwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Koeffizient ko'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Koeffizient k2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Koeffizient k3'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bcdb') then Result := 'Grenzwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Ersatzwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'untere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'maximal erlaubter Gradient'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'timeout in Sek. bei Erfassung über DSfG'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'bce') then Result := 'Überdruck'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bcea') then Result := 'Kennwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Koeffizient ko'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Koeffizient k1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Koeffizient k2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Koeffizient k3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Korrekturwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bceb') then Result := 'Grenzwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Ersatzwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'untere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'maximal erlaubter Gradient'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'bceg') then Result := 'timeout in Sek. bei Erfassung über DSfG'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'bcf') then Result := 'Gastemperatur'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bcfa') then Result := 'Kennwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Koeffizient ko'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Koeffizient k1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Koeffizient k2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Koeffizient k3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Korrekturwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bcfb') then Result := 'Grenzwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Ersatzwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'untere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'maximal erlaubter Gradient'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'timeout in Sek. bei Erfassung über DSfG'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'bcg') then Result := 'Temperatur am Betriebsdichtegeber'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bcga') then Result := 'Kennwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Koeffizient ko'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Koeffizient k1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Koeffizient k2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Koeffizient k3'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bcgb') then Result := 'Grenzwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Ersatzwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'untere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'maximal erlaubter Gradient'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'timeout in Sek. bei Erfassung über DSfG'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'h') then begin
        if (sValue = 'bch') then Result := 'Temperatur für Schallgeschwindigkeit'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bcha') then Result := 'Kennwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Koeffizient ko'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Koeffizient k1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Koeffizient k2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Koeffizient k3'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bchb') then Result := 'Grenzwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Ersatzwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'maximal erlaubter Gradient'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'timeout in Sek. bei Erfassung über DSfG'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'j') then begin
        if (sValue = 'bcj') then Result := 'Brennwert'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bcja') then Result := 'Kennwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Koeffizient ko'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Koeffizient k1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Koeffizient k2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Koeffizient k3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Korrekturwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bcjb') then Result := 'Grenzwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Ersatzwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'untere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'maximal erlaubter Gradient'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'timeout in Sek. bei Erfassung über DSfG'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'k') then begin
        if (sValue = 'bck') then Result := 'Kohlenstoffdioxidgehalt'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bcka') then Result := 'Kennwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Koeffizient ko'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Koeffizient k1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Koeffizient k2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Koeffizient k3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Korrekturwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bckb') then Result := 'Grenzwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Ersatzwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'untere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'maximal erlaubter Gradient'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'timeout in Sek. bei Erfassung über DSfG'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'l') then begin
        if (sValue = 'bcl') then Result := 'Wasserstoffgehalt'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bcla') then Result := 'Kennwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Koeffizient ko'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Koeffizient k1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Koeffizient k2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Koeffizient k3'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bclb') then Result := 'Grenzwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Ersatzwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'untere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'maximal erlaubter Gradient'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'timeout in Sek. bei Erfassung über DSfG'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'd') then begin
      if (sValue = 'bd') then Result := 'erfaßte Meßwerte'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'bda') then Result := 'Betriebsvolumendurchfluß'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bdaa') then Result := 'Frequenz 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bdab') then Result := 'Frequenz 2'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'bdb') then Result := 'Betriebsdichte'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bdba') then Result := 'Meßgröße'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bdbb') then Result := 'errechneter Meßwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bdbc') then Result := 'Mittelwert der Betriebsdichte seit dem letzten Ereignis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bdbd') then Result := 'Korrekturwert'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'bdc') then Result := 'Normdichte'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bdcb') then Result := 'Meßgröße 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bdcc') then Result := 'errechneter Meßwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bdcd') then Result := 'Mittelwert des zur Umwertung verwendeten Meßwertes seit dem letzten'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bdce') then Result := 'aktuell zur Umwertung verwendeter Wert'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'bdd') then Result := 'Absolutdruck'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bdda') then Result := 'Meßgröße 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bddb') then Result := 'Meßgröße 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bddc') then Result := 'errechneter Meßwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bddd') then Result := 'Mittelwert verwendeten Meßwertes seit dem letzten Eeignis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bdde') then Result := 'aktuell zur Umwertung verwendeter Wert'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'bde') then Result := 'Überdruck'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bdea') then Result := 'Meßgröße 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bdeb') then Result := 'Meßgröße 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bdec') then Result := 'errechneter Meßwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bded') then Result := 'Mittelwert des verwendeten Meßwertes seit dem letzten Eeignis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'bdf') then Result := 'Gastemperatur'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bdfa') then Result := 'Meßgröße 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bdfb') then Result := 'Meßgröße 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bdfc') then Result := 'errechneter Meßwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bdfd') then Result := 'Mittelwert des verwendeten Meßwertes seit dem letzten Eeignis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bdfe') then Result := 'aktuell zur Umwertung verwendeter Wert'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'bdg') then Result := 'Temperatur am Betriebsdichtegeber'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bdga') then Result := 'Meßgröße 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bdgb') then Result := 'Meßgröße 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bdgc') then Result := 'errechneter Meßwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bdgd') then Result := 'Mittelwert des verwendeten Meßwertes seit dem letzten Eeignis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bdge') then Result := 'aktuell zur Umwertung verwendeter Wert'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'h') then begin
        if (sValue = 'bdh') then Result := 'Temperatur für Schallgeschwindigkeitskorrektur'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bdha') then Result := 'Meßgröße 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bdhb') then Result := 'Meßgröße 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bdhc') then Result := 'errechneter Meßwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bdhd') then Result := 'Mittelwert des verwendeten Meßwertes seit dem letzten Eeignis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bdhe') then Result := 'aktuell zur Umwertung verwendeter Wert'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'i') then begin
        if (sValue = 'bdi') then Result := 'Schallgeschwindigkeit'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bdia') then Result := 'Meßgröße 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bdib') then Result := 'Meßgröße 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bdic') then Result := 'errechneter Meßwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bdid') then Result := 'Mittelwert des Meßwertes seit dem letzten Eeignis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bdie') then Result := 'aktuell zur Umwertung verwendeter Wert'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'j') then begin
        if (sValue = 'bdj') then Result := 'Brennwert'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bdja') then Result := 'Meßgröße 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bdjb') then Result := 'Meßgröße 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bdjc') then Result := 'errechneter Meßwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bdjd') then Result := 'Mittelwert des verwendeten Meßwertes seit dem letzten Eeignis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bdje') then Result := 'aktuell zur Umwertung verwendeter Wert'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'k') then begin
        if (sValue = 'bdk') then Result := 'Kohlenstoffdioxidgehalt'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bdka') then Result := 'Meßgröße 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bdkb') then Result := 'Meßgröße 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bdkc') then Result := 'errechneter Meßwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bdkd') then Result := 'Mittelwert des verwendeten Meßwertes seit dem letzten Eeignis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bdke') then Result := 'aktuell zur Umwertung verwendeter Wert'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'l') then begin
        if (sValue = 'bdl') then Result := 'Wasserstoffgehalt'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bdla') then Result := 'Meßgröße 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bdlb') then Result := 'Meßgröße 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bdlc') then Result := 'errechneter Meßwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bdld') then Result := 'Mittelwert des verwendeten Meßwertes seit dem letzten Eeignis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bdle') then Result := 'aktuell zur Umwertung verwendeter Wert'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'e') then begin
      if (sValue = 'be') then Result := 'Korrekturverfahren'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'bea') then Result := 'K - Zahl Berechnung'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'beaa') then Result := 'Grundeinstellung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Normdruck'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Normtemperatur'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Festwert'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'beb') then Result := 'Tabelle 1 für Gasbeschaffenheit Analyse 1'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'beba') then Result := 'Brennwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bebb') then Result := 'Normdichte'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bebc') then Result := 'Dichteverhältnis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bebd') then Result := 'Kohlenstoffdioxidgehalt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bebe') then Result := 'Stickstoffgehalt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'bebf') then Result := 'Wasserstoffgehalt'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'bec') then Result := 'Tabelle 2 für Gasbeschaffenheit'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'beca') then Result := 'Brennwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'becb') then Result := 'Normdichte'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'becc') then Result := 'Dichteverhältnis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'becd') then Result := 'Kohlenstoffdioxidgehalt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bece') then Result := 'Stickstoffgehalt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'becf') then Result := 'Wasserstoffgehalt'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'bed') then Result := 'Tabelle 3 für Gasbeschaffenheit'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'beda') then Result := 'Brennwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bedb') then Result := 'Normdichte'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bedc') then Result := 'Kohlenstoffdioxidgehalt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bedd') then Result := 'Dichteverhältnis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bede') then Result := 'Stickstoffgehalt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'bedf') then Result := 'Wasserstoffgehalt'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'bee') then Result := 'Tabelle 4 für Gasbeschaffenheit'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'beea') then Result := 'Brennwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'beeb') then Result := 'Normdichte'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'beec') then Result := 'Dichteverhältnis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'beed') then Result := 'Kohlenstoffdioxidgehalt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'beee') then Result := 'Stickstoffgehalt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'beef') then Result := 'Wasserstoffgehalt'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'bef') then Result := 'Betriebsdichte'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'befa') then Result := 'Modus'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'befb') then Result := 'Faktor 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'befc') then Result := 'Faktor 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'befd') then Result := 'Faktor 3'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'befe') then Result := 'Faktor 4'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'beg') then Result := 'Schallgeschwindigkeit'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bega') then Result := 'Modus'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'begb') then Result := 'Faktor 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'begc') then Result := 'Faktor 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'begd') then Result := 'Faktor 3'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bege') then Result := 'Faktor 4'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'h') then begin
        if (sValue = 'beh') then Result := 'Durchflußzähler'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'beha') then Result := 'Stützpunkte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stützpunkt 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Stützpunkt 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Stützpunkt 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Stützpunkt 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Stützpunkt 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Stützpunkt 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Stützpunkt 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Stützpunkt 8'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'behb') then Result := 'Korrekturpunkte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Korrektur1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Korrektur 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Korrektur 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Korrektur 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Korrektur 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Korrektur 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Korrektur 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Korrektur 8'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'f') then begin
      if (sValue = 'bf') then Result := 'errechnete Kennwerte'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'bfa') then Result := 'Zustandszahl'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'bfb') then Result := 'Z unter Betriebsbedingungen'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'bfc') then Result := 'Z unter Normbedingungen'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'bfd') then Result := 'Kompressibilitätszahl'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'bfe') then Result := 'Wobbeindex'
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'g') then begin
      if (sValue = 'bg') then Result := 'Schnittstellensignale zur Steuerung'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'bga') then Result := 'Ausgangsimpulse'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bgaa') then Result := 'Impulskanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'maximale Frequenz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Pulsbreite'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Pulswertigkeit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsspeicher'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bgab') then Result := 'Impulskanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'maximale Frequenz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Pulsbreite'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Pulswertigkeit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsspeicher'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bgac') then Result := 'Impulskanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'maximale Frequenz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Pulsbreite'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Pulswertigkeit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsspeicher'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bgad') then Result := 'Impulskanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'maximale Frequenz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Pulsbreite'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Pulswertigkeit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsspeicher'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'bgb') then Result := 'Ausgangsströme Stromausgänge für die Regelung'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bgba') then Result := 'Ausgangsstrom 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'untere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Gradient'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Strom 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bgbb') then Result := 'Ausgangsstrom 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'untere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Gradient'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Strom 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bgbc') then Result := 'Ausgangsstrom 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'untere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Gradient'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Strom 3'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bgbd') then Result := 'Ausgangsstrom 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Parametrierung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'untere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'obere Warngrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'untere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'obere Alarmgrenze'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Gradient'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Strom 4'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'bgc') then Result := 'Rechnerschnittstelle'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bgca') then Result := 'Parameter 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bgcb') then Result := 'Parameter 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bgcc') then Result := 'Parameter 3'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bgcd') then Result := 'Parameter 4'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bgce') then Result := 'Parameter 5'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'bgd') then Result := 'Grenzkontakte'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bgda') then Result := 'Grenzkontakt 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ansprechschwelle min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Ansprechschwelle max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Leitungsdruck'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Gastemperatur'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bgdb') then Result := 'Grenzkontakt 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ansprechschwelle min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Ansprechschwelle max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Leitungsdruck'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Gastemperatur'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'h') then begin
      if (sValue = 'bh') then Result := 'Umwerterinterne Werte'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'bha') then Result := 'Kalender'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bhaa') then Result := 'Meßperiodenlänge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bhab') then Result := 'Startdatum der Meßperiode'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bhac') then Result := 'Betriebszeit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'bhad') then Result := 'Batteriebelastungszeit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'bhae') then Result := 'maximale Betriebszeit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'bhaf') then Result := 'maximale Batteriebelastungszeit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'bhag') then Result := 'Start fliegende Eichung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'bhah') then Result := 'Ende fliegende Eichung'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'bhb') then Result := 'Signifikanzzahl des Umwerters'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'bhc') then Result := 'Quarzfrequenz'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'bhd') then Result := 'Referenzstrom'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'bhe') then Result := 'Rechenzyklus'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'bhf') then Result := 'Ereignismeldung'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'bhfa') then Result := 'letztes Ereignis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'bhfb') then Result := 'Datum des Ereignisses'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'bhfc') then Result := 'Zustandsübersicht (Bitleiste)'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'i') then begin
      if (sValue = 'bi') then Result := 'zusammengefaßte Einzelwerte'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'bia') then Result := 'Standardabfrage 1'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'bib') then Result := 'Standardabfrage 2'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'bic') then Result := 'Standardabfrage 3'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'bid') then Result := 'Standardabfrage 4'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'bie') then Result := 'Standardabfrage 5'
      end
    end
    else Result := 'Datenelementadresse nicht gespeichert';
  end;
end;

{ Gibt Datenelementbezeichnung für   }
{ Datenelemente mit Anfagsbuchstaben }
{ 'c' zurück                         }
{ Parameter: Datenelement-Adresse    }
{ Rückgabe: Datenelementbezeichnung  }
{------------------------------------}
function GetDsfgDeaBezeichnungC(sValue: string): string;
{------------------------------------}
begin
  if (sValue = 'c') then Result := 'Registrier-Instanz'
  else begin
    if (Length(sValue) >= 2) and (sValue[2] = 'a') then begin
      if (sValue = 'ca') then Result := 'Archiv'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'caa') then Result := 'Archivgruppe 1'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caaa') then Result := 'Kennung Archivgruppe 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'caab') then Result := 'Zahl Kanäle in Archivgruppe 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'caac') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'caad') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'caae') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'caaf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'caag') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'caah') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'caai') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'caaj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'caak') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'caal') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'caam') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'caan') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'caao') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'caap') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'caaq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'caar') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'caas') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'caat') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'caau') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'caav') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'caaw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'caax') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'caay') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'caaz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'cab') then Result := 'Archivgruppe 2'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caba') then Result := 'Kennung Archivgruppe 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cabb') then Result := 'Zahl Kanäle in Archivgruppe 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cabc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cabd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cabe') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cabf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cabg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cabh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cabi') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cabj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cabk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'cabl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'cabm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'cabn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'cabo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'cabp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'cabq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'cabr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cabs') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'cabt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'cabu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'cabv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'cabw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'cabx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'caby') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'cabz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'cac') then Result := 'Archivgruppe 3'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caca') then Result := 'Kennung Archivgruppe 3'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cacb') then Result := 'Zahl Kanäle in Archivgruppe 3'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cacc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cacd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cace') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cacf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cacg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cach') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'caci') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cacj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cack') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'cacl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'cacm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'cacn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'caco') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'cacp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'cacq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'cacr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cacs') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'cact') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'cacu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'cacv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'cacw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'cacx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'cacy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'cacz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'cad') then Result := 'Archivgruppe 4'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'cada') then Result := 'Kennung Archivgruppe 4'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cadb') then Result := 'Zahl Kanäle in Archivgruppe 4'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cadc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cadd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cade') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cadf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cadg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cadh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cadi') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cadj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cadk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'cadl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'cadm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'cadn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'cado') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'cadp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'cadq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'cadr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cads') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'cadt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'cadu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'cadv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'cadw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'cadx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'cady') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'cadz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'cae') then Result := 'Archivgruppe 5'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caea') then Result := 'Kennung Archivgruppe 5'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'caeb') then Result := 'Zahl Kanäle in Archivgruppe 5'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'caec') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'caed') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'caee') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'caef') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'caeg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'caeh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'caei') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'caej') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'caek') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'cael') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'caem') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'caen') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'caeo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'caep') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'caeq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'caer') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'caes') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'caet') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'caeu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'caev') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'caew') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'caex') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'caey') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'caez') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'caf') then Result := 'Archivgruppe 6'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'cafa') then Result := 'Kennung Archivgruppe 6'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cafb') then Result := 'Zahl Kanäle in Archivgruppe 6'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cafc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cafd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cafe') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'caff') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cafg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cafh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cafi') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cafj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cafk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'cafl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'cafm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'cafn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'cafo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'cafp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'cafq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'cafr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cafs') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'caft') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'cafu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'cafv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'cafw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'cafx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'cafy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'cafz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'cag') then Result := 'Archivgruppe 7'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caga') then Result := 'Kennung Archivgruppe 7'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cagb') then Result := 'Zahl Kanäle in Archivgruppe 7'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cagc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cagd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cage') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cagf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cagg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cagh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cagi') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cagj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cagk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'cagl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'cagm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'cagn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'cago') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'cagp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'cagq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'cagr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cags') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'cagt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'cagu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'cagv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'cagw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'cagx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'cagy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'cagz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'h') then begin
        if (sValue = 'cah') then Result := 'Archivgruppe 8'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caha') then Result := 'Kennung Archivgruppe 8'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cahb') then Result := 'Zahl Kanäle in Archivgruppe 8'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cahc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cahd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cahe') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cahf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cahg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cahh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cahi') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cahj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cahk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'cahl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'cahm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'cahn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'caho') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'cahp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'cahq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'cahr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cahs') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'caht') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'cahu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'cahv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'cahw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'cahx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'cahy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'cahz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'i') then begin
        if (sValue = 'cai') then Result := 'Archivgruppe 9'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caia') then Result := 'Kennung Archivgruppe 9'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'caib') then Result := 'Zahl Kanäle in Archivgruppe 9'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'caic') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'caid') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'caie') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'caif') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'caig') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'caih') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'caii') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'caij') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'caik') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'cail') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'caim') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'cain') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'caio') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'caip') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'caiq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'cair') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cais') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'cait') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'caiu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'caiv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'caiw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'caix') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'caiy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'caiz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'j') then begin
        if (sValue = 'caj') then Result := 'Archivgruppe 10'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caja') then Result := 'Kennung Archivgruppe 10'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cajb') then Result := 'Zahl Kanäle in Archivgruppe 10'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cajc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cajd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'caje') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cajf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cajg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cajh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'caji') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cajj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cajk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'cajl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'cajm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'cajn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'cajo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'cajp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'cajq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'cajr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cajs') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'cajt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'caju') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'cajv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'cajw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'cajx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'cajy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'cajz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'k') then begin
        if (sValue = 'cak') then Result := 'Archivgruppe 11'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caka') then Result := 'Kennung Archivgruppe 11'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cakb') then Result := 'Zahl Kanäle in Archivgruppe 11'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cakc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cakd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cake') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cakf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cakg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cakh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'caki') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cakj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cakk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'cakl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'cakm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'cakn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'cako') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'cakp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'cakq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'cakr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'caks') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'cakt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'caku') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'cakv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'cakw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'cakx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'caky') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'cakz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'l') then begin
        if (sValue = 'cal') then Result := 'Archivgruppe 12'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'cala') then Result := 'Kennung Archivgruppe 12'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'calb') then Result := 'Zahl Kanäle in Archivgruppe 12'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'calc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cald') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cale') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'calf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'calg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'calh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cali') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'calj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'calk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'call') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'calm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'caln') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'calo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'calp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'calq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'calr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cals') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'calt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'calu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'calv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'calw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'calx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'caly') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'calz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'm') then begin
        if (sValue = 'cam') then Result := 'Archivgruppe 13'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'cama') then Result := 'Kennung Archivgruppe 13'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'camb') then Result := 'Zahl Kanäle in Archivgruppe 13'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'camc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'camd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'came') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'camf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'camg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'camh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cami') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'camj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'camk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'caml') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'camm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'camn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'camo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'camp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'camq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'camr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cams') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'camt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'camu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'camv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'camw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'camx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'camy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'camz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'n') then begin
        if (sValue = 'can') then Result := 'Archivgruppe 14'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'cana') then Result := 'Kennung Archivgruppe 14'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'canb') then Result := 'Zahl Kanäle in Archivgruppe 14'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'canc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cand') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cane') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'canf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cang') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'canh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cani') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'canj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cank') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'canl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'canm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'cann') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'cano') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'canp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'canq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'canr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cans') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'cant') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'canu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'canv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'canw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'canx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'cany') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'canz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'o') then begin
        if (sValue = 'cao') then Result := 'Archivgruppe 15'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caoa') then Result := 'Kennung Archivgruppe 15'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'caob') then Result := 'Zahl Kanäle in Archivgruppe 15'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'caoc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'caod') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'caoe') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'caof') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'caog') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'caoh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'caoi') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'caoj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'caok') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'caol') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'caom') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'caon') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'caoo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'caop') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'caoq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'caor') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'caos') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'caot') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'caou') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'caov') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'caow') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'caox') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'caoy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'caoz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'p') then begin
        if (sValue = 'cap') then Result := 'Archivgruppe 16'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'capa') then Result := 'Kennung Archivgruppe 16'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'capb') then Result := 'Zahl Kanäle in Archivgruppe 16'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'capc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'capd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cape') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'capf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'capg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'caph') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'capi') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'capj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'capk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'capl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'capm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'capn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'capo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'capp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'capq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'capr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'caps') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'capt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'capu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'capv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'capw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'capx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'capy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'capz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'q') then begin
        if (sValue = 'caq') then Result := 'Archivgruppe 17'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caqa') then Result := 'Kennung Archivgruppe 17'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'caqb') then Result := 'Zahl Kanäle in Archivgruppe 17'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'caqc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'caqd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'caqe') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'caqf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'caqg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'caqh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'caqi') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'caqj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'caqk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'caql') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'caqm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'caqn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'caqo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'caqp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'caqq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'caqr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'caqs') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'caqt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'caqu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'caqv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'caqw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'caqx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'caqy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'caqz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'r') then begin
        if (sValue = 'car') then Result := 'Archivgruppe 18'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'cara') then Result := 'Kennung Archivgruppe 18'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'carb') then Result := 'Zahl Kanäle in Archivgruppe 18'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'carc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'card') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'care') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'carf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'carg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'carh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cari') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'carj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cark') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'carl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'carm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'carn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'caro') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'carp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'carq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'carr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cars') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'cart') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'caru') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'carv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'carw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'carx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'cary') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'carz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 's') then begin
        if (sValue = 'cas') then Result := 'Archivgruppe 19'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'casa') then Result := 'Kennung Archivgruppe 19'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'casb') then Result := 'Zahl Kanäle in Archivgruppe 19'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'casc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'casd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'case') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'casf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'casg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cash') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'casi') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'casj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cask') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'casl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'casm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'casn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'caso') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'casp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'casq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'casr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cass') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'cast') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'casu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'casv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'casw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'casx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'casy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'casz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 't') then begin
        if (sValue = 'cat') then Result := 'Archivgruppe 20'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'cata') then Result := 'Kennung Archivgruppe 20'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'catb') then Result := 'Zahl Kanäle in Archivgruppe 20'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'catc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'catd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cate') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'catf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'catg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cath') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cati') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'catj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'catk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'catl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'catm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'catn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'cato') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'catp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'catq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'catr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cats') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'catt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'catu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'catv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'catw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'catx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'caty') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'catz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'u') then begin
        if (sValue = 'cau') then Result := 'Archivgruppe 21'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caua') then Result := 'Kennung Archivgruppe 21'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'caub') then Result := 'Zahl Kanäle in Archivgruppe 21'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cauc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'caud') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'caue') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cauf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'caug') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cauh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'caui') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cauj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cauk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'caul') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'caum') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'caun') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'cauo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'caup') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'cauq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'caur') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'caus') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'caut') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'cauu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'cauv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'cauw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'caux') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'cauy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'cauz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'v') then begin
        if (sValue = 'cav') then Result := 'Archivgruppe 21'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'cava') then Result := 'Kennung Archivgruppe 21'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cavb') then Result := 'Zahl Kanäle in Archivgruppe 21'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cavc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cavd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cave') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cavf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cavg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cavh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cavi') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cavj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cavk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'cavl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'cavm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'cavn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'cavo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'cavp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'cavq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'cavr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cavs') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'cavt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'cavu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'cavv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'cavw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'cavx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'cavy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'cavz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'w') then begin
        if (sValue = 'caw') then Result := 'Archivgruppe 22'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'cawa') then Result := 'Kennung Archivgruppe 22'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cawb') then Result := 'Zahl Kanäle in Archivgruppe 22'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cawc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cawd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cawe') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cawf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cawg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cawh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cawi') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cawj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cawk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'cawl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'cawm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'cawn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'cawo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'cawp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'cawq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'cawr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'caws') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'cawt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'cawu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'cawv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'caww') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'cawx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'cawy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'cawz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'x') then begin
        if (sValue = 'cax') then Result := 'Archivgruppe 23'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caxa') then Result := 'Kennung Archivgruppe 23'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'caxb') then Result := 'Zahl Kanäle in Archivgruppe 23'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'caxc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'caxd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'caxe') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'caxf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'caxg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'caxh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'caxi') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'caxj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'caxk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'caxl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'caxm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'caxn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'caxo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'caxp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'caxq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'caxr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'caxs') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'caxt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'caxu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'caxv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'caxw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'caxx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'caxy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'caxz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'y') then begin
        if (sValue = 'cay') then Result := 'Archivgruppe 24'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caya') then Result := 'Kennung Archivgruppe 24'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cayb') then Result := 'Zahl Kanäle in Archivgruppe 24'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cayc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cayd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'caye') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cayf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cayg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cayh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cayi') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cayj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cayk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'cayl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'caym') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'cayn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'cayo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'cayp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'cayq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'cayr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cays') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'cayt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'cayu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'cayv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'cayw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'cayx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'cayy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'cayz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'z') then begin
        if (sValue = 'caz') then Result := 'Archivgruppe 26'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'caza') then Result := 'Kennung Archivgruppe 26'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cazb') then Result := 'Zahl Kanäle in Archivgruppe 26'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cazc') then Result := 'Füllstand von Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cazd') then Result := 'Füllstand bis Ordnungsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'caze') then Result := 'Füllstand, bei dem Service request auftritt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cazf') then Result := 'Archivkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cazg') then Result := 'Archivkanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cazh') then Result := 'Archivkanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cazi') then Result := 'Archivkanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cazj') then Result := 'Archivkanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'cazk') then Result := 'Archivkanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'cazl') then Result := 'Archivkanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'cazm') then Result := 'Archivkanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'cazn') then Result := 'Archivkanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'cazo') then Result := 'Archivkanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'cazp') then Result := 'Archivkanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'cazq') then Result := 'Archivkanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'cazr') then Result := 'Archivkanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'cazs') then Result := 'Archivkanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 't') then begin
          if (sValue = 'cazt') then Result := 'Archivkanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'cazu') then Result := 'Archivkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'v') then begin
          if (sValue = 'cazv') then Result := 'Archivkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'w') then begin
          if (sValue = 'cazw') then Result := 'Archivkanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 18'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'x') then begin
          if (sValue = 'cazx') then Result := 'Archivkanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 19'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'y') then begin
          if (sValue = 'cazy') then Result := 'Archivkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'z') then begin
          if (sValue = 'cazz') then Result := 'Archivkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivtyp'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'EADR der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DEL-Adresse der Quell-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Archivdaten Kanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'CRC12-Startwert der Quelle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'phys.Einheit der Quelle'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'b') then begin
      if (sValue = 'cb') then Result := 'Logbuch'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'cba') then Result := 'Logbuchgruppe 1'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'cbaa') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse A'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cbab') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse B'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cbac') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse C'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cbad') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse D'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cbae') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse E'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cbaf') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse F'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cbag') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse G'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cbah') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse H'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cbai') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse I'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cbaj') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse J'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'cbb') then Result := 'Logbuchgruppe 2'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'cbba') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse K'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cbbb') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse L'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cbbc') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse M'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cbbd') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse N'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cbbe') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse O'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cbbf') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse P'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cbbg') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse Q'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cbbh') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse R'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cbbi') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse S'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cbbj') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse T'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'cbc') then Result := 'Logbuchgruppe 3'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'cbca') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse U'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'cbcb') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse V'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'cbcc') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse W'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'cbcd') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse X'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'cbce') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse Y'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'cbcf') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse Z'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'cbcg') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse ['
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'cbch') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse \'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'cbci') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse ]'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'cbcj') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse ^'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'cbd') then Result := 'Logbuchgruppe 4'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'cbda') then Result := 'Logbuch für DSfG-Teilnehmer mit Adresse _'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstand von Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand bis Ordnungsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand, bei dem Service request auftritt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Logbuch-Einträge'
        end
      end
    end
    else Result := 'Datenelementadresse nicht gespeichert';
  end;
end;

{ Gibt Datenelementbezeichnung für   }
{ Datenelemente mit Anfagsbuchstaben }
{ 'd' zurück                         }
{ Parameter: Datenelement-Adresse    }
{ Rückgabe: Datenelementbezeichnung  }
{------------------------------------}
function GetDsfgDeaBezeichnungD(sValue: string): string;
{------------------------------------}
begin
  if (sValue = 'd') then Result := 'Gasbeschaffenheit'
  else begin
    if (Length(sValue) >= 2) and (sValue[2] = 'a') then begin
      if (sValue = 'da') then Result := 'Messdaten'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'daa') then Result := 'Brennwert'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'daaa') then Result := 'aktueller Wert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'daab') then Result := 'unkorrigierter Wert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'daac') then Result := 'Mittelwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'letzte Viertelstunde'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'letzte Stunde'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'letzter Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'letzter Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'seit dem letztem Ereignis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'dab') then Result := 'Normdichte'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'daba') then Result := 'aktueller Wert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dabb') then Result := 'unkorrigierter Wert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dabc') then Result := 'Mittelwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'letzte Viertelstunde'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'letzte Stunde'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'letzter Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'letzter Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'seit dem letztem Ereignis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'dac') then Result := 'Dichteverhältnis'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'daca') then Result := 'aktueller Wert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dacb') then Result := 'unkorrigierter Wert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dacc') then Result := 'Mittelwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'letzte Viertelstunde'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'letzte Stunde'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'letzter Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'letzter Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'seit dem letztem Ereignis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'dad') then Result := 'Kohlendioxid'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dada') then Result := 'aktueller Wert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dadb') then Result := 'unkorrigierter Wert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dadc') then Result := 'Mittelwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'letzte Viertelstunde'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'letzte Stunde'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'letzter Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'letzter Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'seit dem letztem Ereignis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'dae') then Result := 'Stickstoff'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'daea') then Result := 'aktueller Wert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'daeb') then Result := 'unkorrigierter Wert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'daec') then Result := 'Mittelwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'letzte Viertelstunde'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'letzte Stunde'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'letzter Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'letzter Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'seit dem letztem Ereignis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'daf') then Result := 'Wasserstoff'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dafa') then Result := 'aktueller Wert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dafb') then Result := 'unkorigierter Wert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dafc') then Result := 'Mittelwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'letzte Viertelstunde'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'letzte Stunde'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'letzter Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'letzter Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'seit dem letztem Ereignis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'dag') then Result := 'weitere Daten'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'daga') then Result := 'Anzahl der Analysen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dagb') then Result := 'Mittelwert Brennwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dagc') then Result := 'aktuelle Periode'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'dagd') then Result := 'Wobbe Index'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'dage') then Result := 'aktueller Wert CO2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'dagf') then Result := 'aktueller Wert H2S'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'dagg') then Result := 'akt. Wert Gesamtschwefel'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'h') then begin
        if (sValue = 'dah') then Result := 'Komponenten (aktuell)'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'daha') then Result := 'nitrogen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dahb') then Result := 'methane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dahc') then Result := 'carbondioxide'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'dahd') then Result := 'ethane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'dahe') then Result := 'propane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'dahf') then Result := 'i-butane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'dahg') then Result := 'n-butane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'dahh') then Result := 'neo-pentane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'dahi') then Result := 'i-pentane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'dahj') then Result := 'n-pentane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'dahk') then Result := 'C6+'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'dahl') then Result := 'oxygen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'dahm') then Result := 'carbonmonoxid'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'dahn') then Result := 'C2H4'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'daho') then Result := 'C3H6'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'dahp') then Result := 'helium'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'dahq') then Result := 'hydrogen'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'i') then begin
        if (sValue = 'dai') then Result := 'areas'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'daia') then Result := 'nitrogen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'daib') then Result := 'methane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'daic') then Result := 'carbondioxide'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'daid') then Result := 'ethane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'daie') then Result := 'propane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'daif') then Result := 'i-butane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'daig') then Result := 'n-butane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'daih') then Result := 'neo-pentane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'daii') then Result := 'i-pentane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'daij') then Result := 'n-pentane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'daik') then Result := 'C6+'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'dail') then Result := 'oxygen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'daim') then Result := 'carbonmonoxid'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'dain') then Result := 'C2H4'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'daio') then Result := 'C3H6'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'daip') then Result := 'helium'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'daiq') then Result := 'hydrogen'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'b') then begin
      if (sValue = 'db') then Result := 'Kalibrierdaten'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'dba') then Result := 'Anzahl der Kalibrierläufe'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'dbb') then Result := 'Zeitpunkt der ersten Kalibr.'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'dbc') then Result := 'Intervallzeit'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'dbd') then Result := 'Konzentration Kalibr. Gase'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dbda') then Result := 'nitrogen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dbdb') then Result := 'methane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dbdc') then Result := 'carbondioxide'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'dbdd') then Result := 'ethane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'dbde') then Result := 'propane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'dbdf') then Result := 'i-butane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'dbdg') then Result := 'n-butane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'dbdh') then Result := 'neo-pentane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'dbdi') then Result := 'i-pentane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'dbdj') then Result := 'n-pentane'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'dbdk') then Result := 'C6+'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'dbdl') then Result := 'Brennwert'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'dbdm') then Result := 'Normdichte'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'dbdn') then Result := 'oxygen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'dbdo') then Result := 'carbonmonoxid'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'dbdp') then Result := 'C2H4'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'dbdq') then Result := 'C3H6'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'r') then begin
          if (sValue = 'dbdr') then Result := 'helium'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 's') then begin
          if (sValue = 'dbds') then Result := 'hydrogen'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'dbe') then Result := 'new response factor'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dbea') then Result := 'Komponente 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dbeb') then Result := 'Komponente 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dbec') then Result := 'Komponente 3'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'dbed') then Result := 'Komponente 4'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'dbee') then Result := 'Komponente 5'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'dbef') then Result := 'Komponente 6'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'dbeg') then Result := 'Komponente 7'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'dbeh') then Result := 'Komponente 8'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'dbei') then Result := 'Komponente 9'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'dbej') then Result := 'Komponente 10'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'dbek') then Result := 'Komponente 11'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'dbel') then Result := 'Komponente 12'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'dbem') then Result := 'Komponente 13'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'dben') then Result := 'Komponente 14'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'dbeo') then Result := 'Komponente 15'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'dbep') then Result := 'Komponente 16'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'dbeq') then Result := 'Komponente 17'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'dbf') then Result := 'new retention time'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dbfa') then Result := 'Komponente 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dbfb') then Result := 'Komponente 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dbfc') then Result := 'Komponente 3'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'dbfd') then Result := 'Komponente 4'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'dbfe') then Result := 'Komponente 5'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'dbff') then Result := 'Komponente 6'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'dbfg') then Result := 'Komponente 7'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'dbfh') then Result := 'Komponente 8'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'dbfi') then Result := 'Komponente 9'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'dbfj') then Result := 'Komponente 10'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'dbfk') then Result := 'Komponente 11'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'dbfl') then Result := 'Komponente 12'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'dbfm') then Result := 'Komponente 13'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'dbfn') then Result := 'Komponente 14'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'dbfo') then Result := 'Komponente 15'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'dbfp') then Result := 'Komponente 16'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'dbfq') then Result := 'Komponente 17'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'dbg') then Result := 'retention time at zero'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dbga') then Result := 'Komponente 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dbgb') then Result := 'Komponente 2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dbgc') then Result := 'Komponente 3'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'dbgd') then Result := 'Komponente 4'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'dbge') then Result := 'Komponente 5'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'dbgf') then Result := 'Komponente 6'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'dbgg') then Result := 'Komponente 7'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'dbgh') then Result := 'Komponente 8'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'dbgi') then Result := 'Komponente 9'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'dbgj') then Result := 'Komponente 10'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'dbgk') then Result := 'Komponente 11'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'dbgl') then Result := 'Komponente 12'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'dbgm') then Result := 'Komponente 13'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'dbgn') then Result := 'Komponente 14'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'dbgo') then Result := 'Komponente 15'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'dbgp') then Result := 'Komponente 16'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'dbgq') then Result := 'Komponente 17'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'c') then begin
      if (sValue = 'dc') then Result := 'Polynom Koeffizienten'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'dca') then Result := 'nitrogen'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dcaa') then Result := 'Polynom Koeffizient a0'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dcab') then Result := 'Polynom Koeffizient a1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dcac') then Result := 'Polynom Koeffizient a2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'dcad') then Result := 'Polynom Koeffizient a3'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'dcb') then Result := 'methane'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dcba') then Result := 'Polynom Koeffizient a0'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dcbb') then Result := 'Polynom Koeffizient a1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dcbc') then Result := 'Polynom Koeffizient a2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'dcbd') then Result := 'Polynom Koeffizient a3'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'h') then begin
        if (sValue = 'dch') then Result := 'carbondioxide'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dcha') then Result := 'Polynom Koeffizient a0'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dchb') then Result := 'Polynom Koeffizient a1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dchc') then Result := 'Polynom Koeffizient a2'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'dchd') then Result := 'Polynom Koeffizient a3'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'i') then begin
        if (sValue = 'dci') then Result := 'fixed components'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dcia') then Result := 'helium'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dcib') then Result := 'oxygen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dcic') then Result := 'hydrogen'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'dcid') then Result := 'argon'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'd') then begin
      if (sValue = 'dd') then Result := 'sonstige Parameter'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'dda') then Result := 'unnormalised sum'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'ddb') then Result := 'Status der Analyse'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'ddc') then Result := 'Löschen einer Periode'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'ddd') then Result := 'aktuelle Periode'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'dde') then Result := 'Abweichung response fact.'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'ddf') then Result := 'Abweichung retention-time'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'ddg') then Result := 'Abweichung unnormal. sum'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'h') then begin
        if (sValue = 'ddh') then Result := 'Grenzwert Säulentemperatur'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'i') then begin
        if (sValue = 'ddi') then Result := 'Abweichung Ho bei Kalibr.'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'j') then begin
        if (sValue = 'ddj') then Result := 'Abweichung rho,n bei Kalibr.'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'k') then begin
        if (sValue = 'ddk') then Result := 'Abweichung CO2 bei Kalibr.'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'l') then begin
        if (sValue = 'ddl') then Result := 'externes kal. Gas'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'm') then begin
        if (sValue = 'ddm') then Result := 'Ofentemperatur'
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'e') then begin
      if (sValue = 'de') then Result := 'GC-Status'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'dea') then Result := 'Betriebsbereitschaft'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'deb') then Result := 'vorgegebene Analysenzeit'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'dec') then Result := 'aktuelle Analysenzeit'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'ded') then Result := 'Zeit bis zur nächsten Kalibr.'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'dee') then Result := 'Datum'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'def') then Result := 'Uhrzeit'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'deg') then Result := 'Gerätenummer'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'h') then begin
        if (sValue = 'deh') then Result := 'Fehleranzeige'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'i') then begin
        if (sValue = 'dei') then Result := 'Status'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'j') then begin
        if (sValue = 'dej') then Result := 'Betriebszeit'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'k') then begin
        if (sValue = 'dek') then Result := 'Batteriebelastungszeit'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'l') then begin
        if (sValue = 'del') then Result := 'Signifikanzzahl'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'm') then begin
        if (sValue = 'dem') then Result := 'Quarzfrequenz'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'n') then begin
        if (sValue = 'den') then Result := 'Referenzstrom'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'o') then begin
        if (sValue = 'deo') then Result := 'Rechenzyklus'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'p') then begin
        if (sValue = 'dep') then Result := 'Ereignismeldung'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'q') then begin
        if (sValue = 'deq') then Result := 'letztes Ereignis'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'r') then begin
        if (sValue = 'der') then Result := 'Datum des Ereignisses'
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'f') then begin
      if (sValue = 'df') then Result := 'Schnittstellen'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'dfa') then Result := 'RS 232 - 1'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dfaa') then Result := 'Baud'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dfab') then Result := '7 / 8 Bit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dfac') then Result := 'Parity'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'dfb') then Result := 'RS 232 - 2'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dfba') then Result := 'Baud'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dfbb') then Result := '7 / 8 Bit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dfbc') then Result := 'Parity'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'dfc') then Result := 'RS 232 - 3'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dfca') then Result := 'Baud'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dfcb') then Result := '7 / 8 Bit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dfcc') then Result := 'Parity'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'dfd') then Result := 'RS 232 - 4'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dfda') then Result := 'Baud'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dfdb') then Result := '7 / 8 Bit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dfdc') then Result := 'Parity'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'dfe') then Result := 'RS 232 - 5'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dfea') then Result := 'Baud'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dfeb') then Result := '7 / 8 Bit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dfec') then Result := 'Parity'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'dff') then Result := 'RS 232 - 6'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dffa') then Result := 'Baud'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dffb') then Result := '7 / 8 Bit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dffc') then Result := 'Parity'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'dfg') then Result := 'Drucker'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dfga') then Result := 'Kalibrierdaten'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus: Ein / Aus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Daten / ohne Grafik'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Daten / mit Grafik'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dfgb') then Result := 'Analysenprotokoll'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Modus: Ein / Aus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Daten / ohne Grafik'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Daten / mit Grafik'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'h') then begin
        if (sValue = 'dfh') then Result := 'Stromausgänge'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'dfha') then Result := 'physikalischer Wert 1'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'dfhb') then Result := 'Strom (mA)'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'dfhc') then Result := 'min. Bereich'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'dfhd') then Result := 'max. Bereich'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'dfhe') then Result := 'Auswahl'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'dfhf') then Result := 'Korrekturfaktor'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'dfhg') then Result := 'Mittlungsfaktor'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'dfhh') then Result := 'Modus'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'g') then begin
      if (sValue = 'dg') then Result := 'Reserve'
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'h') then begin
      if (sValue = 'dh') then Result := 'Reserve'
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'i') then begin
      if (sValue = 'di') then Result := 'zusammengefaßte Einzelwerte'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'dia') then Result := 'Standardabfrage 1'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'dib') then Result := 'Standardabfrage 2'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'dic') then Result := 'weitere Standardabfragen reserviert für spätere Anwendung'
      end
    end
    else Result := 'Datenelementadresse nicht gespeichert';
  end;
end;

{ Gibt Datenelementbezeichnung für   }
{ Datenelemente mit Anfagsbuchstaben }
{ 'f' zurück                         }
{ Parameter: Datenelement-Adresse    }
{ Rückgabe: Datenelementbezeichnung  }
{------------------------------------}
function GetDsfgDeaBezeichnungF(sValue: string): string;
{------------------------------------}
begin
  if (sValue = 'f') then Result := 'Steuereinheit spezifisch'
  else if (sValue = 'fa') then Result := 'Sammel-Ereignisse'
  else if (sValue = 'fb') then Result := 'Prozeß-EA Momentanwerte'
  else if (sValue = 'fba') then Result := 'Interface-Karte 1'
  else if (sValue = 'fbaa') then Result := 'Kartentyp/Versionsstand'
  else if (sValue = 'fbab') then Result := 'Endwert Kanal 1'
  else if (sValue = 'fbac') then Result := 'Endwert Kanal 2'
  else if (sValue = 'fc') then Result := 'Prozeß-EA Kennwerte'
  else if (sValue = 'fca') then Result := 'Interface-Karte 1'
  else if (sValue = 'fcaa') then Result := 'Kanal 1'
  else if (sValue = 'fd') then Result := 'Funktion Gaszähler/Zählerschutz'
  else if (sValue = 'fda') then Result := 'Gaszähler Momentanwerte'
  else if (sValue = 'fdaa') then Result := 'Gaszähler 1'
  else if (sValue = 'fdaaa') then Result := 'VB'
  else if (sValue = 'fdaab') then Result := 'QB'
  else if (sValue = 'fdaac') then Result := 'VN'
  else if (sValue = 'fdaad') then Result := 'QN'
  else if (sValue = 'fdaae') then Result := 'E'
  else if (sValue = 'fdaaf') then Result := 'QE'
  else if (sValue = 'fdaag') then Result := 'Grenzwerte verletzt (Bitleiste)'
  else if (sValue = 'fe') then Result := 'Regler'
  else if (sValue = 'fea') then Result := 'Regler Nr. 1'
  else if (sValue = 'feaa') then Result := 'Regler-Ausprägung 1'
  else if (sValue = 'feaaa') then Result := 'Reglertyp'
  else if (sValue = 'feaab') then Result := 'Sollwert'
  else if (sValue = 'feaac') then Result := 'Reglerzustand'
  else Result := 'Datenelementadresse nicht gespeichert';
end;

{ Gibt Datenelementbezeichnung für   }
{ Datenelemente mit Anfagsbuchstaben }
{ 'w' zurück                         }
{ Parameter: Datenelement-Adresse    }
{ Rückgabe: Datenelementbezeichnung  }
{------------------------------------}
function GetDsfgDeaBezeichnungW(sValue: string): string;
{------------------------------------}
begin
  if (sValue = 'w') then Result := 'MRG-Instanz'
  else begin
    if (Length(sValue) >= 2) and (sValue[2] = 'a') then begin
      if (sValue = 'wa') then Result := 'Systemdaten'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'waa') then Result := 'Gerätedaten'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'waaa') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Geräteversion2200'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Gerätekennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Standardparametersatz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Fabriknummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Baujahr'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Version der DCF77-Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Version der ext. Speichereinheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'waab') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'MRG-Zustandsübersicht'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Zustand der DCF77-Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Zustand der ext. Speichereinheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Fehlernummer 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Fehlernummer 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Fehlernummer 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Fehlernummer 4'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'waac') then Result := 'Ereignisse'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Löschzeit Ereignisse'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisliste'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'wab') then Result := 'Zeitsystem'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'waba') then Result := 'aktuelle Zeit und Datum'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zeit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Datum'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wabb') then Result := 'Gastag und Gasjahr'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Tagesende Stunde'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Jahresende Monat'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wabc') then Result := 'Zeitumschaltung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Sommerzeitumschaltung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'akt. Sommerzeitumschaltung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'akt. Winterzeitumschaltung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wabd') then Result := 'Synchronisation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Sync.Tele auf DSfG-Bus senden'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Synchronisation durch Kontakt'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'wac') then Result := 'Zeitkontakte'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'waca') then Result := 'Zeitkontakt 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Auslösungszeit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Kontaktausgangsnummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wacb') then Result := 'Zeitkontakt 2 usw.'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'wad') then Result := 'Identifikation I/O-Karten'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wada') then Result := 'I/O-Karte 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'ID-Nummer'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wadb') then Result := 'I/O-Karte 2 usw.'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'wae') then Result := 'Konfiguration Ein-/Ausgänge'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'waea') then Result := 'Impulseingänge'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Anzahl'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'waeb') then Result := 'Impulsausgänge'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Anzahl'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'waec') then Result := 'Analogeingänge'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Anzahl'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'waed') then Result := 'Analogausgänge'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Anzahl'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'waee') then Result := 'Meldeeingänge'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Anzahl'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'waef') then Result := 'Meldeausgänge'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Anzahl'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'b') then begin
      if (sValue = 'wb') then Result := 'Ereignisverwaltung'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'wba') then Result := 'Systemmeldungen'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wbaa') then Result := 'Gesamtzustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Meldungszustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Quittierungszustand'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wbab') then Result := 'Softwarefehler 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wbac') then Result := 'Softwarefehler 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wbad') then Result := 'Standardparameter aktiviert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wbae') then Result := 'Systemreset'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wbaf') then Result := 'DSfG-Störung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'wbag') then Result := 'Betriebsspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'wbah') then Result := 'Netzspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'wbai') then Result := 'Uhr gestellt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'wbaj') then Result := 'DSfG-Sync. abgelehnt'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'wbak') then Result := 'Eichschalter'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'wbal') then Result := 'Revisionsschalter'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'wbam') then Result := 'Summenspeicher X voll'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'wban') then Result := 'Summenspeicher U voll'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'wbb') then Result := 'Löschen von Archiven'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wbba') then Result := 'Gesamtzustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Meldungszustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Quittierungszustand'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wbbb') then Result := 'Archiv/Logbuch Umwerter 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wbbc') then Result := 'Archiv/Logbuch Umwerter 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wbbd') then Result := 'Archiv/Logbuch Umwerter 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wbbe') then Result := 'Archiv/Logbuch Umwerter 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wbbf') then Result := 'Archiv/Logbuch Umwerter 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'wbbg') then Result := 'Archiv/Logbuch Umwerter 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'wbbh') then Result := 'Archiv/Logbuch Umwerter 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'wbbi') then Result := 'Archiv/Logbuch Umwerter 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'wbbj') then Result := 'Archiv/Logbuch PGC 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'wbbk') then Result := 'Archiv/Logbuch PGC 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'wbbl') then Result := 'Archiv/Logbuch Prozess 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'wbbm') then Result := 'Archiv/Logbuch Prozess 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'wbbn') then Result := 'Archiv/Logbuch Prozess 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'wbbo') then Result := 'Archiv/Logbuch Prozess 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'wbbp') then Result := 'Archiv/Logbuch MRG'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'wbc') then Result := 'Löschen von Höchstwerten'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wbca') then Result := 'Gesamtzustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Meldungszustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Quittierungszustand'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wbcb') then Result := 'Höchstwerte Umwerter 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wbcc') then Result := 'Höchstwerte Umwerter 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wbcd') then Result := 'Höchstwerte Umwerter 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wbce') then Result := 'Höchstwerte Umwerter 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wbcf') then Result := 'Höchstwerte Umwerter 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'wbcg') then Result := 'Höchstwerte Umwerter 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'wbch') then Result := 'Höchstwerte Umwerter 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'wbci') then Result := 'Höchstwerte Umwerter 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'wbcj') then Result := 'Höchstwerte MRG'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'wbd') then Result := 'Analoggrenzwerte oben 1-16'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wbda') then Result := 'Gesamtzustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Meldungszustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Quittierungszustand'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wbdb') then Result := 'Analogkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'wbdq') then Result := 'Analogkanal 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'wbe') then Result := 'Analoggrenzwerte oben 17-32'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wbea') then Result := 'Gesamtzustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Meldungszustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Quittierungszustand'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wbeb') then Result := 'Analogkanal 17'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'wbeq') then Result := 'Analogkanal 32'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'wbf') then Result := 'Analoggrenzwerte oben 33-48'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wbfa') then Result := 'Gesamtzustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Meldungszustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Quittierungszustand'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wbfb') then Result := 'Analogkanal 33'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'wbfq') then Result := 'Analogkanal 48'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'wbg') then Result := 'Analoggrenzwerte oben 49-60'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wbga') then Result := 'Gesamtzustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Meldungszustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Quittierungszustand'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wbgb') then Result := 'Analogkanal 49'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'wbgq') then Result := 'Analogkanal 60'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'h') then begin
        if (sValue = 'wbh') then Result := 'Analoggrenzwerte unten 1-20'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wbha') then Result := 'Gesamtzustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Meldungszustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Quittierungszustand'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wbhb') then Result := 'Analogkanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'wbhu') then Result := 'Analogkanal 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'i') then begin
        if (sValue = 'wbi') then Result := 'Analoggrenzwerte unten 21-40'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wbia') then Result := 'Gesamtzustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Meldungszustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Quittierungszustand'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wbib') then Result := 'Analogkanal 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'wbiu') then Result := 'Analogkanal 40'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'j') then begin
        if (sValue = 'wbj') then Result := 'Analoggrenzwerte unten 41-60'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wbja') then Result := 'Gesamtzustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Meldungszustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Quittierungszustand'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wbjb') then Result := 'Analogkanal 41'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'wbju') then Result := 'Analogkanal 60'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'l') then begin
        if (sValue = 'wbl') then Result := 'Externe Meldungen 1-20'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wbla') then Result := 'Gesamtzustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Meldungszustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Quittierungszustand'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wblb') then Result := 'Ext. Meldung 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'wblu') then Result := 'Ext. Meldung 20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'm') then begin
        if (sValue = 'wbm') then Result := 'Externe Meldungen 21-40'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wbma') then Result := 'Gesamtzustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Meldungszustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Quittierungszustand'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wbmb') then Result := 'Ext. Meldung 21'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'wbmu') then Result := 'Ext. Meldung 40'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'n') then begin
        if (sValue = 'wbn') then Result := 'Externe Meldungen 41-60'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wbna') then Result := 'Gesamtzustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Meldungszustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Quittierungszustand'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wbnb') then Result := 'Ext. Meldung 41'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'u') then begin
          if (sValue = 'wbnu') then Result := 'Ext. Meldung 60'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Ereignisreaktionen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Meldungstext'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'c') then begin
      if (sValue = 'wc') then Result := 'Impulseingänge'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'wca') then Result := 'Impulseingänge 1-12'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wcaa') then Result := 'Kanalstatus 1-12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Kanal 1-12 aktiv'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Kanal 1-12 in Prüfung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wcab') then Result := 'Impulseingang 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Eingangszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Messperiodenzähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Tageszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Monatszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsfaktor'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Impulsteiler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wcac') then Result := 'Impulseingang 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Eingangszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Messperiodenzähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Tagszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Monatszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsfaktor'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Impulsteiler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wcad') then Result := 'Impulseingang 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Eingangszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Messperiodenzähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Tageszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Monatszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsfaktor'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Impulsteiler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Knalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wcae') then Result := 'Impulseingang 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Eingangszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Messperiodenzähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Tageszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Monatszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsfaktor'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Impulsteiler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wcaf') then Result := 'Impulseingang 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Eingangszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Messperiodenzähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Tageszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Monatszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsfaktor'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Impulsteiler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'wcag') then Result := 'Impulseingang 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Eingangszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Messperiodenzähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Tageszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Monatszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsfaktor'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Impulsteiler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'wcah') then Result := 'Impulseingang 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Eingangszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Messperiodenzähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Tageszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Monatszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsfaktor'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Impulsteiler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'wcai') then Result := 'Impulseingang 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Eingangszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Messperiodenzähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Tageszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Monatszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsfaktor'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Impulsteiler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'wcaj') then Result := 'Impulseingang 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Eingangszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Messperiodenzähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Tageszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Monatszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsfaktor'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Impulsteiler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'wcak') then Result := 'Impulseingang 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Eingangszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Messperiodenzähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Tageszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Monatszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsfaktor'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Impulsteiler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'wcal') then Result := 'Impulseingang 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Eingangszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Messperiodehzähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Tageszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Monatszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsfaktor'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Impulsteiler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'wcam') then Result := 'Impulseingang 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Eingangszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Messperiodenzähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Tageszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Monatszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Impulsfaktor'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Impulsteiler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Kanalbezeichnung'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'd') then begin
      if (sValue = 'wd') then Result := 'reserviert'
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'e') then begin
      if (sValue = 'we') then Result := 'Analogeingänge'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'wea') then Result := 'Analogeingänge 1-16'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'weaa') then Result := 'Kanalstatus 1-16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Kanal 1-16 aktiv'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'weab') then Result := 'Analogeingang 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grenzwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grenzwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'weac') then Result := 'Analogeingang 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grenzwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grenzwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wead') then Result := 'Analogeingang 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grenzwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grenzwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'weae') then Result := 'Analogeingang 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grezwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grezwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'weaf') then Result := 'Analogeingang 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grenzwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grenzwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'weag') then Result := 'Analogeingang 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grenzwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grenzwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'weah') then Result := 'Analogeingang 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspanung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grenzwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grezwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'weai') then Result := 'Analogeingang 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grenzwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grenzwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'weaj') then Result := 'Analogeingang 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grezwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grenzwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'weak') then Result := 'Analogeingang 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grenzwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grenzwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'weal') then Result := 'Analogeingang 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grenzwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grenzwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'weam') then Result := 'Analogausgang 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grenzwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grenzwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'wean') then Result := 'Analogeingang 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grenzwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grenzwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'weao') then Result := 'Analogeingang 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grenzwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grenzwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'weap') then Result := 'Analogeingang 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grenzwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grenzwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'weaq') then Result := 'Analogeingang 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Mittelwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Momentanwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Messstrom'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Einheit'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Kommastellen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'g') then Result := 'Messspannung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'h') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'i') then Result := 'Messbereich von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'j') then Result := 'Messbereich bis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'k') then Result := 'Grenzwert min'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'l') then Result := 'Grenzwert max'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'm') then Result := 'Grenzwert Periode'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'n') then Result := 'Grenzwertmodus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'o') then Result := 'Grenzwert auf Ausgangsnr.'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'p') then Result := 'Kanalbezeichnung'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'f') then begin
      if (sValue = 'wf') then Result := 'Analogausgänge'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'wfa') then Result := 'Analogausgänge 1-16'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wfaa') then Result := 'Kanalstatus 1-16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Kanal 1-16 aktiv'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wfab') then Result := 'Analogausgang 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wfac') then Result := 'Analogausgang 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wfad') then Result := 'Analogausgang 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wfae') then Result := 'Analogausgang 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wfaf') then Result := 'Analogausgang 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'wfag') then Result := 'Analogausgang 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'wfah') then Result := 'Analogausgang 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'wfai') then Result := 'Analogausgang 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'wfaj') then Result := 'Analogausgang 9'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'wfak') then Result := 'Analogausgang 10'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'l') then begin
          if (sValue = 'wfal') then Result := 'Analogausgang 11'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'm') then begin
          if (sValue = 'wfam') then Result := 'Analogausgang 12'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'n') then begin
          if (sValue = 'wfan') then Result := 'Analogausgang 13'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'o') then begin
          if (sValue = 'wfao') then Result := 'Analogausgang 14'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'p') then begin
          if (sValue = 'wfap') then Result := 'Analogausgang 15'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'q') then begin
          if (sValue = 'wfaq') then Result := 'Analogausgang 16'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stellwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Strombereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Spannungsbereich'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Kanalbezeichnung'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'g') then begin
      if (sValue = 'wg') then Result := 'MRG-Summierung (Impulse)'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'wga') then Result := 'Summierung 1'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wgaa') then Result := 'Zähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Messperiodenzähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Tageszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Monatszähler'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wgab') then Result := 'Bewertung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Faktor Ausgang'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Teiler Ausgang'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wgac') then Result := 'Summendefinition'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Kanalauswahl 01-20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Kanalauswahl 21-40'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Kanalauswahl 41-60'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wgad') then Result := 'Summenausgang'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zeit Kontakt auf [ms]'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Zeit Kontakt zu [ms]'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'akt. Summenspeicher'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Max. Summenspeicher'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wgae') then Result := 'Signalausgangsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Ausgang 1'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'wgb') then Result := 'Summierung 2 usw.'
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'h') then begin
      if (sValue = 'wh') then Result := 'Umwerter-Summierung (DSfG)'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'wha') then Result := 'Summierung 1'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'whaa') then Result := 'Zähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Messperiodenzähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Tageszähler'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Monatszähler'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'whab') then Result := 'Bewertung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Faktor Ausgang'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Teiler Ausgang'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'whac') then Result := 'Summendefinition'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Umwerterauswahl 1-8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Umwerterkanal (Vb,Vn,E)'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Zählerstandsabfragezyklus [ms]'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'whad') then Result := 'Summenausgang'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zeit Kontakt auf [ms]'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Zeit Kontakt zu [ms]'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'akt. Summenspeicher'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Max. Summenspeicher'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'whae') then Result := 'Signalausgangsnummer'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Ausgang 1'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'whb') then Result := 'Summierung 2 usw.'
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'i') then begin
      if (sValue = 'wi') then Result := 'DSfG-Parametrierung'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'wia') then Result := 'Schnittstelle'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wiaa') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'DSfG-SSK Version'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wiab') then Result := 'Zustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'DSfG-SSK Status'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'DSfG-Fehlerliste'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DSfG-Teilnehmer A-P'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'DSfG-Teilnehmer Q-_'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'e') then Result := 'Anzahl Empfangstelegramme'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'f') then Result := 'Anzahl Sendetelegramme'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wiac') then Result := 'Busparameter'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Baudrate'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'DSfG-Timeout Ta'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'DSfG-Timeout Tc'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'DSfG-Timeout Ts'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'wib') then Result := 'MRG'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wiba') then Result := 'Adressen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Adresse MRG-Instanz'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Adresse Registrierinstanz'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wibb') then Result := 'Pollingparameter'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Generalpollingzyklus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'erneutes Polling nach NAK-Wdh.'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wibc') then Result := 'CRC12-Parameter'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'CRC12-Startwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'CRC12 an DFUE senden'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'wic') then Result := 'Umwerter'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wica') then Result := 'Momentanwertabfragezyklus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Abfragezyklus'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wicb') then Result := 'Zweirichtungsbetrieb'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Umwerter 1/2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Umwerter 3/4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Umwerter 5/6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'Umwerter 7/8'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wicc') then Result := 'Prüfungsschaltung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Umwerter in Prüfung'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wicd') then Result := 'Umwerter 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Adresse'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Störzählerabfrage'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'CRC12-Startwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'erw. Zählwerksstruktur'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wice') then Result := 'Umwerter 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Adresse'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Störzählerabfrage'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'CRC12-Startwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'erw. Zählwerkstruktur'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wicf') then Result := 'Umwerter 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Adresse'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Störzählerabfrage'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'CRC12-Startwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'erw. Zählwerkstruktur'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'wicg') then Result := 'Umwerter 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Adresse'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Störzählerabfrage'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'CRC12-Startwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'erw. Zählwerkstruktur'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'wich') then Result := 'Umwerter 5'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Adresse'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Störzählerabfrage'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'CRC12-Startwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'erw. Zählwerkstruktur'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'i') then begin
          if (sValue = 'wici') then Result := 'Umwerter 6'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Adresse'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Störzählerabfrage'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'CRC12-Startwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'erw. Zählwerkstruktur'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'j') then begin
          if (sValue = 'wicj') then Result := 'Umwerter 7'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Adresse'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Störzählerabfrage'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'CRC12-Startwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'erw. Zählwerkstruktur'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'k') then begin
          if (sValue = 'wick') then Result := 'Umwerter 8'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Adresse'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Störzählerabfrage'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'CRC12-Startwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'd') then Result := 'erw. Zählwerkstruktur'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'wid') then Result := 'PGC'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wida') then Result := 'Momentanwertabfragezyklus'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Abfragezyklus'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'widb') then Result := 'PGC 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Adresse'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'CRC12-Startwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Aufzeichnungsmodus'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'widc') then Result := 'PGC 2'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Adresse'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'CRC12-Startwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Aufzeichnungsmodus'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'widd') then Result := 'PGC 3'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Adresse'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'CRC12-Startwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Aufzeichnungsmodus'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wide') then Result := 'PGC 4'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Adresse'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'CRC12-Startwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Aufzeichnungsmodus'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'j') then begin
      if (sValue = 'wj') then Result := 'Parametrierung der Prozessarchive'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'wja') then Result := 'Archiv 1'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wjaa') then Result := 'Aufzeichnungsart'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aufzeichnungstakt'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wjab') then Result := 'Quellenidentifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Adresse'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wjac') then Result := 'Einheiten für Kanäle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Einheit Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit Kanal 2 usw.'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wjad') then Result := 'Datenelemente für Kanäle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Datenelement Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Datenelement Kanal 2 usw.'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wjae') then Result := 'Datentypen für Kanäle'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Datentyp Kanal 1'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Datentyp Kanal 2 usw.'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'wjb') then Result := 'Var. Archiv 2 usw.'
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'k') then begin
      if (sValue = 'wk') then Result := 'DSfG-Archivgruppen'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'wka') then Result := 'Archivorganisation'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkaa') then Result := 'Konfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Archivgruppen'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'wkb') then Result := 'Archivgruppe 1'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkba') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkbb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkbc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'wkc') then Result := 'Archivgruppe 2'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkca') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkcb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkcc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'wkd') then Result := 'Archivgruppe 3'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkda') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkdb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkdc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'wke') then Result := 'Archivgruppe 4'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkea') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkeb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkec') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'wkf') then Result := 'Archivgruppe 5'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkfa') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkfb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkfc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'wkg') then Result := 'Archivgruppe 6'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkga') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkgb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkgc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'h') then begin
        if (sValue = 'wkh') then Result := 'Archivgruppe 7'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkha') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkhb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkhc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'i') then begin
        if (sValue = 'wki') then Result := 'Archivgruppe 8'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkia') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkib') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkic') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'j') then begin
        if (sValue = 'wkj') then Result := 'Archivgruppe 9'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkja') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkjb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkjc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'k') then begin
        if (sValue = 'wkk') then Result := 'Archivgruppe 10'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkka') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkkb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkkc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'l') then begin
        if (sValue = 'wkl') then Result := 'Archivgruppe 11'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkla') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wklb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wklc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'm') then begin
        if (sValue = 'wkm') then Result := 'Archivgruppe 12'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkma') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkmb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkmc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'n') then begin
        if (sValue = 'wkn') then Result := 'Archivgruppe 13'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkna') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max, Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wknb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wknc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'o') then begin
        if (sValue = 'wko') then Result := 'Archivgruppe 14'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkoa') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkob') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkoc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'p') then begin
        if (sValue = 'wkp') then Result := 'Archivgruppe 15'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkpa') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkpb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkpc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'q') then begin
        if (sValue = 'wkq') then Result := 'Archivgruppe 16'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkqa') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkqb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkqc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'r') then begin
        if (sValue = 'wkr') then Result := 'Archivgruppe 17'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkra') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkrb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkrc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 's') then begin
        if (sValue = 'wks') then Result := 'Archivgruppe 18'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wksa') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wksb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wksc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 't') then begin
        if (sValue = 'wkt') then Result := 'Archivgruppe 19'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkta') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wktb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wktc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'u') then begin
        if (sValue = 'wku') then Result := 'Archivgruppe 20'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkua') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkub') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkuc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'v') then begin
        if (sValue = 'wkv') then Result := 'Archivgruppe 21'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkva') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkvb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkvc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'w') then begin
        if (sValue = 'wkw') then Result := 'Archivgruppe 22'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkwa') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkwb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkwc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'x') then begin
        if (sValue = 'wkx') then Result := 'Archivgruppe 23'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkxa') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkxb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkxc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'y') then begin
        if (sValue = 'wky') then Result := 'Archivgruppe 24'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkya') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkyb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkyc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'z') then begin
        if (sValue = 'wkz') then Result := 'Archivgruppe 25'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wkza') then Result := 'Identifikation'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Archivgruppenkennung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'max. Anzahl Datensätze'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wkzb') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wkzc') then Result := 'Kanalkonfiguration'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Aktive Kanäle'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'l') then begin
      if (sValue = 'wl') then Result := 'DSfG-Logbücher'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'wla') then Result := 'Logbuchorganisation'
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'wlb') then Result := 'Logbuch 1'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wlba') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'wlc') then Result := 'Logbuch 2'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wlca') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'wld') then Result := 'Logbuch 3'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wlda') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'wle') then Result := 'Logbuch 4'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wlea') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'wlf') then Result := 'Logbuch 5'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wlfa') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'wlg') then Result := 'Logbuch 6'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wlga') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'h') then begin
        if (sValue = 'wlh') then Result := 'Logbuch 7'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wlha') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'i') then begin
        if (sValue = 'wli') then Result := 'Logbuch 8'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wlia') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'j') then begin
        if (sValue = 'wlj') then Result := 'Logbuch 9'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wlja') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'k') then begin
        if (sValue = 'wlk') then Result := 'Logbuch 10'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wlka') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'l') then begin
        if (sValue = 'wll') then Result := 'Logbuch 11'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wlla') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'm') then begin
        if (sValue = 'wlm') then Result := 'Logbuch 12'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wlma') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'n') then begin
        if (sValue = 'wln') then Result := 'Logbuch 13'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wlna') then Result := 'Füllstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Füllstandswarnung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Füllstand von'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Füllstand bis'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'm') then begin
      if (sValue = 'wm') then Result := 'Umwerter-Momentanwerte'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'wma') then Result := 'Umwerter 1'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wmaa') then Result := 'Originalzählerstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wmab') then Result := 'Betriebsvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wmac') then Result := 'Normvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wmad') then Result := 'Wärmemenge'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wmae') then Result := 'Druck'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wmaf') then Result := 'Temperatur'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'wmag') then Result := 'Betriebsdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'wmah') then Result := 'Normdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'wmb') then Result := 'Umwerter 2'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wmba') then Result := 'Originalzählerstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wmbb') then Result := 'Betriebsvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wmbc') then Result := 'Normvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wmbd') then Result := 'Wärmemenge'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wmbe') then Result := 'Druck'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wmbf') then Result := 'Temperatur'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'wmbg') then Result := 'Betriebsdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'wmbh') then Result := 'Normdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'wmc') then Result := 'Umwerter 3'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wmca') then Result := 'Originalzählerstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wmcb') then Result := 'Betriebsvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wmcc') then Result := 'Normvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wmcd') then Result := 'Wärmemenge'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wmce') then Result := 'Druck'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wmcf') then Result := 'Temperatur'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'wmcg') then Result := 'Betriebsdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'wmch') then Result := 'Normdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'wmd') then Result := ' Umwerter 4'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wmda') then Result := 'Originalzählerstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wmdb') then Result := 'Betriebsvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wmdc') then Result := 'Normvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wmdd') then Result := 'Wärmemenge'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wmde') then Result := 'Druck'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wmdf') then Result := 'Temperatur'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'wmdg') then Result := 'Betriebsdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'wmdh') then Result := 'Normdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'wme') then Result := 'Umwerter 5'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wmea') then Result := 'Originalzählerstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wmeb') then Result := 'Betriebsvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wmec') then Result := 'Normvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wmed') then Result := 'Wärmemenge'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wmee') then Result := 'Druck'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wmef') then Result := 'Temperatur'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'wmeg') then Result := 'Betriebsdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'wmeh') then Result := 'Normdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'wmf') then Result := 'Umwerter 6'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wmfa') then Result := 'Originalzählerzustand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wmfb') then Result := 'Betriebsvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wmfc') then Result := 'Normvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wmfd') then Result := 'Wärmemenge'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wmfe') then Result := 'Druck'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wmff') then Result := 'Temperatur'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'wmfg') then Result := 'Betriebsdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'wmfh') then Result := 'Normdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'wmg') then Result := 'Umwerter 7'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wmga') then Result := 'Originalzählerstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wmgb') then Result := 'Betriebsvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wmgc') then Result := 'Normvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wmgd') then Result := 'Wärmemenge'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wmge') then Result := 'Druck'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wmgf') then Result := 'Temperatur'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'wmgg') then Result := 'Betriebsdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'wmgh') then Result := 'Normdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'h') then begin
        if (sValue = 'wmh') then Result := 'Umwerter 8'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wmha') then Result := 'Originalzählerstand'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wmhb') then Result := 'Betriebsvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wmhc') then Result := 'Normvolumen'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wmhd') then Result := 'Wärmemenge'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wmhe') then Result := 'Druck'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wmhf') then Result := 'Temperatur'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'g') then begin
          if (sValue = 'wmhg') then Result := 'Betriebsdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'h') then begin
          if (sValue = 'wmhh') then Result := 'Normdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'n') then begin
      if (sValue = 'wn') then Result := 'PGC-Momentanwerte'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'wna') then Result := 'PGC 1'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wnaa') then Result := 'Brennwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wnab') then Result := 'Normdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wnac') then Result := 'Dichteverhältnis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wnad') then Result := 'Kohlendioxid'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wnae') then Result := 'Stickstoff'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wnaf') then Result := 'Wasserstoff'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'wnb') then Result := 'PGC 2'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wnba') then Result := 'Brennwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wnbb') then Result := 'Normdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wnbc') then Result := 'Dichteverhältnis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wnbd') then Result := 'Kohlendioxid'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wnbe') then Result := 'Stickstoff'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wnbf') then Result := 'Wasserstoff'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'wnc') then Result := 'PGC 3'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wnca') then Result := 'Brennwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wncb') then Result := 'Normdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wncc') then Result := 'Dichteverhältnis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wncd') then Result := 'Kohlendioxid'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Eiheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wnce') then Result := 'Stickstoff'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wncf') then Result := 'Wasserstoff'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'wnd') then Result := 'PGC 4'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wnda') then Result := 'Brennwert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wndb') then Result := 'Normdichte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wndc') then Result := 'Dichteverhältnis'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'd') then begin
          if (sValue = 'wndd') then Result := 'Kohlendioxid'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'e') then begin
          if (sValue = 'wnde') then Result := 'Stickstoff'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Einheit'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'f') then begin
          if (sValue = 'wndf') then Result := 'Wasserstoff'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Wert'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Eiheit'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'o') then begin
      if (sValue = 'wo') then Result := 'MRG-Höchstwerte'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'woa') then Result := 'MRG-Höchstwert Summe'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'woaa') then Result := 'Höchstwertbildung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Impulskanäle 01-20'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Impulskanäle 21-40'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Impulskanäle 41-60'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'woab') then Result := 'Stundenhöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stundenhöchstwert/Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Stundenhöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Stundenhöchstwert/Jahr'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'woac') then Result := 'Tageshöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Tageshöchstwerte/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Tageshöchstwerte/Jahr'
        end
      end
    end
    else
    if (Length(sValue) >= 2) and (sValue[2] = 'p') then begin
      if (sValue = 'wp') then Result := 'Umwerter-Höchstwerte'
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'a') then begin
        if (sValue = 'wpa') then Result := 'Höchstwert Umwerter 1'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wpaa') then Result := 'Höchstwertbildung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zählerauswahl'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wpab') then Result := 'Stundenhöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stundenhöchstwert/Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Stundenhöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Stundenhöchstwert/Jahr'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wpac') then Result := 'Tageshöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Tageshöchstwerte/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Tageshöchstwerte/Jahr'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'b') then begin
        if (sValue = 'wpb') then Result := 'Höchstwert Umwerter 2'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wpba') then Result := 'Höchstwertbildung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zählerauswahl'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wpbb') then Result := 'Stundenhöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stundenhöchstwert/Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Stundenhöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Stundenhöchstwert/Jahr'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wpbc') then Result := 'Tageshöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Tageshöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Tageshöchstwert/Jahr'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'c') then begin
        if (sValue = 'wpc') then Result := 'Höchstwert Umwerter 3'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wpca') then Result := 'Höchstwertbildung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zählerauswahl'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wpcb') then Result := 'Stundenhöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stundenhöchstwert/Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Stundenhöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Stundenhöchstwert/Jahr'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wpcc') then Result := 'Tageshöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Tageshöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Tageshöchstwert/Jahr'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'd') then begin
        if (sValue = 'wpd') then Result := 'Höchstwert Umwerter 4'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wpda') then Result := 'Höchstwertbildung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zählerauwahl'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wpdb') then Result := 'Stundenhöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stundenhöchstwert/Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Stundenhöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Stundenhöchstwert/Jahr'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wpdc') then Result := 'Tageshöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Tageshöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Tageshöchstwert/Jahr'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'e') then begin
        if (sValue = 'wpe') then Result := 'Höchstwert Umwerter 5'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wpea') then Result := 'Höchstwertbildung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zählerauswahl'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wpeb') then Result := 'Stundenhöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stundenhöchstwert/Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Stundenhöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Stundenhöchstwert/Jahr'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wpec') then Result := 'Tageshöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Tageshöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Tageshöchstwert/Jahr'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'f') then begin
        if (sValue = 'wpf') then Result := 'Höchstwert Umwerter 6'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wpfa') then Result := 'Höchstwertbildung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zählerauswahl'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wpfb') then Result := 'Stundenhöchswerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stundenhöchstwert/Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Stundenhöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Stundenhöchstwert/Jahr'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wpfc') then Result := 'Tageshöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Tageshöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Tageshöchstwert/Jahr'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'g') then begin
        if (sValue = 'wpg') then Result := 'Höchstwert Umwerter 7'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wpga') then Result := 'Höchstwertbildung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zählerauswahl'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wpgb') then Result := 'Stundenhöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stundenhöchstwert/Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Stundenhöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Stundenhöchstwert/Jahr'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wpgc') then Result := 'Tageshöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Tageshöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Tageshöchstwert/Jahr'
        end
      end
      else
      if (Length(sValue) >= 3) and (sValue[3] = 'h') then begin
        if (sValue = 'wph') then Result := 'Höchstwert Umwerter 8'
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'a') then begin
          if (sValue = 'wpha') then Result := 'Höchstwertbildung'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Zählerauswahl'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'b') then begin
          if (sValue = 'wphb') then Result := 'Stundenhöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Stundenhöchstwert/Tag'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Stundenhöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'c') then Result := 'Stundenhöchstwert/Jahr'
        end
        else
        if (Length(sValue) >= 4) and (sValue[4] = 'c') then begin
          if (sValue = 'wphc') then Result := 'Tageshöchstwerte'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'a') then Result := 'Tageshöchstwert/Monat'
          else
          if (Length(sValue) = 5) and (sValue[5] = 'b') then Result := 'Tageshöchstwert/Jahr'
        end
      end
    end
    else Result := 'Datenelementadresse nicht gespeichert';
  end;
end;

{ Gibt Datenelementbezeichnung zurück}
{ Parameter: Datenelement-Adresse    }
{ Rückgabe: Datenelementbezeichnung  }
{------------------------------------}
function GetDsfgDeaBezeichnung(sValue: string): string;
{------------------------------------}
begin
  case sValue[1] of
    'a' : Result := GetDsfgDeaBezeichnungA(sValue);
    'b' : Result := GetDsfgDeaBezeichnungB(sValue);
    'c' : Result := GetDsfgDeaBezeichnungC(sValue);
    'd' : Result := GetDsfgDeaBezeichnungD(sValue);
    'f' : Result := GetDsfgDeaBezeichnungF(sValue);
    'w' : Result := GetDsfgDeaBezeichnungW(sValue);
    else Result := 'Datenelementadresse nicht gespeichert';
  end;
end;

end.
