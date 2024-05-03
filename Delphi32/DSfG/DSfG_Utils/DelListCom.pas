{------------------------------------------------------------------------------}
{ Zugriff auf COM-Server für Zugriff auf DelList.db                            }
{                                                                              }
{ 14.06.2002  GD  Vs. 1.0  Neu                                                 }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2002                                          }
{------------------------------------------------------------------------------}
unit DelListCom;

interface

uses
  SysUtils, Classes, ComObj, Variants,
  DMomLists, GD_Utils;

function GetDeaBezeichnung(sDea: string; iGerType: byte): string;
function GetDeaDefRecordByDea(sDea: string; iGerType: byte): PDDEARec;
function GetDeaDefRecordByIndex(iIndex: integer): PDDEARec;
function GetDeaDefCount: integer;
function GetDeaTree(sDea1: string; iLevel: integer = 1): string;
function GetUserDefDeasList(
  sDea1: string = ''; bAktualize: boolean = False): string;
function GetDeviceCrossRefList(iGerTypNr: integer = 0; sRefCode: string = ''): string;
function GetGeraeteTypNr(iGerTypName: string): integer;
function GetGeraeteTypName(iGerTypNr: integer): string;
function GetErsatzwert(const sDea, sValue: string): string;
function GetErsatzwertList: TStrings;
function WriteDatenelemente(
  iInstId: integer; pList: TStrings; bDelete: boolean = True): boolean;
function ReadDatenelemente(
  iInstId: integer; sDeaVon: string; sDeaBis: string = ''): TStrings;
function HasDatenelemente(iInstId: integer): boolean;

implementation

const
  C_DeaListComServer = 'DelList.TDelList';

var
  FDelListObject : OleVariant;
  PDeaRec        : PDDEARec;
  FUserDefList   : TStrings;

{ Gibt Bezeichnung eines DE zurück        }
{ Parameter: DEA, Gerätetypnumer          }
{ Rückgabe: Bezeichnung oder ''           }
{-----------------------------------------}
function GetDeaBezeichnung(sDea: string; iGerType: byte): string;
{-----------------------------------------}
begin
  if (VarType(FDelListObject) = varEmpty) then
    FDelListObject := CreateOleObject(C_DeaListComServer);

  Result := FDelListObject.GetBezeichnung(sDea, iGerType);
end;

{ Gibt Definitionsrec. eines DE zurück    }
{ Parameter: DEA, Gerätetypnumer          }
{ Rückgabe: Definitions-Record            }
{-----------------------------------------}
function GetDeaDefRecordByDea(sDea: string; iGerType: byte): PDDEARec;
{-----------------------------------------}
var
  s : string;
begin
  if (VarType(FDelListObject) = varEmpty) then
    FDelListObject := CreateOleObject(C_DeaListComServer);

  FillChar(PDeaRec^, SizeOf(TDDEARec), 0);

  s := FDelListObject.GetRecordStringByDea(sDea, iGerType);
  // Struktur: DEA<us>Bezeichnung<us>Zugriff<us>Typ<us>GerTypNr 
  if (s <> '') then begin
    PDeaRec^.DEA := GetStringPart(s, 1);
    PDeaRec^.Bezeichnung := GetStringPart(s, 2);
    PDeaRec^.Zugriff := GetStringPart(s, 3);
    PDeaRec^.DEATyp := GetStringPart(s, 4);
    PDeaRec^.GTypNr := StrToIntDef(GetStringPart(s, 5), 0);
  end;

  Result := PDeaRec;
end;

{ Gibt Definitionsrec. eines DE zurück    }
{ Parameter: DEA, Gerätetypnumer          }
{ Rückgabe: Definitions-Record            }
{-----------------------------------------}
function GetDeaDefRecordByIndex(iIndex: integer): PDDEARec;
{-----------------------------------------}
var
  s : string;
begin
  if (VarType(FDelListObject) = varEmpty) then
    FDelListObject := CreateOleObject(C_DeaListComServer);

  FillChar(PDeaRec^, SizeOf(TDDEARec), 0);

  s := FDelListObject.GetRecordStringByIndex(iIndex);
  // Struktur: DEA<us>Bezeichnung<us>Zugriff<us>Typ<us>GerTypNr 
  if (s <> '') then begin
    PDeaRec^.DEA := GetStringPart(s, 1);
    PDeaRec^.Bezeichnung := GetStringPart(s, 2);
    PDeaRec^.Zugriff := GetStringPart(s, 3);
    PDeaRec^.DEATyp := GetStringPart(s, 4);
    PDeaRec^.GTypNr := StrToIntDef(GetStringPart(s, 5), 0);
  end;

  Result := PDeaRec;
end;

{ Gibt Definitionsrec. eines DE zurück    }
{ Parameter: DEA, Gerätetypnumer          }
{ Rückgabe: Definitions-Record            }
{-----------------------------------------}
function GetDeaDefCount: integer;
{-----------------------------------------}
begin
  if (VarType(FDelListObject) = varEmpty) then
    FDelListObject := CreateOleObject(C_DeaListComServer);

  Result := FDelListObject.Count;
end;

{ Gibt Liste mit DEA-Definitionen zurück  }
{ Parameter: erster Buchstabe der DEA,6,7 }
{ Rückgabe: string mit Listenstruktur     }
{-----------------------------------------}
function GetDeaTree(sDea1: string; iLevel: integer = 1): string;
{-----------------------------------------}
begin
  if (VarType(FDelListObject) = varEmpty) then
    FDelListObject := CreateOleObject(C_DeaListComServer);

  Result := FDelListObject.GetDeaTree(sDea1, iLevel);
end;

{ Gibt Userdef. DEA-Collections zurück    }
{ Parameter: Instanzbuchstaben            }
{ Rückgabe: Stringlist mit Struktur       }
{  InstTyp<tab>Name<tab>Dea<tab>DeaName   }
{-----------------------------------------}
function GetUserDefDeasList(
  sDea1: string = ''; bAktualize: boolean = False): string;
{-----------------------------------------}
var
  i : integer;
  p : TStrings;
begin
  Result := '';

  if (VarType(FDelListObject) = varEmpty) then
    FDelListObject := CreateOleObject(C_DeaListComServer);

  if (bAktualize) or (not Assigned(FUserDefList)) then begin
    if (not Assigned(FUserDefList)) then FUserDefList := TStringList.Create;
    FUserDefList.Text := FDelListObject.GetUserDefDeasList;
  end;

  p := TStringList.Create;
  try
    if (sDea1 = '')
    then p.Assign(FUserDefList)
    else for i := 0 to FUserDefList.Count-1 do
      if (Pos(FUserDefList[i][1], sDea1) > 0) then p.Add(FUserDefList[i]);

    Result := p.Text;
  finally
    p.Free;
  end;
end;

{ Gibt Userdef. DEA-Collections zurück    }
{ Parameter: Instanzbuchstaben            }
{ Rückgabe: Stringlist mit Struktur       }
{  GerName<us>Ref.<us>Dea<us>Name         }
{-----------------------------------------}
function GetDeviceCrossRefList(
  iGerTypNr: integer = 0; sRefCode: string = ''): string;
{-----------------------------------------}
begin
  if (VarType(FDelListObject) = varEmpty) then
    FDelListObject := CreateOleObject(C_DeaListComServer);

  Result := FDelListObject.GetDeviceCrossRefList(iGerTypNr, sRefCode);
end;

{ Gibt Userdef. DEA-Collections zurück    }
{ Parameter: Instanzbuchstaben            }
{ Rückgabe: Stringlist mit Struktur       }
{  GerName<us>Ref.<us>Dea<us>Name         }
{-----------------------------------------}
function GetGeraeteTypNr(iGerTypName: string): integer;
{-----------------------------------------}
begin
  if (VarType(FDelListObject) = varEmpty) then
    FDelListObject := CreateOleObject(C_DeaListComServer);

  Result := FDelListObject.GetGeraeteTypNr(iGerTypName);
end;

{ Gibt Userdef. DEA-Collections zurück    }
{ Parameter: Instanzbuchstaben            }
{ Rückgabe: Stringlist mit Struktur       }
{  GerName<us>Ref.<us>Dea<us>Name         }
{-----------------------------------------}
function GetGeraeteTypName(iGerTypNr: integer): string;
{-----------------------------------------}
begin
  if (VarType(FDelListObject) = varEmpty) then
    FDelListObject := CreateOleObject(C_DeaListComServer);

  Result := FDelListObject.GetGeraeteTypName(iGerTypNr);
end;

{ Gibt Ersatzwert für Datenelement zurück }
{ Parameter: Datenelement, Wert           }
{ Rückgabe: Ersatzwert                    }
{-----------------------------------------}
function GetErsatzwert(const sDea, sValue: string): string;
{-----------------------------------------}
begin
  Result := '';

  try
    if (VarType(FDelListObject) = varEmpty) then
      FDelListObject := CreateOleObject(C_DeaListComServer);

    Result := FDelListObject.GetErsatzwert(sDea, sValue);
  except
  end;
end;

{ Gibt Ersatzwertliste zurück             }
{ Rückgabe: Stringlist                    }
{-----------------------------------------}
function GetErsatzwertList: TStrings;
{-----------------------------------------}
begin
  Result := TStringList.Create;

  try
    if (VarType(FDelListObject) = varEmpty) then
      FDelListObject := CreateOleObject(C_DeaListComServer);

    Result.Text := FDelListObject.GetErsatzwertList;
  except
  end;
end;

{ Gibt Definitionen zu einer DEA zurück              }
{ Parameter: DEA, Gerätetyp-Nummer                   }
{ Rückgabe: Liste der Definitionen                   }
{----------------------------------------------------}
function GetDeaDefValues(sDea: string; iGerTyp: byte = 7): TStrings; register;
{----------------------------------------------------}
begin
  Result := TStringList.Create;

  try

    if (VarType(FDelListObject) = varEmpty) then
      FDelListObject := CreateOleObject(C_DeaListComServer);

    Result.Text := FDelListObject.GetDeaDefValues(sDea, iGerTyp);

  except
  end;
end;

{ Löscht einen anw.def. inst.spez. Eintrag           }
{ Parameter: InstanzTyp, Ordnerbezeichnung, DEA      }
{            Bei sDEA = '' wird der Ordner gelöscht  }
{ Rückgabe: Erfolg Ja/Nein (nur Exceptions)          }
{----------------------------------------------------}
function DeleteInstDefEntry(
  cInstTyp: string; sBezeichnung, sDea: string): boolean; register;
{----------------------------------------------------}
begin
  Result := False;

  try

    if (VarType(FDelListObject) = varEmpty) then
      FDelListObject := CreateOleObject(C_DeaListComServer);

    Result := FDelListObject.DeleteInstDefEntry(cInstTyp, sBezeichnung, sDEA);

  except
  // Result ist schon False
  end;
end;

{ Holt die stationsspez. DEA-Definitionen            }
{ Parameter: StationsId                              }
{ Rückgabe: Liste mit stationsspez. DEA-Def.         }
{----------------------------------------------------}
function GetDeaStaDefList(iStaId: integer): TStaDefDeaList; register;
{----------------------------------------------------}
var
  pSl      : TStrings;
  sBez, sList : string;
  i        : integer;
begin
  Result := TStaDefDeaList.Create;

  try

    if (VarType(FDelListObject) = varEmpty) then
      FDelListObject := CreateOleObject(C_DeaListComServer);

    with TStringList.Create do
    try

      CommaText := FDelListObject.GetDeaStaDefList(iStaId);
      for i := 0 to Count-1 do begin
        sList := Strings[i];
        sBez := GetStringPart(sList, 1);
        System.Delete(sList, 1, Length(sBez) + 1);
        pSl := TStringList.Create;
        pSl.Text := sList;
        Result.AddObject(sBez, pSl);
      end;

    finally
      Free;
    end;

  except
  end;
end;

{ Löscht einen anw.def. inst.spez. Eintrag           }
{ Parameter: InstanzTyp, Ordnerbezeichnung,          }
{            InstanzAdresse, DEA                     }
{ Rückgabe: Erfolg Ja/Nein (nur Exceptions)          }
{----------------------------------------------------}
function DeleteStaDefEntry(iStaId: integer; sBezeichnung: string;
  cInstAdr: string = '0'; sDEA: string = ''): boolean; register;
{----------------------------------------------------}
begin
  Result := False;

  try

    if (VarType(FDelListObject) = varEmpty) then
      FDelListObject := CreateOleObject(C_DeaListComServer);

    Result := FDelListObject.DeleteStaDefEntry(
      iStaId, sBezeichnung, cInstAdr, sDEA);

  except
  // Result ist schon False
  end;
end;

{ Trägt neue Liste mit Datenelementen ein            }
{ Parameter: StationsId, Bezeichnung, Liste          }
{ Rückgabe: Erfolg Ja/Nein                           }
{----------------------------------------------------}
function NewStaList(iStaId: integer; sBezeichnung: string;
  pList: TDSfGMomValueList): boolean; register;
{----------------------------------------------------}
var
  pSl : TStrings;
  i   : integer;
begin
  Result := False;

  try

    if (VarType(FDelListObject) = varEmpty) then
      FDelListObject := CreateOleObject(C_DeaListComServer);

    pSl := TStringList.Create;
    try
      for i := 0 to pList.Count-1 do
        pSl.Add(pList.InstanzAdresse[i] + Chr(us) + pList.DEAdresse[i] +
          Chr(us) + pList.Wert[i] + Chr(us) + IntToStr(pList.Stellen[i]));
      Result := FDelListObject.NewStaList(iStaId, sBezeichnung, pSl.Text);
    finally
      pSl.Free;
    end;

  except
  // Result ist schon False
  end;
end;

{ Trägt neue Liste mit Datenelementen ein            }
{ Parameter: InstanzTyp, Bezeichnung, Liste          }
{ Rückgabe: Erfolg Ja/Nein                           }
{----------------------------------------------------}
function NewDeaList(cInstTyp: string; sBezeichnung: string;
  pDeaList: TStrings): boolean; register;
{----------------------------------------------------}
var
  s : string;
begin
  Result := False;

  try

    if (VarType(FDelListObject) = varEmpty) then
      FDelListObject := CreateOleObject(C_DeaListComServer);

      s := cInstTyp;
      Result := FDelListObject.NewDeaList(s, sBezeichnung, pDeaList.Text);

  except
  // Result ist schon False
  end;
end;

{ Liste mit Datenelementen in DB eintragen}
{ Parameter: StaName, InstAdr, DE-Liste   }
{ Rüchgabe: Erfolg Ja/Nein                }
{-----------------------------------------}
function WriteDatenelemente(iInstId: integer; pList: TStrings;
  bDelete: boolean = True): boolean; register;
{-----------------------------------------}
begin
  Result := False;

  try
    if (VarType(FDelListObject) = varEmpty) then
      FDelListObject := CreateOleObject(C_DeaListComServer);
    Result := FDelListObject.WriteDatenelemente(iInstId, pList.Text, bDelete);
  except
  // Result ist schon False
  end;
end;

{ Sind Datenelemente für diese Station vorhanden ?   }
{ Parameter: Stationsname                            }
{ Rüchgabe: Datenelemente vorhanden ja/nein          }
{-----------------------------------------}
function HasDatenelemente(iInstId: integer): boolean; register;
{-----------------------------------------}
begin
  Result := False;

  try
    if (VarType(FDelListObject) = varEmpty) then
      FDelListObject := CreateOleObject(C_DeaListComServer);
    Result := FDelListObject.HasDatenelemente(iInstId);
  except
  // Result ist schon nil
  end;
end;

{ Liste mit Datenelementen einlesen       }
{ Parameter: InstId, DE-Liste             }
{ Rüchgabe: Erfolg Ja/Nein                }
{-----------------------------------------}
function ReadDatenelemente(
  iInstId: integer; sDeaVon: string; sDeaBis: string = ''): TStrings; register;
{-----------------------------------------}
begin
  Result := TStringList.Create;

  try
    if (VarType(FDelListObject) = varEmpty) then
      FDelListObject := CreateOleObject(C_DeaListComServer);
    Result.Text := FDelListObject.ReadDatenelemente(iInstId, sDeaVon, sDeaBis);
  except
  // Result ist schon nil
  end;
end;

{ Liste mit Stationen mit gespeicherten Parametern   }
{ Rüchgabe: Liste mit Stationen                      }
{----------------------------------------------------}
function GetStatWithParams: TStrings; register;
{----------------------------------------------------}
begin
  Result := TStringList.Create;

  try
    if (VarType(FDelListObject) = varEmpty) then
      FDelListObject := CreateOleObject(C_DeaListComServer);
    Result.Text := FDelListObject.GetStatWithParams;
  except
  // Liste ist leer
  end;
end;

exports
  GetDeaBezeichnung,
  GetDeaDefRecordByDea,
  GetDeaDefRecordByIndex,
  GetDeaDefCount,
  GetDeaTree,
  GetUserDefDeasList,
  GetDeviceCrossRefList,
  GetGeraeteTypNr,
  GetGeraeteTypName,
  GetErsatzwertList,
  GetErsatzwert,
  DeleteInstDefEntry,
  GetDeaStaDefList,
  DeleteStaDefEntry,
  NewStaList,
  NewDeaList,
  GetDeaDefValues,
  GetStatWithParams;

initialization
  New(PDeaRec);

finalization
  Dispose(PDeaRec);
  if (Assigned(FUserDefList)) then FreeAndNil(FUserDefList);

end.
