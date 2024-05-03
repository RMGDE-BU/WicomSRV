{********************************************************************************}
{* Unit: Zugriff auf DSfG-Instanzwerte-Tabelle (gelesene Datenelemente)         *}
{* 07.12.1999 WW                                                                *}
{********************************************************************************}
unit TbDSfGIW;

interface

uses
  Forms, Classes, Db, DbTables, SysUtils, WTables;

const

 { DSfG-Instanzwerte-Tabelle }
  C_TbDInstWert       = 'InstWert';

  C_TfDInst_InstanzId  = 'InstanzId';      { int }
  C_TfDInst_DEL_Adr    = 'DEL_Adr';        { str10 }
  C_TfDInst_Wert       = 'Wert';           { str40 }    // 06.03.2002 WW, erweitert von 20 auf 40
  C_TfDInst_DatumZeit  = 'DatumZeit';      { datetime }
  C_TfDInst_Zeitzone   = 'Zeitzone';       { str1 }
  C_TfDInst_Zeitquelle = 'Zeitquelle';     { str10 }


type

  { Objekt für Zugriff auf Instanzwerte-Tabelle }

  TTbDInstanzwert = class(TObject)
  protected
    tbDInstanzwert: TTableExt;
  public
    constructor Create (APath: TFileName);
    destructor Destroy; override;
    procedure Loesche_Instanzwerte (InstanzId: integer);
  end;

implementation


{ TTbDInstanzwert }

{----------------------------------------------------}
constructor TTbDInstanzwert.Create (APath: TFileName);
{----------------------------------------------------}
begin
  inherited Create;
  tbDInstanzwert:=TTableExt.Create(nil);
  tbDInstanzwert.DataBaseName:=APath;
  tbDInstanzwert.TableName:=C_TbDInstWert;
end;

{---------------------------------}
destructor TTbDInstanzwert.Destroy;
{---------------------------------}
begin
  tbDInstanzwert.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------}
procedure TTbDInstanzwert.Loesche_Instanzwerte (InstanzId: integer);
{------------------------------------------------------------------}
{ löschen von Instanzwerten aus InstWert.db;
  Übergabe: InstanzId }
var
  Q: TQueryExt;
begin
  if tbDInstanzwert.Exists then begin
    Q:=TQueryExt.Create (nil);
    try
      Q.DataBaseName:=tbDInstanzwert.DataBaseName;
      with Q do begin
        Sql.Add ('DELETE FROM ' + C_TbDInstWert);
        Sql.Add ('WHERE ' + C_TfDInst_InstanzId + ' = ' + IntToStr (InstanzId));
        ExecSql;
      end;
    finally
      Q.Free;
    end;
  end;
end;

end.

