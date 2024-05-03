{------------------------------------------------------------------------------}
{ Ini-Object f�r Informationen �ber WIESER - COM-Server                        }
{                                                                              }
{ 02.07.2002  GD  Neu                                                          }
{                                                                              }
{ (C) Karl Wieser GmbH 2002                                                    }
{------------------------------------------------------------------------------}
unit ComObjIni;

interface

uses
  IniFiles, SysUtils, Classes,
  PathIni;

const
  C_ComObjIniFileName = 'ComObj.INI';

  C_COM_DelList = 'DelList';
  C_COM_V24     = 'DV24Com';
  C_COM_DFU     = 'DDfuCom';
  C_COM_WICOM   = 'DWicomCom';

type
  TComObjectIni = class(TIniFile)
    constructor Create;
  private
  protected
    function GetComClassName(const sExeName: TFileName): string;
  public
    procedure GetComServerList(pSl: TStrings);
    property ComClassName [const s: TFileName]: string read GetComClassName;
  end;

implementation

const
  C_Section_ComServer  = 'COMSERVER';

{------------------------------------------}
constructor TComObjectIni.Create;
{------------------------------------------}
var
  s : TFileName;
begin
  with TProgramIni.Create do
  try
    s := ExtractFilePath(WieserIniFile) + C_ComObjIniFileName;
  finally
    Free;
  end;

  inherited Create(s);
end;

{ Gibt Namen aller COMSERVER-EXEs zur�ck   }
{ Parameter: �bergabe-Stringliste          }
{------------------------------------------}
procedure TComObjectIni.GetComServerList(pSl: TStrings);
{------------------------------------------}
var
  i : integer;
  s : TFileName;
begin
  ReadSection(C_Section_ComServer, pSl);

  for i := pSl.Count-1 downto 0 do begin
    s := ExtractFilePath(FileName) + ChangeFileExt(pSl[i], '.EXE');
    if (FileExists(s)) then pSl[i] := s else pSl.Delete(i);
  end;
end;

{ Gibt Klassenamen zu EXE zur�ck           }
{ Parameter: EXE-Dateiname                 }
{ R�ckgabe: Klassenname oder ''            }
{------------------------------------------}
function TComObjectIni.GetComClassName(const sExeName: TFileName): string;
{------------------------------------------}
var
  s : string;
begin
  s := ChangeFileExt((ExtractFileName(sExeName)), '');
  Result := ReadString(C_Section_ComServer, s, '');
end;

end.
