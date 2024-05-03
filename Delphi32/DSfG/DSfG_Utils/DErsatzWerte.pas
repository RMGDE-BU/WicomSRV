{------------------------------------------------------------------------------}
{ Ersatzwertbidung für DSfG-Abfragen                                           }
{                                                                              }
{ 25.09.2001  GD  Neu                                                          }
{ 04.02.2002  GD  DSta auf TStrings anstatt Commatext umgestellt               }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, 2002                                    }
{------------------------------------------------------------------------------}
unit DErsatzWerte;

interface

uses
  Classes, 
  DMomLists, DStaDll, GD_Utils;

type
  TErsatzwertList = class(TStringList)   // 05.09.2001
    constructor Create;
    destructor Destroy; override;
  private
  protected
  public
    procedure Clear; override;
    function GetErsatzwert(sDea, sValue: string): string; virtual;
  end;

implementation

type
  PStr40 = ^Str40;

{------------------------------ TErsatzwertList ------------------------------}

{------------------------------------}
constructor TErsatzwertList.Create;
{------------------------------------}
var
  i   : integer;
  p   : PStr40;
  pSl : TStrings;
begin
  inherited Create;

  pSl :=  ClientStammdaten.GetErsatzwertList;
  if (Assigned(pSl)) then
  try
    for i := 0 to pSl.Count-1 do Self.Add(pSl[i]);
  finally
    pSl.Free;
  end;

  for i := 0 to Count-1 do begin
    New(p);
    p^ := GetStringPart(Strings[i], 3);
    Strings[i] :=
      GetStringPart(Strings[i], 1) + Chr(us) + GetStringPart(Strings[i], 2);
    Objects[i] := TObject(p);
  end;
end;

{------------------------------------}
destructor TErsatzwertList.Destroy;
{------------------------------------}
begin
  Clear;

  inherited Destroy;
end;

{------------------------------------}
procedure TErsatzwertList.Clear;
{------------------------------------}
var
  i : integer;
begin
  for i := Count-1 downto 0 do
    if (Assigned(Objects[i])) then Dispose(PStr40(Objects[i]));

  inherited Clear;
end;

{------------------------------------}
function TErsatzwertList.GetErsatzwert(sDea, sValue: string): string;
{------------------------------------}
var
  i : integer;
begin
  Result := '';  // Default

  i := IndexOf(sDea + Chr(us) + sValue);
  if (i >= 0) then Result := PStr40(Objects[i])^;
end;

end.
 