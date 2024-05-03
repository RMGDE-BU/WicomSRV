{------------------------------------------------------------------------------}
{ Export eines DBGrids in Exel mit vorgeschalteter Auswahl                     }
{                                                                              }
{ 11.05.2002  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Fa. Geert Dade - JustForFun-Software 2002                      }
{------------------------------------------------------------------------------}
unit FDbGrdExcelExport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, DbGrids, Db,
  FLineChbDlg, Com_Utils;

type
  TFormDbGridExportDialog = class(TFormLineChbDialog)
    bbtnHelp: TBitBtn;
    procedure bbtnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bbtnOkClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FDbGrid   : TDbGrid;
    FSlResult : TStrings;
    procedure DoExport;
  public
    { Public-Deklarationen }
    { Private-Deklarationen }
    function GetDbGrid(Sender: TWinControl): TDBGrid;
    procedure ExportDbGrid(pDbGrid: TDbGrid);
  end;

implementation

{$R *.DFM}

resourcestring
  S_HelpText = 'Auswahldialog'#13#10#13#10 +
    'Für jede Spalte des zu exportierenden'#13#10 +
    'Datengitters ist eine Zeile vorhanden.'#13#10#13#10 +
    'Die entsprechende Spalte wird nur exportiert, wenn'#13#10 +
    'das entsprechende Kontrollkästchen markiert ist.'#13#10#13#10 +
    'In das Editierfeld zu jeder Spalte kann'#13#10 +
    'ein Filter eingetragen werden:'#13#10 +
    'Ein "*" am Anfang bedeutet, dass die nachfolgende'#13#10 +
    'Zeichenkette an einer beliebigen Position stehen darf.'#13#10 +
    'Weiter Ersatzzeichen dürfen dann nicht verwendet werden.'#13#10 +
    'Ein "*" am Ende bedeutet, dass beliebige Zeichen '#13#10 +
    'noch folgen dürfen.'#13#10 +
    'In diesem Fall können beliebige Buchstaben mit einem "?"'#13#10 +
    'markiert werden.'#13#10 +
    'Es wird nicht zwischen Gross- und Kleinschreibung unterschieden.';

{----------------------------------------------}
procedure TFormDbGridExportDialog.FormCreate(Sender: TObject);
{----------------------------------------------}
begin
  inherited;

  FSlResult := TStringList.Create;
end;

{----------------------------------------------}
procedure TFormDbGridExportDialog.FormDestroy(Sender: TObject);
begin
  FSlResult.Free;

  inherited;
end;

{----------------------------------------------}
procedure TFormDbGridExportDialog.bbtnHelpClick(Sender: TObject);
{----------------------------------------------}
begin
  MessageDlg(S_HelpText, mtInformation, [mbOk], 0);
end;

{----------------------------------------------}
function TFormDbGridExportDialog.GetDbGrid(Sender: TWinControl): TDBGrid;
{----------------------------------------------}
var
  i : integer;
begin
  Result := nil;  // Default

  for i := 0 to Sender.ComponentCount-1 do
    if (Sender.Components[i] is TDbGrid) then begin
      Result := TDbGrid(Sender.Components[i]);
      Break;
    end;
end;

{----------------------------------------------}
procedure TFormDbGridExportDialog.ExportDbGrid(pDbGrid: TDbGrid);
{----------------------------------------------}
var
  i           : integer;
  pSlCaptions : TStrings;
begin
  if (not Assigned(pDbGrid)) then Exit;
  FDbGrid := pDbGrid;

  pSlCaptions := TStringList.Create;
  try
    // Überschriften erzeugen
    for i := 0 to pDbGrid.Columns.Count-1 do
      pSlCaptions.Add(pDbGrid.Columns[i].Title.Caption);

    // Auswahldialog erzeugen und anzeigen
    SetEingabeList(pSlCaptions, FSlResult);
  finally
    pSlCaptions.Free;
  end;
end;

{----------------------------------------------}
procedure TFormDbGridExportDialog.DoExport;
{----------------------------------------------}

  // Feststellen, ob der Text im Suchstring enthalten ist
  function IsToExport(sSearchString, sText: string): boolean;
  var
    i     : integer;
    s, s1 : string;
  begin
    s := Trim(sSearchString);
    if (s <> '') and (s <> '*') then begin
      if (s[1] = '*')
      then Result := (Pos(Copy(s, 2, Length(s)-1), sText) > 0)
      else begin
        if (s[Length(s)] = '*') then begin
          s := Copy(s, 1, Length(s)-1);
          s1 := Copy(sText, 1, Length(s));
        end
        else s1 := sText;

        for i := 1 to Length(s) do
          if (s[i] = '?') then
            if (Length(s1) >= i) then s1[i] := '?';

        Result := (AnsiCompareText(s, s1) = 0);
      end;
    end
    else Result := True;
  end;

var
  b                                 : boolean;
  s                                 : string;
  i                                 : integer;
  pSlExport : TStrings;
  pSavePlace                        : TBookmark;
begin
  if (not Assigned(FDbGrid)) then Exit;

  pSavePlace := FDbGrid.DataSource.DataSet.GetBookmark;
  FDbGrid.DataSource.DataSet.DisableControls;
  pSlExport := TStringList.Create;
  try
    // Exportliste entspechend der Auswahlliste zusammenstellen
    FDbGrid.DataSource.DataSet.First;
    while (not FDbGrid.DataSource.DataSet.Eof) do begin
      s := '';
      b := True;
      for i := 0 to FDbGrid.Columns.Count-1 do
        if (not IsToExport(ResultList[i], FDbGrid.Fields[i].asString)) then begin
          b := False;
          Break;
        end
        else if (IsChecked[i]) then s := s + FDbGrid.Fields[i].asString + #9;
      if (b) then pSlExport.Add(s);
      FDbGrid.DataSource.DataSet.Next;
    end;

    // Exportieren
    InsertToExcel(pSlExport, #9, 2, 2, True);

    // Wieder auf ursprünglichen Datensatz gehen
    FDbGrid.DataSource.DataSet.GotoBookmark(pSavePlace);
  finally
    pSlExport.Free;
    FDbGrid.DataSource.DataSet.FreeBookmark(pSavePlace);
    FDbGrid.DataSource.DataSet.EnableControls;
  end;
end;

{----------------------------------------------}
procedure TFormDbGridExportDialog.bbtnOkClick(Sender: TObject);
{----------------------------------------------}
begin
  inherited;

  DoExport;
end;

end.
