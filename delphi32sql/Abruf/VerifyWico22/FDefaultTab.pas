{------------------------------------------------------------------------------}
{ Basis-Tabulator-Fenster für Überwachung für WICO22                           }
{                                                                              }
{ 21.09.2007  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) RMG Messtechnik GmbH 2007                                      }
{------------------------------------------------------------------------------}
unit FDefaultTab;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls,
  GD_Utils, VerifyWico22Ini;

type
  TFormVerifyWico22Tab = class;
  TFormVerifyWico22TabClass = class of TFormVerifyWico22Tab;
  TMyExceptionHandling = procedure(E: Exception; const sText: string) of object;

  TFormVerifyWico22Tab = class(TForm)
    pnButtons: TPanel;
    pnClient: TPanel;
    pnModules: TPanel;
    gbExternalModules: TGroupBox;
    lvExternalModules: TListView;
    procedure lvExternalModulesResize(Sender: TObject); virtual;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    FFilePath             : TFileName;
    FLastError            : string;
    FInterval             : Cardinal;
    FModuleList           : TStrings;
    FDescList             : TStrings;
    FMyExceptionHandling  : TMyExceptionHandling;
    FSetGlobalModuleState : TNotifyBoolean;
    FJournalCallback      : TSetCaptionEvent;
    FDataTakeOver         : TCbBoolFunction;
  protected
    { Protected-Deklarationen }
    procedure InitComponents(bState: boolean); virtual;
    procedure MyExceptionHandling(E: Exception; const sText: string); virtual;
    procedure LoadExternalModuleList; virtual;
    procedure StartExternalModules; virtual;
    procedure SetFilePath(const sFilePath: TFileName);
    procedure WriteJournal(sText: string); overload; virtual; 
    property ModuleList: TStrings read FModuleList;
    property DescList: TStrings read FDescList;
  public
    { Public-Deklarationen }
    procedure Actualize; virtual;
    function Verify: boolean; virtual; abstract;
    property LastError: string read FLastError write FLastError;
    property FilePath: TFileName read FFilePath write SetFilePath;
    property ExceptionHandling: TMyExceptionHandling
      write FMyExceptionHandling;
    property SetGlobalModuleState: TNotifyBoolean
      read FSetGlobalModuleState write FSetGlobalModuleState;
    property JournalCallback: TSetCaptionEvent write FJournalCallback;
    property DataTakeOver: TCbBoolFunction
      read FDataTakeOver write FDataTakeOver;
  end;

implementation

uses TypInfo;

{$R *.dfm}

{----------------------------- TFormVerifyWico22Tab ---------------------------}

{--------------------------------------------}
procedure TFormVerifyWico22Tab.FormCreate(Sender: TObject);
{--------------------------------------------}
begin
  InitComponents(True);
end;

{--------------------------------------------}
procedure TFormVerifyWico22Tab.FormDestroy(Sender: TObject);
{--------------------------------------------}
begin
  InitComponents(False);
end;

{ Start von externen Programmmodulen         }
{--------------------------------------------}
procedure TFormVerifyWico22Tab.InitComponents(bState: boolean);
{--------------------------------------------}
begin
  if (bState) then begin
    FLastError := '';                   // Letzter interner Fehler
    FInterval := 60000;                 // Aktionsintervall: 60 sec.
    FFilePath := ExtractFilePath(ParamStr(0));  // Pfad zu Dateien
    FModuleList := TStringList.Create;  // Liste für externe Module und handles
    FDescList := TStringList.Create;    // Liste für Modulbeschreibungen
    FMyExceptionHandling := nil;
    FSetGlobalModuleState := nil;
    Actualize;
  end
  else begin
    FMyExceptionHandling := nil;
    FreeAndNil(FModuleList);
    FreeAndNil(FDescList);
  end;
end;

{ Internes Exception-Handling                }
{ Parameter: Exception, Hilfstext            }
{--------------------------------------------}
procedure TFormVerifyWico22Tab.MyExceptionHandling(
  E: Exception; const sText: string);
{--------------------------------------------}
begin
  FLastError := DateTimeToStr(Now) + ': ' + sText + ' - ' + E.Message;
  if (Assigned(FMyExceptionHandling)) then FMyExceptionHandling(E, sText);
end;

{ Übergeordnete Journalfunktion              }
{--------------------------------------------}
procedure TFormVerifyWico22Tab.WriteJournal(sText: string);
{--------------------------------------------}
begin
  if (Assigned(FJournalCallback)) then FJournalCallback(sText);
end;

{ Fenster aktualisieren                      }
{--------------------------------------------}
procedure TFormVerifyWico22Tab.SetFilePath(const sFilePath: TFileName);
{--------------------------------------------}
begin
  FFilePath := IncludeTrailingBackslash(sFilePath);
end;

{ Fenster aktualisieren                      }
{--------------------------------------------}
procedure TFormVerifyWico22Tab.Actualize;
{--------------------------------------------}
begin
  LoadExternalModuleList;
end;

{ Laden der Liste externer Programmmodulen   }
{--------------------------------------------}
procedure TFormVerifyWico22Tab.LoadExternalModuleList;
{--------------------------------------------}
var
  i, iIx : integer;
begin
  lvExternalModules.Items.BeginUpdate;
  try
    WriteJournal('Lade Modulliste');
    lvExternalModules.Clear;
    FModuleList.Clear;
    FDescList.Clear;
    with TStringList.Create do
    try
      CommaText := VerifyWicoIni.GetFormModuleList(Self);
      WriteJournal(Self.ClassName + ' - Modulliste: ' + CommaText);
      for i := 0 to Count-1 do begin
        // Dateiname vor dem Komma, Beschrebung danach
        iIx := FModuleList.Add(GetStringPart(Strings[i], 1, '='));
        // Beschreibung zu Dateiname, Prüfung auf gleiche Eintragsanzahl
        if (FDescList.Add(GetStringPart(Strings[i], 2, '=')) <> iIx) then
          raise Exception.Create('Unbelieveable: not matching listcount!');
        with lvExternalModules.Items.Add do begin
          Caption := GetStringPart(FDescList[iIx], 1, '|');
          SubItems.Add(GetStringPart(FModuleList[iIx], 1, '='));
        end;
      end;
    finally
      Free;
    end;
  finally
    lvExternalModules.Items.EndUpdate;
  end;
end;

{ Start von externen Programmmodulen         }
{--------------------------------------------}
procedure TFormVerifyWico22Tab.StartExternalModules;
{--------------------------------------------}
var
  i : integer;
  s : string;
begin
  try
    for i := 0 to FModuleList.Count-1 do begin
      if (FileExists(FModuleList[i])) then begin
        s := FModuleList[i];
        WinExec(PChar(s), SW_SHOW);
      end;
    end;
  except
    on E:Exception do MyExceptionHandling(E,
      'TFormVerifyWico22Tab.StartExternalModules');
  end;
end;

{--------------------------------------------}
procedure TFormVerifyWico22Tab.lvExternalModulesResize(Sender: TObject);
{--------------------------------------------}
var
  i, iWidth : integer;
begin
  try
    if (not (csDestroying in Self.ComponentState)) and
      (Assigned(lvExternalModules)) and (lvExternalModules.Columns.Count > 0) then
    begin
//      WriteJournal(Self.ClassName + ' - Größenänderung');
      iWidth := (lvExternalModules.Width - (lvExternalModules.Columns.Count+1) -
        GetSystemMetrics(SM_CXVSCROLL)) div lvExternalModules.Columns.Count;
      for i := 0 to lvExternalModules.Columns.Count-1 do
        lvExternalModules.Columns[i].Width := iWidth;
    end;
  except
    on E:Exception do MyExceptionHandling(E,
      'TFormVerifyWico22Tab.lvExternalModulesResize');
  end;
end;

end.
