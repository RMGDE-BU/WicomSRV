{------------------------------------------------------------------------------}
{ Komponente für Benutzerverwaltung                                            }
{                                                                              }
{ 06.01.2004  GD  Neu                                                          }
{ 17.01.2005  GD  Erweitert um "PasswordFunctionDisabled"                      }
{ 13.10.2005  GD  Erweitert um Rückgabe eines Password-Levels                  }
{ 13.12.2005  WW  ohne Property-Editoren bei Delphi 7, um Programme ohne Lauf- }
{                 zeitpackages ausführen zu können                             }
{ 21.01.2008  GD  Version wird überschrieben durch FileAge der Applikation     }
{ 19.08.2009  GD  Pathserver ohne eigene Session     			                     }
{ 27.01.2010  GD  Kein Anmeldefenster bei 'AdminPW = <leer>'                   }
{ 05.12.2011  GD  Auch keine Passwortmenu bei 'AdminPW = <leer>'               }
{ 27.07.2020  WW  Optische Korrektur der Schalter in InputPWQuery              }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2004, RMG Messtechnik GmbH 2020               }
{------------------------------------------------------------------------------}
unit Password;

interface               
                                                             
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, DbTables, Db, Math, ActnList, Menus,
  TypInfo, DbAutoInc, PathIni, WTables;

resourcestring
  S_EnablePassword = 'Passwortschutz aufheben';
  S_DisablePassword = 'Passwortschutz aktivieren';
  S_UserHasNoRights = 'Anwender %s hat für dieses Programm keine Rechte !';

const
  C_PwAdmin   = 'Administrator';
  C_Pw_Wieser = 'wieser';

type
  TByteSet         = set of Byte;

  TMyTabList = class(TStringList)
    destructor Destroy; override;
  private
    function GetCol(iIndex: integer): TStrings;
  protected
  public
    procedure Delete(iIndex: integer); override;
    procedure Clear; override;
    property Col [iIndex: integer]: TStrings read GetCol;
  end;

  TUserLevelList = class(TStringList)
  private
  protected
  public
    procedure AddRecord(sUserName, sPassword: string; iLevel: integer);
    function GetPassword(const sUserName: string): string;
    function GetUserLevels(const sUserName: string): TByteSet;
    function GetUserName(iIndex: integer): string;
  end;

  TMandantenList = class(TStringList)
  private
  protected
  public
    procedure AddRecord(sMandantName: string; iMandantIndex: word);
    function GetMandantKennungByName(const sMandantName: string): word;
    function GetMandantKennungByIndex(iIndex: integer): word;
    function GetMandantNameByMandantKennung(iKennung: word): string;
    function GetMandantNameByIndex(iIndex: integer): string;
    function HasMandantName(const sMandantName: string): boolean;
    function HasMandantKennung(iKennung: word): boolean;
  end;

  TDbBenutzer = class(TObject)
    constructor Create(pDatabase: TDatabase); virtual;
    destructor Destroy; override;
  private
    FDatabase      : TDatabase;
    FDbAutoInc     : TDbAutoInc;
    FUserLevelList : TUserLevelList;
    FMandantenList : TMandantenList;
    procedure CreateTables;
    function GetProgPwVersion(const sProgName: string): double;
  protected
  public
    function GetPwLevels(sUserName: string): TByteSet;
    function GetProgPwControls: TStrings;
    function SetProgPwControls(
      pCtrls: TStrings; sPrgName: string = ''): boolean;
    function GetProgPwLevels(sPrgName: string = ''): TStrings;
    function SetPrgPwLevels(
      fVersion: double; pLevels: TStrings; sPrgName: string = ''): boolean;
    function GetProgramList(
      pUsrLevelList: TUserLevelList = nil; sProgName: string = ''): boolean;
    function GetMandantenList(
      sUser: string = '';pMandatentenList: TMandantenList = nil): boolean;
    function GetCompletePwGrpList(
      pGrpLevelList, pUsrGrpList, pMdtList: TMyTabList): boolean;
    procedure SetCompletePwGrpList(
      pGrpLevelList, pUsrGrpList, pMdtList: TMyTabList);
    function GetUserList: TStrings;
    function GetPassword(const sUser: string): string;
    property UserList: TUserLevelList read FUserLevelList;
    property MandantenList: TMandantenList read FMandantenList;
    property ProgPwVersion [const sProgName: string]: double
      read GetProgPwVersion;
  end;

  TPwListObject = class(TMyTablist)
    constructor Create(pDatabase: TDatabase); virtual;
    destructor Destroy; override;
  private
    FRefList    : TStrings;
    FPwLevels   : TByteSet;
    FDbBenutzer : TDbBenutzer;
    FAktUser    : string;
    FLoadMandanten : boolean;
    FPasswordFunctionDisabled : boolean;
    procedure SetPwLevels(pLevels: TByteSet);
    function GetReferenzPwLevel(
      pForm: TForm; pComponent: TComponent; iPwLevel: byte): byte;
    function GetPwLevel(
      pForm: TForm; pComponent: TComponent): byte;
    function GetMandantenListe: TMandantenList;
    procedure SetPasswordFunctionDisabled(bState: boolean);
  protected
  public
    procedure InsertComponent(
      pForm: TForm; pComponent: TComponent; iPwLevel: byte);
    procedure DeleteComponent(pForm: TForm; pComponent: TComponent);
    procedure DeleteForm(pForm: TForm);
    procedure EnDisablePwControls;
    procedure EnableUser;
    procedure DisableUser;
    property PwLevels: TByteSet read FPwLevels write SetPwLevels;
    property DbBenutzer: TDbBenutzer read FDbBenutzer;
    property AktUser: string read FAktUser;
    property LoadMandanten: boolean write FLoadMandanten;
    property MandantenListe: TMandantenList read GetMandantenListe;
    property PasswordFunctionDisabled: boolean
      read FPasswordFunctionDisabled write SetPasswordFunctionDisabled;
    property PwLevel [pForm: TForm; pComponent: TComponent]: byte
      read GetPwLevel;
  end;

  TFormPasswordDlg = class(TForm)
    pnClient: TPanel;
    pnBottom: TPanel;
    pnBottomBtnOk: TPanel;
    bbtnOk: TBitBtn;
    Label4: TLabel;
    cbUsernames: TComboBox;
    Label2: TLabel;
    ePassword: TEdit;
    Panel1: TPanel;
    bbtnCancel: TBitBtn;
    procedure bbtnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
    FBenutzerObj : TDbBenutzer;
    procedure SetUserObject(pUserObj: TDbBenutzer);
    function CheckEingaben: boolean;
    function GetUsername: string;
    procedure SetUsername(sUsername: string);
    function GetPassword: string;
    function GetPwLevel: TByteSet;
  public
    { Public-Deklarationen }
    property UserObject: TDbBenutzer write SetUserObject;
    property UserName: string read GetUsername write SetUsername;
    property Password: string read GetPassword;
    property PwLevel: TByteSet read GetPwLevel;
  end;

  TPasswordButton = class(TPanel)
    constructor Create(pOwner: TComponent); override;
    destructor Destroy; override;
  private
    { Private-Deklarationen }
    FButton   : TSpeedButton;
    FAction   : TAction;
    FMenu     : TMenuItem;
    FDatabase : TDatabase;
    FDbName   : string;
    FOnPasswordExecute : TNotifyEvent;
    procedure aPasswordExecute(Sender: TObject);
    procedure InsertActionToActionList(Sender: TObject);
  protected
    { Protected-Deklarationen }
    procedure CreateDatabase; virtual;
    procedure SetCaption(const sCaption: string);
    function GetCaption: string;
  public
    { Public-Deklarationen }
    procedure SetBounds(iLeft, iTop, iWidth, iHeight: integer); override;
    procedure Execute;
  published
    { Published-Deklarationen }
    property Caption: string read GetCaption write SetCaption;
    property DatabaseName: string read FDbName write FDbName;
    property OnPasswordExecute: TNotifyEvent
      read FOnPasswordExecute write FOnPasswordExecute;
  end;

  procedure InsertPwData(fVersion: double; pSlCtrls, pSlLevels: TStrings);
  function InputPWQuery(
    const sCaption, sPrompt: string; var sValue: string): boolean;
  function PwListObject: TPwListObject;

const
  PasswordObject : TPwListObject = nil;  // Man. Alternative zu "PwListObject"

procedure Register;

implementation

{$R *.DFM}
{$R *.RES}

{$IFNDEF VER150}
uses
  DsgnIntf;

type
  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TAllDatabaseNamesProperty = class(TDBStringProperty)
  public
    procedure GetValueList(pSlList: TStrings); override;
  end;

{ TDBStringProperty }

function TDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBStringProperty.GetValueList(List: TStrings);
begin
end;

procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{ TAllDatabasesNameProperty }

procedure TAllDatabaseNamesProperty.GetValueList(pSlList: TStrings);
var
  pSl  : TStrings;
  i, j : integer;
begin
  pSlList.Clear;

  pSl := TStringList.Create;
  try
    for i := 0 to Sessions.Count-1 do begin
      Sessions.Sessions[i].GetDatabaseNames(pSl);
      for j := 0 to pSl.Count-1 do pSlList.Add(pSl[j]);
    end;
  finally
    pSl.Free;
  end;
end;
{$ENDIF}

resourcestring
  S_UnknownUser = 'Unbekannter Anwender "%s"';
  S_DifPWs      = 'Falsches Passwort !';

const
  C_Res_Open = 'LOCK_OPEN';
  C_Res_Shut = 'LOCK_SHUT';

  C_MinPwLevel = 1;   // 1. Password-Level
  C_MaxPwLevel = 16;  // Letzter Password-Level

  { Tabelle der Programm-Beschreibungen (Index, Name, Beschreibung) }
  C_Tb_PwProgDefs            = 'pwprogdefs';

  C_Tf_PwProgDefs_ProgIndex  = 'progindex';      { integer }
  C_Tf_PwProgDefs_ProgName   = 'progname';       { str20 }
  C_Tf_PwProgDefs_ProgDesc   = 'progdesc';       { str50 }
  C_Tf_PwProgDefs_ProgVersion = 'progversion';   { float }

  C_Ti_PwProgDefs_Name       = 'ixname';         // Index auf ProgName

  { Tabelle der Programm-Level (PrgIndex, LevelIndex, Beschreibung) }
  C_Tb_PwProgLevels          = 'pwproglevels';

  C_Tf_PwProgLevels_ProgIndex  = 'progindex';    { integer }
  C_Tf_PwProgLevels_LevelIndex = 'levelindex';   { integer }
  C_Tf_PwProgLevels_LevelName  = 'levelname';    { str40 }

  { Tabelle der Funktions-Level (Index, Programm, Control, Level) }
  C_Tb_PwFktnLevel           = 'pwfktnlevel';

  C_Tf_PwFktnLevel_FktnIndex = 'fktnindex';      { integer }
  C_Tf_PwFktnLevel_ProgIndex = 'progindex';      { integer } // C_Tb_PwProgDefs
  C_Tf_PwFktnLevel_FormName  = 'formname';       { str30 }
  C_Tf_PwFktnLevel_CtrlName  = 'ctrlname';       { str30 }
  C_Tf_PwFktnLevel_FktnDesc  = 'fktndesc';       { str80 }
  C_Tf_PwFktnLevel_FktnLevel = 'fktnlevel';      { smallint } // 1..16

  { Tabelle der Gruppen-Beschreibungen (Index, Name, Beschreibung) }
  C_Tb_PwGrpDefs             = 'pwgrpdefs';

  C_Tf_PwGrpDefs_GrpIndex    = 'grpindex';       { integer }
  C_Tf_PwGrpDefs_GrpName     = 'grpname';        { str40 }
  C_Tf_PwGrpDefs_GrpDesc     = 'grpdesc';        { str120 }

  C_Ti_PwGrpDefs_Name        = 'ixname';         // Index auf GrpName

  { Tabelle der Benutzer-Beschreibungen (Index, Name) }
  C_Tb_PwUserDefs            = 'pwuserdefs';

  C_Tf_PwUserDefs_UserIndex  = 'userindex';      { integer }
  C_Tf_PwUserDefs_UserName   = 'username';       { str30 }
  C_Tf_PwUserDefs_UserDesc   = 'userdesc';       { str120 }
  C_Tf_PwUserDefs_Password   = 'mypassword';       { str30 }

  C_Ti_PwUserDefs_Name       = 'ixname';         // Index auf GrpName

  { Tabelle der Zuordnungnen User - Gruppen }
  C_Tb_PwUserInGrp           = 'pwuseringrp';

  C_Tf_PwUserInGrp_UserIndex  = 'userindex';     { integer } // C_Tb_PwUserDefs
  C_Tf_PwUserInGrp_GrpIndex   = 'grpindex';      { integer } // C_Tb_PwGrpDefs

  { Tabelle der Zugriffslevel der Gruppen bez. auf Programm (bin. codiert) }
  C_Tb_PwGrpLevel            = 'pwgrplevel';

  C_Tf_PwGrpLevel_GrpIndex   = 'grpindex';       { integer } // C_Tb_PwGrpDefs
  C_Tf_PwGrpLevel_ProgIndex  = 'progindex';      { integer } // C_Tb_PwProgDefs
  C_Tf_PwGrpLevel_LevelSet   = 'levelset';       { integer } // 2E1..16 - binär

  { Tabelle der anwenderbezogenen Mandanten }
  C_Tb_PwMandanten           = 'pwmandanten';

  C_Tf_PwMandanten_MdtIndex  = 'mdtindex';       { integer }
  C_Tf_PwMandanten_UserIndex = 'userindex';      { integer }
  C_Tf_PwMandanten_MdtName   = 'mdtname';        { str80 }
  C_Tf_PwMandanten_MdtKennung= 'mdtkennung';     { integer }

const
  FPwListObject   : TPwListObject = nil;
  FPasswordButton : TPasswordButton = nil;

{---------------------------------------------}
function PwListObject: TPwListObject;
{---------------------------------------------}
begin
  Result := nil;  // Default

  if (Assigned(FPasswordButton)) then FPasswordButton.CreateDatabase;
//  else raise Exception.Create(
//    'Please define a "TPasswordButton"-object first !');

  if (Assigned(FPwListObject)) then Result := FPwListObject;
//  else raise Exception.Create('"PwListObject" is not available !'#13#10 +
//    'Please check the databaseconnection.');
end;

{ Input dialog }

{ InputQuery mit maskierter Eingabe           }
{ Parameter: Überschrift, Eingabeprompt, Def. }
{ Rückgabe: Eingabe wurde bestätigt ja/nein   }
{---------------------------------------------}
function InputPWQuery(
  const sCaption, sPrompt: string; var sValue: string): boolean;
{---------------------------------------------}

  function GetAveCharSize(Canvas: TCanvas): TPoint;
  var
    I: Integer;
    Buffer: array[0..51] of Char;
  begin
    for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
    for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
    GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
    Result.X := Result.X div 52;
  end;

var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := sCaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      ClientHeight := MulDiv(63, DialogUnits.Y, 8);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        AutoSize := True;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Caption := sPrompt;
      end;
      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        PasswordChar := '*';
        Top := MulDiv(19, DialogUnits.Y, 8);
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := sValue;
        SelectAll;
      end;
      ButtonTop := MulDiv(41, DialogUnits.Y, 8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8) + 2;  // Korrektur für BitBtn; 27.07.2020, WW
      with TBitBtn.Create(Form) do
      begin
        Parent := Form;
        Kind := bkOK;
        SetBounds(MulDiv(38 - 16, DialogUnits.X, 4), ButtonTop, ButtonWidth + 16,
          ButtonHeight);  // Korrektur Left, Width für BitBtn; 27.07.2020, WW
      end;
      with TBitBtn.Create(Form) do
      begin
        Parent := Form;
        Kind := bkCancel;
        SetBounds(MulDiv(92 + 8, DialogUnits.X, 4), ButtonTop, ButtonWidth + 16,
          ButtonHeight);  // Korrektur Left, Width für BitBtn; 27.07.2020, WW
      end;
      if ShowModal = mrOk then
      begin
        sValue := Edit.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

{ Update von Pw-Informationen                 }
{ Parameter: Akt. PrgVersion (nicht mehr verwendet) }
{            Listen für Controls und PwLevels }
{---------------------------------------------}
procedure InsertPwData(fVersion: double; pSlCtrls, pSlLevels: TStrings);
{---------------------------------------------}
var
  pDatabase : TDatabase;
begin
  // Version wird überschrieben durch FileAge der Applikation  // 21.01.2008
  fVersion := FileAge(ParamStr(0));

  with TProgramIni.Create(True, False) do  // NICHT userspez. // 21.01.2008
  try
    // Lokal prüfen, ob DIESER Client bereits upgedated hat
    if (fVersion <> ReadFloat('SETTINGS', 'PWVERSION', 0)) then begin

      with TPathServer.Create(WieserIniFile, [WStammDb], False, False) do
      try
        Check;
        pDatabase := GetDatabase(WStammDb);
      finally
        Free;
      end;

      with TDbBenutzer.Create(pDatabase) do
      try
        // Datenbank prüfen, ob Version bereits upgedated wurde
        if (GetProgPwVersion(ParamStr(0)) < fVersion) then begin
          SetProgPwControls(pSlCtrls);
          SetPrgPwLevels(fVersion, pSlLevels);
        end;
      finally
        Free;
        pDatabase.Free;
      end;

      WriteFloat('SETTINGS', 'PWVERSION', fVersion);
    end;

  finally
    Free;
  end;
end;

{ Gibt Element aus String mit <US> zurück    }
{ Parameter: String, Index des Elementes     }
{ Rückgabe: Element oder ''                  }
{--------------------------------------------}
function GetStringPart(
  sString: string; iIndex: integer; cSeparator: char = #31): string;
{--------------------------------------------}
var
  iPos : integer;
  iLoop : integer;
begin
  Result := '';
  for iLoop := 1 to iIndex-1 do begin
    iPos := Pos(cSeparator, sString);
    if (iPos = 0) then Exit
    else Delete(sString, 1, iPos);
  end;
  iPos := Pos(cSeparator, sString);
  if (iPos > 0) then Result := Copy(sString, 1, iPos-1)
  else if (Length(sString) > 0) then Result := sString;
end;

{----------------------------------- TMyTabList -------------------------------}

{----------------------------------------------------}
destructor TMyTabList.Destroy;
{----------------------------------------------------}
begin
  Clear;

  inherited;
end;

{----------------------------------------------------}
procedure TMyTabList.Delete(iIndex: integer);
{----------------------------------------------------}
begin
  if (Assigned(Objects[iIndex])) then Objects[iIndex].Free;

  inherited;
end;

{----------------------------------------------------}
procedure TMyTabList.Clear;
{----------------------------------------------------}
var
  i : integer;
begin
  for i := 0 to Count-1 do if (Assigned(Objects[i])) then Objects[i].Free;

  inherited;
end;

{----------------------------------------------------}
function TMyTabList.GetCol(iIndex: integer): TStrings;
{----------------------------------------------------}
begin
  if (Assigned(Objects[iIndex]))
  then Result := TStrings(Objects[iIndex])
  else Result := nil;
end;

{--------------------------------- TUserLevelList --------------------------------}

{---------------------------------------------}
procedure TUserLevelList.AddRecord(sUserName, sPassword: string; iLevel: integer);
{---------------------------------------------}
var
  iIndex : integer;
begin
  if (sUserName <> '') and (iLevel > 0) then begin
    iIndex := IndexOf(sUserName + '|' + sPassword);
    if (iIndex < 0)
    then AddObject(sUserName + '|' + sPassword, TObject(iLevel))
    else Objects[iIndex] := TObject(iLevel or Integer(Objects[iIndex]));
  end;
end;

{---------------------------------------------}
function TUserLevelList.GetPassword(const sUserName: string): string;
{---------------------------------------------}
var
  i, iIndex : integer;
begin
  iIndex := -1;
  for i := 0 to Count-1 do
    if (LowerCase(GetStringPart(Strings[i], 1, '|')) = LowerCase(sUserName))
    then begin
      iIndex := i;
      Break;
    end;

  if (iIndex >= 0)
  then Result := GetStringPart(Strings[iIndex], 2, '|')
  else raise Exception.Create(Format (S_UnknownUser, [sUserName]));
end;

{---------------------------------------------}
function TUserLevelList.GetUserLevels(const sUserName: string): TByteSet;
{---------------------------------------------}
var
  i, iIndex, iLevel : integer;
begin
  Result := [];
  iIndex := -1;
  for i := 0 to Count-1 do
    if (LowerCase(GetStringPart(Strings[i], 1, '|')) = LowerCase(sUserName))
    then begin
      iIndex := i;
      Break;
    end;

  if (iIndex >= 0) then begin
    iLevel := Integer(Objects[iIndex]);
    for i := C_MinPwLevel to C_MaxPwLevel do begin
      if ((iLevel and (Trunc(IntPower(2, i-1)))) > 0) then Include(Result, i);
    end;
  end
  else raise Exception.Create(Format (S_UnknownUser, [sUserName]));     
end;

{---------------------------------------------}
function TUserLevelList.GetUserName(iIndex: integer): string;
{---------------------------------------------}
begin
  Result := GetStringPart(Strings[iIndex], 1, '|');
end;

{-------------------------------- TMandantenList ------------------------------}

{---------------------------------------------}
procedure TMandantenList.AddRecord(sMandantName: string; iMandantIndex: word);
{---------------------------------------------}
var
  iIndex : integer;
begin
  iIndex := IndexOf(sMandantName);
  if (iIndex < 0)
  then AddObject(sMandantName, TObject(iMandantIndex))
  else Objects[iIndex] := TObject(iMandantIndex);
end;

{---------------------------------------------}
function TMandantenList.GetMandantKennungByName(
  const sMandantName: string): word;
{---------------------------------------------}
var
  iIndex : integer;
begin
  iIndex := IndexOf(sMandantName);
  if (iIndex >= 0) and (Assigned(Objects[iIndex]))
  then Result := Integer(Objects[iIndex])
  else Result := 0;
end;

{---------------------------------------------}
function TMandantenList.GetMandantKennungByIndex(iIndex: integer): word;
{---------------------------------------------}
begin
  if (Assigned(Objects[iIndex]))
  then Result := Integer(Objects[iIndex])
  else Result := 0;
end;

{---------------------------------------------}
function TMandantenList.GetMandantNameByMandantKennung(iKennung: word): string;
{---------------------------------------------}
var
  i : integer;
begin
  i := IndexOfObject(TObject(iKennung));
  if (i >= 0) then Result := Strings[i] else Result := '';
end;

{---------------------------------------------}
function TMandantenList.GetMandantNameByIndex(iIndex: integer): string;
{---------------------------------------------}
begin
  Result := Strings[iIndex];
end;

{---------------------------------------------}
function TMandantenList.HasMandantName(const sMandantName: string): boolean;
{---------------------------------------------}
begin
  Result := (IndexOf(sMandantName) >= 0);
end;

{---------------------------------------------}
function TMandantenList.HasMandantKennung(iKennung: word): boolean;
{---------------------------------------------}
begin
  Result := (IndexOfObject(TObject(iKennung)) >= 0);
end;

{--------------------------------- TDbBenutzer --------------------------------}

{---------------------------------------------}
constructor TDbBenutzer.Create(pDatabase: TDatabase);
{---------------------------------------------}
begin
  inherited Create;

  FDatabase := pDatabase;
  FDbAutoInc := TDbAutoInc.Create(FDatabase);
  CreateTables;
  FUserLevelList := TUserLevelList.Create;
  FMandantenList := TMandantenList.Create;
end;

{---------------------------------------------}
destructor TDbBenutzer.Destroy;
{---------------------------------------------}
begin
  FDbAutoInc.Free;
  FUserLevelList.Free;
  FMandantenList.Free;

  inherited Destroy;
end;

{---------------------------------------------}
procedure TDbBenutzer.CreateTables;
{---------------------------------------------}
begin
  with TTableExt.Create(nil) do
  try
    DatabaseName := FDatabase.DatabaseName;
    SessionName := FDatabase.SessionName;

  { Tabelle der Programm-Beschreibungen (Index, Name, Beschreibung) }
    TableName := C_Tb_PwProgDefs;
    if (not Exists) then begin
      with FieldDefs do begin
        Add(C_Tf_PwProgDefs_ProgIndex, ftInteger, 0, True);
        Add(C_Tf_PwProgDefs_ProgName, ftString, 20, True);
        Add(C_Tf_PwProgDefs_ProgDesc, ftString, 50, False);
        Add(C_Tf_PwProgDefs_ProgVersion, ftFloat, 0, False);
      end;
      with IndexDefs do begin
        Add('ixmain', C_Tf_PwProgDefs_ProgIndex, [ixPrimary, ixUnique]);
        Add(C_Ti_PwProgDefs_Name, C_Tf_PwProgDefs_ProgName, [ixCaseInSensitive]);
      end;
      CreateTable;
      FieldDefs.Clear;
      IndexDefs.Clear;
    end;

    { Tabelle der Programm-Level (PrgIndex, LevelIndex, Beschreibung) }
    TableName := C_Tb_PwProgLevels;
    if (not Exists) then begin
      with FieldDefs do begin
        Add(C_Tf_PwProgLevels_ProgIndex, ftInteger, 0, True);
        Add(C_Tf_PwProgLevels_LevelIndex, ftInteger, 0, True);
        Add(C_Tf_PwProgLevels_LevelName, ftString, 40, False);
      end;
      with IndexDefs do begin
        Add('ixmain', C_Tf_PwProgLevels_ProgIndex + ';' +
          C_Tf_PwProgLevels_LevelIndex, [ixPrimary, ixUnique]);
      end;
      CreateTable;
      FieldDefs.Clear;
      IndexDefs.Clear;
    end;

    { Tabelle der Funktions-Level (Index, Programm, Control, Level) }
    TableName := C_Tb_PwFktnLevel;
    if (not Exists) then begin
      with FieldDefs do begin
        Add(C_Tf_PwFktnLevel_FktnIndex, ftInteger, 0, True);
        Add(C_Tf_PwFktnLevel_ProgIndex, ftInteger, 0, True);
        Add(C_Tf_PwFktnLevel_FormName, ftString, 30, True);
        Add(C_Tf_PwFktnLevel_CtrlName, ftString, 30, True);
        Add(C_Tf_PwFktnLevel_FktnDesc, ftString, 80, False);
        Add(C_Tf_PwFktnLevel_FktnLevel, ftSmallint, 0, True);
      end;
      with IndexDefs do begin
        Add('ixmain', C_Tf_PwFktnLevel_FktnIndex, [ixPrimary, ixUnique]);
      end;
      CreateTable;
      FieldDefs.Clear;
      IndexDefs.Clear;
    end;

    { Tabelle der Gruppen-Beschreibungen (Index, Name, Beschreibung) }
    TableName := C_Tb_PwGrpDefs;
    if (not Exists) then begin
      with FieldDefs do begin
        Add(C_Tf_PwGrpDefs_GrpIndex, ftInteger, 0, True);
        Add(C_Tf_PwGrpDefs_GrpName, ftString, 40, True);
        Add(C_Tf_PwGrpDefs_GrpDesc, ftString, 120, False);
      end;
      with IndexDefs do begin
        Add('ixmain', C_Tf_PwGrpDefs_GrpIndex, [ixPrimary, ixUnique]);
        Add(C_Ti_PwGrpDefs_Name, C_Tf_PwGrpDefs_GrpName, [ixCaseInSensitive]);
      end;
      CreateTable;
      OpenExclusive;
      AppendRecord([1, C_PwAdmin, '']);
      FDbAutoInc.NextIndex[C_Tb_PwGrpDefs] := 2;
      Close;
      FieldDefs.Clear;
      IndexDefs.Clear;
    end;

    { Tabelle der Benutzer-Beschreibungen (Index, Name) }
    TableName := C_Tb_PwUserDefs;
    if (not Exists) then begin
      with FieldDefs do begin
        Add(C_Tf_PwUserDefs_UserIndex, ftInteger, 0, True);
        Add(C_Tf_PwUserDefs_UserName, ftString, 30, True);
        Add(C_Tf_PwUserDefs_UserDesc, ftString, 120, False);
        Add(C_Tf_PwUserDefs_Password, ftString, 30, False);
      end;
      with IndexDefs do begin
        Add('ixmain', C_Tf_PwUserDefs_UserIndex, [ixPrimary, ixUnique]);
        Add(C_Ti_PwUserDefs_Name, C_Tf_PwUserDefs_UserName, [ixCaseInSensitive]);
      end;
      CreateTable;
      OpenExclusive;
      AppendRecord([1, C_PwAdmin, C_Pw_Wieser]);
      FDbAutoInc.NextIndex[C_Tb_PwUserDefs] := 2;
      Close;
      FieldDefs.Clear;
      IndexDefs.Clear;
    end;

    { Tabelle der Zuordnungnen User - Gruppen }
    TableName := C_Tb_PwUserInGrp;
    if (not Exists) then begin
      with FieldDefs do begin
        Add(C_Tf_PwUserInGrp_UserIndex, ftInteger, 0, True);
        Add(C_Tf_PwUserInGrp_GrpIndex, ftInteger, 0, True);
      end;
      with IndexDefs do begin
        Add('ixmain', C_Tf_PwUserInGrp_UserIndex + ';' +
          C_Tf_PwUserInGrp_GrpIndex, [ixPrimary, ixUnique]);
      end;
      CreateTable;
      OpenExclusive;
      AppendRecord([1, 1]);  // Weist der Gruppe "Admin" den User "Admin" zu
      Close;
      FieldDefs.Clear;
      IndexDefs.Clear;
    end;

    { Tabelle der Zugriffslevel der Gruppen bez. auf Programm (bin. codiert) }
    TableName := C_Tb_PwGrpLevel;
    if (not Exists) then begin
      with FieldDefs do begin
        Add(C_Tf_PwGrpLevel_GrpIndex, ftInteger, 0, True);
        Add(C_Tf_PwGrpLevel_ProgIndex, ftInteger, 0, True);
        Add(C_Tf_PwGrpLevel_LevelSet, ftInteger, 0, True);
      end;
      with IndexDefs do begin
        Add('ixmain', C_Tf_PwGrpLevel_GrpIndex + ';' +
          C_Tf_PwGrpLevel_ProgIndex, [ixPrimary, ixUnique]);
      end;
      CreateTable;
      FieldDefs.Clear;
      IndexDefs.Clear;
    end;

    { Tabelle der anwenderbezogenen Mandanten }
    TableName := C_Tb_PwMandanten;
    if (not Exists) then begin
      with FieldDefs do begin
        Add(C_Tf_PwMandanten_MdtIndex, ftInteger, 0, True);
        Add(C_Tf_PwMandanten_UserIndex, ftInteger, 0, True);
        Add(C_Tf_PwMandanten_MdtName, ftString, 30, False);
        Add(C_Tf_PwMandanten_MdtKennung, ftInteger, 0, False);
      end;
      with IndexDefs do begin
        Add('ixmain', C_Tf_PwMandanten_MdtIndex, [ixPrimary, ixUnique]);
      end;
      CreateTable;
      FieldDefs.Clear;
      IndexDefs.Clear;
    end;
  finally
    Free;
  end;
end;

{ Gibt die Pw-Version eines Programms zurück  }
{ Parameter: Programname                      }
{ Rückgabe: Pw-Version oder 0                 }
{---------------------------------------------}
function TDbBenutzer.GetProgPwVersion(const sProgName: string): double;
{---------------------------------------------}
begin
  Result := 0;
  with TQueryExt.Create(nil) do
  try
    DatabaseName := FDatabase.DatabaseName;
    SessionName := FDatabase.SessionName;

    Sql.Add('SELECT ' + C_Tf_PwProgDefs_ProgVersion);
    Sql.Add('FROM ' + C_Tb_PwProgDefs);
    Sql.Add('WHERE LOWER(' + C_Tf_PwProgDefs_ProgName + ') = ''' +
      LowerCase(ExtractFileName(ChangeFileExt(sProgName, ''))) + '''');
    if (Open) then begin
      if (not Eof) then Result := Fields[0].asFloat;
      Close;
    end;
  finally
    Free;
  end;
end;

{ Gibt die PwLevels für einen User zurück     }
{ Parameter: Username                         }
{ Rückgabe: Levels als ByteSet                }
{---------------------------------------------}
function TDbBenutzer.GetPwLevels(sUserName: string): TByteSet;
{---------------------------------------------}
begin
  Result := FUserLevelList.GetUserLevels(sUserName);
end;

{ Gibt die Controls für aktuelles Prg. zurück }
{ Rückgabe: Liste mit Ctrls und Levels        }
{---------------------------------------------}
function TDbBenutzer.GetProgPwControls: TStrings;
{---------------------------------------------}
begin
  Result := TStringList.Create;

  with TQueryExt.Create(nil) do
  try
    DatabaseName := FDatabase.DatabaseName;
    SessionName := FDatabase.SessionName;

    Sql.Add('SELECT A.' + C_Tf_PwFktnLevel_CtrlName + ',');
    Sql.Add('       A.' + C_Tf_PwFktnLevel_FormName + ',');
    Sql.Add('       A.' + C_Tf_PwFktnLevel_FktnLevel);
    Sql.Add('FROM ' + C_Tb_PwFktnLevel + ' A, ' + C_Tb_PwProgDefs + ' B');
    Sql.Add('WHERE LOWER(B.' + C_Tf_PwProgDefs_ProgName + ') = ''' +
      LowerCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + '''');
    Sql.Add('AND A.' + C_Tf_PwFktnLevel_ProgIndex + ' = B.' +
      C_Tf_PwProgDefs_ProgIndex);
    if (Open) then begin
      while (not Eof) do begin
        Result.AddObject(
          LowerCase(FieldByName(C_Tf_PwFktnLevel_FormName).asString) + '|' +
          LowerCase(FieldByName(C_Tf_PwFktnLevel_CtrlName).asString),
          TObject(FieldByName(C_Tf_PwFktnLevel_FktnLevel).asInteger));
        Next;
      end;
      Close;
    end;
  finally
    Free;
  end;
end;

{ Übergibt die Controls für (aktuelles) Prg.  }
{ Parameter: Liste m. Ctrls und Levels, PName }
{ Rückgabe: Erfolg ja/nein                    }
{---------------------------------------------}
function TDbBenutzer.SetProgPwControls(
  pCtrls: TStrings; sPrgName: string = ''): boolean;
{---------------------------------------------}
var
  bTransact           : boolean;
  i, iPrgIx, iLevel   : integer;
  sForm, sCtrl, sDesc : string;
begin
  Result := False;

  if (sPrgName = '') then
    sPrgName := ChangeFileExt(ExtractFileName(ParamStr(0)), '');

  bTransact := (FDatabase.IsSQLBased) and (not FDatabase.InTransaction);
  if (bTransact) then FDatabase.StartTransaction;
  try
    // Programmindex herausfinden
    with TQueryExt.Create(nil) do
    try
      DatabaseName := FDatabase.DatabaseName;
      SessionName := FDatabase.SessionName;
      Sql.Add('SELECT ' + C_Tf_PwProgDefs_ProgIndex);
      Sql.Add('FROM ' + C_Tb_PwProgDefs);
      Sql.Add('WHERE LOWER(' + C_Tf_PwProgDefs_ProgName + ') = ''' +
        LowerCase(sPrgName) + '''');
      Open;
      if (not Eof) then iPrgIx := Fields[0].asInteger else iPrgIx := -1;
      Close;

      // Wenn Programm nicht enthalten ...
      if (iPrgIx < 0)
      // ... neuer Index, ...
      then begin
        iPrgIx := FDbAutoInc.NextIndex[C_Tb_PwProgDefs];
        // Programm in Definitionstabelle eintragen
        with TTableExt.Create(nil) do
        try
          DatabaseName := FDatabase.DatabaseName;
          SessionName := FDatabase.SessionName;
          TableName := C_Tb_PwProgDefs;
          if (OpenExclusive) then begin
            AppendRecord([iPrgIx, sPrgName, '']);
            Close;
          end;
        finally
          Free;
        end;
      end
      // ... sonst Einträge löschen
      else begin
        Sql.Clear;
        Sql.Add('DELETE FROM ' + C_Tb_PwFktnLevel);
        Sql.Add('WHERE ' + C_Tf_PwFktnLevel_ProgIndex + ' = ' +
          IntToStr(iPrgIx));
        ExecSql;
      end;
    finally
      Free;
    end;

    // Controls eintragen
    with TTableExt.Create(nil) do
    try
      DatabaseName := FDatabase.DatabaseName;
      SessionName := FDatabase.SessionName;
      TableName := C_Tb_PwFktnLevel;
      if (OpenExclusive) then begin
        for i := 0 to pCtrls.Count-1 do begin
          sForm := GetStringPart(pCtrls[i], 1, '|');
          sCtrl := GetStringPart(pCtrls[i], 2, '|');
          sDesc := GetStringPart(pCtrls[i], 3, '|');
          iLevel := Integer(TObject(pCtrls.Objects[i]));
          FDbAutoInc.NextIndex[C_Tb_PwFktnLevel];
          AppendRecord([FDbAutoInc.NextIndex[C_Tb_PwFktnLevel], iPrgIx,
            sForm, sCtrl, sDesc, iLevel]);
        end;
        Close;
        Result := True;
      end;
    finally
      Free;
    end;

    if (bTransact) then FDatabase.Commit;
  finally
    if (bTransact) and (FDatabase.InTransaction) then FDatabase.Rollback;
  end;
end;

{ Gibt die PwLevels für aktuelles Prg. zurück }
{ Rückgabe: Liste mit Ctrls und Levels        }
{---------------------------------------------}
function TDbBenutzer.GetProgPwLevels(sPrgName: string = ''): TStrings;
{---------------------------------------------}
begin
  if (sPrgName = '') then
    sPrgName := ChangeFileExt(ExtractFileName(ParamStr(0)), '');

  Result := TStringList.Create;

  with TQueryExt.Create(nil) do
  try
    DatabaseName := FDatabase.DatabaseName;
    SessionName := FDatabase.SessionName;

    Sql.Add('SELECT A.' + C_Tf_PwProgLevels_LevelIndex + ',');
    Sql.Add('       A.' + C_Tf_PwProgLevels_LevelName);
    Sql.Add('FROM ' + C_Tb_PwProgLevels + ' A, ' + C_Tb_PwProgDefs + ' B');
    Sql.Add('WHERE LOWER(B.' + C_Tf_PwProgDefs_ProgName + ') = ''' +
      LowerCase(sPrgName) + '''');
    Sql.Add('AND A.' + C_Tf_PwProgLevels_ProgIndex + ' = B.' +
      C_Tf_PwProgDefs_ProgIndex);
    Sql.Add('ORDER BY A.' + C_Tf_PwProgLevels_LevelIndex);
    if (Open) then begin
      while (not Eof) do begin
        Result.AddObject(
          FieldByName(C_Tf_PwProgLevels_LevelName).asString,
          TObject(FieldByName(C_Tf_PwProgLevels_LevelIndex).asInteger));
        Next;
      end;
      Close;
    end;
  finally
    Free;
  end;
end;

{ Setzt die PwLevels für (aktuelles) Prg.     }
{ Parameter: Version, PrgName, Level-Liste    }
{ Rückgabe: Erfolg ja/nein                    }
{---------------------------------------------}
function TDbBenutzer.SetPrgPwLevels(
  fVersion: double; pLevels: TStrings; sPrgName: string = ''): boolean;
{---------------------------------------------}
var
  bTransact : boolean;
  i, iPrgIx : integer;
  iPwLevel  : integer;
begin
  Result := False;

  if (sPrgName = '') then
    sPrgName := ChangeFileExt(ExtractFileName(ParamStr(0)), '');

  bTransact := (FDatabase.IsSQLBased) and (not FDatabase.InTransaction);
  if (bTransact) then FDatabase.StartTransaction;
  try
    // Programmindex herausfinden
    with TQueryExt.Create(nil) do
    try
      DatabaseName := FDatabase.DatabaseName;
      SessionName := FDatabase.SessionName;
      Sql.Add('SELECT ' + C_Tf_PwProgDefs_ProgIndex);
      Sql.Add('FROM ' + C_Tb_PwProgDefs);
      Sql.Add('WHERE LOWER(' + C_Tf_PwProgDefs_ProgName + ') = ''' +
        LowerCase(sPrgName) + '''');
      Open;
      if (not Eof) then iPrgIx := Fields[0].asInteger else iPrgIx := -1;
      Close;

      // Wenn Programm nicht enthalten ...
      if (iPrgIx < 0)
      // ... neuer Index, ...
      then begin
        iPrgIx := FDbAutoInc.NextIndex[C_Tb_PwProgDefs];
        // Programm in Definitionstabelle eintragen ...
        with TTableExt.Create(nil) do
        try
          DatabaseName := FDatabase.DatabaseName;
          SessionName := FDatabase.SessionName;
          TableName := C_Tb_PwProgDefs;
          if (OpenExclusive) then begin
            AppendRecord([iPrgIx, sPrgName, '', fVersion]);
            Close;
          end;
        finally
          Free;
        end;
      end
      // ... sonst Einträge löschen und Version updaten
      else begin
        Sql.Clear;
        Sql.Add('DELETE FROM ' + C_Tb_PwProgLevels);
        Sql.Add('WHERE ' + C_Tf_PwProgLevels_ProgIndex + ' = ' +
          IntToStr(iPrgIx));
        ExecSql;    // Löschen

        Sql.Clear;
        Sql.Add('UPDATE ' + C_Tb_PwProgDefs);
        Sql.Add('SET ' + C_Tf_PwProgDefs_ProgVersion + ' = :Version');
        ParamByName('Version').asFloat := fVersion;
        Sql.Add('WHERE ' + C_Tf_PwProgDefs_ProgIndex + ' = ' +
          IntToStr(iPrgIx));
        ExecSql;   // Updaten
      end;
    finally
      Free;
    end;

    // ProgrammLevel eintragen
    with TTableExt.Create(nil) do
    try
      DatabaseName := FDatabase.DatabaseName;
      SessionName := FDatabase.SessionName;
      TableName := C_Tb_PwProgLevels;
      if (OpenExclusive) then begin
        iPwLevel := 0;  // Max. Passwortberechtigung (für Administrator)
        for i := 0 to pLevels.Count-1 do begin
          iPwLevel :=
            iPwLevel + Trunc(IntPower(2, Integer(pLevels.Objects[i])-1));
          AppendRecord([iPrgIx, Integer(pLevels.Objects[i]), pLevels[i]]);
        end;
        Close;

        // Eintragen aller Berechtigungen für den Administrator (GrpIx: 1)
        TableName := C_Tb_PwGrpLevel;
        SafeUpdateIndexDefs;
        IndexName := IndexDefs[0].Name;
        if (OpenExclusive) then begin
          if (FindKey([1, iPrgIx])) then begin
            Edit;
          end
          else begin
            Append;
            FieldByName(C_Tf_PwGrpLevel_GrpIndex).asInteger := 1;  // Admin.
            FieldByName(C_Tf_PwGrpLevel_ProgIndex).asInteger := iPrgIx;
          end;
          FieldByName(C_Tf_PwGrpLevel_LevelSet).asInteger := iPwLevel;
          Post;
          Close;

          Result := True;
        end;

      end;
    finally
      Free;
    end;

    if (bTransact) then FDatabase.Commit;
  finally
    if (bTransact) and (FDatabase.InTransaction) then FDatabase.Rollback;
  end;
end;

{ Gibt Einstellungen für akt. Prg. zurück     }
{ Parameter: Liste mit User-Informationen     }
{ Rückgabe: Erfolg ja/nein                    }
{---------------------------------------------}
function TDbBenutzer.GetProgramList(
  pUsrLevelList: TUserLevelList = nil; sProgName: string = ''): boolean;
{---------------------------------------------}
begin
  try
    if (not Assigned(pUsrLevelList)) then pUsrLevelList := FUserLevelList;
    if (sProgName = '')
    then sProgName := ChangeFileExt(ExtractFileName(ParamStr(0)), '')
    else sProgName := ChangeFileExt(ExtractFileName(sProgName), '');
    pUsrLevelList.Clear;

    with TQueryExt.Create(nil) do
    try
      DatabaseName := FDatabase.DatabaseName;
      SessionName := FDatabase.SessionName;

      // Eintragen der Zuordnung Gruppen-Programme-Level
      Sql.Add('SELECT A.' + C_Tf_PwProgDefs_ProgName + ',');
      Sql.Add('       A.' + C_Tf_PwProgDefs_ProgDesc + ',');
      Sql.Add('       B.' + C_Tf_PwGrpLevel_LevelSet + ',');
      Sql.Add('       C.' + C_Tf_PwUserDefs_UserName + ',');
      Sql.Add('       C.' + C_Tf_PwUserDefs_Password);
      Sql.Add('FROM ' + C_Tb_PwProgDefs + ' A, ' + C_Tb_PwGrpLevel + ' B,');
      Sql.Add(C_Tb_PwUserDefs + ' C, ' + C_Tb_PwUserInGrp + ' D');
      Sql.Add('WHERE LOWER(A.' + C_Tf_PwProgDefs_ProgName + ') = ''' +
        LowerCase(sProgName) + '''');
      Sql.Add('AND A.' + C_Tf_PwProgDefs_ProgIndex + ' = B.' +
        C_Tf_PwGrpLevel_ProgIndex);
      Sql.Add('AND B.' + C_Tf_PwGrpLevel_LevelSet + ' > 0');
      Sql.Add('AND B.' + C_Tf_PwGrpLevel_GrpIndex + ' = D.' +
        C_Tf_PwUserInGrp_GrpIndex);
      Sql.Add('AND D.' + C_Tf_PwUserInGrp_UserIndex + ' = C.' +
        C_Tf_PwUserDefs_UserIndex);
      if (Open) then begin
        while (not Eof) do begin
          pUsrLevelList.AddRecord(
            FieldByName(C_Tf_PwUserDefs_UserName).asString,
            FieldByName(C_Tf_PwUserDefs_Password).asString,
            FieldByName(C_Tf_PwGrpLevel_LevelSet).asInteger);
          Next;
        end;
        Close;
      end;

      Result := True;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

{ Gibt Mandantenliste zurück                  }
{ Parameter: Aktueller Anwender, Übergabel.   }
{ Rückgabe: Erfolg ja/nein                    }
{---------------------------------------------}
function TDbBenutzer.GetMandantenList(
  sUser: string = ''; pMandatentenList: TMandantenList = nil): boolean;
{---------------------------------------------}
begin
  try
    if (not Assigned(pMandatentenList)) then pMandatentenList := FMandantenList;
    pMandatentenList.Clear;

    with TQueryExt.Create(nil) do
    try
      DatabaseName := FDatabase.DatabaseName;
      SessionName := FDatabase.SessionName;

      Sql.Add('SELECT B.' + C_Tf_PwMandanten_MdtName + ',');
      Sql.Add('       B.' + C_Tf_PwMandanten_MdtKennung);
      Sql.Add('FROM ' + C_Tb_PwUserDefs + ' A, ' + C_Tb_PwMandanten + ' B');
      Sql.Add('WHERE A.' + C_Tf_PwUserDefs_UserIndex + ' = B.' +
        C_Tf_PwMandanten_UserIndex);
      if (sUser <> '') then
        Sql.Add('AND LOWER(A.' + C_Tf_PwUserDefs_UserName + ') = ''' +
          LowerCase(sUser) + '''');
      if (Open) then begin
        while (not Eof) do begin
          pMandatentenList.AddRecord(
            FieldByName(C_Tf_PwMandanten_MdtName).asString,
            FieldByName(C_Tf_PwMandanten_MdtKennung).asInteger);
          Next;
        end;
        Close;
      end;

      Result := True;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

{ Gibt komplette Einstellungen zurück         }
{ Rückgabe: Einstellungen in Tabellenform     }
{  Oben: Programe,Links: Gruppen,Rest: Levels }
{  Oben: User, Rest: Gruppen                  }
{  Oben: User, Rest: Mandantenlisten          }
{---------------------------------------------}
function TDbBenutzer.GetCompletePwGrpList
  (pGrpLevelList, pUsrGrpList, pMdtList: TMyTabList): boolean;
{---------------------------------------------}

  procedure InsertThisRecordSet(sProgName, sGrpName: string; iLevels: integer);
  var
    i, iGrpIx, iPrgIx : integer;
  begin
    // Zugehörige Programmspalte suchen
    iPrgIx := pGrpLevelList.IndexOf(sProgName);
    // Ggf. neue Programmspalte erzeugen ...
    if (iPrgIx <= 0) then begin
      iPrgIx := pGrpLevelList.AddObject(sProgName, TList.Create);
      for i := 0 to pGrpLevelList.Col[0].Count-1 do // ... und auffüllen
        TList(pGrpLevelList.Objects[iPrgIx]).Add(nil);
    end;

    // Zugehörige Gruppenzeile suchen
    iGrpIx := pGrpLevelList.Col[0].IndexOf(sGrpName);
    // Ggf. neue Gruppenzeile erzeugen ...
    if (iGrpIx < 0) then begin
      iGrpIx := pGrpLevelList.Col[0].Add(sGrpName);
      // ... und auffüllen
      for i := 1 to pGrpLevelList.Count-1 do
        TList(pGrpLevelList.Objects[i]).Add(nil);
    end;

    // PwLevel eintragen
    TList(pGrpLevelList.Objects[iPrgIx])[iGrpIx] := TObject(iLevels);
  end;

  procedure InsertUserRecordSet(sUserName, sGrpName: string);
  var
    iUsrIx, iGrpIx : integer;
  begin
    // Zugehörige Benutzerspalte suchen
    iUsrIx := pUsrGrpList.IndexOf(sUserName);
    // Ggf. neue Benutzerspalte erzeugen ...
    if (iUsrIx < 0) then
      iUsrIx := pUsrGrpList.AddObject(sUserName, TStringList.Create);

    // Zugehörige Gruppenzeile suchen
    iGrpIx := pUsrGrpList.Col[iUsrIx].IndexOf(sGrpName);
    // Ggf. neuen Gruppeneintrag erzeugen ...
    if (iGrpIx < 0) then pUsrGrpList.Col[iUsrIx].Add(sGrpName);
  end;

  procedure InsertMandantenRecordSet(sUserName, sMdtName: string);
  var
    iUsrIx, iMdtIx : integer;
  begin
    // Zugehörige Benutzerspalte suchen
    iUsrIx := pMdtList.IndexOf(sUserName);
    // Ggf. neue Benutzerspalte erzeugen ...
    if (iUsrIx < 0) then
      iUsrIx := pMdtList.AddObject(sUserName, TStringList.Create);

    // Zugehörige Gruppenzeile suchen
    iMdtIx := pMdtList.Col[iUsrIx].IndexOf(sMdtName);
    // Ggf. neuen Gruppeneintrag erzeugen ...
    if (iMdtIx < 0) then pMdtList.Col[iUsrIx].Add(sMdtName);
  end;

var
  i, iColIx : integer;
begin
  try
    pGrpLevelList.Clear;
    pUsrGrpList.Clear;
    pMdtList.Clear;
    pGrpLevelList.AddObject('', TStringList.Create);  // Spalte der Gruppennamen

    with TQueryExt.Create(nil) do
    try
      DatabaseName := FDatabase.DatabaseName;
      SessionName := FDatabase.SessionName;

      // Holen aller Gruppen
      Sql.Add('SELECT ' + C_Tf_PwGrpDefs_GrpName + ', ' +
        C_Tf_PwGrpDefs_GrpDesc);
      Sql.Add('FROM ' + C_Tb_PwGrpDefs);
      Sql.Add('ORDER BY ' + C_Tf_PwGrpDefs_GrpName);
      if (Open) then begin
        while (not Eof) do begin
          pGrpLevelList.Col[0].Add(Fields[0].asString + '|' + Fields[1].asString);
          Next;
        end;
        Close;
      end;
      Sql.Clear;

      // Holen aller Programme
      Sql.Add('SELECT ' + C_Tf_PwProgDefs_ProgName + ', ' +
        C_Tf_PwProgDefs_ProgDesc);
      Sql.Add('FROM ' + C_Tb_PwProgDefs);
      Sql.Add('ORDER BY ' + C_Tf_PwProgDefs_ProgName);
      if (Open) then begin
        while (not Eof) do begin
          iColIx := pGrpLevelList.AddObject(
            Fields[0].asString + '|' + Fields[1].asString, TList.Create);
          for i := 0 to pGrpLevelList.Col[0].Count-1 do
            TList(pGrpLevelList.Objects[iColIx]).Add(TObject(1));
          Next;
        end;
        Close;
      end;
      Sql.Clear;

      // Holen aller User
      Sql.Add('SELECT ' + C_Tf_PwUserDefs_UserName + ', ' +
        C_Tf_PwUserDefs_UserDesc + ', ' + C_Tf_PwUserDefs_Password);
      Sql.Add('FROM ' + C_Tb_PwUserDefs);
      Sql.Add('ORDER BY ' + C_Tf_PwUserDefs_UserName);
      if (Open) then begin
        while (not Eof) do begin
          pUsrGrpList.AddObject(Fields[0].asString + '|' +
            Fields[1].asString + '|' + Fields[2].asString, TStringList.Create);
          pMdtList.AddObject(Fields[0].asString, TStringList.Create);
          Next;
        end;
        Close;
      end;
      Sql.Clear;

      // Eintragen der Zuordnung Gruppen-Programme-Level
      Sql.Add('SELECT A.' + C_Tf_PwProgDefs_ProgName + ',');
      Sql.Add('       A.' + C_Tf_PwProgDefs_ProgDesc + ',');
      Sql.Add('       B.' + C_Tf_PwGrpDefs_GrpName + ',');
      Sql.Add('       B.' + C_Tf_PwGrpDefs_GrpDesc + ',');
      Sql.Add('       C.' + C_Tf_PwGrpLevel_LevelSet);
      Sql.Add('FROM ' + C_Tb_PwProgDefs + ' A, ' + C_Tb_PwGrpDefs + ' B,');
      Sql.Add(C_Tb_PwGrpLevel + ' C');
      Sql.Add('WHERE B.' + C_Tf_PwGrpDefs_GrpIndex + ' = C.' +
        C_Tf_PwGrpLevel_GrpIndex);
      Sql.Add('AND C.' + C_Tf_PwGrpLevel_ProgIndex + ' = A.' +
        C_Tf_PwProgDefs_ProgIndex);
      if (Open) then begin
        while (not Eof) do begin
          InsertThisRecordSet(FieldByName(C_Tf_PwProgDefs_ProgName).asString +
            '|' + FieldByName(C_Tf_PwProgDefs_ProgDesc).asString,
            FieldByName(C_Tf_PwGrpDefs_GrpName).asString + '|' +
            FieldByName(C_Tf_PwGrpDefs_GrpDesc).asString,
            FieldByName(C_Tf_PwGrpLevel_LevelSet).asInteger+1);
          Next;
        end;
        Close;
      end;

      // Eintragen der Zuordnung Gruppen-User
      Sql.Clear;
      Sql.Add('SELECT A.' + C_Tf_PwUserDefs_UserName + ',');
      Sql.Add('       A.' + C_Tf_PwUserDefs_UserDesc + ',');
      Sql.Add('       A.' + C_Tf_PwUserDefs_Password + ',');
      Sql.Add('       B.' + C_Tf_PwGrpDefs_GrpName);
      Sql.Add('FROM ' + C_Tb_PwUserDefs + ' A, ' + C_Tb_PwGrpDefs + ' B,');
      Sql.Add(C_Tb_PwUserInGrp + ' C');
      Sql.Add('WHERE B.' + C_Tf_PwGrpDefs_GrpIndex + ' = C.' +
        C_Tf_PwUserInGrp_GrpIndex);
      Sql.Add('AND C.' + C_Tf_PwUserInGrp_UserIndex + ' = A.' +
        C_Tf_PwUserDefs_UserIndex);
      if (Open) then begin
        while (not Eof) do begin
          InsertUserRecordSet(FieldByName(C_Tf_PwUserDefs_UserName).asString +
            '|' + FieldByName(C_Tf_PwUserDefs_UserDesc).asString +
            '|' + FieldByName(C_Tf_PwUserDefs_Password).asString,
            FieldByName(C_Tf_PwGrpDefs_GrpName).asString);
          Next;
        end;
        Close;
      end;

      // Eintragen der Zuordnung Mandanten-User
      Sql.Clear;
      Sql.Add('SELECT A.' + C_Tf_PwUserDefs_UserName + ',');
      Sql.Add('       B.' + C_Tf_PwMandanten_MdtName + ',');
      Sql.Add('       B.' + C_Tf_PwMandanten_MdtKennung);
      Sql.Add('FROM ' + C_Tb_PwUserDefs + ' A, ' + C_Tb_PwMandanten + ' B');
      Sql.Add('WHERE A.' + C_Tf_PwUserDefs_UserIndex + ' = B.' +
        C_Tf_PwMandanten_UserIndex);
      if (Open) then begin
        while (not Eof) do begin
          InsertMandantenRecordSet(FieldByName(C_Tf_PwUserDefs_UserName).asString,
            FieldByName(C_Tf_PwMandanten_MdtName).asString + '|' +
            FieldByName(C_Tf_PwMandanten_MdtKennung).asString);
          Next;
        end;
        Close;
      end;

      Result := True;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

{ Setzt komplette Gruppen-Einstellungen       }
{ Parameter: Einstellungen in Tabellenform    }
{  Oben: Programe,Links: Gruppen,Rest: Levels }
{  Oben: User, Rest: Gruppen                  }
{---------------------------------------------}
procedure TDbBenutzer.SetCompletePwGrpList(
  pGrpLevelList, pUsrGrpList, pMdtList: TMyTabList);
{---------------------------------------------}

  procedure CreateThisTable(var pTable: TTableExt; sTbName: string);
  begin
    pTable := TTableExt.Create(nil);
    pTable.DatabaseName := FDatabase.DatabaseName;
    pTable.SessionName := FDatabase.SessionName;
    pTable.TableName := sTbName;
    pTable.SafeUpdateIndexDefs;
  end;

  procedure CarePrgTable(pTbProg: TTableExt);
  var
    sProg  : string;
    bFound : boolean;
    i      : integer;
  begin
    pTbProg.Last;
    while (not pTbProg.Bof) do begin
      sProg := pTbProg.FieldByName(C_Tf_PwProgDefs_ProgName).asString;
      bFound := False;
      for i := 0 to pGrpLevelList.Count-1 do begin
        if (GetStringPart(pGrpLevelList[i], 1, '|') = sProg) then begin
          bFound := True;
          Break;
        end;
      end;
      if (bFound) then pTbProg.Prior else pTbProg.Delete;
    end;
  end;

  procedure CareGrpTable(pTbGrp: TTableExt);
  var
    sGroup : string;
    bFound : boolean;
    i      : integer;
  begin
    pTbGrp.Last;
    while (not pTbGrp.Bof) do begin
      sGroup := pTbGrp.FieldByName(C_Tf_PwGrpDefs_GrpName).asString;
      bFound := False;
      for i := 0 to pGrpLevelList.Col[0].Count-1 do begin
        if (GetStringPart(pGrpLevelList.Col[0][i], 1, '|') = sGroup) then begin
          bFound := True;
          Break;
        end;
      end;
      if (bFound) then pTbGrp.Prior else pTbGrp.Delete;
    end;
  end;

  procedure CareUserTable(pTbUser: TTableExt);
  var
    sUser  : string;
    bFound : boolean;
    i      : integer;
  begin
    pTbUser.Last;
    while (not pTbUser.Bof) do begin
      sUser := pTbUser.FieldByName(C_Tf_PwUserDefs_UserName).asString;
      bFound := False;
      for i := 0 to pMdtList.Count-1 do begin
        if (pMdtList[i] = sUser) then begin
          bFound := True;
          Break;
        end;
      end;
      if (bFound) then pTbUser.Prior else pTbUser.Delete;
    end;
  end;

  function GetThisIndex(pTb: TTableExt; sIndexString: string): integer;
  var
    sName, sDesc, sPW : string;
  begin
    sName := GetStringPart(sIndexString, 1, '|');
    sDesc := GetStringPart(sIndexString, 2, '|');
    sPW := GetStringPart(sIndexString, 3, '|');
    if (pTb.FindKey([sName])) then begin
      Result := pTb.Fields[0].asInteger;
      pTb.Edit;
    end
    else begin
      Result := FDbAutoInc.NextIndex[pTb.TableName];
      pTb.Append;
      pTb.Fields[0].asInteger := Result;
      pTb.Fields[1].asString := sName;
    end;
      pTb.Fields[2].asString := sDesc;
//      if (sPW <> '') then 
      if (pTb.Tablename = C_Tb_PwUserDefs) then 
        pTb.Fields[3].asString := sPW;  // 27.01.2010
      pTb.Post;
  end;

var
  i, j, iPrgIx, iGrpIx, iUsrIx                : integer;
  bTransact                                   : boolean;
  pTbPrg, pTbGrp, pTbLevel, pTbUsr, pTbGrpUsr, pTbMdt : TTableExt;
begin
  bTransact := (FDatabase.IsSQLBased) and (not FDatabase.InTransaction);
  if (bTransact) then FDatabase.StartTransaction;
  try

    CreateThisTable(pTbPrg, C_Tb_PwProgDefs);
    CreateThisTable(pTbGrp, C_Tb_PwGrpDefs);
    CreateThisTable(pTbLevel, C_Tb_PwGrpLevel);
    CreateThisTable(pTbUsr, C_Tb_PwUserDefs);
    CreateThisTable(pTbGrpUsr, C_Tb_PwUserInGrp);
    CreateThisTable(pTbMdt, C_Tb_PwMandanten);
    try

      pTbPrg.IndexName := C_Ti_PwProgDefs_Name;
      pTbGrp.IndexName := C_Ti_PwGrpDefs_Name;
      pTbUsr.IndexName := C_Ti_PwUserDefs_Name;

      pTbPrg.OpenExclusive;
      pTbGrp.OpenExclusive;
      pTbUsr.OpenExclusive;

      pTbLevel.EmptyTable;
      pTbLevel.OpenExclusive;
      pTbGrpUsr.EmptyTable;
      pTbGrpUsr.OpenExclusive;
      pTbMdt.EmptyTable;
      FDbAutoInc.NextIndex[C_Tb_PwMandanten] := 1;
      pTbMdt.OpenExclusive;
      try

        CarePrgTable(pTbPrg);
        CareGrpTable(pTbGrp);
        CareUserTable(pTbUsr);

        // Eintragen der Zuordnung Gruppen-Levels aus Liste in Tabelle
        for i := 1 to pGrpLevelList.Count-1 do begin  // Schleife über Programme
          iPrgIx := GetThisIndex(pTbPrg, pGrpLevelList[i]);

          for j := 0 to pGrpLevelList.Col[0].Count-1 do begin  // Schleife über Gruppen
            iGrpIx := GetThisIndex(pTbGrp, pGrpLevelList.Col[0][j]);

            // Eintragen des Levels
            if (pTbLevel.FindKey([iGrpIx, iPrgIx])) then begin
              pTbLevel.Edit;
              pTbLevel.FieldByName(C_Tf_PwGrpLevel_LevelSet).asInteger :=
                Integer(TList(pGrpLevelList.Objects[i]).Items[j])-1;
              pTbLevel.Post;
            end
            else begin
              pTbLevel.Append;
              pTbLevel.FieldByName(C_Tf_PwGrpLevel_GrpIndex).asInteger :=
                iGrpIx;
              pTbLevel.FieldByName(C_Tf_PwGrpLevel_ProgIndex).asInteger :=
                iPrgIx;
              pTbLevel.FieldByName(C_Tf_PwGrpLevel_LevelSet).asInteger :=
                Integer(TList(pGrpLevelList.Objects[i]).Items[j])-1;
              pTbLevel.Post;
            end;
          end;
        end;

        // Eintragen der Zuordnung User-Gruppen aus Liste in Tabelle
        for i := 0 to pUsrGrpList.Count-1 do begin  // Schleife über User
          iUsrIx := GetThisIndex(pTbUsr, pUsrGrpList[i]);

          for j := 0 to pUsrGrpList.Col[i].Count-1 do begin  // Schleife über Gruppen
            iGrpIx := 0;  // Ergibt Hinweis - Auskommentieren ergibt Warnung ...
            if (iGrpIx > 1) then;
            if (not pTbGrp.FindKey([pUsrGrpList.Col[i][j]]))
            then Continue
            else iGrpIx := pTbGrp.Fields[0].asInteger;

            // Eintragen des Levels
            if (not pTbGrpUsr.FindKey([iUsrIx, iGrpIx])) then begin
              pTbGrpUsr.Append;
              pTbGrpUsr.FieldByName(C_Tf_PwUserInGrp_UserIndex).asInteger :=
                iUsrIx;
              pTbGrpUsr.FieldByName(C_Tf_PwUserInGrp_GrpIndex).asInteger :=
                iGrpIx;
              pTbGrpUsr.Post;
            end;
          end;
        end;

        // Eintragen der Mandanten aus Liste in Tabelle
        for i := 0 to pMdtList.Count-1 do begin  // Schleife über User (User = Mandant !)
          iUsrIx := GetThisIndex(pTbUsr, pUsrGrpList[i]);

          for j := 0 to pMdtList.Col[i].Count-1 do begin  // Schleife über Gruppen
            pTbMdt.Append;
            pTbMdt.FieldByName(C_Tf_PwMandanten_MdtIndex).asInteger :=
              FDbAutoInc.NextIndex[C_Tb_PwMandanten];
            pTbMdt.FieldByName(C_Tf_PwMandanten_UserIndex).asInteger :=
              iUsrIx;
            pTbMdt.FieldByName(C_Tf_PwMandanten_MdtName).asString :=
              GetStringPart(pMdtList.Col[i][j], 1, '|');
            if (GetStringPart(pMdtList.Col[i][j], 2, '|') <> '') then
              pTbMdt.FieldByName(C_Tf_PwMandanten_MdtKennung).asInteger :=
                StrToIntDef(GetStringPart(pMdtList.Col[i][j], 2, '|'), 0);
            pTbMdt.Post;
          end;
        end;

      finally
        pTbMdt.Close;
        pTbPrg.Close;
        pTbGrp.Close;
        pTbLevel.Close;
        pTbUsr.Close;
        pTbGrpUsr.Close;
      end;

    finally
      pTbMdt.Free;
      pTbPrg.Free;
      pTbGrp.Free;
      pTbLevel.Free;
      pTbUsr.Free;
      pTbGrpUsr.Free;
    end;

    if (bTransact) then FDatabase.Commit;
  finally
    if (bTransact) and (FDatabase.InTransaction)
    then FDatabase.Rollback
    else Self.GetProgramList;
  end;
end;

{---------------------------------------------}
function TDbBenutzer.GetUserList: TStrings;
{---------------------------------------------}
var
  i : integer;
begin
  Result := TStringList.Create;

  for i := 0 to FUserLevelList.Count-1 do
    Result.Add(FUserLevelList.GetUserName(i));
end;

{---------------------------------------------}
function TDbBenutzer.GetPassword(const sUser: string): string;
{---------------------------------------------}
begin
  Result := FUserLevelList.GetPassword(sUser);
end;

{----------------------------- TFormPwEingabeDlg ------------------------------}

{----------------------------------------------------}
procedure TFormPasswordDlg.FormShow(Sender: TObject);
{----------------------------------------------------}
begin
  if (Visible) then ePassword.SetFocus;
end;

{----------------------------------------------------}
procedure TFormPasswordDlg.SetUserObject(pUserObj: TDbBenutzer);
{----------------------------------------------------}
var
  pSl : TStrings;
begin
  if (Assigned(pUserObj)) then begin
    FBenutzerObj := pUserObj;
    pSl := FBenutzerObj.GetUserList;
    try
      cbUsernames.Items.Assign(pSl);
    finally
      pSl.Free;
    end;
  end;
end;

{----------------------------------------------------}
function TFormPasswordDlg.CheckEingaben: boolean;
{----------------------------------------------------}
begin
  Result := (ePassword.Text = FBenutzerObj.GetPassword(cbUsernames.Text));
end;

{----------------------------------------------------}
function TFormPasswordDlg.GetUsername: string;
{----------------------------------------------------}
begin
  if (CheckEingaben) then Result := Trim(cbUsernames.Text) else Result := '';
end;

{----------------------------------------------------}
procedure TFormPasswordDlg.SetUsername(sUsername: string);
{----------------------------------------------------}
begin
  cbUsernames.ItemIndex := cbUsernames.Items.IndexOf(sUsername);
  if (cbUsernames.ItemIndex < 0) and (cbUsernames.Items.Count > 0) then
    cbUsernames.ItemIndex := 0;
end;

{----------------------------------------------------}
function TFormPasswordDlg.GetPassword: string;
{----------------------------------------------------}
begin
  if (CheckEingaben) then Result := ePassword.Text else Result := '';
end;

{----------------------------------------------------}
function TFormPasswordDlg.GetPwLevel: TByteSet;
{----------------------------------------------------}
begin
  if (CheckEingaben)
  then Result := FBenutzerObj.GetPwLevels(cbUsernames.Text)
  else Result := [];
end;

{----------------------------------------------------}
procedure TFormPasswordDlg.bbtnOkClick(Sender: TObject);
{----------------------------------------------------}
begin
  if (not CheckEingaben) then begin
    MessageDlg(S_DifPWs, mtError, [mbOk], 0);
    ModalResult := mrNone;
    ePassword.Text := '';
    ePassword.SetFocus;
  end
  else ModalResult := mrOk;
end;

{------------------------------- TPwListObject --------------------------------}

{------------------------------------------------------}
constructor TPwListObject.Create(pDatabase: TDatabase);
{------------------------------------------------------}
begin
  inherited Create;

  FRefList := TStringList.Create;
  FDbBenutzer := TDbBenutzer.Create(pDatabase);
  FDbBenutzer.GetProgramList;
  FAktUser := '';           // Aktuell angemeldeter Anwender
  FLoadMandanten := False;  // Flag, ob Mandanten bei Userwechsel geladen werden
  FPasswordFunctionDisabled := False;  // Passwortschutz enabled
end;

{------------------------------------------------------}
destructor TPwListObject.Destroy;
{------------------------------------------------------}
begin
  FRefList.Free;
  FDbBenutzer.Free;

  inherited;
end;

{ Übergibt die Pw-Levels des aktuellen Anwenders       }
{ Parameter: Set von PW-Levels                         }
{------------------------------------------------------}
procedure TPwListObject.SetPwLevels(pLevels: TByteSet);
{------------------------------------------------------}
begin
  if (pLevels <> FPwLevels) then begin
    FPwLevels := pLevels;
    EnDisablePwControls;
  end;
end;

{ Gibt Passwortlevel für Control zurück                }
{ Parameter: Übergeordnetes Formular, Ctrl, PW-Level   }
{ Rückgabe: PW-Level (aus RefListe oder überg. Default)}
{------------------------------------------------------}
function TPwListObject.GetReferenzPwLevel(
  pForm: TForm; pComponent: TComponent; iPwLevel: byte): byte;
{------------------------------------------------------}
var
  sIndexName : string;
  iIndex     : integer;
begin
  // Referenzierte Controls sind unter <formname>|<ctrlname> gespeichert
  sIndexName :=
    LowerCase(pForm.ClassName) + '|' + LowerCase(pComponent.ClassName);
  // Index aus Referenzliste ermitteln
  iIndex := FRefList.IndexOf(sIndexName);
  if (iIndex >= 0)
  then Result := Integer(FRefList.Objects[iIndex]) // Ergebnis aus Liste
  else Result := iPwLevel;                         // Ergebnis aus Default-Wert
end;

{ Fügt ein Passwort-gesteuertes Control in Liste ein   }
{ Parameter: Übergeordnetes Formular, Ctrl, PW-Level   }
{------------------------------------------------------}
function TPwListObject.GetPwLevel(pForm: TForm; pComponent: TComponent): byte;
{------------------------------------------------------}
var
  iIndex : integer;
begin
  Result := 0;
  iIndex := IndexOf(IntToStr(Integer(pForm)));
  if (iIndex >= 0) then
    with Col[iIndex] do begin
      iIndex := IndexOfObject(pComponent);
      if (iIndex >= 0) then Result := StrToInt(Strings[iIndex]);
    end;
end;

{ Setzt den "Disabled"-Status des Passwort-Objekts     }
{ Parameter: Status: T=Disabled, F=Enabled             }
{------------------------------------------------------}
procedure TPwListObject.SetPasswordFunctionDisabled(bState: boolean);
{------------------------------------------------------}
var
  i : integer;
  p : TByteSet;
begin
  if (bState <> FPasswordFunctionDisabled) then begin
    FPasswordFunctionDisabled := bState;
    if (bState) then begin
      p := [];
      for i := C_MinPwLevel to C_MaxPwLevel do Include(p, i);
      PwLevels := p;
    end
    else begin
      if (FAktUser <> '')
      then FDbBenutzer.GetPwLevels(FAktUser)
      else PwLevels := [];
      EnDisablePwControls;
    end;

    EnDisablePwControls;
    if (Assigned(FPasswordButton)) then begin
      FPasswordButton.Click;
      if (Assigned(FPasswordButton.OnPasswordExecute)) then
        FPasswordButton.OnPasswordExecute(FPasswordButton);
      FPasswordButton.Visible := (not FPasswordFunctionDisabled);
      FPasswordButton.FMenu.Visible := (not FPasswordFunctionDisabled);  // 05.12.2011
    end;
  end;
end;

{ Fügt ein Passwort-gesteuertes Control in Liste ein   }
{ Parameter: Übergeordnetes Formular, Ctrl, PW-Level   }
{------------------------------------------------------}
procedure TPwListObject.InsertComponent(
  pForm: TForm; pComponent: TComponent; iPwLevel: byte);
{------------------------------------------------------}
var
  iIndex, iLevel : integer;
begin
  iIndex := IndexOf(IntToStr(Integer(pForm)));  // Formular-Zeiger als string
  if (iIndex < 0) then                          // Ggf. FormListe einfügen
    iIndex := Self.AddObject(IntToStr(Integer(pForm)), TStringList.Create);
  // Passwortlevel als String, Control als Object
  iLevel := GetReferenzPwLevel(pForm, pComponent, iPwLevel);
  Col[iIndex].AddObject(IntToStr(iLevel), pComponent);

  if (pComponent is TControl) then
    TControl(pComponent).Enabled := (iLevel in PwLevels)
  else if (pComponent is TAction) then
    TAction(pComponent).Enabled := (iLevel in PwLevels)
  else if (pComponent is TMenuItem) then
    TMenuItem(pComponent).Enabled := (iLevel in PwLevels);
end;

{ Fügt ein Passwort-gesteuertes Control in Liste ein   }
{ Parameter: Übergeordnetes Formular, Ctrl, PW-Level   }
{------------------------------------------------------}
procedure TPwListObject.DeleteComponent(pForm: TForm; pComponent: TComponent);
{------------------------------------------------------}
var
  iIndex : integer;
begin
  iIndex := IndexOf(IntToStr(Integer(pForm)));
  if (iIndex >= 0) then
    with Col[iIndex] do begin
      iIndex := IndexOfObject(pComponent);
      if (iIndex >= 0) then Delete(iIndex);
    end;
end;

{ Entfernt alle Controls zu einem Formular aus Liste   }
{ Parameter: Formular, dessen Ctrls freizugeben sind   }
{------------------------------------------------------}
procedure TPwListObject.DeleteForm(pForm: TForm);
{------------------------------------------------------}
var
  iIndex : integer;
begin
  iIndex := IndexOf(IntToStr(Integer(pForm)));
  if (iIndex >= 0) then Delete(iIndex);
end;

{ En-/Disablen aller gespeicherten Controls gem. Level }
{ Parameter: Aktueller Passwort-Level                  }
{------------------------------------------------------}
procedure TPwListObject.EnDisablePwControls;
{------------------------------------------------------}
var
  i, j, iForm : integer;
begin
  for i := Count-1 downto 0 do begin  // Schleife über alle Formulare
    iForm := StrToInt(Strings[i]);
    with Col[i] do
      for j := 0 to Count-1 do  // Alle Ctrls des betreffenden Formulars
        if (Objects[j] is TForm) then begin
          if (Integer(Objects[j]) = iForm) and
             (StrToInt(Strings[j]) in FPwLevels) then
          begin
            TControl(Objects[j]).Enabled := True;
          end
          else begin
            TForm(Objects[j]).Free;
            Break;
          end;
        end
        else if (Objects[j] is TControl) then
          TControl(Objects[j]).Enabled := (StrToInt(Strings[j]) in FPwLevels)
        else if (Objects[j] is TAction) then
          TAction(Objects[j]).Enabled := (StrToInt(Strings[j]) in FPwLevels)
        else if (Objects[j] is TMenuItem) then
          TMenuItem(Objects[j]).Enabled := (StrToInt(Strings[j]) in FPwLevels);
  end;
  if (Count > 0) and (Assigned(Application.MainForm)) then
    Application.MainForm.Invalidate;
end;

{------------------------------------------------------}
procedure TPwListObject.EnableUser;
{---------------------------------------------}
var
  sLastUser : string;
begin
  if (not FPasswordFunctionDisabled) then begin
    if (FDbBenutzer.GetPassword(C_PwAdmin) = '')  // 27.01.2010
    then PasswordFunctionDisabled := True
    else
      with TProgramIni.Create do
      try
        sLastUser := ReadString('SETTINGS', 'PWLASTUSER', '');

        with TFormPasswordDlg.Create(nil) do
        try
          UserObject := FDbBenutzer;
          UserName := sLastUser;
          if (ShowModal = mrOk) then begin
            WriteString('SETTINGS', 'PWLASTUSER', UserName);
            if (UserName <> Self.FAktUser) then begin
              Self.FAktUser := UserName;
              Self.PwLevels := PwLevel;
              if (FLoadMandanten) then Self.FDbBenutzer.GetMandantenList(UserName);
              Self.EnDisablePwControls;
            end;
          end;
        finally
          Free;
        end;

    finally
      Free;
    end;
  end;
end;

{------------------------------------------------------}
procedure TPwListObject.DisableUser;
{------------------------------------------------------}
begin
  if (not FPasswordFunctionDisabled) then begin
    Self.PwLevels := [];
    FAktUser := '';
    Self.EnDisablePwControls;
  end;
end;

{------------------------------------------------------}
function TPwListObject.GetMandantenListe: TMandantenList;
{------------------------------------------------------}
begin
  Result := FDbBenutzer.MandantenList;
end;

{------------------------------- TPasswordButton ------------------------------}

{-------------------------------------------------------}
constructor TPasswordButton.Create(pOwner: TComponent);
{-------------------------------------------------------}
begin
  inherited;

  FDatabase := nil;
  FPwListObject := nil;
  FOnPasswordExecute := nil;

  Text := '';

  BevelInner := bvLowered;
  BorderWidth := 2;
  Height := 31;
  Width := 31;

  FAction := TAction.Create(Self);
  FAction.Caption := S_EnablePassword;
  FAction.Hint := S_DisablePassword;
  FAction.Name := 'aSetPw' + IntToStr(Integer(Self));
  FAction.OnExecute := aPasswordExecute;

  FMenu := NewItem(
    '' , 0, False, True, nil, 0, 'miSetPw' + IntToStr(Integer(Self)));
  FMenu.Action := FAction;

  FButton := TSpeedButton.Create(Self);
  FButton.Parent := Self;
  FButton.GroupIndex := Integer(Self);
  FButton.AllowAllUp := True;
  FButton.NumGlyphs := 2;
  FButton.Glyph.LoadFromResourceName(HINSTANCE, C_Res_Shut);
  FButton.Action := FAction;
  FButton.Caption := '';
  FButton.Visible := True;

  FPasswordButton := Self;

  InsertActionToActionList(nil);
end;

{-------------------------------------------------------}
destructor TPasswordButton.Destroy;
{-------------------------------------------------------}
begin
  FPasswordButton := nil;
  FreeAndNil(FButton);
  FreeAndNil(FPwListObject);

  inherited;
end;

{-------------------------------------------------------}
procedure TPasswordButton.InsertActionToActionList(Sender: TObject);
{-------------------------------------------------------}
const
  C_OnShowEvent: TNotifyEvent = nil;
var
  i      : integer;
  pForm  : TForm;
  pAList : TActionList;
  pMenu  : TMainMenu;
  pWCtrl : TWinControl;
begin
  if (not (csDesigning in Self.ComponentState)) then begin
    pAList := nil;
    pMenu := nil;
    pForm := nil;
    pWCtrl := nil;
    if (Assigned(Self.Parent)) then pWCtrl := Self.Parent
    else if (Self.Owner is TWinControl) then
      pWCtrl := TWinControl(Self.Owner);

    while Assigned(pWCtrl) do begin
      if (pWCtrl is TForm) then begin
        pForm := TForm(pWCtrl);
        Break;
      end
      else pWCtrl := pWCtrl.Parent;
    end;

    if (Assigned(pForm)) then begin
      if (Sender = nil) then begin
        C_OnShowEvent := pForm.OnShow;
        pForm.OnShow := Self.InsertActionToActionList;
      end
      else begin
        pForm.OnShow := C_OnShowEvent;
        for i := 0 to pForm.ComponentCount-1 do begin
          if (pForm.Components[i] is TActionList) then
            pAList := TActionList(pForm.Components[i])
          else if (pForm.Components[i] is TMainMenu) then
            pMenu := TMainMenu(pForm.Components[i]);
          if (Assigned(pAList)) and (Assigned(pMenu)) then Break;
        end;
        if (Assigned(pAList)) then FAction.ActionList := pAList;
        if (Assigned(pMenu)) and (pMenu.Items.Count > 0) then
          pMenu.Items[0].Insert(0, FMenu);

        if (Assigned(C_OnShowEvent)) then C_OnShowEvent(Sender);
      end;

    end;
  end;
end;

{-------------------------------------------------------}
procedure TPasswordButton.CreateDatabase;
{-------------------------------------------------------}
var
  i : integer;
begin
  if (FDbName <> '') then begin

    if (not ((Assigned(FDatabase)) and (FDatabase.DatabaseName = FDbName))) then
    begin

      for i := 0 to Sessions.Count-1 do begin
        FDatabase := Sessions[i].FindDatabase(FDbName);
        if (Assigned(FDatabase)) then Break;
      end;
      FreeAndNil(FPwListObject);
      if (Assigned(FDatabase)) then
        FPwListObject := TPwListObject.Create(FDatabase);
    end;
  end
  else begin
    FDatabase := nil;
    FreeAndNil(FPwListObject);
  end;
end;

{-------------------------------------------------------}
procedure TPasswordButton.SetCaption(const sCaption: string);
{-------------------------------------------------------}
begin
  FButton.Caption := sCaption;
  Text := '';
end;

{-------------------------------------------------------}
function TPasswordButton.GetCaption: string;
{-------------------------------------------------------}
begin
  Result := FButton.Caption;
end;

{-------------------------------------------------------}
procedure TPasswordButton.SetBounds(iLeft, iTop, iWidth, iHeight: integer);
{-------------------------------------------------------}
begin
  inherited;

  if (not (csDestroying in Self.ComponentState)) and (Assigned(FButton)) then
    FButton.SetBounds(4, 4, iWidth-8, iHeight-8);
end;


{---------------------------------------------}
procedure TPasswordButton.Execute;
{---------------------------------------------}
begin
  FAction.Tag := 0;
  FButton.Down := False;
  FAction.Hint := S_EnablePassword;
  FAction.Caption := S_EnablePassword;
  aPasswordExecute(FButton);
end;

{---------------------------------------------}
procedure TPasswordButton.aPasswordExecute(Sender: TObject);
{---------------------------------------------}
begin
  if (Assigned(FPwListObject)) then begin
    if (FAction.Tag = 0) then begin
      FPwListObject.EnableUser;

      if (FPwListObject.PwLevels = []) then begin  // Keine Rechte ...
        if (FPwListObject.AktUser <> '') then begin  // ... weil rechtloser Gesell
          MessageDlg(Format(S_UserHasNoRights, [FPwListObject.AktUser]),
            mtInformation, [mbOk], 0);
          FPwListObject.DisableUser;
        end;   // Alternative Möglichkeit ist ein konsequenloser Abbruch
      end
      else begin
        FAction.Tag := 1;
        FAction.Hint := S_DisablePassword;
        FAction.Caption := S_DisablePassword;
      end;
    end
    else begin
      FPwListObject.DisableUser;
      FAction.Tag := 0;
      FAction.Hint := S_EnablePassword;
      FAction.Caption := S_EnablePassword;
    end;

    FButton.Caption := '';
    FButton.Down := (FAction.Tag = 1);
  end;

  if (FButton.Down)
  then FButton.Glyph.LoadFromResourceName(HINSTANCE, C_Res_Open)
  else FButton.Glyph.LoadFromResourceName(HINSTANCE, C_Res_Shut);

  if (Assigned(FOnPasswordExecute)) then FOnPasswordExecute(Sender);
end;


procedure Register;
begin
  RegisterComponents('Wieser', [TPasswordButton]);
{$IFNDEF VER150}
  RegisterPropertyEditor(TypeInfo(string), TPasswordButton,
    'DatabaseName', TAllDatabaseNamesProperty);
{$ENDIF}
end;

end.
