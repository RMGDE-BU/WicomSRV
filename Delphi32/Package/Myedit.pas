{ 22.04.1999 GD; TypeCasting zum Unterdrücken von Warnungen                    }
{ 13.12.2005 WW; ohne Property-Editoren bei Delphi 7, um Programme ohne Lauf-  }
{                zeitpackages ausführen zu können                              }
{ 20.01.2006 WW; resourcestrings                                               }
unit MYEdit;

interface            

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Spin, DBCtrls, DB;

type
  TValidChar   = String;
  TMyEdit = Class(TEdit)
    Private
      FAlignment: TAlignment;
      FOnReturn: TNotifyEvent;
      FOnEscape: TNotifyEvent;
      FOnEnoughChars: TNotifyEvent;

      FRequired    : Boolean;
      FRequiredDialog: Boolean;
      FRequiredColor: TColor;
      FBeepInvalid : Boolean;
      FCheckEnoughChars: boolean;
      FValidch     : TValidChar;
      ValidChar    : set of char;
      FAbout       : string;

    Protected
      procedure CreateParams(var Params: TCreateParams); override;
      procedure SetAlignment(Value: TAlignment);
      procedure KeyPress(var Key: Char); override;

      Procedure SetValidCh(Value:TValidChar);
      Procedure SetRequired(Value:Boolean);
      Procedure SetBeepInvalid(Value:Boolean);
      Procedure DoEnter; override;
      Procedure DoExit; override;

    Public
      constructor Create(AOwner: TComponent); override;

    Published
      property About: string read FAbout write FAbout;
      property Alignment: TAlignment read FAlignment write SetAlignment;
      property RequiredColor: TColor read FRequiredColor write  FRequiredColor stored true;
      property OnReturn: TNotifyEvent read FOnReturn write FOnReturn;
      property OnEscape: TNotifyEvent read FOnEscape write FOnEscape;
      property OnEnoughChars: TNotifyEvent read FOnEnoughChars write FOnEnoughChars;

      Property ValidChars:TValidChar read FValidCh Write SetValidCh stored true;
      Property RequiredField:Boolean read FRequired write SetRequired stored true;
      property RequiredDialog: boolean read FRequiredDialog write FRequiredDialog;
      Property BeepOnInvalid:Boolean read FBeepInvalid write SetBeepInvalid stored true;
      Property CheckEnoughChars: boolean read FCheckEnoughChars write FCheckEnoughChars stored true;

    end;


  TDBMyEdit = Class(TDBEdit)
    Private
      FAlignment: TAlignment;
      FOnReturn: TNotifyEvent;
      FOnEscape: TNotifyEvent;
      FOnEnoughChars: TNotifyEvent;

      FRequired    : Boolean;
      FRequiredDialog: Boolean;
      FRequiredColor: TColor;
      FBeepInvalid : Boolean;
      FCheckEnoughChars: boolean;
      FValidch     : TValidChar;
      ValidChar    : set of char;
      FAbout       : string;

    Protected
      procedure CreateParams(var Params: TCreateParams); override;
      procedure SetAlignment(Value: TAlignment);
      procedure KeyPress(var Key: Char); override;

      Procedure SetValidCh(Value:TValidChar);
      Procedure SetRequired(Value:Boolean);
      Procedure SetBeepInvalid(Value:Boolean);
      Procedure DoEnter; override;
      Procedure DoExit; override;

    Public
      constructor Create(AOwner: TComponent); override;

    Published
      property About: string read FAbout write FAbout;
      property Alignment: TAlignment read FAlignment write SetAlignment;
      property RequiredColor: TColor read FRequiredColor write  FRequiredColor stored true;
      property OnReturn: TNotifyEvent read FOnReturn write FOnReturn;
      property OnEscape: TNotifyEvent read FOnEscape write FOnEscape;
      property OnEnoughChars: TNotifyEvent read FOnEnoughChars write FOnEnoughChars;

      Property ValidChars:TValidChar read FValidCh Write SetValidCh stored true;
      Property RequiredField:Boolean read FRequired write SetRequired stored true;
      Property BeepOnInvalid:Boolean read FBeepInvalid write SetBeepInvalid stored true;
      Property CheckEnoughChars: boolean read FCheckEnoughChars write FCheckEnoughChars stored true;

    end;


  TDBSpinEdit = class(TSpinEdit)
  private
{    FMinValue: longint;
    FMaxValue: longint; }
    FIncrement: longint;
    FReadOnly: boolean;
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const VDataField: string);
    procedure SetDataSource(VDataSource: TDataSource);
    procedure DataChange(Sender: TObject);
{    procedure SetValue(NewValue: longint);
    function CheckValue(NewValue: longint): longint;}
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure UpdateData(Sender: TObject);

  protected
    procedure DownClick(Sender: TObject); override;
    procedure UpClick(Sender: TObject); override;
    function IsValidChar(Key: Char): boolean; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;


procedure Register;

implementation

{$IFNDEF VER150}
uses
  DsgnIntf;

type
  THelpProperty = Class(TStringProperty)
    protected
      procedure InternalEdit( const HelpFile: string );
    public
      function GetAttributes: TPropertyAttributes; override;
//     function GetValue: string; override;
    end;

  TMyEditHelp = Class(THelpProperty)
  public
    procedure Edit; override;
  end;


procedure THelpProperty.InternalEdit( const HelpFile: string );
var cmdLine: array[0..79] of char;
begin
  if HelpFile<>'' then begin
    StrPCopy(cmdLine,'WINHELP V:\DELPHI32\PACKAGE\' + HelpFile);
    WinExec(cmdLine, SW_SHOW);
  end;
end;


function THelpProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;


//function THelpProperty.GetValue: string;
//begin
//  result := 'Click for Help ... ';
//end;


procedure TMyEditHelp.Edit;
begin
  InternalEdit('MYEDIT.HLP');
end;
{$ENDIF}

resourcestring
  SEditLeer     = 'Das Eingabefeld ist leer !';
  SEditPruefung = 'Eingabeüberprüfung';


constructor TMyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  ControlStyle := [csClickEvents, csSetCaption, csFramed];               { auskommentiert, damit gleich zu TEdit   WW }
  Autosize := False;
  FAlignment := taLeftJustify;
  FValidCh:='[]';
  FRequired:=False;
  FRequiredColor := Color;
  FBeepInvalid := False;
  FRequiredDialog := True;
  FCheckEnoughChars := False;
end;



 procedure TMyEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Longint = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'MYEDIT');
  { auskommentiert, damit Selektion funktioniert: }
  Params.Style := Params.Style {or ES_MULTILINE} or
                  DWORD(Alignments[FAlignment]); { GeDa }
end;


procedure TMyEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value
    then begin
      FAlignment := Value;
      RecreateWnd;
    end;
end;


Procedure TMyEdit.SetRequired(Value:Boolean);
Begin
  if Value<>FRequired
    then begin
      FRequired := Value;
      end;
end;


Procedure TMyEdit.SetBeepInvalid(Value:Boolean);
Begin
  if Value<>FBeepInvalid then
    FBeepInvalid := Value;
end;


procedure TMyEdit.DoExit;
var tmpColor: TColor;
begin
  inherited DoExit;
  if FRequired then begin
    if (Text = '') and FRequiredDialog then Begin
        Application.MessageBox(pchar(SEditLeer), pchar(SEditPruefung), MB_OK OR MB_ICONERROR);
        SetFocus;
        exit;
    end;
    if FRequiredColor <> Color then begin
      tmpColor := Color;
      Color := FRequiredColor;
      FRequiredColor := tmpColor;
    end;
  end;
end;


Procedure TMyEdit.DoEnter;
var tmpColor: TColor;
begin
  if FRequired then begin
    if FRequiredColor <> Color then begin
      tmpColor := Color;
      Color := FRequiredColor;
      FRequiredColor := tmpColor;
    end;
  end;
  inherited DoEnter;
end;


Procedure TMyEdit.SetValidCh(Value:TValidChar);
var vc3 :  set of char;
    x   :  integer;
    ch1,ch2,ch3 : char;
    SetRange    :  boolean;

  {Internal Procedure}
  Procedure MakeSet;
  Begin
    if SetRange then vc3:=[ch1..ch2]
    else
    vc3:=[ch1];
    validchar:=validchar+vc3;
    ch1:=#0;ch2:=#0;ch3:=#0;
    SetRange:=False;
  End;


Begin {SetValidCh}
  SetRange:=False;
  if Value<>FValidCh then BEGIN
    FValidCh:=Value;
    if (FValidCh[1]='[') and
      (FValidCh[length(FValidCh)]=']' )then begin
      x:=2;
      While x <= length(FValidCh) do
      begin
        ch3:=FValidCh[x];
        if (ch3=',') or (ch3=']') then
        begin
          MakeSet;
        end;
        if ch3='.' then SetRange:=True;
        if ch3='''' then
        begin
          ch3:=FValidCh[x+1];
          inc(x, 2);
        end;
        if (SetRange=False) then ch1:=ch3
        else ch2:=ch3;
        inc(x);
      end;
    end
    else
      FValidCh:='Invalid Format '+ FValidCh;
  end;
end;


procedure TMyEdit.KeyPress(var Key: char);
var i: integer;
begin
  Case Key of
    #13: begin
           if Assigned(FOnReturn)
             then FOnReturn(Self);
           Key:=#0;
           end;
    #27: begin
           if Assigned(FOnEscape)
             then FOnEscape(Self);
           Key:=#0;
           end;
    else begin
      try
        if ValidChar<>[]
          then if (NOT (Key in ValidChar)) and (Key<>#08)
            then begin
              if FBeepInvalid
                then messagebeep(0);
              Key:=#0;
              end;
      except
        on e:exception do
          MessageDlg('MyEdit ValidChars: ' +e.Message, mtError, [mbOK], 0);
      end;

      try
        Inherited KeyPress(Key);
      except
        on e:exception do
          MessageDlg('MyEdit Inherited KeyPress: ' +e.Message, mtError, [mbOK], 0);
      end;

      try

        if (key<>#0) and (key<>#08)
          then if FCheckEnoughChars and (MaxLength>0)
            then begin

              i := length(Text) - Length(SelText);
              if (i+1=MaxLength)
                then if Assigned(FOnEnoughChars)
                  then begin
                    Text := Text + key;
                    FOnEnoughChars(Self);
                    end;

            end; (* if (key<>#0) and (key<>#08) and (MaxLength>0) and ... *)

      except
        on e:exception do
          MessageDlg('MyEdit KeyPress: ' +e.Message, mtError, [mbOK], 0);
      end;

      end;
    end;
end;


constructor TDBMyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  ControlStyle := [csClickEvents, csSetCaption, csFramed];             { auskommentiert, damit gleich zu TDBEdit   WW }
  Width := 60;
  Height := 25;
  TabStop := True;
  ParentColor := False;
  Autosize := False;
  BorderStyle := bsSingle;
  FAlignment := taLeftJustify;
  ParentFont := True;
  FValidCh:='[]';
  FRequired:=False;
  FRequiredColor := Color;
  FBeepInvalid := False;
  FCheckEnoughChars := False;
end;


procedure TDBMyEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Longint = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'MYDBEDIT');
  { auskommentiert, damit Selektion funktioniert: }
  Params.Style := Params.Style {or ES_MULTILINE} or
                  DWORD(Alignments[FAlignment]); { GeDa }
end;


procedure TDBMyEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value
    then begin
      FAlignment := Value;
      RecreateWnd;
    end;
end;


Procedure TDBMyEdit.SetRequired(Value:Boolean);
var tmpColor: TColor;
Begin
  if Value<>FRequired
    then begin
      FRequired := Value;
      tmpColor := Color;
      Color := FRequiredColor;
      FRequiredColor := tmpColor;
      end;
end;


Procedure TDBMyEdit.SetBeepInvalid(Value:Boolean);
Begin
  if Value<>FBeepInvalid then
    FBeepInvalid := Value;
end;


Procedure TDBMyEdit.DoEnter;
var tmpColor: TColor;
begin
  if FRequired
    then begin
      tmpColor := Color;
      Color := FRequiredColor;
      FRequiredColor := tmpColor;
      end;
  inherited DoEnter;
end;


procedure TDBMyEdit.DoExit;
var tmpColor: TColor;
begin
  inherited DoExit;
  if FRequired
    then begin
      if (Text = '') and FRequiredDialog
        then Begin
          MessageDlg(SEditLeer, mtWarning, [mbOK],0);
          SetFocus;
        end;
      tmpColor := Color;
      Color := FRequiredColor;
      FRequiredColor := tmpColor;
    end;
end;


Procedure TDBMyEdit.SetValidCh(Value:TValidChar);
var vc3 :  set of char;
    x   :  integer;
    ch1,ch2,ch3 : char;
    SetRange    :  boolean;

  {Internal Procedure}
  Procedure MakeSet;
  Begin
    if SetRange then vc3:=[ch1..ch2]
    else
    vc3:=[ch1];
    validchar:=validchar+vc3;
    ch1:=#0;ch2:=#0;ch3:=#0;
    SetRange:=False;
  End;


Begin {SetValidCh}
  SetRange:=False;
  if Value<>FValidCh then BEGIN
    FValidCh:=Value;
    if (FValidCh[1]='[') and
      (FValidCh[length(FValidCh)]=']' )then begin
      x:=2;
      While x <= length(FValidCh) do begin
        ch3:=FValidCh[x];
        if (ch3=',') or (ch3=']') then
        begin
          MakeSet;
        end;
        if ch3='.' then SetRange:=True;
        if ch3='''' then
        begin
          ch3:=FValidCh[x+1];
          inc (x, 2);
        end;
        if (SetRange=False) then ch1:=ch3
        else ch2:=ch3;
        inc (x);
      end;
    end
    else
      FValidCh:='Invalid Format '+ FValidCh;
  end;
end;


procedure TDBMyEdit.KeyPress(var Key: char);
var i: integer;
begin
  Case Key of
    #13: begin
           if Assigned(FOnReturn)
             then FOnReturn(Self);
           Key:=#0;
           end;
    #27: begin
           if Assigned(FOnEscape)
             then FOnEscape(Self);
           Key:=#0;
           end;
    else begin
      if ValidChar<>[]
        then if (NOT (Key in ValidChar)) and (Key<>#08)
          then begin
            if FBeepInvalid
              then messagebeep(0);
            Key:=#0;
            end;

      Inherited KeyPress(Key);

      try

        if (key<>#0) and (key<>#08)
          then if FCheckEnoughChars and (MaxLength>0)
            then begin

              i := length(Text) - Length(SelText);
              if (i+1>=MaxLength)
                then if Assigned(FOnEnoughChars)
                  then FOnEnoughChars(Self);

              end; (* if (key<>#0) and (key<>#08) and (MaxLength>0) ... *)

      except
        on e:exception do
          MessageDlg('DBMyEdit KeyPress: ' +e.Message, mtError, [mbOK], 0);
      end;

      end;
    end;
end;


constructor TDBSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIncrement := 1;
  FDataLink := TFieldDataLink.Create;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TDBSpinEdit.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.Free;
  inherited Destroy;
end;


function TDBSpinEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBSpinEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBSpinEdit.SetDatafield(const VDatafield: string);
begin
  FDataLink.FieldName := VDatafield;
end;

procedure TDBSpinEdit.SetDataSource(VDataSource: TDataSource);
begin
  FDataLink.DataSource := VDataSource;
end;

procedure TDBSpinEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field = nil then Value := 0
  else Value := FDataLink.Field.AsInteger;
end;
{
procedure TDBSpinEdit.SetValue(NewValue: longint);
begin
  Text := IntToStr(CheckValue(NewValue));
  UpdateData(Self);
end;

function TDBSpinEdit.CheckValue(NewValue: longint): longint;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then
    begin
    if NewValue < FMinValue then Result := FMinValue
    else if NewValue > FMaxValue then Result := FMaxvalue;
    end;
end;   }

procedure TDBSpinEdit.CMEnter(var Message: TCMGotFocus);
begin
  inherited;
  UpdateData(Self);
end;

procedure TDBSpinEdit.CMExit(var Message: TCMExit);
begin
  try
    UpdateData(Self);
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TDBSpinEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Modified;
  FDataLink.Edit;
  FDataLink.Field.AsInteger := Value;
end;

procedure TDBSpinEdit.DownClick(Sender: TObject);
begin
  if ReadOnly then MessageBeep(0)
  else begin
    Value := Value - FIncrement;
    UpdateData(Self);
    end;
end;

procedure TDBSpinEdit.UpClick(Sender: TObject);
begin
  if ReadOnly then MessageBeep(0)
  else begin
    Value := Value + FIncrement;
    UpdateData(Self);
    end;
end;

function TDBSpinEdit.IsValidChar(Key: Char): boolean;
begin
  Result := false;
  if ReadOnly then MessageBeep(0)
  else Result:= inherited IsValidChar(Key);
  if Result then UpdateData(Self);
end;


procedure Register;
begin
  RegisterComponents('Wieser',[TMyEdit]);
  RegisterComponents('Wieser',[TDBMyEdit,TDBSpinEdit]);
{$IFNDEF VER150}
  RegisterPropertyEditor(TypeInfo(string), TMyEdit, 'About', TMyEditHelp);
  RegisterPropertyEditor(TypeInfo(string), TDBMyEdit, 'About', TMyEditHelp);
{$ENDIF}
end;

end.
