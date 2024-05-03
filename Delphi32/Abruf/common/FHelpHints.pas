{------------------------------------------------------------------------------}
{ Hilfefenster                                                                 }
{                                                                              }
{ -> Benötigt DSta.DLL                                                         }
{                                                                              }
{ 28.05.2001  GD  Neu                                                          }
{ 26.10.2007  WW  resourcestrings                                              }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001                                          }
{------------------------------------------------------------------------------}
unit FHelpHints;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, DStaDll;

type
  TFormHilfeTexte = class(TForm)
    pnBottom: TPanel;
    pnTop: TPanel;
    pnClient: TPanel;
    memoHelpText: TMemo;
    bbtnOk: TBitBtn;
    bbtnsave: TBitBtn;
    procedure bbtnsaveClick(Sender: TObject);
    procedure memoHelpTextChange(Sender: TObject);
    procedure bbtnOkClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FRefString : string;
  public
    { Public-Deklarationen }
    procedure ShowHelp(sSubject: string; pHelpList: TStrings);
    property RefString: string read FRefString write FRefString;
  end;

procedure ExecuteDeaHelpWindow(sSubject, sDea: string);

implementation

{$R *.DFM}

resourcestring
  S_SaveHelpText = 'Neuen Hilfetext abspeichern ?';
  S_Thema = 'Thema: ';
  S_ThemaKeinHilfeEintrag = 'Zu dem Thema "%s" ist bisher kein Hilfeeintrag vorhanden.';
  S_TextEingeben = 'Sie haben die Möglichkeit, hier einen Text ' +
    'einzugeben und diesen anschließend zu speichern.';


{---------------------------- Allgemeine Funktionen ---------------------------}

{ Zeigt Hilfe an                               }
{ Parameter: Thema, Dea                        }
{----------------------------------------------}
procedure ExecuteDeaHelpWindow(sSubject, sDea: string);
{----------------------------------------------}
var
  pSl1, pSl2 : TStrings;
begin
  pSl1 := ClientStammdaten.GetDeaHint(sDea);
  if (Assigned(pSl1)) then
  try
    // daten in 'lokale' Stringliste übertragen
    pSl2 := TStringList.Create;
    try
      pSl2.CommaText := pSl1.CommaText;

      with TFormHilfeTexte.Create(nil) do
      try
        RefString := sDea;
        ShowHelp(sSubject, pSl2);
        ShowModal;
      finally
        Free;
      end;
    finally
      pSl2.Free;
    end;

  finally
    pSl1.Free;
  end;
end;


{------------------------------ TFormHilfeTexte -------------------------------}

{ Zeigt Hilfe an                               }
{ Parameter: Thema, Stringliste mit Hilfetext  }
{----------------------------------------------}
procedure TFormHilfeTexte.ShowHelp(sSubject: string; pHelpList: TStrings);
{----------------------------------------------}
begin
  pnTop.Caption := S_Thema + sSubject;

  memoHelpText.Clear;
  if (pHelpList.Count = 0) then begin
    memoHelpText.Lines.Add(Format (S_ThemaKeinHilfeEintrag, [sSubject]));
    memoHelpText.Lines.Add('');
    memoHelpText.Lines.Add(S_TextEingeben);
  end
  else memoHelpText.Lines.Assign(pHelpList);
end;

{----------------------------------------------}
procedure TFormHilfeTexte.bbtnsaveClick(Sender: TObject);
{----------------------------------------------}
begin
  if (RefString <>  '') then
    ClientStammdaten.SetDeaHint(RefString, memoHelpText.Lines);
  bbtnsave.Enabled := False;
end;

{----------------------------------------------}
procedure TFormHilfeTexte.memoHelpTextChange(Sender: TObject);
{----------------------------------------------}
begin
  if (RefString <> '') then bbtnsave.Enabled := True;
end;

{----------------------------------------------}
procedure TFormHilfeTexte.bbtnOkClick(Sender: TObject);
{----------------------------------------------}
begin
  if (bbtnsave.Enabled) then
    if (MessageDlg(S_SaveHelpText, mtConfirmation, [mbYes, mbNo], 0) = mrYes)
      then bbtnsaveClick(Self);
end;

end.
