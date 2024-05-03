//------------------------------------------------------------------------------
// Default-Fenster für Assistenten
//
// 05.10.2011  GD  Neu
//
// Copyright (C) RMG Messtechnik GmbH 2011
//------------------------------------------------------------------------------
unit FAssistentDefault;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  WGraphics;

type
  TFormAssistentDefault = class(TForm)
    pnClient: TPanel;
    bbtnGoOn: TBitBtn;
    bbtnCancel: TBitBtn;
    Panel1: TPanel;
    Memo: TMemo;
    Panel2: TPanel;
    Image: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    constructor Create(
      pOwner: TComponent; const sCaption, sText: string); reintroduce;
  end;

implementation

{$R *.dfm}

// Konstruktor
// Parameter: Owner (vererbt), Fenstertitel, Anzeigetext
//--------------------------------------------------
constructor TFormAssistentDefault.Create(
  pOwner: TComponent; const sCaption, sText: string);
//--------------------------------------------------
begin
  inherited Create(pOwner);

  if (sCaption <> '') then Self.Caption := sCaption
  else if (Self.Caption = '') then Self.Caption := Application.Title;
  Memo.Text := sText;
end;

// Ereignisse bei Erstellung
//--------------------------------------------------
procedure TFormAssistentDefault.FormCreate(Sender: TObject);
//--------------------------------------------------
var
  b : TBitmap;
begin
  // Anzeige des Programm-Icons
  b := IconToBitmap(Application.Icon);
  try
    Image.Picture.Bitmap := b;
  finally
    b.Free;
  end;
end;

end.
