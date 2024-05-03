unit Endewin;

interface

uses
  Windows, Classes, Graphics, Controls, Forms, ExtCtrls, StdCtrls;

type
  TEndeFenster = class(TForm)
    Image: TImage;
    lText: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  EndeFenster: TEndeFenster;

Procedure EndeFensterZeigen(Icon: TIcon; Titel: string; Beschriftung: string);

implementation

{$R *.DFM}

procedure TEndeFenster.FormCreate(Sender: TObject);
begin
  Screen.Cursor:=crHourglass;
end;

Procedure EndeFensterZeigen(Icon: TIcon; Titel: string; Beschriftung: string);
begin
  EndeFenster:=TEndeFenster.Create(Application);
  EndeFenster.Image.Picture.Icon:=Icon;
  EndeFenster.Caption:=Titel;
  EndeFenster.lText.Caption:=Beschriftung;
  EndeFenster.Show;
  Application.ProcessMessages;
end;

end.
