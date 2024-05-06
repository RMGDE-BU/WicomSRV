{-------------------------------------------------------------------}
{ unit ObjData                                                      }
{ 25.09.1996 DA, Copyright Karl Wieser GmbH                         }
{ Definitionen von erweiterten Datentypen                           }
{ 20.01.2006 WW, resourcestrings                                    }
{-------------------------------------------------------------------}

unit Objdata;

interface

uses
  SysUtils;

type

  EDataException = class (Exception);

  TData = class (TObject)
  private
    FModified: Boolean;
    FNull: Boolean;
    procedure CheckNull;
  protected
    function GetAsString: string; virtual; abstract;
    procedure SetAsString (const Value: String); virtual; abstract;
    function GetAsInteger: Longint; virtual; abstract;
    procedure SetAsInteger (Value: LongInt); virtual; abstract;
    function GetAsBoolean: Boolean; virtual; abstract;
    procedure SetAsBoolean (Value: Boolean); virtual; abstract;
    function GetAsFloat: Double; virtual; abstract;
    procedure SetAsFloat (Value: Double); virtual; abstract;
    procedure RemoveData; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure ClearModified; virtual;
    property asString: String read GetAsString write SetAsString;
    property asInteger: LongInt read GetAsInteger write SetAsInteger;
    property asBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property asFloat: Double read GetAsFloat write SetAsFloat;
    property Modified: Boolean read FModified write FModified;
    property Null: Boolean read FNull;
  end;

  TStringData = class (TData)
  protected
    FValue: PString;
    FMaxLen: Byte;
    function GetAsString: string; override;
    procedure SetAsString (const Value: String); override;
    function GetAsInteger: Longint; override;
    procedure SetAsInteger (Value: LongInt); override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean (Value: Boolean); override;
    function GetAsFloat: Double; override;
    procedure SetAsFloat (Value: Double); override;
    procedure SetMaxLen (Value: Byte);
    procedure RemoveData; override;
  public
    constructor Create;
    constructor CreateRange (AMaxLen: Byte);
    property MaxLen: Byte read FMaxLen write SetMaxLen;
  end;

  TXStringData = class (TStringData)
  protected
    procedure SetAsString (const Value: String); override;
  private
    TruncChar: Char;
    function GetTruncString (const Value: String): string;
  public
    constructor Create (ATruncChar: Char);
    constructor CreateRange (AMaxLen: Byte; ATruncChar: Char);
  end;

  TIntegerData = class (TData)
  private
    FValue: Longint;
    FMin: LongInt;
    FMax: LongInt;
  protected
    function GetAsString: string; override;
    procedure SetAsString (const Value: string); override;
    function GetAsInteger: Longint; override;
    procedure SetAsInteger (Value: LongInt); override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean (Value: Boolean); override;
    function GetAsFloat: Double; override;
    procedure SetAsFloat (Value: Double); override;
    procedure SetMin (Value: LongInt);
    procedure SetMax (Value: LongInt);
  protected
    procedure RemoveData; override;
  public
    constructor Create;
    constructor CreateRange (AMin, AMax: Longint);
    property Min: LongInt read FMin write SetMin;
    property Max: LongInt read FMax write SetMax;
  end;

  TBooleanData = class (TData)
  private
    FValue: Boolean;
  protected
    function GetAsString: string; override;
    procedure SetAsString (const Value: string); override;
    function GetAsInteger: Longint; override;
    procedure SetAsInteger (Value: LongInt); override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean (Value: Boolean); override;
    function GetAsFloat: Double; override;
    procedure SetAsFloat (Value: Double); override;
    procedure RemoveData; override;
  public
    constructor Create;
  end;

  TNumericData = class (TData)
  private
    FValue: Double;
    FMin: Double;
    FMax: Double;
    procedure SetMin (Value: Double);
    procedure SetMax (Value: Double);
  protected
    function GetAsString: string; override;
    procedure SetAsString (const Value: string); override;
    function GetAsInteger: Longint; override;
    procedure SetAsInteger (Value: LongInt); override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean (Value: Boolean); override;
    function GetAsFloat: Double; override;
    procedure SetAsFloat (Value: Double); override;
    procedure RemoveData; override;
  public
    constructor Create;
    constructor CreateRange (AMin, AMax: Double);
    property Min: Double read FMin write SetMin;
    property Max: Double read FMax write SetMax;
  end;

implementation
                                                   
resourcestring
  CE_IsNull     = 'Wert nicht vorhanden';
  CE_Range      = 'Bereichsgrenzenänderung nicht zulässig';
  CE_OutofRange = 'Bereichsüberschreitung';

const
  STrue  = 'True';
  SFalse = 'False';

{ TData }

procedure TData.CheckNull;
begin
  if FNull then
    raise EDataException.Create (CE_IsNull);
end;

procedure TData.RemoveData;
begin
end;

constructor TData.Create;
begin
  inherited Create;
  FNull := True;
  FModified := True;
end;

destructor TData.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TData.Clear;
begin
  FNull := True;
  FModified := True;
  RemoveData;
end;

procedure TData.ClearModified;
begin
  FModified := False;
end;

{ TStringData }

procedure TStringData.RemoveData;
begin
  DisposeStr (FValue);
  FValue := nil;
end;

function TStringData.GetAsString: string;
begin
  if Null then
    Result := ''
  else
    Result := FValue^;
end;

procedure TStringData.SetAsString (const Value: String);
begin
  Clear;
  if Length (Value) > FMaxLen then
    raise EDataException.Create (CE_OutofRange);
  if Length (Value) > 0 then
  begin
    FValue := NewStr (Value);
    FNull := False;
  end;
end;

function TStringData.GetAsInteger: Longint;
begin
  CheckNull;
  result := StrToInt (FValue^);
end;

procedure TStringData.SetAsInteger (Value: LongInt);
begin
  SetAsString (IntToStr (Value));
end;

function TStringData.GetAsBoolean: Boolean;
begin
  CheckNull;
  if CompareText (FValue^, STrue) = 0 then
    result := True
  else if CompareText (FValue^, SFalse) = 0 then
    result := False
  else
    raise EDataException.Create (CE_OutofRange);
end;

procedure TStringData.SetAsBoolean (Value: Boolean);
begin
  if Value then
    SetAsString (STrue)
  else
    SetAsString (SFalse);
end;

function TStringData.GetAsFloat: Double;
begin
  CheckNull;
  result := StrToFloat (FValue^);
end;

procedure TStringData.SetAsFloat (Value: Double);
begin
  SetAsString (FloatToStr (Value));
end;

procedure TStringData.SetMaxLen (Value: Byte);
begin
  if not FNull then
    raise EDataException.Create (CE_Range);
  FMaxLen := Value;
end;

constructor TStringData.Create;
begin
  inherited Create;
  FValue := nil;
  FMaxLen := 255;
end;

constructor TStringData.CreateRange (AMaxLen: Byte);
begin
  inherited Create;
  FValue := nil;
  FMaxLen := AMaxLen;
end;

{ TXStringData }

procedure TXStringData.SetAsString (const Value: String);
begin
  inherited SetAsString (GetTruncString (Value));
end;

function TXStringData.GetTruncString (const Value: String): string;
begin
  Result := Value;
  while (Length (Result) > 0) and (Result [1] = TruncChar) do
    Result := copy (Result, 2, Length (Result) - 1);
  while (Length (Result) > 0) and (Result [Length (Result)] = TruncChar) do
    Result := copy (Result, 1, Length (Result) - 1);
end;

constructor TXStringData.Create (ATruncChar: Char);
begin
  inherited Create;
  TruncChar := ATruncChar;
end;

constructor TXStringData.CreateRange (AMaxLen: Byte; ATruncChar: Char);
begin
  inherited CreateRange (AMaxLen);
  TruncChar := ATruncChar;
end;

{ TIntegerData }

function TIntegerData.GetAsString: string;
begin
  CheckNull;
  result := IntToStr (FValue);
end;

procedure TIntegerData.SetAsString (const Value: string);
begin
  Clear;
  if Length (Value) > 0 then
    SetAsInteger (StrToInt (Value));
end;

function TIntegerData.GetAsInteger: LongInt;
begin
  CheckNull;
  result := FValue;
end;

procedure TIntegerData.SetAsInteger (Value: Longint);
begin
  Clear;
  if (Value < FMin) or (Value > FMax) then
    raise EDataException.Create (CE_OutofRange);
  FValue := Value;
  FNull := False;
end;

function TIntegerData.GetAsBoolean: Boolean;
begin
  CheckNull;
  result := (FValue <> 0);
end;

procedure TIntegerData.SetAsBoolean (Value: Boolean);
begin
  if Value then
    SetAsInteger (1)
  else
    SetAsInteger (0);
end;

function TIntegerData.GetAsFloat: Double;
begin
  result := GetAsInteger;
end;

procedure TIntegerData.SetAsFloat (Value: Double);
begin
  SetAsInteger (Trunc (Value));
end;

procedure TIntegerData.SetMin (Value: LongInt);
begin
  if not FNull then
    raise EDataException.Create (CE_Range);
  FMin := Value;
end;

procedure TIntegerData.SetMax (Value: LongInt);
begin
  if not FNull then
    raise EDataException.Create (CE_Range);
  FMax := Value;
end;

procedure TIntegerData.RemoveData;
begin
  FValue := 0;
end;

constructor TIntegerData.Create;
begin
  inherited Create;
  FValue := 0;
  FMin := - MaxLongint - 1;
  FMax := MaxLongint;
end;

constructor TIntegerData.CreateRange (AMin, AMax: Longint);
begin
  inherited Create;
  FValue := 0;
  FMin := AMin;
  FMax := AMax;
end;

{ TBooleanData }

function TBooleanData.GetAsString: string;
begin
  CheckNull;
  if FValue then
    result := STrue
  else
    result := SFalse;
end;

procedure TBooleanData.SetAsString (const Value: string);
begin
  Clear;
  if CompareText (Value, STrue) = 0 then
  begin
    FValue := True;
    FNull := False;
  end
  else if CompareText (Value, SFalse) = 0 then
  begin
    FValue := False;
    FNull := False;
  end
  else
    raise EDataException.Create (CE_OutofRange);
end;

function TBooleanData.GetAsInteger: Longint;
begin
  CheckNull;
  if FValue then
    result := 1
  else
    result := 0;
end;

procedure TBooleanData.SetAsInteger (Value: LongInt);
begin
  Clear;
  FValue := (Value <> 0);
  FNull := False;
end;

function TBooleanData.GetAsBoolean: Boolean;
begin
  if Null then
    Result := False
  else
    Result := FValue;
end;

procedure TBooleanData.SetAsBoolean (Value: Boolean);
begin
  Clear;
  FValue := Value;
  FNull := False;
end;

function TBooleanData.GetAsFloat: Double;
begin
  result := GetAsInteger;
end;

procedure TBooleanData.SetAsFloat (Value: Double);
begin
  Clear;
  FValue := (Value <> 0);
  FNull := False;
end;

procedure TBooleanData.RemoveData;
begin
  FValue := False;
end;

constructor TBooleanData.Create;
begin
  inherited Create;
  FValue := False;
end;

{ TNumericData }

function TNumericData.GetAsString: string;
begin
  CheckNull;
  result := FloatToStr (FValue);
end;

procedure TNumericData.SetAsString (const Value: string);
begin
  Clear;
  if Length (Value) > 0 then
    SetAsFloat (StrToFloat (Value));
end;

function TNumericData.GetAsInteger: Longint;
begin
  CheckNull;
  result := Trunc (FValue);
end;

procedure TNumericData.SetAsInteger (Value: LongInt);
begin
  SetAsFloat (Value);
end;

function TNumericData.GetAsBoolean: Boolean;
begin
  CheckNull;
  result := (FValue <> 0);
end;

procedure TNumericData.SetAsBoolean (Value: Boolean);
begin
  Clear;
  if Value then
    FValue := 0
  else
    FValue := 1;
  FNull := False;
end;

function TNumericData.GetAsFloat: Double;
begin
  CheckNull;
  result := FValue;
end;

procedure TNumericData.SetAsFloat (Value: Double);
begin
  Clear;
  if (Value < FMin) or (Value > FMax) then
    raise EDataException.Create (CE_OutofRange);
  FValue := Value;
  FNull := False;
end;

procedure TNumericData.SetMin (Value: Double);
begin
  if not FNull then
    raise EDataException.Create (CE_Range);
  FMin := Value;
end;

procedure TNumericData.SetMax (Value: Double);
begin
  if not FNull then
    raise EDataException.Create (CE_Range);
  FMax := Value;
end;

procedure TNumericData.RemoveData;
begin
  FValue := 0;
end;

constructor TNumericData.Create;
begin
  inherited Create;
  FValue := 0;
  FMin := -1E100;
  FMax := +1E100;
end;

constructor TNumericData.CreateRange (AMin, AMax: Double);
begin
  inherited Create;
  FValue := 0;
  FMin := AMin;
  FMax := AMax;
end;

end.

