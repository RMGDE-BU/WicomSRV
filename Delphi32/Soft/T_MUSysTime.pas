unit T_MUSysTime;

interface

uses
  SysUtils, Types,
  GD_Utils;

type
  TByteArray8 = array [0..7] of byte;

function SysTimeStringToBuffer(const sSysTime: string): TByteArray8;
function BufferToSysTimeString(pBuffer: TByteArray8): string;
function EncodeSysTime(iUnix: LongWord): TByteArray8;
function EncodeSysTimeStr(const sUnix: string): string;
function DecodeSysTime(pBuffer: TByteArray8): Longword;
function DecodeSysTimeStr(const sBuffer: string): string;

implementation

type

  TCCTAB = record
    iSum5   : byte;						 		// Summand für niedrigste Stelle
    pIndS   : array [0..7] of byte; 		// AblageIndex der Stelle
    pCC     : array [0..15] of char;	  // Code einer Stelle
  end;

const
  C_CCTab : array [0..3] of TCCTAB =
    (
      (
        iSum5: $23;
        pIndS: (5,6,7,4,1,3,0,2);
        pCC: ('u','b','r','#','F','!','H','_','Z','\','O','~','z','e','N','o')
      ),
      (
        iSum5: $3a;
        pIndS: (5,1,4,6,2,7,0,3);                                
        pCC: (']','''','x','8','i','p','s','*','K','-','>','G','q','6','v','P')      // ' statt \ in pCC[1]; 08.08.2012, WW
      ),
      (
        iSum5: $50;
        pIndS: (5,3,0,1,7,2,4,6);
        pCC: (',','@','"','$','4','`','<','%','{','R','3','C',')','I','^','9')
      ),
      (
        iSum5: $68;
        pIndS: (5,2,7,4,1,6,0,3);
        pCC: ('2','w','U','&',';','S','n','f','h','Q','=','/','L','a','m','V')
      )
    );

// Zum Codieren: welche Ziffer an Position 5 welche ccTab[] verwendet.
// 0 verwendet ccTab[1], 1 ccTab[2], 2 ccTab[0] ... 0xf ccTab[2]
  C_No2Tab: array [0..15] of byte = (1,2,0,2,3,1,2,0,3,0,1,0,3,1,3,2);

// 'Schiebzahl'
  C_SZ: array [0..7] of byte = (0,4,8,12,16,20,24,28);

function SysTimeStringToBuffer(const sSysTime: string): TByteArray8;
var
  i : byte;
begin
  for i := 0 to 7 do begin
    if (Length(sSysTime) >= i+1)
    then Result[i] := Ord(sSysTime[i+1])
    else Result[i] := 0;
  end;
end;

function BufferToSysTimeString(pBuffer: TByteArray8): string;
var
  i : byte;
begin
  Result := '';
  for i := 0 to 7 do Result := Result + Char(pBuffer[i]);
end;

// SysTime in st codieren nach buf[]
function EncodeSysTime(iUnix: LongWord): TByteArray8;
var
  c, i, t, j : byte;
begin
	c := iUnix and $F;									// niedrigste Stelle
	t := C_No2Tab[c];	     							// Index auf die zu verwendente ccTab
//	pBuffer[5] := (c+=ccTab[t].sum5);  		// niedrigste Stelle ASCII ablegen
  c := c + C_CCTab[t].iSum5;
	Result[5] := c;        		// niedrigste Stelle ASCII ablegen
	for i := 1 to 7 do begin
    iUnix := iUnix shr 4;
		c := iUnix and $F;								// momentan niedrigste Stelle
		j := C_CCTab[t].pIndS[i];					// Ablageindex
		Result[j] := Ord(C_CCTab[t].pCC[c]);				// codiert ablegen
  end;
end;

function EncodeSysTimeStr(const sUnix: string): string;
begin
  Result := BufferToSysTimeString(EncodeSysTime(StrToIntDef('$' + sUnix, 0)));
end;

// SysTime (oder auch nicht!) in buf[] decodieren
// Rückgabe 0: fehlerhaft!, sonst UNIX-Zeit
function DecodeSysTime(pBuffer: TByteArray8): Longword;
var
  c, i, t, j : byte;
  iStd       : LongWord;
begin
	c := pBuffer[5];
	for t := 0 to 3 do begin							// aus Zeichen an Position verwendete ccTab[] ermitteln
		if ((c-C_CCTab[t].iSum5) <= $F) then begin
			c := c - (C_CCTab[t].iSum5);
			Break;
    end;
  end;
  // nicht gefunden!  	// keine Ziffern-korrekte ccTab!
	if (c > $F) or (t <> C_No2Tab[c]) then begin
    Result := 0;
    Exit;
  end;

	iStd := c;											// niedrigste 'Stelle' in st
  i := 0;
  while (i < 8) do begin
    for j := 0 to 15 do begin   // 'Wert' suchen
      c := pBuffer[i];
			if (Ord(C_CCTab[t].pCC[j]) = c) then begin
				c := j;								// Wert ist Offset auf ASCII-Wert!
				Break;
			end;
    end;

		if (c > $F) then begin    // nicht gefunden!!
      Result := 0;
      Exit;
    end;

		for j := 0 to 15 do begin         	// Position suchen
			if (C_CCTab[t].pIndS[j] = i) then Break;
    end;
		iStd := iStd or (DWORD(c) shl C_SZ[j]);
		Inc(i);
		if (i = 5) then Inc(i);
  end;
	Result := iStd;
end;

function DecodeSysTimeStr(const sBuffer: string): string;
begin
  Result := IntToHex(DecodeSysTime(SysTimeStringToBuffer(sBuffer)), 8);
end;

end.
