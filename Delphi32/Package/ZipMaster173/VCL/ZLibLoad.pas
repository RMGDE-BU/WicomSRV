unit ZLibLoad;
(* Dll loader unit for ZipMaster 1.73
 by R. Peters
 edited 10 September 2003
				 2 October 2003
 *)

interface
uses
	classes, windows;

type
	TZipLibLoader = class(TObject)
	private
		hndl: HWND;
		ExecFunc: function(Rec: pointer): DWord; stdcall;
		VersFunc: function: DWord; stdcall;
		PrivFunc: function: DWord; stdcall;
    IsZip: boolean;
    FPath: string;
    FOwner: TObject;
		loadLevel: integer;
		Ver: integer;
		Priv: integer;
		TmpFileName: string;
		function GetLoaded: integer;
		function GetLoadedPath: string;
		function GetVer: integer;
		function GetPath: string;
		function GetPriv: integer;
	protected
        procedure Clear;
		function DoExec(Rec: pointer): integer;
		function DoLoad(level: integer): integer;
		function DoUnload(level: integer): integer;
		function Expand(src: string; dest: string): integer;
		function GetResDllPath: String;
		function LoadLib(FullPath: string; MustExist: Boolean ): integer;
	public
		constructor Create(master: TObject; zip: boolean);
		destructor Destroy; override;
		function LoadDll(minVer: integer; hold: boolean): integer; // return version
		procedure Unload(held: boolean);
		property Loaded: integer read GetLoaded;
		property Path: string read GetPath;
		property Version: integer read GetVer;
		property Build: integer read GetPriv;
  end;

implementation
uses
	ZipMsg, ZipMstr, SysUtils, LZExpand;
type
	TFriendZip = class(TZipMaster)
	end;

const             
	RType = 'BinFile';
	DllRes			 : array[false..true] of integer = (RDLL_Unz, RDLL_Zip);
	// new resource numbers  RDLL_ZVer= 11607, RDLL_UVer = 11608
	VerRes			 : array[false..true] of integer = (RDLL_UVer, RDLL_ZVer);
	DllNames     : array[false..true] of string = ('UNZDLL.DLL', 'ZIPDLL.DLL');
	ExecNames    : array[false..true] of string = ('UnzDllExec', 'ZipDllExec');
	VersNames    : array[false..true] of string = ('GetUnzDllVersion', 'GetZipDllVersion');
	PrivNames    : array[false..true] of string = ('GetUnzDllPrivVersion', 'GetZipDllPrivVersion');

                        
	zldTemp = 1;
	zldAuto = 2;
	zldFixed = 4;

procedure TZipLibLoader.Clear;
begin
	hndl := 0;
	ExecFunc := nil;
	VersFunc := nil;
	PrivFunc := nil;   
	Ver := 0;
	Priv := 0;
end;

constructor TZipLibLoader.Create(master: TObject; zip: boolean);
begin
	inherited Create;
{	hndl := 0;
	ExecFunc := nil;
	VersFunc := nil;
	PrivFunc := nil;}
	Clear;
	IsZip := zip;
	FOwner := master;
	loadLevel := 0;
//  Ver := 0;
  FPath := DllNames[IsZip];
  TmpFileName := '';
end;

destructor TZipLibLoader.Destroy;
begin
	if hndl <> 0 then
    FreeLibrary(hndl);
  hndl := 0;
	if (TmpFileName <> '') and FileExists(TmpFileName) then
		sysUtils.DeleteFile(TmpFileName);
	inherited;
end;

(*? TZipLibLoader.LoadDll
1.73 27 July 2003 RA / RP unload wrong version
*)
function TZipLibLoader.LoadDll(minVer: integer; hold: boolean): integer;
begin
  Result := 0;
  if hold then
		DoLoad(zldFixed)
  else
    DoLoad(zldAuto);
  if hndl <> 0 then
    Result := Ver;
	if Ver < minVer then
	begin
		DoUnload(zldFixed + zldAuto + zldTemp);
		raise EZipMaster.CreateResDrive( LD_BadDll, FPath);
	end;
end;
//? TZipLibLoader.LoadDll

(*? TZipLibLoader.LoadLib
1.73.2.4 10 Srptember 2003 RP new function
*)
function TZipLibLoader.LoadLib(FullPath: string; MustExist: Boolean): integer;
VAR
	oldMode      : cardinal;
begin
		if hndl>0 then
			FreeLibrary(hndl);
		Clear;
		oldMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
		try
			hndl := LoadLibrary(pChar(Fullpath));
			if hndl > HInstance_Error then
			begin
				@ExecFunc := GetProcAddress(hndl, pChar(ExecNames[IsZip]));
				@VersFunc := GetProcAddress(hndl, pChar(VersNames[IsZip]));
				@PrivFunc := GetProcAddress(hndl, pChar(PrivNames[IsZip]));
				FPath := GetLoadedPath;
			end;
		finally
			SetErrorMode(oldMode);
		end;
		if hndl <= HInstance_Error then
		begin
			Clear;
			LoadLevel := 0;
			if MustExist then
				raise EZipMaster.CreateResDrive(LD_NoDll, fullpath);
			Result := 0;
			exit;
		end;
		if (@ExecFunc <> nil) and (@VersFunc <> nil) then
		begin
			Ver := VersFunc;
			if @PrivFunc <> nil then
				Priv := PrivFunc;
		end;
		if (Ver < 153) or (Ver > 300) then
		begin
			fullpath := FPath;
			FreeLibrary(hndl);
			Clear;
			LoadLevel := 0;
			if MustExist then
				raise EZipMaster.CreateResDrive(LD_BadDll, fullpath);
		end;
		Result := Priv;
end;
//? TZipLibLoader.LoadLib

procedure TZipLibLoader.Unload(held: boolean);
begin
  if held then
    DoUnload(zldFixed)
  else
    DoUnload(zldAuto);
end;

function TZipLibLoader.Expand(src: string; dest: string): integer;
var sTOF, dTOF: TOFStruct; sH, dH: integer;
begin
	sH := -1;
	dH := -1;
	Result := 0;
	try
		sH := LZOpenFile(pChar(src),sTOF, OF_READ);
		dH := LZOpenFile(pChar(dest),dTOF, OF_CREATE);
		if (sH>0) and (dH>=0) then
			Result := LZCopy(sH, dH);
	finally
		if sH>=0 then
			LZClose(sH);
		if dH>=0 then
			LZClose(dH);
	end;
end;

function TZipLibLoader.GetLoaded: integer;
begin
  Result := 0;
  if hndl <> 0 then
    Result := Ver;
end;

function TZipLibLoader.GetLoadedPath: string;
var
  buf          : string;
begin
  Result := '';
  if hndl <> 0 then
  begin
		SetLength(buf, 4096);
    if GetModuleFileName(hndl, pChar(buf), 4096) <> 0 then
			Result := pChar(buf);
  end;
end;

function TZipLibLoader.DoExec(Rec: pointer): integer;
begin
	DoLoad(zldTemp);
	Result := ExecFunc(Rec);
	DoUnload(zldTemp);
end;

(*? TZipLibLoader.DoLoad
1.73.2.6 10 September 2003 RP only load resource if later or no other found
1.73 24 July 2003 RA fix
*)
function TZipLibLoader.DoLoad(level: integer): integer;
var
	DllDir, FullPath: string;
	zip          : TFriendZip;
	RVer: integer;
	RVrs: string;
begin
	loadLevel := (loadLevel or level) and 7;
	if hndl = 0 then
	begin
		Ver := 0;
		fullpath := '';
		zip := TFriendZip(FOwner);
		DllDir := zip.DllDirectory;
		if TmpFileName <> '' then
			fullpath := TmpFileName;
		if fullpath = '' then
		begin
			if DLLDir <> '' then
			begin
				fullpath := PathConcat(DLLDir, DllNames[IsZip]);
				if DLLDir[1] = '.' then
					fullpath := PathConcat(ExtractFilePath(ParamStr(0)),fullpath);
				if not FileExists(fullpath) then
					fullpath := '';
			end;
		end;
		if fullpath = '' then
			fullpath := DllNames[IsZip];      // Let Windows search the std dirs
	RVrs := LoadStr(VerRes[IsZip]);
	RVer := StrToIntDef(copy(RVrs,1,5),0);
	if RVer> LoadLib(fullPath, RVer<17300) then
	begin
		if LoadLib(GetResDllPath, false)<17300 then
			LoadLib(fullPath, true);  	// could not load resource
	end;
		if zip.Verbose then
			zip.CallBack(zacMessage,0,zip.LoadZipStr(LD_DllLoaded,'loaded ')+FPath,0);
	end;
	Result := Ver;
end;
//? TZipLibLoader.DoLoad

function TZipLibLoader.DoUnload(level: integer): integer;
begin
	loadLevel := (loadLevel and (not level)) and 7;
	if (loadLevel = 0) and (hndl <> 0) then
  begin
		with TFriendZip(FOwner) do
		begin
			if Verbose then
				CallBack(zacMessage,0,LoadZipStr(LD_DllUnloaded,'unloaded ')+FPath,0);
    end;
    FreeLibrary(hndl);
    hndl := 0;
  end;
	if hndl = 0 then
	begin
		Clear;
		LoadLevel := 0;
	end;
	Result := Ver;
end;

(*? TZipLoader.GetResDllPath
1.73.2.6 7 September 2006 extract 'compressed' resource files
*)
function TZipLibLoader.GetResDllPath: String;
var m: Word;
	fs           : TFileStream;
	rs           : TResourceStream;
	done         : boolean;
	tmp					 : string;
	ver : integer;
begin
	Result := '';
	done := false;
	fs := nil;
	tmp := LoadStr(VerRes[IsZip]);
	ver := StrToIntDef(copy(tmp,1,5),0);
	if ver>17300 then
	begin                          
		if IsZip then
			TmpFileName := TZipMaster(FOwner).MakeTempFileName('ZMZ','.DLL')
		else
			TmpFileName := TZipMaster(FOwner).MakeTempFileName('ZMU','.dll');
		rs := TResourceStream.CreateFromID(HInstance, DllRes[IsZip], RType);
		try
			if assigned(rs) then
			begin
				try
					rs.Read(m,2);
					if m=0 then
					begin
						fs := TFileStream.Create(TmpFileName, fmCreate);
						rs.Position := 6;
						done := fs.CopyFrom(rs,rs.Size-6)=(rs.Size-6);
					end
					else 
					if m=2 then
					begin
						Tmp := TZipMaster(FOwner).MakeTempFileName('ZMt','.tmp');
						rs.Position := 6;                                
						fs := TFileStream.Create(Tmp, fmCreate);
						done := fs.CopyFrom(rs,rs.Size-6)=(rs.Size-6);
						FreeAndNil(fs);
						if done then
						begin
							done := Expand(tmp, TmpFileName)>100000;
							DeleteFile(tmp);
						end;
					end ;
				finally
					fs.Free;
				end;
			end;
		finally
			rs.Free;
			if (not done) and FileExists(TmpFileName) then
        DeleteFile(TmpFileName);
			if not FileExists(TmpFileName) then
				TmpFileName := ''
			else
				Result := TmpFileName;
		end;
	end;
end;
//? TZipLoader.GetResDllPath

(*? TZipLibLoader.GetVer
1.73.2.8 2 Oct 2003 new getter
*)
function TZipLibLoader.GetVer: integer;
begin
	DoLoad(zldTemp);
	Result := Ver;
	DoUnload(zldTemp);
end;
//? TZipLibLoader.GetVer
                          
(*? TZipLibLoader.GetPath
1.73.2.8 2 Oct 2003 new getter
*)
function TZipLibLoader.GetPath: string;
begin
	DoLoad(zldTemp);
	Result := FPath;
	DoUnload(zldTemp);
end;        
//? TZipLibLoader.GetPath
                                
(*? TZipLibLoader.GetPriv
1.73.2.8 2 Oct 2003 new getter
*)
function TZipLibLoader.GetPriv: integer; 
begin
	DoLoad(zldTemp);
	Result := Priv;
	DoUnload(zldTemp);
end;         
//? TZipLibLoader.GetPriv

end.


