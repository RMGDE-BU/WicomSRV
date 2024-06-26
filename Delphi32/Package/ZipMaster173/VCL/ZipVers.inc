// VER80 = Delphi 1 but DelZip is not made for it.

{$IfDef VER90}
   {$Define KNOWNCOMPILER}
   {$Define DELPHI}
   {$Define VERD2D3} 
   {$DEFINE VERD5-}
{$EndIf}
{$IfDef VER93}
   {$DEFINE KNOWNCOMPILER}
   {$DEFINE CBUILDER}
   {$DEFINE CBUILDER1}  
   {$DEFINE VERD5-}
{$EndIf}
{$IfDef VER100}
   {$DEFINE KNOWNCOMPILER}
   {$DEFINE DELPHI}
   {$DEFINE DELPHI3}
   {$Define VERD2D3}  
   {$DEFINE VERD5-}
{$EndIf}
{$IfDef VER110}
   {$DEFINE KNOWNCOMPILER}
   {$DEFINE CBUILDER}
   {$ObjExportAll On}
   {$DEFINE CBUILDER3} 
   {$DEFINE VERD5-}
{$EndIf}
{$IfDef VER120}
   {$DEFINE KNOWNCOMPILER}
   {$DEFINE DELPHI}
   {$DEFINE DELPHI4}
   {$Define VERD4+} 
   {$DEFINE VERD5-}
{$EndIf}
{$IfDef VER125}
   {$DEFINE KNOWNCOMPILER}
   {$DEFINE CBUILDER}
   {$DEFINE CBUILDER4}
   {$ObjExportAll On}
   {$DEFINE VERD4+}  
   {$DEFINE VERD5-}
{$EndIf}
{$IfDef VER130}
 {$IfNDef BCB}   
   {$DEFINE KNOWNCOMPILER}
   {$DEFINE DELPHI}
   {$DEFINE DELPHI5}
   {$Define VERD4+} 
   {$DEFINE VERD5-}
 {$EndIf}
{$EndIf}
{$IfDef VER130}
 {$IfDef BCB}
   {$DEFINE KNOWNCOMPILER}
   {$DEFINE CBUILDER}
   {$DEFINE CBUILDER5}
   {$ObjExportAll On}
   {$DEFINE VERD4+}
   {$DEFINE VERD5-}
 {$EndIf}
{$EndIf}
{$IfDef VER140}  // EWE: Support Delphi 6 and BCB 6
 {$IfNDef BCB}
	 {$DEFINE KNOWNCOMPILER}
	 {$DEFINE DELPHI}
	 {$DEFINE DELPHI6}
	 {$Define VERD4+}
 {$EndIf}
{$EndIf}
{$IfDef VER140}
 {$IfDef BCB}
	 {$DEFINE KNOWNCOMPILER}
	 {$DEFINE CBUILDER}
	 {$DEFINE CBUILDER6}
	 {$ObjExportAll On}
	 {$DEFINE VERD4+}
 {$EndIf}
{$EndIf}   
{$IfDef VER150}  // RP: ?? Support Delphi 7 and BCB 7?
 {$IfNDef BCB}
	 {$DEFINE KNOWNCOMPILER}
	 {$DEFINE DELPHI}
	 {$DEFINE DELPHI7}
	 {$Define VERD4+}
 {$EndIf}
{$EndIf}
{$IfDef VER150}
 {$IfDef BCB}
	 {$DEFINE KNOWNCOMPILER}
	 {$DEFINE CBUILDER}
	 {$DEFINE CBUILDER7}
	 {$ObjExportAll On}
	 {$DEFINE VERD4+}
 {$EndIf}
{$EndIf}

{$IfNDef KNOWNCOMPILER}
 {$DEFINE KNOWNCOMPILER}
 {$IfNDef BCB}
	 {$DEFINE CBUILDER}
	 {$DEFINE CBUILDER6}
	 {$ObjExportAll On}
 {$Endif}
 {$DEFINE VERD4+}
{$EndIf}

{$IfNDef KNOWNCOMPILER}
YOU ARE USING A COMPILER FOR WHICH THIS PACKAGE IS NOT YET DESIGNED!

YOU SHOULD ADD A COMPILER DEFINITION FIRST TO THE FILE ZipVers.inc

YOU CAN REMOVE THIS ERROR MESSAGE BY REMOVING THE DOT (.) ON THE LINE
"{.$DEFINE KNOWNCOMPILER}"  ABOVE

YOU HAVE BEEN WARNED !!!

{$EndIf}
