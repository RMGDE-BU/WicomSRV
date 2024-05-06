{******************************************************************************}
{* Unit: Konstantendefinitionen für Serial.pas                                *}
{* 14.11.2012  WW  Neu                                                        *}
{*                                                                            *}
{* Copyright © RMG Messtechnik GmbH 2012                                      *}
{******************************************************************************}
unit SerialConst;

interface

const
  // MaxPort = 49;           { es wird bis COM50 geprüft }
  MaxPort = 256;
  MaxPossiblePort = 256;  { mehr als COM256 ist technisch nicht möglich }

  // serielle Datenformate
  CSDataFormat_8N1 = '8N1';
  CSDataFormat_8E1 = '8E1';
  CSDataFormat_8O1 = '8O1';
  CSDataFormat_7E1 = '7E1';

implementation
  
end.

