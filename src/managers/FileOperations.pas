{******************************************************************************}
{                                                                              }
{  ImageWriter - File Operations Unit                                         }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{  Based on dd for Windows by John Newbigin (http://chrysocome.net/dd)        }
{                                                                              }
{  This program is free software: you can redistribute it and/or modify       }
{  it under the terms of the GNU General Public License as published by       }
{  the Free Software Foundation, either version 3 of the License, or          }
{  (at your option) any later version.                                        }
{                                                                              }
{  Description:                                                                }
{    File operations and size calculations.                                   }
{    Provides utilities for file size formatting and disk space checks.       }
{                                                                              }
{******************************************************************************}

unit FileOperations;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Classes, SysUtils;

type
  // File operations utility class
  TFileOperations = class
  public
    // Get file size (supports files > 2GB)
    class function GetFileSize64(const FileName: string): Int64;
    
    // Parse size string (e.g., "1024", "512K", "1M", "2G")
    class function ParseSize(const SizeStr: string; DefaultValue: Int64 = 512): Int64;
    
    // Convert bytes to human-readable format
    class function BytesToHuman(Bytes: Int64): string;
    
    // Convert bytes to short format (e.g., "1G", "512M")
    class function BytesShort(Bytes: Int64): string;
    
    // Check if file is GZIP archive
    class function IsGZipFile(const FileName: string): Boolean;
    
    // Check if file is ZIP archive
    class function IsZipFile(const FileName: string): Boolean;
    
    // Get GZIP comment
    class function GetGZipComment(const FileName: string): string;
    
    // Get ZIP comment
    class function GetZipComment(const FileName: string): string;
    
    // Network share access helper
    class function EnsureNetworkShareAccess(const FilePath: string): Boolean;
  end;

implementation

uses
  NetShareAuth;

{ TFileOperations }

class function TFileOperations.GetFileSize64(const FileName: string): Int64;
var
  SearchRec: TSearchRec;
  Size64: Int64Rec;
begin
  Result := 0;
  
  {$WARN SYMBOL_PLATFORM OFF}
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
  begin
    try
      // Use FindData for files > 2GB (SearchRec.Size is only 32-bit signed)
      Size64.Lo := SearchRec.FindData.nFileSizeLow;
      Size64.Hi := SearchRec.FindData.nFileSizeHigh;
      Result := Int64(Size64);
    finally
      FindClose(SearchRec);
    end;
  end;
  {$WARN SYMBOL_PLATFORM ON}
end;

class function TFileOperations.ParseSize(const SizeStr: string; DefaultValue: Int64): Int64;
var
  NumPart: string;
  Multiplier: Int64;
  i: Integer;
  C: Char;
begin
  Result := DefaultValue;
  
  if Trim(SizeStr) = '' then
    Exit;
    
  NumPart := '';
  Multiplier := 1;
  
  // Extract numeric part and multiplier
  for i := 1 to Length(SizeStr) do
  begin
    C := UpCase(SizeStr[i]);
    
    if C in ['0'..'9'] then
      NumPart := NumPart + C
    else if C = 'K' then
      Multiplier := 1024
    else if C = 'M' then
      Multiplier := 1048576
    else if C = 'G' then
      Multiplier := 1073741824
    else if C = 'T' then
      Multiplier := 1099511627776;
  end;
  
  if NumPart <> '' then
    Result := StrToInt64Def(NumPart, DefaultValue) * Multiplier
  else
    Result := DefaultValue;
end;

class function TFileOperations.BytesToHuman(Bytes: Int64): string;
begin
  if Bytes >= 1099511627776 then
    Result := Format('%.2f TB', [Bytes / 1099511627776])
  else if Bytes >= 1073741824 then
    Result := Format('%.2f GB', [Bytes / 1073741824])
  else if Bytes >= 1048576 then
    Result := Format('%.2f MB', [Bytes / 1048576])
  else if Bytes >= 1024 then
    Result := Format('%.2f KB', [Bytes / 1024])
  else
    Result := Format('%d B', [Bytes]);
end;

class function TFileOperations.BytesShort(Bytes: Int64): string;
begin
  if Bytes >= 1099511627776 then
    Result := Format('%dT', [Bytes div 1099511627776])
  else if Bytes >= 1073741824 then
    Result := Format('%dG', [Bytes div 1073741824])
  else if Bytes >= 1048576 then
    Result := Format('%dM', [Bytes div 1048576])
  else if Bytes >= 1024 then
    Result := Format('%dK', [Bytes div 1024])
  else
    Result := Format('%d', [Bytes]);
end;

class function TFileOperations.IsGZipFile(const FileName: string): Boolean;
var
  Ext: string;
  f: File;
  magic: array[0..1] of Byte;
  BytesRead: Integer;
begin
  Result := False;
  
  // Check extension first
  Ext := LowerCase(ExtractFileExt(FileName));
  if (Ext = '.gz') or (Ext = '.gzip') then
  begin
    Result := True;
    Exit;
  end;
  
  // Check magic bytes (1F 8B)
  if FileExists(FileName) then
  begin
    AssignFile(f, FileName);
    FileMode := 0; // Read-only
    Reset(f, 1);
    try
      if FileSize(f) >= 2 then
      begin
        BlockRead(f, magic, 2, BytesRead);
        Result := (BytesRead = 2) and (magic[0] = $1F) and (magic[1] = $8B);
      end;
    finally
      CloseFile(f);
    end;
  end;
end;

class function TFileOperations.IsZipFile(const FileName: string): Boolean;
var
  Ext: string;
  f: File;
  magic: array[0..3] of Byte;
  BytesRead: Integer;
begin
  Result := False;
  
  // Check extension first
  Ext := LowerCase(ExtractFileExt(FileName));
  if Ext = '.zip' then
  begin
    Result := True;
    Exit;
  end;
  
  // Check magic bytes (50 4B 03 04)
  if FileExists(FileName) then
  begin
    AssignFile(f, FileName);
    FileMode := 0; // Read-only
    Reset(f, 1);
    try
      if FileSize(f) >= 4 then
      begin
        BlockRead(f, magic, 4, BytesRead);
        Result := (BytesRead = 4) and (magic[0] = $50) and (magic[1] = $4B) and 
                  (magic[2] = $03) and (magic[3] = $04);
      end;
    finally
      CloseFile(f);
    end;
  end;
end;

class function TFileOperations.GetGZipComment(const FileName: string): string;
begin
  // GZIP doesn't have standard comment field
  Result := '';
end;

class function TFileOperations.GetZipComment(const FileName: string): string;
begin
  // TODO: Implement ZIP comment extraction
  Result := '';
end;

class function TFileOperations.EnsureNetworkShareAccess(const FilePath: string): Boolean;
begin
  Result := True;
  
  // Check if this is a UNC path
  if (Length(FilePath) >= 2) and (FilePath[1] = '\') and (FilePath[2] = '\') then
  begin
    // Try to authenticate if needed
    {$IFDEF IMAGEWRITER_PRO}
    Result := NetShareAuth.EnsureNetworkShareAccess(FilePath);
    {$ENDIF}
  end;
end;

end.
