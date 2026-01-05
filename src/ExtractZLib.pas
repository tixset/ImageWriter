{******************************************************************************}
{                                                                              }
{  ImageWriter - Compression Libraries Extraction Unit                        }
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
{    Handles extraction of embedded compression DLLs from resources.          }
{    Supports zlib1.dll, libbz2.dll, and liblzma.dll extraction.              }
{    Provides unified path management for all extracted DLL files.            }
{                                                                              }
{******************************************************************************}

unit ExtractZLib;

interface

/// <summary>
/// Get directory path where extracted DLLs are stored
/// </summary>
/// <returns>Path to %LOCALAPPDATA%\ImageWriter\ or fallback location</returns>
function GetLibraryPath: string;

/// <summary>
/// Backward compatibility alias for GetLibraryPath
/// </summary>
function GetZLibPath: string;

/// <summary>
/// Extract zlib1.dll from embedded resources
/// </summary>
procedure ExtractZLibDLL;

/// <summary>
/// Extract libbz2.dll from embedded resources
/// </summary>
procedure ExtractBZip2DLL;

/// <summary>
/// Extract liblzma.dll from embedded resources
/// </summary>
procedure ExtractLzmaDLL;

/// <summary>
/// Extract all compression libraries from resources
/// </summary>
procedure ExtractAllCompressionDLLs;

implementation

uses
  Windows, SysUtils, Classes;

function GetFileSize64(const FileName: string): Int64; forward;
procedure ExtractDLLFromResource(const ResourceName, FileName: string); forward;

function GetLibraryPath: string;
var
  LocalAppData: array[0..MAX_PATH] of Char;
  Len: Integer;
begin
  // Get %LOCALAPPDATA% path
  Len := GetEnvironmentVariable('LOCALAPPDATA', LocalAppData, MAX_PATH);
  
  if Len = 0 then
  begin
    // Try APPDATA if LOCALAPPDATA not available
    Len := GetEnvironmentVariable('APPDATA', LocalAppData, MAX_PATH);
  end;
  
  if Len = 0 then
    Result := ExtractFilePath(ParamStr(0))
  else
    Result := StrPas(LocalAppData);
    
  Result := IncludeTrailingPathDelimiter(Result) + 'ImageWriter\';
end;

function GetZLibPath: string;
begin
  // Backward compatibility - old function name
  Result := GetLibraryPath;
end;

procedure ExtractDLLFromResource(const ResourceName, FileName: string);
var
  ResStream: TResourceStream;
  DirPath: string;
  FullPath: string;
  ExePath: string;
begin
  // Extract to AppData
  DirPath := GetLibraryPath;
  if not DirectoryExists(DirPath) then
    ForceDirectories(DirPath);
    
  FullPath := DirPath + FileName;
  
  // Check if DLL already exists and is valid
  if FileExists(FullPath) and (GetFileSize64(FullPath) > 0) then
    Exit; // Already extracted
  
  try
    // Try to extract from resource
    ResStream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
    try
      ResStream.SaveToFile(FullPath);
    finally
      ResStream.Free;
    end;
  except
    on E: Exception do
    begin
      // If extraction fails, try to copy from exe directory
      ExePath := ExtractFilePath(ParamStr(0)) + FileName;
      if FileExists(ExePath) then
      begin
        try
          Windows.CopyFile(PChar(ExePath), PChar(FullPath), False);
        except
          // Last resort: try lib/ subdirectory
          ExePath := ExtractFilePath(ParamStr(0)) + 'lib\' + FileName;
          if FileExists(ExePath) then
          begin
            try
              Windows.CopyFile(PChar(ExePath), PChar(FullPath), False);
            except
              // Give up silently - DLL will be loaded from lib/ or exe directory
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure ExtractZLibDLL;
begin
  ExtractDLLFromResource('ZLIB1DLL', 'zlib1.dll');
end;

procedure ExtractBZip2DLL;
begin
  ExtractDLLFromResource('LIBBZ2DLL', 'libbz2.dll');
end;

procedure ExtractLzmaDLL;
begin
  ExtractDLLFromResource('LIBLZMADLL', 'liblzma.dll');
end;

procedure ExtractAllCompressionDLLs;
begin
  ExtractZLibDLL;
  ExtractBZip2DLL;
  ExtractLzmaDLL;
end;

function GetFileSize64(const FileName: string): Int64;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Result := 0;
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := Int64(FindData.nFileSizeHigh) shl 32 or FindData.nFileSizeLow;
  end;
end;

end.
