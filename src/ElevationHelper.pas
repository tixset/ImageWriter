{******************************************************************************}
{                                                                              }
{  ImageWriter - UAC Elevation Helper Unit                                    }
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
{    Provides UAC elevation management for Vista and later Windows versions.  }
{    Handles privilege checks and elevation requests.                         }
{                                                                              }
{******************************************************************************}

unit ElevationHelper;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes, ActiveX;

// Check if current process is running with administrator privileges
function IsUserAdmin: Boolean;

// Check if process is elevated (Vista+)
function IsProcessElevated: Boolean;

// Launch process with elevated privileges
function RunElevated(const AFileName, AParameters: string; AWaitForProcess: Boolean = False): Boolean;

// Create unique pipe name for IPC
function CreatePipeName(const APrefix: string): string;

implementation

uses
  ShellAPI;

type
  TOKEN_ELEVATION = record
    TokenIsElevated: DWORD;
  end;

const
  TokenElevation = 20;

function IsUserAdmin: Boolean;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  x: Integer;
  bSuccess: BOOL;
  SIA: SID_IDENTIFIER_AUTHORITY;
begin
  Result := False;
  SIA.Value[5] := 32;  // SECURITY_NT_AUTHORITY
  
  bSuccess := AllocateAndInitializeSid(SIA, 2,
    32, 544, 0, 0, 0, 0, 0, 0,  // SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS
    psidAdministrators);
    
  if bSuccess then
  begin
    if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken) then
    begin
      try
        GetMem(ptgGroups, 1024);
        try
          bSuccess := GetTokenInformation(hAccessToken, TokenGroups,
            ptgGroups, 1024, dwInfoBufferSize);
          if bSuccess then
          begin
            for x := 0 to ptgGroups.GroupCount - 1 do
            begin
              if EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then
              begin
                Result := True;
                Break;
              end;
            end;
          end;
        finally
          FreeMem(ptgGroups);
        end;
      finally
        CloseHandle(hAccessToken);
      end;
    end;
    FreeSid(psidAdministrators);
  end;
end;

function IsProcessElevated: Boolean;
var
  hToken: THandle;
  Elevation: TOKEN_ELEVATION;
  dwSize: DWORD;
begin
  Result := False;
  
  // Only Vista+ supports elevation
  if Win32MajorVersion < 6 then
  begin
    Result := IsUserAdmin;
    Exit;
  end;
  
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hToken) then
  begin
    try
      if GetTokenInformation(hToken, TTokenInformationClass(TokenElevation),
        @Elevation, SizeOf(Elevation), dwSize) then
      begin
        Result := Elevation.TokenIsElevated <> 0;
      end;
    finally
      CloseHandle(hToken);
    end;
  end;
end;

function RunElevated(const AFileName, AParameters: string; AWaitForProcess: Boolean = False): Boolean;
var
  sei: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  Result := False;
  
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := 0;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  sei.lpVerb := 'runas';
  sei.lpFile := PChar(AFileName);
  if AParameters <> '' then
    sei.lpParameters := PChar(AParameters)
  else
    sei.lpParameters := nil;
  sei.nShow := SW_SHOWNORMAL;
  
  Result := ShellExecuteEx(@sei);
  if not Result then
    Exit;
  
  if AWaitForProcess and (sei.hProcess <> 0) then
  begin
    try
      WaitForSingleObject(sei.hProcess, INFINITE);
      GetExitCodeProcess(sei.hProcess, ExitCode);
      Result := (ExitCode = 0);
    finally
      CloseHandle(sei.hProcess);
    end;
  end;
end;

function CreatePipeName(const APrefix: string): string;
var
  GUID: TGUID;
  GUIDStr: string;
begin
  if CoCreateGuid(GUID) = S_OK then
  begin
    GUIDStr := GUIDToString(GUID);
    // Remove { and }
    GUIDStr := Copy(GUIDStr, 2, Length(GUIDStr) - 2);
    Result := '\\.\pipe\' + APrefix + '_' + GUIDStr;
  end
  else
    Result := '\\.\pipe\' + APrefix + '_' + IntToStr(GetTickCount);
end;

end.
