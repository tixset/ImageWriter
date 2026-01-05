{******************************************************************************}
{                                                                              }
{  ImageWriter - Network Share Authentication Unit                            }
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
{    Network share authentication and mounting for Pro version.               }
{                                                                              }
{******************************************************************************}

unit NetShareAuth;

interface

uses Windows, SysUtils, Dialogs;
// Minimal declarations for WNetAddConnection2 API
type
  TNetResource = record
    dwScope: DWORD;
    dwType: DWORD;
    dwDisplayType: DWORD;
    dwUsage: DWORD;
    lpLocalName: PChar;
    lpRemoteName: PChar;
    lpComment: PChar;
    lpProvider: PChar;
  end;
  PNetResource = ^TNetResource;

const
  RESOURCETYPE_DISK = 1;
  CONNECT_INTERACTIVE = $00000008;
  CONNECT_PROMPT = $00000010;
  NO_ERROR = 0;

function WNetAddConnection2(lpNetResource: PNetResource; lpPassword, lpUserName: PChar; dwFlags: DWORD): DWORD; stdcall; external 'mpr.dll' name 'WNetAddConnection2A';

function EnsureNetworkShareAccess(const UNCPath: string): Boolean;

implementation
function ExtractShareFromUNC(const UNCPath: string): string;
var
  i, slashCount: Integer;
begin
  // Example: \\192.168.1.1\Share\folder\file.img -> \\192.168.1.1\Share
  Result := '';
  slashCount := 0;
  for i := 1 to Length(UNCPath) do
  begin
    if UNCPath[i] = '\' then
      Inc(slashCount);
    if slashCount = 4 then
    begin
      Result := Copy(UNCPath, 1, i - 1);
      Exit;
    end;
  end;
  if slashCount >= 2 then
    Result := UNCPath;
end;

function EnsureNetworkShareAccess(const UNCPath: string): Boolean;
var
  NetResource: TNetResource;
  Share: string;
  res: DWORD;
begin
  Result := True;
  if (Pos('\\', UNCPath) = 1) and (not FileExists(UNCPath)) then
  begin
    Share := ExtractShareFromUNC(UNCPath);
    FillChar(NetResource, SizeOf(NetResource), 0);
    NetResource.dwType := RESOURCETYPE_DISK;
    NetResource.lpRemoteName := PChar(Share);
    // Attempt to connect with standard authentication dialog
    res := WNetAddConnection2(@NetResource, nil, nil, CONNECT_INTERACTIVE or CONNECT_PROMPT);
    Result := (res = NO_ERROR);
  end;
end;

end.
